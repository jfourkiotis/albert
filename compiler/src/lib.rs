use ast::*;
use code::*;
use std::collections::LinkedList;
use string_interner::{DefaultStringInterner, Sym, Symbol};

mod symbol_table;
pub use symbol_table::*;

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum Constant {
    Num(f64),
    Str(usize), // an interned string
    True,
    False,
    Null,
    Array(usize),
    Function(usize),
    Closure(usize),
}

impl std::fmt::Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Constant::Num(v) => write!(f, "{}", v),
            Constant::Str(s) => write!(f, "string({:?})", s),
            Constant::True => write!(f, "true"),
            Constant::False => write!(f, "false"),
            Constant::Null => write!(f, "null"),
            Constant::Array(a) => write!(f, "array({})", a),
            Constant::Function(findex) => write!(f, "function({})", findex),
            Constant::Closure(cindex) => write!(f, "closure({})", cindex),
        }
    }
}

impl Constant {
    pub fn type_name(self) -> &'static str {
        match self {
            Constant::Num(_) => "NUMBER",
            Constant::Str(_) => "STRING",
            Constant::True => "BOOLEAN",
            Constant::False => "BOOLEAN",
            Constant::Null => "NULL",
            Constant::Array(_) => "ARRAY",
            Constant::Function(_) => "FUNCTION",
            Constant::Closure(_) => "CLOSURE",
        }
    }
}

pub struct CompiledFunction {
    pub name: String,
    pub instructions: Vec<u8>,
    pub num_locals: usize,
    pub num_params: usize,
}

pub struct Closure {
    pub cfunc: usize,
    pub freev: Vec<Constant>,
}

pub struct Bytecode {
    pub instructions: Vec<u8>,
    pub constants: Vec<Constant>,
    pub strings: Vec<String>,
    pub functions: Vec<CompiledFunction>,
}

impl Bytecode {
    fn print_instructions(f: &mut std::fmt::Formatter<'_>, ins: &[u8]) -> std::fmt::Result {
        let mut ip = 0usize;
        while ip < ins.len() {
            let op = ins[ip];
            let def = lookup(op).unwrap();

            write!(f, "[{:>4}] {} ", ip, def.name)?;
            ip += 1;
            for (_i, w) in def.operand_widths.iter().enumerate() {
                if *w == 2 {
                    let operand = [ins[ip], ins[ip + 1]];
                    write!(f, "{} ", u16::from_be_bytes(operand))?;
                } else if *w == 1 {
                    write!(f, "{} ", ins[ip])?;
                }
                ip += *w;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

impl std::fmt::Display for Bytecode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "[CONSTANTS]")?;
        for (i, c) in self.constants.iter().enumerate() {
            writeln!(f, "{:04}: {:}", i, c)?;
        }
        writeln!(f, "[STRINGS]")?;
        for (i, s) in self.strings.iter().enumerate() {
            writeln!(f, "{:04}: \"{:}\"", i, *s)?;
        }
        writeln!(f, "[INSTRUCTIONS]")?;

        Bytecode::print_instructions(f, &self.instructions)?;

        writeln!(f, "[FUNCTIONS]")?;

        for (i, cf) in self.functions.iter().enumerate() {
            writeln!(f, "#{:04}", i)?;
            Bytecode::print_instructions(f, &cf.instructions)?;
        }
        Ok(())
    }
}

#[derive(Debug, Copy, Clone)]
struct EmittedInstruction {
    op: OpCode,
    pos: usize,
}

#[derive(Default)]
struct CompilationScope {
    instructions: Vec<u8>,
    last_instruction: Option<EmittedInstruction>,
    prev_instruction: Option<EmittedInstruction>,
}

pub struct Compiler<'a> {
    constants: Vec<Constant>,
    functions: Vec<CompiledFunction>,
    symbol_tables: LinkedList<SymbolTable<'a>>,
    str_interner: DefaultStringInterner,
    scopes: Vec<CompilationScope>,
    scope_index: usize,
}

impl<'a> Default for Compiler<'a> {
    fn default() -> Self {
        let mut c = Compiler {
            constants: vec![],
            functions: vec![],
            symbol_tables: LinkedList::new(),
            str_interner: DefaultStringInterner::default(),
            scopes: vec![CompilationScope::default()],
            scope_index: 0,
        };

        c.symbol_tables.push_front(SymbolTable::default());
        c
    }
}

#[macro_use]
macro_rules! current_scope {
    ($compiler:ident) => {
        $compiler.scopes[$compiler.scope_index]
    };
}

impl<'a> Compiler<'a> {
    pub fn compile_program(self, prog: &'a Program<'a>) -> Result<Bytecode, String> {
        self.compile_statements(&prog.statements, &prog.stmt_nodes, &prog.expr_nodes)
    }

    fn compile_statements(
        mut self, statements: &[StmtId], stmt_nodes: &[Statement], expr_nodes: &'a [Node],
    ) -> Result<Bytecode, String> {
        for (_, stmt) in statements.iter().enumerate() {
            self.compile_statement(*stmt, &stmt_nodes, &expr_nodes)?;
        }

        let top_scope = self.scopes.remove(0);

        Ok(Bytecode {
            instructions: top_scope.instructions,
            constants: self.constants,
            functions: self.functions,
            strings: self
                .str_interner
                .into_iter()
                .map(|pair| pair.1)
                .collect::<Vec<String>>(),
        })
    }

    fn compile_statement(
        &mut self, stmt: StmtId, stmt_nodes: &[Statement], expr_nodes: &'a [Node],
    ) -> Result<(), String> {
        match &stmt_nodes[stmt] {
            Statement::Expression { expression, .. } => {
                if let Some(expr_id) = expression {
                    self.compile_expression(*expr_id, stmt_nodes, expr_nodes)?;
                    self.emit(OpCode::Pop, &[]).map(|_u| ())
                } else {
                    Ok(())
                }
            }
            Statement::Block { statements, .. } => {
                for (_, stmt) in statements.iter().enumerate() {
                    self.compile_statement(*stmt, &stmt_nodes, &expr_nodes)?;
                }
                Ok(())
            }
            Statement::Let { name, value, .. } => match &expr_nodes[*name] {
                Node::Identifier { value: id, .. } => {
                    let current_symtab = self
                        .symbol_tables
                        .front_mut()
                        .expect("ICE: empty symbol table list");
                    let symbol = current_symtab.define(id);
                    self.compile_expression(*value, stmt_nodes, expr_nodes)?;
                    if symbol.scope == SymbolScope::Global {
                        self.emit(OpCode::SetGlobal, &[symbol.index]).map(|_u| ())
                    } else {
                        self.emit(OpCode::SetLocal, &[symbol.index]).map(|_u| ())
                    }
                }
                _ => unreachable!("fatal compiler error"),
            },
            Statement::Return { return_value, .. } => {
                if let Some(r) = return_value {
                    self.compile_expression(*r, stmt_nodes, expr_nodes)?;
                    self.emit(OpCode::ReturnValue, &[]).map(|_u| ())
                } else {
                    self.emit(OpCode::Return, &[]).map(|_u| ())
                }
            }
        }
    }

    fn compile_expression(
        &mut self, expr: ExprId, stmt_nodes: &[Statement], expr_nodes: &'a [Node],
    ) -> Result<(), String> {
        match &expr_nodes[expr] {
            Node::Infix {
                left,
                operator,
                right,
                ..
            } => {
                self.compile_expression(*left, stmt_nodes, expr_nodes)?;
                self.compile_expression(*right, stmt_nodes, expr_nodes)?;
                match *operator {
                    "+" => self.emit(OpCode::Add, &[]).map(|_u| ()),
                    "-" => self.emit(OpCode::Sub, &[]).map(|_u| ()),
                    "*" => self.emit(OpCode::Mul, &[]).map(|_u| ()),
                    "/" => self.emit(OpCode::Div, &[]).map(|_u| ()),
                    "^" => self.emit(OpCode::Pow, &[]).map(|_u| ()),
                    ">" => self.emit(OpCode::GreaterThan, &[]).map(|_u| ()),
                    "<" => self.emit(OpCode::LessThan, &[]).map(|_u| ()),
                    "==" => self.emit(OpCode::Equal, &[]).map(|_u| ()),
                    "!=" => self.emit(OpCode::NotEqual, &[]).map(|_u| ()),
                    unsupported => unimplemented!("unsupported operator {}", unsupported),
                }
            }
            Node::Prefix {
                right, operator, ..
            } => {
                self.compile_expression(*right, stmt_nodes, expr_nodes)?;
                if *operator == "-" {
                    self.emit(OpCode::Minus, &[]).map(|_u| ())
                } else if *operator == "!" {
                    self.emit(OpCode::Bang, &[]).map(|_u| ())
                } else {
                    Err(format!("unknown operator: {}", operator))
                }
            }
            Node::If {
                condition,
                consequence,
                alternative,
                ..
            } => {
                self.compile_expression(*condition, stmt_nodes, expr_nodes)?;
                let jump_if_false_pos = self.emit(OpCode::JumpIfFalse, &[9999])?;
                self.compile_statement(*consequence, stmt_nodes, expr_nodes)?;
                self.rem_last_instruction_if_pop();
                let jump_pos = self.emit(OpCode::Jump, &[9999])?;
                let after_consequence_pos = current_scope!(self).instructions.len();
                self.change_operand(jump_if_false_pos, after_consequence_pos)?;

                if let Some(alt) = alternative {
                    self.compile_statement(*alt, stmt_nodes, expr_nodes)?;
                    self.rem_last_instruction_if_pop();
                } else {
                    self.emit(OpCode::Null, &[])?;
                }
                let after_alternative_pos = current_scope!(self).instructions.len();
                self.change_operand(jump_pos, after_alternative_pos)
            }
            Node::Num { value, .. } => {
                let index = self.add_constant(*value);
                self.emit(OpCode::Constant, &[index])?;
                Ok(())
            }
            Node::Boolean { value, .. } => self
                .emit(if *value { OpCode::True } else { OpCode::False }, &[])
                .map(|_u| ()),
            Node::Str { value, .. } => {
                let sindex = self.str_interner.get_or_intern(*value);
                let cindex = self.add_str_constant(sindex);
                self.emit(OpCode::Constant, &[cindex])?;
                Ok(())
            }
            Node::Identifier { value, .. } => {
                let current_symtab = self
                    .symbol_tables
                    .front_mut()
                    .expect("ICE: empty symbol table list");
                let symbol = current_symtab.resolve_or_define_free(value);
                if let Some(s) = symbol {
                    let index = s.index;
                    match s.scope {
                        SymbolScope::Global => self.emit(OpCode::GetGlobal, &[index]).map(|_u| ()),
                        SymbolScope::Local => self.emit(OpCode::GetLocal, &[index]).map(|_u| ()),
                        SymbolScope::Free => self.emit(OpCode::GetFree, &[index]).map(|_u| ()),
                        SymbolScope::Function => {
                            self.emit(OpCode::CurrentClosure, &[]).map(|_u| ())
                        }
                    }
                } else {
                    Err(format!("undefined variable {}", value))
                }
            }
            Node::Array { elements, .. } => {
                for e in elements.iter() {
                    self.compile_expression(*e, stmt_nodes, expr_nodes)?;
                }
                self.emit(OpCode::Array, &[elements.len()]).map(|_u| ())
            }
            Node::Index { array, index, .. } => {
                self.compile_expression(*array, stmt_nodes, expr_nodes)?;
                self.compile_expression(*index, stmt_nodes, expr_nodes)?;
                self.emit(OpCode::Index, &[]).map(|_u| ())
            }
            Node::Func {
                parameters,
                body,
                name,
                ..
            } => {
                self.enter_scope();

                let current_symtab = self
                    .symbol_tables
                    .front_mut()
                    .expect("ICE: empty symbol table list");
                if !name.is_empty() {
                    current_symtab.define_function(name);
                }
                for (i, p) in parameters.iter().enumerate() {
                    match &expr_nodes[*p] {
                        Node::Identifier { value, .. } => {
                            current_symtab.define(value);
                        }
                        _ => return Err(format!("invalid function argument #{}", i)),
                    }
                }

                self.compile_statement(*body, stmt_nodes, expr_nodes)?;
                self.replace_last_instruction_if_pop_with_return()?;
                if !self.last_instruction_is(OpCode::ReturnValue) {
                    self.emit(OpCode::Return, &[])?;
                }

                // We need to reborrow here, because the borrow checked does not allow us
                // to use current_symtab. This may be a smelly code.
                let current_symtab = self
                    .symbol_tables
                    .front()
                    .expect("ICE: empty symbol table list");
                let num_locals = current_symtab.num_definitions;
                let free_symbols = current_symtab.free_symbols.clone();
                let instructions = self.leave_scope();

                for fs in free_symbols.iter() {
                    match fs.scope {
                        SymbolScope::Global => {
                            self.emit(OpCode::GetGlobal, &[fs.index]).map(|_u| ())
                        }
                        SymbolScope::Local => self.emit(OpCode::GetLocal, &[fs.index]).map(|_u| ()),
                        SymbolScope::Free => self.emit(OpCode::GetFree, &[fs.index]).map(|_u| ()),
                        SymbolScope::Function => {
                            self.emit(OpCode::CurrentClosure, &[]).map(|_u| ())
                        }
                    }?;
                }

                let compiled_fn = CompiledFunction {
                    instructions,
                    name: "".to_string(), // we have a name problem
                    num_locals,
                    num_params: parameters.len(),
                };
                self.functions.push(compiled_fn);
                let findex = self.functions.len() - 1;
                let cindex = self.add_fun_constant(findex);
                self.emit(OpCode::Closure, &[cindex, free_symbols.len()])
                    .map(|_u| ())
            }
            Node::Call {
                function,
                arguments,
                ..
            } => {
                self.compile_expression(*function, stmt_nodes, expr_nodes)?;
                for a in arguments.iter() {
                    self.compile_expression(*a, stmt_nodes, expr_nodes)?;
                }
                self.emit(OpCode::Call, &[arguments.len()]).map(|_u| ())
            }
        }
    }

    fn add_constant(&mut self, number: f64) -> usize {
        self.constants.push(Constant::Num(number));
        self.constants.len() - 1
    }

    fn add_str_constant(&mut self, s: Sym) -> usize {
        self.constants.push(Constant::Str(s.to_usize()));
        self.constants.len() - 1
    }

    fn add_fun_constant(&mut self, f: usize) -> usize {
        self.constants.push(Constant::Function(f));
        self.constants.len() - 1
    }

    fn add_instruction(&mut self, ins: Vec<u8>) -> usize {
        let offset = current_scope!(self).instructions.len();
        current_scope!(self).instructions.extend_from_slice(&ins);
        offset
    }

    fn emit(&mut self, op: OpCode, operands: &[usize]) -> Result<usize, String> {
        let ins = make_instruction(op, &operands)?;
        let pos = self.add_instruction(ins);
        self.set_last_instruction(op, pos);
        Ok(pos)
    }

    fn set_last_instruction(&mut self, op: OpCode, pos: usize) {
        current_scope!(self).prev_instruction = current_scope!(self).last_instruction.take();
        current_scope!(self).last_instruction = Some(EmittedInstruction { op, pos });
    }

    fn last_instruction_is(&self, op: OpCode) -> bool {
        let last_instruction = self.scopes[self.scope_index].last_instruction;
        if let Some(last) = last_instruction {
            last.op == op
        } else {
            false
        }
    }

    fn rem_last_instruction_if_pop(&mut self) {
        let last_instruction = current_scope!(self).last_instruction;
        match last_instruction {
            Some(last) if last.op == OpCode::Pop => {
                current_scope!(self).instructions.resize(last.pos, 0xFF);
                current_scope!(self).last_instruction = current_scope!(self).prev_instruction;
            }
            _ => (),
        }
    }

    fn replace_last_instruction_if_pop_with_return(&mut self) -> Result<(), String> {
        let last_instruction = current_scope!(self).last_instruction;
        match last_instruction {
            Some(last) if last.op == OpCode::Pop => {
                let pos = last.pos;
                let ret_ins = make_instruction(OpCode::ReturnValue, &[])?;
                for (i, byte) in ret_ins.iter().enumerate() {
                    current_scope!(self).instructions[pos + i] = *byte;
                }
                current_scope!(self)
                    .last_instruction
                    .replace(EmittedInstruction {
                        op: OpCode::ReturnValue,
                        pos,
                    });
                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn replace_instruction(&mut self, pos: usize, new_instr: Vec<u8>) {
        for (i, byte) in new_instr.iter().enumerate() {
            current_scope!(self).instructions[pos + i] = *byte;
        }
    }

    fn change_operand(&mut self, pos: usize, new_operand: usize) -> Result<(), String> {
        let op = BYTE_TO_OPCODE[current_scope!(self).instructions[pos] as usize];
        let new_instr = make_instruction(op, &[new_operand])?;
        self.replace_instruction(pos, new_instr);
        Ok(())
    }

    fn enter_scope(&mut self) {
        let scope = CompilationScope::default();
        self.scopes.push(scope);
        self.scope_index += 1;

        // push a new symbol table
        let top_symtab = self
            .symbol_tables
            .front_mut()
            .expect("ICE: empty symbol table list");
        let local_symtab = SymbolTable::new_enclosed(top_symtab);
        self.symbol_tables.push_front(local_symtab);
    }

    fn leave_scope(&mut self) -> Vec<u8> {
        let scope = self
            .scopes
            .pop()
            .expect("fatal error: leave_scope called on empty scopes");
        self.scope_index -= 1;

        // pop the last local symbol table
        self.symbol_tables.pop_front();
        scope.instructions
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lexer::Lexer;
    use parser::Parser;

    #[macro_use]
    macro_rules! True {
        () => {
            make_instruction(OpCode::True, &[]).unwrap()
        };
    }

    #[macro_use]
    macro_rules! False {
        () => {
            make_instruction(OpCode::False, &[]).unwrap()
        };
    }

    #[macro_use]
    macro_rules! Bang {
        () => {
            make_instruction(OpCode::Bang, &[]).unwrap()
        };
    }

    #[macro_use]
    macro_rules! Pop {
        () => {
            make_instruction(OpCode::Pop, &[]).unwrap()
        };
    }

    #[macro_use]
    macro_rules! Index {
        () => {
            make_instruction(OpCode::Index, &[]).unwrap()
        };
    }

    #[macro_use]
    macro_rules! Array {
        ($i:expr) => {
            make_instruction(OpCode::Array, &[$i]).unwrap()
        };
    }

    #[macro_use]
    macro_rules! Add {
        () => {
            make_instruction(OpCode::Add, &[]).unwrap()
        };
    }

    #[macro_use]
    macro_rules! Sub {
        () => {
            make_instruction(OpCode::Sub, &[]).unwrap()
        };
    }

    #[macro_use]
    macro_rules! Minus {
        () => {
            make_instruction(OpCode::Minus, &[]).unwrap()
        };
    }

    #[macro_use]
    macro_rules! Mul {
        () => {
            make_instruction(OpCode::Mul, &[]).unwrap()
        };
    }

    #[macro_use]
    macro_rules! Div {
        () => {
            make_instruction(OpCode::Div, &[]).unwrap()
        };
    }

    #[macro_use]
    macro_rules! Pow {
        () => {
            make_instruction(OpCode::Pow, &[]).unwrap()
        };
    }

    #[macro_use]
    macro_rules! GreaterThan {
        () => {
            make_instruction(OpCode::GreaterThan, &[]).unwrap()
        };
    }

    #[macro_use]
    macro_rules! LessThan {
        () => {
            make_instruction(OpCode::LessThan, &[]).unwrap()
        };
    }

    #[macro_use]
    macro_rules! Equal {
        () => {
            make_instruction(OpCode::Equal, &[]).unwrap()
        };
    }

    #[macro_use]
    macro_rules! NotEqual {
        () => {
            make_instruction(OpCode::NotEqual, &[]).unwrap()
        };
    }

    #[macro_use]
    macro_rules! Null {
        () => {
            make_instruction(OpCode::Null, &[]).unwrap()
        };
    }

    #[macro_use]
    macro_rules! ReturnValue {
        () => {
            make_instruction(OpCode::ReturnValue, &[]).unwrap()
        };
    }

    #[macro_use]
    macro_rules! Return {
        () => {
            make_instruction(OpCode::Return, &[]).unwrap()
        };
    }

    #[macro_use]
    macro_rules! CurrentClosure {
        () => {
            make_instruction(OpCode::CurrentClosure, &[]).unwrap()
        };
    }

    #[macro_use]
    macro_rules! Call {
        ($i:expr) => {
            make_instruction(OpCode::Call, &[$i]).unwrap()
        };
    }

    #[macro_use]
    macro_rules! Constant {
        ($i:expr) => {
            make_instruction(OpCode::Constant, &[$i]).unwrap()
        };
    }

    #[macro_use]
    macro_rules! Closure {
        ($i:expr, $j:expr) => {
            make_instruction(OpCode::Closure, &[$i, $j]).unwrap()
        };
    }

    #[macro_use]
    macro_rules! JumpIfFalse {
        ($i:expr) => {
            make_instruction(OpCode::JumpIfFalse, &[$i]).unwrap()
        };
    }

    #[macro_use]
    macro_rules! Jump {
        ($i:expr) => {
            make_instruction(OpCode::Jump, &[$i]).unwrap()
        };
    }

    #[macro_use]
    macro_rules! SetGlobal {
        ($i:expr) => {
            make_instruction(OpCode::SetGlobal, &[$i]).unwrap()
        };
    }

    #[macro_use]
    macro_rules! GetGlobal {
        ($i:expr) => {
            make_instruction(OpCode::GetGlobal, &[$i]).unwrap()
        };
    }

    #[macro_use]
    macro_rules! SetLocal {
        ($i:expr) => {
            make_instruction(OpCode::SetLocal, &[$i]).unwrap()
        };
    }

    #[macro_use]
    macro_rules! GetLocal {
        ($i:expr) => {
            make_instruction(OpCode::GetLocal, &[$i]).unwrap()
        };
    }

    #[macro_use]
    macro_rules! GetFree {
        ($i:expr) => {
            make_instruction(OpCode::GetFree, &[$i]).unwrap()
        };
    }

    #[macro_use]
    macro_rules! instructions {
        ($($i:expr),*) => {{
            let mut v = vec![];
            $(v.push($i);)*
            v.into_iter().flatten().collect::<Vec<u8>>()
        }};
    }

    #[derive(Default)]
    struct T {
        input: &'static str,
        expected_strings: Vec<&'static str>,
        expected_constants: Vec<Constant>,
        expected_instructions: Vec<u8>,
        expected_functions: Vec<Vec<u8>>,
    }

    #[test]
    fn integer_arithmetic_works() {
        let tests = [
            T {
                input: "!true;",
                expected_instructions: instructions! {
                    True!(), Bang!(), Pop!()
                },
                ..Default::default()
            },
            T {
                input: "-1;",
                expected_constants: vec![Constant::Num(1f64)],
                expected_instructions: instructions! {
                    Constant!(0), Minus!(), Pop!()
                },
                ..Default::default()
            },
            T {
                input: "1 > 2;",
                expected_constants: vec![Constant::Num(1f64), Constant::Num(2f64)],
                expected_instructions: instructions! {
                    Constant!(0), Constant!(1), GreaterThan!(), Pop!()
                },
                ..Default::default()
            },
            T {
                input: "1 < 2;",
                expected_constants: vec![Constant::Num(1f64), Constant::Num(2f64)],
                expected_instructions: instructions! {
                    Constant!(0), Constant!(1), LessThan!(), Pop!()
                },
                ..Default::default()
            },
            T {
                input: "1 == 2;",
                expected_constants: vec![Constant::Num(1f64), Constant::Num(2f64)],
                expected_instructions: instructions! {
                    Constant!(0), Constant!(1), Equal!(), Pop!()
                },
                ..Default::default()
            },
            T {
                input: "1 != 2;",
                expected_constants: vec![Constant::Num(1f64), Constant::Num(2f64)],
                expected_instructions: instructions! {
                    Constant!(0), Constant!(1), NotEqual!(), Pop!()
                },
                ..Default::default()
            },
            T {
                input: "1 ^ 2;",
                expected_constants: vec![Constant::Num(1f64), Constant::Num(2f64)],
                expected_instructions: instructions! {
                    Constant!(0), Constant!(1), Pow!(), Pop!()
                },
                ..Default::default()
            },
            T {
                input: "1 + 2;",
                expected_constants: vec![Constant::Num(1f64), Constant::Num(2f64)],
                expected_instructions: instructions! {
                    Constant!(0), Constant!(1), Add!(), Pop!()
                },
                ..Default::default()
            },
            T {
                input: "1 - 2;",
                expected_constants: vec![Constant::Num(1f64), Constant::Num(2f64)],
                expected_instructions: instructions! {
                    Constant!(0), Constant!(1), Sub!(), Pop!()
                },
                ..Default::default()
            },
            T {
                input: "1 * 2;",
                expected_constants: vec![Constant::Num(1f64), Constant::Num(2f64)],
                expected_instructions: instructions! {
                    Constant!(0), Constant!(1), Mul!(), Pop!()
                },
                ..Default::default()
            },
            T {
                input: "2 / 1;",
                expected_constants: vec![Constant::Num(2f64), Constant::Num(1f64)],
                expected_instructions: instructions! {
                    Constant!(0), Constant!(1), Div!(), Pop!()
                },
                ..Default::default()
            },
            T {
                input: "1; 2;",
                expected_constants: vec![Constant::Num(1f64), Constant::Num(2f64)],
                expected_instructions: instructions! {
                    Constant!(0), Pop!(), Constant!(1), Pop!()
                },
                ..Default::default()
            },
            T {
                input: "true;",
                expected_instructions: instructions! {
                    True!(), Pop!()
                },
                ..Default::default()
            },
            T {
                input: "false;",
                expected_instructions: instructions! {
                    False!(), Pop!()
                },
                ..Default::default()
            },
            T {
                input: "!true;",
                expected_instructions: instructions! {
                    True!(), Bang!(), Pop!()
                },
                ..Default::default()
            },
        ];

        run_compiler_tests(&tests);
    }

    #[test]
    fn compiling_conditionals_works() {
        let tests = [
            T {
                input: "if (true) { 10 }; 3333;",
                expected_constants: vec![Constant::Num(10f64), Constant::Num(3333f64)],
                expected_instructions: instructions! {
                    True!(),
                    JumpIfFalse!(10),
                    Constant!(0),
                    Jump!(11),
                    Null!(),
                    Pop!(),
                    Constant!(1),
                    Pop!()
                },
                ..Default::default()
            },
            T {
                input: "if (true) { 10 } else { 20 }; 3333;",
                expected_constants: vec![
                    Constant::Num(10f64),
                    Constant::Num(20f64),
                    Constant::Num(3333f64),
                ],
                expected_instructions: instructions! {
                    True!(),
                    JumpIfFalse!(10),
                    Constant!(0),
                    Jump!(13),
                    Constant!(1),
                    Pop!(),
                    Constant!(2),
                    Pop!()
                },
                ..Default::default()
            },
        ];

        run_compiler_tests(&tests);
    }

    #[test]
    fn compiling_let_statements_works() {
        let tests = [
            T {
                input: "let one = 1; let two = 2;",
                expected_constants: vec![Constant::Num(1f64), Constant::Num(2f64)],
                expected_instructions: instructions! {
                    Constant!(0), SetGlobal!(0), Constant!(1), SetGlobal!(1)
                },
                ..Default::default()
            },
            T {
                input: "let one = 1; one;",
                expected_constants: vec![Constant::Num(1f64)],
                expected_instructions: instructions! {
                    Constant!(0), SetGlobal!(0), GetGlobal!(0), Pop!()
                },
                ..Default::default()
            },
            T {
                input: "let one = 1; let two = one; two;",
                expected_constants: vec![Constant::Num(1f64)],
                expected_instructions: instructions! {
                    Constant!(0), SetGlobal!(0), GetGlobal!(0), SetGlobal!(1), GetGlobal!(1), Pop!()
                },
                ..Default::default()
            },
        ];

        run_compiler_tests(&tests);
    }

    #[test]
    fn compiling_string_expressions_works() {
        let tests = [
            T {
                input: r#""monkey";"#,
                expected_strings: vec!["monkey"],
                expected_constants: vec![Constant::Str(0)],
                expected_instructions: instructions! {
                    Constant!(0), Pop!()
                },
                ..Default::default()
            },
            T {
                input: r#""mon" + "key";"#,
                expected_strings: vec!["mon", "key"],
                expected_constants: vec![Constant::Str(0), Constant::Str(1)],
                expected_instructions: instructions! {
                    Constant!(0), Constant!(1), Add!(), Pop!()
                },
                ..Default::default()
            },
        ];
        run_compiler_tests(&tests);
    }

    #[test]
    fn compiling_array_literals_works() {
        let tests = [
            T {
                input: "[];",
                expected_instructions: instructions! {
                    Array!(0), Pop!()
                },
                ..Default::default()
            },
            T {
                input: "[1, 2, 3];",
                expected_constants: vec![
                    Constant::Num(1f64),
                    Constant::Num(2f64),
                    Constant::Num(3f64),
                ],
                expected_instructions: instructions! {
                    Constant!(0), Constant!(1), Constant!(2), Array!(3), Pop!()
                },
                ..Default::default()
            },
            T {
                input: "[1 + 2, 3 - 4, 5 * 6];",
                expected_constants: vec![
                    Constant::Num(1f64),
                    Constant::Num(2f64),
                    Constant::Num(3f64),
                    Constant::Num(4f64),
                    Constant::Num(5f64),
                    Constant::Num(6f64),
                ],
                expected_instructions: instructions! {
                    Constant!(0),
                    Constant!(1),
                    Add!(),
                    Constant!(2),
                    Constant!(3),
                    Sub!(),
                    Constant!(4),
                    Constant!(5),
                    Mul!(),
                    Array!(3),
                    Pop!()
                },
                ..Default::default()
            },
        ];
        run_compiler_tests(&tests);
    }

    #[test]

    fn compiling_function_calls_works() {
        let tests = [
            T {
                input: "fn() { 20 }();",
                expected_constants: vec![Constant::Num(20f64), Constant::Function(0)],
                expected_instructions: instructions! {
                    Closure!(1, 0),
                    Call!(0),
                    Pop!()
                },
                expected_functions: vec![instructions! {
                    Constant!(0),
                    ReturnValue!()
                }],
                ..Default::default()
            },
            T {
                input: "let noArg = fn() { 24 }; noArg();",
                expected_constants: vec![Constant::Num(24f64), Constant::Function(0)],
                expected_instructions: instructions! {
                    Closure!(1, 0),
                    SetGlobal!(0),
                    GetGlobal!(0),
                    Call!(0),
                    Pop!()
                },
                expected_functions: vec![instructions! {
                    Constant!(0), ReturnValue!()
                }],
                ..Default::default()
            },
            T {
                input: "let oneArg = fn(a) { a }; oneArg(24);",
                expected_constants: vec![Constant::Function(0), Constant::Num(24f64)],
                expected_instructions: instructions! {
                    Closure!(0, 0),
                    SetGlobal!(0),
                    GetGlobal!(0),
                    Constant!(1),
                    Call!(1),
                    Pop!()
                },
                expected_functions: vec![instructions! {
                    GetLocal!(0),
                    ReturnValue!()
                }],
                ..Default::default()
            },
            T {
                input: "let manyArg = fn(a, b, c) { a; b; c; }; manyArg(24, 25, 26);",
                expected_constants: vec![
                    Constant::Function(0),
                    Constant::Num(24f64),
                    Constant::Num(25f64),
                    Constant::Num(26f64),
                ],
                expected_instructions: instructions! {
                    Closure!(0, 0),
                    SetGlobal!(0),
                    GetGlobal!(0),
                    Constant!(1),
                    Constant!(2),
                    Constant!(3),
                    Call!(3),
                    Pop!()
                },
                expected_functions: vec![instructions! {
                    GetLocal!(0),
                    Pop!(),
                    GetLocal!(1),
                    Pop!(),
                    GetLocal!(2),
                    ReturnValue!()
                }],
                ..Default::default()
            },
        ];
        run_compiler_tests(&tests);
    }

    #[test]
    fn compiling_indexing_expressions_works() {
        let tests = [
            T {
                input: "[1, 2, 3][1 + 1];",
                expected_constants: vec![
                    Constant::Num(1f64),
                    Constant::Num(2f64),
                    Constant::Num(3f64),
                    Constant::Num(1f64),
                    Constant::Num(1f64),
                ],
                expected_instructions: instructions! {
                    Constant!(0),
                    Constant!(1),
                    Constant!(2),
                    Array!(3),
                    Constant!(3),
                    Constant!(4),
                    Add!(),
                    Index!(),
                    Pop!()
                },
                ..Default::default()
            },
            T {
                input: "[1,2][0];",
                expected_constants: vec![
                    Constant::Num(1f64),
                    Constant::Num(2f64),
                    Constant::Num(0f64),
                ],
                expected_instructions: instructions! {
                    Constant!(0),
                    Constant!(1),
                    Array!(2),
                    Constant!(2),
                    Index!(),
                    Pop!()
                },
                ..Default::default()
            },
        ];
        run_compiler_tests(&tests);
    }

    #[test]
    fn compiling_functions_works() {
        let tests = [
            T {
                input: "fn() { return 5 + 10; };",
                expected_constants: vec![
                    Constant::Num(5f64),
                    Constant::Num(10f64),
                    Constant::Function(0),
                ],
                expected_instructions: instructions! {
                    Closure!(2, 0), Pop!()
                },
                expected_functions: vec![instructions! {
                    Constant!(0), Constant!(1), Add!(), ReturnValue!()
                }],
                ..Default::default()
            },
            T {
                input: "fn() {  };",
                expected_constants: vec![Constant::Function(0)],
                expected_instructions: instructions! {
                    Closure!(0, 0), Pop!()
                },
                expected_functions: vec![instructions! {
                    Return!()
                }],
                ..Default::default()
            },
        ];

        run_compiler_tests(&tests);
    }

    #[test]
    fn compiling_let_statement_scopes_works() {
        let tests = [
            T {
                input: "let num = 55; fn() { num };",
                expected_constants: vec![Constant::Num(55f64), Constant::Function(0)],
                expected_instructions: instructions! {
                    Constant!(0),
                    SetGlobal!(0),
                    Closure!(1, 0),
                    Pop!()
                },
                expected_functions: vec![instructions! {
                    GetGlobal!(0),
                    ReturnValue!()
                }],
                ..Default::default()
            },
            T {
                input: "fn() { let num = 55; num; };",
                expected_constants: vec![Constant::Num(55f64), Constant::Function(0)],
                expected_instructions: instructions! {
                    Closure!(1, 0),
                    Pop!()
                },
                expected_functions: vec![instructions! {
                    Constant!(0),
                    SetLocal!(0),
                    GetLocal!(0),
                    ReturnValue!()
                }],
                ..Default::default()
            },
            T {
                input: "fn() { let a = 55; let b = 77; a + b; };",
                expected_constants: vec![
                    Constant::Num(55f64),
                    Constant::Num(77f64),
                    Constant::Function(0),
                ],
                expected_instructions: instructions! {
                    Closure!(2, 0),
                    Pop!()
                },
                expected_functions: vec![instructions! {
                    Constant!(0),
                    SetLocal!(0),
                    Constant!(1),
                    SetLocal!(1),
                    GetLocal!(0),
                    GetLocal!(1),
                    Add!(),
                    ReturnValue!()
                }],
                ..Default::default()
            },
        ];

        run_compiler_tests(&tests);
    }

    #[test]
    fn compiling_recursive_closures_works() {
        let tests = [
            T {
                input: "let countDown = fn(x) { countDown(x - 1); }; countDown(1);",
                expected_constants: vec![
                    Constant::Num(1f64),
                    Constant::Function(0),
                    Constant::Num(1f64),
                ],
                expected_instructions: instructions! {
                    Closure!(1, 0),
                    SetGlobal!(0),
                    GetGlobal!(0),
                    Constant!(2),
                    Call!(1),
                    Pop!()
                },
                expected_functions: vec![instructions! {
                    CurrentClosure!(),
                    GetLocal!(0),
                    Constant!(0),
                    Sub!(),
                    Call!(1),
                    ReturnValue!()
                }],
                ..Default::default()
            },
            T {
                input: "
                let wrapper = fn() {
                    let countDown = fn(x) { countDown(x - 1); };
                    countDown(1);
                };
                wrapper();
                ",
                expected_constants: vec![
                    Constant::Num(1f64),
                    Constant::Function(0),
                    Constant::Num(1f64),
                    Constant::Function(1),
                ],
                expected_instructions: instructions! {
                    Closure!(3, 0),
                    SetGlobal!(0),
                    GetGlobal!(0),
                    Call!(0),
                    Pop!()
                },
                expected_functions: vec![
                    instructions! {
                        CurrentClosure!(),
                        GetLocal!(0),
                        Constant!(0),
                        Sub!(),
                        Call!(1),
                        ReturnValue!()
                    },
                    instructions! {
                        Closure!(1, 0),
                        SetLocal!(0),
                        GetLocal!(0),
                        Constant!(2),
                        Call!(1),
                        ReturnValue!()
                    },
                ],
                ..Default::default()
            },
        ];

        run_compiler_tests(&tests);
    }

    #[test]
    fn compiling_closures_works() {
        let tests = [
            T {
                input: "
                    fn(a) {
                        fn(b) {
                            a + b
                        }
                    };
                ",
                expected_constants: vec![Constant::Function(0), Constant::Function(1)],
                expected_instructions: instructions! {
                    Closure!(1, 0),
                    Pop!()
                },
                expected_functions: vec![
                    instructions! {
                        GetFree!(0),
                        GetLocal!(0),
                        Add!(),
                        ReturnValue!()
                    },
                    instructions! {
                        GetLocal!(0),
                        Closure!(0, 1),
                        ReturnValue!()
                    },
                ],
                ..Default::default()
            },
            T {
                input: "
                    fn(a) {
                        fn(b) {
                            fn(c) {
                                a + b + c
                            }
                        }
                    };
                ",
                expected_constants: vec![
                    Constant::Function(0),
                    Constant::Function(1),
                    Constant::Function(2),
                ],
                expected_instructions: instructions! {
                    Closure!(2, 0),
                    Pop!()
                },
                expected_functions: vec![
                    instructions! {
                        GetFree!(0),
                        GetFree!(1),
                        Add!(),
                        GetLocal!(0),
                        Add!(),
                        ReturnValue!()
                    },
                    instructions! {
                        GetFree!(0),
                        GetLocal!(0),
                        Closure!(0, 2),
                        ReturnValue!()
                    },
                    instructions! {
                        GetLocal!(0),
                        Closure!(1, 1),
                        ReturnValue!()
                    },
                ],
                ..Default::default()
            },
            T {
                input: "
                let global = 55;

                fn() {
                    let a = 66;

                    fn() {
                        let b = 77;

                        fn() {
                            let c = 88;

                            global + a + b + c;
                        }
                    }
                };
                ",
                expected_constants: vec![
                    Constant::Num(55f64),
                    Constant::Num(66f64),
                    Constant::Num(77f64),
                    Constant::Num(88f64),
                    Constant::Function(0),
                    Constant::Function(1),
                    Constant::Function(2),
                ],
                expected_instructions: instructions! {
                    Constant!(0),
                    SetGlobal!(0),
                    Closure!(6, 0),
                    Pop!()
                },
                expected_functions: vec![
                    instructions! {
                        Constant!(3),
                        SetLocal!(0),
                        GetGlobal!(0),
                        GetFree!(0),
                        Add!(),
                        GetFree!(1),
                        Add!(),
                        GetLocal!(0),
                        Add!(),
                        ReturnValue!()
                    },
                    instructions! {
                        Constant!(2),
                        SetLocal!(0),
                        GetFree!(0),
                        GetLocal!(0),
                        Closure!(4, 2),
                        ReturnValue!()
                    },
                    instructions! {
                        Constant!(1),
                        SetLocal!(0),
                        GetLocal!(0),
                        Closure!(5, 1),
                        ReturnValue!()
                    },
                ],
                ..Default::default()
            },
        ];

        run_compiler_tests(&tests);
    }

    fn run_compiler_tests(tests: &[T]) {
        for (n, test) in tests.iter().enumerate() {
            let lexer = Lexer::new(test.input);
            let parser = Parser::new(lexer);
            let prog = parser.parse_program().unwrap();
            let compiler = Compiler::default();
            let bytecode = compiler.compile_program(&prog).unwrap();

            assert_eq!(
                test.expected_constants.len(),
                bytecode.constants.len(),
                "number of constants differ (#{})",
                n
            );
            assert_eq!(
                test.expected_instructions.len(),
                bytecode.instructions.len(),
                "number of instructions differ"
            );
            assert_eq!(
                test.expected_strings.len(),
                bytecode.strings.len(),
                "number of strings differ"
            );
            assert_eq!(
                test.expected_functions.len(),
                bytecode.functions.len(),
                "number of functions differ",
            );

            // constants
            for (i, (c1, c2)) in test
                .expected_constants
                .iter()
                .zip(bytecode.constants.iter())
                .enumerate()
            {
                assert_eq!(c1, c2, "constant value at pos: {} as not as expected", i);
            }

            // strings
            for (i, (s1, s2)) in test
                .expected_strings
                .iter()
                .zip(bytecode.strings.iter())
                .enumerate()
            {
                assert_eq!(s1, s2, "string at pos: {} as not as expected", i);
            }

            // instructions
            for (i, (b1, b2)) in test
                .expected_instructions
                .iter()
                .zip(bytecode.instructions.iter())
                .enumerate()
            {
                if b1 != b2 {
                    for (i, (b1, b2)) in test
                        .expected_instructions
                        .iter()
                        .zip(bytecode.instructions.iter())
                        .enumerate()
                    {
                        println!("{:04} {:#8b} {:#8b}", i, b1, b2);
                    }
                    panic!("instruction byte at pos: {} not as expected (#{})", i, n);
                }
            }

            // functions
            for (i, (fx, fa)) in test
                .expected_functions
                .iter()
                .zip(bytecode.functions.iter())
                .enumerate()
            {
                assert_eq!(
                    *fx, fa.instructions,
                    "instructions differ for function #{}",
                    i
                );
            }
        }
    }
}
