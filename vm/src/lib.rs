mod frame;

use code::*;
use compiler::*;
use frame::Frame;

use std::convert::TryInto;

const STACK_SIZE: usize = 2048;
const MAX_FRAMES: usize = 1024;
pub const GLOBALS_SIZE: usize = 65536;

// A virtual machine will execute a Bytecode. For this to happen,
// it needs a stack.
pub struct Vm<'a> {
    // bytecode
    constants: Vec<Constant>,
    strings: Vec<String>,
    arrays: Vec<Vec<Constant>>,
    functions: Vec<CompiledFunction>,
    closures: Vec<Closure>,
    // a virtual machine will execute a bytecode. For this to happen, it
    // will need a stack of constants
    stack: Vec<Constant>,
    // `sp`, the stack pointer points to the next value. the top of the stack
    // is `stack[sp-1]`
    sp: usize,
    globals: &'a mut Vec<Constant>,
    frames: Vec<Frame>,
    frame_index: usize,
}

#[macro_use]
macro_rules! pop {
    ($a:expr) => {{
        let top = $a.stack[$a.sp - 1];
        $a.sp -= 1;
        top
    }};
}

#[macro_use]
macro_rules! push {
    ($a:expr, $v:expr) => {{
        $a.stack[$a.sp] = $v;
        $a.sp += 1;
    }};
}

#[macro_use]
macro_rules! raise_operator_error {
    ($a:expr, $o:expr, $b:expr) => {
        return Err(format!("unknown operator: {}{}{}", $a, $o, $b,));
    };
}

#[macro_use]
macro_rules! current_frame {
    ($f:ident) => {
        $f.frames[$f.frame_index]
    };
}

#[macro_use]
macro_rules! current_ip {
    ($f: ident) => {
        current_frame!($f).ip
    };
}

#[macro_use]
macro_rules! current_instructions {
    ($f: ident) => {
        $f.functions[$f.closures[current_frame!($f).ci].cfunc].instructions
    };
}

#[macro_use]
macro_rules! remaining_instructions {
    ($f: ident) => {
        current_instructions!($f)[current_ip!($f)..]
    };
}

impl<'a> Vm<'a> {
    pub fn for_bytecode(bytecode: Bytecode, globals: &'a mut Vec<Constant>) -> Self {
        let mut vm = Vm {
            constants: bytecode.constants,
            strings: bytecode.strings,
            functions: bytecode.functions,
            closures: vec![],
            arrays: vec![],
            stack: vec![],
            sp: 0,
            globals,
            frames: vec![],
            frame_index: 0,
        };

        vm.stack.resize_with(STACK_SIZE, || Constant::Int(0));
        vm.frames.resize_with(MAX_FRAMES, || Frame {
            ci: std::usize::MAX,
            ip: std::usize::MAX,
            len: std::usize::MAX,
            base_pointer: std::usize::MAX,
        });

        // setup frames
        let main_fn = CompiledFunction {
            instructions: bytecode.instructions,
            name: "".to_string(),
            num_locals: 0,
            num_params: 0,
        };

        // there is no Constant::Function(main_index) on purpose.
        let main_instructions_len = main_fn.instructions.len();
        vm.functions.push(main_fn);
        let main_index = vm.functions.len() - 1;
        let main_closure = Closure {
            cfunc: main_index,
            freev: vec![],
        };
        vm.closures.push(main_closure);
        let cindex = vm.closures.len() - 1;
        vm.frames[0] = Frame {
            ci: cindex,
            ip: 0,
            base_pointer: 0,
            len: main_instructions_len,
        };

        vm
    }

    fn println(&self, val: &Constant) -> String {
        match val {
            Constant::Int(v) => format!("{}", v),
            Constant::True => "true".to_string(),
            Constant::False => "false".to_string(),
            Constant::Null => "null".to_string(),
            Constant::Str(s) => format!("\"{}\"", self.strings[*s]),
            Constant::Array(a) => {
                let mut buffer = String::new();
                buffer.push('[');
                let arr = &self.arrays[*a];
                let mut iter = arr.iter();
                if let Some(e) = iter.next() {
                    buffer.push_str(&self.println(e));
                    for e in iter {
                        buffer.push_str(", ");
                        buffer.push_str(&self.println(e));
                    }
                }
                buffer.push(']');
                buffer
            }
            Constant::Function(f) => format!("<function {}>", *f),
            Constant::Closure(c) => format!("<closure {}>", *c),
        }
    }

    pub fn run(mut self) -> Result<(Constant, String), String> {
        loop {
            if self.current_ip() >= self.current_instructions_len() {
                break;
            }

            let op = self.current_opcode();
            *self.current_ip_mut() += 1;
            let op = if cfg!(debug_assertion) {
                BYTE_TO_OPCODE[op as usize]
            } else {
                unsafe { *BYTE_TO_OPCODE.get_unchecked(op as usize) }
            };
            match op {
                OpCode::Constant => {
                    let (op_bytes, _) =
                        remaining_instructions!(self).split_at(std::mem::size_of::<u16>());
                    let op_index = u16::from_be_bytes(op_bytes.try_into().unwrap());
                    *self.current_ip_mut() += 2;
                    push!(self, self.constants[op_index as usize]);
                }
                OpCode::Add => {
                    let right = pop!(self);
                    let left = pop!(self);

                    match (left, right) {
                        (Constant::Int(v1), Constant::Int(v2)) => {
                            push!(self, Constant::Int(v1 + v2));
                        }
                        (Constant::Str(s1), Constant::Str(s2)) => {
                            let mut buffer = String::from(&self.strings[s1]);
                            buffer.push_str(&self.strings[s2]);
                            self.strings.push(buffer);
                            push!(self, Constant::Str(self.strings.len() - 1));
                        }
                        (a, b) => raise_operator_error!(a.type_name(), "+", b.type_name()),
                    }
                }
                OpCode::Sub => {
                    let right = pop!(self);
                    let left = pop!(self);
                    match (left, right) {
                        (Constant::Int(v1), Constant::Int(v2)) => {
                            push!(self, Constant::Int(v1 - v2));
                        }
                        (a, b) => raise_operator_error!(a.type_name(), "-", b.type_name()),
                    }
                }
                OpCode::Mul => {
                    let right = pop!(self);
                    let left = pop!(self);
                    match (left, right) {
                        (Constant::Int(v1), Constant::Int(v2)) => {
                            push!(self, Constant::Int(v1 * v2));
                        }
                        (a, b) => raise_operator_error!(a.type_name(), "*", b.type_name()),
                    }
                }
                OpCode::Div => {
                    let right = pop!(self);
                    let left = pop!(self);
                    match (left, right) {
                        (Constant::Int(v1), Constant::Int(v2)) => {
                            push!(self, Constant::Int(v1 / v2));
                        }
                        (a, b) => raise_operator_error!(a.type_name(), "/", b.type_name()),
                    }
                }
                OpCode::Pow => {
                    let right = pop!(self);
                    let left = pop!(self);
                    match (left, right) {
                        (Constant::Int(v1), Constant::Int(v2)) => {
                            push!(self, Constant::Int(v1.pow(v2.try_into().unwrap())));
                        }
                        (a, b) => raise_operator_error!(a.type_name(), "^", b.type_name()),
                    }
                }
                OpCode::Minus => {
                    let right = pop!(self);
                    if let Constant::Int(v) = right {
                        push!(self, Constant::Int(-v));
                    } else {
                        return Err(format!("unknown operator: -{}", right.type_name()));
                    }
                }
                OpCode::Bang => {
                    let right = pop!(self);
                    match right {
                        Constant::True => push!(self, Constant::False),
                        Constant::False => push!(self, Constant::True),
                        Constant::Null => push!(self, Constant::True),
                        _ => push!(self, Constant::False),
                    }
                }
                OpCode::True => {
                    push!(self, Constant::True);
                }
                OpCode::False => {
                    push!(self, Constant::False);
                }
                OpCode::Equal => {
                    let right = pop!(self);
                    let left = pop!(self);
                    push!(
                        self,
                        if left == right {
                            Constant::True
                        } else {
                            Constant::False
                        }
                    );
                }
                OpCode::NotEqual => {
                    let right = pop!(self);
                    let left = pop!(self);
                    push!(
                        self,
                        if left != right {
                            Constant::True
                        } else {
                            Constant::False
                        }
                    );
                }
                OpCode::GreaterThan => {
                    let right = pop!(self);
                    let left = pop!(self);
                    push!(
                        self,
                        if left > right {
                            Constant::True
                        } else {
                            Constant::False
                        }
                    );
                }
                OpCode::LessThan => {
                    let right = pop!(self);
                    let left = pop!(self);
                    push!(
                        self,
                        if left < right {
                            Constant::True
                        } else {
                            Constant::False
                        }
                    );
                }
                OpCode::Jump => {
                    let (op_bytes, _) =
                        remaining_instructions!(self).split_at(std::mem::size_of::<u16>());
                    let op_index = u16::from_be_bytes(op_bytes.try_into().unwrap());
                    *self.current_ip_mut() = op_index as usize;
                }
                OpCode::JumpIfFalse => {
                    let (op_bytes, _) =
                        remaining_instructions!(self).split_at(std::mem::size_of::<u16>());
                    let jump_pos = u16::from_be_bytes(op_bytes.try_into().unwrap());
                    *self.current_ip_mut() += 2;

                    let condition = pop!(self);
                    if !self.is_truthy(condition) {
                        *self.current_ip_mut() = jump_pos as usize;
                    }
                }
                OpCode::Null => push!(self, Constant::Null),
                OpCode::SetGlobal => {
                    let (op_bytes, _) =
                        remaining_instructions!(self).split_at(std::mem::size_of::<u16>());
                    let op_index = u16::from_be_bytes(op_bytes.try_into().unwrap());
                    *self.current_ip_mut() += 2;

                    self.globals[op_index as usize] = pop!(self);
                }
                OpCode::GetGlobal => {
                    let (op_bytes, _) =
                        remaining_instructions!(self).split_at(std::mem::size_of::<u16>());
                    let op_index = u16::from_be_bytes(op_bytes.try_into().unwrap());
                    *self.current_ip_mut() += 2;

                    push!(self, self.globals[op_index as usize]);
                }
                OpCode::Array => {
                    let (op_bytes, _) =
                        remaining_instructions!(self).split_at(std::mem::size_of::<u16>());
                    let nelems = u16::from_be_bytes(op_bytes.try_into().unwrap());
                    *self.current_ip_mut() += 2;
                    let top = self.sp;
                    self.sp -= nelems as usize;
                    let mut array = vec![];
                    array.extend_from_slice(&self.stack[self.sp..top]);
                    self.arrays.push(array);
                    push!(self, Constant::Array(self.arrays.len() - 1));
                }
                OpCode::Index => {
                    let index = pop!(self);
                    let left = pop!(self);

                    match (left, index) {
                        (Constant::Array(a), Constant::Int(i)) => {
                            let array = &self.arrays[a];
                            if i < 0 {
                                return Err(format!("array index cannot be negative: {}", i));
                            }
                            let i = i as usize;
                            if i >= array.len() {
                                push!(self, Constant::Null)
                            } else {
                                push!(self, array[i])
                            }
                        }
                        _ => {
                            return Err(format!(
                                "index operator not supported: {}",
                                left.type_name()
                            ));
                        }
                    }
                }
                OpCode::Call => {
                    let num_args = self.current_opcode() as usize; // FIXME: operand, not opcode
                    *self.current_ip_mut() += 1;
                    if let Constant::Closure(cindex) = self.stack[self.sp - 1 - num_args] {
                        let findex = self.closures[cindex].cfunc;
                        if self.functions[findex].num_params != num_args {
                            return Err(format!(
                                "wrong number of arguments: expected {}, but got {}",
                                self.functions[findex].num_params, num_args
                            ));
                        }
                        self.frame_index += 1;
                        let frame = Frame {
                            ci: cindex,
                            ip: 0,
                            base_pointer: self.sp - num_args,
                            len: self.functions[findex].instructions.len(),
                        };
                        self.sp = frame.base_pointer + self.functions[findex].num_locals;
                        self.frames[self.frame_index] = frame;
                    } else {
                        return Err("tried to call non-function".to_string());
                    }
                }
                OpCode::Closure => {
                    let (findex, _) =
                        remaining_instructions!(self).split_at(std::mem::size_of::<u16>());
                    let findex = u16::from_be_bytes(findex.try_into().unwrap());
                    *self.current_ip_mut() += 2;
                    let num_free = current_instructions!(self)[self.current_ip()] as usize;
                    *self.current_ip_mut() += 1;
                    if let Constant::Function(findex) = self.constants[findex as usize] {
                        let mut freev = Vec::new();
                        freev.extend_from_slice(&self.stack[self.sp - num_free..self.sp]);
                        assert_eq!(freev.len(), num_free);
                        self.sp -= num_free;
                        let closure = Closure {
                            cfunc: findex,
                            freev,
                        };
                        self.closures.push(closure);
                        push!(self, Constant::Closure(self.closures.len() - 1));
                    } else {
                        return Err("tried to call non-function".to_string());
                    }
                }
                OpCode::GetFree => {
                    let free_index = current_instructions!(self)[self.current_ip()] as usize;
                    *self.current_ip_mut() += 1;
                    let current_closure = self.current_closure();
                    push!(self, current_closure.freev[free_index]);
                }
                OpCode::ReturnValue => {
                    // calling convention:
                    //
                    // top   -> [retval]
                    // top-1 -> [Constant::Function(fid)]
                    //
                    let retval = pop!(self);
                    self.sp = self.current_frame().base_pointer - 1;
                    self.frame_index -= 1;
                    push!(self, retval);
                }
                OpCode::Return => {
                    self.sp = self.current_frame().base_pointer - 1;
                    self.frame_index -= 1;
                    push!(self, Constant::Null);
                }
                OpCode::Pop => {
                    self.sp -= 1;
                }
                OpCode::SetLocal => {
                    let local_index = self.current_opcode() as usize; // FIXME: we mean ... operand
                    *self.current_ip_mut() += 1;
                    let sindex = self.current_frame().base_pointer + local_index;
                    self.stack[sindex] = pop!(self);
                }
                OpCode::GetLocal => {
                    let local_index = self.current_opcode() as usize; // FIXME: we mean ... operand
                    *self.current_ip_mut() += 1;
                    let sindex = self.current_frame().base_pointer + local_index;
                    push!(self, self.stack[sindex]);
                }
                OpCode::CurrentClosure => {
                    let ccindex = self.current_frame().ci;
                    push!(self, Constant::Closure(ccindex));
                }
                OpCode::Max => unreachable!(),
            }
        }
        Ok((self.stack[self.sp], self.println(&self.stack[self.sp])))
    }

    fn is_truthy(&self, val: Constant) -> bool {
        match val {
            Constant::True => true,
            Constant::False => false,
            Constant::Null => false,
            _ => true,
        }
    }

    #[cfg(debug_assertions)]
    #[inline(always)]
    fn current_frame(&self) -> &Frame {
        &self.frames[self.frame_index]
    }

    #[cfg(not(debug_assertions))]
    #[inline(always)]
    fn current_frame(&self) -> &Frame {
        &self.frames[self.frame_index]
    }

    #[inline(always)]
    fn current_ip_mut(&mut self) -> &mut usize {
        &mut self.frames[self.frame_index].ip
    }

    #[cfg(debug_assertions)]
    fn current_ip(&self) -> usize {
        self.current_frame().ip
    }

    #[cfg(not(debug_assertions))]
    #[inline(always)]
    fn current_ip(&self) -> usize {
        unsafe { self.frames.get_unchecked(self.frame_index).ip }
    }

    #[cfg(debug_assertions)]
    fn current_instructions_len(&self) -> usize {
        self.current_frame().len
    }

    #[cfg(not(debug_assertions))]
    #[inline(always)]
    fn current_instructions_len(&self) -> usize {
        unsafe { self.frames.get_unchecked(self.frame_index).len }
    }

    #[cfg(debug_assertions)]
    #[inline(always)]
    fn current_closure(&self) -> &Closure {
        &self.closures[self.current_frame().ci]
    }

    #[cfg(not(debug_assertions))]
    #[inline(always)]
    fn current_closure(&self) -> &Closure {
        unsafe {
            let frame = &self.frames.get_unchecked(self.frame_index);
            &self.closures.get_unchecked(frame.ci)
        }
    }

    #[cfg(debug_assertions)]
    #[inline(always)]
    fn current_function(&self) -> &CompiledFunction {
        &self.functions[self.current_closure().cfunc]
    }

    #[cfg(debug_assertions)]
    fn current_opcode(&self) -> u8 {
        let frame = self.current_frame();
        let funct = self.current_function();
        funct.instructions[frame.ip]
    }

    #[cfg(not(debug_assertions))]
    #[inline(always)]
    fn current_opcode(&self) -> u8 {
        unsafe {
            let frame = &self.frames.get_unchecked(self.frame_index);
            let closure = &self.closures.get_unchecked(frame.ci);
            let function = &self.functions.get_unchecked(closure.cfunc);
            *function.instructions.get_unchecked(frame.ip)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lexer::Lexer;
    use parser::Parser;
    struct T(&'static str, Constant);

    #[test]
    fn integer_arithmetic_works() {
        let tests = [
            T("1;", Constant::Int(1)),
            T("2;", Constant::Int(2)),
            T("1 + 2;", Constant::Int(3)),
            T("1 - 2;", Constant::Int(-1)),
            T("1 * 2;", Constant::Int(2)),
            T("4 / 2;", Constant::Int(2)),
            T("2 ^ 3 ^ 2;", Constant::Int(512)),
            T("50 / 2 * 2 + 10 - 5;", Constant::Int(55)),
            T("5 + 5 + 5 + 5 - 10;", Constant::Int(10)),
            T("2 * 2 * 2 * 2 * 2;", Constant::Int(32)),
            T("5 * 2 + 10;", Constant::Int(20)),
            T("5 + 2 * 10;", Constant::Int(25)),
            T("5 * (2 + 10);", Constant::Int(60)),
            T("-5;", Constant::Int(-5)),
            T("-10;", Constant::Int(-10)),
            T("-50 + 100 + -50;", Constant::Int(0)),
            T("(5 + 10 * 2 + 15 / 3) * 2 + -10;", Constant::Int(50)),
        ];

        run_vm_tests(&tests);
    }

    #[test]
    fn boolean_literals_work() {
        let tests = [T("true;", Constant::True), T("false;", Constant::False)];
        run_vm_tests(&tests);
    }

    #[test]
    fn logical_expressions_work() {
        let tests = [
            T("1 < 2;", Constant::True),
            T("1 > 2;", Constant::False),
            T("1 < 1;", Constant::False),
            T("1 > 1;", Constant::False),
            T("1 == 1;", Constant::True),
            T("1 != 1;", Constant::False),
            T("1 == 2;", Constant::False),
            T("1 != 2;", Constant::True),
            T("true == true;", Constant::True),
            T("false == false;", Constant::True),
            T("true == false;", Constant::False),
            T("true != false;", Constant::True),
            T("false != true;", Constant::True),
            T("(1 < 2) == true;", Constant::True),
            T("(1 < 2) == false;", Constant::False),
            T("(1 > 2) == true;", Constant::False),
            T("(1 > 2) == false;", Constant::True),
            T("!true;", Constant::False),
            T("!false;", Constant::True),
            T("!5;", Constant::False),
            T("!!true;", Constant::True),
            T("!!false;", Constant::False),
            T("!!5;", Constant::True),
        ];

        run_vm_tests(&tests);
    }

    #[test]
    fn conditional_expressions_work() {
        let tests = [
            T("if (true) { 10 };", Constant::Int(10)),
            T("if (true) { 10 } else { 20 };", Constant::Int(10)),
            T("if (false) { 10 } else { 20 }; ", Constant::Int(20)),
            T("if (1) { 10 };", Constant::Int(10)),
            T("if (1 < 2) { 10 };", Constant::Int(10)),
            T("if (1 < 2) { 10 } else { 20 };", Constant::Int(10)),
            T("if (1 > 2) { 10 } else { 20 };", Constant::Int(20)),
            T("if (1 > 2) { 10 };", Constant::Null),
            T("if (false) { 10 };", Constant::Null),
        ];

        run_vm_tests(&tests);
    }

    #[test]
    fn global_let_statements_work() {
        let tests = [
            T("let one = 1; one;", Constant::Int(1)),
            T("let one = 1; let two = 2; one + two;", Constant::Int(3)),
            T(
                "let one = 1; let two = one + one; one + two;",
                Constant::Int(3),
            ),
        ];

        run_vm_tests(&tests);
    }

    #[test]
    fn string_expressions_work() {
        let tests = [
            T(r#""monkey";"#, Constant::Str(0)),
            T(r#""mon" + "key";"#, Constant::Str(2)),
            T(r#""mon" + "key" + "banana";"#, Constant::Str(4)),
        ];

        run_vm_tests(&tests);
    }

    #[test]
    fn array_expressions_work() {
        let tests = [
            T("[];", Constant::Array(0)),
            T("[1, 2, 3];", Constant::Array(0)),
            T("[1 + 2, 3 * 4, 5 * 6];", Constant::Array(0)),
            T("[[1 + 2], [3 * 4], [5 * 6]];", Constant::Array(3)),
        ];

        run_vm_tests(&tests);
    }

    #[test]
    fn index_expressions_work() {
        let tests = [
            T("[1, 2, 3][1];", Constant::Int(2)),
            T("[1, 2, 3][0 + 2];", Constant::Int(3)),
            T("[[1, 1, 1]][0][0];", Constant::Int(1)),
            T("[][0];", Constant::Null),
            T("[1, 2, 3][99];", Constant::Null),
            T("[1][[0][0]];", Constant::Int(1)),
        ];

        run_vm_tests(&tests);
    }

    #[test]
    fn calling_functions_without_arguments_works() {
        let tests = [
            T(
                "let fivePlusTen = fn() { 5 + 10; }; fivePlusTen();",
                Constant::Int(15),
            ),
            T(
                "let one = fn() { 1; }; let two = fn() { 2; }; one() + two();",
                Constant::Int(3),
            ),
            T(
                "let a = fn() { 1 }; let b = fn() { a () + 1 }; let c = fn() { b() + 1 }; c();",
                Constant::Int(3),
            ),
            T(
                "let earlyExit = fn() { return 99; 100; }; earlyExit();",
                Constant::Int(99),
            ),
            T(
                "let earlyExit = fn() { return 99; return 100; }; earlyExit();",
                Constant::Int(99),
            ),
        ];

        run_vm_tests(&tests);
    }

    #[test]
    fn calling_functions_without_return_value_works() {
        let tests = [
            T(
                "let noReturn = fn() {}; noReturn();",
                Constant::Null,
            ),
            T(
                "let noReturn = fn() {}; let noReturnTwo = fn() { noReturn(); }; noReturn(); noReturnTwo();",
                Constant::Null,
            ),
        ];

        run_vm_tests(&tests);
    }

    #[test]
    fn first_class_functions_calling_works() {
        let tests = [T(
            "
                let returnsOne = fn() { 1; };
                let returnsOneReturner = fn() { returnsOne; };
                returnsOneReturner()();
                ",
            Constant::Int(1),
        )];

        run_vm_tests(&tests);
    }

    #[test]
    fn calling_functions_with_bindings_works() {
        let tests = [
            T(
                "let one = fn() { let one = 1; one; }; one();",
                Constant::Int(1),
            ),
            T(
                "let oneAndTwo = fn() { let one = 1; let two = 2; one + two; }; oneAndTwo();",
                Constant::Int(3),
            ),
            T(
                "
                let oneAndTwo = fn() { let one = 1; let two = 2; one + two; };
                let threeAndFour = fn() { let three = 3; let four = 4; three + four;};
                oneAndTwo() + threeAndFour();
                ",
                Constant::Int(10),
            ),
            T(
                "
                let firstFoobar = fn() { let foobar = 50; foobar; };
                let secondFoobar = fn() { let foobar = 100; foobar; };
                firstFoobar() + secondFoobar();
                ",
                Constant::Int(150),
            ),
            T(
                "
                    let globalSeed = 50;
                    let minusOne = fn() {
                        let num = 1;
                        globalSeed - num;
                    };
                    let minusTwo = fn() {
                        let num = 2;
                        globalSeed - num;
                    };
                    minusOne() + minusTwo();
                ",
                Constant::Int(97),
            ),
        ];

        run_vm_tests(&tests);
    }

    #[test]
    fn calling_functions_with_arguments_works() {
        let tests = [
            T(
                "
                    let identity = fn(a) { a; };
                    identity(4);
                ",
                Constant::Int(4),
            ),
            T(
                "
                    let sum = fn(a, b) { a + b; };
                    sum(1, 2);
                ",
                Constant::Int(3),
            ),
            T(
                "
                    let globalNum = 10;
                    let sum = fn(a, b) {
                        let c = a + b;
                        c + globalNum;
                    };

                    let outer = fn() {
                        sum(1, 2) + sum(3, 4) + globalNum;
                    };

                    outer() + globalNum;
                ",
                Constant::Int(50),
            ),
        ];

        run_vm_tests(&tests);
    }

    #[test]
    fn calling_functions_with_wrong_arguments() {
        struct Test(&'static str, &'static str);

        let tests = [
            Test(
                "fn() {1;}(1);",
                "wrong number of arguments: expected 0, but got 1",
            ),
            Test(
                "fn(a) { a; }();",
                "wrong number of arguments: expected 1, but got 0",
            ),
            Test(
                "fn(a, b) { a + b; }(1);",
                "wrong number of arguments: expected 2, but got 1",
            ),
        ];

        for test in tests.iter() {
            let lexer = Lexer::new(test.0);
            let parser = Parser::new(lexer);
            let prog = parser.parse_program().unwrap();
            let compiler = Compiler::default();
            let bytecode = compiler.compile_program(&prog).unwrap();
            let mut globals = vec![Constant::Int(0); GLOBALS_SIZE];
            let vm = Vm::for_bytecode(bytecode, &mut globals);
            let ret = vm.run();
            if let Err(s) = ret {
                assert_eq!(test.1, s);
            } else {
                panic!("expected VM error but did not get one");
            }
        }
    }

    #[test]
    fn creating_closures_works() {
        let tests = [
            T(
                "
                let newClosure = fn(a) {
                    fn() { a; };
                };
                let closure = newClosure(99);
                closure();
                ",
                Constant::Int(99),
            ),
            T(
                "
                let newAdder = fn(a, b) {
                    fn(c) { a + b + c };
                };
                let adder = newAdder(1, 2);
                adder(8);
                ",
                Constant::Int(11),
            ),
            T(
                "let newAdder = fn(a, b) {
                    let c = a + b;
                    fn(d) { c + d };
                };
                let adder = newAdder(1, 2);
                adder(8);",
                Constant::Int(11),
            ),
            T(
                "
                let newAdderOuter = fn(a, b) {
                    let c = a + b;
                    fn(d) {
                        let e = d + c;
                        fn(f) { e + f; };
                    };
                };
                let newAdderInner = newAdderOuter(1, 2);
                let adder = newAdderInner(3);
                adder(8);
                ",
                Constant::Int(14),
            ),
            T(
                "
                let a = 1;
                let newAdderOuter = fn(b) {
                    fn(c) {
                        fn(d) { a + b + c + d };
                    };
                };
                let newAdderInner = newAdderOuter(2);
                let adder = newAdderInner(3);
                adder(8);
                ",
                Constant::Int(14),
            ),
            T(
                "
                let newClosure = fn(a, b) {
                    let one = fn() { a; };
                    let two = fn() { b; };
                    fn() { one() + two(); };
                };
                let closure = newClosure(9, 90);
                closure();
                ",
                Constant::Int(99),
            ),
        ];
        run_vm_tests(&tests);
    }

    #[test]
    fn creating_recursive_closures_works() {
        let tests = [
            T(
                "let countDown = fn(x) {
                    if (x == 0) {
                        return 0;
                    } else {
                        countDown(x - 1);
                    }
                };
                countDown(1);",
                Constant::Int(0),
            ),
            T(
                "
                   let countDown = fn(x) {
                   if (x == 0) {
                       return 0;
                   } else {
                       countDown(x - 1);
                   }
                   };
                   let wrapper = fn() {
                   countDown(1);
                   };
                   wrapper();
                   ",
                Constant::Int(0),
            ),
            T(
                "
                   let wrapper = fn() {
                       let countDown = fn(x) {
                           if (x == 0) {
                               return 0;
                           } else {
                               countDown(x - 1);
                           }
                       };
                       countDown(1);
                   };
                   wrapper();
                   ",
                Constant::Int(0),
            ),
        ];
        run_vm_tests(&tests);
    }

    fn run_vm_tests(tests: &[T]) {
        for (i, test) in tests.iter().enumerate() {
            let lexer = Lexer::new(test.0);
            let parser = Parser::new(lexer);
            let prog = parser.parse_program().unwrap();
            let compiler = Compiler::default();
            let bytecode = compiler.compile_program(&prog).unwrap();
            let mut globals = vec![Constant::Int(0); GLOBALS_SIZE];
            let vm = Vm::for_bytecode(bytecode, &mut globals);
            let (stack_top, _) = vm.run().unwrap();
            assert_eq!(
                test.1, stack_top,
                "unexpected value: {} (expected: {}) (test #{})",
                stack_top, test.1, i
            );
        }
    }
}
