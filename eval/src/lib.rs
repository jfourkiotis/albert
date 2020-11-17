use ast::*;
use object::env::Env;
use object::*;
use sema::{NameResolution, VarResolution};
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryInto;
use std::rc::Rc;
use string_interner::Sym;

const MAX_FRAMES: usize = 1024;
const STACK_SIZE: usize = 2048;

struct Frame<'a> {
    name: &'a str,
    base_pointer: usize,
}

pub struct Interpreter<'a> {
    rax: Option<Value<'a>>,
    pub(crate) nil: Value<'a>,
    ret: Value<'a>,
    pos: Value<'a>, // true
    neg: Value<'a>, // false
    builtins: HashMap<&'static str, Value<'a>>,
    resolved: VarResolution,
    frames: Vec<Frame<'a>>,
    frame_index: usize,
    stack: Vec<Value<'a>>,
    sp: usize,
}

impl<'a> Interpreter<'a> {
    pub fn new(resolved: VarResolution) -> Self {
        let mut obj = Interpreter {
            rax: None,
            nil: Object::Null,
            ret: Object::Return,
            pos: Object::True,
            neg: Object::False,
            builtins: HashMap::new(),
            resolved,
            frames: vec![],
            frame_index: 0,
            stack: vec![],
            sp: 0,
        };

        // Only functions use frames, but we add a dummy frame for the main script file
        obj.frames.resize_with(MAX_FRAMES, || Frame {
            name: "",
            base_pointer: 0,
        });
        // The base pointer here means nothing, it will never be used.
        obj.frames[0] = Frame {
            name: "main",
            base_pointer: 0,
        };
        // the stack is only used by functions
        obj.stack.resize_with(STACK_SIZE, || Object::Num(0.0));

        fn len<'a>(args: &[Value<'a>]) -> Result<Value<'a>, String> {
            if args.len() != 1 {
                Err(format!(
                    "len: expected {} argument(s), got {}",
                    1,
                    args.len()
                ))
            } else {
                match &args[0] {
                    Object::Str(s) => Ok(Object::Num(s.len() as f64)),
                    Object::Array(a) => Ok(Object::Num(a.len() as f64)),
                    _other => Err("len: invalid argument, expected STRING".to_string()),
                }
            }
        }

        obj.builtins
            .insert("len", Object::Builtin(Rc::new(BuiltinDef("len", len))));
        obj
    }
    pub fn eval_program(
        &mut self, prog: &Program<'a>, env: Rc<RefCell<Env<'a>>>,
    ) -> Result<Value<'a>, RuntimeError> {
        match self.eval_statements(&prog.statements, &prog.stmt_nodes, &prog.expr_nodes, env) {
            Err(message) => {
                let stacktrace = self.frames[..=self.frame_index]
                    .iter()
                    .map(|f| f.name.to_string())
                    .collect::<Vec<String>>();
                Err(RuntimeError {
                    message,
                    stacktrace,
                })
            }
            Ok(r) => Ok(r),
        }
    }

    fn eval_statements(
        &mut self, statements: &[StmtId], stmt_nodes: &[Statement], expr_nodes: &[Node<'a>],
        env: Rc<RefCell<Env<'a>>>,
    ) -> Result<Value<'a>, String> {
        let mut result = self.nil.clone();
        for (_, stmt) in statements.iter().enumerate() {
            let ret = self.eval_statement(*stmt, &stmt_nodes, &expr_nodes, env.clone())?;
            if let Object::Return = ret {
                return Ok(self.rax.take().map_or(self.nil.clone(), |r| r));
            } else {
                result = ret.clone();
            }
        }
        Ok(result)
    }

    pub fn eval_statement(
        &mut self, stmt: StmtId, stmt_nodes: &[Statement], expr_nodes: &[Node<'a>],
        env: Rc<RefCell<Env<'a>>>,
    ) -> Result<Value<'a>, String> {
        match &stmt_nodes[stmt] {
            Statement::Expression { expression, .. } => {
                if let Some(expr_id) = expression {
                    self.eval_expression(*expr_id, stmt_nodes, expr_nodes, env)
                } else {
                    Ok(self.nil.clone())
                }
            }
            Statement::Block { statements, .. } => {
                self.eval_block_statements(&statements, stmt_nodes, expr_nodes, env)
            }
            Statement::Return { return_value, .. } => {
                if let Some(rv) = return_value {
                    let result = self.eval_expression(*rv, stmt_nodes, expr_nodes, env)?;
                    self.rax.replace(result);
                }
                Ok(self.ret.clone())
            }
            Statement::Let { name, value, .. } => {
                let v = self.eval_expression(*value, stmt_nodes, expr_nodes, env.clone())?;
                if let Node::Identifier { value, .. } = expr_nodes[*name] {
                    match self.resolved[*name] {
                        NameResolution::Local { depth, index } => {
                            Ok(env.borrow_mut().insert_local((depth, index), v))
                        }
                        NameResolution::Global { symbol } => {
                            Ok(env.borrow_mut().insert_global(symbol, v))
                        }
                        NameResolution::Unresolved => panic!("unresolved variable: {}", value),
                    }
                } else {
                    unreachable!()
                }
            }
        }
    }

    fn eval_block_statements(
        &mut self, statements: &[StmtId], stmt_nodes: &[Statement], expr_nodes: &[Node<'a>],
        env: Rc<RefCell<Env<'a>>>,
    ) -> Result<Value<'a>, String> {
        let mut result = self.nil.clone();
        for (_, stmt) in statements.iter().enumerate() {
            let ret = self.eval_statement(*stmt, &stmt_nodes, &expr_nodes, env.clone())?;
            if let Object::Return = ret {
                return Ok(ret);
            } else {
                result = ret;
            }
        }
        Ok(result)
    }

    fn eval_expression(
        &mut self, expr: ExprId, stmt_nodes: &[Statement], expr_nodes: &[Node<'a>],
        env: Rc<RefCell<Env<'a>>>,
    ) -> Result<Value<'a>, String> {
        match &expr_nodes[expr] {
            Node::Num { value, .. } => Ok(Object::Num(*value)),
            Node::Str { value, .. } => Ok(Object::Str(Rc::new((*value).to_string()))),
            Node::Boolean { value, .. } => {
                if *value {
                    Ok(self.pos.clone())
                } else {
                    Ok(self.neg.clone())
                }
            }
            Node::Identifier { value, .. } => match self.resolved[expr] {
                NameResolution::Local { depth, index } => {
                    if depth == 0 {
                        Ok(self.stack[self.frames[self.frame_index].base_pointer + index].clone())
                    } else {
                        env.borrow()
                            .get_local((depth, index))
                            .ok_or_else(|| format!("unknown identifier: {}", value))
                    }
                }
                NameResolution::Global { symbol } => self.eval_identifier(symbol, value, env),
                NameResolution::Unresolved => panic!("unresolved variable {}", value),
            },
            Node::Prefix {
                operator, right, ..
            } => {
                let rhs = self.eval_expression(*right, stmt_nodes, expr_nodes, env)?;
                self.eval_prefix_expression(operator, rhs)
            }
            Node::Infix {
                left,
                operator,
                right,
                ..
            } => {
                let lhs = self.eval_expression(*left, stmt_nodes, expr_nodes, env.clone())?;
                let rhs = self.eval_expression(*right, stmt_nodes, expr_nodes, env)?;
                self.eval_infix_expression(operator, lhs, rhs)
            }
            Node::If {
                condition,
                consequence,
                alternative,
                ..
            } => {
                let cond = self.eval_expression(*condition, stmt_nodes, expr_nodes, env.clone())?;
                if self.is_truthy(&cond) {
                    self.eval_statement(*consequence, stmt_nodes, expr_nodes, env)
                } else if let Some(alt) = alternative {
                    self.eval_statement(*alt, stmt_nodes, expr_nodes, env)
                } else {
                    Ok(self.nil.clone())
                }
            }
            Node::Func { .. } => Ok(Object::Func(Rc::new(FuncDef(expr, env)))),
            Node::Array { elements, .. } => {
                let elements = self.eval_expressions(elements, stmt_nodes, expr_nodes, env)?;
                Ok(Object::Array(Rc::new(elements)))
            }
            Node::Index { array, index, .. } => {
                let array = self.eval_expression(*array, stmt_nodes, expr_nodes, env.clone())?;
                let array_type = array.type_name();
                let index = self.eval_expression(*index, stmt_nodes, expr_nodes, env)?;
                let index_type = index.type_name();
                match (array, index) {
                    (Object::Array(v), Object::Num(i)) => {
                        if i.fract() != 0.0 {
                            return Err(format!("invalid index value: {}", i));
                        }
                        let i = i as i64;
                        if i < 0 {
                            Ok(self.nil.clone())
                        } else {
                            let i = i as usize;
                            if i < v.len() {
                                Ok(v[i].clone())
                            } else {
                                Ok(self.nil.clone())
                            }
                        }
                    }
                    _other => Err(format!(
                        "index operator not supported: {} [{}]",
                        array_type, index_type,
                    )),
                }
            }
            Node::Call {
                function,
                arguments,
                ..
            } => {
                let func = self.eval_expression(*function, stmt_nodes, expr_nodes, env.clone())?;
                let type_name = func.type_name();
                match func {
                    Object::Func(fdef) => {
                        if let Node::Func {
                            parameters,
                            body,
                            name,
                            ..
                        } = &expr_nodes[fdef.0]
                        {
                            if arguments.len() != parameters.len() {
                                Err(format!(
                                    "{} argument(s) were needed, but {} were given",
                                    parameters.len(),
                                    arguments.len()
                                ))
                            } else {
                                let num_args = self.eval_expressions_fast(
                                    arguments, stmt_nodes, expr_nodes, env,
                                )?;
                                self.frame_index += 1;
                                self.frames[self.frame_index] = Frame {
                                    name,
                                    base_pointer: self.sp - num_args,
                                };
                                let result = self.eval_statement(
                                    *body,
                                    stmt_nodes,
                                    expr_nodes,
                                    fdef.1.clone(),
                                )?;
                                self.sp = self.frames[self.frame_index].base_pointer;
                                self.frame_index -= 1;
                                if let Object::Return = &result {
                                    Ok(self.rax.take().map_or(self.nil.clone(), |r| r))
                                } else {
                                    Ok(result)
                                }
                            }
                        } else {
                            Err(format!("not a function: {}", type_name))
                        }
                    }
                    Object::Builtin(bdef) => {
                        let args = self.eval_expressions(arguments, stmt_nodes, expr_nodes, env)?;
                        bdef.1(&args)
                    }
                    _other => Err(format!("not a function: {}", type_name)),
                }
            }
        }
    }

    fn eval_expressions(
        &mut self, args: &[ExprId], stmt_nodes: &[Statement], expr_nodes: &[Node<'a>],
        env: Rc<RefCell<Env<'a>>>,
    ) -> Result<Vec<Value<'a>>, String> {
        let mut objects: Vec<Value<'a>> = vec![];
        for expr in args.iter() {
            objects.push(self.eval_expression(*expr, stmt_nodes, expr_nodes, env.clone())?);
        }
        Ok(objects)
    }

    fn eval_expressions_fast(
        &mut self, args: &[ExprId], stmt_nodes: &[Statement], expr_nodes: &[Node<'a>],
        env: Rc<RefCell<Env<'a>>>,
    ) -> Result<usize, String> {
        for expr in args.iter() {
            self.stack[self.sp] =
                self.eval_expression(*expr, stmt_nodes, expr_nodes, env.clone())?;
            self.sp += 1;
        }
        Ok(args.len())
    }

    fn eval_identifier(
        &self, symbol: Sym, name: &str, env: Rc<RefCell<Env<'a>>>,
    ) -> Result<Value<'a>, String> {
        if let Some(v) = env.borrow().get_global(symbol) {
            Ok(v)
        } else if let Some(builtin) = self.builtins.get(name) {
            Ok(builtin.clone())
        } else {
            Err(format!("identifier not found: {}", name))
        }
    }

    fn is_truthy(&self, obj: &Object) -> bool {
        match obj {
            Object::Null => false,
            Object::True => true,
            Object::False => false,
            _ => true,
        }
    }

    fn eval_prefix_expression(&self, operator: &str, obj: Value<'a>) -> Result<Value<'a>, String> {
        if operator == "!" {
            Ok(self.eval_bang_operator_expression(obj))
        } else if operator == "-" {
            self.eval_minus_prefix_operator_expression(obj)
        } else {
            Err(format!("unknown operator: {}{}", operator, obj.type_name()))
        }
    }

    fn eval_bang_operator_expression(&self, obj: Value<'a>) -> Value<'a> {
        match obj {
            Object::True => self.neg.clone(),
            Object::False => self.pos.clone(),
            Object::Null => self.pos.clone(),
            _other => self.neg.clone(),
        }
    }

    fn eval_minus_prefix_operator_expression(&self, obj: Value<'a>) -> Result<Value<'a>, String> {
        if let Object::Num(v) = obj {
            Ok(Object::Num(-v))
        } else {
            Err(format!("unknown operator: -{}", obj.type_name()))
        }
    }

    fn eval_infix_expression(
        &self, operator: &str, lhs: Value<'a>, rhs: Value<'a>,
    ) -> Result<Value<'a>, String> {
        match (lhs, rhs) {
            (Object::Num(v1), Object::Num(v2)) => {
                self.eval_number_infix_expression(operator, v1, v2)
            }
            (Object::Str(s1), Object::Str(s2)) if operator == "==" => Ok(if s1 == s2 {
                self.pos.clone()
            } else {
                self.neg.clone()
            }),
            (Object::Str(s1), Object::Str(s2)) if operator == "!=" => Ok(if s1 != s2 {
                self.pos.clone()
            } else {
                self.neg.clone()
            }),
            (Object::True, Object::True) => {
                self.eval_boolean_infix_expression(operator, true, true)
            }
            (Object::False, Object::True) => {
                self.eval_boolean_infix_expression(operator, false, true)
            }
            (Object::True, Object::False) => {
                self.eval_boolean_infix_expression(operator, true, false)
            }
            (Object::False, Object::False) => {
                self.eval_boolean_infix_expression(operator, false, false)
            }
            (a @ Object::Num(_), b) | (a, b @ Object::Num(_)) => Err(format!(
                "type mismatch: {} {} {}",
                a.type_name(),
                operator,
                b.type_name()
            )),
            (a, b) => Err(format!(
                "unknown operator: {} {} {}",
                a.type_name(),
                operator,
                b.type_name(),
            )),
        }
    }

    fn eval_number_infix_expression(
        &self, operator: &str, v1: f64, v2: f64,
    ) -> Result<Value<'a>, String> {
        match operator {
            "+" => Ok(Object::Num(v1 + v2)),
            "-" => Ok(Object::Num(v1 - v2)),
            "*" => Ok(Object::Num(v1 * v2)),
            "/" => Ok(Object::Num(v1 / v2)),
            ">" => Ok(if v1 > v2 {
                self.pos.clone()
            } else {
                self.neg.clone()
            }),
            "^" => Ok(Object::Num(v1.powf(v2.try_into().unwrap()))),
            "<" => Ok(if v1 < v2 {
                self.pos.clone()
            } else {
                self.neg.clone()
            }),
            "==" => Ok(if v1 == v2 {
                self.pos.clone()
            } else {
                self.neg.clone()
            }),
            "!=" => Ok(if v1 != v2 {
                self.pos.clone()
            } else {
                self.neg.clone()
            }),
            other => Err(format!("unknown operator: NUMBER {} NUMBER", other)),
        }
    }

    fn eval_boolean_infix_expression(
        &self, operator: &str, v1: bool, v2: bool,
    ) -> Result<Value<'a>, String> {
        match operator {
            "!=" => Ok(if v1 != v2 {
                self.pos.clone()
            } else {
                self.neg.clone()
            }),
            "==" => Ok(if v1 == v2 {
                self.pos.clone()
            } else {
                self.neg.clone()
            }),
            _ => Err(format!("unknown operator: BOOLEAN {} BOOLEAN", operator)),
        }
    }
}

#[cfg(test)]
mod tests {
    use lexer::Lexer;
    use parser::Parser;
    use sema::Resolver;

    use super::*;

    fn test_eval_or_error(input: &str) -> Result<Value, RuntimeError> {
        let lexer = Lexer::new(input);
        let parser = Parser::new(lexer);
        let prog = parser
            .parse_program()
            .unwrap_or_else(|_err| panic!("errors encountered during parsing"));

        let resolver = Resolver::default();
        match resolver.resolve_program(&prog) {
            Ok(resolved) => {
                let mut interpreter = Interpreter::new(resolved);
                interpreter.eval_program(&prog, Rc::new(RefCell::new(Env::new_global())))
            }
            Err(error) => panic!("{}", error.message),
        }
    }

    fn test_eval(input: &str) -> Value {
        match test_eval_or_error(input) {
            Ok(o) => o,
            Err(e) => panic!("{}", e.message),
        }
    }

    fn test_number_object(obj: &Object, expected: f64) {
        assert!(
            matches!(obj, Object::Num(v) if v == &expected),
            "expected: {}, got object: {}",
            expected,
            obj.type_name(),
        );
    }

    fn test_boolean_object(obj: &Object, expected: bool) {
        if let Object::True = obj {
            assert!(expected);
        } else if let Object::False = obj {
            assert!(!expected);
        } else {
            panic!("unexpected obj: {}", obj.type_name());
        }
    }

    #[test]
    fn eval_number_expression_works() {
        struct Test(&'static str, f64);
        let t = Test;

        let tests = [
            t("5;", 5.0),
            t("10;", 10.0),
            t("-5;", -5.0),
            t("-10;", -10.0),
            t("5 + 5 + 5 + 5 - 10;", 10.0),
            t("2 * 2 * 2 * 2 * 2;", 32.0),
            t("-50 + 100 + -50;", 0.0),
            t("5 * 2 + 10;", 20.0),
            t("5 + 2 * 10;", 25.0),
            t("20 + 2 * -10;", 0.0),
            t("50 / 2 * 2 + 10;", 60.0),
            t("2 * (5 + 10);", 30.0),
            t("3 * 3 * 3 + 10;", 37.0),
            t("3 * (3 * 3) + 10;", 37.0),
            t("(5 + 10 * 2 + 15 / 3) * 2 + -10;", 50.0),
        ];
        for (_, t) in tests.iter().enumerate() {
            let result = test_eval(t.0);
            test_number_object(&result, t.1);
        }
    }

    #[test]
    fn eval_boolean_expression_works() {
        struct Test(&'static str, bool);
        let t = Test;

        let tests = [
            t("false;", false),
            t("true;", true),
            t("1 < 2;", true),
            t("1 > 2;", false),
            t("1 == 1;", true),
            t("1 != 1;", false),
            t("1 == 2;", false),
            t("1 != 2;", true),
            t("true == true;", true),
            t("false == false;", true),
            t("true == false;", false),
            t("false != true;", true),
            t("(1 < 2) == true;", true),
            t("(1 < 2) == false;", false),
            t("(1 > 2) == true;", false),
            t("(1 > 2) == false;", true),
        ];
        for (_, t) in tests.iter().enumerate() {
            let result = test_eval(t.0);
            test_boolean_object(&result, t.1);
        }
    }

    #[test]
    fn eval_bang_operator_works() {
        struct Test(&'static str, bool);
        let t = Test;

        let tests = [
            t("!true;", false),
            t("!false;", true),
            t("!5;", false),
            t("!!true;", true),
            t("!!false;", false),
            t("!!5;", true),
        ];

        for (_, t) in tests.iter().enumerate() {
            let result = test_eval(t.0);
            test_boolean_object(&result, t.1);
        }
    }

    #[test]
    fn eval_if_else_expression_works() {
        struct Test(&'static str, Option<f64>);
        let t = Test;

        let tests = [
            t("if (true) { 10 };", Some(10.0)),
            t("if (false) { 10 };", None),
            t("if (1) { 10 };", Some(10.0)),
            t("if (1 < 2) { 10 };", Some(10.0)),
            t("if (1 > 2) { 10 };", None),
            t("if (1 > 2) { 10 } else { 20 };", Some(20.0)),
            t("if (1 < 2) { 10 } else { 20 };", Some(10.0)),
        ];

        for (_, test) in tests.iter().enumerate() {
            let result = test_eval(test.0);
            if let Some(expected) = test.1 {
                assert!(matches!(result, Object::Num(v) if v == expected));
            } else {
                assert!(matches!(result, Object::Null));
            }
        }
    }

    #[test]
    fn eval_return_expression_works() {
        struct Test(&'static str, Option<f64>);
        let t = Test;

        let tests = [
            t("fn() { return 10; } ();", Some(10.0)),
            t("fn() { return 10; 9; }();", Some(10.0)),
            t("fn() { return 2 * 5; 9; }();", Some(10.0)),
            t("fn() { 9; return 2 * 5; 0; } ();", Some(10.0)),
            t("fn() {10; return; return 20 + 2; }();", None),
            t(
                "fn() {
                if (10 > 1) {
                if (10 > 1) {
                    return 10;
                };
                return 1;
              }}();",
                Some(10.0),
            ),
        ];

        for (_, test) in tests.iter().enumerate() {
            let result = test_eval(test.0);
            if let Some(expected) = test.1 {
                test_number_object(&result, expected);
            } else {
                assert!(matches!(result, Object::Null));
            }
        }
    }

    #[test]
    fn error_handling_works() {
        struct Test(&'static str, &'static str);
        let t = Test;

        let tests = [
            t("5 + true;", "type mismatch: NUMBER + BOOLEAN"),
            t("5 + true; 5;", "type mismatch: NUMBER + BOOLEAN"),
            t("-true;", "unknown operator: -BOOLEAN"),
            t("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
            t("5; true + false; 5;", "unknown operator: BOOLEAN + BOOLEAN"),
            t(
                "if (10 > 1) { true + false; };",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            t(
                "
            fn() {
            if (10 > 1) {
                if (10 > 1) {
                    return true + false;
                };
                return 1;
            } }();",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            t("foobar;", "identifier not found: foobar"),
        ];

        for (_, test) in tests.iter().enumerate() {
            if let Err(e) = test_eval_or_error(test.0) {
                assert!(
                    &e.message == test.1,
                    "expected: '{}', got: '{}'",
                    test.1,
                    e.message
                );
            } else {
                panic!("error expected!");
            }
        }
    }

    #[test]
    fn eval_let_statement_works() {
        struct Test(&'static str, f64);
        let t = Test;

        let tests = [
            t("let a = 5; a;", 5.0),
            t("let a = 5 * 5; a;", 25.0),
            t("let a = 5; let b = a; b;", 5.0),
            t("let a = 5; let b = a; let c = a + b + 5; c;", 15.0),
        ];

        for (_, test) in tests.iter().enumerate() {
            test_number_object(&test_eval(test.0), test.1);
        }
    }

    #[test]
    fn eval_function_application_works() {
        struct Test(&'static str, f64);
        let t = Test;

        let tests = [
            t("let identity = fn(x) { return x; }; identity(5);", 5.0),
            t("let double = fn(x) { return x * 2; }; double(5);", 10.0),
            t("let add = fn(x, y) { return x + y; }; add(5, 5);", 10.0),
            t(
                "let add = fn(x, y) { return x + y; }; add(5 + 5, add(5, 5));",
                20.0,
            ),
            t("fn(x) { return x; }(5);", 5.0),
        ];

        for (_, test) in tests.iter().enumerate() {
            test_number_object(&test_eval(test.0), test.1);
        }
    }

    #[test]
    #[ignore]
    fn eval_fib33() {
        let input = "
            let fib = fn(n) { 
                if (n < 2) {
                    return n;
                } else {
                    return fib(n-1) + fib(n-2);
                }
            };
            fib(33);
        ";

        test_number_object(&test_eval(input), 3_524_578f64);
    }
}
