use std::cell::RefCell;
use std::fmt::Display;
use std::rc::Rc;

#[cfg(not(target_os = "linux"))]
use mimalloc::MiMalloc;

#[cfg(not(target_os = "linux"))]
#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

pub mod env;

pub type Value<'a> = Object<'a>;

pub struct RuntimeError {
    pub message: String,
    pub stacktrace: Vec<String>,
}

// builtin signature
type BuiltinFunc<'a> = fn(&[Value<'a>]) -> Result<Value<'a>, String>;

pub struct FuncDef<'a>(pub usize, pub Rc<RefCell<env::Env<'a>>>);
pub struct BuiltinDef<'a>(pub &'static str, pub BuiltinFunc<'a>);

#[derive(Clone)]
pub enum Object<'a> {
    Num(f64),
    Str(Rc<String>),
    True,
    False,
    Null,
    Return,
    Func(Rc<FuncDef<'a>>), // index of Node::Func in expr_nodes
    // name, callback
    Builtin(Rc<BuiltinDef<'a>>),
    Array(Rc<Vec<Value<'a>>>),
}

#[allow(dead_code)]
const OBJ_SIZE_CHECK: usize = (std::mem::size_of::<Object>() == 16) as usize - 1;

impl<'a> Object<'a> {
    pub fn type_name(&self) -> &'static str {
        match self {
            Object::Num(_) => "NUMBER",
            Object::Str(_) => "STRING",
            Object::True => "BOOLEAN",
            Object::False => "BOOLEAN",
            Object::Null => "NULL",
            Object::Func(_) => "FUNC",
            Object::Builtin(_) => "BUILTIN",
            Object::Array(_) => "ARRAY",
            Object::Return => panic!("Return.name!"),
        }
    }
}

impl<'a> Display for Object<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Num(v) => write!(f, "{}", v),
            Object::Str(s) => write!(f, "{}", s),
            Object::True => write!(f, "true"),
            Object::False => write!(f, "false"),
            Object::Null => write!(f, "null"),
            Object::Return => write!(f, "return"),
            Object::Func(_) => write!(f, "func"),
            Object::Array(_) => write!(f, "array"),
            Object::Builtin(b) => write!(f, "builtin({})", b.0),
        }
    }
}
