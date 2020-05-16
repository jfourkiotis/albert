use ast::Program;
use compiler::{Compiler, Constant};
use eval::Interpreter;
use lexer::Lexer;
use parser::Parser;
use sema::{Resolver, VarResolution};
use std::cell::RefCell;
use std::error::Error;
use std::fs;
use std::path::PathBuf;
use std::rc::Rc;
use std::time::Instant;
use vm::{Vm, GLOBALS_SIZE};

use colored::*;
use rustyline::error::ReadlineError;
use rustyline::{CompletionType, Config, Editor};
use structopt::StructOpt;

const PROMPT: &str = ">> ";

#[derive(Debug)]
enum Backend {
    Tw,  // tree-walk
    Vm,  // bytecode vm
    Vmd, // debug bytecode vm
}

impl std::str::FromStr for Backend {
    type Err = BackendError;

    fn from_str(s: &str) -> Result<Backend, BackendError> {
        match s {
            "tw" => Ok(Backend::Tw),
            "vm" => Ok(Backend::Vm),
            "vmd" => Ok(Backend::Vmd),
            _ => {
                let details = "Choose between 'tw' and 'vm'".to_string();
                Err(BackendError::InvalidArg { details })
            }
        }
    }
}

#[derive(Debug)]
enum BackendError {
    InvalidArg { details: String },
}

impl std::string::ToString for BackendError {
    fn to_string(&self) -> String {
        match self {
            BackendError::InvalidArg { details } => details.to_string(),
        }
    }
}

#[derive(StructOpt)]
struct Cli {
    /// Show elapsed time for each command
    #[structopt(short, long)]
    show_elapsed_time: bool,
    /// A script to execute
    #[structopt(parse(from_os_str))]
    input: Option<PathBuf>,
    /// The backend selection. Use `tw` for a tree-walking
    /// interpreter, and `vm` for a bytecode vm
    #[structopt(short, long)]
    backend: Option<Backend>, // default: Tw
}

fn main() -> Result<(), Box<dyn Error>> {
    let cli_options = Cli::from_args();

    match cli_options.backend {
        None | Some(Backend::Tw) => start_tw(cli_options.input, cli_options.show_elapsed_time),
        Some(Backend::Vm) => start_vm(cli_options.input, cli_options.show_elapsed_time),
        Some(Backend::Vmd) => start_vmd(cli_options.input, cli_options.show_elapsed_time),
    }
}

fn start_tw(input: Option<PathBuf>, show_elapsed_time: bool) -> Result<(), Box<dyn Error>> {
    if let Some(path) = input {
        exec_script(path, show_elapsed_time, run_tw_backend)
    } else {
        repl(show_elapsed_time, run_tw_backend)
    }
}

fn start_vm(input: Option<PathBuf>, show_elapsed_time: bool) -> Result<(), Box<dyn Error>> {
    let mut globals = vec![Constant::Int(0); GLOBALS_SIZE];
    if let Some(path) = input {
        exec_script(path, show_elapsed_time, move |p, r, s| {
            run_vm_backend(p, r, s, &mut globals)
        })
    } else {
        repl(show_elapsed_time, move |p, r, s| {
            run_vm_backend(p, r, s, &mut globals)
        })
    }
}

fn start_vmd(input: Option<PathBuf>, show_elapsed_time: bool) -> Result<(), Box<dyn Error>> {
    if let Some(path) = input {
        exec_script(path, show_elapsed_time, run_vmd_backend)
    } else {
        repl(show_elapsed_time, run_vmd_backend)
    }
}

// `run_frontend` is used by all backends
fn run_frontend(input: &str) -> Result<(Program, VarResolution), Vec<String>> {
    let lexer = Lexer::new(input);
    let parser = Parser::new(lexer);
    match parser.parse_program() {
        Ok(program) => {
            let resolver = Resolver::default();
            let resolution = resolver.resolve_program(&program);
            if let Err(res_error) = resolution {
                Err(vec![res_error])
            } else {
                Ok((program, resolution.unwrap()))
            }
        }
        Err(errors) => Err(errors),
    }
}

fn run_tw_backend(prog: Program, res: VarResolution, show_elapsed_time: bool) {
    let start = Instant::now();
    let mut interpreter = Interpreter::new(res);
    match interpreter.eval_program(&prog, Rc::new(RefCell::new(Default::default()))) {
        Err(rt_err) => println!("{}: {}", "runtime error".red(), rt_err),
        Ok(obj) => {
            println!("{}", obj);
            if show_elapsed_time {
                println!("[{}: {:?}]", "elapsed time".green(), start.elapsed());
            }
        }
    }
}

fn run_vm_backend(
    prog: Program, _res: VarResolution, show_elapsed_time: bool, globals: &mut Vec<Constant>,
) {
    let compiler = Compiler::default();
    match compiler.compile_program(&prog) {
        Ok(bytecode) => {
            let vm = Vm::for_bytecode(bytecode, globals);
            let start = Instant::now();
            let stack_top = vm.run();
            let elapsed = start.elapsed();
            match stack_top {
                Err(rerr) => println!("{}: {}", "runtime error".red(), rerr),
                Ok((_, s)) => {
                    println!("{}", s);
                    if show_elapsed_time {
                        println!("[{}: {:?}]", "elapsed time".green(), elapsed);
                    }
                }
            }
        }
        Err(cerr) => println!("{}: {}", "compilation error".red(), cerr),
    }
}

fn run_vmd_backend(prog: Program, _res: VarResolution, show_elapsed_time: bool) {
    let compiler = Compiler::default();
    let start = Instant::now();
    match compiler.compile_program(&prog) {
        Ok(bytecode) => {
            let elapsed = start.elapsed();
            println!("{}", bytecode);
            if show_elapsed_time {
                println!("[{}: {:?}]", "compilation time".yellow(), elapsed);
            }
        }
        Err(cerr) => println!("{}: {}", "compilation error".red(), cerr),
    }
}

fn repl<B: FnMut(Program, VarResolution, bool)>(
    show_elapsed_time: bool, mut backend: B,
) -> Result<(), Box<dyn Error>> {
    println!(
        "Hello {}!. This is the Albert programming language!",
        whoami::username()
    );
    println!("Feel free to type in commands");
    let config = Config::builder()
        .history_ignore_space(true)
        .completion_type(CompletionType::List)
        .build();
    let mut rl = Editor::<()>::with_config(config);
    let _err = rl.load_history("history.txt");
    loop {
        let readline = rl.readline(PROMPT);
        match readline {
            Ok(line) => {
                if line.is_empty() {
                    continue;
                }
                rl.add_history_entry(&line[..]);
                match run_frontend(&line[..]) {
                    Err(errors) => {
                        for e in errors.iter() {
                            println!("{}: {}", "error".red(), *e);
                        }
                    }
                    Ok((prog, res)) => backend(prog, res, show_elapsed_time),
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    rl.save_history("history.txt")?;
    Ok(())
}

fn exec_script<B: FnMut(Program, VarResolution, bool)>(
    path: PathBuf, show_elapsed_time: bool, mut run_backend: B,
) -> Result<(), Box<dyn Error>> {
    let code = fs::read_to_string(path)?;

    match run_frontend(&code[..]) {
        Err(errors) => {
            for e in errors.iter() {
                println!("{}: {}", "error".red(), *e);
            }
        }
        Ok((prog, res)) => run_backend(prog, res, show_elapsed_time),
    }
    Ok(())
}
