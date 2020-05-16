use std::collections::HashMap;

#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum OpCode {
    Constant = 0,
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    True,
    False,
    Equal,
    NotEqual,
    GreaterThan, // 10
    LessThan,
    Minus,
    Bang,
    JumpIfFalse,
    Jump,
    Null,
    Pop,
    GetGlobal,
    SetGlobal,
    Array, // 20
    Index,
    Call,
    Return,
    ReturnValue,
    GetLocal,
    SetLocal,
    Closure,
    GetFree,
    CurrentClosure,
    Max, // this must be last
}

pub const BYTE_TO_OPCODE: [OpCode; OpCode::Max as usize] = [
    OpCode::Constant,
    OpCode::Add,
    OpCode::Sub,
    OpCode::Mul,
    OpCode::Div,
    OpCode::Pow,
    OpCode::True,
    OpCode::False,
    OpCode::Equal,
    OpCode::NotEqual,
    OpCode::GreaterThan,
    OpCode::LessThan,
    OpCode::Minus,
    OpCode::Bang,
    OpCode::JumpIfFalse,
    OpCode::Jump,
    OpCode::Null,
    OpCode::Pop,
    OpCode::GetGlobal,
    OpCode::SetGlobal,
    OpCode::Array,
    OpCode::Index,
    OpCode::Call,
    OpCode::Return,
    OpCode::ReturnValue,
    OpCode::GetLocal,
    OpCode::SetLocal,
    OpCode::Closure,
    OpCode::GetFree,
    OpCode::CurrentClosure,
];

pub fn make_instruction(op: OpCode, operands: &[usize]) -> Result<Vec<u8>, String> {
    let def = lookup(op as u8)?;

    let length = std::mem::size_of::<OpCode>() + def.operand_widths.iter().sum::<usize>();
    let mut instruction: Vec<u8> = vec![0; length];

    instruction[0] = op as u8;
    let mut offset = 1usize;
    for (i, operand) in operands.iter().enumerate() {
        let width = def.operand_widths[i];
        if width == 2 {
            let bytes = (*operand as u16).to_be_bytes();
            instruction[offset] = bytes[0];
            instruction[offset + 1] = bytes[1];
        } else if width == 1 {
            instruction[offset] = *operand as u8;
        }
        offset += width;
    }

    Ok(instruction)
}

// Metadata about OpCodes
// .0: instruction name (e.g for Constant -> CONSTANT )
// .1: operand widths
pub struct Definition {
    pub name: &'static str,
    pub operand_widths: Vec<usize>,
}

#[macro_use]
extern crate lazy_static;

lazy_static! {
    static ref DEFINITIONS: HashMap<u8, Definition> = {
        let mut definitions = HashMap::new();
        definitions.insert(
            OpCode::Constant as u8,
            Definition {
                name: "CONSTANT",
                operand_widths: vec![2],
            },
        );
        definitions.insert(
            OpCode::Add as u8,
            Definition {
                name: "ADD",
                operand_widths: vec![],
            },
        );
        definitions.insert(
            OpCode::Sub as u8,
            Definition {
                name: "SUB",
                operand_widths: vec![],
            },
        );
        definitions.insert(
            OpCode::Mul as u8,
            Definition {
                name: "MUL",
                operand_widths: vec![],
            },
        );
        definitions.insert(
            OpCode::Div as u8,
            Definition {
                name: "DIV",
                operand_widths: vec![],
            },
        );
        definitions.insert(
            OpCode::Pow as u8,
            Definition {
                name: "POW",
                operand_widths: vec![],
            },
        );
        definitions.insert(
            OpCode::True as u8,
            Definition {
                name: "TRUE",
                operand_widths: vec![],
            },
        );
        definitions.insert(
            OpCode::False as u8,
            Definition {
                name: "FALSE",
                operand_widths: vec![],
            },
        );
        definitions.insert(
            OpCode::Equal as u8,
            Definition {
                name: "EQUAL",
                operand_widths: vec![],
            },
        );
        definitions.insert(
            OpCode::NotEqual as u8,
            Definition {
                name: "NOT_EQUAL",
                operand_widths: vec![],
            },
        );
        definitions.insert(
            OpCode::GreaterThan as u8,
            Definition {
                name: "GREATER_THAN",
                operand_widths: vec![],
            },
        );
        definitions.insert(
            OpCode::LessThan as u8,
            Definition {
                name: "LESS_THAN",
                operand_widths: vec![],
            },
        );
        definitions.insert(
            OpCode::Minus as u8,
            Definition {
                name: "MINUS",
                operand_widths: vec![],
            },
        );
        definitions.insert(
            OpCode::Bang as u8,
            Definition {
                name: "BANG",
                operand_widths: vec![],
            },
        );
        definitions.insert(
            OpCode::JumpIfFalse as u8,
            Definition {
                name: "JUMP_IF_FALSE",
                operand_widths: vec![2],
            },
        );
        definitions.insert(
            OpCode::Jump as u8,
            Definition {
                name: "JUMP",
                operand_widths: vec![2],
            },
        );
        definitions.insert(
            OpCode::Null as u8,
            Definition {
                name: "NULL",
                operand_widths: vec![],
            },
        );
        definitions.insert(
            OpCode::Pop as u8,
            Definition {
                name: "POP",
                operand_widths: vec![],
            },
        );
        definitions.insert(
            OpCode::GetGlobal as u8,
            Definition {
                name: "GET_GLOBAL",
                operand_widths: vec![2],
            },
        );
        definitions.insert(
            OpCode::SetGlobal as u8,
            Definition {
                name: "SET_GLOBAL",
                operand_widths: vec![2],
            },
        );
        definitions.insert(
            OpCode::Array as u8,
            Definition {
                name: "ARRAY",
                operand_widths: vec![2],
            },
        );
        definitions.insert(
            OpCode::Index as u8,
            Definition {
                name: "INDEX",
                operand_widths: vec![],
            },
        );
        definitions.insert(
            OpCode::Call as u8,
            Definition {
                name: "CALL",
                operand_widths: vec![1],
            },
        );
        definitions.insert(
            OpCode::Return as u8,
            Definition {
                name: "RET",
                operand_widths: vec![],
            },
        );
        definitions.insert(
            OpCode::ReturnValue as u8,
            Definition {
                name: "RETVAL",
                operand_widths: vec![],
            },
        );
        definitions.insert(
            OpCode::GetLocal as u8,
            Definition {
                name: "GET_LOCAL",
                operand_widths: vec![1],
            },
        );
        definitions.insert(
            OpCode::SetLocal as u8,
            Definition {
                name: "SET_LOCAL",
                operand_widths: vec![1],
            },
        );
        definitions.insert(
            OpCode::Closure as u8,
            Definition {
                name: "CLOSURE",
                operand_widths: vec![2, 1], //two operands !!! [compiled function index, free vars num]
            },
        );
        definitions.insert(
            OpCode::GetFree as u8,
            Definition {
                name: "GET_FREE",
                operand_widths: vec![1],
            },
        );
        definitions.insert(
            OpCode::CurrentClosure as u8,
            Definition {
                name: "CURRENT_CLOSURE",
                operand_widths: vec![],
            },
        );
        definitions
    };
}

pub fn lookup(op: u8) -> Result<&'static Definition, String> {
    DEFINITIONS
        .get(&op)
        .ok_or_else(|| format!("opcode {} undefined", op))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn make_works() {
        // .0: OpCode
        // .1: operand value as integer
        // .2: expected operand value as bytes
        //
        // From .0 & .1 we create an instruction by calling `make`
        // and the result must match .2
        struct Test(OpCode, Vec<usize>, Vec<u8>);
        let t = Test;

        let tests = [
            t(
                OpCode::Constant,
                vec![65534],
                vec![OpCode::Constant as u8, 255, 254],
            ),
            t(OpCode::Add, vec![], vec![OpCode::Add as u8]),
            t(OpCode::Pop, vec![], vec![OpCode::Pop as u8]),
            t(
                OpCode::GetLocal,
                vec![255],
                vec![OpCode::GetLocal as u8, 255],
            ),
            t(
                OpCode::Closure,
                vec![65534, 255],
                vec![OpCode::Closure as u8, 255, 254, 255],
            ),
        ];

        for test in tests.iter() {
            let instruction = make_instruction(test.0, &test.1).unwrap();

            assert_eq!(
                test.2.len(),
                instruction.len(),
                "instruction with wrong length: expected {} but got {}",
                test.2.len(),
                instruction.len()
            );

            for (i, b) in test.2.iter().enumerate() {
                assert_eq!(
                    *b, instruction[i],
                    "wrong byte at pos {}: expected {} but got {}",
                    i, *b, instruction[i],
                );
            }
        }
    }
}
