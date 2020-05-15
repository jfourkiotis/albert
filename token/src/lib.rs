use std::collections::HashMap;

#[derive(PartialEq, Debug, Copy, Clone, Eq, Hash)]
#[repr(u8)]
pub enum TokenType {
    Illegal,
    Eof,
    // Identifiers + literals
    Ident,
    Int,
    Str,
    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Power, // ^
    LessThan,
    GreaterThan,
    Equal,
    NotEqual,

    // Delimiters
    Comma,
    Semicolon,

    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Lbracket,
    Rbracket,

    // Keywords
    Function,
    Let,
    Return,
    True,
    False,
    If,
    Else,
}

#[derive(Debug)]
pub struct Token<'a> {
    pub token_type: TokenType,
    pub literal: &'a str,
}

#[macro_use]
extern crate lazy_static;

lazy_static! {
    pub static ref KEYWORDS: HashMap<&'static str, TokenType> = {
        let mut m = HashMap::new();
        m.insert("fn", TokenType::Function);
        m.insert("let", TokenType::Let);
        m.insert("return", TokenType::Return);
        m.insert("true", TokenType::True);
        m.insert("false", TokenType::False);
        m.insert("if", TokenType::If);
        m.insert("else", TokenType::Else);
        m
    };
}
