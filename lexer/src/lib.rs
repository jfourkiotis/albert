use token::{Token, TokenType};

pub struct Lexer<'a> {
    input: &'a str,
    // current position in input, corresponds to
    // `ch`.
    position: usize,
    // current reading position, always points to
    // the next character in the input.
    read_position: usize,
    // current byte under examination (at `position`)
    ch: u8,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        let mut lexer = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: 0,
        };
        // initialize lexer to a correct state
        lexer.read_char();
        lexer
    }

    // create a token based on the current value of
    // `ch`, and then call `read_char` to move to the next
    // byte.
    pub fn next_token(&mut self) -> Token<'a> {
        self.skip_whitespace();

        let tok;
        match self.ch as char {
            '=' => {
                let look_ahead = self.peek_char() as char;
                if look_ahead == '=' {
                    self.read_char();
                    tok = Token {
                        token_type: TokenType::Equal,
                        literal: "==",
                    }
                } else {
                    tok = Token {
                        token_type: TokenType::Assign,
                        literal: "=",
                    }
                }
            }
            '+' => {
                tok = Token {
                    token_type: TokenType::Plus,
                    literal: "+",
                }
            }
            '-' => {
                tok = Token {
                    token_type: TokenType::Minus,
                    literal: "-",
                }
            }
            '!' => {
                let look_ahead = self.peek_char() as char;
                if look_ahead == '=' {
                    self.read_char();
                    tok = Token {
                        token_type: TokenType::NotEqual,
                        literal: "!=",
                    }
                } else {
                    tok = Token {
                        token_type: TokenType::Bang,
                        literal: "!",
                    }
                }
            }
            '/' => {
                tok = Token {
                    token_type: TokenType::Slash,
                    literal: "/",
                }
            }
            '*' => {
                tok = Token {
                    token_type: TokenType::Asterisk,
                    literal: "*",
                }
            }
            '^' => {
                tok = Token {
                    token_type: TokenType::Power,
                    literal: "^",
                }
            }
            '<' => {
                tok = Token {
                    token_type: TokenType::LessThan,
                    literal: "<",
                }
            }
            '>' => {
                tok = Token {
                    token_type: TokenType::GreaterThan,
                    literal: ">",
                }
            }
            ';' => {
                tok = Token {
                    token_type: TokenType::Semicolon,
                    literal: ";",
                }
            }
            '(' => {
                tok = Token {
                    token_type: TokenType::Lparen,
                    literal: "(",
                }
            }
            ')' => {
                tok = Token {
                    token_type: TokenType::Rparen,
                    literal: ")",
                }
            }
            ',' => {
                tok = Token {
                    token_type: TokenType::Comma,
                    literal: ",",
                }
            }
            '{' => {
                tok = Token {
                    token_type: TokenType::Lbrace,
                    literal: "{",
                }
            }
            '}' => {
                tok = Token {
                    token_type: TokenType::Rbrace,
                    literal: "}",
                }
            }
            '[' => {
                tok = Token {
                    token_type: TokenType::Lbracket,
                    literal: "[",
                }
            }
            ']' => {
                tok = Token {
                    token_type: TokenType::Rbracket,
                    literal: "]",
                }
            }
            '"' => {
                tok = Token {
                    token_type: TokenType::Str,
                    literal: self.read_str(),
                }
            }
            '\0' => {
                tok = Token {
                    token_type: TokenType::Eof,
                    literal: "",
                }
            }
            c => {
                if self.char_can_start_identifier(c) {
                    let identifier = self.read_identifier();
                    tok = Token {
                        token_type: *token::KEYWORDS.get(identifier).unwrap_or(&TokenType::Ident),
                        literal: identifier,
                    };
                    return tok;
                } else if (c as char).is_digit(10) {
                    let number = self.read_number();
                    tok = Token {
                        token_type: TokenType::Int,
                        literal: number,
                    };
                    return tok;
                } else {
                    tok = Token {
                        token_type: TokenType::Illegal,
                        literal: &self.input[self.position..=self.position],
                    };
                }
            }
        }

        self.read_char();
        tok
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0;
        } else {
            // FIXME: we support only ASCII characters
            self.ch = self.input.as_bytes()[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&mut self) -> u8 {
        if self.read_position >= self.input.len() {
            0
        } else {
            self.input.as_bytes()[self.read_position]
        }
    }

    fn read_identifier(&mut self) -> &'a str {
        let position = self.position;
        let mut c = self.ch as char;
        while self.is_valid_identifier_char(c) {
            self.read_char();
            c = self.ch as char;
        }
        &self.input[position..self.position]
    }

    fn read_str(&mut self) -> &'a str {
        let position = self.position + 1;
        loop {
            self.read_char();
            let c = self.ch as char;
            if c == '"' || c == '\0' {
                break;
            }
        }
        &self.input[position..self.position]
    }

    fn read_number(&mut self) -> &'a str {
        let position = self.position;
        let mut c = self.ch as char;
        while c.is_digit(10) {
            self.read_char();
            c = self.ch as char;
        }
        &self.input[position..self.position]
    }

    fn skip_whitespace(&mut self) {
        let mut c = self.ch as char;
        while c.is_ascii_whitespace() {
            self.read_char();
            c = self.ch as char;
        }
    }

    fn char_can_start_identifier(&self, letter: char) -> bool {
        letter.is_alphabetic() || letter == '_'
    }

    fn is_valid_identifier_char(&self, letter: char) -> bool {
        self.char_can_start_identifier(letter) || letter.is_digit(10)
    }

}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Token<'a>> {
        let tok = self.next_token();
        if tok.token_type != TokenType::Eof {
            Some(tok)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn next_token_works() {
        let input = r#"let five = 5;
        let ten = 10;

        let add = fn(x, y) {
            x + y;
        };

        let result = add(five, ten);
        !-/*5;
        5 < 10 > 5;

        10 == 10;
        10 != 9;

        false; true;
        ^ 
        ^
        if (x < y) { x } else { y };

        "foobar"

        "foo bar"
        [1, 2];

        hello_world
        pi314 _12a
        "#;

        struct Test(TokenType, &'static str);
        let t = Test;

        let tests = [
            t(TokenType::Let, "let"),
            t(TokenType::Ident, "five"),
            t(TokenType::Assign, "="),
            t(TokenType::Int, "5"),
            t(TokenType::Semicolon, ";"),
            t(TokenType::Let, "let"),
            t(TokenType::Ident, "ten"),
            t(TokenType::Assign, "="),
            t(TokenType::Int, "10"),
            t(TokenType::Semicolon, ";"),
            t(TokenType::Let, "let"),
            t(TokenType::Ident, "add"),
            t(TokenType::Assign, "="),
            t(TokenType::Function, "fn"),
            t(TokenType::Lparen, "("),
            t(TokenType::Ident, "x"),
            t(TokenType::Comma, ","),
            t(TokenType::Ident, "y"),
            t(TokenType::Rparen, ")"),
            t(TokenType::Lbrace, "{"),
            t(TokenType::Ident, "x"),
            t(TokenType::Plus, "+"),
            t(TokenType::Ident, "y"),
            t(TokenType::Semicolon, ";"),
            t(TokenType::Rbrace, "}"),
            t(TokenType::Semicolon, ";"),
            t(TokenType::Let, "let"),
            t(TokenType::Ident, "result"),
            t(TokenType::Assign, "="),
            t(TokenType::Ident, "add"),
            t(TokenType::Lparen, "("),
            t(TokenType::Ident, "five"),
            t(TokenType::Comma, ","),
            t(TokenType::Ident, "ten"),
            t(TokenType::Rparen, ")"),
            t(TokenType::Semicolon, ";"),
            t(TokenType::Bang, "!"),
            t(TokenType::Minus, "-"),
            t(TokenType::Slash, "/"),
            t(TokenType::Asterisk, "*"),
            t(TokenType::Int, "5"),
            t(TokenType::Semicolon, ";"),
            t(TokenType::Int, "5"),
            t(TokenType::LessThan, "<"),
            t(TokenType::Int, "10"),
            t(TokenType::GreaterThan, ">"),
            t(TokenType::Int, "5"),
            t(TokenType::Semicolon, ";"),
            t(TokenType::Int, "10"),
            t(TokenType::Equal, "=="),
            t(TokenType::Int, "10"),
            t(TokenType::Semicolon, ";"),
            t(TokenType::Int, "10"),
            t(TokenType::NotEqual, "!="),
            t(TokenType::Int, "9"),
            t(TokenType::Semicolon, ";"),
            t(TokenType::False, "false"),
            t(TokenType::Semicolon, ";"),
            t(TokenType::True, "true"),
            t(TokenType::Semicolon, ";"),
            t(TokenType::Power, "^"),
            t(TokenType::Power, "^"),
            t(TokenType::If, "if"),
            t(TokenType::Lparen, "("),
            t(TokenType::Ident, "x"),
            t(TokenType::LessThan, "<"),
            t(TokenType::Ident, "y"),
            t(TokenType::Rparen, ")"),
            t(TokenType::Lbrace, "{"),
            t(TokenType::Ident, "x"),
            t(TokenType::Rbrace, "}"),
            t(TokenType::Else, "else"),
            t(TokenType::Lbrace, "{"),
            t(TokenType::Ident, "y"),
            t(TokenType::Rbrace, "}"),
            t(TokenType::Semicolon, ";"),
            t(TokenType::Str, "foobar"),
            t(TokenType::Str, "foo bar"),
            t(TokenType::Lbracket, "["),
            t(TokenType::Int, "1"),
            t(TokenType::Comma, ","),
            t(TokenType::Int, "2"),
            t(TokenType::Rbracket, "]"),
            t(TokenType::Semicolon, ";"),
            t(TokenType::Ident, "hello_world"),
            t(TokenType::Ident, "pi314"),
            t(TokenType::Ident, "_12a"),
            t(TokenType::Eof, ""),
        ];

        let mut lexer = Lexer::new(input);
        for (_, test) in tests.iter().enumerate() {
            let tok = lexer.next_token();
            assert_eq!(tok.token_type, test.0);
            assert_eq!(tok.literal, test.1);
        }
    }
}
