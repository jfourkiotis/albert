use std::fmt::{Display, Error};
use token::Token;

pub type StmtId = usize;
pub type ExprId = usize;

#[derive(Debug)]
pub enum Statement<'a> {
    Let {
        token: Token<'a>,
        name: ExprId,
        value: ExprId,
    },
    Return {
        token: Token<'a>,
        return_value: Option<ExprId>,
    },
    Expression {
        token: Token<'a>,
        expression: Option<ExprId>,
    },
    Block {
        token: Token<'a>,
        statements: Vec<StmtId>,
    },
}

#[derive(Debug)]
pub enum Node<'a> {
    Identifier {
        token: Token<'a>,
        value: &'a str,
    },
    Num {
        token: Token<'a>,
        value: f64,
    },
    Str {
        token: Token<'a>,
        value: &'a str,
    },
    Prefix {
        token: Token<'a>,
        operator: &'a str,
        right: ExprId,
    },
    Infix {
        token: Token<'a>,
        left: ExprId,
        operator: &'a str,
        right: ExprId,
    },
    Boolean {
        token: Token<'a>,
        value: bool,
    },
    If {
        token: Token<'a>,
        condition: ExprId,
        consequence: StmtId,
        alternative: Option<StmtId>,
    },
    Func {
        token: Token<'a>,
        parameters: Vec<ExprId>, // Identifiers
        body: StmtId,
        name: &'a str,
    },
    Call {
        token: Token<'a>,
        function: ExprId, // Identifier, or Func
        arguments: Vec<ExprId>,
    },
    Array {
        token: Token<'a>, // "["
        elements: Vec<ExprId>,
    },
    Index {
        token: Token<'a>,
        array: ExprId,
        index: ExprId,
    },
}

impl<'a> Node<'a> {
    pub fn token_literal(&self) -> &'a str {
        match *self {
            Node::Identifier { ref token, .. } => token.literal,
            Node::Num { ref token, .. } => token.literal,
            Node::Str { ref token, .. } => token.literal,
            Node::Prefix { ref token, .. } => token.literal,
            Node::Infix { ref token, .. } => token.literal,
            Node::Boolean { ref token, .. } => token.literal,
            Node::If { ref token, .. } => token.literal,
            Node::Func { ref token, .. } => token.literal,
            Node::Call { ref token, .. } => token.literal,
            Node::Array { ref token, .. } => token.literal,
            Node::Index { ref token, .. } => token.literal,
        }
    }
}

pub struct Program<'a> {
    pub statements: Vec<StmtId>,
    pub expr_nodes: Vec<Node<'a>>,
    pub stmt_nodes: Vec<Statement<'a>>,
}

impl<'a> Program<'a> {
    fn fmt_let_statement(
        &self, literal: &str, name: ExprId, value: ExprId, f: &mut std::fmt::Formatter<'_>,
    ) -> Result<(), Error> {
        write!(f, "{} ", literal)?;
        self.fmt_identifier(name, f)?;
        write!(f, " = ")?;
        self.fmt_expression(value, f)?;
        write!(f, ";")?;
        Ok(())
    }

    fn fmt_return_statement(
        &self, literal: &str, return_value: &Option<ExprId>, f: &mut std::fmt::Formatter<'_>,
    ) -> Result<(), Error> {
        write!(f, "{} ", literal)?;
        if let Some(expr_id) = return_value {
            self.fmt_expression(*expr_id, f)?;
        }
        write!(f, ";")?;
        Ok(())
    }

    fn fmt_block(
        &self, _literal: &str, statements: &[StmtId], f: &mut std::fmt::Formatter<'_>,
    ) -> Result<(), Error> {
        for (_, s) in statements.iter().enumerate() {
            self.fmt_statement(*s, f)?;
        }
        Ok(())
    }

    fn fmt_expression_statement(
        &self, _literal: &str, expression: &Option<ExprId>, f: &mut std::fmt::Formatter<'_>,
    ) -> Result<(), Error> {
        if let Some(expr_id) = expression {
            self.fmt_expression(*expr_id, f)?;
        }
        Ok(())
    }

    fn fmt_expression(
        &self, expr_id: ExprId, f: &mut std::fmt::Formatter<'_>,
    ) -> Result<(), Error> {
        match &self.expr_nodes[expr_id] {
            Node::Identifier { value, .. } => write!(f, "{}", value)?,
            Node::Num { value, .. } => write!(f, "{}", value)?,
            Node::Str { value, .. } => write!(f, "\"{}\"", value)?,
            Node::Prefix {
                operator, right, ..
            } => {
                write!(f, "({}", operator)?;
                self.fmt_expression(*right, f)?;
                write!(f, ")")?;
            }
            Node::Infix {
                left,
                operator,
                right,
                ..
            } => {
                write!(f, "(")?;
                self.fmt_expression(*left, f)?;
                write!(f, " {} ", operator)?;
                self.fmt_expression(*right, f)?;
                write!(f, ")")?;
            }
            Node::Boolean { value, .. } => write!(f, "{}", value)?,
            Node::If {
                condition,
                consequence,
                alternative,
                ..
            } => {
                write!(f, "if")?;
                self.fmt_expression(*condition, f)?;
                write!(f, " ")?;
                // it's a little bit strange that we call
                // fmt_statement instead of fmt_block...
                self.fmt_statement(*consequence, f)?;
                if let Some(alt) = alternative {
                    write!(f, "else ")?;
                    // it's a little bit strange that we call
                    // fmt_statement instead of fmt_block...
                    self.fmt_statement(*alt, f)?;
                }
            }
            Node::Func {
                parameters,
                body,
                name,
                ..
            } => {
                write!(
                    f,
                    "fn: {} ({})",
                    name,
                    parameters
                        .iter()
                        .map(|&id| self.expr_nodes[id].token_literal())
                        .collect::<Vec<&str>>()
                        .join(", ")
                )?;
                self.fmt_statement(*body, f)?;
            }
            Node::Call {
                function,
                arguments,
                ..
            } => {
                self.fmt_expression(*function, f)?;
                write!(f, "(")?;
                for (i, eid) in arguments.iter().enumerate() {
                    self.fmt_expression(*eid, f)?;
                    if i < arguments.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")?;
            }
            Node::Array { elements, .. } => {
                write!(f, "[")?;
                for (i, eid) in elements.iter().enumerate() {
                    self.fmt_expression(*eid, f)?;
                    if i < elements.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "]")?;
            }
            Node::Index { array, index, .. } => {
                write!(f, "(")?;
                self.fmt_expression(*array, f)?;
                write!(f, "[")?;
                self.fmt_expression(*index, f)?;
                write!(f, "]")?;
                write!(f, ")")?;
            }
        }
        Ok(())
    }

    fn fmt_identifier(
        &self, name_id: ExprId, f: &mut std::fmt::Formatter<'_>,
    ) -> Result<(), Error> {
        self.fmt_expression(name_id, f)
    }

    fn fmt_statement(&self, stmt_id: StmtId, f: &mut std::fmt::Formatter<'_>) -> Result<(), Error> {
        match &self.stmt_nodes[stmt_id] {
            Statement::Let {
                ref token,
                name,
                value,
            } => self.fmt_let_statement(token.literal, *name, *value, f)?,
            Statement::Return {
                ref token,
                return_value,
            } => self.fmt_return_statement(token.literal, return_value, f)?,
            Statement::Expression {
                ref token,
                expression,
            } => self.fmt_expression_statement(token.literal, expression, f)?,
            Statement::Block {
                ref token,
                statements,
            } => self.fmt_block(token.literal, statements, f)?,
        }

        Ok(())
    }
}

impl<'a> Display for Program<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), Error> {
        for &stmt_id in self.statements.iter() {
            self.fmt_statement(stmt_id, f)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use token::TokenType;
    #[test]
    fn pretty_printing_works() {
        // let myVar = anotherVar;
        let stmt_nodes = vec![Statement::Let {
            token: Token {
                token_type: TokenType::Let,
                literal: "let",
                line: 0,
                offset: 0,
            },
            name: 0,
            value: 1,
        }];

        let expr_nodes = vec![
            Node::Identifier {
                token: Token {
                    token_type: TokenType::Ident,
                    literal: "myVar",
                    line: 0,
                    offset: 0,
                },
                value: "myVar",
            },
            Node::Identifier {
                token: Token {
                    token_type: TokenType::Ident,
                    literal: "anotherVar",
                    line: 0,
                    offset: 0,
                },
                value: "anotherVar",
            },
        ];

        let program = Program {
            statements: vec![0],
            stmt_nodes,
            expr_nodes,
        };

        let rep = format!("{}", program);
        assert_eq!(
            rep, "let myVar = anotherVar;",
            "invalid string representation"
        );
    }
}
