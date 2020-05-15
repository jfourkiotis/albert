use ast::*;
use lexer::Lexer;
use std::collections::HashMap;
use token::*;

#[repr(i32)]
#[derive(Debug, Copy, Clone)]
enum Precedence {
    Lowest = 10,
    Equals = 20,      // ==
    LessGreater = 30, // > or <
    Sum = 40,         // +
    Product = 50,     // *
    Power = 60,       // ^
    Prefix = 70,      // -X or !X
    Call = 80,        // my_function(X)
    Index = 90,       // array[index], a(10)[20] --> ?
}

#[macro_use]
extern crate lazy_static;

lazy_static! {
    static ref PRECEDENCES: HashMap<TokenType, Precedence> = {
        let mut m = HashMap::new();
        m.insert(TokenType::Equal, Precedence::Equals);
        m.insert(TokenType::NotEqual, Precedence::Equals);
        m.insert(TokenType::LessThan, Precedence::LessGreater);
        m.insert(TokenType::GreaterThan, Precedence::LessGreater);
        m.insert(TokenType::Plus, Precedence::Sum);
        m.insert(TokenType::Minus, Precedence::Sum);
        m.insert(TokenType::Slash, Precedence::Product);
        m.insert(TokenType::Asterisk, Precedence::Product);
        m.insert(TokenType::Power, Precedence::Power);
        m.insert(TokenType::Lparen, Precedence::Call);
        m.insert(TokenType::Lbracket, Precedence::Index);
        m
    };
}

const INFIX_OPERATORS: [TokenType; 11] = [
    TokenType::Plus,
    TokenType::Minus,
    TokenType::Asterisk,
    TokenType::Slash,
    TokenType::Equal,
    TokenType::NotEqual,
    TokenType::LessThan,
    TokenType::GreaterThan,
    TokenType::Power,
    TokenType::Lparen,
    TokenType::Lbracket,
];

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Option<Token<'a>>,
    peek_token: Option<Token<'a>>,
    nodes: Vec<Node<'a>>,
    statements: Vec<Statement<'a>>,
    errors: Vec<String>,
    // used as a placeholder for the current let AST
    var_name: Option<&'a str>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let mut p = Parser {
            lexer,
            current_token: None,
            peek_token: None,
            nodes: vec![],
            statements: vec![],
            errors: vec![],
            var_name: None,
        };

        // read two tokens so current_token and peek_token are both set:
        p.next_token();
        p.next_token();
        p
    }

    /// parse_program consumes the parser and returns the parsed AST and all
    /// errors encountered.
    pub fn parse_program(mut self) -> Result<Program<'a>, Vec<String>> {
        let mut statements: Vec<StmtId> = vec![];
        while let Some(tok) = self.current_token.take() {
            match self.parse_statement(tok) {
                Ok(id) => statements.push(id),
                Err(msg) => self.errors.push(msg),
            }
            self.next_token();
        }

        if self.errors.is_empty() {
            Ok(Program {
                statements,
                expr_nodes: self.nodes,
                stmt_nodes: self.statements,
            })
        } else {
            Err(self.errors)
        }
    }

    fn parse_statement(&mut self, tok: Token<'a>) -> Result<StmtId, String> {
        if tok.token_type == TokenType::Let {
            self.parse_let_statement(tok)
        } else if tok.token_type == TokenType::Return {
            self.parse_return_statement(tok)
        } else {
            self.parse_expression_statement(tok)
        }
    }

    fn register_node(&mut self, node: Node<'a>) -> ExprId {
        self.nodes.push(node);
        self.nodes.len() - 1
    }

    fn register_statement(&mut self, statement: Statement<'a>) -> StmtId {
        self.statements.push(statement);
        self.statements.len() - 1
    }

    fn parse_let_statement(&mut self, let_tok: Token<'a>) -> Result<StmtId, String> {
        let token = self.expect_peek(TokenType::Ident)?;
        let clone = Token { ..token };
        let identifier = Node::Identifier {
            token,
            value: clone.literal,
        };

        self.expect_peek(TokenType::Assign)?;
        let current = self.next_token_or_error("Unexpected EOF".to_string())?;
        let old_var_name = self.var_name.replace(clone.literal);
        let value = self.parse_expression(current, Precedence::Lowest as i32)?;
        self.var_name = old_var_name;
        self.expect_peek(TokenType::Semicolon)?;

        let name = self.register_node(identifier);
        let let_stmt = Statement::Let {
            token: let_tok,
            name,
            value,
        };

        Ok(self.register_statement(let_stmt))
    }

    fn parse_return_statement(&mut self, token: Token<'a>) -> Result<StmtId, String> {
        let current = self.next_token_or_error("Unexpected EOF".to_string())?;
        let return_value = if current.token_type != TokenType::Semicolon {
            let val = self.parse_expression(current, Precedence::Lowest as i32)?;
            self.expect_peek(TokenType::Semicolon)?;
            Some(val)
        } else {
            None
        };

        let ret_stmt = Statement::Return {
            token,
            return_value,
        };

        Ok(self.register_statement(ret_stmt))
    }

    fn parse_expression_statement(&mut self, expr_tok: Token<'a>) -> Result<StmtId, String> {
        let tok = Token {
            token_type: expr_tok.token_type,
            literal: expr_tok.literal,
        };

        let stmt = if expr_tok.token_type == TokenType::Semicolon {
            Statement::Expression {
                token: tok,
                expression: None,
            }
        } else {
            let tmp = Statement::Expression {
                token: tok,
                expression: Some(self.parse_expression(expr_tok, Precedence::Lowest as i32)?),
            };

            // we allow expression statements without a semicolon if and only if
            // a right brace follows (we assume we read the end of a block statement)
            if !self.peek_token_is(TokenType::Rbrace) {
                self.expect_peek(TokenType::Semicolon)?;
            }
            tmp
        };

        Ok(self.register_statement(stmt))
    }

    fn is_prefix_operator(&self, token_type: TokenType) -> bool {
        token_type == TokenType::Bang || token_type == TokenType::Minus
    }

    fn parse_expression(&mut self, tok: Token<'a>, prec: i32) -> Result<ExprId, String> {
        let mut left;
        if tok.token_type == TokenType::Ident {
            left = self.parse_identifier(tok)?;
        } else if tok.token_type == TokenType::Int {
            left = self.parse_integer_literal(tok)?;
        } else if tok.token_type == TokenType::Str {
            left = self.parse_str_literal(tok)?;
        } else if tok.token_type == TokenType::True || tok.token_type == TokenType::False {
            left = self.parse_boolean_literal(tok)?;
        } else if tok.token_type == TokenType::Lparen {
            left = self.parse_grouped_expression(tok)?;
        } else if tok.token_type == TokenType::Lbracket {
            left = self.parse_array_literal(tok)?;
        } else if tok.token_type == TokenType::If {
            left = self.parse_if_expression(tok)?;
        } else if self.is_prefix_operator(tok.token_type) {
            left = self.parse_prefix_expression(tok)?;
        } else if tok.token_type == TokenType::Function {
            left = self.parse_function_literal(tok)?;
        } else {
            return Err(format!("invalid prefix token {}", tok.literal));
        }

        while !self.peek_token_is(TokenType::Semicolon) && prec < self.peek_precedence() {
            let infix_tok = self.expect_peek_infix()?;
            left = if infix_tok.token_type == TokenType::Lparen {
                self.parse_call_expression(left, infix_tok)?
            } else if infix_tok.token_type == TokenType::Lbracket {
                self.parse_index_expression(left, infix_tok)?
            } else {
                self.parse_infix_expression(left, infix_tok)?
            }
        }
        Ok(left)
    }

    fn parse_array_literal(&mut self, token: Token<'a>) -> Result<ExprId, String> {
        let elements = self.parse_array_elements()?;
        let array = Node::Array { token, elements };
        Ok(self.register_node(array))
    }

    fn parse_array_elements(&mut self) -> Result<Vec<ExprId>, String> {
        let mut elements: Vec<ExprId> = vec![];

        if !self.peek_token_is(TokenType::Rbracket) {
            loop {
                let arg = self.next_token_or_error("Unexpected EOF".to_string())?;
                let argid = self.parse_expression(arg, Precedence::Lowest as i32)?;
                elements.push(argid);
                if self.peek_token_is(TokenType::Comma) {
                    self.next_token();
                } else {
                    self.expect_peek(TokenType::Rbracket)?;
                    break;
                }
            }
        } else {
            self.next_token();
        }
        Ok(elements)
    }

    fn parse_call_expression(
        &mut self, function: ExprId, token: Token<'a>,
    ) -> Result<ExprId, String> {
        let arguments = self.parse_call_arguments()?;
        let call = Node::Call {
            token,
            function,
            arguments,
        };
        Ok(self.register_node(call))
    }

    fn parse_index_expression(
        &mut self, array: ExprId, token: Token<'a>,
    ) -> Result<ExprId, String> {
        let index_expr_token = self.next_token_or_error("Unexpected EOF".to_string())?;
        let index = self.parse_expression(index_expr_token, Precedence::Lowest as i32)?;
        self.expect_peek(TokenType::Rbracket)?;
        Ok(self.register_node(Node::Index {
            token,
            array,
            index,
        }))
    }

    fn parse_call_arguments(&mut self) -> Result<Vec<ExprId>, String> {
        let mut arguments: Vec<ExprId> = vec![];

        if !self.peek_token_is(TokenType::Rparen) {
            loop {
                let arg = self.next_token_or_error("Unexpected EOF".to_string())?;
                let argid = self.parse_expression(arg, Precedence::Lowest as i32)?;
                arguments.push(argid);
                if self.peek_token_is(TokenType::Comma) {
                    self.next_token();
                } else {
                    self.expect_peek(TokenType::Rparen)?;
                    break;
                }
            }
        } else {
            self.next_token();
        }
        Ok(arguments)
    }

    fn parse_function_literal(&mut self, token: Token<'a>) -> Result<ExprId, String> {
        let lparen = self.expect_peek(TokenType::Lparen)?;
        let parameters = self.parse_function_parameters(lparen)?;
        let lbrace = self.expect_peek(TokenType::Lbrace)?;
        let body = self.parse_block_statement(lbrace)?;
        let f = Node::Func {
            token,
            parameters,
            body,
            name: self.var_name.unwrap_or(""),
        };
        Ok(self.register_node(f))
    }

    fn parse_function_parameters(&mut self, _tok: Token<'a>) -> Result<Vec<ExprId>, String> {
        let mut parameters: Vec<ExprId> = vec![];

        if !self.peek_token_is(TokenType::Rparen) {
            loop {
                let param = self.expect_peek(TokenType::Ident)?;
                let clone = Token { ..param };
                let ident = Node::Identifier {
                    token: param,
                    value: clone.literal,
                };
                parameters.push(self.register_node(ident));
                if self.peek_token_is(TokenType::Comma) {
                    self.next_token();
                } else {
                    self.expect_peek(TokenType::Rparen)?;
                    break;
                }
            }
        } else {
            self.next_token();
        }
        Ok(parameters)
    }

    fn parse_if_expression(&mut self, tok: Token<'a>) -> Result<ExprId, String> {
        self.expect_peek(TokenType::Lparen)?;

        let current = self.next_token_or_error("Unexpected EOF".to_string())?;
        let condition = self.parse_expression(current, Precedence::Lowest as i32)?;
        self.expect_peek(TokenType::Rparen)?;
        let lbrace = self.expect_peek(TokenType::Lbrace)?;
        let consequence = self.parse_block_statement(lbrace)?;

        let alternative = if self.peek_token_is(TokenType::Else) {
            self.next_token();
            let lbrace = self.expect_peek(TokenType::Lbrace)?;
            Some(self.parse_block_statement(lbrace)?)
        } else {
            None
        };

        let expr = Node::If {
            token: tok,
            condition,
            consequence,
            alternative,
        };
        Ok(self.register_node(expr))
    }

    fn parse_block_statement(&mut self, lbrace: Token<'a>) -> Result<ExprId, String> {
        let mut statements: Vec<StmtId> = vec![];
        loop {
            let current = self.next_token_or_error("Unexpected EOF".to_string())?;
            if current.token_type == TokenType::Rbrace {
                break;
            }
            statements.push(self.parse_statement(current)?);
        }
        let block = Statement::Block {
            token: lbrace,
            statements,
        };
        Ok(self.register_statement(block))
    }

    fn parse_grouped_expression(&mut self, _tok: Token<'a>) -> Result<ExprId, String> {
        let current = self.next_token_or_error("Unexpected EOF".to_string())?;
        let expr_id = self.parse_expression(current, Precedence::Lowest as i32)?;
        self.expect_peek(TokenType::Rparen)?;
        Ok(expr_id)
    }

    fn parse_prefix_expression(&mut self, tok: Token<'a>) -> Result<ExprId, String> {
        let current = self.next_token_or_error("Unexpected EOF".to_string())?;

        let expr_id = self.parse_expression(current, Precedence::Prefix as i32)?;
        //FIXME: we create a dummy copy of our token because we need to initialize
        //two struct members with the same token, something that I cannot find a way
        //to do in Rust. I think I'll just remove the `token` member from each AST
        //node.
        let tok_copy = Token { ..tok };
        Ok(self.register_node(Node::Prefix {
            token: tok,
            operator: tok_copy.literal,
            right: expr_id,
        }))
    }

    fn parse_infix_expression(
        &mut self, left: ExprId, infix_tok: Token<'a>,
    ) -> Result<ExprId, String> {
        let mut precedence = self.token_precedence(infix_tok.token_type);

        // right assoc
        if infix_tok.token_type == TokenType::Power {
            precedence -= 1;
        }

        let tmp = Token { ..infix_tok };
        let current = self.next_token_or_error("Unexpected EOF".to_string())?;

        let right = self.parse_expression(current, precedence)?;
        let expression = Node::Infix {
            token: infix_tok,
            left,
            operator: tmp.literal,
            right,
        };
        Ok(self.register_node(expression))
    }

    fn peek_precedence(&self) -> i32 {
        *self.peek_token.as_ref().map_or(&Precedence::Lowest, |tok| {
            PRECEDENCES
                .get(&tok.token_type)
                .unwrap_or(&Precedence::Lowest)
        }) as i32
    }

    fn token_precedence(&self, token_type: TokenType) -> i32 {
        *PRECEDENCES.get(&token_type).unwrap_or(&Precedence::Lowest) as i32
    }

    fn parse_identifier(&mut self, token: Token<'a>) -> Result<ExprId, String> {
        let clone = Token { ..token };
        let node = Node::Identifier {
            token,
            value: clone.literal,
        };
        Ok(self.register_node(node))
    }

    fn parse_integer_literal(&mut self, token: Token<'a>) -> Result<ExprId, String> {
        if let Ok(val) = token.literal.parse::<i64>() {
            let node = Node::Int { token, value: val };
            Ok(self.register_node(node))
        } else {
            Err(format!("{} is not a valid integer", token.literal))
        }
    }

    fn parse_str_literal(&mut self, token: Token<'a>) -> Result<ExprId, String> {
        let clone = Token { ..token };
        Ok(self.register_node(Node::Str {
            token,
            value: clone.literal,
        }))
    }

    fn parse_boolean_literal(&mut self, token: Token<'a>) -> Result<ExprId, String> {
        let value = token.token_type == TokenType::True;
        let node = Node::Boolean { token, value };
        Ok(self.register_node(node))
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.take();
        self.peek_token = self.lexer.next();
    }

    fn next_token_or_error(&mut self, msg: String) -> Result<Token<'a>, String> {
        self.next_token();
        self.current_token.take().map_or(Err(msg), Ok)
    }

    fn peek_token_is(&self, t: TokenType) -> bool {
        self.peek_token
            .as_ref()
            .map_or(false, |tok| tok.token_type == t)
    }

    fn peek_error(&mut self, t: TokenType) -> String {
        format!(
            "expected next token be {:?}, got {:?} instead",
            t,
            self.peek_token
                .as_ref()
                .map_or(TokenType::Illegal, |ref tok| tok.token_type)
        )
    }

    fn expect_peek(&mut self, t: TokenType) -> Result<Token<'a>, String> {
        if self.peek_token_is(t) {
            self.next_token();
            Ok(self.current_token.take().unwrap())
        } else {
            Err(self.peek_error(t))
        }
    }

    fn expect_peek_infix(&mut self) -> Result<Token<'a>, String> {
        let token = self.next_token_or_error("Unexpected EOF".to_string())?;
        if INFIX_OPERATORS.contains(&token.token_type) {
            Ok(token)
        } else {
            Err(format!("expected infix operator, got '{}'", token.literal))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[macro_use]
    macro_rules! test_literal {
        ($p:path, $n:expr, $e:expr) => {
            match $n {
                $p { ref token, value } => {
                    assert_eq!(value, &$e);
                    assert_eq!(token.literal, $e.to_string());
                }
                other => assert!(false, "unexpected node: {:?}", other),
            }
        };
    }

    #[macro_use]
    macro_rules! test_literal_expression {
        ($input:expr, $literal_node:path, $expected:expr) => {
            let Program {
                statements,
                expr_nodes,
                stmt_nodes,
            } = parse_input($input);
            check_number_of_statements(1, statements.len());
            match &stmt_nodes[statements[0]] {
                Statement::Expression {
                    expression: Some(expr_id),
                    ..
                } => test_literal!($literal_node, &expr_nodes[*expr_id], $expected),
                other => assert!(
                    false,
                    "program.statements[0] is not a Statement::Expression, but a {:?}",
                    other
                ),
            }
        };
    }

    #[macro_use]
    macro_rules! test_prefix_expression {
        ($input:expr, $op:expr, $rhs_type:path, $rhs:expr) => {
            let Program {
                statements,
                expr_nodes,
                stmt_nodes,
            } = parse_input($input);

            check_number_of_statements(1, statements.len());
            match &stmt_nodes[statements[0]] {
                Statement::Expression {
                    expression: Some(expr_id),
                    ..
                } => match &expr_nodes[*expr_id] {
                    Node::Prefix {
                        operator, right, ..
                    } => {
                        assert_eq!(operator, &$op, "operators do not match");
                        test_literal!($rhs_type, &expr_nodes[*right], $rhs);
                    }
                    other => assert!(
                        false,
                        "expected Node::Prefix, but got a {:?} instead",
                        other
                    ),
                },
                other => assert!(
                    false,
                    "program.statements[0] is not a Statement::Expression, but a {:?}",
                    other
                ),
            }
        };
    }

    #[macro_use]
    macro_rules! test_infix_expression {
        ($node_id:expr, $expr_nodes:expr, $lhs_type:path, $lhs:expr, $op:expr, $rhs_type:path, $rhs:expr) => {
            match &$expr_nodes[$node_id] {
                Node::Infix {
                    left,
                    operator,
                    right,
                    ..
                } => {
                    test_literal!($lhs_type, &$expr_nodes[*left], $lhs);
                    assert_eq!(operator, &$op, "operators do not match");
                    test_literal!($rhs_type, &$expr_nodes[*right], $rhs);
                }
                other => panic!("expected infix node, got a {:?} instead", other),
            }
        };
    }

    #[macro_use]
    macro_rules! test_infix_stmt_expression {
        ($node:expr, $expr_nodes:expr, $lhs_type:path, $lhs:expr, $op:expr, $rhs_type:path, $rhs:expr) => {
            match &$node {
                Statement::Expression {
                    expression: Some(expr_id),
                    ..
                } => test_infix_expression!(
                    *expr_id,
                    $expr_nodes,
                    $lhs_type,
                    $lhs,
                    $op,
                    $rhs_type,
                    $rhs
                ),
                _other => assert!(false, "expected Statement::Expression"),
            }
        };
    }

    #[macro_use]
    macro_rules! test_infix_input {
        ($input:expr, $lhs_type:path, $lhs:expr, $op:expr, $rhs_type:path, $rhs:expr) => {
            let Program {
                statements,
                expr_nodes,
                stmt_nodes,
            } = parse_input($input);
            check_number_of_statements(1, statements.len());
            test_infix_stmt_expression!(
                &stmt_nodes[statements[0]],
                expr_nodes,
                $lhs_type,
                $lhs,
                $op,
                $rhs_type,
                $rhs
            );
        };
    }

    #[test]
    fn parse_let_statements_works() {
        let input = "
        let x = 5;
        let y = 10;
        let foobar = 838383;
        ";

        let Program {
            statements,
            expr_nodes,
            stmt_nodes,
        } = parse_input(input);
        check_number_of_statements(3, statements.len());

        struct Test(&'static str, i64);
        let t = Test;
        let tests = [t("x", 5), t("y", 10), t("foobar", 838_383)];

        for (i, test) in tests.iter().enumerate() {
            let stmt_id = statements[i];
            test_let_statement(&stmt_nodes[stmt_id], test.0, test.1, &expr_nodes)
        }
    }

    fn test_return_statement(stmt: &Statement, expected_value: Option<i64>, nodes: &[Node]) {
        match stmt {
            Statement::Return {
                ref token,
                return_value,
            } => {
                assert_eq!(
                    token.literal, "return",
                    "node.token.literal not 'return', got {}",
                    token.literal
                );
                assert!(return_value.is_some() == expected_value.is_some());
                if let Some(rvid) = return_value {
                    let rexpr = &nodes[*rvid];
                    assert!(
                        matches!(rexpr, Node::Int { value, .. } if *value == expected_value.unwrap())
                    );
                }
            }
            other => {
                assert!(false, "node is not Node::Return but a {:?}", other);
            }
        }
    }

    #[test]
    fn parse_return_statement_works() {
        let input = "
        return 5;
        return 10;
        return ;
        return 838383;
        ";

        struct Test(Option<i64>);
        let t = Test;
        let tests = [t(Some(5)), t(Some(10)), t(None), t(Some(838_383))];

        let Program {
            statements,
            stmt_nodes,
            expr_nodes,
        } = parse_input(input);
        check_number_of_statements(4, statements.len());

        for (i, test) in tests.iter().enumerate() {
            let stmt_id = statements[i];
            test_return_statement(&stmt_nodes[stmt_id], test.0, &expr_nodes);
        }
    }

    #[test]
    fn parse_empty_statement_works() {
        let input = ";";
        let Program {
            statements,
            stmt_nodes,
            ..
        } = parse_input(input);
        check_number_of_statements(statements.len(), 1);

        match &stmt_nodes[statements[0]] {
            Statement::Expression {
                expression: expr, ..
            } => assert_eq!(true, !expr.is_some()),
            _other => panic!("expected empty statement"),
        }
    }

    #[test]
    fn parse_empty_statements_works() {
        let input = ";;;;
        
        ; "; // 5

        let Program {
            statements,
            stmt_nodes,
            ..
        } = parse_input(input);
        check_number_of_statements(statements.len(), 5);

        for (_, id) in statements.iter().enumerate() {
            match &stmt_nodes[statements[*id]] {
                Statement::Expression {
                    expression: expr, ..
                } => assert_eq!(true, !expr.is_some()),
                _other => panic!("expected empty statement"),
            }
        }
    }

    #[test]
    fn parse_function_literal_works() {
        let input = "fn(x, y) { x + y };";
        let Program {
            statements,
            stmt_nodes,
            expr_nodes,
        } = parse_input(input);

        check_number_of_statements(1, statements.len());

        match &stmt_nodes[statements[0]] {
            Statement::Expression {
                expression: Some(expr),
                ..
            } => match &expr_nodes[*expr] {
                Node::Func {
                    parameters, body, ..
                } => {
                    assert_eq!(2, parameters.len());
                    test_literal!(Node::Identifier, &expr_nodes[parameters[0]], "x");
                    test_literal!(Node::Identifier, &expr_nodes[parameters[1]], "y");
                    if let Statement::Block { statements, .. } = &stmt_nodes[*body] {
                        assert_eq!(1, statements.len());
                        test_infix_stmt_expression!(
                            stmt_nodes[statements[0]],
                            expr_nodes,
                            Node::Identifier,
                            "x",
                            "+",
                            Node::Identifier,
                            "y"
                        );
                    } else {
                        panic!("expected block statement");
                    }
                }
                _other => panic!("expected function literal"),
            },

            _other => panic!("expected exression statement"),
        }
    }

    #[test]
    fn parse_identifier_expression_works() {
        test_literal_expression!("foobar;", Node::Identifier, "foobar");
    }

    #[test]
    fn parse_integer_expression_works() {
        test_literal_expression!("5;", Node::Int, 5);
    }

    #[test]
    fn parse_boolean_expression_works() {
        test_literal_expression!("false;", Node::Boolean, false);
        test_literal_expression!("true;", Node::Boolean, true);
    }

    #[test]
    fn parse_prefix_expression_works() {
        test_prefix_expression!("!5;", "!", Node::Int, 5);
        test_prefix_expression!("-5;", "-", Node::Int, 5);
        test_prefix_expression!("!true;", "!", Node::Boolean, true);
        test_prefix_expression!("!false;", "!", Node::Boolean, false);
    }

    #[test]
    fn parse_infix_expression_works() {
        test_infix_input!("5 + 5;", Node::Int, 5, "+", Node::Int, 5);
        test_infix_input!("5 - 5;", Node::Int, 5, "-", Node::Int, 5);
        test_infix_input!("5 * 5;", Node::Int, 5, "*", Node::Int, 5);
        test_infix_input!("5 / 5;", Node::Int, 5, "/", Node::Int, 5);
        test_infix_input!("5 > 5;", Node::Int, 5, ">", Node::Int, 5);
        test_infix_input!("5 < 5;", Node::Int, 5, "<", Node::Int, 5);
        test_infix_input!("5 == 5;", Node::Int, 5, "==", Node::Int, 5);
        test_infix_input!("5 != 5;", Node::Int, 5, "!=", Node::Int, 5);
        test_infix_input!(
            "true == true;",
            Node::Boolean,
            true,
            "==",
            Node::Boolean,
            true
        );
        test_infix_input!(
            "true != false;",
            Node::Boolean,
            true,
            "!=",
            Node::Boolean,
            false
        );
        test_infix_input!(
            "false == false;",
            Node::Boolean,
            false,
            "==",
            Node::Boolean,
            false
        );
    }

    #[test]
    fn test_operator_precedence_works() {
        struct Test(&'static str, &'static str);
        let t = Test;

        let precedence_tests = [
            t("-a * b;", "((-a) * b)"),
            t("!-a;", "(!(-a))"),
            t("a + b + c;", "((a + b) + c)"),
            t("a + b - c;", "((a + b) - c)"),
            t("a * b * c;", "((a * b) * c)"),
            t("a * b / c;", "((a * b) / c)"),
            t("a + b / c;", "(a + (b / c))"),
            t("a + b * c + d / e - f;", "(((a + (b * c)) + (d / e)) - f)"),
            t("3 + 4; -5 * 5;", "(3 + 4)((-5) * 5)"),
            t("5 > 4 == 3 < 4;", "((5 > 4) == (3 < 4))"),
            t("5 < 4 != 3 > 4;", "((5 < 4) != (3 > 4))"),
            t(
                "3 + 4 * 5 == 3 * 1 + 4 * 5;",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            t("true;", "true"),
            t("false;", "false"),
            t("3 > 5 == false;", "((3 > 5) == false)"),
            t("3 < 5 == true;", "((3 < 5) == true)"),
            t("1 + (2 + 3) + 4;", "((1 + (2 + 3)) + 4)"),
            t("2 / (5 + 5);", "(2 / (5 + 5))"),
            t("-(5 + 5);", "(-(5 + 5))"),
            t("!(true == true);", "(!(true == true))"),
            t("a ^ b ^ c;", "(a ^ (b ^ c))"),
            t("a + add(b * c) + d;", "((a + add((b * c))) + d)"),
            t(
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8));",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            t(
                "add(a + b + c * d / f + g);",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
            t(
                "a * [1, 2, 3, 4][b * c] * d;",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            ),
            t(
                "add(a * b[2], b[1], 2 * [1, 2][1]);",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
            ),
        ];

        for (_, test) in precedence_tests.iter().enumerate() {
            let program = parse_input(test.0);

            let rep = format!("{}", program);
            assert_eq!(rep, test.1, "expected: {}, got: {}", test.1, rep);
        }
    }

    #[test]
    fn parse_array_literal_works() {
        let input = "[1, 2 * 2, 3 + 3];";
        let Program {
            statements,
            stmt_nodes,
            expr_nodes,
        } = parse_input(input);

        match &stmt_nodes[statements[0]] {
            Statement::Expression {
                expression: Some(expr_id),
                ..
            } => match &expr_nodes[*expr_id] {
                Node::Array { elements, .. } => {
                    assert_eq!(
                        3,
                        elements.len(),
                        "expected {} array elements, got {}",
                        3,
                        elements.len()
                    );
                    test_literal!(Node::Int, &expr_nodes[elements[0]], 1);
                    test_infix_expression!(
                        elements[1],
                        expr_nodes,
                        Node::Int,
                        2,
                        "*",
                        Node::Int,
                        2
                    );
                    test_infix_expression!(
                        elements[2],
                        expr_nodes,
                        Node::Int,
                        3,
                        "+",
                        Node::Int,
                        3
                    );
                }
                _other => panic!("expected Node::Array literal"),
            },
            _other => panic!("expected Statement::Expression"),
        }
    }

    #[test]
    fn parse_call_expression_works() {
        let input = "add(1, 2*3, 4+5);";
        let Program {
            statements,
            stmt_nodes,
            expr_nodes,
        } = parse_input(input);

        check_number_of_statements(1, statements.len());
        match &stmt_nodes[statements[0]] {
            Statement::Expression {
                expression: Some(expr_id),
                ..
            } => match &expr_nodes[*expr_id] {
                Node::Call {
                    function,
                    arguments,
                    ..
                } => {
                    test_literal!(Node::Identifier, &expr_nodes[*function], "add");
                    assert_eq!(3, arguments.len());
                    test_literal!(Node::Int, &expr_nodes[arguments[0]], 1);
                    // we cannot use test_infix_expression! here, because it expects a
                    // semicolon after each expression, so, let's do it manually:
                    match &expr_nodes[arguments[1]] {
                        Node::Infix {
                            left,
                            operator,
                            right,
                            ..
                        } => {
                            test_literal!(Node::Int, &expr_nodes[*left], 2);
                            assert_eq!(&"*", operator);
                            test_literal!(Node::Int, &expr_nodes[*right], 3);
                        }
                        _other => panic!("expected infix expression: 2 * 3"),
                    }

                    match &expr_nodes[arguments[2]] {
                        Node::Infix {
                            left,
                            operator,
                            right,
                            ..
                        } => {
                            test_literal!(Node::Int, &expr_nodes[*left], 4);
                            assert_eq!(&"+", operator);
                            test_literal!(Node::Int, &expr_nodes[*right], 5);
                        }
                        _other => panic!("expected infix expression: 4 + 5"),
                    }
                }
                _other => panic!("expected Node::Call"),
            },
            other => panic!("expected Statement::Expression, got {:?}", other),
        }
    }

    fn parse_input(input: &str) -> Program {
        let lexer = Lexer::new(input);
        let parser = Parser::new(lexer);
        parser.parse_program().unwrap_or_else(|errors| {
            check_parser_errors(errors);
            unreachable!();
        })
    }

    fn test_let_statement(
        s: &Statement, expected_name: &'static str, expected_value: i64, nodes: &[Node],
    ) {
        match s {
            Statement::Let {
                ref token,
                name,
                value,
            } => {
                assert_eq!(
                    token.literal, "let",
                    "s.token.literal not 'let', got: {}",
                    token.literal
                );

                match &nodes[*name] {
                    &Node::Identifier { value, .. } => assert_eq!(
                        value, expected_name,
                        "s.name.token.literal did not match expected name"
                    ),
                    other => assert!(false, "s.name is not an ast::Identifier, but a {:?}", other),
                }

                assert!(matches!(
                    &nodes[*value],
                    Node::Int {
                        value, ..
                    } if *value == expected_value
                ));
            }
            _ => assert!(false, "s is not a Node::LetStatement but a {:?}", s),
        }
    }

    fn check_number_of_statements(num: usize, expected: usize) {
        assert_eq!(
            num, expected,
            "program.statements.len should be {}, but it was {}",
            num, expected
        );
    }

    fn check_parser_errors(errors: Vec<String>) {
        for (_, msg) in errors.iter().enumerate() {
            println!("parser error: {}", msg);
        }
        assert_eq!(
            errors.is_empty(),
            true,
            "parser had {} errors",
            errors.len()
        );
    }
}
