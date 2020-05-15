use ast::*;
use std::collections::HashMap;
use string_interner::{DefaultStringInterner, Sym};

#[derive(Debug, Copy, Clone)]
pub enum NameResolution {
    Global { symbol: Sym },
    Local { depth: usize, index: usize },
    Unresolved,
}

pub type VarResolution = Vec<NameResolution>;

#[derive(Default)]
pub struct Resolver<'a> {
    // for each expression, mark the environment index where this
    // variable is defined, and its index at that environment.
    resolved: VarResolution,
    scopes: Vec<HashMap<&'a str, (bool, usize)>>,
    in_func: usize,
    interner: DefaultStringInterner,
}

impl<'a> Resolver<'a> {
    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn begin_funcdef(&mut self) {
        self.in_func += 1;
    }

    fn end_funcdef(&mut self) {
        if self.in_func == 0 {
            panic!("end_funcdef without begin_funcdef ?");
        }
        self.in_func -= 1;
    }

    pub fn resolve_program(mut self, prog: &Program) -> Result<VarResolution, String> {
        self.resolved
            .resize(prog.expr_nodes.len(), NameResolution::Unresolved);
        self.resolve_statements(&prog.statements, &prog.stmt_nodes, &prog.expr_nodes)
    }

    fn resolve_statements(
        mut self, statements: &[StmtId], stmt_nodes: &[Statement<'a>], expr_nodes: &[Node<'a>],
    ) -> Result<VarResolution, String> {
        for (_, stmt) in statements.iter().enumerate() {
            self.resolve_statement(*stmt, stmt_nodes, expr_nodes)?;
        }
        Ok(self.resolved)
    }

    fn resolve_statement(
        &mut self, stmt: StmtId, stmt_nodes: &[Statement<'a>], expr_nodes: &[Node<'a>],
    ) -> Result<(), String> {
        match &stmt_nodes[stmt] {
            Statement::Expression { expression, .. } => {
                if let Some(expr_id) = expression {
                    self.resolve_expression(*expr_id, stmt_nodes, expr_nodes)?;
                }
            }
            Statement::Return { return_value, .. } => {
                if self.in_func == 0 {
                    return Err("'return' outside function".to_string());
                } else if let Some(expr_id) = return_value {
                    self.resolve_expression(*expr_id, stmt_nodes, expr_nodes)?;
                }
            }
            Statement::Block { statements, .. } => {
                self.resolve_block_statements(&statements, stmt_nodes, expr_nodes)?;
            }
            Statement::Let {
                name,
                value: let_value,
                ..
            } => match &expr_nodes[*name] {
                Node::Identifier { value, .. } => {
                    self.declare(value);
                    self.resolve_local(*name, value);
                    self.resolve_expression(*let_value, stmt_nodes, expr_nodes)?;
                    self.define(value);
                }
                _other => unimplemented!(""),
            },
        }
        Ok(())
    }

    fn declare(&mut self, name: &'a str) {
        if !self.scopes.is_empty() {
            let last = self.scopes.last_mut().unwrap();
            last.insert(name, (false, last.len()));
        }
    }

    fn define(&mut self, name: &'a str) {
        if !self.scopes.is_empty() {
            let last = self.scopes.last_mut().unwrap();
            let old = last.get(name).cloned().unwrap();
            last.insert(name, (true, old.1));
        }
    }

    fn resolve_expression(
        &mut self, expr: ExprId, stmt_nodes: &[Statement<'a>], expr_nodes: &[Node<'a>],
    ) -> Result<(), String> {
        match &expr_nodes[expr] {
            Node::Prefix { right, .. } => {
                Ok(self.resolve_expression(*right, stmt_nodes, expr_nodes)?)
            }
            Node::Infix { left, right, .. } => {
                self.resolve_expression(*left, stmt_nodes, expr_nodes)?;
                Ok(self.resolve_expression(*right, stmt_nodes, expr_nodes)?)
            }
            Node::If {
                condition,
                consequence,
                alternative,
                ..
            } => {
                self.resolve_expression(*condition, stmt_nodes, expr_nodes)?;
                self.resolve_statement(*consequence, stmt_nodes, expr_nodes)?;
                if let Some(alt) = alternative {
                    Ok(self.resolve_statement(*alt, stmt_nodes, expr_nodes)?)
                } else {
                    Ok(())
                }
            }
            Node::Func {
                token,
                parameters,
                body,
                ..
            } => {
                self.declare(token.literal);
                self.define(token.literal);

                self.begin_funcdef(); // allow return
                self.begin_scope();
                for (_, p) in parameters.iter().enumerate() {
                    match &expr_nodes[*p] {
                        Node::Identifier { token, .. } => {
                            self.declare(token.literal);
                            self.define(token.literal);
                            self.resolve_local(*p, token.literal);
                        }
                        _ => unreachable!(),
                    }
                }

                self.resolve_statement(*body, stmt_nodes, expr_nodes)?;
                self.end_scope();
                self.end_funcdef(); // disallow return
                Ok(())
            }
            Node::Call {
                function,
                arguments,
                ..
            } => {
                self.resolve_expression(*function, stmt_nodes, expr_nodes)?;
                for (_, a) in arguments.iter().enumerate() {
                    self.resolve_expression(*a, stmt_nodes, expr_nodes)?;
                }
                Ok(())
            }
            Node::Identifier { token, .. } => {
                self.resolve_local(expr, token.literal);
                Ok(())
            }
            Node::Array { elements, .. } => {
                for (_, e) in elements.iter().enumerate() {
                    self.resolve_expression(*e, stmt_nodes, expr_nodes)?;
                }
                Ok(())
            }
            Node::Index { array, index, .. } => {
                self.resolve_expression(*array, stmt_nodes, expr_nodes)?;
                self.resolve_expression(*index, stmt_nodes, expr_nodes)
            }
            Node::Int { .. } | Node::Str { .. } | Node::Boolean { .. } => Ok(()),
        }
    }

    fn resolve_local(&mut self, id: ExprId, name: &str) {
        for (i, scope) in self.scopes.iter().rev().enumerate() {
            if let Some((_b, index)) = scope.get(name) {
                self.resolved[id] = NameResolution::Local {
                    depth: i,
                    index: *index,
                };
                return;
            }
        }
        let sym = self.interner.get_or_intern(name);
        self.resolved[id] = NameResolution::Global { symbol: sym };
    }

    fn resolve_block_statements(
        &mut self, statements: &[StmtId], stmt_nodes: &[Statement<'a>], expr_nodes: &[Node<'a>],
    ) -> Result<(), String> {
        for (_, stmt) in statements.iter().enumerate() {
            self.resolve_statement(*stmt, stmt_nodes, expr_nodes)?;
        }
        Ok(())
    }
}
