use std::vec;

use crate::ast;

pub enum S {
    Atom(String),
    List(Vec<S>)
}

impl ToString for S {
    fn to_string(&self) -> String {
        match self {
            Self::Atom(a) => a.clone(),
            Self::List(l) => {
                "(".to_owned()
                    + &l.iter().map(|s| s.to_string()).collect::<Vec<_>>().join(" ")
                    + ")"
            }
        }
    }
}

pub trait IntoSExpression {
    fn to_s_expr<'a>(&self) -> S;
}

impl<T: ToString> IntoSExpression for T {
    fn to_s_expr<'a>(&self) -> S {
        S::Atom(self.to_string())
    }
}

// Ast implementations

use ast::*;

impl IntoSExpression for Statement<'_> {
    fn to_s_expr<'a>(&self) -> S {
        match self {
            Statement::Class(c) => c.to_s_expr(),
            Statement::AbstractClass(abstract_class) => todo!(),
            Statement::Interface(interface) => todo!(),
            Statement::Mixin(mixin) => todo!(),
            Statement::Enum(_) => todo!(),
            Statement::Extend(extend_with) => todo!(),
            Statement::TypeDefinition(type_definition) => todo!(),
            Statement::Constant(const_assignment) => todo!(),
            Statement::Function(function_definition) => todo!(),
            Statement::Use(use_path) => todo!(),
            Statement::Eof => S::Atom("".to_owned()),
        }
    }
}

impl IntoSExpression for Class<'_> {
    fn to_s_expr<'a>(&self) -> S {
        let mut list = vec!["class".to_s_expr(), self.name.0.lexeme.to_s_expr()];

        if let Some(parent) = self.parent {
            list.push(parent.to_s_expr());
        }
        
        if !self.with.is_empty() {
            let with = vec![S::Atom("with".to_owned())] + self.with.iter().map(|t| t.to_s_expr()).collect();
            list.push(S::List(with));
        }

        let body = self.body.statements.iter().map(|s| s.to_s_expr()).collect();
        list.push(S::List(body));

        S::List(list)
    }
}

impl IntoSExpression for Type<'_> {
    fn to_s_expr<'a>(&self) -> S {
        match self {
            Type::Named { path, generics } => S::Atom(path.iter().map(|i| i.0.lexeme).collect<Vec<_>>()),
            Type::SelfKeyword { path, generics } => S::Atom("Self".to_owned()),
            Type::SuperKeyword { path, generics } => S::Atom("Super".to_owned()),
            Type::Interface(statement_block) => S::List(
                vec!["interface".to_s_expr(), S::List(statement_block.statements.iter().map(|s| s.to_s_expr()).collect())]
            ),
            Type::Tuple(items) => S::List(items.iter().map(|i| i.to_s_expr()).collect()),
            Type::Object(object_type) => S::List(
                vec![object_type.supertype.map(|s| s.to_s_expr()).unwrap_or(S::Atom("object".to_owned()))]
                + object_type.body.statements.iter().map(|s| s.to_s_expr()).collect()
            ),
            Type::Array(t, expression) => S::List(
                vec![S::Atom("array".to_owned()), t.to_s_expr(), expression.to_s_expr()]
            ),
            Type::Slice(_) => todo!(),
            Type::Reference(_) => todo!(),
            Type::Pointer(_) => todo!(),
            Type::Function(function_type) => todo!(),
            Type::Union(items) => todo!(),
            Type::Intersection(items) => todo!(),
            Type::Literal(literal) => todo!(),
        }
    }
}

impl IntoSExpression for Expression<'_> {
    fn to_s_expr<'a>(&self) -> S {
        match self {
            Expression::Block(statement_block) => S::List(
                vec!["do".to_s_expr()] + statement_block.statements.iter().map(|s| s.to_s_expr()).collect()
            ),
            Expression::As(expr, t) => S::List(vec!["as".to_s_expr(), t.to_s_expr(), expr.to_s_expr()]),
            Expression::Is(expr, t) => S::List(vec!["is?".to_s_expr(), t.to_s_expr(), expr.to_s_expr()]),
            Expression::Assignment(token, assignment_target, expression) => todo!(),
            Expression::Binary(token, expression, expression1) => todo!(),
            Expression::Unary(token, expression) => todo!(),
            Expression::Field(expression, identifier) => todo!(),
            Expression::TupleField(expression, _) => todo!(),
            Expression::Subscript { array, index } => todo!(),
            Expression::FunctionCall { function, arguments } => todo!(),
            Expression::Dereference(expression) => todo!(),
            Expression::Try(expression) => S::List(vec!["?".to_s_expr(), expression.to_s_expr()]),
            Expression::TryErr(expression) => S::List(vec!["!".to_s_expr(), expression.to_s_expr()]),
            Expression::If(if_expression) => todo!(),
            Expression::While(while_expression) => todo!(),
            Expression::For(for_expression) => todo!(),
            Expression::Loop(statement_block) => todo!(),
            Expression::Match(match_expression) => todo!(),
            Expression::Return(expression) => todo!(),
            Expression::Break(identifier, expression) => todo!(),
            Expression::Continue(identifier) => todo!(),
            Expression::Identifier(identifier) => todo!(),
            Expression::With(_) => todo!(),
            Expression::Literal(literal) => todo!(),
        }
    }
}