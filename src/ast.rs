use crate::token::Token;

pub enum Statement<'a> {
    Class(Class<'a>),
    AbstractClass(AbstractClass<'a>),
    Interface(Interface<'a>),
    Mixin(Mixin<'a>),
    Enum(Enum<'a>),
    Function(FunctionDefinition<'a>),
    Use(UseClause<'a>),
    Constant(ConstAssignment<'a>)
}

pub enum UseClause<'a> {
    /// End of use statement
    None,
    /// "*"
    Wildcard,
    // An identifier, optionally followed by another use clause
    // Yes it's a linked list
    Identifier(Identifier<'a>, Box<UseClause<'a>>),
    IdentifierWithAlias(Identifier<'a>, Identifier<'a>),
    // A block allowing multiple items to be used from the same namespace
    BracesOrSomething(Vec<UseClause<'a>>),
    /// Only allowed inside "use lib::{self, ns1, *}"
    /// Allows optional alias
    SelfKeyword(Option<Identifier<'a>>)
}

// Wait, isn't this more like a static constant?
// In Rust, you have let and let mut.

pub struct ConstAssignment<'a> {
    name: Identifier<'a>,
    /// Unlike let assignments, the type is required.
    item_type: Type<'a>,
    /// Must be checked if it can be run at compile time
    value: Expression<'a>
}

pub struct StatementBlock<T> {
    pub statements: Vec<T>,
}

#[derive(Clone, Copy)]
pub struct Identifier<'a>(pub Token<'a>);

impl<'a> Identifier<'a> {
    pub fn name(&self) -> &'a str { self.0.lexeme }
}

pub enum Type<'a> {
    Named(Identifier<'a>, Vec<Type<'a>>),
    Interface(InterfaceBody<'a>),
    Tuple(Vec<Type<'a>>),
    Object{
        body: AbstractClassBody<'a>,
        supertype: Option<Box<Type<'a>>>
    },
    Array(Box<Type<'a>>, usize),
    Slice(Box<Type<'a>>),
    Reference(Box<Type<'a>>),
    Pointer(Box<Type<'a>>),
    Function{
        parameters: Vec<Type<'a>>,
        // Defaults to nil
        return_type: Option<Box<Type<'a>>>,
    },
    Union(Vec<Type<'a>>),
    Literal(Literal<'a>),
}

pub struct FunctionDeclaration<'a> {
    pub name: Identifier<'a>,
    pub generics: Vec<Generic<'a>>,
    pub parameters: Vec<MatchClause<'a>>,
    pub return_type: Option<Type<'a>>,
}

pub struct FunctionDefinition<'a> {
    pub declaration: FunctionDeclaration<'a>,
    pub body: Block<'a>
}

pub type Block<'a> = StatementBlock<FunctionStatement<'a>>;

pub enum FunctionStatement<'a> {
    // Note: check for expression statement which don't cause side-effects or are not the last statement in the block
    Expression(Expression<'a>),
    Let(LetStatement<'a>),
    Assignment(Assignment<'a>),
    Return(Option<Expression<'a>>),
    Forever(Block<'a>),
    While(WhileStatement<'a>),
    For(ForStatement<'a>),
    Break(BreakStatement<'a>),
    Continue(ContinueStatement<'a>),
}

pub struct WhileStatement<'a> {
    pub condition: Expression<'a>,
    pub body: Block<'a>,
    /// Used for when a labeled while block ends by the condition evaluating to false
    pub else_branch: Option<Block<'a>>
}

pub struct ForStatement<'a> {
    loop_variable: Option<MatchClause<'a>>,
    /// If no "in" is found and an identifier was parsed, promote that identifier to an expression
    iterator: Expression<'a>
}

pub struct BreakStatement<'a> {
    label: Option<Identifier<'a>>,
    value: Option<Expression<'a>>
}

pub struct LetStatement<'a> {
    pub clause: MatchClause<'a>,
    pub value: Option<Expression<'a>>
}

pub struct Assignment<'a> {
    pub target: AssignmentTarget<'a>,
    pub value: Expression<'a>
}

pub enum AssignmentTarget<'a> {
    Variable(Identifier<'a>),
    Field(Expression<'a>, Identifier<'a>),
    Subscript{
        array: Expression<'a>,
        index: Expression<'a>
    },
    Dereference(Expression<'a>)
}

pub enum Literal<'a> {
    Nil,
    False,
    True,
    Integer(u64),
    Float(f64),
    String(InterpolatedString<'a>),
    Tuple(Vec<Expression<'a>>),
    Object(Vec<(Identifier<'a>, Expression<'a>)>),
    Array(Vec<Expression<'a>>),
    Range(Option<Box<Expression<'a>>>, Option<Box<Expression<'a>>>),
}

pub struct InterpolatedString<'a>{
    token: Token<'a>,
    pieces: Vec<InterpolatedStringPiece<'a>>,
}

pub enum InterpolatedStringPiece<'a> {
    Literal(&'a str),
    Escaped(char),
    Expression(Expression<'a>),
    TypedExpression(Expression<'a>, Type<'a>)
}

pub enum Expression<'a> {
    Block(Block<'a>),
    Binary(Token<'a>, Box<Expression<'a>>, Box<Expression<'a>>),
    Unary(Token<'a>, Box<Expression<'a>>),
    /// "object.foo"
    Field(Box<Expression<'a>>, Identifier<'a>),
    /// "tuple.1"
    TupleField(Box<Expression<'a>>, u32),
    /// "array[index]"
    Subscript{
        array: Box<Expression<'a>>,
        index: Box<Expression<'a>>
    },
    /// "function(a, b, c)"
    FunctionCall{
        function: Box<Expression<'a>>,
        arguments: Vec<Expression<'a>>
    },
    /// "*pointer"
    Dereference(Box<Expression<'a>>),
    If(IfExpression<'a>),
    Match(MatchExpression<'a>),
    Identifier(Identifier<'a>),
    With(WithExpression<'a>),
    Literal(Literal<'a>)
}

pub struct ConditionalBlock<'a> {
    pub condition: Box<Expression<'a>>,
    pub body: Block<'a>
}

// Note: "unless" is syntactic sugar for if. It implicitly wraps the expression in "not"
pub struct IfExpression<'a> {
    pub conditional_branches: Vec<ConditionalBlock<'a>>,
    pub else_branch: Block<'a> 
}

pub struct MatchExpression<'a> {
    pub input: Box<Expression<'a>>,
    pub match_arms: Vec<(MatchClause<'a>, Expression<'a>)>
}

pub struct MatchClause<'a> {
    name: Option<Identifier<'a>>,
    destructure: Option<Destructure<'a>>,
    item_type: Option<Type<'a>>,
    // "if|unless <condition>"
    guard_clause: Option<Expression<'a>>
}

pub enum Destructure<'a> {
    Tuple(Vec<MatchClause<'a>>),
    Array(Vec<ArrayDestructure<'a>>),
    Object(Vec<ObjectDestructure<'a>>)
}

pub enum ArrayDestructure<'a> {
    Splat(Option<Destructure<'a>>),
    Plain(MatchClause<'a>)
}

/// Syntax: "name @ des: type = field"
pub struct ObjectDestructure<'a> {
    clause: MatchClause<'a>,
    // If None, use the destructure's name
    field: Option<Identifier<'a>>
}

pub struct WithExpression<'a> {
    variables: Vec<Identifier<'a>>,
    body: Block<'a>
}

pub struct Generic<'a> {
    pub name: Identifier<'a>,
    pub supertype: Type<'a>,
    pub default_type: Option<Type<'a>>
}

pub struct Class<'a> {
    pub generics: Vec<Generic<'a>>,
    pub name: Identifier<'a>,
    pub parent: Option<Type<'a>>,
    pub with: Vec<Type<'a>>,
    pub body: ClassBody<'a>
}

pub type ClassBody<'a> = StatementBlock<ClassStatement<'a>>;

// Todo make separate AbstractClass struct
pub enum ClassStatement<'a> {
    StaticField(StaticField<'a>),
    Field(Field<'a>),
    TypeDefinition(TypeDefinition<'a>),
    Method(FunctionDefinition<'a>),
    StaticMethod(FunctionDefinition<'a>)
}

pub struct Field<'a> {
    name: Identifier<'a>,
    item_type: Type<'a>,
}

pub struct AbstractClass<'a> {
    pub generics: Vec<Generic<'a>>,
    pub name: Identifier<'a>,
    pub parent: Option<Type<'a>>,
    pub with: Vec<Type<'a>>,
    pub body: AbstractClassBody<'a>
}

pub type InterfaceBody<'a> = StatementBlock<InterfaceStatement<'a>>;
pub type AbstractClassBody<'a> = InterfaceBody<'a>;

pub enum InterfaceStatement<'a> {
    StaticRequirement(PartialStaticField<'a>),
    StaticDefinition(StaticField<'a>),
    FieldRequirement(PartialField<'a>),
    FieldDefinition(Field<'a>),
    TypeRequirement(TypeDeclaration<'a>),
    TypeDefinition(TypeDefintion<'a>),
    StaticMethodRequirement(FunctionDeclaration<'a>),
    StaticMethod(FunctionDefinition<'a>),
    MethodRequirement(FunctionDeclaration<'a>),
    Method(FunctionDefinition<'a>)
}

pub struct PartialStaticField<'a> {
    pub name: Identifier<'a>,
    pub item_type: Type<'a>,
}

pub struct StaticField<'a> {
    pub name: Identifier<'a>,
    pub item_type: Type<'a>,
    pub value: Expression<'a>
}

pub struct Interface<'a> {
    pub generics: Vec<Generic<'a>>,
    pub name: Identifier<'a>,
    pub parent: Option<Type<'a>>,
    pub body: InterfaceBody<'a>
}

pub struct Mixin<'a> {
    pub name: Identifier<'a>,
    pub generics: Vec<Generic<'a>>,
    pub body: ClassBody<'a>
}

pub struct Enum<'a> {
    pub name: Identifier<'a>,
    pub generics: Vec<Generic<'a>>,
    pub body: EnumBody<'a>
}

pub type EnumBody<'a> = Block<(Identifier<'a>, EnumValue<'a>)>;

pub enum EnumValue<'a> {
    None,
    // "= 1"
    Value(Literal<'a>),
    ValueOfConstant(Identifier<'a>),
    // "(Type1, Type2)"
    Tuple(Vec<Type<'a>>),
    // "{ name: Type }"
    Object(Vec<(Identifier<'a>, Type<'a>)>)
}