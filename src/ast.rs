use crate::token::Token;

#[derive(Clone)]
pub enum Statement<'a> {
    Class(Class<'a>),
    AbstractClass(AbstractClass<'a>),
    Interface(Interface<'a>),
    Mixin(Mixin<'a>),
    Enum(Enum<'a>),
    Extend(ExtendWith<'a>),
    TypeDefinition(TypeDefinition<'a>),
    ConstantDefinition(ConstAssignment<'a>),
    Function(FunctionDefinition<'a>),
    Use(UsePath<'a>),
    Constant(ConstAssignment<'a>)
}

#[derive(Clone)]
pub struct ExtendWith<'a> {
    pub extend: Type<'a>,
    pub with: Option<Type<'a>>,
    pub body: ClassBody<'a>
}

#[derive(Clone)]
pub enum UsePath<'a> {
    /// End of use statement
    None,
    /// "use .name"
    TopLevel(Box<UsePath<'a>>),
    /// "*"
    Wildcard,
    // An identifier, optionally followed by another use clause
    // Yes it's a linked list
    Identifier(Identifier<'a>, Box<UsePath<'a>>),
    IdentifierWithAlias(Identifier<'a>, Identifier<'a>),
    // A block allowing multiple items to be used from the same namespace
    Block(Vec<UsePath<'a>>),
    /// Only allowed inside "use lib.{self, ns1, *}"
    SelfKeyword,
    /// "{self as foo}"
    SelfWithAlias(Identifier<'a>)
}

// Wait, isn't this more like a static constant?
// In Rust, you have let and let mut.

#[derive(Clone)]
pub struct ConstAssignment<'a> {
    pub name: Identifier<'a>,
    /// Unlike let assignments, the type is required.
    pub item_type: Type<'a>,
    /// Must be checked if it can be run at compile time
    pub value: Expression<'a>
}

#[derive(Clone)]
pub struct StatementBlock<T> {
    pub statements: Vec<T>,
}

#[derive(Clone, Copy)]
pub struct Identifier<'a>(pub Token<'a>);

impl<'a> Identifier<'a> {
    pub fn name(&self) -> &'a str { self.0.lexeme }
}

#[derive(Clone)]
pub enum Type<'a> {
    Named{
        path: Vec<Identifier<'a>>,
        generics: Vec<Type<'a>>
    },
    SelfKeyword{
        path: Vec<Identifier<'a>>,
        generics: Vec<Type<'a>>
    },
    Interface(InterfaceBody<'a>),
    Tuple(Vec<Type<'a>>),
    Object(ObjectType<'a>),
    Array(Box<Type<'a>>, Expression<'a>),
    Slice(Box<Type<'a>>),
    Reference(Box<Type<'a>>),
    Pointer(Box<Type<'a>>),
    Function(FunctionType<'a>),
    Union(Vec<Type<'a>>),
    Intersection(Vec<Type<'a>>),
    Literal(Literal<'a>),
}

#[derive(Clone)]
pub struct FunctionType<'a> {
    pub parameters: Vec<Type<'a>>,
    pub return_type: Option<Box<Type<'a>>>
}

#[derive(Clone)]
pub struct ObjectType<'a> {
    pub body: ClassBody<'a>,
    pub supertype: Option<Box<Type<'a>>>
}

#[derive(Clone)]
pub struct TypePath<'a> {
    pub top: Identifier<'a>,
    pub rest: Vec<Identifier<'a>>
}

#[derive(Clone)]
pub struct FunctionDeclaration<'a> {
    pub name: Identifier<'a>,
    pub generics: Vec<Generic<'a>>,
    pub parameters: Vec<MatchClause<'a>>,
    pub return_type: Option<Type<'a>>,
}

#[derive(Clone)]
pub struct FunctionDefinition<'a> {
    pub declaration: FunctionDeclaration<'a>,
    pub body: Block<'a>
}

pub type Block<'a> = StatementBlock<FunctionStatement<'a>>;

#[derive(Clone)]
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
    Continue(Option<Identifier<'a>>),
}

#[derive(Clone)]
pub struct WhileStatement<'a> {
    pub condition: Expression<'a>,
    pub body: Block<'a>,
    /// Used for when a labeled while block ends by the condition evaluating to false
    pub else_branch: Option<Block<'a>>
}

#[derive(Clone)]
pub struct ForStatement<'a> {
    pub loop_variable: Option<MatchClause<'a>>,
    /// If no "in" is found and an identifier was parsed, promote that identifier to an expression
    pub iterator: Expression<'a>,
    pub body: Block<'a>
}

#[derive(Clone)]
pub struct BreakStatement<'a> {
    pub label: Option<Identifier<'a>>,
    pub value: Option<Expression<'a>>
}

#[derive(Clone)]
pub struct LetStatement<'a> {
    pub clause: MatchClause<'a>,
    pub value: Option<Expression<'a>>
}

#[derive(Clone)]
pub struct Assignment<'a> {
    pub target: AssignmentTarget<'a>,
    pub value: Expression<'a>
}

#[derive(Clone)]
pub enum AssignmentTarget<'a> {
    Variable(Identifier<'a>),
    Field(Expression<'a>, Identifier<'a>),
    Subscript{
        array: Expression<'a>,
        index: Expression<'a>
    },
    Dereference(Expression<'a>)
}

#[derive(Clone)]
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

#[derive(Clone)]
pub struct InterpolatedString<'a>{
    pub token: Token<'a>,
    pub pieces: Vec<InterpolatedStringPiece<'a>>,
}

#[derive(Clone)]
pub enum InterpolatedStringPiece<'a> {
    Literal(&'a str),
    Escaped(char),
    Expression(Expression<'a>),
    TypedExpression(Expression<'a>, Type<'a>)
}

#[derive(Clone)]
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
    /// "pointer.*"
    Dereference(Box<Expression<'a>>),
    If(IfExpression<'a>),
    Match(MatchExpression<'a>),
    Identifier(Identifier<'a>),
    With(WithExpression<'a>),
    Literal(Literal<'a>)
}

#[derive(Clone)]
pub struct ConditionalBlock<'a> {
    pub condition: Box<Expression<'a>>,
    pub body: Block<'a>
}

// Note: "unless" is syntactic sugar for if. It implicitly wraps the expression in "not"
#[derive(Clone)]
pub struct IfExpression<'a> {
    pub conditional_branches: Vec<ConditionalBlock<'a>>,
    pub else_branch: Block<'a> 
}

#[derive(Clone)]
pub struct MatchExpression<'a> {
    pub input: Box<Expression<'a>>,
    pub match_arms: Vec<(MatchClause<'a>, Expression<'a>)>
}

#[derive(Clone)]
pub struct MatchClause<'a> {
    pub name: Option<Identifier<'a>>,
    pub destructure: Option<Destructure<'a>>,
    pub item_type: Option<Type<'a>>,
    // "if|unless <condition>"
    pub guard_clause: Option<Expression<'a>>
}

impl<'a> MatchClause<'a> {
    pub fn as_identifier(self) -> Option<Identifier<'a>> {
        if self.destructure.is_none() && self.item_type.is_none() && self.guard_clause.is_none() {
            self.name
        } else {
            None
        }
    }
    
    pub fn as_literal(self) -> Option<Literal<'a>> {
        if self.name.is_none() && self.destructure.is_none() && self.guard_clause.is_none() {
            if let Type::Literal(lit) = self.item_type? {
                return Some(lit);
            }
        }
        return None;
    }
}

#[derive(Clone)]
pub enum Destructure<'a> {
    Tuple(Vec<MatchClause<'a>>),
    Array(Vec<ArrayDestructure<'a>>),
    Object(Vec<ObjectDestructure<'a>>)
}

#[derive(Clone)]
pub enum ArrayDestructure<'a> {
    Splat(Option<Destructure<'a>>),
    Plain(MatchClause<'a>)
}

/// Syntax: "name @ des: type = field"
#[derive(Clone)]
pub struct ObjectDestructure<'a> {
    clause: MatchClause<'a>,
    // If None, use the destructure's name
    field: Option<Identifier<'a>>
}

#[derive(Clone)]
pub struct WithExpression<'a> {
    pub variables: Vec<Identifier<'a>>,
    pub body: Block<'a>
}

#[derive(Clone)]
pub struct Generic<'a> {
    pub name: Identifier<'a>,
    pub supertype: Option<Type<'a>>,
    pub default_type: Option<Type<'a>>
}

#[derive(Clone)]
pub struct Class<'a> {
    pub generics: Vec<Generic<'a>>,
    pub name: Identifier<'a>,
    pub parent: Option<Type<'a>>,
    pub with: Vec<Type<'a>>,
    pub body: ClassBody<'a>
}

pub type ClassBody<'a> = StatementBlock<ClassStatement<'a>>;

// Todo make separate AbstractClass struct
#[derive(Clone)]
pub enum ClassStatement<'a> {
    StaticField(StaticField<'a>),
    Field(Field<'a>),
    TypeDefinition(TypeDefinition<'a>),
    Method(FunctionDefinition<'a>),
    StaticMethod(FunctionDefinition<'a>)
}

#[derive(Clone)]
pub struct TypeDefinition<'a> {
    pub name: Identifier<'a>,
    pub generics: Vec<Generic<'a>>,
    pub type_value: Type<'a>
}

#[derive(Clone)]
pub struct Field<'a> {
    pub name: Identifier<'a>,
    pub item_type: Type<'a>,
}

#[derive(Clone)]
pub struct AbstractClass<'a> {
    pub generics: Vec<Generic<'a>>,
    pub name: Identifier<'a>,
    pub parent: Option<Type<'a>>,
    pub with: Vec<Type<'a>>,
    pub body: AbstractClassBody<'a>
}

pub type InterfaceBody<'a> = StatementBlock<InterfaceStatement<'a>>;
pub type AbstractClassBody<'a> = InterfaceBody<'a>;

#[derive(Clone)]
pub enum InterfaceStatement<'a> {
    StaticRequirement(PartialStaticField<'a>),
    StaticDefinition(StaticField<'a>),
    Field(Field<'a>),
    TypeRequirement(Identifier<'a>),
    TypeDefinition{
        name: Identifier<'a>,
        item_type: Type<'a>
    },
    StaticMethodRequirement(FunctionDeclaration<'a>),
    StaticMethod(FunctionDefinition<'a>),
    MethodRequirement(FunctionDeclaration<'a>),
    Method(FunctionDefinition<'a>)
}

#[derive(Clone)]
pub struct PartialStaticField<'a> {
    pub name: Identifier<'a>,
    pub item_type: Type<'a>,
}

#[derive(Clone)]
pub struct StaticField<'a> {
    pub name: Identifier<'a>,
    pub item_type: Type<'a>,
    pub value: Expression<'a>
}

#[derive(Clone)]
pub struct Interface<'a> {
    pub generics: Vec<Generic<'a>>,
    pub name: Identifier<'a>,
    pub parent: Option<Type<'a>>,
    pub body: InterfaceBody<'a>
}

#[derive(Clone)]
pub struct Mixin<'a> {
    pub name: Identifier<'a>,
    pub generics: Vec<Generic<'a>>,
    pub body: ClassBody<'a>
}

#[derive(Clone)]
pub struct Enum<'a> {
    pub name: Identifier<'a>,
    pub generics: Vec<Generic<'a>>,
    pub body: EnumBody<'a>
}

pub type EnumBody<'a> = Vec<(Identifier<'a>, EnumValue<'a>)>;

#[derive(Clone)]
pub enum EnumValue<'a> {
    None,
    // "= 1"
    Value(Literal<'a>),
    NamedConstant(Identifier<'a>),
    // "(Type1, Type2)"
    Tuple(Vec<Type<'a>>),
    // "{ name: Type }"
    Object(Vec<(Identifier<'a>, Type<'a>)>)
}