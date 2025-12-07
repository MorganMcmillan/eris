use crate::token::Token;

pub enum Statement<'a> {
    Class(Class<'a>),
    Interface(Interface<'a>),
    Mixin(Mixin<'a>),
    Enum(Enum<'a>),
    Function(FunctionDefinition<'a>),
    Use(Use<'a>),
    Constant(ConstAssignment<'a>)
}

pub struct StatementBlock<T> {
    pub statements: Vec<T>,
}

pub struct Identifier<'a>(pub Token<'a>);

impl<'a> Identifier<'a> {
    pub fn name(&self) -> &'a str { self.0.lexeme }
}

pub enum Type<'a> {
    Named(Identifier<'a>, Vec<Type<'a>>),
    Interface(InterfaceBody),
    Tuple(Vec<Type<'a>>),
    Object{
        body: AbstractClassBody,
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
    pub generics: Vec<Generic>,
    pub parameters: Vec<MatchClause>,
    pub return_type: Option<Type<'a>>,
}

pub struct FunctionDefinition<'a> {
    pub declaration: FunctionDeclaration<'a>,
    pub body: Block<'a>
}

pub type Block<'a> = StatementBlock<FunctionStatement<'a>>;

pub enum FunctionStatement<'a> {
    // Note: check for expression statement which don't cause side-effects or are not the last statement in the block
    Expression(Expression),
    Let(LetStatement),
    Assignment(Assignment),
    Return(ReturnStatement),
    Forever(ForeverStatement),
    While(WhileStatement),
    For(ForStatement),
    Break(BreakStatement),
    Continue(ContinueStatement),
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
    Function()
    Range(Option<Box<Literal<'a>>>, Option<Box<Literal<'a>>>),
}

pub enum Expression<'a> {
    Block(Block<'a>),
    Binary(Token<'a>, Box<Expression<'a>>, Box<Expression<'a>>),
    Unary(Token<'a>, Box<Expression<'a>>),
    Field(Box<Expression<'a>>, Identifier<'a>),
    Subscript{
        array: Box<Expression<'a>>,
        index: Box<Expression<'a>>
    },
    FunctionCall{
        function: Box<Expression<'a>>,
        arguments: Vec<Expression<'a>>
    },
    If(IfExpression<'a>),
    Match(MatchExpression<'a>),
    Identifier(Identifier<'a>),
    With(WithExpression),
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
    // Todo
}

pub struct Generic<'a> {
    pub name: Identifier<'a>,
    pub supertype: Type,
    pub default_type: Option<Type>
}

pub struct Class<'a> {
    pub generics: Vec<Generic>,
    pub name: Identifier<'a>,
    pub parent: Option<Type>,
    pub with: Vec<Type<'a>>,
    pub body: ClassBody
}

type ClassBody = Block<ClassStatement>;

// Todo make separate AbstractClass struct
pub enum ClassStatement {
    // Can be assigned either a value or (for abstract classes only) be left blank
    StaticField(StaticField),
    Field(Field),
    // Abstract class only,
    TypeDeclaration(TypeDeclaration),
    TypeDefinition(TypeDefinition),
    Method(FunctionDefinition),
    StaticMethod(FunctionDefinition)
}

pub struct Interface<'a> {
    pub generics: Vec<Generic>,
    pub name: Identifier<'a>,
    pub parent: Option<Type>,
    pub body: InterfaceBody
}

pub type InterfaceBody = Block<InterfaceStatement>;

pub enum InterfaceStatement {
    MethodDeclaration(FunctionDeclaration),
    MethodDefinition(FunctionDefinition),
    TypeDeclaration(TypeDeclaration),
    StaticDeclaration(StaticField)
}

pub struct Mixin<'a> {
    pub name: Identifier<'a>,
    pub generics: Vec<Generic>,
    pub body: ClassBody
}

pub struct Enum<'a> {
    pub name: Identifier<'a>,
    pub generics: Vec<Generic>,
    pub body: EnumBody<'a>
}

pub type EnumBody<'a> = Block<(Identifier<'a>, EnumValue<'a>)>;

pub enum EnumValue<'a> {
    None,
    // "= 1"
    Value(Literal),
    ValueOfConstant(Identifier<'a>),
    // "(Type1, Type2)"
    Tuple(Vec<Type>),
    // "{ name: Type }"
    Object(Vec<(Identifier<'a>, Type)>)
}