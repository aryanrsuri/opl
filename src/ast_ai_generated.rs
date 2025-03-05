// ast.rs

pub type Program = Vec<Statement>;

#[derive(Debug, Clone)]
pub enum Statement {
    LetStatement(Identifier, Expression),
    ExpressionStatement(Expression),
    TypeDeclaration(Identifier, TypeDefinition),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Identifier(pub String);

#[derive(Debug, Clone)]
pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    BooleanLiteral(bool),
    // Prefix expression, e.g. -a or !b.
    Prefix {
        operator: String,
        right: Box<Expression>,
    },
    // Infix expression, e.g. a + b.
    Infix {
        left: Box<Expression>,
        operator: String,
        right: Box<Expression>,
    },
    If {
        condition: Box<Expression>,
        consequence: Box<Expression>,
        alternative: Option<Box<Expression>>,
    },
    // Anonymous function literal.
    Function {
        parameters: Vec<Identifier>,
        body: Box<Expression>,
    },
    // Function application.
    FunctionCall {
        function: Box<Expression>,
        arguments: Vec<Expression>,
    },
    // Block expression: a series of statements inside { ... }.
    Block(Vec<Statement>),
    // List literal, e.g. [1, 2, 3]
    ListLiteral(Vec<Expression>),
    // Record literal, e.g. { key = value, ... }
    RecordLiteral(Vec<(Identifier, Expression)>),
    // Match expression for pattern matching.
    Match {
        expr: Box<Expression>,
        arms: Vec<(Pattern, Expression)>,
    },
    // Option and Result constructors.
    OptionSome(Box<Expression>),
    OptionNone,
    ResultOk(Box<Expression>),
    ResultError(Box<Expression>),
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Identifier(Identifier),
    IntegerLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    BooleanLiteral(bool),
    // Wildcard pattern: _
    Wildcard,
    // Constructor pattern, e.g. Some x, Ok y, etc.
    Constructor {
        name: Identifier,
        argument: Option<Box<Pattern>>,
    },
    // Infix pattern (e.g. for list cons): head :: tail.
    Infix {
        left: Box<Pattern>,
        operator: String,
        right: Box<Pattern>,
    },
    EmptyList,
}

#[derive(Debug, Clone)]
pub enum TypeDefinition {
    // Tagged union type: list of variants, each with an optional associated type.
    Union(Vec<(Identifier, Option<TypeAnnotation>)>),
    // Record type: list of fields and their types.
    Record(Vec<(Identifier, TypeAnnotation)>),
    // Type alias.
    Alias(TypeAnnotation),
}

#[derive(Debug, Clone)]
pub struct TypeAnnotation {
    pub name: Identifier,
    pub parameters: Vec<TypeAnnotation>,
}
