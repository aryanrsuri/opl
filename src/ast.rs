use crate::lexer::Token;
pub type Program = Vec<Statement>;

// FIXME: This should simply be a 'String.
pub type Identifier = Token;

#[derive(PartialEq, Debug, Clone)]
pub enum Statement {
    Let(Identifier, Expression),
    Return(Expression),
    Comment(Identifier),
    Expression(Expression),
    Type(Identifier, Type),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Literal {
    Integer(i128),
    Float(f64),
    String(String),
    Boolean(bool),
    // Char is not used
    Char(char),
    Unit,
    List(Vec<Expression>),
    Record(Vec<(Identifier, Expression)>),
    HashMap(Vec<(Expression, Expression)>),
    Tuple(Vec<Expression>),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Expression {
    Identifier(Identifier),
    // Option
    OptionSome(Box<Expression>),
    OptionNone,
    // Result
    ResultOk(Box<Expression>),
    ResultErr(Box<Expression>),
    // Union
    Variant(Identifier, Option<Box<Expression>>),
    // Literals
    Literal(Literal),
    // Expression Variants
    Prefix(Prefix, Box<Expression>),
    Infix(Infix, Box<Expression>, Box<Expression>),
    // Control Flow
    Block(Program),
    If {
        condition: Box<Expression>,
        consequence: Program,
        alternative: Option<Program>,
    },
    Function {
        parameters: Vec<Identifier>,
        body: Program,
    },
    Call {
        function: Box<Expression>,
        arguments: Vec<Expression>,
    },
    Match {
        expr: Box<Expression>,
        arms: Vec<(Pattern, Program)>,
    },
    // Replace BuiltIn with DotAccess and NamespacedCall
    DotAccess {
        object: Box<Expression>,
        property: Identifier,
    },
    NamespacedCall {
        namespace: Identifier,
        function: Identifier,
        arguments: Vec<Expression>,
    },
    // Range expression [start..end]
    Range {
        start: Box<Expression>,
        end: Box<Expression>,
    },
}

#[derive(PartialEq, Debug, Clone)]
pub enum Pattern {
    // e.g. this_is_an_identifier
    Identifier(Identifier),
    // e.g. 1 or "hello"
    Literal(Literal),
    // e.g. Some(a)
    Variant(Identifier, Option<Box<Pattern>>),
    // e.g. { a, b }
    Record(Vec<(Identifier, Pattern)>),
    // e.g a :: b
    Infix(Infix, Box<Pattern>, Box<Pattern>),
    // Underscore pattern _ to match any value
    Wildcard,
    // []
    Empty,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Type {
    Union(Vec<(Identifier, Option<Alias>)>),
    Record(Vec<(Identifier, Alias)>),
    Alias(Alias),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Constructor {
    Int,
    Float,
    String,
    Char,
    Bool,
    List,
    Option,
    Result,
    HashMap,
    Unit,
    Tuple,
    // Union, Records, and Aliases should be defined previously in a type statement
}

#[derive(PartialEq, Debug, Clone)]
pub enum TypeConstructor {
    BuiltIn(Constructor),
    Custom(Identifier),
}

#[derive(PartialEq, Debug, Clone)]
pub struct Alias {
    pub name: TypeConstructor,
    pub parameters: Vec<Alias>,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Prefix {
    Plus,
    Minus,
    Bang,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Infix {
    Plus,
    Minus,
    Concat, // ++
    Product,
    ForwardSlash,
    Equal,
    DoesNotEqual,
    GreaterThan,
    LessThan,
    GTOrEqual,
    LTOrEqual,
    Caret,
    Modulo,
    Ampersand,
    Cons,
    Pipe,
}

// Add namespace enum to track available namespaces
#[derive(PartialEq, Debug, Clone)]
pub enum Namespace {
    Std,    // Standard library (println, etc)
    List,   // List operations (map, filter, etc)
    Option, // Option operations
    Result, // Result operations
    String, // String operations
    Union,  // Union operations (is_variant, etc)
}
