use crate::lexer::token;

pub type BlockStatement = Vec<Statement>;
pub type Program = Vec<Statement>;

pub enum Node {
    Program(Program),
    Statement(Statement),
    Expression(Expression)
}

#[derive(Clone, Debug)]
pub enum Statement {
    Expression(Expression),
    Let(DeclareStatement),
    Const(DeclareStatement),
    Return(ReturnStatement),
    Block(BlockStatement),
}

#[derive(Clone, Debug)]
pub enum Expression {
    Identifier(String),
    Integer(token::Integer),
    Float(token::Float),
    Boolean(bool),
    String(String),
    Array(ArrayLiteral),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    If(IfExpression),
    Function(FunctionLiteral),
    Call(CallExpression),
    Index(IndexExpression),
    Hash(HashLiteral),
    Assign(AssignExpression)
}

#[derive(Clone, Debug)]
pub struct ArrayLiteral {
    pub elements: Vec<Expression>,
}

#[derive(Clone, Debug)]
pub struct PrefixExpression {
    pub operator: token::Token,
    pub right: Box<Expression>,
}

#[derive(Clone, Debug)]
pub struct InfixExpression {
    pub left: Box<Expression>,
    pub operator: token::Token,
    pub right: Box<Expression>,
}

#[derive(Clone, Debug)]
pub struct IfExpression {
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    // Else statement
    pub alternative: Option<BlockStatement>,
}

#[derive(Clone, Debug)]
pub struct DeclareStatement {
    pub name: String,
    pub value: Expression,
}

#[derive(Clone, Debug)]
pub struct FunctionLiteral {
    pub parameters: Vec<String>,
    pub body: BlockStatement,
}

#[derive(Clone, Debug)]
pub struct ReturnStatement {
    pub value: Expression,
}

#[derive(Clone, Debug)]
pub struct CallExpression {
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

#[derive(Clone, Debug)]
pub struct IndexExpression {
    pub left: Box<Expression>,
    pub index: Box<Expression>,
}

#[derive(Clone, Debug)]
pub struct AssignExpression {
    pub left: Box<Expression>,
    pub value: Box<Expression>
}

#[derive(Clone, Debug)]
pub struct HashLiteral {
    pub pairs: Vec<(Expression, Expression)>,
}