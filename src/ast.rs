#[derive(PartialEq, Debug)]
pub struct Name(pub String);

#[derive(PartialEq, Debug)]
pub enum Compare {
    GT,
    LT,
    GTE,
    LTE,
    NEQ,
    EQ,
}

#[derive(Debug, PartialEq)]
pub struct FuncName {
    pub path: Vec<Name>,
    pub method: Option<Name>,
}

/// binop ::=  ‘+’ | ‘-’ | ‘*’ | ‘/’ | ‘//’ | ‘^’ | ‘%’ |
///         ‘&’ | ‘~’ | ‘|’ | ‘>>’ | ‘<<’ | ‘..’ |
///         ‘<’ | ‘<=’ | ‘>’ | ‘>=’ | ‘==’ | ‘~=’ |
///         and | or
#[derive(Debug, PartialEq)]
pub enum BinOp {
    Plus,
    Minus,
    Mul,
    Div,
    IntDiv,
    Pow,
    Mod,
    BitAnd,
    BitXor,
    BitOr,
    BitShr,
    BitShl,
    Concat,
    LT,
    LTE,
    GT,
    GTE,
    EQ,
    NEQ,
    And,
    Or,
}

#[derive(Debug, PartialEq)]
pub struct BinExp {
    pub op: BinOp,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug, PartialEq)]
pub struct UnExp {
    pub op: Unop,
    pub exp: Box<Expr>,
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Nil,
    Bool(bool),
    Num(f64),
    Str(String),
    Dots,
    FuncDef(FunctionDef),
    PrefixExp(Box<PrefixExpr>),
    Table(TableConstructor),
    BinExp(BinExp),
    UnExp(UnExp),
}

#[derive(Debug, PartialEq)]
pub enum Field {
    NameAssign(Name, Expr),
    ExprAssign(Expr, Expr),
    PosAssign(Expr),
}

#[derive(Debug, PartialEq)]
pub struct TableConstructor(pub Vec<Field>);

#[derive(Debug, PartialEq)]
pub enum Args {
    ExprList(Vec<Expr>),
    TableConstructor(TableConstructor),
    String(String),
}

#[derive(Debug, PartialEq)]
pub struct FunctionCall {
    pub expr: Box<PrefixExpr>,
    pub args: Args,
}

#[derive(Debug, PartialEq)]
pub struct IndexExpr {
    pub expr: Box<PrefixExpr>,
    pub arg: Expr,
}

#[derive(Debug, PartialEq)]
pub struct PropertyAccess {
    pub expr: Box<PrefixExpr>,
    pub name: Name,
}

#[derive(Debug, PartialEq)]
pub enum Var {
    Name(Name),
    IndexExpr(IndexExpr),
    PropertyAccess(PropertyAccess),
}

#[derive(Debug, PartialEq)]
pub enum PrefixExpr {
    Var(Var),
    FunctionCall(FunctionCall),
    Expr(Expr),
}

#[derive(Debug, PartialEq)]
pub struct Block {
    pub stats: Vec<Stat>,
    pub retstat: Option<Vec<Expr>>,
}

/// varlist '=' explist
#[derive(Debug, PartialEq)]
pub struct Assignment {
    pub varlist: Vec<Var>,
    pub exprlist: Vec<Expr>,
}

/// local namelist [‘=’ explist]
#[derive(Debug, PartialEq)]
pub struct LocalAssignment {
    pub namelist: Vec<Name>,
    pub exprlist: Option<Vec<Expr>>,
}

/// while exp do block end
#[derive(Debug, PartialEq)]
pub struct WhileBlock {
    pub expr: Expr,
    pub block: Block,
}

/// repeat block until exp
#[derive(Debug, PartialEq)]
pub struct RepeatBlock {
    pub block: Block,
    pub expr: Expr,
}

#[derive(Debug, PartialEq)]
pub struct ElseIf {
    pub expr: Expr,
    pub block: Block,
}

/// if exp then block {elseif exp then block} [else block] end
#[derive(Debug, PartialEq)]
pub struct IfBlock {
    pub expr: Expr,
    pub block: Block,
    pub elseif: Vec<ElseIf>,
    pub else_blk: Option<Block>,
}

/// for Name ‘=’ exp ‘,’ exp [‘,’ exp] do block end
#[derive(Debug, PartialEq)]
pub struct ForRange {
    pub name: Name,
    pub exprs: (Expr, Expr, Option<Expr>),
    pub block: Block,
}

/// for namelist in explist do block end
#[derive(Debug, PartialEq)]
pub struct ForIn {
    pub namelist: Vec<Name>,
    pub exprlist: Vec<Expr>,
    pub block: Block,
}

#[derive(Debug, PartialEq)]
pub enum Stat {
    SemiColon,
    Assignment(Assignment), // varlist '=' explist
    FunctionCall(FunctionCall),
    Label(Name),
    Break,
    Goto(Name),
    DoBlock(Block),
    WhileBlock(WhileBlock),
    RepeatBlock(RepeatBlock),
    IfBlock(Box<IfBlock>),
    ForRange(Box<ForRange>),
    ForIn(ForIn),
    FunctionDef(FunctionDef),
    LocalFunctionDef(LocalFunctionDef),
    LocalAssignment(LocalAssignment),
}

#[derive(Debug, PartialEq)]
pub struct FuncBody {
    pub params: Params,
    pub body: Block,
}

#[derive(Debug, PartialEq)]
pub struct LocalFunctionDef {
    pub name: Name,
    pub body: FuncBody,
}

#[derive(Debug, PartialEq)]
pub struct FunctionDef {
    pub name: FuncName,
    pub body: FuncBody,
}

#[derive(Debug, PartialEq)]
pub struct Params {
    pub names: Vec<Name>,
    pub variadic: bool,
}

#[derive(Debug, PartialEq)]
pub enum Unop {
    Minus,
    Not,
    Len,
    BitNot,
}
