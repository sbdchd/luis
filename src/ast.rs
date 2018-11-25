/// Name
#[derive(PartialEq, Debug)]
pub struct Name(pub String);

/// funcname ::= Name {‘.’ Name} [‘:’ Name]
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

/// exp binop exp
#[derive(Debug, PartialEq)]
pub struct BinExp {
    pub op: BinOp,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

/// unop exp
#[derive(Debug, PartialEq)]
pub struct UnExp {
    pub op: Unop,
    pub exp: Box<Expr>,
}

/// exp ::= nil | false | true | Numeral | LiteralString | ‘...’ | functiondef |
///         prefixexp | tableconstructor | exp binop exp | unop exp
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

/// field ::= ‘[’ exp ‘]’ ‘=’ exp | Name ‘=’ exp | exp
#[derive(Debug, PartialEq)]
pub enum Field {
    NameAssign(Name, Expr),
    ExprAssign(Expr, Expr),
    PosAssign(Expr),
}

/// tableconstructor ::= ‘{’ [fieldlist] ‘}’
#[derive(Debug, PartialEq)]
pub struct TableConstructor(pub Vec<Field>);

/// args ::=  ‘(’ [explist] ‘)’ | tableconstructor | LiteralString
#[derive(Debug, PartialEq)]
pub enum Args {
    ExprList(Vec<Expr>),
    TableConstructor(TableConstructor),
    String(String),
}

/// functioncall ::=  prefixexp args | prefixexp ‘:’ Name args
#[derive(Debug, PartialEq)]
pub struct FunctionCall {
    pub expr: Box<PrefixExpr>,
    pub args: Args,
}

/// prefixexp ‘[’ exp ‘]’
#[derive(Debug, PartialEq)]
pub struct IndexExpr {
    pub expr: Box<PrefixExpr>,
    pub arg: Expr,
}

/// prefixexp ‘.’ Name
#[derive(Debug, PartialEq)]
pub struct PropertyAccess {
    pub expr: Box<PrefixExpr>,
    pub name: Name,
}

/// var ::=  Name | prefixexp ‘[’ exp ‘]’ | prefixexp ‘.’ Name
#[derive(Debug, PartialEq)]
pub enum Var {
    Name(Name),
    IndexExpr(IndexExpr),
    PropertyAccess(PropertyAccess),
}

/// prefixexp ::= var | functioncall | ‘(’ exp ‘)’
#[derive(Debug, PartialEq)]
pub enum PrefixExpr {
    Var(Var),
    FunctionCall(FunctionCall),
    Expr(Expr),
}

/// block ::= {stat} [retstat]
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

/// elseif exp then block
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

/// stat ::=  ‘;’ |
///         varlist ‘=’ explist |
///         functioncall |
///         label |
///         break |
///         goto Name |
///         do block end |
///         while exp do block end |
///         repeat block until exp |
///         if exp then block {elseif exp then block} [else block] end |
///         for Name ‘=’ exp ‘,’ exp [‘,’ exp] do block end |
///         for namelist in explist do block end |
///         function funcname funcbody |
///         local function Name funcbody |
///         local namelist [‘=’ explist]
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

/// funcbody ::= ‘(’ [parlist] ‘)’ block end
#[derive(Debug, PartialEq)]
pub struct FuncBody {
    pub params: Params,
    pub body: Block,
}

/// local function Name funcbody
#[derive(Debug, PartialEq)]
pub struct LocalFunctionDef {
    pub name: Name,
    pub body: FuncBody,
}

/// function funcname funcbody
#[derive(Debug, PartialEq)]
pub struct FunctionDef {
    pub name: FuncName,
    pub body: FuncBody,
}

/// parlist ::= namelist [‘,’ ‘...’] | ‘...’
#[derive(Debug, PartialEq)]
pub struct Params {
    pub names: Vec<Name>,
    pub variadic: bool,
}

/// unop ::= ‘-’ | not | ‘#’ | ‘~’
#[derive(Debug, PartialEq)]
pub enum Unop {
    Minus,
    Not,
    Len,
    BitNot,
}
