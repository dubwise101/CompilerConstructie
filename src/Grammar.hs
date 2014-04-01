module Grammar where

data SPL        = Decl [Decl] deriving (Show, Eq)
data Decl       = VarDecl | FunDecl deriving (Show, Eq)
data VarDecl    = Eq Type Id Exp deriving (Show, Eq)
data FunDecl    = Func RetType Id Paren FArgs Curl [VarDecl] Stmt [Stmt] deriving (Show, Eq)
data RetType    = RType | Void deriving (Show, Eq)
data Type       = Int|Bool| EmptyList |Id String | List Type | Tup Type Comma Type | TVoid deriving (Show, Eq)
data FArgs      = FunArg [(FArgs,Comma)] Type Id deriving (Show, Eq)
data Stmt       = MultState
                | IfState
                | ElseState
                | WhileState
                | EqState
                | FuncSate
                | Return deriving (Show, Eq)
data Exp        = Item Id Field
                | Sum Exp Op2 Exp
                | Val Op1 Exp
                | Intv Int
                | ExBool Bool
                | Pexp Paren Exp Paren
                | FunCa FunCall
                | EmpList
                | DExp Paren Exp Comma Exp  deriving (Show, Eq)
data Field      = EField | FieldT FieldTypes deriving (Show, Eq)
data FieldTypes = FieldHD | FieldTL | FieldFST | FieldSND deriving (Show, Eq)
data FunCall    = FunC Id Paren ActArgs | FunCV Id Paren deriving (Show, Eq)
data ActArgs    = ActArg Exp Comma ActArgs | Exp deriving (Show, Eq)
data Op2        = Plus | Minus | Times | Div | Mod | Comp | Lt | Gt | LEQ | GEQ | NEQ | AND | OR | Colon
         deriving (Show, Eq)
data Op1        = ExclMark | PreMinus
        deriving (Show, Eq)
data Curl       = CurlO | CurlC  deriving (Show, Eq)
data Paren      = ParenceO | ParenceC deriving (Show, Eq)
data Brack      = BrackO | BrackC deriving (Show, Eq)
newtype  Alpha  = MKAlpha Char deriving (Show, Eq)
data  AlphaNum  = Digits | Alpha | Undersc deriving (Show, Eq)
data Id         = Name Alpha [AlphaNum] deriving (Show, Eq)
data Comma      = Com deriving (Show, Eq)

data Token      = TokSPL
                | TokDecl
                | TokVarDecl
                | TokFunDecl
                | TokRetType RetType
                | TokType Type
                | TokFArgs
                | TokStmt Stmt
                | TokExp
                | TokField Field
                | TokFunCall
                | TokActArgs
                | TokOp2 Op2
                | TokOp1 Op1
                | TokInt Int
                | TokId String
                | TokCurl Curl
                | TokParen Paren
                | TokError String
                | TokBool Bool
                | TokText String
                | TokBrack Brack
                | TokComma

data Tree a      = Branch Token [Tree a] -- | Error [a] String deriving (Show)

instance Show (Token) where
    show (TokRetType x)                 = show x
    show (TokType EmptyList)            = "[]"
    show (TokType (Id a))               = a
    show (TokType (List a))             = "List "++ show a
    show (TokType x)                    = show x
    show TokFArgs                       = ""
    show (TokStmt Return)               = "return "
    show (TokStmt IfState)              = "if"
    show (TokStmt ElseState)            = "else"
    show (TokStmt WhileState)           = "while"
    show (TokStmt MultState)            = "{"
    show (TokStmt _)                    = ""
    show TokExp                          = ""
    show (TokField (FieldT FieldHD))    = ".hd"
    show (TokField (FieldT FieldTL))    = ".tl"
    show (TokField (FieldT FieldFST))   = ".fst"
    show (TokField (FieldT FieldSND))   = ".fst"
    show (TokField x)                   = show x
    show (TokFunCall )                  = ""
    show (TokActArgs )                  = ""
    show (TokOp2 Plus)                  = "+"
    show (TokOp2 Minus)                 = "-"
    show (TokOp2 Times)                 = "*"
    show (TokOp2 Div)                   = "/"
    show (TokOp2 Mod)                   = "%"
    show (TokOp2 Mod)                   = "%"
    show (TokOp2 Comp)                  = "=="
    show (TokOp2 Lt)                    = "<"
    show (TokOp2 Gt)                    = ">"
    show (TokOp2 LEQ)                   = "<="
    show (TokOp2 GEQ)                   = ">="
    show (TokOp2 NEQ)                   = "!="
    show (TokOp2 AND)                   = "&&"
    show (TokOp2 OR)                    = "||"
    show (TokOp2 Colon)                 = ":"
    show (TokOp1 ExclMark)              = "!"
    show (TokOp1 PreMinus)              = "-"
    show (TokId x)                      = x
    show (TokCurl CurlO)                = "{"
    show (TokCurl CurlC)                = "}"
    show (TokInt x)                     = show x
    show (TokError x)                   = x
    show (TokParen ParenceO)            = "("
    show (TokParen ParenceC)            = ")"
    show (TokBrack BrackO)              = "["
    show (TokBrack BrackC)              = "] "
    show (TokBool x)                    = show x
    show (TokText x)                    = x
    show (TokComma)                     = ","