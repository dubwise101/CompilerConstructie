import Data.Char
import Data.Bool
import System.IO

data SPL        = Decl [Decl] deriving (Show, Eq)
data Decl       = VarDecl | FunDecl deriving (Show, Eq)
data VarDecl    = Eq Type Id Exp deriving (Show, Eq)
data FunDecl    = Func RetType Id Paren FArgs Curl [VarDecl] Stmt [Stmt] deriving (Show, Eq)
data RetType    = RType | Void deriving (Show, Eq)
data Type       = Int|Bool| Tup | EmptyList |Id deriving (Show, Eq)
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
data Error      = Exep String deriving (Show, Eq)

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
                | TokError Error
                | TokBool Bool    
                | TokText String  
                | TokBrack Brack 
                | TokComma 

data Tree a      = Branch Token [Tree a] | Error [a] String deriving (Show)                
                
instance Show (Main.Token) where
    show (TokSPL )                      = "SPL"
    show (TokDecl )                     = "Decl"
    show (TokVarDecl)                   = "VarDecl"
    show (TokFunDecl)                   = "Fundecl" 
    show (TokRetType x)                 = show x
    show (TokType EmptyList)            = "[]"
    show (TokType x)                    = show x
    show (TokFArgs )                    = ""
    show (TokStmt Return)               = "return "
    show (TokStmt IfState)              = "if"
    show (TokStmt ElseState)            = "else"
    show (TokStmt WhileState)           = "while"
    show (TokStmt MultState)            = "{"
    show (TokStmt _)                    = ""
    show (TokExp)                       = ""
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
    show (TokError x)                   = show x
    show (TokParen ParenceO)            = "("
    show (TokParen ParenceC)            = ")"
    show (TokBrack BrackO)              = "["
    show (TokBrack BrackC)              = "] "
    show (TokBool x)                    = show x   
    show (TokText x)                    = x       
    show (TokComma)                     = ","   
     
tupleToTree :: (Tree a, String) -> Tree a
tupleToTree (a,_) = a

noError :: (Tree a, String) -> Data.Bool.Bool
noError (Branch (TokError (Exep _)) _,_)       = Data.Bool.False      
noError _                                      = Data.Bool.True

dropChar :: (Tree a, String) -> Prelude.Int -> (Tree a, String)
dropChar (x,xs) a
        | length (ds xs) < a    = (Branch (TokError (Exep "cannot remove this many chars at:")) [], [])
        | otherwise             = (x,drop a (ds xs))
                
insertTup :: (Tree a, String) -> (Tree a, String) -> (Tree a, String)
insertTup (Branch _ _, _) (Branch (TokError (Exep y)) b, s2)     = (Branch (TokError (Exep y)) b, s2)
insertTup (Branch x [], _) (Branch y a, s2)                      = (Branch x [Branch y a], s2)
insertTup (Branch x a, _) (Branch y [], s2)                      = (Branch x (a ++ [Branch y []]), s2)
insertTup (Branch x a, _) (Branch y b, s2)                       = (Branch x (a ++ [Branch y b]), s2)

insertNode :: Tree a -> Tree a -> Tree a
insertNode (Branch x []) (Branch y a)   = Branch x [Branch y a]
insertNode (Branch x a) (Branch y [])   = Branch x (a ++ [Branch y []])
insertNode (Branch x a) (Branch y b)    = Branch x (a ++ [Branch y b])

isSPL :: String -> Tree a
isSPL [] = Branch (TokError (Exep "empty input file")) []
isSPL xs = getDecl (isDecl (Branch TokSPL [], xs))
        where
        getDecl l@(t,s)
                | noError l &&  not (null  (ds s))      = insertNode t (isSPL (ds s))    
                | otherwise                             = tupleToTree (isDecl (Branch TokSPL [], xs))
                
isDecl :: (Tree a, String) -> (Tree a, String)
isDecl (_, []) = (Branch (TokError (Exep "No decleration found")) [], [])
isDecl l@(a,xs)
        | noError (isVarDecl (a,xs))    = insertTup l (isVarDecl (Branch TokDecl [], ds xs))  
        | noError (isFunDecl (a,xs))    = insertTup l (isFunDecl (Branch TokDecl [], ds xs))
        | otherwise                     = isVarDecl (a, xs)

isVarDecl:: (Tree a, String) -> (Tree a, String)
isVarDecl (_, [])       = (Branch (TokError (Exep "empty input Variable")) [], [])
isVarDecl l@(_, xs)      = insertTup l (hasId $ fs (isType (Branch TokVarDecl [], ds xs)))    
        where
        hasId t
                | noError t = hasEqSign $ fs (isId t)
                | otherwise = t 
                where
                hasEqSign l@(_, s)
                        | noError l && not (null s) && head s == '=' = insertTup l (hasExp $ fs (isExp $ fs (dropChar (Branch (TokText "=") [],s) 1)))
                        | noError l = (Branch (TokError (Exep (" = sign missing in varDecl at: "++ s))) [], s)
                        | otherwise = l
                        where
                        hasExp l@(_, s)
                                | noError l && not (null s) && head s == ';' = insertTup l (dropChar (Branch (TokText ";") [],s) 1)
                                | noError l = (Branch (TokError (Exep (" ; sign missing in varDecl at: "++ s))) [], s)
                                | otherwise = l

isFunDecl:: (Tree a, String) -> (Tree a, String)
isFunDecl (_, []) = (Branch (TokError (Exep "functiondecl missing")) [], [])
isFunDecl l@(_,s) = insertTup l (hasId $ fs (isRetType (Branch TokFunDecl [], ds s)))
        where 
        hasId t
                | noError t = hasParen $ fs (isId $ fs t)
                | otherwise = t
                where
                hasParen l@(_,s)
                        | not (null s) && head s == '(' = insertTup l (hasFunargs $ fs (dropChar (Branch (TokParen ParenceO) [], s) 1))
                        | otherwise = (Branch (TokError (Exep (" ( sign missing in funDecl at: "++ s))) [], s)        
                        where                
                        hasFunargs l@ (_,s)                              
                                | noError (isFunarg $ fs l)                     = hasFunargs $ fs (isFunarg $ fs l)
                                | not (null (ds s)) && head (ds s) == ')'       = insertTup l (hasMult (fs (Branch (TokParen ParenceC) [], s)))
                                where
                                hasMult l@(t,s)
                                        | head (ds(tail (ds s))) == '{' = insertTup l (hasVardecl (fs(Branch (TokCurl CurlO) [],tail(ds(tail (ds s))))))
                                        | otherwise                     = (Branch (TokError (Exep ("wrong funarg or missing ) Or { token, at:" ++s))) [], s)
                                       where                           
                                       hasVardecl l@(t,s)
                                                | noError(isVarDecl (fs l))     = hasVardecl $ fs (isVarDecl (fs l))
                                                | otherwise                     = hasStatement $ fs (isStatement (fs l))
                                                where                                            
                                                hasStatement l@(t,s)
                                                    | noError l && noError (isStatement (fs l)) = hasStatement (isStatement (fs l))
                                                    | noError l && head (ds s) == '}'           = insertTup l (dropChar (Branch (TokCurl CurlC) [], s) 1)
                                                    | otherwise                                 = l

isFunarg :: (Tree a, String) -> (Tree a, String)
isFunarg (_, [])        = (Branch (TokError (Exep "function argument missing")) [], [])
isFunarg l@(_, xs)       = insertTup l (hasType (isType (Branch TokFArgs [], ds xs)))
            where
            hasType l@(_, xs)
                | noError l     = hasId $ fs (isId (fs l))
                | otherwise     = (Branch (TokError (Exep ("no match in funarg at:" ++ xs))) [], [])
                where 
                hasId (_, []) = (Branch (TokError (Exep "no id found in funarg")) [], [])
                hasId l@(_, xs) 
                        | noError l && not (null (ds xs)) && head (ds xs) == ','        = insertTup l (isFunarg $ fs (dropChar (Branch (TokText ",") [], ds xs) 1))
                        | otherwise                                                     = l                        
                        
isStatement :: (Tree a, String) -> (Tree a, String)
isStatement (_, []) = (Branch (TokError (Exep "empty input in a statement")) [], [])
isStatement l@(x,xs)
        | head (ds xs) == '{'                                                   = insertTup l (multiStatement $ fs (Branch (TokStmt MultState) [Branch (TokCurl CurlO) []], tail (ds xs)))
        | take 2 (ds xs) == "if" && head (ds (drop 2 (ds xs))) == '('           = insertTup l (hasExp $ fs (isExp (Branch (TokStmt IfState) [Branch (TokParen ParenceO) []], drop 1 (ds (drop 2 (ds xs)))))) 
        | take 5 (ds xs) == "while" && head (ds (drop 5 (ds xs))) == '('        = insertTup l (hasExp $ fs (isExp (Branch (TokStmt WhileState) [Branch (TokParen ParenceO) []], tail (ds (drop 5 (ds xs))))))        
        | noError(isFunCall (x,xs))                                             = insertTup l (hasFuncall (isFunCall (Branch (TokStmt FuncSate) [], ds xs)))
        | take 6 (ds xs) == "return"                                            = insertTup l (maybeExp $ fs (Branch (TokStmt Return) [], ds (drop 6 (ds xs))))
        | noError(isId (fs l))                                                  = insertTup l (hasId $ fs (isId (Branch (TokStmt EqState) [], ds xs)))        
        | otherwise                                                             = (Branch (TokError (Exep ("no matching statement at:" ++ ds xs))) [], xs)
        where
         multiStatement (_, []) = (Branch (TokError (Exep "empty input in multi statement")) [], [])
         multiStatement l@(x,xs)
                | noError (isStatement (x, ds xs))              = multiStatement (isStatement (x, ds xs))
                | not (null (ds xs)) && head (ds xs) == '}'     = insertTup l (Branch (TokCurl CurlC) [], tail (ds xs))
                | otherwise                                     = isStatement (x, ds xs)
         hasExp (_, []) = (Branch (TokError (Exep "error in exp for statement")) [], [])
         hasExp l@(_, xs) 
                | noError l
                && not (null (ds xs))
                && head (ds xs) == ')'  = insertTup l (hasStatement $ fs (Branch (TokParen ParenceC) [], ds(tail (ds xs))))
                | noError l             =  (Branch (TokError (Exep ("missing ) at:"++ xs))) [], [])
                | otherwise             =  l
                where
                hasStatement (_, []) = (Branch (TokError (Exep "empty input for state for statement")) [], [])
                hasStatement (x, xs) 
                        | noError(isStatement (x, ds xs))       = hasElse $ fs (isStatement (x, ds xs))
                        | otherwise                             = isStatement (x, ds xs) 
                        where
                        hasElse (x, [])  = (x, [])                     
                        hasElse l@(x,xs)
                                | take 4 (ds xs) == "else"      = insertTup l (hasStatement (Branch (TokStmt ElseState) [], drop 4 (ds xs)))
                                | otherwise                     = (x,xs)
         hasId t
                | noError t = hasField $fs (isField (fs t))
                | otherwise = t
                where
                hasField l@(_, xs)
                        | noError l && not (null (ds xs)) && head  (ds xs) == '='       = insertTup l (hasIdExp $ fs (isExp (dropChar (Branch (TokText "=")[],xs) 1)))
                        | noError l                                                     = (Branch (TokError (Exep ("= missing in statement at:"++xs))) [], [])
                        | otherwise                                                     = l
                        where
                        hasIdExp l@(_,xs)
                                | noError l && not (null (ds xs)) && head  (ds xs) == ';'       = insertTup l (dropChar (Branch (TokText ";")[],xs) 1)
                                | noError l                                                     = (Branch (TokError (Exep ("; missing in statement at:"++xs))) [], [])
                                | otherwise                                                     = l
         hasFuncall l@(_,xs)
                | noError l && not (null (ds xs)) && head (ds xs) == ';'        = insertTup l (dropChar (Branch (TokText ";") [], ds xs) 1) 
                | otherwise                                                     = l
         maybeExp l@(_, xs)
                | noError (isExp (fs l))                        = maybeExp (isExp (fs l))
                | not (null (ds xs)) && head (ds xs) == ';'     = insertTup l (dropChar (Branch (TokText ";") [], ds xs) 1)
                | otherwise                                     = (Branch (TokError (Exep ("; missing from return at:"++xs))) [], []) 

isRetType :: (Tree a, String) -> (Tree a, String)
isRetType (_, [])        = (Branch (TokError (Exep "data missing for Return type")) [], [])
isRetType l@(x,xs)
        | take 4 (ds xs) == "Void"      = insertTup l (Branch (TokRetType Void) [], drop 4 (ds xs))
        | noError(isType (x, ds xs))    = insertTup l $ fs (isType (Branch (TokRetType RType) [], ds xs))        
        | otherwise                     = isType (x, ds xs)

isType :: (Tree a, String) -> (Tree a, String)
isType (_, []) = (Branch (TokError (Exep "empty input type")) [], [])
isType l@(x,xs)      
        | head (ds xs) == '['           = insertTup l (hasTypeList $ fs (isType (Branch (TokBrack BrackO) [], ds (tail (ds xs)))))    
        | head (ds xs) == '('           = insertTup l (hasTypeTup $ fs (isType (dropChar (Branch (TokParen ParenceO) [], ds xs) 1)) )
        | take 3 (ds xs) == "Int"       = insertTup l (Branch (TokType Int) [], ds (drop 3 (ds xs)))       
        | take 4 (ds xs) == "Bool"      = insertTup l (Branch (TokType Bool) [], ds (drop 4 (ds xs)))
        | noError(isId (x, ds xs))      = insertTup l (isId (Branch (TokType Id) [], ds xs))
        | otherwise                     = (Branch (TokError (Exep ("no input type found at: " ++ xs))) [], [])
        where
        hasTypeList l@(_, xs)
               | noError l && not (null (ds xs)) && head (ds xs) == ']' = insertTup l (dropChar (Branch (TokBrack BrackC) [], ds xs) 1)
               | noError l                                              = (Branch (TokError (Exep ("] missing in for type at:"++xs))) [],xs)
               | otherwise                                              = l
        hasTypeTup l@(_, xs)
                | noError l && not (null (ds xs)) && head (ds xs) == ','        = insertTup l (hasSecondType $ fs (isType $ fs (dropChar (Branch (TokText ",") [], xs ) 1)))
                | noError l                                                     = (Branch (TokError (Exep (",-token missing in for typeTupel at:"++xs))) [],xs)
                | otherwise                                                     = l
                where 
                hasSecondType l@(_, xs)
                        | noError l && not (null (ds xs)) && head (ds xs) == ')'        = insertTup l (dropChar (Branch (TokParen ParenceC) [], xs ) 1)
                        | noError l                                                     = (Branch (TokError (Exep (")-token missing in for typeTupel at (max two types in each tuple):"++xs))) [],xs)
                        | otherwise                                                     = l

isTrue :: (Tree a, String) -> (Tree a, String)
isTrue (_, []) = (Branch (TokError (Exep "empty input true")) [], [])
isTrue l@(_, xs)
        | take 4 (ds xs) == "True"      = insertTup l (Branch (TokBool True) [], drop 4 (ds xs))
        | otherwise                     = (Branch (TokError (Exep "no match on true")) [], [])

isFalse :: (Tree a, String) -> (Tree a, String)
isFalse (_, []) = (Branch (TokError (Exep "empty input False")) [], [])
isFalse l@(_, xs)
        | take 5 (ds xs) == "False"     = insertTup l (Branch (TokBool False) [], drop 5 (ds xs))
        | otherwise                     = (Branch (TokError (Exep "no match on false")) [], [])

isExp :: (Tree a, String) -> (Tree a, String)
isExp (_, []) = (Branch (TokError (Exep "expression missing")) [], [])
isExp l@(x,xs) 
        | noError (isOp1 (x, ds xs))                            = insertTup l (hasExp $ fs (isExp $ fs (isOp1 (Branch TokExp [], ds xs))))
        | noError (isInt (x, ds xs))                            = insertTup l (hasExp $ fs (isInt (Branch TokExp [], ds xs)))
        | noError (isTrue (x, ds xs))                           = insertTup l (hasExp $ fs (isTrue (Branch TokExp [], ds xs))) 
        | noError (isFalse (x, ds xs))                          = insertTup l (hasExp $ fs (isFalse (Branch TokExp [], ds xs))) 
        | not (null (ds xs)) &&  head (ds xs) == '('            = insertTup l (hasTuple $ fs (isExp $ fs (dropChar (Branch (TokParen ParenceO) [], ds xs) 1)))
        | take 2 (ds xs) == "[]"                                = insertTup l (hasExp $ fs (Branch TokExp [Branch (TokType EmptyList) []], drop 2 (ds xs))) 
        | noError (isFunCall (Branch TokExp [], ds xs))         = insertTup l (hasExp $ fs (isFunCall (Branch TokExp [], ds xs))) 
        | noError (isId (x, ds xs))                             = insertTup l (hasExp $ fs (isField $ fs (isId (Branch TokExp [], ds xs)) ))       
        | otherwise                                             = (Branch (TokError (Exep ("no matching exp found at:" ++ xs))) [], xs)
        where
        hasTuple l@(_, xs)
                | noError l && not (null (ds xs)) && head (ds xs) == ')'        = insertTup l (hasExp (Branch (TokParen ParenceC) [], drop 1 (ds xs)))
                | noError l && not (null (ds xs)) && head (ds xs) == ','        = insertTup l (hasTuple $ fs (isExp $ fs(Branch (TokText ",") [], drop 1 (ds xs))))
                | noError l                                                     = (Branch (TokError (Exep (")-token missing at:"++xs))) [], xs)
                | otherwise                                                     = l       
        hasExp l@(_, xs)
                | noError l && noError (isOp2 $ fs l)   = fixOrder (isExp $ fs (isOp2 $ fs l)) --fixOrder
                | noError l                             = l
                | otherwise                             = (Branch (TokError (Exep ("no expression found at:" ++ xs))) [], xs)

fixOrder :: (Tree a, String) -> (Tree a, String)
fixOrder (Branch x y, s) = (Branch x (isMultEx y), s)
        where
        isMultEx :: [Tree a] -> [Tree a]
        isMultEx t 
                | length t == 3           = init t ++ [compareOperators (getOp (t !! 1)) (t !! 2)]
                | length t == 4           = init t ++ [compareOperators (getOp (t !! 2)) (t !! 3)] -- id has field
                | otherwise               = [Branch (TokError (Exep ("no operator" ++ show (getOp (t !! 2))))) []]
                where
                compareOperators :: Op2 -> Tree a -> Tree a
                compareOperators op (Branch x y)
                        | length y == 3 && isBigger (getOp (y !! 1)) op = Branch x ([Branch (TokParen ParenceO) []] ++ (init y) ++ [(compareOperators (getOp (y !! 1)) (y !! 2))] ++ [Branch (TokParen ParenceC) []])
                        | length y == 3                                 = Branch x (init y ++ [(compareOperators (getOp (y !! 1)) (y !! 2))])
                        | length y == 4 && isBigger (getOp (y !! 2)) op = Branch x ([Branch (TokParen ParenceO) []] ++ (init y) ++ [(compareOperators (getOp (y !! 2)) (y !! 3))] ++ [Branch (TokParen ParenceC) []])
                        | length y == 4                                 = Branch x (init y ++ [(compareOperators (getOp (y !! 2)) (y !! 3))])
                        | otherwise                                     = Branch x y
                
getOp :: Tree a -> Op2
getOp (Branch (TokOp2 y) _)     = y
getOp (Branch _ _)              =  error "geen operator in exp op2 exp"
                        
isBigger :: Op2 -> Op2 -> Bool
isBigger Times Plus     = True
isBigger Times Minus    = True
isBigger Div   Plus     = True
isBigger Div   Minus    = True
isBigger Mod   Plus     = True
isBigger Mod   Minus    = True
isBigger _     _        = False                                  
 
isField :: (Tree a, String) -> (Tree a, String)
isField l@(_, []) = insertTup l (Branch (TokField EField) [], [])
isField l@(_, xs)
        | take 3 (ds xs) == ".hd"       = insertTup l(isField (Branch (TokField (FieldT FieldHD))  [], drop 3 (ds xs)))
        | take 3 (ds xs) == ".tl"       = insertTup l(isField (Branch (TokField (FieldT FieldTL))  [], drop 3 (ds xs)))
        | take 4 (ds xs) == ".snd"      = insertTup l(isField (Branch (TokField (FieldT FieldSND)) [], drop 4 (ds xs)))
        | take 4 (ds xs) == ".fst"      = insertTup l(isField (Branch (TokField (FieldT FieldFST)) [], drop 4 (ds xs)))
        | otherwise                     = l
        
isFunCall :: (Tree a, String) -> (Tree a, String)
isFunCall (_, [])        = (Branch (TokError (Exep "FunCall missing")) [], [])  
isFunCall l@(_, xs)      = insertTup l (hasId $ fs (isId (Branch TokFunCall [], ds xs)))
        where
        hasId l@(_,xs)
                | noError l && not (null (ds xs)) && head (ds xs) == '('        = insertTup l (hasActArgs $ fs (dropChar (Branch (TokParen ParenceO) [], ds xs) 1))
                | noError l                                                     = (Branch (TokError (Exep ("(-symbol missing for funCall at:"++xs))) [], [])
                | otherwise                                                     = l
                where
                hasActArgs l@(_,xs)
                        | noError (isActArgs $ fs l)                    = hasActArgs $ fs (isActArgs $ fs l)
                        | not (null (ds xs)) && head (ds xs) == ')'     = insertTup l (dropChar (Branch (TokParen ParenceC) [], xs) 1)
                        | otherwise                                     = isActArgs $ fs l
  
isActArgs :: (Tree a, String) -> (Tree a, String)
isActArgs (_, [])       = (Branch (TokError (Exep "ActArgs missing")) [], [])  
isActArgs l@(_, xs)      = insertTup l (hasExp $ fs (isExp (Branch TokActArgs [], ds xs)))
        where 
        hasExp l@(_, xs)
                | noError l && not (null (ds xs)) && head (ds xs) == ','        = insertTup l (hasExp $ fs (isExp (Branch TokActArgs [Branch TokComma []], ds(drop 1 (ds xs)))))
                | otherwise                                                     = l
        
isOp2 :: (Tree a, String) -> (Tree a, String)
isOp2 (_, [])                   = (Branch (TokError (Exep "op2 missing")) [], [])
isOp2 l@(_, xs)      
      | take 2 (ds xs) == "=="  = insertTup l (Branch (TokOp2 Comp) [], drop 2 (ds xs)) 
      | take 2 (ds xs) == "!="  = insertTup l (Branch (TokOp2 NEQ) [], drop 2 (ds xs))
      | take 2 (ds xs) == "&&"  = insertTup l (Branch (TokOp2 AND) [], drop 2 (ds xs))
      | take 2 (ds xs) == "||"  = insertTup l (Branch (TokOp2 OR) [], drop 2 (ds xs))
      | take 2 (ds xs) == "<="  = insertTup l (Branch (TokOp2 LEQ) [], drop 2 (ds xs))
      | take 2 (ds xs) == ">="  = insertTup l (Branch (TokOp2 GEQ) [], drop 2 (ds xs))
      | head (ds xs) == '+'     = insertTup l (Branch (TokOp2 Plus) [], tail (ds xs))
      | head (ds xs) == '-'     = insertTup l (Branch (TokOp2 Minus) [], tail (ds xs))  
      | head (ds xs) == '*'     = insertTup l (Branch (TokOp2 Times) [], tail (ds xs))
      | head (ds xs) == '/'     = insertTup l (Branch (TokOp2 Div) [], tail (ds xs))
      | head (ds xs) == '%'     = insertTup l (Branch (TokOp2 Mod) [], tail (ds xs))
      | head (ds xs) == '<'     = insertTup l (Branch (TokOp2 Lt) [], tail (ds xs))
      | head (ds xs) == '>'     = insertTup l (Branch (TokOp2 Gt) [], tail (ds xs))
      | head (ds xs) == ':'     = insertTup l (Branch (TokOp2 Colon) [], tail (ds xs))
isOp2 _                         = (Branch (TokError (Exep "geen match op2")) [], [])

isOp1 :: (Tree a, String) -> (Tree a, String)
isOp1 (_, [])         = (Branch (TokError (Exep "op1 missing")) [], [])
isOp1 l@(_,xs)
      | head (ds xs) == '!'     = insertTup l (Branch (TokOp1 ExclMark) [], tail (ds xs))
      | head (ds xs) == '-'     = insertTup l (Branch (TokOp1 PreMinus) [], tail (ds xs))
      | otherwise               = (Branch (TokError (Exep "incorrect op1")) [], [])

isInt :: (Tree a, String) -> (Tree a, String)
isInt (_, []) = (Branch (TokError (Exep "Integer missing")) [], [])
isInt (x, a : b) 
        | a `elem` "1234567890"         = getInt (x, toInt (a : b) 0)      
        | otherwise                     = isInt (x, [])
        where                   
                getInt (x,(xs,c)) = insertTup (x, xs) (Branch (TokInt c) [], xs)

toInt :: String -> Int -> (String, Int)
toInt [] _ = ("error", 0)
toInt l@(a:b) c
        | elem a "1234567890" && not (null b)   = toInt b ((c*10) + digitToInt a)
        | elem a "1234567890" && null b         = ([], (c * 10) + digitToInt a)
        | otherwise                             = (l, c) 

isId :: (Tree a, String) -> (Tree a, String)
isId (_, []) = (Branch (TokError (Exep "empty input id")) [], [])
isId t@(_, l@(x:xs)) | isLetter x         = let (first, rest) = readIdentfier l
                                              in insertTup t (Branch (TokId first) [], ds rest)
                    | otherwise          = (Branch (TokError (Exep ("no id Found at: " ++ xs))) [], [])
                                                
readIdentfier [] = ([], [])
readIdentfier l@(x:xs) = let (first, rest) = readLettersOrDigitsOrUnderscores xs
                             identifier = (x:first)
                             in (identifier, rest)
 
readLettersOrDigitsOrUnderscores :: String -> (String, String)
readLettersOrDigitsOrUnderscores xs = span isLetterOrDigitOrUnderscore xs   
 
isLetterOrDigitOrUnderscore ch = isLetter ch || isDigit ch || ch == '_'                                                                                   

stripComments :: String -> String
stripComments []                = []
stripComments ('/':'/':xs)      = inComment xs 
stripComments ('/':'*':xs)      = inMultiComment xs
stripComments (x:xs)            = x : stripComments xs

inComment :: String -> String
inComment ('\n':xs)     = stripComments xs
inComment (_:xs)        = inComment xs
inComment []            = []

inMultiComment :: String -> String
inMultiComment ('*':'/':xs)     = stripComments xs
inMultiComment (_:xs)           = inMultiComment xs
inMultiComment []               = []       

replaceStr :: String -> String -> String -> String
replaceStr [] _ _       = []
replaceStr str old new  = loop str
  where
    loop [] = []
    loop str =
      let (prefix, rest) = splitAt n str
      in
        if old == prefix              
        then new ++ loop rest       
        else head str : loop (tail str)
    n = length old     

openFile :: String -> IO ()
openFile fileName = do
        spl <- readSPL fileName
        printTreeToFile spl
 
readSPL :: String -> IO (Tree a)
readSPL fileName = do
                text <- readFile fileName              
                return   (isSPL (stripLayout (stripComments text)))
                where
                stripLayout xs = replaceStr (replaceStr xs "\n" " ") "\t" " "              

fs :: (a, String) -> (a, String)
fs (x, []) = (x, [])
fs (x, xs) = (x, ds xs)

ds :: String -> String
ds []           = []
ds l@(a:b)      =  case a <= ' ' of
                           Data.Bool.True  -> ds b
                           Data.Bool.False -> l
      
prettyPrintTree :: Tree a -> String
prettyPrintTree (Branch x []) = show x
prettyPrintTree (Branch _ xs) = stripMultNew (printTree' xs 0)
        where
        printTree' [] i = []
        printTree' ((Branch a []):l@(Branch t@(TokCurl CurlC) []):x)  i  = show a ++ newline (i-1) ++ show t ++ newline (i-1) ++ printTree' x (i-1)
        printTree' ((Branch a b):l@(Branch t@(TokCurl CurlC) []):x)   i  = show a ++ printTree' b i ++ newline (i-1) ++ show t ++ newline (i-1) ++ printTree' x (i-1)
        printTree' ((Branch a []):l@(Branch t@(TokCurl CurlC) y):x)  i   = show a ++ newline (i-1) ++ show t ++ newline (i-1) ++ printTree' y (i-1) ++ printTree' x (i-1)
        printTree' ((Branch a b):l@(Branch t@(TokCurl CurlC) y):x)   i   = show a ++ printTree' b i ++ newline (i-1) ++ show t ++ newline (i-1) ++ printTree' y (i-1) ++ printTree' x (i-1)
        printTree' ((Branch a []):l@(Branch (TokText ";") []):x)    i    = show a ++ ";"++newline i ++  printTree' x i
        printTree' ((Branch a b):l@(Branch (TokText ";") []):x)     i    = show a ++ printTree' b i ++ ";"++newline i++  printTree' x i
        printTree' ((Branch t@(TokOp2 _) _):x)                      i    = show t  ++ " " ++ printTree' x i
        printTree' ((Branch t@(TokOp1 _) _):x)                      i    = show t  ++ printTree' x i
        printTree' ((Branch t1@(TokId a) []):Branch t2@(TokField (FieldT _)) _:x) i  = show t1 ++ show t2 ++ " "++ printTree' x i
        printTree' ((Branch TokFunDecl x):y)                i            = "\n" ++ newline i ++ printTree' x i ++ printTree' y i
        printTree' ((Branch m@(TokRetType Void) x):y)       i            = show m ++ " "++ printTree' x i ++ printTree' y i
        printTree' ((Branch m@(TokStmt Return) x):y)        i            = show m ++ printTree' x i ++ printTree' y i
        printTree' ((Branch m@(TokStmt IfState) x):y)       i            = show m ++ " " ++ printTree' x i ++ printTree' y i
        printTree' ((Branch m@(TokStmt ElseState) x):y)     i            = show m ++ " " ++ printTree' x i ++ printTree' y i
        printTree' ((Branch m@(TokStmt WhileState) x):y)    i            = show m ++ " " ++ printTree' x i ++ printTree' y i
        printTree' ((Branch t@(TokParen b) a):x)            i            = show t ++ " " ++ printTree' a i ++ printTree' x i
        printTree' ((Branch t@(TokText "=") a):x)           i            = show t ++ " " ++ printTree' a i ++ printTree' x i  
        printTree' ((Branch TokVarDecl a):x)                i            = printTree' a i ++ printTree' x i
        printTree' ((Branch t@(TokBrack _) a):x)            i            = show t ++ printTree' a i ++ printTree' x i
        printTree' ((Branch t@(TokBrack _) a):[])           i            = show t ++ printTree' a i   
        printTree' ((Branch t@(TokType Id) a):[])           i            = printTree' a i
        printTree' ((Branch t@(TokCurl CurlO) a):x)         i            = newline i ++  show t ++ newline (i+1) ++ printTree' a (i+1) ++ printTree' x (i+1)      
        printTree' ((Branch t@(TokCurl _) []):[])           i            = show t ++ newline (i)
        printTree' ((Branch t@(TokCurl CurlC) a):x)         i            = show t ++ newline (i-1) ++ printTree' a (i-1) ++ printTree' x (i-1)
        printTree' ((Branch t@(TokText ",") a):x)           i            = show t ++ " " ++ printTree' a i ++ printTree' x i
        printTree' ((Branch (TokField EField) _):x)         i            = printTree' x i        
        printTree' ((Branch x []): y)                       i            = show x ++ " " ++ printTree' y i
        printTree' ((Branch (TokRetType RType) a): b)       i            = printTree' a i ++ printTree' b i
        printTree' ((Branch x a): y)                        i            = printTree' a i ++ printTree' y i

newline :: Int -> String
newline 0 = "\n"
newline a = "\n" ++ tabs a

tabs :: Int -> String
tabs 0 = "";
tabs a = "\t" ++ tabs (a-1)

stripMultNew :: String -> String
stripMultNew []                 = []
stripMultNew (' ':';':a)        = stripMultNew (';':a)
stripMultNew (' ':']':a)        = stripMultNew (']':a)
stripMultNew (' ':')':a)        = stripMultNew (')':a)
stripMultNew ('(':' ':a)        = stripMultNew ('(':a)
stripMultNew (' ':',':a)        = stripMultNew (',':a)
stripMultNew ('\n':a)           = ['\n'] ++ stripMultNew (striplayout a 0)
       where
           striplayout ('\t': a) b = striplayout a (b+1)
           striplayout ('\n': a) _ = a
           striplayout a b = (tabs b) ++ a
stripMultNew (a:b) = [a] ++ stripMultNew b
        
printTreeToFile tree = do 
        outh <- System.IO.openFile "prettyprint.txt" WriteMode
        hPutStrLn outh (prettyPrintTree tree)   
        hClose outh   
                 
main = Main.openFile "testProgram10.spl"  -- AST is pretty printed to prettyprint.txt           