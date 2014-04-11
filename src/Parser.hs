module Parser where
import Data.Char
import Data.Tree

import Grammar

tupleToTree :: (Tree Token, String) -> Tree Token
tupleToTree (tree, _) = tree

noError :: (Tree Token, String) -> Bool
noError (Node (TokError _) _,_)       = False
noError _                               = True

dropChar :: (Tree Token, String) -> Int -> (Tree Token, String)
dropChar (tree, input) a
        | length (ds input) < a    = (Node (TokError "cannot remove this many chars at:") [], [])
        | otherwise             = (tree, drop a (ds input))

insertTup :: (Tree Token, String) -> (Tree Token, String) -> (Tree Token, String)
insertTup (Node _ _, _) (Node (TokError y) b, s2)     = (Node (TokError y) b, s2)
insertTup (Node x [], _) (Node y a, s2)                      = (Node x [Node y a], s2)
insertTup (Node x a, _) (Node y [], s2)                      = (Node x (a ++ [Node y []]), s2)
insertTup (Node x a, _) (Node y b, s2)                       = (Node x (a ++ [Node y b]), s2)

insertNode :: Tree Token -> Tree Token -> Tree Token
insertNode (Node x []) (Node y a)    = Node x [Node y a]
insertNode (Node x a)  (Node y [])   = Node x (a ++ [Node y []])
insertNode (Node x a)  (Node y b)    = Node x (a ++ [Node y b])

isSPL :: String -> Tree Token
isSPL [] = Node (TokError "empty input file") []
isSPL input = tupleToTree (getDecl (isDecl (Node TokSPL [], input)))
        where
        getDecl l@(t,s)
                | noError l &&  not (null(ds s)) =  (getDecl (isDecl $ (fs l)))
                | noError l                     = l                -- klaar
                | otherwise                     = l

isDecl :: (Tree Token, String) -> (Tree Token, String)
isDecl (_, []) = (Node (TokError "No decleration found") [], [])
isDecl l@(tree, input)
        | noError (isVarDecl l)     = insertTup l (isVarDecl (Node TokDecl [], ds input))
        | noError (isFunDecl l)     = insertTup l (isFunDecl (Node TokDecl [], ds input))
        | otherwise                 = isFunDecl l

isVarDecl:: (Tree Token, String) -> (Tree Token, String)
isVarDecl (_, [])       = (Node (TokError "empty input Variable") [], [])
isVarDecl l@(_, input)      = insertTup l (hasId $ fs (isType (Node TokVarDecl [], ds input)))
        where
        hasId t
                | noError t = hasEqSign $ fs (isId t)
                | otherwise = t
                where
                hasEqSign l@(_, s)
                        | noError l && not (null s) && head s == '=' = insertTup l (hasExp $ fs (isExp $ fs (dropChar (Node (TokText "=") [],s) 1)))
                        | noError l = (Node (TokError (" = sign missing in varDecl at: "++ s)) [], s)
                        | otherwise = l
                        where
                        hasExp l@(_, s)
                                | noError l && not (null s) && head s == ';' = insertTup l (dropChar (Node (TokText ";") [],s) 1)
                                | noError l = (Node (TokError (" ; sign missing in varDecl at: "++ s)) [], s)
                                | otherwise = l

isFunDecl:: (Tree Token, String) -> (Tree Token, String)
isFunDecl (_, []) = (Node (TokError "functiondecl missing") [], [])
isFunDecl l@(_,input) = insertTup l (hasId $ fs (isRetType (Node TokFunDecl [], ds input)))
        where
        hasId t
                | noError t = hasParen $ fs (isId $ fs t)
                | otherwise = t
                where
                hasParen l@(y,input)
                        | not (null input) && head input == '(' = (hasFunargs (insertTup l (Node (TokParen ParenceO) [], (drop 1 input))) )
                        | otherwise = (Node (TokError (" ( sign missing in funDecl at: "++ input)) [], input)
                        where
                        hasFunargs l@ (_,input)
                                | noError (isFunarg $ fs l)                     = hasFunargs $ fs (isFunarg $ fs l)
                                | not (null (ds input)) && head (ds input) == ')'       = insertTup l (hasMult (fs (Node (TokParen ParenceC) [], input)))
                                where
                                hasMult l@(t,input)
                                        | head (ds(tail (ds input))) == '{' = insertTup l (hasVardecl (fs(Node (TokCurl CurlO) [],tail(ds(tail (ds input))))))
                                        | otherwise                     = (Node (TokError ("wrong funarg or missing ) Or { token, at:" ++ input)) [], input)
                                       where
                                       hasVardecl l
                                                | noError(isVarDecl (fs l))     = hasVardecl $ fs (isVarDecl (fs l))
                                                | otherwise                     = hasStatement $ fs (isStatement (fs l))
                                                where
                                                hasStatement l@(_,input)
                                                    | noError l && noError (isStatement (fs l)) = hasStatement (isStatement (fs l))
                                                    | noError l && head (ds input) == '}'       = insertTup l (dropChar (Node (TokCurl CurlC) [], input) 1)
                                                    | otherwise                                 = l

isFunarg :: (Tree Token, String) -> (Tree Token, String)
isFunarg (_, [])        = (Node (TokError "function argument missing") [], [])
isFunarg l@(_, input)       = insertTup l (hasType (isType (Node TokFArgs [], ds input)))
            where
            hasType l@(_, input)
                | noError l     = hasId $ fs (isId (fs l))
                | otherwise     = (Node (TokError ("no match in funarg at:" ++ input)) [], [])
                where
                hasId (_, []) = (Node (TokError "no id found in funarg") [], [])
                hasId l@(_, input)
                        | noError l && not (null (ds input)) && head (ds input) == ','  = insertTup l (isFunarg $ fs (dropChar (Node (TokText ",") [], ds input) 1))
                        | otherwise                                                     = l

isStatement :: (Tree Token, String) -> (Tree Token, String)
isStatement (_, []) = (Node (TokError "empty input in a statement") [], [])
isStatement l@(x,xs)
        | head (ds xs) == '{'                                                   = insertTup l (multiStatement $ fs (Node (TokStmt MultState) [Node (TokCurl CurlO) []], tail (ds xs)))
        | take 2 (ds xs) == "if" && head (ds (drop 2 (ds xs))) == '('           = insertTup l (hasExp $ fs (isExp (Node (TokStmt IfState) [Node (TokParen ParenceO) []], drop 1 (ds (drop 2 (ds xs))))))
        | take 5 (ds xs) == "while" && head (ds (drop 5 (ds xs))) == '('        = insertTup l (hasExp $ fs (isExp (Node (TokStmt WhileState) [Node (TokParen ParenceO) []], tail (ds (drop 5 (ds xs))))))
        | noError(isFunCall (x,xs))                                             = insertTup l (hasFuncall (isFunCall (Node (TokStmt FuncSate) [], ds xs)))
        | take 6 (ds xs) == "return"                                            = insertTup l (maybeExp $ fs (Node (TokStmt Return) [], ds (drop 6 (ds xs))))
        | noError(isId (fs l))                                                  = insertTup l (hasId $ fs (isId (Node (TokStmt EqState) [], ds xs)))
        | otherwise                                                             = (Node (TokError ("no matching statement at:" ++ ds xs)) [], xs)
        where
         multiStatement (_, []) = (Node (TokError "empty input in multi statement") [], [])
         multiStatement l@(x,xs)
                | noError (isStatement (x, ds xs))              = multiStatement (isStatement (x, ds xs))
                | not (null (ds xs)) && head (ds xs) == '}'     = insertTup l (Node (TokCurl CurlC) [], tail (ds xs))
                | otherwise                                     = isStatement (x, ds xs)
         hasExp (_, []) = (Node (TokError "error in exp for statement") [], [])
         hasExp l@(_, xs)
                | noError l
                && not (null (ds xs))
                && head (ds xs) == ')'  = insertTup l (hasStatement $ fs (Node (TokParen ParenceC) [], ds(tail (ds xs))))
                | noError l             =  (Node (TokError ("missing ) at:"++ xs)) [], [])
                | otherwise             =  l
                where
                hasStatement (_, []) = (Node (TokError "empty input for state for statement") [], [])
                hasStatement (x, xs)
                        | noError(isStatement (x, ds xs))       = hasElse $ fs (isStatement (x, ds xs))
                        | otherwise                             = isStatement (x, ds xs)
                        where
                        hasElse (x, [])  = (x, [])
                        hasElse l@(x,xs)
                                | take 4 (ds xs) == "else"      = insertTup l (hasStatement (Node (TokStmt ElseState) [], drop 4 (ds xs)))
                                | otherwise                     = (x,xs)
         hasId t
                | noError t = hasField $fs (isField (fs t))
                | otherwise = t
                where
                hasField l@(_, xs)
                        | noError l && not (null (ds xs)) && head  (ds xs) == '='       = insertTup l (hasIdExp $ fs (isExp (dropChar (Node (TokText "=")[],xs) 1)))
                        | noError l                                                     = (Node (TokError ("= missing in statement at:"++xs)) [], [])
                        | otherwise                                                     = l
                        where
                        hasIdExp l@(_,xs)
                                | noError l && not (null (ds xs)) && head  (ds xs) == ';'       = insertTup l (dropChar (Node (TokText ";")[],xs) 1)
                                | noError l                                                     = (Node (TokError ("; missing in statement at:"++xs)) [], [])
                                | otherwise                                                     = l
         hasFuncall l@(_,xs)
                | noError l && not (null (ds xs)) && head (ds xs) == ';'        = insertTup l (dropChar (Node (TokText ";") [], ds xs) 1)
                | otherwise                                                     = l
         maybeExp l@(_, xs)
                | noError (isExp (fs l))                        = maybeExp (isExp (fs l))
                | not (null (ds xs)) && head (ds xs) == ';'     = insertTup l (dropChar (Node (TokText ";") [], ds xs) 1)
                | otherwise                                     = (Node (TokError ("; missing from return at:"++xs)) [], [])

isRetType :: (Tree Token, String) -> (Tree Token, String)
isRetType (_, [])        = (Node (TokError "data missing for Return type") [], [])
isRetType l@(x,xs)
        | take 4 (ds xs) == "Void"      = insertTup l (Node (TokRetType Void) [], drop 4 (ds xs))
        | noError(isType (x, ds xs))    = insertTup l $ fs (isType (Node (TokRetType RType) [], ds xs))
        | otherwise                     = isType (x, ds xs)

isType :: (Tree Token, String) -> (Tree Token, String)
isType (_, []) = (Node (TokError "empty input type") [], [])
isType l@(x,xs)
        | head (ds xs) == '['           = insertTup l (hasTypeList $ fs (isType (Node (TokBrack BrackO) [], ds (tail (ds xs)))))
        | head (ds xs) == '('           = insertTup l (hasTypeTup $ fs (isType (dropChar (Node (TokParen ParenceO) [], ds xs) 1)) )
        | take 3 (ds xs) == "Int"       = insertTup l (Node (TokType Int) [], ds (drop 3 (ds xs)))
        | take 4 (ds xs) == "Bool"      = insertTup l (Node (TokType Bool) [], ds (drop 4 (ds xs)))
        | noError(isId (x, ds xs))      = insertTup l (isId (Node (TokType (Id "")) [], ds xs))
        | otherwise                     = (Node (TokError ("no input type found at: " ++ xs)) [], [])
        where
        hasTypeList l@(_, xs)
               | noError l && not (null (ds xs)) && head (ds xs) == ']' = insertTup l (dropChar (Node (TokBrack BrackC) [], ds xs) 1)
               | noError l                                              = (Node (TokError ("] missing in for type at:"++xs)) [],xs)
               | otherwise                                              = l
        hasTypeTup l@(_, xs)
                | noError l && not (null (ds xs)) && head (ds xs) == ','        = insertTup l (hasSecondType $ fs (isType $ fs (dropChar (Node (TokText ",") [], xs ) 1)))
                | noError l                                                     = (Node (TokError (",-token missing in for typeTupel at:"++xs)) [],xs)
                | otherwise                                                     = l
                where
                hasSecondType l@(_, xs)
                        | noError l && not (null (ds xs)) && head (ds xs) == ')'        = insertTup l (dropChar (Node (TokParen ParenceC) [], xs ) 1)
                        | noError l                                                     = (Node (TokError (")-token missing in for typeTupel at (max two types in each tuple):"++xs)) [],xs)
                        | otherwise                                                     = l

isTrue :: (Tree Token, String) -> (Tree Token, String)
isTrue (_, []) = (Node (TokError "empty input true") [], [])
isTrue l@(_, xs)
        | take 4 (ds xs) == "True"      = insertTup l (Node (TokBool True) [], drop 4 (ds xs))
        | otherwise                     = (Node (TokError "no match on true") [], [])

isFalse :: (Tree Token, String) -> (Tree Token, String)
isFalse (_, []) = (Node (TokError "empty input False") [], [])
isFalse l@(_, xs)
        | take 5 (ds xs) == "False"     = insertTup l (Node (TokBool False) [], drop 5 (ds xs))
        | otherwise                     = (Node (TokError "no match on false") [], [])

isExp :: (Tree Token, String) -> (Tree Token, String)
isExp (_, []) = (Node (TokError "expression missing") [], [])
isExp l@(x,xs)
        | noError (isOp1 (x, ds xs))                            = insertTup l (hasExp $ fs (isExp $ fs (isOp1 (Node TokExp [], ds xs))))
        | noError (isInt (x, ds xs))                            = insertTup l (hasExp $ fs (isInt (Node TokExp [], ds xs)))
        | noError (isTrue (x, ds xs))                           = insertTup l (hasExp $ fs (isTrue (Node TokExp [], ds xs)))
        | noError (isFalse (x, ds xs))                          = insertTup l (hasExp $ fs (isFalse (Node TokExp [], ds xs)))
        | not (null (ds xs)) &&  head (ds xs) == '('            = insertTup l (hasTuple $ fs (isExp $ fs (dropChar (Node (TokParen ParenceO) [], ds xs) 1)))
        | take 2 (ds xs) == "[]"                                = insertTup l (hasExp $ fs (Node TokExp [Node (TokType EmptyList) []], drop 2 (ds xs)))
        | noError (isFunCall (Node TokExp [], ds xs))         = insertTup l (hasExp $ fs (isFunCall (Node TokExp [], ds xs)))
        | noError (isId (x, ds xs))                             = insertTup l (hasExp $ fs (isField $ fs (isId (Node TokExp [], ds xs)) ))
        | otherwise                                             = (Node (TokError ("no matching exp found at:" ++ xs)) [], xs)
        where
        hasTuple l@(_, xs)
                | noError l && not (null (ds xs)) && head (ds xs) == ')'        = insertTup l (hasExp (Node (TokParen ParenceC) [], drop 1 (ds xs)))
                | noError l && not (null (ds xs)) && head (ds xs) == ','        = insertTup l (hasTuple $ fs (isExp $ fs(Node (TokText ",") [], drop 1 (ds xs))))
                | noError l                                                     = (Node (TokError (")-token missing at:"++xs)) [], xs)
                | otherwise                                                     = l
        hasExp l@(_, xs)
                | noError l && noError (isOp2 $ fs l)   = fixOrder (isExp $ fs (isOp2 $ fs l)) --fixOrder
                | noError l                             = l
                | otherwise                             = (Node (TokError ("no expression found at:" ++ xs)) [], xs)

fixOrder :: (Tree Token, String) -> (Tree Token, String)
fixOrder (Node x y, s) = (Node x (isMultEx y), s)
        where
        isMultEx :: [Tree Token] -> [Tree Token]
        isMultEx t
                | length t == 3           = init t ++ [compareOperators (getOp (t !! 1)) (t !! 2)]
                | length t == 4           = init t ++ [compareOperators (getOp (t !! 2)) (t !! 3)] -- id has field
                | otherwise               = [Node (TokError ("no operator" ++ show (getOp (t !! 2)))) []]
                where
                compareOperators :: Op2 -> Tree Token -> Tree Token
                compareOperators op (Node x y)
                        | length y == 3 && isBigger (getOp (y !! 1)) op = Node x ([Node (TokParen ParenceO) []] ++ (init y) ++ [(compareOperators (getOp (y !! 1)) (y !! 2))] ++ [Node (TokParen ParenceC) []])
                        | length y == 3                                 = Node x (init y ++ [(compareOperators (getOp (y !! 1)) (y !! 2))])
                        | length y == 4 && isBigger (getOp (y !! 2)) op = Node x ([Node (TokParen ParenceO) []] ++ (init y) ++ [(compareOperators (getOp (y !! 2)) (y !! 3))] ++ [Node (TokParen ParenceC) []])
                        | length y == 4                                 = Node x (init y ++ [(compareOperators (getOp (y !! 2)) (y !! 3))])
                        | otherwise                                     = Node x y

getOp :: Tree Token -> Op2
getOp (Node (TokOp2 y) _)     = y
getOp (Node _ _)              =  error "geen operator in exp op2 exp"

isBigger :: Op2 -> Op2 -> Bool
isBigger Times Plus     = True
isBigger Times Minus    = True
isBigger Div   Plus     = True
isBigger Div   Minus    = True
isBigger Mod   Plus     = True
isBigger Mod   Minus    = True
isBigger _     _        = False

isField :: (Tree Token, String) -> (Tree Token, String)
isField l@(_, []) = insertTup l (Node (TokField EField) [], [])
isField l@(_, xs)
        | take 3 (ds xs) == ".hd"       = insertTup l(isField (Node (TokField (FieldT FieldHD))  [], drop 3 (ds xs)))
        | take 3 (ds xs) == ".tl"       = insertTup l(isField (Node (TokField (FieldT FieldTL))  [], drop 3 (ds xs)))
        | take 4 (ds xs) == ".snd"      = insertTup l(isField (Node (TokField (FieldT FieldSND)) [], drop 4 (ds xs)))
        | take 4 (ds xs) == ".fst"      = insertTup l(isField (Node (TokField (FieldT FieldFST)) [], drop 4 (ds xs)))
        | otherwise                     = l

isFunCall :: (Tree Token, String) -> (Tree Token, String)
isFunCall (_, [])        = (Node (TokError "FunCall missing") [], [])
isFunCall l@(_, xs)      = insertTup l (hasId $ fs (isId (Node TokFunCall [], ds xs)))
        where
        hasId l@(_,xs)
                | noError l && not (null (ds xs)) && head (ds xs) == '('        = insertTup l (hasActArgs $ fs (dropChar (Node (TokParen ParenceO) [], ds xs) 1))
                | noError l                                                     = (Node (TokError ("(-symbol missing for funCall at:"++xs)) [], [])
                | otherwise                                                     = l
                where
                hasActArgs l@(_,xs)
                        | noError (isActArgs $ fs l)                    = hasActArgs $ fs (isActArgs $ fs l)
                        | not (null (ds xs)) && head (ds xs) == ')'     = insertTup l (dropChar (Node (TokParen ParenceC) [], xs) 1)
                        | otherwise                                     = isActArgs $ fs l

isActArgs :: (Tree Token, String) -> (Tree Token, String)
isActArgs (_, [])       = (Node (TokError "ActArgs missing") [], [])
isActArgs l@(_, xs)      = insertTup l (hasExp $ fs (isExp (Node TokActArgs [], ds xs)))
        where
        hasExp l@(_, xs)
                | noError l && not (null (ds xs)) && head (ds xs) == ','        = insertTup l (hasExp $ fs (isExp (Node TokActArgs [Node TokComma []], ds(drop 1 (ds xs)))))
                | otherwise                                                     = l

isOp2 :: (Tree Token, String) -> (Tree Token, String)
isOp2 (_, [])                           = (Node (TokError "op2 missing") [], [])
isOp2 l@(_, input)
      | take 2 (ds input) == "=="       = insertTup l (Node (TokOp2 Comp)  [], drop 2 (ds input))
      | take 2 (ds input) == "!="       = insertTup l (Node (TokOp2 NEQ)   [], drop 2 (ds input))
      | take 2 (ds input) == "&&"       = insertTup l (Node (TokOp2 AND)   [], drop 2 (ds input))
      | take 2 (ds input) == "||"       = insertTup l (Node (TokOp2 OR)    [], drop 2 (ds input))
      | take 2 (ds input) == "<="       = insertTup l (Node (TokOp2 LEQ)   [], drop 2 (ds input))
      | take 2 (ds input) == ">="       = insertTup l (Node (TokOp2 GEQ)   [], drop 2 (ds input))
      | head (ds input) == '+'          = insertTup l (Node (TokOp2 Plus)  [], tail (ds input))
      | head (ds input) == '-'          = insertTup l (Node (TokOp2 Minus) [], tail (ds input))
      | head (ds input) == '*'          = insertTup l (Node (TokOp2 Times) [], tail (ds input))
      | head (ds input) == '/'          = insertTup l (Node (TokOp2 Div)   [], tail (ds input))
      | head (ds input) == '%'          = insertTup l (Node (TokOp2 Mod)   [], tail (ds input))
      | head (ds input) == '<'          = insertTup l (Node (TokOp2 Lt)    [], tail (ds input))
      | head (ds input) == '>'          = insertTup l (Node (TokOp2 Gt)    [], tail (ds input))
      | head (ds input) == ':'          = insertTup l (Node (TokOp2 Colon) [], tail (ds input))
isOp2 _                                 = (Node (TokError "geen match op2") [], [])

isOp1 :: (Tree Token, String) -> (Tree Token, String)
isOp1 (_, [])         = (Node (TokError "op1 missing") [], [])
isOp1 l@(_, input)
      | head (ds input) == '!'  = insertTup l (Node (TokOp1 ExclMark) [], tail (ds input))
      | head (ds input) == '-'  = insertTup l (Node (TokOp1 PreMinus) [], tail (ds input))
      | otherwise               = (Node (TokError "incorrect op1") [], [])

isInt :: (Tree Token, String) -> (Tree Token, String)
isInt (_, []) = (Node (TokError "Integer missing") [], [])
isInt (x, a : b)
        | a `elem` "1234567890"         = getInt (x, toInt (a : b) 0)
        | otherwise                     = isInt (x, [])
        where
                getInt (x,(xs,c)) = insertTup (x, xs) (Node (TokInt c) [], xs)

toInt :: String -> Int -> (String, Int)
toInt [] _ = ("error", 0)
toInt l@(a:b) c
        | elem a "1234567890" && not (null b)   = toInt b ((c*10) + digitToInt a)
        | elem a "1234567890" && null b         = ([], (c * 10) + digitToInt a)
        | otherwise                             = (l, c)

isId :: (Tree Token, String) -> (Tree Token, String)
isId (_, []) = (Node (TokError "empty input id") [], [])
isId t@(_, l@(x:xs)) | isLetter x         = let (first, rest) = readIdentfier l
                                              in insertTup t (Node (TokId first) [], ds rest)
                    | otherwise          = (Node (TokError ("no id Found at: " ++ xs)) [], [])

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

fs :: (a, String) -> (a, String)
fs (x, []) = (x, [])
fs (x, xs) = (x, ds xs)

ds :: String -> String
ds []           = []
ds l@(a:b)      =  if a <= ' ' then ds b else l