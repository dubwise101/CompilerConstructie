module Parser where

import Grammar

import Data.Char


tupleToTree :: (Tree a, String) -> Tree a
tupleToTree (tree, _) = tree

noError :: (Tree a, String) -> Bool
noError (Node (TokError _) _,_)         = False
noError _                               = True

dropChar :: (Tree a, String) -> Int -> (Tree a, String)
dropChar (tree, input) a
        | length (deleteSpace input) < a        = (Node (TokError "Cannot remove this many chars at:") [], [])
        | otherwise                             = (tree, drop a (deleteSpace input))

insertTup :: (Tree a, String) -> (Tree a, String) -> (Tree a, String)
insertTup (Node _ _, _) (Node (TokError y) b, s2)       = (Node (TokError y) b, s2)
insertTup (Node x [], _) (Node y a, s2)                 = (Node x [Node y a], s2)
insertTup (Node x a, _) (Node y [], s2)                 = (Node x (a ++ [Node y []]), s2)
insertTup (Node x a, _) (Node y b, s2)                  = (Node x (a ++ [Node y b]), s2)

insertNode :: Tree a -> Tree a -> Tree a
insertNode (Node x []) (Node y a)       = Node x [Node y a]
insertNode (Node x a) (Node y [])       = Node x (a ++ [Node y []])
insertNode (Node x a) (Node y b)        = Node x (a ++ [Node y b])

isSPL :: String -> Tree a
isSPL []        = Node (TokError "empty input file") []
isSPL input     = getDecl (isDecl (Node TokSPL [], input))
        where
        getDecl l@(t,s)
                | noError l &&  not (null  (deleteSpace s))     = insertNode t (isSPL (deleteSpace s))
                | otherwise                                     = tupleToTree (isDecl (Node TokSPL [], input))

isDecl :: (Tree a, String) -> (Tree a, String)
isDecl (_, []) = (Node (TokError "No declaration found") [], [])
isDecl l@(tree, input)
        | noError (isVarDecl l)     = insertTup l (isVarDecl (Node TokDecl [], deleteSpace input))
        | noError (isFunDecl l)     = insertTup l (isFunDecl (Node TokDecl [], deleteSpace input))
        | otherwise                 = isVarDecl l

isVarDecl:: (Tree a, String) -> (Tree a, String)
isVarDecl (_, [])       = (Node (TokError "Empty input variable") [], [])
isVarDecl l@(_, input)  = insertTup l (hasId $ findSpace (isType (Node TokVarDecl [], deleteSpace input)))
        where
        hasId t
                | noError t = hasEqSign $ findSpace (isId t)
                | otherwise = t
                where
                hasEqSign l@(_, s)
                        | noError l && not (null s) && head s == '='    = insertTup l (hasExp $ findSpace (isExp $ findSpace (dropChar (Node (TokText "=") [],s) 1)))
                        | noError l                                     = (Node (TokError (" = sign missing in varDecl at: "++ s)) [], s)
                        | otherwise                                     = l
                        where
                        hasExp l@(_, s)
                                | noError l && not (null s) && head s == ';'    = insertTup l (dropChar (Node (TokText ";") [],s) 1)
                                | noError l                                     = (Node (TokError (" ; sign missing in varDecl at: "++ s)) [], s)
                                | otherwise                                     = l

isFunDecl:: (Tree a, String) -> (Tree a, String)
isFunDecl (_, [])       = (Node (TokError "FunDecl missing") [], [])
isFunDecl l@(_,input)   = insertTup l (hasId $ findSpace (isRetType (Node TokFunDecl [], deleteSpace input)))
        where
        hasId t
                | noError t = hasParen $ findSpace (isId $ findSpace t)
                | otherwise = t
                where
                hasParen l@(_,input)
                        | not (null input) && head input == '(' = insertTup l (hasFunargs $ findSpace (dropChar (Node (TokParen ParenceO) [], input) 1))
                        | otherwise                             = (Node (TokError (" ( sign missing in funDecl at: "++ input)) [], input)
                        where
                        hasFunargs l@ (_,input)
                                | noError (isFunarg $ findSpace l)                                      = hasFunargs $ findSpace (isFunarg $ findSpace l)
                                | not (null (deleteSpace input)) && head (deleteSpace input) == ')'     = insertTup l (hasMult (findSpace (Node (TokParen ParenceC) [], input)))
                                where
                                hasMult l@(t,input)
                                        | head (deleteSpace(tail (deleteSpace input))) == '{'   = insertTup l (hasVardecl (findSpace(Node (TokCurl CurlO) [],tail(deleteSpace(tail (deleteSpace input))))))
                                        | otherwise                                             = (Node (TokError ("Wrong FunArg or missing ) or { sign, at:" ++ input)) [], input)
                                       where
                                       hasVardecl l
                                                | noError(isVarDecl (findSpace l))      = hasVardecl $ findSpace (isVarDecl (findSpace l))
                                                | otherwise                             = hasStatement $ findSpace (isStatement (findSpace l))
                                                where
                                                hasStatement l@(_,input)
                                                    | noError l && noError (isStatement (findSpace l))  = hasStatement (isStatement (findSpace l))
                                                    | noError l && head (deleteSpace input) == '}'      = insertTup l (dropChar (Node (TokCurl CurlC) [], input) 1)
                                                    | otherwise                                         = l

isFunarg :: (Tree a, String) -> (Tree a, String)
isFunarg (_, [])        = (Node (TokError "Function argument missing") [], [])
isFunarg l@(_, input)   = insertTup l (hasType (isType (Node TokFArgs [], deleteSpace input)))
            where
            hasType l@(_, input)
                | noError l     = hasId $ findSpace (isId (findSpace l))
                | otherwise     = (Node (TokError ("No match in FunArg at:" ++ input)) [], [])
                where
                hasId (_, []) = (Node (TokError "No id found in FunArg") [], [])
                hasId l@(_, input)
                        | noError l && not (null (deleteSpace input)) && head (deleteSpace input) == ','        = insertTup l (isFunarg $ findSpace (dropChar (Node (TokText ",") [], deleteSpace input) 1))
                        | otherwise                                                                             = l

isStatement :: (Tree a, String) -> (Tree a, String)
isStatement (_, []) = (Node (TokError "Empty input in a statement") [], [])
isStatement l@(x,xs)
        | head (deleteSpace xs) == '{'                                                                  = insertTup l (multiStatement $ findSpace (Node (TokStmt MultState) [Node (TokCurl CurlO) []], tail (deleteSpace xs)))
        | take 2 (deleteSpace xs) == "if" && head (deleteSpace (drop 2 (deleteSpace xs))) == '('        = insertTup l (hasExp $ findSpace (isExp (Node (TokStmt IfState) [Node (TokParen ParenceO) []], drop 1 (deleteSpace (drop 2 (deleteSpace xs))))))
        | take 5 (deleteSpace xs) == "while" && head (deleteSpace (drop 5 (deleteSpace xs))) == '('     = insertTup l (hasExp $ findSpace (isExp (Node (TokStmt WhileState) [Node (TokParen ParenceO) []], tail (deleteSpace (drop 5 (deleteSpace xs))))))
        | noError(isFunCall (x,xs))                                                                     = insertTup l (hasFuncall (isFunCall (Node (TokStmt FuncSate) [], deleteSpace xs)))
        | take 6 (deleteSpace xs) == "return"                                                           = insertTup l (maybeExp $ findSpace (Node (TokStmt Return) [], deleteSpace (drop 6 (deleteSpace xs))))
        | noError(isId (findSpace l))                                                                   = insertTup l (hasId $ findSpace (isId (Node (TokStmt EqState) [], deleteSpace xs)))
        | otherwise                                                                                     = (Node (TokError ("no matching statement at:" ++ deleteSpace xs)) [], xs)
        where
         multiStatement (_, []) = (Node (TokError "Empty input in multi statement") [], [])
         multiStatement l@(x,xs)
                | noError (isStatement (x, deleteSpace xs))                     = multiStatement (isStatement (x, deleteSpace xs))
                | not (null (deleteSpace xs)) && head (deleteSpace xs) == '}'   = insertTup l (Node (TokCurl CurlC) [], tail (deleteSpace xs))
                | otherwise                                                     = isStatement (x, deleteSpace xs)
         hasExp (_, []) = (Node (TokError "Error in exp for statement") [], [])
         hasExp l@(_, xs)
                | noError l
                && not (null (deleteSpace xs))
                && head (deleteSpace xs) == ')' = insertTup l (hasStatement $ findSpace (Node (TokParen ParenceC) [], deleteSpace(tail (deleteSpace xs))))
                | noError l                     =  (Node (TokError ("Missing ) at:"++ xs)) [], [])
                | otherwise                     = l
                where
                hasStatement (_, []) = (Node (TokError "Empty input for state for statement") [], [])
                hasStatement (x, xs)
                        | noError(isStatement (x, deleteSpace xs))      = hasElse $ findSpace (isStatement (x, deleteSpace xs))
                        | otherwise                                     = isStatement (x, deleteSpace xs)
                        where
                        hasElse (x, [])  = (x, [])
                        hasElse l@(x,xs)
                                | take 4 (deleteSpace xs) == "else"     = insertTup l (hasStatement (Node (TokStmt ElseState) [], drop 4 (deleteSpace xs)))
                                | otherwise                             = (x,xs)
         hasId t
                | noError t = hasField $findSpace (isField (findSpace t))
                | otherwise = t
                where
                hasField l@(_, xs)
                        | noError l && not (null (deleteSpace xs)) && head  (deleteSpace xs) == '='     = insertTup l (hasIdExp $ findSpace (isExp (dropChar (Node (TokText "=")[],xs) 1)))
                        | noError l                                                                     = (Node (TokError ("= missing in statement at:"++xs)) [], [])
                        | otherwise                                                                     = l
                        where
                        hasIdExp l@(_,xs)
                                | noError l && not (null (deleteSpace xs)) && head  (deleteSpace xs) == ';'     = insertTup l (dropChar (Node (TokText ";")[],xs) 1)
                                | noError l                                                                     = (Node (TokError ("; missing in statement at:"++xs)) [], [])
                                | otherwise                                                                     = l
         hasFuncall l@(_,xs)
                | noError l && not (null (deleteSpace xs)) && head (deleteSpace xs) == ';'      = insertTup l (dropChar (Node (TokText ";") [], deleteSpace xs) 1)
                | otherwise                                                                     = l
         maybeExp l@(_, xs)
                | noError (isExp (findSpace l))                                 = maybeExp (isExp (findSpace l))
                | not (null (deleteSpace xs)) && head (deleteSpace xs) == ';'   = insertTup l (dropChar (Node (TokText ";") [], deleteSpace xs) 1)
                | otherwise                                                     = (Node (TokError ("; missing from return at:"++xs)) [], [])

isRetType :: (Tree a, String) -> (Tree a, String)
isRetType (_, [])        = (Node (TokError "Data missing for RetType") [], [])
isRetType l@(x,xs)
        | take 4 (deleteSpace xs) == "Void"     = insertTup l (Node (TokRetType Void) [], drop 4 (deleteSpace xs))
        | noError(isType (x, deleteSpace xs))   = insertTup l $ findSpace (isType (Node (TokRetType RType) [], deleteSpace xs))
        | otherwise                             = isType (x, deleteSpace xs)

isType :: (Tree a, String) -> (Tree a, String)
isType (_, []) = (Node (TokError "Empty input Type") [], [])
isType l@(x,xs)
        | head (deleteSpace xs) == '['          = insertTup l (hasTypeList $ findSpace (isType (Node (TokBrack BrackO) [], deleteSpace (tail (deleteSpace xs)))))
        | head (deleteSpace xs) == '('          = insertTup l (hasTypeTup $ findSpace (isType (dropChar (Node (TokParen ParenceO) [], deleteSpace xs) 1)) )
        | take 3 (deleteSpace xs) == "Int"      = insertTup l (Node (TokType Int) [], deleteSpace (drop 3 (deleteSpace xs)))
        | take 4 (deleteSpace xs) == "Bool"     = insertTup l (Node (TokType Bool) [], deleteSpace (drop 4 (deleteSpace xs)))
        | noError(isId (x, deleteSpace xs))     = insertTup l (isId (Node (TokType Id) [], deleteSpace xs))
        | otherwise                             = (Node (TokError ("No input type found at: " ++ xs)) [], [])
        where
        hasTypeList l@(_, xs)
               | noError l && not (null (deleteSpace xs)) && head (deleteSpace xs) == ']'       = insertTup l (dropChar (Node (TokBrack BrackC) [], deleteSpace xs) 1)
               | noError l                                                                      = (Node (TokError ("] missing in for type at:"++xs)) [],xs)
               | otherwise                                                                      = l
        hasTypeTup l@(_, xs)
                | noError l && not (null (deleteSpace xs)) && head (deleteSpace xs) == ','      = insertTup l (hasSecondType $ findSpace (isType $ findSpace (dropChar (Node (TokText ",") [], xs ) 1)))
                | noError l                                                                     = (Node (TokError (",-token missing in for typeTupel at:"++xs)) [],xs)
                | otherwise                                                                     = l
                where
                hasSecondType l@(_, xs)
                        | noError l && not (null (deleteSpace xs)) && head (deleteSpace xs) == ')'      = insertTup l (dropChar (Node (TokParen ParenceC) [], xs ) 1)
                        | noError l                                                                     = (Node (TokError (")-token missing in for typeTupel at (max two types in each tuple):"++xs)) [],xs)
                        | otherwise                                                                     = l

isTrue :: (Tree a, String) -> (Tree a, String)
isTrue (_, []) = (Node (TokError "Empty input true") [], [])
isTrue l@(_, xs)
        | take 4 (deleteSpace xs) == "True"     = insertTup l (Node (TokBool True) [], drop 4 (deleteSpace xs))
        | otherwise                             = (Node (TokError "No match on True") [], [])

isFalse :: (Tree a, String) -> (Tree a, String)
isFalse (_, []) = (Node (TokError "Empty input False") [], [])
isFalse l@(_, xs)
        | take 5 (deleteSpace xs) == "False"    = insertTup l (Node (TokBool False) [], drop 5 (deleteSpace xs))
        | otherwise                             = (Node (TokError "No match on False") [], [])

isExp :: (Tree a, String) -> (Tree a, String)
isExp (_, []) = (Node (TokError "Expression missing") [], [])
isExp l@(x,xs)
        | noError (isOp1 (x, deleteSpace xs))                           = insertTup l (hasExp $ findSpace (isExp $ findSpace (isOp1 (Node TokExp [], deleteSpace xs))))
        | noError (isInt (x, deleteSpace xs))                           = insertTup l (hasExp $ findSpace (isInt (Node TokExp [], deleteSpace xs)))
        | noError (isTrue (x, deleteSpace xs))                          = insertTup l (hasExp $ findSpace (isTrue (Node TokExp [], deleteSpace xs)))
        | noError (isFalse (x, deleteSpace xs))                         = insertTup l (hasExp $ findSpace (isFalse (Node TokExp [], deleteSpace xs)))
        | not (null (deleteSpace xs)) &&  head (deleteSpace xs) == '('  = insertTup l (hasTuple $ findSpace (isExp $ findSpace (dropChar (Node (TokParen ParenceO) [], deleteSpace xs) 1)))
        | take 2 (deleteSpace xs) == "[]"                               = insertTup l (hasExp $ findSpace (Node TokExp [Node (TokType EmptyList) []], drop 2 (deleteSpace xs)))
        | noError (isFunCall (Node TokExp [], deleteSpace xs))          = insertTup l (hasExp $ findSpace (isFunCall (Node TokExp [], deleteSpace xs)))
        | noError (isId (x, deleteSpace xs))                            = insertTup l (hasExp $ findSpace (isField $ findSpace (isId (Node TokExp [], deleteSpace xs)) ))
        | otherwise                                                     = (Node (TokError ("No matching exp found at:" ++ xs)) [], xs)
        where
        hasTuple l@(_, xs)
                | noError l && not (null (deleteSpace xs)) && head (deleteSpace xs) == ')'      = insertTup l (hasExp (Node (TokParen ParenceC) [], drop 1 (deleteSpace xs)))
                | noError l && not (null (deleteSpace xs)) && head (deleteSpace xs) == ','      = insertTup l (hasTuple $ findSpace (isExp $ findSpace(Node (TokText ",") [], drop 1 (deleteSpace xs))))
                | noError l                                                                     = (Node (TokError (")-token missing at:"++xs)) [], xs)
                | otherwise                                                                     = l
        hasExp l@(_, xs)
                | noError l && noError (isOp2 $ findSpace l)    = fixOrder (isExp $ findSpace (isOp2 $ findSpace l)) --fixOrder
                | noError l                                     = l
                | otherwise                                     = (Node (TokError ("No expression found at:" ++ xs)) [], xs)

fixOrder :: (Tree a, String) -> (Tree a, String)
fixOrder (Node x y, s) = (Node x (isMultEx y), s)
        where
        isMultEx :: [Tree a] -> [Tree a]
        isMultEx t
                | length t == 3           = init t ++ [compareOperators (getOp (t !! 1)) (t !! 2)]
                | length t == 4           = init t ++ [compareOperators (getOp (t !! 2)) (t !! 3)] -- id has field
                | otherwise               = [Node (TokError ("No operator" ++ show (getOp (t !! 2)))) []]
                where
                compareOperators :: Op2 -> Tree a -> Tree a
                compareOperators op (Node x y)
                        | length y == 3 && isBigger (getOp (y !! 1)) op = Node x ([Node (TokParen ParenceO) []] ++ (init y) ++ [(compareOperators (getOp (y !! 1)) (y !! 2))] ++ [Node (TokParen ParenceC) []])
                        | length y == 3                                 = Node x (init y ++ [(compareOperators (getOp (y !! 1)) (y !! 2))])
                        | length y == 4 && isBigger (getOp (y !! 2)) op = Node x ([Node (TokParen ParenceO) []] ++ (init y) ++ [(compareOperators (getOp (y !! 2)) (y !! 3))] ++ [Node (TokParen ParenceC) []])
                        | length y == 4                                 = Node x (init y ++ [(compareOperators (getOp (y !! 2)) (y !! 3))])
                        | otherwise                                     = Node x y

getOp :: Tree a -> Op2
getOp (Node (TokOp2 y) _)     = y
getOp (Node _ _)              =  error "No operator in Exp Op2 Exp"

isBigger :: Op2 -> Op2 -> Bool
isBigger Times Plus     = True
isBigger Times Minus    = True
isBigger Div   Plus     = True
isBigger Div   Minus    = True
isBigger Mod   Plus     = True
isBigger Mod   Minus    = True
isBigger _     _        = False

isField :: (Tree a, String) -> (Tree a, String)
isField l@(_, []) = insertTup l (Node (TokField EField) [], [])
isField l@(_, xs)
        | take 3 (deleteSpace xs) == ".hd"      = insertTup l(isField (Node (TokField (FieldT FieldHD))  [], drop 3 (deleteSpace xs)))
        | take 3 (deleteSpace xs) == ".tl"      = insertTup l(isField (Node (TokField (FieldT FieldTL))  [], drop 3 (deleteSpace xs)))
        | take 4 (deleteSpace xs) == ".snd"     = insertTup l(isField (Node (TokField (FieldT FieldSND)) [], drop 4 (deleteSpace xs)))
        | take 4 (deleteSpace xs) == ".fst"     = insertTup l(isField (Node (TokField (FieldT FieldFST)) [], drop 4 (deleteSpace xs)))
        | otherwise                             = l

isFunCall :: (Tree a, String) -> (Tree a, String)
isFunCall (_, [])        = (Node (TokError "FunCall missing") [], [])
isFunCall l@(_, xs)      = insertTup l (hasId $ findSpace (isId (Node TokFunCall [], deleteSpace xs)))
        where
        hasId l@(_,xs)
                | noError l && not (null (deleteSpace xs)) && head (deleteSpace xs) == '('      = insertTup l (hasActArgs $ findSpace (dropChar (Node (TokParen ParenceO) [], deleteSpace xs) 1))
                | noError l                                                                     = (Node (TokError ("(-symbol missing for funCall at:"++xs)) [], [])
                | otherwise                                                                     = l
                where
                hasActArgs l@(_,xs)
                        | noError (isActArgs $ findSpace l)                             = hasActArgs $ findSpace (isActArgs $ findSpace l)
                        | not (null (deleteSpace xs)) && head (deleteSpace xs) == ')'   = insertTup l (dropChar (Node (TokParen ParenceC) [], xs) 1)
                        | otherwise                                                     = isActArgs $ findSpace l

isActArgs :: (Tree a, String) -> (Tree a, String)
isActArgs (_, [])       = (Node (TokError "ActArgs missing") [], [])
isActArgs l@(_, xs)     = insertTup l (hasExp $ findSpace (isExp (Node TokActArgs [], deleteSpace xs)))
        where
        hasExp l@(_, xs)
                | noError l && not (null (deleteSpace xs)) && head (deleteSpace xs) == ','      = insertTup l (hasExp $ findSpace (isExp (Node TokActArgs [Node TokComma []], deleteSpace(drop 1 (deleteSpace xs)))))
                | otherwise                                                                     = l

isOp2 :: (Tree a, String) -> (Tree a, String)
isOp2 (_, [])                           = (Node (TokError "Op2 missing") [], [])
isOp2 l@(_, input)
      | take 2 (deleteSpace input) == "=="      = insertTup l (Node (TokOp2 Comp)  [], drop 2 (deleteSpace input))
      | take 2 (deleteSpace input) == "!="      = insertTup l (Node (TokOp2 NEQ)   [], drop 2 (deleteSpace input))
      | take 2 (deleteSpace input) == "&&"      = insertTup l (Node (TokOp2 AND)   [], drop 2 (deleteSpace input))
      | take 2 (deleteSpace input) == "||"      = insertTup l (Node (TokOp2 OR)    [], drop 2 (deleteSpace input))
      | take 2 (deleteSpace input) == "<="      = insertTup l (Node (TokOp2 LEQ)   [], drop 2 (deleteSpace input))
      | take 2 (deleteSpace input) == ">="      = insertTup l (Node (TokOp2 GEQ)   [], drop 2 (deleteSpace input))
      | head (deleteSpace input) == '+'         = insertTup l (Node (TokOp2 Plus)  [], tail (deleteSpace input))
      | head (deleteSpace input) == '-'         = insertTup l (Node (TokOp2 Minus) [], tail (deleteSpace input))
      | head (deleteSpace input) == '*'         = insertTup l (Node (TokOp2 Times) [], tail (deleteSpace input))
      | head (deleteSpace input) == '/'         = insertTup l (Node (TokOp2 Div)   [], tail (deleteSpace input))
      | head (deleteSpace input) == '%'         = insertTup l (Node (TokOp2 Mod)   [], tail (deleteSpace input))
      | head (deleteSpace input) == '<'         = insertTup l (Node (TokOp2 Lt)    [], tail (deleteSpace input))
      | head (deleteSpace input) == '>'         = insertTup l (Node (TokOp2 Gt)    [], tail (deleteSpace input))
      | head (deleteSpace input) == ':'         = insertTup l (Node (TokOp2 Colon) [], tail (deleteSpace input))
isOp2 _                                         = (Node (TokError "No match Op2") [], [])

isOp1 :: (Tree a, String) -> (Tree a, String)
isOp1 (_, [])         = (Node (TokError "Op1 missing") [], [])
isOp1 l@(_, input)
      | head (deleteSpace input) == '!' = insertTup l (Node (TokOp1 ExclMark) [], tail (deleteSpace input))
      | head (deleteSpace input) == '-' = insertTup l (Node (TokOp1 PreMinus) [], tail (deleteSpace input))
      | otherwise                       = (Node (TokError "Incorrect Op1") [], [])

isInt :: (Tree a, String) -> (Tree a, String)
isInt (_, []) = (Node (TokError "Integer missing") [], [])
isInt (x, a : b)
        | a `elem` "1234567890"         = getInt (x, toInt (a : b) 0)
        | otherwise                     = isInt (x, [])
        where
                getInt (x,(xs,c)) = insertTup (x, xs) (Node (TokInt c) [], xs)

toInt :: String -> Int -> (String, Int)
toInt [] _ = ("Error", 0)
toInt l@(a:b) c
        | elem a "1234567890" && not (null b)   = toInt b ((c*10) + digitToInt a)
        | elem a "1234567890" && null b         = ([], (c * 10) + digitToInt a)
        | otherwise                             = (l, c)

isId :: (Tree a, String) -> (Tree a, String)
isId (_, []) = (Node (TokError "Empty input id") [], [])
isId t@(_, l@(x:xs)) | isLetter x         = let (first, rest) = readIdentfier l
                                              in insertTup t (Node (TokId first) [], deleteSpace rest)
                    | otherwise          = (Node (TokError ("No id Found at: " ++ xs)) [], [])

readIdentfier :: [Char] -> ([Char], [Char])
readIdentfier [] = ([], [])
readIdentfier l@(x:xs) = let (first, rest) = readLettersOrDigitsOrUnderscores xs
                             identifier = (x:first)
                             in (identifier, rest)

readLettersOrDigitsOrUnderscores :: String -> (String, String)
readLettersOrDigitsOrUnderscores xs = span isLetterOrDigitOrUnderscore xs

isLetterOrDigitOrUnderscore :: Char -> Bool
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

findSpace :: (a, String) -> (a, String)
findSpace (x, []) = (x, [])
findSpace (x, xs) = (x, deleteSpace xs)

deleteSpace :: String -> String
deleteSpace []           = []
deleteSpace l@(a:b)      =  if a <= ' ' then deleteSpace b else l
