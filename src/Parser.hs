module Parser where
import Data.Char
import Grammar

tupleToTree :: (Tree a, String) -> Tree a
tupleToTree (tree, _) = tree

noError :: (Tree a, String) -> Bool
noError (Branch (TokError _) _,_)       = False
noError _                               = True

dropChar :: (Tree a, String) -> Int -> (Tree a, String)
dropChar (tree, input) a
        | length (ds input) < a    = (Branch (TokError "cannot remove this many chars at:") [], [])
        | otherwise             = (tree, drop a (ds input))

insertTup :: (Tree a, String) -> (Tree a, String) -> (Tree a, String)
insertTup (Branch _ _, _) (Branch (TokError y) b, s2)     = (Branch (TokError y) b, s2)
insertTup (Branch x [], _) (Branch y a, s2)                      = (Branch x [Branch y a], s2)
insertTup (Branch x a, _) (Branch y [], s2)                      = (Branch x (a ++ [Branch y []]), s2)
insertTup (Branch x a, _) (Branch y b, s2)                       = (Branch x (a ++ [Branch y b]), s2)

insertNode :: Tree a -> Tree a -> Tree a
insertNode (Branch x []) (Branch y a)   = Branch x [Branch y a]
insertNode (Branch x a) (Branch y [])   = Branch x (a ++ [Branch y []])
insertNode (Branch x a) (Branch y b)    = Branch x (a ++ [Branch y b])

isSPL :: String -> Tree a
isSPL [] = Branch (TokError "empty input file") []
isSPL input = getDecl (isDecl (Branch TokSPL [], input))
        where
        getDecl l@(t,s)
                | noError l &&  not (null  (ds s))      = insertNode t (isSPL (ds s))
                | otherwise                             = tupleToTree (isDecl (Branch TokSPL [], input))

isDecl :: (Tree a, String) -> (Tree a, String)
isDecl (_, []) = (Branch (TokError "No decleration found") [], [])
isDecl l@(tree, input)
        | noError (isVarDecl l)     = insertTup l (isVarDecl (Branch TokDecl [], ds input))
        | noError (isFunDecl l)     = insertTup l (isFunDecl (Branch TokDecl [], ds input))
        | otherwise                 = isVarDecl l

isVarDecl:: (Tree a, String) -> (Tree a, String)
isVarDecl (_, [])       = (Branch (TokError "empty input Variable") [], [])
isVarDecl l@(_, input)      = insertTup l (hasId $ fs (isType (Branch TokVarDecl [], ds input)))
        where
        hasId t
                | noError t = hasEqSign $ fs (isId t)
                | otherwise = t
                where
                hasEqSign l@(_, s)
                        | noError l && not (null s) && head s == '=' = insertTup l (hasExp $ fs (isExp $ fs (dropChar (Branch (TokText "=") [],s) 1)))
                        | noError l = (Branch (TokError (" = sign missing in varDecl at: "++ s)) [], s)
                        | otherwise = l
                        where
                        hasExp l@(_, s)
                                | noError l && not (null s) && head s == ';' = insertTup l (dropChar (Branch (TokText ";") [],s) 1)
                                | noError l = (Branch (TokError (" ; sign missing in varDecl at: "++ s)) [], s)
                                | otherwise = l

isFunDecl:: (Tree a, String) -> (Tree a, String)
isFunDecl (_, []) = (Branch (TokError "functiondecl missing") [], [])
isFunDecl l@(_,input) = insertTup l (hasId $ fs (isRetType (Branch TokFunDecl [], ds input)))
        where
        hasId t
                | noError t = hasParen $ fs (isId $ fs t)
                | otherwise = t
                where
                hasParen l@(_,input)
                        | not (null input) && head input == '(' = insertTup l (hasFunargs $ fs (dropChar (Branch (TokParen ParenceO) [], input) 1))
                        | otherwise = (Branch (TokError (" ( sign missing in funDecl at: "++ input)) [], input)
                        where
                        hasFunargs l@ (_,input)
                                | noError (isFunarg $ fs l)                     = hasFunargs $ fs (isFunarg $ fs l)
                                | not (null (ds input)) && head (ds input) == ')'       = insertTup l (hasMult (fs (Branch (TokParen ParenceC) [], input)))
                                where
                                hasMult l@(t,input)
                                        | head (ds(tail (ds input))) == '{' = insertTup l (hasVardecl (fs(Branch (TokCurl CurlO) [],tail(ds(tail (ds input))))))
                                        | otherwise                     = (Branch (TokError ("wrong funarg or missing ) Or { token, at:" ++ input)) [], input)
                                       where
                                       hasVardecl l
                                                | noError(isVarDecl (fs l))     = hasVardecl $ fs (isVarDecl (fs l))
                                                | otherwise                     = hasStatement $ fs (isStatement (fs l))
                                                where
                                                hasStatement l@(_,input)
                                                    | noError l && noError (isStatement (fs l)) = hasStatement (isStatement (fs l))
                                                    | noError l && head (ds input) == '}'       = insertTup l (dropChar (Branch (TokCurl CurlC) [], input) 1)
                                                    | otherwise                                 = l

isFunarg :: (Tree a, String) -> (Tree a, String)
isFunarg (_, [])        = (Branch (TokError "function argument missing") [], [])
isFunarg l@(_, input)       = insertTup l (hasType (isType (Branch TokFArgs [], ds input)))
            where
            hasType l@(_, input)
                | noError l     = hasId $ fs (isId (fs l))
                | otherwise     = (Branch (TokError ("no match in funarg at:" ++ input)) [], [])
                where
                hasId (_, []) = (Branch (TokError "no id found in funarg") [], [])
                hasId l@(_, input)
                        | noError l && not (null (ds input)) && head (ds input) == ','  = insertTup l (isFunarg $ fs (dropChar (Branch (TokText ",") [], ds input) 1))
                        | otherwise                                                     = l

isStatement :: (Tree a, String) -> (Tree a, String)
isStatement (_, []) = (Branch (TokError "empty input in a statement") [], [])
isStatement l@(x,xs)
        | head (ds xs) == '{'                                                   = insertTup l (multiStatement $ fs (Branch (TokStmt MultState) [Branch (TokCurl CurlO) []], tail (ds xs)))
        | take 2 (ds xs) == "if" && head (ds (drop 2 (ds xs))) == '('           = insertTup l (hasExp $ fs (isExp (Branch (TokStmt IfState) [Branch (TokParen ParenceO) []], drop 1 (ds (drop 2 (ds xs))))))
        | take 5 (ds xs) == "while" && head (ds (drop 5 (ds xs))) == '('        = insertTup l (hasExp $ fs (isExp (Branch (TokStmt WhileState) [Branch (TokParen ParenceO) []], tail (ds (drop 5 (ds xs))))))
        | noError(isFunCall (x,xs))                                             = insertTup l (hasFuncall (isFunCall (Branch (TokStmt FuncSate) [], ds xs)))
        | take 6 (ds xs) == "return"                                            = insertTup l (maybeExp $ fs (Branch (TokStmt Return) [], ds (drop 6 (ds xs))))
        | noError(isId (fs l))                                                  = insertTup l (hasId $ fs (isId (Branch (TokStmt EqState) [], ds xs)))
        | otherwise                                                             = (Branch (TokError ("no matching statement at:" ++ ds xs)) [], xs)
        where
         multiStatement (_, []) = (Branch (TokError "empty input in multi statement") [], [])
         multiStatement l@(x,xs)
                | noError (isStatement (x, ds xs))              = multiStatement (isStatement (x, ds xs))
                | not (null (ds xs)) && head (ds xs) == '}'     = insertTup l (Branch (TokCurl CurlC) [], tail (ds xs))
                | otherwise                                     = isStatement (x, ds xs)
         hasExp (_, []) = (Branch (TokError "error in exp for statement") [], [])
         hasExp l@(_, xs)
                | noError l
                && not (null (ds xs))
                && head (ds xs) == ')'  = insertTup l (hasStatement $ fs (Branch (TokParen ParenceC) [], ds(tail (ds xs))))
                | noError l             =  (Branch (TokError ("missing ) at:"++ xs)) [], [])
                | otherwise             =  l
                where
                hasStatement (_, []) = (Branch (TokError "empty input for state for statement") [], [])
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
                        | noError l                                                     = (Branch (TokError ("= missing in statement at:"++xs)) [], [])
                        | otherwise                                                     = l
                        where
                        hasIdExp l@(_,xs)
                                | noError l && not (null (ds xs)) && head  (ds xs) == ';'       = insertTup l (dropChar (Branch (TokText ";")[],xs) 1)
                                | noError l                                                     = (Branch (TokError ("; missing in statement at:"++xs)) [], [])
                                | otherwise                                                     = l
         hasFuncall l@(_,xs)
                | noError l && not (null (ds xs)) && head (ds xs) == ';'        = insertTup l (dropChar (Branch (TokText ";") [], ds xs) 1)
                | otherwise                                                     = l
         maybeExp l@(_, xs)
                | noError (isExp (fs l))                        = maybeExp (isExp (fs l))
                | not (null (ds xs)) && head (ds xs) == ';'     = insertTup l (dropChar (Branch (TokText ";") [], ds xs) 1)
                | otherwise                                     = (Branch (TokError ("; missing from return at:"++xs)) [], [])

isRetType :: (Tree a, String) -> (Tree a, String)
isRetType (_, [])        = (Branch (TokError "data missing for Return type") [], [])
isRetType l@(x,xs)
        | take 4 (ds xs) == "Void"      = insertTup l (Branch (TokRetType Void) [], drop 4 (ds xs))
        | noError(isType (x, ds xs))    = insertTup l $ fs (isType (Branch (TokRetType RType) [], ds xs))
        | otherwise                     = isType (x, ds xs)

isType :: (Tree a, String) -> (Tree a, String)
isType (_, []) = (Branch (TokError "empty input type") [], [])
isType l@(x,xs)
        | head (ds xs) == '['           = insertTup l (hasTypeList $ fs (isType (Branch (TokBrack BrackO) [], ds (tail (ds xs)))))
        | head (ds xs) == '('           = insertTup l (hasTypeTup $ fs (isType (dropChar (Branch (TokParen ParenceO) [], ds xs) 1)) )
        | take 3 (ds xs) == "Int"       = insertTup l (Branch (TokType Int) [], ds (drop 3 (ds xs)))
        | take 4 (ds xs) == "Bool"      = insertTup l (Branch (TokType Bool) [], ds (drop 4 (ds xs)))
        | noError(isId (x, ds xs))      = insertTup l (isId (Branch (TokType (Id "")) [], ds xs))
        | otherwise                     = (Branch (TokError ("no input type found at: " ++ xs)) [], [])
        where
        hasTypeList l@(_, xs)
               | noError l && not (null (ds xs)) && head (ds xs) == ']' = insertTup l (dropChar (Branch (TokBrack BrackC) [], ds xs) 1)
               | noError l                                              = (Branch (TokError ("] missing in for type at:"++xs)) [],xs)
               | otherwise                                              = l
        hasTypeTup l@(_, xs)
                | noError l && not (null (ds xs)) && head (ds xs) == ','        = insertTup l (hasSecondType $ fs (isType $ fs (dropChar (Branch (TokText ",") [], xs ) 1)))
                | noError l                                                     = (Branch (TokError (",-token missing in for typeTupel at:"++xs)) [],xs)
                | otherwise                                                     = l
                where
                hasSecondType l@(_, xs)
                        | noError l && not (null (ds xs)) && head (ds xs) == ')'        = insertTup l (dropChar (Branch (TokParen ParenceC) [], xs ) 1)
                        | noError l                                                     = (Branch (TokError (")-token missing in for typeTupel at (max two types in each tuple):"++xs)) [],xs)
                        | otherwise                                                     = l

isTrue :: (Tree a, String) -> (Tree a, String)
isTrue (_, []) = (Branch (TokError "empty input true") [], [])
isTrue l@(_, xs)
        | take 4 (ds xs) == "True"      = insertTup l (Branch (TokBool True) [], drop 4 (ds xs))
        | otherwise                     = (Branch (TokError "no match on true") [], [])

isFalse :: (Tree a, String) -> (Tree a, String)
isFalse (_, []) = (Branch (TokError "empty input False") [], [])
isFalse l@(_, xs)
        | take 5 (ds xs) == "False"     = insertTup l (Branch (TokBool False) [], drop 5 (ds xs))
        | otherwise                     = (Branch (TokError "no match on false") [], [])

isExp :: (Tree a, String) -> (Tree a, String)
isExp (_, []) = (Branch (TokError "expression missing") [], [])
isExp l@(x,xs)
        | noError (isOp1 (x, ds xs))                            = insertTup l (hasExp $ fs (isExp $ fs (isOp1 (Branch TokExp [], ds xs))))
        | noError (isInt (x, ds xs))                            = insertTup l (hasExp $ fs (isInt (Branch TokExp [], ds xs)))
        | noError (isTrue (x, ds xs))                           = insertTup l (hasExp $ fs (isTrue (Branch TokExp [], ds xs)))
        | noError (isFalse (x, ds xs))                          = insertTup l (hasExp $ fs (isFalse (Branch TokExp [], ds xs)))
        | not (null (ds xs)) &&  head (ds xs) == '('            = insertTup l (hasTuple $ fs (isExp $ fs (dropChar (Branch (TokParen ParenceO) [], ds xs) 1)))
        | take 2 (ds xs) == "[]"                                = insertTup l (hasExp $ fs (Branch TokExp [Branch (TokType EmptyList) []], drop 2 (ds xs)))
        | noError (isFunCall (Branch TokExp [], ds xs))         = insertTup l (hasExp $ fs (isFunCall (Branch TokExp [], ds xs)))
        | noError (isId (x, ds xs))                             = insertTup l (hasExp $ fs (isField $ fs (isId (Branch TokExp [], ds xs)) ))
        | otherwise                                             = (Branch (TokError ("no matching exp found at:" ++ xs)) [], xs)
        where
        hasTuple l@(_, xs)
                | noError l && not (null (ds xs)) && head (ds xs) == ')'        = insertTup l (hasExp (Branch (TokParen ParenceC) [], drop 1 (ds xs)))
                | noError l && not (null (ds xs)) && head (ds xs) == ','        = insertTup l (hasTuple $ fs (isExp $ fs(Branch (TokText ",") [], drop 1 (ds xs))))
                | noError l                                                     = (Branch (TokError (")-token missing at:"++xs)) [], xs)
                | otherwise                                                     = l
        hasExp l@(_, xs)
                | noError l && noError (isOp2 $ fs l)   = fixOrder (isExp $ fs (isOp2 $ fs l)) --fixOrder
                | noError l                             = l
                | otherwise                             = (Branch (TokError ("no expression found at:" ++ xs)) [], xs)

fixOrder :: (Tree a, String) -> (Tree a, String)
fixOrder (Branch x y, s) = (Branch x (isMultEx y), s)
        where
        isMultEx :: [Tree a] -> [Tree a]
        isMultEx t
                | length t == 3           = init t ++ [compareOperators (getOp (t !! 1)) (t !! 2)]
                | length t == 4           = init t ++ [compareOperators (getOp (t !! 2)) (t !! 3)] -- id has field
                | otherwise               = [Branch (TokError ("no operator" ++ show (getOp (t !! 2)))) []]
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
isFunCall (_, [])        = (Branch (TokError "FunCall missing") [], [])
isFunCall l@(_, xs)      = insertTup l (hasId $ fs (isId (Branch TokFunCall [], ds xs)))
        where
        hasId l@(_,xs)
                | noError l && not (null (ds xs)) && head (ds xs) == '('        = insertTup l (hasActArgs $ fs (dropChar (Branch (TokParen ParenceO) [], ds xs) 1))
                | noError l                                                     = (Branch (TokError ("(-symbol missing for funCall at:"++xs)) [], [])
                | otherwise                                                     = l
                where
                hasActArgs l@(_,xs)
                        | noError (isActArgs $ fs l)                    = hasActArgs $ fs (isActArgs $ fs l)
                        | not (null (ds xs)) && head (ds xs) == ')'     = insertTup l (dropChar (Branch (TokParen ParenceC) [], xs) 1)
                        | otherwise                                     = isActArgs $ fs l

isActArgs :: (Tree a, String) -> (Tree a, String)
isActArgs (_, [])       = (Branch (TokError "ActArgs missing") [], [])
isActArgs l@(_, xs)      = insertTup l (hasExp $ fs (isExp (Branch TokActArgs [], ds xs)))
        where
        hasExp l@(_, xs)
                | noError l && not (null (ds xs)) && head (ds xs) == ','        = insertTup l (hasExp $ fs (isExp (Branch TokActArgs [Branch TokComma []], ds(drop 1 (ds xs)))))
                | otherwise                                                     = l

isOp2 :: (Tree a, String) -> (Tree a, String)
isOp2 (_, [])                           = (Branch (TokError "op2 missing") [], [])
isOp2 l@(_, input)
      | take 2 (ds input) == "=="       = insertTup l (Branch (TokOp2 Comp)  [], drop 2 (ds input))
      | take 2 (ds input) == "!="       = insertTup l (Branch (TokOp2 NEQ)   [], drop 2 (ds input))
      | take 2 (ds input) == "&&"       = insertTup l (Branch (TokOp2 AND)   [], drop 2 (ds input))
      | take 2 (ds input) == "||"       = insertTup l (Branch (TokOp2 OR)    [], drop 2 (ds input))
      | take 2 (ds input) == "<="       = insertTup l (Branch (TokOp2 LEQ)   [], drop 2 (ds input))
      | take 2 (ds input) == ">="       = insertTup l (Branch (TokOp2 GEQ)   [], drop 2 (ds input))
      | head (ds input) == '+'          = insertTup l (Branch (TokOp2 Plus)  [], tail (ds input))
      | head (ds input) == '-'          = insertTup l (Branch (TokOp2 Minus) [], tail (ds input))
      | head (ds input) == '*'          = insertTup l (Branch (TokOp2 Times) [], tail (ds input))
      | head (ds input) == '/'          = insertTup l (Branch (TokOp2 Div)   [], tail (ds input))
      | head (ds input) == '%'          = insertTup l (Branch (TokOp2 Mod)   [], tail (ds input))
      | head (ds input) == '<'          = insertTup l (Branch (TokOp2 Lt)    [], tail (ds input))
      | head (ds input) == '>'          = insertTup l (Branch (TokOp2 Gt)    [], tail (ds input))
      | head (ds input) == ':'          = insertTup l (Branch (TokOp2 Colon) [], tail (ds input))
isOp2 _                                 = (Branch (TokError "geen match op2") [], [])

isOp1 :: (Tree a, String) -> (Tree a, String)
isOp1 (_, [])         = (Branch (TokError "op1 missing") [], [])
isOp1 l@(_, input)
      | head (ds input) == '!'  = insertTup l (Branch (TokOp1 ExclMark) [], tail (ds input))
      | head (ds input) == '-'  = insertTup l (Branch (TokOp1 PreMinus) [], tail (ds input))
      | otherwise               = (Branch (TokError "incorrect op1") [], [])

isInt :: (Tree a, String) -> (Tree a, String)
isInt (_, []) = (Branch (TokError "Integer missing") [], [])
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
isId (_, []) = (Branch (TokError "empty input id") [], [])
isId t@(_, l@(x:xs)) | isLetter x         = let (first, rest) = readIdentfier l
                                              in insertTup t (Branch (TokId first) [], ds rest)
                    | otherwise          = (Branch (TokError ("no id Found at: " ++ xs)) [], [])

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