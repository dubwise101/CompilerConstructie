module TypeChecker where
import Grammar
import Data.Tree

getGlobalTypes :: Tree Token -> [(Type, String)]  --[(RetType, String)]
getGlobalTypes (Node TokSPL x)      = findInList x []
getGlobalTypes _ = error ("Input is no valid SPL:")


getType :: [Tree Token] -> (Type, String)
getType [] = error ("no type")
getType ((Node (TokType (Id _)) x):(Node (TokId b) _):_) = (Id (showId x), b)
        where
        showId ((Node (TokId a) _ ): _) = a
        showId _ = "no Id"
getType ((Node (TokType a) _):(Node (TokId b) _):_) = (a,b)
getType ((Node (TokParen a) x):(Node (TokId b) _):_) = (getTypes [(Node (TokParen a) x)] , b)
getType ((Node (TokBrack _) x):(Node (TokId b) _):_) = (List (getTypes x), b)
getType ((Node (TokRetType Void) _):(Node (TokId b) _):_) = (TVoid, b)
getType ((Node (TokRetType RType) x):(Node (TokId b) _):_) = (getTypes x, b)
--getType x = error ("Error at:" ++ printTree x)

getTypes :: [Tree Token] -> Type
getTypes [] = error "test"
getTypes ((Node (TokType Int) _):_) = Int
getTypes ((Node (TokType Bool) _):_) = Bool
getTypes ((Node (TokType (Id _)) y ):_) = Id (getId y)
        where
        getId ((Node (TokId a) _):_) = a
--        getId x = "no match at: "++printTree x
getTypes ((Node (TokParen _) x):_) = getTypeType x
        where
        getTypeType (a:y) = getComma y (getTypes [a])
                where
                getComma ((Node (TokText ",") _):y) a = Tup a Com (getTypes x)                      
                getComma _ _ = error "test" --"no match comma "++ printTree y
--        getTypeType _ = error ("no match type "++ printTree x)
getTypes ((Node (TokBrack _) x):_) = List (getTypes x)
--getTypes x = error ("Error at:" ++ printTree x)

findInList :: [Tree Token] -> [(Type, String)] -> [(Type, String)]
findInList [] x = x
findInList (Node TokSPL x : a) f  = findInList a (findInList x f)
findInList (Node TokDecl x : a) f 
        | (length a) > 0 =  findInList a (findInList x f)
        | otherwise = (findInList x f)
findInList (Node TokVarDecl x: _) f  = checklist (getType x) f
findInList (Node TokFunDecl x: _) f  = checklist (getType x) f -- error ("Error at:" ++ printTree x)
findInList (Node x _ : _) _          = [(Bool, (show x))]

checklist :: (Type, String) -> [(Type, String)] -> [(Type, String)]
checklist l [] = [l]
checklist l@(a,b) ((c,d):e)
        | b == d = ((a,b):e)
        | otherwise = [(c,d)]++ checklist l e


{-|
printTree :: [Tree Token] -> String
printTree [] = []
printTree ((Node (TokSPL x) []): [])  = x
printTree ((Node (TokId x) []): [])  = x
printTree ((Node (TokType x) []): [])  = show (x)
printTree ((Node (TokRetType x) []): [])  = show (x)
printTree ((Node (TokParen x) []): [])  = show (TokParen x)
printTree ((Node (TokId x) []): y)  = show (x) ++ printTree (y)
printTree ((Node (TokType x) []): y)  = "type"++show (x) ++ printTree (y)
printTree ((Node (TokRetType x) []): y)  = show (x) ++ printTree (y)
printTree ((Node (TokParen x) []): y)  = show (TokParen x) ++ printTree (y)
printTree ((Node _ a): [])   = printTree (a)
printTree ((Node _ a): y)    = printTree (a)++" ---- "++ printTree (y)
-}

printTree :: Tree Token -> String
printTree (Node x []) =  show(x)
printTree (Node x xs) =  show x ++ "->"++ printTrees xs 0
        where
        printTrees :: [Tree Token] -> Int -> String
        printTrees [] i = []        
        printTrees ((Node x []): []) i  = show (x) ++ " at " ++ printInt i
        printTrees ((Node x []): y) i  = show (x)  ++ " at " ++ printInt i++ printTrees y i
        printTrees ((Node x a): []) i   = show(x) ++ " at " ++ printInt i ++" "++ printTrees a (i+1)
        printTrees ((Node x a): y)  i  = show(x) ++ " at "++ printInt i ++" "++ printTrees a (i+1)++"  "++ printTrees (y) i

printInt :: Int -> String
printInt a = show a

printGlobalTypes :: [(Type, String)] -> String
printGlobalTypes [] =""
printGlobalTypes (((List a),s):b) = "list "++ show a ++ " "++ s ++ "; "++printGlobalTypes b
printGlobalTypes ((t,s):b) = show t ++ " "++ s ++ "; "++printGlobalTypes b
