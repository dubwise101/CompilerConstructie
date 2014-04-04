module TypeChecker where
import Grammar

getGlobalTypes :: Tree a -> [(Type, String)]  --[(RetType, String)]
getGlobalTypes (Branch TokSPL x)      = checklist ( findInList x)
getGlobalTypes (Branch TokDecl x)     = findInList x
getGlobalTypes (Branch TokVarDecl x)  = [getType x]
getGlobalTypes (Branch TokFunDecl x)  = [getType x] --error ("Error at:" ++ printTree x)
--getGlobalTypes (Branch TokFunDecl x)  = error ("Error at:" ++ printTree x)
getGlobalTypes (Branch _ _)           = []

getType :: [Tree a] -> (Type, String)
getType [] = error ("no type")
getType ((Branch (TokType (Id _)) x):(Branch (TokId b) _):_) = (Id (showId x), b)
        where
        showId ((Branch (TokId a) _ ): _) = a
        showId _ = "no Id"
getType ((Branch (TokType a) x):(Branch (TokId b) _):_) = (a,b)
getType ((Branch (TokParen a) x):(Branch (TokId b) _):_) = (getTypes [(Branch (TokParen a) x)] , b)
getType ((Branch (TokBrack a) x):(Branch (TokId b) _):_) = (List (getTypes x), b)
getType ((Branch (TokRetType Void) x):(Branch (TokId b) _):_) = (TVoid, b)
getType ((Branch (TokRetType RType) x):(Branch (TokId b) _):_) = (getTypes x, b)
getType x = error ("Error at:" ++ printTree x)

getTypes :: [Tree a] -> Type
getTypes [] = error "test"
getTypes ((Branch (TokType Int) _):_) = Int
getTypes ((Branch (TokType Bool) _):_) = Bool
getTypes ((Branch (TokType (Id _)) y ):_) = Id (getId y)
        where
        getId ((Branch (TokId a) _):_) = a
        getId x = "no match at: "++printTree x
getTypes ((Branch (TokParen _) x):_) = getTypeType x
        where
        getTypeType (a:y) = getComma y (getTypes [a])
                where
                getComma ((Branch (TokText ",") x):y) a = Tup a Com (getTypes x)                      
                getComma _ _ = error "test" --"no match comma "++ printTree y
        getTypeType _ = error ("no match type "++ printTree x)
getTypes ((Branch (TokBrack _) x):_) = List (getTypes x)
getTypes x = error ("Error at:" ++ printTree x)

findInList :: [Tree a] -> [(Type, String)]
findInList [] = []
findInList (a:b)= getGlobalTypes a ++ findInList b

checklist :: [(Type, String)] -> [(Type, String)]
checklist [] = []
checklist ((a,b):c)
        | notEqual b c = ((a,b):c)
        | otherwise = error "er zit een dubbele tussen"
        where
        notEqual a [] = True
        notEqual a ((b,c):d)
                | a /= c = ((notEqual a d) && (notEqual c d))
                | otherwise = error ("er zit een dubbele tussen: " ++ a++ " = "++ c)


printTree :: [Tree a] -> String
printTree [] = []
printTree ((Branch (TokId x) []): [])  = "id = " ++ x
printTree ((Branch (TokType x) []): [])  = show (x)
printTree ((Branch (TokRetType x) []): [])  = show (x)
printTree ((Branch (TokParen x) []): [])  = show (TokParen x)
printTree ((Branch (TokId x) []): y)  = show (x) ++ printTree (y)
printTree ((Branch (TokType x) []): y)  = "type"++show (x) ++ printTree (y)
printTree ((Branch (TokRetType x) []): y)  = show (x) ++ printTree (y)
printTree ((Branch (TokParen x) []): y)  = show (TokParen x) ++ printTree (y)
printTree ((Branch _ a): [])   = printTree (a)
printTree ((Branch _ a): y)    = printTree (a)++" ---- "++ printTree (y)

printGlobalTypes :: [(Type, String)] -> String
printGlobalTypes [] =""
printGlobalTypes (((List a),s):b) = "list "++ show a ++ " "++ s ++ "; "++printGlobalTypes b
printGlobalTypes ((t,s):b) = show t ++ " "++ s ++ "; "++printGlobalTypes b
