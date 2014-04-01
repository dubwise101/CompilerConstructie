module PrettyPrinter where
import Grammar

prettyPrintTree :: Tree a -> String
prettyPrintTree (Branch x []) = show x
prettyPrintTree (Branch _ xs) = stripMultNew (printTree' xs 0)
        where
        printTree' [] _ = []
        printTree' (Branch a [] : Branch t@(TokCurl CurlC) [] : x)              i       = show a ++ newline (i-1) ++ show t ++ newline (i-1) ++ printTree' x (i-1)
        printTree' (Branch a b : Branch t@(TokCurl CurlC) [] : x)               i       = show a ++ printTree' b i ++ newline (i-1) ++ show t ++ newline (i-1) ++ printTree' x (i-1)
        printTree' (Branch a [] : Branch t@(TokCurl CurlC) y : x)               i       = show a ++ newline (i-1) ++ show t ++ newline (i-1) ++ printTree' y (i-1) ++ printTree' x (i-1)
        printTree' (Branch a b : Branch t@(TokCurl CurlC) y :x)                 i       = show a ++ printTree' b i ++ newline (i-1) ++ show t ++ newline (i-1) ++ printTree' y (i-1) ++ printTree' x (i-1)
        printTree' (Branch a [] : Branch (TokText ";") [] : x)                  i       = show a ++ ";"++newline i ++  printTree' x i
        printTree' (Branch a b : Branch (TokText ";") [] : x)                   i       = show a ++ printTree' b i ++ ";"++newline i++  printTree' x i
        printTree' (Branch t@(TokOp2 _) _ : x)                                  i       = show t ++ " " ++ printTree' x i
        printTree' (Branch t@(TokOp1 _) _ : x)                                  i       = show t ++ printTree' x i
        printTree' (Branch t1@(TokId _) []:Branch t2@(TokField (FieldT _)) _:x) i       = show t1 ++ show t2 ++ " "++ printTree' x i
        printTree' (Branch TokFunDecl x : y)                                    i       = "\n" ++ newline i ++ printTree' x i ++ printTree' y i
        printTree' (Branch m@(TokRetType Void) x : y)                           i       = show m ++ " "++ printTree' x i ++ printTree' y i
        printTree' (Branch m@(TokStmt Return) x : y)                            i       = show m ++ printTree' x i ++ printTree' y i
        printTree' (Branch m@(TokStmt IfState) x : y)                           i       = show m ++ " " ++ printTree' x i ++ printTree' y i
        printTree' (Branch m@(TokStmt ElseState) x : y)                         i       = show m ++ " " ++ printTree' x i ++ printTree' y i
        printTree' (Branch m@(TokStmt WhileState) x : y)                        i       = show m ++ " " ++ printTree' x i ++ printTree' y i
        printTree' (Branch t@(TokParen _) a : x)                                i       = show t ++ " " ++ printTree' a i ++ printTree' x i
        printTree' (Branch t@(TokText "=") a : x)                               i       = show t ++ " " ++ printTree' a i ++ printTree' x i
        printTree' (Branch TokVarDecl a : x)                                    i       = printTree' a i ++ printTree' x i
        printTree' (Branch t@(TokBrack _) a : x)                                i       = show t ++ printTree' a i ++ printTree' x i
        printTree' (Branch t@(TokBrack _) a : [])                               i       = show t ++ printTree' a i
        printTree' (Branch (TokType (Id _)) a : [])                                 i       = printTree' a i
        printTree' (Branch t@(TokCurl CurlO) a : x)                             i       = newline i ++  show t ++ newline (i+1) ++ printTree' a (i+1) ++ printTree' x (i+1)
        printTree' (Branch t@(TokCurl _) [] : [])                               i       = show t ++ newline i
        printTree' (Branch t@(TokCurl CurlC) a : x)                             i       = show t ++ newline (i-1) ++ printTree' a (i-1) ++ printTree' x (i-1)
        printTree' (Branch t@(TokText ",") a : x)                               i       = show t ++ " " ++ printTree' a i ++ printTree' x i
        printTree' (Branch (TokField EField) _ : x)                             i       = printTree' x i
        printTree' (Branch x [] : y)                                            i       = show x ++ " " ++ printTree' y i
        printTree' (Branch (TokRetType RType) a : b)                            i       = printTree' a i ++ printTree' b i
        printTree' (Branch _ a : y)                                             i       = printTree' a i ++ printTree' y i

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
stripMultNew ('\n':a)           = '\n' : stripMultNew (striplayout a 0)
       where
           striplayout ('\t': a) b = striplayout a (b+1)
           striplayout ('\n': a) _ = a
           striplayout a b = tabs b ++ a
stripMultNew (a:b) = a : stripMultNew b