        module PrettyPrinter where
        
        import Grammar
        
        prettyPrintTree :: Tree a -> String
        prettyPrintTree (Node x []) = show x
        prettyPrintTree (Node _ xs) = stripMultNew (printTree' xs 0)
                where
                printTree' [] _ = []
                printTree' (Node a [] : Node t@(TokCurl CurlC) [] : x)              i       = show a ++ newline (i-1) ++ show t ++ newline (i-1) ++ printTree' x (i-1)
                printTree' (Node a b : Node t@(TokCurl CurlC) [] : x)               i       = show a ++ printTree' b i ++ newline (i-1) ++ show t ++ newline (i-1) ++ printTree' x (i-1)
                printTree' (Node a [] : Node t@(TokCurl CurlC) y : x)               i       = show a ++ newline (i-1) ++ show t ++ newline (i-1) ++ printTree' y (i-1) ++ printTree' x (i-1)
                printTree' (Node a b : Node t@(TokCurl CurlC) y :x)                 i       = show a ++ printTree' b i ++ newline (i-1) ++ show t ++ newline (i-1) ++ printTree' y (i-1) ++ printTree' x (i-1)
                printTree' (Node a [] : Node (TokText ";") [] : x)                  i       = show a ++ ";"++newline i ++  printTree' x i
                printTree' (Node a b : Node (TokText ";") [] : x)                   i       = show a ++ printTree' b i ++ ";"++newline i++  printTree' x i
                printTree' (Node t@(TokOp2 _) _ : x)                                  i       = show t ++ " " ++ printTree' x i
                printTree' (Node t@(TokOp1 _) _ : x)                                  i       = show t ++ printTree' x i
                printTree' (Node t1@(TokId _) []:Node t2@(TokField (FieldT _)) _:x) i       = show t1 ++ show t2 ++ " "++ printTree' x i
                printTree' (Node TokFunDecl x : y)                                    i       = "\n" ++ newline i ++ printTree' x i ++ printTree' y i
                printTree' (Node m@(TokRetType Void) x : y)                           i       = show m ++ " "++ printTree' x i ++ printTree' y i
                printTree' (Node m@(TokStmt Return) x : y)                            i       = show m ++ printTree' x i ++ printTree' y i
                printTree' (Node m@(TokStmt IfState) x : y)                           i       = show m ++ " " ++ printTree' x i ++ printTree' y i
                printTree' (Node m@(TokStmt ElseState) x : y)                         i       = show m ++ " " ++ printTree' x i ++ printTree' y i
                printTree' (Node m@(TokStmt WhileState) x : y)                        i       = show m ++ " " ++ printTree' x i ++ printTree' y i
                printTree' (Node t@(TokParen _) a : x)                                i       = show t ++ " " ++ printTree' a i ++ printTree' x i
                printTree' (Node t@(TokText "=") a : x)                               i       = show t ++ " " ++ printTree' a i ++ printTree' x i
                printTree' (Node TokVarDecl a : x)                                    i       = printTree' a i ++ printTree' x i
                printTree' (Node t@(TokBrack _) a : x)                                i       = show t ++ printTree' a i ++ printTree' x i
                -- printTree' (Node t@(TokBrack _) a : [])                               i       = show t ++ printTree' a i     DEZE REGEL KOMT IE NOOIT IN
                printTree' (Node (TokType Id) a : [])                                 i       = printTree' a i
                printTree' (Node t@(TokCurl CurlO) a : x)                             i       = newline i ++  show t ++ newline (i+1) ++ printTree' a (i+1) ++ printTree' x (i+1)
                printTree' (Node t@(TokCurl _) [] : [])                               i       = show t ++ newline i
                printTree' (Node t@(TokCurl CurlC) a : x)                             i       = show t ++ newline (i-1) ++ printTree' a (i-1) ++ printTree' x (i-1)
                printTree' (Node t@(TokText ",") a : x)                               i       = show t ++ " " ++ printTree' a i ++ printTree' x i
                printTree' (Node (TokField EField) _ : x)                             i       = printTree' x i
                printTree' (Node x [] : y)                                            i       = show x ++ " " ++ printTree' y i
                printTree' (Node (TokRetType RType) a : b)                            i       = printTree' a i ++ printTree' b i
                printTree' (Node _ a : y)                                             i       = printTree' a i ++ printTree' y i
        
        newline :: Int -> String
        newline 0 = "\n"
        newline a = "\n" ++ tabs a
        
        tabs :: Int -> String
        tabs 0 = "";
        tabs a = "\t" ++ tabs (a-1)
        
        stripMultNew :: String -> String
        stripMultNew []                 = []
        stripMultNew (' ':';':xs)        = stripMultNew (';':xs)
        stripMultNew (' ':']':xs)        = stripMultNew (']':xs)
        stripMultNew (' ':')':xs)        = stripMultNew (')':xs)
        stripMultNew ('(':' ':xs)        = stripMultNew ('(':xs)
        stripMultNew (' ':',':xs)        = stripMultNew (',':xs)
        stripMultNew ('\n':xs)           = '\n' : stripMultNew (striplayout xs 0)
               where
                   striplayout ('\t': a) b = striplayout a (b+1)
                   striplayout ('\n': a) _ = a
                   striplayout a b = tabs b ++ a
        stripMultNew (a:b) = a : stripMultNew b
