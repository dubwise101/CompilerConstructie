module Main where

import Grammar
import Parser
import PrettyPrinter
import TypeChecker

import System.IO
import Data.Tree

openFile :: String -> IO ()
openFile fileName = do
        spl <- readSPL fileName
        print $ (printTree spl)

readSPL :: String -> IO (Tree Token)
readSPL fileName = do
                text <- readFile fileName
                return   (isSPL (stripLayout (stripComments text)))
                where
                stripLayout xs = replaceStr (replaceStr xs "\n" " ") "\t" " "


printTreeToFile :: Tree Token -> IO ()
printTreeToFile tree = do
        outh <- System.IO.openFile "prettyprint.txt" WriteMode
        hPutStrLn outh (prettyPrintTree tree)
        hClose outh
        error "klaar"

main :: IO ()
main = Main.openFile "test/testProgram10.spl"  -- AST is pretty printed to prettyprint.txt
