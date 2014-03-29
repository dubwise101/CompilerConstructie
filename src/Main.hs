module Main where

import Grammar
import Parser
import PrettyPrinter

import System.IO

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


printTreeToFile :: Tree a -> IO ()
printTreeToFile tree = do
        outh <- System.IO.openFile "prettyprint.txt" WriteMode
        hPutStrLn outh (prettyPrintTree tree)
        hClose outh

main :: IO ()
main = Main.openFile "test/testProgram10.spl"  -- AST is pretty printed to prettyprint.txt
