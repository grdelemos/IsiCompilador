module Main where
import Data.Char
import Data.List
import System.Environment (getArgs)
import System.IO 
import Lexer
import Parser
import SemanticPy
import SemantiC


main :: IO ()
main = do  
    args <- getArgs       
    file <- openFile (head args) ReadMode
    text <- hGetContents file
    let toks = tokenize text
        tree = parse toks
        (code, warnings) = case args !! 1 of
            "py" -> semanticPy tree []
            "c"  -> semantiC tree []
            _    -> error "Opção de linguagem inválida"
        (nome, ts) = span (/= '.') (head args) in
            do 
                putStrLn warnings
                putStrLn  code
                writeFile (nome ++ "." ++ (args !! 1)) code
    