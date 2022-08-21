module Lexer where

import Data.Char
import Data.List

---- Lexer ----

data Operator = Plus | Minus | Times | Div | Pow | Sqrt | Log | Log2 | Log10
    deriving (Show, Eq, Read)

data OpRel = Greater | Less | Equal | GreatEq | LessEq | Diff
   deriving (Show, Eq, Read)

data Token = TokOp Operator
           | TokOpRel OpRel
           | TokEnquanto
           | TokFaca
           | TokPonto
           | TokVirgula
           | TokAssign
           | TokParenE
           | TokParenD
           | TokColchE
           | TokColchD
           | TokIdent String
           | TokNum String
           | TokText String
           | TokEnd
           | TokProg
           | TokFim
           | TokAspasA
           | TokAspasF
           | TokDeclaraNum
           | TokDeclaraTxt
           | TokLeia
           | TokEscreva
           | TokSe
           | TokEntao
           | TokSenao
    deriving (Show, Eq, Read)

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Times
           | c == '/' = Div
           | c == '^' = Pow

operatorRel :: Char -> String -> OpRel
operatorRel c cs
   | "<=" `isPrefixOf` cs = LessEq
   | ">=" `isPrefixOf` cs = GreatEq
   | "!=" `isPrefixOf` cs = Diff
   | c == '<' = Less
   | c == '>' = Greater
   
assign :: String -> [Token]
assign (c : cs)
   | c == '=' = TokAssign : tokenize cs
   | otherwise = error  "Erro Léxico na atribuição "

equOr :: String -> [Token]
equOr (c : cs)
   | c == '=' = TokOpRel Equal : tokenize cs
   | otherwise = error "Erro léxico no sinal de ="

lessOr :: String -> [Token]
lessOr (c : cs)
   | c == '=' = TokOpRel LessEq : tokenize cs
   | otherwise = TokOpRel Less : tokenize (c : cs)

greatOr :: String -> [Token]
greatOr (c : cs)
   | c == '=' = TokOpRel GreatEq : tokenize cs
   | otherwise = TokOpRel Greater : tokenize (c : cs)

diffOr :: String -> [Token]
diffOr (c : cs)
   | c == '=' = TokOpRel Diff : tokenize cs
   | otherwise = error $ "Erro léxico no sinal de = " ++ [c]



tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : cs) 
    | c `elem` "+-*/^" = TokOp (operator c) : tokenize cs
    | c == ','  = TokVirgula : tokenize cs
    | c == '.'  = TokPonto : tokenize cs
    | c == '<'  = lessOr cs
    | c == '>'  = greatOr cs
    | c == '!'  = diffOr cs
    | c == '='  = equOr cs
    | c == ':'  = assign cs
    | c == '('  = TokParenE : tokenize cs
    | c == ')'  = TokParenD : tokenize cs
    | c == '{'  = TokColchE : tokenize cs
    | c == '}'  = TokColchD : tokenize cs
    | c == '"'  = TokAspasA : textVal cs
    | isDigit c = number c cs
    | isAlpha c = reserved c (c : cs)
    | isSpace c = tokenize cs
    | otherwise = error $ "Erro Léxico em " ++ [c]

identifier :: Char -> String -> [Token]
identifier c cs = let (name, cs') = span isAlphaNum cs in
                  TokIdent (c:name) : tokenize cs'

textVal :: String -> [Token]
textVal cs = let (text, cs') = span (/= '"') cs in
             TokText text : TokAspasF : tokenize (drop 1 cs')

reserved :: Char -> String -> [Token]
reserved c cs 
   | "programa" `isPrefixOf` cs = TokProg : tokenize (drop 8 cs)
   | "fimprog" `isPrefixOf` cs = TokFim : tokenize (drop 7 cs)
   | "declarenum" `isPrefixOf` cs = TokDeclaraNum : tokenize (drop 10 cs)
   | "declaretxt" `isPrefixOf` cs = TokDeclaraTxt : tokenize (drop 10 cs)
   | "leia" `isPrefixOf` cs = TokLeia : tokenize (drop 4 cs)
   | "escreva" `isPrefixOf` cs = TokEscreva : tokenize (drop 7 cs)
   | "se " `isPrefixOf` cs = TokSe : tokenize (drop 3 cs)
   | "entao" `isPrefixOf` cs = TokEntao : tokenize (drop 5 cs)
   | "senao" `isPrefixOf` cs = TokSenao : tokenize (drop 5 cs)
   | "enquanto" `isPrefixOf` cs = TokEnquanto : tokenize (drop 8 cs)
   | "faça" `isPrefixOf` cs = TokFaca : tokenize (drop 4 cs)
   | "rzqd" `isPrefixOf` cs = TokOp Sqrt : tokenize (drop 4 cs)
   | "log2" `isPrefixOf` cs = TokOp Log2 : tokenize (drop 4 cs)
   | "log10" `isPrefixOf` cs = TokOp Log10 : tokenize (drop 5 cs)
   | "log" `isPrefixOf` cs = TokOp Log : tokenize (drop 3 cs)
   | otherwise = identifier c (drop 1 cs)

number :: Char -> String -> [Token]
number c cs = 
   let (digs, cs') = span isDigit cs in
      if head cs' == '.' && isDigit (cs' !! 1) then
         let (digs', cs'') = span isDigit (tail cs') 
             numero = (c : digs)
             decimal = ('.' : digs') in
             TokNum (numero ++ decimal) : tokenize cs''
      else
         TokNum (c : digs) : tokenize cs'
