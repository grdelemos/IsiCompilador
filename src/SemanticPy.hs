module SemanticPy where
import Data.Char
import Data.List
import Lexer
import Parser


data Var =  Var { nome :: String , tipo :: String, utilizada :: Bool} deriving (Show, Eq, Read)

compatVar :: String -> String -> [Var] -> Bool
compatVar nome' tipo' symTab =
    let variavel = filter (\x -> nome x == nome' && tipo x == tipo') symTab in
        not (null variavel)
      
foiDeclarada :: String -> [Var] -> Bool
foiDeclarada nome' symTab =
    let variavel = filter (\x -> nome x == nome') symTab in
        not (null variavel)

atualizaSymTab :: String -> [Var] -> [Var]
atualizaSymTab nome' symTab =
  let 
    variavel = filter (\x -> nome x == nome') symTab
    tipo' = tipo (head variavel)
    symTab' = delete (head variavel) symTab in
      symTab' ++ [Var nome' tipo' True]



semanticPy :: Tree -> [Var] -> (String, String)

semanticPy (NoProg declara blocos) symTab = 
   let 
    
    (declr, symTab') =  declarar declara symTab
    (blc, symTab'') = avaliaBloco blocos symTab'
    in
    --error $ "erro" ++ show symTab''
    
    ("import math\n" ++ declr ++ blc, warnings symTab'')

warnings :: [Var] -> String
warnings [] = []

warnings symTab = 
    let nusadas = filter (not . utilizada) symTab in
        if null nusadas then [] else
            let var = nome (head nusadas) in
                "\nAVISO: A variável " ++ var ++ " foi declarada, mas não foi usada\n" ++ warnings (tail nusadas)




declarar :: [Tree] -> [Var] -> (String, [Var])
declarar [] symTab = ([], symTab)

declarar (t : ts) symTab =
    case t of 
        (NoDeclaraNum x) ->
            let (num, symTab') = declararNumero x symTab
                (prox, symTab'') = declarar ts symTab' in
              (num ++ prox, symTab'') 
        (NoDeclaraTxt x) ->
            let (txt, symTab') = declararTexto x symTab 
                (prox', symTab'') = declarar ts symTab' in
              (txt ++ prox', symTab'')
        (NoDeclara var) ->
            declarar var symTab


declararNumero ::[Tree] -> [Var] -> (String, [Var])
declararNumero [] symTab = ([], symTab)

declararNumero (t : ts) symTab =
    case t of NoId a ->
                        let (prox, symTab') = declararNumero ts symTab 
                            declarada = symTab' ++ [Var {nome = a, tipo = "num", utilizada = False}] in
                        if foiDeclarada a symTab' then error $ "Variável " ++ show a ++ " declarada mais de uma vez" else
                        if null prox then (a ++ " = 0\n", declarada) else (a ++ " = " ++ prox, declarada)

declararTexto ::[Tree] -> [Var] -> (String, [Var])
declararTexto [] symTab = ([], symTab)

declararTexto (t : ts) symTab =
    case t of NoId a ->
                        let (prox, symTab') = declararTexto ts symTab
                            declarada = symTab' ++ [Var {nome = a, tipo = "txt", utilizada = False}] in
                        if foiDeclarada a symTab' then error $ "Variável " ++ show a ++ " declarada mais de uma vez" else
                        if null prox then (a ++ " = \"String\"\n", declarada) else (a ++ " = " ++ prox, declarada)

avaliaBloco :: [Tree] -> [Var] -> (String, [Var])
avaliaBloco [] symTab = ([], symTab)

avaliaBloco (t : ts) symTab =
    case t of 
        NoBloco blc ->
                        avaliaBloco blc symTab
        NoEscrita x ->
                        let   
                            (print, symTab') = avaliaPrint x symTab
                            (bloco, symTab'') = avaliaBloco ts symTab' 
                            in
                                (print ++ bloco, symTab'')
        NoLeitura x ->
                        let
                            (leitura, symTab') = avaliaEntrada x symTab
                            (bloco, symTab'') = avaliaBloco ts symTab'
                            in
                                (leitura ++ bloco, symTab'')
        NoAssign var tree ->
                        let
                            (atrib, symTab') = avalAssign var tree symTab
                            (bloco, symTab'') = avaliaBloco ts symTab'
                            in
                                (atrib ++ bloco, symTab'')
        NoAssignTxt var tree ->
                        let
                            (atrib, symTab') = avalAssignTxt var tree symTab
                            (bloco, symTab'') = avaliaBloco ts symTab'
                            in
                                (atrib ++ bloco, symTab'')
        NoSe expr1 op expr2 entao senao ->
                        let
                            (bloSe, symTab') = avalSe t symTab
                            (bloco, symTab'') = avaliaBloco ts symTab'
                            in
                                (bloSe ++ bloco, symTab'')
        NoEnquanto expr1 op expr2 faca ->
                        let
                            (bloEnquanto, symTab') = avalEnquanto t symTab
                            (bloco, symTab'') = avaliaBloco ts symTab'
                            in
                                (bloEnquanto ++ bloco, symTab'')

              


avaliaPrint :: Tree -> [Var] -> (String, [Var])
avaliaPrint t symTab = 
  case t of (NoId a) ->  if not (foiDeclarada a symTab) then error $ "Variável " ++ show a ++ " não foi declarada" else
                        ("print (" ++ a ++ ")\n", atualizaSymTab a symTab)
            (NoTexto x) -> ("print (\"" ++ x ++  "\")\n", symTab)

avaliaEntrada :: Tree -> [Var] -> (String, [Var]) 
avaliaEntrada t symTab = 
    case t of 
        (NoId a) -> 
            if not (foiDeclarada a symTab) then error $ "Variável " ++ show a ++ " não foi declarada" else
                let variavel = filter (\x -> nome x == a) symTab
                    tipo' = tipo (head variavel) in
                        case tipo' of
                            "num" -> (a ++ " = float (input())\n", atualizaSymTab a symTab)
                            "txt" -> (a ++ " = (input())\n", atualizaSymTab a symTab)

avalAssign :: String -> Tree -> [Var] -> (String, [Var])
avalAssign var ts symTab =
    let (expressao, symTab') = avalExpress ts symTab in
    if not (foiDeclarada var symTab) then error $ "Variável " ++ show var ++ " não foi declarada" else
        if not (compatVar var "num" symTab) then error $ "Variável " ++ show var ++ " não é compatível com o tipo num" else
            (var ++ " = " ++ expressao ++ "\n", atualizaSymTab var symTab')

avalAssignTxt :: String -> Tree -> [Var] -> (String, [Var])
avalAssignTxt var ts symTab =
    let (expressao, symTab') = avalExpress ts symTab in
    if not (foiDeclarada var symTab) then error $ "Variável " ++ show var ++ " não foi declarada" else
        if not (compatVar var "txt" symTab) then error $ "Variável " ++ show var ++ " não é compatível com o tipo txt" else
            (var ++ " = " ++ expressao ++ "\n", atualizaSymTab var symTab')

avalExpress :: Tree -> [Var] -> (String, [Var])
avalExpress expr symTab = 
    case expr of
        NoTexto x -> ("\"" ++ x ++ "\"", symTab)
        NoNum x -> (x, symTab)
        NoId x -> (x, atualizaSymTab x symTab)
        NoIdParen x -> ("(" ++ x ++ ")", atualizaSymTab x symTab)
        NoSoma op exp exp' ->
            let operador = if op == Plus then " + " else " - " 
                (expressao, symTab') = avalExpress exp symTab
                (expressao', symTab'') = avalExpress exp' symTab' in
                    (expressao ++ operador ++ expressao', symTab'')
        NoProd op exp exp' ->
            let operador = if op == Times then " * " else " / " 
                (expressao, symTab') = avalExpress exp symTab
                (expressao', symTab'') = avalExpress exp' symTab' in
                    (expressao ++ operador ++ expressao', symTab'')
        NoUnario op exp ->
            let operador = if op == Plus then "+" else "-" 
                (expressao, symTab') = avalExpress exp symTab in
                    (operador ++ expressao, symTab')
        NoSomaParen op exp exp' ->
            let operador = if op == Plus then " + " else " - " 
                (expressao, symTab') = avalExpress exp symTab
                (expressao', symTab'') = avalExpress exp' symTab' in
                    ("(" ++ expressao ++ operador ++ expressao' ++ ")", symTab'')
        NoProdParen op exp exp' ->
            let operador = if op == Times then " * " else " / " 
                (expressao, symTab') = avalExpress exp symTab
                (expressao', symTab'') = avalExpress exp' symTab' in
                    ("(" ++ expressao ++ operador ++ expressao' ++ ")", symTab'')
        NoUnarioParen op exp ->
            let operador = if op == Plus then "+" else "-" 
                (expressao, symTab') = avalExpress exp symTab in
                    ("(" ++ operador ++ expressao ++ ")", symTab')
        NoPow exp exp' ->
            let (expressao, symTab') = avalExpress exp symTab
                (expressao', symTab'') = avalExpress exp' symTab' in
                    (expressao ++ " ** " ++ expressao', symTab'')
        NoPowParen exp exp' ->
            let (expressao, symTab') = avalExpress exp symTab
                (expressao', symTab'') = avalExpress exp' symTab' in
                    ("(" ++ expressao ++ " ** " ++ expressao' ++ ")", symTab'')
        NoOpAdv op exp ->
            let operador = case op of
                                Sqrt -> "math.sqrt("
                                Log -> "math.log("
                                Log2 -> "math.log2("
                                Log10 -> "math.log10("
                (expressao, symTab') = avalExpress exp symTab
                 in
                    (operador ++ expressao ++ ")" , symTab')


avalSe :: Tree -> [Var] -> (String, [Var])
avalSe blocoSe symTab =
    case blocoSe of
        NoSe expr1 op expr2 entao senao ->
            let (expr', symTab1) = avalExpress expr1 symTab
                operador = case op of
                    Greater -> " > "
                    Less    -> " < "
                    Equal   -> " == "
                    GreatEq -> " >= "
                    LessEq  -> " <= "
                    Diff    -> " != "
                (expr'', symTab2) = avalExpress expr2 symTab1
                (blocEntao, symTab3) = case entao of NoEntao x -> avaliaBloco x symTab2
                (blocSenao, symTab4) = if senao == Nothing then (" ", symTab3) else
                    case senao of
                        Just (NoSenao x) -> avaliaBloco x symTab3
            in
                let blocElse = if blocSenao == " " then "" else "else:\n" ++ (unlines . map ('\t' :) . lines) blocSenao in
                ("if " ++ expr' ++ operador ++ expr'' ++ " :\n" ++ (unlines . map ('\t' :) . lines) blocEntao ++ blocElse, symTab4)

avalEnquanto :: Tree -> [Var] -> (String, [Var])
avalEnquanto blocoEnquanto symTab =
    case blocoEnquanto of
        NoEnquanto expr1 op expr2 faca ->
            let (expr', symTab1) = avalExpress expr1 symTab
                operador = case op of
                    Greater -> " > "
                    Less    -> " < "
                    Equal   -> " == "
                    GreatEq -> " >= "
                    LessEq  -> " <= "
                    Diff    -> " != "
                (expr'', symTab2) = avalExpress expr2 symTab1
                (blocfaca, symTab3) = case faca of NoFaca x -> avaliaBloco x symTab2
            in
                ("while " ++ expr' ++ operador ++ expr'' ++ " :\n" ++ (unlines . map ('\t' :) . lines) blocfaca, symTab3)