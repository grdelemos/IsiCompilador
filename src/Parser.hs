module Parser where

import Data.Char
import Data.List
import Lexer

---- Parser ----

data Tree = NoProg [Tree] [Tree]
            | NoDeclara [Tree]
            | NoDeclaraNum [Tree]
            | NoDeclaraTxt [Tree]
            | NoBloco [Tree]
            | NoLeitura Tree
            | NoEscrita Tree
            | NoId String
            | NoIdParen String
            | NoTexto String
            | Noexpr Tree Tree
            | NoSoma Operator Tree Tree 
            | NoSomaParen Operator Tree Tree
            | NoPow Tree Tree
            | NoPowParen Tree Tree
            | NoOpAdv Operator Tree
            | NoAssign String Tree
            | NoAssignTxt String Tree
            | NoNum String
            | NoUnario Operator Tree
            | NoUnarioParen Operator Tree
            | NoProd Operator Tree Tree
            | NoProdParen Operator Tree Tree
            | NoSe Tree OpRel Tree Tree (Maybe Tree)
            | NoEntao [Tree]
            | NoSenao [Tree]
            | NoEnquanto Tree OpRel Tree Tree
            | NoFaca [Tree]
    deriving (Show, Eq, Read)

programa :: [Token] -> (Tree, [Token])
programa toks
   | head toks /= TokProg = error "Faltando a palavra 'programa'"
   | last toks /= TokPonto = error "Faltando ponto no final do programa"
   | last (init toks) /= TokFim = error "Faltando a palavra fimprog"
   | otherwise = 
      let progtokens = (init $ tail toks)
          (declaraTree, toks') = declaraOr [] progtokens
          (blocoTree, toks'') = blocos [] toks'
          in
            if head progtokens /= TokDeclaraNum && head progtokens /= TokDeclaraTxt then error "Faltando a palavra 'declare'" else
            (NoProg declaraTree  blocoTree, toks'')


declaraOr :: [Tree] -> [Token] -> ([Tree], [Token])
declaraOr ts toks
   | head toks == TokDeclaraNum = let (numTree, toks') = ident [] (tail toks) in
                                    declaraOr (ts ++ [NoDeclaraNum numTree]) toks'
   | head toks == TokDeclaraTxt = let (txtTree, toks'') = ident [] (tail toks) in
                                    declaraOr (ts ++ [NoDeclaraTxt txtTree]) toks''
   | otherwise = ([NoDeclara ts], toks)
                             

ident :: [Tree] -> [Token] -> ([Tree], [Token])
ident ts toks =
   case head toks of 
      (TokIdent x) -> ident (ts ++ [NoId x]) (tail toks)
      TokVirgula -> ident ts (tail toks)
      TokPonto -> (ts, tail toks)

blocos :: [Tree] -> [Token] -> ([Tree], [Token])
blocos ts toks = let (blocoTree, toks') = comandos ts toks in
                  ([NoBloco blocoTree], drop 1 toks')
                  
comandos :: [Tree] -> [Token] -> ([Tree], [Token])
comandos ts toks =
   case head toks of
      TokColchD ->
         (ts, tail toks)
      TokFim -> 
         (ts, toks)
      TokLeia -> 
         let (leiTree, toks') = leitura ts (tail toks) in
            comandos leiTree toks'
      TokEscreva ->
         let (escTree, toks'') = escrita ts (tail toks) in
            comandos escTree toks''
      TokIdent _ ->
         let (expTree, toks2) = retornaExpressao ts toks in
            comandos expTree toks2
      TokSe ->
         let (seTree, toks3) = comandoSe ts (tail toks) in
            comandos seTree toks3
      TokEnquanto ->
         let (enquantoTree, toks4) = comEnquanto ts (tail toks) in
            comandos enquantoTree toks4
      _ -> error $ "Erro de sintaxe" ++ show toks
              
                     

leitura :: [Tree] -> [Token] -> ([Tree], [Token])
leitura ts toks
   | head toks /= TokParenE = error "Faltando parenteses depois do leia"
   | (toks !! 2) /= TokParenD = error "Faltando fechar parenteses"
   | (toks !! 3) /= TokPonto = error "Faltando ponto no leia"
   | otherwise = case head $ tail toks of 
                     (TokIdent x) -> (ts ++ [NoLeitura (NoId x)], drop 4 toks)
                     _            -> error "Deve ter um identificador no leia"

escrita :: [Tree] -> [Token] -> ([Tree], [Token])
escrita ts toks
   | head toks /= TokParenE = error "Faltando parenteses depois do escreva"
   | otherwise = case toks !! 1 of
                     (TokIdent x) ->
                        if (toks !! 2) /= TokParenD then error "Faltando fechar parenteses" else
                           if (toks !! 3) /= TokPonto then error "Faltando ponto no escreva" else
                        (ts ++ [NoEscrita (NoId x)], drop 4 toks)
                     TokAspasA ->
                        case toks !! 2 of
                           (TokText x) ->
                               if (toks !! 3) /= TokAspasF then error "Faltando fechar aspas" else
                                 if (toks !! 4) /= TokParenD then error "Faltando fechar parenteses" else
                                    if (toks !! 5) /= TokPonto then error "Faltando ponto no escreva" else
                              (ts ++ [NoEscrita (NoTexto x)], drop 6 toks)

retornaExpressao :: [Tree] -> [Token] -> ([Tree], [Token])
retornaExpressao ts toks = let (tree, toks') = expressao toks in
   if head toks' /= TokPonto then error $ "Faltando ponto" ++ show toks' else
   (ts ++ [tree], tail toks')


expressao :: [Token] -> (Tree, [Token])
expressao toks =
   let (termTree, toks') = term toks
   in
      case head toks' of
         (TokOp op) | op `elem` [Plus, Minus] -> 
            let (exTree, toks'') = expressao (tail toks') 
            in (NoSoma op termTree exTree, toks'')
         TokOp Pow ->
            let (exTree, toks'') = expressao (tail toks') 
            in (NoPow termTree exTree, toks'')
         TokAssign ->
            case termTree of
               NoId str -> 
                  let (exTree, toks'') = expressao (tail toks') 
                  in 
                     case exTree of 
                        NoTexto x -> (NoAssignTxt str exTree, toks'')
                        _ -> (NoAssign str exTree, toks'')
               _ -> error "Falta um identificador na atribuição"
         _ -> (termTree, toks')

expressaoParen :: [Token] -> (Tree, [Token])
expressaoParen toks =
   let (termTree, toks') = termParen toks
   in
      case head toks' of
         (TokOp op) | op `elem` [Plus, Minus] -> 
            let (exTree, toks'') = expressao (tail toks') 
            in (NoSomaParen op termTree exTree, toks'')
         TokOp Pow ->
            let (exTree, toks'') = expressao (tail toks') 
            in (NoPowParen termTree exTree, toks'')
         _ -> (termTree, toks')

term :: [Token] -> (Tree, [Token])
term toks = 
   let (facTree, toks') = fator toks
   in
      case head toks' of
         (TokOp op) | op `elem` [Times, Div] ->
            let (termTree, toks'') = term (tail toks') 
            in (NoProd op facTree termTree, toks'')
         _ -> (facTree, toks')

termParen :: [Token] -> (Tree, [Token])
termParen toks = 
   let (facTree, toks') = fatorParen toks
   in
      case head toks' of
         (TokOp op) | op `elem` [Times, Div] ->
            let (termTree, toks'') = term (tail toks') 
            in (NoProdParen op facTree termTree, toks'')
         _ -> (facTree, toks')

fator :: [Token] -> (Tree, [Token])
fator toks = 
   case head toks of
      (TokNum x)     -> (NoNum x, tail toks)
      (TokIdent str) -> (NoId str, tail toks)
      (TokOp op) | op `elem` [Plus, Minus] -> 
            let (facTree, toks') = fator (tail toks) 
            in (NoUnario op facTree, toks')
      TokOp Sqrt ->
         if toks !! 1 /= TokParenE then error "Falta abrir parêntese depois de rzqd" else
         let (expTree, toks') = expressao (drop 2 toks)
         in
            if head toks' /= TokParenD then error "Falta fechar parêntese depois de rzqd" else
            (NoOpAdv Sqrt expTree, tail toks')
      TokOp Log ->
         if toks !! 1 /= TokParenE then error "Falta abrir parêntese depois de log" else
         let (expTree, toks') = expressao (drop 2 toks)
         in
            if head toks' /= TokParenD then error "Falta fechar parêntese depois de log" else
            (NoOpAdv Log expTree, tail toks')
      TokOp Log2 ->
         if toks !! 1 /= TokParenE then error "Falta abrir parêntese depois de log2" else
         let (expTree, toks') = expressao (drop 2 toks)
         in
            if head toks' /= TokParenD then error "Falta fechar parêntese depois de log2" else
            (NoOpAdv Log2 expTree, tail toks')
      TokOp Log10 ->
         if toks !! 1 /= TokParenE then error "Falta abrir parêntese depois de log10" else
         let (expTree, toks') = expressao (drop 2 toks)
         in
            if head toks' /= TokParenD then error "Falta fechar parêntese depois de log10" else
            (NoOpAdv Log10 expTree, tail toks')
      TokParenE -> 
         let (expTree, toks') = expressaoParen (tail toks)
         in
            if head toks' /= TokParenD 
            then error "Falta fechar parênteses"
            else (expTree, tail toks')
      TokAspasA ->
         if (toks !! 2) /= TokAspasF then error "Faltando fechar aspas" else
            if (toks !! 3) /= TokPonto then error "Faltando ponto depois de atribuição de texto" else
               case toks !! 1 of (TokText x) -> (NoTexto x, drop 3 toks)
      _ -> error $ "Erro Léxico em: " ++ show toks

fatorParen :: [Token] -> (Tree, [Token])
fatorParen toks = 
   case head toks of
      (TokNum x)     -> (NoNum x, tail toks)
      (TokIdent str) -> (NoIdParen str, tail toks)
      (TokOp op) | op `elem` [Plus, Minus] -> 
            let (facTree, toks') = fator (tail toks) 
            in (NoUnarioParen op facTree, toks')
      TokOp Sqrt ->
         if toks !! 1 /= TokParenE then error "Falta abrir parêntese depois de rzqd" else
         let (expTree, toks') = expressao (drop 2 toks)
         in
            if head toks' /= TokParenD then error "Falta fechar parêntese depois de rzqd" else
            (NoOpAdv Sqrt expTree, tail toks')
      TokOp Log ->
         if toks !! 1 /= TokParenE then error "Falta abrir parêntese depois de log" else
         let (expTree, toks') = expressao (drop 2 toks)
         in
            if head toks' /= TokParenD then error "Falta fechar parêntese depois de log" else
            (NoOpAdv Log expTree, tail toks')
      TokOp Log2 ->
         if toks !! 1 /= TokParenE then error "Falta abrir parêntese depois de log2" else
         let (expTree, toks') = expressao (drop 2 toks)
         in
            if head toks' /= TokParenD then error "Falta fechar parêntese depois de log2" else
            (NoOpAdv Log2 expTree, tail toks')
      TokOp Log10 ->
         if toks !! 1 /= TokParenE then error "Falta abrir parêntese depois de log10" else
         let (expTree, toks') = expressao (drop 2 toks)
         in
            if head toks' /= TokParenD then error "Falta fechar parêntese depois de log10" else
            (NoOpAdv Log10 expTree, tail toks')
      TokParenE-> 
         let (expTree, toks') = expressaoParen (tail toks)
         in
            if head toks' /= TokParenD 
            then error "Falta fechar parênteses"
            else (expTree, tail toks')
      _ -> error $ "Erro Léxico em: " ++ show toks

comandoSe :: [Tree] -> [Token] -> ([Tree], [Token])
comandoSe ts toks  
   | head toks /= TokParenE = error "Faltando abrir parênteses"
   | otherwise = let (tree1, toks') = expressao (tail toks) in
      case head toks' of 
         (TokOpRel op) | op `elem` [Greater, Less, Equal, GreatEq, LessEq, Diff] ->
            let (tree2, toks'') = expressao (tail toks') in
               if head toks'' /= TokParenD then error "Faltou fechar parenteses" else
               case toks'' !! 1 of 
                  TokEntao ->
                     let (treeEntao, toks2) = comandoSEntao (tail toks'') in
                        case head toks2 of
                           TokSenao -> 
                              let (treeSenao, toks3) = comandoSEntao toks2 in
                                (ts ++ [NoSe tree1 op tree2 treeEntao (Just treeSenao)], toks3)
                           _ -> (ts ++ [NoSe tree1 op tree2 treeEntao Nothing], toks2)
                  _ -> error $ "erro de sintaxe" ++ show toks''

comandoSEntao :: [Token] -> (Tree, [Token])
comandoSEntao toks
   | toks !! 1 /= TokColchE = error $ "Faltando abrir colchetes" ++ show toks
   | otherwise = let (tree, toks') = comandos [] (drop 2 toks) in
                     case head toks of
                        TokEntao -> (NoEntao tree, toks')
                        TokSenao -> (NoSenao tree, toks')
                        _ -> error $ "Erro de sintaxe: " ++ show toks'

comEnquanto :: [Tree] -> [Token] -> ([Tree], [Token])
comEnquanto ts toks 
   | head toks /= TokParenE = error $ "Faltando abrir parênteses" ++ show toks
   | otherwise = let (tree1, toks') = expressao (tail toks) in
      case head toks' of 
         (TokOpRel op) | op `elem` [Greater, Less, Equal, GreatEq, LessEq, Diff] ->
            let (tree2, toks'') = expressao (tail toks') in
               if head toks'' /= TokParenD then error "Faltou fechar parenteses" else
               case toks'' !! 1 of 
                  TokFaca ->
                     if toks'' !! 2 /= TokColchE then error "Faltou abrir colchetes" else
                     let (treeEnquanto, toks2) = comandos [] (drop 3 toks'') in
                        (ts ++ [NoEnquanto tree1 op tree2 (NoFaca treeEnquanto)], toks2)
                  _ -> error $ "erro de sintaxe" ++ show toks''


parse :: [Token] -> Tree
parse toks = let (tree, toks') = programa toks
             in
               if null toks' 
               then tree
               else error $ "Erro de sintaxe: " ++ show toks'