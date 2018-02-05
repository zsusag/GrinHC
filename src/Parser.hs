module Parser where

import Token
import AST

parseTokenStream :: [Token] -> IO (Exp Int)
parseTokenStream ts = let (ast,ts') = parseTokenStreamH ts
                      in case ts' of
                           [] -> return ast
                           _  -> error ("Error during parsing. Unable to parse " ++ show ts')


parseTokenStreamH :: [Token] -> (Exp Int, [Token])
parseTokenStreamH (TokenLParen:TokenPlus:ts) = let (e1,ts') = parseTokenStreamH ts
                                                   (e2,ts'') = parseTokenStreamH ts'
  in case ts'' of
       (TokenRParen:ts''') -> (Add e1 e2, ts''')
       _                   -> error "Parentheses mismatch."
parseTokenStreamH (TokenInt x:ts) = (LitInt x, ts)
parseTokenStreamH _ = error "Improper syntax."


