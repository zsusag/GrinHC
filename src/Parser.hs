module Parser where

import Token
import AST

parseTokenStream :: [Token] -> IO (Exp Int)
parseTokenStream ts = let (ast,ts') = parseTokenStreamH ts
                      in case ts' of
                           [] -> return ast
                           _  -> error ("Error during parsing. Unable to parse " ++ show ts')


parseTokenStreamH :: [Token] -> (Exp Int, [Token])
parseTokenStreamH (TokenLParen:ts) = let (e1,ts') = parseTokenStreamH $ tail ts
                                         (e2,ts'') = parseTokenStreamH ts'
  in case ts'' of
       (TokenRParen:ts''') -> case head ts of
                                TokenPlus -> (Add e1 e2, ts''')
                                TokenSub  -> (Sub e1 e2, ts''')
                                TokenMult -> (Mul e1 e2, ts''')
                                TokenDiv  -> (Div e1 e2, ts''')
                                t         -> error ("Operator not supported: " ++ show t)
       _                   -> error "Parentheses mismatch."
parseTokenStreamH (TokenInt x:ts) = (LitInt x, ts)
parseTokenStreamH _ = error "Improper syntax."


