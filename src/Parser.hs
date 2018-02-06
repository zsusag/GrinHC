module Parser where

import Token
import AST

parseTokenStream :: [Token] -> IO Exp
parseTokenStream ts = let (ast,ts') = parseTokenStreamH ts
                      in case ts' of
                           [] -> return ast
                           _  -> error ("Error during parsing. Unable to parse " ++ show ts')


parseTokenStreamH :: [Token] -> (Exp, [Token])
parseTokenStreamH (TokenLParen:TokenIf:ts) = let (e1, ts') = parseTokenStreamH ts
                                                 (e2, ts'') = parseTokenStreamH ts'
                                                 (e3, ts''') = parseTokenStreamH ts''
  in case ts''' of
       (TokenRParen:ts'''') -> (EIf e1 e2 e3, ts'''')
       _                    -> error "Parenthesis mismatch"
parseTokenStreamH (TokenLParen:ts) = let (e1,ts') = parseTokenStreamH $ tail ts
                                         (e2,ts'') = parseTokenStreamH ts'
  in case ts'' of
       (TokenRParen:ts''') -> case head ts of
                                TokenPlus -> (EAdd e1 e2, ts''')
                                TokenSub  -> (ESub e1 e2, ts''')
                                TokenMult -> (EMul e1 e2, ts''')
                                TokenDiv  -> (EDiv e1 e2, ts''')
                                TokenLEQ  -> (ELeq e1 e2, ts''')
                                t         -> error ("Operator not supported: " ++ show t)
       _                   -> error "Parentheses mismatch."
parseTokenStreamH (TokenInt x:ts) = (EInt x, ts)
parseTokenStreamH (TokenBool True:ts) = (EBool True, ts)
parseTokenStreamH (TokenBool False:ts) = (EBool False, ts)
parseTokenStreamH ts = error ("Improper syntax: " ++ show ts)
