module Error where

import Lang (Pos(..))

posError :: Pos -> String -> String -> a
posError (line,col) errortype msg = errorWithoutStackTrace posMsg
  where
    posMsg = errortype ++ " at (line " ++ show line ++ ", column " ++ show col ++ ")" ++ msg
