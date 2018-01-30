module Main where

import Data.Semigroup((<>))
import Options.Applicative

import Lib


data Args = Args
  { len       :: Bool
  , otherArgs :: [String] }

parseLen :: Parser Bool
parseLen = switch
      (  long "length"
        <> short 'l'
        <> help "Print length of arguments instead of arguments themselves")

parseOtherArgs :: Parser [String]
parseOtherArgs = many (argument str (metavar "ARGS..."))

parseArgs :: Parser Args
parseArgs = Args <$> parseLen <*> parseOtherArgs

parseArgInfo :: ParserInfo Args
parseArgInfo = info (parseArgs <**> helper)
  (  fullDesc
  <> progDesc "A compiler written in Haskell for CSC-312 during 2018S"
  <> header   "GrinHC: A compiler written in Haskell at Grinnell College" )

main :: IO ()
main = do
  options <- execParser parseArgInfo
  case options of
    Args True xs  -> mapM_ (print . length) xs
    Args False xs -> mapM_ putStrLn xs
