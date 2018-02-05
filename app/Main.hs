module Main where

import Data.Semigroup((<>))
import Options.Applicative
import System.IO
import System.Directory

import Lexer
import Parser
import AST

newtype Args = Args
  { file      :: FilePath }

parseLen :: Parser Bool
parseLen = switch
      (  long "length"
        <> short 'l'
        <> help "Print length of arguments instead of arguments themselves")

parseFilePath :: Parser FilePath
parseFilePath = argument str (metavar "FILE")

parseArgs :: Parser Args
parseArgs = Args <$> parseFilePath

parseArgInfo :: ParserInfo Args
parseArgInfo = info (parseArgs <**> helper)
  (  fullDesc
  <> progDesc "A compiler written in Haskell for CSC-312 during 2018S"
  <> header   "GrinHC: A compiler written in Haskell at Grinnell College" )

main :: IO ()
main = do
  options <- execParser parseArgInfo
  case options of
    Args filePath -> do
      handle <- tryOpen filePath
      contents <- hGetContents handle
      tokenStream <- lexString contents
      ast <- parseTokenStream tokenStream
      print $ eval ast
      hClose handle
    where tryOpen filePath = case filePath of
            [] -> error ("Could not open file: File does not exist: " ++ filePath)
            _  -> do
              exists <- doesFileExist filePath
              if exists
                then openFile filePath ReadMode
                else error ("Could not open file: File does not exist: " ++ filePath)
