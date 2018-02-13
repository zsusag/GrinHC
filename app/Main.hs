{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Main where

import Data.Semigroup((<>))
import Options.Applicative
import Control.Monad
import System.IO
import System.Directory
import System.Exit

import Lexer
import Parser
import Eval

data Args = Args
  { file :: FilePath
  , printTS :: Bool
  , printAST :: Bool
  }

parseLex :: Parser Bool
parseLex = switch
  (long "lex"
   <> short 'l'
   <> help "Print the token stream instead of evaluating code")

parseParse :: Parser Bool
parseParse = switch
  (long "parse"
  <> short 'p'
  <> help "Print the AST instead of evaluating code")

parseFilePath :: Parser FilePath
parseFilePath = argument str (metavar "FILE")

parseArgs :: Parser Args
parseArgs = Args <$> parseFilePath <*> parseLex <*> parseParse

parseArgInfo :: ParserInfo Args
parseArgInfo = info (parseArgs <**> helper)
  (  fullDesc
  <> progDesc "A compiler written in Haskell for CSC-312 during 2018S"
  <> header   "GrinHC: A compiler written in Haskell at Grinnell College" )

main :: IO ()
main = do
   (Args file printTS printAST) <- execParser parseArgInfo
   handle <- tryOpen file
   contents <- hGetContents handle
   let tokenStream = alexScanTokens contents
   printAndExit printTS tokenStream handle
   let ast = Parser.parse tokenStream
   printAndExit printAST ast handle
   putStrLn $ evaluate ast
   hClose handle
     where
       tryOpen filePath = case filePath of
         [] -> errorWithoutStackTrace ("Could not open file: File does not exist: " ++ filePath)
         _  -> do
           exists <- doesFileExist filePath
           if exists
             then openFile filePath ReadMode
             else errorWithoutStackTrace ("Could not open file: File does not exist: " ++ filePath)
       printAndExit b ts handle = when b (do print ts
                                             hClose handle
                                             exitSuccess)
    
