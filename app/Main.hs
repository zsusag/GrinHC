{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds #-}
module Main where

import Data.Semigroup((<>))
import qualified Data.Map.Strict as Map
import Control.DeepSeq
import Options.Applicative
import Control.Monad
import System.IO
import System.Directory
import System.Exit

import Lexer
import Parser
import Eval
import Typecheck

data Args = Args
  { file :: FilePath
  , printTS :: Bool
  , printAST :: Bool
  , printSteps :: Bool
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

parseStep :: Parser Bool
parseStep = switch
  (long "step"
  <> short 's'
  <> help "Print out every step of evaluation")

parseFilePath :: Parser FilePath
parseFilePath = argument str (metavar "FILE")

parseArgs :: Parser Args
parseArgs = Args <$> parseFilePath <*> parseLex <*> parseParse <*> parseStep

parseArgInfo :: ParserInfo Args
parseArgInfo = info (parseArgs <**> helper)
  (  fullDesc
  <> progDesc "A compiler written in Haskell for CSC-312 during 2018S"
  <> header   "GrinHC: A compiler written in Haskell at Grinnell College" )

main :: IO ()
main = do
   (Args file printTS printAST printSteps) <- execParser parseArgInfo
   handle <- tryOpen file
   contents <- hGetContents handle
   if null contents then
     errorWithoutStackTrace "Error: File is empty"
     else do 
     let tokenStream = alexScanTokens contents
     printAndExit printTS tokenStream handle
     let prog@(decl,exp) = Parser.parse tokenStream
     printAndExit printAST exp handle
     typ <- typecheck prog
     typ `deepseq` evaluate exp printSteps (0,Map.empty)
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
