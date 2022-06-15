module Main where

import qualified Data.ByteString as ByteString
import Data.Numeric.Array.Simple (decodeArray)
import Options.Applicative as Options
import System.Exit (die)

data Args = Read FilePath

argsParser :: Options.Parser Args
argsParser = Read <$> Options.strArgument (Options.metavar "array")

argsInfo :: Options.ParserInfo Args
argsInfo = Options.info (argsParser <**> Options.helper) mempty

main :: IO ()
main = do
  Read file <- Options.execParser argsInfo
  content <- ByteString.readFile file
  case decodeArray content of
    Left err -> die err
    Right a -> print a
