{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Data.Text.IO as T
import Language.Python.Common
import Language.Python.Version3 as V3
import System.Environment
import ToHaskell.Pure
-- import Text.Pretty.Simple (pPrint)

data PythonVersion = Two | Three
  deriving (Eq, Show)

type Parser = String -> String -> Either ParseError (ModuleSpan, [Token])

main :: IO ()
main = do
  args <- getArgs
  case args of
    (inFile : _rest) -> do
      contents <- readFile inFile
      case parseAndPretty V3.parseModule inFile contents of
        Left error' -> putStrLn $ prettyText error'
        Right ast -> do
--           pPrint ast
          toHaskell' ast >>= T.putStrLn
          -- putStrLn $ prettyText ast
    _other -> putStrLn "Incorrect command line. Expected: inputFileName"

parseAndPretty :: Parser -> FilePath -> String -> Either ParseError ModuleSpan
parseAndPretty parser fileName contents =
  case parser contents fileName of
    Left e -> Left e
    Right (ast, _comments) -> Right ast
