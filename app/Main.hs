import System.IO
import System.Exit
import System.Environment
import Paths_lambda (version)
import Data.Version (showVersion)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Void
import Text.Megaparsec.Error

import Language.Lambda
import Language.Lambda.Parser

repl :: IO ()
repl = do
    input <- putStr "λ> " >> hFlush stdout >> getLine
    case input of
      (':':c:_)
          | c == 'q' -> exitSuccess
          | c `elem` ['?', 'h'] -> putStrLn help >> repl
          | otherwise -> putStrLn "Invalid command" >> repl

      _ -> multiline input >>= putStrLn . either
             errorBundlePretty
             ((++ "\n") . show . reductMax) . parseLambda
           >> repl

finished :: Either (ParseErrorBundle String Void) Lambda -> Bool
finished (Left ParseErrorBundle
    { bundleErrors = (TrivialError _ (Just EndOfInput) _) :| _
    }) = False
finished _ = True

multiline :: String -> IO String
multiline s
    | finished (parseLambda s) = pure s
    | otherwise = (s ++) . ('\n' :) <$> ((putStr " > " >> hFlush stdout >> getLine) >>= multiline)

help :: String
help = unlines
    [ "  Commands available for the prompt:"
    , ""
    , "    <lambda expression>    reduct the expression"
    , "    :h, :?                 display this help"
    , "    :q                     exit this REPL"
    ]

main :: IO ()
main = do
    let message = "lambda " ++ showVersion version ++ "   :? for help"
    args <- getArgs
    case args of
      ("repl":_) -> putStrLn message >> repl
      _ -> getContents >>= putStrLn . either errorBundlePretty (show . reductMax) . parseLambda
