import System.IO
import System.Exit
import Paths_lambda (version)
import Data.Version (showVersion)

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

      _ -> putStrLn (case reductMax <$> parse input of
                       Right s -> show s ++ "\n"
                       Left s -> s) >> repl

help :: String
help = unlines
    [ "  Commands available for the prompt:"
    , ""
    , "    <lambda expression>    reduct the expression"
    , "    :h, :?                 display this help"
    , "    :q                     exit this REPL"
    ]

main :: IO ()
main = putStrLn message >> repl
    where message = "lambda " ++ showVersion version ++ "   :? for help"
