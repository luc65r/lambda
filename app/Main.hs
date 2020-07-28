import System.IO
import System.Exit

import Language.Lambda
import Language.Lambda.Parser

repl :: IO ()
repl = do
    input <- putStr "λ> " >> hFlush stdout >> getLine
    case input of
      (':':c:_) -> case c of
                      'q' -> exitSuccess
                      _ -> putStrLn "Invalid command" >> repl
      _ -> putStrLn (case reductMax <$> parse input of
                       Right s -> show s
                       Left s -> "Invalid input: " ++ s) >> repl

main :: IO ()
main = repl
