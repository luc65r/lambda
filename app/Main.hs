import System.IO
import System.Exit

import Language.Lambda
import Language.Lambda.Parser

repl :: IO ()
repl = do
    input <- putStr "λ> " >> hFlush stdout >> getLine
    case input of
      (':':c:cs) -> case c of
                      'q' -> exitSuccess
                      _ -> putStrLn "Invalid command" >> repl
      _ -> putStrLn (case reductMax <$> parse input of
                       Just s -> show s
                       Nothing -> "Invalid input") >> repl

main :: IO ()
main = repl
