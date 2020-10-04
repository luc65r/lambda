import System.Environment
import Paths_lambda (version)
import Data.Version (showVersion)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Void
import Text.Megaparsec.Error
import System.Console.Haskeline

import Language.Lambda
import Language.Lambda.Parser

repl :: IO ()
repl = runInputT defaultSettings $ withInterrupt loop
    where
        loop :: InputT IO ()
        loop = do
            minput <- handleInterrupt (return (Just "")) $ getInputLine "λ> "
            case minput of
              Nothing -> return ()
              Just "" -> loop

              Just (':':c:_)
                | c == 'q' -> return ()
                | c `elem` ['?', 'h'] -> outputStrLn help >> loop
                | otherwise -> outputStrLn "Invalid command" >> loop

              Just input -> handleInterrupt (return ())
                (multiline input >>= outputStrLn . either
                    errorBundlePretty
                    (show . reductMax) . parseLambda) >> loop

finished :: Either (ParseErrorBundle String Void) Lambda -> Bool
finished (Left ParseErrorBundle
    { bundleErrors = (TrivialError _ (Just EndOfInput) _) :| _
    }) = False
finished _ = True

multiline :: String -> InputT IO String
multiline s
    | finished (parseLambda s) = pure s
    | otherwise = getInputLine " > " >>= (multiline . (s ++) . ('\n' :) . mte)
    where mte Nothing = ""
          mte (Just a) = a

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
