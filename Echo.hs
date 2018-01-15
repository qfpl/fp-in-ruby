module Echo where

import Control.Monad (replicateM)
import Data.Either (either)
import System.Environment (getArgs)
import Text.Read (readEither)


main :: IO ()
main = do
  args <- getArgs
  let
    failParse s =
      error $ "Unable to parse '" ++ s ++ "' as Int"

    go' =
      case args of
        (s:_) -> either (const (failParse s)) go $ readEither s
        _     -> error "Expected at least one arg"

    go _ "q" = pure ()
    go n s   = do
        (() <$) . replicateM n . putStrLn $ s
        getLine >>= go n

  getLine >>= go'
