module Abstractions where

import Control.Applicative (liftA3)
import qualified Control.Monad.State as S
import           Control.Monad (replicateM)
import qualified Data.Map.Strict as M

addThreeFailures :: M.Map String Int -> Maybe Int
addThreeFailures m =
  liftA3 (\a b c -> a + b + c)
         (M.lookup "a" m)
         (M.lookup "b" m)
         (M.lookup "c" m)

addThreeDependentFailures :: M.Map String (String, Int) -> Maybe Int
addThreeDependentFailures m =
  do
    (ka, a) <- (M.lookup "a" m)
    (kb, b) <- (M.lookup ka m)
    (_, c) <- (M.lookup kb m)
    pure $ a + b + c

listDeps :: M.Map String (String, Int) -> Maybe Int
listDeps m =
  let
    s = do
      key <- S.get
      (key', n) <- S.lift $ M.lookup key m
      S.put key'
      pure n
  in
    sum <$> S.evalStateT (replicateM 3 s) "a"


test :: IO ()
test =
  let
    m = M.fromList [("a", ("b", 1)), ("b", ("c", 3)), ("c", ("d", 5))]
  in
    do
      print $ addThreeDependentFailures m
      print $ listDeps m
