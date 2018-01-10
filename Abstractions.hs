module Abstractions where

import Control.Applicative (liftA3)
import qualified Control.Monad.State as S
import           Control.Monad (replicateM, (<=<))
import qualified Data.Map.Strict as M
import Text.Read (readMaybe)

goodMap = M.fromList [("a", ("b", 1)), ("b", ("c", 3)), ("c", ("d", 5))]
badMap = M.fromList [("a", ("b", 1)), ("b", ("d", 3)), ("c", ("d", 5))]

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

listDepsP :: (Monad m, Monoid n) => (k -> m (k, n)) -> k -> m n
listDepsP f s =
  let
    nextN = do
      key <- S.get
      (key', n) <- S.lift $ f key
      S.put key'
      pure n
  in
    mconcat <$> S.evalStateT (replicateM 3 nextN) s

whoozits :: Int -> Maybe Int
whoozits n
  | odd n = Just $ n * n + 1
  | otherwise = Nothing

evenToString :: Int -> Maybe String
evenToString n
  | even n = Just $ show n
  | otherwise = Nothing

kComp :: String -> Maybe String
kComp =
  evenToString <=< whoozits <=< readMaybe

kCompP :: Monad m => (a -> m b) -> (b -> m c) -> (c -> m d) -> a -> m d
kCompP f g h s =
  h <=< g <=< f $ s
