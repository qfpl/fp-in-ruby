module Abstractions where

import Control.Applicative (liftA3)
import qualified Control.Monad.State as S
import           Control.Monad (replicateM, (<=<))
import qualified Data.Map.Strict as M
import Data.Semigroup (Semigroup, (<>))
import Data.Maybe (maybe)
import Data.Monoid (Sum (Sum), Product (Product))
import Data.Validation (AccValidation (AccSuccess, AccFailure))
import Text.Read (readMaybe)

goodMap :: M.Map String Int
goodMap = M.fromList [("a", 1), ("b", 3), ("c", 5)]

goodDepMap, badDepMap :: M.Map String (String, Int)
goodDepMap = M.fromList [("a", ("b", 1)), ("b", ("c", 3)), ("c", ("d", 5))]
badDepMap = M.fromList [("a", ("b", 1)), ("b", ("d", 3)), ("c", ("d", 5))]

addThreeMaybes :: M.Map String Int -> Maybe Int
addThreeMaybes m =
  liftA3 (\a b c -> a + b + c)
         (M.lookup "a" m)
         (M.lookup "b" m)
         (M.lookup "c" m)

addThreeMaybesL h =
  fmap sum . traverse (`M.lookup` h) $ ["foo", "bar", "baz"]

addThreeValidations h keys =
  let
    f k = maybe (AccFailure $ ["Couldn't find key: " <> k])
                AccSuccess
                (M.lookup k h)
  in
    fmap sum . traverse f $ keys

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
      (key, n) <- S.get >>= S.lift . f
      S.put key
      pure n
  in
    mconcat <$> S.evalStateT (replicateM 3 nextN) s

testListDepsPMaybe =
  listDepsP (flip M.lookup (fmap Sum <$> goodDepMap)) "a"

testListDepsPEither =
  let
    f k = maybe (Left $ "Couldn't find " <> k)
                Right
                (M.lookup k (fmap Product <$> badDepMap))
  in
    listDepsP f "a"

testListDepsPList =
  let
    f k = maybe [] pure (M.lookup k (fmap Sum <$> goodDepMap))
  in
    listDepsP f "a"

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
