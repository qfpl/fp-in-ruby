module Abstractions where

import           Control.Applicative (liftA3)
import           Control.Monad       (replicateM, (<=<))
import qualified Control.Monad.State as S
import qualified Data.Map.Strict     as M
import           Data.Maybe          (maybe)
import           Data.Monoid         (Product (Product), Sum (Sum))
import           Data.Semigroup      (Semigroup, (<>))
import           Data.Validation     (AccValidation, AccValidation (AccFailure, AccSuccess))
import           Text.Read           (readMaybe)

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

addMaybes :: M.Map String Int -> [String] -> Maybe Int
addMaybes h keys =
  fmap sum . traverse (`M.lookup` h) $ keys

addValidations :: M.Map String Int -> [String] -> AccValidation [String] Int
addValidations h keys =
  let
    f k = maybe (AccFailure $ ["Couldn't find key: " <> k])
                AccSuccess
                (M.lookup k h)
  in
    fmap sum . traverse f $ keys

-- validatedLookup :: Num a
--                 => M.Map String a
--                 -> String
--                 -> AccValidation [String] a
validatedLookup h k =
  maybe (AccFailure $ ["Couldn't find key: " <> k])
        AccSuccess
        (M.lookup k h)

-- addThings ::
--   ( Num n
--   , Applicative f
--   , Traversable t
--   )
--   => (k -> f n)
--   -> t k
--   -> f n
addThings f as =
  fmap sum . traverse f $ as

addMultiplesOf n ms =
  addThings (*) ms n

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
