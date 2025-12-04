{-# LANGUAGE InstanceSigs #-}

module Main where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Either

---------------------------------------------------------
-- HC19T1: Applicative Instance for Pair
---------------------------------------------------------
data Pair a = Pair a a deriving Show

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
    pure x = Pair x x
    (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

---------------------------------------------------------
-- HC19T2: addThreeApplicative
---------------------------------------------------------
addThreeApplicative :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
addThreeApplicative a b c = (+) <$> ((+) <$> a <*> b) <*> c

---------------------------------------------------------
-- HC19T3: safeProduct
---------------------------------------------------------
safeProduct :: [Maybe Int] -> Maybe Int
safeProduct = foldr (liftA2 (*)) (Just 1)

---------------------------------------------------------
-- HC19T4: liftAndMultiply
---------------------------------------------------------
liftAndMultiply :: Int -> Int -> Int
liftAndMultiply = liftA2 (*) id id

---------------------------------------------------------
-- HC19T5: applyEffects
---------------------------------------------------------
applyEffects :: (IO Int, IO Int) -> IO Int
applyEffects (io1, io2) = (+) <$> io1 <*> io2

---------------------------------------------------------
-- HC19T6: repeatEffect
---------------------------------------------------------
repeatEffect :: IO () -> IO ()
repeatEffect = forever

---------------------------------------------------------
-- HC19T7: conditionalPrint
---------------------------------------------------------
conditionalPrint :: Bool -> String -> IO ()
conditionalPrint cond msg = when cond $ putStrLn msg

---------------------------------------------------------
-- HC19T8: discardSecond
---------------------------------------------------------
discardSecond :: IO a -> IO b -> IO a
discardSecond = (<*)

---------------------------------------------------------
-- HC19T9: pureAndApply
---------------------------------------------------------
pureAndApply :: IO Int
pureAndApply = pure (+1) <*> pure 5

---------------------------------------------------------
-- HC19T10: combineResults for Either
---------------------------------------------------------
combineResults :: Either e Int -> Either e Int -> Either e Int
combineResults = liftA2 (+)

---------------------------------------------------------
-- HC19T11: Applicative Instance for Wrapper
---------------------------------------------------------
data Wrapper a = Wrapper a deriving Show

instance Functor Wrapper where
    fmap f (Wrapper x) = Wrapper (f x)

instance Applicative Wrapper where
    pure = Wrapper
    (Wrapper f) <*> (Wrapper x) = Wrapper (f x)

---------------------------------------------------------
-- HC19T12: sumThreeApplicative for Either String Int
---------------------------------------------------------
sumThreeApplicative :: Either String Int -> Either String Int -> Either String Int -> Either String Int
sumThreeApplicative a b c = (+) <$> ((+) <$> a <*> b) <*> c

---------------------------------------------------------
-- HC19T13: whenApplicative
---------------------------------------------------------
whenApplicative :: Bool -> IO () -> IO ()
whenApplicative cond action = if cond then action else pure ()

---------------------------------------------------------
-- HC19T14: replicateEffect
---------------------------------------------------------
replicateEffect :: Int -> IO a -> IO [a]
replicateEffect = replicateM

---------------------------------------------------------
-- HC19T15: sequenceEffects
---------------------------------------------------------
sequenceEffects :: Applicative f => [f a] -> f [a]
sequenceEffects = sequenceA

---------------------------------------------------------
-- HC19T16: applyWithEffects
---------------------------------------------------------
applyWithEffects :: IO Int -> IO Int -> IO Int
applyWithEffects a b = (+) <$> a <*> b

---------------------------------------------------------
-- HC19T17: simulateMaybeEffect
---------------------------------------------------------
simulateMaybeEffect :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
simulateMaybeEffect = liftA2

---------------------------------------------------------
-- HC19T18: combineEitherResults
---------------------------------------------------------
combineEitherResults :: Either String Int -> Either String Int -> Either String Int
combineEitherResults = liftA2 (+)

---------------------------------------------------------
-- HC19T19: sequenceApplicative for Maybe List
---------------------------------------------------------
sequenceApplicative :: [Maybe a] -> Maybe [a]
sequenceApplicative = sequenceA

---------------------------------------------------------
-- HC19T20: replicateForever
---------------------------------------------------------
replicateForever :: IO () -> IO ()
replicateForever = forever

---------------------------------------------------------
-- MAIN FUNCTION: Demonstrates key HC19 tasks
---------------------------------------------------------
main :: IO ()
main = do
    putStrLn "--- HC19T1: Pair Applicative ---"
    print $ Pair (+1) (*2) <*> Pair 3 4

    putStrLn "\n--- HC19T2: addThreeApplicative ---"
    print $ addThreeApplicative (Just 1) (Just 2) (Just 3)

    putStrLn "\n--- HC19T3: safeProduct ---"
    print $ safeProduct [Just 2, Just 3, Just 4]

    putStrLn "\n--- HC19T4: liftAndMultiply ---"
    print $ liftAndMultiply 3 4

    putStrLn "\n--- HC19T5: applyEffects ---"
    applyEffects (pure 5, pure 10) >>= print

    putStrLn "\n--- HC19T7: conditionalPrint ---"
    conditionalPrint True "This prints because condition is True"
    conditionalPrint False "This won't print"

    putStrLn "\n--- HC19T8: discardSecond ---"
    discardSecond (putStrLn "First") (putStrLn "Second") 

    putStrLn "\n--- HC19T9: pureAndApply ---"
    print =<< pureAndApply

    putStrLn "\n--- HC19T10: combineResults ---"
    print $ combineResults (Right 5) (Right 10)

    putStrLn "\n--- HC19T12: sumThreeApplicative ---"
    print $ sumThreeApplicative (Right 1) (Right 2) (Right 3)

    putStrLn "\n--- HC19T14: replicateEffect ---"
    replicateEffect 3 (print "Hello")

    putStrLn "\n--- HC19T15: sequenceEffects ---"
    sequenceEffects [Just 1, Just 2, Just 3] >>= print . fromJust

    putStrLn "\n--- HC19T17: simulateMaybeEffect ---"
    print $ simulateMaybeEffect (+) (Just 5) (Just 10)

    putStrLn "\n--- HC19T19: sequenceApplicative ---"
    print $ sequenceApplicative [Just 1, Just 2, Just 3]
