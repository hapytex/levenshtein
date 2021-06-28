{-# LANGUAGE ExistentialQuantification, RankNTypes, TypeApplications #-}

module Data.Foldable.LevenshteinSpec (
    spec
  ) where

import Data.Foldable.Levenshtein(applyEdits, levenshtein, levenshteinDistance)

import Test.Hspec(Spec, it)
import Test.QuickCheck(maxSuccess, property, quickCheckWith, stdArgs)

spec :: Spec
spec = do
  it "lowerbound string difference" (quickCheckWith stdArgs { maxSuccess = 10000000 } (property (lowerBoundLengthDiff @ Int)))
  it "upperbound largest string" (quickCheckWith stdArgs { maxSuccess = 10000000 } (property (upperBoundLengthDiff @ Int)))
  it "if zero then same list" (quickCheckWith stdArgs { maxSuccess = 10000000 } (property (ifZeroThenSame @ Int)))
  it "test triangle inequality" (quickCheckWith stdArgs { maxSuccess = 10000000 } (property (triangleInequality @ Int)))
  it "test applying edits" (quickCheckWith stdArgs { maxSuccess = 10000000 } (property (testApplyingEdits @ Int)))
  it "lowerbound string difference" (quickCheckWith stdArgs { maxSuccess = 10000000 } (property (lowerBoundLengthDiff @ Char)))
  it "upperbound largest string" (quickCheckWith stdArgs { maxSuccess = 10000000 } (property (upperBoundLengthDiff @ Char)))
  it "if zero then same list" (quickCheckWith stdArgs { maxSuccess = 10000000 } (property (ifZeroThenSame @ Char)))
  it "test triangle inequality" (quickCheckWith stdArgs { maxSuccess = 10000000 } (property (triangleInequality @ Int)))
  it "test applying edits" (quickCheckWith stdArgs { maxSuccess = 10000000 } (property (testApplyingEdits @ Char)))
  it "lowerbound string difference" (quickCheckWith stdArgs { maxSuccess = 10000000 } (property (lowerBoundLengthDiff @ Bool)))
  it "upperbound largest string" (quickCheckWith stdArgs { maxSuccess = 10000000 } (property (upperBoundLengthDiff @ Bool)))
  it "if zero then same list" (quickCheckWith stdArgs { maxSuccess = 10000000 } (property (ifZeroThenSame @ Bool)))
  it "test triangle inequality" (quickCheckWith stdArgs { maxSuccess = 10000000 } (property (triangleInequality @ Bool)))
  it "test applying edits" (quickCheckWith stdArgs { maxSuccess = 10000000 } (property (testApplyingEdits @ Bool)))


lowerBoundLengthDiff :: forall a . Eq a => [a] -> [a] -> Bool
lowerBoundLengthDiff xs ys = abs (length xs - length ys) <= levenshteinDistance xs ys

upperBoundLengthDiff :: forall a . Eq a => [a] -> [a] -> Bool
upperBoundLengthDiff xs ys = levenshteinDistance xs ys <= max (length xs :: Int) (length ys)

triangleInequality :: forall a . Eq a => [a] -> [a] -> [a] -> Bool
triangleInequality xs ys zs = levenshteinDistance xs zs <= levenshteinDistance xs ys + (levenshteinDistance ys zs :: Int)

ifZeroThenSame :: forall a . Eq a => [a] -> [a] -> Bool
ifZeroThenSame xs ys = (levenshteinDistance xs ys == (0 :: Int)) == (xs == ys)

testApplyingEdits :: forall a . Eq a => [a] -> [a] -> Bool
testApplyingEdits xs ys = applyEdits (snd (levenshtein xs ys)) xs == Just ys
