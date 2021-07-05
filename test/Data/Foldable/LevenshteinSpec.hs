{-# LANGUAGE ExistentialQuantification, RankNTypes, ScopedTypeVariables, TypeApplications #-}

module Data.Foldable.LevenshteinSpec (
    spec
  ) where

import Data.Foldable.Levenshtein(Edits, applyEdits, genericLevenshteinDistance, genericLevenshteinDistance', levenshteinDistance, levenshteinDistance', genericLevenshtein, genericLevenshtein', levenshtein, levenshtein', genericReversedLevenshtein, genericReversedLevenshtein', reversedLevenshtein, reversedLevenshtein')

import Test.Hspec(Spec, it)
import Test.QuickCheck(maxSuccess, property, quickCheckWith, stdArgs)

ntimes :: Int
ntimes = 100000

spec :: Spec
spec = do
  it "lowerbound string difference" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (lowerBoundLengthDiff @ Int)))
  it "upperbound largest string" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (upperBoundLengthDiff @ Int)))
  it "if zero then same list" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (ifZeroThenSame @ Int)))
  it "test triangle inequality" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (triangleInequality @ Int)))
  it "test applying edits" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (testApplyingEdits @ Int)))
  it "Hamming distance bound" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (hammingDistanceBound @ Int)))
  it "Test if all distance metrics report the same" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (allDistancesSame @ Int)))
  it "lowerbound string difference" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (lowerBoundLengthDiff @ Char)))
  it "upperbound largest string" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (upperBoundLengthDiff @ Char)))
  it "if zero then same list" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (ifZeroThenSame @ Char)))
  it "test triangle inequality" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (triangleInequality @ Char)))
  it "test applying edits" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (testApplyingEdits @ Char)))
  it "Hamming distance bound" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (hammingDistanceBound @ Char)))
  it "Test if all distance metrics report the same" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (allDistancesSame @ Char)))
  it "lowerbound string difference" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (lowerBoundLengthDiff @ Bool)))
  it "upperbound largest string" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (upperBoundLengthDiff @ Bool)))
  it "if zero then same list" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (ifZeroThenSame @ Bool)))
  it "test triangle inequality" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (triangleInequality @ Bool)))
  it "test applying edits" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (testApplyingEdits @ Bool)))
  it "Hamming distance bound" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (hammingDistanceBound @ Bool)))
  it "Test if all distance metrics report the same" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (allDistancesSame @ Bool)))

allDistancesSame :: forall a . Eq a => [a] -> [a] -> Bool
allDistancesSame xs ys = da == db && db == dc && dc == dd && dd == de && de == df && df == dg && dg == dh
                      && dh == di && di == dj && dj == dk && dk == dl && efa == efb && efb == efc
                      && efc == efd && reverse efd == era && era == erb && erb == erc && erc == erd
  where da = genericLevenshteinDistance (const 1) (const 1) (const (const 1)) xs ys :: Int
        db = genericLevenshteinDistance' (==) (const 1) (const 1) (const (const 1)) xs ys :: Int
        dc = levenshteinDistance xs ys :: Int
        dd = levenshteinDistance' (==) xs ys :: Int
        (de, efa) = genericLevenshtein (const 1) (const 1) (const (const 1)) xs ys :: (Int, Edits a)
        (df, efb) = genericLevenshtein' (==) (const 1) (const 1) (const (const 1)) xs ys :: (Int, Edits a)
        (dg, efc) = levenshtein xs ys :: (Int, Edits a)
        (dh, efd) = levenshtein' (==) xs ys :: (Int, Edits a)
        (di, era) = genericReversedLevenshtein (const 1) (const 1) (const (const 1)) xs ys :: (Int, Edits a)
        (dj, erb) = genericReversedLevenshtein' (==) (const 1) (const 1) (const (const 1)) xs ys :: (Int, Edits a)
        (dk, erc) = reversedLevenshtein xs ys :: (Int, Edits a)
        (dl, erd) = reversedLevenshtein' (==) xs ys :: (Int, Edits a)


lowerBoundLengthDiff :: forall a . Eq a => [a] -> [a] -> Bool
lowerBoundLengthDiff xs ys = abs (length xs - length ys) <= levenshteinDistance xs ys

upperBoundLengthDiff :: forall a . Eq a => [a] -> [a] -> Bool
upperBoundLengthDiff xs ys = levenshteinDistance xs ys <= max (length xs :: Int) (length ys)

triangleInequality :: forall a . Eq a => [a] -> [a] -> [a] -> Bool
triangleInequality xs ys zs = levenshteinDistance xs zs <= levenshteinDistance xs ys + (levenshteinDistance ys zs :: Int)

ifZeroThenSame :: forall a . Eq a => [a] -> [a] -> Bool
ifZeroThenSame xs ys = (levenshteinDistance xs ys == (0 :: Int)) == (xs == ys)

testApplyingEdits :: forall a . Eq a => [a] -> [a] -> Bool
testApplyingEdits xs ys = applyEdits (snd ((levenshtein @[] @[] @a @Int) xs ys)) xs == Just ys

hammingDistanceBound :: forall a . Eq a => [a] -> [a] -> Bool
hammingDistanceBound xs ys = length xs /= length ys || levenshteinDistance xs ys <= hammingDistance xs ys

hammingDistance :: forall a . Eq a => [a] -> [a] -> Int
hammingDistance (x:xs) (y:ys)
  | x == y = hammingDistance xs ys
  | otherwise = 1 + hammingDistance xs ys
hammingDistance _ _ = 0
