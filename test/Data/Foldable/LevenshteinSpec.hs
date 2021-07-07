{-# LANGUAGE ExistentialQuantification, RankNTypes, ScopedTypeVariables, TypeApplications #-}

module Data.Foldable.LevenshteinSpec (
    spec
  ) where

import Data.Default(def)
import Data.Foldable.Levenshtein(Edits, editsCost, applyEdits, genericLevenshteinDistance, genericLevenshteinDistance', genericLevenshteinDistanceWithScore, genericLevenshteinDistanceWithScore', levenshteinDistance, levenshteinDistance', genericLevenshtein, genericLevenshtein', genericLevenshteinWithScore, genericLevenshteinWithScore', levenshtein, levenshtein', genericReversedLevenshtein, genericReversedLevenshtein', genericReversedLevenshteinWithScore, genericReversedLevenshteinWithScore', reversedLevenshtein, reversedLevenshtein', constantEditScore, getOrigin, getTarget)

import Test.Hspec(Spec, it)
import Test.QuickCheck(maxSuccess, property, quickCheckWith, stdArgs)

ntimes :: Int
ntimes = 1000

spec :: Spec
spec = do
  it "lowerbound string difference" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (lowerBoundLengthDiff @ Int)))
  it "upperbound largest string" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (upperBoundLengthDiff @ Int)))
  it "if zero then same list" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (ifZeroThenSame @ Int)))
  it "test triangle inequality" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (triangleInequality @ Int)))
  it "test applying edits" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (testApplyingEdits @ Int)))
  it "Hamming distance bound" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (hammingDistanceBound @ Int)))
  it "Test if all distance metrics report the same" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (allDistancesSame @ Int)))
  it "Check if the score edit is linear" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (checkLinearCost @ Int)))
  it "Check if a linear modification yields the same edits" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (checkOptimalPathSame @ Int)))
  it "Check if a linear fmap yields the same edits" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (checkOptimalPathSameLinearFmap @ Int)))
  it "Check if we can recover the origin and target with the edits" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (checkRecoveryOfOriginAndTarget @ Int)))
  it "lowerbound string difference" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (lowerBoundLengthDiff @ Char)))
  it "upperbound largest string" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (upperBoundLengthDiff @ Char)))
  it "if zero then same list" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (ifZeroThenSame @ Char)))
  it "test triangle inequality" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (triangleInequality @ Char)))
  it "test applying edits" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (testApplyingEdits @ Char)))
  it "Hamming distance bound" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (hammingDistanceBound @ Char)))
  it "Test if all distance metrics report the same" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (allDistancesSame @ Char)))
  it "Check if the score edit is linear" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (checkLinearCost @ Char)))
  it "Check if a linear modification yields the same edits" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (checkOptimalPathSame @ Char)))
  it "Check if a linear fmap yields the same edits" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (checkOptimalPathSameLinearFmap @ Char)))
  it "Check if we can recover the origin and target with the edits" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (checkRecoveryOfOriginAndTarget @ Char)))
  it "lowerbound string difference" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (lowerBoundLengthDiff @ Bool)))
  it "upperbound largest string" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (upperBoundLengthDiff @ Bool)))
  it "if zero then same list" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (ifZeroThenSame @ Bool)))
  it "test triangle inequality" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (triangleInequality @ Bool)))
  it "test applying edits" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (testApplyingEdits @ Bool)))
  it "Hamming distance bound" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (hammingDistanceBound @ Bool)))
  it "Test if all distance metrics report the same" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (allDistancesSame @ Bool)))
  it "Check if the score edit is linear" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (checkLinearCost @ Bool)))
  it "Check if a linear modification yields the same edits" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (checkOptimalPathSame @ Bool)))
  it "Check if a linear fmap yields the same edits" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (checkOptimalPathSameLinearFmap @ Bool)))
  it "Check if we can recover the origin and target with the edits" (quickCheckWith stdArgs { maxSuccess = ntimes } (property (checkRecoveryOfOriginAndTarget @ Bool)))

allDistancesSame :: forall a . Eq a => [a] -> [a] -> Bool
allDistancesSame xs ys = da == db && db == dc && dc == dd && dd == de && de == df && df == dg && dg == dh
                      && dh == di && di == dj && dj == dk && dk == dl && dl == dm && dm == dn
                      && dn == do' && do' == dp && dp == dq && dq == dr
                      && efa == efb && efb == efc && efc == efd && efd == efe && efe == eff
                      && reverse eff == era && era == erb && erb == erc && erc == erd
                      && erd == ere && ere == erf
                      && editsCost def efa == editsCost @[] @Int def era && editsCost @[] @Int def efa == da
  where da = genericLevenshteinDistance (const 1) (const 1) (const (const 1)) xs ys :: Int
        db = genericLevenshteinDistance' (==) (const 1) (const 1) (const (const 1)) xs ys :: Int
        dc = levenshteinDistance xs ys :: Int
        dd = levenshteinDistance' (==) xs ys :: Int
        de = genericLevenshteinDistanceWithScore def xs ys :: Int
        df = genericLevenshteinDistanceWithScore' (==) def xs ys :: Int
        (dg, efa) = genericLevenshtein (const 1) (const 1) (const (const 1)) xs ys :: (Int, Edits a)
        (dh, efb) = genericLevenshtein' (==) (const 1) (const 1) (const (const 1)) xs ys :: (Int, Edits a)
        (di, efc) = genericLevenshteinWithScore def xs ys :: (Int, Edits a)
        (dj, efd) = genericLevenshteinWithScore' (==) def xs ys :: (Int, Edits a)
        (dk, efe) = levenshtein xs ys :: (Int, Edits a)
        (dl, eff) = levenshtein' (==) xs ys :: (Int, Edits a)
        (dm, era) = genericReversedLevenshtein (const 1) (const 1) (const (const 1)) xs ys :: (Int, Edits a)
        (dn, erb) = genericReversedLevenshtein' (==) (const 1) (const 1) (const (const 1)) xs ys :: (Int, Edits a)
        (do', erc) = genericReversedLevenshteinWithScore def xs ys :: (Int, Edits a)
        (dp, erd) = genericReversedLevenshteinWithScore' (==) def xs ys :: (Int, Edits a)
        (dq, ere) = reversedLevenshtein xs ys :: (Int, Edits a)
        (dr, erf) = reversedLevenshtein' (==) xs ys :: (Int, Edits a)

checkRecoveryOfOriginAndTarget :: forall a . Eq a => [a] -> [a] -> Bool
checkRecoveryOfOriginAndTarget xs ys = getOrigin efa == xs && getTarget efa == ys
  where (_, efa) = (levenshtein @ [] @ [] @ a @ Int) xs ys

checkLinearCost :: forall a . Eq a => Int -> [a] -> [a] -> Bool
checkLinearCost n' xs ys = (genericLevenshteinDistanceWithScore def xs ys) * n == genericLevenshteinDistanceWithScore (constantEditScore n) xs ys
  where n = abs n' + 1

checkOptimalPathSame :: forall a . Eq a => Int -> [a] -> [a] -> Bool
checkOptimalPathSame n' xs ys = snd (genericLevenshteinWithScore @[] @[] @a @Int def xs ys) == snd (genericLevenshteinWithScore (constantEditScore n) xs ys)
  where n = abs n' + 1

checkOptimalPathSameLinearFmap :: forall a . Eq a => Int -> [a] -> [a] -> Bool
checkOptimalPathSameLinearFmap n' xs ys = snd (genericLevenshteinWithScore @[] @[] @a @Int def xs ys) == snd (genericLevenshteinWithScore ((n*) <$> def) xs ys)
  where n = abs n' + 1

lowerBoundLengthDiff :: forall a . Eq a => [a] -> [a] -> Bool
lowerBoundLengthDiff xs ys = abs (length xs - length ys) <= levenshteinDistance xs ys

upperBoundLengthDiff :: forall a . Eq a => [a] -> [a] -> Bool
upperBoundLengthDiff xs ys = levenshteinDistance xs ys <= max (length xs :: Int) (length ys)

triangleInequality :: forall a . Eq a => [a] -> [a] -> [a] -> Bool
triangleInequality xs ys zs = levenshteinDistance xs zs <= levenshteinDistance xs ys + (levenshteinDistance ys zs :: Int)

ifZeroThenSame :: forall a . Eq a => [a] -> [a] -> Bool
ifZeroThenSame xs ys = (levenshteinDistance xs ys == (0 :: Int)) == (xs == ys)

testApplyingEdits :: forall a . Eq a => [a] -> [a] -> Bool
testApplyingEdits xs ys = applyEdits eds xs == Just ys && (xs == ys || applyEdits eds ys == Nothing)
  where (_, eds) = (levenshtein @[] @[] @a @Int) xs ys

hammingDistanceBound :: forall a . Eq a => [a] -> [a] -> Bool
hammingDistanceBound xs ys = length xs /= length ys || levenshteinDistance xs ys <= hammingDistance xs ys

hammingDistance :: forall a . Eq a => [a] -> [a] -> Int
hammingDistance (x:xs) (y:ys)
  | x == y = hammingDistance xs ys
  | otherwise = 1 + hammingDistance xs ys
hammingDistance _ _ = 0
