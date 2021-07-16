{-# OPTIONS -fno-warn-orphans #-}
module Main (main) where

import Control.Exception(evaluate)
import Control.DeepSeq (rnf)

import Criterion.Main(Benchmark, bench, bgroup, defaultMain, nf)

import Data.Default(def)
import Data.Foldable.Levenshtein(Edit, editsCost, levenshteinDistance)

import DiffBy(diffBy)

smallA, smallB, bigA, bigB :: [Int]
smallA = [0, 5 .. 100]
smallB = [0, 3 .. 72]
bigA = [0, 5 .. 10000]
bigB = [0, 3 .. 7200]

smallC, smallD, bigC, bigD :: [Char]
smallC = ['\00', '\05' .. '\100']
smallD = ['\00', '\03' .. '\72']
bigC = ['\00', '\05' .. '\10000']
bigD = ['\00', '\03' .. '\7200']


levInt :: ([Int], [Int]) -> Int
levInt = uncurry levenshteinDistance

dfbInt :: ([Int], [Int]) -> [Edit Int]
dfbInt = uncurry (diffBy (==))

levChar :: ([Char], [Char]) -> Int
levChar = uncurry levenshteinDistance

dfbChar :: (String, String) -> [Edit Char]
dfbChar = uncurry (diffBy (==))

bTests :: Eq a => (([a], [a]) -> Int) -> (([a], [a]) -> [Edit a]) -> ([a], [a]) -> [Benchmark]
bTests f g xys = [bench "lib" (nf f xys), bench "tree-diff" (nf ((editsCost def :: [Edit a] -> Int) . g) xys)]

btest0 :: ([Int], [Int]) -> [Benchmark]
btest0 = bTests levInt dfbInt

btest1 :: ([Char], [Char]) -> [Benchmark]
btest1 = bTests levChar dfbChar

main :: IO ()
main = do
  evaluate (rnf smallA)
  evaluate (rnf smallB)

  evaluate (rnf bigA)
  evaluate (rnf bigB)

  defaultMain
    [ bgroup "Int"
      [ bgroup "same"
        [ bgroup "small" ((btest0 (smallA, smallA)) ++ btest0 (smallB, smallB))
        , bgroup "big" ((btest0 (bigA, bigA)) ++ btest0 (bigB, bigB))
        ]
      , bgroup "different"
        [ bgroup "small" (btest0 (smallA, smallB) ++ btest0 (smallB, smallA))
        , bgroup "mixed" (btest0 (smallA, bigB) ++ btest0 (bigB, smallA) ++ btest0 (smallA, bigB) ++ btest0 (bigB, smallA) ++ btest0 (smallB, bigB) ++ btest0 (smallB, bigA) ++ btest0 (bigA, smallB))
        , bgroup "big" (btest0 (bigA, bigB) ++ btest0 (bigB, bigA))
        ]
      ]
    , bgroup "Char"
      [ bgroup "same"
        [ bgroup "small" ((btest1 (smallC, smallC)) ++ btest1 (smallD, smallD))
        , bgroup "big" ((btest1 (bigC, bigC)) ++ btest1 (bigD, bigD))
        ]
      , bgroup "different"
        [ bgroup "small" (btest1 (smallC, smallD) ++ btest1 (smallD, smallC))
        , bgroup "mixed" (btest1 (smallC, bigD) ++ btest1 (bigD, smallC) ++ btest1 (smallC, bigD) ++ btest1 (bigD, smallC) ++ btest1 (smallD, bigD) ++ btest1 (smallD, bigC) ++ btest1 (bigC, smallD))
        , bgroup "big" (btest1 (bigC, bigD) ++ btest1 (bigD, bigC))
        ]
      ]

    ]
