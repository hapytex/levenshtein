{-# OPTIONS -fno-warn-orphans #-}
module Main (main) where

import Control.Exception(evaluate)
import Control.DeepSeq (rnf)

import Criterion.Main(bench, bgroup, defaultMain, nf)

import Data.Foldable.Levenshtein(levenshteinDistance)

smallA :: [Int]
smallB :: [Int]

smallA = [0, 5 .. 100]
smallB = [0, 3 .. 72]

bigA :: [Int]
bigA = [0, 5 .. 10000]

bigB :: [Int]
bigB = [0, 3 .. 7200]

levInt :: ([Int], [Int]) -> Int
levInt = uncurry levenshteinDistance

main :: IO ()
main = do
  evaluate (rnf smallA)
  evaluate (rnf smallB)

  evaluate (rnf bigA)
  evaluate (rnf bigB)

  defaultMain
    [ bgroup "same"
      [ bgroup "small"
        [ bench "lib" $ (nf levInt) (smallA, smallA)
        , bench "lib" $ (nf levInt) (smallB, smallB)
        ]
      , bgroup "big"
        [ bench "lib" $ (nf levInt) (bigA, bigA)
        , bench "lib" $ (nf levInt) (bigB, bigB)
        ]
      ]
    , bgroup "different"
      [ bgroup "small"
        [ bench "lib" $ (nf levInt) (smallA, smallB)
        , bench "lib" $ (nf levInt) (smallB, smallA)
        ]
      , bgroup "mixed"
        [ bench "lib" $ (nf levInt) (smallA, bigB)
        , bench "lib" $ (nf levInt) (bigB, smallA)
        , bench "lib" $ (nf levInt) (smallA, bigB)
        , bench "lib" $ (nf levInt) (bigB, smallA)
        , bench "lib" $ (nf levInt) (smallB, bigB)
        , bench "lib" $ (nf levInt) (bigB, smallB)
        , bench "lib" $ (nf levInt) (smallB, bigA)
        , bench "lib" $ (nf levInt) (bigA, smallB)
        ]
      , bgroup "big"
        [ bench "lib" $ (nf levInt) (bigA, bigB)
        , bench "lib" $ (nf levInt) (bigB, bigA)
        ]
      ]
    ]
