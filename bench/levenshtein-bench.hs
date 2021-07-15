{-# OPTIONS -fno-warn-orphans #-}
module Main (main) where

import Control.Exception(evaluate)
import Control.DeepSeq (rnf)

import Criterion.Main(bench, bgroup, defaultMain, nf)

import Data.Foldable.Levenshtein(levenshteinDistance)

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

levChar :: ([Char], [Char]) -> Int
levChar = uncurry levenshteinDistance

main :: IO ()
main = do
  evaluate (rnf smallA)
  evaluate (rnf smallB)

  evaluate (rnf bigA)
  evaluate (rnf bigB)

  defaultMain
    [ bgroup "Int"
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
    , bgroup "Char"
      [ bgroup "same"
        [ bgroup "small"
          [ bench "lib" $ (nf levChar) (smallC, smallC)
          , bench "lib" $ (nf levChar) (smallD, smallD)
          ]
        , bgroup "big"
          [ bench "lib" $ (nf levChar) (bigC, bigC)
          , bench "lib" $ (nf levChar) (bigD, bigD)
          ]
        ]
      , bgroup "different"
        [ bgroup "small"
          [ bench "lib" $ (nf levChar) (smallC, smallD)
          , bench "lib" $ (nf levChar) (smallD, smallC)
          ]
        , bgroup "mixed"
          [ bench "lib" $ (nf levChar) (smallC, bigD)
          , bench "lib" $ (nf levChar) (bigD, smallC)
          , bench "lib" $ (nf levChar) (smallC, bigD)
          , bench "lib" $ (nf levChar) (bigD, smallC)
          , bench "lib" $ (nf levChar) (smallD, bigD)
          , bench "lib" $ (nf levChar) (bigD, smallD)
          , bench "lib" $ (nf levChar) (smallD, bigC)
          , bench "lib" $ (nf levChar) (bigC, smallD)
          ]
        , bgroup "big"
          [ bench "lib" $ (nf levChar) (bigC, bigD)
          , bench "lib" $ (nf levChar) (bigD, bigC)
          ]
        ]
      ]

    ]
