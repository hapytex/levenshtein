{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DeriveTraversable, Safe #-}

module Data.Foldable.Levenshtein
    ( someFunc
    ) where

import Data.Data(Data)

import GHC.Generics(Generic, Generic1)

-- | A data type that is used to list how to edit a sequence to form another sequence.
data Edit a
  = Add a  -- ^ We add the given element to the sequence.
  | Rem a  -- ^ We remove the given element to the sequence.
  | Copy a  -- ^ We copy an element from the sequence, this basically act as a /no-op/.
  | Swap a a  -- ^ We modify the given first item into the second item, this thus denotes a replacement.
  deriving (Data, Eq, Foldable, Functor, Generic, Generic1, Ord, Read, Show, Traversable)

-- | A function to determine the /Levenshtein distance/ by specifying the cost functions of adding, removing and editing characters. The 2-tuple returns the distance
-- as first item of the 2-tuple, and the list of 'Edit's in reverse order as second item.
genericReversedLevenshtein' :: (Foldable f, Foldable g)
  => (a -> a -> Bool)  -- ^ The given equivalence relation to work with.
  -> (a -> Int)  -- ^ The cost of adding the given item.
  -> (a -> Int)  -- ^ The cost of removing the given item.
  -> (a -> a -> Int)  -- ^ The cost that it takes to replace an item of the first parameter with one of the second parameter.
  -> f a  -- ^ The original given sequence.
  -> g a  -- ^ The target sequence.
  -> (Int, [Edit a])  -- ^ A 2-tuple with the edit score as first item, and a list of modifications in /reversed/ order as second item to transform the first sequence to the second one.
genericReversedLevenshtein' eq ad rm sw xs' ys' = last (foldl (nextRow ys') row0 xs')
  where
    row0 = scanl (\(w, is) i -> (w+ad i, Add i: is)) (0, []) ys'
    nextCell x (l, le) y (lt, lte) (t, te)
      | eq x y = (lt, Copy x : lte)
      | scs <= scr && lt <= sca = (scs, Swap x y:lte)
      | sca <= scr = (sca, Add y:le)
      | otherwise = (scr, Rem x:te)
      where sca = l + ad y
            scr = t + rm x
            scs = lt + sw x y
    curryNextCell x l = uncurry (uncurry (nextCell x l))
    nextRow ys da@(~((dn, de):ds)) x = scanl (curryNextCell x) (dn+rm x,Rem x:de) (zip (zip ys da) ds)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
