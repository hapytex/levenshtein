{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DeriveTraversable, Safe #-}

module Data.Foldable.Levenshtein (
    -- * Simple Levenshtein edit metrics
    levenshteinDistance, levenshteinDistance'
    -- * Define generic Levenshtein distances
  , genericLevenshteinDistance, genericReversedLevenshtein, genericLevenshtein
    -- * Data type to present modifications from one 'Foldable' to the other.a
  , Edit(Add, Rem, Copy, Swap)
  ) where

import Control.Arrow(second)

import Data.Data(Data)
import Data.Foldable(toList)

import GHC.Generics(Generic, Generic1)

_defaddrem :: Num b => a -> b
_defaddrem = const 1

_defswap :: Num b => a -> a -> b
_defswap = const _defaddrem

-- | A data type that is used to list how to edit a sequence to form another sequence.
data Edit a
  = Add a  -- ^ We add the given element to the sequence.
  | Rem a  -- ^ We remove the given element to the sequence.
  | Copy a  -- ^ We copy an element from the sequence, this basically act as a /no-op/.
  | Swap a a  -- ^ We modify the given first item into the second item, this thus denotes a replacement.
  deriving (Data, Eq, Foldable, Functor, Generic, Generic1, Ord, Read, Show, Traversable)

levenshteinDistance :: (Foldable f, Foldable g, Eq a, Num b, Ord b)
  => f a  -- ^ The original given sequence.
  -> g a  -- ^ The target sequence.
  -> b  -- ^ The edit distance between the two 'Foldable's.
levenshteinDistance = levenshteinDistance' (==)

levenshteinDistance' :: (Foldable f, Foldable g, Num b, Ord b)
  => (a -> a -> Bool)  -- ^ The given equivalence relation to work with.
  -> f a  -- ^ The original given sequence.
  -> g a  -- ^ The target sequence.
  -> b  -- ^ The edit distance between the two 'Foldable's.
levenshteinDistance' eq = (eq `genericLevenshteinDistance`) _defaddrem _defaddrem _defswap

-- | A function to determine the /Levenshtein distance/ by specifying the cost functions of adding, removing and editing characters. This function returns
-- a number that is the sum of the costs to transform the first 'Foldable' into the second 'Foldable'.
-- as first item of the 2-tuple, and the list of 'Edit's in reverse order as second item.
genericLevenshteinDistance :: (Foldable f, Foldable g, Num b, Ord b)
  => (a -> a -> Bool)  -- ^ The given equivalence relation to work with.
  -> (a -> b)  -- ^ The cost of adding the given item. The return value should be positive.
  -> (a -> b)  -- ^ The cost of removing the given item.  The return value should be positive.
  -> (a -> a -> b)  -- ^ The cost that it takes to replace an item of the first parameter with one of the second parameter. The return value should be positive.
  -> f a  -- ^ The original given sequence.
  -> g a  -- ^ The target sequence.
  -> b  -- ^ The edit distance between the two 'Foldable's.
genericLevenshteinDistance eq ad rm sw xs' ys' = last (foldl (nextRow tl) row0 xs')
  where
    row0 = scanl (\w i -> (w+ad i)) 0 tl
    nextCell x l y lt t
      | eq x y = lt
      | scs <= scr && lt <= sca = scs
      | sca <= scr = sca
      | otherwise = scr
      where sca = l + ad y
            scr = t + rm x
            scs = lt + sw x y
    curryNextCell x l = uncurry (uncurry (nextCell x l))
    nextRow ys ~(da@(~(dn:ds))) x = scanl (curryNextCell x) (dn+rm x) (zip (zip ys da) ds)
    tl = toList ys'

-- | A function to determine the /Levenshtein distance/ by specifying the cost functions of adding, removing and editing characters. The 2-tuple returns the distance
-- as first item of the 2-tuple, and the list of 'Edit's in normal order as second item.
genericLevenshtein :: (Foldable f, Foldable g, Num b, Ord b)
  => (a -> a -> Bool)  -- ^ The given equivalence relation to work with.
  -> (a -> b)  -- ^ The cost of adding the given item. The return value should be positive.
  -> (a -> b)  -- ^ The cost of removing the given item.  The return value should be positive.
  -> (a -> a -> b)  -- ^ The cost that it takes to replace an item of the first parameter with one of the second parameter. The return value should be positive.
  -> f a  -- ^ The original given sequence.
  -> g a  -- ^ The target sequence.
  -> (b, [Edit a])  -- ^ A 2-tuple with the edit score as first item, and a list of modifications in /normal/ order as second item to transform the first sequence to the second one.
genericLevenshtein eq ad rm sw xs' = second reverse . genericReversedLevenshtein eq ad rm sw xs'


-- | A function to determine the /Levenshtein distance/ by specifying the cost functions of adding, removing and editing characters. The 2-tuple returns the distance
-- as first item of the 2-tuple, and the list of 'Edit's in reverse order as second item.
genericReversedLevenshtein :: (Foldable f, Foldable g, Num b, Ord b)
  => (a -> a -> Bool)  -- ^ The given equivalence relation to work with.
  -> (a -> b)  -- ^ The cost of adding the given item. The return value should be positive.
  -> (a -> b)  -- ^ The cost of removing the given item.  The return value should be positive.
  -> (a -> a -> b)  -- ^ The cost that it takes to replace an item of the first parameter with one of the second parameter. The return value should be positive.
  -> f a  -- ^ The original given sequence.
  -> g a  -- ^ The target sequence.
  -> (b, [Edit a])  -- ^ A 2-tuple with the edit score as first item, and a list of modifications in /reversed/ order as second item to transform the first sequence to the second one.
genericReversedLevenshtein eq ad rm sw xs' ys' = last (foldl (nextRow tl) row0 xs')
  where
    row0 = scanl (\(w, is) i -> (w+ad i, Add i: is)) (0, []) tl
    nextCell x (l, le) y (lt, lte) (t, te)
      | eq x y = (lt, Copy x : lte)
      | scs <= scr && lt <= sca = (scs, Swap x y:lte)
      | sca <= scr = (sca, Add y:le)
      | otherwise = (scr, Rem x:te)
      where sca = l + ad y
            scr = t + rm x
            scs = lt + sw x y
    curryNextCell x l = uncurry (uncurry (nextCell x l))
    nextRow ys ~(da@(~(~(dn, de):ds))) x = scanl (curryNextCell x) (dn+rm x,Rem x:de) (zip (zip ys da) ds)
    tl = toList ys'
