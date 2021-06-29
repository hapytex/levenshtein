{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DeriveTraversable, Safe #-}

{-|
Module      : Data.Foldable.Levenshtein
Description : A module to determine the edit distance and the edits to rewrite a given 'Foldable' to another 'Foldable'.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

The /Levenshtein distance/ is the /minimal/ number of additions, removals, and updates one has to make to
convert one list of items into another list of items. In this module we provide some functions that makes
it convenient to calculate the distance and the sequence of edits, and furthermore ways to alter the score
for an addition, removal, edit that can depend on what item is modified.
-}

module Data.Foldable.Levenshtein (
    -- * Calculate the Levenshtein distance
    genericLevenshteinDistance, levenshteinDistance, levenshteinDistance'
    -- * Obtain the Levenshtein distance together with the path of 'Edit's
  , genericLevenshtein, levenshtein, levenshtein'
    -- * Obtain the Levenshtein distance together with a reversed path of 'Edit's
  , genericReversedLevenshtein, reversedLevenshtein, reversedLevenshtein'
    -- * Data type to present modifications from one 'Foldable' to the other.a
  , Edit(Add, Rem, Copy, Swap), applyEdits
  ) where

import Control.Arrow(second)
import Control.DeepSeq(NFData, NFData1)

import Data.Binary(Binary(put, get), getWord8, putWord8)
import Data.Data(Data)
import Data.Foldable(toList)
import Data.Functor.Classes(Eq1(liftEq), Ord1(liftCompare))
import Data.Hashable(Hashable)
import Data.Hashable.Lifted(Hashable1)

import GHC.Generics(Generic, Generic1)

import Test.QuickCheck(oneof)
import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), Arbitrary1(liftArbitrary), arbitrary1)

_defaddrem :: Num b => a -> b
_defaddrem = const 1

_defswap :: Num b => a -> a -> b
_defswap = const _defaddrem

_addDefaults :: Num b => ((a -> b) -> (a -> b) -> (a -> a -> b) -> c) -> c
_addDefaults f = f _defaddrem _defaddrem _defswap

-- | A data type that is used to list how to edit a sequence to form another sequence.
data Edit a
  = Add a  -- ^ We add the given element to the sequence.
  | Rem a  -- ^ We remove the given element to the sequence.
  | Copy a  -- ^ We copy an element from the sequence, this basically act as a /no-op/.
  | Swap a a  -- ^ We modify the given first item into the second item, this thus denotes a replacement.
  deriving (Data, Eq, Foldable, Functor, Generic, Generic1, Ord, Read, Show, Traversable)

instance Arbitrary1 Edit where
    liftArbitrary arb = oneof [Add <$> arb, Rem <$> arb, Copy <$> arb, Swap <$> arb <*> arb]

instance Arbitrary a => Arbitrary (Edit a) where
    arbitrary = arbitrary1

instance Binary a => Binary (Edit a) where
    put (Add x) = putWord8 0 >> put x
    put (Rem x) = putWord8 1 >> put x
    put (Copy x) = putWord8 2 >> put x
    put (Swap xa xb) = putWord8 3 >> put xa >> put xb
    get = do
        tp <- getWord8
        case tp of
          0 -> Add <$> get
          1 -> Rem <$> get
          2 -> Copy <$> get
          3 -> Swap <$> get <*> get
          _ -> fail ("The numer " ++ show tp ++ " is not a valid Edit item.")

instance Eq1 Edit where
  liftEq eq = go
    where go (Add xa) (Add xb) = eq xa xb
          go (Rem xa) (Rem xb) = eq xa xb
          go (Copy xa) (Copy xb) = eq xa xb
          go (Swap xa ya) (Swap xb yb) = eq xa xb && eq ya yb
          go _ _ = False

instance Hashable a => Hashable (Edit a)

instance Hashable1 Edit

instance NFData a => NFData (Edit a)

instance NFData1 Edit

instance Ord1 Edit where
  liftCompare cmp = go
    where go (Add a) (Add b) = cmp a b
          go (Add _) _ = LT
          go _ (Add _) = GT
          go (Rem a) (Rem b) = cmp a b
          go (Rem _) _ = LT
          go _ (Rem _) = GT
          go (Copy a) (Copy b) = cmp a b
          go (Copy _) _ = LT
          go _ (Copy _) = GT
          go (Swap xa ya) (Swap xb yb) = cmp xa xb <> cmp ya yb

-- | Apply the given list of 'Edit's to the given list.
-- If the edits make sense, it returns the result wrapped
-- in a 'Just', if a check with the item that is removed/replaced
-- fails, the function will return 'Nothing'.
applyEdits :: Eq a
  => [Edit a]  -- ^ The given list of 'Edit's to apply to the given list.
  -> [a]  -- ^ The list of items to edit with the given 'Edit's.
  -> Maybe [a]  -- ^ The modified list, given the checks hold about what item to remove/replace wrapped in a 'Just'; 'Nothing' otherwise.
applyEdits [] ys = Just ys
applyEdits (Add x : xs) ys = (x :) <$> applyEdits xs ys
applyEdits (Rem x : xs) (y : ys)
  | x == y = applyEdits xs ys
applyEdits (Swap y x : xs) (y' : ys)
  | y == y' = (x :) <$> applyEdits xs ys
applyEdits (Copy x : xs) (y : ys)
  | x == y = (y :) <$> applyEdits xs ys
applyEdits _ _ = Nothing

-- | Determine the edit distance where an addition, removal, and change all count as 1, and where
-- the 'Eq' instance is used to determine if there is a change between two items.
levenshteinDistance :: (Foldable f, Foldable g, Eq a, Num b, Ord b)
  => f a  -- ^ The original given sequence.
  -> g a  -- ^ The target sequence.
  -> b  -- ^ The edit distance between the two 'Foldable's.
levenshteinDistance = levenshteinDistance' (==)

-- | Determine the edit distance together with the steps to transform the first foldable
-- (as list) into a second foldable (as list). Add, remove and swapping items all count
-- as one edit distance.
levenshtein :: (Foldable f, Foldable g, Eq a, Num b, Ord b)
  => f a  -- ^ The original given sequence.
  -> g a  -- ^ The target sequence.
  -> (b, [Edit a])  -- ^ The edit distance between the two 'Foldable's.
levenshtein = levenshtein' (==)

-- | Determine the edit distance together with the steps to transform the first foldable
-- (as list) into a second 'Foldable' (as list). Add, remove and swapping items all count
-- as one edit distance. One passes a function to determine if the elements of the two
-- 'Foldable's are equivalent.
levenshtein' :: (Foldable f, Foldable g, Num b, Ord b)
  => (a -> a -> Bool)  -- ^ The given equivalence relation to work with.
  -> f a  -- ^ The original given sequence.
  -> g a  -- ^ The target sequence.
  -> (b, [Edit a])  -- ^ The edit distance between the two 'Foldable's together with a list of edits to transform the first 'Foldable' to the second one.
levenshtein' = _addDefaults . genericLevenshtein

-- | Determine the edit distance together with the steps to transform the first foldable
-- (as list) into a second 'Foldable' (as list). Add, remove and swapping items all count
-- as one edit distance. The equality function '(==)' is used to determine if two items are
-- equivalent.
reversedLevenshtein :: (Foldable f, Foldable g, Eq a, Num b, Ord b)
  => f a  -- ^ The original given sequence.
  -> g a  -- ^ The target sequence.
  -> (b, [Edit a])  -- ^ The edit distance between the two 'Foldable's.
reversedLevenshtein = reversedLevenshtein' (==)

-- | Determine the edit distance together with the steps to transform the first foldable
-- (as list) into a second 'Foldable' (as list) in /reversed/ order. Add, remove and
-- swapping items all count as one edit distance. The given equality function is used
-- to determine if two items are equivalent.
reversedLevenshtein' :: (Foldable f, Foldable g, Num b, Ord b)
  => (a -> a -> Bool)  -- ^ The given equivalence relation to work with.
  -> f a  -- ^ The original given sequence.
  -> g a  -- ^ The target sequence.
  -> (b, [Edit a])  -- ^ The edit distance between the two 'Foldable's.
reversedLevenshtein' = _addDefaults . genericReversedLevenshtein

-- | Determine the edit distance together with the steps to transform the first foldable
-- (as list) into a second 'Foldable' (as list). Add, remove and swapping items all count
-- as one edit distance
levenshteinDistance' :: (Foldable f, Foldable g, Num b, Ord b)
  => (a -> a -> Bool)  -- ^ The given equivalence relation to work with.
  -> f a  -- ^ The original given sequence.
  -> g a  -- ^ The target sequence.
  -> b  -- ^ The edit distance between the two 'Foldable's.
levenshteinDistance' = _addDefaults . genericLevenshteinDistance

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
    row0 = scanl (\w i -> w + ad i) 0 tl
    nextCell x l y lt t
      | eq x y = lt
      | scs <= scr && scs <= sca = scs
      | sca <= scr = sca
      | otherwise = scr
      where sca = l + ad y
            scr = t + rm x
            scs = lt + sw x y
    curryNextCell x l = uncurry (uncurry (nextCell x l))
    nextRow ys da@(~(dn:ds)) x = scanl (curryNextCell x) (dn+rm x) (zip (zip ys da) ds)
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
      | scs <= scr && scs <= sca = (scs, Swap x y:lte)
      | sca <= scr = (sca, Add y:le)
      | otherwise = (scr, Rem x:te)
      where sca = l + ad y
            scr = t + rm x
            scs = lt + sw x y
    curryNextCell x l = uncurry (uncurry (nextCell x l))
    nextRow ys da@(~(~(dn, de):ds)) x = scanl (curryNextCell x) (dn+rm x,Rem x:de) (zip (zip ys da) ds)
    tl = toList ys'
