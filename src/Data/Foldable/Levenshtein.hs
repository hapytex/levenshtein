{-# LANGUAGE CPP, DeriveDataTypeable, DeriveGeneric, DeriveTraversable, Safe #-}

{-|
Module      : Data.Foldable.Levenshtein
Description : A module to determine the edit distance and the 'Edit's to rewrite a given 'Foldable' to another 'Foldable'.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

The /Levenshtein distance/ is the /minimal/ number of additions, removals, and updates one has to make to
convert one list of items into another list of items. In this module we provide some functions that makes
it convenient to calculate the distance and the sequence of 'Edit's, and furthermore ways to alter the score
for an addition, removal, edit that can depend on what item is modified.
-}

module Data.Foldable.Levenshtein (
    -- * Levenshtein distance
    -- ** Calculate the Levenshtein distance
    genericLevenshteinDistance, genericLevenshteinDistance', genericLevenshteinDistanceWithScore, genericLevenshteinDistanceWithScore', levenshteinDistance, levenshteinDistance'
    -- ** Obtain the Levenshtein distance together with the path of 'Edit's
  , genericLevenshtein, genericLevenshtein', genericLevenshteinWithScore, genericLevenshteinWithScore', levenshtein, levenshtein'
    -- ** Obtain the Levenshtein distance together with a reversed path of 'Edit's
  , genericReversedLevenshtein, genericReversedLevenshtein', genericReversedLevenshteinWithScore, genericReversedLevenshteinWithScore', reversedLevenshtein, reversedLevenshtein'
    -- * Damerau-Levenshtein distance
    -- * Data type to present modifications from one 'Foldable' to another.
  , Edit(Add, Rem, Copy, Swap, Transpose), Edits, applyEdits
    -- * Present the modification costs
  , EditScore(editAdd, editRemove, editReplace, editTranspose), editCost, editsCost, constantEditScore, getOrigin, getTarget
  ) where

import Control.Arrow(second)
import Control.DeepSeq(NFData, NFData1)

import Data.Binary(Binary(put, get), getWord8, putWord8)
import Data.Data(Data)
import Data.Default(Default(def))
import Data.Foldable(toList)
import Data.Functor.Classes(Eq1(liftEq), Ord1(liftCompare))
import Data.Hashable(Hashable)
import Data.Hashable.Lifted(Hashable1)
#if __GLASGOW_HASKELL__ < 803
import Data.Semigroup(Semigroup((<>)))
#endif

import GHC.Generics(Generic, Generic1)

import Test.QuickCheck(oneof)
import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), Arbitrary1(liftArbitrary), arbitrary1)

_defaddrem :: Num b => a -> b
_defaddrem = const 1

_defswap :: Num b => a -> a -> b
_defswap = const _defaddrem

_addDefaults :: Num b => ((a -> b) -> (a -> b) -> (a -> a -> b) -> c) -> c
_addDefaults f = f _defaddrem _defaddrem _defswap

-- | A data type that provides information about how costly a certain edit is. One can make use
-- of this data type to change the cost functions in an effective way. The 'EditScore' scales linear,
-- this means that if we double all the costs, the minimal edit cost will also double.
data EditScore a b
  = EditScore {
      editAdd :: a -> b  -- ^ A function that determines the penalty to insert a given item.
    , editRemove :: a -> b  -- ^ A function that determines the penalty of removing a given item.
    , editReplace :: a -> a -> b  -- ^ A function that determines the penalty of replacing a given item with another given item.
    , editTranspose :: a -> a -> b  -- ^ A function that determines the penalty of transposing two items.
  }
  deriving (Functor, Generic, Generic1)

-- | A function to construct an 'EditScore' object where the cost of adding, removing, replacing
-- and transposing all have the same given cost.
constantEditScore
  :: b  -- ^ The given cost for all the operations.
  -> EditScore a b  -- ^ The corresponding 'EditScore' object.
constantEditScore x = EditScore c1 c1 c2 c2
  where c1 = const x
        c2 = const c1

instance Num b => Default (EditScore a b) where
  def = constantEditScore 1

-- | A data type that is used to list how to edit a sequence to form another sequence.
data Edit a
  = Add a  -- ^ We add the given element to the sequence.
  | Rem a  -- ^ We remove the given element to the sequence.
  | Copy a  -- ^ We copy an element from the sequence, this basically act as a /no-op/.
  | Swap a a  -- ^ We modify the given first item into the second item, this thus denotes a replacement.
  | Transpose a a -- ^ We swap two characters for the given string, this edit is only available for the /Damerau-Levenshtein distance/.
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
    put (Transpose xa xb) = putWord8 4 >> put xa >> put xb
    get = do
        tp <- getWord8
        case tp of
          0 -> Add <$> get
          1 -> Rem <$> get
          2 -> Copy <$> get
          3 -> Swap <$> get <*> get
          4 -> Transpose <$> get <*> get
          _ -> fail ("The number " ++ show tp ++ " is not a valid Edit item.")

instance Eq1 Edit where
  liftEq eq = go
    where go (Add xa) (Add xb) = eq xa xb
          go (Rem xa) (Rem xb) = eq xa xb
          go (Copy xa) (Copy xb) = eq xa xb
          go (Swap xa ya) (Swap xb yb) = eq xa xb && eq ya yb
          go (Transpose xa ya) (Transpose xb yb) = eq xa xb && eq ya yb
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
          go (Swap _ _) _ = LT
          go _ (Swap _ _) = GT
          go (Transpose xa ya) (Transpose xb yb) = cmp xa xb <> cmp ya yb

-- | A type alias for a /list/ of 'Edit's.
type Edits a = [Edit a]

-- | A type alias for a 2-tuple where the first item is the /cost/ of the 'Edit's and the second item a list of 'Edit's.
type CostEdits a b = (b, Edits a)

-- | Determine the cost of a given 'Edit' as described with the given 'EditScore' object.
editCost :: Num b
  => EditScore a b  -- ^ An 'EditScore' object that determines how costly each transformation is.
  -> Edit a  -- ^ The given 'Edit' for which we want to calculate the score.
  -> b  -- ^ The given edit distance for the given 'Edit' with the given 'EditScore'.
editCost EditScore { editAdd=ad, editRemove=rm, editReplace=rp, editTranspose=tp } = go
  where go (Add x) = ad x
        go (Rem x) = rm x
        go (Copy _) = 0
        go (Swap x y) = rp x y
        go (Transpose x y) = tp x y

-- | Determine the cost of the given sequence of 'Edit's with the given 'EditScore' object
-- that determines the cost for each edit. The sum of the 'Edit's is returned.
editsCost :: (Foldable f, Num b)
  => EditScore a b  -- ^ An 'EditScore' object that determines how costly each transformation is.
  -> f (Edit a)  -- ^ The given 'Foldable' of 'Edit's for which we want to calculate the score.
  -> b  -- ^ The given edit distance for all the given 'Edit's with the given 'EditScore'.
editsCost es = foldr ((+) . editCost es) 0

-- | Apply the given list of 'Edit's to the given list.
-- If the 'Edit's make sense, it returns the result wrapped
-- in a 'Just', if a check with the item that is removed/replaced
-- fails, the function will return 'Nothing'.
applyEdits :: Eq a
  => Edits a  -- ^ The given list of 'Edit's to apply to the given list.
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
applyEdits (Transpose xa xb : xs) (ya : yb : ys)
  | xa == yb && xb == ya = (yb :) . (ya :) <$> applyEdits xs ys
applyEdits _ _ = Nothing

-- | Get the origin of the edits as a list.
getOrigin
  :: Edits a  -- ^ A list of 'Edit's for which we want to reconstruct the origin as a list.
  -> [a]  -- ^ The original data as a list.
getOrigin = foldr f []
  where f (Rem x) = (x:)
        f (Swap y _) = (y:)
        f (Copy x) = (x:)
        f (Transpose xa xb) = (xb:) . (xa:)
        f (Add _) = id

-- | Get the targets of the edits as a list.
getTarget
  :: Edits a  -- ^ A list of 'Edit's for which we want to reconstruct the target as a list.
  -> [a]  -- ^ The target data as a list.
getTarget = foldr f []
  where f (Add x) = (x:)
        f (Swap _ x) = (x:)
        f (Copy x) = (x:)
        f (Transpose xa xb) = (xa:) . (xb:)
        f (Rem _) = id

-- | Determine the edit distance where an addition, removal, and change all count as one, and where
-- the 'Eq' instance is used to determine whether two items are equivalent, this is for example useful
-- for case-insensitve matching.
levenshteinDistance :: (Foldable f, Foldable g, Eq a, Num b, Ord b)
  => f a  -- ^ The given original sequence.
  -> g a  -- ^ The given target sequence.
  -> b  -- ^ The edit distance between the two 'Foldable's.
levenshteinDistance = levenshteinDistance' (==)

-- | Determine the edit distance together with the steps to transform the first 'Foldable'
-- (as list) into a second 'Foldable' (as list). Add, remove and swapping items all count
-- as one edit distance.
levenshtein :: (Foldable f, Foldable g, Eq a, Num b, Ord b)
  => f a  -- ^ The given original sequence.
  -> g a  -- ^ The given target sequence.
  -> CostEdits a b  -- ^ The edit distance between the two 'Foldable's.
levenshtein = levenshtein' (==)

-- | Determine the edit distance together with the steps to transform the first 'Foldable'
-- (as list) into a second 'Foldable' (as list). Add, remove and swapping items all count
-- as one edit distance. The first parameter is a function to determine if two items
-- are of the 'Foldable's are considered equivalent.
levenshtein' :: (Foldable f, Foldable g, Num b, Ord b)
  => (a -> a -> Bool)  -- ^ The given equivalence relation to work with.
  -> f a  -- ^ The given original sequence.
  -> g a  -- ^ The given target sequence.
  -> CostEdits a b  -- ^ The edit distance between the two 'Foldable's together with a list of 'Edit's to transform the first 'Foldable' to the second one.
levenshtein' = _addDefaults . genericLevenshtein'

-- | Determine the edit distance together with the steps to transform the first 'Foldable'
-- (as list) into a second 'Foldable' (as list). Add, remove and swapping items all count
-- as one edit distance. The equality function '(==)' is used to determine if two items are
-- equivalent.
reversedLevenshtein :: (Foldable f, Foldable g, Eq a, Num b, Ord b)
  => f a  -- ^ The given original sequence.
  -> g a  -- ^ The given target sequence.
  -> CostEdits a b  -- ^ The edit distance between the two 'Foldable's together with the 'Edit's to make to convert the first sequence into the second.
reversedLevenshtein = reversedLevenshtein' (==)

-- | Determine the edit distance together with the steps to transform the first 'Foldable'
-- (as list) into a second 'Foldable' (as list) in /reversed/ order. Add, remove and
-- swapping items all count as one edit distance. The given equality function is used
-- to determine if two items are equivalent.
reversedLevenshtein' :: (Foldable f, Foldable g, Num b, Ord b)
  => (a -> a -> Bool)  -- ^ The given equivalence relation to work with.
  -> f a  -- ^ The given original sequence.
  -> g a  -- ^ The given target sequence.
  -> CostEdits a b  -- ^ The edit distance between the two 'Foldable's together with a reversed list of 'Edit's to transform the original sequence into the target sequence.
reversedLevenshtein' = _addDefaults . genericReversedLevenshtein'

-- | Determine the edit distance to transform the first 'Foldable' (as list)
-- into a second 'Foldable' (as list). Add, remove and swapping items all count
-- as one edit distance. The first parameter is an equivalence relation that
-- is used to determine if two items are considered equivalent.
levenshteinDistance' :: (Foldable f, Foldable g, Num b, Ord b)
  => (a -> a -> Bool)  -- ^ The given equivalence relation to work with.
  -> f a  -- ^ The given original sequence.
  -> g a  -- ^ The given target sequence.
  -> b  -- ^ The edit distance between the two 'Foldable's.
levenshteinDistance' = _addDefaults . genericLevenshteinDistance'

-- | A function to determine the /Levenshtein distance/ by specifying the cost functions of adding, removing and editing characters. This function returns
-- the sum of the costs to transform the first 'Foldable' (as list) into the second 'Foldable' (as list). The '(==)' function is used
-- to determine if two items are equivalent.
genericLevenshteinDistance :: (Foldable f, Foldable g, Eq a, Num b, Ord b)
  => (a -> b)  -- ^ The cost of adding the given item. The return value should be positive.
  -> (a -> b)  -- ^ The cost of removing the given item. The return value should be positive.
  -> (a -> a -> b)  -- ^ The cost that it takes to replace an item of the first parameter with one of the second parameter. The return value should be positive.
  -> f a  -- ^ The given original sequence.
  -> g a  -- ^ The given target sequence.
  -> b  -- ^ The edit distance between the two 'Foldable's.
genericLevenshteinDistance = genericLevenshteinDistance' (==)

-- | Calculate the Levenshtein distance with the given 'EditScore' object that determine how costly each edit is.
-- The function determines the minimal score with 'Add', 'Rem', 'Copy' and 'Swap' edits. We determine if two
-- items are the same with the 'Eq' instance for the item type.
genericLevenshteinDistanceWithScore :: (Foldable f, Foldable g, Eq a, Num b, Ord b)
  => EditScore a b  -- ^ The given 'EditScore' object that determines the cost per edit.
  -> f a  -- ^ The given original sequence.
  -> g a  -- ^ The given target sequence.
  -> b  -- ^ The edit distance between the two 'Foldable's.
genericLevenshteinDistanceWithScore = genericLevenshteinDistanceWithScore' (==)

-- | Calculate the Levenshtein distance with the given equivalence relation, and the given 'EditScore' object that determine how costly each edit is.
-- The function determines the minimal score with 'Add', 'Rem', 'Copy' and 'Swap' edits.
genericLevenshteinDistanceWithScore' :: (Foldable f, Foldable g, Num b, Ord b)
  => (a -> a -> Bool)  -- ^ The given equivalence relation to work with.
  -> EditScore a b  -- ^ The given 'EditScore' object that determines the cost per edit.
  -> f a  -- ^ The given original sequence.
  -> g a  -- ^ The given target sequence.
  -> b  -- ^ The edit distance between the two 'Foldable's.
genericLevenshteinDistanceWithScore' cmp (EditScore ad rm sw _) = genericLevenshteinDistance' cmp ad rm sw

-- | A function to determine the /Levenshtein distance/ by specifying the cost functions of adding, removing and editing characters. This function returns
-- the sum of the costs to transform the first 'Foldable' (as list) into the second 'Foldable' (as list). The first parameter is an equivalence relation
-- to determine if two items are considered equivalent.
genericLevenshteinDistance' :: (Foldable f, Foldable g, Num b, Ord b)
  => (a -> a -> Bool)  -- ^ The given equivalence relation to work with.
  -> (a -> b)  -- ^ The cost of adding the given item. The return value should be positive.
  -> (a -> b)  -- ^ The cost of removing the given item. The return value should be positive.
  -> (a -> a -> b)  -- ^ The cost that it takes to replace an item of the first parameter with one of the second parameter. The return value should be positive.
  -> f a  -- ^ The given original sequence.
  -> g a  -- ^ The given target sequence.
  -> b  -- ^ The edit distance between the two 'Foldable's.
genericLevenshteinDistance' eq ad rm sw xs' ys' = last (foldl (nextRow tl) row0 xs')
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

-- | A function to determine the /Levenshtein distance/ together with a list of 'Edit's
-- to apply to convert the first 'Foldable' (as list) into the second item (as list)
-- The cost functions of adding, removing and editing characters will be used to minimize
-- the total edit distance. The first parameter is an equivalence relation that is used
-- to determine if two items of the 'Foldable's are considered equivalent.
genericLevenshtein' :: (Foldable f, Foldable g, Num b, Ord b)
  => (a -> a -> Bool)  -- ^ The given equivalence relation to work with.
  -> (a -> b)  -- ^ The cost of adding the given item. The return value should be positive.
  -> (a -> b)  -- ^ The cost of removing the given item. The return value should be positive.
  -> (a -> a -> b)  -- ^ The cost that it takes to replace an item of the first parameter with one of the second parameter. The return value should be positive.
  -> f a  -- ^ The given original sequence.
  -> g a  -- ^ The given target sequence.
  -> CostEdits a b  -- ^ A 2-tuple with the edit score as first item, and a list of modifications in /normal/ order as second item to transform the first sequence to the second one.
genericLevenshtein' eq ad rm sw xs' = second reverse . genericReversedLevenshtein' eq ad rm sw xs'

-- | A function to determine the /Levenshtein distance/ together with a list of 'Edit's
-- to apply to convert the first 'Foldable' (as list) into the second item (as list)
-- The cost functions of adding, removing and editing characters will be used to minimize
-- the total edit distance. The '(==)' function is used to determine if two items of the
-- 'Foldable's are considered equivalent.
genericLevenshtein :: (Foldable f, Foldable g, Eq a, Num b, Ord b)
  => (a -> b)  -- ^ The cost of adding the given item. The return value should be positive.
  -> (a -> b)  -- ^ The cost of removing the given item. The return value should be positive.
  -> (a -> a -> b)  -- ^ The cost that it takes to replace an item of the first parameter with one of the second parameter. The return value should be positive.
  -> f a  -- ^ The given original sequence.
  -> g a  -- ^ The given target sequence.
  -> CostEdits a b  -- ^ A 2-tuple with the edit score as first item, and a list of modifications in /normal/ order as second item to transform the first sequence to the second one.
genericLevenshtein = genericLevenshtein' (==)

-- | Calculate the Levenshtein distance and the modifications with the given equivalence relation, and the given 'EditScore' object that determine how costly each edit is.
-- The function determines the minimal score with 'Add', 'Rem', 'Copy' and 'Swap' edits.
genericLevenshteinWithScore' :: (Foldable f, Foldable g, Num b, Ord b)
  => (a -> a -> Bool)  -- ^ The given equivalence relation to determine if two items are the same.
  -> EditScore a b  -- ^ The given 'EditScore' object that specifies the cost of each mutation (add, remove, replace).
  -> f a  -- ^ The given original sequence.
  -> g a  -- ^ The given target sequence.
  -> CostEdits a b  -- ^ A 2-tuple with the edit score as first item, and a list of modifications as second item to transform the first 'Foldable' (as list) to the second 'Foldable' (as list).
genericLevenshteinWithScore' eq (EditScore ad rm sw _) = genericLevenshtein' eq ad rm sw

-- | Calculate the Levenshtein distance and the modifications with the given 'EditScore' object that determine how costly each edit is.
-- The function determines the minimal score with 'Add', 'Rem', 'Copy' and 'Swap' edits. The 'Eq' instance of the elements is used
-- to determine if two items are equivalent.
genericLevenshteinWithScore :: (Foldable f, Foldable g, Eq a, Num b, Ord b)
  => EditScore a b  -- ^ The given 'EditScore' object that specifies the cost of each mutation (add, remove, replace).
  -> f a  -- ^ The given original sequence.
  -> g a  -- ^ The given target sequence.
  -> CostEdits a b  -- ^ A 2-tuple with the edit score as first item, and a list of modifications as second item to transform the first 'Foldable' (as list) to the second 'Foldable' (as list).
genericLevenshteinWithScore = genericLevenshteinWithScore' (==)

-- | A function to determine the /Levenshtein distance/ together with a list of 'Edit's
-- to apply to convert the first 'Foldable' (as list) into the second item (as list)
-- in /reversed/ order. The cost functions of adding, removing and editing characters
-- will be used to minimize the total edit distance. The first parameter is an
-- equivalence relation that is used to determine if two items of the 'Foldable's are considered equivalent.
genericReversedLevenshtein' :: (Foldable f, Foldable g, Num b, Ord b)
  => (a -> a -> Bool)  -- ^ The given equivalence relation to work with.
  -> (a -> b)  -- ^ The cost of adding the given item. The return value should be positive.
  -> (a -> b)  -- ^ The cost of removing the given item. The return value should be positive.
  -> (a -> a -> b)  -- ^ The cost that it takes to replace an item of the first parameter with one of the second parameter. The return value should be positive.
  -> f a  -- ^ The given original sequence.
  -> g a  -- ^ The given target sequence.
  -> CostEdits a b  -- ^ A 2-tuple with the edit score as first item, and a list of modifications in /reversed/ order as second item to transform the first 'Foldable' (as list) to the second 'Foldable' (as list).
genericReversedLevenshtein' eq ad rm sw xs' ys' = last (foldl (nextRow tl) row0 xs')
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

-- | A function to determine the /Levenshtein distance/ together with a list of 'Edit's
-- to apply to convert the first 'Foldable' (as list) into the second item (as list)
-- in /reversed/ order. The cost functions of adding, removing and editing characters
-- will be used to minimize the total edit distance. The '(==)' function is used
-- to determine if two items of the 'Foldable's are considered equivalent.
genericReversedLevenshtein :: (Foldable f, Foldable g, Eq a, Num b, Ord b)
  => (a -> b)  -- ^ The cost of adding the given item. The return value should be positive.
  -> (a -> b)  -- ^ The cost of removing the given item. The return value should be positive.
  -> (a -> a -> b)  -- ^ The cost that it takes to replace an item of the first parameter with one of the second parameter. The return value should be positive.
  -> f a  -- ^ The given original sequence.
  -> g a  -- ^ The given target sequence.
  -> CostEdits a b  -- ^ A 2-tuple with the edit score as first item, and a list of modifications in /reversed/ order as second item to transform the first 'Foldable' (as list) to the second 'Foldable' (as list).
genericReversedLevenshtein = genericReversedLevenshtein' (==)

-- | Calculate the Levenshtein distance and the modifications with the given equivalence relation, and the given 'EditScore' object that determine how costly each edit is.
-- The function determines the minimal score with 'Add', 'Rem', 'Copy' and 'Swap' edits.
genericReversedLevenshteinWithScore' :: (Foldable f, Foldable g, Num b, Ord b)
  => (a -> a -> Bool)  -- ^ The given equivalence relation to determine if two items are the same.
  -> EditScore a b  -- ^ The given 'EditScore' object that specifies the cost of each mutation (add, remove, replace).
  -> f a  -- ^ The given original sequence.
  -> g a  -- ^ The given target sequence.
  -> CostEdits a b  -- ^ A 2-tuple with the edit score as first item, and a list of modifications in /reversed/ order as second item to transform the first 'Foldable' (as list) to the second 'Foldable' (as list).
genericReversedLevenshteinWithScore' cmp (EditScore ad rm sw _) = genericReversedLevenshtein' cmp ad rm sw

-- | Calculate the Levenshtein distance and the modifications with the given 'EditScore' object that determine how costly each edit is.
-- The function determines the minimal score with 'Add', 'Rem', 'Copy' and 'Swap' edits. The 'Eq' instance of the items will determine
-- the equivalence relation.
genericReversedLevenshteinWithScore :: (Foldable f, Foldable g, Eq a, Num b, Ord b)
  => EditScore a b  -- ^ The given 'EditScore' object that specifies the cost of each mutation (add, remove, replace).
  -> f a  -- ^ The given original sequence.
  -> g a  -- ^ The given target sequence.
  -> CostEdits a b  -- ^ A 2-tuple with the edit score as first item, and a list of modifications in /reversed/ order as second item to transform the first 'Foldable' (as list) to the second 'Foldable' (as list).
genericReversedLevenshteinWithScore = genericReversedLevenshteinWithScore' (==)


-- DAMERAU

{-
-- | Determine the edit distance where an addition, removal, and change all count as one, and where
-- the 'Eq' instance is used to determine whether two items are equivalent, this is for example useful
-- for case-insensitve matching.
levenshteinDistance :: (Foldable f, Foldable g, Eq a, Num b, Ord b)
  => f a  -- ^ The given original sequence.
  -> g a  -- ^ The given target sequence.
  -> b  -- ^ The edit distance between the two 'Foldable's.
levenshteinDistance = levenshteinDistance' (==)

-- | Determine the edit distance together with the steps to transform the first 'Foldable'
-- (as list) into a second 'Foldable' (as list). Add, remove and swapping items all count
-- as one edit distance.
levenshtein :: (Foldable f, Foldable g, Eq a, Num b, Ord b)
  => f a  -- ^ The given original sequence.
  -> g a  -- ^ The given target sequence.
  -> (b, Edits a)  -- ^ The edit distance between the two 'Foldable's.
levenshtein = levenshtein' (==)

-- | Determine the edit distance together with the steps to transform the first 'Foldable'
-- (as list) into a second 'Foldable' (as list). Add, remove and swapping items all count
-- as one edit distance. The first parameter is a function to determine if two items
-- are of the 'Foldable's are considered equivalent.
levenshtein' :: (Foldable f, Foldable g, Num b, Ord b)
  => (a -> a -> Bool)  -- ^ The given equivalence relation to work with.
  -> f a  -- ^ The given original sequence.
  -> g a  -- ^ The given target sequence.
  -> (b, Edits a)  -- ^ The edit distance between the two 'Foldable's together with a list of 'Edit's to transform the first 'Foldable' to the second one.
levenshtein' = _addDefaults . genericLevenshtein'

-- | Determine the edit distance together with the steps to transform the first 'Foldable'
-- (as list) into a second 'Foldable' (as list). Add, remove and swapping items all count
-- as one edit distance. The equality function '(==)' is used to determine if two items are
-- equivalent.
reversedLevenshtein :: (Foldable f, Foldable g, Eq a, Num b, Ord b)
  => f a  -- ^ The given original sequence.
  -> g a  -- ^ The given target sequence.
  -> (b, Edits a)  -- ^ The edit distance between the two 'Foldable's together with the 'Edit's to make to convert the first sequence into the second.
reversedLevenshtein = reversedLevenshtein' (==)

-- | Determine the edit distance together with the steps to transform the first 'Foldable'
-- (as list) into a second 'Foldable' (as list) in /reversed/ order. Add, remove and
-- swapping items all count as one edit distance. The given equality function is used
-- to determine if two items are equivalent.
reversedLevenshtein' :: (Foldable f, Foldable g, Num b, Ord b)
  => (a -> a -> Bool)  -- ^ The given equivalence relation to work with.
  -> f a  -- ^ The given original sequence.
  -> g a  -- ^ The given target sequence.
  -> (b, Edits a)  -- ^ The edit distance between the two 'Foldable's together with a reversed list of 'Edit's to transform the original sequence into the target sequence.
reversedLevenshtein' = _addDefaults . genericReversedLevenshtein'

-- | Determine the edit distance to transform the first 'Foldable' (as list)
-- into a second 'Foldable' (as list). Add, remove and swapping items all count
-- as one edit distance. The first parameter is an equivalence relation that
-- is used to determine if two items are considered equivalent.
levenshteinDistance' :: (Foldable f, Foldable g, Num b, Ord b)
  => (a -> a -> Bool)  -- ^ The given equivalence relation to work with.
  -> f a  -- ^ The given original sequence.
  -> g a  -- ^ The given target sequence.
  -> b  -- ^ The edit distance between the two 'Foldable's.
levenshteinDistance' = _addDefaults . genericLevenshteinDistance'

-- | A function to determine the /Levenshtein distance/ by specifying the cost functions of adding, removing and editing characters. This function returns
-- the sum of the costs to transform the first 'Foldable' (as list) into the second 'Foldable' (as list). The '(==)' function is used
-- to determine if two items are equivalent.
genericLevenshteinDistance :: (Foldable f, Foldable g, Eq a, Num b, Ord b)
  => (a -> b)  -- ^ The cost of adding the given item. The return value should be positive.
  -> (a -> b)  -- ^ The cost of removing the given item. The return value should be positive.
  -> (a -> a -> b)  -- ^ The cost that it takes to replace an item of the first parameter with one of the second parameter. The return value should be positive.
  -> f a  -- ^ The given original sequence.
  -> g a  -- ^ The given target sequence.
  -> b  -- ^ The edit distance between the two 'Foldable's.
genericLevenshteinDistance = genericLevenshteinDistance' (==)

-- | Calculate the Levenshtein distance with the given 'EditScore' object that determine how costly each edit is.
-- The function determines the minimal score with 'Add', 'Rem', 'Copy' and 'Swap' edits. We determine if two
-- items are the same with the 'Eq' instance for the item type.
genericLevenshteinDistanceWithScore :: (Foldable f, Foldable g, Eq a, Num b, Ord b)
  => EditScore a b  -- ^ The given 'EditScore' object that determines the cost per edit.
  -> f a  -- ^ The given original sequence.
  -> g a  -- ^ The given target sequence.
  -> b  -- ^ The edit distance between the two 'Foldable's.
genericLevenshteinDistanceWithScore = genericLevenshteinDistanceWithScore' (==)

-- | Calculate the Levenshtein distance with the given equivalence relation, and the given 'EditScore' object that determine how costly each edit is.
-- The function determines the minimal score with 'Add', 'Rem', 'Copy' and 'Swap' edits.
genericLevenshteinDistanceWithScore' :: (Foldable f, Foldable g, Num b, Ord b)
  => (a -> a -> Bool)  -- ^ The given equivalence relation to work with.
  -> EditScore a b  -- ^ The given 'EditScore' object that determines the cost per edit.
  -> f a  -- ^ The given original sequence.
  -> g a  -- ^ The given target sequence.
  -> b  -- ^ The edit distance between the two 'Foldable's.
genericLevenshteinDistanceWithScore' cmp (EditScore ad rm sw _) = genericLevenshteinDistance' cmp ad rm sw

-- | A function to determine the /Levenshtein distance/ by specifying the cost functions of adding, removing and editing characters. This function returns
-- the sum of the costs to transform the first 'Foldable' (as list) into the second 'Foldable' (as list). The first parameter is an equivalence relation
-- to determine if two items are considered equivalent.
genericLevenshteinDistance' :: (Foldable f, Foldable g, Num b, Ord b)
  => (a -> a -> Bool)  -- ^ The given equivalence relation to work with.
  -> (a -> b)  -- ^ The cost of adding the given item. The return value should be positive.
  -> (a -> b)  -- ^ The cost of removing the given item. The return value should be positive.
  -> (a -> a -> b)  -- ^ The cost that it takes to replace an item of the first parameter with one of the second parameter. The return value should be positive.
  -> f a  -- ^ The given original sequence.
  -> g a  -- ^ The given target sequence.
  -> b  -- ^ The edit distance between the two 'Foldable's.
genericLevenshteinDistance' eq ad rm sw xs' ys' = last (foldl (nextRow tl) row0 xs')
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

-- | A function to determine the /Levenshtein distance/ together with a list of 'Edit's
-- to apply to convert the first 'Foldable' (as list) into the second item (as list)
-- The cost functions of adding, removing and editing characters will be used to minimize
-- the total edit distance. The first parameter is an equivalence relation that is used
-- to determine if two items of the 'Foldable's are considered equivalent.
genericLevenshtein' :: (Foldable f, Foldable g, Num b, Ord b)
  => (a -> a -> Bool)  -- ^ The given equivalence relation to work with.
  -> (a -> b)  -- ^ The cost of adding the given item. The return value should be positive.
  -> (a -> b)  -- ^ The cost of removing the given item. The return value should be positive.
  -> (a -> a -> b)  -- ^ The cost that it takes to replace an item of the first parameter with one of the second parameter. The return value should be positive.
  -> f a  -- ^ The given original sequence.
  -> g a  -- ^ The given target sequence.
  -> (b, Edits a)  -- ^ A 2-tuple with the edit score as first item, and a list of modifications in /normal/ order as second item to transform the first sequence to the second one.
genericLevenshtein' eq ad rm sw xs' = second reverse . genericReversedLevenshtein' eq ad rm sw xs'

-- | A function to determine the /Levenshtein distance/ together with a list of 'Edit's
-- to apply to convert the first 'Foldable' (as list) into the second item (as list)
-- The cost functions of adding, removing and editing characters will be used to minimize
-- the total edit distance. The '(==)' function is used to determine if two items of the
-- 'Foldable's are considered equivalent.
genericLevenshtein :: (Foldable f, Foldable g, Eq a, Num b, Ord b)
  => (a -> b)  -- ^ The cost of adding the given item. The return value should be positive.
  -> (a -> b)  -- ^ The cost of removing the given item. The return value should be positive.
  -> (a -> a -> b)  -- ^ The cost that it takes to replace an item of the first parameter with one of the second parameter. The return value should be positive.
  -> f a  -- ^ The given original sequence.
  -> g a  -- ^ The given target sequence.
  -> (b, Edits a)  -- ^ A 2-tuple with the edit score as first item, and a list of modifications in /normal/ order as second item to transform the first sequence to the second one.
genericLevenshtein = genericLevenshtein' (==)

-- | Calculate the Levenshtein distance and the modifications with the given equivalence relation, and the given 'EditScore' object that determine how costly each edit is.
-- The function determines the minimal score with 'Add', 'Rem', 'Copy' and 'Swap' edits.
genericLevenshteinWithScore' :: (Foldable f, Foldable g, Num b, Ord b)
  => (a -> a -> Bool)  -- ^ The given equivalence relation to determine if two items are the same.
  -> EditScore a b  -- ^ The given 'EditScore' object that specifies the cost of each mutation (add, remove, replace).
  -> f a  -- ^ The given original sequence.
  -> g a  -- ^ The given target sequence.
  -> (b, Edits a)  -- ^ A 2-tuple with the edit score as first item, and a list of modifications as second item to transform the first 'Foldable' (as list) to the second 'Foldable' (as list).
genericLevenshteinWithScore' eq (EditScore ad rm sw _) = genericLevenshtein' eq ad rm sw

-- | Calculate the Levenshtein distance and the modifications with the given 'EditScore' object that determine how costly each edit is.
-- The function determines the minimal score with 'Add', 'Rem', 'Copy' and 'Swap' edits. The 'Eq' instance of the elements is used
-- to determine if two items are equivalent.
genericLevenshteinWithScore :: (Foldable f, Foldable g, Eq a, Num b, Ord b)
  => EditScore a b  -- ^ The given 'EditScore' object that specifies the cost of each mutation (add, remove, replace).
  -> f a  -- ^ The given original sequence.
  -> g a  -- ^ The given target sequence.
  -> (b, Edits a)  -- ^ A 2-tuple with the edit score as first item, and a list of modifications as second item to transform the first 'Foldable' (as list) to the second 'Foldable' (as list).
genericLevenshteinWithScore = genericLevenshteinWithScore' (==)
--}

-- | A function to determine the /Levenshtein distance/ together with a list of 'Edit's
-- to apply to convert the first 'Foldable' (as list) into the second item (as list)
-- in /reversed/ order. The cost functions of adding, removing and editing characters
-- will be used to minimize the total edit distance. The first parameter is an
-- equivalence relation that is used to determine if two items of the 'Foldable's are considered equivalent.
genericReversedDamerauLevenshtein' :: (Foldable f, Foldable g, Num b, Ord b)
  => (a -> a -> Bool)  -- ^ The given equivalence relation to work with.
  -> (a -> b)  -- ^ The cost of adding the given item. The return value should be positive.
  -> (a -> b)  -- ^ The cost of removing the given item. The return value should be positive.
  -> (a -> a -> b)  -- ^ The cost that it takes to replace an item of the first parameter with one of the second parameter. The return value should be positive.
  -> (a -> a -> b)  -- ^ The cost of swapping two consecutive characters.
  -> f a  -- ^ The given original sequence.
  -> g a  -- ^ The given target sequence.
  -> (b, Edits a)  -- ^ A 2-tuple with the edit score as first item, and a list of modifications in /reversed/ order as second item to transform the first 'Foldable' (as list) to the second 'Foldable' (as list).
genericReversedDamerauLevenshtein' eq ad rm sw tr xs' ys' = last (foldl (nextRow' tl) row0 xs')
  where
    row0 = scanl (\(w, is) i -> (w+ad i, Add i: is)) (0, []) tl
    nextCell' x (l, le) y (lt, lte) (t, te)
      | eq x y = (lt, Copy x : lte)
      | scs <= scr && scs <= sca = (scs, Swap x y:lte)
      | sca <= scr = (sca, Add y:le)
      | otherwise = (scr, Transpose y x:lte)  -- x and x2 and lltte
      | otherwise = (scr, Rem x:te)
      where sca = l + ad y
            scr = t + rm x
            scs = lt + sw x y
            sct = lt + tr x y -- lltt ()
    curryNextCell' x l = uncurry (uncurry (nextCell' x l))
    nextRow' ys da@(~(~(dn, de):ds)) x = scanl (curryNextCell' x) (dn+rm x,Rem x:de) (zip (zip ys da) ds)
    tl = toList ys'

{-
-- | A function to determine the /Levenshtein distance/ together with a list of 'Edit's
-- to apply to convert the first 'Foldable' (as list) into the second item (as list)
-- in /reversed/ order. The cost functions of adding, removing and editing characters
-- will be used to minimize the total edit distance. The '(==)' function is used
-- to determine if two items of the 'Foldable's are considered equivalent.
genericReversedLevenshtein :: (Foldable f, Foldable g, Eq a, Num b, Ord b)
  => (a -> b)  -- ^ The cost of adding the given item. The return value should be positive.
  -> (a -> b)  -- ^ The cost of removing the given item. The return value should be positive.
  -> (a -> a -> b)  -- ^ The cost that it takes to replace an item of the first parameter with one of the second parameter. The return value should be positive.
  -> f a  -- ^ The given original sequence.
  -> g a  -- ^ The given target sequence.
  -> (b, Edits a)  -- ^ A 2-tuple with the edit score as first item, and a list of modifications in /reversed/ order as second item to transform the first 'Foldable' (as list) to the second 'Foldable' (as list).
genericReversedLevenshtein = genericReversedLevenshtein' (==)

-- | Calculate the Levenshtein distance and the modifications with the given equivalence relation, and the given 'EditScore' object that determine how costly each edit is.
-- The function determines the minimal score with 'Add', 'Rem', 'Copy' and 'Swap' edits.
genericReversedLevenshteinWithScore' :: (Foldable f, Foldable g, Num b, Ord b)
  => (a -> a -> Bool)  -- ^ The given equivalence relation to determine if two items are the same.
  -> EditScore a b  -- ^ The given 'EditScore' object that specifies the cost of each mutation (add, remove, replace).
  -> f a  -- ^ The given original sequence.
  -> g a  -- ^ The given target sequence.
  -> (b, Edits a)  -- ^ A 2-tuple with the edit score as first item, and a list of modifications in /reversed/ order as second item to transform the first 'Foldable' (as list) to the second 'Foldable' (as list).
genericReversedLevenshteinWithScore' cmp (EditScore ad rm sw _) = genericReversedLevenshtein' cmp ad rm sw

-- | Calculate the Levenshtein distance and the modifications with the given 'EditScore' object that determine how costly each edit is.
-- The function determines the minimal score with 'Add', 'Rem', 'Copy' and 'Swap' edits. The 'Eq' instance of the items will determine
-- the equivalence relation.
genericReversedLevenshteinWithScore :: (Foldable f, Foldable g, Eq a, Num b, Ord b)
  => EditScore a b  -- ^ The given 'EditScore' object that specifies the cost of each mutation (add, remove, replace).
  -> f a  -- ^ The given original sequence.
  -> g a  -- ^ The given target sequence.
  -> (b, Edits a)  -- ^ A 2-tuple with the edit score as first item, and a list of modifications in /reversed/ order as second item to transform the first 'Foldable' (as list) to the second 'Foldable' (as list).
genericReversedLevenshteinWithScore = genericReversedLevenshteinWithScore' (==)
-}
