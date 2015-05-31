{-# LANGUAGE
    KindSignatures
  , MultiParamTypeClasses
  , FlexibleInstances
  #-}

module LText.Internal.Classes where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Foldable as Fold

class SetLike (c :: * -> *) where
  empty :: c a
  union :: Ord a => c a -> c a -> c a
  intersection :: Ord a => c a -> c a -> c a
  difference :: Ord a => c a -> c a -> c a

instance Ord k => SetLike (Map.Map k) where
  empty = Map.empty
  union = Map.union
  intersection = Map.intersection
  difference = Map.difference

instance SetLike Set.Set where
  empty = Set.empty
  union = Set.union
  intersection = Set.intersection
  difference = Set.difference


class ( SetLike vars
      ) => Bindable vars name a where
  fv :: a -> vars name

class ( SetLike (subst name)
      ) => Substitutable subst name domain a where
  apply :: subst name domain -> a -> a


instance ( Fold.Foldable f
         , Ord name
         , Bindable vars name a
         ) => Bindable vars name (f a) where
  fv = Fold.foldr (union . fv) empty

instance ( Functor f
         , Substitutable subst name domain a
         ) => Substitutable subst name domain (f a) where
  apply s = fmap (apply s)
