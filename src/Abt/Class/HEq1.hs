{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}

module Abt.Class.HEq1 where

import Control.Applicative
import Data.Vinyl

-- | Essentially, Martin-Löf's identity type.
--
data a :=: b where
  Refl ∷ a :=: a

-- | Type constructors are extensional.
--
cong ∷ a :=: b → f a :=: f b
cong Refl = Refl

-- | Uniform variant of 'Eq' for indexed types. This is different from
-- 'Data.Functor.Eq1' in that it is properly kind polymorphic and crucially
-- heterogeneous, and it places no constraint on the index. Because it is
-- heterogeneous, it is useful to project equality in the base space from
-- equality in the total space.
--
class HEq1 f where
  -- | When both sides are equal, give in addition a proof that their indices
  -- are equal; otherwise return 'Nothing'.
  --
  heq1 ∷ f i → f j → Maybe (i :=: j)

  -- | A boolean version of 'heq1', which must agree with it.
  --
  (===) ∷ f i → f j → Bool
  x === y = maybe False (const True) $ heq1 x y

instance HEq1 el ⇒ HEq1 (Rec el) where
  heq1 RNil RNil = Just Refl
  heq1 (x :& xs) (y :& ys)
    | Just Refl ← heq1 x y = cong <$> heq1 xs ys
  heq1 _ _ = Nothing
