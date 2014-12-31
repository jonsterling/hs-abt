{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UnicodeSyntax #-}

module Abt.Class.HEq1 where

import Data.Vinyl

-- | Uniform variant of 'Eq' for indexed types. This is different from
-- 'Data.Functor.Eq1' in that it is properly kind polymorphic and crucially
-- heterogeneous, and it places no constraint on the index.
--
class HEq1 f where
  (===) ∷ f i → f j → Bool

instance HEq1 el ⇒ HEq1 (Rec el) where
  RNil === RNil = True
  (x :& xs) === (y :& ys) = x === y && xs === ys
  _ === _ = False
