{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnicodeSyntax #-}

module Abt.Types.View
( View(..)
, mapView
) where

import Abt.Types.Nat
import Abt.Types.HList

-- | @v@ is the type of variables; @o@ is the type of operators parameterized
-- by arities; @n@ is the "higher type" of the term (i.e. a term has @n=0@, a
-- single binding has @n=1@, etc.); @φ@ is the functor which interprets the
-- inner structure of the view.
--
data View (v ∷ *) (o ∷ [Nat] → *) (n ∷ Nat) (φ ∷ Nat → *) where
  V ∷ v → View v o Z φ
  (:\) ∷ v → φ n → View v o (S n) φ
  (:$) ∷ o ns → HList φ ns → View v o Z φ

infixl 2 :$

-- | Views are a (higher) functor.
--
mapView
  ∷ (∀ j. φ j → ψ j) -- ^ a natural transformation @φ → ψ@
  → View v o n φ -- ^ a view at @φ@
  → View v o n ψ
mapView eta = \case
  V v → V v
  v :\ e → v :\ eta e
  o :$ es → o :$ hmap eta es
