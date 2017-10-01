{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Abt.Types.View
( View(..)
, View0
, _ViewOp
, mapView
) where

import Abt.Class.HEq1
import Abt.Types.Nat

import Data.Profunctor
import Data.Typeable hiding (Refl)
import Data.Vinyl

-- | @v@ is the type of variables; @o@ is the type of operators parameterized
-- by arities; @n@ is the "higher type"/valence of the term (i.e. a term has
-- @n=0@, a single binding has @n=1@, etc.); @phi@ is the functor which
-- interprets the inner structure of the view.
--
data View (v :: *) (o :: [Nat] -> *) (n :: Nat) (phi :: Nat -> *) where
  V :: v -> View0 v o phi
  (:\) :: v -> phi n -> View v o ('S n) phi
  (:$) :: o ns -> Rec phi ns -> View0 v o phi

deriving instance Typeable View

infixl 2 :$

-- | First order term views.
--
type View0 v o phi = View v o 'Z phi

-- | Views are a (higher) functor.
--
mapView
  :: (forall j . phi j -> psi j) -- ^ a natural transformation @phi -> psi@
  -> View v o n phi -- ^ a view at @phi@
  -> View v o n psi
mapView η = \case
  V v -> V v
  v :\ e -> v :\ η e
  o :$ es -> o :$ η <<$>> es

-- | A prism to extract arguments from a proposed operator.
--
-- @
-- '_ViewOp' :: 'HEq1' o => o ns -> Prism' ('View0' v o phi) ('Rec' phi ns)
-- @
--
_ViewOp
  :: ( Choice p
    , Applicative f
    , HEq1 o
    )
  => o ns
  -> p (Rec phi ns) (f (Rec phi ns))
  -> p (View0 v o phi) (f (View0 v o phi))
_ViewOp o = dimap fro (either pure (fmap (o :$))) . right'
  where
    fro = \case
      o' :$ es | Just Refl <- heq1 o o' -> Right es
      e -> Left e
