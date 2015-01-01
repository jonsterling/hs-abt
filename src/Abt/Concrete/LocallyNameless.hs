{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnicodeSyntax #-}

module Abt.Concrete.LocallyNameless
( Tm(..)
, Tm0
, _TmOp
, Var(..)
, varName
, varIndex
) where

import Abt.Types.Nat
import Abt.Types.View
import Abt.Class.HEq1
import Abt.Class.Show1
import Abt.Class.Abt
import Abt.Class.Monad

import Control.Applicative
import Data.Profunctor
import Data.Vinyl

-- | A variable is a De Bruijn index, optionally decorated with a display name.
data Var
  = Var
  { _varName ∷ !(Maybe String)
  , _varIndex ∷ !Int
  }

instance Show Var where
  show (Var (Just v) _) = v
  show (Var Nothing i) = "@" ++ show i

instance Eq Var where
  (Var _ i) == (Var _ j) = i == j

instance Ord Var where
  compare (Var _ i) (Var _ j) = compare i j

-- | A lens for '_varName'.
--
-- @
-- 'varName' ∷ Lens' 'Var' ('Maybe' 'String')
-- @
--
varName
  ∷ Functor f
  ⇒ (Maybe String → f (Maybe String))
  → Var
  → f Var
varName i (Var n j) =
  (\n' → Var n' j)
    <$> i n

-- | A lens for '_varIndex'.
--
-- @
-- 'varIndex' ∷ Lens' 'Var' 'Int'
-- @
--
varIndex
  ∷ Functor f
  ⇒ (Int → f Int)
  → Var
  → f Var
varIndex i (Var n j) =
  (\j' → Var n j')
    <$> i j

-- | Locally nameless terms with operators in @o@ at order @n@.
--
data Tm (o ∷ [Nat] → *) (n ∷ Nat) where
  Free ∷ Var → Tm0 o
  Bound ∷ Int → Tm0 o
  Abs ∷ Tm o n → Tm o (S n)
  App ∷ o ns → Rec (Tm o) ns → Tm0 o

-- | First order terms (i.e. terms not headed by abstractions).
--
type Tm0 o = Tm o Z

instance HEq1 o ⇒ HEq1 (Tm o) where
  heq1 (Free v1) (Free v2) | v1 == v2 = Just Refl
  heq1 (Bound m) (Bound n) | m == n = Just Refl
  heq1 (Abs e1) (Abs e2) = cong <$> heq1 e1 e2
  heq1 (App o1 es1) (App o2 es2)
    | Just Refl ← heq1 o1 o2
    , Just Refl ← heq1 es1 es2 = Just Refl
  heq1 _ _ = Nothing

shiftVar
  ∷ Var
  → Int
  → Tm o n
  → Tm o n
shiftVar v n = \case
  Free v' → if v == v' then Bound n else Free v'
  Bound m → Bound m
  Abs e → Abs $ shiftVar v (n + 1) e
  App p es → App p $ shiftVar v n <<$>> es

addVar
  ∷ Var
  → Int
  → Tm o n
  → Tm o n
addVar v n = \case
  Free v' → Free v'
  Bound m → if m == n then Free v else Bound m
  Abs e → Abs $ addVar v (n + 1) e
  App p es → App p $ addVar v n <<$>> es

instance Show1 o ⇒ Abt Var o (Tm o) where
  into = \case
    V v → Free v
    v :\ e → Abs $ shiftVar v 0 e
    v :$ es → App v es

  out = \case
    Free v → return $ V v
    Bound _ → fail "bound variable occured in out"
    Abs e → do
      v ← fresh
      return $ v :\ addVar v 0 e
    App p es → return $ p :$ es

-- | A prism to extract arguments from a proposed operator.
--
-- @
-- '_TmOp' ∷ 'HEq1' o ⇒ o ns → Prism' ('Tm0' o) ('Rec' ('Tm0' o) ns)
-- @
--
_TmOp
  ∷ ( Choice p
    , Applicative f
    , HEq1 o
    )
  ⇒ o ns
  → p (Rec (Tm o) ns) (f (Rec (Tm o) ns))
  → p (Tm0 o) (f (Tm0 o))
_TmOp o = dimap fro (either pure (fmap (App o))) . right'
  where
    fro = \case
      App o' es | Just Refl ← heq1 o o' → Right es
      e → Left e
