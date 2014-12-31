{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnicodeSyntax #-}

module Abt.Class.Abt
( Abt(..)
) where

import Abt.Types.Nat
import Abt.Types.HList
import Abt.Types.View
import Abt.Class.Monad
import Abt.Class.Show1

import Control.Applicative
import qualified Data.List as L

-- | The 'Abt' signature represents mediation between an arbitrary (possibly
-- nameless) term representaion, and a first-order one (the 'View'). Based on
-- the (effectful) ismorphism @'into' / 'out'@ between representations, many
-- operations can be defined generically for arbitrary operator sets, including
-- substitution and aggregation of free variables.
--
class (Show1 o, Show v) ⇒ Abt (v ∷ *) (o ∷ [Nat] → *) (t ∷ Nat → *) | t → v o, o → t where
  -- | Convert a 'View' into a term.
  --
  into
    ∷ View v o n t
    → t n

  -- | Convert a term into a first-order 'View'.
  --
  out
    ∷ MonadVar v m
    ⇒ t n
    → m (View v o n t)

  -- | The injection from variables to terms.
  --
  var
    ∷ v
    → t Z
  var = into . V

  -- | Construct an abstraction.
  --
  (\\)
    ∷ v
    → t n
    → t (S n)
  v \\ e = into $ (v :\ e)

  -- | Construct an operator term.
  --
  ($$)
    ∷ o ns
    → HList t ns
    → t Z
  o $$ es = into $ o :$ es
  infixl 1 $$

  -- | Substitute a term for a variable.
  --
  subst
    ∷ MonadVar v m
    ⇒ t Z
    → v
    → t n
    → m (t n)
  subst e v e' = do
    oe' ← out e'
    case oe' of
      V v' → return $ if v == v' then e else e'
      v' :\ e'' → do
        e''' ← subst e v e''
        return $ v' \\ e'''
      o :$ es → do
        es' ← htraverse (subst e v) es
        return $ o $$ es'

  -- | Instantiate the bound variable of an abstraction.
  --
  (//)
    ∷ MonadVar v m
    ⇒ t (S n)
    → t Z
    → m (t n)
  xe // e' = do
    v :\ e ← out xe
    subst e' v e

  -- | Compute the free variables of a term.
  --
  freeVars
    ∷ MonadVar v m
    ⇒ t n
    → m [v]
  freeVars e = do
    oe ← out e
    case oe of
      V v → return [v]
      v :\ e' → do
        L.delete v <$>
          freeVars e'
      o :$ es → do
        concat <$>
          homogenizeA freeVars es

  -- | Render a term into a human-readable string.
  --
  toString
    ∷ MonadVar v m
    ⇒ t n
    → m String
  toString e = do
    vu ← out e
    case vu of
      V v → return $ show v
      v :\ e → do
        e' ← toString e
        return $ show v ++ "." ++ e'
      o :$ es → do
        es' ← homogenizeA toString es
        return $ show1 o ++ "[" ++ L.intercalate ";" es' ++ "]"

