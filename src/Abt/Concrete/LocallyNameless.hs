{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnicodeSyntax #-}

module Abt.Concrete.LocallyNameless
( Tm(..)
) where

import Abt.Types.Nat
import Abt.Types.HList
import Abt.Types.View
import Abt.Class.HEq1
import Abt.Class.Show1
import Abt.Class.Abt
import Abt.Class.Monad
import Abt.Concrete.Var

-- | Locally nameless terms with operators in @o@ at arity @n@.
data Tm (o ∷ [Nat] → *) (n ∷ Nat) where
  Free ∷ Var → Tm o Z
  Bound ∷ Int → Tm o Z
  Abs ∷ Tm o n → Tm o (S n)
  App ∷ o ns → HList (Tm o) ns → Tm o Z

instance HEq1 o ⇒ HEq1 (Tm o) where
  Free v1 === Free v2 = v1 == v2
  Bound m === Bound n = m == n
  Abs e1 === Abs e2 = e1 === e2
  App o1 es1 === App o2 es2 = o1 === o2 && es1 === es2
  _ === _ = False

shiftVar
  ∷ Var
  → Int
  → Tm o n
  → Tm o n
shiftVar v n = \case
  Free v' → if v == v' then Bound n else Free v'
  Bound m → Bound m
  Abs e → Abs (shiftVar v (n + 1) e)
  App p es → App p $ hmap (shiftVar v n) es

addVar
  ∷ Var
  → Int
  → Tm o n
  → Tm o n
addVar v n = \case
  Free v' → Free v'
  Bound m → if m == n then Free v else Bound m
  Abs e → Abs (addVar v (n + 1) e)
  App p es → App p $ hmap (addVar v n) es

instance Show1 o ⇒ Abt Var o (Tm o) where
  into = \case
    V v → Free v
    v :\ e → Abs (shiftVar v 0 e)
    v :$ es → App v es

  out = \case
    Free v → return $ V v
    Bound n → fail "bound variable occured in out"
    Abs e → do
      v ← fresh
      return $ v :\ addVar v 0 e
    App p es → return $ p :$ es
