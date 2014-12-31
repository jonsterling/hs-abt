{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnicodeSyntax #-}

module Abt.Tutorial where

import Abt.Class
import Abt.Types
import Abt.Concrete.LocallyNameless

import Control.Applicative
import Control.Monad.Trans.State.Strict

-- | We'll start off with a monad in which to manipulate ABTs; we'll need some
-- state for fresh variable generation.
--
newtype M α
  = M
  { _M ∷ State Int α
  } deriving (Functor, Applicative, Monad)

-- | We'll run an ABT computation by starting the variable counter at @0@.
--
runM ∷ M α → α
runM (M m) = evalState m 0

-- | Check out the source to see fresh variable generation.
--
instance MonadVar Var M where
  fresh = M $ do
    n ← get
    let n' = n + 1
    put n'
    return $ Var Nothing n'

  named a = do
    v ← fresh
    return $ v { _varName = Just a }

-- | Next, we'll define the operators for a tiny lambda calculus as a datatype
-- indexed by arities.
--
data Lang ns where
  Lam ∷ Lang '[S Z]
  Ap ∷ Lang '[Z,Z]

instance Show1 Lang where
  show1 = \case
    Lam → "lam"
    Ap → "ap"

instance HEq1 Lang where
  Lam === Lam = True
  Ap === Ap = True
  _ === _ = False

-- | Check out the source to see this example term: note that number of
-- arguments and presence of abstractions is guaranteed by the types.  The
-- representation is not scope-safe (i.e. free variables are permitted), but
-- that's how we want it.
--
example ∷ M [Var]
example = do
  x ← fresh
  y ← fresh
  let tm = Lam $$ x \\ (Ap $$ var x :* var y :* Nil) :* Nil
  freeVars tm
