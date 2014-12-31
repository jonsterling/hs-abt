{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
import Control.Monad.Trans.Maybe

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

-- | A monad transformer for small step operational semantics.
--
newtype StepT m α
  = StepT
  { runStepT ∷ MaybeT m α
  } deriving (Monad, Functor, Applicative, Alternative)

-- | To indicate that a term is in normal form.
--
stepsExhausted ∷ Applicative m ⇒ StepT m α
stepsExhausted = StepT . MaybeT $ pure Nothing

instance MonadVar Var m ⇒ MonadVar Var (StepT m) where
  fresh = StepT . MaybeT $ Just <$> fresh
  named str = StepT . MaybeT $ Just <$> named str

-- | A single evaluation step.
--
step ∷ Tm Lang Z → StepT M (Tm Lang Z)
step tm =
  out tm >>= \case
    Ap :$ m :* n :* Nil →
      out m >>= \case
        Lam :$ xe :* Nil → xe // n
        _ → app <$> step m <*> pure n <|> app <$> pure m <*> step n
          where
            app a b = Ap $$ a :* b :* Nil
    e → stepsExhausted

-- | The reflexive-transitive closure of a small-step operational semantics.
--
star
  ∷ Monad m
  ⇒ (α → StepT m α)
  → (α → m α)
star f a =
  runMaybeT (runStepT $ f a) >>=
    return a `maybe` star f

-- | Evaluate a term to normal form
--
eval ∷ Tm Lang Z → Tm Lang Z
eval = runM . star step

-- | @λx.x@
--
identityTm ∷ M (Tm Lang Z)
identityTm = do
  x ← fresh
  return $ Lam $$ x \\ var x :* Nil

-- | @(λx.x)(λx.x)@
--
appTm ∷ M (Tm Lang Z)
appTm = do
  tm ← identityTm
  return $ Ap $$ tm :* tm :* Nil

-- | A demonstration of evaluating (and pretty-printing). Output:
--
-- @
-- ap[lam[\@2.\@2];lam[\@3.\@3]] ~>* lam[\@2.\@2]
-- @
--
main ∷ IO ()
main = do
  let mm = runM $ appTm >>= toString
      mm' = runM $ appTm >>= toString . eval
  print $ mm ++ " ~>* " ++ mm'

