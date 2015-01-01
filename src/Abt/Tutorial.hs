{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module Abt.Tutorial where

import Abt.Class
import Abt.Types
import Abt.Concrete.LocallyNameless

import Control.Applicative
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Maybe
import Data.Vinyl

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
  LAM ∷ Lang '[S Z]
  APP ∷ Lang '[Z,Z]

instance Show1 Lang where
  show1 = \case
    LAM → "lam"
    APP → "ap"

instance HEq1 Lang where
  LAM === LAM = True
  APP === APP = True
  _ === _ = False

lam ∷ Tm Lang (S Z) → Tm0 Lang
lam e = LAM $$ e :& RNil

app ∷ Tm0 Lang → Tm0 Lang → Tm0 Lang
app m n = APP $$ m :& n :& RNil

-- | A monad transformer for small step operational semantics.
--
newtype StepT m α
  = StepT
  { runStepT ∷ MaybeT m α
  } deriving (Monad, Functor, Applicative, Alternative)

-- | To indicate that a term is in normal form.
--
stepsExhausted
  ∷ Applicative m
  ⇒ StepT m α
stepsExhausted = StepT . MaybeT $ pure Nothing

instance MonadVar Var m ⇒ MonadVar Var (StepT m) where
  fresh = StepT . MaybeT $ Just <$> fresh
  named str = StepT . MaybeT $ Just <$> named str

-- | A single evaluation step.
--
step
  ∷ Tm0 Lang
  → StepT M (Tm0 Lang)
step tm =
  out tm >>= \case
    APP :$ m :& n :& RNil →
      out m >>= \case
        LAM :$ xe :& RNil → xe // n
        _ → app <$> step m <*> pure n <|> app <$> pure m <*> step n
    _ → stepsExhausted

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
eval ∷ Tm0 Lang → Tm0 Lang
eval = runM . star step

-- | @λx.x@
--
identityTm ∷ M (Tm0 Lang)
identityTm = do
  x ← fresh
  return $ lam (x \\ var x)

-- | @(λx.x)(λx.x)@
--
appTm ∷ M (Tm0 Lang)
appTm = do
  tm ← identityTm
  return $ APP $$ tm :& tm :& RNil

-- | A demonstration of evaluating (and pretty-printing). Output:
--
-- @
-- ap[lam[\@2.\@2];lam[\@3.\@3]] ~>* lam[\@2.\@2]
-- @
--
main ∷ IO ()
main =
  print . runM $ do
    mm ← appTm
    mmStr ← toString mm
    mmStr' ← toString $ eval mm
    return $ mmStr ++ " ~>* " ++ mmStr'

