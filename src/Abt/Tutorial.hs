{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Abt.Tutorial where

import Abt.Class
import Abt.Types
import Abt.Concrete.LocallyNameless

import Control.Applicative
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Data.Vinyl
import Prelude hiding (pi)

-- | We'll start off with a monad in which to manipulate ABTs; we'll need some
-- state for fresh variable generation.
--
newtype M a
  = M
  { _M :: State Int a
  } deriving (Functor, Applicative, Monad)

-- | We'll run an ABT computation by starting the variable counter at @0@.
--
runM :: M a -> a
runM (M m) = evalState m 0

-- | Check out the source to see fresh variable generation.
--
instance MonadVar Var M where
  fresh = M $ do
    n <- get
    let n' = n + 1
    put n'
    return $ Var Nothing n'

  named a = do
    v <- fresh
    return $ v { _varName = Just a }

  clone v =
    case _varName v of
      Just s -> named s
      _ -> fresh

-- | Next, we'll define the operators for a tiny lambda calculus as a datatype
-- indexed by arities.
--
data Lang ns where
  LAM :: Lang '[ 'S 'Z]
  APP :: Lang '[ 'Z, 'Z]
  PI :: Lang '[ 'Z, 'S 'Z]
  UNIT :: Lang '[]
  AX :: Lang '[]

instance Show1 Lang where
  show1 = \case
    LAM -> "lam"
    APP -> "ap"
    PI -> "pi"
    UNIT -> "unit"
    AX -> "<>"

instance HEq1 Lang where
  heq1 LAM LAM = Just Refl
  heq1 APP APP = Just Refl
  heq1 PI PI = Just Refl
  heq1 UNIT UNIT = Just Refl
  heq1 AX AX = Just Refl
  heq1 _ _ = Nothing

lam :: Tm Lang ('S 'Z) -> Tm0 Lang
lam e = LAM $$ e :& RNil

app :: Tm0 Lang -> Tm0 Lang -> Tm0 Lang
app m n = APP $$ m :& n :& RNil

ax :: Tm0 Lang
ax = AX $$ RNil

unit :: Tm0 Lang
unit = UNIT $$ RNil

pi :: Tm0 Lang -> Tm Lang ('S 'Z) -> Tm0 Lang
pi a xb = PI $$ a :& xb :& RNil

-- | A monad transformer for small step operational semantics.
--
newtype StepT m a
  = StepT
  { runStepT :: MaybeT m a
  } deriving (Monad, Functor, Applicative, Alternative)

-- | To indicate that a term is in normal form.
--
stepsExhausted
  :: Applicative m
  => StepT m a
stepsExhausted = StepT . MaybeT $ pure Nothing

instance MonadVar Var m => MonadVar Var (StepT m) where
  fresh = StepT . MaybeT $ Just <$> fresh
  named str = StepT . MaybeT $ Just <$> named str
  clone v = StepT . MaybeT $ Just <$> clone v

-- | A single evaluation step.
--
step
  :: Tm0 Lang
  -> StepT M (Tm0 Lang)
step tm =
  out tm >>= \case
    APP :$ m :& n :& RNil ->
      out m >>= \case
        LAM :$ xe :& RNil -> xe // n
        _ -> app <$> step m <*> pure n <|> app <$> pure m <*> step n
    PI :$ a :& xb :& RNil -> pi <$> step a <*> pure xb
    _ -> stepsExhausted

-- | The reflexive-transitive closure of a small-step operational semantics.
--
star
  :: Monad m
  => (a -> StepT m a)
  -> (a -> m a)
star f a =
  runMaybeT (runStepT $ f a) >>=
    return a `maybe` star f

-- | Evaluate a term to normal form
--
eval :: Tm0 Lang -> Tm0 Lang
eval = runM . star step

newtype JudgeT m a
  = JudgeT
  { runJudgeT :: ExceptT String m a
  } deriving (Monad, Functor, Applicative, Alternative)

instance MonadVar Var m => MonadVar Var (JudgeT m) where
  fresh = JudgeT . ExceptT $ Right <$> fresh
  named str = JudgeT . ExceptT $ Right <$> named str
  clone v = JudgeT . ExceptT $ Right <$> clone v

type Ctx = [(Var, Tm0 Lang)]

raise :: Monad m => String -> JudgeT m a
raise = JudgeT . ExceptT . return . Left

checkTy
  :: Ctx
  -> Tm0 Lang
  -> Tm0 Lang
  -> JudgeT M ()
checkTy g tm ty = do
  let ntm = eval tm
      nty = eval ty
  (,) <$> out ntm <*> out nty >>= \case
    (LAM :$ xe :& RNil, PI :$ a :& yb :& RNil) -> do
      z <- fresh
      ez <- xe // var z
      bz <- yb // var z
      checkTy ((z,a):g) ez bz
    (AX :$ RNil, UNIT :$ RNil) -> return ()
    _ -> do
      ty' <- inferTy g tm
      if ty' === nty
        then return ()
        else raise "Type error"

inferTy
  :: Ctx
  -> Tm0 Lang
  -> JudgeT M (Tm0 Lang)
inferTy g tm = do
  out (eval tm) >>= \case
    V v | Just (eval -> ty) <- lookup v g -> return ty
        | otherwise -> raise "Ill-scoped variable"
    APP :$ m :& n :& RNil -> do
      inferTy g m >>= out >>= \case
        PI :$ a :& xb :& RNil -> do
          checkTy g n a
          eval <$> xb // n
        _ -> raise "Expected pi type for lambda abstraction"
    _ -> raise "Only infer neutral terms"

-- | @λx.x@
--
identityTm :: M (Tm0 Lang)
identityTm = do
  x <- named "x"
  return $ lam (x \\ var x)

-- | @(λx.x)(λx.x)@
--
appTm :: M (Tm0 Lang)
appTm = do
  tm <- identityTm
  return $ app tm tm

-- | A demonstration of evaluating (and pretty-printing). Output:
--
-- @
-- ap[lam[\@2.\@2];lam[\@3.\@3]] ~>* lam[\@4.\@4]
-- @
--
main :: IO ()
main = do
  -- Try out the type checker
  either fail print . runM . runExceptT . runJudgeT $ do
    x <- fresh
    checkTy [] (lam (x \\ var x)) (pi unit (x \\ unit))

  print . runM $ do
    im <- identityTm
    imStr <- toString im
    return imStr

  print . runM $ do
    mm <- appTm
    mmStr <- toString mm
    mmStr' <- toString $ eval mm
    return $ mmStr ++ " ~>* " ++ mmStr'

