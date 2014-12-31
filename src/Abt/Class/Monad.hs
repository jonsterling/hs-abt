{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnicodeSyntax #-}

module Abt.Class.Monad where

import Control.Applicative

class (Ord v, Eq v, Show v, Monad m, Applicative m) ⇒ MonadVar v m | m → v where
  -- | Generates a fresh variable
  fresh ∷ m v

  -- | Generates a fresh variable tagged with a name
  named ∷ String → m v
