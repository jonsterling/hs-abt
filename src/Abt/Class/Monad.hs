{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Abt.Class.Monad where

class (Ord v, Eq v, Show v, Monad m, Applicative m) => MonadVar v m | m -> v where
  -- | Generates a fresh variable
  fresh :: m v

  -- | Generates a fresh variable tagged with a name
  named :: String -> m v

  -- | Clones a variable with a name
  clone :: v -> m v