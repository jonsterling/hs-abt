{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

module Abt.Types.Nat where

import Data.Typeable

data Nat
  = Z
  | S !Nat
  deriving Typeable

deriving instance Typeable Z
deriving instance Typeable S

