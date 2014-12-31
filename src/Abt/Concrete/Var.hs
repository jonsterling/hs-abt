{-# LANGUAGE UnicodeSyntax #-}

module Abt.Concrete.Var
( Var(..)
, varName
, varIndex
) where

import Control.Applicative

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
-- varName ∷ Lens' 'Var' ('Maybe' 'String')
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
-- varIndex ∷ Lens' 'Var' 'Int'
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
