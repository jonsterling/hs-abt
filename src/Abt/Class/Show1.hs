{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UnicodeSyntax #-}

module Abt.Class.Show1 where

-- | Uniform variant of 'Show' for indexed types. This is different from
-- 'Data.Functor.Show1' in that it is properly kind polymorphic.
--
class Show1 f where
  showsPrec1 ∷ Int → f i → ShowS
  showsPrec1 i x = (show1 x ++)

  show1 ∷ f i → String
  show1 x = showsPrec1 0 x ""

