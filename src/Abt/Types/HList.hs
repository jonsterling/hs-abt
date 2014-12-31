{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}

module Abt.Types.HList
( HList(..)
, hmap
, htraverse
, homogenizeA
) where

import Control.Applicative
import Abt.Class.HEq1

data HList ∷ (κ → *) → [κ] → * where
  Nil ∷ HList el '[]
  (:*) ∷ el x → HList el xs → HList el (x ': xs)
infixr 8 :*

hmap ∷ (∀ x. f x → g x) → HList f xs → HList g xs
hmap η = \case
  Nil → Nil
  x :* xs → η x :* hmap η xs

htraverse ∷ Applicative h ⇒ (∀ x. f x → h (g x)) → HList f xs → h (HList g xs)
htraverse η = \case
  Nil → pure Nil
  x :* xs → (:*) <$> η x <*> htraverse η xs

homogenizeA ∷ Applicative h ⇒ (∀ x. el x → h α) → HList el xs → h [α]
homogenizeA η = \case
  Nil → pure []
  x :* xs → (:) <$> η x <*> homogenizeA η xs

instance HEq1 el ⇒ HEq1 (HList el) where
  Nil === Nil = True
  (x :* xs) === (y :* ys) = x === y && xs === ys
  _ === _ = False

