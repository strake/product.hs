module Control.Category.Product ((:×:) (..)) where

import Control.Category
import Control.Category.Groupoid
import Data.Type.Equality

data ((s :: αₛ -> αₛ -> *) :×: (t :: αₜ -> αₜ -> *)) :: (αₛ, αₜ) -> (αₛ, αₜ) -> * where
    (:×:) :: s aₛ bₛ -> t aₜ bₜ -> (s :×: t) '(aₛ, aₜ) '(bₛ, bₜ)

instance (Category s, Category t) => Category (s :×: t) where
    id :: ∀ a . (s :×: t) a a
    id = pair `gcastWith` (id :×: id) where pair = pair :: a :~: '(Fst a, Snd a)
    (fₛ :×: fₜ) . (gₛ :×: gₜ) = (fₛ . gₛ) :×: (fₜ . gₜ)

type family Fst a where Fst '(a, b) = a
type family Snd a where Snd '(a, b) = b

instance (Groupoid s, Groupoid t) => Groupoid (s :×: t) where
    invert (f :×: g) = invert f :×: invert g
