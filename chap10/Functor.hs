-- Functor

class Functor f where
  fmap :: (a ->b) ->f a ->f b

{-CONDITIONS-}
-- 1 type parameter only :: Type a
--

{-PROPERTIES-}
-- identity
fmap id = id
-- associativity
fmap (f . g) = fmap f . fmap g

{-Either-}
class Functor (Either Int) where
  fmap _ (Left n)  = Left n
  fmap f (Right r) = Right (f r)
