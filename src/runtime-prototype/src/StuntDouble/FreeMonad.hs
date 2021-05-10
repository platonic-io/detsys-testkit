{-# LANGUAGE DeriveFunctor #-}

module StuntDouble.FreeMonad where

data Free f a = Pure a | Free (f (Free f a))
  deriving Functor

instance Functor f => Applicative (Free f) where
  pure = Pure

  mf <*> Pure x = fmap (\g -> g x) mf
  mf <*> Free m = Free (fmap (mf <*>) m)

instance Functor f => Monad (Free f) where
  return = pure

  Pure x >>= k = k x
  Free m >>= k = Free (fmap (>>= k) m)

------------------------------------------------------------------------

iter :: Functor f => (f b -> b) -> (a -> b) -> Free f a -> b
iter _phi psi (Pure x) = psi x
iter  phi psi (Free m) = phi (fmap (iter phi psi) m)

iterM :: (Functor f, Monad m) => (f (m b) -> m b) -> (a -> m b) -> Free f a -> m b
iterM _phi psi (Pure x) = psi x
iterM  phi psi (Free m) = phi (fmap (iterM phi psi) m)

recM :: (Functor f, Monad m) => (f (m b, Free f a) -> m b) -> (a -> m b) -> Free f a -> m b
recM _phi psi (Pure x) = psi x
recM  phi psi (Free m) = phi (fmap (\m' -> (recM phi psi m', m')) m)
