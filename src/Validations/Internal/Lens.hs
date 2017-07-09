{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types                #-}

module Validations.Internal.Lens
  ( getter
  , setter
  , lens
  , Lens
  ) where

import           Control.Applicative    (Const (Const), getConst, (<$>))
import           Control.Monad.Identity (Identity (Identity), runIdentity)

type Lens a s = forall f. Functor f => (a -> f a) -> s -> f s

getter :: Lens a s -> s -> a
getter lns x = getConst $ lns (\y -> Const y) x

setter :: Lens a s -> s -> a -> s
setter lns x y = runIdentity $ lns (\_ -> Identity y) x

lens :: (Functor f) => (s -> a) -> (s -> a -> s) -> (a -> f a) -> s -> f s
lens get set lift input = set input <$> (lift . get) input
