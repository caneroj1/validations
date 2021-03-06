module Validations.Validator
  ( attach
  , attachM
  , Validator(..)
  , composeValidator
  ) 
    where

import Validations.Types.Checker(Checker, MonadicChecker)
import Control.Category(Category(..))

attach :: (Monad m) => Checker ev a b -> ek -> Validator ek ev m a b
attach validator fieldName = Validator $ \x -> case (validator x) of
  Right x' -> return $ Right x'
  Left  e   -> return $ Left (fieldName, e)

attachM :: (Monad m) => MonadicChecker ev m a b -> ek -> Validator ek ev m a b
attachM validator fieldName = Validator $ \x -> validator x >>= \y -> case y of
  Right y' -> return $ Right y'
  Left  e   -> return $ Left (fieldName, e)

newtype Validator errorKey errorValue monad a b  = Validator { runValidator :: a -> monad (Either (errorKey, errorValue) b) }

instance (Monad m) => Category (Validator ek ev m) where
  id = Validator (\x -> return (Right x))
  y . x = composeValidator x y


composeValidator :: (Monad m) => Validator ek ev m a b -> Validator ek ev m b c -> Validator ek ev m a c
composeValidator (Validator f) (Validator g) = Validator $ (\a -> f a >>= \rb -> case rb of
  Right b     -> g b
  Left   (fn,e) -> return (Left (fn, e)))
