-- | This module provides the simplest implementation of a free monad.

module Control.Monad.Free (
    -- * Free monad
    -- $free    
    Free(..),
    liftF
    ) where

import Control.Applicative
import Control.Monad

{- $free
    Every functor @f@ gives rise to a corresponding monad: @Free f@.
-}

-- | A free monad over a functor resembles a \"list\" of that functor:
data Free f r
  -- | 'Return' behaves like @[]@ by not using the functor at all
  = Return r
  -- | 'Wrap' behaves like (@:@) by prepending another layer of the functor
  | Wrap (f (Free f r))

instance (Functor f) => Functor (Free f) where
    fmap = liftM

instance (Functor f) => Applicative (Free f) where
    pure  = return
    (<*>) = ap

instance (Functor f) => Monad (Free f) where
    return  = Return
    m >>= f = case m of
        Return r -> f r
        Wrap   w -> Wrap $ fmap (>>= f) w

-- | Creates a \"singleton\" list from just one layer of the functor
liftF :: (Functor f) => f r -> Free f r
liftF x = Wrap $ fmap Return x
