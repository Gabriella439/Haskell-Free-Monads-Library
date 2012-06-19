-- | This module is the indexed version of "Control.Monad.Free"

{-# LANGUAGE TypeOperators #-}

module Control.IMonad.Free (
    -- * Free monad
    -- $free
    Free(..),
    liftF
    ) where

import Control.Category.Index
import Control.IMonad

{- $free
    Indexed free monads simply lift the constructor signatures to the category
    of indexed Haskell functions (':->'):

> Return ::           r  :-> Free f r
> Wrap   :: f (Free f r) :-> Free f r
-}

-- | Indexed equivalent to @Free@ from "Control.Monad.Free"
data Free f r i = Return (r i) | Wrap (f (Free f r) i)

instance (IFunctor f) => IFunctor (Free f) where
    fmapI f x = x ?>= returnI . f

instance (IFunctor f) => IMonad (Free f) where
    returnI = Return
    bindI f x = case x of
        Return r -> f r
        Wrap   w -> Wrap $ fmapI (bindI f) w

-- | Indexed equivalent to @liftF@ from "Control.Monad.Free"
liftF :: (IFunctor f) => f r :-> Free f r
liftF x = Wrap $ fmapI Return x
