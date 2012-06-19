-- | This module is the indexed version of "Control.Monad.Free"

{-# LANGUAGE TypeOperators #-}

module Control.IMonad.Free (
    -- * Free monad
    -- $free
    IFree(..),
    liftF
    ) where

import Control.Category.Index
import Control.IMonad

{- $free
    Indexed free monads lift the constructor signatures to the category of
    indexed Haskell functions: (':->')

> Return ::            r  :-> IFree f r
> Wrap   :: f (IFree f r) :-> IFree f r
-}

-- | Indexed equivalent to @Free@ from "Control.Monad.Free"
data IFree f r i = Return (r i) | Wrap (f (IFree f r) i)

instance (IFunctor f) => IFunctor (IFree f) where
    fmapI f x = x ?>= returnI . f

instance (IFunctor f) => IMonad (IFree f) where
    returnI = Return
    bindI f x = case x of
        Return r -> f r
        Wrap   w -> Wrap $ fmapI (bindI f) w

-- | Indexed equivalent to @liftF@ from "Control.Monad.Free"
liftF :: (IFunctor f) => f r :-> IFree f r
liftF x = Wrap $ fmapI Return x
