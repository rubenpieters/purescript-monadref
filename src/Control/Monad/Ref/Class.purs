-- | This module defines the `MonadRef` type class and its instances.

module Control.Monad.Ref.Class where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.ST as ST
import Control.Monad.Eff.Ref as R

import Type.Row.Effect.Equality (class EffectRowEquals, effTo)

class MonadRef r m | m -> r where
    newRef :: forall a. a -> m (r a)
    readRef :: forall a. r a -> m a
    writeRef :: forall a. r a -> a -> m Unit
    modifyRef :: forall a. r a -> (a -> a) -> m Unit

instance refMonadRef :: (EffectRowEquals (ref :: R.REF | e) r) => MonadRef R.Ref (Eff r) where
    newRef a = R.newRef a # effTo
    readRef ra = R.readRef ra # effTo
    writeRef ra a = R.writeRef ra a # effTo
    modifyRef ra f = R.modifyRef ra f # effTo

instance stMonadRef :: (EffectRowEquals (st :: (ST.ST s) | e) r) => MonadRef (ST.STRef s) (Eff r) where
    newRef a = ST.newSTRef a # effTo
    readRef ra = ST.readSTRef ra # effTo
    writeRef ra a = ST.writeSTRef ra a # void # effTo
    modifyRef ra f = ST.modifySTRef ra f # void # effTo
