-- | This module defines functions for creating coroutines on top of the `Aff` monad.
-- |
-- | The `Aff` monad only supports actions which return a single value, asynchronously, so this
-- | module provides a principled way to deal with asynchronous _streams_ of values, and asynchronous consumers
-- | of streamed data.

module Control.Coroutine.Aff where

import Prelude

import Data.Either (Either())

import Control.Coroutine (Producer(), producer)
import Control.Monad.Aff (Aff(), runAff)
import Control.Monad.Aff.AVar (AVAR(), makeVar, takeVar, putVar)
import Control.Monad.Aff.Class (MonadAff, liftAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Free.Trans (hoistFreeT)
import Control.Monad.Trans (lift)

-- | Create a `Producer` using an asynchronous callback.
-- |
-- | The callback should provide zero or more values of type `a`, which will be
-- | emitted by the `Producer`, terminated by an optional value of type `r`. No values
-- | should be provided after a value of type `r` has been provided.
-- |
-- | For example:
-- |
-- | ```purescript
-- | produce \emit -> do
-- |   log "Working..."
-- |   emit (Left "progress")
-- |   log "Done!"
-- |   emit (Right "finished")
-- | ```
produce
  :: forall a r eff
   . ((Either a r -> Eff (avar :: AVAR | eff) Unit) -> Eff (avar :: AVAR | eff) Unit)
  -> Producer a (Aff (avar :: AVAR | eff)) r
produce recv = hoistFreeT liftAff do
  v <- lift makeVar
  lift $ liftEff $ recv $ runAff (const (return unit)) return <<< putVar v
  producer (takeVar v)

-- | A version of `produce` that creates a `Producer` with an underlying
-- | `MonadAff`, rather than `Aff` specifically.
produce'
  :: forall a r m eff
   . (MonadAff (avar :: AVAR | eff) m)
  => ((Either a r -> Eff (avar :: AVAR | eff) Unit) -> Eff (avar :: AVAR | eff) Unit)
  -> Producer a m r
produce' = hoistFreeT liftAff <<< produce
