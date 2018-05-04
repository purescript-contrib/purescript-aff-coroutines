-- | This module defines functions for creating coroutines on top of the `Aff`
-- | monad.
-- |
-- | The `Aff` monad only supports actions which return a single value,
-- | asynchronously, so this module provides a principled way to deal with
-- | asynchronous _streams_ of values, and asynchronous consumers of streamed
-- | data.
module Control.Coroutine.Aff where

import Prelude

import Control.Coroutine (Producer, producer)
import Effect.Aff (Aff, runAff, forkAff)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect (Effect)
import Effect.Class (liftEffect)
import Control.Monad.Free.Trans (hoistFreeT)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Newtype (class Newtype)

newtype Emitter m a r = Emitter (Step a r -> m Unit)

derive instance newtypeEmitter :: Newtype (Emitter m a r) _

data Step a b
  = Emit a
  | Finish b

emit :: forall m a r. Emitter m a r -> a -> m Unit
emit (Emitter f) = f <<< Emit

close :: forall m a r. Emitter m a r -> r -> m Unit
close (Emitter f) = f <<< Finish

-- | Create a `Producer` using an asynchronous callback.
-- |
-- | The callback should provide zero or more values of type `a`, which will be
-- | emitted by the `Producer`, terminated by an optional value of type `r`. No values
-- | should be provided after a value of type `r` has been provided.
-- |
-- | For example:
-- |
-- | ```purescript
-- | produce \emitter -> do
-- |   log "Working..."
-- |   emit emitter "progress"
-- |   log "Done!"
-- |   close emitter "finished"
-- | ```
produce :: forall a r. (Emitter Effect a r -> Effect Unit) -> Producer a Aff r
produce recv = produceAff \(Emitter send) ->
  liftEffect (recv (Emitter (void <<< runAff (const (pure unit)) <<< send)))

-- | A version of `produce` that creates a `Producer` with an underlying
-- | `MonadAff`, rather than `Aff` specifically.
produce' :: forall a r m. MonadAff m => (Emitter Effect a r -> Effect Unit) -> Producer a m r
produce' = hoistFreeT liftAff <<< produce

-- | A variant of `produce` where the setup and callback functions use the `Aff`
-- | monad. This can be helpful in certain cases.
-- |
-- | For example:
-- |
-- | ```purescript
-- | produceAff \emitter -> do
-- |   delay $ Milliseconds 1000
-- |   emit emitter "progress"
-- |   delay $ Milliseconds 1000
-- |   close emitter "finished"
-- | ```
produceAff :: forall a r. (Emitter Aff a r -> Aff Unit) -> Producer a Aff r
produceAff recv = do
  v <- lift AVar.empty
  _ <- lift (forkAff (recv (Emitter (flip AVar.put v))))
  producer $ AVar.take v <#> case _ of
    Emit a -> Left a
    Finish b -> Right b
