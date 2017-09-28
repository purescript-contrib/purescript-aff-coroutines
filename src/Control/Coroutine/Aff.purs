-- | This module defines functions for creating coroutines on top of the `Aff` monad.
-- |
-- | The `Aff` monad only supports actions which return a single value, asynchronously, so this
-- | module provides a principled way to deal with asynchronous _streams_ of values, and asynchronous consumers
-- | of streamed data.

module Control.Coroutine.Aff where

import Prelude

import Control.Coroutine (Producer, producer)
import Control.Monad.Aff (Aff, runAff, forkAff)
import Control.Monad.Aff.AVar (AVAR, makeEmptyVar, takeVar, putVar)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Free.Trans (hoistFreeT)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))

data Emission a b
  = Step a
  | Result b

toEither :: forall a b. Emission a b -> Either a b
toEither = case _ of
  Step a -> Left a
  Result b -> Right b

newtype Emitter m a r eff = Emitter (Emission a r -> m (avar :: AVAR | eff) Unit)

emit
  :: forall m a r eff
   . Emitter m a r eff
  -> Emission a r
  -> m (avar :: AVAR | eff) Unit
emit (Emitter f) = f

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
-- |   emit emitter (Left "progress")
-- |   log "Done!"
-- |   emit emitter (Right "finished")
-- | ```
produce
  :: forall a r eff
   . (Emitter Eff a r eff -> Eff (avar :: AVAR | eff) Unit)
  -> Producer a (Aff (avar :: AVAR | eff)) r
produce recv = produceAff \(Emitter send) ->
  liftEff (recv (Emitter (void <<< runAff (const (pure unit)) <<< send)))

-- | A version of `produce` that creates a `Producer` with an underlying
-- | `MonadAff`, rather than `Aff` specifically.
produce'
  :: forall a r m eff
   . MonadAff (avar :: AVAR | eff) m
  => (Emitter Eff a r eff -> Eff (avar :: AVAR | eff) Unit)
  -> Producer a m r
produce' = hoistFreeT liftAff <<< produce

-- | A variant of `produce` where the setup and callback functions use the `Aff`
-- | monad. This can be helpful in certain cases.
-- |
-- | For example:
-- |
-- | ```purescript
-- | produceAff \emit -> do
-- |   delay $ Milliseconds 1000
-- |   emit  $ Left "progress"
-- |   delay $ Milliseconds 1000
-- |   emit  $ Right "finished"
-- | ```
produceAff
  :: forall a r eff
   . (Emitter Aff a r eff -> Aff (avar :: AVAR | eff) Unit)
  -> Producer a (Aff (avar :: AVAR | eff)) r
produceAff recv = do
  v <- lift makeEmptyVar
  _ <- lift (forkAff (recv (Emitter (flip putVar v))))
  producer (toEither <$> takeVar v)
