## Module Control.Coroutine.Aff

This module defines functions for creating coroutines on top of the `Aff` monad.

The `Aff` monad only supports actions which return a single value, asynchronously, so this
module provides a principled way to deal with asynchronous _streams_ of values, and asynchronous consumers
of streamed data.

#### `produce`

``` purescript
produce :: forall a r eff. ((Either a r -> Eff (avar :: AVAR | eff) Unit) -> Eff (avar :: AVAR | eff) Unit) -> Producer a (Aff (avar :: AVAR | eff)) r
```

Create a `Producer` using an asynchronous callback.

The callback should provide zero or more values of type `a`, which will be
emitted by the `Producer`, terminated by an optional value of type `r`. No values
should be provided after a value of type `r` has been provided.

For example:

```purescript
produce \emit -> do
  log "Working..."
  emit (Left "progress")
  log "Done!"
  emit (Right "finished")
```

#### `produce'`

``` purescript
produce' :: forall a r m eff. MonadAff (avar :: AVAR | eff) m => ((Either a r -> Eff (avar :: AVAR | eff) Unit) -> Eff (avar :: AVAR | eff) Unit) -> Producer a m r
```

A version of `produce` that creates a `Producer` with an underlying
`MonadAff`, rather than `Aff` specifically.


