module Test.Main where

import Prelude

import Control.Coroutine (Consumer, Producer, runProcess, consumer, ($$))
import Control.Coroutine.Aff (emit, close, produceAff)
import Control.Monad.Aff (Aff, Milliseconds(..), delay, runAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Either (either)
import Data.Maybe (Maybe(..))

p :: forall eff. Producer String (Aff (avar :: AVAR | eff)) String
p = produceAff \emitter -> do
  delay (Milliseconds 1000.0)
  emit emitter "Working..."
  delay (Milliseconds 1000.0)
  emit emitter "Working..."
  delay (Milliseconds 1000.0)
  emit emitter "Working..."
  delay (Milliseconds 1000.0)
  close emitter "Done!"

c :: forall eff. Consumer String (Aff (console :: CONSOLE | eff)) String
c = consumer \s -> liftEff (log s) $> Nothing

main :: forall eff. Eff (console :: CONSOLE, avar :: AVAR, err :: EXCEPTION | eff) Unit
main = void $ runAff (either logShow log) $ runProcess (p $$ c)
