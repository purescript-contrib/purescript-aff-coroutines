module Test.Main where

import Prelude

import Control.Coroutine (Consumer, Producer, runProcess, consumer, ($$))
import Control.Coroutine.Aff (emit, close, produceAff)
import Effect.Aff (Aff, Milliseconds(..), delay, runAff)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Data.Either (either)
import Data.Maybe (Maybe(..))

p :: Producer String Aff String
p = produceAff \emitter -> do
  delay (Milliseconds 1000.0)
  emit emitter "Working..."
  delay (Milliseconds 1000.0)
  emit emitter "Working..."
  delay (Milliseconds 1000.0)
  emit emitter "Working..."
  delay (Milliseconds 1000.0)
  close emitter "Done!"

c :: Consumer String Aff String
c = consumer \s -> liftEffect (log s) $> Nothing

main :: Effect Unit
main = void $ runAff (either logShow log) $ runProcess (p $$ c)
