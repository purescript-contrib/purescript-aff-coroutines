module Test.Main where

import Prelude

import Control.Coroutine (Consumer, Producer, runProcess, consumer, ($$))
import Control.Coroutine.Aff (produceAff)
import Control.Monad.Aff (Aff, runAff, later')
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Either (Either(..))
import Data.Functor (($>))
import Data.Maybe (Maybe(..))

p :: forall eff. Producer String (Aff (avar :: AVAR | eff)) String
p = produceAff \emit -> do
  later' 1000 $ emit $ Left "Working..."
  later' 1000 $ emit $ Left "Working..."
  later' 1000 $ emit $ Left "Working..."
  later' 1000 $ emit $ Right "Done!"

c :: forall eff. Consumer String (Aff (console :: CONSOLE | eff)) String
c = consumer \s -> liftEff (log s) $> Nothing

main :: forall eff. Eff ( console :: CONSOLE
                        , avar :: AVAR
                        , err :: EXCEPTION
                        | eff
                        ) Unit
main = runAff logShow log $ runProcess (p $$ c)
