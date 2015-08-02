module Test.Main where

import Prelude

import Data.Maybe
import Data.Either
import Data.Functor (($>))

import Control.Coroutine
import Control.Coroutine.Aff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console
import Control.Monad.Aff
  
p :: Producer String (Aff _) String
p = produce \emit -> launchAff do
  later' 1000 $ liftEff $ emit $ Left "Working..."
  later' 1000 $ liftEff $ emit $ Left "Working..."
  later' 1000 $ liftEff $ emit $ Left "Working..."
  later' 1000 $ liftEff $ emit $ Right "Done!"
  
c :: Consumer String (Aff (console :: CONSOLE | _)) String
c = consumer \s -> liftEff (log s) $> Nothing

main = runAff print log $ runProcess (p $$ c)