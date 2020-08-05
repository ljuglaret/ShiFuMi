module Main where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Jeu2 as Jeu

main :: Effect Unit
main = HA.runHalogenAff $
       HA.awaitBody >>= runUI Jeu.page unit
