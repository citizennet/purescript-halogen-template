module Main where

import Prelude

import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Network.HTTP.Affjax (AJAX)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Component (component)

type Effects =
  ( ajax :: AJAX
  , console :: CONSOLE
  )

main :: Eff (HA.HalogenEffects Effects) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body
