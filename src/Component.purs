module Component where

import Prelude

import Control.Monad.Aff.Class (class MonadAff)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH

data Query a
  = NoOp a

type State = Unit
type Input = Unit

type Message = Void

component :: ∀ eff m
  . MonadAff eff m
 => H.Component HH.HTML Query Input Message m
component =
  H.component
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: Input -> State
  initialState = const unit

  render :: State -> H.ComponentHTML Query
  render st = HH.div_ []

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    NoOp next -> pure next
