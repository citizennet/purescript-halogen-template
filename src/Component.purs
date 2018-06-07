module Component where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH

data Query a
  = NoOp a

type State = Unit
type Input = Unit

type Message = Void

type ChildSlot = Unit
type ChildQuery = Const Void

component :: ∀ m. MonadAff m => H.Component HH.HTML Query Input Message m
component =
  H.parentComponent
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: Input -> State
  initialState = const unit

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
  render st = HH.div_ []

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message m
  eval = case _ of
    NoOp next -> pure next
