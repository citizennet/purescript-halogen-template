module Component where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff)
import DOM (DOM)
import Data.Argonaut (Json, decodeJson, (.?))
import Data.Array (difference, filter, mapWithIndex, null, (:))
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax (AJAX, get)
import Network.RemoteData (RemoteData(..), fromEither, withDefault)
import Select as Select
import Select.Utils.Setters (setItemProps, setContainerProps, setInputProps)

data Query a
  = HandleSelect (Select.Message Query Item) a
  | Remove Item a

type Item = String
type Selection = String

type State =
  { items :: RemoteData String (Array Item)
  , selections :: Array Selection
  }

type Input = Unit

type Message = Void

type ChildSlot = Unit
type ChildQuery eff = Select.Query Query Item (Effects eff)

type Effects eff =
  ( avar :: AVAR
  , dom :: DOM
  , ajax :: AJAX
  | eff
  )

component :: ∀ eff m
  . MonadAff (Effects eff) m
 => H.Component HH.HTML Query Input Message m
component =
  H.parentComponent
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: Input -> State
  initialState = const
    { items: NotAsked
    , selections: [] }

  render :: State -> H.ParentHTML Query (ChildQuery eff) ChildSlot m
  render st =
    HH.div_
    [ HH.slot unit Select.component selectInput (HE.input HandleSelect) ]
    where

    selectInput =
      { inputType: Select.TextInput
      , items: []
      , initialSearch: Nothing
      , debounceTime: Just $ Milliseconds 500.0
      , render: typeahead st
      }

  eval
    :: Query
    ~> H.ParentDSL State Query (ChildQuery eff) ChildSlot Message m
  eval = case _ of
    Remove item next -> do
      H.modify \s -> s { selections = filter ((/=) item) s.selections }

      -- TODO: The new items should be available to select
      -- ...

      pure next

    HandleSelect message next -> case message of
      Select.Emit query -> do
        -- TODO: We should evaluate our own query, which has been raised
        -- ...
        pure next

      Select.Searched search -> do
        H.modify _ { items = Loading }

        -- TODO: There should be no items available to click
        -- ...

        newItems <- H.liftAff $ fetchItems search
        H.modify _ { items = fromEither newItems }

        -- TODO: The new items should be available to select
        -- ...

        pure next

      Select.Selected item -> do
        H.modify \st -> st { selections = item : st.selections }

        -- TODO: The new items should be available to select
        -- ...

        pure next

      Select.VisibilityChanged vis -> do
        pure next


----------
-- Render Function

typeahead :: ∀ eff
   . State
  -> Select.State Item (Effects eff)
  -> Select.ComponentHTML Query Item (Effects eff)
typeahead parentState selectState =
  HH.div_ [ renderSelections, renderInput, renderContainer ]

  where

  renderSelections = HH.ul_ $ parentState.selections <#>
    (\item ->
      HH.li
      -- TODO: Ensure clicking an item removes it.
      [ ]
      [ HH.text item ]
    )

  renderInput =
    -- TODO: Ensure typing causes searches or key events
    HH.input ( [] )

  renderContainer = case selectState.visibility of
    Select.Off -> HH.text ""
    Select.On ->
      HH.ul
      -- TODO: Ensure defaults are prevented
      ( [ ] )
      case null selectState.items of
        false ->
          renderItem `mapWithIndex` selectState.items
        _ ->
          [ HH.li
            -- TODO: Ensure clicking this button triggers a new empty
            -- search we can handle, thereby fetching new data
            [ ]
            [ HH.text "Fetch data again" ]
          ]

  renderItem ix item =
    HH.li
    ( -- TODO: Ensure items can be highlighted, register clicks, and more
      id $
      case Just ix == selectState.highlightedIndex of
        true -> [ red ]
        _ -> [ ]
    )
    [ HH.text item ]


----------
-- Helpers

-- CSS for highlighting
red :: ∀ i p. HH.IProp i p
red = HP.attr (HH.AttrName "style") "color: red;"

-- A function to search an external API, decode it, and return the
-- array of strings on success.
fetchItems :: ∀ eff
   . String
  -> Aff (Effects eff) (Either String (Array Item))
fetchItems str = do
   (res :: Json) <- _.response
     <$> get ("https://swapi.co/api/people/?search=" <> str)
   pure $ do
     obj <- decodeJson res
     arr <- obj .? "results"
     traverse (decodeJson <=< flip (.?) "name") arr

-- A convenience function for updating the `Select` component with
-- new items.
replaceItemsWith :: ∀ eff m
   . MonadAff (Effects eff) m
  => (Array Item -> Array Selection -> Array Item)
  -> H.ParentDSL State Query (ChildQuery eff) ChildSlot Message m Unit
replaceItemsWith f = do
  st <- H.get
  _ <- H.query unit
       $ Select.replaceItems
       $ f (withDefault [] st.items) st.selections
  pure unit
