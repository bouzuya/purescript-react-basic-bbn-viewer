module Component.App
  ( app
  ) where

import Prelude

import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import React.Basic (Component, JSX, Self, StateUpdate(..), capture_, createComponent, make, sendAsync)
import React.Basic.DOM as H

type Props =
  {}

type State =
  { article :: String
  }

data Action
  = FetchArticle
  | UpdateArticle String

component :: Component Props
component = createComponent "App"

app :: JSX
app = make component { initialState, render, update } {}

initialState :: State
initialState =
  { article: "Markdown"
  }

render :: Self Props State Action -> JSX
render self =
  H.div
  { className: "app"
  , children:
    [ H.div
      { className: "header"
      , children:
        [ H.h1_
          [ H.text "App" ]
        ]
      }
    , H.div
      { className: "body"
      , children:
        [ H.div_
          [ H.label_
            [ H.span_ [ H.text "Date" ]
            , H.input { placeholder: "YYYY-MM-DD" }
            ]
          , H.button
            { onClick: capture_ self FetchArticle
            , children: [ H.text "OK" ]
            }
          ]
        , H.div_
          [ H.text "Loading..." ]
        , H.div_
          [ H.text self.state.article ]
        ]
      }
    , H.div
      { className: "footer" }
    ]
  }

update :: Self Props State Action -> Action -> StateUpdate Props State Action
update self FetchArticle =
  SideEffects
    (\self' -> do
      sendAsync self' do
        Aff.delay (Milliseconds 1000.0)
        pure (UpdateArticle "Loaded"))
update self@{ state } (UpdateArticle s) =
  Update state { article = s }
