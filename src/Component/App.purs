module Component.App
  ( app
  ) where

import React.Basic (Component, JSX, Self, StateUpdate(..), createComponent, make, send)
import React.Basic.DOM as H

type Props =
  {}

type State =
  {}

data Action
  = Noop

component :: Component Props
component = createComponent "App"

app :: JSX
app = make component { initialState, render, update } {}

initialState :: State
initialState =
  {}

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
          , H.button_ [ H.text "OK" ]
          ]
        , H.div_
          [ H.text "Loading..." ]
        , H.div_
          [ H.text "Markdown" ]
        ]
      }
    , H.div
      { className: "footer" }
    ]
  }

update :: Self Props State Action -> Action -> StateUpdate Props State Action
update self Noop = NoUpdate
