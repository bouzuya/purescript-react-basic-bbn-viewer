module Component.App
  ( app
  ) where

import Prelude

import Bouzuya.HTTP.Client (fetch, method, url)
import Bouzuya.HTTP.Method as Method
import Data.Maybe (Maybe, fromMaybe)
import Data.Options ((:=))
import React.Basic (Component, JSX, Self, StateUpdate(..), capture_, createComponent, make, sendAsync)
import React.Basic.DOM as H
import Simple.JSON as SimpleJSON

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
        { body } <- fetch
          (method := Method.GET
          <> url := "https://blog.bouzuya.net/2018/12/08/index.json"
          )
        s <-
          pure
            (fromMaybe "ERROR" do
              b <- body :: Maybe String
              { data: d } <- SimpleJSON.readJSON_ b :: Maybe { "data" :: String }
              pure d)
        pure (UpdateArticle s))
update self@{ state } (UpdateArticle s) =
  Update state { article = s }
