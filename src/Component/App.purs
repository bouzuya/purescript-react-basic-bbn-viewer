module Component.App
  ( app
  ) where

import Prelude

import Bouzuya.HTTP.Client (fetch, method, url)
import Bouzuya.HTTP.Method as Method
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Options ((:=))
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Effect.Aff (Aff, error, throwError)
import React.Basic (Component, JSX, Self, StateUpdate(..), capture, capture_, createComponent, make, sendAsync)
import React.Basic.DOM as H
import React.Basic.DOM.Events (targetValue)
import Simple.JSON as SimpleJSON

type Props =
  {}

type State =
  { article :: String
  , date :: String
  }

data Action
  = FetchArticle
  | UpdateArticle String
  | UpdateDate String

component :: Component Props
component = createComponent "App"

app :: JSX
app = make component { initialState, render, update } {}

fetchBbn :: String -> Aff String
fetchBbn date = do
  date' <- pure (String.replaceAll (Pattern "-") (Replacement "/") date)
  { body } <- fetch
    (method := Method.GET
    <> url := ("https://blog.bouzuya.net/" <> date' <> "/index.json")
    )
  b <- maybe (throwError (error "body is nothing")) pure body
  { "data": d } <-
    maybe
      (throwError (error "json is invalid"))
      pure
      (SimpleJSON.readJSON_ b :: Maybe { "data" :: String })
  pure d

initialState :: State
initialState =
  { article: "Markdown"
  , date: ""
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
            , H.input
              { onChange: capture self targetValue (\v -> UpdateDate (fromMaybe "" v))
              , placeholder: "YYYY-MM-DD"
              }
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
        s <- fetchBbn self'.state.date
        pure (UpdateArticle s))
update self@{ state } (UpdateArticle s) =
  Update state { article = s }
update self@{ state } (UpdateDate s) =
  Update state { date = s }
