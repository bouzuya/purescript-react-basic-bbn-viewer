module Component.App
  ( app
  ) where

import Bouzuya.HTTP.Client (fetch, method, url)
import Bouzuya.HTTP.Method as Method
import Data.Either (either)
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Options ((:=))
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Effect (Effect)
import Effect.Aff (Aff, error, throwError)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Markdown as Markdown
import Prelude (Unit, bind, const, discard, pure, unit, (<>))
import React.Basic (Component, JSX, Self, StateUpdate(..), capture, capture_, createComponent, keyed, make, sendAsync)
import React.Basic as JSX
import React.Basic.Components.Async as Async
import React.Basic.DOM as H
import React.Basic.DOM.Components.Ref as Ref
import React.Basic.DOM.Events (targetValue)
import Simple.JSON as SimpleJSON
import Web.DOM (Element, Node)
import Web.DOM.Element as Element

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

foreign import unsafeSetInnerHTML :: String -> Element -> Effect Unit

component :: Component Props
component = createComponent "App"

app :: JSX
app = make component { initialState, render, update } {}

fetchBbn :: String -> Aff String
fetchBbn date = do
  regex <-
    either
      (const (throwError (error "regex is invalid")))
      pure
      (Regex.regex "^\\d{4}-\\d{2}-\\d{2}$" RegexFlags.noFlags)
  if Regex.test regex date
    then pure unit
    else throwError (error "date is not YYYY-MM-DD")
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
  { article: ""
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
          [ H.text "blog.bouzuya.net Viewer" ]
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
        , Ref.ref
            (\nodeMaybe ->
              fromMaybe JSX.empty do
                node <- nodeMaybe
                element <- Element.fromNode node
                pure
                  (keyed
                    self.state.article
                    (Async.async do
                      liftEffect
                        (unsafeSetInnerHTML
                          (Markdown.toHtmlString self.state.article)
                          element)
                      pure JSX.empty)))
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
