module Markdown
  ( toHtmlString
  ) where

foreign import toHtmlStringImpl :: String -> String

toHtmlString :: String -> String
toHtmlString s = toHtmlStringImpl s
