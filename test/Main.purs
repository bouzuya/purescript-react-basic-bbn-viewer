module Test.Main
  ( main
  ) where

import Effect (Effect)
import Markdown as Markdown
import Prelude (Unit, discard)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  suite "Main" do
    test "1 == 1" do
      Assert.equal 1 1
  suite "Markdown" do
    test "toHtmlString" do
      Assert.equal
        "<p>Hello</p>\n"
        (Markdown.toHtmlString "Hello")
