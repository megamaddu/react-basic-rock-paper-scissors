module Components.App where

import Prelude

import Components.Game (mkGame)
import React.Basic.Hooks (CreateComponent, component, element)

mkApp :: CreateComponent {}
mkApp = do
  game <- mkGame
  component "App" \_ ->
    pure $ element game {}
