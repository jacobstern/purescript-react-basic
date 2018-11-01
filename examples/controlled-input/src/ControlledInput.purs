module ControlledInput where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe, maybe)
import React.Basic (CreateComponent, component, fragment, render, useState, (/\))
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture, targetValue, timeStamp)
import React.Basic.Events (merge)

mkControlledInput :: CreateComponent {}
mkControlledInput =
  component "ControlledInput" \props -> do
    state /\ replaceState <- useState initialState

    render $ fragment
      [ R.input
          { onChange:
              capture (merge { targetValue, timeStamp })
                \{ timeStamp, targetValue } -> do
                  replaceState \_ ->
                    { value: fromMaybe "" targetValue
                    , timestamp: Just timeStamp
                    }
          , value: state.value
          }
      , R.p_ [ R.text ("Current value = " <> show state.value) ]
      , R.p_ [ R.text ("Changed at = " <> maybe "never" show state.timestamp) ]
      ]
  where
    initialState =
      { value: "hello world"
      , timestamp: Nothing
      }
