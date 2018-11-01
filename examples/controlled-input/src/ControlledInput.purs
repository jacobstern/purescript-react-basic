module ControlledInput where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe, maybe)
import React.Basic (CreateComponent, Render, component, fragment, useState, (/\))
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture, targetValue, timeStamp)
import React.Basic.Events (EventHandler, merge)

mkControlledInput :: CreateComponent {}
mkControlledInput =
  component "ControlledInput" \props -> do
    input <- useInput "hello world"

    pure $ fragment
      [ R.input { onChange: input.onChange, value: input.value }
      , R.p_ [ R.text ("Current value = " <> show input.value) ]
      , R.p_ [ R.text ("Changed at = " <> maybe "never" show input.lastChanged) ]
      ]
  where
    initialState =
      { value: "hello world"
      , lastChanged: Nothing
      }

useInput :: String -> Render { onChange :: EventHandler, value :: String, lastChanged :: Maybe Number }
useInput initialValue = do
  { value, lastChanged } /\ replaceState <- useState { value: initialValue, lastChanged: Nothing }
  pure
    { onChange: capture
        (merge { targetValue, timeStamp })
        \{ timeStamp, targetValue } -> do
          replaceState \_ ->
            { value: fromMaybe "" targetValue
            , lastChanged: Just timeStamp
            }
    , value
    , lastChanged
    }
