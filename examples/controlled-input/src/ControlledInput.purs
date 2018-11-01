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
    firstName <- useInput "hello"
    lastName <- useInput "world"

    pure $ R.form_
      [ renderInput firstName
      , renderInput lastName
      ]
  where
    renderInput input =
      fragment
        [ R.input { onChange: input.onChange, value: input.value }
        , R.p_ [ R.text ("Current value = " <> show input.value) ]
        , R.p_ [ R.text ("Changed at = " <> maybe "never" show input.lastChanged) ]
        ]

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
