module ToggleButton where

import Prelude

import Effect.Console (log)
import React.Basic (CreateComponent, component, ref, useEffect, useState, (/\))
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)

mkToggleButton :: CreateComponent { label :: String }
mkToggleButton = do
  component "ToggleButton" \{ label } -> do
    on /\ setOn <- useState false

    useEffect [ref on] do
      log $ "State: " <> if on then "On" else "Off"
      pure (pure unit)

    pure $ R.button
      { onClick: capture_ $ setOn not
      , children:
          [ R.text label
          , R.text if on then " On" else " Off"
          ]
      }
