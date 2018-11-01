module Counter where

import Prelude

import Effect (Effect)
import React.Basic (CreateComponent, component, ref, useEffect, useState, (/\))
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)

mkCounter :: CreateComponent {}
mkCounter = do
  component "Counter" \props -> do
    counter /\ setCounter <- useState 0

    useEffect [ref counter] do
      setDocumentTitle $ "Count: " <> show counter
      pure (pure unit)

    pure $ R.button
      { onClick: capture_ $ setCounter (_ + 1)
      , children: [ R.text $ "Increment: " <> show counter ]
      }

foreign import setDocumentTitle :: String -> Effect Unit
