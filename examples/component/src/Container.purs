module Container where

import Prelude

import React.Basic (CreateComponent, component, element, render)
import React.Basic.DOM as R
import ToggleButton (mkToggleButton)

mkToggleButtonContainer :: CreateComponent {}
mkToggleButtonContainer = do
  toggleButton <- mkToggleButton

  component "Container" \_ -> do
    render $ R.div
      { children:
          [ element toggleButton { label: "A" }
          , element toggleButton { label: "B" }
          ]
      }
