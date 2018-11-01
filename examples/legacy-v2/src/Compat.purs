module React.Basic.Compat
  ( component
  , stateless
  , module React.Basic
  ) where

import Prelude

import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic (Component, JSX, element)
import React.Basic (Tuple(..), component, ref, useEffect, useState) as React

-- | Supports a common subset of the v2 API to ease the upgrade process
component
  :: forall props state
   . { displayName :: String
     , initialState :: { | state }
     , receiveProps :: { props :: { | props }, state :: { | state }, setState :: ({ | state } -> { | state }) -> Effect Unit } -> Effect Unit
     , render :: { props :: { | props }, state :: { | state }, setState :: ({ | state } -> { | state }) -> Effect Unit } -> JSX
     }
  -> Component { | props }
component { displayName, initialState, receiveProps, render } = unsafePerformEffect do
  React.component displayName \props -> do
    React.Tuple state setState <- React.useState initialState
    React.useEffect [React.ref props, React.ref state] do
      receiveProps { props, state, setState }
      pure $ pure unit
    pure $ render { props, state, setState }

-- | Supports a common subset of the v2 API to ease the upgrade process
stateless
  :: forall props
   . { displayName :: String
     , render :: { | props } -> JSX
     }
  -> Component { | props }
stateless { displayName, render } = unsafePerformEffect do
  React.component displayName (pure <<< render)
