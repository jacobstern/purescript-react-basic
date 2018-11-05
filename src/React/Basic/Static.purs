module React.Basic.Static (Static, staticRender, useState, useEffect) where

import Prelude

import Data.Either (Either(..), either)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Star (Star(..))
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple, uncurry)
import Effect (Effect)
import React.Basic (Key, Render, unsafeRender)
import React.Basic as Render

newtype Static a b = Static (Star Render a b)

derive newtype instance functorStatic :: Functor (Static a)
derive newtype instance applyStatic :: Apply (Static a)
derive newtype instance semigroupoidStatic :: Semigroupoid Static
derive newtype instance profunctorStatic :: Profunctor Static
derive newtype instance strongStatic :: Strong Static

instance categoryStatic :: Category Static where
  identity = Static (Star unsafeRender)

instance applicativeStatic :: Applicative (Static a) where
  pure x = Static (Star \_ -> unsafeRender x)

instance choiceStatic :: Choice Static where
  left (Static (Star f)) = Static <<< Star $ either (map Left <<< f) (unsafeRender <<< Right)
  right (Static (Star f)) = Static <<< Star $ either (unsafeRender <<< Left) (map Right <<< f)

staticRender :: forall a b. Static a b -> (a -> Render b)
staticRender (Static (Star f)) = f

useState
  :: forall state
     . Static state (Tuple state ((state -> state) -> Effect Unit))
useState = Static <<< Star $ Render.useState

useEffect :: Static (Tuple (Array Key) (Effect (Effect Unit))) Unit
useEffect = Static <<< Star $ uncurry Render.useEffect
