module Counter where

import Prelude

import Data.Profunctor (class Profunctor, arr)
import Data.Profunctor.Strong (class Strong, (***))
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import React.Basic (CreateComponent, component, toKey)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)
import React.Basic.Static (staticRender, useEffect, useState)

twin :: forall a. a -> Tuple a a
twin a = Tuple a a

splitRight
  :: forall p a b c
     . Category p => Strong p => Semigroupoid p
     => p a b
     -> p a c
     -> p a c
splitRight x y = twin ^>> (x *** y) >>^ snd

splitArrRight :: forall p a b c . Category p => Strong p => Semigroupoid p => p a b -> (a -> c) -> p a c
splitArrRight x f = splitRight x (arr f)

postArrRight :: forall f a b c. Category f => Profunctor f => f a b -> (b -> c) -> f a c
postArrRight a f = a >>> arr f

infixr 1 postArrRight as >>^

preArrRight :: forall f a b c. Category f => Profunctor f => (a -> b) -> f b c -> f a c
preArrRight f a = arr f >>> a

infixr 1 preArrRight as ^>>

mkCounter :: CreateComponent {}
mkCounter = component "Counter" $ staticRender go
  where
    go = const 1 ^>> useState >>>
         splitArrRight (updateDocumentTitle ^>> useEffect) jsx
    updateDocumentTitle (Tuple counter _) =
      Tuple [toKey counter] do
        setDocumentTitle $ "Count: " <> show counter
        pure (pure unit)
    jsx (Tuple counter setCounter) =
      R.button
      { onClick: capture_ $ setCounter (_ + 1)
      , children: [ R.text $ "Increment: " <> show counter ]
      }

foreign import setDocumentTitle :: String -> Effect Unit
