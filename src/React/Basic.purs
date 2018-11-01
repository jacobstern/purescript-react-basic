module React.Basic
  ( Component
  , Render
  , CreateComponent
  , JSX
  , component
  , useState
  , useEffect
  , Ref
  , class ToRef
  , ref
  , empty
  , keyed
  , fragment
  , element
  , elementKeyed
  , displayName
  , module Data.Tuple
  , module Data.Tuple.Nested
  ) where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (tuple2, (/\))
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn1, runEffectFn1, runEffectFn2)
import Unsafe.Coerce (unsafeCoerce)

newtype Component props = Component (EffectFn1 props JSX)

newtype Render a = Render (Effect a)

instance functorRender :: Functor Render where
  map f (Render a) = Render (map f a)

instance applyRender :: Apply Render where
  apply (Render f) (Render a) = Render (apply f a)

instance applicativeRender :: Applicative Render where
  pure a = Render (pure a)

instance bindRender :: Bind Render where
  bind (Render m) f = Render (bind m \a -> case f a of Render b -> b)

type CreateComponent props = Effect (Component props)

component :: forall props. String -> (props -> Render JSX) -> CreateComponent props
component name render =
  let c = Component (mkEffectFn1 (unsafeCoerce render))
   in runEffectFn2 unsafeSetDisplayName name c

-- | useState
useState :: forall state. state -> Render (Tuple state ((state -> state) -> Effect Unit))
useState initialState = Render do
  { value, setValue } <- runEffectFn1 useState_ initialState
  pure (Tuple value (runEffectFn1 setValue))

foreign import useState_
  :: forall state
   . EffectFn1
       state
       { value :: state
       , setValue :: EffectFn1 (state -> state) Unit
       }

-- | useState
useEffect :: Array Ref -> Effect (Effect Unit) -> Render Unit
useEffect refs effect = Render (runEffectFn2 useEffect_ effect refs)

foreign import useEffect_
  :: EffectFn2
       (Effect (Effect Unit))
       (Array Ref)
       Unit

data Ref

class ToRef a where
  ref :: a -> Ref

instance trString :: ToRef String where
  ref = unsafeCoerce

instance trInt :: ToRef Int where
  ref = unsafeCoerce

instance trNumber :: ToRef Number where
  ref = unsafeCoerce

instance trBoolean :: ToRef Boolean where
  ref = unsafeCoerce

instance trRecord :: ToRef (Record a) where
  ref = unsafeCoerce

instance trArray :: ToRef (Array a) where
  ref = unsafeCoerce

-- | Represents rendered React VDOM (the result of calling `React.createElement`
-- | in JavaScript).
-- |
-- | `JSX` is a `Monoid`:
-- |
-- | - `append`
-- |   - Merge two `JSX` nodes using `React.Fragment`.
-- | - `mempty`
-- |   - The `empty` node; renders nothing.
-- |
-- | __*Hint:* Many useful utility functions already exist for Monoids. For example,
-- |   `guard` can be used to conditionally render a subtree of components.__
foreign import data JSX :: Type

instance semigroupJSX :: Semigroup JSX where
  append a b = fragment [ a, b ]

instance monoidJSX :: Monoid JSX where
  mempty = empty

-- | An empty `JSX` node. This is often useful when you would like to conditionally
-- | show something, but you don't want to (or can't) modify the `children` prop
-- | on the parent node.
-- |
-- | __*See also:* `JSX`, Monoid `guard`__
foreign import empty :: JSX

-- | Apply a React key to a subtree. React-Basic usually hides React's warning about
-- | using `key` props on components in an Array, but keys are still important for
-- | any dynamic lists of child components.
-- |
-- | __*See also:* React's documentation regarding the special `key` prop__
keyed :: String -> JSX -> JSX
keyed = runFn2 keyed_

-- | Render an Array of children without a wrapping component.
-- |
-- | __*See also:* `JSX`__
foreign import fragment :: Array JSX -> JSX

-- | Create a `JSX` node from a `Component`, by providing the props.
-- |
-- | This function is for non-React-Basic React components, such as those
-- | imported from FFI.
-- |
-- | __*See also:* `Component`, `elementKeyed`__
element
  :: forall props
   . Component { | props }
  -> { | props }
  -> JSX
element (Component c) props = runFn2 element_ c props

-- | Create a `JSX` node from a `Component`, by providing the props and a key.
-- |
-- | This function is for non-React-Basic React components, such as those
-- | imported from FFI.
-- |
-- | __*See also:* `Component`, `element`, React's documentation regarding the special `key` prop__
elementKeyed
  :: forall props
   . Component { | props }
  -> { key :: String | props }
  -> JSX
elementKeyed = runFn2 elementKeyed_

-- | Retrieve the Display Name from a `ComponentSpec`. Useful for debugging and improving
-- | error messages in logs.
-- |
-- | __*See also:* `displayNameFromSelf`, `createComponent`__
foreign import displayName
  :: forall props
   . Component props
  -> String



-- |
-- | Internal utility or FFI functions
-- |

foreign import unsafeSetDisplayName
  :: forall props
   . EffectFn2 String (Component props) (Component props)

foreign import keyed_ :: Fn2 String JSX JSX

foreign import element_
  :: forall props
   . Fn2 (EffectFn1 { | props } JSX) { | props } JSX

foreign import elementKeyed_
  :: forall props
   . Fn2 (Component { | props }) { key :: String | props } JSX
