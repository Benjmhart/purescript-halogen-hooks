module Example.Hooks.UseEvent
  ( useToggle
  , UseEvent
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\), type (/\))
import Effect.Class (class MonadEffect)
import Effect.Class.Console(log)
import Halogen.Hooks (HookM, Hook, UseState)
import Halogen.Hooks as Hooks

newtype UseEvent hooks = UseEvent (UseState Boolean hooks)

derive instance newtypeUseEvent :: Newtype (UseEvent hooks) _

useToggle
  :: forall slots output m 
   . MonadEffect m 
  => (Boolean -> Boolean)
  -> (Boolean -> (HookM slots output m Unit) -> HookM slots output m Unit)
  -> Hook slots output m UseEvent (Boolean /\ HookM slots output m Unit)
useToggle predicate effectfn = Hooks.wrap Hooks.do
   state /\ stateToken <- Hooks.useState false
   let 
     modifyToggle = do
       if not $ predicate state 
         then pure unit
         else do
           effectfn state (Hooks.modify_ stateToken \s -> not s)
           Hooks.modify_ stateToken \s -> not s
   Hooks.pure (state /\ modifyToggle)

