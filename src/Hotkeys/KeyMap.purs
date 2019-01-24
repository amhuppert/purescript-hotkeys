module Hotkeys.KeyMap
       ( BoundValue(..)
       , KeyBindingsMap
       , KeySequence(..)
       , Binding(..)
       , lookup
       , lookupCommand
       , create
       , getAllAccessibleCommands
       , flattenBindings
       ) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray, (!!))
import Data.Array.NonEmpty as NEA
import Data.Foldable (foldl, foldr)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Hotkeys.Keys (KeyPress)

newtype KeyBindingsMap scope cmd = KeyBindingsMap (Map scope (Map KeyPress (BoundValue cmd)))

data BoundValue cmd = NestedBindingsMap (Map KeyPress (BoundValue cmd)) | BoundCmd cmd

derive instance genericBoundValue :: Generic (BoundValue cmd) _

type KeySequence = NonEmptyArray KeyPress

type Binding cmd =
  { command :: cmd
  , keys :: KeySequence
  }

-- | Lookup whatever's bound to the key sequence in the given scopes.
-- | If the key sequence matches that of a command, that command will be returned.
-- | If the key sequence partially matches that of one or more commands,
-- | a key bindings map will be returned.
-- | Otherwise, returns Nothing.
-- |
-- | In the case where the commands from different scopes have conflicting key sequences,
-- | the command from the scope specified later in the array overrides commands in earlier scopes.
lookup :: forall scope cmd. Ord scope => Array scope -> KeySequence -> KeyBindingsMap scope cmd -> Maybe (BoundValue cmd)
lookup scopes keys kbm = do
  let bindings = mergeScopes scopes kbm
  doLookup 0 bindings

  where
    doLookup i bs | Just bv <- (keys !! i) >>= flip Map.lookup bs =
      case bv of
        BoundCmd cmd ->
          if i == NEA.length keys - 1
             then Just bv
             else Nothing
        NestedBindingsMap nested -> do
          let nextIx = i + 1
          if nextIx < NEA.length keys
            then doLookup nextIx nested
            else Just bv
    doLookup _ _ = Nothing

-- | The order in which the scopes are provided matters.
-- | Commands in later scopes will override commands with the same bindings in earlier scopes.
mergeScopes :: forall scope cmd. Ord scope => Array scope -> KeyBindingsMap scope cmd -> Map KeyPress (BoundValue cmd)
mergeScopes scopes (KeyBindingsMap bindingsByScope) = foldl doMerge Map.empty scopes
  where
    doMerge bs scope =
      maybe
        bs
        (\currScopeBindings -> Map.union currScopeBindings bs)
        (Map.lookup scope bindingsByScope)

lookupCommand :: forall scope cmd. Ord scope => Array scope -> KeySequence -> KeyBindingsMap scope cmd -> Maybe cmd
lookupCommand scopes keys kmap =
  case lookup scopes keys kmap of
    Just (BoundCmd cmd) -> Just cmd
    _ -> Nothing

-- | Get a complete list of all commands accessible in the specified scopes.
getAllAccessibleCommands :: forall scope cmd. Ord scope => Array scope -> KeyBindingsMap scope cmd -> Map KeySequence (Binding cmd)
getAllAccessibleCommands scopes kbm = flattenBindings $ mergeScopes scopes kbm

flattenBindings :: forall cmd. Map KeyPress (BoundValue cmd) -> Map KeySequence (Binding cmd)
flattenBindings = doFlatten []
  where
    doFlatten keys kmap = foldlWithIndex flattenKey Map.empty kmap
      where
        flattenKey key accum = case _ of
          NestedBindingsMap nestedMap -> accum `Map.union` doFlatten (NEA.toArray currKeys) nestedMap
          BoundCmd command ->
            let binding = { command, keys: currKeys }
             in Map.insert currKeys binding accum
          where
            currKeys = NEA.snoc' keys key

create :: forall scope cmd. Ord scope =>
          Array { scope :: scope, bindings :: Array (Binding cmd)}
       -> KeyBindingsMap scope cmd
create scopeBindings =
  let addScopeBindings accum s = Map.insert s.scope (toBindingsMap s.bindings) accum
   in KeyBindingsMap $ foldl addScopeBindings Map.empty scopeBindings
  where
    toBindingsMap = map singletonBindingsMap >>> mergeCommands
      where
        singletonBindingsMap {command, keys} = go keys
          where go ks  = let {head,tail} = NEA.uncons ks
                             boundedValue =
                                case NEA.fromArray tail of
                                  Just nonEmptyTail -> NestedBindingsMap $ go nonEmptyTail
                                  Nothing -> BoundCmd command
                          in Map.singleton head boundedValue
        mergeCommands = foldr (Map.unionWith mergeBoundValues) Map.empty
          where
            mergeBoundValues (BoundCmd x) (BoundCmd y) = BoundCmd y
            mergeBoundValues (BoundCmd x) (NestedBindingsMap y) = BoundCmd x
            mergeBoundValues (NestedBindingsMap x) (BoundCmd y) = BoundCmd y
            mergeBoundValues (NestedBindingsMap x) (NestedBindingsMap y) =
              NestedBindingsMap $ Map.unionWith mergeBoundValues x y
