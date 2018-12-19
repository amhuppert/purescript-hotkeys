module Hotkeys.KeyMap
       ( BoundValue(..)
       , KeyBindingsMap
       , ScopedKeyBindingsTree(..)
       , KeySequence(..)
       , Binding(..)
       , lookup
       , lookupCommand
       , create
       , getScopeBindings
       , flattenBindings
       ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray, (!!))
import Data.Array.NonEmpty as NEA
import Data.Foldable (foldl, foldr)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.String (joinWith) as String
import Data.String.CodeUnits (fromCharArray) as String

import Hotkeys.Keys (KeyPress)

newtype KeyBindingsMap scope cmd = KeyBindingsMap (Map scope (CBind scope cmd))

data BoundValue cmd = NestedBindingsMap (Map KeyPress (BoundValue cmd)) | BoundCmd cmd

derive instance genericBoundValue :: Generic (BoundValue cmd) _

type KeySequence = NonEmptyArray KeyPress

type CBind scope cmd = { parentScope :: Maybe scope
                       , bindings :: Map KeyPress (BoundValue cmd)
                       }

type Binding cmd =
  { command :: cmd
  , keys :: KeySequence
  }

lookup :: forall scope cmd. Ord scope => scope -> KeySequence -> KeyBindingsMap scope cmd -> Maybe (BoundValue cmd)
lookup scope keys sm@(KeyBindingsMap scopeMap) = do
    { parentScope, bindings } <- (Map.lookup scope scopeMap)
    case lookupCmdWithinScope bindings keys of
      Found boundValue -> pure boundValue
      OverrideParent -> Nothing
      NotFound -> parentScope >>= \s -> lookup s keys sm

  where
    lookupCmdWithinScope bindings ks = doLookup 0 bindings
      where
        doLookup i bs | Just bv <- (ks !! i) >>= flip Map.lookup bs =
          case bv of
            BoundCmd cmd ->
              if i == NEA.length ks - 1
                 then Found bv
                 else OverrideParent
            NestedBindingsMap nested -> do
              let nextIx = i + 1
              if nextIx < NEA.length ks
                then doLookup nextIx nested
                else Found bv
        doLookup _ _ = NotFound

data LookupResult cmd =
    NotFound
  | OverrideParent
  | Found (BoundValue cmd)

lookupCommand :: forall scope cmd. Ord scope => scope -> KeySequence -> KeyBindingsMap scope cmd -> Maybe cmd
lookupCommand scope keys kmap =
  case lookup scope keys kmap of
    Just (BoundCmd cmd) -> Just cmd
    _ -> Nothing


getScopeBindings :: forall scope cmd. Ord scope => scope -> KeyBindingsMap scope cmd -> Map KeySequence (Binding cmd)
getScopeBindings scope kmap'@(KeyBindingsMap kmap) = foldl addBinding Map.empty allBoundKeySeqs
  where
    addBinding accum keys =
      case lookupCommand scope keys kmap' of
        Just command ->
          let binding = { command, keys }
           in Map.insert keys binding accum
        Nothing -> accum
    -- Includes those that are overridden. They will be filtered out later.
    allBoundKeySeqs = go Set.empty scope
      where
        go accum s =
          case Map.lookup s kmap of
            Just {parentScope: Just parentScope, bindings} ->
              go (Set.union accum $ getKeys bindings) parentScope
            Just {parentScope: Nothing, bindings} ->
              Set.union accum $ getKeys bindings
            Nothing -> accum
        getKeys = Map.keys <<< flattenBindings

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

data ScopedKeyBindingsTree scope cmd =
  Scope { id :: scope
        , bindings :: Array (Binding cmd)
        }
        (Array (ScopedKeyBindingsTree scope cmd))

derive instance genericScopedKeyBindingsTree :: Generic (ScopedKeyBindingsTree scope cmd) _

instance showScopedKeyBindingsTree :: (Show scope, Show cmd) => Show (ScopedKeyBindingsTree scope cmd) where
  show = show' 0
    where
      show' depth (Scope s cs) =
        let ind = indent depth
            ind2 = indent (depth+1)
            ind3 = indent (depth+2)
            cs' = map (show' (depth + 1)) cs
         in ind <> "Scope " <> "\n" <>
            ind2 <> "id: " <> show s.id <> "\n" <>
            ind2 <> "bindings: \n" <>
            String.joinWith "\n" (map (\b -> ind3 <> show b) s.bindings) <> "\n" <>
            ind <> "[\n" <>
            String.joinWith "\n" cs' <>
            "\n" <> ind <> "]"
      indent n = String.fromCharArray $ Array.replicate (2 * n) ' '

create :: forall scope cmd. Ord scope => ScopedKeyBindingsTree scope cmd -> KeyBindingsMap scope cmd
create scopeTree = KeyBindingsMap $ doCreate Nothing scopeTree Map.empty
  where
    doCreate :: Maybe scope -> ScopedKeyBindingsTree scope cmd -> Map scope (CBind scope cmd) -> Map scope (CBind scope cmd)
    doCreate parentScope (Scope {id: currScopeId, bindings: currScopeBindings} childScopes) accum =
      let currScopeMapValue = toMapValue parentScope currScopeBindings
          initial = Map.insert currScopeId currScopeMapValue accum
       in foldr (doCreate (Just currScopeId)) initial childScopes
    toMapValue parentScope bindings =
      let bMap = toBindingsMap bindings
        in { parentScope, bindings: bMap }
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
