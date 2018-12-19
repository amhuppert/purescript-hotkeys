module Test.Main where

import Prelude

import Data.Array (all, (!!))
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Foldable (find)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Hotkeys.KeyMap (BoundValue(..), KeySequence, ScopedKeyBindingsTree(..))
import Hotkeys.KeyMap as KeyMap
import Hotkeys.Keys as Keys
import Hotkeys.Test.Gen (S(..), Cmd)
import Test.QuickCheck (Result(..), quickCheck)
import Test.Spec (it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter as Reporter
import Test.Spec.Runner (run)

main :: Effect Unit
main = runTests

runTests :: Effect Unit
runTests = run [Reporter.consoleReporter] do
  it "Property: getScopeBindings is consistent with lookup" do
    liftEffect $ quickCheck \tree ->
      let scopes = getScopes tree
          maybeInvalid = join $ find isJust $ map (isGetScopeBindingsConsistentWithLookup tree) scopes
       in case maybeInvalid of
            Just invalid -> Failed $ show invalid <> "\n" <> show tree
            Nothing -> Success
  it "Property: For every binding X returned by getScopeBindings, there are no other bindings Y such that\
     \ the key sequence for Y is a prefix of the key sequence for X" do
    liftEffect $ quickCheck \tree ->
      let scopes = getScopes tree
       in all (noPrefixes tree) scopes
  it "Sub-scope commands override commands in the parent scope" do
    let parentCmd = "parent-cmd"
        childCmd = "child-cmd"
        keys = NEA.cons' (Keys.char 'a') [ Keys.char 'b' ]
        tree =
          Scope
          { id: "parent"
          , bindings:
            [ { command: parentCmd
              , keys
              }
            ]
          }
          [ Scope
            { id: "child"
            , bindings:
              [ { command: childCmd
                , keys
                }
              ]
            }
            []
          ]
        keyMap = KeyMap.create tree
    assertCmdEquals childCmd $ KeyMap.lookup "child" keys keyMap
  it "getScopeBindings does not return commands that have been overridden" do
    let tree =
          Scope
          { id: "scope0"
          , bindings:
            [ { command: "scope0-cmd1"
              , keys: NEA.cons' (Keys.char 'x') [ Keys.char 'y' ]
              }
            ]
          }
          [ Scope
            { id: "scope1"
            , bindings:
              [ { command: "scope1-cmd1"
                , keys: singleChar 'x'
                }
              ]
            }
            []
          ]
        keyMap = KeyMap.create tree
        expected = Map.singleton (singleChar 'x') { command: "scope1-cmd1", keys: singleChar 'x' }
    KeyMap.getScopeBindings "scope1" keyMap `shouldEqual` expected
  it "lookup does not return commands that have been overridden" do
    let overriddenKeys = NEA.cons' (Keys.char 'x') [ Keys.char 'y' ]
        tree =
          Scope
          { id: "scope0"
          , bindings:
            [ { command: "scope0-cmd1"
              , keys: overriddenKeys
              }
            ]
          }
          [ Scope
            { id: "scope1"
            , bindings:
              [ { command: "scope1-cmd1"
                , keys: singleChar 'x'
                }
              ]
            }
            []
          ]
        keyMap = KeyMap.create tree
    case KeyMap.lookup "scope1" overriddenKeys keyMap of
      Nothing -> pure unit
      Just bv -> fail $ "not Nothing: " <> showBV bv
  it "lookup returns Nothing when a command is bound to an incomplete prefix of the key sequence used for the lookup" do
    let tree =
          Scope
          { id: "a"
          , bindings:
            [ { command: "some-cmd"
              , keys: singleChar 'x'
              }
            ]
          }
          []
        keyMap = KeyMap.create tree
        lookupKeys = NEA.cons' (Keys.char 'x') [ Keys.char 'y' ]
    case KeyMap.lookup "a" lookupKeys keyMap of
      Nothing -> pure unit
      Just bv -> fail $ "not Nothing: " <> showBV bv
  it "When parent scope is active, parent bindings are not overridden by child scopes" do
    let parentCmd = "parent-cmd"
        childCmd = "child-cmd"
        keys = NEA.cons' (Keys.char 'a') [ Keys.char 'b' ]
        tree =
          Scope
          { id: "parent"
            , bindings:
              [ { command: parentCmd
                , keys
                }
              ]
            }
            [ Scope
                { id: "child"
                , bindings:
                  [ { command: childCmd
                    , keys
                    }
                  ]
                }
                []
            ]
        keyMap = KeyMap.create tree
    assertCmdEquals parentCmd $ KeyMap.lookup "parent" keys keyMap
  it "Child scopes can access commands bound in ancestor scopes" do
    let scopeA = "a"
        scopeB = "b"
        scopeC = "c"
        keyA = singleChar 'a'
        keyB = singleChar 'b'
        keyC = singleChar 'c'
        tree =
          Scope
          { id: scopeA
          , bindings:
            [ { command: scopeA
              , keys: keyA
              }
            ]
          }
          [ Scope
            { id: scopeB
            , bindings:
              [ { command: scopeB
                , keys: keyB
                }
              ]
            }
            [ Scope
              { id: scopeC
              , bindings:
                [ { command: scopeC
                  , keys: keyC
                  }
                ]
              }
              []
            ]
          ]
        keyMap = KeyMap.create tree
    assertCmdEquals scopeC $ KeyMap.lookup scopeC keyC keyMap
    assertCmdEquals scopeB $ KeyMap.lookup scopeC keyB keyMap
    assertCmdEquals scopeA $ KeyMap.lookup scopeC keyA keyMap
  it "getScopeBindings includes commands in ancestor scopes" do
    let scopeA = "a"
        scopeB = "b"
        scopeC = "c"
        keyA = singleChar 'a'
        keyB = singleChar 'b'
        keyC = singleChar 'c'
        tree =
          Scope
          { id: scopeA
          , bindings:
            [ { command: scopeA
              , keys: keyA
              }
            ]
          }
          [ Scope
            { id: scopeB
            , bindings:
              [ { command: scopeB
                , keys: keyB
                }
              ]
            }
            [ Scope
              { id: scopeC
              , bindings:
                [ { command: scopeC
                  , keys: keyC
                  }
                ]
              }
              []
            ]
          ]
        keyMap = KeyMap.create tree
        commands = KeyMap.getScopeBindings scopeC keyMap
    Map.member keyA commands `shouldEqual` true
    Map.member keyB commands `shouldEqual` true
    Map.member keyC commands `shouldEqual` true
  where
    singleChar = NEA.singleton <<< Keys.char

assertCmdEquals :: String -> Maybe (BoundValue String) -> Aff Unit
assertCmdEquals expected actual =
  case actual of
    Just (BoundCmd cmd) -> expected `shouldEqual` cmd
    _ -> fail $ "Expected command to be " <> expected <> " but was " <> show (showBV <$> actual)

isGetScopeBindingsConsistentWithLookup
  :: S
  -> Array Int -- scope
  -> Maybe { getScopeBindings :: Maybe Cmd
           , lookup :: Maybe String
           , scope :: Array Int
           , keys :: KeySequence
           }
isGetScopeBindingsConsistentWithLookup (S tree) scope =
  join $ find isJust $ map check (Array.fromFoldable $ Map.keys scopeBindings)
  where
    kmap = KeyMap.create tree
    scopeBindings = KeyMap.getScopeBindings scope kmap
    check keys =
      case Map.lookup keys scopeBindings, KeyMap.lookup scope keys kmap of
        Just {command}, Just bv@(BoundCmd command') ->
          if command == command'
             then Nothing
             else Just { getScopeBindings: Just command
                       , lookup: Just $ showBV bv
                       , scope
                       , keys
                       }
        getScopeBindings, lookup -> Just
          { getScopeBindings: toCmd getScopeBindings
          , lookup: showBV <$> lookup
          , scope
          , keys
          }
    toCmd (Just {command}) = Just command
    toCmd _ = Nothing

showBV :: forall t5. Show t5 => BoundValue t5 -> String
showBV b@(BoundCmd cmd) = "BoundCmd " <> show cmd
showBV _ = "<NestedBindingsMap>"

getScopes :: S -> Array (Array Int)
getScopes (S tree) = getScopes' tree
  where
    getScopes' (Scope {id} cs) =
      Array.cons id $ Array.concatMap getScopes' cs

findScope :: S
          -> Array Int -- scope
          -> Maybe { id :: Array Int
                   , bindings ::
                        Array { command :: Cmd
                              , keys :: KeySequence
                              }
                   , children :: Array S
                   }
findScope (S tree) target = findScope' tree
  where
    findScope' (Scope s cs) =
      if s.id == target
         then Just { id: s.id
                   , bindings: s.bindings
                   , children: map S cs
                   }
         else Array.head $
              Array.mapMaybe findScope' cs

noPrefixes :: S -> Array Int -> Boolean
noPrefixes (S tree) scope = go 0
  where
    go i | i == Array.length orderedBindings = true
    go i =
      if keysAtIndexIsPrefixOfNext i
         then false
         else go (i + 1)
    kmap = KeyMap.create tree
    bindings = KeyMap.getScopeBindings scope kmap
    orderedBindings = Array.sort $ Array.fromFoldable $ Map.keys bindings
    keysAtIndexIsPrefixOfNext i = fromMaybe false do
      curr <- orderedBindings !! i
      next <- orderedBindings !! (i + 1)
      pure $ curr `isPrefixOf` next
    isPrefixOf xs ys = NEA.take (NEA.length xs) ys == NEA.toArray xs
