module Test.Main where

import Prelude

import Data.Array ((!!))
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Foldable (find)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Hotkeys.KeyMap (BoundValue(..), KeySequence)
import Hotkeys.KeyMap as KeyMap
import Hotkeys.Keys as Keys
import Hotkeys.Test.Gen (Cmd, S(..), genScopes)
import Test.QuickCheck (Result(..), arbitrary, quickCheckGen)
import Test.Spec (it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter as Reporter
import Test.Spec.Runner (run)

main :: Effect Unit
main = runTests

runTests :: Effect Unit
runTests = run [Reporter.consoleReporter] do
  it "Property: getAllAccessibleCommands is consistent with lookup" do
    liftEffect $ quickCheckGen do
      scopeBindings <- arbitrary
      scopes <- genScopes scopeBindings
      let maybeInvalid = isGetScopeBindingsConsistentWithLookup scopeBindings scopes
      case maybeInvalid of
        Just invalid -> pure $ Failed $ show invalid <> "\n" <> show scopeBindings
        Nothing -> pure Success
  it "Property: For every binding X returned by getAllAccessibleCommands, there are no other bindings Y such that\
     \ the key sequence for Y is a prefix of the key sequence for X" do
    liftEffect $ quickCheckGen do
      scopeBindings <- arbitrary
      scopes <- genScopes scopeBindings
      pure $ noPrefixes scopeBindings scopes
  it "Sub-scope commands override commands in the parent scope" do
    let parentCmd = "parent-cmd"
        childCmd = "child-cmd"
        keys = NEA.cons' (Keys.char 'a') [ Keys.char 'b' ]
        tree =
          [ { scope: "parent"
            , bindings:
              [ { command: parentCmd
                , keys
                }
              ]
            }
          , { scope: "child"
            , bindings:
              [ { command: childCmd
                , keys
                }
              ]
            }
          ]
        keyMap = KeyMap.create tree
    assertCmdEquals childCmd $ KeyMap.lookup [ "parent", "child" ] keys keyMap
  it "getAllAccessibleCommands does not return commands that have been overridden" do
    let tree =
          [ { scope: "scope0"
            , bindings:
              [ { command: "scope0-cmd1"
                , keys: NEA.cons' (Keys.char 'x') [ Keys.char 'y' ]
                }
              ]
            }
          , { scope: "scope1"
            , bindings:
              [ { command: "scope1-cmd1"
                , keys: singleChar 'x'
                }
              ]
            }
          ]
        keyMap = KeyMap.create tree
        expected = Map.singleton (singleChar 'x') { command: "scope1-cmd1", keys: singleChar 'x' }
    KeyMap.getAllAccessibleCommands [ "scope0", "scope1" ] keyMap `shouldEqual` expected
  it "lookup does not return commands that have been overridden" do
    let overriddenKeys = NEA.cons' (Keys.char 'x') [ Keys.char 'y' ]
        tree =
          [{ scope: "scope0"
            , bindings:
              [ { command: "scope0-cmd1"
                , keys: overriddenKeys
                }
              ]
            }
          , { scope: "scope1"
            , bindings:
              [ { command: "scope1-cmd1"
                , keys: singleChar 'x'
                }
              ]
            }
          ]
        keyMap = KeyMap.create tree
    case KeyMap.lookup [ "scope0", "scope1" ] overriddenKeys keyMap of
      Nothing -> pure unit
      Just bv -> fail $ "not Nothing: " <> showBV bv
  it "lookup returns Nothing when a command is bound to an incomplete prefix of the key sequence used for the lookup" do
    let tree =
          [ { scope: "a"
            , bindings:
              [ { command: "some-cmd"
                , keys: singleChar 'x'
                }
              ]
            }
          ]
        keyMap = KeyMap.create tree
        lookupKeys = NEA.cons' (Keys.char 'x') [ Keys.char 'y' ]
    case KeyMap.lookup [ "a" ] lookupKeys keyMap of
      Nothing -> pure unit
      Just bv -> fail $ "not Nothing: " <> showBV bv
  it "When parent scope is active, parent bindings are not overridden by child scopes" do
    let parentCmd = "parent-cmd"
        childCmd = "child-cmd"
        keys = NEA.cons' (Keys.char 'a') [ Keys.char 'b' ]
        tree =
          [{ scope: "parent"
            , bindings:
              [ { command: parentCmd
                , keys
                }
              ]
            }
          , { scope: "child"
            , bindings:
              [ { command: childCmd
                , keys
                }
              ]
            }
          ]
        keyMap = KeyMap.create tree
    assertCmdEquals parentCmd $ KeyMap.lookup [ "parent" ] keys keyMap
  it "Child scopes can access commands bound in ancestor scopes" do
    let scopeA = "a"
        scopeB = "b"
        scopeC = "c"
        keyA = singleChar 'a'
        keyB = singleChar 'b'
        keyC = singleChar 'c'
        tree =
          [ { scope: scopeA
            , bindings:
              [ { command: scopeA
                , keys: keyA
                }
              ]
            }
          , { scope: scopeB
            , bindings:
              [ { command: scopeB
                , keys: keyB
                }
              ]
            }
          , { scope: scopeC
            , bindings:
              [ { command: scopeC
                , keys: keyC
                }
              ]
            }
          ]
        keyMap = KeyMap.create tree
    assertCmdEquals scopeC $ KeyMap.lookup [ scopeA, scopeB, scopeC ] keyC keyMap
    assertCmdEquals scopeB $ KeyMap.lookup [ scopeA, scopeB, scopeC ] keyB keyMap
    assertCmdEquals scopeA $ KeyMap.lookup [ scopeA, scopeB, scopeC ] keyA keyMap
  it "getAllAccessibleCommands includes commands in ancestor scopes" do
    let scopeA = "a"
        scopeB = "b"
        scopeC = "c"
        keyA = singleChar 'a'
        keyB = singleChar 'b'
        keyC = singleChar 'c'
        tree =
          [ { scope: scopeA
            , bindings:
              [ { command: scopeA
                , keys: keyA
                }
              ]
            }
          , { scope: scopeB
            , bindings:
              [ { command: scopeB
              , keys: keyB
                }
              ]
            }
          , { scope: scopeC
            , bindings:
              [ { command: scopeC
                , keys: keyC
                }
              ]
            }
          ]
        keyMap = KeyMap.create tree
        commands = KeyMap.getAllAccessibleCommands [ scopeA, scopeB, scopeC ] keyMap
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
  -> Maybe { getAllAccessibleCommands :: Maybe Cmd
           , lookup :: Maybe String
           , scopes :: Array Int
           , keys :: KeySequence
           }
isGetScopeBindingsConsistentWithLookup (S tree) scopes =
  join $ find isJust $ map check (Array.fromFoldable $ Map.keys scopeBindings)
  where
    kmap = KeyMap.create tree
    scopeBindings = KeyMap.getAllAccessibleCommands scopes kmap
    check keys =
      case Map.lookup keys scopeBindings, KeyMap.lookup scopes keys kmap of
        Just {command}, Just bv@(BoundCmd command') ->
          if command == command'
             then Nothing
             else Just { getAllAccessibleCommands: Just command
                       , lookup: Just $ showBV bv
                       , scopes
                       , keys
                       }
        getAllAccessibleCommands, lookup -> Just
          { getAllAccessibleCommands: toCmd getAllAccessibleCommands
          , lookup: showBV <$> lookup
          , scopes
          , keys
          }
    toCmd (Just {command}) = Just command
    toCmd _ = Nothing

showBV :: forall t5. Show t5 => BoundValue t5 -> String
showBV b@(BoundCmd cmd) = "BoundCmd " <> show cmd
showBV _ = "<NestedBindingsMap>"

getScopes :: S -> Array Int
getScopes (S tree) = map (\s -> s.scope) tree

noPrefixes :: S -> Array Int -> Boolean
noPrefixes (S tree) scope = go 0
  where
    go i | i == Array.length orderedBindings = true
    go i =
      if keysAtIndexIsPrefixOfNext i
         then false
         else go (i + 1)
    kmap = KeyMap.create tree
    bindings = KeyMap.getAllAccessibleCommands scope kmap
    orderedBindings = Array.sort $ Array.fromFoldable $ Map.keys bindings
    keysAtIndexIsPrefixOfNext i = fromMaybe false do
      curr <- orderedBindings !! i
      next <- orderedBindings !! (i + 1)
      pure $ curr `isPrefixOf` next
    isPrefixOf xs ys = NEA.take (NEA.length xs) ys == NEA.toArray xs
