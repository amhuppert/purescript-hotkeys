module Hotkeys.Test.Gen where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List as List
import Data.Maybe (fromJust)
import Data.NonEmpty ((:|))
import Data.Tuple (Tuple(..))
import Hotkeys.KeyMap (ScopedKeyBindingsTree(..))
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen as G

type Cmd = { scope :: Array Int
           , n :: Int
           }

newtype S = S (ScopedKeyBindingsTree (Array Int) Cmd)

derive instance genericS :: Generic S _

instance showS :: Show S where
  show = genericShow

instance arbitraryS :: Arbitrary S where
  arbitrary = genS

genS :: Gen S
genS = do
  frequentKeySequence <- genKeySequence
  tree <- genTree frequentKeySequence [0] 0
  pure $ S tree
  where
    genTree frequentKeySequence parentScope i = G.sized \size -> do
      let scope = Array.snoc parentScope i
          maxChildren = size
      childCount <- G.chooseInt 0 maxChildren
      children <- G.resize (size / 2) $ genChildren frequentKeySequence scope childCount 0
      bindings <- genBindings frequentKeySequence scope
      pure $ Scope
        { id: scope
        , bindings
        }
        children
    genChildren frequentKeySequence parentScope childCount i
      | i == childCount = pure []
      | otherwise = do
          child <- genTree frequentKeySequence parentScope i
          rest <- genChildren frequentKeySequence parentScope childCount (i+1)
          pure $ [child] <> rest
    genBindings frequentKeySequence scope = G.arrayOf (genBinding frequentKeySequence scope)
    genBinding frequentKeySequence scope = do
      n <- arbitrary
      let command = { scope, n }
          frequentPrefix = do
            seq <- genKeySequence
            pure $ frequentKeySequence <> seq
      keys <- G.frequency $
              (Tuple 0.3 $ pure frequentKeySequence) :|
              List.fromFoldable
              [ Tuple 0.5 genKeySequence
              , Tuple 0.2 frequentPrefix
              ]
      pure { command, keys }
    genKeySequence = do
      len <- G.chooseInt 1 4
      keys <- G.vectorOf len arbitrary
      let nonEmptyKeys = unsafePartial $ fromJust $ NEA.fromArray keys
      pure nonEmptyKeys
