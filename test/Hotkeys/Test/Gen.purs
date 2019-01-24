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
import Hotkeys.KeyMap (Binding)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen as G

type Cmd = { scope :: Int
           , n :: Int
           }

newtype S = S (Array { scope :: Int, bindings :: Array (Binding Cmd)})

derive instance genericS :: Generic S _

instance showS :: Show S where
  show = genericShow

instance arbitraryS :: Arbitrary S where
  arbitrary = genS

genS :: Gen S
genS = do
  frequentKeySequence <- genKeySequence
  numScopes <- G.chooseInt 0 5
  scopeBindings <- G.vectorOf numScopes $ genScope frequentKeySequence 0
  pure $ S scopeBindings
  where
    genScope frequentKeySequence i = do
      let scope = i
      bindings <- genBindings frequentKeySequence scope
      pure { scope, bindings }
    genBindings frequentKeySequence scope = do
      n <- G.chooseInt 0 10
      G.vectorOf n (genBinding frequentKeySequence scope)
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

genScopes :: S -> Gen (Array Int)
genScopes (S sb) = do
  let scopes = map _.scope sb
  count <- G.chooseInt 0 (Array.length scopes)
  Array.take count <$> G.shuffle scopes
