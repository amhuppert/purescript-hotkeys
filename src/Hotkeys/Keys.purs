module Hotkeys.Keys
       ( KeyValue(..)
       , keyboardEventToKeyValue
       , KeyPress(..)
       , keyboardEventToKeyPress
       , key
       , key'
       , char
       , char'
       , Modifiers(..)
       , noModifiers
       ) where

import Prelude

import Data.Char as Char
import Data.Either (Either(..), either)
import Data.Enum (fromEnum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromJust)
import Data.String as String
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck.Arbitrary (class Arbitrary, genericArbitrary)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KeyboardEvent

data KeyValue =
    KeyChar Char

  -- modifier keys
  | Alt
  | AltGraph
  | CapsLock
  | Control
  | Fn
  | FnLock
  | Hyper
  | Meta
  | NumLock
  | ScrollLock
  | Shift
  | Super
  | Symbol
  | SymbolLock

  -- white space keys
  | Enter
  | Tab

  -- navigation keys
  | ArrowDown
  | ArrowLeft
  | ArrowRight
  | ArrowUp
  | End
  | Home
  | PageDown
  | PageUp

  -- editing keys
  | Backspace
  | Clear
  | Copy
  | CrSel
  | Cut
  | Delete
  | EraseEof
  | ExSel
  | Insert
  | Paste
  | Redo
  | Undo

  -- UI keys
  | Accept
  | Again
  | Attn
  | Cancel
  | ContextMenu
  | Escape
  | Execute
  | Find
  | Finish
  | Help
  | Pause
  | Play
  | Props
  | Select
  | ZoomIn
  | ZoomOut

  -- function keys
  | F1
  | F2
  | F3
  | F4
  | F5
  | F6
  | F7
  | F8
  | F9
  | F10
  | F11
  | F12
  | F13
  | F14
  | F15
  | F16
  | F17
  | F18
  | F19
  | F20
  | Soft1
  | Soft2
  | Soft3
  | Soft4

  -- document keys
  | Close
  | New
  | Open
  | Print
  | Save
  | SpellCheck
  | MailForward
  | MailReply
  | MailSend

  -- browser control keys
  | BrowserBack
  | BrowserFavorites
  | BrowserForward
  | BrowserHome
  | BrowserRefresh
  | BrowserSearch
  | BrowserStop

  -- number pad keys
  | Key11
  | Key12

derive instance eqKeyValue :: Eq KeyValue

derive instance ordKeyValue :: Ord KeyValue

derive instance genericKeyValue :: Generic KeyValue _

instance showKeyValue :: Show KeyValue where
  show = genericShow

instance arbitraryKeyValue :: Arbitrary KeyValue where
  arbitrary = genericArbitrary

-- | Gets the value of the key pressed by the user while taking into consideration the state of the modifier keys as well as the keyboard locale/layout.
-- |
-- | Additionally, this function attempts to resolve inconsistencies across different browsers. For example, some browsers report the Windows key as "OS", while others report it as "Meta". This function will map either to the `Meta` key value.
-- |
-- | See https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key/Key_Values for more information.
keyboardEventToKeyValue :: KeyboardEvent -> Maybe KeyValue
keyboardEventToKeyValue = either Just (const Nothing) <<< parse
  where
    parse = parsePrintableChar >=> parseNonPrintable

parsePrintableChar :: KeyboardEvent -> Either KeyValue KeyboardEvent
parsePrintableChar e =
  case String.uncons (KeyboardEvent.key e) of
    Just { head, tail } ->
      if String.null tail
        then Left $ KeyChar (toChar head)
        else Right e
    Nothing -> Right e

  where
    toChar codePoint = unsafePartial $ (fromEnum >>> Char.fromCharCode >>> fromJust) codePoint

parseNonPrintable :: KeyboardEvent -> Either KeyValue KeyboardEvent
parseNonPrintable e = case String.toLower (KeyboardEvent.key e) of
  "alt" -> Left Alt
  "altgraph" -> Left AltGraph
  "capslock" -> Left CapsLock
  "control" -> Left Control
  "fn" -> Left Fn
  "fnlock" -> Left FnLock
  "hyper" -> Left Hyper
  "meta" -> Left Meta
  -- Windows key reported as "os" for some IE and FF
  "os" -> Left Meta
  "numlock" -> Left NumLock
  "scrolllock" -> Left ScrollLock
  -- IE9 reports Scroll instead of ScrollLock
  "scroll" -> Left ScrollLock
  "shift" -> Left Shift
  "super" -> Left Super
  "symbol" -> Left Symbol
  "symbollock" -> Left SymbolLock
  "enter" -> Left Enter
  "tab" -> Left Tab
  " " -> Left (KeyChar ' ')
  -- Older browsers may report "Spacebar" instead of " " (IE 9, 10, 11)
  "spacebar" -> Left (KeyChar ' ')
  -- IE reports "Left", "Right", "Up", and "Down"
  "arrowdown" -> Left ArrowDown
  "down" -> Left ArrowDown
  "arrowleft" -> Left ArrowLeft
  "left" -> Left ArrowLeft
  "arrowright" -> Left ArrowRight
  "right" -> Left ArrowRight
  "arrowup" -> Left ArrowUp
  "up" -> Left ArrowUp
  "end" -> Left End
  "home" -> Left Home
  "pagedown" -> Left PageDown
  "pageup" -> Left PageUp
  "backspace" -> Left Backspace
  "clear" -> Left Clear
  "copy" -> Left Copy
  "crsel" -> Left CrSel
  "cut" -> Left Cut
  "delete" -> Left Delete
  "del" -> Left Delete
  "eraseeof" -> Left EraseEof
  "exsel" -> Left ExSel
  "insert" -> Left Insert
  "paste" -> Left Paste
  "redo" -> Left Redo
  "undo" -> Left Undo
  "accept" -> Left Accept
  "again" -> Left Again
  "attn" -> Left Attn
  "cancel" -> Left Cancel
  "contextmenu" -> Left ContextMenu
  -- Very old browsers may report "Apps" instead of "ContextMenu"
  "apps" -> Left ContextMenu
  "escape" -> Left Escape
  "esc" -> Left Escape
  "execute" -> Left Execute
  "find" -> Left Find
  "finish" -> Left Finish
  "help" -> Left Help
  "pause" -> Left Pause
  "play" -> Left Play
  "props" -> Left Props
  "select" -> Left Select
  "zoomin" -> Left ZoomIn
  "zoomout" -> Left ZoomOut
  "f1" -> Left F1
  "f2" -> Left F2
  "f3" -> Left F3
  "f4" -> Left F4
  "f5" -> Left F5
  "f6" -> Left F6
  "f7" -> Left F7
  "f8" -> Left F8
  "f9" -> Left F9
  "f10" -> Left F10
  "f11" -> Left F11
  "f12" -> Left F12
  "f13" -> Left F13
  "f14" -> Left F14
  "f15" -> Left F15
  "f16" -> Left F16
  "f17" -> Left F17
  "f18" -> Left F18
  "f19" -> Left F19
  "f20" -> Left F20
  "soft1" -> Left Soft1
  "soft2" -> Left Soft2
  "soft3" -> Left Soft3
  "soft4" -> Left Soft4
  "close" -> Left Close
  "new" -> Left New
  "open" -> Left Open
  "print" -> Left Print
  "save" -> Left Save
  "spellcheck" -> Left SpellCheck
  "mailforward" -> Left MailForward
  "mailreply" -> Left MailReply
  "mailsend" -> Left MailSend
  "browserback" -> Left BrowserBack
  "browserfavorites" -> Left BrowserFavorites
  "browserforward" -> Left BrowserForward
  "browserhome" -> Left BrowserHome
  "browserrefresh" -> Left BrowserRefresh
  "browsersearch" -> Left BrowserSearch
  "browserstop" -> Left BrowserStop
  "decimal"   -> Left (KeyChar '.')
  "key11" -> Left Key11
  "key12" -> Left Key12
  "multiply"   -> Left (KeyChar '*')
  "add"   -> Left (KeyChar '+')
  "divide"   -> Left (KeyChar '/')
  "subtract"   -> Left (KeyChar '-')
  _ -> Right e


type Modifiers = { alt :: Boolean
                 , shift :: Boolean
                 , control :: Boolean
                 , meta :: Boolean
                 }

noModifiers :: Modifiers
noModifiers = { alt: false
              , shift: false
              , control: false
              , meta: false
              }

data KeyPress = KeyPress KeyValue Modifiers

derive instance eqKey :: Eq KeyPress
derive instance ordKey :: Ord KeyPress
derive instance genericKey :: Generic KeyPress _

instance showKeyPress :: Show KeyPress where
  show = genericShow

derive instance genericKeyPress :: Generic KeyPress _

instance arbitraryKeyPress :: Arbitrary KeyPress where
  arbitrary = genericArbitrary

key :: KeyValue -> KeyPress
key v = key' v noModifiers

char :: Char -> KeyPress
char c = key (KeyChar c)

key' :: KeyValue -> Modifiers -> KeyPress
key' v mod = KeyPress v mod

char' :: Char -> Modifiers -> KeyPress
char' c = key' (KeyChar c)

keyboardEventToKeyPress :: KeyboardEvent -> Maybe KeyPress
keyboardEventToKeyPress ke = keyboardEventToKeyValue ke >>= mkKey
  where
    mkKey keyValue =
      let mod = { shift: KeyboardEvent.shiftKey ke
                , meta: KeyboardEvent.metaKey ke
                , control: KeyboardEvent.ctrlKey ke
                , alt: KeyboardEvent.altKey ke
                }
      in Just (key' keyValue mod)
