# purescript-hotkeys

Support for flexible key bindings in PureScript.

This package provides a type for mapping sequences of key presses to arbitrary commands.
The `KeyBindingsMap` type and associated functions are pure, so you'll need to bring your own 
`keydown` event pub-sub logic.

## `Hotkeys.Keys`

The `Hotkeys.Keys` module contains these types:
- `KeyValue`
- `KeyPress`

`KeyValue` is a (nearly) exhaustive enumeration representing all the keys that can be pressed.

`KeyPress` is a `KeyValue` with information about whether any modifier keys were additionally pressed.

There are also utilities for creating `KeyPress` values and converting a `KeyboardEvent` to a `KeyPress`.

## `Hotkeys.KeyMap`

The `Hotkeys.KeyMap` module provides the `KeyBindingsMap` type for associating sequences of `KeyPress` values with commands.
`KeyBindingsMap` is parameterized by the command type, so commands can be anything (typically an enum type or a string). 

`KeyBindingsMap` should do just fine for implementing the most common key binding styles, including:
- Traditional modifier key + character key bindings (e.g., Ctrl+z)
- Vim-style bindings with no modifer key (e.g., "xp" - press "x" then "p")
- Any combination of the above (e.g., Ctrl+x then "x" then "p")

### Vim-Style Modes

To support the implementation of Vim-style modal bindings, each binding is associated with a scope.

The scopes control when each binding is accessible. 
For example, you could have a "normal mode" scope and an "insert mode" scope.

You specify one or more active scopes when you invoke `lookup`, `lookupCommand`, or `getAccessibleCommands`.

### Scope Hierarchies - Inheriting and overriding commands

By passing multiple scopes to one of the `lookup*` functions in a consistent order, you can 
achieve the effect of having hierarchical scopes.

When you specify multiple active scopes, all of those scopes will be searched.
If the same key sequence is bound in more than one scope, the binding in the scope that appears 
later in the array will override the bindings in earlier scopes.

#### Example

`lookupCommand ["parent-scope", "child-scope"] <SOME KEY SEQUENCE> keyMap`
- "child-scope" effectively inherits all the bindings defined in "parent-scope".
- For key sequences bound in both "child-scope" and "parent-scope", the "child-scope" binding overrides the "parent-scope" binding.
