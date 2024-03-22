module Electron.Helpers

open Fable.Core
open Fable.Core.JsInterop


[<StringEnum; RequireQualifiedAccess>]
type Modifier =
  /// macOS only. Use CmdOrCtrl instead.
  | [<CompiledName("Command")>] Command
  /// Alias for Command
  | [<CompiledName("Cmd")>] Cmd
  | [<CompiledName("Control")>] Control
  /// Alias for Control
  | [<CompiledName("Ctrl")>] Ctrl
  | [<CompiledName("CommandOrControl")>] CommandOrControl
  /// Alias for CommandOrControl
  | [<CompiledName("CmdOrCtrl")>] CmdOrCtrl
  | [<CompiledName("Alt")>] Alt
  /// macOS only. Use Alt instead.
  | [<CompiledName("Option")>] Option
  | [<CompiledName("AltGr")>] AltGr
  | [<CompiledName("Shift")>] Shift
  /// Mapped to the Windows key on Windows and Linux and the Cmd key on macOS.
  | [<CompiledName("Super")>] Super


[<StringEnum; RequireQualifiedAccess>]
type Key =
  /// The number 0 (not NumPad).
  | [<CompiledName("0")>] N0
  /// The number 1.
  | [<CompiledName("1")>] N1
  /// The number 2.
  | [<CompiledName("2")>] N2
  /// The number 3.
  | [<CompiledName("3")>] N3
  /// The number 4.
  | [<CompiledName("4")>] N4
  /// The number 5.
  | [<CompiledName("5")>] N5
  /// The number 6.
  | [<CompiledName("6")>] N6
  /// The number 7.
  | [<CompiledName("7")>] N7
  /// The number 8.
  | [<CompiledName("8")>] N8
  /// The number 9.
  | [<CompiledName("9")>] N9
  /// The letter A.
  | [<CompiledName("A")>] A
  /// The letter B.
  | [<CompiledName("B")>] B
  /// The letter C.
  | [<CompiledName("C")>] C
  /// The letter D.
  | [<CompiledName("D")>] D
  /// The letter E.
  | [<CompiledName("E")>] E
  /// The letter F.
  | [<CompiledName("F")>] F
  /// The letter G.
  | [<CompiledName("G")>] G
  /// The letter H.
  | [<CompiledName("H")>] H
  /// The letter I.
  | [<CompiledName("I")>] I
  /// The letter J.
  | [<CompiledName("J")>] J
  /// The letter K.
  | [<CompiledName("K")>] K
  /// The letter L.
  | [<CompiledName("L")>] L
  /// The letter M.
  | [<CompiledName("M")>] M
  /// The letter N.
  | [<CompiledName("N")>] N
  /// The letter O.
  | [<CompiledName("O")>] O
  /// The letter P.
  | [<CompiledName("P")>] P
  /// The letter Q.
  | [<CompiledName("Q")>] Q
  /// The letter R.
  | [<CompiledName("R")>] R
  /// The letter S.
  | [<CompiledName("S")>] S
  /// The letter T.
  | [<CompiledName("T")>] T
  /// The letter U.
  | [<CompiledName("U")>] U
  /// The letter V.
  | [<CompiledName("V")>] V
  /// The letter W.
  | [<CompiledName("W")>] W
  /// The letter X.
  | [<CompiledName("X")>] X
  /// The letter Y.
  | [<CompiledName("Y")>] Y
  /// The letter Z.
  | [<CompiledName("Z")>] Z
  /// Function key 1.
  | [<CompiledName("F1")>] F1
  /// Function key 2.
  | [<CompiledName("F2")>] F2
  /// Function key 3.
  | [<CompiledName("F3")>] F3
  /// Function key 4.
  | [<CompiledName("F4")>] F4
  /// Function key 5.
  | [<CompiledName("F5")>] F5
  /// Function key 6.
  | [<CompiledName("F6")>] F6
  /// Function key 7.
  | [<CompiledName("F7")>] F7
  /// Function key 8.
  | [<CompiledName("F8")>] F8
  /// Function key 9.
  | [<CompiledName("F9")>] F9
  /// Function key 10.
  | [<CompiledName("F10")>] F10
  /// Function key 11.
  | [<CompiledName("F11")>] F11
  /// Function key 12.
  | [<CompiledName("F12")>] F12
  /// Function key 13.
  | [<CompiledName("F13")>] F13
  /// Function key 14.
  | [<CompiledName("F14")>] F14
  /// Function key 15.
  | [<CompiledName("F15")>] F15
  /// Function key 16.
  | [<CompiledName("F16")>] F16
  /// Function key 17.
  | [<CompiledName("F17")>] F17
  /// Function key 18.
  | [<CompiledName("F18")>] F18
  /// Function key 19.
  | [<CompiledName("F19")>] F19
  /// Function key 20.
  | [<CompiledName("F20")>] F20
  /// Function key 21.
  | [<CompiledName("F21")>] F21
  /// Function key 22.
  | [<CompiledName("F22")>] F22
  /// Function key 23.
  | [<CompiledName("F23")>] F23
  /// Function key 24.
  | [<CompiledName("F24")>] F24
  /// )
  | [<CompiledName(")")>] RParen
  /// (
  | [<CompiledName("(")>] LParen
  /// !
  | [<CompiledName("!")>] Exclamation
  /// ?
  | [<CompiledName("?")>] Question
  /// @
  | [<CompiledName("@")>] At
  /// #
  | [<CompiledName("#")>] Hash
  /// $
  | [<CompiledName("$")>] Dollar
  /// %
  | [<CompiledName("%")>] Percent
  /// ^
  | [<CompiledName("^")>] Caret
  /// &
  | [<CompiledName("&")>] Ampersand
  /// *
  | [<CompiledName("*")>] Asterisk
  /// :
  | [<CompiledName(":")>] Colon
  /// ;
  | [<CompiledName(";")>] Semicolon
  /// =
  | [<CompiledName("=")>] Equals
  /// <
  | [<CompiledName("<")>] LessThan
  /// >
  | [<CompiledName(">")>] GreaterThan
  /// ,
  | [<CompiledName(",")>] Comma
  /// _
  | [<CompiledName("_")>] Underscore
  /// -
  | [<CompiledName("-")>] Dash
  /// Alias for Dash
  | [<CompiledName("-")>] Hyphen
  /// .
  | [<CompiledName(".")>] Dot
  /// /
  | [<CompiledName(".")>] ForwardSlash
  /// \
  | [<CompiledName("\\")>] Backslash
  /// ~
  | [<CompiledName("~")>] Tilde
  /// `
  | [<CompiledName("`")>] Backtick
  /// {
  | [<CompiledName("{")>] LBrace
  /// }
  | [<CompiledName("}")>] RBrace
  /// [
  | [<CompiledName("[")>] LBracket
  /// ]
  | [<CompiledName("]")>] RBracket
  /// |
  | [<CompiledName("|")>] Pipe
  /// '
  | [<CompiledName("'")>] SingleQuote
  /// Alias for SingleQuote
  | [<CompiledName("'")>] Apostrophe
  /// "
  | [<CompiledName("\"")>] DoubleQuote
  | [<CompiledName("Plus")>] Plus
  | [<CompiledName("Space")>] Space
  | [<CompiledName("Tab")>] Tab
  | [<CompiledName("Capslock")>] Capslock
  | [<CompiledName("Numlock")>] Numlock
  | [<CompiledName("Scrolllock")>] ScrollLock
  | [<CompiledName("Backspace")>] Backspace
  | [<CompiledName("Delete")>] Delete
  | [<CompiledName("Insert")>] Insert
  | [<CompiledName("Return")>] Return
  /// Alias for Return
  | [<CompiledName("Enter")>] Enter
  | [<CompiledName("Up")>] Up
  | [<CompiledName("Down")>] Down
  | [<CompiledName("Left")>] Left
  | [<CompiledName("Right")>] Right
  | [<CompiledName("Home")>] Home
  | [<CompiledName("End")>] End
  | [<CompiledName("PageUp")>] PageUp
  | [<CompiledName("PageDown")>] PageDown
  | [<CompiledName("Escape")>] Escape
  /// Alias for Escape
  | [<CompiledName("Esc")>] Esc
  | [<CompiledName("VolumeUp")>] VolumeUp
  | [<CompiledName("VolumeDown")>] VolumeDown
  | [<CompiledName("VolumeMute")>] VolumeMute
  | [<CompiledName("MediaNextTrack")>] MediaNextTrack
  | [<CompiledName("MediaPreviousTrack")>] MediaPreviousTrack
  | [<CompiledName("MediaStop")>] MediaStop
  | [<CompiledName("MediaPlayPause")>] MediaPlayPause
  | [<CompiledName("PrintScreen")>] PrintScreen
  /// Numpad 0
  | [<CompiledName("num0")>] Num0
  /// Numpad 1
  | [<CompiledName("num1")>] Num1
  /// Numpad 2
  | [<CompiledName("num2")>] Num2
  /// Numpad 3
  | [<CompiledName("num3")>] Num3
  /// Numpad 4
  | [<CompiledName("num4")>] Num4
  /// Numpad 5
  | [<CompiledName("num5")>] Num5
  /// Numpad 6
  | [<CompiledName("num6")>] Num6
  /// Numpad 7
  | [<CompiledName("num7")>] Num7
  /// Numpad 8
  | [<CompiledName("num8")>] Num8
  /// Numpad 9
  | [<CompiledName("num9")>] Num9
  /// Numpad decimal
  | [<CompiledName("numdec")>] NumDec
  /// Numpad +
  | [<CompiledName("numadd")>] NumAdd
  /// Numpad -
  | [<CompiledName("numsub")>] NumSub
  /// Numpad *
  | [<CompiledName("nummult")>] NumMult
  /// Numpad /
  | [<CompiledName("numdiv")>] NumDiv


/// Returns an accelerator string that can be used to register shortcuts.
let createAccelerator (modifiers: Modifier list) (key: Key) =
  modifiers, !!key ||> List.foldBack (fun (m: Modifier) acc -> !!m + "+" + acc)
