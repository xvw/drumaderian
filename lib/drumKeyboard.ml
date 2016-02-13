(*
 * Drumaderian
 *
 * Copyright (C) 2015  Xavier Van de Woestyne <xaviervdw@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
*)

open DrumPervasives

module Internal =
struct

  type keyboard_state = { press : int array }
  let singleton_keyboard = { press = Array.make 256 0 }

  let keydown event =
    let kc = event ## keyCode in
    let state = singleton_keyboard.press.(kc) in
    singleton_keyboard.press.(kc) <- state + 1

  let keyup event =
    let kc = event ## keyCode in
    singleton_keyboard.press.(kc) <- 0

  let value kc = singleton_keyboard.press.(kc)

end

let press kc = (Internal.value kc) > 0
let trigger kc = (Internal.value kc) = 1
let repeat kc =
  trigger kc ||
  (Internal.value kc) >= 24
  && ((Internal.value kc) mod 6) = 0

module Key =
struct

  let backspace   = 8
  let tab         = 9
  let enter       = 13
  let space       = 32
  let shift       = 16
  let ctrl        = 17
  let alt         = 18
  let pause       = 19
  let break       = 19
  let caps_lock   = 20
  let escape      = 27
  let page_up     = 33
  let page_down   = 34
  let end'        = 35
  let home        = 36
  let left        = 37
  let up          = 38
  let right       = 39
  let down        = 40
  let insert      = 45
  let delete      = 46
  let zero        = 48
  let one         = 49
  let two         = 50
  let three       = 51
  let four        = 52
  let five        = 53
  let six         = 54
  let seven       = 55
  let eight       = 56
  let nine        = 57
  let a           = int_of_char 'A'
  let b           = int_of_char 'B'
  let c           = int_of_char 'C'
  let d           = int_of_char 'D'
  let e           = int_of_char 'E'
  let f           = int_of_char 'F'
  let g           = int_of_char 'G'
  let h           = int_of_char 'H'
  let i           = int_of_char 'I'
  let j           = int_of_char 'J'
  let k           = int_of_char 'K'
  let l           = int_of_char 'L'
  let m           = int_of_char 'M'
  let n           = int_of_char 'N'
  let o           = int_of_char 'O'
  let p           = int_of_char 'P'
  let q           = int_of_char 'Q'
  let r           = int_of_char 'R'
  let s           = int_of_char 'S'
  let t           = int_of_char 'T'
  let u           = int_of_char 'U'
  let v           = int_of_char 'V'
  let w           = int_of_char 'W'
  let x           = int_of_char 'X'
  let y           = int_of_char 'Y'
  let z           = int_of_char 'Z'
  let lwindow     = 91
  let rwindow     = 92
  let num_zero    = 96
  let num_one     = 97
  let num_two     = 98
  let num_three   = 99
  let num_four    = 100
  let num_five    = 101
  let num_six     = 102
  let num_seven   = 103
  let num_eight   = 104
  let num_nine    = 105
  let multiply    = 106
  let add         = 107
  let substract   = 109
  let decimal     = 110
  let divide      = 111
  let f1          = 112
  let f2          = 113
  let f3          = 114
  let f4          = 115
  let f5          = 116
  let f6          = 117
  let f7          = 118
  let f8          = 119
  let f9          = 120
  let f10         = 121
  let f11         = 122
  let f12         = 123
  let num_lock    = 144
  let scroll_lock = 145
  let semi_colon  = 186
  let equal       = 187
  let comma       = 188
  let dash        = 189
  let period      = 190
  let forwardslash= 191
  let grave_accent= 192
  let open_bracket= 219
  let backslash   = 220

  let close_bracket = 221
  let single_quote  = 222

end

include Key
