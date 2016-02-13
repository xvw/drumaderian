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

type mouse_state = {
  mutable x : int
; mutable y : int
; press : int array
; click : bool
}
let singleton_mouse = {
  x = 0 ; y = 0; press = Array.make 3 0; click = false
}



(* To be modified with a new Canvas handler

let mouse_position event =
  let rect = DrumCanvas.boundedRect () in
  let clientX = event ## clientX in
  let clientY = event ## clientY in
  let top = int_of_float (rect ## top) in
  let left = int_of_float (rect ## left) in
  (clientX - left, clientY - top)

let retreive_position event =
  let (x, y) = mouse_position event in
  let _ = singleton_mouse.x <- x in
  let _ = singleton_mouse.y <- y in
  Js._true

let keycode event =
  let open Dom_html in
  let w = (event ## which) in
  let ev = Js.Optdef.get w (fun () -> No_button) in
  match ev with
  | No_button -> 0
  | Left_button -> 1
  | Right_button -> 3
  | Middle_button -> 2

let mousedown event =
  let kc = keycode event in
  let state = singleton_mouse.press.(kc) in
  singleton_mouse.press.(kc) <- state + 1

let mouseup event =
  let kc = keycode event in
  singleton_mouse.press.(kc) <- 0

let value kc = singleton_mouse.press.(kc)

let press kc = (value kc) > 0
let trigger kc = (value kc) = 1
let click = trigger
let repeat kc =
  trigger kc || (value kc) >= 24 && ((value kc) mod 6) = 0

let x () = singleton_mouse.x
let y () = singleton_mouse.y

let left = 1
let center = 2
let right = 3


*)
