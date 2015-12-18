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
   
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
*)

open DrumPervasives

type mouse_coords = {
  mutable x : int
; mutable y : int
}
let singleton_mouse = { x = 0 ; y = 0}

let mouse_position event =
  let rect = DrumCanvas.boundedRect () in
  let clientX = event ## clientX in
  let clientY = event ## clientY in
  let top = int_of_float (rect ## top) in
  let left = int_of_float (rect ## left) in
  (clientX - left, clientY - left)

let retreive_position event =
  let (x, y) = mouse_position event in
  let _ = singleton_mouse.x <- x in
  let _ = singleton_mouse.y <- y in
  Js._true
    
let x () = singleton_mouse.x
let y () = singleton_mouse.y

  
