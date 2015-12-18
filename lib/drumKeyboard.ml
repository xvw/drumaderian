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

type keyboard_state = { mutable press : int array }
let singleton_keyboard = { press = Array.make 256 0 }

let keydown event =
  let kc = event ## keyCode in
  let state = singleton_keyboard.press.(kc) in
  singleton_keyboard.press.(kc) <- state + 1

let keyup event =
  let kc = event ## keyCode in
  singleton_keyboard.press.(kc) <- 0

let value kc = singleton_keyboard.press.(kc)

let press kc = (value kc) > 0
let trigger kc = (value kc) = 1
let repeat kc = trigger kc || (value kc) >= 24 && ((value kc) mod 6) = 0
