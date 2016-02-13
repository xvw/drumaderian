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

type t = {
  red : int
; green : int
; blue : int
; alpha : int
}

let bound v =
  if v > 255 then 255
  else if v < 0 then 0
  else v

let make ?(alpha = 0) r g b = {
  red = bound r
; green = bound g
; blue = bound b
; alpha = alpha
}


let of_rgb_string str =
  let s = Regexp.(global_replace (regexp " ") "" str) in 
  try  Scanf.sscanf s "rgb(%d,%d,%d)" (fun a b c -> make a b c)
  with _ ->
    try Scanf.sscanf s "rgba(%d,%d,%d,%d)"
          (fun a b c d -> make ~alpha:d a b c)
    with _ -> make 255 255 255

let of_hexa_string s =
  Scanf.sscanf s "#%2x%2x%2x" (fun a b c -> make a b c)

let of_string s =
  try of_hexa_string s
  with _ ->
  try of_rgb_string s
  with _ -> make 255 255 255


let to_string c =
  Printf.sprintf "rgb(%d, %d, %d)"
    c.red
    c.green
    c.blue

let js c = c |> to_string |> String.js

let red = make 255 0 0
let green = make 0 255 0
let blue = make 0 0 255
let white = make 255 255 255
let black = make 0 0 0
