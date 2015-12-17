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

type t = {
  red   : int
; green : int
; blue  : int
; alpha : float
}

let bound v =
  if v > 255 then 255
  else if v < 0 then 0
  else v

let make ?(alpha = 0.) r g b = {
  red = bound r
; green = g
; blue = b
; alpha = alpha
}

let rgb r g b = make r g b 
let rgba r g b a = make ~alpha:a r g b
let rgbx r g b = make (r*17) (g*17) (b*17)

let ofrgb str =
  let open Scanf in
  try sscanf str "rgb(%d,%d,%d)" rgb with _ ->
    sscanf str "rgba(%d,%d,%d,%g)"  rgba

let ofxa str =
  let open Scanf in
  try sscanf str "#%x%x%x" rgbx with _ ->
  try sscanf str "#%2x%2x%2x" rgb  with _ ->
  try sscanf str "%x%x%x" rgbx with _ ->
    sscanf str "%2x%2x%2x" rgb

let of_string str =
  let s = Regexp.(global_replace (regexp " ") "" str) in
  try ofxa s with _ ->
  try ofrgb s with _ ->
    make 255 255 255
  
let to_string color =
  let a, tl =
    if color.alpha <> 0.
    then "a", ","^(string_of_float color.alpha)
    else "", ""
  in Printf.sprintf "rgb%s(%d,%d,%d%s)"
    a
    color.red
    color.green
    color.blue
    tl

let js c =
  to_string c
  |> js_string
