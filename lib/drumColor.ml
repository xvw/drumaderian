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

type 'a color = {
  red   : 'a
; green : 'a
; blue  : 'a
; alpha : 'a
}

type t = int color
type gl = float color

let bound v =
  if v > 255 then 255
  else if v < 0 then 0
  else v

let itof v =
  (float_of_int v) /. 255.0

let make ?(alpha = 255) r g b = {
  red = itof (bound r)
; green = itof (bound g)
; blue = itof (bound b)
; alpha = itof (bound alpha)
}

let rgb r g b = make r g b 
let rgba r g b a = make ~alpha:a r g b
let rgbx r g b = make (r*17) (g*17) (b*17)

let ofrgb str =
  let open Scanf in
  try sscanf str "rgb(%d,%d,%d)" rgb with _ ->
    sscanf str "rgba(%d,%d,%d,%d)"  rgba

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
    then "a", ","^(string_of_int ((int_of_float color.alpha) * 255))
    else "", ""
  in Printf.sprintf "rgb%s(%d,%d,%d%s)"
    a
    ((int_of_float color.red)*255)
    ((int_of_float color.green)*255)
    ((int_of_float color.blue)*255)
    tl

let js c =
  to_string c
  |> js_string


let red   = make 255 0 0
let green = make 0 255 0
let blue  = make 0 0 255
let white = make 255 255 255
let black = make 0 0 0

let join c =
  Printf.sprintf
    "%g,%g,%g,%g"
    c.red
    c.green
    c.blue
    c.alpha
    
    
