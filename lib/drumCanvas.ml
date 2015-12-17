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

(* Store the current canvas *)
let canvas = ref None

(* Perform an operation on a canvas *)
let perform f =
  match !canvas with
  | None -> raise DrumExceptions.Canvas_not_created
  | Some x -> f x

(* Create a canvas *)
let create width height =
  match !canvas with
  | Some _ -> raise DrumExceptions.Canvas_already_created
  | None ->
    let c = Dom_html.(createCanvas document) in
    let _ = c ## width <- width in
    let _ = c ## height <- height
    in canvas := Some c

(* Append canvas to an element *)
let appendTo elt = perform (fun canvas -> Dom.appendChild elt canvas)
let createIn elt w h = let _ = create w h in appendTo elt
