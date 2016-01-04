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
let context = ref None

(* Perform an operation on a canvas *)
let perform f =
  match !canvas with
  | None -> raise DrumExceptions.Canvas_not_created
  | Some x -> f x

let perform3d f =
  match (!canvas, !context) with
  | None, _ | _, None -> raise DrumExceptions.Canvas_not_ready
  | Some cn, Some ctx -> f cn ctx


(* Retreive the WebGL's context *)
let retreive_ctx () = perform
    (fun canvas ->
       Js.Opt.get (
         try WebGL.getContext canvas
         with _ -> Js.null
       ) (fun () -> raise DrumExceptions.WebGL_not_allowed)
    )

let webgl_initialize rcolor =
  perform3d (fun canvas ctx ->
      let open DrumColor in
      let color = match rcolor with
        | Some x -> x
        | None -> black
      in 
      let _ = ctx ## clearColor(
          color.red,
          color.green,
          color.blue,
          color.alpha
        ) in
      let _ = ctx ## enable(ctx ## _DEPTH_TEST_) in
      let _ = ctx ## depthFunc(ctx ## _LESS) in
      let _ = ctx ## clear(
          ctx##_DEPTH_BUFFER_BIT_ lor ctx##_COLOR_BUFFER_BIT_) in
      ()
    )


(* Create a canvas *)
let create width height rcolor =
  match !canvas with
  | Some _ -> raise DrumExceptions.Canvas_already_created
  | None ->
    let c = Dom_html.(createCanvas document) in
    let _ = c ## width <- width in
    let _ = c ## height <- height in 
    let _ = canvas  := Some c in 
    let _ = context := Some (retreive_ctx ()) in
    webgl_initialize rcolor

(* Append canvas to an element *)
let appendTo elt = perform (fun canvas -> Dom.appendChild elt canvas)
let createIn elt w h rcolor = let _ = create w h rcolor in appendTo elt

let boundedRect () =
  perform (fun canvas -> canvas ## getBoundingClientRect ())
