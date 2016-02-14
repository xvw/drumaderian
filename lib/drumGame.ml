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

type state = {
  mutable canvas  : Dom_html.canvasElement Js.t option
; mutable context : Dom_html.canvasRenderingContext2D Js.t option
}

let singleton = {
  canvas  = None
; context = None
}

let canvas  () = singleton.canvas
let context () = singleton.context

let fill_canvas clr canvas ctx =
  let w  = float_of_int (canvas ## width) in
  let h  = float_of_int (canvas ## height) in
  let () = ctx ## fillStyle <- (DrumColor.js clr) in
  ctx ## fillRect(0., 0., w, h)

let create ?(bgcolor = DrumColor.black) width height receiver =
  match singleton.canvas with
  | Some _ -> Error.fail "Canvas already created"
  | None ->
    let elt =
      Error.try_with
        (fun () -> Dom_html.getElementById receiver)
        ("Unable to find #" ^ receiver)
    in
    let canvas = Dom_html.(createCanvas document) in
    let ()  = canvas ## width <- width in
    let ()  = canvas ## height <- height in
    let ctx = canvas ## getContext(Dom_html._2d_) in
    let ()  = Dom.appendChild elt canvas in
    let ()  = singleton.canvas <- Some canvas in
    let ()  = singleton.context <- Some ctx in
    fill_canvas bgcolor canvas ctx
