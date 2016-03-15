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
  canvas  : Dom_html.canvasElement Js.t
; ctx : Dom_html.canvasRenderingContext2D Js.t
; time : float
}

let singleton canvas ctx =
  let t = jsnew Js.date_now () in
  {
    canvas  = canvas
  ; ctx     = ctx
  ; time    = t ## getTime()
  }

let next_state state =
  let t = jsnew Js.date_now () in
  { state with time = t ## getTime() }

let fill_canvas clr state =
  let w  = float_of_int (state.canvas ## width) in
  let h  = float_of_int (state.canvas ## height) in
  let () = state.ctx ## fillStyle <- (DrumColor.js clr) in
 state. ctx ## fillRect(0., 0., w, h)

let rec update state =
  Dom_html.window ## requestAnimationFrame(
    Js.wrap_callback (fun t ->
        let _ = if DrumKeyboard.(press Key.space) then
            fill_canvas (DrumColor.random ()) state
        in
        update (next_state state)
      )
  ) |> ignore


let initialize_keyboard state =
  let open Lwt_js_events in
  let _ =
    async_loop keydown Dom_html.document
      (fun e _ ->
         let _ = DrumKeyboard.keydown e
         in Lwt.return_unit
      )
  in
  let _ =
    async_loop keyup Dom_html.document
      (fun e _ ->
         let _ = DrumKeyboard.keyup e
         in Lwt.return_unit
      )
  in ()


let create ?(bgcolor = DrumColor.black) width height receiver =
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
  let st  = singleton canvas ctx in
  let _   = fill_canvas bgcolor st in
  st

let run f state =
    let () = initialize_keyboard state in
    let _ = f state in
    update state
