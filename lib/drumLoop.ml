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

include Lwt_js_events
open DrumPervasives

let rec delayed ?(delay=0.1) f =
  let _ = f () in
  Lwt_js.sleep delay >>= (fun _ -> delayed ~delay f)

let forever = delayed

let until ?(delay=0.1) pred f =
  let cpt = ref 0 in
  let _ = while (pred !cpt) do
      let _ = Lwt_js.sleep delay
        >>= (fun _ -> Lwt.return (f ()))
      in cpt := !cpt + 1
    done
  in Lwt.return_unit

let forN ?(delay=0.1) n = until ~delay (fun i -> i < n)

let ev_window_wrap ev f =
  async_loop
    ev
    Dom_html.window
    ( fun e _ -> let _ = f e in Lwt.return_unit )
    
let ev_canvas_wrap ev f =
  DrumCanvas.perform (
    fun canvas ->
      async_loop ev canvas ( fun e _ ->
          let _ = f e in Lwt.return_unit
        )
  )

(* Loop for mouse position *)
let initialize_mouse () =
  let _ =
    ev_canvas_wrap
      mousemove
      DrumMouse.retreive_position
  in
  let _ =
    ev_window_wrap
      mousedown
      DrumMouse.mousedown
  in
  let _ =
    ev_window_wrap
      mouseup
      DrumMouse.mouseup
  in ()

(* Loop for keyboard state *)
let initialize_keyboard () =
  let _ =
    ev_window_wrap
      keydown
      DrumKeyboard.Internal.keydown
  in
  let _ =
    ev_window_wrap
      keyup
      DrumKeyboard.Internal.keyup
  in ()

let initialize () =
  let _ = initialize_mouse () in
  let _ = initialize_keyboard () in
  ()
