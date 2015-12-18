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


(* Loop for mouse position *)
let initialize_mouse () =
  DrumCanvas.perform (fun canvas -> 
      async_loop
        mousemove
        canvas
        (fun evt _ ->
           let _ = DrumMouse.retreive_position evt in
           Lwt.return_unit
        )
    )

(* Loop for keyboard state *)
let initialize_keyboard () =
  let _ = DrumCanvas.perform (fun canvas ->
      async_loop
        keydown
        Dom_html.window
        (fun evt _ ->
           let _ = DrumKeyboard.keydown evt in
           Lwt.return_unit
        )
    ) in
  DrumCanvas.perform (fun canvas ->
      async_loop
        keyup
        Dom_html.window
        (fun evt _ ->
           let _ = DrumKeyboard.keyup evt in
           Lwt.return_unit
        )
    )

let initialize () =
  let _ = initialize_mouse () in
  let _ = initialize_keyboard () in
  ()
