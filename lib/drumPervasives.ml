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

let ( >>= ) = Lwt.bind
let caml_string = Js.to_string
let js_string = Js.string
let id x = x
let mk_unit f x = (fun () -> f x)

let alert v = Dom_html.window ## alert (js_string v)
let log r =
  Firebug.console ## log (r)
  |> ignore

let document = Dom_html.document
let window = Dom_html.window
let fail i () = raise (DrumExceptions.Unbound_id i)
let unopt i x = Js.Opt.get x (fail i) 
let perform_fail exn () = raise exn
let js_true expr = expr = Js._true

let float32array a =
  let len = Array.length a in 
  let jsarray = jsnew Typed_array.float32Array(len) in
  let _ = Array.iteri (fun i v -> Typed_array.set jsarray i v) a in
  jsarray

let int16array a =
  let len = Array.length a in 
  let jsarray = jsnew Typed_array.int16Array(len) in
  let _ = Array.iteri (fun i v -> Typed_array.set jsarray i v) a in
  jsarray  

module Html =
struct

  let getById id =
    Dom_html.document ## getElementById (js_string id)
    |> unopt id

  let getById_opt id =
    try Some (getById id)
    with _ -> None 
  

end
