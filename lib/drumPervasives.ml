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

module Html =
struct

  let fail i () = raise (DrumExceptions.Unbound_id i)
  let unopt i x = Js.Opt.get x (fail i) 
  let getById id =
    Dom_html.document ## getElementById (js_string id)
    |> unopt id
  

end
