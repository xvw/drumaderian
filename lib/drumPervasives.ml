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

type image = Dom_html.imageElement Js.t

let ( >>= ) = Lwt.bind
let id x = x
let ( $ ) f x = f x
let flip f x y = f y x
let ( % ) f g x = f (g x)
let ( %> ) f g x = g (f x)

let tf = float_of_int
let ti = int_of_float

let document = Dom_html.document

module String =
struct

  include String
  let caml   = Js.to_string
  let js     = Js.string

  let i_js x =
    string_of_int x
    |> js


end


let alert str = Dom_html.window ## alert (String.js str)
let log v = Firebug.console ## log (v)

module Error =
struct

  exception RuntimeError of string

  let fail message =
    let () = alert message in
    let () = log (String.js message) in
    raise (RuntimeError message)

  let try_with f message = try f () with _ -> fail message

end
