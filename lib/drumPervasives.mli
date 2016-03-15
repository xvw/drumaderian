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

val ( >>= ) : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t

val id : 'a -> 'a

val ( $ ) : ('a -> 'b) -> 'a -> 'b

val ( % ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b

val ( %> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c

val alert : string -> unit

val log : 'a Js.t -> unit

val document : Dom_html.document Js.t

val tf : int -> float
val ti : float -> int

module String :
sig

  include (module type of String)
  val caml : Js.js_string Js.t -> string
  val js : string -> Js.js_string Js.t
  val i_js : int -> Js.js_string Js.t

end

module Error :
sig

  exception RuntimeError of string
  val fail : string -> unit
  val try_with : (unit -> 'a) -> string -> 'a

end
