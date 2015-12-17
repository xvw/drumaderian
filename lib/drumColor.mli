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

(** This module provide a simple color interface using a type *)

(** {2 Internal representation of a color} *)
type t = {
  red   : int
; green : int
; blue  : int
; alpha : float
}

val make : ?alpha:float -> int -> int -> int -> t
(** [Color.make ~alpha:a r g b] create a color *)

val of_string : string -> t
(** [Color.of_string s] unsafe coersion *)

val js : t -> Js.js_string Js.t
(** Create a color usable by a JavaScript function *)
