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

module Point :
sig

  type t

  val float : float -> float -> t
  val int : int -> int -> t
  val real : t -> t
  val zero : t
  val x : t -> float
  val y : t -> float

end

module Dimension :
sig

  type t

  val float : float -> float -> t
  val int : int -> int -> t
  val real : t -> t
  val width : t -> float
  val height : t -> float

end

module Rect :
sig

  type t

  val mk : ?origin:Point.t -> Point.t -> Dimension.t -> t
  val mk_area : ?origin:Point.t -> Point.t -> Point.t -> t
  val x : t -> float
  val y : t -> float
  val width : t -> float
  val height : t -> float
  val center : t -> t
  val area : t -> float
  val origin : t -> Point.t
  val set_origin : t -> Point.t -> t

end


