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

module Point =
struct

  type 'a pt = {
    x : 'a
  ; y : 'a
  ; coers : 'a pt -> float pt
  }

  type t =
    | F of float pt
    | I of int pt

  let float x y =
    F {
      x = x
    ; y = y
    ; coers = id
    }

  let int x y =
    I {
      x = x
    ; y = y
    ; coers = fun r ->
        {
          x = (float_of_int r.x)
        ; y = (float_of_int r.y)
        ; coers = id
        }
    }

  let real = function
    | F p -> p
    | I p -> p.coers p

end

module Dimension =
struct

  type 'a dim = {
    width : 'a
  ; height : 'a
  ; coers : 'a dim -> float dim
  }

  type t =
    | F of float dim
    | I of int dim

  let float width height =
    F {
      width = width
    ; height = height
    ; coers = id
    }

  let int width height =
    I {
      width = width
    ; height = height
    ; coers = fun r ->
        {
          width = (float_of_int r.width)
        ; height = (float_of_int r.height)
        ; coers = id
        }
    }

  let real = function
    | F p -> p
    | I p -> p.coers p 

end
