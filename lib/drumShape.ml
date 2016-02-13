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

  type 'a t = {
    x : 'a
  ; y : 'a
  ; coers : 'a t -> float t
  }

  type coord =
    | F of float t
    | I of int t

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



