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


open DrumPervasives

type point = float * float
             
let shape arr =
  let len = Array.length arr in
  let new_array = Array.make (2*len) 0.0 in
  let i, j = ref 0, ref 0 in
  let _ =
    while !i < len do
        let (x, y) = arr.(!i) in
        let _= new_array.(!j) <- x in
        let _= new_array.(!j+1) <- y in
        let _ = incr i in
        j := !j + 2
    done
  in float32array new_array
    
