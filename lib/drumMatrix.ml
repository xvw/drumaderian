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

type t = float array
type js = (float, [ `Float32 ]) Typed_array.typedArray

let scale x y z =
  [|
    x;  0.; 0.; 0.;
    0.; y ; 0.; 0.;
    0.; 0.; z ; 0.;
    0.; 0.; 0.; 1.;
  |] |> float32array

let translate x y z =
  [|
    1.; 0.; 0.; 0.;
    0.; 1.; 0.; 0.;
    0.; 0.; 1.; 0.;
    x ; y ; z ; 1.;
  |] |> float32array

 let rotate_x t =
   [|
     1.; 0.; 0.; 0.;
     0.; cos t; sin t; 0.;
     0.; -.sin t; cos t; 0.;
     0.; 0.; 0.; 1.;
   |] |> float32array
   
let rotate_y t =
  [|
    cos t; 0.; -.sin t; 0.;
    0.; 1.; 0.; 0.;
    sin t; 0.; cos t; 0.;
    0.; 0.; 0.; 1.;
  |] |> float32array
  

let mult a b =
  let sub x y = x * 4 + y
  and trs x = x/4, x mod 4 in 
  let aux x =
    let i, j = trs x in
    a.(sub i 0) *. b.(sub 0 j)
    +. a.(sub i 1) *. b.(sub 1 j)
    +. a.(sub i 2) *. b.(sub 2 j)
    +. a.(sub i 3) *. b.(sub 3 j)
  in
  Array.init 16 aux
  |> float32array 
