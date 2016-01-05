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

(* Stoled from glMatrix by Brandon Jones and Colin MacKenzie *)
let identity arr =
  let _   = Array.(fill arr 0 (length arr) 0.) in
  let _   = arr.(0)  <- 1. in
  let _   = arr.(5)  <- 1. in
  let _   = arr.(10) <- 1. in
  let _   = arr.(15) <- 1. in
  arr
  
let create () =
  identity (Array.make 16 0.)

let perspective matrix fov aspect near far =
  let f  = 1.0 /. tan(fov /. 2.) in
  let nf = 1.0 /. (near -. far) in
  let _  = matrix.(0) <- f /. aspect in
  let _  = for i = 0 to 4 do matrix.(i) <- 0. done in
  let _  = matrix.(5) <- f in
  let _  = for i = 6 to 9 do matrix.(i) <- 0. done in
  let _ = matrix.(10) <- (far +. near) *. nf in
  let _ = matrix.(11) <- -1. in
  let _ = matrix.(12) <- 0. in
  let _ = matrix.(13) <- 0. in
  let _ = matrix.(14) <- (2. *. far *. near) *. nf in
  let _ = matrix.(15) <- 0. in
  matrix

let create_perspective fov aspect near far =
  let matrix = create () in
  perspective matrix fov aspect near far

  
