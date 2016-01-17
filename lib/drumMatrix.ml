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

type t = Typed_array.float32Array Js.t


let length (a : t) = a ## length
let get = Typed_array.get
let set = Typed_array.set
let (@->) a i = (fun v -> set a i v)

let get_with a default_value i =
  Js.Optdef.get (get a i) (fun () -> default_value)
let get_safe a = get_with a 0.
let (<@>) mat i  = get_safe mat i 

let of_array = float32array
let to_array a = Array.init (a ## length )(get_with a 0. )

let new_obj len = jsnew Typed_array.float32Array(len)
    
let make len value =
  let arr = new_obj len in
  let () =  for i = 0 to (len - 1) do set arr i value done
  in arr

let init len func =
  let arr = new_obj len in
  let () =  for i = 0 to (len - 1) do set arr i (func i) done
  in arr

let copy a : t =  init (length a) (get_with a 0.)

module Effect =
struct

  let iteri (f:int -> 'a ->unit) mat =
    let len = length mat in
    for i = 0 to (len -1) do f i (get_safe mat i) done

  let iter f =  iteri (fun _ x ->  f x)
  let replace mat v = iteri (fun i _ -> set mat i v ) mat
  let replace_for mat elts i = List.iter (fun x -> set mat x i ) elts
  let mapi f mat = iteri (fun i x -> set mat i (f i x)) mat 
  let map f = mapi (fun _ x -> f x)
    
end


module Mat4 =
struct

  let identity (mat) =
    let () = Effect.replace mat 0. in 
    let () = Effect.replace_for mat [0; 5; 10; 15] 1.
    in mat

  let create () =
    let mat = make 16 0. in
    let _ = Effect.replace_for mat [0; 5; 10; 15] 1.
    in mat

  let perspective ~fov ~aspect ~near ~far mat =
    let f = 1. /. tan (fov /. 2.) in
    let nf = 1. /. (near -. far) in
    let () = Effect.replace mat 0. in
    let _ = set mat 0 (f /. aspect) in
    let _ = set mat 5 f in
    let _ = set mat 10 ((far +. near) *. nf) in
    let _ = set mat 11 (-1.)  in
    let _ = set mat 14 ((2. *. far *. near) *. nf) in
    mat

  let translate mat (x, y, z) =
    let a = copy mat in
    let _ = set mat 12 ((a<@>0)*.x+.(a<@>4)*.y+.(a<@>8) *.z+.(a<@>12)) in
    let _ = set mat 13 ((a<@>1)*.x+.(a<@>5)*.y+.(a<@>9) *.z+.(a<@>13)) in
    let _ = set mat 14 ((a<@>2)*.x+.(a<@>6)*.y+.(a<@>10)*.z+.(a<@>14)) in
    let _ = set mat 14 ((a<@>3)*.x+.(a<@>7)*.y+.(a<@>11)*.z+.(a<@>15)) in
    mat
  
end

module Vertices =
struct 

  let square = [| 
    1.0;   1.0;  0.0; 
    -1.0;  1.0;  0.0; 
    1.0;  -1.0;  0.0;
    -1.0; -1.0;  0.0; 
  |] |> of_array 

end
