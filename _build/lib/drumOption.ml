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

let safe f x = try Some (f x) with _ -> None

let unit_map f = function
  | Some e -> f e
  | None -> ()

let some x = Some x
let none = None

let default value = function
  | None -> value
  | Some x -> x

let map f = function
  | None -> None
  | Some x -> Some (f x)

let apply = function
  | None -> id
  | Some f -> f

let is_some = function
  | Some _ -> true
  | _ -> false

let is_none = function
  | None -> true
  | _ -> false

let effect f message = function
  | None -> Error.fail message
  | Some x -> ignore $ f x



module Bind : DrumInterfaces.Monad.BIND with type 'a t = 'a option =
struct
  type 'a t = 'a option
  let return x = some x
  let bind m f = match m with
    | Some x -> f x
    | None -> None
end

module Plus : DrumInterfaces.Monad.PLUS with type 'a t = 'a option =
struct
  type 'a t = 'a option
  let mempty = None
  let mplus a b =
    match (a, b) with
    | None, x -> x
    | x, None -> x
    | x, _ -> x
end

module Basic_monad = DrumMonad.Make.WithBind(Bind)
include DrumMonad.Make.Plus (Basic_monad) (Plus)

let perform v = function
  | None -> v
  | Some x -> x

let (>?=) = perform
