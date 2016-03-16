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
include List

exception Empty_list

module Join : DrumInterfaces.Monad.JOIN with type 'a t = 'a list =
struct
  type 'a t = 'a list
  let return x = [x]
  let fmap = List.map
  let join = List.flatten
end

module Plus : DrumInterfaces.Monad.PLUS with type 'a t = 'a list =
struct
  type 'a t = 'a list
  let mempty = []
  let mplus = List.append
end

module Basic_monad = DrumMonad.Make.WithJoin (Join)
include DrumMonad.Make.Plus (Basic_monad) (Plus)

let reduce f = function
  | [] -> raise Empty_list
  | x :: xs -> fold_left f x xs
