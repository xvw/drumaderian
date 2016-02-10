(*
 * Drumaderian
 * This file come form Oml and Js of ocaml bootstrapper
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


(** This module provide generators for handle monadic behaviour. *)

module Make :
sig

  (** List of all functors*)

  (** Creates the minimal context with Join and fmap.
      Returns a BASIC_INTERFACE
  *)
  module WithJoin (M : DrumInterfaces.Monad.JOIN ) :
    DrumInterfaces.Monad.BASIC_INTERFACE with type 'a t = 'a M.t

  (** Creates the minimal context with bind.
      Returns a BASIC_INTERFACE
  *)
  module WithBind (M : DrumInterfaces.Monad.BIND) :
    DrumInterfaces.Monad.BASIC_INTERFACE with type 'a t = 'a M.t

  (** Creates a complete context with a BASIC_INTERFACE
  *)
  module Base (M : DrumInterfaces.Monad.BASIC_INTERFACE) :
    DrumInterfaces.Monad.INTERFACE with type 'a t = 'a M.t

  (** Creates a complete context with a BASIC_INTERFACE and a
      PLUS interface
  *)
  module Plus
      (M : DrumInterfaces.Monad.BASIC_INTERFACE)
      (P : DrumInterfaces.Monad.PLUS with type 'a t = 'a M.t) :
    DrumInterfaces.Monad.PLUS_INTERFACE with type 'a t = 'a M.t
end
