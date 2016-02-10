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

(** This module provides all interfaces using for the functors. *)


open DrumPervasives


module Make =
struct

  (* Functors for monad implementation *)
  (* Using join interface *)
  module WithJoin ( M : DrumInterfaces.Monad.JOIN) :
    DrumInterfaces.Monad.BASIC_INTERFACE with type 'a t = 'a M.t =
  struct
    include M
    let bind m f = join (fmap f m)
  end

  (* Using Bind interface *)
  module WithBind(M : DrumInterfaces.Monad.BIND) :
    DrumInterfaces.Monad.BASIC_INTERFACE with type 'a t = 'a M.t =
  struct
    include M
    let join m = bind m id
    let fmap f m = bind m (fun x -> return (f x))
  end

  (* Complete interface *)
  module Base(M : DrumInterfaces.Monad.BASIC_INTERFACE) :
    DrumInterfaces.Monad.INTERFACE with type 'a t = 'a M.t =
  struct
    include M
    let void _ = return ()
    let ( >>= ) f x = bind f x
    let ( <*> ) fs ms =
      fs >>= fun f ->
      ms >>= fun x -> return $ f x
    let ( <$> ) = fmap
    let ( <$  ) v = fmap (fun _ -> v)
    let ( *> ) x    = ( <*> ) (fmap (fun _ -> id) x)
	  let ( <* ) x _  = ( <*> ) (return id) x
    let ( <**> ) f x = flip (<*>) f x
    let ( >> ) m n   = m >>= (fun _ -> n)
    let liftM = fmap
    let liftM2 f x y = f <$> x <*> y
    let ( >|= ) x f = fmap f x
    let ( <=< ) f g  = fun x -> g x >>= f
	  let ( >=> ) f g  = flip ( <=< ) f g
	  let ( =<< ) f x  = flip ( >>= ) f x
  end

  module Plus
      (M : DrumInterfaces.Monad.BASIC_INTERFACE)
      (P : DrumInterfaces.Monad.PLUS with type 'a t = 'a M.t) :
    DrumInterfaces.Monad.PLUS_INTERFACE with type 'a t = 'a M.t =
  struct
    include Base(M)
    let mempty = P.mempty
    let mplus a b = P.mplus a b
    let ( <+> ) = mplus
    let keep_if f x =
      if f x then return x
      else mempty

  end

end
