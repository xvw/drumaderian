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

include Lwt_js_events
open DrumPervasives

let rec delayed ?(delay=0.1) f =
  let _ = f () in
  Lwt_js.sleep delay
  >>= (fun _ -> delayed ~delay f)

let forever = delayed

let until ?(delay=0.1) pred f =
  let cpt = ref 0 in
  let _ = while (pred !cpt) do
      let _ = Lwt_js.sleep delay
        >>= (fun _ -> Lwt.return (f ()))
      in cpt := !cpt + 1
    done
  in Lwt.return_unit

let forN ?(delay=0.1) n =
  until ~delay (fun i -> i < n)

