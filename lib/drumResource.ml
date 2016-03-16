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

let unopt x =
  Js.Opt.get x
    (fun () -> raise (Error.RuntimeError "unable to find"))

let load_image ?id ?path ~onload () =
  let img =
    Dom_html.createImg document
    |> Dom_html.CoerceTo.img
    |> unopt
  in
  let _ = DrumOption.unit_map (fun x -> img ## id  <- (String.js x)) id   in
  let _ = DrumOption.unit_map (fun x -> img ## src <- (String.js x)) path in
  let _ =
    if Js.to_bool (img ## complete)
    then onload img
    else img ## onload <- Dom.handler (fun _ -> onload img; Js._false)
  in img
