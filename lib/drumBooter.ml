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

module type CTX =
sig

  val receiver : string
  val width : int
  val height : int
  val fill : DrumColor.t option

end

module Game = functor (F : CTX) ->
struct

  let _ = DrumPromises.(
      run dom_onload (fun () ->
          let elt = Html.getById F.receiver in
          let _   = DrumCanvas.createIn elt F.width F.height in
          ()
        )
    ) 
  
end
