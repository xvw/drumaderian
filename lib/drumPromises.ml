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

let wakeup w x _ = let _ = Lwt.wakeup w () in x
let wrap f = (fun x -> Lwt.return (f x))
let run promise f elt = promise elt >>= (wrap f)
                                        
let raw_onload elt () =
  let thread, wakener = Lwt.wait () in
  let _ = elt ## onload <-
      Dom.handler (wakeup wakener Js._true)
  in thread
    
let dom_onload () = raw_onload (Dom_html.window) ()
let img_onload i  = raw_onload i ()
