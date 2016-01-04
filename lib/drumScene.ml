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

class scene ?(min = 0.1) ?(max = 100.) (gl_context, angle) =
  object(self)

    val mutable fov = angle
    val mutable far = max
    val mutable near = min
    val mutable aspect = 0.0
    
    val pMatrix : float array  = DrumMatrix.create ()
    val mMatrix : float array  = DrumMatrix.create ()
    val context = gl_context

    initializer
      let w = gl_context ## viewportWidth in
      let h = gl_context ## viewportHeight in 
      let _ = gl_context ## viewport(0, 0, w, h) in
      let _ = aspect <- w /. h in
      let _ =
        gl_context ## clear (
          gl_context ##_DEPTH_BUFFER_BIT_ lor gl_context ##_COLOR_BUFFER_BIT_
        )
      in
      DrumMatrix.perspective pMatrix fov aspect near far
      |> ignore
    
  end
