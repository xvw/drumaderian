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

class t((gl_in : gl), (vertices_in : DrumMatrix.t)) =
  object(self)

    val gl = gl_in
    val buffer = gl_in ## createBuffer()
    val itemSize = 3
    val vertices = vertices_in
    val numItems = (DrumMatrix.length vertices_in) / 3
    val mutable binded = false

    method obj() = buffer
      
    method bind() =
      if (not binded)
      then 
        let _ = gl ## bindBuffer(gl ## _ARRAY_BUFFER_, buffer) in
        let _ =
          gl ## bufferData(
            gl ## _ARRAY_BUFFER_, vertices,
            gl ## _STATIC_DRAW_
          ) in
        let _ = binded <- true in
        ()

    method prepare(program : DrumProgram.std) : unit =
      let _ = gl ## bindBuffer(gl ## _ARRAY_BUFFER_, buffer) in
      gl ## vertexAttribPointer(
        program # vPosition(),
        itemSize,
        gl ## _FLOAT,
        Js._false,
        0, 0
      )

    method drawArrays() =
      gl ## drawArrays(gl ## _TRIANGLE_STRIP_, 0, numItems)

    initializer 
      self # bind()
      
  end

let square gl = new t(gl, DrumMatrix.Vertices.square)
