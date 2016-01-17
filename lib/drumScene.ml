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

class t((gl_in : gl)) =
  object(self)

    val gl = gl_in
    val mvMatrix = DrumMatrix.Mat4.create ()
    val pMatrix  = DrumMatrix.Mat4.create ()

    method initViewport() =
      let w, h = DrumCanvas.dimension () in
      gl ## viewport(0, 0, w, h)

    method clear() =
      gl ## clear (
        gl##_DEPTH_BUFFER_BIT_
        lor gl##_COLOR_BUFFER_BIT_
      )

    method perspective(fov, aspect, near, far) =
      DrumMatrix.Mat4.perspective ~fov ~aspect ~near ~far pMatrix

    method translate(x, y, z) =
      DrumMatrix.Mat4.translate mvMatrix (x, y, z)

    method setMatrixUniform(program : DrumProgram.std) =
      let _ = gl ## uniformMatrix4fv_typed(
          program#pMatrix(), Js._false, pMatrix ) in
      let _ = gl ## uniformMatrix4fv_typed(
          program#mvMatrix(), Js._false, mvMatrix) in
      ()

    method drawBuffer((program : DrumProgram.std), (buffer : DrumBuffer.t)) =
      let _ = buffer#prepare(program) in
      let _ = self#setMatrixUniform(program) in
      buffer#drawArrays()

    method initialize(fov, aspect, near, far) =
      let _ = self # initViewport() in
      let _ = self # clear () in
      let _ = self # perspective (fov, aspect, near, far) in
      let _ = DrumMatrix.Mat4.identity(mvMatrix) in
      ()

    
  end
