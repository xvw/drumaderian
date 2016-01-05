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
    
    val mutable pMatrix : float array  = DrumMatrix.create ()
    val mutable mMatrix : float array  = DrumMatrix.create ()
    val context : WebGL.renderingContext Js.t = gl_context

    method getProjection () = pMatrix
    method getMovement () = mMatrix

    method perspective() =
      DrumMatrix.perspective pMatrix fov aspect near far

    method identity () =
      DrumMatrix.identity mMatrix

    method setAMatrixUniforms (
        (
          program,
          name,
          matrix
        ) : (DrumShader.program_obj * string * float array)
      ) =
      let alphaUniform = gl_context ## getUniformLocation(
          program # get_obj (),
          js_string name
        )
      in gl_context ## uniformMatrix4fv_typed (
        alphaUniform,
        Js._false,
        float32array matrix
      ) |> ignore

    method set_matrix_uniforms (shaderA, shaderB, program) =
      let _ = self # setAMatrixUniforms(program, shaderA, pMatrix) in
      self # setAMatrixUniforms(program, shaderB, mMatrix)

    (* method translate (x, y, z) = *)
    (*   DrumM *)

    method draw_buffer (
        (shaderA, shaderB, program, buffer, vertex_position) :
          string
          * string
          * DrumShader.program_obj
          * DrumShader.buffer
          * DrumShader.vertex_position) =
      let _ = buffer # bind_for_draw (vertex_position) in
      let _ = self # set_matrix_uniforms (shaderA, shaderB, program) in
      let _ = gl_context ## drawArrays(
          gl_context ## _TRIANGLE_STRIP_,
          0, 4
        )
      in ()

    initializer
      let w, h = DrumCanvas.dimension () in
      let wi : WebGL.sizei = w in
      let hi : WebGL.sizei = h in
      let _ = gl_context ## viewport(0, 0, wi, hi) in
      let _ = aspect <- (float_of_int w) /. (float_of_int h) in
      let _ =
        gl_context ## clear (
          gl_context ##_DEPTH_BUFFER_BIT_ lor gl_context ##_COLOR_BUFFER_BIT_
        )
      in
      let _ = self#perspective() in
      let _ = self#identity() in
      ()
    
  end


module InDebug =
struct

  let draw_rect gl =
    let open DrumShader in
    let vertex = new shader_obj(gl, Presaved.x_vertex) in
    let fragment = new shader_obj(gl, Presaved.generic_fragment) in
    let program = new program_obj(gl) in
    let scene = new scene (gl, 45.0) in
    let _ = program # attach(vertex) in
    let _ = program # attach(fragment) in
    let _ = program # link () in
    let _ = program # use () in
    let b =  new buffer(
      gl,
      DrumVertice.standard_rect,
      BufferArray,
      DrawStatic
    ) in
    let v =  new vertex_position(gl, program, "aPosition") in
    let _ = scene # draw_buffer ("uPMatrix", "uMVMatrix", program, b, v) in
    ()

end
