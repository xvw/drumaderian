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

type shader = WebGL.shader Js.t
type ctx = WebGL.renderingContext Js.t
type program = WebGL.program Js.t
type 'a uniLoc = 'a WebGL.uniformLocation Js.t

type kind =
  | Vertex of string
  | Fragment of string

type buffer_kind =
  | BufferArray

type draw_king =
  | DrawStatic

let of_buffer gl = function
  | BufferArray -> gl ## _ARRAY_BUFFER_

let of_draw gl = function
  | DrawStatic -> gl ## _STATIC_DRAW_

let create_shader (gl:ctx) sh =
  let txt, shader =
    match sh with
    | Vertex x -> x, gl ## createShader(gl ## _VERTEX_SHADER_)
    | Fragment x -> x, gl ## createShader(gl ## _FRAGMENT_SHADER_)
  in
  let _ = gl ## shaderSource(shader, js_string txt) in
  let _ = gl ## compileShader (shader) in
  let r = gl ## getShaderParameter(shader, gl##_COMPILE_STATUS_) in
  let _ = if (r != Js._true) then
      let log = gl##getShaderInfoLog(shader) in
      alert ("An error is occured : " ^ (caml_string log))
  in shader


class shader_obj (gl_context, k) =
  object(self)

    val context : ctx = gl_context
    val raw_shader : kind = k
    val shader : shader = create_shader gl_context k

    method get_obj (): shader  = shader

  end

class program_obj (gl_context) =
  object(self)

    val context : ctx = gl_context
    val program : program = gl_context ## createProgram()
        
    method link () : unit =
      let _ = context ## linkProgram (program) in
      let _ =
        context ## getProgramParameter(
          program,
          context ## _LINK_STATUS_)
      in ()

    method use () : unit = context ## useProgram (program)
        
    method attach (shader : shader_obj) : unit =
      context ## attachShader(program, (shader # get_obj()))
        
    method attach_more shaders =
      List.iter (fun x -> self # attach (x)) shaders

    method get_obj () = program
      
    
  end

class vertex_position (gl_context, program_in, name) =
  object(self)

    val context = gl_context
    val program = program_in # get_obj ()
    val raw_name = name
    val position : WebGL.uint =
      gl_context ## getAttribLocation(
        program_in # get_obj (),
        js_string name
      )
    
    method get_position () = position

    initializer
      context ## enableVertexAttribArray (position)

  end

class buffer (gl_context, vertices_in, buff, drw) =
  object(self)

    val context : ctx  = gl_context
    val buffer = gl_context ## createBuffer()
    val vertices = vertices_in
    val buffer_kind = of_buffer gl_context buff
    val draw_kind = of_draw gl_context drw

    method bind_for_draw (vertex_position : vertex_position) =
      let zerosize : WebGL.sizei = 0 in
      let zerointpr : WebGL.intptr = 0 in 
      let _ = context ## bindBuffer (buffer_kind, buffer) in
      let () = context ## vertexAttribPointer(
          vertex_position # get_position (),
          3,
          context##_FLOAT,
          Js._false,
          zerosize, zerointpr
        ) in ()

    initializer
      let _ = context ## bindBuffer (buffer_kind, buffer) in
      let r = float32array vertices
      in
      let () = context ## bufferData(
          buffer_kind, r , draw_kind
        ) in
      ()
      
  end

module Presaved =
struct

  let x_vertex =
    Vertex (
      "attribute vec3 aPosition;\n"
      ^ "uniform mat4 uMVMatrix;\nuniform mat4 uPMatrix;"
      ^ "void main(void) {\n"
      ^ "gl_Position = uPMatrix * uMVMatrix * vec4(aPosition, 1.0);\n}"
    )

  let x_fragment (color : DrumColor.gl) =
    let open DrumColor in
    Fragment (
      "void main(void) {\n"
      ^ (Printf.sprintf
           "gl_FragColor = vec4(%g, %g, %g, %g);\n}"
           color.red color.green color.blue color.alpha)
    )

  let generic_fragment = x_fragment DrumColor.white
  
end

