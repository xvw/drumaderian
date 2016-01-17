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

type x_shader =
  | XFragment of string
  | XVertex of string

let retreive_shader gl shad =
  let shader = match shad with
    | XFragment _ -> gl ## _FRAGMENT_SHADER_
    | XVertex _ -> gl ## _VERTEX_SHADER_
  in gl ## createShader(shader)

let retreive_shader_content = function
  | XFragment str -> js_string str
  | XVertex str -> js_string str

(* Represent a Shader *)
class t((gl_in : gl), shader_in) =
  object(self)
        
    val gl = gl_in
    val raw_shader = shader_in
    val shader = retreive_shader gl_in shader_in
    val shader_str = retreive_shader_content shader_in
    val mutable compiled = false

    method isCompiled() = compiled
    method obj() = shader
    method source()  : unit = gl ## shaderSource(shader, shader_str)
        
    method pre_compile() : unit =
      let _ = gl ## compileShader(shader) in
      if (js_false (gl ## getShaderParameter(shader, gl ## _COMPILE_STATUS_)))
      then js_alert (gl ## getShaderInfoLog(shader))
      else compiled <- true
          
    method compile() : unit =
      (* Compile only if it is necessary *)
      if (not (self#isCompiled()))
      then let () = self#source() in self#compile()
      
  end

(* Presaved Shader *)
module Cached =
struct

  let x_fragment color =
    let open DrumColor in
    let s = 
      "precision mediump float;\n"
      ^ "void main(void) {\n"
      ^ "gl_FragColor = vec4("
      ^ join color
      ^ ");"
      ^ "}"
    in XFragment s

  let default_fragment = x_fragment DrumColor.white

  let x_vertex position uMVMatrix uPMatrix =
    let s = 
      "attribute vec3 "
      ^ position ^ ";\n"
      ^ "uniform mat4 " ^ uMVMatrix ^ ";\n"
      ^ "uniform mat4 " ^ uPMatrix ^ ";\n"
      ^ "void main(void){"
      ^ "gl_Position = " ^ uPMatrix ^ " * " ^ uMVMatrix
      ^ " * vec4(" ^ position ^ ", 1.0);\n" 
      ^ "}"
    in XVertex s

  let default_vertex = x_vertex
      "aVertexPosition"
      "uMVMatrix"
      "uPMatrix"

end
