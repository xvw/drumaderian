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

type kind =
  | Vertex of string
  | Fragment of string

let create_shader gl sh =
  let txt, shader =
    match sh with
    | Vertex x -> x, gl ## createShader(gl ## _VERTEX_SHADER_)
    | Fragment x -> x, gl ## createShader(gl ## _FRAGMENT_SHADER_)
  in
  let _ = gl ## shaderSource(shader, js_string txt) in
  let _ = gl ## compileShader (shader) in
  let _ =
    if (not (js_true (
        gl ## getShaderParameter(shader, gl##_COMPILE_STATUS_))))
    then raise (DrumExceptions.Compilation_shader
                  (txt, gl ## getShaderInfoLog (shader)))
  in shader


class shader_obj (gl_context, k) =
  object(self)

    val context = gl_context
    val raw_shader : kind = k
    val shader : shader = create_shader gl_context k

    method get_obj (): shader  = shader

  end

class program_obj (gl_context) =
  object(self)

    val context = gl_context
    val program : program = gl_context ## createProgram()
        
    method link () : unit = context ## linkProgram (program)
    method use () : unit = context ## useProgram (program)
    method attach (shader : shader_obj) : unit =
      context ## attachShader(program, (shader # get_obj()))
        
    method attach_more shaders =
      List.iter (fun x -> self # attach (x)) shaders
    
  end

