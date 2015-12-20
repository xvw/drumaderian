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


let apply f = DrumCanvas.perform3d ( fun _ gl -> f gl )
let create_fragment gl = gl ## createShader (gl ##_FRAGMENT_SHADER_)
let create_vertex gl = gl ## createShader (gl ##_VERTEX_SHADER_)

let create_shader_from_script gl script =
  let rtype = caml_string (script ## _type) in
  match rtype with
  | "x-shader/x-fragment" -> create_fragment gl
  | "x-shader/x-vertex" -> create_vertex gl
  | _ -> perform_fail DrumExceptions.Invalid_shader

let compile gl shader content =
  let _ = gl ## shaderSource(shader, content) in 
  let _ = gl ## compileShader(shader) in
  if (not (js_true (gl ## getShaderParameter(shader, gl##_COMPILE_STATUS_))))
  then
    raise
      (DrumExceptions.Compilation_shader
         (caml_string content, gl##getShaderInfoLog(shader)))

let create_program gl = gl ## createProgram()
let attach gl prg shader = gl ## attachShader(prg, shader)
let link gl prg =
  let _ = gl ## linkProgram (prg) in
  let _ =
    if not (js_true (gl ## getProgramParameter(prg, gl##_LINK_STATUS_)))
    then (raise DrumExceptions.Unlinkable_shader)
  in prg
let use gl prg = gl ## useProgram(prg)

let use_shaders gl shaders =
  let prg = create_program gl in
  let _ = List.iter (fun x -> attach gl prg x) shaders in
  let _ = link gl prg in
  use gl prg
  

let retrieve_from_DOM gl id =
  let script =
    match Html.getById_opt id with
    | None -> raise (DrumExceptions.Unknown_shader id)
    | Some x ->
      Dom_html.CoerceTo.script x
      |> (fun x ->
          Js.Opt.get x
            (perform_fail DrumExceptions.Invalid_shader)
        )
  in
  
  let text = script ## text in
  let shader = create_shader_from_script gl script in 
  let _ = compile gl shader text in
  shader
  
  
let location ?(enable=true) gl prg label =
  let lbl = gl ## getAttribLocation(prg, label) in
  if enable then gl ## enableVertexAttribArray(lbl)


module Presaved =
struct

  let sample_vertex =
    ("attribute vec3 aVertexPosition;\n"
    ^ "uniform mat4 uMVMatrix;\nuniform mat4 uPMatrix;\n"
    ^ "void main(void) {"
    ^ "gl_Position = uPMatrix * uMVMatrix * vec4(aVertexPosition, 1.0);}")
    |> js_string

  let sample_fragment color =
    let open DrumColor in
    (Printf.sprintf
       "void main(void) {gl_FragColor = vec4(%g, %g, %g, %g);}"
       color.red
       color.green
       color.blue
       color.alpha
    )
    |> js_string
  
end
