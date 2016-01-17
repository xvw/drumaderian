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

type shader = DrumShader.t

(* Represent a Shader's program *)
class t((gl_in : gl), (fragment_in : shader), (vertex_in : shader)) =
  object(self)
        
    val gl = gl_in
    val fragment = fragment_in
    val vertex = vertex_in
    val program = gl_in ## createProgram()
    val mutable linked = false
    val mutable used = false
    val mutable position = []
    val mutable pMatrix  = []
    val mutable mvMatrix = []

    method obj() = program

    method isLinked() = linked
    method isUsed() = used
      
    method attachShader(shader : shader) : unit =
      gl ## attachShader(program, shader#obj())

    method attachShaders() : unit =
      let _ = self # compileShaders() in
      let _ = self # attachShader(vertex) in
      self # attachShader(fragment)
    
    method compileShaders() : unit =
      let _ = fragment # compile ()
      in vertex # compile ()

    method link() : unit =
      let _ = gl ## linkProgram(program) in
      if (js_false (gl ## getProgramParameter(program, gl ## _LINK_STATUS_)))
      then alert "Unlinkable program !"
      else linked <- true

    method pre_use() : unit =
      let _ = gl ## useProgram(program) in
      used <- true

    method enablePosition(pos) : unit =
      let pos = gl ## getAttribLocation(program, js_string pos) in
      let _ = position <- [pos]
      in gl ## enableVertexAttribArray(pos)

    method positionAttribute() =
      match position with
      | [pos] -> pos
      | _ ->
        let () = alert "Position malformed" in 
        raise DrumExceptions.Malformed_position

    method uniformLocation(var) : [`vec4] WebGL.uniformLocation Js.t =
      gl ## getUniformLocation(program, js_string var)

    method use(pos, pmatrix, mvmatrix) : unit =
      if not (self # isUsed())
      then 
        let _ = self # compileShaders() in
        let _ = self # attachShaders() in
        let _ = self # link() in
        let _ = self # pre_use() in
        let _ = self # enablePosition(pos) in
        let _ = pMatrix  <- [self # uniformLocation(pmatrix)] in
        let _ = mvMatrix <- [self # uniformLocation(mvmatrix)] in
        () 

  end
