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

let ratio () =
  let ((w:int), (h:int))  = DrumCanvas.dimension () in
  let fi, fj = float_of_int w, float_of_int h in
  fi /. fj

let scene_with_rect gl =
  let _ = log "a" in
  let xvertex = new DrumShader.t(gl, DrumShader.Cached.default_vertex) in
  let _ = log "b" in
  let xfragmt = new DrumShader.t(gl, DrumShader.Cached.default_fragment) in
  let _ = log "c" in
  let _ = xvertex # compile() in
  let _ = log "d" in
  let _ = xfragmt # compile() in
  let _ = log "e" in
  let program = DrumProgram.create gl xfragmt xvertex in
  let _ = log "f" in
  let _ = program # use("aVertexPosition", "uMVMatrix", "uPMatrix") in
  let _ = log "g" in
  let buffer  = new DrumBuffer.t(gl, DrumMatrix.Vertices.square) in
  let _ = log "h" in
  let scene   = new DrumScene.t(gl) in
  let _ = log "i" in
  let _       = scene # initialize(45.0, ratio(), 0.1, 100.0) in
  let _ = log "j" in
  let _ = scene # translate(3.0, 0., 0.) in
  let _ = log "k" in
  let _ = scene # drawBuffer(program, buffer) in
  let _ = log "l" in
  ()
