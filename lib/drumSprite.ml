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
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
*)

open DrumPervasives

class sprite =
  object(this)

    val mutable x        = 0.0
    val mutable y        = 0.0
    val mutable zoom_x   = 1.0
    val mutable zoom_y   = 1.0
    val mutable opacity  = 1.0
    val mutable angle    = 0.0
    val mutable texture  : basic_texture option = None

    method get_x         = x
    method get_y         = y
    method get_zoom_x    = zoom_x
    method get_zoom_y    = zoom_y
    method get_opacity   = opacity
    method get_angle     = angle

    method set_x xi      = x       <- xi
    method set_y yi      = y       <- yi
    method set_zoom_x z  = zoom_x  <- z
    method set_zoom_y z  = zoom_y  <- z
    method set_opacity v = opacity <- v
    method set_angle a   = angle   <- a
    method set_texture t = texture <- t

  end

and virtual basic_texture (width_in, height_in) =
  object(this)

    val mutable width : float   = width_in
    val mutable height : float  = height_in

    method get_width            = width
    method get_height           = height

    method virtual draw    : DrumGame.state -> sprite -> unit
    method virtual dispose : DrumGame.state -> sprite -> unit

  end
