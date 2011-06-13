(*
 * Copyright (c) 2005 David Scott <djs@fraserresearch.org>
 * Copyright (c) 2005 Fraser Research 
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * $Id: colours.ml,v 1.4 2005/06/17 00:00:58 avsm Exp $
 *)

open Gfxutils

(* 1. A set of useful colours *)

let white = (1., 1., 1., 1.)
let black = (0., 0., 0., 1.)
let bgcolor = (0.95, 0.95, 0.95, 1.0)

let grey = (0.8, 0.8, 0.8, 1.0)
let lightgrey = (0.9, 0.9, 0.9, 1.0)

let lightred = (1.0, 0.2, 0.2, 1.0) (* eg the nodes *)
let lightblue = (0.2, 0.2, 1.0, 1.0) (* eg the nodes *)
let lightgreen = (0.2, 1.0, 0.2, 1.0)

(* 2. Conversions between different colour formats *)

let rgba_to_rgb (r,g,b,a) = (r,g,b)

exception HSV_Out_Of_Range

(** hsv_to_rgb converts HSV to RGB. h \in Some [0,6] or None, s \in [0,1], v \in [0,1]
    r,g,b values are \in [0,1]
    conversion from C: http://www.acm.org/jgt/papers/SmithLyons96/hsv_rgb.html *)
let hsva_to_rgba (h,s,v,a) = match h with
    | None -> v,v,v,a
    | Some h ->
        let i = int_of_float (floor h) in
        let f = h -. (float_of_int i) in
        let f' = if i mod 2 = 0 then 1. -. f else f in
        let m = v *. (1. -. s) in
        let n = v *. (1. -. s *. f') in match i with
        | 6
        | 0 -> v,n,m ,a
        | 1 -> n,v,m ,a
        | 2 -> m,v,n ,a
        | 3 -> m,n,v ,a
        | 4 -> n,m,v ,a
        | 5 -> v,m,n ,a
        | _ -> raise HSV_Out_Of_Range

exception HSV_Undefined

let rgba_to_hsva (r,g,b,a) = 
    let x = min r (min g b)
    and v = max r (max g b) 
    and x_smallest = r <= (min g b)
    and g_smallest = g <= (min r b) in
    if (x = v) then raise HSV_Undefined;
    let f, i = match x_smallest, g_smallest with
    | true,  _     -> g -. b, 3
    | false, true  -> b -. r, 5
    | false, false -> r -. g, 1 in
    float_of_int i -. f /. (v -. x), (v -. x) /. v, v, a

let rgba_to_rgb (r,g,b,_) = (r,g,b)

let transparent a (r, g, b, _) = (r, g, b, a)
let lighten multiplier rgb = 
    let (h, s, v, a) = rgba_to_hsva rgb in
    let s' = s *. multiplier in
    let s' = min 1. (max 0. s') in
    hsva_to_rgba (Some h, s', v, a)


(* 3. To avoid getting confused with setting colours when in 2d/3d mode: *)
let drawing_mode : [ `Unlit | `Lit ] ref = ref `Unlit

let set_drawing_mode x = 
    drawing_mode := x;
    if !drawing_mode = `Unlit then Gl.disable `lighting else Gl.enable `lighting

let set_colour (r,g,b,a) = match !drawing_mode with
    | `Unlit -> GlDraw.color ~alpha:a (r,g,b)
    | `Lit -> GlLight.material ~face:`both (`diffuse (r,g,b,a))

(* 4. Random theme stuff: *)

module BlueTheme = struct
    let l = 0.5

    let text_colour = black
    let window_titlebar = lighten (l+.0.2) (transparent 1. lightblue)
    let window_background = lighten l (transparent 0.9 lightblue)
    let window_close = lighten l (transparent 1. lightred)
    let window_minimise = lighten l (transparent 1. lightgreen)

    let window_titlebar_button_size = 1.5
end

module Theme = BlueTheme
