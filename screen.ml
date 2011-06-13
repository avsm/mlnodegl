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
 * $Id: screen.ml,v 1.7 2005/06/17 00:00:58 avsm Exp $
 *)

(* Wrapper around the GLUT main window, monitors resize events etc *)

open Colours
open Gfxutils

let debug = Debug.debug "screen"
let error = Debug.error "screen"

class screen = object(self)

    val mutable handle = None
    val mutable device_w = 0
    val mutable device_h = 0

    val mutable aspect_ratio = 0.
    val pixel_multiplier = 1.

    method aspect_ratio = aspect_ratio
    method h = device_h

    method redraw = 
        GlClear.depth 1.0; 
        GlClear.color (rgba_to_rgb bgcolor);
        GlClear.clear [`color; `depth];

        GlDraw.color (rgba_to_rgb black);

        List.iter (fun x -> Gl.disable x) [ `lighting; `light0; `light1; `depth_test; `normalize];
        set_drawing_mode `Unlit

    method keypress key x y = 
        debug (Printf.sprintf "keypress key = '%c'  x = %d y = %d" (char_of_int key) x y) 

    method to_world (x, y) =
        try 
            let x', y', _ = GluMat.unproject (float x, float (device_h - y), 0.) in
            x', y'
        with Gl.GLerror txt ->
            Printf.printf "ignored error: %s\n" txt;
            (0.,0.)

    (* Set viewport to at least w,h size with the right aspect ratio *)
    method ortho2d x y w h =
        let newasp = w /. h in
        let nw,nh = if newasp > aspect_ratio then
            (w, w /. aspect_ratio)
        else
            (h *. aspect_ratio, h)
        in
        GluMat.ortho2d ~x:(x, x +. nw) ~y:(y, y +. nh)

    method init ~init_w ~init_h ~title ~fullscreen = 
        let argv' = Glut.init Sys.argv in
        
        Glut.initDisplayMode ~alpha:true ~double_buffer:true
                             ~depth:true ~stencil:true ();
        Glut.initWindowSize ~w:init_w ~h:init_h;

        handle <- Some (Glut.createWindow ~title:title);
        
        device_w <- init_w;
        device_h <- init_h;
        aspect_ratio <- float device_w /. (float device_h);
        
        Glut.reshapeFunc 
            ~cb:(fun ~w ~h ->
                    device_w <- w;
                    device_h <- h;            
                    aspect_ratio <- (float w) /. (float h) *. pixel_multiplier;
                    GlDraw.viewport 0 0 w h;
                    self#redraw 
            );
        Glut.displayFunc ~cb:(fun () -> self#redraw);
        Glut.keyboardFunc (fun ~key:key ~x:x ~y:y -> self#keypress key x y);
        
        Glut.mouseFunc (Mouse.global_mouse_handler#mouseFunc);
        Glut.motionFunc (Mouse.global_mouse_handler#motionFunc);
        Glut.passiveMotionFunc (Mouse.global_mouse_handler#passiveMotionFunc);
        
        Gl.enable `alpha_test;
        Gl.enable `line_smooth;
        Gl.enable `blend;
        Gl.enable `normalize;
        GlFunc.blend_func `src_alpha `one_minus_src_alpha;
        GlMisc.hint `line_smooth `nicest;
        GlMisc.hint `perspective_correction `nicest;
        GlMisc.hint `polygon_smooth `nicest;
        
        GlDraw.shade_model `smooth;
        
        Fonts.init_fonts "verdana";
        if fullscreen then Glut.fullScreen ()
        
    method mainLoop = Glut.mainLoop ()

end        


let main : screen option ref = ref None

let get () = match !main with
| None -> error "no screen object defined; subclass screen and set the reference!"; raise Not_found
| Some x -> x
