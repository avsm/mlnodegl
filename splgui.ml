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
 * $Id: splgui.ml,v 1.13 2005/06/17 23:17:03 avsm Exp $
 *)

open Utils
open Node
open Glnode
open Glnodenet

let debug = Debug.debug "main"
let warn = Debug.debug "warn"


(* Initialises everything *)
class screen gi net = object(self)
    inherit Screen.screen as super
    
    val mw = new Widgets.mainwindow 1.
    val mutable glnode = None
    
    method init ~init_w ~init_h ~title ~fullscreen =
        super#init ~init_w:init_w ~init_h:init_h ~title:title ~fullscreen:fullscreen;
        let gln = new Glnode.glnode self gi net in
        glnode <- Some gln;
        gln#init;
        (* Create a couple of default dialog windows *)
        let h = new Widgets.subwindow "Help" Dialogs.keys_dialog in
        let s = new Widgets.subwindow "Status" (Dialogs.status_dialog :> Widgets.widget) in
        (* Position them (see Widgets.screen_width) *)
        mw#add_subwindow h; mw#move_subwindow h (5.) (30.); 
        mw#add_subwindow s; mw#move_subwindow h (10.) (20.);
        (* Make sure we're listening to mouse click events *)
        List.iter Mouse.global_mouse_handler#push [
            (new glnode_mouse_hover gln :> Mouse.mouse_handler);
            (new glnode_mouse_click gln :> Mouse.mouse_handler);
            (new Widgets.Events.mouse_window_click mw :> Mouse.mouse_handler);
        ]

    val mutable help_window_number = 2

    method keypress key x y =
        match (char_of_int key) with
        |'q' -> exit 0
        |'h' ->
            let new_h = new Widgets.subwindow ("Help window #" ^ 
                (string_of_int help_window_number)) Dialogs.help_dialog in
             help_window_number <- help_window_number + 1;
             mw#add_subwindow new_h;
             ignore(Widgets.drop_window_from_sky new_h 0. (-50.) (20.) 50. 0.)
        |'r' -> ignore(may (fun x -> x#randomize) glnode);
        |'i' -> ignore(may (fun x -> x#init) glnode);
        |'t' -> ignore(may (fun x -> x#reset_temperature) glnode);
        |'='|'+' -> ignore(may (fun x ->
            let c = x#get_conf in
            c.circle_radius <- c.circle_radius +. 0.2) glnode);
        |'-' -> ignore(may (fun x ->
            let c = x#get_conf in
            c.circle_radius <- c.circle_radius -. 0.2) glnode);
        |_ as c -> warn (Printf.sprintf "Unknown key: '%c'" c)

        
    method redraw = 
        super#redraw;        
        ignore (may (fun x -> x#redraw) glnode);
        GlMat.mode `projection;
        GlMat.load_identity ();
        Widgets.ortho ();
        GlMat.mode `modelview;
        GlMat.load_identity ();
        mw#redraw;
        Gl.flush ();
        Glut.swapBuffers();
        Glut.postRedisplay()

    initializer Screen.main := Some (self :> Screen.screen)
end

let fullscreen = ref false

let iof = int_of_float
let foi = float_of_int

let _ = 
    Random.self_init ();
    let default_port = 4000 in
    let initial_nodes = 40 in
    let blocksize = 20 in
    let max_repulsion_dist = 8. in
    let initial_temperature = 5. in
    let size = {x=75.;y=75.} in
    let k = 2. in
    let c = 0.05 in
    let counter = ref 0 in
    let gi = { 
        initial_nodes=initial_nodes;
        size=size; blocksize=blocksize; nodes=[]; counter=counter;
        max_repulsion_dist=max_repulsion_dist;
        initial_temperature=initial_temperature; k=k; c=c;
    } in
    let port = ref default_port in
    let spec = [ 
	       ("-fullscreen", Arg.Unit (fun () -> fullscreen := true), "full-screen mode");
	       ("-p", Arg.Int (fun x -> port := x), "TCP port to listen on");
	        ] in
    Arg.parse spec (fun x -> Printf.printf "Unknown argument: %s" x) "test the widgets thing";

    let glnodenet = new glnodenet in
    glnodenet#init !port;
    let screen = new screen gi glnodenet in
    screen#init ~init_w:1000 ~init_h:1000 ~title:"SPLAT Debugger" ~fullscreen:(!fullscreen);
    
    screen#mainLoop
