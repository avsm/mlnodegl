(*
 * Copyright (c) 2005 Anil Madhavapeddy <anil@recoil.org>
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
 * $Id: glnode.ml,v 1.5 2005/06/17 23:17:03 avsm Exp $
 *)

open Printf
open Debug
open Gfxutils
open Node
open Utils

type colour = (float * float * float)

type render_conf = {
    mutable circle_radius: float;
    mutable triangle_size: float;
    mutable circle_colour: colour;
    mutable edge_colour: colour;
    mutable edge_label_colour: colour;
    mutable node_label_colour: colour;
    mutable node_label_scale: float;
    mutable edge_label_scale: float;
}

let default_conf = {
    circle_radius=1.;
    triangle_size=0.3;
    circle_colour=(0.,0.,0.1);
    edge_colour=(0.2,0.,0.);
    edge_label_colour=(0.0,0.1,0.);
    node_label_colour=(0.9,0.9,1.0);
    node_label_scale=0.25;
    edge_label_scale=0.25;
}

let draw_grid gi =
    GlDraw.color ~alpha:0.5 (0.8,0.8,0.8);
    GlDraw.line_width 0.1;
    for x = 0 to (iof gi.size.x) do
        let xp = foi x in
        for y = 0 to (iof gi.size.y) do
            let yp = foi y in
            Widgets.Shapes.line 0. yp gi.size.x yp;
            Widgets.Shapes.line xp 0. xp gi.size.y;
        done;
    done

let draw_edge gi conf edge =
    let f = !(edge.from_node) in
    let t = !(edge.to_node) in
    let mv = t.pos -/ f.pos in
    let dmv = vec_mag mv in
    let axis = {x=1.;y=0.} in
    let pi = 4.0 *. atan 1.0 in
    let theta = atan (mv.y /. mv.x) in
    let theta = if mv.x < 0. then theta +. pi else theta in
    let deg = theta *. (180. /. pi) in
    let rad = conf.circle_radius *. 0.75 in
    push (fun () ->
        GlDraw.line_width edge.weight;
        (* Move origin to the start point *)
        GlMat.translate3 (f.pos.x, f.pos.y, 0.);
        (* Rotate axes around so we're pointing to target along x axis *)
        GlMat.rotate3 ~angle:deg (0.,0.,1.);
        GlDraw.color conf.edge_colour;
        (* Draw line from edge of our circle to edge of target *)
        Widgets.Shapes.line rad 0. (dmv -. rad) 0.;
        push (fun () ->
            GlMat.translate3 ((dmv -. rad), 0., 0.);
            Widgets.Shapes.filled_triangle 0.25 0.25;
        );
        push (fun () ->
            let sc = conf.edge_label_scale in
            let labellen = Fonts.string_width edge.label *. sc in
            let t = (dmv /. 2.) -. (labellen /. 2.) in
            GlMat.translate3 (t, 0.1, 0.);
            GlMat.scale3 (sc,sc,sc);
            Fonts.aa_render_string 0.9 conf.edge_label_colour edge.label;
        );
        GlDraw.line_width 1.0
    )
    
let draw_nodes gi conf =
    List.iter (fun node ->
        pushname node.uid (fun () ->
            push (fun () ->
                let rad = conf.circle_radius in
                GlDraw.color conf.circle_colour;
                GlMat.translate3 (node.pos.x, node.pos.y, 0.);
                Widgets.Shapes.filled_circle rad;
                let sc = conf.node_label_scale in
                let fw = Fonts.string_width node.name *. sc in
                let fh = sc in
                (* Currently in middle of circle drawing right and up *)
                GlMat.translate3 (-0.5 *. fw, -0.5 *. fh, 0.);
                GlMat.scale3 (sc, sc, sc);
                Fonts.aa_render_string 0.9 conf.node_label_colour node.name;
            );
        );
    ) gi.nodes;
    List.iter (fun node ->
        push (fun () ->
            List.iter (draw_edge gi conf) node.edges
        )
    ) gi.nodes;

class glnode = fun screen gi net -> object(self)
    
    val mutable settled = false
    val conf = default_conf
    
    method finished = false
    
    method get_conf = conf

    method init =
        gi.nodes <- generate_nodes gi;
        generate_edges gi;
        settled <- true;
        self#start_tick

    method private start_tick =
        if settled then begin
            settled <- false;
            Idle.add (self :> Utils.active_object);
        end

    method setup_projection =
        let pad = 1. in
        let (xmin, ymin, width, height) = nodes_bounding_box gi in
        screen#ortho2d (xmin -. pad) (ymin -. pad)
            (width +. (2. *. pad)) (height +. (2. *. pad));
        ()
                
    method redraw =
        GlMat.mode `projection;
        GlMat.load_identity ();
        self#setup_projection;
        GlMat.mode `modelview;
        (* grid drawing is too slow for large grids
        draw_grid gi;
        *)
        draw_nodes gi conf;
        ()
    
    method redraw_pick x y =
        GlMat.mode `projection;
        GlMat.load_identity ();
        GluMat.pick_matrix ~x:(float_of_int x) ~y:(float_of_int (screen#h - y))
            ~width:1. ~height:1.;
        self#setup_projection;
        GlMat.mode `modelview;
        draw_nodes gi conf;
        ()
        
    method pick_nodes x y =
        let names = pick (fun () -> self#redraw_pick x y) in
        let set = List.fold_left (fun s e -> IntSet.add e s) IntSet.empty names in
        let uids = IntSet.elements set in
        List.fold_left (fun a uid ->
            match find_node_from_uid gi uid with
            |None -> a
            |Some n -> n :: a
        ) [] uids

    method tick =
        if not settled then begin
            let dist = tick_nodes gi in
            settled <- (dist < 0.4);
            Glut.postRedisplay ()
        end;
        while net#queue_size > 0 do
            match net#pop with
            |None -> ()
            |Some v ->
                print_endline ("got: " ^ v);
        done

    method randomize =
        randomize_nodes gi gi.nodes;
        self#start_tick
        
    method reset_temperature =
        List.iter (fun n -> n.temperature <- gi.initial_temperature) gi.nodes;
        self#start_tick
end

class glnode_mouse_click = fun glnode -> object(self)
    inherit Mouse.mouse_click Glut.LEFT_BUTTON
    
    method click x y =
        let nodes = glnode#pick_nodes x y in
        if List.length nodes > 0 then begin
            debug "node_click" (String.concat ", " (List.map (fun x -> x.name) nodes));
            true;
        end else false
        
    method name = "glnode_mouse_click"
end
    
class glnode_mouse_hover = fun glnode -> object(self)
    inherit Mouse.mouse_over

    method hover x y =
        let nodes = glnode#pick_nodes x y in
        if List.length nodes > 0 then begin
            debug "node_hover" (String.concat ", " (List.map (fun x -> x.name) nodes));
            true;
        end else false
    
    method name = "glnode_mouse_hover"
end