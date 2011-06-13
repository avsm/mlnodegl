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
 * $Id: node.ml,v 1.5 2005/06/16 23:57:28 avsm Exp $
 *)

open Printf
open Debug

type vector = {
    x: float;
    y: float;
}
and
edge = {
    from_node: node ref;
    to_node: node ref;
    weight: float;
    label: string;
}
and node = {
    name: string;
    uid: int;
    mutable edges: edge list;
    mutable pos: vector;
    mutable force: vector;
    mutable temperature: float;
}
and grid_info = {
    size: vector;
    blocksize: int;
    mutable nodes: node list;
    counter: int ref;
    max_repulsion_dist: float;
    initial_temperature: float;
    initial_nodes: int;
    k: float;
    c: float;
}

(* convenience functions *)
let foi = float_of_int
let iof = int_of_float
let vec_op fn v1 v2 = {x=(fn v1.x v2.x); y=(fn v1.y v2.y)}
let vec_const fn v1 v = {x=(fn v1.x v); y=(fn v1.y v)}
let vec_map fn v = {x=(fn v.x); y=(fn v.y)}
let (+/) = vec_op (+.) 
let (-/) = vec_op (-.)
let ( */) = vec_op ( *.)
let (+//) = vec_const (+.)
let (-//) = vec_const (-.)
let ( *//) = vec_const ( *.)
let vec_mag v = sqrt ((v.x *. v.x) +. (v.y *. v.y))
let range minv maxv v = max (min maxv v) (minv)

(* debugging aids *)
let string_of_vec v = sprintf "[x=%.2f;y=%.2f]" v.x v.y
 
(* Generate list of numbers from 0->n *)
let rec numlist n a =
    match n with |(-1) -> a |x -> numlist (n-1) (x::a)

let randomize_nodes gi nodes =
    let nodehash = Hashtbl.create 1 in
    List.iter (fun node ->
        let gotcoords = ref false in
        let xpos = ref 0. in
        let ypos = ref 0. in
        while not !gotcoords do
            xpos := Random.float gi.size.x;
            ypos := Random.float gi.size.y;
            let t = ((iof !xpos), (iof !ypos)) in
            try let _ = Hashtbl.find nodehash t in ()
            with Not_found -> begin
                Hashtbl.add nodehash t ();
                gotcoords := true;
            end;
        done;
        node.pos <- {x= !xpos; y= !ypos};
    ) nodes
    
(* Generate some random and non-overlapping nodes *)
let generate_nodes gi =
    gi.counter := 0;
    let num = gi.initial_nodes in
    let nodehash = Hashtbl.create 1 in
    let n = List.map (fun x -> 
        let pos = {x=0.;y=0.} in
        let force = {x=0.;y=0.} in
        { 
          name = (sprintf "%d" x); uid=(50+x);
          pos=pos; force=force; edges=[];
          temperature=gi.initial_temperature;
        }
    ) (numlist (num-1) []) in
    randomize_nodes gi n;
    n

let new_label gi =
    let label = sprintf "L%d" !(gi.counter) in
    incr gi.counter;
    label

(* Generate some random edges *)
let generate_edges gi =
    let num = gi.initial_nodes / 2 in
    let rand_node n = List.nth n (Random.int (List.length n)) in
    let rand_weight () = foi (Random.int 3 + 1) in
    for i = 1 to num do
        let n1 = rand_node gi.nodes in
        let n2 = rand_node gi.nodes in
        let weight = rand_weight () in
        if n1.name <> n2.name then begin
            let label = new_label gi in
            let edge = {from_node= ref n1; to_node= ref n2; label=label;
                weight=weight} in
            n1.edges <- edge :: n1.edges;
        end;
    done;
    (* Point any orphaned nodes to a random node with an edge *)
    let tnodes = List.filter (fun x -> List.length x.edges > 0) gi.nodes in
    List.iter (fun n ->
        if List.length n.edges = 0 then begin
            let tnode = rand_node tnodes in
            let label = new_label gi in
            let weight = rand_weight () in
            let edge = {from_node = ref n; to_node = ref tnode; label=label;
                weight=weight} in
            n.edges <- [edge];
        end;
    ) gi.nodes

(* Iterate over other nodes *)
let iter_other_nodes gi node fn =
    List.iter (fun n ->
        if n.name <> node.name then
            fn gi node n;
    ) gi.nodes

(* Calculate replusive force on two nodes *)
let calculate_repulsion gi n1 n2 =
    let delta_pos = n1.pos -/ n2.pos in
    let mag = vec_mag delta_pos in
    if mag < gi.max_repulsion_dist then begin
        let rep_force = (gi.k *. gi.k) /. mag in
        let force_delta = delta_pos *// (rep_force /. mag) in
        n1.force <- n1.force +/ force_delta;
        n2.force <- n2.force -/ force_delta;
    end

(* Calculate attraction forces based on edges between nodes *)
let calculate_attraction edge gi n1 n2 =
    let delta_pos = n1.pos -/ n2.pos in
    let mag = min (vec_mag delta_pos) gi.max_repulsion_dist in
    if mag < 0.0001 then begin
        failwith (sprintf "ERR: edge from %s -> %s" (!(edge.from_node)).name (!(edge.to_node)).name);
        exit 1;
    end;
    let att_force = ((mag *. mag) -. (gi.k *. gi.k)) /. gi.k in
(*    let att_force = att_force *. (((log edge.weight) *. 0.2) +. 1.) in *)
    let force_delta = delta_pos *// (att_force /. mag) in
    n1.force <- n1.force -/ force_delta;
    n2.force <- n2.force +/ force_delta

(* Move node and return distance it actually moved *)
let apply_movement gi node =
    let move_delta = node.force *// gi.c in
    let rng = range (-1. *. node.temperature) (node.temperature) in
    let move = vec_map rng move_delta in
    let move_mag = vec_mag move in
    node.pos <- node.pos +/ move;
    if vec_mag move < 1.  then begin
        node.temperature <- node.temperature /. 1.03;
    end;
    move_mag

(* Return xmin, ymin, width, height of all the nodes *)
let nodes_bounding_box gi =
    let xmin, xmax, ymin, ymax = List.fold_left (fun (xmin,xmax,ymin,ymax) b ->
        let xmin = min b.pos.x xmin in
        let xmax = max b.pos.x xmax in
        let ymin = min b.pos.y ymin in
        let ymax = max b.pos.y ymax in
        (xmin, xmax, ymin, ymax)
        ) (100000.,-100000.,100000.,-100000.) gi.nodes in
    (xmin, ymin, xmax -. xmin, ymax -. ymin)

(* Find a node with a given uid *)
let find_node_from_uid gi uid =
    List.fold_left (fun a b -> if b.uid = uid then Some b else a) None gi.nodes
    
let tick_nodes gi =
    (* reset forces on nodes, revisit in future if we want inertial model *)
    List.iter (fun node ->
        node.force <- {x=0.; y=0.};
        node.temperature <- gi.initial_temperature) gi.nodes;
    (* nodes repulse each other *)
    List.iter (fun node -> iter_other_nodes gi node calculate_repulsion) gi.nodes;
    (* and attract on edges *)
    List.iter (fun node -> List.iter (fun edge ->
        calculate_attraction edge gi !(edge.from_node) !(edge.to_node);
    ) node.edges) gi.nodes;
    (* apply the movement to each node *)
    let dist = List.fold_left (fun a b -> a +. (apply_movement gi b)) 0. gi.nodes in
(*    debug "tick_nodes" (sprintf "%.2f" dist); *)
    dist
