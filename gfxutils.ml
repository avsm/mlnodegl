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
 * $Id: gfxutils.ml,v 1.4 2005/06/17 00:00:58 avsm Exp $
 *)

(** Miscellaneous OpenGL utility and drawing functions *)

open Debug

(** Stateful wrapper for a display list. The constructor is supplied 
    a "build_fn" which is executed by the "create" method called     
    before the first call of the list. Note list is created          
    automagically on first use.                                      *)
 
class display_list build_fn = object(self)
    val mutable dlist: GlList.t option = None
    
    method create = (* called after initialisation to create the list *)
        match dlist with
        | Some _ -> () (* done already *)
        | None ->
            let list = GlList.create `compile in 
            Utils.always GlList.ends build_fn;
            dlist <- Some list
                         
    method get = match dlist with
        | None ->
            self # create;
            begin
                match dlist with
                | None -> failwith "Failed to initialise displaylist"
                | Some dl -> dl
            end
        | Some dl -> dl

    method call = (* called to "execute" the display list *)
        let dl = self#get
        in GlList.call dl

    method execute_immediately = ignore(build_fn ())
end

(** Handle the (limited) OpenGL matrix stack. Apparently there are no warnings for 
    pushing too many objects on the stack.                                         *)
let matrix_stack_depth = ref 0
let matrix_stack_limit = 16 (* a guess *)

let push fn = 
    matrix_stack_depth := !matrix_stack_depth + 1;
    if !matrix_stack_depth > matrix_stack_limit then
        debug "push" "WARNING: matrix stack exceeding safe limit";
    GlMat.push ();
    Utils.always (fun () -> GlMat.pop();
                 matrix_stack_depth := !matrix_stack_depth - 1) fn

(* Push name onto the name stack for picking *)
let pushname name fn = 
    GlMisc.push_name name;
    Utils.always GlMisc.pop_name fn
    

(* Wrapper around the low-level picking routines. Returns a list of names found 
 * while executing render_fn *)
let pick render_fn = 
    let array = Raw.create_static `uint 20000 in
    (* Make sure the static array is always freed *)
    Utils.always (fun () -> Raw.free_static array) 
        (fun () ->
            (* Make sure we never leave the render_mode in `select otherwise it might *)
            (* scribble over the Raw array we free'd                                  *)
            Utils.always (fun () -> ignore(GlMisc.render_mode `render))
                (fun () ->
                    GlMisc.select_buffer array;
                    ignore(GlMisc.render_mode `select);
                    
                    GlMisc.init_names ();
                    GlMisc.push_name 0;
                    (* render the scene here *)
                    render_fn ();

                    (* examine the data in the array *)
                    let read_hit array index = 
                        let numnames = Raw.get array (index + 0)
                        and zmin     = Raw.get array (index + 1)
                        and zmax     = Raw.get array (index + 2) in
                        let names = List.map (fun x -> Raw.get array ~pos:x) (Utils.range (index + 3) (index + 3 + numnames - 1))in
                        index + 3 + numnames, names (* ignore everything else for now *)
                        in
                        let num_hits = GlMisc.render_mode `render in
                        let _,all_names = List.fold_left 
                            (fun (i, acc) _ -> 
                                let i', acc' = read_hit array i
                                in i', acc @ acc') (0, []) 
                            (Utils.range 1 num_hits) in
                        all_names
                )
        )

(* A source of globally-unique names for graphical elements *)
let id = ref 1000 
let next_id () = 
    let result = !id in
    id := !id + 1;
    result

(* A list of idle tasks which are executed in an idleFunc. The idleFunc is created and
 * destroyed on demand. *)
module Idle = struct
    let active_objects = ref []
    
    let idle () = 
        List.iter (fun x -> x#tick) !active_objects;
        active_objects := List.filter (fun x -> not(x#finished)) !active_objects;
        Glut.postRedisplay();
        if !active_objects = [] then Glut.idleFunc None
    
    let add (x: Utils.active_object) = 
        if not(List.mem x !active_objects) then active_objects := x::!active_objects;
        if !active_objects <> [] then Glut.idleFunc ~cb:(Some idle)
end
