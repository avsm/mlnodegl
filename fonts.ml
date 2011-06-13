(*
 * Copyright (c) 2004 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2004 Intel Research Cambridge
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
 * $Id: fonts.ml,v 1.5 2005/06/17 00:00:58 avsm Exp $
 *)

(* djs's observation: GLTRANSLATES occur in two places:
 *   at the beginning of a char with x=y=z=0
 *   at the end of a char with x<>0 and y=z=0 corresponding to a 'kerning' value. 
 *)


type font_entry = { mutable dlist: GlList.t; mutable width: float }

let last_translate_x = ref 0. (* x-coord of last GLTRANSLATE *)

let string_of_list_of_string l =
    String.concat "|" l

let string_of_list_of_float l =
    String.concat "," (List.map string_of_float l)

let string_of_tuple_of_float (x,y) =
    (string_of_float x) ^ "," ^ (string_of_float y)

let proc_dbl_args args =
    let nums = List.map float_of_string args in
    let y = List.tl nums in
    let z = List.tl y in
    (List.hd nums, List.hd y, List.hd z)

let parse_glbegin_type t =
    match t with 
    |"POINTS" -> `points
    |"LINES" -> `lines
    |"LINE_LOOP" -> `line_loop
    |"LINE_STRIP" -> `line_strip
    |"TRIANGLES" -> `triangles
    |"TRIANGLE_STRIP" -> `triangle_strip
    |"TRIANGLE_FAN" -> `triangle_fan
    |"QUADS" -> `quads
    |"QUAD_STRIP" -> `quad_strip
    |"POLYGON" -> `polygon
    |_ -> failwith ("didnt recognize gltype " ^ t)

let current_char = ref ' '
let current_dlist = ref None

let parse_font_line hash str =
    let bits = Str.split (Str.regexp " ") str in
    let cmd = List.hd bits in
    let args = List.tl bits in
    match cmd with
    | "STARTCHAR" ->
        let letter = List.hd args in
        let curchar = (
            if (String.length letter > 0) then
            letter.[0]
            else
            ' ') in
        current_char := curchar;
        current_dlist := Some (GlList.create `compile);
    | "ENDCHAR" -> 
        begin match !current_dlist with
            | Some x -> 
                GlList.ends ();
                let entry = { dlist = x; width = !last_translate_x } in
                Hashtbl.add hash !current_char entry
            | None ->
                Printf.printf "Error: not building a display list properly\n"
        end
    | "GLBEGIN" ->
        let gltype = parse_glbegin_type (List.hd args) in
        GlDraw.begins gltype
    | "GLEND" -> GlDraw.ends ()
    | "GLPUSHMATRIX" -> ()
    | "GLPOPMATRIX" -> ()
    | "GLVERTEX" -> GlDraw.vertex3 (proc_dbl_args args)
    | "GLTRANSLATE" -> 
        let x,y,z = proc_dbl_args args in
        last_translate_x := x
    | _ -> failwith ("unknown: " ^ cmd)

let fonts = Hashtbl.create 100
let outlines = Hashtbl.create 100

let aa_render_char c =
    let entry1 = Hashtbl.find fonts c in
    let entry2 = Hashtbl.find outlines c in
    GlDraw.line_width 0.5;
       GlList.call entry1.dlist;
       Gl.enable `line_smooth;
       GlList.call entry2.dlist;
       GlMat.translate3(entry2.width, 0., 0.);
       GlDraw.line_width 1.0

let aa_render_string alpha col s =
    GlMat.push ();
    GlDraw.color ~alpha:alpha col;
    String.iter aa_render_char s;
    GlMat.pop ()

let string_width s =
    List.fold_left (fun total char -> total +.
        (Hashtbl.find outlines char).width) 0. (Utils.explode s)

let init_fonts fname =
    let ffill = open_in (fname ^ ".fill") in
    let fout = open_in (fname ^ ".outlines") in
    try while true do parse_font_line fonts (input_line ffill) done
    with End_of_file -> close_in ffill;
    try while true do parse_font_line outlines (input_line fout) done
    with End_of_file -> close_in fout
