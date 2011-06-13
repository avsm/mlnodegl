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
 * $Id: fp.ml,v 1.1 2005/06/16 22:35:27 avsm Exp $
 *)

(* This application uses relatively small numbers so 31-bit int
   is fine, conversion to int32/int64 is easy *)
   
module FP = struct
    type t = int
    let precision = 8
    
    let float_conv = 2. ** (float_of_int precision)
    let int_conv = int_of_float float_conv
    
    let fp_of_float f =
        int_of_float (f *. float_conv)
    
    let float_of_fp fp =
        (float_of_int fp) /. float_conv

    let add a b =
        a + b
    
    let sub a b =
        a - b
    
    (* (a * b) >> precision *)
    let mul a b =
        (a * b) / int_conv
    
    (* (a << precision) / b *)
    let div a b =
        (a * int_conv) / b

end

module FP_Regress = struct

    open FP
    open Printf
    
    let tests = [
        "add", "+", ( +.), add, 
            [2.,3.,5.; 56.,4.,60.; 354354.35,22.342,354376.692];
        "sub", "-", ( -.), sub,
            [3.,2.,1.; 1.,1.,0.; 4245.23,1334.21,2911.02];
        "mul", "*", ( *.), mul,
            [1.,1.,1.; 1.,2.,2.; 0.,2.,0.; 20.56,127.89,2629.4184;
             2345.67,1234.56,2895870.3552];
        "div", "/", ( /.), div,
            [1.,1.,1.; 0.,2.,0.; 12345.56, 2345.56, 5.263374204881];
    ]
    
    let run () =
        let o = print_endline in
        List.iter (fun (name, op, fn, fpfn, vals) ->
            List.iter (fun (a,b,res) ->
                let fpa = fp_of_float a in
                let fpb = fp_of_float b in
                let fpres = fpfn fpa fpb in
                let fpres_fl = float_of_fp fpres in
                let diff = res -. fpres_fl in
                o (sprintf "%f%s%f (good=%f,fp=%f)\ndiff=%f\n"
                    a op b res fpres_fl diff);
            ) vals;
        ) tests
    
end