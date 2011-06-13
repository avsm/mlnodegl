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
 * $Id: utils.ml,v 1.4 2005/06/16 23:57:28 avsm Exp $
 *)


exception No_value
let valof = function
  | None -> raise No_value
  | Some x -> x

let may f = function
  | None -> None
  | Some x -> Some (f x)
  
let default d = function
  | None -> d
  | Some x -> x

let rec options = function
  | [] -> []
  | None::xs -> options xs
  | (Some x)::xs -> x::(options xs)

(* string -> char list *)
let rec explode s = 
  if (String.length s = 0) then []
  else (String.get s 0)::(explode (String.sub s 1 (String.length s - 1)))

(* char list -> string *)
let rec implode chars = 
  let s = String.create (List.length chars) in
  let rec setchar n = function
    | [] -> ()
    | (c::cs) -> String.set s n c; setchar (n+1) cs

  in  setchar 0 chars; s


let rec take n lst = match n with
  | 0 -> []
  | n -> (List.hd lst)::(take (n-1) (List.tl lst))

let rec drop n lst = match n with
  | 0 -> lst
  | n -> drop (n-1) (List.tl lst)

let rec range f t = if (f > t) then [] 
    else f::(range (f+1) t)

let rec zip a b = match a,b with
  | [], _ -> []
  | _, [] -> []
  | (x::xs), (y::ys) -> (x,y)::(zip xs ys)

let always finally thing = 
  try
    let result = thing () in
      finally ();
      result
  with e ->
    finally ();
    raise e


let rec make_index f = function
  | [] -> []
  | (x::xs) -> (f, x)::(make_index (f+1) xs)

(*let make_index f x = zip (range f (f + List.length x)) x*)

let foreach xs f = List.iter f (make_index 0 xs)

let exponentiate n k = List.fold_right (fun _ a -> a*n) (range 1 k) 1

type 'a ll = End
           | Item of 'a * (unit -> 'a ll)

let rec ll_of_list = function
  | [] -> End
  | x::xs -> Item(x, fun () -> ll_of_list xs)

let rec list_of_ll = function
  | End -> []
  | Item(x, xs) -> x::(list_of_ll (xs()))

let rec ll_map f = function
  | End -> End
  | Item(x, xs) -> Item(f x, fun () -> ll_map f (xs()))

let rec ll_filter f = function
  | End -> End
  | Item(x, xs) -> if f x then Item(x, fun () -> ll_filter f (xs())) else ll_filter f (xs())

class type active_object = object
  method tick : unit
  method finished : bool
end

(** Every time the method #tick is called, executes the function f with argument delta_t
    where delta_t is the time between initialisation and the current wall-clock *)
class ['a] f_of_t = fun tmax f -> object(self)
  (* record the current time *)
  
  val mutable expired = true
  val mutable delta_t = 0.0
    
  method finished = expired
    
  val mutable start_time = 0.
  initializer start_time <- Unix.gettimeofday()

  method restart = start_time <- Unix.gettimeofday(); expired <- false

  method tick = 
    (* fetch the current time *)
    let now = Unix.gettimeofday() in
      (* work out the time difference *)
      delta_t <- now -. start_time;
      (* are we done? *)
      if delta_t > tmax then expired <- true
        
  method evaluate : 'a = 
    self#tick;
    match expired with
    | false -> f (delta_t /. tmax)
    | true  -> f 1.0 
end

let linear_interpolate a b t = 
  let vector = b -. a in a +. vector *. t

(* IntSet is used to represent a set of widget ids gained through picking *)
module IntOrd = struct
    type t = int
    let compare = Pervasives.compare
end

module IntSet = Set.Make(IntOrd)

