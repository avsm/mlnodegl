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
 * $Id: mouse.ml,v 1.6 2005/06/17 00:00:58 avsm Exp $
 *)


(* Mouse handling code *)

(* A generic thing which can receive Glut mouse events, returning true if they are 'absorbed' *)
class type mouse_handler = object
    method mouseFunc : button:Glut.button_t -> state:Glut.mouse_button_state_t -> x:int -> y:int -> unit -> bool
    method motionFunc : x:int -> y:int -> unit -> bool
    method hoverFunc : x:int -> y:int -> unit -> bool
    method name : string
end

(* Exactly one of these is created which keeps a list of delegate mouse_handlers objects and
 * which tries each one in turn until a hander decides to absorb the event *)
class global_mouse_handler_t = 
    (* really a fold_left (&&) true (lazy_map .. ) *)
    let rec dispatch = function
    | [] -> ()
    | (f::fs) -> if (f ()) then () else dispatch fs in
    
    (* This was to debug code which forgot to remove temporary handlers eg mouse grab for dragging *)
    let debug_wrap d result () = 
    let r = result () in
          if r then begin
          (* Printf.printf "dispatch stopped by %s\n" (d#name); *)
          flush stdout
      end;
      r
      
    in fun (delegates : mouse_handler list) -> object(self)

    val mutable delegates = delegates
    
    method dump = 
        print_endline "global mouse handler delegates:";
        List.iter print_endline (List.map (fun x -> "  " ^ x#name) delegates) ;
        flush stdout
    
    method push d = delegates <- d::delegates
    method pop () = delegates <- List.tl delegates

    method mouseFunc ~button:b ~state:state ~x:x ~y:y = 
        dispatch (List.map (fun f -> debug_wrap f (f#mouseFunc ~button:b ~state:state ~x:x ~y:y)) delegates);
        Glut.postRedisplay()

    method motionFunc ~x:x ~y:y =
        dispatch (List.map (fun f -> f#motionFunc ~x:x ~y:y) delegates)
        
    method passiveMotionFunc ~x:x ~y:y =
        dispatch (List.map (fun f -> f#hoverFunc ~x:x ~y:y) delegates)
    
end

(* Here is the singleton instance: *)
let global_mouse_handler = new global_mouse_handler_t []

(* Helper class for mouse dragging. Subclass and overrride virtual methods *)
(* This one is started by a click... this was used to rotate the switch when the user
 * clicked and dragged on the empty background. *)
class virtual mouse_drag = fun (button: Glut.button_t) -> object(self)
  
    val mutable dragging = false
    val mutable init_x = 0
    val mutable init_y = 0

    method virtual start : unit (* dragging started *)
    method virtual delta : int -> int -> unit (* drag to here so far *)
    method virtual stop : unit (* dragging stopped *)

    method mouseFunc ~button:b ~state:state ~x:x ~y:y () = 
        if b <> button then false else begin
            begin match state with
            | Glut.DOWN ->
                init_x <- x; init_y <- y;
                dragging <- true;
                self#start;
            | Glut.UP ->
                dragging <- false;
                self#stop;
            end;
            true
        end

    method hoverFunc ~x:(x:int) ~y:(y:int) () = false
    
    method motionFunc ~x:x ~y:y () =
        if dragging then begin
            self#delta (x - init_x) (y - init_y); true
        end else false
end

(* Like mouse_drag only starting from a particular place, e.g. a menu bar *)
class virtual drag_from_here = fun x y (button: Glut.button_t) -> object(self)
    val mutable running = true

    method mouseFunc ~button:b ~state:(state:Glut.mouse_button_state_t) ~x:(x:int) ~y:(y:int) () = 
        if b <> button then false else begin
            running <- false;
            self#stop;
            true
        end

    method hoverFunc ~x:(x:int) ~y:(y:int) () = false

    method motionFunc ~x:x' ~y:y' () =
        if not(running) then false else begin
        self#delta (x' - x) (y' - y); 
        true
        end
        
    method virtual delta : int -> int -> unit
    method virtual stop : unit
end


class virtual mouse_click = fun (button: Glut.button_t) -> object(self)

  method mouseFunc ~button:b ~state:state ~x:x ~y:y () = 
    if b <> button then false else
      match state with
        | Glut.DOWN ->
            self#click x y;
        | Glut.UP -> false

  method hoverFunc ~x:(x:int) ~y:(y:int) () = false

  method motionFunc ~x:(x:int) ~y:(y:int) () = false
  method virtual click : int -> int -> bool

end

class virtual mouse_over = object(self)

    method mouseFunc ~button:(b:Glut.button_t) ~state:(state:Glut.mouse_button_state_t)
       ~x:(x:int) ~y:(y:int) () = false

    method motionFunc ~x:(x:int) ~y:(y:int) () = false
        
    method hoverFunc ~x:(x:int) ~y:(y:int) () =
        self#hover x y;
    
    method virtual hover: int -> int -> bool    
end
