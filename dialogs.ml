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
 * $Id: dialogs.ml,v 1.8 2005/06/17 23:17:42 avsm Exp $
 *)

(** Definitions of some dialogs/ windows are in here *)

open Gfxutils
open Utils
open Colours

let debug = Debug.debug "dialogs"

(* Boolean flag toggled with the spacebar *)
let trace_packets = ref false

(* Window which shows the status *)
let status_dialog = 
    let cols, rows = 2, 5 in object(self)
    inherit Widgets.table 2 5 as super (* whole thing is a table *)
    
    val connection = new Widgets.text ""
    val packet_display = new Widgets.text ""
    val mode = new Widgets.text ""
    val time = new Widgets.text ""
    val flow = new Widgets.text ""
    
    initializer   
        self#set_data 0 0 (new Widgets.text "Connection:" :> Widgets.widget);
        self#set_data 0 1 (new Widgets.text "Packet display:" :> Widgets.widget);
        self#set_data 0 2 (new Widgets.text "Mode:" :> Widgets.widget);
        self#set_data 0 3 (new Widgets.text "Time:" :> Widgets.widget);
        self#set_data 0 4 (new Widgets.text "Flow:" :> Widgets.widget);
        
        self#set_data 1 0 (connection :> Widgets.widget);
        self#set_data 1 1 (packet_display :> Widgets.widget);
        self#set_data 1 2 (mode :> Widgets.widget);
        self#set_data 1 3 (time :> Widgets.widget);
        self#set_data 1 4 (flow :> Widgets.widget);
        
        for row = 0 to rows - 1 do
            self#set_hjustify 0 row Widgets.Max;
            self#set_hjustify 1 row Widgets.Min;
            self#add_borders 0 row [ Widgets.Right ]
        done;

        (* Add a border to every cell -- makes it look nicer *)
        self#map_contents (may (fun x -> (new Widgets.border 0.5 x :> Widgets.widget)))

    method redraw = 
        (* Every time we redraw, make sure the controls are current *)
        connection#set_text ("none");
        packet_display#set_text (if !trace_packets then "on" else "off");
        mode#set_text("single-stepping");
        time#set_text("1");
        flow#set_text("none");
        super#redraw
end

let keys_dialog  =
    let lines = [
        "i", "reinitialize nodes";
        "r", "randomize positions";
        "t", "reset temperatures";
        "q", "quit";
    ] in
    let r,g,b,a = lightblue in
    let rows, cols = List.length lines + 1, 2 in
    let table = new Widgets.table cols rows in
    table#set_data 0 0 (new Widgets.text "keys" :> Widgets.widget);
    table#set_hjustify 0 0 Widgets.Max;
    table#set_vjustify 0 0 Widgets.Center;

    table#set_data 1 0 (new Widgets.text "description" :> Widgets.widget);
    table#set_hjustify 1 0 Widgets.Min;
    table#set_vjustify 1 0 Widgets.Center;

    foreach lines (fun (index, (key, desc)) ->
                    table#set_data 0 (index+1) (new Widgets.text key :> Widgets.widget);
                    table#set_data 1 (index+1) (new Widgets.text desc :> Widgets.widget);

                    table#add_borders 0 (index+1) [ Widgets.Right ];
                    
                    table#set_hjustify 0 (index+1) Widgets.Max;
                    table#set_hjustify 1 (index+1) Widgets.Min);

    table#add_borders 0 0 [ Widgets.Bottom ];
    table#add_borders 1 0 [ Widgets.Bottom ];
    
    (* Add a border to every cell *)
    table#map_contents (may (fun x -> (new Widgets.border 0.5 x :> Widgets.widget)));
    
    (table :> Widgets.widget)
(* Entirely static window which shows the keys used to control the simulator *)
let help_dialog = 
     let lines = [ "'1' '2' '3' '4'", "change flow pattern";
                   "up/down arrow", "increase/decrease probability";
                   "m", "animate switch folding/unfolding";
                   "space", "toggle display of packets";
                   "p", "cycle through Null insertion policies";
                   "n", "advance time by 1 step";
                   "r", "toggle continuous mode";
                   "h", "another help window";
                   "q", "quit" ] in

    let r,g,b,a = lightblue  in
    let rows, cols = List.length lines + 1, 2 in
    let table = new Widgets.table cols rows in
    
    table#set_data 0 0 (new Widgets.text "keys" :> Widgets.widget);
    table#set_hjustify 0 0 Widgets.Max;
    table#set_vjustify 0 0 Widgets.Center;

    table#set_data 1 0 (new Widgets.text "description" :> Widgets.widget);
    table#set_hjustify 1 0 Widgets.Min;
    table#set_vjustify 1 0 Widgets.Center;

    foreach lines (fun (index, (key, desc)) ->
                    table#set_data 0 (index+1) (new Widgets.text key :> Widgets.widget);
                    table#set_data 1 (index+1) (new Widgets.text desc :> Widgets.widget);

                    table#add_borders 0 (index+1) [ Widgets.Right ];
                    
                    table#set_hjustify 0 (index+1) Widgets.Max;
                    table#set_hjustify 1 (index+1) Widgets.Min);

    table#add_borders 0 0 [ Widgets.Bottom ];
    table#add_borders 1 0 [ Widgets.Bottom ];
    
    (* Add a border to every cell *)
    table#map_contents (may (fun x -> (new Widgets.border 0.5 x :> Widgets.widget)));
    
    (table :> Widgets.widget)


    


