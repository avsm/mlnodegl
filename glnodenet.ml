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
 * $Id: glnodenet.ml,v 1.1 2005/06/17 23:17:03 avsm Exp $
 *)

open Unix
open Utils
open Printf

let string_of_sockaddr = function
  |ADDR_UNIX path ->
    sprintf "ADDR_UNIX(%s)" path
  |ADDR_INET (server_addr, port) -> 
    sprintf "ADDR_INET(%s,%d)" (string_of_inet_addr server_addr) port      

let with_mutex mutex fn =
        Mutex.lock mutex;
        try
            let x = fn () in
            Mutex.unlock mutex;
            x
        with _ as e -> begin
            Mutex.unlock mutex;
            raise e
        end

class glnodenet = object(self)
    
    val mutex = Mutex.create ()
    val mutable queue = []

    method init port =
        let s = socket PF_INET SOCK_STREAM 0 in
        setsockopt s SO_REUSEADDR true;
        bind s (ADDR_INET(inet_addr_loopback,port));
        ignore(Thread.create self#accept_loop s);
        ()
    
    method queue_size =
        with_mutex mutex (fun () -> List.length queue)
    
    method push v =
        with_mutex mutex (fun () -> queue <- v :: queue)
    
    method pop =
        with_mutex mutex (fun () ->
            match queue with
            |x::xs ->
                queue <- xs;
                Some x
            |[] ->
                None
        )

    method parse_input line =
        self#push line

    method accept_loop sock =
        listen sock 5;
        while true do
            print_endline "Waiting for connection...";
            let s, caller = accept sock in
            print_endline ("Connection from: " ^ string_of_sockaddr caller);
            let chan = in_channel_of_descr s in
            try while true do
                self#parse_input (input_line chan)
            done with _ -> begin
                close_in chan;
            end;
            print_endline "Connection closed";
        done

end