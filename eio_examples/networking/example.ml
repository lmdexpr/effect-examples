open Eio.Std

(* ref. https://github.com/ocaml-multicore/eio#networking *)

let run_client ~net ~addr =
  traceln "Connecting to server...";
  let flow sw  = Eio.Net.connect ~sw net addr in
  let greet sw = Eio.Flow.copy_string "Hello from client" (flow sw) in
  Switch.run greet

let run_server socket =
  let server flow _addr =
    traceln "Server accepted connection from client";
    let b = Buffer.create 100 in
    Eio.Flow.copy flow (Eio.Flow.buffer_sink b);
    traceln "Server received: %S" (Buffer.contents b)
  and on_error = traceln "Error handling connection: %a" Fmt.exn in
  let server sw = Eio.Net.accept_fork socket ~sw server ~on_error in
  Switch.run server;
  traceln "(normally we'd loop and accept more connections here)"

let main ~net ~addr sw =
  let server = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:5 addr in
  traceln "Server ready...";
  Fiber.both
    (fun () -> run_server server)
    (fun () -> run_client ~net ~addr)

let main env =
  let net  = Eio.Stdenv.net env
  and addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080) in
  Switch.run @@ main ~net ~addr

let () = Eio_main.run main
