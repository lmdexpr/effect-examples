open Effect
open Effect.Deep

type _ Effect.t += Print : string -> unit Effect.t
type _ Effect.t += Read : unit -> string Effect.t

let f _ =
  let _        = perform (Print "What is your forename?") in
  let forename = perform (Read ()) in
  let _        = perform (Print "What is your surname?") in
  let surname  = perform (Read ()) in
  perform (Print (forename ^ " " ^ surname))

let stdout s k = continue k @@ print_endline s
let bob k = continue k "Bob"

let _ =
  try_with f () { effc = fun (type a) (e : a Effect.t) ->
    match e with
    | Print s -> Some(fun (k : (a, _) continuation) -> stdout s k)
    | Read () -> Some(fun (k : (a, _) continuation) -> bob k)
    | _ -> None
  }
