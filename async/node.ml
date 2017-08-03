open Core
open Async
open Bitcoin

let dns_seed = "seed.bitcoin.sipa.be"

let main_loop s r w =
  let open P2p in
  Deferred.unit

let main testnet port daemon datadir rundir logdir loglevel () =
  let port = match testnet, port with
    | _, Some port -> port
    | true, None -> P2p.Network.(port Testnet)
    | false, None -> P2p.Network.(port Mainnet) in
  stage begin fun `Scheduler_started ->
    Tcp.(with_connection (to_host_and_port dns_seed port) main_loop)
  end

let command =
  let spec =
    let open Command.Spec in
    empty
    +> flag "-testnet" no_arg ~doc:" Use testnet"
    +> flag "-port" (optional int) ~doc:"int TCP port to use"
    +> flag "-daemon" no_arg ~doc:" Run as a daemon"
    +> flag "-datadir" (optional_with_default "data" string) ~doc:"dirname Data directory (data)"
    +> flag "-rundir" (optional_with_default "run" string) ~doc:"dirname Run directory (run)"
    +> flag "-logdir" (optional_with_default "log" string) ~doc:"dirname Log directory (log)"
    +> flag "-loglevel" (optional_with_default 1 int) ~doc:"1-3 global loglevel"
  in
  Command.Staged.async ~summary:"Bitcoin Node" spec main

let () = Command.run command

