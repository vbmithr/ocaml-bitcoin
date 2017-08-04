open Core
open Async
open Bitcoin
open Log.Global

let network = ref P2p.Network.Mainnet

let write_cstruct w (cs : Cstruct.t) =
  Writer.write_bigstring w cs.buffer ~pos:cs.off ~len:cs.len

let process_msg = function
  | P2p.Message.Version v ->
    debug "%s" "Got Version!" ;
  | _ -> ()

let rec consume_cs cs =
  let open P2p in
  let msg, cs = Message.of_cstruct cs in
  process_msg msg ;
  if cs.Cstruct.off < cs.len then consume_cs cs

let handle_chunk buf ~pos ~len =
  debug "handle_chunk %d %d" pos len ;
  let open P2p in
  let cs = Cstruct.of_bigarray buf ~off:pos ~len in
  consume_cs cs ;
  return `Continue

let buf = Cstruct.create 4096

let main_loop port s r w =
  let open P2p in
  info "Connected!" ;
  let cs = Message.to_cstruct ~network:!network
      buf (Version (Version.create ~recv_port:port ~trans_port:port ())) in
  write_cstruct w (Cstruct.sub buf 0 cs.off) ;
  Reader.read_one_chunk_at_a_time r ~handle_chunk >>= function
  | `Eof -> Deferred.unit
  | `Eof_with_unconsumed_data data -> Deferred.unit
  | `Stopped v -> Deferred.unit

let main testnet port daemon datadir rundir logdir loglevel () =
  if testnet then network := P2p.Network.Testnet ;
  let host = match testnet with
    | true -> List.hd_exn P2p.Network.(seed Testnet)
    | false -> List.hd_exn P2p.Network.(seed Mainnet) in
  let port = match testnet, port with
    | _, Some port -> port
    | true, None -> P2p.Network.(port Testnet)
    | false, None -> P2p.Network.(port Mainnet) in
  stage begin fun `Scheduler_started ->
    info "Connecting to %s:%d" host port ;
    Tcp.(with_connection (to_host_and_port host port) (main_loop port))
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

