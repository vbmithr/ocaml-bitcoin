open Core
open Async
open Bitcoin.Util
open Bitcoin.Protocol
open Bitcoin.P2p
open Log.Global

let buf = Cstruct.create 4096
let network = ref Network.Mainnet

let headers : Header.t String.Table.t = String.Table.create ()

let write_cstruct w (cs : Cstruct.t) =
  debug "write_cstruct %d %d" cs.off cs.len ;
  Writer.write_bigstring w cs.buffer ~pos:cs.off ~len:cs.len

let write_cstruct2 w cs cs2 =
  let len = cs2.Cstruct.off - cs.Cstruct.off in
  Writer.write_bigstring w cs.buffer ~pos:cs.off ~len

let process_msg w = function
  | Message.Version { version; services; timestamp; recv_services; recv_ipaddr; recv_port;
                          trans_services; trans_ipaddr; trans_port; nonce; user_agent; start_height;
                          relay } ->
    debug "Got Version %d (%s)" version user_agent ;
    let cs = Message.to_cstruct ~network:!network buf VerAck in
    write_cstruct w (Cstruct.sub buf 0 cs.off) ;
    debug "Sent VerAck"
  | VerAck ->
    debug "Got VerAck!" ;
  | Reject rej ->
    error "%s" (Format.asprintf "%a" Reject.pp rej)
  | SendHeaders ->
    let cs = CompactSize.to_cstruct_int buf (String.Table.length headers) in
    write_cstruct2 w buf cs ;
    (* String.Table.iter headers ~f:begin fun h -> *)
    (*   Header. *)
    (* end *)
  | _ ->
    debug "Got Unsupported message!"

let rec consume_cs w cs =
  let msg, cs = Message.of_cstruct cs in
  process_msg w msg ;
  if cs.Cstruct.off < cs.len then consume_cs w cs

let handle_chunk w buf ~pos ~len =
  debug "handle_chunk %d %d" pos len ;
  let cs = Cstruct.of_bigarray buf ~off:pos ~len in
  consume_cs w cs ;
  return `Continue

let main_loop port s r w =
  info "Connected!" ;
  let cs = Message.to_cstruct ~network:!network
      buf (Version (Version.create ~recv_port:port ~trans_port:port ())) in
  write_cstruct w (Cstruct.sub buf 0 cs.off) ;
  Reader.read_one_chunk_at_a_time r ~handle_chunk:(handle_chunk w) >>= function
  | `Eof ->
    info "EOF" ;
    Deferred.unit
  | `Eof_with_unconsumed_data data ->
    info "EOF with unconsumed data" ;
    Deferred.unit
  | `Stopped v ->
    info "Stopped" ;
    Deferred.unit

let set_loglevel = function
  | 2 -> set_level `Info
  | 3 -> set_level `Debug
  | _ -> ()

let main testnet host port daemon datadir rundir logdir loglevel () =
  set_loglevel loglevel ;
  if testnet then network := Network.Testnet ;
  let host = match testnet, host with
    | _, Some host -> host
    | true, None -> List.hd_exn Network.(seed Testnet)
    | false, None -> List.hd_exn Network.(seed Mainnet) in
  let port = match testnet, port with
    | _, Some port -> port
    | true, None -> Network.(port Testnet)
    | false, None -> Network.(port Mainnet) in
  stage begin fun `Scheduler_started ->
    info "Connecting to %s:%d" host port ;
    Tcp.(with_connection (to_host_and_port host port) (main_loop port))
  end

let command =
  let spec =
    let open Command.Spec in
    empty
    +> flag "-testnet" no_arg ~doc:" Use testnet"
    +> flag "-host" (optional string) ~doc:"string Hostname to use"
    +> flag "-port" (optional int) ~doc:"int TCP port to use"
    +> flag "-daemon" no_arg ~doc:" Run as a daemon"
    +> flag "-datadir" (optional_with_default "data" string) ~doc:"dirname Data directory (data)"
    +> flag "-rundir" (optional_with_default "run" string) ~doc:"dirname Run directory (run)"
    +> flag "-logdir" (optional_with_default "log" string) ~doc:"dirname Log directory (log)"
    +> flag "-loglevel" (optional_with_default 1 int) ~doc:"1-3 global loglevel"
  in
  Command.Staged.async ~summary:"Bitcoin Node" spec main

let () = Command.run command

