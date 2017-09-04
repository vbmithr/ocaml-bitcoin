open Core
open Async
open Bitcoin.Util
open Bitcoin.Protocol
open Bitcoin.P2p
open Log.Global

module HGraph = Graph.Imperative.Digraph.Concrete(Header)
let headers = HGraph.create ()
let best_header : Header.t option ref = ref None

let buf = Cstruct.create 4096
let network = ref Network.Mainnet

let write_cstruct w (cs : Cstruct.t) =
  (* debug "write_cstruct %d %d" cs.off cs.len ; *)
  Writer.write_bigstring w cs.buffer ~pos:cs.off ~len:cs.len

let write_cstruct2 w cs cs2 =
  let len = cs2.Cstruct.off - cs.Cstruct.off in
  Writer.write_bigstring w cs.buffer ~pos:cs.off ~len

let process_error w header =
  sexp ~level:`Error (MessageHeader.sexp_of_t header)

let process_msg w header msg =
  sexp ~level:`Debug (Message.sexp_of_t msg) ;
  match msg with
  | Message.Version { version; services; timestamp; recv_services; recv_ipaddr; recv_port;
                      trans_services; trans_ipaddr; trans_port; nonce; user_agent; start_height;
                      relay } ->
    let cs = Message.to_cstruct ~network:!network buf VerAck in
    write_cstruct2 w buf cs ;
    debug "Sent VerAck"
  | VerAck ->
    debug "Got VerAck!" ;
  | Reject rej ->
    error "%s" (Format.asprintf "%a" Reject.pp rej)
  | SendHeaders ->
    debug "Got SendHeaders!" ;
    (* let nb_headers = String.Table.length headers in *)
    (* let cs = CompactSize.to_cstruct_int buf nb_headers in *)
    (* write_cstruct2 w buf cs ; *)
    (* String.Table.iter headers ~f:begin fun h -> *)
    (*   let cs = Header.to_cstruct buf h in *)
    (*   write_cstruct2 w buf cs *)
    (* end ; *)
    (* debug "Sent %d headers" nb_headers *)
  | SendCmpct t ->
    sexp ~level:`Debug (SendCmpct.sexp_of_t t) ;
  | GetAddr ->
    debug "Got GetAddr!" ;
  | Addr _ ->
    debug "Got Addr!" ;
  | Ping i ->
    debug "Got Ping!" ;
    let cs = Message.to_cstruct ~network:!network buf (Pong i) in
    write_cstruct2 w buf cs ;
    debug "Sent Pong" ;
  | Pong i ->
    debug "Got Pong!" ;
  | GetBlocks _ ->
    debug "Got GetBlocks!"
  | GetData _ ->
    debug "Got GetData!"
  | GetHeaders _ ->
    debug "Got GetHeaders!"
  | Block _ ->
    debug "Got Block!"
  | MerkleBlock _ ->
    debug "Got MerkleBlock!"
  | Headers _ ->
    debug "Got Headers!"
  | Inv invs ->
    List.iter invs ~f:begin fun inv ->
      sexp ~level:`Debug (Inv.sexp_of_t inv)
    end
  | NotFound _ ->
    debug "Got NotFound!"
  | MemPool ->
    debug "Got MemPool!"
  | Tx _ ->
    debug "Got Tx!"
  | FeeFilter fee ->
    debug "Got FeeFilter: %Ld" fee
  | FilterAdd _ ->
    debug "Got FilterAdd!"
  | FilterClear ->
    debug "Got FilterClear!"
  | FilterLoad _ ->
    debug "Got FilterLoad!"

let handle_chunk w buf ~pos ~len =
  (* debug "consume_cs %d %d" pos len ; *)
  if len < MessageHeader.size then
    return (`Consumed (0, `Need MessageHeader.size))
  else
    let cs = Cstruct.of_bigarray ~off:pos ~len buf in
    let hdr, cs_payload = MessageHeader.of_cstruct cs in
    let msg_size = MessageHeader.size + hdr.size in
    if Cstruct.len cs_payload < hdr.size then
      return (`Consumed (0, `Need msg_size))
    else
      match Message.of_cstruct cs with
      | Error (Invalid_checksum h), cs ->
        process_error w h ;
        return (`Stop ())
      | Ok (_, msg), cs ->
        process_msg w hdr msg ;
        return (`Consumed (msg_size, `Need_unknown))

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

