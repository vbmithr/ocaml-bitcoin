open Core
open Async
open Bitcoin.Util
open Bitcoin.Protocol
open Bitcoin.P2p
open Log.Global

let headers = Hash256.Table.create 13
let best_hh = ref Header.genesis_hash

let buf = Cstruct.create 4096
let network = ref Network.Mainnet

let my_addresses =
  Base58.Bitcoin.of_string_exn c "mjVrE2kfz42sLR5gFcfvG6PwbAjhpmsKnn"

let write_cstruct w (cs : Cstruct.t) =
  (* debug "write_cstruct %d %d" cs.off cs.len ; *)
  Writer.write_bigstring w cs.buffer ~pos:cs.off ~len:cs.len

let write_cstruct2 w cs cs2 =
  let len = cs2.Cstruct.off - cs.Cstruct.off in
  Writer.write_bigstring w cs.buffer ~pos:cs.off ~len

let request_hdrs w start =
  let msg =
    Message.GetHeaders (GetHashes.create [start]) in
  let cs = Message.to_cstruct ~network:!network buf msg in
  write_cstruct2 w buf cs ;
  debug "Sent GetHeaders"

let load_filter w data =
  let filterload = FilterLoad.of_data data Update_none in
  let msg = Message.FilterLoad filterload in
  let cs = Message.to_cstruct ~network:!network buf msg in
  write_cstruct2 w buf cs ;
  debug "Sent FilterLoad"

let get_data w invs =
  let msg = Message.GetData invs in
  let cs = Message.to_cstruct ~network:!network buf msg in
  write_cstruct2 w buf cs ;
  debug "Sent GetData"

let process_error _w header =
  sexp ~level:`Error (MessageHeader.sexp_of_t header)

let process_msg w msg =
  (* sexp ~level:`Debug (Message.sexp_of_t msg) ; *)
  match msg with
  | Message.Version _ ->
    let cs = Message.to_cstruct ~network:!network buf VerAck in
    write_cstruct2 w buf cs ;
    debug "Sent VerAck"
  | VerAck ->
    debug "Got VerAck!" ;
    let data = [Cstruct.of_string my_addresses.payload] in
    load_filter w data
    (* get_data w [Inv.filteredblock (Hash256.of_hex_rpc (`Hex "00000000000007650b584bdba841c87876c9536953fe29ddd1a9107f0f25e486"))] *)
    (* Requesting headers *)
    (* request_hdrs w Header.genesis_hash *)
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
  | Pong _ ->
    debug "Got Pong!" ;
  | GetBlocks _ ->
    debug "Got GetBlocks!"
  | GetData _ ->
    debug "Got GetData!"
  | GetHeaders _ ->
    debug "Got GetHeaders!"
  | Block _ ->
    debug "Got Block!"
  | MerkleBlock mblock ->
    debug "MerkleBlock %s" (Sexplib.Sexp.to_string_hum (MerkleBlock.sexp_of_t mblock))
  | Headers hdrs ->
    List.iteri hdrs ~f:begin fun _i h ->
      let hh = Header.hash256 h in
      (* debug "Got block header %d: %s" i (Hash256.show hh) ; *)
      Hash256.Table.add headers hh h ;
      best_hh := hh
    end ;
    debug "headers table has %d entries" (Hash256.Table.length headers) ;
    if List.length hdrs = 2000 then
      request_hdrs w !best_hh
  | Inv invs ->
    List.iter invs ~f:begin fun inv ->
      debug "Inv %s" (Sexplib.Sexp.to_string_hum (Inv.sexp_of_t inv))
    end
  | NotFound _ ->
    debug "Got NotFound!"
  | MemPool ->
    debug "Got MemPool!"
  | Tx _ ->
    debug "Got Tx!"
    (* debug "%s" (Sexplib.Sexp.to_string_hum (Transaction.sexp_of_t tx)) *)
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
    if Cstruct.length cs_payload < hdr.size then
      return (`Consumed (0, `Need msg_size))
    else
      match Message.of_cstruct cs with
      | Error (Invalid_checksum h), _ ->
        process_error w h ;
        return (`Stop ())
      | Ok (_, msg), _ ->
        process_msg w msg ;
        return (`Consumed (msg_size, `Need_unknown))

let main_loop port _s r w =
  info "Connected!" ;
  let cs = Message.to_cstruct ~network:!network
      buf (Version (Version.create ~recv_port:port ~trans_port:port ())) in
  write_cstruct w (Cstruct.sub buf 0 cs.off) ;
  Reader.read_one_chunk_at_a_time r ~handle_chunk:(handle_chunk w) >>= function
  | `Eof ->
    info "EOF" ;
    Deferred.unit
  | `Eof_with_unconsumed_data _data ->
    info "EOF with unconsumed data" ;
    Deferred.unit
  | `Stopped _ ->
    info "Stopped" ;
    Deferred.unit

let set_loglevel = function
  | 2 -> set_level `Info
  | 3 -> set_level `Debug
  | _ -> ()

let main testnet host port _daemon _datadir _rundir _logdir loglevel () =
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
    Tcp.(with_connection
           Where_to_connect.(of_host_and_port (Host_and_port.create ~host ~port))
           (main_loop port))
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
  Command.Staged.async_spec ~summary:"Bitcoin Node" spec main

let () = Command.run command

