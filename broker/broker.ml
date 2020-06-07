let wrapper_contract =
  let ch = open_in "wrapper.tz" in
  let str = really_input_string ch (in_channel_length ch) in
  close_in ch; str

module%scamltypes BrokerContractTypes = struct
  open SCaml

  type param = unit
  type spirit_hash = string
  type book_hash = string
  type template_hash = string
  type fee_descriptor =
    | FreeOfCharge
    | Charged of {
        fee_amount : tz;
        fee_collector : address;
      }

  type origination_request = {
      sprthash : string;
      template : nat;
      genparam : bytes;
      initbalance : tz;
    }

  type template_descriptor = {
      template_id  : nat;
      template_fee : fee_descriptor;
      template_availability : bool;

      ccgen: bytes;
      (** should unpack to [ccgen] *)

      bookhash : book_hash option;
      tmplhash : template_hash;
    }

  and  rclabel = string

  and  wfunc = bytes*bytes -> operation list*bytes
  [@@scaml.noconv]

  and  ccgen = {
      genprog  : (bytes ->
                  (** genesis-params *)
                  (rclabel*wfunc*bytes) list
                  (** contract ensemble *)
                 );
      initprog : (bytes ->
                  (** genesis-params *)
                  (rclabel, address) map ->
                  (** contract addresses *)
                  (rclabel, bytes) map
                  (** optional initialization message per contract *)
                 );
    }
  [@@scaml.noconv]

  type wrapper_storage = {
      wfunc    : wfunc;
      wstorage : bytes;
      avatarid : avatar_identity option;
    }
  [@@scaml.noconv]

  and avatar_identity = {
      sprthash : string;
      rclabel  : string;
    }

  type instance_descriptor = {
      sprthash   : spirit_hash;
      originator : address;
      tmplhash   : template_hash;
      ensemble   : (rclabel, address) map;
    }
  type storage = {
      broker_fee    : fee_descriptor;
      broker_admins : address set;
      broker_availability: bool;

      templates : (nat, template_descriptor) big_map;
      instances : (spirit_hash, instance_descriptor) big_map;
    }

  type message =
    | Originate of origination_request
    | SudoAddAdmin of address
    | SudoRemoveAdmin of address
    | SudoAddTemplate of template_descriptor

  let empty_storage = {
      broker_fee = FreeOfCharge;
      broker_admins = Set [];
      broker_availability = false;

      templates = BigMap [];
      instances = BigMap [];
    }
end

module%scamlcontract BrokerContract = struct
  open SCaml
  open BrokerContractTypes

  let flatten_ops (opss : operations list) =
    List.fold_left' (fun (acc, ops) -> List.rev_append (List.rev ops) acc) [] opss

  let wrap body storage0 avatarid initbalance =
    Contract.create_from_tz_file
      "wrapper.tz"
      None initbalance
      { wfunc = body; wstorage = storage0; avatarid }

  (* XXX not handling fee processing *)
  let process_origination
        template
        { sprthash; genparam; initbalance; _ } =
    let { ccgen; tmplhash; _ } = match template with
        Some x -> x | None -> failwith "112" in
    let { genprog; initprog } = match (Obj.unpack ccgen : ccgen option) with
      | None -> failwith "114"
      | Some x -> x
    in
    let rclist = genprog genparam in
    let rcolist =
      let f (rclabel, body, storage0) =
        let (op, addr) = wrap body storage0 (Some { sprthash; rclabel }) initbalance in
        (rclabel, op, addr) in
      List.map f rclist in
    let initcalls =
      let m = List.fold_left (fun acc (rclabel, _, addr) ->
                Map.update rclabel (Some addr) acc)
                Map.empty rcolist in
      initprog genparam m in
    let ops =
      let f (rclabel, op, addr) =
        let contract addr : bytes contract =
          match Contract.contract addr with
          | None -> failwith "process_origination#contract"
          | Some c -> c in
        let initop addr msg =
          Operation.transfer_tokens msg (Tz 0.) (contract addr) in
        match Map.get rclabel initcalls with
        | None -> [op]
        | Some msg -> [op; initop addr msg] in
      flatten_ops (List.map f rcolist) in
    let ensemble =
      let pairs = List.map (fun (l,_,a) -> (l,a)) rcolist in
      List.fold_left (fun acc (k,v) -> Map.update k (Some v) acc) Map.empty pairs in
    ops, tmplhash, ensemble

  let check_sudo { broker_admins; _ } =
    let src = Global.get_source() in
    if not (Set.mem src broker_admins) then failwith "sudoer check fails"
    else ()

  let main (msg : message)
        (({templates; instances; broker_admins; _} as storage) : storage) = match msg with
    | Originate ({ template; sprthash; _} as req) ->
       if BigMap.mem sprthash instances then failwith "sprthash conflict";
       let template = BigMap.get template templates in
       let (ops, tmplhash, ensemble) = process_origination template req in
       let instance = { sprthash; originator = Global.get_source();
                        tmplhash; ensemble } in
       let instances = BigMap.update sprthash (Some instance) instances in
       ops, {storage with instances}
    | SudoAddAdmin addr ->
       check_sudo storage;
       [], { storage with broker_admins = Set.update addr true broker_admins }
    | SudoRemoveAdmin addr ->
       check_sudo storage;
       [], { storage with broker_admins = Set.update addr false broker_admins }
    | SudoAddTemplate ({ template_id = id; _ } as tmpl) ->
       if BigMap.mem id templates then failwith "template id conflict";
       [], { storage with templates = BigMap.update id (Some tmpl) templates }
  [@@entry]
end

module Args = struct
  let tzout : string option ref = ref None
  let printing : [ `Nothing
                 | `WrapperContractCode
                 | `BrokerContractCode
                 | `EmptyStorage
                 ] ref = ref `Nothing

  let toprint x () = printing := x

  let speclist = [
      ("-wrapper-code", Arg.Unit (toprint `WrapperContractCode),
       "to print the wrapper contract code");
      ("-broker-code", Arg.Unit (toprint `BrokerContractCode),
       "to print the wrapper contract code");
      ("-empty-storage", Arg.Unit (toprint `EmptyStorage),
       "to print the empty storage for the broker contract");
    ]

  let usage() = Arg.usage speclist "broker.ml: Broker smart-contract coordinator\n\
                                    please specify an argument"
  let () = Arg.parse speclist print_endline "broker.ml"
end

let () =
  match !Args.printing with
  | `WrapperContractCode -> 
     wrapper_contract |> print_endline
  | `BrokerContractCode -> 
     [%scamlcontract BrokerContract] |> print_endline
  | `EmptyStorage ->
     [%scamlvalue BrokerContractTypes.empty_storage] |> print_endline
  | `Nothing -> Args.usage()
