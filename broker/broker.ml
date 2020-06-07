let wrapper_contract =
  let ch = open_in "wrapper.tz" in
  let str = really_input_string ch (in_channel_length ch) in
  close_in ch; str

module%scamltypes BrokerContractTypes = struct
  open SCaml

  type spirit_hash = string
  type book_hash = string
  type template_hash = bytes
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
      broker   : address;
      sprthash : string;
      rclabel  : string;
    }

  type instance_descriptor = {
      sprthash   : spirit_hash;
      originator : address;
      tmplhash   : template_hash;
      ensemble   : (rclabel, address) map;
    }

  type _storage = {
      broker_fee    : fee_descriptor;
      broker_admins : address set;
      broker_availability: bool;

      templates : nat;
      instances : nat;
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
    | SudoOriginate of origination_request

    | SudoAddAdmin of address
    | SudoRemoveAdmin of address
    | SudoUploadTemplate of {
        tmplid : nat;
        ccgen  : bytes;
      }

    | SudoUpdateBroker of {
        new_fee : fee_descriptor option;
        new_availability : bool option;
      }
    | SudoUpdateTemplate of {
        template : nat;
        new_fee  : fee_descriptor option;
        new_availability : bool option;
        new_bookhash     : book_hash option option;
      }

    | SudoWithdraw of {
        amount    : tz;
        collector : address;
      }
end

let init_storage admins =
  BrokerContractTypes.{
      broker_fee = FreeOfCharge;
      broker_admins = admins;
      broker_availability = false;

      templates = BigMap [];
      instances = BigMap [];
  }

module Printers = struct
  open SCaml
  let pp_address ppf =
    function Address addr ->
      Format.pp_print_string ppf addr

  let pp_tz ppf =
    function Tz tz ->
      Format.fprintf ppf "%gtz" tz
  let pp_nat ppf =
    function Nat x ->
      Format.fprintf ppf "%i" x

  type fee_descriptor = BrokerContractTypes.fee_descriptor =
    | FreeOfCharge
    | Charged of {
        fee_amount : tz;
        fee_collector : address;
      }
  [@@deriving show { with_path = false }]

  type scamlbytes = SCaml.bytes
  let pp_scamlbytes ppf =
    function Bytes b -> Format.fprintf ppf "<<%s>>" b

  type spirit_hash = string [@@deriving show]
  type book_hash = string [@@deriving show]
  type rclabel = string [@@deriving show]
  type template_hash = scamlbytes [@@deriving show]

  let pp_list_contents ?sep:(sep=',') epp ppf list =
    Format.(
      Stdlib.List.iter
        (fun x -> fprintf ppf "%a%c" epp x sep; pp_print_space ppf ())
        list)

  let pp_map0 (kpp, vpp) ppf m =
      Format.(
        let epp ppf (k, v) =
          let sp () = pp_print_space ppf () in
          kpp ppf k; sp(); fprintf ppf "->"; sp(); vpp ppf v in
        fprintf ppf "{ %a }" (pp_list_contents ~sep:';' epp) m)

  let pp_map (kpp, vpp) ppf = function Map m -> pp_map0 (kpp, vpp) ppf m
  let pp_big_map (kpp, vpp) ppf = function BigMap m -> pp_map0 (kpp, vpp) ppf m

  type ensemble = (rclabel, address) map

  let pp_ensemble = pp_map (Format.pp_print_string, pp_address)
  
  type template_descriptor = BrokerContractTypes.template_descriptor = {
      template_id  : nat;
      template_fee : fee_descriptor;
      template_availability : bool;

      ccgen: scamlbytes;
      (** should unpack to [ccgen] *)

      bookhash : book_hash option;
      tmplhash : template_hash;
    } [@@deriving show { with_path = false }]
  type instance_descriptor = BrokerContractTypes.instance_descriptor = {
      sprthash   : spirit_hash;
      originator : address;
      tmplhash   : template_hash;
      ensemble   : ensemble;
    } [@@deriving show { with_path = false }]

  type template_store = (nat, template_descriptor) big_map
  type instance_store = (spirit_hash, instance_descriptor) big_map

  let pp_template_store = pp_big_map (pp_nat, pp_template_descriptor)
  let pp_instance_store = pp_big_map (pp_spirit_hash, pp_instance_descriptor)

  type address_set = address set
  let pp_address_set ppf = 
    function Set xs ->
      Format.(fprintf ppf "{ %a }"
                (pp_list_contents pp_address) xs)

  type storage = BrokerContractTypes.storage = {
      broker_fee    : fee_descriptor;
      broker_admins : address_set;
      broker_availability: bool;

      templates : template_store;
      instances : instance_store;
    } [@@deriving show { with_path = false }]

  type big_map_handler = nat
  let pp_big_map_handler ppf =
    function Nat x ->
      Format.fprintf ppf "bigmap/%i" x

  type storage' = BrokerContractTypes._storage = {
      broker_fee    : fee_descriptor;
      broker_admins : address_set;
      broker_availability: bool;

      templates : big_map_handler;
      instances : big_map_handler;
    } [@@deriving show { with_path = false }]

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

  type fees = { broker_earns   : tz*address option;
                template_earns : tz*address option;
                residue : tz }

  let calculate_fees sudo broker_fee template_fee initbalance =
    let zerotz = Tz 0. in
    let total = Global.get_amount() in
    let (-$) x y = if x < y then failwith "insufficient transaction amount" else x -$ y in
    let residue = total -$ initbalance in
    if sudo then { broker_earns = zerotz, None; template_earns = zerotz, None; residue = total }
    else let earns = function
           | FreeOfCharge -> zerotz, None
           | Charged info -> info.fee_amount, Some info.fee_collector in
         let broker_earns = earns broker_fee in
         let template_earns = earns template_fee in
         let residue = residue -$ (fst broker_earns) -$ (fst template_earns) in
         { broker_earns; template_earns; residue }


  let transfer_fees fees =
    let op addr amount = match (Contract.contract addr : unit contract option) with
      | None -> failwith "incorrect or not-supported collector address"
      | Some c -> Operation.transfer_tokens () amount c in
    let op = function (amount, Some addr) -> [op addr amount] | _ -> [] in
    List.rev_append (op fees.broker_earns) (op fees.template_earns)

  let process_origination
        broker sudo
        template_fee broker_fee
        { ccgen; tmplhash; _ }
        { sprthash; genparam; initbalance; _ } =
    let fees = calculate_fees sudo broker_fee template_fee initbalance in
    let { genprog; initprog } = match (Obj.unpack ccgen : ccgen option) with
      | None -> failwith "114"
      | Some x -> x
    in
    let rclist = genprog genparam in
    let rcolist =
      let f (rclabel, body, storage0) =
        let (op, addr) = wrap body storage0 (Some { broker; sprthash; rclabel }) initbalance in
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
    let ops = List.rev_append (transfer_fees fees) ops in
    ops, tmplhash, ensemble

  let check_sudo { broker_admins; _ } =
    let src = Global.get_source() in
    if not (Set.mem src broker_admins) then failwith "sudoer check fails"
    else ()

  let typecheck_template ccgen =
    match (Obj.unpack ccgen : ccgen option) with
    | None -> failwith "template.ccgen typecheck fails"
    | _ -> ()

  let fetch_template (tmplid : nat) templates : template_descriptor =
    match BigMap.get tmplid templates with
    | Some x -> x | None -> failwith "template not exists"

  let main (msg : message)
        (({templates; instances; broker_admins;
           broker_fee; broker_availability;
           _} as storage) : storage) =
    let broker = Contract.(address self) in
    match msg with
    | Originate ({ template; sprthash; _} as req) ->
       if not broker_availability then failwith "broker not available";
       if BigMap.mem sprthash instances then failwith "sprthash conflict";
       let ({ template_fee; template_availability; _ } as template) =
         fetch_template template templates in
       if not template_availability then failwith "template not available";
       let (ops, tmplhash, ensemble) = process_origination broker false template_fee broker_fee template req in
       let instance = { sprthash; originator = Global.get_source();
                        tmplhash; ensemble } in
       let instances = BigMap.update sprthash (Some instance) instances in
       ops, {storage with instances}
    | SudoOriginate ({ template; sprthash; _} as req) ->
       check_sudo storage;
       if BigMap.mem sprthash instances then failwith "sprthash conflict";
       let ({ template_fee; _ } as template) =  fetch_template template templates in
       let (ops, tmplhash, ensemble) = process_origination broker true template_fee broker_fee template req in
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
    | SudoUploadTemplate { tmplid; ccgen } ->
       check_sudo storage;
       if BigMap.mem tmplid templates then failwith "template id conflict";
       typecheck_template ccgen;
       let tmplhash = Crypto.blake2b ccgen in
       let template = { template_id = tmplid; template_fee = FreeOfCharge;
                        template_availability = false;
                        ccgen; bookhash = None; tmplhash; } in
       [], { storage with templates = BigMap.update tmplid (Some template) templates }
    | SudoUpdateBroker {
        new_fee;
        new_availability;
      } ->
       check_sudo storage;
       let fee = match new_fee with None -> broker_fee | Some x -> x in
       let availability = match new_availability with None -> broker_availability | Some x -> x in
       [], { storage with broker_fee = fee; broker_availability = availability }
    | SudoUpdateTemplate {
        template = tmplid;
        new_fee;
        new_availability;
        new_bookhash;
      } ->
       check_sudo storage;
       let template = BigMap.get tmplid templates in
       let ({ template_fee; template_availability; bookhash; _ } as template) = match template with
           Some x -> x | None -> failwith "template not found" in
       let fee = match new_fee with None -> template_fee | Some x -> x in
       let availability = match new_availability with None -> template_availability | Some x -> x in
       let bookhash = match new_bookhash with None -> bookhash | Some x -> x in
       let template = { template with template_fee = fee; template_availability = availability ; bookhash } in
       let templates = BigMap.update tmplid (Some template) templates in
       [], { storage with templates = templates }
    | SudoWithdraw {
        amount;
        collector;
      } ->
       check_sudo storage;
       let op = match (Contract.contract collector : unit contract option) with
         | None -> failwith "incorrect or not-supported collector address"
         | Some c -> Operation.transfer_tokens () amount c
       in [op], storage
  [@@entry]
end

module Args = struct
  let tzout : string option ref = ref None
  let printing : [ `Nothing
                 | `WrapperContractCode
                 | `BrokerContractCode
                 | `CcgenType
                 | `InitStorage of SCaml.address SCaml.set
                 | `InterpStorage of string
                 | `OriginateContract of (string*bool)
                 (** [("sprthash,initbal,tmplid,genparam", sudo?)]  *)
                 | `SwitchBroker of bool
                 | `MakeBrokerCharge of string
                 | `MakeBrokerFree
                 | `MakeTemplateCharge of string
                 | `MakeTemplateFree of string
                 | `UploadTemplate of string
                 | `SwitchTemplate of (string*bool)
                 | `Withdraw of string
                 ] ref = ref `Nothing

  let toprint x () = printing := x

  let speclist = [
      ("-wrapper-code", Arg.Unit (toprint `WrapperContractCode),
       "to print the wrapper contract code");
      ("-broker-code", Arg.Unit (toprint `BrokerContractCode),
       "to print the wrapper contract code");
      ("-ccgen-type", Arg.Unit (toprint `CcgenType),
       "to print the Michelson type of ccgen");
      ("-init-storage", Arg.String (fun str ->
                            let open SCaml in 
                            let addrs = Str.split_delim (Str.regexp ",") str in
                            let addrs = addrs |> List.map (fun x -> Address x) in
                            let addrs = Set addrs in
                            printing := `InitStorage addrs),
       "<admins> to print the an initial storage for the broker contract");
      ("-interp-storage", Arg.String (fun str ->
                            printing := `InterpStorage str),
       "<storage-tz> interpret an on-chain storage");
      ("-originate", Arg.String (fun str ->
                         printing := `OriginateContract (str, false)),
       "<sprthash,initbal,tmplid,genparam> originate contract with [tmplid]");
      ("-sudo-originate", Arg.String (fun str ->
                         printing := `OriginateContract (str, true)),
       "<sprthash,initbal,tmplid,genparam> sudo originate contract with [tmplid], \
        this ignores both the broker and template availability switch and
        suppresses fee processing");
      ("-enable-broker", Arg.Unit (fun () ->
                             printing := `SwitchBroker true),
       "switch the broker on");
      ("-disable-broker", Arg.Unit (fun () ->
                             printing := `SwitchBroker false),
       "switch the broker off");
      ("-make-broker-charge", Arg.String (fun str ->
                                  printing := `MakeBrokerCharge str),
       "<fee,collector> make the broker to charge [fee], transferred to [collector]");
      ("-make-broker-free", Arg.Unit (toprint `MakeBrokerFree),
       "make the broker free of charge");
      ("-make-template-charge", Arg.String (fun str ->
                                  printing := `MakeTemplateCharge str),
       "<tmplid,fee,collector> make the template [tmplid] to charge [fee], transferred to [collector]");
      ("-make-template-free", Arg.String (fun str ->
                                  printing := `MakeTemplateFree str),
       "<tmplid> make the template [tmplid] free of charge");
      ("-upload-template", Arg.String (fun str ->
                             printing := `UploadTemplate str),
       "<tmplid,ccgen> upload a new template; [ccgen] should be packed in bytes");
      ("-enable-template", Arg.String (fun str ->
                             printing := `SwitchTemplate (str, true)),
       "<tmplid> switch the template on");
      ("-disable-template", Arg.String (fun str ->
                             printing := `SwitchTemplate (str, false)),
       "<tmplid> switch the template off");
      ("-withdraw", Arg.String (fun str ->
                        printing := `Withdraw str),
       "<amount,collector> withdraw [amount], transferred to [collector]");
    ]

  let usage() = Arg.usage speclist "broker.ml: Broker smart-contract coordinator\n\
                                    please specify an argument"
  let () = Arg.parse speclist print_endline "broker.ml"
end

let (&) a b = b |> a

let () =
  match !Args.printing with
  | `WrapperContractCode -> 
     wrapper_contract |> print_endline
  | `BrokerContractCode -> 
     [%scamlcontract BrokerContract] |> print_endline
  | `CcgenType ->
     [%scamltype.tz: BrokerContractTypes.ccgen] |> print_endline
  | `InitStorage admins ->
     [%scamltype: BrokerContractTypes.storage]#convert
       (init_storage admins)
     |> print_endline
  | `InterpStorage tz ->
     [%scamltype: BrokerContractTypes._storage]#revert tz |>
       begin function
         | None -> Format.eprintf "unable to parse data"; exit 2
         | Some s -> Format.printf "%a\n" Printers.pp_storage' s
       end
  | `OriginateContract (str, sudo) ->
     Scanf.sscanf str "%s@,%g,%i,%s" &
       fun sprthash initbal tmplid genparam ->
       let open BrokerContractTypes in
       let msg req = match sudo with true -> SudoOriginate req | false -> Originate req in
       [%scamltype: BrokerContractTypes.message]#convert
         (msg { sprthash; template = Nat tmplid;
                genparam = Bytes genparam; initbalance = Tz initbal; })
       |> print_endline
  | `SwitchBroker x ->
     let open BrokerContractTypes in
     [%scamltype: BrokerContractTypes.message]#convert
       (SudoUpdateBroker { new_fee = None; new_availability = Some x })
     |> print_endline
  | `MakeBrokerCharge str ->
     Scanf.sscanf str "%g,%s" &
       fun fee collector ->
       let open BrokerContractTypes in
       [%scamltype: BrokerContractTypes.message]#convert
         (SudoUpdateBroker { new_fee =
                               Option.some
                               & Charged
                                   { fee_amount = Tz fee;
                                     fee_collector = Address collector};
                             new_availability = None })
       |> print_endline
  | `MakeBrokerFree ->
     let open BrokerContractTypes in
     [%scamltype: BrokerContractTypes.message]#convert
       (SudoUpdateBroker { new_fee = Some FreeOfCharge;
                           new_availability = None })
     |> print_endline
  | `MakeTemplateCharge str ->
     Scanf.sscanf str "%i,%g,%s" &
       fun tmplid fee collector ->
       let open BrokerContractTypes in
       [%scamltype: BrokerContractTypes.message]#convert
         (SudoUpdateTemplate { template = Nat tmplid; new_availability = None;
                               new_fee = Option.some
                                         & Charged
                                             { fee_amount = Tz fee;
                                               fee_collector = Address collector};
                               new_bookhash = None; })
       |> print_endline
  | `MakeTemplateFree str ->
     Scanf.sscanf str "%i" &
       fun tmplid ->
       let open BrokerContractTypes in
       [%scamltype: BrokerContractTypes.message]#convert
         (SudoUpdateTemplate { template = Nat tmplid; new_availability = None;
                               new_fee = Some FreeOfCharge;
                               new_bookhash = None; })
       |> print_endline
  | `UploadTemplate str ->
     Scanf.sscanf str "%i,%s" &
       fun tmplid ccgen ->
       let open BrokerContractTypes in
       [%scamltype: BrokerContractTypes.message]#convert
         (SudoUploadTemplate { tmplid = Nat tmplid;
                               ccgen = Bytes ccgen;})
       |> print_endline
  | `SwitchTemplate (str, x) ->
     Scanf.sscanf str "%i" &
       fun tmplid ->
       let open BrokerContractTypes in
       [%scamltype: BrokerContractTypes.message]#convert
         (SudoUpdateTemplate { template = Nat tmplid; new_availability = Some x;
                               new_fee = None; new_bookhash = None; })
       |> print_endline
  | `Withdraw str ->
     Scanf.sscanf str "%g,%s" &
       fun amount collector ->
       let open BrokerContractTypes in
       [%scamltype: BrokerContractTypes.message]#convert
         (SudoWithdraw { amount = Tz amount; collector = Address collector })
       |> print_endline
  | `Nothing -> Args.usage()
