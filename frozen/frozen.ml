module%scamltypes FrozenTypes = struct
  open SCaml
  type param =
    (* | Withdraw of *)
    { amount : tz;
      (** amount to be withdrawn *)

      beneficiary : address;
      (** the account to receive the withdrawn fund *)
    }
  type storage = {
      fund_owners : address set;
      (** the set of accounts who could each withdraw the fund when it's unfrozen *)

      unfrozen : timestamp;
      (** the date after which the deposit could be withdrawn *)
    }
end

module%scamlcontract FrozenMain = struct
  open FrozenTypes
  open SCaml

  type entrypoint = (param, storage) SCaml.entry

  let perform_withdraw ( { amount; beneficiary }) =
    match (Contract.contract beneficiary : unit contract option) with
    | None -> failwith "incorrect or not-supported beneficiary address"
    | Some c -> Operation.transfer_tokens () amount c

  let validate_invocation
        { amount; _ }
        { fund_owners; unfrozen } =
    if Global.get_amount() > (Tz 0.)
    then failwith "frozen contract cannot accept tokens";
    if not (Set.mem (Global.get_source()) fund_owners)
    then failwith "source of operation is not whitelisted for withdrawal operations";
    if Global.get_now() < unfrozen
    then failwith "deposit is still frozen";
    if Global.get_balance() < amount
    then failwith "requested withdrawal amount exceeds the balance";
    ()

  let main : entrypoint
    = fun param storage ->
    validate_invocation param storage;
    let op = perform_withdraw param in
    [op], storage

  let [@entry] entrypoint = main
end

let init_storage :
      owners:(string list) ->
      unfrozen:string ->
      FrozenTypes.storage
  = fun ~owners ~unfrozen ->
  let open SCaml in
  FrozenTypes.{
      fund_owners = Set (owners |> List.map (fun x -> Address x));
      unfrozen = Timestamp unfrozen;
  }

let pp_timestamp ppf ts =
  let (>>=) = Option.bind in
  (match int_of_string_opt ts >>= fun ts ->
         Ptime.Span.of_int_s ts |> Ptime.of_span with
   | Some t -> Ptime.pp_human () ppf t
   | None ->
      match Ptime.of_rfc3339 ~strict:false ts with
      | Ok (t, _, _) -> Ptime.pp_human () ppf t
      | Error _ -> Format.fprintf ppf "error parsing RFC3339 format");
  Format.fprintf ppf " ; orig='%s'" ts

let explain_storage : FrozenTypes.storage -> unit =
  fun { fund_owners = Set (fund_owners);
        unfrozen = Timestamp unfrozen } ->
  let open Format in
  print_flush();
  print_string
    "[frozen.ml contract]"; print_newline();
  print_string
    "Fund Owners: ["; print_newline();
  print_string "  ";
  print_flush();
  open_box 2;
  fund_owners
  |> List.iter (fun (SCaml.Address acc) ->
         print_string acc;
         print_string "; "; print_cut());
  close_box(); print_cut(); print_string "]";
  print_newline();
  print_string
    "Unfrozen Timestamp: ";
  pp_timestamp std_formatter unfrozen;
  print_newline();
  print_flush()
  

module Args = struct
  let tzout : string option ref = ref None
  let printing : [`Nothing |
                  `ParameterType |
                  `StorageType |
                  `InitStorage |
                  `ParamWithdraw |
                  `InterpStorage of string |
                  `ContractCode
                 ] ref = ref `Nothing

  let toprint x () = printing := x

  module GenesisParams = struct
    let owners    : string option ref = ref None
    let unfrozen : string option ref = ref None
    let set r x =
      r := Some x;
      printing := `InitStorage
    let get r = Option.get !r
  end

  module InvokWithdrawParams = struct
    let amount : float option ref = ref None
    let beneficiary : string option ref = ref None
    let set r x =
      r := Some x;
      printing := `ParamWithdraw
    let get r = Option.get !r
  end

  let speclist = [
      ("-tzout", Arg.String (fun x -> tzout := Some x;
                                      if x = "-" then printing := `ContractCode),
       "<out> specify to write the contract code; '-' means stdout");
      ("-paramtype", Arg.Unit (toprint `ParameterType),
       "print contract Michelson parameter type");
      ("-storagetype", Arg.Unit (toprint `StorageType),
       "print contract Michelson strage type");
      ("-invok-withdraw", Arg.Tuple InvokWithdrawParams.[
           (Arg.Float (set amount));
           (Arg.String (set beneficiary))
       ], "<amount> <beneficiary> print contract argument in Michelson to perform a Withdraw");
      ("-interp-storage", Arg.String (fun st -> printing := (`InterpStorage st)),
       "<storage> interpret and describe the storage value in Michelson");
      ("-initstorage", Arg.Tuple GenesisParams.[
           (Arg.String (set owners));
           (Arg.String (set unfrozen));
       ], "<fund-owners> <unfrozen-date> \
           synthesize the initial storage for the contract; \
           <fond-owners> in comma separated list");
    ]

  let usage() = Arg.usage speclist "frozen.ml: Frozen smart-contract coordinator\n\
                                    please specify an argument"
  let () = Arg.parse speclist print_endline "frozen.ml"
end

let () =
  let open Args in
  (match !tzout with
   | Some out ->
      let ch =
        if out = "-" then stdout
        else open_out out
      in output_string ch [%scamlcontract FrozenMain];
         flush ch
   | _ -> ());
  (match !printing with
   | `ParameterType ->
      [%scamltype: FrozenTypes.param]#tztype |> print_endline
   | `StorageType ->
      [%scamltype: FrozenTypes.storage]#tztype |> print_endline
   | `InitStorage ->
      let open GenesisParams in
      let iv = init_storage
                 ~owners:((get owners) |> Str.split_delim (Str.regexp ","))
                 ~unfrozen:(get unfrozen) in
      [%scamltype: FrozenTypes.storage]#convert iv
      |> print_endline
   | `ParamWithdraw -> 
      let open InvokWithdrawParams in
      let open FrozenTypes in
      [%scamltype: FrozenTypes.param]#convert
        { amount = Tz (get amount);
          beneficiary = Address (get beneficiary);
        } |> print_endline
   | `InterpStorage st -> begin
       try
         [%scamltype: FrozenTypes.storage]#revert st
         |> Option.get
         |> explain_storage
       with _ -> Format.eprintf "Michelson parsing error"
     end
   | `ContractCode ->
      () (* already handled above *)
   | `Nothing -> usage()
  )
