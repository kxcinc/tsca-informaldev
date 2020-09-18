module%scamltypes CrowdfundingTypes = struct
  open SCaml
  type stage =
    | Prefunding | Funding | Funded | Unfunded | Withdrawn | Refunding
  type action =
    | RaiserWithdraw of {
        beneficiary : address;
      }
    | Contribute of {
        refund_address : address;
      }
    | RefundRequest of {
        contribution_index : nat;
      }
  type contribution_entry = {
      contributor  : address;
      contribution : tz;
      refund_address : address;
      contribution_timestamp : timestamp;
      refunding_timestamp    : timestamp option;
    }
  type storage = {
      raisers : address set;
      funding_start : timestamp;
      funding_end   : timestamp;
      unconditional_refund_start : timestamp;

      funding_target : tz option;
      maximal_raise  : tz option;
      minimal_contribution : tz option;

      total_raised : tz;
      contribution_registry   : (nat, contribution_entry) big_map;
      next_contribution_index : nat;
    }
end

module%scamlcontract CrowdfundingMain = struct
  open CrowdfundingTypes
  open SCaml

  type entrypoint = (action, storage) SCaml.entry

  let calc_stage storage =
    let { funding_start; funding_end;
          total_raised; funding_target;
          unconditional_refund_start; _ } = storage in
    let now = Global.get_now() in
    if now < funding_start then Prefunding
    else if now < funding_end then Funding
    else begin
        (* funding ended *)
        let funding_successful = match funding_target with
          | None -> true | Some target -> total_raised >= target in
        if not funding_successful then Unfunded
        else begin
            (* a successful funding *)
            let withdrawn = Global.get_balance() = (Tz 0.) in
            if withdrawn then Withdrawn
            else if now < unconditional_refund_start then Funded
            else Refunding
          end
      end

  let perform_transfer beneficiary amount (errmsg : string) =
    match (Contract.contract beneficiary : unit contract option) with
    | None -> failwith errmsg
    | Some c -> Operation.transfer_tokens () amount c

  let handle_contribution = function
    | Contribute { refund_address }, storage ->
       let contribution = Global.get_amount() in
       let contributor = Global.get_sender() in
       let { minimal_contribution; maximal_raise; total_raised;
             contribution_registry = registry; next_contribution_index = idx;
             _ } = storage in
       let fallshort_minimal_contribution() = match minimal_contribution with
         | Some minimal -> contribution < minimal
         | None -> false in
       let maximal_raise_excess() = match maximal_raise with
         | Some maximal -> total_raised +$ contribution > maximal
         | None -> false in
       if fallshort_minimal_contribution() then
         failwith "your contribution is less than the minimal"
       else if maximal_raise_excess() then
         failwith "your contribution will make excess the maximal raise amount"
       else begin
           let entry = {
               contributor; contribution; refund_address;
               contribution_timestamp = Global.get_now();
               refunding_timestamp = None;
             } in
           let registry' = BigMap.update idx (Some entry) registry in
           let storage' = {
               storage with
               contribution_registry = registry';
               next_contribution_index = idx +^ (Nat 1);
             } in
           ([] : operations), storage'
         end
    | _ -> failwith "panic"

  let handle_refund_request stage = function
    | RefundRequest { contribution_index = idx }, storage -> begin
        let { contribution_registry = registry;
              total_raised;
              _ } = storage in
        match BigMap.get idx registry with
        | None -> failwith "contribution specified does not exist"
        | Some entry ->
           let requester = Global.get_source() in
           let beneficiary = entry.refund_address in
           let contribution = entry.contribution in
           let already_refunded() = match entry.refunding_timestamp with
             | None -> false | Some _ -> true in
           if already_refunded() then
             failwith "contribution amount has already been refunded"
           else if requester <> beneficiary then
             failwith "requester is not authorized to perform this refund operation"
           else begin
               let op = perform_transfer beneficiary contribution
                          "incorrect or not-supported refund_address" in
               let entry' = { entry with
                              refunding_timestamp = Some (Global.get_now()) } in
               let storage' = {
                   storage with
                   total_raised =
                     if stage = Funding then total_raised -$ contribution
                     else total_raised;
                   contribution_registry = BigMap.update idx (Some entry') registry;
                 } in
               [op], storage'
             end
      end
    | _ -> failwith "panic"

  let handle_withdrawal = function
    | RaiserWithdraw { beneficiary }, storage ->
       let { raisers; total_raised;
             _ } = storage in
       let requester = Global.get_source() in
       if requester <> beneficiary then
         failwith "one can only request a withdrawal on its own behalf"
       else if not (Set.mem beneficiary raisers) then
         failwith "only raisers are permitted to perform a withdrawal"
       else begin
           let op = perform_transfer beneficiary total_raised
                      "incorrect or not-supported refund_address raiser address" in
           [op], storage
         end
    | _ -> failwith "panic"


  let main : entrypoint
    = fun action storage ->
    let stage = calc_stage storage in
    let ops, storage = match stage, action with
    | Prefunding, _ -> failwith "funding period hasn't started yet: no operation allowed"
    | Funding, RaiserWithdraw _ -> failwith "still in funding period: no raiser withdrawal allowed"
    | Funding, Contribute _ -> handle_contribution (action, storage)
    | Funding, RefundRequest _ -> handle_refund_request stage (action, storage)
    | Funded, RaiserWithdraw _ -> handle_withdrawal (action, storage)
    | Funded, _ -> failwith "campaign has been funded: only raiser withdrawal allowed"
    | Unfunded, RefundRequest _ -> handle_refund_request stage (action, storage)
    | Unfunded, _ -> failwith "campaign has been unsuccessful: only refund request will be processed"
    | Withdrawn, _ -> failwith "campaign fund has been withdrawn by the raisers: no further operation allowed"
    | Refunding, RefundRequest _ -> handle_refund_request stage (action, storage)
    | Refunding, _ -> failwith "campaign reached the refunding stage: only refund request will be processed"
    in ops, storage

  let [@entry] entrypoint = main
end

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

module Args = struct
  let tzout : string option ref = ref None
  let printing : [`Nothing |
                  `ParameterType |
                  `StorageType |
                  `ContractCode
                 ] ref = ref `Nothing

  let toprint x () = printing := x

  let speclist = [
      ("-tzout", Arg.String (fun x -> tzout := Some x;
                                      if x = "-" then printing := `ContractCode),
       "<out> specify to write the contract code; '-' means stdout");
      ("-paramtype", Arg.Unit (toprint `ParameterType),
       "print contract Michelson parameter type");
      ("-storagetype", Arg.Unit (toprint `StorageType),
       "print contract Michelson strage type");
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
      in output_string ch [%scamlcontract CrowdfundingMain];
         flush ch
   | _ -> ());
  (match !printing with
   | `ParameterType ->
      [%scamltype: CrowdfundingTypes.action]#tztype |> print_endline
   | `StorageType ->
      [%scamltype: CrowdfundingTypes.storage]#tztype |> print_endline
   | `ContractCode ->
      () (* already handled above *)
   | `Nothing -> usage()
  )
