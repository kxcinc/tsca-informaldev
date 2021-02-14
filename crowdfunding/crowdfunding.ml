module%scamltypes CrowdfundingTypes = struct
  open SCaml

  type storage = {
      raisers      : address set;
      refund_table : (address, tz) big_map;
      withdrawn    : bool;

      funding_start : timestamp;
      funding_end   : timestamp;
      unconditional_refund_start : timestamp;
    }

  type action =
    | Contribute of {
        refund_address : address;
      }
    | Withdraw of {
        beneficiary : address;
      }
    | Refund of {
        eligible_address : address;
      }
end

module%scamlcontract CrowdfundingMain = struct
  open CrowdfundingTypes
  open SCaml

  let create_transfer amount beneficiary =
    match (Contract.contract beneficiary : unit contract option) with
    | None -> failwith "incorrect or not-supported beneficiary address"
    | Some c -> Operation.transfer_tokens () amount c

  let main : (action, storage) SCaml.entry =
    fun action storage ->
    let now = Global.get_now() in
    let amount = Global.get_amount() in
    let source = Global.get_source() in
    match action with
    | Contribute { refund_address } ->
       if storage.funding_start <= now && now <= storage.funding_end then (
         let refundable_total = match BigMap.get refund_address storage.refund_table with
         | None -> amount
         | Some prev -> prev +$ amount in
         let updated_table =
           BigMap.update refund_address (Some refundable_total)
             storage.refund_table in
         [], { storage with refund_table = updated_table }
       ) else (
         failwith "not in funding period"
       )
    | Withdraw { beneficiary } ->
       if storage.funding_start > now
          || storage.funding_end < now
          || storage.unconditional_refund_start <= now then (
         failwith "invalid time to make withdrawal request"
       ) else if storage.withdrawn then (
         failwith "raised fund already withdrawn"
       ) else if (Set.mem source storage.raisers = false) then (
         failwith "only raisers could request withdrawal"
       ) else (
         [create_transfer (Global.get_balance()) beneficiary],
         { storage with withdrawn = true }
       )
    | Refund { eligible_address } ->
       if storage.unconditional_refund_start < now then (
         failwith "invalid time to make refund request"
       ) else if storage.withdrawn then (
         failwith "raised fund already withdrawn"
       ) else (
         match BigMap.get eligible_address storage.refund_table with
         | None -> failwith "not eligible for refund or already refunded"
         | Some refundable_amount ->
            let updated_table =
              BigMap.update eligible_address None storage.refund_table in
            [create_transfer refundable_amount eligible_address],
            { storage with refund_table = updated_table }
       )

  let [@entry] entrypoint = main
end
