module%scamldefs HeavyLifting = struct
  open SCaml

  type rclabel = string
  type wfunc = bytes*bytes -> operation list*bytes
  [@@scaml.noconv]

  type ccgen = {
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

  and genprog  = (bytes ->
                  (** genesis-params *)
                  (rclabel*wfunc*bytes) list
                  (** contract ensemble *))
  [@@scaml.noconv]

  and initprog = (bytes ->
                  (** genesis-params *)
                  (rclabel, address) map ->
                  (** contract addresses *)
                  (rclabel, bytes) map
                  (** optional initialization message per contract *))
  [@@scaml.noconv]

  let genfunny =
    { genprog = begin
        fun _ ->
        let funny : wfunc = fun (paddr, storage) ->
          let addr = match (Obj.unpack paddr : address option) with
            | None -> failwith "argument unpack failed: expecting type of [address]"
            | Some addr -> addr in
          let amount = Global.get_balance() in
          let amount = amount /$^ (Nat 2) in
          let op = match (Contract.contract addr : unit contract option) with
            | None -> failwith "incorrect or not-supported beneficiary address"
            | Some c -> Operation.transfer_tokens () amount c
          in [op], storage
        in ["funny", funny, Bytes ""]
      end;

      initprog = (fun _ _ -> Map.empty);
    }
end

let () =
  [%scamlvalue HeavyLifting.genfunny] |> print_endline
