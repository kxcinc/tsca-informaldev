module%scamldefs FrozenDefs = struct
  open SCaml

  type param = {
      amount      : tz;
      beneficiary : address;
    }

  type storage = {
      fund_owners : address set;
      unfrozen    : timestamp;
    }

  type genparam = {
      fund_owners : address set;
      fund_amount : tz;
      unfrozen    : timestamp;
    }
end

module%scamldefs CcInterface = struct
  open SCaml

  type rclabel = string
  type wparam   = bytes
  type wstorage = bytes

  type wgenparam = bytes
  type winitparam = bytes

  type wfunc = wparam*wstorage -> operation list*wstorage
  [@@scaml.noconv]

  and  ccgen = {
      genprog  : genprog;
      initprog : initprog;
    }
  [@@scaml.noconv]

  and  genprog  = (wgenparam ->
                   (** genesis-params *)
                   (rclabel, wfunc*tz*wstorage) map
                   (** contract ensemble *))
  [@@scaml.noconv]

  and  initprog = (wgenparam ->
                   (** genesis-params *)
                   (rclabel, address) map ->
                   (** contract addresses *)
                   (rclabel, winitparam) map
                   (** optional initialization message per contract *))
  [@@scaml.noconv]

end

(* module%scamldefs FrozenCcgen = struct
 *   open CcInterface
 *   open FrozenDefs
 *   open SCaml
 * 
 *   let ccgenfrozen : ccgen =
 *     { genprog = begin
 *         fun wgenparam ->
 *         let rclabel_frozen : string const = "frozen" in
 * 
 *         let validate_genparam : genparam -> storage
 *           = fun { fund_owners; fund_amount; unfrozen; } ->
 *           if not (Set.length fund_owners > Nat 0) then
 *             failwith "there must be at least one fund owner";
 *           let txnamount = Global.get_amount() in
 *           let currtimestamp = Global.get_now() in
 *           if txnamount <> fund_amount then
 *             failwith "transaction amount doesn't match fund amount";
 *           if not (currtimestamp < unfrozen) then
 *             failwith "unfrozen timestamp must be in the future";
 *           ({ fund_owners; unfrozen } : storage) in
 * 
 *         let validate_invocation : param -> storage -> unit =
 *           fun { amount = request_amount; _ } { fund_owners; unfrozen } ->
 *           let txnamount = Global.get_amount() in
 *           let invoker = Global.get_source() in
 *           let currtimestamp = Global.get_now() in
 *           let contractbalance = Global.get_balance() in
 *           if txnamount > Tz 0. then
 *             failwith "cannot accept positive amount transaction";
 *           if not (Set.mem invoker fund_owners) then
 *             failwith "operation is not authorized";
 *           if currtimestamp < unfrozen then
 *             failwith "fund not released yet";
 *           if contractbalance < request_amount then
 *             failwith "fund amount is insufficient";
 *           () in
 * 
 *         let perform_transfer amount beneficiary =
 *           match (Contract.contract beneficiary : unit contract option) with
 *           | None -> failwith "incorrect or not-supported beneficiary address"
 *           | Some c -> Operation.transfer_tokens () amount c in
 * 
 * 
 *         let frozen_main : wfunc =
 *           fun (wparam, wstorage) ->
 *           let param : param option = Obj.unpack wparam in
 *           let storage : storage option = Obj.unpack wstorage in
 *           let param = match param with
 *             | Some x -> x | None ->
 *                              failwith "unable to unpack wparam" in
 *           let storage = match storage with
 *             | Some x -> x | None ->
 *                              failwith "unable to unpack wstorage" in
 *           validate_invocation param storage;
 *           let op = perform_transfer param.amount param.beneficiary in
 *           [op], wstorage
 *         in
 * 
 *         let genparam : genparam option = Obj.unpack wgenparam in
 *         let genparam = match genparam with
 *           | Some x -> x | None ->
 *                            failwith "unable to unpack wgenparam" in
 *         let initstorage = validate_genparam genparam in
 *         let initstorage = Obj.pack initstorage in
 *         Map [ rclabel_frozen, (frozen_main, genparam.fund_amount, initstorage) ]
 *       end;
 *       initprog = (fun _ _ -> Map.empty); }
 * end *)

module Args = struct
  let printing : [ `FrozenPacked
                 | `FrozenCode
                 | `FrozenGenparam of string
                 | `CcgenType
                 | `FrozenGenparamType
                 | `FrozenStorageType
                 ] ref = ref `FrozenCode

  let toprint x () = printing := x

  let speclist = [
      ("-genparam", Arg.String (fun str -> printing := `FrozenGenparam str),
       "<amount,unfrozen,owner1[,owner2,...]> genparam for frozen");
      ("-genparam-type", Arg.Unit (toprint `FrozenGenparamType),
       "genparam michelson type");
      ("-storage-type", Arg.Unit (toprint `FrozenStorageType),
       "storage michelson type");
      ("-code", Arg.Unit (toprint `FrozenCode),
       "print ccgen code in Michelson");
      ("-packed", Arg.Unit (toprint `FrozenPacked),
       "print packed version ccgen in hex");
      ("-ccgen-type", Arg.Unit (toprint `CcgenType),
       "print ccgen type");
    ]

  let usage() = Arg.usage speclist __FILE__
  let () = Arg.parse speclist print_endline __FILE__
end

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
end

let () =
  let (&) a b = b |> a in
  match !Args.printing with
  (* | `FrozenCode ->
   *    [%scamlvalue FrozenCcgen.ccgenfrozen] |> print_endline
   * | `FrozenPacked ->
   *    let hex = Hex.of_string [%scamlvalue.packed FrozenCcgen.ccgenfrozen] in
   *    Format.printf "%a" Hex.pp hex *)
  | `CcgenType ->
     [%scamltype.tz: CcInterface.ccgen] |> print_endline
  | `FrozenGenparam str ->
     Scanf.sscanf str "%g,%s@,%s" &
       fun amount unfrozen owners ->
       let owners = Str.split_delim (Str.regexp ",") owners in
       let owners = owners |> List.map (fun x -> SCaml.Address x) in
       let conv = [%scamltype: FrozenDefs.genparam] in
       conv#convert {
           fund_owners = SCaml.Set owners;
           fund_amount = Tz amount;
           unfrozen    = Timestamp unfrozen; }
       |> print_endline
  | `FrozenGenparamType ->
     [%scamltype.tz: FrozenDefs.genparam] |> print_endline
  | `FrozenStorageType ->
     [%scamltype.tz: FrozenDefs.storage] |> print_endline

