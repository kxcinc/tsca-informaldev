module%scamlcontract WrapperContract = struct
  open SCaml

  type wparam = bytes
  type wstorage = bytes

  type storage = {
      wstorage : wstorage;
      wfunc    : wfunc;
      avatarid : avatar_identity option;
    }
  and wfunc = wparam*wstorage -> operation list*wstorage
  and avatar_identity = {
      broker   : address;
      sprthash : string;
      rclabel  : string;
      tmplversion : string;
    }

  let main arg ({wfunc;wstorage;_} as st) =
    let (ops, new_wstorage) = wfunc (arg, wstorage) in
    (ops, { st with wstorage = new_wstorage })
  [@@entry]
end

module Args = struct
  let out : string option ref = ref None
  let speclist = [
      ("-o", Arg.String (fun x -> out := Some x),
       "<out> specify where to write the contract code");
    ]

  let usage() = Arg.usage speclist "wrapper.ml: Broker smart-contract coordinator"
  let () = Arg.parse speclist print_endline "wrapper.ml"
end

let () =
  let ch =
    match !Args.out with
    | None -> stdout
    | Some f -> open_out f
  in output_string ch [%scamlcontract WrapperContract];
     flush ch
