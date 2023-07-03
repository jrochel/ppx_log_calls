open Ppxlib

module Constants = struct
  let pp_prefix = "pp_"
  let pp = "pp"
  let pp_pbox = "PpxLogCallsAux.pp_pbox"
  let pp_tuple n = "PpxLogCallsAux.pp_tuple" ^ string_of_int n
  let pp_string = "Stdlib.Format.pp_print_string"
  let pp_space = "Stdlib.Format.pp_print_space"
  let pp_cut = "Stdlib.Format.pp_print_cut"
  let pp_flush = "Stdlib.Format.pp_print_flush"
  let pp_open_box = "PpxLogCallsAux.pp_open_box"
  let pp_close_box = "PpxLogCallsAux.pp_close_box"
  let const = "PpxLogCallsAux.const"
  let todo = "PpxLogCallsAux.unsupported"
  let log = "PpxLogCallsAux.log"
  let fmt = "fmt"
end

module Builder (Loc : Ast_builder.Loc) = struct
  include Ast_builder.Make (struct let loc = Loc.loc end)

  let fmt = evar Constants.fmt
  let pp_string str = eapply (evar Constants.pp_string) [fmt; estring str]
  let pp_space = eapply (evar Constants.pp_space) [fmt; eunit]
  let pp_open_box = eapply (evar Constants.pp_open_box) [fmt]
  let pp_close_box = eapply (evar Constants.pp_close_box) [fmt]
  let pp_pbox pp = eapply (evar Constants.pp_pbox) [pp]
  let pp_cut = eapply (evar Constants.pp_cut) [fmt; eunit]
  let pp_flush = eapply (evar Constants.pp_flush) [fmt; eunit]
end

exception Unsupported of string loc

let unsupported ~loc txt = raise @@ Unsupported {txt; loc}

let rec pp_var_by_typ ~loc ?(no_parentheses = false) {ptyp_desc; _} =
  let open Builder (struct let loc = loc end) in
  let const str = eapply (evar Constants.const) [estring str] in
  let todo str = eapply (evar Constants.todo) [estring str] in
  match ptyp_desc with
  | Ptyp_arrow _ -> const "<fun>"
  | Ptyp_constr ({txt; _}, args) ->
    let pp_t =
      let pp = function "t" -> Constants.pp | t -> Constants.pp_prefix ^ t in
      match txt with
      | Lapply _ -> failwith "unsupported:Lapply"
      | Lident c -> evar @@ pp c
      | Ldot _ ->
        let pp_str =
          match List.rev @@ Longident.flatten_exn txt with
          | v :: mods -> String.concat "." @@ List.rev (pp v :: mods)
          | _ -> assert false
        in
        evar pp_str
    in
    let pp_saturated = eapply pp_t @@ List.map (pp_var_by_typ ~loc) args in
    if no_parentheses then pp_saturated else pp_pbox pp_saturated
  | Ptyp_alias (ptyp, _) -> pp_var_by_typ ~loc ptyp
  | Ptyp_tuple elts ->
    let pp_tuple_n =
      match elts with
      | [_; _] -> Constants.pp_tuple 2
      | [_; _; _] -> Constants.pp_tuple 3
      | [_; _; _; _] -> Constants.pp_tuple 4
      | [_; _; _; _; _] -> Constants.pp_tuple 5
      | [_; _; _; _; _; _] -> Constants.pp_tuple 6
      | [_; _; _; _; _; _; _] -> Constants.pp_tuple 7
      | [_; _; _; _; _; _; _; _] -> Constants.pp_tuple 8
      | _ -> unsupported ~loc "Ptyp_tuple: too many elements"
    in
    eapply (evar pp_tuple_n)
      (List.map (pp_var_by_typ ~loc ~no_parentheses:true) elts)
  | Ptyp_object (_, _) -> todo "Ptyp_object"
  | Ptyp_class (_, _) -> todo "Ptyp_class"
  | Ptyp_variant (_, _, _) -> todo "Ptyp_variant"
  | Ptyp_poly (_, _) -> todo "Ptyp_poly"
  | Ptyp_package _ -> todo "Ptyp_package"
  | Ptyp_extension _ -> todo "Ptyp_extension"
  | Ptyp_any | Ptyp_var _ -> const "_"

type param =
  {param_name : string; param_type : core_type option; param_label : arg_label}

let pp_param
    { loc;
      txt =
        { param_name : string;
          param_type : core_type option;
          param_label : arg_label } }
  =
  let open Builder (struct let loc = loc end) in
  let p_label label e =
    match label with
    | Nolabel -> e
    | Labelled l ->
      esequence [pp_string @@ "~" ^ l ^ ":"; pp_open_box; e; pp_close_box]
    | Optional l ->
      esequence [pp_string @@ "?" ^ l ^ ":"; pp_open_box; e; pp_close_box]
  in
  let pp_var_by_typ_o = function
    | None -> pp_string "_"
    | Some pt -> eapply (pp_var_by_typ ~loc pt) [fmt; evar param_name]
  in
  esequence
    [ pp_space;
      pp_open_box;
      p_label param_label (pp_var_by_typ_o param_type);
      pp_close_box ]

let print_call ~loc params fname =
  let open Builder (struct let loc = loc end) in
  let seq =
    [pp_string fname; esequence (List.rev_map pp_param params); pp_cut; pp_flush]
  in
  let log pp =
    eapply (evar Constants.log)
      [eabstract [ppat_var {txt = Constants.fmt; loc}] pp]
  in
  value_binding ~pat:punit ~expr:(log @@ esequence seq)

let transform_binding ~traversal ~loc vb =
  (* recurse through lambdas accumulating typed parameters in [params] *)
  let rec add_print ~loc fname params = function
    | {pexp_desc = Pexp_fun (lbl, exp0, pat, expr); _} as pexp_fun ->
      let transform_pattern pat =
        match pat with
        | { ppat_desc = Ppat_constraint ({ppat_desc = Ppat_var {txt; _}; _}, t);
            ppat_loc;
            _ } ->
          let param =
            { loc = ppat_loc;
              txt = {param_name = txt; param_type = Some t; param_label = lbl}
            }
          in
          add_print ~loc fname (param :: params) expr
        | {ppat_loc; _} ->
          let param =
            { loc = ppat_loc;
              txt = {param_name = "_"; param_type = None; param_label = lbl} }
          in
          add_print ~loc fname (param :: params) expr
      in
      let expr' = transform_pattern pat in
      {pexp_fun with pexp_desc = Pexp_fun (lbl, exp0, pat, expr')}
    | expr ->
      Ast_builder.Default.pexp_let ~loc Nonrecursive
        [print_call ~loc params fname]
        (traversal#expression expr)
  in
  match vb.pvb_pat.ppat_desc with
  | Ppat_var {txt; loc} -> {vb with pvb_expr = add_print ~loc txt [] vb.pvb_expr}
  | _ ->
    let msg =
      Format.asprintf "unsupported binding pattern: %a" Pprintast.pattern
        vb.pvb_pat
    in
    unsupported ~loc msg

class traversal =
  object (traversal)
    inherit Ppxlib.Ast_traverse.map as super

    method structure_item_desc =
      function
      | Pstr_extension (({txt = "log"; loc}, payload), _) -> begin
        match payload with
        | PStr [{pstr_desc = Pstr_value (rec_flag, bindings); _}] ->
          let bindings' =
            List.map (transform_binding ~traversal ~loc) bindings
          in
          Pstr_value (rec_flag, bindings')
        | _ -> failwith @@ "unsupported structure item for %log"
      end
      | x -> super#structure_item_desc x

    method expression_desc =
      function
      | Pexp_extension ({txt = "log"; loc}, payload) -> begin
        match payload with
        | PStr
            [ { pstr_desc =
                  Pstr_eval
                    ({pexp_desc = Pexp_let (rec_flag, bindings, expr); _}, _);
                _ } ] ->
          let bindings' =
            List.map (transform_binding ~traversal ~loc) bindings
          in
          Pexp_let (rec_flag, bindings', expr)
        | _ -> failwith @@ "unsupported payload for %log"
      end
      | x -> super#expression_desc x
  end

let instrument = (new traversal)#structure
let () = Driver.register_transformation ~impl:instrument "ppx_log_calls"
