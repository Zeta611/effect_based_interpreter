open Core
open Effect
open Effect.Deep
module Var = String

module Loc = struct
  type t = int [@@deriving sexp, compare]

  include Comparator.Make (struct
    type nonrec t = t

    let compare = compare
    let sexp_of_t = sexp_of_t
  end)
end

module Syntax = struct
  type expr =
    | Unit
    | Int of int
    | Var of Var.t
    | Seq of expr * expr
    | Bin_op of { op : bin_op; left : expr; right : expr }
    | Cond of { pred : expr; cons : expr; alt : expr }
    | Fn of { param : Var.t; body : expr }
    | App of { fn : expr; arg : expr }
    | Set of { loc : Loc.t; value : expr }
    | Get of Loc.t
  [@@deriving sexp]

  and bin_op = Plus | Minus | Times
end

module Domain = struct
  type value = Unit | Int of int | Loc of Loc.t | Clos of clos
  [@@deriving sexp]

  and mem = value Map.M(Loc).t
  and env = value Map.M(Var).t
  and clos = { param : Var.t; body : Syntax.expr; env : env }
end

type pgm_err = Type_error of { expected : string } | Unbound_var of Var.t

exception Pgm_err of pgm_err

type _ Effect.t +=
  | Read_env : Domain.env t
  | In_env : Domain.env -> (Domain.value Lazy.t -> Domain.value) t
  | Lookup_loc : Loc.t -> Domain.value t
  | Update_loc : Loc.t * Domain.value -> unit t

let bin_op (op : Syntax.bin_op) (v1 : Domain.value) (v2 : Domain.value) :
    Domain.value =
  match (op, v1, v2) with
  | Plus, Domain.Int n1, Domain.Int n2 -> Domain.Int (n1 + n2)
  | Minus, Domain.Int n1, Domain.Int n2 -> Domain.Int (n1 - n2)
  | Times, Domain.Int n1, Domain.Int n2 -> Domain.Int (n1 * n2)
  | _ -> raise (Pgm_err (Type_error { expected = "int" }))

let rec eval (expr : Syntax.expr) : Domain.value =
  let open Domain in
  match expr with
  | Unit -> Unit
  | Int i -> Int i
  | Var x -> (
      let env = perform Read_env in
      match Map.find env x with
      | None -> raise (Pgm_err (Unbound_var x))
      | Some v -> v)
  | Seq (e1, e2) ->
      eval e1 |> ignore;
      eval e2
  | Bin_op { op; left; right } -> bin_op op (eval left) (eval right)
  | Cond { pred; cons; alt } -> (
      match eval pred with
      | Int 0 -> eval alt
      | Int _ -> eval cons
      | _ -> raise (Pgm_err (Type_error { expected = "int" })))
  | Fn { param; body } ->
      let env = perform Read_env in
      Clos { param; body; env }
  | App { fn; arg } -> (
      match eval fn with
      | Clos { param; body; env } ->
          let v = eval arg in
          let env' = Map.set env ~key:param ~data:v in
          (perform (In_env env')) (lazy (eval body))
      | _ -> raise (Pgm_err (Type_error { expected = "fn" })))
  | Set { loc; value } ->
      perform (Update_loc (loc, eval value));
      Unit
  | Get loc -> perform (Lookup_loc loc)

let rec env_handler =
  {
    retc =
      (fun x ~env ->
        ignore env;
        x);
    exnc = raise;
    effc =
      (fun (type a) (eff : a t) ->
        match eff with
        | Read_env ->
            Some
              (fun (k : (a, _) continuation) ~(env : Domain.env) ->
                continue k env ~env)
        | In_env new_env ->
            Some
              (fun (k : (a, _) continuation) ~(env : Domain.env) ->
                continue k
                  (fun v -> match_with force v env_handler ~env:new_env)
                  ~env)
        | _ -> None);
  }

let mem_handler =
  {
    retc =
      (fun x ~mem ->
        ignore mem;
        x);
    exnc = raise;
    effc =
      (fun (type a) (eff : a t) ->
        match eff with
        | Lookup_loc l ->
            Some
              (fun (k : (a, _) continuation) ~(mem : Domain.mem) ->
                continue k (Map.find_exn mem l) ~mem)
        | Update_loc (l, v) ->
            Some
              (fun (k : (a, _) continuation) ~(mem : Domain.mem) ->
                continue k () ~mem:(Map.set mem ~key:l ~data:v))
        | _ -> None);
  }

let empty_env = Map.empty (module Var)
let empty_mem = Map.empty (module Loc)

let run_interp (expr : Syntax.expr) : Domain.value =
  match_with force
    (lazy (match_with eval expr env_handler ~env:empty_env))
    mem_handler ~mem:empty_mem
