open Core
open Effect_based_interpreter.Interpreter

let fact_expr =
  let open Syntax in
  let fix =
    let z =
      Fn
        {
          param = "x";
          body =
            App
              {
                fn = Var "f";
                arg =
                  Fn
                    {
                      param = "y";
                      body =
                        App
                          {
                            fn = App { fn = Var "x"; arg = Var "x" };
                            arg = Var "y";
                          };
                    };
              };
        }
    in
    Fn { param = "f"; body = App { fn = z; arg = z } }
  in
  let fct =
    Fn
      {
        param = "fct";
        body =
          Fn
            {
              param = "n";
              body =
                Cond
                  {
                    pred = Var "n";
                    cons =
                      Bin_op
                        {
                          op = Times;
                          left = Var "n";
                          right =
                            App
                              {
                                fn = Var "fct";
                                arg =
                                  Bin_op
                                    {
                                      op = Minus;
                                      left = Var "n";
                                      right = Int 1;
                                    };
                              };
                        };
                    alt = Int 1;
                  };
            };
      }
  in
  let factorial = App { fn = fix; arg = fct } in
  App { fn = factorial; arg = Int 10 }

let get_set_expr =
  let open Syntax in
  Seq (Set { loc = 42; value = Int 1337 }, Get 42)

let value_to_string = Fn.compose Sexp.to_string_hum Domain.sexp_of_value

let _ =
  let open List.Let_syntax in
  let%map expr = [ fact_expr; get_set_expr ] in
  let result = run_interp expr in
  print_endline (value_to_string result)
