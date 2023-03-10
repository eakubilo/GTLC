open GTLC

let y = ref 0

let make_fresh_label () =
  y := !y + 1 ;
  AST.Label !y

let is_exception e = match e () with exception _ -> true | _ -> false

let%test _ = parse "0" = Int 0

let%test _ = parse "\\x : int .x" = Lam (Var "x", B Int, Var "x")

let%test _ = parse "\\x.x" = Lam (Var "x", Dyn, Var "x")

let%test _ = parse "\\x.\\y.x" = Lam (Var "x", Dyn, Lam (Var "y", Dyn, Var "x"))

let%test _ =
  parse "\\x:int.\\y:bool.x"
  = Lam (Var "x", B Int, Lam (Var "y", B Bool, Var "x"))

let%test _ =
  parse "\\x:int->int.x" = Lam (Var "x", Arrow (B Int, B Int), Var "x")

let%test _ =
  parse "\\x: int->( int-> int).x"
  = Lam (Var "x", Arrow (B Int, Arrow (B Int, B Int)), Var "x")

let%test _ =
  parse "\\x.(x x)"
  = Lam (Var "x", Dyn, App (Var "x", Var "x", make_fresh_label ()))

let%test _ =
  parse "\\x.(x x) x"
  = Lam
      ( Var "x"
      , Dyn
      , App
          ( App (Var "x", Var "x", make_fresh_label ())
          , Var "x"
          , make_fresh_label () ) )

let%test _ =
  parse "\\x:int.(x x) x"
  = Lam
      ( Var "x"
      , B Int
      , App
          ( App (Var "x", Var "x", make_fresh_label ())
          , Var "x"
          , make_fresh_label () ) )

let%test _ =
  parse "\\x:int.\\y:bool.y ((x x) x)"
  = Lam
      ( Var "x"
      , B Int
      , Lam
          ( Var "y"
          , B Bool
          , App
              ( Var "y"
              , App
                  ( App (Var "x", Var "x", make_fresh_label ())
                  , Var "x"
                  , make_fresh_label () )
              , make_fresh_label () ) ) )

let%test _ =
  parse "\\x:int->int.\\y:bool.y ((x x) x)"
  = Lam
      ( Var "x"
      , Arrow (B Int, B Int)
      , Lam
          ( Var "y"
          , B Bool
          , App
              ( Var "y"
              , App
                  ( App (Var "x", Var "x", make_fresh_label ())
                  , Var "x"
                  , make_fresh_label () )
              , make_fresh_label () ) ) )

let%test _ = consistency (B Int, B Bool) = true

let%test _ = consistency (B Int, Dyn) = true

let%test _ = consistency (Dyn, B Int) = true

let%test _ = consistency (B Int, B Int) = true

let%test _ = consistency (B Bool, B Bool) = true

let%test _ = consistency (B Bool, Arrow (B Int, B Bool)) = false

let%test _ = consistency (B Int, Arrow (B Int, B Bool)) = false

let%test _ = consistency (Arrow (B Bool, B Int), Arrow (B Int, B Bool)) = true

let%test _ = consistency (Arrow (Dyn, B Int), Arrow (B Int, B Bool)) = true

let%test _ = consistency (Dyn, Arrow (B Int, B Bool)) = true

let%test _ = consistency (Arrow (Dyn, Dyn), Dyn) = true

let%test _ = consistency (Arrow (Dyn, Dyn), B Int) = false

let%test _ =
  well_typed_gtlc [] (parse "\\x:int.\\y:bool.x")
  = Arrow (B Int, Arrow (B Bool, B Int))

let%test _ = well_typed_gtlc [] (parse "\\x:int.x") = Arrow (B Int, B Int)

let%test _ =
  is_exception (fun _ -> well_typed_gtlc [] (parse "\\x:int\\y:bool.x y"))

(*throwaway*)

;;
make_fresh_label ()

let%test _ = well_typed_gtlc [] (parse "0") = B Int

let%test _ = well_typed_gtlc [] (parse "true") = B Bool

let%test _ = well_typed_gtlc [] (parse "false") = B Bool

let%test _ =
  evalString "(\\x.x) 1" = F (Int 1, Cast (B Int, Dyn, make_fresh_label ()))

let%test _ = evalString "(\\x:int.x) 1" = Int 1

;;
make_fresh_label ()

let%test _ = evalString "(\\y:int.y) (\\x:int.x) 1" = Int 1

;;
make_fresh_label ()

;;
make_fresh_label ()

let%test _ = evalString "(\\y:bool.y) (\\x:int.x) 1" = Int 1

;;
make_fresh_label ()

;;
make_fresh_label ()

let%test _ = is_exception (fun _ -> evalString "(\\y:bool.y) (\\x:int.x)")

;;
make_fresh_label ()

let l = make_fresh_label ()

let%test _ =
  evalString "(\\y.y) (\\x:int.x)"
  = F
      ( F
          ( Lam (Var "x", B Int, Var "x")
          , Cast (Arrow (B Int, B Int), Arrow (Dyn, Dyn), l) )
      , Cast (Arrow (Dyn, Dyn), Dyn, l) )
