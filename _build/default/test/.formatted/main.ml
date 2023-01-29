open GTLC

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
  parse "\\x.(x x):LABEL(application)"
  = Lam (Var "x", Dyn, App (Var "x", Var "x", Label "application"))

let%test _ =
  parse "\\x.((x x):LABEL(first) x):LABEL(second)"
  = Lam
      ( Var "x"
      , Dyn
      , App (App (Var "x", Var "x", Label "first"), Var "x", Label "second") )

let%test _ =
  parse "\\x:int.((x x):LABEL(first) x):LABEL(second)"
  = Lam
      ( Var "x"
      , B Int
      , App (App (Var "x", Var "x", Label "first"), Var "x", Label "second") )

let%test _ =
  parse
    "\\x:int.\\y:bool.(y ((x x):LABEL(first) x):LABEL(second)):LABEL(third)"
  = Lam
      ( Var "x"
      , B Int
      , Lam
          ( Var "y"
          , B Bool
          , App
              ( Var "y"
              , App
                  ( App (Var "x", Var "x", Label "first")
                  , Var "x"
                  , Label "second" )
              , Label "third" ) ) )

let%test _ =
  parse
    "\\x:int->int.\\y:bool.(y ((x x):LABEL(first) \
     x):LABEL(second)):LABEL(third)"
  = Lam
      ( Var "x"
      , Arrow (B Int, B Int)
      , Lam
          ( Var "y"
          , B Bool
          , App
              ( Var "y"
              , App
                  ( App (Var "x", Var "x", Label "first")
                  , Var "x"
                  , Label "second" )
              , Label "third" ) ) )

let%test _ = consistency (Arrow (B Int, B Bool), Arrow (B Bool, B Int)) = true

let%test _ = consistency (B Int, B Bool) = true

let%test _ = consistency (B Int, B Dyn) = true

let%test _ = consistency (B Int, B Dyn) = true

let%test _ = consistency (B Int, B Int) = true

let%test _ = consistency (B Bool, B Bool) = true

let%test _ = consistency (B Bool, Arrow (B Int, B Bool)) = true
