module Examples where
import GCL

--Q.E.D.
program1 :: Stmt
program1 =
    Var [("x", Int)]
    (
      Assume (Ref "x" :> I 0) :&
      ["x"] := [I 0] :&
      ["x"] := [Ref "x" :+ I 2] :&
      ["return"] := [Ref "x"] :&
      Assert (Ref "return" :== I 2)
    )

--Falsifiable
program2 :: Stmt
program2 =
    Var [("x", Int)]
    (
      Assume (Ref "x" :> I 0) :&
      ["x"] := [I 1] :&
      ["return"] := [Ref "x"] :&
      Assert (Ref "return" :== I 0)
    )

--Q.E.D.
program3 :: Stmt
program3 =
    Var [("x", Int)]
    (
      Assume (Ref "x" :> I 0):&
      Inv (Ref "x" :>= I 0) (Ref "x" :> I 0) (["x"] := [Ref "x" :- I 1]) :&
      ["return"] := [Ref "x"] :&
      Assert (Ref "return" :== I 0)
    )

--Falsifiable
program3b :: Stmt
program3b =
    Var [("x", Int)]
    (
        ["x"] := [I 3] :&
        Inv (Ref "x" :>= I 0) (Ref "x" :> I 0) (["x"] := [Ref "x" :- I 2]) :&
        Assert (Ref "x" :== I 0)
    )

--Falsifiable.
program3_2 :: Stmt
program3_2 =
    Var [("x", Int)]
    (
      Assume (Ref "x" :> I 0):&
      Inv (Ref "x" :>= I 0) (Ref "x" :> I 2) (["x"] := [Ref "x" :- I 1]) :&
      ["return"] := [Ref "x"] :&
      Assert (Ref "return" :== I 1)
    )
--Falsifiable.
program3_3 :: Stmt
program3_3 =
    Var [("x", Int)]
    (
      Assume (Ref "x" :> I 0):&
      Inv (Ref "x" :>= I 0) (Ref "x" :> I 2) (["x"] := [Ref "x" :- I 2]) :&
      ["return"] := [Ref "x"] :&
      Assert (Ref "return" :== I 0)
    )

--Falsifiable
program3_4 :: Stmt
program3_4=
    Var [("x", Int)]
    (
      ["x"] := [I 3] :&
      Inv (Ref "x" :>= I 0) (Ref "x" :> I 0) (["x"] := [Ref "x" :- I 2]) :&
      ["return"] := [Ref "x"] :&
      Assert (Ref "return" :== I 0)
    )


--Q.E.D.
program4 :: Stmt
program4 =
    Var [("x", Int)]
    (
      Assume (Ref "x" :> I 3) :&
      ["x"] := [I 3]:&
      ["return"] := [Ref "x" :+ I 1] :&
      Assert (Ref "return" :== I 4)
    )

--Falsefiable
programScope :: Stmt
programScope =
      ["x"] := [I 3] :&
      --["x"] := [Ref "x" :+ I 1] :&
      Var [("x", Int)] -- new scope
      (
        ["return"] := [Ref "x"]
      ) :&
      Assert (Ref "return" :== I 4)



--Q.E.D.
program5 :: Stmt
program5 = Skip

--Falsifiable
program6 :: Stmt
program6 =
    Var [("x", Int)]
    (
      Assume (Ref "x" :> I (-2)) :&
      ["x"] := [I 1 :+ I 6 :+ Ref "x"] :&
      ["return"] := [Ref "x"] :&
      Assert (Ref "return" :> I 6)
    )

--array Q.E.D.
program7 :: Stmt
program7 =
    Var [("x", Int),("y", Array)]
    (
      Assume (Ref "x" :> I (-2)) :&
      SetArray "y" (1, 1) :&
      Assert (RefA 1 "y" :== I 1)
    )

-- array Falsifiable.
program8 :: Stmt
program8 =
    Var [("x", Int),("y", Array)]
    (
      Assume (Ref "x" :> I (-2)) :&
      SetArray "y" (1, 1) :&
      Assert (RefA 1 "y" :== I 2)
    )
program9 :: Stmt
program9 =
    program1 :& program2 :& program3 :& program4 :& program5 :& program6 :& program7 :& program8

program10 :: Stmt
program10 =
    program1 :& program2 :& program3 :& program4 :& program5 :& program6 :& program7 :& program8 :& program9

program11 :: Stmt
program11 =
    program1 :& program2 :& program3 :& program4 :& program5 :& program6 :& program7 :& program8 :& program9 :& program10
