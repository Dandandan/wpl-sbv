module Main where
import Data.SBV
import GCL
import Examples
import Data.Monoid
import qualified Data.Map as M
import Data.List

-- | Convert Expr to SInteger given the environment
exprIntegerToSInteger :: Env -> Expr -> SInteger
exprIntegerToSInteger env expr =
    case expr of
        I i ->
            i
        Ref s ->
            exprIntegerToSInteger (M.delete s env) (env M.! s)
        RefA i s -> do
            let A sA = env M.! s
            readArray sA (literal i)
        (:+) a b ->
            exprIntegerToSInteger env a + exprIntegerToSInteger env b
        (:-) a b ->
            exprIntegerToSInteger env a - exprIntegerToSInteger env b
        (:*) a b ->
            exprIntegerToSInteger env a * exprIntegerToSInteger env b
        (:/) a b ->
            exprIntegerToSInteger env a `sDiv` exprIntegerToSInteger env b
        _ ->
            error "no int"

exprBoolToSBool :: Env -> Expr -> SBool
exprBoolToSBool env expr =
    case expr of
        (:||) a b ->
            exprBoolToSBool env a ||| exprBoolToSBool env b
        (:&&) a b ->
            exprBoolToSBool env a &&& exprBoolToSBool env b
        (:^) a b ->
            exprBoolToSBool env a &&& exprBoolToSBool env b
        V a b ->
            exprBoolToSBool env a ||| exprBoolToSBool env b
        (:=>) a b ->
            exprBoolToSBool env a ==> exprBoolToSBool env b
        (:<=>) a b ->
            exprBoolToSBool env a <=> exprBoolToSBool env b
        (:<) a b ->
            exprIntegerToSInteger env a .< exprIntegerToSInteger env b
        (:<=) a b ->
            exprIntegerToSInteger env a .<= exprIntegerToSInteger env b
        (:>) a b ->
            exprIntegerToSInteger env a .> exprIntegerToSInteger env b
        (:>=) a b ->
            exprIntegerToSInteger env a .>= exprIntegerToSInteger env b
        (:==) a b ->
            exprIntegerToSInteger env a .== exprIntegerToSInteger env b
        Not a ->
            bnot (exprBoolToSBool env a)
        TRUE ->
            true
        FALSE ->
            false
        B b ->
            b
        Ref s ->
            exprBoolToSBool (M.delete s env) (env M.! s )
        _ ->
            error ("bool error: " ++ show expr)

type Env =  M.Map String Expr

-- | Filters post condition
onlyPost :: Expr -> Expr
onlyPost expr =
    case expr of
        p :^ _ -> -- ~= assertion
            p -- throw away rest of
        _ ->
            TRUE

-- | The wlp transformer
wlp :: Int -> Stmt -> Expr -> Expr
wlp scopeId stmt q =
    case stmt of
        Skip ->
            q
        ["return"] := [e] ->
            Replace ["return"] [e] (onlyPost q)
        x :=  e ->
            Replace x e q
        Box s t->
            wlp scopeId s q :^ wlp scopeId t q
        Assert p ->
            p :^ q
        Assume p ->
            p :=> q
        Inv i g s ->
            InvExpr scopeId i g q s
        Var refs stmts -> do
            --Forall refs (wlp scopeId stmts q)
            let nstmts = (foldr (\r -> replaceStmt r (r ++ show scopeId)) stmts (map fst refs))
            Forall (map (\(s, t) -> (s ++ show scopeId, t)) refs) (wlp (scopeId + 1) nstmts q)
        (:&) s1 s2 ->
            wlp scopeId s1 (wlp scopeId s2 q)
        SetArray r (i,j) ->
            ArrayRef r (i,j,q)

replaceStmt :: String -> String -> Stmt -> Stmt
replaceStmt ref new stmt =
    case stmt of
        xs := es ->
            map (\x -> if x == ref then new else x) xs := map (\e -> replaceExpr ref e (Ref new)) es
        Box s t->
            Box (replaceStmt ref new s) (replaceStmt ref new t)
        Assert p ->
            Assert (replaceExpr ref p (Ref new))
        Assume p ->
            Assume (replaceExpr ref p (Ref new))
        Inv i g s ->
            Inv (rE i) (rE g) (replaceStmt ref new s)
        Var refs stmts ->
            let nStmts =
                 if ref `elem` map fst refs
                     then stmts
                     else replaceStmt ref new stmts
            in  Var refs nStmts
        (:&) s1 s2 ->
            replaceStmt ref new s1 :& replaceStmt ref new s2
        SetArray r (i,j) ->
            let nr = if ref == r then new else r
            in SetArray nr (i,j)
        q -> q
        where rE x = replaceExpr ref x (Ref new)


vars :: Expr -> [(String, Type)]
vars e =
    case e of
        Ref s ->
            [(s, Int)]
        RefA _ s ->
            [(s, Array)]
        ArrayRef s _ ->
            [(s, Array)]
        (:+) a b ->
            vars a ++ vars b
        (:-) a b ->
            vars a ++ vars b
        (:/) a b ->
            vars a ++ vars b
        (:*) a b ->
            vars a ++ vars b
        (:||) a b ->
            vars a ++ vars b
        (:&&) a b ->
            vars a ++ vars b
        (:=>) a b ->
            vars a ++ vars b
        (:<) a b ->
            vars a ++ vars b
        (:<=) a b ->
            vars a ++ vars b
        (:>) a b ->
            vars a ++ vars b
        (:>=) a b ->
            vars a ++ vars b
        (:==) a b ->
            vars a ++ vars b
        Not a ->
            vars a
        (:^) a b ->
            vars a ++ vars b
        V a b ->
            vars a ++ vars b
        (:<=>) a b ->
            vars a ++ vars b

        _     ->
            []

-- | Replace free (often symbolic) variables that occur in expression
replaceExpr :: String -> Expr -> Expr -> Expr
replaceExpr s old new =
    case old of
        Ref s' ->
            if s == s' then new else Ref s'
        RefA i s' ->
            case new of
                Ref ref -> if s == s' then RefA i ref else RefA i s'
                _     -> error "no Reference"
        ArrayRef s' (i, j, e) ->
            case new of
                Ref ref -> if s == s' then ArrayRef ref (i, j, e) else ArrayRef s' (i, j, e)
                _     -> error "no Reference"
        Replace xs es e ->
            case new of
                Ref ref -> Replace (map (\x -> if x  == s then ref else x) xs) (map (\o -> replaceExpr s o new) es)
                         (replaceExpr s e new)
                _ -> error "no Reference"
        (:+) a b ->
            (:+) (r a) (r b)
        (:-) a b ->
            (:-) (r a) (r b)
        (:/) a b ->
            (:/) (r a) (r b)
        (:*) a b ->
            (:*) (r a) (r b)
        (:||) a b ->
            (:||) (r a) (r b)
        (:&&) a b ->
            (:&&) (r a) (r b)
        (:=>) a b ->
            (:=>) (r a) (r b)
        (:<) a b ->
            (:<) (r a) (r b)
        (:<=) a b ->
            (:<=) (r a) (r b)
        (:>) a b ->
            (:>) (r a) (r b)
        (:>=) a b ->
            (:>=) (r a) (r b)
        (:==) a b ->
            (:==) (r a) (r b)
        Not a ->
            Not (r a)
        x -> x
        where r o = replaceExpr s o new

-- | Converts Expr 'Predicate' to Predicate from SBV using symbolic variables
toPred :: [(String, Type)] -> Env -> Expr -> Predicate
toPred forallVars varEnv expr = go
    where
    go = do
      --generate symbolic variables for forallVars
      symbolicVarsI <- mapM forall [var | (var, Int)   <- forallVars]
      symbolicVarsB <- mapM forall [var | (var, Bool)  <- forallVars]
      symbolicVarsA <- mapM (`newArray` Nothing) [var | (var, Array) <- forallVars]
      let ei  = M.fromList (zip [var | (var, Int) <- forallVars] (map I symbolicVarsI))
          eb =  M.fromList (zip [var | (var, Bool) <- forallVars] (map B symbolicVarsB))
          ea =  M.fromList (zip [var | (var, Array) <- forallVars] (map A symbolicVarsA))
      run (ei <> eb <> ea <> varEnv) expr
    run :: Env -> Expr -> Predicate
    run env e = do
        let rEnv = run env
        case e of
            q :^ r -> do
                x <- rEnv q
                y <- rEnv r
                return (x &&& y)
            V q r -> do
                x <- rEnv q
                y <- rEnv r
                return (x ||| y)
            q :=> r -> do
                x <- rEnv q
                y <- rEnv r
                return (x ==> y)
            q :<=> r -> do
                x <- rEnv q
                y <- rEnv r
                return (x <=> y)
            InvExpr sId i g q s -> do
                let p = (i :^ Not g :=> q) :^ (i :^ g :=> wlp sId s i)
                -- 'boxing' vars inside loop
                p' <- toPred (nub (vars p)) M.empty p
                i' <- rEnv i
                o' <- rEnv (Not g :^ q)
                return (sBranch p' i' o')
            Not p -> do
                p' <- rEnv p
                return (bnot p')
            Forall refs q -> do
                toPred (map (\(r,t) -> (r, t)) refs) env q
            ArrayRef ref (i, j, q)-> do
                let A sA = env M.! ref
                let x = writeArray sA (literal i) (literal j)
                let nEnv = M.singleton ref (A x) <> env
                toPred [] nEnv q
            Replace x xs q -> do
                let nEnv = M.fromList (map (\(ref, ex) -> (ref, maybe ex (replaceExpr ref ex) (M.lookup ref env))) (zip x xs))
                toPred [] (nEnv <> env) q
            ex -> return (exprBoolToSBool env ex)

-- | The main entry point.
-- | Computes the WLP and proves it using SBV + Z3 (by default)
main :: IO ()
main = do
    let program = program8
    let q = wlp 0 program TRUE
    let s = toPred [] M.empty q
    -- output z3 code, second boolean negates the goal (for proving)
    smtLib <- compileToSMTLib True False s
    putStrLn smtLib
    r <- prove s
    print r
