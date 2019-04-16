import Simple
import Prelude

-- 1. ------------------------------------------------------------------------------------------------------------------

data Subst = Subst Ident Ident

subst :: Subst -> Expr -> Expr
subst s   (Variable ident) = let
                                Subst from to = s
                             in
                                Variable (
                                  if (from == ident)   then
                                         to
                                    else
                                        ident
                                )
subst _                 const@(Constructor _)
                                                = const
subst s                 (Apply expr1 expr2)
                                                = Apply (subst s expr1) (subst s expr2)
subst s                 (Case expr xs)
                                                = Case (subst s expr) (map (subst_branch s) xs)


subst_branch :: Subst -> Branch -> Branch
subst_branch s (Branch p e) = Branch p (subst s e)


-- 2. ------------------------------------------------------------------------------------------------------------------

-- Returns the Arity of a Function
-- fails for an empty equation list or if the equations are not of the same arity
funArity::FuncDecl -> Int
funArity (FuncDecl ident [])  = error ("No equation for Function " ++ ident)
funArity (FuncDecl ident eqs@(_:_)) = let
                                    l:ls = map (\(Equation _ p _) -> length p) eqs
                                in
                                    if any (\x -> not (x==l)) ls then
                                        l
                                    else
                                        error ("Variable Arity Function "++ ident ++"Detected")

isVarPattern :: Pattern -> Bool
isVarPattern (VarPattern  _   ) = True
isVarPattern (ConsPattern _ _ ) = False

isConsPattern :: Pattern -> Bool
isConsPattern (VarPattern  _   ) = False
isConsPattern (ConsPattern _ _ ) = True

arguments :: Pattern -> [Pattern]
arguments (ConsPattern _ sub) = sub
arguments (VarPattern  _    ) = []

sameCons :: Pattern -> Pattern -> Bool
sameCons (VarPattern         _) (VarPattern         _)  = error "Are two VarPatterns contain the same Constructor?"
sameCons (VarPattern         _) (ConsPattern _      _)  = False
sameCons (ConsPattern _      _) (VarPattern  _       )  = False
sameCons (ConsPattern ident1 _) (ConsPattern ident2 _)  = ident1 == ident2

getConstructorCount :: Pattern -> Int
getConstructorCount (VarPattern _) = 0 -- variables don't have Constructors
getConstructorCount (ConsPattern "Bool" _) = 2
getConstructorCount (ConsPattern ident      _) = error ("The Constructor Count for the Type of " ++ ident ++ "is unknown")

-- 3. ------------------------------------------------------------------------------------------------------------------

todo msg = error ("Not Yet Implemented: "++ msg)

type MatchAlt = ([Pattern], Expr)

type Error = Expr

err :: Error
err = Variable "ERROR"

firstPattern :: MatchAlt -> Pattern
firstPattern ([]  , _) = error "Out Of Patterns"
firstPattern (x:xa, _) = x

match :: [Ident] -> [MatchAlt]  -> Error -> Expr
match []         []            err  = err -- err would need to be compiler evaluated
match []         (([],expr):_) _    = expr
match []         (((_:_),_):_) _    = error "Too Many Patterns"
match (ident:xs) matches       err  | all isVarPattern (map firstPattern matches)
                                        = match xs (map (var_apply ident) matches) err
                                    | all isConsPattern (map firstPattern matches)
                                        = todo "Generate Case"
                                    | otherwise
                                        = todo "Group By Variables and Constructors"

var_apply :: Ident -> MatchAlt -> MatchAlt
var_apply to ((VarPattern from:ps),expr) = (ps, subst (Subst from to) expr)

main = print "Test"