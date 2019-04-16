import Simple
import Prelude
import Data.List (sortOn,groupBy)

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
                                        error ("Variable Arity Function "++ ident ++"Detected")
                                    else
                                        l

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
sameCons (VarPattern         _) (VarPattern         _)  = True
sameCons (VarPattern         _) (ConsPattern _      _)  = False
sameCons (ConsPattern _      _) (VarPattern  _       )  = False
sameCons (ConsPattern _      _) (ConsPattern _      _)  = True

getConstructorCount :: Pattern -> Int
getConstructorCount (VarPattern _) = 0 -- variables don't have Constructors
getConstructorCount (ConsPattern "Bool"  _) = 2
getConstructorCount (ConsPattern "[]"    _) = 2
getConstructorCount (ConsPattern "(:)"   _) = 2
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

replaceFirstPattern ::  (Pattern -> [Pattern])  -> MatchAlt -> MatchAlt
replaceFirstPattern _   ([], _)     = error "No Pattern to Replace"
replaceFirstPattern fun (x:xa, exp) = (fun x ++ xa,exp)


match :: [Ident] -> [MatchAlt]  -> Error -> Expr
match []             []            err  = err -- err would need to be compiler evaluated
match []             (([],expr):_) _    = expr
match []             (((_:_),_):_) _    = error "Too Many Patterns"
match ids@(ident:xs) matches@(_:_) err  | all isVarPattern (map firstPattern matches)
                                        = match xs (map (var_apply ident) matches) err
                                        | all isConsPattern (map firstPattern matches)
                                        =   let
                                                sorted = sortOn (extract_ident . firstPattern) matches
                                                grouped = groupBy (\x y ->  (==) (extract_ident.firstPattern $ x)(extract_ident.firstPattern $ y)) sorted
                                                branches = map (create_branche_for_group xs err) grouped
                                                cons_count = getConstructorCount $ firstPattern.head $ matches
                                                catch_all = if length branches == cons_count then [] else [(Branch (VarPattern "_") err)]
                                            in
                                                Case (Variable ident) $ branches ++ catch_all
                                        | otherwise
                                        =  foldr (\g r-> match ids g r) err $ groupBy (\x y -> sameCons  (firstPattern x) (firstPattern y)) matches

extract_sub_patterns :: Pattern -> [Pattern]
extract_sub_patterns (VarPattern  _     ) = []
extract_sub_patterns (ConsPattern _ pats) = pats

identifyers :: [Ident]
identifyers =  map ('_':) strings
    where
        strings::[Ident]
        strings =  [c:string | string <- [(c:[]) |c <- ['a'..'z']] ++ strings, c <- ['a'..'z']]

id_to_var_pat :: Ident -> Pattern
id_to_var_pat i = VarPattern i

create_branche_for_group :: [Ident] -> Expr -> [MatchAlt] -> Branch
create_branche_for_group idents err  group@(g:_) =    let
                                                        pat = firstPattern g
                                                        ident = extract_ident pat
                                                        n = length.arguments $ pat
                                                        usedIdents = concatMap collect_idents_m group
                                                        addIdents  = (take n).(filter $ flip notElem usedIdents) $ identifyers
                                                        matches = map (replaceFirstPattern extract_sub_patterns) group
                                                        varPat = map (\i -> VarPattern i) addIdents
                                                    in
                                                       Branch (ConsPattern ident varPat)  (match  (addIdents ++ idents) matches err)

-- collect_idents_* could be turned into a type class

collect_idents_m :: MatchAlt -> [Ident]
collect_idents_m (pats,expr) = concatMap collect_idents_p pats ++ collect_idents_e expr

collect_idents_p::Pattern -> [Ident]
collect_idents_p (VarPattern i) = [i]
collect_idents_p (ConsPattern _ pats) = concatMap collect_idents_p pats

collect_idents_e::Expr -> [Ident]
collect_idents_e (Variable i)           = [i]
collect_idents_e (Constructor _)        = []
collect_idents_e (Apply a b)            = concatMap collect_idents_e [a,b]
collect_idents_e (Case expr branches)   = collect_idents_e expr ++ concatMap collect_idents_b branches

collect_idents_b::Branch -> [Ident]
collect_idents_b (Branch pattern expr) = collect_idents_p pattern ++ collect_idents_e expr


extract_ident::Pattern -> Ident
extract_ident (ConsPattern ident _) = ident
extract_ident (VarPattern  ident  ) = ident

var_apply :: Ident -> MatchAlt -> MatchAlt
var_apply to ((VarPattern from:ps),expr) = (ps, subst (Subst from to) expr)


-- 4. ------------------------------------------------------------------------------------------------------------------

patternMatch :: FuncDecl -> FuncDecl
patternMatch fun@(FuncDecl fid eqs) = FuncDecl fid (
    let
        ids = take (funArity fun) identifyers
        alts = map (\(Equation _ pats expr) -> (pats,expr)) eqs
        expr = match ids alts err
        pats = map id_to_var_pat ids
    in
        [Equation fid pats expr])

-- 5. ------------------------------------------------------------------------------------------------------------------




main = print "Test"
