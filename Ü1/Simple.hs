-- ---------------------------------------------------------------------------
-- Abstract syntax tree + pretty printing of a simple functional language
-- ---------------------------------------------------------------------------

module Simple where

import Text.PrettyPrint

-- |Identifier
type Ident = String

-- |Module declaration
data Module = Module Ident [FuncDecl]
  deriving Show

-- |Function declaration
data FuncDecl = FuncDecl Ident [Equation]
    deriving Show

-- |Function equation with function's 'Ident', a list of 'Pattern' and a
-- result 'Expr'
data Equation = Equation Ident [Pattern] Expr
    deriving Show

-- |Pattern
data Pattern
  = VarPattern Ident            -- ^ Variable pattern
  | ConsPattern Ident [Pattern] -- ^ Constructor pattern
    deriving Show

-- |Expression
data Expr
  = Variable    Ident         -- ^ Variable, @f@
  | Constructor Ident         -- ^ Constructor, @True@
  | Apply       Expr Expr     -- ^ application, @f x@
  | Case        Expr [Branch] -- ^ Case expression: @case x of True -> False@
    deriving Show

-- |Branch of a case expression
data Branch = Branch Pattern Expr
    deriving Show

-- pretty printing
--
-- Use it like:
--
-- ghci> ppFuncDecl xorFun
-- xor True True = False
-- xor False b = b
-- xor b False = b

ppModule :: Module -> Doc
ppModule (Module m fs)
  = hsep (map text ["module", m, "where"])
    $+$ foldr (\f d -> d $+$ text "" $+$ ppFuncDecl f) empty fs

ppFuncDecl :: FuncDecl -> Doc
ppFuncDecl (FuncDecl _ eqs) = vcat $ map ppEquation eqs

ppEquation :: Equation -> Doc
ppEquation (Equation f ps e) =  text f <+> hsep (map ppPattern ps)
                            <+> equals <+> ppExpr e

ppPattern :: Pattern -> Doc
ppPattern (VarPattern     v) = text v
ppPattern (ConsPattern c ps) = parenIf (not $ null ps)
                             $ text c <+> hsep (map ppPattern ps)

ppExpr :: Expr -> Doc
ppExpr = ppExpr' 0

ppExpr' :: Int -> Expr -> Doc
ppExpr' _ (Variable    v) = text v
ppExpr' _ (Constructor c) = text c
ppExpr' p (Apply   e1 e2) = parenIf (p > 1)
                          $ sep [ppExpr' 1 e1, ppExpr' 2 e2]
ppExpr' p (Case     e bs) = parenIf (p > 0)
                          $ (text "case" <+> ppExpr e <+> text "of") $+$
                            (nest 2 $ vcat $ map ppBranch bs)

ppBranch :: Branch -> Doc
ppBranch (Branch p e) = ppPattern p <+> text "->" <+> ppExpr e

parenIf :: Bool -> Doc -> Doc
parenIf True  doc = parens doc
parenIf False doc = doc

-- test expression

xorFun :: FuncDecl
xorFun = FuncDecl "xor"
  [ Equation "xor" [ConsPattern "True" [], ConsPattern "True" []] (Constructor "False")
  , Equation "xor" [ConsPattern "False" [], VarPattern "b"] (Variable "b")
  , Equation "xor" [VarPattern "b", ConsPattern "False" []] (Variable "b")
  ]

zipFun :: FuncDecl
zipFun = FuncDecl "zip"
  [ Equation "zip" [ConsPattern "[]" [], VarPattern "bs"] (Constructor "[]")
  , Equation "zip" [VarPattern "as", ConsPattern "[]" []] (Constructor "[]")
  , Equation "zip" [ ConsPattern "(:)" [VarPattern "a", VarPattern "as"]
                   , ConsPattern "(:)" [VarPattern "b", VarPattern "bs"]
                   ] (Apply (Apply (Constructor "(:)") (Apply (Apply (Constructor "(,)") (Variable "a")) (Variable "b")))
                            (Apply (Apply (Variable "zip") (Variable "as")) (Variable "bs")))
  ]

zipFun2 :: FuncDecl
zipFun2 = FuncDecl "zip2"
  [ Equation "zip2" [ConsPattern "[]" [], VarPattern "_"] (Constructor "[]")
  , Equation "zip2" [ConsPattern "(:)" [VarPattern "_", VarPattern "_"], ConsPattern "[]" []] (Constructor "[]")
  , Equation "zip2" [ ConsPattern "(:)" [VarPattern "a", VarPattern "as"]
                   , ConsPattern "(:)" [VarPattern "b", VarPattern "bs"]
                   ] (Apply (Apply (Constructor "(:)") (Apply (Apply (Constructor "(,)") (Variable "a")) (Variable "b")))
                            (Apply (Apply (Variable "zip2") (Variable "as")) (Variable "bs")))
  ]
