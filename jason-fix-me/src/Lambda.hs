-- Code to translate aliencode to the lambda calculus.

module Lambda(prettyPrintEquation) where

import Data.List(intersperse)
import Eval
import Parse

data LcExpr =
  LcConstant Integer
  | LcIdent String
  | LcPair Integer Integer
  | LcList [LcExpr]
  | LcLambda String LcExpr
  | LcApply LcExpr LcExpr

instance Show LcExpr where
  show (LcConstant i) = show i
  show (LcIdent name) = name
  show (LcPair i j) = show (i, j)
  show (LcList exprs) = "[" ++ concat (intersperse ", " (fmap show exprs)) ++ "]"
  show (LcLambda name body) = "(\\" ++ name ++ " -> " ++ show body ++ ")"
  show (LcApply f e@(LcApply _ _)) = show f ++ " (" ++ show e ++ ")"
  show (LcApply f x) = show f ++ " " ++ show x

countFree :: String -> LcExpr -> Int
countFree _ (LcConstant _) = 0
countFree x (LcIdent name) = if x == name then 1 else 0
countFree _ (LcPair _ _) = 0
countFree x (LcList exprs) = sum (fmap (countFree x) exprs)
countFree x (LcLambda name _) | name == x = 0
countFree x (LcLambda _ body) = countFree x body
countFree x (LcApply f y) = countFree x f + countFree x y

isSimpleLc (LcConstant _) = True
isSimpleLc (LcIdent _) = True
isSimpleLc (LcPair _ _) = True
isSimpleLc _ = False

subst _ _ expr@(LcConstant _) = expr
subst name value (LcIdent x) | x == name = value
subst _ _ expr@(LcIdent _) = expr
subst _ _ expr@(LcPair _ _) = expr
subst name value (LcList exprs) = LcList (fmap (subst name value) exprs)
subst name value (LcLambda param body) | param /= name = LcLambda param (subst name value body)
subst _ _ expr@(LcLambda _ _) = expr
subst name value (LcApply f arg) = apply (subst name value f) (subst name value arg)

apply :: LcExpr -> LcExpr -> LcExpr
apply (LcIdent "i") x = x
apply (LcApply (LcIdent "cons") (LcConstant x)) (LcConstant y) = LcPair x y
apply (LcApply (LcIdent "cons") head) (LcIdent "nil") = LcList [head]
apply (LcApply (LcIdent "cons") head) (LcList tail) = LcList (head : tail)
apply (LcLambda name body) actual | countFree name body < 2 || isSimpleLc actual = subst name actual body
apply f x = LcApply f x


isListLike :: Expression -> Bool
isListLike (Identifier "nil") = True
isListLike (Apply (Apply (Identifier "cons") _) t) = isListLike t
isListLike _ = False

translateTail :: [String] -> Expression -> ([LcExpr], [String])
translateTail e (Identifier "nil") = ([], e)
translateTail e (Apply (Apply (Identifier "cons") h) t) =
  let (h', e1) = translate e h
      (t', e2) = translateTail e t
  in (h' : t', e2)

isSimple :: Expression -> Bool
isSimple (Constant _) = True
isSimple (Identifier _) = True
isSimple _ = False

variableNames = ["x", "y", "z", "w"] ++ ["v" ++ show i | i <- [0..]]

translate :: [String] -> Expression -> (LcExpr, [String])
-- *** Simplify by identities
-- i x = x
translate e (Apply (Identifier "i") x) = translate e x
-- c f x y = f y x
translate e (Apply (Apply (Apply (Identifier "c") f) x) y) = translate e (Apply (Apply f y) x)
-- b f g x = f (g x)
translate e (Apply (Apply (Apply (Identifier "b") f) g) x) = translate e (Apply f (Apply g x))
-- s f g x = f x (g x)
translate e (Apply (Apply (Apply (Identifier "s") f) g) x) | isSimple x = translate e (Apply (Apply f x) (Apply g x))
-- *** Expand combinators
translate e (Identifier "c") = let f : x : y : e' = e in (LcLambda f (LcLambda x (LcLambda y (apply (apply (LcIdent f) (LcIdent y)) (LcIdent x)))), e')
translate e (Identifier "b") = let f : g : x : e' = e in (LcLambda f (LcLambda g (LcLambda x (apply (LcIdent f) (apply (LcIdent g) (LcIdent x))))), e')
translate e (Identifier "s") = let f : g : x : e' = e in (LcLambda f (LcLambda g (LcLambda x (apply (apply (LcIdent f) (LcIdent x)) (apply (LcIdent g) (LcIdent x))))), e')
-- *** Core language
translate e (Constant i) = (LcConstant i, e)
translate e (Identifier name) = (LcIdent name, e)
translate e (Apply f x) =
  let (f', e1) = translate e f
      (x', e2) = translate e1 x
  in (apply f' x', e2)

-- Make the variable names a little nicer
renameVariables :: [(String, String)] -> [String] -> LcExpr -> LcExpr
renameVariables _ _ expr@(LcConstant _) = expr
renameVariables renames available (LcIdent name) = LcIdent (maybe name id $ lookup name renames)
renameVariables _ _ expr@(LcPair _ _) = expr
renameVariables renames available (LcList exprs) = LcList (fmap (renameVariables renames available) exprs)
renameVariables renames available (LcLambda name body) =
  let renames' = (name, newName) : renames
      newName : available' = available
  in LcLambda newName (renameVariables renames' available' body)
renameVariables renames available (LcApply f x) = LcApply (renameVariables renames available f) (renameVariables renames available x)


-- b (b f)
-- = \x

-- b (b b)
-- = compose (compose compose)
-- = \f -> compose (compose f)
-- = \f -> \g -> \y -> (compose f) (g y)
-- = \f -> \g -> \y -> compose f (g y)
-- = \f -> \g -> \y -> compose f (g y)
-- = \f -> \g -> \y -> \x -> f (g y z)    -- Compose 1-arg to 2-arg.

-- b (b b) . b (b b)
-- = comp12 . comp12
-- = \f -> comp12 (comp12 f)
-- = \f g -> comp12 (comp12 f) g
-- = \f g x y -> comp12 f (g x y)
-- = \f g x y z w -> f (g x y z w)  -- Compose 1-arg to 4-arg.

renderSimplified :: Expression -> String
renderSimplified expr =
  let (expr', _) = translate variableNames expr
  in show $ renameVariables [] variableNames expr'


renderTail :: Expression -> String
renderTail (Identifier "nil") = ""
renderTail (Apply (Apply (Identifier "cons") h) t) = ", " ++ render h ++ renderTail t

-- *** Simplify by identities
-- i x = x
render (Apply (Identifier "i") x) = render x
-- c f x y = f y x
render (Apply (Apply (Apply (Identifier "c") f) x) y) = render (Apply (Apply f y) x)
-- b f g x = f (g x)
render (Apply (Apply (Apply (Identifier "b") f) g) x) = render (Apply f (Apply g x))
-- s f g x = f x (g x)
render (Apply (Apply (Apply (Identifier "s") f) g) x) | isSimple x = render (Apply (Apply f x) (Apply g x))
-- *** Render using some syntactic sugar
-- b f g = f . g    (using haskell's `.` operator to mean "compose")
render (Apply (Apply (Identifier "b") f) g) = render f ++ " . " ++ render g
-- cons 3 5 = (3, 5)
render (Apply (Apply (Identifier "cons") (Constant x)) (Constant y)) = "(" ++ show x ++ ", " ++ show y ++ ")"
-- cons x (cons y nil) = [x, y]
render (Apply (Apply (Identifier "cons") h) t) | isListLike t = "[" ++ render h ++ renderTail t ++ "]"
-- *** Core notation
render (Constant i) = show i
render (Identifier name) = name
render (Apply f (Apply g x)) = render f ++ " (" ++ render (Apply g x) ++ ")"
render (Apply f x) = render f ++ " " ++ render x

prettyPrintEquation (name, expr) =
  name ++ " = " ++ renderSimplified expr ++ "\n"
