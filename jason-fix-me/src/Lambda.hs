-- Code to translate aliencode to the lambda calculus.

module Lambda(prettyPrintEquation) where

import Data.List(intersperse)
import DataTypes(Value(..))
import Eval
import Parse
import Text.Show(showParen)

data LcExpr =
  LcConstant Integer
  | LcIdent String
  | LcPair Integer Integer
  | LcList [LcExpr]
  | LcLambda String LcExpr
  | LcApply LcExpr LcExpr

instance Show LcExpr where
  showsPrec _ (LcConstant i) = showString $ show i
  showsPrec _ (LcIdent name) = showString name
  showsPrec _ (LcPair i j) = showString $ show (i, j)
  showsPrec _ (LcList exprs) = showString $ "[" ++ concat (intersperse ", " (fmap show exprs)) ++ "]"
  showsPrec p (LcLambda name body) = showParen (p > 1) $ showString $ renderLambda name body
  showsPrec p (LcApply f x) = showApply p f x

renderLambda left (LcLambda name body) = renderLambda (left ++ " " ++ name) body
renderLambda left body = "\\" ++ left ++ " -> " ++ show body

showApply p f@(LcApply (LcIdent name) argLeft) argRight = case lookup name operators of
  Just (op, pLeft, pRight) -> showParen (p >= max pRight pLeft) $
    showsPrec pLeft argLeft . showString (" " ++ op ++ " ") . showsPrec pRight argRight
  Nothing -> showApplyBasic p f argRight
showApply p f x = showApplyBasic p f x

showApplyBasic p f x = showParen (p > 9) $ showsPrec 9 f . showString " " . showsPrec 10 x

operators = [
  ("add", ("+", 6, 6)),
  ("mul", ("*", 7, 7)),
  ("lt", ("<", 4, 5)),
  ("eq", ("==", 4, 5)),
  ("div", ("`div`", 7, 8)),
  ("or", ("||", 2, 2)),
  ("and", ("&&", 3, 3))]

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


renderSimplified :: Expression -> String
renderSimplified expr =
  let (expr', _) = translate variableNames expr
  in show $ renameVariables [] variableNames expr'

prettyPrintEquation (name, expr) =
  let (expr', _) = translate variableNames expr
      expr'' = renameVariables [] variableNames expr'
      pp :: String -> LcExpr -> String
      pp left (LcLambda a b) = pp (left ++ " " ++ a) b
      pp left other = left ++ " = " ++ show other ++ "\n"
  in pp name expr''

-- Evaluate lambda calculus expressions. Not currently used.
--
-- The idea was to let QuickCheck chew on this. Evaluating the output of
-- `translate` should give the same results as evaluating aliencode
-- directly. Unfortunately, we don't have a good way of checking equality of
-- values since most are functions.
evalLc :: [(String, Value)] -> LcExpr -> Value
evalLc _ (LcConstant i) = IntValue i
evalLc env (LcIdent name) =
  case lookup name env of
    Nothing -> error $ "no such variable found in scope: " ++ name
    Just value -> value
evalLc _ (LcPair x y) = ConsValue (IntValue x) (IntValue y)
evalLc env (LcList exprs) = toValue (map (evalLc env) exprs)
evalLc env funexpr@(LcLambda param body) =
  FunValue (show funexpr) (\argument -> evalLc ((param, argument) : env) body)
evalLc env (LcApply f x) = ap (evalLc env f) (evalLc env x)
