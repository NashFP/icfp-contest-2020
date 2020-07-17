module Exp where

import Data.Int

-- https://message-from-space.readthedocs.io/en/latest/condensed-version.html

-- 
data Exp = 
    Ap
  | Num Int64
  | Var
  | Bool
  | Exp2
  | Car
  | Cdr
  | Nil
  | ListSyn
  | Vec
  | Draw
  | DrawMany
  | StatelessDraw
  | StatefulDraw
  | Lam
  | Mod
  | Demod
  | ModList
  | Send
  | IsZero
  | Interact
  deriving Show

data Comb = 
    KC
  | SC
  | CC
  | BC
  | IC
  deriving Show

data Binop =
    Succ
  | Pred
  | Neg
  | StrictLT
  deriving Show

data Monop =
    Sum
  | Prod
  | Div
  deriving Show