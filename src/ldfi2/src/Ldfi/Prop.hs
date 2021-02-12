{-# LANGUAGE DeriveFoldable #-}
module Ldfi.Prop where

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Test.QuickCheck as QC

------------------------------------------------------------------------
-- * Propositional logic formulae

infixr 3 :&&
infixr 2 :||

data FormulaF var
  = FormulaF var :&& FormulaF var
  | FormulaF var :|| FormulaF var
  | And [FormulaF var]
  | Or [FormulaF var]
  | Neg (FormulaF var)
  | TT
  | FF
  | Var var
  deriving (Eq, Show, Foldable)

type Formula = FormulaF String

genFormula :: [v] -> Int -> QC.Gen (FormulaF v)
genFormula env 0 = QC.oneof
  [ pure TT
  , pure FF
  , Var <$> QC.elements env
  ]
genFormula env size =
  QC.oneof [ genFormula env 0
           , Neg <$> genFormula env (size - 1)
           , QC.elements [(:&&), (:||)] <*> genFormula env (size `div` 2) <*> genFormula env (size `div` 2)
           , do
               n <- QC.choose (0, size `div` 2)
               QC.elements [And, Or] <*> QC.vectorOf n (genFormula env (size `div` 4))
           ]

instance QC.Arbitrary v => QC.Arbitrary (FormulaF v) where
  arbitrary = do
    env <- QC.listOf1 QC.arbitrary
    QC.sized (genFormula env)

getVars :: Ord var => FormulaF var -> Set var
getVars = foldMap Set.singleton

simplify1 :: Formula -> Formula
simplify1 (TT :&& r)  = simplify1 r
simplify1 (l  :&& r)  = simplify1 l :&& simplify1 r
simplify1 (FF :|| r)  = simplify1 r
simplify1 (_l :|| TT) = TT
simplify1 (l  :|| r)  = simplify1 l :|| simplify1 r
simplify1 (And [])    = TT
simplify1 (And [f])   = f
simplify1 (And fs)    = And (map simplify1 fs)
simplify1 (Neg f)     = Neg (simplify1 f)
simplify1 f           = f

-- simplify (TT :&& r)     = simplify r
-- simplify (And xs :&& y) = And (map simplify xs ++ [simplify y])
-- simplify (x :&& And ys) = And (simplify x : map simplify ys)
-- simplify (FF :|| r)     = simplify r
-- simplify (l  :|| r)     = simplify l :|| simplify r
-- simplify (And fs)       = case filter (/= TT) . (>>= expandAnd) $ map simplify fs of
--   [] -> TT
--   [f] -> f
--   fs' -> And fs'
--   where
--     expandAnd (And xs) = xs
--     expandAnd (l :&& r) = [l, r]
--     expandAnd f = [f]

fixpoint :: Formula -> Formula
fixpoint f | simplify1 f == f = f
           | otherwise        = fixpoint (simplify1 f)

makeVars :: Set String -> Formula
makeVars = And . map Var . Set.toList
