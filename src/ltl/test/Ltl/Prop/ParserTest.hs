{-# LANGUAGE OverloadedStrings #-}
module Ltl.Prop.ParserTest where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Test.QuickCheck as QC
import Test.HUnit

import Ltl.Prop
import Ltl.Prop.Parser

ppI :: IntExpr -> QC.Gen Text
ppI (IVarAdd v 0) = QC.elements
  [ Text.pack v,
    Text.pack v <> "+0"
  ]
ppI (IVarAdd v k) = pure $ Text.pack v <> "+" <> Text.pack (show k)
ppI (IConst k) = pure $ Text.pack (show k)


ppE :: Expr -> QC.Gen Text
ppE (IntLang ie) = ppI ie

ppP :: Predicate -> QC.Gen Text
ppP (Eq l r) = do
  lr <- ppE l
  rr <- ppE r
  pure $ lr <> " = " <> rr

-- this is a generator so that we can give different representations for the formula
pp :: Formula -> QC.Gen Text
pp f = maybeParens $ case f of
  TT -> pure "TT"
  FF -> pure "FF"
  P p -> ppP p
  Always f -> do
    box <- QC.elements ["always", "[]"]
    fr <- pp f
    pure $ box <> " " <> fr
  Eventually f -> do
    box <- QC.elements ["eventually", "<>"]
    fr <- pp f
    pure $ box <> " " <> fr
  ForallInt is i f -> do
    fr <- pp f
    pure $ "forall " <> Text.pack i <> " in " <> Text.pack (show is) <> "." <> fr
  ExistsInt is i f -> do
    fr <- pp f
    pure $ "exists " <> Text.pack i <> " in " <> Text.pack (show is) <> "." <> fr
  ForallNode n f -> do
    fr <- pp f
    pure $ "forall " <> Text.pack n <> "." <> fr
  ExistsNode n f -> do
    fr <- pp f
    pure $ "exists " <> Text.pack n <> "." <> fr
  Neg f -> do
    box <- QC.elements ["~", "!"]
    fr <- pp f
    pure $ box <> "(" <> fr <> ")"
  And l r -> do
    c <- QC.elements ["&&", "/\\"]
    lr <- pp l
    rr <- pp r
    -- Would be good to not always have the parens
    pure $ "(" <> lr <> ")" <> c <> "(" <> rr <> ")"
  Or l r -> do
    c <- QC.elements ["||", "\\/"]
    lr <- pp l
    rr <- pp r
    -- Would be good to not always have the parens
    pure $ "(" <> lr <> ")" <> c <> "(" <> rr <> ")"
  Imp l r -> do
    lr <- pp l
    rr <- pp r
    -- Would be good to not always have the parens
    pure $ "(" <> lr <> ")" <> "->" <> "(" <> rr <> ")"
  where
    maybeParens g = QC.frequency
      [ (1, g >>= \x -> pure $ "(" <> x <> ")"), (10, g)]

stringVar :: QC.Gen String
stringVar = QC.elements ["x", "y", "z"] -- do better

-- TODO we should have no spaces in the var
genI :: QC.Gen IntExpr
genI = QC.oneof
  [ IVarAdd <$> stringVar <*> QC.arbitrary,
    IVarAdd <$> stringVar <*> pure 0,
    IConst <$> QC.arbitrary
  ]

genE :: QC.Gen Expr
genE = QC.oneof
  [ IntLang <$> genI
  ]

genP :: QC.Gen Predicate
genP = Eq <$> genE <*> genE

genF :: QC.Gen Formula
genF = QC.sized $ go
  where
    go 0 = QC.oneof
      [ pure TT,
        pure FF,
        P <$> genP
       ]
    go n = QC.oneof
      [ go 0,
        QC.elements [Always, Eventually, Neg] <*> go (n `div` 2),
        QC.elements [Imp, And, Or] <*> go (n `div` 2) <*> go (n `div` 2),
        QC.elements [ForallNode, ExistsNode] <*> stringVar <*> go (n `div` 2),
        QC.elements [ForallInt, ExistsInt] <*> QC.arbitrary <*> stringVar <*> go (n `div` 2)
      ]

prop_can_parse :: QC.Property
prop_can_parse =
  QC.forAll genF $ \f ->
  QC.forAll (pp f) $ \ repr ->
  QC.counterexample ("String repr: " ++ show repr) $
  case parse repr of
    Left _ -> False
    Right f' -> f == f'

unit_imp_right_assoc :: Assertion
unit_imp_right_assoc = case parse "TT -> FF -> TT" of
  Left l -> assertFailure l
  Right f -> f @?= (Imp TT (Imp FF TT))
