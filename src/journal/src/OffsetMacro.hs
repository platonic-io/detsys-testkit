{-# LANGUAGE TemplateHaskell #-}
module OffsetMacro where

import Foreign.Storable (sizeOf)
import Language.Haskell.TH

offsetTo :: Name -> Q Exp
offsetTo name = do
  info <- reify name
  let abort = fail $ "offsetTo requires to be given a field name (of a non-polymorphic type, TODO)"
  case info of
    VarI _name_again (AppT (AppT ArrowT (ConT typName)) _result_type) _unused -> do
      typInfo <- reify typName
      case typInfo of
        TyConI (DataD _context
                      _typName_again
                      []
                      _kind
                      [RecC _name_constructor bindings]
                      _derive) | name `elem` map (\(n,_,_) ->n) bindings -> do
          -- we are not taking alignment into account
          let typesToSum = map (\(_,_,t)->t) $ takeWhile (\(n,_,_) -> n /= name) bindings
          let xs = flip map typesToSum $ \t -> AppE (VarE 'sizeOf) (SigE (VarE 'undefined) t)
          -- would be nice to run this at compile time
          pure $ foldr (\e e' -> UInfixE e (VarE '(+)) e') (LitE $ IntegerL 0) xs
        _ -> abort
    _ -> abort
