module System.Hardware.ParameterTH (instanceRead, QueryType (..)) where

import Language.Haskell.TH

data QueryType = CAPQuery | CAPGet

instanceRead :: String -> String -> String -> QueryType -> Q [Dec]
instanceRead cmd class' par q = do
  let className = mkName class'
      instanceName = mkName "SenseCAPRead"
  get <- genGetValue cmd q
  return [InstanceD Nothing [] (AppT (ConT instanceName) $ ConT className) [genParseValue par, get]]

genParseValue :: String -> Dec
genParseValue parse =
  let fm = VarE $ mkName "<$>"
      coerce = VarE $ mkName "coerce"
      f = VarE $ mkName parse
      par = mkName "parseValue"
   in FunD par [Clause [] (NormalB (InfixE (Just coerce) fm (Just f))) []]

genGetValue :: String -> QueryType -> Q Dec
genGetValue cmd typ = do
  cap <- newName "cap"
  let extract = VarE $ mkName "extract"
      fm = VarE $ mkName "<$>"
      qu = VarE $ mkName "querySenseCAP"
      ge = VarE $ mkName "getSenseCAP"
      fname = mkName "getValue"
      cmdLit = LitE $ StringL cmd
      sendFun = case typ of
        CAPGet -> ge
        CAPQuery -> qu
  return $ FunD fname [Clause [VarP cap] (NormalB (InfixE (Just $ AppE extract cmdLit) fm (Just $ AppE (AppE sendFun (VarE cap)) cmdLit))) []]
