module System.Hardware.ParameterTH (instanceRead, instanceRead', instanceWrite, QueryType (..)) where

import Language.Haskell.TH

data QueryType = CAPQuery | CAPGet

instance' :: Name -> Name -> [Dec] -> InstanceDec
instance' instanceName className = InstanceD Nothing [] (AppT (ConT instanceName) $ ConT className)

instanceRead :: String -> String -> String -> QueryType -> Q [Dec]
instanceRead cmd = instanceRead' cmd cmd

instanceRead' :: String -> String -> String -> String -> QueryType -> Q [Dec]
instanceRead' cmd sep class' par q = do
  let className = mkName class'
      instanceName = mkName "SenseCAPRead"
  get <- genGetValue cmd sep q
  return [instance' instanceName className [genParseValue par, get]]

genParseValue :: String -> Dec
genParseValue parse =
  let fm = VarE $ mkName "<$>"
      coerce = VarE $ mkName "coerce"
      f = VarE $ mkName parse
      par = mkName "parseValue"
   in FunD par [Clause [] (NormalB (InfixE (Just coerce) fm (Just f))) []]

genGetValue :: String -> String -> QueryType -> Q Dec
genGetValue cmd sep typ = do
  cap <- newName "cap"
  let extract = VarE $ mkName "extract"
      fm = VarE $ mkName "<$>"
      qu = VarE $ mkName "querySenseCAP"
      ge = VarE $ mkName "getSenseCAP"
      fname = mkName "getValue"
      cmdLit = LitE $ StringL cmd
      sepLit = LitE $ StringL sep
      sendFun = case typ of
        CAPGet -> ge
        CAPQuery -> qu
  return $ FunD fname [Clause [VarP cap] (NormalB (InfixE (Just $ AppE extract sepLit) fm (Just $ AppE (AppE sendFun (VarE cap)) cmdLit))) []]

instanceWrite :: String -> String -> Maybe String -> Q [Dec]
instanceWrite cmd class' maybeParse = do
  let className = mkName class'
      instanceName = mkName "SenseCAPWrite"
  set <- genSetValue cmd
  return [instance' instanceName className [genUnparseValue maybeParse, set]]

genUnparseValue :: Maybe String -> Dec
genUnparseValue maybeParse = 
  let unpar = mkName "unParseValue"
      coerce = VarE $ mkName "coerce"
      compose = VarE $ mkName "."
  in FunD unpar [Clause [] (case maybeParse of
    Nothing -> NormalB coerce
    Just a -> NormalB $ InfixE (Just $ VarE $ mkName a) compose (Just coerce)) []]

genSetValue :: String -> Q Dec
genSetValue cmd = do
  cap <- newName "cap"
  na <- newName "na"
  let fname = mkName "setValue"
      extract = VarE $ mkName "extract"
      fm = VarE $ mkName "<$>"
      set = VarE $ mkName "setSenseCAP"
      un = VarE $ mkName "unParseValue"
      cmdLit = LitE $ StringL cmd
  return $ FunD fname [Clause [VarP cap, VarP na] (NormalB (InfixE (Just $ AppE extract cmdLit) fm (Just $ AppE (AppE (AppE set (VarE cap)) cmdLit) (AppE un $ VarE na)))) []]