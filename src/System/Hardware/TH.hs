-- | For internal usage, generating instances for parameters. Not for API usage!
module System.Hardware.TH (instancesReadAndJSON, instancesReadAndJSON', instanceWrite, instancesJSON, QueryType (..)) where

import Language.Haskell.TH

data QueryType = CAPQuery | CAPGet

instance' :: Name -> Name -> [Dec] -> InstanceDec
instance' instanceName className = InstanceD Nothing [] (AppT (ConT instanceName) $ ConT className)

instancesReadAndJSON :: String -> String -> String -> QueryType -> Q [Dec]
instancesReadAndJSON cmd = instancesReadAndJSON' cmd cmd

instancesReadAndJSON' :: String -> String -> String -> String -> QueryType -> Q [Dec]
instancesReadAndJSON' cmd sep class' par q = do
  let className = mkName class'
      instanceName = mkName "SenseCAPRead"
  get <- genGetValue cmd sep q
  j <- instancesJSON class'
  return $ [instance' instanceName className [genParseValue par, get]] <> j

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

instancesJSON :: String -> Q [Dec]
instancesJSON c = return $ ($ c) <$> [genFromJSON, genToJSON]

genToJSON :: String -> Dec
genToJSON c = instance' (mkName "ToJSON") (mkName c) []

genFromJSON :: String -> Dec
genFromJSON c = instance' (mkName "FromJSON") (mkName c) []

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
   in FunD
        unpar
        [ Clause
            []
            ( case maybeParse of
                Nothing -> NormalB coerce
                Just a -> NormalB $ InfixE (Just $ VarE $ mkName a) compose (Just coerce)
            )
            []
        ]

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
