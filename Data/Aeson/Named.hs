{-# LANGUAGE KindSignatures, TemplateHaskell, DataKinds #-}
module Data.Aeson.Named where

import GHC.TypeLits
import Data.Aeson (ToJSON, toJSON, object, (.=))
import Language.Haskell.TH

newtype Field (n :: Symbol) v = Field { unField :: v }
  deriving Show

deriveToJSONFields ty = do
  t <- reify ty
  let nfTy = tail $ nameBase ty
  case t of
    TyConI (DataD ctx _ ts [cs] der) -> do
      let (n, cs') = case cs of
                      -- NormalC n xs -> (n, [(Nothing, t) | (_, t)    <- xs])
                      RecC n xs ->    (n, [(nz, t) | (nz, _, t) <- xs])
      fs <- sequence [(,) (fieldName x) `fmap` newName "a" | (_, x) <- cs']
      sequence [
          dataD (return ctx) (mkName nfTy) ts [nameDup n cs'] der
        , instanceD (return []) (appT (conT ''ToJSON) (conT ty)) [
            funD 'toJSON [clause [conP n (map (varP . snd) fs)] (normalB (
              appE (varE 'object) (listE [
                appE (appE (varE '(.=)) (litE (StringL fk)))
                     (appE (varE 'unField) (varE fv))
              | (fk, fv) <- fs ])
            )) []]]
        , instanceD (return []) (appT (conT ''ToJSON) (conT (mkName nfTy))) [
            funD 'toJSON [clause [conP (mkName (tail (nameBase n))) (map (varP . snd) fs)] (normalB (
              appE (varE 'object) (listE [
                appE (appE (varE '(.=)) (litE (StringL fk)))
                     (varE fv)
              | (fk, fv) <- fs ])
            )) []]]
        ]
    _ -> error "single constr only for now"
  where
    fieldName :: Type -> String
    fieldName (AppT (AppT (ConT _Name) (LitT (StrTyLit s))) _) = s

    nameDup :: Name -> [(Name, Type)] -> Q Con
    nameDup c xs = recC (mkName (tail (nameBase c))) (map (\(n, (AppT _ t)) -> varStrictType (mkName (tail (nameBase n))) (strictType notStrict (return t))) xs)
