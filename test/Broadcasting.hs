{-# LANGUAGE OverloadedLists, OverloadedStrings  #-}

module Broadcasting where 
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Types
import Control.Monad (forM)
import qualified Data.Vector  as V
import qualified Data.Text  as T
import qualified Data.List.NonEmpty as NEL

mkShape :: [Either String Int] -> Shape 
mkShape = V.fromList . map (either (CardFV . T.pack) CardN)

mkTy :: (Bool, [Either String Int]) -> Ty 
mkTy (True,  sh) = Ty (mkShape sh) REAL
mkTy (False, sh) = Ty (mkShape sh) INT

instance Arbitrary Card where 
    arbitrary = either (CardFV . T.pack) (CardN . abs) <$> arbitrary

instance Arbitrary Ty where 
    arbitrary = mkTy <$> arbitrary


check_broadcastsTo :: SpecWith ()
check_broadcastsTo = describe "shape broadcasts to : " $ do
  it "shape broadcasts to itseld" $ do 
    property $ \sh' -> let sh = (V.fromList . map CardN) sh' in 
            sh `shapeBroadcastsTo` sh
  it "scalar broadcasts to anything" $ do 
    property $ \sh' -> let sh = (V.fromList . map CardN) sh' in 
            [] `shapeBroadcastsTo` sh
  it "non scalar does not broadcast to scalar" $ do 
    property $ \x sh' -> let sh = (V.fromList . map CardN . (x:)) sh' in 
            not $ sh `shapeBroadcastsTo` []
  
  it "axes with 1 broadcast up arbitrarily" $ do 
    property $ \sh_mask -> 
      let 
        (sh', mask) = (fst <$> sh_mask, snd <$> sh_mask)
        make_1s mask = V.zipWith (\b x -> if b then x else CardN 1 ) mask
        sh = mkShape sh' 
        sh_1 = make_1s (V.fromList mask) sh
      in  sh_1 `shapeBroadcastsTo` sh

  it "shape broadcasts to itself plus a prefix" $ do 
    property $ \sh prefix ->
      let 
        sh_rhs = mkShape $ prefix ++ sh
        sh_lhs = mkShape sh
      in sh_lhs `shapeBroadcastsTo` sh_rhs

check_shDiff :: SpecWith ()
check_shDiff = describe "shape difference: " $ do
  it "1" $ do 
    property $ \sh' -> 
      let 
        sh = mkShape sh'
      in 
        shDiff sh [] == (if V.length sh == 0 then Nothing else Just sh)
  it "2" $ do 
    property $ \sh' -> 
      let 
        sh = mkShape sh' 
      in 
        sh `shDiff` sh == Nothing
  it "3" $ do 
    property $ \prefix' sh' -> 
      let 
        sh = mkShape sh'
        prefix = mkShape prefix' 
      in 
        (prefix <> sh) `shDiff` sh == 
          (if V.length prefix == 0 then Nothing else Just prefix)


-- check_unify :: SpecWith _
check_unify = describe "unification" $ do 
  it "desc" $ do 
    property $ \tys' ty ->  
      let 
        tys = tys'
        args = (\t -> ("", t)) <$> tys
      in (unify tys (FunctionTy 0 args ty)) == (Right (Nothing, ty)) 

  it "desc2" $ do 
    property $ \prefix' tys' ty ->  
      let 
        prefix = mkShape prefix' 
        tys = [ Ty (prefix <> sh) el | (Ty sh el) <- tys']
        args = (\t -> ("", t)) <$> tys
      in (unify tys (FunctionTy 0 args ty)) == (Right (Nothing, ty)) 

  it "scalar function can take anything" $ do 
    property $ \ty ->  
      let 
        Ty sh elt = ty 
        scalarFn = FunctionTy 0 [("x", Ty [] elt)] (Ty [] elt)
      in (unify [ty] scalarFn) == (
        case sh of [] -> Right (Nothing, ty) ; _ -> (Right (Just sh, ty))
      )

  it "vecToScalar" $ do 
    property $ \ty card ->  
      let 
        Ty sh elt = ty
        ty' = Ty (sh <> [card]) elt 
        funTy = FunctionTy 1 [("x", Ty [CardBV 0] elt)] (Ty [] elt)
      in (unify [ty'] funTy) == (
        case sh of [] -> Right (Nothing, ty) ; _ -> (Right (Just sh, ty))
      )