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

instance Arbitrary Shape where
  arbitrary = MkShape . V.fromList <$> listOf (arbitrary :: Gen Card) 

instance Arbitrary Card where 
  arbitrary = oneof [n, v]
    where 
      n = CardN <$> arbitrarySizedNatural
      v = CardFV . T.pack <$> listOf1 (elements ['A' .. 'Z'])
      

instance Arbitrary Ty where 
    arbitrary = do 
      sh <- arbitrary 
      el <- elements [REAL, INT]
      return $ Ty sh el


check_broadcastsTo :: SpecWith ()
check_broadcastsTo = describe "shape broadcasts to : " $ do
  it "shape broadcasts to itseld" $ do 
    property $ \sh ->
            sh `shapeBroadcastsTo` sh
  it "scalar broadcasts to anything" $ do 
    property $ \sh ->
            [] `shapeBroadcastsTo` sh
  it "non scalar does not broadcast to scalar" $ do 
    property $ \x sh -> let sh' = shCons x sh in 
            not $ sh' `shapeBroadcastsTo` []
  
  it "axes with 1 broadcast up arbitrarily" $ do 
    property $ \sh_mask -> 
      let 
        (sh, mask) = (fst <$> sh_mask, snd <$> sh_mask)
        make_1s mask = zipWith (\b x -> if b then x else CardN 1) mask
        sh_1 = make_1s mask sh
      in  (shFromList sh_1) `shapeBroadcastsTo` (shFromList sh)

  it "shape broadcasts to itself plus a prefix" $ do 
    property $ \sh prefix ->
      let 
        sh_rhs = prefix <> sh
        sh_lhs = sh
      in sh_lhs `shapeBroadcastsTo` sh_rhs

check_shDiff :: SpecWith ()
check_shDiff = describe "shape difference: " $ do
  it "shape minus nothing" $ do 
    property $ \sh ->
      shDiff sh [] == (if shRank sh == 0 then Nothing else Just sh)
  it "shape minus itself" $ do 
    property $ \sh -> 
        sh `shDiff` sh == Nothing
  it "prefix + shape - shape = prefix" $ do 
    property $ \prefix sh -> 
      (prefix <> sh) `shDiff` sh == 
        (if shRank prefix == 0 then Nothing else Just prefix)


-- check_unify :: SpecWith _
check_unify = describe "unification" $ do 
  it "arguments of exactly the right type unify" $ do 
    property $ \tys ty ->  
      let 
        args = (\t -> ("", t)) <$> tys
      in (unify tys (FunctionTy 0 args ty)) `shouldBe` (Right (Nothing, ty)) 

  it "broadcasts over common prefix" $ do 
    property $ \prefix tys ty ->  
      let 
        tys' = [ Ty (prefix <> sh) el | (Ty sh el) <- tys]
        fTy = FunctionTy 0 ((\t -> ("", t)) <$> tys) ty 
        ty' = let Ty sh el = ty in 
          if length tys > 0 
          then Ty (prefix <> sh) el
          else Ty sh el
        prefix_ = if (shRank prefix > 0)  && (length tys > 0) then (Just prefix) else Nothing
      in (unify tys' fTy) `shouldBe` (Right (prefix_, ty')) 

  it "scalar function can take anything" $ do 
    property $ \ty ->  
      let 
        Ty sh elt = ty 
        scalarFn = FunctionTy 0 [("x", Ty [] elt)] (Ty [] elt)
      in (unify [ty] scalarFn) `shouldBe` (
        case sh of [] -> Right (Nothing, ty) ; _ -> (Right (Just sh, ty))
      )

  it "vecToScalar" $ do 
    property $ \ty card ->  
      let 
        Ty sh elt = ty
        ty' = Ty (sh <> [card]) elt 
        funTy = FunctionTy 1 [("x", Ty [CardBV 0] elt)] (Ty [] elt)
      in (unify [ty'] funTy) `shouldBe` (
        case sh of [] -> Right (Nothing, ty) ; _ -> (Right (Just sh, ty))
      )