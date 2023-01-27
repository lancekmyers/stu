{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}

module Types where

import Control.Monad.Except
import Control.Monad.State
import Data.List (intercalate, maximumBy)
import qualified Data.Map.Strict as M
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Exts (IsList (..))
import Prettyprinter
import GHC.Exts (IsList (..))

data ElTy
  = REAL
  | INT
  | BOOL
  | IND Card
  deriving (Eq)

instance Pretty ElTy where pretty = viaShow

instance Show ElTy where
  show REAL = "real"
  show INT = "int"
  show (IND card) = show card

newtype Shape = MkShape {getVec :: Vector Card}
  deriving newtype (Eq, Show, Semigroup, Monoid)

instance IsList Shape where
  type Item Shape = Card
  fromList = MkShape . V.fromList
  toList = V.toList . getVec

prettyShape :: Shape -> Doc ann
prettyShape sh = tupled . fmap viaShow $ V.toList (getVec sh)

data Ty = Ty {shape :: Shape, elTy :: ElTy}
  deriving (Eq)

rank :: Ty -> Int
rank (Ty sh _) = length (getVec sh)

shRank :: Shape -> Int
shRank = V.length . getVec

shTake :: Int -> Shape -> Shape
shTake n (MkShape v) = MkShape (V.take n v)

shDrop :: Int -> Shape -> Shape
shDrop n (MkShape v) = MkShape (V.drop n v)

shCons :: Card -> Shape -> Shape
shCons c (MkShape v) = MkShape (V.cons c v)

shUncons :: Shape -> Maybe (Card, Shape)
shUncons (MkShape v) = case V.uncons v of
  Nothing -> Nothing
  Just (c, sh) -> Just (c, MkShape sh)

shFromList :: [Card] -> Shape
shFromList = MkShape . V.fromList

shToList :: Shape -> [Card]
shToList = V.toList . getVec

instance Pretty Ty where pretty = viaShow

instance Show Ty where
  show (Ty sh el) = show sh <> show el

data Card
  = CardN Int
  | CardFV Text
  | CardBV Text -- polymorphic var
  -- maybe eventually existentially bound variables?
  deriving (Eq)

instance Pretty Card where pretty = viaShow

instance Show Card where
  show (CardN n) = "#" <> show n
  show (CardFV v) = "#" <> T.unpack v
  show (CardBV i) = '\'' : (T.unpack i)

cardMatches (CardN 1) x = True
cardMatches x (CardN 1) = True
cardMatches x y = x == y

broadcast :: Shape -> Shape -> Maybe Shape
broadcast (MkShape xs) (MkShape ys) = MkShape . mappend prefix <$> zs
  where
    go (CardN 1) y = Just y
    go x (CardN 1) = Just x
    go x y = if x == y then Just x else Nothing
    prefix
      | (V.length xs > V.length ys) = V.take (V.length xs - V.length ys) xs
      | otherwise = V.take (V.length ys - V.length xs) ys

    zs = sequence . V.reverse $ V.zipWith go (V.reverse xs) (V.reverse ys)

-- | Does sh broadcast to sh'?
broadcastsTo :: Ty -> Ty -> Bool
broadcastsTo (Ty sh el) (Ty sh' el') = (el == el') && (shapeBroadcastsTo sh sh')

shapeBroadcastsTo :: Shape -> Shape -> Bool
shapeBroadcastsTo (MkShape sh) (MkShape sh')
  | length sh > length sh' = False
  | otherwise = and $ V.zipWith go (V.reverse sh) (V.reverse sh')
  where
    n = length sh
    go (CardN 1) _ = True
    go x y = x == y

shDiff :: Shape -> Shape -> Maybe Shape
shDiff (MkShape sh') (MkShape sh) =
  if n > 0 then Just (MkShape prefix) else Nothing
  where
    larger = if (V.length sh < V.length sh') then sh' else sh
    n = abs $ V.length sh - V.length sh'
    prefix = V.take n larger

data FunctionTy
  = FunctionTy [(Text, Ty)] Ty

instance Show FunctionTy where
  show (FunctionTy args ret) = "(" ++ args_shown ++ ") -> " ++ (show ret)
    where
      showArg (name, ty) = T.unpack name <> " : " <> (show ty)
      args_shown = intercalate ", " $ showArg <$> args

instance Pretty FunctionTy where
  pretty (FunctionTy args ret) = align $ tupled args' <+> "->" <+> (pretty ret)
    where
      args' = [pretty name <+> ":" <+> pretty ty | (name, ty) <- args]

-- TODO: rename to UnifErr
data TyErr = TyErr String
  deriving (Eq, Show)

type CardMap = M.Map Text Card

-- TODO: use mutable veector for this
cardUnify ::
  (MonadError TyErr m) =>
  (Card, Card) ->
  StateT CardMap m ()
cardUnify (c, (CardBV i)) = do
  cmap <- get
  case M.lookup i cmap of
    Nothing -> put $ M.insert i c cmap
    Just c' -> if c == c' then pure () else throwError (TyErr "")
cardUnify (c, c') = if c == c' then pure () else throwError (TyErr "ahhh")

shapeUnify ::
  (MonadError TyErr m) =>
  (Shape, Shape) ->
  StateT CardMap m ()
shapeUnify (MkShape sh_given, MkShape sh_expected) = do
  let n_given = V.length sh_given
  let n_expected = V.length sh_expected
  let sh' = V.drop (n_expected - n_given) sh_expected
  forM_ (V.zip sh_given sh') cardUnify

go (x, y)
  | x == y = pure ()
  | otherwise = throwError (TyErr "unequal")

substitute :: CardMap -> Ty -> Ty
substitute cmap (Ty sh elty) = Ty (MkShape sh') elty
  where
    sh' = substGo <$> (getVec sh)
    substGo (CardBV i) = cmap M.! i
    substGo c = c

-- | unifies function type with arguments, returns error or function return type and broadcasting shape
unify :: [Ty] -> FunctionTy -> Either TyErr (Maybe Shape, Ty)
unify [] (FunctionTy [] ty) = Right (Nothing, ty)
unify [] (FunctionTy xs ty) = Left $ TyErr ""
unify xs (FunctionTy [] ty) = Left $ TyErr ""
unify tys (FunctionTy args ret) = do
  let err x = Left (TyErr x)

  let tys' = map snd args
  let shs  = shape <$> tys -- given shapes
  let shs' = shape <$> tys' -- expected shapes
  let ranks = shRank <$> shs
  let ranks' = shRank <$> shs'

  -- enusre element types match
  when (not . and $ zipWith (==) (elTy <$> tys) (elTy <$> tys')) (err "1")

  -- args must be of rank at least that of the parameters
  when (not . and $ zipWith (>=) ranks ranks') (err "2")

  let prefixes = zipWith shTake (zipWith (-) ranks ranks') shs
  let suffixes = zipWith shDrop (zipWith (-) ranks ranks') shs

  let longest_prefix = maximumBy (comparing shRank) prefixes
  let prefixes_broadcast = all (`shapeBroadcastsTo` longest_prefix) prefixes

  when (not prefixes_broadcast) (err "bad shapes 1")

  let cz = zip suffixes shs'

  cmap <- runExcept $ execStateT (forM_ cz shapeUnify) mempty
  {-
  let Ty ret_sh ret_el = substitute cmap ret
  return $ case prefixes of
    sh : _ -> (_, Ty (sh <> ret_sh) ret_el)
    [] -> (_, Ty ret_sh ret_el)
  -}
  let Ty ret_sh ret_el = substitute cmap ret
  let br_sh =
        if shRank longest_prefix > 0
          then Just longest_prefix
          else Nothing

  return (br_sh, Ty (longest_prefix <> ret_sh) ret_el)

real = Ty mempty REAL

-- (=:) :: Text -> Ty -> (Text, Ty)
-- name := ty = (name, ty)
