{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Prelude hiding ((<>))

import Data.Version
import Data.Coerce
import Text.ParserCombinators.ReadP
import Data.List (unfoldr, dropWhileEnd, sort, groupBy, isPrefixOf, intercalate)
import Data.Char (isSpace)
import Data.Function (on)
import Data.Maybe (listToMaybe, catMaybes)
import Data.Functor ((<&>))
import GHC.Read (list)
import Text.PrettyPrint.HughesPJClass

vsPandoc :: String
vsPandoc =
    "3.1.8, 3.1.7, 3.1.6.2, 3.1.6.1, 3.1.6, 3.1.5, 3.1.4, \
    \ 3.1.3, 3.1.2, 3.1.1, 3.1, 3.0.1, 3.0, 2.19.2, 2.19.1, 2.19, 2.18, 2.17.1.1, \
    \ 2.17.1, 2.17.0.1, 2.17, 2.16.2, 2.16.1, 2.16, 2.15, 2.14.2, 2.14.1, 2.14.0.3, \
    \ 2.14.0.2, 2.14.0.1, 2.14, 2.13, 2.12, 2.11.4, 2.11.3.2, 2.11.3.1, 2.11.3, \
    \ 2.11.2, 2.11.1.1, 2.11.1, 2.11.0.4, 2.11.0.2, 2.11.0.1, 2.11, 2.10.1, 2.10, \
    \ 2.9.2.1, 2.9.2, 2.9.1.1, 2.9.1, 2.9, 2.8.1, 2.8.0.1, 2.8, 2.7.3, 2.7.2, 2.7.1, \
    \ 2.7, 2.6, 2.5, 2.4, 2.3.1, 2.3, 2.2.3.2, 2.2.3.1, 2.2.3, 2.2.2.1, 2.2.2, 2.2.1, \
    \ 2.2, 2.1.3, 2.1.2, 2.1.1, 2.1, 2.0.6, 2.0.5, 2.0.4, 2.0.3, 2.0.2, 2.0.1.1, \
    \ 2.0.1, 2.0.0.1, 2.0, 1.19.2.4, 1.19.2.3, 1.19.2.2, 1.19.2.1, 1.19.2, 1.19.1, \
    \ 1.19, 1.18, 1.17.2, 1.17.1, 1.17.0.3, 1.17.0.2, 1.17.0.1, 1.17, 1.16.0.2, \
    \ 1.16.0.1, 1.16, 1.15.2.1, 1.15.2, 1.15.1.1, 1.15.1, 1.15.0.6, 1.15.0.5, \
    \ 1.15.0.4, 1.15.0.3, 1.15.0.2, 1.14.1, 1.13.2.1, 1.13.2, 1.13.1, 1.13.0.1, 1.13, \
    \ 1.12.4.2, 1.12.4, 1.12.3.3, 1.12.3.2, 1.12.3.1, 1.12.3, 1.12.2.1, 1.12.2, \
    \ 1.12.1, 1.12.0.2, 1.12.0.1, 1.12, 1.11.1, 1.11, 1.10.1, 1.10.0.5, 1.10.0.4, \
    \ 1.10.0.3, 1.10.0.2, 1.10.0.1, 1.10, 1.9.4.5, 1.9.4.4, 1.9.4.3, 1.9.4.2, 1.9.4.1, \
    \ 1.9.4, 1.9.3, 1.9.2, 1.9.1.2, 1.9.1.1, 1.9.1, 1.9.0.5, 1.9.0.4, 1.9.0.3, \
    \ 1.9.0.2, 1.9, 1.8.2.1, 1.8.2, 1.8.1.2, 1.8.1.1, 1.8.1, 1.8.0.3, 1.8.0.2, \
    \ 1.8.0.1, 1.8, 1.6.0.1, 1.6, 1.5.1.1, 1.5.1, 1.5.0.1, 1.5, 1.4, 1.3, 1.2.1, 1.2, \
    \ 1.1, 1.0.0.1, 1.0, 0.46, 0.45, 0.44, 0.43, 0.42, 0.41, 0.4, 2.11.0.3, 1.15.0.1, \
    \ 1.15, 1.14.0.4, 1.14.0.3, 1.14.0.2, 1.14.0.1, 1.14"

vsCabal :: String
vsCabal =
    "3.10.2.1, 3.10.2.0, 3.10.1.0, 3.8.1.0, 3.6.3.0, 3.6.2.0, 3.6.1.0, 3.6.0.0, \
    \ 3.4.1.0, 3.4.0.0, 3.2.1.0, 3.2.0.0, 3.0.2.0, 3.0.1.0, 3.0.0.0, 2.4.1.0, 2.4.0.1, \
    \ 2.4.0.0, 2.2.0.1, 2.2.0.0, 2.0.1.1, 2.0.1.0, 2.0.0.2, 1.24.2.0, 1.24.0.0, \
    \ 1.22.8.0, 1.22.7.0, 1.22.6.0, 1.22.5.0, 1.22.4.0, 1.22.3.0, 1.22.2.0, 1.22.1.1, \
    \ 1.22.1.0, 1.22.0.0, 1.20.0.4, 1.20.0.3, 1.20.0.2, 1.20.0.1, 1.20.0.0, 1.18.1.7, \
    \ 1.18.1.6, 1.18.1.5, 1.18.1.4, 1.18.1.3, 1.18.1.2, 1.18.1.1, 1.18.1, 1.18.0, \
    \ 1.16.0.3, 1.16.0.2, 1.16.0.1, 1.16.0, 1.14.0, 1.12.0, 1.10.2.0, 1.10.1.0, \
    \ 1.10.0.0, 1.8.0.6, 1.8.0.4, 1.8.0.2, 1.6.0.3, 1.6.0.2, 1.6.0.1, 1.4.0.2, \
    \ 1.4.0.1, 1.4.0.0, 1.2.4.0, 1.2.3.0, 1.2.2.0, 1.2.1, 1.1.6, 1.24.1.0"

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_ : xs) = xs

unintersperse :: Char -> String -> [String]
unintersperse mark = unfoldr unintersperse1
  where
    unintersperse1 str
      | null str = Nothing
      | otherwise =
          let (this, rest) = break (== mark) str
           in Just (this, safeTail rest)

parseVer :: String -> Version
parseVer s = case [ v | (v,"") <- readP_to_S parseVersion s ] of
    (v:_) -> v
    []    -> error ("parseVer: " ++ show s)

main :: IO ()
main = do
    let ps = unintersperse ',' vsPandoc
    let vs  = parseVer . trim <$> ps
    -- print ps
    -- sequence_ $ print . showVersion <$> vs

    putStrLn "\nSTEP-0"
    let bs :: [V0] = versionBranch <$> vs
    sequence_ $ print <$> sort bs

    putStrLn "\nV-0"
    let v0 = mkV0 bs
    putStrLn . render $ pPrint v0

    putStrLn "\nSTEP-1"
    let step1 :: [(Maybe Int, [V0])] = mkStep bs
    sequence_ $ print <$> step1

    putStrLn "\nV-1'"
    let v1s' = V1s $ mkV1 <$> step1
    putStrLn . render $ pPrint v1s'
    putStrLn "\nV-1"
    let v1s = V1s $ mkVerStep bs
    putStrLn . render $ pPrint v1s

    putStrLn "\nSTEP-2"
    let step2 :: [(Maybe Int, [(Maybe Int, [V0])])] = fmap (fmap mkStep) step1
    sequence_ $ print <$> step2

    putStrLn "\nV-2"

    putStrLn "\nSTEP-3"
    let step3 :: [(Maybe Int, [(Maybe Int, [(Maybe Int, [V0])])])] = (fmap . fmap . fmap) (fmap mkStep) step2
    sequence_ $ print <$> step3
    putStrLn "\nSTEP-4"
    let step4 = (fmap . fmap . fmap . fmap . fmap) (fmap mkStep) step3
    sequence_ $ print <$> step4

-- >>> mkStep [[0,4],[0,41],[0,42]]
-- [(Just 0,[[4],[41],[42]])]
--
-- >>> mkStep [[3,0,1],[3,1],[3,1,1],[3,1,2],[3,1,3],[3,1,4],[3,1,5],[3,1,6],[3,1,6,1],[3,1,6,2],[3,1,7],[3,1,8]]
-- [(Just 3,[[0,1],[1],[1,1],[1,2],[1,3],[1,4],[1,5],[1,6],[1,6,1],[1,6,2],[1,7],[1,8]])]
mkStep :: [V0] -> [(Maybe Int, [V0])]
mkStep (sort -> bs) =
    filter (\case
        (Nothing, []) -> False
        _ -> True
    ) .
    (fmap assoc) <$>
    groupBy ((==) `on` fst) $ (\vs -> (listToMaybe vs, drop 1 vs)) <$> bs

assoc :: [(Maybe Int, [Int])] -> (Maybe Int, [V0])
assoc xs =
    (listToMaybe vs, sort ws)
    where
        (vs, ws) = unzip [(x, ys) | (Just x, ys) <- xs]


mkVerStep :: [V0] -> [V1]
mkVerStep =
    (\xs ->
        [ V1 i [V0s ys] | (i@(Just _), ys) <- xs]
        )
    . mkStep

showVer :: [Int] -> ShowS
showVer = showString . showVersion . makeVersion

-- | Raw versions.
type V0 = [Int]

newtype V0s = V0s [V0]
    deriving (Show)

-- | First group of versions.
data V1 = V1 { v1 :: Maybe Int, w1 :: [V0s] }
    deriving (Show)

newtype V1s = V1s [V1]
    deriving (Show)

-- | Second group of versions.
data V2 = V2 { v2 :: Maybe Int, w2 :: [V1] }
    deriving (Show)

newtype V2s = V2s [V2]
    deriving (Show)

-- | Second group of versions.
data V3 = V3 { v3 :: Maybe Int, w3 :: [V2] }
    deriving (Show)

mkV0 :: [V0] -> V0s
mkV0 = V0s . sort

mkV1 :: (Maybe Int, [V0]) -> V1
mkV1 (v, vs) = V1 v $ [mkV0 $ filter (not . null) vs]

-- >>> pPrint $ V0s [[0,4],[0,41],[0,42]]
-- 0.4, 0.41, 0.42
instance Pretty V0s where
    pPrint (V0s []) = empty
    pPrint (V0s [x]) = text $ showVer x ""
    pPrint (V0s (x:xs)) = text (showVer x "") <> comma <+> pPrint (V0s xs)

-- >>> mkV1 $ (Just 0,[[4],[41],[42]])
-- V1 {v1 = Just 0, w1 = [V0s [[4],[41],[42]]]}
--
-- >>> pPrint $ V1 (Just 0) [V0s [[4],[41],[42]]]
-- 0.[4, 41, 42]
instance Pretty V1 where
    pPrint (V1 v0 []) = maybe empty (text . show) v0
    pPrint (V1 v0 vs) = maybe empty (\v -> text (show v) <> text ".") v0 <> pPrint vs

-- >>> pPrint $ V1s [V1 (Just 0) [V0s [[4],[41],[42]]]]
-- 0.[4, 41, 42]
instance Pretty V1s where
    pPrint (V1s []) = empty
    pPrint (V1s [V1 x []]) = pPrint x
    pPrint (V1s [x]) = pPrint x
    pPrint (V1s (x:xs)) = pPrint x <> comma <+> pPrint (V1s xs)

-- >>> mkV2 (Just 0,[(Just 4,[[]]),(Just 41,[[]]),(Just 42,[[]])])
-- V2 {v2 = Just 0, w2 = [V1 {v1 = Just 4, w1 = [V0s []]},V1 {v1 = Just 41, w1 = [V0s []]},V1 {v1 = Just 42, w1 = [V0s []]}]}
--
-- >>> pPrint $ V2 (Just 0) [V1 (Just 4) [], V1 (Just 41) [], V1 (Just 43) []]
-- 0.4, 41, Just 43
--
-- >>> pPrint $ V2 (Just 0) [V1 (Just 4) [V0s []], V1 (Just 41) [V0s []], V1 (Just 42) [V0s []]]
-- 0.4.[], 41.[], 42.[]
instance Pretty V2 where
    pPrint (V2 v []) = maybe empty (text . show) v
    pPrint (V2 v [V1 v' []]) = maybe empty (text . show) v <> text "." <> pPrint v'
    pPrint (V2 v vs) = maybe empty (\ver -> text (show ver) <> text ".") v <> pPrint (V1s vs)

instance Pretty V2s where
    pPrint (V2s []) = empty
    pPrint (V2s [V2 x []]) = pPrint x
    pPrint (V2s [x]) = pPrint x
    pPrint (V2s (x:xs)) = pPrint x <> comma <+> pPrint (V2s xs)
