{-# LANGUAGE LambdaCase, NamedFieldPuns, DeriveGeneric, TupleSections #-}
module Data.MakeObj.CountNodes

where

import GHC.Generics (Generic)
import Data.MakeObj.PP
import Data.MakeObj.AST
import Data.Aeson (ToJSON)
import Data.List (foldl', intercalate)
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Control.Monad.Writer
import Debug.Trace
import qualified Data.Set as Set
import Data.Set (Set)


data Meta = Meta
  { maxGeneratedNodes :: Int
  , unboundNodes :: Int
  , defsRequired :: [TypeLabel]
  , leafCounts :: HashMap String Int
  } deriving (Eq, Show, Generic)

instance ToJSON Meta 

info :: Meta -> String
info Meta{maxGeneratedNodes, unboundNodes, defsRequired, leafCounts}
  = concat [ "Meta for tree: "
           , "\n\tMaximum Generated Nodes (not counting unbounded): "
           , show maxGeneratedNodes
           , "\n\tAmount of unbound nodes: ", show unboundNodes
           , "\n\tObjects required definition set: "
           , intercalate "," (map pp defsRequired)
           , "\n\tLeaf distribution: "
           , "\n\t\t", intercalate "\n\t\t" (map (\(k,v) -> k ++ ": " ++ show v) (HashMap.toList leafCounts))
           ]

instance Semigroup Meta where
  (Meta mgA unA defsA leafsA) <> (Meta mgB unB defsB leafsB)
    = Meta { maxGeneratedNodes= mgA + mgB
           , unboundNodes = unA + unB
           , defsRequired = defsA ++ defsB
           , leafCounts = HashMap.unionWith (+) leafsA leafsB
           }

instance Monoid Meta where
  mempty = Meta 0 0 [] HashMap.empty
  mappend (Meta gn unb defs leafsA) (Meta gn' unb' defs' leafs')
    = Meta { maxGeneratedNodes = gn + gn'
           , unboundNodes = unb + unb'
           , defsRequired = defs ++ defs'
           , leafCounts = HashMap.unionWith (+) leafsA leafs'
           }
  mconcat metas = Meta
    { maxGeneratedNodes= sum $ map maxGeneratedNodes metas
    , unboundNodes= sum $ map unboundNodes metas
    , defsRequired= concatMap defsRequired metas
    , leafCounts = foldl' (HashMap.unionWith (+))
                   HashMap.empty (map leafCounts metas)
    }

fromNodes :: Int -> String -> Meta
fromNodes i leafName = mempty { maxGeneratedNodes= i, leafCounts = HashMap.singleton leafName 1}

named :: String -> Meta
named name = mempty { leafCounts = HashMap.singleton name 1}

meta :: GenerateTree -> Meta
meta = execWriter . countMeta

countMeta :: GenerateTree -> Writer Meta ()
countMeta = \case
  GObj o -> do
    mapM_ (countMeta . snd) $ HashMap.toList o
    tell $ fromNodes 1 "object"
  GList l -> case l of
    Unbounded  _     -> tell $ mempty { unboundNodes = 1
                                      , leafCounts = HashMap.singleton "unbounded list" 1 }
    ListOf     i _   -> tell $ fromNodes i "fixed list"
    RangedList _ i _ -> tell $ fromNodes i "ranged list"
    LiteralList ls   -> do
      mapM_ countMeta ls
      tell $ fromNodes (length ls) "literal list"
  GType t -> tell $ mempty
    { defsRequired= [t]
    , leafCounts = HashMap.singleton "typelabel" 1
    }
  GRx _ -> tell $ named "regex"
  GRange (DateRange _ _)-> tell $ named "date range"
  GRange _-> tell $ named "numeric range"
  GLiteral _ -> tell $ named "literal"

inlineDefinitions :: Defs -> GenerateTree -> Either String GenerateTree
inlineDefinitions = go Nothing Set.empty
  where go currentType usedDefs defs = \case
          t@(GLiteral _) -> Right t
          t@(GRange _)   -> Right t
          t@(GRx _)      -> Right t
          GType tl
            | Set.member tl usedDefs -> Left $ concat
                 [ "Circular definition found!"
                 , "\nAlready invoked definition '", pp tl
                 , "' was called ", "by '"
                 , maybe "root" pp currentType, "'"
                 ]
            | otherwise -> case lookup tl (unDefs defs) of
                Just def -> go (Just tl) (Set.insert tl usedDefs) defs def
                Nothing -> Left $ "Definition not found: '" ++ pp tl
                  ++ "', was in definition "
                  ++ maybe "root" pp currentType
          GObj obj ->
            let mapF (k, v) = (k, ) <$> go currentType usedDefs defs v
            in GObj . HashMap.fromList <$> mapM mapF (HashMap.toList obj)
          GList l -> let next c t = c <$> go currentType usedDefs defs t
                     in GList <$> case l of
            Unbounded t      -> next Unbounded t
            ListOf i t       -> next (ListOf i) t
            RangedList a b t -> next (RangedList a b) t
            LiteralList ts   -> LiteralList <$> mapM (go currentType usedDefs defs) ts

balanceUnbounded :: Int -> Meta -> GenerateTree -> Either String GenerateTree
balanceUnbounded maxSize Meta{maxGeneratedNodes, unboundNodes}
  | maxSize < maxGeneratedNodes = const . Left $
    "Tree generates too many nodes"
    ++ ", try to lower the amount of nodes to generate in the tree!"
  | unboundNodes == 0 = Right
  | otherwise = let nodesLeft = maxSize - maxGeneratedNodes
                    nodesPerUnbound = nodesLeft `div` unboundNodes
                in Right . go nodesPerUnbound
  where go i = \case
          GObj o -> GObj $ HashMap.map (go i) o
          GList (Unbounded t) -> GList (RangedList 0 i t)
          other -> other
