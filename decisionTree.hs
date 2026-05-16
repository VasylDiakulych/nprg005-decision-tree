import Data.Map
import Data.Foldable (maximumBy, minimumBy)
import Data.Ord
import Data.Maybe
import Data.List
import Data.Foldable1 (Foldable1(toNonEmpty))

-- ============Defining data structures and type synonims============
type Label = String
type Feature = String
data Value = VString String | VDouble Double
    deriving (Show)

-- tree is either a leaf
-- or split by some categorical feature, where (Map String Tree) represent branching for Categorical nodes
-- or split by some numerical feature, where Double is a value, and left and right subtrees are <= value and > value
data Tree = Leaf Label | CategoricalNode Feature (Map String Tree) | NumericalNode Feature Double Tree Tree
    deriving (Show)

-- instance(the same as sample or example) from the labeled dataset
data Instance = Instance { features :: Map Feature Value, label :: Label }
    deriving (Show)

-- list of instances and all features
data Dataset = Dataset { instances :: [Instance], allFeatures :: [Feature] }
    deriving (Show)

data Criterion = Gini | Entropy
    deriving (Eq, Read, Show)

-- counts how many entries of each class are there
type ClassCounts = Map Label Double

-- ============Implementation of criterion functions============

classCounts :: [Label] -> ClassCounts
classCounts labels = fromListWith (+) [(l, 1) | l <- labels]

-- c_Gini(T) = sum (p_T(k)(1 - p_T(k)))
-- where T is a set of instances 
-- k is a class
-- p_T(k) - proportion of class k in a region T
gini :: ClassCounts -> Double
gini counts =
    let total = sum (elems counts)
        ps = Prelude.map (/ total) (elems counts)
    in sum [p * (1 - p) | p <- ps, p > 0]

-- c_entropy(T) = H(p_T) = - sum_{k, p_T(k) != 0} (p_T(k) log p_T(k))
-- where T is a set of instances 
-- H(p_T) - entropy of vector of probabilities p_T
-- k is a class
-- p_T(k) - proportion of class k in a region T
entropy :: ClassCounts -> Double
entropy counts =
    let total = sum (elems counts)
        ps = Prelude.map (/ total) (elems counts)
    in -sum [p * log p | p <- ps, p > 0]

criterionFn :: Criterion -> (ClassCounts -> Double)
criterionFn criterion
    | criterion == Entropy = entropy
    | otherwise = gini

-- ============Tree building============

data Split = CategoricalSplit Feature (Map String [Instance]) Double
           | NumericalSplit Feature Double [Instance] [Instance] Double
           deriving (Show)

impurityOfSplit :: Split -> Double
impurityOfSplit (CategoricalSplit _ _ imp) = imp
impurityOfSplit (NumericalSplit _ _ _ _ imp) = imp

getBestSplit :: Criterion -> [Feature] -> [Instance] -> Maybe Split
getBestSplit criterion features instances =
    let catResult = getCategoricalSplit criterion features instances
        numResult = getNumericalSplit criterion features instances
        candidates = case catResult of
            Nothing -> []
            Just (f, groups, score) -> CategoricalSplit f groups score : case numResult of
                Nothing -> []
                Just (f, t, l, r, score) -> [NumericalSplit f t l r score]

    in case candidates of
        [] -> Nothing
        _  -> Just (minimumBy (comparing impurityOfSplit) candidates)

getLabels :: [Instance] -> [Label]
getLabels = Prelude.map label

majorityLabel :: [Instance] -> Label
majorityLabel instances =
    let counts = toList (classCounts (getLabels instances))
    in
        fst (maximumBy (comparing snd) counts)

-- Criterion, maxDepth, minSamplesToSplit,
-- currentDepth, features available to split on, instances at this node
buildTree :: Criterion -> Int -> Int
    -> Int -> [Feature] -> [Instance] -> Tree
buildTree criterion maxDepth minSamplesToSplit currentDepth features instances
    -- if all samples have the same label, assign this label to the leaf
    | (l:ls) <- getLabels instances, all (== l) ls = Leaf l

    -- if we've reached maxDepth, stop and assign majority label
    | currentDepth >= maxDepth = Leaf (majorityLabel instances)

    -- if we don't have enough samples, assign majority label
    | length instances < minSamplesToSplit = Leaf (majorityLabel instances)

    -- otw find best split
    | otherwise = case getBestSplit criterion features instances of
        -- if cannot find good split, stop the process
        Nothing -> Leaf (majorityLabel instances)

        -- if best split is categorical, create categorical node
        Just (CategoricalSplit f groups _) ->
            CategoricalNode f
                (fromList [(cat, buildTree criterion maxDepth minSamplesToSplit
                (currentDepth+1) (Data.List.delete f features) group) | (cat, group) <- toList groups])

        -- otw create numerical node
        Just (NumericalSplit f t left right _) ->
            NumericalNode f t
                (buildTree criterion maxDepth minSamplesToSplit (currentDepth + 1) (Data.List.delete f features) left)
                (buildTree criterion maxDepth minSamplesToSplit (currentDepth + 1) (Data.List.delete f features) right)

-- ============Getting best categorical split============

getVString :: Feature -> Instance -> Maybe String
getVString feature inst =
    case Data.Map.lookup feature (features inst) of
        Just (VString s) -> Just s
        _ -> Nothing

groupByCategory :: Feature -> [Instance] -> Maybe (Map String [Instance])
groupByCategory feature instances =
    let vals = Prelude.map (getVString feature) instances
    in if any isNothing vals
       then Nothing
       else let categories = Prelude.map fromJust vals
                pairs = zip categories (Prelude.map (: []) instances)
            in Just (fromListWith (++) pairs)

getImpurity :: Criterion -> Map String [Instance] -> Double
getImpurity criterion categories =
    let groups = elems categories
        total = fromIntegral (sum (Prelude.map length groups))
        counts = Prelude.map getLabels groups
        impurities = Prelude.map (criterionFn criterion . classCounts) counts
        weights = Prelude.map (\g -> fromIntegral (length g) / total) groups
    -- impurity of a split is a sum of weighted impurities, 
    -- where we multiply ratio of size each group to total size of instances in all groups('weights' above) 
    -- by impurity of each group
    in sum (zipWith (*) weights impurities)

getCategoricalSplit :: Criterion -> [Feature] -> [Instance]
    -> Maybe (Feature, Map String [Instance], Double)
getCategoricalSplit criterion features instances =
    -- for each feature calculate possible results
    let results = [(f, groups, getImpurity criterion groups) | f <- features, Just groups <- [groupByCategory f instances]]
    in case results of
        [] -> Nothing
        -- pick the best one(argmin of impurities)
        _ -> let (bestFeature, bestGroups, bestScore) = minimumBy (comparing (\ (_, _, score) -> score)) results
            in Just (bestFeature, bestGroups, bestScore)

-- ============Getting best numerical split============

getVDouble :: Feature -> Instance -> Maybe Double
getVDouble feature inst =
    case Data.Map.lookup feature (features inst) of
        Just (VDouble s) -> Just s
        _ -> Nothing

-- ratio of left group to the total size of groups * impurity of left group
-- +
-- ratio of right group to the total size of groups * impurity of right group
splitImpurity :: Criterion -> [Instance] -> [Instance] -> Double
splitImpurity criterion left right =
    let total = fromIntegral (length left + length right)
        leftImpurity  = criterionFn criterion (classCounts (getLabels left))
        rightImpurity = criterionFn criterion (classCounts (getLabels right))
    in (fromIntegral (length left) / total) * leftImpurity + (fromIntegral (length right) / total) * rightImpurity

bestForFeature :: Criterion -> Feature -> [Instance] -> Maybe (Double, [Instance], [Instance], Double)
bestForFeature criterion feature instances =
    -- all pairs should have a value for a feature to split on it
    let pairs = [(v, i) | i <- instances, Just v <- [getVDouble feature i]]
    -- if some don't have
    in if length pairs /= length instances
        -- we can't split
       then Nothing

        -- otw
       else
            let sPairs = sortBy (comparing fst) pairs
                sVals = Prelude.map fst sPairs
                -- thresholds should be beetween 2 consequtive values
                thresholds = [(v1 + v2) / 2 | (v1, v2) <- zip sVals (Data.List.drop 1 sVals), v1 /= v2]

                eval t =
                    let l = [i | (v, i) <- sPairs, v <= t]
                        r = [i | (v, i) <- sPairs, v > t]
                        score = splitImpurity criterion l r
                    in (t, l, r, score)

                -- evaluate impurity of each possible split
                candidates = Prelude.map eval thresholds
            in  if Prelude.null candidates
                then Nothing
                -- return best split from candidates
                else Just (minimumBy (comparing (\(_, _, _, s) -> s)) candidates)

getNumericalSplit :: Criterion -> [Feature] -> [Instance]
    -> Maybe (Feature, Double, [Instance], [Instance], Double)
getNumericalSplit criterion features instances =
    -- calculate split for each feature
    let results = [(f, threshold, left, right, score) | f <- features, Just (threshold, left, right, score) <- [bestForFeature criterion f instances]]
    in case results of
            [] -> Nothing

            -- return best split
            _  -> let (bestFeature, bestThreshold, bestLeft, bestRight, bestScore) = minimumBy (comparing (\(_, _, _, _, s) -> s)) results
                in Just (bestFeature, bestThreshold, bestLeft, bestRight, bestScore)

-- ============Predicting============
predict :: Tree -> Instance -> Label
-- if the tree is a leaf, return the label
predict (Leaf label) _ = label

predict (CategoricalNode feature branches) inst
    -- if current node is categorical, lookup the feature category for instance we're predicting for
    | Just (VString val) <- Data.Map.lookup feature (features inst),

        -- if found the value, find branch with this value and recurse
        Just branch <- Data.Map.lookup val branches = predict branch inst

    -- otw, return empty string
    | otherwise = ""

predict (NumericalNode feature threshold left right) inst
    -- if current node is numerical, lookup the feature value for instance we're predicting for
    | Just (VDouble val) <- Data.Map.lookup feature (features inst) =
        -- if value is below threshold
        if val <= threshold
            -- recurse into the left branch
            then predict left inst
        else
            -- otw, recurse into the right branch
            predict right inst

    -- if the feature has no value, return empty string as label
    | otherwise = ""

-- ============Parsing CSV dataset============

-- custom split by predicate
wordsBy :: [a] -> (a -> Bool) -> [[a]]
wordsBy s f = go [] s
  where
    go acc [] = [reverse acc]
    go acc (c:cs)
        | not (f c) = reverse acc : go [] cs
        | otherwise = go (c:acc) cs

-- split by comma
splitCSV :: String -> [String]
splitCSV s = wordsBy s (/= ',')

-- parsing header of the csv
-- if entry is "feature:c", where c is some character, then parse header as type c
-- otw parse it as enumerable type
parseHeader :: String -> [(Feature, Char)]
parseHeader line = Prelude.map parseEntry (splitCSV line)
    where parseEntry entry =
            case wordsBy entry (/= ':') of
                (name : t : _) ->
                    case t of
                        (c:_) -> (name, c)
                        _ -> (name, 'e')
                ("" : _) -> error "name in the header cannot be empty"
                (name : _) -> (name, 'e')
                _ -> ("", 'e')

parseValue :: Char -> String -> Maybe Value
-- if the value is numeric, try to parse it as a Double
parseValue 'n' s = case reads s of
    [(v, "")] -> Just (VDouble v)
    -- otw the value is invalid
    _ -> Nothing
-- otw parse it as a string
parseValue _ s = Just (VString s)

parseRow :: Int -> [(Feature, Char)] -> String -> Maybe Instance
-- parse one row of CSV file given id of target feature, header and a line
parseRow targetId header line =
    -- split line by commas
    let entries = splitCSV line
    -- if number of entries doesn't match number of features in header, return Nothing
    in if length entries /= length header
        then Nothing
        -- otw
        else
            -- get label from targetId-th element
            let label = entries !! targetId
                -- for each feature that's not target, parse the value
                featurePairs = [(feature, value) |
                    (i, (feature, typ)) <- zip [0..] header,
                    -- skip target feature
                    i /= targetId, Just value <- [parseValue typ (entries !! i)]]
            -- if some values failed to parse, return Nothing
            in if length featurePairs /= length header - 1
                then Nothing
                -- otw create an Instance
                else Just $ Instance (fromList featurePairs) label

findTargetIndex :: Feature -> [(Feature, Char)] -> Maybe Int
-- find target feature in the header and return its index
findTargetIndex target = Data.List.findIndex ((== target) . fst)

parseDataset :: String -> Feature -> Maybe (Dataset, [Feature])
-- parse content of the CSV file, given target feature name
parseDataset content targetName =
    -- split content into lines
    let (h : ls) = lines content
    in case ls of
        -- if there are no data lines, return Nothing
        [] -> Nothing
        _ -> 
            -- parse header
            let header = parseHeader h
            -- find index of target feature in header
            in case findTargetIndex targetName header of 
                -- if target feature not found, return Nothing
                Nothing -> Nothing
                Just idx -> 
                        -- for each line parse the row
                        let instances = Data.Maybe.mapMaybe (parseRow idx header) ls
                            -- get all feature names except target
                            featureNames = [feature | (i, (feature, _)) <- zip [0..] header, i /= idx]
                        -- return Dataset and feature names
                        in Just (Dataset instances featureNames, featureNames)


loadDataset :: FilePath -> Feature -> IO (Dataset, [Feature])
-- read CSV file from the given path and parse it
loadDataset path targetName = do
    content <- readFile path
    case parseDataset content targetName of
        -- if parsed successfully, return the result
        Just result -> return result
        -- otw throw an error
        Nothing     -> error "Failed to parse dataset"
