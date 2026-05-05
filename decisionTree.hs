import Data.Map
import Data.Foldable (maximumBy, minimumBy)
import Data.Ord
import Data.Maybe
import Data.List

-- ============Defining data structures and type synonims============
type Label = String
type Feature = String
data Value = VString String | VDouble Double
    deriving (Show)

-- tree is either a leaf
-- or split by some categorical feature, where (Map String Tree) represent branching for Categorical nodes
-- or split by some numerical feature, where Double is a value, and left and right subtrees are > value and <= value
data Tree = Leaf Label | CategoricalNode Feature (Map String Tree) | NumericalNode Feature Double Tree Tree
    deriving (Show)

-- instance(the same as sample or example) from the labeled dataset
data Instance = Instance { features :: Map Feature Value, label :: Label }
    deriving (Show)

-- list of instances and all features
data Dataset = Dataset { instances :: [Instance], allFeatures :: [Feature] }
    deriving (Show)

data Criterion = Gini | Entropy
    deriving (Eq)

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
    let count = toList (classCounts (getLabels instances))
    in
        fst (maximumBy (comparing snd) count)

-- Criterion, maxDepth, minSamplesToSplit,
-- currentDepth, features available to split on, instances at this node
buildTree :: Criterion -> Int -> Int
    -> Int -> [Feature] -> [Instance] -> Tree
buildTree criterion maxDepth minSamplesToSplit currentDepth features instances
    | all (== head (getLabels instances)) (getLabels instances) = Leaf ( label (head instances))
    | currentDepth >= maxDepth = Leaf (majorityLabel instances)
    | length instances < minSamplesToSplit = Leaf (majorityLabel instances)
    | otherwise = case getBestSplit criterion features instances of
        Nothing -> Leaf (majorityLabel instances)
        Just (CategoricalSplit f groups _) -> CategoricalNode f (fromList [(cat, buildTree criterion maxDepth minSamplesToSplit (currentDepth+1) (Data.List.delete f features) group) | (cat, group) <- toList groups])
        Just (NumericalSplit f t left right _) -> NumericalNode f t (buildTree criterion maxDepth minSamplesToSplit (currentDepth + 1) (Data.List.delete f features) left) (buildTree criterion maxDepth minSamplesToSplit (currentDepth + 1) (Data.List.delete f features) right)


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
    in sum (zipWith (*) weights impurities)

getCategoricalSplit :: Criterion -> [Feature] -> [Instance]
    -> Maybe (Feature, Map String [Instance], Double)
getCategoricalSplit criterion features instances =
    let results = [(f, groups, getImpurity criterion groups) | f <- features, Just groups <- [groupByCategory f instances]]
    in case results of
        [] -> Nothing
        _ -> let (bestFeature, bestGroups, bestScore) = minimumBy (comparing (\ (_, _, score) -> score)) results
            in Just (bestFeature, bestGroups, bestScore)

-- ============Getting best numerical split============

getVDouble :: Feature -> Instance -> Maybe Double
getVDouble feature inst =
    case Data.Map.lookup feature (features inst) of
        Just (VDouble s) -> Just s
        _ -> Nothing

validateFeature :: Feature -> [Instance] -> Maybe [Double]
validateFeature feature instances =
    let vals = Prelude.map (getVDouble feature) instances
    in if any isNothing vals
        then Nothing
        else Just (Prelude.map fromJust vals)

splitImpurity :: Criterion -> [Instance] -> [Instance] -> Double
splitImpurity criterion left right =
    let total = fromIntegral (length left + length right)
        leftImpurity  = criterionFn criterion (classCounts (getLabels left))
        rightImpurity = criterionFn criterion (classCounts (getLabels right))
    in (fromIntegral (length left) / total) * leftImpurity + (fromIntegral (length right) / total) * rightImpurity

bestForFeature :: Criterion -> Feature -> [Instance] -> Maybe (Double, [Instance], [Instance], Double)
bestForFeature criterion feature instances =
    let pairs = [(v, i) | i <- instances, Just v <- [getVDouble feature i]]
    in if length pairs /= length instances
       then Nothing

       else
            let sPairs = sortBy (comparing fst) pairs
                sVals = Prelude.map fst sPairs
                thresholds = [(v1 + v2) / 2 | (v1, v2) <- zip sVals (Data.List.drop 1 sVals), v1 /= v2]

                eval t =
                    let l = [i | (v, i) <- sPairs, v <= t]
                        r = [i | (v, i) <- sPairs, v > t]
                        score = splitImpurity criterion l r
                    in (t, l, r, score)
                candidates = Prelude.map eval thresholds
            in  if Prelude.null candidates
                then Nothing
                else Just (minimumBy (comparing (\(_, _, _, s) -> s)) candidates)

getNumericalSplit :: Criterion -> [Feature] -> [Instance]
    -> Maybe (Feature, Double, [Instance], [Instance], Double)
getNumericalSplit criterion features instances =
   let results = [(f, threshold, left, right, score) | f <- features, Just (threshold, left, right, score) <- [bestForFeature criterion f instances]]
    in case results of
         [] -> Nothing
         _  -> let (bestFeature, bestThreshold, bestLeft, bestRight, bestScore) = minimumBy (comparing (\(_, _, _, _, s) -> s)) results
               in Just (bestFeature, bestThreshold, bestLeft, bestRight, bestScore)
