import Data.Map

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

