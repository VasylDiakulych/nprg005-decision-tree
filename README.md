# CART Decision Tree in Haskell

A command-line tool for CART decision trees

## Format support

Tool supports .csv files with value type encoded in header as a suffix(:e for categorical features, :n for numerical)

**Example (`adult.csv`):**
```
age:n,workClass:e,fnlwgt:n,education:e,education-num:n,marital_status:e,occupation:e,relationship:e,race:e,sex:e,capital-gain:n,capital-loss:n,hours-per-week:n,native-country:e,result:e
39, State-gov, 77516, Bachelors, 13, Never-married, Adm-clerical, Not-in-family, White, Male, 2174, 0, 40, United-States, <=50Ks
```

## Usage

Three modes of operation:

### 1. Train and Evaluate
```
runghc dataset.csv fraction target [seed] [maxDepth] [minSamples] [criterion]
```

**Arguments:**
1. `file.csv` - mandatory field, path to CSV dataset 
2. `fraction` - mandatory field, train fraction (0.0–1.0), usually 0.8 (so, 80% of the dataset goes for training, 20% goes for testing)
3. `target` - mandatory field, name of target column
4. `seed` - optional field, default value is 42,  random seed for dataset shuffling
5. `maxDepth` - optional field, default value is 5, maximum tree depth
6. `minSamples` - optional field, default value is 2, minimum samples to split
7. `criterion` - optional field, default value is Gini, impurity measure: `Gini` or `Entropy`

**Example:**
```
runghc Datasets/iris.csv 0.8 class 42 5 2 Gini
# Output: 28/30 (93.33%) classified correctly
```

### 2. Train and Save Tree
```
runghc save file.csv fraction target seed maxDepth minSamples criterion treeFile
```
Same as above, but writes the trained tree to `treeFile`
### 3. Load Tree and Predict
```
runghc predictSet treeFile file.csv target
```
Loads a previously saved tree and evaluates it on the given dataset

### 4. Predict Using Tree

```
runghc predictOne treeFile feature1=val1 feature2=val2 ...
```
Loads a previously saved tree and predict target label for a given sample

## Datasets

4 datasets are included in `Datasets/`:

1. playTennis.csv - 14 instances, 4 categorical features, play prediction
2. iris.csv - 150 instances, 4 numerical features, predicts flower species, ~95% accuracy
3. agaricus-lepiota.csv - 8.124 instances, 22 categorical features, mushroom edibility prediction, ~99% accuracy
4. adult.csv - 32,561 instances, 14 mixed features, income prediction, ~80% accuracy

Requires GHC 9.6+ with  `random` package.
