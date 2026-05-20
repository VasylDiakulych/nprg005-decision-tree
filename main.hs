module Main where

import DecisionTree
import System.Environment

run :: FilePath -> Double -> Feature -> Int -> Int -> Int -> Criterion -> IO ()
run file fraction target seed maxDepth minSamples criterion = do
    (Dataset instances features, _) <- loadDataset file target
    let (train, test) = splitDataset fraction seed instances
        tree = buildTree criterion maxDepth minSamples 0 features train
        correct = length [() | inst <- test, predict tree inst == label inst]
        total = length test
    putStrLn $ show correct ++ "/" ++ show total ++ " (" ++ show (fromIntegral correct / fromIntegral total * 100) ++ "%) classified correctly"

stdSeed = 42
stdDepth = 5
stdMinSamples = 2
stdCriterion = Gini

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file, fractionString, target] -> 
            run file (read fractionString) target stdSeed stdDepth stdMinSamples stdCriterion
        [file, fractionString, target, seed] -> 
            run file (read fractionString) target (read seed) stdDepth stdMinSamples stdCriterion
        [file, fractionString, target, seed, maxDepth] ->
            run file (read fractionString) target (read seed) (read maxDepth) stdMinSamples stdCriterion
        [file, fractionString, target, seed, maxDepth, minSamples] ->
            run file (read fractionString) target (read seed) (read maxDepth) (read minSamples) stdCriterion
        [file, fractionString, target, seed, maxDepth, minSamples, criterion] ->
            run file (read fractionString) target (read seed) (read maxDepth) (read minSamples) (read criterion)
        _ -> putStrLn "Usage: runghc main.hs file.csv fraction target [seed] [maxDepth] [minSamples] [criterion]"

