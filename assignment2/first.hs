import Data.List

type AlignmentType = (String, String)

scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1

string1 = "writers"
string2 = "vintner"

similarityScore :: String -> String -> Int
similarityScore [] ys = sum $ map (score '-') ys
similarityScore xs [] = sum $ map (score '-') xs
similarityScore (x:xs) (y:ys) = maximum [similarityScore xs ys + score x y,
                                         similarityScore xs (y:ys) + score x '-',
                                         similarityScore (x:xs) ys + score '-' y]

score :: Char -> Char -> Int
score charOne charTwo
                    | charOne == charTwo               = scoreMatch
                    | charOne == '-' || charTwo == '-' = scoreSpace
                    | otherwise                        = scoreMismatch

scoreString :: (String, String) -> Int
scoreString ([], x) = length x * scoreSpace
scoreString (x, []) = length x * scoreSpace
scoreString ((x:xs), (y:ys)) = score x y + scoreString (xs, ys)

attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 list = [(h1:xs, h2:ys) | (xs,ys) <- list]

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy function list = map (list !! ) indexList
                    where
                    listOfVals = map function list
                    maxValue = maximum listOfVals
                    indexList = elemIndices maxValue listOfVals

optAlignments :: String -> String -> [AlignmentType]
optAlignments (x:xs) [] = maximaBy (scoreString) (attachHeads x '-' (optAlignments xs []))
optAlignments [] (x:xs) = maximaBy (scoreString) $ attachHeads '-' x $ optAlignments [] xs
optAlignments [] [] = [("", "")]
optAlignments (x:xs) (y:ys)
                            | x == y             = maximaBy (scoreString) $ attachHeads x y $ optAlignments xs ys
                            | otherwise          = maximaBy (scoreString) ((attachHeads '-' y $ optAlignments (x:xs) ys) ++ (attachHeads x '-' $ optAlignments xs (y:ys)) ++ (attachHeads x y $ optAlignments xs ys))
showTuple :: (String, String) -> IO()
showTuple (x, y) = putStrLn $ x ++ "\n" ++ y ++ "\n\n"
                                
outputOptAlignments :: String -> String -> IO()
outputOptAlignments x y = do
    putStrLn $ "There are " ++ (show $ length result) ++ " optimal alignments:\n\n"
    mapM_ showTuple result
    putStrLn $ "There were " ++ (show $ length result) ++ " optimal alignments!"
    
    where result = optAlignments x y