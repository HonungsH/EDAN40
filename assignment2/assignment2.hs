import Data.List

type AlignmentType = (String, String)

scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1

string1 = "writers"
string2 = "vintner"

long1 = "aferociousmonadatemyhamster"
long2 = "functionalprogrammingrules"

similarityScore :: String -> String -> Int
similarityScore [] ys = sum $ map (score '-') ys
similarityScore xs [] = sum $ map (score '-') xs
similarityScore (x:xs) (y:ys) = maximum [similarityScore xs ys + score x y,
                                         similarityScore xs (y:ys) + score x '-',
                                         similarityScore (x:xs) ys + score '-' y]

similarityScoreOptimized :: String -> String -> Int
similarityScoreOptimized xs ys = mcsLen (length xs) (length ys)
    where
        mcsLen i j = mcsTable !! i !! j
        mcsTable = [[mcsEntry i j | j <- [0..]] | i <-[0..]]
           
        mcsEntry :: Int -> Int -> Int
        mcsEntry i 0 = scoreSpace * i
        mcsEntry 0 j = scoreSpace * j
        mcsEntry i j = maximum [mcsLen (i - 1) (j - 1) + score x y,
                                mcsLen (i - 1) j + score x '-',
                                mcsLen i (j - 1) + score '-' y]
                    where
                        x = xs !! (i - 1)
                        y = ys !! (j - 1)

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

reverseResult :: (String, String) -> (String, String)
reverseResult (a, b) = (reverse a, reverse b)

optAlignmentsOptimized :: String -> String -> [AlignmentType]
optAlignmentsOptimized s1 s2 = map reverseResult $ snd $ mcsLen (length s1) (length s2)
    where
        mcsLen i j = mcsTable !! i !! j
        mcsTable = [[mcsEntry i j | j <- [0..]] | i <- [0..]]
           
        mcsEntry :: Int -> Int -> (Int, [AlignmentType])
        mcsEntry 0 0 = (0, [("", "")])
        mcsEntry i 0 = (i * scoreSpace, [(take i s1, replicate i '-')])
        mcsEntry 0 j = (j * scoreSpace, [(replicate j '-', take j s2)])
        mcsEntry i j = (fst $ head $ result, [b | a <- map snd result, b <- a])
            where
                x = s1 !! (i - 1)
                y = s2 !! (j - 1)
                
                result = maximaBy fst $ [(firstScore, firstResult), (secondScore, secondResult), (thirdScore, thirdResult)]
                
                firstResult = attachHeads x y $ snd $ mcsLen (i - 1) (j - 1)
                firstScore = (fst $ mcsLen (i - 1) (j - 1)) + (score x y)
                
                secondResult = attachHeads '-' y $ snd $ mcsLen i (j - 1)
                secondScore = (fst $ mcsLen i (j - 1)) + (score '-' y)
                
                thirdResult = attachHeads x '-' $ snd $ mcsLen (i - 1) j
                thirdScore = (fst $ mcsLen (i - 1) j) + (score x '-')