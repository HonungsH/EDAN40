module Chatterbot where
import Utilities
import System.Random
import Data.Char
import Data.Maybe

chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
    putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
    botloop
  where
    brain = rulesCompile botRules
    botloop = do
      putStr "\n: "
      question <- getLine
      answer <- stateOfMind brain
      putStrLn (botName ++ ": " ++ (present . answer . prepare) question)
      if (not . endOfDialog) question then botloop else return ()

--------------------------------------------------------

type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]


--------------------------------------------------------

stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
stateOfMind botBrain = do
    num <- randomIO :: IO Float
    return $ rulesApply $ (map . map2) (id, pick num) botBrain

rulesApply :: [PhrasePair] -> Phrase -> Phrase
rulesApply = (maybe [] id . ) . transformationsApply "*" reflect

reflect :: Phrase -> Phrase
reflect = map $ try $ flip lookup reflections

{--
reflect :: Phrase -> Phrase
reflect [] = []
reflect (x:xs)
    | (lookup x reflections) == Nothing   = x : (reflect xs)
    | otherwise                           = (fromJust (lookup x reflections)) : (reflect xs)
--}

reflections =
  [ ("am",     "are"),
    ("was",    "were"),
    ("i",      "you"),
    ("i'm",    "you are"),
    ("i'd",    "you would"),
    ("i've",   "you have"),
    ("i'll",   "you will"),
    ("my",     "your"),
    ("me",     "you"),
    ("are",    "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your",   "my"),
    ("yours",  "mine"),
    ("you",    "me")
  ]


---------------------------------------------------------------------------------

endOfDialog :: String -> Bool
endOfDialog = (=="quit") . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|")

rulesCompile :: [(String, [String])] -> BotBrain
rulesCompile = map $ map2 (f, g)
    where   f = words . map toLower
            g = map words

--------------------------------------


reductions :: [PhrasePair]
reductions = (map.map2) (words, words)
  [ ( "please *", "*" ),
    ( "can you *", "*" ),
    ( "could you *", "*" ),
    ( "tell me if you are *", "are you *" ),
    ( "tell me who * is", "who is *" ),
    ( "tell me what * is", "what is *" ),
    ( "do you know who * is", "who is *" ),
    ( "do you know what * is", "what is *" ),
    ( "are you very *", "are you *" ),
    ( "i am very *", "i am *" ),
    ( "hi *", "hello *")
  ]

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions

reductionsApply :: [PhrasePair] -> Phrase -> Phrase
reductionsApply = fix . try . transformationsApply "*" id

{--
reductionsApply :: [PhrasePair] -> Phrase -> Phrase
reductionsApply r p
    | result == Nothing = p
    | otherwise         = reductionsApply r (fromJust result)
    where result = transformationsApply "*" id r p
--}

-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute _ _ [] = []
substitute _ [] _ = []
substitute x (y:ys) z
    | x == y     = z ++ substitute x ys z
    | otherwise  = y : substitute x ys z

-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ [] (x:xs) = Nothing
match _ (x:xs) [] = Nothing
match x (y:ys) (z:zs)
    | x == y            = orElse (singleWildcardMatch (x:ys) (z:zs)) (longerWildcardMatch (x:ys) (z:zs))
    | x /= y && y == z  = match x ys zs
    | otherwise         = Nothing

-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (wc:ps) (x:xs) = mmap (const [x]) (match wc ps xs)
{--
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (wc:ps) (x:xs)
    | isJust (match wc ps xs)   = Just [x]
    | otherwise                 = Nothing
--}

longerWildcardMatch (wc:ps) (x:xs) = mmap (x : ) (match wc (wc:ps) xs)

-- Test cases --------------------

--testPattern =  "a=*;"
--testSubstitutions = "32"
--testString = "a=32;"

--substituteTest = substitute '*' testPattern testSubstitutions
--substituteCheck = substituteTest == testString

--matchTest = match '*' testPattern testString
--matchCheck = matchTest == Just testSubstitutions

-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- test case
--frenchPresentation = ("My name is *", "Je m'appelle *")

--swedishPresentation = ("My name is *", "Mitt namn är *")
--presentations = [frenchPresentation, swedishPresentation]

{-
transformationsApplyTest =
  test [
    transformationsApply '*' id presentations "My name is Zacharias"
      ~?= Just "Je m'appelle Zacharias",
    transformationsApply '*' id (reverse presentations) "My name is Zacharias"
      ~?= Just "Mitt namn är Zacharias",
    transformationsApply '*' id (reverse presentations) "My shoe size is 45"
      ~?= Nothing
  ]
-}

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
--transformationApply _ _ _ _ = Nothing
transformationApply x f y (o, t)-- = Just (substitute x t (f (fromJust (match x o y))))
    | match x o y == Nothing    = Nothing
    | otherwise                 = Just (substitute x t (f (fromJust (match x o y))))

-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ [] _ = Nothing
transformationsApply wc f (p:ps) o
    | (transformationApply wc f o p) == Nothing = (transformationsApply wc f ps o)
    | otherwise                                 = (transformationApply wc f o p)
