module Chatterbot where
import Utilities
import Pattern
import System.Random
import Data.Char

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
--generatePhrasePairs :: BotBrain -> [PhrasePair]
--generatePhrasePairs botBrain = [(map2 (id, pick (rollDice)) botBrainPair) | botBrainPair <- botBrain]

stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
{- TO BE WRITTEN -}
		
stateOfMind botBrain = do
		r <- randomIO :: IO Float
		let phrasePair = [(map2 (id, pick (r)) botBrainPair) | botBrainPair <- botBrain]
		--print phrasePair
		return $ rulesApply phrasePair

rulesApply :: [PhrasePair] -> Phrase -> Phrase
{- TO BE WRITTEN -}

rulesApply phrasePairs phrase = try (transformationsApply "*" reflect phrasePairs) phrase

reflect :: Phrase -> Phrase
{- TO BE WRITTEN -}

reflectHelp :: String -> [(String, String)] -> String
reflectHelp word [] = word
reflectHelp word pairs = if(word == (fst $ head pairs)) then (snd $ head pairs) else reflectHelp word (tail pairs)

reflect [] = []
reflect phrase = reflectHelp (head phrase) reflections : reflect (tail phrase)
						
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

stringToPhrase :: [String] -> [Phrase]
stringToPhrase stringList = [ words string | string <- stringList]

rulesCompile :: [(String, [String])] -> BotBrain
{- TO BE WRITTEN -}
rulesCompile stringBotBrain = [map2 ((words . map toLower), stringToPhrase) pair | pair <- stringBotBrain] 

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
{- TO BE WRITTEN -}
reductionsApply = fix $ try $ transformationsApply "*" id reductions

--reductionsApply [] phrase = phrase
--reductionsApply phrasePairs phrase = reductionsApply (tail phrasePairs) (maybe phrase id (transformationApply "*" id phrase (head phrasePairs))) --fungerar inte. Tar bara ifall det är det första ordet


--reductionsApply phrasePairs phrase = try (transformationsApply "*" id phrasePairs) phrase

