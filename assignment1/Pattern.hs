module Pattern where
import Utilities
import Data.List

-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute wildcard [] _ = []
substitute wildcard (t:ts) (s:ss) 
	| t == wildcard = (s:ss) ++ substitute wildcard ts (s:ss)
	| otherwise = t : substitute wildcard ts (s:ss)
substitute wildcard t [] = t
{- TO BE WRITTEN -}


-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]

match wildcard [] [] = Just []
match wildcard [] _ = Nothing
match wildcard (p:ps) [] = Nothing
match wildcard (p:ps) (s:ss)  
	| p == wildcard 	= orElse (singleWildcardMatch (p:ps) (s:ss)) (longerWildcardMatch (p:ps) (s:ss))
	| p == s 	= match wildcard ps ss
	| otherwise 			= Nothing
{- TO BE WRITTEN -}
	

-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]

singleWildcardMatch (p:ps) (s:ss) = mmap (const [s]) (match p ps ss)
{- TO BE WRITTEN -}


longerWildcardMatch (p:ps) (s:ss) = mmap ([s]++) (match p (p:ps) ss)


{- TO BE WRITTEN -}



-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions



-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
 
transformationApply wildcard f str pair = mmap (substitute wildcard (snd pair) . f) $ match wildcard (fst pair) str
 
{- TO BE WRITTEN -}


-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ [] _ = Nothing
transformationsApply wildcard f pairs str = foldr1 orElse $ map (transformationApply wildcard f str) pairs
{- TO BE WRITTEN -}
