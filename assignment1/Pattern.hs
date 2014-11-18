module Pattern where
import Utilities
import Data.List

-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute wildcard t s = concatMap (\list -> if list == wildcard then s else [list]) t
{- TO BE WRITTEN -}


-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]

match wildcard [] [] = Just []
match wildcard [] _ = Nothing
match wildcard p [] = Nothing
match wildcard p s  
	| (head p) == wildcard 	= orElse (singleWildcardMatch p s) (longerWildcardMatch p s)
	| (head p) == (head s) 	= match wildcard (tail p) (tail s)
	| otherwise 			= Nothing
{- TO BE WRITTEN -}


-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]

singleWildcardMatch p s 
	| match (head p) (tail p) (tail s) /= Nothing = Just [head s]
	| otherwise = Nothing
{- TO BE WRITTEN -}

longerWildcardMatch p s = mmap ([head s]++) (match (head p) p (tail s))


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
transformationApply wildcard f str pair = mmap f1 (mmap f f2)
		    	       	   	  where f1 = substitute wildcard (snd pair)
					  	f2 = (match wildcard (fst pair) str)
 
{- TO BE WRITTEN -}


-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ [] _ = Nothing
transformationsApply wildcard f (pair:pairs) str = orElse (transformationApply wildcard f str pair) (transformationsApply wildcard f pairs str)

{- TO BE WRITTEN -}
