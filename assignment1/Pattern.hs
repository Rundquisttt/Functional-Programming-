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
match wildcard [] _ = Just []
match wildcard p [] = Nothing
-- match wildcard p s = orElse
match wildcard p s
	| (head p) == wildcard = orElse (singleWildcardMatch p s) (longerWildcardMatch p s)
	| (head p) == (head s) = match wildcard (tail p) (tail s)
	| otherwise = Nothing
{- TO BE WRITTEN -}


-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (wc:ps) (x:xs) = if(ps == xs) 
										then Just [x]
										else Nothing

{- TO BE WRITTEN -}
longerWildcardMatch [] _ = Nothing
longerWildcardMatch (x:xs) (y:ys) = if(f2 (x:xs) (y:ys)) then Just (f1 (x:xs) (y:ys)) else Nothing
                    where   f1 (x:xs) ys = take (length ys - length xs) ys
                            f2 (x:xs) ys = (drop (length ys - length xs) ys) == xs && length ys - length xs > 1
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
transformationApply _ _ _ _ = Nothing
{- TO BE WRITTEN -}


-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ _ _ = Nothing
{- TO BE WRITTEN -}
