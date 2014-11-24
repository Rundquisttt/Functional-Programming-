> module AutoComp where 
> import Haskore hiding (Key, Mode, Minor, Major)
> import Data.List
>
> type Key = (PitchClass, Mode)
>
> data BassStyle = Basic | Calypso | Boogie
>
> type ChordProgression = [(Pitch, Dur)]
>
> type Chord = [Pitch]
>
> data Mode = Ionian | Dorian | Phrygian | Lydian | Mixolydian | Aeolian | Major | Minor deriving (Eq, Show, Enum)
> 
> autoComp :: BassStyle -> Key -> ChordProgression -> Music
> autoComp style key chordProgression = chord [autoBass style key chordProgression, autoChord key chordProgression]
>
> autoBass :: BassStyle -> Key -> ChordProgression -> Music 
> autoBass style key chordProgression = line [cut (snd pair) (getPattern (fst pair) key style) | pair <-  chordProgression] 
>													
> 
> getPattern :: Pitch -> Key -> BassStyle -> Music
> getPattern p key Basic = (Note p hn []) :+: (Note (getRelPosInKey p key 5) hn [])
> getPattern p key Calypso = times' 2 (Rest qn :+: Note p en [] :+: Note (getRelPosInKey p key 3) en [])
> getPattern p key Boogie = times' 2 (Note p en [] :+: Note (getRelPosInKey p key 5) en [] :+: Note (getRelPosInKey p key 6) en [] :+: Note (getRelPosInKey p key 5) en [])
>
>
> times'  1    m = m
> times' n m = m :+: (times' (n - 1) m) --TODO remove
> 
>
> autoChord :: Key -> ChordProgression -> Music 
> autoChord key chordProgression = line [buildChord pair key | pair <- chordProgression]
> --autoChord key chordProgression = autoChordRecursive key chordProgression Nothing
> --autoChordRecursive key [] previous = []
> --autoChordRecursive key chordProgression previous = buildChord pair key previous : autoChordRecursive key $ tail chordProgression $ head chordProgression
>
> -- buildChord :: (Pitch, Dur) -> Key -> Chord -> Chord
>
> buildChord :: (Pitch, Dur) -> Key -> Music 
> buildChord pair key = chord [Note (fst pair) (snd pair) [], Note (getRelPosInKey (fst pair) key 3) (snd pair) [], Note (getRelPosInKey (fst pair) key 5) (snd pair) []]
>
>
> --autoChord = buildChord pair key previous : autoChord tail chordProgression previous
>
> chordDistance :: Chord -> Chord -> Int
> chordDistance c1 c2 = minimum $ map sum $ [map abs $ zipWith (-) c1' $ map absPitch c2 | c1' <- map (map absPitch) $ zipWith id functions $ take 3 $ repeat c1]
>		where 	functions = [id, rotate, rotate . rotate]
>								 
>
> rotate ls = (tail ls) ++ [head ls]
>
>
> --moder, tonarter och sånt
> getScale mode		| mode == Ionian || mode == Major 	= [0,2,4,5,7,9,11]
> 					| mode == Dorian 					= [0,2,3,5,7,9,10]
>					| mode == Phrygian 				= [0,1,3,5,7,8,10]
>					| mode == Lydian 					= [0,2,4,6,7,9,11]
>					| mode == Mixolydian 				= [0,2,4,5,7,9,10] 
>					| mode == Aeolian || mode == Minor = [0,2,3,5,7,8,10]
>
> 
> 
>
> keyToNotes :: Key -> [PitchClass]
> keyToNotes key = [ fst $ trans i tone | i <- getScale $ snd key]
>					where tone = (fst key, 0)	
>
> correspondingModes :: [PitchClass] -> [PitchClass] -> Bool
> correspondingModes [] sndMode = True
> correspondingModes fstMode sndMode 	| elemIndex (head fstMode) sndMode /= Nothing = correspondingModes (tail fstMode) sndMode
>										| otherwise = False					
>
> correspondingKeys :: Key -> Key -> Bool
> correspondingKeys key1 key2 = correspondingModes (keyToNotes key1) (keyToNotes key2)
>
> helpF :: Key -> Pitch -> [Mode] -> Maybe Mode
> helpF key tone [] = Nothing
> helpF key tone modes 	| correspondingKeys (fst tone, head modes) key = Just (head modes)
>						| otherwise = helpF key tone (tail modes)
>
>
> getCorrespondingMode :: Pitch -> Key -> Maybe Mode
> getCorrespondingMode tone key = helpF key tone [Ionian .. Aeolian]
>
>
> getRelPosInKey :: Pitch -> Key -> Int -> Pitch
> getRelPosInKey tone key pos = getRelPosInMode tone (getCorrespondingMode tone key) pos
>
> getRelPosInMode :: Pitch -> Maybe Mode -> Int -> Pitch
> getRelPosInMode tone Nothing pos = tone
> getRelPosInMode tone (Just mode) pos = trans ((quot pos 7) * 12 +  ((getScale mode) !! ((mod pos 7) -1))) tone
> 
> ---------------------------------------------------------
>
> transposeChordProg :: ChordProgression -> Int -> ChordProgression
> transposeChordProg chordProg amount = [(trans amount (fst pair), (snd pair)) | pair <- chordProg]
>
>
> --stulen från https://www.haskell.org/haskellwiki/Data.List.Split#Splits_of_known_lengths
> chunk :: Int -> [a] -> [[a]]
> chunk _ [] = []
> chunk n xs = y1 : chunk n y2
>   where
>     (y1, y2) = splitAt n xs
>
>
