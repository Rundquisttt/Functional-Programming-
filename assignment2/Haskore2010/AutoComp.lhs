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
> autoBass style key chordProgression = line [cut (snd pair) (getPattern (fst pair) key style) | pair <- transposeChordProg chordProgression (-12)] 
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
> --autoChord key chordProgression = line $ map (flip buildChord key) chordProgression
> autoChord key chordProgression = line $ optimize key chordProgression Nothing where
>	optimize key [] _ = []	
>	optimize key (pair:chordProg) Nothing = chordToMusic (basicChord (fst pair) key) (snd pair) : optimize key chordProg (Just (basicChord (fst pair) key))
>	optimize key (pair:chordProg) (Just prev) = chordToMusic (optimalChord (fst pair) key prev) (snd pair) : optimize key chordProg (Just (optimalChord (fst pair) key prev))
>
>  -- help functions for optimizing the phrasing of the chords ---------------------------------------
>
> buildChord :: (Pitch, Dur) -> Key -> Music 
> buildChord pair key = chord [getNote pair 0 key, getNote pair 3 key, getNote pair 7 key]
>	where getNote pair pos key = Note (getRelPosInKey (fst pair) key pos) (snd pair) []
>
> --buildChord :: (Pitch, Dur) -> Key -> Chord -> Music
> --buildChord pair key previous = chordToMusic (optimalChord (fst pair) key previous) (snd pair)
>
> optimalChord :: Pitch -> Key -> Chord -> Chord
> optimalChord pit key prev = finder (basicChord pit key) (chordVariants pit key) where
>	finder cho [] = cho
>	finder cho (c:cs) 	| (chordDistance c prev) < (chordDistance cho prev) = finder c cs
>						| otherwise = finder cho cs
>
> chordVariants :: Pitch -> Key -> [Chord]
> chordVariants pit key = zipWith id variants $ take (length variants) $ repeat $ basicChord pit key
> 		where 	variants = [rephraseChordDown . rephraseChordDown, rephraseChordDown, id, rephraseChordUp, rephraseChordUp . rephraseChordUp]
> 
> basicChord :: Pitch -> Key -> Chord
> basicChord pit key = [getRelPosInKey pit key 1, getRelPosInKey pit key 3, getRelPosInKey pit key 5]
>
> rephraseChordUp, rephraseChordDown :: Chord -> Chord
> rephraseChordUp = rotate $ trans 12
> rephraseChordDown = reverse . (rotate $ trans (-12)) . reverse
>
> chordDistance :: Chord -> Chord -> Int
> chordDistance c1 c2 = minimum $ map sum $ [map abs $ zipWith (-) c1' $ map absPitch c2 | c1' <- map (map absPitch) $ zipWith id functions $ take 3 $ repeat c1]
>		where 	functions = [id, rotate id , (rotate id) . (rotate id)]
>								 
>
> rotate f (l:ls) = ls ++ [f l]
>
> chordToMusic :: Chord -> Dur -> Music
> chordToMusic cho dur = chord [Note pit dur [] | pit <- cho]
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
> 
>
> getCorrespondingMode :: Pitch -> Key -> Maybe Mode
> getCorrespondingMode tone key = helpF key tone [Ionian .. Aeolian] where
>			helpF key tone [] = Nothing
>			helpF key tone modes 	| correspondingKeys (fst tone, head modes) key = Just (head modes)
>									| otherwise = helpF key tone (tail modes)
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
> 
>
>
