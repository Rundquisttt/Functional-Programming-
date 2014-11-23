> module AutoBass where 
> import Haskore hiding (Key, Mode, Minor, Major)
> import Data.List
>
> type Key = (PitchClass, Mode)
>
> data BassStyle = Basic | Calypso | Boogie
>
> type ChordProgression = [(Pitch, Dur)]
>
> data Mode = Ionian | Dorian | Phrygian | Lydian | Mixolydian | Aeolian | Major | Minor deriving (Eq, Show, Enum)
> 
> autoBass :: BassStyle -> Key -> ChordProgression -> Music 
> autoBass Basic key chordProgression = line [cut (snd pair) (times' 4 ((halfNote (fst pair)) :+: (halfNote (getRelPosInKey (fst pair) key 5)))) | pair <-  chordProgression] 
>						where 	halfNote p = Note p en []
> 								
> 
> times'  1    m = m
> times' n m = m :+: (times' (n - 1) m) --TODO remove
> 
>
> 
>
> 
>
>
>
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
>
>
