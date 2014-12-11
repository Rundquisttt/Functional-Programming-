Uppgift 2 Funktionell musik EDAN40

Uppgiften var att skriva ett program som utifrån en given 'BassStyle', en tonart och en ackordföljd genererar
komp och ett basmönster till ett musikstycke.

För att testa vår lösning av uppgiften i GHCI gör ett av följande alternativ för att skapa en mid-fil med
musik:
	
	Twinkle Twinkle Little Star:
		:l Twinkle.lhs
		test ttls


	Fortunate Son:
		:l FortunateSon.hs
		test fortunateSon

Kör sedan filen test.mid som skapas.

> module AutoComp where 
> import Haskore hiding (Key, Mode, Minor, Major)
> import Data.List
>

Key är ett par bestående av en tonhöjd och en mod. Key är den tonart ett musikstycket går i, det betyder att
noterna som främst används tillhör den skalan.

>
> type Key = (PitchClass, Mode)
>

Det finns många olika typer av 'BassStyle'. Vi har nöjt oss med att definiera tre stycken olika som kan
användas för att generera ett basmönster, Basic bass, Calypso bass och Boogie bass.

>
> data BassStyle = Basic | Calypso | Boogie
>
> getPattern :: Pitch -> Key -> BassStyle -> Music
> getPattern p key Basic = (Note p hn []) :+: (Note (getRelPosInKey p key 5) hn [])
> getPattern p key Calypso = times' 2 (Rest qn :+: Note p en [] :+: Note (getRelPosInKey p key 3) en [])
> getPattern p key Boogie = times' 2 (Note p en [] :+: Note (getRelPosInKey p key 5) en [] :+: Note (getRelPosInKey p key 6) en [] :+: Note (getRelPosInKey p key 5) en [])
>

En ackordföljd, ChordProgression, har vi definerat som en följd av par med tonhöjd (Pitch) och längd (Dur). Vi valde att ha med Dur också eftersom
vi tyckte att det var viktigt.

>
> type ChordProgression = [(Pitch, Dur)]
>

Ett ackord är en följd av noter från en skala. T.ex. är ackordet durtreklang den första, den tredje och den
femte noten från en given skala.

>
> type Chord = [Pitch]
>

Det finns sju olika skalor. Alla skalor har samma mönster men olika noter som bas, skalorna är alltså
endast förskjutningar av varandra. Vi använder endast sex av skalorna. Ionian och Major är olika
benämningar av samma skala. Även Aeolian och Minor är samma skala.
 
>
> data Mode = Ionian | Dorian | Phrygian | Lydian | Mixolydian | Aeolian | Major | Minor deriving (Eq, Show, Enum)
>
>
> getScale mode		| mode == Ionian || mode == Major 	= [0,2,4,5,7,9,11]
> 					| mode == Dorian 					= [0,2,3,5,7,9,10]
>					| mode == Phrygian 					= [0,1,3,5,7,8,10]
>					| mode == Lydian 					= [0,2,4,6,7,9,11]
>					| mode == Mixolydian 				= [0,2,4,5,7,9,10] 
>					| mode == Aeolian || mode == Minor  = [0,2,3,5,7,8,10]
>

Funktionen autoBass tar en 'BassStyle', en tonart och en ackordföljd och skapar utifrån det en Music genom
att kombinera ett basmönster och en ackordföljd. Music kan tolkas av Haskore. För att hitta rätt toner för
basmönstren och ackord så har vi kollat på var i tonarten varje given ton ligger och sedan bildat en skala
med tonerna som utgår från tonen i tonarten. Alltså tagit fram den mod som svarar mot tonen i tonarten. Ifall
tonen inte finns med i tonarten så kommer alla försök att göra något relativt till den att bara ge tillbaka
samma ton.

>
> --- Main functions ---------------------------- 
> autoComp :: BassStyle -> Key -> ChordProgression -> Music
> autoComp style key chordProgression = chord [autoBass style key chordProgression, autoChord key chordProgression]
>
>
> autoBass :: BassStyle -> Key -> ChordProgression -> Music 
> autoBass style key chordProgression = line [cut (snd pair) (getPattern (fst pair) key style) | pair <- transposeChordProg chordProgression (-12)] 
>													
> autoChord :: Key -> ChordProgression -> Music 
> autoChord key chordProgression = line $ optimize key chordProgression Nothing where
>	optimize key [] _ = []	
>	optimize key (pair:chordProg) Nothing = chordToMusic (basicChord (fst pair) key) (snd pair) : optimize key chordProg (Just (basicChord (fst pair) key))
>	optimize key (pair:chordProg) (Just prev) = chordToMusic (optimalChord (fst pair) key prev) (snd pair) : optimize key chordProg (Just (optimalChord (fst pair) key prev))
> 
> ---------------------------------------------------------------------------
>
>
>
> times'  1    m = m
> times' n m = m :+: (times' (n - 1) m) 
> 
>
>
>  -- help functions for optimizing the phrasing of the chords ---------------------------------------
>
> buildChord :: (Pitch, Dur) -> Key -> Music 
> buildChord pair key = chord [getNote pair 0 key, getNote pair 3 key, getNote pair 7 key]
>	where getNote pair pos key = Note (getRelPosInKey (fst pair) key pos) (snd pair) []
>



optimalChord hittar det 'optimala' ackordet givet en ton, en tonart och det förra ackordet. Ackordet kommer
alltid att bli det samma givet en ton och en tonart. 'Phrasingen' i ackordet påverkas av det tidigare
ackordet. Det optimala ackordet är det som har kortast avstånd till det tidigare ackordet. Avtåndet mellan
två ackord ges av chordDistance.

>
> optimalChord :: Pitch -> Key -> Chord -> Chord
> optimalChord pit key prev = finder (basicChord pit key) (chordVariants pit key) where
>	finder cho [] = cho
>	finder cho (c:cs) 	| (chordDistance c prev) < (chordDistance cho prev) = finder c cs
>						| otherwise = finder cho cs
>

chordVariants tar ett ackord, t.ex. C. ((C,2), (E,2), (G,2)). Och sedan ger det alla varianter av phrasing
av det ackordet som 'slutna'. Exemplet ger alltså ackorden:
	
	((E,1), (G,1), (C,2))
	((G,1), (C,2), (E,2))
	((C,2), (E,2), (G,2))
	((E,2), (G,2), (C,3))
	((G,2), (C,3), (E,3))

>
> chordVariants :: Pitch -> Key -> [Chord]
> chordVariants pit key = zipWith id variants $ take (length variants) $ repeat $ basicChord pit key
> 		where 	variants = [rephraseChordDown . rephraseChordDown, rephraseChordDown, id, rephraseChordUp, rephraseChordUp . rephraseChordUp]
> 

basicChord tar en ton och en tonart och genererar ett ackord av tonerna i tonarten med den ursprungliga
tonen i basen.

>
> basicChord :: Pitch -> Key -> Chord
> basicChord pit key = [getRelPosInKey pit key 1, getRelPosInKey pit key 3, getRelPosInKey pit key 5]
>

rephraseChord tar ett ackord och ändrar 'phrasingen' i det antingen uppåt eller neråt genom att antingen ta
den mörkaste tonen och höja den en oktav eller genom att ta den ljusaste tonen och sänka den en oktav.
Ordningen i ackordet ändras även så att tonerna ligger i ordning efter tonhöjd.
rephraseChordDown . rephraseChordUp = id

>
> rephraseChordUp, rephraseChordDown :: Chord -> Chord
> rephraseChordUp = rotate $ trans 12
> rephraseChordDown = reverse . (rotate $ trans (-12)) . reverse
>

chordDistance ger ett numeriskt värde på 'avståndet' mellan två ackord. Den gör det genom att kontrollera
avstånden mellan de olika tonerna i ackorden. Detta gör den i olika ordningnar och resturnerar sedan värdet
från den mappning som gav det kortaste 'avståndet'.

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
>
> 

keyToNotes ger en lista med PitchClass givet en tonart. (G, Major) ger t.ex. [G, A, B, C, D, E, Fs]

>
> keyToNotes :: Key -> [PitchClass]	
> keyToNotes key = map (fst . flip trans (fst key, 0)) $ getScale $ snd key 
>
>

sameTones kontrollerar ifall två listor med PitchClass innehåller samma objekt. Den ger True även ifall de
ligger i olika ordning.

> sameTones :: [PitchClass] -> [PitchClass] -> Bool
> sameTones [] sndMode = True
> sameTones (fm:fms) sndMode 	| elemIndex fm sndMode /= Nothing = sameTones fms sndMode
>										| otherwise = False					
>

correspondingKeys kontrollerar ifall två tonarter innehåller samma toner. T.ex så ger (C, Major) och
(G, Mixolydian) True. Medan (C, Major) och (G, Major) False.

>
> correspondingKeys :: Key -> Key -> Bool
> correspondingKeys key1 key2 = sameTones (keyToNotes key1) (keyToNotes key2)
>
> 
>

getCorrespondingMode hämtar ut den Mod som svarar mot den givna tonen i den givna tonarten. Om tonen inte
finns med i tonarten så returneras Nothing.

> getCorrespondingMode :: Pitch -> Key -> Maybe Mode
> getCorrespondingMode tone key = helpF key tone [Ionian .. Aeolian] where
>			helpF key tone [] = Nothing
>			helpF key tone modes 	| correspondingKeys (fst tone, head modes) key = Just (head modes)
>									| otherwise = helpF key tone (tail modes)
>
>

GetRelPosInKey är en central funktion. Den används för att stega sig igenom en skala från en given ton i en
given tonart.  T.ex. om man tar tonen G i tonarten C Major och vill ha den femte tonen (kvinten) så anropar
man GetRelPosInKey (G,3) (C, Major) 5 och så får man ut (D, 4).

>
> getRelPosInKey :: Pitch -> Key -> Int -> Pitch
> getRelPosInKey tone key pos = getRelPosInMode tone (getCorrespondingMode tone key) pos
>
>

getRelPosInMode är en hjälpfunktion till getRelPosInKey. Den hanterar ifall tonen inte finns i tonarten, då
ger den bara tillbaka den ursprungliga tonen. Ifall antalet steg som man vill gå uppåt är mer än 7 så adderas
en oktav (12 semitoner). Sedan tar man antalet steg modulo 7 och hämtar ut den positionen i skalan och
adderar motsvarande antal semitoner. Eftersom musiker (oftast) inte är programmerare så betyder pos 1 här den
första positionen i skalan. I stället för att pos 0 är den första positionen i skalan.

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
