 module FortunateSon where
 import Haskore hiding (Key, Mode, Minor, Major)
 import Control.Exception hiding (assert)
 import AutoComp

 -- repeat something n times
 times  1    m = m
 times n m = m :+: (times (n - 1) m)

 --intro
 i1a = Note (B, 5) en [] :+: Note (G, 6) dhn [] :+: Rest en 
 i1b = Note (C, 5) en [] :+: Note (F, 6) dhn [] :+: Rest en 
 i1c = Note (C, 5) en [] :+: Note (E, 6) dhn [] :+: Rest en 
 i1d = Note (B, 5) en [] :+: Note (D, 6) qn [] :+: Rest en :+: Note (B, 5) en [] :+: Note (G, 5) qn [] :+: Rest en
 intro = times 2 $ i1a :+: i1b :+: i1c :+: i1d
 --main voice
 
 mvr = times 4 Rest wn
 mv1a = Rest en :+: Note (G, 5) en [] :+: Note (G, 5) en [] :+: Note (F, 5) en [] :+: Note (G, 5) qn [] :+: Rest qn
 mv1b = Note (A, 5) en [] :+: Note (G, 5) en [] :+: Note (G, 5) en [] :+: Note (F, 5) en [] :+: Note (G, 5) qn [] :+: Rest qn
 mv1c = Rest en :+:
 --chordprogression
 
 chordProgression = [((G, 4), wn), ((F, 4), wn), ((C, 4), wn), ((G, 4), wn)]


 bassLine = chordProgToMusic chordProgression
 --mainVoice = v1 :+: v2 :+: v1
 fsCalypso = autoComp Boogie (G, Mixolydian) chordProgression :=: intro :=: mainVoice
 -- Putting it all together:
 fortunateSon = Instr "sax" (Tempo 2 (Phrase [Dyn SF] fsCalypso))



--- skit
 chordProgToMusic :: ChordProgression -> Music
 chordProgToMusic chordProgression = line $ map noteBuilder chordProgression 
 noteBuilder :: (Pitch, Dur) -> Music 
 noteBuilder pair = Note (fst pair) (snd pair) []
