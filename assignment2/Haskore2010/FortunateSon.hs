 module FortunateSon where
 import Haskore hiding (Key, Mode, Minor, Major)
 import Control.Exception hiding (assert)
 import AutoBass

 -- repeat something n times
 times  1    m = m
 times n m = m :+: (times (n - 1) m)

 --intro
 i1a = Note (B, 4) en [] :+: Note (G, 5) hn :+: Rest qn :+: Rest en
 i1b = Note (C, 4) en [] :+: Note (F, 5) hn :+: Rest qn :+: Rest en
 i1c = Note (C, 4) en [] :+: Note (E, 5) hn :+: Rest qn :+: Rest en
 i1d = Note (B, 4) en [] :+: Note (D, 5) qn :+: Rest en :+: Note (B, 4) en [] :+: Note (G, 4) qn :+: Rest en
intro = i1a :+: i1b :+: i1c :+: i1d
 --main voice
 
 --chordprogression
 
 chordProgression = [((G, 4), wn), ((F, 4), wn), ((C, 4), wn), ((G, 4), wn)]


 bassLine = chordProgToMusic chordProgression
 mainVoice = v1 :+: v2 :+: v1
 -- Putting it all together:
 ttls = Instr "piano" (Tempo 2 (Phrase [Dyn SF] bassLine :=: intro))

 chordProgToMusic :: ChordProgression -> Music
 chordProgToMusic chordProgression = line $ map noteBuilder chordProgression 
 noteBuilder :: (Pitch, Dur) -> Music 
 noteBuilder pair = Note (fst pair) (snd pair) []