\section{Twinkle twinkle little star}
\label{chick}

{\small\begin{verbatim} 

> module TTLS where
> import Haskore hiding (Key, Mode, Minor, Major)
> import AutoComp
>
> -- repeat something n times
> times  1    m = m
> times n m = m :+: (times (n - 1) m)
>
> 
>
> b2a = Note (C, 4) hn [] :+: Note (G, 4) hn []
> b2 = times 4 b2a
> --main voice
> v1a = Note (C, 5) qn [] :+: Note (C, 5) qn [] :+: Note (G, 5) qn [] :+: Note (G, 5) qn [] :+: Note (A, 5) qn [] :+: Note (A, 5) qn [] :+: Note (G, 5) hn []
> v1b = Note (F, 5) qn [] :+: Note (F, 5) qn [] :+: Note (E, 5) qn [] :+: Note (E, 5) qn [] :+: Note (D, 5) qn [] :+: Note (D, 5) qn [] :+: Note (C, 5) hn [] 
>
> v1 = v1a :+: v1b
>
> v2a = Note (G, 5) qn [] :+: Note (G, 5) qn [] :+: Note (F, 5) qn [] :+: Note (F, 5) qn [] :+: Note (E, 5) qn [] :+: Note (E, 5) qn [] :+: Note (D, 5) hn []
> v2 = times 2 v2a
> --chordprogression
> c1a = [((C, 3), wn),((F, 3), hn),((C, 3), hn)]
> c1b = [((G, 3), hn),((C, 3), hn)]
> c1 = c1a ++ c1b ++ c1b
>
> c2a = [((C, 3), hn), ((G, 3), hn)]
> c2 = c2a ++ c2a ++ c2a ++ c2a
>
> twinkleChords = c1 ++ c2 ++ c1
> twinkleMelody = v1 :+: v2 :+: v1
> -- bassLine = autoBass Boogie (C, Major) chordProgression
> -- chordzz = autoChord (C, Major) (transposeChordProg chordProgression 12)  
>
> twinkleBasic   = twinkleMelody :=: autoComp Basic (C, Major) twinkleChords
> twinkleCalypso = twinkleMelody :=: autoComp Calypso (C, Major) twinkleChords
> twinkleBoogie  = twinkleMelody :=: autoComp Boogie (C, Major) twinkleChords
>
> -- Putting it all together:
> ttls = Instr "piano" (Tempo 2 (Phrase [Dyn SF] twinkleCalypso))
>
>
> -- testing purposes
> chordProgToMusic :: ChordProgression -> Music
> chordProgToMusic chordProgression = line $ map noteBuilder chordProgression 
> noteBuilder :: (Pitch, Dur) -> Music 
> noteBuilder pair = Note (fst pair) (snd pair) []
>

\end{verbatim} }
