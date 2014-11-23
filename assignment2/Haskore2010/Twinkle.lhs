\section{Twinkle twinkle little star}
\label{chick}

{\small\begin{verbatim} 

> module TTLS where
> import Haskore hiding (Key, Mode, Minor, Major)
> import AutoBass
>
> -- repeat something n times
> times  1    m = m
> times n m = m :+: (times (n - 1) m)
>
> --baseline
> b1a = Note (C, 4) wn [] :+: Note (F, 4) hn [] :+: Note (C, 4) hn []
> b1b = Note (G, 4) hn [] :+: Note (C, 4) hn []
> b1 = b1a :+: times 2 b1b
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
> c1a = [((C, 4), wn),((F, 4), hn),((C, 4), hn)]
> c1b = [((G, 4), hn),((C, 4), hn)]
> c1 = c1a ++ c1b ++ c1b
>
> c2a = [((C, 4), hn), ((G, 4), hn)]
> c2 = c2a ++ c2a ++ c2a ++ c2a
>
> chordProgression = c1 ++ c2 ++ c1
>
>
> bassLine = autoBass Basic (C, Major) chordProgression -- (b1 :+: b2 :+: b1)  
> mainVoice = v1 :+: v2 :+: v1
> -- Putting it all together:
> ttls = Instr "piano" (Tempo 2 (Phrase [Dyn SF] bassLine :=: mainVoice))
>
>
> -- testing purposes
> chordProgToMusic :: ChordProgression -> Music
> chordProgToMusic chordProgression = line $ map noteBuilder chordProgression 
> noteBuilder :: (Pitch, Dur) -> Music 
> noteBuilder pair = Note (fst pair) (snd pair) []
>

\end{verbatim} }
