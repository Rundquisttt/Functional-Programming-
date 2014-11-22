\section{Twinkle twinkle little star}
\label{chick}

{\small\begin{verbatim} 

> module ChildSong6 where
> import Haskore
>
> -- repeat something n times
> times  1    m = m
> times n m = m :+: (times (n - 1) m)
>
> --comment
> b1a = Note (C, 4) wn [] :+: Note (F, 4) hn [] :+: Note (C, 4) hn []
> b1b = Note (G, 4) hn [] :+: Note (C, 4) hn []
> b1 = b1a :+: times 2 b1b
>
> b2a = Note (C, 4) hn [] :+: Note (G, 4) hn []
> b2 = times 4 b2a
>
> v1a = Note (C, 5) qn [] :+: Note (C, 5) qn [] :+: Note (G, 5) qn [] :+: Note (G, 5) qn [] :+: Note (A, 5) qn [] :+: Note (A, 5) qn [] :+: Note (G, 5) hn []
> v1b = Note (F, 5) qn [] :+: Note (F, 5) qn [] :+: Note (E, 5) qn [] :+: Note (E, 5) qn [] :+: Note (D, 5) qn [] :+: Note (D, 5) qn [] :+: Note (C, 5) hn [] 
>
> v1 = v1a :+: v1b
>
> v2a = Note (G, 5) qn [] :+: Note (G, 5) qn [] :+: Note (F, 5) qn [] :+: Note (F, 5) qn [] :+: Note (E, 5) qn [] :+: Note (E, 5) qn [] :+: Note (D, 5) hn []
> v2 = times 2 v2a
>
> bassLine = b1 :+: b2 :+: b1
> mainVoice = v1 :+: v2 :+: v1
> -- Putting it all together:
> ttls = Instr "piano" (Tempo 2 (Phrase [Dyn SF] bassLine :=: mainVoice))

\end{verbatim} }
