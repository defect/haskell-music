import AutoComp
import Haskore

{-
 - Stairway to Heaven
 - by Led Zeppelin
 -
 - The song is in A minor and contains quite a few chords that we either can't generate (such as C Major with a 
 - G base, F Major seventh) or that doesn't resolve in the chord scale table provided so we had to change a few 
 - of them around. It doesn't sound perfect but it works.
 -
 - Due to a lack of time we only transcribed the first three bars.
 -}
 
-- The notes in the melody.
l1n1 = Rest en :+: (line $ zipWith (\x y -> Note x y [Volume 80]) 
       [(C,6), (E,6), (A,6), 
	(B,6), (E,6), (C,6), (B,6), 
	(C,7), (E,6), (C,6), (C,7),
       (Fs,6), (D,6), (A,5), (F,6), 
        (E,6), (C,6), (A,5), (C,6), 
	       (E,6), (C,6), (A,5)] ((replicate 18 en) ++ [qn] ++ (replicate 3 en)))
	       
l1n2 = line $ zipWith (\x y -> Note x y [Volume 80]) (map (flip (,) 5) [A, Gs, Gs, Fs, Fs]) [hn, hn, hn, hn, wn]


-- Last few chords. Hard coded as we don't know their names.
l2 = chord [(Note (B,5) en [Volume 80]), 
	    (Note (G,5) en [Volume 80]), 
	    (Note (B,4) en [Volume 80])]
     :+:
     chord [(Note (E,6) en [Volume 80]), 
            (Note (C,6) en [Volume 80]), 
	    (Note (A,5) en [Volume 80]), 
	    (Note (A,4) en [Volume 80])] 
     :+:
     chord [(Note (E,6) (3/8) [Volume 80]), 
            (Note (C,6) (3/8) [Volume 80]), 
	    (Note (A,5) (3/8) [Volume 80]), 
	    (Note (A,4) (3/8) [Volume 80])]

-- The chord progression. 
chord1 = [((A, Major), hn), ((E, Major), hn), ((C, Minor), hn), ((D, Major), hn), ((F, Minor), wn)]

-- The midi instruments doesn't sound too real, but we can't play Zeppelin without a guitar.
stairwayMelody = Trans (-1) $ Instr "guitar" $ (l1n1 :=: l1n2) :+: l2
stairwayBass = Instr "bass" $ autoBass boogie (A, Minor) chord1
stairwayChords = autoChord (A, Minor) chord1

stairway = stairwayMelody :=: stairwayBass :=: stairwayChords
