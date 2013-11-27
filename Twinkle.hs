import AutoComp
import Haskore

-- Defining twinkle twinkle

-- The notes in the melody.
note1 = map (flip (,) 5) [C,C,G,G,A,A,G,F,F,E,E,D,D,C]
note2 = map (flip (,) 5) [G,G,F,F,E,E,D,G,G,F,F,E,E,D]

-- The chord progression. 
chord1 = [((C, Major), wn), ((F, Major), hn), ((C, Major), hn), ((G, Major), hn), ((C, Major), hn), ((G, Major), hn), ((C, Major), hn)]
chord2 = [((C, Major), hn), ((G, Major), hn), ((C, Major), hn), ((G, Major), hn), ((C, Major), hn), ((G, Major), hn), ((C, Major), hn), ((G, Major), hn)]

-- The note times.
time = (replicate 6 qn) ++ [hn] ++ (replicate 6 qn) ++ [hn]

-- The song consists of two lines, both with the same times for the notes
l1 = line $ zipWith (\x y -> Note x y [Volume 80]) note1 time
l2 = line $ zipWith (\x y -> Note x y [Volume 80]) note2 time

-- The song consist of three parts. The first and third are identical
twinkleMelody = l1 :+: l2 :+: l1
twinkleChords = chord1 ++ chord2 ++ chord1

twinkleCalypso = Tempo 2 $ twinkleMelody :=: autoComp calypso (C,Major) twinkleChords
--
