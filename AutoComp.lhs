        					FUNCTIONAL MUSIC

	      				      dt07fa6 and dt07vb3
		  				  2010-11-22

The purpose of this text is to show how functional programming can be applied to domains that usually does not 
involve a lot of programming. In this case it is the domain of music and music theory. The final goal of the
program is to be able to generate a bass line and chord voicing for a song based on the melody, chord progression
and key of the song. We will introduce these terms more thoroughly through out the text. Some basic knowledge of
Haskell and the Haskore music library is expected from the reader as this text focuses on how to apply functional
programming to music theory and not how to use Haskore.

> module AutoComp
> where
> import Haskore
> import Data.List

We start out by defining a few basic building blocks. The key type, representing the key of a song, already exists 
in Haskore, but we redefine it to better suit our needs. The Chord type represents a chord and consists of the 
root note in PitchClass format and the harmonic quality (in this assignment limited to Major and Minor). We will
expand on the construction of chords and the attributes of chords in a little bit. ChordProgression is a simply a 
list of Chords and their respective durations.

> type Key' = (PitchClass, Mode)
> type Chord = (PitchClass, Mode)
> type ChordProgression = [(Chord, Dur)]

Next we define what a scale is and what they consist of. A scale is simply a subset of all notes consisting of 
seven notes. We represent this as a list of Pitches. Scales are built using predetermined patterns which are 
defined below as ScalePattern, a list of ints. Each element represents the number of semitones from the root 
the note in the scale is.

The scale of A minor (the aeolian pattern gives us the minor scale) would then be: {A, B, C, D, E, F, G}. B is two 
semitones above A, C is 3 semitones above A and so on.

> type Scale = [Pitch]
> type ScalePattern = [Int]

> ionian, lydian, mixolydian, aeolian, dorian, phrygian :: ScalePattern
> ionian 		= [0,2,4,5,7,9,11]
> lydian 		= [0,2,4,6,7,9,11]
> mixolydian 		= [0,2,4,5,7,9,10]
> aeolian 		= [0,2,3,5,7,8,10]
> dorian 		= [0,2,3,5,7,9,10]
> phrygian 		= [0,1,3,5,7,8,10]

Now to actually generate scales we simply take the pattern of the scale and give it the root we want.

> scale :: PitchClass -> ScalePattern -> Scale
> scale root pattern = [ pitch x | x <- map ((+) (pitchClass root)) pattern ]

Now that we have a scale we proceed to create some functions to generate chords. First of all, a chord consists of 
a root, a harmonic quality and a chord pattern. In this assignent we will only deal with major or minor harmonic 
qualities and the basic triad as chord pattern.

The basic triad is the first (root), third and fifth notes of the chord scale. To obtain the chord scale we first 
have to find what position in the key of the song the root note of the chord has. 
Let's use F Maj as the chord and C Maj as the key. The root of the chord is F which has position four in the C Maj 
scale. We now have enough information to find what scale pattern to use for our chord scale. Using the following 
table:

Pos.	Maj		Min
--------------------------------
 1  	Ionian
 2  	Mixolydian	Dorian
 3  			Phrygian
 4  	Lydian
 5  	Mixolydian
 6  			Aeolian
 7  

We can now see the to find the chord scale for F Maj when the song is in the key of C Maj we have to use the Lydian
scale pattern with F as our root. This procedure is defined below in the chordscale function.

> chordscale :: Chord -> Scale -> Scale
> chordscale (root,Major) key
> 	| pos == 1	= scale root ionian
> 	| pos == 2	= scale root mixolydian
> 	| pos == 4	= scale root lydian
> 	| pos == 5	= scale root mixolydian
> 	where pos = (maybe 0 id (elemIndex root (map fst key)))+1
> chordscale (root,Minor) key
> 	| pos == 2	= scale root dorian
> 	| pos == 3	= scale root phrygian
> 	| pos == 6	= scale root aeolian
> 	where pos = (maybe 0 id (elemIndex root (map fst key)))+1 

A chord consists of three notes called a triad. These are the first (root), third and fifth note of the chord 
scale (see above).
The function triad takes a root, a harmonic quality (i.e. Major or Minor) and the key of the song and generates 
the triad corresponding to the chord (recall that a triad is the first, third and fifth notes in the chord scale).

> triad :: PitchClass -> Mode -> Scale -> [Pitch]
> triad root harmonic key = [ (chordscale (root,harmonic) key) !! (x-1) | x <- [1,3,5] ]

The final building block is our chord' function which takes a list of (three) pitches and a duration and returns 
the chord with the correct duration applied.

> chord' :: [Pitch] -> Dur -> Music
> chord' notes d = chord $ map (\x -> Note x d [Volume 80]) notes

Since the chords we are going to generate have different harmonic qualities, it is adviseable to associate some of 
their respective patterns, namely ionian and aeolian, to the corresponding harmonic quality.

> modeToPattern :: Mode -> ScalePattern
> modeToPattern Major = ionian
> modeToPattern Minor = aeolian

In order to be able to generate bass patterns, we first have to define a type, in our case BassStyle, which 
defines three different bass styles, basic, boogie and calypso. The type BassStyle consists of a number, of the 
type Int, and a duration. The number represents what note in the chord scale to play and the duration in which 
the note lasts. If the number is set to 0 it means a rest in the bass line.

> type BassStyle = [(Int, Dur)]
> basic, boogie, calypso :: BassStyle 
> basic 	= [(1,hn), (5,hn)]
> boogie 	= [(1,en), (5,en), (6,en), (5,en), (1,en), (5, en), (6,en), (5,en)]
> calypso 	= [(0,qn), (1,en), (3,en), (0,qn), (1,en), (3, en)]

-- Bassline generation

As explained above the bassline is generated by one of three patterns (basic, boogie and calypso). The data type 
BassStyle is defined above and if we take a look at the most simple pattern, basic, we see that each bar consists 
of the first note in the chord scale with a duration of a halfnote and then the fifth note in the chord scale with 
the same duration to make up a whole note.

If a bar is split by two chords we only want to use the first chord for the first half of the bass pattern and the 
second chord for the rest. As seen in autoBass' and bassNotes below this solution assumes that a bar will only 
ever contain either a single chord or two chords. 

autoBass' takes a whole bar worth of chords in the chord progression and uses bassNotes to generate the actual 
bassLine.

> autoBass' :: BassStyle -> Key' -> ChordProgression -> [Music]
> autoBass' style key chords
> 	| chords == []			= [Rest 0]
> 	| snd (head chords) == wn	= bassNotes style songscale [head chords] ++ 
>					  autoBass' style key (tail chords)
> 	| snd (head chords) == hn	= bassNotes style songscale (take 2 chords) ++ 
>					  autoBass' style key (drop 2 chords)
> 	where songscale = scale (fst key) (modeToPattern (snd key))

bassNotes checks whether we have a bar of one or two chords. The simpler of the two cases is when we have a single 
chord which means we can just use the bass pattern straight through (all bass patterns used adds up to a full bar). 
If we have two chords we split the pattern and do two list comprehensions to generate the notes. All the patterns 
we use in this assignment can 
be nicely divided in two.

> bassNotes :: BassStyle -> Scale -> ChordProgression -> [Music]
> bassNotes style scale prog
>	| chords == 1	= [ singleBassNote (fst (head prog)) (snd x) scale (fst x) | x <- style ]
>	| chords == 2	= [ singleBassNote (fst (head prog)) (snd x) scale (fst x) | 
>			    x <- (take (div (length style) 2) style) ] ++
>			  [ singleBassNote (fst (last prog)) (snd x) scale (fst x) | 
>			    x <- (drop (div (length style) 2) style) ]
>	where chords = length prog

singleBass note uses the chordscale function defined above to generate the chord scale from which we can pick our 
notes for the bassline. It then simply picks the note as told by the bassline pattern.

> singleBassNote :: Chord -> Dur -> Scale -> Int -> Music
> singleBassNote chord dur keyscale pos
>	| pos == 0	= Rest dur
>	| otherwise	= Note (cscale !! (pos - 1)) dur [Volume 80]
>	where cscale = chordscale chord keyscale

The autoBass function is really just there to provide a nice interface to the bassline generation. autoBass' gives 
a list of Music-objects (a list of single notes to be precise) which we concatenate and transpose to the third 
octave.

> autoBass :: BassStyle -> Key' -> ChordProgression -> Music
> autoBass style key chords = Trans (3*12) $ line $ autoBass' style key chords


-- Generating the chord voicing

Our chords are built using triads (a collection of three notes played simultaneosly). However, we are not 
restricted to one specific triad for each chord.  We can transpose the notes in the chord any number of octaves 
and still have the same chord. There are some guidelines to choose the version to use though.

First, we want our notes to be in the range of E,4 to G,5.

Second, we want the change between the notes to be as small as possible. People will perceive the chord progression 
as having three melodies, one for each note in the triads. Therefore, we want to minimize the changes between the 
notes in adjacent triads to get a closer voicing.

Finally, we want the notes in the chord to be as close as possible.

To minimize the changes between chords we define melodyChanges as a heuristic that tells us how far apart the two 
chords are. We use the absPitch to get a numerical representation of the notes and define the heuristic as the 
sum of the distance between the notes. We would like the changes to be as small as possible so a small number is 
better. 

> melodyChanges :: [Pitch] -> [Pitch] -> Int
> melodyChanges p1 p2 = sum $ map (\x -> noteScore (fst x) (snd x)) (zip p1 p2)

> noteScore :: Pitch -> Pitch -> Int
> noteScore p1 p2 = abs $ (absPitch p1) - (absPitch p2)

To get closer voicing of the chords as described in the final "rule" of chord voicing above, tightness defines 
another heuristic that depends on the distance of the notes within the chord. A low number indicates a close 
	voicing.

> tightness :: [Pitch] -> Int
> tightness p = (last ps) - (head ps) 
>	where ps = sort (map absPitch p)

We define a special type of sorting algorithm to use the heuristics defined above to decide what chord of the 
different possibilities to choose. For this assignment we mainly use the melodyChanges heuristic and only fall back 
to the tightness heuristic in case the first is equal.

> chordSort :: [Pitch] -> [Pitch] -> [Pitch] -> Ordering
> chordSort [] p1 p2 = compare (tightness p1) (tightness p2)
> chordSort p0 p1 p2
>	| melodyChanges p0 p1 /= melodyChanges p0 p2 = compare (melodyChanges p0 p1) (melodyChanges p0 p2)
>	| otherwise = compare (tightness p1) (tightness p2)

To both fulfill the first "rule" and start actually generating the possible chords we define a function that 
generates all valid triads in the range specified in the "rule".

> genAlternateTriads :: [Pitch] -> [[Pitch]]
> genAlternateTriads orig = [ [x,y,z] | x <- alternateNotes (orig !! 0), 
>					y <- alternateNotes (orig !! 1), 
>					z <- alternateNotes (orig !! 2)]

> alternateNotes :: Pitch -> [Pitch]
> alternateNotes n = [ pitch x | x <- [52,53..67], (x - (absPitch n)) `mod` 12 == 0]

We are now ready to define a function to select the chord to use of the different alternatives using the 
heuristics defined.

> selectChord :: Chord -> Chord -> Key' -> [Pitch]
> selectChord pchord cchord key = head $ sortBy (chordSort (triad (fst pchord) (snd pchord) songscale)) 
>						(genAlternateTriads (triad (fst cchord) (snd cchord) songscale))
>	where songscale = scale (fst key) (modeToPattern (snd key))

The autoChord function (and its helper autoChord') is somewhat awkward but we wanted to give the user a easy 
interface to the chord voicing mechanism so there is some converting from Chord-format to [Pitch]-format and 
generation of chord scales. The basic idea is to use selectChord for each chord to find the best choice.

> autoChord :: Key' -> ChordProgression -> Music
> autoChord key chords = line $ (chord' (head $ sortBy (chordSort []) (genAlternateTriads (triad (fst (fst 
> (head chords))) (snd (fst (head chords))) songscale))) (snd (head chords))) : autoChord' key chords
> 	where songscale = scale (fst key) (modeToPattern (snd key))

> autoChord' :: Key' -> ChordProgression -> [Music]
> autoChord' key chords
>	| length chords == 2 	= [chord' (selectChord (fst (head chords)) (fst (chords !! 1)) key) 
>				  (snd (chords !! 1))]
>	| otherwise 		= chord' (selectChord (fst (head chords)) (fst (chords !! 1)) key) 
>				  (snd (chords !! 1)) : autoChord' key (tail chords)


-- Combining the two

Finally we can use the two functions autoBass and autoChord and combine them in to one.

> autoComp :: BassStyle -> Key' -> ChordProgression -> Music
> autoComp style key chords = (autoBass style key chords) :=: (autoChord key chords)

