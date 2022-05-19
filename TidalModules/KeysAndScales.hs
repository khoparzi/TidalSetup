import qualified Sound.Tidal.Scales as Scales

import qualified Sound.Tidal.Chords as Chords

let scale = getScale (scaleTable ++ [("skepta", [0, 1, 4, 5, 7 , 8, 10, 11]), ("bhairavi", [0, 1, 3, 4, 7 , 8, 10])])
    -- harmony
    chordTable = Chords.chordTable
    scaleList = Scales.scaleList
    majork = ["major", "minor", "minor", "major", "major", "minor", "dim7"]
    minork = ["minor", "minor", "major", "minor", "major", "major", "major"]
    doriank = ["minor", "minor", "major", "major", "minor", "dim7", "major"]
    phrygiank = ["minor", "major", "major", "minor", "dim7", "major", "minor"]
    lydiank = ["major", "major", "minor", "dim7", "major", "minor", "minor"]
    mixolydiank = ["major", "minor", "dim7", "major", "minor", "minor", "major"]
    locriank = ["dim7", "major", "minor", "minor", "major", "major", "minor"]
    keyTable = [("major", majork),("minor", minork),("dorian", doriank),("phrygian", phrygiank),("lydian", lydiank),("mixolydian", mixolydiank),("locrian", locriank),("ionian", majork),("aeolian", minork)]
    keyL p = (\name -> fromMaybe [] $ lookup name keyTable) <$> p
    -- | @chord p@ turns a pattern of chord names into a pattern of
    -- numbers, representing note value offsets for the chords
    -- chord :: Num a => Pattern String -> Pattern a
    chord p = flatpat $ Chords.chordL p
    harmonise ch p = scale ch p + chord (flip (!!!) <$> p <*> keyL ch)
