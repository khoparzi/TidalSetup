:{
let hat808 = s "808:1"
    bd808 = s "808bd"
    bd808' a = s "808bd" # n a
    -- Breaks
    breaks = s "breaks"
    breaks8 p = slice 8 p $ s "breaks"
    breaks16 p = slice 16 p $ s "breaks"
    breaks32 p = slice 32 p $ s "breaks"
    breaksp16 p = splice 16 p $ s "breaks"
    breaksp32 p = splice 32 p $ s "breaks"
    jukeclap = s "indigoclap" # n 3
    gretsch = inhabit [
            ("bd", n "13" # s "gretsch")
            , ("sd", n "20" # s "gretsch")
            , ("sn", n "20" # s "gretsch")
            , ("ride", n "19" # s "gretsch")
            , ("hp", n "4" # s "gretsch")
            ]
    pads = inhabit [
            ("apollo", n "3" # s "pads")
            , ("deep", n "9" # s "pads")
            , ("mel1", n "12" # s "pads")
            , ("mel2", n "13" # s "pads")
            , ("brightup", n "22" # s "pads")
            , ("mel3", n "24" # s "pads")
            , ("bass", n "44" # s "pads")
            , ("dnb", n "46" # s "pads")
            , ("sparkle", n "52" # s "pads")
            ]
    basses = inhabit [
            ("deep", n "3" # s "beben")
            , ("organ", n "5" # s "beben")
            , ("smooth", n "6" # s "beben")
            , ("pluck", n "12" # s "beben")
            , ("organ2", n "15" # s "beben")
            , ("dub", n "0" # s "dubr")
            , ("grr", n "13" # s "dubr")
            ]
    dhol = inhabit [
            ("dhi", n 5 # s "dhol")
            , ("ti", n 6 # s "dhol")
            , ("ta", n 7 # s "dhol")
            , ("dha", n 2 # s "dhol")
            , ("tik", n 3 # s "dhol")
            , ("tak", n 4 # s "dhol")
            ]
    
    drumMachine name ps = stack 
                    (map (\ x -> 
                        (# s (name ++| (extractS "s" (x)))) $ x
                        ) ps)
    drumFrom name drum = s (name ++| drum)
    drumM = drumMachine
    drumF = drumFrom
    
:}
