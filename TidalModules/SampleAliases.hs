:{
let deepbass = s "beben" # n 1
    orgbass = s "beben" # n 5
    orgbass2 = s "beben" # n 15
    smthbass = s "beben" # n 6
    pluckbass = s "beben" # n 12
    jazzbass = s "abst" # n 1
    wubbass = s "backwhen" # n 0
    dubbass = s "dubr" # n 0
    grrbass = s "dubr" # n 13
    apollopad = s "pads" # n 3
    deeppad = s "pads" # n 9
    melpad1 = s "pads" # n 12
    melpad2 = s "pads" # n 13
    brightuppad = s "pads" # n 22
    melpad = s "pads" # n 24
    basspad = s "pads" # n 44
    dnbpad = s "pads" # n 46
    sparklepad = s "pads" # n 52
    hat808 = s "808:1"
    bd808 = s "808bd"
    bd808' a = s "808bd" # n a
    -- Breaks
    breaks = s "breaks"
    carelessbreak = s "breaks" # n 34
    championbreak = s "breaks" # n 36
    darksidebreak = s "breaks" # n 53
    differencebreak = s "breaks" # n 59
    firstcontactbreak = s "breaks" # n 71
    futurebreak = s "breaks" # n 75
    gamesbreak = s "breaks" # n 76
    helibreak = s "breaks" # n 89
    libertybreak = s "breaks" # n 104
    bulldozeramen = s "breaks" # n 30
    breaks8 p = slice 8 p $ s "breaks"
    breaks16 p = slice 16 p $ s "breaks"
    breaks32 p = slice 32 p $ s "breaks"
    breaksp16 p = splice 16 p $ s "breaks"
    breaksp32 p = splice 32 p $ s "breaks"
    jukeclap = s "jukeit" # n 3
    
    drumMachine name ps = stack 
                    (map (\ x -> 
                        (# s (name ++| (extractS "s" (x)))) $ x
                        ) ps)
    drumFrom name drum = s (name ++| drum)
    drumM = drumMachine
    drumF = drumFrom
:}
