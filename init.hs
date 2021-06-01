
import Sound.Tidal.Utils

import Sound.Tidal.Params

import Data.Maybe

import Control.Applicative

-------------------------------------------------------------------------------

-- Aliases
let inter = interlace
    bpm(a) = setcps(a/120)
    st = stack
    ct = cat
    cs i a = (segment i $ choose a)
    levl = gain . fmap (\x -> if abs x > 1 then 1 else x)
    chup = silent
    takeaway p = degradeBy "<[0.1..0.9>" $ p
    foldEVery = foldEvery
    accelrate = accelerate
    discretize = discretise
    loud p = (# gain 1.2) $ p
    soft p = (# gain 0.8) $ p
    slowsine = (slow 4 (sine))
    slowersine = (slow 8 (sine))
    slowsaw = (slow 4 (saw))
    slowersaw = (slow 8 (saw))
    slowtri = (slow 4 (tri))
    slowertri = (slow 8 (tri))
    fa = fast
    sl = slow
    -- continous function shorthands
    sin = sine
    cos = cosine
    sq  = square
    pulse w = sig $ \t -> if ((snd $ properFraction t) >= w) then 1.0 else 0.0
    pulse' w = liftA2 (\a b -> if (a>=b) then 1.0 else 0.0) saw w
    pw = pulse
    pw' = pulse'
    stb a p = sometimesBy a p
    x e = (10 ** e) -- For exponent frequencys
    describe = deconstruct

    -- range shorthands
    range' from to p = (p*to - p*from) + from
    rg' = range'
    rg = range -- old: scale
    rgx = rangex -- old: scalex

    -- continuous at freq
    sinf  f = fast f $ sin -- sine at freq
    cosf  f = fast f $ cos -- cosine at freq
    trif  f = fast f $ tri -- triangle at freq
    sawf  f = fast f $ saw -- saw at freq
    isawf f = fast f $ isaw -- inverted saw at freq
    sqf   f = fast f $ sq -- square at freq
    pwf w f = fast f $ pw w -- pulse at freq
    pwf' w f = fast f $ pw' w -- pulse' at freq
    randf f = fast f $ rand -- rand at freq

    -- ranged continuous
    rsin start out = rg' start out sin -- ranged' sine
    rcos start out = rg' start out cos -- ranged' cosine
    rtri start out = rg' start out tri -- ranged' triangle
    rsaw start out = rg' start out saw -- ranged' saw
    risaw start out = rg' start out isaw -- ranged' inverted saw
    rsq start out = rg' start out sq -- ranged' square
    -- rpw start out w = rg' start out pw w -- ranged' pulse
    -- rpw' start out w = rg' start out pw' w -- ranged' pulse'
    rrand start out = rg' start out rand -- ranged' rand
    rxsin start out = rgx start out sin -- ranged' exponential sine
    rxcos start out = rgx start out cos -- ranged' exponential cosine
    rxtri start out = rgx start out tri -- ranged' exponential triangle
    rxsaw start out = rgx start out saw -- ranged' exponential saw
    rxisaw start out = rgx start out isaw -- ranged' exponential inverted saw
    rxsq  start out = rgx start out sq -- ranged' exponential sqaure
    rxpw start out width = rgx start out pw width -- ranged' exponential pulse
    rxpw' start out width = rgx start out pw' width -- ranged' exponential pulse'
    rxrand start out = rgx start out rand -- ranged' exponential rand

    -- ranged continuous at freq
    rsinf start out mult = fast mult $ rsin start out -- ranged' sine at freq
    rcosf start out mult = fast mult $ rcos start out -- ranged' cosine at freq
    rtrif start out mult = fast mult $ rtri start out -- ranged' triangle at freq
    rsawf start out mult = fast mult $ rsaw start out -- ranged' saw at freq
    risawf start out mult = fast mult $ risaw start out  -- ranged' inverted saw at freq
    rsqf start out mult = fast mult $ rsq start out  -- ranged' square at freq
    rsins start out s = slow s $ rsin start out -- ranged' sine at freq
    rcoss start out s = slow s $ rcos start out -- ranged' cosine at freq
    rtris start out s = slow s $ rtri start out -- ranged' triangle at freq
    rsaws start out s = slow s $ rsaw start out -- ranged' saw at freq
    risaws start out s = slow s $ risaw start out  -- ranged' inverted saw at freq
    rsqs start out s = slow s $ rsq start out  -- ranged' square at freq
    -- rpwf start out width mult = fast mult $ rpw' start out width -- ranged' pulse at freq
    rrandf start out mult = fast mult $ rrand start out -- ranged' rand at freq
    rxsinf start out mult = fast mult $ rxsin start out -- ranged' exponential sine at freq
    rxcosf start out mult = fast mult $ rxcos start out -- ranged' exponential cosine at freq
    rxtrif start out mult = fast mult $ rxtri start out -- ranged' exponential triangle at freq
    rxsawf start out mult = fast mult $ rxsaw start out -- ranged' exponential saw at freq
    rxisawf start out mult = fast mult $ rxisaw start out -- ranged' exponential inverted saw at freq
    rxsqf start out mult = fast mult $ rxsq start out -- ranged' exponential square at freq
    rxpwf start out width mult = fast mult $ rxpw start out width -- ranged' exponential pulse at freq
    rxpwf' start out width mult = fast mult $ rxpw' start out width -- ranged' exponential pulse' at freq
    rxrandf start out mult = fast mult $ rxrand start out  -- ranged' exponential random at freq

    htime p = slow 2 $ p
    qttime p = slow 4 $ p
    dtime p = fast 2 $ p
    qdtime p = fast 4 $ p
    slow1 p = slow 1 $ p
    slow2 p = slow 2 $ p
    slow3 p = slow 3 $ p
    slow4 p = slow 4 $ p
    orb = orbit
    mc = midichan
    sg = segment
    cceuc8 c p = euclid (floor <$> (segment 1 $ range 1 8 $ cF 0 c)) 8 $ p
    cceuc16 c p = euclid (floor <$> (segment 1 $ range 1 16 $ cF 0 c)) 16 $ p
    qdelayt a = delayt ((1/4) * a)
    edelayt a = delayt ((1/8) * a)
    sdelayt a = delayt ((1/16) * a)

-- For slicing and looping
let slice8 a p = slice 8 a $ p
    slice16 a p = slice 16 a $ p
    slice32 a p = slice 32 a $ p
    slice64 a p = slice 64 a $ p
    loop2 p = loopAt 2 $ p
    loop4 p = loopAt 4 $ p
    loop8 p = loopAt 8 $ p

-- A different kind of slicer
let sl8 s p = ((# begin ((1/8)*s)) . (# end ((1/8)*(s+1)))) $ p
    sl16 s p = ((# begin ((1/16)*s)) . (# end ((1/16)*(s+1)))) $ p
    sl32 s p = ((# begin ((1/32)*s)) . (# end ((1/32)*(s+1)))) $ p

-- From Kindohm
let erun = (run 8)
    srun = (run 16)
    delays = [(1/512), (1/256), (1/128), (1/64), (1/32), (1/16), (1/8)]
    spike p = ((# delaytime (range 0.001 0.3 $ slow 7.1 sine)) . (# delayfeedback (range 0.7 0.99 $ slow 6.71 sine))) $ p
    spike' p = (# delay "0.4") $ spike $ p
    shrand n = (n <~) $ rand
    shirand n p = (n <~) $ irand p
    replicator text1 = [putStr (text1) | x <- replicate 3000 text1]
    flood text2 = sequence_(replicator text2)
    rip feed timediv p = within (0.25, 0.75) (slow 2 . rev . stut 8 feed timediv) p
    rip' starttime endtime depth feed timediv p = within (starttime, endtime) (slow 2 . rev . stut depth feed timediv) p
    jit start amount p = within (start, (start + 0.5)) (trunc (amount)) p
    gtfo p = mute
    gtfo' p = (const $ midinote "~") p
    gtfom = gtfo'
    gtfo2 = gtfo'
    shift p = (1024 <~)  p
    shift' x p = (x <~) p
    one p = stut' 1 (0.125/2) (|*| gain "1") $ p
    one' p = rarely (stut' 1 (0.125/2) (|*| gain "1")) $ shift' 1024 $ p
    one'' p = sometimes (stut' 1 (0.125/2) (|*| gain "1")) $ shift' 1024 $ p
    rep depth p = stut' (depth - 1) (0.125*3) (|*| gain "1") $ p
    rep' depth p = stut' (depth - 1) (0.125/2*3) (|*| gain "1") $ p
    rep'' depth p = stut' (depth - 1) (0.125/4*3) (|*| gain "1") $ p
    randDelay p = ((# delay (range 0.5 0.7 $ shift' 5001 $ rand)) . (# delaytime (shift' 5002 $ choose delays)) . (# delayfeedback (range 0.5 0.9 $ shift' 5003 $ rand))) $ p
    rando = randDelay
    stupid = randDelay
    weird = randDelay
    beginend bpat durpat = (begin bpat) # (end $ (+) <$> bpat <*> durpat)
    inverse 1 = 0
    inverse 0 = 1
    inverse 11 = 0
    inverse 10 = 1
    move p = foldEvery [3,4] (0.25 <~) $ p
    move'' p = foldEvery [2,3] (0.25 <~) $ p
    move' p = foldEvery [3,4] (0.25 ~>) $ p
    move''' p = foldEvery [2,3] (0.25 ~>) $ p
    thicken' x percent p = superimpose ((# pan 1) . (|* speed percent)) $ ((# speed x) . (# pan 0)) $ p
    thicken p = thicken' 1 0.8 $ p
    fuckery p = every 5 (rip 0.5 0.1) $ every 6 (rip' 0 1 8 0.5 0.1) $ every 7 (# coarse "{4 8 6 12 16}%14") $ every 8 (# accelerate "-0.5 0.5") $ p
    discRange a b = (discretise 1 $ range a b $ shrand 10003)
    discrange = discRange
    triRange a b c = (range a b $ slow c tri)
    trirange = triRange
    cpsDisc a b = (|* cps (discRange a b))
    cpsRange a b c = (|* cps (triRange a b c))
    cpsTri = cpsRange --cps(a/120/2)
    bpmDisc a b = (|* cps ((discRange a b) / 60))
    bpmRange a b c = (|* cps ((triRange a b c) / 60))
    bpmTri = bpmRange


-- From jArm https://gist.github.com/jarmitage/627a7d5a9263475ba53f725f1405d6a2

-- sequence generators
let r = run
    ri a = rev (r a) -- run inverted
    -- rd a = (0 - (r a)) -- run down e.g. (10 - (r 10))
    c = choose
    odd    a = (((r a) + 1) * 2) - 1 -- run of odd numbers
    even   a =  ((r a) + 1) * 2 -- run of even numbers
    -- codd   a = c (patToList (odd   a)) -- choose odd
    -- ceven  a = c (patToList (even  a)) -- choose even
    oddi   a = rev (odd a) -- odd inverted
    eveni  a = rev (even a) -- even inverted
    qn = (1/4)
    en = (1/8)
    qv = (1/8)
    sn = (1/16)
    sqv = (1/16) -- Semi quaver
    tn = (1/32)
    dsqv = (1/32) -- Demi Semi quaver
    hdsqv = (1/64)  -- Hemi Demi Semi quaver
    qn' m = (1/4) * m
    en' m = (1/8) * m
    qv' m = (1/8) * m
    sn' m = (1/16) * m
    sqv' m = (1/16) * m
    tn' m = (1/32) * m
    dsqv' m = (1/32) * m
    hdsqv' m = (1/64) * m
    -- Time division runs
    qnr = (r 4) * (1/4)
    enr = (r 8) * (1/8)
    qvr = (r 8) * (1/8)
    snr = (r 16) * (1/16)
    sqvr = (r 16) * (1/16) -- Semi quaver
    tnr = (r 32) * (1/32)
    dsqvr = (r 32) * (1/32) -- Demi Semi quaver
    hdsqvr = (r 64) * (1/64)  -- Hemi Demi Semi quaver
    qnr' m = (r m) * (1/4)
    enr' m = (r m) * (1/8)
    qvr' m = (r m) * (1/8)
    snr' m = (r m) * (1/16)
    sqvr' m = (r m) * (1/16)
    tnr' m = (r m) * (1/32)
    dsqvr' m = (r m) * (1/32)
    hdsqvr' m = (r m) * (1/64)

-- Abbrevs for transitions
let j p n  = jumpIn' n
    j2 p   = jumpIn' p 2
    j4 p   = jumpIn' p 4
    j8 p   = jumpIn' p 8
    j16 p  = jumpIn' p 16
    xf p n = xfadeIn  p n
    xf2 p  = xfadeIn  p 2
    xf4 p  = xfadeIn  p 4
    xf8 p  = xfadeIn  p 8
    xf16 p = xfadeIn  p 16
    cl p n = clutchIn p n
    cl2 p  = clutchIn p 2
    cl4 p  = clutchIn p 4
    cl8 p  = clutchIn p 8
    cl16 p = clutchIn p 16

-- FX groups
let snl  = grp [mF "sound",   mF "n", mF "legato"]
    adsr = grp [mF "attack",  mF "decay", mF "sustain", mF "release"]
    del  = grp [mF "delay",   mF "delaytime", mF "delayfeedback"]
    scc  = grp [mF "shape",   mF "coarse", mF "crush"]
    -- lpf  = grp [mF "cutoff",  mF "resonance"] -- deprecated
    -- bpf  = grp [mF "bandf",   mF "bandq"] -- deprecated
    -- hpf  = grp [mF "hcutoff", mF "hresonance"] -- deprecated
    spa  = grp [mF "speed",   mF "accelerate"]
    rvb  = grp [mF "room",    mF "size"]
    gco  = grp [mF "gain",    mF "cut", mF "orbit"]
    glo  = grp [mF "gain",    mF "legato", mF "orbit"]
    go   = grp [mF "gain",    mF "orbit"]
    io   = grp [mF "begin",   mF "end"]
    eq   = grp [mF "cutoff",  mF "resonance", mF "bandf", mF "bandq", mF "hcutoff", mF "hresonance"]
    tremolo = grp [mF "tremolorate", mF "tremolodepth"]
    phaser  = grp [mF "phaserrate", mF "phaserdepth"]
    -- TODO: add SpectralTricks / SC FX groups
    -- FX groups' function version
    snl' a b = sound a # n b # legato 1
    snl'' a b l = sound a # n b # legato l
    slg s = sound s # legato 1
    adsr' a d s r = attack a # decay d # sustain s # release r
    ar' a r = attack a # decay 1 # sustain 1 # release r
    del' amount timediv feed = delay amount # delaytime timediv # delayfeedback feed
    delq' amount timediv feed = delay amount # delaytime ((1/4)* timediv) # delayfeedback feed
    dele' amount timediv feed = delay amount # delaytime ((1/8)* timediv) # delayfeedback feed
    dels' amount timediv feed = delay amount # delaytime ((1/16)* timediv) # delayfeedback feed
    scc' shapeamount coarseamount crushamount = shape shapeamount # coarse coarseamount # crush crushamount
    lpf' c r = cutoff c # resonance r
    lpfx f x = lpf (f * (10 ** x)) -- Lowpass with exponent
    bpf' f q = bandf f # bandq q
    bpfx f x = bpf (f * (10 ** x))
    hpf' c r = hcutoff c # hresonance r
    hpfx f x = hpf (f * (10 ** x))
    spa' s a = speed s # accelerate a
    gco' g c o = gain g # cut c # orbit o
    glo' g l o = gain g # legato l # orbit o
    go' g o = gain g # orbit o
    rvb' r s = room r # size s
    io' start out  = begin start # end out
    eq' h b l q = cutoff l # resonance q # bandf b # bandq q # hcutoff h # hresonance q
    tremolo' r d = tremolorate r # tremolodepth d
    phaser' r d = phaserrate r # phaserdepth d
    grain8 s w = begin (en' s) # end ((en' s) + w)
    grain16 s w = begin (sn' s) # end ((sn' s) + w)

-- Useful patterns
let footwork1 = struct "t(3,8,2)"
    footwork2 = struct "t(5,8)"
    footwork3 = struct "t(<3 5>,8,2)"
    footwork4 = struct "t(3,8,<0 7>)"
    footwork = struct "[t ~ ~ t ~ ~ t ~]"
    footwork' f p = fast f $ struct "[t ~ ~ t ~ ~ t ~]" $ p
    altfoot = struct (cat ["[t ~ ~ t ~ ~ t ~]", "[~ t ~ ~ t ~ ~ t]", "[~ ~ t ~ ~ t ~ ~]"])
    dancehall = struct "[1 ~ ~ 1] ~"
    dancehall' f p = fast f $ struct "[1 ~ ~ 1] ~" $ p
    dancehall1 = struct "[1 ~ ~ 1] ~"
    dancehall2 = struct "[1 <~ 1> ~ 1] ~"
    dancehall3 = struct "[1 <~ 1 ~> <~ 1> 1] ~"
    dancehall4 = struct "[t ~ ~ t] [~ <~ ~ ~ t>]"
    erev b p = every b (rev) $ p
    grimer1 = struct "t [~ t]? ~ [t [~ t] [~ t?] t]/2"
    grimer2 = struct (cat ["t [~ t] t*2 [~ t]", "t [t [~ t] [~ t] t]"])
    swinger = struct "[t [t ~ t]]*2"
    swinger1 = struct "[[t ~ ~] [t ~ <~ t>]]*2"
    swinger2 = struct "[t [t ~ <~ t>]]*2"
    swinger3 = struct "[[t ~ <t ~>] [t ~ <~ t>]]*2"
    swinger4 = struct "[[t ~ <t ~>]]*4"
    son23 = struct "[[1 ~ ~ 1 ~ ~ 1 ~] [~ ~ 1 ~ 1 ~ ~ ~]]"
    son32 = struct "[[~ ~ 1 ~ 1 ~ ~ ~] [1 ~ ~ 1 ~ ~ 1 ~]]"
    rhumba23 = struct "[[~ ~ 1 ~ 1 ~ ~ ~] [1 ~ ~ 1 ~ ~ ~ 1]]"
    rhumba32 = struct "[[1 ~ ~ 1 ~ ~ ~ 1] [~ ~ 1 ~ 1 ~ ~ ~]]"
    asstruct p = struct (ascii p)
    asstruct' s a p = slow s $ struct (ascii a) $ p
    binstruct p = struct (binary p)
    binstruct' s a p = struct (binaryN s a) $ p
    spreadstruct bin = spread binstruct bin
    spreadstruct' speed bin = fast speed . spread binstruct bin
    offed = struct "[~ t]*2"
    offed' f p = fast f $ struct "[~ t]" $ p
    hatgain p = (# gain "1 0.9 0.5 0.8") $ p
    voxfilt p = ((# bpf (slowsaw * 2000)) . (# bpq 0.2)) $ p
    freqsweep = (range 200 2000 (sine))
    highsweep = (range 4000 8000 (sine))
    midsweep = (range 800 4000 (sine))
    lowsweep = (range 50 500 (sine))
    highsweep' s = slow s $ (range 4000 8000 (sine))
    midsweep' s = slow s $ (range 800 4000 (sine))
    lowsweep' s = slow s $ (range 50 500 (sine))
    chancervb p = (# room (scramble 8 "0!7 0.9")) . (# size (wchoose[(1,0.1), (0.5, 0.25), (0, 0.5)])) $ p
    mascan = ((>| n ("0 1 2 3 4 5 6 7" + "<0 8>")) . (# legato 1))

let scale = getScale (scaleTable ++ [("skepta", [0, 1, 4, 5, 7 , 8, 10, 11]), ("bhairavi", [0, 1, 3, 4, 7 , 8, 10])])

-- For song sections
let muteGroup g = mapM (streamMute tidal) g
    muteGroup' s g = muteGroup (fromJust $ lookup g s)
    mg = muteGroup
    mg' = muteGroup'

-- Orca like abbrevs
let a = accelerate
    b = begin
    d = discretise
    e = end
    g x = gain $ minimum [1.5,x] -- Safetied gain
    gi x = g (x * 0.1) -- Integer gain
    l = legato
    u = up
    o = octave
    t = trunc
    si = superimpose

-- Pattern effects
let bo p = trunc (segment 8 $ slowsaw + 0.125) $ p
    ob = trunc (slow 4 $ "<0.25 0.5 0.75 1>")
    ob' d = trunc (slow d $ "<0.25 0.5 0.75 1>")
    dubd p = sometimes (stut (choose[4, 8]) 0.0125 (1/8)) $ p
    dubd' d p = sometimes (stut (choose[4, 8]) d (1/8)) $ p
    dubd'' d t p = sometimes (stut (choose[4, 8]) d t) $ p
    stbdub d p = sometimesBy d (stut (choose[4, 8]) en en) $ p
    stbdub' d t p = sometimesBy d (stut (choose[4, 8]) en t) $ p
    stbdub'' d t f p = sometimesBy d (stut (choose[4, 8]) f t) $ p
    crumble = slow 2 $ sound "[k*16 ~]/2 ~" # n (run 32)
    uppit p = spread fast [1, 2, 3, 4, 5, 7, 8] $ p
    ruppit p = spread fast [1, 2, 3, 4] $ p
    crippery p = every 4 (jux (# accelerate "[-0.1..0.2]/4")) $ every 6 (jux (# accelerate "[-0.3..0.4]/2")) $ p
    rater = rarely (iter (cs 1 [4,8]))
    fastflip = fast "1 [2 1]"
    withflip = within (0.75, 1) (fast 2)
    someflip = sometimes (fast "1 [2 1]")
    oftflip = often (fast "1 [2 1]")
    rareflip = rarely (fast "1 [2 1]")
    htrapper = within (0.5, 0.75) (hurry "1 <3 1.5>")
    ftrapper = within (0.5, 0.75) (fast "1 <3 1.5>")
    ftrapper' s r = within (s, s + r) (fast "<3 1.5>")
    htrapper' s r = within (s, s + r) (hurry "<3 1.5>")
    microd p = often ((# delay 0.3) . (# delaytime (choose[(1/16), (1/32)])) . (# delayfeedback 0.8)) $ p
    microd' p = rarely ((# delay 0.3) . (# delaytime (choose[(1/16), (1/32)])) . (# delayfeedback 0.8)) $ p
    microstb' a p = stb a ((# delay 0.3) . (# delaytime (choose[(1/16), (1/32)])) . (# delayfeedback 0.8)) $ p
    foldedParty p = foldEvery [3, 7, 13] (spread ($) [fast 4, jux (rev), spike]) $ p
    simplefuck p = foldEVery [5, 6, 7] (rip 0.5 "<0.1 0.2 0.4>") $ every 7 (# coarse "{4 8 6 12 16}%14") $ every 8 (# accelerate "-0.5 0.5") $ p
    mess = simplefuck
    simplefuck' p = foldEVery [5, 6, 7] (rip' 0 1 8 0.5 "<0.1 0.2 0.4>") $ every 7 (# coarse "{4 8 6 12 16}%14") $ every 8 (# accelerate "-0.5 0.5") $ p
    mess' = simplefuck'
    crushit p = (# crush (range 3 8 $ slow 1.1 tri)) $ p
    messup = fuckery
    messitup = fuckery
    funkSpread = spread ($)
    funkySpread = spreadChoose ($)

-- Utility values
let bassCut = 0 -- for live performances

-- For minimal
let f = pF "f"
    freq = pF "freq"
    cfreq = pF "centerFreq"
    mul = pF "mul"
    scw a p = ((# s "scw-one") . (# loop a)) $ p

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
    -- Drums
    breaks = s "breaks"
    breaks8 p = slice 8 p $ s "breaks"
    breaks16 p = slice 16 p $ s "breaks"
    breaks32 p = slice 32 p $ s "breaks"
    breaksp16 p = splice 16 p $ s "breaks"
    breaksp32 p = splice 32 p $ s "breaks"
    jukeclap = s "jukeit" # n 3

-- Params to control visuals
let vis = p "vis" . (|< s "dummy")
    rotx = pF "rotx"
    roty = pF "roty"
    rotz = pF "rotz"
    rotxa = pF "rotxa"
    rotya = pF "rotya"
    rotza = pF "rotza"
    posx = pF "posx"
    posy = pF "posy"
    posz = pF "posz"
    pos' = grp [mF "posx", mF "posy", mF "posz"]
    pos x y z = posx x # posy y # posz z
    dolly = pF "dolly"
    sep = pF "sep"
    fs = pF "fs"
    is = pF "is"
    vs = pF "vs"
    saxis = pS "saxis"

-- Params for moogbass
chor = pF "chor"

-- Params for SuperFM synth
-- Taken from https://club.tidalcycles.org/t/superfm/1761/14

-- sets the amount of operator 'op' in the superfm output mix
-- (1 <= op <= 6)
fmamp :: Int -> Pattern Double -> ControlPattern
fmamp op = pF ("amp" ++ show op)

-- sets the ratio for operator 'op'.
-- the frequency is note * ratio + detune Hz
-- (1 <= op <= 6)
fmratio :: Int -> Pattern Double -> ControlPattern
fmratio op = pF ("ratio" ++ show op)

-- set the detune for operator 'op'
fmdetune :: Int -> Pattern Double -> ControlPattern
fmdetune op = pF ("detune" ++ show op)

-- set the modulation of oerator opa by operator opb
-- if opa == opb, then the modulation amount is multiplied by the
-- 'feedback' parameter
fmmod :: Int -> Int -> Pattern Double -> ControlPattern
fmmod opa opb = pF ("mod" ++ show opa ++ show opb)

-- feedback
fmfeedback :: Pattern Double -> ControlPattern
fmfeedback = pF "feedback"

-- Envelope definition: each operator has an envelop with 4 steps
fmeglevel :: Int -> Int -> Pattern Double -> ControlPattern
fmeglevel op step = pF ("eglevel" ++ show op ++ show step)

-- Envelope definition: sets the rate at which the envelope moves
-- between steps.  Low numbers are slow, high numbers are fast.
fmegrate :: Int -> Int -> Pattern Double -> ControlPattern
fmegrate op step = pF ("egrate" ++ show op ++ show step)

-- For muting
let d1m = p 1 . (const "~")
    d2m = p 2 . (const "~")
    d3m = p 3 . (const "~")
    d4m = p 4 . (const "~")
    d5m = p 5 . (const "~")
    d6m = p 6 . (const "~")
    d7m = p 7 . (const "~")
    d8m = p 8 . (const "~")

-- Mute patterns every x cycles
    d1m' a = p 1 . (|< orbit 0) . (every a (const "~"))
    d2m' a = p 2 . (|< orbit 1) . (every a (const "~"))
    d3m' a = p 3 . (|< orbit 2) . (every a (const "~"))
    d4m' a = p 4 . (|< orbit 3) . (every a (const "~"))
    d5m' a = p 5 . (|< orbit 4) . (every a (const "~"))
    d6m' a = p 6 . (|< orbit 5) . (every a (const "~"))
    d7m' a = p 7 . (|< orbit 6) . (every a (const "~"))
    d8m' a = p 8 . (|< orbit 7) . (every a (const "~"))
    d9m' a = p 9 . (|< orbit 8) . (every a (const "~"))

-- For Ableton Link -- Doesn't work here, but does
-- when you copy it to a tidal file (seems like an indentation issue)
-- link = do
          -- sock <- carabiner tidal 4 (-0.1)
          -- putStrLn "Starting Link synchronisation..."

-- For drum machines
let sendMidiClock = p "clock" $ fast 2 $ midicmd "midiClock*48" # s "iac"
let sendMidiStop = once $ midicmd "stop" # s "iac"
let sendMidiStart = once $ midicmd "start" #s "iac"

:set prompt "GoBombastic!>"
