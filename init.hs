
import Sound.Tidal.Utils

import Sound.Tidal.Params

import Data.Maybe

import Control.Applicative

-- Aliases
let inter = interlace
    bpm(a) = setcps(a/120/2)
    bpmm b = cps (b/120/2)
    st = stack
    ct = cat
    cs i a = (segment i $ choose a)
    levl = gain . fmap (\x -> if abs x > 1 then 1 else x)
    chup = const "~"
    takeaway p = degradeBy "<[0.1..0.9>" $ p
    foldEVery = foldEvery
    accelrate = accelerate
    discretize = discretise
    loud p = (# gain 1.2) $ p
    louder p = (# gain 1.4) $ p
    soft p = (# gain 0.8) $ p
    softer p = (# gain 0.4) $ p
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
    rsin i o = rg' i o sin -- ranged' sine
    rcos i o = rg' i o cos -- ranged' cosine
    rtri i o = rg' i o tri -- ranged' triangle
    rsaw i o = rg' i o saw -- ranged' saw
    risaw i o = rg' i o isaw -- ranged' inverted saw
    rsq i o = rg' i o sq -- ranged' square
    -- rpw i o w = rg' i o pw w -- ranged' pulse
    -- rpw' i o w = rg' i o pw' w -- ranged' pulse'
    rrand i o = rg' i o rand -- ranged' rand
    rxsin i o = rgx i o sin -- ranged' exponential sine
    rxcos i o = rgx i o cos -- ranged' exponential cosine
    rxtri i o = rgx i o tri -- ranged' exponential triangle
    rxsaw i o = rgx i o saw -- ranged' exponential saw
    rxisaw i o = rgx i o isaw -- ranged' exponential inverted saw
    rxsq  i o = rgx i o sq -- ranged' exponential sqaure
    rxpw i o w = rgx i o pw w -- ranged' exponential pulse
    rxpw' i o w = rgx i o pw' w -- ranged' exponential pulse'
    rxrand i o = rgx i o rand -- ranged' exponential rand

    -- ranged continuous at freq
    rsinf i o f = fast f $ rsin i o -- ranged' sine at freq
    rcosf i o f = fast f $ rcos i o -- ranged' cosine at freq
    rtrif i o f = fast f $ rtri i o -- ranged' triangle at freq
    rsawf i o f = fast f $ rsaw i o -- ranged' saw at freq
    risawf i o f = fast f $ risaw i o  -- ranged' inverted saw at freq
    rsqf i o f = fast f $ rsq i o  -- ranged' square at freq
    -- rpwf i o w f = fast f $ rpw' i o w -- ranged' pulse at freq
    rrandf i o f = fast f $ rrand i o -- ranged' rand at freq
    rxsinf i o f = fast f $ rxsin i o -- ranged' exponential sine at freq
    rxcosf i o f = fast f $ rxcos i o -- ranged' exponential cosine at freq
    rxtrif i o f = fast f $ rxtri i o -- ranged' exponential triangle at freq
    rxsawf i o f = fast f $ rxsaw i o -- ranged' exponential saw at freq
    rxisawf i o f = fast f $ rxisaw i o -- ranged' exponential inverted saw at freq
    rxsqf i o f = fast f $ rxsq i o -- ranged' exponential square at freq
    rxpwf i o w f = fast f $ rxpw i o w -- ranged' exponential pulse at freq
    rxpwf' i o w f = fast f $ rxpw' i o w -- ranged' exponential pulse' at freq
    rxrandf i o f = fast f $ rxrand i o  -- ranged' exponential random at freq

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
let padgap1 p = gap "{16 8 2}" $ p
    padgap2 p = gap "{32 16 8 2}" $ p
    padgap3 p = gap "{8 4}" $ p
    padgap4 p = gap "{32 16}" $ p
    sliced1 p = slice 8 "[0..3]" $ p
    sliced2 p = slice 8 "[4..7]" $ p
    sliced3 p = slice 16 "[0..3]" $ p
    sliced4 p = slice 16 "[4..7]" $ p
    sliced8 p = slice 16 "[12..15]" $ p
    sliced8h p = slice 16 "[14..15]" $ p
    slice8 a p = slice 8 a $ p
    slice16 a p = slice 16 a $ p
    slice32 a p = slice 32 a $ p
    slice64 a p = slice 64 a $ p
    loop2 p = loopAt 2 $ p
    loop4 p = loopAt 4 $ p
    loop8 p = loopAt 8 $ p
    push1 p = ("{0 0.02 0 0.0025}" ~>) $ p
    push2 p = ("{0 0.05 0 0.005}" ~>) $ p
    push3 p = ("{0 0.07 0.02 0.05}" ~>) $ p
    push4 p = ("{0 0.07 0.02 0.05 0 0.02 0 0.0025}" ~>) $ p

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
    rip a b p = within (0.25, 0.75) (slow 2 . rev . stut 8 a b) p
    rip' a b c d e p = within (a, b) (slow 2 . rev . stut c d e) p
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
    rep n p = stut' (n-1) (0.125*3) (|*| gain "1") $ p
    rep' n p = stut' (n-1) (0.125/2*3) (|*| gain "1") $ p
    rep'' n p = stut' (n-1) (0.125/4*3) (|*| gain "1") $ p
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
    del' l t f = delay l # delaytime t # delayfeedback f
    delq' l t f = delay l # delaytime ((1/4)*t) # delayfeedback f
    dele' l t f = delay l # delaytime ((1/8)*t) # delayfeedback f
    dels' l t f = delay l # delaytime ((1/16)*t) # delayfeedback f
    scc' s c c' = shape s # coarse c # crush c'
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
    io' i o  = begin i # end o
    eq' h b l q = cutoff l # resonance q # bandf b # bandq q # hcutoff h # hresonance q
    tremolo' r d = tremolorate r # tremolodepth d
    phaser' r d = phaserrate r # phaserdepth d
    grain' = grp [mF "begin", mF "end"]
    grain s w = begin s # end (s + w)
    grain8 s w = begin (en' s) # end ((en' s) + w)
    grain16 s w = begin (sn' s) # end ((sn' s) + w)

-- Useful patterns
let footwork1 = struct "t(3,8,2)"
    footwork2 = struct "t(5,8)"
    footwork3 = struct "t(<3 5>,8,2)"
    footwork = struct "[t ~ ~ t ~ ~ t ~]"
    footwork' f p = fast f $ struct "[t ~ ~ t ~ ~ t ~]" $ p
    altfoot = struct (cat ["[t ~ ~ t ~ ~ t ~]", "[~ t ~ ~ t ~ ~ t]", "[~ ~ t ~ ~ t ~ ~]"])
    dancehall = struct "t ~ ~ t"
    grimer1 = struct "t [~ t]? ~ [t [~ t] [~ t?] t]/2"
    grimer2 = struct (cat ["t [~ t] t*2 [~ t]", "t [t [~ t] [~ t] t]"])
    swinger = struct "[t [t ~ t]]*2"
    swinger1 = struct "[[t ~ ~] [t ~ <~ t>]]*2"
    swinger2 = struct "[t [t ~ <~ t>]]*2"
    swinger3 = struct "[[t ~ <t ~>] [t ~ <~ t>]]*2"
    swinger4 = struct "[[t ~ <t ~>]]*4"
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

let scale = getScale (scaleTable ++ [("skepta", [0, 1, 4, 5, 7 , 8, 10, 11])])

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
    crumble = slow 2 $ sound "[k*16 ~]/2 ~" # n (run 32)
    uppit p = spread fast [1, 2, 3, 4, 5, 7, 8] $ p
    ruppit p = spread fast [1, 2, 3, 4] $ p
    crippery p = every 4 (jux (# accelerate "[-0.1..0.2]/4")) $ every 6 (jux (# accelerate "[-0.3..0.4]/2")) $ p
    rater = rarely (iter (cs 1 [4,8]))
    fastflip = fast "1 [2 1]"
    someflip = sometimes (fast "1 [2 1]")
    oftflip = often (fast "1 [2 1]")
    rareflip = rarely (fast "1 [2 1]")
    microd p = often ((# delay 0.3) . (# delaytime (choose[(1/16), (1/32)])) . (# delayfeedback 0.8)) $ p
    microd' p = rarely ((# delay 0.3) . (# delaytime (choose[(1/16), (1/32)])) . (# delayfeedback 0.8)) $ p
    foldedParty p = foldEvery [3, 7, 13] (spread ($) [fast 4, jux (rev), spike]) $ p
    simplefuck p = foldEVery [5, 6, 7] (rip 0.5 "<0.1 0.2 0.4>") $ every 7 (# coarse "{4 8 6 12 16}%14") $ every 8 (# accelerate "-0.5 0.5") $ p
    mess = simplefuck
    simplefuck' p = foldEVery [5, 6, 7] (rip' 0 1 8 0.5 "<0.1 0.2 0.4>") $ every 7 (# coarse "{4 8 6 12 16}%14") $ every 8 (# accelerate "-0.5 0.5") $ p
    mess' = simplefuck'
    crushit p = (# crush (range 3 8 $ slow 1.1 tri)) $ p
    messup = fuckery
    messitup = fuckery

-- Utility values
let bassCut = 0 -- for live performances

-- For minimal
let f = pF "f"
    freq = pF "freq"
    scw a p = ((# s "scw-one") . (# loop a)) $ p
    sinew p = ((# s "tutorial5")) $ p

-- For drum machines
let sendMidiClock = p "clock" $ fast 2 $ midicmd "midiClock*48" # s "iac"
let sendMidiStop = once $ midicmd "stop" # s "iac"
let sendMidiStart = once $ midicmd "start" #s "iac"

:set prompt "GoBombastic!>"
