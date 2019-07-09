-- Aliases
let inter = interlace
    bpm(a) = setcps(a/120)
    st = stack
    ct = cat
    gi x = gain (x * 0.1) -- Integer gain
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
    rsine a b = (range a b $ sine)
    rsine' a b c = (range a b $ slow c $ sine)
    rsaw a b = (range a b $ saw)
    rsaw' a b c = (range a b $ slow c $ saw)
    rtri a b = (range a b $ tri)
    rtri' a b c = (range a b $ slow c $ tri)
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

-- continous function shorthands
let sin = sine
    cos = cosine
    sq  = square

-- Useful patterns
let footwork1 = struct "t(3,8,2)"
    footwork2 = struct "t(5,8)"
    footwork3 = struct "t(<3 5>,8,2)"
    dancehall = struct "t ~ ~ t"
    grimer1 = struct "t [~ t]? ~ [t [~ t] [~ t?] t]/2"
    grimer2 = struct (cat ["t [~ t] t*2 [~ t]", "t [t [~ t] [~ t] t]"])
    hatgain p = (# gain "1 0.9 0.5 0.8") $ p
    voxfilt p = ((# bpf (slowsaw * 2000)) . (# bpq 0.2)) $ p
    freqsweep = (range 200 2000 (sine))
    highsweep = (range 4000 8000 (sine))
    midsweep = (range 800 4000 (sine))
    lowsweep = (range 50 500 (sine))

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
    l = legato
    u = up
    o = octave
    t = trunc

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
    microd p = often ((# delay 0.3) . (# delaytime (choose[(1/16), (1/32)])) . (# delayfeedback 0.8)) $ p
    foldedParty p = foldEvery [3, 7, 13] (spread ($) [fast 4, jux (rev), spike]) $ p
    simplefuck p = foldEVery [5, 6, 7] (rip 0.5 "<0.1 0.2 0.4>") $ every 7 (# coarse "{4 8 6 12 16}%14") $ every 8 (# accelerate "-0.5 0.5") $ p
    mess = simplefuck
    simplefuck' p = foldEVery [5, 6, 7] (rip' 0 1 8 0.5 "<0.1 0.2 0.4>") $ every 7 (# coarse "{4 8 6 12 16}%14") $ every 8 (# accelerate "-0.5 0.5") $ p
    mess' = simplefuck'
    crushit p = (# crush (range 3 8 $ slow 1.1 tri)) $ p

-- Utility values
let bassCut = 0 -- for live performances

-- For minimal
let f = pF "f"
    scw a p = ((# s "scw-one") . (# loop a)) $ p
    sinew p = ((# s "tutorial5")) $ p

-- For drum machines
let sendMidiClock = p "clock" $ fast 2 $ midicmd "midiClock*48" # s "iac"
let sendMidiStop = once $ midicmd "stop" # s "iac"
let sendMidiStart = once $ midicmd "start" #s "iac"

:set prompt "GoBombastic!>"
