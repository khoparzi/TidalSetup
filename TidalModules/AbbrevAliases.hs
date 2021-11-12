-- Aliases
let inter = interlace
    bpm(a) = setcps(a/120)
    bpmm b = cps (b/120)
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
    slowsine = (slow 4 $ sine)
    slowersine = (slow 8 $ sine)
    slowsaw = (slow 4 $ saw)
    slowersaw = (slow 8 $ saw)
    slowtri = (slow 4 $ tri)
    slowertri = (slow 8 $ tri)
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
    x e = (10 ** e) -- For exponent frequencies
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

-- Grain shorthands
    grain8 s w = begin ((1/8) * s) # end (((1/8) * s) + w)
    grain16 s w = begin ((1/16) * s) # end (((1/16) * s) + w)

-- From jArm https://gist.github.com/jarmitage/627a7d5a9263475ba53f725f1405d6a2
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
    io' i o  = begin i # end o
    eq' h b l q = cutoff l # resonance q # bandf b # bandq q # hcutoff h # hresonance q
    tremolo' r d = tremolorate r # tremolodepth d
    phaser' r d = phaserrate r # phaserdepth d

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

-- For song sections
let muteGroup g = mapM (streamMute tidal) g
    muteGroup' s g = muteGroup (fromJust $ lookup g s)
    mg = muteGroup
    mg' = muteGroup'
