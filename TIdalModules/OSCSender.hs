-- Based on https://gist.github.com/jarmitage/100ec087997100b448f173a0258d1885
-- Bitwig Tidal API

:{
let scAddress   = "127.0.0.1"
    scPort      = 57120
    bwLatency   = 0.02
    bwPreamble  = []
    bwTimestamp = BundleStamp
:}

:{
let bwAddress   = "127.0.0.1"
    bwPort      = 8000
    bwLatency   = 0.02
    bwPreamble  = []
    bwTimestamp = BundleStamp
:}

:{
-- sdBankLoader = OSCTarget {oName = "SuperDirtBankLoader", oAddress = scAddress, oPort = scPort,
--                       oPath = "/loadBank",
--                       oShape = Just [("bank", Just $ VF 0)],
--                       oLatency = bwLatency, oPreamble = bwPreamble, oTimestamp = bwTimestamp}
:}

:{
bwTrackTarget = OSCTarget {oName = "BitwigTrack", oAddress = bwAddress, oPort = bwPort,
                          oPath = "/track/{track}/{action}",
                          oShape = Just [("volume", Just $ VF 0), ("pan", Just $ VF 0)],
                          oLatency = bwLatency, oPreamble = bwPreamble, oTimestamp = bwTimestamp}
:}

:{
bwTrack  <- startTidal bwTrackTarget  (defaultConfig {cFrameTimespan = 1/20})
-- scBank <- startTidal sdBankLoader  (defaultConfig {cFrameTimespan = 1/20})
:}

:{
    let channel  = pI "channel"
        value   = pF "value"
        -- vi       = pI "value"
        action   = pS "action"
        tempo    = pF "raw"
        scene    = pI "scene"
        track    = pI "track"
        clip     = pI "clip"
        -- p c v    = value (range 0 16384 v) # channel c
    let bank = pS "bank"
        loadBank = streamOnce scBank
:}
