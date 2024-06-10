-- Params for moogbass
chor = pF "chor"
moogspan = pF "span"

-- For minimal
let f = pF "f"
    freq = pF "freq"
    cfreq = pF "centerFreq"
    mul = pF "mul"
    rq = pF "rq"
    scw a p = ((# s "scw-one") . (# loop a)) $ p
-- For shepard synth
    freqModAmount = pF "freqModAmount"
    tFreq = pF "tFreq"
    overlap = pF "overlap"
    numOsc = pF "numOsc"