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

let -- mutable effects
    cloudspitch = pF "cloudspitch"
    cloudspos = pF "cloudspos"
    cloudssize = pF "cloudssize"
    cloudsdens = pF "cloudsdens"
    cloudstex = pF "cloudstex"
    cloudswet = pF "cloudswet"
    cloudsgain = pF "cloudsgain"
    cloudsspread = pF "cloudsspread"
    cloudsrvb = pF "cloudsrvb"
    cloudsfb = pF "cloudsfb"
    cloudsfreeze = pF "cloudsfreeze"
    cloudsmode = pF "cloudsmode"
    cloudslofi = pF "cloudslofi"
    clouds p s d t = cloudsgain 1 # cloudspos p # cloudssize s # cloudsdens d # cloudstex t
    cloudsblend w s f r = cloudsgain 1 # cloudswet w # cloudsspread s # cloudsfb f # cloudsrvb r
    elementspitch = pF "elementspitch"
    elementsstrength = pF "elementsstrength"
    elementscontour = pF "elementscontour"
    elementsbowlevel = pF "elementsbowlevel"
    elementsblowlevel = pF "elementsblowlevel"
    elementsstrikelevel = pF "elementsstrikelevel"
    elementsflow = pF "elementsflow"
    elementsmallet = pF "elementsmallet"
    elementsbowtimb = pF "elementsbowtimb"
    elementsblowtimb = pF "elementsblowtimb"
    elementsstriketimb = pF "elementsstriketimb"
    elementsgeom = pF "elementsgeom"
    elementsbright = pF "elementsbright"
    elementsdamp = pF "elementsdamp"
    elementspos = pF "elementspos"
    elementsspace = pF "elementsspace"
    elementsmodel = pI "elementsmodel"
    elementseasteregg = pI "elementseasteregg"
    mu = pF "mu"
    ringsfreq = pF "ringsfreq"
    ringsstruct = pF "ringsstruct"
    ringsbright = pF "ringsbright"
    ringsdamp = pF "ringsdamp"
    ringspos = pF "ringspos"
    ringsmodel = pF "ringsmodel"
    ringspoly = pI "ringspoly"
    ringsinternal = pI "ringsinternal"
    ringseasteregg = pI "ringseasteregg"
    rings f s b d p = ringsfreq f # ringsstruct s # ringsbright b # ringsdamp d # ringspos p
    ripplescf = pF "ripplescf"
    ripplesreson = pF "ripplesreson"
    ripplesdrive = pF "ripplesdrive"
    ripples c r d = ripplescf c # ripplesreson r # ripplesdrive d
    verbgain = pF "verbgain"
    verbwet = pF "verbwet"
    verbtime = pF "verbtime"
    verbdamp = pF "verbdamp"
    verbhp = pF "verbhp"
    verbfreeze = pI "verbfreeze"
    verbdiff = pF "verbdiff"
    verb w t d h = verbgain 1 # verbwet w # verbtime t # verbdamp d # verbhp h
    warpsalgo = pI "warpsalgo"
    warpstimb = pF "warpstimb"
    warpsosc = pI "warpsosc"
    warpsfreq = pF "warpsfreq"
    warpsvgain = pF "warpsvgain"
    warpseasteregg = pI "warpseasteregg"