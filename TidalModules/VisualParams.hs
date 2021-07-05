-- visualTarget for pattern visualizing
:{

visualTarget :: Target
visualTarget = Target {oName = "visualiser",
                       oAddress = "127.0.0.1",
                       oPort = 3333,
                       oLatency = 0.02,
                       oSchedule = Live,       -- The scheduling method - see below
                       oWindow = Nothing,      -- Not yet used
                       oHandshake = False,     -- SuperDirt specific
                       oBusPort = Nothing      -- Also SuperDirt specific
                      }
visualShape :: OSC
visualShape = OSC "/frag/play" $ ArgList [("v", Nothing),
                                          ("sep", Just $ VF 1)
                                      ]
v = pS "v"
fs = pI "fs"
is = pI "is"
vs = pI "vs"
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
saxis = pS "saxis"
vword = pS "vword"
visOscMap = [(visualTarget, [visualShape])]
:}

visuals <- startStream (defaultConfig {cCtrlListen = False}) visOscMap

-- Params to control visuals
let vis = streamReplace visuals
    visHush = streamHush visuals
    visOnce = streamOnce visuals
    v1 = vis 1 . (|< orbit 0)
    sendVisOsc path str = sendO False (sListen visuals) (head $ sCxs visuals) $ OSC.Message path [OSC.string str]
    bloomVis = sendVisOsc "/bloom" "true"
    kaliedVis = sendVisOsc "/kaleid" "true"
    pixVis = sendVisOsc "/pixelate" "true"
    godrayVis = sendVisOsc "/godray" "true"
    beatVis = sendVisOsc "/beat" "true"
    uiVis = sendVisOsc "/ui" "true"
    zminVis zmin = sendVisOsc "/zmin" zmin
    zmaxVis zmax = sendVisOsc "/zmax" zmax
