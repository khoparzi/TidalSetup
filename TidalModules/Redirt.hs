:{
linlin slo shi dlo dhi value =
    let norm = (value - slo) / shi
    in dlo + norm * (dhi - dlo)
:}

:{
let renoise = Target {
        oName="renoise",
        oAddress="127.0.0.1",
        oPort=8000,
        oLatency=0.2,
        oSchedule=Live,
        oWindow=Nothing,
        oBusPort=Nothing,
        oHandshake=False
        }
    formats = [
        -- send note_on event
        OSC "/renoise/trigger/note_on" $ ArgList [
            ("instrument", Just $ VI (-1)),
            ("track", Just $ VI (-1)),
            ("note", Nothing),
            ("velocity", Just $ VI 127)
            ],

        -- send note_off event (not very useful if you ask me)
        OSC "/renoise/trigger/note_off" $ ArgList [
            ("sound", Just $ VI (-1)),
            ("track", Just $ VI (-1)),
            ("noteOff", Nothing)
            ],

        -- instrument macro controls
        OSC "/renoise/song/instrument/{instrument}/macro1" $ ArgList [("m1", Nothing)],
        OSC "/renoise/song/instrument/{instrument}/macro2" $ ArgList [("m2", Nothing)],
        OSC "/renoise/song/instrument/{instrument}/macro3" $ ArgList [("m3", Nothing)],
        OSC "/renoise/song/instrument/{instrument}/macro4" $ ArgList [("m4", Nothing)],
        OSC "/renoise/song/instrument/{instrument}/macro5" $ ArgList [("m5", Nothing)],
        OSC "/renoise/song/instrument/{instrument}/macro6" $ ArgList [("m6", Nothing)],
        OSC "/renoise/song/instrument/{instrument}/macro7" $ ArgList [("m7", Nothing)],
        OSC "/renoise/song/instrument/{instrument}/macro8" $ ArgList [("m8", Nothing)],

        -- set device parameters (effects)
        OSC "/renoise/song/track/{track}/device/{device}/set_parameter_by_name" $ ArgList [
            ("param", Nothing),
            ("value", Nothing)
            ]
        ]
    -- use t1 .. t8 to select the track
    track = pI "track"
    -- sound to select an instrument
    sound = pI "instrument"
    s = sound
    -- triggers note_on event
    note = pI "note"
    n = note
    -- triggers note_off event
    noteOff = pI "noteOff"
    -- gain controls velocity, remapping from [0 1] to [0 127]
    -- gain = pF "velocity" . linlin 0 1 0 127
    -- instument macros
    m1 = pF "m1"
    m2 = pF "m2"
    m3 = pF "m3"
    m4 = pF "m4"
    m5 = pF "m5"
    m6 = pF "m6"
    m7 = pF "m7"
    m8 = pF "m8"
    -- device params
    device = pI "device"
    dev = device
    param = pS "param"
    par = param
    value = pF "value"
    val = value
    -- redefining octave. use it with operator |+ instead of #
    octave = \p -> note (p |* 12)
    renoisemap = [(renoise, formats)]
:}

tidal <- startStream defaultConfig renoisemap

:{
let rt = streamReplace tidal
    t1 = rt 1 . (|< track 1)
    t2 = rt 2 . (|< track 2)
    t3 = rt 3 . (|< track 3)
    t4 = rt 4 . (|< track 4)
    t5 = rt 5 . (|< track 5)
    t6 = rt 6 . (|< track 6)
    t7 = rt 7 . (|< track 7)
    t8 = rt 8 . (|< track 8)
:}
