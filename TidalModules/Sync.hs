-- For Ableton Link -- Doesn't work here, but does
-- when you copy it to a tidal file (seems like an indentation issue)
-- link = do
          -- sock <- carabiner tidal 4 (-0.1)
          -- putStrLn "Starting Link synchronisation..."

-- For drum machines
let sendMidiClock = p "clock" $ fast 2 $ midicmd "midiClock*48" # s "iac"
let sendMidiStop = once $ midicmd "stop" # s "iac"
let sendMidiStart = once $ midicmd "start" #s "iac"