-- Params for SuperFM synth
-- Taken from https://club.tidalcycles.org/t/superfm/1761/14

    -- sets the amount of operator 'op' in the superfm output mix
    -- (1 <= op <= 6)
    -- fmamp :: Int -> Pattern Double -> ControlPattern
let fmamp op = pF ("amp" ++ show op)

    -- sets the ratio for operator 'op'.
    -- the frequency is note * ratio + detune Hz
    -- (1 <= op <= 6)
    -- fmratio :: Int -> Pattern Double -> ControlPattern
    fmratio op = pF ("ratio" ++ show op)

    -- set the detune for operator 'op'
    -- fmdetune :: Int -> Pattern Double -> ControlPattern
    fmdetune op = pF ("detune" ++ show op)

    -- set the modulation of oerator opa by operator opb
    -- if opa == opb, then the modulation amount is multiplied by the
    -- 'feedback' parameter
    -- fmmod :: Int -> Int -> Pattern Double -> ControlPattern
    fmmod opa opb = pF ("mod" ++ show opa ++ show opb)

    -- feedback
    -- fmfeedback :: Pattern Double -> ControlPattern
    fmfeedback = pF "feedback"

    -- Envelope definition: each operator has an envelop with 4 steps
    -- fmeglevel :: Int -> Int -> Pattern Double -> ControlPattern
    fmeglevel op step = pF ("eglevel" ++ show op ++ show step)

    -- Envelope definition: sets the rate at which the envelope moves
    -- between steps.  Low numbers are slow, high numbers are fast.
    -- fmegrate :: Int -> Int -> Pattern Double -> ControlPattern
    fmegrate op step = pF ("egrate" ++ show op ++ show step)