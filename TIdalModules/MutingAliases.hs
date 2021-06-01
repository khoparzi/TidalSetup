-- For muting
let d1m = p 1 . silence
    d2m = p 2 . silence
    d3m = p 3 . silence
    d4m = p 4 . silence
    d5m = p 5 . silence
    d6m = p 6 . silence
    d7m = p 7 . silence
    d8m = p 8 . silence
-- Mute patterns every x cycles
    d1m' a = p 1 . (|< orbit 0) . (every a silence)
    d2m' a = p 2 . (|< orbit 1) . (every a silence)
    d3m' a = p 3 . (|< orbit 2) . (every a silence)
    d4m' a = p 4 . (|< orbit 3) . (every a silence)
    d5m' a = p 5 . (|< orbit 4) . (every a silence)
    d6m' a = p 6 . (|< orbit 5) . (every a silence)
    d7m' a = p 7 . (|< orbit 6) . (every a silence)
    d8m' a = p 8 . (|< orbit 7) . (every a silence)
    d9m' a = p 9 . (|< orbit 8) . (every a silence)