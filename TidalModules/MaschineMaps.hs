abstL = fmap drumLookup
    where
      -- Bass
      drumLookup "bs1" = 0
      drumLookup "bs2" = 1
      -- Closed hat
      drumLookup "hh" = 2
      drumLookup "hc1" = 2
      drumLookup "hc2" = 3
      -- Kick
      drumLookup "bd" = 5
      drumLookup "kd1" = 5
      drumLookup "kd2" = 6
      drumLookup "kd3" = 7
      -- Open hat
      drumLookup "ho" = 8
      drumLookup "ho1" = 8
      drumLookup "ho2" = 15
      -- fx
      drumLookup "fx" = 4
      drumLookup "fx1" = 4
      drumLookup "fx2" = 9
      drumLookup "fx3" = 10
      drumLookup "fx4" = 11
      -- Sd
      drumLookup "sd" = 12
      drumLookup "sd1" = 12
      drumLookup "sd2" = 13
      -- String
      drumLookup "st" = 14
buzzcutL = fmap drumLookup
    where
      drumLookup "cp" = 0
      -- Closed hat
      drumLookup "hh" = 1
      drumLookup "hc1" = 1
      drumLookup "hc2" = 2
      drumLookup "sh" = 26
      -- Kick
      drumLookup "bd" = 20
      drumLookup "kd1" = 20
      drumLookup "kd2" = 21
      drumLookup "pr" = 22
      drumLookup "rd" = 23
      -- fx
      drumLookup "fx" = 24
      drumLookup "fx1" = 24
      drumLookup "fx2" = 25
      drumLookup "fx3" = 27
      -- sd
      drumLookup "sd" = 26
      -- sd
      drumLookup "lt" = 28
      drumLookup "mt" = 28
      drumLookup "ht" = 28
jukeitL = fmap drumLookup
    where
      drumLookup "cp" = 3
      drumLookup "cp1" = 3
      drumLookup "cp2" = 4
      -- Closed hat
      drumLookup "hh" = 5
      drumLookup "hc" = 5
      drumLookup "ho" = 9
      drumLookup "ho1" = 9
      drumLookup "ho2" = 10
      -- Kick
      drumLookup "bd" = 6
      drumLookup "kd" = 6
      drumLookup "kd1" = 6
      drumLookup "kd2" = 7
      --
      drumLookup "lt" = 8
      drumLookup "mt" = 14
      drumLookup "ht" = 15
      -- sd
      drumLookup "sn" = 11
      drumLookup "sd" = 12
      -- Fx
      drumLookup "fx1" = 13
      drumLookup "fx2" = 0
      drumLookup "fx3" = 1
      drumLookup "fx4" = 2
      drumLookup "fx5" = 2

let abst i = (abstL i # s "abst")
    buzzcut i = (buzzcutL i # s "buzzcut")
    jukeit i = (jukeitL i # s "jukeit")
