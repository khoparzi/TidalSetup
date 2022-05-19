bassMap = fmap bassLookup
    where
      bassLookup "deep" = 1
      bassLookup "org" = 5
      bassLookup "org2" = 15
      bassLookup "smooth" = 6

let bass i = (bassMap i # s "bass")
