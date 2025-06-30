import qualified Sound.Osc as OSC
import qualified Sound.Osc.Fd as FD
import System.IO.Unsafe (unsafePerformIO)

-- sendOsc :: OSC.Address_Pattern -> String -> IO ()
let otherHead = (!! 0)
    sendOsc path str = sendO False (sListen tidal) (otherHead $ sCxs tidal) $ OSC.Message path [OSC.string str]

let tosc m = sendOsc m ""
    tosc' m a = sendOsc m a
    loadVis = sendOsc "/loadVis" ""
    load s = sendOsc "/loadBank" s
    loadFolder s = sendOsc "/loadFolder" s
    loadFolders s = sendOsc "/loadFolders" s
    -- loadMaschine s = sendOsc "/loadFolder" ("/Users/khoparzi/Documents/Native Instruments/Maschine 2/Groups/" ++ s)
    loadPath s = sendOsc "/loadPath" s
    free s = sendOsc "/freeBank" s
    quitsc = sendOsc "/exit" ""
    rebootsc = sendOsc "/reboot" ""
    record = sendOsc "/record" ""
    stoprecord = sendOsc "/stoprecord" ""
    iacLatency l = sendOsc "/iacLatency" l
    sidechain = sendOsc "/sidechained" ""
    showsynths = sendOsc "/showsynths" ""
    showsamples = sendOsc "/sampleInfo" ""
