import qualified Sound.OSC as OSC
import qualified Sound.OSC.FD as FD
import System.IO.Unsafe (unsafePerformIO)

-- | Global reference of the UDP port used to
-- | communicate with SCLang
globalUDPRef = unsafePerformIO $ OSC.openUDP "127.0.0.1" 57120

-- | Takes a path and an argument (both strings)
-- | and sends them to SCLang
-- | oscStringMessage :: String -> String -> IO ()
oscStringMessage path str = FD.sendMessage globalUDPRef $ OSC.Message path [OSC.string str]

let tosc m = oscStringMessage m ""
    tosc' m a = oscStringMessage m a
    loadVis = oscStringMessage "/loadVis" ""
    load s = oscStringMessage "/loadBank" s
    loadFolder s = oscStringMessage "/loadFolder" s
    loadFolders s = oscStringMessage "/loadFolders" s
    -- loadMaschine s = oscStringMessage "/loadFolder" ("/Users/khoparzi/Documents/Native Instruments/Maschine 2/Groups/" ++ s)
    loadPath s = oscStringMessage "/loadPath" s
    free s = oscStringMessage "/freeBank" s
    quitsc = oscStringMessage "/exit" ""
    rebootsc = oscStringMessage "/reboot" ""
    record = oscStringMessage "/record" ""
    stoprecord = oscStringMessage "/stoprecord" ""
