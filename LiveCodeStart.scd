(// With minimal samples, four orbits
    s.options.sampleRate = 44100;
    s.options.numBuffers = 1024 * 256;
    s.options.memSize = 8192 * 32;
    s.options.maxNodes = 1024 * 32;
    s.options.numInputBusChannels = 0;
    s.options.numOutputBusChannels = 2;
    s.latency = nil;
    currentEnvironment.put(\setupPath, "/Users/khoparzi/Dev/TidalSetup/");
    currentEnvironment.put(\samplesPath, "/Users/khoparzi/Music/Samples/");
    currentEnvironment.put(\batteryPath, "/Users/Shared/Battery 4 Factory Library/Samples/Drums/");

    (~setupPath++"/SCModules/BasicSuperDirt.scd").load();
    // To send to another address
    // currentEnvironment.put(\visAddr, "khoparzi-jnano.local");
    (~setupPath++"/SCModules/OSCDefs.scd").load();
    // (~setupPath++"VSTLoader.scd").load(); // Temporary VST loader
);

// (
// s.options.outDevice = "Loopback Audio";
// s.options.outDevice = "Macbook Air Speakers";
// s.reboot;
// )

// m = NetAddr("127.0.0.1", 57120); // loopback
// m.sendMsg("/sampleInfo", "");

// (type:\dirt, dirt: ~dirt, s: \bossdr660bd, n: 0).play;
