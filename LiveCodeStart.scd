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

// ~instruments.at(\fm8).editor
// ~instruments.at(\massiveX).editor

// Test a tidal synth
// (type:\dirt, dirt: ~dirt, s: \soshats, n: -10, \release: 0.6).play;

// (type:\dirt, dirt: ~dirt, s: \vst, n: 0, vstName: "fm8").play;

// m = NetAddr("127.0.0.1", 57120); // loopback
// m.sendMsg("/sampleInfo", "");

// Show all synths
// SynthDescLib.global.read;
// 0.2.wait;
// SynthDescLib.global.browse;
// Show synth parameters
// SuperDirt.postTidalParameters([\pmsin, \default]);

// (type:\dirt, dirt: ~dirt, s: \bossdr660bd, n: 0).play;
