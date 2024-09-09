// generate the synth def for cdskip
({
    var busses = ~dirt.orbits.collect { |x| x.dryBus };
    (1..SuperDirt.maxSampleNumChannels).do { |numChannels|

        SynthDef("dirt_global_cdskip" ++ numChannels, {
            |dryBus, effectBus, skipauto = 0, cdskipFreq = 1, cdskip = 0|
            var sig = In.ar(dryBus, numChannels);

            var signal = CDSkip.ar(
                sig ! 2,
                // autoMode: skipauto, // Disabled right now because it can't be reliably switched off
                skipTrigger: Impulse.ar(cdskipFreq, mul: cdskip)
            );

            Out.ar(effectBus, signal)
        }, [\ir, \ir]).add;
    };
0.2.wait;
    // add cdskip effect to the orbits
    ~dirt.orbits.do { |x|
        var cdskip = GlobalDirtEffect(\dirt_global_cdskip, [\skipauto, \cdskipFreq, \cdskip]);
        x.globalEffects = x.globalEffects.addFirst(cdskip);
        x.initNodeTree;
    };
    "Added skip global effect".postln;
0.2.wait;
    SynthDef(\skiporb, { 
        |out, amp = 1, n = 0, pan=0.5, cdpos = 0, cdresetFreq = 1, cdskipFreq = 1, cdreset = 0, cdskip = 0|
        var bus, sound;

        bus = Select.kr(n, busses);
        sound = InFeedback.ar(bus, ~dirt.numChannels);
        sound = CDSkip.ar(
                sound,
                // pos: SinOsc.ar(cdpos),
                // freeze: cdfreeze,
                pos: cdpos,
                skipTrigger: Impulse.ar(cdskipFreq, mul: cdskip)
            );
        sound = CDSkip.ar(
                sound,
                pos: cdpos,
                resetTrigger: Impulse.ar(cdresetFreq, mul: cdreset),
                autoSpeed: 1
            );

        Out.ar(out, DirtPan.ar(sound, ~dirt.numChannels, pan))
    }).add;
    "Added skip synth".postln;
}.fork;)