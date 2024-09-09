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
)}.fork;)