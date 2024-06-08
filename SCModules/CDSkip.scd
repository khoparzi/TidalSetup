// generate the synth def for cdskip
(
    (1..SuperDirt.maxSampleNumChannels).do { |numChannels|

        SynthDef("dirt_global_cdskip" ++ numChannels, {
            |dryBus, effectBus, cdfreeze = 0, cdpos = 0, cdresetFreq = 1, cdskipFreq = 1, cdreset = 0, cdskip = 0|
            var sig = In.ar(dryBus, numChannels);

            var signal = CDSkip.ar(
                sig ! 2,
                pos: SinOsc.ar(cdpos),
                skipTrigger: Impulse.ar(cdskipFreq, mul: cdskip),
                resetTrigger: Impulse.ar(cdresetFreq, mul: cdreset),
                freeze: cdfreeze
            );

            Out.ar(effectBus, signal)
        }, [\ir, \ir]).add;
    };
)