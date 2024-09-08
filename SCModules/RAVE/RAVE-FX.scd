({
    OSCdef.new(\switchFXModel, {
		arg msg;
		var busKey = msg[1].asString().split($\:), busNum = 0, modelName;
		modelName = msg[1].asString;

        SynthDef("ravefx" ++ ~dirt.numChannels, { |out, ravemix = 1|
            var sig = In.ar(out, 1);
            sig = NN(modelName.asSymbol, \forward).ar(sig, 2048);
            ReplaceOut.ar(out, sig!2)
        },[\ir]).add;

        // (1..SuperDirt.maxSampleNumChannels).do { |numChannels|
        //     SynthDef("global_rave" ++ numChannels, {
        //         |dryBus, effectBus, ravewet=0 ravegain=0|
        //         var signal = In.ar(dryBus, 1);
        //         Out.ar(effectBus, NN(modelName.asSymbol, \forward).ar(signal, 2048));
        //     }, [\ir, \ir]).add;
        // };
        "Switched FX Model: ".post;
        modelName.postln;
    }, '/switchFXModel', nil, 57120);

0.5.wait;

	~dirt.addModule('ravefx', { |dirtEvent|
		dirtEvent.sendSynth('ravefx' ++ ~dirt.numChannels,
			[
				ravemix: ~ravemix,
				out: ~out
			]
		)
	}, { ~ravemix.notNil });

	SynthDef("ravefx" ++ ~dirt.numChannels, { |out, ravemix = 1|
        var sig = In.ar(out, 1);
        sig = NN(\percussion, \forward).ar(sig, 2048);
		ReplaceOut.ar(out, sig!2)
	},[\ir]).add;

(1..SuperDirt.maxSampleNumChannels).do { |numChannels|
    SynthDef("global_rave" ++ numChannels, { 
        |dryBus, effectBus, ravewet=0, ravegain=0|
        var signal = In.ar(dryBus, 1);
        Out.ar(effectBus, NN(\percussion, \forward).ar(signal, 2048));
    }, [\ir, \ir]).add;
};

0.2.wait;

~dirt.orbits.do { |x|
    var rave = GlobalDirtEffect(\global_rave, [\ravewet, \ravegain]);
    x.globalEffects = x.globalEffects
    .addFirst(rave);
    x.initNodeTree;
};

"\n\nSome ravin' effects".postln;
}.fork)