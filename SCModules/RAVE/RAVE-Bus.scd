(
    ~raveSynth = {};
	OSCdef.new(\switchBusModel, {
		arg msg;
		var busKey = msg[1].asString().split($\:), busNum = 0, modelName;
		modelName = msg[1].asString;

		// RAVE Forward
		~raveOutBus = Bus.new(index:0, numChannels:2);
		~raveBus = Bus.audio(s, numChannels:~dirt.numChannels);
		if (busKey.size>1, {
			busNum = busKey[1].asInteger - 1;
			~dirt.orbits[busNum].outBus = ~raveBus;
			modelName = busKey[0].asString;
	    	postf("% loaded on orbit %\n", modelName, busKey[1].asInteger);
		},
		{
			~dirt.orbits[0].outBus = ~raveBus;
		});
		~raveSynth = {
			var in, forward, out;
			in = Mix.new(InBus.ar(~raveBus, 2)) * 0.5;
			forward = NN(modelName.asSymbol, \forward).ar(in, 2048);
			out = forward;
		    // out = LinXFade2.ar(in, out, \raveMix.kr(1), \raveGain.kr(1));
			out = (in * (1 - \raveMix.kr(1))) + (out * \raveMix.kr(1));
			out = out * \raveGain.kr(1) * 0.8;
			out = out!~dirt.numChannels;
			// out = PitchShift.ar(out, \ravePitchGrain.kr(0.2), \ravePitchRatio.kr(1.0), \ravePitchDisperse.kr(0.003), \ravePitchTimeDisperse.kr(0.003));
		}.play(outbus:~raveOutBus);
	}, '/switchBusModel', nil, 57120);

	OSCdef.new(\freeRaveBus, {
			~dirt.orbits[0].outBus = 0;
	        ~raveSynth.free;
	}, '/freeRaveBus', nil, 57120);
)