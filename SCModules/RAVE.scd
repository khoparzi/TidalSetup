(
	~raveSynth = {};
	currentEnvironment.put(\modelsPath, "/Users/khoparzi/Dev/pretrained_models/");
	NN.load(\percussion, ~modelsPath++"percussion.ts");
	currentEnvironment.put(\model, "percussion");
	OSCdef.new(\loadModel, {
		arg msg;
		postf("% : %\n", msg[0], msg[1]);
		case
			{msg[1].asString == "strengj"} {
			    NN.load(msg[1].asSymbol, ~modelsPath++"mrp_strengjavera_b2048_r44100_z16.ts");
			}
			{msg[1].asString == "nasa"} {
			    NN.load(msg[1].asSymbol, ~modelsPath++"nasa.ts", action: _.describe);
			}
			{msg[1].asString == "percussion"} {
			    NN.load(msg[1].asSymbol, ~modelsPath++"percussion.ts");
			}
			{msg[1].asString == "dnb"} {
			    NN.load(msg[1].asSymbol, ~modelsPath++"dnb-embedded2.1m.ts");
			}
			{msg[1].asString == "meta"} {
			    NN.load(msg[1].asSymbol, ~modelsPath++"meta-v2.ts");
			}
			{msg[1].asString == "cinema-textures"} {
			    NN.load(msg[1].asSymbol, ~modelsPath++"cinema-textures-v3_1.45m.ts");
			}
			{msg[1].asString == "guitar"} {
			    NN.load(msg[1].asSymbol, ~modelsPath++"guitar_iil_b2048_r48000_z16.ts");
			}
			{msg[1].asString == "sax"} {
			    NN.load(msg[1].asSymbol, ~modelsPath++"sax_soprano_franziskaschroeder_b2048_r48000_z20.ts");
			}
			{msg[1].asString == "bhagwat"} {
			    NN.load(msg[1].asSymbol, ~modelsPath++"bhagwat-v2_1m.ts");
			}
		}, '/loadModel', nil, 57120);

	OSCdef.new(\switchModel, {
		arg msg;
		currentEnvironment.put(\model, msg[1].asString);
		"\n\n Switched model to" ++ ":" ++ msg[1].postln;

		SynthDef(\raveclick, { |out, sustain = 1, speed = 1, begin = 0, end = 1, pan, accelerate, offset|
			var env, sound, rate, phase, forward;
			phase = Line.ar(begin, end, sustain);
			rate = (begin + 1) * (speed + Sweep.kr(1, accelerate));
			sound = Impulse.ar(0);
			sound = Decay.ar(sound, sustain);
			forward = NN(msg[1].asSymbol, \forward).ar(sound, 2048);
			OffsetOut.ar(out,
				DirtPan.ar(forward, ~dirt.numChannels, pan)
			)
		}).add;

		SynthDef(\raveclick2, { arg out, gate = 1, centerFreq = 15000, rq = 0.9, mul = 25, pan = 0, amp = 1;
			var env, noise, sig, forward;
			env = EnvGen.ar(Env.perc(0.001, 0.00001 * LFNoise1.kr(4).abs, 1, -4), gate, doneAction: 0);
			noise = PinkNoise.ar(env);
			sig = BPF.ar(noise, centerFreq.max(1), rq, mul).fold(-1, 1);
			env = amp.curvelin(0, 1, 0, 1, log(10));
			forward = NN(msg[1].asSymbol, \forward).ar(sig * env, 2048);
			OffsetOut.ar(out, DirtPan.ar(forward, ~dirt.numChannels, pan));
		}).add;

		SynthDef(\ravesine, {
		  |out, sustain = 0.1, freq = 440, speed = 1, begin=0, end=0.1, pan=0.5, accelerate, offset|
		  var env, envTimes, envGen, line, sound, rate, phase, forward;
		  envTimes = [1,100];
		  envTimes = envTimes/envTimes.sum;
		  env = Env([0,1,0],envTimes);
		  line = Line.ar(0,1,sustain,doneAction:2);
		  envGen = IEnvGen.ar(env,line);
		  sound = SinOsc.ar(freq*speed) * envGen;
		  forward = NN(msg[1].asSymbol, \forward).ar(sound, 2048);
		  // OffsetOut.ar(out, DirtPan.ar(forward, ~dirt.numChannels, pan, envGen));
		  OffsetOut.ar(out,
				DirtPan.ar(forward, ~dirt.numChannels, pan)
		  )
		}).add;

	}, '/switchModel', nil, 57120);

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

	OSCdef.new(\freeRave, {
	        ~raveSynth.free;
	}, '/freeRave', nil, 57120);
);