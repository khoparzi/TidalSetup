(
	var busses = ~dirt.orbits.collect { |x| x.dryBus };
    "\n\nSome ravin' synths".postln;
    OSCdef.new(\switchSynthModel, {
		arg msg;
		"\n\n Switched synth model to" ++ ":" ++ msg[1].postln;

		SynthDef(\raveclick, { |out, sustain = 1, speed = 1, begin = 0, end = 1, pan, accelerate, offset|
			var env, sound, rate, forward;
			rate = (begin + 1) * (speed + Sweep.kr(1, accelerate));
			sound = Impulse.ar(0);
			sound = Decay.ar(sound, sustain);
			forward = NN(msg[1].asSymbol, \forward).ar(sound, 2048);
			Out.ar(out, DirtPan.ar(forward, ~dirt.numChannels, pan))
		}).add;

		SynthDef(\raveclick2, { arg out, gate = 1, centerFreq = 15000, rq = 0.9, mul = 25, pan = 0, amp = 1;
			var env, noise, sig, forward;
			env = EnvGen.ar(Env.perc(0.001, 0.00001 * LFNoise1.kr(4).abs, 1, -4), gate, doneAction: 0);
			noise = PinkNoise.ar(env);
			sig = BPF.ar(noise, centerFreq.max(1), rq, mul).fold(-1, 1);
			env = amp.curvelin(0, 1, 0, 1, log(10));
			forward = NN(msg[1].asSymbol, \forward).ar(sig * env, 2048);
			Out.ar(out, DirtPan.ar(forward, ~dirt.numChannels, pan));
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
		  Out.ar(out, DirtPan.ar(forward, ~dirt.numChannels, pan))
		}).add;

		SynthDef(\raveorb, { |out, amp = 1, n = 0, pan=0.5| // number
			var bus, sound, ravePitchGrain = 0.2, ravePitchDisperse = 0.003, ravePitchRatio = 1, ravePitchTimeDisperse = 0.003;

			bus = Select.kr(n, busses);
			sound = InFeedback.ar(bus, 1);
			sound = NN(msg[1].asSymbol, \forward).ar(sound, 2048);
			// sound = PitchShift.ar(sound, ravePitchGrain, ravePitchRatio, ravePitchDisperse, ravePitchTimeDisperse);

			Out.ar(out, DirtPan.ar(sound, ~dirt.numChannels, pan))
		}).add;

	}, '/switchSynthModel', nil, 57120);
)