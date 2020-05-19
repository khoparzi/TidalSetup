(
	{
		if (~visAddr.isNil == false,
			{ "Visuals OSC to: ".post; ~visAddr; },
			{ currentEnvironment.put(\visAddr, "127.0.0.1"); }
		)
	}.defer;
	{var visAddr = NetAddr.new(~visAddr, 3334);
	"\n\nConnecting to Hydra".postln;
	// Hydra Forwarding
	OSCFunc({ |msg, time, tidalAddr|
		var latency = time - Main.elapsedTime;
		msg = msg ++ ["time", time, "latency", latency];
		// msg.postln;
		visAddr.sendBundle(latency, msg)
	}, '/play2').fix;}.defer(1);

	SynthDef(\dummy, { |out, sustain = 1, speed = 1, begin = 0, end = 1, pan, accelerate, offset|
		var env, sound, rate, phase;
		phase = Line.ar(begin, end, sustain);
		rate = (begin + 1) * (speed + Sweep.kr(1, accelerate));
		sound = DC.ar(0);
		OffsetOut.ar(out,
			DirtPan.ar(sound, ~dirt.numChannels, pan)
		)
	}).add;
)
