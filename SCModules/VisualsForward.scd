(
	{
		if (~visAddr.isNil == false,
			{ "Visuals OSC to: ".post; ~visAddr; },
			{ "Visuals to localhost: ".post; currentEnvironment.put(\visAddr, "127.0.0.1"); }
		)
	}.defer;
	{var visAddr = NetAddr.new(~visAddr, 41234);
	"\n\nConnecting to Visuals".postln;
	// Hydra Forwarding
	OSCFunc({ |msg, time, tidalAddr|
		var latency = time - Main.elapsedTime;
		msg = msg ++ ["time", time, "latency", latency];
		msg.postln;
		visAddr.sendBundle(latency, msg)
	}, '/dirt/play').fix;}.defer(1);

	SynthDef(\dummy, { |out, sustain = 1, speed = 1, begin = 0, end = 1, pan, accelerate, offset, rate, phase|
		var sound;
		sound = DC.ar(0);
		OffsetOut.ar(out,
			DirtPan.ar(sound, ~dirt.numChannels, pan)
		)
	}).add;
)
