(
	var visAddr = NetAddr.new(~visAddr, 3334);
	"\n\nConnecting to Hydra".postln;
	// Hydra Forwarding
	OSCFunc({ |msg, time, tidalAddr|
		var latency = time - Main.elapsedTime;
		msg = msg ++ ["time", time, "latency", latency];
		// msg.postln;
		visAddr.sendBundle(latency, msg)
	}, '/play2').fix;
)
