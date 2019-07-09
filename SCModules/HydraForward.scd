(
	var addr = NetAddr.new("127.0.0.1", 3333);
	// Hydra Forwarding
	OSCFunc({ |msg, time, tidalAddr|
		var latency = time - Main.elapsedTime;
		msg = msg ++ ["time", time, "latency", latency];
		// msg.postln;
		addr.sendBundle(latency, msg)
	}, '/play2').fix;
)
