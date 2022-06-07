(
	{
		if (~visAddr.isNil == false,
			{ "Visuals OSC to: ".post; ~visAddr; },
			{ currentEnvironment.put(\visAddr, "127.0.0.1"); }
		)
	}.defer;
	{var visAddr = NetAddr.new(~visAddr, 3333);
	"\n\nConnecting to Visuals".postln;
	// Based to Tadokoro san's approach here
	// https://club.tidalcycles.org/t/sending-osc-out-of-tidal-again-help/3318/10
	// Visuals Forwarding
	OSCdef(\tidalplay, {
		arg msg;
		visAddr.sendMsg(*msg);
		}, '/dirt/play', n);
	}.defer;
);
