(//
s.waitForBoot {
	~dirt = SuperDirt(s.options.numOutputBusChannels, s); // two output channels, increase if you want to pan across more channels
	~dirt.loadSoundFiles;   // load samples
	s.sync;
	if ((s.options.outDevice == "BlackHole" || s.options.outDevice == "JackBridge"),
		// If we want orbits to go to different channels for eg for JackBridge
		{~dirt.start(57120, [0, 2, 4, 6, 0, 2, 4, 6, 0, 2, 4, 6]);},
		{~dirt.start(57120, 0 ! 12);}
	);
})
