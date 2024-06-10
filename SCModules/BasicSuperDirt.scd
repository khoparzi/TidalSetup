(//
s.waitForBoot {
	~dirt = SuperDirt(s.options.numOutputBusChannels, s); // two output channels, increase if you want to pan across more channels
	~dirt.loadSoundFiles;   // load samples
	s.sync;
	switch(s.options.outDevice)
	{ "ES-9" } { ~dirt.start(57120, [0, 2, 4, 6, 8, 0, 2, 4, 6, 8]); }
	{ "JackBridge" } { ~dirt.start(57120, [0, 2, 4, 6, 0, 2, 4, 6, 0, 2, 4, 6]); }
	{ ~dirt.start(57120, 0 ! 12); };

	~dirt.soundLibrary.addSynth(\panic, (play: { ~dirt.orbits.do(_.freeSynths) }));
})
