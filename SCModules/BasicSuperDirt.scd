(//
s.waitForBoot {
	~dirt = SuperDirt(s.options.numOutputBusChannels, s); // two output channels, increase if you want to pan across more channels
	~dirt.loadSoundFiles;   // load samples
	s.sync;
	switch(s.options.outDevice)
	{ "ES-9" } { ~dirt.start(57120, [0, 2, 4, 6, 8, 0, 2, 4, 6, 8]); }
	{ "JackBridge" } { ~dirt.start(57120, [0, 2, 4, 6, 0, 2, 4, 6, 0, 2, 4, 6]); }
	//{ ~dirt.start(57120, 0 ! 12); };
	{ ~dirt.start(57120, [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 3]); };

	~dirt.soundLibrary.addSynth(\panic, (play: { ~dirt.orbits.do(_.freeSynths) }));
	// optional, needed for convenient access from sclang:
	~d1 = ~dirt.orbits[0]; ~d2 = ~dirt.orbits[1]; ~d3 = ~dirt.orbits[2];
	~d4 = ~dirt.orbits[3]; ~d5 = ~dirt.orbits[4]; ~d6 = ~dirt.orbits[5];
	~d7 = ~dirt.orbits[6]; ~d8 = ~dirt.orbits[7]; ~d9 = ~dirt.orbits[8];
	~d10 = ~dirt.orbits[9]; ~d11 = ~dirt.orbits[10]; ~d12 = ~dirt.orbits[11];
})
