// With minimal samples, four orbits
(
	currentEnvironment.put(\setupPath, "/Users/khoparzi/Dev/Live-Coding/setup/");
	currentEnvironment.put(\samplesPath, "/Users/khoparzi/Music/Samples/");
	currentEnvironment.put(\batteryPath, "/Users/Shared/Battery 4 Factory Library/Samples/Drums/");
	// currentEnvironment.put(\visAddr, "127.0.0.1");
	currentEnvironment.put(\visAddr, "khoparzi-jnano.local");
   (~setupPath++"/SCModules/HydraForward.scd").load();
   ~visCAddr = NetAddr.new(~visAddr, 3335);
	//
	s.waitForBoot {
		~dirt = SuperDirt(2, s); // two output channels, increase if you want to pan across more channels
		~dirt.loadSoundFiles;   // load samples
		s.sync;
		if (s.options.outDevice == "JackBridge",
			// If we want orbits to go to different channels for eg for JackBridge
			{~dirt.start(57120, [0, 2, 4, 6, 0, 2, 4, 6, 0, 2, 4, 6]);},
			{~dirt.start(57120, 0 ! 12);}
		);
	}
)

// All the sample loading
(
	// "\n\nExtra superdirt".postln; ~dirt.loadSoundFiles(~samplesPath++"/extra-superdirt-samples/*");

	// Personal superdirt samples
	~dirt.loadSoundFileFolder(~samplesPath++"/superdirt/oneshots", \oneshots);

	// "\n\nFiles in superdirt folder".postln; ~dirt.loadSoundFiles(~samplesPath++"/superdirt/*");

	"\n\nFiles in superdirt folder".postln; ~dirt.loadSoundFiles(~samplesPath++"/smalldirt/*");

	// "\n\nRachmiel loops".postln; ~dirt.loadSoundFileFolder(~samplesPath++"/Loops/rachMiel/", \rach);

	// (~setupPath++"/SCModules/Loops.scd").load();

	// ~visCAddr.sendMsg("/fullscreen", true);
	// ~visCAddr.sendMsg("/ui", true);
	// ~visCAddr.sendMsg("/beat", true);
	~visCAddr.sendMsg("/zmin", 1000);
	// ~visCAddr.sendMsg("/zmax", 100);
	~visCAddr.sendMsg("/bloom", true);
	// ~visCAddr.sendMsg("/pixelate", true);
	~visCAddr.sendMsg("/kaleid", true);
	~visCAddr.sendMsg("/godray", true);
	// (~setupPath++"/SCModules/MidiIn.scd").load();
	(~setupPath++"/SCModules/MidiOut.scd").load();

	(~setupPath++"/SCModules/SingleCycleWaveforms.scd").load();

	(~setupPath++"/SCModules/BreaksSamples.scd").load();

	(~setupPath++"/SCModules/Afro.scd").load();

	(~setupPath++"/SCModules/Voxs.scd").load();

	(~setupPath++"/SCModules/MaschineSamples.scd").load();

	"\n\nDiego's synths".postln;
	"/Users/khoparzi/Dev/Live-Coding/diegodorado-tidal-synths/loadAll.scd".load();

	(~setupPath++"/SCModules/synthDef.scd").load();
	currentEnvironment.put(\renickPath, "/Users/khoparzi/Dev/Live-Coding/tidal-playground/synths/");
	(~setupPath++"/SCModules/RenickSynths.scd").load();
)


	// Personal superdirt samples
	// ~dirt.loadSoundFileFolder(~samplesPath++"/superdirt/oneshots", \oneshots);
	// ~dirt.loadSoundFiles(~samplesPath++"/superdirt/*");
)

// To free sample from Maschine
~dirt.freeSoundFiles([\abst, \buzzcut, \waxdoc, \allme, \aye, \backwhen, \battleg, \beatboard, \beben, \calizone, \charge, \conant, \imperial])

// To use ESP
Esp.start;
Esp.person;
TempoClock.default.pause;

// To see all incoming osc
OSCFunc.trace(true, true);

// Stop seeing
OSCFunc.trace(false, false);

// Get some scopes going
s.scope;
s.freqscope;
s.meter;

// GUI To make recordings
s.makeWindow;

// Or through commands
s.record;
s.stopRecording;

// Stop superdirt
~dirt.stop();

// Find out currently used interlace
s.options.outDevice.postln;

// List all loaded samples
~dirt.postSampleInfo;

// List extra synths
SynthDescLib.global.synthDescs.keys
.reject{ |synth| synth.asString.beginsWith("system_").or(synth.asString.beginsWith("dirt_")) }
.reject{ |synth| synth.asString.beginsWith("system_").or(synth.asString.beginsWith("super")) }
.asList
.collect(_.asSymbol)

~dirt.stop()
