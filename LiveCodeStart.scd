Esp.start;
Esp.person;
TempoClock.default.pause;

OSCFunc.trace(true, true);

OSCFunc.trace(false, false);

s.scope;
s.freqscope;

// GUI To make recordings
s.makeWindow;

// Or through commands
s.record;
s.stopRecording;

~dirt.stop();

s.options.outDevice = "Soundflower (64ch)";
s.options.outDevice = "DisplayPort";

s.options.outDevice.postln;

~dirt.postSampleInfo;

// With minimal samples, four orbits
(
	currentEnvironment.put(\setupPath, "/Users/khoparzi/Dev/Live-Coding/setup/");
	currentEnvironment.put(\samplesPath, "/Users/khoparzi/Music/Samples/");
	// Hydra Forwarding
	//
	s.waitForBoot {
		~dirt = SuperDirt(2, s); // two output channels, increase if you want to pan across more channels
		~dirt.loadSoundFiles;   // load samples
		s.sync;
		// if (s.options.outDevice == "JackBridge") {
			// If we want orbits to go to different channels for eg for JackBridge
	    	// ~dirt.start(57120, [0, 2, 4, 6]);
		// }
		// if (s.options.outDevice != "JackBridge") {
	    	~dirt.start(57120, [0, 0, 0, 0]);
		// }
	}
)

(
	// (~setupPath++"/SCModules/HydraForward.scd").load();
	// (~setupPath++"/SCModules/MidiIn.scd").load();
	// (~setupPath++"/SCModules/MidiOut.scd").load();

	(~setupPath++"SCModules/SingleCycleWaveforms.scd").load();

	(~setupPath++"/SCModules/BreaksSamples.scd").load();

	(~setupPath++"/SCModules/Afro.scd").load();

	(~setupPath++"/SCModules/MaschineSamples.scd").load();

	"/Users/khoparzi/Dev/Live-Coding/diegodorado-tidal-synths/loadAll.scd".load();

	(~setupPath++"/SCModules/synthDefs.scd").load();

	~dirt.loadSoundFiles(samplesPath++"/extra-superdirt-samples/*");

	// Personal superdirt samples
	// ~dirt.loadSoundFileFolder(samplesPath++"/superdirt/oneshots", \oneshots);
	// ~dirt.loadSoundFiles(samplesPath++"/superdirt/*");
)


~dirt.freeSoundFiles([\abst, \buzzcut, \waxdoc, \allme, \aye, \backwhen, \battleg, \beatboard, \beben, \calizone, \charge, \conant, \imperial])

// List extra synths
SynthDescLib.global.synthDescs.keys
.reject{ |synth| synth.asString.beginsWith("system_").or(synth.asString.beginsWith("dirt_")) }
.reject{ |synth| synth.asString.beginsWith("system_").or(synth.asString.beginsWith("super")) }
.asList
.collect(_.asSymbol)

~dirt.stop()
