(// With minimal samples, four orbits
	currentEnvironment.put(\setupPath, "/Users/khoparzi/Dev/Live-Coding/setup/");
	currentEnvironment.put(\samplesPath, "/Users/khoparzi/Music/Samples/");
	currentEnvironment.put(\batteryPath, "/Users/Shared/Battery 4 Factory Library/Samples/Drums/");

	(~setupPath++"/SCModules/BasicSuperDirt.scd").load();
	// To send to another address
	// currentEnvironment.put(\visAddr, "khoparzi-jnano.local");

	OSCdef.new(\loadVis, {
		arg msg;
		msg[0].postln; msg[1].postln;
		(~setupPath++"/SCModules/HydraForward.scd").load();
		~visCAddr = NetAddr.new(~visAddr, 3335);
		}, '/loadVis', nil, 57120);

	OSCdef.new(\exit, {
		s.quit;
		}, '/exit', nil, 57120);
	OSCdef.new(\record, {
		s.record;
		}, '/record', nil, 57120);
	OSCdef.new(\stoprecord, {
		s.stopRecording;
		}, '/stoprecord', nil, 57120);

	// All the sample loading
	OSCdef.new(\loadSample, {
		arg msg;
		// msg[0] ++ ":" ++ msg[1].postln;
		case
			{msg[1].asString == "extras"} {
				"\n\nExtra superdirt".postln; ~dirt.loadSoundFiles(~samplesPath++"/extra-superdirt-samples/*");
			}
			{msg[1].asString == "smalldirt"} {
				"\n\nFiles in superdirt folder".postln;
				~dirt.loadSoundFiles(~samplesPath++"/smalldirt/*");
			}
			{msg[1].asString == "superdirt"} {
				"\n\nFiles in superdirt folder".postln;
				~dirt.loadSoundFiles(~samplesPath++"/superdirt/*");
			}
			{msg[1].asString == "oneshots"} {
				~dirt.loadSoundFileFolder(~samplesPath++"/superdirt/oneshots", \oneshots);
			}
			{msg[1].asString == "loops"} {
				"\n\nFiles in loops folder".postln;
				(~setupPath++"/SCModules/Loops.scd").load();
			}
			{msg[1].asString == "rach"} {
				"\n\nRachmiel loops".postln; ~dirt.loadSoundFileFolder(~samplesPath++"/Loops/rachMiel/", \rach);
			}
			{msg[1].asString == "breaks"} {
				(~setupPath++"/SCModules/BreaksSamples.scd").load();
			}
			{msg[1].asString == "scw"} {
				(~setupPath++"/SCModules/SingleCycleWaveforms.scd").load();
			}
			{msg[1].asString == "afro"} {
				(~setupPath++"/SCModules/Afro.scd").load();
			}
			{msg[1].asString == "voxs"} {
				(~setupPath++"/SCModules/Voxs.scd").load();
			}
			{msg[1].asString == "maschine"} {
				(~setupPath++"/SCModules/MaschineSamples.scd").load();
			}
			{msg[1].asString == "freemaschine"} {
				// To free samples from Maschine
				~dirt.freeSoundFiles([\abst, \buzzcut, \waxdoc, \allme, \aye, \backwhen, \battleg, \beatboard, \beben, \calizone, \charge, \conant, \dubr, \ghettot, \imperial, \jukeit, \realblue])
			}
			{msg[1].asString == "collab"} {
				"\n\nFiles for collabs".postln;
				~dirt.loadSoundFiles(~samplesPath++"/collab/*");
			}
			{msg[1].asString == "algomech"} {
				"\n\nFiles for Algomech".postln;
				~dirt.loadSoundFiles(~samplesPath++"/Algomech/*");
			}
			{msg[1].asString == "midiin"} {
				(~setupPath++"/SCModules/MidiIn.scd").load();
			}
			{msg[1].asString == "midiout"} {
				(~setupPath++"/SCModules/MidiOut.scd").load();
			}
			{msg[1].asString == "synths"} {
				"\n\nDiego's synths".postln;
				"/Users/khoparzi/Dev/Live-Coding/diegodorado-tidal-synths/loadAll.scd".load();
				(~setupPath++"/SCModules/synthDef.scd").load();
				currentEnvironment.put(\renickPath, "/Users/khoparzi/Dev/Live-Coding/tidal-playground/synths/");
				(~setupPath++"/SCModules/RenickSynths.scd").load();
			};
		}, '/loadSample', nil, 57120);
		OSCdef.new(\loadFolders, {
			arg msg;
			~dirt.loadSoundFiles(msg[1].asString);
		}, '/loadFolders', nil, 57120);
		OSCdef.new(\freeSample, {
			arg msg;
			~dirt.freeSoundFiles([msg[1].asSymbol]);
			}, '/freeSample', nil, 57120);
	)
