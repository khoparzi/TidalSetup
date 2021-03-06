(// With minimal samples, four orbits
	s.options.numOutputBusChannels = 2;
	currentEnvironment.put(\setupPath, "/Users/khoparzi/Dev/TidalSetup/");
	currentEnvironment.put(\samplesPath, "/Users/khoparzi/Music/Samples/");
	currentEnvironment.put(\batteryPath, "/Users/Shared/Battery 4 Factory Library/Samples/Drums/");

	(~setupPath++"/SCModules/BasicSuperDirt.scd").load();
	// To send to another address
	// currentEnvironment.put(\visAddr, "khoparzi-jnano.local");

	OSCdef.new(\loadVis, {
		arg msg;
		msg[0].postln; msg[1].postln;
		(~setupPath++"/SCModules/VisualsForward.scd").load();
		~visCAddr = NetAddr.new(~visAddr, 3335);
		}, '/loadVis', nil, 57120);

	OSCdef.new(\exit, {
		s.quit;
		}, '/exit', nil, 57120);
	OSCdef.new(\reboot, {
		s.reboot;
		}, '/reboot', nil, 57120);
	OSCdef.new(\record, {
		s.record;
		}, '/record', nil, 57120);
	OSCdef.new(\stoprecord, {
		s.stopRecording;
		}, '/stoprecord', nil, 57120);

	// All the sample loading
	OSCdef.new(\loadBank, {
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
			{msg[1].asString == "bt"} {
				(~setupPath++"/SCModules/BreakTweaker.scd").load();
			}
			{msg[1].asString == "iris"} {
				(~setupPath++"/SCModules/Iris.scd").load();
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
			{msg[1].asString == "ignoto"} {
				"\n\nFiles for ignoto".postln;
				~dirt.loadSoundFiles(~samplesPath++"/ignoto/*");
			}
			{msg[1].asString == "supercontinent"} {
				"\n\nFiles for SuperContinent".postln;
				~dirt.loadSoundFiles(~samplesPath++"/supercontinent/*");
			}
			{msg[1].asString == "midiin"} {
				(~setupPath++"/SCModules/MidiIn.scd").load();
			}
			{msg[1].asString == "midiout"} {
				(~setupPath++"/SCModules/MidiOut.scd").load();
			}
			{msg[1].asString == "linuxmidi"} {
				MIDIClient.init;
				MIDIClient.list;
				~midiOut = MIDIOut.newByName("Midi Through", "Midi Through Port-0");
				~dirt.soundLibrary.addMIDI(\nixmidi, ~midiOut);
			}
			{msg[1].asString == "synths"} {
				(~setupPath++"/SCModules/synthDef.scd").load();
				"\n\nDiego's synths".postln;
				"/Users/khoparzi/Dev/Live-Coding/diegodorado-tidal-synths/loadAll.scd".load();
				currentEnvironment.put(\renickPath, "/Users/khoparzi/Dev/Live-Coding/tidal-playground/synths/");
				(~setupPath++"/SCModules/RenickSynths.scd").load();
			};
		}, '/loadBank', nil, 57120);
		OSCdef.new(\loadFolders, {
			arg msg;
			~dirt.loadSoundFiles(msg[1].asString);
		}, '/loadFolders', nil, 57120);
		OSCdef.new(\loadFolder, {
			arg msg;
			var folderKey = msg[1].asString().split($\:);
			if (folderKey.size>1, {
				~dirt.loadSoundFileFolder(folderKey[0], folderKey[1].asSymbol);
				},
				{
				~dirt.loadSoundFileFolder(folderKey[0], folderKey.asSymbol);
			});
		}, '/loadFolder', nil, 57120);
		OSCdef.new(\freeBank, {
			arg msg;
			~dirt.freeSoundFiles([msg[1].asSymbol]);
			}, '/freeBank', nil, 57120);
		OSCdef.new(\startrms, {
			arg msg;
			// For feedforward
			~dirt.startSendRMS;
			}, '/startrms', nil, 57120);
		OSCdef.new(\openscope, {
			arg msg;
			s.scope;
			s.freqscope;
			}, '/openscope', nil, 57120);
		OSCdef.new(\showsynths, {
			arg msg;
			// Show synths
			SynthDescLib.global.read;
			SynthDescLib.global.browse;
			}, '/showsynths', nil, 57120);
		OSCdef.new(\nosidechain, {
			arg msg;
			~dirt.orbits[0].outBus = 0;
			~bus.free;
			"\n\nNo Sidechain now".postln;
			}, '/noside', nil, 57120);
		OSCdef.new(\sidechained, {
			arg msg;
			// Sidechain d1 with d2
			~bus = Bus.audio(s, numChannels:2);
			~dirt.orbits[0].outBus = ~bus;
			(
				Ndef(\d1side, {
					var control = InBus.ar(~dirt.orbits[1].dryBus, 2).sum;
					var dirt = InBus.ar(~bus, 2);
					Compander.ar(dirt, control, thresh: 0.001, slopeBelow: 1.0,
						slopeAbove: 0.651, clampTime: 0.05, relaxTime: 0.1)
					}).play;
				"\n\nd1 is now sidechained by d2".postln;
				)
			}, '/sidechained', nil, 57120);
	)
