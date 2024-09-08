(
	OSCdef.new(\loadVis, {
		arg msg;
		msg[0].postln; msg[1].postln;
		(~setupPath++"/SCModules/VisualsForward.scd").load();
		// (~setupPath++"/SCModules/TidalVis.scd").load();
		//~visCAddr = NetAddr.new(~visAddr, 3335);
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
			{msg[1].asString == "khoparzi"} {
				"\n\nMy custom samples folder".postln;
				~dirt.loadSoundFiles(~samplesPath++"/smalldirt/*");
			}
			{msg[1].asString == "smalldirt"} {
				"\n\nMy custom samples folder".postln;
				~dirt.loadSoundFiles(~samplesPath++"/smalldirt/*");
			}
			{msg[1].asString == "batteryBass"} {
				"\n\nSamples from battery folder".postln;
				~dirt.loadSoundFiles(~batteryPath++"One Shots/Synth Note/*");
			}
			{msg[1].asString == "superdirt"} {
				"\n\nSamples in superdirt folder".postln;
				~dirt.loadSoundFiles(~samplesPath++"/superdirt/*");
			}
			{msg[1].asString == "cyberbass"} {
				"\n\nSamples in cyberbass folder".postln;
				~dirt.loadSoundFiles(~samplesPath++"/cyberbass/*");
			}
			{msg[1].asString == "oneshots"} {
				~dirt.loadSoundFileFolder(~samplesPath++"/superdirt/oneshots", \oneshots);
			}
			{msg[1].asString == "loops"} {
				"\n\nSamples in loops folder".postln;
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
				"\n\nSamples from collabs".postln;
				~dirt.loadSoundFiles(~samplesPath++"/collab/*");
			}
			{msg[1].asString == "cringe"} {
				"\n\nSamples for cringing it out".postln;
				~dirt.loadSoundFiles(~samplesPath++"/cringe/*");
			}
			{msg[1].asString == "rave"} {
				{
					(~setupPath++"/SCModules/RAVE.scd").load();
					0.2.wait;
					(~setupPath++"/SCModules/RAVE/RAVE-FX.scd").load();
					(~setupPath++"/SCModules/RAVE/RAVE-Bus.scd").load();
					(~setupPath++"/SCModules/RAVE/RAVE-Synth.scd").load();
				}.fork;
			}
			{msg[1].asString == "ignoto"} {
				"\n\n\nSamples for ignoto".postln;
				~dirt.loadSoundFiles(~samplesPath++"/ignoto/*");
			}
			{msg[1].asString == "supercontinent"} {
				"\n\nSamples for SuperContinent".postln;
				~dirt.loadSoundFiles(~samplesPath++"/supercontinent/*");
			}
			{msg[1].asString == "midiin"} {
				(~setupPath++"/SCModules/MidiIn.scd").load();
			}
			{msg[1].asString == "vstsynths"} {
				(~setupPath++"/ExternalModules/TidalVST/TidalVST.scd").load();
			}
			{msg[1].asString == "midiout"} {
				(~setupPath++"/SCModules/MidiOut.scd").load();
			}
			{msg[1].asString == "midilxr"} {
				(~setupPath++"/SCModules/LXR-02Midi.scd").load();
			}
			{msg[1].asString == "midilaunchpad"} {
				(~setupPath++"/SCModules/LaunchpadOut.scd").load();
			}
			{msg[1].asString == "linuxmidi"} {
				MIDIClient.init;
				MIDIClient.list;
				~midiOut = MIDIOut.newByName("Midi Through", "Midi Through Port-0");
				~dirt.soundLibrary.addMIDI(\nixmidi, ~midiOut);
			}
			{msg[1].asString == "synths"} {
				(~setupPath++"/SCModules/synthDef.scd").load();
			}
			{msg[1].asString == "misynths"} {
				(~setupPath++"/SCModules/mi-synthdef.scd").load();
			}
			{msg[1].asString == "isynths"} {
				(~setupPath++"/SCModules/IkedaSynths.scd").load();
			}
			{msg[1].asString == "ksynths"} {
				(~setupPath++"/SCModules/KeysSynths.scd").load();
			}
			{msg[1].asString == "dsynths"} {
				"\n\nDiego's synths".postln;
				~setupPath++"/ExternalModules/diegodorado-tidal-synths/loadAll.scd".load();
			}
			{msg[1].asString == "rsynths"} {
				currentEnvironment.put(\renickPath, ~setupPath++"/ExternalModules/tidal-playground/synths/");
				(~setupPath++"/SCModules/RenickSynths.scd").load();
			}
			{msg[1].asString == "lsynths"} {
				(~setupPath++"/ExternalModules/loopier-synthdefs/synthdef-superfm.scd").load();
			}
			{msg[1].asString == "drum-machines"} {
	    // add this to your superdirt startup
				~drumMachinesDir = Quarks.all.detect({ |x| x.name == "tidal-drum-machines" }).localPath;
				~dirt.loadSoundFiles(~drumMachinesDir +/+ "machines" +/+ "*" +/+ "*", namingFunction: { |x| x.basename.replace("-","")});
			}
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
			{
				SynthDescLib.global.read;
				0.2.wait;
				SynthDescLib.global.browse;
			}.fork;
			}, '/showsynths', nil, 57120);
		OSCdef.new(\sampleInfo, {
			arg msg;
			~dirt.postSampleInfo;
			}, '/sampleInfo', nil, 57120);
		OSCdef.new(\showlevels, {
			arg msg;
			var w, spec;
			spec = \db.asSpec;
			w = Window("orbital volume", Rect(240, 240, 300, 500)).front;
			w.view.layout = HLayout(*~dirt.orbits.collect { |x|
				var val = spec.unmap(x.get(\amp).ampdb);
				Slider().value_(val).action_({ |v| x.set(\amp, spec.map(v.value).dbamp.postln) })
			});
			"\n\nShow levels now".postln;
			}, '/showlevels', nil, 57120);
		OSCdef.new(\nosidechain, {
			arg msg;
			~dirt.orbits[0].outBus = 0;
			~bus.free;
			"\n\nNo Sidechain now".postln;
			}, '/nosidechain', nil, 57120);
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
		OSCdef.new(\setsidechain, {
			arg msg;
			~bus = Bus.audio(s, numChannels:2);
			{
				if (msg.isNil == false,
					{ ~scChannel = msg },
					{ ~scChannel = 0; }
				)
			}.defer;
			}, '/setsidechain', nil, 57120);
	)
