Esp.start;
Esp.person;
TempoClock.default.pause;

OSCFunc.trace(true, true);

OSCFunc.trace(false, false);

Server.local.options.device = nil;
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

// With minimal samples, four orbits and midi
(
	s.waitForBoot {
		~dirt = SuperDirt(2, s); // two output channels, increase if you want to pan across more channels
		~dirt.loadSoundFiles;   // load samples
		~dirt.loadSoundFiles("/Users/khoparzi/Music/Samples/superdirt/scw-one");
		~dirt.loadSoundFiles("/Users/khoparzi/Music/Samples/superdirt/scw-two");
		s.sync;
		~dirt.start(57120, [0,0,0,0]);

		MIDIClient.init;
		MIDIClient.list;
		~midiOut = MIDIOut.newByName("IAC Driver", "Bus 1");
		~dirt.soundLibrary.addMIDI(\iac, ~midiOut);
		~midiOut = MIDIOut.newByName("Network", "Session 1");
		~dirt.soundLibrary.addMIDI(\net, ~midiOut);
	}
)

// With all samples, midi and 4 stereo outs
(
	var on, off, cc;
	var osc;
	// Hydra Forwarding
	var addr = NetAddr.new("127.0.0.1", 3334);
	s.waitForBoot {
		~dirt = SuperDirt(2, s); // two output channels, increase if you want to pan across more channels
		~dirt.loadSoundFiles;   // load samples (path containing a wildcard can be passed in)
		~dirt.loadSoundFiles("/Users/khoparzi/Music/Samples/superdirt/*");
		~dirt.loadSoundFiles("/Users/khoparzi/Music/Samples/extra-superdirt-samples/*");
		// for example: ~dirt.loadSoundFiles("/Users/myUserName/Dirt/samples/*");
		s.sync; // wait for samples to be read
		// ~dirt.start(57120, [0, 0, 0, 0]);   // start listening on port 57120, create two orbits, each sending audio to channel 0. You can direct sounds to the orbits from tidal e.g. by: `# orbit "0 1 1"

		// if (s.options.outDevice == "JackBridge") {
	    	// ~dirt.start(57120, [0, 2, 4, 6]);
		// }
		// if (s.options.outDevice != "JackBridge") {
	    	~dirt.start(57120, [0, 0, 0, 0]);
		// }

		MIDIClient.init;
		MIDIClient.list;

		~midiOut = MIDIOut.newByName("IAC Driver", "Bus 1");
		~dirt.soundLibrary.addMIDI(\iac, ~midiOut);
		~midiOut = MIDIOut.newByName("Network", "Session 1");
		~dirt.soundLibrary.addMIDI(\net, ~midiOut);

		//
		"/Users/khoparzi/Dev/Live-Coding/diegodorado-tidal-synths/loadAll.scd".load();
		"/Users/khoparzi/Dev/Live-Coding/livecode-this/synths/tidalInstruments.scd".load();
		"/Users/khoparzi/Dev/Live-Coding/tidal_midi_harmony/e_fairbanks.scd".load();

		osc = NetAddr.new("127.0.0.1", 6010);

		MIDIIn.connectAll;

		// ~notes = [];
		// MIDIFunc.noteOn({ |veloc, num, chan, src|
		// 	~notes = ~notes.add(num);
		// 	~notes = ~notes.sort({|a,b| a>b}).reverse;
		// 	osc.sendMsg('/ctrl', "notes", format("%", ~notes-60));
		// 	~notes.postln;
		// });
		// MIDIFunc.noteOff({ |veloc, num, chan, src|
		// 	~notes = ~notes.takeThese({|x| x==num});
		// 	osc.sendMsg('/ctrl', "notes", format("%", ~notes-60));
		// 	~notes.postln;
		// });

		on = MIDIFunc.noteOn({ |val, num, chan, src|
			osc.sendMsg("/ctrl", num.asString, val/127);
		});

		off = MIDIFunc.noteOff({ |val, num, chan, src|
			osc.sendMsg("/ctrl", num.asString, 0);
		});

		cc = MIDIFunc.cc({ |val, num, chan, src|
			osc.sendMsg("/ctrl", num.asString, val/127);
		});

		if (~stopMidiToOsc != nil, {
			~stopMidiToOsc.value;
		});

		~stopMidiToOsc = {
			on.free;
			off.free;
			cc.free;
		};

		// Hydra Forwarding
		OSCFunc({ |msg, time, tidalAddr|
			var latency = time - Main.elapsedTime;
			msg = msg ++ ["time", time, "latency", latency];
			// msg.postln;
			addr.sendBundle(latency, msg)
		}, '/play2').fix;

	}
)

// With minimal samples and four orbits
(
	s.waitForBoot {
		~dirt = SuperDirt(2, s); // two output channels, increase if you want to pan across more channels
		~dirt.loadSoundFiles;   // load samples
		s.sync;
		~dirt.start(57120, [0,0,0,0]);
	}
)

// List extra synths
SynthDescLib.global.synthDescs.keys
.reject{ |synth| synth.asString.beginsWith("system_").or(synth.asString.beginsWith("dirt_")) }
// .reject{ |synth| synth.asString.beginsWith("system_").or(synth.asString.beginsWith("super")) }
.asList
.collect(_.asSymbol)

~dirt.stop()
