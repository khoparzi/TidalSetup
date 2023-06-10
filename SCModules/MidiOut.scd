(
	"\n\nConnecting to midi out...".postln;
	MIDIClient.init;
	MIDIClient.list;
	~midiOut = MIDIOut.newByName("IAC Driver", "Bus 1");
	~dirt.soundLibrary.addMIDI(\iac, ~midiOut);
	~midiOut = MIDIOut.newByName("Sonic Potions USB MIDI", "Sonic Potions USB MIDI");
	~dirt.soundLibrary.addMIDI(\lxr, ~midiOut);
	// ~midiOut = MIDIOut.newByName("Launchpad Pro 8", "MIDI Port");
	// ~dirt.soundLibrary.addMIDI(\lppro, ~midiOut);
	// ~midiOut = MIDIOut.newByName("Network", "Session 1");
	// ~dirt.soundLibrary.addMIDI(\net, ~midiOut);
	~midiOut.latency = 0.01;
	OSCdef.new(\iacLatency, {
		arg msg;
		"IAC Bus latency set to: ".post; msg[1].asString.post;
		~midiOut.latency = msg[1].asString;
		})
)
