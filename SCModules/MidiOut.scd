(
	MIDIClient.init;
	MIDIClient.list;
	~midiOut = MIDIOut.newByName("IAC Driver", "Bus 1");
	~dirt.soundLibrary.addMIDI(\iac, ~midiOut);
	~midiOut = MIDIOut.newByName("Network", "Session 1");
	~dirt.soundLibrary.addMIDI(\net, ~midiOut);
)
