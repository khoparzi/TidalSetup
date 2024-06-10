(
	"\n\nConnecting to launchpad out...".postln;
	MIDIClient.init;
	MIDIClient.list;
	~launchOut = MIDIOut.newByName("Launchpad Pro 8", "MIDI Port");
	~dirt.soundLibrary.addMIDI(\lppro, ~launchOut);
)