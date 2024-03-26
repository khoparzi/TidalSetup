(
	"\n\nConnecting to LXR...".postln;
	MIDIClient.init;
	MIDIClient.list;
	~lxrOut = MIDIOut.newByName("Sonic Potions USB MIDI", "Sonic Potions USB MIDI");
	~dirt.soundLibrary.addMIDI(\lxr, ~lxrOut);
)