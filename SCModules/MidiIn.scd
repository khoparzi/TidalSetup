// From Eric Fairbanks

(
	// here's the bit that sends MIDI data from all controllers to TidalCycles via OSC
	"\n\nMidi in is enabled...".postln;
	MIDIClient.init;
	MIDIIn.connectAll;
	~tidalSocket = NetAddr("127.0.0.1", 6010);
	~notes=[];
	MIDIFunc.cc({|val, num, chan, src| ~tidalSocket.sendMsg('/ctrl', num, val/127.0);}, nil, nil, nil);
	MIDIFunc.noteOn({|veloc, num, chan, src| ~notes=~notes.add(num); ~notes=~notes.sort({|a,b| a>b}).reverse; ~tidalSocket.sendMsg('/ctrl', "notes", format("%", ~notes-60)); ~notes.postln;});
	MIDIFunc.noteOff({|veloc, num, chan, src| ~notes=~notes.takeThese({|x| x==num}); ~tidalSocket.sendMsg('/ctrl', "notes", format("%", ~notes-60)); ~notes.postln;});
	if (~stopMidiToOsc != nil, {
		~stopMidiToOsc.value;
	});

	~stopMidiToOsc = {
		on.free;
		off.free;
		cc.free;
	};
)
