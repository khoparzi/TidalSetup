(
	"\n\nLoading all the breaks...".postln;
	~dirt.loadSoundFileFolder("/Users/khoparzi/Music/Samples/superdirt/breaks", \breaks);
	~dirt.loadSoundFileFolder("/Users/khoparzi/Music/Samples/superdirt/electrobreaks", \breaks, true);
	~dirt.loadSoundFileFolder("/Users/khoparzi/Music/Samples/superdirt/funkybreaks", \breaks, true);
	~dirt.loadSoundFileFolder("/Users/khoparzi/Music/Samples/superdirt/oldschool", \breaks, true);
	~dirt.loadSoundFileFolder("/Users/khoparzi/Music/Samples/superdirt/vinylbreaks", \breaks, true);
	~dirt.loadSoundFileFolder(~samplesPath++"/Blue-Mar-Ten-Jungle/Breaks/", \marten-breaks);
)
