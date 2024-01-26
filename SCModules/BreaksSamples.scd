(
	"\n\nLoading all the breaks...".postln;
	~dirt.loadSoundFileFolder(~samplesPath++"/Breaks/breaks", \breaks);
	~dirt.loadSoundFileFolder(~samplesPath++"/Breaks/electrobreaks", \breaks, true);
	~dirt.loadSoundFileFolder(~samplesPath++"/Breaks/funkybreaks", \breaks, true);
	~dirt.loadSoundFileFolder(~samplesPath++"/Breaks/oldschool", \breaks, true);
	~dirt.loadSoundFileFolder(~samplesPath++"/Breaks/vinylbreaks", \breaks, true);
	// ~dirt.loadSoundFileFolder(~samplesPath++"/Blue-Mar-Ten-Jungle/Breaks/", \mbreaks);
	~dirt.loadSoundFileFolder(~samplesPath++"/Breaks/classbreaks");
	~dirt.loadSoundFiles(~samplesPath++"/Breaks/dnb-breaks", \dnbbreaks);
)
