(
	"\n\nLoading all the breaks...".postln;
	~dirt.loadSoundFileFolder(~samplesPath++"/Breaks/breaks", \breaks);
	~dirt.loadSoundFileFolder(~samplesPath++"/Breaks/electrobreaks", \breaks, true);
	~dirt.loadSoundFileFolder(~samplesPath++"/Breaks/funkybreaks", \breaks, true);
	~dirt.loadSoundFileFolder(~samplesPath++"/Breaks/oldschool", \breaks, true);
	~dirt.loadSoundFileFolder(~samplesPath++"/Breaks/vinylbreaks", \breaks, true);
	~dirt.loadSoundFiles(~samplesPath++"/Breaks/classicbreaks/*");
	// ~dirt.loadSoundFileFolder(~samplesPath++"/Blue-Mar-Ten-Jungle/Breaks/", \mbreaks);
)
