(
	var irisPath = ~samplesPath++"/Iris\ 2\ Library/Samples/";
	"\n\nLoading Iris samples...".postln;
	~dirt.loadSoundFileFolder(irisPath++"Abstract/Drones", \irisdrone);
	~dirt.loadSoundFileFolder(irisPath++"Environments/Drones", \irisdrone, true);
	~dirt.loadSoundFileFolder(irisPath++"Synthesizers/Circuit\ Bent", \irisbent);
	~dirt.loadSoundFileFolder(irisPath++"Synthesizers/Arp 2500", \irisarp25);
	~dirt.loadSoundFileFolder(irisPath++"Synthesizers/Arp 2600", \irisarp26);
	~dirt.loadSoundFileFolder(irisPath++"Synthesizers/Nord", \irisnord);
	~dirt.loadSoundFileFolder(irisPath++"Voice/Vowels\ Female", \irisvowf);
)
