(
	var akWFPath = "/Users/khoparzi/Music/Samples/AKWF/";
	"\n\nLoading single cycle waveforms for minimal glitchiness...".postln;
	~dirt.loadSoundFileFolder(~samplesPath++"/SCW/scw-one", \scw);
	~dirt.loadSoundFileFolder(~samplesPath++"/SCW/scw-one", \scwone);
	~dirt.loadSoundFileFolder(~samplesPath++"/SCW/scw-nasty", \scwnasty);
	~dirt.loadSoundFileFolder(~samplesPath++"/SCW/scw-two", \scwtwo);
	~dirt.loadSoundFileFolder(~samplesPath++"/SCW/scw-two", \scw, true);
	~dirt.loadSoundFileFolder(~samplesPath++"/SCW/scw-nasty", \scw, true);
	//
	~dirt.loadSoundFileFolder(akWFPath++"/AKWF_0001", \akwf);
	~dirt.loadSoundFileFolder(akWFPath++"/AKWF_0002", \akwf, true);
	~dirt.loadSoundFileFolder(akWFPath++"/AKWF_0003", \akwf, true);
	~dirt.loadSoundFileFolder(akWFPath++"/AKWF_0004", \akwf, true);
	~dirt.loadSoundFileFolder(akWFPath++"/AKWF_0005", \akwf, true);
	~dirt.loadSoundFileFolder(akWFPath++"/AKWF_0006", \akwf, true);
	~dirt.loadSoundFileFolder(akWFPath++"/AKWF_0007", \akwf, true);
	~dirt.loadSoundFileFolder(akWFPath++"/AKWF_0008", \akwf, true);
	~dirt.loadSoundFileFolder(akWFPath++"/AKWF_0009", \akwf, true);
	~dirt.loadSoundFileFolder(akWFPath++"/AKWF_0010", \akwf, true);
)
