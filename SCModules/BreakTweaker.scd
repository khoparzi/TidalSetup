(
	var breakTweakPath = ~samplesPath++"/BreakTweaker\ Library/Samples/";
	"\n\nLoading BreakTweaker samples...".postln;
	~dirt.loadSoundFileFolder(breakTweakPath++"1-Kick/Minimal", \btkick);
	~dirt.loadSoundFileFolder(breakTweakPath++"1-Kick/Dubstep", \btdubkick);
	// ~dirt.loadSoundFileFolder(breakTweakPath++"1-Kick/Dubstep", \btdubkick, true);
	~dirt.loadSoundFileFolder(breakTweakPath++"2-Snare/Minimal", \btsnr);
	~dirt.loadSoundFileFolder(breakTweakPath++"2-Snare/Dubstep", \btdubsnr);
	// hats
	~dirt.loadSoundFileFolder(breakTweakPath++"4-HiHat/Basics", \bthat);
	~dirt.loadSoundFileFolder(breakTweakPath++"4-HiHat/Electro", \bthat, true);
	~dirt.loadSoundFileFolder(breakTweakPath++"4-HiHat/Minimal", \bthat, true);
	~dirt.loadSoundFileFolder(breakTweakPath++"4-HiHat/Vintage", \bthat, true);
	// Basses
	~dirt.loadSoundFileFolder(breakTweakPath++"12-Synth/Bass", \btbass);
	~dirt.loadSoundFileFolder(breakTweakPath++"12-Synth/Chord", \btchord);
	~dirt.loadSoundFileFolder(breakTweakPath++"9-Glitch", \btglitch);
)
