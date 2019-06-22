// From Eric Fairbanks
// http://ericfairbanks.org

(SynthDef(\donk, {
  |out, sustain = 1, freq = 440, speed = 1, begin=0, end=1, pan, accelerate, offset|
  var line = Line.ar(begin, end, sustain, doneAction:2);
  var env = Env([0, 1, 0], [0.01, 1], curve:-3);
  var envGen = IEnvGen.ar(env, line*env.times.sum*abs(speed));
  var modulator = (SinOsc.ar(freq*[1.0001,0.9999]*abs(speed))*2).tanh;
  var carrier = (SinOsc.ar((freq/4)+(modulator*1000*envGen))*0.8).tanh;
  var sound = DirtPan.ar(carrier, ~dirt.numChannels, pan, envGen);
  Out.ar(out, sound);
}).add;
SynthDef(\wub, {
  |out, sustain = 1, freq = 440, speed = 1, begin=0, end=1, pan, accelerate, offset|
  var line = Line.ar(begin, end, sustain, doneAction:2);
  var env = Env([0, 1, 0],[1,5], curve:2);
  var envGen = IEnvGen.ar(env, line*env.times.sum*abs(speed));
  // --- //
  var speedFreq = freq*abs(speed)/4;
  var mod = Latch.ar((SinOsc.ar(speedFreq*[7,11,13,5])*5).tanh*2000*envGen, Saw.ar(21000*envGen));
  var car =  Latch.ar(((SinOsc.ar(speedFreq+mod)*10)+(SinOsc.ar(speedFreq)!2)).tanh, Saw.ar(21000*envGen));
  var sound = LPF.ar(car, DC.ar(20000)*envGen+DC.ar(10))*(-11.dbamp);
  Out.ar(out, DirtPan.ar(sound, ~dirt.numChannels, pan, envGen));
}).add;
SynthDef(\sawpluck, {
  |out, sustain = 1, freq = 440, speed = 1, begin=0, end=1, pan, accelerate, offset|
  var line = Line.ar(begin, end, sustain, doneAction:2);
  var env = Env([0, 1, 0.333, 0],[5, 70, 1000]);
  var envGen = IEnvGen.ar(env, line*env.times.sum*abs(speed));
  var sound = Saw.ar(freq*abs(speed));
  Out.ar(out, DirtPan.ar(sound, ~dirt.numChannels, pan, envGen));
}).add;
SynthDef(\vibsawpluck, {
  |out, sustain = 1, freq = 440, speed = 1, begin=0, end=1, pan, accelerate, offset|
  var line = Line.ar(begin, end, sustain, doneAction:2);
  var env = Env([0, 1, 0.333, 0],[5, 70, 1000]);
  var envGen = IEnvGen.ar(env, line*env.times.sum*abs(speed));
  var sound = Saw.ar(freq*abs(speed)+(SinOsc.ar(10)*(freq*0.06)*line*line));
  sound = RLPF.ar(sound, Clip.ar(envGen*freq*48, 0, 20000), 0.5);
  Out.ar(out, DirtPan.ar(sound, ~dirt.numChannels, pan, envGen));
}).add;
SynthDef(\plucklead, {
  |out, sustain = 1, freq = 440, speed = 1, begin=0, end=1, pan, accelerate, offset|
  var line = Line.ar(begin, end, sustain, doneAction:2);
  var env = Env([0, 1, 0.333, 0],[5, 70, 1000]);
  var envGen = IEnvGen.ar(env, line*env.times.sum*abs(speed));
  var speedFreq = freq*abs(speed);
  var pulseLfo = SinOsc.ar(Rand(-1,1));
  var sound = RLPF.ar(Pulse.ar([speedFreq*Rand(0.99,1.01)*2,speedFreq*Rand(0.99,1.01)*2],pulseLfo)*0.5+Saw.ar(speedFreq), (20000*(envGen**2.8))+DC.ar(10), 0.5);
  Out.ar(out, DirtPan.ar(sound, ~dirt.numChannels, pan, envGen));
}).add;
SynthDef(\wob, {
  |out, sustain = 1, freq = 440, speed = 1, begin=0, end=1, pan, accelerate, offset|
  var line = Line.ar(begin, end, sustain, doneAction:2);
  var env = Env([0, 1, 0],[1, 1]);
  var envGen = IEnvGen.ar(env, line*env.times.sum*abs(speed));
  // --- //
  var speedFreq = freq*abs(speed)/4;
  var mod = Latch.ar((SinOsc.ar(speedFreq*[7,11,13,5])*5).tanh*2000*envGen, Saw.ar(21000*envGen));
  var car =  Latch.ar(((SinOsc.ar(speedFreq+mod)*10)+(SinOsc.ar(speedFreq)!2)).tanh, Saw.ar(21000*envGen));
  var sound = car*(-10.dbamp);
  Out.ar(out, DirtPan.ar(sound, ~dirt.numChannels, pan, envGen));
}).add;
SynthDef(\noisebuild, {
  |out, sustain = 1, freq = 440, speed = 1, begin=0, end=1, pan, accelerate, offset|
  var line = Line.ar(begin, end, sustain, doneAction:2);
  var env = Env([0, 1, 0],[127, 1],curve:4);
  var envGen = IEnvGen.ar(env, line*env.times.sum/*abs(speed)*/);
  // --- //
  var sound = HPF.ar(BPF.ar(WhiteNoise.ar()*envGen, 10+(20000*envGen), 0.8), 1000);
  Out.ar(out, DirtPan.ar(sound, ~dirt.numChannels, pan, envGen));
}).add;
SynthDef(\fmp, {
	|out,sustain = 1,freq = 440,speed = 1,begin=0,end=1,pan,accelerate,offset,cStartFreq=440,cEndFreq=440,cEnvFMod=0,mStartFreq=440,mEndFreq=440,mEnvFMod=0,fMod=0,pMod=0,cFFeedback=0,cPFeedback=0,mFFeedback=0,mPFeedback=0,feedbackDelay=0.01,attackCurve=1,decayCurve=1,atk=1,dcy=1|
	var line = Line.ar(begin, end, sustain, doneAction:2);
	var env = Env([0, 1, 0],[atk,dcy],[attackCurve,decayCurve]);
	var envGen = IEnvGen.ar(env, line*env.times.sum*abs(speed));
	var feedback = LocalIn.ar(1);
	var mFreq = ((1-line)*mStartFreq)+(line*mEndFreq)+(envGen*mEnvFMod)+(mFFeedback*feedback);
	var mPhase = mPFeedback*feedback;
	var modulator = SinOsc.ar(mFreq,mPhase)*envGen;
	var cFreq = ((1-line)*cStartFreq)+(line*cEndFreq)+(envGen*cEnvFMod)+(modulator*fMod)+(cFFeedback*feedback);
	var cPhase = (modulator*pMod)+(cPFeedback*feedback);
	var carrier = SinOsc.ar(cFreq,cPhase);
	var sound = carrier*envGen;
	LocalOut.ar(DelayL.ar(sound,10,feedbackDelay));
	Out.ar(out, DirtPan.ar(sound,~dirt.numChannels,pan,1));
}).add;
SynthDef(\ab2, {
  arg out, freq = 440, sustain = 5, speed = 1, begin =0, end=1, pan, accelerate, offset;
  var line = Line.ar(begin, end, sustain, doneAction:2);
  var env = Env([0, 1, 0], [0.01, 1], curve:-3);
  var envGen = IEnvGen.ar(env, line*env.times.sum*abs(speed));
  var snd;
  var sound;
  snd = Saw.ar([freq, freq*1.022]);
  8.do { |k|
	  var trig, ffreq;
	  snd = AllpassN.ar(snd, 1, 0.1 / (12 - k), 2);
	  trig = Impulse.ar(2 ** (8 - k) / 32, 1 / 2);
	  ffreq = 80 ** TRand.ar(1, 2, trig).lag((2 ** (8 - k)).reciprocal);
	  snd = BRF.ar(snd, ffreq, 2);
  };
  sound = DirtPan.ar(snd, ~dirt.numChannels, pan, envGen);
  Out.ar(out, sound);
}).add;
SynthDef(\flutter, {
  |out, sustain = 1, freq = 440, speed = 1, begin=0, end=1, pan, accelerate, offset|
  var line = Line.ar(begin, end, sustain, doneAction:2);
  var env = Env([0,1,0],[0.2,0.8]);
  var envGen = IEnvGen.ar(env, line*env.times.sum*abs(speed));
  var lfo = SinOsc.ar(Rand(-0.1,0.1),Rand(0,3.145))*(freq*abs(speed))/80;
  var sound = Saw.ar((freq*abs(speed))+lfo+(Rand(-1,1)*(freq*abs(speed))/DC.ar(100)));
  sound = LPF.ar(sound, (freq*abs(speed))*1.5);
  //sound = RLPF.ar(sound, Clip.ar(envGen*freq*48, 0, 20000), 0.5);
  Out.ar(out, DirtPan.ar(sound, ~dirt.numChannels, pan, envGen));
}).add;
SynthDef(\futuresaw, {
  |out, sustain = 1, freq = 440, speed = 1, begin=0, end=1, pan, accelerate, offset|
  var line = Line.ar(begin, end, sustain, doneAction:2);
  var envCurve = -1.5;
  var env = Env([0,1,0],[0.5,0.5],[-1*envCurve,envCurve]);
  var envGen = IEnvGen.ar(env, line*env.times.sum*abs(speed));
  var sound = Saw.ar((freq*abs(speed))+(Rand(-1,1)*(freq*abs(speed))/DC.ar(100)));
  sound = RLPF.ar(sound, Clip.ar(envGen*freq*48, 0, 20000), 0.5);
  Out.ar(out, DirtPan.ar(sound, ~dirt.numChannels, pan, envGen));
}).add;
SynthDef(\futuresub, {
  |out, sustain = 1, freq = 440, speed = 1, begin=0, end=1, pan, accelerate, offset|
  var line = Line.ar(begin, end, sustain, doneAction:2);
  var envCurve = 5;
  var env = Env([0,1,0],[0.5,0.5],[-1*envCurve,envCurve]);
  var envGen = IEnvGen.ar(env, line*env.times.sum*abs(speed));
  var sound = SinOsc.ar((freq*abs(speed))+(Rand(-1,1)*(freq*abs(speed))/DC.ar(100)));
  sound = sound*2;
  sound = sound.tanh;
  Out.ar(out, DirtPan.ar(sound, ~dirt.numChannels, pan, envGen));
}).add;
SynthDef(\ultrasaw, {
  |out, sustain = 1, freq = 440, speed = 1, begin=0, end=1, pan, accelerate, offset|
  var line = Line.ar(begin, end, sustain, doneAction:2);
  var env = Env([0,1,1,0],[0.001,1,0.001]);
  var envGen = IEnvGen.ar(env, line*env.times.sum*abs(speed));
  var freqs = {Rand(-1,1)*(freq/60)}.dup(12);
  var sound = Saw.ar((freq+freqs));
  sound = sound + Saw.ar((freq+freqs)*2);
  sound = sound + Saw.ar((freq+freqs)*0.5);
  sound = sound + Saw.ar((freq+freqs)*4);
  sound = sound + Saw.ar((freq+freqs)*0.25);
  sound = LPF.ar(sound, freq*1.5);
  //sound = RLPF.ar(sound, Clip.ar(envGen*freq*48, 0, 20000), 0.5);
  Out.ar(out, DirtPan.ar(sound, ~dirt.numChannels, pan, envGen));
}).add;
SynthDef(\acid, {
  |out, sustain = 1, freq = 440, speed = 1, begin=0, end=1, pan, accelerate, offset, clamp=1, res=0.2, mix=0.1|
  var line = Line.ar(begin, end, sustain, doneAction:2);
  var env = Env([0, 1, 0], [0.01, 1], curve:-3);
  var envGen = IEnvGen.ar(env, line*env.times.sum*abs(speed));
  var carrier = RLPF.ar((Saw.ar(freq)*(1-mix))+(Pulse.ar(freq)*mix), envGen*clamp*18000,res);
  var sound = DirtPan.ar(carrier, ~dirt.numChannels, pan, envGen);
  Out.ar(out, sound);
}).add;
SynthDef(\sinosc, {
  |out, sustain = 1, freq = 440, speed = 1, begin=0, end=1, pan=0.5, accelerate, offset|
  var env, envTimes, envGen, line, sound, rate, phase;
  envTimes = [1,100];
  envTimes = envTimes/envTimes.sum;
  env = Env([0,1,0],envTimes);
  line = Line.ar(0,1,sustain,doneAction:2);
  envGen = IEnvGen.ar(env,line);
  sound = SinOsc.ar(freq*speed)*envGen;
  OffsetOut.ar(out, DirtPan.ar(sound, ~dirt.numChannels, pan, envGen));
}).add;

SynthDef(\dummy, { |out, sustain = 1, speed = 1, begin = 0, end = 1, pan, accelerate, offset|
	var env, sound, rate, phase;
	phase = Line.ar(begin, end, sustain);
	rate = (begin + 1) * (speed + Sweep.kr(1, accelerate));
	sound = DC.ar(0);
	OffsetOut.ar(out,
		DirtPan.ar(sound, ~dirt.numChannels, pan)
	)
}).add
);

(
SynthDef(\click, { |out, sustain = 1, speed = 1, begin = 0, end = 1, pan, accelerate, offset|
	var env, sound, rate, phase;
	phase = Line.ar(begin, end, sustain);
	rate = (begin + 1) * (speed + Sweep.kr(1, accelerate));
	sound = Impulse.ar(0);
	sound = Decay.ar(sound, sustain);
	OffsetOut.ar(out,
		DirtPan.ar(sound, ~dirt.numChannels, pan)
	)
}).add
);

(
SynthDef(\stiff, { |out, sustain = 1, freq, amp=0.5, c3=20, pan, fB = 80, rel= 0.1, offset|

	var env = EnvGen.ar(Env.new([0,1, 1, 0],[0.001,0.006, 0.0005],[5,-5, -8]), timeScale:sustain);
	var inp = amp * LFClipNoise.ar(2000) * env;
	var sound = DWGPluckedStiff.ar(freq, amp, 1,0.14,1,c3,inp,rel,fB);  //here was the problem!... input, release, feedback

	DetectSilence.ar(sound, 0.001, doneAction:2);
	OffsetOut.ar(out,
		DirtPan.ar(sound * 0.1, ~dirt.numChannels, pan)
	)
}).add
);

/*(
SynthDef(\piano, { |out, sustain = 1, freq=440, /*gate, */ amp=0.5, rho=1, pan, offset|
var env = EnvGen.ar( Env.asr(0,1,0.1), /*gate, */ timeScale:sustain, doneAction:2 );
var sound = OteyPianoStrings.ar( freq, amp, rho:rho ) * env;
OffsetOut.ar(out,
DirtPan.ar(sound * 1, ~dirt.numChannels, LinLin.kr(freq,36.midicps,90.midicps,-0.75,0.75), env )
)
}).add;

{
SynthDef(\piano_soundboard,{
var audio = In.ar([20, 21], 2);
audio = OteySoundBoard.ar(audio);
Out.ar(0,audio);
}).add;
s.sync;
//start soundboard effect
Synth.tail(1,\piano_soundboard)
}.fork;
);*/


(
SynthDef(\atari, { | out, freq, freqb=550, freqc=660, vola=15, volb=0, volc=0, chink=1, wobbly=1, pan, amp=0.1, /*gate, */ sustain = 1, offset|
	var ay, chinkenv, wobblyenv, env;

	env = EnvGen.kr(Env.asr(0.01, 1, 0.05), /*gate, */ timeScale:sustain, doneAction:2);
	//chinkenv = if(chink>0, EnvGen.kr(Env.new([0.06125, 0.06125, 1, 1], [0.05, 0, 0.1], 0, 4, 4)), 1);
	chinkenv = if(chink>0, EnvGen.kr(Env.new([2, 2, 1, 1], [0.05, 0, 0.1], 0, 4, 4)), 1);
	wobblyenv = LFPulse.kr(10, 0.5, mul:wobbly).range(0.5, 1);

	# freq, freqb, freqc = [freq, freqb, freqc] * [1, wobblyenv, wobblyenv] * chinkenv;
	ay = AY.ar(AY.freqtotone(freq), AY.freqtotone(freqb), AY.freqtotone(freqc),
		0, 3, vola, volb, volc, mul: amp);
	// ay = ay * env;
	// Out.ar(0, Pan2.ar(ay, pan));
	OffsetOut.ar(out,
		DirtPan.ar(ay, ~dirt.numChannels, pan, env)
	)
}).add;
);

(
SynthDef(\bow, { |out, freq, amp=0.5, force=1, /*gate, */ pos=0.07, c1=0.25, c3=31, pan, offset/*, sustain = 0.1*/|
	var vib = Gendy1.kr(1,1,1,1,0.1, 4,mul:0.003,add:1);
	var son = DWGBowedTor.ar(freq*vib, amp,force, /*gate, */pos,0.1,c1,c3);
	son = DWGSoundBoard.ar(son);
	son = BPF.ar(son,118,1)+son;
	son = BPF.ar(son,430,1)+son;
	son = BPF.ar(son,490,1)+son;
	son = LPF.ar(son,6000);
	// Out.ar(out, Pan2.ar(son * 0.1, pan));
	OffsetOut.ar(out,
		DirtPan.ar(son * 0.1, ~dirt.numChannels, pan)
	)
}).add;
);

(
SynthDef(\windsaw, { | out, offset, sustain = 1, freq, attack = 0.1, release = 5, decay = 3, amp = 0.3, /*maxosc = 20, loosc = 6,*/ maxamp = 2.5, loamp = 0.1, gate = 1, pan |
	var snd, env;
	env = Env.adsr(
		attackTime: attack,
		decayTime: decay,
		sustainLevel: 0.75,
		releaseTime: release,
		peakLevel: amp
	).kr(doneAction: 2, /*gate: gate, */ timeScale: sustain);
	snd = LFSaw.ar(
		freq: freq*[2, 3, 4]/6,
		mul: env /* * SinOsc.kr(freq: XLine.kr(maxosc, loosc, 5).range(maxamp, loamp))*/ );
	snd = snd + LFTri.ar(
		freq: freq,
		mul: env * SinOsc.kr(freq: LFTri.kr((sustain * 10).reciprocal, 3).exprange(loamp, maxamp)));
	snd = Splay.ar(snd);
	OffsetOut.ar(out,
		DirtPan.ar(snd, ~dirt.numChannels, pan, env)
	)
}).add;
);

(
SynthDef(\saw, { | freq, amp = 0.1, att = 0.001, rel = 0.2, lofreq = 1000, hifreq = 3000, out, offset, pan |
	var env, snd;
	env = Env.perc(
		attackTime: att,
		releaseTime: rel,
		level: amp
	).kr(doneAction: 2);
	snd = Saw.ar(freq: freq * [0.99, 1, 1.001, 1.008], mul: env);
	snd = LPF.ar(
		in: snd,
		freq: LFNoise2.kr(1).range(lofreq, hifreq)
	);
	snd = Splay.ar(snd);
	OffsetOut.ar(out,
		DirtPan.ar(snd, ~dirt.numChannels, pan, env)
	)
}).add;
);

(
SynthDef(\bazz, { | freq, amp=0.01, attack = 0.025, decay = 0.6, release = 0.5, crunchy = 200, /*gate, */ out, pan, offset, sustain = 1 |
	var snd, env, snd2, sum;
	env = Env.adsr(
		attackTime: attack, decayTime: decay, sustainLevel: 0.5, releaseTime: release
	).kr(doneAction:2, timeScale:sustain/*, gate:gate*/);
	env = env/3;
	snd = LFSaw.ar(freq: SinOsc.kr(XLine.kr(7, 2, 1)).range(freq, freq/2), mul: env/2)
	+ LFSaw.ar(freq: freq, mul: env)
	+ LFTri.ar(freq: freq*[3], mul: env/2)
	+ SinOsc.ar(freq: freq*[2, 4, 6], mul: env);
	snd = LPF.ar(
		in: snd,
		freq: XLine.kr( 10000, crunchy, attack * 5 )
	);
	sum = snd;
	sum = Splay.ar(sum);
	OffsetOut.ar(out,
		DirtPan.ar(sum, ~dirt.numChannels, pan, env)
	)
}).add;
);

(
SynthDef(\rhodes, { | out freq pan vel = 0.8 modind = 0.2 mix = 0.2 lfospeed = 0.4 lfodepth = 0.1 sustain = 1 gate = 1 |
    var env1, env2, env3, env4;
    var osc1, osc2, osc3, osc4, snd;
    lfospeed = lfospeed * 12;
    freq = freq * 2;
    env1 = EnvGen.ar(Env.adsr(0.001, 1.25, 0.0, 0.04, curve: \lin));
    env2 = EnvGen.ar(Env.adsr(0.001, 1.00, 0.0, 0.04, curve: \lin));
    env3 = EnvGen.ar(Env.adsr(0.001, 1.50, 0.0, 0.04, curve: \lin));
    env4 = EnvGen.ar(Env.adsr(0.001, 1.50, 0.0, 0.04, curve: \lin));
    osc4 = SinOsc.ar(freq * 0.5) * 2pi * 2 * 0.535887 * modind * env4 * vel;
    osc3 = SinOsc.ar(freq, osc4) * env3 * vel;
    osc2 = SinOsc.ar(freq * 15) * 2pi * 0.108819 * env2 * vel;
    osc1 = SinOsc.ar(freq, osc2) * env1 * vel;
    snd = Mix((osc3 * (1 - mix)) + (osc1 * mix));
    snd = snd * (SinOsc.ar(lfospeed) * lfodepth + 1);
    // using the doneAction: 2 on the other envs can create clicks (bc of the linear curve maybe?)
    snd = snd * EnvGen.ar(Env.asr(0, 1, 0.1), gate, doneAction: 2);
    snd = Pan2.ar(snd, pan);
    OffsetOut.ar(out,
		DirtPan.ar(snd, ~dirt.numChannels, pan)
	)
}).add;
);
