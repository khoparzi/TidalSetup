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
}).add;

SynthDef(\sinosc, {
  |out, sustain = 0.1, freq = 440, speed = 1, begin=0, end=0.1, pan=0.5, accelerate, offset|
  var env, envTimes, envGen, line, sound, rate, phase;
  envTimes = [1,100];
  envTimes = envTimes/envTimes.sum;
  env = Env([0,1,0],envTimes);
  line = Line.ar(0,1,sustain,doneAction:2);
  envGen = IEnvGen.ar(env,line);
  sound = SinOsc.ar(freq*speed)*envGen;
  OffsetOut.ar(out, DirtPan.ar(sound, ~dirt.numChannels, pan, envGen));
}).add;

SynthDef(\ikclick, { arg out = 0, gate = 1, centerFreq = 15000, rq = 0.9, mul = 25, pan = 0, amp = 1;
    var env, noise, sig;
    env = EnvGen.ar(Env.perc(0.001, 0.00001 * LFNoise1.kr(4).abs, 1, -4), gate, doneAction: 0);
    noise = PinkNoise.ar(env);
    sig = BPF.ar(noise, centerFreq.max(1), rq, mul).fold(-1, 1);
    Out.ar(out, Pan2.ar(sig, pan, amp.curvelin(0, 1, 0, 1, log(10))));
}).add;
);
