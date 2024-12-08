(
// DrumSynths SC Example - SOS Drums by Renick Bell, renick@gmail.com
// recipes from Gordon Reid in his Sound on Sound articles
// SOSkick — http:www.soundonsound.com/sos/jan02/articles/synthsecrets0102.asp
// increase pitch1 and voice for interesting electronic percussion
SynthDef(\soskick, {
  arg out, pan, freq = 50, pitch1 = 5, voice = 5, sustain = 1, nl = 0.025;

  var pitchContour, drumOsc, drumLpf, drumEnv, env;
  var beaterSource, beaterHpf, beaterLpf, lpfCutoffContour, beaterEnv;
  var kickMix, snd;

  pitchContour = Line.kr(freq * 2, freq, 0.02);
  drumOsc = PMOsc.ar(pitchContour, pitch1, voice / 1.3, mul: 1, add: 0);
  drumLpf = LPF.ar(in: drumOsc, freq: 1000, mul: 1, add: 0);
  env = EnvGen.kr(Env.perc(0.005, sustain), 1.0, doneAction: Done.freeSelf);
  drumEnv = drumLpf * env;

  beaterSource = WhiteNoise.ar(nl);
  beaterHpf = HPF.ar(in: beaterSource, freq: 500, mul: 1, add: 0);

  lpfCutoffContour = Line.kr(6000, 500, 0.03);

  beaterLpf = LPF.ar(in: beaterHpf, freq: lpfCutoffContour, mul: 1, add: 0);
  beaterEnv = beaterLpf * EnvGen.kr(Env.perc, 1.0);

  kickMix = (drumEnv + beaterEnv) * 2;

  Out.ar(out, DirtPan.ar(kickMix, ~dirt.numChannels, pan, env));
},
metadata: (
  credit: "Renick Bell",
  category: \drums,
  tags: [\pitched, \bass, \sos]
)).add;
);

(
// Retrieved from http://sccode.org/1-5aD
// DrumSynths SC Example - SOS Drums by Renick Bell, renick@gmail.com
// Recipes from Gordon Reid in his Sound on Sound articles
// SOShats — http:www.soundonsound.com/sos/Jun02/articles/synthsecrets0602.asp
// Modified by Bruno Ruviaro and Josh Mitchell 8/19
SynthDef(\soshats, {
  arg out = 0, pan = 0, freq = 220, pitch1 = 238.5, resonance = 1, sustain = 0.5;

  var root, initialEnv, initialSweep, initial, bodyEnv, bodySweep, body, snd, env;

  root = Pulse.ar(freq: freq, width: 0.5, mul: 1);
  root = PMOsc.ar(carfreq: root, modfreq: freq * [1.34, 2.405, 3.09, 1.309], pmindex: pitch1 * [1, 0.22, 0.014, 0.0038]);
  root = Mix(root);

  initialEnv = Env.perc(attackTime: 0.005, releaseTime: sustain / 2, curve: -4).kr;
  initialSweep = Line.kr(start: 15000, end: 9000, dur: sustain / 2);
  initial = BPF.ar(in: root, freq: initialSweep, rq: resonance, mul: initialEnv);

  bodyEnv = Env.perc(attackTime: 0.005, releaseTime: sustain / 2, curve: -4 / 2).kr;
  bodySweep = Line.kr(start: 9000, end: 12000, dur: sustain / 2);
  body = HPF.ar(in: root, freq: bodySweep, mul: bodyEnv);

  snd = (initial + body) / 4;

  env = Line.kr(start: 1, end: 1, dur: sustain);

  Out.ar(out, DirtPan.ar(snd, ~dirt.numChannels, pan, env));
},
metadata: (
  credit: "Renick Bell",
  category: \drums,
  tags: [\pitched, \hihats, \sos]
)).add;
);

(
// Retrieved from http:sccode.org/1-5aD
// DrumSynths SC Example - SOS Drums by Renick Bell, renick_at_gmail.com
// recipes from Gordon Reid in his Sound on Sound articles
// SOStom — http:www.soundonsound.com/sos/Mar02/articles/synthsecrets0302.asp
// Modified by Bruno Ruviaro and Josh Mitchell 8/19.
SynthDef(\sostoms, {
  arg out, pan, sustain = 0.5, freq = 250, voice = 0.77;

  var drumMode, drumModeEnv, stick, stickEnv, snd;

  drumModeEnv = Env.perc(attackTime: 0.005, releaseTime: sustain / 1.25, level: 0.5, curve: -6).kr(doneAction: Done.freeSelf);

  drumMode = PMOsc.ar(carfreq: Saw.ar(freq: freq * 0.9), modfreq: freq * 0.85, pmindex: voice, mul: drumModeEnv * 10);
  drumMode = drumMode + SinOsc.ar(freq: [freq, freq * 0.8], mul: drumModeEnv);
  drumMode = Mix.ar(drumMode) * 0.5;

  stick = Crackle.ar(chaosParam: 2.01);
  stickEnv = Env.perc(attackTime: 0.005, releaseTime: sustain / 5, level: 3, curve: -6).kr;

  snd = (drumMode + stickEnv) / 4;
  snd = LeakDC.ar(snd);

  Out.ar(out, DirtPan.ar(snd, ~dirt.numChannels, pan, drumModeEnv));
},
metadata: (
  credit: "Renick Bell",
  category: \drums,
  tags: [\pitched, \tom, \sos]
)).add;
);

(
// Retrieved from http:sccode.org/1-5aD
// DrumSynths SC Example — SOS Drums by Renick Bell, renick_at_gmail.com
// recipes from Gordon Reid in his Sound on Sound articles
// SOSsnare — http:www.soundonsound.com/sos/Mar02/articles/synthsecrets0302.asp
// Latch.ar(WhiteNoise.ar(0.1), Impulse.ar(nyquist * 2)) is added aliasing for effect
// Modified by Bruno Ruviaro and Josh Mitchell 8/19.
SynthDef(\sossnare, {
  arg out, pan, freq = 405, curve = -4, voice = 0.385, semitone = 0.452, pitch1 = 2000, resonance = 0.1, bwr = 1;

  var drumMode, drumModeEnv, snares, snareEnv, snd, p1;

  drumModeEnv = Env.perc(attackTime: 0.005, releaseTime: 0.1, level: 0.5, curve: curve).kr(doneAction: Done.freeSelf);
  drumMode = SinOsc.ar(freq: freq * 0.53, mul: drumModeEnv);
  drumMode = drumMode + SinOsc.ar(freq: freq, mul: drumModeEnv);
  drumMode = drumMode + PMOsc.ar(carfreq: Saw.ar(freq * 0.85), modfreq: freq * semitone, pmindex: voice, mul: drumModeEnv * 10);
  drumMode = drumMode * 0.25;

  snareEnv = Env.perc(attackTime: 0.005, releaseTime: 0.1, curve: curve).kr;

  snares = Latch.ar(WhiteNoise.ar(0.1), Impulse.ar(1700 * 2));
  snares = BRF.ar(in: snares, freq: 4 * pitch1, mul: 0.5, rq: resonance);
  snares = BRF.ar(in: snares, freq: 2.5 * pitch1, mul: 0.5, rq: resonance);
  snares = BRF.ar(in: snares, freq: 1.8 * pitch1, mul: 0.5, rq: resonance);
  snares = BRF.ar(in: snares, freq: pitch1, mul: snareEnv, rq: resonance);
  snares = Resonz.ar(in: snares, freq: pitch1 / 2, bwr: bwr, mul: 40);

  snd = (drumMode + snares) * 8;

  Out.ar(out, DirtPan.ar(snd, ~dirt.numChannels, pan, drumModeEnv));
},
metadata: (
  credit: "Renick Bell",
  category: \drums,
  tags: [\pitched, \snare, \sos]
)).add;
);
