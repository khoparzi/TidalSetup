(SynthDef(\piano, { |out, sustain = 1, freq=440, /*gate, */ amp=0.5, rho=1, pan, offset|
    var env = EnvGen.ar( Env.asr(0,1,0.1), /*gate, */ timeScale:sustain, doneAction:2 );
    var sound = OteyPianoStrings.ar( freq, amp, rho:rho ) * env;
    OffsetOut.ar(out,
    DirtPan.ar(sound * 1, ~dirt.numChannels, LinLin.kr(freq,36.midicps,90.midicps,-0.75,0.75), env )
    )
    }).add;

SynthDef(\piano_soundboard,{
var audio = In.ar([20, 21], 2);
audio = OteySoundBoard.ar(audio);
Out.ar(0,audio);
    }).add;

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
