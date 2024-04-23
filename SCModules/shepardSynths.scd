// dietcv's example on scsynth.org
// https://scsynth.org/t/how-can-i-make-the-junkers-ju-87-jericho-trumpet-sound-using-a-glissando/9238/4

(
var multiChannelPhase = { |numChannels, rate|
	var localRate = rate / numChannels;
	numChannels.collect{ |i|
		var localPhase, localTrig, hasTriggered;
		localPhase = Phasor.ar(DC.ar(0), localRate * SampleDur.ir);
		localPhase = (localPhase + (1 - (i / numChannels)) - SampleDur.ir).wrap(0, 1);
		localTrig = HPZ1.ar(localPhase) < 0;
		hasTriggered = PulseCount.ar(localTrig) >= 1;
		//localPhase * hasTriggered;
		localPhase;
	};
};

SynthDef(\shepard, {

	var numChannels = 10;

	var tFreq, windowRates, windowPhases, freqWindows, grainWindows, freq, sig;

	tFreq = \tFreq.kr(10);
	windowRates = tFreq / \overlap.kr(1);
	windowPhases = multiChannelPhase.(numChannels, windowRates);

	freqWindows = IEnvGen.ar(Env([0, 1], [1.0], \lin), windowPhases);
	grainWindows = IEnvGen.ar(Env([0, 1, 0], [0.5, 0.5], \sin), windowPhases);

	freq = \freq.kr(440) * (2 ** (freqWindows * \freqModAmount.kr(4)));

	sig = SinOsc.ar(freq);

	sig = sig * grainWindows;

	sig = Splay.ar(sig);

	sig = sig * \amp.kr(-20.dbamp);

	sig = sig * Env.asr(0.001, 1, 0.001).ar(Done.freeSelf, \gate.kr(1));

	Out.ar(\out.kr(0), sig);
}).add;
)



(
Routine({

	var ratios = [1, 5/4, 3/2];

	s.bind {
		ratios.collect{ |ratio|

			Synth(\shepard, [

				\tFreq, 1,

				\freq, 220 * ratio,
				\freqModAmount, 4,
				\overlap, 10,

				\amp, -40.dbamp,
				\out, 0,

			]);
		};
	};

}).play;
)


// With Additive synthesis
(
var multiChannelTrigger = { |numChannels, trig|
	var rate = if(trig.rate == \audio, \ar, \kr);
	numChannels.collect{ |chan|
		PulseDivider.perform(rate, trig, numChannels, chan);
	};
};

var multiChannelPhase = { |triggers, windowRate|
	var rate = if(triggers.rate == \audio, \ar, \kr);
	triggers.collect{ |localTrig, i|
		var hasTriggered = PulseCount.perform(rate, localTrig) > 0;
		Sweep.perform(rate, localTrig, windowRate * hasTriggered);
	};
};

var makeStretchedHarmonicSeries = { |numPartials, freq, inharmonicity|
	var ratios = (1..numPartials);
	var chain = (
		numPartials: numPartials,
		ratios: ratios,
		amps: 1 ! numPartials,
	);
	chain[\freqs] = freq * ratios * (1 + (inharmonicity * ratios * ratios)).sqrt;
	chain;
};

var addSpectralTilt = { |chain, tiltPerOctave|
	chain[\amps] = chain[\amps] * (chain[\ratios].log2 * tiltPerOctave).dbamp;
	chain;
};

SynthDef(\additive_glisson, {

	var	numChannels = 5;
	var numPartials = 16;

	var tFreq, trig, triggers, overlap, windowRate, windowPhases;
	var grainWindows, freqWindows, freqs, sigs, sig;

	tFreq = \tFreq.kr(0.1);
	trig = Impulse.kr(tFreq);

	triggers = multiChannelTrigger.(numChannels, trig);

	overlap = \overlap.kr(1);
	windowRate = tFreq / overlap;

	windowPhases = multiChannelPhase.(triggers, windowRate);

	grainWindows = IEnvGen.ar(Env([0, 1, 0], [0.5, 0.5], \sin), windowPhases);

	freqWindows = IEnvGen.kr(Env([0, 1], [1], \lin), windowPhases);
	freqs = \freq.kr(110) * (2 ** (freqWindows * \freqModAmount.kr(4)));

	sigs = freqs.collect { |freq, i|
		var chain, sig;

		chain = makeStretchedHarmonicSeries.(numPartials, freq, \inharmonicity.kr(0.01));
		chain = addSpectralTilt.(chain, \tiltPerOctaveDb.kr(-3));

		sig = SinOsc.ar(
			freq: chain[\freqs],
			phase: { Rand(0, 2pi) } ! chain[\numPartials],
			mul: chain[\amps]
		);

		sig = sig[0,2..].sum + ([-1,1] * sig[1,3..].sum);

		sig * grainWindows[i];

	};

	sig = sigs.sum;

	sig = sig * \amp.kr(-25.dbamp);

	sig = sig * Env.asr(0.001, 1, 0.001).ar(Done.freeSelf, \gate.kr(1));

	Out.ar(\out.kr(0), sig);
}).add;
)

(
Routine({

	var ratios = [1.0, 1.2885607692297, 1.9661338478527];

	s.bind {

		ratios.collect{ |ratio|
			var note = 40.midicps;

			Synth(\additive_glisson, [

				\tFreq, 0.1,
				\overlap, 5,

				\freq, 22 * ratio,
				\freqModAmount, 4,

				\inharmonicity, 0.005,
				\tiltPerOctaveDb, -3,

				\amp, -25.dbamp,
				\out, 0,

			]);
		};

	};

}).play;
)