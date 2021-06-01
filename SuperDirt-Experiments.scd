
VSTPlugin.search(s, "~/.vst_linvst")
VSTPlugin.clear


({
	q = q ? ();
	("\n loading VST Effects \n ").postln;
	q.busses0 = ~dirt.orbits.collect(_.dryBus) ++ 0;

	(
		SynthDef(\insertVst, { | buz = 0|
			var out = Select.kr( buz, q.busses0 );
			ReplaceOut.ar(out, VSTPlugin.ar( In.ar(out, ~dirt.numChannels), ~dirt.numChannels ));
		}).add;
	);
	s.sync;
	0.2.wait;

	// Master Effect
	// VSTPlugin.pluginKeys(s)
	// add a group after default group
	~masterFxGroup = Group.after(1);
	s.sync;
	0.1.wait;
	(
		~oscilloscope = VSTPluginController(
			Synth( \insertVst,[ \buz, ( q.busses0.last ) ], target: ~masterFxGroup , addAction: 'addToTail')
		).open( "MOscilloscope", editor: true );
	);
	s.sync;
	0.2.wait;
	/*
	~oscilloscope.info.parameters.do{|v|v.postln};
	~oscilloscope.close;
	~oscilloscope.editor;
	~oscilloscope.gui;
	*/
	// s.sync;
}.fork;)


// Mutable Instruments clouds

(

(1..SuperDirt.maxSampleNumChannels).do { |numChannels|

	  SynthDef("global_mi_verb" ++ numChannels, { |dryBus, effectBus, verbwet=0, verbtime=0, verbdamp=0, verbhp=0, verbfreeze=0, verbdiff=0.625, verbgain=0|
	    var signal = In.ar(dryBus, ~dirt.numChannels);
	    Out.ar(effectBus, MiVerb.ar(
	      signal,
	      verbtime,
	      verbwet,
	      verbdamp,
	      verbhp,
	      verbfreeze,
	      verbdiff
	    ) * verbgain);
	  }, [\ir, \ir]).add;

	  SynthDef("global_mi_clouds" ++ numChannels, { |dryBus, effectBus, cloudspitch=0, cloudspos=0, cloudssize=0, cloudsdens=0, cloudstex=0, cloudswet=0, cloudsgain=0, cloudsspread=0, cloudsrvb=0, cloudsfb=0, cloudsfreeze=0, cloudsmode=0, cloudslofi=0|
	    var signal = In.ar(dryBus, ~dirt.numChannels);
	    Out.ar(effectBus, MiClouds.ar(
	      inputArray: signal,
	      pit: cloudspitch,
	      pos: cloudspos,
	      size: cloudssize,
	      dens: cloudsdens,
	      tex: cloudstex,
	      drywet: cloudswet,
	      in_gain: cloudsgain,
	      spread: cloudsspread,
	      rvb: cloudsrvb,
	      fb: cloudsfb,
	      freeze: cloudsfreeze,
	      mode: cloudsmode,
	      lofi: cloudslofi
	    ));
	  }, [\ir, \ir]).add;
	};
);

(
	~d1 = ~dirt.orbits[0]; ~d2 = ~dirt.orbits[1]; ~d3 = ~dirt.orbits[2]; ~d4 = ~dirt.orbits[3]; ~d5 = ~dirt.orbits[4]; ~d6 = ~dirt.orbits[5]; ~d7 = ~dirt.orbits[6]; ~d8 = ~dirt.orbits[7]; ~d9 = ~dirt.orbits[8]; ~d10 = ~dirt.orbits[9];
	~dirt.orbits.do { |x|
		var clouds = GlobalDirtEffect(\global_mi_clouds, [\cloudspitch, \cloudspos, \cloudssize, \cloudsdens, \cloudstex, \cloudswet, \cloudsgain, \cloudsspread, \cloudsrvb, \cloudsfb, \cloudsfreeze, \cloudsmode, \cloudslofi]);
		var verb = GlobalDirtEffect(\global_mi_verb, [\verbwet, \verbtime, \verbdamp, \verbhp, \verbfreeze, \verbdiff, \verbgain]); x.globalEffects = x.globalEffects .addFirst(clouds) .addFirst(verb);
	};
);
