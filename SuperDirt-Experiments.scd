(SynthDef(\plaits, {|out, begin=0, end=1, sustain=1, pan=0.5, accelerate, freq, harm=0.5, timbre=0.5, morph=0.5, engine=0, speed = 1|
	// var line = Line.ar(begin, end, sustain, doneAction:2);
	// var env = Env([0, 1, 0],[1,5], curve:2);
    // var envGen = IEnvGen.ar(env, line * env.times.sum * abs(speed));
	var sound = MiPlaits.ar(
			pitch: freq.cpsmidi,
			engine: engine,
			harm: harm,
			timbre: timbre,
			morph: morph
		);
	Out.ar(out,
		DirtPan.ar(sound, ~dirt.numChannels, pan)
	)
}).add);

x = { Pan2(MiPlaits.ar(35, 0, harm: 0.252, morph: 0.2, mul: 0.2), 2, 0.5) }.play
x.free


(
	// Setup FX addition
	~busses = ~dirt.orbits.collect(_.dryBus) ++ 0;

	SynthDef(\dirt_vst_fx, { | buz = 0|
		var out = Select.kr(buz, ~busses);
		ReplaceOut.ar(out, VSTPlugin.ar( In.ar(out, ~dirt.numChannels), ~dirt.numChannels ));
	}).add;
)

(
	// Add compressor to first orbit
	//-----------------------------------------------------------------------------
	{
		~vst0Orb0 = Synth("dirt_vst_fx", [ \buz, 0 ], ~dirt.orbits[0].group, addAction: \addAfter);
		~comp0Orb0 = VSTPluginController(~vst0Orb0).open( "MSaturator.vst3", editor: true ); { ~comp0Orb0.gui }.defer;
		// ~comp0Orb0.readProgram("mypresetfolder/text.fxp");
	}.fork;

	// Add compressor to fifth orbit
	//-----------------------------------------------------------------------------
	{
		~vst0Orb5 = Synth("dirt_vst_fx", [ \buz, 0 ], ~dirt.orbits[4].group, addAction: \addAfter);
		~comp0Orb5 = VSTPluginController(~vst0Orb5).open( "MOscilloscope.vst3", editor: true ); { ~comp0Orb5.gui }.defer;
		// ~comp0Orb5 = VSTPluginController(~vst0Orb5).open( "iZotope Trash 2", editor: true ); { ~comp0Orb5.gui }.defer;
	}.fork;
)

~comp0Orb0.gui
~vst0Orb0.info.PrintPrograms


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
