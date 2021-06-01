q = q ? ();
q.fx = q.fx ? ();
{
	// Ndef(\orb0).ar(~dirt.numChannels);
	// Ndef(\orb1).ar(~dirt.numChannels);
	// Ndef(\orb2).ar(~dirt.numChannels);
	// Ndef(\orb3).ar(~dirt.numChannels);
	// 0.2.wait;
	// ~dirt.orbits[0].outBus = Ndef(\orb0).bus;
	// ~dirt.orbits[1].outBus = Ndef(\orb1).bus;
	// ~dirt.orbits[2].outBus = Ndef(\orb2).bus;
	// ~dirt.orbits[3].outBus = Ndef(\orb3).bus;
	// 0.2.wait;
	// Ndef(\orb0).play; //(~dirt.numChannels);
	// Ndef(\orb1).play; //(~dirt.numChannels * 2);
	// Ndef(\orb2).play; //(~dirt.numChannels * 3);
	// Ndef(\orb3).play; //(~dirt.numChannels * 4);
	// 0.2.wait;
	ProxyChain.add3( \leakDC,  \filterIn -> { |in, leak=0.99| LeakDC.ar(in, leak) },
		1,
		(\leak: [0.5, 0.999, \exp])
	);

	ProxyChain.add3( \comp, \filterIn -> { | in, thresh = 0.33, slopea = 0.33, clamp = 0.03, relax = 0.35 |
		var comp = Compander.ar( in, in, thresh.linexp(0, 1, 0.001, 1), 1, slopea, clamp, relax );
		Limiter.ar( comp );
	},
	1,
	(
		\thresh: [-60.dbamp, 1, \amp],
		\slopea: [0.05, 1],
		\clamp: [0.001, 0.4],
		\relax: [0.1, 1]
	)
	);

	ProxyChain.add3( \ampFin, \filter -> { |in, drive=1.2, ampLimit=0.8 |
		Limiter.ar(in * drive, ampLimit);
	},
	1,
	(
		\drive: [1, 10, \exp],
		\ampLimit: \amp
	)
	);

	q.masta = MasterFX.new(server: s,
		numChannels: s.options.numOutputBusChannels,
		slotNames: [\leakDC, \ampFin],
		busIndex: 0
	);
	q.masta.add(\leakDC, 1);
	q.masta.add(\ampFin, 1);  // limit output
	{q.mastaGui = q.masta.gui('Masta!', 10)}.defer;
}.fork;

{
    ("\n loading VST Effects \n ").postln;
    q.busses0 = ~dirt.orbits.collect(_.dryBus) ++ 0;

	(
		SynthDef(\insertVst, { | buz = 0|
            var out = Select.kr(buz, q.busses0);
			ReplaceOut.ar(out, VSTPlugin.ar( In.ar(out, ~dirt.numChannels), ~dirt.numChannels ));
		}).add;
	);
    s.sync;
	0.2.wait;

	//-----------------------------------------------------------------------------
	(
		~verbOrb0 = VSTPluginController(
        Synth(\insertVst, [ \buz, 0 ], ~dirt.orbits[0].group, addAction: \addAfter)
    ).open( "Calf Reverb", editor: false );
	);
	s.sync;
    0.2.wait;
	// ~verbOrb0.readProgram("/home/skmecs/vst/orb0verb.fxp");
	" \n tal verb at orb 0 loaded \n ".postln;
	{ ~verbOrb0.gui }.defer;

    /*
    ~verbOrb0.close;
    ~verbOrb0.gui;
	~verbOrb0.close;
	~verbOrb0.info.parameters.printcsAll;
	*/
	" \n tal verb at orb 0 loaded  \n ".postln;

	(
		~verbOrb1 = VSTPluginController(
		Synth(\insertVst, [ \buz, 1 ], ~dirt.orbits[1].group, addAction: \addAfter)
	).open( "Calf Reverb", editor: false );
	);
	s.sync;
	0.2.wait;
	// ~verbOrb1.readProgram("/home/skmecs/vst/orb1verb.fxp");
	" \n tal verb at orb 1 loaded \n ".postln;
	{ ~verbOrb1.gui }.defer;
	/*
	~verbOrb1.gui;
	~verbOrb1.close;
	*/

	(
		~verbOrb2 = VSTPluginController(
		Synth(\insertVst, [ \buz, 2 ], ~dirt.orbits[2].group, addAction: \addAfter)
	).open( "Calf Reverb", editor: false );
	);
	s.sync;
	0.2.wait;
	// ~verbOrb2.readProgram("/home/skmecs/vst/orb2verb.fxp");
	" \n tal verb at orb 2 loaded \n ".postln;
	{ ~verbOrb2.gui }.defer;

	(
		~verbOrb3 = VSTPluginController(
			Synth(\insertVst, [ \buz, 3 ], ~dirt.orbits[3].group, addAction: \addAfter)
		).open( "Calf Reverb", editor: false );
	);
	s.sync;
	0.2.wait;
	// ~verbOrb3.readProgram("/home/skmecs/vst/orb3verb.fxp");
	" \n tal verb at orb 3 loaded \n ".postln;
	{ ~verbOrb3.gui }.defer;
	/*
    ~verbOrb3.editor;
    ~verbOrb3.close;
    */

" \n vst Reverbs loaded \n ".postln;
"\n VST FXs - LOADED \n ".postln;
}.fork;
