(
	~instrumentList = Dictionary [
		\fm8 -> "FM8",
		\massiveX -> "Massive X",
		\massive -> "Massive",
		\battery -> "Battery 4",
		\reaktor -> "Reaktor 6",
		\kontakt -> "Kontakt",
		\pigments -> "Pigments"
	];
	if (~vstInstruments.value.isNil, {
			~vstInstruments = Dictionary.new;
			SynthDef(\vsti, { | out, pan = 0, n = 0, gate = 1, buz |
				var sound;
				var env = EnvGen.kr(Env.asr(0.1, 1, 0.1), gate: 1, doneAction:2);
				sound = In.ar(out, ~dirt.numChannels);
				sound = VSTPlugin.ar( sound , ~dirt.numChannels );
				Out.ar(out,
					DirtPan.ar(sound * env, ~dirt.numChannels, pan)
				)
			}).add;
		});
	OSCdef.new(\vstLoad, {
		arg msg;
		{
			~vstInstruments.add(msg[1].asSymbol -> VSTPluginController(Synth(\vsti)).open( ~instrumentList.at(msg[1].asSymbol), editor: true ));
			s.sync;
			0.2.wait;
			~dirt.soundLibrary.addMIDI(msg[1].asSymbol, ~vstInstruments.at(msg[1].asSymbol).midi);
		}.fork;
	}, '/vstLoad', nil, 57120);
	OSCdef.new(\vstEditor, {
		arg msg;
		// Does something like
		// ~vstInstruments.at(\fm8).editor;
		~vstInstruments.at(msg[1].asSymbol).editor;
	}, '/vstEditor', nil, 57120);
	OSCdef.new(\vstGui, {
		arg msg;
		// Does something like
		~vstInstruments.at(msg[1].asSymbol).gui;
	}, '/vstGui', nil, 57120);
	OSCdef.new(\vstSave, {
		arg msg;
		var saveName = msg[1].asString().split($\:);
		var newSaveName = ~setupPath +/+ "presets/" ++ saveName[0] ++ "-" ++ saveName[1] ++ ".vstpreset";
		~vstInstruments.at(saveName[0].asSymbol).writeProgram(newSaveName); // For writing preset
		// ~massiveX.writeProgram(~setupPath +/+ "presets/AldenteHollywood.vstpreset"); // For writing preset
		// ~vstInstruments.at(\massiveX).writeProgram(~setupPath +/+ "presets/massiveX-AldenteHollywood.vstpreset"); // For writing preset
	}, '/vstSave', nil, 57120);
	OSCdef.new(\vstRead, {
		arg msg;
		var saveName = msg[1].asString().split($\:);
		var newSaveName = ~setupPath +/+ "presets/" ++ saveName[0] ++ "-" ++ saveName[1] ++ ".vstpreset";
		~vstInstruments.at(saveName[0].asSymbol).readProgram(newSaveName); // For writing preset
	}, '/vstRead', nil, 57120);
	)
