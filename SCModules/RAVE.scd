(
	currentEnvironment.put(\modelsPath, "/Users/khoparzi/Dev/pretrained_models/");
	NN.load(\percussion, ~modelsPath++"percussion.ts");
	OSCdef.new(\loadModel, {
		arg msg;
		postf("% : %\n", msg[0], msg[1]);
		case
			{msg[1].asString == "strengj"} {
			    NN.load(msg[1].asSymbol, ~modelsPath++"mrp_strengjavera_b2048_r44100_z16.ts");
			}
			{msg[1].asString == "nasa"} {
			    NN.load(msg[1].asSymbol, ~modelsPath++"nasa.ts", action: _.describe);
			}
			{msg[1].asString == "percussion"} {
			    NN.load(msg[1].asSymbol, ~modelsPath++"percussion.ts");
			}
			{msg[1].asString == "dnb"} {
			    NN.load(msg[1].asSymbol, ~modelsPath++"dnb-embedded2.1m.ts");
			}
			{msg[1].asString == "meta"} {
			    NN.load(msg[1].asSymbol, ~modelsPath++"meta-v2.ts");
			}
			{msg[1].asString == "cinema-textures"} {
			    NN.load(msg[1].asSymbol, ~modelsPath++"cinema-textures-v3_1.45m.ts");
			}
			{msg[1].asString == "guitar"} {
			    NN.load(msg[1].asSymbol, ~modelsPath++"guitar_iil_b2048_r48000_z16.ts");
			}
			{msg[1].asString == "sax"} {
			    NN.load(msg[1].asSymbol, ~modelsPath++"sax_soprano_franziskaschroeder_b2048_r48000_z20.ts");
			}
			{msg[1].asString == "bhagwat"} {
			    NN.load(msg[1].asSymbol, ~modelsPath++"bhagwat-v2_1m.ts");
			}
		}, '/loadModel', nil, 57120);
);