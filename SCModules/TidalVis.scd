// ~clockText.string_("BOOM")
// ~clockText.font = Font("Monaco", 24);

// Inspired by Pulu's code from TidalClub
// start superdirt first
(
var clockMods, clockBeats, screenW, screenH, clockW, clockH, clockX, clockY, resizable, border;
clockMods = [4,6];
clockBeats = 4;
screenW = 1440;
screenH = 900;
clockW = 200;
clockH = 22;
clockX = screenW - clockW;
clockY = screenH - 1;
resizable = false;
border = false;

"\n\nConnecting to Tidal Visuals".postln;

~clockText = StaticText()
.string_("[clock]")
// .font_(Font.defaultMonoFace)
.font_(Font("Monaco", 24))
.align_(\center)
.stringColor_(Color(1,1,1))
.minHeight_(20);

~updateClock = { |cycle|
    var text, beat;
    text = clockMods.collect { |m| "" ++ (cycle.floor.asInteger.mod(m) + 1) ++ "/" ++ m; }.join(" ");
    beat = (cycle.mod(1)*clockBeats).round.asInteger + 1;
    text = text ++ " " ++ clockBeats.collect { |i| if(i < beat, ".", " "); }.join;
    ~clockText.string_(text);
};

~clockWindow = Window("clock", Rect(clockX, clockY, clockW, clockH), resizable, border)
.background_(Color(0.3,0.3,0.3))
.layout_(
    HLayout(
        ~clockText
    ).margins_(0!4)
);

~clockWindow.alwaysOnTop_(true);
~clockWindow.visible_(true);

SynthDef(\tick, { |cycle|
    SendReply.kr(Impulse.kr(0), "/tick", [cycle]);
    FreeSelf.kr(Impulse.kr(0));
}).add;

OSCdef(\tick, { |msg|
    var cycle;
    #cycle = msg[3..];
    Routine {
        { ~updateClock.(cycle); }.defer;
    }.play(SystemClock);
}, "/tick");
)
