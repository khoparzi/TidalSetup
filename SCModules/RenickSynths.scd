({
	// Drums
    (~renickPath++"/drums/kick/ek.scd").load;
    (~renickPath++"/drums/kick/gabberkick.scd").load;
    (~renickPath++"/drums/hh/soshats.scd").load;
    (~renickPath++"/drums/snare/sossnare.scd").load;
    (~renickPath++"/drums/cymbal/cymbal808.scd").load;
    (~renickPath++"/drums/bells/sosbell.scd").load;
    (~renickPath++"/drums/perc/kalimba.scd").load;
    (~renickPath++"/drums/kick/k1.scd").load;
    (~renickPath++"/drums/kick/k2.scd").load;
    (~renickPath++"/drums/kick/k3.scd").load;
    (~renickPath++"/drums/kick/kick808.scd").load;
    (~renickPath++"/drums/kick/kickOto309.scd").load;
    (~renickPath++"/drums/kick/soskick.scd").load;
    "\n\nLoaded Renick Drums".postln;
    0.2.wait;
    // Bass
    (~renickPath++"/bass/moogbass.scd").load;
    (~renickPath++"/bass/pluckbass.scd").load;
    (~renickPath++"/bass/ikedaBass.scd").load;
    // (~renickPath++"/bass/bassWarsaw.scd").load;
    (~renickPath++"/bass/acidOto3091.scd").load;
    (~renickPath++"/bass/acidOto3092.scd").load;
    "\n\nLoaded Renick Basses".postln;
    0.2.wait;
    // Leads
    (~renickPath++"/leads/cs80.scd").load;
    (~renickPath++"/leads/fm7.scd").load;
    "\n\nLoaded Renick synths".postln;
}.fork;)
