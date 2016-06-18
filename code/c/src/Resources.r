// Generated automatically from PilRC, decompiling the prc file "Resources.rcp"
// Do NOT change this file. Your changes will be lost
#include "UIResDefs.r"
type 'tSLT' {
	integer;										/* Button ID */
	integer;										/* Left */
	integer;										/* Top */
	integer;										/* Width */
	integer;										/* Height */
	byte			notUsable=0, usable=1;			/* Usable */
	fill byte;
	byte			rightAnchor=0, leftAnchor=1;	/* Left Anchor */
	fill byte;
	byte			palmFont;						/* Font ID */
	cstring;										/* Button Label */
};	/* end tSLT */


resource 'tAIN' (1000) {
	 "Starter"
};


resource 'Talt' (1001) {
	confirmationAlert,
	0, // help Rsc ID
	1, // # Buttons
	1, // Default Button Index
	 "System Incompatible",
	 "System Version 3.0 or greater is required to run this application.",
	{
		 "OK",
	}
};

resource 'tTTL' (30000) {
	 "OReilly Starter Main"
};

resource 'tBTN' (2001) {
	2001, // controlID
	40, // left
	100, // top
	32, // width
	12, // height
	usable,
	leftAnchor,
	frame,
	nonBoldFrame,
	stdFont, // font ID
	 "Beep"
};

resource 'tBTN' (2002) {
	2002, // controlID
	40, // left
	117, // top
	86, // width
	12, // height
	usable,
	leftAnchor,
	frame,
	nonBoldFrame,
	stdFont, // font ID
	 "Goto Second Form"
};


resource 'tFRM' (2000) {
	0, // left
	0, // top
	160,  // width
	160, // height
	usable,
	notModal,
	saveBehind,
	2000, // formID
	0, // helpID
	0, // menuRsrcID
	0, // default button ID
	{
		30000, "tTTL";
		2001, "tBTN";
		2002, "tBTN";
	}
};

resource 'tTTL' (30001) {
	 "OReilly Starter Second"
};

resource 'tBTN' (3001) {
	3001, // controlID
	40, // left
	130, // top
	77, // width
	12, // height
	usable,
	leftAnchor,
	frame,
	nonBoldFrame,
	stdFont, // font ID
	 "Goto Main Form"
};


resource 'tFRM' (3000) {
	0, // left
	0, // top
	160,  // width
	160, // height
	usable,
	notModal,
	saveBehind,
	3000, // formID
	0, // helpID
	0, // menuRsrcID
	0, // default button ID
	{
		30001, "tTTL";
		3001, "tBTN";
	}
};