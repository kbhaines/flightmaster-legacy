#ifndef TIMERFORM_H
#define TIMERFORM_H

#include "Platform.h"
#include "Constants.h"

/*
 * timer information structure
 *
 */

typedef struct {

	Boolean countdown;		// true if timer counts down
	Boolean autoReset;		// true if timer should reset to initialValue

	Int32 seconds;			// current value of timer (seconds)
	Int32 initialValue;		// value to re-initialise timer to if autoreset is true

	enum { timerStopped, timerFlight, timerRunning } state;

	/*
	 * timerFlight - timer only runs if aircraft is moving at speed
	 *
	 */

	char label[8];

} TimerType;

#define NUMTIMERS 4


extern Boolean TimerFormHandleEvent(EventPtr eventP) TIMERFORM_SECTION;


#endif
