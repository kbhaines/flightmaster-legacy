/*
 * AlarmManager.c
 *
 */

#include "Platform.h"
#include "GlobalTypes.h"
#include "Modules.h"
#include "ResourceDefines.h"
#include "Constants.h"
#include "AlarmManager.h"
#include "MessageDialog.h"
#include "FMStrings.h"

/*******************************************************************************
 *
 * module variables
 *
 */

#define MAX_ALARMS 16
#define MAX_RETURNS 10

static Int16 numAlarms = 0;
struct {

	AlarmLevelType level;
	MessageDialogDataType *data;
	Boolean ack;

	UInt32 time;
	UInt16 returnedCounter;

} alarm[MAX_ALARMS];

/*******************************************************************************
 *
 * private functions
 *
 */

#define ModuleID AlarmManagerModuleID


//PRV:

static void ClearAlarm(Int16 num) ALARM_MANAGER_SECTION;
static Int16 GetHighestPriorityAlarm(Boolean checkReturnedCounter) ALARM_MANAGER_SECTION;
static Boolean AckAlarm(Int16 alarmID) ALARM_MANAGER_SECTION;

/*
 * ClearAlarm
 *
 * Clears the specified alarm, removing it from the alarms 
 * array and shuffling the remaining alarms down to fill the gap
 *
 */

static void ClearAlarm(Int16 num) {

	Int16 j;

	MessageDialogDataFree(alarm[num].data);

	numAlarms--;
	for (j=num; j < numAlarms; j++) alarm[j] = alarm[j+1];

}

/*
 * GetHighestPriorityAlarm
 *
 * Returns ID of the highest priority alarm. If checkReturnedCounter is true
 * then it takes into account the value of returnedCounter.
 *
 */

static Int16 GetHighestPriorityAlarm(Boolean checkReturnedCounter) {

	Int16 j;
	Int16 highestLevel= alarmOff;
	Int16 alarmToShow = -1;

	/*
	 * locate the oldest, highest-priority alarm
	 *
	 */

	if (!numAlarms) return -1;

	for (j = 0; j<numAlarms; j++) {

		if (!alarm[j].ack && alarm[j].level > highestLevel) {

			if (!checkReturnedCounter || alarm[j].returnedCounter) {

				highestLevel = alarm[j].level;
				alarmToShow = j;

			}

		}

	}

	return alarmToShow;

}

/*
 * AckAlarm
 *
 * Returns true if the ack'd alarm was also removed
 */

static Boolean AckAlarm(Int16 alarmID) {

	alarm[alarmID].ack = true;
	if (alarm[alarmID].level == alarmMessage || alarm[alarmID].level == alarmInfo) {

		ClearAlarm(alarmID);
		return true;

	}

	return false;

}
		
/*******************************************************************************
 *
 * public functions
 *
 */

//PUB:


/*
 * AlarmSetCondition
 *
 */

void AlarmSetCondition(MessageDialogDataType *md, AlarmLevelType level) {

	Int16 j;

	/*
	 * check existing alarms 
	 *
	 */

	LOGENTRY;

	for (j=0; j< numAlarms; j++) {

		if (StrCompare(md->message, alarm[j].data->message) == 0) {

			/*
			 * found the same alarm, check if it's being cleared or if we should
			 * increase the alarm level 
			 *
			 */

			LOGTAG("Found existing alarm");
			LOGSTR(alarm[j].data->message);

			if (level == alarmOff) {

				/*
				 * clear the alarm
				 *
				 */

				ClearAlarm(j);
				MessageDialogDataFree(md);
				LOGEXIT;
				return;

			}

			/*
			 * increase alarm level?
			 *
			 */

			if (alarm[j].level < level) {
				
				alarm[j].level = level;
				alarm[j].time = PFGetSeconds();
				alarm[j].returnedCounter = MAX_RETURNS;

			}

			MessageDialogDataFree(md);
			LOGEXIT;
			return;

		}

	}

	/*
	 * the code above should have removed the existing alarm condition 
	 * if level == alarmOff, so just return from here
	 *
	 */

	if (level == alarmOff) {

		MessageDialogDataFree(md);
		LOGEXIT;
		return;

	}

	/*
	 * new alarm
	 *
	 */

	ModErrThrowIf(numAlarms == MAX_ALARMS);
	alarm[numAlarms].level = level;
	alarm[numAlarms].data  = md;
	alarm[numAlarms].ack = false;
	alarm[numAlarms].time = PFGetSeconds();
	alarm[numAlarms].returnedCounter = MAX_RETURNS;

	numAlarms++;

	LOGEXIT;

}
		

/*
 * AlarmShow
 *
 */

Boolean AlarmShow(void) {

	Int16 j;
	MessageDialogDataType *md, *alarmMd;
	Int16 alarmToShow = GetHighestPriorityAlarm(false);

	LOGENTRY;

	if (alarmToShow == -1) {

		LOGEXIT;
		return false;

	}

	/*
	 * set up a new MessageDialogDataType structure, copy in
	 * details from message we want to display
	 *
	 */

	alarmMd = alarm[alarmToShow].data;

	md = MessageDialogDataNew(alarmMd->message,0,NULL,0,NULL,0,NULL,0,NULL,0);
	ModErrThrowIf(!md);

	/*
	 * set up the message structure; the first button
	 * is the OK/ack button and the rest of the buttons come
	 * from the alarm's data
	 *
	 */

	md->string[0] = OKString;
	md->action[0] = MnShowAlarm;

	for (j=0; j<alarmMd->numItems; j++) {
		
		md->string[j+1] = alarmMd->string[j];
		md->action[j+1] = alarmMd->action[j];

	}
	md->numItems = alarmMd->numItems + 1;
	
	MessageDialogInit(md);
	GUIFormPopup(MessageDialog);

	AckAlarm(alarmToShow);

	LOGEXIT;
	return true;
	
}

/*
 * AlarmGetMaxLevel
 *
 */

AlarmLevelType AlarmGetMaxLevel(void) {

	Int16 j;
	AlarmLevelType level = alarmOff;
	UInt32 now = PFGetSeconds();

	LOGENTRY;
	j=0;
	while (j<numAlarms) {

		/*
		 * check the age of the alarm and auto-ack it
		 *
		 */

		Int16 interval = now - alarm[j].time;
		
		if ((alarm[j].level > alarmInfo && interval > 30) || 
				(alarm[j].level == alarmInfo && interval > 2) ) { 
			
			/*
			 * only increment j if alarm was *not* cleared (i.e. only ack'd)
			 *
			 * (if it was cleared, then it was also deleted!)
			 */

			if (!AckAlarm(j)) j++;

		} else {

			if (!alarm[j].ack && alarm[j].level > level) level = alarm[j].level;

			j++;

		}

	}

	LOGEXIT;
	return level;

}

/*
 * AlarmGetMostUrgent
 *
 */

const char *AlarmGetMostUrgent(void) {

	Int16 alarmID = GetHighestPriorityAlarm(true);

	if (alarmID != -1 && alarm[alarmID].returnedCounter) {

		if ( --alarm[alarmID].returnedCounter == 0) {

			/*
			 * an alarm that has reached zero on the return counter
			 * is assumed to have been ack'd
			 *
			 */

			// AckAlarm(alarmID);

		}

		return alarm[alarmID].data->message;

	}

	return NULL;

}

/*
 * AlarmPurgeAll
 *
 */

void AlarmPurgeAll(void) {

	Int16 j;
	Int16 stopAt = numAlarms;

	for (j=0; j<stopAt; j++)

		ClearAlarm(0);

}

