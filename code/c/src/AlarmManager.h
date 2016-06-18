/*
 * AlarmManager.h
 *
 */

#ifndef ALARM_MANAGER_H_INCLUDED
#define ALARM_MANAGER_H_INCLUDED

#include "Platform.h"
#include "MessageDialog.h"
#include "Constants.h"

/*
 * alarms levels, increasing in severity
 *
 */

typedef enum { 

	alarmOff = 0, 
	alarmInfo,
	alarmMessage, 
	alarmWarning, 
	alarmAlert 

} AlarmLevelType;

/*
 * AlarmSetCondition
 *
 * Call this function to set the specified alarm. Fill a MessageDialogDataType
 * structure with the information for the alarm: md->message is the alarm
 * 'key'.
 *
 * Set level = alarmOff to clear the alarm condition.
 * 
 * Note that the first button/action is simply to acknowledge the alarm, hence
 * only 3 user-defined ack/actions are available. All buttons cause
 * the alarm to be acknowledged.
 *
 * Note also that *md will be freed by the alarm manager when appropriate, so
 * make sure that the memory is allocated on the heap in the first place.
 *
 */

extern void AlarmSetCondition(/*@only@*/ MessageDialogDataType *md, AlarmLevelType level)
	ALARM_MANAGER_SECTION;
		

/*
 * AlarmShow
 *
 * Displays the next alarm in priority order, using the Message Dialog.
 *
 * Sets up the OK button so that it causes the next alarm to be automatically
 * displayed.
 *
 * Any alarms of class alarmMessage are cleared from the alarm list as they are
 * being displayed. Other alarms are left in the list, but acknowledged - in
 * this way alarmMessages can be repeated.
 *
 * Returns true if there was an alarm to display
 *
 */

extern Boolean AlarmShow(void) ALARM_MANAGER_SECTION;

/*
 * AlarmGetMaxLevel
 *
 * Returns the level of the highest alarm in the system
 *
 * This function should be called periodically to 'age' the alarms
 * and cause them to disappear when they're old
 *
 */

extern AlarmLevelType AlarmGetMaxLevel(void) ALARM_MANAGER_SECTION;

/*
 * AlarmGetMostUrgent
 *
 * The most urgent alarm is returned for 5 calls of this function, then the
 * next most urgent alarm for 5 calls etc.
 *
 * After 5 returns of the same alarm, that alarm is ack'd (or cleared, if it is
 * type alarmMessage)
 *
 * Returns the string description of the alarm. Do not free the memory!
 *
 */

extern /*@shared@*/ /*@null@*/ const char *AlarmGetMostUrgent(void) ALARM_MANAGER_SECTION;

/*
 * AlarmPurgeAll
 *
 * Purges all current alarms from the system
 *
 */

extern void AlarmPurgeAll(void) ALARM_MANAGER_SECTION;

#endif
