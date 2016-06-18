
#ifndef LOGGING_H_INCLUDED
#define LOGGING_H_INCLUDED

/*
 * Logging functions
 *
 */

/*
 * should only be called from Platform.c
 *
 */

extern void LogOpen(void);
extern void LogClose(void);

/*
 * log entry to / exit from a function
 *
 */


extern void LogEntry(const char *fname);
extern void LogExit(const char *fname,UInt16 line);

/*
 * line-level trace
 *
 */

extern void LogLine(UInt16 lineNo);
extern void LogTag(const char *tag);

/*
 * variable trace
 *
 */

extern void LogInt16(const char *name, Int16 val);
extern void LogInt32(const char *name, Int32 val);
extern void LogUInt32(const char *name, UInt32 val);

extern void LogDouble(const char *name, double val);
extern void LogString(const char *name, const char *s);

#ifdef LOGGING
#define LOGINT32(v) LogInt32(#v, v)
#define LOGTAG(s) LogTag(s)
#define LOGTIMESTART { UInt32 loggingstart = PFGetTicks();
#define LOGTIMESTOP LOGTAG("Timed:");LOGINT32(PFGetTicks()-loggingstart); }
#endif

#ifdef LOGGING

#define LOGSTR(s) LogString(#s, s)
#define LOGUINT32(v) LogUInt32(#v, v)
#define LOGINT16(v) LogInt16(#v, v)
#define LOGFLOAT(v) LogDouble(#v,(double)v)
#define LOGDOUBLE(v) LogDouble(#v, v)
#define LOGLINE LogLine(__LINE__)
#define LOGEXIT  LogExit(__FUNCTION__,__LINE__)
#define LOGENTRY LogEntry(__FUNCTION__)


#else

#define LOGENTRY
#define LOGEXIT
#define LOGLINE
#define LOGTAG(s)
#define LOGINT16(v)
#define LOGINT32(v)
#define LOGFLOAT(v)
#define LOGDOUBLE(v)
#define LOGUINT32(v)
#define LOGSTR(s)
#define LOGTIMESTART
#define LOGTIMESTOP

#endif


/*
 * Tracing functions
 *
 */

#ifdef TRACETOOLS
#define DUMP_INT16(i,x,y)  { char tmp[20];StrPrintF(tmp,"%d",i);WinDrawChars(tmp,StrLen(tmp),x,y); }
#define DUMP_INT32(i,x,y)  { char tmp[20];StrPrintF(tmp,"%ld",i);WinDrawChars(tmp,StrLen(tmp),x,y); }
#define DUMP_STR(s,x,y)  { char tmp[20];StrPrintF(tmp,"%s",s);WinDrawChars(tmp,StrLen(tmp),x,y); }
#define FTRACE { FontID f; char tmp[40]; StrPrintF(tmp,"%s-%d",__FUNCTION__,__LINE__); f = FntSetFont(stdFont); WinDrawChars(tmp,StrLen(tmp),0,0); FntSetFont(f); }
#define CTRACE(n) { static UInt32 count=0;FontID f; char tmp[40]; StrPrintF(tmp,"%s-%d-%ld",__FUNCTION__,__LINE__,count++); f = FntSetFont(stdFont); WinDrawChars(tmp,StrLen(tmp),0,n); FntSetFont(f); }
#define TRACE(s) { FontID f; char tmp[40]; StrPrintF(tmp,"%s-%d",s,__LINE__); f = FntSetFont(stdFont); WinDrawChars(tmp,StrLen(tmp),0,0); FntSetFont(f); }

#define TIME_START { UInt32 start = PFGetTicks();
#define TIME_STOP(x,y) DUMP_INT32((PFGetTicks()-start),(x),(y)); }

#else

#define FTRACE
#define TRACE(s) 
#define CTRACE(n) 
#define DUMP_INT16(i,x,y)
#define DUMP_INT32(i,x,y)
#define DUMP_STR(s,x,y)
#define TIME_START 
#define TIME_STOP(x,y) 

#endif

#endif
