/*
 * C logger
 */
extern void put_clog_message(char *filename, int linenumber, char *message, int value);
#define DBGMSG(msg,val)  put_clog_message(__FILE__, __LINE__, msg, (val))
#define VALUE(val)       DBGMSG(#val,val)
