/* Code specific to Lightspeed C on MacIntosh.
   This detects that the character APPLE-DOT is depressed,
   and then expects that sending a newline to the console
   will invoke the proper signal handling code. 

   See the file "THINK C 5.0 FOLDER/C LIBRARIES/SOURCES/CONSOLE.C"

   It would be a good thing to have some code in here that would call
   the proper inside-mac OS routines to determine allowable machine
   stack size, because of lack of protection against stack
   overflow bashing another program.

 */
  

#include <stdio.h>
#include <console.h>

#include <MacHeaders>

static int interrupt_key_down(void);
void full_interrupt_poll(int *counter);

void full_interrupt_poll(int *counter)
{SystemTask();
 if (interrupt_key_down())
     putc('\n',stdout);
  /* 200 seems to be a good compromise here between
     interrupt latency and cpu-bound performance */   
 *counter = 200;}

static int interrupt_key_down(void)
{EvQElPtr l;
 for(l = (EvQElPtr) EventQueue.qHead; l; l = (EvQElPtr) l->qLink)
   if ((l->evtQWhat == keyDown) &&
       ((char) l->evtQMessage == '.') &&
       (l->evtQModifiers & cmdKey))
     return(1);
 return(0);}
