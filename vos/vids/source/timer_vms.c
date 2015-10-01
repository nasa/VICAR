#include "VIDSdefs.h"
#include <timeb>			/* VAX/VMS timing stuff		*/
/* *************  Global variables **************************************/
struct timeb	savedTime;
/************************************************************************/
/* StartTimer saves the current time for checking by the other time 
 * functions, such as TimeElapsed.
 */
StartTimer()
{
  ftime(&savedTime);
  return;
}
/************************************************************************/
/* TimeElapsed returns True if "secs" seconds have passed, False otherwise.
 */
Boolean TimeElapsed(secs)
  float		secs;			/* number of seconds		*/
{
  struct timeb	curTime;
  float		elapsed;
  long		diff,millidiff;
  
  ftime(&curTime);
  diff = curTime.time - savedTime.time;
  millidiff = curTime.millitm - savedTime.millitm;
  if (millidiff < 0)
  {
    diff--;
    millidiff += 1000;
  }
  elapsed = (float) diff + ((float) millidiff / 1000.0);
  if (elapsed > secs) return True;
  return False;
}
/************************************************************************/
WaitElapsed(time)
  float time;
{
  Boolean TimeElapsed();

  while (!TimeElapsed(time));
  return;
}  
/************************************************************************/
