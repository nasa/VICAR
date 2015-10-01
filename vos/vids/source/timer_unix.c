#include "VIDSdefs.h"
#include <sys/time.h>
/* *************  Global variables **************************************/
struct timeval savedTime;
struct timezone tz;
/************************************************************************/
/* StartTimer saves the current time for checking by the other time 
 * functions, such as TimeElapsed.
 */
StartTimer()
{
  gettimeofday(&savedTime, &tz);
  return;
}
/************************************************************************/
/* TimeElapsed returns True if "secs" seconds have passed, False otherwise.
 */
Boolean TimeElapsed(secs)
  float		secs;			/* number of seconds		*/
{
  struct timeval	curTime;
  float		elapsed;
  long		diff,microdiff;
  
  gettimeofday(&curTime, &tz);
  diff = curTime.tv_sec - savedTime.tv_sec;
  microdiff = curTime.tv_usec - savedTime.tv_usec;
  if (microdiff < 0)
  {
    diff--;
    microdiff += 1000000;
  }
  elapsed = (float) diff + ((float) microdiff / 1000000.0);
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
