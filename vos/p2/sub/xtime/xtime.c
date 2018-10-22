#include "xvmaininc.h"
#include "ftnbridge.h"
#include <string.h>
#include <time.h>

/**************************************************************************

This routine returns the current time in the format "HHMMSS".

     Input args: NONE

     Output args: time_now - Pointer to the variable to hold the time string

     Returned status: none

     Modifications:   3-6-95 AMS(CRI) MSTP S/W CONVERSION (VICAR PORTING)

***************************************************************************/

/**************************************************************************/
/*    FORTRAN-Callable Subroutine                                         */
/**************************************************************************/

void FTN_NAME2(xtime, XTIME) (char *time_now, ZFORSTR_PARAM)
{
     ZFORSTR_BLOCK
     unsigned char c_string[6];

     zxtime(c_string);
     zsc2for((char *)c_string,6,time_now,&time_now,1,1,1, time_now);
}

/**************************************************************************/
/*    C-Callable Subroutine                                               */
/**************************************************************************/

zxtime(time_now)

char *time_now;

{
     time_t time(), t;
     char *temp_time, *ctime();

     t=time(0);
     temp_time=ctime(&t);
     strncpy(time_now, (temp_time + 11), 2);
     strncpy((time_now + 2), (temp_time + 14), 2);
     strncpy((time_now + 4), (temp_time + 17), 2);
     *(time_now + 6) = '\0';

     return;

}
