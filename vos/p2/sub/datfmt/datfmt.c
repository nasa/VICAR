#define	JAN	0
#define	FEB	31
#define	MAR	28
#define	APR	31
#define	MAY	30
#define	JUN	31
#define	JUL	30
#define	AUG	31
#define	SEP	31
#define	OCT	30
#define	NOV	31
#define	DEC	30
#define	MAR1	29

#include "xvmaininc.h"
#include "ftnbridge.h"
#include <string.h>
#include <math.h>
#include "date_check.h"
#include <stdlib.h>
#include <stdio.h>

/**************************************************************************

This routine will return the date in various formats depending on the format
chosen.  The date may be returned as an ascii string or an int value or both.

input:  If flag = 1 - Format is YYDDD (YY=YEAR, DDD=DAY OF YEAR)
		      both ASCII and int.

	If flag = 2 - Format is 'WED MAY 11, 1983' 
        	      Only ASCII is returned (int part = 0)

Output: 	  A - ASCII string date format
		  I - Integer value (For flag = 1 only)


     Modifications:   7-15-93 (ddk) ported to Unix and converted to C.
     Modifications:   3-25-94 (ddk) fix sc2for function for Fortran 
                                    string so that it would run under 
                                    Solaris. (FR 85185) 

     Jun. 30, 1998 ...T. Huang... Modified return date integer to have
                                  four-digit year number.
                                  Modified output format of test program.
                                  Modified to use 'date_check' subroutine
                                  to check for leap year.
     Aug. 30, 2000 ... AXC ...    Modified condition statement to conform
                                  to the number of elements in array
                                  'day_name' for date format FLAG=2.
                                  Modifed .hlp to reflect the correct
                                  format flag. (AR-104622)

***************************************************************************/

void zdatfmt();

/**************************************************************************/
/*    FORTRAN-Callable Subroutine                                         */
/**************************************************************************/

void FTN_NAME2(datfmt, DATFMT) (short int *flag, char *A, int *I, ZFORSTR_PARAM)
{
     ZFORSTR_BLOCK
     char c_string[20];
     int I_temp;
    
     zdatfmt(*flag,c_string,&I_temp);

     *I = I_temp;

     zsc2for(c_string,20,A,&flag,3,2,1, I);
}


/**************************************************************************/
/*    C-Callable Subroutine                                               */
/**************************************************************************/

void zdatfmt(flag, A, I)
   short int flag;
   char *A;
   int *I;
{
     long time(), t;
     int month, i, leap, year, day;
     char *date_time, *ctime(), charnum[1], aday[3], amonth[3], ayear[4];
     char A_temp[20];

     static short int day_tab[2] [12] =
     {{JAN,
       JAN+FEB,
       JAN+FEB+MAR,
       JAN+FEB+MAR+APR,
       JAN+FEB+MAR+APR+MAY,
       JAN+FEB+MAR+APR+MAY+JUN,
       JAN+FEB+MAR+APR+MAY+JUN+JUL,
       JAN+FEB+MAR+APR+MAY+JUN+JUL+AUG,
       JAN+FEB+MAR+APR+MAY+JUN+JUL+AUG+SEP,
       JAN+FEB+MAR+APR+MAY+JUN+JUL+AUG+SEP+OCT,
       JAN+FEB+MAR+APR+MAY+JUN+JUL+AUG+SEP+OCT+NOV,
       JAN+FEB+MAR+APR+MAY+JUN+JUL+AUG+SEP+OCT+NOV+DEC},
      {JAN,
       JAN+FEB,
       JAN+FEB+MAR1,
       JAN+FEB+MAR1+APR,
       JAN+FEB+MAR1+APR+MAY,
       JAN+FEB+MAR1+APR+MAY+JUN,
       JAN+FEB+MAR1+APR+MAY+JUN+JUL,
       JAN+FEB+MAR1+APR+MAY+JUN+JUL+AUG,
       JAN+FEB+MAR1+APR+MAY+JUN+JUL+AUG+SEP,
       JAN+FEB+MAR1+APR+MAY+JUN+JUL+AUG+SEP+OCT,
       JAN+FEB+MAR1+APR+MAY+JUN+JUL+AUG+SEP+OCT+NOV,
       JAN+FEB+MAR1+APR+MAY+JUN+JUL+AUG+SEP+OCT+NOV+DEC},
      };          

     static char *mon_name[] = 
     {
         "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
              "Nov", "Dec"
     };

     static char *mon_caps_name[] = 
     {
         "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT",
              "NOV", "DEC"
     };

     static char *day_name[] = 
     {
         "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"
     };

     static char *day_caps_name[] = 
     {
         "MON", "TUE", "WED", "THU", "FRI", "SAT", "SUN"
     };

     if (flag == 1)
     {
          t=time(0);
          date_time=ctime(&t);

          strcpy(A_temp," ");

          strncpy(ayear, (date_time+20), 4);
          strncpy(amonth, (date_time+4), 3);
          strncpy(aday, (date_time+8), 3);               
          
          year=atoi(ayear);
          day=atoi(aday);

          month = -1;
          for(i=0; i<12; i++)
          {
               if(strncmp(amonth, mon_name[i], 3) == 0)
               {
                    month = i;
                    break;
               }

          }

          leap = zchk_leap (year);

          day = day_tab[leap][month] + day;

          strncat(A_temp, (date_time+20), 4);

          day = day + (atoi(A_temp)*1000);
   
          *I = day;

          sprintf(A_temp,"%d",day);

          strcpy(A,A_temp);

     }

     else if (flag == 2)
     {
          *I=0;

          t=time(0);
          date_time=ctime(&t);

          strcpy(A," ");
  
          for (i=0; i<7; i++)
          {
               if (strncmp(day_name[i], date_time, 3) == 0)
               {
                    strcat(A, day_caps_name[i]);
               }
          }

          strcat(A, " ");

          for (i=0; i<12; i++)
          {
               if (strncmp(mon_name[i], (date_time+4), 3) == 0)
               {
                    strcat(A, mon_caps_name[i]);
               }
          }

          strcat(A, " ");

          strncpy(charnum, (date_time+8), 1);

          if ((strcmp(charnum,"0")) != 0) strncat(A, (date_time+8), 2);
     
          else if ((strcmp(charnum,"0")) == 0) strncat(A, (date_time+9), 1);

          strcat(A, ", ");
          strncat(A, (date_time+20), 4);
          
          return;
     }
}
