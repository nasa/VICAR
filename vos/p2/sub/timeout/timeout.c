/*===========================================================================*
 | TIMEOUT.C  -- Routine to generate an ASCII string of the form:	     |
 |			"YYYY.DAY HH:MM:SS.MSC"				     |
 |		 given year, day, hour, minutes, seconds, and milliseconds   |
 |		 as inputs.						     |
 *===========================================================================*/

#include <stdio.h>
#include <string.h>

timeout(date_buf,string)
int date_buf[6];	/* input buffer containing year,day,hour,etc..*/
char string[25];	/* output ASCII string */
{
	int year,day,hour,minute,second,msec;

	day = year = date_buf[0];
        if(!(year/1000)) { /* year not four digit */
          if(year<=20) year+=2000;
          else year+=1900;
          sprintf(string,"Year=%d changed to %d.",day,year);
          zvmessage(string,0);
        }
	day = date_buf[1];
	hour = date_buf[2];
	minute = date_buf[3];
	second = date_buf[4];
	msec = date_buf[5];

	sprintf(string,"%04d.%03d %02d:%02d:%02d.%03d",year,day,hour,minute,second,msec);

	strcat(string,"\n");
}
