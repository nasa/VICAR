/*===========================================================================*
 |  tzsfdudate.c -- Routine to test sfdudate.c                               |
 |									     |
 |  The test consists of calling sfdudate to convert from SDR to SFDU time   |
 |  format and printing the results.  Then sdrdate is called to invert the   |
 |  process and the result compared with the original date for consistency.  |
 |  If the result differs, an extra line is printed.			     |
 |									     |
 |  A gross check is made at the start and end of each year of the century,  |
 |  followed by a check through each day of the period surrounding the VGR   |
 |  Uranus encounter.							     |
 *===========================================================================*/
#include "vicmain_c"
#include "ftnbridge.h"
#include "sfdutime.h"

main44()
{
	struct sfdu_time_int sfdut;
	struct sdr_time sdrt;
	struct sdr_time sdrt2;
	int hr,i;
	int year,sdr_year,hour,second,msec;
	unsigned int ss_1950,frac_sec;
	char msg[80];

	zvmessage("Test the C interface"," ");
        zvmessage(" "," ");

	zvmessage("Gross check through entire century"," ");
        zvmessage(" "," ");

	for (i=0; i<3; i++)
		{
		if (i == 0) hr=0;		/* Jan 1, 00:00 hours */
		else if (i==1) hr=1;		/* Jan 1, 01:00 hours */
		else hr=8759;			/* Dec 31, 23:00 hours */
                zvmessage(" "," ");
		sprintf(msg,"Hour of year= %d",hr);
                zvmessage(msg," ");
                zvmessage(" "," ");

		zvmessage("ss_sec,fsec,year,hour,sec,msec"," ");
                zvmessage(" "," ");

		for (year=0; year<100; year++)
			{
			sdrt.hour_of_year = hr + 24;
			sdrt.second_of_hour = 3500;
			sdrt.msec_of_second = 890;
			zsfdudate(&sfdut,&sdrt,&year);
			zsdrdate(&sfdut,&sdrt2,&sdr_year);
			ss_1950 = sfdut.seconds_since_1950;
			frac_sec = sfdut.fractional_seconds;
			hour = sdrt.hour_of_year;
			second = sdrt.second_of_hour;
			msec = sdrt.msec_of_second;
			sprintf(msg,"%12d %12d %12d %12d %12d %12d",ss_1950,frac_sec,year,hour,second,msec);
                        zvmessage(msg," ");
			if (   (year   != sdr_year)
			    || (hour   != sdrt2.hour_of_year)
			    || (second != sdrt2.second_of_hour)
			    || (msec   != sdrt2.msec_of_second) )
				{
				hour = sdrt2.hour_of_year;
				second = sdrt2.second_of_hour;
				msec = sdrt2.msec_of_second;
			        sprintf(msg,"%12d %12d %12d %12d %12d %12d",ss_1950,frac_sec,sdr_year,hour,second,msec);
                         	zvmessage(msg," ");
				}
			}
		}

	zvmessage("Check through each day of VGR Uranus period"," ");
        zvmessage(" "," ");

	for (year=85; year<89; year++)
		{
		zvmessage(" "," ");
                sprintf(msg,"Year= %d",year);
        	zvmessage(msg," ");
		zvmessage(" "," ");

		for (hr=0; hr<8784; hr+=24)
			{
			if ((year != 88) && (hr == 8760)) break;
			sdrt.hour_of_year = hr + 24;
			sdrt.second_of_hour = 3500;
			sdrt.msec_of_second = 890;
			zsfdudate(&sfdut,&sdrt,&year);
			zsdrdate(&sfdut,&sdrt2,&sdr_year);
			ss_1950 = sfdut.seconds_since_1950;
			frac_sec = sfdut.fractional_seconds;
			hour = sdrt.hour_of_year;
			second = sdrt.second_of_hour;
			msec = sdrt.msec_of_second;
			sprintf(msg,"%12d %12d %12d %12d %12d %12d",ss_1950,frac_sec,year,hour,second,msec);
                        zvmessage(msg," ");
			if (   (year   != sdr_year)
			    || (hour   != sdrt2.hour_of_year)
			    || (second != sdrt2.second_of_hour)
			    || (msec   != sdrt2.msec_of_second) )
				{
				hour = sdrt2.hour_of_year;
				second = sdrt2.second_of_hour;
				msec = sdrt2.msec_of_second;
			        sprintf(msg,"%12d %12d %12d %12d %12d %12d",ss_1950,frac_sec,sdr_year,hour,second,msec);
                         	zvmessage(msg," ");
				}
			}

  		}

     zvmessage(" "," ");
     zvmessage("Test the Fortran interface"," ");
     zvmessage(" "," ");

     FTN_NAME(tsfdudate)();     
}	

