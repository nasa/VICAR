/*===========================================================================*
 | SFDUDATE -- Routine to convert SDR time format to SFDU time format.	     |
 |									     |
 | This routine is valid between 1950 and 2050.  The only obscure part of    |
 | the code is the calculation of leap years.  Starting from 0 A.D., leap    |
 | years are all years divisible by 4, except years divisible by 100 but not |
 | by 400.  Note that the year 2000 is a leap year.  To keep things simple,  |
 | this routine ignores the 100 and 400 year rules since they cancel out for |
 | the coming century.							     |
 | 									     |
 | The year 1900 is used as a base for calculating leap years, rather than   |
 | the year 0 to reduce the possibility of round-off errors.  The total      |
 | number of days elapsed from the year 1900 to the beginning of the current |
 | year is 365.25*year + 0.75.  The 0.75 adjusts for the century (1900)      |
 | starting on a leap year.  The total number of days elapsed since the year |
 | 1950 is								     |
 |	days = 365.25*year + 0.75         !days since 1900		     |
 |	           - 18263		  !days between 1900 and 1950        |
 | 		   + 0.125		  !half of 0.25 to reduce rnd-off err|
 | 									     |
 | See also subroutine SDRDATE.						     |
 *===========================================================================*/
#include "xvmaininc.h"
#include "ftnbridge.h"
#include "sfdutime.h"

/****************************************************************************/
/*  FORTRAN Callable Subroutine                                             */
/****************************************************************************/

void FTN_NAME2(sfdudate, SFDUDATE) (sfdut1,sfdut2,sdrtim,sdr_year)
unsigned int *sfdut1;
unsigned int *sfdut2;
short sdrtim[3];
int *sdr_year;			
{
     struct sfdu_time_int sfdut;
     struct sdr_time sdrt;
     unsigned int seconds_since_1950;
     unsigned short fractional_seconds;
     short hour_of_year, second_of_hour, msec_of_second;

     sdrt.hour_of_year = sdrtim[0];
     sdrt.second_of_hour = sdrtim[1];
     sdrt.msec_of_second = sdrtim[2];

     zsfdudate(&sfdut,&sdrt,sdr_year);

     *sfdut1 = sfdut.seconds_since_1950;
     *sfdut2 = sfdut.fractional_seconds;
}

/****************************************************************************/
/*  C Callable Subroutine                                                   */
/****************************************************************************/

zsfdudate(sfdut,sdrt,sdr_year)
struct sfdu_time_int *sfdut;	/* output Epoch 1950 time */
struct sdr_time  *sdrt;		/* input Julian day_of_year,sec_of_hour,msec*/
int *sdr_year;			/* input year of century (0-99) */

{
	int year,days;
	unsigned int frac_sec;
	float fsec;

	year = *sdr_year;
	if (year < 50) year += 100;		/* years since 1900 */
	days = 365.25*year - 18262.125;		/* days since 1950 */

	sfdut->seconds_since_1950 = 86400*days
		  + 3600*(sdrt->hour_of_year - 24)
		  + sdrt->second_of_hour;

	frac_sec = sdrt->msec_of_second;
	fsec = frac_sec<<16;
	sfdut->fractional_seconds = fsec/1000. + 0.5;
}
