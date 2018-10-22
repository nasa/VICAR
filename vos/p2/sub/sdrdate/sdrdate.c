/*===========================================================================*
 | SDRDATE -- Routine to convert from SFDU time format to SDR time format    |
 |                                                                           |
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
 *===========================================================================*/
#include "xvmaininc.h"
#include "ftnbridge.h"
#include "sfdutime.h"

/*****************************************************************************/
/*   FORTRAN Callable Subroutine                                             */
/*****************************************************************************/

void FTN_NAME2(sdrdate, SDRDATE) (sfdut1,sfdut2,sdrtim,sdr_year)
unsigned int *sfdut1;
unsigned int *sfdut2;
short sdrtim[3];
int sdr_year;
{
     struct sfdu_time_int sfdut;
     struct sdr_time sdrt;

     sfdut.seconds_since_1950 = *sfdut1;
     sfdut.fractional_seconds = *sfdut2;

     zsdrdate(&sfdut,&sdrt,sdr_year);

     sdrtim[0] = sdrt.hour_of_year;
     sdrtim[1] = sdrt.second_of_hour;
     sdrtim[2] = sdrt.msec_of_second; 

}     

/*****************************************************************************/
/*   C Callable Subroutine                                                   */
/*****************************************************************************/

zsdrdate(sfdut,sdrt,sdr_year)
struct sfdu_time_int *sfdut;	/* input SFDU date & time */
struct sdr_time  *sdrt;		/* output SDR date & time */
int *sdr_year;			/* output year of century */
{
	unsigned int secs;		/* seconds since 1950 */
	unsigned int frac_secs;
	int days,years;

	secs = sfdut->seconds_since_1950;
	frac_secs = sfdut->fractional_seconds;
	days = secs/86400;			/* days since 1950 */
	years = (days+0.625)/365.25;		/* years since 1950 */
	days = years*365.25 + 0.375;	/* days from 1950 to current year*/
	secs -= 86400*days;
	*sdr_year = (years+50)%100;
	sdrt->hour_of_year = secs/3600 + 24;
	sdrt->second_of_hour = secs%3600;
	sdrt->msec_of_second = (1000*frac_secs + 500)>>16;
}
