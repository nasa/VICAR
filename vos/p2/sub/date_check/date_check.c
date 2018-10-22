#include <stdio.h>
#include "xvmaininc.h"
#include "date_check.h"
#include "ftnbridge.h"
#include <zvproto.h>

/******************************************************************************

 see date_check.hlp for documentation

 March 23, 1998 ...T. Huang... Initial release.
******************************************************************************/


/************************** FORTRAN Bridges *********************************/

/* verify year number is in YYYY format */
void FTN_NAME2_(chk_year, CHK_YEAR) (int *year, int *status)
{
   *status = zchk_year(*year);
} /* end chk_year fortran bridge */


/* verify if the supplied number is within a given range */
void FTN_NAME2_(chk_range, CHK_RANGE) (int *min, int *max, int *number, int *status)
{
   *status = zchk_range(*min, *max, *number);
} /* end chk_range fortran bridge */


/* verify month number is in MM format and within a specific range */
void FTN_NAME2_(chk_month, CHK_MONTH) (int *month, int *status)
{
   *status = zchk_month (*month);
} /* end chk_month fortran bridge */


/* verify leap year */ 
void FTN_NAME2_(chk_leap, CHK_LEAP) (int *year, int *status)
{
   *status = zchk_leap (*year);
} /* end chk_leap fortran bridge */


/* verify day number is in DD format and within a specific range of a month */ 
void FTN_NAME2_(chk_day, CHK_DAY) (int *year, int *month, int *day, int *status)
{
   *status = zchk_day (*year, *month, *day);
} /* end chk_day fortran bridge */


/* verify hour number is in hh format and within the hour range */ 
void FTN_NAME2_(chk_hour, CHK_HOUR) (int *hour, int *status)
{
   *status = zchk_hour (*hour);
} /* end chk_hour fortran bridge */


/* verify minute number is in mm format and within the minute range */ 
void FTN_NAME2_(chk_minute, CHK_MINUTE) (int *minute, int *status)
{
   *status = zchk_minute (*minute);
} /* end chk_minute fortran bridge */


/* verify second number is in ss format and within the second range */ 
void FTN_NAME2_(chk_second, CHK_SECOND) (int *second, int *status)
{
   *status = zchk_minute (*second);
} /* end chk_second fortran bridge */


/* verify millisecond number is in mm format and within the msec range */ 
void FTN_NAME2_(chk_msec, CHK_MSEC) (int *msec, int *status)
{
   *status = zchk_msec (*msec);
} /* end chk_msec fortran bridge */


/* verify julian day is in a specified year */ 
void FTN_NAME2_(chk_julian, CHK_JULIAN) (int *year, int *julian, int *status)
{
   *status = zchk_julian (*year, *julian);
} /* end chk_julian fortran bridge */


/* verify SCET date in (YYYY,DDD,hh,mm,ss,mm) format */ 
void FTN_NAME2_(chk_scet_date, CHK_SCET_DATE) (int *date_buf, int *status)
{
   *status = zchk_scet_date (date_buf);
} /* end chk_scet_date fortran bridge */


/* verify Standard date in (YYYY,MM,DD,hh.mm,ss,mm) format */ 
void FTN_NAME2_(chk_std_date, CHK_STD_DATE) (int *date_buf, int *status)
{
   *status = zchk_std_date (date_buf);
} /* end chk_std_date fortran bridge */
/****************************************************************************/



/*************************** C Subroutines *********************************/

/* subroutine to check for 4-digit year number (i.e. YYYY format) */
int zchk_year (int year)
{
   if (year < 0)
   {
      zvmessage ("Warning::year number is negative.","");
      return 0;
   }
   else if (((year/1000) == 0) || ((year/1000) > 9)) 
   {
      zvmessage ("Warning::year number is not 4-digit.","");
      return 0;
   }
   else
      return 1;
} /* end zchk_year subroutine */


/* subroutine to check the input number is between min and max */
int zchk_range (int min, int max, int number)
{
   if (max < min) return 0;
   if ((number >= min) && (number <= max))
      return 1;
   else
      return 0;
} /* end zchk_range subroutine */


/* subroutine to check month number is in MM format and within 1-12 range.  */
int zchk_month (int month)
{
   if (!zchk_range (1, 12, month))
   {
      zvmessage ("Warning::month number is not between 1-12.","");
      return 0;
   }
   else
      return 1;
} /* end zchk_month subroutine */


/* subroutine to check for leap year.  It returns (1) if the given year is a 
   leap year */
int zchk_leap (int year)
{
   if ((year%4 == 0) && (year%100 != 0)) 
      return 1;
   else if ((year%400) == 0)
      return 1;
   else
      return 0;
} /* end zchk_leap subroutine */


/* subroutine to check for day of the month */
int zchk_day (int year, int month, int day)
{
   /* constant number of days in each month of a non-leap year. */
   const short int day_list[12] =
      {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

   int num_days = 0;
   int status = 0;

   /* make sure the input year is in YYYY format */
   if ((status = zchk_year(year)))
   {
      /* make sure the input month is within 1-12 */
     if ((status = zchk_month(month)))
      {
         num_days = day_list [month-1]; /* get # of days of the given month */

         /* check leap year and the month is Feb. then # day = 29 */
         if (zchk_leap(year) && (month == 2)) ++num_days;

         /* make sure the input day is within 1-num_days range */
         if (zchk_range(1, num_days, day)) status = 1;
         else
         {
            zvmessage 
               ("Warning::Invalid day number for the given year and month.","");
            status = 0;
         }
      }
   }
   return status;
} /* end zchk_day subroutine */


/* subroutine to check the hour number is in hh format and within the right 
   range */
int zchk_hour (int hour)
{
   if (!zchk_range (0, 23, hour))
   {
      zvmessage ("Warning::hour number is not between 0-23.","");
      return 0;
   }
   else
      return 1;
} /* end zchk_hour subroutine */


/* subroutine to check the minute number is in mm format and within the right 
   range */
int zchk_minute (int minute)
{
   if (!zchk_range (0, 59, minute))
   {
      zvmessage ("Warning::minute number is not between 0-59.","");
      return 0;
   }
   else
      return 1;
} /* end zchk_minute subroutine */


/* subroutine to check the second number is in ss format and within the right 
   range */
int zchk_second (int second)
{
   if (!zchk_range (0, 59, second))
   {
      zvmessage ("Warning::second number is not between 0-59.","");
      return 0;
   }
   else
      return 1;
} /* end zchk_second subroutine */


/* subroutine to check for millisecond number is between 0-999 */
int zchk_msec (int msec)
{
   if (!zchk_range (0, 999, msec))
   {
      zvmessage ("Warning::millisecond number is not between 0-999.","");
      return 0;
   }
   else
      return 1;
} /* end zchk_msec subroutine */


/* subroutine to check the input julian day with respect to the given year */
int zchk_julian (int year, int julian)
{
   int status = 0;

   /* make sure the year is in YYYY format */
   if ((status = zchk_year (year)))
   {
      /* see if the given year is a leap year */
      if (zchk_leap (year))
      {
         /* if is a leap year, make sure the range is within 1-366 day */
         if (zchk_range(1, 366, julian)) status = 1;
         else
         {
            zvmessage 
               ("Warning::Invalid Julian day for leap year, range 1-366.","");
            status = 0;
         }
      }
      else
      {
         /* if is not a leap year, make sure the range is within 1-365 day */
         if (zchk_range(1, 365, julian)) status = 1;
         else
         {
            zvmessage 
             ("Warning::Invalid Julian day for non-leap year, range 1-365.","");
            status = 0;
         }
      }
   }
   return status;
} /* end zchk_julian subroutine */


/* subroutine to verify the scet date in (YYYY,DDD,hh,mm,ss,mm) format and 
   within the correct range */
int zchk_scet_date (int *date_buf)
{
   int year,day,hour,minute,second,msec;

   int status = 0;

   /* retrieve the correct fields */ 
   year   = *date_buf++;
   day    = *date_buf++;
   hour   = *date_buf++;
   minute = *date_buf++;
   second = *date_buf++;
   msec   = *date_buf;

   if ((zchk_year(year)) && (zchk_julian(year, day)))
      if ((zchk_hour(hour)) && (zchk_minute(minute)) && 
          (zchk_second(second)) && (zchk_msec(msec)))
         status = 1;
   return status;
} /* end zchk_scet_date subroutine */


/* subroutine to verify the standard date in (YYYY,MM,DD,hh,mm,ss,mm) format and
   within the correct range */
int zchk_std_date (int *date_buf)
{
   int year,month,day,hour,minute,second,msec;
 
   int status = 0;

   /* retrieve the correct fields */
   year   = *date_buf++;
   month  = *date_buf++;
   day    = *date_buf++;
   hour   = *date_buf++;
   minute = *date_buf++;
   second = *date_buf++;
   msec   = *date_buf;
 
   if ((zchk_year(year)) && (zchk_month(month)) && 
       (zchk_day(year, month, day)))
      if ((zchk_hour(hour)) && (zchk_minute(minute)) &&
          (zchk_second(second)) && (zchk_msec(msec)))
         status = 1;
   return status;
} /* end zchk_std_date subroutine */

/***************************************************************************/

