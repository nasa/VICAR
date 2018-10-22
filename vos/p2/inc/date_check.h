/******************************************************************************
 
This file contains the protocols of a set of date-utility-subroutines to
assist in verifying date formats and ranges.  Its purpose is to assist the
Year2000 compliance conversion project.
 
   int zchk_year (int);
   :: subroutine to check for 4-digit year number (i.e. YYYY format)
 
   int zchk_range (int, int, int);
   :: subroutine to check the input number is between min and max.
 
   int zchk_month (int);
   :: subroutine to check month number is in MM format and within 1-12 range.
 
   int zchk_leap (int);
   :: subroutine to check for leap year.  It returns (1) if the given year is a
   :: leap year.
 
   int zchk_day (int, int, int);
   :: subroutine to check for day of the month.
 
   int zchk_hour (int);
   :: subroutine to check the hour number is in hh format and within the right
   :: range.
 
   int zchk_minute (int);
   :: subroutine to check the minute number is in mm format and within the
   :: right range.
 
   int zchk_second (int);
   :: subroutine to check the second number is in ss format and within the
   :: right range.
 
   int zchk_msec (int);
   :: subroutine to check for millisecond number is between 0-999.
 
   int zchk_julian (int, int);
   :: subroutine to check the input julian day with respect to the given year.
 
   int zchk_scet_date (int *);
   :: subroutine to verify the scet date in (YYYY,DDD,hh,mm,ss,mm) format and
   :: within the correct range.
 
   int zchk_std_date (int *);
   :: subroutine to verify the standard date in (YYYY,MM,DD,hh,mm,ss,mm)
   :: format and within the correct range.
 
 
History:
 
   March 23, 1998 ...T. Huang... Initial release.
   April 08, 1998 ...T. Huang... Corrected Fortran-bridge problem.
******************************************************************************/

#ifndef _DATE_CHECK_H
#define _DATE_CHECK_H

   int zchk_year (int);
   int zchk_range (int, int, int);
   int zchk_month (int);
   int zchk_leap (int);
   int zchk_day (int, int, int);
   int zchk_hour (int);
   int zchk_minute (int);
   int zchk_second (int);
   int zchk_msec (int);
   int zchk_julian (int, int);
   int zchk_scet_date (int *);
   int zchk_std_date (int *);

#endif

