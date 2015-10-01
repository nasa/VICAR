$!****************************************************************************
$!
$! Build proc for MIPL module date_check
$! VPACK Version 1.9, Monday, December 07, 2009, 16:10:18
$!
$! Execute by entering:		$ @date_check
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!   OTHER       Only the "other" files are created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module date_check ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
$ Create_Other = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if primary .eqs. "OTHER" then Create_Other = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Create_Other .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to date_check.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Create_Other then gosub Other_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$   Create_Other = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("date_check.imake") .nes. ""
$   then
$      vimake date_check
$      purge date_check.bld
$   else
$      if F$SEARCH("date_check.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake date_check
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @date_check.bld "STD"
$   else
$      @date_check.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create date_check.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack date_check.com -mixed -
	-s date_check.c -
	-i date_check.imake -
	-t tdate_check.f tzdate_check.c tdate_check.imake tzdate_check.imake -
	   tdate_check.pdf tzdate_check.pdf tstdate_check.pdf -
	-o date_check.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create date_check.c
$ DECK/DOLLARS="$ VOKAGLEVE"
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

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create date_check.imake
/* Imake file for VICAR subroutine DATE_CHECK */

#define SUBROUTINE  date_check

#define MAIN_LANG_C
#define MODULE_LIST  date_check.c
#define USES_ANSI_C

#define P2_SUBLIB
#define LIB_TAE
#define LIB_RTL


$ Return
$!#############################################################################
$Test_File:
$ create tdate_check.f
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44

      IMPLICIT NONE

      INTEGER*4 SCET_DATE(6), STD_DATE(7), COUNT, STATUS
      LOGICAL XVPTST

      CALL XVP ('SCET', SCET_DATE, COUNT)
      IF (COUNT .EQ. 6) THEN
         CALL CHK_SCET_DATE (SCET_DATE, STATUS)
         IF (STATUS .EQ. 1) THEN
            CALL XVMESSAGE ('CHK_SCET_DATE: Valid date', 0)
         ELSE
            CALL XVMESSAGE ('CHK_SCET_DATE: Invalid date',0)
         END IF

         IF (XVPTST('DEBUG')) THEN
            CALL XVMESSAGE ('--- Check All Fields ---', 0)

            CALL CHK_YEAR (SCET_DATE(1), STATUS)
            IF (STATUS .EQ. 1) THEN
               CALL XVMESSAGE ('CHK_YEAR: Valid year', 0)
            ELSE
               CALL XVMESSAGE ('CHK_YEAR: Invalid year', 0)
            END IF

            CALL CHK_JULIAN (SCET_DATE(1), SCET_DATE(2), STATUS)
            IF (STATUS .EQ. 1) THEN
               CALL XVMESSAGE ('CHK_JULIAN: Valid julian day', 0)
            ELSE
               CALL XVMESSAGE ('CHK_JULIAN: Invalid julian day', 0)
            END IF

            CALL CHK_HOUR (SCET_DATE(3), STATUS)
            IF (STATUS .EQ. 1) THEN
               CALL XVMESSAGE ('CHK_HOUR: Valid hour number', 0)
            ELSE
               CALL XVMESSAGE ('CHK_HOUR: Invalid hour number', 0)
            END IF

            CALL CHK_MINUTE (SCET_DATE(4), STATUS)
            IF (STATUS .EQ. 1) THEN
               CALL XVMESSAGE ('CHK_MINUTE: Valid minute number', 0)
            ELSE
               CALL XVMESSAGE ('CHK_MINUTE: Invalid minute number', 0)
            END IF

            CALL CHK_SECOND (SCET_DATE(5), STATUS)
            IF (STATUS .EQ. 1) THEN
               CALL XVMESSAGE ('CHK_SECOND: Valid second number', 0)
            ELSE
               CALL XVMESSAGE ('CHK_SECOND: Invalid second number', 0)
            END IF

            CALL CHK_MSEC (SCET_DATE(6), STATUS)
            IF (STATUS .EQ. 1) THEN
               CALL XVMESSAGE 
     &            ('CHK_MSEC: Valid millisecond number', 0)
            ELSE
               CALL XVMESSAGE 
     &            ('CHK_MSEC: Invalid millisecond number', 0)
            END IF
         END IF

      END IF

      CALL XVP ('STD', STD_DATE, COUNT)
      IF (COUNT .EQ. 7) THEN
         CALL CHK_STD_DATE (STD_DATE, STATUS)
         IF (STATUS .EQ. 1) THEN
            CALL XVMESSAGE ('CHK_STD_DATE: Valid date', 0)
         ELSE
            CALL XVMESSAGE ('CHK_STD_DATE: Invalid date', 0)
         END IF

         IF (XVPTST('DEBUG')) THEN
            CALL XVMESSAGE ('--- Check All Fields ---', 0)
 
            CALL CHK_YEAR (STD_DATE(1), STATUS)
            IF (STATUS .EQ. 1) THEN
               CALL XVMESSAGE ('CHK_YEAR: Valid year', 0)
            ELSE
               CALL XVMESSAGE ('CHK_YEAR: Invalid year', 0)
            END IF
 
            CALL CHK_MONTH (STD_DATE(2), STATUS)
            IF (STATUS .EQ. 1) THEN
               CALL XVMESSAGE ('CHK_MONTH: Valid month', 0)
            ELSE
               CALL XVMESSAGE ('CHK_MONTH: Invalid month', 0)
            END IF

            CALL CHK_DAY (STD_DATE(1), STD_DATE(2), STD_DATE(3), STATUS)
            IF (STATUS .EQ. 1) THEN
               CALL XVMESSAGE ('CHK_DAY: Valid day', 0)
            ELSE
               CALL XVMESSAGE ('CHK_DAY: Invalid day', 0)
            END IF
 
            CALL CHK_HOUR (STD_DATE(4), STATUS)
            IF (STATUS .EQ. 1) THEN
               CALL XVMESSAGE ('CHK_HOUR: Valid hour number', 0)
            ELSE
               CALL XVMESSAGE ('CHK_HOUR: Invalid hour number', 0)
            END IF
 
            CALL CHK_MINUTE (STD_DATE(5), STATUS)
            IF (STATUS .EQ. 1) THEN
               CALL XVMESSAGE ('CHK_MINUTE: Valid minute number', 0)
            ELSE
               CALL XVMESSAGE ('CHK_MINUTE: Invalid minute number', 0)
            END IF
 
            CALL CHK_SECOND (STD_DATE(6), STATUS)
            IF (STATUS .EQ. 1) THEN
               CALL XVMESSAGE ('CHK_SECOND: Valid second number', 0)
            ELSE
               CALL XVMESSAGE ('CHK_SECOND: Invalid second number', 0)
            END IF
 
            CALL CHK_MSEC (STD_DATE(7), STATUS)
            IF (STATUS .EQ. 1) THEN
               CALL XVMESSAGE 
     &            ('CHK_MSEC: Valid millisecond number', 0)
            ELSE
               CALL XVMESSAGE 
     &            ('CHK_MSEC: Invalid millisecond number', 0)
            END IF
         END IF

      END IF

      RETURN
      END

$!-----------------------------------------------------------------------------
$ create tzdate_check.c
#include "vicmain_c.h"
#include "date_check.h"

void main44 ()
{
   int scet_date[6], std_date[7], count;

   zvp ("SCET",scet_date, &count);
   if (count == 6)
   {
      if (zchk_scet_date (scet_date)) 
         zvmessage ("zchk_scet_date: Valid date", "");
      else
         zvmessage ("zchk_scet_date: Invalid date","");

      if (zvptst("DEBUG"))
      {
         zvmessage ("--- Check All Fields ---","");
         if (zchk_year(scet_date[0]))
            zvmessage ("zchk_year: Valid year","");
         else
            zvmessage ("zchk_year: Invalid year","");

         if (zchk_julian(scet_date[0],scet_date[1]))
            zvmessage ("zchk_julian: Valid julian day","");
         else
            zvmessage ("zchk_julian: Invalid julian day","");

         if (zchk_hour(scet_date[2]))
            zvmessage ("zchk_hour: Valid hour number", "");
         else
            zvmessage ("zchk_hour: Invalid hour number", "");

         if (zchk_minute (scet_date[3]))
            zvmessage ("zchk_minute: Valid minute number", "");
         else
            zvmessage ("zchk_minute: Invalid minute number", "");
 
         if (zchk_second (scet_date[4]))
            zvmessage ("zchk_second: Valid second number", "");
         else
            zvmessage ("zchk_second: Invalid second number", "");
 
         if (zchk_msec (scet_date[5]))
            zvmessage ("zchk_msec: Valid millisecond  number", "");
         else
            zvmessage ("zchk_msec: Invalid millisecond number", "");
      } 

   }
   zvp ("STD",std_date, &count);
   if (count == 7)
   {
      if (zchk_std_date (std_date))
         zvmessage ("zchk_std_date: Valid date", "");
      else
         zvmessage ("zchk_std_date: Invalid date", "");

      if (zvptst("DEBUG"))
      {
         zvmessage ("--- Check All Fields ---","");
         if (zchk_year(std_date[0]))
            zvmessage ("zchk_year: Valid year","");
         else
            zvmessage ("zchk_year: Invalid year","");
 
         if (zchk_month(std_date[1]))
            zvmessage ("zchk_month: Valid month","");
         else
            zvmessage ("zchk_month: Invalid month","");

         if (zchk_day(std_date[0], std_date[1], std_date[2]))
            zvmessage ("zchk_day: Valid day","");
         else
            zvmessage ("zchk_day: Invalid day","");
 
         if (zchk_hour(std_date[3]))
            zvmessage ("zchk_hour: Valid hour number", "");
         else
            zvmessage ("zchk_hour: Invalid hour number", "");
 
         if (zchk_minute (std_date[4]))
            zvmessage ("zchk_minute: Valid minute number", "");
         else
            zvmessage ("zchk_minute: Invalid minute number", "");
 
         if (zchk_second (std_date[5]))
            zvmessage ("zchk_second: Valid second number", "");
         else
            zvmessage ("zchk_second: Invalid second number", "");
 
         if (zchk_msec (std_date[6]))
            zvmessage ("zchk_msec: Valid millisecond  number", "");
         else
            zvmessage ("zchk_msec: Invalid millisecond number", "");
      }

   }
}

$!-----------------------------------------------------------------------------
$ create tdate_check.imake
#define PROGRAM tdate_check

#define MODULE_LIST tdate_check.f 

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE       
#define LIB_P2SUB

/* #define LIB_LOCAL */    /*  disable when delivery   */
 
$!-----------------------------------------------------------------------------
$ create tzdate_check.imake
#define PROGRAM tzdate_check

#define MODULE_LIST tzdate_check.c

#define MAIN_LANG_C
#define TEST

#define USES_ANSI_C

#define LIB_RTL
#define LIB_TAE       
#define LIB_P2SUB

/* #define LIB_LOCAL */    /*  disable when delivery   */

$!-----------------------------------------------------------------------------
$ create tdate_check.pdf
process
   parm SCET   TYPE=INTEGER COUNT=(0:6) DEFAULT=--
   parm STD    TYPE=INTEGER COUNT=(0:7) DEFAULT=--
   parm DEBUG  TYPE=KEYWORD VALID=(NODEBUG, DEBUG) DEFAULT=NODEBUG
end-proc

$!-----------------------------------------------------------------------------
$ create tzdate_check.pdf
process
   parm SCET   TYPE=INTEGER COUNT=(0:6) DEFAULT=--
   parm STD    TYPE=INTEGER COUNT=(0:7) DEFAULT=--
   parm DEBUG  TYPE=KEYWORD VALID=(NODEBUG, DEBUG) DEFAULT=NODEBUG
end-proc

$!-----------------------------------------------------------------------------
$ create tstdate_check.pdf
procedure
refgbl $echo

body
let _onfail="continue"
let $echo="no"

write "Note To Testers: "
write "   In order to execute this test pdf, successful compilation of both "
write "   tzdate_check.c and tzdate_check.f are required.  To do so, you "
write "   will need to execute tzdate_check.imake and tdate_check.imake "
write "   individually."
write ""
write ""

write "******** Testing C interfaces ********"
write ""
write ">>>>> Testing CHK_SCET_DATE subroutine <<<<<"
write ""
write "::: A non-leap year test "
let $echo = "yes"
tzdate_check scet=(1998,82,10,59,40,68) 'DEBUG

let $echo = "no"
write ""
write "::: Another non-leap year test "
let $echo = "yes"
tzdate_check scet=(1998,365,10,59,40,68)

let $echo = "no"
write ""
write "::: A leap year test "
let $echo = "yes"
tzdate_check scet=(2000,83,10,59,40,68)

let $echo = "no"
write ""
write "::: Another leap year test "
let $echo = "yes"
tzdate_check scet=(2000,366,10,59,40,68)

let $echo = "no"
write ""
write "::: Test for invalid date formats "
write ""

write "::: Test for invalid year format "
let $echo = "yes"
tzdate_check scet=(98,82,10,59,40,68) 'DEBUG

let $echo = "no"
write ""
write "::: Another test for invalid year format "
let $echo = "yes"
tzdate_check scet=(998,82,10,59,40,68)

let $echo = "no"
write ""
write "::: Another test for invalid year format "
let $echo = "yes"
tzdate_check scet=(-1998,82,10,59,40,68)

let $echo = "no"
write ""
write "::: Test for invalid julian day for a non-leap year"
let $echo = "yes"
tzdate_check scet=(1998,366,10,59,40,68) 'DEBUG

let $echo = "no"
write ""
write "::: Test for invalid julian day for a leap year"
let $echo = "yes"
tzdate_check scet=(2000,367,10,59,40,68)

let $echo = "no"
write ""
write "::: Test for invalid julian day number"
let $echo = "yes"
tzdate_check scet=(1998,0,10,59,40,68)

let $echo = "no"
write ""
write "::: Test for invalid hour number"
let $echo = "yes"
tzdate_check scet=(1998,82,-1,59,40,68) 'DEBUG

let $echo = "no"
write ""
write "::: Another est for invalid hour number"
let $echo = "yes"
tzdate_check scet=(1998,82,24,59,40,68)

let $echo = "no"
write ""
write "::: Test for invalid minute number"
let $echo = "yes"
tzdate_check scet=(1998,82,10,-1,40,68) 'DEBUG

let $echo = "no"
write ""
write "::: Another test for invalid minute number"
let $echo = "yes"
tzdate_check scet=(1998,82,10,60,40,68)

let $echo = "no"
write ""
write "::: Test for invalid second number"
let $echo = "yes"
tzdate_check scet=(1998,82,10,59,-1,68) 'DEBUG

let $echo = "no"
write ""
write "::: Another test for invalid second number"
let $echo = "yes"
tzdate_check scet=(1998,82,10,59,60,68)

let $echo = "no"
write ""
write "::: Test for invalid millisecond number"
let $echo = "yes"
tzdate_check scet=(1998,82,10,59,40,-1) 'DEBUG

let $echo = "no"
write ""
write "::: Test for invalid millisecond number"
let $echo = "yes"
tzdate_check scet=(1998,82,10,59,40,1000)

let $echo = "no"
write ""
write ">>>>> Testing CHK_STD_DATE subroutine <<<<<"
write ""
write "::: A non-leap year test "
let $echo = "yes"
tzdate_check std=(1998,3,23,10,59,40,68) 'DEBUG
 
let $echo = "no"
write ""
write "::: Another non-leap year test "
let $echo = "yes"
tzdate_check std=(1998,12,31,10,59,40,68)
 
let $echo = "no"
write ""
write "::: A leap year test "
let $echo = "yes"
tzdate_check std =(2000,3,23,10,59,40,68)
 
let $echo = "no"
write ""
write "::: Another leap year test "
let $echo = "yes"
tzdate_check std=(2000,12,31,10,59,40,68)
 
let $echo = "no"
write ""
write "::: Test for invalid date formats "
write ""
 
write "::: Test for invalid year format "
let $echo = "yes"
tzdate_check std=(98,3,23,10,59,40,68)
 
let $echo = "no"
write ""
write "::: Another test for invalid year format "
let $echo = "yes"
tzdate_check std=(998,3,23,10,59,40,68)

let $echo = "no"
write ""
write "::: Another test for invalid year format "
let $echo = "yes"
tzdate_check std=(-1998,3,23,10,59,40,68)

let $echo = "no"
write ""
write "::: Test for invalid date for a non-leap year"
let $echo = "yes"
tzdate_check std=(1998,2,29,10,59,40,68)
 
let $echo = "no"
write ""
write "::: Test for invalid date for a leap year"
let $echo = "yes"
tzdate_check std=(2000,2,30,10,59,40,68)
 
let $echo = "no"
write ""
write "::: Test for invalid month number"
let $echo = "yes"
tzdate_check std=(1998,0,23,10,59,40,68) 'DEBUG

let $echo = "no"
write ""
write "::: Another test for invalid month number"
let $echo = "yes"
tzdate_check std=(1998,13,23,10,59,40,68)

let $echo = "no"
write ""
write "::: Test for invalid day number"
let $echo = "yes"
tzdate_check std=(1998,3,0,10,59,40,68) 'DEBUG

let $echo = "no"
write ""
write "::: Test for invalid day number"
let $echo = "yes"
tzdate_check std=(1998,3,32,10,59,40,68)
 
let $echo = "no"
write ""
write "::: Test for invalid hour number"
let $echo = "yes"
tzdate_check std=(1998,3,23,-1,59,40,68)
 
let $echo = "no"
write ""
write "::: Another test for invalid hour number"
let $echo = "yes"
tzdate_check std=(1998,3,23,24,59,40,68)
 
let $echo = "no"
write ""
write "::: Test for invalid minute number"
let $echo = "yes"
tzdate_check std=(1998,3,23,10,-1,40,68)
 
let $echo = "no"
write ""
write "::: Another test for invalid minute number"
let $echo = "yes"
tzdate_check std=(1998,3,23,10,60,40,68)
 
let $echo = "no"
write ""
write "::: Test for invalid second number"
let $echo = "yes"
tzdate_check std=(1998,3,23,10,59,-1,68)
 
let $echo = "no"
write ""
write "::: Another test for invalid second number"
let $echo = "yes"
tzdate_check std=(1998,3,23,10,59,60,68)
 
let $echo = "no"
write ""
write "::: Test for invalid millisecond number"
let $echo = "yes"
tzdate_check std=(1998,3,23,10,59,40,-1)
 
let $echo = "no"
write ""
write "::: Test for invalid millisecond number"
let $echo = "yes"
tzdate_check std=(1998,3,23,10,59,40,1000)
 

let $echo="no"
write ""
write ""
write "******** Testing FORTRAN interfaces ********"
write ""
write ">>>>> Testing CHK_SCET_DATE subroutine <<<<<"
write ""
write "::: A non-leap year test "
let $echo = "yes"
tdate_check scet=(1998,82,10,59,40,68) 'DEBUG

let $echo = "no"
write ""
write "::: Another non-leap year test "
let $echo = "yes"
tdate_check scet=(1998,365,10,59,40,68)

let $echo = "no"
write ""
write "::: A leap year test "
let $echo = "yes"
tdate_check scet=(2000,83,10,59,40,68)

let $echo = "no"
write ""
write "::: Another leap year test "
let $echo = "yes"
tdate_check scet=(2000,366,10,59,40,68)

let $echo = "no"
write ""
write "::: Test for invalid date formats "
write ""

write "::: Test for invalid year format "
let $echo = "yes"
tdate_check scet=(98,82,10,59,40,68) 'DEBUG

let $echo = "no"
write ""
write "::: Another test for invalid year format "
let $echo = "yes"
tdate_check scet=(998,82,10,59,40,68)

let $echo = "no"
write ""
write "::: Another test for invalid year format "
let $echo = "yes"
tdate_check scet=(-1998,82,10,59,40,68)

let $echo = "no"
write ""
write "::: Test for invalid julian day for a non-leap year"
let $echo = "yes"
tdate_check scet=(1998,366,10,59,40,68) 'DEBUG

let $echo = "no"
write ""
write "::: Test for invalid julian day for a leap year"
let $echo = "yes"
tdate_check scet=(2000,367,10,59,40,68)

let $echo = "no"
write ""
write "::: Test for invalid julian day number"
let $echo = "yes"
tdate_check scet=(1998,0,10,59,40,68)

let $echo = "no"
write ""
write "::: Test for invalid hour number"
let $echo = "yes"
tdate_check scet=(1998,82,-1,59,40,68) 'DEBUG

let $echo = "no"
write ""
write "::: Another est for invalid hour number"
let $echo = "yes"
tdate_check scet=(1998,82,24,59,40,68)

let $echo = "no"
write ""
write "::: Test for invalid minute number"
let $echo = "yes"
tdate_check scet=(1998,82,10,-1,40,68) 'DEBUG

let $echo = "no"
write ""
write "::: Another test for invalid minute number"
let $echo = "yes"
tdate_check scet=(1998,82,10,60,40,68)

let $echo = "no"
write ""
write "::: Test for invalid second number"
let $echo = "yes"
tdate_check scet=(1998,82,10,59,-1,68) 'DEBUG

let $echo = "no"
write ""
write "::: Another test for invalid second number"
let $echo = "yes"
tdate_check scet=(1998,82,10,59,60,68)

let $echo = "no"
write ""
write "::: Test for invalid millisecond number"
let $echo = "yes"
tdate_check scet=(1998,82,10,59,40,-1) 'DEBUG

let $echo = "no"
write ""
write "::: Test for invalid millisecond number"
let $echo = "yes"
tdate_check scet=(1998,82,10,59,40,1000)

let $echo = "no"
write ""
write ">>>>> Testing CHK_STD_DATE subroutine <<<<<"
write ""
write "::: A non-leap year test "
let $echo = "yes"
tdate_check std=(1998,3,23,10,59,40,68) 'DEBUG
 
let $echo = "no"
write ""
write "::: Another non-leap year test "
let $echo = "yes"
tdate_check std=(1998,12,31,10,59,40,68)
 
let $echo = "no"
write ""
write "::: A leap year test "
let $echo = "yes"
tdate_check std =(2000,3,23,10,59,40,68)
 
let $echo = "no"
write ""
write "::: Another leap year test "
let $echo = "yes"
tdate_check std=(2000,12,31,10,59,40,68)
 
let $echo = "no"
write ""
write "::: Test for invalid date formats "
write ""
 
write "::: Test for invalid year format "
let $echo = "yes"
tdate_check std=(98,3,23,10,59,40,68)
 
let $echo = "no"
write ""
write "::: Another test for invalid year format "
let $echo = "yes"
tdate_check std=(998,3,23,10,59,40,68)

let $echo = "no"
write ""
write "::: Another test for invalid year format "
let $echo = "yes"
tdate_check std=(-1998,3,23,10,59,40,68)

let $echo = "no"
write ""
write "::: Test for invalid date for a non-leap year"
let $echo = "yes"
tdate_check std=(1998,2,29,10,59,40,68)
 
let $echo = "no"
write ""
write "::: Test for invalid date for a leap year"
let $echo = "yes"
tdate_check std=(2000,2,30,10,59,40,68)
 
let $echo = "no"
write ""
write "::: Test for invalid month number"
let $echo = "yes"
tdate_check std=(1998,0,23,10,59,40,68) 'DEBUG

let $echo = "no"
write ""
write "::: Another test for invalid month number"
let $echo = "yes"
tdate_check std=(1998,13,23,10,59,40,68)

let $echo = "no"
write ""
write "::: Test for invalid day number"
let $echo = "yes"
tdate_check std=(1998,3,0,10,59,40,68) 'DEBUG

let $echo = "no"
write ""
write "::: Test for invalid day number"
let $echo = "yes"
tdate_check std=(1998,3,32,10,59,40,68)
 
let $echo = "no"
write ""
write "::: Test for invalid hour number"
let $echo = "yes"
tdate_check std=(1998,3,23,-1,59,40,68)
 
let $echo = "no"
write ""
write "::: Another test for invalid hour number"
let $echo = "yes"
tdate_check std=(1998,3,23,24,59,40,68)
 
let $echo = "no"
write ""
write "::: Test for invalid minute number"
let $echo = "yes"
tdate_check std=(1998,3,23,10,-1,40,68)
 
let $echo = "no"
write ""
write "::: Another test for invalid minute number"
let $echo = "yes"
tdate_check std=(1998,3,23,10,60,40,68)
 
let $echo = "no"
write ""
write "::: Test for invalid second number"
let $echo = "yes"
tdate_check std=(1998,3,23,10,59,-1,68)
 
let $echo = "no"
write ""
write "::: Another test for invalid second number"
let $echo = "yes"
tdate_check std=(1998,3,23,10,59,60,68)
 
let $echo = "no"
write ""
write "::: Test for invalid millisecond number"
let $echo = "yes"
tdate_check std=(1998,3,23,10,59,40,-1)
 
let $echo = "no"
write ""
write "::: Test for invalid millisecond number"
let $echo = "yes"
tdate_check std=(1998,3,23,10,59,40,1000)
 
end-proc

$ Return
$!#############################################################################
$Other_File:
$ create date_check.hlp
1 Help for date_check subroutines:

The date_check subroutines are a set of date-utility-subroutines to
assist in verifying date formats and ranges.  Its purpose is to assist the
Year2000 compliance conversion project.

C Bridges:
 
   int zchk_year (int year);
   :: subroutine to check for 4-digit year number (i.e. YYYY format)
 
   int zchk_range (int min, int max, int number);
   :: subroutine to check the input number is between min and max.
 
   int zchk_month (int month);
   :: subroutine to check month number is in MM format and within 1-12 range.
 
   int zchk_leap (int year);
   :: subroutine to check for leap year.  It returns (1) if the given year is a
   :: leap year.
 
   int zchk_day (int year, int month, int day);
   :: subroutine to check for day of the month.
 
   int zchk_hour (int hour);
   :: subroutine to check the hour number is in hh format and within the right
   :: range.
 
   int zchk_minute (int minute);
   :: subroutine to check the minute number is in mm format and within the
   :: right range.
 
   int zchk_second (int second);
   :: subroutine to check the second number is in ss format and within the
   :: right range.
 
   int zchk_msec (int msec);
   :: subroutine to check for millisecond number is between 0-999.
 
   int zchk_julian (int year, int julian);
   :: subroutine to check the input julian day with respect to the given year.
 
   int zchk_scet_date (int *date_buf);
   :: subroutine to verify the scet date in (YYYY,DDD,hh,mm,ss,mm) format and
   :: within the correct range.
 
   int zchk_std_date (int *date_buf);
   :: subroutine to verify the standard date in (YYYY,MM,DD,hh,mm,ss,mm)
   :: format and within the correct range.


FORTAN Bridges:

   INTEGER*4 YEAR, MONTH, DAY, JULIAN
   INTEGER*4 HOUR, MINUTE, SECOND, MSEC
   INTEGER*4 SCET_DATE(6), STD_DATE(7)
   INTEGER*4 MIN, MAX, NUM, STATUS
   ... 
   CALL CHK_YEAR   (YEAR, STATUS)
   CALL CHK_RANGE  (MIN, MAX, NUM, STATUS)
   CALL CHK_MONTH  (MONTH, STATUS)
   CALL CHK_LEAP   (YEAR, STATUS)
   CALL CHK_DAY    (YEAR, MONTH, DAY, STATUS)
   CALL CHK_HOUR   (HOUR, STATUS)
   CALL CHK_MINUTE (MINUTE, STATUS)
   CALL CHK_SECOND (SECOND, STATUS)
   CALL CHK_MSEC   (MSEC, STATUS)
   CALL CHK_JULIAN (YEAR, JULIAN, STATUS)
   CALL CHK_SCET_DATE (SCET_DATE, STATUS)
   CALL CHK_STD_DATE  (STD_DATE, STATUS)
   ...

 
2 History:
 
   Original Programmer: Thomas Huang, March 23, 1998
   Source Language: C
   Revision history: New


   April 8, 1998     Thomas Huang     Corrected Fortran-bridge problem.

$ Return
$!#############################################################################
