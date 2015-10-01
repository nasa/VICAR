$!****************************************************************************
$!
$! Build proc for MIPL module sdrdate
$! VPACK Version 1.9, Monday, December 07, 2009, 16:35:31
$!
$! Execute by entering:		$ @sdrdate
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
$ write sys$output "*** module sdrdate ***"
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
$ write sys$output "Invalid argument given to sdrdate.com file -- ", primary
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
$   if F$SEARCH("sdrdate.imake") .nes. ""
$   then
$      vimake sdrdate
$      purge sdrdate.bld
$   else
$      if F$SEARCH("sdrdate.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake sdrdate
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @sdrdate.bld "STD"
$   else
$      @sdrdate.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create sdrdate.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack sdrdate.com -mixed -
	-s sdrdate.c -
	-i sdrdate.imake -
	-t tsdrdate.f tzsdrdate.c tsdrdate.imake tsdrdate.pdf tstsdrdate.pdf -
	-o sdrdate.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create sdrdate.c
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create sdrdate.imake
/***********************************************************************

                     IMAKE FILE FOR SUBROUTINE LIBRARY sdrdate

   To Create the build file give the command:

	$ vimake sdrdate                 (VMS)
   or
	% vimake sdrdate                 (Unix)


*************************************************************************/

#define SUBROUTINE sdrdate

#define MODULE_LIST sdrdate.c 

#define P2_SUBLIB

#define USES_C
$ Return
$!#############################################################################
$Test_File:
$ create tsdrdate.f
       SUBROUTINE TSDRDATE()
       INTEGER*4 SYEAR,HR,HOUR,I,YEAR
       INTEGER*4 FRAC_SEC,SFDUT1,SFDUT2,SS_1950,SECOND,MSEC
       INTEGER*2 SDR(3),SDR2(3)
       CHARACTER*100 MSG

       CALL XVMESSAGE('Gross check through entire century',' ')
       CALL XVMESSAGE(' ',' ')
       DO 25 I=0,2
        IF (I.EQ.0) HR=0
        IF (I.EQ.1) HR=1
        IF ((I.NE.0).AND.(I.NE.1)) HR=8759
        CALL XVMESSAGE(' ',' ')
        WRITE(MSG,1000) HR
        CALL XVMESSAGE(MSG,' ')
 1000      FORMAT('Hour of year = ', I)
        CALL XVMESSAGE(' ',' ')
        CALL XVMESSAGE('ss_sec,fsec,year,hour,sec,msec',' ')
        CALL XVMESSAGE(' ',' ')

        DO 50 YEAR=0,99
         SDR(1)=HR+24
         SDR(2)=3500
         SDR(3)=890
         CALL SFDUDATE(SFDUT1,SFDUT2,SDR,YEAR)
         CALL SDRDATE(SFDUT1,SFDUT2,SDR2,SYEAR)
         SS_1950 = SFDUT1
         FRAC_SEC = SFDUT2
         HOUR = SDR(1)
         SECOND = SDR(2)
         MSEC = SDR(3)
         WRITE(MSG,1500)SS_1950,FRAC_SEC,YEAR,HOUR,SECOND,MSEC
         CALL XVMESSAGE(MSG,' ')
 1500          FORMAT(6I12)
         IF (YEAR.NE.SYEAR.OR.HOUR.NE.SDR2(1).OR.
     c   SECOND.NE.SDR2(2).OR.MSEC.NE.SDR2(3)) THEN
          HOUR = SDR2(1)
          SECOND = SDR2(2)
          MSEC = SDR2(3)
          WRITE(MSG,1500)SS_1950,FRAC_SEC,SYEAR,HOUR,SECOND,MSEC
          CALL XVMESSAGE(MSG,' ')
         ENDIF
   50  CONTINUE
   25  CONTINUE

       CALL XVMESSAGE('Check through each day of VGR Uranus period',' ')
       CALL XVMESSAGE(' ',' ')
       DO 125 YEAR=85,88
       WRITE(MSG,2000)YEAR
       CALL XVMESSAGE(' ',' ')
       CALL XVMESSAGE(MSG,' ')    
       CALL XVMESSAGE(' ',' ')
 2000  FORMAT('Year = ', I)
       DO 150 HR=0,8783,24
       SDR(1)=HR+24
       SDR(2)=3500
       SDR(3)=890
       IF (YEAR.NE.88.AND.HR.EQ.8760) GO TO 125
       CALL SFDUDATE(SFDUT1,SFDUT2,SDR,YEAR)
       CALL SDRDATE(SFDUT1,SFDUT2,SDR2,SYEAR)
       SS_1950 = SFDUT1
       FRAC_SEC = SFDUT2
       HOUR = SDR(1)
       SECOND = SDR(2)
       MSEC = SDR(3)
       WRITE(MSG,1500)SS_1950,FRAC_SEC,YEAR,HOUR,SECOND,MSEC
       CALL XVMESSAGE(MSG,' ')
       IF (YEAR.NE.SYEAR.OR.HOUR.NE.SDR2(1).OR.
     c  SECOND.NE.SDR2(2).OR.MSEC.NE.SDR2(3)) THEN
         HOUR = SDR2(1)
         SECOND = SDR2(2)
         MSEC = SDR2(3)
         WRITE(MSG,1500)SS_1950,FRAC_SEC,SYEAR,HOUR,SECOND,MSEC
         CALL XVMESSAGE(MSG,' ')
       ENDIF
  150  CONTINUE
  125  CONTINUE
       END
$!-----------------------------------------------------------------------------
$ create tzsdrdate.c
/*===========================================================================*
 |  tzsdrdate.c -- Routine to test sdrdate.c                                 |
 |									     |
 |  The test consists of calling sdrdate to convert from SFDU to SDR time    |
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

     FTN_NAME(tsdrdate)();     
}	

$!-----------------------------------------------------------------------------
$ create tsdrdate.imake
/* Imake file for Test of VICAR subroutine sdrdate */

#define PROGRAM tsdrdate

#define MODULE_LIST tsdrdate.f tzsdrdate.c

#define MAIN_LANG_C
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_FORTRAN
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

$!-----------------------------------------------------------------------------
$ create tsdrdate.pdf
process help=*
END-PROC
$!-----------------------------------------------------------------------------
$ create tstsdrdate.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
tsdrdate
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create sdrdate.hlp
1 SDRDATE

  Routine to convert SFDU time format to SDR time format.

  C Calling Sequence:  zsdrdate(sfdut,sdrt,*sdr_year)

  Arguments:  struct sfdu_time sfdut	!Output Epoch 1950 time
	      struct sdr_time  sdrt     !Input Julian day_of_year,sec_of_hour,
					 and millisecond of second.
	      int *sdr_year		!Input year of century (0-99)

  Note: Data structures sfdu_time and sdr_time may be obtained by including
        SFDUTIME.H from the C-Include Library.
  
  FORTRAN Calling Sequence: SDRDATE(SFDUT1,SFDUT2,SDRTIM,SDR_YEAR) 

  Arguments:  INTEGER*4 SFDUT1, SFDUT2, SDR_YEAR
              INTEGER*2 SDRTIM(3)
  
  For SDRDATE:

             The sfdut structure or SFDUT1 and SFDUT2 are the inputs
             where the first object in sfdut and SFDUT1 are the seconds
             since 1950 and the second object in sfdut and SFDUT2 are the
             fractional seconds.

             The output for SDRDATE is the year (SDR_YEAR or sdr_year) and
             for the sdrt structure and SDRTIM(3) array the hour of year,
             second of hour, and msec of second.

2 History

  Original Programmer: Gary Yagi, November 22, 1984
  Current Cognizant Programmer: Gary Yagi
  Source Language: C
  Ported to Unix:  Damon D. Knight  August 17, 1993

3 Operation

  This routine is valid between 1950 and 2050.  The only obscure part of 
  the code is the calculation of leap years.  Starting from 0 A.D., leap
  years are all years divisible by 4, except years divisible by 100 but not
  by 400.  Note that the year 2000 is a leap year.  To keep things simple,
  this routine ignores the 100 and 400 years rules since they cancel out for
  the coming century.

  The year 1900 is used as a base for calculating leap years, rather than 
  the year 0 to reduce the possibility of round-off errors.  The total
  number of days elasped for the year 1900 to the beginning of the current
  year is 365.25*year + 0.75.  The 0.75 adjusts for the century (1900) 
  starting on a leap year.  The total number of days elapsed since the year
  1950 is:

         days = 365.25*year+0.75            !days since 1900
                    - 18263                 !days between 1900 and 1950
                    + 0.125                 !half of 0.25 to reduce rnd-off err
$ Return
$!#############################################################################
