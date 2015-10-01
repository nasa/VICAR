$!****************************************************************************
$!
$! Build proc for MIPL module datfmt
$! VPACK Version 1.9, Monday, December 07, 2009, 16:10:28
$!
$! Execute by entering:		$ @datfmt
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
$ write sys$output "*** module datfmt ***"
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
$ write sys$output "Invalid argument given to datfmt.com file -- ", primary
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
$   if F$SEARCH("datfmt.imake") .nes. ""
$   then
$      vimake datfmt
$      purge datfmt.bld
$   else
$      if F$SEARCH("datfmt.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake datfmt
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @datfmt.bld "STD"
$   else
$      @datfmt.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create datfmt.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack datfmt.com -mixed -
	-s datfmt.c -
	-i datfmt.imake -
	-t tdatfmt.f tzdatfmt.c tdatfmt.imake tdatfmt.pdf tstdatfmt.pdf -
	-o datfmt.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create datfmt.c
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create datfmt.imake
/***********************************************************************

                     IMAKE FILE FOR SUBROUTINE LIBRARY datfmt

   To Create the build file give the command:

	$ vimake datfmt                     (VMS)
   or
	% vimake datfmt                     (Unix)


*************************************************************************/

#define SUBROUTINE datfmt

#define MODULE_LIST datfmt.c

#define FTN_STRING
#define P2_SUBLIB

#define USES_ANSI_C

$ Return
$!#############################################################################
$Test_File:
$ create tdatfmt.f
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44

      CHARACTER*20 A
      CHARACTER*45 MS1
      INTEGER*4 I
      INTEGER*2 FLAG

C-------- TEST FOR FIRST FORMAT ----------

      CALL XVMESSAGE ('Test the FORTRAN interface',' ')
      CALL XVMESSAGE (' ',' ')

      CALL XVMESSAGE ('*** Testing for flag = 1',' ')
      FLAG = 1

      CALL DATFMT(FLAG, A, I)

      CALL PRNT(4,1,I,'    -> The integer date is .')

      MS1(1:25) = '    -> The ASCII date is '
      MS1(26:45) = A(1:20)
      CALL XVMESSAGE(MS1 , ' ')

      CALL XVMESSAGE(' ',' ')
C-------- TEST FOR SECOND FORMAT ----------

      CALL XVMESSAGE ('*** Testing for flag = 2',' ') 
      FLAG = 2
      
      CALL DATFMT(FLAG, A, I)

      CALL PRNT(4,1,I,'    -> The integer date is .')
      MS1(1:25) = '    -> The ASCII date is '
      MS1(26:45) = A(1:20)
      CALL XVMESSAGE(MS1 , ' ')
      CALL XVMESSAGE(' ',' ')

      CALL tzdatfmt

      RETURN
      END
$!-----------------------------------------------------------------------------
$ create tzdatfmt.c
/* Test for subroutine datfmt for C callable         */

#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tzdatfmt) ()
{
     char intmsg[8], ascmsg[46];
     unsigned char A[20];
     int I;

     zvmessage("Test the C interface"," ");
     zvmessage(" "," ");

     zvmessage ("*** Testing for flag = 1", " ");
     zdatfmt(1, A, &I);
     
     zprnt (4,1,&I,"   -> The integer date is .");

     sprintf(ascmsg,"   -> The ASCII date is %s", A);
     zvmessage(ascmsg," ");
     zvmessage(" "," ");

     zvmessage ("*** Testing for flag = 2", " ");
     zdatfmt(2, A, &I);

     zprnt (4,1,&I,"   -> The integer date is .");
     
     sprintf(ascmsg,"   -> The ASCII date is %s", A);
     zvmessage(ascmsg," ");
     zvmessage(" "," ");

}
$!-----------------------------------------------------------------------------
$ create tdatfmt.imake
/* Imake file for Test of VICAR subroutine datfmt */

#define PROGRAM tdatfmt

#define MODULE_LIST tdatfmt.f tzdatfmt.c 

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

/* #define LIB_LOCAL       /* Comment out upon delivery */

$!-----------------------------------------------------------------------------
$ create tdatfmt.pdf
PROCESS
END-PROC
$!-----------------------------------------------------------------------------
$ create tstdatfmt.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
write "Results may differ from day to day because current date is printed"
tdatfmt
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create datfmt.hlp
1 DATFMT

 This routine will return the date in various formats depending on the format
 chosen.  The date may be returned as an ascii string or an int value or both.
 The routine takes into account leap years.


	 Input:	If flag = 1 - Format is YYDDD (YY=YEAR, DDD=DAY OF YEAR)
                	      both ASCII and int.

	        If flag = 2 - Format is 'WED JUL 16, 1993'
                              Only ASCII is returned (int part = 0)

 	Output:   		A - ASCII string date format
				I - Integer value (For flag = 1 only)


  FORTRAN  Calling Sequence:

	CALL DATFMT(FLAG, A, I)

  C Calling Sequence:

        zdatfmt(flag, A, I);
  
  ARGUMENTS
  
  flag -  Address of the short integer where the format option will be placed. 
  A    -  Address of the character string for date will be placed.
  I    -  Address of the integer for the date will be placed.


2 HISTORY

  Original Programmer: C. Avis  6-28-83
  Current Cognizant Programmer: D. D. Knight
  Original Source Language: FORTRAN
  Converted to C: 7-16-93
  Ported to Unix: D. D. Knight 7-16-93

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
$ Return
$!#############################################################################
