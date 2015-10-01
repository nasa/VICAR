$!****************************************************************************
$!
$! Build proc for MIPL module jday
$! VPACK Version 1.9, Monday, December 07, 2009, 16:25:18
$!
$! Execute by entering:		$ @jday
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
$ write sys$output "*** module jday ***"
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
$ write sys$output "Invalid argument given to jday.com file -- ", primary
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
$   if F$SEARCH("jday.imake") .nes. ""
$   then
$      vimake jday
$      purge jday.bld
$   else
$      if F$SEARCH("jday.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake jday
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @jday.bld "STD"
$   else
$      @jday.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create jday.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack jday.com -mixed -
	-s jday.f zjday.c -
	-i jday.imake -
	-t tjday.f tzjday.c tjday.imake tjday.pdf tstjday.pdf -
	-o jday.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create jday.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      SUBROUTINE JDAY(M,D,Y,DOY)
      IMPLICIT NONE

C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C  
C  Routine to Calculate Day-of-Year from the given Month, Day, and Year 
C  information	
C
C  Fortran Usage:    Call  JDAY(M, D, Y, DOY)
C
C  Parameters:-
C
C       M:  Month           Input,   Integer*4
C       D:  Day of Month    Input,   Integer*4
C       Y:  Year            Input,   Integer*4
C     DOY:  Day of Year     Output,  Integer*4
C
C   History
C
C   07-28-92....WPL....Initial Release under MSTP (Ported for UNIX conversion) 
C
C   07-08-1998 .TXH.   Modified date checking statements to use
C                      'date_check' subroutine calls for correct
C                      calculation of leap year.
C
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

       INTEGER*4  M,D,Y,DOY

       INTEGER*4  LEAP,STATUS  ! flags to indicate leap year and status.

       Integer*4  BDOY(12)     ! Beginning Day of Year for the 12 months

C    Define beginnign day for each month (Regular, NON-Leap Year)

        BDOY(1) =   1   ! Beginning of January   is Day    1 of Year
        BDOY(2) =  32   !              Februrary          32
        BDOY(3) =  60   !              March              60 
        BDOY(4) =  91   !              April              91
        BDOY(5) = 121   !              May               121
        BDOY(6) = 152   !              June              152
        BDOY(7) = 182   !              July              182
        BDOY(8) = 213   !              August            213
        BDOY(9) = 244   !              September         244
        BDOY(10)= 274   !              October           274
        BDOY(11)= 305   !              November          305
        BDOY(12)= 335   !              December          335

        LEAP = 0    ! initialize leap year flag
        STATUS = 0  ! initialize status flag

C Check some default inputs (original implementation)
C
C        If (M .LT. 1 .or.  M .GT. 12)  then
C          Call Xvmessage(' Input value of MONTH out of bound', ' ')
C          Call Abend
C        Endif 
C        If (D .LT. 1  .or. D. GT. 31)  then
C          Call Xvmessage(' Input value of DAY out of bound', ' ')
C          Call Abend
C        Endif

C Check some default inputs (new implementation)
C Using the 'date_check' subroutines
        CALL CHK_DAY (Y,M,D,STATUS)
        IF (STATUS .EQ. 0) THEN
           CALL XVMESSAGE ('Invalid date input.',' ')
           CALL ABEND
        END IF

C    Calculate DOY  for NON-Leap year !
        DOY = BDOY(M) + D - 1

C Determine if the year is a leap year using 'date_check' subroutine
        CALL CHK_LEAP (Y, LEAP)
 
C Add 1 day for LEAP Year if Month passes Februrary
        IF ((LEAP .EQ. 1) .AND. (M .GT. 2)) THEN
            DOY = DOY + 1
        ENDIF
 
        RETURN
        END

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zjday.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include  "xvmaininc.h"  
#include  "ftnbridge.h"

/************************************************************************/
/*  C-Callable Version zjday - Calcualte Day of Year value              */
/*  (See Fortran Source code of JDAY)                                   */
/************************************************************************/


void zjday (m,d,y,doy)  
int   m;             /* month,         input  */
int   d;             /* day of month,  input  */
int   y;             /* year,          input  */
int   *doy;           /* day of year,    output */
{
FTN_NAME2(jday, JDAY) (&m, &d, &y, doy) ;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create jday.imake
/* Imake file for VICAR subroutine JDAY*/

#define SUBROUTINE  jday

#define MODULE_LIST  jday.f  zjday.c

#define P2_SUBLIB

#define USES_ANSI_C
#define USES_FORTRAN
#define FTNINC_LIST fortport

$ Return
$!#############################################################################
$Test_File:
$ create tjday.f
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      IMPLICIT NONE

      INTEGER*4  MONTH, DAY, YEAR, OUT
      INTEGER*4  ICNT, IDEF

      CALL XVMESSAGE('******  Testing FORTRAN version  ******', ' ')

      CALL XVPARM ('YEAR',  YEAR,  ICNT, IDEF, 1)
      CALL XVPARM ('MONTH', MONTH, ICNT, IDEF, 1)
      CALL XVPARM ('DAY',   DAY,   ICNT, IDEF, 1)
      CALL JDAY (MONTH, DAY, YEAR, OUT)
      CALL PRNT (4, 1, OUT, 'Day-of-Year = .')

      CALL tzjday

      RETURN
      END

$!-----------------------------------------------------------------------------
$ create tzjday.c
#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tzjday) ()
{
  int  month, day, year, out;
  int  icnt, idef;

  zvmessage(" ", " ");
  zvmessage("******  Testing C-Bridge version  ****** ", " ");
  zvparm ("YEAR",  &year,  &icnt, &idef, 0, 0);
  zvparm ("MONTH", &month, &icnt, &idef, 0, 0);
  zvparm ("DAY",   &day,   &icnt, &idef, 0, 0);
  zjday (month, day, year, &out);
  zprnt (4, 1, &out, "Day-of-Year = .");
  zvmessage (" ", " ");

}

$!-----------------------------------------------------------------------------
$ create tjday.imake
/* Imake file for Fortran-Test of VICAR subroutine  JDAY*/

#define PROGRAM tjday

#define MODULE_LIST tjday.f  tzjday.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_ANSI_C

#define LIB_RTL
#define LIB_TAE       
/* #define LIB_LOCAL   /*  disable when delivery   */
#define LIB_P2SUB
    
$!-----------------------------------------------------------------------------
$ create tjday.pdf
process
   PARM YEAR	TYPE=INTEGER	COUNT=1	DEFAULT=1998
   PARM	MONTH	TYPE=INTEGER	COUNT=1 DEFAULT=1
   PARM DAY	TYPE=INTEGER	COUNT=1	DEFAULT=1
end-proc
$!-----------------------------------------------------------------------------
$ create tstjday.pdf
procedure
refgbl $echo

body
let _onfail="continue"

let $echo="no"
write ">>> Testing first day of a leap year"
let $echo="yes"
tjday year=1996 month=1 day=1

let $echo="no"
write ""
write ">>> Testing first day of a non-leap year"
let $echo="yes"
tjday year=1998 month=1 day=1

let $echo="no"
write ""
write ">>> Testing last day of a leap year"
let $echo="yes"
tjday year=1996 month=12 day=31
 
let $echo="no"
write ""
write ">>> Testing last day of a non-leap year"
let $echo="yes"
tjday year=1998 month=12 day=31
 
let $echo="no"
write ""
write ">>> Testing a day after FEB in a leap year"
let $echo="yes"
tjday year=1996 month=3 day=1

let $echo="no"
write ""
write ">>> Testing a day after FEB in a non-leap year"
let $echo="yes"
tjday year=1998 month=3 day=1

let $echo="no"
write ""
write ""
write "***** Year 2000 Compilance Tests *****"
write "*****     It is a Leap Year      *****"
write ""
write ">>> Testing a day after FEB"
let $echo="yes"
tjday year=2000 month=3 day=1

let $echo="no"
write ""
write ">>> Testing last day of the year"
let $echo="yes"
tjday year=2000 month=12 day=31

let $echo="no"
write ""
write ">>> Testing the year 20001, a non-leap year"
let $echo="yes"
tjday year=2001 month=3 day=1

let $echo="no"
write ""
write ">>> Testing last day of year 2001"
let $echo="yes"
tjday year=2001 month=12 day=31

let $echo="no"
write ""
write ""
write "***** Invalid Date Testing *****"
write "***** this case will ABEND *****"
let $echo="yes"
tjday year=2001 month=2 day=29
 
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create jday.hlp
1  JDAY

  Routine for calculating Day-of-year value from .

  Calling Sequence:  CALL JDAY(M, D, Y, Doy)

  Arguments:

	INTEGER*4 M		Month         (1 <= M <= 12)
	INTEGER*4 D		Day of Month  (1 <= D <= 31)
	INTEGER*4 Y		Year
	INTEGER*4 Doy		Day of Year  


2 Operation

  The subroutine JDAY reads in the numerical values of Month, Day, and Year
  converts these information into Day-of-Year value.
 
  For the Leap year, the code will add 1 to the DOY value if the Month of
  concern is after Februrary.


2 Example

       Integer*4  Month, Day, Year
       Integer*4  D1, D2
         
       Month = 3
       Day =  22
       Year = 1982
       Call JDAY(Month, Day, Year, D1)  --->  D1 will be =  81
       Year = 1992
       Call JDAY(Month, Day, Year, D2)  --->  D2 will be =  82 
       Call JDAY(1, 1, 1991, D1)        --->  D1 will be =   1
       Call JDAY(12,31,1992, D2)        --->  D2 will be = 366
 

2 History

  07-28-92 ...WPL.... Initial Release under MSTP (UNIX Porting)

  07-08-1998 .TXH.   Modified date checking statements to use
                     'date_check' subroutine calls for correct
                     calculation of leap year.

  Source Language:  FORTRAN

$ Return
$!#############################################################################
