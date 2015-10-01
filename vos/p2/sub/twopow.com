$!****************************************************************************
$!
$! Build proc for MIPL module twopow
$! VPACK Version 1.6, Wednesday, July 28, 1993, 15:13:47
$!
$! Execute by entering:		$ @twopow
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
$ write sys$output "*** module twopow ***"
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
$   if F$SEARCH("twopow.imake") .nes. ""
$   then
$      vimake twopow
$      purge twopow.bld
$   else
$      if F$SEARCH("twopow.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake twopow
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @twopow.bld "STD"
$   else
$      @twopow.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create twopow.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack twopow.com -
	-s twopow.f ztwopow.c -
	-i twopow.imake -
	-t ttwopow.f tztwopow.c ttwopow.imake ttwopow.pdf tsttwopow.pdf -
	-o twopow.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create twopow.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C
C ***************** FORTRAN CALLABLE ROUTINE *********************
C
C RETURNS .TRUE. IF |N| IS A POWER OF 2.
C POW IS OPTIONAL ARGUMENT GIVING FIRST POWER OF 2 .GE. |N|.
C  85-5-10  ...LWK...
C  93-7-23  ...DDK...

	INTEGER*2 FUNCTION TWOPOW( N, POW)

	INTEGER*2 N, POW, NN, P, I 

        TWOPOW = 1
	IF (N.EQ.0) RETURN	! IN THIS CASE POW IS UNDEFINED

        NN = ABS(N)

	P = 0
	I = 1
	DO WHILE (I.LT.NN)
	  P = P+1
	  I = 2*I
	ENDDO

	IF (I.NE.NN) TWOPOW = 0

        IF (NN.NE.0) POW = P

	RETURN
	END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ztwopow.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*  This is the C Callable Subroutine for the twopow.f program */
/*  Returns a 1 if |n| is a power of 2.  pow is an optional    */
/*  argument giving first power of 2 .GE. |n|                  */

#include "xvmaininc.h"
#include "ftnbridge.h"

void ztwopow(result,n, pow)
short int *result, n, *pow;
{
     short int ptemp;

     *result=FTN_NAME(twopow)(&n, &ptemp);
     *pow=ptemp;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create twopow.imake
/* Imake file for VICAR subroutine TWOPOW */

#define SUBROUTINE twopow

#define MODULE_LIST twopow.f ztwopow.c

#define P2_SUBLIB

#define USES_C
#define USES_FORTRAN
#define FTNINC_LIST fortport

$ Return
$!#############################################################################
$Test_File:
$ create ttwopow.f
C  This program will test the FORTRAM Callable Subroutine
C  for twopow

       SUBROUTINE TTWOPOW()

       INTEGER*2 I,N,TWOPOW
       CHARACTER*40 MS1, MS2

       DO I= -16,9,5
         IF (TWOPOW(I,N).EQ.1) THEN
           WRITE(MS1, 100) I
           WRITE(MS2, 200) N
           CALL XVMESSAGE(MS1,' ')
           CALL XVMESSAGE(MS2,' ')
         ELSE
           WRITE(MS1, 100) I
           WRITE(MS2, 300) N
           CALL XVMESSAGE(MS1,' ')
           CALL XVMESSAGE(MS2,' ')
         ENDIF
       ENDDO           
 100   FORMAT ('number = ', I5)
 200   FORMAT ('  power of 2 = ', I5)
 300   FORMAT ('  next power of 2 = ', I5)
       RETURN
       END
$!-----------------------------------------------------------------------------
$ create tztwopow.c
/*  This is a program that test the C Callable Subroutine for */
/*  twopow.                                                   */


#include "vicmain_c"
#include "ftnbridge.h"

main44()
{
     short int n, pow, result;
     char msg1[40], msg2[40];

     zvmessage("Test the C interface"," ");
     zvmessage(" "," ");

     
     for (n = -16; n < 10; )
     {
          ztwopow(&result,n,&pow);

          if(result == 1)
          {
               sprintf(msg1, "number = %d", n);
               sprintf(msg2, "  power of 2 = %d", pow);

               zvmessage(msg1," ");
               zvmessage(msg2," ");
          }

          else
          {
               sprintf(msg1, "number = %d", n);
               sprintf(msg2, "  next power of 2 = %d", pow);

               zvmessage(msg1," ");
               zvmessage(msg2," ");
          }

     n=n+5;
     }

     zvmessage(" "," ");
     zvmessage("Test the FORTRAN interface"," ");
     zvmessage(" "," ");

     FTN_NAME(ttwopow)();
}
$!-----------------------------------------------------------------------------
$ create ttwopow.imake
/* Imake file for Test of VICAR subroutine TWOPOW */

#define PROGRAM ttwopow

#define MODULE_LIST ttwopow.f tztwopow.c

#define MAIN_LANG_C
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL
#define LIB_FORTRAN
#define LIB_TAE
#define LIB_P2SUB 
$!-----------------------------------------------------------------------------
$ create ttwopow.pdf
process
end-proc
$!-----------------------------------------------------------------------------
$ create tsttwopow.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
ttwopow
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create twopow.hlp
1 TWOPOW

  Logical function indicating whether a number is a power of two.
 
  FORTRAN Calling Sequence:   
                      INTEGER*2 TWOP, N, POW
                      TWOP = TWOPOW( N , POW)

  C Calling Sequence:	
                      short int result,n,pow
                      ztwopow(&result,n,&pow)

  If |N| is a power of two, then TWOPOW is 1, otherwise it is 0.

  The argument POW is optional.  If specified, it returns the first
  power of two that is greater than or equal to |N|.  If N=0, then 
  POW is undefined.

  Original Programmer: L. W. Kamp, 10 May 1985.
  Code Modified on 23 July 1993: D. D. Knight
  Current Cognizant Programmer: D. D. Knight
  Source Language: Fortran
  Ported to Unix: 23 July 1993.
$ Return
$!#############################################################################
