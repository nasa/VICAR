$!****************************************************************************
$!
$! Build proc for MIPL module expand
$! VPACK Version 1.9, Monday, December 07, 2009, 16:16:52
$!
$! Execute by entering:		$ @expand
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
$ write sys$output "*** module expand ***"
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
$ write sys$output "Invalid argument given to expand.com file -- ", primary
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
$   if F$SEARCH("expand.imake") .nes. ""
$   then
$      vimake expand
$      purge expand.bld
$   else
$      if F$SEARCH("expand.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake expand
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @expand.bld "STD"
$   else
$      @expand.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create expand.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack expand.com -mixed -
	-s expand.c -
	-i expand.imake -
	-t texpand.c texpandf.f texpand.imake texpand.pdf tstexpand.pdf -
	-o expand.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create expand.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/******************************************************************************/
/*                                                                            */
/*  FORTRAN:                                                                  */
/*  call expand(buf, nsi, inc)                                                */
/*     logical*1 buf(1)                                                       */
/*     integer nsi, inc                                                       */
/*                                                                            */
/*  C:                                                                        */
/*  zexpand(buf, nsi, inc)                                                    */
/*     unsigned char buf[];                                                   */
/*     int nsi;                                                               */
/*     int inc;                                                               */
/*                                                                            */
/*  This function will magnify the length of a video line by an integral      */
/*  multiple.  Magnification is performed by replicating pixels.  The func-   */
/*  tion is useful in programs which blow up pictures for display.            */
/*                                                                            */
/*  The parameters are:                                                       */
/*     buf - a byte array containing the line of data to be expanded.  The    */
/*           array must be large enough to contain the expanded data.         */
/*     nsi - the number of input samples.                                     */
/*     inc - the magnification factor.                                        */
/*                                                                            */
/*  Cognizant Programmer:  Florance Moss                                      */
/*                                                                            */
/*  Revision History:                                                         */
/*    Date    FR #   Description                                              */
/*  --------  -----  -------------------------------------------------------  */
/*  10-21-92   ---   PDB-Ported to UNIX.  Converted from Fortran to C.        */
/*                                                                            */
/******************************************************************************/

#include "xvmaininc.h"
#include "ftnbridge.h"

void zexpand(buf, nsi, inc)
unsigned char buf[];
int nsi, inc;
{
   int index, i, j;

   index = nsi * inc - 1;
   for (i = nsi-1; i >= 0; i--) {
      for (j = 0; j < inc; j++, index--) {
         buf[index] = buf[i];
      }
   }
}

void FTN_NAME2(expand, EXPAND) (buf, nsi, inc)
unsigned char buf[];
int *nsi, *inc;
{
   zexpand(buf, *nsi, *inc);
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create expand.imake
/* Imake file for VICAR subroutine EXPAND */

#define SUBROUTINE expand

#define MODULE_LIST expand.c

#define P2_SUBLIB

#define USES_C
$ Return
$!#############################################################################
$Test_File:
$ create texpand.c
#include "vicmain_c"
#include "ftnbridge.h"

/* Program to test subroutine expand().                                       */
main44()
{
   unsigned char buf[200], msg[50];
   int nsi=100, inc=2, i, j;

   zvmessage("\nTesting EXPAND subroutine C interface\n", "");
   sprintf(msg, "nsi = %d, inc = %d\n", nsi, inc);
   zvmessage(msg, "");
   for (i = 0; i < nsi; i++)
      buf[i] = i;
   zvmessage("Original buffer 1:\n", "");
   print_buffer(buf, nsi);
   zexpand(buf, nsi, inc);
   zvmessage("Expanded buffer 1:\n", "");
   print_buffer(buf, 2*nsi);

   buf[0] = 0x00;
   buf[1] = 0xff;
   buf[2] = 0x00;
   for (i = 3; i < 8; i++)
      buf[i] = 0xff;
   for (i = 8; i < 13; i++)
      buf[i] = 0x00;
   for (i = 13; i < 50; i++)
      buf[i] = 0xff;
   for (i = 50; i < nsi; i++)
      buf[i] = 0x00;

   zvmessage("Original buffer 2:\n", "");
   print_buffer(buf, nsi);
   zexpand(buf, nsi, inc);
   zvmessage("Expanded buffer 2:\n", "");
   print_buffer(buf, 2*nsi);

   FTN_NAME(texpandf)();
   exit(0);
}

print_buffer(buf, nsi)
unsigned char buf[];
int nsi;
{
   int i, j;
   char msg[81], temp[10];

   for (i = 0; i < nsi; ) {
      msg[0] = 0;
      for (j = 0; j < 18 && i < nsi; j++, i++) {
         sprintf(temp, " %3d", (int) buf[i]);
         strcat(msg, temp);
      }
      zvmessage(msg, "");
   }
   sprintf(msg, "\n");
   zvmessage(msg, "");
}
$!-----------------------------------------------------------------------------
$ create texpandf.f
C FORTRAN ROUTINE TO TEST FORTRAN BRIDGE TO SUBROUTINE EXPAND
      SUBROUTINE TEXPANDF()
      LOGICAL*1 BUF(200)
      DATA BUF /0,255,0,5*255,5*0,37*255,150*0/
      NSI=100
      INC=2
      CALL XVMESSAGE('Testing EXPAND subroutine FORTRAN interface', ' ')
      CALL XVMESSAGE(' ', ' ')
      CALL XVMESSAGE('Original buffer:', ' ')
      CALL XVMESSAGE(' ', ' ')
      CALL PRINT_FBUFFER(BUF,100)
      CALL EXPAND(BUF,NSI,INC)
      CALL XVMESSAGE('Expanded buffer:', ' ')
      CALL XVMESSAGE(' ', ' ')
      CALL PRINT_FBUFFER(BUF,200)
      RETURN
      END

      SUBROUTINE PRINT_FBUFFER(BUF,NSI)
      IMPLICIT INTEGER(A-Z)
      LOGICAL*1 BUF(1)
      CHARACTER MSG*80
      INTEGER*2 INTBUF(200)
      INTEGER   TRBUF(12)

      CALL XVTRANS_SET(TRBUF, 'BYTE', 'HALF', STATUS)
      IF (STATUS .EQ. 1) THEN
         CALL XVTRANS(TRBUF, BUF, INTBUF, NSI)
      ELSE
         DO 10 I=1,NSI
   10       INTBUF(I) = BUF(I)
      ENDIF
      DO 20 I=1,NSI,18
         IF ((I+17).LE.NSI) THEN
            WRITE(MSG, 100) (INTBUF(J), J=I,I+17)
         ELSE
            WRITE(MSG, 100) (INTBUF(J), J=I,NSI)
         ENDIF
         CALL XVMESSAGE(MSG, ' ')
   20 CONTINUE
      CALL XVMESSAGE(' ', ' ')
      RETURN
  100 FORMAT(X,18(I3,X))
      END
$!-----------------------------------------------------------------------------
$ create texpand.imake
/* Imake file for test of VICAR subroutine EXPAND */

#define PROGRAM texpand

#define MODULE_LIST texpand.c texpandf.f

#define MAIN_LANG_C
#define TEST

#define USES_C
#define USES_FORTRAN

#define LIB_FORTRAN
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$!-----------------------------------------------------------------------------
$ create texpand.pdf
process
end-proc
$!-----------------------------------------------------------------------------
$ create tstexpand.pdf
procedure
refgbl $echo
refgbl $autousage
body
let _onfail="continue"
let $autousage="none"
let $echo="yes"
texpand
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create expand.hlp
1 EXPAND

  EXPAND will magnify the length of a video line by an integral
  multiple.  Magnification is performed by replicating pixels.
  The subroutine is useful in programs which blow up pictures 
  for display.

  Calling Sequence:  expand(buf, nsi, inc);

  Arguments:   buf   byte array containing the line to be expanded.  The
                     array must be large enough to contain the expanded data.
               nsi   number of input samples
               inc   magnification factor

2 History

  Original Programmer: 
  Current Cognizant Programmer: Florance Moss   7-12-83
  Source Language: IBM Assembly
  Conversion Language:  Fortran
  Ported to UNIX and converted to C:  Paul Bartholomew   10-20-92
$ Return
$!#############################################################################
