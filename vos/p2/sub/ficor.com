$!****************************************************************************
$!
$! Build proc for MIPL module ficor
$! VPACK Version 1.9, Monday, December 07, 2009, 16:17:32
$!
$! Execute by entering:		$ @ficor
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
$ write sys$output "*** module ficor ***"
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
$ write sys$output "Invalid argument given to ficor.com file -- ", primary
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
$   if F$SEARCH("ficor.imake") .nes. ""
$   then
$      vimake ficor
$      purge ficor.bld
$   else
$      if F$SEARCH("ficor.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake ficor
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @ficor.bld "STD"
$   else
$      @ficor.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create ficor.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack ficor.com -mixed -
	-s ficor.c -
	-i ficor.imake -
	-t tficor.f tzficor.c tficor.imake tficor.pdf tstficor.pdf -
	-o ficor.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ficor.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*  This is the FICOR subroutine that will return conversion factors

    If mode = 0 the intensity conversion factor is returned
    If mode = 1 the flux conversion factor is returned.
    If mode = 2 both are returned

    The intensity con factor is returned assuming label is in units of
    nanowatts/(cm**2, st, nm, dn) returns watts/(cm**2,st,nm,dn)        */

#include "xvmaininc.h"
#include "ftnbridge.h"
#include <zvproto.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

void zficor();

/*  This is the Fortran Callable portion of the FICOR subroutine      */

void FTN_NAME2(ficor, FICOR) ( int *inunit, char buf[7200],
			float con[2], int *mode, ZFORSTR_PARAM)
{
     ZFORSTR_BLOCK
     char c_string[7200];
     zficor(*inunit, c_string, con, *mode);
     zsc2for(c_string,7200,buf,&inunit,4,2,1, mode);
}


/*  This is the C Callable portion of the FICOR subroutine           */

void zficor( inunit, buf, con, mode)
int inunit, mode;
char buf[7200];
float con[2];
{
     int bufsize, istat,indx;
     int inc;
     char *i, *results, *ptr;
     float buf1;
     char msg[100], *rptr; 
     char *compare = " MULTIPLY DN VALUE BY";

     indx = 0;
     bufsize=7200;
     istat = zlgetlabel( inunit, buf, &bufsize);
     i = buf;

     if (istat != 1)
     {
          zvmessage(" UNABLE TO RETURN VICAR LABEL"," "); 
          exit(-1);
     }
  
     rptr = strstr(buf,compare);
     results = rptr;
  
     if (results == 0)
     {
          zvmessage(" CONV VALUE NOT FOUND"," ");
          exit(-1);
     }

     inc = (results-i)+22;

     if (mode != 1)
     {
          ptr = i+inc;
          sscanf((buf+inc), " %f " ,&buf1);
          con[0] = (buf1)*0.000000001;
          sprintf(msg,"FIRST CONV FACTOR = %11.4E", con[0]);
          zvmessage(msg," ");
          indx=1;
     }

     if (mode >= 1)
     {
          rptr = strstr((buf+inc),compare);
          results = rptr;
  
          if (results == 0)
          {
               zvmessage(" CONV VALUE NOT FOUND"," ");
               exit(-1);
          }

          inc = (results-i)+22;
          ptr = i+inc;
          sscanf((buf+inc), " %f " ,&buf1);
          con[indx] = buf1; 
          sprintf(msg,"SECOND CONV FACTOR = %11.4E", con[indx]);
          zvmessage(msg," ");
     }
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create ficor.imake
/* Imake file for VICAR subroutine FICOR */

#define SUBROUTINE ficor

#define MODULE_LIST ficor.c 

#define P2_SUBLIB

#define USES_ANSI_C
#define FTN_STRING

$ Return
$!#############################################################################
$Test_File:
$ create tficor.f
C  THIS IS A PROGRAM THAT WILL TEST THE FORTRAN CALLABLE PORTION OF THE
C  TFICOR SUBROUTINE.

	SUBROUTINE TFICOR()
 
        IMPLICIT INTEGER (A-Z)
        CHARACTER*7200 BUF
        REAL*4 X(2)
        CHARACTER*100 MSG

        CALL XVUNIT(INUNIT, 'INP', 1, STATUS,' ')
        CALL XVOPEN(INUNIT, STATUS, ' ')

        CALL FICOR(INUNIT, BUF, X, 0)
        WRITE(MSG,100) X(1)
 100    FORMAT('Mode = 0: ', E11.4)
        CALL XVMESSAGE(MSG,' ')
        CALL XVMESSAGE(' ',' ')

        CALL FICOR(INUNIT, BUF, X, 1)
        WRITE(MSG,150) X(1)
 150    FORMAT('Mode = 1: ', E11.4)
        CALL XVMESSAGE(MSG,' ')
        CALL XVMESSAGE(' ',' ')

        CALL FICOR(INUNIT, BUF, X, 2)
        WRITE(MSG,200) X(1), X(2)
 200    FORMAT('Mode = 2: ', 2E11.4)
        CALL XVMESSAGE(MSG,' ')
        CALL XVMESSAGE(' ',' ')

        RETURN
        END
$!-----------------------------------------------------------------------------
$ create tzficor.c
/*  This is a program that will tes the C Callable portion of the 
    TFICOR subroutine.                                             */

#include "vicmain_c"
#include "ftnbridge.h"

main44()
{
     int inunit,status;
     char buf[7200];
     float x[2];
     char msg[200];

     zvmessage("Test the C Interface"," ");
     zvmessage(" "," ");

     status = zvunit(&inunit, "INP", 1, 0);
     status = zvopen(inunit, 0);

     zficor(inunit, buf, x, 0);
     sprintf(msg,"Mode = 0: %6.4e", x[0]);
     zvmessage(msg," ");
     zvmessage(" "," ");

     zficor(inunit, buf, x, 1);
     sprintf(msg,"Mode = 1: %6.4e", x[0]);
     zvmessage(msg," ");
     zvmessage(" "," ");

     zficor(inunit, buf, x, 2);
     sprintf(msg,"Mode = 2: %6.4e %6.4e", x[0], x[1]);
     zvmessage(msg," ");
     zvmessage(" "," ");

     status = zvclose(inunit,0);

     zvmessage(" "," ");
     zvmessage("Test the FORTRAN Interface"," ");
     zvmessage(" "," ");

     FTN_NAME(tficor)();
     return 0;
}
 
$!-----------------------------------------------------------------------------
$ create tficor.imake
/* Imake file for Test of VICAR subroutine FICOR */

#define PROGRAM tficor

#define MODULE_LIST tficor.f tzficor.c

#define MAIN_LANG_C 
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_FORTRAN
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB 
#define LIB_LOCAL
$!-----------------------------------------------------------------------------
$ create tficor.pdf
process
parm inp string
end-proc
$!-----------------------------------------------------------------------------
$ create tstficor.pdf
procedure
refgbl $echo
refgbl $syschar
body
let _onfail="continue"
let $echo="yes"

local path type=string init="wms_test_work:[testdata.mipl.vgr]"
if ($syschar(1) = "UNIX")
    let path="/project/test_work/testdata/mipl/vgr/"
end-if

tficor &"path"f1636832.geo
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create ficor.hlp
1 FICOR

This subroutine returns the conversion value(s) from the FICOR77 label.

  Fortran Calling Sequence:  CALL FICOR(UNIT, BUF, CON, MODE)

				INTEGER*4 UNIT
				BYTE BUF 
				REAL*4 CON 
				INTEGER*4 MODE

  C Calling Sequence:  ficor(unit, buf, con, mode) 

				int unit
				unsigned char buf[7200]
                                char cbuf[7200]
				float con 
				int mode 

2 History

  Original Programmer: Joel A. Mosher
  Current Cognizant Programmer: G.Yagi
  Source Language: Originally FORTRAN buf converted to C
  Ported to Unix:  D.D. Knight   9/23/93

  Revisions:
    24 May 99  GMY  Changed "i = buf[0]" to "i = buf"

3 Operation

  This subroutine grabs the entire label (up to 7200 bytes long)
  using XLGETLABEL and then locates the conversion factors by
  searching for the characters 'MULTIPLIED DN VALUE BY'.

4 Arguments

  UNIT (unit): standard Vicar2 unit number for the image whose label is
         to be searched.

  BUF (buf): buffer to hold the label; this should contain up to 7200 bytes.
       
  CON (con): if MODE is 0 or 1, then this should be one longword (REAL*4);
       if MODE is 2, then this should be dimensioned CON(2).

  MODE (mode)

  MODE=0: the conversion factor to microwatts/cm**2/steradian/nanometer
          is returned.
  MODE=1: the conversion factor to Albedo*10000 is returned.
  MODE=2: both are returned.$ if p1.eqs."HELP" then exit
$ Return
$!#############################################################################
