$!****************************************************************************
$!
$! Build proc for MIPL module hexcon
$! VPACK Version 1.9, Monday, December 07, 2009, 16:22:31
$!
$! Execute by entering:		$ @hexcon
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
$ write sys$output "*** module hexcon ***"
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
$ write sys$output "Invalid argument given to hexcon.com file -- ", primary
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
$   if F$SEARCH("hexcon.imake") .nes. ""
$   then
$      vimake hexcon
$      purge hexcon.bld
$   else
$      if F$SEARCH("hexcon.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake hexcon
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @hexcon.bld "STD"
$   else
$      @hexcon.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create hexcon.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack hexcon.com -mixed -
	-s hexcon.c -
	-i hexcon.imake -
	-t thexcon.f tzhexcon.c thexcon.imake thexcon.pdf tsthexcon.pdf -
	-o hexcon.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create hexcon.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*===========================================================================*
 |  HEXCON.C -- Routine to convert a hexadecimal array into ASCII (for       |
 |	printed output)							     |
 *===========================================================================*/

#include "xvmaininc.h"
#include "ftnbridge.h"
#include <stdlib.h>

void zhexcon(unsigned char[], unsigned char *, int *);

/*===========================================================================*
 |  Fortran Callable Subroutine                                              |
 *===========================================================================*/

void FTN_NAME2(hexcon, HEXCON) (unsigned char ibuf[], char *obuf, int *n,
								ZFORSTR_PARAM)
{
     ZFORSTR_BLOCK
     unsigned char *c_string;
     c_string = (unsigned char *) malloc(2*(*n));
     zhexcon(ibuf,c_string, n);
     zsc2for((char *)c_string,2*(*n),obuf,c_string,3,2,1, n); 
     free(c_string);
     return;
}


/*===========================================================================*
 |  C-Callable Subroutine                                                    |
 *===========================================================================*/

void zhexcon(ibuf,obuf,n)
unsigned char ibuf[];		/* Input hexadecimal array */
unsigned char *obuf;		/* Output ASCII string */
int *n;				/* Number of bytes in ibuf */
{
	static char ascii[16]={'0','1','2','3','4','5','6','7',
			       '8','9','A','B','C','D','E','F' };
	int i,j,msbits,lsbits;

	for (i=0,j=0; i<*n; i++)
		{
 		msbits = ibuf[i]/16;
		lsbits = ibuf[i]%16;
		*obuf = ascii[msbits];
                obuf++;
                *obuf = ascii[lsbits];
                obuf++;
                }
}		
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create hexcon.imake
/***********************************************************************

                     IMAKE FILE FOR SUBROUTINE LIBRARY hexcon

   To Create the build file give the command:

	$ vimake hexcon                     (VMS)
   or
	% vimake hexcon                     (Unix)


*************************************************************************/

#define SUBROUTINE hexcon

#define MODULE_LIST hexcon.c

#define P2_SUBLIB

#define USES_ANSI_C
$ Return
$!#############################################################################
$Test_File:
$ create thexcon.f
C  THEXCON IS THE FORTRAN PROGRAM THAT TEST THE FORTRAN CALLABLE PORTION
C  OF THE HEXCON SUBROUTINE.

      SUBROUTINE THEXCON()

      INCLUDE 'fortport'

      BYTE IBUF(256) 
      CHARACTER MSG*100
      CHARACTER OBUF*512
      INTEGER*4 I,J

      DO I=1,256
          IBUF(I)=INT2BYTE(I-1)
      ENDDO

      CALL HEXCON(IBUF,OBUF,256)

      DO J=1,512,32
          WRITE(MSG,100) OBUF(J:J+31)
          CALL XVMESSAGE(MSG,' ')
100       FORMAT(A32)
      ENDDO

      END
$!-----------------------------------------------------------------------------
$ create tzhexcon.c
/*  tzhexcon is the program that test the C-Callable hexcon subroutine */

#include "vicmain_c"
#include "ftnbridge.h"

main44()
{
	unsigned char ibuf[256];
	int i,j,n;
        char obuf[512], ms1[256];

        sprintf(ms1,"Test the C interface");
        zvmessage(ms1," ");
        zvmessage(" "," ");

	for (i=0; i<256; i++) ibuf[i]=i;
        n = 256;
	zhexcon(ibuf,obuf,&n);
	for (j=0; j<512; j+=32)
		{
		for (i=0; i<32; i++) ibuf[i]=obuf[i+j];
		ibuf[32] = 0;
		sprintf(ms1,"%s",ibuf);
                zvmessage(ms1," ");
		}

        zvmessage(" "," ");
        sprintf(ms1,"Test the FORTRAN interface");
        zvmessage(ms1," ");
        zvmessage(" "," ");

        FTN_NAME(thexcon)();
}
$!-----------------------------------------------------------------------------
$ create thexcon.imake
/* Imake file for Test of VICAR subroutine hexcon */

#define PROGRAM thexcon

#define MODULE_LIST thexcon.f tzhexcon.c

#define MAIN_LANG_C
#define TEST

#define FTNINC_LIST fortport
#define USES_FORTRAN
#define USES_C

#define LIB_FORTRAN
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

$!-----------------------------------------------------------------------------
$ create thexcon.pdf
process help=*
END-PROC
$!-----------------------------------------------------------------------------
$ create tsthexcon.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
thexcon
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create hexcon.hlp
1 HEXCON

  Routine to convert a hexadecimal array into ASCII (for printed output)

  FORTRAN CALLING SEQUENCE:  CALL HEXCON(IBUF,OBUF,N)

			     BYTE IBUF(N)
                             CHARACTER  OBUF*(2*N)
			     INTEGER N

  C CALLING SEQUENCE:        hexcon(ibuf,obuf,n)

		             unsigned char ibuf[n]
                             char obuf[2*n]
                             integer n
  
  ARGUMENTS

  	IBUF (ibuf) - is the input hexadecimal array containing N bytes.
	OBUF (obuf) - is the output ASCII string containing 2*N bytes.     
        If N (n) is less than or equal to 0, then HEXCON returns immediately.

2 HISTORY

  Original Programmer:     
  Concurrent Cognizant Engineer:     9/21/93          Damon Knight
  Ported to Unix:                    9/21/93          Damon Knight
$ Return
$!#############################################################################
