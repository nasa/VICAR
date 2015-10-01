$!****************************************************************************
$!
$! Build proc for MIPL module intrpa
$! VPACK Version 1.9, Monday, December 07, 2009, 16:24:17
$!
$! Execute by entering:		$ @intrpa
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
$ write sys$output "*** module intrpa ***"
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
$ write sys$output "Invalid argument given to intrpa.com file -- ", primary
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
$   if F$SEARCH("intrpa.imake") .nes. ""
$   then
$      vimake intrpa
$      purge intrpa.bld
$   else
$      if F$SEARCH("intrpa.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake intrpa
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @intrpa.bld "STD"
$   else
$      @intrpa.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create intrpa.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack intrpa.com -mixed -
	-s intrpa.f zintrpa.c -
	-i intrpa.imake -
	-t tintrpa.f tzintrpa.c tintrpa.imake tintrpa.pdf tstintrpa.pdf -
	-o intrpa.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create intrpa.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      SUBROUTINE INTRPA(ICODE,NS,BUF,I1,I2)
C SUBROUTINE USED TO FILL IN MISSING VALUES IN AN ARRAY 
C USING A LINEAR INTERPOLATION SCHEME.
C ICODE      DATA FORMAT
C   1        BYTE
C   2        INTEGER*2
C   4        INTEGER*4
C NS      IS THE NUMBER OF SAMPLES TO BE FILLED IN
C BUF     IS THE ARRAY
C I1,I2   ARE THE SAMPLE VALUES BETWEEN WHICH THE INTER-
C POLATION IS TO BE PERFORMED.
C UPON RETURN, THE ARRAY BUF WILL CONTAIN NS ELEMENTS 
C SUCH THAT BUF(1)=I1, BUF(NS)=I2, AND ALL INTERMEDIATE
C ELEMENTS ARE GENERATED BY INTERPOLATING BETWEEN I1
C AND I2.
      IMPLICIT INTEGER(A-Z)
           INTEGER*4 BUF(1)
      IF (ICODE .EQ. 1) CALL S1(ICODE,NS,BUF,I1,I2)
      IF (ICODE .EQ. 2) CALL S2(ICODE,NS,BUF,I1,I2)
      IF (ICODE .EQ. 4) CALL S4(ICODE,NS,BUF,I1,I2)
      IF (ICODE .LT. 1 .OR. ICODE .GT. 4 .OR. ICODE .EQ. 3) THEN
          CALL PRNT(4,1,ICODE,' ICODE OUT OF RANGE,ICODE=.')
      ENDIF
      RETURN
      END
      SUBROUTINE S1(ICODE,NS,BUF,I1,I2)
C     BYTE IN,BYTE OUT
      IMPLICIT INTEGER(A-Z)
      REAL*4 DY,F1,F2
      BYTE BUF(*)
      F1=I1
      F2=I2
      DY=(F2-F1)/(NS-1)
      F1=F1+0.5
      DO I=1,NS
          DN=F1                       !CONVERT NUMBER TO INTEGER
          IF (F1 .GT. 255.) DN=255
          IF (F1 .LT.  0.0) DN=0
          CALL MVE(-5,1,DN,BUF(I),1,1)!STUFF THE FULLWORD VALUE INTO A BYTE ARRAY
	                              !THE VALUES OF BUF RANGE FROM 0 TO 255
          F1=F1+DY
      ENDDO
      RETURN
      END
      SUBROUTINE S2 (ICODE,NS,BUF,I1,I2)
C     HALFWORD IN, HALFWORD OUT
      IMPLICIT INTEGER(A-Z)
      REAL*4 DY,F1,F2,MAX,MIN
      INTEGER *2 BUF(1)
      F1=I1
      F2=I2
      DY=(F2-F1)/(NS-1)
      F1=F1+0.5
      MIN=-32768.0
      MAX=32767.0
      DO I=1,NS
      IF (F1 .GT. MAX) THEN
          BUF(I)=MAX
      ELSE IF (F1 .LT. MIN) THEN
          BUF(I)=MIN
      ELSE
          BUF(I)=F1
      ENDIF
      F1=F1+DY
      ENDDO
      RETURN
      END
      SUBROUTINE S4(ICODE,NS,BUF,I1,I2)
C     FULLWORD IN, FULLWORD  OUT
      IMPLICIT INTEGER(A-Z)
      REAL*4 DY,F1,F2
      INTEGER*4 BUF(1)
      F1=I1
      F2=I2
      DY=(F2-F1)/(NS-1)
      F1=F1+0.5
      DO I=1,NS
      BUF(I)=F1
      F1=F1+DY
      ENDDO
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zintrpa.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/* C-Callable Version: zintrpa - fill in missing values in an array	*/
/*				 using linear interpolation.		*/
/************************************************************************

 Arguments of zintrpa:

 ICODE   is the data format of the array

	ICODE		FORMAT
	   1        Unsigned Char
	   2        Short Int
	   4        Int

 NS      is the number of samples to be filled.
 BUF     is the array.
 I1,I2   are the sample values between which the interpolation is to be 
	 performed. upon return, the array BUF will contain NS elements 
	 such that BUF(1)=I1, BUF(NS)=I2, and all intermediate elements 
 	 are generated by interpolating between I1 and I2.		*/

void zintrpa( icode, n, buf, i1, i2)
int icode;		/* integer code for data format		*/
int n;			/* number of samples to be filled	*/
void *buf;			/* array 				*/
int i1;			/* sample values between which inter-	*/
int i2;			/* polation is performed. 		*/

{
FTN_NAME2(intrpa, INTRPA) ( &icode, &n, buf, &i1, &i2);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create intrpa.imake
/* Imake file for VICAR subroutine intrpa */

#define SUBROUTINE intrpa

#define MODULE_LIST intrpa.f zintrpa.c

#define P2_SUBLIB

#define USES_C
#define USES_FORTRAN
#define FTNINC_LIST fortport
$ Return
$!#############################################################################
$Test_File:
$ create tintrpa.f
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     THIS IS A TEST FOR MODULE INTRPA   C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                        C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  RUN THROUGH ALL OF THE DATA TYPES     C
C  I.E. INTERPOLATE BYTE, HALF AND       C
C  FULL WORD DATA                        C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      BYTE       B(10)             !BYTE VARIABLES
      INTEGER*2  H(10)             !HALFWORD VARIABLE
      INTEGER*4  F(10)             !FULLWORD VARIABLE
      CHARACTER  CARD*80           !PRINT BUFFER

      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE('**BEGIN TEST OF MODULE INTRPA**',' ')


      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE('**BYTE DATA**',' ')
      CALL INTRPA(1, 10, B, 1, 10)
      WRITE(CARD, 100) B
      CALL XVMESSAGE(CARD,' ')

      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE('**HALFWORD DATA**',' ')
      CALL INTRPA(2, 10, H, 1, 10)
      WRITE(CARD, 100) H
      CALL XVMESSAGE(CARD,' ')

      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE('**FULLWORD DATA**',' ')
      CALL INTRPA(4, 10, F, 1, 10)
      WRITE(CARD, 100) F
      CALL XVMESSAGE(CARD,' ')

      CALL XVMESSAGE('**END TEST OF MODULE INTRPA**',' ')
      CALL XVMESSAGE(' ',' ')

      CALL XVMESSAGE(
     + 'Repeat test cases in C to test C interface: zintrpa',' ')
      call tzintrpa
      RETURN
 100  FORMAT(1X, 10Z5.2)

      END
$!-----------------------------------------------------------------------------
$ create tzintrpa.c
#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/* 	*/
/************************************************************************/

/*  	DCODE......Transfer mode
	          1  = Move byte array to byte array
                  2  = Move halfword to halfword
                  3  = Move byte to halfword
                  4  = Move fullword to fullword
                  5  = Move byte to fullword
                  6  = Move halfword to fullword
                  7  = Move real (single) to real.
                  8  = Move double to double.
                  9  = Move real to double
	           negative values -1 to -9 reverse of above.	  */

void FTN_NAME(tzintrpa)() 
{
	unsigned char  	b[10];		/* Byte variables	*/
	short int	h[10];		/* Halfword variable	*/
	int		f[10];		/* Fullword variable	*/
	char		card[100];	/* Print buffer		*/
	char		string[30];	/* String for concaten.	*/

	zvmessage(" "," ");
	zvmessage("**BEGIN TEST OF MODULE INTRPA**"," ");
	zvmessage(" "," ");

 	zvmessage("**BYTE DATA**"," ");
	zintrpa(1, 10, b, 1, 10);
	sprintf(card,"    %02X   %02X   %02X   %02X   %02X   %02X   %02X",
		b[0],b[1],b[2],b[3],b[4],b[5],b[6]);
	sprintf(string,"   %02X   %02X   %02X",b[7],b[8],b[9]);
	strcat(card,string);
	zvmessage(card," ");
	zvmessage(" "," ");

 	zvmessage("**HALFWORD DATA**"," ");
 	zintrpa(2, 10, h, 1, 10);
      	sprintf(card,"    %02X   %02X   %02X   %02X   %02X   %02X   %02X",
		h[0],h[1],h[2],h[3],h[4],h[5],h[6]);
      	sprintf(string,"   %02X   %02X   %02X",h[7],h[8],h[9]);
	strcat(card,string);
	zvmessage(card," ");
	zvmessage(" "," ");

 	zvmessage("**FULLWORD DATA**"," ");
 	zintrpa(4, 10, f, 1, 10);
      	sprintf(card,"    %02X   %02X   %02X   %02X   %02X   %02X   %02X",
		f[0],f[1],f[2],f[3],f[4],f[5],f[6]);
      	sprintf(string,"   %02X   %02X   %02X",f[7],f[8],f[9]);
	strcat(card,string);
	zvmessage(card," ");

	zvmessage("**END TEST OF MODULE INTRPA**"," ");
      	zvmessage(" "," ");

}
$!-----------------------------------------------------------------------------
$ create tintrpa.imake
/* Imake file for Test of VICAR subroutine intrpa */

#define PROGRAM tintrpa

#define MODULE_LIST tintrpa.f tzintrpa.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$!-----------------------------------------------------------------------------
$ create tintrpa.pdf
PROCESS
END-PROC
$!-----------------------------------------------------------------------------
$ create tstintrpa.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
tintrpa
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create intrpa.hlp
1 INTRPA

PURPOSE: Fills the elements of an array with values using a
         linear interpolation scheme.

USAGE:  CALL INTRPA(ICODE, NS, BUF, I1, I2)

PARAMETERS:

    ICODE  = Data type code for BUF (See table below).
    NS     = Number of data elements in BUF to fill.
    BUF    = Output array containing interpolated values.
             BUF(1) will contain I1 and BUF(NS) will contain
             I2. Intermediate elements are generated by in-
             terpolation betwen I1 and I2.
    I1     = Lower value of interpolation range.
    I2     = Upper value of interpolation range.





    DATA TYPES
    =================
    ICODE    BUF
    =================
      1      BYTE    
      2      HALF
      4      FULL
    =================

2 NOTES

DESCRIPTION

  This FORTRAN routine provides the FORTRAN or C programmer with a
  function to fill-in missing data in an array, by means of linear
  interpolation, for byte, halfword and fullword data types.

HISTORY

  Original Programmer: J.J. Lorre  11/26/73
  Converted to Vax by: Joel Mosher 06/01/83
  Current Cog Progr:   J.R. Stagner
  Ported to UNIX/VMS:  J.McNeill   03/26/93

$ Return
$!#############################################################################
