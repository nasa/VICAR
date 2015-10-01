$!****************************************************************************
$!
$! Build proc for MIPL module bmss
$! VPACK Version 1.5, Monday, March 29, 1993, 13:32:55
$!
$! Execute by entering:		$ @bmss
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
$!   PDF         Only the PDF file is created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
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
$ write sys$output "*** module bmss ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Imake = ""
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
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Imake = "Y"
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
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("bmss.imake") .nes. ""
$   then
$      vimake bmss
$      purge bmss.bld
$   else
$      if F$SEARCH("bmss.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake bmss
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @bmss.bld "STD"
$   else
$      @bmss.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create bmss.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack bmss.com -
	-s bmss.f -
	-i bmss.imake -
	-p bmss.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create bmss.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
C
C**********************************************************************
C
C     BMSS will combine input images into a single 3-D BIL format image.
C     Each band of the various inputs
C     must contain the same number of lines and samples.
C
C**********************************************************************
C
      SUBROUTINE MAIN44
C
      IMPLICIT NONE
C
      INTEGER*4     I, INCNT, J, K, OUTUNIT, STATUS, TOTNB
      INTEGER*4     INUNIT(50), NB(50), NL(50), NS(50)
      REAL*4        BUF(32767)
      CHARACTER*4   INFMT(50)
C
      CALL XVPCNT('INP',INCNT)
      DO I = 1,INCNT
         CALL XVUNIT(INUNIT(I),'INP',I,STATUS,' ')
         CALL XVOPEN(INUNIT(I),STATUS,'U_FORMAT','REAL','OPEN_ACT',
     &        'SA','IO_ACT','SA',' ')
         CALL XVGET(INUNIT(I),STATUS,'NL',NL(I),'NS',NS(I),'NB',
     &        NB(I),'FORMAT',INFMT(I),' ')
      ENDDO
      DO I = 2,INCNT
         IF (NL(I).NE.NL(1)) THEN
	    CALL XVMESSAGE(
     &		' INPUTS MUST HAVE THE SAME NUMBER OF LINES',' ')
	    CALL ABEND
	 ENDIF
         IF (NS(I).NE.NS(1)) THEN
	    CALL XVMESSAGE(
     &		' INPUTS MUST HAVE THE SAME NUMBER OF SAMPLES',' ')
	    CALL ABEND
	 ENDIF
         IF (INFMT(I).NE.INFMT(1)) THEN
	    CALL XVMESSAGE(' INPUTS MUST HAVE THE SAME FORMAT',' ')
	    CALL ABEND
	 ENDIF
      ENDDO
C
      TOTNB = 0
      DO I = 1,INCNT
         TOTNB = TOTNB + NB(I)
      ENDDO
C
C  OUTPUT DATA
C
      CALL XVUNIT(OUTUNIT,'OUT',1,STATUS,' ')
      CALL XVOPEN(OUTUNIT,STATUS,'OP','WRITE','U_NL',NL(1),
     &     'U_NS',NS(1),'U_NB',TOTNB,'U_ORG','BIL','U_FORMAT',
     &     'REAL','OPEN_ACT','SA','IO_ACT','SA',' ')
C
      DO I = 1,NL(1)
         DO J = 1,INCNT
            DO K = 1,NB(J)
               CALL XVREAD(INUNIT(J),BUF,STATUS,'SAMP',1,'NSAMPS',
     &              NS(1),'LINE',I,'BAND',K,' ')
               CALL XVWRIT(OUTUNIT,BUF,STATUS,' ')
            ENDDO
         ENDDO
      ENDDO
      DO I = 1,INCNT
         CALL XVCLOSE(INUNIT(I),STATUS,' ')
      ENDDO
      CALL XVCLOSE(OUTUNIT,STATUS,' ')
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create bmss.imake
#define  PROGRAM   bmss

#define MODULE_LIST bmss.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
$PDF_File:
$ create bmss.pdf
process help=*
PARM INP TYPE=STRING COUNT=2:50
PARM OUT TYPE=STRING
END-PROC
.TITLE
BMSS
.HELP
PURPOSE:
BMSS combines up to 50 datasets into a single dataset, concatenating the
input datasets in a left to right fashion.  Each dataset may have any
number of bands.  The inputs must have the same number of lines and samples 
and be of the same format (byte, halfword, fullword, or real*4).  The output
will be in BIL organization.

EXECUTION:

Example

BMSS INP=(A,B,C) OUT=D  will put images A, B, and C side-by-side to form D.

A, B, and C must have the same number of lines and samples, but
may have varying number of bands.


WRITTEN BY:  J. R. Heyada,  13 July 1987
COGNIZANT PROGRAMMER:  J. R. Heyada

.LEVEL1
.VARIABLE INP
STRING - Input image files
.VARIABLE OUT
STRING - Output image file
.LEVEL2
.VARIABLE INP
INP specifies the input data sets.  Up to 50 are allowed.  Each must have
the same number of lines and samples but may vary in the number of bands.
.END
$ Return
$!#############################################################################
