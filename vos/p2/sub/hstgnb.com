$!****************************************************************************
$!
$! Build proc for MIPL module hstgnb
$! VPACK Version 1.9, Monday, December 07, 2009, 16:23:02
$!
$! Execute by entering:		$ @hstgnb
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
$ write sys$output "*** module hstgnb ***"
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
$ write sys$output "Invalid argument given to hstgnb.com file -- ", primary
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
$   if F$SEARCH("hstgnb.imake") .nes. ""
$   then
$      vimake hstgnb
$      purge hstgnb.bld
$   else
$      if F$SEARCH("hstgnb.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake hstgnb
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @hstgnb.bld "STD"
$   else
$      @hstgnb.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create hstgnb.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack hstgnb.com -mixed -
	-s hstgnb.f zhstgnb.c -
	-i hstgnb.imake -
	-t thstgnb.f tzhstgnb.c thstgnb.imake thstgnb.pdf tsthstgnb.pdf -
	-o hstgnb.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create hstgnb.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	SUBROUTINE HSTGNB(NSAMP,PIXLIN,HIST)
C	THIS SUBROUTINE GENERATES A HISTOGRAM ON BYTE DATA.
C	EACH CALL TO THE SUBROUTINE PROCESSES ONE PICTURE LINE.
C	IT IS ASSUMED THE DATA IS STILL IN PACKED FORMAT
C	ARGUMENT LIST
C	NSAMP     NUMBER OF SAMPLES IN A PICTURE LINE
C	PIXLIN    ARRAY CONTAINING ONE PICTURE LINE
C	HIST      ARRAY CONTAINING RUNNING ACCUMULATION FOR HISTOGRAM 256 WORDS
C	NRANGE    RUNNING TOTAL OF VALUES OUTSIDE THE RANGE OF 0-255
C	6/3/83 -JAM- INITIAL CODING
C	8/29/84 SP - REPLACED CALL TO MVE WITH IZEXT FUNCTION
C       10/31/94 AS - CRI MSTP S/W CONVERSION (VICAR PORTING)
C
        INCLUDE 'fortport'
	BYTE PIXLIN(*)
	INTEGER*4 HIST(256)
	DO I=1,NSAMP
	    INDEX= 1 + BYTE2INT( PIXLIN(I) )
	    HIST(INDEX)=HIST(INDEX)+1
	ENDDO	       
	RETURN
	END

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zhstgnb.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/* C-Callable Version: zhstgnb - generates a histogram on a picture	*/
/************************************************************************/

void zhstgnb( nsamp, pixlin, hist)
int nsamp;                       /* number of samples in a picture line */
void *pixlin;                    /* array containing one picture line   */
void *hist;      /* array containing running accumulation for histogram */

{
FTN_NAME2(hstgnb, HSTGNB) ( &nsamp, pixlin, hist);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create hstgnb.imake
/* Imake file for VICAR subroutine hstgnb */

#define SUBROUTINE hstgnb

#define MODULE_LIST hstgnb.f zhstgnb.c

#define P2_SUBLIB

#define USES_C
#define USES_FORTRAN
#define FTNINC_LIST fortport

$ Return
$!#############################################################################
$Test_File:
$ create thstgnb.f
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C 	   PROGRAM THSTGEN TO TEST HSTGNB
	INTEGER HIST(256),SLO,SSO
	BYTE BUF(100000)
        DATA HIST/256*0/

        CALL XVEACTION('SA',' ')
        CALL XVUNIT(IUNIT,'INP',1,STAT,' ')
        CALL XVOPEN(IUNIT,STAT,' ')
        CALL XVSIZE(SLO,SSO,NLO,NSO,NLI,NSI)
	DO LINE=SLO,SLO+NLO-1
           CALL XVREAD(IUNIT,BUF,STAT,'LINE',LINE,'SAMP',SSO,
     &                 'NSAMPS',NSO,' ')
  	   CALL HSTGNB(NSO,BUF,HIST)
	ENDDO
	CALL PRNT(4,256,HIST,' HIST=.')

        CALL ZIA(HIST,256)
	DO LINE=SLO,SLO+NLO-1
           CALL XVREAD(IUNIT,BUF,STAT,'LINE',LINE,'SAMP',SSO,
     &                 'NSAMPS',NSO,' ')
  	   CALL TZHSTGNB(NSO,BUF,HIST)
	ENDDO
	CALL PRNT(4,256,HIST,' HIST=.')

        CALL XVCLOSE(IUNIT,STAT,' ')
	RETURN
	END
$!-----------------------------------------------------------------------------
$ create tzhstgnb.c
/*  TEST PGM FOR HSTGNB */
#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tzhstgnb)(nsamp,pixlin,hist)
/* ...ARGUMENT DECLARATIONS  */

   int *nsamp;
   void *pixlin,*hist;

{
   zhstgnb(*nsamp,pixlin,hist);
   return;
}

$!-----------------------------------------------------------------------------
$ create thstgnb.imake
/* Imake file for Test of VICAR subroutine hstgnb */

#define PROGRAM thstgnb

#define MODULE_LIST thstgnb.f tzhstgnb.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL


#define LIB_TAE
#define LIB_P2SUB 
$!-----------------------------------------------------------------------------
$ create thstgnb.pdf
PROCESS
PARM INP TYPE=STRING
PARM SIZE TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
END-PROC
$!-----------------------------------------------------------------------------
$ create tsthstgnb.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
!THIS SCR FILE TESTS HSTGNB
gen A 100 100 
thstgnb A
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create hstgnb.hlp
1 HSTGNB

   This subroutine generates a histogram on a picture.

2  OPERATION

        Each call to the routine processes one picture line.  

2  CALLING SEQUENCE

    FORTRAN:  CALL HSTGNB (NSAMP, PIXLIN, HIST) byte data
    C:        STATUS = HSTGNB (NSAMP, PIXLIN, HIST) byte data

2  ARGUMENTS
 

    NSAMP    Number of samples in a picture line

    PIXLIN   Array containing one picture line
              (2 bytes/sample for HSTGEN)
              (1 byte /sample for HSTGNB)

    HIST     Array containing running accumulation for histogram
              (full word integer)
              256 words - HSTGNB
              512 words - HSTGEN

              note:  The HIST array should be set to zeros before the
                     first call is issued.


    Original Programmer: A. A. Schwartz
    Current Cognizant Programmer: S. Pohorsky
    Source Language: Fortran
    Revisions: 31-OCT-94 AMS (CRI) Made portable for UNIX

$ Return
$!#############################################################################
