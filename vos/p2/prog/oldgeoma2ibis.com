$!****************************************************************************
$!
$! Build proc for MIPL module oldgeoma2ibis
$! VPACK Version 1.8, Wednesday, June 19, 1996, 12:01:36
$!
$! Execute by entering:		$ @oldgeoma2ibis
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
$!   TEST        Only the test files are created.
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
$ write sys$output "*** module oldgeoma2ibis ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Test = ""
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
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to oldgeoma2ibis.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
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
$   if F$SEARCH("oldgeoma2ibis.imake") .nes. ""
$   then
$      vimake oldgeoma2ibis
$      purge oldgeoma2ibis.bld
$   else
$      if F$SEARCH("oldgeoma2ibis.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake oldgeoma2ibis
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @oldgeoma2ibis.bld "STD"
$   else
$      @oldgeoma2ibis.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create oldgeoma2ibis.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack oldgeoma2ibis.com -
	-s oldgeoma2ibis.f -
	-i oldgeoma2ibis.imake -
	-p oldgeoma2ibis.pdf -
	-t tstoldgeoma2ibis.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create oldgeoma2ibis.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'

      SUBROUTINE MAIN44

C*********************OLDGEOMA2IBIS**************************
C     JUL 95   SP  INITIAL RELEASE
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      IMPLICIT NONE

      INTEGER COLCOUNT, STATUS, UNIT, PS, NSAMP
      INTEGER NAH, NAV, NTIEP, IP, PARMFIL, PNL, TPTR, STAT,J, NPAR
      INTEGER IPAR (500000)
      CHARACTER*4 cmp

C==================================================================

      CALL IFMESSAGE('OLDGEOMA2IBIS version 24-JUL-95')

C..READ OLD GEOMA FORMAT

      CALL XVUNIT(PARMFIL,'INP',1,STAT,' ')
      CALL XVOPEN(PARMFIL,STAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
      CALL XVGET(PARMFIL,STAT,'NS',NSAMP, 'PIX_SIZE',PS, ' ')
      IF (PS .NE. 4  .OR.  NSAMP .NE. 900)
     .   CALL MABEND('ERROR: INPUT NOT IN OLD GEOMA FORMAT')

      CALL XVREAD(PARMFIL,IPAR,STAT,' ')
      PNL = (IPAR(1)-11) / 900
      DO J = 1, PNL
        CALL XVREAD(PARMFIL,IPAR(J*900+1),STAT,' ')
      END DO
      CALL XVCLOSE(PARMFIL,STAT,' ')

      NPAR = IPAR(1) + 1
      IP = 2
    1 IF (IP .GT. NPAR) GO TO 20
      CALL MVLC(IPAR(IP), cmp, 4)
      IF (cmp .EQ. 'NAH ') THEN
         NAH = IPAR(IP+2)
         IP = IP + 3
      ELSE IF (cmp .EQ. 'NAV ') THEN
         NAV = IPAR(IP+2)
         IP = IP + 3
      ELSE IF (cmp .EQ. 'HALF') THEN
         IP = IP + 2      !NO LONGER NEEDED - FORMAT IS IN LABEL
      ELSE IF (cmp .EQ. 'TIEP') THEN
         TPTR = IP + 2
         NTIEP = 4 * (NAH+1) * (NAV+1)
         IP = IP + NTIEP + 2
         IF (IP .GT. NPAR+1) GO TO 999
      ELSE
         GO TO 996
      END IF

      GO TO 1

20    CONTINUE

C..WRITE IN IBIS-2 FORMAT

      CALL XVUNIT(UNIT,'OUT',1,STATUS,' ')
      CALL XVSIGNAL(UNIT, STATUS, 1)     ! ABORT IF ERROR

      COLCOUNT = 4
      CALL IWRITE_TIEPOINTS(UNIT,NAH,NAV,0,IPAR(TPTR),COLCOUNT)  !WRITE FILE.

      RETURN

C     End of parameter processing
  996 CALL XVMESSAGE ('*** PARAMETER ERROR. ABEND.',' ')
      CALL ABEND
  999 CALL XVMESSAGE ('*** ERROR IN NAH, NAV, OR TIEPOINT.',' ')
      CALL XVMESSAGE ('*** ABEND.',' ')
      CALL ABEND
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create oldgeoma2ibis.imake
/***********************************************************************
                     IMAKE FILE FOR PROGRAM oldgeoma2ibis

   To Create the build file give the command:

		$ vimake oldgeoma2ibis			(VMS)
************************************************************************/
#define PROGRAM	oldgeoma2ibis
#define R3LIB

#define MODULE_LIST oldgeoma2ibis.f
#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create oldgeoma2ibis.pdf
process help=*	
PARM INP	TYPE=STRING	
PARM OUT        TYPE=STRING     

!# annot function="Vicar Data Conversion"
!# annot keywords=(IBIS,geoma,transform,VMS,"non-portable",tiepoint)

END-PROC
.TITLE
Convert (obsolete) GEOMA parameters to IBIS format
.HELP
PURPOSE:

             GEOMA no longer supports a non-IBIS second input file.  
             Old non-IBIS 'GEOMA' files (which were often used to specify
             the geometric transformation needed to correct for camera
             distortion) can be converted on the VAX or Alpha using the program
             OLDGEOMA2IBIS.
             OLDGEOMA2IBIS converts an old non-portable (VMS) "geoma"
             file into an IBIS tiepoint file.  OLDGEOMA2IBIS runs only under
             VMS because it deals with a non-portable VMS-specific input file. 
.PAGE
EXECUTION:

The following TAE command line formats show the most common usages:
      OLDGEOMA2IBIS INP=a OUT=b 
      OLDGEOMA2IBIS a b 

       Here 'a' represents the input old 'GEOMA' file.
       'b' represents an IBIS input file containing tiepoint information.

The input parameter dataset contains tiepoint parameters. This dataset must
have a standard VICAR label, and a record size of 3600. The first word of the
first record following the label is an integer specifying the number of
parameter words which follow. If one parameter record is not sufficient,
additional records may be used. 

.PAGE
WRITTEN BY:	Steve Pohorsky		July 1995

COGNIZANT PROGRAMMER:  Steve Pohorsky

REVISION HISTORY
  7-95    SP  Initial release - just a temporary program to convert those
              old "geoma" files to IBIS tiepoint files.
.end
$ Return
$!#############################################################################
$Test_File:
$ create tstoldgeoma2ibis.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
!THIS IS A TEST FILE FOR OLDGEOMA2IBIS, a program that only runs under VMS.
DCL ASS WMS_TEST_WORK:[TESTDATA.MIPL.VGR]F1636832.GPR GEO
LIST GEO (1,1,1,8) 'HEX
LIST GEO (1,10,1,8) 
OLDGEOMA2IBIS GEO I
LABEL-LIST I    !CHECK NAH,NAV MATCH  HEX VALUES (AFTER CONVERSION TO DECIMAL)
IBIS-LIST  I
end-proc
$ Return
$!#############################################################################
