$!****************************************************************************
$!
$! Build proc for MIPL module blempic
$! VPACK Version 1.8, Monday, June 09, 1997, 12:10:36
$!
$! Execute by entering:		$ @blempic
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
$ write sys$output "*** module blempic ***"
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
$ write sys$output "Invalid argument given to blempic.com file -- ", primary
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
$   if F$SEARCH("blempic.imake") .nes. ""
$   then
$      vimake blempic
$      purge blempic.bld
$   else
$      if F$SEARCH("blempic.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake blempic
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @blempic.bld "STD"
$   else
$      @blempic.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create blempic.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack blempic.com -
	-s blempic.f -
	-i blempic.imake -
	-p blempic.pdf -
	-t tstblempic.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create blempic.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
C VICAR PROGRAM BLEMPIC
C
C           BLEMPIC BLEMFILE (PBLEM,SAT,TBLEM) NL=800 NS=800
C
C Create images of the permanent blemishes PBLEM, low-full-well pixels
C SAT, and all the blemishes TBLEM (both permanent blemishes and low-full-well
C pixels).  Output image sizes are NLxNS.
C
C 9-95 AS  ...CRI... MSTP S/W CONVERSION (VICAR PORTING)
C 6-97 SP            Modified per Cassini.  Changed 255
C                    to INT2BYTE(255).

      SUBROUTINE MAIN44
      COMMON/C1/BLEM(4,5000),PBLEM(4096),SAT(4096),TBLEM(4096),HIS(4096)
      INTEGER*2 BLEM
      BYTE PBLEM,SAT,TBLEM
      INTEGER*4 SAMP,CLASS,OUNIT(3),HIS,SLO,SSO
      INCLUDE 'fortport'

      CALL IFMESSAGE('BLEMPIC version 9-June-97')
      CALL XVUNIT(IUNIT,'INP',1,IND,' ')
      CALL XVOPEN(IUNIT,IND,'OPEN_ACT','SA','IO_ACT','SA',' ')
      CALL XVGET(IUNIT,IND,'NS',NSI,' ')
      CALL XVREAD(IUNIT,BLEM,IND,' ')	!Read in blemishes
      NBLEM = NSI/4			!Number of blemishes in input file

      CALL XVSIZE(SLO,SSO,NLO,NSO,NLI,NSI)
      DO I=1,3
          CALL XVUNIT(OUNIT(I),'OUT',I,IND,' ')
          CALL XVOPEN(OUNIT(I),IND,'OP','WRITE','U_NL',NLO,'U_NS',NSO,
     &    'U_FORMAT','BYTE','O_FORMAT','BYTE',
     &    'OPEN_ACT','SA','IO_ACT','SA',' ')
      ENDDO
      CALL XLADD(OUNIT(1),'HISTORY','BLEMGEN','PERMANENT BLEMISHES',
     &    IND,'FORMAT','STRING',' ')
      CALL XLADD(OUNIT(2),'HISTORY','BLEMGEN','LOW FULL-WELL PIXELS',
     &    IND,'FORMAT','STRING',' ')
      CALL XLADD(OUNIT(3),'HISTORY','BLEMGEN','TOTAL BLEMISHES',
     &    IND,'FORMAT','STRING',' ')

      CALL ZIA(HIS,4096)		!Zero out histogram of saturated DN
      NSX = (NSO+3)/4
      NPBLEM = 0		!number of permanent blemishes
      NSAT = 0			!number of low full-well pixels
      NUNCLASS = 0		!number of unclassified blemishes
      NDCBLEM = 0		!number of double column blemishes
      N = 1
C
      DO 100 L=1,NLO
      CALL ZIA(PBLEM,NSX)
      CALL ZIA(SAT,NSX)
      CALL ZIA(TBLEM,NSX)

   30 LINE = BLEM(1,N)
      SAMP = BLEM(2,N)
      CLASS = BLEM(3,N)
      ISAT = BLEM(4,N)
      IF (LINE.EQ.L) THEN
          IF (CLASS.EQ.0) THEN
               TBLEM(SAMP) = INT2BYTE(255)
               NUNCLASS = NUNCLASS + 1
          ELSE
               IF (CLASS.GT.15) NDCBLEM=NDCBLEM+1
               TBLEM(SAMP) = INT2BYTE(CLASS)
          ENDIF
          IF (ISAT.GT.0) THEN
               SAT(SAMP)=INT2BYTE(ISAT)
               HIS(ISAT+1) = HIS(ISAT+1) + 1
               NSAT = NSAT + 1
          ELSE
               PBLEM(SAMP) = TBLEM(SAMP)
               NPBLEM = NPBLEM + 1
          ENDIF
          N = N + 1
          IF (N.LE.NBLEM) GOTO 30          
          N = N - 1
      ENDIF

      CALL XVWRIT(OUNIT(1),PBLEM,IND,' ')
      CALL XVWRIT(OUNIT(2),SAT,IND,' ')
      CALL XVWRIT(OUNIT(3),TBLEM,IND,' ')
  100 CONTINUE

      CALL PRNT(4,1,NPBLEM,'Number of permanent blemishes=.')
      CALL PRNT(4,1,NSAT,'Number of low full-well pixels=.')
      CALL PRNT(4,1,NUNCLASS,'Number of unclassified blemishes=.')
      IF (NDCBLEM.GT.0)
     & CALL PRNT(4,1,NDCBLEM,'***Number of double column blemishes=.')
      CALL PRNT(4,1,NBLEM,'Total number of blemishes=.')
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE
     &     ('Histogram of saturation DN of low full-well pixels',' ')
      IF (NSAT.GT.0) CALL PHIST(HIS,NSAT,0,255,0,0)
      CALL XVMESSAGE('BLEMPIC task completed',' ')
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create blempic.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM blempic

   To Create the build file give the command:

		$ vimake blempic			(VMS)
   or
		% vimake blempic			(Unix)


************************************************************************/


#define PROGRAM	blempic
#define R2LIB

#define MODULE_LIST blempic.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define FTNINC_LIST fortport

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create blempic.pdf
PROCESS HELP=*
 PARM INP     TYPE=STRING	COUNT=1
 PARM OUT     TYPE=STRING	COUNT=3
 PARM NL      TYPE=INTEGER	COUNT=1
 PARM NS      TYPE=INTEGER	COUNT=1 
END-PROC
.TITLE
VICAR2 PROGRAM "blempic"
.HELP
PURPOSE:

"blempic" creates images of the permanent blemishes, low-full-well
pixels, and total blemishes, as recorded in the blemish file output by
"blemgen".  The program is intended as a diagnostic tool for supporting
radiometric calibration of CCD camera systems and was initially written
for the Galileo project.

EXECUTION:

    Example:  blempic INP=BLEMFILE OUT=(PBLEM,SAT,TBLEM) NL=800 NS=800

where BLEMFILE is the input blemish file created by "blemgen", PBLEM is
an image of the permanent blemishes, SAT is an image of the low-full-well
pixels, and TBLEM is an image of all blemishes (permanent blemishes and
low-full-well pixels).  BLEM, SAT, and TBLEM are of size NLxNS (800x800
in this example).

All parameters are required.  The output picture format (NLxNS) should
be the same as the picture format of the camera.

.page
OPERATION:

The image PBLEM contains a non-zero DN at the (line,sample) location of
each permanent blemish, and zeroes in the remaining pixels:

	DN(i,j) = 0    if the pixel at picture coordinate (i,j) is not
                       a permanent blemish.
	DN(i,j) = n    if the pixel is a permanent blemish of class n.
	DN(i,j) = 255  if the pixel is an unclassified permanent blemish.

See "blemgen" help file for blemish classification skeem.

The image SAT contains a non-zero DN at the (line,sample) location of
each low-full-well pixel, and zeroes in the remaining pixels:

	DN(i,j) = n    if the pixel saturates at DN=n.
        DN(i,j) = 0    if the pixel is normal or a permanent blemish.

The image TBLEM is similar to PBLEM (i.e. the DN values represent blemish
classification numbers) except that it includes all blemishes (permanent
blemishes and low-full-well pixels).  This image characterizes the case
of a high-exposure frame in which all low-full-well pixels are saturated.

"blempic" prints out the following statistics:

	1) number of permanent blemishes
	2) number of low-full-well pixels
	3) number of unclassified blemishes
	4) total number of blemishes
        5) histogram of the DN at which low-full-well pixels saturate

.page
HISTORY:

Written by: Gary Yagi		18 Aug 88
Current Cognizant Programmer: Gary Yagi
Revisions:
 08 Jun 97  S. Pohorsky Added corrections for Cassini.
 08 May 95  A. Scop     (CRI) Made portable for UNIX
 18 Nov 88  G. Yagi     Fix documentation of output TBLEM image.
 16 Oct 88  G. Yagi	Add test to PHIST call.

.LEVEL1
.VARIABLE INP
TYPE=STRING COUNT=1
Blemish file
created by
BLEMGEN
.VARIABLE OUT
TYPE=STRING COUNT=3
OUT=(PBLEM,SAT,TBLEM)
where
PBLEM=permanent blemishes
SAT=low-full-well pixels
TBLEM=total blemishes
.VARIABLE NL
TYPE=INTEGER
Number of lines
in output image
.VARIABLE NS
TYPE=INTEGER
Number of samples
in output image
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstblempic.pdf
procedure
refgbl $echo
refgbl $autousage
refgbl $syschar
LOCAL DIR    TYPE=STRING 
LOCAL INPIC   TYPE=STRING
body
  if ($syschar(1) = "VAX_VMS")
    let DIR = "wms_test_work:[testdata.mipl.gll]"
  else ! Unix
    let DIR   = "/project/test_work/testdata/mipl/gll/"
  end-if
let $autousage="none"
let _onfail="continue"
let $echo="yes"
let INPIC= "&DIR"//"blem889.100"
blempic &INPIC (PBLEM,SAT,TBLEM) NL=800 NS=800
hist PBLEM
hist SAT
hist TBLEM
end-proc
$ Return
$!#############################################################################
