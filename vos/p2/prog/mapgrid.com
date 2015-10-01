$!****************************************************************************
$!
$! Build proc for MIPL module mapgrid
$! VPACK Version 1.9, Tuesday, January 15, 2013, 17:24:23
$!
$! Execute by entering:		$ @mapgrid
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
$ write sys$output "*** module mapgrid ***"
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
$ write sys$output "Invalid argument given to mapgrid.com file -- ", primary
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
$   if F$SEARCH("mapgrid.imake") .nes. ""
$   then
$      vimake mapgrid
$      purge mapgrid.bld
$   else
$      if F$SEARCH("mapgrid.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake mapgrid
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @mapgrid.bld "STD"
$   else
$      @mapgrid.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create mapgrid.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack mapgrid.com -mixed -
	-s mapgrid.f -
	-i mapgrid.imake -
	-p mapgrid.pdf -
	-t tstmapgrid.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create mapgrid.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
c   10 Jan 2013  ...lwk... fixed CHARACTER continuation line for new compiler flag on Solaris
C   18 Nov   1993 ...FFM... Made portable
C   15 APRIL 1987 ...FFM... Increase buffer size (fix FR 30176)
C   12 Jun 1986 ...FFM... Convert to Vicar2 I/O
c   14 Feb 1985 ...LWK... fix Vicar1* label processing; disable HALF/BYTE
c                         parameters
C   27 JUNE 1975...DAH...CHANGES FOR CONVERSION TO 360/OS
C   18 NOV 71 - JBS - FIX STUPID MISTAKE IN PREVIOUS UPDATE
C  10/6/71 JMM61     HALFWORD
C     PROGRAM TO INSERT MAP GRID
C     MFLAG=0.....MODULATED GRID WITH DEFAULTED VALUE 64
C     MFLAG=1.....MODULATED GRID WITH SPECIAL VALUE
C     MFLAG=2.....BLACK AND WHITE GRID
C     HMODE=0.....BYTE IMAGE
C     HMODE=1.....HALFWORD IMAGE

      CHARACTER*27  MSG1/'**** MAPGRID LINES TRUNATEd'/
      CHARACTER*30  MSG2/'**** MAPGRID SAMPLES TRUNCATEd'/
      CHARACTER*26  MSG3/'**** MAPGRID run COMPLETED'/
      CHARACTER*49  MSG4/'**** MAPGRID-REQUESTED AREA OUTSIDE INPUT PICTURE'/
      LOGICAL XVPTST
      CHARACTER*32 TYPE
      CHARACTER*7 FMT 
      INTEGER*4 OUTUNIT
      INTEGER*2 LOUT(8000,2)
      INTEGER MAX,HMODE,HMAX,MOD
      INTEGER SSO,SLO,MFLAG
      INTEGER FORMAT
      DATA    MAX/255/,HMODE/0/,HMAX/511/
      DATA    MFLAG/0/

      CALL XVMESSAGE('MAPGRID version 10-Jan-13',' ')
C
C     OPEN INPUT DATA
      CALL XVUNIT(INUNIT,'INP',1,ISTATUS,' ')
      CALL XVOPEN(INUNIT,ISTATUS,'OPEN_ACT','SA','IO_ACT','SA',
     + 'U_FORMAT','HALF',' ') 
C
C     OPEN OUTPUT DATA
      CALL XVUNIT(OUTUNIT,'OUT',1,ISTATUS,' ')
      CALL XVOPEN(OUTUNIT,ISTATUS,'OP','WRITE','OPEN_ACT','SA',
     +'IO_ACT','SA','U_FORMAT','HALF',' ')
C
C     FIND OUT INPUT FORMAT
      CALL XVGET(INUNIT,ISTAT,'PIX_SIZE',IBYTPIX,'FORMAT',FMT,'TYPE',
     +TYPE,' ')
      IF (IBYTPIX .EQ. 2) HMODE=1 

C
C        'BLAC - BLACK AND WHITE GRID'
      IF (XVPTST('BLACK')) MFLAG=2
C
C        'MODU - MODULATED GRID WIRH SPECIFIED VALUE MOD
      CALL XVPARM('MODU',MOD,ICOUNT,IDEF,1)
      IF (IDEF .EQ. 0) MFLAG=1

C
C     ADJUST SIZE
      CALL XVSIZE(SLO,SSO,NLO,NSO,NLI,NSI)
      SSO=SSO-1
      IF((SSO+NSO).GT.NSI) THEN
          NSO=NSI-SSO
          CALL XVMESSAGE(MSG2,' ')
          IF(NSO.LE.0) CALL MABEND(MSG4)
      END IF
      SLO=SLO-1
      IF((SLO+NLO).GT.NLI) THEN
          NLO=NLI-SLO
          CALL XVMESSAGE(MSG1,' ')
          IF(NLO.LE.0) CALL MABEND(MSG4)
      END IF
      ISW=2
      SSO = SSO + 1
      SLO = SLO + 1
      FORMAT=1-HMODE
      IF(HMODE.EQ.1)MAX=HMAX
      IF(MFLAG.EQ.0)MOD=64
      IF(MFLAG.EQ.2)MOD=MAX
      IF(FORMAT.EQ.1)CALL XVMESSAGE('BYTE INPUT',' ')
      IF(HMODE.EQ.1)CALL XVMESSAGE('HALFWORD INPUT',' ')
      DO 20 I=1,NLO
      ISW=3-ISW
      CALL XVREAD(INUNIT,LOUT(1,ISW),ISTATUS,'LINE',SLO,'SAMP',SSO,
     +'NSAMPS',NSO,' ')
      SLO=0
    7 IF(25*(I/25).EQ.I) GO TO 13
      NTEST=(I+4)/5
      IF(2*(NTEST/2).EQ.NTEST) GO TO 10
      DO 8 J=25,NSO,50
      NLOG=LOUT(J,ISW)
    8 LOUT(J,ISW)=MIN0(NLOG+MOD,MAX)
      DO 80 J=50,NSO,50
      NLOG=LOUT(J,ISW)
   80 LOUT(J,ISW)=MAX0(NLOG-MOD,0)
   81 CONTINUE
      DO 9 J=100,NSO,100
      NLOG=LOUT(J,ISW)
    9 LOUT(J,ISW)=MAX0(NLOG-MOD,0)
      GO TO 19
   10 DO 11 J=25,NSO,50
      NLOG=LOUT(J,ISW)
   11 LOUT(J,ISW)=MAX0(NLOG-MOD,0)
      DO 110 J=50,NSO,50
      NLOG=LOUT(J,ISW)
  110 LOUT(J,ISW)=MIN0(NLOG+MOD,MAX)
  111 CONTINUE
      DO 12 J=100,NSO,100
      NLOG=LOUT(J,ISW)
   12 LOUT(J,ISW)=MIN0(NLOG+MOD,MAX)
      GO TO 19
   13 IF(100*(I/100).EQ.I) GO TO 16
      NTEST=I/25
      IF(2*(NTEST/2).EQ.NTEST) GO TO 150
      DO 14 J=1,NSO,10
      M=J+4
      DO 14 K=J,M
      NLOG=LOUT(K,ISW)
   14 LOUT(K,ISW)=MIN0(NLOG+MOD,MAX)
      DO 15 J=6,NSO,10
      M=J+4
      DO 15 K=J,M
      NLOG=LOUT(K,ISW)
   15 LOUT(K,ISW)=MAX0(NLOG-MOD,0)
      GO TO 81
  150 DO 151 J=1,NSO,10
      M=J+4
      DO 151 K=J,M
      NLOG=LOUT(K,ISW)
  151 LOUT(K,ISW)=MAX0(NLOG-MOD,0)
      DO 152 J=6,NSO,10
      M=J+4
      DO 152 K=J,M
      NLOG=LOUT(K,ISW)
  152 LOUT(K,ISW)=MIN0(NLOG+MOD,MAX)
      GO TO 111
   16 DO 17 J=1,NSO,10
      M=J+4
      DO 17 K=J,M
      NLOG=LOUT(K,ISW)
  17  LOUT(K,ISW)=MAX0(NLOG-MOD-MOD,0)
      DO 18 J=6,NSO,10
      M=J+4
      DO 18 K=J,M
      NLOG=LOUT(K,ISW)
   18 LOUT(K,ISW)=MIN0(NLOG+MOD+MOD,MAX)
   19 CONTINUE
      CALL XVWRIT(OUTUNIT,LOUT(1,ISW),ISTATUS,'NSAMPS',NSO,' ')
   20 CONTINUE
      CALL XVMESSAGE(MSG3,' ')
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create mapgrid.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM mapgrid

   To Create the build file give the command:

		$ vimake mapgrid			(VMS)
   or
		% vimake mapgrid			(Unix)


************************************************************************/


#define PROGRAM	mapgrid
#define R2LIB

#define MODULE_LIST mapgrid.f 

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create mapgrid.pdf
process help=*
 PARM INP TYPE=STRING
 PARM OUT TYPE=STRING
 PARM SIZE TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
 PARM BLAC TYPE=KEYWORD COUNT=0:1 VALID=BLACK DEFAULT=--
 PARM MODU TYPE=INTEGER DEFAULT=64
 END-PROC
.TITLE
 MAPGRID
.HELP
 PURPOSE:
 "MAPGRID" is a vicar applications program which will
 insert a reference grid into a picture.  The interval
 between lines of the grid is 25 pixels, and each grid
 line consists of alternating light and dark bars which
 are 1 pixel wide and 5 pixels long.
 Two modes exist for generating the DN intensities of
 the bars.  In the first, the light and dark bars
 alternate between intensities of 0 DN and 255 DN.
 In the second mode, the intensities of the bars are
 determined by adding a constant to and subtracting it
 from the corresponding pixel DN.  In this mode, every
 fourth grid line is emphasized by using twice the
 constant value in the additions and subtractions.

 EXECUTION:
   The following is the execution statement for MAPGRID
            MAPGRID  INP  OUT  PARAMS
 where INP, OUT, and PARAMS are parameters discussed in their
 respective parameter section in TUTOR mode.

 WRITTEN BY: T. C. RINDFLEISCH		7/22/69
 CONVERTED TO VAX BY:  FLORANCE MOSS	4/29/83
 CONVERTED TO VICAR2 BY:  FLORANCE MOSS  6/19/86
 MADE PORTABLE:           FLORANCE MOSS  11/29/93

.LEVEL1
.VARI INP
   input data set
.VARI OUT
   output data set
.VARI SIZE
   image size
.VARI BLAC
   black and white grid
.VARI MODU
   modulated grid
.LEVEL2
.VARI INP
   input image file name
.VARI OUT
   Output image file name.  The output picture consists of the
   input with the appropriate grid patern superimposed.
.VARI SIZE
   (SL,SS,NL,NS)  SL and SS are, respectively the starting data
   record and starting byte in the input picture at which processing
   is to begin.  NL and NS are, respectively, the number of data
   record and the number of bytes to be processed from the input
   data set.  When the size field is defaulted, values for SL and
   SS of 1 will be assumed, and values for NL and NS will be assigned
   equal to the values in the input data set system label.  In this
   way,the output data will be equal in size to the input data.
.VARI BLAC
   This keyword causes a 0 DN and 255 DN grid pattern to be used.
.VARI MODU
   This keyword causes a modulated grid patern to be used with the
   specicied integer value.  The integer represents the constant for
   modulation.  If neither BLAC nor MODU is specified, the modulated
   grid patern is used with the integer value defaulted to 64.
$ Return
$!#############################################################################
$Test_File:
$ create tstmapgrid.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
!THIS IS A TEST OF MAPGRID
!TEST ON BYTE IMAGE FROM NOW ON
!GENERATE A 150 BY 150 IMAGE
GEN NL=150 NS=150 OUT=GEN
!PRINT OUT THE SYSTEM LABEL OF THE GENERATED IMAGE
LABEL-LIST INP=GEN
!PRINT OUT THE PIXEL VALUE OF THE GENERATED IMAGE
LIST GEN (1,1,5,5)
!TEST WITH DEFAULTED PARAMETERS
MAPGRID INP=GEN OUT=MAP1.DAT
!R2LIB:MAPGRID INP=GEN OUT=MAP11.DAT
!DIFPIC (MAP1.DAT,MAP11.DAT) A
!MAKE SURE THE OUTPUT LABEL REMAIN THE SAME
LABEL-LIST INP=MAP1.DAT
!LABEL-LIST INP=MAP11.DAT
!TEST THE PIXEL VALUES AROUND AND AT GRID LINE
LIST INP=MAP1.DAT SIZE=(20,20,10,10)
LIST MAP1.DAT (95,95,10,10)

!TEST PARM BLAC WITH BYTE IMAGE
MAPGRID INP=GEN OUT=MAP2.DAT 'BLAC
!R2LIB:MAPGRID INP=GEN OUT=MAP22.DAT 'BLAC
!DIFPIC (MAP2.DAT,MAP22.DAT) A
!MAKE SURE THE GRID PATERN PARAM DOESN'T AFFECT THE LABEL
LABEL-LIST INP=MAP2.DAT
!LABEL-LIST INP=MAP22.DAT
!FIND OUT WHAT THE BLAC REALLY DO
LIST MAP2.DAT (20,20,10,10)

!TEST THE MODU PATERN WITH SPECIFIED CONSTANT
MAPGRID INP=GEN OUT=MAP3.DAT MODU=10
!R2LIB:MAPGRID INP=GEN OUT=MAP33.DAT MODU=10
!DIFPIC (MAP3.DAT,MAP33.DAT) A
!FIND OUT WHAT THE SPECIFIED CONSTANT DO
LIST MAP3.DAT (20,20,10,10)
LIST MAP3.DAT (95,95,10,10)

!NOW TEST ON HALF IMAGE
!GENERATE A 150 BY 150 HALFWORD IMAGE
GEN NL=150 NS=150 OUT=GEN FORMAT=HALF
LABEL-LIST INP=GEN
LIST GEN (1,1,5,10)
!TEST WITH DEFAULTED PARAMTERS
MAPGRID INP=GEN OUT=MAP4.DAT
!R2LIB:MAPGRID INP=GEN OUT=MAP44.DAT
!DIFPIC (MAP4.DAT,MAP44.DAT) A
LIST MAP4.DAT (20,20,10,20)

!TEST PARAM BLAC
MAPGRID INP=GEN OUT=MAP5.DAT 'BLAC
!R2LIB:MAPGRID INP=GEN OUT=MAP55.DAT 'BLAC
!DIFPIC (MAP5.DAT,MAP55.DAT) A
LIST MAP5.DAT (20,20,10,20)

!TEST PARAM MODU WITH SPECIAL VALUE
MAPGRID INP=GEN OUT=MAP6.DAT MODU=10
!R2LIB:MAPGRID INP=GEN OUT=MAP66.DAT MODU=10
!DIFPIC (MAP6.DAT,MAP66.DAT) A
LIST MAP6.DAT (20,20,10,20)
LIST MAP6.DAT (95,95,10,20)
end-proc
$ Return
$!#############################################################################
