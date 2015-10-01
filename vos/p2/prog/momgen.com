$!****************************************************************************
$!
$! Build proc for MIPL module momgen
$! VPACK Version 1.9, Monday, December 07, 2009, 16:44:44
$!
$! Execute by entering:		$ @momgen
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
$ write sys$output "*** module momgen ***"
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
$ write sys$output "Invalid argument given to momgen.com file -- ", primary
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
$   if F$SEARCH("momgen.imake") .nes. ""
$   then
$      vimake momgen
$      purge momgen.bld
$   else
$      if F$SEARCH("momgen.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake momgen
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @momgen.bld "STD"
$   else
$      @momgen.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create momgen.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack momgen.com -mixed -
	-s momgen.f -
	-i momgen.imake -
	-p momgen.pdf -
	-t tstmomgen.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create momgen.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C VICAR program MOMGEN
C Radiometric calibration program.  Extracts moments from specified areas
C of input images and stores them in the Light Transfer File.
C
C        MOMGEN (E1,E2,...EN) LTFILE PARAMS
C        or
C        MOMGEN LIST LTFILE PARAMS
C
C LIGHT TRANSFER FILE FORMAT:
C
C The Light Transfer File (LTF) is in VICAR format, and consists of VICAR
C labels containing num_areas, num_expos and an array of the exposures,
C followed by one record for each exposure level of the light transfer or
C reciprocity sequence.
C
C The label array areas contains the size fields for each area
C specified (see LTGEN):
C
C       where   AREA(1,K)=starting line for area K
C               AREA(2,K)=starting sample
C               AREA(3,K)=number of lines
C               AREA(4,K)=number of samples
C
C The exposure time (msec)is in the label of the LTF file.
C Each exposure record contains the number of input frames at that
C exposure (NI), and the moments for each area specified:
C
C       NI,OUT(3*NI*NAREA)
C
C where the array OUT consists of moment information in the following order:
C
C        SUM(1),SUM(2),...,SUM(NI)          sum of DNs for area 1
C        SUM(1),SUM(2),...,SUM(NI)          sum of DNs for area 2
C          .    .          .
C          .    .          .
C        SUM2(1),SUM2(2),...,SUM2(NI)       sum of squares for area 1
C        SUM2(1),SUM2(2),...,SUM2(NI)       sum of squares for area 2
C          .    .          .
C          .    .          .
C        SUMXY(1),SUMXY(2),...,SUMXY(NI)    sum of cross terms for area 1
C        SUMXY(1),SUMXY(2),...,SUMXY(NI)    sum of cross terms for area 2
C          .    .          .
C          .    .          .
C The exposure records are arranged in order of increasing exposure.
C If an extended exposure dark current record is present, it occurs
C first (with EXPO=-1.0), followed by the normal DC frame (EXPO=0.0),
C followed by the lowest to highest exposure levels.
C
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
         IMPLICIT REAL*8 (A-H,O-Z)

         REAL*4 OUT(8192),EXPO,EXP(100)
         REAL*8 SUM(50),SUM2(50),SUMXY(50)

         INTEGER IUNI(50),OUNI,SS,SL,NS,NL,NLA,NSA,STAT,NAREA,NEXP
         INTEGER*4 AREA(4,400)
         integer*4 INSTANCE(30)
         INTEGER*2 E(1024,50)

         LOGICAL DBUG,XVPTST
      
         CHARACTER*1000 MSG
         character*9 tasks(30)
         character*80 lfname,fn
 
C       Maximum number samples in input image=1024
C       Maximum number of areas=400
C       Maximim number inputs=50


         CALL IFMESSAGE ('MOMGEN Version 19-MAR-1997')
 
         DBUG = XVPTST('DBUG')
         CALL XVP('LIST',lfname,NJ)                !LIST used or
         CALL XVPCNT('INP',NI)                     !  input frames
 
         IF (NI .NE. 0) THEN                     !Open input files
            OUT(1) = FLOAT(NI)
            DO I=1,NI
               CALL XVUNIT(IUNI(I),'INP',I,STAT,' ')
               CALL XVOPEN(IUNI(I),STAT,'OPEN_ACT','SA','IO_ACT','SA',
     &                     'U_FORMAT','HALF',' ')
            ENDDO
         ELSE IF (NJ .NE. 0) THEN                !open input list
            open(unit=99,file=lfname,status='OLD',err=950)
            read(99,fmt=1,err=940) fn                     !skip first line
    1       format(a)
 
            DO i=1,51                             !open files in list
               read(99,fmt=1,end=11) fn
               if (i .ge. 51) goto 920
               NI = i
               OUT(1) = NI
               call xvunit(iuni(i),'NONE',I,ist,'u_name',fn,' ')
               CALL XVOPEN(IUNI(I),IST,'OPEN_ACT','SA','IO_ACT','SA',
     &                     'U_FORMAT','HALF',' ')
            END DO
   11       continue
         ELSE
            CALL XVMESSAGE('Specify input files as INP or LIST',' ')
            CALL ABEND
         END IF
 
         CALL XVP('EXPO',EXPO,ICNT)
         if (ICNT .eq. 1) then
            GOTO 12
         ELSE
c-------Get exposure time from the label of one frame
            NINSTANCE = 30
            CALL XLHINFO(iuni(1),TASKS,INSTANCE,NINSTANCE,IST,' ')
C-------Try Cassini type
            call xlget(iuni(1),'PROPERTY','EXPOSURE_DURATION',EXPO,ist,
     1                 'FORMAT','REAL','PROPERTY','CASSINI-ISS',' ')
            if (ist .eq. 1) goto 12
C-------Try Galileo type
            call xlget(iuni(1),'HISTORY','EXP',EXPO,ist,
     1                 'FORMAT','REAL','HIST',TASKS(1),' ')
            if (ist .eq. 1) goto 12
         END IF
 
c-------No exposure in labels either
         GOTO 960
 
12       continue            !found exposure

c-------Open Light Transfer File for update
         CALL XVUNIT(OUNI,'OUT',1,STAT,' ')
         CALL XVOPEN(OUNI,STAT,'OPEN_ACT','SA','IO_ACT','SA',
     &               'OP','UPDATE',' ')
         CALL XVGET(OUNI,STAT,'NL',NLA,'NS',NSA,' ')   !Get size of LTFILE

C        Read in area locations from LTFILE
         CALL XLGET(OUNI,'HISTORY','NUM_AREAS',NAREA,STAT,
     &              'FORMAT','INT','HIST','LTGEN',' ')
         CALL XLGET(OUNI,'HISTORY','AREAS',AREA,STAT,'NELEMENT',4*NAREA,
     &              'FORMAT','INT','HIST','LTGEN',' ')
         NCH = MIN0(20*NI+32,132)
 
         CALL XVMESSAGE(' ',' ')
         CALL PRNT(7,1,EXPO, 'EXPOSURE TIME=  .')
         CALL PRNT(4,1,NI,   'INPUT FRAMES=   .')
         CALL PRNT(4,1,NAREA,'NUMBER OF AREAS=.')
 
C----------------------------------------------------
         IF (DBUG) THEN    !Print column headings for mean/sigma table
            MSG = ' '
            MSG='AREA  STARTING    SIZE OF    MEAN'
            MSG(30+10*NI:28+10*NI+7)='STANDARD'
            CALL XVMESSAGE(MSG,' ')
            MSG=' '
            MSG='NO.  COORDINATES   AREA      VALUES...'
            MSG(30+10*NI:28+10*NI+12)='DEVIATIONS...'
            CALL XVMESSAGE(MSG,' ')
            MSG=' '
            MSG='     (LINE,SAMP)  (NL,NS)'
            ICNT=33
            DO I=1,NI
               MSG(ICNT:ICNT)='M'
               MSG(ICNT+10*NI:ICNT+10*NI)='S'
               WRITE(MSG(ICNT+1:ICNT+1),9000)I
               WRITE(MSG(ICNT+1+10*NI:ICNT+1+10*NI),9000)I
 9000          FORMAT(I1)
               ICNT = ICNT+10
            ENDDO
 
            CALL XVMESSAGE(MSG,' ')
         ENDIF
 
         IB = 2 
 
C-----Main loop:  Read areas from inputs
         DO 100 K=1,NAREA              !Loop through each area
            SL = AREA(1,K)              !Load area size field
            SS = AREA(2,K)
            NL = AREA(3,K)
            NS = AREA(4,K)
            CALL ZIA(SUM,2*NI)
            CALL ZIA(SUM2,2*NI)
            CALL ZIA(SUMXY,2*NI)
 
            DO L=1,NL
               LL = SL+L-1
               DO I=1,NI     !Read a line from each input frame
                  CALL XVREAD(IUNI(I),E(1,I),STAT,'LINE',LL,
     &                        'SAMP',SS,'NSAMPS',NS,' ')
               ENDDO
               DO I=1,NI
                  II = MOD(I,NI)+1
                  CALL MOMGEN(E(1,I),E(1,II),SUM(I),SUM2(I), 
     &                        SUMXY(I),NS)
               ENDDO
            ENDDO
 
            IF (DBUG) THEN          !Print mean and sigma of area
               MSG=' '
               WRITE(MSG,9001)K,SL,SS,NL,NS
 9001          FORMAT(I3,'  (',I4,',',I4,') (',I4,',',I4,')')
               N = NL*NS
               IJ = 38 
 
               DO I=1,NI
                  II = MOD(I,NI) + 1
                  R = SUM(I)/N
                  SIG = ((SUM2(I)+SUM2(II)-2*SUMXY(I))/N
     &                   -((SUM(I)-SUM(II))/N)**2)/2.D0
                  S = DSQRT(SIG)
                  WRITE(MSG(IJ-8:IJ),'(F8.2)') R
                  WRITE(MSG(IJ+10*NI-8:IJ+10*NI),'(F8.4)') S
                  IJ = IJ + 10
               ENDDO
 
               CALL XVMESSAGE(MSG,' ')
            ENDIF
 
C-----Load moments into output record...
            CALL MVE(-9,NI,SUM,OUT(IB),1,1)
            CALL MVE(-9,NI,SUM2,OUT(IB+NI),1,1)
            CALL MVE(-9,NI,SUMXY,OUT(IB+2*NI),1,1)
  100    IB = IB+3*NI
 
C-----Search LTFILE for exposure record....
c-----and fill with moments and quit
         CALL XLGET(OUNI,'HISTORY','NUM_EXPOS',NEXP,STAT,
     &              'FORMAT','INT','HIST','LTGEN',' ')
         CALL XLGET(OUNI,'HISTORY','EXPOSURES',EXP(1),STAT,
     &             'NELEMENT',NEXP,'FORMAT','REAL',
     &             'HIST','LTGEN',' ')
         DO L=1,NEXP
            IF (EXP(L).EQ.EXPO) THEN
               CALL XVWRIT(OUNI,OUT,STAT,'LINE',L,'NSAMPS',NSA,' ')
               RETURN
            ENDIF
         ENDDO
 
c-----continues to here if correct exposure record not found
  920    call xvmessage('more than 50 filenames in LIST',' ')
         goto 999
  940    call xvmessage('***Invalid SRCH-format input',' ')
         call xvmessage(lfname,' ')
         goto 999
  950    call xvmessage('could not open input list file',' ')
         call xvmessage(lfname,' ')
         goto 999
  960    call xvmessage('EXPO not specified or in labels',' ')
         goto 999
  999    CALL XVMESSAGE('***MOMGEN task cancelled',' ')
         CALL ABEND
      END


      SUBROUTINE MOMGEN(X,Y,S,S2,SXY,NS)
         IMPLICIT NONE 
         REAL*8 DN, S, S2, SXY 
         INTEGER*2 X(1),Y(1)
         INTEGER J, NS
 
         DO J=1,NS
            DN = X(J)
            S = S + DN
            S2 = S2 + DN**2
            SXY = SXY + DN*Y(J)
         ENDDO
 
         RETURN
      END

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create momgen.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM momgen

   To Create the build file give the command:

		$ vimake momgen			(VMS)
   or
		% vimake momgen			(Unix)


************************************************************************/


#define PROGRAM	momgen
#define R2LIB

#define MODULE_LIST momgen.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create momgen.pdf
PROCESS HELP=*
PARM INP  TYPE=STRING    COUNT=(0:50)   DEFAULT=--
PARM OUT  TYPE =STRING
PARM EXPO TYPE=REAL      COUNT=0:1      DEFAULT=--
PARM LIST TYPE=STRING    COUNT=0:1      DEFAULT=--
PARM DBUG TYPE=KEYWORD   COUNT=(0:1)    VALID=DBUG       DEFAULT=--
END-PROC
.TITLE
VICAR Application Program MOMGEN
.HELP
PURPOSE:
 
MOMGEN is the second of a sequence of VICAR applications programs which
measure the radiometric properties of a camera system.  The programs
are designed to support radiometric calibration of flight camera systems.
 
MOMGEN extracts statistical information from specified areas in the
input image(s) and stores the data in a record in the Light Transfer
File (LTF) corresponding to the specified exposure.
 
EXECUTION:
                MOMGEN INP=(E1,E2,E3,...EN) OUT=LTF PARMS
                or
                MOMGEN LIST=file OUT=LTF PARMS
 
.PAGE
OPERATION:
 
Input data to MOMGEN consists of one or more flat field frames taken
at the same exposure from a light transfer or reciprocity sequence.
These images may be input using either the INP or LIST parameters.
 
MOMGEN extracts statistical information from specified areas in the
input image(s) and stores the data in a record in the Light Transfer
File (LTF) corresponding to the specified exposure.  The LTF must have
been previously initialized by LTGEN, the first program in the sequence.
 
Multiple executions of MOMGEN are performed to extract and store data
from each exposure level into the LTF.  Note that if one of these
MOMGEN executions fails due to a tape read error or a bad frame, that
execution may be rerun without having to repeat all other executions.
Each exposure record in the LTF will contain data from the last MOMGEN
execution at that exposure.

After all exposure records in the LTF have been completed by MOMGEN,
data analysis routines may be executed to access the LTF and determine
camera properties such as:
 
        1) System gain constant and read noise floor (see CCDNOISE).
        2) Light transfer curve slope and offset (see CCDSLOPE).
        3) Shutter offset and camera sensitivity (see CCDRECIP).
 
.PAGE
LIGHT TRANSFER FILE FORMAT:
 
The Light Transfer File (LTF) is in VICAR format, and consists of VICAR
labels containing num_areas, num_expos and an array of the exposures,
followed by one record for each exposure level of the light transfer or
reciprocity sequence.

The label array areas contains the size fields for each area
specified (see LTGEN):

        where   AREA(1,K)=starting line for area K
                AREA(2,K)=starting sample
                AREA(3,K)=number of lines
                AREA(4,K)=number of samples
 
The exposure time (msec)is in the label of the LTF file.
Each exposure record contains the number of input frames at that
exposure (NI), and the moments for each area specified:

      NI,OUT(3*NI*NAREA)

where the array OUT consists of moment information in the following order:
 
         SUM(1),SUM(2),...,SUM(NI)          sum of DNs for area 1
         SUM(1),SUM(2),...,SUM(NI)          sum of DNs for area 2
            .    .          .
            .    .          .
         SUM2(1),SUM2(2),...,SUM2(NI)       sum of squares for area 1
         SUM2(1),SUM2(2),...,SUM2(NI)       sum of squares for area 2
            .    .          .
            .    .          .
         SUMXY(1),SUMXY(2),...,SUMXY(NI)    sum of cross terms for area 1
         SUMXY(1),SUMXY(2),...,SUMXY(NI)    sum of cross terms for area 2
            .    .          .
            .    .          .
The exposure records are arranged in order of increasing exposure.
 
Galileo specific:
If an extended exposure dark current record is present, it occurs
first in the LTF (with EXPO=-1.0), followed by the normal DC frame
(EXPO=0.0), followed by the lowest to highest exposure levels.
 
.PAGE
ORIGINAL PROGRAMMER: Gary Yagi, circa 1983
COGNIZANT PROGRAMMER: Gary Yagi
REVISION HISTORY:

   19 MAR 97....T.Huang........Ported from VAX to UNIX to support
                               both Cassini and Galileo.
   23 NOV 93....C.C.Avis.......Added LIST and label search for EXPO
                               Added error checking, max inputs to 50,
                               max size 1024.
   26 APR 88....G.M.Yagi.......Added more documentation: FR 35678.
    4 JUL 86....G.M.Yagi.......Code and documentation clean-up
   19 FEB 85....M.E.MORRILL....INCLUDED EXTENDED EXPOSURE
                                 MODE DEFINITION
   14 JAN 85....M.E.MORRILL....EXPANDED TO 400 AREAS
    1 OCT 84....M.E.MORRILL....VAX-VICAR*2 Conversion
          82....G.M.YAGI.......INITIAL RELEASE
 
.LEVEL1
.VARIABLE INP
 STRING
 Input flat field frames
 at same exposure level.
.VARIABLE OUT
 STRING
 Output Light Transfer File.
.VARIABLE EXPO
 REAL
 Exposure values for
 input frames (must be
 entered exactly as in
 LTGEN run).
.VARIABLE LIST
 STRING
 Name of SRCH-format
 file containing the
 filenames to process
.VARIABLE DBUG
 KEYWORD
 Enables diagnostic
 printout for debugging.
.LEVEL2
.VARIABLE INP
 STRING-(Required if LIST not used)
 One to 50 input flat field frames at same exposure level.
.VARIABLE OUT
 STRING-Required
 Output Light Transfer File.  The file must be initialized via LTGEN.  Repeated
 MOMGEN runs are then used to store data in the file for each exposure level.
.VARIABLE EXPO
 REAL-Optional
 Exposure value for input frames.  The exposure value is normally extracted
 from Cassini or Galileo image labels.  This parameter can be used to
 override the label value.  The value should be entered exactly as in
 the LTGEN run (e.g. if EXPO=33.3333, make sure you use the same number of
 digits in both places).  For Galileo, use EXPO=-1.0 for extended mode DC.
.VARIABLE LIST
 STRING-Optional
 The name of a SRCH-format file containing the names of the images to
 process (see program SRCH).
.VARIABLE DBUG
 KEYWORD--Optional
 Enables diagnostic printout for debugging.  The printout gives an indication
 of input data quality and may bring to light problems in the input sequence.
.END

$ Return
$!#############################################################################
$Test_File:
$ create tstmomgen.pdf
procedure
refgbl $echo
refgbl $autousage
refgbl $syschar

body
local dir string
let $autousage="none"
let _onfail="continue"
let $echo="yes"
 
!GENERAL TEST:
gen e1.img 200 200 sinc=0
gen e2.img 200 200 sinc=0 ival=2
gen e3.img 200 200 sinc=0 ival=4
ltgen e1.img ltf.out gres=(2,10,10) expo=(-1.0,0.,50.,66.67,100.) ni=3

!verify exposure and area in VICAR label.
label-list ltf.out

!This should return all zeros, because they are place holders for momgen
list ltf.out
 
momgen (e1.img,e2.img,e3.img) ltf.out 'DBUG expo=-1.
momgen (e1.img,e2.img,e3.img) ltf.out 'DBUG expo=0.
momgen (e1.img,e2.img,e3.img) ltf.out 'DBUG expo=50.
label-list ltf.out
list ltf.out (1,1,4,2) 
 
!CASSINI TEST:
if ($syschar(1)="UNIX")
   let dir = "/project/test_work/testdata/cassini/iss/"
   defcmd-replace typeit "ush cat"
else
   let dir = "wms_test_work:[testdata.cassini.iss]"
   defcmd-replace typeit "dcl type"
end-if
 
!---------------------------
! Make a test light transfer file which has exposure levels of
! 0,10,20,40 and each input frame was 10,110,210,410 dn respectively.
! Each level has 3 frames associated with it.
 
!Set dns to 10 and replicate - set exposure to 0
f2 &"dir"sum2.1 l1.a func=10
label-rep l1.a 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=0"
copy l1.a l1.b
copy l1.a l1.c
 
!Set dns to 110 and replicate - set exposure to 10
f2 &"dir"sum2.1 l2.a func=110
label-rep l2.a 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=10."
copy l2.a l2.b
copy l2.a l2.c
 
!Set dns to 210 and replicate - set exposure to 20
f2 &"dir"sum2.1 l3.a func=210
label-rep l3.a 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=20."
copy l3.a l3.b
copy l3.a l3.c
 
!Set dns to 410 and replicate - set exposure to 40
f2 &"dir"sum2.1 l4.a func=410
label-rep l4.a 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=40."
copy l4.a l4.b
copy l4.a l4.c
 
!Create list of the files created in SRCH format
createfile m.list
addtofile m.list "NEXT FILE=0001"
addtofile m.list "l1.a"
addtofile m.list "l1.b"
addtofile m.list "l1.c"
addtofile m.list "l2.a"
addtofile m.list "l2.b"
addtofile m.list "l2.c"
addtofile m.list "l3.a"
addtofile m.list "l3.b"
addtofile m.list "l3.c"
addtofile m.list "l4.a"
addtofile m.list "l4.b"
addtofile m.list "l4.c"
reset m.list
typeit m.list
 
!Initialize Light Transfer File
ltgen l1.a out=testltf.out list=m.list 'GRID

!Verify area and exposure on VICAR label
label-list testltf.out 
 
!Create list of the files for one exposure level in SRCH format
createfile m1.list
addtofile m1.list "NEXT FILE=0001"
addtofile m1.list "l1.a"
addtofile m1.list "l1.b"
addtofile m1.list "l1.c"
 
!Fill Light Transfer File with stats
momgen list=m1.list out=testltf.out 'DBUG
 
!check what MOMGEN did to the output file
!the record with exposure time = 0.0 should be filled
!word 1=#images (i*4), rest is r*4 stats
label-list testltf.out 
list testltf.out (1,1,4,2) 
 
!Repeat with override of exposure gotten from labels
!(it must be an exposure value that LTGEN knew about)
momgen list=m1.list out=testltf.out exp=10.
 
!check what MOMGEN did to the output file
!the records with exposure time = 0.0 & 10. should be filled
!word 1=#images (i*4), rest is r*4 stats
label-list testltf.out 
list testltf.out (1,1,4,2) 

if ($syschar(1)="UNIX")
   ush rm e1.*
   ush rm e2.*
   ush rm e3.*
   ush rm l1.*
   ush rm l2.*
   ush rm l3.*
   ush rm l4.*
   ush rm m.list
   ush rm m1.list
   ush rm *.out
else
   dcl del e1.*;*
   dcl del e2.*;*
   dcl del e3.*;*
   dcl del l1.*;*
   dcl del l2.*;*
   dcl del l3.*;*
   dcl del l4.*;*
   dcl del m.list;*
   dcl del m1.list;*
   dcl del *.out;*
end-if
end-proc

$ Return
$!#############################################################################
