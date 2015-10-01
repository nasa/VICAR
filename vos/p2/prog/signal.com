$!****************************************************************************
$!
$! Build proc for MIPL module signal
$! VPACK Version 1.9, Monday, December 07, 2009, 17:02:31
$!
$! Execute by entering:		$ @signal
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
$ write sys$output "*** module signal ***"
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
$ write sys$output "Invalid argument given to signal.com file -- ", primary
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
$   if F$SEARCH("signal.imake") .nes. ""
$   then
$      vimake signal
$      purge signal.bld
$   else
$      if F$SEARCH("signal.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake signal
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @signal.bld "STD"
$   else
$      @signal.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create signal.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack signal.com -mixed -
	-s signal.f -
	-i signal.imake -
	-p signal.pdf -
	-t tstsignal.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create signal.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
C
C Program signal
C
C 28 JUL 1997 ...RRD...      Ported to UNIX.
C 17 NOV 1995 ...JRY...      Written
C
      SUBROUTINE MAIN44
      INTEGER*2 DN
      INTEGER*4 CNT, DEF, ICNT, IND, ISTAT, NI, NL, NS
      INTEGER*4 EXPOS(50), INSTANCES(30), IUNIT(50), LS(2), PICSCALE(50)
      REAL*4    SIGNAL, REXPOS
      CHARACTER*255 LFNAME, TBL
      CHARACTER*8  TASKS(30)
      CHARACTER*1 TAB

      TAB = CHAR(9)
      CALL IFMESSAGE('signal version 28-Jul-97')
      CALL XVPCNT('INP',NI)	
      CALL XVP('LIST',LFNAME,ICNT)
      CALL XVPARM('TBL',TBL,CNT,DEF,0)
      CALL XVPARM('LS',LS,CNT,DEF,0)

      IF (NI .NE. 0) THEN
         DO I=1,NI
            CALL XVUNIT(IUNIT(I),'INP',I,STAT,' ')
            CALL XVOPEN(IUNIT(I),STAT,'OPEN_ACT','SA','IO_ACT','SA',
     &                  'U_FORMAT','HALF',' ')
         ENDDO
      ELSE IF (ICNT .NE. 0) THEN    !input frames are in SRCH-list
     	 OPEN(UNIT=99,FILE=LFNAME,STATUS='OLD',ERR=999)
	 READ(99,FMT=1) LFNAME                      !SKIP FIRST LINE
1        FORMAT(A)
 	 DO I=1,51
	    READ(99,FMT=1,END=11,ERR=999) LFNAME
	    IF (I .GE. 51) GO TO 990
            NI = I
	    CALL XVUNIT(IUNIT(I),'NONE',I,ISTAT,'U_NAME',LFNAME,' ')
	    CALL XVOPEN(IUNIT(I),ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     &                  'U_FORMAT','HALF',' ')
         ENDDO
11       CONTINUE
      ELSE
         CALL XVMESSAGE('Specify input files as INP or LIST',' ')
         CALL ABEND
      END IF

C     Get PICSCALE from file label (number of frames PICSUM'd for each input)

      CALL XVGET(IUNIT(1),IND,'NL',NL,'NS',NS,' ')
      IF (LS(1) .GT. NL .OR. LS(2) .GT. NS) THEN
         CALL XVMESSAGE('LS EXCEEDS SIZE OF IMAGE', ' ')
         CALL ABEND         
      END IF
      DO 5 I=1,NI
         CNT = 30
         CALL XLGET(IUNIT(I),'PROPERTY','EXPOSURE_DURATION',REXPOS,
     &              ISTAT,'PROPERTY','CASSINI-ISS','FORMAT','REAL',' ')
         IF (ISTAT .NE. 1) THEN
            CALL MABEND('ERROR READING LABEL EXPOSURE_DURATION')
         END IF
         EXPOS(I) = IFIX(REXPOS)
         CALL XLHINFO(IUNIT(I),TASKS,INSTANCES,CNT,IND,' ')
         DO J=CNT,1,-1		!Search for last value of picture scale
            CALL XLGET(IUNIT(I),'HISTORY','PICSCALE',PICSCALE(I),IND,
     &            'HIST',TASKS(J),'INSTANCE',INSTANCES(J),
     &            'FORMAT','INT',' ')
            IF (IND.EQ.1) GOTO 5
         ENDDO

C Didn't find a PICSCALE
         PICSCALE(I) = 1		!Default picture scale
5     CONTINUE

C Open output table
      OPEN(15,FILE=TBL,STATUS='UNKNOWN',IOSTAT=JST,ERR=992)
      WRITE(15,100) ' EXPOSURE',TAB,' SIGNAL'
100   FORMAT(A9,A1,A7)

      DO I=1,NI 	!Read data from each flat-field frame...
         CALL XVREAD(IUNIT(I),DN,IND,'LINE',LS(1),'SAMP',LS(2),
     &               'NSAMPS',1,' ')
         SIGNAL = FLOAT(DN)/PICSCALE(I)
         WRITE(15,110) EXPOS(I),TAB,SIGNAL
      ENDDO
110   FORMAT(I10,A1,F8.2)

C Close inputs and output
      DO I=1,NI
        CALL XVCLOSE(IUNIT(I),IND,' ')
      ENDDO
      CLOSE(15)

      CALL XVMESSAGE('signal task completed',' ')
      RETURN

  992 CALL XVMESSAGE('ERROR WRITING OUTPUT TABLE', ' ')
      CALL ABEND
  990 CALL XVMESSAGE('MORE THAN 50 FILENAMES IN LIST', ' ')
      CALL ABEND
  999 CALL XVMESSAGE('ERROR OPENING INPUT SRCH LIST FILE', ' ')
      CALL ABEND
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create signal.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM signal

   To Create the build file give the command:

		$ vimake signal			(VMS)
   or
		% vimake signal			(Unix)


************************************************************************/


#define PROGRAM	signal
#define R2LIB

#define MODULE_LIST signal.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create signal.pdf
PROCESS HELP=*
  PARM INP   STRING   COUNT=0:50   DEFAULT=--
  PARM TBL   STRING   COUNT=1
  PARM LIST  STRING   COUNT=0:1    DEFAULT=--
  PARM LS    INTEGER  COUNT=2                   VALID=(1:1024)
END-PROC
.TITLE
VICAR Program signal
.HELP
PURPOSE:
   Signal is a VICAR applications program which outputs the signal of a specific
pixel from a set of flat-field image.  The exposure and signal is output into
an ASCII text file.  If the input images are PICSUM'd, then the value of the
pixel specified by LS is divided by the PISCALE value that is in the VICAR
label.  If there is no PICSCALE value, then the input is assumed not to be
PICSUM'd.

This program was written for the Cassini ISS camera and expects that the
flat-field images have a valid Cassini label.

.page
EXECUTION STATEMENT:

  SIGNAL INP=(D1,D2,D3,...,Dn) TBL=SIGNAL.TBL LS=(100,201)

     or

  SIGNAL LIST=FILENAME.LIST TBL=SIGNAL.TBL LS=(100,201)

The flat-field filenames may be input to the program in the form of a
SRCH-format text file (see program NXT).  The output TBL produces a
tab-delimited ASCII text file containing EXPOSURE and SIGNAL.

.page
RESTRICTIONS:

1.  A maximum of 50 exposure levels are allowed.

.page
PROGRAM HISTORY:

Written by Jan Yoshimizu, 17 Nov 95
Current cognizant programmer:  Jan Yoshimizu
Revisions:
   28 JUL 97  RRD  Made portable for UNIX
		   Made computation of signal float division
   17 NOV 95  JRY  Initial release

.LEVEL1
.VARIABLE INP
 STRING-OPTIONAL
 Flat-field & DC images.
.VARIABLE TBL
 STRING--REQUIRED
 ASCII text output file
.VARIABLE LIST
 STRING--OPTIONAL
 SRCH-format file containing
 the names of the files
 to be processed.
.VARIABLE LS
 STRING--REQUIRED
Line and sample for which
you want the signal
.LEVEL2
.VARIABLE INP
 STRING--OPTIONAL
	INP=(D1,D2,D3,...,Dn)
 Inputs are flat-field images which may have been PICSUM'd.
.VARIABLE TBL
 STRING--REQUIRED
 The output ASCII text file which contains the exposure and signal of the
 specified pixel.
.VARIABLE LIST
 STRING--OPTIONAL
 Specifies the name of the SRCH-format text file containing the names of
 the input files to be processed.  This is an alternative to listing the 
 filenames with the INP parameter.  
.VARIABLE LS
 STRING--REQUIRED
 The line and sample of the pixel whose exposure and signal is output.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstsignal.pdf
procedure
refgbl $echo
refgbl $autousage
refgbl $syschar
local DIR    type=string
local (IN1 IN2 IN3 IN4 IN5 IN6 IN7 IN8 IN9 IN10 IN11)	type=string
body
let $echo="no"
let $autousage="none"
let _onfail="continue"
if ($syschar(1) = "UNIX")
   let DIR   ="/project/test_work/testdata/cassini/iss/"
else
   let DIR   ="WMS_TEST_WORK:[TESTDATA.CASSINI.ISS]"
end-if
let IN1 = "&DIR"//"pcsm15.1"
let IN2 = "&DIR"//"pcsm15.2"
let IN3 = "&DIR"//"pcsm15.3"
let IN4 = "&DIR"//"pcsm15.4"
let IN5 = "&DIR"//"pcsm15.5"
let IN6 = "&DIR"//"pcsm15.6"
let IN7 = "&DIR"//"pcsm15.7"
let IN8 = "&DIR"//"pcsm15.8"
let IN9 = "&DIR"//"pcsm15.9"
let IN10 = "&DIR"//"pcsm15.10"
let IN11 = "&DIR"//"pcsm15.11"
let $echo=("yes","no","no")	!echo top level only

signal inp=(&IN1 &IN2 &IN3 &IN4 &IN5 &IN6 &IN7 &IN8 &IN9 &IN10 &IN11) +
       tbl=pcsm15.tbl1 ls=(100,100)

typetext pcsm15.tbl1

createfile tmp.srchlist
addtofile tmp.srchlist "NEXT FILE=00001"
addtofile tmp.srchlist "&IN1"
addtofile tmp.srchlist "&IN2"
addtofile tmp.srchlist "&IN3"
addtofile tmp.srchlist "&IN4"

signal list=tmp.srchlist tbl=pcsm15.tbl2 ls=(100,100)

typetext pcsm15.tbl2

label-delete &IN1 t1 key="PICSCALE"
label-delete &IN2 t2 key="PICSCALE"
label-delete &IN3 t3 key="PICSCALE"
label-delete &IN4 t4 key="PICSCALE"

signal inp=(t1 t2 t3 t4) tbl=pcsm15.tbl3 ls=(100,100)

typetext pcsm15.tbl3
list t1 (100,100,1,1)
list t2 (100,100,1,1)
list t3 (100,100,1,1)
list t4 (100,100,1,1)

end-proc
$ Return
$!#############################################################################
