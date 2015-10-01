$!****************************************************************************
$!
$! Build proc for MIPL module insert
$! VPACK Version 1.7, Sunday, July 03, 1994, 20:28:39
$!
$! Execute by entering:		$ @insert
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
$ write sys$output "*** module insert ***"
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
$ write sys$output "Invalid argument given to insert.com file -- ", primary
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
$   if F$SEARCH("insert.imake") .nes. ""
$   then
$      vimake insert
$      purge insert.bld
$   else
$      if F$SEARCH("insert.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake insert
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @insert.bld "STD"
$   else
$      @insert.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create insert.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack insert.com -
	-s insert.f -
	-i insert.imake -
	-p insert.pdf -
	-t tstinsert.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create insert.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C    02 MAY 1994 ...CRI... MSTP S/W CONVERSION (VICAR PORTING)
C   INSERT  -  SOURCE, HELP FILE AND PDF
C
C        'INSERT'   LINE INSERTION PROGRAM
C
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      IMPLICIT INTEGER (A-Z)
      LOGICAL FILL,AVG,TRUNC,FIX,NOPRNT
      LOGICAL XVPTST
      INTEGER LINEAR(100),MAXCNT
      COMMON /C1/ FILL,AVG,TRUNC,FIX,NOPRNT,TNL,NL,NS,SL,SS,NPXL,NB,
     & NSI,IDN,WUNIT,RUNIT,LINEAR
      EXTERNAL WORK
      DATA MAXCNT /100/
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C        PARAMETERS
C     'LINE',I,J,K..   I,J,K ARE LINES TO BE INSERTED
C     'DN',N         INSERTED LINES TO HAVE DN OF N
C     'AVG'          INSERTED LINE TO BE AVG OF NEIGHBORS
C     'TRUNC'        OUTPUT TRUNCATED TO NL LINES
C     'NOFIX'       SUPPRESSES FIXING OF DROPPED LINES
C     'NOPRINT'     SUPPRESSES PRINTOUT
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C        DEFAULT PARAMETERS
      FILL= .FALSE.
      AVG= .TRUE.
      TRUNC= .FALSE.
      FIX= .TRUE.
      NOPRNT= .FALSE.
C
C----    GET PARAMETERS
C
      CALL IFMESSAGE ('INSERT version 02-MAY-94')
      CALL XVEACTION ('SA',' ')
      CALL XVPARM('LINE',LINEAR,NLINE,LINEDF,MAXCNT)
      IF (LINEDF.EQ.1) NLINE=0
      IF (LINEDF.EQ.1 .OR. NLINE.EQ.1)  GO TO 20
C  SORT LINES BY INCREASING LINE NUMBER:
      DO I = 1,NLINE-1
	DO J = I+1,NLINE
	  IF (LINEAR(I).EQ.LINEAR(J)) THEN
	    CALL XVMESSAGE('"LINE" MAY NOT CONTAIN REPEATED NUMBERS',' ')
	    CALL ABEND
	  ELSEIF (LINEAR(I).GT.LINEAR(J)) THEN
	    SAVE = LINEAR(I)
	    LINEAR(I) = LINEAR(J)
	    LINEAR(J) = SAVE
	  ENDIF
	ENDDO
      ENDDO
	  
20    AVG = .NOT.XVPTST('NOAVG')

      CALL XVPARM('DN',IDN,NDN,DNDF,100)
      IF (DNDF.EQ.0)  AVG = .FALSE.

      TRUNC = XVPTST('TRUNC')

      FIX = .NOT.XVPTST('NOFIX')

      NOPRNT= XVPTST('NOPRINT')

C----    OPEN DATA SETS
C
      CALL XVUNIT(RUNIT,'INP',1,STATUS,' ')
      CALL XVOPEN(RUNIT,STATUS,' ')
      CALL XVGET(RUNIT,STATUS,'PIX_SIZE',NPXL,'NS',NSI,' ')
      CALL XVSIZE(SL,SS,NL,NS,NLI,NSI)
      NB = NPXL*NS
      TNL= NL+NLINE
      IF (TRUNC)  TNL= NL

      CALL XVUNIT(WUNIT,'OUT',1,STATUS,' ')
      CALL XVOPEN(WUNIT,STATUS,'OP','WRITE','U_NL',TNL,'U_NS',NS,' ')
      IF (TNL.NE.NL) THEN
	CALL PRNT(4,1,TNL,'OUTPUT NL=.')
	CALL PRNT(4,1,NS,'OUTPUT NS=.')
      ENDIF

      CALL STACKA(4, WORK, 2, NB, NB)

      CALL XVCLOSE(RUNIT,STATUS,' ')
      CALL XVCLOSE(WUNIT,STATUS,' ')
      RETURN
      END

C*********************************************************

      SUBROUTINE WORK( BUF, L1, FILBUF, L2)
      IMPLICIT INTEGER*4 (A-Z)
      BYTE BUF(L1), FILBUF(L2)
      LOGICAL FILL,AVG,EOF,TRUNC,FIX,NOPRNT
      INTEGER LINEAR(100),TMP_BUF1(12),TMP_BUF2(12),IB,IA
      COMMON /C1/ FILL,AVG,TRUNC,FIX,NOPRNT,TNL,NL,NS,SL,SS,NPXL,NB,
     & NSI,IDN,WUNIT,RUNIT,LINEAR

      EOF = .FALSE.
C  LREAD IS THE NUMBER OF THE CURRENT INPUT LINE WRT SL
C  LWRIT IS THE NUMBER OF THE CORRESPONDING OUTPUT LINE
C  LCNT  IS THE NEXT NUMBER IN THE INSERTED LINE LIST
      LCNT= 1
      LREAD = 0
      LWRIT = 1

      IF (NPXL.EQ.1) THEN
        CALL XVTRANS_SET(TMP_BUF1,'BYTE','FULL',STAT)
	IF (STAT.NE.1) THEN
          CALL XVMESSAGE('BUFFER SETUP UNSUCCESSFUL',' ')
          CALL ABEND
        ENDIF
        CALL XVTRANS_SET(TMP_BUF2,'FULL','BYTE',STAT)
	IF (STAT.NE.1) THEN
          CALL XVMESSAGE('BUFFER SETUP UNSUCCESSFUL',' ')
          CALL ABEND
        ENDIF
      ELSE
        CALL XVTRANS_SET(TMP_BUF1,'HALF','FULL',STAT)
	IF (STAT.NE.1) THEN
          CALL XVMESSAGE('BUFFER SETUP UNSUCCESSFUL',' ')
          CALL ABEND
        ENDIF
        CALL XVTRANS_SET(TMP_BUF2,'FULL','HALF',STAT)
	IF (STAT.NE.1) THEN
          CALL XVMESSAGE('BUFFER SETUP UNSUCCESSFUL',' ')
          CALL ABEND
        ENDIF
      ENDIF

      IF (AVG)  GO TO 500
      DN = IDN
C  LOAD FILL BUFFER WITH SPECIFIED DN
        IF (NPXL.EQ.1) THEN
          CALL MVE(-5,NS,DN,FILBUF,0,1)
        ELSE
          CALL MVE(-6,NS,DN,FILBUF,0,1)
        ENDIF

C  MAIN LOOP:

500   IF (LREAD.LT.NL)  GO TO 510
      IF (LINEAR(LCNT).EQ.LWRIT)  GO TO 520
      RETURN
510   IF (LREAD.EQ.0 .AND. SL.NE.1) THEN
	CALL XVREAD(RUNIT,BUF,STATUS,'LINE',SL,'SAMP',SS,
     .	 'NSAMPS',NS,' ')
      ELSEIF (SS.NE.1 .OR. NSI.NE.NS) THEN
	CALL XVREAD(RUNIT,BUF,STATUS,'SAMP',SS,'NSAMPS',NS,' ')
      ELSE
	CALL XVREAD(RUNIT,BUF,STATUS,' ')
      ENDIF
      LREAD= LREAD+1
      IF (STATUS.EQ.-30) GO TO 650
      IF (STATUS.NE.1.AND. FIX)  GO TO 515
      IF (LINEAR(LCNT).NE.LWRIT)  GO TO 700
      GO TO 520

C  LINE TO BE INSERTED
515   LREAD = LREAD+1
520   LCNT = LCNT+1
      IF (.NOT.AVG)  GO TO 600
      IF (LWRIT.EQ.1)  CALL MVE(1,NB,BUF,FILBUF,1,1)
      JJ = 0
      DO 570 J=1,NS
        IF (NPXL.EQ.1) THEN
	  JJ = J
        ELSE
  	  JJ = J * 2 - 1
        ENDIF
        CALL XVTRANS(TMP_BUF1,BUF(JJ),IA,1)
        CALL XVTRANS(TMP_BUF1,FILBUF(JJ),IB,1)
	IB = 0.5*(IA+IB)+0.5
        CALL XVTRANS(TMP_BUF2,IB,FILBUF(JJ),1)
570   CONTINUE
600   IF (.NOT.NOPRNT) CALL PRNT(4,1,LWRIT,'LINE INSERTED.')
      CALL XVWRIT(WUNIT,FILBUF,STATUS,' ')
      LWRIT= LWRIT+1
      IF (LINEAR(LCNT).EQ.LWRIT)  GO TO 520
      IF (EOF) RETURN
      GO TO 700
C
C        FILL-IN BOTTOM OF PICTURE
650   EOF= .TRUE.
      IF (LINEAR(LCNT).NE.LWRIT)  GO TO 655
      CALL MVE(1,NB,FILBUF,BUF,1,1)
      GO TO 520
655   FILL= .TRUE.
      CALL MVE(1,NB,FILBUF,BUF,1,1)
675   IF (LWRIT.GT.TNL)  RETURN
      IF (.NOT.NOPRNT) CALL PRNT(4,1,LWRIT,'LINE INSERTED.')

C---- WRITE THE LINE

700   CONTINUE
      CALL XVWRIT(WUNIT,BUF,STATUS,' ')
      LWRIT = LWRIT+1
      IF (FILL)  GO TO 675
      IF (AVG)  CALL MVE(1,NB,BUF,FILBUF,1,1)
      GO TO 500

      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create insert.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM insert

   To Create the build file give the command:

		$ vimake insert			(VMS)
   or
		% vimake insert			(Unix)


************************************************************************/


#define PROGRAM	insert
#define R2LIB

#define MODULE_LIST insert.f

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
$ create insert.pdf
process help=*
PARM INP   TYPE=STRING
PARM OUT   TYPE=STRING
PARM SIZE  TYPE=INTEGER COUNT=4                DEFAULT=(1,1,0,0)
PARM SL    TYPE=INTEGER                        DEFAULT=1
PARM SS    TYPE=INTEGER                        DEFAULT=1
PARM NL    TYPE=INTEGER                        DEFAULT=0
PARM NS    TYPE=INTEGER                        DEFAULT=0
PARM LINE  TYPE=INTEGER COUNT=1:100            DEFAULT=0
PARM DN    TYPE=INTEGER                        DEFAULT=0
PARM NOAVG   KEYWORD COUNT=0:1 VALID=NOAVG DEFAULT=--
PARM TRUNC TYPE=KEYWORD VALID=TRUNC COUNT=0:1 DEFAULT=--
PARM NOFIX KEYWORD VALID=NOFIX COUNT=0:1 DEFAULT=--
PARM NOPRINT KEYWORD VALID=NOPRINT COUNT=0:1 DEFAULT=--
END-PROC

.TITLE
 "insert"
.HELP

 PURPOSE:
 INSERT IS AN APPLICATIONS PROGRAM THAT WILL INSERT
 ARTIFICIAL LINES IN A PICTURE AND RESCALE THE
 REMAINING LINES DOWN THE APPROPRIATE AMOUNT. INSERT
 IS USEFUL FOR CORRECTING MISREGISTRATION DUE TO
 LINE DROPOUT.

 EXECUTION:
 THE FOLLOWING IS THE EXECUTION STATEMENT FOR INSERT:
        insert  INP  OUT  PARAMS
.PAGE
 EXAMPLES:
 insert INP=A OUT=B SIZE=(1,1,100,200) LINE=(1,10)

 LINES 1 AND 10 OF THE OUTPUT DATA SET B ARE ARTIFICIALLY
 GENERATED BY AVERAGING PIXELS IN ADJOINING LINES.  LINES
 2 THROUGH 9 ARE LINES 1 THROUGH 8 OF THE INPUT DATA SET.
 LINES 11 THROUGH 102 ARE LINES 9 THROUGH 100 OF THE INPUT
 DATA SET.

 insert INP=A OUT=B SIZE=(1,1,100,200) DN=128 LINE=5 TRUNC=TRUNC

 LINE 5 OF THE OUTPUT DATA SET, B, WILL BE ARTIFICIAL WITH DN
 OF 128.  LINES 1 THROUGH 4 ARE LINES 1 THROUGH 4 OF THE
 INPUT DATA SET, A.  LINES 6 THROUGH 100 ARE LINES 5 THROUGH 99 OF
 THE INPUT DATA SET, A.
.PAGE
 RESTRICTIONS:

 MAX. NUMBER OF LINES THAT CAN BE INSERTED = 100.

 IF NO LINES ARE SPECIFIED FOR INSERTION THE INPUT
 DATA SET IS COPIED TO THE OUTPUT DATASET.  IF AN
 UNEXPECTED EOF OCCURS, ARTIFICIAL LINES ARE
 ADDED TO MAKE UP THE DIFFERENCE.

 TIMING: NONE AVAILABLE FOR THE VAX
.PAGE
 ORIGINAL PROGRAMMER:  J. D. ADDINGTON,  19 JUNE 1974
 CONVERTED TO VAX:     J. A. MOSHER,  15 MARCH 1983
 CONVERTED TO VICAR2:  S. POHORSKY,  19 NOV. 1984
 REVISION 1:           L. W. KAMP,  20 JAN. 1985
 Made portable for UNIX Alan Scop (CRI),  2 MAY 1994    
CURRENT COGNIZANT PROGRAMMER:  L. W. KAMP
.LEVEL1
.VARIABLE INP
 INPUT DATASET
.VARIABLE OUT
 OUTPUT DATASET
.VARIABLE SIZE
 IMAGE SIZE
.VARI SS
 INTEGER - STARTING SAMPLE
.VARI SL
 INTEGER - STARTING LINE
.VARI NL
 INTEGER - NUMBER OF LINES
.VARI NS
 INTEGER - NUMBER OF SAMPLES
.VARIABLE LINE
 1-100 ARTIFICIAL-LINE NUMBERS
.VARIABLE DN
 DN OF INSERTED LINE PIXELS
.VARIABLE NOAVG
 SUPPRESS AVERAGING OF INSERTED PIXELS?
.VARIABLE TRUNC
 OUTPUT LINES TO BE TRUNCATED?
.VARI NOFIX
 SUPPRESS FIXING OF BAD LINES?
.VARI NOPRINT
 SUPPRESS MESSAGES?
.LEVEL2
.VARI INP
 STANDARD VICAR1 INPUT DATASET PARAMETER
 (ONE DATASET)
.VARI OUT
 STANDARD VICAR1 OUTPUT DATASET PARAMETER
 (ONE DATASET)
.VARI SIZE
 STANDARD VICAR1 SIZE PARAMETER.
.VARIABLE LINE
 NEW LINES WILL BE INSERTED AT THE LINE(S) SPECIFIED.
 LINE NUMBERS ARE WITH RESPECT TO OUTPUT PICTURE.
.VARIABLE DN
 DENOTES THE INSERTED LINE PIXELS HAVE A DN OF VALUE N.
.VARIABLE NOAVG
 VALID KEYWORD VALUE: NOAVG.

 'NOAVG' DENOTES THAT INSERTED LINES WILL HAVE ALL DN'S EQUAL
 TO ZERO (OR TO 'DN', IF THAT PARAMETER IS SPECIFIED).
 DEFAULT IS THAT INSERTED LINE PIXELS ARE GENERATED BY
 AVERAGING THE PIXELS IN NEIGHBORING LINES.

 IF PARAMETER 'DN' IS SPECIFIED, THEN NO AVERAGING WILL BE
 PERFORMED.
.VARIABLE TRUNC
 VALID KEYWORD VALUE: TRUNC.
 THIS DENOTES THE OUTPUT PICTURE WILL BE TRUNCATED TO
 THE NUMBER OF LINES SPECIFIED IN THE SIZE PARAMETER, OR,
 IF NO 'SIZE' SPECIFIED, IN THE INPUT IMAGE.
.VARI NOFIX
 VALID KEYWORD VALUE: NOFIX.

 THIS DENOTES THAT BAD LINES WILL BE WRITTEN AS READ.  DEFAULT IS 
 TO REPLACE A BAD LINE WITH A NEW LINE CONSISTING OF ZEROES OR AN
 AVERAGE OF THE ADJACENT LINES (DEPENDING ON WHETHER 'NOAVG' WAS
 SPECIFIED).

 A BAD LINE MEANS A LINE THAT GENERATED ANY I/O ERROR OTHER THAN
 'END OF FILE' WHEN READ.
.VARI NOPRINT
 VALID KEYWORD VALUE: NOPRINT.

 THIS CAUSES MESSAGES TO BE SUPPRESSED.  IF THIS KEYWORD IS NOT
 SPECIFIED, A MESSAGE WILL BE PRINTED FOR EVERY LINE INSERTED.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstinsert.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage = "none"
let _onfail="continue"
let $echo="yes"
!THIS IS A TEST OF PROGRAM INSERT
!FIRST, TEST ALL FUNCTIONS ON BYTE DATA
!al INS.DAT;1 NL=50 NS=50
! allocate extra line for insertion below!
gen OUT=GEN.DAT;1 NL=10 NS=10
list INP=GEN.DAT;1
label-list INP=GEN.DAT;1
!COPY FILE ONLY
insert INP=GEN.DAT;1 SIZE=(3,3,5,5) OUT=INS.DAT;1
list INP=INS.DAT;1
label-list INP=INS.DAT;1
!INSERT A LINE BY AVERAGING TWO LINES
insert INP=GEN.DAT;1 OUT=INS.DAT;1 LINE=5
list INP=INS.DAT;1
! SUPPRESS AVERAGING
insert INP=GEN.DAT;1 OUT=INS.DAT;1 'NOAVG LINE=5
list INP=INS.DAT;1
label-list INP=INS.DAT;1
!INSERT A LINE AT 128 DN; MAINTAIN SAME SIZE
insert INP=GEN.DAT;1 OUT=INS.DAT;1 DN=128 LINE=5 'TRUNC
list INP=INS.DAT;1
label-list INP=INS.DAT;1
! INSERT MULTIPLE LINES, RANDOM ORDER
insert INP=GEN.DAT;1 OUT=INS.DAT;1 LINE=(13,3,8,4)
list INP=INS.DAT;1
!NOW FOR HALFWORD DATA
gen OUT=GEN.DAT;1 NL=10 NS=10 LINC=100 SINC=100 FORMAT=HALF
list INP=GEN.DAT;1
label-list INP=GEN.DAT;1
!COPY FILE ONLY
insert INP=GEN.DAT;1 SIZE=(3,3,5,6) OUT=INS.DAT;1
list INP=INS.DAT;1
label-list INP=INS.DAT;1
!INSERT A LINE BY AVERAGING TWO LINES
insert INP=GEN.DAT;1 OUT=INS.DAT;1 LINE=5
list INP=INS.DAT;1
label-list INP=INS.DAT;1
!INSERT A LINE AT 1234 DN; MAINTAIN SAME SIZE FIELD
insert INP=GEN.DAT;1 OUT=INS.DAT;1 DN=1234 LINE=5 'TRUNC
list INP=INS.DAT;1
label-list INP=INS.DAT;1
end-proc
$ Return
$!#############################################################################
