$!****************************************************************************
$!
$! Build proc for MIPL module dropout
$! VPACK Version 1.7, Monday, May 23, 1994, 13:00:32
$!
$! Execute by entering:		$ @dropout
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
$ write sys$output "*** module dropout ***"
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
$ write sys$output "Invalid argument given to dropout.com file -- ", primary
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
$   if F$SEARCH("dropout.imake") .nes. ""
$   then
$      vimake dropout
$      purge dropout.bld
$   else
$      if F$SEARCH("dropout.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake dropout
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @dropout.bld "STD"
$   else
$      @dropout.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create dropout.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack dropout.com -
	-s dropout.f -
	-i dropout.imake -
	-p dropout.pdf -
	-t tstdropout.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create dropout.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C 5 SEP 1994 .... CRI .... MSTP S/W CONVERSION (VICAR PORTING)

      include 'VICMAIN_FOR'
      SUBROUTINE MAIN44 ! DROPOUT
      IMPLICIT INTEGER(A-Z)
      INTEGER DROPPED(2000)
      INTEGER bufsize
      INTEGER*2 BUF(1000000)
      CHARACTER*80 MSG1
      LOGICAL MISSING, MORE
      COMMON/C1/SL,SB,NLO,NBO,NLI,NBI

      DATA BUFSIZE/1000000/
      CALL IFMESSAGE('DROPOUT version 5-SEP-94')
      CALL XVEACTION('SA',' ')

C     Open input data
      CALL XVUNIT(UNIT2,'INP',1,IND,' ')
      CALL XVOPEN(UNIT2,IND,'U_FORMAT','HALF',' ')

C     Open output data 
      CALL XVUNIT(UNIT1,'OUT',1,IND,' ')
      CALL XVOPEN(UNIT1,IND,'OP','WRITE',' ')

C     Get size field
      CALL XVSIZE(SL,SB,NLO,NBO,NLI,NBI)
      EL=SL+NLO-1
      IF(EL.GT.NLI)NLO=NLI-SL+1

C     Param processing
      CALL XVPARM('THRESH',THRESH,COUNT,DEF,1)

      NDROP=0
      NSO=NBO
      DO I=SL,EL
         CALL XVREAD(UNIT2,BUF,IND,' ')
         J=0
         MISSING=.TRUE.
         MORE=.TRUE.
         DO WHILE(MORE.AND.MISSING)!LOOK FOR DROPPED LINE
            J=J+1
            IF(BUF(J).GT.THRESH)MISSING=.FALSE.
            IF(J.EQ.NSO)MORE=.FALSE.
         ENDDO
         IF(MISSING)THEN  ! SAVE THE LINE NUMBER OF MISSING LINES
              NDROP=NDROP+1
              DROPPED(NDROP)=I
         ELSE
              CALL FIX(BUF,NSO,THRESH)  ! FIX GROUPS OF MISSING PIXELS
         ENDIF
         CALL XVWRIT(UNIT1,BUF,IND,' ')
      ENDDO
      call xvclose(unit1,ind,' ')
      CALL XVOPEN(UNIT1,IND,'OP','UPDATE','U_FORMAT','HALF',' ')
c             fix missing lines, groups of missing lines should be consecutive
      IF(NDROP.GT.0)THEN
         START=DROPPED(1)
         STOP=START
         DO I=1,NDROP
            IF(DROPPED(I+1).GT.STOP+1.or.i.eq.ndrop)THEN
               if((stop-start+2)*nso.lt.bufsize)then
                  CALL FIXLINES(START,STOP,BUF,NSO,STOP-START+1,UNIT1)
                  START=DROPPED(I+1)
                  STOP=START
               else
                  write(MSG1,101)START,STOP
101               format('too many lines to fix, start=',I4,' stop=',I4)
                  CALL XVMESSAGE(MSG1,' ')
               endif
            ELSE
               STOP=STOP+1
            ENDIF
         ENDDO
      ENDIF
      RETURN
      END
      SUBROUTINE FIXLINES(START,STOP,BUF,NSO,NLINES,UNIT1)
      IMPLICIT INTEGER(A-Z)
      INTEGER*2 BUF(NSO,NLINES+2)
      REAL F1,F2,DY
      CHARACTER*80 MSG1
      COMMON/C1/SL,SB,NLO,NBO,NLI,NBI
      IF(START.EQ.1.OR.STOP.EQ.NLO)RETURN
       REC=START-SL
       CALL XVREAD(UNIT1,BUF(1,1),IND,'LINE',REC,' ')
c        last row in buffer is next good line
      REC=STOP-SL+2
      CALL XVREAD(UNIT1,BUF(1,NLINES+2),IND,'LINE',REC,' ')
      WRITE(MSG1,102)START,STOP
102   FORMAT(' lines ',I4,' to ',I4,' are missing')
      CALL XVMESSAGE(MSG1,' ')
      DO I=1,NSO
          F1=BUF(I,1)
          F2=BUF(I,NLINES+2)
          DY=(F2-F1)/(NLINES+1)
          DO J=1,NLINES
             F1=F1+DY
             BUF(I,J+1)=F1
          ENDDO
      ENDDO
      DO I=1,NLINES
           REC=START-1+I-SL+1
           CALL XVWRIT(UNIT1,BUF(1,I+1),IND,'LINE',REC,' ')
      ENDDO
      RETURN
      END
      SUBROUTINE FIX(BUF,NBO,THRESH)
      IMPLICIT INTEGER(A-Z)
      INTEGER*2 BUF(10000)
      LOGICAL MORE,MORE2
      MORE=.TRUE.
C     SKIP OVER MASK
      LEFT=0
      DO WHILE(MORE)
         LEFT=LEFT+1
         IF(BUF(LEFT).GT.THRESH)MORE=.FALSE.
         IF(LEFT.EQ.NBO)MORE=.FALSE.
      ENDDO
      MORE=.TRUE.
      RIGHT=NBO+1
      DO WHILE(MORE)
         RIGHT=RIGHT-1
         IF(BUF(RIGHT).GT.THRESH)MORE=.FALSE.
         IF(RIGHT.EQ.1)MORE=.FALSE.
      ENDDO
      MORE=.TRUE.
      I=LEFT-1
      DO WHILE(MORE)
         I=I+1
         IF(I.ge.RIGHT)MORE=.FALSE.
         IF(BUF(I).LE.THRESH)THEN
            START=I
            MORE2=.TRUE.
            DO WHILE(MORE2)
               I=I+1
               if(i.ge.right)more2=.false.
               IF(BUF(I).GT.THRESH)THEN
                  MORE2=.FALSE.  ! 
                  STOP=I-1
               ENDIF
            ENDDO
            if(stop.ge.start)then
               DNSTART=BUF(START-1)
               DNSTOP=BUF(STOP+1)
               CALL INTRPA(2,STOP-START+2,BUF(START-1),DNSTART,DNSTOP)
            endif
         ENDIF
      ENDDO
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create dropout.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM dropout

   To Create the build file give the command:

		$ vimake dropout			(VMS)
   or
		% vimake dropout			(Unix)


************************************************************************/


#define PROGRAM	dropout
#define R2LIB

#define MODULE_LIST dropout.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create dropout.pdf
process help=*
PARM INP          TYPE=STRING       COUNT=1
PARM OUT          TYPE=STRING       COUNT=1
PARM SIZE         TYPE=INTEGER      COUNT=4            DEFAULT=(1,1,0,0)
PARM SL           TYPE=INTEGER                         DEFAULT=1
PARM SS           TYPE=INTEGER                         DEFAULT=1
PARM NL           TYPE=INTEGER                         DEFAULT=0
PARM NS           TYPE=INTEGER                         DEFAULT=0
PARM THRESH       TYPE=INTEGER                         DEFAULT=0
END-PROC
.TITLE
VICAR2 program "dropout"
.HELP
PURPOSE:
"dropout" is a VICAR program which finds and fixes pixel drop outs and
missing lines. Viking Orbiter images can have series of zero pixels
corresponding to data missing from the tape recorder. Data was
originally recorded longitudinally on the tape but was played back
latitudinally. When a data outage occurred, a regular pattern of
missing pixels resulted. Ground reconstruction of the data sometimes
lost whole lines. DROPOUT fixes both of these problems by
interpolating over the missing data. 

 Written By: Joel Mosher,  24-JUN-1986
 Cognizant Programmer: Joel Mosher
 9-25-86 ... Converted to VICAR2 param i/o...FFM
 9-5-94  ... Made portable for UNIX by A. Scop (CRI)
.page
OPERATION:
"dropout" reads each line and locates the missing lines, saving the
line numbers in a buffer. Up to 2000 lines can be saved. If the line
is not missing, the line is examined pixel by pixel to locate missing
pixels. The missing pixels are fixed by interpolation. After each
line is fixed, DROPOUT goes back to fix the missing lines. Data is
determined to be missing if the pixel values are less than or equal
to THRESH (default is THRESH=0) 

TIMING:
depends on number of missing pixels and lines but usually only a
couple of minutes. 

EXECUTION:
  "dropout" can be executed in the following manner:
    dropout INP=IN OUT=OUT SIZE=(...) PARAMS
  where IN, OUT, SIZE, and PARAMS are parameters and are
  explained in their respective PARAMETER section.

.LEVEL1
.VARI INP
A standard VICAR input file
.VARI OUT
Output file
.VARI SIZE
Standard VICAR size field.
.VARI SL
Starting line
.VARI SS
Starting sample
.VARI NL
Number of lines
.VARI NS
Number of samples
.VARI THRESH
Threshold for determining 
if data are missing. 
.LEVEL2
.VARI IN
INP=A where A is the name of a standard VICAR input file.
A is the only input file and the name of the file to be processed.
.VARI OUT
OUT=B where B is the name of a standard Vicar output file.
.VARI SIZE
SIZE=(0,0,nl,ns) where nl is the number of lines and ns is the number of
samples and defines a standard VICAR size field.
.VARI THRESH
Threshold for determining if data are missing. Default is 0.
$ Return
$!#############################################################################
$Test_File:
$ create tstdropout.pdf
procedure
refgbl $autousage
refgbl $echo
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
gen a nl=45 ns=27
write " zero out lines 10 thru 20"
write " put a black rectangle 10 by 10 at line 30 samp 10"
sargonb a b zero 0 (10,1,20,1,20,27,10,27) zero 0 (30,10,40,10,40,20,30,20)
write "list the input"
list b
dropout b a 
write "list the output"
list a
end-proc
$ Return
$!#############################################################################
