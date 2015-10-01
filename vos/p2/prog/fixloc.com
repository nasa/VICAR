$!****************************************************************************
$!
$! Build proc for MIPL module fixloc
$! VPACK Version 1.8, Friday, July 25, 1997, 13:28:17
$!
$! Execute by entering:		$ @fixloc
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
$ write sys$output "*** module fixloc ***"
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
$ write sys$output "Invalid argument given to fixloc.com file -- ", primary
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
$   if F$SEARCH("fixloc.imake") .nes. ""
$   then
$      vimake fixloc
$      purge fixloc.bld
$   else
$      if F$SEARCH("fixloc.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake fixloc
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @fixloc.bld "STD"
$   else
$      @fixloc.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create fixloc.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack fixloc.com -
	-s fixloc.f -
	-i fixloc.imake -
	-p fixloc.pdf -
	-t tstfixloc.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create fixloc.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C      PROGRAM FIXLOC

C  REVISION HISTORY
C     7-97  RRD  MERGED CA'S ADDITIONS INTO PORTED VERSION
C     4-94  CRI  MSTP S/W CONVERSION (VICAR PORTING)
C     4-94  CA   ADDED READING OF GRID NC,NR FROM LABEL
C    12-93  CA   ADDED DUMMY AND NOPRINT
C     4-86  SP   ADDED OPEN_ACT AND IO_ACT TO XVOPEN CALLS.
C     4-86  SP   DELETED XVADD CALL AND PUT ASSOCIATED PARAMETERS IN XVOPEN
C                CALL.
C     4-86  SP   CONVERTED TO USE XVPARM.
C     4-86  SP   CHANGED MVE CALLS TO USE DCODE OF 7.  DCODE OF 8 DOES  NOT
C                WORK WHEN THE LINE COORDINATE IS 0.0


      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      IMPLICIT INTEGER(A-Z)
      INCLUDE 'fortport'

      REAL*4 X(2,1000) 
      CHARACTER*132 MSG
      DATA MSG/' ***  (****.*,****.*)    (****.*,****.*)'/
      CHARACTER*132 DEL
      CHARACTER*132 INS
      REAL*4 LOC(2,1000),DL,DS,DMIN,R1,R2
      INTEGER PAR(9000)
      real rpar(9000)
      LOGICAL XVPTST
      EQUIVALENCE (PAR,RPAR),(LOC,X)
C
      CALL IFMESSAGE('FIXLOC version 24-JULY-97')

      CALL XVUNIT(inunit,'INP',1,status,' ')
      CALL XVOPEN(inunit,status,
     .            'OPEN_ACT','SA','IO_ACT','SA',' ')
      CALL XVSIZE(sl,ss,nl,ns,nli,nsi)
      NLOC = MAX0(NSI,NS)/2
      NC = 0
      NR = 0
      CALL XVREAD(inunit,X,status,'NSAMPS',NSI,' ')

C          SCAN FOR SPECIAL END OF RECORD MARK
      DO 1 I = 1, NLOC
         IF (LOC(1,I).EQ.-999.0 .AND. LOC(2,I).EQ.-999.0) GOTO 1000
    1 CONTINUE
      I = NLOC + 1
 1000 NLOC = I - 1
      NLOCSV = NLOC
C
C-------GET GRID SIZE FROM REFERENCE LABEL EITHER INTERLOC OR GRIDLOCB
        CALL XLGET(INUNIT,'HISTORY','GRID_NROW',NR,IST,'HIST',
     1             'INTERLOC','INSTANCE',1,'FORMAT','INT',' ')
        IF (IST .NE. 1) 
     1    CALL XLGET(INUNIT,'HISTORY','GRID_NROW',NR,IST,'HIST',
     2               'GRIDLOCB','INSTANCE',1,'FORMAT','INT',' ')
        IF (IST .NE. 1) 
     1    CALL XVMESSAGE('GRID_NROW NOT FOUND IN LABEL',' ')

        CALL XLGET(INUNIT,'HISTORY','GRID_NCOL',NC,IST,'HIST',
     1             'INTERLOC','INSTANCE',1,'FORMAT','INT',' ')
        IF (IST .NE. 1) 
     1	  CALL XLGET(INUNIT,'HISTORY','GRID_NCOL',NC,IST,'HIST',
     2               'GRIDLOCB','INSTANCE',1,'FORMAT','INT',' ')
        IF (IST .NE. 1) 
     1    CALL XVMESSAGE('GRID_NCOL NOT FOUND IN LABEL',' ')
C
C....READ PARAMETERS

      CALL XVPARM( 'NC', PAR, ICOUNT, IDEF, 0)
      IF ( ICOUNT .GT. 0 )  THEN
           NC = PAR(1)
      END IF

      CALL XVPARM( 'CHANGE', PAR, ICOUNT, IDEF, 0)
      IF ( ICOUNT .GT. 0 )  THEN
           IF ( MOD( ICOUNT, 3 ) .NE. 0 )  THEN
              CALL XVMESSAGE( 'INVALID COUNT FOR CHANGE PARAM.',' ' )
              CALL ABEND
           END IF

           CALL XVMESSAGE('THE FOLLOWING LOCATIONS ARE CHANGED',' ')
           CALL XVMESSAGE('LOC       INPUT              OUTPUT',' ')
           CALL XVMESSAGE(' ',' ')

           DO II = 1, ICOUNT, 3
             J = RPAR(II)
             WRITE (MSG(2:4),'(I3)') J
             WRITE (MSG(8:13),'(F6.1)') X(1,J)
             WRITE (MSG(15:20),'(F6.1)') X(2,J)
             X(1,J) = RPAR(II+1)
             X(2,J) = RPAR(II+2)
             WRITE (MSG(27:32),'(F6.1)') X(1,J)
             WRITE (MSG(34:39),'(F6.1)') X(2,J)
             CALL XVMESSAGE(MSG(2:40),' ')
           END DO
      END IF

      CALL XVPARM( 'DUMMY', PAR, ICOUNT, IDEF, 0 )
      IF ( ICOUNT .GT. 0 )  THEN

           CALL XVMESSAGE('THE FOLLOWING LOCATIONS ARE CHANGED',' ')
           CALL XVMESSAGE('LOC       INPUT              OUTPUT',' ')
           CALL XVMESSAGE(' ',' ')

           DO II = 1, ICOUNT
             J = PAR(II)
             WRITE (MSG(2:4),'(I3)') J
             WRITE (MSG(8:13),'(F6.1)') X(1,J)
             WRITE (MSG(15:20),'(F6.1)') X(2,J)
	     X(1,J) = -99.
	     X(2,J) = -99.
             WRITE (MSG(27:32),'(F6.1)') X(1,J)
             WRITE (MSG(34:39),'(F6.1)') X(2,J)
             CALL XVMESSAGE(MSG(2:40),' ')
           END DO
      END IF

      CALL XVPARM( 'DELETE', PAR, ICOUNT, IDEF, 0 )
      IF ( ICOUNT .GT. 0 )  THEN
           DO  II = 1, ICOUNT
             J = RPAR(II)
             WRITE (DEL,9900) J,X(1,J),X(2,J)
9900  FORMAT (' LOCATION ',I3,' (',F6.1,',',F6.1,') IS DELETED')
             CALL XVMESSAGE(DEL(2:40),' ')

             IF (NLOC .GT. J) THEN
               CALL MVE(7,2*(NLOC-J),X(1,J+1),X(1,J),1,1)
             END IF
             NLOC = NLOC - 1
           END DO
      END IF

      CALL XVPARM( 'INSERT', PAR, ICOUNT, IDEF, 0 )
      IF ( ICOUNT .GT. 0 )  THEN
           IF ( MOD( ICOUNT, 3 ) .NE. 0 )  THEN
              CALL XVMESSAGE( 'INVALID COUNT FOR INSERT PARAM.',' ' )
              CALL ABEND
           END IF

           DO II = 1, ICOUNT, 3
             J = RPAR(II)
             NLOC = NLOC + 1
             IF (NLOC .GE. J) THEN
                CALL MVE(7, 2*(NLOC-J+1), X(2,NLOC-1),X(2,NLOC),-1,-1)
             END IF
             X(1,J) = RPAR(II+1)
             X(2,J) = RPAR(II+2)
             WRITE (INS,9910) J,X(1,J),X(2,J)
9910  FORMAT (' LOCATION ',I3,' (',F6.1,',',F6.1,') IS INSERTED')
             CALL XVMESSAGE(INS(2:41),' ')
           END DO
      END IF
C
      IF (XVPTST('NOPRINT')) GO TO 100
C
   60 CALL XVMESSAGE('CORRECTED CENTERS',' ')
      IF (NLOC .EQ. 202) GOTO 72
      IF (NLOC .EQ. 103) GOTO 70
C-----ALREADY KNOW GRID NC, NR ?
      IF (NC .NE. 0) GOTO 68
C
C          DETERMINE FORMAT OF DATA
C              NR=NUMBER OF ROWS
C              NC=NUMBER OF COLUMNS
C          FIRST SEE IF SAMPLE COORDINATES INCREASE FASTER THAN LINES

      DL = 0.
      DS = 0.
C
      DO 61 K=2,NLOC
         IF(LOC(1,K).LT.0.) GOTO 61
         DL = DL + ABS(LOC(1,K)-LOC(1,K-1))
         DS = DS + ABS(LOC(2,K)-LOC(2,K-1))
   61 CONTINUE
C          (I1,I2)=(1,2) IF CENTERS ARE ARRANGED ROW BY ROW
C                 =(2,1) IF COLUMN BY COLUMN
      I1 = 1
      IF (DL .GT. DS) I1 = 2
      I2 = 3 - I1
      N = 1
      MINN = 1
C
      DO 62 K=2,NLOC
         IF(LOC(I2,K-1).GT.0.AND.LOC(I2,K).GT.LOC(I2,K-1)) GOTO 62
         IF(N.GT.MINN) MINN=N
         N = 0
   62 N = N + 1
C
      DMIN = 1.0E32
      MAXN = NLOC/2
C
      DO 64 N=MINN,MAXN
         M = NLOC/N
         IF(M*N.NE.NLOC) GOTO 64
         DS = 0.
         KNT = 0
         K = 1
C
         DO 63 J=1,M
            R2 = LOC(I1,K)
            K = K + 1
C
            DO 63 I=2,N
               R1 = R2
               R2 = LOC(I1,K)
               IF(R1.LT.0..OR.R2.LT.0.) GOTO 63
               DS = DS + ABS(R2-R1)
               KNT = KNT + 1
   63    K = K + 1
C
         DS = DS/KNT
         IF (DS .GT. DMIN) GOTO 64
         DMIN =DS
         NC = N
   64 CONTINUE
C
C
   68 IF (NC .LE. 1) NC = 20
      IF (NR .LE. 1) NR = (NLOC-1) / NC + 1
      CALL PGRID(LOC,NR,NC,TRIX,0)
      GOTO 100
C
C          VO RESEAU PATTERN
   70 CALL PU75(LOC,1)
      GOTO 100
C
C          MJS RESEAU PATTERN
   72 CALL PMJS(LOC,1)
C
  100 CALL XVPCNT( 'OUT', NO )  ! NUMBER OF OUTPUT FILES.
      IF (NO .LT. 1) RETURN

      CALL XVUNIT(outunit,'OUT',1,status,' ')

      CALL XVOPEN(outunit,status,'OP','WRITE','U_NL',1,
     .            'U_NS',NLOC*2,'OPEN_ACT','SA','IO_ACT','SA',' ')
      CALL XVWRIT(outunit,X,status,'NSAMPS',NLOC*2,' ')
      CALL XVCLOSE(inunit,status,' ')
      CALL XVCLOSE(outunit,status,' ')
C
      RETURN
      END


$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create fixloc.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM fixloc

   To Create the build file give the command:

		$ vimake fixloc			(VMS)
   or
		% vimake fixloc			(Unix)


************************************************************************/


#define PROGRAM	fixloc
#define R2LIB

#define MODULE_LIST fixloc.f 

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
$ create fixloc.pdf
process help=*
PARM INP    TYPE=STRING
PARM OUT    TYPE=STRING	  COUNT=0:1		DEFAULT=--
PARM SIZE   TYPE=INTEGER  COUNT=4		DEFAULT=(1,1,0,0)
PARM CHANGE TYPE=REAL     COUNT=0:300		DEFAULT=--
PARM DELETE TYPE=REAL     COUNT=0:100		DEFAULT=--
PARM DUMMY  TYPE=INTEGER  COUNT=0:100		DEFAULT=--
PARM INSERT TYPE=REAL     COUNT=0:200		DEFAULT=--
PARM MODE   TYPE=KEYWORD  COUNT=0:1  VALID=(PRINT,NOPRINT)  DEFAULT=PRINT
PARM NC     TYPE=INTEGER  COUNT=0:1		DEFAULT=--
END-PROC
.TITLE
VICAR program fixloc --	edit data sets containing coordinate information.
.HELP
PURPOSE:

 Fixloc is used to edit and list MARK-format data sets containing coordinate
 information. Examples of such data sets include grid location data sets as
 generated by gridlocb, gridgen, and interloc and reseau location data sets 
 as generated by resloc and reslocvo.

EXECUTION:

		fixloc INP OUT PARAMS

.page
OPERATION:

 Fixloc reads the coordinates from the input file and acts upon them as
 specified by the parameters before writing them out in the same
 MARK-format (see program MARK).  The following edit operations are
 performed:

	CHANGE - replaces the value of the specified coordinate with
		 user-specified values.
	DUMMY -  replaces the value of the specified coordinate with
                 the values (-99.,-99.) (the 'missing' value).
	DELETE - eliminates the specified coordinate and renumbers the set.
	INSERT - adds the specified coordinate and values and renumbers 
  		 the set.

 The CHANGE operations are executed first, then the DUMMY operations
 (essentially shorthand versions of CHANGE), then the DELETE operations,
 and lastly the INSERT operations.   The DELETE and INSERT parameters
 cause an immediate renumbering of the tiepoints. If the 3rd coordinate is
 deleted, for example, then all coordinates following it are renumbered to fill
 in the gap.  This is a potential source for confusion and a serious defect in
 the program, since, e.g., DELETE=(6,50)  deletes the 6th and 51st points!

 The coordinates, with corrections (if any), are listed out unless
 the 'NOPRINT keyword is used.  An array format (see NC parameter) is used 
 unless a VO or VGR reseau data set is input, in which case, the program 
 prints out the standard reseau map.

 NOTE:  Although the output file is optional, the input is never modified.
        With no output file, the changes only occur in the printout.

ORIGINAL PROGRAMMER: Gary Yagi			29 November 1978

CURRENT COGNIZANT PROGRAMMER: S. Pohorsky

REVISIONS:
   7-97    ...RRD... merged CCA's additions into ported version
   4-94    ...CRI... MSTP S/W CONVERSION (VICAR PORTING)
   4-94    ...CCA... added reading of grid nc,nr from label
  12-93    ...CCA... added DUMMY and NOPRINT parameters, some help
  04-86    ...SXP... revised parameter processing using XVPARM.
  11-78    ...GMY... original release

.LEVEL1
.VARI INP
 STRING-Input file name.
.VARI OUT
 STRING-Output file name.
.VARI CHANGE
 REAL-Tiepoints and values
 to be changed.
.VARI DUMMY
 INTEGER-Tiepoints
 to be set to -99.,-99.
.VARI DELETE
 REAL-Tiepoints to be 
 deleted.
.VARI INSERT
 REAL-Tiepoints and values
 to be inserted.
.VARI NC
 INTEGER-Number of coordinates
 to be printed on each row.
.VARI MODE
 KEYWORD-Specifies whether
 to print the corrected
 array of tiepoints.
.LEVEL2
.VARI INP
 STRING - Input coordinate data set. 
.VARI OUT
 STRING - Optional data set containing corrected coordinates.
.VARI CHANGE
 REAL - A string of triplets consisting of a tiepoint number followed
 by the line and sample coordinates of that tiepoint.
.VARI DELETE
 INTEGER - A list of tiepoints to be set to the 'missing' value of
 (-99.,-99.).
.VARI DELETE
 REAL - A list of tiepoints to be deleted.
.VARI INSERT
 REAL - A string of triplets consisting of a tiepoint number followed
 by the line and sample coordinates of that tiepoint which are inserted 
 between the n-1 and n tiepoint.
.VARI NC
 INTEGER - Specifies the number of coordinates to be printed on each row. 
 The corrected coordinates are printed in a rectangular format unless a 
 VO or VGR reseau data set is input, in which case, the program prints out 
 the standard reseau map.
 If NC is not given, fixloc will attempt to figure out the grid dimensions.
.VARI MODE
 KEYWORD - Specifies whether to print the corrected coordinate array or
 not.  Valid values are:  'PRINT and 'NOPRINT.  Default is to PRINT.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstfixloc.pdf
procedure
refgbl $echo
refgbl $syschar
local DIR    type=string
local INPIC  type=string

body
let _onfail="continue"
let $echo="yes"
if ($syschar(1) = "UNIX")
   let DIR   ="/project/test_work/testdata/cassini/iss/"
else
   let DIR   ="WMS_TEST_WORK:[TESTDATA.CASSINI.ISS]"
end-if

write "This will change the coordinate values of location 3 and 6"
gridgen v nrow=10 ncol=10 inc=5
list v (1,5,1,8)
fixloc v a change=(3,102,150,6,32,251)
list a (1,5,1,8)

write "This will del the coordinates at location 6 and 10 and print out"
write "only 98 values while also printing them out to a specified file."
list v (1,11,1,10)
fixloc v a del=(6,10)
list a (1,11,1,10)

write "This will insert 2 values and print out 102 values"
list v (1,3,1,10)
fixloc v a insert=(2,501,616,5,500.5,-100.1)
list a (1,3,1,10)

write "This will set coordinate 12 to (-99.,-99.) without printing."
list v (1,23,1,4)
fixloc v a dummy=(12) 'noprint
write "list up to coordinate 12."
list a (1,23,1,4)

write "Read the label for the grid size"
let INPIC = "&DIR"//"grid.interloc"
fixloc &INPIC             !INTERLOC LABEL
let INPIC = "&DIR"//"grid.mark"
fixloc &INPIC             !GRIDLOCB LABEL
end-proc
$ Return
$!#############################################################################
