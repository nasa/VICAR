$!****************************************************************************
$!
$! Build proc for MIPL module zipcol
$! VPACK Version 1.8, Tuesday, February 14, 1995, 08:29:38
$!
$! Execute by entering:		$ @zipcol
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
$ write sys$output "*** module zipcol ***"
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
$ write sys$output "Invalid argument given to zipcol.com file -- ", primary
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
$   if F$SEARCH("zipcol.imake") .nes. ""
$   then
$      vimake zipcol
$      purge zipcol.bld
$   else
$      if F$SEARCH("zipcol.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake zipcol
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @zipcol.bld "STD"
$   else
$      @zipcol.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create zipcol.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack zipcol.com -
	-s zipcol.f -
	-p zipcol.pdf -
	-i zipcol.imake -
	-t tstzipcol.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create zipcol.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C
C---- IBIS PROGRAM "ZIPCOL".
C
C    6-March-1995 ... CRI ... MSTP S/W CONVERSION (VICAR PORTING)
C
C     PURPOSE: TO ENTER THE DATA INTO AN EXISTING IBIS
C      INTERFACE FILE FROM ANOTHER INTERFACE FILE. DATA IS ENTERED BY MATCHING
C      KEYS BETWEEN THE INTERFACE FILES. PRESORTING OF      BOTH FILES IS REQUIRED.
C     USER PARAMETERS:
C      INCOL,X1,...XK    INCOL IS USED TO SPECIFY COLUMNS IN INT1 WHICH ARE
C            TO BE USED AS MATCHING KEYS.
C      OUTCOL,Y1,...YN   OUTCOL IS USED TO SPECIFY COLUMNS OF INT1 WHICH WILL
C            RECEIVE DATA WHEN INCOL MATCHES ARE FOUND.
C      NULL,C1,...CN     NULL IS USED TO SPECIFY WHAT OPERATION WILL TAKE
C            PLACE IF NO MATCH IS FOUND FOR ANY OF THE KEYS IN INT1. IF
C            NULL IS NOT SPECIFIED THEN DATA IN COLUMNS Y1,...YN REMAIN
C            UNCHANGED. OTHERWISE VALUES C1,...CN ARE USED AS A REPLACEMENT.
C      FILE,D1,...DK,    COLUMNS D1,...DK ARE THE MATCHING E1,...EN KEYS IN
C            INT2 CORRESPONDING TO INCOL MATCHING KEYS. E1,...EN ARE THE
C            COLUMNS OF INT2 WHERE DATA TO BE TRANSFERED TO INT1 ARE FOUND.
C
      IMPLICIT NONE  !IMPLICIT INTEGER is just asking for trouble!
      INTEGER RUNIT1,RUNIT2,IBIS,IBIS_R,NIN,NOUT
      INTEGER COUNT,STATUS,CLENA,CLENB,NCOLA,NCOLB
      INTEGER INCODF,OUTCDF,NFIL,FILEDF,NULL,NULLDF,NULSDF
      INTEGER I,CSIZE,CSZ,IBIS_FILE_GET
      INTEGER RECORD,RECORD_R,JROW,OJROW,IROW
      INTEGER FILE,INCOL,OUTCOL,MAXOUT,MAXIN,MAXCOL
      INTEGER COL(1000000)
      LOGICAL USE_A256, ALPHA,XVPTST,IBIS2R,IBIS2U
      PARAMETER (MAXOUT=30,MAXIN=20,MAXCOL=MAXOUT+MAXIN)
      CHARACTER*4     CBUF1_A4(MAXIN),CBUF2_A4(MAXIN)
      CHARACTER*4     STRNUL_A4(MAXOUT)
      character*7     ibisftype,infmt,outfmt
      character*256   CBUF1_A256(MAXIN)
      character*256   STRNUL_A256(MAXOUT)
      REAL*4          RBUF1(MAXIN),RBUF2(MAXIN), VALNUL(MAXOUT)
      DIMENSION INCOL(MAXIN),OUTCOL(MAXOUT),FILE(MAXCOL)
C
      equivalence (CBUF1_A256(1),CBUF1_A4(1))
C
C       Initialize:
C
      ibisftype = '      '
      infmt     = '      '
      outfmt    = '      '
C
C---- OPEN FILES. READ PARAMETERS.
C
      CALL IFMESSAGE('ZIPCOL version 6-MAR-95')
      CALL XVUNIT(RUNIT1,'INP',1,STATUS,' ')
       CALL IBIS_FILE_OPEN(RUNIT1,IBIS,'UPDATE',0,0,' ',' ',STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
C
      CALL XVUNIT(RUNIT2,'INP',2,STATUS,' ')
      CALL IBIS_FILE_OPEN(RUNIT2,IBIS_R,'READ',0,0,' ',' ',STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
C
      COUNT=IBIS_FILE_GET(IBIS,'VERSION',ibisftype,1,1)
      IF (COUNT.LE.0) CALL IBIS_SIGNAL(IBIS,STATUS,1)
      IBIS2U =  (ibisftype(1:6).eq.'IBIS-2')
      if (IBIS2U) then
           call xvmessage('UPDATE file is IBIS-2',' ')
      else
           call xvmessage('UPDATE file is IBIS-1',' ')
      endif
C
      COUNT=IBIS_FILE_GET(IBIS_R,'VERSION',ibisftype,1,1)
      IF (COUNT.LE.0) CALL IBIS_SIGNAL(IBIS,STATUS,1)
      IBIS2R = (ibisftype(1:6).eq.'IBIS-2')
      if (IBIS2R) then
         call xvmessage('Reference  file is IBIS-2',' ')
      else
         call xvmessage('Reference  file is IBIS-1',' ')
      endif
C
      COUNT=IBIS_FILE_GET(IBIS,'NR',CLENA,1,1)
      IF (COUNT.LE.0) CALL IBIS_SIGNAL(IBIS,STATUS,1)
C
      COUNT=IBIS_FILE_GET(IBIS,'NC',NCOLA,1,1)
      IF (COUNT.LE.0) CALL IBIS_SIGNAL(IBIS,STATUS,1)
C
      COUNT=IBIS_FILE_GET(IBIS_R,'NR',CLENB,1,1)
      IF (COUNT.LE.0) CALL IBIS_SIGNAL(IBIS,STATUS,1)
C
      COUNT=IBIS_FILE_GET(IBIS_R,'NC',NCOLB,1,1)
      IF (COUNT.LE.0) CALL IBIS_SIGNAL(IBIS,STATUS,1)
C
      ALPHA = XVPTST('ALPHA')
      CALL XVPARM ('INCOL',INCOL,NIN,INCODF,MAXIN)
      CALL XVPARM ('OUTCOL',OUTCOL,NOUT,OUTCDF,MAXOUT)
      CALL XVPARM ('FILE',FILE,NFIL,FILEDF,MAXCOL)
      CALL XVPARM ('NULL',VALNUL,NULL,NULLDF,MAXOUT)
      IF (ALPHA.AND.NULLDF.EQ.0)
     +    call mabend('*** Cant specify NULL for ALPHA ***')
      CALL XVPARM ('NULSTR',STRNUL_A256,NULL,NULSDF,MAXOUT)
      IF (.NOT.ALPHA.AND.NULSDF.EQ.0)
     +    call mabend('*** Cant specify NULSTR for Non-ALPHA ***')
       if (ALPHA.and.IBIS2R.and..not.IBIS2U)
     +    call mabend('*** File types not compatable! ***')
C
C ---- set the ibis file formats
C
      USE_A256=.false.
      if (ALPHA)  then

           infmt='A4'  ! read and sort only 1st 4 characters even if ibis-2
           outfmt='A4'  ! read and sort only 1st 4 characters even if ibis-2

           !
           ! -- Set up Reference File
	   !
           if (.not.IBIS2R) then  ! coerce ibis-1 columns to ascii
              do i=1,NFIL
               call ibis_column_set(IBIS_R,'FORMAT','A4',
     +                 FILE(i),STATUS)
              enddo
           endif

           !
           ! -- Set up Update File
	   !
           USE_A256=.FALSE.
           if (IBIS2U) then    ! UPDATE file is ibis-2
                  if (NULL.gt.1) then       ! NULSTR is user specified
		      do i=1,NOUT
                         call ibis_column_get(IBIS,'U_SIZE',
     +                         CSIZE,i,STATUS)
                         IF (STATUS.NE.1) 
     +				CALL IBIS_SIGNAL(IBIS,STATUS,1)
			 csz = csize -1
			 if (csz.GT.4) USE_A256 = .TRUE.
                      enddo
                  endif
                  if (USE_A256) outfmt = 'A256' 
          else                ! UPDATE file is ibis-1 -- coerce to ascii
              do i=1,NOUT
                call ibis_column_set(IBIS,'FORMAT','A4',
     +                          OUTCOL(i),STATUS)
                IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
              enddo
              do i=1,NIN
                 call ibis_column_set(IBIS,'FORMAT','A4',
     +                          INCOL(i),STATUS)
                 IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
              enddo
         endif
	 if (.not.USE_A256) THEN
	 	DO I=1,NULL
		   STRNUL_A4(I) = STRNUL_A256(I)(1:4)
		ENDDO
	 ENDIF
      else    ! not ALPHA
         infmt ='REAL'
         outfmt='REAL'
      endif
C
C-- Sanity check
C
      IF (((NULLDF.EQ.0).AND.(NULSDF.EQ.0)).OR.    
     +       (NIN+NOUT.NE.NFIL)) GO TO 666   ! a parameter fault
     
C
C---- READ AND COMPARE KEYS.
C
C
C
C--- open & read the update file (INCOL) into RBUF1 or CBUF1_A4
C
      CALL IBIS_RECORD_OPEN (IBIS,RECORD,' ',
     +                               INCOL,NIN,infmt,STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
      CALL IBIS_RECORD_OPEN (IBIS_R,RECORD_R,' ',
     +                        FILE,NIN,infmt,STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
      
      
      JROW = 1                    !
      OJROW = 0
      DO IROW=1,CLENA             !
            COL(IROW) = 0             !
            if (ALPHA) then
                  call ibis_record_read(RECORD,CBUF1_A4,IROW,STATUS)
            else
                  CALL IBIS_RECORD_READ(RECORD,RBUF1,IROW,STATUS)
            endif
            IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)

            !   --- open & read Ref file (matching col) into RBUF2 or CBUF2_A4

 5          IF (JROW.NE.OJROW) THEN
                  if (ALPHA) then
                     call ibis_record_read(RECORD_R,CBUF2_A4,
     +                                                JROW,STATUS)
                  else
                     CALL IBIS_RECORD_READ(RECORD_R,RBUF2,JROW,STATUS)
                  endif
                  IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
            ENDIF

            OJROW = JROW
            DO I=1,NIN
                  IF ((.NOT.ALPHA.AND.RBUF1(I).GT.RBUF2(I))
     *              .OR. (ALPHA.AND.CBUF1_A4(I).GT.CBUF2_A4(I))) THEN
                  JROW = JROW+1
                  IF (JROW.GT.CLENB) GO TO 51
                  GO TO 5
                  ELSEIF (.NOT.ALPHA.AND.RBUF1(I).LT.RBUF2(I)
     *              .OR. ALPHA.AND.CBUF1_A4(I).LT.CBUF2_A4(I) ) THEN
                  GO TO 50
                  ENDIF
            ENDDO
            COL(IROW) = JROW ! if COL(IROW) = matching row no.; 0=no match
 50   ENDDO
 51   DO I=1,NOUT             ! move out col numbers 
            FILE(I) = FILE(I+NIN)
      ENDDO
      CALL IBIS_RECORD_CLOSE(RECORD,STATUS)
      CALL IBIS_RECORD_CLOSE(RECORD_R,STATUS)

      !---- MERGE.

      do i=1,NOUT
            call ibis_column_set(IBIS,'U_FORMAT',outfmt,
     +                                     OUTCOL(i),STATUS)
            IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
            call ibis_column_set(IBIS_R,'U_FORMAT',outfmt,
     +                                     file(i),STATUS)
            IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
      enddo
      CALL IBIS_RECORD_OPEN (IBIS_R,RECORD_R,' ',
     +                        FILE,NOUT,outfmt,STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
      CALL IBIS_RECORD_OPEN (IBIS,RECORD,' ',
     +                         OUTCOL,NOUT,outfmt,STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
      JROW = 1
      DO IROW=1,CLENA
	    JROW = COL(IROW)
            IF (JROW.NE.0) THEN   ! if match read out data to buffers
                  if (ALPHA) then
		     IF (USE_A256) THEN
                        call ibis_record_read(RECORD_R,CBUF1_A256,
     +                                            JROW,STATUS)
		     ELSE
                        call ibis_record_read(RECORD_R,CBUF1_A4,
     +                                            JROW,STATUS)
		     ENDIF
                  else
                     CALL IBIS_RECORD_READ(RECORD_R,RBUF1,JROW,STATUS)
                  endif
                  IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
                  if (ALPHA) then
		     IF (USE_A256) THEN
                        call ibis_record_write(RECORD,CBUF1_A256,
     +                                           IROW,STATUS)
                     ELSE
                        call ibis_record_write(RECORD,CBUF1_A4,
     +                                           IROW,STATUS)
                     ENDIF
                  else
                        CALL IBIS_RECORD_WRITE(RECORD,RBUF1,
     +                                           IROW,STATUS)
                  endif
                  IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
            ELSE    ! no matching data COL(IROW)=0
                  IF (ALPHA.AND.NULSDF.EQ.0) THEN
                      !--   write the string specified by user
                      if (USE_A256) then
                          call ibis_record_write(RECORD, 
     +                           STRNUL_A256,IROW,STATUS)  
                      else    ! for ibis-1 files write STRNUL_A256              
	                   call ibis_record_write(RECORD,
     +                            STRNUL_A4,IROW,STATUS)
                      endif
                      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
                  ELSEIF (.NOT.ALPHA.AND.NULLDF.EQ.0) THEN 
	              call ibis_record_write(RECORD,
     +                            VALNUL,IROW,STATUS)
                      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
                  ENDIF
            ENDIF
      ENDDO
      CALL IBIS_RECORD_CLOSE(RECORD,STATUS)
      CALL IBIS_RECORD_CLOSE(RECORD_R,STATUS)
      CALL IBIS_FILE_CLOSE(IBIS,'UDELETE',STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
      CALL IBIS_FILE_CLOSE(IBIS_R,'UDELETE',STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
      CALL XVCLOSE (RUNIT1,STATUS,' ')
      CALL XVCLOSE (RUNIT2,STATUS,' ')
      RETURN
      
  666   CONTINUE  !Failure
      CALL XVMESSAGE ('PARAMETER ERROR',' ')
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create zipcol.pdf
PROCESS		HELP=*
PARM INP     TYPE=(STRING,72) COUNT=(1:2)
PARM INCOL   TYPE=INTEGER COUNT=(1:20)
PARM OUTCOL  TYPE=INTEGER COUNT=(1:30)
PARM FILE    TYPE=INTEGER COUNT=(1:50)
PARM NULL    TYPE=REAL COUNT=(1:30) DEFAULT=0
PARM NULSTR  TYPE=(STRING,80) COUNT=(1:30) DEFAULT=""
PARM MODE    TYPE=KEYWORD COUNT=(0:1) VALID=ALPHA DEFAULT=--
END-PROC
.TITLE
VICAR/IBIS Program zipcol
.HELP
PURPOSE

	ZIPCOL is used to enter data into an existing IBIS interface file.
Data can be transferred to the interface file from another interface file,
data cards, or both sources. Data is entered by matching keys between the
interface file and the data sources. Pre-sorting of the interface file and
the data sources is required. The size of the interface file is not changed
by operation of ZIPCOL.

TAE COMMAND LINE FORMAT

	zipcol INP=(A,B) INCOL=(X1,X2,X3,...XK) OUTCOL=(Y1,Y2,Y3,...YK)
		NULL=(C1,C2,...CN) FILE=(D1,D2,D3,...DK,E1,E2,...EN)

	Where A is the IBIS interface file being modified and B is a
secondary input interface file.

EXAMPLE

	zipcol INP=(INT1,INT2) INCOL=(28,27) OUTCOL=(1,2,3,4)
		FILE=(6,7,8,9,10,11) NULL=(0,0,0,0)

	In this example, columns 6, 7, from INT2 will be used to match
columns 28, 27 from INT1. If a match is made to columns 6,7 of INT2,
then columns 8, 9, 10, 11 from INT2 will be placed in columns 1, 2, 3, 4
of INT1. If no match is found zeros will be placed in columns 1, 2, 3, 4
of INT1.

.PAGE
OPERATION

	ZIPCOL operation is as follows. Each key in the first file is checked
against each key in the second. If a match is found, data is transferred from
the second file to the first file. Resultant information is written in an
update mode to the interface file being modified.

	WRITTEN BY		A. L. Zobrist		25 Aug 1981
	COGNIZANT PROGRAMMER	K. F. Evans
	DOCUMENTED BY		A. L. Zobrist
	REVISION		1			19 Aug 1982
	PORTED TO UNIX          C. R. Schenk (CRI)       6 Mar 1995

.LEVEL1
.VARIABLE INP
1. The IBIS interface file
     being modified.
2. The secondary input
     interface file.
.VARIABLE INCOL
Matching columns in first file
.VARIABLE OUTCOL
Columns in first file to 
receive data from second
.VARIABLE NULL
Values to use in no match
.VARIABLE NULSTR
NULL for alphas
.VARIABLE FILE
Matching and data columns
in second interface file
.VARIABLE MODE
'ALPHA for matching on
alphanumeric string data

.LEVEL2
.VARIABLE INP
INP number 1 is the IBIS interface file being modified.
INP number 2 is the secondary input interface file.
.VARIABLE INCOL
INCOL is used to specify columns in INP number 1, which are used as matching
keys. These keys are in columns X1 to Xk.
.VARIABLE OUTCOL
OUTCOL is used to specify columns Y1 to Yn of INT1 which will recieve data
when INCOL matches are found.
.VARIABLE NULL
NULL is used to specify what operation will take place if no match is found
for any keys in INP number 1. If NULL is not specified, the data in columns
Y1 to Yn remain unchanged if a match does not occur. If NULL is specified,
then values C1 to Cn are to be used as data values if no match is found. 
.VARIABLE NULSTR
 Same as NULL, used to input a 4 character string.
.VARIABLE FILE
Columns D1 toDk are the matching keys in the secondary input file. Column
numbers E1 to En specify the columns where data to be transferred to the
OUTCOL columns in INP number 1 are found. The number of 'D' values and 'E'
values must correspond to the number of values following INCOL and OUTCOL
respectively.
.VARIABLE MODE
Use the keyword 'ALPHA if the matching columns contain alphanumeric 
string data rather than real values.  The files must have been sorted
alphabetically with SORT.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create zipcol.imake
#define  PROGRAM   zipcol

#define MODULE_LIST zipcol.f

#define MAIN_LANG_FORTRAN
#define R2LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_MATH77
$ Return
$!#############################################################################
$Test_File:
$ create tstzipcol.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $echo="no"
let $autousage="none"

! Test general capabilities
Write "  Test REAL format capability"
   ibis-gen a nc=5 nr=10 index=1 datacol=2 +
     data=(2,4,6,8,1,3,5,7,9,0)
   ibis-copy a b 
   mf b fun=("c2=c2*(index%2)","c3=index+2","c4=index+3","c5=index+4")

! Must sort the index columns (1,2) of both files
   sort a sort=(1,2)
   sort b sort=(1,2)
   
   ibis-list b
   ibis-list a

! If cols (1,2) of A match (1,2) of B then put
! columns (3,4,5) of B into (3,4,5) of A, and
! if they don't match put (123, 456,0) into (3,4,5) of A
!
   zipcol inp=(a,b) incol=(1,2) outcol=(3,4,5) +
          file=(1,2,3,4,5) null=(123,456,789)

   ibis-list a


! Test ALPHA mode for IBIS-1
Write " "
Write "  Test for IBIS-1 ALPHA capability"
   ibis-gen a nc=5 nr=10 'ibis-1 format=(A4,A4,A4,A4) +
     strcol=(1,2,3,4) +
     string=(a,5,z,z,c,c,z,z,e,e,z,z,g,5,z,z,jj,jj,z,z,+
             a,a,z,z,c,7,z,z,e,e,z,z,g,g,z,z,jj,jj,z,z)
   ibis-gen b nc=5 nr=10 'ibis-1 format=(A4,A4,A4,A4) +
     strcol=(1,2,3,4) +
     string=(a,a,x,b,c,c,d,d,e,e,x,f,g,g,hh,hh,jj,jj,kk,kk,+
             a,a,b,b,c,c,d,d,e,e,f,f,g,x,hh,hh,jj,jx,kk,kk)

! Must sort the index columns (1,2) of both files
   sort a sort=(1,2) 'alpha
   sort b sort=(1,2) 'alpha

   ibis-list b a4col=(1,2,3,4)
   ibis-list a a4col=(1,2,3,4)

! If cols (1,2) of A match (1,2) of B then put
! columns (3,4) of B into (3,4) of A, and
! if they don't match put (not, good) into (3,4) of A
!
   zipcol inp=(a,b) incol=(1,2) outcol=(3,4) +
          file=(1,2,3,4) nulstr=(not,good) 'alpha
   ibis-list a a4col=(1,2,3,4)
!
! The following added for IBIS-2 portability testing:
!

! Test ALPHA mode for IBIS-2
Write " "
Write "  Test IBIS-2 Capability where Column widths are shorter than STRNUL"
   ibis-gen a nc=5 nr=10 'ibis-2 format=(A4,A4,A4,A4) +
     strcol=(1,2,3,4) +
     string=(a,5,z,z,c,c,z,z,e,e,z,z,g,5,z,z,jj,jj,z,z,+
             a,a,z,z,c,7,z,z,e,e,z,z,g,g,z,z,jj,jj,z,z)
   ibis-gen b nc=5 nr=10 'ibis-2 format=(A4,A4,A4,A4) +
     strcol=(1,2,3,4) +
     string=(a,a,x,b,c,c,d,d,e,e,x,f,g,g,hh,hh,jj,jj,kk,kk,+
             a,a,b,b,c,c,d,d,e,e,f,f,g,x,hh,hh,jj,jx,kk,kk)
!
! Write " IBIS-2 files a & b before sort"
!   ibis-list b 
!   ibis-list a 
!
! Must sort the index columns (1,2) of both files
   sort a sort=(1,2) 'alpha
   sort b sort=(1,2) 'alpha
!
! write " IBIS-2 files after sort"
!   ibis-list b 
!   ibis-list a
!
! If cols (1,2) of A match (1,2) of B then put
! columns (3,4) of B into (3,4) of A, and
! if they don't match put (not, good) into (3,4) of A
!
   zipcol inp=(a,b) incol=(1,2) outcol=(3,4) +
          file=(1,2,3,4) nulstr=(not,good) 'alpha
   ibis-list a a4col=(1,2,3,4)

! test IBIS-2 with wider ASCII columns
Write " "
Write "  Test IBIS-2 Capability where Column widths are wider than STRNUL"

                                                
   ibis-gen a nc=5 nr=10 'ibis-2 format=(A12,A12,A12,A12) +
     strcol=(1,2,3,4) +
     string=(a,5,z,z,c,c,z,z,e,e,z,z,g,5,z,z,jj,jj,z,z,+
             a,a,z,z,c,7,z,z,e,e,z,z,g,g,z,z,jj,jj,z,z)

   ibis-gen b nc=5 nr=10 'ibis-2 format=(A12,A12,A12,A12) +
     strcol=(1,2,3,4) +
     string=(a,a,x,b,c,c,d,d,e,e,x,f,g,g,hh,hh,jj,jj,kk,kk,+
             a,a,b,b,c,c,d,d,e,e,f,f,g,x,hh,hh,jj,jx,kk,kk)
!
!Write " IBIS-2 files a & b before sort"
!   ibis-list b
!   ibis-list a
!
! Must sort the index columns (1,2) of both files
   sort a sort=(1,2) 'alpha
   sort b sort=(1,2) 'alpha
!
! write " IBIS-2 files after sort"
!   ibis-list b
!   ibis-list a
!
! If cols (1,2) of A match (1,2) of B then put
! columns (3,4) of B into (3,4) of A, and
! if they don't match put (not, good) into (3,4) of A
!
   zipcol inp=(a,b) incol=(1,2) outcol=(3,4) +
          file=(1,2,3,4) nulstr=(not,good) 'alpha
   ibis-list a 
!
end-proc
$ Return
$!#############################################################################
