$!****************************************************************************
$!
$! Build proc for MIPL module ibis2tcl
$! VPACK Version 1.9, Wednesday, February 02, 2000, 13:07:31
$!
$! Execute by entering:		$ @ibis2tcl
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
$ write sys$output "*** module ibis2tcl ***"
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
$ write sys$output "Invalid argument given to ibis2tcl.com file -- ", primary
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
$   if F$SEARCH("ibis2tcl.imake") .nes. ""
$   then
$      vimake ibis2tcl
$      purge ibis2tcl.bld
$   else
$      if F$SEARCH("ibis2tcl.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake ibis2tcl
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @ibis2tcl.bld "STD"
$   else
$      @ibis2tcl.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create ibis2tcl.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack ibis2tcl.com -
	-s ibis2tcl.f -
	-i ibis2tcl.imake -
	-p ibis2tcl.pdf -
	-t tstibis2tcl.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ibis2tcl.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C 2 JAN 1995 ...CRI... MSTP S/W CONVERSION (VICAR PORTING)
c 
C
	INCLUDE 'VICMAIN_FOR'

	SUBROUTINE MAIN44
	IMPLICIT NONE
	INCLUDE 'pgminc' 	       ! TAE CONSTANTS & PARAMETERS
	INTEGER  VBLOCK(xprdim)
	CHARACTER*3  VARNAME
	CHARACTER*72 STRING
        CHARACTER*7  IBISVRSN
        CHARACTER*80 S2DATA
        CHARACTER*5  S1DATA
	INTEGER	IBISLOC(400), VARTYPE(200)
	INTEGER	LOCCNT, LOCDEF, VARCNT, DUMMY, CLEN, NCOL
	INTEGER	ROW, COL, UNIT, STATUS, N,IBIS, IDATA
	REAL	RDATA
        real*8  ddata
	LOGICAL IBIS2

        IBIS2=.FALSE.

        CALL IFMESSAGE('IBIS2TCL version 2-FEB-00')
        CALL XVEACTION('SA',' ')

	CALL XVPARM ('IBISLOC', IBISLOC, LOCCNT, LOCDEF, 400)
	VARCNT = LOCCNT/2
	IF (LOCDEF .EQ. 1) THEN
	   CALL XVP('VARCNT', VARCNT, DUMMY)
	ENDIF
c
c       get the variable type
c
	CALL XVP ('VARTYPE', VARTYPE, DUMMY)

C		OPEN IBIS INTERFACE FILE
        CALL XVUNIT(UNIT, 'INP', 1, STATUS, ' ')

	CALL IBIS_FILE_OPEN(UNIT,IBIS,'READ',0,0,' ',' ',STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(UNIT,STATUS,1)
        CALL IBIS_FILE_GET(IBIS,'NC',NCOL,1,1)
        CALL IBIS_FILE_GET(IBIS,'NR',CLEN,1,1)
        CALL IBIS_FILE_GET(IBIS,'VERSION',IBISVRSN,1,1)

        IF (IBISVRSN(6:6).EQ.'2') IBIS2=.TRUE.

C		CALCULATE THE DEFAULT LOCATIONS
	IF (LOCDEF .EQ. 1) THEN
	   DO N = 1, VARCNT
	      IBISLOC(2*N-1) = INT((N-1)/NCOL) + 1
	      IBISLOC(2*N) = MOD(N-1,NCOL) + 1
	   ENDDO
	ENDIF

C		CREATE V-BLOCK
	CALL XQINI( VBLOCK, xprdim, xabort)

C		PUT THE VALUES IN THE V-BLOCK ONE AT A TIME
	DO N = 1, VARCNT

	   ROW = IBISLOC(2*N-1)
	   COL = IBISLOC(2*N)

           IF (N.LE.9) THEN            !assuming n .le. 25
	      WRITE (VARNAME,'(A1,I1)') 'V',N
           ELSE
	      WRITE (VARNAME,'(A1,I2)') 'V',N
           END IF

	   IF (ROW .GE. 1  .AND.  ROW .LE. CLEN
     +		   .AND.  COL .GE. 1  .AND.  COL .LE. NCOL) THEN
C
C DOUBLE
C
	      IF (VARTYPE(N) .LT. 0) THEN
	         CALL IBIS_COLUMN_READ(IBIS,DDATA,COL,ROW,1,STATUS)
                 IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	         CALL XQDBLE(VBLOCK, VARNAME, 1, DDATA, xadd, STATUS)
C
C REAL
C
	      ELSE IF (VARTYPE(N) .EQ. 0) THEN
	         CALL IBIS_COLUMN_READ(IBIS,RDATA,COL,ROW,1,STATUS)
                 IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	         CALL XQREAL(VBLOCK, VARNAME, 1, RDATA, xadd, STATUS)
C
C INTEGER
C
	      ELSE IF (VARTYPE(N) .EQ. 1) THEN
	         CALL IBIS_COLUMN_READ(IBIS,IDATA,COL,ROW,1,STATUS)
                 IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	         CALL XQINTG (VBLOCK, VARNAME, 1,IDATA, xadd, STATUS)
C
C CHARACTERS
C
	      ELSE IF (VARTYPE(N) .EQ. 2) THEN
C
C 80 CHARACTERS
C
                 IF (IBIS2) THEN   ! up to 80 characters
	            CALL IBIS_COLUMN_READ(IBIS,S2DATA,COL,ROW,1,STATUS)
                    IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
		    CALL XQSTR (VBLOCK,VARNAME,1,S2DATA, xadd, STATUS)
C
C 4 CHARACTERS
C
                 ELSE       ! IBIS-1 4 characters only
                    CALL IBIS_COLUMN_SET(IBIS,'FORMAT','A4',COL,STATUS)
                    CALL IBIS_COLUMN_SET(IBIS,'U_FORMAT','A4',
     *                                   COL,STATUS)
	            CALL IBIS_COLUMN_READ(IBIS,S1DATA,COL,ROW,1,STATUS)
                    IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
		    CALL XQSTR(VBLOCK,VARNAME,1,S1DATA,xadd,STATUS)
                 END IF
C
C REAL TO INTEGER
C
	      ELSE
	         CALL IBIS_COLUMN_READ(IBIS,RDATA,COL,ROW,1,STATUS)
                 IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	         CALL XQINTG (VBLOCK,VARNAME,1,NINT(RDATA),xadd,STATUS)
              ENDIF
	   ELSE
	      WRITE (STRING, '(A,I5,A,I2,A)') ' Location: (',ROW,
     +		     ',', COL,')  (row,column) does not exist.'
	      CALL XVMESSAGE (STRING,' ')
	   ENDIF
	ENDDO	

	CALL IBIS_FILE_CLOSE(IBIS,' ',STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(UNIT,STATUS,1)

	CALL XQINTG (VBLOCK, 'VNCOL', 1, NCOL, xadd, STATUS)
	CALL XQINTG (VBLOCK, 'VCLEN', 1, CLEN, xadd, STATUS)

	CALL XVQOUT( VBLOCK, STATUS)

	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create ibis2tcl.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM ibis2tcl

   To Create the build file give the command:

		$ vimake ibis2tcl			(VMS)
   or
		% vimake ibis2tcl			(Unix)


************************************************************************/


#define PROGRAM	ibis2tcl
#define R2LIB

#define MODULE_LIST ibis2tcl.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define FTNINC_LIST pgminc

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create ibis2tcl.pdf
PROCESS HELP=*
  LOCAL DUMREAL TYPE=REAL
  LOCAL DUMINT  TYPE=INTEGER
  PARM INP      TYPE=(STRING,80)
  PARM V1	TYPE=NAME DEFAULT=DUMREAL
  PARM V2	TYPE=NAME DEFAULT=DUMREAL
  PARM V3	TYPE=NAME DEFAULT=DUMREAL
  PARM V4	TYPE=NAME DEFAULT=DUMREAL
  PARM V5	TYPE=NAME DEFAULT=DUMREAL
  PARM V6	TYPE=NAME DEFAULT=DUMREAL
  PARM V7	TYPE=NAME DEFAULT=DUMREAL
  PARM V8	TYPE=NAME DEFAULT=DUMREAL
  PARM V9	TYPE=NAME DEFAULT=DUMREAL
  PARM V10	TYPE=NAME DEFAULT=DUMREAL
  PARM V11	TYPE=NAME DEFAULT=DUMREAL
  PARM V12	TYPE=NAME DEFAULT=DUMREAL
  PARM V13	TYPE=NAME DEFAULT=DUMREAL
  PARM V14	TYPE=NAME DEFAULT=DUMREAL
  PARM V15	TYPE=NAME DEFAULT=DUMREAL
  PARM V16	TYPE=NAME DEFAULT=DUMREAL
  PARM V17	TYPE=NAME DEFAULT=DUMREAL
  PARM V18	TYPE=NAME DEFAULT=DUMREAL
  PARM V19	TYPE=NAME DEFAULT=DUMREAL
  PARM V20	TYPE=NAME DEFAULT=DUMREAL
  PARM V21	TYPE=NAME DEFAULT=DUMREAL
  PARM V22	TYPE=NAME DEFAULT=DUMREAL
  PARM V23	TYPE=NAME DEFAULT=DUMREAL
  PARM V24	TYPE=NAME DEFAULT=DUMREAL
  PARM V25	TYPE=NAME DEFAULT=DUMREAL
  PARM IBISLOC  TYPE=INTEGER  COUNT=0:400 DEFAULT=(1,1)
  PARM VARCNT	TYPE=INTEGER  DEFAULT=0
  PARM VARTYPE	TYPE=INTEGER  COUNT=0:200 DEFAULT=0 VALID=-1:3
  PARM VNCOL    TYPE=NAME DEFAULT=DUMINT
  PARM VCLEN    TYPE=NAME DEFAULT=DUMINT
END-PROC
.TITLE
VICAR/IBIS Program "ibis2tcl"
.HELP
PURPOSE

     "IBIS2TCL" transfers particular values specified by row and
column from an IBIS interface file to TCL variables.   The values
may be transfered in double, real, integer, or character (string) format.
The number of columns and column length may also be transfered 
to TCL variables.  Whereas the IBIS-1 strings were four characters
long, IBIS-2 character strings are allowed to be up to eighty (80)
characters long.


EXECUTION

  ibis2tcl  FILE.INT  V1=LAT V2=LONG V3=LABEL V4=COUNT   +
			IBISLOC=(1,1, 4,3, 2,2, 4,5)  VARTYPE=(0,0,2,1)

In this example, the input interface file is FILE.INT.  The real values at
(row 1 , col 1) and (row 4 , col 3) in the interface file are put into the
real TCL variables LAT and LONG, respectively.  The character data at (2,2)
is transfered to the LABEL string variable, and the integer format data
at (4,5) is transfered to the COUNT integer variable.
The TCL variable names are entered with the name parameters V1, V2, ... , V25.  
The variable names can also be entered positionally and real data types
assumed:

  ibis2tcl  FILE.INT  LAT LONG   IBISLOC=(5,6,2,1)


IBISLOC can be defaulted in which case the assumed locations start with the
first row and column and continue with all the elements in each row for as
many rows as required to get VARCNT elements:

  ibis2tcl  FILE.INT  A B C D E F  VARCNT=6

The number of values put in TCL variables is equal to the number of locations
specified by IBISLOC or is specified by VARCNT if IBISLOC is defaulted;
there should be enough variables listed to accommodate that many.  If the
locations are defaulted then VARCNT must be specified.


The number of columns and/or the length of the columns in the interface
file can be transfered to TCL integer variables.  Values in the interface
file can be transfered as well.

  ibis2tcl FILE.INT  LABEL  IBISLOC=(6,1) VARTYPE=2  VNCOL=NCOL  VCLEN=CLEN
  ibis2tcl FILE.INT  VCLEN=CLEN




The following procedure is an example of its use:

PROCEDURE
  LOCAL LAT   TYPE=REAL
  LOCAL LONG  TYPE=REAL
  LOCAL LABEL TYPE=(STRING,4)  !4 for IBIS-1, up to 80 for IBIS-2
  LOCAL CLEN  TYPE=INTEGER
BODY
  ibis2tcl  FILE.INT  LAT LONG LABEL    IBISLOC=(1,1,4,3,2,2)    +
					VARTYPE=(0,0,2)  VCLEN=CLEN
  write "LAT = &LAT    LONG = &LONG "
  write "LABEL = &LABEL"
  write "COLUMN LENGTH = &CLEN"
END-PROC





RESTRICTIONS

The TCL variable types must match the type specified with the
    VARTYPE parameter.
The TCL variables for the number of columns and the column length 
    must be of integer type.
The maximum number of TCL variables allowed per execution is 25.
The interface file cannot have more than 1,000,000 rows or 200 columns.



 WRITTEN BY:		K.F. Evans	June 1985

 COGNIZANT PROGRAMMER:  B.A. McGuffie

 REVISION:	3		April 1986
                4 AMS  (CRI)    Jan.  1995  Made portable for UNIX
                                            and added IBIS-2 capabilities
                5 BAM           June  1998  Increased columns; corrected pdf.
                6 BAM           Dec.  1998  Allowed for double precision.

.LEVEL1
.VARIABLE INP
 Input interface file
.VARIABLE V1
The first TCL variable name, etc
.VARIABLE IBISLOC
The IBIS rows and columns 
.VARIABLE VARCNT
The number of variables
.VARIABLE VARTYPE
The type for each variable
-1 for double precision,
0 for real, 
1 for integer,
2 for character string,
3 for real to integer conversion
.VARIABLE VNCOL
The TCL variable name
to get the number of columns
.VARIABLE VCLEN
The TCL variable name
to get the column length

.LEVEL2
.VARIABLE INP
   INP specifies the IBIS interface (tabular) file that has the data to
be transfered.
.VARIABLE V1
   The Vn parameters are TCL NAME parameters that contain the names of
the TCL variables to receive the data, e.g.  V1=LATITUDE  V2=LABEL .
The variables must have been declared in TCL before running this program,
and must have types corresponding to those specified with the VARTYPE
parameter.  The maximum number of variables that can be specified is
25, i.e. V25 is the last one.
.VARIABLE IBISLOC
    IBISLOC specifies the (row,column) locations in the interface file
of the data to be transfered.  The first pair of numbers are the row and 
column of the first value to be transfered, the second pair are for the
second value, etc.  If this parameter is defaulted then the VARCNT parameter
is used and the default locations are used (see help).
.VARIABLE VARCNT
    VARCNT is only used if IBISLOC is not specified so the default locations
are used.  VARCNT specifies the number of values to be transfered.
.VARIABLE VARTYPE
    The VARTYPE parameter specifies the data type of each value to be
transfered:  -1 for double precision, 
              0 for real, 
              1 for integer, 
              2 for character string,
              3 for real value to be rounded for output to an integer.

If VARTYPE is defaulted then all real types will be assumed.
.VARIABLE VNCOL
    VNCOL is the TCL name parameter that specifies the TCL variable
to receive the number of columns in the IBIS interface file.
.VARIABLE VCLEN
    VCLEN is the TCL name parameter that specifies the TCL variable
to receive the column length of the IBIS interface file.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstibis2tcl.pdf
procedure
  refgbl $echo
  refgbl $autousage
  LOCAL LAT  TYPE=REAL
  LOCAL LONG  TYPE=REAL
  LOCAL INTL TYPE=INTEGER
  LOCAL STR  TYPE=(STRING,80)
  LOCAL NCOL INTEGER
  LOCAL CLEN INTEGER
body
  let $autousage="none"
  let _onfail="continue"
  let $echo="yes"
  ibis-gen  FILE.INT NC=3 NR=2 DATA=(1,2,3,4) DATACOLS=(1,2) +
            FORMAT=(REAL,REAL,A4) STRCOL=3 STRING=(HELP,MEEE) 'IBIS-1
  ibis-list FILE.INT A4COL=3
  ibis2tcl  FILE.INT V1=LAT V2=LONG V3=STR IBISLOC=(1,1,2,2,1,3) VNCOL=NCOL +
            VCLEN=CLEN VARTYPE=(0,0,2)
  write "LAT = &LAT"
  write "LONG = &LONG"
  write "STR = &STR"
  write "NCOL = &NCOL"
  write "CLEN = &CLEN"
  ibis-gen  FILE.INT NC=3 NR=2 DATA=(5,6,7,8) DATACOLS=(1,2) +
            FORMAT=(REAL,FULL,A6) STRCOL=3 STRING=(HELPME,PLEASE)
  ibis-list FILE.INT
  ibis2tcl  FILE.INT V1=LAT V2=INTL V3=STR IBISLOC=(1,1,2,2,2,3) VNCOL=NCOL +
            VCLEN=CLEN VARTYPE=(0,1,2)
  write "LAT = &LAT"
  write "LONG = &INTL"
  write "STR = &STR"
  write "NCOL = &NCOL"
  write "CLEN = &CLEN"
  ibis-gen  A NC=3 NR=2 DATA=(1,2,3,4) DATACOLS=(1,2) +
            FORMAT=(DOUB,REAL,A4) STRCOL=3 STRING=(HELP,MEEE)
  MFD       A  OUTCOL=1  'SET  VALUE=123456789.123456789
  MF        A  FUNC="C2=123456789.123456789"
  ibis-list A A4COL=3 CFORM="%20.9f %20.9f %5.5s" 'format
  ibis2tcl A V1=LAT V2=LONG V3=STR +
  IBISLOC=(1,1,2,2,1,3) VNCOL=NCOL VCLEN=CLEN VARTYPE=(-1,0,2)
  write "LAT = &LAT"
  write "LONG = &LONG"
  write "STR = &STR"
  write "NCOL = &NCOL"
  write "CLEN = &CLEN"
END-PROC 

END-PROC
$ Return
$!#############################################################################
