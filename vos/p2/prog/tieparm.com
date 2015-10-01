$!****************************************************************************
$!
$! Build proc for MIPL module tieparm
$! VPACK Version 1.8, Wednesday, May 17, 1995, 09:45:04
$!
$! Execute by entering:		$ @tieparm
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
$ write sys$output "*** module tieparm ***"
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
$ write sys$output "Invalid argument given to tieparm.com file -- ", primary
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
$   if F$SEARCH("tieparm.imake") .nes. ""
$   then
$      vimake tieparm
$      purge tieparm.bld
$   else
$      if F$SEARCH("tieparm.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake tieparm
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @tieparm.bld "STD"
$   else
$      @tieparm.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create tieparm.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack tieparm.com -
	-s tieparm.f -
	-i tieparm.imake -
	-p tieparm.pdf -
	-t tsttieparm.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create tieparm.f
$ DECK/DOLLARS="$ VOKAGLEVE"

C
C		program: "TIEPARM"
C		author: Leo M. Bynum
C		date written: Feb 14, 1985
C
C		purpose: To take an IBIS interface file containing
C			tiepoint coordinates as input and output a
C			parameter file of these same coordinates to
C			be used as input for geometric correction
C			programs. (TOIBIS mode transforms tiepoint
C                       coordinates in a parameter file into an
C                       IBIS interface file.)
C
C		Revisions:
C			1	Put in delta Z tiepoints (3 columns)
C				K. F. Evans	April 1986
C
C                       2       Added TOIBIS and PARMS parameters to allow
C                               conversion from parameter file to IBIS 
C                               interface file.
C                               Steve Pohorsky  June 1986
C
C			3	Corrected TOIBIS mode to be independent
C				of NAH and NAV - for none gridded tiepoints.
C				Frank Evans   March 1987
C
C			4	Ported to UNIX.
C				Randy Schenk (CRI) May 1995
C
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44

	IMPLICIT INTEGER (A-Z)
	LOGICAL	XVPTST
	INTEGER	COLS(5)
	REAL*4	DATA(5)
	REAL 	TIEPOINT(5*40000)
	CHARACTER*72  OUTFILE

C=======================================================

      CALL IFMESSAGE('TIEPARM Version 8-May-95')
      IF ( .NOT. XVPTST( 'TOIBIS' )  )   THEN

C......................................START OF REGULAR (TOPARM) MODE SECTION.
C		open input file
C
	CALL XVUNIT( UNIT,'INP', 1, STATUS,' ')
	IF (STATUS.NE.1) THEN
	   CALL XVMESSAGE('INPUT FILE NOT FOUND by XVUNIT',' ')
	   CALL ABEND
	ENDIF
	CALL XVPARM ('COLS', COLS, COLCOUNT, DEF, 5)
	CALL IBIS_FILE_OPEN(UNIT,IBIS,'READ',0,0,' ',' ',STATUS)
	IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	CALL IBIS_FILE_GET(IBIS,'NR',CLEN,1,1)
	CALL IBIS_FILE_GET(IBIS,'NC',NCOL,1,1)
C
C		open output file
C
	CALL XVP ('OUT', OUTFILE, COUNT)
	CALL XVP ('NAV', NAV,     COUNT)
	CALL XVP ('NAH', NAH,     COUNT)
	CALL XVPOPEN(STATUS,3,4*COLCOUNT*CLEN + 8,OUTFILE,'U',OUNIT)
	IF (STATUS .NE.1) THEN
	  CALL XVMESSAGE('OUTPUT FILE CANNOT BE OPENED',' ')
	  CALL ABEND
	ENDIF
C
C		read in the column numbers to be used for
C		newline, newsample, oldline, and oldsample values.
C		 or line, sample, and delta Z values.
C
C
C		read and store tiepoint coordinates
C
	TINDX = 1
	CALL IBIS_RECORD_OPEN(IBIS,RECORD,' ',
     +                        COLS,COLCOUNT,'REAL',STATUS)
	IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	DO ROW = 1, CLEN
C	  CALL GETREC(IUNIT, COLCOUNT, COLS, DATA, ROW, CLEN, A)
	  CALL IBIS_RECORD_READ(RECORD,DATA,ROW,STATUS)

	  IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	  TIEPOINT(TINDX) = DATA(1)
	  TIEPOINT(TINDX + 1) = DATA(2)
	  TIEPOINT(TINDX + 2) = DATA(3)
	  TIEPOINT(TINDX + 3) = DATA(4)
	  TIEPOINT(TINDX + 4) = DATA(5)
	  TINDX = TINDX + COLCOUNT
	ENDDO

	CALL IBIS_FILE_CLOSE(IBIS,' ',STATUS)
	IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
C
C		output tiepoint coordinates
C
	CALL XVPOUT(STATUS, 'NAV', NAV, 'INT', 1)
	CALL XVPOUT(STATUS, 'NAH', NAH, 'INT', 1)
	CALL XVPOUT(STATUS, 'TIEPOINT', TIEPOINT, 'REAL', CLEN*COLCOUNT)
C
	CALL XVPCLOSE(STATUS)

C......................................END OF REGULAR (TOPARM) MODE SECTION.

      ELSE

C......................................START OF TOIBIS MODE SECTION.

C..PROCESS PARAMETERS

        CALL XVPARM( 'COLS', COLS, COLCOUNT, IDEF, 5 )
        MAXCOL = COLS(1)
        DO I = 2, COLCOUNT
            MAXCOL = MAX( MAXCOL, COLS(I) )
        END DO

        CALL XVPARM( 'TIEPOINT', TIEPOINT, NTIE, IDEF,(5*40000))
        NTIE = NTIE / COLCOUNT

        CALL XVPARM( 'NCOL', NCOL, COUNT, IDEF, COLCOUNT)
	IF (IDEF .EQ. 1)  NCOL = MAXCOL

        CALL XVPCNT( 'INP', NI )         ! NUMBER OF INP AND OUT FILES.
        CALL XVPCNT( 'OUT', NO )
        IF ( (NI .NE. 0 .OR. NO .NE. 1)  .AND.
     .       (NI .NE. 1 .OR. NO .NE. 0)  )
     .       CALL MABEND( ' ERROR:INCORRECT NUMBER OF FILES SPECIFIED.')

C..OPEN IBIS INTERFACE FILE. USE UPDATE MODE IF FILE ALREADY EXISTS.

        IF (NO .EQ. 1)  THEN
	  CLEN = NTIE
C            CALL WRFIL( OUNIT, 1, CLEN, NCOL, NOFILE)
	  CALL XVUNIT( IFUNIT,'OUT', 1, STATUS,' ')
	  IF (STATUS.NE.1) THEN
	    CALL XVMESSAGE('OUTPUT FILE NOT FOUND by XVUNIT',' ')
	    CALL ABEND
	  ENDIF
	  CALL IBIS_FILE_OPEN(IFUNIT,IBIS,'WRITE',NCOL,CLEN,
     +                          ' ',' ',STATUS)
        ELSE                         ! UPDATE the INP file
C            CALL RDFIL( OUNIT, 1, CLEN, NCOL, NOFILE)
	  CALL XVUNIT( IFUNIT,'INP', 1, STATUS,' ')
	  IF (STATUS.NE.1) THEN
	    CALL XVMESSAGE('OUTPUT FILE NOT FOUND by XVUNIT',' ')
	    CALL ABEND
	  ENDIF
	  CALL IBIS_FILE_OPEN(IFUNIT,IBIS,'UPDATE',0,0,' ',' ',STATUS)
	  IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	  CALL IBIS_FILE_GET(IBIS,'NC',NCOL,1,1)
	  CALL IBIS_FILE_GET(IBIS,'NR',CLEN,1,1)
          IF ( NCOL .LT. MAXCOL .OR. CLEN .NE. NTIE) CALL MABEND(
     .      '  ERROR: PARAMS DO NOT AGREE WITH SIZE OF IBIS FILE')
        END IF

C..WRITE DATA TO IBIS INTERFACE FILE.

        TINDX = 1
	CALL IBIS_RECORD_OPEN(IBIS,RECORD,'FORMAT:REAL',
     +                        COLS,COLCOUNT,'REAL',STATUS)
	IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)

       DO I = 1, CLEN
           ROW = I
C           CALL PUTREC(OUNIT,COLCOUNT,COLS,TIEPOINT(TINDX),ROW,CLEN,A)
	    CALL IBIS_RECORD_WRITE(RECORD,TIEPOINT(TINDX),ROW,STATUS)
	    IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
           TINDX = TINDX+ COLCOUNT
        END DO
	CALL IBIS_FILE_CLOSE(IBIS,' ',STATUS)
	IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
      END IF

C......................................END OF TOIBIS MODE SECTION.

      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create tieparm.imake
#define  PROGRAM   tieparm

#define MODULE_LIST tieparm.f

#define MAIN_LANG_FORTRAN
#define R2LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_MATH77
$ Return
$!#############################################################################
$PDF_File:
$ create tieparm.pdf
PROCESS help=*
  PARM INP     TYPE=(STRING,40) COUNT=0:1  DEFAULT=--
  PARM OUT     TYPE=(STRING,40) COUNT=0:1  DEFAULT=--
  PARM MODE    TYPE=KEYWORD     COUNT=1    DEFAULT=TOPARM VALID=(TOPARM,TOIBIS)
  PARM COLS    TYPE=INTEGER COUNT=3:5 DEFAULT=(1,2,3,4)
  PARM NCOL    TYPE=INTEGER COUNT=0:1 DEFAULT=--
  PARM MINL    TYPE=REAL    COUNT=1 DEFAULT=1
  PARM MAXL    TYPE=REAL    COUNT=1 DEFAULT=512
  PARM MINS    TYPE=REAL    COUNT=1 DEFAULT=1
  PARM MAXS    TYPE=REAL    COUNT=1 DEFAULT=512
  PARM NAV     TYPE=INTEGER COUNT=1 DEFAULT=20
  PARM NAH     TYPE=INTEGER COUNT=1 DEFAULT=20
  PARM TIEPOINT TYPE=REAL   COUNT=0:600 DEFAULT=--
  PARM PARMS   TYPE=STRING  COUNT=0:1  DEFAULT=--
END-PROC
.TITLE
VICAR/IBIS Program tieparm
.HELP
PURPOSE

    tieparm has two modes, the default mode (TOPARM) and the TOIBIS mode.
In default mode tieparm takes tiepoints in an IBIS interface (tabular) file
and outputs them in a parameter dataset form for TIECONM
and the geometrical correction programs.  Geometrical tiepoints
(4 values), delta Z (3 values) tiepoints, or geometrical tiepoints with
delta Zs (5 values) may be transfered.
    In TOIBIS mode, tieparm converts a tiepoint parameter dataset into an
IBIS interface (tabular) file.
.page
EXECUTION

  TOPARM is the default mode.  In default mode, there is an IBIS interface file
as input and a parameter dataset as output.  

  In  TOIBIS mode there is an input parameter data set specified via the PARMS 
parameter (unless NAH, NAV, and TIEPOINT are specified on the command line.)
There is also an output IBIS interface file.  The IBIS interface file is
specified using the OUT parameter if a new file is to be written.  It is
specified using the INP parameter if an existing file is to be written into.

tieparm can be used to produce a parameter data set for program MZGEOM.
This is illustrated in one of the following examples.
.PAGE
 EXAMPLES: 

tieparm INP=TIEPOINTS.INT OUT=TIEPOINTS.PDS  NAH=20 NAV=15 COLS=(1,2,5,6)

This example, which uses all of the parameters in default (TOPARM) mode
will read the tiepoints (new line, new sample, old line, old sample) from
columns 1, 2, 5, and 6 from the interface file.  The default
columns are 1, 2, 3, and 4.


tieparm INP=TIEPOINTS.INT OUT=TIEPOINTS.PDS  NAH=12 NAV=10 COLS=(1,2,4)

This example shows how to transfer DN values tiepoints for TIECONM and
GEOMZ.  The tiepoints are in the form (line, sample, delta Z).
.PAGE
tieparm  PARMS=TIEM.PAR  OUT=TIEM.INT NCOL=7 'TOIBIS
tieparm  PARMS=TIEG.PAR  INP=TIEM.INT COLS=(5,6,7) 'TOIBIS
tieparm  INP=TIEM.INT  OUT=TIEMZ.PAR NAH=1 NAV=1 COLS=(1,2,3,4,7) 'TOPARM

In this example tieparm is used to combine a parameter dataset for MGEOM
(TIEM.PAR) and a parameter dataset for GEOMZ (TIEG.PAR) to produce  a parameter
dataset for MZGEOM (TIEMZ.PAR).  The tiepoint locations in the parameter
dataset for GEOMZ must match the new line and sample tiepoint locations in the
parameter dataset for MGEOM. 
.page
RESTRICTIONS

    The maximum number of tiepoints is 40,000.



Original Programmer:     Leo Bynum         February 1985

Ported to UNIX by:	 Randy Schenk (CRI) May 1995

Cognizant Programmer:    Frank Evans


.LEVEL1
.VARI INP
IBIS interface file name (input)
.VARI OUT
Parameter data set name (output)
or IBIS interface file name for
TOIBIS mode.
.VARI MODE
Enter 'TOPARM to convert an IBIS
interface file to a parameter
dataset.  Enter 'TOIBIS to 
convert a parameter dataset to an
IBIS interface file.
.VARI COLS
Columns of the interface file
to be used for newline, newsample,
oldline, and oldsample;
or line, sample, delta Z;
or newline, newsample,
oldline, oldsample, and delta Z.
.VARI NCOL
Number of columns in IBIS 
interface file (TOIBIS mode).
.VARI NAV
Number of areas vertical.
Default is 20.
.VARI NAH
Number of areas horizontal.
Default is 20.
.VARI TIEPOINT
Tiepoint coordinates.
.VARI PARMS
optional parameter file.
.VARI MINL
Dummy parameter
.VARI MAXL
Dummy parameter
.VARI MINS
Dummy parameter
.VARI MAXS
Dummy parameter

.LEVEL2
.VARI INP
Input file name.  This parameter is input as:
     INP=innam
where "innam" is the input file name.
.VARI OUT
Output file name. This parameter is input as:
     OUT=outnam
where:
"outnam" is the output file name.
.VARI MODE
  TOPARM is the default mode.  In default mode, there is an IBIS interface file
as input and a parameter dataset as output.  

  In  TOIBIS mode there is an input parameter data set specified via the PARMS 
parameter (unless NAH, NAV, and TIEPOINT are specified on the command line.)
There is also an output IBIS interface file.  The IBIS interface file is
specified using the OUT parameter if a new file is to be written.  It is
specified using the INP parameter if an existing file is to be written into.
.VARI COLS
The default columns in default (TOPARM) mode are 1,2,3,4.
The default columns in  TOIBIS mode are 1,2,3,4
.VARI NCOL
NCOL specifies the number of columns to be contained in a new IBIS interface
file produced in TOIBIS mode.  The default for NCOL is the highest column
number specified for the COLS parameter.  
.VARI NAH
The nah is number of grid cells horizontally, the number of tiepoints 
across is one larger (nah+1).  NAH must be an integer.  The default value is 20.
.VARI NAV
The nav is number of grid cells vertically, the number of tiepoints
vertically is one larger (nav+1).  NAV must be an integer.  The default value 
is 20.
.VARI TIEPOINT
The TIEPOINT keyword and associated numbers specify the mapping of control
points between output and input pictures.  The numbers which follow this
keyword are in groups of 3, 4, or 5, one group for each tiepoint.  The numbers
may be either integer or real.  The total number of tiepoint numbers must
be  3,4,or 5 times the desired number of tiepoints.
The order in which the tiepoints are specified is left to right within
a horizontal row of tiepoints.  The horizontal rows are ordered from top
to bottom.
.VARI PARMS
A parameter data set containing the tiepoint parameters.  This file should
have been written by a program which uses the XVP routines for writing
parameter data sets, such as TIECONM.
.end
$ Return
$!#############################################################################
$Test_File:
$ create tsttieparm.pdf
procedure
refgbl $echo
refgbl $autousage
body
let _onfail="continue"
let $autousage="none"
let $echo="yes"

!  TEST PDF FOR tieparm

!  CHECK BASIC OPERATION AND CHECK THAT TOPARM AND TOIBIS MODES ARE INVERSES.

tieparm OUT=tie1.int 'TOIBIS NAH=3 NAV=2       +
 tiepoint=( 3,2,10,5  3,4,10,10  3,7,10,15  3,9,10,20  +        
            5,2,15,5  5,4,15,10  5,7,15,15  5,9,15,20  +
            8,2,20,5  8,4,20,10  8,7,20,15  8,9,20,20  )
ibis-list tie1.int
tieparm INP=tie1.int OUT=tie1.par NAH=3 NAV=2 
tieparm PARMS=tie1.par OUT=tie2.int   'TOIBIS
ibis-list tie2.int

!  try setting up for mzgeom using input parameter files from tieconm

tieconm OUT= tie3.par 'MGEOM NAH=1 NAV=1       +
   tiepoint=( 1,1 1,1     1,25 1,13   +
              25,1 13,1   25,25 13,13 )
tieconm OUT= tie4.par 'GEOMZ NAH=1 NAV=1       +
   tiepoint=( 1,1,0     1,25,0    +
              25,1,240  25,25,240 )
tieparm  PARMS=tie3.par  OUT=tiem.int NCOL=7 'TOIBIS
tieparm  PARMS=tie4.par  INP=tiem.int COLS=(5,6,7) 'TOIBIS
tieparm  INP=tiem.int  OUT=tiemz.par NAH=1 NAV=1 COLS=(1,2,3,4,7) 'TOPARM

ibis-list tiem.int

! Following test is commented out until mzgeom is made portable
!                  (mzgeom has approx. 160 lines of assembly code)
! gen OUT=tie5 NL=13 NS=13 LINC=2 SINC=2
! LIST tie5
! mzgeom tie5 (tie5z,IDS) SIZE=(1,1,25,25) PARMS=tiemz.par
! LIST tie5z

! CLEAN UP

!DCL DELETE tie*.par;*,tie*.int;*,tie*.z*.*
end-proc
$ Return
$!#############################################################################
