$!****************************************************************************
$!
$! Build proc for MIPL module geomz
$! VPACK Version 1.8, Monday, May 08, 1995, 08:46:36
$!
$! Execute by entering:		$ @geomz
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
$ write sys$output "*** module geomz ***"
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
$ write sys$output "Invalid argument given to geomz.com file -- ", primary
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
$   if F$SEARCH("geomz.imake") .nes. ""
$   then
$      vimake geomz
$      purge geomz.bld
$   else
$      if F$SEARCH("geomz.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake geomz
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @geomz.bld "STD"
$   else
$      @geomz.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create geomz.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack geomz.com -
	-s geomz.f -
	-i geomz.imake -
	-p geomz.pdf -
	-t tstgeomz.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create geomz.f
$ DECK/DOLLARS="$ VOKAGLEVE"
       PROGRAM  geomz
C#######################################################################
C  NAME OF ROUTINE
C      geomz ( GEOM Z-values )
C
C  PURPOSE
C      THIS IS THE STANDARD MAIN PROGRAM USED FOR TAE/VICAR PROGRAMS.
C      THIS MODULE CALLS SUBROUTINE MAIN44 TO ENTER INTO THE BODY OF THE
C      PROGRAM.
C      Program "geomz" is a VICAR applications program which is used to 
C      perform rubber sheet vertical correction of images by a set of
C      points specified as parameters or in a parameter data set.
C  PREPARED FOR USE ON MIPL SYSTEM BY
C      STEVE POHORSKY   STERLING SOFTWARE/INFORMATICS     APRIL 1986
C  FOR
C      MIPL SOFTWARE DEVELOPMENT
C
C  ORIGINAL "geomz" PROGRAM BY
C      H. WILCZYNSKI
C
C  ENVIRONMENT
C      VAX 11       VMS  with TAE/VICAR2 EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C     14 FEB 80   ...ALZ...   REPLACE CALL END BY RETURN
C     24 AUG 78    ...ALZ...   INITIAL RELEASE
C     4-86  SP   CONVERTED FROM IBM VICAR VERSION.
C     4-86  SP   REWROTE ENTIRE COMPUTATIONAL SECTION BECAUSE OF ERRORS AND
C                BECAUSE REQUESTOR WANTED CALL TO DGELG SUBROUTINE REMOVED.
C                THE IBM VERSION ALWAYS USED THE VALUE FROM THE INPUT 
C                PARAMETERS AS THE INITIAL VAL FOR A CELL OF THE IMAGE.
C                BILINEAR INTERPOLATION SHOULD BE USED INSTEAD SINCE THE 
C                FIRST PIXEL IN THE CELL MIGHT NOT BE AT THE TIEPOINT.
C                THIS IS THE CASE WHEN THE FOUR CORNERS OF THE IMAGE ARE NOT
C                TIEPOINTS.  THE IBM CODE COULD NOT HANDLE THE CASE WHERE
C                THE STARTING LINE AND SAMPLE DID NOT BELONG TO THE UPPER-LEFT
C                CELL, WHICH SHOULD BE THEORETICALLY ALLOWABLE. THE NEW
C                COMPUTATIONAL SECTION DOES NOT REQUIRE THAT THE SPACING
C                OF TIEPOINTS BE UNIFORM.  THE CHECK FOR UNIFORMITY WAS LEFT
C                IN TO LEAVE THE PROGRAM FUNCTIONALLY THE SAME.
C     5-86  SP   CHANGED ROUNDING TO USE NINT(VAL(IAH)) INSTEAD OF VAL(IAH)
C                IN THE FINAL ADD TO WORK(J).
C     5-95  VRU  ... CRI ... MSTP S/W CONVERSION (VICAR PORTING)
C  USER PARAMETERS:
C       IN,       INPUT IMAGE FILE 
C       OUT,      OUTPUT IMAGE (Z CORRECTION APPLIED)
C       NAH=N     THE INTEGER N DENOTES THE NUMBER OF AREAS HORIZONTALLY IN THE
C                 TIEPOINT GRID.
C       NAV=M     THE INTEGER M DENOTES THE NUMBER OF AREAS VERTICALLY IN THE
C                 TIEPOINT GRID.
C       TIEPOINT, INPUT GRID
C                 X1,Y1,DZ1,...,XN,YN,DZN   
C                 THE NUMBERS XI,YI,DZI SPECIFY THE POINTS OF
C                 A UNIFORM RECTANGULAR GRID. THE NUMBERS MAY BE REAL OR 
C                 INTEGER.  THE XI AND YI SPECIFY THE PIXEL LOCATION OF 
C                 THE TIEPOINT AND THE DZI SPECIFY THE DN SHIFT.
C      FIXVAL=V   THE INTEGER V INDICATES THAT ALL PIXELS OF VALUE V ARE TO
C                 BE UNCHANGED.
C  PROGRAM LIMITATIONS
C      SEE HLP FILE.
C  SUBROUTINES CALLED
C      MAIN44
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44 
C#######################################################################
C  NAME OF ROUTINE
C     MAIN44 (name for top level subroutine by VICAR convention)
C
C  PURPOSE
C      MAIN44 performs rubber sheet vertical correction of images by a set
C      points specified as parameters or in a parameter data set.
C  CONVERTED FOR USE ON MIPL SYSTEM BY   
C      STEVE POHORSKY   STERLING SOFTWARE/INFORMATICS     APRIL 1986
C  FOR
C      MIPL SOFTWARE DEVELOPMENT
C  ENVIRONMENT
C      VAX 11       VMS  with TAE/VICAR EXECUTIVE       FORTRAN-77
C  CALLING SEQUENCE
C      Standard subroutine call and return.
C  CALLED BY
C      "geomz"
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT INTEGER(A-Z)

      PARAMETER (DIM_PAR = 120000)         ! MAX OF 120,000/3 TIEPOINTS

      COMMON /COM/COEFF(200),PAR(DIM_PAR),WORK(60000)
     +,VAL(200),INCL(200),INCS(200),LVAL(200)
      REAL*4 LINES(3001), SAMPS(201)
      REAL VAL,INCL,INCS,LVAL
      REAL EPSX,EPSY, VALA, VALB, F1, F2, F3, F4
      INTEGER*2 WORK
      REAL  COEFF,RPAR(DIM_PAR),DXX,DX,DYY,DY
      CHARACTER*8 FORMAT
      DATA HALF/0/,IMAX/255/

C
C======================START OF EXECUTABLE CODE======================

      CALL IFMESSAGE('GEOMZ version 2-JAN-95')
      CALL XVEACTION('SA',' ')
C..OPEN INPUT FILE

      CALL XVUNIT(IUNIT,'INP',1,ISTAT, ' ')
      IRETURN = 1
      CALL XVSIGNAL(IUNIT, ISTAT, IRETURN)
      CALL XVOPEN(IUNIT,ISTAT, 'OP', 'READ', 'U_FORMAT','HALF', ' ')

      CALL XVSIZE( STLINE, STSAMP, NLINE, NSAMP, NLI, NSI)
      CALL XVGET(IUNIT,ISTAT,'FORMAT',FORMAT, ' ')
      IF(FORMAT.NE.'BYTE')   HALF = 1
      IF (HALF.EQ.1)         IMAX = 32767

C..GET PARAMETERS

      CALL XVP('NAH',NAH,COUNT)
      CALL XVP('NAV',NAV,COUNT)
      IF ( NAH .EQ. 0  .OR.  NAV .EQ. 0)   GOTO 999
      NAH1 = NAH+1
      NAV1 = NAV+1
      IF (NAH1*NAV1 .GT. DIM_PAR/3)
     &   CALL MABEND('ERROR: NAH*NAV TOO BIG.')

      CALL XVPARM('TIEPOINT',RPAR,COUNT,IDEF, DIM_PAR)
      IF (COUNT.NE.NAH1*NAV1*3)
     &   CALL MABEND('NUMBER OF TIEPOINTS DOES NOT MATCH NAH/NAV')
      PTR = 1

      CALL XVPARM( 'FIXVAL', FIXVAL, ICOUNT, IDEF, 1 )
      IF (ICOUNT .EQ. 0)  FIXVAL = 99999

C..OPEN THE OUTPUT FILE.

      CALL XVUNIT(OUNIT,'OUT',1,ISTAT, ' ')
      IRETURN = 1
      CALL XVSIGNAL(OUNIT, ISTAT, IRETURN)
      CALL XVOPEN(OUNIT, ISTAT, 'OP', 'WRITE', 'U_NL',NLINE,
     .        'U_NS',NSAMP, 'U_FORMAT','HALF', ' ' )

      DYY = RPAR(NAH1*3+PTR)-RPAR(PTR)
      DXX = RPAR(PTR+4)-RPAR(PTR+1)
      EPSX = DXX/1000.
      EPSY = DYY/1000.

C..MAKE SURE THE SPACING IS UNIFORM.

      DO 10 I=1,NAV1
      DO 20 J=1,NAH1
      IF(I.EQ.NAV1)GOTO 15
      IF(ABS(RPAR(NAH1*3+PTR)-RPAR(PTR)-DYY).GE.EPSY)GOTO 998
   15 IF(J.EQ.NAH1)GOTO 16
      IF(ABS(RPAR(PTR+4)-RPAR(PTR+1)-DXX).GE.EPSX) GOTO 998
   16 PTR = PTR+3
   20 CONTINUE
   10 CONTINUE

      ELINE = STLINE + NLINE - 1
      ESAMP = STSAMP + NSAMP - 1

C..STORE THE LINE AND SAMPLE NUMBERS FOR THE ROWS AND COLUMNS OF THE TIEPOINT
C..GRID.

      DO IAV = 1, NAV1
         LINES(IAV) = RPAR(1 + 3*NAH1* (IAV-1) )
      END DO

      DO IAH = 1, NAH1
         SAMPS(IAH) = RPAR(2 + 3* (IAH-1) )
      END DO

C..FIND THE GRID CELLS THAT CONTAIN (STLINE,STSAMP) AND (ELINE,ESAMP).
C..USUALLY THIS WILL BE THE FIRST AND THE LAST, AND WE WILL END UP WITH
C..   IAVBEG=IAHBEG=1  AND IAVEND=NAV AND IAHEND=NAH.

      IAVBEG = 2
      DO WHILE( LINES(IAVBEG) .LE. STLINE .AND. IAVBEG .LT. NAV1 )
         IAVBEG = IAVBEG+1
      END DO
      IAVBEG = IAVBEG-1

      IAVEND = NAV
      DO WHILE( LINES(IAVEND) .GE. ELINE .AND. IAVEND .GT. 1)
         IAVEND = IAVEND-1
      END DO

      IAHBEG = 2
      DO WHILE( SAMPS(IAHBEG) .LE. STSAMP .AND. IAHBEG .LT. NAH1 )
         IAHBEG = IAHBEG+1
      END DO
      IAHBEG = IAHBEG-1

      IAHEND = NAH
      DO WHILE( SAMPS(IAHEND) .GE. ESAMP .AND. IAHEND .GT. 1)
         IAHEND = IAHEND-1
      END DO

C..THIS IS NOT QUITE AN OUTER LINE LOOP WITH AN INNER SAMPLE LOOP.
C..WE LOOP OVER THE ROWS OF GRID CELLS. FOR EACH ROW WE COMPUTE THE 
C..TRANSFORMATIONS TO BE APPLIED AND THEN TRANSFORM THE LINES OF THE CELLS.
C..THIS PROCESSES THE LINES OF THE IMAGE IN CONSECUTIVE ORDER.

      DO IAV = IAVBEG, IAVEND
         IF (IAV .EQ. IAVBEG)  THEN
            LBEG = STLINE           ! BEGINNING AND ENDING LINES FOR THIS ROW.
         ELSE 
            LBEG = LEND+1
         END IF

         IF (IAV .EQ. IAVEND)  THEN
            LEND = ELINE
         ELSE 
            LEND = LINES(IAV+1)
         END IF

         DO IAH = IAHBEG, IAHEND
           IF (IAH .EQ. IAHBEG)  THEN
              SBEG = STSAMP        ! BEGINNING AND ENDING SAMPS FOR THIS COLUMN.
           ELSE 
              SBEG = SEND+1
           END IF

           IF (IAH .EQ. IAHEND)  THEN
              SEND = ESAMP
           ELSE 
              SEND = SAMPS(IAH+1)
           END IF

           DX = SAMPS(IAH+1) - SAMPS(IAH)
           DY = LINES(IAV+1) - LINES(IAV)

           PTR1 = 3 + 3*(IAH-1) + 3*NAH1* (IAV-1)

C..COMPUTE VAL(IAH), THE FIRST DZ VALUE FOR THIS CELL, BY USING BILINEAR
C..INTERPOLATION FROM THE VALUES (F1,F2,F3,F4) FOR THE FOUR TIEPOINTS FOR
C..THE CELL.

           F1 = RPAR( PTR1 )
           F2 = RPAR( PTR1 +3)
           F3 = RPAR( PTR1 +3*NAH1)
           F4 = RPAR( PTR1 +3*NAH1+3)

           VALA = F1 + (F2-F1) * (SBEG-SAMPS(IAH) ) / DX
           VALB = F3 + (F4-F3) * (SBEG-SAMPS(IAH) ) / DX

           VAL(IAH) = VALA + (VALB-VALA)* (LBEG-LINES(IAV))/DY

C..THE DZ VALUES FOR THE REST OF THE CELL WILL BE COMPUTED BY ADJUSTING
C..THE INITIAL VALUE BY A LINE INCREMENT (INCL) AT THE START OF EACH LINE
C..OF THE CELL AND ADJUSTING THE VALUE BY A SAMPLE INCREMENT (INCS) WITHIN
C..EACH LINE OF THE CELL.

           LVAL(IAH) = VAL(IAH)
           INCL(IAH) = (VALB-VALA) / DY
           COEFF(IAH) = ( F4-F2+F1-F3 ) / (DY*DX)
           INCS(IAH) = (F2-F1)/DX  +  COEFF(IAH) * ( LBEG-LINES(IAV) )

         END DO

C..NOW THE LINE LOOP FOR THIS ROW.

         DO LINE = LBEG,LEND

           IF ( LINE .EQ. STLINE )   THEN
             CALL XVREAD( IUNIT, WORK, ISTAT, 'LINE', STLINE, ' ' )
           ELSE
             CALL XVREAD( IUNIT, WORK, ISTAT, ' ' )
           END IF

C..LOOP THROUGH THE CELLS FOR THIS LINE.

           DO IAH = IAHBEG, IAHEND
             IF (IAH .EQ. IAHBEG)  THEN
                SBEG = STSAMP      ! BEGINNING AND ENDING SAMPS FOR THIS COLUMN.
             ELSE 
                SBEG = SEND+1
             END IF

             IF (IAH .EQ. IAHEND)  THEN
                SEND = ESAMP
             ELSE 
                SEND = SAMPS(IAH+1)
             END IF

C..LOOP THROUGH THE SAMPLES FOR EACH CELL.

             DO J = SBEG, SEND
                IF (WORK(J) .NE. FIXVAL)  THEN
                   DZ = NINT( VAL(IAH) )
                   IMVAL = WORK(J)+DZ
                   IF(IMVAL .LT. 0)IMVAL = 0
                   IF(IMVAL .GT. IMAX)IMVAL = IMAX
                   WORK(J) = IMVAL
                END IF
                VAL(IAH) = VAL(IAH) + INCS(IAH)
             END DO

C..SET UP FOR NEXT LINE OF THIS CELL.

             INCS(IAH) = INCS(IAH) + COEFF(IAH)
             VAL(IAH)  = LVAL(IAH) + INCL(IAH)
             LVAL(IAH) = VAL(IAH)

           END DO

           CALL XVWRIT(OUNIT, WORK(STSAMP), ISTAT, ' ')

        END DO
      END DO

      CALL XVCLOSE( IUNIT, ISTAT, ' ' )
      CALL XVCLOSE( OUNIT, ISTAT, ' ' )
      RETURN

  998 CALL XVMESSAGE('TIEPOINTS NOT EVENLY SPACED', ' ')
      CALL ABEND
  999 CALL XVMESSAGE('PARM ERROR', ' ')
      CALL ABEND
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create geomz.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM geomz

   To Create the build file give the command:

		$ vimake geomz			(VMS)
   or
		% vimake geomz			(Unix)


************************************************************************/


#define PROGRAM	geomz
#define R2LIB

#define MODULE_LIST geomz.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create geomz.pdf
process help=*
 !
 PARM INP     TYPE=STRING COUNT=1
 PARM OUT     TYPE=STRING COUNT=1
 PARM SIZE    TYPE=INTEGER COUNT=0:4 DEFAULT=(1,1,0,0)
 PARM SL      TYPE=INTEGER COUNT=0:1 DEFAULT=1
 PARM SS      TYPE=INTEGER COUNT=0:1 DEFAULT=1
 PARM NL      TYPE=INTEGER COUNT=0:1 DEFAULT=0
 PARM NS      TYPE=INTEGER COUNT=0:1 DEFAULT=0
 PARM NAH TYPE=INTEGER,VALID=(1:200) default=1
 PARM NAV TYPE=INTEGER,VALID=(1:3000) default=1
 PARM TIEPOINT TYPE=REAL,COUNT=0:600 default=--
 PARM FIXVAL  TYPE= INTEGER COUNT=(0:1) DEFAULT=--
 PARM PARMS   TYPE=STRING  COUNT=0:1  DEFAULT=--
 !
 END-PROC
!
! HELP TEXT FOR "geomz"
.TITLE
VICAR PROGRAM "geomz"
.HELP
 PURPOSE:

Program "geomz" is a VICAR applications program which is used to 
perform vertical (brightness or DN) correction of images by a set of
points specified as parameters or in a parameter data set.
"geomz" does not perform a geometric transformation (of line and sample 
coordinates) of the image.
.PAGE
 EXECUTION:

"geomz" is designed to be an analog of program "GEOMA".  Its application is
for vertical (brightness or DN) manipulations of images.  Vertical corrections
are specified by the user through tiepoints either as parameters or in a 
parameter file.  (Program "TIECONM" can produce a parameter file for "geomz".)
The "GEOMA" execution format has been imitated as closely as possible.

The size of the output image is determined by the number of lines and number 
of samples in the SIZE field if the SIZE field is entered by the user.  If the
SIZE field is not entered, the output file is the same size as the input file.

The data type of the input image may either be byte or halfword data.
The data type is obtained from the VICAR label of the input image.
The output image has the same data format  (byte or halfword) as the input 
image.  
.PAGE
TAE COMMAND LINE FORMAT
      The following command line formats show the major allowable forms:
      geomz INP=a OUT=b SIZE=(sl,ss,nl,ns) parameters
      geomz INP=a OUT=b  SL=sl SS=ss NL=nl NS=ns parameters
      geomz a b (sl,ss,nl,ns) parameters
      geomz a b parameters
      geomz INP=a OUT=b SIZE=(sl,ss,nl,ns) PARMS=c optional parameters
      geomz INP=a OUT=b  SL=sl SS=ss NL=nl NS=ns PARMS=c optional parameters
      geomz a b (sl,ss,nl,ns) PARMS=c optional parameters
      geomz a b PARMS=c optional parameters

       Here 'a' represents the input image file name,
       'b' represents the output image file name,
       and 'c' represents the parameter file name.
.page
The tiepoint grid must be specified as as set of parameters either in the
TAE command line or in the parameter file.
The tiepoint grid is specified using the parameters NAH, NAV, and TIEPOINT.

   NAH=nah  NAV=nav TIEPOINT=( L1,S1,DZ1  L2,S2,DZ2 ... )

nah is the number of grid cells horizontally (across the top in the sample
direction), nav is the number of grid cells vertically. The grid must be
evenly-spaced and perfectly rectangular and the rows and columns must be
perfectly horizontal and vertical respectively. 
.PAGE
EXAMPLE

       geomz INP=A OUT=B SIZE=(1,1,500,1000)  NAH=1 NAV=1  FIXVAL=0   +
             TIEPOINT=(   1,1,10      1,1000, -10 +
                        500,1,10    500,1000, -10   )

This example cause the input to be changed by a ramp function with the
leftmost pixels becoming 10 DN brighter and the rightmost pixels becoming
10 DN dimmer.  Pixels with a 0 value in A are not changed because of the
FIXVAL parameter.
.PAGE
RESTRICTIONS
1. The input and output images must be byte or halfword data.
2. The maximum number of pixels per line is 60,000.
3. The maximum number of tiepoints is 40,000.  (If parameters are used 
   instead of a parameter file, the limit is 200.)
4. The maximum value for NAH is 200.
5. The maximum value for NAV is 3000
.page
 OPERATION:

The input picture is transformed pixel by pixel to the output picture by
adjusting the DN value according to nearby tiepoints.  A four (NAH) by 
three (NAV) tiepoint grid is illustrated below.  The tiepoints are marked 
with a '+' character.  The lines show how areas are defined by the grid.
A pixel within an area is adjusted according to the four tiepoints that
define the area containing that pixel.  Usually, this is a rectangle 
containing the pixel; but if the point lies outside the convex hull of the
grid, then the nearest rectangle is used for adjustment.
.page

                     |      |      |      
              +      +      +      +      +  
                     |      |      |      
        ------+------+------+------+------+------
                     |      |      |      
        ------+------+------+------+------+------
                     |      |      |      
              +      +      +      +      +  
                     |      |      |      

.page
The DN adjustment is obtained by bilinear interpolation (or extrapolation)
of the adjustment at the four grid points.  The formula is

     DN (output) = DN (input) + ax + by + cxy + d

where x and y are the pixel coordinates and a,b,c,d are constants chosen
so that 

    delta DN(k) = ax(k) + by(k) + cx(k)y(k) + d          for k = 1,2,3,4,

where x(k) and y(k) are the coordinate values and delta DN(k) are the DN
shifts of the four selected tiepoints for the point x,y.

The calculation is performed by a double incrementing scheme.  For a single
line, the DN increment per pixel is given by  b+cx.  The starting values for
subsequent lines are incremented by  a+cy.  Thus the full solution is needed
only at the upper left corner of an area, and all other values are obtained
by incrementing.

After the delta-DN (DZ) values are added, the resulting DNs are then checked
for being valid for the data type (byte or halfword) of the image and are
adjusted if invalid.  For byte data, DNs less than 0 are set to 0, and DNs
greater than 255 are set to 255.  For halfword data, DNs less than 0 are set to
0, and DNs greater than 32767 are set to 32767. 

WRITTEN BY:                 Steve Pohorsky                1 May 1986

Original Programmer:        H. Wilczynski                 1 Sep 1977

Cognizant Programmer:       Steve Pohorsky                1 May 1986

Made portable for UNIX      VRU  (CRI)                    8 May 1995
.LEVEL1
.VARI INP
Input file name
.VARI OUT
Output file name
.VARI SIZE
Standard VICAR Size Field
.VARI SL
Starting line for output
.VARI SS
Starting sample for output
.VARI NL
Number of lines for output
.VARI NS
Number of samples for output
.VARI NAH
number of grid
cells horizontally
.VARI NAV
number of grid
cells vertically
.VARI TIEPOINT
grid corner tiepoints in
rows L1,S1,DZ1,L2,...
.VARI FIXVAL
a single DN value (optional)
to be left unchanged by the
DN transformation.
.VARI PARMS
optional parameter file
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
.VARI SIZE
The size field is specified with four arguments,
     SIZE=(a,b,c,d)
where:
a is the starting line number of the output picture.
b is the starting sample of the output picture.
c is the number of lines, and
d is the number of samples
For example, SIZE=(1,1,40,50)
would create an output picture of size 40 lines by 50 pixels.
The size field can be thought of as a window relative to the output
grid.  The first two values offset the window down and to the right
causing the features in the image to move up and to the left.
.VARI SL
SL can be used to specify the starting line of the output picture.
This is actually a coordinate relative to the output grid, therefore,
it offsets the output picture by (SL - 1.)  The default for SL is 1.
.VARI SS
SS can be used to specify the starting sample of the output picture.
This is actually a coordinate relative to the output grid, therefore,
it offsets the output picture by (SS - 1.)  The default for SS is 1.
.VARI NL
NL can be used in conjunction with NS in place of the SIZE
parameter to specify the size of the output picture.  It simply
represents the number of lines for output.
.VARI NS
NS can be used in conjunction with NS in place of the SIZE
parameter to specify the size of the output picture.  It simply
represents the number of samples (pixels) for output.
.VARI NAH
the nah is number of grid cells horizontally, the number of tiepoints 
across is one larger (nah+1).  NAH must be an integer.  The default value is 1.
.VARI NAV
the nav is number of grid cells vertically, the number of tiepoints
vertically is one larger (nav+1).  NAV must be an integer.  The default value 
is 1.
.VARI TIEPOINT
The TIEPOINT keyword and associated numbers specify the mapping of control
points between output and input pictures.  The numbers which follow this
keyword are in groups of three, one group for each tiepoint.  The numbers
may be either integer or real.  The total number of tiepoint numbers must
be  3*(n+1)*(m+1), where n and m are given by the NAH and NAV parameters.
Within each group of three numbers describing a tiepoint, the first two
numbers give the line and sample coordinates of the tiepoint in the input
picture.  (The SIZE field, SS, SL have no effect on this location.)
The third value (DZ) specifies the amount of (vertical) shift in the DN
value at the tiepoint.  A positive value indicates an upward DN shift
(brighter), and a negative value indicates a downward DN shift (darker).
The order in which the tiepoints are specified is left to right within
a horizontal row of tiepoints.  The horizontal rows are ordered from top
to bottom.  A major difference between "geomz" and GEOMA formats is that
"geomz" tiepoints must be in an evenly-spaced rectangular grid.
.VARI FIXVAL
The default is to transform all DN values. FIXVAL=0 is commonly used to preserve
the outer background of an image so that mosaicking with THESH=0 can be used.
.VARI PARMS
A parameter data set containing the tiepoint parameters.  This file should
have been written by a program which uses the XVP routines for writing
parameter data sets, such as "TIECONM".
$ Return
$!#############################################################################
$Test_File:
$ create tstgeomz.pdf
procedure
refgbl $echo
refgbl $syschar
refgbl $autousage
body
let $autousage="none" 
let _onfail="continue"
let $echo="yes"
!THIS IS A TEST FILE FOR "geomz"
! first work with byte data

!THIS TAKES A 10 X 10 IMAGE AND ADDS 11 TO EACH PIXEL.

gen tgen1 10 10 linc=5 sinc=10
geomz inp=tgen1 out=tgen2 nah=1 nav=1 tiepoint=( 1,1,11  1,10,11 +
      10,1,11  10,10,11)
list tgen1
list tgen2
difpic (tgen2,tgen1) tgend 'MOD
list tgend

! test using size field and fixval

geomz inp=tgen1 out=tgen2 nah=1 nav=1 tiepoint=( 1,1,11  1,10,11 +
      10,1,11  10,10,11)  size = (2,3,5,6)  fixval=50
list tgen2

! try out with another grid.

gen tgen3 nl=25 ns=25 ival=100 linc=0 sinc=0
geomz inp=tgen3 out=tgen4 nah=3 nav=2       +
 tiepoint=( 10,5,0  10,10,10.7  10,15,-4.4  10,20,0  +        
            15,5,1  15,10,11.7  15,15,-3.4  15,20,1  +
            20,5,2  20,10,12.7  20,15,-2.4  20,20,2  )
list tgen4

!  apply inverse transformation and see if we get what we started with.

geomz inp=tgen4 out=tgen5 nah=3 nav=2       +
 tiepoint=( 10,5,0   10,10,-10.7  10,15,4.4  10,20,0  +        
            15,5,-1  15,10,-11.7  15,15,3.4  15,20,-1  +
            20,5,-2  20,10,-12.7  20,15,2.4  20,20,-2  )
list tgen5

!  try using an input parameter file from "tieconm"

gen OUT=tgen6 NL=25 NS=25 IVAL=0 LINC=0 SINC=0
tieconm OUT=tgen7 'GEOMZ  NAH=2 NAV=4 +
   tiepoint=( 1   1    0 +
               25  1    0 +
               1  25    0 +
               25 25    0 +
               13 13   255)
geomz INP=tgen6 PARMS=tgen7 OUT=tgen8
list tgen8 'ZERO

! now try halfword data

!THIS TAKES A 10 X 10 IMAGE AND ADDS 11 TO EACH PIXEL.

gen tgen1 10 10 linc=5 sinc=10 'half
geomz inp=tgen1 out=tgen2 nah=1 nav=1 tiepoint=( 1,1,11  1,10,11 +
      10,1,11  10,10,11)
list tgen1
list tgen2
difpic (tgen2,tgen1) tgend 'MOD
list tgend

! test using size field and fixval

geomz inp=tgen1 out=tgen2 nah=1 nav=1 tiepoint=( 1,1,11  1,10,11 +
      10,1,11  10,10,11)  size = (2,3,5,6)  fixval=50
list tgen2

! try out with another grid.

gen tgen3 nl=25 ns=25 ival=100 linc=0 sinc=0 'half
geomz inp=tgen3 out=tgen4 nah=3 nav=2       +
 tiepoint=( 10,5,0  10,10,10.7  10,15,-4.4  10,20,0  +        
            15,5,1  15,10,11.7  15,15,-3.4  15,20,1  +
            20,5,2  20,10,12.7  20,15,-2.4  20,20,2  )
list tgen4

!  apply inverse transformation and see if we get what we started with.

geomz inp=tgen4 out=tgen5 nah=3 nav=2       +
 tiepoint=( 10,5,0   10,10,-10.7  10,15,4.4  10,20,0  +        
            15,5,-1  15,10,-11.7  15,15,3.4  15,20,-1  +
            20,5,-2  20,10,-12.7  20,15,2.4  20,20,-2  )
list tgen5

!  try using an input parameter file from "tieconm"
gen OUT=tgen6 NL=20 NS=20 IVAL=0 LINC=0 SINC=0 'HALF
tieconm OUT=tgen7 'GEOMZ  NAH=2 NAV=4 +
   tiepoint=( 1   1    0 +
               25  1    0 +
               1  25    0 +
               25 25    0 +
               13 13   255)
geomz INP=tgen6 PARMS=tgen7 OUT=tgen8
list tgen8 'ZERO

! Clean up and exit
if ($syschar(1) = "VAX_VMS")
  DCL DEL TGEN%.Z*.*
else
  ush rm tgen*
end-if

end-proc
$ Return
$!#############################################################################
