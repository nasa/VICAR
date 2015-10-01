$!****************************************************************************
$!
$! Build proc for MIPL module pixgrad
$! VPACK Version 1.9, Wednesday, March 10, 2010, 12:25:08
$!
$! Execute by entering:		$ @pixgrad
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
$ write sys$output "*** module pixgrad ***"
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
$ write sys$output "Invalid argument given to pixgrad.com file -- ", primary
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
$   if F$SEARCH("pixgrad.imake") .nes. ""
$   then
$      vimake pixgrad
$      purge pixgrad.bld
$   else
$      if F$SEARCH("pixgrad.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake pixgrad
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @pixgrad.bld "STD"
$   else
$      @pixgrad.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create pixgrad.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack pixgrad.com -mixed -
	-s pixgrad.f -
	-i pixgrad.imake -
	-p pixgrad.pdf -
	-t tstpixgrad.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create pixgrad.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C***************************************************************************
C    PIXGRAD calculates the magnitude of the gradiend of a picture.
C    The absolute value of the difference between the current pixel
C    and its eight adjacent neighbors is calculated.  For the current
C    output pixel location, the largest difference is produced on the
C    primary ourput data set; the secondary output data set will
C    contain the index of the largest difference.  The differences 
C    are labelled 1 through 8 counterclockwise starting with the
C    difference of the current pixel (I,J) and the neighbor on the
C    left (I,J-1).
C
C    VARIABLE INP
C             Input filename 
C    VARIABLE OUT
C             Output filenames - one for dip and the 
C             other for azimuth. Byte data output.
C    VARIABLE SIZE
C             Vicar size field.
C             -----------------
C             Starting line          default - input image
C             Starting sample
C             Number of lines
C             Number of samples
C    VARIABLE NORTH
C             Direction of North in degrees clockwise from 
C                           top of image.
C             Default = 0.0
C    VARIABLE ADIP
C             Scale factor for slope.
C             DN(DIP) = (ADIP * DIP) + BDIP
C             where
C             adip - scale factor
C             dip  - computed dip in degrees
C             bdip - offset factor ( see BDIP )
C             Default = 255./90.
C    VARIABLE BDIP
C             Offset factor for slope.
C             DN(DIP) = (ADIP * DIP) + BDIP
C             where
C             adip - scale factor ( see ADIP )
C             dip  - computed dip in degrees
C             bdip - offset factor 
C             Default = 0.0
C    VARIABLE AAZ
C             Scale factor for azimuth.
C             DN(AZIM) = (AAZ * AZIMUTH) + BAZ
C             where
C             aaz      - scale factor 
C             azimuth  - computed azimuth in degrees from North
C             baz      - offset factor ( see BAZ )
C             Default = 256./360.
C    VARIABLE BAZ
C             Offset factor for azimuth.
C             DN(AZIM) = (AAZ * AZIMUTH) + BAZ
C             where
C             aaz      - scale factor ( see AAZ )
C             azimuth  - computed azimuth in degrees from North
C             baz      - offset factor 
C             Default = 0.0
C    VARIABLE LSCALE
C             Line scale in meters per line.
C             Default = 1.0
C    VARIABLE SSCALE
C             Sample scale in meters per sample.
C             Default = 1.0
C    VARIABLE AALT
C             Altitude scale in meters per dn of input.
C             Default = 1.0 meter/dn
C***************************************************************************
C
C     10-10-88  ...SP...   CHANGED TO IGNORE FORMAT PARAMETER BECAUSE VICAR2 
C                          USES ONLY THE FORMAT IN LABEL.
C     08-05-94   (CRI)     MSTP S/W Conversion (VICAR porting) 
C     11-08-94  ..REA...   Changed default scalings, I/O cleanup
C
C    A = INPUT ELEVATION IMAGE
C    B = GRADIENT MAGNITUDE IMAGE (DIP)      BYTE
C    C = GRADIENT DIRECTION IMAGE (AZIMUTH)  BYTE
C
C    SIZE FIELD REFERS TO THE INPUT IMAGE
C
C    PARAMETER  VARIABLE  FORMAT  DEFAULT   DESCRIPTION
C    NORTH      NORTH     FLOAT   0.        DIR OF NRTH DEG CLKWS FRM UP
C    ADIP       ADIP      FLOAT   255./90.  DN(DIP) = ADIP*(DIP+BDIP)
C    BDIP       BDIP      FLOAT   0.        
C    AAZ        AAZ       FLOAT   256./360. DN(AZIM)=AAZ*(AZIMUTH+BAZ)
C    BAZ        BAZ       FLOAT   0.        
C    LSCALE     LSCALE    FLOAT   1.0       LINE SCALE, IN METERS/LINE
C    SSCALE     SSCALE    FLOAT   1.0       SAMPLE SCALE, IN METERS/SAMPLE
C    AALT       AALT      FLOAT   1.0 METERS/DN ALTITUDE=AALT*DN(ALT)
C***************************************************************************
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44

      IMPLICIT NONE

      INCLUDE 'fortport'
      REAL*4  ALT(10002,3)                ! altitude array 
      BYTE  DIP(10000)                    ! gradient magnitude array
      BYTE  AZIM(10000)                   ! gradient direction array

      CHARACTER*72 LBL

      REAL*4  NORTH                  ! DIR OF NRTH, DEG CLKWS FRM UP
      REAL*4  ADIP                   ! DN(DIP) = (ADIP*DIP)+BDIP
      REAL*4  BDIP                   ! DN(DIP) = (ADIP*DIP)+BDIP
      REAL*4  AAZ                    ! DN(AZIM)=(AAZ*AZIMUTH)+BAZ
      REAL*4  BAZ                    ! DN(AZIM)=(AAZ*AZIMUTH)+BAZ
      REAL*4  LSCALE                 ! LINE SCALE, IN MILES/LINE
      REAL*4  SSCALE                 ! SAMPLE SCALE, IN MILES/SAMPLE
      REAL*4  AALT                   ! ALTITUDE=AALT*DN(ALT)+BALT
      REAL*4  PI/3.1415927/          ! pi

      INTEGER*4 SL                   ! starting line
      INTEGER*4 SS                   ! starting sample
      INTEGER*4 NL                   ! number of lines output
      INTEGER*4 NS                   ! number of samples output
      INTEGER*4 NLIN                 ! number of lines input
      INTEGER*4 NSIN                 ! number of samples input
      INTEGER*4 ICOUNT,LAST,NOW,NEXT ! temp variables
      INTEGER*4 LINE,ISAMP,LOC,N     ! temp variables
      REAL*4    LFAC,SFAC,TWOPI      ! temp variables
      REAL*4    X,Y,Z,XDIP,XAZIM     ! temp variables
C
      INTEGER*4 INUNIT               ! unit of input data set
      INTEGER*4 OUTUNIT(2)           ! 2 units for output data sets
      INTEGER*4 STATUS               ! status of xv returns
C
      CALL XVEACTION ('SA',' ')
      CALL XVMESSAGE('PIXGRAD version 09-NOV-94',' ')
C							open datasets
C
      CALL XVUNIT ( INUNIT, 'INP', 1, STATUS, ' ' ) ! get unit for input data
      CALL XVOPEN  ( INUNIT, STATUS, 'U_FORMAT','REAL', ' ')      
      CALL XVUNIT(OUTUNIT(1), 'OUT', 1, STATUS,' ') ! first output data set
      CALL XVOPEN(OUTUNIT(1), STATUS, 'U_FORMAT','BYTE',
     +            'O_FORMAT','BYTE','OP','WRITE',' ')  
      CALL XVUNIT(OUTUNIT(2), 'OUT', 2, STATUS,' ') ! second output data set
      CALL XVOPEN(OUTUNIT(2), STATUS, 'U_FORMAT','BYTE',
     +            'O_FORMAT','BYTE','OP','WRITE',' ')  
      CALL XVSIZE ( SL, SS, NL, NS, NLIN, NSIN )
      IF ( SL + NL .GT. 1 + NLIN ) THEN
	  CALL XVMESSAGE(
     +       '$$$ Requested size field exceeds input image length $$$',
     +	     ' ')
	  CALL ABEND
      ENDIF
      IF ( SS + NS .GT. 1 + NSIN ) THEN
	  CALL XVMESSAGE(
     +         '$$$ Requested size field exceeds input image width $$$',
     +	       ' ')
	  CALL ABEND
      ENDIF
C
C								get parameters
      CALL XVP ( 'NORTH',  NORTH,  ICOUNT )
      CALL XVP ( 'ADIP',   ADIP,   ICOUNT )
      CALL XVP ( 'BDIP',   BDIP,   ICOUNT )
      CALL XVP ( 'AAZ',    AAZ,    ICOUNT )
      CALL XVP ( 'BAZ',    BAZ,    ICOUNT )
      CALL XVP ( 'LSCALE', LSCALE, ICOUNT )
      CALL XVP ( 'SSCALE', SSCALE, ICOUNT )
      CALL XVP ( 'AALT',   AALT,   ICOUNT )
C								print parameters
      WRITE (LBL,100) AALT
  100 FORMAT ('          Altitude Image...    ALT = ',F8.3,
     +        ' * DN meters')
      CALL XVMESSAGE(LBL,' ')
      WRITE (LBL,110) LSCALE
  110 FORMAT ('          Line scale is  ',F10.3,' meters/pixel')
      CALL XVMESSAGE(LBL,' ')
      WRITE (LBL,120) SSCALE
  120 FORMAT ('          Sample scale is',F10.3,' meters/pixel')
      CALL XVMESSAGE(LBL,' ')
      WRITE (LBL,130) ADIP,BDIP
  130 FORMAT ('Gradient Magnitude Image...     DN = ',F8.3,
     +        ' * DIP(deg) ',SP,F8.3)
      CALL XVMESSAGE(LBL,' ')
      WRITE (LBL,140) AAZ,BAZ
  140 FORMAT ('Gradient Direction Image...     DN = ',F8.3,
     +        ' * AZIMUTH(deg) ',SP,F8.3)
      CALL XVMESSAGE(LBL,' ')
C								update labels
      CALL XLADD(OUTUNIT(1),'HISTORY','IMAGE','Gradient Magnitude',
     +           STATUS,'FORMAT','STRING',' ')
      WRITE (LBL,150) ADIP,BDIP
  150 FORMAT (F8.3,' * DIP(deg) ',SP,F8.3)
      CALL XLADD(OUTUNIT(1),'HISTORY','DN',LBL,STATUS,'FORMAT','STRING',
     +           ' ')
C
      CALL XLADD(OUTUNIT(2),'HISTORY','IMAGE','Gradient Direction',
     +           STATUS,'FORMAT','STRING',' ')
      WRITE (LBL,160) AAZ,BAZ
  160 FORMAT (F8.3,' * AZIMUTH(deg) ',SP,F8.3)
      CALL XLADD(OUTUNIT(2),'HISTORY','DN',LBL,STATUS,'FORMAT','STRING',
     +           ' ')
C					change linear scaling factors to pixels
C					change angular factors to include
C					conversion from radians to degrees.
      LFAC = 0.5 * AALT / LSCALE
      SFAC = 0.5 * AALT / SSCALE
      ADIP = ADIP * 180.0 / PI
      AAZ  = AAZ  * 180.0 / PI
      TWOPI = 2.0 * PI
      NORTH = TWOPI - (NORTH*PI/180.0)
C				fill in initial values into the altitude buffer
      CALL XVREAD(INUNIT,ALT(2,1),STATUS,'LINE',SL,'SAMP',SS,
     +            'NSAMPS',NS,' ')
      ALT(1,1) = ALT(2,1)
      ALT(NS+2,1) = ALT(NS+1,1)
      CALL MVE(7,NS+2,ALT(1,1),ALT(1,2),1,1)
C
      LAST = 1				! index to previous line ALT values
      NOW = 2				! index to this line ALT values
      NEXT = 3				! index to next line ALT values
C
C					loop through each line of the image
      DO LINE = SL+1,SL+NL-1
          CALL XVREAD(INUNIT,ALT(2,NEXT),STATUS,'LINE',LINE,'SAMP',SS,
     +                'NSAMPS',NS,' ')
          ALT(1,NEXT) = ALT(2,NEXT)
          ALT(NS+2,NEXT) = ALT(NS+1,NEXT)
C								sample loop
          DO ISAMP = 1,NS
              LOC = ISAMP + 1
              X = SFAC * (ALT(LOC+1,NOW) - ALT(LOC-1,NOW))
              Y = LFAC * (ALT(LOC,NEXT) - ALT(LOC,LAST))
              Z = X*X + Y*Y
              IF (Z .EQ. 0.0) THEN
                  XDIP = BDIP
                  XAZIM = BAZ
              ELSE
                  XDIP = ADIP * ATAN(SQRT(Z)) + BDIP
                  XAZIM = AAZ * AMOD(ATAN2(-X,Y)+NORTH,TWOPI) + BAZ
              END IF
              N = MAX(0,MIN(255,NINT(XDIP)))
              DIP(ISAMP) = INT2BYTE(N)
              N = MAX(0,MIN(255,NINT(XAZIM)))
              AZIM(ISAMP) = INT2BYTE(N)
          END DO
          CALL XVWRIT(OUTUNIT(1),DIP,STATUS,'NSAMPS',NS,' ')
          CALL XVWRIT(OUTUNIT(2),AZIM,STATUS,'NSAMPS',NS,' ')
C						     increment ALT array indices
          LOC = LAST
          LAST = NOW
          NOW = NEXT
          NEXT = LOC
      END DO
C						    last line remains to be done
      CALL MVE(7,NS+2,ALT(1,NOW),ALT(1,NEXT),1,1)
      DO ISAMP = 1,NS
          LOC = ISAMP + 1
          X = SFAC * (ALT(LOC+1,NOW) - ALT(LOC-1,NOW))
          Y = LFAC * (ALT(LOC,NEXT) - ALT(LOC,LAST))
          Z = X*X + Y*Y
          IF (Z .EQ. 0.0) THEN
              XDIP = BDIP
              XAZIM = BAZ
          ELSE
              XDIP = ADIP * ATAN(SQRT(Z)) + BDIP
              XAZIM = AAZ * AMOD(ATAN2(-X,Y)+NORTH,TWOPI) + BAZ
          END IF
          N = MAX(0,MIN(255,NINT(XDIP)))
          DIP(ISAMP) = INT2BYTE(N)
          N = MAX(0,MIN(255,NINT(XAZIM)))
          AZIM(ISAMP) = INT2BYTE(N)
      END DO
      CALL XVWRIT(OUTUNIT(1),DIP,STATUS,'NSAMPS',NS,' ')
      CALL XVWRIT(OUTUNIT(2),AZIM,STATUS,'NSAMPS',NS,' ')
C								close data sets
C
      CALL XVCLOSE(INUNIT, STATUS,' ')
      CALL XVCLOSE(OUTUNIT(1), STATUS,' ')
      CALL XVCLOSE(OUTUNIT(2), STATUS,' ')
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create pixgrad.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM pixgrad

   To Create the build file give the command:

		$ vimake pixgrad			(VMS)
   or
		% vimake pixgrad			(Unix)


************************************************************************/


#define PROGRAM	pixgrad
#define R2LIB

#define MODULE_LIST pixgrad.f

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
$ create pixgrad.pdf
process help=*
!  Input and output file names
!
PARM INP     TYPE=(STRING,60)   COUNT=1
PARM OUT     TYPE=(STRING,60)   COUNT=2
!
!  Input parameters
!
PARM SIZE    TYPE=INTEGER  COUNT=4       DEFAULT=(1,1,0,0)
!
PARM LSCALE  TYPE=REAL     COUNT=1       DEFAULT=1.0
PARM SSCALE  TYPE=REAL     COUNT=1       DEFAULT=1.0
PARM AALT    TYPE=REAL     COUNT=1       DEFAULT=1.0
PARM ADIP    TYPE=REAL     COUNT=1       DEFAULT=2.8333
PARM BDIP    TYPE=REAL     COUNT=1       DEFAULT=0.0
PARM AAZ     TYPE=REAL     COUNT=1       DEFAULT=0.7111
PARM BAZ     TYPE=REAL     COUNT=1       DEFAULT=0.0
PARM NORTH   TYPE=REAL     COUNT=1       DEFAULT=0.0
END-PROC
.TITLE
VICAR2 Program PIXGRAD
.HELP
PURPOSE
-------

PIXGRAD calculates the magnitude (slope) and direction (azimuth)
of the gradient at each pixel within an image.  The image of slope
values is saved in the first output dataset, and the azimuth image
is sent to the second output.

The gradients are computed by examining the four adjacent pixels 
(left, right, above, below) to the point being evaluated.  From these
pairs of points, a line-direction and a sample-direction gradient is
computed. The composite slope and azimuth are computed by a
trigonometric summation of these two gradients.

Prior to rescaling, the slope is computed as degrees from horizontal,
while the azimuth is degrees East of North.  The default rescaling
maps the slope values to 0 DN for horizontal and 255 DN for vertical.
The default azimuth rescaling places North at 0 DN, East at 64 DN,
South at 128 DN, and West at 192 DN.

EXECUTION
---------
Execution is initiated by:

pixgrad INP (OUT1,OUT2) PARAMETERS


LIMITATIONS
-----------

Input image must be no more than 10,000 samples wide.
.LEVEL1
.VARIABLE INP
Input filename.
.VARIABLE OUT
Output filenames.
(1) dip image
(2) azimuth image
.VARIABLE SIZE
Vicar size field.
.VARIABLE NORTH
Direction of North in
degrees clockwise from up.
.VARIABLE ADIP
Scale factor for slope.
.VARIABLE BDIP
Offset factor for slope.
.VARIABLE AAZ
Scale factor for azimuth.
.VARIABLE BAZ
Offset factor for azimuth.
.VARIABLE LSCALE
Line scale in 
meters per line.
.VARIABLE SSCALE
Sample scale in
meters per sample.
.VARIABLE AALT
Altitude scale in meters
per DN of input.
.LEVEL2
.VARIABLE INP
Filename of the input elevation dataset.
.VARIABLE OUT
Filenames of the output datasets. The first dataset will contain the 
gradient magnitude (aka dip, or slope) image.  The second dataset will 
contain the gradient azimuth (aka slope azimuth, or slope direction) image.
.VARIABLE SIZE
             Vicar size field.
             -----------------
             Starting line          default - input image
             Starting sample
             Number of lines
             Number of samples
.VARIABLE NORTH
Direction of North in degrees clockwise from top of image.
             Default = 0.0
.VARIABLE ADIP
             Scale factor for slope.

             DN(DIP) = (ADIP * DIP) + BDIP
             where
             adip - scale factor
             dip  - computed dip in degrees
             bdip - offset factor

             Default ADIP = 255./90.
                     BDIP = 0.0
.VARIABLE BDIP
             Offset factor for slope.

             DN(DIP) = (ADIP * DIP) + BDIP
             where
             adip - scale factor
             dip  - computed dip in degrees
             bdip - offset factor

             Default ADIP = 255./90.
                     BDIP = 0.0
.VARIABLE AAZ
             Scale factor for azimuth.
 
             DN(AZIM) = (AAZ * AZIMUTH) + BAZ
             where
             aaz      - scale factor 
             azimuth  - computed azimuth in degrees from North
             baz      - offset factor

             Default AAZ = 256./360.
                     BAZ = 0.0
.VARIABLE BAZ
             Offset factor for azimuth.

             DN(AZIM) = (AAZ * AZIMUTH) + BAZ
             where
             aaz      - scale factor 
             azimuth  - computed azimuth in degrees from North
             baz      - offset factor

             Default AAZ = 256./360.
                     BAZ = 0.0
.VARIABLE LSCALE
             Line scale in meters per line.

             Default = 1.0
.VARIABLE SSCALE
             Sample scale in meters per sample.

             Default = 1.0
.VARIABLE AALT
             Altitude scale in meters per dn of input.

             Default = 1.0 meter/dn
2  HISTORY

  Made Portable for UNIX  RNR(CRI)  05-SEP-94
  Changed default values and modified I/O REA 08-NOV-94

$ Return
$!#############################################################################
$Test_File:
$ create tstpixgrad.pdf
!  Procedure to test the procedure pixgrad
procedure
refgbl $echo
refgbl $autousage
body
let _onfail="continue"
let $echo="yes"
let $autousage="none"
gen a 10 10 sinc=2
pixgrad a (b c)
list b
list c
gen a 10 10 sinc=2 'half
pixgrad a (b c)
list b
list c
end-proc
$ Return
$!#############################################################################
