$!****************************************************************************
$!
$! Build proc for MIPL module astergeo
$! VPACK Version 1.8, Thursday, May 17, 2001, 15:35:05
$!
$! Execute by entering:		$ @astergeo
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
$ write sys$output "*** module astergeo ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
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
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Imake .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to astergeo.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
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
$   if F$SEARCH("astergeo.imake") .nes. ""
$   then
$      vimake astergeo
$      purge astergeo.bld
$   else
$      if F$SEARCH("astergeo.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake astergeo
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @astergeo.bld "STD"
$   else
$      @astergeo.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create astergeo.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack astergeo.com -
	-s astergeo.f -
	-p astergeo.pdf -
	-i astergeo.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create astergeo.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
C	This program accepts either (a) the two 11x11 lat/long arrays
C	associated with ASTER scenes, and included in the ASTER hdf files,
C	or (b) the corner point latitude and longitude coordinates input
C	as parameters.  From this, a pair of full size images is created,
C	which give the latitude and longitude for each ASTER scene pixel.
C
C	5/14/01  ...rea... Initial release
C
	IMPLICIT NONE
	REAL*8 CORNER(2,4),XLATIN(11,11),XLONGIN(11,11),CONV
	REAL*8 CONVFAC/0.99330562/
	INTEGER NLO,NSO,NINP,ICOUNT,IDEF,INUNIT1,INUNIT2,ISTAT,NBANDS
	INTEGER ILINE,IOUT
	LOGICAL XVPTST
C
	CALL XVMESSAGE('ASTERGEO Version: May 18, 2001',' ')
C							   determine output size
	IF (XVPTST('VNIR')) THEN
	    NLO = 4200
	    NSO = 4980
	ELSE IF (XVPTST('SWIR')) THEN
	    NLO = 2100
	    NSO = 2490
	ELSE
	    NLO = 700
	    NSO = 830
	END IF
C					     is coordinate conversion requested?
	IF (XVPTST('GEODETIC')) THEN
	    CONV = 1.0 / CONVFAC
	ELSE IF (XVPTST('GEOCENTRIC')) THEN
	    CONV = CONVFAC
	ELSE
	    CONV = 1.0
	END IF
C						   get input files or parameters
	CALL XVPCNT('INP',NINP)
	IF (NINP .EQ. 0) THEN
	    CALL XVPARMD('UL',CORNER(1,1),ICOUNT,IDEF,2)
	    IF (ICOUNT .NE. 2) THEN
		CALL XVMESSAGE('Two UL values or input file needed',' ')
		CALL ABEND
	    ENDIF
	    CALL XVPARMD('UR',CORNER(1,2),ICOUNT,IDEF,2)
	    IF (ICOUNT .NE. 2) THEN
		CALL XVMESSAGE('Two UR values needed',' ')
		CALL ABEND
	    ENDIF
	    CALL XVPARMD('LL',CORNER(1,3),ICOUNT,IDEF,2)
	    IF (ICOUNT .NE. 2) THEN
		CALL XVMESSAGE('Two LL values needed',' ')
		CALL ABEND
	    ENDIF
	    CALL XVPARMD('LR',CORNER(1,4),ICOUNT,IDEF,2)
	    IF (ICOUNT .NE. 2) THEN
		CALL XVMESSAGE('Two LR values needed',' ')
		CALL ABEND
	    ENDIF
	ELSE IF (NINP .EQ. 1) THEN
	    CALL XVUNIT(INUNIT1,'INP',1,ISTAT,' ')
	    CALL XVOPEN(INUNIT1,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     +		    'U_FORMAT','DOUB',' ')
	    CALL XVGET(INUNIT1,ISTAT,'NB',NBANDS,' ')
	    IF (NBANDS .LT. 2) THEN
		CALL XVMESSAGE(
     +	'If one input file, it must contain both lat and long bands',' ')
		CALL ABEND
	    ENDIF
	    DO ILINE=1,11
		CALL XVREAD(INUNIT1,XLATIN(1,ILINE),ISTAT,'LINE',ILINE,
     +			    'BAND',1,' ')
		CALL XVREAD(INUNIT1,XLONGIN(1,ILINE),ISTAT,'LINE',ILINE,
     +			    'BAND',2,' ')
	    END DO
	ELSE
	    CALL XVUNIT(INUNIT1,'INP',1,ISTAT,' ')
	    CALL XVOPEN(INUNIT1,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     +		    'U_FORMAT','DOUB',' ')
	    CALL XVUNIT(INUNIT2,'INP',2,ISTAT,' ')
	    CALL XVOPEN(INUNIT2,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     +		    'U_FORMAT','DOUB',' ')
	    DO ILINE=1,11
		CALL XVREAD(INUNIT1,XLATIN(1,ILINE),ISTAT,'LINE',ILINE,
     +			    ' ')
		CALL XVREAD(INUNIT2,XLONGIN(1,ILINE),ISTAT,'LINE',ILINE,
     +			    ' ')
	    END DO
	END IF
C							        open output file
	CALL XVUNIT(IOUT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUT,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     &		'U_NL',NLO,'U_NS',NSO,'U_ORG','BIL','OP','WRITE',
     &		'U_FORMAT','DOUB','O_FORMAT','DOUB','U_NB',2,' ')
C
	IF (NINP .EQ. 0) THEN
	    CALL FILL2(IOUT,NLO,NSO,CORNER,CONV)
	ELSE
	    CALL FILL11(IOUT,NLO,NSO,XLATIN,XLONGIN,CONV)
	END IF
C
	RETURN
	END
C*******************************************************************************
	SUBROUTINE FILL2(IOUT,NLO,NSO,CORNER,CONV)
C
C	This routine creates the output dataset, from the corner points
C	that have been input by the user. Bilinear interpolation is used.
C
	IMPLICIT NONE
	REAL*8 CORNER(2,4),XLATOUT(4980),XLONGOUT(4980),CONV
	REAL*8 XNL,XNS,X,FRAC1,FRAC2
	INTEGER IOUT,NLO,NSO,ILINE,ISAMP,ISTAT
	REAL*8 PI/3.141592653589793D0/

C						 if requested, convert latitudes
	IF (CONV .NE. 1.0) THEN
	    DO ILINE=1,4
		X = CONV*TAN(CORNER(1,ILINE)*PI/180.)
		CORNER(1,ILINE) = ATAN(X)*180./PI
	    END DO
	END IF
C
	XNL = NLO
	XNS = NSO
C
	DO ILINE=1,NLO
	    X = ILINE - 1
	    FRAC1 = X/XNL
	    DO ISAMP=1,NSO
		X = ISAMP - 1
		FRAC2 = X/XNS
		XLATOUT(ISAMP) = (1.0-FRAC1)*(1.0-FRAC2)*CORNER(1,1) +
     +				 (1.0-FRAC1)*FRAC2*CORNER(1,2) +
     +				 FRAC1*(1.0-FRAC2)*CORNER(1,3) +
     +				 FRAC1*FRAC2*CORNER(1,4) 
		XLONGOUT(ISAMP) = (1.0-FRAC1)*(1.0-FRAC2)*CORNER(2,1) +
     +				  (1.0-FRAC1)*FRAC2*CORNER(2,2) +
     +				  FRAC1*(1.0-FRAC2)*CORNER(2,3) +
     +				  FRAC1*FRAC2*CORNER(2,4) 
	    END DO
	    CALL XVWRIT(IOUT,XLATOUT,ISTAT,' ')
	    CALL XVWRIT(IOUT,XLONGOUT,ISTAT,' ')
	END DO
C
	RETURN
	END
C*******************************************************************************
	SUBROUTINE FILL11(IOUT,NLO,NSO,XLATIN,XLONGIN,CONV)
C
C	This routine creates the output dataset, from the 11x11 latitude
C	and longitude arrays. Bilinear interpolation is used.
C
	IMPLICIT NONE
	REAL*8 XLATIN(11,11),XLONGIN(11,11),XLAT(11),XLONG(11)
	REAL*8 CONV,XNL,XNS,X,FRAC,XLATOUT(4980),XLONGOUT(4980)
	INTEGER IOUT,NLO,NSO,ILINE,ISAMP,INDEX,IX,ISTAT
	REAL*8 PI/3.141592653589793D0/
C
	XNL = NLO
	XNS = NSO
C						 if requested, convert latitudes
	IF (CONV .NE. 1.0) THEN
	    DO ILINE=1,11
		DO ISAMP=1,11
		    X = CONV*TAN(XLATIN(ISAMP,ILINE)*PI/180.)
		    XLATIN(ISAMP,ILINE) = ATAN(X)*180./PI
		END DO
	    END DO
	END IF
C
	DO ILINE=1,NLO
	    X = 10 * (ILINE - 1)
	    FRAC = X/XNL
	    INDEX = INT(FRAC)
	    FRAC = FRAC - INDEX
	    INDEX = INDEX + 1
	    DO IX=1,11
		XLAT(IX) = (1.0-FRAC)*XLATIN(IX,INDEX) + 
     +			   FRAC*XLATIN(IX,INDEX+1)
		XLONG(IX) = (1.0-FRAC)*XLONGIN(IX,INDEX) + 
     +			    FRAC*XLONGIN(IX,INDEX+1)
	    END DO
	    DO ISAMP=1,NSO
		X = 10 * (ISAMP - 1)
		FRAC = X/XNS
		INDEX = INT(FRAC)
		FRAC = FRAC - INDEX
		INDEX = INDEX + 1
		XLATOUT(ISAMP) = (1.0-FRAC)*XLAT(INDEX) + 
     +				 FRAC*XLAT(INDEX+1)
		XLONGOUT(ISAMP) = (1.0-FRAC)*XLONG(INDEX) + 
     +				  FRAC*XLONG(INDEX+1)
	    END DO
	    CALL XVWRIT(IOUT,XLATOUT,ISTAT,' ')
	    CALL XVWRIT(IOUT,XLONGOUT,ISTAT,' ')
	END DO
C
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create astergeo.pdf
Process help=*
parm  INP     (string,40) count=0:2 default=--
parm  OUT     (string,40)
parm  MODE    keyword     valid=(VNIR,SWIR,TIR)
parm  CONVERT keyword valid=(NOCONVERT,GEODETIC,GEOCENTRIC) default=NOCONVERT
parm  UL      real        count=0:2 default=--
parm  UR      real        count=0:2 default=--
parm  LL      real        count=0:2 default=--
parm  LR      real        count=0:2 default=--
End-proc

.TITLE
TAE PROCESS ASTERGEO
.HELP
PURPOSE:
ASTER images have, in their files in hdf format, two sources of information
to enable geolocation of the image pixels.  First, the corner points of the
image are stored in the metadata ("productmetadata.0", mastergroup 
ASTERGENERICMETADATA, group SCENEINFORMATION, subgroup SCENEFOURCORNERS, 
objects UPPERLEFT, UPPERRIGHT, LOWERLEFT, LOWERRIGHT).  Second, there is an 
11 by 11 pixel image array of latitudes, and a similar array for longitude.
This program will generate full scale pixel by pixel maps of the latitudes 
and longitudes from either of these two sources.  That is, the output file will
contain two bands, with the first band containing the latitude corresponding to
each ASTER pixel, with the second band containing the longitude at each pixel.

If the corner points are used as input, they will be entered manually as
VICAR parameters.  If the 11x11 latitude and longitude arrays are used, they
are entered as either one or two input images, with the appropriate VICAR 
labels previously applied.

If requested, this program will also convert between geocentric and geodetic
coordinate systems.  The default is that no coordinate conversion is performed.
.LEVEL1
.VARI INP
11x11 latitude and longitude
arrays (with VICAR labels)
Either 1 file with 2 bands,
or latitude file, then
   longitude file
.VARI OUT
Output image file
.VARI MODE
Spectral Range
Valid:  VNIR, SWIR, TIR
.VARI CONVERT
Convert coordinates to:
Valid: GEODETIC, GEOCENTRIC,
       or NOCONVERT (for
       no change)
.VARI UL
Latitude and longitude of
upper left corner
.VARI UR
Latitude and longitude of
upper right corner
.VARI LL
Latitude and longitude of
lower left corner
.VARI LR
Latitude and longitude of
lower right corner
.LEVEL2
.VARI INP
INP is used to specify the names of 0, 1, or 2 files.  If no file name is
given, then the user must enter the corner coordinates via the parameters
UL, UR, LL, and LR.  If one file is input, it must be a VICAR file with
two bands, 11 lines, 11 samples.  The first band shall contain the latitudes,
and the second band shall contain the longitudes.  If two files are input,
the first file shall contain the latitudes, the second shall contain the
longitudes.
.VARI OUT
The value of OUT is the name of the output file, which will contain two bands
(first latitude, then longitude) in double precision format, and having the
same size as the image data for the spectral region specified by the MODE
parameter.
.VARI MODE
This parameter is used to specify the required size of the output image. The
output sizes are:

       MODE       LINES     SAMPLES
       ----       -----     -------
       VNIR        4200      4980
       SWIR        2100      2490
        TIR         700       830
.VARI CONVERT
The corner points listed in the hdf metadata are in geodetic coordinates,
while the 11x11 image arrays are in geocentric coordinates for Level 1 data
products and early Level 2 products, and in geodetic coordinates for more
recent Level 2 products.  The user may convert from one coordinate system to
the other, by using the CONVERT paramter, giving it the name of the desired
result (either GEODETIC or GEOCENTRIC).  If defaulted, no conversion is
performed.
.VARI UL
This is the latitude (positive North) and longitude (positive East) for the
upper left corner of the ASTER scene.  These values may be found in the
metadata, in "productmetadata.0", mastergroup ASTERGENERICMETADATA, group
SCENEINFORMATION, subgroup SCENEFOURCORNERS, object UPPERLEFT.

For example,
      UL=(19.447252,-155.613547)
for 19.447252 North, 155.613547 West
.VARI UR
This is the latitude (positive North) and longitude (positive East) for the
upper right corner of the ASTER scene.  These values may be found in the
metadata, in "productmetadata.0", mastergroup ASTERGENERICMETADATA, group
SCENEINFORMATION, subgroup SCENEFOURCORNERS, object UPPERRIGHT.

For example,
      UR=(19.447252,-155.613547)
for 19.447252 North, 155.613547 West
.VARI LL
This is the latitude (positive North) and longitude (positive East) for the
lower left corner of the ASTER scene.  These values may be found in the
metadata, in "productmetadata.0", mastergroup ASTERGENERICMETADATA, group
SCENEINFORMATION, subgroup SCENEFOURCORNERS, object LOWERLEFT.

For example,
      LL=(19.447252,-155.613547)
for 19.447252 North, 155.613547 West
.VARI LR
This is the latitude (positive North) and longitude (positive East) for the
lower right corner of the ASTER scene.  These values may be found in the
metadata, in "productmetadata.0", mastergroup ASTERGENERICMETADATA, group
SCENEINFORMATION, subgroup SCENEFOURCORNERS, object LOWERRIGHT.

For example,
      LR=(19.447252,-155.613547)
for 19.447252 North, 155.613547 West
.END
$ Return
$!#############################################################################
$Imake_File:
$ create astergeo.imake
#define  PROGRAM   astergeo

#define MODULE_LIST astergeo.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
