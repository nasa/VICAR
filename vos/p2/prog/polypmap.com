$!****************************************************************************
$!
$! Build proc for MIPL module polypmap
$! VPACK Version 1.8, Tuesday, April 09, 1996, 13:29:52
$!
$! Execute by entering:		$ @polypmap
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
$ write sys$output "*** module polypmap ***"
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
$ write sys$output "Invalid argument given to polypmap.com file -- ", primary
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
$   if F$SEARCH("polypmap.imake") .nes. ""
$   then
$      vimake polypmap
$      purge polypmap.bld
$   else
$      if F$SEARCH("polypmap.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake polypmap
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @polypmap.bld "STD"
$   else
$      @polypmap.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create polypmap.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack polypmap.com -
	-s polypmap.f -
	-i polypmap.imake -
	-p polypmap.pdf -
	-t tstpolypmap.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create polypmap.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44


      IMPLICIT NONE
      include 'mp_for_defs'  !needed for MP software.  This version (March 1996)
                             ! uses CONVEV, but is part way converted to use
                             ! the MP routines, when they are debugged. It would
                             ! need a call to mp_buf2mpo, and then the call to
                             ! convev can be replaced with calls to mp_xy2ll and
                             ! mp_ll2xy.
      INTEGER*4 istat                    !for mp routines
      REAL*8 mp
	INTEGER COUNT, DEF, DEF1, DEF2
	INTEGER SKIP
	REAL	X, Y, EXTRA(40)
	REAL	ERAD, PRAD
	INTEGER	FLAG, MAPTYPE, MODE, STATUS
	REAL	LINE, SAMP, LAT, LONG
	REAL*8	SPECSAMP, SPECLINE, SPECLAT, PARALLEL1, PARALLEL2
	REAL*8	SPECLONG, PIXSCALE, POLEFLAG
	REAL*8	POLRADIUS, EQUARADIUS, NORTHANG
	LOGICAL EOF, ZERO,  XVPTST
        INTEGER WRGR,  RDGR, GETGR, PUTGR, CLGR

	CHARACTER*12  PLANET
        REAL RDATA(40)
        INTEGER IDATA(40)
        EQUIVALENCE (IDATA(1),RDATA(1))

	COMMON /MAPCOM/  MAPTYPE, MODE, SPECSAMP, SPECLINE, 
     +		        SPECLAT, PARALLEL1, PARALLEL2, SPECLONG, 
     +		PIXSCALE, POLEFLAG, POLRADIUS, EQUARADIUS, NORTHANG



        CALL XVMESSAGE('POLYPMAP version Mar 8 1996',' ')
        CALL INIT_SPICE

        call mp_init( mp,istat)
        if(istat.ne.mp_success) call mabend('error in mp_init')

C		Get the parameters

	IF (XVPTST('MERCATOR')) THEN
	    MAPTYPE = 6
            call mp_set_value_str(mp,'MAP_PROJECTION_TYPE',
     +                      'MERCATOR',istat)
	ELSE IF (XVPTST('LAMBERT')) THEN
	    MAPTYPE = 5
            call mp_set_value_str(mp,'MAP_PROJECTION_TYPE',
     +                      'LAMBERT_CONFORMAL',istat)
	ELSE IF (XVPTST('CYLINDRI')) THEN
	    MAPTYPE = 9
            call mp_set_value_str(mp,'MAP_PROJECTION_TYPE',
     +                      'NORMAL_CYLINDRICAL',istat)
	ELSE IF (XVPTST('RECTANGU')) THEN
	    MAPTYPE = 10
            call mp_set_value_str(mp,'MAP_PROJECTION_TYPE',
     +                      'SIMPLE_CYLINDRICAL',istat)
	ELSE IF (XVPTST('POLSTERE')) THEN
	    MAPTYPE = 3
            call mp_set_value_str(mp,'MAP_PROJECTION_TYPE',
     +                      'POLAR_STEREOGRAPHIC',istat)
	ELSE IF (XVPTST('STEREOGR')) THEN
	    MAPTYPE = 4
            call mp_set_value_str(mp,'MAP_PROJECTION_TYPE',
     +                      'OBLIQUE_STEREOGRAPHIC',istat)
	ELSE IF (XVPTST('POLORTHO')) THEN
	    MAPTYPE = 1
            call mp_set_value_str(mp,'MAP_PROJECTION_TYPE',
     +                      'POLAR_ORTHOGRAPHIC',istat)
	ELSE IF (XVPTST('ORTHOGRA')) THEN
	    MAPTYPE = 2
            call mp_set_value_str(mp,'MAP_PROJECTION_TYPE',
     +                      'OBLIQUE_ORTHOGRAPHIC',istat)
	ELSE IF (XVPTST('OBLICYL')) THEN
	    MAPTYPE = 11
            call mp_set_value_str(mp,'MAP_PROJECTION_TYPE',
     +                      'OBLIQUE_SIMPLE_CYLINDRICAL',istat)
	ELSE IF (XVPTST('SINUSOID')) THEN
	    MAPTYPE = 12
            call mp_set_value_str(mp,'MAP_PROJECTION_TYPE',
     +                      'SINUSOIDAL',istat)
	ELSE IF (XVPTST('OBSINUSO')) THEN
	    MAPTYPE = 13
            call mp_set_value_str(mp,'MAP_PROJECTION_TYPE',
     +                      'OBLIQUE_SINUSOIDAL',istat)
	ELSE IF (XVPTST('MOLLWEID')) THEN
	    MAPTYPE = 14
            call mp_set_value_str(mp,'MAP_PROJECTION_TYPE',
     +                      'MOLLWEIDE',istat)
	ELSE IF (XVPTST('TMERCATO')) THEN
	    MAPTYPE = 15
            call mp_set_value_str(mp,'MAP_PROJECTION_TYPE',
     +                      'TRANSVERSE_MERCATOR',istat)
	ENDIF


	IF (XVPTST('INVERSE')) THEN
	    MODE = 2
	ELSE
	    MODE = 1
	ENDIF

	CALL XVP ('PLANET', PLANET, COUNT)
	CALL GETPLANETSIZE (PLANET, ERAD, PRAD)
	EQUARADIUS = DBLE(ERAD)
	POLRADIUS = DBLE(PRAD)

	CALL XVPARMD ('SCALE', PIXSCALE, COUNT, DEF,  1)
	CALL XVPARMD ('LATITUDE', SPECLAT, COUNT, DEF,  1)
	CALL XVPARMD ('LONGITUD', SPECLONG, COUNT, DEF,  1)
	CALL XVPARMD ('PARAL1', PARALLEL1, COUNT, DEF,  1)
	CALL XVPARMD ('PARAL2', PARALLEL2, COUNT, DEF,  1)
	CALL XVPARMD ('NORTHANG', NORTHANG, COUNT, DEF,  1)
	CALL XVPARMD ('LINE', SPECLINE, COUNT, DEF,  1)
	CALL XVPARMD ('SAMPLE', SPECSAMP, COUNT, DEF,  1)

	IF (XVPTST('SOUTH')) THEN
	    POLEFLAG = -1.0D0
	ELSE
	    POLEFLAG = +1.0D0
	ENDIF
	IF (MAPTYPE .EQ. 5) THEN
	    POLEFLAG = DSIGN (1.0D0, MIN(PARALLEL1,PARALLEL2) )
	ENDIF

	CALL XVP ('SKIP', SKIP, COUNT)


C...Set up the MP BUFFER.

       IDATA(39)=MAPTYPE      ! MAP PROJECTION TYPE
       RDATA(1)=SPECSAMP   ! SPECIAL SAMPLE
       RDATA(2)=SPECLINE   ! SPECIAL LINE
       RDATA(3)=SPECLAT    ! SPECIAL LATITUDE DEGREES
       RDATA(4)=PARALLEL1  ! LAT OF NORTH PARALLEL DEGREES (LAMBERT CASE)
       RDATA(5)=PARALLEL2  ! LAT OF SOUTH PARALLEL DEGREES (LAMBERT CASE)
       RDATA(6)=SPECLONG   ! SPECIAL LONGITUDE DEGREES
       RDATA(7)=PIXSCALE   ! SCALE KM/PXL
       RDATA(8)=POLEFLAG! VISIBLE POLE +1 FOR NORTH -1 FOR SOUTH
C       SPECIAL CASE FOR LAMBERT
       IF(MAPTYPE.EQ.5)THEN
         IF(PARALLEL1/PARALLEL2.LT.0.)  CALL mabend(
     .      'FOR LAMBERT BOTH PARALLELS MUST ON SAME SIDE OF EQUATOR')
       ENDIF
       RDATA(9)=NORTHANG      ! NORTH ANGLE DEGREES
       RDATA(25)=PRAD         ! POLAR RADIUS KM
       RDATA(26)=ERAD         ! EQUATORIAL RADIUS


	IF (XVPTST('AUTOMAT')) THEN
	    CALL AUTOPLACE(RDATA)
	ENDIF

C		Open the input graphics 1 file
	STATUS = RDGR (1, 1, SKIP+2)
        if (status.ne.1) call signalgr(1,status,1)

C		Open the output graphics 1 file
	STATUS = WRGR (1, 2, SKIP+2)
        if (status.ne.1) call signalgr(2,status,1)



C	    Main loop through input graphics file

	DO WHILE (.TRUE.)
	    STATUS = GETGR (1, ZERO, EOF, X, Y, EXTRA)
            if (status.ne.1) call signalgr(1,status,1)
	    IF (EOF) GOTO 90
	    IF (X .NE. 0.0 .OR. Y .NE. 0.0) THEN
                
		IF (MODE .EQ. 1) THEN
		    LAT = X
		    LONG = Y
		ELSE
		    LINE = X
		    SAMP = Y
		ENDIF
		CALL MAPPROJ (LAT, LONG, LINE, SAMP,RDATA)
		IF (MODE .EQ. 1) THEN
		    X = LINE
		    Y = SAMP
		ELSE
		    X = LAT
		    Y = LONG
		ENDIF
	    ENDIF
	    STATUS = PUTGR (2, X, Y, EXTRA)
            if (status.ne.1) call signalgr(2,status,1)
	ENDDO

90	CONTINUE
	STATUS = CLGR (1)
        if (status.ne.1) call signalgr(1,status,1)
	STATUS = CLGR (2)
        if (status.ne.1) call signalgr(2,status,1)

        call mp_free(mp)
	RETURN
	END




	SUBROUTINE MAPPROJ (LAT, LONG, LINE, SAMP,RDATA)
	IMPLICIT NONE
	REAL	LINE, SAMP, LAT, LONG, RDATA(40)
        REAL    DUMMY       ! POLYPMAP DOES NOT SUPPORT IMAGE SPACE
	INTEGER	FLAG, MAPTYPE, MODE
	REAL*8	SPECSAMP, SPECLINE, SPECLAT, PARALLEL1, PARALLEL2
	REAL*8	SPECLONG, PIXSCALE, POLEFLAG
	REAL*8	POLRADIUS, EQUARADIUS, NORTHANG

	COMMON /MAPCOM/  MAPTYPE, MODE, SPECSAMP, SPECLINE, 
     +			SPECLAT, PARALLEL1, PARALLEL2, SPECLONG, 
     +		PIXSCALE, POLEFLAG, POLRADIUS, EQUARADIUS, NORTHANG

c==================================================================

        CALL CONVEV(FLAG, RDATA,RDATA, LINE,SAMP, LAT,LONG,MODE,DUMMY)
	IF (FLAG .NE. 0) THEN     ! IF POINT OFF PLANET, SET OUTPUT TO 0.
	    IF (MODE .EQ. 1) THEN
		LINE = 0.0
		SAMP = 0.0
	    ELSE
		LAT = 0.0
		LONG = 0.0
	    ENDIF
	ENDIF

	RETURN
	END




	SUBROUTINE AUTOPLACE(RDATA)
	IMPLICIT NONE
	INTEGER	COUNT, MODE, MAPTYPE
	REAL	LATRANGE(2), LONRANGE(2), SPLINE, SPSAMP
	REAL	MINLINE, MINSAMP, MAXLINE, MAXSAMP
	REAL	LINE, SAMP, LAT, LONG, RDATA(40)
	REAL*8	SPECSAMP, SPECLINE, SPECLAT, PARALLEL1, PARALLEL2
	REAL*8	SPECLONG, PIXSCALE, POLEFLAG
	REAL*8	POLRADIUS, EQUARADIUS, NORTHANG
	CHARACTER*72 STRING

	COMMON /MAPCOM/  MAPTYPE, MODE, SPECSAMP, SPECLINE, 
     +			SPECLAT, PARALLEL1, PARALLEL2, SPECLONG, 
     +		PIXSCALE, POLEFLAG, POLRADIUS, EQUARADIUS, NORTHANG

C==================================================================
	CALL XVP ('LATRANGE', LATRANGE, COUNT)
	CALL XVP ('LONRANGE', LONRANGE, COUNT)
	CALL XVP ('LINE', SPLINE, COUNT)
	CALL XVP ('SAMPLE', SPSAMP, COUNT)
	IF (MODE .EQ. 2) THEN
	    CALL XVMESSAGE (
     +          'Auto placement not available in inverse mode.',' ')
	    CALL ABEND
	ENDIF

	SPECLINE = 0.0
	SPECSAMP = 0.0
        RDATA(1)=SPECSAMP   ! Update the values in rdata accordingly.
        RDATA(2)=SPECLINE   ! SPECIAL LINE
	MINLINE = +1.0E30
	MAXLINE = -1.0E30
	MINSAMP = +1.0E30
	MAXSAMP = -1.0E30

	LAT = LATRANGE(1)
	DO WHILE (LAT .LE. LATRANGE(2))
	    CALL MAPPROJ  (LAT, LONRANGE(1), LINE, SAMP, RDATA)
	    MINLINE = MIN (MINLINE, LINE)
	    MAXLINE = MAX (MAXLINE, LINE)
	    MINSAMP = MIN (MINSAMP, SAMP)
	    MAXSAMP = MAX (MAXSAMP, SAMP)
	    LAT = LAT + (LATRANGE(2)-LATRANGE(1))/10.
	ENDDO
	LAT = LATRANGE(1)
	DO WHILE (LAT .LE. LATRANGE(2))
	    CALL MAPPROJ  (LAT, LONRANGE(2), LINE, SAMP, RDATA)
	    MINLINE = MIN (MINLINE, LINE)
	    MAXLINE = MAX (MAXLINE, LINE)
	    MINSAMP = MIN (MINSAMP, SAMP)
	    MAXSAMP = MAX (MAXSAMP, SAMP)
	    LAT = LAT + (LATRANGE(2)-LATRANGE(1))/10.
	ENDDO

	    CALL MAPPROJ  (LATRANGE(2), LONRANGE(2), LINE, SAMP, RDATA)
	    MINLINE = MIN (MINLINE, LINE)
	    MAXLINE = MAX (MAXLINE, LINE)  ! Make double sure this corner
	    MINSAMP = MIN (MINSAMP, SAMP)  ! is not skipped due to rounding.
	    MAXSAMP = MAX (MAXSAMP, SAMP)

	LONG = LONRANGE(1)
	DO WHILE (LONG .LE. LONRANGE(2))
	    CALL MAPPROJ  (LATRANGE(1), LONG, LINE, SAMP, RDATA)
	    MINLINE = MIN (MINLINE, LINE)
	    MAXLINE = MAX (MAXLINE, LINE)
	    MINSAMP = MIN (MINSAMP, SAMP)
	    MAXSAMP = MAX (MAXSAMP, SAMP)
	    LONG = LONG + (LONRANGE(2)-LONRANGE(1))/10.
	ENDDO
	LONG = LONRANGE(1)
	DO WHILE (LONG .LE. LONRANGE(2))
	    CALL MAPPROJ  (LATRANGE(2), LONG, LINE, SAMP, RDATA)
	    MINLINE = MIN (MINLINE, LINE)
	    MAXLINE = MAX (MAXLINE, LINE)
	    MINSAMP = MIN (MINSAMP, SAMP)
	    MAXSAMP = MAX (MAXSAMP, SAMP)
	    LONG = LONG + (LONRANGE(2)-LONRANGE(1))/10.
	ENDDO

	SPECLINE = DBLE( SPLINE - MINLINE )
	SPECSAMP = DBLE( SPSAMP - MINSAMP )
	MAXLINE = MAXLINE + SPECLINE
	MAXSAMP = MAXSAMP + SPECSAMP
	WRITE (STRING, '(A,F10.2,1X,F10.2)' ) 
     +		'Special line and sample :',	SPECLINE, SPECSAMP
	CALL XVMESSAGE (STRING,' ')
	WRITE (STRING, '(A,F10.2,1X,F10.2)' ) 
     +		'Max line and sample :',	MAXLINE, MAXSAMP
	CALL XVMESSAGE (STRING,' ')

c..Update the values in rdata accordingly.

        RDATA(1)=SPECSAMP   ! SPECIAL SAMPLE
        RDATA(2)=SPECLINE   ! SPECIAL LINE

	RETURN
	END




	SUBROUTINE GETPLANETSIZE (PLANET, EQRADIUS, POLRADIUS)
	IMPLICIT NONE
	CHARACTER*12 PLANET
	REAL	EQRADIUS, POLRADIUS
	INTEGER	I
	REAL	PBBUFFER(20)
C==================================================================
	CALL PBDATA (planet, PBBUFFER, *999)
	EQRADIUS = PBBUFFER(1)
	POLRADIUS = PBBUFFER(3)

	RETURN

999	CONTINUE
	CALL XVMESSAGE (' '//PLANET//
     +                  ' is not an implemented planet.',' ')
	CALL ABEND
	RETURN
	END


$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create polypmap.imake
/***********************************************************************
                     IMAKE FILE FOR PROGRAM polypmap

   To Create the build file give the command:

		$ vimake polypmap			(VMS)
   or
		% vimake polypmap			(Unix)
************************************************************************/
#define PROGRAM	polypmap
#define R2LIB

#define MODULE_LIST polypmap.f
#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST mp_for_defs  
#define LIB_RTL
#define LIB_TAE

#define LIB_P2SUB
#define LIB_SPICE
#define LIB_NETWORK
#define LIB_MATH77
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create polypmap.pdf
PROCESS HELP=*
 PARM INP      TYPE=STRING
 PARM OUT      TYPE=STRING
 PARM PROJ     TYPE=KEYWORD  VALID=(MERCATOR,LAMBERT ,CYLINDRI,RECTANGU,  +
		  POLSTERE,STEREOGR,POLORTHO,ORTHOGRA,OBLICYL ,SINUSOID,  +
                  OBSINUSO,MOLLWEID,TMERCATO)
 PARM MODE     TYPE=KEYWORD  VALID=(DIRECT,INVERSE)  DEFAULT=DIRECT
 PARM PLANET   TYPE=(STRING,12)
 PARM SCALE    TYPE=REAL    
 PARM LINE     TYPE=REAL    DEFAULT=1
 PARM SAMPLE   TYPE=REAL    DEFAULT=1
 PARM LATITUDE TYPE=REAL    DEFAULT=0
 PARM LONGITUD TYPE=REAL    DEFAULT=0
 PARM PARAL1   TYPE=REAL    DEFAULT=35.83
 PARM PARAL2   TYPE=REAL    DEFAULT=59.17
 PARM NORTHANG TYPE=REAL    DEFAULT=0
 PARM POLE     TYPE=KEYWORD VALID=(NORTH,SOUTH) DEFAULT=NORTH
 PARM PLACEMEN TYPE=KEYWORD VALID=(AUTOMAT,MANUAL) DEFAULT=MANUAL
 PARM LATRANGE TYPE=REAL    COUNT=2  DEFAULT=(-85,85)
 PARM LONRANGE TYPE=REAL    COUNT=2  DEFAULT=(0,360)
 PARM SKIP     TYPE=INTEGER VALID=(0:38) DEFAULT=0
END-PROC
.TITLE
VICAR/IBIS Program POLYPMAP
.HELP
PURPOSE

    POLYPMAP converts coordinates for the specified map projection between
latitude-longitude space and line-sample space.

The following projections are currently implemented:

  Mercator					'MERCATOR
  Lambert Two-Standard Conformal Conic		'LAMBERT
  Cylindrical (Normal)				'CYLINDRI
  Simple Cylindrical (Rectangular)		'RECTANGU
  Polar Stereographic				'POLSTERE
  Oblique Stereographic 			'STEREOGR
  Polar Orthographic				'POLORTHO
  Oblique Orthographic 				'ORTHOGRA
  Oblique Simple Cylindrical 			'OBLICYL
  Sinusoidal Equal Area Projection  		'SINUSOID 
  Oblique Sinusoidal                            'OBSINUSO
  Mollweide                                     'MOLLWEID
  Transverse Mercator                           'TMERCATO

     The MIPL subroutine CONVEV (TRANV) is used to perform the map projections.
     POLYPMAP does not support "image space" or "object space".
     The results of the ported POLYPMAP  differ from the unported POLYPMAP
     in a few cases; (see under Precision and Accuracy).

EXECUTION

polypmap  INPUT.GRA OUTPUT.GRA  'projection    'mode     'placement     +
			PLANET=body              SCALE=pixel scale      +
			LATITUDE=special lat     LONGITUD=special long   +
			LINE=special line        SAMPLE=special sample  +
			PARAL1=first parallel    PARAL2=second parallel +
			NORTHANG=north angle     'pole			+
			LATRANGE=(minlat,maxlat) LONRANGE=(minlon,maxlon) +
			SKIP=nominal data



    The map projection, the planet, and the scale must be specified.
    The MODE keyword is used to indicate the direction of the transformation;
the default is latitude-longitude to line-sample; use 'INVERSE for line-samp
to lat-long.
    The 'AUTOMAT keyword will cause the special line and sample to be
calculated so that latitudes and longitudes inside the LATRANGE,
LONRANGE box will project to a desired location.   'AUTO placement 
can only be used for a forward transformation.



EXAMPLES

polypmap  INPUT.GRA OUTPUT.GRA    'LAMBERT   'AUTOPLAC   +
			PLANET=MARS      SCALE=0.1    +
			LINE=10   SAMPLE=10		+
			LATRANGE=(30,35)  LONRANGE=(10,15)  +
			PARAL1 = 35.83  PARAL2=59.17  



polypmap  INPUT.GRA OUTPUT.GRA    'MERCATOR   +
			PLANET=EARTH      SCALE=8.7    +
			LONGITUD=118   +
			LINE=500   SAMPLE=1000




NOTE: All latitudes and longitudes are in degrees, and all 
	longitudes are in West degrees (0 to 360).




    Projection Scaling
	In all map projections there is at least one point at which the scale
	of the projection equals the scale on the sphere. The scale at such
	points, in km/pixel, determines the size of the input picture in the
	output area. For the Mercator projection, scale is correct everywhere
	on the equator. For the Lambert, it is valid all along both standard
	parallels. For the Orthographic and Stereographic it is valid at the
	center of projection. (See also under Help for parameter PROJ.)


        Mercator Projection Parameters 
             LATITUDE    LONGITUD     LINE   SAMPLE     

        Lambert Projection Parameters
 	     LATITUDE    LONGITUD     PARAL1   PARAL2     LINE   SAMPLE

	Normal Cylindrical Parameters 
             LATITUDE    LONGITUD     LINE   SAMPLE     

	Simple Cylindrical (Rectangular) Projection Parameters
             LATITUDE    LONGITUD     LINE   SAMPLE     

        Polar Stereographic and Orthographic
	     LATITUDE    LONGITUD     POLE    LINE   SAMPLE
		
        Oblique Stereographic and Orthographic
	     LATITUDE   LONGITUD     NORTHANG    LINE   SAMPLE

	Oblique Simple Cylindrical
	     LATITUDE   LONGITUD     NORTHANG    LINE   SAMPLE

	Sinusoidal Equal Area Projection
             LONGITUD     LINE   SAMPLE     

	Oblique Sinusoidal Projection
             LATITUDE   LONGITUD     LINE   SAMPLE      PARAL1

        Mollweide Projection
             LONGITUD     LINE   SAMPLE     SCALE
       
        Transverse Mercator
             LATITUDE   LONGITUD     LINE   SAMPLE      SCALE
.page
PRECISION AND ACCURACY

The unported POLYPMAP used a local version of the SUBLIB routine TRANV to do
coordinate transformations.  The ported POLYPMAP uses the SUBLIB routine 
CONVEV instead.  The change was made for the following reasons:
1) The local version of TRANV was ten years behind the SUBLIB version.  It was
   missing several corrections and several projection types.  Instead of
   using routine MERCPATCH for Mercator projections, this version of TRANV
   had one line commented out;  this appeared to be an inadequate substitute
   for MERCPATCH, giving different results.  There was nothing comparable
   to routine RECTPATCH in the local version of TRANV.
2) The use of local variants of SUBLIB routines makes VICAR maintenance more
   difficult.
3) Jean Lorre and Lucas Kamp said that TRANV was not intended for direct calls
   from application programs but was intended to be accessed via subroutine
   CONVEV.  

The results from the ported POLYPMAP differ from the unported POLYPMAP in
the following cases:
a) Mercator projection:  Because of the code differences described in 1) above,
   differences such as the following occur:
   Input lat,lon:       -60.73      -45.21
   Output line,samp
    from ported pgm     -24.04       51.33
   Output line,samp
    from unported pgm    86.65       51.33

b) Oblique simple cylindrical projection: The unported local version on TRANV
   appears to be in error, failing to convert THR1 to east longitude for the
   calculation of OLAMBDA.  The final equation for computing SAMPLE appears to
   be different.
   Differences such as the following occur:
   Input lat,lon:       -60.73      -45.21
   Output line,samp
    from ported pgm     -76.53     -134.44
   Output line,samp
    from unported pgm   -76.53      -54.67
c) Sinusoidal projection:  The computations in the ported TRANV have been
   overhauled, resulting in minor differences such as the following: 
   Input lat,lon:       -60.73      -45.21
   Output line,samp
    from ported pgm      -9.30       25.58
   Output line,samp
    from unported pgm    -9.44       25.69

.page
PROGRAM HISTORY:
Original Programmer:    Frank Evans	May 1986
Cognizant Programmer:   Steve Pohorsky
Made portable for UNIX: Steve Pohorsky  Feb 1996
Revision:
  25 Jun 92  GMY  Added call to INIT_SPICE 70964
     Feb 96  SP   Ported to UNIX; now using SUBLIB routine CONVEV instead
                  of a local version of TRANV.  Works differently in a few
                  cases: see under Precision and Accuracy.
.LEVEL1
.VARIABLE INP
Input IBIS graphics-1 file
.VARIABLE OUT
Output IBIS graphics-1 file
to be created.
.VARIABLE PROJ
The map projection to use.
.VARIABLE MODE
'DIRECT for lat-lon to 
   line-sample
'INVERSE for line-sample to 
   lat-lon 
.VARIABLE PLANET
Name of target planetary body
.VARIABLE SCALE
Scale in km/pixel.
.VARIABLE LINE
Special line 
.VARIABLE SAMPLE
Special sample
.VARIABLE LATITUDE
Special latitude
.VARIABLE LONGITUD
Special longitude
.VARIABLE PARAL1
First standard parallel(Lambert)
; or, special longitude for Oblique.
.VARIABLE PARAL2
Second standard parallel
(Only used for Lambert)
.VARIABLE NORTHANG
Angle of north in degrees. 
(Only used for Oblique Orthographic, 
Oblique Stereographic,
and Oblique Cylindrical)
.VARIABLE POLE
Visible pole
(Only used for Polar Orthographic 
and Polar Stereographic)
.VARIABLE PLACEMEN
 'AUTO for automatic placement
 of output in line-sample.
.VARIABLE LATRANGE
 LATRANGE specifies the 
 latitude range for 'AUTO 
 placement mode.
.VARIABLE LONRANGE
 LONRANGE specifies the 
 longitude range for 'AUTO 
 placement mode.
.VARIABLE SKIP
The number of nominal data
values to skip over.


.LEVEL2
.VARIABLE INP
Input IBIS graphics-1 file.  (This file may be in IBIS1 or IBIS2 format.)
.VARIABLE OUT
Output IBIS graphics-1 file to be created.  (This file will be in IBIS2 format.)
.VARIABLE PLANET
 PLANET is the name of the target body (up to 12 characters).  
 The subroutine PBDATA (containing the radii of the nine
 planets and  and their moons) is called to obtain the 
 appropriate radii.

.VARIABLE PROJ
Each possible projection type is described below.
 'MERCATOR specifies a Mercator projection. 
 This projection  maps the sphere, except for the two poles, onto a 
 strip on the plane.  The width of the strip is equal to the scaled
 circumference of the  planet at the equator.  It extends infinitely
 in both vertical  directions.  Longitude lines project to the
 infinitely long, vertical straight lines which are equally spaced.
 Latitude circles become horizontal line segments whose spacing
 increases without limit as  you approach the pole.  The Mercator is a
 conformal projection (scale  errors at any point are equal in all
 directions, so shapes of small  areas are preserved).  
    The MERCator projection works somewhat differently than before the
 port to UNIX:
        LINE      The line in the output to which LATITUDE will project.
                  Defaults to 1.0.
        SAMPLE    The sample in the output to which LONGITUDE will project.
                  Defaults to 1.0.
        LATITUDE  The latitude of LINE in the output.
                  Default computes latitude to center input in output.
        LONGITUDE The longitude of SAMPLE in the output.
                  Default computes longitude to center input in output.


.PAGE
 'LAMBERT specifies a Two-Standard Lambert Conformal Conic
 projection. In  this projection two latitude parallels on the same
 side of the equator  are chosen as "standard".  There will be no
 scale error any where on  these parallels.  In addition, some
 longitude is chosen as "central".  The projection is developed on a
 cone which intersects the planet at  the standard parallels.  This
 cone is cut along the meridian 180 degrees  away from the central
 meridian.  The result is that the sphere is mapped onto the region
 between two semi-infinite rays emanating from the projection of the
 pole in the same hemisphere as the standard parallels.  The opposite
 pole is the only point on the sphere which is not mapped.  Longitude
 meridians become semi-infinite rays emanating from the visible
 pole, but not at their true angle since all longitudes are confined
 to lie between the two outermost rays, each of which  represent the
 meridian along which the cone was cut.  The central meridian is the
 only vertical line among the longitude meridians.  Latitude circles
 become circular arcs centered on the projected pole and confined
 between the outer longitude meridians.  The longitude rays are
 equally spaced.  The latitude circles are too widely spaced outside 
 the standards and too closely spaced between them.  The spacing of
 the  latitude circles increases without limit as you approach the
 opposite pole.  As its name inmplies, this projection is conformal.
 Neither the  Mercator nor the Lambert are true perspective projections. 
	PARAL1 and PARAL2 are the latitudes of the standard parallels.
	The LONGITUD parameter specifies the central meridian.
	The line origin is the pole.
	The sample origin is central meridian.

.PAGE
 'CYLINDRI requests the cylindrical (normal) projection. 
 This projection maps the sphere onto a strip on the plane.  The width 
 of the strip is equal to the scaled circumference of the planet at 
 the equator.  Longitude lines project to vertical lines which are
 equally spaced and extend from one pole to the other.  Latitude
 circles become horizontal lines whose spacing varies as the cosine
 of the latitude.  The cylindrical projection is an equal area
 projection.  
	The LATITUDE parameter specifies the line origin.
	The LONGITUD parameter specifies the sample origin.

.PAGE
 'RECTANGU requests the Simple Cylindrical (Rectangular) projection.
 This projection is similar to the Normal Cylindrical except that
 the spacing of the latitude circles (horizontal lines) is constant
 with and is equal to the spacing of the longitude lines.  
	The LATITUDE parameter specifies the line origin.
	The LONGITUD parameter specifies the sample origin.

.PAGE
 'POLSTERE requests an Polar Stereographic projection. 
 The stereographic is true perspective projection.  The projection plane 
 is tangent to the planet at the pole.  Perspective lines emanate from 
 the other pole.  Thus the entire sphere except for one point is mapped 
 to the entire plane.  Longitude meridians are  straight lines intersecting
 at the pole at their correct angles.  Latitude circles project to complete 
 circles centered on the pole.  The projection is conformal.  Features are 
 expanded  more and more without limit as you move away from the center of 
 projection.
	The LONGITUD parameter specifies the meridian that is in the
	    "up" direction in the line-sample space.
	The POLE parameter specifies which pole.

.PAGE
 'STEREOGR requests an Oblique Stereographic projection. 
 The stereographic is true perspective projection.  A plane is placed  
 tangent to the sphere at the center of projection.  Perspective lines  
 emanate from the point on the sphere diametrically opposite from the
 center of projection.  Thus the entire sphere except for one point is  
 mapped to the entire plane.  Longitude lines and latitude circles  
 project to ellipses whose spacing and orientation vary in a  
 complicated way.  The projection is conformal.  Features are expanded  
 more and more without limit as you move away from the center of projection.
	The LATITUDE and LONGITUD parameters specify the center of
	    projection (i.e. the point on the planet directly below
	    perspective point).
	The NORTHANG parameter specifies the orientation of the projection.
	The line origin is the center of projection. 
	The sample origin is the center of projection. 

.PAGE
 'POLORTHO requests an Polar Orthographic projection. 
 This is a true perspective projection with perspective point at 
 infinity.  The projection plane is tangent to the planet at the pole.  
 Longitude meridians are  straight lines intersectiong at the pole at 
 their correct angles.  Latitude circles project to complete circles 
 centered on the pole.  The hemisphere centered at the pole is mapped 
 to a circle on the plane of radius Req.  Features are compressed 
 relative to their true scale as you move away from the pole.  No point 
 in the other hemisphere can be projected. 
	The LONGITUD parameter specifies the meridian that is in the
	    "up" direction in the line-sample space.
	The POLE parameter specifies which pole.

.PAGE
 'ORTHOGRA requests an Oblique Orthographic projection. 
 This is a true perspective projection with perspective point at 
 infinity.  The projection plane is tangent to the planet at the center 
 of projection.  Perspective lines are parallel to each other,
 perpendicular to the  projection plane.  Thus one hemisphere centered
 at the center of  projection is mapped to a circle on the plane of
 radius Req.  Longitude  meridians and latitude circles map to
 ellipses.  Features are compressed relative to their true scale as
 you move away from the center of projection.  No point more than 90
 degrees away from the center can be  projected.  This projection is
 frequently used by space projects because it makes the input frame
 appear much as it would from the spacecraft if it were directly
 above the center of projection.
	The LATITUDE and LONGITUD parameters specify the center of
	    projection (i.e. the point on the planet directly below
	    perspective point).
	The NORTHANG parameter specifies the orientation of the projection.
	The line origin is the center of projection. 
	The sample origin is the center of projection. 
.PAGE

 'OBLICYL requests an Oblique Simple Cylindrical projection. 
 The scale is correct along the oblique equator.
	The LATITUDE and LONGITUD parameters specify the center of
	    projection 
	The NORTHANG parameter specifies the orientation of the projection.
	The line origin is the center of projection. 
	The sample origin is the center of projection. 

.PAGE

 'SINUSOID requests a Sinusoidal Equal Area Projection of the authalic sphere.
 Latitude parallels are equally space in the line direction.  Longitude
 meridians are sinusoidal lines, except for the central meridian which is a
 straight vertical line.  The latitude scale is correct everywhere, and the
 longitude scale is correct along the central meridian.  Distortion 
 increases away from the central meridian.

	The LONGITUD parameter specifies the central meridian.
	The line origin is the equator (latitude = 0).
	The sample origin is the central meridian.

.PAGE
 'OBSINUSO requests Oblique sinusoidal projection. Same as sinusoidal except
 the sphere can be rotated before the projection is performed.
        LINE specifies the location of latitude=0,  
        SAMP specifies that of longitude=180.
        LATI and LONG specify the position of the North pole.  
        PARAL1 is the longitude to which LONG will move.

.PAGE 
 'MOLLWEID requests the mollweid projection. Equal area. Latitudes are
 straight parallel lines. Longitudes converge on the poles. Scale is true
 at latitudes +/- 40 deg 44 min.
        LINE      The output line of latitude 0.
                  Defaults to the center of the output.
        SAMPLE    The output sample of the reference longitude 
                  specified by LONGITUDE.
                  Defaults to the center of the output.
        LATITUDE  not used.
        LONGITUDE The REFERENCE longitude for the projection.
                  Will be a vertical line in the output.
                  Defaults to the center of the input.
        SCALE     The scale in km/pixel at latitude 40 deg 44 min N or S.

.PAGE
 'TMERCATO requests the Transverse mercator projection. Same as Mercator
 except the central meridian is substituted for the equator, permitting both
 poles to be seen. Central meridian, other meridians 90 degrees distant,
 and the equator are straight lines.
        LINE      The output line of the latitude specified by LATITUDE.
                  Defaults to the center of the output.
        SAMPLE    The output sample of the reference longitude 
                  specified by LONGITUDE.
                  Defaults to the center of the output.
        LATITUDE  The latitude placed at LINE in the output.
                  Defaults to the center of the input.
        LONGITUDE The REFERENCE longitude for the projection
                  ( That longitude which maps to a vertical line ).
                  Defaults to the center of the input.
        SCALE     The scale in km/pixel at central meridian.

.VARIABLE SCALE
 SCALE represents the scale, in km/pixel, at the undistorted (see under PROJ)
 part of the projection.  

.VARIABLE LINE
 The LINE parameter specifies the translation of the line-sample space
 in the line direction.  LINE is the line coordinate of the line origin.
 (E.g. for a Lambert projection the pole has a line coordinate given
 by the LINE parameter).
 In AUTO placement mode the LINE parameter specifies the minimum output 
 line of the region.

.VARIABLE SAMPLE
 The SAMPLE parameter specifies the translation of the line-sample space
 in the sample direction.  SAMPLE is the sample coordinate of the sample
 origin.  (E.g. for a Lambert projection the central meridian has a sample
 coordinate given by the SAMPLE parameter).
 In AUTO placement mode the SAMPLE parameter specifies the minimum output 
 sample of the region.

.VARIABLE LATITUDE
 The LATITUDE parameter specifies the special latitude.  See the help
 for the PROJ parameter on the particular use of the parameter for the
 desired map projection.

.VARIABLE LONGITUD
 The LONGITUD parameter specifies the special longitude. See the help
 for the PROJ parameter on the particular use of the parameter for the
 desired map projection.

.VARIABLE PARAL1
 PARAL1 and PARAL2 are used mainly for the Lambert projection.  They are
 the latitudes of the two standard parallels which define the projection.
 This parameter is also used to specify the special longitude in the
 Oblique projections.
.VARIABLE PARAL2
 PARAL2 is used only for the Lambert projection.  They are
 the latitudes of the two standard parallels which define the projection.

.VARIABLE NORTHANG
 NORTHANG is used only for Oblique Stereographic, Oblique Orthographic,
 and Oblique Cylindrical projections.  NORTHANG is the angle in degrees 
 of north in the output space.  This angle is measured in the projection 
 plane at the center of projection clockwise from up. 

.VARIABLE POLE
 POLE is only used for the Polar Orthographic and Polar Stereographic
 projections.  POLE specifies the pole visible in the projection, either
 NORTH or SOUTH.

.VARIABLE PLACEMEN
 PLACEMEN is a keyword the specifies the placement of the lat-long
 in the line-sample space.  The default is for the placement to
 depend on the LINE and SAMPLE parameters.   'AUTOMAT will cause the 
 special line and sample to be calculated so that latitudes and 
 longitudes inside the LATRANGE, LONRANGE box will project to only 
 positive line and samples.   'AUTO placement can only be used for 
 a forward transformation.

.VARIABLE LATRANGE
 LATRANGE specifies the minimum and maximum latitude of the box to be
 used in the 'AUTO placement mode.

.VARIABLE LONRANGE
 LONRANGE specifies the minimum and maximum longitude of the box to be
 used in the 'AUTO placement mode.

.VARIABLE SKIP
 The number of nominal data values to skip over in the graphics-1 file.
 These data values are carried along to the output graphics file
 without modification.

.END
$ Return
$!#############################################################################
$Test_File:
$ create tstpolypmap.pdf
procedure
!  TO RUN ON UNIX OR Alpha, TYPE   tstpolypmap

refgbl $echo
refgbl $syschar
LOCAL DIR    TYPE=STRING 
LOCAL INGR   TYPE=STRING
body
let _onfail="continue"
let $echo="yes"
if ($syschar(1) = "UNIX")
   LET DIR  ="/project/test_work/testdata/sitod1/test_data/images/"
else 
   LET DIR  ="WMS_TEST_WORK:[TESTDATA.SITOD1.TEST_DATA.IMAGES]"
end-if

LET INGR = "&DIR"//"antar.outline"      ! lat,lons of points on Antartica coast
!
! POLAR ORTHOGRAPHIC
polypmap inp=&INGR out=antar.polortho +
 proj=POLORTHO mode=DIRECT planet=EARTH scale=100. line=1 +
 sample=1 latitude=-90. longitud=0 paral1=35.83 paral2=59.17 +
 northang=0 pole=SOUTH placemen=AUTOMAT latrange=(-90,-50) +
 lonrange=(-180,180) skip=0
ibis-list antar.polortho GR1DIM=2 NR=4
!
polypmap inp=&INGR out=antar.polortho +
 proj=POLORTHO mode=DIRECT planet=EARTH scale=100. line=1 +
 sample=1 latitude=-90. longitud=0 paral1=35.83 paral2=59.17 +
 northang=0 pole=SOUTH 
ibis-list antar.polortho GR1DIM=2 NR=4
! TRY 'INVERSE TO GET BACK TO LAT,LON
polypmap inp=antar.polortho out=antar2.polortho +
 proj=POLORTHO 'INVERSE planet=EARTH scale=100. line=1 +
 sample=1 latitude=-90. longitud=0 paral1=35.83 paral2=59.17 +
 northang=0 pole=SOUTH 
!
!
! orthographic
polypmap inp=&INGR out=antar.orthogra +
 proj=orthogra  mode=DIRECT planet=EARTH scale=100. line=1 +
 sample=1 latitude=-80. longitud=0  northang=0 
ibis-list antar.orthogra  GR1DIM=2 NR=4
! TRY 'INVERSE TO GET BACK TO LAT,LON
polypmap inp=antar.orthogra  out=antar2.orthogra  +
 proj=orthogra  'INVERSE planet=EARTH scale=100. line=1 +
 sample=1 latitude=-80. longitud=0  northang=0 
!
!
!polstere 
polypmap inp=&INGR out=antar.polstere +
 proj=polstere  mode=DIRECT planet=EARTH scale=100. line=1 +
 sample=1 latitude=-90. longitud=0  northang=0 pole=SOUTH 
ibis-list antar.polstere  GR1DIM=2 NR=4
! TRY 'INVERSE TO GET BACK TO LAT,LON
polypmap inp=antar.polstere  out=antar2.polstere  +
 proj=polstere  'INVERSE planet=EARTH scale=100. line=1 +
 sample=1 latitude=-90. longitud=0  northang=0 pole=SOUTH 
!
!
! stereogr 
polypmap inp=&INGR out=antar.stereogr +
 proj=stereogr  mode=DIRECT planet=EARTH scale=100. line=1 +
 sample=1 latitude=-70. longitud=0  northang=0 
ibis-list antar.stereogr  GR1DIM=2 NR=4
! TRY 'INVERSE TO GET BACK TO LAT,LON
polypmap inp=antar.stereogr  out=antar2.stereogr  +
 proj=stereogr  'INVERSE planet=EARTH scale=100. line=1 +
 sample=1 latitude=-70. longitud=0  northang=0 
!
!
! mercator 
polypmap inp=&INGR out=antar.mercator +
 proj=mercator  mode=DIRECT planet=EARTH scale=100. +
 latitude=-70. longitud=0  northang=0 
ibis-list antar.mercator  GR1DIM=2 NR=4
! TRY 'INVERSE TO GET BACK TO LAT,LON
polypmap inp=antar.mercator  out=antar2.mercator  +
 proj=mercator  'INVERSE planet=EARTH scale=100.  +
 latitude=-70. longitud=0  northang=0 
!
!
! lambert 
polypmap inp=&INGR out=antar.lambert +
 proj=lambert  mode=DIRECT planet=EARTH scale=100. line=1 +
 sample=1 latitude=-70. longitud=0 paral1=35.83 paral2=59.17 +
 northang=0  
ibis-list antar.lambert  GR1DIM=2 NR=4
! TRY 'INVERSE TO GET BACK TO LAT,LON
polypmap inp=antar.lambert  out=antar2.lambert  +
 proj=lambert  'INVERSE planet=EARTH scale=100. line=1 +
 sample=1 latitude=-70. longitud=0 paral1=35.83 paral2=59.17 +
 northang=0  
!
!
! cylindri 
polypmap inp=&INGR out=antar.cylindri +
 proj=cylindri  mode=DIRECT planet=EARTH scale=100. line=1 +
 sample=1 latitude=-70. longitud=0  northang=0  
ibis-list antar.cylindri  GR1DIM=2 NR=4
! TRY 'INVERSE TO GET BACK TO LAT,LON
polypmap inp=antar.cylindri  out=antar2.cylindri  +
 proj=cylindri  'INVERSE planet=EARTH scale=100. line=1 +
 sample=1 latitude=-70. longitud=0  northang=0  
!
!
! rectangu 
polypmap inp=&INGR out=antar.rectangu +
 proj=rectangu  mode=DIRECT planet=EARTH scale=100. line=1 +
 sample=1 latitude=-70. longitud=0  northang=0  
ibis-list antar.rectangu  GR1DIM=2 NR=4
! TRY 'INVERSE TO GET BACK TO LAT,LON
polypmap inp=antar.rectangu  out=antar2.rectangu  +
 proj=rectangu  'INVERSE planet=EARTH scale=100. line=1 +
 sample=1 latitude=-70. longitud=0  northang=0  
!
!
! oblicyl 
polypmap inp=&INGR out=antar.oblicyl +
 proj=oblicyl  mode=DIRECT planet=EARTH scale=100. line=1 +
 sample=1 latitude=-70. longitud=0 paral1=35.83 +
 northang=0  
ibis-list antar.oblicyl  GR1DIM=2 NR=4
! TRY 'INVERSE TO GET BACK TO LAT,LON
polypmap inp=antar.oblicyl  out=antar2.oblicyl  +
 proj=oblicyl  'INVERSE planet=EARTH scale=100. line=1 +
 sample=1 latitude=-70. longitud=0 paral1=35.83 +
 northang=0  
!
!
! sinusoid 
polypmap inp=&INGR out=antar.sinusoid +
 proj=sinusoid  mode=DIRECT planet=EARTH scale=100. line=1 +
 sample=1 latitude=-70. longitud=0  northang=0  
ibis-list antar.sinusoid  GR1DIM=2 NR=4
! TRY 'INVERSE TO GET BACK TO LAT,LON
polypmap inp=antar.sinusoid  out=antar2.sinusoid  +
 proj=sinusoid  'INVERSE planet=EARTH scale=100. line=1 +
 sample=1 latitude=-70. longitud=0  northang=0  
!
!
! obsinuso 
polypmap inp=&INGR out=antar.obsinuso +
 proj=obsinuso  mode=DIRECT planet=EARTH scale=100. line=1 +
 sample=1 latitude=-70. longitud=0 paral1=35.83 +
 northang=0  
ibis-list antar.obsinuso  GR1DIM=2 NR=4
! TRY 'INVERSE TO GET BACK TO LAT,LON
polypmap inp=antar.obsinuso  out=antar2.obsinuso  +
 proj=obsinuso  'INVERSE planet=EARTH scale=100. line=1 +
 sample=1 latitude=-70. longitud=0 paral1=35.83 +
 northang=0  
!
!
! mollweid 
polypmap inp=&INGR out=antar.mollweid +
 proj=mollweid  mode=DIRECT planet=EARTH scale=100. line=1 +
 sample=1 latitude=-70. longitud=0  northang=0  
ibis-list antar.mollweid  GR1DIM=2 NR=4
! TRY 'INVERSE TO GET BACK TO LAT,LON
polypmap inp=antar.mollweid  out=antar2.mollweid  +
 proj=mollweid  'INVERSE planet=EARTH scale=100. line=1 +
 sample=1 latitude=-70. longitud=0  northang=0  
!
!
! tmercato 
polypmap inp=&INGR out=antar.tmercato +
 proj=tmercato  mode=DIRECT planet=EARTH scale=100. line=1 +
 sample=1 latitude=-70. longitud=0  northang=0  
ibis-list antar.tmercato  GR1DIM=2 NR=4
! TRY 'INVERSE TO GET BACK TO LAT,LON
polypmap inp=antar.tmercato  out=antar2.tmercato  +
 proj=tmercato  'INVERSE planet=EARTH scale=100. line=1 +
 sample=1 latitude=-70. longitud=0  northang=0  
!
!LIST SOME FROM ALL THE LAT LON FILES;  ALL THESE SHOULD BE THE SAME AS INGR.

ibis-list &INGR GR1DIM=2 NR=4
ibis-list antar2.polortho GR1DIM=2 NR=4
ibis-list antar2.orthogra GR1DIM=2 NR=4
ibis-list antar2.polstere GR1DIM=2 NR=4
ibis-list antar2.stereogr GR1DIM=2 NR=4
ibis-list antar2.mercator GR1DIM=2 NR=4
ibis-list antar2.lambert  GR1DIM=2 NR=4
ibis-list antar2.cylindri GR1DIM=2 NR=4
ibis-list antar2.rectangu GR1DIM=2 NR=4
ibis-list antar2.oblicyl  GR1DIM=2 NR=4
ibis-list antar2.sinusoid GR1DIM=2 NR=4
ibis-list antar2.obsinuso GR1DIM=2 NR=4
ibis-list antar2.mollweid GR1DIM=2 NR=4
ibis-list antar2.tmercato GR1DIM=2 NR=4

end-proc
$ Return
$!#############################################################################
