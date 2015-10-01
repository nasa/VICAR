$!****************************************************************************
$!
$! Build proc for MIPL module tristim
$! VPACK Version 1.9, Friday, January 11, 2013, 10:02:40
$!
$! Execute by entering:		$ @tristim
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
$ write sys$output "*** module tristim ***"
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
$ write sys$output "Invalid argument given to tristim.com file -- ", primary
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
$   if F$SEARCH("tristim.imake") .nes. ""
$   then
$      vimake tristim
$      purge tristim.bld
$   else
$      if F$SEARCH("tristim.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake tristim
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @tristim.bld "STD"
$   else
$      @tristim.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create tristim.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack tristim.com -mixed -
	-s tristim.f -
	-i tristim.imake -
	-p tristim.pdf -
	-t tsttristim.pdf tsttristim.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create tristim.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C  02 MAY 1994 ... CRI ... MSTP S/W CONVERSION (VICAR PORTING)
c  11 Jan 2013 -lwk- fixed long CHARACTER continuation line for new compiler flag on Solaris

      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      REAL*4 CMFX(40), CMFY(40), CMFZ(40)
      REAL*4 AX(40), AY(40), AZ(40)
      REAL*4 BX(40), BY(40), BZ(40)
      REAL*4 CX(40), CY(40), CZ(40)
      REAL*4 XENONX(40), XENONY(40), XENONZ(40)
      REAL*4 D55X(40), D55Y(40), D55Z(40)
      REAL*4 D65X(40), D65Y(40), D65Z(40)
      REAL*4 D75X(40), D75Y(40), D75Z(40)
      REAL*4 TX(40),TY(40),TZ(40)
      REAL*4 RAD(40,10),XRAD(40),LAMBDA,CONT(40),BACK(40)
      INTEGER ISET,NSPECT
      CHARACTER*78 MSG1
      CHARACTER*80 MSG2,MSG3,MSG4,MSG5
      LOGICAL XVPTST

      DATA CMFX/.0014,.0042,.0143,.0435,.1344,.2839,.3483,.3362,
     1.2908,.1954,.0956,.032,.0049,.0093,.0633,.1655,.2904,.4334,
     2.5945,.7621,.9163,1.0263,1.0622,1.0026,.8544,.6424,.4479,.2835,
     3.1649,.0874,.0468,.0227,.0114,.0058,.0029,.0014,.0007,.0003,.0002,
     4.0001/
      DATA CMFY/0.,.0001,.0004,.0012,.004,.0116,.023,.038,
     1.06,.091,.139,.208,.323,.503,.71,.862,.954,.995,.995,.952,
     3.87,.757,.631,.503,.381,.265,.175,.107,.061,.032,.017,.0082,
     5.0041,.0021,.001,.0005,.0002,.0001,.0001,0./
      DATA CMFZ/.0065,.0201,.0679,.2074,.6456,1.3856,1.7471,
     11.7721,1.6692,1.2876,.813,.4652,.272,.1582,.0782,.0422,.0203,
     2.0087,.0039,.0021,.0017,.0011,.0008,.0003,.0002,15*0./
      DATA AX/.001,.005,.019,.071,.262,.649,.926,1.031,1.019,
     1.776,.428,.16,.027,.057,.425,1.214,2.313,3.732,5.510,7.571,
     29.719,11.579,12.704,12.669,11.373,8.98,6.558,4.336,2.628,1.448,
     3.804,.404,.209,.11,.057,.028,.014,.006,.004,.002/
      DATA AY/.000,.0,.001,.002,.008,.027,.061,.117,.21,.362,
     1.622,1.039,1.792,3.080,4.771,6.322,7.6,8.568,9.222,9.457,9.228,
     28.54,7.547,6.356,5.071,3.704,2.562,1.637,.972,.53,.292,.146,.075,
     3.04,.019,.01,.006,.002,.002,.0/
      DATA AZ/.006,.023,.093,.34,1.256,3.167,4.647,5.435,5.851,
     15.116,3.636,2.324,1.509,.969,.525,.309,.162,.075,.036,.021,
     2.018,.012,.01,.004,.003,15*0./
      DATA BX/.003,.013,.056,.217,.812,1.983,2.689,2.744,2.454,
     11.718,.87,.295,.044,.081,.541,1.458,2.689,4.183,5.84,
     27.472,8.843,9.728,9.948,9.436,8.14,6.2,4.374,2.815,1.655,.876,
     3.465,.22,.108,.053,.026,.012,.006,.002,.002,.001/
      DATA BY/0.,0.,.002,.006,.024,.081,.178,.31,.506,.8,1.265,
     21.918,2.908,4.36,6.072,7.594,8.834,9.603,9.774,9.334,8.396,
     37.176,5.909,4.734,3.63,2.558,1.709,1.062,.612,.321,.169,.08,
     4.039,.019,.009,.004,.002,.001,.001,0./
      DATA BZ/.014,.06,.268,1.033,3.899,9.678,13.489,14.462,
     214.085,11.319,7.396,4.29,2.449,1.371,.669,.372,.188,.084,.038,
     3.021,.016,.01,.007,.003,.002,15*0./
      DATA CX/.004,.019,.085,.329,1.238,2.997,3.975,3.915,3.362,
     12.272,1.112,.363,.052,.089,.576,1.523,2.785,4.282,5.88,
     37.322,8.417,8.984,8.949,8.325,7.07,5.309,3.693,2.349,1.361,
     4.708,.369,.171,.082,.039,.019,.008,.004,.002,.001,.001/
      DATA CY/0.,0.,.002,.009,.037,.122,.262,.443,.694,1.058,
     11.618,2.358,3.401,4.833,6.462,7.934,9.149,9.832,9.841,
     2 9.147,7.992,6.627,5.316,4.176,3.153,2.19,1.443,.886,.504,.259,
     3.134,.062,.029,.014,.006,.003,.002,.001,.001,.0/
      DATA CZ/.02,.089,.404,1.57,5.949,14.628,19.938,20.638,19.299
     1,14.972,9.461,5.274,2.864,1.52,.712,.388,.195,.086,.039,.02,.016,
     2.01,.007,.002,.002,15*0./
      DATA XENONX/.004,.014,.057,.193,.671,1.506,2.092,2.363,
     12.272,1.895,.803,.270,.040,.078,.553,1.484,2.651,4.045,5.657,
     27.333,9.058,10.257,10.332,9.852,8.768,6.416,4.316,2.848,1.582,
     3.874,.471,.246,.107,.060,.028,.015,.007,.003,.002,.001/
      DATA XENONY/0.,0.,.001,.005,.020,.062,.138,.267,.469,.883,
     11.168,1.756,2.634,4.233,6.203,7.730,8.709,9.287,9.468,9.160,
     28.601,7.565,6.138,4.943,3.910,2.646,1.686,1.075,.586,.320,.171,
     3.089,.039,.022,.009,.005,.002,.001,.001,0./
      DATA XENONZ/.017,.066,.270,.918,3.221,7.349,10.497,12.457,
     113.045,12.489,6.834,3.928,2.219,1.332,.684,.378,.185,.081,.037,
     2.020,.017,.011,.008,.003,.002,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
     30.,0.,0.,0.,0./
      DATA D55X/.004,.015,.083,.284,.915,1.834,2.836,3.135,2.781,
     11.857,.935,.299,.047,.089,.602,1.641,2.821,4.248,5.656,7.048,
     28.517,8.925,9.540,9.071,7.658,5.525,3.933,2.398,1.417,.781,.400,
     3.172,.089,.047,.019,.011,.006,.002,.001,.001/
      DATA D55Y/0.0,0.,.002,.008,.027,.075,.187,.354,.574,.865,
     21.358,1.942,3.095,4.819,6.755,8.546,9.267,9.750,9.467,8.804,
     38.087,6.583,5.667,4.551,3.415,2.279,1.537,.9050,.524,.286,
     4.146,.062,.032,.017,.007,.004,.002,.001,0.,0./
      DATA D55Z/.020,.073,.394,1.354,4.398,8.951,14.228,16.523,
     215.960,12.239,7.943,4.342,2.606,1.516,.744,.418,.197,.086,.037,
     3.019,.015,.010,.007,.003,.002,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
     40.,0.,0.,0.,0./
      DATA D65X/.006,.022,.112,.377,1.188,2.329,3.456,3.722,
     13.242,2.123,1.049,.33,.051,.095,.627,1.686,2.869,4.267,5.625,
     26.947,8.305,8.613,9.047,8.5,7.091,5.063,3.547,2.147,1.252,
     3.68,.346,.15,.077,.041,.017,.01,.005,.002,.001,.001/
      DATA D65Y/0.,.001,.003,.01,.035,.095,.228,.421,.669,
     1.989,1.525,2.142,3.342,5.131,7.04,8.784,9.425,9.796,9.415,
     28.678,7.886,6.353,5.374,4.265,3.162,2.089,1.386,.81,.463,.249,
     3.126,.054,.028,.015,.006,.003,.002,.001,.0,0./
      DATA D65Z/.031,.104,.531,1.795,5.708,11.365,17.336,19.621,
     118.608,13.995,8.917,4.79,2.815,1.614,.776,.43,.201,.086,.037,
     2.019,.015,.009,.007,.003,.002,15*0./
      DATA D75X/.009,.028,.137,.457,1.424,2.749,3.965,4.2,3.617
     1,2.336,1.139,.354,.054,.099,.646,1.716,2.9,4.271,5.584,6.843,
     28.108,8.387,8.7,8.108,6.71,4.749,3.298,1.992,1.151,.619,.315,
     3.136,.069,.037,.015,.009,.004,.002,.001,0./
      DATA D75Y/0.,.001,.004,.013,.042,.112,.262,.475,.746,
     11.088,1.656,2.302,3.538,5.372,7.249,8.939,9.526,9.804,9.346,
     28.549,7.698,6.186,5.168,4.068,2.992,1.959,1.289,.752,.426,
     3.227,.114,.049,.025,.013,.006,.003,.002,.001,0.,0./
      DATA D75Z/.04,.132,.649,2.18,6.84,13.419,19.889,22.139,
     120.759,15.397,9.683,5.147,2.979,1.69,.799,.437,.203,.086,.037,
     2.019,.015,.009,.007,.003,.001,15*0./
      DATA TX/40*0./,TY/40*0./,TZ/40*0./
      DATA ISET/0/
      DATA MSG1/'WAVELENGTH RADIANCE   XBAR      YBAR      ZBAR      R*X       R*Y       R*Z  '/

      CALL IFMESSAGE ('TRISTIM VERSION 11-JAN-2013')
      CALL XVEACTION ('SA',' ')
      IF (XVPTST( 'XENON')) THEN
	CALL XVMESSAGE('XENON ILLUMINANT',' ')
	DO I=1,40
	  TX(I)=XENONX(I)
	  TY(I)=XENONY(I)
	  TZ(I)=XENONZ(I)
	ENDDO
      ENDIF

      IF (XVPTST( 'D55')) THEN
	CALL XVMESSAGE('D55 ILLUMINANT',' ')
	DO I=1,40
	  TX(I)=D55X(I)
	  TY(I)=D55Y(I)
	  TZ(I)=D55Z(I)
	ENDDO
      ENDIF

      IF (XVPTST( 'D65')) THEN
	CALL XVMESSAGE('D65 ILLUMINANT',' ')
	DO I=1,40
	  TX(I)=D65X(I)
	  TY(I)=D65Y(I)
	  TZ(I)=D65Z(I)
	ENDDO
      ENDIF

      IF (XVPTST( 'D75')) THEN
	CALL XVMESSAGE('D75 ILLUMINANT',' ')
	DO I=1,40
	  TX(I)=D75X(I)
	  TY(I)=D75Y(I)
	  TZ(I)=D75Z(I)
	ENDDO
      ENDIF

      IF (XVPTST( 'A')) THEN
	CALLXVMESSAGE('ILLUMINANT A',' ')
	DO I=1,40
	  TX(I)=AX(I)
	  TY(I)=AY(I)
	  TZ(I)=AZ(I)
	ENDDO
      ENDIF

      IF (XVPTST( 'B')) THEN
	CALL XVMESSAGE('ILLUMINANT B',' ')
	DO I=1,40
	  TX(I)=BX(I)
	  TY(I)=BY(I)
	  TZ(I)=BZ(I)
	ENDDO
      ENDIF

      IF (XVPTST( 'C')) THEN
	CALL XVMESSAGE('ILLUMINANT C',' ')
	DO I=1,40
	  TX(I)=CX(I)
	  TY(I)=CY(I)
	  TZ(I)=CZ(I)
	ENDDO
      ENDIF

C     COLOR MATCHING FUNCTIONS
C     USER INPUTS SPECTRAL REFLECTANCE (TRANSMITTANCE) TIMES
C     SPECTRAL POWER
      IF (XVPTST( 'CMF')) THEN
	CALL XVMESSAGE('COLOR MATCHING FUNCTION',' ')
	DO I=1,40
	  TX(I)=CMFX(I)
	  TY(I)=CMFY(I)
	  TZ(I)=CMFZ(I)
	ENDDO
      ENDIF

C RADIANCES:

      NSPECT = 1	! DEFAULT

      CALL XVPARM( 'RADIANCE', RAD, ICNT, IDEF, 400) ! RADIANCES IN ALL SPECTRA
      IF (ICNT.EQ.0) THEN
	DO I = 1,40
	  RAD( I, 1) = 1.
	ENDDO
      ELSE
	NSPECT = ICNT/40
	IF (ICNT.NE.40*NSPECT) THEN
	  CALL XVMESSAGE
     &         ('** MUST SPECIFY 40 RADIANCES PER SPECTRUM **',' ')
	  CALL ABEND
	ENDIF
      ENDIF

      CALL XVPARM( 'ADD', CONST, ICNT, IDEF, 1)  ! ADD A CONSTANT TO RADIANCES
      IF (ICNT.GT.0) ISET = 1

      CALL XVPARM( 'MULT', CONSTM, ICNT, IDEF, 1) ! MULT RADIANCES BY CONSTANT
      IF (ICNT.GT.0) ISET = 1

C MULTIPLY RADIANCES TOGETHER
      DO J=1,40
	XRAD(J)=1.
	DO I=1,NSPECT
	  XRAD(J)=RAD(J,I)*XRAD(J)
	ENDDO
      ENDDO
C ADJUST RADIANCES VALUES
      DO I=1,40
        XRAD(I) = XRAD(I)*CONSTM + CONST
      ENDDO

C  OPTION TO ENTER "ABSOLUTE" INTENSITY VALUES NEEDS CONTINUUM &
C  BACKGROUND VALUES:

      CALL XVPARM( 'CONTIN', CONT, ICNT, IDEF, 40)
      IF (ICNT.GT.0) THEN
	CALL XVPARM( 'BACK', BACK, ICNT, IDEF, 40)
	IF (ICNT.LE.1) THEN
	  DO I=2,40
	    BACK(I) = BACK(1)
	  ENDDO
	ENDIF
	DO I=1,40
	  XRAD(I) = (XRAD(I)-BACK(I))/CONT(I)
	ENDDO
      ENDIF

C  PRINT OUT THE RADIANCES

      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE(MSG1,' ')

      LAMBDA=380.
      RADSUM=0.
      XSUM=0.
      YSUM=0.
      ZSUM=0.
      DO 300 I=1,40
      X=TX(I)*XRAD(I)
      Y=TY(I)*XRAD(I)
      Z=TZ(I)*XRAD(I)
      XSUM=X+XSUM
      YSUM=Y+YSUM
      ZSUM=Z+ZSUM

      WRITE (MSG2,9900) NINT(LAMBDA),XRAD(I),TX(I),TY(I),TZ(I),X,Y,Z
9900  FORMAT ('   ',I6,F10.5,F10.4,F10.4,F10.4,F10.5,F10.5,F10.5)
      CALL XVMESSAGE(MSG2,' ')
      LAMBDA=LAMBDA+10.
      RADSUM=RADSUM+XRAD(I)
  300 CONTINUE
      WRITE (MSG3,9910) CONSTM,CONST
9910  FORMAT ('OUTPUT RADIANCE=INPUT*',F10.5,'  +     ',F5.2)
      IF(ISET.EQ.1) CALL XVMESSAGE(' ',' ')
      IF(ISET.EQ.1) CALL XVMESSAGE(MSG3,' ')

C  PRINT OUT TRISTIMULUS VALUES

      TOTAL = XSUM + YSUM + ZSUM
      XCHROM=XSUM/TOTAL
      YCHROM=YSUM/TOTAL
      ZCHROM=ZSUM/TOTAL
      WRITE (MSG4,9920) XSUM,YSUM,ZSUM
9920  FORMAT ('X TRISTIM = ',F10.6,' Y TRISTIM =',F10.6,'  Z TRISTIM ',
     +F10.6,'  ')
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE(MSG4,' ')
      WRITE (MSG5,9930) XCHROM,YCHROM,ZCHROM
9930  FORMAT ('X CHROM  =  ',F10.8,' Y CHROM   =',F10.8,'  Z CHROM   ',
     +F10.8,'  ')
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE(MSG5,' ')
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE('TRISTIM END',' ')
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create tristim.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM tristim

   To Create the build file give the command:

		$ vimake tristim			(VMS)
   or
		% vimake tristim			(Unix)


************************************************************************/


#define PROGRAM	tristim
#define R2LIB

#define MODULE_LIST tristim.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create tristim.pdf
process help=*
PARM ILLUMIN  KEYWORD VALID=(XENON,D55,D65,D75,A,B,C,CMF) DEFAULT=XENON
PARM RADIANCE REAL    COUNT=0:400			  DEFAULT=--
PARM ADD      REAL 					  DEFAULT=0.
PARM MULT     REAL					  DEFAULT=1.
PARM CONTIN   REAL    COUNT=(0,40)			  DEFAULT=--
PARM BACK     REAL    COUNT=(1:40)                        DEFAULT=0.
!PARM BACK REAL COUNT=(1,40) DEFAULT=0.  !!not legal under TAE??
end-proc
.TITLE
VICAR1 Program TRISTIM
.HELP
"tristim" is a Vicar applications program that will calculate the tristimulus
values and chromaticity coordinates for a given spectrum, which may be of
intensity, reflectivity, or transmittivity. Tristimulus values and chromaticity
coordinates are quantities which must be known to do color reconstruction.  
For a complete discussion of colorimetry, see Reference 1.
 
EXECUTION:

tristim PARAMS

where PARAMS are parameters listed in Tutor mode.  (There are no input or
output datasets.)
.page
OPERATION:
 
"tristim" computes tristimulus values for a spectrum by the formulas:
 
  X = k * SUM( F(i) * x'(i) )
  Y = k * SUM( F(i) * y'(i) )
  Z = k * SUM( F(i) * z'(i) )
 
where:   X, Y, and Z are the tristimulus values,
 
         F is the input spectral distribution of intensity, reflectivity,
         or transmittivity.
 
         x', y', and z' are the C.I.E. standard colorimetric observer
         values, also known as the color matching functions, optionally
         weighted by a specified illuminant,
 
         and i is the wavelength.
 
The sums are taken from i = 380 nm to 770 nm.

The constant k is defined by the relation:

                    100
         k = -------------------
             SUM( S(i) * y'(i) )

         where S(i) is the spectral radiance of the illuminant.
 
For standard illuminants D55, D65, D75, A, B, and C, the color matching
functions have been weighted by the relative spectral power distributions
of these illuminants in such a way that k=1.  The XENON illuminant
weighting functions have also been computed so that k=1.  However, XENON
is not a standard of the C.I.E. and refers only to the JPL 12-inch light
cannon on 13 April 1976 (see Reference 2).
 
If the user is using some other illuminant, k will have to be computed
separately.  This can easily be done with another execution of TRISTIM,
inputting the spectral radiance of the illuminant for the 40 radiance
values, and then taking the ratio of 100 and the Y tristimulus value.
 
"tristim" also outputs the chromaticity coordinates of the spectrum in
question.  These are related by the equations:
 
          X                  Y                  Z
    x = ----- ,        y = ----- ,        z = ----- .
        X+Y+Z              X+Y+Z              X+Y+Z
.page
The user-supplied input spectrum (RADIANCE) may be specified as a multiple 
of up to ten separate spectral distributions, of exactly 40 points each 
(R1,...,R400). This can represent a series of filters through which light 
passes.
 
The RADIANCE values R1,...,R40 can also be modified (for whatever reason) 
by the equation:
 
   NEW_RADIANCE(i) = Ri * MULT + ADD

where MULT and ADD are the values specified for the parameters of those
names.
.page
In addition, the user may specify Continuum and Background spectra, CONTIN
and BACK, which will be applied to the input spectrum according to the
following formula:

                        (Ri - BACK(i))
   NEW_RADIANCE(i) = -------------------
                     (CONTIN(i) - BACK(i))

This can be useful to process spectrophotometric tracings of inputs in
certain colors and of a white reference input.
.page
REFERENCES

1. Judd, D.B., and G, Wyszecki, "Color in Business, Science, and Industry",
   Third Edition, 1975, John Wiley and Sons.
 
2. JPL IOM 26 April 1976, To: L. Snyder, From: M. Benesh, New Photometric
   Characteristics of 12" (30-cm) Light Cannon No. 1.
 
 
TIMING
 
TRISTIM takes about 4 CPU seconds to run.
.PAGE 
COGNIZANT PROGRAMMER
 
Written by:  Joel Mosher,  1 Oct. 1977
 
Converted to VAX by:  L. W. Kamp,  10 Jan. 1984
 
Current Cognizant Programmer:  L. W. Kamp

Made portable for UNIX   Alan Scop (CRI) 2 May 1994

.LEVEL1
.vari illumin
KEYWORD: Valid = D55, D65,
D75, A, B, C, CMF, XENON.
.vari radiance
List of Radiances
.vari add
Constant added to RADIANCE.
.vari mult
Constant by which RADIANCE
is multiplied.
.vari CONTIN
Continuum reference spectrum
.vari BACK
Background spectrum
.LEVEL2
.vari illumin
ILLUMINANT: specifies the illuminant by whose spectral radiance the C.I.E.
color matching functions are weighted.
 
Valid values are: D55, D65, D75, A, B, C, XENON, CMF.
 
The first six of these (D55 thru C) are C.I.E. standard illuminants.
Note that D55, D65, and D75 refer to blackbody curves (Planck function)
at 5500, 6500, and 7500 Kelvins, respectively.

XENON specifies that the illuminant is the JPL 12-inch light cannon No. 1.
 
CMF specifies that the unweighted C.I.E. color matching functions are to be
used.
.vari radiance
RADIANCE:  List of numbers which specify the spectral radiance or reflectivity
or transmittivity of the object under study.  
 
This spectrum must be specified at exactly 40 points, starting at 380 nm 
and ending at 770 nm.  Up to 10 separate spectra may be specified, each of
40 points, which will be multiplied together inside the program to obtain
the final spectral distribution.  These separate spectra are simple entered
sequentially under this parameter.  E.g., if RADIANCE contains 80 numbers,
of which the first 40 are 2.0 and the next 40 are 5.0, then the resulting
spectrum values are 10.0 at all 40 points.
 
Note that the user may also cause a linear transformation to be applied to
the input spectrum, using the ADD and MULT parameters.
.vari add
ADD: This is a constant that will be added to each of the 40 spectral 
values specified by the RADIANCE parameter, before computing the tristimulus
values.
.vari mult
MULT: This is a constant that will be multiplied with each of the 40 spectral 
values specified by the RADIANCE parameter, before computing the tristimulus
values.
.vari contin
This specifies 40 points that will be used as a reference (continuum) spectrum
to compute relative radiance values from the values input in RADIANCE.

If this parameter is defaulted, then no relative radiance computation will
be done.
.vari back
This specifies 40 points that will be used a background spectrum if the
parameter CONTIN has been specified (if not, then this parameter is
ignored).

If only one value is specified, then that value will be used for all 40
points.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tsttristim.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
!
!  test with defaults only:
tristim
!
!  test parm ILLUM:
tristim 'D55
tristim 'A
!
!  test parm RADIANCE with ADD & MULT:
tristim 'CMF ADD=1.0 MULT=0.5 RADI=(+
.2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2+
.2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2+
.4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4+
.4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4)
!
!  test parm RADIANCE with CONTIN & BACK:
tristim 'XENON RADIANCE=( +
 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, +
 42, 42, 42, 47, 59, 83, 125, 181, 245, 308, 364, 418, 470, 510, 547, 580, +
 603, 620, 633, 643, 652, 661, 665, 673, 677, 682) +
 BACK=22 +
 CONT=( 700, 700, 700, 692, 700, 712, 720, 725, 733, 730, 728, 740, 739, 740, +
 740, 740, 739, 738, 741, 739, 738, 738, 738, 737, 736, 734, 734, 733, +
 731, 737, 737, 738, 738, 738, 736, 736, 735, 735, 735, 733)
end-proc
$!-----------------------------------------------------------------------------
$ create tsttristim.log
tsttristim
tristim
Beginning VICAR task tristim
TRISTIM VERSION 11-JAN-2013
XENON ILLUMINANT

WAVELENGTH RADIANCE   XBAR      YBAR      ZBAR      R*X       R*Y       R*Z
      380   1.00000    0.0040    0.0000    0.0170   0.00400   0.00000   0.01700
      390   1.00000    0.0140    0.0000    0.0660   0.01400   0.00000   0.06600
      400   1.00000    0.0570    0.0010    0.2700   0.05700   0.00100   0.27000
      410   1.00000    0.1930    0.0050    0.9180   0.19300   0.00500   0.91800
      420   1.00000    0.6710    0.0200    3.2210   0.67100   0.02000   3.22100
      430   1.00000    1.5060    0.0620    7.3490   1.50600   0.06200   7.34900
      440   1.00000    2.0920    0.1380   10.4970   2.09200   0.13800  10.49700
      450   1.00000    2.3630    0.2670   12.4570   2.36300   0.26700  12.45700
      460   1.00000    2.2720    0.4690   13.0450   2.27200   0.46900  13.04500
      470   1.00000    1.8950    0.8830   12.4890   1.89500   0.88300  12.48900
      480   1.00000    0.8030    1.1680    6.8340   0.80300   1.16800   6.83400
      490   1.00000    0.2700    1.7560    3.9280   0.27000   1.75600   3.92800
      500   1.00000    0.0400    2.6340    2.2190   0.04000   2.63400   2.21900
      510   1.00000    0.0780    4.2330    1.3320   0.07800   4.23300   1.33200
      520   1.00000    0.5530    6.2030    0.6840   0.55300   6.20300   0.68400
      530   1.00000    1.4840    7.7300    0.3780   1.48400   7.73000   0.37800
      540   1.00000    2.6510    8.7090    0.1850   2.65100   8.70900   0.18500
      550   1.00000    4.0450    9.2870    0.0810   4.04500   9.28700   0.08100
      560   1.00000    5.6570    9.4680    0.0370   5.65700   9.46800   0.03700
      570   1.00000    7.3330    9.1600    0.0200   7.33300   9.16000   0.02000
      580   1.00000    9.0580    8.6010    0.0170   9.05800   8.60100   0.01700
      590   1.00000   10.2570    7.5650    0.0110  10.25700   7.56500   0.01100
      600   1.00000   10.3320    6.1380    0.0080  10.33200   6.13800   0.00800
      610   1.00000    9.8520    4.9430    0.0030   9.85200   4.94300   0.00300
      620   1.00000    8.7680    3.9100    0.0020   8.76800   3.91000   0.00200
      630   1.00000    6.4160    2.6460    0.0000   6.41600   2.64600   0.00000
      640   1.00000    4.3160    1.6860    0.0000   4.31600   1.68600   0.00000
      650   1.00000    2.8480    1.0750    0.0000   2.84800   1.07500   0.00000
      660   1.00000    1.5820    0.5860    0.0000   1.58200   0.58600   0.00000
      670   1.00000    0.8740    0.3200    0.0000   0.87400   0.32000   0.00000
      680   1.00000    0.4710    0.1710    0.0000   0.47100   0.17100   0.00000
      690   1.00000    0.2460    0.0890    0.0000   0.24600   0.08900   0.00000
      700   1.00000    0.1070    0.0390    0.0000   0.10700   0.03900   0.00000
      710   1.00000    0.0600    0.0220    0.0000   0.06000   0.02200   0.00000
      720   1.00000    0.0280    0.0090    0.0000   0.02800   0.00900   0.00000
      730   1.00000    0.0150    0.0050    0.0000   0.01500   0.00500   0.00000
      740   1.00000    0.0070    0.0020    0.0000   0.00700   0.00200   0.00000
      750   1.00000    0.0030    0.0010    0.0000   0.00300   0.00100   0.00000
      760   1.00000    0.0020    0.0010    0.0000   0.00200   0.00100   0.00000
      770   1.00000    0.0010    0.0000    0.0000   0.00100   0.00000   0.00000

OUTPUT RADIANCE=INPUT*   1.00000  +      0.00

X TRISTIM =  99.223999 Y TRISTIM =100.001991  Z TRISTIM  76.067993

X CHROM  =  0.36042923 Y CHROM   =0.36325529  Z CHROM   0.27631551

TRISTIM END
tristim 'D55
Beginning VICAR task tristim
TRISTIM VERSION 11-JAN-2013
D55 ILLUMINANT

WAVELENGTH RADIANCE   XBAR      YBAR      ZBAR      R*X       R*Y       R*Z
      380   1.00000    0.0040    0.0000    0.0200   0.00400   0.00000   0.02000
      390   1.00000    0.0150    0.0000    0.0730   0.01500   0.00000   0.07300
      400   1.00000    0.0830    0.0020    0.3940   0.08300   0.00200   0.39400
      410   1.00000    0.2840    0.0080    1.3540   0.28400   0.00800   1.35400
      420   1.00000    0.9150    0.0270    4.3980   0.91500   0.02700   4.39800
      430   1.00000    1.8340    0.0750    8.9510   1.83400   0.07500   8.95100
      440   1.00000    2.8360    0.1870   14.2280   2.83600   0.18700  14.22800
      450   1.00000    3.1350    0.3540   16.5230   3.13500   0.35400  16.52300
      460   1.00000    2.7810    0.5740   15.9600   2.78100   0.57400  15.96000
      470   1.00000    1.8570    0.8650   12.2390   1.85700   0.86500  12.23900
      480   1.00000    0.9350    1.3580    7.9430   0.93500   1.35800   7.94300
      490   1.00000    0.2990    1.9420    4.3420   0.29900   1.94200   4.34200
      500   1.00000    0.0470    3.0950    2.6060   0.04700   3.09500   2.60600
      510   1.00000    0.0890    4.8190    1.5160   0.08900   4.81900   1.51600
      520   1.00000    0.6020    6.7550    0.7440   0.60200   6.75500   0.74400
      530   1.00000    1.6410    8.5460    0.4180   1.64100   8.54600   0.41800
      540   1.00000    2.8210    9.2670    0.1970   2.82100   9.26700   0.19700
      550   1.00000    4.2480    9.7500    0.0860   4.24800   9.75000   0.08600
      560   1.00000    5.6560    9.4670    0.0370   5.65600   9.46700   0.03700
      570   1.00000    7.0480    8.8040    0.0190   7.04800   8.80400   0.01900
      580   1.00000    8.5170    8.0870    0.0150   8.51700   8.08700   0.01500
      590   1.00000    8.9250    6.5830    0.0100   8.92500   6.58300   0.01000
      600   1.00000    9.5400    5.6670    0.0070   9.54000   5.66700   0.00700
      610   1.00000    9.0710    4.5510    0.0030   9.07100   4.55100   0.00300
      620   1.00000    7.6580    3.4150    0.0020   7.65800   3.41500   0.00200
      630   1.00000    5.5250    2.2790    0.0000   5.52500   2.27900   0.00000
      640   1.00000    3.9330    1.5370    0.0000   3.93300   1.53700   0.00000
      650   1.00000    2.3980    0.9050    0.0000   2.39800   0.90500   0.00000
      660   1.00000    1.4170    0.5240    0.0000   1.41700   0.52400   0.00000
      670   1.00000    0.7810    0.2860    0.0000   0.78100   0.28600   0.00000
      680   1.00000    0.4000    0.1460    0.0000   0.40000   0.14600   0.00000
      690   1.00000    0.1720    0.0620    0.0000   0.17200   0.06200   0.00000
      700   1.00000    0.0890    0.0320    0.0000   0.08900   0.03200   0.00000
      710   1.00000    0.0470    0.0170    0.0000   0.04700   0.01700   0.00000
      720   1.00000    0.0190    0.0070    0.0000   0.01900   0.00700   0.00000
      730   1.00000    0.0110    0.0040    0.0000   0.01100   0.00400   0.00000
      740   1.00000    0.0060    0.0020    0.0000   0.00600   0.00200   0.00000
      750   1.00000    0.0020    0.0010    0.0000   0.00200   0.00100   0.00000
      760   1.00000    0.0010    0.0000    0.0000   0.00100   0.00000   0.00000
      770   1.00000    0.0010    0.0000    0.0000   0.00100   0.00000   0.00000

OUTPUT RADIANCE=INPUT*   1.00000  +      0.00

X TRISTIM =  95.642982 Y TRISTIM =100.000000  Z TRISTIM  92.085007

X CHROM  =  0.33240768 Y CHROM   =0.34755051  Z CHROM   0.32004189

TRISTIM END
tristim 'A
Beginning VICAR task tristim
TRISTIM VERSION 11-JAN-2013
ILLUMINANT A

WAVELENGTH RADIANCE   XBAR      YBAR      ZBAR      R*X       R*Y       R*Z
      380   1.00000    0.0010    0.0000    0.0060   0.00100   0.00000   0.00600
      390   1.00000    0.0050    0.0000    0.0230   0.00500   0.00000   0.02300
      400   1.00000    0.0190    0.0010    0.0930   0.01900   0.00100   0.09300
      410   1.00000    0.0710    0.0020    0.3400   0.07100   0.00200   0.34000
      420   1.00000    0.2620    0.0080    1.2560   0.26200   0.00800   1.25600
      430   1.00000    0.6490    0.0270    3.1670   0.64900   0.02700   3.16700
      440   1.00000    0.9260    0.0610    4.6470   0.92600   0.06100   4.64700
      450   1.00000    1.0310    0.1170    5.4350   1.03100   0.11700   5.43500
      460   1.00000    1.0190    0.2100    5.8510   1.01900   0.21000   5.85100
      470   1.00000    0.7760    0.3620    5.1160   0.77600   0.36200   5.11600
      480   1.00000    0.4280    0.6220    3.6360   0.42800   0.62200   3.63600
      490   1.00000    0.1600    1.0390    2.3240   0.16000   1.03900   2.32400
      500   1.00000    0.0270    1.7920    1.5090   0.02700   1.79200   1.50900
      510   1.00000    0.0570    3.0800    0.9690   0.05700   3.08000   0.96900
      520   1.00000    0.4250    4.7710    0.5250   0.42500   4.77100   0.52500
      530   1.00000    1.2140    6.3220    0.3090   1.21400   6.32200   0.30900
      540   1.00000    2.3130    7.6000    0.1620   2.31300   7.60000   0.16200
      550   1.00000    3.7320    8.5680    0.0750   3.73200   8.56800   0.07500
      560   1.00000    5.5100    9.2220    0.0360   5.51000   9.22200   0.03600
      570   1.00000    7.5710    9.4570    0.0210   7.57100   9.45700   0.02100
      580   1.00000    9.7190    9.2280    0.0180   9.71900   9.22800   0.01800
      590   1.00000   11.5790    8.5400    0.0120  11.57900   8.54000   0.01200
      600   1.00000   12.7040    7.5470    0.0100  12.70400   7.54700   0.01000
      610   1.00000   12.6690    6.3560    0.0040  12.66900   6.35600   0.00400
      620   1.00000   11.3730    5.0710    0.0030  11.37300   5.07100   0.00300
      630   1.00000    8.9800    3.7040    0.0000   8.98000   3.70400   0.00000
      640   1.00000    6.5580    2.5620    0.0000   6.55800   2.56200   0.00000
      650   1.00000    4.3360    1.6370    0.0000   4.33600   1.63700   0.00000
      660   1.00000    2.6280    0.9720    0.0000   2.62800   0.97200   0.00000
      670   1.00000    1.4480    0.5300    0.0000   1.44800   0.53000   0.00000
      680   1.00000    0.8040    0.2920    0.0000   0.80400   0.29200   0.00000
      690   1.00000    0.4040    0.1460    0.0000   0.40400   0.14600   0.00000
      700   1.00000    0.2090    0.0750    0.0000   0.20900   0.07500   0.00000
      710   1.00000    0.1100    0.0400    0.0000   0.11000   0.04000   0.00000
      720   1.00000    0.0570    0.0190    0.0000   0.05700   0.01900   0.00000
      730   1.00000    0.0280    0.0100    0.0000   0.02800   0.01000   0.00000
      740   1.00000    0.0140    0.0060    0.0000   0.01400   0.00600   0.00000
      750   1.00000    0.0060    0.0020    0.0000   0.00600   0.00200   0.00000
      760   1.00000    0.0040    0.0020    0.0000   0.00400   0.00200   0.00000
      770   1.00000    0.0020    0.0000    0.0000   0.00200   0.00000   0.00000

OUTPUT RADIANCE=INPUT*   1.00000  +      0.00

X TRISTIM = 109.827988 Y TRISTIM = 99.999992  Z TRISTIM  35.546997

X CHROM  =  0.44759247 Y CHROM   =0.40753949  Z CHROM   0.14486806

TRISTIM END
tristim 'CMF ADD=1.0 MULT=0.5 RADI=( +
.2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 +
.2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 +
.4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 +
.4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4)
Beginning VICAR task tristim
TRISTIM VERSION 11-JAN-2013
COLOR MATCHING FUNCTION

WAVELENGTH RADIANCE   XBAR      YBAR      ZBAR      R*X       R*Y       R*Z
      380   1.04000    0.0014    0.0000    0.0065   0.00146   0.00000   0.00676
      390   1.04000    0.0042    0.0001    0.0201   0.00437   0.00010   0.02090
      400   1.04000    0.0143    0.0004    0.0679   0.01487   0.00042   0.07062
      410   1.04000    0.0435    0.0012    0.2074   0.04524   0.00125   0.21570
      420   1.04000    0.1344    0.0040    0.6456   0.13978   0.00416   0.67142
      430   1.04000    0.2839    0.0116    1.3856   0.29526   0.01206   1.44102
      440   1.04000    0.3483    0.0230    1.7471   0.36223   0.02392   1.81698
      450   1.04000    0.3362    0.0380    1.7721   0.34965   0.03952   1.84298
      460   1.04000    0.2908    0.0600    1.6692   0.30243   0.06240   1.73597
      470   1.04000    0.1954    0.0910    1.2876   0.20322   0.09464   1.33910
      480   1.04000    0.0956    0.1390    0.8130   0.09942   0.14456   0.84552
      490   1.04000    0.0320    0.2080    0.4652   0.03328   0.21632   0.48381
      500   1.04000    0.0049    0.3230    0.2720   0.00510   0.33592   0.28288
      510   1.04000    0.0093    0.5030    0.1582   0.00967   0.52312   0.16453
      520   1.04000    0.0633    0.7100    0.0782   0.06583   0.73840   0.08133
      530   1.04000    0.1655    0.8620    0.0422   0.17212   0.89648   0.04389
      540   1.04000    0.2904    0.9540    0.0203   0.30202   0.99216   0.02111
      550   1.04000    0.4334    0.9950    0.0087   0.45074   1.03480   0.00905
      560   1.04000    0.5945    0.9950    0.0039   0.61828   1.03480   0.00406
      570   1.04000    0.7621    0.9520    0.0021   0.79258   0.99008   0.00218
      580   1.04000    0.9163    0.8700    0.0017   0.95295   0.90480   0.00177
      590   1.04000    1.0263    0.7570    0.0011   1.06735   0.78728   0.00114
      600   1.04000    1.0622    0.6310    0.0008   1.10469   0.65624   0.00083
      610   1.04000    1.0026    0.5030    0.0003   1.04270   0.52312   0.00031
      620   1.04000    0.8544    0.3810    0.0002   0.88858   0.39624   0.00021
      630   1.04000    0.6424    0.2650    0.0000   0.66810   0.27560   0.00000
      640   1.04000    0.4479    0.1750    0.0000   0.46582   0.18200   0.00000
      650   1.04000    0.2835    0.1070    0.0000   0.29484   0.11128   0.00000
      660   1.04000    0.1649    0.0610    0.0000   0.17150   0.06344   0.00000
      670   1.04000    0.0874    0.0320    0.0000   0.09090   0.03328   0.00000
      680   1.04000    0.0468    0.0170    0.0000   0.04867   0.01768   0.00000
      690   1.04000    0.0227    0.0082    0.0000   0.02361   0.00853   0.00000
      700   1.04000    0.0114    0.0041    0.0000   0.01186   0.00426   0.00000
      710   1.04000    0.0058    0.0021    0.0000   0.00603   0.00218   0.00000
      720   1.04000    0.0029    0.0010    0.0000   0.00302   0.00104   0.00000
      730   1.04000    0.0014    0.0005    0.0000   0.00146   0.00052   0.00000
      740   1.04000    0.0007    0.0002    0.0000   0.00073   0.00021   0.00000
      750   1.04000    0.0003    0.0001    0.0000   0.00031   0.00010   0.00000
      760   1.04000    0.0002    0.0001    0.0000   0.00021   0.00010   0.00000
      770   1.04000    0.0001    0.0000    0.0000   0.00010   0.00000   0.00000

OUTPUT RADIANCE=INPUT*   0.50000  +      1.00

X TRISTIM =  11.110942 Y TRISTIM = 11.113025  Z TRISTIM  11.104080

X CHROM  =  0.33338112 Y CHROM   =0.33344361  Z CHROM   0.33317524

TRISTIM END
tristim 'XENON RADIANCE=(  +
 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22,  +
 42, 42, 42, 47, 59, 83, 125, 181, 245, 308, 364, 418, 470, 510, 547, 580,  +
 603, 620, 633, 643, 652, 661, 665, 673, 677, 682)  +
 BACK=22  +
 CONT=( 700, 700, 700, 692, 700, 712, 720, 725, 733, 730, 728, 740, 739, 740,  +
 740, 740, 739, 738, 741, 739, 738, 738, 738, 737, 736, 734, 734, 733,  +
 731, 737, 737, 738, 738, 738, 736, 736, 735, 735, 735, 733)
Beginning VICAR task tristim
TRISTIM VERSION 11-JAN-2013
XENON ILLUMINANT

WAVELENGTH RADIANCE   XBAR      YBAR      ZBAR      R*X       R*Y       R*Z
      380   0.00000    0.0040    0.0000    0.0170   0.00000   0.00000   0.00000
      390   0.00000    0.0140    0.0000    0.0660   0.00000   0.00000   0.00000
      400   0.00000    0.0570    0.0010    0.2700   0.00000   0.00000   0.00000
      410   0.00000    0.1930    0.0050    0.9180   0.00000   0.00000   0.00000
      420   0.00000    0.6710    0.0200    3.2210   0.00000   0.00000   0.00000
      430   0.00000    1.5060    0.0620    7.3490   0.00000   0.00000   0.00000
      440   0.00000    2.0920    0.1380   10.4970   0.00000   0.00000   0.00000
      450   0.00000    2.3630    0.2670   12.4570   0.00000   0.00000   0.00000
      460   0.00000    2.2720    0.4690   13.0450   0.00000   0.00000   0.00000
      470   0.00000    1.8950    0.8830   12.4890   0.00000   0.00000   0.00000
      480   0.00000    0.8030    1.1680    6.8340   0.00000   0.00000   0.00000
      490   0.00000    0.2700    1.7560    3.9280   0.00000   0.00000   0.00000
      500   0.00000    0.0400    2.6340    2.2190   0.00000   0.00000   0.00000
      510   0.00000    0.0780    4.2330    1.3320   0.00000   0.00000   0.00000
      520   0.02703    0.5530    6.2030    0.6840   0.01495   0.16765   0.01849
      530   0.02703    1.4840    7.7300    0.3780   0.04011   0.20892   0.01022
      540   0.02706    2.6510    8.7090    0.1850   0.07175   0.23570   0.00501
      550   0.03388    4.0450    9.2870    0.0810   0.13703   0.31460   0.00274
      560   0.04993    5.6570    9.4680    0.0370   0.28247   0.47276   0.00185
      570   0.08254    7.3330    9.1600    0.0200   0.60530   0.75610   0.00165
      580   0.13957    9.0580    8.6010    0.0170   1.26419   1.20041   0.00237
      590   0.21545   10.2570    7.5650    0.0110   2.20984   1.62986   0.00237
      600   0.30217   10.3320    6.1380    0.0080   3.12200   1.85471   0.00242
      610   0.38806    9.8520    4.9430    0.0030   3.82316   1.91818   0.00116
      620   0.46467    8.7680    3.9100    0.0020   4.07426   1.81687   0.00093
      630   0.53951    6.4160    2.6460    0.0000   3.46149   1.42754   0.00000
      640   0.61035    4.3160    1.6860    0.0000   2.63429   1.02906   0.00000
      650   0.66576    2.8480    1.0750    0.0000   1.89608   0.71569   0.00000
      660   0.71819    1.5820    0.5860    0.0000   1.13618   0.42086   0.00000
      670   0.75712    0.8740    0.3200    0.0000   0.66173   0.24228   0.00000
      680   0.78833    0.4710    0.1710    0.0000   0.37130   0.13480   0.00000
      690   0.81030    0.2460    0.0890    0.0000   0.19933   0.07212   0.00000
      700   0.82791    0.1070    0.0390    0.0000   0.08859   0.03229   0.00000
      710   0.84146    0.0600    0.0220    0.0000   0.05049   0.01851   0.00000
      720   0.85598    0.0280    0.0090    0.0000   0.02397   0.00770   0.00000
      730   0.86821    0.0150    0.0050    0.0000   0.01302   0.00434   0.00000
      740   0.87483    0.0070    0.0020    0.0000   0.00612   0.00175   0.00000
      750   0.88571    0.0030    0.0010    0.0000   0.00266   0.00089   0.00000
      760   0.89116    0.0020    0.0010    0.0000   0.00178   0.00089   0.00000
      770   0.90041    0.0010    0.0000    0.0000   0.00090   0.00000   0.00000

OUTPUT RADIANCE=INPUT*   1.00000  +      0.00

X TRISTIM =  26.192980 Y TRISTIM = 14.684481  Z TRISTIM   0.049205

X CHROM  =  0.63999790 Y CHROM   =0.35879982  Z CHROM   0.00120228

TRISTIM END
end-proc
exit
slogoff
if ($RUNTYPE = "INTERACTIVE")
  if ($syschar(1) = "VAX_VMS")
  end-if
else
  if ($syschar(1) = "VAX_VMS")
  end-if
end-if
ulogoff
END-PROC
END-PROC
$ Return
$!#############################################################################
