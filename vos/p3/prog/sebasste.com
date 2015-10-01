$!****************************************************************************
$!
$! Build proc for MIPL module sebasste
$! VPACK Version 1.8, Tuesday, March 17, 1998, 23:48:37
$!
$! Execute by entering:		$ @sebasste
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
$ write sys$output "*** module sebasste ***"
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
$ write sys$output "Invalid argument given to sebasste.com file -- ", primary
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
$   if F$SEARCH("sebasste.imake") .nes. ""
$   then
$      vimake sebasste
$      purge sebasste.bld
$   else
$      if F$SEARCH("sebasste.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake sebasste
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @sebasste.bld "STD"
$   else
$      @sebasste.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create sebasste.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack sebasste.com -
	-s sebasste.f -
	-p sebasste.pdf -
	-i sebasste.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create sebasste.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
	REAL RAD(128,128),TEMP(128),WVLEN(128)
	INTEGER*2 TBUF(128),EBUF(128,128)
C						Open datasets, get size field
	CALL XVEACTION('SA',' ')
	CALL XVUNIT(IN,'INP',1,ISTAT,' ')
	CALL XVOPEN(IN,ISTAT,'U_FORMAT','REAL',' ')
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
	CALL XVUNIT(IOUT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUT,ISTAT,'U_FORMAT','HALF','O_FORMAT','HALF',
     &	           'OP','WRITE','U_ORG','BIL','U_NL',NL,'U_NS',NS,' ')
	CALL XVUNIT(IOUT2,'OUT',2,ISTAT,' ')
	CALL XVOPEN(IOUT2,ISTAT,'U_FORMAT','HALF','O_FORMAT','HALF',
     &	  'OP','WRITE','U_NB',1,'U_ORG','BSQ','U_NL',NL,'U_NS',NS,' ')
C
C						Get the other parameters
	CALL XVPARM('KEY',KEY,NUM,IDEF,0)
	KEY = 129 - KEY
	CALL XVPARM('EMIS',EMIS,NUM,IDEF,0)
	CALL XVPARM('DATE',IDATE,NUM,IDEF,0)
	CALL GET_SEBASS_WAVLEN(IDATE,WVLEN)
C						Update label
	CALL XLADD(IOUT,'HISTORY','LBL1','Emissivity Image',ISTAT,
     &		   'FORMAT','STRING',' ')
	CALL XLADD(IOUT,'HISTORY','LBL2','DN = 10,000*Emissivity',
     &		   ISTAT,'FORMAT','STRING',' ')
	CALL XLADD(IOUT2,'HISTORY','LBL1','Ground Temperature Image',
     &			ISTAT,'FORMAT','STRING',' ')
	CALL XLADD(IOUT2,'HISTORY','LBL2',
     &		  'DN = 100*Degrees Celsius',ISTAT,
     &		  'FORMAT','STRING',' ')
C
	DO ILINE=ISL,ISL+NL-1
	    DO ICHAN=1,128
		CALL XVREAD(IN,RAD(1,ICHAN),ISTAT,'LINE',ILINE,
     &			    'BAND',ICHAN,'SAMP',ISS,'NSAMPS',NS,' ')
	    END DO
	    DO J=1,NS
		DO K=1,128
		    TEMP(K) = PLKINV(WVLEN(K),0.001*RAD(J,K)/EMIS)
		END DO
		CALL SORTR(TEMP,128)
		XTEMP = TEMP(KEY)
		ITEMP = NINT(100.0*(XTEMP-273.15))
		TBUF(J) = MIN(32767,MAX(-32768,ITEMP))
		DO K=1,128
		    XRAD = 1000.0*PLANCK(WVLEN(K),XTEMP)
		    XEMIS = RAD(J,K) / XRAD
		    IEMIS = NINT(10000*XEMIS)
		    EBUF(J,K) = MIN(32767,MAX(-32768,IEMIS))
		END DO
	    END DO
	    DO ICHAN=1,128
		CALL XVWRIT(IOUT,EBUF(1,ICHAN),ISTAT,' ')
	    END DO
	    CALL XVWRIT(IOUT2,TBUF,ISTAT,' ')
	END DO
C
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create sebasste.pdf
Process help=*
parm  INP	(string,40)
parm  OUT	(string,40) 			count=2
parm  SIZE	integer	default=(1,1,0,0)	count=4
parm  SL	integer	default=1
parm  SS	integer	default=1
parm  NL	integer	default=0
parm  NS	integer	default=0
parm  KEY	integer default=11
parm  EMIS	real	default=0.98
parm  DATE      integer default=19970921 valid=(19970101:21000000)
End-proc

.TITLE
TAE PROCESS SEBASSTE
.HELP
PURPOSE:
SEBASSTE is a VICAR program that converts a SEBASS radiance (preferably
ground radiance) image to temperature and emissivity images.  This is done
by a greybody fitting method.  A greybody spectrum of emissivity EMIS is
draped over the radiance spectrum until the radiance spectrum touches or 
pierces the greybody spectrum at KEY channels.  This determines the kinetic
temperature of the pixel, which is then used to compute the emissivity of
the remaining channels.
.PAGE
OPERATION:
SEBASSTE calculates temperature and emissivity in the following manner: 
First, the brightness temperature for each of the 128 channels of the pixel
is calculated. These 128 temperatures are sorted from high to low and the
channel of the KEY'th highest temperature is assigned an emissivity of the
value of EMIS.  Using this emissivity, the kinetic temperature is calculated.
Using this kinetic temperature, the emissivities of the remaining channels
are calculated.

WRITTEN BY:  Ron Alley April 30, 1997

COGNIZANT PROGRAMMER:  Ron Alley

REVISION: New
.LEVEL1
.VARI INP
Radiance at surface
SEBASS image
.VARI OUT
(1) Emissivity image
(2) Temperature image
.VARI SIZE
The standard  VICAR output size
field.
.VARI SL
Starting line
.VARI SS
Starting sample
.VARI NL
Number of lines
.VARI NS
Number of samples
.VARI KEY
The rank of the channel set to
the standard emissivity.
.VARI EMIS
The standard emissivity
.VARI DATE
Date of data acquisition
as yyyymmdd
.LEVEL2
.VARI INP
The input file should be a 128 band SEBASS file of radiance data (preferably
ground radiance).  The units should be milliWatts per square meter per
steradian par micrometer, which are the units output by the program SEBASSCAL.
.VARI OUT
The first output will be the emissivity image.  It will be a 128 channel file
of halfword data.  The pixels are scaled by a factor of 10,000.  That is, an
emissivity of 1.0 is given the value 10000; an emissivity of 0.915 becomes
9150, etc.
The second output will be the kinetic temperature image.  It will be a single
channel file of halfword data.  The pixels will be in units of degrees 
Celsius scaled by a factor of 100.  That is, a temperature of 9.87 deg C
is given the value 987; a temperature of -15.35 becomes -1535.
.VARI SIZE
The standard VICAR output size field.   Default will calibrate
the entire data set.
	Example: SIZE = (1,1,200,128)
.VARI SL
Starting line (same as SIZE(1)).
.VARI SS
Starting sample (same as SIZE(2)).
.VARI NL
Number of lines (same as SIZE(3)).
.VARI NS
Number of samples (same as SIZE(4)).
.VARI KEY
SEBASSTE calculates temperature and emissivity in the following manner: 
First, the brightness temperature for each of the 128 channels of the pixel
is calculated. These 128 temperatures are sorted from high to low and the
channel of the KEY'th highest temperature is assigned an emissivity of the
value of EMIS.  Using this emissivity, the kinetic temperature is calculated.
Using this kinetic temperature, the emissivities of the remaining channels
are calculated.
     Therefore, KEY represents the number of channels that pierce the
greybody curve envelope that is draped over the radiance spectrum. Noise
free input should be run with KEY=1, and the value of KEY should be raised
as the amount of overall noise or number of noisy channels increases.
.VARI EMIS
SEBASSTE calculates temperature and emissivity in the following manner: 
First, the brightness temperature for each of the 128 channels of the pixel
is calculated. These 128 temperatures are sorted from high to low and the
channel of the KEY'th highest temperature is assigned an emissivity of the
value of EMIS.  Using this emissivity, the kinetic temperature is calculated.
Using this kinetic temperature, the emissivities of the remaining channels
are calculated.
     Therefore, EMIS represents the expected peak of the emissivity in the
pixel's true (noise free) spectrum.  The output spectrum will in fact have
KEY-1 spectral emissivities higher than EMIS, but these are regarded as
high due to noise.
.VARI DATE
This is the date the data were acquired, in the format yyyymmdd (e.g.
19970922 for September 22, 1997).  It is used to determine which
wavelength calibration to use.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create sebasste.imake
#define  PROGRAM   sebasste

#define MODULE_LIST sebasste.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
