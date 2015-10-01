$!****************************************************************************
$!
$! Build proc for MIPL module astertemp
$! VPACK Version 1.8, Wednesday, November 22, 2000, 16:43:17
$!
$! Execute by entering:		$ @astertemp
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
$ write sys$output "*** module astertemp ***"
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
$ write sys$output "Invalid argument given to astertemp.com file -- ", primary
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
$   if F$SEARCH("astertemp.imake") .nes. ""
$   then
$      vimake astertemp
$      purge astertemp.bld
$   else
$      if F$SEARCH("astertemp.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake astertemp
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @astertemp.bld "STD"
$   else
$      @astertemp.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create astertemp.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack astertemp.com -
	-s astertemp.f -
	-p astertemp.pdf -
	-i astertemp.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create astertemp.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C
C	VICAR program ASTERTEMP, used to convert from upwelling radiance at
C	surface to spectral kinetic temperature of the surface, assuming a
C	given spectral emissivity and downwelling sky irradiance.  This
C	program works on the thermal IR bands of ASTER (Channels 10-14).
C
C	Ron Alley	11 June 2000
C
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
	REAL EMIS(5),SKY(5),RAD(2000),RLUT(32767,5)
	REAL PI/3.141593/
	INTEGER*2 ITEMP(2000)
	LOGICAL QINTERP,XVPTST
	CHARACTER*80 INP_NAMES(2)
C						Open datasets, get size field
	CALL XVUNIT(IN,'INP',1,ISTAT,' ')
	CALL XVOPEN(IN,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     +		    'U_FORMAT','REAL',' ')
	CALL XVUNIT(INLUT,'INP',2,ISTAT,' ')
	CALL XVOPEN(INLUT,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',' ')
	CALL XVUNIT(IOUT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUT,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     +	           'OP','WRITE','U_FORMAT','HALF','O_FORMAT','HALF',
     +		   'U_NB',5,'U_NL',NL,'U_NS',NS,'U_ORG','BSQ',' ')
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
C							      update VICAR label
	CALL XLADD(IOUT,'HISTORY','LBL1',
     +		 'ASTER Temperature Image',ISTAT,'FORMAT','STRING',' ')
	CALL XLADD(IOUT,'HISTORY','LBL2','DN = 100*Degrees Celsius',
     +		   ISTAT,'FORMAT','STRING',' ')
	CALL XVPARM('INP',INP_NAMES,NUM,IDEF,2)
	CALL XLADD(IOUT,'HISTORY','TEMP_LUT',INP_NAMES(2),ISTAT,
     +		   'FORMAT','STRING',' ')
C							      Get the parameters
	CALL XVPARM('SKYRAD',SKY,NUM,IDEF,5)
	CALL XVPARM('EMIS',EMIS,NUM,IDEF,5)
	QINTERP = .NOT. XVPTST('NO')
C
C					    Convert downwelling radiances to the
C						 amount of radiance reflected up
	DO ICHAN=1,5
	    SKY(ICHAN) = (1.0-EMIS(ICHAN))*SKY(ICHAN)/PI
	END DO
C					       Load the temperature lookup table
	DO ICHAN=1,5
	    CALL XVREAD(INLUT,RLUT(1,ICHAN),ISTAT,' ')
	END DO
	CALL XVCLOSE(INLUT,ISTAT,' ')
C								   Do conversion
	IF (QINTERP) THEN
	    CALL XVMESSAGE(' Interpolating lookup table values',' ')
	    DO ICHAN=1,5
		DO ILINE=ISL,ISL+NL-1
		    CALL XVREAD(IN,RAD,ISTAT,'LINE',ILINE,'BAND',ICHAN,
     +				'SAMP',ISS,'NSAMPS',NS,' ')
		    DO ISAMP=1,NS
			IF (RAD(ISAMP) .LE. 0.0) THEN
			    ITEMP(ISAMP) = -27315
			ELSE
			    RADX = (RAD(ISAMP)-SKY(ICHAN)) / EMIS(ICHAN)
			    INDEX = MAX(MIN(INT(RADX),32766),1)
			    FRAC = RADX - FLOAT(INDEX)
			    TEMP = (1.0-FRAC)*RLUT(INDEX,ICHAN) + 
     +				   FRAC*RLUT(INDEX+1,ICHAN) - 273.15
			    ITEMP(ISAMP) = MAX(MIN(NINT(100.0*TEMP),
     +						   32767), -32768)
			END IF
		    END DO
		    CALL XVWRIT(IOUT,ITEMP,ISTAT,' ')
		END DO
	    END DO
	ELSE
	    CALL XVMESSAGE(' Using closest lookup table values',' ')
	    DO ICHAN=1,5
		DO ILINE=ISL,ISL+NL-1
		    CALL XVREAD(IN,RAD,ISTAT,'LINE',ILINE,'BAND',ICHAN,
     +				'SAMP',ISS,'NSAMPS',NS,' ')
		    DO ISAMP=1,NS
			IF (RAD(ISAMP) .LE. 0.0) THEN
			    ITEMP(ISAMP) = -27315
			ELSE
			    RADX = (RAD(ISAMP)-SKY(ICHAN)) / EMIS(ICHAN)
			    INDEX = MAX(MIN(NINT(RADX),32767),1)
			    TEMP = RLUT(INDEX,ICHAN) - 273.15
			    ITEMP(ISAMP) = MAX(MIN(NINT(100.0*TEMP),
     +					       32767),-32768)
			END IF
		    END DO
		    CALL XVWRIT(IOUT,ITEMP,ISTAT,' ')
		END DO
	    END DO
	END IF
C								Close datasets
	CALL XVCLOSE(IN,ISTAT,' ')
	CALL XVCLOSE(IOUT,ISTAT,' ')
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create astertemp.pdf
Process help=*
parm  INP	(string,80) count=2 +
		default=("xxx","$V2TOP/luts/lut.aster.temp")
parm  OUT	(string,80)
parm  SIZE	integer	default=(1,1,0,0)	count=4
parm  SL	integer	default=1
parm  SS	integer	default=1
parm  NL	integer	default=0
parm  NS	integer	default=0
parm  SKYRAD	real	count=5 default=(0.0,0.0,0.0,0.0,0.0)
parm  EMIS	real	count=5 default=(0.95,0.95,0.95,0.95,0.95)
parm  INTERP	keyword valid=(YES,NO) count=1 default=YES
End-proc

.TITLE
TAE PROCESS ASTERTEMP
.HELP
PURPOSE:

   ASTERTEMP is a program that accepts as input the five ASTER TIR channels
of radiance (Channels 10-14), and calculates the corresponding kinetic 
temperatures for each channel at each pixel.


OPERATION:

   ASTERTEMP requires as input a 5 channel ASTER TIR image dataset of
radiance and the radiance-to-temperature lookup table.  The program accepts
as parameter input the spectral emissivity and the downwelling sky irradiance
for each channel.  First, the input radiance is adjusted via the following 
formula:

      Rad(emitted) = Rad(upwelling) - (1-emissivity)*Irrad(downwelling)/pi

The emitted radiance is converted to the corresponding blackbody radiance
by dividing by the spectral emissivity, and the supplied lookup table is
used to convert the blackbody radiance to kinetic temperature.  In the
default mode, linear interpolation is used between lookup table entries.
If the keyword NO is supplied, no interpolation is performed, and the 
closest table entry is used.


WRITTEN BY:  Ron Alley         June 11,  2000

COGNIZANT PROGRAMMER:  Ron Alley

REVISION: 1.1   14 June 2000

.LEVEL1
.VARI INP
(1) ASTER TIR radiance image
(2) ASTER TIR temperature
    lookup table
.VARI OUT
Output dataset of the 5 ASTER
TIR bands, for kinetic
temperature
.VARI SIZE
The standard  VICAR2 output size
field.
.VARI SL
Starting line
.VARI SS
Starting sample
.VARI NL
Number of lines
.VARI NS
Number of samples
.VARI SKYRAD
Sky irradiance for each
ASTER channel.
.VARI EMIS
Spectral emissivity for each
ASTER channel.
.VARI INTERP
Iterpolate lookup table values?
Valid: YES, NO
.LEVEL2
.VARI INP
The first file should contain the 5 TIR channels of ASTER image data,
calibrated in units of milliWatts per square meter per steradian per 
micrometer.

The second file should be the ASTER TIR radiance to temperature lookup 
table.  The default file contains the standard calibration lookup table.

.VARI OUT
The output dataset will contain surface kinetic temperature for each of the
5 input channels, assuming the emissivities supplied in the EMIS parameter,
and the sky irradiances supplied in the SKYRAD parameter.  Note, however,
that if the emissivity is 1.0, the value of the sky irradiance is 
irrelevant.

Output pixels are halfword (16 bit signed integer), in units of degrees
Celsius, scaled by a factor of 100.0

For example,

       DN = 2531  denotes a temperature of 25.31 degrees Celsius.
.VARI SIZE
The standard VICAR2 output size field.   Default will process
the entire data set.
	Example: SIZE = (1,1,200,638)
.VARI SL
Starting line (same as SIZE(1)).
.VARI SS
Starting sample (same as SIZE(2)).
.VARI NL
Number of lines (same as SIZE(3)).
.VARI NS
Number of samples (same as SIZE(4)).
.VARI SKYRAD
These are the values for the full hemisphere downwelling sky irradiance, for
each of the 5 ASTER channels.  They must be supplied in units of 
milliWatts per square meter per micrometer.
.VARI EMIS
These are the spectral emissivity values assumed when computing the amount
of reflected downwelling radiance in the input, and when using the Planck
function to convert from radiance to temperature.  Five values are required,
one for each ASTER channel from Channel 10 to 14.
.VARI INTERP
In the default mode, linear interpolation is used between lookup table
entries.  If the keyword NO is supplied, no interpolation is performed,
and the closest table entry is used.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create astertemp.imake
#define  PROGRAM   astertemp

#define MODULE_LIST astertemp.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
