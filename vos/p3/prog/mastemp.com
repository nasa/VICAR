$!****************************************************************************
$!
$! Build proc for MIPL module mastemp
$! VPACK Version 1.8, Tuesday, September 22, 1998, 16:14:07
$!
$! Execute by entering:		$ @mastemp
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
$ write sys$output "*** module mastemp ***"
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
$ write sys$output "Invalid argument given to mastemp.com file -- ", primary
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
$   if F$SEARCH("mastemp.imake") .nes. ""
$   then
$      vimake mastemp
$      purge mastemp.bld
$   else
$      if F$SEARCH("mastemp.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake mastemp
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @mastemp.bld "STD"
$   else
$      @mastemp.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create mastemp.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack mastemp.com -
	-s mastemp.f -
	-p mastemp.pdf -
	-i mastemp.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create mastemp.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C
C	VICAR program MASTEMP, used to convert from upwelling radiance at
C	surface to spectral kinetic temperature of the surface, assuming a
C	given spectral emissivity.  This program works on the thermal IR bands
C	of MAS (Channels 42-50).
C
C	Ron Alley	27 August 1998
C	Revision: 1.1   Report temperature lookup table name     rea 9/21/98
C
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
	REAL EMIS(9),RAD(2000)
	REAL SKY(9)/9*0.0/
	REAL PI/3.141593/
	INTEGER*2 LUT(32767,9),ITEMP(2000)
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
     +		   'U_NB',9,'U_NL',NL,'U_NS',NS,'U_ORG','BSQ',' ')
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
C							      update VICAR label
	CALL XLDEL(IOUT,'HISTORY','LBL1',ISTAT,'HIST','MASTIR',' ')
	CALL XLDEL(IOUT,'HISTORY','LBL2',ISTAT,'HIST','MASTIR',' ')
	CALL XLADD(IOUT,'HISTORY','LBL1','MAS Temperature Image',ISTAT,
     +		   'FORMAT','STRING',' ')
	CALL XLADD(IOUT,'HISTORY','LBL2','DN = 100*Degrees Celsius',
     +		   ISTAT,'FORMAT','STRING',' ')
        CALL XVPARM('INP',INP_NAMES,NUM,IDEF,2)
        CALL XLADD(IOUT,'HISTORY','TEMP_LUT',INP_NAMES(2),ISTAT,
     +             'FORMAT','STRING',' ')
C						   Get the emissivity parameters
	CALL XVPARM('EMIS',EMIS,NUM,IDEF,9)
C						   Get the downwelling radiances
C						   from the VICAR label
	IF (EMIS(1) .NE. 1.0) CALL XLGET(IN,'HISTORY','SKY42',SKY(1),
     +					 ISTAT,'FORMAT','REAL',' ')
	IF (EMIS(2) .NE. 1.0) CALL XLGET(IN,'HISTORY','SKY43',SKY(2),
     +					 ISTAT,'FORMAT','REAL',' ')
	IF (EMIS(3) .NE. 1.0) CALL XLGET(IN,'HISTORY','SKY44',SKY(3),
     +					 ISTAT,'FORMAT','REAL',' ')
	IF (EMIS(4) .NE. 1.0) CALL XLGET(IN,'HISTORY','SKY45',SKY(4),
     +					 ISTAT,'FORMAT','REAL',' ')
	IF (EMIS(5) .NE. 1.0) CALL XLGET(IN,'HISTORY','SKY46',SKY(5),
     +					 ISTAT,'FORMAT','REAL',' ')
	IF (EMIS(6) .NE. 1.0) CALL XLGET(IN,'HISTORY','SKY47',SKY(6),
     +					 ISTAT,'FORMAT','REAL',' ')
	IF (EMIS(7) .NE. 1.0) CALL XLGET(IN,'HISTORY','SKY48',SKY(7),
     +					 ISTAT,'FORMAT','REAL',' ')
	IF (EMIS(8) .NE. 1.0) CALL XLGET(IN,'HISTORY','SKY49',SKY(8),
     +					 ISTAT,'FORMAT','REAL',' ')
	IF (EMIS(9) .NE. 1.0) CALL XLGET(IN,'HISTORY','SKY50',SKY(9),
     +					 ISTAT,'FORMAT','REAL',' ')
C
C					    Convert downwelling radiances to the
C						 amount of radiance reflected up
	DO ICHAN=1,9
	    SKY(ICHAN) = (1.0-EMIS(ICHAN))*SKY(ICHAN)/PI
	END DO
C					       Load the temperature lookup table
	DO ICHAN=1,9
	    CALL XVREAD(INLUT,LUT(1,ICHAN),ISTAT,' ')
	END DO
	CALL XVCLOSE(INLUT,ISTAT,' ')
C								   Do conversion
	DO ICHAN=1,9
	    DO ILINE=ISL,ISL+NL-1
		CALL XVREAD(IN,RAD,ISTAT,'LINE',ILINE,'BAND',ICHAN,
     +			    'SAMP',ISS,'NSAMPS',NS,' ')
		DO ISAMP=1,NS
		    RADX = (RAD(ISAMP)-SKY(ICHAN)) / EMIS(ICHAN)
		    INDEX = MAX(MIN(NINT(RADX),32767),1)
		    ITEMP(ISAMP) = LUT(INDEX,ICHAN)
		END DO
		CALL XVWRIT(IOUT,ITEMP,ISTAT,' ')
	    END DO
	END DO
C								Close datasets
	CALL XVCLOSE(IN,ISTAT,' ')
	CALL XVCLOSE(IOUT,ISTAT,' ')
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create mastemp.pdf
Process help=*
parm  INP	(string,80)			count=2
parm  OUT	(string,80)
parm  SIZE	integer	default=(1,1,0,0)	count=4
parm  SL	integer	default=1
parm  SS	integer	default=1
parm  NL	integer	default=0
parm  NS	integer	default=0
parm  EMIS	real	count=9 +
		default=(0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95)
End-proc

.TITLE
TAE PROCESS MASTEMP
.HELP
PURPOSE:

   MASTEMP is a program that accepts as input the ten MAS TIR channels
of radiance (Channels 42-50), and calculates the corresponding kinetic 
temperatures for each channel at each pixel.


OPERATION:

   MASTEMP requires as input a 9 channel MAS TIR image dataset of
radiance and the radiance-to-temperature lookup table appropriate for the
image dataset.  The program accepts as parameter input the spectral emissivity
for each channel.  If the spectral emissivity for a channel is not 1.0, then
the downwelling irradiance for each channel is read from the VICAR label and 
radiance is adjusted via the following formula:

      Rad(emitted) = Rad(upwelling) - (1-emissivity)*Irrad(downwelling)/pi

The emitted radiance is converted to the corresponding blackbody radiance
by dividing by the spectral emissivity, and the supplied lookup table is
used to convert the blackbody radiance to kinetic temperature.


WRITTEN BY:  Ron Alley         August 27, 1998

COGNIZANT PROGRAMMER:  Ron Alley

REVISION: New

.LEVEL1
.VARI INP
(1) MAS TIR radiance image
(2) MAS TIR temperature
    lookup table
.VARI OUT
Output dataset of the 9 MAS
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
.VARI EMIS
Spectral emissivity for each
MAS channel.
.LEVEL2
.VARI INP
The first file should contain the 9 TIR channels of MAS image data,
calibrated in units of milliWatts per square meter per steradian per 
micrometer.

The second file should be the MAS TIR radiance to temperature lookup 
table for the spectral response calibration that is in effect for the date
of data acquisition.
.VARI OUT
The output dataset will contain surface kinetic temperature for each of the
9 input channels, assuming the emissivities supplied in the EMIS parameter.
Output pixels are halfword (16 bit signed integer), in units of degrees
Celsius, scaled by a factor of 100.0

For example,

       DN = 2531  denotes a temperature of 25.31 degrees Celsius.
.VARI SIZE
The standard VICAR2 output size field.   Default will process
the entire data set.
	Example: SIZE = (1,1,200,716)
.VARI SL
Starting line (same as SIZE(1)).
.VARI SS
Starting sample (same as SIZE(2)).
.VARI NL
Number of lines (same as SIZE(3)).
.VARI NS
Number of samples (same as SIZE(4)).
.VARI EMIS
These are the spectral emissivity values assumed when computing the amount
of reflected downwelling radiance in the input, and when using the Planck
function to convert from radiance to temperature.  Nine values are required,
one for each MAS channel from Channel 42 to 50.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create mastemp.imake
#define  PROGRAM   mastemp

#define MODULE_LIST mastemp.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
