$!****************************************************************************
$!
$! Build proc for MIPL module mastertemp
$! VPACK Version 1.8, Wednesday, November 10, 2004, 20:11:08
$!
$! Execute by entering:		$ @mastertemp
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
$ write sys$output "*** module mastertemp ***"
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
$ write sys$output "Invalid argument given to mastertemp.com file -- ", primary
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
$   if F$SEARCH("mastertemp.imake") .nes. ""
$   then
$      vimake mastertemp
$      purge mastertemp.bld
$   else
$      if F$SEARCH("mastertemp.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake mastertemp
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @mastertemp.bld "STD"
$   else
$      @mastertemp.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create mastertemp.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack mastertemp.com -
	-s mastertemp.f -
	-p mastertemp.pdf -
	-i mastertemp.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create mastertemp.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C
C	VICAR program MASTERTEMP, used to convert from upwelling radiance at
C	surface to spectral kinetic temperature of the surface, assuming a
C	given spectral emissivity.  This program works on the thermal IR bands
C	of MASTER (Channels 41-50).
C
C	Ron Alley	14 August 1998
C	Revision: 1.1   Report temperature lookup table name rea 9/21/98
C	Revision: 1.2   Change inputs and outputs to unscaled real pixels
C								rea 11/12/2004
C
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
	REAL EMIS(10),RAD(2000),TEMP(2000)
	REAL SKY(10)/10*0.0/
	REAL PI/3.141593/
	INTEGER*2 LUT(32767,10)
	CHARACTER*80 INP_NAMES(2)
C						Open datasets, get size field
	CALL XVUNIT(IN,'INP',1,ISTAT,' ')
	CALL XVOPEN(IN,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     +		    'U_FORMAT','REAL',' ')
	CALL XVUNIT(INLUT,'INP',2,ISTAT,' ')
	CALL XVOPEN(INLUT,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',' ')
	CALL XVUNIT(IOUT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUT,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     +	           'OP','WRITE','U_FORMAT','REAL','O_FORMAT','REAL',
     +		   'U_NB',10,'U_NL',NL,'U_NS',NS,'U_ORG','BSQ',' ')
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
C							      update VICAR label
	CALL XLDEL(IOUT,'HISTORY','LBL1',ISTAT,'HIST','MASTERTIR',' ')
	CALL XLDEL(IOUT,'HISTORY','LBL2',ISTAT,'HIST','MASTERTIR',' ')
	CALL XLADD(IOUT,'HISTORY','LBL1',
     +		 'MASTER Temperature Image',ISTAT,'FORMAT','STRING',' ')
	CALL XLADD(IOUT,'HISTORY','LBL2','DN = Degrees Celsius',ISTAT,
     +		   'FORMAT','STRING',' ')
	CALL XVPARM('INP',INP_NAMES,NUM,IDEF,2)
	CALL XLADD(IOUT,'HISTORY','TEMP_LUT',INP_NAMES(2),ISTAT,
     +		   'FORMAT','STRING',' ')
C						   Get the emissivity parameters
	CALL XVPARM('EMIS',EMIS,NUM,IDEF,10)
C						   Get the downwelling radiances
C						   from the VICAR label
        IF (EMIS(1) .NE. 1.0) CALL XLGET(IN,'HISTORY','SKY41',SKY(1),
     +                   ISTAT,'FORMAT','REAL','HIST','MASTERTI',' ')
        IF (EMIS(2) .NE. 1.0) CALL XLGET(IN,'HISTORY','SKY42',SKY(2),
     +                   ISTAT,'FORMAT','REAL','HIST','MASTERTI',' ')
        IF (EMIS(3) .NE. 1.0) CALL XLGET(IN,'HISTORY','SKY43',SKY(3),
     +                   ISTAT,'FORMAT','REAL','HIST','MASTERTI',' ')
        IF (EMIS(4) .NE. 1.0) CALL XLGET(IN,'HISTORY','SKY44',SKY(4),
     +                   ISTAT,'FORMAT','REAL','HIST','MASTERTI',' ')
        IF (EMIS(5) .NE. 1.0) CALL XLGET(IN,'HISTORY','SKY45',SKY(5),
     +                   ISTAT,'FORMAT','REAL','HIST','MASTERTI',' ')
        IF (EMIS(6) .NE. 1.0) CALL XLGET(IN,'HISTORY','SKY46',SKY(6),
     +                   ISTAT,'FORMAT','REAL','HIST','MASTERTI',' ')
        IF (EMIS(7) .NE. 1.0) CALL XLGET(IN,'HISTORY','SKY47',SKY(7),
     +                   ISTAT,'FORMAT','REAL','HIST','MASTERTI',' ')
        IF (EMIS(8) .NE. 1.0) CALL XLGET(IN,'HISTORY','SKY48',SKY(8),
     +                   ISTAT,'FORMAT','REAL','HIST','MASTERTI',' ')
        IF (EMIS(9) .NE. 1.0) CALL XLGET(IN,'HISTORY','SKY49',SKY(9),
     +                   ISTAT,'FORMAT','REAL','HIST','MASTERTI',' ')
        IF (EMIS(10) .NE. 1.0) CALL XLGET(IN,'HISTORY','SKY50',SKY(10),
     +                   ISTAT,'FORMAT','REAL','HIST','MASTERTI',' ')
C
C					    Convert downwelling radiances to the
C						 amount of radiance reflected up
	DO ICHAN=1,10
	    SKY(ICHAN) = (1.0-EMIS(ICHAN))*SKY(ICHAN)/(1000.0*PI)
	END DO
C					       Load the temperature lookup table
	DO ICHAN=1,10
	    CALL XVREAD(INLUT,LUT(1,ICHAN),ISTAT,' ')
	END DO
	CALL XVCLOSE(INLUT,ISTAT,' ')
C								   Do conversion
	DO ICHAN=1,10
	    DO ILINE=ISL,ISL+NL-1
		CALL XVREAD(IN,RAD,ISTAT,'LINE',ILINE,'BAND',ICHAN,
     +			    'SAMP',ISS,'NSAMPS',NS,' ')
		DO ISAMP=1,NS
		    RADX = (RAD(ISAMP)-SKY(ICHAN)) / EMIS(ICHAN)
		    INDEX = MAX(MIN(NINT(1000.0*RADX),32767),1)
		    TEMP(ISAMP) = LUT(INDEX,ICHAN) / 100.0
		END DO
		CALL XVWRIT(IOUT,TEMP,ISTAT,' ')
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
$ create mastertemp.pdf
Process help=*
parm  INP	(string,80)			count=2
parm  OUT	(string,80)
parm  SIZE	integer	default=(1,1,0,0)	count=4
parm  SL	integer	default=1
parm  SS	integer	default=1
parm  NL	integer	default=0
parm  NS	integer	default=0
parm  EMIS	real	count=10 +
		default=(0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95)
End-proc

.TITLE
TAE PROCESS MASTERTEMP
.HELP
PURPOSE:

   MASTERTEMP is a program that accepts as input the ten MASTER TIR channels
of radiance (Channels 41-50), and calculates the corresponding kinetic 
temperatures for each channel at each pixel.


OPERATION:

   MASTERTEMP requires as input a 10 channel MASTER TIR image dataset of
radiance and the radiance-to-temperature lookup table appropriate for the
image dataset.  The program accepts as parameter input the spectral emissivity
for each channel.  If the spectral emissivity for a channel is not 1.0, then
the downwelling irradiance for each channel is read from the VICAR label and 
radiance is adjusted via the following formula:

      Rad(emitted) = Rad(upwelling) - (1-emissivity)*Irrad(downwelling)/pi

The emitted radiance is converted to the corresponding blackbody radiance
by dividing by the spectral emissivity, and the supplied lookup table is
used to convert the blackbody radiance to kinetic temperature.


WRITTEN BY:  Ron Alley         August 12, 1998

COGNIZANT PROGRAMMER:  Ron Alley

REVISION: Version 1.2  12 Nov 2004

.LEVEL1
.VARI INP
(1) MASTER TIR radiance image
(2) MASTER TIR temperature
    lookup table
.VARI OUT
Output dataset of the 10 MASTER
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
MASTER channel.
.LEVEL2
.VARI INP
The first file should contain the 10 TIR channels of MASTER image data,
calibrated in units of Watts per square meter per steradian per 
micrometer.  Note that these units are different from what the program
expected prior to November, 2004.

The second file should be the MASTER TIR radiance to temperature lookup 
table for the spectral response calibration that is in effect for the date
of data acquisition.  These files are typically located in the directory 

          $V2TOP/luts

and have names of the form

         lut.master.temp.YEAR.MON
.VARI OUT
The output dataset will contain surface kinetic temperature for each of the
10 input channels, assuming the emissivities supplied in the EMIS parameter.
Output pixels are real (32 bit floating point values), in units of degrees
Celsius.  Note that the pixel size and data scaling have changed from the
values in effect prior to November, 2004.
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
.VARI EMIS
These are the spectral emissivity values assumed when computing the amount
of reflected downwelling radiance in the input, and when using the Planck
function to convert from radiance to temperature.  Ten values are required,
one for each MASTER channel from Channel 41 to 50.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create mastertemp.imake
#define  PROGRAM   mastertemp

#define MODULE_LIST mastertemp.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
