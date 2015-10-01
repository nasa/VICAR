$!****************************************************************************
$!
$! Build proc for MIPL module amer_re
$! VPACK Version 1.8, Thursday, January 11, 1996, 16:43:24
$!
$! Execute by entering:		$ @amer_re
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
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
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module amer_re ***"
$!
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
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Repack .or. Create_PDF .or. Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to amer_re.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_PDF then gosub PDF_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_PDF = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("amer_re.imake") .nes. ""
$   then
$      vimake amer_re
$      purge amer_re.bld
$   else
$      if F$SEARCH("amer_re.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake amer_re
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @amer_re.bld "STD"
$   else
$      @amer_re.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create amer_re.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack amer_re.com -
	-p amer_re.pdf -
	-i amer_re.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create amer_re.pdf
! VICAR procedure to do calculate beta emissivity by iteration

PROCEDURE help=*

! TIMSCAL2 allowed input parameters
parm  INP	(string,40)
parm  OUT       (string,40)
parm  SL	integer	default=1
parm  NL	integer	default=0
parm  DATE	integer default=-1
parm  HEIGHT	real	default=10.0
parm  DATUM	real	default=0.001
parm  AMODEL	string	valid=(TR,MS,MW,SS,SW,ST,RS)
parm  ATEMP	string	default="  "	valid=(TR,MS,MW,SS,SW,ST,"  ")
parm  AHUMID	string	default="  "	valid=(TR,MS,MW,SS,SW,ST,"  ")
parm  AOZONE	string	default="  "	valid=(TR,MS,MW,SS,SW,ST,"  ")
parm  NLAYERS	integer	default=0
parm  WTYPE	keyword	default=RH	valid=(DP,RH,DE)
parm  ALTITUDE	real	count=0:34	default=0.0
parm  PRESSURE	real	count=0:34	default=0.0
parm  TEMP	real	count=0:34	default=0.0
parm  WATER	real	count=0:34	default=0.0
parm  WATERFAC  real                    default=1.0
parm  O3FAC	real	                default=1.0
parm  CO2FAC	real	                default=1.0
parm  SO2FAC	real	                default=1.0
parm  CH4FAC	real	                default=1.0
parm  O2FAC	real	                default=1.0
parm  E		real	default=(0.95,0.95,0.95,0.95,0.95,0.95)	count=6
parm  ATMODEL   keyword default=MODTRAN valid=(LOWTRAN,MODTRAN)

! BETAE allowed input parameters
parm    WAVE    type=real count=(1:6)
parm    NITER   type=integer default=1

! LOCAL variables
LOCAL ETEMPORARY type=string
LOCAL GTEMPORARY type=string
LOCAL COUNTER    type=integer
LOCAL DEFAULT    type=string

BODY

! Set the output file name
LET etemporary = out // ".1"
! Set the temporary grad file
LET gtemporary = out // ".temporary"

! Call TIMSCAL2 once with assumed emissivities in the 6 TIMS channels to
! get a GRAD image.
timscal2 inp=&inp out=&gtemporary sl=&sl nl=&nl date=&date +
  height=&height datum=&datum calmode="GRAD" amodel=&amodel +
  atemp=@atemp ahumid=@ahumid aozone=@aozone +  
  nlayers=&nlayers wtype=&wtype altitude=&altitude  + 
  pressure=&pressure temp=&temp water=&water waterfac=&waterfac + 
  o3fac=&o3fac co2fac=&co2fac so2fac=&so2fac ch4fac=&ch4fac +
  o2fac=&o2fac e=&e atmodel=&atmodel
    
! Call alphamod once to get an emissivity image to use with TIMSCAL2RE
amer inp=&gtemporary out=&etemporary wave=&wave

! Set the counters
LET counter=1
LET etemporary = out // ".&counter"
   
LOOP
  IF (counter >= niter) BREAK
  LET counter=counter+1
 
! Correct to ground radiance
  timscal2re inp=(&inp,&etemporary) out=&gtemporary + 
    sl=&sl nl=&nl date=&date height=&height datum=&datum +
    amodel=&amodel atemp=@atemp ahumid=@ahumid aozone=@aozone + 
    nlayers=&nlayers wtype=&wtype altitude=&altitude +
    pressure=&pressure temp=&temp water=&water waterfac=&waterfac + 
    o3fac=&o3fac co2fac=&co2fac so2fac=&so2fac ch4fac=&ch4fac + 
    o2fac=&o2fac atmodel=&atmodel
 
  LET etemporary = out // ".&counter"
  
 write ""
 write "Iteration Number &counter"
 write ""


! Extract the emissivity  
  amer inp=&gtemporary out=&etemporary wave=&wave
   
END-LOOP

! Delete the temporary grad file
ush \rm &gtemporary

END-PROC
.HELP
Program to calculate amer emissivity by iteration.  
.LEVEL1
.VARIABLE INP
Input radiance at sensor 
dataset (Halfword) in 
radiance (mW/m*m/um/sr)
.VARIABLE OUT
Root name for output. Root 
will have ".1", ".2" etc 
appended for each iteration. 
Output includes emissivity and 
an extra band of temperature. 
All output data in Halfword. 
Emissivity data *1.0e4. 
Temperature data degrees*100.
.VARI SL
Starting line
.VARI NL
Number of lines
.VARI AMODEL
Selects the model for which the
atmospheric corrections will
be performed via LOWTRAN/MODTRAN
.VARI ATEMP
Selects atmospheric temperature
and pressure profiles, if 
different from that defaulted
with AMODEL.
.VARI AHUMID
Selects the atmospheric water
vapor model, if different from
that defaulted with AMODEL.
.VARI AOZONE
Selects the ozone model, if
different from that defaulted
with AMODEL.
.VARI HEIGHT
Aircraft altitude (km above MSL)
.VARI DATUM
Surface elevation (km above MSL)
.VARI NLAYERS
The number of atmospheric lay-
ers in the radiosonde data
(if used).
.VARI ALTITUDE
The altitudes of each of the
atmospheric layers given in
the radiosonde data. (km.)
.VARI PRESSURE
The atmospheric pressures of
each of the atmospheric lay-
ers given in the radiosonde
data. (mb.)
.VARI TEMP
The temperatures of the at-
mospheric layers given in
the radiosonde data. (deg C)
.VARI WATER
The water profile in terms
of the parameter WTYPE, for
each layer in the radiosonde
data.
.VARI WTYPE
The units in which H2OS are
given.  The three types are
relative humidity (RH), dew-
point (DP), and density (DE)
.VARI WATERFAC
Varies the moisture profile
by the factor specified
.VARI O3FAC
Varies the ozone profile by
the factor specified
.VARI CO2FAC
Varies the carbon dioxide
profile by the factor specified
.VARI SO2FAC
Varies the sulfur dioxide
profile by the factor specified
.VARI CH4FAC
Varies the methane profile by
the factor specified
.VARI O2FAC
Varies the oxygen profile by
the factor specified
.VARI E
The emissivity assumed for
each band. 
.VARI DATE
Date of data acquisition. Used
to override the date in the
VICAR label (yymmdd)
.VARI ATMODEL
Atmospheric model to be used
(LOWTRAN or MODTRAN)
.VARIABLE WAVE
the central wavelength for the 
input channels, in micrometers.
(1 value per channel)
.VARIABLE NITER
Number of iterations.
.LEVEL2
.VARIABLE WAVE
The centroid wavelength value for each channel in micrometers. This can be 
calculated for a TIMS image using TIMSRESP (The central wavelength values are
dumped to the screen after the program runs).
.END




$ Return
$!#############################################################################
$Imake_File:
$ create amer_re.imake
#define  PROCEDURE amer_re

#define R2LIB 
$ Return
$!#############################################################################
