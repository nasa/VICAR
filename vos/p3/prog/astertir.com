$!****************************************************************************
$!
$! Build proc for MIPL module astertir
$! VPACK Version 1.8, Monday, April 14, 1997, 14:50:16
$!
$! Execute by entering:		$ @astertir
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
$ write sys$output "*** module astertir ***"
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
$ write sys$output "Invalid argument given to astertir.com file -- ", primary
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
$   if F$SEARCH("astertir.imake") .nes. ""
$   then
$      vimake astertir
$      purge astertir.bld
$   else
$      if F$SEARCH("astertir.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake astertir
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @astertir.bld "STD"
$   else
$      @astertir.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create astertir.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack astertir.com -
	-s astertir.f -
	-p astertir.pdf -
	-i astertir.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create astertir.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
C	This program accepts as input a TIMS image in temperature
C	units, and produces as output a simulation of an ASTER
C	image in radiance units.  It does this by matching each ASTER
C	channel to its closest TIMS channel, and assuming that there
C	is no emissivity change over the change of wavelengths.
C
C	4/11/97 ...rea... Modified to use the PFM calibration of the
C			  ASTER sensor
C
	REAL WAVE(6)/8.2873,
     &		     8.6349,
     &		     9.0793,
     &              99.9999,
     &		    10.6592,
     &		    11.2893/
	INTEGER*2 HBUF(10000)
C
C		Open data sets
C
	CALL XVUNIT(INUNIT,'INP',1,ISTAT,' ')
	CALL XVOPEN(INUNIT,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     &		    'U_FORMAT','HALF',' ')
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
	CALL XVUNIT(IOUT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUT,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     &		'U_NL',NL,'U_NS',NS,'U_ORG','BIL','OP','WRITE',
     &		'U_FORMAT','HALF','O_FORMAT','HALF','U_NB',5,' ')
C
	IEL = ISL+NL-1
	IES = ISS+NS-1
C
	IF(IEL.GT.NLIN .OR. IES.GT.NSIN) THEN
	    CALL XVMESSAGE(
     &         ' Size field specified overruns file dimensions',' ')
	    CALL ABEND
	ENDIF
C
	CALL XLADD(IOUT,'HISTORY','LBL1',
     &		'Radiance Image to Simulate ASTER TIR',ISTAT,
     &		'FORMAT','STRING',' ')
	CALL XLADD(IOUT,'HISTORY','LBL2',
     &		   'DN = milliwatts/m*m*sr*micrometer',
     &		   ISTAT,'FORMAT','STRING',' ')
C
	DO ILINE=ISL,IEL
C
	    DO ICHAN=1,6
C							read & write a line
		CALL XVREAD(INUNIT,HBUF,ISTAT,'LINE',ILINE,'SAMP',ISS,
     &			    'BAND',ICHAN,'NSAMPS',NS,' ')
		IF (ICHAN .NE. 4) THEN
		    DO ISAMP=1,NS
			X = (FLOAT(HBUF(ISAMP)) / 100.0) + 273.15
			Y = 1000.0 * PLANCK(WAVE(ICHAN),X)
			HBUF(ISAMP) = NINT(Y)
		    END DO
		    CALL XVWRIT(IOUT,HBUF,ISTAT,' ')
		END IF
	    END DO
	END DO
C							Close all datasets
C
	CALL XVCLOSE(INUNIT,ISTAT,' ')
	CALL XVCLOSE(IOUT,ISTAT,' ')
C
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create astertir.pdf
Process help=*
parm  INP	(string,40)
parm  OUT	(string,40)
parm  SIZE	integer	default=(1,1,0,0)	count=4
parm  SL	integer	default=1
parm  SS	integer	default=1
parm  NL	integer	default=0
parm  NS	integer	default=0
End-proc

.TITLE
TAE PROCESS ASTERTIR
.HELP
PURPOSE:

   ASTERTIR is a program that converts a TIMS ITEM, GTEM, or BTEM image to
a simulation of an ASTER TIR image, in radiance units.


.LEVEL1
.VARI INP
TIMS temperature image file,
(BTEM, ITEM, or GTEM)
.VARI OUT
ASTER TIR radiance image file,
5 channels, BIL format.
.VARI SIZE
The standard VICAR output size
field.
.VARI SL
Starting line
.VARI SS
Starting sample
.VARI NL
Number of lines
.VARI NS
Number of samples
.LEVEL2
.VARI INP
The input file should be a TIMS image, in temperature units.
.VARI OUT
The output file will be a simulated ASTER TIR image, 5 channels, halfword 
data, in BIL format.  The image will be in the standard radiance units
(milliWatts per square meter per steradian per micrometer)
.VARI SIZE
	The standard VICAR2 output size field.   Default will calibrate
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
$ Return
$!#############################################################################
$Imake_File:
$ create astertir.imake
#define  PROGRAM   astertir

#define MODULE_LIST astertir.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
