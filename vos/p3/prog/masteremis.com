$!****************************************************************************
$!
$! Build proc for MIPL module masteremis
$! VPACK Version 1.8, Wednesday, November 10, 2004, 16:29:57
$!
$! Execute by entering:		$ @masteremis
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
$ write sys$output "*** module masteremis ***"
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
$ write sys$output "Invalid argument given to masteremis.com file -- ", primary
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
$   if F$SEARCH("masteremis.imake") .nes. ""
$   then
$      vimake masteremis
$      purge masteremis.bld
$   else
$      if F$SEARCH("masteremis.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake masteremis
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @masteremis.bld "STD"
$   else
$      @masteremis.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create masteremis.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack masteremis.com -
	-s masteremis.f -
	-p masteremis.pdf -
	-i masteremis.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create masteremis.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C
C	This program computes the temperature and spectral emissivity for
C	the pixels of a MASTER TIR radiance image.
C
C	July 13, 2001      ...rea... Initial release
C	September 25, 2002 ...rea... Add 8CHANFIT mode
C	April 8, 2003      ...rea... Fix bug for REFCHAN=50 case
C	November 10, 2004  ...rea... Change to unscaled real values for both
C				     input and output.
C
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
	IMPLICIT NONE
	REAL X,EMIS,BUF(5000)/5000*0.0/
	REAL SKYRAD(10)/10*0.0/
	REAL PI/3.141593/
	INTEGER IN,IOUT,IOUT2,ISTAT,ISL,ISS,NL,NS,NLIN,NSIN,NUM,IDEF
	INTEGER IREF,IFIT,I,J,LUTUNIT1,LUTUNIT2
	INTEGER*2 RAD_LUT(40000,10),TEMP_LUT(32767,10)
	LOGICAL XVPTST,QBBFIT,QFIT8
	CHARACTER*100 LABEL
	CHARACTER*7 ORDINAL(10)/'       ',' second',' third ',' fourth',
     +	    ' fifth ',' sixth' ,'seventh',' eighth',' ninth ',' tenth '/
C
C						   Open datasets, get size field
	CALL XVUNIT(IN,'INP',1,ISTAT,' ')
	
	CALL XVOPEN(IN,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     +		    'U_FORMAT','REAL',' ')
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
	CALL XVUNIT(IOUT2,'OUT',2,ISTAT,' ')
	CALL XVOPEN(IOUT2,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     +		 'OP','WRITE','U_NL',NL,'U_NS',NS,'U_NB',1,
     +		 'U_ORG','BSQ','U_FORMAT','REAL','O_FORMAT','REAL',' ')
	CALL XVUNIT(IOUT,'OUT',1,ISTAT,' ')
	IF (XVPTST('BSQ')) THEN
	    CALL XVOPEN(IOUT,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     +			'OP','WRITE','U_ORG','BSQ','U_NL',NL,'U_NS',NS,
     +			'U_FORMAT','REAL','O_FORMAT','REAL',' ')
	    DO I=1,10
		DO J=1,NL
		    CALL XVWRIT(IOUT,BUF,ISTAT,' ')
		END DO
	    END DO
	    CALL XVCLOSE(IOUT,ISTAT,' ')
	    CALL XVOPEN(IOUT,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     +	           'OP','UPDATE','U_FORMAT','REAL',' ')
	ELSE
	    CALL XVOPEN(IOUT,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     +	           'OP','WRITE','U_ORG','BIL','U_NL',NL,'U_NS',NS,
     +		   'U_FORMAT','REAL','O_FORMAT','REAL',' ')
	END IF
C							Get the other parameters
	QFIT8 = XVPTST('8CHANFIT')
	QBBFIT = XVPTST('BBFIT')
	CALL XVPARM('EMIS',EMIS,NUM,IDEF,0)
	CALL XVPARM('REFCHAN',IREF,NUM,IDEF,0)
	CALL XVPARM('FITCHAN',IFIT,NUM,IDEF,0)
C							       Get lookup tables
	CALL XVUNIT(LUTUNIT1,'INP',2,ISTAT,' ')
	CALL XVOPEN(LUTUNIT1,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',' ')
	CALL XVUNIT(LUTUNIT2,'INP',3,ISTAT,' ')
	CALL XVOPEN(LUTUNIT2,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',' ')
	DO I=1,10
	    CALL XVREAD(LUTUNIT1,RAD_LUT(1,I),ISTAT,' ')
	    CALL XVREAD(LUTUNIT2,TEMP_LUT(1,I),ISTAT,' ')
	END DO
	CALL XVCLOSE(LUTUNIT1,ISTAT,' ')
	CALL XVCLOSE(LUTUNIT2,ISTAT,' ')
C						     Get downwelling irradiances
	X = 0.0
	CALL XLGET(IN,'HISTORY','SKY41',X,ISTAT,'FORMAT','REAL',' ')
	SKYRAD(1) = X / (1000.0*PI)
	CALL XLGET(IN,'HISTORY','SKY42',X,ISTAT,'FORMAT','REAL',' ')
	SKYRAD(2) = X / (1000.0*PI)
	CALL XLGET(IN,'HISTORY','SKY43',X,ISTAT,'FORMAT','REAL',' ')
	SKYRAD(3) = X / (1000.0*PI)
	CALL XLGET(IN,'HISTORY','SKY44',X,ISTAT,'FORMAT','REAL',' ')
	SKYRAD(4) = X / (1000.0*PI)
	CALL XLGET(IN,'HISTORY','SKY45',X,ISTAT,'FORMAT','REAL',' ')
	SKYRAD(5) = X / (1000.0*PI)
	CALL XLGET(IN,'HISTORY','SKY46',X,ISTAT,'FORMAT','REAL',' ')
	SKYRAD(6) = X / (1000.0*PI)
	CALL XLGET(IN,'HISTORY','SKY47',X,ISTAT,'FORMAT','REAL',' ')
	SKYRAD(7) = X / (1000.0*PI)
	CALL XLGET(IN,'HISTORY','SKY48',X,ISTAT,'FORMAT','REAL',' ')
	SKYRAD(8) = X / (1000.0*PI)
	CALL XLGET(IN,'HISTORY','SKY49',X,ISTAT,'FORMAT','REAL',' ')
	SKYRAD(9) = X / (1000.0*PI)
	CALL XLGET(IN,'HISTORY','SKY50',X,ISTAT,'FORMAT','REAL',' ')
	SKYRAD(10) = X / (1000.0*PI)
C					    Remove obsolete VICAR history labels
	CALL XLDEL(IOUT,'HISTORY','LBL1',ISTAT,'HIST','MASTERTI',' ')
	CALL XLDEL(IOUT,'HISTORY','LBL2',ISTAT,'HIST','MASTERTI',' ')
	CALL XLDEL(IOUT,'HISTORY','LBL3',ISTAT,'HIST','MASTERTI',' ')
	CALL XLDEL(IOUT2,'HISTORY','LBL1',ISTAT,'HIST','MASTERTI',' ')
	CALL XLDEL(IOUT2,'HISTORY','LBL2',ISTAT,'HIST','MASTERTI',' ')
	CALL XLDEL(IOUT2,'HISTORY','LBL3',ISTAT,'HIST','MASTERTI',' ')
C						    Add new VICAR history labels
	CALL XLADD(IOUT,'HISTORY','LBL1','Emissivity Image',
     &			ISTAT,'FORMAT','STRING',' ')
	CALL XLADD(IOUT2,'HISTORY','LBL1','Ground Temperature Image',
     &			ISTAT,'FORMAT','STRING',' ')
	CALL XLADD(IOUT2,'HISTORY','LBL2',
     &	      'Units = Degrees Celsius',ISTAT,'FORMAT','STRING',' ')
C
	IF (QBBFIT) THEN
C									bbfit
	    WRITE (LABEL,100) EMIS,ORDINAL(IFIT)
  100	    FORMAT(F5.3,' Emittance graybody fit to ',A7,
     +		   ' highest spectral emittance')
	    CALL XLADD(IOUT,'HISTORY','METHOD',LABEL,ISTAT,
     &			'FORMAT','STRING',' ')
	    CALL XLADD(IOUT2,'HISTORY','METHOD',LABEL,ISTAT,
     &			'FORMAT','STRING',' ')
	    CALL EMISCAL2(IN,IOUT,IOUT2,ISL,ISS,NL,NS,IFIT,EMIS,SKYRAD,
     +			  RAD_LUT,TEMP_LUT,QFIT8)
	ELSE IF (QFIT8) THEN
	    WRITE (LABEL,150) EMIS,ORDINAL(IFIT)
  150	    FORMAT(F5.3,' Emittance graybody fit to ',A7,
     +		   ' highest spectral emittance of Bands 42-49')
	    CALL XLADD(IOUT,'HISTORY','METHOD',LABEL,ISTAT,
     &			'FORMAT','STRING',' ')
	    CALL XLADD(IOUT2,'HISTORY','METHOD',LABEL,ISTAT,
     &			'FORMAT','STRING',' ')
	    CALL EMISCAL3(IN,IOUT,IOUT2,ISL,ISS,NL,NS,IFIT,EMIS,SKYRAD,
     +			  RAD_LUT,TEMP_LUT,QFIT8)
	ELSE
C									refchan
     	    WRITE (LABEL,200) EMIS,IREF
  200	    FORMAT(F5.3,' emittance fit to reference Channel ',I2)
   	    CALL XLADD(IOUT,'HISTORY','METHOD',LABEL,ISTAT,
     &		       'FORMAT','STRING',' ')
  	    CALL XLADD(IOUT2,'HISTORY','METHOD',LABEL,ISTAT,
     &			'FORMAT','STRING',' ')
	    IREF = MOD(IREF,10)
	    IF (IREF .EQ. 0) IREF = 10
	    CALL EMISCAL(IN,IOUT,IOUT2,ISL,ISS,NL,NS,IREF,EMIS,SKYRAD,
     +			 RAD_LUT,TEMP_LUT)
	END IF
C								Close datasets
	CALL XVCLOSE(IN,ISTAT,' ')
	CALL XVCLOSE(IOUT,ISTAT,' ')
	CALL XVCLOSE(IOUT2,ISTAT,' ')
	RETURN
	END
C***********************************************************************
	SUBROUTINE EMISCAL(IN,IOUT,IOUT2,ISL,ISS,NL,NS,IREF,EMIS,
     +			   SKYRAD,RAD_LUT,TEMP_LUT)
C
C	This routine computes emissivities, using the reference channel
C	method.
C
	IMPLICIT NONE
	REAL EMIS,SKYRAD(10),BUFIN(5000),BBRAD(5000,10),OUTBUF(5000),X
	REAL FRAC,TEMP
	INTEGER IN,IOUT,IOUT2,ISL,ISS,NL,NS,IREF,ILINE,ISAMP,ISTAT,I,J
	INTEGER ICHAN,INDEX
	INTEGER*2 RAD_LUT(40000,10),TEMP_LUT(32767,10)
C
	ILINE = ISL
	DO I=1,NL
	    CALL XVREAD(IN,BUFIN,ISTAT,'LINE',ILINE,'BAND',IREF,' ')
	    ISAMP = ISS
	    DO J=1,NS
		X = 1000.0*(BUFIN(ISAMP)-(1.0-EMIS)*SKYRAD(IREF)) / EMIS
		INDEX = MIN(32766.0,MAX(1.0,X))
		FRAC = X - INDEX
		TEMP = ((1.0-FRAC)*TEMP_LUT(INDEX,IREF) +
     +			FRAC*TEMP_LUT(INDEX+1,IREF)) / 100.0
		OUTBUF(J) = TEMP
		INDEX = 100.0 * (TEMP + 273.15)
		INDEX = MIN(39999,MAX(1,INDEX))
		FRAC = (100.0 * (TEMP + 273.15)) - INDEX
		DO ICHAN=1,10
		    BBRAD(J,ICHAN) = ((1.0-FRAC)*RAD_LUT(INDEX,ICHAN) +
     +				   FRAC*RAD_LUT(INDEX+1,ICHAN)) / 1000.0
C                                               BBRAD(J,ICHAN) is the blackbody
C                                               radiance of channel ICHAN at
C                                               temperature TEMP.
		END DO
		ISAMP = ISAMP+1
	    END DO
	    CALL XVWRIT(IOUT2,OUTBUF,ISTAT,' ')
	    DO ICHAN=1,10
		CALL XVREAD(IN,BUFIN,ISTAT,'LINE',ILINE,
     +			    'BAND',ICHAN,' ')
		ISAMP = ISS
		DO J=1,NS
		    OUTBUF(J) = (BUFIN(ISAMP)-SKYRAD(ICHAN)) /
     +				(BBRAD(J,ICHAN)-SKYRAD(ICHAN))
		    ISAMP = ISAMP+1
		END DO
		CALL XVWRIT(IOUT,OUTBUF,ISTAT,'LINE',I,'BAND',ICHAN,' ')
	    END DO
	    ILINE = ILINE+1
	END DO
	RETURN
	END
C***********************************************************************
	SUBROUTINE EMISCAL2(IN,IOUT,IOUT2,ISL,ISS,NL,NS,IFIT,EMIS,
     +			    SKYRAD,RAD_LUT,TEMP_LUT)
C
C	This routine computes emissivities, using the blackbody (graybody)
C	fitting method.
C
	IMPLICIT NONE
	REAL SKYRAD(10),BUFIN(5000,10),OUTBUF(5000,10),TBUF(5000)
	REAL EMIS,X,TEMP,FRAC,TEMPX(10)
	INTEGER IN,IOUT,IOUT2,ISL,ISS,NL,NS,IEL,ILINE,ISAMP,ICHAN,ISTAT
	INTEGER IFIT,INDEX
	INTEGER*2 RAD_LUT(40000,10),TEMP_LUT(32767,10)
C
	IFIT = 11 - IFIT
C
	IEL = ISL + NL - 1
	DO ILINE=ISL,IEL
	    DO ICHAN=1,10
		CALL XVREAD(IN,BUFIN(1,ICHAN),ISTAT,'LINE',ILINE,
     +			    'SAMP',ISS,'NSAMPS',NS,'BAND',ICHAN,' ')
	    END DO
	    DO ISAMP=1,NS
C						Find the max temperature that is
C						consistent with the given max e
		DO ICHAN=1,10
		    X = 1000.0 * (BUFIN(ISAMP,ICHAN) -
     +				  (1.0-EMIS)*SKYRAD(ICHAN)) / EMIS
		    INDEX = MIN(32766.0,MAX(1.0,X))
		    FRAC = X - INDEX
		    TEMPX(ICHAN) = ((1.0-FRAC)*TEMP_LUT(INDEX,ICHAN) +
     +				   FRAC*TEMP_LUT(INDEX+1,ICHAN)) / 100.0
		END DO
		CALL SORTR(TEMPX,10)
		TEMP = TEMPX(IFIT)
		TBUF(ISAMP) = TEMP
		INDEX = 100.0 * (TEMP + 273.15)
		INDEX = MIN(39999,MAX(1,INDEX))
		DO ICHAN=1,10
		    OUTBUF(ISAMP,ICHAN) = 
     +			(BUFIN(ISAMP,ICHAN)-SKYRAD(ICHAN)) /
     +			((RAD_LUT(INDEX,ICHAN)/1000.0)-SKYRAD(ICHAN))
		END DO
	    END DO
	    CALL XVWRIT(IOUT2,TBUF,ISTAT,' ')
	    DO ICHAN=1,10
		CALL XVWRIT(IOUT,OUTBUF(1,ICHAN),ISTAT,
     +			    'LINE',ILINE-ISL+1,'BAND',ICHAN,' ')
	    END DO
	END DO
	RETURN
	END
C***********************************************************************
	SUBROUTINE EMISCAL3(IN,IOUT,IOUT2,ISL,ISS,NL,NS,IFIT,EMIS,
     +			    SKYRAD,RAD_LUT,TEMP_LUT)
C
C	This routine computes emissivities, using the blackbody (graybody)
C	fitting method, but excludes Bands 41 and 50 from the list of
C	bands used for the fit.
C
	IMPLICIT NONE
	REAL SKYRAD(10),BUFIN(5000,10),OUTBUF(5000,10),TBUF(5000)
	REAL EMIS,X,TEMP,FRAC,TEMPX(10)
	INTEGER IN,IOUT,IOUT2,ISL,ISS,NL,NS,IEL,ILINE,ISAMP,ICHAN,ISTAT
	INTEGER IFIT,INDEX
	INTEGER*2 RAD_LUT(40000,10),TEMP_LUT(32767,10)
C
	IFIT = 9 - IFIT
C
	IEL = ISL + NL - 1
	DO ILINE=ISL,IEL
	    DO ICHAN=1,10
		CALL XVREAD(IN,BUFIN(1,ICHAN),ISTAT,'LINE',ILINE,
     +			    'SAMP',ISS,'NSAMPS',NS,'BAND',ICHAN,' ')
	    END DO
	    DO ISAMP=1,NS
C						Find the max temperature that is
C						consistent with the given max e
		DO ICHAN=2,9
		    X = 1000.0 * (BUFIN(ISAMP,ICHAN) -
     +				  (1.0-EMIS)*SKYRAD(ICHAN)) / EMIS
		    INDEX = MIN(32766.0,MAX(1.0,X))
		    FRAC = X - INDEX
		    TEMPX(ICHAN-1) = ((1.0-FRAC)*TEMP_LUT(INDEX,ICHAN) +
     +				   FRAC*TEMP_LUT(INDEX+1,ICHAN)) / 100.0
		END DO
		CALL SORTR(TEMPX,8)
		TEMP = TEMPX(IFIT)
		TBUF(ISAMP) = TEMP
		INDEX = 100.0 * (TEMP + 273.15)
		INDEX = MIN(39999,MAX(1,INDEX))
		DO ICHAN=1,10
		    OUTBUF(ISAMP,ICHAN) =
     +			(BUFIN(ISAMP,ICHAN)-SKYRAD(ICHAN)) /
     +			((RAD_LUT(INDEX,ICHAN)/1000.0)-SKYRAD(ICHAN))
		END DO
	    END DO
	    CALL XVWRIT(IOUT2,TBUF,ISTAT,' ')
	    DO ICHAN=1,10
		CALL XVWRIT(IOUT,OUTBUF(1,ICHAN),ISTAT,
     +			    'LINE',ILINE-ISL+1,'BAND',ICHAN,' ')
	    END DO
	END DO
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create masteremis.pdf
Process help=*
parm  INP	(string,60)	count=3 +
  default=("dummy", +
           "$V2TOP/luts/lut.master.rad.2004.aug", +
           "$V2TOP/luts/lut.master.temp.2004.aug")
parm  OUT	(string,40) 	count=2
parm  SIZE	integer	default=(1,1,0,0)	count=4
parm  SL	integer	default=1
parm  SS	integer	default=1
parm  NL	integer	default=0
parm  NS	integer	default=0
parm  MODE	keyword default=REFCHAN	valid=(REFCHAN,BBFIT,8CHANFIT)
parm  EMIS	real	default=0.95
parm  REFCHAN	integer	default=50 +
	valid=(1,2,3,4,5,6,7,8,9,10,41,42,43,44,45,46,47,48,49,50)
parm  FITCHAN	integer default=1 valid=1:10
parm  ORG       keyword valid=(BIL,BSQ)  default=BIL
End-proc

.TITLE
VICAR PROCESS MASTEREMIS
.HELP
PURPOSE:
     MASTEREMIS is a program to generate spectral emissivity images from
calibrated MASTER data. The primary input dataset should be the 10 calibrated
(IRAD or GRAD) MASTER TIR channels. The first output will contain the 10
spectral emittance images, while the second output will contain the temperature
image.
     Three algorithms (MODEs) have presently been implemented. The first 
method, called REFCHAN, sets a specified reference channel to a specified 
constant emittance value. The default sets Channel 50 to 0.95. The temperature
is computed based upon the radiance and given emittance of the reference 
channel, then the other nine spectral emittances are computed, using this 
temperature.
     The second method, called BBFIT, assumes that the highest spectral
emittance is at a user-specified level (default = 0.95). It then computes the
highest temperature consistent with this assumption, and the ten spectral
emittances, based upon the computed temperature.
     The third method, called 8CHANFIT, is the same as BBFIT, except that
Bands 41 and 50 are not used when searching for the channel to set to the
specified emittance.

WRITTEN BY:               Ron Alley     7/13/01
COGNIZANT PROGRAMMER:     Ron Alley

REVISION:                 1.2           11/10/04
.LEVEL1
.VARI INP
(1) MASTER TIR (10 channel)
    radiance image (RAD)
    i.e., the output file from 
    MASTERTIR
(2) MASTER Radiance lookup table
(3) MASTER Temperature lookup 
    table
.VARI OUT
(1) Spectral emittance image
(2) Temperature image
.VARI SIZE
The standard  VICAR output
size field.
.VARI SL
Starting line
.VARI SS
Starting sample
.VARI NL
Number of lines
.VARI NS
Number of samples
.VARI REFCHAN
(For REFCHAN mode only)
The channel to be used as the
reference channel. 
Valid: (41:50)
.VARI FITCHAN
(For BBFIT and 8CHANFIT modes
only) Specified EMIS value is
for the Nth (FITCHANth) 
highest emissivity
.VARI EMIS
The emittance assumed for the
reference channel (REFCHAN
mode); or the maximum
emittance (BBFIT or 8CHANFIT
mode)
.VARI MODE
REFCHAN, BBFIT, or 8CHANFIT
.VARI ORG
Organization of output dataset
.LEVEL2
.VARI INP
The first input dataset shall contain the upwelling radiance at surface data 
for the ten MASTER TIR channels (Channels 41 to 50), in units of Watts 
per sqare meter per steradian per micrometer.  Normally, this is the output 
dataset from MASTERTIR. 

The second file should be the MASTER TIR temperature to radiance lookup 
table for the spectral response calibration that is in effect for the date
of data acquisition.  These files are typically located in the directory 

          $V2TOP/luts

and have names of the form

         lut.master.rad.YEAR.MON

The third file should be the MASTER TIR radiance to temperature lookup 
table for the spectral response calibration that is in effect for the date
of data acquisition.  These files are typically located in the directory 

          $V2TOP/luts

and have names of the form

         lut.master.temp.YEAR.MON
.VARI OUT
     The first output dataset is a halfword image of all ten bands of MASTER
TIR emittance data, scaled such that an emittance of 1.0 is 10,000 DN.
     The second output dataset is a halfword temperature image, where each DN
corresponds to 0.01 degree Celsius. 0 DN corresponds to 0 degrees C.
.VARI SIZE
The standard VICAR2 output size field.   Default will calibrate
the entire data set.
	Example: SIZE = (1,1,200,500)
.VARI SL
Starting line (same as SIZE(1)).
.VARI SS
Starting sample (same as SIZE(2)).
.VARI NL
Number of lines (same as SIZE(3)).
.VARI NS
Number of samples (same as SIZE(4)).
.VARI REFCHAN
In the REFCHAN mode, this is the channel which is set to the value given
by the parameter EMIS.  In the BBFIT mode, this parameter is ignored. The
user may specify either the MASTER channel number (41 through 50), or the
input file band number (1 through 10).
		Example: REFCHAN = 5
                     or, REFCHAN = 45
(both have the same result.)
.VARI FITCHAN
In the BBFIT or 8CHANFIT mode, the Nth highest evissivity is assigned the 
value given by the parameter EMIS.  The FITCHAN parameter defines N.  That
is, for the default (FITCHAN=1), the highest emissivity is assigned the value
of EMIS.  If FITCHAN were set to 3, then the third highest emissivity would 
be set to EMIS.
.VARI EMIS
     In the REFCHAN mode, the spectral emittance of the reference channel is
set to a constant value, given by this parameter.
     In the BBFIT or 8CHANFIT mode, spectral emittances are computed so that 
the maximum of the emittance values is equal to the value of EMIS.
.VARI MODE
    Three algorithms (MODEs) have presently been implemented. The first method,
called REFCHAN, sets a specified reference channel to a specified constant
emittance value. The default sets Channel 50 to 0.95. The temperature is 
computed based upon the radiance and given emittance of the reference channel,
then the other nine spectral emittances are computed, using this temperature.
    The second method, called BBFIT, assumes that the highest spectral
emittance is at a user-specified level (default = 0.95). It then computes the
highest temperature consistent with this assumption, and the ten spectral
emittances that are consistent with the computed temperature.
    The third method, called 8CHANFIT, is the same as BBFIT, except that
Bands 41 and 50 are not used when searching for the channel to set to the
specified emittance.
.VARI ORG
ORG specifies the organizational format of the first (emissivity) output 
dataset. BIL (the default) and BSQ are supported.  
$ Return
$!#############################################################################
$Imake_File:
$ create masteremis.imake
#define  PROGRAM   masteremis

#define MODULE_LIST masteremis.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
