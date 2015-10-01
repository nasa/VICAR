$!****************************************************************************
$!
$! Build proc for MIPL module l7cal
$! VPACK Version 1.8, Wednesday, March 28, 2001, 19:06:58
$!
$! Execute by entering:		$ @l7cal
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
$ write sys$output "*** module l7cal ***"
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
$ write sys$output "Invalid argument given to l7cal.com file -- ", primary
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
$   if F$SEARCH("l7cal.imake") .nes. ""
$   then
$      vimake l7cal
$      purge l7cal.bld
$   else
$      if F$SEARCH("l7cal.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake l7cal
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @l7cal.bld "STD"
$   else
$      @l7cal.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create l7cal.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack l7cal.com -
	-s l7cal.f -
	-p l7cal.pdf -
	-i l7cal.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create l7cal.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
C	Jul 2, 2001   ...rea...  Update MODTRAN3 argument list
C
	COMMON /RADIO/ ML,ALTS,PRESS,TEMPS,H2OS,IH2OTYPE
	REAL ALTS(61),PRESS(61),TEMPS(61),H2OS(61)
C
	INTEGER*2 TEMP_LUT(32767)
	CHARACTER*2 SLMODEL, SLHUMID, SLOZONE, SLH2O, SLTEMP
	CHARACTER*80 MODFILE,MODTAB,RSTABLE
	LOGICAL XVPTST,GTEM,BTEM
C						Open datasets, get size field
	CALL XVUNIT(IN,'INP',1,ISTAT,' ')
	CALL XVOPEN(IN,ISTAT,'IO_ACT','SA','OPEN_ACT','SA','U_FORMAT',
     +		    'HALF',' ')
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
	CALL XVUNIT(IOUT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUT,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     +		    'U_FORMAT','HALF','O_FORMAT','HALF','OP','WRITE',
     +		    'U_NL',NL,'U_NS',NS,' ')
C							Get the other parameters
	GTEM = XVPTST('GTEM')
	BTEM = XVPTST('BTEM')
	CALL XVPARM('AMODEL',SLMODEL,NUM,IDEF,0)
	CALL XVPARM('AHUMID',SLHUMID,NUM,IDEF,0)
	CALL XVPARM('AOZONE',SLOZONE,NUM,IDEF,0)
	CALL XVPARM('ATEMP',SLTEMP,NUM,IDEF,0)
	CALL XVPARM('DATUM',DATUM,NUM,IDEF,0)
	CALL XVPARM('WTYPE',SLH2O,NUM,IDEF,0)
	CALL XVPARM('O3FAC',O3FAC,NUM,IDEF,0)
	CALL XVPARM('O2FAC',O2FAC,NUM,IDEF,0)
	CALL XVPARM('CO2FAC',CO2FAC,NUM,IDEF,0)
	CALL XVPARM('SO2FAC',SO2FAC,NUM,IDEF,0)
	CALL XVPARM('CH4FAC',CH4FAC,NUM,IDEF,0)
	CALL XVPARM('WATERFAC',WATERFAC,NUM,IDEF,0)
	CALL XVPARM('E',EMIS,NUM,IDEF,0)
C						if radiosonde input, get the
C						values at each layer
	IF (SLMODEL .EQ. 'RS') THEN
	    CALL XVPARM('RSTABLE',RSTABLE,NUM,IDEF,0)
	    IF (NUM .EQ. 0) THEN
		CALL XVPARM('NLAYERS',ML,NUM,IDEF,0)
		CALL XVPARM('ALTITUDE',ALTS,NUM,IDEF,0)
		CALL XVPARM('PRESSURE',PRESS,NUM,IDEF,0)
		CALL XVPARM('TEMP',TEMPS,NUM,IDEF,0)
		CALL XVPARM('WATER',H2OS,NUM,IDEF,0)
	    ELSE
		CALL READ_RSTABLE(RSTABLE)
	    END IF
	END IF
C						if necessary, convert
C						temperature profiles to Celsius
	IF (XVPTST('KELVIN') .AND. ML.GT.0) THEN
	    DO I=1,ML
		TEMPS(I) = TEMPS(I) - 273.15
	    END DO
	END IF
C								report files
	CALL XVPARM('MODFILE',MODFILE,NUM,IDEF,0)
	IF (NUM .EQ. 0) MODFILE='dummy'
	CALL XVPARM('MODTAB',MODTAB,NUM,IDEF,0)
	IF (NUM .EQ. 0) MODTAB='dummy'
C							Interpret MODEL number
	IF(SLMODEL.EQ.'TR') THEN
	    LMODEL = 1
	ELSE IF(SLMODEL.EQ.'MS') THEN
	    LMODEL = 2
	ELSE IF(SLMODEL.EQ.'MW') THEN
	    LMODEL = 3
	ELSE IF(SLMODEL.EQ.'SS') THEN
	    LMODEL = 4
	ELSE IF(SLMODEL.EQ.'SW') THEN
	    LMODEL = 5
	ELSE IF(SLMODEL.EQ.'ST') THEN
	    LMODEL = 6
	ELSE IF(SLMODEL.EQ.'RS') THEN
	    LMODEL = 7
	END IF
C
	IF(SLMODEL.NE.'RS') THEN
	    IF(SLHUMID.EQ.'  ') THEN
		LHUMID = LMODEL
	    ELSE IF(SLHUMID.EQ.'TR') THEN
		LHUMID = 1
	    ELSE IF(SLHUMID.EQ.'MS') THEN
		LHUMID = 2
	    ELSE IF(SLHUMID.EQ.'MW') THEN
		LHUMID = 3
	    ELSE IF(SLHUMID.EQ.'SS') THEN
		LHUMID = 4
	    ELSE IF(SLHUMID.EQ.'SW') THEN
		LHUMID = 5
	    ELSE IF(SLHUMID.EQ.'ST') THEN
		LHUMID = 6
	    END IF
	END IF
C
	IF(SLOZONE.EQ.'  ') THEN
	    IF (LMODEL.NE.7) THEN
		LOZONE = LMODEL
	    ELSE
		LOZONE = 2
	    END IF
	ELSE IF(SLOZONE.EQ.'TR') THEN
	    LOZONE = 1
	ELSE IF(SLOZONE.EQ.'MS') THEN
	    LOZONE = 2
	ELSE IF(SLOZONE.EQ.'MW') THEN
	    LOZONE = 3
	ELSE IF(SLOZONE.EQ.'SS') THEN
	    LOZONE = 4
	ELSE IF(SLOZONE.EQ.'SW') THEN
	    LOZONE = 5
	ELSE IF(SLOZONE.EQ.'ST') THEN
	    LOZONE = 6
	END IF
C
	IF(SLTEMP.EQ.'  ') THEN
	    LTEMP = 0
	ELSE IF(SLTEMP.EQ.'TR') THEN
	    LTEMP = 1
	ELSE IF(SLTEMP.EQ.'MS') THEN
	    LTEMP = 2
	ELSE IF(SLTEMP.EQ.'MW') THEN
	    LTEMP = 3
	ELSE IF(SLTEMP.EQ.'SS') THEN
	    LTEMP = 4
	ELSE IF(SLTEMP.EQ.'SW') THEN
	    LTEMP = 5
	ELSE IF(SLTEMP.EQ.'ST') THEN
	    LTEMP = 6
	END IF
C
	IF (SLH2O.NE.'  ') THEN
	    IF (SLH2O.EQ.'DP') IH2OTYPE=1
	    IF (SLH2O.EQ.'RH') IH2OTYPE=2
	    IF (SLH2O.EQ.'DE') IH2OTYPE=3
	END IF
C							  load  temperature LUT
	IF (GTEM .OR. BTEM) THEN
	    CALL XVUNIT(IN2,'INP',2,ISTAT,' ')
	    CALL XVOPEN(IN2,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',' ')
	    CALL XVREAD(IN2,TEMP_LUT,ISTAT,' ')
	    CALL XVCLOSE(IN2,ISTAT,' ')
	END IF
C
C				      get atmospheric radiance and transmittance
C				      get sky radiance
C
	CALL ATMOS1(LMODEL,LTEMP,LHUMID,LOZONE,DATUM,MODFILE,MODTAB,
     &		    WATERFAC,CO2FAC,O3FAC,O2FAC,CH4FAC,SO2FAC,0.0,
     &		    TRAN,RADUP,RADDOWN)
	CALL XLADD(IOUT,'HISTORY','SKYRAD',RADDOWN,ISTAT,'FORMAT',
     +		   'REAL',' ')
C									GTEM
	IF(GTEM) THEN
	    CALL XLADD(IOUT,'HISTORY','LBL1','Ground Temperature Image',
     &			ISTAT,'FORMAT','STRING',' ')
	    CALL XLADD(IOUT,'HISTORY','LBL2',
     &		 'DN = 100*Degrees Celsius',ISTAT,'FORMAT','STRING',' ')
	    CALL GTEMCAL(IN,IOUT,ISL,ISS,NL,NS,TEMP_LUT,TRAN,RADUP,
     &			 RADDOWN,EMIS)
C									BTEM
	ELSE IF(BTEM) THEN
	    CALL XLADD(IOUT,'HISTORY','LBL1',
     &		'Brightness Temperature Image',ISTAT,
     &		'FORMAT','STRING',' ')
	    CALL XLADD(IOUT,'HISTORY','LBL2',
     &		 'DN = 100*Degrees Celsius',ISTAT,'FORMAT','STRING',' ')
	    EMIS = 1.0
	    CALL GTEMCAL(IN,IOUT,ISL,ISS,NL,NS,TEMP_LUT,TRAN,RADUP,
     &			 RADDOWN,EMIS)
C									GRAD
	ELSE
	    CALL XLADD(IOUT,'HISTORY','LBL1','Ground Radiance Image',
     &		       ISTAT,'FORMAT','STRING',' ')
	    CALL XLADD(IOUT,'HISTORY','LBL2',
     &		       'DN = milliwatts/(m*m*sr*micrometer)',
     &		       ISTAT,'FORMAT','STRING',' ')
	    CALL GRADCAL(IN,IOUT,ISL,ISS,NL,NS,TRAN,RADUP,RADDOWN,EMIS)
	ENDIF
C								Close datasets
	CALL XVCLOSE(IN,ISTAT,' ')
	CALL XVCLOSE(IOUT,ISTAT,' ')
	RETURN
	END
C***********************************************************************
	SUBROUTINE GRADCAL(IN,IOUT,ISL,ISS,NL,NS,TRAN,RADUP,RADDOWN,
     +			   EMIS)
C
C
	INTEGER*2 OUTBUF(5000),INBUF(5000)
C
	ILINE = ISL
	DO I=1,NL
	    CALL XVREAD(IN,INBUF,ISTAT,'LINE',ILINE,'SAMP',ISS,
     &			'NSAMPS',NS,' ')
	    DO J=1,NS
		NUM = NINT((INBUF(J)-RADUP)/TRAN - (1.0-EMIS)*RADDOWN)
		OUTBUF(J) = MIN(32767,MAX(-32768,NUM))
	    END DO
	    CALL XVWRIT(IOUT,OUTBUF,ISTAT,'LINE',I,' ')
	    ILINE = ILINE+1
	END DO
	RETURN
	END
C***********************************************************************
	SUBROUTINE GTEMCAL(IN,IOUT,ISL,ISS,NL,NS,TEMP_LUT,TRAN,RADUP,
     &			   RADDOWN,EMIS)
C
	INTEGER*2 OUTBUF(5000),INBUF(5000),TEMP_LUT(32767)
C
	ILINE = ISL
	DO I=1,NL
	    CALL XVREAD(IN,INBUF,ISTAT,'LINE',ILINE,'SAMP',ISS,
     &			'NSAMPS',NS,' ')
	    DO J=1,NS
		NUM = NINT(((INBUF(J)-RADUP)/TRAN - 
     +					(1.0-EMIS)*RADDOWN) / EMIS)
		NUM = MIN(32767,MAX(1,NUM))
		OUTBUF(J) = TEMP_LUT(NUM)
	    END DO
	    CALL XVWRIT(IOUT,OUTBUF,ISTAT,'LINE',I,' ')
	    ILINE = ILINE+1
	END DO
	RETURN
	END
C*******************************************************************************
	SUBROUTINE ATMOS1(IMODEL,IM1,IM2,IM3,DATUM,MODFILE,MODTAB,
     +			  WATERFAC,CO2FAC,O3FAC,O2FAC,CH4FAC,SO2FAC,
     +			  TRAN,RADUP,RADDOWN)
C
	INCLUDE 'pgminc'
	COMMON /PARB/PARB
	INTEGER PARB(xprdim)
C
	COMMON /RADIO/ ML,ALTS,PRESS,TEMPS,H2OS,IH2OTYPE
	REAL ALTS(61),PRESS(61),TEMPS(61),H2OS(61)
C
	REAL ARAD1(232),TRAN1(232),SKYRAD(232)
	CHARACTER*80 MSG,MODFILE,MODTAB
C
	REAL FILTER(301)/
     + 0.0000000, 0.0000559, 0.0001118, 0.0001235, 0.0000412, 0.0001000,
     + 0.0001588, 0.0001471, 0.0001588, 0.0001588, 0.0001765, 0.0002176,
     + 0.0002588, 0.0002176, 0.0003823, 0.0003117, 0.0003059, 0.0002882,
     + 0.0004000, 0.0004941, 0.0006411, 0.0007294, 0.0007882, 0.0010176,
     + 0.0010294, 0.0012764, 0.0015058, 0.0016823, 0.0020234, 0.0022587,
     + 0.0026057, 0.0028940, 0.0030469, 0.0031998, 0.0036116, 0.0039998,
     + 0.0042645, 0.0046703, 0.0048409, 0.0050703, 0.0052350, 0.0053350,
     + 0.0054703, 0.0056468, 0.0056762, 0.0057115, 0.0056997, 0.0057644,
     + 0.0057585, 0.0058820, 0.0057703, 0.0056997, 0.0058115, 0.0057291,
     + 0.0055997, 0.0056056, 0.0056115, 0.0056173, 0.0056232, 0.0056291,
     + 0.0056291, 0.0056291, 0.0056291, 0.0056291, 0.0056291, 0.0056203,
     + 0.0056115, 0.0056026, 0.0055938, 0.0055850, 0.0055762, 0.0055762,
     + 0.0055762, 0.0055762, 0.0055762, 0.0055762, 0.0055468, 0.0055174,
     + 0.0054879, 0.0054585, 0.0054291, 0.0054197, 0.0054103, 0.0054009,
     + 0.0053915, 0.0053821, 0.0053562, 0.0053303, 0.0053044, 0.0052785,
     + 0.0052527, 0.0052574, 0.0052621, 0.0052668, 0.0052715, 0.0052762,
     + 0.0052632, 0.0052503, 0.0052374, 0.0052244, 0.0052115, 0.0052080,
     + 0.0052044, 0.0052009, 0.0051974, 0.0051938, 0.0051927, 0.0051915,
     + 0.0051903, 0.0051891, 0.0051880, 0.0051856, 0.0051833, 0.0051809,
     + 0.0051785, 0.0051762, 0.0051703, 0.0051644, 0.0051585, 0.0051527,
     + 0.0051468, 0.0051244, 0.0051021, 0.0050797, 0.0050574, 0.0050350,
     + 0.0050365, 0.0050380, 0.0050394, 0.0050409, 0.0050091, 0.0049774,
     + 0.0049456, 0.0049139, 0.0048821, 0.0048856, 0.0048892, 0.0048927,
     + 0.0048962, 0.0048997, 0.0048609, 0.0048221, 0.0047833, 0.0047445,
     + 0.0047056, 0.0047021, 0.0046986, 0.0046950, 0.0046915, 0.0046880,
     + 0.0046786, 0.0046692, 0.0046598, 0.0046503, 0.0046409, 0.0045880,
     + 0.0045351, 0.0044821, 0.0044292, 0.0043762, 0.0043395, 0.0043027,
     + 0.0042659, 0.0042292, 0.0042198, 0.0042104, 0.0042010, 0.0041915,
     + 0.0041821, 0.0041680, 0.0041539, 0.0041398, 0.0041257, 0.0041115,
     + 0.0040727, 0.0040339, 0.0039951, 0.0039563, 0.0039174, 0.0039174,
     + 0.0039174, 0.0039174, 0.0039174, 0.0038727, 0.0038280, 0.0037833,
     + 0.0037386, 0.0036939, 0.0036916, 0.0036892, 0.0036869, 0.0036845,
     + 0.0036822, 0.0036322, 0.0035822, 0.0035322, 0.0034822, 0.0034751,
     + 0.0034681, 0.0034610, 0.0034539, 0.0034469, 0.0035116, 0.0036057,
     + 0.0035351, 0.0031998, 0.0032410, 0.0033998, 0.0033351, 0.0032116,
     + 0.0032822, 0.0031998, 0.0030057, 0.0033234, 0.0030528, 0.0032116,
     + 0.0033351, 0.0032057, 0.0031175, 0.0031175, 0.0031881, 0.0031292,
     + 0.0031292, 0.0031351, 0.0032998, 0.0030469, 0.0031292, 0.0031351,
     + 0.0030822, 0.0032351, 0.0033351, 0.0031234, 0.0032528, 0.0030998,
     + 0.0031587, 0.0031175, 0.0029822, 0.0028704, 0.0028528, 0.0026293,
     + 0.0026293, 0.0024999, 0.0024469, 0.0023822, 0.0022822, 0.0021058,
     + 0.0021175, 0.0016176, 0.0015999, 0.0017058, 0.0011529, 0.0012058,
     + 0.0012235, 0.0010117, 0.0008764, 0.0006411, 0.0006176, 0.0004412,
     + 0.0004059, 0.0004529, 0.0005823, 0.0004059, 0.0004294, 0.0005000,
     + 0.0004059, 0.0001765, 0.0002647, 0.0003059, 0.0000765, 0.0001353,
     + 0.0000529, 0.0001882, 0.0001000,-0.0001588, 0.0000647, 0.0001000,
     +-0.0001118, 0.0000294, 0.0000647, 0.0000176, 0.0000647,-0.0000647,
     + 0.0000765, 0.0000471,-0.0000706, 0.0001176, 0.0000882,-0.0000059,
     +-0.0001529,-0.0000882, 0.0000059, 0.0000000, 0.0000000, 0.0000000,
     + 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000,
     + 0.0000000/
C
C	Return the atmospheric radiance, transmittance, and sky radiance
C	in ARAD1, TRAN1, and SKYRAD respectively, indexed by wavenumber.
C
	CALL MODTRAN_INT(IMODEL,IM1,IM2,IM3,DATUM,ARAD1,TRAN1,SKYRAD,ML,
     +			 ALTS,PRESS,TEMPS,H2OS,IH2OTYPE,MODFILE,MODTAB,
     +			 WATERFAC,CO2FAC,O3FAC,O2FAC,CH4FAC,SO2FAC)
C
C	Convolve with the Normalized response function to get the
C	path radiance (RADUP), sky radiance (RADDOWN), and path
C	transmittance (TRAN).
C
	TRAN = 0.0
	RADUP = 0.0
	RADDOWN = 0.0
	DO I=1,301
	    WVLEN = FLOAT(I+999) / 100.0
	    WVNUM = 10000.0 / WVLEN
	    X = WVNUM - 768.0
	    INDEX = INT(X)
	    FRAC = X - FLOAT(INDEX)
	    TRAN = TRAN + FILTER(I)*((1.0-FRAC)*TRAN1(INDEX) +
     +				      FRAC*TRAN1(INDEX+1))
	    RADUP = RADUP + FILTER(I)*((1.0-FRAC)*ARAD1(INDEX) +
     +				        FRAC*ARAD1(INDEX+1))
	    RADDOWN = RADDOWN + FILTER(I)*((1.0-FRAC)*SKYRAD(INDEX) +
     +				            FRAC*SKYRAD(INDEX+1))
	END DO
C
C							get the units right
	RADUP = 1.0E+7 * RADUP
	RADDOWN = 1.0E+7 * RADDOWN / 3.14159
C
C							send RADDOWN value
C							back to PDF
	CALL XQINI(PARB,xprdim,xcont,ISTAT)
	CALL XQREAL(PARB,'SKYRAD',1,RADDOWN,xadd,ISTAT)      
	CALL XQOUT(PARB,ISTAT)
C
C	Report the results
C
	CALL XVMESSAGE(' ',' ')
	WRITE (MSG,100) TRAN
  100	FORMAT('Transmittance:',F9.5)
	CALL XVMESSAGE(MSG,' ')
	WRITE (MSG,200) RADUP
  200	FORMAT('Path Radiance:',F9.3,' mW/m^2/sr/um')
	CALL XVMESSAGE(MSG,' ')
	WRITE (MSG,300) RADDOWN
  300	FORMAT('Sky Radiance: ',F9.3,' mW/m^2/sr/um')
	CALL XVMESSAGE(MSG,' ')
	CALL XVMESSAGE(' ',' ')
C
	RETURN
	END
C****************************************************************************
	SUBROUTINE MODTRAN_INT(MODEL,M1,M2,M3,H2,PATHRAD,TRANSMIT,
     +			       SKYRAD,LAYERS,ALT,PRESS,TEMP,WATER,MTYPE,
     +                         MODFILE,MODTAB,WATERFAC,CO2FAC,O3FAC,
     +                         O2FAC,CH4FAC,SO2FAC)
C
C	Interface to the MODTRAN subroutine.
C
C	INCLUDE 'mod3files'
	REAL WAVELEN(525),TRANSMIT(*),PATHRAD(*),SKYRAD(*)
	REAL ALT(61),PRESS(61),TEMP(61),WATER(61),AHAZE(61)
        REAL EQLWCZ(61),RRATZ(61),AWCCON(4),VX(47),WAVLEN(47)
        REAL EXTC(4,47),ABSC(4,47),ASYM(4,47),ANGF(50),F(4,50)
	REAL TITLE2D(18,4),SPECALB(8000)
        REAL ZCLD(16),CLD(16),CLDICE(16),RR(16)
	REAL WMOL(12,61)/732*0.0/
	REAL WMOLX(13,61)/793*0.0/
	INTEGER IHA1(61),ICLD1(61),IVUL1(61),ISEA1(61),ICHR1(61)
	INTEGER IREG(4)
	CHARACTER*80 BUF,MODFILE,MODTAB
	CHARACTER*80 MODLIB,SUN,DIRAC,UFTAPX
	CHARACTER*4 TITLEX(16)/'User',' Sup','plie','d Mo','del ',
     +                         11*'    '/
	CHARACTER*1 JCHAR(15,61)/915*' '/
	CHARACTER*1 JCHARX(61)/61*' '/
	CHARACTER*1 SYM(3)/'G','H','D'/
C
	CHARACTER*44 MODEL_NAME(7)
     +      /' TROPICAL TEMPERATURE AND PRESSURE          ',
     +       ' MIDLATITUDE SUMMER TEMPERATURE AND PRESSURE',
     +       ' MIDLATITUDE WINTER TEMPERATURE AND PRESSURE',
     +       ' SUBARCTIC SUMMER TEMPERATURE AND PRESSURE  ',
     +       ' SUBARCTIC WINTER TEMPERATURE AND PRESSURE  ',
     +       ' 1976 U.S. STANDARD ATMOSPHERE              ',
     +       ' USER SPECIFIED ATMOSPHERIC MODEL           '/
C
	CHARACTER*50 M2_NAME(6)
     +      /' TROPICAL WATER VAPOR PROFILE                     ',
     +       ' MIDLATITUDE SUMMER WATER VAPOR PROFILE           ',
     +       ' MIDLATITUDE WINTER WATER VAPOR PROFILE           ',
     +       ' SUBARCTIC SUMMER WATER VAPOR PROFILE             ',
     +       ' SUBARCTIC WINTER WATER VAPOR PROFILE             ',
     +       ' 1976 U.S. STANDARD ATMOSPHERE WATER VAPOR PROFILE'/
C
	CHARACTER*44 M3_NAME(6)
     +      /' TROPICAL OZONE PROFILE                     ',
     +       ' MIDLATITUDE SUMMER OZONE PROFILE           ',
     +       ' MIDLATITUDE WINTER OZONE PROFILE           ',
     +       ' SUBARCTIC SUMMER OZONE PROFILE             ',
     +       ' SUBARCTIC WINTER OZONE PROFILE             ',
     +       ' 1976 U.S. STANDARD ATMOSPHERE OZONE PROFILE'/
C
	CHARACTER*43 IHAZE_NAME(9)
     +	    /' RURAL AEROSOLS, VISIBILITY - 23 KM.       ',
     +       ' RURAL AEROSOLS, VISIBILITY - 5 KM.        ',
     +       ' NAVY MARITIME AEROSOL ATTENUATION         ',
     +       ' MARITIME AEROSOLS, VISIBILITY - 23 KM.    ',
     +       ' URBAN AEROSOLS, VISIBILITY - 5 KM.        ',
     +       ' TROPOSPHERIC AEROSOLS, VISIBILITY - 50 KM.',
     +       ' INVALID AEROSOL OPTION                    ',
     +       ' ADVECTION FOG, VISIBILITY - 0.2 KM.       ',
     +       ' RADIATION FOG, VISIBILITY - 0.5 KM.       '/
C							   hard wired "defaults"
	IV1 = 769
	IV2 = 1000
	IDV = 1
	IFWHM = 1
	ANGLE = 180.0
	IHAZE = 1
	H1 = 700.0
C
 	IF (MODEL .EQ. 7) THEN
	    IF (H2 .LT. ALT(1)) THEN
		CALL XVMESSAGE('The lowest atmospheric layer must',' ')
		CALL XVMESSAGE('be at least as low as the DATUM value',' ')
		CALL ABEND
	    END IF
 	    IF (M3 .EQ. 0) M3=6
 	    M4 = 6
 	    M5 = 6
 	    M6 = 6
 	    MDEF = 1
 	ELSE
 	    IF (M2 .EQ. 0) M2=MODEL
 	    IF (M3 .EQ. 0) M3=MODEL
 	    M4 = MODEL
 	    M5 = MODEL
 	    M6 = MODEL
 	    MDEF = 0
 	END IF
C							       report parameters
	CALL XVMESSAGE(' ',' ')
	CALL XVMESSAGE('       ***MODTRAN PARAMETERS***',' ')
	IF (M1.EQ.0) THEN
	    CALL XVMESSAGE(MODEL_NAME(MODEL),' ')
	ELSE
	    CALL XVMESSAGE(MODEL_NAME(M1),' ')
	END IF
	IF (MODEL.EQ.7) THEN
	    IF (MTYPE.EQ.1) THEN
		CALL XVMESSAGE(
     +			'   ALTITUDE  PRESSURE    TEMP    DEWPOINT',' ')
		CALL XVMESSAGE( 
     +			'     (KM.)     (MB.)   (DEG. C)  (DEG. C)',' ')
	    ELSE IF (MTYPE.EQ.2) THEN
		CALL XVMESSAGE(
     +			'   ALTITUDE  PRESSURE    TEMP    HUMIDITY',' ')
		CALL XVMESSAGE(
     +			'     (KM.)     (MB.)   (DEG. C)     (%)  ',' ')
	    ELSE
		CALL XVMESSAGE(
     +			'   ALTITUDE  PRESSURE    TEMP   VAPOR DEN',' ')
		CALL XVMESSAGE(
     +			'     (KM.)     (MB.)   (DEG. C) (GM/M**3)',' ')
	    END IF
	    DO I=1,LAYERS
		WRITE (BUF,100) ALT(I),PRESS(I),TEMP(I),WATER(I)
  100		FORMAT(F10.3,3F10.2)
		CALL XVMESSAGE(BUF,' ')
	    END DO
	ELSE
	    CALL XVMESSAGE(M2_NAME(M2),' ')
	    CALL XVMESSAGE(M3_NAME(M3),' ')
	END IF
C
	WRITE (BUF,110) WATERFAC
  110	FORMAT(' Water Rescaling Factor =',F6.3)
	CALL XVMESSAGE(BUF,' ')
C
	WRITE (BUF,120) O3FAC
  120	FORMAT(' Ozone Rescaling Factor =',F6.3)
	CALL XVMESSAGE(BUF,' ')
C
	WRITE (BUF,130) CO2FAC
  130	FORMAT(' Carbon Dioxide Rescaling Factor =',F6.3)
	CALL XVMESSAGE(BUF,' ')
C
	WRITE (BUF,140) SO2FAC
  140	FORMAT(' Sulfur Dioxide Rescaling Factor =',F6.3)
	CALL XVMESSAGE(BUF,' ')
C
	WRITE (BUF,150) CH4FAC
  150	FORMAT(' Methane Rescaling Factor =',F6.3)
	CALL XVMESSAGE(BUF,' ')
C
	WRITE (BUF,160) O2FAC
  160	FORMAT(' Diatomic Oxygen Rescaling Factor =',F6.3)
	CALL XVMESSAGE(BUF,' ')
C
	IF (IHAZE.GT.0) CALL XVMESSAGE(IHAZE_NAME(IHAZE),' ')
C
	WRITE (BUF,200) H1
  200	FORMAT(' Sensor Altitude =',F9.3,' km')
	CALL XVMESSAGE(BUF,' ')
C
	WRITE (BUF,210) H2
  210	FORMAT(' Target Altitude =',F9.3,' km')
	CALL XVMESSAGE(BUF,' ')
C
	WRITE (BUF,300) ANGLE
  300	FORMAT(' Angle =',F7.2,' Degrees')
	CALL XVMESSAGE(BUF,' ')
C
	WRITE (BUF,400) IV1,IV2,IDV
  400	FORMAT(' Spectral Range =',I6,' to',I6,' with',I3,
     +		   ' cm-1 Step Size')
	CALL XVMESSAGE(BUF,' ')
C					JCHAR gives units codes for pressure,
C					temperature, and moisture. WMOL(1,I)
C					is the final home for moisture content.
	DO I=1,LAYERS
	    WMOL(1,I) = WATER(I)
	    JCHAR(1,I) = 'A'
	    JCHAR(2,I) = 'B'
	    JCHAR(3,I) = SYM(MTYPE)
	END DO
C					Make albedo=0; this is probably
C					unnecessary -- but just to be safe.
	DO I=1,IV2
	    SPECALB(I) = 0.0
	END DO
C
	IF (MODEL .EQ. 7) THEN
	    IM = 1
	ELSE
	    IM = 0
	END IF
C
	IEMSCT = 1
	ITYPE = 2
	CO2MX = 330.0 * CO2FAC
C						generate correct input filenames
        CALL XGETENV_VIC('VICARMODTRAN35',MODLIB)
        SUN = MODLIB(1:LNBLNK(MODLIB)) // '/sun3'
        DIRAC = MODLIB(1:LNBLNK(MODLIB)) // '/MOLBMP96.BIN'
        UFTAPX = MODLIB(1:LNBLNK(MODLIB)) // '/CFCBMP96.ASC'
C
	CALL MODTRAN3(MODFILE,MODTAB,PLTABDEF,SUN,DIRAC,UFTAPX,
     +                WATERFAC,O3FAC,O2FAC,CH4FAC,SO2FAC,0.0,
     +		      'T',MODEL,ITYPE,IEMSCT,1,M1,M2,M3,M4,M5,M6,MDEF,
     +                    IM,0,0.0,SPECALB,
     +                .FALSE.,0,.TRUE.,5,CO2MX,
     +		      IHAZE,0,0,1,0,0,0.0,0.0,0.0,0.0,H2,
     +		      H1,H2,ANGLE,0.0,0.0,0.0,0,0.0,
     +                IV1,IV2,IDV,IFWHM,' ',' ','        ',
     +                0.0,0.0,0.0,0,0,0,0.0,0.0,0.0,0.0,0.0,0.0,
     +                0.0,0.0,0.0,
     +		      LAYERS,0,0,TITLEX,
     +		      ALT,PRESS,TEMP,WMOL,WMOLX,JCHAR,JCHARX,
     +		     AHAZE,EQLWCZ,RRATZ,IHA1,ICLD1,IVUL1,ISEA1,ICHR1,
     +		     IREG,AWCCON,TITLE2D,WAVLEN,VX,EXTC,ABSC,ASYM,
     +               ZCLD,CLD,CLDICE,RR,
     +		     0,0,0,0,
     +		     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
     +		     0,ANGF,F,WAVELEN,TRANSMIT,PATHRAD,SKYRAD)
	RETURN
	END
C*******************************************************************************
	SUBROUTINE READ_RSTABLE(RSTABLE)
C
C      This subroutine reads records from the file RSTABLE, and fills the
C      appropriate radiosonde arrays.  If more than 61 entries are 
C      encountered, the routine aborts.  If something other than a 
C      numeric or delimiter is encountered prior to finding four numeric
C      fields, the entire line is discarded.
C
	COMMON /RADIO/ ML,ALTS,PRESS,TEMPS,H2OS,IH2OTYPE
	REAL ALTS(61),PRESS(61),TEMPS(61),H2OS(61)
	CHARACTER*64 RSTABLE
C
	OPEN (51,FILE=RSTABLE,STATUS='OLD')
	ML = 1
  100	CONTINUE
	    READ (51,*,END=900,ERR=100) ALTS(ML),PRESS(ML),TEMPS(ML),
     +					H2OS(ML)
	    IF (PRESS(ML) .LT. 0.0) PRESS(ML)=1.0E-10
	    ML = ML+1
	    IF (ML .GT. 62) THEN
		CALL XVMESSAGE('Sonde table has too many layers',' ')
		CALL ABEND
	    ENDIF
	    GO TO 100
  900	CONTINUE
	ML = ML - 1
	CLOSE (51)
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create l7cal.pdf
Process help=*
local LSKYRAD   real    initial=0.0
parm  INP	(string,40)			count=1:2
parm  OUT	(string,40)
parm  SIZE	integer	default=(1,1,0,0)	count=4
parm  SL	integer	default=1
parm  SS	integer	default=1
parm  NL	integer	default=0
parm  NS	integer	default=0
parm  CALMODE	keyword	valid=(GRAD,GTEM,BTEM)
parm  DATUM	real	default=0.001
parm  AMODEL	string	valid=(TR,MS,MW,SS,SW,ST,RS)
parm  ATEMP	string	default="  "	valid=(TR,MS,MW,SS,SW,ST,"  ")
parm  AHUMID	string	default="  "	valid=(TR,MS,MW,SS,SW,ST,"  ")
parm  AOZONE	string	default="  "	valid=(TR,MS,MW,SS,SW,ST,"  ")
parm  RSTABLE   (string,64) count=0:1   default=--
parm  NLAYERS	integer	default=0
parm  WTYPE	keyword	default=RH	valid=(DP,RH,DE)
parm  TTYPE     keyword default=CELSIUS  valid=(KELVIN,CELSIUS)
parm  ALTITUDE	real	count=0:61	default=0.0
parm  PRESSURE	real	count=0:61	default=0.0
parm  TEMP	real	count=0:61	default=0.0
parm  WATER	real	count=0:61	default=0.0
parm  WATERFAC  real                    default=1.0
parm  O3FAC	real	                default=1.0
parm  CO2FAC	real	                default=1.0
parm  SO2FAC	real	                default=1.0
parm  CH4FAC	real	                default=1.0
parm  O2FAC	real	                default=1.0
parm  E		real	default=0.95
parm  MODFILE   (string,64) count=0:1   default=--
parm  MODTAB    (string,64) count=0:1   default=--
parm  SKYRAD    name    default=LSKYRAD 
End-proc

.TITLE
TAE PROCESS L7CAL
.HELP
PURPOSE:

   L7CAL is a program which calibrates the LANDSAT-7 thermal channel data 
to ground radiance or ground temperature. The input is LANDSAT-7 data which 
have already been calibrated to instrument radiance 
(in milliWatts/square meter/steradian/micrometer).

EXECUTION:

   The following is the execution statement format for L7CAL:

	L7CAL INP=PIX OUT=CALPIX PARAMS

   where INP, OUT, and PARAMS are parameters discussed in their res-
pective parameter sections. 



OPERATION:

   L7CAL takes as input the radiance at sensor for the LANDSAT-7 thermal
channel. It uses MODTRAN to compute the atmospheric path radiance, 
transmittance, and ground reflected atmospheric radiance. These values are 
convolved with the LANDSAT-7 spectral response functions and the instrument 
perceived radiances to obtain either ground radiance, ground temperature, or 
emittance.  The formula used to compute ground radiance is:

        IRAD = [GRAD + (1.0-e)*L   ]*Trans + L
                                sky           path

   The units output for radiance images are in milliwatts/m**2/um/sr. The units 
output for temperature images are in hundredths of degrees Celsius. Output 
is in halfword (two byte signed integer) format. 


WRITTEN BY:  Ron Alley  27 August 1999

COGNIZANT PROGRAMMER:  Ron Alley

REVISION: New

.LEVEL1
.VARI INP
(1) LANDSAT-7 thermal channel,
    radiance at sensor
    (mW/m^2/sr/um)
(2) Radiance to temperature
    lookup table (Not needed
    if CALMODE=GRAD)
.VARI OUT
Output data set of surface
radiance or temperature
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
.VARI CALMODE
This keyword selects the type
of decalibatrion to be performed
Valid: GRAD, GTEM, BTEM
.VARI AMODEL
Selects the model for which the
atmospheric corrections will
be performed via MODTRAN
.VARI ATEMP
Selects atmospheric temperature
and pressure profiles, if 
different form that defaulted
with AMODEL.
.VARI AHUMID
Selects the atmospheric water
vapor model, if different from
that defaulted with AMODEL.
.VARI AOZONE
Selects the ozone model, if
different from that defaulted
with AMODEL.
.VARI DATUM
Surface elevation (km above MSL)
.VARI NLAYERS
The number of atmospheric lay-
ers in the radiosonde data
(if used).
.VARI ALTITUDE
The altitudes of each of the
atmospheric layers given in
the radiosonde data. 
(km. above MSL)
.VARI PRESSURE
The atmospheric pressures of
each of the atmospheric lay-
ers given in the radiosonde
data. (mb.)
.VARI TEMP
The temperatures of the at-
mospheric layers given in
the radiosonde data. Units
are either Celsius (the default)
or Kelvin, if the KELVIN
keyword has been specified.
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
.VARI TTYPE
The units in which the 
temperature profiles are given.
Valid: KELVIN, CELSIUS
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
The assumed surface emissivity
.VARI RSTABLE
Dataset name for a table of
radiosonde style input values
.VARI MODFILE
Dataset name for MODTRAN
output report file.
.VARI MODTAB
Dataset name for MODTRAN
output tabular file.
.VARI SKYRAD
Output parameter - do not modify
.LEVEL2
.VARI INP
The file containing the LANDSAT-7 thermal channel radiance at sensor, in
milliWatts/square meter/steradian/micrometer.
.VARI OUT
Output data set in 16-bit integer format.  The exact type of data output 
(e.g. temperature, radiance, etc) is determined by the MODE parameter.
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
.VARI CALMODE
      This keyword selects the type of decalibatrion to be performed.
      There is a choice of three possibilities:
            GRAD - (Ground RADiance)
                   Radiance values of the ground target in the
                   scene, incorporating atmospheric corrections.
            GTEM - (Ground TEMperature)
                   Kinetic temperature of the ground target, assuming the
                   emissivity values given by the parameter "E"
            BTEM - (Brightness TEMperature)
                   Brightness temperature of the ground target.
      Example: MODE = GRAD
.VARI AMODEL
	This selects the model atmosphere for which the atmospheric 
        corrections will be computed.  The program uses a version of
	MODTRAN as a subroutine, to which AMODEL is passed.  The exact 
        parameters of each model is given in the MODTRAN handbook.  The 
        options in brief:
		TR - TRopical model atmosphere
		MS - Midlatitude Summer
		MW - Midlatitude Winter
		SS - Subarctic Summer
		SW - Subarctic Winter
		ST - U.S. STandard
                RS - User supplied (e.g. Radiosonde) model atmosphere
	These same abbreviations are used with the HUMID parameter, with
	the same meanings.
		Example: LMODEL = SW
.VARI ATEMP
The ATEMP parameter is used to modify the AMODEL given, with respect to 
its temperature and pressure profiles.  The choices are the same as those 
given in the AMODEL description and with the same meanings.  The ATEMP 
parameter will only change AMODEL's temperature and pressure profiles and 
nothing else.  Here are the valid strings:
		TR - TRopical model atmosphere
		MS - Midlatitude Summer
		MW - Midlatitude Winter
		SS - Subarctic Summer
		SW - Subarctic Winter
		ST - U.S. STandard
		Example: ATEMP = MW
.VARI AHUMID
	The AHUMID parameter is used to modify the AMODEL given, with
	respect to its water vapor content.  The choices are the same
	as those given in the AMODEL description and with the same
	meanings.  The AHUMID parameter will only change AMODEL's water
	vapor profile and nothing else.  Here are the valid strings:
		TR - TRopical model atmosphere
		MS - Midlatitude Summer
		MW - Midlatitude Winter
		SS - Subarctic Summer
		SW - Subarctic Winter
		ST - U.S. STandard
		Example: HUMID = MW
.VARI AOZONE
	The AOZONE parameter is used to modify the AMODEL given, with
	respect to its ozone concentrations. The choices are the same
	as those given in the AMODEL description and with the same
	meanings.  The AOZONE parameter will only change AMODEL's ozone
	concentration profile and nothing else.  Here are the valid strings:
		TR - TRopical model atmosphere
		MS - Midlatitude Summer
		MW - Midlatitude Winter
		SS - Subarctic Summer
		SW - Subarctic Winter
		ST - U.S. STandard
		Example: HUMID = MW
.VARI DATUM
	The DATUM is the mean elevation in kilometers of the targeted
	terrain	above mean sea level.  A default value of 1 meter is
	used if the user does not specify the DATUM explicitly. 
		Example: DATUM = 0.2
.VARI NLAYERS
If the atmospheric model to be used is user-defined (AMODEL=RS), NLAYERS
specifies the number of altitudes for which the atmospheric parameters are
expressed. NLAYERS has a minimum of 2 and a maximum of 61. It is not used
if one of the standard MODTRAN models is selected.
.VARI ALTITUDE
This parameter is used only if the atmospheric model to be used is user-defined
(AMODEL=RS). It specifies the altitudes of each of the NLAYERS of atmospheric 
layers given in the radiosonde data. The proper units are kilometers above mean
sea level.
.VARI PRESSURE
This parameter is used only if the atmospheric model to be used is user-defined
(AMODEL=RS). It specifies the pressure (in millibars) of each of the NLAYERS 
of atmospheric layers given in the radiosonde data. 
.VARI TEMP
This parameter is used only if the atmospheric model to be used is user-defined
(AMODEL=RS). It specifies the temperature of each of the NLAYERS of atmospheric
layers given in the radiosonde data.  The units of the temperature profile may
be either Celsius (the default), or Kelvin (if the keyword KELVIN has been
specified.
.VARI WATER
This parameter is used only if the atmospheric model to be used is user-defined
(AMODEL=RS). It specifies the moisture content of each of the NLAYERS of 
atmospheric layers given in the radiosonde data. It may be specified in units
of dewpoint temperature, grams per cubic meter, or relative humidity, as 
indicated by the WTYPE parameter.
.VARI WTYPE
This parameter is used only if the atmospheric model to be used is user-defined
(AMODEL=RS). It specifies the units used by the WATER parameter to denote
moisture content. The valid values are:
                       RH for relative humidity (percent of sauration)
                       DP for dewpoint temperature (degrees Celsius)
                   and DE for density (grams per cubic meter)
.VARI TTYPE
This parameter is used only if the atmospheric model to be used is user-defined
(AMODEL=RS). It specifies the units used by the TEMP parameter to denote
atmospheric temperature. The valid values are:
        CELSIUS - the default value
        KELVIN  
.VARI O3FAC
	Used to vary the impact of the ozone model in use. All ozone
        concentrations are multiplied by this factor prior to atmospheric
        parameter computation.
.VARI O2FAC
	Used to vary the impact of the oxygen model in use. All oxygen
        concentrations are multiplied by this factor prior to atmospheric
        parameter computation.
.VARI CO2FAC
	Used to vary the impact of the carbon dioxide model in use. All
        carbon dioxide concentrations are multiplied by this factor prior to 
        atmospheric parameter computation.
.VARI SO2FAC
	Used to vary the impact of the sulfur dioxide model in use. All
        sulfur dioxide concentrations are multiplied by this factor prior to 
        atmospheric parameter computation.
.VARI CH4FAC
	Used to vary the impact of the methane model in use. All methane
        concentrations are multiplied by this factor prior to atmospheric
        parameter computation.
.VARI WATERFAC
	Used to vary the impact of the moisture profile in use. All moisture
        concentrations are multiplied by this factor prior to atmospheric
        parameter computation.
.VARI E
        This is the emissivity assumed when computing the
        sky radiance reflected by the ground. In the GTEM mode, these values
        are also used to compute the ground kinetic temperature.
.VARI RSTABLE
   If the radiosonde (RS) atmospheric model has been selected, the user may
   input the model parameters via the VICAR parameters ALTITUDE, PRESSURE,
   TEMP, and WATER, or alternatively, supply an ASCII file containing these
   values.  If the values are supplied in a file, the name of the file
   should be given for the value of RSTABLE.
         The file must be organized in the following manner:
   Column 1   altitudes in ascending elevations, units are kilometers
              above mean sea level
   Column 2   pressures in millibars
   Column 3   temperatures
   Column 4   moisture content

   If there are more than 4 columns, the additional columns are ignored.
   Column headers are permitted and ignored.  The user need not specify
   the number of layers, but if more than 61 layers are contained in the 
   file, the program will abort.
.VARI MODFILE
   Normally, the report file generated by MODTRAN is discarded at the 
   conclusion of the MODTRAN run.  If a dataset name is given as the 
   parameter value to MODFILE, the report of MODTRAN is saved in that dataset.
.VARI MODTAB
   Normally, the tabular file generated by MODTRAN is discarded at the 
   conclusion of the MODTRAN run.  If a dataset name is given as the 
   parameter value to MODTAB, the report of MODTRAN is saved in that dataset.
$ Return
$!#############################################################################
$Imake_File:
$ create l7cal.imake
#define  PROGRAM   l7cal

#define MODULE_LIST l7cal.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN
#define FTNINC_LIST pgminc

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
#define LIB_MOD35
$ Return
$!#############################################################################
