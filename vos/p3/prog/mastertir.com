$!****************************************************************************
$!
$! Build proc for MIPL module mastertir
$! VPACK Version 1.8, Wednesday, November 10, 2004, 18:28:39
$!
$! Execute by entering:		$ @mastertir
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
$ write sys$output "*** module mastertir ***"
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
$ write sys$output "Invalid argument given to mastertir.com file -- ", primary
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
$   if F$SEARCH("mastertir.imake") .nes. ""
$   then
$      vimake mastertir
$      purge mastertir.bld
$   else
$      if F$SEARCH("mastertir.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake mastertir
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @mastertir.bld "STD"
$   else
$      @mastertir.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create mastertir.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack mastertir.com -
	-s mastertir.f -
	-p mastertir.pdf -
	-i mastertir.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create mastertir.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C
C	VICAR program MASTERTIR, used to calculate upwelling radiance at
C	surface from radiance at sensor for the MASTER instrument, thermal IR
C	channels (Bands 41-50).
C
C	Ron Alley	12 August 1998
C	Revision: 1.1   Report calibration file name          rea  9/21/98
C	Revision: 1.2   get filenames from tape5              rea  5/21/99
C	Revision: 1.3   integrate over wavelengths, rather than 
C			wavenumbers                           rea 10/21/99
C	Revision: 1.3.1 Update MODTRAN3 arguments             rea  7/02/01
C	Revision: 1.4   Change output from halfword to real   rea 11/12/04
C
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
	REAL RESP(800,10),TRANX(716,10),PATHX(716,10)
	CHARACTER*80 MODINP,MODOUT,MODTAB,INPUT_NAMES(2)
	CHARACTER*80 FULLFILE,TABFILE
C						Open datasets, get size field
	CALL XVUNIT(IN,'INP',1,ISTAT,' ')
	CALL XVOPEN(IN,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     +		    'U_FORMAT','REAL',' ')
	CALL XVUNIT(INCAL,'INP',2,ISTAT,' ')
	CALL XVOPEN(INCAL,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',' ')
	CALL XVUNIT(IOUT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUT,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     +	           'OP','WRITE','U_FORMAT','REAL','O_FORMAT','REAL',
     +		   'U_ORG','BSQ','U_NB',10,'U_NL',NL,'U_NS',NS,' ')
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
	IF (NS .NE. 716) THEN
	    IF (NSIN .NE. 716) THEN
		CALL XVMESSAGE('Input image must be 716 pixels wide',' ')
		CALL ABEND
	    ENDIF
	    CALL XVMESSAGE('WARNING: NS parameter is ignored',' ')
	    CALL XVMESSAGE('The full image width will be processed',' ')
	END IF
C							      update VICAR label
	CALL XLADD(IOUT,'HISTORY','LBL1','MASTER Ground Radiance Image',
     +		   ISTAT,'FORMAT','STRING',' ')
	CALL XLADD(IOUT,'HISTORY','LBL2',
     +		       'DN = Watts/(m*m*sr*micrometer)',
     +		       ISTAT,'FORMAT','STRING',' ')
	CALL XLADD(IOUT,'HISTORY','LBL3',
     +		       'skyrad units = milliwatts/(m*m*micrometer)',
     +		       ISTAT,'FORMAT','STRING',' ')
	CALL XVPARM('INP',INPUT_NAMES,NUM,IDEF,2)
	CALL XLADD(IOUT,'HISTORY','CAL_FILE',INPUT_NAMES(2),ISTAT,
     +		   'FORMAT','STRING',' ')
C							Get the other parameters
	CALL XVPARM('MODINP',MODINP,NUM,IDEF,0)
C						Load spectral response functions
	DO ICHAN=1,10
	    CALL XVREAD(INCAL,RESP(1,ICHAN),ISTAT,' ')
	END DO
	CALL XVCLOSE(INCAL,ISTAT,' ')
C						Get MODTRAN parameters from
C						MODTRAN input file
	FULLFILE = 'dummy'
	TABFILE = 'dummy'
	CALL GET_MODTRAN_INPUT(MODINP,FULLFILE,TABFILE)
	CALL XVPARM('MODOUT',MODOUT,NUM,IDEF,0)
	IF (NUM .EQ. 0) MODOUT=FULLFILE
	CALL XVPARM('MODTAB',MODTAB,NUM,IDEF,0)
	IF (NUM .EQ. 0) MODTAB=TABFILE
	CALL REPORT_MODTRAN_PARMS(MODOUT,MODTAB)
C						Run MODTRAN
	CALL MODTRAN_INTERFACE(IOUT,RESP,MODOUT,MODTAB,TRANX,PATHX)
C						Do the atmospheric correction
	CALL GRADCAL(IN,IOUT,ISL,NL,TRANX,PATHX)
C								Close datasets
	CALL XVCLOSE(IN,ISTAT,' ')
	CALL XVCLOSE(IOUT,ISTAT,' ')
	RETURN
	END
C*******************************************************************************
	SUBROUTINE GET_MODTRAN_INPUT(MODINP,FULLFILE,TABFILE)
	CHARACTER*80 MODINP,FULLFILE,TABFILE
C
	PARAMETER(LAYDIM=61,NSPECX=13,NZCLD=16,NAER=7,NWAVLN=47)
	COMMON /MODPARS/H2O,O3,CH4,SO2,TEMPADJ,MODEL,M1,M2,M3,M4,M5,M6,
     +		MDEF,TBOUND,CO2MIX,IHAZE,ISEASN,IVULCN,ICSTL,VIS,WSS,
     +		WHH,H1,H2,ML,IRD1,IRD2,TITLE,ZMDL,P,T,WMOL,XMOL,JCHAR,
     +		JCHARX,AHAZE,EQLWCZ,IHA1,IVUL1,ISEA1,ICHR1,IPARM,IPH,
     +		IDAY,PARM1,PARM2,PARM3,PARM4,TIME,G
	REAL ZMDL(LAYDIM),P(LAYDIM),T(LAYDIM),AHAZE(LAYDIM)
	REAL WMOL(12,LAYDIM),XMOL(NSPECX,LAYDIM),EQLWCZ(LAYDIM)
	INTEGER IHA1(LAYDIM),IVUL1(LAYDIM),ISEA1(LAYDIM),ICHR1(LAYDIM)
	CHARACTER*4 TITLE(16)
	CHARACTER*1 JCHAR(15,LAYDIM),JCHARX(LAYDIM)
C
	REAL HOLD(NSPECX),RRATZ(LAYDIM),AWCCON(4)
	REAL WAVLEN(NWAVLN),VX(NWAVLN),EXTC(NAER,NWAVLN)
	REAL ABSC(NAER,NWAVLN),ASYM(NAER,NWAVLN),ANGF(50),F(4,50)
	REAL ZCLD(NZCLD),CLD(NZCLD),CLDICE(NZCLD),RR(NZCLD)
	INTEGER ICLD1(LAYDIM),IREG(4)
	CHARACTER*8 DLIMIT
	CHARACTER*4 TITLE2D(18,4)
	CHARACTER*1 YFLAG,XFLAG,CODE
	LOGICAL LDISORT,LSUN1
C								    open dataset
	OPEN (UNIT=81,FILE=MODINP,STATUS='OLD')
C                                                                       Card 1
	READ (81,100) CODE,MODEL,ITYPE,IEMSCT,IMULT,M1,M2,M3,M4,M5,M6,
     +		      MDEF,IM,NOPRNT,TBOUND,SALB  
  100	FORMAT(A1,I4,12I5,F8.3,F7.2)
C									Card 1A
	READ (81,120) LDISORT,ISTRM,LSUN1,ISUN,CO2MIX
  120	FORMAT(L1,I4,L1,I4,F10.3)
C                                                                       Card 2
	READ (81,200) IHAZE,ISEASN,IVULCN,ICSTL,ICLD,IVSA,VIS,WSS,WHH,
     +		      RAINRT,GNDALT
  200	FORMAT(6I5,5F10.3)
C                                                                       Card 2A
	IF (ICLD.GE.18) READ (81,210) CTHIK,CALT,CEXT,ISEED
  210	FORMAT(3F8.3,2I4,6F8.3)
	IF (ICLD.GE.1 .AND. ICLD.LE.10) READ (81,210) CTHIK,CALT,CEXT,
     +		NCRALT,NCRSPC,CWAVLN,CCOLWD,CCOLIP,CHUMID,ASYMWD,ASYMIP
C                                                                       Card 2B
	IF (IVSA.EQ.1) READ (81,220) ZCVSA,ZTVSA,ZINVSA
  220	FORMAT(3F10.3)
C                                                                       Card 2C
	IF (IM.EQ.1 .AND. (MODEL.EQ.0 .OR. MODEL.EQ.7)) THEN
	    READ (81,230) ML,IRD1,IRD2,TITLE
  230	    FORMAT(3I5,18A4)
	    DO I=1,ML
C                                                                       Card 2C1
		READ (81,240) ZMDL(I),P(I),T(I),WMOL(1,I),WMOL(2,I),
     +                    WMOL(3,I),JCHAR(1,I),JCHAR(2,I),JCHAR(3,I),
     +                    JCHAR(4,I),JCHAR(5,I),JCHAR(6,I),JCHAR(7,I),
     +                    JCHAR(8,I),JCHAR(9,I),JCHAR(10,I),JCHAR(11,I),
     +                    JCHAR(12,I),JCHAR(13,I),JCHAR(14,I),
     +                    JCHAR(15,I),JCHARX(I)
  240		FORMAT(F10.3,5E10.3,15A1,A1)
C
		IF (IRD1.EQ.1) THEN
C                                                                       Card 2C2
		    READ (81,250) WMOL(4,I),WMOL(5,I),WMOL(6,I),
     +			 	  WMOL(7,I),WMOL(8,I),WMOL(9,I),
     +				  WMOL(10,I),WMOL(11,I),WMOL(12,I)
  250		    FORMAT(8E10.3)
		    IF (MDEF .EQ. 2) THEN
C									Card 2CX
			READ (81,250) (HOLD(J),J=1,NSPECX)
			DO J=1,NSPECX
			    XMOL(J,I) = HOLD(J)
			END DO
		    END IF
		END IF
C                                                                       Card 2C3
		IF (IRD2.EQ.1) READ (81,260) AHAZE(I),EQLWCZ(I),
     +					     RRATZ(I),IHA1(I),ICLD1(I),
     +					     IVUL1(I),ISEA1(I),ICHR1(I)
  260		FORMAT(10X,3F10.3,5I5)
	    END DO
	END IF
C                                                                       Card 2D
	IF (IHAZE.EQ.7 .OR. ICLD.EQ.11) THEN
	    READ (81,280) IREG
  280	    FORMAT(4I5)
	    DO I=1,4
		IF (IREG(I).NE.0) THEN
C                                                                       Card 2D1
		    READ (81,283) AWCCON(I),(TITLE2D(J,I), J=1,17)
  283		    FORMAT(E10.3,17A4)
		    TITLE2D(18,I) = '    '
C                                                                       Card 2D2
		    READ (81,285) (VX(J),EXTC(I,J),ABSC(I,J),ASYM(I,J),
     +				  J=1,NWAVLN)
  285		    FORMAT(3(F6.2,2F7.5,F6.4))
		END IF
	    END DO
	END IF
C
	IF (ICLD.GE.1 .AND. ICLD.LE.10) THEN
C									Card 2E1
	    IF (NCRALT .GE. 2) THEN
		DO I=1,NCRALT
		    READ (81,292) ZCLD(I),CLD(I),CLDICE(I),RR(I)
  292		    FORMAT(4F10.5)
		END DO
	    END IF
C									Card 2E2
	    IF (NCRSPC .GE. 2) THEN
		DO I=1,NCRSPC
		    READ (81,296) WAVLEN(I),EXTC(6,I),ABSC(6,I),
     +				 ASYM(6,I),EXTC(7,I),ABSC(7,I),ASYM(7,I)
  296		    FORMAT(7F10.5)
		END DO
	    END IF
	END IF
C
	IF (IEMSCT.EQ.3) THEN
C                                                                       Card 3
C                                                                    (Alternate)
	    READ (81,300) H1,H2,ANGLE,IDAY,RO,ISOURC,ANGLEM
  300	    FORMAT(3F10.3,I5,5X,F10.3,I5,F10.3)
	ELSE
C                                                                       Card 3
	    READ (81,310) H1,H2,ANGLE,RANGE,BETA,RO,LEN,PHI
  310	    FORMAT(6F10.5,I5,5X,F10.5)
	    IF (IEMSCT.EQ.2) THEN
C                                                                       Card 3A1
		READ (81,320) IPARM,IPH,IDAY,ISOURC
  320		FORMAT(4I5)
C                                                                       Card 3A2
		READ(81,330) PARM1,PARM2,PARM3,PARM4,TIME,PSIPO,ANGLEM,G
  330		FORMAT(8F10.3)
		IF (IPH.EQ.1) THEN
C                                                                       Card 3B1
		    READ (81,340) NANGLS
  340		    FORMAT(I5)
		    DO I=1,NANGLS
C                                                                       Card 3B2
			READ(81,350) ANGF(I),F(1,I),F(2,I),F(3,I),F(4,I)
  350			FORMAT(5E10.3)
		    END DO
		END IF
	    END IF
	END IF
C                                                                       Card 4
	READ (81,400) IV1,IV2,IDV,IRES,YFLAG,XFLAG,DLIMIT
  400	FORMAT(4I10,2A1,A8)
C                                                                       Card 5
	READ (81,500) IRPT
  500	FORMAT(I5)
C                                                                 Rescaling Card
	H2O = 1.0
	CO2 = 1.0
	O3 = 1.0
	O2 = 1.0
	CH4 = 1.0
	SO2 = 1.0
	TEMPADJ = 0.0
	READ (81,600,END=690) H2O,CO2,O3,O2,CH4,SO2,TEMPADJ
  600	FORMAT(7F10.3)
C								  filenames card
	CALL READ_FILENAMES(FULLFILE,TABFILE)
C
  690	CONTINUE
	IF (CO2 .LE. 0.0) CO2 = 1.0E-20
	IF (CO2MIX .LE. 0.0) CO2MIX = 330.0
	CO2MIX = CO2MIX * CO2
C
	CLOSE(81)
	RETURN
	END
C*******************************************************************************
	SUBROUTINE READ_FILENAMES(FULLFILE,TABFILE)
C
	CHARACTER*140 TEXT
	CHARACTER*80 FULLFILE,TABFILE,IGNORE,GET_FILENAME
C							read a card
  100	CONTINUE
	READ (81,200,END=800) TEXT
  200	FORMAT(A139)
	LOC = 1
C							scan for keywords
	DO WHILE (LOC .LE. 140)
	    IF (TEXT(LOC:LOC) .EQ. 'F' .OR. TEXT(LOC:LOC) .EQ. 'f') THEN
		LOC = LOC + 1
		LOC1 = LOC + 1
		IF (TEXT(LOC:LOC1) .EQ. 'UL' .OR. 
     +		    TEXT(LOC:LOC1) .EQ. 'ul') THEN
C								       FULL file
		    LOC = LOC + 2
		    FULLFILE = GET_FILENAME(TEXT,LOC)
		END IF
	    ELSE IF(TEXT(LOC:LOC).EQ.'T' .OR. TEXT(LOC:LOC).EQ.'t') THEN
		LOC = LOC + 1
		LOC1 = LOC + 1
		IF (TEXT(LOC:LOC1) .EQ. 'AB' .OR. 
     +		    TEXT(LOC:LOC1) .EQ. 'ab') THEN
C									TAB file
		    LOC = LOC + 2
		    TABFILE = GET_FILENAME(TEXT,LOC)
		END IF
	    ELSE IF(TEXT(LOC:LOC).EQ.'P' .OR. TEXT(LOC:LOC).EQ.'p') THEN
		LOC = LOC + 1
		LOC1 = LOC + 1
		IF (TEXT(LOC:LOC1) .EQ. 'LT' .OR. 
     +		    TEXT(LOC:LOC1) .EQ. 'lt') THEN
C								      PLTAB file
		    LOC = LOC + 2
		    IGNORE = GET_FILENAME(TEXT,LOC)
		END IF
	    ELSE IF (TEXT(LOC:LOC).EQ.'S' .OR. TEXT(LOC:LOC).EQ.'s') THEN
		LOC = LOC + 1
		LOC1 = LOC + 1
		IF (TEXT(LOC:LOC1) .EQ. 'UN' .OR. 
     +		    TEXT(LOC:LOC1) .EQ. 'un') THEN
C									SUN file
		    LOC = LOC + 2
		    IGNORE = GET_FILENAME(TEXT,LOC)
		END IF
	    ELSE IF(TEXT(LOC:LOC).EQ.'R' .OR. TEXT(LOC:LOC).EQ.'r') THEN
		LOC = LOC + 1
		LOC1 = LOC + 1
		IF (TEXT(LOC:LOC1) .EQ. 'EF' .OR. 
     +		    TEXT(LOC:LOC1) .EQ. 'ef') THEN
C								       REFL file
		    LOC = LOC + 2
		    IGNORE = GET_FILENAME(TEXT,LOC)
		END IF
	    ELSE IF(TEXT(LOC:LOC).EQ.'D' .OR. TEXT(LOC:LOC).EQ.'d') THEN
		LOC = LOC + 1
		LOC1 = LOC + 1
		IF (TEXT(LOC:LOC1) .EQ. 'IR' .OR. 
     +		    TEXT(LOC:LOC1) .EQ. 'ir') THEN
C								      DIRAC file
		    LOC = LOC + 2
		    IGNORE = GET_FILENAME(TEXT,LOC)
		END IF
	    ELSE IF (TEXT(LOC:LOC).EQ.'U' .OR. TEXT(LOC:LOC).EQ.'u') THEN
		LOC = LOC + 1
		LOC1 = LOC + 1
		IF (TEXT(LOC:LOC1) .EQ. 'FT' .OR. 
     +		    TEXT(LOC:LOC1) .EQ. 'ft') THEN
C								     UFTAPX file
		    LOC = LOC + 2
		    IGNORE = GET_FILENAME(TEXT,LOC)
		END IF
	    ELSE IF (TEXT(LOC:LOC) .EQ. '+') THEN
C							       continuation mark
		GO TO 100
	    END IF
	    LOC = LOC + 1
	END DO
C								normal return
  800	CONTINUE
	RETURN
	END
C*******************************************************************************
	CHARACTER*80 FUNCTION GET_FILENAME(TEXT,LOC)
C
C	This routine searches for a filename in the string TEXT starting at
C	the character position LOC.  LOC is updated to point to the first
C	non-delimiting character that follows the file name.
C
	CHARACTER*140 TEXT
	CHARACTER*80 FILENAME
C						space forward to the delimiter
	N = LOC
	DO WHILE (N .LE. 140)
	    IF (TEXT(N:N) .EQ. ' ' .OR. TEXT(N:N) .EQ. ',' .OR.
     +					TEXT(N:N) .EQ. '=') THEN
		N = N + 1
		GO TO 100
	    ELSE
		N = N + 1
	    END IF
	END DO
C						get rid of extra delimiters
  100	CONTINUE
	DO WHILE (N.LE.140 .AND. (TEXT(N:N).EQ.' ' .OR. TEXT(N:N).EQ.'='
     + 				  .OR. TEXT(N:N) .EQ. ','))
	    N = N + 1
	END DO
C						capture the filename
	I = 0
	DO WHILE (N .LE. 140)
	    IF (TEXT(N:N).EQ.'"' .OR. TEXT(N:N).EQ.'''') THEN
		N = N + 1
	    ELSE IF (TEXT(N:N).EQ.' ' .OR. TEXT(N:N).EQ.',')THEN
		N = N + 1
		GO TO 300
	    ELSE IF (TEXT(N:N) .EQ. '+') THEN
		GO TO 400
	    ELSE
		I = I + 1
		FILENAME(I:I) = TEXT(N:N)
		N = N + 1
	    END IF
	END DO
C						get rid of trailing delimiters
  300	CONTINUE
	DO WHILE (N .LE. 140 .AND. 
     +			(TEXT(N:N) .EQ. ' ' .OR. TEXT(N:N) .EQ. ','))
	    N = N + 1
	END DO
  400	CONTINUE
	GET_FILENAME = FILENAME(1:I)
	LOC = N - 1
	RETURN
	END
C****************************************************************************
	SUBROUTINE REPORT_MODTRAN_PARMS(MODOUT,MODTAB)
C
	CHARACTER*80 MODOUT,MODTAB
C
	PARAMETER(LAYDIM=61,NSPECX=13,NZCLD=16,NAER=7,NWAVLN=47)
	COMMON /MODPARS/H2O,O3,CH4,SO2,TEMPADJ,MODEL,M1,M2,M3,M4,M5,M6,
     +		MDEF,TBOUND,CO2MIX,IHAZE,ISEASN,IVULCN,ICSTL,VIS,WSS,
     +		WHH,H1,H2,ML,IRD1,IRD2,TITLE,ZMDL,P,T,WMOL,XMOL,JCHAR,
     +		JCHARX,AHAZE,EQLWCZ,IHA1,IVUL1,ISEA1,ICHR1,IPARM,IPH,
     +		IDAY,PARM1,PARM2,PARM3,PARM4,TIME,G
	REAL ZMDL(LAYDIM),P(LAYDIM),T(LAYDIM),AHAZE(LAYDIM)
	REAL WMOL(12,LAYDIM),XMOL(NSPECX,LAYDIM),EQLWCZ(LAYDIM)
	INTEGER IHA1(LAYDIM),IVUL1(LAYDIM),ISEA1(LAYDIM),ICHR1(LAYDIM)
	CHARACTER*4 TITLE(16)
	CHARACTER*1 JCHAR(15,LAYDIM),JCHARX(LAYDIM)
C
	CHARACTER*132 BUF
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
C
 	IF (MODEL.EQ.7 .AND. H2.LT.ZMDL(1)) THEN
	    CALL XVMESSAGE('The lowest atmospheric layer must be',' ')
	    CALL XVMESSAGE(
     +		'at least as low as the ground elevation value',' ')
	    CALL ABEND
 	END IF
C
	CALL XVMESSAGE(' ',' ')
	CALL XVMESSAGE('       ***MODTRAN PARAMETERS***',' ')
	IF (M1.EQ.0) THEN
	    CALL XVMESSAGE(MODEL_NAME(MODEL),' ')
	ELSE
	    CALL XVMESSAGE(MODEL_NAME(M1),' ')
	END IF
	IF (MODEL.EQ.7) THEN
	    CALL XVMESSAGE('   ALTITUDE  PRESSURE    TEMP    WATER',' ')
	    DO I=1,ML
		WRITE (BUF,100) ZMDL(I),P(I),T(I),WMOL(1,I),
     +				(JCHAR(J,I),J=1,15),JCHARX(I)
  100		FORMAT(F10.3,3F10.2,5X,15A1,2X,A1)
		CALL XVMESSAGE(BUF,' ')
	    END DO
	ELSE
	    CALL XVMESSAGE(M2_NAME(M2),' ')
	    CALL XVMESSAGE(M3_NAME(M3),' ')
	END IF
C
	WRITE (BUF,110) H2O
  110	FORMAT(' Water Rescaling Factor =',F6.3)
	CALL XVMESSAGE(BUF,' ')
C
	WRITE (BUF,120) O3
  120	FORMAT(' Ozone Rescaling Factor =',F6.3)
	CALL XVMESSAGE(BUF,' ')
C
	WRITE (BUF,130) CH4
  130	FORMAT(' Methane Rescaling Factor =',F6.3)
	CALL XVMESSAGE(BUF,' ')
C
	WRITE (BUF,140) SO2
  140	FORMAT(' Sulfur Dioxide Rescaling Factor =',F6.3)
	CALL XVMESSAGE(BUF,' ')
C
	WRITE (BUF,150) CO2MIX
  150	FORMAT(' Carbon Dioxide Mixing Ratio =',F8.2,' ppm')
	CALL XVMESSAGE(BUF,' ')
C
	IF (TEMPADJ .NE. 0.0) THEN
	    WRITE (BUF,155) TEMPADJ
  155	    FORMAT(' Temperature Adjustment =',F6.2,' degrees Celsius')
	    CALL XVMESSAGE(BUF,' ')
	END IF
C
	IF (IHAZE.GT.0) THEN
	    CALL XVMESSAGE(IHAZE_NAME(IHAZE),' ')
	    IF (VIS .NE. 0.0) THEN
		WRITE (BUF,160) VIS
  160		FORMAT(' Visibility set to ',F7.2,' km.')
		CALL XVMESSAGE(BUF,' ')
	    END IF
	ELSE
	    CALL XVMESSAGE(' No boundary layer aerosol attenuation',' ')
	END IF
C
	WRITE (BUF,200) H1
  200	FORMAT(' Sensor Altitude =',F9.3,' km')
	CALL XVMESSAGE(BUF,' ')
C
	WRITE (BUF,210) H2
  210	FORMAT(' Target Altitude =',F9.3,' km')
	CALL XVMESSAGE(BUF,' ')
C
	IF (MODOUT .NE. 'dummy') THEN
	    WRITE (BUF,300) MODOUT
  300	    FORMAT(' MODTRAN report file is stored in ',A80)
	    CALL XVMESSAGE(BUF,' ')
	END IF
C
	IF (MODTAB .NE. 'dummy') THEN
	    WRITE (BUF,310) MODTAB
  310	    FORMAT(' MODTRAN tabular file is stored in ',A80)
	    CALL XVMESSAGE(BUF,' ')
	END IF
C
	CALL XVMESSAGE(' ',' ')
	RETURN
	END
C***********************************************************************
	SUBROUTINE MODTRAN_INTERFACE(IOUT,RESP,MODOUT,MODTAB,TRANX,
     +				     PATHX)
C
	REAL RESP(800,10),TRANX(716,10),PATHX(716,10)
	CHARACTER*80 MODOUT,MODTAB
C
	PARAMETER(LAYDIM=61,NSPECX=13,NZCLD=16,NAER=7,NWAVLN=47)
	COMMON /MODPARS/H2O,O3,CH4,SO2,TEMPADJ,MODEL,M1,M2,M3,M4,M5,M6,
     +		MDEF,TBOUND,CO2MIX,IHAZE,ISEASN,IVULCN,ICSTL,VIS,WSS,
     +		WHH,H1,H2,ML,IRD1,IRD2,TITLE,ZMDL,P,T,WMOL,XMOL,JCHAR,
     +		JCHARX,AHAZE,EQLWCZ,IHA1,IVUL1,ISEA1,ICHR1,IPARM,IPH,
     +		IDAY,PARM1,PARM2,PARM3,PARM4,TIME,G
	REAL ZMDL(LAYDIM),P(LAYDIM),T(LAYDIM),AHAZE(LAYDIM)
	REAL WMOL(12,LAYDIM),XMOL(NSPECX,LAYDIM),EQLWCZ(LAYDIM)
	INTEGER IHA1(LAYDIM),IVUL1(LAYDIM),ISEA1(LAYDIM),ICHR1(LAYDIM)
	CHARACTER*4 TITLE(16)
	CHARACTER*1 JCHAR(15,LAYDIM),JCHARX(LAYDIM)
C
	REAL WAVELEN(50000),TRANSMIT(50000),PATHRAD(50000),SKYRAD(50000)
	REAL SPECALB(50000),RRATZ(LAYDIM),AWCCON(4),WAVLEN(47),VX(47)
	REAL EXTC(7,47),ABSC(7,47),ASYM(7,47)
	REAL ZCLD(16),CLD(16),CLDICE(16),RR(16)
	REAL ANGF(50),F(4,50),FRAC(358),TRANS(10,2),PATH(10,2),SKY(10,2)
	INTEGER ICLD1(LAYDIM),IREG(4)
	LOGICAL LDISORT,LSUN1
	CHARACTER*80 MODLIB,PLTABFILE,SUNFILE,DIRACFILE,UFTAPXFILE,MSG
	CHARACTER*8 DLIMIT
	CHARACTER*4 TITLE2D(18,4)
	CHARACTER*1 CODE,YFLAG,XFLAG
C						  set up interpolation fractions
C						  for each sample location
	DEFL = 42.96
	DELTA = DEFL/358.0
	COSFULL = COS(DEFL*PI/180)
	DO I=1,358
	    COSI = COS(DELTA*FLOAT(I)*PI/180)
	    FRAC(I) = (COSI-COSFULL) / (COSI*(1.0-COSFULL))
	END DO
C						       declare modtran constants
	PLTABFILE = 'null'
	CALL XGETENV_VIC('VICARMODTRAN35',MODLIB)
	SUNFILE = MODLIB(1:LNBLNK(MODLIB)) // '/sun3'
	DIRACFILE = MODLIB(1:LNBLNK(MODLIB)) // '/MOLBMP96.BIN'
	UFTAPXFILE = MODLIB(1:LNBLNK(MODLIB)) // '/CFCBMP96.ASC'
	O2 = 1.0
	CODE = 'T'
	ITYPE = 2
	IEMSCT = 1
	IMULT = 1
	IF (MODEL .EQ. 7) THEN
	    IM = 1
	ELSE
	    IM = 0
	END IF
	NOPRNT = 0
	DO I=1,50000
	    SPECALB(I) = 0.05
	END DO
	LDISORT = .FALSE.
	ISTRM = 0
	LSUN1 = .TRUE.
	ISUN = 5
	ICLD = 0
	IVSA = 0
	RAINRT = 0.0
	GNDALT = H2
	RANGE = 0.0
	BETA = 0.0
	RO = 0.0
	LEN = 0
	PHI = 0.0
	IV1 = 601
	IV2 = 1500
	IDV = 1
	IRES = 2
	YFLAG = ' '
	XFLAG = ' '
	IF (IRD2 .NE. 0) THEN
	    DO I=1,ML
		RRATZ(I) = 0.0
		ICLD1(I) = 0
	    END DO
	END IF
	ANGLE = 180.0
C						       run MODTRAN at two angles
	DO IANGLE=1,2
	    CALL MODTRAN3(MODOUT,MODTAB,PLTABFILE,SUNFILE,DIRACFILE,
     +		UFTAPXFILE,H2O,O3,O2,CH4,SO2,TEMPADJ,CODE,MODEL,ITYPE,
     +		IEMSCT,IMULT,M1,M2,M3,M4,M5,M6,MDEF,IM,NOPRNT,
     +		TBOUND,SPECALB,LDISORT,ISTRM,LSUN1,ISUN,CO2MIX,
     +		IHAZE,ISEASN,IVULCN,ICSTL,ICLD,IVSA,VIS,WSS,WHH,
     +		RAINRT,GNDALT,H1,H2,ANGLE,RANGE,BETA,RO,LEN,PHI,
     +		IV1,IV2,IDV,IRES,YFLAG,XFLAG,DLIMIT,CTHIK,CALT,
     +		CEXT,ISEED,NCRALT,NCRSPC,CWAVLN,CCOLWD,CCOLIP,
     +		CHUMID,ASYMWD,ASYMIP,ZCVSA,ZTVSA,ZINVSA,ML,IRD1,
     +		IRD2,TITLE,ZMDL,P,T,WMOL,XMOL,JCHAR,JCHARX,
     +		AHAZE,EQLWCZ,RRATZ,IHA1,ICLD1,IVUL1,ISEA1,ICHR1,
     +		IREG,AWCCON,TITLE2D,WAVLEN,VX,EXTC,ABSC,ASYM,
     +		ZCLD,CLD,CLDICE,RR,IPARM,IPH,IDAY,ISOURC,PARM1,
     +		PARM2,PARM3,PARM4,TIME,PSIPO,ANGLEM,G,NANGLS,
     +		ANGF,F,WAVELEN,TRANSMIT,PATHRAD,SKYRAD)
C
	    CALL WVNUM2WVLEN(TRANSMIT,PATHRAD,SKYRAD)
C
C					Compute the transmittance, path radiance
C					and sky radiance for each channel by
C					multiplying the per wavenumber values by
C					the 10 normalized response functions.
	    DO ICHAN=1,10
		TRANS(ICHAN,IANGLE) = 0.0
		PATH(ICHAN,IANGLE) = 0.0
		SKY(ICHAN,IANGLE) = 0.0
		DO IWAVE=1,800
		    TRANS(ICHAN,IANGLE) = TRANS(ICHAN,IANGLE) +
     +			 	       RESP(IWAVE,ICHAN)*TRANSMIT(IWAVE)
		    PATH(ICHAN,IANGLE) = PATH(ICHAN,IANGLE) +
     +					RESP(IWAVE,ICHAN)*PATHRAD(IWAVE)
		    SKY(ICHAN,IANGLE) = SKY(ICHAN,IANGLE) +
     +					RESP(IWAVE,ICHAN)*SKYRAD(IWAVE)
		END DO
C					      Change units from W/cm^2 to W/m^2
C
		PATH(ICHAN,IANGLE) = 10000.0 * PATH(ICHAN,IANGLE)
C
C					      Change units from W/cm^2 to mW/m^2
C
		SKY(ICHAN,IANGLE) = 1.0E7 * SKY(ICHAN,IANGLE)
C
C						      to prevent division by 0.0
C
		IF (TRANS(ICHAN,IANGLE).LE.0.0) TRANS(ICHAN,IANGLE)=1.0
	    END DO
C					    update params for second modtran run
	    MODOUT = 'dummy'
	    MODTAB = 'dummy'
	    ANGLE = 180.0 - DEFL
	END DO
C					  put sky radiance values in VICAR label
	CALL XLADD(IOUT,'HISTORY','SKY41',SKY(1,1),ISTAT,
     +		   'FORMAT','REAL',' ')
	CALL XLADD(IOUT,'HISTORY','SKY42',SKY(2,1),ISTAT,
     +		   'FORMAT','REAL',' ')
	CALL XLADD(IOUT,'HISTORY','SKY43',SKY(3,1),ISTAT,
     +		   'FORMAT','REAL',' ')
	CALL XLADD(IOUT,'HISTORY','SKY44',SKY(4,1),ISTAT,
     +		   'FORMAT','REAL',' ')
	CALL XLADD(IOUT,'HISTORY','SKY45',SKY(5,1),ISTAT,
     +		   'FORMAT','REAL',' ')
	CALL XLADD(IOUT,'HISTORY','SKY46',SKY(6,1),ISTAT,
     +		   'FORMAT','REAL',' ')
	CALL XLADD(IOUT,'HISTORY','SKY47',SKY(7,1),ISTAT,
     +		   'FORMAT','REAL',' ')
	CALL XLADD(IOUT,'HISTORY','SKY48',SKY(8,1),ISTAT,
     +		   'FORMAT','REAL',' ')
	CALL XLADD(IOUT,'HISTORY','SKY49',SKY(9,1),ISTAT,
     +		   'FORMAT','REAL',' ')
	CALL XLADD(IOUT,'HISTORY','SKY50',SKY(10,1),ISTAT,
     +		   'FORMAT','REAL',' ')
C					     interpolate transmittance and path
C					     radiance to a value for each sample
	DO ICHAN=1,10
	    DO I=1,358
		TRANX(358+I,ICHAN) = FRAC(I)*TRANS(ICHAN,1) +
     +				     (1.0-FRAC(I))*TRANS(ICHAN,2)
		PATHX(358+I,ICHAN) = FRAC(I)*PATH(ICHAN,1) +
     +				     (1.0-FRAC(I))*PATH(ICHAN,2)
		TRANX(359-I,ICHAN) = TRANX(358+I,ICHAN)
		PATHX(359-I,ICHAN) = PATHX(358+I,ICHAN)
	    END DO
	END DO
C							      report the relults
	CALL XVMESSAGE(' ',' ')
	CALL XVMESSAGE(
     +'          Transmittance      Path  Radiance      Sky Radiance',
     +' ')
	CALL XVMESSAGE(
     +'                             (W/m**2/sr/u)        (mW/m**2/u)',
     +' ')
	CALL XVMESSAGE(
     +' Band   Nadir   Off-nadir   Nadir   Off-nadir',' ')
	DO ICHAN=1,10
	    WRITE (MSG,500) ICHAN+40,TRANS(ICHAN,1),TRANS(ICHAN,2),
     +			    PATH(ICHAN,1),PATH(ICHAN,2),SKY(ICHAN,1)
  500	    FORMAT(I4,F10.5,F9.5,F11.4,F9.4,F15.1)
	    CALL XVMESSAGE(MSG,' ')
	END DO
C
	RETURN
	END
C****************************************************************************
	SUBROUTINE WVNUM2WVLEN(TRANSMIT,PATHRAD,SKYRAD)
C
C	This routine takes the arrays returned from MODTRAN (900 elements,
C	from 601 to 1500 wavenumbers), and returns arrays that are indexed
C	in wavelength intervals (800 elements, from 7.00 to 14.99 micrometers).
C
	REAL TRANSMIT(900),PATHRAD(900),SKYRAD(900)
	REAL TR(1500),PATH(1500),SKY(1500)
C
	DO I=1,900
	    TR(I+600) = TRANSMIT(I)
	    PATH(I+600) = PATHRAD(I)
	    SKY(I+600) = SKYRAD(I)
	END DO
C
	DO I=1,800
	    WAVELENGTH = FLOAT(I+699) / 100.0
	    WAVENUM = 10000.0 / WAVELENGTH
	    INDEX = INT(WAVENUM)
	    FRAC = WAVENUM - FLOAT(INDEX)
	    TRANSMIT(I) = (1.0-FRAC)*TR(INDEX) + FRAC*TR(INDEX+1)
	    PATHRAD(I) = (1.0-FRAC)*PATH(INDEX) + FRAC*PATH(INDEX+1)
	    SKYRAD(I) = (1.0-FRAC)*SKY(INDEX) + FRAC*SKY(INDEX+1)
	END DO
C
	RETURN
	END
C****************************************************************************
	SUBROUTINE GRADCAL(IN,IOUT,ISL,NL,TRANX,PATHX)
C
	REAL TRANX(716,10),PATHX(716,10),BUF(716),OUTBUF(716)
C
	DO ICHAN=1,10
	    DO ILINE=ISL,ISL+NL-1
		CALL XVREAD(IN,BUF,ISTAT,'LINE',ILINE,'BAND',ICHAN,' ')
		DO J=1,716
		    OUTBUF(J) = (BUF(J)-PATHX(J,ICHAN)) / TRANX(J,ICHAN)
		END DO
		CALL XVWRIT(IOUT,OUTBUF,ISTAT,' ')
	    END DO
	END DO
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create mastertir.pdf
Process help=*
parm  INP	(string,80)			count=2
parm  OUT	(string,80)
parm  SIZE	integer	default=(1,1,0,0)	count=4
parm  SL	integer	default=1
parm  SS	integer	default=1
parm  NL	integer	default=0
parm  NS	integer	default=0
parm  MODINP	(string,40)
parm  MODOUT	(string,40)	count=(0:1)	default=--
parm  MODTAB	(string,40)	count=(0:1)	default=--
End-proc

.TITLE
TAE PROCESS MASTERTIR
.HELP
PURPOSE:

   MASTERTIR is a program that accepts as input MASTER TIR radiance at sensor 
image data and outputs upwelling radiance at surface for the thermal infrared
channels (Channels 41-50).  Also required as input are a valid MODTRAN input
file, to define the atmospheric model, and a MASTER TIR spectral response
file.


OPERATION:

   MASTERTIR takes as input the 10 Channels of MASTER TIR radiance at sensor
data in units of Watts per square meter per steradian per micrometer, and a
file containing the the MASTER TIR spectral response functions. This second
file must be a VICAR labelled file consisting of ten lines (one for each TIR
channel) and 800 samples (one for the spectral response at each wavelength
from 7.00 to 14.99 micrometers, in 0.01 micrometer increments). The spectral 
response functions must be normalized, i.e., the sum of all values on each 
line must be 1.0.
   The atmospheric model used is defined by a user supplied MODTRAN input
file.  MASTERTIR uses these parameters to run MODTRAN twice; once with a
nadir looking geometry, and once for the case of viewing at maximum deflection
from nadir.  From these two runs, the atmospheric transmittance, path
radiance, and downwelling sky radiance at surface are calculated for the ten
MASTER TIR bandpasses.  The downwelling sky radiance values are placed in
the VICAR label of the output dataset.
   Values of transmittance and path radiance are calculated for each pixel
location in the scene by interpolation, where it is assumed that each
quantity varies linearly with path length.
   The upwelling radiance at surface is then computed by the following 
formula:

        Rad(at sensor) = RAD(upwelling)*transmittance + Rad(path)

It should be noted that the output quantity (upwelling radiance) of MASTERTIR
differs from the output quantity of TIMSCAL2 (surface emitted radiance). It
it includes both the surface emitted component plus the surface reflected
component.
   The output dataset consists of 10 channels of real (32 bit floating
point) pixels, in units of Watts per square meter per steradian per
micrometer.  These units are the same as the input units, but different than
the units produced by this program prior to November, 2004.  The downwelling
sky irradiance values that are placed in the VICAR label are in units of 
milliWatts per square meter per micrometer.  To convert to radiance in units 
consistent with the output images, one must divide by 1000.0*pi.


WRITTEN BY:  Ron Alley         August 12, 1998

COGNIZANT PROGRAMMER:  Ron Alley

REVISION: 1.4 Nov 12, 2004

.LEVEL1
.VARI INP
(1) MASTER TIR radiance at
    sensor image
(2) MASTER TIR spectral
    response file
.VARI OUT
Output data set of the 10 MASTER
TIR bands, for upwelling
radiance at surface.
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
.VARI MODINP
Dataset name for the MODTRAN
input file, containing the
atmospheric data relevant to
this image.
.VARI MODOUT
Dataset name for MODTRAN
output report file.
.VARI MODTAB
Dataset name for MODTRAN
output tabular file.
.LEVEL2
.VARI INP
The first file should contain the 10 channels of MASTER TIR image data,
calibrated as radiance at sensor, in units of Watts per square meter per
steradian per micrometer.

The second file should be the MASTER TIR spectra response calibration file 
(in VICAR format) that is in effect for the date of data acquisition.
These files are typically located in the directory 

          $V2TOP/luts

and have names of the form

         resp.master.tir.YEAR.MON

.VARI OUT
The output dataset will contain upwelling radiance at surface data for the
ten MASTER TIR channels (Channels 41 to 50), in units of Watts per
square meter per steradian per micrometer.  Output pixels are real values
(32 bit floating point).  The units of the downwelling sky irradiance (values
placed in the VICAR label of the output) are milliWatts per square meter per
micrometer (no steradian term).
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
.VARI MODINP
   The atmospheric and geometric parameters needed by this program to run
   MODTRAN are supplied by the user providing a valid MODTRAN input (TAPE5)
   file.  This file may be generated by the stand alone program "modin",
   or by any other means convenient to the user.
.VARI MODOUT
   As a default, the report file generated by MODTRAN is discarded 
   at the conclusion of the MODTRAN run.  If a dataset name is given
   as the parameter value to MODOUT, the report for the nadir looking case
   of MODTRAN is saved in that dataset.  If a dataset name is not given
   as a parameter, but one is supplied in the MODTRAN input file, then the
   report file is saved under that name.
.VARI MODTAB
   As a default, the tabular file generated by MODTRAN is discarded 
   at the conclusion of the MODTRAN run.  If a dataset name is given
   as the parameter value to MODTAB, the report for the nadir looking case
   of MODTRAN is saved in that dataset.  If a dataset name is not given
   as a parameter, but one is supplied in the MODTRAN input file, then the
   tabular file is saved under that name.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create mastertir.imake
#define  PROGRAM   mastertir

#define MODULE_LIST mastertir.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
#define LIB_MOD35
$ Return
$!#############################################################################
