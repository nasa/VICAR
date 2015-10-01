$!****************************************************************************
$!
$! Build proc for MIPL module garslog
$! VPACK Version 1.8, Friday, May 28, 1999, 15:58:47
$!
$! Execute by entering:		$ @garslog
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
$ write sys$output "*** module garslog ***"
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
$ write sys$output "Invalid argument given to garslog.com file -- ", primary
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
$   if F$SEARCH("garslog.imake") .nes. ""
$   then
$      vimake garslog
$      purge garslog.bld
$   else
$      if F$SEARCH("garslog.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake garslog
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @garslog.bld "STD"
$   else
$      @garslog.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create garslog.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack garslog.com -
	-s garslog.f -
	-i garslog.imake -
	-p garslog.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create garslog.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
	INTEGER IBUF(1926)
	LOGICAL*1 BUF(7704)
	EQUIVALENCE (BUF,IBUF)
	CHARACTER*4 IBUFTST
C								open input
	CALL XVUNIT(INUNIT,'INP',1,ISTAT,' ')
	CALL XVOPEN(INUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
	CALL XVGET(INUNIT,ISTAT,'NL',NLIN,'NS',NSIN,' ')
C
	DO I=1,NLIN
	    CALL XVREAD(INUNIT,BUF,ISTAT,' ')
	    CALL CEBCDC(BUF,4)
	    CALL MVLC(IBUF(1), IBUFTST, 4)
	    IF (IBUFTST .EQ. 'DIR ') THEN
		CALL READ_DIR(IBUF(7),NL,NS,NB,NBPP,ISKIP,IOUTUNIT)
	    ELSE IF (IBUFTST .EQ. 'WORD') THEN
		CALL READ_DATA(IBUF(7),IBUF(7),IBUF(7),NL,NS,NB,NBPP,
     +			       ISKIP,IOUTUNIT)
	    END IF
	END DO
	RETURN
	END
C***********************************************************************
	SUBROUTINE READ_DIR(IBUF,NL,NS,NB,NBPP,ISKIP,IOUTUNIT)
C
	INTEGER IBUF(64),NCHAN(32)
	INTEGER ID(96)/  1, 3, 2, 3, 4, 5, 6, 7, 8, 3, 
     +			 3, 3, 9,10,11,12,13,14,15,16,
     +			17,18,19,20,21,22,23,24,25,26,
     +			27,28,29,30, 3, 3,31,31,31,31,
     +			31,32,33,34,35,36,37,37,37,37,
     +			37,38,38,38,38,38, 3, 3, 3, 3,
     +			 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
     +			 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
     +			39, 3, 3, 3, 3, 3, 3, 3, 3, 3,
     +			 3, 3, 3, 3, 3,40/
	CHARACTER*18 SENSOR(40)/
     +	'Non-Image Derived ','Graphics          ','Miscellaneous     ',
     +	'Meteosat Visible  ','Meteosat Infrared ','Meteosat Water Vap',
     +  'Radar             ','Misc Aircraft-MAMS','GMS Visible       ',
     +  'GMS Infrared      ','ATS 6 Visible     ','ATS 6 Infrared    ',
     +  'SMS-1 Visible     ','SMS-1 Infrared    ','SMS-2 Visible     ',
     +  'SMS-2 Infrared    ','GOES-1 Visible    ','GOES-1 Infrared   ',
     +  'GOES-2 Visible    ','GOES-2 Infrared   ','GOES-3 Visible    ',
     +  'GOES-3 Infrared   ','GOES-4 Visible VAS','GOES-4 IR & Wv VAS',
     +  'GOES-5 Visible    ','GOES-5 IR & Wv VAS','GOES-6 Visible    ',
     +  'GOES-6 Infrared   ','GOES-7 Visible    ','GOES-7 Infrared   ',
     +  'NOAA Series Sat   ','TIROS-N           ','NOAA-6            ',
     +  'NOAA-7            ','NOAA-8            ','NIMBUS Satellites ',
     +	'Mariner X Sats    ','Digitized Radar   ','ERBE              ',
     +  'NOAA-9            '/
	CHARACTER*80 PRT
	CHARACTER*21 DATESTR
	CHARACTER*4 FORMAT(4)/'BYTE','HALF','XXXX','FULL'/
	CHARACTER*3 ORG
C						decode data from IBM format
	IDNUM = IBUF(3)
	NL    = IBUF(9)
	NS    = IBUF(10)
	NBPP  = IBUF(11)
	LINC  = IBUF(12)
	ISINC = IBUF(13)
	NB    = IBUF(14)
	ISKIP = IBUF(15)
	ICHAN = IBUF(19)
	JDATE = IBUF(46)
	JTIME = IBUF(47)
	CALL CEBCDC(IBUF(52),8)
C							     open output dataset
	IF (NB.EQ.1) THEN
	    ORG = 'BSQ'
	ELSE
	    ORG = 'BIP'
	END IF
	CALL XVUNIT(IOUTUNIT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUTUNIT,ISTAT,'U_NL',NL,'U_NS',NS,'U_NB',NB,
     +		'U_ORG',ORG,'OP','WRITE','OPEN_ACT','SA','IO_ACT','SA',
     +		'O_FORMAT',FORMAT(NBPP),'U_FORMAT',FORMAT(NBPP),' ')
C							 put sensor name, date, 
C							 and time in VICAR label
	CALL XLADD(IOUTUNIT,'HISTORY','SENSOR',SENSOR(ID(IDNUM)+1),
     +		   ISTAT,'FORMAT','STRING',' ')
	CALL XVMESSAGE(SENSOR(ID(IDNUM)+1),' ')
	CALL DATIME(JDATE,JTIME,DATESTR)
	CALL XLADD(IOUTUNIT,'HISTORY','DATE_TIME',DATESTR,ISTAT,
     +		   'FORMAT','STRING','ULEN',21,' ')
	CALL XVMESSAGE(DATESTR,' ')
C						 compute and report band numbers
	N = 0
	DO I=1,32
	    IF (BTEST(ICHAN,I-1)) THEN
		N = N + 1
		NCHAN(N) = I
	    END IF
	END DO
	WRITE (PRT,100) (NCHAN(I),I=1,N)
  100	FORMAT(32I3)
	CALL XLADD(IOUTUNIT,'HISTORY','BAND(S)',PRT,ISTAT,
     +		   'FORMAT','STRING','ULEN',3*N,' ')
	CALL XVMESSAGE(PRT,' ')
C					put source and calibration type in label
	WRITE (PRT,200) IBUF(52),IBUF(53)
  200	FORMAT(A4,'/',A4)
	CALL XLADD(IOUTUNIT,'HISTORY','SRCE/CAL',PRT,ISTAT,
     +		   'FORMAT','STRING','ULEN',9,' ')
	CALL XVMESSAGE(PRT,' ')
C					   if subsampling done, note it in label
	IF (LINC.NE.1 .OR. ISINC.NE.1) THEN
	    WRITE (PRT,300) LINC,ISINC
  300	    FORMAT(' EVERY',I3,' LINES,',I3,' SAMPLES')
	    CALL XLADD(IOUTUNIT,'HISTORY','SAMPLED',PRT,ISTAT,
     +		   'FORMAT','STRING','ULEN',27,' ')
	    CALL XVMESSAGE(PRT,' ')
	END IF
C	
	RETURN
	END
C***********************************************************************
	SUBROUTINE READ_DATA(BUF,HBUF,IBUF,NL,NS,NB,NBPP,ISKIP,IOUTUNIT)
C
	INTEGER IBUF(*)
	INTEGER*2 HBUF(*),HOLD2(3840)
	INTEGER NLAST/0/,NLWROTE/0/
	LOGICAL*1 BUF(*),HOLD(7680)
	EQUIVALENCE (HOLD,HOLD2)
	SAVE
C
	LOC = 0
	IF (NB .EQ. 1) THEN				! BSQ output
	    IF (NBPP .EQ. 1) THEN			!	byte data
		IF (NLAST .NE. 0) THEN
		    CALL MVE(1,ISKIP+NS-NLAST,BUF,HOLD(NLAST+1),1,1)
		    LOC = ISKIP+NS-NLAST
		    IF (NLWROTE.LT.NL) THEN
			CALL XVWRIT(IOUTUNIT,HOLD(ISKIP+1),ISTAT,' ')
			NLWROTE = NLWROTE + 1
		    END IF
		END IF
		DO WHILE (LOC+NS+ISKIP .LE. 7680)
		    LOC = LOC + ISKIP
		    IF (NLWROTE.LT.NL) THEN
			CALL XVWRIT(IOUTUNIT,BUF(LOC+1),ISTAT,' ')
			NLWROTE = NLWROTE +1
		    END IF
		    LOC = LOC + NS
		END DO
		NLAST = 7680 - LOC
		IF (NLAST .NE. 0) CALL MVE(1,NLAST,BUF(LOC+1),HOLD,1,1)
	    ELSE IF (NBPP .EQ. 2) THEN			!	halfword data
		ISKIPH = ISKIP/2
		IF (NLAST .NE. 0) THEN
		    CALL MVE(2,ISKIPH+NS-NLAST,HBUF,HOLD2(NLAST+1),1,1)
		    LOC = ISKIPH+NS-NLAST
		    IF (NLWROTE.LT.NL) THEN
			CALL XVWRIT(IOUTUNIT,HOLD2(ISKIPH+1),ISTAT,' ')
			NLWROTE = NLWROTE + 1
		    END IF
		END IF
		DO WHILE (LOC+NS+ISKIPH .LE. 3840)
		    LOC = LOC + ISKIPH
		    IF (NLWROTE.LT.NL) THEN
			CALL XVWRIT(IOUTUNIT,HBUF(LOC+1),ISTAT,' ')
			NLWROTE = NLWROTE +1
		    END IF
		    LOC = LOC + NS
		END DO
		NLAST = 3840 - LOC
		IF (NLAST .NE. 0) CALL MVE(2,NLAST,HBUF(LOC+1),HOLD2,1,
     +					   1)
	    ELSE					!	integer data
		CALL XVMESSAGE(' 1 BAND, INTEGER INPUT NOT IMPLEMENTED',' ')
		CALL ABEND
	    END IF
	ELSE						! BIP output
	    IF (NBPP .EQ. 1) THEN			!	byte data
		CALL XVMESSAGE(' MULTICHANNEL, BYTE INPUT NOT IMPLEMENTED',' ')
		CALL ABEND
	    ELSE IF (NBPP .EQ. 2) THEN			!	halfword data
		CALL XVMESSAGE(' MULTICHAN, HALFWD INPUT NOT IMPLEMENTED',' ')
		CALL ABEND
	    ELSE					!	integer data
		CALL XVMESSAGE(' MULTICHAN, INTEGER INPUT NOT IMPLEMENTED',' ')
		CALL ABEND
	    END IF
	END IF
	RETURN
	END
C***********************************************************************
	SUBROUTINE DATIME(JDATE,JTIME,DATESTR)
C
	CHARACTER*21 DATESTR
	CHARACTER*3 MON(12)/'JAN','FEB','MAR','APR','MAY','JUN','JUL',
     +			    'AUG','SEP','OCT','NOV','DEC'/
	INTEGER IMON(12)/0,31,60,91,121,152,182,213,244,274,305,335/
C
	IYR = JDATE/1000
	IDAY = JDATE - 1000*IYR
	IF (MOD(IYR,4).NE.0 .AND. IDAY.GT.59) IDAY=IDAY+1   ! handle leap year
	DO I=1,12
	    IF (IMON(I) .GE. IDAY) GO TO 100
	END DO
  100	CONTINUE
	I = I-1
	IDAY = IDAY - IMON(I)
	IHR = JTIME/10000
	MIN = MOD(JTIME/100,100)
	ISEC = MOD(JTIME,100)
C							    build date string
	IF (IYR .GT. 99) THEN
	    WRITE (DATESTR,150) MON(I),IDAY,IYR,IHR,MIN,ISEC
  150	    FORMAT(A3,I3,',',I5,I3,':',I2.2,':',I2.2)
	ELSE IF (IYR .GT. 60) THEN
	    WRITE (DATESTR,200) MON(I),IDAY,IYR,IHR,MIN,ISEC
  200	    FORMAT(A3,I3,', 19',I2,I3,':',I2.2,':',I2.2)
	ELSE
	    WRITE (DATESTR,300) MON(I),IDAY,IYR,IHR,MIN,ISEC
  300	    FORMAT(A3,I3,', 20',I2,I3,':',I2.2,':',I2.2)
	END IF
C
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create garslog.imake
#define  PROGRAM   garslog

#define MODULE_LIST garslog.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
$PDF_File:
$ create garslog.pdf
process help=*
PARM INP TYPE=(STRING,40)
PARM OUT TYPE=(STRING,40)
END-PROC
.TITLE
GARSLOG
.HELP
     GARSLOG is a program to log into VICAR format satellite image data that
is currently in the "GARS" format. GARS format is used by the University of
Wisconsin for tape output.  A number of sensor sources are supported, most
commonly GOES, NOAA, and TIROS series satellites. 
     The input dataset should alreadly be VICAR labelled (by CONVIM or some
similar means). There are no parameters other than input and output datasets.
.LEVEL1
.VARIABLE INP
input dataset
(VICAR labelled)
.VARIABLE OUT
output dataset
.LEVEL2
.END
$ Return
$!#############################################################################
