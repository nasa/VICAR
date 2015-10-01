$!****************************************************************************
$!
$! Build proc for MIPL module avhrrlog
$! VPACK Version 1.5, Monday, August 23, 1993, 16:51:35
$!
$! Execute by entering:		$ @avhrrlog
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
$ write sys$output "*** module avhrrlog ***"
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
$   if F$SEARCH("avhrrlog.imake") .nes. ""
$   then
$      vimake avhrrlog
$      purge avhrrlog.bld
$   else
$      if F$SEARCH("avhrrlog.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake avhrrlog
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @avhrrlog.bld "STD"
$   else
$      @avhrrlog.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create avhrrlog.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack avhrrlog.com -
	-s avhrrlog.f -
	-p avhrrlog.pdf -
	-i avhrrlog.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create avhrrlog.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C     
C     AVHRRLOG - ADVANCED VERY HIGH RESOLUTION RADIOMETER
C     
C     PURPOSE:  LOG AVHRR IMAGES INTO VICAR 2 FORMAT
C
C
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C
      REAL*8 SLOPE,INTER
      INTEGER*4 INBUF(3700),IOUT(5)
      INTEGER*2 OBUF(10242),OBUF2(2048)
      BYTE INBYTE(14800)
      CHARACTER*80 MSG
      CHARACTER*1 XL1,XL2,XL3,XL4
      EQUIVALENCE (INBYTE,INBUF)
c								open datasets
      CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
      IF (NSIN .NE. 7400) THEN
	  CALL XVMESSAGE(' Input file is not a LAC/HRPT dataset',' ')
	  CALL ABEND
      ENDIF
      NL = (NLIN/2) - 1
      NS = 2048
      CALL XVUNIT(INP,'INP',1,ISTAT,' ')
      CALL XVOPEN(INP,STAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
      DO I=1,5
	CALL XVUNIT(IOUT(I),'OUT',I,ISTAT,' ')
	CALL XVOPEN(IOUT(I),ISTAT,'U_NL',NL,'U_NS',NS,'U_NB',1,
     +		  'U_ORG','BSQ','U_FORMAT','HALF','O_FORMAT','HALF',
     +		  'OPEN_ACT','SA','IO_ACT','SA','OP','WRITE',' ')
      END DO
C
C        DECODE AVHRR AND WRITE VICAR IMAGES AND CALIBRATION COEFFS
C
      CALL XVREAD(INP,INBYTE,ISTAT,' ')
C						decode and store lats and longs
      WRITE(MSG,10) (INBYTE(J),J=76,89)
   10 FORMAT(14A1)
      READ (MSG,20) LAT1,LAT2,LONG1,LONG2
   20 FORMAT(I3,I3,I4,I4)
      IF (LAT1 .GE. 0) THEN
	XL1 = 'N'
      ELSE
	XL1 = 'S'
	LAT1 = -LAT1
      END IF
      IF (LAT2 .GE. 0) THEN
	XL2 = 'N'
      ELSE
	XL2 = 'S'
	LAT2 = -LAT2
      END IF
      IF (LONG1 .GE. 0) THEN
	XL3 = 'E'
      ELSE
        XL3 = 'W'
	LONG1 = -LONG1
      END IF
      IF (LONG2 .GE. 0) THEN
	XL4 = 'E'
      ELSE
        XL4 = 'W'
	LONG2 = -LONG2
      END IF
      WRITE (MSG,30) LAT1,XL1,LAT2,XL2,LONG1,XL3,LONG2,XL4
   30 FORMAT('Latitude Range: ',I2,A1,'-',I2,A1,'   Longitude Range: ',
     +       I3,A1,'-',I3,A1)
      CALL XVMESSAGE(MSG,' ')
      DO I=1,5
	CALL XLADD(IOUT(I),'HISTORY','BAND',I,ISTAT,'FORMAT','INT',' ')
      	CALL XLADD(IOUT(I),'HISTORY','LOCATION',MSG,ISTAT,
     +		   'FORMAT','STRING',' ')
      END DO
C								read past header
      CALL XVREAD(INP,INBYTE,ISTAT,' ')
      CALL XVREAD(INP,INBYTE,ISTAT,' ')
C
      DO L = 1,NL
C								read input data
        CALL XVREAD(INP,INBYTE,ISTAT,' ')
        CALL XVREAD(INP,INBYTE(7401),ISTAT,' ')
C							print calibration data
        IF (L.EQ.1) THEN
          CALL XVMESSAGE(
     +		   ' FIRST LINE - SLOPE AND INTERCEPT CALIBRATION:',' ')
	  DO I=1,5
	    M = 2*(I+1)
            SLOPE = DBLE(INBUF(M))/1073741824.D0
            INTER = DBLE(INBUF(M+1))/4194304.D0
	    WRITE (MSG,100) I,SLOPE,INTER
  100	    FORMAT(' Channel',I2,'  Slope =',F15.10,'  Intercept =',
     +		   F15.10)
	    CALL XVMESSAGE(MSG,' ')
	    X = SLOPE
	    CALL XLADD(IOUT(I),'HISTORY','CAL-SLOPE',X,ISTAT,
     +		       'FORMAT','REAL',' ')
	    X = INTER
	    CALL XLADD(IOUT(I),'HISTORY','CAL-INTER',X,ISTAT,
     +		       'FORMAT','REAL',' ')
          END DO
	END IF
C
	IF (L.EQ.NL) THEN
          CALL XVMESSAGE(
     +		   ' LAST LINE  - SLOPE AND INTERCEPT CALIBRATION:',' ')
	  DO I=1,5
	    M = 2*(I+1)
            SLOPE = DBLE(INBUF(M))/1073741824.D0
            INTER = DBLE(INBUF(M+1))/4194304.D0
	    WRITE (MSG,100) I,SLOPE,INTER
	    CALL XVMESSAGE(MSG,' ')
          END DO
        END IF
C								unpack data
        CALL UNPK10(INBUF(113),OBUF)
C							write out each band
	DO I=1,5
	  IOFF = I-5
	  DO LOC=1,2048
	    OBUF2(LOC) = OBUF(5*LOC+IOFF)
	  END DO
          CALL XVWRIT(IOUT(I),OBUF2,ISTAT,' ')
	END DO
      END DO
C								close files
      CALL XVCLOSE(INP,ISTAT,' ')
      DO I=1,5
	CALL XVCLOSE(IOUT(I),ISTAT,' ')
      END DO
      RETURN
      END
C*******************************************************************************
      SUBROUTINE UNPK10(ISCAN,TBUF)
      INTEGER*2 TBUF(10242)
      INTEGER*4 ISCAN(3700)
C
      J = 1
      DO I = 1, 3414
        IWORD = ISCAN(I)
	TBUF(J+2) = MOD(IWORD,1024)
	IWORD = IWORD/1024
	TBUF(J+1) = MOD(IWORD,1024)
	TBUF(J) = IWORD/1024
	J = J+3
      END DO
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create avhrrlog.pdf
process help=*
PARM INP TYPE=(STRING,40)
PARM OUT TYPE=(STRING,40) COUNT=5
END-PROC
.TITLE
VICAR Program AVHRRLOG
.HELP
AVHRRLOG is used to log AVHRR data that is in the format of AVHRR LAC/HRPT data
as supplied by NOAA.  LAC/HRPT AVHRR data consists of five channels of images,
at the following wavelengths:
                Channel             Micrometers
                   1               0.55  -  0.68 (0.55-0.90 for TIROS-N)
                   2               0.725 -  1.10
                   3               3.55  -  3.93
                   4              10.30  - 11.30 (for NOAA-7,-9,-11,-12,-I,-J)
                                  10.50  - 11.50 (for TIROS-N and NOAA-6,-8,-10)
                   5              11.50  - 11.50 for NOAA-7,-9,-11,-12,-I,-J
                                  Ch 4 repeated for TIROS-N and NOAA-6,-8,-10)
The data are acquired at 10 bit precision (data range 0-1023), and, after
logging, are stored as 5 separate halfword (16 bit) images.  Output images 
contain 2048 samples per scan line.

The input tape data for LAC/HRPT AVHRR data consists of one 122 byte header
record (called the TBM Header record), two 7400 byte header records (Data Set
Header records), and 2*N 7400 byte image data records (N = number of scan lines
in the image).  Prior to running AVHRRLOG, a VICAR label needs to be added to
the file, by running CONVIM, or by similar means.
.LEVEL1
.VARIABLE INP
Input filename,
VICAR labelled
.VARIABLE OUT
Output filenames
(five needed, one per band)
.LEVEL2
.VARIABLE INP
INP contains the name of the INPut file, with VICAR label applied via CONVIM,
or some similar means.
.VARIABLE OUT
OUT contains the names of the output files. AVHRRLOG produces five channels of
output, with the following bandpasses:
                Channel             Micrometers
                   1               0.55  -  0.68 (0.55-0.90 for TIROS-N)
                   2               0.725 -  1.10
                   3               3.55  -  3.93
                   4              10.30  - 11.30 (for NOAA-7,-9,-11,-12,-I,-J)
                                  10.50  - 11.50 (for TIROS-N and NOAA-6,-8,-10)
                   5              11.50  - 11.50 for NOAA-7,-9,-11,-12,-I,-J
                                  Ch 4 repeated for TIROS-N and NOAA-6,-8,-10)
The output datasets are halfword (16 bit) images, 2048 samples per line.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create avhrrlog.imake
#define  PROGRAM   avhrrlog

#define MODULE_LIST avhrrlog.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
