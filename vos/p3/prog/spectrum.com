$!****************************************************************************
$!
$! Build proc for MIPL module spectrum
$! VPACK Version 1.8, Friday, June 29, 2001, 19:24:15
$!
$! Execute by entering:		$ @spectrum
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
$ write sys$output "*** module spectrum ***"
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
$ write sys$output "Invalid argument given to spectrum.com file -- ", primary
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
$   if F$SEARCH("spectrum.imake") .nes. ""
$   then
$      vimake spectrum
$      purge spectrum.bld
$   else
$      if F$SEARCH("spectrum.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake spectrum
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @spectrum.bld "STD"
$   else
$      @spectrum.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create spectrum.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack spectrum.com -
	-s spectrum.f -
	-p spectrum.pdf -
	-i spectrum.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create spectrum.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
C
C  89-05-24 ...REA...  new program
C  90-03-05 ...REA...  add areas and sensors options
C  90-04-25 ...REA...  add TITLE, replace SBDS with ST_CHAN, use AXISPTS2
C  90-05-15 ...REA...  allow the user to supress the 1-sigma envelope on plots
C  90-10-01 ...REA...  add pause to PRINT option, add laser plotting capability
C                      and read wavelength file
C  94-11-07 ...REA...  add NOPLOT option
C  97-08-21 ...REA...  fix bug in NOSIGMA option
C  01-06-29 ...REA...  add MASTERTIR to list of sensors
C
	SUBROUTINE MAIN44
C
	REAL WAVE(600),DN(1800)
	INTEGER IAREA(4,50)
	LOGICAL XVPTST,QPR,QCUM,QEXCL,QSIGMA,QWINDOW,QPLOT,QPSPLOT
	CHARACTER*50 TITLE
	CHARACTER*40 NAME(2)
        CHARACTER*20 SENSOR
	CHARACTER*3 ORG
C								open input file
	CALL XVUNIT(INUNIT,'INP',1,ISTAT,' ')
	CALL XVOPEN(INUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +		    'U_FORMAT','REAL',' ')
	CALL XVBANDS(ISB,NB,NBI)
	CALL XVGET(INUNIT,ISTAT,'ORG',ORG,' ')
C					                      process parameters
C
	QWINDOW = XVPTST('WINDOW')
	QPSPLOT = XVPTST('PSCRIPT')
	QPLOT = .NOT. XVPTST('NOPLOT')
	QPR = XVPTST('PRINT')
	QCUM = XVPTST('CUMUL')
	QEXCL = XVPTST('EXCLUDE')
	QSIGMA = .NOT. XVPTST('NOSIGMA')
	CALL XVP('TITLE',TITLE,ICNT)
        CALL XVP('SENSOR',SENSOR,ICNT)
	CALL XVPARM('ST_CHAN',JSTART,ICNT,IDEF,0)
	IF (JSTART .LE. 0) JSTART = ISB
	CALL XVPARM('LINE',LINE,ICNT,IDEF,0)
	CALL XVPARM('SAMPLE',ISAMP,ICNT,IDEF,0)
	CALL XVPARM('XAXIS',DN,ICNT,IDEF,0)
	X1 = MIN(DN(1),DN(2))
	X2 = MAX(DN(1),DN(2))
	CALL XVPARM('YAXIS',DN,ICNT,IDEF,0)
	Y1 = MIN(DN(1),DN(2))
	Y2 = MAX(DN(1),DN(2))
	CALL XVPARM('AREA',IAREA,ICNT,IDEF,0)
	NAREAS = ICNT/4
	IF (.NOT. QPLOT) QPR = .TRUE.
	IF (QPSPLOT) QPLOT = .FALSE.
	IF (4*NAREAS.NE.ICNT .AND. IDEF.EQ.0) THEN
	    CALL XVMESSAGE(' Invalid number of AREA parameters',' ')
	    CALL ABEND
	ENDIF
C				if both LINE and AREA defaulted, do entire image
	IF (NAREAS.EQ.0 .AND. LINE.EQ.0) THEN
	    CALL XVSIZE(IAREA(1,1),IAREA(2,1),IAREA(3,1),IAREA(4,1),
     +			NLIN,NSIN)
	    NAREAS = 1
	END IF
C
	CALL XVMESSAGE(TITLE,' ')
	CALL XVPCNT('INP',ICNT)
	IF (ICNT .EQ. 2) THEN
	    CALL XVP('INP',NAME,ICNT)
	    CALL READWAV(NAME(2),WAVE,NB)
	ELSE
	    CALL WAVLEN(SENSOR,JSTART,NB,WAVE)
	END IF
C								  process data
	IF (NAREAS.LT.1) THEN
	    CALL READ_POINT(ORG,INUNIT,LINE,ISAMP,ISB,NB,X1,X2,Y1,Y2,
     +			    WAVE,DN,QPR,QCUM,QWINDOW,QPLOT,QPSPLOT,
     +			    TITLE)
	ELSE
	    CALL READ_AREAS(ORG,INUNIT,IAREA,NAREAS,ISB,NB,X1,X2,Y1,Y2,
     +			    WAVE,DN,QPR,QCUM,QEXCL,QSIGMA,QWINDOW,QPLOT,
     +			    QPSPLOT,TITLE)
	END IF
C
	RETURN
	END
C*******************************************************************************
	SUBROUTINE READWAV(FILENAME,WAVE,NB)
C
C      This subroutine reads NB records from the file FILENAME, and puts
C      the first numeric value that it finds in the WAVE (wavelength) array.
C      If something other than a numeric or delimiter is encountered prior to 
C      finding a numeric field, the line is discarded.
C
	CHARACTER*40 FILENAME
	REAL WAVE(NB)
C
	OPEN (51,FILE=FILENAME,STATUS='OLD')
C
	DO I=1,NB
  100	    CONTINUE
	    READ (51,*,END=900,ERR=100) WAVE(I)
	END DO
	RETURN
  900	CONTINUE
	CALL XVMESSAGE(
     +	    ' Unexpected end-of-file while reading wavelengths',' ')
	DO I=1,NB
	    WAVE(I) = I
	END DO
	RETURN
	END
C*******************************************************************************
	SUBROUTINE READ_POINT(ORG,INUNIT,LINE,ISAMP,ISB,NB,X1,X2,Y1,Y2,
     +			      WAVE,DN,QPR,QCUM,QWINDOW,QPLOT,QPSPLOT,
     +			      TITLE)
C
C					Used when the specrum of a single point
C					is requested.
C
	REAL DN(NB),WAVE(NB)
	LOGICAL QPR,QCUM,QPSPLOT,QWINDOW,QPLOT
	CHARACTER*140 MSG2
	CHARACTER*80 PRT
	CHARACTER*50 TITLE
	CHARACTER*40 XLABEL,YLABEL
	CHARACTER*3 ORG
C
	IF (ORG .EQ. 'BIP') THEN
	    CALL XVREAD(INUNIT,DN,ISTAT,'LINE',LINE,'SAMP',ISAMP,
     +			'BAND',ISB,'NBANDS',NB,' ')
	ELSE
	    DO I=1,NB
		IB = ISB+I-1
		CALL XVREAD(INUNIT,DN(I),ISTAT,'LINE',LINE,'SAMP',ISAMP,
     +			    'BAND',IB,'NSAMPS',1,' ')
	    END DO
	END IF
C				      if requested, convert to cumulative values
	IF (QCUM) THEN
	    DO I=2,NB
		DN(I) = DN(I) + DN(I-1)
	    END DO
	END IF
C					     if requested, print out the values
	IF (QPR) THEN
	    DO I=1,NB
		WRITE (PRT,100) WAVE(I),DN(I)
  100		FORMAT(5X,G12.4,G13.5)
		CALL XVMESSAGE(PRT,' ')
	    END DO
	    IF (QPLOT) CALL XVINTRACT('IPARAM','Press RETURN')
	END IF
C							compute the axis scaling
	IF (X1.EQ.X2) THEN
	    CALL MINMAX(7,NB,WAVE,X1,X2,IMIN,IMAX)
	    IF (X1.EQ.X2) THEN
		CALL XVMESSAGE(' More than 1 channel is needed.',' ')
		CALL ABEND
	    ENDIF
	    CALL AXISPTS(X1,X2,XLO,XHI,NXTIC)
	ELSE
	    CALL AXISPTS2(X1,X2,NXTIC)
	    XLO = X1
	    XHI = X2
	END IF
C
	IF (Y1.EQ.Y2) THEN
	    CALL MINMAX(7,NB,DN,Y1,Y2,IMIN,IMAX)
	    IF (Y1.EQ.Y2) Y2 = Y1+1
	    CALL AXISPTS(Y1,Y2,YLO,YHI,NYTIC)
	ELSE
	    CALL AXISPTS2(Y1,Y2,NYTIC)
	    YLO = Y1
	    YHI = Y2
	END IF
C							    screen plot the data
	IF (QWINDOW) THEN
	    OPEN (11,FILE='scrvspec001',STATUS='NEW')
	    WRITE(11,288) XLO,XHI,YLO,YHI
	    WRITE(11,277) NXTIC,NYTIC
 277        FORMAT(2I10)
 288        FORMAT(4E16.7)
 299        FORMAT(2E16.7)
	    DO I=1,NB
		WRITE (11,299) WAVE(I),DN(I)
	    END DO
	    CLOSE(11)
	    ISTAT = SYSTEM('idl vicarspec.inp')
	    OPEN (11,FILE='scrvspec001',STATUS='OLD')
	    CLOSE(11,STATUS='DELETE')
	    CALL XVINTRACT('IPARAM','Print this plot (Yes, No) [No]? ')
	    CALL XVIPARM('PENPLOT',PRT,ICNT,IDEF,0)
	    QPSPLOT = PRT(1:1).EQ.'L' .OR. PRT(1:1).EQ.'l' .OR.
     +		      PRT(1:1).EQ.'Y' .OR. PRT(1:1).EQ.'y'
	END IF
C								submit to laser
C								printer
	IF (QPSPLOT) THEN
	    CALL XVGET(INUNIT,ISTAT,'NAME',PRT,' ')
	    WRITE (MSG2,700) PRT,LINE,ISAMP
  700	    FORMAT(A40,'####Line#=#', I5, '####Sample#=#', I5)
	    LEN = 74
	    CALL SQUEEZE(MSG2,PRT,LEN)
	    CALL XVPARM('XLABEL',XLABEL,ICNT,IDEF,0)
	    CALL ADD0(XLABEL,40,LEN)
	    CALL XVPARM('YLABEL',YLABEL,ICNT,IDEF,0)
	    CALL ADD0(YLABEL,40,LEN)
	    CALL ADD0(TITLE,50,LEN)
	    MSG2 = ' ' // CHAR(0)
	    CALL PSPLOT(WAVE,DN,NB,XLO,XHI,YLO,YHI,NXTIC,NYTIC,
     +			XLABEL,YLABEL,TITLE,PRT,MSG2,-1)
	END IF
	RETURN
	END
C*******************************************************************************
	SUBROUTINE READ_AREAS(ORG,INUNIT,IAREA,NAREAS,ISB,NB,X1,X2,Y1,Y2,
     +			      WAVE,DN,QPR,QCUM,QEXCL,QSIGMA,QWINDOW,
     +			      QPLOT,QPSPLOT,TITLE)
C
	REAL DN(NB,3),WAVE(NB),BUF(10000)
	INTEGER IAREA(4,NAREAS)
	INTEGER NZEROES(224)/224*0/
	REAL*8 SUM(224)/224*0.0/,SUMSQ(224)/224*0.0/,XMEAN,PIXELS
	LOGICAL QEXCL,QPR,QCUM,QPSPLOT,QSIGMA,QWINDOW,QPLOT
	CHARACTER*140 MSG2
	CHARACTER*80 PRT
	CHARACTER*50 TITLE
	CHARACTER*40 XLABEL,YLABEL
	CHARACTER*3 ORG
C								accumulate stats
	IF (ORG .EQ. 'BSQ') THEN
	    DO N=1,NAREAS
		ISL = IAREA(1,N)
		ISS = IAREA(2,N)
		NL = IAREA(3,N)
		NS = IAREA(4,N)
		IEL = ISL + NL - 1
		NPIXELS = NPIXELS + NL*NS
		DO IBAND = 1,NB
		    DO LINE = ISL,IEL
			CALL XVREAD(INUNIT,BUF,ISTAT,'LINE',LINE,
     +			  'SAMP',ISS,'BAND',ISB+IBAND-1,'NSAMPS',NS,' ')
			DO ISAMP=1,NS
			    SUM(IBAND) = SUM(IBAND) + BUF(ISAMP)
			    SUMSQ(IBAND) = SUMSQ(IBAND) + 
     +					   BUF(ISAMP)*BUF(ISAMP)
			    IF (BUF(ISAMP) .EQ. 0.0) 
     +				NZEROES(IBAND) = NZEROES(IBAND) + 1
			END DO
		    END DO
		END DO
	    END DO
	ELSE IF (ORG .EQ. 'BIL') THEN
	    DO N=1,NAREAS
		ISL = IAREA(1,N)
		ISS = IAREA(2,N)
		NL = IAREA(3,N)
		NS = IAREA(4,N)
		IEL = ISL + NL - 1
		NPIXELS = NPIXELS + NL*NS
		DO LINE = ISL,IEL
		    DO IBAND = 1,NB
			CALL XVREAD(INUNIT,BUF,ISTAT,'LINE',LINE,
     +			  'SAMP',ISS,'BAND',IBAND+ISB-1,'NSAMPS',NS,' ')
			DO ISAMP=1,NS
			    SUM(IBAND) = SUM(IBAND) + BUF(ISAMP)
			    SUMSQ(IBAND) = SUMSQ(IBAND) + 
     +					   BUF(ISAMP)*BUF(ISAMP)
			    IF (BUF(ISAMP) .EQ. 0.0) 
     +				NZEROES(IBAND) = NZEROES(IBAND) + 1
			END DO
		    END DO
		END DO
	    END DO
	ELSE
	    DO N=1,NAREAS
		ISL = IAREA(1,N)
		ISS = IAREA(2,N)
		NL = IAREA(3,N)
		NS = IAREA(4,N)
		IEL = ISL + NL - 1
		IES = ISS + NS - 1
		NPIXELS = NPIXELS + NL*NS
		DO LINE = ISL,IEL
		    DO ISAMP = ISS,IES
			CALL XVREAD(INUNIT,BUF,ISTAT,'LINE',LINE,
     +				'SAMP',ISAMP,'BAND',ISB,'NBANDS',NB,' ')
			DO IBAND=1,NB
			    SUM(IBAND) = SUM(IBAND) + BUF(IBAND)
			    SUMSQ(IBAND) = SUMSQ(IBAND) + 
     +					   BUF(ISAMP)*BUF(IBAND)
			    IF (BUF(IBAND) .EQ. 0.0) 
     +				NZEROES(IBAND) = NZEROES(IBAND) + 1
			END DO
		    END DO
		END DO
	    END DO
	END IF
C
	IF (QPR) CALL XVMESSAGE('Wavelength  mean-sigma     mean     mea
     +n+sigma    sigma         S/N       zeroes',' ')
C							compute mean & sigma
	PIXELS = NPIXELS
	DO I=1,NB
	    IF (QEXCL) THEN
		PIXELS = NPIXELS - NZEROES(I)
		IF (PIXELS .EQ. 0.0) PIXELS=1.0
	    END IF
	    XMEAN = SUM(I)/PIXELS
	    IF (PIXELS .EQ. 1.0) THEN
		SIGMA = 0.0
	    ELSE
		SIGMA = (SUMSQ(I)-PIXELS*XMEAN*XMEAN)/(PIXELS-1.0)
		IF (SIGMA .GT. 0.0) THEN
		    SIGMA = SQRT(SIGMA)
		ELSE
		    SIGMA = 0.0
		END IF
	    END IF
	    IF (QCUM .AND. I.GT.1) XMEAN=XMEAN+DN(I-1,2)
	    DN(I,1) = XMEAN - SIGMA
	    DN(I,2) = XMEAN
	    DN(I,3) = XMEAN + SIGMA
	    IF (QPR) THEN				! if requested, print
C							! out stats
		WRITE (PRT,100) WAVE(I),(DN(I,J),J=1,3),SIGMA,
     +				XMEAN/(SIGMA+1e-10),NZEROES(I)
  100		FORMAT(6G12.5,I6)
		CALL XVMESSAGE(PRT,' ')
	    END IF
	END DO
	IF (QPR .AND. QPLOT) CALL XVINTRACT('IPARAM','Press RETURN')
C							compute the axis scaling
	IF (X1.EQ.X2) THEN
	    CALL MINMAX(7,NB,WAVE,X1,X2,IMIN,IMAX)
	    IF (X1.EQ.X2) THEN
		CALL XVMESSAGE(' More than 1 channel is needed.',' ')
		CALL ABEND
	    ENDIF
	    CALL AXISPTS(X1,X2,XLO,XHI,NXTIC)
	ELSE
	    CALL AXISPTS2(X1,X2,NXTIC)
	    XLO = X1
	    XHI = X2
	END IF
C
	IF (Y1.EQ.Y2) THEN
	    IF (QSIGMA) THEN
		CALL MINMAX(7,3*NB,DN,Y1,Y2,IMIN,IMAX)
	    ELSE
		CALL MINMAX(7,NB,DN(1,2),Y1,Y2,IMIN,IMAX)
	    END IF
	    IF (Y1.EQ.Y2) Y2 = Y1+1
	    CALL AXISPTS(Y1,Y2,YLO,YHI,NYTIC)
	ELSE
	    CALL AXISPTS2(Y1,Y2,NYTIC)
	    YLO = Y1
	    YHI = Y2
	END IF
C							    screen plot the data
	IF (QWINDOW) THEN
	    OPEN (11,FILE='scrvspec001',STATUS='NEW')
	    WRITE(11,388) XLO,XHI,YLO,YHI
	    WRITE(11,377) NXTIC,NYTIC
 377        FORMAT(2I10)
 388        FORMAT(4E16.7)
 399        FORMAT(2E16.7)
	    IF (QSIGMA) THEN
		DO I=1,NB
		    WRITE (11,388) WAVE(I),DN(I,1),DN(I,2),DN(I,3)
		END DO
		CLOSE(11)
		ISTAT = SYSTEM('idl vicarspecb.inp')
	    ELSE
		DO I=1,NB
		    WRITE (11,399) WAVE(I),DN(I,2)
		END DO
		CLOSE(11)
		ISTAT = SYSTEM('idl vicarspec.inp')
	    END IF
	    OPEN (11,FILE='scrvspec001',STATUS='OLD')
	    CLOSE(11,STATUS='DELETE')
	    CALL XVINTRACT('IPARAM','Print this plot (Yes, No) [No]? ')
	    CALL XVIPARM('PENPLOT',PRT,ICNT,IDEF,0)
	    QPSPLOT = PRT(1:1).EQ.'Y' .OR. PRT(1:1).EQ.'y' .OR.
     +		      PRT(1:1).EQ.'L' .OR. PRT(1:1).EQ.'l'
	END IF
C								submit to laser
C								printer
	IF (QPSPLOT) THEN
	    CALL XVGET(INUNIT,ISTAT,'NAME',XLABEL,' ')
	    WRITE (PRT,400) XLABEL
  400	    FORMAT('File Name = ',A40)
	    CALL ADD0(PRT,52,LEN)
C
	    IF (NAREAS.GT.5) NAREAS=5
	    WRITE (MSG2,500) ((IAREA(J,I), J=1,4), I=1,NAREAS)
  500	    FORMAT(5('(', 3(I5,','), I5, ') '))
	    MSG2(26*NAREAS:26*NAREAS) = CHAR(0)
C
	    CALL XVP('XLABEL',XLABEL,ICNT)
	    CALL XVP('YLABEL',YLABEL,ICNT)
	    CALL ADD0(XLABEL,40,LEN)
	    CALL ADD0(YLABEL,40,LEN)
	    IF (QSIGMA) THEN
	      CALL PSPLOT(WAVE,DN(1,2),NB,XLO,XHI,YLO,YHI,NXTIC,NYTIC,
     +			  XLABEL,YLABEL,TITLE,PRT,MSG2,0)
	      CALL PSPLOT(WAVE,DN(1,1),NB,XLO,XHI,YLO,YHI,NXTIC,NYTIC,
     +			  XLABEL,YLABEL,TITLE,PRT,MSG2,0)
	      CALL PSPLOT(WAVE,DN(1,3),NB,XLO,XHI,YLO,YHI,NXTIC,NYTIC,
     +			  XLABEL,YLABEL,TITLE,PRT,MSG2,-1)
	    ELSE
	      CALL PSPLOT(WAVE,DN(1,2),NB,XLO,XHI,YLO,YHI,NXTIC,NYTIC,
     +			  XLABEL,YLABEL,TITLE,PRT,MSG2,-1)
	    END IF
	END IF
C
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create spectrum.pdf
process help=*
SUBCMD-DEFAULT NORMAL
PARM INP     TYPE=(STRING,40) 		      COUNT=(1:2)
PARM BANDS   TYPE=INTEGER     DEFAULT=--      COUNT=(0:2) 
PARM SB      TYPE=INTEGER     DEFAULT=1
PARM NB      TYPE=INTEGER     DEFAULT=0
PARM ST_CHAN TYPE=INTEGER     DEFAULT=-1
PARM SENSOR  TYPE=KEYWORD     DEFAULT=--      COUNT=0:1 +
           VALID=(MASTERTIR,TM,AVIRIS,TIMS,GEOSCAN,87AVIRIS,90AVIRIS)
PARM PRINT   TYPE=KEYWORD     DEFAULT=NOPRINT VALID=(PRINT,NOPRINT)
PARM LINE    TYPE=INTEGER     DEFAULT=0
PARM SAMPLE  TYPE=INTEGER     DEFAULT=0
PARM AREA    TYPE=INTEGER     DEFAULT=0       COUNT=(0:200)
PARM XAXIS   TYPE=REAL        DEFAULT=(0,0)   COUNT=2
PARM YAXIS   TYPE=REAL        DEFAULT=(0,0)   COUNT=2
PARM XLABEL  TYPE=(STRING,40) DEFAULT="Channel"
PARM YLABEL  TYPE=(STRING,40) DEFAULT="DN"
PARM TITLE   TYPE=(STRING,40) DEFAULT=" "
PARM SIGMA   TYPE=KEYWORD     DEFAULT=SIGMA   VALID=(SIGMA,NOSIGMA)
PARM EXCLUDE TYPE=KEYWORD     DEFAULT=NOEXCL  VALID=(EXCLUDE,NOEXCL)
PARM CUMUL   TYPE=KEYWORD     DEFAULT=--      VALID=CUMUL    COUNT=0:1
PARM MODE    TYPE=KEYWORD DEFAULT=WINDOW VALID=(NOPLOT,WINDOW,PSCRIPT)
END-SUBCMD
SUBCMD IPARAM
PARM PENPLOT TYPE=KEYWORD DEFAULT=-- VALID=(YES,NO,LASER) COUNT=(0:1)
END-SUBCMD
END-PROC
.TITLE
VICAR Program SPECTRUM
.HELP
PURPOSE:
SPECTRUM produces a plot of DN versus channel or wavelength for a single 3-D 
pixel, or of the mean DN for a region specified by one or more rectangular
areas. The plot may be directed to an x-window (via IDL), or to a
PostScript printer.

WRITTEN BY:  Ron Alley,  May 1989
COGNIZANT PROGRAMMER:  Ron Alley
REVISION: 3 - November 8, 1994

.LEVEL1
.VARIABLE INP
Input image file,
Optional wavelength file
.VARIABLE BANDS
Starting band, # of bands
.VARIABLE SB
Starting band 
.VARIABLE NB
Number of bands
.VARIABLE LINE
Line number of plotted pixel
.VARIABLE SAMPLE
Sample number of plotted pixel
.VARIABLE XAXIS
Range of x-axis (bands/wavlen)
.VARIABLE YAXIS
Range of y-axis (DN's)
.VARIABLE AREA
(SL1,SS1,NL1,NS1,...,
SLn,SSn,NLn,NSn)
.VARIABLE SENSOR
MASTERTIR, TM, AVIRIS, TIMS,
GEOSCAN, 87AVIRIS, 90AVIRIS
.VARIABLE XLABEL
(for laser printer only)
label for for x-axis
.VARIABLE YLABEL
(for laser printer only)
label for for y-axis
.VARIABLE ST_CHAN
Channel number of first 
channel to be plotted
.VARIABLE TITLE
Title to be placed on pen plot
.VARIABLE CUMUL
Plot the cumulative function
instead of the spectrum
.VARIABLE PRINT
Print a table of the values?
(PRINT or NOPRINT)
.VARIABLE SIGMA
Plot the 1-std deviation 
envelope also?
(SIGMA and NOSIGMA)
.VARIABLE EXCLUDE
Exclude zero valued pixels?
(EXCLUDE or NOEXCL)
.VARIABLE MODE
use NOPLOT to suppress
plotting and pauses,
use WINDOW to plot in
Xwindow, use PSCRIPT for a
hardcopy plot
.LEVEL2
.vari INP
The first value of INP is the name of the input image file.  A second input
file may be used to supply the wavelengths of the input channels.  This 
second input file should be an ASCII file (no VICAR labels) in the form of
a table.  The wavelength values must be the first column (there need not be
any other columns), and there must be one row per channel.
.vari BANDS
BANDS specifies the starting band desired, and the number of bands to be 
processed.  If the first channel in the dataset is not Channel 1, the 
parameter ST_CHAN should also be specified.
.vari SB
This parameter denotes the first channel in the dataset to be included in the 
plotting. If the first channel in the dataset is not Channel 1, the parameter 
ST_CHAN should also be specified.
.vari NB
This parameter denotes the number of bands to be included in the plot.
.vari LINE
This is the line number of the pixel to be plotted. If the spectrum of a region
is wanted, the AREA parameter is used and this parameter is ignored.
.vari SAMPLE
This is the sample number of the pixel to be plotted. If the spectrum of a 
region is wanted, the AREA parameter is used and this parameter is ignored.
.vari XAXIS
XAXIS indicates the full scale limits of the x-axis. If defaulted, the program
chooses limits that completely span the range of the data plotted. In some
cases, the program will adjust (widen) the user specified limits, in order to
place the tic marks at reasonable values.
.vari YAXIS
YAXIS indicates the full scale limits of the y-axis. If defaulted, the program
chooses limits that completely span the range of the data plotted. In some
cases, the program will adjust (widen) the user specified limits, in order to
place the tic marks at reasonable values.
     In addition to the mean spectral values, the one standard deviation range
is also plotted, ifthe AREA option is chosen.
.VARIABLE XLABEL
XLABEL is the title for the x-axis on plots printed on the laser printer. It
is not used on any other plots. The default is "Channel".
.VARIABLE YLABEL
YLABEL is the title for the y-axis on plots printed on the laser printer. It
is not used on any other plots. The default is "DN".
.VARIABLE AREA
If the average spectum of a region is desired, it may be specified using the
AREA parameter.  Up to fifty rectangular areas, specified by SL,SS,NL,NS, may
be used to form the average spectrum. The areas need not be contiguous, and if
two area overlap, those pixels are counted twice. If the AREA parameter is used,
the LINE and SAMPLE parameters are ignored.
.VARIABLE SENSOR
The channel wavelengths of certain sensors are known to this program. By 
specifying the name of the sensor, DN values are plotted against wavelengths
(in micrometers) rather than channel number.  If the sensor is not specified
or not known, the channel number is used.  As of April 2,1991, AVIRIS, TIMS,
TM, 87AVIRIS, 90AVIRIS, and GEOSCAN wavelengths are known to this program.
.VARIABLE ST_CHAN
This parameter indicates the channel number of the first channel to be plotted.
It must be specified if the first channel in the input dataset is not Channel 1
of the sensor.  If defaulted, ST_CHAN is assumed to be equal to the starting
band (SB).
.VARIABLE CUMUL
Specifying this parameter produces a plot of the cumulative DN values for the
spectrum, and not the spectrum itself.
.VARIABLE TITLE
This parameter allows the user to place a title or line of annotation on the
printed plot, session log, and standard output, if such products are produced.
It may be up to 40 characters in length.
.VARIABLE PRINT
Specifying this parameter causes a table of the spectral values to be printed
to the user and in the SESSION.LOG
.VARIABLE SIGMA
In the default mode (SIGMA) for the spectrum of an area, the one standard 
deviation envelope about the mean spectrum is plotted.  If NOSIGMA is 
specified, this envelope is not plotted.  If the spectrum is for a single 
point, rather than an area, this keyword has no relevance, and is ignored.
.VARIABLE EXCLUDE
This parameter is specified if the user wishes to exclude zero valued pixels
from the compuation of the mean and stadard deviation.
.VARIABLE MODE
If WINDOW is specified (the default), spectrum plots the graph in an IDL
Xwindow. If PSCRIPT is specified, no interactive plotting is done, but a
hardcopy PostScript plot is made.  If NOPLOT is specified, printing is enabled,
plotting is disabled, and there are no interactive pauses.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create spectrum.imake
#define  PROGRAM   spectrum

#define MODULE_LIST spectrum.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
