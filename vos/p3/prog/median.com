$!****************************************************************************
$!
$! Build proc for MIPL module median
$! VPACK Version 1.5, Monday, March 29, 1993, 14:50:39
$!
$! Execute by entering:		$ @median
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
$ write sys$output "*** module median ***"
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
$   if F$SEARCH("median.imake") .nes. ""
$   then
$      vimake median
$      purge median.bld
$   else
$      if F$SEARCH("median.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake median
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @median.bld "STD"
$   else
$      @median.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create median.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack median.com -
	-s median.f -
	-p median.pdf -
	-i median.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create median.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C
C     PROGRAM MEDIAN
C      4 DEC 91    ...REA...   REMOVE BYTE RESTICTION, ADDING ALGORITHMS
C                              MOVE REFLECTION AXIS TO TRUE EDGE OF IMAGE
C     24 APR 91    ...REA...   CONVERT TO UNIX/VICAR
C     30 NOV 83    ...HBD...   REWRITE ASSEMBLER TO FORTRAN
C     22 JULY 80   ...HJF...   MAKE USE OF FASTER ALGORITHM
C     18 DEC 78    ...WDB...    INITIAL RELEASE
C
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      EXTERNAL FILT,HFILT,VFILT,BFILT

      COMMON /C1/ INUNIT,IOUTUNIT,ISL,ISS,NL,NS,NLIN,NSIN,NLW,NSW,IVAL,
     +            HIGH,DCLEV,DCTRAN,XMIN,XMAX
      LOGICAL HIGH,XVPTST
      REAL XMINX(4)/  0.0,-32768.0,-2147483648.0,-1.6E38/
      REAL XMAXX(4)/255.0, 32767.0, 2147483647.0, 1.6E38/
      CHARACTER*10 FMT

C
C  OPEN INPUT & OUTPUT FOR SEQUENTIAL I/O
C
      CALL XVUNIT(INUNIT,'INP',1,STATUS,' ')
      CALL XVOPEN(INUNIT,STATUS,'OPEN_ACT','SA','IO_ACT','SA',
     +            'U_FORMAT','REAL',' ')
      CALL XVUNIT(IOUTUNIT,'OUT',1,STATUS,' ')
      CALL XVOPEN(IOUTUNIT,STATUS,'OP','WRITE','OPEN_ACT','SA',
     +          'IO_ACT','SA','U_FORMAT','REAL',' ')
C
      CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
      CALL XVGET(INUNIT,STATUS,'PIX_SIZE',NBPP,'FORMAT',FMT,' ')
      IF (NBPP.EQ.4 .AND. FMT.EQ.'FULL') NBPP=3
      XMIN = XMINX(NBPP)
      XMAX = XMAXX(NBPP)
C
C  GET NLW AND NSW VALUES
      CALL XVP('NLW',NLW,CNT)
      CALL XVP('NSW',NSW,CNT)
      NLW = (NLW/2)*2 + 1
      NSW = (NSW/2)*2 + 1
      IF (NLW .LE. 0) THEN
	 CALL XVMESSAGE(' ***ILLEGAL NLW VALUE',' ')
	 CALL ABEND
      ENDIF
      IF (NSW .LE. 0) THEN
	 CALL XVMESSAGE(' ***ILLEGAL NSW VALUE',' ')
	 CALL ABEND
      ENDIF
C
C  GET PERCENT AND DETERMINE THRESHOLD VALUE
      CALL XVP('PERCENT',PERC,CNT)
      PERC = PERC / 100.0
      IF (PERC .LE. 0.0 .AND. PERC .GT. 1.0) THEN
	 CALL XVMESSAGE(' ***ILLEGAL PERCENT VALUE',' ')
	 CALL ABEND
      ENDIF
      IVAL = NINT(PERC*NLW*NSW + 0.5)
C
C  DETERMINE IF HIGHPASS;  IF SO, GET DCTRAN AND DCLEVEL
      HIGH = XVPTST('HIGHPASS')
      IF (HIGH) THEN
         CALL XVP('DCTRAN',DCTRAN,CNT)
         CALL XVP('DCLEVEL',DCLEV,CNT)
      END IF
C
C  DETERMINE ARRAY SIZES AND CALL STACKA
      NLWX = NLW
      NSX = NS + NSW - 1
      NWIN = NLW*NSW
      N1 = 4*NSX*NLWX
      N2 = 4*NS
      N3 = N1
      N4 = 8*NWIN
      N5 = 4*NLW
      N6 = 4*NSW
      N7 = 1024
      IF (NBPP .EQ. 1) THEN
	 CALL STACKA(8,BFILT,3,N1,N2,N7,NLWX,NSX,NWIN)
      ELSE IF (NSW .EQ. 1) THEN
	 CALL STACKA(7,VFILT,3,N1,N2,N5,NLWX,NSX)
      ELSE IF (NLW .EQ. 1) THEN
	 CALL STACKA(5,HFILT,3,N1,N2,N6)
      ELSE
         CALL STACKA(9,FILT,4,N1,N2,N3,N4,NLWX,NSX,NWIN)
      END IF
C
C *****         CLOSE DATA SETS
      CALL XVCLOSE(INUNIT,STATUS,' ')
      CALL XVCLOSE(IOUTUNIT,STATUS,' ')
      RETURN
      END
C**********************************************************
      SUBROUTINE BFILT(BUF,N1,OUT,N2,IHIST,N3,NLWX,NSX,NPIXELS)
C
C     BFILT is the median filtering routine used for all cases of byte
C     data input.  It searches a histogram of the window of surrounding
C     values for the median.  It is usually the fastest of the algorithms,
C     but works only on byte data.
C
      INTEGER BUF(*),OUT(*),IHIST(0:255)
C
      COMMON /C1/ INUNIT,IOUTUNIT,ISL,ISS,NL,NS,NLIN,NSIN,NLW,NSW,IVAL,
     +            HIGH,DCLEV,DCTRAN,XMIN,XMAX
      LOGICAL HIGH
C
      INSIZE = NLWX*NSX			! size of BUF array
      NLW2 = NLW/2			! half of the line weights
      NSW2 = NSW/2			! half of the sample weights
      IST = MAX(ISS-NSW2,1)		! first sample to be read in
      LAST = MIN(ISS+NS+NSW2-1,NSIN)	! last sample to be read in
      NLEFT = IST - ISS + NSW2		! number of pixels to pad on left
      NRIGHT = NS + NSW2 - LAST 	! number of pixels to pad on right
      NSREAD = LAST - IST + 1		! number of samples to be read
      LOC = INSIZE-NSX+1		! line position in input buffer
      JVAL = NPIXELS - IVAL + 1		! we are searching for the JVAL'th
C					  highest and IVAL'th lowest value in
C					  the window
C
C					Close and reopen datasets, so that we
C					can use integer buffers
      CALL XVCLOSE(INUNIT,ISTAT,' ')
      CALL XVCLOSE(IOUTUNIT,ISTAT,' ')
      CALL XVOPEN(INUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +		  'U_FORMAT','FULL',' ')
      CALL XVOPEN(IOUTUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +		  'U_FORMAT','FULL','OP','WRITE',' ')
C
C						Set up input buffer
      DO LINE = ISL+NLW2, ISL-NLW2, -1
         IF (LINE .GE. 1) THEN
 	    CALL XVREAD(INUNIT,BUF(NLEFT+LOC),ISTAT,'LINE',LINE,
     +		        'SAMP',IST,'NSAMPS',NSREAD,' ')
	    CALL REFLCT(BUF(LOC),NLEFT,NRIGHT,NS+2*NSW2)
	    LOCX = LOC
	 ELSE
	    CALL MVE(4,NSX,BUF(LOCX),BUF(LOC),1,1)
	    LOCX = LOCX + NSX
	 END IF
	 LOC = LOC - NSX
      END DO
C
      LOC = 1					!location for next line into BUF
      LINE = ISL + NLW2 +1			!line number of next line
C						looping thru all output lines
      DO L=1,NL
C						   initialize histogram
	 CALL ZIA(IHIST,256)
	 N = 0
	 DO I=1,NLW
	    DO J=1,NSW
	       IHIST(BUF(N+J)) = IHIST(BUF(N+J)) + 1
	    END DO
	    N = N + NSX
	 END DO
	 NLOW = 0
	 MED = -1
	 DO WHILE (NLOW .LT. IVAL)
	    MED = MED + 1
	    NLOW = NLOW + IHIST(MED)
	 END DO
	 OUT(1) = MED
	 NHIGH = NPIXELS - NLOW			! number of pixels > median
	 NLOW = NLOW - IHIST(MED)		! number of pixels < median
C						    compute value for each pixel
         NREM = 1
         NADD = NREM + NSW
	 DO ISAMP=2,NS
C							update histogram
	    N = 0
	    DO I=1,NLW
	       NUM = BUF(N+NREM)
	       IHIST(NUM) = IHIST(NUM) - 1
	       IF (NUM .GT. MED) THEN
		  NHIGH = NHIGH - 1
	       ELSE IF (NUM .LT. MED) THEN
		  NLOW = NLOW - 1
	       END IF
	       NUM = BUF(N+NADD)
	       IHIST(NUM) = IHIST(NUM) + 1
	       IF (NUM .GT. MED) THEN
		  NHIGH = NHIGH + 1
	       ELSE IF (NUM .LT. MED) THEN
		  NLOW = NLOW + 1
	       END IF
	       N = N + NSX
	    END DO
C							search for median
	    IF (NLOW .GE. IVAL) THEN
	       DO WHILE (NLOW .GE. IVAL)
		  MED = MED - 1
		  NLOW = NLOW - IHIST(MED)
	       END DO
	       NHIGH = NPIXELS - NLOW - IHIST(MED)
	    ELSE IF (NHIGH .GE. JVAL) THEN
	       DO WHILE (NHIGH .GE. JVAL)
		  MED = MED +1
		  NHIGH = NHIGH - IHIST(MED)
	       END DO
	       NLOW = NPIXELS - NHIGH - IHIST(MED)
	    END IF
	    OUT(ISAMP) = MED
	    NREM = NREM + 1
	    NADD = NADD + 1
	 END DO
C							for highpass filtering
	 IF (HIGH) THEN
	    INLINE = MOD(LOC+NLW2*NSX+NSW2,INSIZE)
	    CALL HPI(BUF(INLINE),OUT)
	 END IF
C								write output
	 CALL XVWRIT(IOUTUNIT,OUT,ISTAT,'NSAMPS',NS,' ')
C							read in next needed line
	 IF (LINE .LE. NLIN) THEN
	    CALL XVREAD(INUNIT,BUF(NLEFT+LOC),ISTAT,'LINE',LINE,
     +			'SAMP',IST,'NSAMPS',NSREAD,' ')
	    CALL REFLCT(BUF(LOC),NLEFT,NRIGHT,NSX)
	    LOCX = LOC
	 ELSE
	    CALL MVE(4,NSX,BUF(LOCX),BUF(LOC),1,1)
	    LOCX = LOCX - NSX
	    IF (LOCX .LT. 1) LOCX = LOCX + INSIZE
	 END IF
	 LOC = LOC + NSX
	 IF (LOC .GT. INSIZE) LOC = 1
	 LINE = LINE + 1
      END DO
      RETURN
      END
C**********************************************************
      SUBROUTINE FILT(BUF,N1,OUT,N2,COL,N3,WINDOW,N4,NLWX,NSX,NPIXELS)
C
C     This routine is used for non-byte data and a two dimensional (NLW>1,
C     NSW>1) window.  This algorithm is the slowest, but works for the
C     general case.
C
      REAL BUF(NSX,NLWX),OUT(*),COL(NLWX,NSX),WINDOW(NPIXELS,2)
C
      COMMON /C1/ INUNIT,IOUTUNIT,ISL,ISS,NL,NS,NLIN,NSIN,NLW,NSW,IVAL,
     +            HIGH,DCLEV,DCTRAN,XMIN,XMAX
      LOGICAL HIGH
C
      NLW2 = NLW/2			! half of the line weights
      NSW2 = NSW/2			! half of the sample weights
      IST = MAX(ISS-NSW2,1)		! first sample to be read in
      LAST = MIN(ISS+NS+NSW2-1,NSIN)	! last sample to be read in
      NLEFT = IST - ISS + NSW2		! number of pixels to pad on left
      NRIGHT = NS + NSW2 - LAST 	! number of pixels to pad on right
      NSREAD = LAST - IST + 1		! number of samples to be read
      LOC = NLW				! line position in input buffer
C
C						Set up input buffer
      DO LINE = ISL+NLW2, ISL-NLW2, -1
         IF (LINE .GE. 1) THEN
 	    CALL XVREAD(INUNIT,BUF(NLEFT+1,LOC),ISTAT,'LINE',LINE,
     +		        'SAMP',IST,'NSAMPS',NSREAD,' ')
	    CALL REFLCT(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
	 ELSE
	    CALL MVE(7,NS+NSW-1,BUF(1,NLW-LOC),BUF(1,LOC),1,1)
	 END IF
	 LOC = LOC - 1
      END DO
C
      LOC = 1					!location for next line into BUF
      LINE = ISL + NLW2 +1			!line number of next line
C						looping thru all output lines
      DO L=1,NL
C						   initialize columns buffers
         DO I=1,NLW
            CALL MVE(7,NSX,BUF(1,I),COL(I,1),1,NLW)
         END DO
         DO I=1,NSX
            CALL SORTM(COL(1,I),NLW)
         END DO
C                                                  initialize window
         CALL MVE(7,NPIXELS,COL,WINDOW,1,1)
         CALL SORTM(WINDOW,NPIXELS)
C						    compute value for each pixel
	 NOW = 1
	 NEXT = 2
         OUT(1) = WINDOW(IVAL,NOW)
         NREM = 1
         NADD = NREM + NSW
	 DO ISAMP=2,NS
	    CALL MERGEM(WINDOW(1,NOW),COL(1,NREM),COL(1,NADD),
     +		        WINDOW(1,NEXT),NPIXELS,NLW)
	    OUT(ISAMP) = WINDOW(IVAL,NEXT)
	    N = NOW
	    NOW = NEXT
	    NEXT = N
	    NREM = NREM + 1
	    NADD = NADD + 1
	 END DO
C							for highpass filtering
	 IF (HIGH) THEN
	    INLINE = MOD(LOC+NLW2-1,NLW) + 1
	    CALL HPR(BUF(NSW2+1,INLINE),OUT)
	 END IF
C								write output
	 CALL XVWRIT(IOUTUNIT,OUT,ISTAT,'NSAMPS',NS,' ')
C							read in next needed line
	 IF (LINE .LE. NLIN) THEN
	    CALL XVREAD(INUNIT,BUF(NLEFT+1,LOC),ISTAT,'LINE',LINE,
     +			'SAMP',IST,'NSAMPS',NSREAD,' ')
	    CALL REFLCT(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
	    LOCX = LOC
	 ELSE
	    CALL MVE(7,NS+NSW-1,BUF(1,LOCX),BUF(1,LOC),1,1)
	    LOCX = LOCX - 1
	    IF (LOCX .LT. 1) LOCX=NLW
	 END IF
	 LOC = LOC + 1
	 IF (LOC .GT. NLW) LOC = 1
	 LINE = LINE + 1
      END DO
      RETURN
      END
C**********************************************************
      SUBROUTINE VFILT(BUF,N1,OUT,N2,WINDOW,N3,NLWX,NSX)
C
C     This routine is used for non-byte data and a vertical window (NSW=1)
C
      REAL BUF(NSX,NLWX),OUT(*),WINDOW(NLWX)
C
      COMMON /C1/ INUNIT,IOUTUNIT,ISL,ISS,NL,NS,NLIN,NSIN,NLW,NSW,IVAL,
     +            HIGH,DCLEV,DCTRAN,XMIN,XMAX
      LOGICAL HIGH
C
      NLW2 = NLW/2			! half of the line weights
      LOC = NLW				! line position in input buffer
C
C						Set up input buffer
      DO LINE = ISL+NLW2, ISL-NLW2, -1
         IF (LINE .GE. 1) THEN
 	    CALL XVREAD(INUNIT,BUF(1,LOC),ISTAT,'LINE',LINE,
     +		        'SAMP',ISS,'NSAMPS',NS,' ')
	 ELSE
	    CALL MVE(7,NS,BUF(1,NLW-LOC),BUF(1,LOC),1,1)
	 END IF
	 LOC = LOC - 1
      END DO
C
      LOC = 1					!location for next line into BUF
      LINE = ISL + NLW2 +1			!line number of next line
C						looping thru all output lines
      DO L=1,NL
C						looping through each pixel
         DO I=1,NS
	    DO J=1,NLW
	       WINDOW(J) = BUF(I,J)
	    END DO
            CALL SORTM(WINDOW,NLW)
	    OUT(I) = WINDOW(IVAL)
         END DO
C							for highpass filtering
	 IF (HIGH) THEN
	    INLINE = MOD(LOC+NLW2-1,NLW) + 1
	    CALL HPR(BUF(1,INLINE),OUT)
	 END IF
C								write output
	 CALL XVWRIT(IOUTUNIT,OUT,ISTAT,'NSAMPS',NS,' ')
C							read in next needed line
	 IF (LINE .LE. NLIN) THEN
	    CALL XVREAD(INUNIT,BUF(1,LOC),ISTAT,'LINE',LINE,
     +			'SAMP',ISS,'NSAMPS',NS,' ')
	    LOCX = LOC
	 ELSE
	    CALL MVE(7,NS,BUF(1,LOCX),BUF(1,LOC),1,1)
	    LOCX = LOCX - 1
	    IF (LOCX .LT. 1) LOCX=NLW
	 END IF
	 LOC = LOC + 1
	 IF (LOC .GT. NLW) LOC = 1
	 LINE = LINE + 1
      END DO
      RETURN
      END
C**********************************************************
      SUBROUTINE HFILT(BUF,N1,OUT,N2,WINDOW,N3)
C
C     This routine is used for non-byte data and a horizontal window
C     (NLW=1)
C
      REAL BUF(*),OUT(*),WINDOW(*)
C
      COMMON /C1/ INUNIT,IOUTUNIT,ISL,ISS,NL,NS,NLIN,NSIN,NLW,NSW,IVAL,
     +            HIGH,DCLEV,DCTRAN,XMIN,XMAX
      LOGICAL HIGH
C
      NSW2 = NSW/2			! half of the sample weights
      IST = MAX(ISS-NSW2,1)		! first sample to be read in
      LAST = MIN(ISS+NS+NSW2-1,NSIN)	! last sample to be read in
      NLEFT = IST - ISS + NSW2		! number of pixels to pad on left
      NRIGHT = NS + NSW2 - LAST 	! number of pixels to pad on right
      NSREAD = LAST - IST + 1		! number of samples to be read
C
C							For each output line
      DO LINE = ISL,ISL+NL-1
C							Read input, reflect ends
	 CALL XVREAD(INUNIT,BUF(NLEFT+1),ISTAT,'LINE',LINE,'SAMP',IST,
     +		    'NSAMPS',NSREAD,' ')
	 CALL REFLCT(BUF,NLEFT,NRIGHT,NS+2*NSW2)
C	                                                initialize window
         CALL MVE(7,NSW,BUF,WINDOW,1,1)
         CALL SORTM(WINDOW,NSW)
C						    compute value for each pixel
         OUT(1) = WINDOW(IVAL)
	 NREM = 1
	 NADD = NREM + NSW
	 DO ISAMP=2,NS
C							add right pixel,
C						 remove left pixel from window
	    IF (BUF(NADD) .GT. BUF(NREM)) THEN
	       I = 1
	       DO WHILE (WINDOW(I) .NE. BUF(NREM))
		  I = I+1
	       END DO
	       DO WHILE (I.LT.NSW .AND. WINDOW(I+1).LT.BUF(NADD))
		  WINDOW(I) = WINDOW(I+1)
		  I = I+1
	       END DO
	       WINDOW(I) = BUF(NADD)
	    ELSE IF (BUF(NADD) .LT. BUF(NREM)) THEN
	       I = NSW
	       DO WHILE (WINDOW(I) .NE. BUF(NREM))
		  I = I-1
	       END DO
	       DO WHILE (I.GT.1 .AND. WINDOW(I-1).GT.BUF(NADD))
		  WINDOW(I) = WINDOW(I-1)
		  I = I-1
	       END DO
	       WINDOW(I) = BUF(NADD)
	    END IF
C
	    OUT(ISAMP) = WINDOW(IVAL)
	    NREM = NREM + 1
	    NADD = NADD + 1
	 END DO
C							for highpass filtering
	 IF (HIGH) CALL HPR(BUF(NSW2+1),OUT)
C								write output
	 CALL XVWRIT(IOUTUNIT,OUT,ISTAT,'NSAMPS',NS,' ')
      END DO
      RETURN
      END
C******************************************************************************
	SUBROUTINE REFLCT(BUF,NLEFT,NRIGHT,LEN)
C
	INTEGER BUF(LEN)
C
	DO I=1,NLEFT
	    BUF(I) = BUF(2*NLEFT-I+1)
	END DO
	DO I=1,NRIGHT
	    BUF(LEN-I+1) = BUF(LEN-2*NRIGHT+I)
	END DO
	RETURN
	END
C******************************************************************************
	SUBROUTINE MERGEM(BUFOLD,BUFREM,BUFADD,BUFNEW,NUM,NLW)
C
	REAL BUFOLD(NUM),BUFREM(NLW),BUFADD(NLW),BUFNEW(NUM)
C
	LOCOLD = 1
	LOCADD = 1
	LOCREM = 1
	DO WHILE (BUFREM(LOCREM) .EQ. BUFOLD(LOCOLD) .AND.
     +		  LOCREM .LE. NLW)
	    LOCREM = LOCREM + 1
	    LOCOLD = LOCOLD + 1
	END DO
	DO I = 1,NUM
	    IF (BUFOLD(LOCOLD).LE.BUFADD(LOCADD) .OR. LOCADD.GT.NLW)THEN
		BUFNEW(I) = BUFOLD(LOCOLD)
	 	LOCOLD = LOCOLD + 1
		IF (LOCOLD .GT. NUM) THEN
		    DO J=0,NLW-LOCADD
			BUFNEW(I+J+1) = BUFADD(LOCADD+J)
		    END DO
		    RETURN
		END IF
		DO WHILE (BUFREM(LOCREM) .EQ. BUFOLD(LOCOLD) .AND.
     +			  LOCREM .LE. NLW)
		    LOCREM = LOCREM + 1
		    IF (LOCOLD .EQ. NUM) THEN
			BUFOLD(NUM) = BUFADD(NLW) + 1.0
		    ELSE
			LOCOLD = LOCOLD + 1
		    END IF
		END DO
	    ELSE
		BUFNEW(I) = BUFADD(LOCADD)
		LOCADD = LOCADD + 1
	    END IF
	END DO
	RETURN
	END
C*******************************************************************************
	SUBROUTINE SORTM(ARR,NUM)
C
C	This is an NlogN sort,  of NUM values in the real array ARR,
C	low to high
C
	REAL ARR(NUM)
C
	ISPAN = NUM/2
	DO WHILE (ISPAN .GT. 0)
	    LAST = NUM - ISPAN
	    LOC = 1
	    DO WHILE (LOC .LE. LAST)
		I1 = LOC
		DO WHILE (I1 .GE. 1)
		    I2 = I1 + ISPAN
		    IF (ARR(I1) .LE. ARR(I2)) GO TO 100
		    HOLD = ARR(I1)
		    ARR(I1) = ARR(I2)
		    ARR(I2) = HOLD
		    I1 = I1 - ISPAN
		END DO
  100		CONTINUE
		LOC = LOC + 1
	    END DO
	    ISPAN = ISPAN/2
	END DO
C
	RETURN
	END
C*******************************************************************************
      SUBROUTINE HPR(IN,OUT)
C
C ***** FUNCTION TO PERFORM HIGH PASS FILTERING ON LINE OF REAL VALUES
C
      REAL IN(*),OUT(*)
C
      COMMON /C1/ INUNIT,IOUTUNIT,ISL,ISS,NL,NS,NLIN,NSIN,NLW,NSW,IVAL,
     +            HIGH,DCLEV,DCTRAN,XMIN,XMAX
C
      DO I = 1,NS
	X = IN(I) - OUT(I) + DCLEV + DCTRAN*OUT(I)
        OUT(I) = MIN(XMAX, MAX(XMIN, X))
      END DO
      RETURN
      END
C*******************************************************************************
      SUBROUTINE HPI(IN,OUT)
C
C ***** FUNCTION TO PERFORM HIGH PASS FILTERING ON LINE OF INTEGER VALUES
C
      INTEGER IN(*),OUT(*)
C
      COMMON /C1/ INUNIT,IOUTUNIT,ISL,ISS,NL,NS,NLIN,NSIN,NLW,NSW,IVAL,
     +            HIGH,DCLEV,DCTRAN,XMIN,XMAX
C
      DO I = 1,NS
	X = IN(I) - OUT(I) + DCLEV + DCTRAN*OUT(I)
        OUT(I) = MIN(XMAX, MAX(XMIN, X))
      END DO
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create median.pdf
process help=*
PARM INP	TYPE = (STRING,60)
PARM OUT	TYPE = (STRING,60)		              DEFAULT = MEDIAN
PARM SIZE	TYPE = INTEGER 	COUNT=0:4    	              DEFAULT=--
PARM SL         TYPE = INTEGER  COUNT=0:1                     DEFAULT=--
PARM SS         TYPE = INTEGER  COUNT=0:1                     DEFAULT=--
PARM NL		TYPE = INTEGER  COUNT=0:1		      DEFAULT=--
PARM NS         TYPE = INTEGER  COUNT=0:1		      DEFAULT=--
PARM NLW        TYPE = INTEGER			              DEFAULT = 11
PARM NSW        TYPE = INTEGER		 	              DEFAULT = 11
PARM HIGHPASS	TYPE = KEYWORD  VALID ="HIGHPASS" COUNT=(0:1) DEFAULT =--
PARM DCLEVEL	TYPE = REAL			              DEFAULT = 128.0
PARM DCTRAN     TYPE = REAL			              DEFAULT = 0.0
PARM PERCENT	TYPE = REAL			              DEFAULT = 50.0
END-PROC
.TITLE
VICAR1 program MEDIAN: Performs spatial median filtering of an image.
.HELP
PURPOSE:

MEDIAN is a VICAR applications program which performs nonlinear spatial
filtering of an image based upon the local (rectangular window) median or
percentile rank of the input. Output may be in either lowpass or highpass form.

.PAGE
EXECUTION:

MEDIAN may be executed in the following manner:

		MEDIAN INP=A OUT=B SIZE=(SL,SS,NL,NS) PARAMS

where INP, OUT, SIZE, AND PARAMS are parameters and are explained in their
respective parameter section.

.PAGE
OPERATION:

MEDIAN finds the local median value of a rectangular window centered at
each pixel by accumulating  a histogram of all contained pixels. This
histogram could be interrogated for three basic statistical quantities:
mean, mode, and median. The mean is the average value in the histogram, the
mode is the most frequently occurring value in the histogram, and the median
is the value which equally divides the population of the histogram. 

The output value of MEDIAN may be modified by the inclusion of the PERCENT
keyword.  For example, PERCENT=25.0 will output the value which has 25% of 
the window pixels less than it, and 75% of the window pixels greater than it.

If HIGHPASS is specified, the final output is given as:

		OUT = IN - MEDIAN + DCLEVEL + DCTRAN*IN

.PAGE
BIBLIOGRAPHY:

Huang, Yang, and Tang, "A Fast Two-Dimensional Median Filtering Algorithm,"
	IEEE Trans., Vol. ASSP-27, No. 1, February 1979.
.PAGE
EXAMPLES:

	1) MEDIAN INP=A OUT=B NLW=5 NSW=7

		This example performs the lowpass median filter of size 5 lines
		by 7 samples.

	2) MEDIAN INP=A OUT=B 'HIGH NLW=3 NSW=3

		This example performs the highpass median filter of size
		3x3 pixels.

	3) MEDIAN INP=A OUT=B PERCENT=20 NLW=7 NSW=9

		This example outputs a value corresponding to the 20% level
 		of a histogram generated by a 7 line by 9 sample filter. The
		output is then the 12th lowest value in the local window
		rather than the 31st lowest value (50% level).
.PAGE
HISTORY:

ORIGINALLY WRITTEN BY: W. D. Benton, 27 November 1978
REWRITTEN WITH FASTER ALGORITHM BY:  H. J. Frieden,  22 July 1980
CONVERTED TO VAX BY:  Helen De Rueda,  30 Nov. 1983
REWRITTEN TO PERMIT NON-BYTE DATA: Ron Alley, 4 Dec 1991
COGNIZANT PROGRAMMER:  Ron Alley
.LEVEL1
.VARI INP
input dataset.
.VARI OUT
output dataset.
.VARI SIZE
VICAR size field,
(SL,SS,NL,NS).
.VARI SL
starting line to be output
.VARI SS
first sample to be output
.VARI NL
number of lines
.VARI NS
Number of samples
.VARI NLW
Size of filter in lines
.VARI NSW
Size of filter in samples
.VARI HIGHPASS
KEYWORD-Highpass option.
.VARI DCLEVEL
Offset to be added to
highpass output.
.VARI PERCENT
Output value percentile
.VARI DCTRAN
DCTRAN*Local Median is
added to the highpass output.
.LEVEL2
.VARI INP
STRING - INP=A where A is the input dataset name.
.VARI OUT
STRING - OUT=B where B is the output dataset name.
.VARI SIZE
4 INTEGERS - SIZE=(SL,SS,NL,NS) where SL is the starting line, SS is the
 starting sample, NL is the number of lines output and NS is the number of 
 samples output. (SIZE is usually defined as SIZE=(1,1,NL,NS)). Default is 
 taken from the VICAR label within the program.
.VARI NL
INTEGER - NL=N1 where is N1 is the number of lines output.
.VARI NS
INTEGER - NS=N1 where is N1 is the number of samples output.
.VARI NLW
INTEGER - NLW=I1 where I1 is an integer and specifies the size of the filter
 kernel in lines. NLW should be an odd integer; if it is not, the program
 uses the next larger odd integer.  Default is NLW=11. 
.VARI NSW
INTEGER - NSW=I2 where I2 is an integer and specifies the size of the filter
 kernel in samples. NSW should be an odd integer; if it is not, the program
 uses the next larger odd integer.  Default is NSW=11.
.VARI HIGHPASS
KEYWORD - Valid:('HIGH) 'HIGHPASS specifies that the output is to be in
 highpass format, i.e., the input minus the local median value. Default
 is the lowpass mode.
.VARI DCLEVEL
DCLEVEL is a constant added to all pixels prior to output, when in the 
HIGHPASS mode.  It is most commonly used to rescale byte data back into
the 0 to 255 range.
.VARI PERCENT
REAL - PERCENT=R4 where R4 is an floating point number and specifies the
 percentage of the filter window values which are less than or equal to the
 output value.  The default of PERCENT=50.0 specifies that the true median 
 value of filter window is to be found, whereas if a lower (or higher)
 value is specified, the output value will be somewhat lower (or higher)
 than the actual median.
.VARI DCTRAN
DCTRAN is a scaling factor, used in the HIGHPASS mode, when the user desires
to add back part of the input image to the output. In the HIGHPASS mode

      OUTPUT  =   INPUT - MEDIAN + (DCTRAN*INPUT) + DCLEVEL
.END
$ Return
$!#############################################################################
$Imake_File:
$ create median.imake
#define  PROGRAM   median

#define MODULE_LIST median.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
