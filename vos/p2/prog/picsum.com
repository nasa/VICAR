$!****************************************************************************
$!
$! Build proc for MIPL module picsum
$! VPACK Version 1.9, Tuesday, February 15, 2011, 11:57:24
$!
$! Execute by entering:		$ @picsum
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
$!   TEST        Only the test files are created.
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
$ write sys$output "*** module picsum ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Test = ""
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
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to picsum.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
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
$   if F$SEARCH("picsum.imake") .nes. ""
$   then
$      vimake picsum
$      purge picsum.bld
$   else
$      if F$SEARCH("picsum.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake picsum
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @picsum.bld "STD"
$   else
$      @picsum.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create picsum.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack picsum.com -mixed -
	-s picsum.f -
	-i picsum.imake -
	-p picsum.pdf -
	-t tstpicsum.pdf tstpicsum.log_solos
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create picsum.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
C Radiometric Calibration Program PICSUM -- Adds multiple frames together.
C        PICSUM  (I1,I2,I3,...,In)  OUT  user-parameters...
C Inputs may be byte or halfword.
C
C Cassini data is 12 bits and so the range for the despike routine is 0 - 4095.

      SUBROUTINE MAIN44

         INTEGER*2 LTBL(0:4095),HTBL(0:4095), IN(32768)
         INTEGER OUNI,IUNI(30),IUNIT,XVPTST,ASCALE,MFLAG,PICSCALE,TFLAG
         INTEGER BUF(30), EXPOS, EXPOS1, SCLK, 
     &           SCLKS(30), TNL, TNS

         REAL*4 PAR(2),LSCALE

         CHARACTER*100 FN, FNAME
         CHARACTER*8 FMT, TFMT
         CHARACTER*5 PROJECT
         CHARACTER*5 CAMERA, CAMERA1, FILT1, FILT11, FILT2, FILT21
         CHARACTER*5 GAIN, GAIN1
         CHARACTER*4 MODE, MODE1

         CALL IFMESSAGE ('PICSUM version 14 Feb 2011')

         CALL XVPCNT('INP',NI)	!Number of input datasets

C        Open all inputs and print out picture labels...

         CALL XVP('LIST',FNAME,NJ)		! use a search list or INP

         IF ((NI .NE. 1) .AND. (NJ .NE. 0)) THEN
            CALL XVMESSAGE('Give only one INP if LIST is given',' ')
            CALL ABEND()
         ENDIF

         IF (NI .NE. 1) THEN 
            DO I=1,NI
               CALL XVUNIT(IUNI(I),'INP',I,IND,' ')
               CALL XVOPEN (IUNI(I),IND,'OPEN_ACT','SA','IO_ACT','SA',
     &                      'U_FORMAT','HALF',' ')
               IF (I .EQ. 1) 
     &            CALL GETPROJ(IUNI(I),PROJECT,ICAM,IFRM,ISTAT)
               IF (PROJECT .EQ. 'CASSI') THEN
                  CALL CASSIPROC(IUNI(I),ISTAT,SGC,MINT,CAMERA,GAIN,
     &                       MODE,EXPOS,FILT1,FILT2,SCLK)
                  IF (I .EQ. 1) THEN
                     CAMERA1 = CAMERA
                     GAIN1 = GAIN
                     MODE1 = MODE
                     EXPOS1 = EXPOS
                     FILT11 = FILT1
                     FILT21 = FILT2
                  ELSEIF ((CAMERA1 .NE. CAMERA) .OR.
     &                    (GAIN1 .NE. GAIN) .OR. 
     &                    (MODE1 .NE. MODE) .OR. 
     &                    (EXPOS1 .NE. EXPOS) .OR. 
     &                    (FILT11 .NE. FILT1) .OR. 
     &                    (FILT21 .NE. FILT2)) THEN
                     CALL XVMESSAGE 
     & ('CAMERA, GAIN, MODE, EXPOS, FILT1, FILT2 must be the same',' ')
                     CALL ABEND()
                  ENDIF
                  SCLKS(I) = SCLK
               ELSE
                  CALL LABPROC(IUNI(I),PROJECT,ISTAT,sgc,mint)
               ENDIF
            ENDDO
         ELSEIF (NJ .NE. 0) THEN		!open search list

C Read INP just to get the VICAR label put on the output
            CALL XVUNIT(IUNIT,'INP',1,IND,' ')
            CALL XVOPEN(IUNIT,IND,'OPEN_ACT','SA','IO_ACT','SA',
     &                'U_FORMAT','HALF',' ')
            OPEN(UNIT=99,FILE=FNAME,STATUS='OLD',ERR=950)
            READ(99,FMT=1) FN                     !SKIP FIRST LINE
1	    FORMAT(A)
	
	    DO I=1,31				!open files in list
	       READ(99,FMT=1,END=11) FN
	       IF (I .GE. 31) GO TO 920
	       NI = I
	       CALL XVUNIT(IUNI(I),'NONE',I,IST,'U_NAME',FN,' ')
               CALL XVOPEN(IUNI(I),IST,'OPEN_ACT','SA','IO_ACT','SA',
     &                'U_FORMAT','HALF',' ')
               IF (I .EQ. 1) 
     &            CALL GETPROJ(IUNI(I),PROJECT,ICAM,IFRM,ISTAT)
               IF (PROJECT .EQ. 'CASSI') THEN
                  CALL CASSIPROC(IUNI(I),ISTAT,SGC,MINT,CAMERA,GAIN,
     &                       MODE,EXPOS,FILT1,FILT2,SCLK)
                  IF (I .EQ. 1) THEN
                     CAMERA1 = CAMERA
                     GAIN1 = GAIN
                     MODE1 = MODE
                     EXPOS1 = EXPOS
                     FILT11 = FILT1
                     FILT21 = FILT2
                  ELSEIF (CAMERA1.NE.CAMERA .OR. GAIN1.NE.GAIN .OR.
     &                    MODE1.NE.MODE .OR. EXPOS1.NE.EXPOS .OR.
     &                    FILT11.NE.FILT1 .OR. FILT21.NE.FILT2) THEN
                     CALL XVMESSAGE
     &  ('CAMERA, GAIN, MODE, EXPOS, FILT1, FILT2 must be the same',' ')
                     CALL ABEND()
                  ENDIF
                  SCLKS(I) = SCLK
               ELSE
                  CALL LABPROC(IUNI(I),PROJECT,ISTAT,sgc,mint)
               ENDIF
	    ENDDO
11	    CONTINUE
         ELSE
            CALL XVMESSAGE 
     &         ('Specify at least two INPs or one INP with LIST',' ')
            CALL ABEND
         END IF

C        Get size and format of first input image NLxNS....
         CALL XVGET(IUNI(1),IND,'FORMAT',FMT,'NL',NL,'NS',NS,' ')
         IF ((FMT .NE. 'BYTE') .AND. (FMT .NE. 'HALF')) THEN
            CALL XVMESSAGE('***Illegal input format',' ')
            GOTO 999
         ENDIF
         DO I=1,NI-1
            CALL XVGET(IUNI(I),IND,'FORMAT',TFMT,'NL',TNL,'NS',TNS,' ')
            IF (TFMT.NE.FMT .OR. TNL.NE.NL .OR. TNS.NE.NS) THEN
               CALL XVMESSAGE
     &         ('***All inputs must be of the same size and format',' ')
               GOTO 999
            ENDIF
         ENDDO

         ASCALE = XVPTST('ASCALE')		!Auto picture scale flag
         IF (ASCALE.EQ.1) THEN
            XSCALE = 128./NI
            PICSCALE = 128
         ELSE
            PICSCALE = NI
         ENDIF

         TFLAG = 0				!Threshold flag for despiking
         CALL XVPARM('TSCALE',PAR,ICNT,IDEF,2)
         IF (IDEF.EQ.0) THEN
            TFLAG = 1			!Turn flag on
            LSCALE = PAR(1)/SQRT(SGC)
            HSCALE = PAR(2)/SQRT(SGC)
            CALL XVPARM('MINT',ival,icnt,idef,1)
            IF (IDEF.NE.1) MINT=IVAL
            CALL PRNT(4,1,MINT,'MINT=.')

            DO I=0,4095
               DN = SQRT(FLOAT(I))
               LTBL(I) = LSCALE*DN
               HTBL(I) = HSCALE*DN
               IF (LTBL(I) .LT. MINT) LTBL(I)=MINT
               IF (HTBL(I) .LT. MINT) HTBL(I)=MINT
            ENDDO
         ENDIF

         MFLAG = .FALSE.
         IF (TFLAG.EQ.0) MFLAG = XVPTST('MEDIAN')		!Median filter flag

         CALL XVUNIT(OUNI,'OUT',1,IND,' ')
         CALL XVOPEN(OUNI,IND,'OPEN_ACT','SA','IO_ACT','SA',
     &         'OP','WRITE','U_NL',NL,'U_NS',NS,'O_FORMAT','HALF',' ')
         CALL XLADD
     &      (OUNI,'HISTORY','PICSCALE',PICSCALE,IND,'FORMAT','INT',' ')
         CALL XLADD(OUNI,'HISTORY','NFRAMES',NI,IND,'FORMAT','INT',' ')
         IF (PROJECT .EQ. 'CASSI') THEN
            CALL SORTIN(SCLKS,NI)
            CALL XLADD(OUNI,'HISTORY','SCLKS',SCLKS,IND,'FORMAT','INT',
     &              'NELEMENT',NI,' ')
         ENDIF
         CALL SUMIT(IUNI,OUNI,IN,IN,BUF,NL,NS,NI,TFLAG,
     &      ASCALE,XSCALE,LTBL,HTBL,MFLAG)
         CALL XVCLOSE(OUNI,IND,' ')
         CALL XVMESSAGE('PICSUM task completed',' ')
         RETURN
CCCCCCCCC
C
920      CALL XVMESSAGE('more than 30 filenames in LIST',' ')
         GO TO 999
950      CALL XVMESSAGE('could not open input list file',' ')
         CALL XVMESSAGE(FNAME,' ')
999      CALL XVMESSAGE('***PICSUM task cancelled',' ')
         END



C Perform the pixel summation (or filtering)
C
      SUBROUTINE SUMIT(IUNI,OUNI,IN,OBUF,BUF,NL,NS,NI,
     &	TFLAG,ASCALE,XSCALE,LTBL,HTBL,MFLAG)

         INTEGER IUNI(NI),OUNI,TFLAG,ASCALE,MFLAG
         INTEGER*2 IN(NS,NI),OBUF(NS),LTBL(0:4095),HTBL(0:4095)
         INTEGER*4 BUF(NI)

         NBAD = 0

         DO 100 L=1,NL
            DO I=1,NI
               CALL XVREAD(IUNI(I),IN(1,I),IND,' ')
            ENDDO

            IF (TFLAG .EQ. 1) THEN    !If thresholds are specified,
               DO I=1,NS
                  NP = 0
                  DO J=1,NI
                     IDN = IN(I,J)
                     IF (IDN.GT.0) THEN
                        NP = NP + 1
                        BUF(NP) = IDN
                     ENDIF
                  ENDDO
                  CALL DESPIKE(BUF,NI,NP,N,IDN,LTBL,HTBL)  !despike data.
                  NBAD = NBAD + (NI-N)	   !Number of bad samples
                  OBUF(I) = MIN0(IDN,32767)
               ENDDO
            ELSEIF (MFLAG .EQ. 1) THEN    !If produce a median file.
               DO I=1,NS
                  NP = 0
                  DO J=1,NI
                     IDN = IN(I,J)
C                     IF (IDN.GT.0) THEN
                        NP = NP + 1
                        BUF(NP) = IDN
C                     ENDIF
                  ENDDO
                  CALL GETMED(BUF,NI,NP,N,IDN,LTBL,HTBL)  !return median
C                  NBAD = NBAD + (NI-N)	   !Number of bad samples
                  OBUF(I) = IDN
               ENDDO
            ELSE
               DO I=1,NS	    !Else, just add lines together
                  IDN = IN(I,1)
                  DO J=2,NI
                     IDN = IDN + IN(I,J)
                  ENDDO
                  OBUF(I) = MIN0(IDN,32767)
               ENDDO
            ENDIF

            IF (ASCALE .EQ. 1) THEN
               DO I=1,NS
                  IDN = OBUF(I)*XSCALE
                  OBUF(I) = MIN0(IDN,32767)
               ENDDO
            ENDIF
  100    CALL XVWRIT(OUNI,OBUF,IND,' ')

         IF (TFLAG.EQ.1) 
     &      CALL PRNT(4,1,NBAD,'Number of bad pixels=.')
         RETURN
      END



C Scan for noise spikes
C
      SUBROUTINE DESPIKE(BUF,NI,NP,N,IDN,LTBL,HTBL)

         INTEGER BUF(NI)

         INTEGER*2 LTBL(0:4095),HTBL(0:4095)
         INTEGER HTHRESH

         IF (NP.EQ.0) THEN		!If all samples are zero
            N = 0
            IDN = 0		!return zero
            RETURN
         ELSEIF (NP.EQ.1) THEN	!If only one non-zero sample
            N = 1
            IDN = BUF(1)*NI	!then no screening necessary
            RETURN
         ENDIF

C     ....Here if at least two non-zero samples are in BUF
         CALL SORTIN(BUF,NP)	!Sort the samples
         MIX = (NP+1)/2		!Index of median
         MEDIAN = BUF(MIX)		!Median DN value
         ISUM = MEDIAN		!Initialize sum
         N = 1			!Number of points in sum

C     ....Add all samples less than median
         IEND = MIX - 1
         IF (IEND.EQ.0) GOTO 20	!Skip if BUF(1) is median
         LTHRESH = LTBL(MEDIAN)	!Lower threshold

         DO I=1,IEND
            IDN = BUF(I)
            IF (MEDIAN-IDN.LE.LTHRESH) THEN
               N = N + 1
               ISUM = ISUM + IDN
            ENDIF
         ENDDO

C     ....Add all samples greater than median
   20    IBEG = MIX + 1
         HTHRESH = HTBL(MEDIAN)	!Upper threshold

         DO I=IBEG,NP
            IDN = BUF(I)
            IF (IDN-MEDIAN.LE.HTHRESH) THEN
               N = N + 1
               ISUM = ISUM + IDN
            ENDIF
         ENDDO

         IF (N.LT.NI) THEN
            IDN = (FLOAT(NI)*ISUM)/N + 0.5
         ELSE
            IDN = ISUM
         ENDIF

         RETURN
      END

C Just use the median value in the buffer
C
      SUBROUTINE GETMED(BUF,NI,NP,N,IDN,LTBL,HTBL)

         INTEGER BUF(NI)

         INTEGER*2 LTBL(0:4095),HTBL(0:4095)
         INTEGER HTHRESH

         IF (NP.EQ.0) THEN		!If all samples are zero
            N = 0
            IDN = 0		!return zero
            RETURN
         ELSEIF (NP.EQ.1) THEN	!If only one non-zero sample
            N = 1
            IDN = BUF(1)	!then no screening necessary
            RETURN
         ENDIF

C     ....Here if at least two non-zero samples are in BUF
         CALL SORTIN(BUF,NP)	!Sort the samples
         MIX = (NP+1)/2		!Index of median
         MEDIAN = BUF(MIX)		!Median DN value
         IDN = MEDIAN		!Initialize sum

         RETURN
      END



C Print input label
C
      SUBROUTINE LABPROC(IUNI,PROJECT,ISTATUS,SGC,MINT)

         INTEGER LBUF(80)

         CHARACTER*5 PROJECT
         CHARACTER*132 MSG

C     ...Galileo gain ratios for 400K  100K  40K  10K
C
         REAL*4 GLL_GAIN_RATIO(4)
         DATA GLL_GAIN_RATIO/47.091,9.809,4.799,1.0/

         INTEGER MINTHRESH(4)  !minimum threshold (DN)
         DATA MINTHRESH/1,1,1,2/

         REAL*4 GLL_GAIN_CONSTANT !electrons/DN at 10K
         DATA GLL_GAIN_CONSTANT/42.3/ 
 
         EQUIVALENCE (EXPO,IEXPO)


  101    FORMAT(' CAMERA=',I4,' FRAME=',I9,' FILTER=',I1,' GAIN=',I1,
     &    ' EXP=',F8.1,' RATE=',I3)

         IF (ISTATUS.NE.1) THEN
            CALL GETLABCON(IUNI,PROJECT,lbuf,ind)
            ICAM = LBUF(6)
            IFRAME = LBUF(2)
            IFILT = LBUF(4)
            IGAIN = LBUF(7)
            IEXPO = LBUF(3)
            IRATE = LBUF(5)
            WRITE(MSG,101,ERR=10) ICAM,IFRAME,IFILT,IGAIN,EXPO,IRATE
   10       CALL XVMESSAGE (MSG,' ')
         ENDIF

         IF (PROJECT.EQ.'GLL') THEN
            SGC = GLL_GAIN_CONSTANT*GLL_GAIN_RATIO(IGAIN)
            MINT = MINTHRESH(IGAIN)
            IF (MOD(LBUF(17),10).EQ.1)
     &         CALL XVMESSAGE ('EXTENDED-EXPOSURE',' ')
         ELSE
            SGC = 1.
            MINT = 3
         ENDIF
         RETURN
      END



C
C Check Cassini input label
C
      SUBROUTINE CASSIPROC(IUNI,ISTATUS,SGC,MINT,CAMERA,GAIN,MODE,
     &  EXPOS,FILT1,FILT2,SCLK)

         INCLUDE 'cas_isslab'
c         INCLUDE 'cas_isslab.fin'  ! remove before delivery

         INTEGER IND, EXPOS, SCLK

         CHARACTER*132 MSG
         CHARACTER*5 CAMERA, FILT1, FILT2, GAIN
         CHARACTER*4 MODE

C     ...Cassini gain ratios for 1400K  400K  100K  40K
C
         INTEGER MINTHRESH(4)     !minimum threshold (DN)
         DATA MINTHRESH/1,1,1,2/

c         REAL*4 CASSI_GAIN_RATIO(4)
c         DATA CASSI_GAIN_RATIO/17.1,7.9,2.4,1.0/

c         REAL*4 CASSI_GAIN_CONSTANT     !electrons/DN at 24K
c         DATA CASSI_GAIN_CONSTANT/12.6/

c     ...Use System Gain Constant as calculated by CISSCAL/DECAL
	 REAL*4 CASSI_SGC_NAC1(4), CASSI_SGC_NAC2(4)
	 REAL*4 CASSI_SGC_WAC1(4), CASSI_SGC_WAC2(4)
         DATA CASSI_SGC_NAC1/233.04, 98.88, 30.27, 12.85/
         DATA CASSI_SGC_NAC2/219.80, 99.32, 29.15, 13.93/
         DATA CASSI_SGC_WAC1/210.566, 85.09, 27.68, 11.85/
         DATA CASSI_SGC_WAC2/194.30, 90.13, 27.66, 11.74/

         IF (ISTATUS.EQ.1) THEN
            CALL XVMESSAGE
     &         ('Must have a correct Cassini Vicar label',' ')
            CALL ABEND()
         ELSE
            CALL ABLE97(IND,IUNI)
            SCLK = LAB_SCLK
            CAMERA = LAB_CAMERA
            GAIN = LAB_GAIN
            MODE=LAB_MODE
            EXPOS = LAB_EXPOS
            FILT1 = LAB_FILTER1
            FILT2 = LAB_FILTER2

            WRITE
     &        (MSG,201,ERR=20)SCLK,CAMERA,GAIN,MODE,EXPOS,FILT1,FILT2

  201       FORMAT('SCLK=',I9,' CAM=',A5,' GAIN=',A5,' MODE=',A4,
     &' EXP=',I7,' FILT1=',A5,' FILT2=',A5)

   20       CALL XVMESSAGE (MSG,' ')
            IF (LAB_GAIN .EQ. '1400K'
     &          .OR. LAB_GAIN(1:3) .EQ. '215') THEN
               IGAIN = 1
            ELSEIF (LAB_GAIN .EQ. '400K'
     &              .OR. LAB_GAIN(1:2) .EQ. '95') THEN
               IGAIN = 2
            ELSEIF (LAB_GAIN .EQ. '100K'
     &              .OR. LAB_GAIN(1:2) .EQ. '29') THEN 
               IGAIN = 3
            ELSE
               IGAIN = 4
            ENDIF
            IF (LAB_CAMERA .EQ. 'ISSNA') THEN
	       IF (LAB_OPTTEMP .LE. 15.) THEN
                  SGC = CASSI_SGC_NAC1(IGAIN)
               ELSE
                  SGC = CASSI_SGC_NAC2(IGAIN)
               ENDIF
            ELSE
	       IF (LAB_OPTTEMP .LE. 15.) THEN
                  SGC = CASSI_SGC_WAC1(IGAIN)
               ELSE
                  SGC = CASSI_SGC_WAC2(IGAIN)
               ENDIF
            ENDIF
c            SGC = CASSI_GAIN_CONSTANT*CASSI_GAIN_RATIO(IGAIN)
            MINT = MINTHRESH(IGAIN)
         ENDIF
         RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create picsum.imake
#define PROGRAM picsum

#define MODULE_LIST picsum.f

#define FTNINC_LIST cas_isslab

#define MAIN_LANG_FORTRAN
#define R2LIB

#define USES_FORTRAN

/* #define LIB_LOCAL /* remove before delivery */
#define LIB_P2SUB
#define LIB_RTL
#define LIB_TAE
$ Return
$!#############################################################################
$PDF_File:
$ create picsum.pdf
process help=*
PARM INP    TYPE=STRING        COUNT=(1:30)
PARM OUT    TYPE=STRING        COUNT=1
PARM ASCALE TYPE=KEYWORD       COUNT=(0:1) VALID=ASCALE	  DEFAULT=--
PARM TSCALE TYPE=REAL          COUNT=(0:2) 		  DEFAULT=--
PARM MINT   TYPE=INTEGER       COUNT=(0:1) VALID=(0:4095) DEFAULT=3
PARM MEDIAN TYPE=KEYWORD       COUNT=(0:1) VALID=MEDIAN	  DEFAULT=--
PARM LIST   TYPE=STRING        COUNT=0:1                  DEFAULT=--
!# parm inp(2-30) hints=default
END-PROC
.TITLE
VICAR Program PICSUM
.HELP
PURPOSE:

PICSUM will add up to 30 byte or halfword images together. The input samples
may be optionally screened for high and/or low noise spikes.  PICSUM was
originally written to support Galileo camera calibration.  

The option to do a median filter instead of a summation is also supported.

Reference: D-4264  MIPL Software Structural Design for the Instrument
 Calibration of GLL SSI Science Processing.

EXECUTION:

    PICSUM  (I1,I2,I3,...,In)  OUT  user-parameters...

The input images may be byte or halfword format.  The output image is in
halfword format.

All input images must be the same size and format.  The product of the number
of samples on each line (NS) and the number of input images (NI) must be
less than 32768.  For example, if 30 images are input, then the number of
samples must be less than 1092.

For Cassini images, camera, gain, mode, expos, filt1, and filt2 must be the
same for all images.

The input images can be given using a SRCH list (LIST).  However, you must
give one (and only one) file with INP if you use LIST.  That one file should
be the first image listed in your SRCH list.  That one file given with INP
is used only for its VICAR label which will be used for the output image (OUT).

.page
DESPIKE ALGORITHM:

The despike algorithm is invoked by specifying the TSCALE parameter.  The
algorithm identifies noise spikes (e.g. transmission or radiation noise).

For a given pixel location, the DN value from each input image is retrieved and
the median DN value determined (Note that when only two images are input, the
median is always the lower DN).  The DN values from each image is then
compared with the median and all pixels differing from the median by more
than a low and high threshold are rejected.  Also, any input sample less than
or equal to 0 DN is rejected as invalid (assumed to be a data drop-out).

For a given pixel, low and high thresholds are computed as follows:
	LTHRESH = lscale*SQRT(median)/SQRT(C)
	HTHRESH = hscale*SQRT(median)/SQRT(C)
where C is the camera system gain constant in electrons/DN and lscale and
hscale are specified via the TSCALE parameter.  For Galileo images, the system
gain constant is 42.3 for 10K gain-state, 203.0 for 40K, 414.9 for 100K, and
1991.9 for 400K.  For Cassini images, the system gain constant is 42.3 for 24K 
gain-state, 203.0 for 100K, 414.9 for 400K, and 1991.9 for 1400K. 

(**NOTE THIS WILL CHANGE ONCE THE ACTUAL VALUES ARE DETERMINED)

For non-Galileo and non_Cassini images, the system gain constant is assumed
to be 1.0.

The computed thresholds (LTHRESH and HTHRESH) are not allowed to be smaller
than a minimum threshold, as specified by the MINT parameter.  If defaulted,
MINT is assigned the following values:  1 DN for 400K gain-state, 1 DN for
100K, 1 DN for 40K, and 2 DN for 10K.  For non-Galileo and non_Cassini images,
the default is MINT=3.

(**NOTE THIS MAY CHANGE FOR CASSINI)

After all bad samples are eliminated, the remaining samples are summed and the
result scaled appropriately to compensate for the discarded samples.  If all
samples are bad (only true if all data is less than 1 DN) then 0 DN is stored.

.page
OUTPUT PICTURE SCALE

If the ASCALE keyword is specified, the output pixels will be scaled by
the factor 128/N, where N is the number of input frames.

If the output DN value at any sample position exceeds 32767, then it is set
equal to 32767.

The output picture scale (either N or 128/N) and number of input frames
are recorded in the output picture label (label items PICSCALE and NFRAMES).

If the input are Cassini images, SCLKS is also put in the output VICAR
label.  SCLKS are sorted and listed in ascending order.

.page
EXAMPLES

1) PICSUM  (IN1,IN2,IN3,IN4,IN5)  OUT  TSCALE=(3.,3.)

   Five input images are added together to form the output image.  If all
   the input images are in byte format, the despike algorithm is invoked.
   The specified TSCALE values will cause any sample differing from the
   median by more than a 3 sigma shot-noise level to be ignored.

   Suppose that for a given pixel location, the input sample values are
   9,10,12,12,13.  The median is 12.  The computed low and high thresholds
   are 2.  The sample value 9 is discarded and the resulting sum is 47.
   This sum is multiplied by 5/4 to account for the discarded sample.

2) PICSUM IN1 OUT LIST=SRCH.LIST

where SRCH.LIST looks like

NEXT FILE = 00001
IN1
IN2
IN3
IN4


.page
PROGRAM HISTORY

ORIGINAL PROGRAMMER: Gary Yagi, circa 1982
CURRENT COGNIZANT PROGRAMMER: Lucas Kamp
HISTORY:
  14 Feb 2011 LWK/HBM added median filter option
  06 June  97 TXH Ported from VAX to Unix and VAX/VMS to support Cassini
  15 May   96 CCA Added first estimate of Cassini gain ratios
  08 Dec   94 JRY Change the despike range from 0:255 to 0:4095 for Cassini.
  12 July  94 JRY For Cassini images, sclks put in VICAR label and LIST added
  28 April 94 JRY Modified to work with Cassini images
  28 April 91 GMY Make despike threshold gain-state dependent
  06 April 91 GMY Add despike algorithm
  04 Nov   87 GMY Added ASCALE parameter
  16 Jan   87 GMY Code and documentation clean-up
  25 SEPT  84 MEM CONVERSION TO VICAR*2
  16 MARCH 84 MEM CONVERSION TO VAX/VICAR-1
           82 GMY INITIAL VERSION
.LEVEL1
.VARIABLE INP
 STRING--REQUIRED
 From 1 to 30 input
 images (byte or half)
.VARIABLE OUT
 STRING--REQUIRED
 Output image file
 name (halfword)
.VARIABLE ASCALE
 KEYWORD--OPTIONAL
 Causes each output
 pixel to be scaled
 by 128/#inputs.
.VARIABLE MEDIAN
 KEYWORD--OPTIONAL
 Compute median as 
 the output.
.VARI TSCALE
 REAL--OPTIONAL
 Low and high thresholds
 for identifying spikes
.VARI MINT
 INTEGER--OPTIONAL
 Minimum threshold value
.VARI LIST
STRING--OPTIONAL
SRCH list containing the
input images
.LEVEL2
.VARIABLE INP
 STRING--REQUIRED
 From 1 to 30 input images.  The images may be byte or halfword format.
 However, all inputs must be of the same format.  If only 1 image is given,
 that 1 image is used only for its VICAR label, and the images that are
 PICSUM'd must be given using LIST.
.VARIABLE OUT
 STRING--REQUIRED
 Output image file name (halfword)
.VARIABLE ASCALE
 KEYWORD--OPTIONAL
 Causes each output pixel to be scaled by 128/#inputs.
.VARI TSCALE
 REAL--OPTIONAL
 Low and high thresholds for identifying spikes.  See HELP PICSUM for details.
.VARI MINT
 INTEGER--OPTIONAL
 Minimum threshold value.  See HELP PICSUM for details.
.VARIABLE MEDIAN
 KEYWORD--OPTIONAL
 Invokes a median filter on the input files instead of a summation.
 This keyword is ignored if TSCALE has been specified.
.VARI LIST
 STRING--OPTIONAL
 The input images can be given either with INP or with a SRCH list (LIST).  If
 LIST is used, you must still give one image with INP which is used only for
 its VICAR label which is used for the output image.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstpicsum.pdf
procedure
refgbl $echo
refgbl $autousage
refgbl $syschar
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"

local dir string

! Test of picsum using gen
gen out=pic1.img nl=10 ns=10 ival=10 linc=4 sinc=6
list pic1.img

gen out=pic2.img nl=10 ns=10 ival=10 linc=6 sinc=4
list pic2.img

picsum inp=(pic1.img,pic2.img) out=sum.img
list sum.img

picsum inp=(pic1.img,pic2.img) out=sum.img tscale=(1.,1.)
list sum.img

picsum inp=(pic1.img,pic2.img) out=sum.img 'ascale
list sum.img

picsum inp=(pic1.img,pic2.img) out=sum.img 'median
list sum.img

! make some more images for median filtering:
gen out=pic3.img nl=10 ns=10 ival=10 linc=8 sinc=2
list pic3.img
gen out=pic4.img nl=10 ns=10 ival=10 linc=1 sinc=5
list pic4.img
gen out=pic5.img nl=10 ns=10 ival=10 linc=9 sinc=7
list pic5.img
picsum inp=(pic1.img,pic2.img,pic3.img,pic4.img,pic5.img) out=sum.img 'median
list sum.img


!!Testing with DN values close/exceed 32767
gen out=pic1.img nl=10 ns=10 ival=32764 'half
list pic1.img

gen out=pic2.img nl=10 ns=10 'half
list pic2.img

picsum inp=(pic1.img, pic2.img) out=picx.img
list picx.img


!!Testing with one HALF and one BYTE input
gen out=pic1.img nl=10 ns=10 ival=32764 'half
list pic1.img

gen out=pic2.img nl=10 ns=10
list pic2.img

picsum inp=(pic1.img, pic2.img) out=picx.img
picsum inp=(pic2.img, pic1.img) out=picz.img
difpic (picx.img, picz.img)
list picx.img


!! Test of Cassini ground calibration images
if ($syschar(1)="UNIX")
   let dir = "/project/test_work/testdata/cassini/iss/"
   ush cp /project/test_work/testdata/cassini/iss/sum2.1 .
   ush cp /project/test_work/testdata/cassini/iss/sum2.2 .
   ush cp /project/test_work/testdata/cassini/iss/sum2.3 .
else
   let dir = "wms_test_work:[testdata.cassini.iss]"
   dcl copy wms_test_work:[testdata.cassini.iss]sum2.1 *
   dcl copy wms_test_work:[testdata.cassini.iss]sum2.2 *
   dcl copy wms_test_work:[testdata.cassini.iss]sum2.3 *
end-if

picsum (&"dir"sum2.1 &"dir"sum2.3 &"dir"sum2.2) out.img
hist out.img 'nohist
label-l out.img

!!Testing the LIST parameter.  This should generate the same output as the 
!!above test case.
!Create list of the files
createfile picsum.srchlist
addtofile picsum.srchlist "NEXT FILE=0001"
addtofile picsum.srchlist "sum2.1"
addtofile picsum.srchlist "sum2.3"
addtofile picsum.srchlist "sum2.2"

picsum &"dir"sum2.1 out.img list=picsum.srchlist
hist out.img 'nohist
label-l out.img

if ($syschar(1)="UNIX")
   ush rm sum2.*
else
   dcl del sum2.*;*
end-if


write "The output DN values should be (sum of 3 DNs * 128.0/3.0)."
picsum (&"dir"sum2.1 &"dir"sum2.2 &"dir"sum2.3) out.img 'ascale
list &"dir"sum2.1 sl=10 ss=10 nl=100 ns=100 linc=10 sinc=10
list &"dir"sum2.2 sl=10 ss=10 nl=100 ns=100 linc=10 sinc=10
list &"dir"sum2.3 sl=10 ss=10 nl=100 ns=100 linc=10 sinc=10
list out.img sl=10 ss=10 nl=100 ns=100 linc=10 sinc=10
hist out.img 'nohist

picsum (&"dir"sum2.1 &"dir"sum2.2 &"dir"sum2.3) out.img tscale=(3.,3.)
hist out.img 'nohist

picsum (&"dir"sum2.1 &"dir"sum2.2 &"dir"sum2.3) out.img tscale=(3.,3.) mint=5
hist out.img 'nohist

!! Will not run, different mode and expos
!picsum (&"dir"sum2.1 &"dir"ubw_1.byte) out.img

end-proc
$!-----------------------------------------------------------------------------
$ create tstpicsum.log_solos
tstpicsum
local dir string
gen out=pic1.img nl=10 ns=10 ival=10 linc=4 sinc=6
Beginning VICAR task gen
GEN Version 6
GEN task completed
list pic1.img
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Mon Feb 14 18:11:21 2011
     Samp     1       3       5       7       9
   Line
      1      10  16  22  28  34  40  46  52  58  64
      2      14  20  26  32  38  44  50  56  62  68
      3      18  24  30  36  42  48  54  60  66  72
      4      22  28  34  40  46  52  58  64  70  76
      5      26  32  38  44  50  56  62  68  74  80
      6      30  36  42  48  54  60  66  72  78  84
      7      34  40  46  52  58  64  70  76  82  88
      8      38  44  50  56  62  68  74  80  86  92
      9      42  48  54  60  66  72  78  84  90  96
     10      46  52  58  64  70  76  82  88  94 100
gen out=pic2.img nl=10 ns=10 ival=10 linc=6 sinc=4
Beginning VICAR task gen
GEN Version 6
GEN task completed
list pic2.img
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Mon Feb 14 18:11:21 2011
     Samp     1       3       5       7       9
   Line
      1      10  14  18  22  26  30  34  38  42  46
      2      16  20  24  28  32  36  40  44  48  52
      3      22  26  30  34  38  42  46  50  54  58
      4      28  32  36  40  44  48  52  56  60  64
      5      34  38  42  46  50  54  58  62  66  70
      6      40  44  48  52  56  60  64  68  72  76
      7      46  50  54  58  62  66  70  74  78  82
      8      52  56  60  64  68  72  76  80  84  88
      9      58  62  66  70  74  78  82  86  90  94
     10      64  68  72  76  80  84  88  92  96 100
picsum inp=(pic1.img,pic2.img) out=sum.img
Beginning VICAR task picsum
PICSUM version 14 Feb 2011
GETPROJ: unrecognizable project
PICSUM task completed
list sum.img
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Mon Feb 14 18:11:21 2011
 Task:PICSUM    User:lwk       Date_Time:Mon Feb 14 18:11:21 2011
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1        20    30    40    50    60    70    80    90   100   110
      2        30    40    50    60    70    80    90   100   110   120
      3        40    50    60    70    80    90   100   110   120   130
      4        50    60    70    80    90   100   110   120   130   140
      5        60    70    80    90   100   110   120   130   140   150
      6        70    80    90   100   110   120   130   140   150   160
      7        80    90   100   110   120   130   140   150   160   170
      8        90   100   110   120   130   140   150   160   170   180
      9       100   110   120   130   140   150   160   170   180   190
     10       110   120   130   140   150   160   170   180   190   200
picsum inp=(pic1.img,pic2.img) out=sum.img tscale=(1.,1.)
Beginning VICAR task picsum
PICSUM version 14 Feb 2011
GETPROJ: unrecognizable project
MINT=          3
Number of bad pixels=         42
PICSUM task completed
list sum.img
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Mon Feb 14 18:11:21 2011
 Task:PICSUM    User:lwk       Date_Time:Mon Feb 14 18:11:22 2011
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1        20    30    40    44    52    60    68    76    84    92
      2        30    40    50    60    64    72    80    88    96   104
      3        40    50    60    70    80    90    92   100   108   116
      4        44    60    70    80    90   100   110   112   120   128
      5        52    64    80    90   100   110   120   130   140   140
      6        60    72    90   100   110   120   130   140   150   160
      7        68    80    92   110   120   130   140   150   160   170
      8        76    88   100   112   130   140   150   160   170   180
      9        84    96   108   120   140   150   160   170   180   190
     10        92   104   116   128   140   160   170   180   190   200
picsum inp=(pic1.img,pic2.img) out=sum.img 'ascale
Beginning VICAR task picsum
PICSUM version 14 Feb 2011
GETPROJ: unrecognizable project
PICSUM task completed
list sum.img
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Mon Feb 14 18:11:21 2011
 Task:PICSUM    User:lwk       Date_Time:Mon Feb 14 18:11:22 2011
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1      1280  1920  2560  3200  3840  4480  5120  5760  6400  7040
      2      1920  2560  3200  3840  4480  5120  5760  6400  7040  7680
      3      2560  3200  3840  4480  5120  5760  6400  7040  7680  8320
      4      3200  3840  4480  5120  5760  6400  7040  7680  8320  8960
      5      3840  4480  5120  5760  6400  7040  7680  8320  8960  9600
      6      4480  5120  5760  6400  7040  7680  8320  8960  9600 10240
      7      5120  5760  6400  7040  7680  8320  8960  9600 10240 10880
      8      5760  6400  7040  7680  8320  8960  9600 10240 10880 11520
      9      6400  7040  7680  8320  8960  9600 10240 10880 11520 12160
     10      7040  7680  8320  8960  9600 10240 10880 11520 12160 12800
picsum inp=(pic1.img,pic2.img) out=sum.img 'median
Beginning VICAR task picsum
PICSUM version 14 Feb 2011
GETPROJ: unrecognizable project
PICSUM task completed
list sum.img
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Mon Feb 14 18:11:21 2011
 Task:PICSUM    User:lwk       Date_Time:Mon Feb 14 18:11:22 2011
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1        10    14    18    22    26    30    34    38    42    46
      2        14    20    24    28    32    36    40    44    48    52
      3        18    24    30    34    38    42    46    50    54    58
      4        22    28    34    40    44    48    52    56    60    64
      5        26    32    38    44    50    54    58    62    66    70
      6        30    36    42    48    54    60    64    68    72    76
      7        34    40    46    52    58    64    70    74    78    82
      8        38    44    50    56    62    68    74    80    84    88
      9        42    48    54    60    66    72    78    84    90    94
     10        46    52    58    64    70    76    82    88    94   100
gen out=pic3.img nl=10 ns=10 ival=10 linc=8 sinc=2
Beginning VICAR task gen
GEN Version 6
GEN task completed
list pic3.img
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Mon Feb 14 18:11:22 2011
     Samp     1       3       5       7       9
   Line
      1      10  12  14  16  18  20  22  24  26  28
      2      18  20  22  24  26  28  30  32  34  36
      3      26  28  30  32  34  36  38  40  42  44
      4      34  36  38  40  42  44  46  48  50  52
      5      42  44  46  48  50  52  54  56  58  60
      6      50  52  54  56  58  60  62  64  66  68
      7      58  60  62  64  66  68  70  72  74  76
      8      66  68  70  72  74  76  78  80  82  84
      9      74  76  78  80  82  84  86  88  90  92
     10      82  84  86  88  90  92  94  96  98 100
gen out=pic4.img nl=10 ns=10 ival=10 linc=1 sinc=5
Beginning VICAR task gen
GEN Version 6
GEN task completed
list pic4.img
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Mon Feb 14 18:11:22 2011
     Samp     1       3       5       7       9
   Line
      1      10  15  20  25  30  35  40  45  50  55
      2      11  16  21  26  31  36  41  46  51  56
      3      12  17  22  27  32  37  42  47  52  57
      4      13  18  23  28  33  38  43  48  53  58
      5      14  19  24  29  34  39  44  49  54  59
      6      15  20  25  30  35  40  45  50  55  60
      7      16  21  26  31  36  41  46  51  56  61
      8      17  22  27  32  37  42  47  52  57  62
      9      18  23  28  33  38  43  48  53  58  63
     10      19  24  29  34  39  44  49  54  59  64
gen out=pic5.img nl=10 ns=10 ival=10 linc=9 sinc=7
Beginning VICAR task gen
GEN Version 6
GEN task completed
list pic5.img
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Mon Feb 14 18:11:23 2011
     Samp     1       3       5       7       9
   Line
      1      10  17  24  31  38  45  52  59  66  73
      2      19  26  33  40  47  54  61  68  75  82
      3      28  35  42  49  56  63  70  77  84  91
      4      37  44  51  58  65  72  79  86  93 100
      5      46  53  60  67  74  81  88  95 102 109
      6      55  62  69  76  83  90  97 104 111 118
      7      64  71  78  85  92  99 106 113 120 127
      8      73  80  87  94 101 108 115 122 129 136
      9      82  89  96 103 110 117 124 131 138 145
     10      91  98 105 112 119 126 133 140 147 154
picsum inp=(pic1.img,pic2.img,pic3.img,pic4.img,pic5.img) out=sum.img 'median
Beginning VICAR task picsum
PICSUM version 14 Feb 2011
GETPROJ: unrecognizable project
PICSUM task completed
list sum.img
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Mon Feb 14 18:11:21 2011
 Task:PICSUM    User:lwk       Date_Time:Mon Feb 14 18:11:23 2011
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1        10    15    20    25    30    35    40    45    50    55
      2        16    20    24    28    32    36    41    46    51    56
      3        22    26    30    34    38    42    46    50    54    58
      4        28    32    36    40    44    48    52    56    60    64
      5        34    38    42    46    50    54    58    62    66    70
      6        40    44    48    52    56    60    64    68    72    76
      7        46    50    54    58    62    66    70    74    78    82
      8        52    56    60    64    68    72    76    80    84    88
      9        58    62    66    70    74    78    82    86    90    94
     10        64    68    72    76    80    84    88    92    96   100
gen out=pic1.img nl=10 ns=10 ival=32764 'half
Beginning VICAR task gen
GEN Version 6
GEN task completed
list pic1.img
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Mon Feb 14 18:11:23 2011
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1     32764 32765 32766 32767-32768-32767-32766-32765-32764-32763
      2     32765 32766 32767-32768-32767-32766-32765-32764-32763-32762
      3     32766 32767-32768-32767-32766-32765-32764-32763-32762-32761
      4     32767-32768-32767-32766-32765-32764-32763-32762-32761-32760
      5    -32768-32767-32766-32765-32764-32763-32762-32761-32760-32759
      6    -32767-32766-32765-32764-32763-32762-32761-32760-32759-32758
      7    -32766-32765-32764-32763-32762-32761-32760-32759-32758-32757
      8    -32765-32764-32763-32762-32761-32760-32759-32758-32757-32756
      9    -32764-32763-32762-32761-32760-32759-32758-32757-32756-32755
     10    -32763-32762-32761-32760-32759-32758-32757-32756-32755-32754
gen out=pic2.img nl=10 ns=10 'half
Beginning VICAR task gen
GEN Version 6
GEN task completed
list pic2.img
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Mon Feb 14 18:11:23 2011
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         0     1     2     3     4     5     6     7     8     9
      2         1     2     3     4     5     6     7     8     9    10
      3         2     3     4     5     6     7     8     9    10    11
      4         3     4     5     6     7     8     9    10    11    12
      5         4     5     6     7     8     9    10    11    12    13
      6         5     6     7     8     9    10    11    12    13    14
      7         6     7     8     9    10    11    12    13    14    15
      8         7     8     9    10    11    12    13    14    15    16
      9         8     9    10    11    12    13    14    15    16    17
     10         9    10    11    12    13    14    15    16    17    18
picsum inp=(pic1.img, pic2.img) out=picx.img
Beginning VICAR task picsum
PICSUM version 14 Feb 2011
GETPROJ: unrecognizable project
PICSUM task completed
list picx.img
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Mon Feb 14 18:11:23 2011
 Task:PICSUM    User:lwk       Date_Time:Mon Feb 14 18:11:23 2011
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1     32764 32766 32767 32767-32764-32762-32760-32758-32756-32754
      2     32766 32767 32767-32764-32762-32760-32758-32756-32754-32752
      3     32767 32767-32764-32762-32760-32758-32756-32754-32752-32750
      4     32767-32764-32762-32760-32758-32756-32754-32752-32750-32748
      5    -32764-32762-32760-32758-32756-32754-32752-32750-32748-32746
      6    -32762-32760-32758-32756-32754-32752-32750-32748-32746-32744
      7    -32760-32758-32756-32754-32752-32750-32748-32746-32744-32742
      8    -32758-32756-32754-32752-32750-32748-32746-32744-32742-32740
      9    -32756-32754-32752-32750-32748-32746-32744-32742-32740-32738
     10    -32754-32752-32750-32748-32746-32744-32742-32740-32738-32736
gen out=pic1.img nl=10 ns=10 ival=32764 'half
Beginning VICAR task gen
GEN Version 6
GEN task completed
list pic1.img
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Mon Feb 14 18:11:23 2011
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1     32764 32765 32766 32767-32768-32767-32766-32765-32764-32763
      2     32765 32766 32767-32768-32767-32766-32765-32764-32763-32762
      3     32766 32767-32768-32767-32766-32765-32764-32763-32762-32761
      4     32767-32768-32767-32766-32765-32764-32763-32762-32761-32760
      5    -32768-32767-32766-32765-32764-32763-32762-32761-32760-32759
      6    -32767-32766-32765-32764-32763-32762-32761-32760-32759-32758
      7    -32766-32765-32764-32763-32762-32761-32760-32759-32758-32757
      8    -32765-32764-32763-32762-32761-32760-32759-32758-32757-32756
      9    -32764-32763-32762-32761-32760-32759-32758-32757-32756-32755
     10    -32763-32762-32761-32760-32759-32758-32757-32756-32755-32754
gen out=pic2.img nl=10 ns=10
Beginning VICAR task gen
GEN Version 6
GEN task completed
list pic2.img
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Mon Feb 14 18:11:24 2011
     Samp     1       3       5       7       9
   Line
      1       0   1   2   3   4   5   6   7   8   9
      2       1   2   3   4   5   6   7   8   9  10
      3       2   3   4   5   6   7   8   9  10  11
      4       3   4   5   6   7   8   9  10  11  12
      5       4   5   6   7   8   9  10  11  12  13
      6       5   6   7   8   9  10  11  12  13  14
      7       6   7   8   9  10  11  12  13  14  15
      8       7   8   9  10  11  12  13  14  15  16
      9       8   9  10  11  12  13  14  15  16  17
     10       9  10  11  12  13  14  15  16  17  18
picsum inp=(pic1.img, pic2.img) out=picx.img
Beginning VICAR task picsum
PICSUM version 14 Feb 2011
GETPROJ: unrecognizable project
PICSUM task completed
picsum inp=(pic2.img, pic1.img) out=picz.img
Beginning VICAR task picsum
PICSUM version 14 Feb 2011
GETPROJ: unrecognizable project
PICSUM task completed
difpic (picx.img, picz.img)
Beginning VICAR task difpic
DIFPIC version 05jul10
 NUMBER OF DIFFERENCES =   0
list picx.img
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Mon Feb 14 18:11:23 2011
 Task:PICSUM    User:lwk       Date_Time:Mon Feb 14 18:11:24 2011
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1     32764 32766 32767 32767-32764-32762-32760-32758-32756-32754
      2     32766 32767 32767-32764-32762-32760-32758-32756-32754-32752
      3     32767 32767-32764-32762-32760-32758-32756-32754-32752-32750
      4     32767-32764-32762-32760-32758-32756-32754-32752-32750-32748
      5    -32764-32762-32760-32758-32756-32754-32752-32750-32748-32746
      6    -32762-32760-32758-32756-32754-32752-32750-32748-32746-32744
      7    -32760-32758-32756-32754-32752-32750-32748-32746-32744-32742
      8    -32758-32756-32754-32752-32750-32748-32746-32744-32742-32740
      9    -32756-32754-32752-32750-32748-32746-32744-32742-32740-32738
     10    -32754-32752-32750-32748-32746-32744-32742-32740-32738-32736
if ($syschar(1)="UNIX")
   let dir = "/project/test_work/testdata/cassini/iss/"
   ush cp /project/test_work/testdata/cassini/iss/sum2.1 .
   ush cp /project/test_work/testdata/cassini/iss/sum2.2 .
   ush cp /project/test_work/testdata/cassini/iss/sum2.3 .
else
end-if
picsum (/project/test_work/testdata/cassini/iss/sum2.1 /project/test_work/testdata/cassini/iss/sum2.3 /project/test_work/testdata/c+
assini/iss/sum2.2) out.img
Beginning VICAR task picsum
PICSUM version 14 Feb 2011
GROUP_BLOCKS VALUE MISSING
ALGORITHM VALUE MISSING
BLOCK_TYPE VALUE MISSING
QUANTIZATION_FACTOR_INDEX VALUE MISSING
MISSING_LINES VALUE MISSING
GROUP_BLOCKS VALUE MISSING
ALGORITHM VALUE MISSING
BLOCK_TYPE VALUE MISSING
QUANTIZATION_FACTOR_INDEX VALUE MISSING
MISSING_LINES VALUE MISSING
SCLK=        1 CAM=ISSNA GAIN=100K  MODE=SUM2 EXP=      0 FILT1=RED   FILT2=CLR
GROUP_BLOCKS VALUE MISSING
ALGORITHM VALUE MISSING
BLOCK_TYPE VALUE MISSING
QUANTIZATION_FACTOR_INDEX VALUE MISSING
MISSING_LINES VALUE MISSING
SCLK=        3 CAM=ISSNA GAIN=100K  MODE=SUM2 EXP=      0 FILT1=RED   FILT2=CLR
GROUP_BLOCKS VALUE MISSING
ALGORITHM VALUE MISSING
BLOCK_TYPE VALUE MISSING
QUANTIZATION_FACTOR_INDEX VALUE MISSING
MISSING_LINES VALUE MISSING
SCLK=        2 CAM=ISSNA GAIN=100K  MODE=SUM2 EXP=      0 FILT1=RED   FILT2=CLR
PICSUM task completed
hist out.img 'nohist
Beginning VICAR task hist
HIST version 15-NOV-05


AVERAGE GRAY LEVEL=51.20888       STANDARD DEVIATION=9.926864       NUMBER ELEMENTS=  262144
MIN. DN=        22
MAX. DN=        87

label-l out.img
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File out.img ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in HALF format from a SUN-SOLR host
                1 bands
                512 lines per band
                512 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Property: CASSINI-ISS ----
MISSION_NAME='CASSINI'
MISSION_PHASE_TYPE='BENCH'
INSTRUMENT_ID='ISSNA'
IMAGE_NUMBER=1
IMAGE_TIME='1997-128T12:34:56.800'
SOFTWARE_VERSION_ID='V4'
INSTRUMENT_MODE_ID='SUM2'
FILTER1_NAME='RED'
FILTER2_NAME='CLR'
GAIN_MODE_ID='100K'
ENCODING_TYPE='NOTCOMP'
CONVERSION_TYPE='8LSB'
DETECTOR_TEMPERATURE=-90.0
OPTICS_TEMPERATURE=-10.0
FILTER_TEMPERATURE=-999.0
LIGHT_FLOOD_STATE_FLAG='OFF'
ANTIBLOOMING_STATE_FLAG='OFF'
CALIB_LAMP_STATE_FLAG='OFF'
COMPRESSION_PARAMETER_VALUE=-1249
OFFSET=3
DARK_CURRENT=2
COMPRESSION_RATIO=-999.0
TARGET_NAME='FLATFIELD'
OBSERVATION_ID='LIGHT_XFR'
ILLUMINANT='TUNGSTEN'
LUMINANCE=5.1
EXPOSURE_DURATION=0.0
RADIANCE=5.1
---- Task: PICSUM -- User: lwk -- Mon Feb 14 18:11:25 2011 ----
PICSCALE=3
NFRAMES=3
SCLKS=(1, 2, 3)
 
************************************************************
createfile picsum.srchlist
refgbl  $syschar
write "CREATEFILE version 1-3-97"
CREATEFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  USH cp /dev/null  picsum.srchlist
end-if
END-PROC
addtofile picsum.srchlist "NEXT FILE=0001"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="NEXT FILE=0001"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile picsum.srchlist "sum2.1"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="sum2.1"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile picsum.srchlist "sum2.3"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="sum2.3"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile picsum.srchlist "sum2.2"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="sum2.2"
Beginning VICAR task addtofil
end-if
END-PROC
picsum /project/test_work/testdata/cassini/iss/sum2.1 out.img list=picsum.srchlist
Beginning VICAR task picsum
PICSUM version 14 Feb 2011
GROUP_BLOCKS VALUE MISSING
ALGORITHM VALUE MISSING
BLOCK_TYPE VALUE MISSING
QUANTIZATION_FACTOR_INDEX VALUE MISSING
MISSING_LINES VALUE MISSING
GROUP_BLOCKS VALUE MISSING
ALGORITHM VALUE MISSING
BLOCK_TYPE VALUE MISSING
QUANTIZATION_FACTOR_INDEX VALUE MISSING
MISSING_LINES VALUE MISSING
SCLK=        1 CAM=ISSNA GAIN=100K  MODE=SUM2 EXP=      0 FILT1=RED   FILT2=CLR
GROUP_BLOCKS VALUE MISSING
ALGORITHM VALUE MISSING
BLOCK_TYPE VALUE MISSING
QUANTIZATION_FACTOR_INDEX VALUE MISSING
MISSING_LINES VALUE MISSING
SCLK=        3 CAM=ISSNA GAIN=100K  MODE=SUM2 EXP=      0 FILT1=RED   FILT2=CLR
GROUP_BLOCKS VALUE MISSING
ALGORITHM VALUE MISSING
BLOCK_TYPE VALUE MISSING
QUANTIZATION_FACTOR_INDEX VALUE MISSING
MISSING_LINES VALUE MISSING
SCLK=        2 CAM=ISSNA GAIN=100K  MODE=SUM2 EXP=      0 FILT1=RED   FILT2=CLR
PICSUM task completed
hist out.img 'nohist
Beginning VICAR task hist
HIST version 15-NOV-05


AVERAGE GRAY LEVEL=51.20888       STANDARD DEVIATION=9.926864       NUMBER ELEMENTS=  262144
MIN. DN=        22
MAX. DN=        87

label-l out.img
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File out.img ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in HALF format from a SUN-SOLR host
                1 bands
                512 lines per band
                512 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Property: CASSINI-ISS ----
MISSION_NAME='CASSINI'
MISSION_PHASE_TYPE='BENCH'
INSTRUMENT_ID='ISSNA'
IMAGE_NUMBER=1
IMAGE_TIME='1997-128T12:34:56.800'
SOFTWARE_VERSION_ID='V4'
INSTRUMENT_MODE_ID='SUM2'
FILTER1_NAME='RED'
FILTER2_NAME='CLR'
GAIN_MODE_ID='100K'
ENCODING_TYPE='NOTCOMP'
CONVERSION_TYPE='8LSB'
DETECTOR_TEMPERATURE=-90.0
OPTICS_TEMPERATURE=-10.0
FILTER_TEMPERATURE=-999.0
LIGHT_FLOOD_STATE_FLAG='OFF'
ANTIBLOOMING_STATE_FLAG='OFF'
CALIB_LAMP_STATE_FLAG='OFF'
COMPRESSION_PARAMETER_VALUE=-1249
OFFSET=3
DARK_CURRENT=2
COMPRESSION_RATIO=-999.0
TARGET_NAME='FLATFIELD'
OBSERVATION_ID='LIGHT_XFR'
ILLUMINANT='TUNGSTEN'
LUMINANCE=5.1
EXPOSURE_DURATION=0.0
RADIANCE=5.1
---- Task: PICSUM -- User: lwk -- Mon Feb 14 18:11:28 2011 ----
PICSCALE=3
NFRAMES=3
SCLKS=(1, 2, 3)
 
************************************************************
if ($syschar(1)="UNIX")
   ush rm sum2.*
else
end-if
write "The output DN values should be (sum of 3 DNs * 128.0/3.0)."
The output DN values should be (sum of 3 DNs * 128.0/3.0).
picsum (/project/test_work/testdata/cassini/iss/sum2.1 /project/test_work/testdata/cassini/iss/sum2.2 /project/test_work/testdata/c+
assini/iss/sum2.3) out.img 'ascale
Beginning VICAR task picsum
PICSUM version 14 Feb 2011
GROUP_BLOCKS VALUE MISSING
ALGORITHM VALUE MISSING
BLOCK_TYPE VALUE MISSING
QUANTIZATION_FACTOR_INDEX VALUE MISSING
MISSING_LINES VALUE MISSING
GROUP_BLOCKS VALUE MISSING
ALGORITHM VALUE MISSING
BLOCK_TYPE VALUE MISSING
QUANTIZATION_FACTOR_INDEX VALUE MISSING
MISSING_LINES VALUE MISSING
SCLK=        1 CAM=ISSNA GAIN=100K  MODE=SUM2 EXP=      0 FILT1=RED   FILT2=CLR
GROUP_BLOCKS VALUE MISSING
ALGORITHM VALUE MISSING
BLOCK_TYPE VALUE MISSING
QUANTIZATION_FACTOR_INDEX VALUE MISSING
MISSING_LINES VALUE MISSING
SCLK=        2 CAM=ISSNA GAIN=100K  MODE=SUM2 EXP=      0 FILT1=RED   FILT2=CLR
GROUP_BLOCKS VALUE MISSING
ALGORITHM VALUE MISSING
BLOCK_TYPE VALUE MISSING
QUANTIZATION_FACTOR_INDEX VALUE MISSING
MISSING_LINES VALUE MISSING
SCLK=        3 CAM=ISSNA GAIN=100K  MODE=SUM2 EXP=      0 FILT1=RED   FILT2=CLR
PICSUM task completed
list /project/test_work/testdata/cassini/iss/sum2.1 sl=10 ss=10 nl=100 ns=100 linc=10 sinc=10
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
     Samp      10    20    30    40    50    60    70    80    90   100
   Line
     10        21    24    21    23    22    20    18    24    18    20
     20        21    24    21    21    21    23    24    19    17    24
     30        21    21    20    21    24    19    23    20    19    19
     40        19    21    19    21    21    24    21    21    21    23
     50        25    25    24    24    21    21    23    20    19    21
     60        24    24    21    25    21    21    19    25    20    19
     70        24    21    21    21    20    20    21    21    21    21
     80        24    21    22    24    22    21    23    24    21    20
     90        24    20    20    21    24    17    21    19    19    23
    100        23    25    21    24    21    21    24    21    21    19
list /project/test_work/testdata/cassini/iss/sum2.2 sl=10 ss=10 nl=100 ns=100 linc=10 sinc=10
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
     Samp      10    20    30    40    50    60    70    80    90   100
   Line
     10        24    25    24    21    19    21    21    22    21    20
     20        25    21    24    21    20    22    25    19    21    21
     30        21    21    20    22    18    21    20    21    21    21
     40        21    25    24    20    24    20    20    25    20    19
     50        24    21    24    21    21    22    20    24    20    21
     60        21    21    24    24    21    21    22    24    17    24
     70        21    21    21    21    21    20    20    21    19    21
     80        21    24    23    21    19    19    19    21    20    22
     90        24    24    21    21    19    24    24    21    21    20
    100        23    21    19    20    22    21    17    21    21    20
list /project/test_work/testdata/cassini/iss/sum2.3 sl=10 ss=10 nl=100 ns=100 linc=10 sinc=10
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
     Samp      10    20    30    40    50    60    70    80    90   100
   Line
     10        21    19    24    25    21    21    19    21    21    25
     20        24    20    21    21    21    21    19    21    19    21
     30        24    20    24    24    22    22    19    24    19    24
     40        21    18    21    24    20    21    20    20    19    21
     50        21    25    25    21    19    21    21    20    20    21
     60        21    23    21    19    21    21    20    17    25    24
     70        21    24    21    21    21    20    19    20    20    19
     80        21    22    22    21    21    21    21    23    25    24
     90        24    25    21    23    21    18    20    21    20    21
    100        24    24    21    24    24    21    24    20    21    21
list out.img sl=10 ss=10 nl=100 ns=100 linc=10 sinc=10
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:PICSUM    User:lwk       Date_Time:Mon Feb 14 18:11:31 2011
     Samp      10    20    30    40    50    60    70    80    90   100
   Line
     10      2816  2901  2944  2944  2645  2645  2474  2858  2560  2773
     20      2986  2773  2816  2688  2645  2816  2901  2517  2432  2816
     30      2816  2645  2730  2858  2730  2645  2645  2773  2517  2730
     40      2602  2730  2730  2773  2773  2773  2602  2816  2560  2688
     50      2986  3029  3114  2816  2602  2730  2730  2730  2517  2688
     60      2816  2901  2816  2901  2688  2688  2602  2816  2645  2858
     70      2816  2816  2688  2688  2645  2560  2560  2645  2560  2602
     80      2816  2858  2858  2816  2645  2602  2688  2901  2816  2816
     90      3072  2944  2645  2773  2730  2517  2773  2602  2560  2730
    100      2986  2986  2602  2901  2858  2688  2773  2645  2688  2560
hist out.img 'nohist
Beginning VICAR task hist
HIST version 15-NOV-05


AVERAGE GRAY LEVEL=2184.599       STANDARD DEVIATION=423.5399       NUMBER ELEMENTS=  262144
MIN. DN=       938
MAX. DN=      3712

picsum (/project/test_work/testdata/cassini/iss/sum2.1 /project/test_work/testdata/cassini/iss/sum2.2 /project/test_work/testdata/c+
assini/iss/sum2.3) out.img tscale=(3.,3.)
Beginning VICAR task picsum
PICSUM version 14 Feb 2011
GROUP_BLOCKS VALUE MISSING
ALGORITHM VALUE MISSING
BLOCK_TYPE VALUE MISSING
QUANTIZATION_FACTOR_INDEX VALUE MISSING
MISSING_LINES VALUE MISSING
GROUP_BLOCKS VALUE MISSING
ALGORITHM VALUE MISSING
BLOCK_TYPE VALUE MISSING
QUANTIZATION_FACTOR_INDEX VALUE MISSING
MISSING_LINES VALUE MISSING
SCLK=        1 CAM=ISSNA GAIN=100K  MODE=SUM2 EXP=      0 FILT1=RED   FILT2=CLR
GROUP_BLOCKS VALUE MISSING
ALGORITHM VALUE MISSING
BLOCK_TYPE VALUE MISSING
QUANTIZATION_FACTOR_INDEX VALUE MISSING
MISSING_LINES VALUE MISSING
SCLK=        2 CAM=ISSNA GAIN=100K  MODE=SUM2 EXP=      0 FILT1=RED   FILT2=CLR
GROUP_BLOCKS VALUE MISSING
ALGORITHM VALUE MISSING
BLOCK_TYPE VALUE MISSING
QUANTIZATION_FACTOR_INDEX VALUE MISSING
MISSING_LINES VALUE MISSING
SCLK=        3 CAM=ISSNA GAIN=100K  MODE=SUM2 EXP=      0 FILT1=RED   FILT2=CLR
MINT=          1
Number of bad pixels=     138487
PICSUM task completed
hist out.img 'nohist
Beginning VICAR task hist
HIST version 15-NOV-05


AVERAGE GRAY LEVEL=51.22421       STANDARD DEVIATION=10.22587       NUMBER ELEMENTS=  262144
MIN. DN=        24
MAX. DN=        87

picsum (/project/test_work/testdata/cassini/iss/sum2.1 /project/test_work/testdata/cassini/iss/sum2.2 /project/test_work/testdata/c+
assini/iss/sum2.3) out.img tscale=(3.,3.) mint=5
Beginning VICAR task picsum
PICSUM version 14 Feb 2011
GROUP_BLOCKS VALUE MISSING
ALGORITHM VALUE MISSING
BLOCK_TYPE VALUE MISSING
QUANTIZATION_FACTOR_INDEX VALUE MISSING
MISSING_LINES VALUE MISSING
GROUP_BLOCKS VALUE MISSING
ALGORITHM VALUE MISSING
BLOCK_TYPE VALUE MISSING
QUANTIZATION_FACTOR_INDEX VALUE MISSING
MISSING_LINES VALUE MISSING
SCLK=        1 CAM=ISSNA GAIN=100K  MODE=SUM2 EXP=      0 FILT1=RED   FILT2=CLR
GROUP_BLOCKS VALUE MISSING
ALGORITHM VALUE MISSING
BLOCK_TYPE VALUE MISSING
QUANTIZATION_FACTOR_INDEX VALUE MISSING
MISSING_LINES VALUE MISSING
SCLK=        2 CAM=ISSNA GAIN=100K  MODE=SUM2 EXP=      0 FILT1=RED   FILT2=CLR
GROUP_BLOCKS VALUE MISSING
ALGORITHM VALUE MISSING
BLOCK_TYPE VALUE MISSING
QUANTIZATION_FACTOR_INDEX VALUE MISSING
MISSING_LINES VALUE MISSING
SCLK=        3 CAM=ISSNA GAIN=100K  MODE=SUM2 EXP=      0 FILT1=RED   FILT2=CLR
MINT=          5
Number of bad pixels=       2268
PICSUM task completed
hist out.img 'nohist
Beginning VICAR task hist
HIST version 15-NOV-05


AVERAGE GRAY LEVEL=51.20851       STANDARD DEVIATION=9.945005       NUMBER ELEMENTS=  262144
MIN. DN=        22
MAX. DN=        87

end-proc
exit
slogoff
if ($RUNTYPE = "INTERACTIVE")
  if ($syschar(1) = "VAX_VMS")
  end-if
else
  if ($syschar(1) = "VAX_VMS")
  end-if
end-if
ulogoff
END-PROC
END-PROC
$ Return
$!#############################################################################
