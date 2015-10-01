$!****************************************************************************
$!
$! Build proc for MIPL module fastclas
$! VPACK Version 1.9, Wednesday, March 10, 2010, 12:09:33
$!
$! Execute by entering:		$ @fastclas
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
$ write sys$output "*** module fastclas ***"
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
$ write sys$output "Invalid argument given to fastclas.com file -- ", primary
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
$   if F$SEARCH("fastclas.imake") .nes. ""
$   then
$      vimake fastclas
$      purge fastclas.bld
$   else
$      if F$SEARCH("fastclas.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake fastclas
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @fastclas.bld "STD"
$   else
$      @fastclas.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create fastclas.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack fastclas.com -mixed -
	-s fastclas.f -
	-i fastclas.imake -
	-p fastclas.pdf -
	-t tstfastclas.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create fastclas.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      implicit none


C     MARCH 1984  ...HBD... CONVERT TO VAX
C     DEC.  1987  ...SP.... CORRECTED BUG FROM VAX CONVERSION FOR CSIGMA.
C                           PROGRAM WAS SWAPPING CLASS AND BAND NUMBERS.
C     MAY   1990  ...HJF... INCREASED COUNTS TO 600, FIXED PRINTOUT BUG,
C			    CHANGED PDF DESCRIPTION OF CSIGMA AND USE,
C			    USE RECSIZE TO GET SIZE OF STATS INPUT.
C     JAN 02 1995 ...CRI... MSTP S/W CONVERSION (VICAR PORTING)
C
C        'FASTCLAS   HYBRID CLASSIFIER USING PARALLELPIPED & BAYESIAN
C  FASCLAS MODIFICATIONS BY ALAN H. STRAHLER
C  DEPT OF GEOGRAPHY, UNIVERSITY OF CALIFORNIA AT SANTA BARBARA
C  MOST RECENT VERSION OF FSTCLSPR, 10/13/78
C

      LOGICAL XVPTST
      INTEGER MSS,PR,PB,MN
      LOGICAL DONT,CHECK,PBAND
      INTEGER PRIO(24),LINE1(10),BND(12),INPFIL(10)
      INTEGER PRIOR(12),PCLIM(12),DIMLST(13), LCB(4),NCLS
      INTEGER USE(12), IL, J, JJ, OP, NPRB, NPCH
      integer NI,input,cnt,nfiles,R2FBUF(12), F2RBUF(12),STATUS
      INTEGER SS,NL,NLI,NSS,NSI
      INTEGER SDS, IN, NFETIN,MTRX,NN,NM,II,NNI,ITEMPNMI
      INTEGER JM,KK,NOSETS,NCHEK,PTR,I,K,IBAND
      INTEGER NBR,NBP,OUTFIL,ABB,MANY,INFIL,ITEMP

      REAL    SIG,RMNTB(3,200)
      REAL    WORK(12,12)
      REAL    RTEMP,VAL,V,SIGMA(600),PROB(600),work1(12,12)

      BYTE    LL
      CHARACTER BLANK,COMMA

      CHARACTER*27  LBL1
      CHARACTER*62  PR1
      CHARACTER*70  PR3
      CHARACTER*96  PR4
      CHARACTER*132 PR8
      CHARACTER*21  PR0
      CHARACTER*120 PR12
      CHARACTER*87  PR13
      character*36  pr14
      character*49  pr15
      character*60  pr16

      character*59  pr17
      character*70  pr18
C
      COMMON /PP/ PPLIST
      REAL        PPLIST(100)

      INTEGER LENDIM, LENTAB, ADIM(10000)
      REAL ATAB(10000)
      COMMON /C4/ LENDIM, LENTAB, ATAB, ADIM

      COMMON /C1/ NS,CON,PTRBUF,VGOOD,GOOD,LOW,HGH,OBUF,BUF
      INTEGER     NS,PTRBUF(12)
      REAL        CON(100)
      BYTE        VGOOD(100),GOOD(100),LOW(100,12),HGH(100,12)
      BYTE        OBUF(2000),BUF(15000)
      CHARACTER*15000 CHBUF
      EQUIVALENCE (BUF,CHBUF)


      COMMON /C2/ MEAN, COV, CLIM, XSIG
      REAL        MEAN(12,100), COV(78,100), XSIG(12,100), CLIM(100)
 
      COMMON /SIZE/ SL
      INTEGER SL

C
      INCLUDE 'fortport'

C Local variable initialization
      DATA  LENDIM/0/, LENTAB/0/
      DATA USE   /1,2,3,4,5,6,7,8,9,10,11,12/ 
      DATA COMMA /','/
      DATA BLANK /' '/

! INTEGER variable initialization
      DATA SS/0/,NL/0/,NS/0/,NLI/0/,NSS/0/
      DATA SDS/0/, IN/0/, NFETIN/0/,MTRX/0/,NN/0/
      DATA NM/0/,II/0/,NNI/0/,ITEMPNMI/0/
      DATA JM/0/,KK/0/,NOSETS/0/,NCHEK/0/,PTR/0/,I/0/,K/0/,IBAND/0/
      DATA NBR/0/,
     &     NBP/0/,
     &     OUTFIL/0/,
     &     ABB/0/,
     &     MANY/0/,
     &     INFIL/0/,
     &     ITEMP/0/
      DATA NCLS/0/,OP/0/,NPRB/0/,NPCH/0/,IL/0/,J/0/
      DATA MSS/0/,DONT/.FALSE./,CHECK/.FALSE./,PBAND/.FALSE./,PR/0/,
     &			PB/0/,MN/0/

      DATA PR8/' '/
      DATA LBL1 /'FASTCLAS CLASSIFICATION MAP'/
      DATA PR3/'*** CLASSIFICATION WILL USE BANDS'/
      DATA PR4/'*** NUMBER ST DEVS  +/-'/                                
      PR12(01:45) = 'CLS BND  MEAN  CLS BND  MEAN  CLS BND  MEAN  '
      pr12(46:90) = 'CLS BND  MEAN  CLS BND  MEAN  CLS BND  MEAN  '
      PR12(91:120)= 'CLS BND  MEAN  CLS BND  MEAN  '
      PR13(01:41) = '*** BANDS USED AS EXTERNAL CLASSIFICATION'
      pr13(42:87) = ' CHANNELS ARE                                 '
      pr14(01:36) = '*** BAYESIAN SEPARATION NOT USED ***'
      pr15(01:49) = '*** Multi-variate confidence interval checked ***'
      PR16(01:43) = '*** Means for bands and classes will be set'
      PR16(44:60) = ' as indicated ***'
      pr17(01:38) = 'PROB CARD IDENTIFIES PRIOR CHANNEL NOT'
      pr17(39:59) = ' FOUND ON PRIOR CARD'
      pr18(01:40) = 'VALUE IN PRIOR CHANNEL IMAGE EXCEEDS MAX'
      pr18(41:70) = ' VALUE SPECIFIED IN PRIOR PARAM'



      CALL zia (ATAB,10000)
      CALL zia (ADIM,10000)
      call zia (prio,24)
      call zia (line1,10)
      call zia (bnd,12)
      call zia (ptrbuf,12) 
      call zia (inpfil,10)
      call zia (prior,12)
      call zia (pclim,12)
      call zia (dimlst,13)
      call zia (lcb,4)
      call zia (R2FBUF,12)
      call zia (PPLIST,100)

! REAL variable initialization
      V   = 0.0
      SIG = 0.0
      call zia (xsig,12*100)
      call zia (clim,100)
      call zia (rmntb,3*200)
      call zia (mean,12*100)
      call zia (cov,78*100)
      call zia (con,100)
      call zia (work,12*12)
      call zia (work1,12*12)
      call zia (sigma,600)
      call zia (prob,600)

! BYTE variable initialization
      call zia (buf,15000/4)
      call zia (obuf,2000/4)
      call zia (low,100*12/4)
      call zia (hgh,100*12/4)
      call zia (good,100/4)
      call zia (vgood,100/4)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C        PARAMETERS
C     'MSS',N       DENOTES INPUT HAS N BANDS IN MSS FORMAT.
C     'BANDS',I,J,K INPUT STATS ARE FROM BANDS I,J,K ...
C     'USE',I,J,K   CLASSIFICATION TO USE BANDS I,J,K ...
C     'SIGMA',R1,R2..  NUMBER OF ST. DEVS TO USE FOR EACH BAND
C     'CSIGMA',I,J,R1...  SIGMA FOR CLASS I & BAND J IS R1
C     'DONT'        DONT SEPARATE WITH BAYESIAN CLASSIFIER
C     'CHECK'       CHECK MULTI-VARIATE CONFIDENCE INTERVAL WITH BAYES
C     'MEAN',I,J,R1...  MEAN R1 FOR CLASS I AND BAND J REPLACES
C                   STATS MEAN
C     'PRIOR',I,J,I,J   BAND I CONTAINS SUBSCRIPTS WITH VALUES 0-J
C                   USED FOR PRIOR PROBABILITY LOOKUP IN FBAYES
C     'PROB',I,J,R1,R2...  R1-RN ARE N PRIOR PROBABILITIES OF CLASS
C                   MEMBERSHIP GIVEN THAT BAND I HAS VALUE J.
C                   WHEN PRIOR IDENTIFIES ONE BAND ONLY, I MUST BE
C                   OMITTED.  IF THE PROBABILITIES ARE SIMPLE
C                   (UNCONDITIONAL) PRIORS, BOTH I AND J MUST
C                   BE OMITTED.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
      CALL IFMESSAGE('FASTCLAS version 02-Jan-95')
C
      call XVEACTION ('SA',' ')

C     Call xvtrans_set for REAL to FULL translation      
      call xvtrans_set (R2FBUF,'REAL','FULL',status)

C     Call xvtrans_set for FULL to REAL translation      
      call xvtrans_set (F2RBUF,'FULL','REAL',status)

C		Find format of files -- MSS OR INDIVIDUAL
      CALL XVPCNT('INP',NI)
      NFILES = NI - 1
      CALL XVPARM('MSS',INPUT,CNT,MSS,1)
      MSS = MSS - 1
      IF (MSS.NE.0) NFILES = INPUT
c
      CALL XVPARM('PRIOR',PRIO,NPCH,PR,24)
      PR = PR - 1
      IF (PR.NE.0) THEN
         DO J = 1, NPCH, 2
            JJ = (J+1) / 2
            PRIOR(JJ) = PRIO(J)
            PCLIM(JJ) = PRIO(J+1)
         END DO
         NPCH = NPCH / 2			! NPCH = # prob. channels
         NPRB = NPCH + 1			! NPRB = # specified probs.
      END IF

C               Open data sets
      CALL XVPARM('SIZE',SL,CNT,STATUS,1)		! Find value of SL
      IF (STATUS .EQ. 1) THEN
         CALL XVPARM('SL',SL,CNT,STATUS,1)
         IF (STATUS .EQ. 1) SL = 1
      END IF                                     
      IF (MSS.NE.0) THEN
         CALL XVUNIT(INPFIL(1),'INP',1,STATUS,' ')
         CALL XVOPEN(INPFIL(1),STATUS,' ')
         LINE1(1) = SL
      ELSE
	 DO I = 1, (NFILES + NPCH)      ! # image files + # prob files
	    CALL XVUNIT(INPFIL(I),'INP',I,STATUS,' ')
	    CALL XVOPEN(INPFIL(I),STATUS,' ')
	    LINE1(I) = SL
	 END DO
      END IF
C		Decide which bands and files to use in analysis
      CALL XVSIZE(SL,SS,NL,NS,NLI,NSS)
      CALL XVPARM('BAND',BND,cnt,STATUS,12)
      if (status .eq. 0) nfiles = cnt
      CALL XVPARM('USE',USE,cnt,STATUS,12)
      if (status .eq. 0) nfiles = cnt
      DO J = 1, NFILES
         IF (BND(J) .EQ. 0)  BND(J) = USE(J)
      END DO
C		Determine NSI
      IF (MSS.NE.0)  THEN
	 NSI = NSS / INPUT
         IF (NS .EQ. NSS)  NS = NSI
      ELSE
	 NSI = NSS
      END IF
      DONT = XVPTST('DONT')
      IF (DONT)  CHECK = .FALSE.
C                Size error checking
      IF (SL+NL-1 .GT. NLI)  GO TO 910
      IF (SS+NS-1 .GT. NSI)  GO TO 910
      IF (NS*(NFILES+NPCH) .GT. 15000)  GO TO 920
C
C
C        Read STAT data set, load table
      CALL XVUNIT(SDS,'INP',NI,STATUS,' ')
      CALL XVOPEN(SDS,STATUS,' ')
      CALL XVGET(SDS,STATUS,'NL',NCLS,'RECSIZE',IN,' ') !HJF 5-90 !
      NFETIN = (SQRT(2.0 * IN - 15.0) - 3.0) / 2.0
      MTRX = (NFETIN * (NFETIN + 1)) / 2
      NN = 4 * NFETIN
      NM = 4 * MTRX

      DO II = 1, NCLS
         CALL XVREAD(SDS,BUF,STATUS,' ')
         CALL MVE (1,NN,BUF(9),MEAN(1,II),1,1)
         CALL MVE (1,NM,BUF(NN+13),COV(1,II),1,1)
      END DO
      CALL XVCLOSE(SDS,STATUS,' ')	! close statistic file
C
C                 Adjust means as input by user
      CALL XVPARM('MEAN',RMNTB,JM,MN,3*200)
      MN = MN - 1
      IF (MN.NE.0) THEN
         IF (MOD(JM,3) .NE. 0) THEN
	    CALL XVMESSAGE('MEAN VALUES MUST BE A MULTIPLE OF 3',' ')
	    CALL ABEND
         END IF
         JM = JM / 3
         DO KK = 1, JM
           MEAN(INT(RMNTB(2,KK)),INT(RMNTB(1,KK))) = RMNTB(3,KK)
         END DO
      END IF
C
C        Set high and low values according to SIGMAS or CSIGMAS
      CALL XVPARM('SIGMA',SIGMA,CNT,STATUS,600)     ! Get SIGMA values
      IF (STATUS .EQ. 1) CNT = 1
      DO I = 1, CNT
         DO J = 1, 100
	   XSIG(I,J) = SIGMA(I)
         END DO
      END DO
      DO I = (CNT+1), NFILES
         DO J = 1, 100
	    XSIG(I,J) = SIGMA(CNT)
         END DO
      END DO
      CALL XVPARM('CSIGMA',SIGMA,CNT,STATUS,600)    ! Get CSIGMA values
      IF (STATUS .EQ. 0) THEN
	 OP = 1
	 IF (MOD(CNT,3) .NE. 0) THEN
	    CALL MABEND('CSIGMA VALUES MUST BE A MULTIPLE OF 3')
	 END IF
	 DO I = 1, CNT, 3
            call xvtrans(r2fbuf,sigma(I),II,1)
            call xvtrans(r2fbuf,sigma(I+1),J,1)
	    IF (J .EQ. -1) THEN
	       DO JJ = 1, NFILES
		 XSIG(JJ,II) = SIGMA(I+2)
	       END DO
	    ELSE
		 XSIG(J,II) = SIGMA(I+2)
	    END IF
	 END DO
      END IF
      DO II = 1, NCLS			! determine high and low values
         DO J = 1, NFILES
            K = BND(J)
            JJ = (K * (K + 1)) / 2
            SIG = SQRT(COV(JJ,II)) * XSIG(J,II)
C
C NOTE THAT XSIG(J) IS USED WITH COV(BND(J)), THE BAND # IN CSIGMA
C REFERS TO THE ORDER OF USE
C
            RTEMP = MEAN(K,II) - SIG + 0.5
            CALL xvtrans(R2FBUF,RTEMP,IL,1)
            LL = INT2BYTE(IL)
            IF (IL .LT. 0)  THEN
               IL = 0
               LL = INT2BYTE(IL)
            ENDIF
            LOW(II,J) = LL
            RTEMP = MEAN(K,II) + SIG + 0.5
            CALL xvtrans(R2FBUF,RTEMP,IL,1)
            LL = INT2BYTE (IL)
            IF (IL .GT. 255)  THEN
               IL = 255
               LL = INT2BYTE(IL)
            ENDIF
            HGH(II,J) = LL
         END DO
      END DO
C
C            Set up dimlst and call DIM for cond. prob. table
      CALL XVPARM('PROB',PROB,CNT,PB,600)
      PB = PB - 1

      IF (PB.NE.0) THEN
         IF (PR.EQ.0) NPRB = 1
      END IF

      IF (NPRB .EQ. 0) GO TO 254
 
      DIMLST(1) = NCLS

      IF (NPRB - 1) 600,600,610

600   CONTINUE
      CALL INIT(DIMLST,1)
      GO TO 618

610   CONTINUE
      DO I = 2, NPRB
         DIMLST(I) = PCLIM(I-1)
      END DO
      CALL INIT(DIMLST,NPRB)
C
C            Read PROB values
618   continue
      NOSETS = CNT / (NCLS + NPCH)
      NCHEK = NOSETS * (NCLS + NPCH)
      PTR = 1
      IF (CNT .NE. NCHEK) GO TO 940
      DIMLST(1) = 1
      IF (NPRB - 2) 620,630,640

620   CONTINUE
      CALL SETLST(DIMLST,PROB(PTR),NCLS)

      DIMLST(1) = 0
      CALL SET(DIMLST,1.)
      CALL CONVRT
      GO TO 254
630   continue
      DO I = 1, NOSETS
         CALL XVTRANS (R2FBUF,PROB(PTR),DIMLST(2),1)
         CALL SETLST(DIMLST,PROB(PTR+1),NCLS)
         PTR = PTR + NCLS + 1
      END DO
      GO TO 254
640   continue
      DO I = 1, NOSETS
         DO J = 2, 13
            DIMLST(J) = 0
         END DO
         CALL XVTRANS (R2FBUF,PROB(PTR),IBAND,1)
         DO J = 1, 12
            IF(PRIOR(J).EQ.IBAND) GO TO 649
         END DO
         GO TO 930
649      continue
         CALL XVTRANS (R2FBUF,PROB(PTR+1),DIMLST(J+1),1)
         CALL SETLST(DIMLST,PROB(PTR+2),NCLS)
         PTR = PTR + NCLS + 2
      END DO
254   CONTINUE
C
C
C        CHECK BAND ORDER
      DO J = 1, NFILES
         IF (USE(J) .NE. J)  THEN
            PBAND = .TRUE.
         END IF
      END DO

      IF (NFILES .NE. NFETIN)  THEN
         PBAND = .TRUE.
      END IF
C
C        PRINT SUMMARY
      WRITE (PR1,9900) NCLS,NFETIN
9900  FORMAT ('*** STATISTICS CONTAIN',I4,' CLASSES AND',I4,
     +' SPECTRAL BANDS ***')
      CALL XVMESSAGE(PR1,' ')
      IF (.NOT. PBAND) THEN
        CALL XVMESSAGE('*** CLASSIFICATION WILL USE ALL BANDS ***',' ')
      ELSE
         PTR = 35
         DO J = 1,NFILES
            PR3(PTR+1:PTR+1) = ','
            WRITE (PR3(PTR-1:PTR),'(I2)') USE(J)
            PTR = PTR + 3
         END DO
         CALL XVMESSAGE(PR3,' ')
      END IF
      IF (OP .LE. 0) THEN			! SIGMA's or CSIGMA's used
         PTR = 30
         DO J = 1, NFILES
            WRITE (PR4(PTR-5:PTR),'(F6.1)') XSIG(J,1)
            PTR = PTR + 6
         END DO
         CALL XVMESSAGE(PR4,' ')
      ELSE
         CALL XVMESSAGE(PR4,' ')
         PR8(1:12) = '        BAND'
         PTR = 18
         DO J = 1, NFILES
            WRITE (PR8(PTR-3:PTR),'(I4)') USE(J)
            PTR = PTR + 6
         END DO
         CALL XVMESSAGE(PR8,' ')
	 CALL XVMESSAGE('  CLASS',' ')
         pr8 = ' '
         DO I = 1, NCLS
            WRITE (PR8(4:7),'(I4)') I
            PTR = 19
            DO J = 1, NFILES
               WRITE (PR8(PTR-5:PTR),'(F6.1)') XSIG(J,I)
               PTR = PTR + 6
            END DO
            CALL XVMESSAGE(PR8,' ')
         END DO
         CALL XVMESSAGE(' ',' ')
      END IF
      IF (DONT) CALL XVMESSAGE (PR14,' ')
      IF (CHECK) CALL XVMESSAGE(PR15,' ')
      IF (MN.EQ.0) GO TO 298		! Means
      CALL XVMESSAGE(PR16,' ')
      PTR = 0
      DO JJ = 1, JM
         WRITE (CHBUF((PTR+1):(PTR+3)),'(I3)') INT(RMNTB(1,JJ))
         WRITE (CHBUF((PTR+4):(PTR+7)),'(I4)') INT(RMNTB(2,JJ))
         call xvtrans (r2fbuf,rmntb(3,jj),itemp,1)
         WRITE (CHBUF((PTR+8):(PTR+13)),'(I6)') itemp
         buf(ptr+14) = ichar(' ')
         buf(ptr+15) = ichar(' ')
         PTR = PTR + 15
      END DO
      CALL XVMESSAGE(PR12(1:PTR),' ')
      CALL XVMESSAGE(chbuf(1:PTR),' ')
C
298   continue
      if (pr .ne. 0) THEN             	! PROBS used
         DO J = 1, NPCH
            JJ = 55 + J * 3


            WRITE (PR13(JJ-2:JJ),'(I2)') PRIOR(J)
         END DO
         CALL XVMESSAGE(PR13,' ')
      END IF
      IF (PB.NE.0) 					! PRIOR probs used
     +   CALL XVMESSAGE('*** PRIOR PROBABILITIES WILL BE USED ***',' ')
C
C           UPDATE AND ADD NEW LABEL
      CALL XVUNIT(OUTFIL,'OUT',1,STATUS,' ')
      CALL XVOPEN(OUTFIL,STATUS,'OP','WRITE','U_NS',NS,' ')
      CALL XLADD(OUTFIL,'HISTORY','COMMENT',LBL1,STATUS,'ULEN',27,
     +          'FORMAT','STRING',' ')
      ABB = SS - 1
      IF (MSS.NE.0) THEN
         WRITE (PR0,9910) NS
9910  FORMAT ('** OUTPUT NS=',I4,' **')
         CALL XVMESSAGE(PR0,' ')
      END IF
      IF (.NOT. DONT)  CALL COVIN(NCLS,NFETIN,NFILES,BND,WORK,work1)
C
 
C        LOAD BUFFERS
      DO J = 1, NCLS
         VGOOD(J) = INT2BYTE(J)
      END DO
      DO J = 1, NFILES			! determine SL within image file(s)
         IF (MSS.NE.0) THEN			! according to format of files
            PTRBUF(J) = NSI * (USE(J) - 1) + ABB
         ELSE
            PTRBUF(J) = NS * (USE(J) - 1)
         END IF
      END DO
      IF (NPCH .NE. 0) THEN		! do the same for prior prob files
         DO J = 1, NPCH
            IF (MSS.NE.0) THEN
                PTRBUF(NFILES+J) = NSI * (PRIOR(J) - 1) + ABB
            ELSE
                PTRBUF(NFILES+J) = NS * (PRIOR(J) - 1)
            END IF
         END DO
      END IF
      CALL TBLSET(NCLS)
      MANY = 0
C
C
C        COMPUTE MULTI-VARIATE CONFIDENCE LIMITS
      IF (CHECK) THEN
         DO I = 1, NCLS
            SIG = 0.
            DO J = 1, NFILES
               SIG = SIG + XSIG(J,I)
            END DO
            CLIM(I) = SQRT(0.5) * SIG / NFILES
         END DO
      END IF
      IF (NPCH .EQ. 0) GO TO 500
C
C FIND JOINT DISTRIBUTION OF PRIOR PROBABILITY CHANNELS
      DIMLST(1) = 0
      NNI = (NL - 1) / 5 + 1
      DO II = 1, NNI
         IF (MSS.NE.0) THEN
            CALL XVREAD(INPFIL(1),BUF,STATUS,' ')
            LINE1(1) = LINE1(1) + 5
         ELSE
            DO J = 1, NPCH
               PTR = PTRBUF(NFILES+J) + 1
               JJ = PRIOR(J)
               CALL XVREAD(INPFIL(JJ),BUF(PTR),status,'LINE',LINE1(JJ),
     +                     'SAMP',ABB,'NSAMPS',NS,' ')
               LINE1(JJ) = LINE1(JJ) + 5
            END DO
         END IF
         DO J = 1, NS
            DO JJ = 2, NPRB
               LL = BUF(PTRBUF(NFILES+JJ-1)+J)
               IL = BYTE2INT(LL)              
               IF (IL .GT. PCLIM(NPRB-1)) GO TO 950
               DIMLST(JJ) = IL
            END DO
            V = VAL(DIMLST)+1.0
            CALL SET(DIMLST,V)
         END DO
      END DO
      CALL CPROB(PCLIM,NPRB,NCLS)
      JJ = NNI * 5
      IF (MSS.NE.0) THEN
         LINE1(1) = LINE1(1) - JJ
      ELSE
         DO J = 1, NPCH
            LINE1(PRIOR(J)) = LINE1(PRIOR(J)) - JJ
         END DO
      END IF
C
C
C        START CLASSIFICATION
500   DO II = 1, NL
         IF (MSS.NE.0) THEN
            CALL XVREAD(INPFIL(1),BUF,STATUS,'LINE',LINE1(1),
     +                 'NSAMPS',NSS,' ')
            LINE1(1) = 0
         ELSE
            DO J = 1, NFILES
               PTR = PTRBUF(J) + 1
               CALL XVREAD(INPFIL(J),BUF(PTR),STATUS,'LINE',LINE1(J),
     +                    'SAMP',SS,'NSAMPS',NS,' ')
               LINE1(J) = 0
            END DO
         END IF
C
C             CLASSIFY THE LINE
         CALL LOOKUP(NFILES,NCLS,DONT,CHECK,MANY,NPRB)
         CALL XVWRIT(OUTFIL,OBUF,STATUS,'NSAMPS',NS,' ')
      END DO
C
      WRITE (PR8,9920) MANY
9920  FORMAT ('... BAYESIAN ROUTINE CALLED',I9,' TIMES ...')
      IF (.NOT. DONT) CALL XVMESSAGE (PR8,' ')

      call XVCLOSE(OUTFIL,STATUS,' ')
      RETURN
C
C
910   CALL MABEND('... Area exceeds input picture size ...',' ')
920   CALL MABEND('... NS*NBANDS .GT. 15000 ...',' ')
930   CALL XVMESSAGE(pr17,' ')
940   CALL MABEND('PROB PARM HAS BAD NUMBER OF VALUES',' ')
950   CALL MABEND(pr18,' ')
      RETURN
      END
C
C
C
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      SUBROUTINE COVIN (NC,NVIN,NV,BND,R,RR)

      implicit none

! Define Passed parameters
      integer NC,NVIN,NV,BND(12)
      REAL    R(NV,NV),RR(NV,NV)

! Define COMMON variables

      COMMON /C1/ NS,CON,PTRBUF,VGOOD,GOOD,LOW,HGH,OBUF,BUF
      INTEGER     NS,PTRBUF(12)
      REAL        CON(100)
      BYTE        VGOOD(100),GOOD(100),LOW(100,12),HGH(100,12)
      BYTE        OBUF(2000),BUF(15000)
      CHARACTER*15000 CHBUF
      EQUIVALENCE (BUF,CHBUF)

      COMMON /C2/ MEAN,COV,CLIM,XSIG
      REAL        MEAN(12,100), COV(78,100), CLIM(100), XSIG(1200)

! Define Local variables
      REAL G(12,12), LG2PI
      REAL DET, VMEAN(12)

      INTEGER LW(12,12), MW(12)
      integer I,J,JJ,JK,L,JM,LM

C  Initialize local variables
      call zia (G,12*12)
      LG2PI = 0.0
      DET   = 0.0
      CALL ZIA (VMEAN,12)
 
      CALL ZIA (LW,12*12)
      CALL ZIA (MW,12)
      I  = 0
      J  = 0
      JJ = 0
      JK = 0
      L  = 0
      JM = 0
      LM = 0 
C

      LG2PI = -0.5 * FLOAT(NV) * ALOG(6.283185)
      DO I = 1, NC
!        UNSCRAMBLE THE COVARIANCE MATRIX
         DO J = 1, 12
            JK =  (J * (J - 1)) / 2
            DO L = 1, J
               G(J,L) = COV(JK+L,I)
               G(L,J) = G(J,L)
            END DO
         END DO
!        EXTRACT BANDS USED, LOAD WORKING BUFFER
         DO J = 1, NV
            JM = BND(J)
            VMEAN(J) = MEAN(JM,I)
            DO L = 1, NV
               LM = BND(L)
               R(J,L) = G(JM,LM)
            END DO
         END DO
C
C           INVERT MATRIX, COMPUTE CONSTANT
         do j = 1, nv
             do l = 1, nv
                rr(j,l) = r(j,l)
             end do
         end do
         CALL INVERT(R,NV,DET,LW,MW)
         IF (DET .LE. 1.0E-10)  DET = 1.
         CON(I) = -0.5 * ALOG(DET) + LG2PI
C
C           RE-LOAD COVARIANCE BUFFER, TRIANGULARIZE
         JJ = 0
         DO J = 1, NV
            MEAN(J,I) = VMEAN(J)
            DO L = 1, J
               JJ = JJ + 1
               IF (L .EQ. J)  R(L,J) = 0.5 * R(L,J)
               COV(JJ,I) = R(L,J)
            END DO
         END DO
C
      END DO
      RETURN
      END
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      subroutine invert(a,n,d,l,m)
      implicit none

C        THIS PROGRAM INVERTS A GENERAL MATIX USING THE GAUSS-JORDAN
C        ELMINATION METHOD AND BACK AND FORWARD SUBSTITUTION. A MATRIX
C        IS SINGULAR IF THE DETERMINANT RETURN AS ZERO.
C
C        PARAMETERS
C           A - INPUT MATRIX, DESTROYED IN COMPUTATION AND REPLACED BY
C               RESULTANT INVERSE.
C           N - ORDER OF MATRIX A
C           D - RESULTANT DETERMINANT
C           L - WORK VECTOR OF LENGTH N
C           M - WORK VECTOR OF LENGTH N
C
C
! Define Passed parameters
      INTEGER L(1),M(1)
      INTEGER N
      REAL    A(N,N),D

! Define local variables
      REAL    LGVAL,TEMP
      INTEGER I,J,K

! Initialize local variables
      LGVAL = 0.0
      TEMP = 0.0
      I = 0
      J = 0
      K = 0

C
C        FIND LARGEST ELEMENT
C
      D = 1.0
      DO K = 1, N
         L(K) = K
         M(K) = K
         LGVAL = A(K,K)
         DO J = K, N
            DO I = K, N
               IF ((ABS(LGVAL) - ABS(A(I,J))) .LT. 0.0) THEN
                  LGVAL = A(I,J)
                  L(K) = I
                  M(K) = J
               END IF
            END DO
         END DO
C
C           INTERCHANGE ROWS
C
         J = L(K)
         IF ((J - K) .GT. 0) THEN
            DO I = 1, N
               TEMP = -A(K,I)
               A(K,I) = A(J,I)
               A(J,I) = TEMP
            END DO
         END IF
C
C           INTERCHANGE COLUMNS
C
         I = M(K)
         IF ((I-K) .GT. 0) THEN
            DO J = 1, N
               TEMP = -A(J,K)
               A(J,K) = A(J,I)
               A(J,I) = TEMP
            END DO
         END IF
C
C           DIVIDE COLUMN BY MINUS PIVOT (VALUE OF PIVOT ELEMENT IS
C           CONTAINED IN LGVAL)
C
         IF (LGVAL .EQ. 0.0) THEN
            D = 0.0
            RETURN
         END IF
         DO I = 1, N
            IF ((I-K) .NE. 0) A(I,K) = A(I,K) / (-LGVAL)
         END DO
C
C           REDUCE MATRIX
C
         DO I = 1,N
            TEMP = A(I,K)
            DO J = 1, N
               IF ((I-K) .NE. 0) THEN
                  IF ((J-K) .NE. 0) A(I,J) = TEMP * A(K,J) + A(I,J)
               END IF
            END DO
         END DO
C
C           DIVIDE ROW BY PIVOT
C
         DO J = 1, N
            IF ((J-K) .NE. 0) A(K,J) = A(K,J) / LGVAL
         END DO
C
C           PRODUCT OF PIVOTS
C
         D = D * LGVAL
C
C           REPLACE PIVOT BY RECIPROCAL
C
         A(K,K) = 1.0 / LGVAL
      END DO
C
C        FINAL ROW AND COLUMN INTERCHANGE
C
      K = N
  100 K = (K - 1)
      IF (K) 150,150,105
  105 I = L(K)
      IF ((I-K) .GT. 0) THEN
         DO J = 1, N
            TEMP = A(J,K)
            A(J,K) = -A(J,I)
            A(J,I) = TEMP
         END DO
      END IF
      J = M(K)
      IF ((J-K) .GT. 0) THEN
         DO I = 1, N
            TEMP = A(K,I)
            A(K,I) = -A(J,I)
            A(J,I) = TEMP
         END DO
      END IF
      GO TO 100
  150 RETURN
      END

!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      SUBROUTINE CPROB(PCLIM,NPRB,NCLS)
C 

! Define Passed parameters
      INTEGER PCLIM(12),NPRB,NCLS

! Define Local variables
      INTEGER SS(3),I1,I2,I3,I,ITER,K,L1,L2,L3,J
      INTEGER R2FBUF(12)
      INTEGER F2RBUF(12)
      REAL    GRAND,MAXDIM(3),SUM,V,F

! Initialize local variables
      SS(1) = 0
      SS(2) = 0
      SS(3) = 0
      I  = 0
      I1 = 0
      I2 = 0
      I3 = 0
      ITER = 0
      J  = 0
      K  = 0
      L1 = 0
      L2 = 0
      L3 = 0
      CALL ZIA (R2FBUF,12)
      CALL ZIA (F2RBUF,12)


      GRAND = 0.0
      MAXDIM(1) = 0.0
      MAXDIM(2) = 0.0
      MAXDIM(3) = 0.0
      SUM = 0.0
      F   = 0.0


C SET UP MAXDIM, TEST NPRB
      MAXDIM(1) = NCLS
      DO I = 2, NPRB
         MAXDIM(I) = PCLIM(I - 1)
      END DO
      IF (NPRB - 2) 100,100,250
C
C NPROB = 2.  SIMPLE 1-WAY CONDITIONALS REQUIRED.  NO ITERATIVE
C FITTING NEEDED.  ONLY CALCULATIONS ARE VALUES FOR UNCONDITIONAL
C PRIOR PROBABILITIES.
C
100   SS(1) = 0
      SUM = 0.
      L1 = MAXDIM(1)
      L2 = MAXDIM(2)
      DO J = 1, L2
         SS(2) = J
         SUM = SUM + VAL(SS)
      END DO
      DO J = 1, L2
         SS(2) = J
         V = VAL(SS) / SUM
         CALL SET(SS,V)
      END DO
C
C FIND UNCONDITIONAL PRIORS FOR CLASSES
C
      DO J = 1, L2
         SS(1) = 0
         SS(2) = J
         F = VAL(SS)
         DO I = 1, L1
            SS(1) = I
            SS(2) = J
            V = F * VAL(SS)
            SS(2) = 0
            V = V + VAL(SS)
            CALL SET(SS,V)
         END DO
      END DO
C
C SET 0,0 ELEMENT, CONVRT, AND RETURN
C
      SS(1) = 0
      SS(2) = 0
      CALL SET(SS,1.0)
      CALL CONVRT
      RETURN
C
C NPROB = 3.  ITERATIVE FITTING FOR MLE'S OF JOINT PROBS REQUIRED.
C
C CONVERT COUNTS FOR PRIOR PROB CHANNELS INTO JOINT PROBS
C
250   SS(1) = 0
      L2 = MAXDIM(2)
      L3 = MAXDIM(3)
      GRAND = 0.0
C
C FIND TOTALS OVER SECOND CHANNEL
      DO J = 1, L2
         SS(2) = J
         SUM = 0.0
         DO K = 1, L3
            SS(3) = K
            SUM = SUM + VAL(SS)
         END DO
         GRAND = GRAND+SUM
         SS(3) = 0
         CALL SET(SS,SUM)
      END DO
C
C FIND TOTALS OVER FIRST CHANNEL
C
      DO K = 1, L3
         SS(3) = K
         SUM = 0.0
         DO J = 1, L2
            SS(2) = J
            SUM = SUM + VAL(SS)
         END DO
         SS(2) = 0
         CALL SET(SS,SUM)
      END DO
C
C STORE GRAND TOTAL
C
      SS(2) = 0
      SS(3) = 0
      CALL SET(SS,GRAND)
C
C CONVERT VALUES AND TOTALS TO PROPORTIONS
C
      L2 = MAXDIM(2) + 1
      L3 = MAXDIM(3) + 1
      DO J = 1, L2
         SS(2) = J - 1
         DO K = 1, L3
            SS(3) = K - 1
            V = VAL(SS) / GRAND
            CALL SET(SS,V)
         END DO
      END DO
C
C CHANGE CONDITIONAL PROBS TO JOINT PROBS.  PRIOR*COND = JOINT
C
      L1 = MAXDIM(1)
      L2 = MAXDIM(2)
      L3 = MAXDIM(3)
      SS(3) = 0
      DO J = 1,L2
         SS(2) = J
         SS(1) = 0
         F = VAL(SS)
         DO I = 1, L1
            SS(1) = I
            V = F * VAL(SS)
            CALL SET(SS,V)
         END DO
      END DO
      SS(2) = 0
      DO 330 K = 1, L3
         SS(3) = K
         SS(1) = 0
         F = VAL(SS)
         DO 330 I = 1, L1
            SS(1) = I
            V = F * VAL(SS)
            CALL SET(SS,V)
  330 CONTINUE
C
C INITIALIZE JOINT PROBS AT 1.
C
      DO 335 I = 1, L1
         SS(1) = I
         DO 335 J = 1, L2
            SS(2) = J
            DO 335 K = 1, L3
               SS(3) = K
               CALL SET(SS,1.)
  335 CONTINUE
C
C ITERATE TO MLE'S FOR JOINT PROBS.  TEN ITERS SHOULD BE ENOUGH.
C
C FIRST THREE LOOPS PERMUTE SUBSCRIPTS
C
      DO 400 ITER = 1, 10
         DO 380 I1 = 1, 3
            DO 370 I2 = 1, 3
               IF (I2 .EQ. I1) GO TO 370
               DO 360 I3 = 1, 3
                  IF (I3.EQ.I1 .OR. I3.EQ.I2) GO TO 360
C
C SET LOOP LIMITS
C
                  L1 = MAXDIM(I1)
                  L2 = MAXDIM(I2)
                  L3 = MAXDIM(I3)
C
C DO ONE SET OF ITERATIONS
C
                  DO 355 I = 1, L1
                     SS(I1) = I
                     DO 350 J = 1, L2
                        SS(I2) = J
                        SUM = 0.0
C
C FIND SUM OF PROBS OVER ALL K, GET MULTIPLIER FACTOR F
C
                        DO 340 K = 1, L3
                           SS(I3) = K
                           SUM = SUM + VAL(SS)
  340                   CONTINUE
C
C CHECK FOR ZERO SUM.  IF ZERO, CLASS HAS PROB ZERO.
C
                        IF (SUM) 350,350,345
345                     SS(I3) = 0
                        F = VAL(SS) / SUM
C
C RESCALE PROBS BY FACTOR
C
                        DO 348 K = 1, L3
                           SS(I3) = K
                           V = VAL(SS) * F
                           CALL SET(SS,V)
  348                   CONTINUE
  350                CONTINUE
  355             CONTINUE
  360          CONTINUE
  370       CONTINUE
  380    CONTINUE
  400 CONTINUE
C
C FIND UNCONDITIONAL PRIOR PROBS
      L1 = MAXDIM(1)
      L2 = MAXDIM(2)
      L3 = MAXDIM(3)
      DO I = 1, L1
         SS(1) = I
         SUM = 0.0
         DO J = 1, L2
            SS(2) = J
            DO K = 1, L3
               SS(3) = K
               SUM = SUM + VAL(SS)
            END DO
         END DO
         SS(2) = 0
         SS(3) = 0
         CALL SET(SS,SUM)
      END DO
C
C CONVERT JOINT PROBS TO CONDITIONALS
C
      DO 430 I = 1, 3
         SS(I) = 0
  430 CONTINUE
      CALL SET(SS,1.0)
      L1 = MAXDIM(1)
      L2 = MAXDIM(2) + 1
      L3 = MAXDIM(3) + 1
      DO 440 J = 1, L2
         SS(2) = J - 1
         DO 440 K = 1, L3
            SS(3) = K - 1
            SS(1) = 0
            F = VAL(SS)
            DO 440 I = 1, L1
               SS(1) = I
               V = VAL(SS) / F
               CALL SET(SS,V)
  440 CONTINUE
      CALL CONVRT
      RETURN
      END
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
!
! 
      SUBROUTINE FBAYES(NV,NC,II,CHECK,NPRB,NCLS)
      implicit none
      INCLUDE 'fortport'
C
C  *** BAYESIAN MAXIMUM LIKELIHOOD ALGORITHM
C
! Define Passed parameters
      INTEGER NV, NC, II, CHECK, NPRB, NCLS
 
! Define COMMON variables
      COMMON /C1/ NS, CON, PTRBUF, VGOOD, GOOD, LOW, HGH, OBUF, BUF
      INTEGER     NS, PTRBUF(12)
      REAL        CON(100)
      BYTE        BUF(15000), OBUF(2000), VGOOD(100), GOOD(100)
      BYTE        LOW(1200), HGH(1200)


      COMMON /C2/ MEAN, COV, CLIM, XSIG
      REAL        MEAN(1200), COV(7800), CLIM(100), XSIG(1200)

      COMMON /C3/ TABLE, TABLE2
      INTEGER     TABLE(100), TABLE2(100)

      COMMON /PP/ PPLIST
      REAL        PPLIST(100)

!@!@       EQUIVALENCE (R7B,R7I), (CLNUMB,CLNUM)
!@!@       EQUIVALENCE (SSVAL,SSVALB)

! Define data translation buffers
      INTEGER R2FBUF(12)
      INTEGER F2RBUF(12)

! Define Local variables
      BYTE     R7B, CLNUMB, SSVALB
      INTEGER  SS(13)
      INTEGER  R7I, LPDEX, TEMP, LIMIT, CTR, PTRVAL
      INTEGER  CLCNTR, CLNUM, IC, STATUS
      INTEGER  CLASS, CLCOV, PTR, LP1, LP2, LC, SSVAL
      REAL     VAL2, TMAX, TSUM, SUM, DATA(10000)

!  Initialize local variables
      DATA SS/1,0,0,0,0,0,0,0,0,0,0,0,0/
      DATA R7B/0/, CLNUMB/0/, SSVALB/0/
      DATA R7I/0/, LPDEX/0/, TEMP/0/, LIMIT/0/, CTR/0/, PTRVAL/0/
      DATA CLCNTR/0/, CLNUM/0/


      DATA CLASS/0/, CLCOV/0/, PTR/0/, LP1/0/, LP2/0/, LC/0/, SSVAL/0/


      DATA VAL2/0.0/, TMAX/0.0/, TSUM/0.0/, SUM/0.0/

      CALL ZIA (DATA,10000)
      CALL ZIA (R2FBUF,12)
      CALL ZIA (F2RBUF,12)

!!    
C  Call xvtrans_set for REAL to FULL and FULL to REAL translations
      call xvtrans_set (R2FBUF,'REAL','FULL',status)
      call xvtrans_set (F2RBUF,'FULL','REAL',status)

C  *** DATA(I) = DN VAL FOR BAND I
      DO LPDEX = 1, NV
         TEMP = II + PTRBUF(LPDEX)
         R7B = BUF(TEMP)
         R7I = BYTE2INT(R7B)       ! Translate byte to int
         CALL XVTRANS (F2RBUF, R7I, DATA(LPDEX), 1)
      END DO

C
C  *** TEST FOR NUMBER OF PRIOR PROBABILITIES
      IF (NPRB .LT. 1) GOTO 70
      IF (NPRB .EQ. 1) GOTO 60
      LIMIT = NPRB - 1
C
C  *** SET UP SUBSCRIPT LIST FOR PROBABILITES
      DO CTR = 1, LIMIT
         PTRVAL = PTRBUF(NV+CTR) + II
         SSVAL = 0
         SSVALB = BUF(PTRVAL)
         SSVAL = BYTE2INT (SSVALB)
         SS(CTR+1) = SSVAL
      END DO
C
C  *** GET PROBABILITIES
   60 continue
      CALL GETLST(SS,PPLIST,NCLS)
      IF (NPRB .NE. 1) GOTO 70
      NPRB = 0
C
C  *** CALCULATE THE LIKELIHOOD VALUES, CHOOSE MAXIMUM
   70 continue
      CLCNTR = 1
      CLNUM = 0
      TMAX = -1.0E38
  120 continue
      CLNUMB = GOOD(CLCNTR)
      CLNUM =  BYTE2INT (CLNUMB)
      PTR = CLNUM
      LC = 1
      TSUM = 0.0

      CLASS = TABLE(PTR)
      CLCOV = TABLE2(PTR)
      DO LP1 = 1, NV
         SUM = 0.0
         DO LP2 = 1, LP1
            SUM = SUM + ((DATA(LP2) - MEAN(LP2+CLASS)) * COV(LC+CLCOV))
            LC = LC + 1
         END DO
         TSUM = TSUM + ((DATA(LP1) - MEAN(LP1+CLASS)) * SUM)
      END DO
      SUM = (CON(PTR) - TSUM) + PPLIST(PTR)
C
C  *** SAVE MAX VALUE AND CLASS NUMBER OF MAX VALUE
      IF (SUM .GT. TMAX) THEN
         TMAX = SUM
         IC = CLNUM
      ENDIF
      CLCNTR = CLCNTR + 1 
      IF (CLCNTR .LE. NC) GOTO 120
C
C  *** ASSIGN DN = CLASS NUMBER OF MAX VALUE
      OBUF(II) = INT2BYTE(IC)
C
C  *** IF CHECK OPTION INVOKED 
      IF (CHECK .NE. 0) THEN
         VAL2 = SQRT(ABS(CON(IC) - TMAX))
         IF (VAL2 .GT. CLIM(IC)) OBUF(II)=0
      ENDIF
      RETURN
      END
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      SUBROUTINE TBLSET(NCLS)
      implicit none
C
C *** ROUTINE TO SET UP THE INDICES INTO THE TABLES
C
! Define Passed parameters
      INTEGER  NCLS

! Define COMMON variables
      COMMON /C3/ TABLE, TABLE2
      INTEGER  TABLE(100), TABLE2(100)

! Define Local variables
      INTEGER INDEX, COUNT, VALUE

C  Initialize local variables
      INDEX = 0
      COUNT = 0
      VALUE = 0

      DO 10 INDEX = 1, NCLS
         TABLE(INDEX) = COUNT
         TABLE2(INDEX) = VALUE
         COUNT = COUNT + 12
         VALUE = VALUE + 78
   10 CONTINUE
      RETURN
      END
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      SUBROUTINE LOOKUP(NFILES, NCLS, DONT, CHECK, MANY, NPRB)

      implicit none

      INCLUDE 'fortport'

! Define Passed parameters
      INTEGER NFILES, NCLS, DONT, CHECK, MANY, NPRB

! Define COMMON variables
      COMMON /C1/ NS, CON, PTRBUF, VGOOD, GOOD, LOW, HGH, OBUF, BUF
      COMMON /C2/ MEAN, COV, CLIM, XSIG
      REAL        MEAN(1200),COV(7800), CLIM(100), XSIG(1200)
      REAL CON(100)

      INTEGER LENDIM, LENTAB, ADIM(10000)
      REAL ATAB(10000)
      COMMON /C4/ LENDIM, LENTAB, ATAB, ADIM

      BYTE    VGOOD(100), GOOD(100), LOW(1200), HGH(1200), OBUF(2000)
      BYTE    BUF(15000)
      INTEGER PTRBUF(12), NS

! Define Local variables
      BYTE    CLCNTRB
      BYTE    IB, VALB, DNVALB, LOWLIMB, UPPERLIMB, GDCLSB
      INTEGER ADDR(12)
      INTEGER CLLPIND,LPIND, LPINC, CLINC, J, I, CLCNTR, DNVAL, CLCOUNT
      INTEGER TABADR, BUFPTR, GDCLS, LOWLIM
      INTEGER NGOOD, J1, UPPERLIM, VAL

! Initialize local variables
      DATA  ADDR/0,100,200,300,400,500,600,700,800,900,1000,1100/
      DATA  CLCNTRB/0/,DNVALB/0/, LOWLIMB/0/, UPPERLIMB/0/
      DATA  GDCLSB/0/
      DATA  CLLPIND/0/,LPIND/0/, LPINC/0/, CLINC/0/, J/0/
      DATA  I/0/, CLCNTR/0/, DNVAL/0/, CLCOUNT/0/
      DATA  TABADR/0/, BUFPTR/0/, GDCLS/0/, LOWLIM/0/
      DATA  NGOOD/0/, J1/0/, UPPERLIM/0/,val/0/
C                   

      DO 450 J = 1, NS			! J IS THE SAMPLE COUNT
C        
C *** MAKE ALL CLASSES VALID CLASSIFICATIONS
         DO I = 1, NCLS
            IB = BYTE2INT(I)
            GOOD(I) = IB
         END DO

         CLCNTR = NCLS
         DNVAL = 0
         GDCLS = 0
C
C  *** DETERMINE VALID CLASSES ACCORDING TO THRESHOLD VALUES
C  *** CLCNTR IS THE NUMBER OF GOOD CLASSES
         DO 180 LPIND = 1, NFILES
            BUFPTR = PTRBUF(LPIND) + J
            DNVALB = BUF(BUFPTR)
            DNVAL   = BYTE2INT(DNVALB)
            CLCOUNT = CLCNTR		!CLASS COUNT
            CLCNTR = 0			!CLASS COUNTER
            DO 80 CLLPIND = 1, CLCOUNT	!CLASS LOOP INDEX
               GDCLSB  = GOOD(CLLPIND)   !GOOD CLASS
               GDCLS = BYTE2INT(GDCLSB)
               TABADR  = GDCLS + ADDR(LPIND)
               LOWLIMB = LOW(TABADR)
               LOWLIM  = BYTE2INT(LOWLIMB) 
               IF (DNVAL .LT. LOWLIM) GOTO 80
               UPPERLIMB = HGH(TABADR)
               UPPERLIM  = BYTE2INT(UPPERLIMB)
               IF (DNVAL .GT. UPPERLIM) GOTO 80
               CLCNTR = CLCNTR + 1
               GOOD(CLCNTR) = GDCLSB
   80       CONTINUE
            IF (CLCNTR .LE. 1) GOTO 200		!IF ONLY 1 VALID CLASS
  180    CONTINUE
         IF (DONT .NE. 0) GOTO 190
         NGOOD = CLCNTR
         GOTO 400
C
C  *** BRANCH TO HERE IF BAYESIAN OPTION NOT USED
  190    VAL = 255
         VALB = INT2BYTE(VAL)
         OBUF(J) = VALB
         GOTO 450
C
C  *** DETEMINE IF ONLY ONE GOOD CLASS EXIST AND IF ALL CHANNELS
C  *** HAVE BEEN USED
  200    IF (CLCNTR .LT. 1) GOTO 300
         CLCNTRB = GOOD(1)
         CLCNTR = BYTE2INT(CLCNTRB)
C  *** VERIFY REMAINING CHANNELS
         DO 220 I = LPIND+1, NFILES
            BUFPTR = PTRBUF(I) + J
            DNVALB = BUF(BUFPTR)
            DNVAL  = BYTE2INT (DNVALB)
            TABADR = CLCNTR + ADDR(I)
            LOWLIMB = LOW(TABADR)
            LOWLIM  = BYTE2INT(LOWLIMB)
            IF (DNVAL .LT. LOWLIM) GOTO 300
            UPPERLIMB = HGH(TABADR)
            UPPERLIM  = BYTE2INT(UPPERLIMB)
            IF (DNVAL .GT. UPPERLIM) GOTO 300
  220    CONTINUE
C
C  *** IF CHECK OPTION ISN'T INVOKED, THEN DN = CLCNTR
         IF (CHECK .EQ. 0) THEN
            clcntrb = int2byte(clcntr)
            OBUF(J) = CLCNTRB
            GOTO 450
         ELSE
C
C  *** ELSE ASSIGN THE NUMBER OF GOOD CLASSES TO 1 AND CALL FBAYES
            NGOOD = 1
            GOTO 400
         ENDIF
C
C  *** CLASS IS UNKNOWN, DN = 0
  300    OBUF(J) = 0
         GOTO 450
C
C  *** CALL FBAYES TO DETERMINE ANY AMBIGUITY
  400    MANY = MANY + 1
         CALL FBAYES(NFILES, NGOOD, J, CHECK, NPRB, NCLS)
  450 CONTINUE
      RETURN
      END

!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!

      SUBROUTINE INIT(DIMLST, NODIM)
      implicit none
C
C
! Define Passed parameters
      INTEGER DIMLST(13), NODIM

! Define COMMON variables
      COMMON /C4/ LENDIM, LENTAB, ATAB, ADIM
      INTEGER LENDIM, LENTAB, ADIM(10000)
      REAL ATAB(10000)
C
! Define Local variables
      INTEGER LIMIT, VAL, CNTR

! Initialize local variables
      LIMIT = 0
      VAL   = 0
      CNTR  = 0

      LIMIT  = NODIM
      LENDIM = NODIM
      LENTAB = 1

      DO 100 CNTR = 1, LIMIT
         VAL = 1 + DIMLST(CNTR)
         LENTAB = LENTAB * VAL
         ADIM(CNTR) = VAL
  100 CONTINUE
      CALL ZIA(ATAB,10000)
      RETURN
      END

!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
 
      SUBROUTINE SET(SUBLIST,VALUE)
      implicit none
C
C*** COPIES VALUE INTO TABLE LOCATED AT SUBLIST + 1
C
! Define Passed parameters
      INTEGER SUBLIST(1)
      REAL    VALUE
 
! Define COMMON variables
      COMMON /C4/ LENDIM, LENTAB, ATAB, ADIM
      INTEGER LENDIM, LENTAB, ADIM(10000)
      REAL    ATAB(10000)
C
! Define Local variables
      INTEGER ADR
      INTEGER IGETADDR


      ADR = IGETADDR(SUBLIST)
      ATAB(ADR+1) = VALUE
      RETURN
      END
 

!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
 
      REAL FUNCTION VAL(SUBLIST)
      implicit none
C
C  *** RETURNS VALUE OF ATAB LOCATED AT SUBLIST + 1
C
      INTEGER SUBLIST(1), ADR, LENDIM, LENTAB, ADIM(10000)
      REAL    ATAB(10000)
C
      COMMON /C4/ LENDIM, LENTAB, ATAB, ADIM
C
      INTEGER IGETADDR


      ADR = IGETADDR(SUBLIST)
      VAL = ATAB(ADR+1)
      RETURN
      END
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!

      SUBROUTINE GETLST(SUBLIST, VALLST, LEN)


      implicit none

C
C  *** COPIES LEN VALUES FROM TABLE INTO VALLST BEGINNING AT ELEMENT
C  *** DENOTED BY SUBLST
C
! Define Passed parameters
      INTEGER SUBLIST(1), LEN
      REAL    VALLST(1)

! Define COMMON variables
      COMMON /C4/ LENDIM, LENTAB, ATAB, ADIM
      REAL    ATAB(10000)
      INTEGER LENDIM, LENTAB, ADIM(10000)

! Define Local variables
      INTEGER ADR, CNTR
      INTEGER IGETADDR
      real val
 
      ADR = 0    
      CNTR = 0


      ADR = IGETADDR(SUBLIST)
C
      DO 10 CNTR = 1, LEN
         VAL = ATAB(CNTR + ADR)
         VALLST(CNTR) = VAL
   10 CONTINUE
      RETURN
      END
 
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
   
      SUBROUTINE SETLST(SUBLST, VALLST, LEN)
      implicit none
C
C  *** COPIES LEN VALUES FORM VALLST INTO TABLE BEGINNING AT ELEMENT
C  *** DENOTED BY SUBLST
C
! Define Passed parameters
      INTEGER SUBLST(1), LEN
      REAL VALLST(1)
C
! Define COMMON variables
      COMMON /C4/ LENDIM, LENTAB, ATAB, ADIM
      INTEGER LENDIM, LENTAB, ADIM(10000)
      REAL    ATAB(10000)
C
! Define Local variables
      REAL    VAL
      INTEGER ADR, CNTR
      INTEGER IGETADDR

      VAL = 0.0
      ADR = 0
      CNTR = 0

      ADR = IGETADDR(SUBLST)
C
      DO 10 CNTR = 1, LEN
         VAL = VALLST(CNTR)
         ATAB(CNTR + ADR) = VAL
   10 CONTINUE
      RETURN
      END
 
  
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
  
      SUBROUTINE CONVRT
      implicit none
C
C  *** CONVERTS TABLED VALUES TO ALOG(VAL)
C
! Define Passed parameters
!     NONE    

! Define COMMON variables
      COMMON /C4/ LENDIM, LENTAB, ATAB, ADIM
      INTEGER LENDIM, LENTAB, ADIM(10000)
      REAL    ATAB(10000)

! Define Local variables
      INTEGER LIMIT, CNTR
      REAL    ELEM

      ELEM = 0.0
      LIMIT = 0
      CNTR = 0


      LIMIT = LENTAB
C
      DO 10 CNTR = 1, LIMIT
         ELEM = ATAB(CNTR)
         IF (ELEM .NE. 0.0) THEN
            ATAB(CNTR) =  ALOG(ATAB(CNTR))
         ELSE
            ATAB(CNTR) = -1E38
         ENDIF
   10 CONTINUE
      RETURN
      END
 
  
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
  
       INTEGER FUNCTION IGETADDR(SS)
       implicit none


C  *** CALCULATES THE ADDRESS ASSOCIATED WITH SUBSCRIPT LIST FOUND AS THE
C  *** FIRST ADDRESS IN THE ARGUMENT LIST IN. ADDRESS IS RETURNED.
C
! Define Passed parameters
      INTEGER SS(1)

! Define COMMON variables
      COMMON /C4/ LENDIM, LENTAB, ATAB, ADIM
      INTEGER LENDIM, LENTAB, ADIM(10000)           
      REAL    ATAB(10000)
C
C
! Define Local variables
      INTEGER CNTR, ADR, LIMIT 


      LIMIT = 0
      ADR = 0
      CNTR = LENDIM

  100 CNTR = CNTR - 1
        IF (CNTR .LE. LIMIT) GOTO 200
        ADR = (ADR + SS(CNTR + 1)) * ADIM(CNTR)
      GOTO 100
      
  200 IGETADDR = ADR + SS(1)
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create fastclas.imake
/***********************************************************************
                     IMAKE FILE FOR PROGRAM fastclas
   To Create the build file give the command:
		$ vimake fastclas			(VMS)
   or
		% vimake fastclas			(Unix)
************************************************************************/
#define PROGRAM	fastclas
#define R2LIB
#define MODULE_LIST fastclas.f
#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST fortport
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create fastclas.pdf
process help=*
PARM INP     TYPE=STRING    COUNT=2:10
PARM OUT     TYPE=STRING
PARM SIZE    TYPE=INTEGER   COUNT=0:4                  DEFAULT=--
PARM SL      TYPE=INTEGER   COUNT=0:1                  DEFAULT=--
PARM SS      TYPE=INTEGER   COUNT=0:1                  DEFAULT=--
PARM NL      TYPE=INTEGER   COUNT=0:1                  DEFAULT=--
PARM NS      TYPE=INTEGER   COUNT=0:1                  DEFAULT=--
PARM MSS     TYPE=INTEGER   count=0:1                  DEFAULT=--
PARM USE     TYPE=INTEGER   COUNT=0:12                 DEFAULT=--
PARM BAND    TYPE=INTEGER   COUNT=0:12                 DEFAULT=--
PARM SIGMA   TYPE=REAL      COUNT=0:12                 DEFAULT=1.0
PARM CSIGMA  TYPE=REAL      COUNT=0:600		       DEFAULT=--
PARM DONT    TYPE=KEYWORD   COUNT=0:1    VALID=DONT    DEFAULT=--
PARM CHECK   TYPE=KEYWORD   COUNT=0:1    VALID=CHECK   DEFAULT=--
PARM PRIOR   TYPE=INTEGER   COUNT=0:24      	       DEFAULT=--
PARM PROB    TYPE=REAL      COUNT=0:600                DEFAULT=--
PARM MEAN    TYPE=REAL      COUNT=0:600                DEFAULT=--
 END-PROC
.TITLE
VICAR program FASTCLAS: parallelepiped/Bayesian multispectral classifier.
.HELP
PURPOSE
	FASTCLAS is a multispectral classifier using an algorithm which
combines the parallelepiped and Bayesian techniques.  Inputs are registered
multispectral data and training statistics from VICAR program STATS.  The
inputed multispectral data can be in either separate Vicar Data Sets or
in MSS format (See help on MSS).  FASTCLAS differs from an earlier version 
in that the program allows prior probabilities to be input in the parameter 
field and used in the Bayesian decision rule.  The input probabilities may 
consists of simple prior probabilities, derived from the expected magnitude 
of representaion of classes in the final image.  Multiple sets of 
probabilities may also be input, with one or two of the data channels serving 
not as multispectral values, but as indices to the appropriate set of prior 
probabilities.  Another feature of FASTCLAS allows the user to reset any 
class mean for any band, thus overriding the mean value provided by STATS.
.PAGE
OPERATION
	FASTCLAS uses a combination of the parallelepiped algorithm and the
Bayesian maximum likelihood algorithm for classifying multispectral data.
Assume that N spectral bands are available and training statistics from the
Vicar program STATS have been computed.  FASTCLAS reads the statistics data
set and generates a look-up table to hold the boundaries in the N-dimensional
decision space for each class.  For each dimension (band) the decision
boundary is  MU +/- (R * SIGMA), where MU is the mean for the class, SIGMA
the standard deviation and R the number of standard deviations to be used.
This is the parallelepiped algorithm.
 
	To be assigned to class J (DNout = J), a pixel's spectral signature
must fall within the N-dimensional decision boundary for class J.  If a
pixel's spectral signature falls outside the decision boundry for all
classes, the pixel is assigned to the unknown class (DNout = 0).  A pixel
whose spectral signature falls within the decision boundary for more than
one class is considered ambiguous.  The user has the option of resolving
the ambiguity by the Bayesian maximum likelihood algorithm, or leaving the
pixel ambiguous (DNout = 255).
.PAGE
	The Bayesian Algorithm performs as follows. First it assumes that 
there are N spectral bands, and considers a pixel as an N-dimensional
              _
sample vector X. Let K  be the covariance matrix computed for the training
            __        i
class i and MU  the mean of class i. The multi-variate probability P  that
_	      i    						    i
X is a member of class i is given by:
 
 	         1	            -1/2   _   __  T  -1      _   __
   P = ------------------------- * e    * (X - MU ) (K  )  * (X - MU )
    i         N/2          1/2              i    i    i        i
           2PI      *  |K |			   
			 i
.PAGE
where |K | = det(K )
        i         i
	But since we are only interested in the maximum P over all classes,
it is convenient to compute:
				_   __	 T  -1	   _   __
	Q = Log (P ) = C  - 1/2(X - MU )  (K  ) * (X - MU ) + Log (PROB )
	 i     e  i     i             i     i	         i       e     i
where:		    
		C = -1/2 (N * Log (2PI) + Log |K |
		 i               e	     e  i
_
X is then assigned to the class i, for which Q is a maximum.
 
	Thus for each pixel in the scene we assign a class number 
corresponding to the class to which the pixel most likely belongs.
.PAGE
	If the keyword CHECK is given, the Bayesian confidence value is
computed for each pixel after it is classified.  If the pixel's spectral
signature is outside the multivariate confidence interval, the pixel is
reclassified as unknown.
 
	The order in which the spectral bands are input to FASTCLAS will
influence the running time.  Since the table look-up portion uses a process 
of elimination, bands which give the best spectral separation between classes 
should be given first.  If the spectral data is in MSS format, the order is 
controlled by the USE parameter.
.PAGE
	If the parameters PROB and/or PRIOR have been coded, the Bayesian
decision rule uses prior probabilities in the final classification.  In the
simplest case, one set of prior probabilities is specified by the PROB
parameter.  One probability must be input for each class described in the
STATS file, and the probabilities must sum to 1.  The program checks to make
sure the appropriate number of probabilities are input, but does not check
to be sure that they sum to 1.
 
	If the PRIOR parameter has been coded, then the user inputs several
sets of probabilities, and the set used in the decision rule is determined
by the value in the band identified as a prior probability channel.  For
example if "PRIOR=(5,3)" is coded, band 5 will be taken as a prior probability
index channel, and will contain only DN values of 0 through 3.  Before each
pixel is classified, the 'DN sub 5' value will be checked.  If the 'DN sub 5'
equals 1, then the first set of prior probabilities input in the PROB
parameter will be used; if 'DN sub 5' equals 2, the second set of prior
probabilities will be used in the Bayesian decision rule, and so forth.  The
program also systematically samples the prior channel image and uses the input
probabilities to calculate, using Bayes' rule, a set of unconditional prior
probabilities which are applied when 'DN sub 5' equals 0.
 
	The PRIOR parameter can also specify two such channels.  In this case,
the program expects a set of probabilities for each possible DN value in each
prior probability channel.  With those probabilities as input, the program
calculates a full set of prior probabilities (under assumptions of indepen-
dence) which are doubly contingent on the indexes present in the two channels
for each pixel.  Zeros may be used freely throughout as index values.  When
a zero is encountered, the program assumes no information concerning that
channel is present, and reverts to a separately calculated set of slightly
contingent or uncontingent probabilities as appropriate.
.PAGE
EXAMPLES
 
1)	FASTCLAS INP=(A,B,C,ST) OUT=OUT SIZE=(1,1,500,500) SIGMA=2.5+
	 CSIGMA=(2,3,1.5) 'DONT
 
	This example classifies the multispectral imagery on data sets
A, B, and C according to the training statistics on data set ST. A 2.5
standard deviation confidence interval is used for each input band in
each class with the exception that band 3 of class 2 uses a 1.5 standard
deviation interval. The Bayesian routine is suppressed for resolving
ambiguity.
.PAGE
2)	FASTCLAS (MS,ST) OUT MSS=6 USE=(2,3,4,5) SIGMA=3.0  +
	 PROB=(0.125,0.137,0.029,0.414,0.295)		    +
	 MEAN=(3,4,144.0,3,5,168.0)
 
	Input data set MS contains 6 specral bands of imagery (MSS format)
with clasification to be performed using only bands 2, 3, 4 & 5. A 3.0
standard deviation confidence interval is used for each band in each class.
Prior probabilities are supplied for each of the classes identified in the
STATS file and will be used in the Bayesian decision rule. (i.e. Class 1 
probability is replaced with .125 in all bands, Class 2 is replaced with
0.137 in all bands, etc.) For class 3, the STATS means for bands 4 and 5 
are to be reset to 144.0 and 168.0 respectively.
 
NOTE: a 0 probability for a class doesn't zero it out unless there is 
also a 0 CSIGMA for it.
.PAGE
3)	FASTCLAS (MSPR2,ST) OUT MSS=5,USE=(4,2,3,1) 'CHECK PRIOR=(5,4)  +
	 PROB=(1,0.071,0.302,0.207,0.319,0.101,				+
	       2,0.271,0.313,0.092,0.107,0.271,				+
	       3,0.112,0.419,0.393,0.076,0.000,				+
	       4,0.2,0.2,0.2,0.4,0.0)
 
	The input data set consists of five bands in MSS format. Bands 
4, 2, 3, and 1 will be used in classification, with the parallelepiped
classifier using them in that order. The multivariate confidence interval 
will be checked. Band 5 is a prior probability index channel assuming
DN values 0-4, and four sets of prior probabilities are specified, each 
set summing to 1. The fact that five values are given for each set (1) 
implies that the STATS file describes exactly five classes and (2) 
associates the first probability values with the first class in the STATS
file, the second value with the second class, etc. (i.e. Prob of class 1
occurring in level 1 of band 5 is 0.071, prob of class 2 occuring in level 1
of band 5 is 0.302, etc.)
.PAGE
4)	FASTCLAS (MSPR2,ST) OUT MSS=6 USE=(1,2,3,4) SIGMA=3.0   +
	 PRIOR=(5,3,6,2) 					+
	 PROB=(5,1,0.017,0.249,0.301,0.433,			+
	       5,2,0.321,0.230,0.409,0.040,			+
	       5,3,0.519,0.107,0.218,0.156,			+
	       6,1,0.213,0.414,0.021,0.352,			+
	       6,2,0.107,0.318,0.052,0.477)
 
	The input dataset is six bands in MSS format. Bands 1-4 are
multispectral, band 5 is a prior probability index band with DN values
ranging from 0-3, and band 6 is also a prior probability index band with 
DN values ranging from 0-2. Prior probabilities in each set sum to 1, and 
the fact that there are four values in each set implies that the STATS 
file describes exactly four classes.
.PAGE
TIMING
	The runnning of FASTCLAS is a function of the picture size, number
of spectral bands, number of possible classes and size of confidence
intervals desired (SIGMA'S).  In addition, running time is data dependent;
that is, it varies depending on the number of times ambiquity must be
resolved.  Therefore it is difficult to estimate the running time accurately.
 
WRITTEN BY: J. D. Addington & A.H. Strahler	Oct. 23, 1984
CONVERTED TO VAX BY: Helen De Rueda		March 8, 1984
COGNIZANT PROGRAMMER:  R. E. Alley
Revisions:
         Made Portable for UNIX ... J. Turner (CRI) Jan 02, 1995
.LEVEL1
.vari INP
input data sets
first, image dataset(s)
last, statistics dataset
.vari OUT
output data set
.vari SIZE
Vicar size field
.vari SL
Starting line of image
.vari SS
Starting sample of image
.vari NL
Number of line in image
.vari NS
Number of samples in image
.vari MSS
Specifies # of bands
.vari BAND
Which bands are stored
.vari USE
which bands are used
.vari SIGMA
Standard deviation 
multiplier for boundary
.vari CSIGMA
Standard Deviations multiplier
for Classes
.vari DONT
No Bayesian if ambiguous
.vari CHECK
Check multivariate confidence
.vari PRIOR
Band contains index values
.vari PROB
Denotes probabilities
.vari MEAN
Replaces STATS mean.
.LEVEL2
.vari INP
INP=(IN1,IN2,...,STAT) or INP=(IMSS,STAT)
Input data sets, where INn are single band inputs and IMSS is the input
in MSS format. The last data set MUST BE the output data set from STATS.
.vari OUT
OUT=OUT
Output data set containing classification map
.vari SIZE
SIZE=(SL,SS,NL,NS) where SL, SS, NL, NS are the starting line, 
starting sample, number of line, number of samples in the output file. It 
is a standard Vicar size field.
.vari SL
Starting line of image
.vari SS
Starting sample of image
.vari NL
Number of lines in image
.vari NS
Number of samples in image
.vari MSS
MSS=N 
Denotes that the input data set is in MSS format and contains N spectral
bands.  (Default is standard format)
.vari USE
USE=(I1,I2,...)
Used in conjunction with MSS; denotes which bands are to be used as input
to the classifier and their order of use.  (Default is to use all bands 
specified by MSS.)
.vari BAND
BAND=(I1,I2,...)
Denotes that statistics data set contains statistics of the specified
bands in the given order.  (Default is the same order as input data)
.vari SIGMA
SIGMA=(R1,R2,...)
Denotes that R1 standard deviations are used for the decision boundaries of
the first band used, all classes; R2 for the second, all classes; etc. The last
value specified is used for all remaining bands. (default=1.0)
.vari CSIGMA
CSIGMA=(I1,J1,X1,I2,J2,X2...)
Denotes that X1 standard deviations are to be used for class I1 and band J1. 
Jn = -1 denotes that all bands for class I1 will use X1.  The boundaries
specified by this parameter set override those specified by SIGMA.
J is the Jth band used, not necessarily band J if MSS and USE are specified.
.vari DONT
('DONT)
Denotes that the Bayesian algorithm is not involved to resolve ambiguity.
.vari CHECK
('CHECK)
Denotes that multivariate confidence boundary is checked by the Bayesian 
algorithm.
.vari PRIOR
PRIOR=(I1,I2,I3,I4,...)
Denotes that band I1 will contain not multispectral values, but index values
indicating which set or sets of prior probabilities are to be used in the
Bayesian decision rule. I2 indicates the maximum number of sets of prior
probabilities associated with band I1. Thus the coding PRIOR=(9,3,10,5)
would indicate that bands 9 and 10 contain indices to probability sets,
with the values in band 9 ranging from0-3, and the values in band 10 ranging
from 0-5.
.vari PROB
PROB=([I1],[J1],X1,[I2],[J2],X2,...)
Denotes that prior probabilities X1-XN, where N is the number of classes, 
are to be used in the Bayesian decision rule. If the PRIOR parameter has
specified one channel as an index to probability sets, then I1, I2,... are
required and indicate the subscript associated with the set. If PRIOR
has specified two channels, the I1,J1,...,I2,J2 are required. The J index 
specifies the band, and the I index specifies the class, to which the set 
of probabilities applies. Probability values of zero are permitted.
.vari MEAN
MEAN=(I1,J1,R1,I2,J2,R2,...)
Denotes that the STATS mean for Class I1 and band J1 is to be replaced by
the value R1.  J refers to the actual band number, not the Jth band used.
(This is different from the interpretation of band for CSIGMA and SIGMA.)
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstfastclas.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
!This is a test file for FASTCLAS
!Gen needed files
GEN A 10 10
GEN B 10 10 SINC=2 LINC=2
GEN C 10 10 SINC=4 LINC=4
GEN D 2 2
SIZE D DD (1,1,10,10)
MSS (A,B,C) FAST
MSS (A,B,C,DD) FAST1
!Run STATS to get statistics
STATS FAST STAT1 MSS=3 'NOPRINT CLASS1=(1,1,5,5) CLASS2=(1,5,5,5)+
 CLASS3=(5,1,5,5) CLASS4=(5,5,5,5)
!Run FASTCLAS (throw in CSIGMA and BAND for test)
FASTCLAS (FAST,STAT1) FASTOUT SIZE=(1,1,10,10) MSS=3 SIGMA=2.5 +
 CSIGMA=(2,3,1.5) BAND=(1,2,3)
!List FASTCLAS output
LIST FASTOUT
!Run FASTCLAS with PRIOR probabilities
FASTCLAS (FAST,STAT1) FASTOUT MSS=3 USE=(1,3) SIGMA=3.0+
 PROB=(.125,.237,.224,.414) MEAN=(3,2,144,3,3,168) 'CHECK
!List file. This output may not look correct, but it is. The output
!listed is due to noise and number truncations and also due to the
!highly correlated training sets.
LIST FASTOUT
!Run FASTCLAS with other PRIOR probabilities
FASTCLAS (FAST1,STAT1) FASTOUT MSS=4 USE=(1,2,3) SIGMA=3.0 PRIOR=(4,2) +
 PROB=(1,0.0,0.3,0.3,0.4,2,0.05,0.05,0.8,0.1) MEAN=(3,2,144,3,3,168) 
LIST FASTOUT
!Run FASTCLAS with check DONT parameter
FASTCLAS (A,B,C,STAT1) FASTOUT SIGMA=2.5 'DONT
!List FASTCLAS output
LIST FASTOUT
end-proc
$ Return
$!#############################################################################
