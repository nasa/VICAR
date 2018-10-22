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
ccc
	integer*4 maxclasses
	parameter (maxclasses = 100)
	integer*4 bands,ibis,iii,npix
	integer*4 nb,unit
	real*4 means(600),covariance(600)
ccc
      logical*4 xvptst,dont,check,pband
ccc      integer*4 mss						!MSS
      integer*4 pr,pb,mn
      integer*4 prio(24),line1(10),inpfil(10)
      integer*4  bnd(12)					!MSS
      integer*4 prior(12),pclim(12),dimlst(13), lcb(4),ncls
      integer*4 use(12)						!MSS
      integer*4  all(12), il, j, jj, op, nprb, npch
      integer*4 ni,cnt,nfiles,afiles,tfiles,status,ind
c	INTEGER*4 R2FBUF(12), F2RBUF(12)
      integer*4 ss,nl,nli,nss,nsi,icode
      integer*4 mtrx,nn,nm,ii,nni,itempnmi
      integer*4 jm,kk,nosets,nchek,ptr,i,k,iband
      integer*4 nbr,nbp,outfil,abb,many,infil,itemp

      real*4    sig,rmntb(3,200)
      real*4    work(12,12)
      real*4    rtemp,val,v,sigma(600),prob(600),work1(12,12)

	character*4 fmt(4)/'BYTE','HALF','FULL','REAL'/
	character*5 format
	character*8 org

ccc      BYTE    LLX
      character blank,comma

      character*27  lbl1
      character*62  pr1
      character*70  pr3
      character*96  pr4
      character*132 pr8
ccc      CHARACTER*21  PR0
      character*120 pr12
      character*87  pr13
      character*36  pr14
      character*49  pr15
      character*60  pr16

      character*59  pr17
      character*72  pr18
	character*8 cname(50)
C
      common /PP/ pplist
      real*4        pplist(maxclasses)

      integer*4 lendim, lentab, adim(10000)
      real*4 atab(10000)
      common /C4/ lendim, lentab, atab, adim

      integer*4     ns,ptrbuf(12)
      real*4        con(maxclasses)
      integer*2 vgood(maxclasses),good(maxclasses),low(maxclasses,12)
      integer*2 hgh(maxclasses,12)
      common /C1/ ns,con,ptrbuf,vgood,good,low,hgh

      integer*2   buffer(32000)
      common /C1A/ buffer

      integer*2 obuffer(32000)
      common /C1B/ obuffer

      real*4  mean(12,maxclasses), cov(78,maxclasses), xsig(12,maxclasses), clim(maxclasses)
      common /C2/ mean, cov, clim, xsig
 
c      COMMON /SIZE/ SL
      integer*4 sl

	character*15000 chbuf

C
      INCLUDE 'fortport'

C Local variable initialization
      data  lendim/0/, lentab/0/
      data use   /1,2,3,4,5,6,7,8,9,10,11,12/			!MSS
      data all   /1,2,3,4,5,6,7,8,9,10,11,12/ 
      data comma /','/
      data blank /' '/

! INTEGER variable initialization
      data ss/0/,nl/0/,ns/0/,nli/0/,nss/0/
      data  mtrx/0/,nn/0/
      data nm/0/,ii/0/,nni/0/,itempnmi/0/
      data jm/0/,kk/0/,nosets/0/,nchek/0/,ptr/0/,i/0/,k/0/,iband/0/
      data nbr/0/,
     &     nbp/0/,
     &     outfil/0/,
     &     abb/0/,
     &     many/0/,
     &     infil/0/,
     &     itemp/0/
      data ncls/0/,op/0/,nprb/0/,npch/0/,il/0/,j/0/
      data dont/.FALSE./,check/.FALSE./,pband/.FALSE./,pr/0/,pb/0/,mn/0/
ccc      data mss/0/						!MSS
      data pr8/' '/
      data lbl1 /'FASTCLAS CLASSIFICATION MAP'/
      data pr3/'*** CLASSIFICATION WILL USE BANDS'/
      data pr4/'*** NUMBER ST DEVS  +/-'/                                
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
      pr18(41:72) = ' VALUE SPECIFIED IN PRIOR PARAM'



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
ccc      call zia (R2FBUF,12)
      call zia (pplist,maxclasses)

! REAL variable initialization
      V   = 0.0
      SIG = 0.0
      call zia (xsig,12*maxclasses)
      call zia (clim,maxclasses)
      call zia (rmntb,3*200)
      call zia (mean,12*maxclasses)
      call zia (cov,78*maxclasses)
      call zia (con,maxclasses)
      call zia (work,12*12)
      call zia (work1,12*12)
      call zia (sigma,600)
      call zia (prob,600)

! BYTE variable initialization
      call zia (BUFFER,32000/2)			!now HALF
      call zia (obuffer,32000/2)		!now HALF
      call zia (low,maxclasses*12/2)			!now HALF
      call zia (hgh,maxclasses*12/2)			!now HALF
      call zia (good,maxclasses/2)			!now HALF
      call zia (vgood,maxclasses/2)			!now HALF
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C        PARAMETERS
C     'MSS',N       DENOTES INPUT HAS N BANDS IN MSS FORMAT.			!MSS
C     'BANDS',I,J,K INPUT STATS ARE FROM BANDS I,J,K ...			!MSS
C     'USE',I,J,K   CLASSIFICATION TO USE BANDS I,J,K ...			!MSS
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
      call ifmessage('fastclas - 26-JUN-2012 (64-bit) - RJB')
C
      call xveaction ('SA',' ')

C     Call xvtrans_set for REAL to FULL translation      
cc      call xvtrans_set (R2FBUF,'REAL','FULL',status)

C     Call xvtrans_set for FULL to REAL translation      
cc      call xvtrans_set (F2RBUF,'FULL','REAL',status)

      call xvpcnt('INP',ni)
      NFILES = NI - 1				!last data set is stats file
	AFILES = NI - 1

      call xvparm('PRIOR',prio,npch,pr,24)
c	NFILES = NFILES - 1
      PR = PR - 1
c	print *,"PR = ",pr
      IF (PR .NE. 0) THEN
         DO J = 1, NPCH, 2
            JJ = (J+1) / 2
            PRIOR(JJ) = PRIO(J)
            PCLIM(JJ) = PRIO(J+1)
         END DO
         NPCH = NPCH / 2			! NPCH = # prob. channels
	 NFILES = NFILES - NPCH
         NPRB = NPCH + 1			! NPRB = # specified probs.
      END IF
c	print *, 'here'
C               Open data sets
      call xvparm('SIZE',sl,cnt,status,1)		! Find value of SL
      IF (STATUS .EQ. 1) THEN
         call xvparm('SL',sl,cnt,status,1)
         IF (STATUS .EQ. 1) SL = 1
      END IF                                     

	call xvunit(inpfil(1),'INP',1,ind,' ')
	call xvsignal(inpfil(1),ind,.TRUE.)
	call xvopen(inpfil(1),ind,'OPEN_ACT','SA','IO_ACT','SA',' ')

	call xvget(inpfil(1),ind,'FORMAT',format,'ORG',org,' ')	
	icode = 0
	if (format.eq.'BYTE') icode=1
	if (format.eq.'HALF'.or.format.eq.'WORD') icode=2
	if (format.eq.'FULL') icode=3
	if (format.eq.'REAL') icode=4
	if (icode.eq.0) then
		call xvmessage('??E - Unknown data format for input image',' ')
		call abend  
	elseif (icode.eq.1) then
		call xvmessage('INPUT DATA IS BYTE FORMAT',' ')
	elseif (icode.eq.2) then
		call xvmessage('INPUT DATA IS HALF FORMAT',' ')
	elseif (icode.eq.3 .or. icode.eq.4) then
	      call xvmessage('??E - Only BYTE and HALF images supported',' ')
              call abend
	endif
	
	call xvclose(INPFIL(1),ind,' ')

	 DO I = 1, (NFILES + NPCH)      ! # image files + # prob files
	    call xvunit(inpfil(i),'INP',i,ind,' ')
	    call xvopen(inpfil(i),ind,'OPEN_ACT','SA','IO_ACT','SA',
     &		'I_FORMAT',fmt(icode),'U_FORMAT',fmt(2),' ')
	    LINE1(I) = SL
	 END DO
      	 do i = 1, nfiles
	    BND(i) = i 						!MSS
	 enddo

C		Decide which bands and files to use in analysis
      call xvsize(sl,ss,nl,ns,nli,nss)

cc      call xvparm('USE',use,cnt,status,12)			!MSS
cc      if (status .eq. 0) then
cc	 nfiles = cnt
cc      endif
      DO J = 1, NFILES
         BND(J) = USE(J)					!MSS
      END DO
	 NSI = NSS
      dont = xvptst('DONT')
      IF (DONT)  CHECK = .FALSE.
C                Size error checking
      IF (SL+NL-1 .GT. NLI)  GO TO 910
      IF (SS+NS-1 .GT. NSI)  GO TO 910
c      IF (NS*(NFILES+NPCH) .GT. 15000)  GO TO 920
	if (ns .gt. 32000) go to 920
C
C
C        Read STAT data set, LAST entry in INP, load means and LL Triangle of COV matrix
      call xvunit(unit,'INP',ni,status,' ')
 	if (status .lt. 0) call xvsignal(unit, status, 1)

	call istat_file_open(unit,'read',0,0,0,status) 
      if (status .lt. 0) call istat_signal(unit, status, 1) 
      ! Get file information 
      call istat_file_Info(unit, ncls, bands, ibis) 
ccc==new
cc	print *,'classes = ',ncls,' bands = ',bands
	MTRX = (BANDS * (BANDS + 1)) / 2             !!!number of cov elements in LL trianglar matrix
      NN = 4 * NFILES			!what is this for?
      NM = 4 * MTRX			!what is thsi for?
c	print *,'mtrx = ', mtrx
ccc==new
C     READ A RECORD, GET THE MEAN AND THE COVARIANCE,
C	MATRIX DIAGONAL IS SIGMA SQUARED (VARIANCE)
      DO III = 1, ncls 

         call istat_record_read (unit, iii, cname(iii), npix,
     +	                         nb,means,covariance,status) 
         if (status .lt. 0) call istat_signal(unit,status,1) 
cc MEAN(FILE,CLASS) COV(FILE.CLASS) [LL Triangular]
c  cov is same down all columns
	   do ii=1,nb
	      mean(ii,iii) = means(ii)
	   enddo
	   do ii=1,mtrx
	      cov(ii,iii) = covariance(ii)
	   enddo
cccc	   print *,'row = ',iii,' classname = ',cname(iii),' npix = ',npix
cccc	   print *,'nb = ',nb
cccc	   print *,'means =',(mean(i,iii),i=1,nb)
cccc	   print *,'covariance = ',(cov(i,iii),i=1,mtrx)

	enddo	!DO III = 1, classes
C     CLOSE INPUT DATA SET
      call xvclose(unit,status,' ')



c--------------------------------------------------
C
C                 Adjust means as input by user
c     (an entry consists of 3 values, Class, Band, and NEW MEAN)
c    JM is total number of entries	
      call xvparm('MEAN',rmntb,jm,mn,3*200)
      MN = MN - 1
      IF (MN .NE. 0) THEN
         IF (MOD(JM,3) .NE. 0) THEN
	    call xvmessage('??E - Mean values must be a multiple of 3',' ')
	    call abend
         END IF
         JM = JM / 3
         DO KK = 1, JM
           MEAN(INT(RMNTB(2,KK)),INT(RMNTB(1,KK))) = RMNTB(3,KK)
         END DO
      END IF
C
C        Set high and low values according to SIGMAS or CSIGMAS
C	SIGMA here is std dev confidence interval, XSIG, .i.e., 3 sigma, etc
      call xvparm('SIGMA',sigma,cnt,status,600)     ! Get SIGMA values
      IF (STATUS .EQ. 1) CNT = 1
      DO I = 1, CNT
         DO J = 1, maxclasses			!100
	   XSIG(I,J) = SIGMA(I)
         END DO
      END DO
c	reflect around diagonal
      DO I = (CNT+1), NFILES
         DO J = 1, maxclasses			!100
	    XSIG(I,J) = SIGMA(CNT)
         END DO
      END DO
C    Reuse SIGMA FOR CSIGMA (CLASS SIGMA), i.e., a different sigma for
C	each class (an entry consists of 3 values, Class, Band, and NEW SIGMA)
      call xvparm('CSIGMA',sigma,cnt,status,600)    ! Get CSIGMA values
      IF (STATUS .EQ. 0) THEN
	 OP = 1
	 IF (MOD(CNT,3) .NE. 0) THEN
	    call mabend('??E - CSIGMA values must be a multiple of 3')
	 END IF
	 DO I = 1, CNT, 3
c	    print *,'sigma(i) = ',sigma(i)
c            call xvtrans(r2fbuf,sigma(I),II,1)
c	    print *,'ii = ',ii
c	    print *,'sigma(i+1) = ',sigma(i+1)
c            call xvtrans(r2fbuf,sigma(I+1),J,1)
c	    print *,'j = ',j
	    ii = int(sigma(i))		!Class
	    j = int(sigma(i+1))		!Band
c		print *,'ii,j = ',ii,j 
	    IF (J .EQ. -1) THEN		! A -1 entry in Band means use sigma against all bands within the class
	       DO JJ = 1, NFILES
		 XSIG(JJ,II) = SIGMA(I+2)	!Sigma is for all bands in class 
	       END DO
	    ELSE
		 XSIG(J,II) = SIGMA(I+2)	!Sigma is for indicated band in the class
	    END IF
	 END DO
      END IF
c	Default XSIG(FILE,CLASS) = 1.0
      DO II = 1, NCLS			! determine high and low values
		TFILES=AFILES
         DO J = 1, TFILES
            K = J			!K = BND(J)		!MSS 
            JJ = (K * (K + 1)) / 2			!LL Triang MATRIX
            SIG = SQRT(COV(JJ,II)) * XSIG(J,II)
c	print *,"SIGMA for File ",j," Class ",ii," is ",sig," cov = ",COV(JJ,II)," XSIG = ",XSIG(J,II)
c	if (SIG .eq. 0.0) print *,"SIGMA for File ",j," Class ",ii," is 0.0"
C
C NOTE THAT XSIG(J) IS USED WITH COV(BND(J)), THE BAND # IN CSIGMA
C REFERS TO THE ORDER OF USE
C
c   compute lower bound 
            RTEMP = MEAN(K,II) - SIG + 0.5
	    il=int(rtemp)
c           CALL xvtrans(R2FBUF,RTEMP,IL,1)
c	print *,'rtemp = ',rtemp
c	print *,'il = ',il
ccc            LLX = INT2BYTE(IL)
c	print *,'ll = ',ll
            IF (IL .LT. 0)  THEN
               IL = 0
ccc               LLX = INT2BYTE(IL)
            ENDIF
            LOW(II,J) = IL		! = LLX		!GET COMPUTED LOW LIMIT FOR CLASS
c  compute upper bound based on sigma 
            RTEMP = MEAN(K,II) + SIG + 0.5
	    il=int(rtemp)
c            CALL xvtrans(R2FBUF,RTEMP,IL,1)
ccc            LLX = INT2BYTE (IL)
ccccc            IF (IL .GT. 255)  THEN
cccc               IL = 255
ccc               LLX = INT2BYTE(IL)
ccc            ENDIF
            HGH(II,J) = IL				!GET COMPUTED HIGH LIMIT FOR CLASS
c	  print *, "K (File), II (Class), MEAN(K,II) , SIG, HGH(II,J), LOW(II,J) = ",
cc	1 K, II, MEAN(K,II), SIG, HGH(II,J), LOW(II,J)
         END DO
      END DO

cccc	print *,'here4'
C
C            Set up dimlst and call DIM for cond. prob. table
      call xvparm('PROB',prob,cnt,pb,600)
      PB = PB - 1

      IF (PB .NE. 0) THEN
         IF (PR .EQ. 0) NPRB = 1		!if PRIOR = 0
      END IF

      IF (NPRB .EQ. 0) GO TO 254		!BR if no PROB ENTERED
 
      DIMLST(1) = NCLS

      IF (NPRB - 1) 600,600,610

600   CONTINUE
      call init(dimlst,1)
      GO TO 618

610   CONTINUE
      DO I = 2, NPRB
         DIMLST(I) = PCLIM(I-1)
      END DO
      call init(dimlst,nprb)
C
C            Read PROB values
618   continue

cccc	print *,'here5'

c	print *,'cnt = ',cnt,' ncls = ',ncls,' npch = ',npch
      NOSETS = CNT / (NCLS + NPCH)
c	print *,'here5aa'
      NCHEK = NOSETS * (NCLS + NPCH)
      PTR = 1
c	print *,'here5ab'
      IF (CNT .NE. NCHEK) GO TO 940
      DIMLST(1) = 1
c	print *,'here5ac'
      IF (NPRB - 2) 620,630,640

620   CONTINUE
      call setlst(dimlst,prob(ptr),ncls)

c	print *,'here5a'
      DIMLST(1) = 0
      call set(dimlst,1.)
      call convrt
      GO TO 254
630   continue
c	print *,'here5b'
      DO I = 1, NOSETS
c         call xvtrans (R2FBUF,PROB(PTR),DIMLST(2),1)
	  dimlst(2) = int(prob(ptr))
         call setlst(dimlst,prob(ptr+1),ncls)
         PTR = PTR + NCLS + 1
      END DO
      GO TO 254
640   continue
c	print *,'here5c'
      DO I = 1, NOSETS
         DO J = 2, 13
            DIMLST(J) = 0
         END DO
c         call xvtrans (R2FBUF,PROB(PTR),IBAND,1)
	 iband = int(prob(ptr))
         DO J = 1, 12
            IF(PRIOR(J).EQ.IBAND) GO TO 649
         END DO
         GO TO 930
649      continue
c	print *,'here5d'
c         call xvtrans (R2FBUF,PROB(PTR+1),DIMLST(J+1),1)
	dimlst(j+1) = int(prob(ptr+1))
         call setlst(dimlst,prob(ptr+2),ncls)
         PTR = PTR + NCLS + 2
      END DO
254   CONTINUE
C	Come here if no PROB enterted
C
C        CHECK BAND ORDER
c	print *,'here6'
      DO J = 1, NFILES
         IF (USE(J) .NE. J)  THEN			!MSS
            PBAND = .TRUE.
         END IF
      END DO

	IF (NFILES .NE. BANDS)  THEN      !IF (NFILES .NE. NFETIN)  THEN
         PBAND = .TRUE.
      END IF
C
C        PRINT SUMMARY
cccc	print *,"printing PR Buffers"
      WRITE (PR1,9900) NCLS,BANDS
9900  FORMAT ('*** STATISTICS CONTAIN',I4,' CLASSES AND',I4,
     +' SPECTRAL BANDS ***')
      call xvmessage(pr1,' ')
      IF (.NOT. PBAND) THEN				!MSS
        call xvmessage('*** CLASSIFICATION WILL USE ALL BANDS ***',' ')
      ELSE
         PTR = 35
         DO J = 1,NFILES
            PR3(PTR+1:PTR+1) = ','
            WRITE (PR3(PTR-1:PTR),'(I2)') USE(J)
            PTR = PTR + 3
         END DO
         call xvmessage(pr3,' ')
      END IF
      IF (OP .LE. 0) THEN			! SIGMA's or CSIGMA's used
         PTR = 30
         DO J = 1, NFILES
            WRITE (PR4(PTR-5:PTR),'(F6.1)') XSIG(J,1)
            PTR = PTR + 6
         END DO
         call xvmessage(pr4,' ')
      ELSE
         call xvmessage(pr4,' ')
         PR8(1:12) = '        BAND'
         PTR = 18
         DO J = 1, NFILES
            WRITE (PR8(PTR-3:PTR),'(I4)') USE(J)
            PTR = PTR + 6
         END DO
         call xvmessage(pr8,' ')
	 call xvmessage('  CLASS',' ')
         pr8 = ' '
         DO I = 1, NCLS
            WRITE (PR8(4:7),'(I4)') I
            PTR = 19
            DO J = 1, NFILES
               WRITE (PR8(PTR-5:PTR),'(F6.1)') XSIG(J,I)
               PTR = PTR + 6
            END DO
            call xvmessage(pr8,' ')
         END DO
         call xvmessage(' ',' ')
      END IF


      if (dont) call xvmessage (pr14,' ')		!*** BAYESIAN SEPARATION NOT USED ***
      if (check) call xvmessage(pr15,' ')		!** Multi-variate confidence interval checked ***
c       print *, "MN = ",mn
      IF (MN .EQ. 0) GO TO 298		! Means
c	get input means
      call xvmessage(pr16,' ')				!*** Means for bands and classes will be set as indicated ***'
      PTR = 0
c
c	print *,"printing CHBUF, JM = ",jm
      DO JJ = 1, JM
         WRITE (CHBUF((PTR+1):(PTR+3)),'(I3)') INT(RMNTB(1,JJ))
         WRITE (CHBUF((PTR+4):(PTR+7)),'(I4)') INT(RMNTB(2,JJ))
c         call xvtrans (r2fbuf,rmntb(3,jj),itemp,1)
	 itemp=int(rmntb(3,jj))
         WRITE (CHBUF((PTR+8):(PTR+13)),'(I6)') itemp
         buffer(ptr+14) = ICHAR(' ')
         buffer(ptr+15) = ICHAR(' ')
         PTR = PTR + 15
      END DO
c	CLS BND  MEAN  CLS BND  MEAN
c	  3   2   144    3   3   168
c
      call xvmessage(pr12(1:ptr),' ')			!CLS BND  MEAN  CLS BND  MEAN 
      call xvmessage(chbuf(1:ptr),' ')
C
cccc	print *,'here7'
298   continue
cccc	print *, 'after 298'
      if (pr .NE. 0) THEN             	! PROBS used
         DO J = 1, NPCH
            JJ = 55 + J * 3


            WRITE (PR13(JJ-2:JJ),'(I2)') PRIOR(J)
         END DO
         call xvmessage(pr13,' ')
      END IF
      IF (PB .NE. 0) 					! PRIOR probs used
     +   call xvmessage('*** PRIOR PROBABILITIES WILL BE USED ***',' ')
C
C           UPDATE AND ADD NEW LABEL
      call xvunit(outfil,'OUT',1,status,' ')
	call xvopen(outfil,status,'OP','WRITE', 'O_FORMAT',fmt(2),'U_FORMAT',fmt(2),
     1  'U_NS',ns,' ')
      call xladd(outfil,'HISTORY','COMMENT',lbl1,status,'ULEN',27,
     +          'FORMAT','STRING',' ')
      ABB = SS - 1
cccc	print *,"DONT = ",DONT
      if (.not. dont)  call covin(ncls,nfiles,bnd,work,work1)	!CALL COVIN(NCLS,NFETIN,NFILES,BND,WORK,work1)
C
cccc	print *,'here8' 
C        LOAD BUFFERS
      DO J = 1, NCLS
         VGOOD(J) = J 		!VGOOD(J) = INT2BYTE(J)
      END DO
c	print *,"USE buffer = ",USE(1),USE(2),USE(3),USE(4),USE(5),USE(6)
      DO J = 1, NFILES   			! determine SL within image file(s)
            PTRBUF(J) = NS * (USE(J) - 1)		!MSS
      END DO
      IF (NPCH .NE. 0) THEN		! do the same for prior prob files
         DO J = 1, NPCH
                PTRBUF(NFILES+J) = NS * (PRIOR(J) - 1)
         END DO
      END IF
cccc	print *,"CALL TBLSET(NCLS"
      call tblset(ncls)
      MANY = 0
C
C
C        COMPUTE MULTI-VARIATE CONFIDENCE LIMITS
cccc	       print *,"CHECK = ",CHECK

      IF (CHECK) THEN
         DO I = 1, NCLS
            SIG = 0.
            DO J = 1, NFILES
               SIG = SIG + XSIG(J,I)
            END DO
            CLIM(I) = SQRT(0.5) * SIG / NFILES
         END DO
      END IF
cccc	print *,"NPCH  = ",NPCH 
      IF (NPCH .EQ. 0) GO TO 500
C
C FIND JOINT DISTRIBUTION OF PRIOR PROBABILITY CHANNELS
      DIMLST(1) = 0
      NNI = (NL - 1) / 5 + 1
      DO II = 1, NNI
            DO J = 1, NPCH
               PTR = PTRBUF(NFILES+J) + 1
               JJ = PRIOR(J)
               call xvread(inpfil(jj),buffer(ptr),status,'LINE',line1(jj),
     +                     'SAMP',abb,'NSAMPS',ns,' ')
               LINE1(JJ) = LINE1(JJ) + 5
            END DO
         DO J = 1, NS
            DO JJ = 2, NPRB
               IL  = BUFFER(PTRBUF(NFILES+JJ-1)+J)
c               IL = BYTE2INT(LLX)              
               IF (IL .GT. PCLIM(NPRB-1)) GO TO 950
               DIMLST(JJ) = IL
            END DO
            V = VAL(DIMLST)+1.0
            call set(dimlst,v)
         END DO
      END DO
      call cprob(pclim,nprb,ncls)
      JJ = NNI * 5
         DO J = 1, NPCH
            LINE1(PRIOR(J)) = LINE1(PRIOR(J)) - JJ
         END DO


C
C
C        START CLASSIFICATION
500	continue
cccc	print *,'here9 - START CLASSIFICATION'
c        print *,"ALL buffer = ",ALL(1),ALL(2),ALL(3),ALL(4),ALL(5),ALL(6)
      DO II = 1, NL

            DO J = 1, AFILES
c               K = BND(J)
		K = J					!K = BND(J)	!MSS
c               PTR = PTRBUF(J) + 1
		PTR = NS * (ALL(J) - 1) + 1
CCC		print *, "NL, J, PTR = ",NL,J,PTR
               call xvread(inpfil(j),buffer(ptr),status,'LINE',line1(j),
     +                    'SAMP',ss,'NSAMPS',ns,' ')
CCC		print *,"BUFFER(PTR) = ",BUFFER(PTR),BUFFER(PTR+1),BUFFER(PTR+2)
               LINE1(J) = 0
            END DO
ccc         END IF

ccc	print *,"BUF(1-10) = ",BUF(1),BUF(2),BUF(3),BUF(4),BUF(5),BUF(6),BUF(7),BUF(8),BUF(9),BUF(10)
ccc	print *,"BUF(11-20) = ",BUF(11),BUF(12),BUF(13),BUF(14),BUF(15),BUF(16),BUF(17),BUF(18),BUF(19),BUF(20)
ccc	print *,"BUF(21-30) = ",BUF(21),BUF(22),BUF(23),BUF(24),BUF(25),BUF(26),BUF(27),BUF(28),BUF(29),BUF(30)
C
C             CLASSIFY THE LINE
cccc	print *,"CALL LOOKUP(NFILES,NCLS,DONT,CHECK,MANY,NPRB) = ",NFILES,NCLS,DONT,CHECK,MANY,NPRB
         call lookup(nfiles,ncls,dont,check,many,nprb)
c	print *,"II 

         call xvwrit(outfil,obuffer,status,'NSAMPS',ns,' ')
      END DO
C
cccc	print *,'here10'
      WRITE (PR8,9920) MANY
9920  FORMAT ('... BAYESIAN ROUTINE CALLED ',I6,' TIMES ...')
      if (.not. dont) call xvmessage (pr8,' ')

      CALL xvclose(outfil,status,' ')
      RETURN
C
C
910   call mabend('??E - Area exceeds input picture size ...')
920   call mabend('??E -  NS Greater than 32000 ...')
930   call xvmessage(pr17,' ')
940   call mabend('??E - PROB parm has bad number of values')
950   call mabend(pr18,' ')
      return
      end
C
C
C
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      subroutine covin (nc,nv,bnd,r,rr)

      implicit none

! Define Passed parameters
	integer*4 nc,nv
	integer*4 bnd(12)					!MSS
	real*4    r(nv,nv),rr(nv,nv)

! Define COMMON variables
        integer*4 maxclasses
        parameter (maxclasses = 100)
	integer*4   ns,ptrbuf(12)
      real*4      con(maxclasses)
      integer*2 vgood(maxclasses),good(maxclasses),low(maxclasses,12)
      integer*2 hgh(maxclasses,12)
      common /C1/ ns,con,ptrbuf,vgood,good,low,hgh

      integer*2   buffer(32000)
      common /C1A/ buffer

      integer*2 obuffer(32000)
      common /C1B/ obuffer

      real*4  mean(12,maxclasses), cov(78,maxclasses), xsig(12*maxclasses), clim(maxclasses)
      common /C2/ mean, cov, clim, xsig

! Define Local variables
      real*4 g(12,12), lg2pi
      real*4 det, vmean(12)

      integer*4 lw(12,12), mw(12)
      integer*4 i,j,jj,jk,l,jm,lm

C  Initialize local variables
      call zia (G,12*12)
      LG2PI = 0.0
      DET   = 0.0
      call zia (vmean,12)
 
      call zia (lw,12*12)
      call zia (mw,12)
      I  = 0
      J  = 0
      JJ = 0
      JK = 0
      L  = 0
      JM = 0
      LM = 0 
C

      LG2PI = -0.5 * FLOAT(NV) * ALOG(6.283185)			!ALOG(pi)
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
            JM = J					!JM = BND(J)   !MSS
            VMEAN(J) = MEAN(JM,I)
            DO L = 1, NV
               LM = L					!LM = BND(L)   !MSS
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
         call invert(r,nv,det,lw,mw)
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
      integer*4 l(*),m(*)
      integer*4 n
      real*4    a(n,n),d

! Define local variables
      real*4    lgval,temp
      integer*4 i,j,k

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
      subroutine cprob(pclim,nprb,ncls)
C 
	implicit none
! Define Passed parameters
      integer*4 pclim(12),nprb,ncls

! Define Local variables
      integer*4 ss(3),i1,i2,i3,i,iter,k,l1,l2,l3,j
ccc      INTEGER*4 R2FBUF(12)
ccc      INTEGER*4 F2RBUF(12)
      real*4    grand,maxdim(3),sum,v,f
	real*4 val		!external function

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
ccc      CALL ZIA (R2FBUF,12)
ccc      CALL ZIA (F2RBUF,12)


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
         call set(ss,v)
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
            call set(ss,v)
         END DO
      END DO
C
C SET 0,0 ELEMENT, CONVRT, AND RETURN
C
      SS(1) = 0
      SS(2) = 0
      call set(ss,1.0)
      call convrt
      return
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
      call set(ss,grand)
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
            call set(ss,v)
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
            call set(ss,v)
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
            call set(ss,v)
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
               call set(ss,1.)
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
                           call set(ss,v)
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
         call set(ss,sum)
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
               call set(ss,v)
  440 CONTINUE
      call convrt
      return
      end
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
!
! 
      subroutine fbayes(nv,nc,ii,check,nprb,ncls)
      implicit none
      INCLUDE 'fortport'
C
C  *** BAYESIAN MAXIMUM LIKELIHOOD ALGORITHM
C
! Define Passed parameters
      integer*4 nv, nc, ii, nprb, ncls
      logical*4 check 
! Define COMMON variables
        integer*4 maxclasses
        parameter (maxclasses = 100)
      integer*4     ns,ptrbuf(12)
      real*4        con(maxclasses)
      integer*2 vgood(maxclasses),good(maxclasses),low(maxclasses,12)
      integer*2 hgh(maxclasses,12)
      common /C1/ ns, con, ptrbuf, vgood, good, low, hgh

      integer*2   buffer(32000)
      common /C1A/ buffer

      integer*2 obuffer(32000)
      common /C1B/ obuffer

      real*4  mean(12*maxclasses), cov(78*maxclasses), xsig(12*maxclasses), clim(maxclasses)
      common /C2/ mean, cov, clim, xsig

      integer*4     table(maxclasses), table2(maxclasses)
      common /C3/ table, table2

      real*4        pplist(maxclasses)
      common /PP/ pplist

!@!@       EQUIVALENCE (R7B,R7I), (CLNUMB,CLNUM)
!@!@       EQUIVALENCE (SSVAL,SSVALB)

! Define data translation buffers
ccc      INTEGER*4 R2FBUF(12)
ccc      INTEGER*4 F2RBUF(12)

! Define Local variables
c      BYTE     R7B, CLNUMB, SSVALB
      integer*4  ss(13)
      integer*4  r7i, lpdex, temp, limit, ctr, ptrval
      integer*4  clcntr, clnum, ic
ccc	integer*4 STATUS
      integer*4  class, clcov, ptr, lp1, lp2, lc, ssval
      real*4     val2, tmax, tsum, sum, data(10000)

!  Initialize local variables
      data ss/1,0,0,0,0,0,0,0,0,0,0,0,0/
c      DATA R7B/0/, CLNUMB/0/, SSVALB/0/
      data r7i/0/, lpdex/0/, temp/0/, limit/0/, ctr/0/, ptrval/0/
      data clcntr/0/, clnum/0/


      data class/0/, clcov/0/, ptr/0/, lp1/0/, lp2/0/, lc/0/, ssval/0/


      data val2/0.0/, tmax/0.0/, tsum/0.0/, sum/0.0/

	IC = 0
      call zia (data,10000)
ccc      CALL ZIA (R2FBUF,12)
ccc      CALL ZIA (F2RBUF,12)

!!    
C  Call xvtrans_set for REAL to FULL and FULL to REAL translations
ccc      call xvtrans_set (R2FBUF,'REAL','FULL',status)
ccc      call xvtrans_set (F2RBUF,'FULL','REAL',status)

C  *** DATA(I) = DN VAL FOR BAND I
      DO LPDEX = 1, NV
         TEMP = II + PTRBUF(LPDEX)
         R7I = BUFFER(TEMP)
ccc         R7I = BYTE2INT(R7B)       ! Translate byte to int
ccc         call xvtrans (F2RBUF, R7I, DATA(LPDEX), 1)
	 data(lpdex) = float(R7I)
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
         SSVAL  = BUFFER(PTRVAL)
ccc         SSVAL = BYTE2INT (SSVALB)
         SS(CTR+1) = SSVAL
      END DO
C
C  *** GET PROBABILITIES
   60 continue
      call getlst(ss,pplist,ncls)
      IF (NPRB .NE. 1) GOTO 70
      NPRB = 0
C
C  *** CALCULATE THE LIKELIHOOD VALUES, CHOOSE MAXIMUM
   70 continue
      CLCNTR = 1
      CLNUM = 0
      TMAX = -1.0E38
  120 continue
      CLNUM = GOOD(CLCNTR)
c      CLNUM =  BYTE2INT (CLNUMB)
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
      OBUFFER(II) = IC				!INT2BYTE(IC)
C
C  *** IF CHECK OPTION INVOKED 
      IF (CHECK) THEN			!IF (CHECK .NE. 0) THE
         VAL2 = SQRT(ABS(CON(IC) - TMAX))
         IF (VAL2 .GT. CLIM(IC)) OBUFFER(II)=0
      ENDIF
      return
      end
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      subroutine tblset(ncls)
      implicit none
C
C *** ROUTINE TO SET UP THE INDICES INTO THE TABLES
C
! Define Passed parameters
      integer*4  ncls

! Define COMMON variables
        integer*4 maxclasses
        parameter (maxclasses = 100)

      common /C3/ table, table2
      integer*4  table(maxclasses), table2(maxclasses)

! Define Local variables
      integer*4 index, count, value

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
      subroutine lookup(nfiles, ncls, dont, check, many, nprb)

      implicit none

      INCLUDE 'fortport'

! Define Passed parameters
      integer*4 nfiles, ncls, many, nprb
      logical*4 dont,check

! Define COMMON variables
        integer*4 maxclasses
        parameter (maxclasses = 100)
      integer*4     ns, ptrbuf(12)
      real*4        con(maxclasses)
      integer*2 vgood(maxclasses),good(maxclasses),low(maxclasses,12),hgh(maxclasses,12)
      common /C1/ ns, con, ptrbuf, vgood, good, low, hgh

      integer*2   buffer(32000)
      common /C1A/ buffer

      integer*2 obuffer(32000)
      common /C1B/ obuffer
c	COV( 78 * maxclasses)
      real*4  mean(12*maxclasses), cov(78*maxclasses), xsig(12*maxclasses), clim(maxclasses)
      common /C2/ mean, cov, clim, xsig

      integer*4 lendim, lentab, adim(10000)
      real*4 atab(10000)
      common /C4/ lendim, lentab, atab, adim

! Define Local variables
ccc     BYTE    CLCNTRB
cc      BYTE    IB, VALB, DNVALB, LOWLIMB, UPPERLIMB, GDCLSB
cc	BYTE    VALB, DNVALB, LOWLIMB, UPPERLIMB, GDCLSB 
ccc	BYTE VALB
      integer*4 addr(12)
      integer*4 cllpind,lpind, lpinc, clinc, j, i, clcntr, dnval, clcount
      integer*4 tabadr, bufptr, gdcls, lowlim
      integer*4 ngood, j1, upperlim, val

! Initialize local variables
      data  addr/0,100,200,300,400,500,600,700,800,900,1000,1100/
c      DATA  CLCNTRB/0/,DNVALB/0/, LOWLIMB/0/, UPPERLIMB/0/
c      DATA  GDCLSB/0/
      data  cllpind/0/,lpind/0/, lpinc/0/, clinc/0/, j/0/
      data  i/0/, clcntr/0/, dnval/0/, clcount/0/
      data  tabadr/0/, bufptr/0/, gdcls/0/, lowlim/0/
      data  ngood/0/, j1/0/, upperlim/0/,val/0/
C
c	print *,"SUBROUTINE LOOKUP(NFILES, NCLS, DONT, CHECK, MANY, NPRB) = ",
c	1 NFILES, NCLS, DONT, CHECK, MANY, NPRB                   
ccc--	print *,"LOOKUP - ns, ncls, nfiles = ",ns,ncls,nfiles
      DO 450 J = 1, NS			! J IS THE SAMPLE COUNT
C        
C *** MAKE ALL CLASSES VALID CLASSIFICATIONS
         DO I = 1, NCLS
ccc            IB = BYTE2INT(I)
            GOOD(I) = I
         END DO

         CLCNTR = NCLS
         DNVAL = 0
         GDCLS = 0
ccc--	print *,"NS, CLCNTR  = ",J ,CLCNTR
C
C  *** DETERMINE VALID CLASSES ACCORDING TO THRESHOLD VALUES
C  *** CLCNTR IS THE NUMBER OF GOOD CLASSES
         DO 180 LPIND = 1, NFILES
ccc---		print *,"LPIND = 1, NFILES = ",LPIND
            BUFPTR = PTRBUF(LPIND) + J			!find ss of class in file
            DNVAL  = BUFFER(BUFPTR)			! in our cuurent line
ccc---		 print *,"LPIND, DNVAL = ",LPIND,DNVAL
ccc            DNVAL   = BYTE2INT(DNVALB)
            CLCOUNT = CLCNTR		!CLASS COUNT
            CLCNTR = 0			!CLASS COUNTER
ccc---	print *,"----> CLCOUNT , CLCNTR = ",CLCOUNT , CLCNTR
            DO 80 CLLPIND = 1, CLCOUNT	!CLASS LOOP INDEX
cccc	if (many>13 .and. many<18) print *,"                                DO 80 LOOP  NS  File Num Class Num = ",J,LPIND,CLLPIND
               GDCLS   = GOOD(CLLPIND)   !GOOD CLASS
c               GDCLS = BYTE2INT(GDCLSB)
c	print *,"ADDR(LPIND) = ",ADDR(LPIND)
c		if (GDCLS .gt.0) print *,"cllpind,  GDCLS (+LOWLIM) = ",cllpind,GDCLS
               TABADR  = GDCLS + ADDR(LPIND)
		LOWLIM = LOW(CLLPIND,LPIND)
cccc	if (many>13 .and. many<18) print *,"Sample, LPIND (FIle), CLLPIND (Class), LOW(CLLPIND,LPIND), = ",J, LPIND,CLLPIND,LOWLIM 
CCCC            LOWLIM  = LOW(TABADR)			!GET COMPUTED LOW LIMIT (MEAN - SIGMA*CONFIDENCE_LEVEL)
c               LOWLIM  = BYTE2INT(LOWLIMB) 
cccc	if (many>13 .and. many<18) print *,"                           cllpind , tabadr, lowlim, dnval = ",cllpind , tabadr, lowlim, dnval
cccc	if (many>13  .and. many<18 .and. DNVAL .LT. LOWLIM) print *,"<<<<<<<<<<< LOWLIM"
               IF (DNVAL .LT. LOWLIM) GOTO 80
		UPPERLIM  = HGH(CLLPIND,LPIND)
CCCC               UPPERLIM  = HGH(TABADR)			!GET COMPUTED HIGH LIMIT (MEAN + SIGMA*CONFIDENCE_LEVEL)
c               UPPERLIM  = BYTE2INT(UPPERLIMB)		
cccc	if (many>13 .and. many<18) print *,"                         cllpind , tabadr, upperlim, dnval = ",cllpind , tabadr, upperlim,dnval
cccc        if (many>13  .and. many<18 .and. DNVAL .GT. UPPERLIM) print *,">>>>>>>>>>> UPPERLIM"
	       IF (DNVAL .GT. UPPERLIM) GOTO 80
               CLCNTR = CLCNTR + 1
               GOOD(CLCNTR) = GDCLS
cccc	if (many>13 .and. many<18)print *,"CLCNTR, GOOD(CLCNTR) = ",CLCNTR,GOOD(CLCNTR) 
   80       CONTINUE
c	if (CLCNTR .LE. 1) print *,"CLCNTR .LE. 1  GOTO 200 = ",CLCNTR
            IF (CLCNTR .LE. 1) GOTO 200		!IF ONLY 1 VALID CLASS
  180    CONTINUE
c	print *,"DONT = ",DONT
         IF (DONT) GOTO 190		!IF (DONT .NE. 0) GOTO 190
         NGOOD = CLCNTR
cccc	print *,"GOTO 400"
         GOTO 400
C
C  *** BRANCH TO HERE IF BAYESIAN OPTION NOT USED
  190    VAL = 255
c         VALB = INT2BYTE(VAL)
         OBUFFER(J) = VAL
         GOTO 450
C
C  *** DETEMINE IF ONLY ONE GOOD CLASS EXIST AND IF ALL CHANNELS
C  *** HAVE BEEN USED
  200	continue
         IF (CLCNTR .LT. 1) GOTO 300
c	print *, "CLCNTR = 1"
         CLCNTR  = GOOD(1)
ccc         CLCNTR = BYTE2INT(CLCNTRB)
C  *** VERIFY REMAINING CHANNELS
         DO 220 I = LPIND+1, NFILES
            BUFPTR = PTRBUF(I) + J
            DNVAL  = BUFFER(BUFPTR)			!GET THE DN
ccc        DNVAL  = BYTE2INT (DNVALB)
            TABADR = CLCNTR + ADDR(I)
CCCC            LOWLIM  = LOW(TABADR)			!GET COMPUTED LOW LIMIT (MEAN - SIGMA*CONFIDENCE_LEVEL)
	     LOWLIM = LOW(CLCNTR,I)
c            LOWLIM  = BYTE2INT(LOWLIMB)
c	    if (J>13) print *,"                           clcntr , tabadr, lowlim, dnval = ",clcntr , tabadr, lowlim, dnval
c	    if (J>13 .and. DNVAL .LT. LOWLIM) print *,"<<<<<<<<<<< LOWLIM 2"
           IF (DNVAL .LT. LOWLIM) GOTO 300
	      UPPERLIM  = HGH(CLCNTR,I)
c            UPPERLIM  = HGH(TABADR)			!GET COMPUTED HIGH LIMIT (MEAN + SIGMA*CONFIDENCE_LEVEL)
c            UPPERLIM  = BYTE2INT(UPPERLIMB)
c	    if (J>13) print *,"                         clcntr , tabadr, upperlim, dnval = ",clcntr , tabadr, upperlim,dnval
c            if (J>13 .and. DNVAL .GT. UPPERLIM) print *,">>>>>>>>>>> UPPERLIM 2"

            IF (DNVAL .GT. UPPERLIM) GOTO 300
  220    CONTINUE
C
C  *** IF CHECK OPTION ISN'T INVOKED, THEN DN = CLCNTR
         IF (.NOT. CHECK) THEN				!IF (CHECK .EQ. 0) THEN
c            clcntrb = int2byte(clcntr)
            OBUFFER(J) = CLCNTR
            GOTO 450
         ELSE
C
C  *** ELSE ASSIGN THE NUMBER OF GOOD CLASSES TO 1 AND CALL FBAYES
            NGOOD = 1
            GOTO 400
         ENDIF
C
C  *** CLASS IS UNKNOWN, DN = 0
  300    OBUFFER(J) = 0
         GOTO 450
C
C  *** CALL FBAYES TO DETERMINE ANY AMBIGUITY
  400   continue 
	MANY = MANY + 1
cccc	print *,"LOOKUP many = ",many

         call fbayes(nfiles, ngood, j, check, nprb, ncls)
  450 continue		!WND OF BIG LOOP
      return
      end

!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!

      subroutine init(dimlst, nodim)
      implicit none
C
C
! Define Passed parameters
      integer*4 dimlst(13), nodim

! Define COMMON variables
      integer*4 lendim, lentab, adim(10000)
      real*4 atab(10000)
      common /C4/ lendim, lentab, atab, adim
C
! Define Local variables
      integer*4 limit, val, cntr

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
      call zia(atab,10000)
      return
      end

!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
 
      subroutine set(sublist,value)
      implicit none
C
C*** COPIES VALUE INTO TABLE LOCATED AT SUBLIST + 1
C
! Define Passed parameters
      integer*4 sublist(*)
      real*4    value
 
! Define COMMON variables
      integer*4 lendim, lentab, adim(10000)
      real*4    atab(10000)
      common /C4/ lendim, lentab, atab, adim
C
! Define Local variables
      integer*4 adr
      integer*4 igetaddr


      adr = igetaddr(sublist)
      ATAB(ADR+1) = VALUE
      RETURN
      END
 

!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      real function val(sublist)
      implicit none
C
C  *** RETURNS VALUE OF ATAB LOCATED AT SUBLIST + 1
C
      integer*4 sublist(*), adr
C
      integer*4 lendim, lentab, adim(10000)
      real*4    atab(10000)
      common /C4/ lendim, lentab, atab, adim
C
      integer*4 igetaddr


      adr = igetaddr(sublist)
      VAL = ATAB(ADR+1)
      return
      end
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!

      subroutine getlst(sublist, vallst, len)
      implicit none

C
C  *** COPIES LEN VALUES FROM TABLE INTO VALLST BEGINNING AT ELEMENT
C  *** DENOTED BY SUBLST
C
! Define Passed parameters
      integer*4 sublist(*), len
      real*4    vallst(*)

! Define COMMON variables
      real*4    atab(10000)
      integer*4 lendim, lentab, adim(10000)
      common /C4/ lendim, lentab, atab, adim

! Define Local variables
      integer*4 adr, cntr
      integer*4 igetaddr
      real*4 val
 
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
   
      subroutine setlst(sublst, vallst, len)
      implicit none
C
C  *** COPIES LEN VALUES FORM VALLST INTO TABLE BEGINNING AT ELEMENT
C  *** DENOTED BY SUBLST
C
! Define Passed parameters
      integer*4 sublst(*), len
      real*4 vallst(*)
C
! Define COMMON variables
      integer*4 lendim, lentab, adim(10000)
      real*4    atab(10000)
      common /C4/ lendim, lentab, atab, adim
C
! Define Local variables
      real*4    val
      integer*4 adr, cntr
      integer*4 igetaddr

      VAL = 0.0
      ADR = 0
      CNTR = 0

      ADR = IGETADDR(SUBLST)
C
      DO 10 CNTR = 1, LEN
         VAL = VALLST(CNTR)
         ATAB(CNTR + ADR) = VAL
   10 CONTINUE
      return
      end
 
  
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      subroutine convrt
      implicit none
C
C  *** CONVERTS TABLED VALUES TO ALOG(VAL)
C
! Define Passed parameters
!     NONE    

! Define COMMON variables
      integer*4 lendim, lentab, adim(10000)
      real*4    atab(10000)
      common /C4/ lendim, lentab, atab, adim

! Define Local variables
      integer*4 limit, cntr
      real*4    elem

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
      return
      end
 
  
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
  
       integer function igetaddr(ss)
       implicit none


C  *** CALCULATES THE ADDRESS ASSOCIATED WITH SUBSCRIPT LIST FOUND AS THE
C  *** FIRST ADDRESS IN THE ARGUMENT LIST IN. ADDRESS IS RETURNED.
C
! Define Passed parameters
      integer*4 ss(*)

! Define COMMON variables
      integer*4 lendim, lentab, adim(10000)
      real*4    atab(10000)
      common /C4/ lendim, lentab, atab, adim
C
C
! Define Local variables
      integer*4 cntr, adr, limit 


      LIMIT = 0
      ADR = 0
      CNTR = LENDIM

  100 CNTR = CNTR - 1
        IF (CNTR .LE. LIMIT) GOTO 200
        ADR = (ADR + SS(CNTR + 1)) * ADIM(CNTR)
      GOTO 100
      
  200 IGETADDR = ADR + SS(1)
      END
