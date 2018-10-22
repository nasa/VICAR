      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C     PROGRAM ustats
C     19 OCT 79   ...REA...    INITIAL RELEASE
C     29 AUG 85   ...JHR...    CONVERT TO VICAR2
C      5 SEP 94   ...CRS (CRI) REVISE FOR PORTING
C     10 JUL 95   ...VRU (CRI) CHANGED FIRST OUTPUT FILE FORMAT TO ISTATFILE
C     15 APR 98   ...RRP (AR-9900) UPDATED USTATS.PDF TO RESTRICT CERTAIN
C                        PARAMETERS TO BE LESS THEN OR EQUAL TO ZERO.
	implicit none
      	INTEGER*4   IPARM(200),STAT,OUNIT
	integer*4 i,j,k,l,m,idef,icount,icclas,icperc
	integer*4 ni,nso,nchan,nclus
	integer*4 nli,icode
      	LOGICAL*4   XVPTST
	REAL*4      RPARM(200)
      	character*8 fmt(4)/'BYTE','HALF','FULL','REAL'/

      	CHARACTER*8 FORMAT
      	CHARACTER*80 CHARINP(10)

        integer*4 incl,incs,nclass,nexcl,sl,ss
        integer*4 nlo,nsi,nschan
	integer*4	msschn				!MSS
        real*4    bound,perc
        logical*4 qprt,qn,qall,qchk
	logical*4 qmss					!MSS
      COMMON /C1/ INCL,INCS,NCLASS,BOUND,PERC,NEXCL,
     &            SL,SS,NLO,NSI,NSCHAN,MSSCHN,
     &            QPRT,QN,QALL,QCHK,QMSS

	integer*4 iexcl(11),iunit(24)
	logical*4 qband(24)				!MSS
	COMMON /C2/ iunit,qband,iexcl
c   called by stacka	
      EXTERNAL    CLUSTR

      DATA QBAND/24*.TRUE./				!MSS

C        SET DEFAULTS AND INITIALIZE
      OUNIT=0
      IEXCL(1) = 9999
      NEXCL = 1
      INCL = 20
      INCS = 20
      QPRT = .TRUE.
      NCLASS = 10
      BOUND = 5.0
      PERC = 0.0
      QMSS = .FALSE.			!MSS mode
      NCLUS = 500			!default is 500 clusters
      QN = .FALSE.
      QALL = .FALSE.
      QCHK = .FALSE.
	m = 0
C
      CALL IFMESSAGE('USTATS version 21-Jul-2011') 	!15-APR-98')
C        DETERMINE NUMBER OF INPUTS
      CALL XVP('INP',CHARINP,NI)
      NCHAN = NI
      NSCHAN = 1
      MSSCHN = 1			!MSS
      IF(NI.EQ.1) then
	QMSS = .TRUE.			!MSS
	CALL XVMESSAGE('??E - USTATS requires more than one input image',' ')
	CALL ABEND
      ENDIF

c
C        GET SIZE AND FORMAT OF PRIMARY INPUT AND SIZE FIELD INFO
	CALL XVUNIT(IUNIT(1),'INP',1,STAT,' ')
      CALL XVOPEN(IUNIT(1),STAT, 'OP', 'READ',
     +                 'OPEN_ACT','SA','IO_ACT','SA',' ')
      CALL XVGET(IUNIT(1),STAT,'FORMAT',FORMAT,'NL',NLI,'NS', NSI,' ')
      CALL XVCLOSE(IUNIT(1),STAT,' ')
       icode = 0
c	print *,"FORMAT = ",format
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



C          OPEN INPUT DATA SETS
      DO I=1,NI
         CALL XVUNIT(IUNIT(I),'INP',I,STAT,' ')
         CALL XVOPEN(IUNIT(I),STAT,'OP', 'READ',
     +               'OPEN_ACT','SA','IO_ACT','SA',
     +                  'I_FORMAT',fmt(icode),'U_FORMAT',fmt(2),' ')
      END DO

C        GET DATA FORMAT AND CHECK
c      DO I=1,NI
c         CALL XVGET(IUNIT(I),STAT,'FORMAT',FORMAT,' ')
c         IF(FORMAT.NE.'BYTE') THEN
c            CALL XVMESSAGE('ustats ACCEPTS BYTE DATA ONLY',' ')
c            CALL ABEND
c         END IF
c      END DO

C        GET SIZE INFORMATION AND CHECK
c	print *, "nli, nsi = ",nli,nsi
      CALL XVSIZE(SL,SS,NLO,NSO,NLI,NSI)
c	print *, "nli, nsi, nlo, nso = ",nli,nsi,nlo,nso
      IF(SL+NLO-1 .GT. NLI) THEN
        CALL XVMESSAGE(
     &             '??E - Number of lines requested exceeds input size',' ')
        CALL ABEND
      END IF
      IF(SS+NSO-1 .GT. NSI) THEN
         CALL XVMESSAGE(
     &       '??E - Number of samples requested exceeds input size',' ')
         CALL ABEND
      END IF

C           *** PROCESS PARAMETERS ***

C        'INC'
      CALL XVPARM('INC',IPARM,ICOUNT,IDEF,1)
      IF(ICOUNT.NE.0) THEN
         INCL=IPARM(1)
         INCS=IPARM(1)
      END IF
C        'LINC'
      CALL XVPARM('LINC',IPARM,ICOUNT,IDEF,1)
      IF(ICOUNT.NE.0) INCL=IPARM(1)
C        'SINC'
      CALL XVPARM('SINC',IPARM,ICOUNT,IDEF,1)
      IF(ICOUNT.NE.0) INCS=IPARM(1)
C        'NOPRINT'
      IF ( XVPTST('NOPRINT') )  QPRT=.FALSE.
C        'CLAS'
      CALL XVPARM('CLASSES',IPARM,ICCLAS,IDEF,1)
      IF(ICCLAS.NE.0) NCLASS=IPARM(1)
C        'INITIAL'
      CALL XVPARM('INITIAL',RPARM,ICOUNT,IDEF,1)
      IF(ICOUNT.NE.0) BOUND = RPARM(1)
C        'PERCENT'
      CALL XVPARM('PERCENT',RPARM,ICPERC,IDEF,1)
      IF(ICPERC.NE.0) PERC=RPARM(1)
C        'MSS'
      CALL XVPARM('MSS',IPARM,ICOUNT,IDEF,1)			!MSS
      IF(ICOUNT.NE.0) then
	 NCHAN=IPARM(1)						!MSS
         MSSCHN = NCHAN						!MSS
         CALL XVMESSAGE('??E - MSS format no longer supported',' ')
	 CALL ABEND
      ENDIF
C        'USE'
      CALL XVPARM('USE',IPARM,ICOUNT,IDEF,24)			!MSS
      IF(ICOUNT.NE.0) THEN
	CALL XVMESSAGE('??E - USE if only for MSS data and is no lon ger supported',' ')
	CALL ABEND

         NCHAN = ICOUNT						!MSS
         DO I=1,NCHAN						!MSS
            QBAND(IPARM(I)) = .FALSE.				!MSS
         END DO							!MSS
c         DO I=1,MSSCHN
c            QBAND(I) = .NOT.QBAND(I)
c         END DO
      END IF
C        'EXCLUDE'
      CALL XVPARM('EXCLUDE',IPARM,ICOUNT,IDEF,24)
      IF(ICOUNT.NE.0) THEN
         NEXCL = ICOUNT
         DO I=1,NEXCL
            IEXCL(I+1) = IPARM(I)
         END DO
         NEXCL = NEXCL+1
      END IF
C        'CLUSTERS'
      CALL XVPARM('CLUSTERS',IPARM,ICOUNT,IDEF,1)
      IF (ICOUNT.NE.0) NCLUS=IPARM(1)
C        'NONN'
      IF (XVPTST('NONN')) QN=.TRUE.
C        'ALL' - do not combine/eliminate clusters
      IF (XVPTST('ALL')) QALL=.TRUE.
C        'NOTIFY'
      IF (XVPTST('NOTIFY')) QCHK=.TRUE.

c      IF (QMSS .AND. NSO .EQ. NSI) NSO=NSO/MSSCHN
c      NSCHAN = NSI/MSSCHN
      IF (ICPERC .NE. 0 .AND. ICCLAS .EQ. 0) NCLASS=NCLUS
      IF (QALL) NCLASS=NCLUS

C        DETERMINE BUFFER SIZES - in bytes
      I = 2*NSO*NCHAN 				!IN		!2*NSO*MSSCHN+4
c	print *, "i , nso  msschn= ",i , nso,  msschn 
c      IF (QMSS) I=2*NSCHAN*MSSCHN
      J = 4*NCLUS*NCHAN				!AVG		!4*NCLUS*NCHAN+4
c	print *, "j , NCLUS, NCHAN =" ,j,nclus, nchan
      K = 8*NCLUS*NCHAN				!VAR		!2*J+4
c	print *, "k j = ",j,k
      L = 4*NCLUS				!PTS		!4*NCLUS+4
c	print *, "l nclus = ", l,nclus
c	print *, "m = ", m
C      M = 12+4*NCHAN+2*NCHAN*(NCHAN+1)
c      IF (M .LT. 132) M=132
c	print *,"i,j,k,l,m = ",i,j,k,l,m
	M = 8*NCLUS*NCHAN*NCHAN			!COV		!2*J+4
C        MAIN PROCESSING
c	DYNAMICALLY ALLOCATE ARRAYS IN FORTRAN
c	      SUBROUTINE CLUSTR(IN,IX,AVG,JX,VAR,KX,PTS,LX)
      CALL STACKA(11,CLUSTR,5,I,J,K,L,M,NSO,NCHAN,NCLUS,*900)

C        CLOSE INPUT DATA SETS
      DO I=1,NI
         CALL XVCLOSE(IUNIT(I),STAT,' ')
      END DO

      RETURN

900   CALL XVMESSAGE(
     &           '??E - Insufficient memory for STACKA CALL',' ')
      CALL ABEND
      END
c==========================================================================
      SUBROUTINE CLUSTR(IN,IX,AVG,JX,VAR,KX,PTS,LX,COV,MX,NSO,NCHAN,NCLUS)

	implicit none
	integer*4   itemp
	integer*4 i,j,k,l,n,p,ix,jx,kx,lx,mx,ichk,istat
	integer*4 last,loc,mclus,nd
	integer*4 nsout,num,num2,nw,nwm,nws,pw
	integer*4   STAT,OUNIT,NSO,NCHAN,NCLUS
	INTEGER*2     IN(NSO,NCHAN)
        REAL*4        AVG(NCLUS,NCHAN),PTS(NCLUS)
        REAL*4        xx(10000), zz(10000)
        REAL*4    avg2,bdist,cutoff,pix,sum,dist
	REAL*4    X,x1
	REAL*8        VAR(NCLUS,NCHAN),COV(NCLUS,NCHAN,NCHAN)
      CHARACTER*10  IFMT,IFMT1,FXFMT,FXFMTS,FXFMTM
      CHARACTER*1400 MOUT
      CHARACTER*8   XOUT

        integer*4 incl,incs,nclass,nexcl,sl,ss
        integer*4 nlo,nsi,nschan,mtrx
        integer*4       msschn                          !MSS
        real*4    bound,perc
        logical*4 qprt,qn,qall,qchk
        logical*4 qmss                                  !MSS
      COMMON /C1/ INCL,INCS,NCLASS,BOUND,PERC,NEXCL,
     &            SL,SS,NLO,NSI,NSCHAN,MSSCHN,
     &            QPRT,QN,QALL,QCHK,QMSS

        integer*4 iexcl(11),iunit(24)
        logical*4 qband(24)				!MSS
        COMMON /C2/ iunit,qband,iexcl

C	To prevent warning messages in compiler
c	print *, "IX,JX,KX,LX = ",IX,JX,KX,LX
c	print *, "NSO , NCHAN, NCLUS , INCL, INCS= ",NSO , NCHAN, NCLUS,INCL,INCS
c	print *, "pts(1,,,2) = ",pts(1),pts(2)
	itemp=IX
	itemp=JX
	itemp=KX
	itemp=MX
      OUNIT=0
      IFMT='         '
      FXFMT='         '
      I = 4*NCLUS
      IF(I.GT.LX) THEN
         CALL XVMESSAGE('??E - Insufficient memory for STACKA buffers',' ')
         CALL ABEND
      END IF
      I = NCLUS*NCHAN
      J = NCLUS*NCHAN*NCHAN
      CALL ZIA(AVG,I)		!real*4			!avg in cluster
      CALL ZIA(VAR,2*I)		!real*8			!variance in cluster
      CALL ZIA(PTS,NCLUS)	!real*4			!points in cluster
      CALL ZIA(COV,2*J)         !real*8                 !covariance in cluster
c	print *,"ZIA PTS(1,,,2) =",pts(1),pts(2)
      NUM = 0				!set current cluster count to 0
      MCLUS = 1
      BOUND = BOUND*BOUND		!BOUND is set by INITIAL (def=5.0, BOUND=25.0)
      ICHK = INCL+NLO/10
      MOUT(1:32) = ' ustats IS     PERCENT COMPLETE'
C
C***SAMPLE THE IMAGE TO FORM CLUSTERS
C***GOES TO 900 CONTINUE
C
      DO 900 I=1,NLO,INCL	!LINES	!INCL is a param (def=20)
C
C        IF NECESSARY,  NOTIFY THE OPERATOR THAT JOB IS N% COMPLETE
         IF (.NOT. QCHK) GO TO 100
         IF (I .LT. ICHK) GO TO 100
         J = (100*(I-INCL))/NLO
cx	print *, "pre-here"  
         WRITE(MOUT(12:14), '(I3)') J
         CALL XVMESSAGE(MOUT(1:32),' ')
         ICHK = ICHK+NLO/10

100	 continue
c	print *, "pre-here2"
c	print *, "pts(1,,,2) = ",pts(1),pts(2)
	 CALL GETLIN(IUNIT,I,IN,SL,SS,NSCHAN,NCHAN,MSSCHN,NSO,QMSS,QBAND)
c   num = running count of clusters
c   nchan = numbr of bands	 
c   nclus = max count of clusters allowed
c   mclus = 
c   bound = set by INITIAL parm
c   incs = set by INCS parm
c   pts = num of points in cluster
c   bdist = square of bounds (due to comparison with variance which is distance)
c   X = distance (a cluster variance)
         DO 700 J=1,NSO,INCS		!SAMPLES  !INCS is a param (def=20)
            CALL EXCHK(IN,J,IEXCL,NEXCL,NCHAN,NSO,*700)	 	!*700		!check for excluded values
c		print *, "pre-here3  i,j,nso,incs,nchan,nclus,num = ",i,j,nso,incs,nchan,nclus,num 
c		print *, "pts(1,,2) = ",pts(1),pts(2)
c		print *, "var(1,1),(1,2) avg(1,1) (1,2)= ",var(1,1),var(1,2),avg(1,1),avg(1,2)
c	print *,"BEFORE 1st ADD: "	! avg(i,j), var(i,j) = ",avg(1,1),var(1,1),avg(4,1),var(4,1)	   
            IF(NUM.EQ.0)  then		!if curr cluster is 0
c	SUBROUTINE ADD(IN,ISAMP,ICLUS,NCHAN,NCLUS,NSO,NUM,VAR,AVG,PTS,*)
c	        mclus = 1
		CALL ADD(IN,J,1,NCHAN,NCLUS,NSO,NUM,VAR,AVG,COV,PTS,*300)	!*300
	    endif	
C
C     FIND NEAREST CLUSTER
C
            MCLUS = NUM+1
            BDIST = BOUND			
            DO 200 K=1,NUM			!for num of clusters
               X = DIST(IN,J,AVG,K,NCHAN,NCLUS,NSO)
c	print *, "do 200 - Return from DIST = X = (BDIST)",x,bdist
               IF (X .GT. BDIST) GO TO 200
               MCLUS = K
               BDIST = X		!reestablish bound
  200       CONTINUE
            IF (MCLUS .LE. NCLUS) GO TO 250	!skip following if we've not exceeded max clusters
            MOUT(1:32) = '    SAMPLING INCOMPLETE AT LINE'
            WRITE(MOUT(32:36),'(I5)') I
            CALL XVMESSAGE(MOUT(2:36),' ')
	    CALL XVMESSAGE("??W - Reached Maximum cluster size"," ") 
            GO TO 1000			!finished since we've exceeded our max clusters

  250       CONTINUE
ccc--	 Print *,"BEFORE 2nd ADD: "
            CALL ADD(IN,J,MCLUS,NCHAN,NCLUS,NSO,NUM,VAR,AVG,COV,PTS,*300)	!*300
ccc--	print *,"AFTER 2nd ADD:  avg(i,j), var(i,j) = ",avg(1,1),var(1,1),avg(4,1),var(4,1)
ccc--  299	    continue
	     
ccc--	    stop 299
  300       CONTINUE
cc	    print *,"AFTER 300 contine:  avg(i,j), var(i,j) = ",avg(1,1),var(1,1),cov(1,1,1)
            IF (QN) GO TO 700		!skip following if 'NONN set
C
C*************************************
C***NEAREST NEIGHBOR CHECK
C*************************************
C
C
C     CHECK PIXEL(S) TO THE LEFT
C
            L = J
  400       CONTINUE
            L = L-1	!point to sample to left
            IF (L .LE. 0) GO TO 500	!skip following if we are at first sample
            CALL EXCHK(IN,L,IEXCL,NEXCL,NCHAN,NSO,*500)		!*500
            X = DIST(IN,L,AVG,MCLUS,NCHAN,NCLUS,NSO)
ccc--		if(num.eq.0) print *, "400 - Return from DIST = X = (BOUND) ",x,bound
            IF (X .GT. BOUND) GO TO 500
ccc--	print *,"BEFORE 3rd ADD:"
            CALL ADD(IN,L,MCLUS,NCHAN,NCLUS,NSO,NUM,VAR,AVG,COV,PTS,*400)	!*400
ccc--	print *,"AFTER 3rd ADD:  avg(i,j), var(i,j) = ",avg(1,1),var(1,1),avg(4,1),var(4,1)
ccc--  499	stop
  500       CONTINUE
C
C     CHECK PIXEL(S) TO THE RIGHT
C
            L = J
  600       CONTINUE
c	print *,"AFTER 4th ADD:  avg(i,j), var(i,j) = ",avg(num,1),var(num,1)
            L = L+1	!point to sample to right
            IF (L .GT. NSO) GO TO 700		!skip following if at end of line
            CALL EXCHK(IN,L,IEXCL,NEXCL,NCHAN,NSO,*700)	!*700
            X = DIST(IN,L,AVG,MCLUS,NCHAN,NCLUS,NSO)
ccc--		print *, "600 - Return from DIST = X = (BOUND) ",x,bound
            IF (X .GT. BOUND) GO TO 700
ccc--	print *,"BEFORE 4th ADD:"
            CALL ADD(IN,L,MCLUS,NCHAN,NCLUS,NSO,NUM,VAR,AVG,COV,PTS,*600)	!*600
ccc--        print *,"AFTER 4th ADD:  avg(i,j), var(i,j) = ",avg(num,1),var(num,1)
  700    CONTINUE
c skipped or finished nearest neigbor (NN) check

  900 CONTINUE		!DO 900
C***********************************
C***COMPLETED SAMPLING THE IMAGE TO FORM CLUSTERS
C***********************************
c
c	print *,"CLUS:  CLASS          BAND         BAND     PTS      AVG              VAR              COV"
c	do i=1,num
c	   print *,"==============="
c	   do j=1,nchan
c	      do k=1,nchan
c		 if (j.eq.k) then
c		     print *,i,j,k,NINT(pts(i)),avg(i,j),var(i,j),cov(i,j,k)," DIAGONAL"
c	         else
c		     print *,i,j,k,NINT(pts(i)),avg(i,j),var(i,j),cov(i,j,k)
c	         endif
c	      enddo
c	   enddo
c	enddo
 1000 CONTINUE
      CALL XVMESSAGE(' ',' ')
      MOUT(1:22) = '       CLUSTERS FORMED'
      WRITE(MOUT(1:6),'(I6)') NUM
      CALL XVMESSAGE(MOUT(2:22),' ')
C*************************************
C***ELIMINATE ONE-PIXEL CLUSTERSa
C** VAR IS CONVERTED TO STD.DEV
C** 1st PLACE WHERE COV COMPUTED
C*************************************
ccc
c	print *,"STARTING 1-PIXEL CLUSTERS , NUM = ",num
c	print *,"     CLASS        BAND       PTS          AVG           VAR             COV"
c	goto 1001
c	do k=1,num
c	     do i = 1,nchan
c		stdev = dsqrt(var(k,i))
cc	  print *,k,i,nint(pts(k)),avg(k,i),var(k,i),stdev
c	     enddo
c	enddo
ccc
c1001  continue
      I=1		!increment clusters
      PIX = 0.0
      DO 1200 J=1,NUM		!GO thru all clusters
         PIX = PIX+PTS(J)
         IF (PTS(J) .EQ. 1.0 .AND. .NOT. QALL) GO TO 1200	!skip elimination step if QALL
         PTS(I) = PTS(J)
ccc--	print *,"before DO K=1,NCHAN:  avg(i,j), var(i,j) = ",avg(1,1),var(1,1),avg(4,1),var(4,1)
         DO K=1,NCHAN
            AVG(I,K) = AVG(J,K)
cx		print *,"j,k, avg(i,k) = ",j,k, avg(j,k)
ccc--		print *,"j,k, VAR(J,K) = ",j,k, VAR(J,K)
ccc--		print *, "J, PTS(J) = ",j, PTS(J)
ccc--		print *, "J, AVG(J,K) = ",j,k, AVG(J,K)
C ORIGINAL:     X = (VAR(J,K) - PTS(J)*AVG(J,K)*AVG(J,K))/(PTS(J) - 0.999999)
ccc--	    VAR(I,K) = VAR(J,K)
ccc--		X = VAR(I,K) 
ccc  VAR(ICLUS,N) = ((X1*X1) + (VAR(ICLUS,N)*PTS(ICLUS)))/(PTS(ICLUS)+1.0)
ccc		X = (VAR(J,K) + (VAR(J,K)*PTS(J))/(PTS(J) - 0.99999) 
	     VAR(I,K) = VAR(J,K)
	     X = VAR(I,K)
		if (x .le. 0.0) x = ABS(X)
ccc--	    print *,"X = ",i,j,k,X
c		print *,"=========="
		do p=1,NCHAN
	            COV(I,K,P) = COV(J,K,P)     !VAR(I,K)
c		    if(k.eq.p) then
c			print *,i,k,pts(i),avg(i,k),var(i,k),cov(i,k,p)," diagonal"
c		    else
c		        print *,i,k,pts(i),avg(i,k),var(i,k),cov(i,k,p)
c		    endif
	        enddo
            VAR(I,K) = SQRT(X)		!VAR is now STDDEV
c	    print *,i,k,pts(i),avg(i,k),var(i,k),cov(i,k,1)
         END DO
         I = I+1
 1200 CONTINUE
c        print *,"after elim 1 pix (DSQRT):  avg(1,1), sigma(1,1), cov(1,1,1) = ",avg(1,1),var(1,1),cov(1,1,1)
ccc--	print *,"before QALL SORT:  avg(i,j), var(i,j) = ",avg(1,1),var(1,1),avg(4,1),var(4,1)
      IF (QALL) CALL SORT(PTS,AVG,VAR,COV,NUM,NCLUS,NCHAN)
ccc--	print *,"after QALL SORT:  avg(i,j), var(i,j) = ",avg(1,1),var(1,1),avg(4,1),var(4,1)
      IF (QALL) GO TO 2500
      IF (I .EQ. 1) THEN
         CALL XVMESSAGE(
     &      '??E - no cluster contains more than one pixel***',' ') 
         CALL XVMESSAGE(' ***PROGRAM TERMINATED***',' ')
         CALL ABEND
      END IF
      NUM = I-1
      MOUT= '       CLUSTERS AFTER REMOVING ONE-PIXEL CLUSTERS' 
      WRITE(MOUT(1:6),'(I6)') NUM
      CALL XVMESSAGE(MOUT(2:50),' ')
ccc--	print *,"rem 1 pix:  avg(i,j), var(i,j) = ",avg(1,1),var(1,1),avg(4,1),var(4,1)
c	num is number odf clusters
      IF (NUM .EQ. 1) GO TO 2500		!skip sort if only 1 cluster
C***********************************************************
C***COMBINE CLUSTERS THAT OVERLAP BY ONE STANDARD DEVIATION
C   VAR is STDDEV
C***********************************************************
      CALL SORT(PTS,AVG,VAR,COV,NUM,NCLUS,NCHAN)
ccc--	print *,"after 1st SORT:  avg(i,j), var(i,j) = ",avg(1,1),var(1,1),avg(4,1),var(4,1)
      LAST = NUM-1
      NUM2 = NUM
      DO 1800 I=1,LAST					!number of clusters
         IF (PTS(I) .EQ. 0.0) GO TO 1800
         N = I+1
 1300    CONTINUE
         DO 1600 J=N,NUM
            IF (PTS(J) .EQ. 0.0) GO TO 1600
            DO K=1,NCHAN
               X = VAR(I,K) + VAR(J,K) - ABS(AVG(I,K) - AVG(J,K))
               IF (X .LE. 0.0) GO TO 1600
            END DO
C
C     COMBINE 2 CLUSTERS, PUTTING RESULT IN THE FIRST CLUSTER'S POSITION
C     SET POPULATION OF SECOND CLUSTER TO ZERO
C
            SUM = PTS(I)+PTS(J)
c	    print *,"COMBINE: CLASS   BAND    BAND    VAR       COV"
            DO 1500 K=1,NCHAN
               AVG2 = (PTS(I)*AVG(I,K) + PTS(J)*AVG(J,K))/SUM
c X is new variance
               X = ((PTS(I)-1.0)*VAR(I,K)*VAR(I,K) + (PTS(J)-1.0)*VAR(J,K)*VAR(J,K)
     +          + PTS(I)*AVG(I,K)*AVG(I,K) + PTS(J)*AVG(J,K)*AVG(J,K) -
     +           SUM*AVG2*AVG2)/(SUM-1.0)
		if (x .lt. 0.0) x = ABS(X)
		do P=1,NCHAN
		    X1 = ((PTS(I)-1.0)*VAR(I,K)*VAR(I,P) + (PTS(J)-1.0)*VAR(J,K)*VAR(J,P) 
     +          + PTS(I)*AVG(I,K)*AVG(I,P) + PTS(J)*AVG(J,K)*AVG(J,P) -
     +           SUM*AVG2*AVG2)/(SUM-1.0)
		   if (x1 .lt. 0.0) x1 = ABS(X1)
	            COV(I,K,P) = X1
c		    if (k.eq.p) then
c	                print *, j,K, P,X,  X1,"  DIAG"
c		    else
c			print *, I,K, P,X,  X1
c		    endif
		enddo
               VAR(I,K) = SQRT(X)	!convert to stddev
               AVG(I,K) = AVG2
cc		    print *, "I,K, X, SIGMA(I,K) COV(I,K,1) = ",I,K, X, VAR(I,K), COV(I,K,1) 
 1500       CONTINUE
            PTS(I) = SUM
            PTS(J) = 0.0
            NUM2 = NUM2-1
            GO TO  1300
 1600    CONTINUE

 1800 CONTINUE
ccc--	print *,"before 2nd SORT:  avg(1,1), var(1,1) = ",avg(1,1),var(1,1)
      CALL SORT(PTS,AVG,VAR,COV,NUM,NCLUS,NCHAN)
c	print *,"after 2nd SORT:  avg(1,1), sigma(1,1) = ",avg(1,1),var(1,1)

      NUM = NUM2
      MOUT=' '
      MOUT(8:44)=
     +'CLUSTERS AFTER COMBINING THOSE WHICH '
      MOUT(45:77)= 'OVERLAP BY ONE STANDARD DEVIATION'
      WRITE(MOUT(1:6),'(I6)') NUM
      CALL XVMESSAGE(MOUT(2:77),' ')
 2500 CONTINUE
C*****************************************************
C***PRINT THE POPULATION AND MEANS FOR EACH CLUSTER
C*****************************************************
      IF (.NOT. QPRT) GO TO 3000	!skip following  if "NOPRINT
      NW = 12  			!col width - changed from 10
      PW = 164			!page width - changed from 120
      ND = 2
      IFMT= '(I12)    '		!I(NW) 	- changed from I10
      FXFMT='(F12.2)  '		!F(NW.decimal) - changed from F10.2
      IF(NCHAN.GT.12) THEN
         NW=PW/NCHAN		!=120/NCHAN
         IFMT= '(I9)     '
         FXFMT='(F9.2)   '
      ENDIF
      IF(NCHAN.GT.13)THEN
         IFMT= '(I8)     '
         FXFMT='(F.2)    '
      ENDIF
      IF(NCHAN.GT.15)THEN
         IFMT= '(I7)     '
         FXFMT='(F7.2)   '
      ENDIF
      IF(NCHAN.GT.17)THEN
         ND=1
         IFMT= '(I6)     '
         FXFMT='(F6.1)   '
      ENDIF
      IF(NCHAN.GT.20)THEN
         ND=0
         IFMT= '(I5)     '
         FXFMT='(F5.0)   '
      ENDIF
      MOUT='       PIXELS'
      CALL XVMESSAGE(' ',' ')
      DO I=1,NCHAN
         WRITE(MOUT(13+NW*I-(NW)+1:13+NW*I),IFMT) I
         IF(NW.GT.6) MOUT(7+NW*I:(7+NW*I)+4)='BAND'
      END DO
      CALL XVMESSAGE(MOUT(2:(13+NW*NCHAN)+1),' ')
      DO 2800 I=1,NUM
         MOUT= ' '
         WRITE(MOUT(1:5),'(I5)') I
         WRITE(MOUT(6:13),'(I8)') NINT(PTS(I))
         DO J=1,NCHAN
            WRITE(MOUT(13+NW*J-(NW)+1:13+NW*J),FXFMT) AVG(I,J)
         END DO
         CALL XVMESSAGE(MOUT(2:(13+NW*NCHAN)),' ')
 2800 CONTINUE
 3000 CONTINUE
C QPRT does not inhibit the following
C***********************************************************************
C***PRINT THE POPULATION, MEANS, AND SIGMAS FOR ALL CLASSES TO BE OUTPUT
C***********************************************************************
C	FORMAT COLUMNS
C     COMPUTE THE WIDTH OF COLUMNS FOR MEANS (FXFMTM) AND SIGMAS (FXFMTS),
C     AND DECIMAL POINTS (ND)
C
c       print *,"BEFORE print:  avg(1,1), var(1,1) = ",avg(1,1),var(1,1)
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE('CLASSES RETAINED FOR OUTPUT',' ')
      NW = 18			!number in col - 18
      PW = 144			!page width in cols - 126
      ND = 2			!number of decimal pts - 2
      IFMT1=  '(I18)    '	!I(NW)
      FXFMTS= '(F8.2)   '	!F(NW.ND)
      FXFMTM= '(F10.2)  '
      IF(NCHAN.GT.8) THEN
         NW=PW/NCHAN		!=126/NCHAN
         IFMT1= '(I15)    ' 
         FXFMTS='(F7.2)   '
         FXFMTM='(F8.2)   '
      END IF 
      NWS = (NW-1)/2
      NWM = NW-NWS
      IF(NW.LT.14) ND=1
      IF(NW.LT.12) ND=0
      IF(NCHAN.EQ.9) THEN
         IFMT1= '(I14)    '
         FXFMTS='(F6.2)   '
         FXFMTM='(F8.2)   '
      END IF 
      IF(NCHAN.EQ.10) THEN
         IFMT1= '(I12)    '
         FXFMTS='(F5.2)   '
         FXFMTM='(F7.1)   '
      END IF 
      IF(NCHAN.EQ.11) THEN
         IFMT1= '(I11)    '
         FXFMTS='(F5.2)   '
         FXFMTM='(F6.0)   '
      END IF
      IF(NCHAN.EQ.12) THEN
         IFMT1= '(I10)    '
         FXFMTS='(F4.2)   '
         FXFMTM='(F6.0)   '
      END IF
      IF(NCHAN.GT.12) THEN
         IFMT1= '(I9)     '
         FXFMTS='(F4.2)   '
         FXFMTM='(F5.0)   '
      END IF
      IF(NCHAN.GT.14) THEN
         IFMT1= '(I8)     '
         FXFMTS='(F3.2)   '
         FXFMTM='(F5.0)   '
      END IF
      IF(NCHAN.GT.15) THEN
         IFMT1= '(I7)     '
         FXFMTS='(F3.2)   '
         FXFMTM='(F4.0)   '
      END IF
      IF(NCHAN.GT.18) THEN
         IFMT1= '(I6)     '
         FXFMTS='(F2.1)   '
         FXFMTM='(F4.0)   '
      END IF
      IF(NCHAN.GT.22) THEN
         IFMT1= '(I5)     '
         FXFMTS='(F2.0)   '
         FXFMTM='(F3.0)   '
      END IF

C     COMPOSE AND PRINT TWO LINES OF HEADER
C
      MOUT(1:4)='    '
      DO I=1,NCHAN
         WRITE(MOUT(4+NW*I-(NW)+1:4+NW*I),IFMT1) I
         IF(NW.GE.9) MOUT(NW*I-2:NW*I+1)='BAND'
      END DO
c	print *, "here"
      CALL XVMESSAGE(MOUT(2:4+NW*NCHAN),' ')
      MOUT = ' CLASS'
      IF(NW.GE.10) THEN
         DO I=1,NCHAN
            MOUT(3+NW*I-NWS:3+NW*I-NWS+3)='MEAN'
            MOUT(4+NW*I:4+NW*I+2)='SIG'
         END DO
      ELSE
         DO I=1,NCHAN
            MOUT(6+NW*I-NWS:6+NW*I-NWS)='M'
            MOUT(6+NW*I:6+NW*I)='S'
         END DO
      END IF
c	print *,"here2"
      CALL XVMESSAGE(MOUT(2:NW*NCHAN+6),' ')
      MOUT(6:6)= ' '
C
C     DETERMINE IF CLUSTER IS TO BE KEPT, AND IF SO, PRINT POPULATION, M
C     AND SIGMAS
C
	
ccc--	print *,"avg(i,j), var(i,j) = ",avg(1,1),var(1,1)
      CUTOFF = 0.01*PERC*PIX
      DO 3300 I=1,NUM
         IF(PTS(I).LT.CUTOFF.OR.I.GT.NCLASS) GO TO 3400
         WRITE(MOUT(1:5),'(I5)') I         
         DO J=1,NCHAN
           WRITE(MOUT(6+NW*J-(NWM+NWS)+1:6+NW*J),FXFMTM) AVG(I,J)
           WRITE(MOUT(6+NW*J-NWS+1:6+NW*J),FXFMTS) VAR(I,J)
         END DO
c	print *,"here3"
         CALL XVMESSAGE(MOUT(2:6+NW*NCHAN),' ')
 3300 CONTINUE
      GO TO 3500
 3400 CONTINUE
      NUM = I-1
 3500 CONTINUE


C        DETERMINE NUMBER OF SAMPLES FOR OUTPUT DATA SET
      NSOUT = 12+4*NCHAN+2*NCHAN*(NCHAN+1)
c       print *,"BEFORE XVWRIT:  avg(1,1), var(1,1) = ",avg(1,1),var(1,1)      
C        OPEN OUTPUT DATA SET
      CALL XVUNIT(OUNIT,'OUT',1,STAT,' ')
      CALL ISTAT_FILE_OPEN(OUNIT,'WRITE',NUM,NCHAN,' ',ISTAT)
      IF (ISTAT.LT.0) CALL ISTAT_SIGNAL(OUNIT,ISTAT,1)

C        WRITE IBIS OUTPUT, ONE RECORD FOR EACH CLASS
c	remember VAR is STD DEV and must be converted back to VARIANCE
c	also need to put out complete covariance matrix
c
c	COVARIANCE MATRIX - COV(NCHAN,NCHAN)
C
	MTRX = (NCHAN * (NCHAN + 1)) / 2             !!!number of cov elements in LL trianglar matrix
C
c	print *,"p,i,j,avg(p,i),COV(p,I,J), VAR(P,I)"
c	do p=1,num
c	DO I=1,NCHAN
c	    DO J=1,NCHAN
c		if (I.EQ.J) then
c		    print *,P,I,J,avg(p,i),COV(P,I,J),VAR(P,I)*VAR(P,I)," VARIANCE DIAGONAL"
c	        else 
c		    print *,P,I,J,avg(p,i),COV(P,I,J)
c		endif
c	    ENDDO
c	ENDDO
c	enddo
c	print *,"========================================================="
      XOUT(1:5) ='CLASS'
      DO 3700 I=1,NUM
         WRITE(XOUT(6:8),'(I3)') I
         LOC = 0
	 P = 0
         DO J=1,NCHAN
CCCC            LOC = LOC + J		!STEP THRU 
CCCC         ZZ(J) = AVG(I,J)
            ZZ(J) = AVG(I,J)		!MEAN OF BAND I
CCCC         XX(LOC) = SNGL(VAR(I,J)*VAR(I,J))
	    do k=1,j
		LOC = LOC + 1
		XX(LOC) = SNGL(COV(I,J,K))
            enddo
	 ENDDO
C	FOR 3-BAND IMAGE
C	C1=CLASS NAME; C2=NUM PIXELS IN CLASS; C3=NUMBER OF BANDS;
C	C4=MEAN OF BAND 1; C5=MEAN OF BAND 2; C6=MEAN OF BAND 3;
C	C7=COV CLASS1,BAND1,BAND1; C8+
      CALL ISTAT_RECORD_WRITE(OUNIT,I,XOUT,NINT(PTS(I)),NCHAN,
     &                        ZZ,XX,ISTAT)
         IF (ISTAT.LT.0) CALL ISTAT_SIGNAL(OUNIT,ISTAT,1)
 3700 CONTINUE

c       print *,"AFTER XVWRIT:  avg(1,1), var(1,1) = ",avg(1,1),var(1,1)
C        CLOSE OUTPUT DATA
      CALL ISTAT_FILE_CLOSE(OUNIT,ISTAT,1)
      IF (ISTAT.LT.0) CALL ISTAT_SIGNAL(OUNIT,ISTAT,1)
      RETURN
      END
c===============================================================================
      SUBROUTINE GETLIN(IUNIT,LINE,IN,SL,SS,NSCHAN,NCHAN,MSSCHN,NSO,
     &                  QMSS,QBAND)

C
C     GETLIN GETS A LINE OF INPUT FOR ALL CHANNELS AND FORMATS THE DATA
C     INTO THE ARRAY 'IN'.
C
	implicit none
      INTEGER*4    SL,SS,STAT,nbytes,IUNIT(24)
	integer*4 i,j,k,m,n,line,nschan,nchan,nso
	integer*4 msschn			!MSS
      INTEGER*2    IN(NSO,NCHAN)
      LOGICAL*4    QMSS,QBAND(24)		!MSS

      N = SL+LINE-1
      IF(QMSS) GO TO 200		!MSS
C
C        MULTIPLE INPUTS
      DO I=1,NCHAN
      CALL XVREAD(IUNIT(I),IN(1,I),STAT,'LINE',N,
     +                              'SAMP',SS,'NSAMPS',NSO,' ')
      END DO

      RETURN
C
C     START    MSS INPUT		!MSS
C
c    following for qmss = .true.
200   NBYTES = NSCHAN*MSSCHN-SS-1
      CALL XVREAD(IUNIT(1),IN,STAT,'LINE',N,'SAMP',S
     +                                   S,'NSAMPS',NBYTES,' ')

      IF(NSO.EQ.NSCHAN) GO TO 250
C        SIZE FIELD BEING USED
c  following for qmss = .true.
      DO I=2,MSSCHN
         M = (I-1)*NSCHAN
         K = M/NSO+1
         J = M-NSO*(K-1)+1
         CALL MVE(2,NSO,IN(J,K),IN(1,I),1,1)
      END DO
c  end of qmss = .true.
  250 CONTINUE
      IF(MSSCHN.EQ.NCHAN) RETURN
C
C        NOT ALL BANDS TO BE USED; REMOVE THE BANDS TO BE IGNORED
c   following for qmss = .true.
      J = 1
      N = 2*NSO
      DO 300 I=1,MSSCHN
         IF(.NOT.QBAND(I)) GO TO 300
         IF(I.NE.J) CALL MVE(1,N,IN(1,I),IN(1,J),1,1)
         J = J+1
  300 CONTINUE

      RETURN
      END
c===============================================================================
      SUBROUTINE SORT(PTS,AVG,VAR,COV,NUM,NCLUS,NCHAN)
C
C***SORT CLUSTERS BY POPULATION
C
	implicit none
	integer*4 i,j,k,l,n,m,p,num,nclus,nchan
      REAL*8 VAR(NCLUS,NCHAN),COV(NCLUS,NCHAN,NCHAN)
      REAL*4 PTS(NCLUS),AVG(NCLUS,NCHAN)
	real*4 hold
	real*8 hold2
c
ccc--	print *,"SORT start:  avg(i,j), var(i,j) = ",avg(1,1),var(1,1),avg(4,1),var(4,1)
	
      M = NUM/2
 2000 CONTINUE
      K = NUM-M
      J = 1		!increm clusters
 2100 CONTINUE
      I = J
 2200 CONTINUE
      L = I+M
      IF(PTS(I).GT.PTS(L)) GO TO 2400
      HOLD = PTS(I)
      PTS(I) = PTS(L)
      PTS(L) = HOLD
C
C REORDER STATISTICS
C
      DO 2300 N=1,NCHAN
         HOLD = AVG(I,N)
         AVG(I,N) = AVG(L,N)
         AVG(L,N) = HOLD
ccc--	print *, "VAR(I,N) = ",i,n,VAR(I,N)
         HOLD2 = VAR(I,N)
         VAR(I,N) = VAR(L,N)
         VAR(L,N) = HOLD2
ccc--	print *, "VAR(L,N) = ",i,n,VAR(L,N)
	 do P=1,NCHAN
	    HOLD2 = COV(I,N,P)
            COV(I,N,P) = COV(L,N,P)
	    COV(L,N,P) = HOLD2
	 enddo
 2300 CONTINUE

      I = I-M
      IF(I.GE.1) GO TO 2200
 2400 CONTINUE
      J = J+1
      IF(J.LE.K) GO TO 2100
      M = M/2
      IF(M.GT.0) GO TO 2000

ccc--        print *,"SORT end:  avg(i,j), var(i,j) = ",avg(1,1),var(1,1)


      RETURN
      END
c===============================================================================
      REAL FUNCTION DIST(IN,ISAMP,AVG,ICLUS,NCHAN,NCLUS,NSO)
C
c	if dist is real*8 things don't work -- why?
C     DIST COMPUTES THE SQUARE OF THE EUCLIDEAN DISTANCE OF PIXEL 'ISAMP
C     FROM THE MEAN OF CLUSTER 'ICLUS'
C
	implicit none
	integer*4 i,isamp,iclus,nchan,nclus,nso
      INTEGER*2 IN(NSO,NCHAN)
      REAL*4 AVG(NCLUS,NCHAN),X

      DIST = 0.0
      DO I=1,NCHAN
         X = IN(ISAMP,I)
         X = X-AVG(ICLUS,I)
         DIST = DIST+(X*X)
      END DO
cc	DIST = SQRT(DIST)
ccc--	print *,"DIST: DIST = ",dist
      RETURN
      END
c===============================================================================
      SUBROUTINE EXCHK(IN,ISAMP,IEXCL,NEXCL,NCHAN,NSO,*)
C
C     EXCHK CHECKS TO SEE IF PIXEL 'ISAMP' HAS A DN THAT IS TO BE EXCLUD
C     IF SO, THE ALTERNATE RETURN IS USED.
C
	implicit none
	integer*4 i,j,isamp,nexcl,nchan,nso
	INTEGER*4 IEXCL(11)
	INTEGER*2 IN(NSO,NCHAN)

      DO 200 I=1,NEXCL
         DO J=1,NCHAN
            IF(IN(ISAMP,J).EQ.IEXCL(I)) RETURN 1	!goes to * in call
         END DO
  200 CONTINUE

      RETURN
      END
c===============================================================================
      SUBROUTINE ADD(IN,ISAMP,ICLUS,NCHAN,NCLUS,NSO,NUM,VAR,AVG,COV,PTS,*)
C
C     ADD COMBINES PIXEL 'ISAMP' WITH CLUSTER 'ICLUS'
C
	implicit none
	integer*4 nclus,nchan,isamp,iclus,nso
	integer*4 num,m,n
      	REAL*8 VAR(NCLUS,NCHAN),COV(NCLUS,NCHAN,NCHAN)
      	REAL*4 AVG(NCLUS,NCHAN),PTS(NCLUS)
	real*4 x,x1,x2(NCHAN)
      	INTEGER*2 IN(NSO,NCHAN)

c                print *, "ADD:  iclus,nso,nchan,nclus,num = ",iclus,nso,nchan,nclus,num
c                print *, "pts(1,,2) = ",pts(1),pts(2)
c                print *, "var(1,1),(1,2) avg(1,1) (1,2)= ",var(1,1),var(1,2),avg(1,1),avg(1,2)
c	print *, "ADD: NUM = ",num
c       print *,"ADD::  CLASS          BAND         BAND     PTS      AVG              VAR              COV"
	
      DO N=1,NCHAN
         X = IN(ISAMP,N)		!X = VARIANCE FROM DIST FUNCT
ccc--	 if (ISAMP .eq. 1) print *, "AVG(ICLUS,N) ,  X = ",n,iclus,AVG(ICLUS,N),X 
	if (NUM .eq. 0) then 
	    AVG(ICLUS,N) = X
	    X1 = 0.0
	    VAR(ICLUS,N) = 0.0d0
	    do m=1,nchan
	       COV(ICLUS,N,m) = 0.0d0
	    enddo
        else		
ccc-	 if (N .eq. 1) print *, "AVG(ICLUS,N) ,  X = ",AVG(ICLUS,N),X
	    X1 = X - AVG(ICLUS,N)
	    do m=1,nchan
		x2(m) = X - AVG(ICLUS,m)
	    enddo    
	endif  		!	- AVG(ICLUS,N)
C             VAR(ICLUS,N) = VAR(ICLUS,N) + ((X - AVG(ICLUS,N)) * (X - AVG(ICLUS,N)))
ccc--         VAR(ICLUS,N) = VAR(ICLUS,N) + (X1*X1)/(PTS(ICLUS)+1.0)
	VAR(ICLUS,N) = ((X1*X1) + (VAR(ICLUS,N)*PTS(ICLUS)))/(PTS(ICLUS)+1.0)
c	print *,"-------------------"
	do m=1,nchan
	    COV(ICLUS,N,m) =  (abs(X1*x2(m)) + (COV(ICLUS,N,M)*PTS(ICLUS)))/(PTS(ICLUS)+1.0)
c	    if (n.eq.m) then
c                     print *,iclus,n,m,NINT(pts(iclus)+1),avg(iclus,n),var(iclus,n),cov(iclus,n,m)," DIAGONAL"
c            else
c                     print *,iclus,n,m,NINT(pts(iclus)+1),avg(iclus,n),var(iclus,n),cov(iclus,n,m)
c            endif

	enddo
ccc--	print *,"> x, x1, n, isamp, iclus, pts(iclus), var(iclus,n), AVG(ICLUS,N) = ",
ccc--	1 x,x1,n,isamp, iclus, pts(iclus), var(iclus,n), AVG(ICLUS,N)
c	print *,"X  ,   pts(iclus) avg(iclus,n)  =  ",x,pts(iclus),avg(iclus,n)
c	temp = AVG(ICLUS,N)
c	AVG(ICLUS,N) = (X - AVG(ICLUS,N)) /(PTS(ICLUS)+1.0)
c	print *, "VAR(NCLUS,N) = ",N, X, AVG(ICLUS,N)
c	print *,"pts(iclus),x = ",pts(iclus), x
C ORIGINAL:   AVG(ICLUS,N) = (AVG(ICLUS,N)*PTS(ICLUS)+X)/(PTS(ICLUS)+1.0)
C	The new average is the X value and the average*number of points in the average / new number of points
	AVG(ICLUS,N) = (X + (AVG(ICLUS,N)*PTS(ICLUS)))/(PTS(ICLUS)+1.0) 
ccc--	print *,"n, iclus, var(iclus,n) = ",n, iclus, var(iclus,n) 
ccc--	print *,"n, iclus, avg(iclus,n) = ",n, iclus, avg(iclus,n) , X
      END DO


c       print *,"nchan, iclus, avg(iclus,n) var(iclus,n) = ",nchan, iclus, avg(iclus,nchan), var(iclus,nchan)
      PTS(ICLUS) = PTS(ICLUS)+1.0		!bump the running count of points in the cluster
      IN(ISAMP,1) = 9999
      IF(ICLUS.GT.NUM) NUM=ICLUS

      RETURN 1
      END
