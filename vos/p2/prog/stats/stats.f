                                                                 
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C     PROGRAM STATS
C      6 MAY  1977 ...JDA... INITIAL RELEASE
C     28 JUNE 1978 ...JDA... CHANGE CALL LABELC TO CALL LABELB
C     19 SEPT 1981 ...REA... FIX BUG THAT EATS CORE, AND
C                            CHANGE LABELB TO LABELC
C     10 APR  1983 ...REA... EXPAND INPUT BUFFER TO 19072 BYTES
C      1 OCT  1983 ...AJR... MODIFY PARAMETERS AND VAX CONVERSION
C     25 JUL  1985 ...REA... FIX BUGS TO MAKE THE 'ALL' FEATURE OF
C				HIST AND SPEC WORK
C     25 FEB  1986 ...SP.... CONVERTED TO VICAR2 CALLS.
C     25 FEB  1986 ...SP.... CHANGED TO ALLOW UP TO 12 INPUT FILES.
C     25 FEB  1986 ...SP.... RENAMED ROUTINE FORMAT AS HEADING TO AVOID CONFLICT
C     25 FEB  1986 ...SP.... CORRECTED BUG IN PFIELD FOR LISTING OF VERTICES.
C     11 OCT  1988 ...SP.... CORRECTED BUG IN PFIELD PRINTING MORE THAN 4 DIGITS
C     31 OCT  1994 ...CRI... MSTP S/W CONVERSION (VICAR PORTING)
C     10 JUL  1995 ...CRI... CHANGED FIRST OUTPUT FILE FORMAT TO ISTATFILE
C     30 MAY  2002 ...AXC... MODIFIED VICAR RTL CALL.  (AR-106777)
C     16 JUN  2011 ...RJB... Many fixes and code fixes to pass fortran 4.4.4
c			     compiler on linux
c			     Fixed badly aligned common blocks in this program
c			     Had to go back to IBM Version of 1984 to correct this
c     04 JUL 2011  ...RJB... HALFWORD format allowed
c
C        'STATS'   STATISTICS PROCESSOR PROGRAM
C
c	IMPLICIT INTEGER (A-Z)
	implicit none
      INCLUDE 'fortport'

c      INTEGER*2 LXBUF(1202),S1BUF(1200),NS1BUF(1200)
        integer*4 maxclasses
        parameter (maxclasses = 50)

	byte bbuf(32000)
      integer*4 exclud(10),nclasses,nbands
      integer*4 line2(12),splot(maxclasses)
      integer*4 ptrbf1(12),ptrbuf(12)
      integer*4 bnd(12)
      integer*4 verts(50)
      integer*4 hist(12)
      integer*4 spec(12)
      integer*4 idn(12),clspts(maxclasses),npix
      integer*4 iunit(12),ounit(2)
	integer*4 cls,clscnt,fldnum,form,ibis,iclass,icount,idef
	integer*4 i,ii,io,ireturn,iscribe,istat,j,j1,j2,jv
	integer*4 k,kk,kl,kl1,kl2,l,lx,maxfld,mtrx,nchan,ncls
	integer*4 nexcl,nhist,ni,nli,nsi,nm,nn,no,nplot,npnts
	integer*4 ns,nschan,nso,nspec,nuse,nx,oscribe
	integer*4 par,parnxt,perpts,nxc,ptr,sl,ss
	integer*4 statunit,xdn,xds,icode
	integer*4 mmm,nnn
      logical*4 xvptst
      logical*4 mss,noprnt,add
      logical*4 stat,scribe

      real*4    cov(78),cor(78),smean
      real*4    means(1000),covariance(1000)
      real*8 xmean(12),xcov(78),pts1,pts2

        character*4 fmt(4)/'BYTE','HALF','FULL','REAL'/

      character*80 msg
      character*10 pname
      character*8 format
      character*40 inst
      character*132 buffer,buffer1
c      BYTE    BUF(7200),OBUF(2000),DNBUF(600)
	integer*2 buf(32000),obuf(32000),dnbuf(600)
      equivalence (buf(1),sbuf(1),obuf(1))

	integer*4 ioline
c	byte    sbuf(19072)
	integer*2 sbuf(32000)
      common /IOSTFF/ ioline,sbuf
c	var:   dev  + mean + cname + sbuf
c	size:  400 + 2400 + 400   + 19072 = 24272
c	start:   0   2401   4801     5301
        real*4 dev(12,maxclasses),mean(12,maxclasses)
        character*8 cname(maxclasses)
        integer*2 ln2(1204),sam2(1204),test(1204),ptbuf(100,2)
        integer*2 ln(300),samp(300)
      common /C1/ dev,mean,cname,ln2,sam2,test,ptbuf,ln,samp


      	integer*2 lnbuf(602),ssbuf(600),nsbuf(600)
      	integer*4 parm(2000),hisbuf(256,12)
	integer*4 hisbuf2(32767,12)
      COMMON /C2/ parm,lnbuf,ssbuf,nsbuf,hisbuf,hisbuf2
C
	integer*2 lxbuf(1202),s1buf(1200),ns1buf(1200)
      COMMON /C3/ lxbuf,s1buf,ns1buf
c
      data bnd/1,2,3,4,5,6,7,8,9,10,11,12/
      data verts/50*1/
      data hist/1,2,3,4,5,6,7,8,9,10,11,12/
      data spec/1,2,3,4,5,6,7,8,9,10,11,12/
C
C
      call ifmessage('STATS version 04-Jul-2011')		!30-MAY-02')
C
C
C        ZERO THE LARGE ARRAYS
      CALL ZIA(DEV,2400)
      CALL ZIA(PARM,8000)       
C
C
C
C        DEFAULT PARAMETERS
      MAXFLD= 600
      NHIST= 0
      NSPEC= 0
      NPLOT= 0
      MSS= .FALSE.
      NOPRNT= .FALSE.
      NEXCL= 0
      SCRIBE= .FALSE.
      ADD= .TRUE.
      STAT= .TRUE.
      ISCRIBE= 1
      FORM= 1
      XDN= 255
      XDS= 2
      smean = 0.0
      DO J=1,50
         splot(j)= j
      END DO
C

C        GET NUMBER OF INPUT AND OUTPUT DATA SETS
      call xvpcnt('INP',ni)
      call xvpcnt('OUT',no)

C        GET UNIT NUMBERS FOR ALL DATA SETS
      DO I=1,NI
         call xvunit(iunit(i),'INP',i,istat,' ')
         IRETURN = 1
         call xvsignal(iunit(i), istat, ireturn)
      END DO
      DO I=1,NO
         call xvunit(ounit(i),'OUT',i,istat,' ')
         IRETURN = 1
         call xvsignal(ounit(i), istat, ireturn)
      END DO

C        GET SIZE AND FORMAT OF PRIMARY INPUT AND SIZE FIELD INFO

      call xvopen(iunit(1),istat, 'OP', 'READ',
     +                 'OPEN_ACT','SA','IO_ACT','SA',' ')
      call xvget(iunit(1),istat,'FORMAT',format,'NL',nli,'NS',
     +                 nsi,' ')
      call xvclose(iunit(1),istat,' ')
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

c      IF(FORMAT.NE.'BYTE') THEN
c         CALL XVMESSAGE('??E - STATS accepts BYTE data only',' ')
c         CALL ABEND
c      END IF

C        GET PARAMETERS
      NUSE= NI                   ! NUSE IS THE NUMBER OF BANDS TO BE USED.

C        'MSS'
      nchan = 0
      call xvparm('MSS',parm,icount,idef,32)
      IF(PARM(1) .GT. 0) THEN
         MSS= .TRUE.
         NCHAN= PARM(1)          ! NCHAN IS THE NUMBER OF IMAGES MSSED.
         NUSE= NCHAN

C        'BAND'
        call xvparm('BAND',parm,icount,idef,32)
        IF(PARM(1) .GT. 0) THEN
           NUSE= ICOUNT
           DO J=1,NUSE
              BND(J)= PARM(J)
           END DO
        END IF
      END IF

C        'HIST'
      call xvparm('HIST',parm,icount,idef,32)
      IF(PARM(1) .EQ. 0)   THEN
        NHIST=NUSE
      ELSE IF (PARM(1) .NE. -1)  THEN
         NHIST= ICOUNT
         DO J=1,NHIST
            HIST(J)= PARM(J)
         END DO
      END IF

C        'SPEC'
      call xvparm('SPEC',parm,icount,idef,32)
      IF(PARM(1) .EQ. 0)   THEN
        NSPEC=NUSE
      ELSE IF (PARM(1) .NE. -1)  THEN
         NSPEC= ICOUNT
         DO J=1,NSPEC
            SPEC(J)= PARM(J)
         END DO
      END IF

C        'NOPR'
      if (xvptst('NOPRINT')) noprnt= .TRUE.
C        'EXCL'
      call xvparm('EXCLUDE',parm,icount,idef,32)
      IF (ICOUNT .GT. 0) THEN
         NEXCL= ICOUNT
         IF (NEXCL.GT.10)  NEXCL= 10
         DO J=1,NEXCL
            EXCLUD(J)= PARM(J)
         END DO
      END IF
     
C        'SPLOT'
      call xvparm('SPLOT',parm,icount,idef,32)
      IF (PARM(1) .GT. 0) THEN      
         NPLOT= ICOUNT
         DO J=1,NPLOT
            SPLOT(J)= PARM(J)
         END DO
      END IF

C        'SCRIBE'
      call xvparm('SCRIBE',parm,icount,idef,1)
      IF (PARM(1) .GT. 0) THEN
         SCRIBE=.TRUE.
         ISCRIBE=PARM(1)
      END IF

C        'VERT'
      call xvparm('VERT',parm,icount,idef,50)
      IF (PARM(1) .GT. 0) THEN
         DO J=1,ICOUNT
            IF(PARM(J) .LE. 50 .AND. PARM(J) .GE. 1) VERTS(PARM(J))=2
         END DO
      END IF

C        'DN'
      call xvparm('DN',parm,icount,idef,1)
      IF (ICOUNT .GT. 0) THEN
         XDN= PARM(1)
         ADD= .FALSE.
      END IF

c100   CONTINUE
      NSCHAN= NSI
      IF (MSS) NSCHAN= NSI/NCHAN
      MTRX= (NUSE*(NUSE+1))/2			!!number of cov elements in LL trianglar matrix
      NSO = 4*(NUSE+MTRX+3)
      IF (NO .GE. 2)  SCRIBE= .TRUE.

      IF (SCRIBE .AND. NO .LT. 2)  XDS= 1
      IF (NO .EQ. 0 .OR. XDS .EQ. 1)  STAT= .FALSE.
      OSCRIBE = OUNIT(XDS)			!output SCRIBED data set
      STATUNIT= OUNIT(1)

      IF (NHIST .GT. 0 .AND. SCRIBE)  THEN
         BUFFER1(1:47)='*** HISTOGRAMS & SCRIBING MUST BE PROCESSED SEP'
         BUFFER1(48:77)='ARATELY ... HISTOGRAMS DELETED'
         call xvmessage(buffer1,' ')
         NHIST= 0
      END IF
C
C        OPEN DATA SETS
c	print *,"open data sets"
      DO I=1,NI
        call xvopen(iunit(i),istat, 'OP', 'READ',
     +               'OPEN_ACT','SA','IO_ACT','SA',
     + 			'I_FORMAT',fmt(icode),'U_FORMAT',fmt(2),' ')
      END DO

c	prepare output statistics file
      IF (STAT) THEN
         NCLASSES=100
         NBANDS=NUSE
         INST=' '
         call istat_file_open(statunit,'WRITE',nclasses,nbands,inst,
     &                        istat)
         IF (ISTAT .LT. 0) call istat_signal(statunit,istat,1)
      END IF
      IF (SCRIBE)  THEN

C        COPY THE SCRIBE PICTURE

	call xvopen(oscribe, istat, 'OP', 'WRITE', 'O_FORMAT',fmt(icode),
     .        'U_FORMAT',fmt(2),'U_NL',nli,
     .           'U_NS',nschan,'OPEN_ACT','SA','IO_ACT','SA',' ')
	do i=1,maxfld
	   dnbuf(i) = XDN
	enddo
c      CALL ITLA(XDN,DNBUF,MAXFLD)
        PTR= 1
        IF (MSS)  PTR= NSCHAN*(ISCRIBE-1)+1
        DO 250 II=1,NLI
           IF (MSS)  THEN
              call xvread(iunit(1), sbuf, istat,' ')
           ELSE
              call xvread(iunit(iscribe), sbuf, istat,' ')
           END IF
           IF (ADD)  then
c		print *,"ADD format = ",format
	      if (format .eq. 'BYTE') then
c	print *,"BYTE1"
	          do i=1,nsi*nli
		      bbuf(I)= int2byte(sbuf(I))
		  enddo
c		  print *, "hstgnb"
		  call hstgnb(nschan,bbuf(ptr),hisbuf(1,1))
	      else
c		  print *, "hstgen"
		  call hstgen(nschan,sbuf(ptr),hisbuf2(1,1))
	      endif

	   ENDIF	!ADD
c	   print *, "xvwrit format = ",format
	   if (format .eq. 'BYTE') then
c		print *,"BYTE"
	        call xvwrit(oscribe, bbuf(ptr), istat,' ')
	   else
c		print *, "HALF"
                call xvwrit(oscribe, sbuf(ptr), istat,' ')
	   endif
250     CONTINUE
	if (format .eq. 'BYTE') then
           XMEAN(1)= 0.
           DO J=1,256
              XMEAN(1)= DFLOAT(J-1)*DFLOAT(HISBUF(J,1))+XMEAN(1)
           END DO
           SMEAN= XMEAN(1)/(DFLOAT(NSCHAN)*DFLOAT(NLI))
        else 
	   XMEAN(1)= 0.
           DO J=1,32767
              XMEAN(1)= DFLOAT(J-1)*DFLOAT(HISBUF2(J,1))+XMEAN(1)
           END DO
           SMEAN= XMEAN(1)/(DFLOAT(NSCHAN)*DFLOAT(NLI))
        endif

        call xvclose(oscribe, istat,' ')
        call xvopen(oscribe, istat, 'OP', 'UPDATE', 'O_FORMAT',fmt(icode),
     .       'U_FORMAT',fmt(2),'U_NL',nli,
     .           'U_NS',nschan,'OPEN_ACT','SA','IO_ACT','SA',' ')

      END IF 	!scribe
C
C        ZERO BUFFERS
      call zia(clspts,maxclasses)
      NCLS= 0
      PARNXT= 0
      CLSCNT= 0
C
C        LOAD POINTER BUFFERS
      K= MAXFLD

      IF (MSS)  K= NSCHAN
      DO J=1,NUSE
         PTRBF1(J)= K*(BND(J)-1)+1
         IF (.NOT. MSS)  PTRBUF(J)=PTRBF1(J)
      END DO

      IF (NEXCL .GT. 0)  call prnt(4,nexcl,exclud,
     & 'THE FOLLOWING DN ARE EXCLUDED FROM STATISTICS:.')

C    LOOP THROUGH THE CLASSnn PARAMETERS AND HANDLE THE SPECIFIED CLASSES.

      PNAME='CLASS'
      DO 800 ICLASS = 1,maxclasses

         IO = ICLASS
         IF (IO .LT. 10 ) THEN
           write(msg,421) io
421        format('CLASS',i1)
         ELSE IF ( IO .LT. 100 ) THEN
             write(msg,422) io
422          format('CLASS',i2)
         ELSE
             write(msg,423) io
423          format('CLASS',i3)
         END IF

         call xvparm( msg, parm, icount, idef,10)


         IF (ICOUNT .LE. 2)    GOTO 800
C
C        GET TRAINING FIELD ADDRESS
C
         FLDNUM=0
         CLSCNT=CLSCNT+1

C  GET THE CLASS NAME
         CLS=CLSCNT
         WRITE(CNAME(CLS),'(A,I3.3)') 'CLASS',CLSCNT

C  CHECK IF CLASS IS RECT (FORM=1) OR VERT (FORM=2)  

         FORM = VERTS(ICLASS)
         IF (NHIST .GT. 0)  call zia(hisbuf(1,1),3072)
         call zia(xmean,24)
         call zia(xcov,156)
         NCLS=CLS
         IF (.NOT. NOPRNT)  call heading(form,cls,cname(cls))

C...LOOP THROUGH THE TRAINING FIELDS FOR THIS TRAINING AREA.

         PAR = 1
         DO WHILE (PAR+3 .LE. ICOUNT)    ! THERE IS ANOTHER TRAINING FIELD IF
                                      ! THERE ARE AT LEAST 4 VALUES LEFT
c		print *,"PAR+3, ICOUNT = ",par+3,icount
         FLDNUM= FLDNUM+1                ! FLDNUM = NUMBER OF FIELDS IN TR. AREA.
c		print *,"FLDNUM = ",fldnum
         NPNTS= 1
         IF (FORM .EQ. 1)  GO TO 350
         PTR = PAR
         DO 325 I=1,(ICOUNT/2) - 1
            PTR= PTR+2         
            IF (PARM(PTR) .EQ. PARM(PAR) .AND. PARM(PTR+1) .EQ. PARM(PAR+1)) 
     .        GO TO 330                 ! MAKE SURE WE HAVE CLOSURE.

325      CONTINUE
         GO TO 960
330      NPNTS= I
C
350      call area(form,par,npnts,perpts)
         if (.not. noprnt) call pfield(form,par,npnts)
         if (scribe)  call outlin(perpts,nli,nschan)
         PARNXT= 2*(NPNTS+1)
C
C        READ FIELD,   GATHER STATS
         II= 0
400      II= II+1
         IF (LNBUF(II) .EQ. 0)  GO TO 7777
         SL= LNBUF(II)
         SS= SSBUF(II)
         NS= NSBUF(II)
         IF (NS .GT. MAXFLD)  NS= MAXFLD
         DO J=1,NI
            LINE2(J)= SL
         END DO
         CLSPTS(CLS)= CLSPTS(CLS)+NS
         IF (MSS)  THEN
            call xvread( iunit(1), sbuf, istat, 'LINE',line2(1),' ')
            DO J=1,NUSE
               PTRBUF(J)= PTRBF1(J)+SS-1
            END DO
         ELSE
ccc	print *,"iclass NUSE = ",iclass,nuse
            DO J=1,NUSE
               PTR= PTRBUF(J)
               call xvread( iunit(j), buf(ptr), istat, 'LINE',line2(j),
     .                    'SAMP',ss, 'NSAMPS', ns,' ')
            END DO
         END IF
         IF (NHIST.EQ.0)  GO TO 435
c	 print *,"iclass: NHIST = ",iclass,nhist
         DO J=1,NHIST
            K= HIST(J)
            PTR= PTRBUF(K)
ccc	print *, "k,ptr,nli = ",k,ptr,nli,format
            if (format .eq. 'BYTE') then
c	print *,"BYTE2"
	       if (MSS) then
c		  print *,"MSS..."
                  do i=1,nsi*nli
                     bbuf(I)= int2byte(sbuf(i))
                  enddo
	       else 
                  do i=1,nsi*nli
                     bbuf(I)= int2byte(buf(i))
                  enddo
	       endif
	       call hstgnb(ns,bbuf(ptr),hisbuf(1,k))
            else		!'BYTE'
	        if (MSS) then
		   call hstgen(ns,sbuf(ptr),hisbuf2(1,k))
	        else	
                   call hstgen(ns,buf(ptr),hisbuf2(1,k))
	        endif
            endif
c
c         CALL HSTGNB(NS,BUF(PTR),HISBUF(1,K))
c	print *,"iclass: HISBUF(1),(2),(3) ...",iclass,HISBUF(1,k),HISBUF(2,k),HISBUF(3,k)
         END DO
C
435      DO 460 J=1,NS
            JV= J-1
            DO K=1,NUSE
               PTR= PTRBUF(K)+JV
cc             IDN(K)= BYTE2INT(BUF(PTR))
	       IDN(k) = BUF(PTR)
            END DO
            IF (NEXCL.EQ.0)  GO TO 445
            DO 444 NX=1,NEXCL
               NXC= EXCLUD(NX)
               DO KK=1,NUSE
                  IF(IDN(KK).EQ.NXC)  GO TO 443
               END DO
               GO TO 444
443            CLSPTS(CLS)= CLSPTS(CLS)-1
               GO TO 460
444         CONTINUE
445         KL=0
            DO K=1,NUSE
               XMEAN(K)= XMEAN(K) + IDN(K)
               DO L=1,K
                  KL=KL+1
                  XCOV(KL)= XCOV(KL)+IDN(K)*IDN(L)
               END DO
            END DO
460      CONTINUE
         GO TO 400
C
C
C        DO THE SCRIBING ON THE PICTURE
7777     CONTINUE
         IF (SCRIBE)  THEN
C
C        SET OUTLINE DN
            IF (.NOT. ADD)  GO TO 739
            XDN= 0
            IF (XMEAN(ISCRIBE)/CLSPTS(CLS) .LT. SMEAN)  XDN= 255
	    do i=1,maxfld
	       dnbuf(i) = xdn
	    enddo
c      CALL ITLA(XDN,DNBUF,MAXFLD)
739         CONTINUE
C
C        SCRIBE FIELD ON PICTURE
            II= 0
740         II= II+1
            LX= LXBUF(II)
            IF (LX .EQ. 0)  GO TO 750
            call xvread( oscribe, obuf, istat, 'LINE', lx,' ')
            J1= S1BUF(II)
            J2= NS1BUF(II)
            IF (J1 .LT. 0 .OR. J2 .LT. 0)  GO TO 740
c mve 1 = byte to byte
c mve 3 = half to half
c           CALL MVE(1,J2,DNBUF,OBUF(J1),1,1)
	    call mve(3,j2,dnbuf,obuf(j1),1,1)
            call xvwrit( oscribe, obuf, istat, 'LINE', lx,' ')
            GO TO 740
750         CONTINUE

         END IF   !IF (SCRIBE)

         PAR = PAR + PARNXT

      END DO

         IF (PAR .NE. ICOUNT+1)  GOTO 960  
C
C
C        COMPUTE STATS
c600   CONTINUE
         PTS1= CLSPTS(CLS)
         PTS2= PTS1-1.
         IF (PTS1 .GE. 2.)  GO TO 610
         PTS1= 1.
         PTS2= 1.
610      PTS1= 1./PTS1
         PTS2= 1./PTS2
         KL= 0
         DO 630 J=1,NUSE
            DO 620 L=1,J
               KL= KL+1
               XCOV(KL)= (XCOV(KL)-XMEAN(J)*XMEAN(L)*PTS1)*PTS2
               COV(KL)= XCOV(KL)
620         CONTINUE
            IF (XCOV(KL) .GT. 0.0) THEN
	       DEV(J,CLS) = DSQRT(XCOV(KL))
	    ELSE
	       DEV(J,CLS) = 0.0
            END IF
630      CONTINUE
         KL= 0
         DO J=1,NUSE
	    MEAN(J,CLS) = XMEAN(J)*PTS1
	    DO L=1,J
	       KL= KL+1
	       IF (DEV(J,CLS) .EQ. 0.0 .OR. DEV(L,CLS) .EQ. 0.0) THEN
		      COR(KL)= 0.
		ELSE
		      COR(KL)= COV(KL)/(DEV(J,CLS)*DEV(L,CLS))
	       END IF
	    END DO
         END DO
         IF (NOPRNT)  GO TO 735
C
         WRITE (BUFFER,1003) CLSPTS(CLS)
         call xvmessage(buffer,' ')
         call xvmessage(' ',' ')
         WRITE (BUFFER,1004) CLS,CNAME(CLS)
         call xvmessage(buffer,' ')
         WRITE (BUFFER,1005) (I,I=1,NUSE)
         call xvmessage(buffer,' ')
         WRITE (BUFFER,1006) (MEAN(I,CLS),I=1,NUSE)
         call xvmessage(buffer,' ')
         WRITE (BUFFER,1007) (DEV(I,CLS),I=1,NUSE)
         call xvmessage(buffer,' ')
         call xvmessage(' ',' ')
         WRITE (BUFFER,1008)
         call xvmessage(buffer,' ')
         KL1= 1
         DO 730 J=1,NUSE
            KL1= KL1+J-1
            KL2= KL1+J-1
            WRITE (BUFFER,1009) (COV(I),I=KL1,KL2)
            call xvmessage(buffer,' ')
730      CONTINUE
         call xvmessage(' ',' ')
         WRITE (BUFFER,1011)
         call xvmessage(buffer,' ')
         KL1= 1
         DO 732 J=1,NUSE
            KL1= KL1+J-1
            KL2= KL1+J-1
            WRITE(BUFFER,1015) (COR(I),I=KL1,KL2)
            call xvmessage(buffer,' ')
732      CONTINUE
C
735	 continue
ccc	 print *,"hist,nhist,nexcl,cname(cls) ",hist,nhist,nexcl,cname(cls)
ccc        do mmm=1,nhist
ccc        do nnn=1,256
ccc          print *,"hisbuf2(i,1) = ",mmm,hisbuf2(nnn,mmm)
ccc        enddo
ccc        enddo

         IF (NHIST .GT. 0)  then
	    if (format .eq. 'BYTE') then
		call histgmb(hisbuf,hist,nhist,cname(cls),
     &                            nexcl,exclud)
	    else 
		call histgm(hisbuf2,hist,nhist,cname(cls),
     &                            nexcl,exclud)
	    endif
         ENDIF
         IF (STAT)  THEN
C
C        WRITE MEANS & COVARIANCE MATRIX ON STAT DATA SET
 
            NN= 4*NUSE
            NM= 4*MTRX
c mve 7 = real to real
            call mve(7,nuse,mean(1,cls),means,1,1)
            call mve(7,1,clspts(cls),npix,1,1)
            call mve(7,mtrx,cov(1),covariance,1,1)
            call istat_record_write(statunit,iclass,cname(cls),npix,nbands,
     &                           means,covariance,istat)
            if (istat .lt. 0) call istat_signal(statunit,istat,1)
         END IF

800   CONTINUE
      IF (NCLS .EQ. 0) GOTO 970        ! IF NO CLASSES.
C
C  PRINT THE CLASS TABLE
      call xvmessage(' ',' ')
      call xvmessage(' CLASS NUMBERS ASSIGNED',' ')
      call xvmessage(' ----------------------',' ')
      DO 805 J=1,NCLS
         WRITE(BUFFER,6002) J,CNAME(J)
6002  FORMAT(I6,' = ',A)
         call xvmessage(buffer,' ')
805   CONTINUE
C
      IF (NPLOT .EQ. 0)  NPLOT= NCLS
      IF (NSPEC .GT. 0)  call spectl(spec,splot,nspec,nplot)

      IF (SCRIBE)  call xvclose(oscribe, istat,' ')
C
      IF (STAT)  THEN
         call istat_file_info (statunit,nclasses,nbands,ibis)
         call ibis_file_set (ibis,'NR',ncls,istat)
         if (istat .ne. 1) call ibis_signal(ibis,istat,1)
         call istat_file_close(statunit,istat,1)
         if (istat .lt. 0) call istat_signal(statunit,istat,1)
      END IF
      RETURN

960   WRITE(BUFFER,1010) ICLASS
      call xvmessage(buffer,' ')
      call abend 
970   call xvmessage('??E -  No training areas specified',' ')
      call abend 
C
C
C
c1000  FORMAT('THE FOLLOWING DN ARE EXCLUDED FROM STATISTICS ',
c     &    10(I5,','))
1003  FORMAT(5X,'TOTAL POINTS =',I7)
      call xvmessage(' ',' ')
1004  FORMAT(' STATISTICS FOR CLASS # ',I2,'   "',A,'"')
1005  FORMAT('   CHANNEL',4X,12(I2,7X))
1006  FORMAT('    MEAN ',12F9.2)
1007  FORMAT('   ST DEV',12F9.2)
      call xvmessage(' ',' ')
1008  FORMAT('   COVARIANCE MATRIX')
1009  FORMAT(9X,12F9.2)
1010  FORMAT('*** PARAMETER ERROR FOR CLASS',I3)
      call xvmessage(' ',' ')
1011  FORMAT('   CORRELATION MATRIX')
c1012  FORMAT('EXTERNAL PARAMETERS TO BE SUPPLIED')
c1013  FORMAT('*** KEYWORD -TRAIN- NOT SPECIFIED')
c1014  FORMAT('... WARNING---HISTOGRAMS AND SCRIBING CANNOT BOTH BE DONE
c     & IN THE SAME EXECUTION',/'              HISTOGRAMS DELETED')
1015  FORMAT(9X,12F9.4)
C
      END
C====================================================================
      subroutine heading(form,cls,cname)
c      IMPLICIT INTEGER (A-Z)
	implicit none
	integer*4 form,par,par1,npnts,cls,kend,sl,ss,nl,ns,npt2
	integer*4 i,k	
      character*132 buffer
c      INTEGER*4 PARM(2000)
      character*8 cname

        integer*4 ioline
        integer*2    sbuf(32000)
      common /IOSTFF/ ioline,sbuf

        integer*2 lnbuf(602),ssbuf(600),nsbuf(600)
        integer*4 parm(2000),hisbuf(256,12)
        integer*4 hisbuf2(32767,12)
      COMMON /C2/ parm,lnbuf,ssbuf,nsbuf,hisbuf,hisbuf2

c---      COMMON /C2/ PARM,FIL(3973)
C
C        PRINT THE CLASS HEADING
C
      call xvmessage(' ',' ')
      WRITE(BUFFER,1001) CLS,CNAME
      call xvmessage(buffer,' ')
      IF (FORM .EQ. 1)  RETURN
      WRITE(BUFFER,1003)
      call xvmessage(buffer,' ')
      WRITE(BUFFER,1004)
      call xvmessage(buffer,' ')
      RETURN
C
C
C
      entry pfield(form,par,npnts)
      GO TO (100,200,300),FORM
C
100   SL= PARM(PAR)
      SS= PARM(PAR+1)
      NL= PARM(PAR+2)
      NS= PARM(PAR+3)
      WRITE(BUFFER,1002) SL,SS,NL,NS
      call xvmessage(buffer,' ')
      RETURN
C
200   NPT2= 2*(NPNTS+1)
      PAR1= PAR-1
      DO 250 K=1,NPT2,14
         KEND = MIN( K+13, NPT2 )
         WRITE(BUFFER,1005) (PARM(PAR1+I),PARM(PAR+I),I=K,KEND,2)
         call xvmessage(buffer(2:132),' ')
250   CONTINUE
      RETURN
C
300   RETURN
C
C
1001  FORMAT('TRAINING AREAS FOR CLASS # ',I2,'   "',A,'"')
1002  FORMAT(9X,'SL=',I5,'   SS=',I5,'   NL=',I5,'   NS=',I5)
1003  FORMAT('IRREGULAR AREA VERTICES')
1004  FORMAT(7('LINE SAMPLE     '))
1005  FORMAT(7(2I6,4X))
      END
C=========================================================================
      subroutine area(form,par,nopts,perpts)
c      IMPLICIT INTEGER (A-Z)
	implicit none
c      INTEGER*2 LNBUF(602),SSBUF(600),NSBUF(600)
c      INTEGER*2 LN2(1204),SAM2(1204),TEST(1204),PTBUF(100,2)
c      INTEGER*2 LN(300),SAMP(300)
        integer*4 maxclasses
        parameter (maxclasses = 50)

	integer*2 temp,temp2

      logical*4 samel,lpos
      real*4 del,a,del1
c	INTEGER*4 FIL2
c      	INTEGER*4 DUMMY(1300)

      integer*4 maxln, maxpts, keep
	integer*4 form,par,nopts,perpts,sl,ss,nl,ns,next1,line
	integer*4 strt,tnum,inc,inc1,endcnt,del2,compr
	integer*4 b,d,e,f,ff,g,h,i,j,jj,k,l,n,qqq,r,sss,xxx

        integer*4 ioline
        integer*2    sbuf(32000)
      common /IOSTFF/ ioline,sbuf

c       var:   dev  + mean + cname + sbuf
c       size:  400 + 2400 + 400   + 19072 = 24272
c       start:   0   2401   4801     5301
        real*4 dev(12,maxclasses),mean(12,maxclasses)
        character*8 cname(maxclasses)
        integer*2 ln2(1204),sam2(1204),test(1204),ptbuf(100,2)
	integer*2 ln(300),samp(300)
      common /C1/ dev,mean,cname,ln2,sam2,test,ptbuf,ln,samp

        integer*2 lnbuf(602),ssbuf(600),nsbuf(600)
        integer*4 parm(2000),hisbuf(256,12)
        integer*4 hisbuf2(32767,12)
      common /C2/ parm,lnbuf,ssbuf,nsbuf,hisbuf,hisbuf2

c      COMMON /C1/ FIL(2562),DUMMY,LN2,SAM2,TEST,PTBUF,LN,SAMP
c      COMMON /C2/ PARM,LNBUF,SSBUF,NSBUF,FIL2(3072)

      data maxln/600/
      data maxpts/300/
C
C
C        TRAINING AREA FORMATS
C     FORM=1   RECTANGULAR FORMAT
C     FORM=2   VERTICES FORMAT
C
	KEEP = 1			! some problem maybe here
      GO TO (100,400),FORM
C
C        RECTANGULAR FORMAT
100   SL= PARM(PAR)
      SS= PARM(PAR+1)
      NL= PARM(PAR+2)
      NS= PARM(PAR+3)
      IF (NS .GT. MAXLN .OR. NL .GT. MAXLN)  GO TO 901
      DO 150 I=1,NL
         LNBUF(I)= SL+I-1
         SSBUF(I)= SS
      	 NSBUF(I)= NS
150	continue
      LNBUF(NL+1)= 0
C
C     GENERATE PERIMETER POINTS FOR OUTLIN
C
      LN2(1)=SL
      SAM2(1)=SS
      TEST(1)=1
      QQQ=NL+1
      DO 1 K=2,QQQ
         LN2(K)=SL+K-2
         SAM2(K)=SS+NS-1
         TEST(K)=2
1	continue
      XXX= NL
      DO 2 K=1,XXX
         SAM2(K+QQQ)=SS
         TEST(K+QQQ)=1
2     LN2(K+QQQ)=NL+SL-K
      PERPTS=2*NL+1
      RETURN
C
C
400   JJ= PAR-2
      IF (NOPTS .GT. MAXPTS)  GO TO 902
      DO 410 J=1,NOPTS
         JJ= JJ+2
         LN(J)= PARM(JJ)
410   SAMP(J)= PARM(JJ+1)
C
C
C        THE FOLLOWING CODE WRITTEN BY BOB BEGGS   8/74
C        (AREA COMPUTATION FOR ARBITRARY POLYGON)
      I=1
      CALL ZIA(TEST,601)
      DO 550 J=1,NOPTS
         LPOS=.FALSE.
         SAMEL=.FALSE.
         IF (J .EQ. NOPTS) GO TO 420
         GO TO 430
420      LN(J+1)=LN(1)
         SAMP(J+1)=SAMP(1)
C
C     PUT THE INPUT POINTS INTO THE PERIMETER ARRAYS
C
430      LN2(I)=LN(J)
         SAM2(I)=SAMP(J)
         IF (LN(J+1) .EQ. LN(J)) GO TO 450
C
C     TEST IS EITHER 1 (START OF A LINE SEGMENT) , 2 (END OF A LINE
C     SEGMENT) , OR 3 ( A SINGLE POINT)
C
C     SEE IF LINE IS GOING UP OR DOWN
C
         IF (LN(J+1) .LT. LN(J)) GO TO 440
C
C     TNUM IS THE TEST NUMBER FOR THE NEXT SET OF INTERPOLATED POINTS
         TNUM=2
         LPOS=.TRUE.
         GO TO 470
C
440      TNUM=1
         GO TO 470
C
450      SAMEL=.TRUE.
         IF (SAMP(J) .LT. SAMP(J+1)) GO TO 460
         TNUM=2
         TEST(I)=2
         GO TO 470
C
460      TNUM=1
         TEST(I)=1
C
470       I=I+1
C
C
         IF (SAMEL) GO TO 550
C
C
C     INTERPOLATION
C
         DEL2=LN(J+1)-LN(J)
C     DEL2 IS THE DIFFERENCE BETWEEN INPUT POINTS
C
         IF (DEL2 .LT. 0) DEL2=0-DEL2
         A=SAMP(J+1)-SAMP(J)
         DEL=A/(LN(J+1)-LN(J))
C
         INC=1
         IF (.NOT. LPOS) GO TO 500
         GO TO 510
C
C     SET UP INITIAL INCREMENTS
500      INC=-1

         DEL=0-DEL
510      DEL1=DEL
         INC1=INC
C
C     CHECK FOR PEAKS AND VALLEYS IN THE FIGURE
C     THEY WILL BE SINGLE PIXELS AND SO WILL HAVE A TEST OF 3
C
         IF (J .EQ. 1) GO TO 535
         FF=LN(J-1)-LN(J)
         IF (FF .LT. 0) FF=0-FF
         COMPR=FF*DEL+SAMP(J)+.5
         IF (LN(J) .LT. LN(J+1) .AND. LN(J) .LT. LN(J-1)) GO TO 520
         IF (LN(J) .GT. LN(J+1) .AND. LN(J) .GT. LN(J-1)) GO TO 525
         GO TO 530
520      IF (SAMP(J-1) .LE. COMPR) GO TO 535
         GO TO 530
525      IF (SAMP(J-1) .GE. COMPR) GO TO 535
530      TEST(I-1)=TNUM
         GO TO 540
535      TEST(I-1)=3
C
540      IF (DEL2 .LT. 2) GO TO 550
         DO 545 L=2,DEL2
            LN2(I)=LN(J)+INC1
            SAM2(I)=SAMP(J)+DEL1+.5
            INC1=INC1+INC
            DEL1=DEL+DEL1
            TEST(I)=TNUM
            I=I+1
545      CONTINUE
550   CONTINUE
C
C     SET UP SORTING ARRAYS
C
C     THIS IS ALSO THE  MAIN LOOP
C
      H=0
      DO 680 B=1,MAXLN
C     SORT THINGS ONE LINE AT A TIME
C
         D=0
         LINE=LN(1)+B-1
C
C     FIND PERIMETER POINTS IN EACH LINE AND PUT THEM IN A 2 DIMENSIONAL
C     ARRAY
C
         PERPTS=I
         DO 570 N=1,I
            IF (LN2(N) .NE. LINE) GO TO 570
            D=D+1
            PTBUF(D,1)=TEST(N)
            PTBUF(D,2)=SAM2(N)
570      CONTINUE
C
C     IF THERE ARE NO POINTS LEFT BREAK OUT OF LOOP AND END
C
         IF (D .EQ. 0) GO TO 690
         IF (D .EQ. 1) GO TO 630
C
C     SINKING SORT
C     PUT THE PERIMETER POINTS IN SAMPLE NUMBER ORDER
C
         G=D
         DO 620 SSS=2,G
            E=2
580         IF (PTBUF(E,2) .GT. PTBUF(E-1,2)) GO TO 610
            IF (PTBUF(E,2) .EQ. PTBUF(E-1,2)) GO TO 590
            TEMP=PTBUF(E-1,2)
            TEMP2=PTBUF(E-1,1)
            PTBUF(E-1,2)=PTBUF(E,2)
            PTBUF(E-1,1)=PTBUF(E,1)
            PTBUF(E,2)=TEMP
            PTBUF(E,1)=TEMP2
            GO TO 610
C
C     IF TWO POINTS ARE THE SAME MAKE THEM INTO ONE SINGLE
590         PTBUF(E-1,1)=3
            D=D-1
            IF (E .GT. D) GO TO 620
            DO 600 R=E,D
               PTBUF(R,2)=PTBUF(R+1,2)
600         PTBUF(R,1)=PTBUF(R+1,1)
610         E=E+1
            IF (E .LE. D) GO TO 580
620      CONTINUE
C
C     SET UP OUTPUT ARRAYS
C
C     STRT IS THE NUMBER OF STARTS(1) ENCOUNTERED
630      STRT=0
         ENDCNT=0
         DO 675 F=1,D
            IF (PTBUF(F,1) .EQ. 1) GO TO 650
            IF (PTBUF(F,1) .EQ. 2) GO TO 670
            IF (STRT .EQ. 0) GO TO 640
C
C     IF A START AND END HAVE BEEN FOUND BEFORE A SINGLE PUT THE LINE
C     SEGMENT IN THE OUTPUT ARRAYS
C
            LNBUF(H)=LINE
            NSBUF(H)=PTBUF(KEEP,2)-SSBUF(H)+1   !originally this is first time for keep
            STRT=0
            ENDCNT=0
C
C     PUT THE SINGLE IN THE OUTPUT ARRAYS
C
640         H=H+1
            SSBUF(H)=PTBUF(F,2)
            LNBUF(H)=LINE
            NSBUF(H)=1
            GO TO 675
C
650         STRT=STRT+1
            IF (STRT .LT. 2) GO TO 660
            IF (ENDCNT .EQ. 0) GO TO 675
            NEXT1=PTBUF(KEEP,2)+1
            IF (NEXT1 .EQ. PTBUF(F,2)) GO TO 831
C
C     IF A SECOND START IS FOUND AND THERE IS AN END BEFORE IT PUT THE
C     SEGMENT IN THE OUTPUT ARRAYS
            LNBUF(H)=LINE
            NSBUF(H)=PTBUF(KEEP,2)-SSBUF(H)+1
            STRT=1
C     KEEP THE START,S LOCATION
            H=H+1
            SSBUF(H)=PTBUF(F,2)
831         ENDCNT=0
            GO TO 675
C
660         H=H+1
            SSBUF(H)=PTBUF(F,2)
            GO TO 675
C
C     KEEP THE LAST END,S LOCATION
C
670         ENDCNT=ENDCNT+1
            KEEP=F
675      CONTINUE
         IF (STRT .EQ. 0) GO TO 680
         LNBUF(H)=LINE
         NSBUF(H)=PTBUF(KEEP,2)-SSBUF(H)+1
680   CONTINUE
690   LNBUF(H+1)= 0
C
C
      RETURN
C
901   call xvmessage('??E - Maximum of 600 lines or samples exceeded',' ')
      GO TO 999
902   call xvmessage('??E - MAXIMUM OF 300 CONTOUR POINTS EXCEEDED',' ')
999   call abend   !   RETURN 2
      end
C==============================================================================
      subroutine outlin(npts, nl, ns)
c      IMPLICIT INTEGER (A-Z)
	implicit none
        integer*4 maxclasses
        parameter (maxclasses = 50)

	integer*4 npts,nl,ns
	integer*4 i,j,k,l
c      INTEGER*2 LN2(1204),SAM2(1204),TEST(1204)
c      INTEGER*2 LXBUF(1202),S1BUF(1200),NS1BUF(1200)
c      INTEGER*4 DUMMY2(2901)

        integer*4 ioline
        integer*2 sbuf(32000)
      common /IOSTFF/ ioline,sbuf

        real*4 dev(12,maxclasses),mean(12,maxclasses)
        character*8 cname(maxclasses)
        integer*2 ln2(1204),sam2(1204),test(1204),ptbuf(100,2)
        integer*2 ln(300),samp(300)
      common /C1/ dev,mean,cname,ln2,sam2,test,ptbuf,ln,samp

        integer*2 lnbuf(602),ssbuf(600),nsbuf(600)
        integer*4 parm(2000),hisbuf(256,12)
        integer*4 hisbuf2(32767,12)
      common /C2/ parm,lnbuf,ssbuf,nsbuf,hisbuf,hisbuf2

        integer*2 lxbuf(1202),s1buf(1200),ns1buf(1200)
      common /C3/ lxbuf,s1buf,ns1buf

c      COMMON /C1/ FIL(2962),DUMMY,LN2,SAM2,TEST
c	FIL2(1271) was padded incorrectly from IBM system in 1984
c      COMMON /C2/ FIL2(1271),DUMMY2,LXBUF,S1BUF,NS1BUF
C
C     "OUTLIN" USES THE BUFFERS LOADED BY "AREA" WHICH CONTAIN THE
C     PERIMETER POINTS IN ORDER CLOCKWISE AROUND THE FIGURE
C
C
C     BASICALLY, OUTLIN DOES A LOT OF TESTS AND FIGURES OUT WHAT
C     THE PERIMETER OF THE CONTOUR IS DOING.   THEN IT PUTS THE
C     OUTLINE POINT(S) FOR THAT PERIMETER POINT INTO LXBUF, S1BUF,
C     AND NS1BUF
C
      K=1
C
C     MAIN LOOP
C
      NPTS=NPTS-1
C     CLOSE CONTOUR
      LN2(NPTS+1)=LN2(1)
      SAM2(NPTS+1)=SAM2(1)
      TEST(NPTS+1)=TEST(1)
      DO 10 J=1,NPTS
         L=TEST(J)
         GO TO (11,12,13),L
C
C     DO THE STARTS OF LINE SEGMENTS
C     MOST OF THE SPECIAL CASES ARE HANDLED IN THIS SECTION
C
11       LXBUF(K)=LN2(J)
         IF (J .EQ. 1) GO TO 6
         IF (J .EQ. 2) GO TO 50
         IF (TEST(J-1) .EQ. 2)GO TO 20
         IF (TEST(J-2) .EQ. 2 .AND. LN2(J-2) .NE. LN2(J) .AND. SAM2(J-2) .LT. SAM2(J)) GO TO 41
50       S1BUF(K)=SAM2(J-1)-1
         NS1BUF(K)=SAM2(J)-SAM2(J-1)+1
         IF (J .EQ. 2) GO TO 19
         IF (TEST(J-2) .NE. 2 .OR. SAM2(J) .LT. SAM2(J-2) .OR. TEST(J-1) .EQ. 3) GO TO 91
         S1BUF(K)=SAM2(J-2)+1
         NS1BUF(K)=SAM2(J)-SAM2(J-2)-1
C
91       IF (J .LE. 4) GO TO 19
         IF (TEST(J-4) .NE. 2 .OR. LN2(J-4) .NE. LN2(J) .OR. TEST(J-2) .EQ. 3) GO TO 19
         S1BUF(K)=SAM2(J-4)+1
         NS1BUF(K)=SAM2(J)-SAM2(J-4)-1
19       IF (TEST(J+1) .EQ. 2) GO TO 16
C
C     SEE IF SLOPE IS NEGATIVE OR POSITIVE
C
         IF(SAM2(J).LE.SAM2(J+1).AND.SAM2(J).GE.SAM2(J-1))GO TO 15
         IF(SAM2(J).LT.SAM2(J-1).AND.SAM2(J).LT.SAM2(J+1))GO TO 101
         S1BUF(K)=SAM2(J+1)-1
         NS1BUF(K)=SAM2(J)-SAM2(J+1)+1
         IF (LN2(J) .EQ. LN2(J+1) .AND. TEST(J+1) .EQ. 1) LXBUF(K)=LN2(J)+1
         IF (J+4 .GT. NPTS) GO TO 15
         IF (SAM2(J+1) .NE. SAM2(J+3) .OR. LN2(J) .NE. LN2(J+4)) GO TO 15
         S1BUF(K)=SAM2(J+4)+1
         NS1BUF(K)=SAM2(J)-SAM2(J+4)-1
         GO TO 15
C
C     DO FIRST POINT
C
6        S1BUF(K)=SAM2(NPTS)-1
         NS1BUF(K)=SAM2(J)-SAM2(NPTS)+1
         IF (SAM2(1) .GE. SAM2(NPTS)) GO TO 16
         S1BUF(K)=SAM2(J)-1
         NS1BUF(K)=1
         GO TO 16
C
16       IF (LN2(J+1) .NE. LN2(J)) GO TO 32
C
C     OVERLINE THE SEGMENT
C
         K=K+1
         LXBUF(K)=LN2(J)-1
         S1BUF(K)=SAM2(J)-1
         NS1BUF(K)=SAM2(J+1)-SAM2(J)+3
         GO TO 15
C
C     UNDERLINE AN INTERIOR SEGMENT
C
32       I=J
         IF (LN2(J+1) .EQ. LN2(J+2)) I=I+1
         S1BUF(K)=SAM2(I+2)+1
         NS1BUF(K)=SAM2(J)-SAM2(I+2)-1
         GO TO 15
C
20       IF (TEST(J+1) .EQ. 2 .AND. LN2(J+1) .EQ. LN2(J)) GO TO 102
         IF (LN2(J-1) .NE. LN2(J)) GO TO 15
C
C     UNDERLINE THE SEGMENT
C
         LXBUF(K)=LN2(J)+1
         S1BUF(K)=SAM2(J)-1
         NS1BUF(K)=SAM2(J-1)-SAM2(J)+3
         K=K+1
         LXBUF(K)=LN2(J)
         GO TO 50
C
C     OVERLINE INTERIOR SEGMENTS
C
41       LXBUF(K)=LN2(J-2)
         S1BUF(K)=SAM2(J-2)+1
         NS1BUF(K)=SAM2(J+1)-SAM2(J-2)-1
         GO TO 15
C
C     DO THE CASE WHERE THE OUTLINE GOES LIKE THIS:  <
C
101      S1BUF(K)=SAM2(J)-1
         NS1BUF(K)=1
         GO TO 15
C
102      LXBUF(K)=LN2(J)-1
         S1BUF(K)=SAM2(J-1)+1
         NS1BUF(K)=SAM2(J+1)-SAM2(J-1)+1
C
15       IF (NS1BUF(K) .LT. 1) GO TO 10
         GO TO 111
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     DO ENDS
C
12       LXBUF(K)=LN2(J)
C
C     SKIP CASES ALREADY HANDLED IN FIRST SECTION
C
         IF (TEST(J+1) .EQ. 1 .AND. LN2(J) .NE. LN2(J+1)) GO TO 10
         IF (TEST(J-1) .EQ. 1 .AND. LN2(J) .GT. LN2(J-1)) GO TO 10
         IF (J .LE. 2) GO TO 90
         IF (TEST(J-2) .EQ. 1 .AND. TEST(J-1) .NE. 3 .AND. SAM2(J-2) .GE. SAM2(J)) GO TO 10
         IF (J .LE. 3) GO TO 90
         IF (TEST(J-3) .EQ. 1 .AND. TEST(J-2) .NE. 3 .AND. SAM2(J-3) .GT. SAM2(J) .AND.
     *LN2(J) .EQ. LN2(J-3)) GO TO 10
90       S1BUF(K)=SAM2(J)+1
         NS1BUF(K)=SAM2(J+1)-SAM2(J)+1
         IF (J+5 .GT. NPTS) GO TO 351
         IF (TEST(J+4) .EQ. 1 .AND. LN2(J) .EQ. LN2(J+4) .AND. SAM2(J+5) .GT. SAM2(J-1
     *))NS1BUF(K)=NS1BUF(K)-1
C
C     SEE IF SLOPE IS NEGATIVE OR POSITIVE
C
351      IF (SAM2(J) .LE. SAM2(J+1) .AND. SAM2(J) .GE. SAM2(J-1)) GO TO 115
         S1BUF(K)=SAM2(J)+1
         NS1BUF(K)=SAM2(J-1)-SAM2(J)+1
         IF (J .LT. 5) GO TO 352
         IF (TEST(J-3) .EQ. 1 .AND. LN2(J) .EQ.  LN2(J-4)) NS1BUF(K)=NS1BUF(K)-1
352      IF (LN2(J) .EQ. LN2(J-1) .AND. TEST(J-1) .EQ. 2) LXBUF(K)=LN2(J)+1
C
C     ACCOUNT FOR THE CASE WHERE THE OUTLINE GOES LIKE THIS:  >
         IF (SAM2(J) .GT. SAM2(J-1) .AND. SAM2(J) .GT. SAM2(J+1)) NS1BUF(K)=1
115      IF (NS1BUF(K) .LE. 0) GO TO 10
         GO TO 111
C
C
C     DO SINGLE POINTS
C     IN THESE CASES THE OUTLINE WILL LOOK LIKE THIS:  V
C     OR WILL LOOK LIKE IT UPSIDE DOWN
C
13       LXBUF(K)=LN2(J)
         IF (LN2(J) .GT. LN2(J+1)) GO TO 70
         IF(J .EQ. 1) GO TO 75
         S1BUF(K)=SAM2(J-1)-1
         NS1BUF(K)=SAM2(J)-SAM2(J-1)+1
         GO TO 76
C
75       S1BUF(K)=SAM2(NPTS)-1
         NS1BUF(K)=SAM2(J)-SAM2(NPTS)+1
         IF (SAM2(NPTS) .LE. SAM2(J)) GO TO 79
         GO TO 77
C
76       IF (SAM2(J-1) .LE. SAM2(J)) GO TO 79
77       S1BUF(K)=SAM2(J)-1
         NS1BUF(K)=1
79       K=K+1
         LXBUF(K)=LN2(J)
         S1BUF(K)=SAM2(J)+1
         NS1BUF(K)=SAM2(J+1)-SAM2(J)+1
         IF (SAM2(J+1) .LT. SAM2(J)) NS1BUF(K)=1
         K=K+1
         LXBUF(K)=LN2(J)-1
         GO TO 71
C
70       S1BUF(K)=SAM2(J+1)-1
         NS1BUF(K)=SAM2(J)-SAM2(J+1)+1
         IF (SAM2(J+1) .LE. SAM2(J)) GO TO 92
         S1BUF(K)=SAM2(J)-1
         NS1BUF(K)=1
92       K=K+1
         LXBUF(K)=LN2(J)
         S1BUF(K)=SAM2(J)+1
         NS1BUF(K)=SAM2(J-1)-SAM2(J)+1
         IF (SAM2(J-1) .GE. SAM2(J)) GO TO 93
         S1BUF(K)=SAM2(J)+1
         NS1BUF(K)=1
93       K=K+1
         LXBUF(K)=LN2(J)+1
71       S1BUF(K)=SAM2(J)-1
         NS1BUF(K)=3
111      K=K+1
10    CONTINUE
      LXBUF(K+1)= 0
      S1BUF(K+1)= 0
      NS1BUF(K+1)= 0
C
C
C        MAKE SURE THE SCRIBE IS WITHIN THE PICTURE BOUNDARY
      K= 0
1001  K= K+1
      IF (LXBUF(K) .EQ. 0 .AND. S1BUF(K) .EQ.0 .AND. NS1BUF(K) .EQ. 0) GO TO 1020
      IF (LXBUF(K) .LE. 0)  LXBUF(K)= 1
      IF (LXBUF(K) .GT. NL)  LXBUF(K)= NL
      IF (S1BUF(K) .GT. 0)  GO TO 1003
      NS1BUF(K)= NS1BUF(K)+S1BUF(K)-1
      S1BUF(K)= 1
1003  IF (S1BUF(K) .LE. NS)  GO TO 1005
      S1BUF(K)= 0
      GO TO 1007
1005  IF (S1BUF(K)+NS1BUF(K) .LE. NS+1)  GO TO 1007
      NS1BUF(K)= 2*NS1BUF(K)+S1BUF(K)-NS-1
1007  CONTINUE
      GO TO 1001
C
1020  CONTINUE
      RETURN
      END
C============================================================================
      subroutine spectl(spec,splot,nspec,nplot)
c      IMPLICIT INTEGER (A-Z)
	implicit none
        integer*4 maxclasses
        parameter (maxclasses = 50)

	integer*4 nspec,nplot,center,left,line_count,right
	integer*4 i,ii,j,jj,k
      integer*4 spec(12),splot(50)
      character*132 buffer
      character*1 blank(132)
      character*1 bar,dash

        real*4 dev(12,maxclasses),mean(12,maxclasses)
        character*8 cname(maxclasses)
        integer*2 ln2(1204),sam2(1204),test(1204),ptbuf(100,2)
        integer*2 ln(300),samp(300)
      common /C1/ dev,mean,cname,ln2,sam2,test,ptbuf,ln,samp

c      COMMON /C1/ DEV,MEAN,FIL(4868)
C
      data blank/132*' '/
      data bar/'|'/
      data dash/'-'/
C
      call xvmessage('                                                  
     +   SPECTRAL PLOT',' ')
      LINE_COUNT = 1
C					plot each requested spectral band
      DO I=1,NSPEC
	  LINE_COUNT = LINE_COUNT+NPLOT+12
	  IF (LINE_COUNT .GT. 60) THEN
	      call xvmessage('1',' ')
	      LINE_COUNT = NPLOT+13
	  END IF
	  II = SPEC(I)
	  Call xvmessage(' ',' ')
	  call xvmessage(' ',' ')
	  call xvmessage(' ',' ')
	  call xvmessage(' ',' ')
          BUFFER(1:12) = ' C     BAND '
          WRITE (BUFFER(13:14),'(I2)') II
          call xvmessage(buffer(2:14),' ')
	  call xvmessage('l            +/- 1 STANDARD DEVIATION',' ')
	  call xvmessage('A',' ')
	  call xvmessage('S',' ')
          buffer(1:46)='S 0   10   20   30   40   50   60   70   80   '
          buffer(47:91)='90   100  110  120  130  140  150  160  170  '
          buffer(92:131)='180  190  200  210  220  230  240  250  '
          call xvmessage(buffer,' ')
c	  CALL XVMESSAGE('S 0   10   20   30   40   50   60   70   80   90
c     +   100  110  120  130  140  150  160  170  180  190  200  210  220
c     +  230  240  250  ',' ')

          buffer(1:46)='--|----|----|----|----|----|----|----|----|---'
          buffer(47:88)='-|----|----|----|----|----|----|----|----|'
          buffer(89:131)='----|----|----|----|----|----|----|----|---'
          call xvmessage(buffer,' ')
c	  CALL XVMESSAGE('--|----|----|----|----|----|----|----|----|----|
c     +----|----|----|----|----|----|----|----|----|----|----|----|----|-
c     +---|----|----|---',' ')
C
C						plot each requested class
C
	  DO J=1,NPLOT
	      JJ = SPLOT(J)
C              CALL MVLC(BLANK,BUFFER(1:132),132)
              BUFFER = ' '
              WRITE (BUFFER(1:3),'(I3)') J
	      LEFT = (MEAN(II,JJ)-DEV(II,JJ)+9)/2.0
	      RIGHT = (MEAN(II,JJ)+DEV(II,JJ)+9)/2.0
	      LEFT = MIN(132,MAX(4,LEFT))
	      RIGHT = MIN(132,MAX(4,RIGHT))
	      DO K=LEFT+1,RIGHT-1
                  BUFFER(K:K) = '-'
	      END DO
              BUFFER(LEFT:LEFT) = '|'
              BUFFER(RIGHT:RIGHT) = '|'
	      IF (JJ.LT.10) THEN
		      CENTER = (MEAN(II,JJ)+9.0)/2.0
                      WRITE (BUFFER(CENTER-0:CENTER),'(I1)') JJ
		  ELSE
		      CENTER = (MEAN(II,JJ)+10.0)/2.0
                      WRITE (BUFFER(CENTER-1:CENTER),'(I2)') JJ
	      END IF
              call xvmessage(buffer(2:132),' ')
	  END DO
          buffer(1:46)='--|----|----|----|----|----|----|----|----|---'
          buffer(47:88)='-|----|----|----|----|----|----|----|----|'
          buffer(89:131)='----|----|----|----|----|----|----|----|---'
          call xvmessage(buffer,' ')
c	  CALL XVMESSAGE('--|----|----|----|----|----|----|----|----|----|
c     +----|----|----|----|----|----|----|----|----|----|----|----|----|-
c     +---|----|----|---',' ')

          buffer(1:46)='  0   10   20   30   40   50   60   70   80   '
          buffer(47:91)='90   100  110  120  130  140  150  160  170  '
          buffer(92:131)='180  190  200  210  220  230  240  250  '
          call xvmessage(buffer,' ')
c	  CALL XVMESSAGE('   0   10   20   30   40   50   60   70   80   90
c     +   100  110  120  130  140  150  160  170  180  190  200  210  220
c     +  230  240  250  ',' ')
      END DO
      RETURN
      END
C==========================================================================
      subroutine histgmb(tally,hisvec,nohist,ttl,nexcl,exclud)
c      IMPLICIT INTEGER (A-Z)
	implicit none
      include 'fortport'

	integer*4 nohist,nexcl,dsiz
	integer*4 i,ik,inc,iz,j,jcnt,jf,jfeat,jh,jj,jj1,jk
	integer*4 jptcnt,jy,k,max,move,numic,pagsiz
	integer*4 xhgh,xlow,xsiz,xsiz2,yscale,ysiz
      character*132 string
      character*132 buffer
      character*8 ttl
      integer*4 tally(256,12),indx
      character*244 hisbuf
      integer*4 hisvec(12),exclud(10)
      real*4 xscale,xshft
      integer*2 xaxis(14)
      character*4 char
      character*1 sym
      character*1 tempbuf
      byte sbyte

        integer*4 ioline
        integer*2    sbuf(32000)
      common /IOSTFF/ ioline,sbuf

      data move/'00000041'X/			!'A'
C
C
C        INITIALIZE
c	do j=1,nohist
c	do i=1,256
c	  print *,"tally(i,1) = ",i,tally(i,j)
c	enddo
c	enddo

cc	stop
      PAGSIZ= 70
      YSIZ= 15
      INC= 2
      XSIZ= 121
      XLOW= 0
      XHGH= XLOW+INC*(XSIZ-1)
      XSIZ2= INC*(XSIZ-1)+1
C
      JPTCNT= PAGSIZ/(YSIZ+9)
      DSIZ= (XSIZ+8)/10+1
      XSCALE= FLOAT(XLOW-XHGH)/(XSIZ-1)
      XSHFT= FLOAT(XSIZ*XHGH-XLOW)/(XSIZ-1)
      DO 90 I=1,DSIZ
         K= DSIZ-I+1
         XAXIS(K)= (10*I-9)*XSCALE+XSHFT+0.501
90	continue
C
C
      JCNT= JPTCNT
      DO 900 JF=1,NOHIST
         JFEAT= HISVEC(JF)
         IF (JCNT .LT. JPTCNT)  GO TO 300
         call xvmessage(' ',' ')
         WRITE(BUFFER,1002) TTL
1002  FORMAT('HISTOGRAM FOR:  "',A,'"')
         call xvmessage(buffer,' ')
         JCNT= 0
C
C        SCALE & PRINT THE HISTOGRAM
300      MAX= 0
         JJ1= XHGH+3
         DO 350 J=(JJ1+1),256
            TALLY(JJ1,JFEAT)= TALLY(JJ1,JFEAT)+TALLY(J,JFEAT)
c	   print *,"TALLY(JJ1,JFEAT) = ",jj1,jfeat,TALLY(JJ1,JFEAT)
            TALLY(J,JFEAT)= 0
350	continue

         IF (NEXCL .EQ. 0)  GO TO 375
c	print *,"here--"
         DO 360 J=1,NEXCL
            JJ= EXCLUD(J)+1
            TALLY(JJ,JFEAT)= 0
360	continue
375      YSCALE= 1
         JCNT= JCNT+1
         DO 400 K=1,(XSIZ2+INC),INC
            J= XLOW+K
c	    print *, "j,TALLY(J,JFEAT) = ",j,TALLY(J,JFEAT)
            JK= TALLY(J,JFEAT)+TALLY(J+1,JFEAT)
c		print *,"k,j, jk,max = ",k,j, jk,max
            IF (JK .GT. MAX)  MAX= JK
400	continue
c	print *,"max = ",max
         IF (MAX .GT. YSIZ)  YSCALE= (MAX+(YSIZ-1))/YSIZ
c	print *,"yscale = ",yscale
         WRITE (BUFFER,1004)  JFEAT
1004  FORMAT('CHANNEL ',I3)
         call xvmessage(buffer,' ')
         WRITE(BUFFER,6004)  YSCALE
6004  FORMAT('   EACH * REPRESENTS',I4,'  POINT(S).')
         call xvmessage(buffer,' ')
C   print out histogram
c	print *,"ysiz,inc = ",ysiz,inc
         DO 600 JY=1,YSIZ
            JH= (YSIZ-(JY-1))*YSCALE
            IK= JH-YSCALE
            DO 500 I=1,(XSIZ2+INC),INC
cc	print *,"i = ",i
               HISBUF(I:i)= ' '				!default
               IZ= XLOW+I
               JK= TALLY(IZ,JFEAT)+TALLY(IZ+1,JFEAT)
               SYM= '*'
               IF (JK .GE. JH)  GO TO 490
               IF (JK .LE. IK)  GO TO 500
               JK= JK-IK
               NUMIC= JK
C      CALL BINBCD(NUMIC,CHAR)
               write(char(4:4),'(i1)') numic
      
               sym(1:1)=char(4:4)
               IF (JK .LT. 10)  GO TO 490
               JK= JK-10
               jk = jk+move			!Add jk to 'A'
               sbyte = int2byte(jk)
               jk = jk - move
               write(sym,'(a1)') sbyte
               if (JK .LT. 100) then
                   write(char(1:2),'(I2)')JK
               else 
                   write(char(1:3),'(I3)')JK
               end if 
         
               IF (JK .LT. 26)  GO TO 490
               SYM(1:1)= '$'
490   continue
               HISBUF(I:i)= SYM(1:1)
               write(tempbuf,'(a1)') sym
500         CONTINUE
            indx = 1
            do 505 i = 1,122
               string(i:i) = hisbuf(indx:indx)
               indx = indx + 2
505         continue
C
            write(buffer,8700) jh,string(1:122)
8700  format(i4,' I',1x,a122)
            call xvmessage(buffer,' ')

c      WRITE(BUFFER,5002)  JH,(HISBUF(I),I=1,(XSIZ2+INC),INC)
c5002  FORMAT(1X,I4,' I',1X,124A1)
c      CALL XVMESSAGE(buffer,' ')
600      CONTINUE
c  write out bottom axis
         WRITE(BUFFER,6001)
6001  FORMAT(7X,12('+---------'),'+=>')
         call xvmessage(buffer,' ')
         WRITE(BUFFER,6002)  (XAXIS(I),I=1,DSIZ)
6002  FORMAT(5X,12(I3,7X),I3)
         call xvmessage(buffer,' ')
900   CONTINUE
      RETURN
      END
C==========================================================================
      subroutine histgm(tally,hisvec,nohist,ttl,nexcl,exclud)
c      IMPLICIT INTEGER (A-Z)
        implicit none
      include 'fortport'

        integer*4 nohist,nexcl,dsiz
        integer*4 i,ik,inc,iz,j,jcnt,jf,jfeat,jh,jj,jj1,jk
        integer*4 jptcnt,jy,k,max,move,numic,pagsiz
        integer*4 xhgh,xlow,xsiz,xsiz2,yscale,ysiz
      character*132 string
      character*132 buffer
      character*8 ttl
      integer*4 tally(32767,12),indx
      character*244 hisbuf
      integer*4 hisvec(12),exclud(10)
      real*4 xscale,xshft
      integer*2 xaxis(14)
      character*4 char
      character*1 sym
      character*1 tempbuf
      byte sbyte

        integer*4 ioline
        integer*2    sbuf(32000)
      common /IOSTFF/ ioline,sbuf

      data move/'00000041'X/                    !'A'

      PAGSIZ= 70
      YSIZ= 15
      INC= 2
      XSIZ= 121
      XLOW= 0
      XHGH= XLOW+INC*(XSIZ-1)
      XSIZ2= INC*(XSIZ-1)+1
C
      JPTCNT= PAGSIZ/(YSIZ+9)
      DSIZ= (XSIZ+8)/10+1
      XSCALE= FLOAT(XLOW-XHGH)/(XSIZ-1)
      XSHFT= FLOAT(XSIZ*XHGH-XLOW)/(XSIZ-1)
      DO 90 I=1,DSIZ
         K= DSIZ-I+1
         XAXIS(K)= (10*I-9)*XSCALE+XSHFT+0.501
90      continue
C
C
      JCNT= JPTCNT
      DO 900 JF=1,NOHIST
         JFEAT= HISVEC(JF)
         IF (JCNT .LT. JPTCNT)  GO TO 300
         call xvmessage(' ',' ')
         WRITE(BUFFER,1002) TTL
1002  FORMAT('HISTOGRAM FOR:  "',A,'"')
         call xvmessage(buffer,' ')
         JCNT= 0
C
C        SCALE & PRINT THE HISTOGRAM
300      MAX= 0
         JJ1= XHGH+3
         DO 350 J=(JJ1+1),32767
            TALLY(JJ1,JFEAT)= TALLY(JJ1,JFEAT)+TALLY(J,JFEAT)
c          print *,"TALLY(JJ1,JFEAT) = ",jj1,jfeat,TALLY(JJ1,JFEAT)
            TALLY(J,JFEAT)= 0
350     continue

         IF (NEXCL .EQ. 0)  GO TO 375
c       print *,"here--"
         DO 360 J=1,NEXCL
            JJ= EXCLUD(J)+1
            TALLY(JJ,JFEAT)= 0
360     continue
375      YSCALE= 1
         JCNT= JCNT+1
         DO 400 K=1,(XSIZ2+INC),INC
            J= XLOW+K
c           print *, "j,TALLY(J,JFEAT) = ",j,TALLY(J,JFEAT)
            JK= TALLY(J,JFEAT)+TALLY(J+1,JFEAT)
c               print *,"k,j, jk,max = ",k,j, jk,max
            IF (JK .GT. MAX)  MAX= JK
400     continue
c       print *,"max = ",max
         IF (MAX .GT. YSIZ)  YSCALE= (MAX+(YSIZ-1))/YSIZ
c       print *,"yscale = ",yscale
         WRITE (BUFFER,1004)  JFEAT
1004  FORMAT('CHANNEL ',I3)
         call xvmessage(buffer,' ')
         WRITE(BUFFER,6004)  YSCALE
6004  FORMAT('   EACH * REPRESENTS',I4,'  POINT(S).')
         call xvmessage(buffer,' ')
C   print out histogram
c       print *,"ysiz,inc = ",ysiz,inc
         DO 600 JY=1,YSIZ
            JH= (YSIZ-(JY-1))*YSCALE
            IK= JH-YSCALE
            DO 500 I=1,(XSIZ2+INC),INC
cc      print *,"i = ",i
               HISBUF(I:i)= ' '                         !default
               IZ= XLOW+I
               JK= TALLY(IZ,JFEAT)+TALLY(IZ+1,JFEAT)
               SYM= '*'
               IF (JK .GE. JH)  GO TO 490
               IF (JK .LE. IK)  GO TO 500
               JK= JK-IK
               NUMIC= JK
C      CALL BINBCD(NUMIC,CHAR)
               write(char(4:4),'(i1)') numic

               sym(1:1)=char(4:4)
               IF (JK .LT. 10)  GO TO 490
               JK= JK-10
               jk = jk+move                     !Add jk to 'A'
               sbyte = int2byte(jk)
               jk = jk - move
               write(sym,'(a1)') sbyte
               if (JK .LT. 100) then
                   write(char(1:2),'(I2)')JK
               else
                   write(char(1:3),'(I3)')JK
               end if

               IF (JK .LT. 26)  GO TO 490
               SYM(1:1)= '$'
490   continue
               HISBUF(I:i)= SYM(1:1)
               write(tempbuf,'(a1)') sym
500         CONTINUE
            indx = 1
            do 505 i = 1,122
               string(i:i) = hisbuf(indx:indx)
               indx = indx + 2
505         continue
C
            write(buffer,8700) jh,string(1:122)
8700  format(i4,' I',1x,a122)
            call xvmessage(buffer,' ')

c      WRITE(BUFFER,5002)  JH,(HISBUF(I),I=1,(XSIZ2+INC),INC)
c5002  FORMAT(1X,I4,' I',1X,124A1)
c      CALL XVMESSAGE(buffer,' ')
600      CONTINUE
c  write out bottom axis
         WRITE(BUFFER,6001)
6001  FORMAT(7X,12('+---------'),'+=>')
         call xvmessage(buffer,' ')
         WRITE(BUFFER,6002)  (XAXIS(I),I=1,DSIZ)
6002  FORMAT(5X,12(I3,7X),I3)
         call xvmessage(buffer,' ')
900   CONTINUE
      RETURN
      END

C********************************************************
      subroutine hstgnb(nsamp,pixlin,hist)
c
c	BYTE value histogram - 255 bins
c
	implicit none
      INCLUDE 'fortport'
C
      byte pixlin(*)
      integer*4 hist(256),nsamp,i,index

      DO I=1,NSAMP
        INDEX=1 + BYTE2INT(PIXLIN(I))
        HIST(INDEX)=HIST(INDEX)+1
      ENDDO
      RETURN
      END
C********************************************************
      subroutine hstgen(nsamp,pixlin,hist)
c
c	INTEGER*2 value histogram - 32767 (only pos) bins
c
        implicit none
      integer*2 pixlin(*)
      integer*4 hist(32767),nsamp,i,index

      DO I=1,NSAMP
        INDEX=1 + PIXLIN(I)
        HIST(INDEX)=HIST(INDEX)+1
      ENDDO
      RETURN
      END

