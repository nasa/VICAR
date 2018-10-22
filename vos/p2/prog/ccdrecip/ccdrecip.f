      INCLUDE 'VICMAIN_FOR'
C VICAR PROGRAM CCDRECIP
C
C Radiometric calibration routine:  Determines the camera sensitivity
C (in DN per foot-lambert-milliseconds or in DN per picoamp-milliseconds
C and shutter offset (in msec) for a CCD or vidicon
C camera system.
C   UNITS
c	15      LTFILE
c	IUNI	INP
c	66	UNCORR	Sensitivity values
C	67      CORR  Sensitivity values
C	ISO 	Shutter Offsets`
C	OUNI	OUT
C	11	Output table
c	12      Offsets
C	13 	AVG
C	18      gnuplot .asc "b" file
C	19      gnuplot .asc file

      SUBROUTINE MAIN44  

	implicit none
         COMMON/CP/VMES,ARMES,RMSG,LABEL

         REAL*8 DC(400),DN(30,400),AO(400),TOS(400)
         REAL*8 W(30),AVDN(30),DENOM,DNSUM
         REAL*8 SWX(400),SWXT(400),SX2(400)
         REAL*8 SW,SWT,SWT2
         REAL*4 LIGHT(30),LYAV(40),LXAV(40),YTOS(402),XTOS(402)
         REAL*4 SHUTTER_OFFSETS(1024), LORS_NUMBERS(1024)
         REAL*4 AVTOS(40),COLU(30,40),COLV(30,40),LT,LTC
         REAL*4 OUT(4320), OUTM(2,2000), EXPOS(30), BEXPO
         REAL*4 AVGOS,EXPO,U,V,X

         INTEGER*4 ATBL,OTBL,CTBL,VTBL,ASIZ
         INTEGER*4 SL,SS,SLI,SSI,NLI,NSI,NLI2,NSI2  ! size variables
         INTEGER*4 OUNI,STAT,CNT,GAREA,SIGTOL,BAD(400)
         INTEGER*4 AREA(4,400), NI
c         INTEGER*4 STATUS
         INTEGER*4 NPTS,NEXP,I,IB,ICNT,IDEF,IND,IPLOT,IREJCT,ISO,IUNI,IX
         INTEGER*4 J,JST,K,L,LCNT,N,NAREA,NCOL,NGOOD,NGR,NL,NLAB,NLO
         INTEGER*4 NLUM,NROW,NS,NSO,NUM,ninpfileb,ntblb
         INTEGER*4 nplotgpi,nplotgpi2,nploteps,ntbl,ninpfile,nplotname

         LOGICAL*4 epsplot

         CHARACTER*63 plotname
         character*80 plotgpi,plotgpi2,ploteps,tbl,tblb
         CHARACTER*256 PLOT_DS,OFFSET_FILE,inpfile,inpfileb,cbuf
         CHARACTER*40 CORFIL(2),ATFIL,OFFIL,AVFIL,LFILE
         CHARACTER*4 DIRECTN
         CHARACTER*8 UNITS,pformat
         CHARACTER*70 MS1, MS2
         CHARACTER*39 LUMMSG
         CHARACTER*34 RADMSG
         CHARACTER*1 TAB

C        global variables
         CHARACTER*54 VMES
         CHARACTER*4320 LABEL
         CHARACTER*51 ARMES
         CHARACTER*41 RMSG  

	character*4 gpi/'.gpi'/,eps/'.eps'/,asc/'.asc'/
	character*1 bee/'b'/

         TAB = CHAR(9)

        epsplot = .false.
        nplotgpi = 0
        nplotgpi2 = 0
        nplotname = 0
        nploteps = 0
        ntbl = 0
        ninpfile = 0
      
         VMES(1:54)=' '
         ARMES = 'NUMBER OF GOOD AREAS=**** OUT OF**** AREAS SAMPLED'
         RMSG = 'NUMBER REJECTED FOR               =     '

         MS1 = ' '
         MS2 = ' '
         MS2(1:26)='AREA   SL   SS   NL   NS  '
         MS2(27:47)='A0 (DN/ENERGY UNIT)  '
         MS2(48:65)='TOS (MILLISECONDS)'
         LUMMSG(1:39)='ENERGY UNIT = FOOT-LAMBERT-MILLISECONDS'
         RADMSG(1:34)='ENERGY UNIT = PICOAMP-MILLISECONDS'

         CALL IFMESSAGE ('CCDRECIP VERSION 06-Jul-2013')

        call xvparm('INP',cbuf,cnt,idef,1)
          inpfile = cbuf
          ninpfile=index(inpfile,'   ') - 1
	  inpfileb = inpfile(1:ninpfile)//bee
	  ninpfileb=index(inpfileb,'   ') - 1

         CALL XVP('SIGTOL',SIGTOL,CNT)
         CALL XVPARM('PLOT',PLOT_DS,IPLOT,IDEF,1)
         IF (IDEF.EQ.0) THEN
               plotname = PLOT_DS
               nplotname=index(plotname,'   ') - 1
               plotgpi=plotname(1:nplotname)//gpi
               nplotgpi=index(plotgpi,'  ') - 1
               plotgpi2=plotname(1:nplotname)//eps//gpi
               nplotgpi2=index(plotgpi2,'  ') - 1
               ploteps=plotname(1:nplotname)//eps
               nploteps=index(ploteps,'  ') - 1
               tbl = plotname(1:nplotname)//asc
               ntbl = index(tbl,'  ') - 1
		tblb = plotname(1:nplotname)//'.'//bee//asc
	       ntblb = index(tblb,'  ') - 1
ccc            CALL PLOTFN(PLOT_DS)
ccc            CALL XRTBEGIN(STATUS)
ccc            IF (STATUS.NE.1) CALL MABEND('Unable to OPEN plotter')
ccc            CALL DISPLAYAXES(1,1,0)! x,y1,y2 axes displayed 1=yes 0=no
ccc            CALL SETACTIVESET(1)   ! endpts on lines 0=no triangles, 1=triangles
            IPLOT = 1
         ELSE
            IPLOT = 0
         ENDIF

         call xvp ('PLOTFMT',pformat,cnt)
         if (pformat .eq. 'eps' .or. pformat .eq. 'EPS') epsplot = .true.

         CALL XVP('REJECT',IREJCT,CNT)

C-------get light data from file or parameters
	 CALL XVPARM('LTFILE',LFILE,ICNT,IDEF,1)
	 IF (ICNT .EQ. 1) THEN
	    OPEN(15,FILE=LFILE,STATUS='OLD',IOSTAT=JST,ERR=996)
	    DO J=1,30
	       READ(15,7,END=6) LIGHT(J)
	       NLUM=J
	    END DO
            CLOSE(15)
    6       CALL PRNT(4,1,NLUM,' LIGHT FROM FILE=.')
	 ELSE
	    CALL XVP('LIGHT',LIGHT,LCNT)
            NLUM = LCNT
	 END IF
	
    7    FORMAT(G20.12)

C-------any light entered?
	 IF (LCNT .EQ. 0 .AND. NLUM .EQ. 0) THEN
	    CALL XVMESSAGE('??E - NO LIGHT DATA INPUT',' ')
	    GO TO 999
	 END IF
	
	 CALL XVPARM('DIRECTIO',DIRECTN,ICNT,IDEF,1) 
         CALL XVPARM('ARRAYSIZ',ASIZ,ICNT,IDEF,1)
	 CALL XVPARM('AREATBL', ATFIL,ATBL,IDEF,1)
	 CALL XVPARM('OFFTBL',  OFFIL,OTBL,IDEF,1)
	 CALL XVPARM('AVOFFTBL',AVFIL,VTBL,IDEF,1)
	 CALL XVPARM('CORRTBL',CORFIL,CTBL,IDEF,2)
	 CALL XVPARM('UNITS',UNITS,ICNT,IDEF,1)

         CALL XVUNIT(IUNI,'INP',1,STAT,' ')
         CALL XVOPEN(IUNI,STAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
         CALL XVSIZE(SLI,SSI,NLI,NSI,NLI2,NSI2)
         CALL LABPROC(IUNI,NLAB,LABEL)

C        Read in area size fields...
         CALL XLGET (IUNI,'HISTORY','NUM_AREAS',NAREA,STAT,
     &               'FORMAT','INT','HIST','LTGEN',' ')
         CALL XLGET (IUNI,'HISTORY','AREAS',AREA,STAT,'NELEMENT',
     &               4*NAREA,'FORMAT','INT','HIST','LTGEN',' ')

C        Rectangular array of areas is expected, so determine number of columns
	NCOL = 0
         DO 111 I=2,NAREA
            if (area(2,i) .lt. area(2,i-1) ) then
               ncol=i-1
               go to 112
            end if
  111    CONTINUE

  112    CONTINUE   ! exit the loop

         NROW = NAREA/NCOL

         IF (NCOL*NROW.NE.NAREA) GOTO 998
         SW=0.0D0
         SWT=0.0D0
         SWT2=0.0D0

         DO I=1,NAREA
            SWX(I)=0.0D0
            SWXT(I)=0.0D0
            SX2(I)=0.0D0
         ENDDO

         CALL XLGET (IUNI,'HISTORY','NUM_EXPOS',NEXP,STAT,
     &               'FORMAT','INT','HIST','LTGEN',' ')
         CALL XLGET (IUNI,'HISTORY','EXPOSURES',EXPOS,STAT,
     &               'NELEMENT',NEXP,'FORMAT','REAL', 
     &               'HIST','LTGEN',' ')


C        Read data and compute weighted sums...
         DO 50 L = 1,NLI                     !Loop through each exposure...
            CALL XVREAD (IUNI,OUT,STAT,'LINE',L,'NSAMPS',NSI,' ')
            NI = NINT (OUT(1))
            IF (NI .EQ. 0) THEN
               BEXPO = EXPOS(L)
               GOTO 970
            ENDIF
            IF (L .GT. 1) THEN
               W(L-1) = DBLE(LIGHT(L)**2)          ! weights
               SW = SW + W(L-1)                         ! sum weights
               SWT = SWT + W(L-1)*DBLE(EXPOS(L))        ! sum weights * time
               SWT2 = SWT2 + W(L-1)*DBLE(EXPOS(L)**2)
            ENDIF
            IB = 0

            DO 50 K = 1,NAREA                    !Area loop
               SL = AREA(1,K)
               SS = AREA(2,K)
               NL = AREA(3,K)
               NS = AREA(4,K)
               N = NI*NL*NS                   !Total # pixels area*NI
               DNSUM = 0.0D0
               DO I = 1,NI
                  DNSUM = DNSUM + DBLE(OUT(IB+I+1))    !Sum dn across inputs
               ENDDO


               IF (L .EQ. 1) THEN
                  DC(K) = DNSUM/N             !Average Dark Current
               ELSE
                  DN(L-1,K) = DNSUM/N - DC(K)   !Average DN
                  X = DN(L-1,K)/LIGHT(L)
                  SWX(K) = SWX(K) + W(L-1)*DBLE(X)
                  SWXT(K) = SWXT(K) + W(L-1)*DBLE(X)*DBLE(EXPOS(L))
                  SX2(K) = SX2(K) + DBLE(X)**2
               ENDIF
   50    IB = IB + 3*NI

         CALL XVMESSAGE (' ',' ')
         DENOM = SWT**2 - SW*SWT2

C        Compute shutter offset and sensitivity for each area....
         IF (UNITS .EQ. 'RADIANCE') THEN
            CALL XVMESSAGE (RADMSG,' ')
         ELSE
            CALL XVMESSAGE (LUMMSG,' ')
         ENDIF
         CALL XVMESSAGE (' ',' ')
         CALL XVMESSAGE (MS2,' ')
         DO K=1,NAREA
            SL = AREA(1,K)
            SS = AREA(2,K)
            NL = AREA(3,K)
            NS = AREA(4,K)

C           calculate camera sensitivity  
            AO(K) = (SWX(K)*SWT-SWXT(K)*SW)/DENOM          

C           calculate shutter-offset 
            TOS(K) = (AO(K)*SWT-SWX(K))/(AO(K)*SW)

C           output AREA results 
            WRITE (MS1,
     &         '(I4,A1,I4,A1,I4,A1,I4,A1,I4,A5,F12.5,A9,F12.5)')
     &         K,' ',SL,' ',SS,' ',NL,' ',NS,'     ',AO(K),
     &         '         ',TOS(K)
            CALL XVMESSAGE (MS1,' ')
         ENDDO

C        Weed out bad areas...
         CALL ZIA(BAD,NAREA)
         IF (NAREA.GT.1) THEN
            CALL XVMESSAGE (' ',' ')
            CALL XVMESSAGE ('Global value for A0...',' ')
            CALL IMEAN(AO,1,NAREA,BAD,SIGTOL)
            CALL XVMESSAGE (' ',' ')
            CALL XVMESSAGE ('Global shutter offset...',' ')
            CALL IMEAN(TOS,2,NAREA,BAD,SIGTOL)
         ENDIF
         CALL XVMESSAGE (' ',' ')

C        Print out all bad areas...
         DO 100 K=1,NAREA
            IF (BAD(K).EQ.0) GOTO 100
            SL = AREA(1,K)
            SS = AREA(2,K)
            NL = AREA(3,K)
            NS = AREA(4,K)
            WRITE (MS1,'(A5,I3,A16,I4,A1,I4,A1,I4,A1,I4,A1)') 'AREA ',
     &             K,' (SL,SS,NL,NS)=(',SL,',',SS,',',NL,',',NS,')'
            IF (BAD(K).EQ.1) MS1(47:70)='***BAD SENSITIVITY******'
            IF (BAD(K).EQ.2) MS1(47:70)='***BAD SHUTTER OFFSET***'
            IF (BAD(K).EQ.4) MS1(47:70)='***BOTH BAD FIT*********'
            CALL XVMESSAGE (MS1,' ')
  100    CONTINUE

C        Get average DN at each exposure level (using only the
C        good areas)....
C         NPTS = NLI - 2		!Number of exposures (excluding DC)
         NPTS = NLI - 1                !Number of exposures (excluding DC)
         GAREA = 0			!Number of good areas
         CALL ZIA(AVDN,2*NPTS)	!Zero out averages
C
         DO 105 K=1,NAREA
            IF ((IREJCT .EQ. 1) .AND. 
     &          ((BAD(K) .EQ. 1) .OR. (BAD(K) .EQ. 4))) GOTO 105
            IF ((IREJCT .EQ. 2) .AND. 
     &          ((BAD(K) .EQ. 2) .OR. (BAD(K) .EQ. 4))) GOTO 105
            IF ((IREJCT .EQ. 3) .AND. (BAD(K) .NE. 0)) GOTO 105
            GAREA = GAREA + 1
            DO J=1,NPTS
               AVDN(J) = AVDN(J) + DN(J,K)
            ENDDO
  105    CONTINUE

         DO J=1,NPTS
            AVDN(J) = AVDN(J)/GAREA
         ENDDO

         CALL XVMESSAGE (' ',' ')
         IF (IREJCT.EQ.1) RMSG(21:34)='SENSITIVITY   '
         IF (IREJCT.EQ.2) RMSG(21:34)='SHUTTER OFFSET'
         IF (IREJCT.EQ.3) RMSG(21:34)='BOTH          '
         WRITE (RMSG(38:40),'(I3)') NAREA-GAREA
   	 IF (IREJCT.EQ.0) THEN
            RMSG(1:41)='NO REJECTION CRITERIA APPLIED            '
	 ENDIF
         CALL RTIMES(NPTS,LIGHT,EXPOS,AVDN,IPLOT,NLAB,
     1        NAREA,GAREA,UNITS,ATBL,ATFIL,plotgpi,nplotgpi,plotgpi2,
     2        nplotgpi2,epsplot,ploteps,nploteps,tbl,ntbl,tblb,ntblb,*999)
c`
cc         CALL RTIMES(NPTS,LIGHT(2),EXPOS(2),AVDN,IPLOT,NLAB,
cc     &               NAREA,GAREA,UNITS,ATBL,ATFIL,*999)
C-------------------------------------------------------
c	print *,'DIRECTN = ',directn
         IF (DIRECTN .EQ. 'LINE') THEN
C-----------    LINE DIRECTION    ---------
C              Get average offset for each row...
            NGR = 0			!Number of good rows

            DO 120 I=1,NROW
               AVGOS = 0.0		!Average offset for row I
               NGOOD = 0		!Number of good areas on row

               DO 110 J=1,NCOL
                  K = NCOL*(I-1) + J
                  IF ((IREJCT .EQ. 1) .AND. 
     &                ((BAD(K) .EQ. 1) .OR. (BAD(K) .EQ. 4))) GOTO 110
                  IF ((IREJCT .EQ. 2) .AND. 
     &                ((BAD(K) .EQ. 2) .OR. (BAD(K) .EQ. 4))) GOTO 110
                  IF ((IREJCT .EQ. 3) .AND. (BAD(K) .NE. 0)) GOTO 110
                  SL = AREA(1,K)
                  SS = AREA(2,K)
                  NL = AREA(3,K)
                  NS = AREA(4,K)
                  XTOS(K) = SL + NL/2
                  YTOS(K) = TOS(K)
                  AVGOS = AVGOS + TOS(K)
                  NGOOD = NGOOD + 1
  110          CONTINUE

               IF (NGOOD.GT.0) THEN
                  NGR = NGR + 1
                  LXAV(NGR) = SL + NL/2
                  LYAV(NGR) = AVGOS/NGOOD
               ENDIF
  120       CONTINUE
	 ELSE
C-----------    SAMPLE DIRECTION    ---------
C           Get average offset for each column...
            NGR = 0                     !Number of good cols

            DO 140 I=1,NCOL
               AVGOS = 0.0              !Average offset for col I
               NGOOD = 0                !Number of good areas on col

               DO 130 J=1,NROW
                  K = NCOL*(J-1) + I
                  IF ((IREJCT .EQ. 1) .AND. 
     &                ((BAD(K) .EQ. 1) .OR. (BAD(K) .EQ. 4))) GOTO 130
                  IF ((IREJCT .EQ. 2) .AND. 
     &                ((BAD(K) .EQ. 2) .OR. (BAD(K) .EQ. 4))) GOTO 130
                  IF ((IREJCT .EQ. 3) .AND. (BAD(K) .NE. 0)) GOTO 130
                  SL = AREA(1,K)
                  SS = AREA(2,K)
                  NL = AREA(3,K)
                  NS = AREA(4,K)
                  XTOS(K) = SS + NS/2
                  YTOS(K) = TOS(K)
                  AVGOS = AVGOS + TOS(K)
                  NGOOD = NGOOD + 1
  130          CONTINUE
               IF (NGOOD.GT.0) THEN
                  NGR = NGR + 1
                  LXAV(NGR) = SS + NS/2
                  LYAV(NGR) = AVGOS/NGOOD
                  AVTOS(I) = LYAV(NGR)      !save by column number
               ENDIF
  140       CONTINUE
	 ENDIF

         IF (CTBL .EQ. 2) THEN
C-------open table of uncorrected sensitivity values by grid column
            open(66,file=corfil(1),status='UNKNOWN',IOSTAT=JST,ERR=999)
C-------open table of corrected sensitivity values by grid column
            open(67,file=corfil(2),status='UNKNOWN',IOSTAT=JST,ERR=999)
            IF (DIRECTN .EQ. 'SAMP') THEN

C-----------  averaging grid columns    ---------
               num = ncol
               do ix=2,nlum
                  lt=light(ix)*expos(ix)
                  do i=1,ncol
                     ltc=light(ix)*(expos(ix) - avtos(i))
                     do j=1,nrow
       	                k=ncol*(j-1)+i
        	        u=dn(ix-1,k)/lt
       	                v=dn(ix-1,k)/ltc
       	                colu(ix-1,i) = colu(ix-1,i) + u
       	                colv(ix-1,i) = colv(ix-1,i) + v
                     enddo
                     colu(ix-1,i) = colu(ix-1,i)/nrow
                     colv(ix-1,i) = colv(ix-1,i)/nrow
                  enddo
                  write(66,661) expos(ix),(TAB,colu(ix-1,i),i=1,ncol)
                  write(67,661) expos(ix),(TAB,colv(ix-1,i),i=1,ncol)
               enddo
            ELSE
C------- averaging grid rows
	       num = nrow
               do ix=2,nlum
                  lt=light(ix)*expos(ix)
                  do i=1,nrow
                     ltc=light(ix)*(expos(ix) - avtos(i))
                     do j=1,ncol
                        k=nrow*(j-1)+i
                        u=dn(ix-1,k)/lt
                        v=dn(ix-1,k)/ltc
                        colu(ix-1,i) = colu(ix-1,i) + u
                        colv(ix-1,i) = colv(ix-1,i) + v
                     enddo
                     colu(ix-1,i) = colu(ix-1,i)/ncol
                     colv(ix-1,i) = colv(ix-1,i)/ncol
                  enddo
                  write(66,661) expos(ix),(TAB,colu(ix-1,i),i=1,nrow)
                  write(67,661) expos(ix),(TAB,colv(ix-1,i),i=1,nrow)
               enddo
            ENDIF 

  661       format(1x,F5.0,40(A1,F11.6)) ! the 40 is # of cols in COLU and COLV

            close(66)
            close(67)
         ENDIF
C-------------------------------------------------------
C-----Note:  Now, the xtos and ytos are 0.0 for bad areas
C-------------------------------------------------------
         IF (IPLOT.EQ.1) THEN
            CALL TPLOT(NLAB,YTOS,XTOS,GAREA,LYAV,LXAV,NGR,directn,
     1        asiz,plotgpi,nplotgpi,plotgpi2,
     2        nplotgpi2,epsplot,ploteps,nploteps,tbl,ntbl,tblb,ntblb)

ccc            CALL PLOT(0,0,999)
            CALL XVCLOSE(IUNI,STAT,' ')
         ENDIF

C-------Write out tables of 1) line/samp number vs. offset (all good areas)
C-------                    2) avg line/samp num. vs. avg. offset 
         if (otbl .eq. 1)
     &      call tbloff(xtos,ytos,narea,offil,directn,*999)
         if (vtbl .eq. 1)
     &      call tblav(lxav,lyav,ngr,avfil,directn,*999)
C
C	Calculate shutter offsets for each line or samp...
C
         CALL XVPARM('OFFSETS',OFFSET_FILE,CNT,IDEF,1)
         IF (IDEF.EQ.0) THEN
	    CALL XVMESSAGE ('Saving shutter offsets...',' ')
	    CALL LINSHUT( NGR, LXAV, LYAV, 1.0, 1.0, ASIZ,
     &                    LorS_Numbers, Shutter_Offsets )
            CALL XVUNIT(ISO,'X',1,IND,'U_NAME',OFFSET_FILE,' ')
            IF (IND.NE.1) GOTO 997
            CALL XVOPEN(ISO,IND,'OP','WRITE','U_NL',1,'U_NS',ASIZ,
     &                  'U_FORMAT','REAL','O_FORMAT','REAL',
     &                  'OPEN_ACT','SA','IO_ACT','SA',' ')
            CALL XLADD(ISO,'HISTORY','FILE','SHUTTER-OFFSET',
     &                 STAT,'FORMAT','STRING',' ')
            CALL XLADD(ISO,'HISTORY','SO_TYPE',DIRECTN//'-DEPENDENT',
     &                 STAT,'FORMAT','STRING',' ')
            CALL XVWRIT(ISO,Shutter_Offsets,IND,' ')
            CALL XVCLOSE(ISO,IND,' ')
         ENDIF

C        Output centers of all bad areas in MARK format file...
         CALL XVUNIT(OUNI,'OUT',1,STAT,' ')
         IF (STAT.NE.1) GOTO 250            !Skip if output file not specified

C        Put the centers into output buffer (OUTM)...
         I = 0
         DO K = 1,NAREA
            IF ((IREJCT .NE. 0) .AND. (BAD(K) .NE. 0) .AND.
     &          ((IREJCT .EQ. BAD(K)) .OR. (IREJCT .EQ. 3) .OR. 
     &           (BAD(K) .EQ. 4))) THEN
               SL = AREA(1,K)
               SS = AREA(2,K)
               NL = AREA(3,K)
               NS = AREA(4,K)
               I = I + 1
               OUTM(1,I)=SL+NL/2
               OUTM(2,I)=SS+NS/2
            ENDIF
         ENDDO

         NSO = 2*(NAREA-GAREA)
         NLO = 1
         CALL XVOPEN(OUNI,STAT,'OPEN_ACT','SA','IO_ACT','SA',
     &               'OP','WRITE','U_NL',NLO,'U_NS',NSO,'U_FORMAT',
     &               'REAL','O_FORMAT','REAL',' ')
         IF(IREJCT .EQ. 1)
     &      CALL XLADD(OUNI,'HISTORY','CCDRECIP',
     &                 ' REJECTED FOR SENSITIVITY',STAT,'FORMAT',
     &                 'STRING',' ')
         IF(IREJCT .EQ. 2)
     &      CALL XLADD(OUNI,'HISTORY','CCDRECIP',
     &                 ' REJECTED FOR SHUTTER OFFSET',STAT,'FORMAT',
     &                 'STRING',' ')
         IF(IREJCT .EQ. 3)
     &      CALL XLADD(OUNI,'HISTORY','CCDRECIP',
     &                 ' REJECTED FOR BOTH',STAT,'FORMAT','STRING',' ')
         CALL XVWRIT(OUNI,OUTM,STAT,'NSAMPS',NSO,' ')
         CALL XVCLOSE(OUNI,STAT,' ')

  250    CALL XVMESSAGE ('CCDRECIP task completed',' ')
         RETURN

C	 Error conditions...
  970    CALL PRNT(7,1,EXPO,'??E - No data for exposure=.')
         CALL XVMESSAGE (
     &      '***Run MOMGEN on this exposure and try again.',' ')
         GOTO 999
  996    CALL XVMESSAGE ('??E - Error opening light file',' ')
         CALL PRNT(4,1,JST,' IOSTAT=.')
         GOTO 999
  997    CALL XVMESSAGE ('??E - Error allocating offset file',' ')
         GOTO 999
  998    CALL XVMESSAGE ('??E - Error in grid',' ')
         CALL PRNT(4,1,NAREA,' ***Narea=.')
  999    CALL XVMESSAGE ('***CCDRECIP task cancelled',' ')
         CALL ABEND
      END

C************************************************************************
C Routine to flag all areas which differ from mean by more than SIGTOL
C sigma...
C Inputs: BUF(N) = input samples
C         TYPE = 1 for sensitivity, =2 for offset
C         N = number of samples
C         SIGTOL = 1 or 2 (sigma)
C Outputs: BAD(N) = TYPE if sample is bad type
C                 = 4 if both sensitivity and offset terms are bad
      SUBROUTINE IMEAN(BUF,TYPE,N,BAD,SIGTOL)
 
         implicit none
         INTEGER*4 N,I,K,NLOOP,NS
         REAL*4 MEAN,EPS,SAMP,SIGMA
         REAL*8 BUF(N),SUM,SSUM
         INTEGER*4 BAD(N),TYPE,SIGTOL
         CHARACTER*44 MSG
     &      /' N=**** MEAN=******.***** SIGMA=******.*****'/

         NLOOP = 4 - SIGTOL

         DO 50 I=1,NLOOP
            NS = 0
            SUM = 0.0D0
            SSUM = 0.0D0

            DO K=1,N
               IF ((BAD(K) .NE. TYPE) .AND. (BAD(K) .NE. 4)) THEN 
                  SAMP = BUF(K)
                  NS = NS+1
                  SUM = SUM+SAMP
                  SSUM = SSUM+SAMP*SAMP
               ENDIF
            ENDDO

            MEAN = SUM/NS
            SIGMA = SSUM/NS-MEAN*MEAN
            SIGMA = SQRT(SIGMA)
            WRITE (MSG,'(A2,I4,A6,F12.5,A7,F12.5)') 'N=',NS,
     &             ' MEAN=',MEAN,' SIGMA=',SIGMA
            IF (I .EQ. 1) 
     &         CALL XVMESSAGE ('Raw mean and sigma are...',' ')
            IF (I .EQ. 2) 
     &         CALL XVMESSAGE (
     &            'After throwing out samples differing by 2 sigma',' ')
            IF (I .EQ. 3) 
     &         CALL XVMESSAGE (
     &            'After throwing out samples differing by 1 sigma',' ')
            CALL XVMESSAGE (MSG,' ')
            IF (I .EQ. NLOOP) GOTO 50

C-------To avoid rejecting all points when perfect data is input
            if (sigma .eq. 0.0) sigma=1.0e-06

            EPS=(3-I)*SIGMA

            DO 20 K=1,N		!Weed out bad samples
               IF ((BAD(K) .EQ. TYPE) .OR. (BAD(K) .EQ. 4)) GOTO 20
               SAMP = BUF(K)
               SAMP = ABS(SAMP-MEAN)
               IF (SAMP .GE. EPS) THEN
                  IF (BAD(K) .NE. 0) THEN
                     BAD(K) = 4
                  ELSE
                     BAD(K) = TYPE
                  ENDIF
               ENDIF
   20       CONTINUE

   50    CONTINUE

         RETURN
      END         


C**************************************************************************************
      SUBROUTINE RTIMES(NPTS,L,T,D,IPLOT,NLAB,NAREA,GAREA,
     1           UNITS,atbl,atfil,plotgpi,nplotgpi,plotgpi2,
     2 nplotgpi2,epsplot,ploteps,nploteps,tbl,ntbl,tblb,ntblb,*)

	implicit none
         REAL*4 LT,TMT0,L(30),T(30)
         REAL*8 D(30)
         REAL*8 X(30),X2(30),X3(30),DL1(30),DL2(30),DL3(30)
         REAL*8 TOS,AO,STOS,SAO,DY,DZ,XYZ
         REAL*8 SW,SWT,SWX,SWXT,SWT2,W,RX,SX,SX2,SDY

         INTEGER*4 NAREA,GAREA,NPTS,IPLOT,NLAB,I,JST
         integer*4 atbl,nplotgpi,nplotgpi2,nploteps,ntbl,ntblb
	 integer*4 natfil,nyt1,nyt2,nyt3,nyt4
	 logical*4 epsplot

         COMMON/CP/VMES,ARMES,RMSG,LABEL
         CHARACTER*4320 LABEL,RMSG*41,VMES*54,ARMES*51

        character*80 plotgpi,plotgpi2,ploteps,tbl,tblb

         CHARACTER*30 YT1, YT2, YT3, YT4
         CHARACTER*132 MSG1
         CHARACTER*9 HD1, HD4
         CHARACTER*12 HD2
         CHARACTER*7 HD3
         CHARACTER*13 HD5
         CHARACTER*19 HD6
         CHARACTER*8 HD7
         CHARACTER*14 HD8
         CHARACTER*6 HD9L, HD9R, HD12B
         CHARACTER*10 HD12A
         CHARACTER*4 HD13, HD16
         CHARACTER*3 HD14, HD15
         CHARACTER*40 atfil
         CHARACTER*8 UNITS
         CHARACTER*1 TAB
c
c	YT is ytitle
C
         YT1 = '(DN-DC)/L'
	 nyt1 = index(yt1,'  ') - 1
         YT2 = '(DN-DC)/(L*T)'
	 nyt2 = index(yt2,'  ') - 1
         YT3 = '(DN-DC)/(L*(T-TOS))'
	 nyt3 = index(yt3,'  ') - 1
         YT4 = ' RESIDUAL FIT'
	 nyt4 = index(yt4,'  ') - 1

         HD1 = 'COMMANDED'
         HD2 = 'ILLUMINATION'
         HD3 = '(DN-DC)'
         HD4 = '(DN-DC)/L'
         HD5 = '(DN-DC)/(L*T)'
         HD6 = '(DN-DC)/[L*(T-TOS)]'
         HD7 = 'RESIDUAL'
         HD8 = 'EXPOSURE T(MS)'
         HD9L = 'L(LUM)'
         HD9R = 'L(RAD)'
         HD12A = '(DN/ENERGY'
         HD12B = ' UNIT)'
         HD13 = 'TOS='
         HD14 = 'SD='
         HD15 = 'AO='
         HD16 = 'RMS='

         TAB = CHAR(9)
         CALL XVMESSAGE (' ',' ')
         MSG1 = ' '
         MSG1(4:12)=HD1
         MSG1(17:28)=HD2
         MSG1(33:39)=HD3
         MSG1(46:54)=HD4
         MSG1(60:67)=HD7
         MSG1(74:86)=HD5
         MSG1(90:97)=HD7
         MSG1(100:118)=HD6
         MSG1(121:129)=HD7
         CALL XVMESSAGE (MSG1(2:132),' ')		!print 1st line header

         MSG1 = ' '
         MSG1(2:15)=HD8
         IF (UNITS .EQ. 'RADIANCE') THEN
           MSG1(19:24)=HD9R
         ELSE
           MSG1(19:24)=HD9L
         ENDIF
         MSG1(45:54)=HD12A
         MSG1(59:68)=HD12A
         MSG1(75:84)=HD12A
         MSG1(89:98)=HD12A
         MSG1(104:113)=HD12A
         MSG1(120:129)=HD12A
         CALL XVMESSAGE (MSG1(2:129),' ')		!print 2nd line header

         MSG1 = ' '
         MSG1(47:52)=HD12B
         MSG1(61:66)=HD12B
         MSG1(77:82)=HD12B
         MSG1(91:96)=HD12B
         MSG1(106:111)=HD12B
         MSG1(122:127)=HD12B
         CALL XVMESSAGE (MSG1(2:127),' ')		!print 3rd line header

c	print *,'here...'
         SX = 0.0D0
         SX2 = 0.0D0
         SW = 0.0D0
         SWX = 0.0D0
         SWT = 0.0D0
         SWXT = 0.0D0
         SWT2 = 0.0D0

         DO I=1,NPTS
             W = L(I+1)**2
             X(I) = D(I)/L(I+1)   ! calculate (DN-DC)/L
             SX = SX + X(I)
             SX2 = SX2 + X(I)**2
             SW = SW + W
             SWX = SWX + W*X(I)
             SWT = SWT + W*T(I+1)
             SWXT = SWXT + W*DBLE(T(I+1))*DBLE(X(I))
             SWT2 = SWT2 + W*DBLE(T(I+1))**2
         ENDDO

C        calculate camera sensitivity
         AO = (SWX*SWT-SWXT*SW)/(SWT**2-SW*SWT2)

C        calculate shutter-offset
         TOS = (AO*SWT-SWX)/(AO*SW)
         MSG1 = ' '
         RX = 0.0D0
         DZ = 0.0D0
         SDY = 0.0D0
	 natfil=index(atfil,'  ') - 1
C------OPEN OUTPUT TABLE IF NEEDED
	 if (atbl .eq. 1) THEN
            open(11,file=atfil(1:natfil),status='UNKNOWN',IOSTAT=JST,ERR=999)
 	    IF (UNITS .EQ. 'RADIANCE') THEN
               WRITE(11,10) 'MEAN_DN.aka.D',TAB,'RAD.aka.L',TAB,
     &               'EXP.aka.T',TAB,'L.times.T',TAB,'T.minus.T0.aka.Q',
     &               TAB,'D.over.L',TAB,'D.over.L.times.T',TAB,
     &               'D.over.L.times.Q'
            ELSE
               WRITE(11,10) 'MEAN_DN.aka.D',TAB,'LUM.aka.L',TAB,
     &               'EXP.aka.T',TAB,'L.times.T',TAB,'T.minus.T0.aka.Q',
     &               TAB,'D.over.L',TAB,'D.over.L.times.T',TAB,
     &               'D.over.L.times.Q'
            ENDIF
         ENDIF

   10    FORMAT(1x,A13,A1,A9,A1,A9,A1,A9,A1,A16,A1,A8,A1,A16,A1,A16)

	if (iplot.eq.1) then
	   OPEN(19,FILE=tbl(1:ntbl),STATUS='UNKNOWN',IOSTAT=JST,ERR=997)
	endif

C	 Get residuals
         DO I=1,NPTS

C           calculate (DN-DC)/(L*T)
            X2(I) = D(I)/(DBLE(L(I+1))*DBLE(T(I+1)))
C           calculate residual
            DL2(I) = X2(I) - AO

C           calculate (DN-DC)/[L*(T-TO)]
            X3(I) = D(I)/(DBLE(L(I+1))*(DBLE(T(I+1))-TOS))
C           calculate residual
            DL3(I) = X3(I) - AO

            RX = RX + DL2(I)**2
            DZ = DZ + DL3(I)**2
            DL1(I) = X(I) - AO*(DBLE(T(I+1))-TOS)
            SDY = SDY + DL1(I)**2

C           output results in table format
            WRITE (MSG1(3:10),'(F8.4)') T(I+1)
            WRITE (MSG1(17:24),'(F8.4)') L(I+1)
            WRITE (MSG1(31:38),'(F8.3)') D(I)
            WRITE (MSG1(45:52),'(F8.4)') X(I)
            WRITE (MSG1(59:66),'(F8.4)') DL1(I)
            WRITE (MSG1(75:82),'(F8.4)') X2(I)
            WRITE (MSG1(89:96),'(F8.4)') DL2(I)
            WRITE (MSG1(104:111),'(F8.4)') X3(I)
            WRITE (MSG1(120:127),'(F8.4)') DL3(I)
            CALL XVMESSAGE (MSG1,' ')
	if (iplot.eq.1) then
	    write (19,10100) MSG1
10100 format (A) 
	
	endif
C           write to table file if AREATBL is specified 
            IF (ATBL .EQ. 1) THEN
	       LT = L(I+1)*T(I+1)
	       TMT0 = T(I+1)-TOS
               WRITE(11,9) D(I),TAB,L(I+1),TAB,T(I+1),TAB,LT,TAB,TMT0,
     &               TAB,X(I),TAB,X2(I),TAB,X3(I)
            ENDIF
C
         ENDDO	!do i=1,npts
	  if (IPLOT .eq.1 ) close(19)
	 IF (ATBL .EQ. 1) CLOSE(11)

    9    FORMAT(1x,2(F12.4,A1),F10.1,A1,3(F16.4,A1),2(F16.6,A1))

         MSG1 = ' '

C        calculate standard deviations of the residuals
         RX = DSQRT(RX)
         DZ = DSQRT(DZ)
         DY = DSQRT(SDY/(NPTS-2.0D0))

         XYZ = NPTS*SX2 - SX**2
         SAO = DY*DSQRT(NPTS/XYZ)
         STOS = DY*DSQRT(SX2/XYZ)

         MSG1(55:58)=HD16
         MSG1(85:88)=HD16
         MSG1(116:119)=HD16
         WRITE (MSG1(60:66),'(F7.4)') DY
         WRITE (MSG1(90:96),'(F7.4)') RX
         WRITE (MSG1(121:127),'(F7.4)') DZ
         CALL XVMESSAGE (MSG1,' ')

         CALL XVMESSAGE (' ',' ')
         MSG1 = ' '
         MSG1(1:4)=HD13
         MSG1(15:17)=HD14
         MSG1(29:31)=HD15
         MSG1(43:45)=HD14
         WRITE (MSG1(5:11),'(F7.4)') TOS
         WRITE (MSG1(18:24),'(F7.4)') STOS
         WRITE (MSG1(32:38),'(F7.4)') AO
         WRITE (MSG1(46:52),'(F7.4)') SAO
         VMES=MSG1
         CALL XVMESSAGE (VMES,' ')
         CALL XVMESSAGE (RMSG,' ')
         WRITE (ARMES(22:25),'(I4)') GAREA
         WRITE (ARMES(33:36),'(I4)') NAREA
         CALL XVMESSAGE (ARMES,' ')

         IF (IPLOT.EQ.0) RETURN

C        PLOT 1 - plot (DN-DC)/L .vs. SHUTTER TIME
         CALL RPLOT(NLAB,YT1,nyt1,X,T,0,NPTS,AO,1,plotgpi,nplotgpi,
     1 plotgpi2,nplotgpi2,epsplot,ploteps,nploteps,tbl,ntbl,tblb,ntblb)
C        PLOT 2 - plot residual .vs. SHUTTER TIME
         CALL RPLOT(NLAB,YT4,nyt4,DL1,T,3,NPTS,AO,2,plotgpi,nplotgpi,
     1 plotgpi2,nplotgpi2,epsplot,ploteps,nploteps,tbl,ntbl,tblb,ntblb)

C        PLOT 3 - plot (DN-DC)/(L*T) .vs. SHUTTER TIME
         CALL RPLOT(NLAB,YT2,nyt2,X2,T,1,NPTS,AO,3,plotgpi,nplotgpi,
     1 plotgpi2,nplotgpi2,epsplot,ploteps,nploteps,tbl,ntbl,tblb,ntblb)
C        PLOT 4 - plot residual .vs. SHUTTER TIME
         CALL RPLOT(NLAB,YT4,nyt4,DL2,T,2,NPTS,AO,4,plotgpi,nplotgpi,
     1 plotgpi2,nplotgpi2,epsplot,ploteps,nploteps,tbl,ntbl,tblb,ntblb)
C        PLOT 5 - plot (DN-DC)/[L*(T-TO)] .vs. SHUTTER TIME
         CALL RPLOT(NLAB,YT3,nyt3,X3,T,1,NPTS,AO,5,plotgpi,nplotgpi,
     1 plotgpi2,nplotgpi2,epsplot,ploteps,nploteps,tbl,ntbl,tblb,ntblb)
C        PLOT 6 - plot residual .vs. SHUTTER TIME
         CALL RPLOT(NLAB,YT4,nyt4,DL3,T,2,NPTS,AO,6,plotgpi,nplotgpi,
     1 plotgpi2,nplotgpi2,epsplot,ploteps,nploteps,tbl,ntbl,tblb,ntblb)

         RETURN
997      CALL xvmessage('??E - ERROR OPENING PLOTTING DATA FILE',' ')
	 CALL PRNT(4,1,JST,' IOSTAT =.')
	RETURN 1
999	 CALL xvmessage('??E - ERROR OPENING AREA TABLE FILE',' ')
	 CALL PRNT(4,1,JST,' IOSTAT =.')
	 RETURN 1
      END

C***************************************************************************
C     This subroutine generates a plot of the inputs DY and DX
      SUBROUTINE RPLOT(NLAB,YTITLE,NYTITLE,DY,DX,IFLAG,NPTS,DAO,
     1        typlot,plotgpi,nplotgpi,plotgpi2,
     2        nplotgpi2,epsplot,ploteps,nploteps,tbl,ntbl,tblb,ntblb)

	implicit none
         COMMON/CP/VMES,ARMES,RMSG,LABEL
         COMMON/PLT1/LXT,LYT,LT
         COMMON/PLT2/XLEN,YLEN,XPAGE,YPAGE,XORIG,YORIG,XDELTA,YDELTA

ccc         INTEGER*4 STATUS
         INTEGER*4 ICNT,NLAB,IFLAG,NPTS,I,LT,LXT,LYT,N,N2,jj,typlot
	 INTEGER*4 nltitle,nxtitle,ntitle(80),plotwid,plotht,isize
         INTEGER*4 nytitle,nplotgpi,nplotgpi2,nploteps,ntbl,ntblb
         INTEGER*4 ncolx,ncoly(6)
         REAL*4 labstep,tmp,xmin,xmax,ymin,ymax,ydiff
         REAL*4 Y(30),X(30),DX(30),AO,ABUF(30),ZBUF(30),XC,YC
         REAL*8 DY(30),DAO
	REAL*4 XDELTA,YDELTA,XLEN,YLEN,XPAGE,YPAGE,XORIG,YORIG

	logical*4 epsplot,first/.true./
        save first
        character*80 plotgpi,plotgpi2,ploteps,tbl,tblb
         CHARACTER*80 ltitle
         CHARACTER*12 MSG(6)	
         CHARACTER*30 YTITLE 
         CHARACTER*4320 LABEL
         CHARACTER*41 RMSG
         CHARACTER*54 VMES
         CHARACTER*51 ARMES
         CHARACTER*86 TITLE(80)
         CHARACTER*30 XTITLE

         LTITLE = 'CCD RECIPROCITY ANALYSIS'
	nltitle = index(ltitle,'    ') - 1
         XTITLE = 'SHUTTER TIME(MS)'
	nxtitle = index(xtitle,'    ') - 1
         LXT = 17
         LYT = 19
         LT = 25
         YC = 12.50           ! set up for HEADER
         XC = 1.50
         YC = YC - 0.25       ! set up for LABEL
         N = MIN0(NLAB,25)

         ICNT = 1

C        insert TASK labels into TITLE
         DO I = 1,N
            TITLE(ICNT)=LABEL(1+(I-1)*72:1+(I-1)*72+71)
	    ntitle(icnt) = index(title(icnt),'    ') - 1
            ICNT = ICNT + 1
         ENDDO

C        insert more labels into TITLE
         TITLE(ICNT)=VMES
	 ntitle(icnt) = 52		!index(title(icnt),'    ') - 1
         ICNT = ICNT + 1
         TITLE(ICNT)=RMSG
         ntitle(icnt) = 40    		!index(title(icnt),'    ') - 1
         ICNT = ICNT + 1
         TITLE(ICNT)=ARMES
	ntitle(icnt) = 50		!index(title(icnt),'    ') - 1

C        display plot header and axis-titles
ccc         CALL HEADER (TITLE,ICNT,0) ! 0=left justify, 1=center justify, 2=right
ccc         CALL AXESTITLES(XTITLE,YTITLE,270,' ',0)
C--  GET THE SIZE OF THE PLOT.
c       default
        plotwid =  648  !640 @72dpi = 8.888.. inches    9 inch = 648
        plotht  =  648  !480 @72dpi = 6.666.. inches    7 inch = 504
        labstep = 0.02          !for size 9 font --- originally 0.04

        if (icnt .gt. 16) then
           tmp = icnt/16
           plotht = int(plotht * tmp)
           labstep = labstep/tmp
        endif

        isize = 10

c  added check on 'first' because of anomalous behaviour of "UNKNOWN" status on Solaris
c  -lwk-  24sep2013
        if (first) then
          first = .false.
          open(98,file=plotgpi(1:nplotgpi),status='UNKNOWN',iostat=jj,err=995)
          if (epsplot) then
             open(97,file=plotgpi2(1:nplotgpi2),status='UNKNOWN',iostat=jj,err=996)
          endif
        endif

            call write_gpi_1(98,tbl,ntbl,tblb,ntblb,ytitle,nytitle,
     1 xtitle,nxtitle,isize,plotwid,plotht,
     2 ploteps,nploteps)
            if (epsplot) then
                call write_gpi_1(97,tbl,ntbl,tblb,ntblb,ytitle,nytitle,
     1 xtitle,nxtitle,isize,plotwid,plotht,
     2 ploteps,nploteps)
            endif

         AO = SNGL(DAO)
         N2 = NPTS + 2

	XMAX = -1.0e20
	XMIN = 1.0
	YMAX = -1.0e20
	YMIN = 1.0e20
         DO I = 1,NPTS
            Y(I) = DY(I)
            if (y(i) .lt. 1.0e-6) y(i) = 0.0
	    if (y(i) .gt. ymax) ymax=y(i)
	    if (y(i) .lt. ymin) ymin=y(i)
            X(I) = DX(I+1)
	    if (x(i) .gt. xmax) xmax=x(i)
         ENDDO
c	print *, 'ymin,ymax = ',ymin,ymax
c        if (ymax .eq. -0.0) ymax = 0.
c        if (ymin .eq. -0.0) ymin = 0.

        if (typlot .eq. 3) then
            ymax = ymax + 0.01*ymax
            ymin = ymin - 0.01*ymin     
        endif
        if (typlot .eq. 4) then
            ymax = ymax - 0.1*ymax
            ymin = ymin + 0.1*ymin
        endif
	
	if (ymin .eq. ymax) then
	    if (ymax .eq. 0.0) then
		ymax = 1.0
                ymin = -1.0
	    else 
	        ymin = ymin - 0.1*ymin
	        ymax = ymax + 0.1*ymax
            endif
        endif
	if (typlot .eq. 1) then
		ymax = 40
		ymin = 0
	endif
c	print *, 'ymin,ynmax = ',ymin,ymax
         DO I = 1,NPTS
            ZBUF(I) = 0.0000
            ABUF(I) = AO
         ENDDO

	ydiff = ymax -  ymin
	if (ydiff .le. 0.001) then
		ymin = ymin - 1.0
		ymax = ymax + 1.0
	endif
	
         XLEN = 8.00
         YLEN = 10.00
         XORIG = 0.0000
         XDELTA = 15.0000
         X(NPTS+1) = XORIG
         X(NPTS+2) = XDELTA

c	print *,'IFLAG = ',IFLAG
         IF (IFLAG .EQ. 3) GOTO 30
         IF (IFLAG .EQ. 2) GOTO 20
         IF (IFLAG .EQ. 1) GOTO 10
c iflag=0    Y='(DN-DC)/L' plot (DN-DC)/L .vs. SHUTTER TIME
         YORIG = 0.0000
         YDELTA = 2.0000
         Y(NPTS+1) = YORIG
         Y(NPTS+2) = YDELTA
         GOTO 40
c iflag=1    Y='(DN-DC)/(L*T)'  plot (DN-DC)/(L*T) .vs. SHUTTER TIME 
c                           or  plot (DN-DC)/[L*(T-TO)] .vs. SHUTTER TIME
   10    YORIG = 0.120
         YDELTA = 0.0100
         Y(NPTS+1) = YORIG
         Y(NPTS+2) = YDELTA
         ABUF(NPTS+1) = YORIG
         ABUF(NPTS+2) = YDELTA
         GOTO 40
c iflag=2    Y=' RESIDUAL FIT'    plot residual .vs. SHUTTER TIME
   20    YORIG =  -0.100
         YDELTA = 0.020
         Y(NPTS+1) = YORIG
         Y(NPTS+2) = YDELTA
         ZBUF(NPTS+1) = YORIG
         ZBUF(NPTS+2) = YDELTA
         GOTO 40
c  iflag=3    Y=' RESIDUAL FIT'    plot residual .vs. SHUTTER TIME
   30    YORIG =  -1.000
         YDELTA = 0.200
         Y(NPTS+1) = YORIG
         Y(NPTS+2) = YDELTA
         ZBUF(NPTS+1) = YORIG
         ZBUF(NPTS+2) = YDELTA

   40    CONTINUE

         X(NPTS+1) = 0.0
         X(NPTS+2) = 1.0
         Y(NPTS+1) = 0.0
         Y(NPTS+2) = 1.0
         ABUF(NPTS+1) = 0.0
         ABUF(NPTS+2) = 1.0
         ZBUF(NPTS+1) = 0.0
         ZBUF(NPTS+2) = 1.0

cc	do i=1,n2		!npts + 2
cc	  print *,'i,x,y,abuf,zbuf = ',i,x(i),y(i),abuf(i),zbuf(i)
cc        enddo

	call write_gpi_2(98,ltitle,nltitle,xmin,xmax,
     1 ymin,ymax,isize)
	   if (epsplot) then
           call write_gpi_2(97,ltitle,nltitle,xmin,xmax,
     1 ymin,ymax,isize)

	   endif

	call write_gpi_lab(98,icnt,title,ntitle,isize,labstep)
        if (epsplot) then
           call write_gpi_lab(97,icnt,title,ntitle,isize,labstep)
        endif

	ncolx = 1
	do i=1,6
	   ncoly(i) = i + 3
        enddo
	msg(1)='PLOT 1'
	msg(2)='PLOT 2'
	msg(3)='PLOT 3'
	msg(4)='PLOT 4'
	msg(5)='PLOT 5'
	msg(6)='PLOT 6'
        call write_ls_gpi_pl(98,tbl,ntbl,ncolx,ncoly,typlot,msg)
        if (epsplot) then
           call write_ls_gpi_pl(97,tbl,ntbl,ncolx,ncoly,typlot,msg)
        endif

ccc         CALL LINE(X,Y,NPTS,1,1,1)
ccc         IF (IFLAG .EQ. 1) CALL LINE(X,ABUF,NPTS,1,1,11)
ccc         IF (IFLAG .EQ. 3) CALL LINE(X,ZBUF,NPTS,1,0,0)
cccc        IF (IFLAG .EQ. 2) CALL LINE(X,ZBUF,NPTS,1,0,0)
ccc         CALL XRTPAGE (STATUS)
cc         IF (STATUS .NE. 1) GOTO 960
         RETURN

ccc  960    CALL XVMESSAGE ('*** Incomplete Ploting Operation.',' ')
ccc         CALL XVMESSAGE ('XRT Window Button Definitions:',' ')
ccc         CALL XVMESSAGE ('SAVE = write to output PostScript file.',' ')
ccc         CALL XVMESSAGE ('PAGE = display next graph.',' ')
ccc         CALL XVMESSAGE ('EXIT = terminate application.',' ')
ccc         CALL MABEND('***CCDRECIP task cancelled',' ')
c   error returns
995     call xvmessage('??E - rplot: Error opening/writing gnuplot file',' ')
        call abend
        return
996     call xvmessage('??E - rplot: Error opening/writing gnuplot eps file',' ')
        call abend
        return

      END


C**********************************************************************
      SUBROUTINE TPLOT(NLAB,Y,X,NPTS,LYAV,LXAV,NRGD,DIR,
     1        asiz,plotgpi,nplotgpi,plotgpi2,
     2        nplotgpi2,epsplot,ploteps,nploteps,tbl,ntbl,tblb,ntblb)

	implicit none
         REAL*4 Y(402),X(402),LYAV(30),LXAV(30)
         REAL*4 XC,XLEN,YLEN,XORIG,YORIG,XPAGE,YPAGE,XDELTA,YDELTA
         REAL*4 YAV,YC,labstep,tmp,xmax,xmin,ymax,ymin

         INTEGER*4 ICNT,P,NLAB,NPTS,NRGD,I,K,LT,LXT,LYT,N,N2,nltitle
         INTEGER*4 nxtitle,nytitle,ntitle(80),asiz,ncolx,ncoly(6)
         INTEGER*4 nplotgpi,nplotgpi2,nploteps,ntbl,ntblb
	 INTEGER*4 plotwid,plotht,isize,jj,jst

         LOGICAL*4 epsplot

        character*80 plotgpi,plotgpi2,ploteps,tbl,tblb
         CHARACTER*12 MSG(6)
         CHARACTER*4320 LABEL
         CHARACTER*41 RMSG
         CHARACTER*54 VMES
         CHARACTER*51 ARMES
         CHARACTER*47 LMESG
	 CHARACTER*80 LTITLE
         CHARACTER*86 TITLE(80)
         CHARACTER*30 XTITLE
         CHARACTER*30 YTITLE
         CHARACTER*4 DIR

         COMMON/PLT1/LXT,LYT,LT
         COMMON/PLT2/XLEN,YLEN,XPAGE,YPAGE,XORIG,YORIG,XDELTA,YDELTA
         COMMON/CP/VMES,ARMES,RMSG,LABEL


         LMESG = 'LINE NUMBER=**** SHUTTER OFFSET=*********** MS'
         LTITLE = 'CCD RECIPROCITY ANALYSIS'
	 nltitle = index(ltitle,'   ')
         XTITLE = 'LINE NUMBER'
	 nxtitle = index(xtitle,'   ')
         YTITLE = 'GLOBAL SHUTTER OFFSET (MS)'
	 nytitle = index(ytitle,'   ') 

         IF (DIR(1:4) .EQ. 'SAMP') THEN
	      LMESG(1:4) = DIR(1:4)
	      XTITLE(1:4) = DIR(1:4)
         END IF

         ICNT = 1
         LXT = 11
         LYT = 26
         LT = 25
         YC = 12.50           !SET UP FOR HEADER
         XC = 1.50
C         CALL SYMBOL(XC,YC,0.21,TITLE,0,0.0,LT)
         YC = YC-0.25         !SET UP FOR LABEL
         N = MIN0(NLAB,25)

         DO P=1,N
            TITLE(ICNT)=LABEL(1+(P-1)*72:1+(P-1)*72+71)
	    ntitle(icnt) = index(title(icnt),'    ') - 1
            ICNT = ICNT + 1
         ENDDO

         TITLE(ICNT)=VMES
         ntitle(icnt) = 52		!index(title(icnt),'    ') - 1
         ICNT = ICNT + 1
         TITLE(ICNT)=RMSG
         ntitle(icnt) = 40		!index(title(icnt),'    ') - 1
         ICNT = ICNT + 1
         TITLE(ICNT)=ARMES
         ntitle(icnt) = 50		!index(title(icnt),'    ') - 1
CCC         CALL HEADER (TITLE,ICNT,0)
ccc         CALL AXESTITLES(XTITLE,YTITLE,270,' ',0)
C--  GET THE SIZE OF THE PLOT.
c       default
        plotwid =  648  !640 @72dpi = 8.888.. inches    9 inch = 648
        plotht  =  648  !480 @72dpi = 6.666.. inches    7 inch = 504
        labstep = 0.02          !for size 9 font --- originally 0.04

        if (icnt .gt. 16) then
           tmp = icnt/16
           plotht = int(plotht * tmp)
           labstep = labstep/tmp
        endif

        isize = 10

c  commented out because of anomalous behaviour of "UNKNOWN" status on Solaris
c  -lwk-  24sep2013
c       open(98,file=plotgpi(1:nplotgpi),status='UNKNOWN',iostat=jj,err=995)
c       if (epsplot) then
c          open(97,file=plotgpi2(1:nplotgpi2),status='UNKNOWN',iostat=jj,err=996)
c       endif

            call write_gpi_1(98,tbl,ntbl,tblb,ntblb,ytitle,nytitle,
     1 xtitle,nxtitle,isize,plotwid,plotht,
     2 ploteps,nploteps)
            if (epsplot) then
                call write_gpi_1(97,tbl,ntbl,tblb,ntblb,ytitle,nytitle,
     1 xtitle,nxtitle,isize,plotwid,plotht,
     2 ploteps,nploteps)
            endif

 
         N2 = NPTS + 2
         I = 1
         K = 1
         YAV = 0.0E0
         CALL XVMESSAGE (' ',' ')

        XMAX = real(asiz)
        XMIN = 0.0
c	print *,'xmax,xmin = ',xmax,xmin
        YMAX = -1e20
        YMIN = 1.0e20
c	print *,'NRGD = ',nrgd

c        if (iplot.eq.1) then
           OPEN(18,FILE=tblb(1:ntblb),STATUS='UNKNOWN',IOSTAT=JST,ERR=997)
c        endif

         DO N=1,NRGD
            WRITE (LMESG(13:16),'(I4)') NINT(LXAV(N))
            WRITE (LMESG(33:43),'(F11.6)') LYAV(N)
	    if (lyav(n) .gt. ymax) ymax=lyav(n)
	    if (lyav(n) .lt. ymin) ymin=lyav(n)
            CALL XVMESSAGE (LMESG,' ')
	    write (18,10100) NINT(LXAV(N)), LYAV(N)
10100 format (i4,1x,f11.6)

         ENDDO

c	if (iplot.eq.1) then
	    close(18)
c	endif
        if (ymin .eq. ymax) then
            if (ymax .eq. 0.0) then
                ymax = 1.0
            else
                ymin = ymin - 0.1*ymin
                ymax = ymax + 0.1*ymax
            endif
        endif
	
         X(NPTS+1) = 0.0
         X(NPTS+2) = 1.0
         Y(NPTS+1) = 0.0
         Y(NPTS+2) = 1.0

ccc         CALL LINE(X,Y,NPTS,1,-1,3)

         LXAV(NRGD+1) = 0.0
         LXAV(NRGD+2) = 1.0
         LYAV(NRGD+1) = 0.0
         LYAV(NRGD+2) = 1.0


        call write_gpi_2(98,ltitle,nltitle,xmin,xmax,
     1 ymin,ymax,isize)
           if (epsplot) then
           call write_gpi_2(97,ltitle,nltitle,xmin,xmax,
     1 ymin,ymax,isize)

           endif
ccc         CALL LINE(LXAV,LYAV,NRGD,1,0,0)
	call write_gpi_lab(98,icnt,title,ntitle,isize,labstep)
        if (epsplot) then
           call write_gpi_lab(97,icnt,title,ntitle,isize,labstep)
        endif

         DO I=1,6
            ncoly(i) = 2
            MSG(I)=' '
         ENDDO
        MSG(1)='PLOT 7'
	ncolx = 1
        call write_ls_gpi_pl(98,tblb,ntblb,ncolx,ncoly,1,msg)
	if (epsplot) then
	   call write_ls_gpi_pl(97,tblb,ntblb,ncolx,ncoly,1,msg)
        endif

         RETURN

c   error returns
995     call xvmessage('??E - tplot: Error opening/writing gnuplot file',' ')
        call abend
        return
996     call xvmessage('??E - tplot: Error opening/writing gnuplot eps file',' ')
        call abend
        return
997      CALL xvmessage('??E - ERROR OPENING PLOTTING B DATA FILE',' ')
         CALL PRNT(4,1,JST,' IOSTAT =.')
        RETURN 



      END


C************************************************************************
C     Performs a piece-wise linear interpolation of the calculated
C     shutter offsets to calculate all offsets.  The first two
C     and last two points are use to extrapolate for the first and
C     last few offsets.
      Subroutine LinShut( NIn, XIn, YIn, XStart, DX, NOut, XOut, YOut)
         Implicit None
         Integer*4  NIn, NOut
         Real*4 XIn(NIn), YIn(NIn), XStart, DX, XOut(NOut), YOut(NOut)
      
         Integer*4 I, IX1, IX2
         Real*4 X, M, B
      
         IX1 = 0
         IX2 = 1
         X = XStart
      
         DO I = 1, NOut
	    IF ( (IX1 .EQ. 0) .OR.		!) THEN
     &	         ( (X .GE. XIn(IX2)) .AND. (IX2 .LT. NIn) ) ) THEN
               IX1 = IX2
               IX2 = IX2 + 1
               M = (YIn(IX1) - YIn(IX2)) / (XIn(IX1) - XIn(IX2))
               B = YIn(IX1) - M*XIn(IX1)
            ENDIF
            XOut(I) = X
            YOut(I) = M*XOut(I) + B
            X = X + DX
         END DO
      
         RETURN
      END

C**************************************************************************
C     This subroutine retrives the VICAR 1 and history label.  It will 
C     display the TASK, USER, and TIME within each TASK entry.
      SUBROUTINE LABPROC(IUNI,NLAB,LABEL)
         IMPLICIT NONE 
         INTEGER*4 INSTANCES(20)
         INTEGER*4 NHIST, J, I             ! loop control variables
         INTEGER*4 NLAB, STAT              ! store # of labels and status
         INTEGER*4 IUNI                    ! store file id number

         CHARACTER*8 TASKS(20)           ! store task nume
         CHARACTER*4320 LABEL            ! store retrived VIC1 label
         CHARACTER*132 MSG               ! store message
         CHARACTER*12 UNAME              ! store user name
         CHARACTER*28 TIME               ! store time
         CHARACTER*65 HBUF

C        initialize display and storage buffers
         HBUF = '----TASK:------------USER:------------------------'
         MSG = ' '
         LABEL = ' '

         CALL VIC1LAB(IUNI,STAT,NLAB,LABEL,0) ! retrive VICAR 1 label
                                              ! NLAB=0 if no VIC1LAB
         NHIST = 20                           ! max # of tasks to retrive

C        retirve history tasks
         CALL XLHINFO(IUNI,TASKS,INSTANCES,NHIST,STAT,' ')

C        for each task, extract the USER name and the DATE_TIME, 
C        write the TASK name, USER name, and DATE_TIME onto buffer (HBUF)
         DO J=1,NHIST
            UNAME = ' '
            TIME = ' '
            CALL XLGET(IUNI,'HISTORY','USER',UNAME,STAT,'HIST',
     &                 TASKS(J),'INSTANCE',INSTANCES(J),'FORMAT',
     &                 'STRING',' ')  ! retrive user name from task entry
            CALL XLGET(IUNI,'HISTORY','DAT_TIM',TIME,STAT,'HIST',
     &                 TASKS(J),'INSTANCE',INSTANCES(J),'FORMAT',
     &                 'STRING',' ')  ! retrive DATE_TIME from task entry
            DO I=1,8                             !BLANKS NULLS
               IF(TASKS(J)(I:I) .LT. '0') TASKS(J)(I:I) = ' '
            ENDDO
            HBUF(10:17) = TASKS(J)
            HBUF(27:38) = UNAME
            HBUF(39:63) = TIME
            LABEL(1+(NLAB+J-1)*72:(1+(NLAB+J-1)*72)+64) = HBUF
         ENDDO

         NLAB=NLAB+NHIST   ! calculate number of labels to be display 

C        output history tasks retrived from the label
         DO I=1,NLAB
            MSG(1:72) = LABEL(1+(I-1)*72:(1+(I-1)*72)+71)
            CALL XVMESSAGE (MSG,' ')
         ENDDO

         RETURN
      END
C*********************************************************************
        subroutine write_gpi_1(unit,tbl,ntbl,tblb,ntblb,ylabel,nylabel,
     1 xlabel,nxlabel,isize,plotwid,plotht,
     2 eps,neps)
c            
c       write out the first part of gpi form
c
        implicit none
        integer*4 unit,isize,plotwid,plotht
        integer*4 jj,psize
        integer*4 ntbl,ntblb,nylabel,nxlabel,neps
        character*16 ylabel,xlabel
        character*80 tbl,tblb,eps
c
	psize = isize
	if (unit .eq. 97) psize = 16
10100 format('# Created by program ccdrecip')              !#'s are ignored in gnuplot
            write(unit,fmt=10100,iostat=jj,err=995)
10105 format('# Gnuplot commands for signal vs shutter time plot')
            write(unit,fmt=10105,iostat=jj,err=995)
10110 format('# Data in ',a,' and ',a)
            write(unit,fmt=10110,iostat=jj,err=995) tbl(1:ntbl),tblb(1:ntblb)

        if (unit.eq.97) then
10300 format('set terminal postscript eps enhanced color "Ariel" ',i2,'  size 11 ,8')
           write(unit,fmt=10300,iostat=jj,err=995) psize
10305 format("set output '",a,"'")
           write(unit,fmt=10305,iostat=jj,err=995) eps(1:neps)
        else
! size is X,Y
10115 format('set term x11 font "ariel,',i2,'" size ',i4,', ',i4)
            write(unit,fmt=10115,iostat=jj,err=995) isize,plotwid,plotht
10116 format('set output')                              !set output to screen
            write(unit,fmt=10116,iostat=jj,err=995)
        endif

10120 format('set grid ')
            write(unit,fmt=10120,iostat=jj,err=995)
10125 format("set ylab '",a,"'" )
            write(unit,fmt=10125,iostat=jj,err=995) ylabel(1:nylabel)
10130 format("set xlab '",a,"'")
            write(unit,fmt=10130,iostat=jj,err=995) xlabel(1:nxlabel)

c10141 format("set clip points")                         !how to deal with points out of range
c            write(unit,fmt=10142,iostat=jj,err=995)
c10142 format("set clip one")                            !how to deal with connecting lines out of range
c            write(unit,fmt=10141,iostat=jj,err=995)
        return
995   continue
        if (unit.eq.97) then
            call xvmessage('??E - write_gpi_1: Error opening/writing gnuplot eps file',' ')
            call abend
        else
            call xvmessage('??E - write_gpi_1: Error opening/writing gnuplot file',' ')
            call abend
        endif


        return
        end
C*********************************************************************
       subroutine write_gpi_2(unit,title,ntitle,xrang1,xrang2,
     1 yrang1,yrang2,isize)
c       ,mycol,nplots,iplot,fscale,nocol,nncol,
c     2 oldcol,newcol,nstart,npoint,ncontr,filename,nfilename)
c
c       write out the repeatable descriptive part of gpi form
c       for first, or only, plot
c
        implicit none
        integer*4 unit
        integer*4 jj,isize,psize
        integer*4 ntitle
        real*4 xrang1,xrang2,yrang1,yrang2
        character*80 title

        psize=isize
        if (unit .eq.97) psize = 16

10145 format('set title "',a,'" font "Ariel,',i2,'"')
             write(unit,fmt=10145,iostat=jj,err=995) title(1:ntitle),psize
C
C---- SET THE SCALING PARAMETERS.
C
C
c       set y range to be like vicar image - lines counting downward from top
c
10135 format("set yrange [",f10.4,":",f10.4,"]")
             write(unit,fmt=10135,iostat=jj,err=995) yrang1,yrang2
10140 format("set xrange [",f8.2,":",f8.2,"]")
             write(unit,fmt=10140,iostat=jj,err=995) xrang1,xrang2

        return
995   continue
        if (unit.eq.97) then
            call xvmessage('??E - write_gpi2_2: Error opening/writing gnuplot eps file',' ')
            call abend
        else
            call xvmessage('??E - write_gpi2_2: Error opening/writing gnuplot file',' ')
            call abend
        endif

        return
        end
c******************************************************************************
        subroutine write_gpi_lab(unit,nhdr,header,nheadr,isize,labstep)
c
c       write image labels to top of plot
c output labels for only top 60% of plot
c
c
        implicit none
        integer*4 unit,ii,jj,nhdr,isize,isize2,nheadr(80)
        real*4  fpos,labstep
        character*86 header(80)
c
	isize2 = isize - 1
	if (unit .eq. 97) isize2 =  16
       write (unit,fmt=10165,iostat=jj,err=995)
10165 format ('unset label')

        fpos=1.0
        do ii=1,nhdr
                fpos = fpos - labstep
c       print *,'ii = ',i,header(ii)(1:nheadr(ii)),fpos
10160 format('set label ',i2,' "',a,'" at graph .25 ,',f5.2,
     1 ' font "ariel,',i2,'" front nopoint tc def')
c       1 ' font "ariel 8" front nopoint tc def')
        write(unit,fmt=10160,iostat=jj,err=995) ii,header(ii)(1:nheadr(ii)), fpos,isize2
cc      print 10160, i,headr(ii)(1:nheadr(ii)), fpos
cc10155 format("set label 2 '",a,"' at graph 0.4, 0.90 front nopoint tc def")
cc        write(98,fmt=10155,iostat=jj,err=995) headr(3)
        enddo
c
        return
c
995   continue
        if (unit.eq.97) then
            call xvmessage('??E - write_gpi_lab: Error opening/writing gnuplot eps file',' ')
            call abend
        else
            call xvmessage('??E - write_gpi_lab: Error opening/writing gnuplot file',' ')
            call abend
        endif
        return
        end
C******************************************************************************
        subroutine write_ls_gpi_pl(unit,tbl,ntbl,ncolx,ncoly,iplot,msg)
c
c       write out data in gpi form for pts, lines or linepoints
c       for a 1 value in control column
c
        implicit none
        integer*4 unit,ntbl
        integer*4 gcol,jj,iplot
        integer*4 ncolx,ncoly(6)
	CHARACTER*12 MSG(6)
        character*80 tbl
        character*100 outline

c       bring in gunplot colors, lines, points, etc
        include 'gnuplotchar'

c	print *, iplot, ' ncolx = ',ncolx,' ncoly = ',ncoly(iplot)

        gcol = iplot
10253 format ("plot  '",a,"' u ",i2,":",i2," t '",a,"' w linespoints lt ",i2," pt ",i2,
     1 " ps 2 lw 2 lc rgb '",a,"'")
           write(unit,fmt=10253,iostat=jj,err=995) tbl(1:ntbl),ncolx,ncoly(gcol),msg(gcol),
     1 lntype(gcol),pttype(gcol),ptcolor(gcol)(1:ptcolorl(gcol))

        if (unit.eq.98) then
10255 format("pause mouse any")                 !allows plot to display on screen until mouse click
           write(unit,fmt=10255,iostat=jj,err=995)
        endif

        return
995     continue
        if (unit.eq.97) then
            call xvmessage('??E - write_ls_gpi_1: Error writing gnuplot eps file',' ')
        write (outline,19999) jj
19999 format ('iostat = ',i8)
            call xvmessage (outline,' ')
            call abend
        else
            call xvmessage('??E - write_ls_gpi_1: Error writing gnuplot file',' ')
        write (outline,19999) jj
            call xvmessage (outline,' ')
            call abend
        endif
        return
        end


C*********************************************************************
      subroutine tbloff(nums,offs,npts,table_ds,directn,*)
         
        implicit none
         INTEGER*4 NPTS,JST,L
         REAL*4 NUMS(NPTS),OFFS(NPTS)

         CHARACTER*40 TABLE_DS
         CHARACTER*4 DIRECTN
         CHARACTER*1 TAB

        TAB = CHAR(9)

C-------NUMS IS THE SET OF LINE OR SAMPLE NUMBERS FOR THE AREAS
C-------OFFS IS THE SET OF SHUTTER OFFSETS FOR THE AREAS
C-------both NUMS and OFFS are 0.0 for bad areas
         OPEN(12,FILE=TABLE_DS,STATUS='UNKNOWN',IOSTAT=JST,ERR=999)

         WRITE(12,10) 
     &      DIRECTN//'_OF_AREA',TAB,'CALCULATED.SHUTTER_OFFSET'

   10    FORMAT(1x,A12,A1,A25)

         DO L=1,NPTS
	   IF (NUMS(L) .NE. 0.0) 
     &        WRITE(12,9) NUMS(L),TAB,OFFS(L)
         END DO

         CLOSE(12)
         RETURN

    9    FORMAT(1x,F8.2,A1,F12.4)

  999    CALL xvmessage('??E - ERROR OPENING OFFSET TABLE FILE',' ')
         CALL PRNT(4,1,JST,' IOSTAT =.')
         RETURN 1
      END


C***********************************************************************
      subroutine tblav(nums,offs,npts,table_ds,directn,*)

	implicit none
	 INTEGER*4 NPTS,JST,L 
         REAL*4 NUMS(NPTS),OFFS(NPTS)

         CHARACTER*40 TABLE_DS
         CHARACTER*4 DIRECTN
         CHARACTER*1 TAB


        TAB = CHAR(9)
C-------NUMS IS THE SET OF AVG LINE OR SAMPLE NUMBERS FOR THE ROWS OR COLS
C-------OFFS IS THE SET OF AVG SHUTTER OFFSETS FOR THE ROWS OR COLS

         OPEN(13,FILE=TABLE_DS,STATUS='UNKNOWN',IOSTAT=JST,ERR=999)

         WRITE(13,10) DIRECTN,TAB,'MEAN.SHUTTER_OFFSET'
   10    FORMAT(1x,A4,A1,A19)

         DO L=1,NPTS
            WRITE(13,9) NUMS(L),TAB,OFFS(L)
         END DO

         CLOSE(13)
         RETURN

    9    FORMAT(1x,F8.2,A1,F12.4)

  999    CALL xvmessage('??E - ERROR OPENING AVERAGE TABLE FILE',' ')
	 CALL PRNT(4,1,JST,' IOSTAT =.')
	 RETURN 1
      END

