      INCLUDE 'VICMAIN_FOR'
C VICAR PROGRAM CCDNOISE
C
C Radiometric calibration routine:  Determines system gain constant (in
C electrons per DN) and the read-noise floor (DN) for a CCD camera system.
C
C         CCDNOISE LTF MARK
C
C If no extended dark currents are present, the records (L) in the LTF
C contain the following:
C   L=1    LTF header record.
C   L=2    Data record for exposure level 0 (dark current)
C   L=3    Data record for exposure level 1
C   L=NLI  Data record for hightest exposure level
C If extended dark currents are present, then:
C   L=1    LTF header record.
C   L=2    Data record for exposure level -1 (extended dark current)
C   L=3    Data record for exposure level 0 (normal dark current)
C   L=4    Data record for exposure level 1
C   L=NLI  Data record for hightest exposure level
C
c	iuni		INP
c	ouni		OUT
c	11		TABLE_DS
C	12		plot. asc
c	97		plot eps.gpi file
c	98		plot gpi file
c
      SUBROUTINE MAIN44
  	implicit none 
         REAL*8 RMEAN,SIGMA,SSUM(10),SSUM2(10),SSUMXY(10)
         REAL*8 EPDNMEAN,RDNMEAN,UI,UI2,SI2,UISI2
         REAL*4 EXP(2),reticle(2,5),OUT(6400),OUTM(2,2000)
         REAL*4 RDNBUF(400),EXPOS(30),EPDBUF(400)
         REAL*4 RSIGNAL(30,5),RNOISE(30,5),REPDN(5),RRDN(5)
         REAL*4 EDC(400),U(30,400),S(30,400),bexpo
         REAL*4 D,DMIN,EPDN,EXP1,EXP2,EXPO,RDN
         INTEGER*4 AREA(4,400)
         INTEGER*4 SL,SS,SLI,SSI,STAT,CNT,BAD(400),GAREA,EFNUM
         INTEGER*4 NG(5),TABLE,NI
	 INTEGER*4 dummy1, dummy2,I,IB,IDEF,IEDC,IMIN,IPLOT,IREJCT
         INTEGER*4 IUNI,OUNI,J,K,L,L1,L2,MAXL,MAXL2,MAXS,MAXS2,N
         INTEGER*4 NAREA,NEXP,NL,NS,NLAB,NLI,NLO,NPTS,NREJCT
         INTEGER*4 NSI,NSO,nplotgpi,nplotgpi2,nploteps,nplotname
         INTEGER*4 ninpfile,ntbl,icount
         LOGICAL*4 DBUG, XVPTST,epsplot, plotdata

        CHARACTER*8 pformat
	CHARACTER*63 plotname
	character*80 plotgpi,plotgpi2,ploteps,tbl
         CHARACTER*40 table_ds
         CHARACTER*41 RMSG
         CHARACTER*41 MS2
         CHARACTER*51 ARMES
         CHARACTER*85 MS1
         CHARACTER*132 MSG
         CHARACTER*100 PLOT_DS,cbuf,inpfile
         CHARACTER*4320 LABEL

        character*4 gpi/'.gpi'/,eps/'.eps'/,asc/'.asc'/

         ARMES = 'NUMBER OF GOOD AREAS=**** OUT OF**** AREAS SAMPLED'
         RMSG = 'NUMBER REJECTED FOR               ='
         MS2 = 'K=XXXXXX.XXXXX E/DN RDN=******.***** DN'
         MS1 = 'AREA XXX (SL,SS,NL,NS)=(XXXX,XXXX,XXXX,XXXX)'

         CALL XVMESSAGE('CCDNOISE version 13-Jul-2013',' ')

        epsplot = .false.
        nplotgpi = 0
        nplotgpi2 = 0
        nplotname = 0
        nploteps = 0
	ntbl = 0
	ninpfile = 0

C        IREJECT=0  Do not reject areas
C               =1  Reject areas with gain terms 2 sigma from average
C               =2  Reject areas with noise terms 2 sigma from average
C	        =3  Reject areas with both bad gain and noise terms
         DBUG = XVPTST('DBUG')
	call xvparm('INP',cbuf,icount,idef,1)
	  inpfile = cbuf
	  ninpfile=index(inpfile,'   ') - 1

	plotdata = .false.
         CALL XVPARM('PLOT',PLOT_DS,IPLOT,IDEF,1)
         IF (IPLOT .GT. 0) THEN
c            CALL PLOTFN(PLOT_DS)
	      plotdata= .true.
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
         ENDIF

         call xvp ('PLOTFMT',pformat,cnt)
	 if (pformat .eq. 'eps' .or. pformat .eq. 'EPS') epsplot = .true.


	 call xvparm('TABLE',TABLE_DS,TABLE,IDEF,1)

         CALL XVP('REJECT',IREJCT,CNT)
         CALL XVP('LIMIT',EXP,CNT)
         IF (CNT .EQ. 2) THEN
            EXP1 = EXP(1)
            EXP2 = EXP(2)
         ELSE
            EXP1 = 0.
            EXP2 = 999999.
         ENDIF
         CALL XVP('EXTEXPO',EFNUM,IEDC)
         IF (EFNUM .EQ. 0) IEDC=0   !Check for extended dark current

         CALL XVUNIT(IUNI,'INP',1,STAT,' ')
         CALL XVOPEN(IUNI,STAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
         CALL XVSIZE(SLI,SSI,NLI,NSI,dummy1,dummy2)
         CALL LABPROC(IUNI,LABEL,NLAB)
C        Read in area size fields...
         CALL XLGET(IUNI,'HISTORY','NUM_AREAS',NAREA,STAT,
     &              'FORMAT','INT',' ')
         CALL XLGET(IUNI,'HISTORY','AREAS',AREA,STAT,'NELEMENT',4*NAREA,
     &              'FORMAT','INT',' ')
         CALL XLGET(IUNI,'HISTORY','NUM_EXPOS',NEXP,STAT,
     &              'FORMAT','INT',' ')
         CALL XLGET(IUNI,'HISTORY','EXPOSURES',EXPOS,STAT,
     &              'NELEMENT',NEXP,'FORMAT','REAL',' ')
         L1 =1 
         L2 = NLI
         NPTS = NLI - IEDC	!Number of points (exposures) of light transfer curve
C
C  Compute mean DN (U) and sigma (S) for each area...
C      U(1,K) = mean DC for area K
C  For L=2,3,4,...,NLI
C      U(L,K) = mean signal at exposure L-2 for area K (dark current subtracted)
C      S(L,K) = mean noise at exposure L-2
C  Note that U(2,K) = mean signal at exposure 0 = DC - DC = 0 
C
         DO 50 L=1,NLI        !Loop through each exposure record...
            CALL XVREAD(IUNI,OUT,STAT,'LINE',L,'NSAMPS',NSI,' ')
            NI=NINT(OUT(1))
            IF (NI .EQ. 0) THEN
               BEXPO=OUT(L)
               GOTO 970
            END IF
            IF (EXPOS(L) .LT. EXP1) L1=L+1
            IF (EXPOS(L) .LE. EXP2) L2=L
            IB=1

            DO 50 K=1,NAREA	   !Loop through each area...
               SL=AREA(1,K)                      !Get area size field
               SS=AREA(2,K)
               NL=AREA(3,K)
               NS=AREA(4,K)
               CALL MVE(9,NI,OUT(IB+1),SSUM,1,1)
               CALL MVE(9,NI,OUT(IB+NI+1),SSUM2,1,1)
               CALL MVE(9,NI,OUT(IB+2*NI+1),SSUMXY,1,1)
               N = NL*NS
               SIGMA = 0.D0
               RMEAN = SSUM(1)

               DO I=2,NI
                  RMEAN = RMEAN+SSUM(I)
                  SIGMA = SIGMA+(SSUM2(I)+SSUM2(I-1)-2*SSUMXY(I-1))/N
     &                    -((SSUM(I)-SSUM(I-1))/N)**2
               ENDDO

               RMEAN = RMEAN/(NI*N)

               IF (NI .GT. 1) SIGMA=SIGMA/(NI-1)
               IF (SIGMA .LT. 0.D0) SIGMA=0.D0

               IF (IEDC .EQ. 0) THEN
                  IF (L .EQ. 1) U(1,K)=RMEAN		!mean DC
c                  U(L+1,K) = RMEAN - U(1,K)		!signal = DN - DC
c                  S(L+1,K) = DSQRT(SIGMA/2.D0)
		    U(L,K) = RMEAN !- U(1,K)           !signal = DN - DC
		    S(L,K) = DSQRT(SIGMA/2.D0)
               ELSE
                  IF (L .EQ. 1) THEN
                     EDC(K) = RMEAN			!extended DC
                     GOTO 50
                  ENDIF
                  IF (L .EQ. 2) U(1,K)=RMEAN		!normal DC
                  IF (L .LT. EFNUM+3) THEN
                     U(L,K) = RMEAN - U(1,K)
                  ELSE
                     U(L,K) = RMEAN - EDC(K)
                  ENDIF
                  S(L,K) = DSQRT(SIGMA/2.D0)
               ENDIF
           
   50    IB = IB+3*NI

         IF (L1 .GE. L2) GOTO 998
         IF (IEDC .EQ. 1) THEN
             L1 = L1 - 1		!EDC is not stored in U or S
             L2 = L2 - 1
         ENDIF

         MAXL = 0			!Maximum line number encountered
         MAXS = 0			!Maximum sample number encountered
         MAXL2 = 0			!Maximum last line number encountered
         MAXS2 = 0			!Maximum last sample number encountered
C
C          Compute system gain constant in electrons per DN (EPDN)
C          and read noise...
C
         DO 100 K=1,NAREA
            SL=AREA(1,K)                      !Get area size field
            SS=AREA(2,K)
            NL=AREA(3,K)
            NS=AREA(4,K)
            MAXL = MAX0(MAXL,SL)
            MAXS = MAX0(MAXS,SS)
            MAXL2 = MAX0(MAXL2,SL+NL-1)
            MAXS2 = MAX0(MAXS2,SS+NS-1)
            N = L2 - L1 + 1		!Number of light levels in fit
            UI = 0.D0
            UI2 = 0.D0
            SI2 = 0.D0
            UISI2 = 0.D0
C           Least squares fit over signal/noise curve...
            DO L=L1,L2
               RMEAN = U(L+1,K)	!mean signal (DN)
               SIGMA = S(L+1,K)	!noise (DN)
               UI = UI + RMEAN
               UI2 = UI2 + RMEAN**2
               SI2 = SI2 + SIGMA**2
               UISI2 = UISI2 + RMEAN*SIGMA**2
            ENDDO

            D = N*UI2 - UI**2			!Calculate read noise and
            IF (D .ne. 0.) THEN
               RDN = (SI2*UI2-UISI2*UI)/D	!system gain constant (in
            else
               rdn = 0.
            endif
            IF (RDN .GT. 0.) RDN=SQRT(RDN)	!electrons per DN) via least
            EPDN = D/(N*UISI2-UI*SI2)		!squares fit...
            RDNBUF(K) = RDN
            EPDBUF(K) = EPDN

            WRITE (MS1(6:8),'(I3)') K
            WRITE (MS1(25:28),'(I4)') SL
            WRITE (MS1(30:33),'(I4)') SS
            WRITE (MS1(35:38),'(I4)') NL
            WRITE (MS1(40:43),'(I4)') NS
            WRITE (MS2(3:14),'(F12.5)') EPDN
            WRITE (MS2(25:36),'(F12.5)') RDN
            MS1(46:85) = MS2
            IF (DBUG) CALL XVMESSAGE(' ',' ')
            CALL XVMESSAGE(MS1,' ')
c	print out all areas	   
            IF (DBUG)
     &         CALL AREADATA(EXPOS(1),U(1,K),S(1,K),NPTS,EPDN,RDN)
  100    CONTINUE

         CALL ZIA(BAD,NAREA)
C        Calculate global system gain and noise constants and
C        weed out bad areas...
         IF (NAREA .GT. 1) THEN
            CALL XVMESSAGE(' ',' ')
            CALL XVMESSAGE('Global value for K...',' ')
            CALL IMEAN(EPDBUF,1,NAREA,BAD)
            CALL XVMESSAGE(' ',' ')
            CALL XVMESSAGE('Global noise floor...',' ')
            CALL IMEAN(RDNBUF,2,NAREA,BAD)
         ENDIF

         CALL XVMESSAGE(' ',' ')
         MS1(1:70) = ' '
         I = 0
C        Report all bad areas...              
         DO 200 K=1,NAREA
            SL=AREA(1,K)                      !Get area size field
            SS=AREA(2,K)
            NL=AREA(3,K)
            NS=AREA(4,K)

            MS1 = ' '
            WRITE (MS1,'(A4,I4,A16,I4,A1,I4,A1,I4,A1,I4,A1)')
     &        'AREA',K,'(SL,SS,NL,NS)=(',SL,',',SS,',',NL,',',NS,')'
            IF (BAD(K).EQ.0) GOTO 200
            IF (BAD(K).EQ.1) MS1(47:71) = '*****BAD SYSTEM GAIN*****'
            IF (BAD(K).EQ.2) MS1(47:71) = '*****BAD NOISE FLOOR*****'
            IF (BAD(K).EQ.4) MS1(47:71) = '****BOTH CONSTANTS BAD***'
            CALL XVMESSAGE(MS1,' ')

            IF (IREJCT .EQ. 0) GOTO 200
            IF ((IREJCT .NE. 3) .AND.
     &          (IREJCT .NE. BAD(K)) .AND.
     &          (BAD(K) .NE. 4)) GOTO 200
            I = I + 1
            OUTM(1,I)=SL+NL/2   !Store center of bad area in MARK output record
            OUTM(2,I)=SS+NS/2
  200    CONTINUE
C
C          Compute means of all good areas
C
         GAREA = 0         !number of good areas
         EPDNMEAN = 0.D0   !mean system gain constant
         RDNMEAN = 0.D0	   !mean read noise

         DO 210 K=1,NAREA
            IF ((IREJCT .GT. 0 .AND. IREJCT .EQ. BAD(K)) .OR.
     &          (IREJCT .EQ. 3 .AND. BAD(K) .GT. 0) .OR.
     &          (IREJCT .GT. 0 .AND. BAD(K) .EQ. 4)) GOTO 210
            GAREA = GAREA + 1
            EPDNMEAN = EPDNMEAN + EPDBUF(K)
            RDNMEAN = RDNMEAN + RDNBUF(K)
  210    CONTINUE

         EPDNMEAN = EPDNMEAN/GAREA
         RDNMEAN = RDNMEAN/GAREA
         NREJCT = NAREA - GAREA

         MSG(43:91)= ' '
         CALL XVMESSAGE(' ',' ')
         IF (IREJCT.EQ.1) RMSG(21:34)=' SYSTEM GAIN  '
         IF (IREJCT.EQ.2) RMSG(21:34)=' NOISE FLOOR  '
         IF (IREJCT.EQ.3) RMSG(21:34)=' NOISE OR GAIN'
	 if (irejct .eq. 0) then
            call xvmessage('No rejection criteria applied',' ')
	 else
            WRITE (RMSG(39:41),'(I3)') NREJCT
            CALL XVMESSAGE(RMSG,' ')
	 end if
         WRITE (ARMES(22:25),'(I4)') GAREA
         WRITE (ARMES(33:36),'(I4)') NAREA
         CALL XVMESSAGE(ARMES,' ')
         WRITE (MS2(3:14),'(F12.5)') EPDNMEAN
         WRITE (MS2(25:36),'(F12.5)') RDNMEAN
         CALL XVMESSAGE(MS2,' ')
         CALL XVMESSAGE(' ',' ')
C
C          Write centers of bad areas into MARK format file...
C
         CALL XVUNIT(OUNI,'OUT',1,STAT,' ')
         IF (STAT.NE.1) GOTO 300		!Skip if no output specified...
         NSO = 2*NREJCT
         NLO = 1
         CALL XVOPEN(OUNI,STAT,'OPEN_ACT','SA','IO_ACT','SA',
     &               'OP','WRITE','U_NL',NLO,'U_NS',NSO,'U_FORMAT',
     &               'REAL','O_FORMAT','REAL',' ')
         IF (IREJCT .EQ. 1)
     &      CALL XLADD(OUNI,'HISTORY','CCDNOISE',
     &                 ' REJECTED FOR SYSTEM GAIN',STAT,'FORMAT',
     &                 'STRING',' ')      
         IF (IREJCT .EQ. 2)
     &      CALL XLADD(OUNI,'HISTORY','CCDNOISE',
     &                 ' REJECTED FOR NOISE FLOOR',STAT,'FORMAT',
     &                 'STRING',' ')      
         IF (IREJCT .EQ. 3)
     &      CALL XLADD(OUNI,'HISTORY','CCDNOISE',
     &                 ' REJECTED FOR BOTH',STAT,'FORMAT',
     &                 'STRING',' ')
         CALL XVWRIT(OUNI,OUTM,STAT,'NSAMPS',NSO,' ')
         CALL XVCLOSE(OUNI,STAT,' ')

C        Plot or table ....
  300    IF ((plotdata) .and. (table .eq. 0)) GOTO 400
C
C-----Set Reticle points based upon the last line/samp used
	 reticle(1,1) = 1            !upper left
	 reticle(2,1) = 1
	 reticle(1,2) = 1            !upper right
	 reticle(2,2) = maxs2
	 reticle(1,3) = maxl2        !lower left
	 reticle(2,3) = 1
	 reticle(1,4) = maxl2        !lower right
	 reticle(2,4) = maxs2
	 reticle(1,5) = maxl2/2      !center
	 reticle(2,5) = maxs2/2
c  zero out ng
	do i=1,5
	  ng(i) = 0
	enddo
C    zero out new arrays
            DO I=1,5
                DO J=1,NPTS
                    RSIGNAL(J,I)=0.0
                    RNOISE(J,I)=0.0
                ENDDO
            ENDDO

c
C        Compute signal and noise at each of the reticle points...
         DO 305 K=1,NAREA
            IF ((IREJCT .EQ. 1) .AND.
     &          ((BAD(K) .EQ. 1) .OR. (BAD(K) .EQ. 4))) GOTO 305
            IF ((IREJCT .EQ. 2) .AND.
     &          ((BAD(K) .EQ. 2) .OR. (BAD(K) .EQ. 4))) GOTO 305
            IF ((IREJCT .EQ. 3) .AND. (BAD(K) .NE. 0)) GOTO 305
            SL=AREA(1,K)                      !Get area size field
            SS=AREA(2,K)
            NL=AREA(3,K)
            NS=AREA(4,K)
C           Find nearest reticle...
            DMIN = 99999.**2
	    IMIN=0
            DO I=1,5
               D = (RETICLE(1,I)-SL)**2 + (RETICLE(2,I)-SS)**2
               IF (D .LT. DMIN) THEN
                  DMIN = D
                  IMIN = I
               ENDIF
            ENDDO
C           Add signal and noise data to that reticle...
            NG(IMIN) = NG(IMIN) + 1
C    zero out new arrays
            DO J=1,NPTS
               RSIGNAL(J,IMIN) = RSIGNAL(J,IMIN) + U(J,K)	!U(J+1,K)
               RNOISE(J,IMIN) = RNOISE(J,IMIN) + S(J,K)		!S(J+1,K)
            ENDDO
  305    CONTINUE

         DO I=1,5
            IF (NG(I) .GT. 0) THEN
               DO J=1,NPTS
                  RSIGNAL(J,I) = RSIGNAL(J,I)/NG(I)
                  RNOISE(J,I) = RNOISE(J,I)/NG(I)
               ENDDO
            ENDIF
         ENDDO
C
C        Compute system gain constant and read noise at reticles...
         DO 350 I=1,5
            N = L2 - L1 + 1		!Number of light levels in fit
            UI = 0.D0
            UI2 = 0.D0
            SI2 = 0.D0
            UISI2 = 0.D0
C           Least squares fit over signal/noise curve...
            DO L=L1,L2
               RMEAN = RSIGNAL(L,I)  !mean signal (DN)
               SIGMA = RNOISE(L,I)   !noise (DN)
               UI = UI + RMEAN
               UI2 = UI2 + RMEAN**2
               SI2 = SI2 + SIGMA**2
               UISI2 = UISI2 + RMEAN*SIGMA**2
            ENDDO

            D = N*UI2 - UI**2			!Calculate read noise and
            RDN = (SI2*UI2-UISI2*UI)/D	!system gain constant (in
            IF (RDN .GT. 0.) RDN=SQRT(RDN)	!electrons per DN) via least
            EPDN = D/(N*UISI2-UI*SI2)		!squares fit...
            RRDN(I) = RDN
            REPDN(I) = EPDN
350      continue

         if (plotdata) then
            CALL NPLOT(NPTS,EXPOS(IEDC+1),RSIGNAL,RNOISE,REPDN,RRDN,
     &     IREJCT,NG,LABEL,NLAB,ARMES,RMSG,MS2,plotgpi,nplotgpi,plotgpi2,
     &     nplotgpi2,epsplot,ploteps,nploteps,tbl,ntbl,*997)
         end if

         if (table .eq. 1) then
            call wrttbl(npts,expos(iedc+2),rsignal,rnoise,repdn,rrdn,
     &             table_ds,*999)
         end if

         CALL XVCLOSE(IUNI,STAT,' ')

  400    CALL XVMESSAGE('CCDNOISE task completed',' ')
         RETURN
CCCCCCCCCCCCCCCC
C          ERROR CONDITIONS
  970    CALL PRNT(7,1,EXPO,'***No data for exposure=.')
         CALL XVMESSAGE('***Run MOMGEN on this exposure and try again.',
     &               ' ')
         GOTO 999
  997    CALL XVMESSAGE('??E - Plot Error.',' ')
         GOTO 999
  998    CALL XVMESSAGE('??E - Err in exposure ranges',' ')
  999    CALL XVMESSAGE('??E - CCDNOISE task cancelled',' ')
         CALL ABEND
       END


C***********************************************************************8
	SUBROUTINE WRTTBL(NPTS,EXP,RSIG,RNOI,REPDN,RRDN,TABLE_DS,*)
C---Routine to write signal and noise tab-delimitted table
c
	implicit none
	   INTEGER*4 NPTS,JST,L
           REAL*4 RSIG(30,5),RNOI(30,5),REPDN(5),RRDN(5)
           REAL*4 EXP(npts),CN1,CN2,CN3,CN4,CN5

           CHARACTER*1 TAB
           CHARACTER*40 TABLE_DS

C-------RSIG AND RNOI ARE (NPTS,NUMBER OF RETICLE AREAS)
C-------REPDN AND RRDN ARE ELECTRONS/DN AND READ NOISE AND ARE
C-------(NUMBER OF RETICLE AREAS)

           OPEN(11,FILE=TABLE_DS,STATUS='UNKNOWN',RECL=1200,
     &          IOSTAT=JST,ERR=999)
	   TAB=CHAR(9)
	
C-------write header for table identifying columns
	   WRITE(11,8) 'Exp.time',
     &       TAB,'Signal-UL',TAB,'Noise-UL',TAB,'Calc.noise-UL',
     &       TAB,'Signal-UR',TAB,'Noise-UR',TAB,'Calc.noise-UR',
     &       TAB,'Signal-LL',TAB,'Noise-LL',TAB,'Calc.noise-LL',
     &       TAB,'Signal-LR',TAB,'Noise-LR',TAB,'Calc.noise-LR',
     &       TAB,'Signal-CEN',TAB,'Noise-CEN',TAB,'Calc.noise-CEN'

	   DO L=1,NPTS
C-------compute the computed noise value for each area at this exp. level
	      CN1 = SQRT(RSIG(L,1)/REPDN(1) + RRDN(1)**2)
	      CN2 = SQRT(RSIG(L,2)/REPDN(2) + RRDN(2)**2)
	      CN3 = SQRT(RSIG(L,3)/REPDN(3) + RRDN(3)**2)
	      CN4 = SQRT(RSIG(L,4)/REPDN(4) + RRDN(4)**2)
	      CN5 = SQRT(RSIG(L,5)/REPDN(5) + RRDN(5)**2)
	      WRITE(11,9) EXP(L),TAB,RSIG(L,1),TAB,RNOI(L,1),TAB,CN1,
     &          TAB,RSIG(L,2),TAB,RNOI(L,2),TAB,CN2,
     &          TAB,RSIG(L,3),TAB,RNOI(L,3),TAB,CN3,
     &          TAB,RSIG(L,4),TAB,RNOI(L,4),TAB,CN4,
     &          TAB,RSIG(L,5),TAB,RNOI(L,5),TAB,CN5
	   ENDDO

	   CLOSE(11)
	   RETURN

8	   FORMAT(1X,A8,15(A1,A14))
9	   FORMAT(1X,F12.5,15(A1,F12.4))
999	   CALL xvmessage('??E - ERROR OPENING TABLE FILE',' ')
	   CALL PRNT(4,1,JST,' IOSTAT =.')
	   RETURN 1
	END
C**********************************************************************
       SUBROUTINE IMEAN(BUF,TYPE,N,BAD)

C Routine to flag all areas which differ from mean by more than 2 sigma.
C Inputs: BUF(N) = input samples
C         TYPE = 1 for system gain constant, 2 for read noise
C         N = number of samples
C Outputs: BAD(N) = TYPE if sample is bad type
C                 = 4 if sample has both bad gain and noise

	implicit none

         INTEGER*4 BAD(*),TYPE,N,NS
	 INTEGER*4 K,IMAX

         REAL*4 BUF(*),MEAN,MAXDIFF,SAMP,SIGMA
         REAL*8 SUM,SSUM

         CHARACTER*43 MSG

         MSG = 'N=**** MEAN=******.***** SIGMA=******.******'

	 IMAX=0
         NS = 0			!Count of number of good samples
         SUM = 0.0D0
         SSUM = 0.0D0
C	 Get sums and sums of squares of all good areas...
         DO K=1,N
            IF ((BAD(K) .NE. TYPE) .AND. (BAD(K) .NE. 4)) THEN 
               SAMP = BUF(K)
               NS = NS+1
               SUM = SUM+DBLE(SAMP)
               SSUM = SSUM+DBLE(SAMP)*DBLE(SAMP)
            ENDIF
         ENDDO
C          ...and use these to compute mean and standard deviation.
         MEAN = SNGL(SUM)/NS
         SIGMA = SNGL(SSUM)/NS-MEAN*MEAN
         IF (SIGMA .GT. 0.0) SIGMA = SQRT(SIGMA)
         WRITE (MSG(3:6),'(I4)') NS
         WRITE (MSG(13:24),'(F12.5)') MEAN
         WRITE (MSG(32:43),'(F12.5)') SIGMA
         CALL XVMESSAGE('Raw mean and sigma are...',' ')
         CALL XVMESSAGE(MSG,' ')
         IF (SIGMA .EQ. 0.0) RETURN

C        Weed out all samples differing by more than 2 sigma from mean...
   10    MAXDIFF = 0.
C        First find sample with largest difference...
         DO 20 K=1,N
            IF ((BAD(K) .EQ. TYPE) .OR. (BAD(K) .EQ. 4)) GOTO 20
            SAMP = ABS(BUF(K)-MEAN)
            IF (SAMP .LE. MAXDIFF) GOTO 20
            MAXDIFF = SAMP
            IMAX = K          
   20    CONTINUE

C        Continue if remaining samples are good.
         IF (MAXDIFF .LE. 2*SIGMA) GOTO 50  
         SAMP = BUF(IMAX)
         SUM = SUM - SAMP			!Delete worst sample from sums,
         SSUM = SSUM - SAMP**2
         NS = NS - 1
         IF (BAD(IMAX) .NE. 0) THEN		!and flag area as bad.
            BAD(IMAX) = 4
         ELSE
            BAD(IMAX) = TYPE
         ENDIF
         GOTO 10

   50    MEAN = SNGL(SUM)/NS
         SIGMA = SNGL(SSUM)/NS-MEAN**2
         IF (SIGMA .GT. 0.0) SIGMA = SQRT(SIGMA)
         WRITE (MSG(3:6),'(I4)') NS
         WRITE (MSG(13:24),'(F12.5)') MEAN
         WRITE (MSG(32:43),'(F12.5)') SIGMA
         CALL XVMESSAGE 
     &      ('After throwing out samples differing by 2 sigma',
     &               ' ')
         CALL XVMESSAGE(MSG,' ')
         RETURN
      END         
C********************************************************************
      SUBROUTINE AREADATA(EXPOS,U,S,NPTS,EPDN,RDN)

C Routine to print out data for area...
C Inputs: EXPOS = exposure times in milliseconds
C         U = mean signal in DN
C         S = RMS noise in DN
C         NPTS = number of points of curve
C         EPDN = system gain constant (electrons/DN)
C         RDN = read noise (DN)
C
	implicit none
	 INTEGER*4 NPTS,L
         REAL*4 EPDN,RDN,R,RATIO,RMEAN,SIGMA,TNOISE
         REAL*4 EXPOS(NPTS),U(NPTS),S(NPTS),EXPO

         CHARACTER*132 MSG

         MSG = ' '
         MSG(1:51) = '    EXPOSURE   MEAN       MEASURED   LOG'          
         MSG(52:132) = 'LOG         COMPUTED       RATIO'
         CALL XVMESSAGE (MSG, ' ')
         MSG = ' '
         MSG(1:51) = '    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)'
         MSG(52:132) = 'NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)'
         CALL XVMESSAGE (MSG, ' ')

         DO L=1,NPTS
            EXPO = EXPOS(L)
            RMEAN = U(L)
            SIGMA = S(L)
            IF ((RMEAN .LT. 0.) .OR. (EPDN .LE. 0)) THEN
               TNOISE = 0.
               RATIO = 0.
            ELSE
               TNOISE = SQRT(RMEAN/EPDN + RDN**2)
               RATIO = SIGMA/TNOISE
            ENDIF
            MSG = ' '
            WRITE (MSG(2:13),'(F12.5)') EXPO !Exposure time (msec)
            WRITE (MSG(15:24),'(F10.4)') RMEAN !Mean signal (DN)
            WRITE (MSG(26:36),'(F11.5)') SIGMA !Noise (DN)
            IF (RMEAN .GT. .1D0) THEN
               R = LOG10(RMEAN)
               WRITE (MSG(38:47),'(F10.5)') R !LOG signal (DN)
            ENDIF
            IF (SIGMA .GT. 0.D0) THEN
               R = LOG10(SIGMA)
               WRITE (MSG(49:58),'(F10.5)') R !LOG noise (DN)
            ENDIF
            WRITE (MSG(60:69),'(F10.5)') TNOISE !Computed noise (DN)
            WRITE (MSG(71:80),'(F10.5)') RATIO !Ratio measured/computed
            CALL XVMESSAGE(MSG,' ')
         ENDDO

         RETURN
      END
C********************************************************************
      SUBROUTINE NPLOT(NPTS,EXPOS,RSIGNAL,RNOISE,REPDN,RRDN,IREJCT,
     &     NG,LABEL,NLAB,ARMES,RMSG,MS2,plotgpi,nplotgpi,plotgpi2,
     &     nplotgpi2,epsplot,ploteps,nploteps,tbl,ntbl,*)
C
C Routine to plot signal vs noise for the 5 reticles...
C All arguments are inputs.
C
        implicit none
         INTEGER*4 NPTS,NLAB,I,II,J,JJ,JST,L,N,IREJCT
	 INTEGER*4 plotwid,plotht,isize,nxtitle,nytitle
	 INTEGER*4 nplotgpi,nplotgpi2,nploteps,ntbl,nltitle
         INTEGER*4 ntitle(80),ncolx(5),ncoly(5)
         REAL*4 RMEAN,SIGMA,labstep,tmp,xrang1,xrang2,yrang1,yrang2
	 REAL*4 maxrmean,minrmean,maxsigma,minsigma
         REAL*4 EXPOS(30),RSIGNAL(30,5),RNOISE(30,5),REPDN(5),RRDN(5)
         REAL*4 X(30),Y(30) !Buffers for log signal and log noise

         INTEGER*4 NG(5),ICNT    !NG(30) befor 18-Nov-2012

	 LOGICAL*4 epsplot

	 CHARACTER*12 MSG(5), LMSG(6)
         CHARACTER*20 YTITLE
         CHARACTER*20 XTITLE
         CHARACTER*41 RMSG,MS2
         CHARACTER*44 REJSTR(3)
         CHARACTER*51 ARMES
         CHARACTER*62 MSG2
	 CHARACTER*80 ltitle
	 character*80 plotgpi,plotgpi2,ploteps,tbl
	CHARACTER*86 TITLE(80)
         CHARACTER*4320 LABEL

	 REJSTR(1)='Rejected for SYSTEM GAIN'
	 REJSTR(2)='Rejected for NOISE FLOOR'
	 REJSTR(3)='Rejected for SYSTEM GAIN and NOISE FLOOR'
         MSG2(1:52)='UPPER-RIGHT CORNER  K=*****.* E/DN  RDN=***.*** DN'
         MSG2(53:62) = 'NAREA=*** '
         ltitle = 'CCD NOISE ANALYSIS'
	 nltitle = index(ltitle,'    ') - 1
         icnt = 1
         ytitle = 'LOG NOISE (DN)'
	 nytitle = index(ytitle,'    ') - 1 
         xtitle = 'LOG SIGNAL (DN)'
	 nxtitle = index(xtitle,'    ') - 1
c
cnplotname=index(plotname,'   ') - 1
         MSG(1)='=UPPER-LEFT '
         MSG(2)='=UPPER-RIGHT'
         MSG(3)='=LOWER-LEFT '
         MSG(4)='=LOWER-RIGHT'
         MSG(5)='=CENTER     '
c
c  process labels
c
         N = MIN0(NLAB,25)
         DO L=1,N
            TITLE(ICNT)=LABEL((L-1)*72+1:(L-1)*72+72)
            ntitle(icnt) = index(title(icnt),'       ') - 1
            ICNT=ICNT+1
         ENDDO
	 if (irejct .eq. 0) then
	    TITLE(ICNT) = "No Rejection Criteria Applied"
	    ntitle(icnt) = index(title(icnt),'       ') - 1
	    ICNT=ICNT+1
	 else
	    TITLE(ICNT) = REJSTR(IREJCT)
	    ntitle(icnt) = index(title(icnt),'       ') - 1
            ICNT=ICNT+1
         endif
         TITLE(ICNT)=ARMES
	 ntitle(icnt) = index(title(icnt),'       ') - 1
         ICNT=ICNT+1
c         TITLE(ICNT)=MS2
c	 ntitle(icnt) = index(title(icnt),'       ') - 1

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

	open(98,file=plotgpi(1:nplotgpi),status='UNKNOWN',iostat=jj,err=995)
	if (epsplot) then
           open(97,file=plotgpi2(1:nplotgpi2),status='UNKNOWN',iostat=jj,err=996)
	endif

            call write_gpi_1(98,tbl,ntbl,ytitle,nytitle,
     1 xtitle,nxtitle,isize,plotwid,plotht,
     2 ploteps,nploteps)
	    if (epsplot) then
		call write_gpi_1(97,tbl,ntbl,ytitle,nytitle,
     1 xtitle,nxtitle,isize,plotwid,plotht,
     2 ploteps,nploteps)
	    endif
	do j=1,5
		ncolx(j) = j
		ncoly(j) = j + 5
c		print *,'j ncolx(j) ncoly(j) = ',j,ncolx(j),ncoly(j)
	enddo
	
       OPEN(12,FILE=tbl(1:ntbl),STATUS='UNKNOWN',IOSTAT=JST,ERR=999)

	do j=1,npts
	   write (12,10010) (LOG10(RSIGNAL(J,I)), i=1,5),(LOG10(RNOISE(J,I)), i=1,5) 
	enddo
10010   format (10e10.3,1x)
	close (12)
C
C	PLOTS LOOP	   
C        Generate plot for each reticle...
	maxrmean = -1.0e20
        minrmean = 1.0e20
        maxsigma = -1.0e20
	minsigma = 1.0e20
         DO 100 I=1,5
            IF (NG(I) .EQ. 0) GOTO 20
            DO J=1,NPTS
               RMEAN = RSIGNAL(J,I)
               IF (RMEAN .GT. 0.) RMEAN=LOG10(RMEAN)
               if (rmean .gt. maxrmean) maxrmean = rmean
	       if (rmean .lt. minrmean) minrmean = rmean
               X(J) = RMEAN
               SIGMA = RNOISE(J,I)
               IF (SIGMA .GT. 0.) SIGMA=LOG10(SIGMA)
	       if (sigma .gt. maxsigma) maxsigma = sigma
               if (sigma .lt. minsigma) minsigma = sigma
               Y(J) = SIGMA
            ENDDO
   20    CONTINUE
c         icnt=2
         MSG2(1:11) = MSG(I)(2:12)
         IF (I .EQ. 5) MSG2(13:19) = ' '
         WRITE (MSG2(23:29),'(F7.1)') REPDN(I)
         WRITE (MSG2(41:47),'(F7.3)') RRDN(I)
         WRITE (MSG2(59:61),'(I3)') NG(I)
         CALL XVMESSAGE(' ',' ')
         CALL XVMESSAGE(MSG2,' ')
         IF (NG(I) .EQ. 0) THEN
            CALL XVMESSAGE(' ',' ')
            CALL XVMESSAGE
     &         ('??E - No good areas in this part of frame',' ')
            GOTO 100
         ENDIF
         CALL AREADATA(EXPOS(1),RSIGNAL(1,I),RNOISE(1,I),NPTS,		!NPTS
     &          REPDN(I),RRDN(I))

         TITLE(ICNT)=MSG2
	if (i .eq. 5) then
	   ntitle(icnt) = 61
	else
           ntitle(icnt) = index(title(icnt),'       ') - 1
	endif
	yrang2 = maxsigma + 0.1
	yrang1 = minsigma - 0.1
	xrang2 = maxrmean + 0.1
	xrang1 = minrmean - 0.1
           call write_gpi_2(98,ltitle,nltitle,xrang1,xrang2,
     1 yrang1,yrang2,isize)
	   if (epsplot) then
           call write_gpi_2(97,ltitle,nltitle,xrang1,xrang2,
     1 yrang1,yrang2,isize)

	   endif

        call write_gpi_lab(98,icnt,title,ntitle,labstep,isize)
	if (epsplot) then
	   call write_gpi_lab(97,icnt,title,ntitle,labstep,isize)
        endif
ccc program power reads label
         DO II=1,6
            LMSG(II)=' '
         ENDDO
         LMSG(I+1)=MSG(I)

        call write_ls_gpi_1(98,tbl,ntbl,ncolx,ncoly,i,msg)
	if (epsplot) then
	   call write_ls_gpi_1(97,tbl,ntbl,ncolx,ncoly,i,msg)
        endif
100      continue
c	END PLOTS LOOP


	RETURN
c   error returns
995     call xvmessage('??E - nplot: Error opening/writing gnuplot file',' ')
        call abend
	return
996	call xvmessage('??E - nplot: Error opening/writing gnuplot eps file',' ')
        call abend
	return

999      CALL XVMESSAGE ('??E - ERROR OPENING PLOT data file',' ')
         CALL PRNT(4,1,JST,'IOSTAT=.')
         CALL ABEND
	RETURN

	END
C********************************************************************
	subroutine write_gpi_1(unit,tbl,ntbl,ylabel,nylabel,
     1 xlabel,nxlabel,isize,plotwid,plotht,
     2 eps,neps)
c            
c       write out the first part of gpi form
c
        implicit none
        integer*4 unit,isize,plotwid,plotht
        integer*4 jj,psize
        integer*4 ntbl,nylabel,nxlabel,neps
        character*16 ylabel,xlabel
        character*80 tbl,eps

c
	psize = isize
	if (unit .eq. 97) psize = 16
10100 format('# Created by program ccdnoise')              !#'s are ignored in gnuplot
            write(unit,fmt=10100,iostat=jj,err=995)
10105 format('# Gnuplot commands for log(signal)vs log(noise) plot')
            write(unit,fmt=10105,iostat=jj,err=995)
10110 format('# Data in ',a)
            write(unit,fmt=10110,iostat=jj,err=995) tbl(1:ntbl)
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

10141 format("set clip points")                         !how to deal with points out of range
            write(unit,fmt=10142,iostat=jj,err=995)
10142 format("set clip one")                            !how to deal with connecting lines out of range
            write(unit,fmt=10141,iostat=jj,err=995)
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
C**************************************************************************
       subroutine write_gpi_2(unit,title,ntitle,xrang1,xrang2,
     1 yrang1,yrang2,isize)
c	,mycol,nplots,iplot,fscale,nocol,nncol,
c     2 oldcol,newcol,nstart,npoint,ncontr,filename,nfilename)
c
c       write out the repeatable descriptive part of gpi form
c       for first, or only, plot
c
        implicit none
        integer*4 unit,isize,psize
        integer*4 jj
        integer*4 ntitle
        real*4 xrang1,xrang2,yrang1,yrang2
        character*80 title
c
        psize = isize
        if (unit .eq. 97) psize = 16

10145 format('set title "',a,'" font "Ariel,',i2,'"')
             write(unit,fmt=10145,iostat=jj,err=995) title(1:ntitle), psize
C
C---- SET THE SCALING PARAMETERS.
C
c       set y range to be like vicar image - lines counting downward from top
c	min = rang1      max =  rang2
10135 format("set yrange [",f10.2,":",f10.2,"]")
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
        subroutine write_gpi_lab(unit,nhdr,header,nheadr,labstep,isize)
c
c       write image labels to top of plot
c output labels for only top 60% of plot
c
c
        implicit none
        integer*4 unit,ii,jj,nhdr,isize,psize,nheadr(80)
        real*4  fpos,labstep
        character*86 header(80)
c
        psize = isize - 1
        if (unit .eq. 97) psize = 16

       write (unit,fmt=10165,iostat=jj,err=995)
10165 format ('unset label')

        fpos=1.0
        do ii=1,nhdr
                fpos = fpos - labstep
10160 format('set label ',i2,' "',a,'" at graph .25 ,',f5.2,
     1 ' font "ariel,',i2,'" front nopoint tc def')
        write(unit,fmt=10160,iostat=jj,err=995) ii,header(ii)(1:nheadr(ii)), fpos, psize
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
        subroutine write_ls_gpi_1(unit,tbl,ntbl,ncolx,ncoly,iplot,msg)
c
c       write out data in gpi form for pts, lines or linepoints
c       for a 1 value in control column
c
        implicit none
        integer*4 unit,ntbl
        integer*4 gcol,jj,iplot
        integer*4 ncolx(5),ncoly(5)
	CHARACTER*12 MSG(5)
        character*80 tbl
        character*100 outline

c       bring in gunplot colors, lines, points, etc
        include 'gnuplotchar'


        gcol = iplot
10253 format ("plot  '",a,"' u ",i2,":",i2," t '",a,"' w linespoints lt ",i2," pt ",i2,
     1 " ps 2 lw 2 lc rgb '",a,"'")
           write(unit,fmt=10253,iostat=jj,err=995) tbl(1:ntbl),ncolx(gcol),ncoly(gcol),msg(gcol),
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

C**************************************************************************
      SUBROUTINE LABPROC(IUNI,LABEL,NLAB)
c
         IMPLICIT NONE
 
         INTEGER INSTANCES(20),STAT,NLAB,IUNI,CNT,I,J

         CHARACTER*8 TASKS(20)
         CHARACTER*4320 LABEL
         CHARACTER*72 MSG
         CHARACTER*12 UNAME
         CHARACTER*28 TIME
         CHARACTER*65 HBUF
 
         hbuf='----TASK:------------USER:'
         MSG= ' '
         LABEL= ' '
         CALL VIC1LAB(IUNI,STAT,NLAB,LABEL,60)
         CNT=20                             !EXTRACTS VIC*2 LAB
         CALL XLHINFO(IUNI,TASKS,INSTANCES,CNT,STAT,' ')
         DO 801 J=1,CNT
            UNAME=' '
            TIME =' '
            CALL XLGET(IUNI,'HISTORY','USER',UNAME,STAT,'HIST',TASKS(J),
     &                 'INSTANCE',INSTANCES(J),'FORMAT','STRING',' ')
            CALL XLGET(IUNI,'HISTORY','DAT_TIM',TIME,STAT,'HIST',
     &                 TASKS(J),'INSTANCE',INSTANCES(J),'FORMAT',
     &                 'STRING',' ')
            DO 802 I=1,8                             !BLANKS NULLS
802            IF (TASKS(J)(I:I) .LT. '0') TASKS(J)(I:I)=' '
            HBUF(11:18)  = TASKS(J)(1:8)
            HBUF(28:39) = UNAME(1:12)
            HBUF(40:64) = TIME(1:25)
801      LABEL(1+(NLAB+J-1)*72:65+(NLAB+J-1)*72) =  HBUF(1:65)
         NLAB=NLAB+CNT
         DO 800 I=1,NLAB
            MSG = LABEL(1+(I-1)*72:1+(I-1)*72+71)
            CALL XVMESSAGE(MSG,' ')
800      MSG=' '
         RETURN
      END


