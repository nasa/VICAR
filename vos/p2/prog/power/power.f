      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C
C     POWER SPECTRUM PROGRAM
C
c     06/09/2011 .. RJB     Changed from commercial plotting package, xrt to gnuplot
c			    create gnuplot commands and always create a table and gpi file 
C     04/16/97  ... SP  ... Made portable for UNIX & VMS.  Replaced handling
C                           of SIZE parameter with call to XVSIZE.  Revised
C                           plotting code using program OTF1 as a guide.
C                           Revised label processing code to do string searches
C                           in an uppercase copy of the label data; this turned 
C                           out to be unnecessary: the problem was that the 
C                           FORMAT optional is needed in the XLGET.
C     11/16/95  ... CCA ... MADE SCAL R*4
C     07/18/94  ... CCA ... ADD OPTION OF NO TABLE COLUMN HEADERS
c     06/14/94  ... CCA ... FIXED FMAX, DELETED HISTOGRAM PLOT
c     04/11/94  ... CCA ... ADDED DNSCALE
c     02/22/94  ... CCA ... MAJOR CODE CLEANUP, REWORK OF LABEL HANDLING,
c			    TEST FILE AND PLOTTING OF LABELS
C     11/22/93  ... CCA ... ADD ASCII TABLE OUTPUT, CHECKS FOR OPEN
C                           ERRORS, FIX TEST, MOD SELECTION OF PLOT
C                           OR PRINT
C     09/01/84  ... FFM ... CONVERT FROM IBM TO VAX
C               T. RINDFLEISCH ... ORIGINAL RELEASE
C
c	In converting from xrg to gnuplot had to change len to index
c	because len is the size of the char variable but index to 2 spaces
c	shows actual length of string. When putting headers or filesnames
c	into gnuplot commands the array is too long
c
c	Inv 
	implicit none
	complex*8 C(1024) 

cc      COMMON /C1/ NLI,NSI
	integer*2 BUF(1024)
	integer*4 STATUS,COUNT,DEF,SL,SS
	integer*4 PTR,EL
	integer*4 icnt,inunit,j,k,kp,line,nhdr,nl,ns,nleft,np,nmid
	integer*4 jj,nli,nsi,ntitle,ntitx,ntot,ntbl,nplotgpi
	integer*4 i,ii,nploteps,plotwid,plotht,nheadr(70)
	integer*4 nplotgpi2,nplotf,isize,isize2,psize
	real*4 SCAL,KPP,BSCALE,fq,fmax,rn,rnn,rnor,vqav,vscl,x
	real*4 vav,y,ylen,fpos,labstep,tmp
	real*4 pow(513),plat(1025)
	real*8 dnor,dvr,dav,vf,vvr
	logical*4 XVPTST,epsplot
	character*1 tab
	character*4 eps/'.eps'/,gpi/'.gpi'/,asc/'.asc'/
        character*8 plotfmt
	character*24 tity,mess,msg3,msg5,msg6
	character*81 htitx,htitle
	character*60 msg2
	character*80  HEADR(70)    !strings for the plot header ala OTF1.
	character*255 filename,ploteps,plotout,tbl,plotgpi,filemsg,plotgpi2
	character*255 plotfile

      EQUIVALENCE (C(181),PLAT(1))

      DATA MESS/' MIPL RMS POWER SPECTRUM'/
      DATA TITY/' AMPLITUDE   (DN P-P)   '/
	DATA MSG2/'    TRANSFORM  SL =        SS =        NL =        NS =     '/
      DATA MSG3/'         POINT TRANSFORM'/
      DATA MSG5/'    MEAN (DN) =         '/
      DATA MSG6/'    SIGMA(DN) =         '/

	call xvmessage('power - Jul 13, 2013 (64-bit gnuplot) - rjb',' ')

	epsplot=.false.				!default = noplot to be saved
      htitx(1:80)=' '
      htitle(1:80)=' '

	nplotgpi=0
	nplotgpi2=0
	nploteps=0
C       OPEN INPUT DATA 
      CALL XVUNIT(INUNIT,'INP',1,STATUS,' ')
      CALL XVOPEN(INUNIT,STATUS,'U_FORMAT','HALF','OPEN_ACT', 'SA',
     .             'IO_ACT', 'SA' ,' ')

C       GET INPUT SIZE
      CALL XVSIZE( SL, SS, NL, NS, NLI, NSI)
C
C       PARAM 'EXPONENT'
      CALL XVPARM('EXPONENT',NP,COUNT,DEF,1)
      IF (COUNT .EQ. 0) THEN
         IF (NS .GE. 1024) THEN		!EXPONENT NOT GIVEN, MAKE ONE
		NP=10
	 ELSE IF (NS .GE. 512) THEN
		NP=9
	 ELSE IF (NS .GE. 256) THEN
		NP=8
	 ELSE IF (NS .GE. 128) THEN
		NP=7
	 ELSE IF (NS .GE. 64) THEN
		NP=6
	 ELSE IF (NS .GE. 32) THEN
		NP=5
	 ELSE IF (NS .GE. 16) THEN
		NP=4
	 ELSE 
         	NP=3
	 END IF
      END IF
C
C       PARAM 'SCALE'              !DN PER INCH OF PLOT
      CALL XVPARM('SCALE',SCAL,COUNT,DEF,1)
C
C       PARAM 'DNSCALE'              !SCALING OF DNS BEFORE POWER
      CALL XVPARM('DNSCALE',BSCALE,COUNT,DEF,1)
C
C       PARAM 'FMAX'
      CALL XVPARM('FMAX',FMAX,COUNT,DEF,1)
C
C       PARAM 'TITLE'
      CALL XVPARM('TITLE',HTITLE,COUNT,DEF,1)
      IF (COUNT .GT. 0) THEN
         CALL XVSPTR(HTITLE,COUNT,PTR,NTITLE)  !GET LENGTH AS ENTERED.
      ELSE
         NTITLE=0
      END IF
C
C       PARAM 'TITLEX' - Default is "FREQUENCY(CPS)"
	CALL XVPARM('TITLEX',htitx,COUNT,DEF,1)
c
c	ntitx=len(htitx)
	ntitx=index(htitx,'  ') - 1		!change len to index
C
C       PARAM 'YLEN'
      CALL XVPARM('YLEN',YLEN,COUNT,DEF,1)
      IF((YLEN .LT. 0.0).OR.(YLEN .GT. 30.0)) THEN
          CALL XVMESSAGE('??E - Illegal value for YLEN',' ')
          CALL ABEND
      END IF
C
C	IF (XVPTST('PRINT')) PRINT=1
C
	epsplot = .false.
	CALL XVPARM ('PLOTFMT',plotfmt,count,def,1)
	    if (plotfmt .eq. 'EPS' .or. plotfmt .eq. 'eps') epsplot = .true.
c	if FILE called then create a postscript file
	CALL XVPARM('PLOTOUT',PLOTFILE,COUNT,DEF,1)
	IF (COUNT .EQ. 1) THEN
		plotout = plotfile
		nplotf=index(plotout,'  ') - 1
		ploteps=plotout(1:nplotf)//eps
		nploteps=index(ploteps,'  ') - 1
		plotgpi=plotout(1:nplotf)//gpi
		nplotgpi=index(plotgpi,' ') - 1		!replaced len with index
	       plotgpi2=plotout(1:nplotf)//eps//gpi
               nplotgpi2=index(plotgpi2,'  ') - 1
            tbl = plotout(1:nplotf)//asc
            ntbl = index(tbl,'  ') - 1
	
	ELSE
	    if (epsplot) then
		ploteps='power.eps'
		nploteps=index(ploteps,' ') - 1
	    else
		PLOTFILE='NONE'
	    endif
	    plotgpi='power.gpi'
	    nplotgpi=index(plotgpi,' ') - 1
	    plotgpi2='power.eps.gpi'
	    nplotgpi2=index(plotgpi2,'  ') - 1
	    tbl = 'power.asc'
            ntbl = index(tbl,'  ') - 1

	END IF
C
C----------------------------------------------------------
C       NTOT = OUTPUT SIZE IN SAMPLES
C       NS=NUMBER OF SAMPLES 
      NTOT=2**NP
      NMID=NTOT/2+1
      IF (NS .GT. NTOT) NS=NTOT
C
C       PRINT OUT THE HEADER
      CALL XVMESSAGE(MESS(2:),' ')

C----------------------------------------------------------
C------PROCESSING SECTION
C       ADJUST NL AND NS SO THEY MAKE SENSE
      IF (SL+NL-1 .GT. NLI) THEN
	CALL XVMESSAGE('??W - Number of lines truncated',' ')
	NL=NLI-SL+1
      END IF
      IF (SS+NS-1 .GT. NSI) THEN
	CALL XVMESSAGE('??W - Number of samples truncated',' ')
	NS=NSI-SS+1
      END IF
      IF (NL+NS .LE. 0) GO TO 997

C       NLEFT=ADDITIONAL SAMPLES POWER WILL HAVE TO GENERATE

        NLEFT=NTOT-NS
        DO J=1,NMID
         POW(J)=0.0
        END DO
C
C       DAV=SUM OF "a" IN DOUBLE PRECISION
C       DVR=SUM OF "a**2" IN DOUBLE PRECISION
      DAV=0.0D+00
      DVR=0.0D+00
      DNOR=1.0D+00/DFLOAT(NS)
C
C       READ INPUT DATA INTO HALFWORD BUFFER
      EL=SL+NL-1
      DO LINE=SL,EL
         CALL XVREAD(INUNIT,BUF,STATUS,'LINE',LINE,'SAMP',SS,'NSAMPS',
     &               NS,' ')
         RN=0.0
         RNN=0.0
C          PUT THE DN IN THE REAL PART OF COMPLEX NUMBER AND ZERO OUT THE
C          IMAGINARY PART
         DO KP=1,NS
            KPP=FLOAT(BUF(KP))/BSCALE  !NOTE THAT KPP IS DECLARED REAL.
            RN=RN+KPP
            RNN=RNN+KPP*KPP
            C(KP)=CMPLX(KPP,0.0)
         END DO
C
C          AVERAGING THE AVAILABLE INPUT DATA
         VQAV= RN/FLOAT(NS)
         IF (NLEFT .LT. 1) GO TO 400
C
C          USE THE AVERAGE VALUE TO FILL IN THE ADDITIONAL SAMPLES
         DO KP=1,NLEFT
            C(NS+KP)=CMPLX(VQAV,0.0)
         END DO
  400    DAV=DAV+DBLE(RN)
         DVR=DVR+DBLE(RNN)
C    
C          1-DIMENSIONAL DIRECT FOURIER TRANSFORM
C        CALL PRNT(7,64,C,'1C*8 BUFFER BEFORE FFTT=.')
C
         CALL FFTT(NP,-1,C)
C
C        CALL PRNT(7,34,C,'1C*8 BUFFER AFTER FFTT=.') 
C
C          ADD THE RESULTS OF ONE LINE'S TRANSFORM TO A CUMULATIVE POWER
C          SPECTRUM

c--------Accumulate the amplitude of the complex data into POW
         CALL CMAG(NMID,C,POW)

C        CALL PRNT(7,NMID,POW,'1CUMULATIVE POWER SPECTRUM=.')
      END DO

	RNOR = 1.0/(NL*NS)

c-------Take summed amplitudes in POW and get mean Peak-to-peak amplitude
c-------at each frequency.
      DO K=1,NMID
         POW(K)= 2*POW(K)*RNOR
c        POW(K)=SQRT(POW(K)/NL)/NS*2      !wrong (see also in CMAG)
      END DO

c-----To make element one the mean DN of image, divide by 4
      POW(1)=POW(1)/4

C COMPUTE MEAN AND SIGMA FOR PRINTOUT
C      DNOR=1.0D+00/(DFLOAT(NS)*DFLOAT(NL))
	DNOR=DBLE(RNOR)
      DAV=DNOR*DAV
      VVR=DSQRT(DABS(DNOR*DVR-DAV*DAV))
      VAV=DAV
      WRITE (MSG2(20:24),'(I5)') SL
      WRITE (MSG2(32:36),'(I5)') SS
      WRITE (MSG2(44:48),'(I5)') NL
      WRITE (MSG2(56:60),'(I5)') NS
      WRITE (MSG3(4:8),'(I5)') NTOT
      WRITE (MSG5(16:24),'(F9.4)') VAV
      WRITE (MSG6(16:24),'(F9.4)') VVR
      CALL XVMESSAGE(MSG2(2:60),' ')
      CALL XVMESSAGE(MSG3(2:24),' ')
      CALL XVMESSAGE(MSG5(2:24),' ')
      CALL XVMESSAGE(MSG6(2:24),' ')

      VF=2.D0*FMAX/NTOT       !FREQ. FMAX IS AT ELEMENT NTOT/2

C------------------------------------------------------
C-------OPEN OUTPUT TEXT FILE TO PRINT ALL POINTS TO NMID
c	tbl="tmp.tbl"
c	CALL XVPARM('TABLE',TBL,COUNT,DEF,1)
c	ntbl=index(tbl,' ') - 1
cc	IF(COUNT .GT. 0) THEN
 	  TAB=CHAR(9)
	  OPEN(99,FILE=TBL(1:ntbl),STATUS='UNKNOWN',IOSTAT=J,ERR=998)

	  IF (XVPTST('COLHDR')) 
     1            WRITE(99,121) '# FREQUENCY',TAB,'RMS_POWER'

	  DO 119 K=1,NMID
	    FQ = (K-1)*0.5/NMID         !FREQ. 0.5 IS AT ELEMENT NMID
119	  WRITE(99,FMT=120,IOSTAT=J,ERR=998) FQ,TAB,POW(K)

	  CLOSE(99)
cc	END IF

121	FORMAT(1X,A9,A1,A9)
120	FORMAT(1X,F7.4,A1,F10.3)

C------------------------------------------------------

cc      IF (PLOTT .EQ. 1) THEN
C-----INITIALIZE THE PLOTTER
c	The following is for xrt package
c	plotfn
c	xrtbegin
c	displayaxes
c	setactiveset
c	header
c	axestitles
c	plot	
c

ccc-----   CALL PLOTFN(PLOTFILE)
ccc-----   CALL XRTBEGIN(STATUS)
ccc---  IF (STATUS.NE.1) CALL MABEND('??E - Unable to start up plotting')
!!        CALL SETLANDSCAPE(0)      ! SET TO 'PORTRAIT' !does not help!
ccc-----   CALL DISPLAYAXES(1,1,0)   ! x,y1 and y2 axes displayed 1=yes 0=no
ccc-----   CALL SETACTIVESET(0)      ! 0= no triangles (default), 1= triangles
        YLEN = YLEN*SCAL          !convert from inches to application units.
C       PLOT THE HEADER, ETC; First load all the strings for heading in HEADR.

        NHDR = 1
	if (ntitle .eq. 0) then
            HEADR(NHDR) = MESS(2:)
	    nheadr(nhdr) = index(headr(nhdr),'  ') - 1
	else
	    headr(nhdr) = htitle
	    nheadr(nhdr) = index(headr(nhdr),'  ') - 1
	endif
        NHDR = NHDR + 1

        CALL XVP('INP',FILENAME,ICNT)	!INPUT FILENAME
        FILEMSG='INPUT = '//FILENAME
        HEADR(NHDR) = FILEMSG
	nheadr(nhdr) = index(headr(nhdr),'   ') - 1 
        NHDR = NHDR + 1

        FILEMSG='PLOT  = '//PLOTFILE	!OUTPUT PLOT FILENAME
        HEADR(NHDR) = FILEMSG
	nheadr(nhdr) = index(headr(nhdr),'   ') - 1
        NHDR = NHDR + 1

C-----PLOT THE LABELS
        CALL OTFLABS(INUNIT,HEADR, NHDR, nheadr, *996) 
C
C PLOT AREA PROCESSED, TRANSFORM SIZE, MEAN AND SIGMA

        HEADR(NHDR) = MSG2(2:)
	nheadr(nhdr) = 60
        NHDR = NHDR + 1
        HEADR(NHDR) = MSG3(2:)
	nheadr(nhdr) = 24
        NHDR = NHDR + 1
        HEADR(NHDR) = MSG5(2:)
	nheadr(nhdr) = 24
        NHDR = NHDR + 1
        HEADR(NHDR) = MSG6(2:)
	nheadr(nhdr) = 24
        NHDR = NHDR + 1

C PLOT PARM 'TITLE'
ccc        IF (NTITLE .NE. 0) THEN
ccc          HEADR(NHDR) = HTITLE
ccc	  nheadr(nhdr) = index(headr(nhdr),'  ') - 1
ccc          NHDR = NHDR + 1
ccc        END IF

        NHDR = NHDR-1
ccc------  CALL HEADER(HEADR,NHDR,0) !WRITE HEADR ARRAY TO PLOT FILE.

C       PLOT THE X & Y AXIS LABELS
ccc------  CALL AXESTITLES( HTITX, TITY(2:),90,  ' ', 0)

C-----DOUBLE THE NUMBER OF POINTS IN THE PLOT BUFFER AND INTERPOLATE
        DO J=1,NMID
          PLAT(2*J-1)=POW(J)
        END DO

        PLAT(2)=0.375*POW(1)+0.75*POW(2)-0.125*POW(3)
        PLAT(4)=0.5*(POW(2)+POW(3))
        PLAT(NTOT)=-0.125*POW(NMID-2)+0.75*POW(NMID-1)+0.375*POW(NMID)
        K=NMID-2

        DO J=3,K
          PLAT(2*J)=-0.0625*(POW(J-1)+POW(J+2))+0.5625*(POW(J)+POW(J+1))
        END DO

C-----THERE ARE NOW NTOT POINTS IN THE BUFFER
        VSCL=1.0
C      K=(2*NMID-1)*fmax/0.5        !change number of pts to do if fmax ne .5
        K=NTOT*fmax/0.5               !change number of pts to do if fmax ne .5
        X=0.0
        Y=YLEN
c
C-----MOVE TO (X,Y)
ccc        CALL PLOT(X,Y,3)

        DO J=1,K
	  X = (J-1)*fmax/K          ! use application units.
          Y=VSCL*PLAT(J)
          IF (Y .GT. YLEN) Y=YLEN
          IF (Y .LT. 0.0) Y=0.0
ccc          CALL PLOT(X,Y,2)          ! DRAW TO (X,Y)
        END DO
c
c GNUPLOT default 640x480 plot - increase vertical size if lots of vicar labels
	plotht = 480
	plotwid = 640
	labstep = 0.04
	isize = 10
	psize = 16
	if (nhdr .gt. 16) then
	   tmp = nhdr/16
	   plotht = int(plotht * tmp)
	   labstep = labstep/tmp

	endif

c	print *, 'plotht = ',plotht,' labstep = ',labstep
ccc	CALL PLOT(0.,0.,999)       ! TERMINATES ALL PLOTS
c
c     Now create a gnuplot command file to display plot to screen
c     if desired, create an eps plot for publications
c
ccc	print *, tbl(1:ntbl), plotwid,plotht,htitx(1:ntitx),ylen,fmax,headr(1)(1:nheadr(1))
        open(98,file=plotgpi(1:nplotgpi),status='UNKNOWN',iostat=jj,err=995)
10100 format('# Created by program power')		!#'s are ignored in gnuplot
        write(98,fmt=10100,iostat=jj,err=995)
10105 format('# Gnuplot commands for power plot')
        write(98,fmt=10105,iostat=jj,err=995) 
10110 format('# Data in ',a)
        write(98,fmt=10110,iostat=jj,err=995) tbl(1:ntbl)
10115 format('set term x11 font "ariel,',i2,'" size ',i4,', ',i4)
C size is XX,YY
        write(98,fmt=10115,iostat=jj,err=995) isize,plotwid,plotht
10116 format('set output')				!set output to screen
	write(98,fmt=10116,iostat=jj,err=995)
10120 format('set grid')
       write(98,fmt=10120,iostat=jj,err=995)
10125 format("set ylab 'AMPLITUDE   (DN P-P)'" )
       write(98,fmt=10125,iostat=jj,err=995) 
10130 format("set xlab '",a,"'")
       write(98,fmt=10130,iostat=jj,err=995) htitx(1:ntitx)
10135 format("set yrange [0:",f6.2,"]")
       write(98,fmt=10135,iostat=jj,err=995) ylen
10140 format("set xrange [0.0:",f5.2,"]")
       write(98,fmt=10140,iostat=jj,err=995) fmax
10141 format("set clip points")				!how to deal with points out of range
	write(98,fmt=10142,iostat=jj,err=995)
10142 format("set clip two")				!how to deal with connecting lines out of range
        write(98,fmt=10141,iostat=jj,err=995)
10145 format('set title "',a,'" font "Ariel,',i2,'"')
       write(98,fmt=10145,iostat=jj,err=995) headr(1)(1:nheadr(1)),isize
ccc	print *, "here2"
c

c output labels for only top 60% of plot
	isize2 = isize - 1
	fpos=1.0
	do ii=2,nhdr
		i = ii - 1
		fpos = fpos - labstep
10160 format('set label ',i2,' "',a,'" at graph .40 ,',f5.2,
     1 ' font "ariel,',i2,'" front nopoint tc def')
c	1 ' font "ariel 8" front nopoint tc def')
	write(98,fmt=10160,iostat=jj,err=995) i,headr(ii)(1:nheadr(ii)), fpos, isize2
cc	print 10160, i,headr(ii)(1:nheadr(ii)), fpos
cc10155 format("set label 2 '",a,"' at graph 0.4, 0.90 front nopoint tc def")
cc        write(98,fmt=10155,iostat=jj,err=995) headr(3)
	enddo

C - the actual plot:
c10250 format("plot '",a,"' u 1:2 t 'Frequency' smooth csplines ls 3 w linespoints")
c10250 format("plot '",a,"' u 1:2 t '' smooth frequency ls 3 w linespoints")
10250 format("plot '",a,"' u 1:2 t '' smooth csplines ls 3 w linespoints")
       write(98,fmt=10250,iostat=jj,err=995) tbl(1:ntbl)
10255 format("pause mouse any")			!allows plot to display on screen until mouse click
       write(98,fmt=10255,iostat=jj,err=995)
	close (98)
c	
c   if desired, create eps output plot
	if (epsplot) then
       open(97,file=plotgpi2(1:nplotgpi2),status='UNKNOWN',iostat=jj,err=995)
C10100 format('# Created by program power')              !#'s are ignored in gnuplot
        write(97,fmt=10100,iostat=jj,err=995)
C10105 format('# Gnuplot commands for power plot')
        write(97,fmt=10105,iostat=jj,err=995)
C10110 format('# Data in ',a)
        write(97,fmt=10110,iostat=jj,err=995) tbl(1:ntbl)
10300 format('set terminal postscript eps enhanced color "Ariel" ',i2,'  size 11 ,8')
        write(97,fmt=10300,iostat=jj,err=995) psize
10305 format("set output '",a,"'")
c	print *,'ploteps = ',ploteps(1:nploteps)
        write(97,fmt=10305,iostat=jj,err=995) ploteps(1:nploteps)
C10120 format('set grid ')
       write(97,fmt=10120,iostat=jj,err=995)
C10125 format("set ylab 'AMPLITUDE   (DN P-P)'" )
       write(97,fmt=10125,iostat=jj,err=995)
C10130 format("set xlab '",a,"'")
       write(97,fmt=10130,iostat=jj,err=995) htitx(1:ntitx)
C10135 format("set yrange [0:",f6.2,"]")
       write(97,fmt=10135,iostat=jj,err=995) ylen
C10140 format("set xrange [0.0:",f5.2,"]")
       write(97,fmt=10140,iostat=jj,err=995) fmax
C10141 format("set clip points")                         !how to deal with points out of range
        write(97,fmt=10142,iostat=jj,err=995)
C10142 format("set clip two")                            !how to deal with connecting lines out of range
        write(97,fmt=10141,iostat=jj,err=995)
C10145 format("set title '",a,"'")
       write(97,fmt=10145,iostat=jj,err=995) headr(1)(1:nheadr(1)), psize

c output labels for only top 60% of plot
        fpos=1.0
          do ii=2,nhdr
                i = ii - 1
                fpos = fpos - labstep
C10160 format('set label ',i2,' "',a,'" at graph .40 ,',f5.2,
C     1 ' font "ariel,9" front nopoint tc def')
c       1 ' font "ariel 8" front nopoint tc def')
        write(97,fmt=10160,iostat=jj,err=995) i,headr(ii)(1:nheadr(ii)), fpos, psize
cc      print 10160, i,headr(ii)(1:nheadr(ii)), fpos
cc10155 format("set label 2 '",a,"' at graph 0.4, 0.90 front nopoint tc def")
cc        write(98,fmt=10155,iostat=jj,err=995) headr(3)
          enddo
	write(97,fmt=10250,iostat=jj,err=995) tbl(1:ntbl)
	endif 	!epsplot

	close(97)
cc      END IF	!IF (PLOTT .EQ. 1) THEN	
C------------------------------------------------------
	CALL XVCLOSE(INUNIT,STATUS,' ')
	RETURN
c   error returns
995	call xvmessage('??E - Error opening/writing gnuplot file',' ')
	call abend
997	call xvmessage('??E - Requested area outside input picture',' ')
	call abend
998	call xvmessage('??E - Error opening/writing table file',' ')
	CALL PRNT(4,1,J,'IOSTAT=.')
	call abend
996	call abend
	END

c *********************************************************************
      SUBROUTINE CMAG(NMID,COMP,POW)
C       has to use CCOMP instead of COMP, otherwise there will be an error 
C       of multiple declaration of names
C       add the results of one line's transform to a cumulative power spectrum
C       NMID  = number of frequencies to be updated 
C       COMP  = input array of data (COMPLEX * 8)
C       POW   = power spectrum array
C       POW(I) = POW(I) + ABS(COMP(I))**2
	implicit none
	integer*4 nmid,i
      real*4 POW(1)
      complex*8 COMP(1)
c-----sum the amplitudes of the complex data
      DO I=1,NMID
        POW(I)=POW(I)+CABS(COMP(I))
c       POW(I)=POW(I)+CABS(COMP(I))**2            wrong
      END DO
      RETURN
      END

c *************************************************************
	SUBROUTINE OTFLABS(INU,HEADR,ICNT,nheadr,*)
C  originally for otf1 vicar program
c	output vicar labels to plot - max 70 header record
c	add char length for each label
	IMPLICIT INTEGER*4 (A-Z)
	INTEGER*4    LV2(60),LPL(60),ICNT,nheadr(70)
	CHARACTER*72 V1(60),V2(60),PL(60),HT(60)
        CHARACTER*80 HEADR(70)

	CALL LABPROC(INU,V1,NV1,V2,LV2,NV2,PL,LPL,NPP,HT,NH)

	IF (NV1 .GT. 0) THEN
	 DO I=1,NV1
            HEADR(ICNT)=V1(I)
	    nheadr(icnt) = index(HEADR(ICNT),'   ')
            ICNT=ICNT+1
	 END DO

	ELSE IF (NPP .GT. 0) THEN
	 DO I=1,NPP
            HEADR(ICNT)=PL(I)
	    nheadr(icnt) = index(HEADR(ICNT),'   ')
            ICNT=ICNT+1
	 END DO

	ELSE IF (NV2 .GT. 0) THEN
	 DO I=1,NV2
            HEADR(ICNT)=V2(I)
	    nheadr(icnt) = index(HEADR(ICNT),'   ')
            ICNT=ICNT+1
	 END DO

	ELSE
	   call xvmessage('??E - Label processing error',' ')
	   RETURN 1
	END IF

	IF (NH .GT. 0) THEN
	 DO I=1,NH
            HEADR(ICNT)=HT(I)
	    nheadr(icnt) = index(HEADR(ICNT),'   ')
            ICNT=ICNT+1
	 END DO
	END IF

	RETURN
	END

c *********************************************************************
C------------------------------------------------------------------
	SUBROUTINE KVPAIR(IU,PAIR,LNG,NUM)
C-------EXTRACTS KEYWORD=VALUE STRINGS FROM FIRST PROPERTY LABEL
	IMPLICIT INTEGER*4 (A-Z)
	INTEGER*4 LNG(60)
	character*72 pair(60)
	character*2048 buf
	character*2048 UPBUF   !UPPER case version of data in buf.  Label
                               !info is mixed case under UNIX.  This way
                               !we don't have to test for PROPERTY or Property.

	max=2048
        buf = ' '              ! This blanks all of buf.
	call xlgetlabel(iu,buf,max,ist)
        UPBUF = buf            !copy data
        call UPRCASE(UPBUF)  !convert to upper case for case insensitive tests
	i=0
	b = index(UPBUF(1:),'PROPERTY')
	IF (B .EQ. 0) GO TO 100

50	L=INDEX(UPBUF(B+1:),' ')
	L=B+L
	Q=INDEX(UPBUF(B+1:),'''')
	Q=B+Q
	IF(Q .LT. L) THEN		!ARE THERE QUOTES IN THE VALUE?
		Q2=INDEX(UPBUF(Q+1:),'''')
		Q2=Q+Q2
		L=INDEX(UPBUF(Q2+1:),' ')
		L=Q2+L
	END IF
	E=L-1
C-------STOP WHEN REACH A 'TASK' OR NEXT PROPERTY LABEL
	IF (UPBUF(B:B+3) .EQ. 'TASK') GO TO 100
	IF (I .GT. 1 .AND. UPBUF(B:B+3).EQ. 'PROP') GO TO 100
	I=I+1
	KK=E-B+1
	LNG(I) = KK			!STORE LENGTH
	PAIR(I) = BUF(B:E)        !STORE STRING (mixed case).
	IF (E .EQ. MAX) GO TO 100        !END OF LABEL?
	DO J=E+1,MAX			!LOOK FOR NEXT STRING
	B=J
	IF (BUF(J:J) .NE. ' ') GO TO 50
	END DO
100	NUM=I
	RETURN
	END
c ************************************************************
	subroutine V2PAIR(IU,PAIR,LNG,NUM)
C-------RETURN THE KEYWORD=VALUE PAIRS FROM THE FIRST V2 TASK
	IMPLICIT INTEGER*4 (A-Z)
	INTEGER*4 LNG(60)
	character*72 pair(60)
	character*2048 buf
	character*2048 UPBUF   !UPPER case version of data in buf.  Label
                               !info is mixed case under UNIX.  This way
                               !we don't have to test for PROPERTY or Property.

	max=2048
        buf = ' '              ! This blanks all of buf.
	call xlgetlabel(iu,buf,max,ist)
        upbuf = buf            !copy data
        call uprcase(upbuf)  !convert to upper case for case insensitive tests

	i=0
	IF (index(UPBUF(1:),'PROPERTY') .GT. 0) GO TO 100
	B = index(UPBUF(1:),'TASK')
	IF (B .EQ. 0) go to 100

50	L=INDEX(UPBUF(B+1:),' ')
	L=B+L
	Q=INDEX(UPBUF(B+1:),'''')
	Q=B+Q
	IF(Q .LT. L) THEN
		Q2=INDEX(UPBUF(Q+1:),'''')
		Q2=Q+Q2
		L=INDEX(UPBUF(Q2+1:),' ')
		L=Q2+L
	END IF
	E=L-1
C-------STOP WHEN ENCOUNTER THE SECOND TASK
	IF (I .GT. 1 .AND. UPBUF(B:B+3) .EQ. 'TASK') GO TO 100
	I=I+1
	KK=E-B+1
	LNG(I) = KK			!STORE LENGTH
	PAIR(I) = BUF(B:E)	!STORE STRING  (mixed case)
	IF (E .EQ. MAX) GO TO 100	!END OF LABEL?
	DO J=E+1,MAX			!LOOK FOR NEXT STRING
	B=J
	IF (BUF(J:J) .NE. ' ') GO TO 50
	END DO
100	NUM=I
	RETURN
	END
c *********************************************************************
      SUBROUTINE LABPROC(IUNI,LABV1,NV1,VPAIR,LV2,NV2,PPAIR,LPL,
     1                   NPP,HSTRY,NH)
      IMPLICIT INTEGER(A-Z)
      INTEGER*4 INSTANCES(20),LV2(60),LPL(60),STAT
      CHARACTER*8 TASKS(20)
      CHARACTER*12 UNAME
      CHARACTER*28 TIME
      CHARACTER*72 HBUF,BL
      CHARACTER*72 LABV1(60),HSTRY(60),VPAIR(60),PPAIR(60)
      CHARACTER*4320 LABS

c--------------------------------------------------------------
c-------get all vicar1 labels
	NV1=60
        CALL VIC1LAB(IUNI,STAT,NV1,LABS,60)
	DO J=1,NV1
           LABV1(J) = LABS(72*(J-1)+1:72*J)  !BREAK INTO PIECES 72 CHARS LONG.
	END DO

c----------------------------------------------------------------
c-------get keyword=value pairs from vicar property labels
	NPP=0
	IF (NV1 .EQ. 0) call kvpair(IUNI,ppair,LPL,npp)

c----------------------------------------------------------------
c-------get keyword=value pairs from first vicar2 task
	NV2=0
	IF (NV1 .EQ. 0 .AND. NPP .EQ. 0) 
     1            call v2pair(IUNI,vpair,LV2,nv2)

c----------------------------------------------------------------

c-------get user and date for each vicar2 history task
        BL(1:36) = '------------------------------------'
        BL(37:72) = '------------------------------------'
	BL(1:1) = ' '
	BL(6:10) = 'TASK:'
	BL(23:27) = 'USER:'

	nh=20                             !EXTRACT VIC*2 LAB
	CALL XLHINFO(IUNI,TASKS,INSTANCES,NH,STAT,' ')

	DO 801 J=1,NH
         LD = 0
         LU = 0
	CALL XLGET(IUNI,'HISTORY','USER',UNAME,STAT,'HIST',TASKS(J),
     *            'INSTANCE',INSTANCES(J),'LENGTH',LU,
     *            'FORMAT','STRING','ULEN',12,' ')
         IF (STAT .NE. 1) 
     *     CALL XVMESSAGE('Warning: USER label item not found',' ')
        CALL XLGET(IUNI,'HISTORY','DAT_TIM',TIME,STAT,'HIST',TASKS(J),
     *            'INSTANCE',INSTANCES(J),'LENGTH',LD,
     *            'FORMAT','STRING','ULEN',28,' ')
         IF (STAT .NE. 1) 
     *     CALL XVMESSAGE('??W - DAT_TIM label item not found',' ')

	HBUF(1:72) = BL(1:72)
	HBUF(11:18) = TASKS(J)
	if (LU .GT. 0) HBUF(28:28+LU-1) = UNAME(1:LU)
	IF (LD .GT. 0) HBUF(40:40+LD-1) = TIME(1:LD)
801	HSTRY(J) = HBUF

      RETURN
      END
