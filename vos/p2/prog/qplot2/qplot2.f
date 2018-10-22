      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C     PROGRAM QPLOT2
C     10 JUL 95   ...CRS (CRI) MST S/W CONVERSION (VICAR PORTING)
C     22 AUG 85   ...JHR...    CONVERTED TO VICAR2, RENAMED QPLOT2
C     22 APR 82   ...JHR...    INITIAL RELEASE
C      E,QPLOT2,IN,*,,PARAMS
C     THIS PROGRAM PLOTS LINES OF DN VS RELATIVE SAMPLE NUMBER.
C     A MAXIMUM OF 10 LINES MAY BE PLOTTED ON THE GRAPH
C     A MAXIMUM OF 10 DATA SETS MAY BE USED
C     ANY LINE DIRECTION MAY BE SPECIFIED
C     IF THE LINE DIRECTION IS NOT HORIZONTAL OR VERTICAL
C     THE OUTPUT SAMPLE POINTS ARE SPACED THE SAME AS THE X AND Y
C     AXES, I.E. IF THE LINE DIRECTION IS 45 DEGREES THE NUMBER OF
C     OUTPUT SAMPLES WILL BE THE SQUARE ROOT OF 2 TIMES THE NUMBER
C     OF INPUT SAMPLES
C
C      * PROCESS IN,SL,SS,EL,ES     SPECIFIES THE INPUT NUMBER,
C          STARTING LINE, STARTING SAMPLE, ENDING LINE, AND
C          ENDING SAMPLE.
C
C
	implicit none
      EXTERNAL EQUIV
      COMMON/C1/ SIZE,displace,RDS,XMIN,XMAX,YMIN,YMAX
     &          ,XSCLMN,XSCLMX,YSCLMN,YSCLMX,XSCLDT
     &          ,YSCLDT,XLNGTH,YLNGTH,FORMAT,NORM,NCHAN
     &          ,xsclset,ysclset
      COMMON/C2/ SL,SS,EL,ES,IN,UNIT,ILINE,NLINES
     &          ,NLI,NSI,NSCHAN,GTYPE,XPAGE,LB,LABTOP
      common/files/filename
      common/commonheader/headermsg,nheadermsg,iiline,i2line

      	integer*4    iiline,i2line,nheadermsg(220)  !! index into header strings
      	INTEGER*4 IN(10),SL(10),SS(10),EL(10),ES(10),UNIT(10)
      	INTEGER*4 GTYPE,TTLTOP,NLI(10),NSI(10),NBI(10)
	integer*4 STAT,IPARM(256),TICS
	integer*4 i,ii,j,jj,n,icount,idef,iline,ind,isize,psize
	integer*4 labtop,lcheck,lx,ly,lb,ni,nlines,np,nschan,ntest
	integer*4 ntics,ntitle,ntitx,ntity,nx,ny,nchan,naline
	integer*4 plotwid,plotht,ntbl,nplotgpi,nplotout
	integer*4 nplotgpi2,nploteps,ntmptbl,charsize,charsteps
        integer*4 pttype(20),lntype(20),ptcolorl(20)

      	REAL*4 RPARM(256),XAXIS(4),YAXIS(4)
      	REAL*4 XMAX(10),XMIN(10),YMAX(10),YMIN(10)
      	REAL*4 XSCLMN,XSCLMX,YSCLMN,YSCLMX,XLNGTH,YLNGTH
	real*4 displace,rds,size,xpage,xscldt,yscldt
	logical*4 XVPTST, NORM, xsclset, ysclset, epsplot, nolabel
        character*1 LPARM(1024)
	character*4 FORMAT(10),aline
        character*8 plotfmt
        character*24 tbl,tmptbl
	character*30 alinenum

      	CHARACTER*63 XTTL,YTTL,TTL,CBUF,XTITLE,YTITLE,TITLE
      	character*63 msg,plotgpi,plotgpi2,ploteps
      	character*56 headermsg(220) !! Labels * (lines per label+2) 
	CHARACTER*63 plotout
	character*120 filename(10)
c
        character*8 ptcolor(20),lncolor(20)
	character*4 gpi/'.gpi'/,eps/'.eps'/,asc/'.asc'/
c
        character*1 num(5)

        character bash
c
        data num/'1','2','3','4','5'/
        data tmptbl/'tmptbl.'/
	data aline/'line'/
C
        data pttype/ 5, 9, 7,13,11, 1, 2, 3, 5, 9, 7,13,11, 1, 2, 3, 5, 9, 7,13/
        data lntype/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1/
        data ptcolor/'green','purple','magenta','blue','brown',
     1 'red','cyan','orange','green','purple',
     2 'magenta','blue','brown','red','cyan',
     3 'orange','green','purple','magenta','blue'/
        data ptcolorl/5,6,7,4,5, 3,4,6,5,6, 7,4,5,3,4, 6,5,6,7,4/
        data lncolor/'beige','red','green','cyan','purple',
     1 'blue','orange','magenta','beige','red',
     2 'green','cyan','purple','blue','orange',
     3 'magenta','beige','red','green','cyan'/
c
      call xvmessage('qplot2 version 2015-08-19',' ')
      bash=achar(92)
C
C   SET DEFAULTS AND INITIALIZE
c	tbl='tmptbl.x'
c	ntbl=index(tbl,'   ') - 1
      YTITLE = 'DN VALUE'
      XTITLE = 'RELATIVE SAMPLE NUMBER'
      TITLE  = 'IPL LINE PLOT'
C        'PLOTNAME'
	epsplot=.false.
        nplotgpi = 0
        nplotgpi2 = 0
        nplotout = 0
	nploteps = 0
	ntbl = 0

        epsplot = .false.
        CALL XVPARM ('PLOTFMT',plotfmt,icount,idef,1)
            if (plotfmt .eq. 'EPS' .or. plotfmt .eq. 'eps') epsplot = .true.


      PLOTOUT= 'qplot.eps'
      nplotout=index(plotout,'   ') - 1
      plotgpi= 'qplot.gpi'
      nplotgpi=index(plotgpi,'   ') - 1
      plotgpi2= 'qplot.eps.gpi'
      nplotgpi2=index(plotgpi2,'   ') - 1
      tbl='qplot.asc'
      ntbl = index(tbl,'  ') - 1
      CALL XVPARM('PLOTOUT',cbuf,ICOUNT,IDEF,1)
        IF (IDEF .EQ. 0) THEN
            if (cbuf .eq. "YES" .or. cbuf .eq."yes") then
c               epsplot = .true.
                plotout='qplot'
                nplotout=index(plotout,'   ') - 1
                plotgpi=plotout(1:nplotout)//gpi
                nplotgpi=index(plotgpi,'  ') - 1
                plotgpi2=plotout(1:nplotout)//eps//gpi
                nplotgpi2=index(plotgpi2,'  ') - 1
		ploteps=plotout(1:nplotout)//eps
                nploteps=index(ploteps,'  ') - 1
                tbl = plotout(1:nplotout)//asc
                ntbl = index(tbl,'  ') - 1
	        tmptbl = tbl(1:ntbl) 

c               Plotout and nplotout from above
            elseif (cbuf .eq. "NONE" .or. cbuf .eq."none") then
c               epsplot = .false.
               plotgpi='qplot.gpi'
               nplotgpi=index(plotgpi,'  ') - 1
            else
               plotout = CBUF
               nplotout=index(plotout,'   ') - 1
               plotgpi=plotout(1:nplotout)//gpi
               nplotgpi=index(plotgpi,'  ') - 1
               plotgpi2=plotout(1:nplotout)//eps//gpi
               nplotgpi2=index(plotgpi2,'  ') - 1
	       ploteps=plotout(1:nplotout)//eps
	       nploteps=index(ploteps,'  ') - 1
                tbl = plotout(1:nplotout)//asc
                ntbl = index(tbl,'  ') - 1
		tmptbl = tbl(1:ntbl)
c               epsplot = .true.
            endif
      ELSE
c            epsplot = .false.
            plotgpi='qplot.gpi'
            nplotgpi=index(plotgpi,'  ') - 1
	    tbl = plotout(1:nplotout)//asc
            ntbl = index(tbl,'  ') - 1
      END IF

      GTYPE=0				!graph type: 1=PROCESS 2=SPROCESS
      NCHAN=1				!number of channels (bands in MSS data)
					! 5 bands
      SIZE=.10
	isize = 10		!gnuplot file
	psize = 16		!eps file
      displace=0.
      RDS=0.
      NTITX=22
      NTITY=8
      NTITLE=13
      NORM=.FALSE.
	nolabel=.false.	!Put vicar labels on graph
      TICS=1			!set default to tics
      LABTOP=1
      TTLTOP=1
      XLNGTH=9.0
      YLNGTH=7.0
      XSCLMN=1.
      XSCLMX=1.
      YSCLMN=0.
      YSCLMX=0.
	xsclset = .false.
	ysclset = .false.
      TTL='IPL LINE PLOT'
      XTTL='RELATIVE SAMPLE NUMBER'
      YTTL='DN VALUE'
      DO 5 J=1,10
        XMIN(J)=0.
        XMAX(J)=0.
        YMIN(J)=0.
        YMAX(J)=255.
    5 CONTINUE
      XPAGE=0.5
      iiline = 1
      i2line = 0
C
C        OPEN INPUT DATA SETS
C
      CALL XVP('INP',LPARM,NI)  		!max = 10
      DO 10 I=1,NI
         CALL XVUNIT(UNIT(I),'INP',I,STAT,' ')
         CALL XVOPEN(UNIT(I),STAT,'U_FORMAT','REAL',' ')
         CALL XVGET(UNIT(I),STAT,'NL',NLI(I),'NS',NSI(I),
     &           'FORMAT',FORMAT(I),'NB',NBI(I),' ')
c         IF (FORMAT(I) .EQ. 'HALF') HALF(I)=1
c	print *, 'Number of bands = ',nbi(i)
	if (nbi(i) .gt. 1) then 
	    call xvmessage("??E - Multiband images not supported"," ")
	    call xvmessage("      Convert to MSS format with TRAN"," ")
	    call abend
	endif
   10 CONTINUE
c
      CALL XVP('INP',FILENAME,ICOUNT)   !INPUT FILENAMES
C
C        *** PROCESS PARAMETERS ***
C
C  'NCHAN'
      CALL XVPARM('NCHAN',NCHAN,ICOUNT,IDEF,1)
      NSCHAN=NSI(1)/NCHAN
c	print *,"nchan = ",nchan
C  'PROCESS' - profile plot for
      CALL XVPARM('PROCESS',IPARM,ICOUNT,IDEF,50)
c   5 numbers, DataSetNum   SL,SS,EL,ES    
      IF (ICOUNT .NE. 0) THEN
         GTYPE=1
         NLINES=ICOUNT/5
         IF (5*NLINES .NE. ICOUNT) THEN
            CALL XVMESSAGE('??E - Invalid count for parameter "PROCESS"',' ')
            CALL ABEND
         END IF
         DO I=1,NLINES
            IN(I)=IPARM(5*(I-1)+1)
            SL(I)=IPARM(5*(I-1)+2)
            SS(I)=IPARM(5*(I-1)+3)
            EL(I)=IPARM(5*(I-1)+4)
            ES(I)=IPARM(5*(I-1)+5)
            IF (IN(I) .LT. 1 .OR. IN(I) .GT. NI) THEN
                call xvmessage ('??E - Invalid input number specified',' ')
		call abend
            ENDIF
            IF (SL(I) .LT. 1) CALL MABEND('??E - invalid starting line')
            IF (SS(I) .LT. 1) CALL MABEND('??E - Invalid starting sample')
            IF (EL(I) .GT. NLI(IN(I))) CALL MABEND('??E - invalid ending line')
            IF (ES(I) .GT. NSI(IN(I)))CALL MABEND('??E - invalid ending sample')
	    IF (SL(I) .EQ. EL(I) .AND. SS(I) .EQ. ES(I)) then
       	       call mabend('??E - null line segment specified')
            endif
            if (format(IN(I)) .EQ. 'HALF') YMAX(I)=32767
	    if (format(IN(i)) .EQ. 'FULL') YMAX(i)=65536
            if (format(IN(i)) .EQ. 'REAL') YMAX(i)=65536.
         END DO
      END IF
C  'SPROCESS' - Spectral Plots
      CALL XVPARM('SPROCESS',IPARM,ICOUNT,IDEF,20)
c	print *,"sprocess icount = ",icount," idef = ",idef
      IF (ICOUNT .NE. 0) THEN
         IF (GTYPE .NE. 0) THEN
            CALL XVMESSAGE
     &        ('??E - Cannot specify both PROCESS and SPROCESS',' ')
            CALL ABEND
         END IF
         IF (NI .NE. 1) THEN
            CALL XVMESSAGE
     &         ('??E - Spectral plots require 1 input in MSS format',' ')
            CALL ABEND
         END IF
         IF (NCHAN .EQ. 1) THEN
            CALL XVMESSAGE('??E - Must specify nchan for spectral plots',' ')
            CALL ABEND
         END IF
         GTYPE=2
         NLINES=ICOUNT/2
         IF (2*NLINES .NE. ICOUNT) THEN
            CALL XVMESSAGE('??E - invalid count for parameter "SPROCESS"',' ')
            CALL ABEND
         END IF
         DO I=1,NLINES
            IN(I)=1
            SL(I)=IPARM(2*(I-1)+1)
            SS(I)=IPARM(2*(I-1)+2)
c	print *, "sl,ss = ",sl(i),ss(i)
         END DO
	TITLE = 'IPL SPECTRAL PLOT' 
c        NTITLE=17 - change to automatically compute string length if TITLE were to change
	ntitle = index(title,'  ') - 1
	XTITLE = 'CHANNEL NUMBER'
c         NTITX=14
	ntitx = index(xtitle,'  ') - 1
c         IF (FORMAT(1) .EQ. 'HALF') YMAX(1)=32767

      END IF	!IF (ICOUNT .NE. 0)
C  'LABELSIZ'
      CALL XVPARM('LABELSIZ',ISIZE,ICOUNT,IDEF,1)	!font in points
c	print *, 'size = ',isize	
C  'LOLABEL'
      IF (XVPTST('LOLABEL')) LABTOP=0
c  'Nolabel'
	if (XVPTST('NOLABEL')) nolabel=.true.
C  'TICS'
      IF (XVPTST('NOTICS')) TICS=0
C  'DISPLACEMENT'
      CALL XVPARM('DISPLACE',displace,ICOUNT,IDEF,1)

        plotwid =  648  !640 @72dpi = 8.888.. inches    9 inch = 648
        plotht  =  504  !480 @72dpi = 6.666.. inches    7 inch = 504
C  'XLENGTH'
      CALL XVPARM('XLENGTH',XLNGTH,ICOUNT,IDEF,1)
c	if idef = 1 then default used
	if (idef.eq.0) then
		plotwid = 72 * xlngth 
	endif
C  'YLENGTH'
      CALL XVPARM('YLENGTH',YLNGTH,ICOUNT,IDEF,1)
        if (idef.eq.0) then
                plotht = 72 * ylngth
        endif

C  'XSCALE'
      CALL XVPARM('XSCALE',RPARM,ICOUNT,IDEF,2)
      IF(ICOUNT .EQ. 2) THEN
         XSCLMN=RPARM(1)
         XSCLMX=RPARM(2)
	 xsclset = .true.
      ENDIF
C  'YSCALE'
      CALL XVPARM('YSCALE',RPARM,ICOUNT,IDEF,2)
      IF(ICOUNT .EQ. 2) THEN
         YSCLMN=RPARM(1)
         YSCLMX=RPARM(2)
	 ysclset = .true.
      ENDIF
C  'XVALUES'
      CALL XVPARM('XVALUES',RPARM,ICOUNT,IDEF,20)
      IF(ICOUNT .GE. 2) THEN
         N=ICOUNT/2
         IF (2*N.NE.ICOUNT) THEN
            CALL XVMESSAGE('??E - invalid count for parameter "XVALUES"',' ')
            CALL ABEND
         END IF
         DO I=1,N
            XMIN(I)=RPARM(2*(I-1)+1)
            XMAX(I)=RPARM(2*(I-1)+2)
         END DO
      ENDIF
C  'YVALUES'
      CALL XVPARM('YVALUES',RPARM,ICOUNT,IDEF,20)
      IF(ICOUNT .GE. 2) THEN
         N=ICOUNT/2
         IF (2*N .NE. ICOUNT) THEN
            CALL XVMESSAGE('??E - Invalid count for parameter "YVALUES"',' ')
            CALL ABEND
         END IF
         DO I=1,N
            YMIN(I)=RPARM(2*(I-1)+1)
            YMAX(I)=RPARM(2*(I-1)+2)
         END DO
      ENDIF
C  'LOTITLE'
      IF (XVPTST('LOTITLE')) TTLTOP=0
C  'NORM'
      NORM = XVPTST('NORM')
      IF (NORM) YLNGTH=5.
      IF (NORM) YSCLMX=1.
C  'RDS'
      CALL XVPARM('RDS',RDS,ICOUNT,IDEF,1)
C  'XTITLE'
      CALL XVPARM('XTITLE',CBUF,ICOUNT,IDEF,1)
      IF (CBUF .NE. XTTL) THEN
	 XTITLE = ' '
	 WRITE(XTITLE(1:),'(A)') CBUF
         NTITX=INDEX(CBUF,'   ') - 1
         IF (NTITX .LE. 0) NTITX=52
      END IF
C  'YTITLE'
      CALL XVPARM('YTITLE',CBUF,ICOUNT,IDEF,1)
      IF (CBUF .NE. YTTL) THEN
	 YTITLE = ' '
	 WRITE(YTITLE(1:),'(A)') CBUF
         NTITY=INDEX(CBUF,'   ') - 1
         IF (NTITY .LE. 0) NTITY=52
      END IF
C  'TITLE'
      CALL XVPARM('TITLE',CBUF,ICOUNT,IDEF,1)
      IF (CBUF .NE. TTL) THEN
	 TITLE = ' '
	 WRITE(TITLE(1:),'(A)') CBUF
         NTITLE=INDEX(CBUF,'   ') - 1
         IF (NTITLE .LE. 0) NTITLE=52
      END IF
C
C  FIND LENGTH OF LONGEST LINE
      NP=0
      IF (GTYPE .EQ. 1) THEN			!PROCESS
c         NP=0
         DO J=1,NLINES
            NX=IABS(SL(J)-EL(J))
            NY=IABS(SS(J)-ES(J))
            NTEST=SQRT(FLOAT(NX*NX+NY*NY))+1
            IF (NTEST .GT. NP) NP=NTEST
         ENDDO
      ENDIF
c	print *, "np = ",np
      IF (GTYPE .EQ. 2) NP=NCHAN		!SPROCESS
C
C   LX IS NUMBER OF BYTES NEEDED FOR X ARRAY.
C    (ONE FULLWORD FOR EACH PT. PLUS TWO MORE FOR XSCLMN AND XSCLDT)
c	used for stacka
      LX=4*(NP+2)
      LY=LX
      LCHECK=LX    	!check of bytes
C
C  DRAW X AXIS
      GOTO 230
      XSCLDT=(XSCLMX-XSCLMN)/XLNGTH
      IF (XSCLDT .NE. 0.) GO TO 230
      XAXIS(1)=XMIN(1)
      XAXIS(2)=XMAX(1)
      DO J=1,NLINES
         XAXIS(1)=AMIN1(XAXIS(1),XMIN(J))
         XAXIS(2)=AMAX1(XAXIS(2),XMAX(J))
      END DO
      IF (XAXIS(1) .GE. XAXIS(2)) XAXIS(2)=NP
ccc---      CALL SCALE(XAXIS,XLNGTH,2,1)
      XAXIS(4) = XSCLDT
      XAXIS(3) = XSCLMN
c -- the following is not really needed with gnuplot
230   continue
      IF (TICS .EQ. 1) THEN
C  SMALL
         NTICS=10*XLNGTH
         NTICS=2*XLNGTH
      END IF
C
C  DRAW Y AXIS
      GOTO 330
      YSCLDT=(YSCLMX-YSCLMN)/YLNGTH
      IF (YSCLDT .NE. 0) GO TO 330
      YAXIS(1)=YMIN(1)
      YAXIS(2)=YMAX(1)
      DO J=1,NLINES
         YAXIS(1)=AMIN1(YAXIS(1),YMIN(J))
         YAXIS(2)=AMAX1(YAXIS(2),YMAX(J))
      END DO
ccc---      CALL SCALE(YAXIS,YLNGTH,2,1)
      YAXIS(3) = YSCLMN
      YAXIS(4) = YSCLDT
      YSCLMX=YSCLMN+YLNGTH*YSCLDT
c -- the following is not really needed with gnuplot
330   Continue
      IF (TICS .EQ. 1) THEN
C           SMALL
         NTICS=10*YLNGTH
         NTICS=2*YLNGTH
      END IF
C
C  DRAW TITLE  (DEFAULT = 'IPL LINE PLOT')
      headermsg(iiline) = title
      iiline = iiline + 1		!+ 3

c -- the following is not really needed with gnuplot
c  here is where "line" is called
c      labels (1) = ' '
c      do II = 1, 10
c         write (msg (1:),'(a)') 'Line   ' 
c         write (msg (6:),'(i2)') II
c         write (msg (9:50),'(a)') filename(ii)(1:40) 
c         labels (II+1) = msg
c	print *,'label (ii+1) = ', labels (II+1)
c      end do


c	print *,'before DO 850 ILINE=1,NLINES  tbl = ',tbl(1:ntbl)
C
      DO 850 ILINE=1,NLINES
C  SET LB=1 IF DATA SET IS SAME AS PREVIOUS ONE
         LB=0
         IF (ILINE .GT. 1) THEN
            IF (IN(ILINE) .EQ. IN(ILINE-1)) LB=1
         END IF
         if (iline .eq. 6) then
            i2line = iiline
            headermsg(iiline) = title
            iiline = iiline + 3
         endif
C
C  ENSURE X ARRAY IS LARGE ENOUGH TO USE AS INPUT BUFFER ALSO
         IF (LX .LT. 4*NSI(IN(ILINE))) LX=4*NSI(IN(ILINE))
C

C CALL SUBROUTINE GRAPH VIA STACKA AND EQUIV
c
c	print *, 'before CALL STACKA(9,EQUIV,.... tmptbl = ',tmptbl(1:ntbl)    
         CALL STACKA(9,EQUIV,2,LX,LY,LCHECK,iline,IND,tmptbl,ntbl)
         IF (IND .EQ. 1) GO TO 999
	
c	print *, 'after CALL STACKA(9,EQUIV,.... tmptbl = ',tmptbl(1:ntbl)
  850 CONTINUE
c        print *, "y-",ysclmn, ysclmx
c        print *, "x-",xsclmn, xsclmx
c This calculation is used for positioning the labels on the chart
c	original method was percentage of height in fpos
cc        labstep = 0.04
	iiline = iiline - 2
cc	go to 10000
c c       if (iiline .gt. 16) then
cc           tmp = iiline/16
cc           plotht = int(plotht * 0.75*tmp)
cc           labstep =(labstep/tmp)
cc        endif
c	compute y-scale height
cc	tmp = ysclmx - ysclmn
cc	ysclmx = ysclmx + 50*labstep*ysclmx
cc	if ((ysclmx-ysclmn) .gt. 2*tmp) ysclmx = 2*tmp	

cc10000 continue
	charsize = 9
	charsteps = (plotht)/(charsize*2) + 4		!divide by 2 for line spacing
	if (charsteps .gt. 54) charsteps = charsteps - 1	!adjust for floating point
c	print *, 'charsteps = ',charsteps

	if (iiline .gt. (charsteps - 5).and. .not.nolabel) then
	   write (msg,10010) 
10010 format ('Plot needs to be taller for all labels to print' )
	   call xvmessage(msg,' ')
	endif 
cc
cc  open gpi data set
cc
        open(98,file=plotgpi(1:nplotgpi),status='UNKNOWN',iostat=jj,err=995)
10100 format('# Created by program qplot2')              !#'s are ignored in gnuplot
        write(98,fmt=10100,iostat=jj,err=995)
10105 format('# Gnuplot commands for line plot(s)')
        write(98,fmt=10105,iostat=jj,err=995)
10110 format('# Data in ',a)
        write(98,fmt=10110,iostat=jj,err=995) tbl(1:ntbl)
10115 format('set term x11 font "ariel,',i2,'" size ',i4,', ',i4)
C  size = XX,YY
        write(98,fmt=10115,iostat=jj,err=995) isize,plotwid,plotht
10116 format('set output')                              !set output to screen
        write(98,fmt=10116,iostat=jj,err=995)
        if (tics .eq. 1) then
10120 format('set grid ')
                write(98,fmt=10120,iostat=jj,err=995)
	else
10121 format ("set noxtics")
	        write(98,fmt=10121,iostat=jj,err=995)
10122 format ("set noytics")
	        write(98,fmt=10122,iostat=jj,err=995)
        endif
10125 format("set ylab '",a,"'" )
       write(98,fmt=10125,iostat=jj,err=995) ytitle(1:ntity)
10130 format("set xlab '",a,"'")
       write(98,fmt=10130,iostat=jj,err=995) xtitle(1:ntitx)
10141 format("set clip points")                         !how to deal with points out of range
        write(98,fmt=10142,iostat=jj,err=995)
10142 format("set clip one")                            !how to deal with connecting lines out of range
        write(98,fmt=10141,iostat=jj,err=995)
10145 format('set title "',a,'" font "Ariel,',i2,'"')
       write(98,fmt=10145,iostat=jj,err=995) title(1:ntitle),isize

10135 format("set yrange [",f8.0,":",f8.0,"]")
       write(98,fmt=10135,iostat=jj,err=995) ysclmn,ysclmx
10140 format("set xrange [",f8.0,":",f7.0,"]")
       write(98,fmt=10140,iostat=jj,err=995) xsclmn,xsclmx

cc	go to 11000
c output labels for only top 60% of plot
cc        fpos=1.0 !			+ labstep
cc        do ii=2,iiline
cc                i = ii - 1
cc                fpos = fpos - labstep
cc10160 format('set label ',i2,' "',a,'" at graph .30 ,',f5.2,
cc     1 ' font "ariel,9" front nopoint tc def')
c       1 ' font "ariel 8" front nopoint tc def')
cc        write(98,fmt=10160,iostat=jj,err=995) i,headermsg(ii)(1:nheadermsg(ii)), fpos
cc      print 10160, i,headr(ii)(1:nheadr(ii)), fpos
cc10155 format("set label 2 '",a,"' at graph 0.4, 0.90 front nopoint tc def")
cc        write(98,fmt=10155,iostat=jj,err=995) headr(3)
cc        enddo

cc11000 continue
	if (.not.nolabel) then
	do ii=2,iiline
	   i = ii - 1
           j = charsteps - ii  	       
10170 format('set label ',i2,' "',a,'" at character 15 ,',i2,
     1 ' font "ariel,9" front nopoint tc def')
c       1 ' font "ariel 8" front nopoint tc def')
        write(98,fmt=10170,iostat=jj,err=995) i,headermsg(ii)(1:nheadermsg(ii)), j

	enddo	
      !! Display labels on the 2nd and possibly the 3rd page 
      if (i2line .eq. 0) then
         !! If i2line == 0, then 5 or less samples
ccc---         call header (headermsg, iiline, 0) !! Title string, lines, adjust left
      else
         !! Display first set of labels and header
      endif
	
	endif !if (.not.nlabel

        if (nlines .eq. 1) then
	   iline=1
           ntmptbl=index(tmptbl,'  ') - 1
           tbl=tmptbl(1:ntmptbl)//num(iline)
           ntbl=index(tbl,'  ') - 1
c	print *, 'if (nlines .eq. 1) tbl = ',tbl(1:ntbl)
	   if (gtype .eq. 1) then
           	alinenum=aline//num(iline)
	        naline=index(alinenum,'  ') - 1
	   else
		write (alinenum,10248) sl(iline),ss(iline)
10248 format("pixel[",i4,",",i4,"]") 
		naline=index(alinenum,'     ') - 1
	   endif


10250 format("plot '",a,"' u 1:2 t '",a,"' w linespoints lt ",i2,
     1 " pt ",i2," ps 2 lc rgb '",a,"'")
        write(98,fmt=10250,iostat=jj,err=995) tbl(1:ntbl),alinenum(1:naline),
     1 lntype(iline),pttype(iline),ptcolor(iline)(1:ptcolorl(iline))


        elseif (nlines .eq. 2) then

           iline = 1
	   ntmptbl=index(tmptbl,'  ') - 1
           tbl=tmptbl(1:ntmptbl)//num(iline)
           ntbl=index(tbl,'  ') - 1
c	print *, 'if (nlines .eq. 2) tbl = ',tbl(1:ntbl)
	   if (gtype .eq. 1) then
                alinenum=aline//num(iline)
                naline=index(alinenum,'  ') - 1
           else
                write (alinenum,10248) sl(iline),ss(iline)
                naline=index(alinenum,'     ') - 1
           endif
c     terminated with bash
10251 format("plot '",a,"' u 1:2 t '",a,"' w linespoints lt ",i2,
     1 " pt ",i2," ps 2 lc rgb '",a,"', ",a)
      write(98,fmt=10251,iostat=jj,err=995) tbl(1:ntbl),
     1 alinenum(1:naline),
     1 lntype(iline),pttype(iline),ptcolor(iline)(1:ptcolorl(iline)),
     1 bash
           iline = 2
           ntmptbl=index(tmptbl,'  ') - 1
           tbl=tmptbl(1:ntmptbl)//num(iline)
           ntbl=index(tbl,'  ') - 1
c	print *, 'iline .eq. 2 tbl = ',tbl(1:ntbl)
           if (gtype .eq. 1) then
                alinenum=aline//num(iline)
                naline=index(alinenum,'  ') - 1
           else
                write (alinenum,10248) sl(iline),ss(iline)
                naline=index(alinenum,'     ') - 1
	   endif
10252 format (" '",a,"' u 1:2 t '",a,"' w linespoints lt ",i2,
     1 " pt ",i2," ps 2 lc rgb '",a,"'")
        write(98,fmt=10252,iostat=jj,err=995) tbl(1:ntbl),alinenum(1:naline),
     1 lntype(iline),pttype(iline),ptcolor(iline)(1:ptcolorl(iline))

        elseif (nlines .gt. 2) then

           iline = 1
           ntmptbl=index(tmptbl,'  ') - 1
           tbl=tmptbl(1:ntmptbl)//num(iline)
           ntbl=index(tbl,'  ') - 1
c	print *, 'elseif (nlines .gt. 2) tbl = ',tbl(1:ntbl)
           if (gtype .eq. 1) then
                alinenum=aline//num(iline)
                naline=index(alinenum,'  ') - 1
           else
                write (alinenum,10248) sl(iline),ss(iline)
                naline=index(alinenum,'     ') - 1
	   endif
        write(98,fmt=10251,iostat=jj,err=995) tbl(1:ntbl),
     1   alinenum(1:naline),
     1   lntype(iline),pttype(iline),ptcolor(iline)(1:ptcolorl(iline)),
     1   bash

        do iline=2,nlines-1
           ntmptbl=index(tmptbl,'  ') - 1
           tbl=tmptbl(1:ntmptbl)//num(iline)
           ntbl=index(tbl,'  ') - 1
c	  print *, 'do iline=2,nlines-1 ntbl = ',tbl(1:ntbl)
           if (gtype .eq. 1) then
                alinenum=aline//num(iline)
                naline=index(alinenum,'  ') - 1
           else
                write (alinenum,10248) sl(iline),ss(iline)
                naline=index(alinenum,'     ') - 1
	   endif
10253 format (" '",a,"' u 1:2 t '",a,"' w linespoints lt ",i2,
     1 " pt ",i2," ps 2 lc rgb '",a,"', ",a)
      write(98,fmt=10253,iostat=jj,err=995) tbl(1:ntbl),
     1  alinenum(1:naline),
     1  lntype(iline),pttype(iline),ptcolor(iline)(1:ptcolorl(iline)),
     1  bash

        enddo
            iline = nlines
           ntmptbl=index(tmptbl,'  ') - 1
           tbl=tmptbl(1:ntmptbl)//num(iline)
           ntbl=index(tbl,'  ') - 1
c	   print *, 'iline = nlines tbl = ',tbl(1:ntbl)
           if (gtype .eq. 1) then
                alinenum=aline//num(iline)
                naline=index(alinenum,'  ') - 1
           else
                write (alinenum,10248) sl(iline),ss(iline)
                naline=index(alinenum,'     ') - 1
	   endif
        write(98,fmt=10252,iostat=jj,err=995) tbl(1:ntbl),alinenum(1:naline),
     1 lntype(iline),pttype(iline),ptcolor(iline)(1:ptcolorl(iline))

	endif

10255 format("pause mouse any")                 !allows plot to display on screen until mouse click
       write(98,fmt=10255,iostat=jj,err=995)

	close(98)

	if (epsplot) then
cc
cc  open eps data set
cc
        open(97,file=plotgpi2(1:nplotgpi2),status='UNKNOWN',iostat=jj,err=996)
        write(97,fmt=10100,iostat=jj,err=996)
        write(97,fmt=10105,iostat=jj,err=996)
        write(97,fmt=10110,iostat=jj,err=996) tbl(1:ntbl)
10300 format('set terminal postscript eps enhanced "Ariel" ',i2,' size 11 ,8')
        write(97,fmt=10300,iostat=jj,err=996) psize		!	plotwid,plotht
10305 format("set output '",a,"'")
        write(97,fmt=10305,iostat=jj,err=996) ploteps(1:nploteps)
        if (tics .eq. 1) then
                write(97,fmt=10120,iostat=jj,err=995)
        else
                write(97,fmt=10121,iostat=jj,err=995)
                write(97,fmt=10122,iostat=jj,err=995)
        endif
       write(97,fmt=10125,iostat=jj,err=996) ytitle(1:ntity)
       write(97,fmt=10130,iostat=jj,err=996) xtitle(1:ntitx)
        write(97,fmt=10142,iostat=jj,err=996)
        write(97,fmt=10141,iostat=jj,err=996)
       write(97,fmt=10145,iostat=jj,err=996) title(1:ntitle),psize
       write(97,fmt=10135,iostat=jj,err=996) ysclmn,ysclmx
       write(97,fmt=10140,iostat=jj,err=996) xsclmn,xsclmx

c output labels for only top 60% of plot
cc        fpos=1.0 + labstep
cc        do ii=2,iiline
cc                i = ii - 1
cc                fpos = fpos - labstep
cc10161 format('set label ',i2,' "',a,'" at graph .30 ,',f5.2,
cc     1 ' font "ariel,16" front nopoint tc def')
c       1 ' font "ariel 8" front nopoint tc def')
cc        write(97,fmt=10161,iostat=jj,err=996) i,headermsg(ii)(1:nheadermsg(ii)), fpos
cc      print 10160, i,headr(ii)(1:nheadr(ii)), fpos
cc10155 format("set label 2 '",a,"' at graph 0.4, 0.90 front nopoint tc def")
cc        write(98,fmt=10155,iostat=jj,err=995) headr(3)
cc        enddo

c
       do ii=2,iiline
           i = ii - 1
           j = charsteps - ii
c       1 ' font "ariel 8" front nopoint tc def')
        write(97,fmt=10170,iostat=jj,err=995) i,headermsg(ii)(1:nheadermsg(ii)), j

        enddo

       if (nlines .eq. 1) then
           iline=1
          ntmptbl=index(tmptbl,'  ') - 1
           tbl=tmptbl(1:ntmptbl)//num(iline)
           ntbl=index(tbl,'  ') - 1
           if (gtype .eq. 1) then
                alinenum=aline//num(iline)
                naline=index(alinenum,'  ') - 1
           else
                write (alinenum,10248) sl(iline),ss(iline)
                naline=index(alinenum,'     ') - 1
           endif
        write(97,fmt=10250,iostat=jj,err=996) tbl(1:ntbl),alinenum(1:naline),
     1 lntype(iline),pttype(iline),ptcolor(iline)(1:ptcolorl(iline))
        elseif (nlines .eq. 2) then

           iline = 1
           ntmptbl=index(tmptbl,'  ') - 1
           tbl=tmptbl(1:ntmptbl)//num(iline)
           ntbl=index(tbl,'  ') - 1
           if (gtype .eq. 1) then
                alinenum=aline//num(iline)
                naline=index(alinenum,'  ') - 1
           else
                write (alinenum,10248) sl(iline),ss(iline)
                naline=index(alinenum,'     ') - 1
           endif
        write(97,fmt=10251,iostat=jj,err=996) tbl(1:ntbl),
     1    alinenum(1:naline),
     1    lntype(iline),pttype(iline),ptcolor(iline)(1:ptcolorl(iline)),
     1    bash
           iline = 2
           ntmptbl=index(tmptbl,'  ') - 1
           tbl=tmptbl(1:ntmptbl)//num(iline)
           ntbl=index(tbl,'  ') - 1
           if (gtype .eq. 1) then
                alinenum=aline//num(iline)
                naline=index(alinenum,'  ') - 1
           else
                write (alinenum,10248) sl(iline),ss(iline)
                naline=index(alinenum,'     ') - 1
           endif
        write(97,fmt=10252,iostat=jj,err=996) tbl(1:ntbl),alinenum(1:naline),
     1 lntype(iline),pttype(iline),ptcolor(iline)(1:ptcolorl(iline))

        elseif (nlines .gt. 2) then

           iline = 1
           ntmptbl=index(tmptbl,'  ') - 1
           tbl=tmptbl(1:ntmptbl)//num(iline)
           ntbl=index(tbl,'  ') - 1
           if (gtype .eq. 1) then
                alinenum=aline//num(iline)
                naline=index(alinenum,'  ') - 1
           else
                write (alinenum,10248) sl(iline),ss(iline)
                naline=index(alinenum,'     ') - 1
           endif
        write(97,fmt=10251,iostat=jj,err=996) tbl(1:ntbl),
     1    alinenum(1:naline),
     1    lntype(iline),pttype(iline),ptcolor(iline)(1:ptcolorl(iline)),
     1    bash

        do iline=2,nlines-1
           ntmptbl=index(tmptbl,'  ') - 1
           tbl=tmptbl(1:ntmptbl)//num(iline)
           ntbl=index(tbl,'  ') - 1
           if (gtype .eq. 1) then
                alinenum=aline//num(iline)
                naline=index(alinenum,'  ') - 1
           else
                write (alinenum,10248) sl(iline),ss(iline)
                naline=index(alinenum,'     ') - 1
           endif
       write(97,fmt=10253,iostat=jj,err=996) tbl(1:ntbl),
     1   alinenum(1:naline),
     1   lntype(iline),pttype(iline),ptcolor(iline)(1:ptcolorl(iline)),
     1   bash

        enddo
            iline = nlines
           ntmptbl=index(tmptbl,'  ') - 1
           tbl=tmptbl(1:ntmptbl)//num(iline)
           ntbl=index(tbl,'  ') - 1
           if (gtype .eq. 1) then
                alinenum=aline//num(iline)
                naline=index(alinenum,'  ') - 1
           else
                write (alinenum,10248) sl(iline),ss(iline)
                naline=index(alinenum,'     ') - 1
           endif
        write(97,fmt=10252,iostat=jj,err=996) tbl(1:ntbl),alinenum(1:naline),
     1 lntype(iline),pttype(iline),ptcolor(iline)(1:ptcolorl(iline))

        endif

        close(97)
	endif

C
C  CLOSE INPUT DATA SETS
c  9999  continue
      DO I=1,NI
         CALL XVCLOSE(UNIT(I),STAT,' ')
      ENDDO   
C
      RETURN
C
995     call xvmessage('??E - Error opening/writing gnuplot file',' ')
        call abend

996	call xvmessage('??E - Error opening/writing gnuplot eps file',' ')
        call abend

999   CALL XVMESSAGE('??E - Stacka error',' ')
      CALL ABEND
      END
C
C  **********************************************************
C
      SUBROUTINE EQUIV(X,LX,Y,LY,LCHECK,LINE,IND,tmptbl,ntbl)
c	X is array of LX bytes
c	Y is array of LY bytes
c	LCHECK verifies the the number of bytes in LY
c	IND is a return, 0=OK, 1= insufficient memory
c
	implicit none
C
	integer*4 ind,lcheck,lx,ly,dum
	integer*4 line,ntbl
	real*4 x(lx),y(ly)
	character*24 tmptbl
c
      IND=0
	dum=lx		!to suppress warning msg in compiler
      IF (LY .LT. LCHECK) GO TO 899
      CALL GRAPH(X,X,Y,line,tmptbl,ntbl)			!,tbl,ntbl)
      RETURN
C
C   INSUFFICIENT MEMORY RETURN
899   IND=1
      RETURN
      END
C
C  **********************************************************
C
      SUBROUTINE GRAPH(X,RBUF,Y,line,tmptbl,ntbl)					!,tbl,ntbl)
	implicit none
C
      COMMON/C1/ SIZE,displace,RDS,XMIN,XMAX,YMIN,YMAX
     &          ,XSCLMN,XSCLMX,YSCLMN,YSCLMX,XSCLDT
     &          ,YSCLDT,XLNGTH,YLNGTH,FORMAT,NORM,NCHAN
     &          ,xsclset,ysclset
      COMMON/C2/ SLX,SSX,ELX,ESX,INX,UNIT,ILINE,NLINES
     &          ,NLI,NSI,NSCHAN,GTYPE,XPAGE,LB,LABTOP
      common/files/filename
      common/commonheader/headermsg,nheadermsg,iiline,i2line
c
      integer*4  iiline,i2line,nheadermsg(220)  !! index into header strings

C
      	REAL*8 MEAN,SIGMA,DBLV
      	REAL*4 XMAX(10),XMIN(10),YMAX(10),YMIN(10)
      	REAL*4 XSCLMN,XSCLMX,YSCLMN,YSCLMX
	REAL*4 TXSCLMN,TXSCLMX,TYSCLMN,TYSCLMX
      	REAL*4 XLNGTH,YLNGTH
      	REAL*4 X(1),RBUF(1),Y(1),YT(4)
	real*4 adx,ady,dnmax,displace,dx,dy,dz,rds,size
	real*4 xinc,xl,xl1,xl2,xpage,xscldt,yinc,ypage,ypeak,yscldt
      	INTEGER*4 INX(10),SLX(10),SSX(10),ELX(10),ESX(10),NLI(10),NSI(10)
      	INTEGER*4 UNIT(10),SN,SL,SS,EL,ES,STAT,GTYPE,sinc
	integer*4 id,idense,ilab,iline,in,inline,inteq,ipt,iq
	integer*4 labtop,lb,linc,ln,ln2,nchan,nlab,nlines,npts
	integer*4 nsamp,nschan,nx,ny,ntmptbl,ntbl
	integer*4 i,j,line
      	LOGICAL*4 NORM,xsclset,ysclset
	character*1 tab
	character*4 format(10)
	character*24 tbl,tmptbl
      	CHARACTER*24 STLAB1
      	CHARACTER*12 STLAB2
      	CHARACTER*56  LABEL(20),xheadermsg
	character*56 headermsg(220) !! Labels * (lines per label+2)
        character*120 filename(10)

C
        character*1 num(5)
c
        data num/'1','2','3','4','5'/
c        data tmptbl/'tmptbl.'/

      STLAB1 = 'AVE GRAY LEVEL = '
      STLAB2 = 'STD DEV = '
      MEAN=0.0
      SIGMA=0.0
      INTEQ=ILINE-1
      IN=1
      LN=SLX(ILINE)
      SN=SSX(ILINE)
C
      LINC=0
      SINC=0
	txsclmn = 40000000
	txsclmx = 1
	tysclmn = 40000000
	tysclmx = 0
        if (line .gt. 1) then
           txsclmn = xsclmn
           txsclmx = xsclmx
           tysclmn = ysclmn
           tysclmx = ysclmx
        endif


      IF (GTYPE .EQ. 1) THEN
C
         IN=INX(ILINE)
         SL=SLX(ILINE)
         SS=SSX(ILINE)
         EL=ELX(ILINE)
         ES=ESX(ILINE)
         NSAMP=MAX0(SS,ES)
c         LINC=0
         IF (EL .GT. SL) LINC=+1
         IF (EL .LT. SL) LINC=-1
c         SINC=0
         IF (ES .GT. SS) SINC=+1
         IF (ES .LT. SS) SINC=-1
      END IF
C
      IF (GTYPE .EQ. 2) GO TO 400
      IF (EL .EQ. SL) GO TO 100
      IF (ES .EQ. SS) GO TO 200
      GO TO 300
C
C  HORIZONTAL LINE
100   continue
      CALL XVREAD(UNIT(IN),RBUF,STAT,'LINE',LN,'NSAMPS',NSAMP,' ')
C      CALL XVCHECK('XVREAD  ',1,'INP',IN,STAT)
      NPTS=IABS(ES-SS)+1
      DO 150 IPT=1,NPTS
         Y(IPT)=RBUF(SN)
         DBLV=Y(IPT)
         MEAN=MEAN+DBLV
         SIGMA=SIGMA+DBLV*DBLV
         SN=SN+SINC
  150 CONTINUE
c        print *,"HORIZONTAL LINE:"
cc        do i=1,npts
cc                print *,"-", x(i),y(i)
cc        enddo
c        print *,"mean, sigma"
c        print *, mean,sigma

      GO TO 500
C
C  VERTICAL LINE
200   continue
      NPTS=IABS(EL-SL)+1
      DO 250 IPT=1,NPTS
      CALL XVREAD(UNIT(IN),RBUF,STAT,'LINE',LN,'NSAMPS',NSAMP,' ')
C      CALL XVCHECK('XVREAD  ',2,'INP',IN,STAT)
         Y(IPT)=RBUF(SN)
         DBLV=Y(IPT)
         MEAN=MEAN+DBLV
         SIGMA=SIGMA+DBLV*DBLV
         LN=LN+LINC
  250 CONTINUE
c        print *,"VERTICAL LINE:"
cc        do i=1,npts
cc                print *,"-", x(i),y(i)
cc        enddo
c        print *,"mean, sigma  summations"
c        print *, mean,sigma

      GO TO 500
C
C  SLANT LINE
300   continue
      NX=IABS(SS-ES)
      NY=IABS(SL-EL)
      NPTS=IFIX(SQRT(FLOAT(NY*NY+NX*NX)))+1
      DZ=ATAN2(FLOAT(NY),FLOAT(NX))
      ADX=COS(DZ)
      ADY=SIN(DZ)
      DX=0.0
      DY=0.0
C
      DO 350 IPT=1,NPTS
      CALL XVREAD(UNIT(IN),RBUF,STAT,'LINE',LN,'NSAMPS',NSAMP,' ')
C      CALL XVCHECK('XVREAD  ',3,'INP',IN,STAT)
         YT(1)=RBUF(SN)
         YT(2)=RBUF(SN+SINC)
C        READ NEXT LINE OF DATA (EXCEPT FOR FIRST OR LAST POINT -
C        IN THAT CASE READ SAME LINE)
         LN2=LN+LINC
         IF (IPT .EQ. 1 .OR. IPT .EQ. NPTS) LN2=LN
      CALL XVREAD(UNIT(IN),RBUF,STAT,'LINE',LN2,'NSAMPS',NSAMP,' ')
C      CALL XVCHECK('XVREAD  ',4,'INP',IN,STAT)
         YT(3)=RBUF(SN)
         YT(4)=RBUF(SN+SINC)
C
         Y(IPT)=YT(1)+DX*(YT(2)-YT(1))+DY*(YT(3)+DX*(YT(4)-YT(3))-YT(1)
     &             -DX*(YT(2)-YT(1)))
         DBLV=Y(IPT)
         MEAN=MEAN+DBLV
         SIGMA=SIGMA+DBLV*DBLV
C
C        CHECK FOR LINE/SAMPLE INCREMENTING
         DX=DX+ADX
         DY=DY+ADY
         IF (DX .LT. 1.0) GO TO 330
C        INCREMENT SAMPLE NUMBER
         SN=SN+SINC
         DX=DX-1.0
         IF (DY .LT. 1.0) GO TO 350
C        INCREMENT LINE NUMBER
330      LN=LN+LINC
         DY=DY-1.0
  350 CONTINUE
cc	print *,"SLANT LINE:"
cc	do i=1,npts
cc		print *,"-", x(i),y(i)
cc	enddo
c	print *,"mean, sigma"
c	print *, mean,sigma
      GO TO 500
C
C        SPECTRAL PLOT
400   continue
      CALL XVREAD(UNIT(IN),RBUF,STAT,'LINE',LN,' ')
C      CALL XVCHECK('XVREAD  ',5,'INP',IN,STAT)
      NPTS=NCHAN
      DO 450 IPT=1,NPTS
         Y(IPT)=RBUF((IPT-1)*NSCHAN+SN)
         DBLV=Y(IPT)
         MEAN=MEAN+DBLV
         SIGMA=SIGMA+DBLV*DBLV
  450 CONTINUE
C
C
C        SCALE DATA ACCORDING TO YVALUES PARAMETERS
500   continue
      DNMAX=255.0
      IF (FORMAT(IN) .EQ. 'HALF') DNMAX=32767.0
      IF (FORMAT(IN) .EQ. 'FULL') DNMAX=65536.0
      IF (FORMAT(IN) .EQ. 'REAL') DNMAX=65536.0
      YINC=(YMAX(ILINE)-YMIN(ILINE))/DNMAX
      yinc = 1.0
      IF ((YINC .EQ. 1.) .AND. (YMIN(ILINE) .EQ. 0.)) GO TO 620
      DO 610 ID=1,NPTS
         Y(ID)=Y(ID)*YINC+YMIN(ILINE)
  610 CONTINUE
cc        print *,"scale to YVALUES:"
cc        do i=1,npts
cc                print *,"-", x(i),y(i)
cc        enddo

C
C        SCALE DATA ACCORDING TO RDS PARAMETER
620   continue
      IF (RDS .EQ. 0) GO TO 630
      DO 625 ID=1,NPTS
         Y(ID)=SQRT(AMAX1(Y(ID)**2-RDS**2,0.))
  625 CONTINUE
cc        print *,"scale to RDS:"
cc        do i=1,npts
cc                print *,"-", x(i),y(i)
cc        enddo

C
C        NORMALIZE DATA
630   continue
      IF (.NOT.NORM) GO TO 640
      YPEAK=Y(1)
      DO 635 ID=2,NPTS
         IF (YPEAK .LT. Y(ID)) YPEAK=Y(ID)
  635 CONTINUE
      DO 638 ID=1,NPTS
         Y(ID)=Y(ID)/YPEAK
  638 CONTINUE
cc        print *,"NORMALIZE:"
cc        do i=1,npts
cc                print *,"-", x(i),y(i)
cc        enddo

C
C        ADD DISPLACEMENT
640   continue
      IF (displace .NE. 0.) then
      	DO  ID=1,NPTS
            Y(ID)=Y(ID)+INTEQ*displace
	ENDDO
      ENDIF
cc        print *,"ADD DISPLACEMENT:"
cc        do i=1,npts
cc                print *,"-", x(i),y(i)
cc        enddo

C
C        COMPUTE MEAN AND STANDARD DEVIATION
      MEAN=MEAN/NPTS
      SIGMA=DSQRT(DABS(SIGMA/NPTS-MEAN*MEAN))
c	print *, "MEAN, STDDEV:"
c        print *, mean,sigma

C
C        LOAD X ARRAY
      X(1)=XMIN(ILINE)
      XINC=(XMAX(ILINE)-XMIN(ILINE))/(NPTS-1)
c	print *, "LOAD X-ARRAY INCREMENT xinc = ",xinc
      IF (XINC .NE. 0.) GO TO 660
      X(1)=1.
      XINC=1.
  660 DO 665 IQ=2,NPTS
         X(IQ)=X(IQ-1)+XINC
  665 CONTINUE
c   now append XSCLMN and XSCLDT to X array
      X(NPTS+1)=XSCLMN
      X(NPTS+2)=XSCLDT
C
c
c	print *,'ysclset, xsclset = ',ysclset,xsclset
      if (.not.ysclset) then
         DO ID=1,NPTS
cc	print *,ID,Y(ID),YSCLMX,YSCLMN
            IF (Y(ID) .GT. YSCLMX) YSCLMX=Y(ID)			!bug here, reversed
            IF (Y(ID) .LT. YSCLMN) YSCLMN=Y(ID)
         ENDDO
      endif
c	X in VICAR IMAGE Always starts at 1,1
      if (.not.xsclset) then
c	 xsclmn = 1
         DO ID=1,NPTS
            IF (X(ID) .GT. XSCLMX) XSCLMX=X(ID)                     !bug here, reversed
            IF (X(ID) .LT. XSCLMN) XSCLMN=X(ID)
         ENDDO   
      endif
c
cc      print *, "ysclmn, ysclmx = ",ysclmn, ysclmx
cc      print *, "tysclmn, tysclmx = ",tysclmn, tysclmx
cc      print *, "xsclmn, xsclmx = ",xsclmn, xsclmx
cc      print *, "txsclmn, txsclmx = ",txsclmn, txsclmx

c
        if (line .gt. 1) then
           if (txsclmn .lt. xsclmn) xsclmn = txsclmn
           if (txsclmx .gt. xsclmx) xsclmx = txsclmx
           if (tysclmn .lt. ysclmn) ysclmn = tysclmn
           if (tysclmx .gt. ysclmx) ysclmx = tysclmx
        endif
c
cc      print *, "ysclmn, ysclmx = ",ysclmn, ysclmx
cc      print *, "xsclmn, xsclmx = ",xsclmn, xsclmx

c
c  now append YSCLMN and YSCLDT to Y array
      Y(NPTS+1)=YSCLMN
      Y(NPTS+2)=YSCLDT
      IDENSE=NPTS/XLNGTH
      IF (NLINES .EQ. 1) IDENSE=0
C
      !! Set SCALE factor to 1.0, as XRT/graph will automatically scale
      !! the X & Y values before displaying the values.
      x(npts+2) = 1.0
      y(npts+2) = 1.0
ccc---      CALL LINE (X,Y,NPTS,1,IDENSE,INTEQ)
      !! Move to (0,0) and set new origin
ccc---      call setactiveset (0)
ccc---      call plot (0.0, 0.0, 3)
      TAB=CHAR(9)
ccccc           tbl=tbl(1:ntbl)//num(iline)
ccccc           ntbl=index(tbl,'  ') - 1

	ntmptbl=index(tmptbl,'  ') - 1
	tbl=tmptbl(1:ntmptbl)//num(line)
	ntbl=index(tbl,'  ') - 1
c	print *, 'before OPEN(99,FILE=TBL(   tbl = ',tbl(1:ntbl)

	OPEN(99,FILE=TBL(1:ntbl),STATUS='UNKNOWN',IOSTAT=J,ERR=998)
	   do i=1,npts
10100 format (1x,f8.0,a1,f10.3)
              WRITE(99,FMT=10100,IOSTAT=J,ERR=998) x(i),tab, y(i)
	   enddo

	CLOSE(99)

C
C
C  **********************************************************
C
C       * LABEL PROCESSING *
C
      inline = 1
      YPAGE=AMAX1(7.,YLNGTH)
      IF (LABTOP .EQ. 1) YPAGE=11.5
      XL2=0.
      XL1=0
      IF (SIZE .EQ. 0.) GO TO 800

C        CHECK IF SAME DATA SET
      IF(LB.EQ.0) GO TO 710

      headermsg (iiline) = 'SAME LABELS'
      nheadermsg (iiline)=56	!index(xheadermsg,'        ') - 1
      inline =inline + 1
      YPAGE = YPAGE-2.0*SIZE
      GO TO 730
C
C        GET LABELS
710   continue

      CALL LABGET(UNIT(IN),NLAB,LABEL)
C        PRINT LABELS

      xheadermsg = ' '
      write (xheadermsg (1:),'(a)') 'Line   ' 
      write (xheadermsg (6:),'(i2)') ILINE
      write (xheadermsg (9:),'(a)') ' - '
      write (xheadermsg (12:50),'(a)') filename(iline)(1:38)

      headermsg (iiline) = xheadermsg 
      nheadermsg (iiline)=56	!index(xheadermsg,'        ') - 1
      iiline = iiline + 1 

      DO 720 ILAB=1,NLAB
C      CALL SYMBOL(XPAGE,YPAGE,SIZE,%DESCR(LABEL(1,ILAB)),0,0.,NCH)
      
         headermsg (iiline) = label(ilab) 
         nheadermsg (iiline)=56		!index(xheadermsg,'        ') - 1
c	print *, 'headermsg = ',headermsg (iiline)
         iiline = iiline + 1 

  720 CONTINUE
C        PRINT MEAN AND STANDARD DEVIATION
730   continue

      write (xheadermsg (1:),'(a)') stlab1       !! 'AVE GRAY SCALE = '
      write (xheadermsg (18:),'(f8.2)') mean 
      headermsg (iiline) = xheadermsg
      nheadermsg (iiline)=56	!index(xheadermsg,'        ') - 1
      iiline = iiline + 1

      write (xheadermsg (1:),'(a)') stlab2       !! 'STD DEV = '
      write (xheadermsg (11:),'(f6.2)') sigma 
      headermsg (iiline) = xheadermsg
      nheadermsg (iiline)=56	!index(xheadermsg,'        ') - 1
      iiline = iiline + 1

C        PRINT SL, SS, EL, ES
      IF(GTYPE.EQ.1) THEN
         write (xheadermsg (1:),'(a)') 'SL='
         write (xheadermsg (4:),'(i3)') SL 
         write (xheadermsg (11:),'(a)') 'SS='
         write (xheadermsg (14:),'(i3)') SS 
         headermsg (iiline) = xheadermsg
	 nheadermsg (iiline)=56		!index(xheadermsg,'        ') - 1
         iiline = iiline + 1

         write (xheadermsg (1:),'(a)') 'EL='
         write (xheadermsg (4:),'(i3)') el 
         write (xheadermsg (11:),'(a)') 'EL='
         write (xheadermsg (14:),'(i3)') es 
         headermsg (iiline) = xheadermsg
	 nheadermsg (iiline)=56		!index(xheadermsg,'        ') - 1
c	 print *,'header = ',headermsg(iiline)(1:nheadermsg (iiline))
         iiline = iiline + 1

      ELSE
         write (xheadermsg (1:),'(a)') 'LINE='
         write (xheadermsg (4:),'(f6.2)') float(ln) 
         write (xheadermsg (11:),'(a)') 'SAMPLE='
         write (xheadermsg (44:),'(f6.2)') float(sn) 
         headermsg (iiline) = xheadermsg
         nheadermsg (iiline)=56	!index(xheadermsg,'        ') - 1

c	print *,'header = ',headermsg(iiline)(1:nheadermsg (iiline))
         iiline = iiline + 1

      END IF
C
800   XL=AMAX1(XL1-XPAGE,XL2-XPAGE)
      XPAGE=XPAGE+XL+0.5
      iiline = iiline + 1       !! Bump index for header strings
C
      RETURN
998     call xvmessage('??E - Error writing gnuplot file - graph',' ')
        call abend

        return

      END

C
C
C
C  **********************************************************
C
C
C
      SUBROUTINE LABGET(UNIT,NLAB,LABEL)
	implicit none
      INTEGER*4 INSTAN(200),STAT,UNIT,COUNT,NLAB
	integer*4 i,j,ichar,ilab,length,lvalue,ntasks
      CHARACTER*500 VALUE
      CHARACTER*32 FORMAT
      CHARACTER*28 TIME,LTIME
      CHARACTER*8 TASKS(200),UNAME,LUNAME
      CHARACTER*32 KEY,LKEY
      CHARACTER*1600 LTASKS
C     LOGICAL*1 LTASKS(1600),LUNAME(8),LKEY(32)
      CHARACTER*56   LABEL(20)
C      LOGICAL*1 LABEL(56,20),LTIME(28),LVALUE(500)
      EQUIVALENCE (TASKS,LTASKS),(UNAME,LUNAME),(TIME,LTIME)
      EQUIVALENCE (KEY,LKEY),(VALUE,LVALUE)

C        BLANK OUT LABEL BUFFER AND INITIALIZE LABEL POINTER
      DO I=1,20
         LABEL(I) = ' '
      ENDDO
C      CALL MVE(1,20*56,' ',LABEL,0,1)
      ILAB=1
      NTASKS=200
C
C        GET NAMES OF ALL HISTORY TASKS
      CALL XLHINFO(UNIT,TASKS,INSTAN,NTASKS,STAT,' ')
C      CALL XVCHECK('XLHINFO ',1,'INP',UNIT,STAT)
C
      DO 200 I=1,NTASKS
C        GET USER AND TIME
      CALL XLGET(UNIT,'HISTORY','USER',UNAME,STAT,'HIST',TASKS(I),
     &           'INSTANCE',INSTAN(I),'FORMAT','STRING',' ')
C      CALL XVCHECK('XLGET   ',1,'INP',UNIT,STAT)
      CALL XLGET(UNIT,'HISTORY','DAT_TIM',TIME,STAT,'HIST',
     &           TASKS(I),'INSTANCE',INSTAN(I),'FORMAT','STRING',' ')
c      CALL XVCHECK('XLGET   ',2,'INP',UNIT,STAT)
C        CONVERT DAT_TIM TO UPPERCASE
      CALL CCASE(TIME,1,28)
C        FILL IN TASK, USER, TIME LINE
C                              1         2         3         4        4 
C                     1234567890123456789012345678901234567890123456789
       LABEL(ILAB) = 'TASK:            USER: '
       WRITE(LABEL(ILAB)(7:14), '(A8)' ) LTASKS(8*I-7:8*I)
       WRITE(LABEL(ILAB)(23:30), '(A8)' ) LUNAME
       WRITE(LABEL(ILAB)(33:56), '(A24)' ) LTIME 
c      CALL MVL('TASK:',LABEL(1,ILAB),5)
c      CALL MVL(LTASKS(8*I-7),LABEL(7,ILAB),8)
c      CALL MVL('USER:',LABEL(17,ILAB),5)
c      CALL MVL(LUNAME,LABEL(23,ILAB),8)
c      CALL MVL(LTIME,LABEL(33,ILAB),24)
      ILAB=ILAB+1
      IF (ILAB .GT. 20) GO TO 500
C
C        SET TO CURRENT TASK
      CALL XLINFO(UNIT,'HISTORY','TASK',FORMAT,LENGTH,COUNT,
     &            STAT,'HIST',TASKS(I),'INSTANCE',INSTAN(I),' ')
C      CALL XVCHECK('XLINFO  ',1,'INP',UNIT,STAT)
      ICHAR=1
C
      DO 100 J=1,999
C        GET NEXT KEYWORD
      CALL XLNINFO(UNIT,KEY,FORMAT,LENGTH,COUNT,STAT,' ')
      IF (STAT .NE. 1 .OR. KEY .EQ. 'TASK') GO TO 150
      IF (KEY .EQ. 'DAT_TIM' .OR. KEY .EQ. 'USER') GO TO 100
C        GET VALUE
      CALL XLGET(UNIT,'HISTORY',KEY,VALUE,STAT,'HIST',TASKS(I),
     &           'INSTANCE',INSTAN(I),'FORMAT','STRING',
     &           'LENGTH',LENGTH,' ')
c      CALL XVCHECK('XLGET   ',3,'INP',UNIT,STAT)
C        TRUNCATE VALUE IF KEYWORD AND VALUE WILL NOT FIT ON ONE LINE
      IF (LENGTH .GT. 47) LENGTH=47
C        SEE IF KEYWORD AND VALUE WILL FIT ON PRESENT LINE
      IF (ICHAR+LENGTH+9 .LT. 56) GO TO 50
      ICHAR=1
      ILAB=ILAB+1
      IF (ILAB .GT. 20) GO TO 500
C        FILL IN KEYWORD AND VALUE INTO LABEL BUFFER
50    WRITE(LABEL(ILAB)(ICHAR:(ICHAR+7)), '(A8)') LKEY
      WRITE(LABEL(ILAB)(ICHAR+8:ICHAR+8), '(A1)' ) '='
      WRITE(LABEL(ILAB)(ICHAR+9:), '(A)') LVALUE
C       CALL MVL(LKEY,LABEL(ICHAR,ILAB),8)
C      CALL MVL('=',LABEL(ICHAR+8,ILAB),1)
C      CALL MVL(LVALUE,LABEL(ICHAR+9,ILAB),LENGTH)
      ICHAR=ICHAR+LENGTH+11
C
  100 CONTINUE

150   ILAB=ILAB+1
      IF (ILAB .GT. 20) GO TO 500

  200 CONTINUE
500   NLAB = ILAB-1

      RETURN
      END
