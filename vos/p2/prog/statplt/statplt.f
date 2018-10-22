      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      implicit none
C     July 10, 1995 ...CRI... MSTP S/W CONVERSION (VICAR PORTING)

      REAL*4    MEANS(600),COVARIANCE(600),EX(363),EY(363)
      INTEGER*4 IPARM(40),STATUS
      integer*4 bands, unit, classes, ibis
      integer*4 npix,nb,row,plotwid,plotht, tics

      integer*4 idel, icount, idef, locxs, locys, locxy
      integer*4 ichan, jchan, I, II, III, L, jj, k
	integer*4 nplotgpi,ntitle,nxtitle,nytitle,nplotname
	integer*4 nplotgpi2
      real*4    xscale, xinc, xloc, xi, sigma, xlo, xhi, ylo, yhi
      real*4    x, xx, yscale, yinc, yloc, y, yy, q, x1,y1
      real*4    a2, b2, c2, theta, sinsq, cossq, sincos
      real*4    r, dx, dy
	logical*4 epsplot
	character*1 two/'2'/
	character*4 gpi/'.gpi'/,eps/'.eps'/
     
      character*80 classname,plotgpi,plotgpi2

      CHARACTER*63 plotname,title,cbuf
      CHARACTER*20 xtitle, ytitle

      CALL XVMESSAGE('STATPLT - 03 July 2012',' ')

C     SET DEFAULT VALUES
      xscale    = 0.0
      xinc      = 0.0
      xloc      = 0.0
      xi        = 0.0
      sigma     = 0.0
      x         = 0.0
      xloc      = 0.0
      xx        = 0.0
      yscale    = 0.0
      yinc      = 0.0
      yloc      = 0.0
      y         = 0.0
      yy        = 0.0
      q         = 0.0
      a2        = 0.0
      b2        = 0.0
      c2        = 0.0
      theta     = 0.0
      sinsq     = 0.0
      cossq     = 0.0
      sincos    = 0.0
      r         = 0.0
      dx        = 0.0
      dy        = 0.0
      bands     = 0
      unit      = 0
      classes   = 0
      ibis      = 0
      npix      = 0
      nb        = 0
      row       = 0
      idel      = 0
      icount    = 0
      idef      = 0
      locxs     = 0
      locys     = 0
      locxy     = 0
      I         = 0
      II        = 0
      III       = 0
      L         = 0
      XSCALE    = 1.0
      YSCALE    = 1.0
      ICHAN     = 1
      JCHAN     = 2
      SIGMA     = 1.0
      XLO       = 0.0
      XHI       = 255.0
      YLO       = 0.0
      YHI       = 255.0
	tics = 0
C                PROCESS PARAMETERS
C        'BANDS'
      CALL XVPARM('BANDS',IPARM,ICOUNT,IDEF,2)
      ICHAN=IPARM(1)
      JCHAN=IPARM(2)

C        'SIGMA'
      CALL XVPARM('SIGMA',SIGMA,ICOUNT,IDEF,1)

C        'PLOTNAME'
        epsplot = .false.
	nplotgpi = 0
	nplotgpi2 = 0
        nplotname = 0
      CALL XVPARM('PLOTNAME',cbuf,ICOUNT,IDEF,1)
        IF (IDEF .EQ. 0) THEN
            if (cbuf .eq. "YES" .or. cbuf .eq."yes") then
               epsplot = .true.
		plotname='statplt'
		nplotname=index(plotname,'   ') - 1
		plotgpi=plotname(1:nplotname)//gpi
	        nplotgpi=index(plotgpi,'  ') - 1
                plotgpi2=plotname(1:nplotname)//eps//gpi
                nplotgpi2=index(plotgpi2,'  ') - 1
c               Plotout and nplotout from above
            elseif (cbuf .eq. "NONE" .or. cbuf .eq."none") then
               epsplot = .false.
	       plotgpi='statplt.gpi'
	       nplotgpi=index(plotgpi,'  ') - 1
            else
               plotname = CBUF
               nplotname=index(plotname,'   ') - 1
               plotgpi=plotname(1:nplotname)//gpi
               nplotgpi=index(plotgpi,'  ') - 1
               plotgpi2=plotname(1:nplotname)//two//gpi
               nplotgpi2=index(plotgpi2,'  ') - 1
               epsplot = .true.
            endif
      ELSE
            epsplot = .false.
            plotgpi='statplt.gpi'
            nplotgpi=index(plotgpi,'  ') - 1
      END IF

C        'XSCALE'
      CALL XVPARM('XSCALE',IPARM,ICOUNT,IDEF,2)
      XLO=IPARM(1)
      XHI=IPARM(2)

C        'YSCALE'
      CALL XVPARM('YSCALE',IPARM,ICOUNT,IDEF,2)
      YLO=IPARM(1)
      YHI=IPARM(2)

C          OPEN INPUT DATA SET
      CALL XVUNIT(UNIT,'INP',1,STATUS,' ')

      if (status .lt. 0) call xvsignal(unit, status, 1) 

C     SET  POINTERS FOR THE LOCATIONS OF MEANS AND SIGMAS

      LOCXS = (ICHAN*(ICHAN+1))/2
      LOCYS = (JCHAN*(JCHAN+1))/2
      LOCXY = MAX0(LOCXS,LOCYS)-IABS(JCHAN-ICHAN)

      call istat_file_open(unit,'read',0,0,0,status) 

      if (status .lt. 0) call istat_signal(unit, status, 1) 
	
      ! Get file information 
      call istat_file_Info(unit, classes, bands, ibis) 

C     INITIALIZE PLOT
!     Specify output PostScript file */
cc ---      call plotfn (plotname)

cc ---      call xrtbegin (status)
cc ---      if (status .ne. 1) then
cc ---         call xvmessage ('Error on XRT/graph initialization',' ')
cc ---         goto 9999
cc ---      endif

cc ---      call setwidgetaspect (800,800)
cc ---      call setgraphaspect (800, 800)

      WRITE (XTITLE,'(A4,I2)') 'BAND', ICHAN
      WRITE (YTITLE,'(A4,I2)') 'BAND', JCHAN
	nxtitle = index(XTITLE,'   ') - 1
	nytitle = index(YTITLE,'   ') - 1
cc ---      CALL axestitles (xtitle,ytitle,90,' ',0)
	title = "Classes from STATS"
	ntitle = index(title,'   ') - 1

      EX(362) = 0.0
      EY(362) = 0.0
      EX(363) = 1.0
      EY(363) = 1.0

      XX = SIGMA/XSCALE
      YY = SIGMA/YSCALE

        plotwid =  648  !640 @72dpi = 8.888.. inches    9 inch = 648
        plotht  =  648  !480 @72dpi = 6.666.. inches    7 inch = 504

	call gpi_dotfile
cc
cc  open gpi data set
cc
	print *,'98 = ',plotgpi(1:nplotgpi)
        open(98,file=plotgpi(1:nplotgpi),status='UNKNOWN',iostat=jj,err=995)
10100 format('# Created by program statplt')              !#'s are ignored in gnuplot
        write(98,fmt=10100,iostat=jj,err=995)
10105 format('# Gnuplot commands for classes')
        write(98,fmt=10105,iostat=jj,err=995)
c10110 format('# Data in ',a)
c        write(98,fmt=10110,iostat=jj,err=995) tbl(1:ntbl)
10115 format('set term x11 font ariel 10 size ',i4,', ',i4)
        write(98,fmt=10115,iostat=jj,err=995) plotwid,plotht
10116 format('set output')                              !set output to screen
        write(98,fmt=10116,iostat=jj,err=995)
        if (tics .eq. 1) then
10120 format('set grid ')
                write(98,fmt=10120,iostat=jj,err=995)
        endif
10125 format("set ylab '",a,"'" )
       write(98,fmt=10125,iostat=jj,err=995) ytitle(1:nytitle)
10130 format("set xlab '",a,"'")
       write(98,fmt=10130,iostat=jj,err=995) xtitle(1:nxtitle)
10141 format("set clip points")                         !how to deal with points out of range
        write(98,fmt=10142,iostat=jj,err=995)
10142 format("set clip one")                            !how to deal with connecting lines out of range
        write(98,fmt=10141,iostat=jj,err=995)
10145 format("set title '",a,"'")
       write(98,fmt=10145,iostat=jj,err=995) title(1:ntitle)
10135 format("set yrange [",f8.0,":",f8.0,"]")
       write(98,fmt=10135,iostat=jj,err=995) ylo,yhi
10140 format("set xrange [",f8.0,":",f7.0,"]")
       write(98,fmt=10140,iostat=jj,err=995) xlo,xhi
10150 format("f(x) = 0.0")
	write(98,fmt=10150,iostat=jj,err=995)
10160 format("set multiplot")
	 write(98,fmt=10160,iostat=jj,err=995)

	if (epsplot) then
cc
cc  open eps data set
cc
           open(97,file=plotgpi2(1:nplotgpi2),status='UNKNOWN',iostat=jj,err=996)
           write(97,fmt=10100,iostat=jj,err=996)
           write(97,fmt=10105,iostat=jj,err=996)
10300 format("set terminal postscript eps enhanced")
           write(97,fmt=10300,iostat=jj,err=996)
10305 format("set output '",a,"'")
           write(97,fmt=10305,iostat=jj,err=996) plotname(1:nplotname)
           write(97,fmt=10125,iostat=jj,err=996) ytitle(1:nytitle)
           write(97,fmt=10130,iostat=jj,err=996) xtitle(1:nxtitle)
           write(97,fmt=10142,iostat=jj,err=996)
           write(97,fmt=10141,iostat=jj,err=996)
           write(97,fmt=10135,iostat=jj,err=996) ylo,yhi
           write(97,fmt=10140,iostat=jj,err=996) xlo,xhi
           write(97,fmt=10150,iostat=jj,err=996)
	endif   !if (epsplot) then
C     READ A RECORD, PLOT THE MEAN AND THE SIGMA ELLIPSE
      DO III = 1, classes 
         row = III

         call istat_record_read (unit, row, classname, npix,
     +	                         nb,means,covariance,status) 
         if (status .lt. 0) call istat_signal(unit,status,1) 

!        Get elipse center point
         X = means(ichan)
         Y = means(jchan)

         IF(X .GE. XLO .AND. X .LE. XHI .and.
     +      Y .GE. YLO .AND. Y .LE. YHI) THEN
            Q = III
cc ---            CALL NUMBER(X,Y+0.1,0.12,Q,0.0,-1)
c10310 format("set object ",i2," polygon from ",f5.0," to ",f5.0,",",f5.0," nohead linestyle ",i3)
c        write (98,fmt=10310,iostat=jj,err=995) iii
ccc	if (iii .eq. 1) then
ccc10200 format ("plot ",i3," w p ls ",i3,",\")
ccc        write (98,fmt=10200,iostat=jj,err=995) iii,iii
ccc	elseif (iii .eq. classes) then
ccc10205 format ("     ",i3," w p ls ",i3,",\")
ccc	write (98,fmt=10205,iostat=jj,err=995) iii,iii
ccc10210 format ("unset multiplot")
ccc	write (98,fmt=10210,iostat=jj,err=995)
ccc	else
cset label 2 "string" at 3,4
ccc	endif
C           CALCULATE THE SIGMA ELLIPSE POINTS and PUT IN EX & EY ARRAYS

            A2 = covariance(LOCXS)+1E-9
            B2 = covariance(LOCYS)+1E-9
            C2 = covariance(LOCXY)+1E-9
            Q = A2*B2-C2*C2

            DO L=1,180
               THETA = real (L)
               !! Provide a crutch for CODA1.  A High performance arithmetic
               !! trap will occur when result from sin or cos approaches zero
               !! and the result is squared 
               if (L .eq. 180) then
                  sinsq = 0.0
               else
                  SINSQ = SIN(THETA)**2
               endif
               if (L .eq. 90) then
                  cossq = 0.0
               else
                  COSSQ = COS(THETA)**2
               endif
               if (L .eq. 90 .or. L .eq. 180) then
                  sincos = 0.0
               else
                  SINCOS = SIN(THETA)*COS(THETA)
               endif
               R = B2*COSSQ+A2*SINSQ-2.0*C2*SINCOS+1E-15
               if (cossq .eq. 0.0) then
                  dx = 0.0
               else
                  DX = SQRT(COSSQ*Q/R)
               endif
               IF(L.GT.90) DX=-DX
               if (sinsq .eq. 0.0) then
                  dy = 0.0
               else
                  DY = SQRT(SINSQ*Q/R)
               endif
               EX(L) = X+DX*SIGMA
               EY(L) = Y+DY*SIGMA
               EX(L+180) = X-DX*SIGMA
               EY(L+180) = Y-DY*SIGMA
               IF(EX(L).LT. XLO) EX(L)= XLO
               IF(EX(L).GT.XHI) EX(L)=XHI
               IF(EY(L).GT.YHI) EY(L)=YHI
               IF(EX(L+180).GT. XHI) EX(L+180)=XHI
               IF(EX(L+180).LT. XLO) EX(L+180)=XLO
               IF(EY(L+180).LT. YLO) EY(L+180)=YLO
            END DO
            EX(361) = EX(1)
            EY(361) = EY(1)
cc ---            CALL LINE(EX,EY,361,1,0,0)

cc	goto 400
	    do k=1,360
10315 format("set arrow from ",f5.0,",",f5.0," to ",f5.0,",",f5.0," nohead linestyle ",i3) 
	write (98,fmt=10315,iostat=jj,err=995) ex(k),ey(k),ex(k+1),ey(k+1),iii
	       if (epsplot) then
	          write (97,fmt=10315,iostat=jj,err=996) ex(k),ey(k),ex(k+1),ey(k+1),iii
	       endif
	    enddo
cc400 	continue
c    print the center point of ellipse
10200 format ("plot '-' t '' w linespoints ps 3")
        write (98,fmt=10200,iostat=jj,err=995)
c10205 format ("set size 0.28")
c       write (98,fmt=10205,iostat=jj,err=995)

10215 format (f7.1,f7.1)
        write (98,fmt=10215,iostat=jj,err=995) X,Y
10220 format ("e")
        write (98,fmt=10220,iostat=jj,err=995)
        x1=x - 1
        y1=y - 5
10230 format('set label 1 "',a,'" at ',f7.1,',',f7.1,' front')
        write (98,fmt=10230,iostat=jj,err=995) classname,X1,Y1

         END IF  !IF(X .GE. XLO .AND. X .LE. XHI .and.
      END DO 	 !DO III = 1, classes

10350 format("plot f(x) t '' ls 1")
	write (98,fmt=10350,iostat=jj,err=995)
	if (epsplot) then
	    write (97,fmt=10350,iostat=jj,err=996)
	    close(97)
	endif
10210 format ("unset multiplot")

	write (98,fmt=10210,iostat=jj,err=995)

10255 format("pause mouse any")                 !allows plot to display on screen until mouse click
       write(98,fmt=10255,iostat=jj,err=995)

	close(98)

C     CLOSE PLOT FILE
cc ---      CALL PLOT(0.0,0.0,999)

C     CLOSE INPUT DATA SET
      CALL XVCLOSE(UNIT,STATUS,' ')

	return
c9999  continue

995	continue
	call xvmessage('??E - Error opening/writing gnuplot file',' ')
        call abend

996     call xvmessage('??E - Error opening/writing gnuplot eps file',' ')
        call abend

      RETURN
      END
c=============================================================================
	subroutine gpi_dotfile
c
c	creates .gnuplot in local directory to define linestypes
c
	integer*4 i,jj,lt(50),pt(50)
	character*10 dotfile /".gnuplot"/
c
	data lt/1,2,3,4,5,6, 2,3,4,5,6,1, 3,4,5,6,1,2, 4,5,6,1,2,3,
     1 5,6,1,2,3,4, 6,1,2,3,4,5, 1,2,3,4,5,6, 2,3,4,5,6,1, 3,4 / !,5,6,1,2,
	data pt/1,2,3,4,5,6, 1,2,3,4,5,6, 1,2,3,4,5,6, 1,2,3,4,5,6,
     1 1,2,3,4,5,6, 1,2,3,4,5,6, 1,2,3,4,5,6, 1,2,3,4,5,6, 1,2/
c
c set term tgif landscape color dashed font "Ariel" 10 500 400
c # line = linedescriptor, lt = line type, lw = line width, 
c # pt = point type, ps = point size , lc = line color 

           open(96,file=dotfile,status='UNKNOWN',iostat=jj,err=999)
10100 format("# created by vicar program statplt")
	write(96,fmt=10100,iostat=jj,err=999)
10110 format("# line = linedescriptor, lt = line type, lw = line width,")
	write(96,fmt=10110,iostat=jj,err=999)
10120 format("# pt = point type, ps = point size , lc = line color")
	write(96,fmt=10120,iostat=jj,err=999)	
10130 format('set term tgif landscape color dashed font "Ariel" 10 500 400')
           write(96,fmt=10130,iostat=jj,err=999)
	do i=1,50
10150 format("set style line ",i3,"  lt ",i2," lw 1 pt ",i2)	
	    write(96,fmt=10150,iostat=jj,err=999) i,lt(i),pt(i)
	enddo
	close(96)

	return

999	continue
        call xvmessage('??E - Error opening/writing dot gnuplot file',' ')
        call abend

	end
