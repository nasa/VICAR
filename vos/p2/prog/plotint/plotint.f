      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C
C---- VICAR PROGRAM "PLOTINT".
C     PURPOSE: TO PLOT  DATA CONTAINED IN  COLUMNS OF AN
C               IBIS INTERFACE FILE.
C
C     PROGRAMMER: BORIS GOKHMAN, JULY 1981.
C
C	MODIFIED:   FRANK EVANS 	NOVEMBER 1985  
C			FOR F77 CALCOMP, GENERAL MODERNIZATION
C
C       MODIFIED:   EJB    SEPT 87 FOR MULTIPLE PLOTS AS PER
C                          CONTROL COLUMN ON SPECIFIED OUTPUT DEVICES
C
C       MODIFIED:   BAM   MSTP PORTING  12/95
C
C	MODIFIED:   PXA   JAN 97 CONVERTED CALCOMP CALLS TO XRTPS
C			  CALLS AND REWROTE PLOT PROCEDURES

c       aug2006  -lwk-  fixed bug (Linux only) when YCOLSTR defaulted;
c			added NODISP and PLOTOUT parameters to support
c			output to file instead of display (no code needed
c			for NODISP as it is parsed by xrtps routines)

	implicit none
        integer*4 ibisrows,ibiscols
        parameter (ibisrows = 2000)
        parameter (ibiscols = 20)

	BYTE      YSTRTMP(1200)
      	integer*4	NPOINT(400), NSTART(400), NCOLY(20),nystr
      	integer*4	LINTYP(20),SYMTYP(20)
     	integer*4	COUNT, freq(20),DEF,runit,ibis_in
      	integer*4	XRANDF,YRANDF
	integer*4 YSTRPTR(20), YSTRLEN(20),asyml(20)
        integer*4   status, plotwid,plotht,ntitle,nxlabel,nylabel
	integer*4 i,jj,k,l,iplot,clen,ntbl,nplotgpi,nfilename
	integer*4 lastpt,mycol,ncol
	integer*4 ncolx,ncontr,nplots,icnt,nploteps,icount
	integer*4 nplotgpi2,nplotname,isize,com1def,com2def,com3def
        real*4	COLX(ibisrows),COLY(ibisrows,ibiscols),CONTRL(100000)
        real*4	XRAN(2),YRAN(2), XRANG1,XRANG2, YRANG1,YRANG2
        real*4	XLEN,YLEN
	logical*4 epsplot,multi
	character*4 eps/'.eps'/,gpi/'.gpi'/,asc/'.asc'/
c& MODIFIED EJB SEPT 87 ************************
c	CHARACTER*7 DEVICE
	character*8  pformat,symcolor(20)
	CHARACTER*100  TITLE, XLABEL,outline
	CHARACTER*60 YLABEL,CBUF
	character*64 comment1,comment2,comment3
	CHARACTER*20 ycolstr(20),asymtyp(20)
	character*80 plotgpi,plotgpi2,ploteps,tbl,plotname
	character*255 filename
c
        include 'gnuplotchar'
c	character*5 ystr(20)
c	integer*4 pttype(20),lntype(20),ystrl(20),ptcolorl(20)
c        character*8 ptcolor(20),lncolor(20),lincolor(20)
c	CHARACTER*20 symclass(3)

c	data pttype/ 5, 9, 7,13,11, 1, 2, 3, 5, 9, 7,13,11, 1, 2, 3, 5, 9, 7,13/
c	data lntype/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1/
c	data ptcolor/'green','magenta','brown','blue','purple',
c     1 'red','cyan','orange','green','purple',
cv     2 'magenta','blue','brown','red','cyan',
c     3 'orange','green','purple','magenta','blue'/
c	data ptcolorl/5,7,5,4,6, 3,4,6,5,6, 7,4,5,3,4, 6,5,6,7,4/
c	data lncolor/'beige','red','green','cyan','purple',
c     1 'blue','orange','magenta','beige','red',
c     2 'green','cyan','purple','blue','orange',
c     3 'magenta','beige','red','green','cyan'/
c	data ystr/'col2','col3','col4','col5','col6',
c     1 'col7','col8','col9','col10','col11',
c     2 'col12','col13','col14','col15','col16',
c     3 'col17','col18','col19','col20','col21'/
c	data ystrl/4,4,4,4,4, 4,4,4,5,5, 5,5,5,5,5, 5,5,5,5,5/
c	data symclass/'points','linespoints','lines'/
c
	TITLE(1:100)=' '
	XLABEL(1:100)=' '
	YLABEL(1:60)=' '
	comment1(1:60)=' '
	comment2(1:60)=' '
	comment3(1:60)=' '
	com1def=0
	com2def=0
	com3def=0
	nplotname = 0
	nplotgpi = 0
	nplotgpi2 = 0
	nploteps = 0
	isize = 10
	def = 0
	mycol=1
	do i=1,20
	    freq(i)=0
	enddo
C&
C& ********************************************
C
	call xvmessage('PLOTINT - 2013-08-19',' ')
C---- READ PARAMETERS 
C
C---- GET THE NUMBERS OF THE X-COL AND THE Y-COL AND CONTROL COL
C
	   
      CALL XVP('YCOL',NCOLY,MYCOL)
c	print *,'mycol = ',mycol
      CALL XVP('XCOL',NCOLX,COUNT)
C
      CALL XVP('CONTROL',NCONTR,COUNT)
C--  GET THE SIZE OF THE PLOT.
c       default
        plotwid =  648  !640 @72dpi = 8.888.. inches    9 inch = 648
        plotht  =  504  !480 @72dpi = 6.666.. inches    7 inch = 504

      CALL XVP('XLEN',XLEN,COUNT)
      CALL XVP('YLEN',YLEN,COUNT)
      IF(XLEN .EQ. 0) THEN
	CALL MABEND('??E -X-AXIS length must be greater than zero...')
      ENDIF
      IF(YLEN .EQ. 0) THEN
	CALL MABEND('??E - Y-AXIS length must be greater than zero...')
      ENDIF
      IF(XLEN .GT. 12.0) THEN
	CALL MABEND('??E - X-AXIS length cannot exceed 5 inches (default)...')
      ENDIF
      IF(YLEN .GT. 12.0) THEN
	CALL MABEND('??E - Y-AXIS length cannot exceed 12 inches ...')
      ENDIF
	plotwid = xlen*72		!Convert to pixels
	plotht = ylen*72
		
C
C--  GET THE TEXT/TITLE INFO AND SIZE
      CALL XVP('TITLE',TITLE,COUNT)
	ntitle=index(title,'   ') - 1

      CALL XVP('XLABEL',XLABEL,COUNT)
	nxlabel=index(xlabel,'  ') - 1
      CALL XVP('YLABEL',YLABEL,COUNT)
	nylabel=index(ylabel,'  ') - 1

C  'LABELSIZ'
      CALL XVPARM('LABELSIZ',ISIZE,COUNT,DEF,1)       !font in points
	if (def .eq. 1) then
		isize=10
	endif
c      CALL XVP('HEIGHT',HEIGHT,COUNT)
c
c	put coly in ycolstr(x)
	DO I=1,mycol
	  ycolstr(i) = ystr(i)
	ENDDO
      CALL XVP('YCOLSTR',YSTRTMP,NYSTR)
      IF (nystr.gt.0) then
         CALL XVSPTR(YSTRTMP,NYSTR,YSTRPTR,YSTRLEN)
         DO L = 1 , NYSTR
	    if (YSTRPTR(nystr) > 99) then
		call xvmessage ('??E - Column numbers > 99 not supported',' ' )
		call abend
	    endif
	    DO K = 1, YSTRLEN(L)
	        YCOLSTR(L)(K:K) = CHAR(YSTRTMP(YSTRPTR(L)+K-1))
	    ENDDO
            ystrl(l) = YSTRLEN(L)		
         ENDDO
      ENDIF
C

C---- READ THE AXIS LABELS AND SYMBOL STUFF
      CALL XVPARM('FREQ',FREQ,COUNT, DEF,20)
c	print *,'def,count for freq = ',def,count
	IF (DEF .EQ. 1) THEN		!if default
		do i = 1,mycol
		freq(i) = 1
		enddo 
	ELSE
		do i = 1,20
		  if (freq(i) .eq. 0) freq(i) = 1
c		print *,"freq(i) = ",freq(i)
		enddo  
	ENDIF

      CALL XVPARM ('SYMTYPE',SYMTYP,COUNT, DEF,20)
      IF (DEF .EQ. 1) THEN         !if default
          DO I = 1,MYCOL
	    SYMTYP(I) = 1
	    asymtyp(i) = "points"
	    asyml(i)=index(asymtyp(i),'   ') - 1
	  ENDDO
      ENDIF
cc set up symbols, lines and colors
      DO I = 1,MYCOL
c          LINTYP(I) = FREQ*(SYMTYP(I)-1)
	  asymtyp(i) = symclass(symtyp(i))
	  asyml(i) = index(asymtyp(i),'   ') - 1	
	  lintyp(i) = lntype(i)
c	  lincolor(i) = lncolor(i)
c	  symtyp(i) = pttype(i)
	  symcolor(i) = ptcolor(i)	  
      ENDDO
C--  READ THE X AND Y RANGE IF PROVIDED
      CALL XVPARM('XRANGE',XRAN,COUNT,XRANDF,2)
      CALL XVPARM('YRANGE',YRAN,COUNT,YRANDF,2)
      XRANG1 = XRAN(1)		!min
      XRANG2 = XRAN(2)		!max
      YRANG1 = YRAN(1)
      YRANG2 = YRAN(2)
      IF (XRANG1 .EQ. XRANG2) THEN
  	  CALL MABEND('??E - XRANGE must vary...')
      ENDIF
      IF (YRANG1 .EQ. YRANG2) THEN
	  CALL MABEND('??E - YRANGE must vary...')
      ENDIF
C	now have to read data if default
C
C--- OPEN THE INTERFACE FILE
	CALL XVP('INP',FILENAME,ICNT)   !INPUT FILENAME
	nfilename = index(filename,'   ') - 1
 
C  'PLOTOUT'
C     Resolve output gnuplot and PostScript filenames
	epsplot = .false.
      CALL XVPARM('PLOTOUT',CBUF,ICOUNT,DEF,1)
          plotname = CBUF
          nplotname=index(plotname,'   ') - 1
          plotgpi=plotname(1:nplotname)//gpi
          nplotgpi=index(plotgpi,'  ') - 1
          plotgpi2=plotname(1:nplotname)//eps//gpi
          nplotgpi2=index(plotgpi2,'  ') - 1
          ploteps=plotname(1:nplotname)//eps
          nploteps=index(ploteps,'  ') - 1
          tbl = plotname(1:nplotname)//asc
          ntbl = index(tbl,'  ') - 1

       call xvp ('PLOTFMT',pformat,icount)
         if (pformat .eq. 'eps' .or. pformat .eq. 'EPS') epsplot = .true.
             
c      COMMENTS
      CALL XVPARM('COMMENT1',comment1,COUNT,COM1DEF,1)
      CALL XVPARM('COMMENT2',comment2,COUNT,COM2DEF,1)
      CALL XVPARM('COMMENT3',comment3,COUNT,COM3DEF,1)

C	open file and get characteristics
	CALL XVUNIT(runit,'INP',1,status,' ')
	if (status .ne. 1) then
		call xvmessage('??E - Error on ibis unit call',' ')
		call mabend('??E - Program terminated',' ')
	endif
        call ibis_file_open(runit,ibis_in,'read',0,0,' ',' ',status)
        if (status .ne. 1) call ibis_signal_u(runit,status,1)
        call ibis_file_get(ibis_in,'nc',ncol,1,1)
        if (ncol .gt. ibiscols) then
            write (outline,10100) ncol,ibiscols
10100 format ('??E More columns in ibis file ',i4,' than ',i4,' can be stored')
            call xvmessage(outline,' ')
            call abend
        endif

       call ibis_file_get(ibis_in,'nr',CLEN,1,1)
        if (clen .gt. ibisrows) then
            write (outline,10105) clen,ibisrows
10105 format ('??E More rows in ibis file ',i4,' than ',i4,' can be stored')
            call xvmessage(outline,' ')
            call abend
        endif

	call ibis2asc (ibis_in,ncol,clen,ibisrows,ibiscols,tbl,ntbl)

       call ibis_column_read(ibis_in,contrl,ncol,1,clen,status)
       if (status .ne. 1) call ibis_signal_u(runit,status,1)	
c	find out how many different numbers in control column
c	starting lines and lengths
      CALL FINDCO(ibis_in,CONTRL,NCONTR,CLEN,NSTART,NPOINT,NPLOTS)
C
C
C---- BEGIN PLOTS. THIS LOOP MAKES COMPLETE PLOTS(TOTAL # =NPLOTS)
C     ACCORDING TO THE CONTROL COLUMN.
C
	open(98,file=plotgpi(1:nplotgpi),status='UNKNOWN',iostat=jj,err=995)
	if (epsplot) then
           open(97,file=plotgpi2(1:nplotgpi2),status='UNKNOWN',iostat=jj,err=996)
	endif
c
c---------MAIN LOOP--------
c
      DO 100 IPLOT = 1,NPLOTS
C---- READ "X" AND "Y" COLUMNS.
C  Rewrite GETSET calls with ibis column read calls
         call ibis_column_read(ibis_in,colx,ncolx,1,clen,status)
         if (status .ne. 1) call ibis_signal_u(ibis_in,status,1)
         do l=1,mycol
 	    call ibis_column_read(ibis_in,coly(1,l),ncoly(l),1,clen,status)
	    if (status .ne. 1) call ibis_signal_u(ibis_in,status,1)
         enddo
         LASTPT = NPOINT(IPLOT)
cc
cc  open gpi data set
c
c	write constant part of gpi
	if (iplot.eq.1) then
	    call write_gpi_1(98,tbl,ntbl,ylabel,nylabel,
     1 xlabel,nxlabel,isize,plotwid,plotht,nplots,iplot,
     2 ploteps,nploteps)
	endif   ! if iplot=1

c	write variable part
	   call write_gpi2_2(98,title,ntitle,xrang1,xrang2,
     1 yrang1,yrang2,xrandf,yrandf,mycol,nplots,iplot,isize,
     2 com1def,com2def,com3def,comment1,comment2,comment3,
     3 colx,coly,nstart,npoint,ncontr,filename,nfilename)
	

	multi = .false.
        if (mycol.gt.1 .and. iplot .eq.1) then
	    multi=.true.
10143 format("set multiplot")
            write(98,fmt=10143,iostat=jj,err=995)
        endif

c	write plots part
	if (iplot.eq.1) then
	    call write_pts_gpi_1(98,mycol,tbl,ntbl,ncolx,ncoly,
     1 ycolstr,ystrl,asymtyp,asyml,lintyp,pttype,freq,ptcolor,ptcolorl)

	else
            call write_pts_gpi_2(98,mycol,tbl,ntbl,ncolx,ncoly,
     1 ycolstr,ystrl,asymtyp,asyml,lintyp,pttype,freq,ptcolor,ptcolorl,
     2 ncontr,iplot)
	endif

	if (multi) then
10254   format('unset multiplot')
	    write(98,fmt=10254,iostat=jj,err=995)
	endif
10255 format("pause mouse any")                 !allows plot to display on screen until mouse click
            write(98,fmt=10255,iostat=jj,err=995)
C	
c----------- eps plot
	if (epsplot) then
	    if (iplot.eq.1) then
cc
cc  open eps data set
cc
	       call write_gpi_1(97,tbl,ntbl,ylabel,nylabel,
     1 xlabel,nxlabel,isize,plotwid,plotht,nplots,iplot,
     2 ploteps,nploteps)
	
	    endif
            call write_gpi2_2(97,title,ntitle,xrang1,xrang2,
     1 yrang1,yrang2,xrandf,yrandf,mycol,nplots,iplot,isize,
     2 com1def,com2def,com3def,comment1,comment2,comment3,
     3 colx,coly,nstart,npoint,ncontr,filename,nfilename)
c
            multi = .false.
            if (mycol.gt.1 .and. iplot .eq.1) then
                multi=.true.
                write(97,fmt=10143,iostat=jj,err=995)
            endif

	    if (iplot.eq.1) then
	        call write_pts_gpi_1(97,mycol,tbl,ntbl,ncolx,ncoly,
     1 ycolstr,ystrl,asymtyp,asyml,lintyp,pttype,freq,ptcolor,ptcolorl)

	    else
                call write_pts_gpi_2(97,mycol,tbl,ntbl,ncolx,ncoly,
     1 ycolstr,ystrl,asymtyp,asyml,lintyp,pttype,freq,ptcolor,ptcolorl,
     2 ncontr,iplot)

	    endif
            if (multi) then
                write(97,fmt=10254,iostat=jj,err=995)
            endif


	endif	!epsplot
c------------
  100 CONTINUE		!DO 100 IPLOT = 1,NPLOTS
c
c==========End Main Loop
	close (98)
	if (epsplot) then
		close (97)
	endif
C
	call ibis_file_close(ibis_in,' ',status)
	if (status .ne. 1) call ibis_signal_u(RUNIT,status,1)

      RETURN
c   error returns
995     call xvmessage('??E - main44: Error opening/writing gnuplot file',' ')
        call abend
	return
996	call xvmessage('??E - main44: Error opening/writing gnuplot eps file',' ')
        call abend
	return

      END
C
C**************************************
	INTEGER*4 FUNCTION SLENGTH(STRING)
	implicit none
	INTEGER*4	I
	CHARACTER*(*) STRING

	I = LEN(STRING)
	DO WHILE (ICHAR(STRING(I:I)) .EQ. 32 .AND. I .GT. 1)
	    I = I - 1
	ENDDO
	SLENGTH = I
	RETURN
	END
C
C*************************************
      REAL*4 FUNCTION ARRMIN(ARRAY,START,NUMBER)
c
c   find minimum value in a list or a segment
c   of a list
c
	implicit none
	integer*4 start,number,i
      REAL*4  ARRAY(1), DUMMY
      DUMMY = ARRAY(start)
      DO I = START+1,NUMBER
        DUMMY = AMIN1(DUMMY,ARRAY(I))
c	print *,'dummy = ',dummy
      ENDDO
      ARRMIN = DUMMY
      RETURN
      END
C*************************************
      REAL*4 FUNCTION ARRMAX(ARRAY,START,NUMBER)
c
c   find maximum value in a list or a segment
c   of a list
c
	implicit none
      REAL*4  ARRAY(1), DUMMY
	integer*4 i,start,number
      DUMMY = ARRAY(start)
      DO I = START+1,NUMBER
        DUMMY = AMAX1(DUMMY,ARRAY(I))
      ENDDO
      ARRMAX = DUMMY
      RETURN
      END

c****************************************************************
	SUBROUTINE FINDCO (ibis_in,CONTRL,NCONTR,LENGTH,NSTART,
     *		     NPOINT,NSETS)
	implicit none
	INTEGER*4 NPOINT(1), NSTART(1)
	INTEGER*4 ibis_in,status,npt,nelem,l,length,nsets,ncontr 
C	REAL CONTRL(100000)
	REAL*4 CONTRL(1)
c	DIMENSION COLUMN(1)
C	NROWS = (LENGTH-1)/128+1
C	NRECTR = NROWS*(NCONTR-1)+2
C
C---- READ CONTROL COLUMN AND FIND START POINTS AND LENGTHS
C     FOR THE SETS.
C
C	CALL GETCOL (UNIT,NCONTR,LENGTH,CONTRL)
C	call ibis_column_read(unit,ncontr,length,contrl,status)
	call ibis_column_read(ibis_in,contrl,ncontr,1,length,status)
	if (status .ne. 1) call ibis_signal_u(ibis_in,status,1)
	NSTART(1) = 1
	NPT = 0
	NSETS = 0
c	LENGTH=100		!length from ibis_file_get
	L = LENGTH-1
	DO NELEM=1,L
	  NPT = NPT+1
	  IF (CONTRL(NELEM).NE.CONTRL(NELEM+1)) THEN
	    NSETS = NSETS+1
	    NSTART(NSETS+1) = NELEM+1
	    NPOINT(NSETS) = NPT
	    NPT = 0
	  ENDIF
	ENDDO
	NSETS = NSETS+1
	NPOINT(NSETS) = NPT+1
	RETURN
	END
C*********************************************************************
        subroutine write_gpi_1(unit,tbl,ntbl,ylabel,nylabel,
     1 xlabel,nxlabel,isize,plotwid,plotht,nplots,iplot,
     2 eps,neps)
c
c       write out the first part of gpi form
c
        implicit none
        integer*4 unit,isize,plotwid,plotht
        integer*4 nplots,iplot,jj,psize
        integer*4 ntbl,nylabel,nxlabel,neps
        character*60 ylabel
        character*80 tbl,eps
        character*100  xlabel
c

	psize=isize
	if (unit .eq. 97) psize=16
10100 format('# Created by program plotint')              !#'s are ignored in gnuplot
            write(unit,fmt=10100,iostat=jj,err=995)
10105 format('# Gnuplot commands for ibis file plot')
            write(unit,fmt=10105,iostat=jj,err=995)
10110 format('# Data in ',a)
            write(unit,fmt=10110,iostat=jj,err=995) tbl(1:ntbl)

        if (unit.eq.97) then
10300 format('set terminal postscript eps enhanced color "Ariel" ',i2,' size 11 ,8')
           write(unit,fmt=10300,iostat=jj,err=995) psize
10305 format("set output '",a,"'")
           write(unit,fmt=10305,iostat=jj,err=995) eps(1:neps)
        else
c  size is XX,YY
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
C*********************************************************************
        subroutine write_gpi2_2(unit,title,ntitle,xrang1,xrang2,
     1 yrang1,yrang2,xrandf,yrandf,mycol,nplots,iplot,isize,
     2 com1def,com2def,com3def,comment1,comment2,comment3,
     3 colx,coly,nstart,npoint,ncontr,filename,nfilename)
c
c       write out the repeatable descriptive part of gpi form
c       for first, or only, plot
c
        implicit none
        integer*4 unit,lastpt,firstpt,isize,psize,isize2
        integer*4 xrandf,yrandf,nplots,iplot,mycol,ii,jj,l
        integer*4 ntitle,ncontr,nfilename,com1def,com2def,com3def
	integer*4 ncom1,ncom2,ncom3
	integer*4 npoint(1), nstart(1)
        real*4 colmin,colmax,xmin,xmax,ymin,ymax
        real*4 xrang1,xrang2,yrang1,yrang2
        REAL*4 colx(2000),coly(2000,20)
        real*4 arrmin,arrmax
        character*64 comment1,comment2,comment3
        character*100  title,filename
c
	ncom1 = 0
	ncom2 = 0
	ncom3 = 0
        colmin = 0.0
        colmax = 0.0
        xmin = 1.E+10
        xmax = -1.E+10
        firstpt = nstart(iplot)
        lastpt = nstart(iplot) + npoint(iplot)
c
c
c       the data set
c
	psize = isize
	if (unit .eq. 97) psize =  16
	isize2 = 10
	if (unit .eq. 97) isize2 =  16

10145 format('set title "',a,'" font "Ariel,',i2,'"')
             write(unit,fmt=10145,iostat=jj,err=995) title(1:ntitle),psize

c    if xrandf .eq 0 then xrange has been entered
        IF (XRANDF .EQ. 1) THEN                        !default
            COLMIN = ARRMIN(COLX,FIRSTPT,LASTPT)
            COLMAX = ARRMAX(COLX,FIRSTPT,LASTPT)
            IF (XMIN .GT. COLMIN) XMIN=COLMIN
            IF (XMAX .LT. COLMAX) XMAX=COLMAX
            xrang1 = xmin
            xrang2 = xmax
        ENDIF
        YMIN = 1.E+10
        YMAX = -1.E+10

        IF (YRANDF .EQ. 1) THEN
            DO L = 1, MYCOL
               COLMIN = ARRMIN(COLY(1,L),FIRSTPT,LASTPT)
               COLMAX = ARRMAX(COLY(1,L),FIRSTPT,LASTPT)
               IF (YMIN .GT. COLMIN) YMIN=COLMIN
               IF (YMAX .LT. COLMAX) YMAX=COLMAX
            ENDDO
            yrang1 = ymin
            yrang2 = ymax
        ENDIF

C
C---- SET THE SCALING PARAMETERS.
C

10135 format("set yrange [",f10.2,":",f10.2,"]")
             write(unit,fmt=10135,iostat=jj,err=995) yrang1,yrang2
10140 format("set xrange [",f10.2,":",f10.2,"]")
             write(unit,fmt=10140,iostat=jj,err=995) xrang1,xrang2

        if (nplots.gt.1) then
	     if (iplot.gt.1) then
		write (unit,fmt=10165,iostat=jj,err=995)
10165 format ('unset label')
	     endif
             write(unit,fmt=10155,iostat=jj,err=995) iplot, iplot,psize
10155 format ('set label ',i2,' "PLOT ',i2,'" at graph .01,.98 font "courier,',i2,'" front nopoint tc rgb "red"')
	     write(unit,fmt=10170,iostat=jj,err=995) 100-iplot, ncontr,iplot,isize2
10170 format ('set label ',i2,' "Col ',i2,' = ',i2,'" at graph .01,.95 font "courier,',i2,'" front nopoint tc rgb "red"')
	else
	     write(unit,fmt=10175,iostat=jj,err=995) iplot,filename(1:nfilename),psize 
10175 format ('set label ',i2,' "File: ',a,'" at graph .01,.98 font "courier,',i2,'" front nopoint tc rgb "red"')

        endif


        if (com1def.eq.0) then
            ii = nplots + 1
            ncom1=index(comment1,'    ') - 1
	    write(unit,fmt=10200,iostat=jj,err=995) ii,comment1(1:ncom1),psize 
10200 format ('set label ',i2,'" ',a,'" at graph .36,.90 font "ariel,',i2,'" front nopoint tc rgb "blue"' )
	endif

        if (com2def.eq.0) then
	    ii = nplots + 2
            ncom2=index(comment2,'    ') - 1
            write(unit,fmt=10205,iostat=jj,err=995) ii,comment2(1:ncom2),psize
10205 format ('set label ',i2,'" ',a,'" at graph .36,.86 font "ariel,',i2,'" front nopoint tc rgb "blue"' )
        endif

        if (com3def.eq.0) then
	    ii = nplots + 3
            ncom3=index(comment3,'    ') - 1
            write(unit,fmt=10210,iostat=jj,err=995) ii,comment3(1:ncom3),psize
10210 format ('set label ',i2,'" ',a,'" at graph .36,.82 font "ariel,',i2,'" front nopoint tc rgb "blue"' )
        endif

	return
995   continue
        if (unit.eq.97) then
            call xvmessage('??E - write_gpi2_2: Error opening/writing gnuplot eps file',' ')
            call abend
        else
            call xvmessage('??E - write_gpi2_2: Error opening/writing gnuplot file',' ')
            call abend
        endif

	end
C******************************************************************************
	subroutine write_pts_gpi_1(unit,mycol,tbl,ntbl,ncolx,ncoly,
     1 ycolstr,ystrl,asymtyp,asyml,lintyp,pttype,freq,ptcolor,ptcolorl)
c
c	write out data in gpi form for pts, lines or linepoints
c	for a 1 value in control column
c
	implicit none
	integer*4 unit,mycol,ncolx,ntbl,asyml(20),ystrl(20)
	integer*4 gcol,jj,gycol
	integer*4 ncoly(20),lintyp(20),pttype(20),FREQ(20),ptcolorl(20)
	character*8 ptcolor(20)
	character*20 ycolstr(20),asymtyp(20)
	character*80 tbl
	character*100 outline
        character bash
        bash=achar(92)

        if (mycol .eq. 1) then
           gcol = 1
           gycol = gcol + 1
c  decide on pts and linepoints vs, lines only
	   if (asymtyp(gcol)(1:asyml(gcol)).eq."lines") then
10350 format("plot '",a,"' u ",i2,":",i2," t '",a,"' w ",a," lt ",i2,
     1 " lc rgb '",a,"'")
	       write(unit,fmt=10350,iostat=jj,err=995) tbl(1:ntbl),ncolx,ncoly(gcol),ycolstr(gcol)(1:ystrl(gcol)),
     1 asymtyp(gcol)(1:asyml(gcol)),lintyp(gcol),ptcolor(gcol)(1:ptcolorl(gcol))
	   else
10250 format("plot '",a,"' u ",i2,":",i2," t '",a,"' w ",a," lt ",i2,
     1 " pt ",i2," ps 2 pi ",i2," lc rgb '",a,"'")
               write(unit,fmt=10250,iostat=jj,err=995) tbl(1:ntbl),ncolx,ncoly(gcol),ycolstr(gcol)(1:ystrl(gcol)),
     1 asymtyp(gcol)(1:asyml(gcol)),lintyp(gcol),pttype(gcol),freq(gcol),ptcolor(gcol)(1:ptcolorl(gcol))

	   endif
        ELSEIF (mycol .eq. 2) then

           gcol = 1
           gycol = gcol + 1

	   if (asymtyp(gcol)(1:asyml(gcol)).eq."lines") then
c     terminated with bash
10351 format ("plot '",a,"' u ",i2,":",i2," t '",a,"' w ",a," lt ",i2,
     1 " lc rgb '",a,"', ",a)
           write(unit,fmt=10351,iostat=jj,err=995) tbl(1:ntbl),ncolx,ncoly(gcol),
     1 ycolstr(gcol)(1:ystrl(gcol)),asymtyp(gcol)(1:asyml(gcol)),lintyp(gcol),
     2 ptcolor(gcol)(1:ptcolorl(gcol)),bash

	   else	
c     terminated with bash
10251 format("plot '",a,"' u ",i2,":",i2," t '",a,"' w ",a," lt ",i2,
     1 " pt ",i2," ps 2 pi ",i2," lc rgb '",a,"', ",a)
           write(unit,fmt=10251,iostat=jj,err=995) tbl(1:ntbl),ncolx,ncoly(gcol),
     1 ycolstr(gcol)(1:ystrl(gcol)),asymtyp(gcol)(1:asyml(gcol)),lintyp(gcol),
     2 pttype(gcol),freq(gcol),ptcolor(gcol)(1:ptcolorl(gcol)),bash

	   endif

           gcol = 2
           gycol = gcol + 1
           if (asymtyp(gcol)(1:asyml(gcol)).eq."lines") then 
10352 format (" '",a,"' u ",i2,":",i2," t '",a,"' w ",a," lt ",i2,
     1 " lc rgb '",a,"'")
	       write(unit,fmt=10352,iostat=jj,err=995) tbl(1:ntbl),ncolx,ncoly(gcol),
     1 ycolstr(gcol)(1:ystrl(gcol)),asymtyp(gcol)(1:asyml(gcol)),lintyp(gcol),
     2 ptcolor(gcol)(1:ptcolorl(gcol))

	   else

10252 format (" '",a,"' u ",i2,":",i2," t '",a,"' w ",a," lt ",i2,
     1 " pt ",i2," ps 2 pi ",i2," lc rgb '",a,"'")
              write(unit,fmt=10252,iostat=jj,err=995) tbl(1:ntbl),ncolx,ncoly(gcol),
     1 ycolstr(gcol)(1:ystrl(gcol)),asymtyp(gcol)(1:asyml(gcol)),lintyp(gcol),pttype(gcol),
     2 freq(gcol),ptcolor(gcol)(1:ptcolorl(gcol))
	   endif

        ELSEIF (mycol .gt. 2) then

           gcol = 1
           gycol = gcol + 1
	   if (asymtyp(gcol)(1:asyml(gcol)).eq."lines") then
	      write(unit,fmt=10351,iostat=jj,err=995) tbl(1:ntbl),ncolx,ncoly(gcol),
     1 ycolstr(gcol)(1:ystrl(gcol)),asymtyp(gcol)(1:asyml(gcol)),lintyp(gcol),
     2 ptcolor(gcol)(1:ptcolorl(gcol)),bash
	   else
              write(unit,fmt=10251,iostat=jj,err=995) tbl(1:ntbl),ncolx,ncoly(gcol),
     1 ycolstr(gcol)(1:ystrl(gcol)),asymtyp(gcol)(1:asyml(gcol)),lintyp(gcol),pttype(gcol),
     2 freq(gcol),ptcolor(gcol)(1:ptcolorl(gcol)),bash
	   endif

           do gcol=2,mycol-1
               gycol = gcol + 1
	       if (asymtyp(gcol)(1:asyml(gcol)).eq."lines") then
c     terminated with bash
10353 format (" '",a,"' u ",i2,":",i2," t '",a,"' w ",a," lt ",i2,
     1 " lc rgb '",a,"', ",a)
           write(unit,fmt=10353,iostat=jj,err=995) tbl(1:ntbl),ncolx,ncoly(gcol),
     1 ycolstr(gcol)(1:ystrl(gcol)),asymtyp(gcol)(1:asyml(gcol)),lintyp(gcol),
     2 ptcolor(gcol)(1:ptcolorl(gcol)),bash
	       else
	   
c     terminated with bash
10253 format (" '",a,"' u ",i2,":",i2," t '",a,"' w ",a," lt ",i2,
     1 " pt ",i2," ps 2 pi ",i2," lc rgb '",a,"', ",a)
           write(unit,fmt=10253,iostat=jj,err=995) tbl(1:ntbl),ncolx,ncoly(gcol),
     1 ycolstr(gcol)(1:ystrl(gcol)),asymtyp(gcol)(1:asyml(gcol)),lintyp(gcol),pttype(gcol),
     2 freq(gcol),ptcolor(gcol)(1:ptcolorl(gcol)),bash
	      endif
           enddo

           gcol = mycol
           gycol = gcol + 1
	   if (asymtyp(gcol)(1:asyml(gcol)).eq."lines") then
	       write(unit,fmt=10352,iostat=jj,err=995) tbl(1:ntbl),ncolx,ncoly(gcol),
     1 ycolstr(gcol)(1:ystrl(gcol)),asymtyp(gcol)(1:asyml(gcol)),lintyp(gcol),
     2 ptcolor(gcol)(1:ptcolorl(gcol))
	   else
               write(unit,fmt=10252,iostat=jj,err=995) tbl(1:ntbl),ncolx,ncoly(gcol),
     1 ycolstr(gcol)(1:ystrl(gcol)),asymtyp(gcol)(1:asyml(gcol)),lintyp(gcol),pttype(gcol),
     2 freq(gcol),ptcolor(gcol)(1:ptcolorl(gcol))
	   endif

        ENDIF !if (mycol .eq. 1) 

	return

995	continue
        if (unit.eq.97) then
            call xvmessage('??E - write_pts_gpi_1: Error writing gnuplot eps file',' ')
	    write (outline,19999) jj
19999 format ('iostat = ',i8)
	    call xvmessage (outline,' ')
            call abend
        else
            call xvmessage('??E - write_pts_gpi_1: Error writing gnuplot file',' ')
	    write (outline,19999) jj
	    call xvmessage (outline,' ')
            call abend
        endif

	return
	end
C******************************************************************************
        subroutine write_pts_gpi_2(unit,mycol,tbl,ntbl,ncolx,ncoly,
     1 ycolstr,ystrl,asymtyp,asyml,lintyp,pttype,freq,ptcolor,ptcolorl,
     2 ncontr,iplot)
c
c       write out data in gpi form for pts, lines or linepoints
c       for a 2 or higher value in control column
c
        implicit none
        integer*4 unit,mycol,ncolx,ntbl,asyml(20),ystrl(20)
        integer*4 gcol,jj,gycol,ncontr,iplot
        integer*4 ncoly(20),lintyp(20),pttype(20),FREQ(20),ptcolorl(20)
	character*3 ascont,asncoly
        character*8 ptcolor(20)
        character*20 ycolstr(20),asymtyp(20)
        character*80 tbl
        character*100 outline
        character bash
        bash=achar(92)

	if (ncontr < 10) then
	    write (ascont,19000) ncontr
19000 format ("$",i1)
	else
	    write (ascont,19001) ncontr
19001 format ("$",i2)
	endif
	 
        if (mycol .eq. 1) then
           gcol = 1
           gycol = gcol + 1

           if (ncoly(gcol) < 10) then
             write (asncoly,19000) ncoly(gcol)
           else
             write (asncoly,19001) ncoly(gcol)
           endif

c  decide on pts and linepoints vs, lines only
c
c  plot 'file' using 1:($3>10 ? $2 : 1/0)
c
c	gnuplot command for plotting against a control column
c   plot 'file' using 1:($4 == 2 ? $2 : 1/0) t 
c
            if (asymtyp(gcol)(1:asyml(gcol)).eq."lines") then
10350 format("plot '",a,"' u ",i2,":(",a," == ",i2," ? ",a," : 1/0) t '",a,"' w ",a," lt ",i2,
     1 " lc rgb '",a,"'")
                write(unit,fmt=10350,iostat=jj,err=995) tbl(1:ntbl),ncolx,ascont,iplot,asncoly,
     1 ycolstr(gcol)(1:ystrl(gcol)),asymtyp(gcol)(1:asyml(gcol)),lintyp(gcol),
     2 ptcolor(gcol)(1:ptcolorl(gcol))
            else
10250 format("plot '",a,"' u ",i2,":(",a," == ",i2," ? ",a," : 1/0) t '",a,"' w ",a," lt ",i2,
     1 " pt ",i2," ps 2 pi ",i2," lc rgb '",a,"'")
                write(unit,fmt=10250,iostat=jj,err=995) tbl(1:ntbl),ncolx,ascont,iplot,asncoly,
     1 ycolstr(gcol)(1:ystrl(gcol)),asymtyp(gcol)(1:asyml(gcol)),lintyp(gcol),pttype(gcol),
     2 freq(gcol),ptcolor(gcol)(1:ptcolorl(gcol))

            endif
        ELSEIF (mycol .eq. 2) then

           gcol = 1
           gycol = gcol + 1
           if (ncoly(gcol) < 10) then
              write (asncoly,19000) ncoly(gcol)
           else
              write (asncoly,19001) ncoly(gcol)
           endif

           if (asymtyp(gcol)(1:asyml(gcol)).eq."lines") then
c     terminated with bash
10351 format ("plot '",a,"' u ",i2,":(",a," == ",i2," ? ",a," : 1/0) t '",a,"' w ",a," lt ",i2,
     1 " lc rgb '",a,"', ",a)
           write(unit,fmt=10351,iostat=jj,err=995) tbl(1:ntbl),ncolx,ascont,iplot,asncoly,
     1 ycolstr(gcol)(1:ystrl(gcol)),asymtyp(gcol)(1:asyml(gcol)),lintyp(gcol),
     2 ptcolor(gcol)(1:ptcolorl(gcol)),bash

           else
c     terminated with bash
10251 format ("plot '",a,"' u ",i2,":(",a," == ",i2," ? ",a," : 1/0) t '",a,"' w ",a," lt ",i2,
     1 " pt ",i2," ps 2 pi ",i2," lc rgb '",a,"', ",a)
           write(unit,fmt=10251,iostat=jj,err=995) tbl(1:ntbl),ncolx,ascont,iplot,asncoly,
     1 ycolstr(gcol)(1:ystrl(gcol)),asymtyp(gcol)(1:asyml(gcol)),lintyp(gcol),
     2 pttype(gcol),freq(gcol),ptcolor(gcol)(1:ptcolorl(gcol)),bash

           endif

           gcol = 2
           gycol = gcol + 1
           if (ncoly(gcol) < 10) then
              write (asncoly,19000) ncoly(gcol)
           else
              write (asncoly,19001) ncoly(gcol)
           endif

           if (asymtyp(gcol)(1:asyml(gcol)).eq."lines") then
10352 format (" '",a,"' u ",i2,":(",a," == ",i2," ? ",a," : 1/0) t '",a,"' w ",a," lt ",i2,
     1 " lc rgb '",a,"'")
               write(unit,fmt=10352,iostat=jj,err=995) tbl(1:ntbl),ncolx,ascont,iplot,asncoly,
     1 ycolstr(gcol)(1:ystrl(gcol)),asymtyp(gcol)(1:asyml(gcol)),lintyp(gcol),
     2 ptcolor(gcol)(1:ptcolorl(gcol))

           else
10252 format (" '",a,"' u ",i2,":(",a," == ",i2," ? ",a," : 1/0) t '",a,"' w ",a," lt ",i2,
     1 " pt ",i2," ps 2 pi ",i2," lc rgb '",a,"'")
               write(unit,fmt=10252,iostat=jj,err=995) tbl(1:ntbl),ncolx,ascont,iplot,asncoly,
     1 ycolstr(gcol)(1:ystrl(gcol)),asymtyp(gcol)(1:asyml(gcol)),lintyp(gcol),pttype(gcol),
     2 freq(gcol),ptcolor(gcol)(1:ptcolorl(gcol))
           endif
  
       ELSEIF (mycol .gt. 2) then

           gcol = 1
           gycol = gcol + 1
           if (ncoly(gcol) < 10) then
              write (asncoly,19000) ncoly(gcol)
           else
              write (asncoly,19001) ncoly(gcol)
           endif
           if (asymtyp(gcol)(1:asyml(gcol)).eq."lines") then
               write(unit,fmt=10351,iostat=jj,err=995) tbl(1:ntbl),ncolx,ascont,iplot,asncoly,
     1 ycolstr(gcol)(1:ystrl(gcol)),asymtyp(gcol)(1:asyml(gcol)),lintyp(gcol),
     2 ptcolor(gcol)(1:ptcolorl(gcol)),bash
           else
               write(unit,fmt=10251,iostat=jj,err=995) tbl(1:ntbl),ncolx,ascont,iplot,asncoly,
     1 ycolstr(gcol)(1:ystrl(gcol)),asymtyp(gcol)(1:asyml(gcol)),lintyp(gcol),pttype(gcol),
     2 freq(gcol),ptcolor(gcol)(1:ptcolorl(gcol)),bash
           endif

           do gcol=2,mycol-1
               if (ncoly(gcol) < 10) then
                  write (asncoly,19000) ncoly(gcol)
               else
                  write (asncoly,19001) ncoly(gcol)
               endif
               gycol = gcol + 1
               if (asymtyp(gcol)(1:asyml(gcol)).eq."lines") then
c     terminated with bash
10353 format (" '",a,"' u ",i2,":(",a," == ",i2," ? ",a," : 1/0) t '",a,"' w ",a," lt ",i2,
     1 " lc rgb '",a,"', ",a)
           write(unit,fmt=10353,iostat=jj,err=995) tbl(1:ntbl),ncolx,ascont,iplot,asncoly,
     1 ycolstr(gcol)(1:ystrl(gcol)),asymtyp(gcol)(1:asyml(gcol)),lintyp(gcol),
     2 ptcolor(gcol)(1:ptcolorl(gcol)),bash
              else
c     terminated with bash
10253 format (" '",a,"' u ",i2,":(",a," == ",i2," ? ",a," : 1/0) t '",a,"' w ",a," lt ",i2,
     1 " pt ",i2," ps 2 pi ",i2," lc rgb '",a,"', ",a)
           write(unit,fmt=10253,iostat=jj,err=995) tbl(1:ntbl),ncolx,ascont,iplot,asncoly,
     1 ycolstr(gcol)(1:ystrl(gcol)),asymtyp(gcol)(1:asyml(gcol)),lintyp(gcol),pttype(gcol),
     2 freq(gcol),ptcolor(gcol)(1:ptcolorl(gcol)),bash
              endif
           enddo

           gcol = mycol
           gycol = gcol + 1
           if (ncoly(gcol) < 10) then
               write (asncoly,19000) ncoly(gcol)
           else
               write (asncoly,19001) ncoly(gcol)
           endif
           if (asymtyp(gcol)(1:asyml(gcol)).eq."lines") then
               write(unit,fmt=10352,iostat=jj,err=995) tbl(1:ntbl),ncolx,ascont,iplot,asncoly,
     1 ycolstr(gcol)(1:ystrl(gcol)),asymtyp(gcol)(1:asyml(gcol)),lintyp(gcol),pttype(gcol),
     2 ptcolor(gcol)(1:ptcolorl(gcol))
           else
               write(unit,fmt=10252,iostat=jj,err=995) tbl(1:ntbl),ncolx,ascont,iplot,asncoly,
     1 ycolstr(gcol)(1:ystrl(gcol)),asymtyp(gcol)(1:asyml(gcol)),lintyp(gcol),pttype(gcol),
     2 freq(gcol),ptcolor(gcol)(1:ptcolorl(gcol))
           endif

        ENDIF !if (mycol .eq. 1) 

        return

995     continue
        if (unit.eq.97) then
            call xvmessage('??E - write_pts_gpi_2: Error writing gnuplot eps file',' ')
            write (outline,19999) jj
19999 format ('iostat = ',i8)
            call xvmessage (outline,' ')
            call abend
        else
            call xvmessage('??E - write_pts_gpi_2: Error writing gnuplot file',' ')
            write (outline,19999) jj
            call xvmessage (outline,' ')
            call abend
        endif

        return
        end
c*************************************************************************
	subroutine ibis2asc (ibis_in,ncol,clen,ibisrows,ibiscols,tbl,ntbl)
c
c	extract data from ibis file and put into an ascii table for .gpi file
c
	integer*4 ibis_in,ncol,clen,ibisrows,ibiscols
	integer*4 ntbl,i,j,jj
	real*4 array(ibisrows,ibiscols)
	character*80 tbl,outline
c	
	do i=1,ncol
           call ibis_column_read(ibis_in,array(1,i),i,1,clen,status)
           if (status .ne. 1) call ibis_signal_u(runit,status,1)
	enddo

	open(13,file=tbl(1:ntbl),status='UNKNOWN',iostat=jj,err=995)
	    do i=1,clen
	    write (13,10100) (array(i,j), j=1,ncol)
10100 format (20f10.3,1x)
	    enddo

	close (13)
	return
995	continue
        call xvmessage('??E - ibis2asc: Error writing gnuplot data file',' ')
        write (outline,19999) jj
19999 format ('iostat = ',i8)
            call xvmessage (outline,' ')
            call abend
	
	return
	end

