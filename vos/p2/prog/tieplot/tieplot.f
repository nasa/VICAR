      INCLUDE 'VICMAIN_FOR'            
      SUBROUTINE MAIN44
C  IBIS MOSAIC TIEPOINT PLOTTING ROUTINE   A. ZOBRIST
C		REVISED		K.F. EVANS  MARCH 1986
C   7-10-95  JCT   (CRI) MSTP S/W CONVERSION (VICAR PORTING)
C
c	unit		file
c	13		gpi data file
c	97		gpi eps file
c	98		gpu file
c	INP		ibis tiepoint file
c
	implicit none
c      IMPLICIT  INTEGER(A-Z)
	integer*4 ibisrows,ibiscols
	parameter (ibisrows = 2000)
	parameter (ibiscols = 20)
        INTEGER*4   DUMMY, NL,NS,i, jj,isize
	integer*4 icount,idef,ncol,nframes
	integer*4 nplotgpi,ntitle,nxlabel,nylabel,nplotname
	integer*4 nplotgpi2,nploteps,ninpfile,ntbl,mycol
	integer*4 plotht,plotwid,nplots,iplot,ncontr
	integer*4 clen,symb,lastpt,nltitle,nmsg
	integer*4 numcol,charcol
        integer*4 ibis_in,runit, status
	INTEGER*4 FRAME(20),NOCOL(2),NNCOL(2)
	INTEGER*4 NPOINT(400), NSTART(400),asyml(20)
        REAL*4    FSCALE
	REAL*4	  XRANG1,XRANG2, YRANG1,YRANG2
	REAL*4    OLDCOL(ibisrows,2),NEWCOL(ibisrows,2),CONTRL(100000)
	logical*4 epsplot
	character*4 gpi/'.gpi'/,eps/'.eps'/,asc/'.asc'/

	character*8 pformat
        CHARACTER*16 ylabel,xlabel    
	character*20 asymtyp(20)
        CHARACTER*32 MSG
	CHARACTER*80 LTITLE
        CHARACTER*30 TITLE
	CHARACTER*63 plotname
	character*80 plotgpi,plotgpi2,ploteps,tbl
	character*100 cbuf,inpfile,outline
C
	include 'gnuplotchar'
c
	do i=1,400
	   NPOINT(i) = 0
	   NSTART(i) = 0
	enddo
      CALL IFMESSAGE('TIEPLOT version 02 Sep 2013 - rjb (64-bit) gnuplot')
      CALL XVEACTION('SA',' ')

      YLABEL = 'LINE'
	 nylabel=index(ylabel,'  ') - 1	
      XLABEL = 'SAMPLE'
	nxlabel=index(xlabel,'  ') - 1
      TITLE = 'TIEPOINT PLOT - ' 
	ntitle=index(title,'   ') - 1
c      TITLE2 = 'FRAME  X  SCALE  XXXX'

      CALL XVP ('NL', NL, DUMMY)			!units for Y-axis (lines)
      CALL XVP ('NS', NS, DUMMY)			!units for X-axis (samples)

      CALL XVP ('KEYCOL',NCONTR,DUMMY)                          !ok
      CALL XVP ('KEY',FRAME,NFRAMES)                     !control column numbers to be used

	nplots=nframes

      CALL XVP ('OLDCOLS', NOCOL, DUMMY)                        !ok
      CALL XVP ('NEWCOLS', NNCOL, DUMMY)                        !ok

C        'PLOTOUT'
        epsplot = .false.
        nplotgpi = 0
        nplotgpi2 = 0
        nplotname = 0
        nploteps = 0

      CALL XVPARM('PLOTOUT',cbuf,ICOUNT,IDEF,1)
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

C     Open IBIS file for 'read'

C       open file and get characteristics
        CALL XVUNIT(runit,'INP',1,status,' ')
        if (status .ne. 1) then
                call xvmessage('??E - Error on ibis unit call',' ')
                call mabend('??E - Program terminated',' ')
        endif
       call ibis_file_open(runit,ibis_in,'read',0,0,' ',' ',status)
       if (status .ne. 1) call ibis_signal_u(runit,status,1)
       call ibis_file_get(ibis_in,'nc',ncol,1,1)
       call ibis_file_get(ibis_in,'nr',CLEN,1,1)

c	print *,'clen,ncol = ',clen,ncol
	if (clen .gt. ibisrows) then
	    write (outline,10100) clen,ibisrows
10100 format ('??E More rows in ibis file ',i4,' than ',i4,' can be stored')
  	    call xvmessage(outline,' ')
	    call abend
	endif

	call ibis2asc (ibis_in,ncol,clen,ibisrows,ibiscols,tbl,ntbl)

       call ibis_column_read(ibis_in,contrl,ncol,1,clen,status)
       if (status .ne. 1) call ibis_signal_u(runit,status,1)
c       find out how many different numbers in control column
c       starting lines and lengths
      CALL FINDCO(ibis_in,CONTRL,NCONTR,CLEN,NSTART,NPOINT,NPLOTS)
c       contrl(100000), nstart(400), npoint(400)
c
c	print *, 'ncontr,clen,nplots = ',ncontr,clen,nplots
	
	call xvparm('INP',cbuf,icount,idef,1)
	  inpfile = cbuf
	  ninpfile=index(inpfile,'   ') - 1

c	tbl = inpfile(1:ninpfile)//asc
c       ntbl = index(tbl,'  ') - 1

      CALL XVP ('NUMCOL', NUMCOL, DUMMY)			!, NUMBDF,7)
      CALL XVP ('CHARCOL', CHARCOL, DUMMY)			!, CHARDF,7)
      CALL XVP ('ICHAR', SYMB, DUMMY)				!, ICHADF,1)
	fscale=1.0
      CALL XVP ('SCALE', FSCALE, DUMMY)
c      IF (KEYCDF.EQ.1)  NPLOTS = 1
C
	mycol=4		!sum of ocol and ncol
C--  GET THE SIZE OF THE PLOT.
c       default
        plotwid =  648  !640 @72dpi = 8.888.. inches    9 inch = 648
        plotht  =  648  !480 @72dpi = 6.666.. inches    7 inch = 504

	isize = 10	!font size
	open(98,file=plotgpi(1:nplotgpi),status='UNKNOWN',iostat=jj,err=995)
	if (epsplot) then
           open(97,file=plotgpi2(1:nplotgpi2),status='UNKNOWN',iostat=jj,err=996)
	endif
c

c      call plotfn (postscriptfilename)
c      call xrtbegin (status) 
c      if (status .ne. 1) goto 9999
c      CALL HEADER (HEADERTITLE, 2, 1)
c      CALL AXESTITLES (XAXISTITLE, YAXISTITLE, 270, ' ', 0)
c      CALL AXESREVERSE (0, 1)  ! Set X normal(0), set Y Reverse(1)

          DO I = 1,NPLOTS
c            symtyp(i) = 1
            asymtyp(i) = "points"
            asyml(i)=index(asymtyp(i),'   ') - 1
          ENDDO


C     Open IBIS record for read
c      CALL IBIS_RECORD_OPEN (IBIS_RD,IBIS_RECORD,' ',
c     &                       dcol,7,'REAL',STATUS)
c      IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(UNIT,STATUS,1)

      DO 100 IPLOT = 1, NPLOTS
	     WRITE (msg, '(A,I4,A,F6.1)') 'FRAME',IPLOT,'  SCALE ',FSCALE 
	     nmsg = index(msg,'       ') - 1
	     write (ltitle,10020) title(1:ntitle),msg(1:nmsg)
10020 format (a,' ',a)
	     nltitle = index(ltitle,'       ') - 1
c         CALL HEADER (HEADERTITLE, 2, 1)
c         call setactiveset (1)
         call ibis_column_read(ibis_in,oldcol(1,1),nocol(1),1,clen,status)
         if (status .ne. 1) call ibis_signal_u(ibis_in,status,1)
         call ibis_column_read(ibis_in,oldcol(1,2),nocol(2),1,clen,status)
         if (status .ne. 1) call ibis_signal_u(ibis_in,status,1)
         call ibis_column_read(ibis_in,newcol(1,1),nncol(1),1,clen,status)
         if (status .ne. 1) call ibis_signal_u(ibis_in,status,1)
         call ibis_column_read(ibis_in,newcol(1,2),nncol(2),1,clen,status)
         if (status .ne. 1) call ibis_signal_u(ibis_in,status,1)




         LASTPT = NPOINT(IPLOT)

c       write constant part of gpi
        if (iplot.eq.1) then
            call write_gpi_1(98,tbl,ntbl,ylabel,nylabel,
     1 xlabel,nxlabel,isize,plotwid,plotht,nplots,iplot,
     2 ploteps,nploteps)

	endif

c       write variable part
           call write_gpi_tp(98,ltitle,nltitle,xrang1,xrang2,
     1 yrang1,yrang2,mycol,nplots,iplot,fscale,nl,ns,nocol,nncol,
     2 oldcol,newcol,nstart,npoint,ncontr,isize,inpfile,ninpfile)

c need logic if iframes = 1 and key .ne. 1
c and if iframes gt 1  key = 1 is not in key list - TEST 4

	if (iplot.eq.1) then
	   call write_ls_gpi_1(98,tbl,ntbl,nocol,nncol,oldcol,newcol,
     1 iplot,fscale,asymtyp,asyml,lntype,pttype,ptcolor,ptcolorl)

	else

           call write_ls_gpi_2(98,tbl,ntbl,nocol,nncol,oldcol,newcol,
     1 iplot,fscale,asymtyp,asyml,lntype,pttype,ptcolor,ptcolorl,
     2 ncontr)

	endif

C EPSPLOT ---
c
        if (epsplot) then
        	if (iplot.eq.1) then
            call write_gpi_1(97,tbl,ntbl,ylabel,nylabel,
     1 xlabel,nxlabel,isize,plotwid,plotht,nplots,iplot,
     2 ploteps,nploteps)

        	endif
c       write variable part
           call write_gpi_tp(97,ltitle,nltitle,xrang1,xrang2,
     1 yrang1,yrang2,mycol,nplots,iplot,fscale,nl,ns,nocol,nncol,
     2 oldcol,newcol,nstart,npoint,ncontr,isize,inpfile,ninpfile)

        	if (iplot.eq.1) then
           call write_ls_gpi_1(97,tbl,ntbl,nocol,nncol,oldcol,newcol,
     1 iplot,fscale,asymtyp,asyml,lntype,pttype,ptcolor,ptcolorl)
c
        	else
           call write_ls_gpi_2(97,tbl,ntbl,nocol,nncol,oldcol,newcol,
     1 iplot,fscale,asymtyp,asyml,lntype,pttype,ptcolor,ptcolorl,
     2 ncontr)

        	endif

	endif
100   CONTINUE
C
c9999  continue
      CALL IBIS_FILE_CLOSE (ibis_in,' ',STATUS)

	close (98)
	if (epsplot) then
		close (97)
	endif

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
C******************************************************
        SUBROUTINE FINDCO (ibis_in,CONTRL,NCONTR,LENGTH,NSTART,
     *               NPOINT,NSETS)
        implicit none
        INTEGER*4 NPOINT(1), NSTART(1)
        INTEGER*4 ibis_in,status,npt,nelem,l,length,nsets,ncontr
C       REAL CONTRL(100000)
        REAL*4 CONTRL(1)
c       DIMENSION COLUMN(1)
C       NROWS = (LENGTH-1)/128+1
C       NRECTR = NROWS*(NCONTR-1)+2
C
C---- READ CONTROL COLUMN AND FIND START POINTS AND LENGTHS
C     FOR THE SETS.
C
C       CALL GETCOL (UNIT,NCONTR,LENGTH,CONTRL)
C       call ibis_column_read(unit,ncontr,length,contrl,status)
        call ibis_column_read(ibis_in,contrl,ncontr,1,length,status)
        if (status .ne. 1) call ibis_signal_u(ibis_in,status,1)
        NSTART(1) = 1
        NPT = 0
        NSETS = 0
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
C*********************************************************************
	subroutine write_gpi_1(unit,tbl,ntbl,ylabel,nylabel,
     1 xlabel,nxlabel,isize,plotwid,plotht,nplots,iplot,
     2 eps,neps)
c
c       write out the first part of gpi form
c
        implicit none
        integer*4 unit,isize,plotwid,plotht
        integer*4 jj,nplots,iplot,psize
        integer*4 ntbl,nylabel,nxlabel,neps
        character*16 ylabel,xlabel
        character*80 tbl,eps

c
        psize=isize
        if (unit .eq.97) psize = 16

10100 format('# Created by program tieplot')              !#'s are ignored in gnuplot
            write(unit,fmt=10100,iostat=jj,err=995)
10105 format('# Gnuplot commands for tiepoints plot')
            write(unit,fmt=10105,iostat=jj,err=995)
10110 format('# Data in ',a)
            write(unit,fmt=10110,iostat=jj,err=995) tbl(1:ntbl)

        if (unit.eq.97) then
10300 format('set terminal postscript eps enhanced color "Ariel" ',i2,'  size 11 ,8')
           write(unit,fmt=10300,iostat=jj,err=995), psize
10305 format("set output '",a,"'")
           write(unit,fmt=10305,iostat=jj,err=995) eps(1:neps)
        else
C size is XX,YY
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
        subroutine write_gpi_tp(unit,title,ntitle,xrang1,xrang2,
     1 yrang1,yrang2,mycol,nplots,iplot,fscale,nl,ns,nocol,nncol,
     2 oldcol,newcol,nstart,npoint,ncontr,isize,filename,nfilename)
c
c       write out the repeatable descriptive part of gpi form
c       for first, or only, plot
c
        implicit none
        integer*4 unit,lastpt,firstpt,nl,ns,isize
        integer*4 nplots,iplot,mycol,i,jj,psize
        integer*4 ntitle,ncontr,nfilename
        integer*4 npoint(1), nstart(1)
        real*4 lcolmin,lcolmax,scolmin,scolmax,xmin,xmax,ymin,ymax
        real*4 xrang1,xrang2,yrang1,yrang2,fscale
	real*4 oldcol(2000,2),newcol(2000,2)
        integer*4 nocol(2),nncol(2),tcol(4)
        real*4 arrmin,arrmax
	character*80 title
        character*100  filename
c
        psize=isize
        if (unit .eq.97) psize = 16

        lcolmin = 0.0
        lcolmax = 0.0
	scolmin = 0.0
        scolmax = 0.0

        ymin = 1.E+10
        ymax = -1.E+10
	xmin = 1.E+10
        xmax = -1.E+10

        firstpt = nstart(iplot)
        lastpt = nstart(iplot) + npoint(iplot) - 1
c
c       the data set
c

10145 format('set title "',a,'" font "Ariel,',i2,'"')
             write(unit,fmt=10145,iostat=jj,err=995) title(1:ntitle),psize
	
	tcol(1) = nocol(1)	!line	
	tcol(2) = nncol(1)	!line
	tcol(3) = nocol(2)	!sample
	tcol(4) = nncol(2)	!sample
c	find line extremes	

	do i=1,2
                LCOLMIN = ARRMIN(OLDCOL(1,i),FIRSTPT,LASTPT)
c		LCOLMIN = ARRMIN(
                LCOLMAX = ARRMAX(OLDCOL(1,i),FIRSTPT,LASTPT)
                IF (YMIN .GT. LCOLMIN) YMIN=LCOLMIN
                IF (YMAX .LT. LCOLMAX) YMAX=LCOLMAX
	enddo
c	find sample extremes
        do i=1,2
c                COL(1) = NEWCOL(1,i)
                SCOLMIN = ARRMIN(NEWCOL(1,i),FIRSTPT,LASTPT)
                SCOLMAX = ARRMAX(NEWCOL(1,i),FIRSTPT,LASTPT)
                IF (XMIN .GT. SCOLMIN) XMIN=SCOLMIN
                IF (XMAX .LT. SCOLMAX) XMAX=SCOLMAX
        enddo
	
	yrang1 = ymin
        yrang2 = ymax
        xrang1 = xmin
        xrang2 = xmax
	if (nl .gt. ymax) yrang2 = nl
	if (ns .gt. xmax) xrang2 = ns
C
C---- SET THE SCALING PARAMETERS.
C
c	set y range to be like vicar image - lines counting downward from top
c
10135 format("set yrange [",f10.2,":",f10.2,"]")
             write(unit,fmt=10135,iostat=jj,err=995) yrang2,yrang1	!upside down for Vicar Image
10140 format("set xrange [",f8.2,":",f8.2,"]")
             write(unit,fmt=10140,iostat=jj,err=995) xrang1,xrang2

        if (nplots.gt.1) then
             if (iplot.gt.1) then
                write (unit,fmt=10165,iostat=jj,err=995)
10165 format ('unset label')
             endif
	endif
             write(unit,fmt=10155,iostat=jj,err=995) iplot, filename(1:nfilename)
10155 format ('set label ',i2,' "',a,'" at graph .45,.98 font "courier,16" front nopoint tc rgb "red"')

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
        subroutine write_ls_gpi_1(unit,tbl,ntbl,nocol,nncol,oldcol,newcol,
     1 iplot,fscale,asymtyp,asyml,lntype,pttype,ptcolor,ptcolorl)
c
c       write out data in gpi form for pts, lines or linepoints
c       for a 1 value in control column
c
        implicit none
        integer*4 unit,ntbl,asyml(20)
        integer*4 gcol,jj,iplot,ystr,ndeltx,ndelty
	integer*4 nocol(2),nncol(2)
        integer*4 lntype(20),pttype(20),ptcolorl(20)
	real*4 fscale,oldcol(2000,2),newcol(2000,2)
	character*3 nocol1,nocol2,nncol1,nncol2
        character*8 ptcolor(20)
	character*30 deltx,delty
        character*20 ycolstr,asymtyp(20)
        character*80 tbl
        character*100 outline
        character bash
        bash=achar(92)

        if (nocol(1) < 10) then
            write (nocol1,19000) nocol(1)
19000 format ("$",i1)
        else
            write (nocol1,19001) nocol(1)
19001 format ("$",i2)
        endif

        if (nocol(2) < 10) then
            write (nocol2,19000) nocol(2)
        else
            write (nocol2,19001) nocol(2)
        endif

        if (nncol(1) < 10) then
            write (nncol1,19000) nncol(1)
        else
            write (nncol1,19001) nncol(1)
        endif

        if (nncol(2) < 10) then
            write (nncol2,19000) nncol(2)
        else
            write (nncol2,19001) nncol(2)
        endif
	write (delty,19005) nocol1,nncol1,fscale
19005 format ("(",a," - ",a,")*",f7.2)
	write (deltx,19005) nocol2,nncol2,fscale
c10143 format("set multiplot")
c            write(unit,fmt=10143,iostat=jj,err=995)
	ndeltx=index(deltx,'        ') - 1
	ndelty=index(delty,'        ') - 1

	ycolstr = 'old image'
	ystr = index(ycolstr,'  ')
c   plot 'i.dat.asc' u  2:3 t 'old image           ' w points lt 90 pt  5 ps 2  lc rgb 'green', \
c     i.dat.asc' u  4:5 t 'new image           ' w points lt  1 pt  9 ps 2  lc rgb 'magenta' , \
c     'i.dat.asc' u 2:3: 8: 9 notitle with vectors lt 1 lw 2 lc rgb 'brown' filled

C plot 'test1.dat.asc' u  2: 3 t 'old image ' w points lt 90 pt  5 ps 2  lc rgb 'green', \
C  'test1.dat.asc' u  4: 5 t 'new image ' w points lt  1 pt  9 ps 2  lc rgb 'magenta', \
C  'test1.dat.asc' u  2: 3: ($4-$2) :($5-$3) notitle with vectors lt 1 lw 2 lc rgb 'brown' filled

	gcol = 1
c     terminated with bash
10250 format("plot '",a,"' u ",i2,":",i2," t '",a,"' w ",a," lt ",i2,
     1 " pt ",i2," ps 2  lc rgb '",a,"', ",a)
        write(unit,fmt=10250,iostat=jj,err=995) tbl(1:ntbl),nocol(2),nocol(1),ycolstr(1:ystr),
     1 asymtyp(iplot)(1:asyml(iplot)),lntype(gcol),pttype(gcol),ptcolor(gcol)(1:ptcolorl(gcol)),
     2 bash
	
	ycolstr = 'new image'
	ystr = index(ycolstr,'  ')
	gcol = gcol + 1
c     terminated with bash
10252 format (" '",a,"' u ",i2,":",i2," t '",a,"' w ",a," lt ",i2,
     1 " pt ",i2," ps 2  lc rgb '",a,"', ",a)
           write(unit,fmt=10252,iostat=jj,err=995) tbl(1:ntbl),nncol(2),nncol(1),ycolstr(1:ystr),
     1 asymtyp(iplot)(1:asyml(iplot)),lntype(gcol),pttype(gcol),
     2 ptcolor(gcol)(1:ptcolorl(gcol)),bash

	
c	ncoldely = 10
c	ncoldelx = 11
	gcol = gcol + 1
10253 format (" '",a,"' u ",i2,":",i2,": (",a,") : (",a,") notitle with vectors lt 1 lw 2 lc rgb '",a,"' filled")
	   write(unit,fmt=10253,iostat=jj,err=995) tbl(1:ntbl),nocol(2),nocol(1),delty(1:ndelty),
     1 deltx(1:ndeltx),ptcolor(gcol)(1:ptcolorl(gcol))

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
C******************************************************************************
        subroutine write_ls_gpi_2(unit,tbl,ntbl,nocol,nncol,oldcol,newcol,
     1 iplot,fscale,asymtyp,asyml,lntype,pttype,ptcolor,ptcolorl,ncontr)
c
c       write out data in gpi form for pts, lines or linepoints
c       for a 1 value in control column
c
        implicit none
        integer*4 unit,ntbl,asyml(20)
        integer*4 gcol,jj,iplot,ystr,ndeltx,ndelty,ncontr
        integer*4 nocol(2),nncol(2)
        integer*4 lntype(20),pttype(20),ptcolorl(20)
        real*4 fscale,oldcol(2000,2),newcol(2000,2)
        character*3 nocol1,nocol2,nncol1,nncol2,ascont
        character*8 ptcolor(20)
        character*30 deltx,delty
        character*20 ycolstr,asymtyp(20)
        character*80 tbl
        character*100 outline
        character bash
        bash=achar(92)
c
        if (ncontr < 10) then
            write (ascont,19000) ncontr
19000 format ("$",i1)
        else
            write (ascont,19001) ncontr
19001 format ("$",i2)
        endif

        if (nocol(1) < 10) then
            write (nocol1,19000) nocol(1)
        else
            write (nocol1,19001) nocol(1)
        endif

        if (nocol(2) < 10) then
            write (nocol2,19000) nocol(2)
        else
            write (nocol2,19001) nocol(2)
        endif

        if (nncol(1) < 10) then
            write (nncol1,19000) nncol(1)
        else
            write (nncol1,19001) nncol(1)
        endif

        if (nncol(2) < 10) then
            write (nncol2,19000) nncol(2)
        else
            write (nncol2,19001) nncol(2)
        endif
        write (delty,19005) nocol1,nncol1,fscale
19005 format ("(",a," - ",a,")*",f7.2)
        write (deltx,19005) nocol2,nncol2,fscale
c10143 format("set multiplot")
c            write(unit,fmt=10143,iostat=jj,err=995)
        ndeltx=index(deltx,'        ') - 1
        ndelty=index(delty,'        ') - 1

c

       ycolstr = 'old image'
        ystr = index(ycolstr,'  ')
c
C plot 'test1.dat.asc' u  2: ($1 == 2 ? $3 : 1/0)  t 'old image ' w points lt 90 pt  5 ps 2  lc rgb 'green', \
C  'test1.dat.asc' u  4: 5 t 'new image ' w points lt  1 pt  9 ps 2  lc rgb 'magenta', \
C  'test1.dat.asc' u  2: 3: ($4-$2) :($5-$3) notitle with vectors lt 1 lw 2 lc rgb 'brown' filled

c       gnuplot command for plotting against a control column
c   plot 'file' using 1:($4 == 2 ? $2 : 1/0) t 

        gcol = 1
c     terminated with bash
10250 format("plot '",a,"' u ",i2,":(",a," == ",i2," ? ",a," : 1/0) t '",a,"' w ",a," lt ",i2,
     1 " pt ",i2," ps 2  lc rgb '",a,"', ",a)
        write(unit,fmt=10250,iostat=jj,err=995) tbl(1:ntbl),nocol(2),ascont,iplot,nocol1,
     1 ycolstr(1:ystr),asymtyp(iplot)(1:asyml(iplot)),lntype(gcol),pttype(gcol),
     1 ptcolor(gcol)(1:ptcolorl(gcol)),bash

        ycolstr = 'new image'
        ystr = index(ycolstr,'  ')
        gcol = gcol + 1
c     terminated with bash
10252 format (" '",a,"' u ",i2,":(",a," == ",i2," ? ",a," : 1/0)  t '",a,"' w ",a," lt ",i2,
     1 " pt ",i2," ps 2  lc rgb '",a,"', ",a)
           write(unit,fmt=10252,iostat=jj,err=995) tbl(1:ntbl),nncol(2),ascont,iplot,nncol1,
     1 ycolstr(1:ystr),asymtyp(iplot)(1:asyml(iplot)),lntype(gcol),pttype(gcol),
     2 ptcolor(gcol)(1:ptcolorl(gcol)),bash


        gcol = gcol + 1
10253 format (" '",a,"' u ",i2,":",i2,": (",a,") : (",a,") notitle with vectors lt 1 lw 2 lc rgb '",a,"' filled")
           write(unit,fmt=10253,iostat=jj,err=995) tbl(1:ntbl),nocol(2),nocol(1),delty(1:ndelty),
     1 deltx(1:ndeltx),ptcolor(gcol)(1:ptcolorl(gcol))
	if (unit.eq.98) then
10255 format("pause mouse any")                 !allows plot to display on screen until mouse click
           write(unit,fmt=10255,iostat=jj,err=995)
	endif

        return
995     continue
        if (unit.eq.97) then
            call xvmessage('??E - write_ls_gpi_2: Error writing gnuplot eps file',' ')
        write (outline,19999) jj
19999 format ('iostat = ',i8)
            call xvmessage (outline,' ')
            call abend
        else
            call xvmessage('??E - write_ls_gpi_2: Error writing gnuplot file',' ')
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

cc	do i=1,clen
cc		print *, 'row i, col j = ',(array(i,j), j=1,ncol)
cc	enddo

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
