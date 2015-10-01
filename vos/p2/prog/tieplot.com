$!****************************************************************************
$!
$! Build proc for MIPL module tieplot
$! VPACK Version 1.9, Friday, January 16, 2015, 11:37:11
$!
$! Execute by entering:		$ @tieplot
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   PDF         Only the PDF file is created.
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module tieplot ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Test = ""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to tieplot.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("tieplot.imake") .nes. ""
$   then
$      vimake tieplot
$      purge tieplot.bld
$   else
$      if F$SEARCH("tieplot.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake tieplot
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @tieplot.bld "STD"
$   else
$      @tieplot.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create tieplot.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack tieplot.com -mixed -
	-s tieplot.f -
	-i tieplot.imake -
	-p tieplot.pdf -
	-t tsttieplot.pdf tsttieplot.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create tieplot.f
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create tieplot.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM tieplot

   To Create the build file give the command:

		$ vimake tieplot			(VMS)
   or
		% vimake tieplot			(Unix)


************************************************************************/


#define PROGRAM	tieplot
#define R2LIB

#define MODULE_LIST tieplot.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST fortport gnuplotchar

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create tieplot.pdf
PROCESS      HELP=*
! TIEPLOT PDF - VICAR/IBIS MOSAIC SOFTWARE
! VICAR2/MIPL VERSION
PARM INP     TYPE=STRING    COUNT=1
PARM NL      TYPE=INTEGER   COUNT=1 DEFAULT=0
PARM NS      TYPE=INTEGER   COUNT=1 DEFAULT=0
PARM PLOTOUT TYPE=STRING    COUNT=(0:1) DEFAULT="tieplot"
PARM PLOTFMT TYPE=STRING    COUNT=(0:1) VALID=(GNUPLOT,EPS) DEFAULT=GNUPLOT
PARM NEWCOLS TYPE=INTEGER   COUNT=2 DEFAULT=(1,2)
PARM OLDCOLS TYPE=INTEGER   COUNT=2 DEFAULT=(3,4)
PARM NUMCOL  TYPE=INTEGER   COUNT=1 DEFAULT=1
PARM CHARCOL TYPE=INTEGER   COUNT=1 DEFAULT=1
PARM ICHAR   TYPE=INTEGER   COUNT=1 DEFAULT=0
PARM SCALE   TYPE=REAL      COUNT=1 DEFAULT=1.0
PARM KEY     TYPE=INTEGER   COUNT=(1:20) DEFAULT=0
PARM KEYCOL  TYPE=INTEGER   COUNT=1  DEFAULT=1

END-PROC
.TITLE
VICAR/IBIS Program TIEPLOT
.HELP
PURPOSE

     TIEPLOT plots tiepoints in an IBIS interface file by
     drawing vectors to indicate the direction and amount of
     shift between the old (line,sample)  new (line,sample)
     pairs.  The image area is outlined and labeled.  A number
     or symbol from the interface file may be plotted at each 
     tiepoint position.

     
TAE COMMAND LINE FORMAT

     TIEPLOT INP=A PARAMS
     A                   is an IBIS interface file.     
     PARAMS              is a standard VICAR parameter field.
.PAGE
OPERATION

     For each tiepoint that has the correct entry in the
     KEYCOL, a vector is drawn with a length proportional
     to the distance between the old (line,sample) coordinates
     and the new (line,sample) coordinates.

.PAGE
PARAMETERS
    INP  an input ibis tabular file containing the tiepoints from
two images
    NL and NS  the number of lines and samples of the image to
show the tiepoints plot   
    PLOTOUT   the name of the output plot. It will yield two
files named PLOTOUT.gpi  and PLOTOUT.asc
    PLOTFMT   The type of plot, either GNUPLOT or EPS
    NEWCOLS  are the column numbers in INP containing the tiepoints
in the new image.
    OLDCOLS are the column numbers in INP containing the tiepoints
in the old image.
    NUMCOL  (optional) is a column containing a number to be plotted
along side the tiepoint symbol.
    CHARCOL (optional)  is a column containing a number of a symbol
to be used to plot to indicate the location of a tiepoint.
    ICHAR  (optional) is a symbol number
    SCALE   is the magnification number for showing tiepoint offsets
    KEY     is which key in the key column to plot.  Each key
specifies a separate plot
    KEYCOL  is the column number to look for the key value


.PAGE
PLOT OUTPUTS

    The other type of output come from the PLOTOUT and PLOTFMT parameters.
PLOTOUT produces a file of gnuplot commands contained in a file having a .gpi
file extension. Another file with an .asc extension is created containing
columms of data that are displayed by the gpi file.

   The PLOTFMT parameter allows the user to generate a postscript file of
the output for use in documentation by choosing PLOTFMT=EPS. The default
is to generate a gnuplot interactive display.

.PAGE

  PLOT NAMING CONVENTIONS

  The user should enter only the parent file name without an extension
  for the PLOTOUT parameter.  The program will supply the extensions.

  For example, if the user has an input file of indata.dat and  PLOTOUT=outplot
  then for the interactive plot the following files are produced:

     outplot.gpi
     outplot.asc

  The first file is the gnuplot instruction file and the second is the
  data file used by gnuplot.      

  If the user wanted an encapsulate postscript file with PLOTFMT=eps
  then the following files are produced:

     outplot.eps.gpi
     outplot.asc


  Remember entering the following command gives the eps file, outplot.eps

  ush gnuplot outplot.eps.gpi

  If you move the gpi file to another directory, you must also move the
  input data file, indata.dat.asc to the same directory.

  Note that the gpi file produced by this program has the name of the
  input file embedded in the plot command inside the gpi file, e.g..

  plot  'output.asc' u  1: 9 t .......


.PAGE
USING GNUPLOT


  INTERACTIVE:

    This program uses the gnuplot package for its plots. Gnuplot is a
  separate package from Vicar and is not actually invoked inside this
  program.  Instead this program creates a template of gnuplot commands
  which are written out as a separate file. The plot is then viewed after
  exiting this program. The file has the extension .gpi. You view
  the plot by issuing the following command in the vicar shell.

  ush gnuplot output.gpi

  or external to vicar as

  gnuplot output.gpi

    After viewing the data, you close the plot by clicking the mouse anywhere
  except on the top bar of the plot. Clicking on the top bar allows you
  to move the plot to any convenient place on the terminal screen.  (While
  the plot is displayed you cannot enter any commands to the vicar shell).

  The data to be plotted by gnuplot is read from a separate file, created
  by this program, which contains columns of data in ascii text.
  File naming conventions are discussed in the OUTPUT section, but in this
  case that file extension will be .asc.

  It is possible to keep the plot alive for comparison purposes by issuing
  the following command.

  ush gnuplot --persist output.gpi
  (You will be able to enter commamds to the vicar shell after clicking on
  the mouse on the plot).

  Note: This program creates 5 output plots per run. You bring up each plot
  panel sequentially. You close each plot by clicking the mouse on any
  portion of the plot.


  HARDCOPY:

  This program also allows you to create a hardcopy encapsulated postscript
  plot suitable for publications. This file can be viewed with ghostscript
  or gimp. The encapsulated postscript file is not created by this program
  by by the gnuplot program from a gpi file made especially for this purpose.
  this file has the extension, eps.gpi. You create the hardcopy plot via
  the following command

  ush gnuplot output.eps.gpi

  This creates the eps file output.eps. You can view this file by

  ush gimp output.eps

.PAGE
    DEVELOPER Note:

    This program used to link to the XRT plot library -lxrt. Calls to
  that library were mitigated through a Calcomp conversion library,
  xrtps located in the p2 subroutine library. With the conversion to
  gnuplot, neither of these packages are used.


     
.PAGE
EXAMPLES

Tieplot tiepoints.int nl=1056 ns=1204  scale=10

In this example, just one plot will be produced since the KEYCOL parameter
has not been specified.  The default columns for the tiepoints (columns 
1, 2, 3, and 4,  same as output by PICMATCH) will be assumed.  The length
of the offset vectors will be magnified by a factor of 10, and no
symbols or numbers will label the vectors. The plot output will be 
named tieplot.


ibis-gen out=table1.dat nc=7 nr=30
mf3 inp=table1.dat func=("c1=@aint(@index/31)+1"$"c2=100*@index"$ +
    "c3=100*@index"$"c4=c2+101"$"c5=c3-200"$"c6=@aint(@index)"$"c7=0")

tieplot inp=i.dat nl=3000 ns=3000 keycol=1 key=(1,2)+
  scale=20 newcols=(4,5) oldcols=(2,3) numcol=6  +
  plotout=tieplot2

In this example an IBIS file of 7 columns and 30 rows is created,
it is divided into 2 subsets of 15 rows each (control column C1
has 1 in the first 15 rows and 2 in the next 15 rows). Old coordinates
are in columns 2 and 3, new coordinates are in columns 4 and 5.
Number of each point is contained in column 6. Two plots, each con-
taining vectors for 15 points will be generated. 

.PAGE
Original Programmer:  A. L. Zobrist       10 October 1980

Cognizant Programmer:  K. F. Evans

Revision History

 02 Sep 2013 R. J. Bambery  Fixed misspelling of lintyp vs. lntype 
 13 Jul 2013 R. J. Bambery  Adjusted eps format to more readable fonts
                            Remove vestiges of debug statments
 12 Jul 2013 R. J. Bambery  Create ascii file for .gpi file
                            Previously used ibis2asc to create in for input
                            Add PLOTFMT. Made file naming conventions
                            consistent
 20 Feb 2013 R. J. Bambery  Removed some debug print statements
                            test script enhancements
 13 Feb 2013 R. J. Bambery  Documentation and test updates
 16 Nov 2012 R. J. Bambery  Linux 64-bit, Gnuplot
  8 May 95   J. Turner (CRI)  Made portable for UNIX and XRT/graph 

 01 Mar 86   KFE  Revision 2   


.LEVEL1
.VARIABLE INP
Input IBIS interface file
.VARIABLE PLOTOUT
STRING - Output Plot file name.
Default="tieplot"
.VARIABLE PLOTFMT
 Output plot format
 GNUPLOT or EPS
.VARIABLE KEYCOL
Control column
.VARIABLE KEY
Keys in the control column
.VARIABLE SCALE
Magnification factor for shifts
.VARIABLE NL
Size of an area in lines
.VARIABLE NS
Size of an area in samples
.VARIABLE NEWCOLS
Columns of new (line,sample)
.VARIABLE OLDCOLS
Columns of old (line,sample)
.VARIABLE NUMCOL
Columns of identifying numbers
.VARIABLE CHARCOL
Column containing Calcomp
special symbol numbers
.VARIABLE ICHAR
Calcomp special symbol number
.LEVEL2
.VARIABLE INP
     INP=A               Input IBIS interface file
.VARIABLE PLOTOUT
    STRING              Output plot file name. 
                        Default if not specified is 'tieplot'
.VARIABLE PLOTFMT
    STRING              Output plot format, GNUPLOT or EPS
    Default = GNUPLOT
.VARIABLE KEYCOL
     KEYCOL=N            The  integer N specifies a  control 
                         column  for selecting a  subset  of 
                         the data for plotting.
.VARIABLE KEY
     KEY=(K1,...,KM)     The  integers  K1,...,KM  specifies 
                         which  keys  in the control  column 
                         are subsetted for plotting.
.VARIABLE SCALE
     SCALE=R             The floating decimal R specifies  a 
                         magnification   factor  to   be 
                         applied to the shifts when plotted.
.VARIABLE NL
     NL=P                The  integer P specify the size  of 
                         the area being plotted in lines. 
.VARIABLE NS
     NL=Q                The  integer Q specify the size  of 
                         the area being plotted in samples.
.VARIABLE NEWCOLS
     NEWCOLS=(A,B)       The  integers A and B  specify  the 
                         columns  of  new coordinates.
.VARIABLE OLDCOLS
     OLDCOLS=(C,D)       The  integers C and D  specify  the 
                         columns   of  old coordinates.
.VARIABLE NUMCOL
     NUMCOL=E            The  integer E specifies  a  column 
                         that  contains identifying  numbers 
                         to  be plotted beside each  vector.  
                         If this keyword is omitted, then no 
                         numbers are plotted.
.VARIABLE CHARCOL
     CHARCOL=F           The  integer  F specifies a  column 
                         that contains the number of the 
			 identifying symbol to be plotted as 
			 the base of each vector.  The numbers
			 should be in the range of 0 to 15.
                         If this keyword is omitted, then no 
                         symbols are plotted from a column.
.VARIABLE ICHAR
     ICHAR=N             The number N specifies the identifying
			 symbol to be plotted as the base of 
			 each vector.  The number should be in 
			 the range of 0 to 15.  This can be
                         used as an alternative  to  the 
                         CHAR keyword.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tsttieplot.pdf
procedure
parm    mode    type=keyword count=(0:1) valid=(batch,nobatch,inter) default=batch

local   clen    type=integer count=1
! Oct 15, 2012 - RJB
! TEST SCRIPT FOR TIEPLOT
!
! Vicar Programs:
!       ibis-gen mf3 ibis-list ibis2asc
!
! External programs
!       gnuplot 4.6.x, gimp 2.6
! 
! Parameters:
!   mode - method for processing: 
!       1) batch provides no display but creates .eps files
!       2) interactive or nobatch is used for display requiring
!       user interaction. 
!           
!   In batch mode it produces files testx.eps by calling gnuplot
!       to create the encapsulated postscript file which can be
!       later viewed with ghostscript or gimp
!   In interactive or nobatch mode gnuplot is called with a window
!       manager for X11. The gnuplot display is killed by
!       a mouse click anywhere on the plot panel
!            
! External test data: 
!       <none>
! Output:    
!   GENed test data sets, .gpi and .eps files and intermediate 
!       tmp* files 
!   the *.gpi data produced by statplt are gnuplot scripts
!
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="stop"
let $echo="yes"
ibis-gen out=table1.dat nc=7 nr=30
mf3 inp=table1.dat func=("c1=@aint(@index/31)+1"$"c2=100*@index"$ +
    "c3=100*@index"$"c4=c2+101"$"c5=c3-200"$"c6=@aint(@index)"$"c7=0")

ibis-list inp=table1.dat screen=132 nc=10 cols=(1,2,3,4,5,6,7)
ibis2tcl table1.dat vclen=clen
!ibis2asc table1.dat table1.dat.asc nr=&clen cols=(1,2,3,4,5,6,7)

!
! TEST 1 - Simple table - Only 1 in keycol - numcol is entry number
!
!ibis2asc table1.dat test1.asc nr=&clen cols=(1,2,3,4,5,6,7)
tieplot inp=table1.dat NL=3000 NS=3000 KEYCOL=1 KEY=(1) +
     OLDCOLS=(2,3) NEWCOLS=(4,5) NUMCOL=6  +
    plotout=test1

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test1.gpi
end-if

!
! TEST 2 - Simple table but scale vectors by 3
!ibis2asc table1.dat test2.asc nr=&clen cols=(1,2,3,4,5,6,7)
tieplot inp=table1.dat NL=3000 NS=3000 KEYCOL=1 KEY=(1) +
     OLDCOLS=(2,3) NEWCOLS=(4,5) NUMCOL=6 SCALE=3  +
    plotout=test2

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test2.gpi
end-if



ibis-gen out=table2.dat nc=7 nr=30

mf3 inp=table2.dat func=("c1=@aint(@index/16)+1"$"c2=100*@index"$ +
    "c3=100*@index"$"c4=c2+0.1"$"c5=c3-0.2"$"c6=@aint(@index)"$"c7=0")
ibis-list inp=table2.dat screen=132 nc=7 cols=(1,2,3,4,5,6,7)
ibis2tcl    table2.dat vclen=clen
!write "&clen"
!ibis2asc table2.dat table2.dat.asc nr=&clen cols=(1,2,3,4,5,6,7)
!
! TEST 3 - Complex table - Control column has 1 and 2 in it
!          create 2 plots - use default PLOTOUT=tieplot
!  You can omit 1st plot it you set key=(2)
!ibis2asc table2.dat tieplot.asc nr=&clen cols=(1,2,3,4,5,6,7)
tieplot inp=table2.dat NL=3000 NS=3000 KEYCOL=1 KEY=(1,2) +
	SCALE=1000 OLDCOLS=(2,3) NEWCOLS=(4,5) NUMCOL=6  +
    plotout=tieplot

if (mode = "nobatch" or mode = "inter")
    ush gnuplot tieplot.gpi
end-if

!
! TEST 4 - Complex table - Control column has 1 and 2 in it
!          create 2 plots
!  You can omit 1st plot it you set key=(2)
!ibis2asc table2.dat test4.asc nr=&clen cols=(1,2,3,4,5,6,7)
tieplot inp=table2.dat NL=3000 NS=3000 KEYCOL=1 KEY=(2) +
    SCALE=1000 OLDCOLS=(2,3) NEWCOLS=(4,5) NUMCOL=6 +
    plotout=test4

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test4.gpi
end-if

!
! TEST 5 - Simple plot - encapsulated postscript file
!
!ibis2asc table1.dat test5.asc nr=&clen cols=(1,2,3,4,5,6,7)
tieplot inp=table1.dat NL=3000 NS=3000 KEYCOL=1 KEY=(1) +
     OLDCOLS=(2,3) NEWCOLS=(4,5) NUMCOL=6  +
    plotout=test5 plotfmt=eps
ush gnuplot test5.eps.gpi

if (mode = "nobatch" or mode = "inter")
    ush gimp test5.eps
end-if

!
!  TEST 6 - Simple plot large plot
!
!ibis2asc table1.dat test6.asc nr=&clen cols=(1,2,3,4,5,6,7)
tieplot inp=table1.dat NL=5000 NS=5000 KEYCOL=1 KEY=(1) +
     OLDCOLS=(2,3) NEWCOLS=(4,5) NUMCOL=6  +
    plotout=test6 

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test6.gpi
end-if

let $echo="no"
end-proc
$!-----------------------------------------------------------------------------
$ create tsttieplot.log
                Version 5C/16C

      ***********************************************************
      *                                                         *
      * VICAR Supervisor version 5C, TAE V5.2                   *
      *   Debugger is now supported on all platforms            *
      *   USAGE command now implemented under Unix              *
      *                                                         *
      * VRDI and VIDS now support X-windows and Unix            *
      * New X-windows display program: xvd (for all but VAX/VMS)*
      *                                                         *
      * VICAR Run-Time Library version 16C                      *
      *   '+' form of temp filename now avail. on all platforms *
      *   ANSI C now fully supported                            *
      *                                                         *
      * See B.Deen(RGD059) with problems                        *
      *                                                         *
      ***********************************************************

  --- Type NUT for the New User Tutorial ---

  --- Type MENU for a menu of available applications ---

ibis-gen out=table1.dat nc=7 nr=30
Beginning VICAR task ibis
mf3 inp=table1.dat func=("c1=@aint(@index/31)+1"$"c2=100*@index"$  +
    "c3=100*@index"$"c4=c2+101"$"c5=c3-200"$"c6=@aint(@index)"$"c7=0")
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
function string = c1=@aint(@index/31)+1$c2=100*@index$c3=100*@index$c4=c2+101$c5=c3-200$c6=@aint(@index)$c7=0
30 records in
ibis-list inp=table1.dat screen=132 nc=10 cols=(1,2,3,4,5,6,7)
Beginning VICAR task ibis
 
Number of Rows:30  Number of Columns: 7       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:30
+-----------+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6         C:7
+-----------+-----------+-----------+-----------+-----------+-----------+-----------
        1.00      100.00      100.00      201.00     -100.00        1.00        0.00
        1.00      200.00      200.00      301.00        0.00        2.00        0.00
        1.00      300.00      300.00      401.00      100.00        3.00        0.00
        1.00      400.00      400.00      501.00      200.00        4.00        0.00
        1.00      500.00      500.00      601.00      300.00        5.00        0.00
        1.00      600.00      600.00      701.00      400.00        6.00        0.00
        1.00      700.00      700.00      801.00      500.00        7.00        0.00
        1.00      800.00      800.00      901.00      600.00        8.00        0.00
        1.00      900.00      900.00     1001.00      700.00        9.00        0.00
        1.00     1000.00     1000.00     1101.00      800.00       10.00        0.00
        1.00     1100.00     1100.00     1201.00      900.00       11.00        0.00
        1.00     1200.00     1200.00     1301.00     1000.00       12.00        0.00
        1.00     1300.00     1300.00     1401.00     1100.00       13.00        0.00
        1.00     1400.00     1400.00     1501.00     1200.00       14.00        0.00
        1.00     1500.00     1500.00     1601.00     1300.00       15.00        0.00
        1.00     1600.00     1600.00     1701.00     1400.00       16.00        0.00
        1.00     1700.00     1700.00     1801.00     1500.00       17.00        0.00
        1.00     1800.00     1800.00     1901.00     1600.00       18.00        0.00
        1.00     1900.00     1900.00     2001.00     1700.00       19.00        0.00
        1.00     2000.00     2000.00     2101.00     1800.00       20.00        0.00
        1.00     2100.00     2100.00     2201.00     1900.00       21.00        0.00
        1.00     2200.00     2200.00     2301.00     2000.00       22.00        0.00
        1.00     2300.00     2300.00     2401.00     2100.00       23.00        0.00
        1.00     2400.00     2400.00     2501.00     2200.00       24.00        0.00
        1.00     2500.00     2500.00     2601.00     2300.00       25.00        0.00
        1.00     2600.00     2600.00     2701.00     2400.00       26.00        0.00
        1.00     2700.00     2700.00     2801.00     2500.00       27.00        0.00
        1.00     2800.00     2800.00     2901.00     2600.00       28.00        0.00
        1.00     2900.00     2900.00     3001.00     2700.00       29.00        0.00
        1.00     3000.00     3000.00     3101.00     2800.00       30.00        0.00
ibis2tcl table1.dat vclen=clen
Beginning VICAR task ibis2tcl
IBIS2TCL version 2-FEB-00
tieplot inp=table1.dat NL=3000 NS=3000 KEYCOL=1 KEY=(1)  +
     OLDCOLS=(2,3) NEWCOLS=(4,5) NUMCOL=6   +
    plotout=test1
Beginning VICAR task tieplot
TIEPLOT version 02 Sep 2013 - rjb (64-bit) gnuplot
if (mode = "nobatch" or mode = "inter")
end-if
tieplot inp=table1.dat NL=3000 NS=3000 KEYCOL=1 KEY=(1)  +
     OLDCOLS=(2,3) NEWCOLS=(4,5) NUMCOL=6 SCALE=3   +
    plotout=test2
Beginning VICAR task tieplot
TIEPLOT version 02 Sep 2013 - rjb (64-bit) gnuplot
if (mode = "nobatch" or mode = "inter")
end-if
ibis-gen out=table2.dat nc=7 nr=30
Beginning VICAR task ibis
mf3 inp=table2.dat func=("c1=@aint(@index/16)+1"$"c2=100*@index"$  +
    "c3=100*@index"$"c4=c2+0.1"$"c5=c3-0.2"$"c6=@aint(@index)"$"c7=0")
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
function string = c1=@aint(@index/16)+1$c2=100*@index$c3=100*@index$c4=c2+0.1$c5=c3-0.2$c6=@aint(@index)$c7=0
30 records in
ibis-list inp=table2.dat screen=132 nc=7 cols=(1,2,3,4,5,6,7)
Beginning VICAR task ibis
 
Number of Rows:30  Number of Columns: 7       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:30
+-----------+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6         C:7
+-----------+-----------+-----------+-----------+-----------+-----------+-----------
        1.00      100.00      100.00      100.10       99.80        1.00        0.00
        1.00      200.00      200.00      200.10      199.80        2.00        0.00
        1.00      300.00      300.00      300.10      299.80        3.00        0.00
        1.00      400.00      400.00      400.10      399.80        4.00        0.00
        1.00      500.00      500.00      500.10      499.80        5.00        0.00
        1.00      600.00      600.00      600.10      599.80        6.00        0.00
        1.00      700.00      700.00      700.10      699.80        7.00        0.00
        1.00      800.00      800.00      800.10      799.80        8.00        0.00
        1.00      900.00      900.00      900.10      899.80        9.00        0.00
        1.00     1000.00     1000.00     1000.10      999.80       10.00        0.00
        1.00     1100.00     1100.00     1100.10     1099.80       11.00        0.00
        1.00     1200.00     1200.00     1200.10     1199.80       12.00        0.00
        1.00     1300.00     1300.00     1300.10     1299.80       13.00        0.00
        1.00     1400.00     1400.00     1400.10     1399.80       14.00        0.00
        1.00     1500.00     1500.00     1500.10     1499.80       15.00        0.00
        2.00     1600.00     1600.00     1600.10     1599.80       16.00        0.00
        2.00     1700.00     1700.00     1700.10     1699.80       17.00        0.00
        2.00     1800.00     1800.00     1800.10     1799.80       18.00        0.00
        2.00     1900.00     1900.00     1900.10     1899.80       19.00        0.00
        2.00     2000.00     2000.00     2000.10     1999.80       20.00        0.00
        2.00     2100.00     2100.00     2100.10     2099.80       21.00        0.00
        2.00     2200.00     2200.00     2200.10     2199.80       22.00        0.00
        2.00     2300.00     2300.00     2300.10     2299.80       23.00        0.00
        2.00     2400.00     2400.00     2400.10     2399.80       24.00        0.00
        2.00     2500.00     2500.00     2500.10     2499.80       25.00        0.00
        2.00     2600.00     2600.00     2600.10     2599.80       26.00        0.00
        2.00     2700.00     2700.00     2700.10     2699.80       27.00        0.00
        2.00     2800.00     2800.00     2800.10     2799.80       28.00        0.00
        2.00     2900.00     2900.00     2900.10     2899.80       29.00        0.00
        2.00     3000.00     3000.00     3000.10     2999.80       30.00        0.00
ibis2tcl    table2.dat vclen=clen
Beginning VICAR task ibis2tcl
IBIS2TCL version 2-FEB-00
tieplot inp=table2.dat NL=3000 NS=3000 KEYCOL=1 KEY=(1,2)  +
	SCALE=1000 OLDCOLS=(2,3) NEWCOLS=(4,5) NUMCOL=6   +
    plotout=tieplot
Beginning VICAR task tieplot
TIEPLOT version 02 Sep 2013 - rjb (64-bit) gnuplot
if (mode = "nobatch" or mode = "inter")
end-if
tieplot inp=table2.dat NL=3000 NS=3000 KEYCOL=1 KEY=(2)  +
    SCALE=1000 OLDCOLS=(2,3) NEWCOLS=(4,5) NUMCOL=6  +
    plotout=test4
Beginning VICAR task tieplot
TIEPLOT version 02 Sep 2013 - rjb (64-bit) gnuplot
if (mode = "nobatch" or mode = "inter")
end-if
tieplot inp=table1.dat NL=3000 NS=3000 KEYCOL=1 KEY=(1)  +
     OLDCOLS=(2,3) NEWCOLS=(4,5) NUMCOL=6   +
    plotout=test5 plotfmt=eps
Beginning VICAR task tieplot
TIEPLOT version 02 Sep 2013 - rjb (64-bit) gnuplot
ush gnuplot test5.eps.gpi
if (mode = "nobatch" or mode = "inter")
end-if
tieplot inp=table1.dat NL=5000 NS=5000 KEYCOL=1 KEY=(1)  +
     OLDCOLS=(2,3) NEWCOLS=(4,5) NUMCOL=6   +
    plotout=test6
Beginning VICAR task tieplot
TIEPLOT version 02 Sep 2013 - rjb (64-bit) gnuplot
if (mode = "nobatch" or mode = "inter")
end-if
let $echo="no"
$ Return
$!#############################################################################
