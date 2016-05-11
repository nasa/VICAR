$!****************************************************************************
$!
$! Build proc for MIPL module plotint
$! VPACK Version 2.1, Friday, January 08, 2016, 13:01:01
$!
$! Execute by entering:		$ @plotint
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
$ write sys$output "*** module plotint ***"
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
$ write sys$output "Invalid argument given to plotint.com file -- ", primary
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
$   if F$SEARCH("plotint.imake") .nes. ""
$   then
$      vimake plotint
$      purge plotint.bld
$   else
$      if F$SEARCH("plotint.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake plotint
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @plotint.bld "STD"
$   else
$      @plotint.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create plotint.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack plotint.com -mixed -
	-s plotint.f -
	-p plotint.pdf -
	-i plotint.imake -
	-t tstplotint.pdf tstplotint.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create plotint.f
$ DECK/DOLLARS="$ VOKAGLEVE"
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

$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create plotint.pdf
PROCESS      HELP=*
PARM INP        TYPE=(STRING)
PARM PLOTOUT    TYPE=(STRING,60)           DEFAULT="plotint"
PARM PLOTFMT    TYPE=STRING     COUNT=(0:1) VALID=(GNUPLOT,EPS) DEFAULT=GNUPLOT
PARM XCOL       TYPE=INTEGER    COUNT=1
PARM YCOL       TYPE=INTEGER    COUNT=(1:20)
PARM CONTROL    TYPE=INTEGER    COUNT=1     DEFAULT=0
PARM XLEN       TYPE=REAL       COUNT=1     DEFAULT=9.0
PARM YLEN       TYPE=REAL       COUNT=1     DEFAULT=7.0
PARM YCOLSTR    TYPE=(STRING,20) COUNT=(0:20) DEFAULT=--
PARM XLABEL     TYPE=(STRING,100) COUNT=1    DEFAULT=""
PARM YLABEL     TYPE=(STRING,60) COUNT=1    DEFAULT=""
PARM TITLE      TYPE=(STRING,100) COUNT=1    DEFAULT=""
PARM LABELSIZ   TYPE=INTEGER    COUNT=1   DEFAULT=10
PARM FREQ       TYPE=INTEGER    COUNT=(0:20)  DEFAULT=(1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1)
PARM SYMTYPE    TYPE=INTEGER    COUNT=(0:20) DEFAULT=1  VALID=1:3
PARM XRANGE     TYPE=REAL       COUNT=(0:2) DEFAULT=(0.0,5.0)
PARM YRANGE     TYPE=REAL       COUNT=(0:2) DEFAULT=(0.0,5.0)
PARM COMMENT1   TYPE=(STRING,60) DEFAULT=""
PARM COMMENT2   TYPE=(STRING,60) DEFAULT=""
PARM COMMENT3   TYPE=(STRING,60) DEFAULT=""

END-PROC
.TITLE
VICAR/IBIS Program PLOTINT
.HELP
PURPOSE

     PLOTINT plots data contained in columns of an IBIS interface file
    using the gnuplot plotting package.

.PAGE
OPERATION
    
    PLOTINT allows columns of an IBIS interface to be plotted. Either lines
or symbols may be plotted, along with annotations using the gnuplot plotting
package. Plots may be displayed on the desktop or as an encapsulated
postscript file.


PARAMETERS

    INPUT is the name of the ibis interface (tabular) File containing the
    columns to be plotted.

    PLOTOUT is the name of the output plot 

    The column number of independent data is XCOL and the column(s) of dependent
    data are in YCOL. Up to 20 dependent columns are allowed.    

    YCOLSTR are the names of the dependent columns. Up to 20.

    The parameter CONTROL points to a column used to identify a specific line in the
    IBIS file. If the number in the column is only 1 then all columns are the same
    line. 1's, 2's, 3's etc. refer to different lines.  Must have the
    same entry for all of the rows that are to be on one plot.

    XLEN and YLEN are the axes lengths in inches.

    XLABEL and YLABEL are the labels for their respective axes.

    TITLE is the Title to be placed on the graph.

    LABELSIZ is the font size for the text on the plot in points. Default=10
  
    FREQ specifies the frequency of the points to be plotted. If 2 then
    every other point is plotted. You do not have to enter a freq value
    for each line. If you have 3 lines and enter freq=2, then it is understood
    that the other two lines will have a freq of 1. 
    Default is to plot each point (1). 

    The SYMTYPE parameter specifies what type of plotting is done for 
    each dependent variable:  
    1 for symbols only, 
    2 for both symbols and points,
    3 for lines only.  

    XRANGE and YRANGE are the max and min values to plot on their respective
    axes.
    
    Up to 3 comments (COMMENT1,COMMENT2,COMMENT3) can be placed on graph. They
    will be on right side of graph.
.PAGE
PLOT OUTPUTS

    The other type of output come from the PLOTOUT and PLOTFMT parameters.
PLOTOUT allows the user to display data from 5 areas on the CCD on an x,y
plot using the gnuplot package after exiting the program. PLOT produces
a file of gnuplot commands contained in a file having a .gpi file extension.
Another file with an .asc extension is create containing columns of data
that are displayed by the gpi file.

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

  plot  'outplot.asc' u  1: 9 t .......



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


     PLOTINT INP=TRANS.INT XCOL=1 YCOL=(3,4) CONTROL=6  +
	YCOLSTR = ("AVERAGE POPULATION X1000","AIRLINE FLIGHTS") +
	XLABEL = "DISTANCE BETWEEN CITIES"  YLABEL = "THOUSANDS"  +
	TITLE = "GRAVITY TRANSPORTATION MODELS"   +
        XLEN=9.0 YLEN=7.5  SYMTYP=(1,1) 

     PLOTINT INP=FUNCTION.INT XCOL=2 YCOL=3 CONTROL=1 +
	XLABEL = "X-AXIS"  YLABEL = "Y-AXIS"  TITLE = "HYPERBOLA" +
	XLEN=6.0 YLEN=3.0 FREQ=4 SYMTYPE=2  +
	XRANGE=(-7.5,7.5) YRANGE=(0,100)  

    INP specifies the input interface file that contains the data to be 
plotted.  The column number of independent data is XCOL and the column(s)
of dependent data are in YCOL.  The control column (CONTROL) must have the
same entry for all of the rows that are to be on one plot.  The axis lengths
XLEN and YLEN are in inches.  


.PAGE
RESTRICTIONS

     The  control column must not specify more than 400 sets 
     (each set plotted on a different page).  Maximum number 
     of columns is 20.  All plotted texts must be shorter
     than 60 characters.

.PAGE
WRITTEN BY:                     B. Gokhman    25AUG1981
COGNIZANT PROGRAMMER:           R. Bambery
REVISIONS:
  1985-11    Frank Evans for F77 CALCOMP, general modernization
  1987-09    EJB for multiple plots as per control column on specified output devices
  1995-12    BAM mstp porting
  1997-01    PXA converted calcomp calls to xrtps calls and rewrote plot procedures
  2006-08    LWK fixed bug (Linux only) when YCOLSTR defaulted;
                 added NODISP and PLOTOUT parameters to support
                 output to file instead of display (no code needed
                 for NODISP as it is parsed by xrtps routines)
  2011-06-20 R. Bambery - Modified to use freeware gnuplot plotting package
  2011-09-20 R. Bambery - Fixed buf that overwrote clen in subroutine FINDCO
  2011-09-21 R. Bambery - Set yrange up to f9.2 from f7.2 and xrange from f9.2 from f7.2
  2012-07-03 R. Bambery - renamed to plotint2 for delivery to MIPL
                which still uses XRT/Graph package, removed debug
                statements, Removed <tab> in front of continuation
                lines to make backward compatible with
                32-bit Linux gfortran 4.2.1, otherwise
                compatible 64-bit Linux gfortran 4.6.3
  2012-10-21 R. Bambery - renamed back to plotint. in agreement
                with Lucas Kamp of mipl. The XRT graph package
                is to be removed from mipl. XRT was never used by
                cartlab.
  2012-12-09 R. Bambery - removed bad statement, and properly
                initialized some variables
  2013-02-12 R. Bambery - fixed PLOTFMT logic, documentation, test
  2013-07-10 R. Bambery - change .asc filename to plotout
                fix format when large plots created. test script
  2013-07-12 R. Bambery - Create ascii file for .gpi file.
                Previously used ibis2asc to create the data for gnuplot
  2013-08-13 R. Bambery - Adjusted eps format to more readable fonts
                Remove vestiges of debug statements
  2013-09-05 R. Bambery - Fixed code that did not set xrange properly
                when writing out eps gpi files. Fixed freq array so
                values will never be 0. Fixed logic for plot ranges
                that are defaulted vs. inputs
  2013-09-06 R. Bambery - Added COMMENT1, COMMENT2, COMMENT3 parameters
  2015-08-19 W. Bunch - Fixed end of line encoding so linux and sun
                will yield same output.
      
.LEVEL1
.VARIABLE INP
Input interface file
.vari plotout
Name of output files 
.VARIABLE XCOL
Column of independent variable
.VARIABLE YCOL
Columns of dependent variables
.VARIABLE CONTROL
Control column
.VARIABLE XLEN
Length of X-axis in inches
.VARIABLE YLEN
Length of Y-axis in inches
.VARIABLE YCOLSTR
Names of dependent variables
.VARIABLE XLABEL
String for X-axis
.VARIABLE YLABEL
String for Y-axis
.VARIABLE TITLE
String of text of the title
.VARIABLE FREQ
Frequency of plotted symbol
.VARIABLE SYMTYPE
Type of data line:
  (lines,symbols and lines)
.VARIABLE XRANGE
Range for X-variable
.VARIABLE YRANGE
Range for Y-variable
.VARIABLE COMMENT1
Up to 60 chars, on right
.VARIABLE COMMENT2
Up to 60 chars, on right
.VARIABLE COMMENT3
Up to 60 chars, on right

.LEVEL2
.VARIABLE INP
     INP=int             Input    IBIS    interface    file, 
                         containing data . Each 
                         variable contained in a column.  If 
                         multiple  sets  are to  be  plotted 
                         from  the  same file they  must  be 
                         designated  by the identical  entry 
                         in the control column.
.vari plotout
    The name of the output .gpi and .asc files

.VARIABLE XCOL
     XCOL=I              Integer  I  specifies  the   column 
                         containing independent variable.
.VARIABLE YCOL
     YCOL=(J1,...,JN)    Integer  J1,...,JN specify  columns 
                         containing   dependent   variables.  
                         The   maximum  number  of   columns 
                         allowed is N=20.
.VARIABLE CONTROL
     CONTROL=K           Integer  K  specifies  the  control 
                         column.   All data to be plotted on 
                         a  page  must have the  same  entry 
                         (number  or alpha) in  the  control 
                         column.   A  change of entry in the 
                         control  column will  indicate  the 
                         beginning  of  a  new  dataset  and 
                         cause initiation of a new plot.

.VARIABLE XLEN
     XLEN=X              X  specifies  the length of the 
			 X-axis in inches.   
.VARIABLE YLEN
     YLEN=Y              Y  specifies  the length of the  
			 Y-axis in inches.   
.VARIABLE YCOLSTR
     YCOLSTR=("STRING1","STRING2",...)
                         Strings "STRING1",...,"STRINGN" are 
                         optional.   They will be written on 
                         the   plot  next  to   the   symbol 
                         designating    the    corresponding 
                         variable.
.VARIABLE XLABEL
     XLABEL="STRING"      "STRING"  will be written along the 
                         X-axis (up to 60 characters).
.VARIABLE YLABEL
     YLABEL="STRING"      "STRING" will be written along  the 
                         Y-axis (up to 60 characters).
.VARIABLE TITLE
     TITLE="STRING"       String of charaters "STRING" (up to 
                         60  characters)  will  be   written 
                         under the plot.
.VARIABLE LABELSIZ
     LABELSIZ=H            Real H  specifies the size  of the
                         title lettering in inches. 
			 Default: Letters are 0.15 inch tall.
.VARIABLE FREQ
     FREQ=M              Integer  M specifies  frequency  of 
                         plotted  symbol (datapoint/symbol).  
                         M=1 - symbol for every data  point, 
                         M=2  symbol  for every second  data 
                         point, etc. Default:  M=1.
.VARIABLE SYMTYPE
     SYMTYPE=(M1,...,MN) Allows  to select the type of  data 
                         line for each dependent variable.

                         M=1   - only   lines  plotted   for 
                         corresponding variable.

                         M=2   - both  lines   and   symbols 
                         plotted for corresponding variable.

                         M=3   - lines only
			 Default is M=1
.VARIABLE XRANGE
     XRANGE=(X1,X2)      The  lower and upper limits of  the 
                         X-axis   values .      Default:  
                         automatic scaling of X-axis.

.VARIABLE YRANGE
     YRANGE=(Y1,Y2)      The  lower and upper limits of  the 
                         Y-axis values. 
                         Default:   automatic scaling of the 
                         Y-axis.

.VARIABLE COMMENT1
Up to 60 chars, on right
.VARIABLE COMMENT2
Up to 60 chars, on right
.VARIABLE COMMENT3
Up to 60 chars, on right
END                         
$ Return
$!#############################################################################
$Imake_File:
$ create plotint.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM plotint

   To Create the build file give the command:

		$ vimake plotint			(VMS)
   or
		% vimake plotint			(Unix)


************************************************************************/


#define PROGRAM	plotint

#define MODULE_LIST plotint.f

#define FTNINC_LIST gnuplotchar

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$Test_File:
$ create tstplotint.pdf
procedure
parm    mode    type=keyword count=(0:1) valid=(batch,nobatch,inter) default=batch

! May 25, 2013 - RJB
! TEST SCRIPT FOR PLOTINT
! tests BYTE, HALF, FULL, REAL images
!
! Vicar Programs:
!       ibis-gen mf3 ibis-list ibis2asc ibis-catenate
! 
! External programs
!       gnuplot 4.4 or greater
! 
! Parameters:
!   mode - method for processing: 
!       1) batch provides no display but creates .eps files
!       2) interactive or nobatch is used for display requiring
!       user interaction. 
!           
!   In batch mode it produces files testx.eps by calling gnuplot
!   In batch mode it produces files testx.eps by calling gnuplot
!       to create the encapsulated postscript file which can be
!       later viewed with ghostscript or gimp
!   In interactive or nobatch mode gnuplot is called with a window
!       manager for X11. The gnuplot display is killed by
!       a mouse click anywhere on the plot panel
!            
! External test data: 
!       <none>
!
! Output:    
!   GENed test data sets, .gpi and .eps files and intermediate 
!       tmp* files 
!   the *.gpi data produced by statplt are gnuplot scripts
!
refgbl $echo
refgbl $autousage
body
enable-log
let $autousage="none"
let _onfail="stop"

write "tstplotint script"
write "TEST 1 "
write ""
let $echo="yes"

! To display properly plotint must be preceded by and ibis2asc call
  ibis-gen out=demo.int nc=4 nr=100  
  mf3 demo.int function=("c1=@index"$"c4=1") 
  mf3 demo.int function=("c2=(c1*c1)*@sin(c1)")
  mf3 demo.int function=("c3=3+(c2+c2)")
  ibis-list demo.int sr=1 nr=10
!    ibis2asc demo.int demo.int.asc nr=100 cols=(1,2,3,4)

  ibis-gen out=xxA nc=4 nr=100
  ibis-gen out=xxB nc=4 nr=100
  mf3 xxA function=("c1=@index"$"c4=1")
  mf3 xxA function=("c2=(c1*c1)*@sin(c1)")
  mf3 xxB function=("c1=@index"$"c4=2")
  mf3 xxB function=("c3=(c1*c1)*@sin(c1)")
  mf3 xxB function=("c2=3+(c3+c3)")
!  icat (xxA,xxB) mult.int 'v        !removed May 25, 2013
  ibis-cat (xxA,xxB) mult.int
  ibis-list mult.int sr=1 nr=10
  ibis-list mult.int sr=101 nr=10

! TEST 1 - Test of minimum parameters - ouptut plot is plotint.gpi
!    ibis2asc demo.int plotint.asc nr=100 cols=(1,2,3,4)
  plotint inp=demo.int  xcol=1 ycol=(2,3) cont=4 + 
    xlabel="Test X" ylabel="Test Y" +
    title="TEST 1 - Minimum Parameters - points only - file: demo.int" 
    
! display this file:
if (mode = "nobatch" or mode = "inter")
    ush gnuplot plotint.gpi
end-if
!
! TEST 2 - Mimimum parameters - lines
!
!    ibis2asc demo.int test2.asc nr=100 cols=(1,2,3,4)
  plotint inp=demo.int  xcol=1 ycol=(2,3) cont=4 +
    symt=(3,3) xlabel="Test X" ylabel="Test Y"  +
    title="TEST 2 - Minimum Parameters file - File: demo.int" +
    plotout=test2

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test2.gpi
end-if
!
! TEST 2A - Column 2 of demo only - line
!    ibis2asc demo.int test2a.asc nr=100 cols=(1,2,3,4)
  plotint inp=demo.int  xcol=1 ycol=(2) cont=4 +
    symt=(3) xlabel="Test X" ylabel="Test Y" +
    title="TEST 2A - Minimum Parameters Column 2 only - File: demo.int" +
    plotout=test2a

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test2a.gpi
end-if
!
! TEST 2B - Column 3 of demo only - line
!    ibis2asc demo.int test2b.asc nr=100 cols=(1,2,3,4)
 plotint inp=demo.int  xcol=1 ycol=(3) cont=4 +
    symt=(3) xlabel="Test X" ylabel="Test Y" +
    title="TEST 2B - Minimum Parameters Column 3 only - File: demo.int" +
    plotout=test2b

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test2b.gpi
end-if

!
! TEST 3 -  Demonstrate minimum parameters with multiple values in control column
!    ibis2asc mult.int test3.asc nr=200 cols=(1,2,3,4)
  plotint inp=mult.int  xcol=1 ycol=(2) cont=4 +
    symt=(3) xlabel="Test X" ylabel="Test Y" +
    title="TEST 3 - Minimum Parameters with multiple control - File: mult.int" +
    plotout=test3

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test3.gpi
end-if

!
! TEST 4 - SYMCLASS = lines
!   This was never implemented in xrtgraph 
!    ibis2asc demo.int test4.asc nr=100 cols=(1,2,3,4)
  plotint inp=demo.int  xcol=1 ycol=(2,3) cont=4  symt=(3,3) +
    xlabel="Test X" ylabel="Test Y" +
    title="TEST 4 - Minimum Parameters with lines only: demo.int" +
    plotout=test4

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test4.gpi
end-if
 
! TEST 5 -  ZOOM in  - every other point
! in xrtgraph freq applied to all graphs - only 1 entry for freq was allowed
!    ibis2asc demo.int test5.asc nr=100 cols=(1,2,3,4)
!    lines + points
  plotint inp=demo.int  xcol=1 ycol=(2,3) cont=4 symt=(2,2)   freq=(2,2) + 
    xlabel="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMONPQRSTUVWXYZ" +
    ylabel="abcdefghijklmonpqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" +
    title = "TEST 5 - Zoom in - every other point - File: demo.int"  +
    xrange = (0.0,15.) yrange=(0.0,150.) +
    plotout=test5 

! display this file:
if (mode = "nobatch" or mode = "inter")
    ush gnuplot plotint.gpi
end-if
let $echo="no"
write "TEST 6 -  3 functions - small labels"
write ""
let $echo="yes"
   
   ibis-gen out=new.int nc=5 nr=100 
   mf3 new.int function=("c1=@index"$"c5=1")
   mf3 new.int function=("c2=@sin(c1)*@cos(c1)"$"c3=.5*c1/20"$ +
	"c4=@tan(c1)*c2")
   ibis-list new.int
!   ibis2asc new.int test6.asc nr=100 cols=(1,2,3,4,5)
   plotint inp=new.int xcol=1 ycol=(2,3,4) cont=5 +
	symt=(2,2,2) freq=(4,2,1) labelsiz=8  xrange=(0,10) yrange=(-1,3) +
    xlabel="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" + 
	ylabel="ordinates of 3 funcs" +
    title="TEST 6 - interface file of 3 funcs - small print - File: new.int" +
    plotout=test6
! display this file:
if (mode = "nobatch" or mode = "inter")
    ush gnuplot test6.gpi
end-if

let $echo="no"
write "TEST 7 - TEST in postscript format "
write ""
let $echo="yes"

!      ibis2asc new.int test7.asc nr=100 cols=(1,2,3,4,5)
   plotint inp=new.int xcol=1 ycol=(2,3,4) cont=5 +
    symt=(1,1,1) freq=(2)   xrange=(0,10) yrange=(-1,3) +
    plotfmt=eps xlabel="Time" +
    ylabel="ordinates of 3 funcs" +
    title="TEST 7 - interface file of 3 funcs - File: new.int" +
    plotout=test7

    ush gnuplot test7.eps.gpi
if (mode = "nobatch" or mode = "inter")
    ush gnuplot test7.gpi
    ush gimp test7.eps
end-if

! TEST 8 - large plot size
! repeat last case with output to file instead of display
!   ibis2asc new.int test8.asc nr=100 cols=(1,2,3,4,5)
   plotint inp=new.int xcol=1 ycol=(2,3,4) cont=5 +
	symt=(3,3,3) freq=10  xlen=10 ylen=12 +
    xlabel="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567" + 
	ylabel="ordinates of 3 funcs" +
    title="TEST 8 - LARGE PLOT - Interface file of 3 funcs - File: new.int" +
        plotout=test8

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test8.gpi
end-if

! TEST 9 - Test 6 with 3 comment lines
   plotint inp=new.int xcol=1 ycol=(2,3,4) cont=5 +
    symt=(2,2,2) freq=(4,2,1) labelsiz=10  xrange=(0,10) yrange=(-1,3) +
    xlabel="abscissa" +
    ylabel="ordinates of 3 funcs" +
    title="TEST 9 - interface file of 3 funcs - File: new.int" +
    comment1="Function 1: sine times cosine" comment2="Function 2: index" +
    comment3="Function 3: tangent of sine times cosine" +
    plotout=test9

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test9.gpi
end-if

let $echo="no"
disable-log
end-proc
$!-----------------------------------------------------------------------------
$ create tstplotint.log
tstplotint script
TEST 1 

  ibis-gen out=demo.int nc=4 nr=100
Beginning VICAR task ibis
  mf3 demo.int function=("c1=@index"$"c4=1")
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
  mf3 demo.int function=("c2=(c1*c1)*@sin(c1)")
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
  mf3 demo.int function=("c3=3+(c2+c2)")
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
  ibis-list demo.int sr=1 nr=10
Beginning VICAR task ibis
 
Number of Rows:100  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:10
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
+-----------+-----------+-----------+-----------
        1.00        0.84        4.68        1.00
        2.00        3.64       10.27        1.00
        3.00        1.27        5.54        1.00
        4.00      -12.11      -21.22        1.00
        5.00      -23.97      -44.95        1.00
        6.00      -10.06      -17.12        1.00
        7.00       32.19       67.38        1.00
        8.00       63.32      129.64        1.00
        9.00       33.38       69.76        1.00
       10.00      -54.40     -105.80        1.00
  ibis-gen out=xxA nc=4 nr=100
Beginning VICAR task ibis
  ibis-gen out=xxB nc=4 nr=100
Beginning VICAR task ibis
  mf3 xxA function=("c1=@index"$"c4=1")
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
  mf3 xxA function=("c2=(c1*c1)*@sin(c1)")
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
  mf3 xxB function=("c1=@index"$"c4=2")
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
  mf3 xxB function=("c3=(c1*c1)*@sin(c1)")
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
  mf3 xxB function=("c2=3+(c3+c3)")
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
  ibis-cat (xxA,xxB) mult.int
Beginning VICAR task ibis
  ibis-list mult.int sr=1 nr=10
Beginning VICAR task ibis
 
Number of Rows:200  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:10
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
+-----------+-----------+-----------+-----------
        1.00        0.84        0.00        1.00
        2.00        3.64        0.00        1.00
        3.00        1.27        0.00        1.00
        4.00      -12.11        0.00        1.00
        5.00      -23.97        0.00        1.00
        6.00      -10.06        0.00        1.00
        7.00       32.19        0.00        1.00
        8.00       63.32        0.00        1.00
        9.00       33.38        0.00        1.00
       10.00      -54.40        0.00        1.00
  ibis-list mult.int sr=101 nr=10
Beginning VICAR task ibis
 
Number of Rows:200  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 101:110
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
+-----------+-----------+-----------+-----------
        1.00        4.68        0.84        2.00
        2.00       10.27        3.64        2.00
        3.00        5.54        1.27        2.00
        4.00      -21.22      -12.11        2.00
        5.00      -44.95      -23.97        2.00
        6.00      -17.12      -10.06        2.00
        7.00       67.38       32.19        2.00
        8.00      129.64       63.32        2.00
        9.00       69.76       33.38        2.00
       10.00     -105.80      -54.40        2.00
  plotint inp=demo.int  xcol=1 ycol=(2,3) cont=4  +
    xlabel="Test X" ylabel="Test Y"  +
    title="TEST 1 - Minimum Parameters - points only - file: demo.int"
Beginning VICAR task plotint
PLOTINT - 2013-08-19
if (mode = "nobatch" or mode = "inter")
end-if
  plotint inp=demo.int  xcol=1 ycol=(2,3) cont=4  +
    symt=(3,3) xlabel="Test X" ylabel="Test Y"   +
    title="TEST 2 - Minimum Parameters file - File: demo.int"  +
    plotout=test2
Beginning VICAR task plotint
PLOTINT - 2013-08-19
if (mode = "nobatch" or mode = "inter")
end-if
  plotint inp=demo.int  xcol=1 ycol=(2) cont=4  +
    symt=(3) xlabel="Test X" ylabel="Test Y"  +
    title="TEST 2A - Minimum Parameters Column 2 only - File: demo.int"  +
    plotout=test2a
Beginning VICAR task plotint
PLOTINT - 2013-08-19
if (mode = "nobatch" or mode = "inter")
end-if
 plotint inp=demo.int  xcol=1 ycol=(3) cont=4  +
    symt=(3) xlabel="Test X" ylabel="Test Y"  +
    title="TEST 2B - Minimum Parameters Column 3 only - File: demo.int"  +
    plotout=test2b
Beginning VICAR task plotint
PLOTINT - 2013-08-19
if (mode = "nobatch" or mode = "inter")
end-if
  plotint inp=mult.int  xcol=1 ycol=(2) cont=4  +
    symt=(3) xlabel="Test X" ylabel="Test Y"  +
    title="TEST 3 - Minimum Parameters with multiple control - File: mult.int"  +
    plotout=test3
Beginning VICAR task plotint
PLOTINT - 2013-08-19
if (mode = "nobatch" or mode = "inter")
end-if
  plotint inp=demo.int  xcol=1 ycol=(2,3) cont=4  symt=(3,3)  +
    xlabel="Test X" ylabel="Test Y"  +
    title="TEST 4 - Minimum Parameters with lines only: demo.int"  +
    plotout=test4
Beginning VICAR task plotint
PLOTINT - 2013-08-19
if (mode = "nobatch" or mode = "inter")
end-if
  plotint inp=demo.int  xcol=1 ycol=(2,3) cont=4 symt=(2,2)   freq=(2,2)  +
    xlabel="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMONPQRSTUVWXYZ"  +
    ylabel="abcdefghijklmonpqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"  +
    title = "TEST 5 - Zoom in - every other point - File: demo.int"   +
    xrange = (0.0,15.) yrange=(0.0,150.)  +
    plotout=test5
Beginning VICAR task plotint
PLOTINT - 2013-08-19
if (mode = "nobatch" or mode = "inter")
end-if
let $echo="no"
TEST 6 -  3 functions - small labels

   ibis-gen out=new.int nc=5 nr=100
Beginning VICAR task ibis
   mf3 new.int function=("c1=@index"$"c5=1")
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
   mf3 new.int function=("c2=@sin(c1)*@cos(c1)"$"c3=.5*c1/20"$  +
	"c4=@tan(c1)*c2")
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
   ibis-list new.int
Beginning VICAR task ibis
 
Number of Rows:100  Number of Columns: 5       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:30
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
        1.00        0.45        0.03        0.71        1.00
        2.00       -0.38        0.05        0.83        1.00
        3.00       -0.14        0.08        0.02        1.00
        4.00        0.49        0.10        0.57        1.00
        5.00       -0.27        0.12        0.92        1.00
        6.00       -0.27        0.15        0.08        1.00
        7.00        0.50        0.17        0.43        1.00
        8.00       -0.14        0.20        0.98        1.00
        9.00       -0.38        0.22        0.17        1.00
       10.00        0.46        0.25        0.30        1.00
       11.00       -0.00        0.28        1.00        1.00
       12.00       -0.45        0.30        0.29        1.00
       13.00        0.38        0.32        0.18        1.00
       14.00        0.14        0.35        0.98        1.00
       15.00       -0.49        0.38        0.42        1.00
       16.00        0.28        0.40        0.08        1.00
       17.00        0.26        0.43        0.92        1.00
       18.00       -0.50        0.45        0.56        1.00
       19.00        0.15        0.47        0.02        1.00
       20.00        0.37        0.50        0.83        1.00
       21.00       -0.46        0.52        0.70        1.00
       22.00        0.01        0.55        0.00        1.00
       23.00        0.45        0.57        0.72        1.00
       24.00       -0.38        0.60        0.82        1.00
       25.00       -0.13        0.62        0.02        1.00
       26.00        0.49        0.65        0.58        1.00
       27.00       -0.28        0.68        0.91        1.00
       28.00       -0.26        0.70        0.07        1.00
       29.00        0.50        0.73        0.44        1.00
       30.00       -0.15        0.75        0.98        1.00
 
Rows: 31:60
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
       31.00       -0.37        0.77        0.16        1.00
       32.00        0.46        0.80        0.30        1.00
       33.00       -0.01        0.82        1.00        1.00
       34.00       -0.45        0.85        0.28        1.00
       35.00        0.39        0.88        0.18        1.00
       36.00        0.13        0.90        0.98        1.00
       37.00       -0.49        0.93        0.41        1.00
       38.00        0.28        0.95        0.09        1.00
       39.00        0.26        0.98        0.93        1.00
       40.00       -0.50        1.00        0.56        1.00
       41.00        0.16        1.02        0.03        1.00
       42.00        0.37        1.05        0.84        1.00
       43.00       -0.46        1.08        0.69        1.00
       44.00        0.02        1.10        0.00        1.00
       45.00        0.45        1.12        0.72        1.00
       46.00       -0.39        1.15        0.81        1.00
       47.00       -0.12        1.17        0.02        1.00
       48.00        0.49        1.20        0.59        1.00
       49.00       -0.29        1.23        0.91        1.00
       50.00       -0.25        1.25        0.07        1.00
       51.00        0.50        1.27        0.45        1.00
       52.00       -0.16        1.30        0.97        1.00
       53.00       -0.36        1.33        0.16        1.00
       54.00        0.46        1.35        0.31        1.00
       55.00       -0.02        1.38        1.00        1.00
       56.00       -0.44        1.40        0.27        1.00
       57.00        0.39        1.42        0.19        1.00
       58.00        0.12        1.45        0.99        1.00
       59.00       -0.49        1.48        0.41        1.00
       60.00        0.29        1.50        0.09        1.00
 
Rows: 61:90
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
       61.00        0.25        1.52        0.93        1.00
       62.00       -0.50        1.55        0.55        1.00
       63.00        0.16        1.58        0.03        1.00
       64.00        0.36        1.60        0.85        1.00
       65.00       -0.47        1.62        0.68        1.00
       66.00        0.03        1.65        0.00        1.00
       67.00        0.44        1.67        0.73        1.00
       68.00       -0.40        1.70        0.81        1.00
       69.00       -0.11        1.73        0.01        1.00
       70.00        0.49        1.75        0.60        1.00
       71.00       -0.29        1.77        0.90        1.00
       72.00       -0.25        1.80        0.06        1.00
       73.00        0.50        1.83        0.46        1.00
       74.00       -0.17        1.85        0.97        1.00
       75.00       -0.36        1.88        0.15        1.00
       76.00        0.47        1.90        0.32        1.00
       77.00       -0.03        1.92        1.00        1.00
       78.00       -0.44        1.95        0.26        1.00
       79.00        0.40        1.98        0.20        1.00
       80.00        0.11        2.00        0.99        1.00
       81.00       -0.49        2.03        0.40        1.00
       82.00        0.30        2.05        0.10        1.00
       83.00        0.24        2.08        0.94        1.00
       84.00       -0.50        2.10        0.54        1.00
       85.00        0.17        2.12        0.03        1.00
       86.00        0.35        2.15        0.85        1.00
       87.00       -0.47        2.17        0.68        1.00
       88.00        0.04        2.20        0.00        1.00
       89.00        0.44        2.22        0.74        1.00
       90.00       -0.40        2.25        0.80        1.00
 
Rows: 91:100
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
       91.00       -0.11        2.28        0.01        1.00
       92.00        0.49        2.30        0.61        1.00
       93.00       -0.30        2.33        0.90        1.00
       94.00       -0.24        2.35        0.06        1.00
       95.00        0.50        2.38        0.47        1.00
       96.00       -0.18        2.40        0.97        1.00
       97.00       -0.35        2.42        0.14        1.00
       98.00        0.47        2.45        0.33        1.00
       99.00       -0.04        2.47        1.00        1.00
      100.00       -0.44        2.50        0.26        1.00
   plotint inp=new.int xcol=1 ycol=(2,3,4) cont=5  +
	symt=(2,2,2) freq=(4,2,1) labelsiz=8  xrange=(0,10) yrange=(-1,3)  +
    xlabel="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"  +
	ylabel="ordinates of 3 funcs"  +
    title="TEST 6 - interface file of 3 funcs - small print - File: new.int"  +
    plotout=test6
Beginning VICAR task plotint
PLOTINT - 2013-08-19
if (mode = "nobatch" or mode = "inter")
end-if
let $echo="no"
TEST 7 - TEST in postscript format 

   plotint inp=new.int xcol=1 ycol=(2,3,4) cont=5  +
    symt=(1,1,1) freq=(2)   xrange=(0,10) yrange=(-1,3)  +
    plotfmt=eps xlabel="Time"  +
    ylabel="ordinates of 3 funcs"  +
    title="TEST 7 - interface file of 3 funcs - File: new.int"  +
    plotout=test7
Beginning VICAR task plotint
PLOTINT - 2013-08-19
    ush gnuplot test7.eps.gpi
if (mode = "nobatch" or mode = "inter")
end-if
   plotint inp=new.int xcol=1 ycol=(2,3,4) cont=5  +
	symt=(3,3,3) freq=10  xlen=10 ylen=12  +
    xlabel="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567"  +
	ylabel="ordinates of 3 funcs"  +
    title="TEST 8 - LARGE PLOT - Interface file of 3 funcs - File: new.int"  +
        plotout=test8
Beginning VICAR task plotint
PLOTINT - 2013-08-19
if (mode = "nobatch" or mode = "inter")
end-if
   plotint inp=new.int xcol=1 ycol=(2,3,4) cont=5  +
    symt=(2,2,2) freq=(4,2,1) labelsiz=10  xrange=(0,10) yrange=(-1,3)  +
    xlabel="abscissa"  +
    ylabel="ordinates of 3 funcs"  +
    title="TEST 9 - interface file of 3 funcs - File: new.int"  +
    comment1="Function 1: sine times cosine" comment2="Function 2: index"  +
    comment3="Function 3: tangent of sine times cosine"  +
    plotout=test9
Beginning VICAR task plotint
PLOTINT - 2013-08-19
if (mode = "nobatch" or mode = "inter")
end-if
let $echo="no"
$ Return
$!#############################################################################
