$!****************************************************************************
$!
$! Build proc for MIPL module statplt
$! VPACK Version 1.9, Friday, March 01, 2013, 15:17:36
$!
$! Execute by entering:		$ @statplt
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
$ write sys$output "*** module statplt ***"
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
$ write sys$output "Invalid argument given to statplt.com file -- ", primary
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
$   if F$SEARCH("statplt.imake") .nes. ""
$   then
$      vimake statplt
$      purge statplt.bld
$   else
$      if F$SEARCH("statplt.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake statplt
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @statplt.bld "STD"
$   else
$      @statplt.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create statplt.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack statplt.com -mixed -
	-s statplt.f -
	-i statplt.imake -
	-p statplt.pdf -
	-t tststatplt.pdf tststatplt.log_solos
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create statplt.f
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create statplt.imake
/***********************************************************************
                     IMAKE FILE FOR PROGRAM STATPLT

   To Create the build file give the command:

		$ vimake statplt			(VMS)
   or
		% vimake statplt			(Unix)
************************************************************************/
#define PROGRAM	statplt
#define R2LIB
#define MODULE_LIST statplt.f
#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST fortport
#define FTN_STRING
/*
#define LIB_MOTIF
*/

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create statplt.pdf
PROCESS help=*
PARM INP    TYPE=STRING
PARM PLOTNAME  TYPE=STRING COUNT=(0:1) DEFAULT='none'
PARM SIZE   TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SL     TYPE=INTEGER DEFAULT=1
PARM NL     TYPE=INTEGER DEFAULT=0
PARM BANDS  TYPE=INTEGER COUNT=2 VALID=(1:4) DEFAULT=(1,2)
PARM SIGMA  TYPE=REAL    DEFAULT=1.0
PARM XSCALE TYPE=INTEGER COUNT=2 DEFAULT=(0,255)
PARM YSCALE TYPE=INTEGER COUNT=2 DEFAULT=(0,255)
!PARM NODISP TYPE=KEYWORD COUNT=(0,1) VALID=NODISP DEFAULT=--
END-PROC
.TITLE
STATPLT
.HELP
PURPOSE: 

STATPLT generates a plot of the contents of a classification statistics
dataset. The plot consists of the centroids and Bayesian confidence regions
for each class, for any two input bands. If the display of the plot is not 
suppressed The plot is automatically displayed, and if requested, is saved
in an output PostScript file.

EXECUTION:
STATPLT SDS BANDS=(1,5) SIGMA=2.5   
                                    The resultant plot will have DN of
                                    Band 1 as the x-axis and the DN of
                                    Band 5 as the y-axis. Ellipses
                                    representing the 2.5 standard
                                    deviation confidence boundaries will
                                    be drawn around each centroid. The plot
                                    will be automatically displayed and
                                    optionally saved in the output PostScript
                                    file, 'statplt.psf'.

STATPLT SDS XSCALE=(0,100) PLOTNAME=(statplt1.psf)

                                    This plot will have the x-axis 
                                    rescaled such that the range 0 to 100 DN
                                    spans the entire length of the x-axis.
                                    Any class whose Band 1 mean (the band
                                    being plotted on the x-axis) is outside
                                    the 0 to 100 DN range, will not be
                                    plotted.  The plot will be automatically
                                    displayed and optionally saved in the 
                                    output PostScript file, 'statplt.psf'.

STATPLT SDS XSCALE=(0,100) YSCALE=(0,100) PLOTNAME=(statplt.psf)

                                    In addition to scaling both axes for
                                    the range 0 to 100, the plot is not
                                    displayed, but is automatically saved in
                                    the output PostScript file.

OPERATION:
     For each class in a classification statistics dataset (or for those
specified by the size field) STATPLT:

           1.  Marks the position of the centroid of the class.
           2.  Labels the centroid with its corresponding class number.
           3.  Draws the ellipse that bounds the range of the class.
               The number of standard deviations within this boundary
               is specified by the SIGMA parameter.

If the centroid of a class lies outside the range of either the x or y axis,
that class will be ignored.

If the plot display is not suppressed by the use of the keyword 'NODISP, the 
plot is displayed at the user terminal via calls to XRTPS. XRTPS also
displays three menu selections and then waits for the user to select one of
three options before continuing. The three options are; 1) exit, 2)
page, and 3) save. Selecting 'exit' results in immediate termination of
STATPLT.  Selecting 'page' results in immediate termination of STATPLT, as
STATPLT creates only one plot each time that it is run. Selecting 'save' 
results in the displayed image being saved in the output PostScript file, 
followed by the termination of STATPLT.

WRITTEN BY:  Ron Alley, 24 October 1978
COGNIZANT PROGRAMMER:  S. Pohorsky
REVISION: 5 April 1984 (Conversion to VAX)
          Made portable for UNIX and for XRT/graph. J. Turner (CRI) May 1995

        26 Jun 2011 - Ray Bambery - Clean up code to prevent warning messages
                            with gfortran 4.4.4 compiler under Linux. Changed
                            plot routine calls from XRT commercial graphics
                            package to gnuplot. Added class names and centroid
                            positions to graphics.
        03 Jul 2012 - Ray Bambery -  Renamed STATPLT2 for delivery to MIPL
                        power still uses XRT/Graph package, removed debug
                        statements, Removed <tab> in front of continuation
                        lines to make backward compatible with
                        32-bit Linux gfortran 4.2.1, otherwise
                         compatible 64-bit Linux gfortran 4.6.3</tab>
        24 Aug 2012 -lwk- renamed back to STATPLT since it has exactly the
                        same functionality and interface as the old one.

.LEVEL1
.VARIABLE INP
STRING - Input image file; must
be a classification statsitics 
dataset.
.VARIABLE PLOTNAME
STRING - Output PostScript file
name. STATPLT names output file 
as 'postscript.ps' if not specified
on VICAR command line.
NONE,YES, or filename, Default="NONE"
.VARIABLE SIZE
INTEGER - Standard VICAR size 
field
.VARIABLE SL
INTEGER - Starting line (class)
.VARIABLE NL
INTEGER - Number of lines 
(classes)
.VARIABLE BANDS
INTEGER - The bands used for the
x and y axes, respectively.
.VARIABLE SIGMA
REAL - The number of standard
deviations about each class.
.VARIABLE XSCALE
INTEGER - The limits in DN 
values for the x-axis.
.VARIABLE YSCALE
INTEGER - The limits in DN 
values for the y-axis.
.VARIABLE 'NODISP
KEYWORD - Specify 'NODISP to
suppress the display of the 
plot on the display. Plot will
be saved in PostScript file.
.LEVEL2
.VARIABLE INP
The input dataset must be in the classification statistics dataset format.
This is the format output by STATS and USTATS. Alternatively, stats may be
gathered in an IBIS interface file and converted to a statistics dataset via
the program SDSIBIS.
.VARIABLE BANDS
The plot produced by STATPLT is two dimensional. Therefore, only two bands
are plotted. The first value of BANDS is used for the x-axis, and the second
value is used for the y-axis. The default is BANDS=(1,2).
.VARIABLE SIGMA
Assuming Normal distributions (STATS, USTATS, and FASTCLAS all make this
assumption), confidence boundaries in two dimensions take the shape of
ellipses. The SIGMA variable allows the user to choose a level of
confidence, in terms of standard deviations from the mean. The default is
SIGMA=1, a one standard deviation region about the mean.
.VARIABLE XSCALE
The values given by XSCALE define the end-points of the axis. 
The default is XSCALE=(0,255).
.VARIABLE YSCALE
The values given by YSCALE define the end-points of the axis. 
The default is YSCALE=(0,255).
.VARIABLE NODISP
The keyword NODISP is used to suppress the automatic display of the output
plot image. When NODISP is specified, the automatic display of the plot
image is suppressed, but is automatically saved in the output PostScript
file. The output PostScript file may be printed using the command 'qpr'.
.VARIABLE PLOTNAME
STATPLT provides the user with the capability of specifying the name of the
output PostScript file.  If a name is not specified, STATPLT identifies the
output file as 'statplt.psf'. 
.END
$ Return
$!#############################################################################
$Test_File:
$ create tststatplt.pdf
procedure

refgbl $echo

! Jun 26, 2012 - RJB
! TEST SCRIPT FOR STATPLT
! tests BYTE, HALF images
!
! Vicar Programs:
!       gen stats  
! 
! parameters:
!   <none>
!
! Requires NO external test data: 
!
! Requires an external graphics package - gnuplot
!  the *.gpi data produced by statplt are gnuplot scripts
!
body
let _onfail="stop"
let $echo="yes"
!
! setup BYTE datasets
!
gen statplta nl=50 ns=50 linc=0 sinc=5
gen statpltb nl=50 ns=50 linc=5 sinc=1
gen statpltc nl=50 ns=50 linc=3 sinc=2
gen statpltd nl=50 ns=50 linc=-2 sinc=3 ival=130
stats (statplta,statpltb,statpltc,statpltd) statplte +
       CLASS1=(1,1,10,25) CLASS2=(11,1,20,15)   CLASS3=(31,1,5,25) +
       CLASS4=(41,1,8,25) CLASS5=(1,20,25,8)    CLASS6=(25,20,25,25) +
       CLASS7=(35,20,10,30) CLASS8=(1,35,20,15) CLASS9=(25,35,25,15) +
       CLASS10=(1,35,30,12)
!
statplt statplte plotname=statplt1.eps
ush gnuplot statplt1.eps.gpi
!       change scales
statplt statplte xscale=(0,160) yscale=(50,250) +
        plotname=statplt.eps
ush gnuplot statplt.eps.gpi
!       ellipses should halve in size;
statplt statplte sigma=0.5 plotname=statplt3.eps
ush gnuplot statplt3.eps.gpi
!       same plot, switching axes
statplt statplte bands=(2,1) plotname=statplt4.eps
ush gnuplot statplt4.eps.gpi
!       try bands 3 and 4
statplt statplte bands=(3,4) plotname=statplt5.eps
ush gnuplot statplt5.eps.gpi
!
! setup HALF data sets
!
gen statplth nl=50 ns=50 linc=0 sinc=5 format=half
gen statplti nl=50 ns=50 linc=5 sinc=1 format=half
gen statpltj nl=50 ns=50 linc=3 sinc=2 format=half
gen statpltk nl=50 ns=50 linc=-2 sinc=3 ival=130 format=half
!
stats (statplth,statplti,statpltj,statpltk) statpltl +
       CLASS1=(1,1,10,25) CLASS2=(11,1,20,15)   CLASS3=(31,1,5,25) +
       CLASS4=(41,1,8,25) CLASS5=(1,20,25,8)    CLASS6=(25,20,25,25) +
       CLASS7=(35,20,10,30) CLASS8=(1,35,20,15) CLASS9=(25,35,25,15) +
       CLASS10=(1,35,30,12)
!
statplt statpltl plotname=statplt10.eps
ush gnuplot statplt10.eps.gpi
!       change scales
statplt statpltl xscale=(0,160) yscale=(50,250) +
        plotname=statplt11.eps
ush gnuplot statplt11.eps.gpi
!       ellipses should halve in size;
statplt statpltl sigma=0.5 plotname=statplt12.eps
ush gnuplot statplt12.eps.gpi
!       same plot, switching axes
statplt statpltl bands=(2,1) plotname=statplt13.eps
ush gnuplot statplt13.eps.gpi
!       try bands 3 and 4
statplt statpltl bands=(3,4) plotname=statplt14.eps
ush gnuplot statplt14.eps.gpi

! clean up
ush rm -f statplt*.eps*.gpi
ush rm -f statplt?
ush rm -f .gnuplot

let $echo="no"
end-proc
$!-----------------------------------------------------------------------------
$ create tststatplt.log_solos
tststatplt
gen statplta nl=50 ns=50 linc=0 sinc=5
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen statpltb nl=50 ns=50 linc=5 sinc=1
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen statpltc nl=50 ns=50 linc=3 sinc=2
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen statpltd nl=50 ns=50 linc=-2 sinc=3 ival=130
Beginning VICAR task gen
GEN Version 6
GEN task completed
stats (statplta,statpltb,statpltc,statpltd) statplte  +
       CLASS1=(1,1,10,25) CLASS2=(11,1,20,15)   CLASS3=(31,1,5,25)  +
       CLASS4=(41,1,8,25) CLASS5=(1,20,25,8)    CLASS6=(25,20,25,25)  +
       CLASS7=(35,20,10,30) CLASS8=(1,35,20,15) CLASS9=(25,35,25,15)  +
       CLASS10=(1,35,30,12)
Beginning VICAR task stats
STATS version 04-Jul-2011 (64-bit) - rjb
INPUT DATA IS BYTE FORMAT

TRAINING AREAS FOR CLASS #  1   "CLASS001"
         SL=    1   SS=    1   NL=   10   NS=   25
     TOTAL POINTS =    250

 STATISTICS FOR CLASS #  1   "CLASS001"
   CHANNEL     1        2        3        4
    MEAN     60.00    34.50    37.50   157.00
   ST DEV    36.13    16.10    16.83    22.43

   COVARIANCE MATRIX
           1305.22
            261.04   259.29
            522.09   228.66   283.38
            783.13    73.80   263.55   503.01

   CORRELATION MATRIX
            1.0000
            0.4487   1.0000
            0.8584   0.8436   1.0000
            0.9665   0.2043   0.6981   1.0000

TRAINING AREAS FOR CLASS #  2   "CLASS002"
         SL=   11   SS=    1   NL=   20   NS=   15
     TOTAL POINTS =    300

 STATISTICS FOR CLASS #  2   "CLASS002"
   CHANNEL     1        2        3        4
    MEAN     35.00   104.50    72.50   112.00
   ST DEV    21.64    29.20    19.37    17.38

   COVARIANCE MATRIX
            468.23
             93.65   852.76
            187.29   537.88   375.17
            280.94  -277.42   -87.79   302.01

   CORRELATION MATRIX
            1.0000
            0.1482   1.0000
            0.4469   0.9509   1.0000
            0.7471  -0.5467  -0.2608   1.0000

TRAINING AREAS FOR CLASS #  3   "CLASS003"
         SL=   31   SS=    1   NL=    5   NS=   25
     TOTAL POINTS =    125

 STATISTICS FOR CLASS #  3   "CLASS003"
   CHANNEL     1        2        3        4
    MEAN     60.00   172.00   120.00   102.00
   ST DEV    36.20    10.14    15.09    21.91

   COVARIANCE MATRIX
           1310.48
            262.10   102.82
            524.19   135.08   227.82
            786.29   137.10   302.42   479.84

   CORRELATION MATRIX
            1.0000
            0.7140   1.0000
            0.9594   0.8826   1.0000
            0.9916   0.6172   0.9147   1.0000

TRAINING AREAS FOR CLASS #  4   "CLASS004"
         SL=   41   SS=    1   NL=    8   NS=   25
     TOTAL POINTS =    200

 STATISTICS FOR CLASS #  4   "CLASS004"
   CHANNEL     1        2        3        4
    MEAN     60.00   224.38   154.50    79.00
   ST DEV    36.15    34.45    16.02    22.17

   COVARIANCE MATRIX
           1306.53
             -8.84  1186.97
            522.61    21.58   256.53
            783.92   -22.05   281.91   491.46

   CORRELATION MATRIX
            1.0000
           -0.0071   1.0000
            0.9027   0.0391   1.0000
            0.9783  -0.0289   0.7940   1.0000

TRAINING AREAS FOR CLASS #  5   "CLASS005"
         SL=    1   SS=   20   NL=   25   NS=    8
     TOTAL POINTS =    200

 STATISTICS FOR CLASS #  5   "CLASS005"
   CHANNEL     1        2        3        4
    MEAN    112.50    82.50    81.00   173.50
   ST DEV    11.49    36.22    22.17    16.02

   COVARIANCE MATRIX
            131.91
             26.38  1311.81
             52.76   794.47   491.46
             79.15  -506.78  -281.91   256.53

   CORRELATION MATRIX
            1.0000
            0.0634   1.0000
            0.2072   0.9895   1.0000
            0.4302  -0.8736  -0.7940   1.0000

TRAINING AREAS FOR CLASS #  6   "CLASS006"
         SL=   25   SS=   20   NL=   25   NS=   25
     TOTAL POINTS =    625

 STATISTICS FOR CLASS #  6   "CLASS006"
   CHANNEL     1        2        3        4
    MEAN    155.00   174.14   170.00   151.00
   ST DEV    36.08    73.43    26.02    26.02

   COVARIANCE MATRIX
           1302.08
           -283.17  5392.01
            520.83  -482.79   677.08
            781.25    76.44     0.00   677.08

   CORRELATION MATRIX
            1.0000
           -0.1069   1.0000
            0.5547  -0.2527   1.0000
            0.8321   0.0400   0.0000   1.0000

TRAINING AREAS FOR CLASS #  7   "CLASS007"
         SL=   35   SS=   20   NL=   10   NS=   30
     TOTAL POINTS =    300

 STATISTICS FOR CLASS #  7   "CLASS007"
   CHANNEL     1        2        3        4
    MEAN    167.50   216.61   182.50   153.50
   ST DEV    43.35    44.57    19.37    26.64

   COVARIANCE MATRIX
           1879.18
           -174.26  1986.56
            751.67   -64.98   375.17
           1127.51  -107.71   401.34   709.62

   CORRELATION MATRIX
            1.0000
           -0.0902   1.0000
            0.8952  -0.0753   1.0000
            0.9764  -0.0907   0.7778   1.0000

TRAINING AREAS FOR CLASS #  8   "CLASS008"
         SL=    1   SS=   35   NL=   20   NS=   15
     TOTAL POINTS =    300

 STATISTICS FOR CLASS #  8   "CLASS008"
   CHANNEL     1        2        3        4
    MEAN    205.00    88.50   110.50   202.43
   ST DEV    21.64    29.20    19.37    74.91

   COVARIANCE MATRIX
            468.23
             93.65   852.76
            187.29   537.88   375.17
           -532.44   589.46   204.60  5611.90

   CORRELATION MATRIX
            1.0000
            0.1482   1.0000
            0.4469   0.9509   1.0000
           -0.3285   0.2695   0.1410   1.0000

TRAINING AREAS FOR CLASS #  9   "CLASS009"
         SL=   25   SS=   35   NL=   25   NS=   15
     TOTAL POINTS =    375

 STATISTICS FOR CLASS #  9   "CLASS009"
   CHANNEL     1        2        3        4
    MEAN    205.00   163.66   190.00   181.00
   ST DEV    21.63    84.46    23.33    19.42

   COVARIANCE MATRIX
            467.91
           -108.34  7132.92
            187.17  -920.46   544.12
            280.75   519.74  -200.53   377.01

   CORRELATION MATRIX
            1.0000
           -0.0593   1.0000
            0.3709  -0.4672   1.0000
            0.6684   0.3169  -0.4428   1.0000

TRAINING AREAS FOR CLASS # 10   "CLASS010"
         SL=    1   SS=   35   NL=   30   NS=   12
     TOTAL POINTS =    360

 STATISTICS FOR CLASS # 10   "CLASS010"
   CHANNEL     1        2        3        4
    MEAN    197.50   112.00   122.50   210.97
   ST DEV    17.28    43.48    26.91    42.92

   COVARIANCE MATRIX
            298.75
             59.75  1890.08
            119.50  1150.78   723.93
            -16.85  -194.85  -121.63  1842.51

   CORRELATION MATRIX
            1.0000
            0.0795   1.0000
            0.2570   0.9838   1.0000
           -0.0227  -0.1044  -0.1053   1.0000

 CLASS NUMBERS ASSIGNED
 ----------------------
     1 = CLASS001
     2 = CLASS002
     3 = CLASS003
     4 = CLASS004
     5 = CLASS005
     6 = CLASS006
     7 = CLASS007
     8 = CLASS008
     9 = CLASS009
    10 = CLASS010
statplt statplte plotname=statplt1.eps
Beginning VICAR task statplt
STATPLT - 03 July 2012
ush gnuplot statplt1.eps.gpi
statplt statplte xscale=(0,160) yscale=(50,250)  +
        plotname=statplt.eps
Beginning VICAR task statplt
STATPLT - 03 July 2012
ush gnuplot statplt.eps.gpi
statplt statplte sigma=0.5 plotname=statplt3.eps
Beginning VICAR task statplt
STATPLT - 03 July 2012
ush gnuplot statplt3.eps.gpi
statplt statplte bands=(2,1) plotname=statplt4.eps
Beginning VICAR task statplt
STATPLT - 03 July 2012
ush gnuplot statplt4.eps.gpi
statplt statplte bands=(3,4) plotname=statplt5.eps
Beginning VICAR task statplt
STATPLT - 03 July 2012
ush gnuplot statplt5.eps.gpi
gen statplth nl=50 ns=50 linc=0 sinc=5 format=half
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen statplti nl=50 ns=50 linc=5 sinc=1 format=half
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen statpltj nl=50 ns=50 linc=3 sinc=2 format=half
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen statpltk nl=50 ns=50 linc=-2 sinc=3 ival=130 format=half
Beginning VICAR task gen
GEN Version 6
GEN task completed
stats (statplth,statplti,statpltj,statpltk) statpltl  +
       CLASS1=(1,1,10,25) CLASS2=(11,1,20,15)   CLASS3=(31,1,5,25)  +
       CLASS4=(41,1,8,25) CLASS5=(1,20,25,8)    CLASS6=(25,20,25,25)  +
       CLASS7=(35,20,10,30) CLASS8=(1,35,20,15) CLASS9=(25,35,25,15)  +
       CLASS10=(1,35,30,12)
Beginning VICAR task stats
STATS version 04-Jul-2011 (64-bit) - rjb
INPUT DATA IS HALF FORMAT

TRAINING AREAS FOR CLASS #  1   "CLASS001"
         SL=    1   SS=    1   NL=   10   NS=   25
     TOTAL POINTS =    250

 STATISTICS FOR CLASS #  1   "CLASS001"
   CHANNEL     1        2        3        4
    MEAN     60.00    34.50    37.50   157.00
   ST DEV    36.13    16.10    16.83    22.43

   COVARIANCE MATRIX
           1305.22
            261.04   259.29
            522.09   228.66   283.38
            783.13    73.80   263.55   503.01

   CORRELATION MATRIX
            1.0000
            0.4487   1.0000
            0.8584   0.8436   1.0000
            0.9665   0.2043   0.6981   1.0000

TRAINING AREAS FOR CLASS #  2   "CLASS002"
         SL=   11   SS=    1   NL=   20   NS=   15
     TOTAL POINTS =    300

 STATISTICS FOR CLASS #  2   "CLASS002"
   CHANNEL     1        2        3        4
    MEAN     35.00   104.50    72.50   112.00
   ST DEV    21.64    29.20    19.37    17.38

   COVARIANCE MATRIX
            468.23
             93.65   852.76
            187.29   537.88   375.17
            280.94  -277.42   -87.79   302.01

   CORRELATION MATRIX
            1.0000
            0.1482   1.0000
            0.4469   0.9509   1.0000
            0.7471  -0.5467  -0.2608   1.0000

TRAINING AREAS FOR CLASS #  3   "CLASS003"
         SL=   31   SS=    1   NL=    5   NS=   25
     TOTAL POINTS =    125

 STATISTICS FOR CLASS #  3   "CLASS003"
   CHANNEL     1        2        3        4
    MEAN     60.00   172.00   120.00   102.00
   ST DEV    36.20    10.14    15.09    21.91

   COVARIANCE MATRIX
           1310.48
            262.10   102.82
            524.19   135.08   227.82
            786.29   137.10   302.42   479.84

   CORRELATION MATRIX
            1.0000
            0.7140   1.0000
            0.9594   0.8826   1.0000
            0.9916   0.6172   0.9147   1.0000

TRAINING AREAS FOR CLASS #  4   "CLASS004"
         SL=   41   SS=    1   NL=    8   NS=   25
     TOTAL POINTS =    200

 STATISTICS FOR CLASS #  4   "CLASS004"
   CHANNEL     1        2        3        4
    MEAN     60.00   229.50   154.50    79.00
   ST DEV    36.15    13.57    16.02    22.17

   COVARIANCE MATRIX
           1306.53
            261.31   184.17
            522.61   183.67   256.53
            783.92   104.02   281.91   491.46

   CORRELATION MATRIX
            1.0000
            0.5327   1.0000
            0.9027   0.8450   1.0000
            0.9783   0.3458   0.7940   1.0000

TRAINING AREAS FOR CLASS #  5   "CLASS005"
         SL=    1   SS=   20   NL=   25   NS=    8
     TOTAL POINTS =    200

 STATISTICS FOR CLASS #  5   "CLASS005"
   CHANNEL     1        2        3        4
    MEAN    112.50    82.50    81.00   173.50
   ST DEV    11.49    36.22    22.17    16.02

   COVARIANCE MATRIX
            131.91
             26.38  1311.81
             52.76   794.47   491.46
             79.15  -506.78  -281.91   256.53

   CORRELATION MATRIX
            1.0000
            0.0634   1.0000
            0.2072   0.9895   1.0000
            0.4302  -0.8736  -0.7940   1.0000

TRAINING AREAS FOR CLASS #  6   "CLASS006"
         SL=   25   SS=   20   NL=   25   NS=   25
     TOTAL POINTS =    625

 STATISTICS FOR CLASS #  6   "CLASS006"
   CHANNEL     1        2        3        4
    MEAN    155.00   211.00   170.00   151.00
   ST DEV    36.08    36.80    26.02    26.02

   COVARIANCE MATRIX
           1302.08
            260.42  1354.17
            520.83   885.42   677.08
            781.25  -364.58     0.00   677.08

   CORRELATION MATRIX
            1.0000
            0.1961   1.0000
            0.5547   0.9247   1.0000
            0.8321  -0.3807   0.0000   1.0000

TRAINING AREAS FOR CLASS #  7   "CLASS007"
         SL=   35   SS=   20   NL=   10   NS=   30
     TOTAL POINTS =    300

 STATISTICS FOR CLASS #  7   "CLASS007"
   CHANNEL     1        2        3        4
    MEAN    167.50   226.00   182.50   153.50
   ST DEV    43.35    16.80    19.37    26.64

   COVARIANCE MATRIX
           1879.18
            375.84   282.11
            751.67   274.50   375.17
           1127.51   142.73   401.34   709.62

   CORRELATION MATRIX
            1.0000
            0.5162   1.0000
            0.8952   0.8438   1.0000
            0.9764   0.3190   0.7778   1.0000

TRAINING AREAS FOR CLASS #  8   "CLASS008"
         SL=    1   SS=   35   NL=   20   NS=   15
     TOTAL POINTS =    300

 STATISTICS FOR CLASS #  8   "CLASS008"
   CHANNEL     1        2        3        4
    MEAN    205.00    88.50   110.50   234.00
   ST DEV    21.64    29.20    19.37    17.38

   COVARIANCE MATRIX
            468.23
             93.65   852.76
            187.29   537.88   375.17
            280.94  -277.42   -87.79   302.01

   CORRELATION MATRIX
            1.0000
            0.1482   1.0000
            0.4469   0.9509   1.0000
            0.7471  -0.5467  -0.2608   1.0000

TRAINING AREAS FOR CLASS #  9   "CLASS009"
         SL=   25   SS=   35   NL=   25   NS=   15
     TOTAL POINTS =    375

 STATISTICS FOR CLASS #  9   "CLASS009"
   CHANNEL     1        2        3        4
    MEAN    205.00   221.00   190.00   181.00
   ST DEV    21.63    36.36    23.33    19.42

   COVARIANCE MATRIX
            467.91
             93.58  1322.19
            187.17   819.52   544.12
            280.75  -465.24  -200.53   377.01

   CORRELATION MATRIX
            1.0000
            0.1190   1.0000
            0.3709   0.9662   1.0000
            0.6684  -0.6590  -0.4428   1.0000

TRAINING AREAS FOR CLASS # 10   "CLASS010"
         SL=    1   SS=   35   NL=   30   NS=   12
     TOTAL POINTS =    360

 STATISTICS FOR CLASS # 10   "CLASS010"
   CHANNEL     1        2        3        4
    MEAN    197.50   112.00   122.50   219.50
   ST DEV    17.28    43.48    26.91    20.20

   COVARIANCE MATRIX
            298.75
             59.75  1890.08
            119.50  1150.78   723.93
            179.25  -715.40  -379.05   408.05

   CORRELATION MATRIX
            1.0000
            0.0795   1.0000
            0.2570   0.9838   1.0000
            0.5134  -0.8146  -0.6974   1.0000

 CLASS NUMBERS ASSIGNED
 ----------------------
     1 = CLASS001
     2 = CLASS002
     3 = CLASS003
     4 = CLASS004
     5 = CLASS005
     6 = CLASS006
     7 = CLASS007
     8 = CLASS008
     9 = CLASS009
    10 = CLASS010
statplt statpltl plotname=statplt10.eps
Beginning VICAR task statplt
STATPLT - 03 July 2012
ush gnuplot statplt10.eps.gpi
statplt statpltl xscale=(0,160) yscale=(50,250)  +
        plotname=statplt11.eps
Beginning VICAR task statplt
STATPLT - 03 July 2012
ush gnuplot statplt11.eps.gpi
statplt statpltl sigma=0.5 plotname=statplt12.eps
Beginning VICAR task statplt
STATPLT - 03 July 2012
ush gnuplot statplt12.eps.gpi
statplt statpltl bands=(2,1) plotname=statplt13.eps
Beginning VICAR task statplt
STATPLT - 03 July 2012
ush gnuplot statplt13.eps.gpi
statplt statpltl bands=(3,4) plotname=statplt14.eps
Beginning VICAR task statplt
STATPLT - 03 July 2012
ush gnuplot statplt14.eps.gpi
ush rm -f statplt*.eps*.gpi
ush rm -f statplt?
ush rm -f .gnuplot
let $echo="no"
exit
slogoff
$ Return
$!#############################################################################
