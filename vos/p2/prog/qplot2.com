$!****************************************************************************
$!
$! Build proc for MIPL module qplot2
$! VPACK Version 1.9, Tuesday, July 01, 2014, 18:14:05
$!
$! Execute by entering:		$ @qplot2
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
$ write sys$output "*** module qplot2 ***"
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
$ write sys$output "Invalid argument given to qplot2.com file -- ", primary
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
$   if F$SEARCH("qplot2.imake") .nes. ""
$   then
$      vimake qplot2
$      purge qplot2.bld
$   else
$      if F$SEARCH("qplot2.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake qplot2
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @qplot2.bld "STD"
$   else
$      @qplot2.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create qplot2.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack qplot2.com -mixed -
	-s qplot2.f -
	-i qplot2.imake -
	-p qplot2.pdf -
	-t tstqplot2.pdf tstqplot2.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create qplot2.f
$ DECK/DOLLARS="$ VOKAGLEVE"
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
      COMMON/C1/ SIZE,DSPLAC,RDS,XMIN,XMAX,YMIN,YMAX
     &          ,XSCLMN,XSCLMX,YSCLMN,YSCLMX,XSCLDT
     &          ,YSCLDT,XLNGTH,YLNGTH,FORMAT,NORM,NCHAN
     &          ,xsclset,ysclset
      COMMON/C2/ SL,SS,EL,ES,IN,UNIT,ILINE,NLINES
     &          ,NLI,NSI,NSCHAN,GTYPE,XPAGE,LB,LABTOP
      common/commonheader/headermsg,nheadermsg,iiline,i2line

      integer*4    iiline,i2line,nheadermsg(220)  !! index into header strings
      INTEGER*4 IN(10),SL(10),SS(10),EL(10),ES(10),UNIT(10)
      INTEGER*4 GTYPE,TTLTOP,NLI(10),NSI(10),NBI(10)
	integer*4 STAT,IPARM(256),TICS
	integer*4 i,ii,j,jj,n,icount,idef,iline,ind,isize,psize
	integer*4 labtop,lcheck,lx,ly,lb,ni,nlines,np,nschan,ntest
	integer*4 ntics,ntitle,ntitx,ntity,nx,ny,nchan,naline
	integer*4 plotwid,plotht,ntbl,nplotgpi,nplotout
	integer*4 nplotgpi2,nploteps,ntmptbl
        integer*4 pttype(20),lntype(20),ptcolorl(20)

      REAL*4 RPARM(256),XAXIS(4),YAXIS(4)
      REAL*4 XMAX(10),XMIN(10),YMAX(10),YMIN(10)
      REAL*4 XSCLMN,XSCLMX,YSCLMN,YSCLMX,XLNGTH,YLNGTH
	real*4 dsplac,rds,size,xpage,xscldt,yscldt
	real*4 fpos,labstep,tmp
	logical*4 XVPTST, NORM, xsclset, ysclset, epsplot
        character*1 LPARM(1024)
	character*4 FORMAT(10),aline
        character*8 plotfmt
	character*10 labels (11)
        character*24 tbl,tmptbl
	character*30 alinenum

      CHARACTER*63 XTTL,YTTL,TTL,CBUF,XTITLE,YTITLE,TITLE
      character*63 msg,plotgpi,plotgpi2,ploteps
      character*56 headermsg(220) !! Labels * (lines per label+2) 
	CHARACTER*63 plotout
c
        character*8 ptcolor(20),lncolor(20)
	character*4 gpi/'.gpi'/,eps/'.eps'/,asc/'.asc'/
c
        character*1 num(5)
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
      call xvmessage('qplot2 version 12-Jul-2013 (64-bit gnuplot) - rjb',' ')
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
      DSPLAC=0.
      RDS=0.
      NTITX=22
      NTITY=8
      NTITLE=13
      NORM=.FALSE.
      TICS=0
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
C  'TICS'
      IF (XVPTST('TICS')) TICS=1
C  'DISPLACEMENT'
      CALL XVPARM('DISPLACE',DSPLAC,ICOUNT,IDEF,1)

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
      iiline = iiline + 3

c -- the following is not really needed with gnuplot
c  here is where "line" is called
      labels (1) = ' '
      do II = 1, 11
         write (msg (1:),'(a)') 'Line   ' 
         write (msg (6:),'(i2)') II 
         labels (II+1) = msg 
      end do


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

        labstep = 0.04

        if (iiline .gt. 16) then
           tmp = iiline/16
           plotht = int(plotht * 0.75*tmp)
           labstep =(labstep/tmp)
        endif
c	compute y-scale height
	tmp = ysclmx - ysclmn
	ysclmx = ysclmx + 50*labstep*ysclmx
	if ((ysclmx-ysclmn) .gt. 2*tmp) ysclmx = 2*tmp	

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


c output labels for only top 60% of plot
        fpos=1.0 + labstep
        do ii=2,iiline
                i = ii - 1
                fpos = fpos - labstep
10160 format('set label ',i2,' "',a,'" at graph .30 ,',f5.2,
     1 ' font "ariel,9" front nopoint tc def')
c       1 ' font "ariel 8" front nopoint tc def')
        write(98,fmt=10160,iostat=jj,err=995) i,headermsg(ii)(1:nheadermsg(ii)), fpos
cc      print 10160, i,headr(ii)(1:nheadr(ii)), fpos
cc10155 format("set label 2 '",a,"' at graph 0.4, 0.90 front nopoint tc def")
cc        write(98,fmt=10155,iostat=jj,err=995) headr(3)
        enddo


      !! Display labels on the 2nd and possibly the 3rd page 
      if (i2line .eq. 0) then
         !! If i2line == 0, then 5 or less samples
ccc---         call header (headermsg, iiline, 0) !! Title string, lines, adjust left
      else
         !! Display first set of labels and header
      endif

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
10251 format("plot '",a,"' u 1:2 t '",a,"' w linespoints lt ",i2,
     1 " pt ",i2," ps 2 lc rgb '",a,"', \")
        write(98,fmt=10251,iostat=jj,err=995) tbl(1:ntbl),alinenum(1:naline),
     1 lntype(iline),pttype(iline),ptcolor(iline)(1:ptcolorl(iline))
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
        write(98,fmt=10251,iostat=jj,err=995) tbl(1:ntbl),alinenum(1:naline),
     1 lntype(iline),pttype(iline),ptcolor(iline)(1:ptcolorl(iline))

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
     1 " pt ",i2," ps 2 lc rgb '",a,"', \")
        write(98,fmt=10253,iostat=jj,err=995) tbl(1:ntbl),alinenum(1:naline),
     1 lntype(iline),pttype(iline),ptcolor(iline)(1:ptcolorl(iline))

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
            write(97,fmt=10120,iostat=jj,err=996)
        endif
       write(97,fmt=10125,iostat=jj,err=996) ytitle(1:ntity)
       write(97,fmt=10130,iostat=jj,err=996) xtitle(1:ntitx)
        write(97,fmt=10142,iostat=jj,err=996)
        write(97,fmt=10141,iostat=jj,err=996)
       write(97,fmt=10145,iostat=jj,err=996) title(1:ntitle),psize
       write(97,fmt=10135,iostat=jj,err=996) ysclmn,ysclmx
       write(97,fmt=10140,iostat=jj,err=996) xsclmn,xsclmx

c output labels for only top 60% of plot
        fpos=1.0 + labstep
        do ii=2,iiline
                i = ii - 1
                fpos = fpos - labstep
10161 format('set label ',i2,' "',a,'" at graph .30 ,',f5.2,
     1 ' font "ariel,16" front nopoint tc def')
c       1 ' font "ariel 8" front nopoint tc def')
        write(97,fmt=10161,iostat=jj,err=996) i,headermsg(ii)(1:nheadermsg(ii)), fpos
cc      print 10160, i,headr(ii)(1:nheadr(ii)), fpos
cc10155 format("set label 2 '",a,"' at graph 0.4, 0.90 front nopoint tc def")
cc        write(98,fmt=10155,iostat=jj,err=995) headr(3)
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
        write(97,fmt=10251,iostat=jj,err=996) tbl(1:ntbl),alinenum(1:naline),
     1 lntype(iline),pttype(iline),ptcolor(iline)(1:ptcolorl(iline))
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
        write(97,fmt=10251,iostat=jj,err=996) tbl(1:ntbl),alinenum(1:naline),
     1 lntype(iline),pttype(iline),ptcolor(iline)(1:ptcolorl(iline))

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
        write(97,fmt=10253,iostat=jj,err=996) tbl(1:ntbl),alinenum(1:naline),
     1 lntype(iline),pttype(iline),ptcolor(iline)(1:ptcolorl(iline))

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
      COMMON/C1/ SIZE,DSPLAC,RDS,XMIN,XMAX,YMIN,YMAX
     &          ,XSCLMN,XSCLMX,YSCLMN,YSCLMX,XSCLDT
     &          ,YSCLDT,XLNGTH,YLNGTH,FORMAT,NORM,NCHAN
     &          ,xsclset,ysclset
      COMMON/C2/ SLX,SSX,ELX,ESX,INX,UNIT,ILINE,NLINES
     &          ,NLI,NSI,NSCHAN,GTYPE,XPAGE,LB,LABTOP
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
	real*4 adx,ady,dnmax,dsplac,dx,dy,dz,rds,size
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
      IF (DSPLAC .EQ. 0.) GO TO 650
      DO 645 ID=1,NPTS
         Y(ID)=Y(ID)+INTEQ*DSPLAC
  645 CONTINUE
cc        print *,"ADD DISPLACEMENT:"
cc        do i=1,npts
cc                print *,"-", x(i),y(i)
cc        enddo

C
C        COMPUTE MEAN AND STANDARD DEVIATION
650   MEAN=MEAN/NPTS
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create qplot2.imake
/***********************************************************************
                     IMAKE FILE FOR PROGRAM qplot2 

   To Create the build file give the command:

		$ vimake qplot2 			(VMS)
   or
		% vimake qplot2 			(Unix)
************************************************************************/
#define PROGRAM	qplot2
#define MODULE_LIST qplot2.f
#define MAIN_LANG_FORTRAN
#define R2LIB
#define USES_FORTRAN 

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create qplot2.pdf
process help=*
 PARM INP        TYPE=STRING   COUNT=(1:10)
 PARM PLOTOUT    TYPE=STRING   COUNT=(0:1)     DEFAULT="NONE"
 PARM PLOTFMT    TYPE=STRING   COUNT=(0:1) VALID=(GNUPLOT,EPS) DEFAULT=-- 
 PARM NCHAN      TYPE=INTEGER  COUNT=1         DEFAULT=1
 PARM PROCESS    TYPE=INTEGER  COUNT=(0,5:50)  DEFAULT=--
 PARM SPROCESS   TYPE=INTEGER  COUNT=(0,2:20)  DEFAULT=--
 PARM TITLE      TYPE=STRING                   DEFAULT="IPL LINE PLOT"
 PARM LOTITLE    TYPE=KEYWORD  VALID=(LOTITLE,HITITLE)  DEFAULT=HITITLE
 PARM XTITLE     TYPE=STRING   DEFAULT="RELATIVE SAMPLE NUMBER"
 PARM YTITLE     TYPE=STRING                   DEFAULT="DN VALUE"
 PARM LABELSIZ   TYPE=INTEGER                  DEFAULT=12
 PARM LOLABEL    TYPE=KEYWORD  VALID=(LOLABEL,HILABEL)  DEFAULT=HILABEL
 PARM TICS       TYPE=KEYWORD  VALID=(TICS,NOTICS)      DEFAULT=TICS
 PARM NORM       TYPE=KEYWORD  VALID=(NORM,NONORM)      DEFAULT=NONORM
 PARM RDS        TYPE=REAL                     DEFAULT=0.0
 PARM DISPLACE   TYPE=REAL                     DEFAULT=0.0
 PARM XLENGTH    TYPE=REAL     COUNT=1         DEFAULT=9.0
 PARM YLENGTH    TYPE=REAL     COUNT=1         DEFAULT=7.0
 PARM XSCALE     TYPE=REAL     COUNT=(0,2)     DEFAULT=--
 PARM YSCALE     TYPE=REAL     COUNT=(0,2)     DEFAULT=--
 PARM XVALUES    TYPE=REAL     COUNT=(0,2:20)  DEFAULT=--
 PARM YVALUES    TYPE=REAL     COUNT=(0,2:20)  DEFAULT=--
! parm nodisp keyword count=0:1 valid=nodisp default=--
 
!# annot function="VICAR Pixel Listings and Plots"
!# annot keywords=(plot,"DN values",spectral,"multi channel data",Regis,
!#   Tektronix,VRDI,Printronix,PostScript)

END-PROC
.TITLE
    AFIDS/VICAR - QPLOT2 - Produce Line or spectral plots using gnuplot
.HELP
PURPOSE:  
    QPLOT2 plots the DN values along a specified straight line through
  an image.  It also does spectral plots for multi-channel data. 

OPERATION:

   There are two operatinal modes for qplot2. One is for one or more
   single band images and the other for a mutliple band image in MSS format.

   Plots for up to 10 input image files are allowed for Single Band images.
   and up to 50 lines (profiles) for each image is allowed. The straight lines
   may be oriented in any direction, horizontal, vertical or at a slant.

   Only 1 input file can be used for MSS images. Furthermore, the basic
   unit is not a line but a point. Up tp 20 points may requested. 

   
   The modes are distinguished via the PROCESS and SPROCESS parameters. 

   PROCESS parameter is used for One band BSQ files
   SPROCESS parameter is used for MSS files. NCHAN parameter is required
   since MSS files are one BAND with files lying side-by-side.
   These parameters are mutually exclusive

PARAMETERS:

  There are two types of parameters:

  (1) Parameters with a single specification:
        INP         NCHAN
        TITLE       LOTITLE     XTITLE
        YTITLE      LABELSIZ    LOLABEL
        NORM        RDS         DISPLACE
        XLENGTH     YLENGTH
        XSCALE      YSCALE

  (2) Parameters which have a specification for each line plotted:
        PROCESS     SPROCESS
        XVALUES     YVALUES

  The parameter PROCESS specifies that one or more lines to be plotted. 
  Following the keyword, each plot is specified by a set of five numbers.
  The first value in each quintet specifies the Input Data Set Number and
  the remaining four values specify the Starting Line, Starting Sample,
  Ending Line, and Ending Sample. (No Default).

  NOTE:  The line plotted will cover SQRT((EL-SL)**2+(ES-SS)**2) "relative 
  samples"  along the horizontal axis and will start at "relative sample" 1.

  This version allows plots for BYTE, HALF, FULL or REAL data files.
  It also allows for mixed filetypes in INP to be plotted on same graph.

  The SPROCESS parameter for spectral data only uses 2 numbers, the
  line and sample position of a single point within the MSS formatted image.
  Up to 20 points can be specified. Thus, it is possible to look at
  a 20 point straight line in an image or 20 widely separated points.

  You can use PROCESS for an MSS image but you will only get a profile
  on the 1st MSS Band.

  The parameters TITLE, XTITLE and YTITLE refer to the usual plot labelling
  schemes.

  The LABELSIZ parameter refers to the font size in points for all the text in the
  graph. This is suitable for changes of XLENGTH and YLENGTH when different 
  from the default. Note, the internal font is Ariel and cannot be changed.

  XSCALE and YSCALE are the max size of the x plot values and y plot values.

  The parameter NORM is used to normalize the DN values to 1. The
  DN values are ratioed to the max DN of the line. Especially useful
  when different data formats occur for each input image.

  XLENGTH and YLENGTH tell gnuplot what size the output is in inches
  at 72 dots/inch. Default is 7 inches wide by 5 inches long,
  or 648 by 504 pixels.

  The parameter TICS refers to placing crosshatching on the plot along
  with the axis tic marks which are in units of 10.

  The parameter RDS refers to ????

  The parameter DISPLACE forces each line to be displaced upward successively
  by the DISPLACE=number units in DN.

  The NCHAN parameter is used with MSS images to select how many of the
  channels (bands) for the data points in SPROCESS.


  LOTITILE and LOLABEL keywords are not supported.
  There is internal code to put statistics (mean,stddev) on
  graphs. This has not been implemented. (Not even in MIPL).
  There is internal code to place vicar labels on the graphs
  This has not been implemented. (Not even in MIPL).

  
NOTES:

  The common plotting routines used in MIPL were removed in this version
  and converted to gnuplot commands. The Mipl version in 2011 uses the
  xrt commercial plotting package.


OUTPUTS:

  Up to two types of outputs can be produced, interactive plots using 
  gnuplot and hardcopy plots suitable for publications using gnuplot and ghostscript
  or gimp.

  The first type of plot is rendered via the creation of a gnuplot instruction, or
  command file. This is indicated by the file extension of .gpi.  After completion
  of the qplot2 task this file creates an interactive plot on the desktop, e.g.,
     ush gnuplot qplot.gpi.
  This display stays active until mouse-clicked somewhere on the plot panel.
  The terminal window prompt is inactive until the panel is closed.

  If you wish, you can issue the following gnuplot command to keep the plot on
  the desktop.

  ush gnuplot -persist gplot.gpi

  Again the terminal prompt is inactive until the plot panel has been mouse-clicked.
  After the mouse click the panel remains but the terminal prompt is restored. This
  makes it easy to compare outputs from several qplot2 commands.

  A second output is derived from the PLOTOUT parameter. By default this parameter is
  set to "NONE" so no hardcopy output is produced.  It can be set to "YES" in which
  case a file called gplot2.gpi is created. If you issue the command

  ush gnuplot qplot2.gpi 
 
  no interactive plot panel is created but it produces a file qplot.eps. This
  file can be displayed using ghostscript (the usual unix command is gs).

  gs qplot2.eps

  or gimp, e.g.,

  gimp qplot2.eps

  PLOTOUT can also be given a file name, e.g.,
  PLOTOUT=fourbands   (not necessary to give file extension) 

  In that case the 2 outputs are produced, fourbands.gpi and fourbands2.gpi.
  Fourbands.gpi gives the interactive gnuplot panel and the second one, when
  run, will create fourbands.eps which can be displayed by ghostscript or gimp.


RESTRICTIONS:

  (1) Maximum number of lines plotted is 10.
  (2) Spectral plots require a single input in mss format.
  (3) Cannot use PROCESS and SPROCESS parameters simultaneously
  (4) Multi Band BSQ files are not supported

Note:  This program makes use of multiple intermediate data sets.
  If this program is run under a directory which does not allow the
  creation of these files either because of disk quotas or protection)
  the program will die and plot file will not be created.


.page

HISTORY:

  Original Programmer:  John H. Reimer,  22 Aug. 1982
  Converted to Vicar2 by:  John H. Reimer,  22 April 1985
  Cognizant Programmer:  Ray Bambery   
  Ported to Unix (MSTP S/W Conversion) C. Randy Schenk (CRI) 10 JUly 1995

  6-13-2011 - Ray Bambery - Converted to use gnuplot, and work
                     under gfortran 4.4.4 on liunx
  6-05-2012 - Ray Bambery - fixed dimensions of x,y in subroutine equiv
  7-10-2012 - Ray Bambery - Renamed qplot2g for delivery to MIPL
                        qplot2 still uses XRT/Graph package, 
                        Removed <tab> in front of continuation
                        lines to make backward compatible with
                        32-bit Linux gfortran 4.2.1, otherwise
                        compatible 64-bit Linux gfortran 4.6.3</tab>
  10-13-2012 - Ray Bambery - Renamed back to qplot2, in agreement
                        with Lucas Kamp of mipl. The XRT graph package
                        is to be removed from mipl. XRT was never used by
                        cartlab. Implemented LABELSIZ parameter
  07-09-2013 - Ray Bambery - Fixed truncated plot labeling
                        created unique names for data sets for plots to
                        prevent confusion in long scripts. Added PLOTFMT
  07-12-2013 - Ray Bambery - Adjusted eps format to more readable fonts

.page
.level1
.variable inp
STRING - Input data set
.VARIABLE PLOTOUT
STRING - Output file
name. QPLOT names output file 
as 'qplot.eps' if "YES" is entered
NONE means no output.
filename, Default="NONE"
.VARIABLE PLOTFMT
string - specification
 of plotter output
 Gnuplot or eps

.variable process
INTEGER - DSN1,SL1,SS1,EL1,ES1, DSN2,SL2,SS2,EL2,ES2, ...
.variable nchan
INTEGER - Number of channels in mss formatted input data set
.variable sprocess
INTEGER - LINE1,SAMPLE1, LINE2,SAMPLE2, ...
.variable title
STRING - Title of plots.
.variable lotitle
KEYWORD - Lower position for title.
.variable xtitle
STRING - X axis title
.variable ytitle
STRING - Y axis title
.variable labelsiz
INTEGER - Font Size, in points, of plot text.
Default=12
.variable lolabel
KEYWORD - Lower position for label.
.variable tics
KEYWORD - Place crosshatching on plot
.variable norm
KEYWORD - Normalizes data to 1.
.variable rds
REAL - DN scaling factor.
.variable displace
REAL - Displacement for subsequent lines.
.variable xlength
REAL - Length of X axis (inches).
.variable ylength
REAL - Length of Y axis (inches).
.variable xscale
REAL - Min & Max of X axis.
.variable yscale
REAL - MIN & Max of Y axis.
.variable xvalues
REAL - X Image Size in inches. 
.variable yvalues
REAL - Y Image Size in inches.
.level2
.variable inp
Input data set; maximum number of 10.
(Only one input allowed for MSS data)
.VARIABLE PLOTOUT
QPLOT2 provides the user with the capability of specifying the name of the
output file.  If a name is not specified, QPLOT2 identifies the
output file as 'qplot.gpi' or qplot.eps. 
.VARIABLE PLOTFMT
 KEYWORD - VALID=(GNUPLOT,EPS)
 EPS Specifies POSTSCRIPT plotting, else GNUPLOT is assumed.
 If neither 'PLOTFMT nor PLOTOUT are specified the output table
 will still be generated with the name qplot2.asc

.variable process
Specifies one or more lines to be plotted. Following the keyword,
each plot is specified by a set of five numbers. The first value in
each set specifies the Input Data Set Number and the remaining four
values specify the Starting Line, Starting Sample, Ending Line, and Ending
Sample. (No Default).  NOTE:  The line plotted will cover
SQRT((EL-SL)**2+(ES-SS)**2) "relative samples" along the horizontal axis
and will start at "relative sample" 1.
.variable nchan
Specifies the number of spectral channels in an input data set in mss
format. This keyword is used in conjunction with the SPROCESS keyword.
(Default is 1)
.variable sprocess
Specifies one or more spectral plots. This keyword requires that the input
be in mss format and that NCHAN is specified. Following the keyword,
each spectral plot is specified by a set of two numbers. The first value
in each set specifies the Line and the second value the Sample of the
point within the first channel through which the spectral plot is to be
done. (No Default)
.variable title
Used to specify the title of plots (Max length of 52 characters).
(Default is 'IPL LINE PLOT', or for spectral plots, 'IPL SPECTRAL PLOT')
.variable lotitle
Specifies that the title will be written within the 8.5 x 11 area
(if the y axis length is less than or equal to 7 inches). (Default
is to place the title at the top of the page)
.variable xtitle
Specifies the title for the X axis (Max length of 52 characters). (Default is
'Relative Sample Number')
.variable ytitle
Specifies the title for the Y axis (Max length of 52 characters). (Default is
'DN Value')
.variable labelsiz
Specifies the Font size, in points, for Plot Text
(Default is 12)
.variable lolabel
Specifies that labels will be written within the 8.5 x 11 space.
(Default is to place the labels at the top of the page.)
.variable norm
Causes DN values to be scaled linearly so that the largest value
becomes 1.  The length of the y axis is set to 5 inches.
(Default is that this is not done.)
.variable rds
Causes DN values to be scaled by the following equation:
OUT=SQRT(IN**2-RDS**2). (Default is that this is not done)
.variable displace
Specifies that subsequent lines on the same plot will be displaced
by the given amount.  This is specified in terms of the final plotted
vertical values, rather than input DN, in the cases where the input
values are scaled. (Default is 0.0)
.variable xlength
Specifies the length in inches of the X axis. (Default is 9.0; Max is 72.0)
.variable ylength
Specifies the length in inches of the Y axis. (Default is 7.0; max is 12.0)
.variable xscale
Specifies the scale used along the X axis.  The X axis will be drawn going
from a minimum of the first value to a maximum of the second.  The defaults
for these values are obtained by determining the minimum and maximum X
values to be plotted on the axis and then passing these values to the
subroutine SCALE.
SCALE determines the scaling so that 1 inch along the axis will always be
an interval of 1,2,3,4,5,6 or 8*(10**n) units.  This usually results in
having the plotted lines occupy only a portion of the axis.  By using the
XSCALE keyword the user can force plots to occupy a greater portion of the
X axis.  Axis values are printed every inch, however, and if it is desired
that these values be nice round numbers the quantity:
                  (XSCALEMAX-XSCALEMIN)/XLENGTH
should be a nice round number.
.variable yscale
Specifies the scale used along the Y axis.  The Y axis will be drawn going
from a minimum of the first value to a maximum of the second.  The defaults
for these values are obtained by determining the minimum and maximum Y
values to be plotted on the axis and then passing these values to the
subroutine SCALE.
SCALE determines the scaling so that 1 inch along the axis will always be
an interval of 1,2,3,4,5,6 or 8*(10**n) units.  This usually results in
having the plotted lines occupy only a portion of the axis.  By using the
YSCALE keyword the user can force plots to occupy a greater portion of the
Y axis.  Axis values are printed every inch, however, and if it is desired
that these values be nice round numbers the quantity:
                  (YSCALEMAX-YSCALEMIN)/YLENGTH
should be a nice round number.
.variable xvalues
Allows the user to rescale the actual x (sample) data values of the
lines to be plotted. XVALUES is followed by two real values specifying the
minimum and maximum x values. The first value (the minimum) is the point
along the X axis where the first data point will be plotted.  The second
value (the maximum) is the point along the X axis where the last data
point will be plotted.
(DEFAULTS: XMIN=1.0, XMAX=SQRT[(EL-SL)**2+(ES-SS)**2]+1
.variable yvalues
Allows the user to rescale the actual y (DN) data values of the lines
to be plotted. YVALUES is followed by groups of two real values, one
group for each line to be plotted. The two values in each group specify the
minimum and maximum y values. The first value (the minimum) is the point
along the Y axis where a DN of zero will be plotted.  The second value
(the maximum) is the point along the Y axis where a DN of 255 (32767
for halfword data) will be plotted.
(DEFAULT: YMIN=0.0, YMAX=255 (byte) YMAX=32767 (halfword)   )

.end
$ Return
$!#############################################################################
$Test_File:
$ create tstqplot2.pdf
procedure
parm    mode    type=string count=(0:1) valid=(batch,nobatch,inter) default=batch
refgbl $echo
! Oct 13, 2012 - RJB
! TEST SCRIPT FOR QPLOT2
! tests BYTE, HALF, FULL, REAL images
!
! Vicar Programs:
!       gen list mss tran  
!
! External programs
!       gnuplot 4.6 or greater
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
!
! Output:
!   GENed test data sets, .gpi and .eps files and intermediate 
!       tmp* files 
!  the *.gpi data produced by qplot2 are gnuplot scripts
!
body
let _onfail="stop"
let $echo="yes"
!BYTE DATA SETS
gen inp1 nl=20 ns=20
gen inp1a nl=40 ns=40 linc=2 sinc=2
gen inp2 nl=20 ns=20 linc=10 sinc=10
gen inp3 nl=128 ns=128 linc=5 sinc=5
!
! one file, one line in sample direction - Just defaults
! remember proc=(dsrn, sl,ss,el,es)
!TEST 1 - default plotout, no .eps file
qplot2 inp1 proc=(1, 1,1,1,20) title=("QPLOT2 TEST 1") 
!  
if (mode = "nobatch" or mode = "inter") 
    ush gnuplot qplot.gpi
end-if
! 
!one file, one line in line direction - Large TITLE font
!
!TEST 2
qplot2 inp1 proc=(1, 1,20,20,20) title=("QPLOT2 TEST 2") +
    labelsiz=14 plotout=test2

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test2.gpi
!else
!    ush gnuplot test22.gpi
end-if
!
! one file, one line slanted at 45 degrees - Just defaults
!TEST 3  - SLANT
qplot2 inp1 proc=(1, 1,1,20,20) title=("QPLOT2 TEST 3") +
    plotout=test3
if (mode = "nobatch" or mode = "inter")
    ush gnuplot test3.gpi
end-if

!TEST 4  - SLANT
qplot2 inp1 proc=(1, 1,1,20,20)  title=("QPLOT2 TEST 4 - SLANT PROFILE") +
    xtitle=("DN Position") ytitle=("DN Value") plotout=test4
if (mode = "nobatch" or mode = "inter")
    ush gnuplot test4.gpi
end-if

!TEST 5  - SLANT - SCALED
qplot2 inp1a proc=(1, 1,1,20,20)  title=("QPLOT2 TEST 5 - SLANT PROFILE") +
    xtitle=("DN Position") ytitle=("DN Value") +
    xscale=(1,40) yscale=(1,255) plotout=test5

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test5.gpi
end-if

!TEST 6  - SLANT PROFILES OF 3 LINES ON ONE IMAGE
qplot2 inp1a proc=(1, 1,1,20,20, 1, 3,3,13,13, 1, 12,12,1,1)  +
   title=("QPLOT2 TEST 6 - SLANT PROFILES OF 3 LINES") +
   xtitle=("DN Position") ytitle=("DN Value") +
   xscale=(1,50) yscale=(1,255) plotout=test6

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test6.gpi
end-if
!
!TEST 7  - SLANT - SMALL GRAPH
qplot2 inp1a proc=(1, 1,1,20,20)  title=("QPLOT2 TEST 7 - SLANT PROFILE") +
   xtitle=("DN Position") ytitle=("DN Value") +
   xscale=(1,30) yscale=(1,100) xlength=4 ylength=3 labelsiz=8 +
    plotout=test7

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test7.gpi
end-if

!
!TEST 8  - SLANT
qplot2 inp1a proc=(1, 1,1,20,20)  title=("QPLOT2 TEST 8 - SLANT PROFILE") +
   xtitle=("DN Position") ytitle=("Normalized DN Value") +
   norm="norm" plotout=test8

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test8.gpi
end-if

!
!TEST 9  - SLANT
qplot2 inp1a proc=(1, 1,1,20,20)  title=("QPLOT2 TEST 9 - SLANT PROFILE") +
   xtitle=("Relative DN Position") ytitle=("DN Value") +
   tics="notics" plotout=test9

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test9.gpi
end-if

!
!TEST 10  - SLANT
qplot2 inp1a proc=(1, 1,1,20,20)  title=("QPLOT2 TEST 10 - SLANT PROFILE") +
   xtitle=("Relative DN Position") ytitle=("DN Value") +
    rds=20.0 plotout=test10

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test10.gpi
end-if

!
!TEST 11  - SLANT
qplot2 inp1a proc=(1, 1,1,20,20)  title=("QPLOT2 TEST 11 - SLANT PROFILE") +
   xtitle=("Relative DN Position") ytitle=("DN Value") +
    rds=-5.0 plotout=test11

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test11.gpi
end-if


let $echo="no"
write "Multiple lines on one band"
let $echo="yes"
!TEST 12 - SLANT
qplot2 (inp1,inp2) proc=(1,1,1,20,20, 2,1,1,20,20) title=("QPLOT2 TEST 12") +
    plotout=test12

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test12.gpi
end-if

!TEST 13  - SLANT
qplot2 (inp1,inp2) proc=(1,1,1,20,20, 2,1,1,20,20) title=("QPLOT2 TEST 13") +
    xscale=(1,40) yscale=(1,255)  plotout=test13

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test13.gpi
end-if

!TEST 14 - SLANT
qplot2 (inp1,inp2) proc=(1,1,1,20,20, 2,1,1,20,20) title=("QPLOT2 TEST 14") +
    xscale=(1,25) yscale=(1,50) plotout=test14

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test14.gpi
end-if

!TEST 15 
qplot2 (inp2,inp3) proc=(1,1,1,20,20, 2,50,50,70,70) title=("QPLOT2 TEST 15") +
    xscale=(1,25) yscale=(1,255) plotout=test15

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test15.gpi
end-if


!TEST 16
qplot2 (inp1,inp2,inp3) proc=(1,1,1,20,20, 2,1,1,20,20, 3,50,50,70,70) +
        title=("QPLOT2 TEST 16") plotout=test16

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test16.gpi
end-if

!
!TEST 17 - ONE SLANT LINE ON EACH OF FOUR IMAGES - make eps image
qplot2 (inp1,inp2,inp3,inp1a)  title=("QPLOT2 TEST 17 - Four Images")  +
    proc=(1,1,1,20,20, 2,1,1,20,20, 3,50,50,70,70, 4,1,1,20,20) +
    plotout=test17

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test17.gpi
end-if
! 
!        plotout="qplot1.psf"
!    displace=5

gen half1 nl=20 ns=20 linc=20 modulo=32767 format=half
gen half1b nl=20 ns=20 linc=20 modulo=32767 format=half
gen half1a nl=40 ns=40 linc=200 sinc=2 ival=20 modulo=2048 format=half
gen half2 nl=20 ns=20 linc=100 sinc=10 ival=44 modulo=3096 format=half
gen half3 nl=128 ns=128 linc=50 sinc=5 ival=75 modulo=1024 format=half
!
let $echo="no"
write "HALF WORD data sets"
let $echo="yes"
!TEST 18
qplot2 (half1,half2,half3,half1a) title=("QPLOT2 TEST 18") +
    proc=(1,1,1,20,20, 2,1,1,20,20, 3,50,50,70,70, 4,1,1,20,20) +
    plotout=test18

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test18.gpi
end-if

!
!TEST 19
qplot2 (half1,half1b) proc=(1,1,1,20,20, 2,1,1,20,20) +
    title=("QPLOT2 TEST 19 - LINE 2 displaced by 5 upward") +
    displace=5 plotout=test19

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test19.gpi
end-if

gen full1 nl=20 ns=20 linc=30 modulo=512 format=full
gen full1b nl=20 ns=20 linc=20 modulo=32767 format=full
gen full1a nl=40 ns=40 linc=100 sinc=2 ival=20 modulo=2048 format=full
gen full2 nl=20 ns=20 linc=100 sinc=10 ival=44 modulo=3096 format=full
gen full3 nl=128 ns=128 linc=50 sinc=5 ival=75 modulo=1024 format=full
let $echo="no"
write "FULL WORD data sets"
let $echo="yes"
!TEST 20
qplot2 (full1,full2,full3,full1a) title=("QPLOT2 TEST 20") +
    proc=(1,1,1,20,20, 2,1,1,20,20, 3,50,50,70,70, 4,1,1,20,20) +
    plotout=test20

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test20.gpi
end-if
!TEST 21
qplot2 (full1,full2,full3,full1a) title=("QPLOT2 TEST 21") +
    proc=(1,1,1,20,20, 2,1,1,20,20, 3,50,50,70,70, 4,1,1,20,20) +
    yscale=(0,5000.) plotout=test21

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test21.gpi
end-if

gen real1 nl=20 ns=20 linc=30 modulo=512 format=real
gen real1b nl=20 ns=20 linc=20 modulo=32767 format=real
gen real1a nl=40 ns=40 linc=100 sinc=2 ival=20 modulo=2048 format=real
gen real2 nl=20 ns=20 linc=100 sinc=10 ival=44 modulo=3096 format=real
gen real3 nl=128 ns=128 linc=50 sinc=5 ival=75 modulo=1024 format=real
let $echo="no"
write "REAL data sets"
let $echo="yes"
!TEST 22
qplot2 (real1,real2,real3,real1a) title=("QPLOT2 TEST 22") +
    proc=(1,1,1,20,20, 2,1,1,20,20, 3,50,50,70,70, 4,1,1,20,20) +
    plotout=test22

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test22.gpi
end-if

let $echo="no"
write "MIXED data sets"
let $echo="yes"
!TEST 23
qplot2 (inp1,half2,full3,real1a)  +
    proc=(1,1,1,20,20, 2,1,1,20,20, 3,50,50,70,70, 4,1,1,20,20) +
    title=("QPLOT TEST 23 -MIXED Data Types - BYTE,HALF,FULL,REAL") +
    plotout=test23

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test23.gpi
end-if

gen b1 nl=50 ns=50
gen b2 nl=50 ns=50 ival=5 linc=1 sinc=1
gen b3 nl=50 ns=50 ival=20 linc=2 sinc=2
gen b4 nl=50 ns=50 ival=40 linc=3 sinc=3

gen b5 nl=50 ns=50 ival=100 linc=3 sinc=3
mss (b1,b2,b3,b4) out=mss4a 
mss (b5,b2,b3,b4) out=mss4b

tran mss4a spec4a outorg=bsq nbands=4
let $echo="no"
write "MSS data sets"
let $echo="yes"
!
!TEST 24 - 1 line on 1st channel of a 4 band MSS image
qplot2 mss4a proc=(1,1,1,20,20) nchan=2 +
    title=("QPLOT2 TEST 24 - One Line from Channel 1 - MSS Data") +
    plotout=test24

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test24.gpi
end-if

!TEST 25 - 1 pixel in spectral plot
qplot2 mss4a sproc=(10,10) nchan=4 +
    title=("QPLOT2 TEST 25 - Spectral Plot") +
    plotout=test25

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test25.gpi
end-if

!TSET 26 - Spectral Plot for 4 points on MSS image
qplot2 mss4a sproc=(10,10,10,11,11,10,11,11) nchan=4 +
    title=("QPLOT2 TEST 26 - Spectral Plot on MSS image") +
    plotout=test26

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test26.gpi
end-if

!TEST 27 - Spectral plot for 4 points on MSS image only first
! 3 channels
qplot2 mss4a sproc=(10,10,10,11,11,10,11,11) nchan=3 +
    title=("QPLOT2 TEST 27 - Spectral Plot 3 channels on MSS image") +
    plotout=test27

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test27.gpi
end-if

let $echo="no"
write "MULTIBAND images  - should FAIL"
let $echo="yes"


!TEST - failure of 4-band bsq image 
let _onfail="continue"
qplot2 spec4a sproc=(10,10,10,11,11,10,11,11) nchan=4 +
    title=("QPLOT2 TEST 28 - Spectral Plot on Multiband image")
let _onfail="stop"

let $echo="no"
write "Test eps output"
let $echo="yes"
!TEST 28 - default name - qplot.eps
qplot2 mss4a sproc=(10,10,10,11,11,10,11,11) nchan=4 +
    title=("QPLOT2 TEST 28X - Spectral Plot on MSS image") +
    plotout="yes" plotfmt="eps"

if (mode = "nobatch" or mode = "inter")
    ush gnuplot qplot.gpi
end-if
ush gnuplot qplot.eps.gpi

!TEST 29 - provide name for eps file
qplot2 mss4a sproc=(10,10,10,11,11,10,11,11) nchan=4 +
    title=("QPLOT2 TEST 29 - Spectral Plot on MSS image") +
    plotout="test29" plotfmt="eps"

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test29.gpi
end-if
ush gnuplot test29.eps.gpi

let $echo="no"
end-proc
$!-----------------------------------------------------------------------------
$ create tstqplot2.log
tstqplot2
gen inp1 nl=20 ns=20
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen inp1a nl=40 ns=40 linc=2 sinc=2
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen inp2 nl=20 ns=20 linc=10 sinc=10
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen inp3 nl=128 ns=128 linc=5 sinc=5
Beginning VICAR task gen
GEN Version 6
GEN task completed
qplot2 inp1 proc=(1, 1,1,1,20) title=("QPLOT2 TEST 1")
Beginning VICAR task qplot2
qplot2 version 12-Jul-2013 (64-bit gnuplot) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
qplot2 inp1 proc=(1, 1,20,20,20) title=("QPLOT2 TEST 2")  +
    labelsiz=14 plotout=test2
Beginning VICAR task qplot2
qplot2 version 12-Jul-2013 (64-bit gnuplot) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
qplot2 inp1 proc=(1, 1,1,20,20) title=("QPLOT2 TEST 3")  +
    plotout=test3
Beginning VICAR task qplot2
qplot2 version 12-Jul-2013 (64-bit gnuplot) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
qplot2 inp1 proc=(1, 1,1,20,20)  title=("QPLOT2 TEST 4 - SLANT PROFILE")  +
    xtitle=("DN Position") ytitle=("DN Value") plotout=test4
Beginning VICAR task qplot2
qplot2 version 12-Jul-2013 (64-bit gnuplot) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
qplot2 inp1a proc=(1, 1,1,20,20)  title=("QPLOT2 TEST 5 - SLANT PROFILE")  +
    xtitle=("DN Position") ytitle=("DN Value")  +
    xscale=(1,40) yscale=(1,255) plotout=test5
Beginning VICAR task qplot2
qplot2 version 12-Jul-2013 (64-bit gnuplot) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
qplot2 inp1a proc=(1, 1,1,20,20, 1, 3,3,13,13, 1, 12,12,1,1)   +
   title=("QPLOT2 TEST 6 - SLANT PROFILES OF 3 LINES")  +
   xtitle=("DN Position") ytitle=("DN Value")  +
   xscale=(1,50) yscale=(1,255) plotout=test6
Beginning VICAR task qplot2
qplot2 version 12-Jul-2013 (64-bit gnuplot) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
qplot2 inp1a proc=(1, 1,1,20,20)  title=("QPLOT2 TEST 7 - SLANT PROFILE")  +
   xtitle=("DN Position") ytitle=("DN Value")  +
   xscale=(1,30) yscale=(1,100) xlength=4 ylength=3 labelsiz=8  +
    plotout=test7
Beginning VICAR task qplot2
qplot2 version 12-Jul-2013 (64-bit gnuplot) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
qplot2 inp1a proc=(1, 1,1,20,20)  title=("QPLOT2 TEST 8 - SLANT PROFILE")  +
   xtitle=("DN Position") ytitle=("Normalized DN Value")  +
   norm="norm" plotout=test8
Beginning VICAR task qplot2
qplot2 version 12-Jul-2013 (64-bit gnuplot) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
qplot2 inp1a proc=(1, 1,1,20,20)  title=("QPLOT2 TEST 9 - SLANT PROFILE")  +
   xtitle=("Relative DN Position") ytitle=("DN Value")  +
   tics="notics" plotout=test9
Beginning VICAR task qplot2
qplot2 version 12-Jul-2013 (64-bit gnuplot) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
qplot2 inp1a proc=(1, 1,1,20,20)  title=("QPLOT2 TEST 10 - SLANT PROFILE")  +
   xtitle=("Relative DN Position") ytitle=("DN Value")  +
    rds=20.0 plotout=test10
Beginning VICAR task qplot2
qplot2 version 12-Jul-2013 (64-bit gnuplot) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
qplot2 inp1a proc=(1, 1,1,20,20)  title=("QPLOT2 TEST 11 - SLANT PROFILE")  +
   xtitle=("Relative DN Position") ytitle=("DN Value")  +
    rds=-5.0 plotout=test11
Beginning VICAR task qplot2
qplot2 version 12-Jul-2013 (64-bit gnuplot) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
let $echo="no"
Multiple lines on one band
qplot2 (inp1,inp2) proc=(1,1,1,20,20, 2,1,1,20,20) title=("QPLOT2 TEST 12")  +
    plotout=test12
Beginning VICAR task qplot2
qplot2 version 12-Jul-2013 (64-bit gnuplot) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
qplot2 (inp1,inp2) proc=(1,1,1,20,20, 2,1,1,20,20) title=("QPLOT2 TEST 13")  +
    xscale=(1,40) yscale=(1,255)  plotout=test13
Beginning VICAR task qplot2
qplot2 version 12-Jul-2013 (64-bit gnuplot) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
qplot2 (inp1,inp2) proc=(1,1,1,20,20, 2,1,1,20,20) title=("QPLOT2 TEST 14")  +
    xscale=(1,25) yscale=(1,50) plotout=test14
Beginning VICAR task qplot2
qplot2 version 12-Jul-2013 (64-bit gnuplot) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
qplot2 (inp2,inp3) proc=(1,1,1,20,20, 2,50,50,70,70) title=("QPLOT2 TEST 15")  +
    xscale=(1,25) yscale=(1,255) plotout=test15
Beginning VICAR task qplot2
qplot2 version 12-Jul-2013 (64-bit gnuplot) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
qplot2 (inp1,inp2,inp3) proc=(1,1,1,20,20, 2,1,1,20,20, 3,50,50,70,70)  +
        title=("QPLOT2 TEST 16") plotout=test16
Beginning VICAR task qplot2
qplot2 version 12-Jul-2013 (64-bit gnuplot) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
qplot2 (inp1,inp2,inp3,inp1a)  title=("QPLOT2 TEST 17 - Four Images")   +
    proc=(1,1,1,20,20, 2,1,1,20,20, 3,50,50,70,70, 4,1,1,20,20)  +
    plotout=test17
Beginning VICAR task qplot2
qplot2 version 12-Jul-2013 (64-bit gnuplot) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
gen half1 nl=20 ns=20 linc=20 modulo=32767 format=half
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen half1b nl=20 ns=20 linc=20 modulo=32767 format=half
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen half1a nl=40 ns=40 linc=200 sinc=2 ival=20 modulo=2048 format=half
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen half2 nl=20 ns=20 linc=100 sinc=10 ival=44 modulo=3096 format=half
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen half3 nl=128 ns=128 linc=50 sinc=5 ival=75 modulo=1024 format=half
Beginning VICAR task gen
GEN Version 6
GEN task completed
let $echo="no"
HALF WORD data sets
qplot2 (half1,half2,half3,half1a) title=("QPLOT2 TEST 18")  +
    proc=(1,1,1,20,20, 2,1,1,20,20, 3,50,50,70,70, 4,1,1,20,20)  +
    plotout=test18
Beginning VICAR task qplot2
qplot2 version 12-Jul-2013 (64-bit gnuplot) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
qplot2 (half1,half1b) proc=(1,1,1,20,20, 2,1,1,20,20)  +
    title=("QPLOT2 TEST 19 - LINE 2 displaced by 5 upward")  +
    displace=5 plotout=test19
Beginning VICAR task qplot2
qplot2 version 12-Jul-2013 (64-bit gnuplot) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
gen full1 nl=20 ns=20 linc=30 modulo=512 format=full
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen full1b nl=20 ns=20 linc=20 modulo=32767 format=full
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen full1a nl=40 ns=40 linc=100 sinc=2 ival=20 modulo=2048 format=full
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen full2 nl=20 ns=20 linc=100 sinc=10 ival=44 modulo=3096 format=full
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen full3 nl=128 ns=128 linc=50 sinc=5 ival=75 modulo=1024 format=full
Beginning VICAR task gen
GEN Version 6
GEN task completed
let $echo="no"
FULL WORD data sets
qplot2 (full1,full2,full3,full1a) title=("QPLOT2 TEST 20")  +
    proc=(1,1,1,20,20, 2,1,1,20,20, 3,50,50,70,70, 4,1,1,20,20)  +
    plotout=test20
Beginning VICAR task qplot2
qplot2 version 12-Jul-2013 (64-bit gnuplot) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
qplot2 (full1,full2,full3,full1a) title=("QPLOT2 TEST 21")  +
    proc=(1,1,1,20,20, 2,1,1,20,20, 3,50,50,70,70, 4,1,1,20,20)  +
    yscale=(0,5000.) plotout=test21
Beginning VICAR task qplot2
qplot2 version 12-Jul-2013 (64-bit gnuplot) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
gen real1 nl=20 ns=20 linc=30 modulo=512 format=real
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen real1b nl=20 ns=20 linc=20 modulo=32767 format=real
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen real1a nl=40 ns=40 linc=100 sinc=2 ival=20 modulo=2048 format=real
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen real2 nl=20 ns=20 linc=100 sinc=10 ival=44 modulo=3096 format=real
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen real3 nl=128 ns=128 linc=50 sinc=5 ival=75 modulo=1024 format=real
Beginning VICAR task gen
GEN Version 6
GEN task completed
let $echo="no"
REAL data sets
qplot2 (real1,real2,real3,real1a) title=("QPLOT2 TEST 22")  +
    proc=(1,1,1,20,20, 2,1,1,20,20, 3,50,50,70,70, 4,1,1,20,20)  +
    plotout=test22
Beginning VICAR task qplot2
qplot2 version 12-Jul-2013 (64-bit gnuplot) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
let $echo="no"
MIXED data sets
qplot2 (inp1,half2,full3,real1a)   +
    proc=(1,1,1,20,20, 2,1,1,20,20, 3,50,50,70,70, 4,1,1,20,20)  +
    title=("QPLOT TEST 23 -MIXED Data Types - BYTE,HALF,FULL,REAL")  +
    plotout=test23
Beginning VICAR task qplot2
qplot2 version 12-Jul-2013 (64-bit gnuplot) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
gen b1 nl=50 ns=50
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen b2 nl=50 ns=50 ival=5 linc=1 sinc=1
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen b3 nl=50 ns=50 ival=20 linc=2 sinc=2
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen b4 nl=50 ns=50 ival=40 linc=3 sinc=3
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen b5 nl=50 ns=50 ival=100 linc=3 sinc=3
Beginning VICAR task gen
GEN Version 6
GEN task completed
mss (b1,b2,b3,b4) out=mss4a
Beginning VICAR task mss
* OUTPUT CONTAINS   4INTERLEAVED DATA SETS **
* ACTUAL OUTPUT RECORD LENGTH    200SAMPLES **
mss (b5,b2,b3,b4) out=mss4b
Beginning VICAR task mss
* OUTPUT CONTAINS   4INTERLEAVED DATA SETS **
* ACTUAL OUTPUT RECORD LENGTH    200SAMPLES **
tran mss4a spec4a outorg=bsq nbands=4
Beginning VICAR task tran
TRAN version 06-04-98
let $echo="no"
MSS data sets
qplot2 mss4a proc=(1,1,1,20,20) nchan=2  +
    title=("QPLOT2 TEST 24 - One Line from Channel 1 - MSS Data")  +
    plotout=test24
Beginning VICAR task qplot2
qplot2 version 12-Jul-2013 (64-bit gnuplot) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
qplot2 mss4a sproc=(10,10) nchan=4  +
    title=("QPLOT2 TEST 25 - Spectral Plot")  +
    plotout=test25
Beginning VICAR task qplot2
qplot2 version 12-Jul-2013 (64-bit gnuplot) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
qplot2 mss4a sproc=(10,10,10,11,11,10,11,11) nchan=4  +
    title=("QPLOT2 TEST 26 - Spectral Plot on MSS image")  +
    plotout=test26
Beginning VICAR task qplot2
qplot2 version 12-Jul-2013 (64-bit gnuplot) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
qplot2 mss4a sproc=(10,10,10,11,11,10,11,11) nchan=3  +
    title=("QPLOT2 TEST 27 - Spectral Plot 3 channels on MSS image")  +
    plotout=test27
Beginning VICAR task qplot2
qplot2 version 12-Jul-2013 (64-bit gnuplot) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
let $echo="no"
MULTIBAND images  - should FAIL
let _onfail="continue"
qplot2 spec4a sproc=(10,10,10,11,11,10,11,11) nchan=4  +
    title=("QPLOT2 TEST 28 - Spectral Plot on Multiband image")
Beginning VICAR task qplot2
qplot2 version 12-Jul-2013 (64-bit gnuplot) - rjb
??E - Multiband images not supported
      Convert to MSS format with TRAN
 ** ABEND called **
continue
let _onfail="stop"
let $echo="no"
Test eps output
qplot2 mss4a sproc=(10,10,10,11,11,10,11,11) nchan=4  +
    title=("QPLOT2 TEST 28X - Spectral Plot on MSS image")  +
    plotout="yes" plotfmt="eps"
Beginning VICAR task qplot2
qplot2 version 12-Jul-2013 (64-bit gnuplot) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
ush gnuplot qplot.eps.gpi
qplot2 mss4a sproc=(10,10,10,11,11,10,11,11) nchan=4  +
    title=("QPLOT2 TEST 29 - Spectral Plot on MSS image")  +
    plotout="test29" plotfmt="eps"
Beginning VICAR task qplot2
qplot2 version 12-Jul-2013 (64-bit gnuplot) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
ush gnuplot test29.eps.gpi
let $echo="no"
exit
slogoff
$ Return
$!#############################################################################
