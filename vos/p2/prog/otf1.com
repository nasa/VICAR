$!****************************************************************************
$!
$! Build proc for MIPL module otf1
$! VPACK Version 2.1, Wednesday, January 13, 2016, 13:52:27
$!
$! Execute by entering:		$ @otf1
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
$ write sys$output "*** module otf1 ***"
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
$ write sys$output "Invalid argument given to otf1.com file -- ", primary
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
$   if F$SEARCH("otf1.imake") .nes. ""
$   then
$      vimake otf1
$      purge otf1.bld
$   else
$      if F$SEARCH("otf1.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake otf1
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @otf1.bld "STD"
$   else
$      @otf1.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create otf1.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack otf1.com -mixed -
	-s otf1.f -
	-i otf1.imake -
	-p otf1.pdf -
	-t tstotf1.pdf tstotf1.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create otf1.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C-------PROGRAM OTF1
C 05 AUG 1997 ...TXH... Ported from VAX to MIPS Env.  Merged changes
C                       made by CCA and the previous MIPS version.
C 10 JUL 1995 ...CRI... MSTP S/W CONVERSION (VICAR PORTING)
C
c 26 dec 2009   -lwk-   defined TRANBUF so that the xvtrans calls work;
c			fixed bug in "line printer" plot that caused infinite
c			loop
c 2 Nov 2012  ... R.J. BAMBERY - completely reorganized  for 64-bit
c			LINUX and gnuplot and more modern psf analysis
c
c	image = image(inlines,insamps)
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
	implicit none

         REAL*4 R(1026),PARMS(512),AMPL9(131)
         REAL*4 CPLOT(131),PPLOT(131),AMPL10(131),WSFT9(129)
         REAL*4 F29(129),REALV(129),AIMAGV(129),RINL
         REAL*4 AMPLSUM,NORMSUM,DENM,crealv(129)
         REAL*4 PI,OBUF(3,400)
	real*4 time1,timint,pzoom
	real*4 norangle,gsd,altitude
	real*8 radtodeg,psfsize
	real*8 dgsd,daltitude,pixrad,pixdeg,psfrad,psfdeg

         INTEGER*2 IBUF(4096),IR(4096)
         INTEGER*4 CNT,IUNI,STAT,OUNI,OSTAT,ZOOMFLAG
         INTEGER*4 NUMBER_OF_LINES_WITHOUT_EDGE,NOPRT,NHEADR(70)			!,STATUS
	integer*4 count,i,i1,icnt,iesf,idef,jj
	integer*4 ilsf,inlines,ins,insamps,insm1,line_cnt
	integer*4 instat,ipic,iplot,inl,ipower,idiag
	integer*4 isl,iss,isso,itable,itst,j,ksamps,key,key4
	integer*4 leftl,lines_pro,linlas,lino,midpt,itmp
	integer*4 n2,jst,noise,nptsin,ntot,psfpix,isum,i2sum
	integer*4 ntotol,tranbuf(12),profplot,nplotprofname
	integer*4 ninfile,esfmax,esfmin,esfmidpt,psfmidpt 
	integer*4 nplotgpi,nplotgpi2,nploteps,nplotname,nimess
 	integer*4 nplotprofgpi,nplotprofgpi2,nplotprofeps
	integer*4 nproftab,lsfmin,lsfmax,nptbl
	integer*4 nmcycle,nmphase,imgpsf_cnt(1026)
	integer*4 imgpsf_val(1026),imgpsf_tmp(1026,1026),imgpsf_avcnt
	integer*4 imgpsf_max(1026),imgpsf_midpt(1026),psf_long(1026)
	integer*4 imgesf_val(1026),imgesf_tmp(1026,1026)
	logical*4 XVPTST
	logical*4 epsplot,phaseplot,profout,pepsplot,aliased
c	character*1 two/'2'/
        character*4 gpi/'.gpi'/,eps/'.eps'/
	character*4 asc/'.asc'/,tbl/'.tbl'/
	character*5 prof/'.prof'/
	CHARACTER*1   TABCHAR
	character*8 plotfmt
	CHARACTER*9   BL
	CHARACTER*40  MCYCLE,MPHASE
	CHARACTER*60  INFILE,OUTFILE,TABLE,proftab,ptbl		!,esfproftab
	CHARACTER*63 plotname,plotprofname
	character*80 plotgpi,plotgpi2,ploteps,plotprofgpi,plotprofgpi2
	character*80 plotprofeps
         CHARACTER*86  HEADR(70),IMESS
	CHARACTER*101 PLOT_DS,LOG,PLOTPROF
         CHARACTER*121 LO
c         CHARACTER*140 ZPBUF
c
c	Fortran units
c
c	11  - ascii table of fft (129 steps) - half of full fft
c	12  - profile table output
c	13  - ascii table file for profile plot
c	90  - ascii table file for plotout 
c	95  - gpi instructions for profile plot
c	96  - gpi instructions for profile eps plot
c	97  - gpi instructions for ploteps
c	98  - gpi instructions for plotout

c        include 'gnuplotchar'

         DATA PI/3.1415926536E0/
c
        call xvmessage ('OTF1 version 2015-08-10',' ')
c


C***************************
C     SET DEFAULTS AND INIT BUFFERS
C***************************
	aliased = .false.
	TABCHAR=CHAR(9)
         DO I = 1,101
            LOG(I:I) = '1'
            plot_ds(i:i) = ' '
         END DO

         DO I= 1,121
            LO(I:I) = '1'
         END DO
         LO(1:20) = '                    ' !

         DO  I=1,129
            REALV(I)=0.0E0
            AIMAGV(I)=0.0E0
	    WSFT9(I)=0.0
	    F29(I) = 0.0
	    crealv(i) = 0.0
         ENDDO
         DO  I=1,4096
            IBUF(I)=0
	    IR(I) =0
         ENDDO
         DO  I=1,1026           !,4488
            R(I)=0.0E0
	   imgpsf_cnt(i) = 0
	   imgpsf_val(i) = 0.0
	   imgesf_val(i) = 0.0
	   imgpsf_max(i) = -100
	   imgpsf_midpt(i) = 0	 
	   psf_long = 0  	   
         ENDDO
	 do i=1,1026
 	    do i1=1,1026
	      imgpsf_tmp(i,i1) = 0.0
	      imgesf_tmp(i,i1) = 0.0
	    enddo
	 enddo
	 do i=1,131
	    AMPL9(I) = 0.0
	    CPLOT(i) = 0.0
	    PPLOT(i) = 0.0
	    AMPL10(i) = 0.0
	 enddo
	do i=1,512
	    PARMS(I) = 0.0
        enddo	
         AMPLSUM=0.0E0
         NORMSUM=0.0E0
         COUNT=0.0
         NPTSIN=0
	imgpsf_avcnt = -100
c	FLAGS
         KEY=0         !KEY=1=REFLECT DATA/KEY=2=MEAN FILLIN
         KEY4=1        !KEY4=1=NORMALIZE /KEY4=0= DON'T
         ILSF=0         !=1 input is LSF
         IESF=0        !=1 input is ESF
         IDIAG=0       !=1 print out diagnostics
         IPLOT=0        !=1 Do gnuplot plot
         ITST=0         !=1 DO SINC TEST
         NUMBER_OF_LINES_WITHOUT_EDGE=0
	rinl = 1.0      ! number of lines processed
	lsfmax = 0
	lsfmin = 0
c
	ksamps=0
	do i=1,70
	  nheadr(i) = 1
	  headr(i) = ' '
	enddo
	infile = ''			!input file name		(IUNI)
	ninfile = 0
	outfile	=''			!OUT Filename and extension	(OUNI)
	epsplot = .false.
	pepsplot = .false.
        profout = .false.

	IPLOT=0        			!=1 Do gnuplot plot 
	profplot = 0
	plotname = ' '			!PLOTOUT basefile name
	nplotname = 1
	plotgpi = ' '			!PLOTOUT gpi instructions	(UNIT 98)
	nplotgpi = 0
	plotgpi2 = ' '			!PLOTOUT GPI instructions for eps files (UNIT 97)
	nplotgpi2 = 0
	ploteps=' '			!PLOTOUT eps filename
        nploteps = 1
c  the following table file contains the points to plot for plotgpi and plotgpi2
c       tbl = ' '			!PLOTOUT data file		(UNIT 90) - in subroutine do_plot
c	ntbl = 1					

	proftab = ''			!TABPSF filename		(UNIT 12)
	nproftab = 0

	table = ' '			!TABLE filename and extension	(UNIT 11)

	plotprofname = ' '		!PLOTPROF basefile name
	nplotprofname = 0	
        plotprofgpi = ' '		!PLOTPROF gpi instructions	(UNIT 95)
	nplotprofgpi = 0
        nplotprofgpi2 = 0		!PLOTPROF gpi instructions for eps files (UNIT 96)
        plotprofeps = ' '		!PLOTPROF eps filename
        nplotprofeps = 1
c  the following table file contains the points to plot for plotpsfgpi and plotpsfgpi2
        ptbl= ' '                  	!PLOTPROF gpi data file              (UNIT 13)
        nptbl = 1 

	psfmidpt = 0

         BL='         '
         IMESS='INTEGRATED MTF AMPLITUDE FROM .25 TO .48 = '
	nimess=index(IMESS,'    ') - 1
         MPHASE='PHASE (RAD) multiplied *'
	nmphase = index(MPHASE,'    ') - 1
         MCYCLE='CYCLES PER PIXEL  multiplied *' 
	nmcycle = index(MCYCLE,'    ') - 1
        instat = 0
        IDEF = 0
         IPIC=0                 !=1 IMG in
         ISL=0
         ISS=0
         INL=0
         INS=0
         INLINES=0
         INSAMPS=0
         LINLAS=0
         LINO=0


c	SETUP TRANSLATION BUFFER
c
         CALL XVTRANS_SET(TRANBUF,'HALF','REAL',STAT)
         IF (STAT .NE. 1) CALL MABEND('??E - XVTRANS BUFFER SETUP UNSUCCESSFUL')

c
C*************************************
C     VICAR PARAM PROCESSOR
C*************************************
c
	 CALL XVP ('PLOTFMT',plotfmt,cnt)
	IF (XVPTST('DIAG')) IDIAG=1		!Diagnostics ??
         CALL XVP('INTERVAL',TIMINT,CNT)
         CALL XVP('NOISE',NOISE,CNT)


         IF (XVPTST('REFLECT')) KEY=1
         IF (XVPTST('MEAN')) KEY=2
         IF (XVPTST('NONORMAL')) KEY4=0

c	print *,'KEY4 = ',key4
c
c new params
	CALL XVP ('NORANGLE',norangle,CNT)	
	CALL XVP ('GSD',gsd,CNT)
	CALL XVP ('ALTITUDE',altitude,CNT)
c  radians per pixel - center of pixel to edge is half a pixel
	dgsd = dble(gsd)
	daltitude = dble(altitude)
	if (daltitude .gt. 0.0d0) then
	    pixrad = 2.0d0*datan2(dgsd,daltitude)	!arctan2(x/y) = x-dir/y-dir = sample/line
	    pixdeg = radtodeg(pixrad)
c		print *,'gsd, altitude = ',gsd,altitude
c		print *,'pixrad, pixdeg = ',pixrad,pixdeg
	endif
C******************     
C  PLOT LSF vs PSF in pixels    
C******************
C
        call xvp ('PLOTPROF',PLOTPROF,stat)
          if (plotprof .eq. "YES" .or. plotprof .eq."yes") then
                profplot = 1
c               pepsplot = .true.
                plotprofname ='plotprof'
                nplotprofname=index(plotprofname,'   ') - 1
                plotprofgpi  =plotprofname(1:nplotprofname)//prof//gpi
                nplotprofgpi =index(plotprofgpi,'  ') - 1
                plotprofgpi2 =plotprofname(1:nplotprofname)//prof//eps//gpi
                nplotprofgpi2=index(plotprofgpi2,'  ') - 1
                plotprofeps  =plotprofname(1:nplotprofname)//prof//eps
                nplotprofeps =index(plotprofeps,'  ') - 1
		ptbl = plotprofname(1:nplotprofname)//prof//asc
                nptbl = index(ptbl,'  ') - 1

c               Plotout and nplotout from above
            elseif (plotprof .eq. "NONE" .or. plotprof .eq."none") then
               pepsplot = .false.
		plotprofname='plotprof'
                nplotprofname=index(plotprofname,'   ') - 1
               plotprofgpi='plotprof.gpi'
               nplotprofgpi=index(plotprofgpi,'  ') - 1
	       ptbl = 'plotprof.asc'
               nptbl = index(ptbl,'  ') - 1

            else
                profplot = 1
               plotprofname = plotprof
               nplotprofname=index(plotprofname,'   ') - 1
               plotprofgpi=plotprofname(1:nplotprofname)//prof//gpi
               nplotprofgpi=index(plotprofgpi,'  ') - 1
               plotprofgpi2=plotprofname(1:nplotprofname)//prof//eps//gpi
               nplotprofgpi2=index(plotprofgpi2,'  ') - 1
                plotprofeps=plotprofname(1:nplotprofname)//prof//eps
               nplotprofeps=index(plotprofeps,'  ') - 1
		ptbl = plotprofname(1:nplotprofname)//prof//asc
                nptbl = index(ptbl,'  ') - 1

               if (plotfmt .eq. 'EPS' .or. plotfmt .eq. 'eps') pepsplot = .true.
            endif
cc        print *, 'psfplot = ',psfplot

c
c************************************
c	PLOT Parameters Section
c	PLOTOUT, PZOOM, and PHASE
c************************************
c
         CALL XVP ('PLOTOUT',PLOT_DS,CNT)
c       print *,'cnt,plot_ds = ', cnt,plot_ds
c        IF (IDEF .EQ. 0) THEN
            if (plot_ds .eq. "YES" .or. plot_ds .eq."yes") then
                iplot = 1
c               epsplot = .true.
                plotname='otfplot'
                nplotname=index(plotname,'   ') - 1
                plotgpi=plotname(1:nplotname)//gpi
                nplotgpi=index(plotgpi,'  ') - 1
                plotgpi2=plotname(1:nplotname)//eps//gpi
                nplotgpi2=index(plotgpi2,'  ') - 1
                ploteps=plotname(1:nplotname)//eps
                nploteps=index(ploteps,'  ') - 1
c               Plotout and nplotout from above
            elseif (plot_ds .eq. "NONE" .or. plot_ds .eq."none") then
               epsplot = .false.
               plotname='otfplot'
               nplotname=index(plotname,'   ') - 1
               plotgpi='otfplot.gpi'
               nplotgpi=index(plotgpi,'  ') - 1
            else
                iplot = 1
               plotname = plot_ds
               nplotname=index(plotname,'   ') - 1
               plotgpi=plotname(1:nplotname)//gpi
               nplotgpi=index(plotgpi,'  ') - 1
               plotgpi2=plotname(1:nplotname)//eps//gpi
               nplotgpi2=index(plotgpi2,'  ') - 1
                ploteps=plotname(1:nplotname)//eps
               nploteps=index(ploteps,'  ') - 1
               if (plotfmt .eq. 'EPS' .or. plotfmt .eq. 'eps') epsplot = .true.
            endif


         CALL XVPARM ('PZOOM',PZOOM,CNT,IDEF,1)
         IF (IDEF .EQ. 0) THEN
             ZOOMFLAG=1
         ELSE
             ZOOMFLAG=0
         ENDIF

        phaseplot=.true.
        IF (XVPTST('NOPHASE')) phaseplot=.false.


C
C*************************************
C     FOUR POSSIBLE FORMS OF INPUT, mutually exclusive
C	  SINCTST
C	  LSF
C	  ESF
C	  INPUT IMAGE
C*************************************
C
C
c*************************************
c     1 - SINCTST SECTION
c*************************************
         IF (XVPTST('SINCTST')) ITST=1
         IF (ITST .EQ. 1) THEN        !TEST WITH SINC
c	    print *,'sinctst **********'
             LINO=1
             NPTSIN=60
             INS=NPTSIN
             INSM1=INS-1
             ILSF=1                     !tell user we have LSF Data
             NTOT=64
             IPOWER=6
             DO  I=1,60
                DENM=(2.0E0*PI*(FLOAT(I)-30.5E0)/5.0E0)
                R(I)=(SIN(DENM)/DENM)**2   !TAB LSF
             ENDDO
             infile = 'input.sinctst'
             ninfile = index(infile,'  ') - 1
         ENDIF
	line_cnt = 1
	

c	print *,'going to 210'
c       end sinc section
         IF (itst .EQ. 1) GOTO 24        !PAR STRING ITST INPUT
c
c*************************************
c     2 - LINE SPREAD FUNCTION (LSF) SECTION
c*************************************
c
         CALL XVP('LSF',PARMS,CNT)     !INPUTS LSF REAL*4 DATA
         IF (CNT .GT. 0) then
             NPTSIN=CNT
             DO  I=3,12
                I1=I
                IF (NPTSIN .LE. 2**I) GOTO 5
             ENDDO
5       CONTINUE
             NTOT=2**I1
             ins=nptsin
	     lsfmin = PARMS(1)
	     lsfmax = PARMS(1)
             DO  I=1,NPTSIN
                R(I)=PARMS(I)
	        IF (R(I) .LT.LSFMIN) LSFMIN=R(I)
	        IF (R(I) .GT.LSFMAX) LSFMAX=R(I)
             ENDDO
             IPOWER=I1
             ILSF=1                     !we have LSF entered
             LINO=1
	     LINLAS=1
             line_cnt = 1
             RINL = 1.0
             LINES_PRO = 1
             CALL PRNT (4,1,LINES_PRO,'NUMBER OF LINES PROCESSED=. ')

c             infile = 'input.lsf'
	     infile = 'input.lsf'
             ninfile = index(infile,'  ') - 1
         endif
	IF (ilsf .EQ. 1) GOTO 24        !PAR STRING LSF INPUT
c
c*************************************
c     3 - EDGE SPREAD FUNCTION (ESF) SECTION
c*************************************
c
         CALL XVP ('ESF',PARMS,CNT)    !INPUTS RAW REAL*4 DATA
         IF (CNT .GT. 0) THEN
c	    check to see if input has a number of samples in a power
c		of two
            line_cnt = 1
	    LINLAS = 1	
            NPTSIN=CNT
            DO  I=3,12
               I1=I
               IF (NPTSIN .LE. 2**I) GOTO 105
            ENDDO
105     CONTINUE
            NTOT=2**I1
            INS=NPTSIN
            INSM1=INS-1
            IPOWER=I1
            IESF=1                     !we have EDGE spread data hand-entered
            LINO=1
            DO  I=1,NPTSIN
               R(I)=PARMS(I)
	       IR(i)=int(parms(i))
            ENDDO
            call accum_esf (ir,line_cnt,ins,imgesf_tmp)
cc	do i=1,ins
cc	   print *,'i,ir(i)) = ',i,ir(i)	
cc	enddo
            LEFTL=0
            IF (R(1) .GT. R(INS)) LEFTL=1  !FIND DIRECTION OF GRADIENT
            IF (LEFTL .EQ. 1) GOTO 500
C****************************
C     DIFFERENTIATE TO GET LSF FROM ESF
C****************************
            IF (IDIAG .EQ. 1) CALL PRNT(4,1,INSM1,'NUMBER OF ELEMENTS IN LSF= ')
            DO I=1,insm1			!INSM1
cc		print *,R(I+1),r(i)
               R(I) = R(I+1)-R(I)
cc		print *,'ri = ',i,int(r(i))
            ENDDO
            GOTO 503

500     continue
            DO  I = 1,insm1		!INSM1
               R(I) = R(I)-R(I+1)
            ENDDO
503     CONTINUE
	    R(insm1+1)=0.0

            lsfmin = R(1)
            lsfmax = R(1)
            DO  I=1,INSM1
               IF (R(I) .LT.LSFMIN) LSFMIN=R(I)
               IF (R(I) .GT.LSFMAX) LSFMAX=R(I)
	    ENDDO
C
c    get midpoint of edge spread function
	    esfmin=72000
            esfmax= -100 !
            esfmidpt = 0
            do i=1,insm1
                if (int(parms(i)).gt.esfmax) then
                    esfmax = int(parms(i))
                endif
	        if (int(parms(i)).lt.esfmin) then
		    esfmin = int(parms(i))
	        endif
            enddo
            itmp = (esfmax - esfmin)/2  + esfmin
cc		print *,'itmp = ',itmp
	     do i=1,insm1
cc		print *,'itmp int(parms(i)) = ',itmp,int(parms(i))
	        if (itmp .le. int(parms(i))) then
		   esfmidpt = i 
		   go to 505
	        endif
	     enddo
505	continue
	     itmp=0
cc	print *,'esfmax, esfmidpt = ',esfmax,esfmidpt	
c             infile = 'input.esf'
	     infile = 'input.esf'
             ninfile = index(infile,'  ') - 1
	     RINL = 1.0	
	     LINES_PRO = 1
             CALL PRNT (4,1,LINES_PRO,'NUMBER OF LINES PROCESSED=. ')
c        call minim_lsf (ntot,line_cnt,r,imgpsf_cnt,imgpsf_tmp,psfmax,psfmidpt)

c
        ENDIF   ! IF (CNT .GT. 0) THEN  FOR ESF input
	IF (IESF .EQ. 1) GOTO 24       !PAR STRING REAL INPUT
c
c*************************************
c     4 - INPUT IMAGE SECTION
c*************************************
c
C
C**************************
C     OPEN INPUT IF THERE, AND GET PROCESSING AREA BRANCH TO 200 IF INPUT 
C     IS LSF OR DATA OPEN OUTPUT IF NEEDED
C**************************
C
         CALL XVUNIT (IUNI,'INP',1,INSTAT,' ')
c	print *, 'instat = ',instat
         IF (INSTAT .NE. 1) then
            call xvmessage('??E - None of the four possible inputs found',' ' )
            call abend
	 ENDIF	
         CALL XVP('INP',INFILE,ICNT)
	 ninfile = index(infile,'    ') - 1

c	print *,'infile = ',infile(1:ninfile)


         CALL XVOPEN(IUNI,STAT,'OPEN_ACT','SA','IO_ACT','SA',
     &            'U_FORMAT','HALF',' ')
         IPIC=1
         CALL XVSIZE(ISL,ISS,INL,INS,INLINES,INSAMPS)
c	print *,'isl,iss,inl,ins,inlines,insamps = ',isl,iss,inl,ins,inlines,insamps
         IF (INL .GT. INLINES)
     &      CALL XVMESSAGE ('INL Truncated to Image Length',' ')
         IF (INL .GT. INLINES) INL=INLINES
         IF(INS .GT. INSAMPS)
     &      CALL XVMESSAGE ('INS Truncated to Image Width',' ')
         IF(INS .GT. INSAMPS) INS=INSAMPS

         INSM1=INS-1      !THIS IS NEEDED FOR LSF COUNTS
         LINO=ISL
         LINLAS=ISL+INL-1			!last line to be read in
         IF (LINLAS .GT. INLINES)
     &      CALL MABEND ('??E - TOO MANY LINES!!!!')
         IF (INS-ISS .GT. INSAMPS)
     &      CALL MABEND ('??E - TOO MANY SAMPS!!!!')
         ISSO=ISS
C***************************
c
c

C******************************
C     BEGIN DATA ACQUISITION
C******************************
cc	print *,'before branches --'
c         IF (IPIC .EQ. 1) GOTO 10        !IMAGE INTEGER INPUT
c         IF (IESF .EQ. 1) GOTO 24       !PAR STRING REAL INPUT
c         IF (ILSF .EQ. 1) GOTO 24        !PAR STRING LSF INPUT
c10       CONTINUE
c
c	****************************************************************
c     LOOP FOR READING IN DATA *****************************************
c	****************************************************************
c	lino = line number of image, line_cnt is subsection line number
c
	line_cnt = 0
11       CONTINUE
c	print *,'at 11 continue'
c	line_cnt = 0
         CALL XVREAD(IUNI,IBUF,STAT,'LINE',LINO,'SAMP',ISSO,
     &               'NSAMPS',INS,' ')
         IF (IDIAG .EQ. 1) THEN
             CALL PRNT(2,INS,IBUF,'IBUF=. ')
             DO  I=1,INS                                
                CALL PRNT(4,1,I,'I=. ')
                CALL PRNT(2,1,IBUF(I),'IBUF(I)=. ')
	     ENDDO
         ENDIF  
	 line_cnt = line_cnt + 1
c   accumulate all rows of esf
c	print *,'accum_esf'
	 call accum_esf (ibuf,line_cnt,ins,imgesf_tmp)
c
c	find lsf in image 
c
	 call find_lsf (ibuf,tranbuf,insm1,idiag,noise,lino,nptsin,
     1 ntot,ipower,ins,lsfmin,lsfmax,r)


24      CONTINUE
c       come here fpr all 4 types of entry
c	optimize lsf - expand to 256 points 
c	REALV and AIMAGV are real and imaginary parts of optimized FFT
c	They have n2 points
c	print *,'optim_lsf'
	call optim_lsf (ntot,nptsin,n2,key,key4,lino,line_cnt,idiag,itst,
     1 ipower,number_of_lines_without_edge,ntotol,time1,timint,r,
     2 midpt,imgpsf_tmp,imgpsf_cnt,imgpsf_max,imgpsf_midpt,realv,aimagv)

         IF ((ILSF .EQ. 1) .OR. (IESF .EQ. 1)) GOTO 49

	 LINO=LINO+1		!bump image line number for next read
cccccc --- note jumps out of loop for DATA or LSF entry
c         IF ((ILSF .EQ. 1) .OR. (IESF .EQ. 1)) GOTO 49		
         IF (LINO .LE. LINLAS) GOTO 11     !GO READ ANOTHER LINE if not at end
c
c	*************************************************************************
c    FINISH LOOP FOR READING IN DATA ********************************************
c	*************************************************************************
c
c49       CONTINUE
         IF (ISL .NE. 1) THEN
            LINES_PRO = LINLAS-ISL+1-NUMBER_OF_LINES_WITHOUT_EDGE
            RINL=FLOAT(LINES_PRO)      !NUMBER OF LINES PROCESSED
            IF (ITST .EQ. 0)
     &         CALL PRNT
     &              (4,1,LINES_PRO,'NUMBER OF LINES PROCESSED=. ')
         ELSE
               LINES_PRO = LINO-1-NUMBER_OF_LINES_WITHOUT_EDGE 
               RINL=FLOAT(LINES_PRO)    !NUMBER OF LINES PROCESSED
               CALL PRNT 
     &              (4,1,LINES_PRO,'NUMBER OF LINES PROCESSED=. ')
         ENDIF

49       CONTINUE
c	print *,'after 49 continue'
C     PRINT OUT TRANSFORM VALUES - half of 256 step PSF
C***************************
C-----IF OUTPUT ASCII TABLE REQUIRED of full FFT?
c	 FREQUENCY     AMPLITUDE     PHASE (optional)
	    CALL XVPARM('TABLE',TABLE,ICNT,IDEF,1)
	    IF(IDEF .EQ. 0) THEN
	       ITABLE = 1
c 	       TABCHAR=CHAR(9)
	       OPEN(11,FILE=TABLE,STATUS='UNKNOWN',IOSTAT=JST,ERR=999)
               IF (XVPTST('COLHDR')) THEN
                  IF (XVPTST('PHASE')) 
     &               WRITE(11,904)
     &            '# FREQUENCY',TABCHAR,'   AMPLITUDE',TABCHAR,'   PHASE'
                  IF (XVPTST('NOPHASE')) 
     &               WRITE(11,905) '# FREQUENCY',TABCHAR,'AMPLITUDE'
               ENDIF
	    ENDIF

904	    FORMAT(1X,A11,A1,A12,A1,A8)
905	    FORMAT(1X,A11,A1,A12)

C-------IS FULL PRINTOUT OF TABLE REQUIRED?
	    NOPRT=0
	    IF (XVPTST('NOPRINT')) NOPRT=1


c	print *,"before out_prep"
cccc	 prepare data for OUTFILE
	    call out_prep(noprt,n2,realv,aimagv,rinl,time1,timint,
     1 ksamps,itable,zoomflag,pzoom,ntotol,amplsum,ampl9,ampl10,f29,cplot,
     2 pplot,obuf,aliased)

	 CALL XVP('OUT',OUTFILE,OSTAT)
         IF (OSTAT .EQ. 1) THEN
            CALL XVUNIT(OUNI,'OUT',1,OSTAT,' ')
            CALL XVOPEN(OUNI,STAT,'OPEN_ACT','SA','IO_ACT',
     &                  'SA','U_FORMAT','REAL','O_FORMAT',
     &                  'REAL','U_NL',3,'U_NS',124,'OP',
     &                  'WRITE',' ')
	    do i=1,3
                CALL XVWRIT(OUNI,OBUF(1,i),STAT,'NSAMPS',KSAMPS,' ')	!samples,lines
	    enddo
            CALL XVCLOSE(OUNI,STAT,' ')
         ENDIF
c	print *, 'amplsum = ',amplsum
         WRITE (IMESS(48:56),'(F9.5)') AMPLSUM
	nimess = index(imess,'         ') - 1
         CALL XVMESSAGE(IMESS,' ')
c
c	average out imgpsf(lines,samps) to imgpsf_val(samps)
c	to put in imgpsf file imgpsf_cnt(samps) to imgpsf_avcnt
c	IGNORE the following if sinctst
	if (itst .eq. 0 ) then
c	    print *, 'AVERAGED...'
	    imgpsf_avcnt = -100
	    if (line_cnt .gt. 1) then
	        do i=1,line_cnt	
cc		    print *,'i,imgpsf_cnt(i) = ',i,imgpsf_cnt(i)
	           if (imgpsf_cnt(i) > imgpsf_avcnt) imgpsf_avcnt = imgpsf_cnt(i)
c	   imgpsf_val(i) = imgpsf_val(i) + 
	        enddo
	        i2sum = 0
	        do i=1,imgpsf_avcnt		!line_cnt
                   isum=0
	           do i1=1,line_cnt		!imgpsf_avcnt
		       isum = isum + imgpsf_tmp(i1,i)
	           enddo
	           imgpsf_val(i) = isum/line_cnt 
c		print *, '1,imgpsf_val(i) = ',i,imgpsf_val(i)
	        enddo
	    else
		imgpsf_avcnt = imgpsf_cnt(1)
		do i=1,imgpsf_avcnt
c		print *,'i,imgpsf_tmp(1,i) = ',i,imgpsf_tmp(1,i)
		     imgpsf_val(i) = imgpsf_tmp(1,i)
		enddo
	    endif
c
c	imgpsf_avcnt = 0
	call xvp ('PROFTAB',PROFTAB,stat)
c	    print *,'imgpsf = ',imgpsf
	    if (PROFTAB.eq.'YES' .or. PROFTAB.eq.'yes') then
	       profout=.true.
	       proftab=infile(1:ninfile)//prof//tbl
	       nproftab=index(proftab,'    ') - 1
	    else if (PROFTAB.eq.'NONE' .or. proftab.eq.'none') then
	       profout = .false.
	    else
	       nproftab=index(proftab,'    ') - 1
	       proftab=proftab(1:nproftab)//prof//tbl
	       nproftab=index(proftab,'    ') - 1
	       profout=.true.
	    endif
ccc	    if (profout) then
c	       print *,'psftab = ',psftab(1:npsftab)
ccc	       open(12,file=proftab(1:nproftab),status='UNKNOWN',iostat=jj,err=992)
ccc	       do i=1,imgpsf_avcnt
ccc10010 format(1x,i6,2x,i6)
cc		   print *,'imgpsf_val(i) = ',i,imgpsf_val(i)
c                write(12,fmt=10010,iostat=jj,err=992) i,imgpsf_val(i)
ccc		   write(12,10010) i,imgpsf_val(i)		 
ccc	       enddo
ccc	       close (12)
ccc	    endif

C***********************
C    SIZE IN RADIANS and DEGREES OF PSF
c***********************
c	
	    psfsize = dble(imgpsf_avcnt)*dgsd
c	    print *,'pixrad,psfsize = ',pixrad,psfsize
            psfrad = datan2(psfsize,daltitude)       !arctan2(x/y) = x-dir/y-dir = sample/line
            psfdeg = radtodeg(psfrad)
	    psfpix = imgpsf_avcnt
c	    print *,'pixrad, pixdeg, psfrad, psfdeg = ',pixrad, pixdeg, psfrad, psfdeg
	    call putparm (pixrad,pixdeg,psfrad,psfdeg,psfpix,
     1 key4,lsfmin,lsfmax)
	
	endif  !if (itst .eq. 0 )

c	print *,'after itst .eq.0'
         IF (IPLOT .EQ. 0) THEN   
C************************
C     TERMINAL SCREEN PLOTS
C************************
c
	     call term_plot (n2,lo,f29,realv,aimagv,ampl10,wsft9)
 
c	END OF TERMINAL SCREEN PLOT
c	     print *,'**********'
c	print *,'infile = ',infile(1:ninfile)
         ELSE                      
cc	     print *,'plot = 1'
c	print *,'plotgpi = ',plotgpi(1:nplotgpi)
c	print *,'infile = ',infile(1:ninfile)
cc	do i=1,n2
cc             print *,cplot(i),ampl9(i),pplot(i)
cc	enddo
C*******************
C    GNUPLOT PLOTS - plot 256 point optimimum data
C*******************
              open(98,file=plotgpi(1:nplotgpi),status='UNKNOWN',iostat=jj,err=995)

	       call do_plot (98,iuni,n2,instat,zoomflag,pzoom,
     1 imess,nimess,plotname,nplotname,ploteps,nploteps,
     2 plotgpi2,nplotgpi2,infile,ninfile,mcycle,nmcycle,mphase,nmphase,
     3 phaseplot,epsplot,iplot,timint,aliased,ampl9,pplot,cplot)

	ENDIF  !IF (IPLOT .EQ. 0)

c	print *,'instat = ',instat

         IF (INSTAT .EQ. 1) CALL XVCLOSE(IUNI,STAT,' ')
C******************	
C  PLOT LSF vs PSF in pixels	
C******************
C
        if (profout) then
           call prep_prof (instat,ins,n2,linlas,line_cnt,imgesf_val,imgpsf_midpt,
     1 imgpsf_avcnt,imgpsf_val,psf_long,imgesf_tmp,realv,crealv)
c              print *,'proftab = ',proftab(1:nproftab)
           open(12,file=proftab(1:nproftab),status='UNKNOWN',iostat=jj,err=992)
                do i=1,ins
c                print *,'i = ',i,imgesf_val(i),psf_long(i),crealv(i)
                write(12,10020) i,imgesf_val(i),psf_long(i),crealv(i)
10020 format(1x,i6,2x,i6,2x,i6,2x,f10.2)
                enddo
            close (12)
         endif

	
c	print *,'before if profplot = ',profplot
	if (profplot .eq. 1) then
	    call prep_prof(instat,ins,n2,linlas,line_cnt,imgesf_val,imgpsf_midpt,
     1 imgpsf_avcnt,imgpsf_val,psf_long,imgesf_tmp,realv,crealv)

	    call do_profplot (instat,ins,linlas,line_cnt,imgesf_val,
     1 infile,ninfile,imgpsf_avcnt,imgpsf_midpt,pepsplot,n2,realv,aimagv,
     2 crealv,imgpsf_val,psf_long,dgsd,daltitude,pixrad,pixdeg,psfrad,psfdeg,
     3 psfpix,norangle,plotprofgpi,nplotprofgpi,plotprofgpi2,nplotprofgpi2,
     4 plotprofeps,nplotprofeps,ptbl,nptbl)

	endif
c	print *,'after do_profplot'
	return
992	call xvmessage('??E - main44: Error opening PROFTAB file',' ')
c	print *,'jj = ',jj
	call abend
	return
995     call xvmessage('??E - main44: Error opening/writing gnuplot gpi file',' ')
        call abend
	return

999      CALL XVMESSAGE ('??E - ERROR OPENING TABLE FILE',' ')
         CALL PRNT(4,1,J,'IOSTAT=.')
         CALL ABEND
         RETURN
      END
C****************************************************************************
        subroutine accum_esf (ibuf,line_cnt,ins,imgesf_tmp)
c
c   accumulate esf entries 
c
	implicit none
        integer*2 ibuf(4096)
        integer*4 line_cnt,ins,i,il
        integer*4 imgesf_tmp(1026,1026)
c
            do i=1,line_cnt        !line_cnt
               do il=1,ins        !imgpsf_avcnt
		    imgesf_tmp(i,il) = ibuf(il)
               enddo
            enddo


	return
	end
C****************************************************************************
        subroutine find_lsf (ibuf,tranbuf,insm1,idiag,noise,lino,nptsin,
     1 ntot,ipower,ins,lsfmin,lsfmax,r)
c
c  find the line spread function within the image
c
	implicit none
	integer*2 ibuf(4096)
	integer*4 tranbuf(12)
	integer*4 insm1,insm2,idiag,noise,lino,nptsin,ntot,ipower
       integer*4 leftl,i,i1,i2,lsfmax,lsfmin,ibad,irt,ileft,ins
	real*4 r(1026)
c
C***************************
C     FIND LINE SPREAD FUNCTION
C***************************
         LEFTL=0
         IF (IBUF(1) .GT. IBUF(INS)) LEFTL=1  !FIND DIRECTION OF GRADIENT
         IF (LEFTL .EQ. 1) GOTO 13
C****************************
C     DIFFERENTIATE TO GET LSF
C****************************
         IF (IDIAG .EQ. 1)
     &      CALL PRNT(4,1,INSM1,'NUMBER OF ELEMENTS IN LSF= ')
         DO  I = 1,INSM1
             IBUF(I)=IBUF(I+1)-IBUF(I)
         ENDDO
         GOTO 15
13      CONTINUE
         DO  I=1,INSM1
            IBUF(I)=IBUF(I)-IBUF(I+1)
         ENDDO
15       CONTINUE
         IF (IDIAG .EQ. 1) THEN
            DO  I=1,INSM1
               CALL PRNT(4,1,I,'I=. ')
               CALL PRNT(2,1,IBUF(I),'DIFF IBUF(I)=. ')
            ENDDO
         ENDIF
         LSFMAX=IBUF(1)
         LSFMIN=IBUF(1)
C*************************
C     LOCATE MAX IN LSF
C*************************
         DO 16 I=1,INSM1
            IF (IBUF(I) .LT.LSFMIN) LSFMIN=IBUF(I)
            IF (IBUF(I) .LT. LSFMAX) GOTO 16
            LSFMAX=IBUF(I)
            I1=I
16       CONTINUE
         INSM2=INSM1-1
         IF (IDIAG .EQ. 1) THEN
            CALL PRNT (4,1,I1,'INDEX FOR MAX I1=. ')
            CALL PRNT (4,1,LSFMAX,'LSFMAX =. ')
            CALL PRNT (4,1,LSFMIN,'LSFMIN =. ')
            CALL PRNT (4,1,INSM2,'INSM2 IS NUM IN LSF-1=. ')
         ENDIF
         IBAD=0
C************************
C     FIND EDGES OF LSF
C************************
         DO 17 I=I1,INSM2
            IRT=I
            IF (IBUF(I) .LT. 1) IBUF(I)=0   !TRUNCATES NEGATIVES
            IF (IBUF(I) .LT. 1) GOTO 18
            IF ((IBUF(I) .LT. IBUF(I+1)) .AND.
     &          (IBUF(I) .LT. NOISE)) GOTO 18
            IF (IBUF(I) .LT. IBUF(I+1)) IBAD=IBAD+1   !COUNTS BAD PIXELS
17       CONTINUE
         IF (IBAD .NE. 0) then
            CALL PRNT(4,1,IBAD,'NUMBER OF BAD PIXELS ON RIGHT= ')
            CALL PRNT(4,1,LINO,'LINE NUMBER=. ')
         ENDIF
18       CONTINUE
         IF (IDIAG .EQ. 1) THEN
            CALL XVMESSAGE ('FIND EDGE OF LSF',' ')
            DO  I=1,INSM1
               CALL PRNT (4,1,I,'I=. ')
               CALL PRNT (2,1,IBUF(I),'IBUF(I)=. ')
            ENDDO
         ENDIF
         DO 19 I=2,I1
            I2=I1-I+2
            ILEFT=I2
            IF (IDIAG .EQ. 1)
     &         CALL PRNT(4,1,I2,'INDEX FOR LEFT EDGE LSF I2=. ')
            IF (IBUF(I2) .LT. 1) IBUF(I2)=0
            IF (IBUF(I2) .LT. 1) GOTO 20
            IF ((IBUF(I2) .LT. IBUF(I2-1)) .AND.
     &          (IBUF(I2) .LT. NOISE)) GOTO 20
            IF (IBUF(I2) .LT. IBUF(I2-1)) IBAD=IBAD+1   !COUNTS BAD PIXELS
19       CONTINUE
         IF (IBAD .NE. 0) THEN
            CALL PRNT(4,1,IBAD,'NUMBER OF BAD PIXELS ON LEFT=. ')
            CALL PRNT(4,1,LINO,'LINE NUMBER=. ')
         ENDIF
20       CONTINUE
         IF(IDIAG .EQ. 1) THEN
            DO  I=1,INSM1
               CALL PRNT(4,1,I,'INDEX FOR LSF EDGE I=. ')
               CALL PRNT(2,1,IBUF(I),'IBUF(I)=. ')
            ENDDO
         ENDIF
         IF((I1-ILEFT) .GT. 30) ILEFT=I1-30   !SETS MAXIMUM WIDTH OF LSF
         IF((IRT-I1) .GT. 30) IRT=I1+30       !TO MAX +- 30 ELEMENTS
         I1=0
         IF (IDIAG .EQ. 1) THEN
            CALL PRNT(4,1,ILEFT,'ILEFT=. ')
            CALL PRNT(4,1,IRT,'IRT=. ')
         ENDIF
         DO   I=ILEFT,IRT
            I1=I1+1
            CALL XVTRANS(TRANBUF,IBUF(I),R(I1),1)
            IF (IDIAG .EQ. 1) CALL PRNT(7,1,R(I1),'LOAD R(I1)=. ')
         ENDDO
         NPTSIN=I1
         IF(IDIAG .EQ. 1) CALL PRNT(4,1,NPTSIN,'NPTSIN=. ')
         DO 22 I=3,12
            I1=I
            IF (NPTSIN .LE. 2**I) GOTO 23
22       CONTINUE
23       NTOT=2**I1
         IPOWER=I1


	return
	end
C****************************************************************************
       subroutine optim_lsf (ntot,nptsin,n2,key,key4,lino,line_cnt,idiag,itst,
     1 ipower,number_of_lines_without_edge,ntotol,time1,timint,r,
     2 midpt,imgpsf_tmp,imgpsf_cnt,imgpsf_max,imgpsf_midpt,realv,aimagv)
c
c	optimize the line spread function to 256 points 
c	REALV and AIMAGV are the real and imaginary parts of the FFT
C	REALV is the optimized MTF of N2 values
C
	implicit none
	integer*4 ntot,nptsin,n2,key,key4,lino,idiag,ipower
	integer*4 llt,midpt,itst,line_cnt
	integer*4 imgpsf_max(1026),imgpsf_midpt(1026)
	real*4 timint,time1,centr,pi,ftnorm,val
	real*4 r(1026),F(1026),WTS(512),REALV(129),AIMAGV(129)
c
	integer*4 i,j,k,nzero,n1,n3,n4,ntotol,nt2,imgpsf_cnt(1026)
	integer*4 number_of_lines_without_edge,imgpsf_tmp(1026,1026)
c	integer*4 itmp
	real*4  sum,sum1,cen
c	logical*4 izero
	complex*8 a(2052)
c
         DATA PI/3.1415926536E0/

C****************************
C     PROCESS LSF NOW
C****************************
c
         NTOT=2**IPOWER
         NTOTOL=NTOT
         NZERO=NPTSIN+1
         N1=NTOT/2
         N2=N1+1
         N3=N1+2
         N4=NTOT+2
         IF (TIMINT .LT. 1.0E-20) TIMINT=1.0
         TIME1=TIMINT*NTOT
c	if (itst.eq.0) then
c         print *,'************************* int(r)'
c         do i=1,ntot
c            print *,i,int(r(i))
c        enddo
c	endif

         IF(NPTSIN .EQ. NTOT) GOTO 31    !NO FILL INS NEEDED
         IF(KEY .EQ. 1) GOTO 26          !FILL IN BY REFLECTING LSF
         IF(KEY .EQ. 2) GOTO 28          !FILL IN BY REPLECATING MEAN LSF
         DO  I=NZERO,NTOT             !SUPPLY BLANKS IF NO FILL INS
             R(I)=0.0E0
         ENDDO
         GOTO 31


26       J=0                            !REFLECTION OF LSF
         DO  I=NZERO,NTOT
            J=J+2
            R(I)=R(I-J)
         ENDDO
         GOTO 31


28       SUM=0.0E0                      !MEAN LSF
         DO  I=1,NPTSIN
            SUM=SUM+R(I)
         ENDDO
         SUM=SUM/NPTSIN
         DO  I=NZERO,NTOT
            R(I)=SUM
         ENDDO
31       CONTINUE
c
c       store image psf in safe place before resampling or normalizing
c      appears to be correc place
c	define its characteristics
c	IGNORE if sinctst	
	if (itst .eq. 0) then

		call minim_lsf (ntot,line_cnt,r,imgpsf_cnt,imgpsf_tmp,
     1 imgpsf_max,imgpsf_midpt)
	endif
c	print *,'KEY4 = ',key4
         IF (KEY4 .EQ. 1) THEN            !NORMALIZE MTF - 0.0 to 1.0
            SUM=0.0E0
            DO  I=1,NTOT
               SUM=SUM+R(I)
            ENDDO
            IF (SUM .EQ. 0) THEN
               CALL XVMESSAGE ('No Edge Found',' ')
               CALL PRNT(4,1,LINO,'LINE  NUMBER=. ')
            NUMBER_OF_LINES_WITHOUT_EDGE = NUMBER_OF_LINES_WITHOUT_EDGE+1
               GOTO 481
            ENDIF
            IF (IDIAG .EQ. 1) THEN
               CALL XVMESSAGE ('NORMALIZING LSF',' ')
               CALL PRNT(7,1,SUM,'SUM OF LSF=. ')
            ENDIF
            DO  I=1,NTOT                 !DIVIDE BY SUM OF LSF
               R(I)=R(I)/SUM                  !TO NORMALIZE MTF
            ENDDO
            SUM=1.0E0
	 ENDIF
C34       CONTINUE
C****************************
C     COMPUTE FIRST MOMENT FOR WEIGHT CENTER OF LSF BY EQU (2)
C****************************
         SUM=0.0E0
         SUM1=0.0E0
         DO  I=1,NTOT
            SUM=SUM+R(I)
            CEN=FLOAT(I-1)*R(I)
            SUM1=SUM1+CEN
         ENDDO
         IF (SUM .EQ. 0) THEN
            CALL XVMESSAGE ('No Edge Found',' ')
            CALL PRNT (4,1,LINO,'LINE  NUMBER=. ')
            GOTO 481



         ENDIF
         CENTR=SUM1/SUM
c	print *, 'sum, sum1, centr = ',sum, sum1, centr
         IF (IDIAG .EQ. 1) THEN
            CALL PRNT(7,1,SUM1,'SUM1=. ')
            CALL PRNT(7,1,SUM,'SUM=. ')
            CALL PRNT(7,1,CENTR,'CENTR=. ')
         ENDIF
c
C****************************
C     SAMPLING THEOREM TO PRODUCE 256 WIDE LSF FOR FFTT
C     LSF AFTER RESAMPLING IS SPLIT WITH MAXIMUM AT R(1),
C     MINIMUM=0 AT R(128) AND MAXIMUM AT R(256).
C****************************
         IF (IDIAG .EQ. 1)
     &      CALL PRNT(7,NTOT,R,'AT SAMPLING THEORM R(I)=. ')
c
         NT2=NTOT/2
         I=CENTR+1.0E-5                 !WANT FRACTIONAL PART OF CENTR
         CEN=128.0E0+NT2+CENTR-I        !WANT FRACTIONAL PART OF CENTR
         IF (IDIAG .EQ. 1)
     &      CALL PRNT(7,1,CEN,'FRACTIONAL OFFSET CEN=. ')
         SUM1=0.0E0
         DO 39 I=1,256+NTOT              !REDEFINE PER NATHAN
            VAL=(CEN-I)*PI                  !"EQU 3" 
            IF (ABS(CEN-I) .LT. 1.0E-10) GOTO 38
            WTS(I)=SIN(VAL)/VAL             !"EQUATION 3"
            GOTO 39
38          WTS(I)=1.0E0
            SUM1=SUM1+WTS(I)                !SUM OF WEIGHTS
39      CONTINUE
         IF (IDIAG .EQ. 1) THEN
            CALL PRNT(7,1,SUM1,'SUM1 OF WTS=. ')
            CALL PRNT(7,256+NTOT,WTS,'WTS(I)=. ')
         ENDIF
         J=256+1                         !REDEFINE PER NATHAN
         DO  I=1,J                     !DOES THE RESAMPLING
            SUM1=0.0E0
            DO  K=1,NTOT
               SUM1=SUM1+WTS(I+K-1)*R(K)       !"INTX" EQU 3.
            ENDDO
            F(I)=SUM1
         ENDDO
         IF (IDIAG .EQ. 1) CALL PRNT(7,256,F,'INT F(I)=. ')
         I=1+(CENTR+1.0E-5)              !FRACTIONAL PART OF CENTR
         MIDPT=128+NT2-I+1
         I=0
         IF (IDIAG .EQ. 1) CALL PRNT(4,1,MIDPT,'MIDPT=. ')
         DO  K=MIDPT,256               !CENTERS LSF, SPLITS
            I=I+1                           !AND FOLDS WITH MAX AT END
            R(I)=F(K)
         ENDDO                      !POINTS
         IF (IDIAG .EQ. 1) CALL PRNT(7,256,R,'R(I)=. ')
         I=257
         LLT=MIDPT-1
         IF (IDIAG .EQ. 1) CALL PRNT(4,1,LLT,'LLT=. ')
         DO  K=1,LLT                   !LEFT JUSTIFIES LSF
            J=LLT-K+1
            I=I-1
            R(I)=F(J)
         ENDDO
         NTOT=256
         IPOWER=8
c
c       256 points for resampled LSF
c
         IF (IDIAG .EQ. 1) THEN
            CALL XVMESSAGE ('RESAMPLED DATA FOR TRANSFORM',' ')
            CALL PRNT(7,256,R,'R=. ')
         ENDIF
         NTOTOL=NTOT
         N1=NTOT/2
         N2=N1+1
         N3=N1+2
         N4=NTOT+2
         TIME1=TIMINT*NTOT    !INITIAL PHASE SHIFT 

C********************
C     DIRECT FORWARD FFT 
C********************
         DO  I=1,NTOT
            A(I)=CMPLX(R(I),0.0E0)
         ENDDO
         CALL FFTT(IPOWER,-1,A)       !REALLY ONLY THREE ARGUMENTS!!!!

C*********************
C     PROCESS FFTT OUTPUT FOR EACH LINE NORMALIZE ONLY ONCE AFTER F.T. 
C     TO AVOID ROUND OFF AND TRUNC. CONSULTATION WITH R. NATHAN
C     REMOVE FFTT FACTOR OF "2" AND NORMALIZE FT COMPONENTS BY "/REAL(A(1))"
C     ASSUMES AIMAG(A(1))=0.0 ALWAYS!!
C*********************
         FTNORM=REAL(A(1))
         DO  I=1,N2                          !SUM UP FT ELEMENTS
            REALV(I)=REALV(I)+(REAL(A(I))/FTNORM) !FOR EACH LINE
            AIMAGV(I)=AIMAGV(I)+(AIMAG(A(I))/FTNORM)
c	print *,'i,realv(i), aimagv(i) = ',i,realv(i), aimagv(i)
         ENDDO
cc

481      CONTINUE


	return
	end
C*************************************************************************
        subroutine minim_lsf (ntot,line_cnt,r,imgpsf_cnt,imgpsf_tmp,
     1 imgpsf_max,imgpsf_midpt)
c
c	put lsf into smallest one line
c
	implicit none
	integer*4 i,j,ntot,line_cnt,itmp,imgpsf_cnt(1026),imgpsf_max(1026)
	integer*4 psf_start,psf_stop,imgpsf_midpt(1026)
	integer*4 imgpsf_tmp(1026,1026)
	real*4 r(1026)

	psf_start = 1
	psf_stop = ntot
c        print *,'store....line_cnt, ntot = ',line_cnt, ntot
c        do i=1,ntot
c          print *, 'i,int(R(i) = ',i,int(R(i))
c        enddo
        itmp = 0
        imgpsf_cnt(line_cnt) = 1

	do i=1,ntot
c	print *,'i,int(r(i)) = ',i,int(r(i))
	   if (int(r(i)) .gt. 0) then
		psf_start = i
c	print *,'start' 
		go to 10
	   endif
	enddo
	call xvmessage('??E - All 0 values in LSF',' ')
	call abend
10	continue
	  j = 0
	  if (psf_start > 1 ) then
		imgpsf_tmp(line_cnt,1) = 0  
		j = j + 1
	  endif
          do i=psf_start,ntot
c             print *, 'i,int(R(i) = ',i,int(R(i))
             if (int(R(i)) .gt. 0) then
		j = j + 1
c                print *, '...i,int(R(i) = ',i,int(R(i))
                imgpsf_tmp(line_cnt,j) = int(R(i))
c		print *, 'imgpsf_tmp(line_cnt,j) = ',j,imgpsf_tmp(line_cnt,j)
                imgpsf_cnt(line_cnt) = imgpsf_cnt(line_cnt) + 1
             else
c                print *,'here j = ',j
		if (psf_start > 1 ) then
		    psf_stop = j + 1
		    imgpsf_cnt(line_cnt) = j + 1
		    imgpsf_tmp(line_cnt,psf_stop) = 0
		    go to 20
		else
		    psf_stop = j
		    imgpsf_cnt(line_cnt) = j 
                    go to 20
cc		else
cc		   psf_stop = j
cc		   go to 20
	        endif
c                endif
             endif
          enddo
20	continue
c        do i=1,imgpsf_cnt(line_cnt)
c                print *, '---> line_cnt,i,imgpsf_tmp(i) = ',line_cnt,i,imgpsf_tmp(line_cnt,i)
c        enddo
        imgpsf_max(line_cnt) = -100 !
        imgpsf_midpt(line_cnt) = 0
           do i=1,imgpsf_cnt(line_cnt)
c            print *, 'i,imgpsf_tmp(line_cnt,i) = ',i,imgpsf_tmp(line_cnt,i)
             if (imgpsf_tmp(line_cnt,i).gt.imgpsf_max(line_cnt)) then
c         print *, 'i,imgpsf_tmp(line_cnt,i),psfmax = ',i,imgpsf_tmp(line_cnt,i),psfmax(line_cnt)
                   imgpsf_max(line_cnt) = imgpsf_tmp(line_cnt,i)
                    imgpsf_midpt(line_cnt) = i - 1
             endif
           enddo
           imgpsf_midpt(line_cnt) = imgpsf_cnt(line_cnt)/2 + 1  
c        print *,'psfmax,psfmidpt,imgpsf_cnt = ',psfmax(line_cnt),psfmidpt(line_cnt),
c     1 imgpsf_cnt(line_cnt)


	return
	end
C*************************************************************************
        subroutine out_prep(noprt,n2,realv,aimagv,rinl,time1,timint,
     1 k,itable,zoomflag,pzoom,ntotol,amplsum,ampl9,ampl10,f29,cplot,
     2 pplot,obuf,aliased)
c
c	Prepare output OTF
c
	implicit none
	integer*4 noprt,n2,i,k,itable,zoomflag,ntotol
	real*4 ampl,ampl2,f2,f3,pi,phase,amplsum,wsft,timint
	real*4 pzoom
	real*4 realv(129),aimagv(129),rinl,time1,c1,c2
	real*4 AMPL9(131),AMPL10(131),CPLOT(131),PPLOT(131)
	REAL*4 OBUF(3,400),WSFT9(129),F29(129)
	logical*4 xvptst,aliased
	CHARACTER*1   TAB
	CHARACTER*140 ZPBUF
c
         DATA PI/3.1415926536E0/

c
	TAB=CHAR(9)
            IF (NOPRT .EQ. 0) THEN
                ZPBUF=' '
                CALL XVMESSAGE(' ',' ')
                ZPBUF(1:59)= ' PIXEL  CYCLES/      FREQUENCY    REAL(FT)     IMAG.(FT)'
                ZPBUF(60:108)= '  INTENSITY    AMPLITUDE     PHASE     WAVE LENGTH'
                CALL XVMESSAGE(ZPBUF,' ')
                ZPBUF=' '
                ZPBUF(10:15)='PIXEL '
                ZPBUF(88:94)='RADIANS'
                ZPBUF(101:105)='SHIFT'
                CALL XVMESSAGE(ZPBUF,' ')
                ZPBUF=' '
            ENDIF

c	print *,'rinl = ',rinl
            K=1
C
         DO 54 I=1,N2                   !LOOP TO PRINTOUT OTF VS FREQUENCY TABLE
            C1=REALV(I)/RINL               !AVERAGE THE SUMED F.T. COMPONENTS
            C2=AIMAGV(I)/RINL
            IF (ABS(C1) .LT. 1.0E-8) C1=0.0E0 !SUPRESS ERROR VALUES
            IF (ABS(C2) .LT. 1.0E-8) C2=0.0E0
            F2=(I-1)/TIME1
            AMPL2=C1*C1+C2*C2
            AMPL=SQRT(AMPL2)

            AMPL9(I)=AMPL             !LOAD FOR PLOTTING
            AMPL10(I)=AMPL

C            AMPL9(I)=-AMPL             !LOAD FOR PLOTTING
C            AMPL10(I)=-AMPL
            IF (AMPL .LT. 0.0005E0) GOTO 60 !SUPRESS PHASE ERRORS

C<<<<<< **** NOTE TO DEVELOPER ****
C       The following change was made as a work-around for the subroutine
C       FFTT.  The problem with FFTT is its inconsistency between platforms on 
C       calculation of small floating values for both real and imaginary parts 
C       of the complex number.  The problem may be caused by hardware round-off.
C       In the case of this program, it affected the imaginary part of the 
C       complex number, which has direct impact on the calculation of PHASE. 
C       REF:  FR90520
C       -- Thomas Huang (txh)
C>>>>>> ***************************
            IF (ABS(C2) .LT. (1E-3 * ABS(C1))) THEN
               IF (C1 .GT. 0.0) THEN
                  PHASE = 0.0
               ELSE
                  PHASE = PI
               ENDIF
            ELSE
               PHASE=ATAN2(C2,C1)
            ENDIF

C<<<<<<< **** ORIGINAL CODE ****
C           PHASE=ATAN2(C2,C1)
C>>>>>>> ***********************

            GOTO 61
60          PHASE=0.0E0
61          CONTINUE
            F3=(I-1)/FLOAT(NTOTOL)         !DEFINES FREQUENCY

C            CPLOT(I)=F3                   !LOAD FOR PLOTTING

C<<<<<< Changed to use the INTERVAL input value.  TXH
            CPLOT(I)=F3*TIMINT         !LOAD FOR PLOTTING

            IF (ZOOMFLAG .EQ. 1) THEN

               PPLOT(I)=PHASE*PZOOM     !LOAD FOR PLOTTING
C               PPLOT(I)=-PHASE*PZOOM     !LOAD FOR PLOTTING

            ELSE

               PPLOT(I)=PHASE
C               PPLOT(I)=-PHASE

            ENDIF
            IF((F3 .LT. 0.2500) .OR. (F3 .GT. 0.484)) GOTO 51
            AMPLSUM=AMPLSUM+AMPL           !INTEGERATES AMPL
51          CONTINUE
            IF(ABS(F2) .LT. 1.0E-20) GOTO 52
            WSFT=PHASE/(2.0E0*PI*F2)
            GOTO 53
52          WSFT=0.0E0
53          CONTINUE
            F29(I)=F2
            WSFT9(I)=WSFT
            IF (NOPRT .EQ. 0) THEN
	       write (ZPBUF(1:3),'(I3)') I			!PIXEL
               WRITE (ZPBUF(6:16),'(F10.5)') F3			!FREQUENCY - CYCLES/PIXEL
               WRITE (ZPBUF(20:29),'(F10.5)') F2		!FREQUENCY
               WRITE (ZPBUF(33:42),'(F10.5)') C1		!REAL	   - MTF
               WRITE (ZPBUF(46:55),'(F10.5)') C2		!IMAGINARY - PTF
               WRITE (ZPBUF(59:68),'(F10.5)') AMPL2		!INTENSITY
               WRITE (ZPBUF(72:81),'(F10.5)') AMPL		!AMPLITUDE
               WRITE (ZPBUF(85:94),'(F10.5)') PHASE		!PHASE,RADIANS
               WRITE (ZPBUF(98:107),'(F10.5)') WSFT		!WAVELENGTH SHIFT
               CALL XVMESSAGE(ZPBUF,' ')
            ENDIF

            IF ((ITABLE .EQ. 1) .AND. XVPTST('PHASE'))
     &         WRITE(11,903) F3,TAB,AMPL,TAB,PHASE
            IF (ITABLE .EQ. 1 .AND. XVPTST('NOPHASE'))
     &         WRITE(11,902) F3,TAB,AMPL

            OBUF(K,1)=F3			!samples, lines
            OBUF(K,2)=C1
            OBUF(K,3)=C2
            K=K+1					!K=K+3
54       CONTINUE
C
C       !END LOOP TO PRINTOUT OTF VS FREQUENCY TABLE 
C
903      FORMAT(1x,F10.5,A1,F10.5,A1,F10.5)
902      FORMAT(1x,F10.5,A1,F10.5)
         IF (ITABLE .EQ. 1) CLOSE(11)

	K= k -1
         CALL PRNT(4,1,K,'NUMBER POINTS OUTPUT = . ')
c
c	check to see if aliased, i.e, c1 > 0 at freq = 0.5 cycles/sample
c
	if (obuf(n2,2) .gt. 0.0) then
	    aliased = .true.
	    call xvmessage ('**** This image is ALIASED ****',' ')
	endif

c         --------------------------------------------
	return
	end
C*******************************************************************
c.... Function to convert radians to degrees
      real*8 function radtodeg(x)
c
      implicit none
      real*8 x,pi
c
c.... Define pi as a parameter
      parameter(pi = 3.14159265d0)
c
      radtodeg = x *180.0/pi
c
      return
      end
C******************************************************************
      subroutine putparm (pixrad,pixdeg,psfrad,psfdeg,psfpix,
     1 key4,lsfmin,lsfmax)
c
c   output parameters to tcl variables
c            
      implicit none
      INCLUDE 'pgminc'            ! FOR XQINI... TAE CONSTANTS & PARAMETERS
      integer*4 vblock(xprdim),stat,psfpix,key4
      integer*4 lsfmin,lsfmax
      real*8 pixrad,pixdeg,psfrad,psfdeg
      character*8 FORM
      character*8 norm/'NORMAL'/,nonorm/'NONORMAL'/
c           
	 
	form=nonorm
	if (key4 .eq. 1) form=norm	
      call xqini (vblock,xprdim,xabort)
      call xqreal (vblock,'PIXRAD',1,sngl(pixrad),xadd,stat)
      call xqreal (vblock,'PIXDEG',1,sngl(pixdeg),xadd,stat)
      call xqreal (vblock,'PSFRAD',1,sngl(psfrad),xadd,stat)
      call xqreal (vblock,'PSFDEG',1,sngl(psfdeg),xadd,stat)
      call xqintg (vblock,'PSFPIX',1,psfpix,xadd,stat)
	call xqstr (vblock,'LSFFORM',1,form,xadd,stat)
	call xqintg (vblock,'LSFMAX',1,lsfmax,xadd,stat)
	call xqintg (vblock,'LSFMIN',1,lsfmin,xadd,stat)

      call xvqout (vblock,stat)
      call chkstat (stat,'??E - XVQout error',0,0,0)

	return
	end

C==================================================================================
      SUBROUTINE OTFLABS(INU,HEADR,ICNT,nheadr,*)
         IMPLICIT INTEGER*4 (A-Z)
         INTEGER*4 LV2(60),LPL(60),ICNT,nheadr(70)
         CHARACTER*72 V1(60),V2(60),PL(60),HT(60)
         CHARACTER*86 HEADR(70)
c
c	extract label records and put in plot header data structure
c 
         CALL LABPROC(INU,V1,NV1,V2,LV2,NV2,PL,LPL,NPP,HT,NH)
 
         IF (NV1 .GT. 0) THEN
            DO I=1,NV1
               HEADR(ICNT)=V1(I)
c		print *,'nv1 ',HEADR(ICNT)
		 nheadr(icnt) = index(HEADR(ICNT),'          ') - 1
               ICNT=ICNT+1
            ENDDO
         ELSE IF (NPP .GT. 0) THEN
            DO I=1,NPP
               HEADR(ICNT)=PL(I)
c		print *,'npp ',HEADR(ICNT)
		 nheadr(icnt) = index(HEADR(ICNT),'          ') - 1
               ICNT=ICNT+1
            ENDDO
         ELSE IF (NV2 .GT. 0) THEN
            DO I=1,NV2
               HEADR(ICNT)=V2(I)
c		print *,'nv2 ',HEADR(ICNT)
		 nheadr(icnt) = index(HEADR(ICNT),'          ') - 1
               ICNT=ICNT+1
            ENDDO
         ELSE
            CALL XVMESSAGE('??E - Label processing error',' ')
            RETURN 1
         ENDIF

         IF (NH .GT. 0) THEN
            DO I=1,NH
               HEADR(ICNT)=HT(I)
c		print *,'nh ',HEADR(ICNT)
		 nheadr(icnt) = index(HEADR(ICNT),'          ') - 1
               ICNT=ICNT+1
            ENDDO
         ENDIF
	if (icnt.gt.70) then
	   call xvmessage ('??E - Number of labels is greater than 70',' ' )
	   call abend
	endif 
         RETURN
      END

C=============================================================================
      SUBROUTINE KVPAIR(IU,PAIR,LNG,NUM)
C-------EXTRACTS KEYWORD=VALUE STRINGS FROM FIRST PROPERTY LABEL
         IMPLICIT INTEGER*4 (A-Z)
         INTEGER*4 LNG(60)
         character*72 pair(60)
         character*2048 buf

         max=2048
         call xlgetlabel(iu,buf,max,ist)
         i=0
         b = index(buf(1:),'PROPERTY')
         IF (B .EQ. 0) GOTO 100

50       L=INDEX(BUF(B+1:),' ')
         L=B+L
         Q=INDEX(BUF(B+1:),'''')
         Q=B+Q
         IF (Q .LT. L) THEN		!ARE THERE QUOTES IN THE VALUE?
            Q2=INDEX(BUF(Q+1:),'''')
            Q2=Q+Q2
            L=INDEX(BUF(Q2+1:),' ')
            L=Q2+L
         ENDIF
         E=L-1
C-------STOP WHEN REACH A 'TASK' OR NEXT PROPERTY LABEL
         IF (BUF(B:B+3) .EQ. 'TASK') GOTO 100
         IF ((I .GT. 1) .AND. 
     &       (BUF(B:B+3) .EQ. 'PROP')) GOTO 100
         I=I+1
         KK=E-B+1
         LNG(I) = KK			!STORE LENGTH
         PAIR(I) = BUF(B:E)        !STORE STRING
         IF (E .EQ. MAX) GOTO 100        !END OF LABEL?
         DO J=E+1,MAX			!LOOK FOR NEXT STRING
            B=J
            IF(BUF(J:J) .NE. ' ') GOTO 50
         END DO
100      NUM=I
         RETURN
      END
c ************************************************************
      subroutine V2PAIR(IU,PAIR,LNG,NUM)
C-------RETURN THE KEYWORD=VALUE PAIRS FROM THE FIRST VICAR2 TASK
         IMPLICIT INTEGER*4 (A-Z)
         INTEGER*4 LNG(60)
         character*72 pair(60)
         character*2048 buf

         max=2048
         call xlgetlabel(iu,buf,max,ist)
         i=0
         IF (index(buf(1:),'PROPERTY') .GT. 0) GOTO 100
         B = index(buf(1:),'TASK')
         IF (B .EQ. 0) goto 100

50       L=INDEX(BUF(B+1:),' ')
         L=B+L
         Q=INDEX(BUF(B+1:),'''')
         Q=B+Q
         IF (Q .LT. L) THEN
            Q2=INDEX(BUF(Q+1:),'''')
            Q2=Q+Q2
            L=INDEX(BUF(Q2+1:),' ')
            L=Q2+L
         ENDIF
         E=L-1
C-------STOP WHEN ENCOUNTER THE SECOND TASK
         IF ((I .GT. 1) .AND. 
     &       (BUF(B:B+3).EQ. 'TASK')) GOTO 100
         I=I+1
         KK=E-B+1
         LNG(I) = KK			!STORE LENGTH
         PAIR(I) = BUF(B:E)	!STORE STRING
         IF (E .EQ. MAX) GOTO 100	!END OF LABEL?
         DO J=E+1,MAX			!LOOK FOR NEXT STRING
            B=J
            IF(BUF(J:J) .NE. ' ') GOTO 50
         END DO
100      NUM=I
         RETURN
      END

C======================================================================
      SUBROUTINE LABPROC(IUNI,LABV1,NV1,VPAIR,LV2,NV2,PPAIR,LPL,
     &                   NPP,HSTRY,NH)
c
c	extract records from vicar label
c
         IMPLICIT INTEGER(A-Z)
         INTEGER INSTANCES(20),LV2(60),LPL(60)
         CHARACTER*8 TASKS(20)
         CHARACTER*12 UNAME
         CHARACTER*28 TIME
         CHARACTER*72 HBUF,BL
         CHARACTER*72 LABV1(60),HSTRY(60),VPAIR(60),PPAIR(60)
         CHARACTER*4320 LABS

c-------get all vicar1 labels
         NV1=40
         CALL VIC1LAB(IUNI,STAT,NV1,LABS,0)
         DO J=1,NV1
            LABV1(J)=LABS(72*(J-1)+1:72*(J-1)+72)
         END DO

c-------get keyword=value pairs from vicar property labels
         NPP=0
         IF (NV1 .EQ. 0) call kvpair(IUNI,ppair,LPL,npp)

c-------get keyword=value pairs from first vicar2 task
         NV2=0
         IF ((NV1 .EQ. 0) .AND. (NPP .EQ. 0)) 
     &      call v2pair(IUNI,vpair,LV2,nv2)

c-------get user and date for each vicar2 history task
         BL = '----TASK:-------------USER:------------------------'

         nh=20                             !EXTRACT VIC*2 LAB
         CALL XLHINFO(IUNI,TASKS,INSTANCES,NH,STAT,' ')

         DO 801 J=1,NH
            CALL XLGET(IUNI,'HISTORY','USER',UNAME,STAT,'HIST',
     &                 TASKS(J),'INSTANCE',INSTANCES(J),'FORMAT',
     &                 'STRING',' ')
            CALL XLGET(IUNI,'HISTORY','DAT_TIM',TIME,STAT,'HIST',
     &                 TASKS(J),'INSTANCE',INSTANCES(J),'FORMAT',
     &                 'STRING',' ')

            HBUF(1:72) = BL(1:72)
            HBUF(10:22) = TASKS(J)
            HBUF(28:39) = UNAME
            HBUF(40:68) = TIME
801         HSTRY(J) = HBUF

         RETURN
      END
C*********************************************************************
       subroutine term_plot (n2,lo,f29,realv,aimagv,ampl10,wsft9)
c
c 	ROUTINE TO PRINT PLOT TO TERMINAL SCREEN
c
	implicit none
	integer*4 i,j,n2,inlo
	real*4 F29(129),REALV(129),AIMAGV(129),AMPL10(131),WSFT9(129)
	character*101 log
	CHARACTER*121 LO
	CHARACTER*140 ZPBUF
	character*1 blnk/' '/,one/'1'/
	character*1 logr/'R'/,logi/'I'/,loga/'A'/,logw/'W'/
	character*9 BL/'        '/
c
         ZPBUF=' '
         ZPBUF(2:10)='FREQUENCY'
         ZPBUF(20:21)='-1'
         ZPBUF(70:70)='0'
         ZPBUF(119:120)='+1'
         CALL XVMESSAGE(ZPBUF,' ')              !Horiz Axis
         CALL XVMESSAGE(LO,' ')
         DO  I=1,N2
            DO  J=1,101
               LOG(J:J)=BLNK
	    ENDDO
            LOG(51:51)=ONE
            INLO=(REALV(I)+1.)*51.
            IF (INLO .LT. 1) INLO=1             !lwk
            IF (INLO .GT. 101) INLO=101
            LOG(INLO:INLO)=LOGR
            INLO=(AIMAGV(I)+1.)*51.
            IF (INLO .LT. 1) INLO=1             !lwk
            IF (INLO .GT. 101) INLO=101
            LOG(INLO:INLO)=LOGI
            INLO=(-AMPL10(I)+1.)*51.
            IF(INLO .LT. 1) INLO=1              !lwk
            IF(INLO .GT. 101) INLO=101
            LOG(INLO:INLO)=LOGA
            INLO=(WSFT9(I)+1.)*51.
            IF (INLO .LT. 1) INLO=1             !lwk
            IF (INLO .GT. 101) INLO=101
            LOG (INLO:INLO)=LOGW
            WRITE (ZPBUF(1:10),'(F10.5)') F29(I)
            ZPBUF(20:120)=LOG(1:101)
            ZPBUF(10:18)=BL
            CALL XVMESSAGE(ZPBUF,' ')
         ENDDO

	return
	end
C*********************************************************************
	subroutine do_plot (unit98,iuni,n2,instat,zoomflag,pzoom,
     1 imess,nimess,plotname,nplotname,ploteps,nploteps,
     2 plotgpi2,nplotgpi2,infile,ninfile,mcycle,nmcycle,mphase,nmphase,
     3 phaseplot,epsplot,iplot,timint,aliased,ampl9,pplot,cplot)
c
c	create gnuplot instructions (.gpi) for plotting
c
	implicit none
	real*4 ampl9(131),pplot(131),cplot(131)
	integer*4 iuni,unit98,instat,zoomflag,n2
	integer*4 ii,jj,jst,iplot
	integer*4 nhdr,nheadr(70),ninfile,nplotname,nimess,nltitle
	integer*4 nxlabel,nylabel1,nylabel2,nmcycle,nmphase,ntbl
	integer*4 isize,plotwid,plotht,nploteps,nplotgpi2
	integer*4 ncolx,ncoly(20),asyml(20)
	real*4 x,y,timint,labstep,tmp,minampl,minphase,pzoom,pi
	real*4 xrang1,xrang2,yrang1,yrang2,y2rang1,y2rang2
	logical*4 phaseplot,epsplot,aliased
	character*20 ycolstr(20),asymtyp(20)
	character*32 ylabel1,ylabel2
	character*40 mcycle,mphase
	character*60 infile,xlabel
	character*63  plotname
	character*80 tbl,ploteps,plotgpi2
	CHARACTER*86  HEADR(70),IMESS,ltitle
c
	character*4 asc/'.asc'/
c	character*4 gpi/'.gpi'/

	DATA PI/3.1415926536E0/
c
c	bring in gunplot colors, lines, points, etc
	include 'gnuplotchar'

C*******************
C     PRINTRONIX PLOTS 
C*******************
cccc         CALL XRTBEGIN(STATUS)
cccc         IF (STATUS .NE. 1) CALL MABEND('Unable to OPEN plotter')
cccc         CALL DISPLAYAXES(1,1,1)   ! x,y1 and y2 axes displayed 1=yes 0=no
cccc         CALL SETACTIVESET(0)      ! 0= no triangles (default), 1= triangles
         X=0.22                         !PRINT HEADER ON PLOT
         Y=3.5

c       print *,'X,Y'
C-------PRINT HEADER ON PLOT
C-------PRINT INPUT FILENAME AND PLOTNAME ON PLOT
C-------GET AND PLOT ALL LABELS
        nhdr=1
         HEADR(1)='OTF1 MODULATION TRANSFER FUNCTION'
         nheadr=index(headr(1),'    ') - 1
c         IF (INSTAT .EQ. 1) THEN
            HEADR(2)='INPUT = '//INFILE(1:ninfile)
            nheadr(2)=index(headr(2),'    ') - 1
            NHDR=3
c         ELSE
c            HEADR(2)='internal test data - SYNC function'
c            nheadr(2)=index(headr(2),'    ') - 1
c            NHDR=3
c         END IF
cc       print *,'nheadr'
         HEADR(NHDR)='PLOT  = '//plotname(1:nplotname)
         nheadr(3)=index(headr(3),'    ') - 1
        ltitle = headr(1)//' '//headr(2)
        nltitle = index(ltitle,'   ') - 1
         NHDR=NHDR+1
c        print *,'=======, instat',instat
c
C-------PRINT INPUT FILENAME ON PLOT
         IF (INSTAT .EQ. 1) THEN
c           print *,'otflabs'
            CALL OTFLABS(IUNI,HEADR,nhdr,NHEADR,*994)
c               print *,'done'
            NHDR=NHDR+1
         ENDIF
c           print *,'nhdr = ',nhdr
c               print *,HEADR(1),nheadr(1)
c               print *,HEADR(2),nheadr(2)
c               print *,HEADR(3),nheadr(3)
c               print *,HEADR(4),nheadr(4)
c               do i=1,nhdr
c               print *,i,HEADR(i)(1:nheadr(i))
c               enddo
         HEADR(NHDR)(1:86)=IMESS(1:nimess)
        nheadr(nhdr) = index(headr(nhdr),'         ') - 1
cccc         CALL HEADER (HEADR,I,0) ! 0=left justify, 1=center justify, 2=right
        ylabel1 = 'AMPLITUDE'
        nylabel1 = index(ylabel1,'   ') - 1
        ylabel2 = 'PHASE (PI = 1)'
        nylabel2 = index(ylabel2,'   ') - 1
         WRITE (MCYCLE(31:37), '(F7.2)') TIMINT
c        print *,'mcycle = ',mcycle
        nmcycle= index(MCYCLE,'      ') - 1
        xlabel = mcycle
        nxlabel = index(xlabel,'     ') - 1
c        print *,'xlabel = ',xlabel(1:nxlabel)
         IF (ZOOMFLAG .EQ. 0) THEN
cccc            CALL AXESTITLES(MCYCLE,'AMPLITUDE',90,
cccc     &                   'PHASE (PI=1)',90)
         ELSE
            WRITE (MPHASE(25:31),'(F7.2)') PZOOM
            nmphase = index(MPHASE,'    ') - 1
cccc            CALL AXESTITLES
cccc     &           (MCYCLE,'AMPLITUDE',90,MPHASE,90)
         ENDIF
cc        print *, 'isize plot'
c        if (iplot.eq.1) then
        isize=10
        plotwid = 648           !72 dpi = 7 inch
        plotht = 648            !72 dpi = 9 inch i

        labstep = 0.02          !for size 9 font --- originally 0.04

        if (nhdr .gt. 16) then
           tmp = nhdr/16
           plotht = int(plotht * tmp)
           labstep = labstep/tmp
        endif

        yrang1 = 1.1
        yrang1 = yrang1 + 2*(nhdr * labstep)
        yrang2 = -1.1
        y2rang1 = yrang1
        y2rang2 = yrang2
        xrang1 = 0.0
        xrang2 = 0.5
c       print *,'infile = ',infile(1:ninfile)
        tbl=plotname(1:nplotname)//asc
        ntbl=index(tbl,'   ') - 1

c       write constant part of gpi    
c       print *,'write_gpi_1'            
            call write_gpi_1(unit98,tbl,ntbl,ylabel1,nylabel1,
     1 ylabel2,nylabel2,xlabel,nxlabel,isize,
     2 plotwid,plotht,iplot,phaseplot,ploteps,nploteps)


         AMPL9(130)=0.00000
         AMPL9(131)=1.00000
         CPLOT(130)=0.00000
         CPLOT(131)=1.00000
         PPLOT(130)=0.00000
         PPLOT(131)=1.00000
c
c       create ascii data file for gnuplot
       OPEN(90,FILE=tbl(1:ntbl),STATUS='UNKNOWN',IOSTAT=JST,ERR=999)

        minampl = 1.0e10
        minphase = 1.0e10
c        call xvmessage ('     CPLOT           AMPL9            PHASE',' ')
        do ii=1,n2
C     plot the "PHASE" curve
            pplot(ii)=pplot(ii)/pi    !scale phase to lie between -1 and 1 
            if (ampl9(ii).lt.minampl) minampl = ampl9(ii)
            if (pplot(ii).lt.minphase) minphase = pplot(ii)
            write (90,907) cplot(ii),ampl9(ii),pplot(ii)
907      FORMAT(1x,F10.5,2X,F10.5,2X,F10.5)
c             print *,cplot(ii),ampl9(ii),pplot(ii)
        enddo
c        print *,'TOTAL = ',n2
        close (90)
c       write variable part
c       print *,'write_gpi_ap'

        if (minampl.ge.0.0 .and. minphase.ge.0.0) then
                yrang2 = -0.1
                y2rang2 = yrang2
        else
                if (minphase .lt. minampl) then
                    minampl = minphase
                endif
                    yrang2 = -1.1
                    y2rang2 = -1.1
        endif
           call write_gpi_ap(unit98,ltitle,nltitle,xrang1,xrang2,
     1 yrang1,yrang2,y2rang1,y2rang2,pzoom,zoomflag,phaseplot,
     2 isize,infile,ninfile)

cccc         CALL LINE(CPLOT,AMPL9,N2,1,0,0)
cccc         CALL LINE(CPLOT,PPLOT,N2,1,0,0)
cccc         CALL PLOT(0,0,999)

c       write labels out to gpi
c
c       print *,'write_gpi_lab'
        call write_gpi_lab(unit98,nhdr,headr,nheadr,isize,labstep,aliased)

        ncolx = 1
        ncoly(1) = 2
        ncoly(2) = 3
        ycolstr(1) = 'AMPLITUDE'
        ystrl(1) = index(ycolstr(1),'  ') - 1
        ycolstr(2) = 'PHASE'
        ystrl(2) = index(ycolstr(2),'  ') - 1
        asymtyp(1) = 'lines'
        asyml(1) = index(asymtyp(1),'   ') - 1
        asymtyp(2) = 'lines'
        asyml(2) = index(asymtyp(2),'   ') - 1
c
c       write out plot data
c
c       print *,'write_gpi_ln'
        call write_gpi_ln(unit98,tbl,ntbl,ncolx,ncoly,phaseplot,
     1 ycolstr,ystrl,asymtyp,asyml,lntype,pttype,ptcolor,ptcolorl)

10255 format("pause mouse any")                 !allows plot to display on screen until mouse click
           write(unit98,fmt=10255,iostat=jj,err=995)


        close (unit98)
c
c       postscript file
c
        if (epsplot) then

            open(97,file=plotgpi2(1:nplotgpi2),status='UNKNOWN',iostat=jj,err=996)

c       write constant part of gpi    
            call write_gpi_1(97,tbl,ntbl,ylabel1,nylabel1,
     1 ylabel2,nylabel2,xlabel,nxlabel,isize,plotwid,plotht,
     2 iplot,phaseplot,ploteps,nploteps)
c
c       write variable part

            call write_gpi_ap(97,ltitle,nltitle,xrang1,xrang2,
     1 yrang1,yrang2,y2rang1,y2rang2,pzoom,zoomflag,phaseplot,
     2 isize,infile,ninfile)
c
c       write labels out to gpi
c
            call write_gpi_lab(97,nhdr,headr,nheadr,isize,labstep,aliased)
c
c
c       write out plot data
c
            call write_gpi_ln(97,tbl,ntbl,ncolx,ncoly,phaseplot,
     1 ycolstr,ystrl,asymtyp,asyml,lntype,pttype,ptcolor,ptcolorl)

            close (97)
        endif

	return
cccc   end do_plot      =============================================== return

995     call xvmessage('??E - do_plot: Error opening/writing gnuplot gpi file',' ')
        call abend
        return

996     call xvmessage('??E - do_plot: Error opening/writing gnuplot eps file',' ')
        call abend
        return

999      CALL XVMESSAGE ('??E - ERROR OPENING TABLE FILE',' ')
         CALL PRNT(4,1,JST,'IOSTAT=.')
994      CALL ABEND
         RETURN

	end
C*********************************************************************
	subroutine write_gpi_1(unit,tbl,ntbl,ylabel1,nylabel1,
     1 ylabel2,nylabel2,xlabel,nxlabel,isize,
     2 plotwid,plotht,iplot,phaseplot,eps,neps)
c            
c       write out the first part of gpi form
c
        implicit none
        integer*4 unit,isize,plotwid,plotht
        integer*4 jj,iplot,psize
        integer*4 ntbl,nylabel1,nylabel2,nxlabel,neps
	logical*4 phaseplot
        character*32 ylabel1,ylabel2,xlabel
        character*80 tbl,eps
c
	psize = isize
	if (unit .eq. 97) psize = 16
c
10100 format('# Created by program otf1')              !#'s are ignored in gnuplot
            write(unit,fmt=10100,iostat=jj,err=995)
10105 format('# Gnuplot commands for optical transfer plot')
            write(unit,fmt=10105,iostat=jj,err=995)
10110 format('# Data in ',a)
            write(unit,fmt=10110,iostat=jj,err=995) tbl(1:ntbl)
        if (unit.eq.97) then
10300 format('set terminal postscript eps enhanced color "Ariel" ',i2,' size 11 ,8')
           write(unit,fmt=10300,iostat=jj,err=995) psize
10305 format("set output '",a,"'")
           write(unit,fmt=10305,iostat=jj,err=995) eps(1:neps)
        else 
10115 format('set term x11 font "ariel,',i2,'" size ',i4,', ',i4)
            write(unit,fmt=10115,iostat=jj,err=995) isize,plotwid,plotht
10116 format('set output')                              !set output to screen
            write(unit,fmt=10116,iostat=jj,err=995)
        endif
             
c10120 format('set grid ')
c            write(unit,fmt=10120,iostat=jj,err=995)
10125 format("set ylab '",a,"'" )
            write(unit,fmt=10125,iostat=jj,err=995) ylabel1(1:nylabel1)

	  if (phaseplot) then
10126 format("set y2lab '",a,"'" ) 
	        write(unit,fmt=10126,iostat=jj,err=995) ylabel2(1:nylabel2)
	  endif

10130 format("set xlab '",a,"'")
            write(unit,fmt=10130,iostat=jj,err=995) xlabel(1:nxlabel)

	  if (phaseplot) then
10135 format("set y2tics")
	    write(unit,fmt=10135,iostat=jj,err=995)
	  endif

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
        subroutine write_gpi_ap(unit,title,ntitle,xrang1,xrang2,
     1 yrang1,yrang2,y2rang1,y2rang2,pzoom,zoomflag,phaseplot,
     2 isize,filename,nfilename)
c
c       write out the repeatable descriptive part of gpi form
c       for first, or only, plot
c
        implicit none
        integer*4 unit,zoomflag
        integer*4 jj ,isize,psize
        integer*4 ntitle,nfilename
        real*4 xrang1,xrang2,yrang1,yrang2,y2rang1,y2rang2,pzoom
	logical*4 phaseplot
        character*86 title
        character*40  filename
c
c       the data set
c
c        print *, 'iplot = ',iplot,' first point = ',firstpt,' last point = ',lastpt

	psize = isize
	if (unit.eq.97) psize = 16
10145 format('set title "',a,'" font "Ariel,',i2,'"')
             write(unit,fmt=10145,iostat=jj,err=995) title(1:ntitle),psize

C
C---- SET THE SCALING PARAMETERS.
C
c       set y range to be like vicar image - lines counting downward from top
c
	if (zoomflag.eq.1) then
		yrang1=pzoom*yrang1
		yrang2=pzoom*yrang2
		y2rang1=yrang1
		y2rang2=yrang2
	endif
10135 format("set yrange [",f10.2,":",f10.2,"]")
             write(unit,fmt=10135,iostat=jj,err=995) yrang2,yrang1
10140 format("set xrange [",f8.2,":",f8.2,"]")
             write(unit,fmt=10140,iostat=jj,err=995) xrang1,xrang2
	  if (phaseplot) then
10155 format("set y2range [",f10.2,":",f10.2,"]")
	     write(unit,fmt=10155,iostat=jj,err=995) y2rang2,y2rang1
	  endif

c            write(unit,fmt=10155,iostat=jj,err=995) iplot, filename(1:nfilename)
c10155 format ('set label ',i2,' "',a,'" at graph .45,.98 font "courier,16" front nopoint tc rgb "red"')



        return
995   continue
        if (unit.eq.97) then
            call xvmessage('??E - write_gpi_ap: Error opening/writing gnuplot eps file',' ')
            call abend
        else
            call xvmessage('??E - write_gpi_ap: Error opening/writing gnuplot file',' ')
            call abend
        endif
	return
        end
c******************************************************************************
	subroutine write_gpi_lab(unit,nhdr,header,nheadr,isize,labstep,aliased)
c
c	write image labels to top of plot
c output labels for only top 60% of plot
c
c
	implicit none
	integer*4 unit,i,ii,jj,isize,psize,nhdr,nheadr(70)
	real*4  fpos,labstep
	logical*4 aliased
	character*86 header(70),aliasmsg
c
	data aliasmsg /"This image is ALIASED"/	
c
	psize = isize
	if (unit .eq. 97) psize = 16
        fpos=1.0
        do ii=2,nhdr
                i = ii - 1
                fpos = fpos - labstep
c	print *,'ii = ',i,header(ii)(1:nheadr(ii)),fpos
10160 format('set label ',i2,' "',a,'" at graph .25 ,',f5.2,
     1 ' font "ariel,',i2,'" front nopoint tc def')
c       1 ' font "ariel 8" front nopoint tc def')
        write(unit,fmt=10160,iostat=jj,err=995) i,header(ii)(1:nheadr(ii)), fpos, psize
cc      print 10160, i,headr(ii)(1:nheadr(ii)), fpos
cc10155 format("set label 2 '",a,"' at graph 0.4, 0.90 front nopoint tc def")
cc        write(98,fmt=10155,iostat=jj,err=995) headr(3)
        enddo
	if (aliased) then
	    i = nhdr + 1
	    fpos = fpos - labstep
10170 format ('set label ',i2,' "',a,'" at graph .25 ,',f5.2,
     1 ' font "ariel,',i2,'" front nopoint tc rgb "red"')
	
	    write(unit,fmt=10170,iostat=jj,err=995) i,aliasmsg,fpos,psize
	endif
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

c******************************************************************************
        subroutine write_gpi_ln(unit,tbl,ntbl,ncolx,ncoly,phaseplot,
     1 ycolstr,ystrl,asymtyp,asyml,lintyp,pttype,ptcolor,ptcolorl)
c
c       write out data in gpi form for pts, lines or linepoints
c       for a 1 value in control column
c
        implicit none
        integer*4 unit,ncolx,ntbl,asyml(20),ystrl(20)
        integer*4 gcol,jj
        integer*4 ncoly(20),lintyp(20),pttype(20),ptcolorl(20)
	logical*4 phaseplot
        character*8 ptcolor(20)
        character*20 ycolstr(20),asymtyp(20)
        character*80 tbl
        character*100 outline

	gcol = 1
	    if (phaseplot) then
10351 format ("plot '",a,"' u ",i2,":",i2," t '",a,"' w ",a," lt ",i2,
     1 " lc rgb '",a,"', \")
           write(unit,fmt=10351,iostat=jj,err=995) tbl(1:ntbl),ncolx,ncoly(gcol),
     1 ycolstr(gcol)(1:ystrl(gcol)),asymtyp(gcol)(1:asyml(gcol)),lintyp(gcol),
     2 ptcolor(gcol)(1:ptcolorl(gcol))

	gcol = gcol + 1
10352 format (" '",a,"' u ",i2,":",i2," ",a," t '",a,"' w ",a," lt ",i2,
     1 " lc rgb '",a,"'")
           write(unit,fmt=10352,iostat=jj,err=995) tbl(1:ntbl),ncolx,ncoly(gcol),
     1 'axes x1y2 ',ycolstr(gcol)(1:ystrl(gcol)),asymtyp(gcol)(1:asyml(gcol)),
     2 lintyp(gcol),ptcolor(gcol)(1:ptcolorl(gcol))

	     else

10361 format ("plot '",a,"' u ",i2,":",i2," t '",a,"' w ",a," lt ",i2,
     1 " lc rgb '",a,"'")
           write(unit,fmt=10361,iostat=jj,err=995) tbl(1:ntbl),ncolx,ncoly(gcol),
     1 ycolstr(gcol)(1:ystrl(gcol)),asymtyp(gcol)(1:asyml(gcol)),lintyp(gcol),
     2 ptcolor(gcol)(1:ptcolorl(gcol))

	    endif

        return

995     continue
        if (unit.eq.97) then
            call xvmessage('??E - write_gpi_ln: Error writing gnuplot eps file',' ')
        write (outline,19999) jj
19999 format ('iostat = ',i8)
            call xvmessage (outline,' ')
            call abend
        else
            call xvmessage('??E - write_gpi_ln: Error writing gnuplot file',' ')
        write (outline,19999) jj
            call xvmessage (outline,' ')
            call abend
        endif

        return
        end
C*******************************************************************************
        subroutine do_profplot (instat,ins,linlas,line_cnt,imgesf_val,
     1 infile,ninfile,imgpsf_avcnt,imgpsf_midpt,pepsplot,n2,realv,aimagv,
     2 crealv,imgpsf_val,psf_long,dgsd,daltitude,pixrad,pixdeg,psfrad,psfdeg,
     3 psfpix,norangle,plotprofgpi,nplotprofgpi,plotprofgpi2,nplotprofgpi2,
     4 plotprofeps,nplotprofeps,ptbl,nptbl)
c
c	plot esf and psf in image space against real part of fft
c
	implicit none
        integer*4 instat,ins,linlas,line_cnt,imgpsf_avcnt
	integer*4 i,jj,n2,esfmax,psfpix
        integer*4 nplotprofgpi,nplotprofgpi2,nplotprofeps,nptbl
	integer*4 ninfile		!,nesfproftab
        integer*4 imgesf_val(1026),psf_long(1026),imgpsf_val(1026)
	integer*4 imgpsf_midpt(1026)
	logical*4 pepsplot
	real*4 REALV(129),AIMAGV(129),crealv(129)
	real*4 norangle
        real*8 dgsd,daltitude,pixrad,pixdeg,psfrad,psfdeg

	CHARACTER*60  INFILE,ptbl			!,esfproftab
        character*80 plotprofgpi,plotprofgpi2
        character*80 plotprofeps
c   plotpsfgpi,nplotpsfgpi,plotpsfgpi2,nplotpsfgpi2,plotpsfeps,nplotpsfeps
c	character*4 dat/'.dat'/
c
c  embed lsf into profile plot since it is smaller thamn esf and fft amplitude
c
c note:  crealv is every 8th pixel from 129 point realv
c
c                print *, 'ptbl = ',ptbl(1:nptbl)
        open(13,file=ptbl(1:nptbl),status='UNKNOWN',iostat=jj,err=991)
c	print *,'           point        esf            lsf       real'
        do i=1,ins
c               print *,'i = ',i,imgesf_val(i),psf_long(i),crealv(i)
            write(13,10020) i,imgesf_val(i),psf_long(i),crealv(i)
10020 format(1x,i6,2x,i6,2x,i6,2x,f10.2)
        enddo
        close(13)
c
c plot commands for PLOTPSF
        open(95,file=plotprofgpi(1:nplotprofgpi),status='UNKNOWN',iostat=jj,err=996)

        call prof_plot (95,ins,imgesf_val,psf_long,ptbl,nptbl,
     1 esfmax,infile,ninfile,dgsd,daltitude,pixrad,pixdeg,psfrad,psfdeg,
     2 psfpix,norangle,n2,realv,aimagv,
     3 plotprofgpi,nplotprofgpi,plotprofgpi2,nplotprofgpi2,plotprofeps,nplotprofeps)
c        endif  !if (pfsplot .eq. 1)

        close (95)
        if (pepsplot) then
            open(96,file=plotprofgpi2(1:nplotprofgpi2),status='UNKNOWN',iostat=jj,err=996)

            call prof_plot (96,ins,imgesf_val,psf_long,ptbl,nptbl,
     1 esfmax,infile,ninfile,dgsd,daltitude,pixrad,pixdeg,psfrad,psfdeg,
     2 psfpix,norangle,n2,realv,aimagv,
     3 plotprofgpi,nplotprofgpi,plotprofgpi2,nplotprofgpi2,plotprofeps,nplotprofeps)

            close (96)
        endif  !if (pepsplot .eq. 1)

	return

991     call xvmessage('??E - main44: Error opening PROFTAB file',' ')
c        print *,'jj = ',jj
        call abend
        return

996     call xvmessage('??E - main44: Error opening/writing gnuplot profplot file',' ')
        call abend
        return


	end
C*******************************************************************************

	subroutine prof_plot (unit,ins,imgesf_val,psf_long,ptbl,nptbl,
     1 esfmax,infile,ninfile,dgsd,daltitude,pixrad,pixdeg,psfrad,psfdeg,
     2 psfpix,norangle,n2,realv,aimagv,
     3 plotprofgpi,nplotprofgpi,plotprofgpi2,nplotprofgpi2,plotprofeps,nplotprofeps)
c
c	plot esf and psf data
c
	implicit none
	integer*4 unit,ins,nptbl,nplotprofgpi,nplotprofgpi2,nplotprofeps
	integer*4 imgesf_val(1026),psf_long(1026)
	integer*4 isize,plotwid,plotht,nxlabel,nylabel1,nylabel2,ntitle,esfmax
	integer*4 i,ii,jj,n2,gcol,ninfile,psfpix,psize,nheadr(5)
	real*4 	xrang1,xrang2,yrang1,yrang2,labstep,fpos,norangle
	real*4 y2rang1,y2rang2
	real*4 realv(129),aimagv(129),realvmax,realvmin
	real*4 aimagvmax,aimagvmin
        real*8 dgsd,daltitude,pixrad,pixdeg,psfrad,psfdeg

	character*32 ylabel1,ylabel2,xlabel
	character*60 ptbl,infile
	character*80 plotprofgpi,plotprofgpi2
        character*80 plotprofeps
	character*100 title
	character*90 header(5)
c
	include 'gnuplotchar'
c
	isize = 10	! font size
	plotwid =  648  !640 @72dpi = 8.888.. inches    9 inch = 648
	plotht  =  504  !480 @72dpi = 6.666.. inches    7 inch = 504
	labstep = 0.03          !0.02 for size 9 font --- originally 0.04
	psize = isize
	if (unit .eq. 96) psize =  16
c
c	Find max of realv	
c
	realvmax = -1.0e10
	realvmin = 1.0e10
	aimagvmax = -1.0e10
	aimagvmin = 1.0e10
	do i=1,n2
	  if (realv(i).gt.realvmax) realvmax = realv(i)
	  if (realv(i).lt.realvmin) realvmin = realv(i)
          if (aimagv(i).gt.aimagvmax) aimagvmax = aimagv(i)
          if (aimagv(i).lt.aimagvmin) aimagvmin = aimagv(i)

	enddo	
	y2rang1 = realvmax + 0.1*(realvmax)
	y2rang2 = realvmin
c	print *, 'realvmax,realvmin = ',realvmax,realvmin
c	print *, 'aimagvmax,aimagvmin = ',aimagvmax,aimagvmin
	
10100 format('# Created by program otf1')              !#'s are ignored in gnuplot
            write(unit,fmt=10100,iostat=jj,err=995)
10105 format('# Gnuplot commands for optical transfer profile plot')
            write(unit,fmt=10105,iostat=jj,err=995)
10110 format('# Data in ',a)
            write(unit,fmt=10110,iostat=jj,err=995) ptbl(1:nptbl)

        if (unit.eq.96) then
10300 format('set terminal postscript eps enhanced color "Ariel" ',i2,' size 11 ,8')
           write(unit,fmt=10300,iostat=jj,err=995) psize
10305 format("set output '",a,"'")
           write(unit,fmt=10305,iostat=jj,err=995) plotprofeps(1:nplotprofeps)
        else
10115 format('set term x11 font "ariel,',i2,'" size ',i4,', ',i4)
            write(unit,fmt=10115,iostat=jj,err=995) isize,plotwid,plotht
10116 format('set output')                              !set output to screen
            write(unit,fmt=10116,iostat=jj,err=995)
        endif
c
c
        ylabel1 = 'DN VALUE'
        nylabel1 = index(ylabel1,'   ') - 1
        ylabel2 = 'FFT AMPLITUDE'
        nylabel2 = index(ylabel2,'   ') - 1
        xlabel = 'Image Samples'
        nxlabel = index(xlabel,'     ') - 1

10125 format("set ylab '",a,"'" )
            write(unit,fmt=10125,iostat=jj,err=995) ylabel1(1:nylabel1)

c          if (phaseplot) then
10126 format("set y2lab '",a,"'" )
                write(unit,fmt=10126,iostat=jj,err=995) ylabel2(1:nylabel2)
c          endif

10130 format("set xlab '",a,"'")
            write(unit,fmt=10130,iostat=jj,err=995) xlabel(1:nxlabel)

c          if (phaseplot) then
10132 format("set y2tics")
            write(unit,fmt=10132,iostat=jj,err=995)

10141 format("set clip points")                         !how to deal with points out of range
            write(unit,fmt=10142,iostat=jj,err=995)
10142 format("set clip one")                            !how to deal with connecting lines out of range
            write(unit,fmt=10141,iostat=jj,err=995)

	title = 'Edge Spread, Line Spread and FFT Profiles for '//infile(1:ninfile)
	ntitle=index(title,'    ') - 1
10145 format('set title "',a,'" font "Ariel,',i2,'"')
             write(unit,fmt=10145,iostat=jj,err=995) title(1:ntitle),psize

C
C---- SET THE SCALING PARAMETERS.
C
c       set y range to be like vicar image - lines counting downward from top
c
	yrang1 = -1
	yrang2 = (esfmax+1)*10
	yrang2 = yrang2/10
	if (yrang2 .lt. 255) yrang2 = 256

	xrang1 = 1
	xrang2 = ins
10135 format("set yrange [",f6.0,":",f6.0,"]")
             write(unit,fmt=10135,iostat=jj,err=995) yrang1,yrang2
10140 format("set xrange [",f6.0,":",f6.0,"]")
             write(unit,fmt=10140,iostat=jj,err=995) xrang1,xrang2
c          if (phaseplot) then
10155 format("set y2range [",f5.2,":",f5.2,"]")
             write(unit,fmt=10155,iostat=jj,err=995) y2rang2,y2rang1
c          endif
c	print *,'here2'
	if (daltitude .gt. 0.0d0) then
	
	     write(header(1),10001) int(daltitude),sngl(dgsd)
10001 format('altitude = ',i10,' meters     gsd = ',f6.2,' meters')
	nheadr(1)=index(header(1),'        ') - 1
	    write(header(2),10002) sngl(pixrad),sngl(pixdeg)
10002 format('pixel size = ',f10.6,' radians     ',f10.6,' degrees')
	nheadr(2)=index(header(2),'        ') - 1
	    write(header(3),10003) sngl(psfrad),sngl(psfdeg),psfpix
10003 format('psf size = ',f10.6,' radians     ',f10.6,' degrees for ',i2,' pixels')
	nheadr(3)=index(header(3),'        ') - 1
	    write (header(4),10004) norangle
10004 format('angle from vertical = ',f10.1,' degrees')
	nheadr(4)=index(header(4),'        ') - 1

            fpos=1.0
            do ii=1,4
                i = ii 
                fpos = fpos - labstep

10160 format('set label ',i2,' "',a,'" at graph .05 ,',f5.2,
     1 ' font "ariel,',i2,'" front nopoint tc def')
c       1 ' font "ariel 8" front nopoint tc def')
        write(unit,fmt=10160,iostat=jj,err=995) i,header(ii)(1:nheadr(ii)), fpos, psize

	   enddo
	endif	!if (daltidude .gt. 0.0d0)
	gcol=1
10361 format ("plot '",a,"' u ",i2,":",i2," t '",a,"' w ",a," lt ",i2,
     1 " lw 2 lc rgb '",a,"',\")
           write(unit,fmt=10361,iostat=jj,err=995) ptbl(1:nptbl),
     1 1,2,'ESF Profile','lines',lntype(gcol),
     2 ptcolor(gcol)(1:ptcolorl(gcol))

	gcol=2
10352 format (" '",a,"' u ",i2,":",i2," t '",a,"' w ",a," lt ",i2,
     1 " lw 2 lc rgb '",a,"',\")
           write(unit,fmt=10352,iostat=jj,err=995) ptbl(1:nptbl),
     1 1,3,'PSF Profile','lines',lntype(gcol),
     2 ptcolor(gcol)(1:ptcolorl(gcol))


        gcol = 3
10355 format (" '",a,"' u ",i2,":",i2," ",a," t '",a,"' w ",a," lt ",i2,
     1 " lw 2 lc rgb '",a,"'")
           write(unit,fmt=10355,iostat=jj,err=995) ptbl(1:nptbl),
     1 1,4, 'axes x1y2','FFT Amplitude','lines',lntype(gcol),
     2 ptcolor(gcol)(1:ptcolorl(gcol))

	if (unit .eq. 95) then
10255 format("pause mouse any")                 !allows plot to display on screen until mouse click
           write(unit,fmt=10255,iostat=jj,err=995)
	endif
	return
C
995     call xvmessage('??E - psf_plot: Error writing gnuplot file',' ')
        call abend
        return

	return
	end
c******************************************************************************
	subroutine prep_prof(instat,ins,n2,linlas,line_cnt,imgesf_val,imgpsf_midpt,
     1 imgpsf_avcnt,imgpsf_val,psf_long,imgesf_tmp,realv,crealv)
c
c	prepare data for prof_plot and/or proftab
c
        integer*4 instat,ins,linlas,line_cnt,imgpsf_avcnt
        integer*4 isum,i2sum,i,i1,n2,esfmax,esfmin,esfmidpt,itmp
        integer*4 psfmin,psfmax,psfmidpt,difmidpt,mult

        integer*4 imgesf_tmp(1026,1026),imgesf_val(1026),psf_long(1026),imgpsf_val(1026)
        integer*4 imgpsf_midpt(1026)
        real*4 REALV(129),crealv(129)

c
c  embed lsf data into profile plot since it is less than
c  the data in esf and fft amplitude
c
c   if input file provided (instead of esf, lsf or sinctst) then do

        if (instat .eq. 1) then
c                print *, 'AVERAGED ESF...'
            i2sum = 0
            do i=1,ins!             linlas        !line_cnt
               isum=0
               do i1=1,linlas        !       ins        !imgpsf_avcnt
c              print *,'imgesf_tmp(i1,i) = ',imgesf_tmp(i,i1)
                   isum = isum + imgesf_tmp(i1,i)
               enddo
               imgesf_val(i) = isum/line_cnt
c                  print *, '1,imgesf_val(i) = ',i,imgesf_val(i)
            enddo
c!         endif
            esfmin=72000
            esfmax= -100
            esfmidpt = 0
            do i=1,ins                      !!!!!!linlas
                if (imgesf_val(i).gt.esfmax) then
c               print *,' imgesf_val(i) = ',imgesf_val(i)
                    esfmax = imgesf_val(i)
                endif
                if (imgesf_val(i).lt.esfmin) then
                    esfmin = imgesf_val(i)
                endif
            enddo
            itmp = (esfmax - esfmin)/2  + esfmin
c                print *,'itmp = ',itmp
            do i=1,ins
c                    print *,'itmp imgesf_val(i) = ',itmp,imgesf_val(i)
                if (itmp .le. imgesf_val(i)) then
                    esfmidpt = i
                    go to 805
                endif
            enddo
805     continue
            itmp=0
c                print *,'esfmax, esfmidpt = ',esfmax,esfmidpt
c
c               integer*4 imgpsf_val(1026),imgpsf_tmp(1026,1026),imgpsf_avcnt
c               integer*4 imgpsf_max(1026),imgpsf_midpt(1026),psf_long(1026)
c       
c       imgpsf_avcnt is number of psf pixels
            psfmidpt = 0
            do i=1,linlas
                 if (imgpsf_midpt(i) .gt. psfmidpt) psfmidpt = imgpsf_midpt(i)
            enddo
            psfmin= 72000
            psfmax= -100 !
            do i=1,imgpsf_avcnt
                if (imgpsf_val(i).gt.psfmax) then
                    psfmax = imgpsf_val(i)
                endif
                if (imgpsf_val(i).lt.psfmin) then
                    psfmin = imgpsf_val(i)
                endif
            enddo
        else    !if (instat .eq. 1)  lsf, esf or sinctst provided ----------
c          print *,'instat = 0'
            do i=1,ins
                imgesf_val(i) = imgesf_tmp(1,i)         !accum_esf
c                print *,' imgesf_val(i) = ',imgesf_val(i)
            enddo
            esfmin=72000
            esfmax= -100
            esfmidpt = 0

            do i=1,ins                      !!!!!!linlas
                if (imgesf_val(i).gt.esfmax) then
c                print *,' imgesf_val(i) = ',imgesf_val(i)
                    esfmax = imgesf_val(i)
                endif
                if (imgesf_val(i).lt.esfmin) then
                    esfmin = imgesf_val(i)
                endif
            enddo
c        print *,'esfmax,esfmin = ',esfmax,esfmin
            itmp = (esfmax - esfmin)/2  + esfmin
            do i=1,ins
c                print *,'itmp imgesf_val(i) = ',itmp,imgesf_val(i)
                if (itmp .ge. imgesf_val(i)) then
                    esfmidpt = i
                    go to 815
                endif
            enddo
815     continue
            itmp=0
c             print *,'esfmax, esfmidpt, = ',esfmax,esfmidpt
c
c               integer*4 imgpsf_val(1026),imgpsf_tmp(1026,1026),imgpsf_avcnt
c               integer*4 imgpsf_max(1026),imgpsf_midpt(1026),psf_long(1026)
c       
c       imgpsf_avcnt is number of psf pixels
            psfmidpt = imgpsf_midpt(1)
            psfmin= 72000
            psfmax= -100 !
            do i=1,imgpsf_avcnt
                if (imgpsf_val(i).gt.psfmax) then
                    psfmax = imgpsf_val(i)
                endif
                if (imgpsf_val(i).lt.psfmin) then
                    psfmin = imgpsf_val(i)
                endif
            enddo

        endif   !if (instat .eq. 1) 

c          print *,'esfmidpt, psfmidpt = ',esfmidpt, psfmidpt
c          print *,'esfmax, psfmax = ',esfmax, psfmax
c          print *,'imgpsf_avcnt = ',imgpsf_avcnt

        difmidpt = esfmidpt - psfmidpt
c   zero out
       do i=1,ins
            psf_long(i) = 0
        enddo


        do i=1,imgpsf_avcnt
c       print *,'i,imgpsf_val(i) = ',i,imgpsf_val(i)
            psf_long(difmidpt+i) = imgpsf_val(i)
        enddo

        mult = int(n2/ins)
c        print *, 'mult = ',mult
        i1 = 0
        do i=1,n2,mult
           i1 = i1 + 1
           crealv(i1) = realv(i)
c           print *,'i,crealv(il) = ',i,i1,crealv(i1)
        enddo




	return
	end

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create otf1.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM otf1

   To Create the build file give the command:

		$ vimake otf1			(VMS)
   or
		% vimake otf1			(Unix)


************************************************************************/


#define PROGRAM	otf1
#define R2LIB

#define MODULE_LIST otf1.f

#define FTNINC_LIST gnuplotchar pgminc

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create otf1.pdf
process help=*
 
PARM INP      TYPE=STRING  COUNT=(0:1)                DEFAULT=--
PARM OUT      TYPE=STRING  COUNT=(0:1)                DEFAULT=--
PARM SIZE     TYPE=INTEGER COUNT=(0:4)                DEFAULT=(1,1,0,0)
PARM TABLE    TYPE=STRING  COUNT=(0:1)                DEFAULT=--
PARM COLUMNS  TYPE=KEYWORD COUNT=(0:1) VALID=(COLHDR,NOCOLHDR) DEFAULT=COLHDR
PARM PLOTOUT  TYPE=STRING  COUNT=(0:1)                DEFAULT="none"
PARM PLOTPROF TYPE=STRING  COUNT=(0:1)                DEFAULT="none"
PARM PLOTFMT  TYPE=STRING  COUNT=(0:1) VALID=(GNUPLOT,EPS)   DEFAULT="GNUPLOT"
PARM PROFTAB  TYPE=STRING  COUNT=(0:1)                DEFAULT="none"
PARM SINCTST  TYPE=KEYWORD COUNT=(0:1) VALID=SINCTST  DEFAULT=--
PARM NOISE    TYPE=INTEGER COUNT=(0:1)                DEFAULT=3
PARM REFLECT  TYPE=KEYWORD COUNT=(0:1) VALID=REFLECT  DEFAULT=--
PARM MEAN     TYPE=KEYWORD COUNT=(0:1) VALID=MEAN     DEFAULT=--
PARM NONORMAL TYPE=KEYWORD COUNT=(0:1) VALID=NONORMAL DEFAULT=--
PARM INTERVAL TYPE=REAL    COUNT=(0:1) VALID=(0:9999.99)  DEFAULT=1.0
PARM ESF      TYPE=REAL    COUNT=(0:512)              DEFAULT=--
PARM LSF      TYPE=REAL    COUNT=(0:512)              DEFAULT=--
PARM PZOOM    TYPE=REAL    COUNT=(0:1) VALID=(0:9999.99)  DEFAULT=--
PARM PHASE    TYPE=KEYWORD COUNT=(0:1) VALID=(PHASE,NOPHASE) DEFAULT=PHASE
PARM NOPRINT  TYPE=KEYWORD COUNT=(0:1) VALID=NOPRINT  DEFAULT=--
PARM DIAG     TYPE=KEYWORD COUNT=(0:1) VALID=DIAG     DEFAULT=--

PARM NORANGLE TYPE=REAL    COUNT=(0:1)                DEFAULT=0.0
PARM GSD      TYPE=REAL    COUNT=(0:1)                  DEFAULT=0.0
PARM ALTITUDE TYPE=REAL    COUNT=(0:1)                  DEFAULT=0.0
local dummy   TYPE=REAL    COUNT=(0:1)
local idum    TYPE=INTEGER COUNT=(0:1)
local strdum  TYPE=STRING  COUNT=(0:1)
PARM PSFRAD   TYPE=NAME    DEFAULT=dummy
PARM PSFDEG   TYPE=NAME    DEFAULT=dummy
PARM PIXRAD   TYPE=NAME    DEFAULT=dummy
PARM PIXDEG   TYPE=NAME    DEFAULT=dummy
PARM PSFPIX   TYPE=NAME    DEFAULT=idum
PARM PSFFORM  TYPE=NAME    DEFAULT=strdum
PARM LSFMIN   TYPE=NAME    DEFAULT=idum
PARM LSFMAX   TYPE=NAME    DEFAULT=idum
!PARM NODISP   TYPE=KEYWORD COUNT=(0:1) VALID=NODISP   DEFAULT=--
END-PROC
.TITLE
VICAR program OTF1
   
   Input an Edge Spread Function, Line Spread Function (or
   one direction of a Point Spread Function) to obtain the 
   Optical Transfer Function (amplitude and phase) of an
   optical system.
.HELP
 
PURPOSE:
 
   OTF1 is a VICAR applications program which performs one-dimensional
Fast Fourier Transformations in order to compute Optical Transfer
Functions (OTF) from degraded edges in images or from either a tabulated
real function, or a line spread function using parameter inputs. 

OTF1 is able to compute the entire optical transfer function (not just 
the MTF) from digital images with greater accuracy and ease than other
methods available. This technique for computing imaging system MTF's is 
preferable to previously used techniques which involved imaging of sine
wave targets.
.page
OPERATION - INPUTS

There are three methods to input data to otf1 and one other internal
test call which requires no input data.

   The first way to input data is an image containing an edge spread
function. That data may have been derived from mathematical considerations
or maybe excised from a test target, shaded buildings, or planet limbs. 
All that is required is an image (INP) containing shapes such as (1) and (2).



          xxx                                      xxx
             x                                    x
              x                                  x
               x                                x
                x                              x
                 x                            x
                  xxxx                    xxxx
             (1)                                (2)
 

   The second method is to put in one line of DNs from an edge spread function
derived from data or mathematical considerations. In that case, You enter
data into the ESF parameter. You may enter the data in either of the (1) or
(2) forms. You may enter up to 512 values. (If you have the patience).

   In the third method you can enter one line of DNs of a line spread function. 
In that case, you enter the DN values into the LSF parameter. Again you may
enter up to 512 values. The entries are in floating point format although
they are processed to produce integer values.

   Finally, there is an internal self-test which requires no input, but is
invoked with the SINCTST keyword parameter. SINCTST creates a SINC function 
and branches to the LSF processor.

  Only one type of input is allowed. If you were to enter multiple inputs
then the program would execute only one of them in the order SINCTST, ESF, 
LSF and INP.
.page
OPERATION - OUTPUTS

   After processing, the program provides output of the Fourier Transform
in terms of frequency, amplitude and phase or frequency, real part of FFT
and imaginary part of FFT. If the user inputs the PHASE=NOPHASE
parameter you can omit the phase although internally it still computes it.
Recall, that the amplitude of the FFT is known as the modulation transfer 
function (MTF).

   The OUTPUTS of the final product can be produced in 3 forms. The first
is specified by the OUT parameter which is a vicar image consisting of
3 lines 124 samples long. The image is in REAL format. The first line
of the image is the frequency (cycles/pixel), the second line is the
the real part of the FFT and the third line is the imaginary part of
the FFT. The phase line is always included even if PHASE=NOPHASE was 
specified.

    The second type output is the TABLE parameter. This table is the
Modulation Transfer Function and consists of either two or three
ascii columns. When it is 3 columns it gives:

        Col 1 FREQUENCY        Col 2  AMPLITUDE        Col 3  PHASE

When it is two columns it is

        Col 1 FREQUENCY        Col 2  AMPLITUDE

Column 3 is not given when the PHASE=NOPHASE is specified. One other
parameter controls whether a column heading is created. That parameter i
is COLUMNS=(COLHDR,NOCOLHDR). By default the header
("# FREQUENCY  AMPLITUDE   PHASE") is written as the first
record of the table. When you choose COLUMNS=NOCOLHDR it is not. Note
the leading # character allows certain plotting programs to ignore
suxch entries.

   Note that these products are only 124 data points long. Internally, the
ESF and PSF data are resampled  to 256 steps from whatever sample size is 
given in the input. Thus, 16 pixels, or whatever, will be expanded to 256.
However, due sampling theory the fourier transform is truncated at
spatial frequencies above 0.484 cycles per sample. This gives a symmeterical
FFT of 247 samples out of 256. What is given in the OUT, TABLE, PLOTOUT
is only the right (124 steps) half of the Fourier transform (if viewed 
as having the DC at the center). To get the output translated to the way we
are used to seeing it, you need to reflect the image or table about the
center point to get the full 247 sample spectrum from 1.0 cycles/sample 
to 0.484 cycles/sample in separate Vicar steps.

   See PLOTOUT discussion below

    The other types of output come from the PLOTOUT and PLOTPROF parameters.
PLOTOUT displays the data in the OUT or TABLE files on an x,y plot using
the gnuplot package. PLOTOUT produces a file of gnuplot commands as shown
with a .gpi file extension.  It also produces a file with the .gpi.dat
extension. This is the data used for the plot using command in the gpi file.
It happens to be same data as in the TABLE parameter (without the column 
headers), but since it can't be guaranteed that the user will create a
table, it creates its own.   

   The PLOTPROF parameter allows the user to display intermediate product
in the processing steps from ESF to the fourier transform in pixel space,
i.e., in the size of the ESF or LSF input pixels, not in the resampled 
256 step mode.

.page
OPERATION - INTERNALS

    In short, what is done is to convert

  edge spread function --> line spread function --> Modulation Transfer Function
                  by differentiation          by  FFT


    The edge spread function can be defined as the integral of one dimensional
pointspread functions. That is

                ESF(x) = Integral of (LSF(x) dx)

                        d
                LSF(x)  -- [ESF(x)]
                        dx

  See: http://calval.cr.usgs.gov/JACIE_files/JACIE04/files/1Helde10.pdf for
  the Quickbird Satellite.

    Internally, the program takes in an ESF or LSF and increases it in size 
to 256 samples. Thus, any ESF of small sample size will be resampled up to 256.
In this way, the output LSF (unidirectional PS) which may be either in the
line or sample direction in the original image can be combined with another
LSF in the orthogal direction to give a 2-D PSF.

The 2-D fft can be created from this and then convolved with a 256x256 image.
This represents the optical system blurring. Here convolution is multipling 
the 2 FFT's together.
 
   To aid in the preparation of filter weights for MTF enhancement, a provision
has been added to output the Real and Imaginary Fourier Transform components.
The output is 3 lines of 124 samples of Real*4 numbers organized as 
Line 1 [Frequency] 
Line 2 [Real Part of FFT],
Line 3 [Imaginary Part of FFT]

from 1.0 cycles/sample to 0.484 cycles/sample.
The output does not extend beyond 0.484 cycles/sample due to
limitations of the Sampling Theorem used for LSF resampling (see last para-
graph of Method below).


.page
 EXECUTION:
 
   OTF1 INP  OUT  PLOTOUT  PARAMS
 
 where: INP is an optional input image. 
 
        OUT is an optional output file with fourier transform data.
 
        PLOTOUT is an optional plot of the output in gnuplot instructions (gpi).
 
        PARMS are optional parameters, which are described below and
             under Tutor.
.page
METHOD:
 
   The data are normally in an input image and are processed line by line.
Each line is left adjusted into an array of dimension equal to the smallest
power of two which contain an entire input line. The data is then different-
iated to produce a Line Spread Function (LSF). For direct input of a Line
Spread Function, the parameters LSF is followed by the LSF values (Real*4).
Processing will commence with the LSF ready for fillin to the nearest power
of two using the MEAN, REFLECT, or FILLIN parameters. For direct input of
a theortical "real" edge or mathematical function, the parameter ESF is
followed by a string of values (Real*4) and will be processed as thought it
was a real one-line image input. SINCTST will cause a SINC function to be
formed and processed as though it were the LSF of an edge.
 
  The Maximum point in the LSF is located and the LSF truncated to 30 points
on either side of the LSF Maximum. If there are less than 30 points to either
side the option exists to fill in extra points by replecating the LSF mean,
by reflection, or with zeros. This LSF is then left justified and the Sampling
Theorem applied to produce a 256 point power spectrum. In addition the
resampled LSF is folded about the maximum, with points from the maximum to the
upper end placed from i=1 to the midpoint, and points from the maximum to the
lower end placed from near the midpoint to 1=256. This array is then in the
proper form upon which the subroutine FFTT can be used to perform an inverse
transform. The real and imaginary components of the transform for each line
are accumulated element by element, and these operations continued until all
the image lines have been processed. From the accumulated real and imaginary
transform components the following are computed and printed.
.page
   Given a complex FT element "a+ib" the output will consist of:
 
(1) The spatial frequency in cycles/pixel.
(2) The actual frequency in cycles/unit of time (INTERVAL).
(3) a=Real part of FT.
(4) b=Imaginary part of FT.
(5) SQRT(a**2+b**2)=The amplitude of the FT. This is also the MTF if the
    input data is an LSF.
(6) (a**2+b**2)=The power spectrum or intensity of the FT.
(7) The Normalized amplitude of the MTF which will be plotted.
(8) ACRTAN(b/a)=The phase angle Omega of the FT measured in radians
    which will also be plotted
(9) W=Omega/2*pi*F=The displacement in the same units as INTERVAL
    (the default of which is samples) of each sine wave component in
    the image from its true position in the object being imaged. F is
    in units of 1/INTERVAL. This quantity is meaningful if the input
    data is an LSF and if OTF has been specified.
.page
If PLOTOUT was specified the Normalized MTF Amplitude and Phase are processed
into an output file containing gnuplot instructions (gpi).
In this case the terminal screen plot is disabled.
 
The default is to generate a display of the pertinent on the terminal screen
components of the OTF in a crude but rapidly analyzable format. Each
curve's data points are represented by a letter with the following code:
 
   A   Amplitude of the FT
   R   Real portion of the FT
   I   Imaginary portion of the FT
   W   Wavelength shift of the FT.
.page
The FT is computed from
 
           N-1
    FT =SUM    [DATA *Exp(-2*PI*inm/N)]                   (1)
           n=0      n
 
where N is the data string length and  0<m<(N-1).
 
   The OTF section of the program is designed to generate an LSF positioned
in such a way as to eliminate both the effects of sampling and the bias
introduced into the phase OMEGA due to the arbitrary positioning of the LSF
data in the input array. Users of FFTT have been forced to obtain clean
OTF's by using symmetrical LSF's positoned half at the beginning of the
data string and half at the end. This was inconvenient and impossible if
the LSF was asymmetrical. In the latter event all they could do was settle
for the MTF, never knowing what the OTF was like. In a situation where the
OTF has gone negative, the MTF is dangerous to use as a restoration function
because it will boost the negative amplitudes the wrong way. The OTF allows
the user to visualize the scrambling of spatial frequencies which occur when
the LSF is asymmetrical, and also to see the sign of the OTF (given by the
real part of the FT).
 
   There are two problems associated with generating unbiased OTF's. They
can be solved simultaneously by use of the Sampling Theorem. If the LSF is
computed from a degraded edge composed of digital data, it is clear that
(particularly for narrow LSF's) the position of the sampled points is going
to influence the symmetry of the LSF. Clearly, trying to dissect several
LSF's into two halves (each different) and store them at opposite ends of
the data string before the FT operation, is going to produce different OTF's
each time. Because the data is band limited, at least at the Nyquist Frequency,
the MTF's generated in the above case will all be about the same. The problem
is to eliminate the effects of sampling. The Sampling Theorem is used by OTF1
to resample the acquired LSF about its center of weight. At this point, if the
LSF is asymmetrical, the effect is real.
.page
   In computing the weight center of the LSF a first moments algorithm is
used:
 
              n=N                  n=N
    CENTER=SUM   [(n-1)*DATA ]/ SUM   [DATA ]               (2)
              n=1           n      n=1     n
 
The Data are then resampled about the CENTER position using
 
            n=+INF
    INT =SUM      [DATA *((SIN(x-n)*PI)/((x-n)*PI))]          (3)
       x    n=-INF     n
 
where x is some integer distance away from CENTER. The data can then be
split about the located center and positioned properly in the array to be
transformed.
 
   The use of many LSF's to produce one OTF is done by averaging the
transform components. Averaging MTF's (if that's what you want) can be
erroneous because even OTF's, which are in reality positive, can be
caused to go negative if one of the LSF's is afflicted with noise. Since
the MTF is the modulus of the OTF, the MTF is always positive. Great care
should be taken in inspecting the LSF's computed from pictures before the
resampling step to be sure they look decent. Averaged runs of 50 or more
LSF's should yield OTF's which are accurate to within a few percent. This
is an order of magnitude better than the sine wave methods used in the past.
 
   Because the Sampling Theorem summation limits must be finite (in this
case +/- 128) the theorem is blind to the spatial frequencies above 0.484
cycles per sample. The OTF values above this frequency are best obtained
by extrapolating from values below 0.484 to the Nyquest limit.
.page
OUTPUTS

OUT      =  An image of the special format. It is a REAL vicar image of 3
            lines each 124 samples long; line 1 is frequency, Line 2
            is amplitude (normalized or unnormalized depending on
            the NONORMAL parameter), Line 3 is the phase.
PLOTOUT  =  A plot of 1-D FFT amplitude and phase (256 points) 

PLOTPROF =  A plot of the profiles of ESF, LSF and FFT in pixel space

TABLE    =  An ascii table of half of FFT of 2 or 3 columns. When 3 columns it gives
            Col 1 FREQUENCY        Col 2  AMPLITUDE        Col 3  PHASE

PROFTAB   =  An ascii table of the PROF in pixel space




.page
 PARAMETERS
     INP        Optional - Input is an image containing an object with an 
                edge spread profile      
     OUT        Optional - A vicar image of special format of the 
                Frequency, Amplitude and Phase 
     SIZE       Optional - Extract out of the image an area containing 
                an edge spread profile. Only when INP is specified.
                It is the usual vicar size parameter, SL, SS, NL, and NS.
     TABLE      Output an ASCII table of the data in the in plotout,
                Frequency
     PROFTAB    Output an ascii table containing the profiles of LSF, ESF 
                and Fourier Transform in pixel space 
     COLUMNS    Add or omit column headers on the output TABLE
     DIAG       Optional - Printout internal diagnostic data - Mostly is
                used for debugging. Also used for especially noisy images.
     SINCTST    Optional - Perform an internal test utilizing the sinc 
                function. Ignores INP, LSF or ESF
     NOISE      Allowable noise level in LSF for finding end points, in 
                units of sigma. Default is 3 sigma 
     PLOTOUT    Output a file containing gnuplot instructions to display,
                the otf in normalize amplitude [0,1} vs. cycles/pixel 
     PLOTPROF   Output a file containing the intermediate results on the
                data entered either in INP,ESF or LSF
     REFLECT    When padding data internally to compute the FFT
     MEAN       Fill in by replecating mean LSF
     NONORMAL   Do not normalize (scale DNs from 0 to 1.0) in the LSF, 
                i.e., Keep the LSF in DN values.
     NOPRINT    Do not print out the data on the screen which is sent to the
                OUT file.
     INTERVAL   Value < 1.0. The amount of reduction in the size of the 
                cycles/pixel to display on the x-axis. 0.5 means one-half.
     ESF        Input one line of edge spread data. Up to 256 samples.
                Ignores INP, SINCTST, and LSF.
     LSF        Input one line of line spread data. Up to 256 samples.
                Ignores INP, SINCTST and ESF.
     PZOOM      Phase Zoom -  Zoom the vertical axis of the phase part of the
                plot by this factor
     PHASE      Omit the display of phase on the PLOTOUT or OUT file.
     NORANGLE   Angle from north, in degrees - eastward rotation is postive, 
                westward is negative.
                This is an angle with respect to image coordinates. Not earth 
                coordinates. This program does not access geotiff information.

  The following two inputs are required in order to obtain values for the 
                output TCL variables:
     GSD        Input a ground sample distance, in meters, for computing the 
                LSF in radians and degrees,
     ALTITUDE   Input the altitude of the spacecraft in meters.

    The following output TCL variables require entries in ALTITUDE and GSD.
     PSFRAD     Output the psf in radians along the angle specified in NORANGLE
     PSFDEG     Output the psf in degrees along the angle specified in NORANGLE
     PIXRAD     Output the pixel size in radians 
     PIXDEG     Output the pixel size in degrees
     PSFPIX     Size of the lsf (psf) in pixels.
     LSFFORM    LSF NORMAL or NONORMAL, normalized or not normalized
     LSFMAX     LSF max value in pixel space
     LSFMIN     LSF min value in pixel space           

.page
.PAGE
PLOT OUTPUTS

    The other types of output come from the PLOTOUT and PLOTPROF parameters.
PLOTOUT displays the data in the OUT or TABLE files on an x,y plot using
the gnuplot package. PLOTOUT produces a file of gnuplot commands as shown
with a .gpi file extension.  Another file with an .asc extension is createid
containing columns of data that are displayed by the gpi file.
It happens to be same data as in the TABLE parameter (without the column
headers), but since it can't be guaranteed that the user will create a
table, it creates its own.

   The PLOTPROF parameter allows the user to display intermediate product
in the processing steps from ESF to the fourier transform in pixel space,
i.e., in the size of the ESF or LSF input pixels, not in the resampled
256 step mode.

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

  Note: This program creates multiple output plots per run. You bring up each plot
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



.page
 RESTRICTIONS:
 
  1. Input picture record size <= 2048 samples.
 
  2. LSF or DATA input elements <= 256.

  3. BYTE and HALF images and values only
.page
 PROGRAM HISTORY:

  1974-05-16 J. LORRE   - ORIGINAL RELEASE.
  1975-03-10 J. LORRE   - RE-RELEASE.
  1975-06-27 D. HASS    - CONVERSION TO 360/OS.
  1982-12-10 E. KORSMO  - PRINTRONIX PLOT PACKAGE ADDED.
  1983-02-02 E. KORMSO  - REVISION TO PRINTRONIX PLOTING.
  1984-11-06 M. MORRILL - CONVERTED TO VAX-VICAR*2.
  1984-12-06 M. MORRILL - CODE MODIFIED TO CONFORM TO THEORY FROM
                          R.NATHAN AND TO REMOVE FILTER OPERATIONS.
  1985-04-17 M. MORRILL - MINOR FIXES TO ADD QUANTITAVE ESTIMATE OF
                          OTF CURVE.
  1985-04-30 M. MORRILL - SAMPLING THEOREM "BUGS"
  1985-05-02 M. MORRILL - REMOVE VAX/FFT FACTOR OF 2 & CLEAN UP
                          NORMALIZATIONS
  1985-05-06 M. MORRILL - ADD OUTPUT FOR F.T. COMP.
  1985-11-15 F. MOSS    - 1.modify the plot for phase angle (the axis
                          range from -1 to 1 instead of from -3 to 3) 
                          2.add a new param "PZOOM" so the user can 
                          either zoom up (PZOOM >1.) or zoom down 
                          (PZOOM <1.) the phase angle
  1987-05-07 F. MOSS    - 1.modify the code to handle size field
                          correctly 2.modify output's XVOPEN and 
                          XVWRIT calling parameters 3.put "NONORMAL" 
                          keyword in the source if the input is a line 
                          spread function
  1988-08-11 F. MOSS    - 1.Add warning message for the line without
                          edge. 2.Fourier Transform will not apply to 
                          the line without edge. 3.Print out the total 
                          number of lines processed. 4.Fix the bug to 
                          avoid calling XVWRIT if there is no output 
                          required. (FR 35605)
           ? J. Lorre   - Scaling of phase plots.
  1994-02-23 C. Avis    - 1.Add tabular output, property label support,
                          2.Improved test and help files.
  1995-06-27 C. AVIS    - Modified test file to use perfect ramp case
  1995-06-28 C. AVIS    - Added header to ASCII table output
  1995-09-28 C. AVIS    - Added option for no Phase to table
  1997-08-05 T. Huang   - Ported from VAX/VMS to UNIX and ALPHA/VMS. 
                          Combined the changes made by C. Avis with 
                          the existing MIPS version of OTF1. Fixed 
                          problems caused by subroutine FFTT, by reducing
                          the precision in calculation with imaginary 
                          part of a complex number for PHASE. (REF: fr90520)
  2010-02-09 L. Kamp    - Bug fixes in code.  Added text to help file to
                          make it clear that 'NODISP is required to save 
                          plots and updated Test proc accordingly. Removed 
                          Cassini test from proc since the file is missing; 
                          added some other tests of NOISE parameter.
  2012-10-30 R. Bambery - Rewritten to use gnuplot instead of the
                          XRT. Graphics package. Removed NODISP parameter.
  2012-11-16 R. Bambery - Add parameters PLOTPSF, PLOTFMT, PSFTAB,
                          NORANGLE, GSD, ALTITUDE. Changed PLOT to PLOTOUT 
                          DATA to ESF.
  2012-12-10 R. Bambery - Remodularization of internals
  2013-01-01 R. Bambery - Documentation changes
  2013-01-02 R. Bambery - Fixed problems in do_psfplot and logic of main44
  2013-02-13 R. Bambery - Updated documentation
  2013-07-12 R. Bambery - Make naming conventions consistent. Separate
                          TABLE data from plot ascii table
  2013-07-13 R. Bambery - Adjusted eps format to more readable
                          fonts. Remove vestiges of debug statements
  2015-08-10 W. Bunch   - replaced xqout call with xvqout call to pass
                          out vars to shell vicar
 
CURRENT COGNIZANT PROGRAMMER:  Ray Bambery
 
.LEVEL1
.VARIABLE INP
 An input image.
.VARIABLE OUT
 Output of Real and
 Imaginary Fourier
 Transform components.
.VARIABLE SIZE
 INTEGER-OPTIONAL
 SL,SS,NL,NS
.VARIABLE ESF
 REAL-OPTIONAL
 Real numbers of which the FT
 will be taken.
.VARIABLE LSF
 REAL-OPTIONAL
 Up to 256 elements from which
 the OTF will be determined.
.VARIABLE TABLE
 STRING-OPTIONAL
 File to receive tabular output.
 In ASCII, not IBIS.
.VARIABLE COLUMNS
 keyword - OPTIONAL
 Specifies whether to write
 column headers in the
 ASCII TABLE.
.VARIABLE PROFTAB
 STRING-OPTIONAL
 Output an ascii table 
containing the profiles
of ESF, LSF and FFT
.VARIABLE PLOTOUT
 STRING-OPTIONAL
 Turns on PLOT of Amplitude
 and phase.
.VARIABLE PLOTPROF
 STRING-OPTIONAL
 Turns on PLOT of ESF
 and LSF of intermediate
 products.
.VARIABLE PLOTFMT
 STRING-OPTIONAL
 Plot data with Gnuplot
 or in EPS format. For both
 PLOTOUT and PLOTPROF.
.VARIABLE DIAG
 KEYWORD-OPTIONAL
 diaganostic printout.
.VARIABLE SINCTST
 KEYWORD-OPTIONAL
 Creates SINC function
 and branches to LSF processing.
.VARIABLE NOISE
 INTEGER-OPTIONAL
 Allowable noise level in LSF.
.VARIABLE REFLECT
 KEYWORD-OPTIONAL
 Reflects data about last
 input point.
.VARIABLE MEAN
 KEYWORD-OPTIONAL
 Causes missing data to be set
 to mean of input data.
.VARIABLE NONORMAL
 KEYWORD-OPTIONAL
 Do not normalize LSF.
.VARIABLE NOPRINT
 KEYWORD-OPTIONAL
 Do not print full tabular
 output.
.VARIABLE INTERVAL
 REAL-OPTIONAL
 interval between
 data points.
.VARIABLE PZOOM
 REAL-OPTIONAL
 Scaling factor for phase
 in plots.
.VARIABLE PHASE
 keyword - optional
 Specifies whether to print
 the Phase to the Ascii table
 or on plot.
 or not.
 
.LEVEL2
.VARIABLE INP
 An input image  which the user wishes to Fourier Transform.
 The FT will be taken of each line and the components then averaged.
 Restriction on input record length is that it not exceed 2048 samples.
 
 If no input image is specified, data may be specified through the
 ESF  or LSF parameters.  If no data are specified, OFT1 will run an
 internal sinc-function test case.
.VARIABLE OUT
 Optional output of Real and Imaginary Fourier Transform components.
 Output is 124 triplets of Real*4 numbers organized as:
   [Frequency,Real,Imaginary],
 from 1.0 cycles/sample to 0.484 cycles/sample.
.VARIABLE SIZE
 INTEGER-OPTIONAL
 Specifies the SL,SS,NL,NS of the image for which the FT will be taken
 of each line separately and then the transforms will be averaged.
.VARIABLE ESF
 REAL-OPTIONAL
 Inputs a string of real arguments which one desires to take the FT of.
 This is limited to 256 elements.
.VARIABLE LSF
 REAL-OPTIONAL
 Input string of real Line Spread Function values for which an OTF is
 desired. This is limited to 256 element.
.VARIABLE PLOTOUT
 STRING-OPTIONAL
 Specifies the filename to receive the output gnuplot instructions.
.VARIABLE PLOTPROF
 STRING-OPTIONAL
 Turns on PLOT of ESF, LSF and FFT of intermediate products.
.VARIABLE PLOTFMT
 STRING-OPTIONAL
 Plot data with Gnuplot or in EPS format. For both PLOTOUT and PLOTPSF.
.VARIABLE DIAG
 KEYWORD-OPTIONAL
 Turns on diaganostic printout.
.VARIABLE SINCTST
 KEYWORD-OPTIONAL
 Creates SINC function and branches to LSF processing.
.VARIABLE NOISE
 INTEGER-OPTIONAL
 Specifies allowable noise level in LSF for finding end points.
.VARIABLE REFLECT
 KEYWORD-OPTIONAL
 Cauese the input data to be reflected about the last input data
 point in order to fill in unspecified locations. The program must
 eventually have a data string which is a power of 2 in length. The
 default is to fill with zeros.
.VARIABLE MEAN
 KEYWORD-OPTIONAL
 Causes missing data to be set equal to the mean of input data.
 Default is to fill with zeros.
.VARIABLE NONORMAL
 KEYWORD-OPTIONAL
 Causes the data to not be normalized so that its sum is unity.
 Default is to normalize the input LSF.
.VARIABLE NOPRINT
 KEYWORD-OPTIONAL
 Suppresses the printout of the full tabular listing.  This listing
 normally includes FOR EACH POINT IN FREQUENCY:
 CYCLES/SAMPLE, FREQUENCY, REAL, IMAGINARY, INTENSITY, AMPLITUDE,
 PHASE and WAVELENGTH SHIFT.
 The default is to print the table.
.VARIABLE INTERVAL
 REAL-OPTIONAL
 Defines the time interval between data points. Default is 1.0 and defines
 the frequency in cycles per sample.
.VARIABLE PZOOM
 REAL-OPTIONAL
 Specifies the scale factor used to plot the phase values so that the
 values are easier to read on the plot.
 The scale factor will be printed on the plot
 (e.g., PHASE (RAD) MULTIPLY *   2.00  if PZOOM =2.0)
.VARIABLE TABLE
 STRING-OPTIONAL
 Specifies the name of a file to receive tabular output.  This file
 contains ASCII text in tab-delimited form.  The columns of the table
 contain Frequency, Amplitude and Phase.
.VARIABLE COLUMNS
 keyword - OPTIONAL
 Specifies whether to include column headers in the ASCII TABLE.
 COLHDR specifies to include the headers.  NOCOLHDR specifies no headers.
 The headers are "FREQUENCY", "AMPLITUDE" and "PHASE".
 Default is to put headers in the table.
.VARIABLE PHASE
 keyword - OPTIONAL - Valid=(phase,nophase)
 Specifies whether to include a column for Phase in the ASCII table.
 Frequency and Amplitude are always included.  Default is to include it.
.end

$ Return
$!#############################################################################
$Test_File:
$ create tstotf1.pdf
procedure
parm    mode    type=keyword count=(0:1) valid=(batch,nobatch,inter) default=batch
local   minval  type=real  count=1
local   maxval  type=real  count=1

local   pixrad  type=real  count=1
local   pixdeg  type=real  count=1
local   psfrad  type=real  count=1
local   psfdeg  type=real  count=1
local   psfpix  type=integer  count=1

! removed VMS - tailored to linux/macosx on AFIDS/Vicar
!

! Nov 15, 2012 - RJB
! TEST SCRIPT FOR OTF1
! tests BYTE, HALF images
!
! Vicar Programs:
!       copy size gen insect list genthis boxflt2 addnoise
!       fft11 ccomp maxmin cform lave img2ascii typetext deriv
! 
! External programs
!       gnuplot 4.6 or greater
! 
! Parameters:
!   mode - method for processing: 
!       1) batch provides no display 
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
!    External data from Cassini is no longer available so has
!       been removed from this script
!     
! Output:    
!   GENed test.imga sets, .gpi and .eps files and intermediate 
!       tmp* files 
!   the *.gpi data produced by otf1 are gnuplot scripts
!            
! NOTE: There are some extra steps that can be run after TEST 32
!       If desired, follow the instructions given there,
!
refgbl $autousage
refgbl $echo
refgbl $syschar
body
enable-log

let _onfail="stop"
let $echo="yes"
!

!
!*********************************************************
! Generate images needed for input to OTF1
!*********************************************************
!
! MAKE STEP FUNCTION IMAGE - BYTE FORMAT
gen out=bk.img nl=40 ns=40 linc=0.0 sinc=0.0 ival=0.0
gen out=wt.img nl=40 ns=40 linc=0.0 sinc=0.0 ival=218
insect inp=(bk.img,wt.img) out=edge.img size=(1,1,40,40) +
   insect=(1,1,40,20,1,21)
list edge.img (1,11,40,30)

!MAKE LINEAR RAMP FOR EDGE, AND EXPAND INTO A 20X16 IMAGE
genthis a.img nl=1 ns=16 +
   dn=(0,0,0,0,24,48,72,96,120,144,168,192,216,216,216,216)
size a.img s1edge.img 'noin lzoom=20 szoom=1
list s1edge.img
!deriv inp=s1edge.img out=(s1lsf.img,s1infl.img) orient=horiz
!SMOOTH THE RAMP WITH A BOX FILTER
boxflt2 inp=s1edge.img out=s2edge.img nlw=9 nsw=9
list s2edge.img
! generate derivative to get lsf
!deriv inp=s2edge.img out=(s2lsf.img,s2infl.img) orient=horiz
! NOW add noise to ramp
addnoise s1edge.img s1edgeN.img sigma=3 seed=1351456636

addnoise s2edge.img s2edgeN.img sigma=3 seed=1351456636
!
! generate short ramp
!
genthis b.img nl=1 ns=8 +
    dn=(0,0,24,72,120,168,216,216)
size b.img t1edge.img 'noin lzoom=20 szoom=1
list t1edge.img


!SMOOTH THE RAMP WITH A BOX FILTER
boxflt2 inp=t1edge.img out=t2edge.img nlw=5 nsw=5
list t2edge.img


! NOW add noise to ramp
addnoise t1edge.img t1edgeN.img sigma=3 seed=1351456636

addnoise t2edge.img t2edgeN.img sigma=3 seed=1351456636
!
! generate padded short ramp
!
genthis c.img nl=1 ns=16 +
    dn=(0,0,0,0,0,0,60,96,108,170,216,216,216,216,216,216)
size c.img u1edge.img 'noin lzoom=20 szoom=1
list u1edge.img

!SMOOTH THE RAMP WITH A BOX FILTER
boxflt2 inp=u1edge.img out=u2edge.img nlw=5 nsw=5
list u2edge.img

! NOW add noise to ramp - use a fixed seed for test
addnoise u1edge.img u1edgeN.img sigma=3 seed=1351456636

addnoise u2edge.img u2edgeN.img sigma=3 seed=1351456636
!
!*********************************************************
! Completed images needed for input to OTF1
!*********************************************************
!

!
! otf1 has many parameters and many tests
!
! The simplest is an internal test - The SINC test
! 
! TEST 0 - INTERNAL TEST WITH SINC FUNCTION - output table
! 
otf1 'sinctst plotout=sinctst
!let $echo="no"
if (mode = "nobatch" or mode = "inter")
    ush gnuplot sinctst.gpi
end-if
!
!  TEST 1 - SINCTST with diagnostics
! an output and diagnostics

otf1 'diag 'sinctst plotout=sinctst2 table=sinctst2.tbl
typetext sinctst2.tbl
!let $echo="no"
if (mode = "nobatch" or mode = "inter")
    ush gnuplot sinctst2.gpi
end-if

! There are 3 mutulually exclusive input types to otf1
! 1) Edge Spread Function - a 1-line entry
! 2) Line Spread Function - a 1-line entry
! 3) An image containing an edge
!
! First, is to input an Edge Spread Function
!
! TEST 2 - ESF entry
!
! 16 step ESF - output, 2 plots and no other parms
!       Note 16 steps is a power of 2 and aids internal FFT
!
otf1 out=test2.tbl esf=(0,0,0,0,24,48,72,96,120,144,168,192,216,216,216,216) +
    plotout=test2
if (mode = "nobatch" or mode = "inter")
    ush gnuplot test2.gpi
end-if
!
! TEST 3 - Another ESF entry
!
! 13-step ESF - output, 2 plots and no other parms
! this is a non-power of two entry
!
!
otf1 out=test3.img esf=(0,0,0,12,30,62,94,126,158,190,206,224,224) +
    plotout=test3 plotprof=test3
if (mode = "nobatch" or mode = "inter")
    ush gnuplot test3.gpi
    ush gnuplot test3.prof.gpi
end-if

copy test3.img test3_r.img sl=2 ss=1 nl=1 ns=124
size test3_r.img test3_124r.img 'noin lzoom=124 szoom=1

polarect test3_124r.img test3_cr.img  'AUTO XCEN=62 YCEN=62
!
! TEST 4 - 16-step ESF entry with multiple parameters
!
!
otf1 out=test4.img esf=(0,0,0,0,24,48,72,96,120,144,168,192,216,216,216,216) +
    plotout=test4 plotprof=test4 proftab=test4 'noprint

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test4.gpi
    ush gnuplot test4.prof.gpi
end-if
!
! TEST 5 - Reverse Step wedge
!
otf1 table=test5.tbl columns=nocolhdr plotprof=test5 proftab=test5 +
  plotout=test5 +
  esf=(100,100,100,100,99,90,66,33,20,18,17,17,17,17,17,17)

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test5.gpi
    ush gnuplot test5.prof.gpi
end-if

!
! TEST 6 - LINE SPREAD FUNCTION
! check against lsf of s2edge.img - below
!
otf1 out=test6.img lsf=(0,8,10,14,16,18,22,24,21,19,16,13,11,8) +
   plotout=test6 'noprint

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test6.gpi
end-if
!
! TEST 7 - Simplest invocation with input image - simple 0,1 step function
!       prints out a 8-column table and a plot of columns 3, 4, 6, and 8 
!       versus frequency (column 1 or 2)  vertically to the terminal screen
!
otf1 inp=edge.img
!
! TEST 8 - Repeat Test 1 but noprint
!
otf1 inp=edge.img 'noprint
! 
! TEST 9 - Repeat edge, but with a plot
!
otf1 inp=edge.img plotout=test9
if (mode = "nobatch" or mode = "inter")
    ush gnuplot test9.gpi
end-if
!
! TEST 10 - Repeat edge, but with a plot and NO phase
!
otf1 inp=edge.img plotout=test10 phase=nophase
if (mode = "nobatch" or mode = "inter")
    ush gnuplot test10.gpi
end-if

!
! TEST 11 - Do same for a noiseless ramp Get an output image and table 
!
otf1 inp=s1edge.img plotout=test11

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test11.gpi
end-if
!
! TEST 12 - Do same for a smooth ramp
!
otf1 inp=s2edge.img plotout=test12

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test12.gpi
end-if
!
! TEST 13 - Now a noisy ramp
!
otf1 inp=s1edgeN.img plotout=test13

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test13.gpi
end-if
!
! TEST 14 - Finally, a Noisy smoothed ramp
!
otf1 inp=s2edgeN.img plotout=test14

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test14.gpi
end-if
!
! TEST 15 - Do similar for a short ramp
!
otf1 inp=t2edge.img plotprof=t2edge plotout=test15

if (mode = "nobatch" or mode = "inter")
    ush gnuplot t2edge.prof.gpi
    ush gnuplot test15.gpi
end-if
!
! TEST 16 - noisy short ramp
!
otf1 inp=t2edgeN.img plotprof=t2edgeN plotout=test16
if (mode = "nobatch" or mode = "inter")
    ush gnuplot t2edgeN.prof.gpi
    ush gnuplot test16.gpi
end-if
!
! TEST 17 - padded short ramp
!
otf1 inp=u2edge.img plotprof=u2edge plotout=test17
if (mode = "nobatch" or mode = "inter")
    ush gnuplot u2edge.prof.gpi
    ush gnuplot test17.gpi
end-if

!
! TEST 18 - padded short Noisy ramp
!
otf1 inp=u2edgeN.img plotprof=u2edgeN plotout=test18
if (mode = "nobatch" or mode = "inter")
    ush gnuplot u2edgeN.prof.gpi
    ush gnuplot test18.gpi
end-if

! Now we will repeat some of the above tests 
! with other parameters
!
!
! TEST 19 - Simple Ramp - Get an output image and table
!
otf1 inp=s1edge.img out=test19.img table=test19.tbl  plotout=test19
typetext test19.tbl

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test19.gpi
end-if
!
! TEST 20 - Simple Ramp - Get an output image and table
!       No headers in table
!
otf1 inp=s1edge.img out=test20.img table=test20.tbl  plotout=test20 +
    columns=nocolhdr
typetext test20.tbl

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test20.gpi
end-if
!
! TEST 21 - get warnings about image size - image is smaller
!   than requested
!
otf1 inp=s1edge.img size=(1,1,35,40) plotout=test21

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test21.gpi
end-if

!
! NEED TO TEST  NOISE. REFLECT, MEAN, NONORMAL, INTERVAL, DATA,
!  LSF, PZOOM, DIAG (on other than sinctest)
!
! TEST 22 - Reflect option - Compare with TEST 5 
!
otf1 inp=s1edge.img 'reflect plotout=test22

if (mode = "nobatch" or mode = "inter")
    ush gnuplot --persist test22.gpi
!    ush gnuplot --persist test5.gpi
end-if

!
! TEST23 - Missing data set to mean 
!
otf1 inp=s1edge.img plotout=test23 'mean table=test23.tbl
typetext test23.tbl
if (mode = "nobatch" or mode = "inter")
    ush gnuplot test23.gpi
end-if

!
! TEST 24 - NORMAL=NONORMAL option
!
otf1 inp=s1edge.img plotout=test24 'nonormal table=test24.tbl
typetext test24.tbl
if (mode = "nobatch" or mode = "inter")
    ush gnuplot test24.gpi
end-if
!
! TEST 25 - Noise parameter
! 
! test the NOISE parameter by adding noise to previous image:

otf1 s1edgeN.img plotout=test25 noise=2 'noprint 

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test25.gpi
end-if
!
! TEST 26 - NOISE=5 sigma
! 
otf1 s1edgeN.img plotout=test26 noise=5 'noprint 
if (mode = "nobatch" or mode = "inter")
    ush gnuplot test26.gpi
end-if
!
! TEST 27 - PZOOM
!
! test pzoom parameter 
otf1 inp=s2edge.img plotout=test27 pzoom=4 'noprint
if (mode = "nobatch" or mode = "inter")
    ush gnuplot test27.gpi
end-if

!
! TEST 28  - INTERVAL
!
otf1 inp=s1edge.img plotout=test28 'nonormal interval=0.5 table=test28.tbl
if (mode = "nobatch" or mode = "inter")
    ush gnuplot test28.gpi
end-if

!
! TEST 29 - TEST plotout=YES plotprof=YES
!
otf1 inp=s1edge.img plotout=yes plotprof=yes

if (mode = "nobatch" or mode = "inter")
    ush gnuplot plotprof.prof.gpi
    ush gnuplot otfplot.gpi
end-if
!
! TEST 30 - Look at the sharp step wedge a.img
!
otf1 inp=a.img plotout=test30 plotprof=test30
if (mode = "nobatch" or mode = "inter")
    ush gnuplot test30.prof.gpi
    ush gnuplot test30.gpi

end-if


! TEST 31 - return parameters
!
! Optics professionals often express pixel sizes in radians
! for specific satellites. OTF1 allows the user to output
! data in radians. Since point spread functions are normally
! circular then OTF allows the user to specify the rotation
! of the edge with respect to the top of the image (NORTH)
! THis is not true north. OTF1 does not access the image 
! coodinate system if provided in the header

otf1 inp=s2edge.img gsd=30. altitude=400000 psfpix=psfpix +
    pixrad=pixrad pixdeg=pixdeg psfrad=psfrad psfdeg=psfdeg +
    norangle=22.0 plotprof=test31

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test31.prof.gpi
end-if

let $echo="no"
write "s2edge.img  gsd = 30 meters   altitude = 400,000 meters"
write "          radians/pixel = &pixrad  degrees/pixel = &pixdeg"
write "   psf is &psfpix pixels  psf in radians = &psfrad  psf in degrees = &psfdeg"
let $echo="yes"

!
! TEST 32 - output plot in eps format
!
otf1 inp=s2edge.img gsd=30. altitude=400000 psfpix=psfpix +
    pixrad=pixrad pixdeg=pixdeg psfrad=psfrad psfdeg=psfdeg +
    norangle=22.0 plotout=test32 plotprof=test32 plotfmt=eps

ush gnuplot test32.eps.gpi
ush gnuplot test32.prof.eps.gpi
let $echo="no"
goto theend
!
!*********************************************************
! The following are steps that can be used for comparison
! to data produced in the previous tests
! To perform the following comment out the goto theend
! statement
!*********************************************************
!
!FFT 1-D of images - used for comparison
! s1edge.img is simple ramp
fft11 s1edge.img  xxA
ccomp xxA out=(xxAa,xxAp)           !ramp
maxmin xxAa minival=minval maxival=maxval
cform xxAa s1edgefft1az.img irange=(&minval,&maxval) +
    orange=(0,1.0) oform=real
lave s1edgefft1az.img xxAl 'vert
img2ascii xxAl s1edgefft1az.asc org=columns index=yes
typetext s1edgefft1az.asc
!
! s2edge.img is smoothed ramp
fft11 s2edge.img xxB
ccomp xxB out=(xxBa,xxBp)           !smoothed
maxmin xxBa minival=minval maxival=maxval
cform xxBa s2edgefft1az.img irange=(&minval,&maxval) +
    orange=(0,1.0) oform=real
lave s2edgefft1az.img xxBl
img2ascii xxBl s2edgefft1az.asc org=columns index=yes
typetext s2edgefft1az.asc
!
! s1edgeN.img is noisy ramp
fft11 s1edgeN.img xxC
ccomp xxC out=(xxCa,xxCp)           !noise on ramp
maxmin xxCa minival=minval maxival=maxval
cform xxCa s1edgeNfft1az.img irange=(&minval,&maxval) +
    orange=(0,1.0) oform=real
lave s1edgeNfft1az.img xxCl
img2ascii xxCl s1edgeNfft1az.asc org=columns index=yes
typetext s1edgeNfft1az.asc
!
! s2edgeN.img is noisy smoothed ramp
fft11 s2edgeN.img xxD
ccomp xxD out=(xxDa,xxDp)           !noise on smoothed ramp
maxmin xxDa minival=minval maxival=maxval
cform xxDa s2edgeNfft1az.img irange=(&minval,&maxval) +
    orange=(0,1.0) oform=real
lave s2edgeNfft1az.img xxDl
img2ascii xxDl s2edgeNfft1az.asc org=columns index=yes
typetext s2edgeNfft1az.asc

! Derivatives of esf (smoothed ramp)
otf1 inp=s2edge.img  imgpsf="yes" 'diag
! MAKE A DERIVATIVE FOR COMPARISON
deriv inp=s2edge.img out=(s2lsf.img,s2infl.img) orient=horiz
! esf of noisy smoothed ramp
otf1 inp=s2edgeN.img imgpsf="yes" 'diag

deriv inp=s2edgeN.img out=(s2lsfN.img,s2inflN.img) orient=horiz



theend>
 
disable-log
end-proc
$!-----------------------------------------------------------------------------
$ create tstotf1.log
gen out=bk.img nl=40 ns=40 linc=0.0 sinc=0.0 ival=0.0
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen out=wt.img nl=40 ns=40 linc=0.0 sinc=0.0 ival=218
Beginning VICAR task gen
GEN Version 6
GEN task completed
insect inp=(bk.img,wt.img) out=edge.img size=(1,1,40,40)  +
   insect=(1,1,40,20,1,21)
Beginning VICAR task insect
INSECT version 7 Jan 2013 (64-bit) - rjb
list edge.img (1,11,40,30)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Wed Jan 13 13:50:27 2016
 Task:INSECT    User:wlb       Date_Time:Wed Jan 13 13:50:27 2016
     Samp    11      13      15      17      19      21      23      25      27      29      31      33      35      37      39
   Line
      1       0   0   0   0   0   0   0   0   0   0 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218
      2       0   0   0   0   0   0   0   0   0   0 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218
      3       0   0   0   0   0   0   0   0   0   0 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218
      4       0   0   0   0   0   0   0   0   0   0 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218
      5       0   0   0   0   0   0   0   0   0   0 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218
      6       0   0   0   0   0   0   0   0   0   0 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218
      7       0   0   0   0   0   0   0   0   0   0 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218
      8       0   0   0   0   0   0   0   0   0   0 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218
      9       0   0   0   0   0   0   0   0   0   0 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218
     10       0   0   0   0   0   0   0   0   0   0 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218
     11       0   0   0   0   0   0   0   0   0   0 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218
     12       0   0   0   0   0   0   0   0   0   0 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218
     13       0   0   0   0   0   0   0   0   0   0 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218
     14       0   0   0   0   0   0   0   0   0   0 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218
     15       0   0   0   0   0   0   0   0   0   0 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218
     16       0   0   0   0   0   0   0   0   0   0 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218
     17       0   0   0   0   0   0   0   0   0   0 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218
     18       0   0   0   0   0   0   0   0   0   0 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218
     19       0   0   0   0   0   0   0   0   0   0 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218
     20       0   0   0   0   0   0   0   0   0   0 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218
     21       0   0   0   0   0   0   0   0   0   0 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218
     22       0   0   0   0   0   0   0   0   0   0 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218
     23       0   0   0   0   0   0   0   0   0   0 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218
     24       0   0   0   0   0   0   0   0   0   0 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218
     25       0   0   0   0   0   0   0   0   0   0 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218
     26       0   0   0   0   0   0   0   0   0   0 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218
     27       0   0   0   0   0   0   0   0   0   0 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218
     28       0   0   0   0   0   0   0   0   0   0 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218
     29       0   0   0   0   0   0   0   0   0   0 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218
     30       0   0   0   0   0   0   0   0   0   0 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218
     31       0   0   0   0   0   0   0   0   0   0 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218
     32       0   0   0   0   0   0   0   0   0   0 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218
     33       0   0   0   0   0   0   0   0   0   0 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218
     34       0   0   0   0   0   0   0   0   0   0 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218
     35       0   0   0   0   0   0   0   0   0   0 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218
     36       0   0   0   0   0   0   0   0   0   0 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218
     37       0   0   0   0   0   0   0   0   0   0 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218
     38       0   0   0   0   0   0   0   0   0   0 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218
     39       0   0   0   0   0   0   0   0   0   0 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218
     40       0   0   0   0   0   0   0   0   0   0 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218 218
genthis a.img nl=1 ns=16  +
   dn=(0,0,0,0,24,48,72,96,120,144,168,192,216,216,216,216)
Beginning VICAR task genthis
 GENTHIS VERSION 2
 GENTHIS TASK COMPLETED
size a.img s1edge.img 'noin lzoom=20 szoom=1
Beginning VICAR task size
 SIZE version 22 Aug 2013 (64-bit) - rjb
      INPUT AREA=(    1,    1,    1,   16)
     OUTPUT SIZE=     20 X     16
 PICTURE SIZE SCALED BY     20*NL,      1*NS
 SIZE task completed
list s1edge.img
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GENTHIS   User:wlb       Date_Time:Wed Jan 13 13:50:27 2016
 Task:SIZE      User:wlb       Date_Time:Wed Jan 13 13:50:27 2016
     Samp     1       3       5       7       9      11      13      15
   Line
      1       0   0   0   0  24  48  72  96 120 144 168 192 216 216 216 216
      2       0   0   0   0  24  48  72  96 120 144 168 192 216 216 216 216
      3       0   0   0   0  24  48  72  96 120 144 168 192 216 216 216 216
      4       0   0   0   0  24  48  72  96 120 144 168 192 216 216 216 216
      5       0   0   0   0  24  48  72  96 120 144 168 192 216 216 216 216
      6       0   0   0   0  24  48  72  96 120 144 168 192 216 216 216 216
      7       0   0   0   0  24  48  72  96 120 144 168 192 216 216 216 216
      8       0   0   0   0  24  48  72  96 120 144 168 192 216 216 216 216
      9       0   0   0   0  24  48  72  96 120 144 168 192 216 216 216 216
     10       0   0   0   0  24  48  72  96 120 144 168 192 216 216 216 216
     11       0   0   0   0  24  48  72  96 120 144 168 192 216 216 216 216
     12       0   0   0   0  24  48  72  96 120 144 168 192 216 216 216 216
     13       0   0   0   0  24  48  72  96 120 144 168 192 216 216 216 216
     14       0   0   0   0  24  48  72  96 120 144 168 192 216 216 216 216
     15       0   0   0   0  24  48  72  96 120 144 168 192 216 216 216 216
     16       0   0   0   0  24  48  72  96 120 144 168 192 216 216 216 216
     17       0   0   0   0  24  48  72  96 120 144 168 192 216 216 216 216
     18       0   0   0   0  24  48  72  96 120 144 168 192 216 216 216 216
     19       0   0   0   0  24  48  72  96 120 144 168 192 216 216 216 216
     20       0   0   0   0  24  48  72  96 120 144 168 192 216 216 216 216
boxflt2 inp=s1edge.img out=s2edge.img nlw=9 nsw=9
Beginning VICAR task boxflt2
BOXFLT2  02-May-2011 (64-bit) RJB
list s2edge.img
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GENTHIS   User:wlb       Date_Time:Wed Jan 13 13:50:27 2016
 Task:BOXFLT2   User:wlb       Date_Time:Wed Jan 13 13:50:27 2016
     Samp     1       3       5       7       9      11      13      15
   Line
      1       5   8  16  26  40  56  74  96 120 141 160 176 189 200 208 210
      2       5   8  16  26  40  56  74  96 120 141 160 176 189 200 208 210
      3       5   8  16  26  40  56  74  96 120 141 160 176 189 200 208 210
      4       5   8  16  26  40  56  74  96 120 141 160 176 189 200 208 210
      5       5   8  16  26  40  56  74  96 120 141 160 176 189 200 208 210
      6       5   8  16  26  40  56  74  96 120 141 160 176 189 200 208 210
      7       5   8  16  26  40  56  74  96 120 141 160 176 189 200 208 210
      8       5   8  16  26  40  56  74  96 120 141 160 176 189 200 208 210
      9       5   8  16  26  40  56  74  96 120 141 160 176 189 200 208 210
     10       5   8  16  26  40  56  74  96 120 141 160 176 189 200 208 210
     11       5   8  16  26  40  56  74  96 120 141 160 176 189 200 208 210
     12       5   8  16  26  40  56  74  96 120 141 160 176 189 200 208 210
     13       5   8  16  26  40  56  74  96 120 141 160 176 189 200 208 210
     14       5   8  16  26  40  56  74  96 120 141 160 176 189 200 208 210
     15       5   8  16  26  40  56  74  96 120 141 160 176 189 200 208 210
     16       5   8  16  26  40  56  74  96 120 141 160 176 189 200 208 210
     17       5   8  16  26  40  56  74  96 120 141 160 176 189 200 208 210
     18       5   8  16  26  40  56  74  96 120 141 160 176 189 200 208 210
     19       5   8  16  26  40  56  74  96 120 141 160 176 189 200 208 210
     20       5   8  16  26  40  56  74  96 120 141 160 176 189 200 208 210
addnoise s1edge.img s1edgeN.img sigma=3 seed=1351456636
Beginning VICAR task addnoise
ADDNOISE - 18-JUN-2012
Input format is BYTE
RANGEN::: Random Seed value of 1351456636 adjusted to 9279356
addnoise s2edge.img s2edgeN.img sigma=3 seed=1351456636
Beginning VICAR task addnoise
ADDNOISE - 18-JUN-2012
Input format is BYTE
RANGEN::: Random Seed value of 1351456636 adjusted to 9279356
genthis b.img nl=1 ns=8  +
    dn=(0,0,24,72,120,168,216,216)
Beginning VICAR task genthis
 GENTHIS VERSION 2
 GENTHIS TASK COMPLETED
size b.img t1edge.img 'noin lzoom=20 szoom=1
Beginning VICAR task size
 SIZE version 22 Aug 2013 (64-bit) - rjb
      INPUT AREA=(    1,    1,    1,    8)
     OUTPUT SIZE=     20 X      8
 PICTURE SIZE SCALED BY     20*NL,      1*NS
 SIZE task completed
list t1edge.img
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GENTHIS   User:wlb       Date_Time:Wed Jan 13 13:50:27 2016
 Task:SIZE      User:wlb       Date_Time:Wed Jan 13 13:50:27 2016
     Samp     1       3       5       7
   Line
      1       0   0  24  72 120 168 216 216
      2       0   0  24  72 120 168 216 216
      3       0   0  24  72 120 168 216 216
      4       0   0  24  72 120 168 216 216
      5       0   0  24  72 120 168 216 216
      6       0   0  24  72 120 168 216 216
      7       0   0  24  72 120 168 216 216
      8       0   0  24  72 120 168 216 216
      9       0   0  24  72 120 168 216 216
     10       0   0  24  72 120 168 216 216
     11       0   0  24  72 120 168 216 216
     12       0   0  24  72 120 168 216 216
     13       0   0  24  72 120 168 216 216
     14       0   0  24  72 120 168 216 216
     15       0   0  24  72 120 168 216 216
     16       0   0  24  72 120 168 216 216
     17       0   0  24  72 120 168 216 216
     18       0   0  24  72 120 168 216 216
     19       0   0  24  72 120 168 216 216
     20       0   0  24  72 120 168 216 216
boxflt2 inp=t1edge.img out=t2edge.img nlw=5 nsw=5
Beginning VICAR task boxflt2
BOXFLT2  02-May-2011 (64-bit) RJB
list t2edge.img
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GENTHIS   User:wlb       Date_Time:Wed Jan 13 13:50:27 2016
 Task:BOXFLT2   User:wlb       Date_Time:Wed Jan 13 13:50:27 2016
     Samp     1       3       5       7
   Line
      1       9  19  43  76 120 158 187 196
      2       9  19  43  76 120 158 187 196
      3       9  19  43  76 120 158 187 196
      4       9  19  43  76 120 158 187 196
      5       9  19  43  76 120 158 187 196
      6       9  19  43  76 120 158 187 196
      7       9  19  43  76 120 158 187 196
      8       9  19  43  76 120 158 187 196
      9       9  19  43  76 120 158 187 196
     10       9  19  43  76 120 158 187 196
     11       9  19  43  76 120 158 187 196
     12       9  19  43  76 120 158 187 196
     13       9  19  43  76 120 158 187 196
     14       9  19  43  76 120 158 187 196
     15       9  19  43  76 120 158 187 196
     16       9  19  43  76 120 158 187 196
     17       9  19  43  76 120 158 187 196
     18       9  19  43  76 120 158 187 196
     19       9  19  43  76 120 158 187 196
     20       9  19  43  76 120 158 187 196
addnoise t1edge.img t1edgeN.img sigma=3 seed=1351456636
Beginning VICAR task addnoise
ADDNOISE - 18-JUN-2012
Input format is BYTE
RANGEN::: Random Seed value of 1351456636 adjusted to 9279356
addnoise t2edge.img t2edgeN.img sigma=3 seed=1351456636
Beginning VICAR task addnoise
ADDNOISE - 18-JUN-2012
Input format is BYTE
RANGEN::: Random Seed value of 1351456636 adjusted to 9279356
genthis c.img nl=1 ns=16  +
    dn=(0,0,0,0,0,0,60,96,108,170,216,216,216,216,216,216)
Beginning VICAR task genthis
 GENTHIS VERSION 2
 GENTHIS TASK COMPLETED
size c.img u1edge.img 'noin lzoom=20 szoom=1
Beginning VICAR task size
 SIZE version 22 Aug 2013 (64-bit) - rjb
      INPUT AREA=(    1,    1,    1,   16)
     OUTPUT SIZE=     20 X     16
 PICTURE SIZE SCALED BY     20*NL,      1*NS
 SIZE task completed
list u1edge.img
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GENTHIS   User:wlb       Date_Time:Wed Jan 13 13:50:28 2016
 Task:SIZE      User:wlb       Date_Time:Wed Jan 13 13:50:28 2016
     Samp     1       3       5       7       9      11      13      15
   Line
      1       0   0   0   0   0   0  60  96 108 170 216 216 216 216 216 216
      2       0   0   0   0   0   0  60  96 108 170 216 216 216 216 216 216
      3       0   0   0   0   0   0  60  96 108 170 216 216 216 216 216 216
      4       0   0   0   0   0   0  60  96 108 170 216 216 216 216 216 216
      5       0   0   0   0   0   0  60  96 108 170 216 216 216 216 216 216
      6       0   0   0   0   0   0  60  96 108 170 216 216 216 216 216 216
      7       0   0   0   0   0   0  60  96 108 170 216 216 216 216 216 216
      8       0   0   0   0   0   0  60  96 108 170 216 216 216 216 216 216
      9       0   0   0   0   0   0  60  96 108 170 216 216 216 216 216 216
     10       0   0   0   0   0   0  60  96 108 170 216 216 216 216 216 216
     11       0   0   0   0   0   0  60  96 108 170 216 216 216 216 216 216
     12       0   0   0   0   0   0  60  96 108 170 216 216 216 216 216 216
     13       0   0   0   0   0   0  60  96 108 170 216 216 216 216 216 216
     14       0   0   0   0   0   0  60  96 108 170 216 216 216 216 216 216
     15       0   0   0   0   0   0  60  96 108 170 216 216 216 216 216 216
     16       0   0   0   0   0   0  60  96 108 170 216 216 216 216 216 216
     17       0   0   0   0   0   0  60  96 108 170 216 216 216 216 216 216
     18       0   0   0   0   0   0  60  96 108 170 216 216 216 216 216 216
     19       0   0   0   0   0   0  60  96 108 170 216 216 216 216 216 216
     20       0   0   0   0   0   0  60  96 108 170 216 216 216 216 216 216
boxflt2 inp=u1edge.img out=u2edge.img nlw=5 nsw=5
Beginning VICAR task boxflt2
BOXFLT2  02-May-2011 (64-bit) RJB
list u2edge.img
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GENTHIS   User:wlb       Date_Time:Wed Jan 13 13:50:28 2016
 Task:BOXFLT2   User:wlb       Date_Time:Wed Jan 13 13:50:28 2016
     Samp     1       3       5       7       9      11      13      15
   Line
      1       0   0   0   0  12  31  52  86 130 161 185 206 216 216 216 216
      2       0   0   0   0  12  31  52  86 130 161 185 206 216 216 216 216
      3       0   0   0   0  12  31  52  86 130 161 185 206 216 216 216 216
      4       0   0   0   0  12  31  52  86 130 161 185 206 216 216 216 216
      5       0   0   0   0  12  31  52  86 130 161 185 206 216 216 216 216
      6       0   0   0   0  12  31  52  86 130 161 185 206 216 216 216 216
      7       0   0   0   0  12  31  52  86 130 161 185 206 216 216 216 216
      8       0   0   0   0  12  31  52  86 130 161 185 206 216 216 216 216
      9       0   0   0   0  12  31  52  86 130 161 185 206 216 216 216 216
     10       0   0   0   0  12  31  52  86 130 161 185 206 216 216 216 216
     11       0   0   0   0  12  31  52  86 130 161 185 206 216 216 216 216
     12       0   0   0   0  12  31  52  86 130 161 185 206 216 216 216 216
     13       0   0   0   0  12  31  52  86 130 161 185 206 216 216 216 216
     14       0   0   0   0  12  31  52  86 130 161 185 206 216 216 216 216
     15       0   0   0   0  12  31  52  86 130 161 185 206 216 216 216 216
     16       0   0   0   0  12  31  52  86 130 161 185 206 216 216 216 216
     17       0   0   0   0  12  31  52  86 130 161 185 206 216 216 216 216
     18       0   0   0   0  12  31  52  86 130 161 185 206 216 216 216 216
     19       0   0   0   0  12  31  52  86 130 161 185 206 216 216 216 216
     20       0   0   0   0  12  31  52  86 130 161 185 206 216 216 216 216
addnoise u1edge.img u1edgeN.img sigma=3 seed=1351456636
Beginning VICAR task addnoise
ADDNOISE - 18-JUN-2012
Input format is BYTE
RANGEN::: Random Seed value of 1351456636 adjusted to 9279356
addnoise u2edge.img u2edgeN.img sigma=3 seed=1351456636
Beginning VICAR task addnoise
ADDNOISE - 18-JUN-2012
Input format is BYTE
RANGEN::: Random Seed value of 1351456636 adjusted to 9279356
otf1 'sinctst plotout=sinctst
Beginning VICAR task otf1
OTF1 version 2015-08-10

 PIXEL  CYCLES/      FREQUENCY    REAL(FT)     IMAG.(FT)     INTENSITY    AMPLITUDE     PHASE     WAVE LENGT
         PIXEL                                                                         RADIANS      SHIFT
  1     0.00000       0.00000      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
  2     0.00391       0.00391      0.99773      0.00000      0.99546      0.99773      0.00000      0.00000
  3     0.00781       0.00781      0.99130     -0.00000      0.98267      0.99130      0.00000      0.00000
  4     0.01172       0.01172      0.98178      0.00000      0.96389      0.98178      0.00000      0.00000
  5     0.01563       0.01563      0.97058      0.00000      0.94202      0.97058      0.00000      0.00000
  6     0.01953       0.01953      0.95903      0.00000      0.91974      0.95903      0.00000      0.00000
  7     0.02344       0.02344      0.94808      0.00000      0.89886      0.94808      0.00000      0.00000
  8     0.02734       0.02734      0.93809      0.00000      0.88001      0.93809      0.00000      0.00000
  9     0.03125       0.03125      0.92888     -0.00000      0.86281      0.92888      0.00000      0.00000
 10     0.03516       0.03516      0.91997      0.00000      0.84634      0.91997      0.00000      0.00000
 11     0.03906       0.03906      0.91084     -0.00000      0.82963      0.91084      0.00000      0.00000
 12     0.04297       0.04297      0.90119      0.00000      0.81214      0.90119      0.00000      0.00000
 13     0.04688       0.04688      0.89099     -0.00000      0.79387      0.89099      0.00000      0.00000
 14     0.05078       0.05078      0.88052      0.00000      0.77531      0.88052      0.00000      0.00000
 15     0.05469       0.05469      0.87010      0.00000      0.75708      0.87010      0.00000      0.00000
 16     0.05859       0.05859      0.86002      0.00000      0.73964      0.86002      0.00000      0.00000
 17     0.06250       0.06250      0.85035     -0.00000      0.72310      0.85035      0.00000      0.00000
 18     0.06641       0.06641      0.84095      0.00000      0.70720      0.84095      0.00000      0.00000
 19     0.07031       0.07031      0.83157      0.00000      0.69151      0.83157      0.00000      0.00000
 20     0.07422       0.07422      0.82198      0.00000      0.67565      0.82198      0.00000      0.00000
 21     0.07813       0.07813      0.81206      0.00000      0.65944      0.81206      0.00000      0.00000
 22     0.08203       0.08203      0.80189      0.00000      0.64302      0.80189      0.00000      0.00000
 23     0.08594       0.08594      0.79164      0.00000      0.62669      0.79164      0.00000      0.00000
 24     0.08984       0.08984      0.78153      0.00000      0.61078      0.78153      0.00000      0.00000
 25     0.09375       0.09375      0.77167     -0.00000      0.59547      0.77167      0.00000      0.00000
 26     0.09766       0.09766      0.76205      0.00000      0.58072      0.76205      0.00000      0.00000
 27     0.10156       0.10156      0.75254      0.00000      0.56632      0.75254      0.00000      0.00000
 28     0.10547       0.10547      0.74296      0.00000      0.55199      0.74296      0.00000      0.00000
 29     0.10938       0.10938      0.73318      0.00000      0.53755      0.73318      0.00000      0.00000
 30     0.11328       0.11328      0.72318      0.00000      0.52298      0.72318      0.00000      0.00000
 31     0.11719       0.11719      0.71304      0.00000      0.50843      0.71304      0.00000      0.00000
 32     0.12109       0.12109      0.70292      0.00000      0.49410      0.70292      0.00000      0.00000
 33     0.12500       0.12500      0.69296      0.00000      0.48020      0.69296      0.00000      0.00000
 34     0.12891       0.12891      0.68321      0.00000      0.46677      0.68321      0.00000      0.00000
 35     0.13281       0.13281      0.67360      0.00000      0.45373      0.67360      0.00000      0.00000
 36     0.13672       0.13672      0.66400      0.00000      0.44090      0.66400      0.00000      0.00000
 37     0.14063       0.14063      0.65430      0.00000      0.42811      0.65430      0.00000      0.00000
 38     0.14453       0.14453      0.64442      0.00000      0.41527      0.64442      0.00000      0.00000
 39     0.14844       0.14844      0.63438      0.00000      0.40243      0.63438      0.00000      0.00000
 40     0.15234       0.15234      0.62428      0.00000      0.38973      0.62428      0.00000      0.00000
 41     0.15625       0.15625      0.61426     -0.00000      0.37732      0.61426      0.00000      0.00000
 42     0.16016       0.16016      0.60440      0.00000      0.36530      0.60440      0.00000      0.00000
 43     0.16406       0.16406      0.59470      0.00000      0.35367      0.59470      0.00000      0.00000
 44     0.16797       0.16797      0.58508      0.00000      0.34232      0.58508      0.00000      0.00000
 45     0.17188       0.17188      0.57542      0.00000      0.33111      0.57542      0.00000      0.00000
 46     0.17578       0.17578      0.56562      0.00000      0.31993      0.56562      0.00000      0.00000
 47     0.17969       0.17969      0.55567      0.00000      0.30877      0.55567      0.00000      0.00000
 48     0.18359       0.18359      0.54562      0.00000      0.29770      0.54562      0.00000      0.00000
 49     0.18750       0.18750      0.53557      0.00000      0.28684      0.53557      0.00000      0.00000
 50     0.19141       0.19141      0.52563      0.00000      0.27629      0.52563      0.00000      0.00000
 51     0.19531       0.19531      0.51585      0.00000      0.26610      0.51585      0.00000      0.00000
 52     0.19922       0.19922      0.50618      0.00000      0.25622      0.50618      0.00000      0.00000
 53     0.20313       0.20313      0.49653      0.00000      0.24654      0.49653      0.00000      0.00000
 54     0.20703       0.20703      0.48680      0.00000      0.23698      0.48680      0.00000      0.00000
 55     0.21094       0.21094      0.47693      0.00000      0.22746      0.47693      0.00000      0.00000
 56     0.21484       0.21484      0.46693      0.00000      0.21802      0.46693      0.00000      0.00000
 57     0.21875       0.21875      0.45688      0.00000      0.20874      0.45688      0.00000      0.00000
 58     0.22266       0.22266      0.44688      0.00000      0.19971      0.44688      0.00000      0.00000
 59     0.22656       0.22656      0.43702      0.00000      0.19099      0.43702      0.00000      0.00000
 60     0.23047       0.23047      0.42729      0.00000      0.18258      0.42729      0.00000      0.00000
 61     0.23438       0.23438      0.41764      0.00000      0.17442      0.41764      0.00000      0.00000
 62     0.23828       0.23828      0.40796      0.00000      0.16643      0.40796      0.00000      0.00000
 63     0.24219       0.24219      0.39816      0.00000      0.15853      0.39816      0.00000      0.00000
 64     0.24609       0.24609      0.38822      0.00000      0.15072      0.38822      0.00000      0.00000
 65     0.25000       0.25000      0.37819      0.00000      0.14303      0.37819      0.00000      0.00000
 66     0.25391       0.25391      0.36816     -0.00000      0.13554      0.36816      0.00000      0.00000
 67     0.25781       0.25781      0.35822      0.00000      0.12832      0.35822      0.00000      0.00000
 68     0.26172       0.26172      0.34842      0.00000      0.12140      0.34842      0.00000      0.00000
 69     0.26563       0.26563      0.33875      0.00000      0.11475      0.33875      0.00000      0.00000
 70     0.26953       0.26953      0.32910     -0.00000      0.10831      0.32910      0.00000      0.00000
 71     0.27344       0.27344      0.31938      0.00000      0.10200      0.31938      0.00000      0.00000
 72     0.27734       0.27734      0.30951      0.00000      0.09580      0.30951      0.00000      0.00000
 73     0.28125       0.28125      0.29951      0.00000      0.08970      0.29951      0.00000      0.00000
 74     0.28516       0.28516      0.28944      0.00000      0.08378      0.28944      0.00000      0.00000
 75     0.28906       0.28906      0.27944      0.00000      0.07808      0.27944      0.00000      0.00000
 76     0.29297       0.29297      0.26956      0.00000      0.07267      0.26956      0.00000      0.00000
 77     0.29688       0.29688      0.25985      0.00000      0.06752      0.25985      0.00000      0.00000
 78     0.30078       0.30078      0.25022     -0.00000      0.06261      0.25022      0.00000      0.00000
 79     0.30469       0.30469      0.24057      0.00000      0.05787      0.24057      0.00000      0.00000
 80     0.30859       0.30859      0.23079      0.00000      0.05326      0.23079      0.00000      0.00000
 81     0.31250       0.31250      0.22083      0.00000      0.04877      0.22083      0.00000      0.00000
 82     0.31641       0.31641      0.21075     -0.00000      0.04442      0.21075      0.00000      0.00000
 83     0.32031       0.32031      0.20067      0.00000      0.04027      0.20067      0.00000      0.00000
 84     0.32422       0.32422      0.19070     -0.00000      0.03637      0.19070      0.00000      0.00000
 85     0.32813       0.32813      0.18092      0.00000      0.03273      0.18092      0.00000      0.00000
 86     0.33203       0.33203      0.17131     -0.00000      0.02935      0.17131      0.00000      0.00000
 87     0.33594       0.33594      0.16175      0.00000      0.02616      0.16175      0.00000      0.00000
 88     0.33984       0.33984      0.15209     -0.00000      0.02313      0.15209      0.00000      0.00000
 89     0.34375       0.34375      0.14221      0.00000      0.02022      0.14221      0.00000      0.00000
 90     0.34766       0.34766      0.13211      0.00000      0.01745      0.13211      0.00000      0.00000
 91     0.35156       0.35156      0.12190      0.00000      0.01486      0.12190      0.00000      0.00000
 92     0.35547       0.35547      0.11177     -0.00000      0.01249      0.11177      0.00000      0.00000
 93     0.35938       0.35938      0.10189      0.00000      0.01038      0.10189      0.00000      0.00000
 94     0.36328       0.36328      0.09233     -0.00000      0.00852      0.09233      0.00000      0.00000
 95     0.36719       0.36719      0.08297      0.00000      0.00688      0.08297      0.00000      0.00000
 96     0.37109       0.37109      0.07356     -0.00000      0.00541      0.07356      0.00000      0.00000
 97     0.37500       0.37500      0.06383      0.00000      0.00407      0.06383      0.00000      0.00000
 98     0.37891       0.37891      0.05361     -0.00000      0.00287      0.05361      0.00000      0.00000
 99     0.38281       0.38281      0.04297      0.00000      0.00185      0.04297      0.00000      0.00000
100     0.38672       0.38672      0.03227     -0.00000      0.00104      0.03227      0.00000      0.00000
101     0.39063       0.39063      0.02215      0.00000      0.00049      0.02215      0.00000      0.00000
102     0.39453       0.39453      0.01332     -0.00000      0.00018      0.01332      0.00000      0.00000
103     0.39844       0.39844      0.00640      0.00000      0.00004      0.00640      0.00000      0.00000
104     0.40234       0.40234      0.00172     -0.00000      0.00000      0.00172      0.00000      0.00000
105     0.40625       0.40625     -0.00076      0.00000      0.00000      0.00076      3.14159      1.23077
106     0.41016       0.41016     -0.00146     -0.00000      0.00000      0.00146      3.14159      1.21905
107     0.41406       0.41406     -0.00104      0.00000      0.00000      0.00104      3.14159      1.20755
108     0.41797       0.41797     -0.00020     -0.00000      0.00000      0.00020      0.00000      0.00000
109     0.42188       0.42188      0.00050      0.00000      0.00000      0.00050      0.00000      0.00000
110     0.42578       0.42578      0.00076      0.00000      0.00000      0.00076      0.00000      0.00000
111     0.42969       0.42969      0.00057      0.00000      0.00000      0.00057      0.00000      0.00000
112     0.43359       0.43359      0.00013     -0.00000      0.00000      0.00013      0.00000      0.00000
113     0.43750       0.43750     -0.00029      0.00000      0.00000      0.00029      0.00000      0.00000
114     0.44141       0.44141     -0.00050      0.00000      0.00000      0.00050      3.14159      1.13274
115     0.44531       0.44531     -0.00042      0.00000      0.00000      0.00042      0.00000      0.00000
116     0.44922       0.44922     -0.00014     -0.00000      0.00000      0.00014      0.00000      0.00000
117     0.45313       0.45313      0.00018      0.00000      0.00000      0.00018      0.00000      0.00000
118     0.45703       0.45703      0.00037      0.00000      0.00000      0.00037      0.00000      0.00000
119     0.46094       0.46094      0.00036     -0.00000      0.00000      0.00036      0.00000      0.00000
120     0.46484       0.46484      0.00016     -0.00000      0.00000      0.00016      0.00000      0.00000
121     0.46875       0.46875     -0.00010     -0.00000      0.00000      0.00010      0.00000      0.00000
122     0.47266       0.47266     -0.00030      0.00000      0.00000      0.00030      0.00000      0.00000
123     0.47656       0.47656     -0.00033      0.00000      0.00000      0.00033      0.00000      0.00000
124     0.48047       0.48047     -0.00018     -0.00000      0.00000      0.00018      0.00000      0.00000
125     0.48438       0.48438      0.00005      0.00000      0.00000      0.00005      0.00000      0.00000
126     0.48828       0.48828      0.00025      0.00000      0.00000      0.00025      0.00000      0.00000
127     0.49219       0.49219      0.00031     -0.00000      0.00000      0.00031      0.00000      0.00000
128     0.49609       0.49609      0.00021     -0.00000      0.00000      0.00021      0.00000      0.00000
129     0.50000       0.50000      0.00005      0.00000      0.00000      0.00005      0.00000      0.00000
NUMBER POINTS OUTPUT =         129
**** This image is ALIASED ****
INTEGRATED MTF AMPLITUDE FROM .25 TO .48 =       7.46190
if (mode = "nobatch" or mode = "inter")
end-if
otf1 'diag 'sinctst plotout=sinctst2 table=sinctst2.tbl
Beginning VICAR task otf1
OTF1 version 2015-08-10
NORMALIZING LSF
SUM OF LSF=  2.4788973
SUM1=  29.499998
SUM=  1.0000000
CENTR=  29.499998
AT SAMPLING THEORM R(I)=
            1.014E-04  2.845E-04  2.338E-15  3.290E-04  1.357E-04  1.470E-04  4.184E-04  2.583E-18  4.999E-04  2.100E-04
            2.321E-04  6.751E-04  1.469E-15  8.487E-04  3.674E-04  4.198E-04  1.268E-03  7.461E-16  1.747E-03  8.005E-04
            9.779E-04  3.198E-03  2.583E-18  5.469E-03  2.918E-03  4.358E-03  1.886E-02  3.124E-16  1.027E-01  3.530E-01
            3.530E-01  1.027E-01  3.124E-16  1.886E-02  4.358E-03  2.918E-03  5.469E-03  2.583E-18  3.198E-03  9.779E-04
            8.005E-04  1.747E-03  7.461E-16  1.268E-03  4.198E-04  3.674E-04  8.487E-04  1.469E-15  6.751E-04  2.321E-04
            2.100E-04  4.999E-04  2.583E-18  4.184E-04  1.470E-04  1.357E-04  3.290E-04  2.338E-15  2.845E-04  1.014E-04
            0.000E+00  0.000E+00  0.000E+00  0.000E+00
FRACTIONAL OFFSET CEN=  160.50000
SUM1 OF WTS=  0.000E+00
WTS(I)=
           -1.996E-03  2.008E-03 -2.021E-03  2.034E-03 -2.047E-03  2.060E-03 -2.074E-03  2.087E-03 -2.101E-03  2.115E-03
           -2.129E-03  2.144E-03 -2.158E-03  2.173E-03 -2.188E-03  2.203E-03 -2.218E-03  2.234E-03 -2.250E-03  2.266E-03
           -2.282E-03  2.298E-03 -2.315E-03  2.332E-03 -2.349E-03  2.367E-03 -2.384E-03  2.402E-03 -2.421E-03  2.439E-03
           -2.458E-03  2.477E-03 -2.497E-03  2.516E-03 -2.536E-03  2.557E-03 -2.577E-03  2.598E-03 -2.620E-03  2.642E-03
           -2.664E-03  2.686E-03 -2.709E-03  2.732E-03 -2.756E-03  2.780E-03 -2.804E-03  2.829E-03 -2.855E-03  2.881E-03
           -2.907E-03  2.934E-03 -2.961E-03  2.989E-03 -3.017E-03  3.046E-03 -3.075E-03  3.105E-03 -3.136E-03  3.167E-03
           -3.199E-03  3.232E-03 -3.265E-03  3.299E-03 -3.333E-03  3.368E-03 -3.404E-03  3.441E-03 -3.479E-03  3.517E-03
           -3.557E-03  3.597E-03 -3.638E-03  3.680E-03 -3.723E-03  3.767E-03 -3.812E-03  3.858E-03 -3.906E-03  3.954E-03
           -4.004E-03  4.055E-03 -4.107E-03  4.161E-03 -4.216E-03  4.273E-03 -4.331E-03  4.390E-03 -4.452E-03  4.515E-03
           -4.580E-03  4.647E-03 -4.716E-03  4.787E-03 -4.860E-03  4.935E-03 -5.013E-03  5.093E-03 -5.176E-03  5.261E-03
           -5.350E-03  5.441E-03 -5.536E-03  5.634E-03 -5.735E-03  5.841E-03 -5.950E-03  6.063E-03 -6.181E-03  6.303E-03
           -6.431E-03  6.563E-03 -6.701E-03  6.845E-03 -6.996E-03  7.153E-03 -7.317E-03  7.490E-03 -7.670E-03  7.860E-03
           -8.058E-03  8.268E-03 -8.488E-03  8.721E-03 -8.966E-03  9.226E-03 -9.502E-03  9.794E-03 -1.011E-02  1.044E-02
           -1.079E-02  1.117E-02 -1.157E-02  1.201E-02 -1.248E-02  1.299E-02 -1.355E-02  1.415E-02 -1.481E-02  1.553E-02
           -1.632E-02  1.721E-02 -1.819E-02  1.929E-02 -2.054E-02  2.195E-02 -2.358E-02  2.546E-02 -2.768E-02  3.032E-02
           -3.351E-02  3.745E-02 -4.244E-02  4.897E-02 -5.787E-02  7.074E-02 -9.095E-02  1.273E-01 -2.122E-01  6.366E-01
            6.366E-01 -2.122E-01  1.273E-01 -9.095E-02  7.074E-02 -5.787E-02  4.897E-02 -4.244E-02  3.745E-02 -3.351E-02
            3.032E-02 -2.768E-02  2.546E-02 -2.358E-02  2.195E-02 -2.054E-02  1.929E-02 -1.819E-02  1.721E-02 -1.632E-02
            1.553E-02 -1.481E-02  1.415E-02 -1.355E-02  1.299E-02 -1.248E-02  1.201E-02 -1.157E-02  1.117E-02 -1.079E-02
            1.044E-02 -1.011E-02  9.794E-03 -9.502E-03  9.226E-03 -8.966E-03  8.721E-03 -8.488E-03  8.268E-03 -8.058E-03
            7.860E-03 -7.670E-03  7.490E-03 -7.317E-03  7.153E-03 -6.996E-03  6.845E-03 -6.701E-03  6.563E-03 -6.431E-03
            6.303E-03 -6.181E-03  6.063E-03 -5.950E-03  5.841E-03 -5.735E-03  5.634E-03 -5.536E-03  5.441E-03 -5.350E-03
            5.261E-03 -5.176E-03  5.093E-03 -5.013E-03  4.935E-03 -4.860E-03  4.787E-03 -4.716E-03  4.647E-03 -4.580E-03
            4.515E-03 -4.452E-03  4.390E-03 -4.331E-03  4.273E-03 -4.216E-03  4.161E-03 -4.107E-03  4.055E-03 -4.004E-03
            3.954E-03 -3.906E-03  3.858E-03 -3.812E-03  3.767E-03 -3.723E-03  3.680E-03 -3.638E-03  3.597E-03 -3.557E-03
            3.517E-03 -3.479E-03  3.441E-03 -3.404E-03  3.368E-03 -3.333E-03  3.299E-03 -3.265E-03  3.232E-03 -3.199E-03
            3.167E-03 -3.136E-03  3.105E-03 -3.075E-03  3.046E-03 -3.017E-03  2.989E-03 -2.961E-03  2.934E-03 -2.907E-03
            2.881E-03 -2.855E-03  2.829E-03 -2.804E-03  2.780E-03 -2.756E-03  2.732E-03 -2.709E-03  2.686E-03 -2.664E-03
            2.642E-03 -2.620E-03  2.598E-03 -2.577E-03  2.557E-03 -2.536E-03  2.516E-03 -2.497E-03  2.477E-03 -2.458E-03
            2.439E-03 -2.421E-03  2.402E-03 -2.384E-03  2.367E-03 -2.349E-03  2.332E-03 -2.315E-03  2.298E-03 -2.282E-03
            2.266E-03 -2.250E-03  2.234E-03 -2.218E-03  2.203E-03 -2.188E-03  2.173E-03 -2.158E-03  2.144E-03 -2.129E-03
            2.115E-03 -2.101E-03  2.087E-03 -2.074E-03  2.060E-03 -2.047E-03  2.034E-03 -2.021E-03  2.008E-03 -1.996E-03
INT F(I)=
           -1.880E-07  1.911E-07 -1.943E-07  1.976E-07 -2.008E-07  2.043E-07 -2.078E-07  2.114E-07 -2.151E-07  2.190E-07
           -2.228E-07  2.268E-07 -2.310E-07  2.353E-07 -2.395E-07  2.441E-07 -2.487E-07  2.535E-07 -2.583E-07  2.634E-07
           -2.687E-07  2.739E-07 -2.796E-07  2.852E-07 -2.910E-07  2.971E-07 -3.035E-07  3.099E-07 -3.165E-07  3.235E-07
           -3.306E-07  3.380E-07 -3.456E-07  3.536E-07 -3.618E-07  3.702E-07 -3.792E-07  3.883E-07 -3.977E-07  4.077E-07
           -4.179E-07  4.286E-07 -4.395E-07  4.512E-07 -4.632E-07  4.756E-07 -4.887E-07  5.024E-07 -5.166E-07  5.314E-07
           -5.470E-07  5.632E-07 -5.805E-07  5.980E-07 -6.171E-07  6.366E-07 -6.573E-07  6.793E-07 -7.022E-07  7.264E-07
           -7.519E-07  7.790E-07 -8.076E-07  8.377E-07 -8.702E-07  9.042E-07 -9.406E-07  9.795E-07 -1.021E-06  1.065E-06
           -1.113E-06  1.164E-06 -1.219E-06  1.278E-06 -1.342E-06  1.412E-06 -1.488E-06  1.570E-06 -1.661E-06  1.760E-06
           -1.870E-06  1.991E-06 -2.127E-06  2.278E-06 -2.448E-06  2.640E-06 -2.860E-06  3.112E-06 -3.405E-06  3.747E-06
           -4.153E-06  4.640E-06 -5.232E-06  5.968E-06 -6.898E-06  8.103E-06 -9.708E-06  1.190E-05 -1.491E-05  1.828E-05
           -2.501E-06  2.592E-04  1.263E-04  1.095E-04  3.518E-04 -8.695E-06  4.089E-04  1.599E-04  1.887E-04  5.181E-04
            5.463E-06  6.349E-04  2.772E-04  3.008E-04  9.070E-04 -4.197E-06  1.183E-03  5.183E-04  6.167E-04  1.906E-03
            3.593E-06  2.849E-03  1.382E-03  1.798E-03  6.422E-03 -3.306E-06  1.444E-02  9.803E-03  2.207E-02  2.311E-01
            4.034E-01  2.311E-01  2.207E-02  9.803E-03  1.444E-02 -3.306E-06  6.422E-03  1.798E-03  1.382E-03  2.849E-03
            3.593E-06  1.906E-03  6.167E-04  5.183E-04  1.183E-03 -4.197E-06  9.070E-04  3.008E-04  2.772E-04  6.349E-04
            5.463E-06  5.181E-04  1.887E-04  1.599E-04  4.089E-04 -8.695E-06  3.518E-04  1.095E-04  1.263E-04  2.592E-04
           -2.501E-06  1.828E-05 -1.491E-05  1.190E-05 -9.708E-06  8.103E-06 -6.898E-06  5.967E-06 -5.232E-06  4.640E-06
           -4.153E-06  3.747E-06 -3.405E-06  3.112E-06 -2.860E-06  2.640E-06 -2.448E-06  2.278E-06 -2.126E-06  1.991E-06
           -1.870E-06  1.760E-06 -1.661E-06  1.570E-06 -1.488E-06  1.412E-06 -1.342E-06  1.278E-06 -1.219E-06  1.164E-06
           -1.113E-06  1.065E-06 -1.021E-06  9.794E-07 -9.407E-07  9.043E-07 -8.702E-07  8.377E-07 -8.076E-07  7.790E-07
           -7.519E-07  7.264E-07 -7.022E-07  6.793E-07 -6.572E-07  6.367E-07 -6.172E-07  5.980E-07 -5.805E-07  5.632E-07
           -5.470E-07  5.314E-07 -5.166E-07  5.024E-07 -4.887E-07  4.756E-07 -4.632E-07  4.512E-07 -4.395E-07  4.286E-07
           -4.179E-07  4.077E-07 -3.976E-07  3.883E-07 -3.792E-07  3.703E-07 -3.618E-07  3.537E-07 -3.457E-07  3.380E-07
           -3.306E-07  3.235E-07 -3.165E-07  3.098E-07 -3.035E-07  2.972E-07 -2.910E-07  2.852E-07 -2.795E-07  2.739E-07
           -2.687E-07  2.635E-07 -2.583E-07  2.535E-07 -2.487E-07  2.441E-07 -2.395E-07  2.353E-07 -2.310E-07  2.268E-07
           -2.228E-07  2.190E-07 -2.151E-07  2.114E-07 -2.079E-07  2.043E-07
MIDPT=        131
R(I)=
            4.034E-01  2.311E-01  2.207E-02  9.803E-03  1.444E-02 -3.306E-06  6.422E-03  1.798E-03  1.382E-03  2.849E-03
            3.593E-06  1.906E-03  6.167E-04  5.183E-04  1.183E-03 -4.197E-06  9.070E-04  3.008E-04  2.772E-04  6.349E-04
            5.463E-06  5.181E-04  1.887E-04  1.599E-04  4.089E-04 -8.695E-06  3.518E-04  1.095E-04  1.263E-04  2.592E-04
           -2.501E-06  1.828E-05 -1.491E-05  1.190E-05 -9.708E-06  8.103E-06 -6.898E-06  5.967E-06 -5.232E-06  4.640E-06
           -4.153E-06  3.747E-06 -3.405E-06  3.112E-06 -2.860E-06  2.640E-06 -2.448E-06  2.278E-06 -2.126E-06  1.991E-06
           -1.870E-06  1.760E-06 -1.661E-06  1.570E-06 -1.488E-06  1.412E-06 -1.342E-06  1.278E-06 -1.219E-06  1.164E-06
           -1.113E-06  1.065E-06 -1.021E-06  9.794E-07 -9.407E-07  9.043E-07 -8.702E-07  8.377E-07 -8.076E-07  7.790E-07
           -7.519E-07  7.264E-07 -7.022E-07  6.793E-07 -6.572E-07  6.367E-07 -6.172E-07  5.980E-07 -5.805E-07  5.632E-07
           -5.470E-07  5.314E-07 -5.166E-07  5.024E-07 -4.887E-07  4.756E-07 -4.632E-07  4.512E-07 -4.395E-07  4.286E-07
           -4.179E-07  4.077E-07 -3.976E-07  3.883E-07 -3.792E-07  3.703E-07 -3.618E-07  3.537E-07 -3.457E-07  3.380E-07
           -3.306E-07  3.235E-07 -3.165E-07  3.098E-07 -3.035E-07  2.972E-07 -2.910E-07  2.852E-07 -2.795E-07  2.739E-07
           -2.687E-07  2.635E-07 -2.583E-07  2.535E-07 -2.487E-07  2.441E-07 -2.395E-07  2.353E-07 -2.310E-07  2.268E-07
           -2.228E-07  2.190E-07 -2.151E-07  2.114E-07 -2.079E-07  2.043E-07  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
LLT=        130
RESAMPLED DATA FOR TRANSFORM
R=
            4.034E-01  2.311E-01  2.207E-02  9.803E-03  1.444E-02 -3.306E-06  6.422E-03  1.798E-03  1.382E-03  2.849E-03
            3.593E-06  1.906E-03  6.167E-04  5.183E-04  1.183E-03 -4.197E-06  9.070E-04  3.008E-04  2.772E-04  6.349E-04
            5.463E-06  5.181E-04  1.887E-04  1.599E-04  4.089E-04 -8.695E-06  3.518E-04  1.095E-04  1.263E-04  2.592E-04
           -2.501E-06  1.828E-05 -1.491E-05  1.190E-05 -9.708E-06  8.103E-06 -6.898E-06  5.967E-06 -5.232E-06  4.640E-06
           -4.153E-06  3.747E-06 -3.405E-06  3.112E-06 -2.860E-06  2.640E-06 -2.448E-06  2.278E-06 -2.126E-06  1.991E-06
           -1.870E-06  1.760E-06 -1.661E-06  1.570E-06 -1.488E-06  1.412E-06 -1.342E-06  1.278E-06 -1.219E-06  1.164E-06
           -1.113E-06  1.065E-06 -1.021E-06  9.794E-07 -9.407E-07  9.043E-07 -8.702E-07  8.377E-07 -8.076E-07  7.790E-07
           -7.519E-07  7.264E-07 -7.022E-07  6.793E-07 -6.572E-07  6.367E-07 -6.172E-07  5.980E-07 -5.805E-07  5.632E-07
           -5.470E-07  5.314E-07 -5.166E-07  5.024E-07 -4.887E-07  4.756E-07 -4.632E-07  4.512E-07 -4.395E-07  4.286E-07
           -4.179E-07  4.077E-07 -3.976E-07  3.883E-07 -3.792E-07  3.703E-07 -3.618E-07  3.537E-07 -3.457E-07  3.380E-07
           -3.306E-07  3.235E-07 -3.165E-07  3.098E-07 -3.035E-07  2.972E-07 -2.910E-07  2.852E-07 -2.795E-07  2.739E-07
           -2.687E-07  2.635E-07 -2.583E-07  2.535E-07 -2.487E-07  2.441E-07 -2.395E-07  2.353E-07 -2.310E-07  2.268E-07
           -2.228E-07  2.190E-07 -2.151E-07  2.114E-07 -2.079E-07  2.043E-07 -1.880E-07  1.911E-07 -1.943E-07  1.976E-07
           -2.008E-07  2.043E-07 -2.078E-07  2.114E-07 -2.151E-07  2.190E-07 -2.228E-07  2.268E-07 -2.310E-07  2.353E-07
           -2.395E-07  2.441E-07 -2.487E-07  2.535E-07 -2.583E-07  2.634E-07 -2.687E-07  2.739E-07 -2.796E-07  2.852E-07
           -2.910E-07  2.971E-07 -3.035E-07  3.099E-07 -3.165E-07  3.235E-07 -3.306E-07  3.380E-07 -3.456E-07  3.536E-07
           -3.618E-07  3.702E-07 -3.792E-07  3.883E-07 -3.977E-07  4.077E-07 -4.179E-07  4.286E-07 -4.395E-07  4.512E-07
           -4.632E-07  4.756E-07 -4.887E-07  5.024E-07 -5.166E-07  5.314E-07 -5.470E-07  5.632E-07 -5.805E-07  5.980E-07
           -6.171E-07  6.366E-07 -6.573E-07  6.793E-07 -7.022E-07  7.264E-07 -7.519E-07  7.790E-07 -8.076E-07  8.377E-07
           -8.702E-07  9.042E-07 -9.406E-07  9.795E-07 -1.021E-06  1.065E-06 -1.113E-06  1.164E-06 -1.219E-06  1.278E-06
           -1.342E-06  1.412E-06 -1.488E-06  1.570E-06 -1.661E-06  1.760E-06 -1.870E-06  1.991E-06 -2.127E-06  2.278E-06
           -2.448E-06  2.640E-06 -2.860E-06  3.112E-06 -3.405E-06  3.747E-06 -4.153E-06  4.640E-06 -5.232E-06  5.968E-06
           -6.898E-06  8.103E-06 -9.708E-06  1.190E-05 -1.491E-05  1.828E-05 -2.501E-06  2.592E-04  1.263E-04  1.095E-04
            3.518E-04 -8.695E-06  4.089E-04  1.599E-04  1.887E-04  5.181E-04  5.463E-06  6.349E-04  2.772E-04  3.008E-04
            9.070E-04 -4.197E-06  1.183E-03  5.183E-04  6.167E-04  1.906E-03  3.593E-06  2.849E-03  1.382E-03  1.798E-03
            6.422E-03 -3.306E-06  1.444E-02  9.803E-03  2.207E-02  2.311E-01

 PIXEL  CYCLES/      FREQUENCY    REAL(FT)     IMAG.(FT)     INTENSITY    AMPLITUDE     PHASE     WAVE LENGT
         PIXEL                                                                         RADIANS      SHIFT
  1     0.00000       0.00000      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
  2     0.00391       0.00391      0.99773      0.00000      0.99546      0.99773      0.00000      0.00000
  3     0.00781       0.00781      0.99130     -0.00000      0.98267      0.99130      0.00000      0.00000
  4     0.01172       0.01172      0.98178      0.00000      0.96389      0.98178      0.00000      0.00000
  5     0.01563       0.01563      0.97058      0.00000      0.94202      0.97058      0.00000      0.00000
  6     0.01953       0.01953      0.95903      0.00000      0.91974      0.95903      0.00000      0.00000
  7     0.02344       0.02344      0.94808      0.00000      0.89886      0.94808      0.00000      0.00000
  8     0.02734       0.02734      0.93809      0.00000      0.88001      0.93809      0.00000      0.00000
  9     0.03125       0.03125      0.92888     -0.00000      0.86281      0.92888      0.00000      0.00000
 10     0.03516       0.03516      0.91997      0.00000      0.84634      0.91997      0.00000      0.00000
 11     0.03906       0.03906      0.91084     -0.00000      0.82963      0.91084      0.00000      0.00000
 12     0.04297       0.04297      0.90119      0.00000      0.81214      0.90119      0.00000      0.00000
 13     0.04688       0.04688      0.89099     -0.00000      0.79387      0.89099      0.00000      0.00000
 14     0.05078       0.05078      0.88052      0.00000      0.77531      0.88052      0.00000      0.00000
 15     0.05469       0.05469      0.87010      0.00000      0.75708      0.87010      0.00000      0.00000
 16     0.05859       0.05859      0.86002      0.00000      0.73964      0.86002      0.00000      0.00000
 17     0.06250       0.06250      0.85035     -0.00000      0.72310      0.85035      0.00000      0.00000
 18     0.06641       0.06641      0.84095      0.00000      0.70720      0.84095      0.00000      0.00000
 19     0.07031       0.07031      0.83157      0.00000      0.69151      0.83157      0.00000      0.00000
 20     0.07422       0.07422      0.82198      0.00000      0.67565      0.82198      0.00000      0.00000
 21     0.07813       0.07813      0.81206      0.00000      0.65944      0.81206      0.00000      0.00000
 22     0.08203       0.08203      0.80189      0.00000      0.64302      0.80189      0.00000      0.00000
 23     0.08594       0.08594      0.79164      0.00000      0.62669      0.79164      0.00000      0.00000
 24     0.08984       0.08984      0.78153      0.00000      0.61078      0.78153      0.00000      0.00000
 25     0.09375       0.09375      0.77167     -0.00000      0.59547      0.77167      0.00000      0.00000
 26     0.09766       0.09766      0.76205      0.00000      0.58072      0.76205      0.00000      0.00000
 27     0.10156       0.10156      0.75254      0.00000      0.56632      0.75254      0.00000      0.00000
 28     0.10547       0.10547      0.74296      0.00000      0.55199      0.74296      0.00000      0.00000
 29     0.10938       0.10938      0.73318      0.00000      0.53755      0.73318      0.00000      0.00000
 30     0.11328       0.11328      0.72318      0.00000      0.52298      0.72318      0.00000      0.00000
 31     0.11719       0.11719      0.71304      0.00000      0.50843      0.71304      0.00000      0.00000
 32     0.12109       0.12109      0.70292      0.00000      0.49410      0.70292      0.00000      0.00000
 33     0.12500       0.12500      0.69296      0.00000      0.48020      0.69296      0.00000      0.00000
 34     0.12891       0.12891      0.68321      0.00000      0.46677      0.68321      0.00000      0.00000
 35     0.13281       0.13281      0.67360      0.00000      0.45373      0.67360      0.00000      0.00000
 36     0.13672       0.13672      0.66400      0.00000      0.44090      0.66400      0.00000      0.00000
 37     0.14063       0.14063      0.65430      0.00000      0.42811      0.65430      0.00000      0.00000
 38     0.14453       0.14453      0.64442      0.00000      0.41527      0.64442      0.00000      0.00000
 39     0.14844       0.14844      0.63438      0.00000      0.40243      0.63438      0.00000      0.00000
 40     0.15234       0.15234      0.62428      0.00000      0.38973      0.62428      0.00000      0.00000
 41     0.15625       0.15625      0.61426     -0.00000      0.37732      0.61426      0.00000      0.00000
 42     0.16016       0.16016      0.60440      0.00000      0.36530      0.60440      0.00000      0.00000
 43     0.16406       0.16406      0.59470      0.00000      0.35367      0.59470      0.00000      0.00000
 44     0.16797       0.16797      0.58508      0.00000      0.34232      0.58508      0.00000      0.00000
 45     0.17188       0.17188      0.57542      0.00000      0.33111      0.57542      0.00000      0.00000
 46     0.17578       0.17578      0.56562      0.00000      0.31993      0.56562      0.00000      0.00000
 47     0.17969       0.17969      0.55567      0.00000      0.30877      0.55567      0.00000      0.00000
 48     0.18359       0.18359      0.54562      0.00000      0.29770      0.54562      0.00000      0.00000
 49     0.18750       0.18750      0.53557      0.00000      0.28684      0.53557      0.00000      0.00000
 50     0.19141       0.19141      0.52563      0.00000      0.27629      0.52563      0.00000      0.00000
 51     0.19531       0.19531      0.51585      0.00000      0.26610      0.51585      0.00000      0.00000
 52     0.19922       0.19922      0.50618      0.00000      0.25622      0.50618      0.00000      0.00000
 53     0.20313       0.20313      0.49653      0.00000      0.24654      0.49653      0.00000      0.00000
 54     0.20703       0.20703      0.48680      0.00000      0.23698      0.48680      0.00000      0.00000
 55     0.21094       0.21094      0.47693      0.00000      0.22746      0.47693      0.00000      0.00000
 56     0.21484       0.21484      0.46693      0.00000      0.21802      0.46693      0.00000      0.00000
 57     0.21875       0.21875      0.45688      0.00000      0.20874      0.45688      0.00000      0.00000
 58     0.22266       0.22266      0.44688      0.00000      0.19971      0.44688      0.00000      0.00000
 59     0.22656       0.22656      0.43702      0.00000      0.19099      0.43702      0.00000      0.00000
 60     0.23047       0.23047      0.42729      0.00000      0.18258      0.42729      0.00000      0.00000
 61     0.23438       0.23438      0.41764      0.00000      0.17442      0.41764      0.00000      0.00000
 62     0.23828       0.23828      0.40796      0.00000      0.16643      0.40796      0.00000      0.00000
 63     0.24219       0.24219      0.39816      0.00000      0.15853      0.39816      0.00000      0.00000
 64     0.24609       0.24609      0.38822      0.00000      0.15072      0.38822      0.00000      0.00000
 65     0.25000       0.25000      0.37819      0.00000      0.14303      0.37819      0.00000      0.00000
 66     0.25391       0.25391      0.36816     -0.00000      0.13554      0.36816      0.00000      0.00000
 67     0.25781       0.25781      0.35822      0.00000      0.12832      0.35822      0.00000      0.00000
 68     0.26172       0.26172      0.34842      0.00000      0.12140      0.34842      0.00000      0.00000
 69     0.26563       0.26563      0.33875      0.00000      0.11475      0.33875      0.00000      0.00000
 70     0.26953       0.26953      0.32910     -0.00000      0.10831      0.32910      0.00000      0.00000
 71     0.27344       0.27344      0.31938      0.00000      0.10200      0.31938      0.00000      0.00000
 72     0.27734       0.27734      0.30951      0.00000      0.09580      0.30951      0.00000      0.00000
 73     0.28125       0.28125      0.29951      0.00000      0.08970      0.29951      0.00000      0.00000
 74     0.28516       0.28516      0.28944      0.00000      0.08378      0.28944      0.00000      0.00000
 75     0.28906       0.28906      0.27944      0.00000      0.07808      0.27944      0.00000      0.00000
 76     0.29297       0.29297      0.26956      0.00000      0.07267      0.26956      0.00000      0.00000
 77     0.29688       0.29688      0.25985      0.00000      0.06752      0.25985      0.00000      0.00000
 78     0.30078       0.30078      0.25022     -0.00000      0.06261      0.25022      0.00000      0.00000
 79     0.30469       0.30469      0.24057      0.00000      0.05787      0.24057      0.00000      0.00000
 80     0.30859       0.30859      0.23079      0.00000      0.05326      0.23079      0.00000      0.00000
 81     0.31250       0.31250      0.22083      0.00000      0.04877      0.22083      0.00000      0.00000
 82     0.31641       0.31641      0.21075     -0.00000      0.04442      0.21075      0.00000      0.00000
 83     0.32031       0.32031      0.20067      0.00000      0.04027      0.20067      0.00000      0.00000
 84     0.32422       0.32422      0.19070     -0.00000      0.03637      0.19070      0.00000      0.00000
 85     0.32813       0.32813      0.18092      0.00000      0.03273      0.18092      0.00000      0.00000
 86     0.33203       0.33203      0.17131     -0.00000      0.02935      0.17131      0.00000      0.00000
 87     0.33594       0.33594      0.16175      0.00000      0.02616      0.16175      0.00000      0.00000
 88     0.33984       0.33984      0.15209     -0.00000      0.02313      0.15209      0.00000      0.00000
 89     0.34375       0.34375      0.14221      0.00000      0.02022      0.14221      0.00000      0.00000
 90     0.34766       0.34766      0.13211      0.00000      0.01745      0.13211      0.00000      0.00000
 91     0.35156       0.35156      0.12190      0.00000      0.01486      0.12190      0.00000      0.00000
 92     0.35547       0.35547      0.11177     -0.00000      0.01249      0.11177      0.00000      0.00000
 93     0.35938       0.35938      0.10189      0.00000      0.01038      0.10189      0.00000      0.00000
 94     0.36328       0.36328      0.09233     -0.00000      0.00852      0.09233      0.00000      0.00000
 95     0.36719       0.36719      0.08297      0.00000      0.00688      0.08297      0.00000      0.00000
 96     0.37109       0.37109      0.07356     -0.00000      0.00541      0.07356      0.00000      0.00000
 97     0.37500       0.37500      0.06383      0.00000      0.00407      0.06383      0.00000      0.00000
 98     0.37891       0.37891      0.05361     -0.00000      0.00287      0.05361      0.00000      0.00000
 99     0.38281       0.38281      0.04297      0.00000      0.00185      0.04297      0.00000      0.00000
100     0.38672       0.38672      0.03227     -0.00000      0.00104      0.03227      0.00000      0.00000
101     0.39063       0.39063      0.02215      0.00000      0.00049      0.02215      0.00000      0.00000
102     0.39453       0.39453      0.01332     -0.00000      0.00018      0.01332      0.00000      0.00000
103     0.39844       0.39844      0.00640      0.00000      0.00004      0.00640      0.00000      0.00000
104     0.40234       0.40234      0.00172     -0.00000      0.00000      0.00172      0.00000      0.00000
105     0.40625       0.40625     -0.00076      0.00000      0.00000      0.00076      3.14159      1.23077
106     0.41016       0.41016     -0.00146     -0.00000      0.00000      0.00146      3.14159      1.21905
107     0.41406       0.41406     -0.00104      0.00000      0.00000      0.00104      3.14159      1.20755
108     0.41797       0.41797     -0.00020     -0.00000      0.00000      0.00020      0.00000      0.00000
109     0.42188       0.42188      0.00050      0.00000      0.00000      0.00050      0.00000      0.00000
110     0.42578       0.42578      0.00076      0.00000      0.00000      0.00076      0.00000      0.00000
111     0.42969       0.42969      0.00057      0.00000      0.00000      0.00057      0.00000      0.00000
112     0.43359       0.43359      0.00013     -0.00000      0.00000      0.00013      0.00000      0.00000
113     0.43750       0.43750     -0.00029      0.00000      0.00000      0.00029      0.00000      0.00000
114     0.44141       0.44141     -0.00050      0.00000      0.00000      0.00050      3.14159      1.13274
115     0.44531       0.44531     -0.00042      0.00000      0.00000      0.00042      0.00000      0.00000
116     0.44922       0.44922     -0.00014     -0.00000      0.00000      0.00014      0.00000      0.00000
117     0.45313       0.45313      0.00018      0.00000      0.00000      0.00018      0.00000      0.00000
118     0.45703       0.45703      0.00037      0.00000      0.00000      0.00037      0.00000      0.00000
119     0.46094       0.46094      0.00036     -0.00000      0.00000      0.00036      0.00000      0.00000
120     0.46484       0.46484      0.00016     -0.00000      0.00000      0.00016      0.00000      0.00000
121     0.46875       0.46875     -0.00010     -0.00000      0.00000      0.00010      0.00000      0.00000
122     0.47266       0.47266     -0.00030      0.00000      0.00000      0.00030      0.00000      0.00000
123     0.47656       0.47656     -0.00033      0.00000      0.00000      0.00033      0.00000      0.00000
124     0.48047       0.48047     -0.00018     -0.00000      0.00000      0.00018      0.00000      0.00000
125     0.48438       0.48438      0.00005      0.00000      0.00000      0.00005      0.00000      0.00000
126     0.48828       0.48828      0.00025      0.00000      0.00000      0.00025      0.00000      0.00000
127     0.49219       0.49219      0.00031     -0.00000      0.00000      0.00031      0.00000      0.00000
128     0.49609       0.49609      0.00021     -0.00000      0.00000      0.00021      0.00000      0.00000
129     0.50000       0.50000      0.00005      0.00000      0.00000      0.00005      0.00000      0.00000
NUMBER POINTS OUTPUT =         129
**** This image is ALIASED ****
INTEGRATED MTF AMPLITUDE FROM .25 TO .48 =       7.46190
typetext sinctst2.tbl
Beginning VICAR task typetext
 # FREQUENCY	   AMPLITUDE	   PHASE
    0.00000	   1.00000	   0.00000
    0.00391	   0.99773	   0.00000
    0.00781	   0.99130	   0.00000
    0.01172	   0.98178	   0.00000
    0.01563	   0.97058	   0.00000
    0.01953	   0.95903	   0.00000
    0.02344	   0.94808	   0.00000
    0.02734	   0.93809	   0.00000
    0.03125	   0.92888	   0.00000
    0.03516	   0.91997	   0.00000
    0.03906	   0.91084	   0.00000
    0.04297	   0.90119	   0.00000
    0.04688	   0.89099	   0.00000
    0.05078	   0.88052	   0.00000
    0.05469	   0.87010	   0.00000
    0.05859	   0.86002	   0.00000
    0.06250	   0.85035	   0.00000
    0.06641	   0.84095	   0.00000
    0.07031	   0.83157	   0.00000
    0.07422	   0.82198	   0.00000
    0.07813	   0.81206	   0.00000
    0.08203	   0.80189	   0.00000
    0.08594	   0.79164	   0.00000
    0.08984	   0.78153	   0.00000
    0.09375	   0.77167	   0.00000
    0.09766	   0.76205	   0.00000
    0.10156	   0.75254	   0.00000
    0.10547	   0.74296	   0.00000
    0.10938	   0.73318	   0.00000
    0.11328	   0.72318	   0.00000
    0.11719	   0.71304	   0.00000
    0.12109	   0.70292	   0.00000
    0.12500	   0.69296	   0.00000
    0.12891	   0.68321	   0.00000
    0.13281	   0.67360	   0.00000
    0.13672	   0.66400	   0.00000
    0.14063	   0.65430	   0.00000
    0.14453	   0.64442	   0.00000
    0.14844	   0.63438	   0.00000
    0.15234	   0.62428	   0.00000
    0.15625	   0.61426	   0.00000
    0.16016	   0.60440	   0.00000
    0.16406	   0.59470	   0.00000
    0.16797	   0.58508	   0.00000
    0.17188	   0.57542	   0.00000
    0.17578	   0.56562	   0.00000
    0.17969	   0.55567	   0.00000
    0.18359	   0.54562	   0.00000
    0.18750	   0.53557	   0.00000
    0.19141	   0.52563	   0.00000
    0.19531	   0.51585	   0.00000
    0.19922	   0.50618	   0.00000
    0.20313	   0.49653	   0.00000
    0.20703	   0.48680	   0.00000
    0.21094	   0.47693	   0.00000
    0.21484	   0.46693	   0.00000
    0.21875	   0.45688	   0.00000
    0.22266	   0.44688	   0.00000
    0.22656	   0.43702	   0.00000
    0.23047	   0.42729	   0.00000
    0.23438	   0.41764	   0.00000
    0.23828	   0.40796	   0.00000
    0.24219	   0.39816	   0.00000
    0.24609	   0.38822	   0.00000
    0.25000	   0.37819	   0.00000
    0.25391	   0.36816	   0.00000
    0.25781	   0.35822	   0.00000
    0.26172	   0.34842	   0.00000
    0.26563	   0.33875	   0.00000
    0.26953	   0.32910	   0.00000
    0.27344	   0.31938	   0.00000
    0.27734	   0.30951	   0.00000
    0.28125	   0.29951	   0.00000
    0.28516	   0.28944	   0.00000
    0.28906	   0.27944	   0.00000
    0.29297	   0.26956	   0.00000
    0.29688	   0.25985	   0.00000
    0.30078	   0.25022	   0.00000
    0.30469	   0.24057	   0.00000
    0.30859	   0.23079	   0.00000
    0.31250	   0.22083	   0.00000
    0.31641	   0.21075	   0.00000
    0.32031	   0.20067	   0.00000
    0.32422	   0.19070	   0.00000
    0.32813	   0.18092	   0.00000
    0.33203	   0.17131	   0.00000
    0.33594	   0.16175	   0.00000
    0.33984	   0.15209	   0.00000
    0.34375	   0.14221	   0.00000
    0.34766	   0.13211	   0.00000
    0.35156	   0.12190	   0.00000
    0.35547	   0.11177	   0.00000
    0.35938	   0.10189	   0.00000
    0.36328	   0.09233	   0.00000
    0.36719	   0.08297	   0.00000
    0.37109	   0.07356	   0.00000
    0.37500	   0.06383	   0.00000
    0.37891	   0.05361	   0.00000
    0.38281	   0.04297	   0.00000
    0.38672	   0.03227	   0.00000
    0.39063	   0.02215	   0.00000
    0.39453	   0.01332	   0.00000
    0.39844	   0.00640	   0.00000
    0.40234	   0.00172	   0.00000
    0.40625	   0.00076	   3.14159
    0.41016	   0.00146	   3.14159
    0.41406	   0.00104	   3.14159
    0.41797	   0.00020	   0.00000
    0.42188	   0.00050	   0.00000
    0.42578	   0.00076	   0.00000
    0.42969	   0.00057	   0.00000
    0.43359	   0.00013	   0.00000
    0.43750	   0.00029	   0.00000
    0.44141	   0.00050	   3.14159
    0.44531	   0.00042	   0.00000
    0.44922	   0.00014	   0.00000
    0.45313	   0.00018	   0.00000
    0.45703	   0.00037	   0.00000
    0.46094	   0.00036	   0.00000
    0.46484	   0.00016	   0.00000
    0.46875	   0.00010	   0.00000
    0.47266	   0.00030	   0.00000
    0.47656	   0.00033	   0.00000
    0.48047	   0.00018	   0.00000
    0.48438	   0.00005	   0.00000
    0.48828	   0.00025	   0.00000
    0.49219	   0.00031	   0.00000
    0.49609	   0.00021	   0.00000
    0.50000	   0.00005	   0.00000
if (mode = "nobatch" or mode = "inter")
end-if
otf1 out=test2.tbl esf=(0,0,0,0,24,48,72,96,120,144,168,192,216,216,216,216)  +
    plotout=test2
Beginning VICAR task otf1
OTF1 version 2015-08-10
NUMBER OF LINES PROCESSED=          1

 PIXEL  CYCLES/      FREQUENCY    REAL(FT)     IMAG.(FT)     INTENSITY    AMPLITUDE     PHASE     WAVE LENGT
         PIXEL                                                                         RADIANS      SHIFT
  1     0.00000       0.00000      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
  2     0.00391       0.00391      0.99799     -0.00000      0.99599      0.99799      0.00000      0.00000
  3     0.00781       0.00781      0.99199      0.00000      0.98404      0.99199      0.00000      0.00000
  4     0.01172       0.01172      0.98202      0.00000      0.96437      0.98202      0.00000      0.00000
  5     0.01563       0.01563      0.96817      0.00000      0.93736      0.96817      0.00000      0.00000
  6     0.01953       0.01953      0.95054      0.00000      0.90352      0.95054      0.00000      0.00000
  7     0.02344       0.02344      0.92924     -0.00000      0.86348      0.92924      0.00000      0.00000
  8     0.02734       0.02734      0.90443      0.00000      0.81799      0.90443      0.00000      0.00000
  9     0.03125       0.03125      0.87628     -0.00000      0.76786      0.87628      0.00000      0.00000
 10     0.03516       0.03516      0.84498     -0.00000      0.71400      0.84498      0.00000      0.00000
 11     0.03906       0.03906      0.81077      0.00000      0.65735      0.81077      0.00000      0.00000
 12     0.04297       0.04297      0.77388      0.00000      0.59888      0.77388      0.00000      0.00000
 13     0.04688       0.04688      0.73455     -0.00000      0.53957      0.73455      0.00000      0.00000
 14     0.05078       0.05078      0.69307     -0.00000      0.48035      0.69307      0.00000      0.00000
 15     0.05469       0.05469      0.64972     -0.00000      0.42214      0.64972      0.00000      0.00000
 16     0.05859       0.05859      0.60479     -0.00000      0.36578      0.60479      0.00000      0.00000
 17     0.06250       0.06250      0.55859     -0.00000      0.31203      0.55859      0.00000      0.00000
 18     0.06641       0.06641      0.51143     -0.00000      0.26156      0.51143      0.00000      0.00000
 19     0.07031       0.07031      0.46362     -0.00000      0.21494      0.46362      0.00000      0.00000
 20     0.07422       0.07422      0.41547      0.00000      0.17261      0.41547      0.00000      0.00000
 21     0.07813       0.07813      0.36729     -0.00000      0.13491      0.36729      0.00000      0.00000
 22     0.08203       0.08203      0.31941      0.00000      0.10202      0.31941      0.00000      0.00000
 23     0.08594       0.08594      0.27211     -0.00000      0.07404      0.27211      0.00000      0.00000
 24     0.08984       0.08984      0.22569     -0.00000      0.05094      0.22569      0.00000      0.00000
 25     0.09375       0.09375      0.18043     -0.00000      0.03256      0.18043      0.00000      0.00000
 26     0.09766       0.09766      0.13661      0.00000      0.01866      0.13661      0.00000      0.00000
 27     0.10156       0.10156      0.09447      0.00000      0.00893      0.09447      0.00000      0.00000
 28     0.10547       0.10547      0.05426     -0.00000      0.00294      0.05426      0.00000      0.00000
 29     0.10938       0.10938      0.01618     -0.00000      0.00026      0.01618      0.00000      0.00000
 30     0.11328       0.11328     -0.01956     -0.00000      0.00038      0.01956      3.14159      4.41379
 31     0.11719       0.11719     -0.05278     -0.00000      0.00279      0.05278      3.14159      4.26667
 32     0.12109       0.12109     -0.08334     -0.00000      0.00695      0.08334      3.14159      4.12903
 33     0.12500       0.12500     -0.11111     -0.00000      0.01235      0.11111      3.14159      4.00000
 34     0.12891       0.12891     -0.13598     -0.00000      0.01849      0.13598      3.14159      3.87879
 35     0.13281       0.13281     -0.15788     -0.00000      0.02493      0.15788      3.14159      3.76471
 36     0.13672       0.13672     -0.17674     -0.00000      0.03124      0.17674      3.14159      3.65714
 37     0.14063       0.14063     -0.19256     -0.00000      0.03708      0.19256      3.14159      3.55556
 38     0.14453       0.14453     -0.20531     -0.00000      0.04215      0.20531      3.14159      3.45946
 39     0.14844       0.14844     -0.21502      0.00000      0.04623      0.21502      3.14159      3.36842
 40     0.15234       0.15234     -0.22175     -0.00000      0.04917      0.22175      3.14159      3.28205
 41     0.15625       0.15625     -0.22556     -0.00000      0.05088      0.22556      3.14159      3.20000
 42     0.16016       0.16016     -0.22654      0.00000      0.05132      0.22654      3.14159      3.12195
 43     0.16406       0.16406     -0.22481      0.00000      0.05054      0.22481      3.14159      3.04762
 44     0.16797       0.16797     -0.22051     -0.00000      0.04863      0.22051      3.14159      2.97674
 45     0.17188       0.17188     -0.21379     -0.00000      0.04570      0.21379      3.14159      2.90909
 46     0.17578       0.17578     -0.20481      0.00000      0.04195      0.20481      3.14159      2.84444
 47     0.17969       0.17969     -0.19377      0.00000      0.03755      0.19377      3.14159      2.78261
 48     0.18359       0.18359     -0.18086      0.00000      0.03271      0.18086      3.14159      2.72340
 49     0.18750       0.18750     -0.16629      0.00000      0.02765      0.16629      3.14159      2.66667
 50     0.19141       0.19141     -0.15028      0.00000      0.02258      0.15028      3.14159      2.61224
 51     0.19531       0.19531     -0.13306      0.00000      0.01770      0.13306      3.14159      2.56000
 52     0.19922       0.19922     -0.11485     -0.00000      0.01319      0.11485      3.14159      2.50980
 53     0.20313       0.20313     -0.09589      0.00000      0.00920      0.09589      3.14159      2.46154
 54     0.20703       0.20703     -0.07641      0.00000      0.00584      0.07641      3.14159      2.41509
 55     0.21094       0.21094     -0.05665      0.00000      0.00321      0.05665      3.14159      2.37037
 56     0.21484       0.21484     -0.03683      0.00000      0.00136      0.03683      3.14159      2.32727
 57     0.21875       0.21875     -0.01717      0.00000      0.00029      0.01717      3.14159      2.28571
 58     0.22266       0.22266      0.00212     -0.00000      0.00000      0.00212      0.00000      0.00000
 59     0.22656       0.22656      0.02082      0.00000      0.00043      0.02082      0.00000      0.00000
 60     0.23047       0.23047      0.03876      0.00000      0.00150      0.03876      0.00000      0.00000
 61     0.23438       0.23438      0.05574      0.00000      0.00311      0.05574      0.00000      0.00000
 62     0.23828       0.23828      0.07161      0.00000      0.00513      0.07161      0.00000      0.00000
 63     0.24219       0.24219      0.08621      0.00000      0.00743      0.08621      0.00000      0.00000
 64     0.24609       0.24609      0.09941      0.00000      0.00988      0.09941      0.00000      0.00000
 65     0.25000       0.25000      0.11111      0.00000      0.01235      0.11111      0.00000      0.00000
 66     0.25391       0.25391      0.12120     -0.00000      0.01469      0.12120      0.00000      0.00000
 67     0.25781       0.25781      0.12961      0.00000      0.01680      0.12961      0.00000      0.00000
 68     0.26172       0.26172      0.13629      0.00000      0.01857      0.13629      0.00000      0.00000
 69     0.26563       0.26563      0.14119      0.00000      0.01994      0.14119      0.00000      0.00000
 70     0.26953       0.26953      0.14431      0.00000      0.02082      0.14431      0.00000      0.00000
 71     0.27344       0.27344      0.14563      0.00000      0.02121      0.14563      0.00000      0.00000
 72     0.27734       0.27734      0.14520      0.00000      0.02108      0.14520      0.00000      0.00000
 73     0.28125       0.28125      0.14305     -0.00000      0.02046      0.14305      0.00000      0.00000
 74     0.28516       0.28516      0.13923      0.00000      0.01938      0.13923      0.00000      0.00000
 75     0.28906       0.28906      0.13383     -0.00000      0.01791      0.13383      0.00000      0.00000
 76     0.29297       0.29297      0.12693      0.00000      0.01611      0.12693      0.00000      0.00000
 77     0.29688       0.29688      0.11865      0.00000      0.01408      0.11865      0.00000      0.00000
 78     0.30078       0.30078      0.10911     -0.00000      0.01190      0.10911      0.00000      0.00000
 79     0.30469       0.30469      0.09843      0.00000      0.00969      0.09843      0.00000      0.00000
 80     0.30859       0.30859      0.08675     -0.00000      0.00753      0.08675      0.00000      0.00000
 81     0.31250       0.31250      0.07424     -0.00000      0.00551      0.07424      0.00000      0.00000
 82     0.31641       0.31641      0.06105     -0.00000      0.00373      0.06105      0.00000      0.00000
 83     0.32031       0.32031      0.04733     -0.00000      0.00224      0.04733      0.00000      0.00000
 84     0.32422       0.32422      0.03326     -0.00000      0.00111      0.03326      0.00000      0.00000
 85     0.32813       0.32813      0.01901      0.00000      0.00036      0.01901      0.00000      0.00000
 86     0.33203       0.33203      0.00473     -0.00000      0.00002      0.00473      0.00000      0.00000
 87     0.33594       0.33594     -0.00939     -0.00000      0.00009      0.00939      3.14159      1.48837
 88     0.33984       0.33984     -0.02321     -0.00000      0.00054      0.02321      3.14159      1.47126
 89     0.34375       0.34375     -0.03657      0.00000      0.00134      0.03657      3.14159      1.45455
 90     0.34766       0.34766     -0.04932     -0.00000      0.00243      0.04932      3.14159      1.43820
 91     0.35156       0.35156     -0.06131     -0.00000      0.00376      0.06131      3.14159      1.42222
 92     0.35547       0.35547     -0.07243     -0.00000      0.00525      0.07243      3.14159      1.40659
 93     0.35938       0.35938     -0.08254      0.00000      0.00681      0.08254      3.14159      1.39130
 94     0.36328       0.36328     -0.09155     -0.00000      0.00838      0.09155      3.14159      1.37634
 95     0.36719       0.36719     -0.09937     -0.00000      0.00987      0.09937      3.14159      1.36170
 96     0.37109       0.37109     -0.10591     -0.00000      0.01122      0.10591      3.14159      1.34737
 97     0.37500       0.37500     -0.11111      0.00000      0.01235      0.11111      3.14159      1.33333
 98     0.37891       0.37891     -0.11493      0.00000      0.01321      0.11493      3.14159      1.31959
 99     0.38281       0.38281     -0.11734     -0.00000      0.01377      0.11734      3.14159      1.30612
100     0.38672       0.38672     -0.11832     -0.00000      0.01400      0.11832      3.14159      1.29293
101     0.39063       0.39063     -0.11787      0.00000      0.01389      0.11787      3.14159      1.28000
102     0.39453       0.39453     -0.11601     -0.00000      0.01346      0.11601      3.14159      1.26733
103     0.39844       0.39844     -0.11278     -0.00000      0.01272      0.11278      3.14159      1.25490
104     0.40234       0.40234     -0.10822     -0.00000      0.01171      0.10822      3.14159      1.24272
105     0.40625       0.40625     -0.10240      0.00000      0.01049      0.10240      3.14159      1.23077
106     0.41016       0.41016     -0.09540     -0.00000      0.00910      0.09540      3.14159      1.21905
107     0.41406       0.41406     -0.08730      0.00000      0.00762      0.08730      3.14159      1.20755
108     0.41797       0.41797     -0.07820     -0.00000      0.00612      0.07820      3.14159      1.19626
109     0.42188       0.42188     -0.06823      0.00000      0.00466      0.06823      3.14159      1.18519
110     0.42578       0.42578     -0.05750      0.00000      0.00331      0.05750      3.14159      1.17431
111     0.42969       0.42969     -0.04615      0.00000      0.00213      0.04615      3.14159      1.16364
112     0.43359       0.43359     -0.03430     -0.00000      0.00118      0.03430      3.14159      1.15315
113     0.43750       0.43750     -0.02210     -0.00000      0.00049      0.02210      3.14159      1.14286
114     0.44141       0.44141     -0.00970      0.00000      0.00009      0.00970      3.14159      1.13274
115     0.44531       0.44531      0.00277      0.00000      0.00001      0.00277      0.00000      0.00000
116     0.44922       0.44922      0.01515     -0.00000      0.00023      0.01515      0.00000      0.00000
117     0.45313       0.45313      0.02729      0.00000      0.00074      0.02729      0.00000      0.00000
118     0.45703       0.45703      0.03907      0.00000      0.00153      0.03907      0.00000      0.00000
119     0.46094       0.46094      0.05034      0.00000      0.00253      0.05034      0.00000      0.00000
120     0.46484       0.46484      0.06096     -0.00000      0.00372      0.06096      0.00000      0.00000
121     0.46875       0.46875      0.07083      0.00000      0.00502      0.07083      0.00000      0.00000
122     0.47266       0.47266      0.07982      0.00000      0.00637      0.07982      0.00000      0.00000
123     0.47656       0.47656      0.08783     -0.00000      0.00771      0.08783      0.00000      0.00000
124     0.48047       0.48047      0.09477      0.00000      0.00898      0.09477      0.00000      0.00000
125     0.48438       0.48438      0.10056      0.00000      0.01011      0.10056      0.00000      0.00000
126     0.48828       0.48828      0.10514      0.00000      0.01105      0.10514      0.00000      0.00000
127     0.49219       0.49219      0.10844     -0.00000      0.01176      0.10844      0.00000      0.00000
128     0.49609       0.49609      0.11044     -0.00000      0.01220      0.11044      0.00000      0.00000
129     0.50000       0.50000      0.11111      0.00000      0.01235      0.11111      0.00000      0.00000
NUMBER POINTS OUTPUT =         129
**** This image is ALIASED ****
INTEGRATED MTF AMPLITUDE FROM .25 TO .48 =       4.94845
if (mode = "nobatch" or mode = "inter")
end-if
otf1 out=test3.img esf=(0,0,0,12,30,62,94,126,158,190,206,224,224)  +
    plotout=test3 plotprof=test3
Beginning VICAR task otf1
OTF1 version 2015-08-10
NUMBER OF LINES PROCESSED=          1

 PIXEL  CYCLES/      FREQUENCY    REAL(FT)     IMAG.(FT)     INTENSITY    AMPLITUDE     PHASE     WAVE LENGT
         PIXEL                                                                         RADIANS      SHIFT
  1     0.00000       0.00000      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
  2     0.00391       0.00391      0.99867     -0.00000      0.99734      0.99867      0.00000      0.00000
  3     0.00781       0.00781      0.99407      0.00001      0.98818      0.99407      0.00000      0.00000
  4     0.01172       0.01172      0.98685      0.00001      0.97387      0.98685      0.00000      0.00000
  5     0.01563       0.01563      0.97643      0.00006      0.95342      0.97643      0.00000      0.00000
  6     0.01953       0.01953      0.96350      0.00007      0.92834      0.96350      0.00000      0.00000
  7     0.02344       0.02344      0.94752      0.00017      0.89780      0.94752      0.00000      0.00000
  8     0.02734       0.02734      0.92921      0.00021      0.86342      0.92921      0.00000      0.00000
  9     0.03125       0.03125      0.90806      0.00037      0.82457      0.90806      0.00000      0.00000
 10     0.03516       0.03516      0.88481      0.00046      0.78289      0.88481      0.00000      0.00000
 11     0.03906       0.03906      0.85901      0.00069      0.73789      0.85901      0.00000      0.00000
 12     0.04297       0.04297      0.83140      0.00084      0.69122      0.83140      0.00102      0.00376
 13     0.04688       0.04688      0.80156      0.00116      0.64251      0.80157      0.00144      0.00490
 14     0.05078       0.05078      0.77026      0.00137      0.59331      0.77026      0.00178      0.00559
 15     0.05469       0.05469      0.73711      0.00178      0.54334      0.73712      0.00241      0.00701
 16     0.05859       0.05859      0.70287      0.00206      0.49402      0.70287      0.00294      0.00798
 17     0.06250       0.06250      0.66718      0.00256      0.44513      0.66718      0.00383      0.00976
 18     0.06641       0.06641      0.63079      0.00292      0.39790      0.63079      0.00462      0.01108
 19     0.07031       0.07031      0.59338      0.00350      0.35211      0.59339      0.00590      0.01336
 20     0.07422       0.07422      0.55568      0.00393      0.30880      0.55570      0.00707      0.01517
 21     0.07813       0.07813      0.51740      0.00460      0.26772      0.51742      0.00889      0.01811
 22     0.08203       0.08203      0.47922      0.00509      0.22968      0.47925      0.01062      0.02060
 23     0.08594       0.08594      0.44090      0.00583      0.19442      0.44093      0.01323      0.02451
 24     0.08984       0.08984      0.40307      0.00638      0.16250      0.40312      0.01582      0.02802
 25     0.09375       0.09375      0.36549      0.00718      0.13364      0.36556      0.01965      0.03335
 26     0.09766       0.09766      0.32878      0.00776      0.10816      0.32888      0.02359      0.03845
 27     0.10156       0.10156      0.29271      0.00861      0.08576      0.29284      0.02940      0.04607
 28     0.10547       0.10547      0.25783      0.00920      0.06656      0.25800      0.03567      0.05382
 29     0.10938       0.10938      0.22394      0.01007      0.05025      0.22416      0.04494      0.06540
 30     0.11328       0.11328      0.19151      0.01066      0.03679      0.19181      0.05559      0.07811
 31     0.11719       0.11719      0.16037      0.01153      0.02585      0.16078      0.07174      0.09744
 32     0.12109       0.12109      0.13093      0.01208      0.01729      0.13148      0.09200      0.12092
 33     0.12500       0.12500      0.10300      0.01292      0.01078      0.10381      0.12474      0.15882
 34     0.12891       0.12891      0.07695      0.01341      0.00610      0.07811      0.17253      0.21302
 35     0.13281       0.13281      0.05261      0.01418      0.00297      0.05449      0.26336      0.31560
 36     0.13672       0.13672      0.03024      0.01459      0.00113      0.03358      0.44950      0.52326
 37     0.14063       0.14063      0.00970      0.01527      0.00033      0.01809      1.00482      1.13722
 38     0.14453       0.14453     -0.00881      0.01556      0.00032      0.01788      2.08602      2.29708
 39     0.14844       0.14844     -0.02544      0.01612      0.00091      0.03012      2.57687      2.76293
 40     0.15234       0.15234     -0.04006      0.01626      0.00187      0.04323      2.75608      2.87930
 41     0.15625       0.15625     -0.05280      0.01666      0.00307      0.05537      2.83591      2.88863
 42     0.16016       0.16016     -0.06361      0.01663      0.00432      0.06575      2.88591      2.86786
 43     0.16406       0.16406     -0.07259      0.01685      0.00555      0.07452      2.91349      2.82634
 44     0.16797       0.16797     -0.07978      0.01662      0.00664      0.08150      2.93624      2.78217
 45     0.17188       0.17188     -0.08526      0.01664      0.00755      0.08686      2.94887      2.73063
 46     0.17578       0.17578     -0.08912      0.01618      0.00820      0.09058      2.96196      2.68180
 47     0.17969       0.17969     -0.09142      0.01598      0.00861      0.09280      2.96856      2.62935
 48     0.18359       0.18359     -0.09233      0.01529      0.00876      0.09358      2.97750      2.58115
 49     0.18750       0.18750     -0.09186      0.01484      0.00866      0.09305      2.98139      2.53068
 50     0.19141       0.19141     -0.09027      0.01391      0.00834      0.09133      2.98870      2.48511
 51     0.19531       0.19531     -0.08751      0.01322      0.00783      0.08850      2.99166      2.43782
 52     0.19922       0.19922     -0.08391      0.01204      0.00719      0.08476      2.99906      2.39594
 53     0.20313       0.20313     -0.07936      0.01110      0.00642      0.08014      3.00257      2.35261
 54     0.20703       0.20703     -0.07427      0.00969      0.00561      0.07490      3.01191      2.31540
 55     0.21094       0.21094     -0.06847      0.00851      0.00476      0.06900      3.01791      2.27705
 56     0.21484       0.21484     -0.06241      0.00687      0.00394      0.06279      3.03200      2.24609
 57     0.21875       0.21875     -0.05589      0.00547      0.00315      0.05615      3.04395      2.21467
 58     0.22266       0.22266     -0.04938      0.00363      0.00245      0.04951      3.06827      2.19320
 59     0.22656       0.22656     -0.04262      0.00204      0.00182      0.04267      3.09379      2.17331
 60     0.23047       0.23047     -0.03615      0.00002      0.00131      0.03615      3.14159      2.16949
 61     0.23438       0.23438     -0.02962     -0.00173      0.00088      0.02967     -3.08338     -2.09380
 62     0.23828       0.23828     -0.02362     -0.00387      0.00057      0.02394     -2.97907     -1.98981
 63     0.24219       0.24219     -0.01773     -0.00574      0.00035      0.01864     -2.82853     -1.85879
 64     0.24609       0.24609     -0.01258     -0.00797      0.00022      0.01489     -2.57681     -1.66649
 65     0.25000       0.25000     -0.00766     -0.00990      0.00016      0.01252     -2.22887     -1.41894
 66     0.25391       0.25391     -0.00364     -0.01216      0.00016      0.01269     -1.86172     -1.16698
 67     0.25781       0.25781      0.00005     -0.01411      0.00020      0.01411     -1.56712     -0.96743
 68     0.26172       0.26172      0.00271     -0.01633      0.00027      0.01656     -1.40621     -0.85513
 69     0.26563       0.26563      0.00501     -0.01823      0.00036      0.01891     -1.30279     -0.78060
 70     0.26953       0.26953      0.00619     -0.02036      0.00045      0.02128     -1.27579     -0.75334
 71     0.27344       0.27344      0.00700     -0.02215      0.00054      0.02323     -1.26471     -0.73613
 72     0.27734       0.27734      0.00666     -0.02412      0.00063      0.02502     -1.30131     -0.74676
 73     0.28125       0.28125      0.00600     -0.02573      0.00070      0.02642     -1.34180     -0.75930
 74     0.28516       0.28516      0.00419     -0.02747      0.00077      0.02779     -1.41954     -0.79229
 75     0.28906       0.28906      0.00214     -0.02885      0.00084      0.02893     -1.49688     -0.82417
 76     0.29297       0.29297     -0.00102     -0.03029      0.00092      0.03031     -1.60446     -0.87162
 77     0.29688       0.29688     -0.00429     -0.03138      0.00100      0.03167     -1.70681     -0.91502
 78     0.30078       0.30078     -0.00860     -0.03247      0.00113      0.03359     -1.82965     -0.96814
 79     0.30469       0.30469     -0.01287     -0.03321      0.00127      0.03562     -1.94046     -1.01361
 80     0.30859       0.30859     -0.01806     -0.03390      0.00148      0.03841     -2.06037     -1.06262
 81     0.31250       0.31250     -0.02305     -0.03425      0.00170      0.04128     -2.16317     -1.10170
 82     0.31641       0.31641     -0.02883     -0.03448      0.00202      0.04495     -2.26711     -1.14038
 83     0.32031       0.32031     -0.03421     -0.03441      0.00235      0.04852     -2.35337     -1.16933
 84     0.32422       0.32422     -0.04025     -0.03417      0.00279      0.05280     -2.43779     -1.19668
 85     0.32813       0.32813     -0.04569     -0.03364      0.00322      0.05674     -2.50701     -1.21601
 86     0.33203       0.33203     -0.05164     -0.03290      0.00375      0.06123     -2.57439     -1.23400
 87     0.33594       0.33594     -0.05680     -0.03191      0.00424      0.06515     -2.62971     -1.24586
 88     0.33984       0.33984     -0.06232     -0.03067      0.00482      0.06946     -2.68428     -1.25710
 89     0.34375       0.34375     -0.06685     -0.02923      0.00532      0.07296     -2.72944     -1.26372
 90     0.34766       0.34766     -0.07162     -0.02749      0.00589      0.07672     -2.77511     -1.27043
 91     0.35156       0.35156     -0.07523     -0.02562      0.00632      0.07947     -2.81337     -1.27363
 92     0.35547       0.35547     -0.07896     -0.02341      0.00678      0.08235     -2.85333     -1.27753
 93     0.35938       0.35938     -0.08137     -0.02115      0.00707      0.08407     -2.88733     -1.27870
 94     0.36328       0.36328     -0.08381     -0.01851      0.00737      0.08583     -2.92421     -1.28111
 95     0.36719       0.36719     -0.08482     -0.01591      0.00745      0.08630     -2.95622     -1.28135
 96     0.37109       0.37109     -0.08580     -0.01290      0.00753      0.08676     -2.99238     -1.28337
 97     0.37500       0.37500     -0.08525     -0.01002      0.00737      0.08583     -3.02459     -1.28368
 98     0.37891       0.37891     -0.08464     -0.00671      0.00721      0.08490     -3.06252     -1.28638
 99     0.38281       0.38281     -0.08245     -0.00364      0.00681      0.08253     -3.09750     -1.28779
100     0.38672       0.38672     -0.08020     -0.00010      0.00643      0.08020     -3.14037     -1.29242
101     0.39063       0.39063     -0.07636      0.00307      0.00584      0.07642      3.10143      1.26364
102     0.39453       0.39453     -0.07251      0.00674      0.00530      0.07282      3.04886      1.22992
103     0.39844       0.39844     -0.06708      0.00991      0.00460      0.06781      2.99498      1.19634
104     0.40234       0.40234     -0.06172      0.01362      0.00399      0.06321      2.92436      1.15679
105     0.40625       0.40625     -0.05484      0.01667      0.00329      0.05732      2.84645      1.11514
106     0.41016       0.41016     -0.04813      0.02033      0.00273      0.05225      2.74192      1.06396
107     0.41406       0.41406     -0.04000      0.02316      0.00214      0.04622      2.61681      1.00583
108     0.41797       0.41797     -0.03217      0.02666      0.00175      0.04178      2.44961      0.93277
109     0.42188       0.42188     -0.02304      0.02915      0.00138      0.03715      2.23969      0.84494
110     0.42578       0.42578     -0.01438      0.03241      0.00126      0.03545      1.98836      0.74324
111     0.42969       0.42969     -0.00455      0.03444      0.00121      0.03474      1.70204      0.63043
112     0.43359       0.43359      0.00463      0.03738      0.00142      0.03767      1.44745      0.53130
113     0.43750       0.43750      0.01483      0.03886      0.00173      0.04159      1.20626      0.43882
114     0.44141       0.44141      0.02418      0.04140      0.00230      0.04795      1.04217      0.37577
115     0.44531       0.44531      0.03438      0.04223      0.00297      0.05445      0.88739      0.31715
116     0.44922       0.44922      0.04356      0.04434      0.00386      0.06215      0.79428      0.28141
117     0.45313       0.45313      0.05341      0.04440      0.00482      0.06945      0.69360      0.24362
118     0.45703       0.45703      0.06204      0.04607      0.00597      0.07728      0.63871      0.22242
119     0.46094       0.46094      0.07119      0.04528      0.00712      0.08437      0.56650      0.19560
120     0.46484       0.46484      0.07895      0.04654      0.00840      0.09165      0.53260      0.18235
121     0.46875       0.46875      0.08708      0.04478      0.00959      0.09792      0.47497      0.16127
122     0.47266       0.47266      0.09366      0.04573      0.01086      0.10423      0.45423      0.15295
123     0.47656       0.47656      0.10047      0.04282      0.01193      0.10922      0.40292      0.13456
124     0.48047       0.48047      0.10561      0.04374      0.01307      0.11431      0.39270      0.13008
125     0.48438       0.48438      0.11087      0.03930      0.01384      0.11763      0.34064      0.11193
126     0.48828       0.48828      0.11436      0.04093      0.01475      0.12146      0.34367      0.11202
127     0.49219       0.49219      0.11789      0.03357      0.01502      0.12257      0.27742      0.08971
128     0.49609       0.49609      0.11960      0.03963      0.01588      0.12600      0.31995      0.10265
129     0.50000       0.50000      0.12113      0.00000      0.01467      0.12113      0.00000      0.00000
NUMBER POINTS OUTPUT =         129
**** This image is ALIASED ****
INTEGRATED MTF AMPLITUDE FROM .25 TO .48 =       3.40253
if (mode = "nobatch" or mode = "inter")
end-if
copy test3.img test3_r.img sl=2 ss=1 nl=1 ns=124
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
size test3_r.img test3_124r.img 'noin lzoom=124 szoom=1
Beginning VICAR task size
 SIZE version 22 Aug 2013 (64-bit) - rjb
      INPUT AREA=(    1,    1,    1,  124)
     OUTPUT SIZE=    124 X    124
 PICTURE SIZE SCALED BY    124*NL,      1*NS
 SIZE task completed
polarect test3_124r.img test3_cr.img  'AUTO XCEN=62 YCEN=62
Beginning VICAR task polarect
POLARECT version 27-Oct-2010
WORK CORE AVAILABLE= 997952 BYTES
 INPUT IMAGE STATISTICS FOR XCEN=  62.000, YCEN=  62.000
  SIZE: NL= 124 NS= 124
  ANGULAR RANGE: PHIMIN=  0.0000, PHIMAX=360.0000
  RADIAL RANGE: RMIN=   0.000, RMAX=  87.681
  OPTIMAL ANGULAR RESOLUTION= 0.65343 YIELDS  552 LINES
  OPTIMAL RADIAL SCALE=   1.000 YIELDS   89  SAMPLE
OUTPUT IMAGE STATISTICS
  SIZE: NL= 124 NS= 124
  ANGULAR RANGE: PHIMIN=  0.0000, PHIMAX=360.0000
  RADIAL RANGE: RMIN=   0.000, RMAX=  87.681
          ANGULAR RESOLUTION= 2.92683
          RADIAL SCALE= 0.71286
VICAR HISTORY LABEL WRITTEN:
         PHIMIN=   0.00000 AT LINE 1, PHIMAX= 360.00000 AT LINE 124
         RMIN=   0.00000 AT SAMP 1, RMAX=  87.68124 AT SAMP 124
         CENTER OF TRANSFORMATION XCEN=   62.000, YCEN=   62.000
 124 INPUT LINES COMPLETE
otf1 out=test4.img esf=(0,0,0,0,24,48,72,96,120,144,168,192,216,216,216,216)  +
    plotout=test4 plotprof=test4 proftab=test4 'noprint
Beginning VICAR task otf1
OTF1 version 2015-08-10
NUMBER OF LINES PROCESSED=          1
NUMBER POINTS OUTPUT =         129
**** This image is ALIASED ****
INTEGRATED MTF AMPLITUDE FROM .25 TO .48 =       4.94845
if (mode = "nobatch" or mode = "inter")
end-if
otf1 table=test5.tbl columns=nocolhdr plotprof=test5 proftab=test5  +
  plotout=test5  +
  esf=(100,100,100,100,99,90,66,33,20,18,17,17,17,17,17,17)
Beginning VICAR task otf1
OTF1 version 2015-08-10
NUMBER OF LINES PROCESSED=          1

 PIXEL  CYCLES/      FREQUENCY    REAL(FT)     IMAG.(FT)     INTENSITY    AMPLITUDE     PHASE     WAVE LENGT
         PIXEL                                                                         RADIANS      SHIFT
  1     0.00000       0.00000      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
  2     0.00391       0.00391      0.99991     -0.00001      0.99982      0.99991      0.00000      0.00000
  3     0.00781       0.00781      0.99867      0.00002      0.99735      0.99867      0.00000      0.00000
  4     0.01172       0.01172      0.99726     -0.00001      0.99452      0.99726      0.00000      0.00000
  5     0.01563       0.01563      0.99471      0.00006      0.98944      0.99471      0.00000      0.00000
  6     0.01953       0.01953      0.99198      0.00003      0.98402      0.99198      0.00000      0.00000
  7     0.02344       0.02344      0.98814      0.00016      0.97642      0.98814      0.00000      0.00000
  8     0.02734       0.02734      0.98412      0.00013      0.96848      0.98412      0.00000      0.00000
  9     0.03125       0.03125      0.97902      0.00032      0.95849      0.97902      0.00000      0.00000
 10     0.03516       0.03516      0.97375      0.00031      0.94818      0.97375      0.00000      0.00000
 11     0.03906       0.03906      0.96745      0.00058      0.93595      0.96745      0.00000      0.00000
 12     0.04297       0.04297      0.96096      0.00059      0.92344      0.96096      0.00000      0.00000
 13     0.04688       0.04688      0.95351      0.00094      0.90918      0.95351      0.00000      0.00000
 14     0.05078       0.05078      0.94587      0.00098      0.89466      0.94587      0.00104      0.00326
 15     0.05469       0.05469      0.93734      0.00142      0.87860      0.93734      0.00151      0.00439
 16     0.05859       0.05859      0.92860      0.00150      0.86230      0.92860      0.00162      0.00440
 17     0.06250       0.06250      0.91906      0.00202      0.84468      0.91907      0.00219      0.00559
 18     0.06641       0.06641      0.90932      0.00215      0.82686      0.90932      0.00236      0.00566
 19     0.07031       0.07031      0.89885      0.00274      0.80794      0.89886      0.00305      0.00691
 20     0.07422       0.07422      0.88817      0.00292      0.78885      0.88818      0.00329      0.00705
 21     0.07813       0.07813      0.87687      0.00359      0.76891      0.87688      0.00409      0.00833
 22     0.08203       0.08203      0.86534      0.00381      0.74883      0.86535      0.00440      0.00854
 23     0.08594       0.08594      0.85329      0.00454      0.72813      0.85331      0.00532      0.00985
 24     0.08984       0.08984      0.84101      0.00480      0.70732      0.84102      0.00571      0.01011
 25     0.09375       0.09375      0.82831      0.00558      0.68614      0.82833      0.00674      0.01144
 26     0.09766       0.09766      0.81537      0.00587      0.66486      0.81539      0.00720      0.01174
 27     0.10156       0.10156      0.80212      0.00669      0.64345      0.80215      0.00834      0.01307
 28     0.10547       0.10547      0.78862      0.00701      0.62196      0.78865      0.00889      0.01341
 29     0.10938       0.10938      0.77492      0.00785      0.60056      0.77496      0.01012      0.01473
 30     0.11328       0.11328      0.76095      0.00817      0.57911      0.76099      0.01074      0.01509
 31     0.11719       0.11719      0.74690      0.00901      0.55794      0.74695      0.01206      0.01638
 32     0.12109       0.12109      0.73256      0.00934      0.53673      0.73262      0.01274      0.01675
 33     0.12500       0.12500      0.71826      0.01015      0.51600      0.71833      0.01413      0.01799
 34     0.12891       0.12891      0.70364      0.01046      0.49522      0.70372      0.01486      0.01835
 35     0.13281       0.13281      0.68918      0.01122      0.47509      0.68927      0.01628      0.01951
 36     0.13672       0.13672      0.67439      0.01150      0.45493      0.67449      0.01705      0.01985
 37     0.14063       0.14063      0.65985      0.01219      0.43555      0.65996      0.01848      0.02091
 38     0.14453       0.14453      0.64497      0.01242      0.41614      0.64509      0.01926      0.02121
 39     0.14844       0.14844      0.63044      0.01302      0.39763      0.63058      0.02065      0.02214
 40     0.15234       0.15234      0.61556      0.01318      0.37909      0.61570      0.02141      0.02237
 41     0.15625       0.15625      0.60112      0.01366      0.36154      0.60128      0.02273      0.02315
 42     0.16016       0.16016      0.58632      0.01374      0.34396      0.58648      0.02343      0.02328
 43     0.16406       0.16406      0.57204      0.01408      0.32743      0.57221      0.02461      0.02388
 44     0.16797       0.16797      0.55738      0.01406      0.31087      0.55756      0.02522      0.02390
 45     0.17188       0.17188      0.54333      0.01424      0.29541      0.54351      0.02620      0.02426
 46     0.17578       0.17578      0.52889      0.01410      0.27992      0.52907      0.02666      0.02414
 47     0.17969       0.17969      0.51511      0.01410      0.26554      0.51530      0.02737      0.02424
 48     0.18359       0.18359      0.50094      0.01384      0.25114      0.50114      0.02762      0.02394
 49     0.18750       0.18750      0.48750      0.01364      0.23784      0.48769      0.02798      0.02375
 50     0.19141       0.19141      0.47366      0.01325      0.22453      0.47385      0.02796      0.02325
 51     0.19531       0.19531      0.46059      0.01284      0.21231      0.46077      0.02787      0.02271
 52     0.19922       0.19922      0.44712      0.01230      0.20007      0.44729      0.02750      0.02197
 53     0.20313       0.20313      0.43445      0.01167      0.18888      0.43461      0.02686      0.02105
 54     0.20703       0.20703      0.42139      0.01098      0.17769      0.42153      0.02606      0.02003
 55     0.21094       0.21094      0.40915      0.01013      0.16751      0.40928      0.02476      0.01868
 56     0.21484       0.21484      0.39653      0.00930      0.15732      0.39664      0.02344      0.01737
 57     0.21875       0.21875      0.38474      0.00822      0.14809      0.38483      0.02137      0.01555
 58     0.22266       0.22266      0.37258      0.00724      0.13887      0.37265      0.01943      0.01389
 59     0.22656       0.22656      0.36125      0.00594      0.13054      0.36130      0.01645      0.01156
 60     0.23047       0.23047      0.34957      0.00482      0.12222      0.34960      0.01378      0.00952
 61     0.23438       0.23438      0.33870      0.00331      0.11473      0.33872      0.00977      0.00663
 62     0.23828       0.23828      0.32752      0.00205      0.10727      0.32752      0.00627      0.00418
 63     0.24219       0.24219      0.31712      0.00034      0.10056      0.31712      0.00107      0.00070
 64     0.24609       0.24609      0.30643     -0.00104      0.09390      0.30643     -0.00338     -0.00219
 65     0.25000       0.25000      0.29649     -0.00293      0.08792      0.29651     -0.00990     -0.00630
 66     0.25391       0.25391      0.28630     -0.00441      0.08199      0.28634     -0.01542     -0.00966
 67     0.25781       0.25781      0.27682     -0.00648      0.07667      0.27689     -0.02340     -0.01444
 68     0.26172       0.26172      0.26713     -0.00804      0.07142      0.26725     -0.03011     -0.01831
 69     0.26563       0.26563      0.25808     -0.01025      0.06671      0.25829     -0.03970     -0.02379
 70     0.26953       0.26953      0.24889     -0.01188      0.06209      0.24918     -0.04771     -0.02817
 71     0.27344       0.27344      0.24027     -0.01421      0.05793      0.24069     -0.05905     -0.03437
 72     0.27734       0.27734      0.23157     -0.01588      0.05388      0.23211     -0.06847     -0.03929
 73     0.28125       0.28125      0.22335     -0.01829      0.05022      0.22410     -0.08171     -0.04624
 74     0.28516       0.28516      0.21513     -0.01998      0.04668      0.21606     -0.09263     -0.05170
 75     0.28906       0.28906      0.20731     -0.02245      0.04348      0.20852     -0.10787     -0.05939
 76     0.29297       0.29297      0.19956     -0.02414      0.04041      0.20101     -0.12038     -0.06540
 77     0.29688       0.29688      0.19210     -0.02663      0.03761      0.19394     -0.13773     -0.07384
 78     0.30078       0.30078      0.18482     -0.02829      0.03496      0.18697     -0.15189     -0.08037
 79     0.30469       0.30469      0.17772     -0.03076      0.03253      0.18036     -0.17141     -0.08953
 80     0.30859       0.30859      0.17088     -0.03237      0.03025      0.17392     -0.18723     -0.09656
 81     0.31250       0.31250      0.16412     -0.03480      0.02815      0.16777     -0.20896     -0.10642
 82     0.31641       0.31641      0.15772     -0.03634      0.02620      0.16186     -0.22644     -0.11390
 83     0.32031       0.32031      0.15129     -0.03869      0.02438      0.15616     -0.25037     -0.12440
 84     0.32422       0.32422      0.14532     -0.04013      0.02273      0.15076     -0.26942     -0.13225
 85     0.32813       0.32813      0.13919     -0.04237      0.02117      0.14550     -0.29548     -0.14332
 86     0.33203       0.33203      0.13365     -0.04369      0.01977      0.14061     -0.31597     -0.15145
 87     0.33594       0.33594      0.12782     -0.04579      0.01844      0.13578     -0.34402     -0.16298
 88     0.33984       0.33984      0.12269     -0.04699      0.01726      0.13138     -0.36573     -0.17128
 89     0.34375       0.34375      0.11715     -0.04892      0.01612      0.12695     -0.39556     -0.18314
 90     0.34766       0.34766      0.11243     -0.04997      0.01514      0.12304     -0.41823     -0.19146
 91     0.35156       0.35156      0.10717     -0.05171      0.01416      0.11899     -0.44955     -0.20351
 92     0.35547       0.35547      0.10285     -0.05261      0.01335      0.11553     -0.47281     -0.21169
 93     0.35938       0.35938      0.09786     -0.05413      0.01251      0.11183     -0.50526     -0.22376
 94     0.36328       0.36328      0.09394     -0.05488      0.01184      0.10880     -0.52869     -0.23162
 95     0.36719       0.36719      0.08922     -0.05616      0.01111      0.10542     -0.56184     -0.24353
 96     0.37109       0.37109      0.08570     -0.05676      0.01057      0.10279     -0.58498     -0.25089
 97     0.37500       0.37500      0.08124     -0.05780      0.00994      0.09970     -0.61837     -0.26244
 98     0.37891       0.37891      0.07811     -0.05824      0.00949      0.09743     -0.64073     -0.26913
 99     0.38281       0.38281      0.07391     -0.05902      0.00895      0.09458     -0.67385     -0.28015
100     0.38672       0.38672      0.07117     -0.05933      0.00859      0.09266     -0.69493     -0.28600
101     0.39063       0.39063      0.06723     -0.05983      0.00810      0.09000     -0.72728     -0.29632
102     0.39453       0.39453      0.06487     -0.06003      0.00781      0.08839     -0.74663     -0.30119
103     0.39844       0.39844      0.06119     -0.06026      0.00738      0.08588     -0.77770     -0.31065
104     0.40234       0.40234      0.05922     -0.06036      0.00715      0.08456     -0.79492     -0.31444
105     0.40625       0.40625      0.05580     -0.06031      0.00675      0.08216     -0.82424     -0.32291
106     0.41016       0.41016      0.05420     -0.06034      0.00658      0.08111     -0.83897     -0.32555
107     0.41406       0.41406      0.05103     -0.06001      0.00621      0.07878     -0.86610     -0.33291
108     0.41797       0.41797      0.04980     -0.06001      0.00608      0.07798     -0.87812     -0.33437
109     0.42188       0.42188      0.04689     -0.05941      0.00573      0.07568     -0.90260     -0.34051
110     0.42578       0.42578      0.04602     -0.05941      0.00565      0.07515     -0.91178     -0.34082
111     0.42969       0.42969      0.04336     -0.05853      0.00531      0.07284     -0.93320     -0.34566
112     0.43359       0.43359      0.04283     -0.05859      0.00527      0.07258     -0.93957     -0.34488
113     0.43750       0.43750      0.04042     -0.05742      0.00493      0.07022     -0.95747     -0.34831
114     0.44141       0.44141      0.04022     -0.05761      0.00494      0.07026     -0.96124     -0.34659
115     0.44531       0.44531      0.03805     -0.05614      0.00460      0.06782     -0.97512     -0.34851
116     0.44922       0.44922      0.03817     -0.05651      0.00465      0.06820     -0.97674     -0.34605
117     0.45313       0.45313      0.03623     -0.05472      0.00431      0.06563     -0.98598     -0.34631
118     0.45703       0.45703      0.03665     -0.05539      0.00441      0.06641     -0.98629     -0.34346
119     0.46094       0.46094      0.03493     -0.05322      0.00405      0.06365     -0.99001     -0.34184
120     0.46484       0.46484      0.03561     -0.05431      0.00422      0.06494     -0.99041     -0.33910
121     0.46875       0.46875      0.03410     -0.05164      0.00383      0.06188     -0.98725     -0.33520
122     0.47266       0.47266      0.03503     -0.05339      0.00408      0.06386     -0.99016     -0.33341
123     0.47656       0.47656      0.03370     -0.04998      0.00363      0.06028     -0.97752     -0.32646
124     0.48047       0.48047      0.03485     -0.05282      0.00400      0.06328     -0.98758     -0.32713
125     0.48438       0.48438      0.03369     -0.04805      0.00344      0.05869     -0.95928     -0.31520
126     0.48828       0.48828      0.03503     -0.05313      0.00405      0.06364     -0.98790     -0.32201
127     0.49219       0.49219      0.03402     -0.04482      0.00317      0.05627     -0.92156     -0.29800
128     0.49609       0.49609      0.03551     -0.05790      0.00461      0.06792     -1.02061     -0.32743
129     0.50000       0.50000      0.03455      0.00000      0.00119      0.03455      0.00000      0.00000
NUMBER POINTS OUTPUT =         129
**** This image is ALIASED ****
INTEGRATED MTF AMPLITUDE FROM .25 TO .48 =       7.93121
if (mode = "nobatch" or mode = "inter")
end-if
otf1 out=test6.img lsf=(0,8,10,14,16,18,22,24,21,19,16,13,11,8)  +
   plotout=test6 'noprint
Beginning VICAR task otf1
OTF1 version 2015-08-10
NUMBER OF LINES PROCESSED=          1
NUMBER POINTS OUTPUT =         129
**** This image is ALIASED ****
INTEGRATED MTF AMPLITUDE FROM .25 TO .48 =       1.50718
if (mode = "nobatch" or mode = "inter")
end-if
otf1 inp=edge.img
Beginning VICAR task otf1
OTF1 version 2015-08-10
NUMBER OF LINES PROCESSED=         40

 PIXEL  CYCLES/      FREQUENCY    REAL(FT)     IMAG.(FT)     INTENSITY    AMPLITUDE     PHASE     WAVE LENGT
         PIXEL                                                                         RADIANS      SHIFT
  1     0.00000       0.00000      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
  2     0.00391       0.00391      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
  3     0.00781       0.00781      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
  4     0.01172       0.01172      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
  5     0.01563       0.01563      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
  6     0.01953       0.01953      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
  7     0.02344       0.02344      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
  8     0.02734       0.02734      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
  9     0.03125       0.03125      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 10     0.03516       0.03516      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 11     0.03906       0.03906      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 12     0.04297       0.04297      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 13     0.04688       0.04688      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 14     0.05078       0.05078      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 15     0.05469       0.05469      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 16     0.05859       0.05859      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 17     0.06250       0.06250      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 18     0.06641       0.06641      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 19     0.07031       0.07031      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 20     0.07422       0.07422      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 21     0.07813       0.07813      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 22     0.08203       0.08203      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 23     0.08594       0.08594      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 24     0.08984       0.08984      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 25     0.09375       0.09375      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 26     0.09766       0.09766      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 27     0.10156       0.10156      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 28     0.10547       0.10547      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 29     0.10938       0.10938      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 30     0.11328       0.11328      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 31     0.11719       0.11719      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 32     0.12109       0.12109      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 33     0.12500       0.12500      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 34     0.12891       0.12891      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 35     0.13281       0.13281      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 36     0.13672       0.13672      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 37     0.14063       0.14063      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 38     0.14453       0.14453      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 39     0.14844       0.14844      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 40     0.15234       0.15234      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 41     0.15625       0.15625      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 42     0.16016       0.16016      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 43     0.16406       0.16406      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 44     0.16797       0.16797      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 45     0.17188       0.17188      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 46     0.17578       0.17578      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 47     0.17969       0.17969      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 48     0.18359       0.18359      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 49     0.18750       0.18750      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 50     0.19141       0.19141      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 51     0.19531       0.19531      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 52     0.19922       0.19922      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 53     0.20313       0.20313      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 54     0.20703       0.20703      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 55     0.21094       0.21094      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 56     0.21484       0.21484      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 57     0.21875       0.21875      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 58     0.22266       0.22266      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 59     0.22656       0.22656      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 60     0.23047       0.23047      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 61     0.23438       0.23438      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 62     0.23828       0.23828      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 63     0.24219       0.24219      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 64     0.24609       0.24609      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 65     0.25000       0.25000      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 66     0.25391       0.25391      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 67     0.25781       0.25781      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 68     0.26172       0.26172      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 69     0.26563       0.26563      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 70     0.26953       0.26953      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 71     0.27344       0.27344      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 72     0.27734       0.27734      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 73     0.28125       0.28125      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 74     0.28516       0.28516      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 75     0.28906       0.28906      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 76     0.29297       0.29297      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 77     0.29688       0.29688      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 78     0.30078       0.30078      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 79     0.30469       0.30469      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 80     0.30859       0.30859      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 81     0.31250       0.31250      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 82     0.31641       0.31641      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 83     0.32031       0.32031      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 84     0.32422       0.32422      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 85     0.32813       0.32813      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 86     0.33203       0.33203      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 87     0.33594       0.33594      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 88     0.33984       0.33984      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 89     0.34375       0.34375      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 90     0.34766       0.34766      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 91     0.35156       0.35156      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 92     0.35547       0.35547      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 93     0.35938       0.35938      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 94     0.36328       0.36328      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 95     0.36719       0.36719      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 96     0.37109       0.37109      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 97     0.37500       0.37500      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 98     0.37891       0.37891      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 99     0.38281       0.38281      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
100     0.38672       0.38672      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
101     0.39063       0.39063      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
102     0.39453       0.39453      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
103     0.39844       0.39844      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
104     0.40234       0.40234      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
105     0.40625       0.40625      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
106     0.41016       0.41016      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
107     0.41406       0.41406      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
108     0.41797       0.41797      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
109     0.42188       0.42188      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
110     0.42578       0.42578      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
111     0.42969       0.42969      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
112     0.43359       0.43359      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
113     0.43750       0.43750      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
114     0.44141       0.44141      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
115     0.44531       0.44531      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
116     0.44922       0.44922      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
117     0.45313       0.45313      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
118     0.45703       0.45703      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
119     0.46094       0.46094      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
120     0.46484       0.46484      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
121     0.46875       0.46875      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
122     0.47266       0.47266      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
123     0.47656       0.47656      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
124     0.48047       0.48047      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
125     0.48438       0.48438      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
126     0.48828       0.48828      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
127     0.49219       0.49219      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
128     0.49609       0.49609      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
129     0.50000       0.50000      1.00001      0.00000      1.00001      1.00001      0.00000      0.00000
NUMBER POINTS OUTPUT =         129
**** This image is ALIASED ****
INTEGRATED MTF AMPLITUDE FROM .25 TO .48 =      60.00000
 FREQUENCY         -1                                                0                                                +1
                    11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
   0.0000          A                                                 W                                                 R
   0.0039          A                                                IW                                                 R
   0.0078          A                                                IW                                                 R
   0.0117          A                                                 W                                                 R
   0.0156          A                                                 W                                                 R
   0.0195          A                                                IW                                                 R
   0.0234          A                                                 W                                                 R
   0.0273          A                                                IW                                                 R
   0.0312          A                                                 W                                                 R
   0.0351          A                                                IW                                                 R
   0.0390          A                                                IW                                                 R
   0.0429          A                                                IW                                                 R
   0.0468          A                                                 W                                                 R
   0.0507          A                                                IW                                                 R
   0.0546          A                                                 W                                                 R
   0.0585          A                                                IW                                                 R
   0.0625          A                                                 W                                                 R
   0.0664          A                                                IW                                                 R
   0.0703          A                                                 W                                                 R
   0.0742          A                                                IW                                                 R
   0.0781          A                                                 W                                                 R
   0.0820          A                                                IW                                                 R
   0.0859          A                                                 W                                                 R
   0.0898          A                                                IW                                                 R
   0.0937          A                                                 W                                                 R
   0.0976          A                                                IW                                                 R
   0.1015          A                                                 W                                                 R
   0.1054          A                                                IW                                                 R
   0.1093          A                                                 W                                                 R
   0.1132          A                                                IW                                                 R
   0.1171          A                                                 W                                                 R
   0.1210          A                                                IW                                                 R
   0.1250          A                                                 W                                                 R
   0.1289          A                                                IW                                                 R
   0.1328          A                                                 W                                                 R
   0.1367          A                                                IW                                                 R
   0.1406          A                                                 W                                                 R
   0.1445          A                                                IW                                                 R
   0.1484          A                                                 W                                                 R
   0.1523          A                                                IW                                                 R
   0.1562          A                                                 W                                                 R
   0.1601          A                                                IW                                                 R
   0.1640          A                                                 W                                                 R
   0.1679          A                                                IW                                                 R
   0.1718          A                                                 W                                                 R
   0.1757          A                                                IW                                                 R
   0.1796          A                                                 W                                                 R
   0.1835          A                                                IW                                                 R
   0.1875          A                                                 W                                                 R
   0.1914          A                                                IW                                                 R
   0.1953          A                                                 W                                                 R
   0.1992          A                                                IW                                                 R
   0.2031          A                                                 W                                                 R
   0.2070          A                                                IW                                                 R
   0.2109          A                                                 W                                                 R
   0.2148          A                                                IW                                                 R
   0.2187          A                                                 W                                                 R
   0.2226          A                                                IW                                                 R
   0.2265          A                                                 W                                                 R
   0.2304          A                                                IW                                                 R
   0.2343          A                                                 W                                                 R
   0.2382          A                                                IW                                                 R
   0.2421          A                                                 W                                                 R
   0.2460          A                                                IW                                                 R
   0.2500          A                                                 W                                                 R
   0.2539          A                                                IW                                                 R
   0.2578          A                                                 W                                                 R
   0.2617          A                                                IW                                                 R
   0.2656          A                                                 W                                                 R
   0.2695          A                                                IW                                                 R
   0.2734          A                                                 W                                                 R
   0.2773          A                                                IW                                                 R
   0.2812          A                                                 W                                                 R
   0.2851          A                                                IW                                                 R
   0.2890          A                                                 W                                                 R
   0.2929          A                                                IW                                                 R
   0.2968          A                                                 W                                                 R
   0.3007          A                                                 W                                                 R
   0.3046          A                                                IW                                                 R
   0.3085          A                                                IW                                                 R
   0.3125          A                                                 W                                                 R
   0.3164          A                                                 W                                                 R
   0.3203          A                                                 W                                                 R
   0.3242          A                                                IW                                                 R
   0.3281          A                                                IW                                                 R
   0.3320          A                                                 W                                                 R
   0.3359          A                                                IW                                                 R
   0.3398          A                                                 W                                                 R
   0.3437          A                                                IW                                                 R
   0.3476          A                                                 W                                                 R
   0.3515          A                                                 W                                                 R
   0.3554          A                                                 W                                                 R
   0.3593          A                                                IW                                                 R
   0.3632          A                                                 W                                                 R
   0.3671          A                                                IW                                                 R
   0.3710          A                                                 W                                                 R
   0.3750          A                                                IW                                                 R
   0.3789          A                                                 W                                                 R
   0.3828          A                                                 W                                                 R
   0.3867          A                                                IW                                                 R
   0.3906          A                                                IW                                                 R
   0.3945          A                                                 W                                                 R
   0.3984          A                                                IW                                                 R
   0.4023          A                                                 W                                                 R
   0.4062          A                                                IW                                                 R
   0.4101          A                                                 W                                                 R
   0.4140          A                                                 W                                                 R
   0.4179          A                                                 W                                                 R
   0.4218          A                                                IW                                                 R
   0.4257          A                                                 W                                                 R
   0.4296          A                                                IW                                                 R
   0.4335          A                                                IW                                                 R
   0.4375          A                                                IW                                                 R
   0.4414          A                                                 W                                                 R
   0.4453          A                                                IW                                                 R
   0.4492          A                                                IW                                                 R
   0.4531          A                                                IW                                                 R
   0.4570          A                                                 W                                                 R
   0.4609          A                                                IW                                                 R
   0.4648          A                                                 W                                                 R
   0.4687          A                                                IW                                                 R
   0.4726          A                                                 W                                                 R
   0.4765          A                                                 W                                                 R
   0.4804          A                                                 W                                                 R
   0.4843          A                                                IW                                                 R
   0.4882          A                                                 W                                                 R
   0.4921          A                                                IW                                                 R
   0.4960          A                                                IW                                                 R
   0.5000          A                                                 W                                                 R
otf1 inp=edge.img 'noprint
Beginning VICAR task otf1
OTF1 version 2015-08-10
NUMBER OF LINES PROCESSED=         40
NUMBER POINTS OUTPUT =         129
**** This image is ALIASED ****
INTEGRATED MTF AMPLITUDE FROM .25 TO .48 =      60.00000
 FREQUENCY         -1                                                0                                                +1
                    11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
   0.0000          A                                                 W                                                 R
   0.0039          A                                                IW                                                 R
   0.0078          A                                                IW                                                 R
   0.0117          A                                                 W                                                 R
   0.0156          A                                                 W                                                 R
   0.0195          A                                                IW                                                 R
   0.0234          A                                                 W                                                 R
   0.0273          A                                                IW                                                 R
   0.0312          A                                                 W                                                 R
   0.0351          A                                                IW                                                 R
   0.0390          A                                                IW                                                 R
   0.0429          A                                                IW                                                 R
   0.0468          A                                                 W                                                 R
   0.0507          A                                                IW                                                 R
   0.0546          A                                                 W                                                 R
   0.0585          A                                                IW                                                 R
   0.0625          A                                                 W                                                 R
   0.0664          A                                                IW                                                 R
   0.0703          A                                                 W                                                 R
   0.0742          A                                                IW                                                 R
   0.0781          A                                                 W                                                 R
   0.0820          A                                                IW                                                 R
   0.0859          A                                                 W                                                 R
   0.0898          A                                                IW                                                 R
   0.0937          A                                                 W                                                 R
   0.0976          A                                                IW                                                 R
   0.1015          A                                                 W                                                 R
   0.1054          A                                                IW                                                 R
   0.1093          A                                                 W                                                 R
   0.1132          A                                                IW                                                 R
   0.1171          A                                                 W                                                 R
   0.1210          A                                                IW                                                 R
   0.1250          A                                                 W                                                 R
   0.1289          A                                                IW                                                 R
   0.1328          A                                                 W                                                 R
   0.1367          A                                                IW                                                 R
   0.1406          A                                                 W                                                 R
   0.1445          A                                                IW                                                 R
   0.1484          A                                                 W                                                 R
   0.1523          A                                                IW                                                 R
   0.1562          A                                                 W                                                 R
   0.1601          A                                                IW                                                 R
   0.1640          A                                                 W                                                 R
   0.1679          A                                                IW                                                 R
   0.1718          A                                                 W                                                 R
   0.1757          A                                                IW                                                 R
   0.1796          A                                                 W                                                 R
   0.1835          A                                                IW                                                 R
   0.1875          A                                                 W                                                 R
   0.1914          A                                                IW                                                 R
   0.1953          A                                                 W                                                 R
   0.1992          A                                                IW                                                 R
   0.2031          A                                                 W                                                 R
   0.2070          A                                                IW                                                 R
   0.2109          A                                                 W                                                 R
   0.2148          A                                                IW                                                 R
   0.2187          A                                                 W                                                 R
   0.2226          A                                                IW                                                 R
   0.2265          A                                                 W                                                 R
   0.2304          A                                                IW                                                 R
   0.2343          A                                                 W                                                 R
   0.2382          A                                                IW                                                 R
   0.2421          A                                                 W                                                 R
   0.2460          A                                                IW                                                 R
   0.2500          A                                                 W                                                 R
   0.2539          A                                                IW                                                 R
   0.2578          A                                                 W                                                 R
   0.2617          A                                                IW                                                 R
   0.2656          A                                                 W                                                 R
   0.2695          A                                                IW                                                 R
   0.2734          A                                                 W                                                 R
   0.2773          A                                                IW                                                 R
   0.2812          A                                                 W                                                 R
   0.2851          A                                                IW                                                 R
   0.2890          A                                                 W                                                 R
   0.2929          A                                                IW                                                 R
   0.2968          A                                                 W                                                 R
   0.3007          A                                                 W                                                 R
   0.3046          A                                                IW                                                 R
   0.3085          A                                                IW                                                 R
   0.3125          A                                                 W                                                 R
   0.3164          A                                                 W                                                 R
   0.3203          A                                                 W                                                 R
   0.3242          A                                                IW                                                 R
   0.3281          A                                                IW                                                 R
   0.3320          A                                                 W                                                 R
   0.3359          A                                                IW                                                 R
   0.3398          A                                                 W                                                 R
   0.3437          A                                                IW                                                 R
   0.3476          A                                                 W                                                 R
   0.3515          A                                                 W                                                 R
   0.3554          A                                                 W                                                 R
   0.3593          A                                                IW                                                 R
   0.3632          A                                                 W                                                 R
   0.3671          A                                                IW                                                 R
   0.3710          A                                                 W                                                 R
   0.3750          A                                                IW                                                 R
   0.3789          A                                                 W                                                 R
   0.3828          A                                                 W                                                 R
   0.3867          A                                                IW                                                 R
   0.3906          A                                                IW                                                 R
   0.3945          A                                                 W                                                 R
   0.3984          A                                                IW                                                 R
   0.4023          A                                                 W                                                 R
   0.4062          A                                                IW                                                 R
   0.4101          A                                                 W                                                 R
   0.4140          A                                                 W                                                 R
   0.4179          A                                                 W                                                 R
   0.4218          A                                                IW                                                 R
   0.4257          A                                                 W                                                 R
   0.4296          A                                                IW                                                 R
   0.4335          A                                                IW                                                 R
   0.4375          A                                                IW                                                 R
   0.4414          A                                                 W                                                 R
   0.4453          A                                                IW                                                 R
   0.4492          A                                                IW                                                 R
   0.4531          A                                                IW                                                 R
   0.4570          A                                                 W                                                 R
   0.4609          A                                                IW                                                 R
   0.4648          A                                                 W                                                 R
   0.4687          A                                                IW                                                 R
   0.4726          A                                                 W                                                 R
   0.4765          A                                                 W                                                 R
   0.4804          A                                                 W                                                 R
   0.4843          A                                                IW                                                 R
   0.4882          A                                                 W                                                 R
   0.4921          A                                                IW                                                 R
   0.4960          A                                                IW                                                 R
   0.5000          A                                                 W                                                 R
otf1 inp=edge.img plotout=test9
Beginning VICAR task otf1
OTF1 version 2015-08-10
NUMBER OF LINES PROCESSED=         40

 PIXEL  CYCLES/      FREQUENCY    REAL(FT)     IMAG.(FT)     INTENSITY    AMPLITUDE     PHASE     WAVE LENGT
         PIXEL                                                                         RADIANS      SHIFT
  1     0.00000       0.00000      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
  2     0.00391       0.00391      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
  3     0.00781       0.00781      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
  4     0.01172       0.01172      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
  5     0.01563       0.01563      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
  6     0.01953       0.01953      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
  7     0.02344       0.02344      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
  8     0.02734       0.02734      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
  9     0.03125       0.03125      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 10     0.03516       0.03516      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 11     0.03906       0.03906      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 12     0.04297       0.04297      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 13     0.04688       0.04688      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 14     0.05078       0.05078      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 15     0.05469       0.05469      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 16     0.05859       0.05859      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 17     0.06250       0.06250      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 18     0.06641       0.06641      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 19     0.07031       0.07031      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 20     0.07422       0.07422      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 21     0.07813       0.07813      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 22     0.08203       0.08203      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 23     0.08594       0.08594      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 24     0.08984       0.08984      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 25     0.09375       0.09375      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 26     0.09766       0.09766      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 27     0.10156       0.10156      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 28     0.10547       0.10547      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 29     0.10938       0.10938      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 30     0.11328       0.11328      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 31     0.11719       0.11719      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 32     0.12109       0.12109      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 33     0.12500       0.12500      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 34     0.12891       0.12891      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 35     0.13281       0.13281      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 36     0.13672       0.13672      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 37     0.14063       0.14063      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 38     0.14453       0.14453      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 39     0.14844       0.14844      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 40     0.15234       0.15234      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 41     0.15625       0.15625      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 42     0.16016       0.16016      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 43     0.16406       0.16406      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 44     0.16797       0.16797      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 45     0.17188       0.17188      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 46     0.17578       0.17578      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 47     0.17969       0.17969      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 48     0.18359       0.18359      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 49     0.18750       0.18750      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 50     0.19141       0.19141      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 51     0.19531       0.19531      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 52     0.19922       0.19922      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 53     0.20313       0.20313      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 54     0.20703       0.20703      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 55     0.21094       0.21094      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 56     0.21484       0.21484      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 57     0.21875       0.21875      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 58     0.22266       0.22266      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 59     0.22656       0.22656      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 60     0.23047       0.23047      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 61     0.23438       0.23438      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 62     0.23828       0.23828      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 63     0.24219       0.24219      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 64     0.24609       0.24609      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 65     0.25000       0.25000      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 66     0.25391       0.25391      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 67     0.25781       0.25781      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 68     0.26172       0.26172      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 69     0.26563       0.26563      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 70     0.26953       0.26953      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 71     0.27344       0.27344      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 72     0.27734       0.27734      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 73     0.28125       0.28125      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 74     0.28516       0.28516      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 75     0.28906       0.28906      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 76     0.29297       0.29297      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 77     0.29688       0.29688      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 78     0.30078       0.30078      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 79     0.30469       0.30469      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 80     0.30859       0.30859      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 81     0.31250       0.31250      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 82     0.31641       0.31641      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 83     0.32031       0.32031      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 84     0.32422       0.32422      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 85     0.32813       0.32813      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 86     0.33203       0.33203      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 87     0.33594       0.33594      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 88     0.33984       0.33984      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 89     0.34375       0.34375      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 90     0.34766       0.34766      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 91     0.35156       0.35156      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 92     0.35547       0.35547      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 93     0.35938       0.35938      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 94     0.36328       0.36328      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 95     0.36719       0.36719      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 96     0.37109       0.37109      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 97     0.37500       0.37500      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 98     0.37891       0.37891      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 99     0.38281       0.38281      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
100     0.38672       0.38672      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
101     0.39063       0.39063      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
102     0.39453       0.39453      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
103     0.39844       0.39844      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
104     0.40234       0.40234      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
105     0.40625       0.40625      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
106     0.41016       0.41016      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
107     0.41406       0.41406      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
108     0.41797       0.41797      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
109     0.42188       0.42188      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
110     0.42578       0.42578      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
111     0.42969       0.42969      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
112     0.43359       0.43359      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
113     0.43750       0.43750      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
114     0.44141       0.44141      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
115     0.44531       0.44531      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
116     0.44922       0.44922      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
117     0.45313       0.45313      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
118     0.45703       0.45703      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
119     0.46094       0.46094      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
120     0.46484       0.46484      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
121     0.46875       0.46875      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
122     0.47266       0.47266      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
123     0.47656       0.47656      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
124     0.48047       0.48047      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
125     0.48438       0.48438      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
126     0.48828       0.48828      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
127     0.49219       0.49219      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
128     0.49609       0.49609      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
129     0.50000       0.50000      1.00001      0.00000      1.00001      1.00001      0.00000      0.00000
NUMBER POINTS OUTPUT =         129
**** This image is ALIASED ****
INTEGRATED MTF AMPLITUDE FROM .25 TO .48 =      60.00000
if (mode = "nobatch" or mode = "inter")
end-if
otf1 inp=edge.img plotout=test10 phase=nophase
Beginning VICAR task otf1
OTF1 version 2015-08-10
NUMBER OF LINES PROCESSED=         40

 PIXEL  CYCLES/      FREQUENCY    REAL(FT)     IMAG.(FT)     INTENSITY    AMPLITUDE     PHASE     WAVE LENGT
         PIXEL                                                                         RADIANS      SHIFT
  1     0.00000       0.00000      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
  2     0.00391       0.00391      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
  3     0.00781       0.00781      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
  4     0.01172       0.01172      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
  5     0.01563       0.01563      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
  6     0.01953       0.01953      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
  7     0.02344       0.02344      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
  8     0.02734       0.02734      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
  9     0.03125       0.03125      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 10     0.03516       0.03516      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 11     0.03906       0.03906      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 12     0.04297       0.04297      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 13     0.04688       0.04688      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 14     0.05078       0.05078      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 15     0.05469       0.05469      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 16     0.05859       0.05859      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 17     0.06250       0.06250      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 18     0.06641       0.06641      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 19     0.07031       0.07031      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 20     0.07422       0.07422      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 21     0.07813       0.07813      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 22     0.08203       0.08203      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 23     0.08594       0.08594      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 24     0.08984       0.08984      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 25     0.09375       0.09375      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 26     0.09766       0.09766      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 27     0.10156       0.10156      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 28     0.10547       0.10547      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 29     0.10938       0.10938      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 30     0.11328       0.11328      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 31     0.11719       0.11719      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 32     0.12109       0.12109      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 33     0.12500       0.12500      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 34     0.12891       0.12891      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 35     0.13281       0.13281      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 36     0.13672       0.13672      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 37     0.14063       0.14063      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 38     0.14453       0.14453      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 39     0.14844       0.14844      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 40     0.15234       0.15234      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 41     0.15625       0.15625      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 42     0.16016       0.16016      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 43     0.16406       0.16406      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 44     0.16797       0.16797      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 45     0.17188       0.17188      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 46     0.17578       0.17578      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 47     0.17969       0.17969      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 48     0.18359       0.18359      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 49     0.18750       0.18750      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 50     0.19141       0.19141      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 51     0.19531       0.19531      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 52     0.19922       0.19922      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 53     0.20313       0.20313      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 54     0.20703       0.20703      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 55     0.21094       0.21094      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 56     0.21484       0.21484      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 57     0.21875       0.21875      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 58     0.22266       0.22266      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 59     0.22656       0.22656      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 60     0.23047       0.23047      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 61     0.23438       0.23438      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 62     0.23828       0.23828      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 63     0.24219       0.24219      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 64     0.24609       0.24609      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 65     0.25000       0.25000      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 66     0.25391       0.25391      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 67     0.25781       0.25781      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 68     0.26172       0.26172      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 69     0.26563       0.26563      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 70     0.26953       0.26953      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 71     0.27344       0.27344      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 72     0.27734       0.27734      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 73     0.28125       0.28125      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 74     0.28516       0.28516      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 75     0.28906       0.28906      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 76     0.29297       0.29297      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 77     0.29688       0.29688      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 78     0.30078       0.30078      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 79     0.30469       0.30469      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 80     0.30859       0.30859      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 81     0.31250       0.31250      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 82     0.31641       0.31641      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 83     0.32031       0.32031      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 84     0.32422       0.32422      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 85     0.32813       0.32813      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 86     0.33203       0.33203      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 87     0.33594       0.33594      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 88     0.33984       0.33984      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 89     0.34375       0.34375      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 90     0.34766       0.34766      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 91     0.35156       0.35156      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 92     0.35547       0.35547      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 93     0.35938       0.35938      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 94     0.36328       0.36328      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 95     0.36719       0.36719      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 96     0.37109       0.37109      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 97     0.37500       0.37500      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
 98     0.37891       0.37891      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
 99     0.38281       0.38281      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
100     0.38672       0.38672      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
101     0.39063       0.39063      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
102     0.39453       0.39453      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
103     0.39844       0.39844      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
104     0.40234       0.40234      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
105     0.40625       0.40625      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
106     0.41016       0.41016      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
107     0.41406       0.41406      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
108     0.41797       0.41797      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
109     0.42188       0.42188      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
110     0.42578       0.42578      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
111     0.42969       0.42969      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
112     0.43359       0.43359      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
113     0.43750       0.43750      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
114     0.44141       0.44141      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
115     0.44531       0.44531      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
116     0.44922       0.44922      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
117     0.45313       0.45313      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
118     0.45703       0.45703      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
119     0.46094       0.46094      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
120     0.46484       0.46484      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
121     0.46875       0.46875      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
122     0.47266       0.47266      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
123     0.47656       0.47656      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
124     0.48047       0.48047      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
125     0.48438       0.48438      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
126     0.48828       0.48828      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
127     0.49219       0.49219      1.00000     -0.00000      1.00000      1.00000      0.00000      0.00000
128     0.49609       0.49609      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
129     0.50000       0.50000      1.00001      0.00000      1.00001      1.00001      0.00000      0.00000
NUMBER POINTS OUTPUT =         129
**** This image is ALIASED ****
INTEGRATED MTF AMPLITUDE FROM .25 TO .48 =      60.00000
if (mode = "nobatch" or mode = "inter")
end-if
otf1 inp=s1edge.img plotout=test11
Beginning VICAR task otf1
OTF1 version 2015-08-10
NUMBER OF LINES PROCESSED=         20

 PIXEL  CYCLES/      FREQUENCY    REAL(FT)     IMAG.(FT)     INTENSITY    AMPLITUDE     PHASE     WAVE LENGT
         PIXEL                                                                         RADIANS      SHIFT
  1     0.00000       0.00000      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
  2     0.00391       0.00391      0.99799     -0.00000      0.99599      0.99799      0.00000      0.00000
  3     0.00781       0.00781      0.99199      0.00000      0.98404      0.99199      0.00000      0.00000
  4     0.01172       0.01172      0.98202      0.00000      0.96437      0.98202      0.00000      0.00000
  5     0.01563       0.01563      0.96818      0.00000      0.93736      0.96818      0.00000      0.00000
  6     0.01953       0.01953      0.95054      0.00000      0.90352      0.95054      0.00000      0.00000
  7     0.02344       0.02344      0.92924     -0.00000      0.86348      0.92924      0.00000      0.00000
  8     0.02734       0.02734      0.90443      0.00000      0.81799      0.90443      0.00000      0.00000
  9     0.03125       0.03125      0.87628      0.00000      0.76786      0.87628      0.00000      0.00000
 10     0.03516       0.03516      0.84498     -0.00000      0.71400      0.84498      0.00000      0.00000
 11     0.03906       0.03906      0.81077      0.00000      0.65735      0.81077      0.00000      0.00000
 12     0.04297       0.04297      0.77388      0.00000      0.59888      0.77388      0.00000      0.00000
 13     0.04688       0.04688      0.73455      0.00000      0.53957      0.73455      0.00000      0.00000
 14     0.05078       0.05078      0.69307     -0.00000      0.48035      0.69307      0.00000      0.00000
 15     0.05469       0.05469      0.64972      0.00000      0.42214      0.64972      0.00000      0.00000
 16     0.05859       0.05859      0.60479     -0.00000      0.36578      0.60479      0.00000      0.00000
 17     0.06250       0.06250      0.55859      0.00000      0.31203      0.55859      0.00000      0.00000
 18     0.06641       0.06641      0.51143     -0.00000      0.26156      0.51143      0.00000      0.00000
 19     0.07031       0.07031      0.46362     -0.00000      0.21494      0.46362      0.00000      0.00000
 20     0.07422       0.07422      0.41547      0.00000      0.17261      0.41547      0.00000      0.00000
 21     0.07813       0.07813      0.36729      0.00000      0.13491      0.36729      0.00000      0.00000
 22     0.08203       0.08203      0.31941     -0.00000      0.10202      0.31941      0.00000      0.00000
 23     0.08594       0.08594      0.27211     -0.00000      0.07404      0.27211      0.00000      0.00000
 24     0.08984       0.08984      0.22569     -0.00000      0.05094      0.22569      0.00000      0.00000
 25     0.09375       0.09375      0.18043      0.00000      0.03256      0.18043      0.00000      0.00000
 26     0.09766       0.09766      0.13661     -0.00000      0.01866      0.13661      0.00000      0.00000
 27     0.10156       0.10156      0.09447      0.00000      0.00893      0.09447      0.00000      0.00000
 28     0.10547       0.10547      0.05426     -0.00000      0.00294      0.05426      0.00000      0.00000
 29     0.10938       0.10938      0.01618      0.00000      0.00026      0.01618      0.00000      0.00000
 30     0.11328       0.11328     -0.01956     -0.00000      0.00038      0.01956      3.14159      4.41379
 31     0.11719       0.11719     -0.05278      0.00000      0.00279      0.05278      3.14159      4.26667
 32     0.12109       0.12109     -0.08334     -0.00000      0.00695      0.08334      3.14159      4.12903
 33     0.12500       0.12500     -0.11111      0.00000      0.01235      0.11111      3.14159      4.00000
 34     0.12891       0.12891     -0.13598     -0.00000      0.01849      0.13598      3.14159      3.87879
 35     0.13281       0.13281     -0.15788     -0.00000      0.02493      0.15788      3.14159      3.76471
 36     0.13672       0.13672     -0.17674     -0.00000      0.03124      0.17674      3.14159      3.65714
 37     0.14063       0.14063     -0.19256      0.00000      0.03708      0.19256      3.14159      3.55556
 38     0.14453       0.14453     -0.20531     -0.00000      0.04215      0.20531      3.14159      3.45946
 39     0.14844       0.14844     -0.21502      0.00000      0.04623      0.21502      3.14159      3.36842
 40     0.15234       0.15234     -0.22175     -0.00000      0.04917      0.22175      3.14159      3.28205
 41     0.15625       0.15625     -0.22556      0.00000      0.05088      0.22556      3.14159      3.20000
 42     0.16016       0.16016     -0.22654     -0.00000      0.05132      0.22654      3.14159      3.12195
 43     0.16406       0.16406     -0.22481      0.00000      0.05054      0.22481      3.14159      3.04762
 44     0.16797       0.16797     -0.22051     -0.00000      0.04863      0.22051      3.14159      2.97674
 45     0.17188       0.17188     -0.21379      0.00000      0.04570      0.21379      3.14159      2.90909
 46     0.17578       0.17578     -0.20481      0.00000      0.04195      0.20481      3.14159      2.84444
 47     0.17969       0.17969     -0.19377      0.00000      0.03755      0.19377      3.14159      2.78261
 48     0.18359       0.18359     -0.18086     -0.00000      0.03271      0.18086      3.14159      2.72340
 49     0.18750       0.18750     -0.16629      0.00000      0.02765      0.16629      3.14159      2.66667
 50     0.19141       0.19141     -0.15028     -0.00000      0.02258      0.15028      3.14159      2.61224
 51     0.19531       0.19531     -0.13306      0.00000      0.01770      0.13306      3.14159      2.56000
 52     0.19922       0.19922     -0.11485     -0.00000      0.01319      0.11485      3.14159      2.50980
 53     0.20313       0.20313     -0.09589      0.00000      0.00920      0.09589      3.14159      2.46154
 54     0.20703       0.20703     -0.07641      0.00000      0.00584      0.07641      3.14159      2.41509
 55     0.21094       0.21094     -0.05665      0.00000      0.00321      0.05665      3.14159      2.37037
 56     0.21484       0.21484     -0.03683      0.00000      0.00136      0.03683      3.14159      2.32727
 57     0.21875       0.21875     -0.01717      0.00000      0.00029      0.01717      3.14159      2.28571
 58     0.22266       0.22266      0.00212     -0.00000      0.00000      0.00212      0.00000      0.00000
 59     0.22656       0.22656      0.02082      0.00000      0.00043      0.02082      0.00000      0.00000
 60     0.23047       0.23047      0.03876     -0.00000      0.00150      0.03876      0.00000      0.00000
 61     0.23438       0.23438      0.05574      0.00000      0.00311      0.05574      0.00000      0.00000
 62     0.23828       0.23828      0.07161      0.00000      0.00513      0.07161      0.00000      0.00000
 63     0.24219       0.24219      0.08621      0.00000      0.00743      0.08621      0.00000      0.00000
 64     0.24609       0.24609      0.09941      0.00000      0.00988      0.09941      0.00000      0.00000
 65     0.25000       0.25000      0.11111      0.00000      0.01235      0.11111      0.00000      0.00000
 66     0.25391       0.25391      0.12120     -0.00000      0.01469      0.12120      0.00000      0.00000
 67     0.25781       0.25781      0.12961      0.00000      0.01680      0.12961      0.00000      0.00000
 68     0.26172       0.26172      0.13629      0.00000      0.01857      0.13629      0.00000      0.00000
 69     0.26563       0.26563      0.14119      0.00000      0.01994      0.14119      0.00000      0.00000
 70     0.26953       0.26953      0.14431      0.00000      0.02082      0.14431      0.00000      0.00000
 71     0.27344       0.27344      0.14563      0.00000      0.02121      0.14563      0.00000      0.00000
 72     0.27734       0.27734      0.14520      0.00000      0.02108      0.14520      0.00000      0.00000
 73     0.28125       0.28125      0.14305      0.00000      0.02046      0.14305      0.00000      0.00000
 74     0.28516       0.28516      0.13923      0.00000      0.01938      0.13923      0.00000      0.00000
 75     0.28906       0.28906      0.13383     -0.00000      0.01791      0.13383      0.00000      0.00000
 76     0.29297       0.29297      0.12693      0.00000      0.01611      0.12693      0.00000      0.00000
 77     0.29688       0.29688      0.11865      0.00000      0.01408      0.11865      0.00000      0.00000
 78     0.30078       0.30078      0.10911     -0.00000      0.01190      0.10911      0.00000      0.00000
 79     0.30469       0.30469      0.09843     -0.00000      0.00969      0.09843      0.00000      0.00000
 80     0.30859       0.30859      0.08675     -0.00000      0.00753      0.08675      0.00000      0.00000
 81     0.31250       0.31250      0.07424      0.00000      0.00551      0.07424      0.00000      0.00000
 82     0.31641       0.31641      0.06105     -0.00000      0.00373      0.06105      0.00000      0.00000
 83     0.32031       0.32031      0.04733     -0.00000      0.00224      0.04733      0.00000      0.00000
 84     0.32422       0.32422      0.03326     -0.00000      0.00111      0.03326      0.00000      0.00000
 85     0.32813       0.32813      0.01901      0.00000      0.00036      0.01901      0.00000      0.00000
 86     0.33203       0.33203      0.00473     -0.00000      0.00002      0.00473      0.00000      0.00000
 87     0.33594       0.33594     -0.00939     -0.00000      0.00009      0.00939      3.14159      1.48837
 88     0.33984       0.33984     -0.02321     -0.00000      0.00054      0.02321      3.14159      1.47126
 89     0.34375       0.34375     -0.03657      0.00000      0.00134      0.03657      3.14159      1.45455
 90     0.34766       0.34766     -0.04932     -0.00000      0.00243      0.04932      3.14159      1.43820
 91     0.35156       0.35156     -0.06131     -0.00000      0.00376      0.06131      3.14159      1.42222
 92     0.35547       0.35547     -0.07243     -0.00000      0.00525      0.07243      3.14159      1.40659
 93     0.35938       0.35938     -0.08254      0.00000      0.00681      0.08254      3.14159      1.39130
 94     0.36328       0.36328     -0.09155     -0.00000      0.00838      0.09155      3.14159      1.37634
 95     0.36719       0.36719     -0.09937     -0.00000      0.00987      0.09937      3.14159      1.36170
 96     0.37109       0.37109     -0.10591     -0.00000      0.01122      0.10591      3.14159      1.34737
 97     0.37500       0.37500     -0.11111      0.00000      0.01235      0.11111      3.14159      1.33333
 98     0.37891       0.37891     -0.11493      0.00000      0.01321      0.11493      3.14159      1.31959
 99     0.38281       0.38281     -0.11734     -0.00000      0.01377      0.11734      3.14159      1.30612
100     0.38672       0.38672     -0.11832     -0.00000      0.01400      0.11832      3.14159      1.29293
101     0.39063       0.39063     -0.11787      0.00000      0.01389      0.11787      3.14159      1.28000
102     0.39453       0.39453     -0.11601     -0.00000      0.01346      0.11601      3.14159      1.26733
103     0.39844       0.39844     -0.11278     -0.00000      0.01272      0.11278      3.14159      1.25490
104     0.40234       0.40234     -0.10822     -0.00000      0.01171      0.10822      3.14159      1.24272
105     0.40625       0.40625     -0.10240      0.00000      0.01049      0.10240      3.14159      1.23077
106     0.41016       0.41016     -0.09540      0.00000      0.00910      0.09540      3.14159      1.21905
107     0.41406       0.41406     -0.08730      0.00000      0.00762      0.08730      3.14159      1.20755
108     0.41797       0.41797     -0.07820     -0.00000      0.00612      0.07820      3.14159      1.19626
109     0.42188       0.42188     -0.06823      0.00000      0.00466      0.06823      3.14159      1.18519
110     0.42578       0.42578     -0.05750      0.00000      0.00331      0.05750      3.14159      1.17431
111     0.42969       0.42969     -0.04615     -0.00000      0.00213      0.04615      3.14159      1.16364
112     0.43359       0.43359     -0.03430     -0.00000      0.00118      0.03430      3.14159      1.15315
113     0.43750       0.43750     -0.02210      0.00000      0.00049      0.02210      3.14159      1.14286
114     0.44141       0.44141     -0.00970      0.00000      0.00009      0.00970      3.14159      1.13274
115     0.44531       0.44531      0.00277      0.00000      0.00001      0.00277      0.00000      0.00000
116     0.44922       0.44922      0.01515     -0.00000      0.00023      0.01515      0.00000      0.00000
117     0.45313       0.45313      0.02729      0.00000      0.00074      0.02729      0.00000      0.00000
118     0.45703       0.45703      0.03907     -0.00000      0.00153      0.03907      0.00000      0.00000
119     0.46094       0.46094      0.05034      0.00000      0.00253      0.05034      0.00000      0.00000
120     0.46484       0.46484      0.06096      0.00000      0.00372      0.06096      0.00000      0.00000
121     0.46875       0.46875      0.07083      0.00000      0.00502      0.07083      0.00000      0.00000
122     0.47266       0.47266      0.07982      0.00000      0.00637      0.07982      0.00000      0.00000
123     0.47656       0.47656      0.08783     -0.00000      0.00771      0.08783      0.00000      0.00000
124     0.48047       0.48047      0.09477      0.00000      0.00898      0.09477      0.00000      0.00000
125     0.48438       0.48438      0.10056      0.00000      0.01011      0.10056      0.00000      0.00000
126     0.48828       0.48828      0.10514      0.00000      0.01105      0.10514      0.00000      0.00000
127     0.49219       0.49219      0.10844     -0.00000      0.01176      0.10844      0.00000      0.00000
128     0.49609       0.49609      0.11044      0.00000      0.01220      0.11044      0.00000      0.00000
129     0.50000       0.50000      0.11111      0.00000      0.01235      0.11111      0.00000      0.00000
NUMBER POINTS OUTPUT =         129
**** This image is ALIASED ****
INTEGRATED MTF AMPLITUDE FROM .25 TO .48 =       4.94845
if (mode = "nobatch" or mode = "inter")
end-if
otf1 inp=s2edge.img plotout=test12
Beginning VICAR task otf1
OTF1 version 2015-08-10
NUMBER OF LINES PROCESSED=         20

 PIXEL  CYCLES/      FREQUENCY    REAL(FT)     IMAG.(FT)     INTENSITY    AMPLITUDE     PHASE     WAVE LENGT
         PIXEL                                                                         RADIANS      SHIFT
  1     0.00000       0.00000      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
  2     0.00391       0.00391      0.99698     -0.00000      0.99397      0.99698      0.00000      0.00000
  3     0.00781       0.00781      0.98793      0.00000      0.97601      0.98793      0.00000      0.00000
  4     0.01172       0.01172      0.97300      0.00000      0.94674      0.97300      0.00000      0.00000
  5     0.01563       0.01563      0.95236      0.00001      0.90700      0.95236      0.00000      0.00000
  6     0.01953       0.01953      0.92631      0.00001      0.85804      0.92631      0.00000      0.00000
  7     0.02344       0.02344      0.89515      0.00002      0.80129      0.89515      0.00000      0.00000
  8     0.02734       0.02734      0.85931      0.00003      0.73841      0.85931      0.00000      0.00000
  9     0.03125       0.03125      0.81923      0.00005      0.67114      0.81923      0.00000      0.00000
 10     0.03516       0.03516      0.77545      0.00007      0.60132      0.77545      0.00000      0.00000
 11     0.03906       0.03906      0.72848      0.00010      0.53069      0.72848      0.00000      0.00000
 12     0.04297       0.04297      0.67894      0.00012      0.46097      0.67894      0.00000      0.00000
 13     0.04688       0.04688      0.62742      0.00017      0.39365      0.62742      0.00000      0.00000
 14     0.05078       0.05078      0.57454      0.00021      0.33009      0.57454      0.00000      0.00000
 15     0.05469       0.05469      0.52092      0.00026      0.27135      0.52092      0.00000      0.00000
 16     0.05859       0.05859      0.46719      0.00032      0.21826      0.46719      0.00000      0.00000
 17     0.06250       0.06250      0.41393      0.00038      0.17134      0.41393      0.00000      0.00000
 18     0.06641       0.06641      0.36174      0.00045      0.13086      0.36174      0.00125      0.00300
 19     0.07031       0.07031      0.31116      0.00054      0.09682      0.31116      0.00172      0.00390
 20     0.07422       0.07422      0.26269      0.00062      0.06901      0.26269      0.00235      0.00504
 21     0.07813       0.07813      0.21678      0.00071      0.04700      0.21678      0.00329      0.00670
 22     0.08203       0.08203      0.17385      0.00080      0.03023      0.17386      0.00462      0.00897
 23     0.08594       0.08594      0.13423      0.00091      0.01802      0.13423      0.00676      0.01252
 24     0.08984       0.08984      0.09821      0.00100      0.00965      0.09821      0.01022      0.01810
 25     0.09375       0.09375      0.06599      0.00111      0.00436      0.06600      0.01681      0.02854
 26     0.09766       0.09766      0.03774      0.00120      0.00143      0.03776      0.03190      0.05199
 27     0.10156       0.10156      0.01352      0.00131      0.00018      0.01358      0.09631      0.15093
 28     0.10547       0.10547     -0.00664      0.00139      0.00005      0.00678      2.93503      4.42904
 29     0.10938       0.10938     -0.02280      0.00148      0.00052      0.02284      3.07679      4.47713
 30     0.11328       0.11328     -0.03505      0.00155      0.00123      0.03508      3.09753      4.35188
 31     0.11719       0.11719     -0.04358      0.00161      0.00190      0.04361      3.10466      4.21651
 32     0.12109       0.12109     -0.04859      0.00165      0.00236      0.04862      3.10770      4.08449
 33     0.12500       0.12500     -0.05037      0.00168      0.00254      0.05040      3.10828      3.95758
 34     0.12891       0.12891     -0.04920      0.00168      0.00242      0.04923      3.10751      3.83671
 35     0.13281       0.13281     -0.04544      0.00167      0.00207      0.04547      3.10494      3.72078
 36     0.13672       0.13672     -0.03944      0.00162      0.00156      0.03947      3.10059      3.60941
 37     0.14063       0.14063     -0.03158      0.00156      0.00100      0.03162      3.09233      3.49980
 38     0.14453       0.14453     -0.02225      0.00146      0.00050      0.02230      3.07626      3.38751
 39     0.14844       0.14844     -0.01185      0.00134      0.00014      0.01192      3.02892      3.24761
 40     0.15234       0.15234     -0.00075      0.00118      0.00000      0.00140      2.13833      2.23393
 41     0.15625       0.15625      0.01065      0.00101      0.00011      0.01070      0.09496      0.09672
 42     0.16016       0.16016      0.02202      0.00080      0.00049      0.02203      0.03654      0.03631
 43     0.16406       0.16406      0.03302      0.00059      0.00109      0.03302      0.01773      0.01720
 44     0.16797       0.16797      0.04335      0.00033      0.00188      0.04335      0.00761      0.00722
 45     0.17188       0.17188      0.05275      0.00007      0.00278      0.05275      0.00134      0.00125
 46     0.17578       0.17578      0.06100     -0.00022      0.00372      0.06100     -0.00356     -0.00322
 47     0.17969       0.17969      0.06792     -0.00050      0.00461      0.06792     -0.00737     -0.00653
 48     0.18359       0.18359      0.07336     -0.00080      0.00538      0.07337     -0.01095     -0.00949
 49     0.18750       0.18750      0.07724     -0.00109      0.00597      0.07724     -0.01411     -0.01198
 50     0.19141       0.19141      0.07949     -0.00138      0.00632      0.07950     -0.01740     -0.01447
 51     0.19531       0.19531      0.08013     -0.00165      0.00642      0.08014     -0.02057     -0.01676
 52     0.19922       0.19922      0.07917     -0.00191      0.00627      0.07919     -0.02407     -0.01923
 53     0.20313       0.20313      0.07671     -0.00212      0.00589      0.07674     -0.02764     -0.02166
 54     0.20703       0.20703      0.07283     -0.00231      0.00531      0.07287     -0.03176     -0.02441
 55     0.21094       0.21094      0.06770     -0.00245      0.00459      0.06774     -0.03615     -0.02728
 56     0.21484       0.21484      0.06146     -0.00255      0.00378      0.06152     -0.04142     -0.03068
 57     0.21875       0.21875      0.05433     -0.00257      0.00296      0.05439     -0.04733     -0.03444
 58     0.22266       0.22266      0.04648     -0.00255      0.00217      0.04655     -0.05480     -0.03917
 59     0.22656       0.22656      0.03816     -0.00244      0.00146      0.03823     -0.06392     -0.04490
 60     0.23047       0.23047      0.02956     -0.00227      0.00088      0.02964     -0.07676     -0.05301
 61     0.23438       0.23438      0.02092     -0.00201      0.00044      0.02102     -0.09586     -0.06509
 62     0.23828       0.23828      0.01244     -0.00168      0.00016      0.01256     -0.13427     -0.08968
 63     0.24219       0.24219      0.00435     -0.00125      0.00002      0.00453     -0.28037     -0.18424
 64     0.24609       0.24609     -0.00319     -0.00075      0.00001      0.00328     -2.91037     -1.88221
 65     0.25000       0.25000     -0.01000     -0.00015      0.00010      0.01000     -3.12620     -1.99020
 66     0.25391       0.25391     -0.01593      0.00051      0.00025      0.01594      3.10931      1.94900
 67     0.25781       0.25781     -0.02086      0.00127      0.00044      0.02090      3.08057      1.90173
 68     0.26172       0.26172     -0.02472      0.00210      0.00062      0.02481      3.05694      1.85897
 69     0.26563       0.26563     -0.02741      0.00300      0.00076      0.02758      3.03255      1.81702
 70     0.26953       0.26953     -0.02894      0.00395      0.00085      0.02921      3.00580      1.77489
 71     0.27344       0.27344     -0.02927      0.00497      0.00088      0.02969      2.97339      1.73067
 72     0.27734       0.27734     -0.02847      0.00602      0.00085      0.02910      2.93323      1.68325
 73     0.28125       0.28125     -0.02656      0.00711      0.00076      0.02749      2.88001      1.62975
 74     0.28516       0.28516     -0.02365      0.00821      0.00063      0.02503      2.80746      1.56693
 75     0.28906       0.28906     -0.01982      0.00932      0.00048      0.02190      2.70181      1.48759
 76     0.29297       0.29297     -0.01523      0.01042      0.00034      0.01845      2.54134      1.38058
 77     0.29688       0.29688     -0.00999      0.01151      0.00023      0.01524      2.28552      1.22527
 78     0.30078       0.30078     -0.00429      0.01255      0.00018      0.01327      1.90032      1.00553
 79     0.30469       0.30469      0.00173      0.01355      0.00019      0.01366      1.44385      0.75420
 80     0.30859       0.30859      0.00787      0.01449      0.00027      0.01649      1.07315      0.55347
 81     0.31250       0.31250      0.01400      0.01535      0.00043      0.02077      0.83136      0.42341
 82     0.31641       0.31641      0.01990      0.01611      0.00066      0.02561      0.68060      0.34235
 83     0.32031       0.32031      0.02547      0.01678      0.00093      0.03050      0.58263      0.28949
 84     0.32422       0.32422      0.03051      0.01734      0.00123      0.03509      0.51678      0.25368
 85     0.32813       0.32813      0.03494      0.01778      0.00154      0.03920      0.47066      0.22829
 86     0.33203       0.33203      0.03859      0.01808      0.00182      0.04261      0.43822      0.21006
 87     0.33594       0.33594      0.04142      0.01825      0.00205      0.04526      0.41510      0.19666
 88     0.33984       0.33984      0.04331      0.01829      0.00221      0.04701      0.39950      0.18709
 89     0.34375       0.34375      0.04427      0.01818      0.00229      0.04786      0.38955      0.18036
 90     0.34766       0.34766      0.04423      0.01792      0.00228      0.04773      0.38503      0.17626
 91     0.35156       0.35156      0.04324      0.01753      0.00218      0.04666      0.38513      0.17435
 92     0.35547       0.35547      0.04129      0.01700      0.00199      0.04465      0.39065      0.17490
 93     0.35938       0.35938      0.03847      0.01634      0.00175      0.04180      0.40159      0.17785
 94     0.36328       0.36328      0.03481      0.01555      0.00145      0.03813      0.42019      0.18409
 95     0.36719       0.36719      0.03047      0.01465      0.00114      0.03381      0.44813      0.19424
 96     0.37109       0.37109      0.02549      0.01365      0.00084      0.02892      0.49151      0.21080
 97     0.37500       0.37500      0.02007      0.01254      0.00056      0.02367      0.55854      0.23705
 98     0.37891       0.37891      0.01429      0.01137      0.00033      0.01826      0.67242      0.28244
 99     0.38281       0.38281      0.00834      0.01013      0.00017      0.01312      0.88192      0.36666
100     0.38672       0.38672      0.00232      0.00885      0.00008      0.00915      1.31405      0.54080
101     0.39063       0.39063     -0.00355      0.00752      0.00007      0.00832      2.01169      0.81964
102     0.39453       0.39453     -0.00920      0.00620      0.00012      0.01110      2.54827      1.02798
103     0.39844       0.39844     -0.01442      0.00487      0.00023      0.01522      2.81614      1.12490
104     0.40234       0.40234     -0.01915      0.00357      0.00038      0.01948      2.95720      1.16978
105     0.40625       0.40625     -0.02320      0.00229      0.00054      0.02331      3.04328      1.19226
106     0.41016       0.41016     -0.02655      0.00109      0.00071      0.02657      3.10072      1.20319
107     0.41406       0.41406     -0.02905     -0.00008      0.00084      0.02905     -3.13889     -1.20651
108     0.41797       0.41797     -0.03071     -0.00113      0.00094      0.03073     -3.10485     -1.18227
109     0.42188       0.42188     -0.03142     -0.00212      0.00099      0.03149     -3.07423     -1.15977
110     0.42578       0.42578     -0.03124     -0.00297      0.00098      0.03138     -3.04689     -1.13891
111     0.42969       0.42969     -0.03012     -0.00374      0.00092      0.03035     -3.01796     -1.11784
112     0.43359       0.43359     -0.02814     -0.00435      0.00081      0.02848     -2.98820     -1.09685
113     0.43750       0.43750     -0.02530     -0.00488      0.00066      0.02577     -2.95103     -1.07353
114     0.44141       0.44141     -0.02175     -0.00523      0.00050      0.02237     -2.90568     -1.04768
115     0.44531       0.44531     -0.01749     -0.00550      0.00034      0.01834     -2.83693     -1.01392
116     0.44922       0.44922     -0.01273     -0.00558      0.00019      0.01390     -2.72840     -0.96665
117     0.45313       0.45313     -0.00750     -0.00560      0.00009      0.00936     -2.49982     -0.87803
118     0.45703       0.45703     -0.00200     -0.00542      0.00003      0.00578     -1.92495     -0.67034
119     0.46094       0.46094      0.00368     -0.00521      0.00004      0.00638     -0.95644     -0.33025
120     0.46484       0.46484      0.00933     -0.00480      0.00011      0.01050     -0.47500     -0.16263
121     0.46875       0.46875      0.01490     -0.00440      0.00024      0.01553     -0.28708     -0.09747
122     0.47266       0.47266      0.02015     -0.00379      0.00042      0.02050     -0.18576     -0.06255
123     0.47656       0.47656      0.02504     -0.00324      0.00064      0.02525     -0.12885     -0.04303
124     0.48047       0.48047      0.02936     -0.00247      0.00087      0.02947     -0.08407     -0.02785
125     0.48438       0.48438      0.03309     -0.00186      0.00110      0.03314     -0.05621     -0.01847
126     0.48828       0.48828      0.03605     -0.00096      0.00130      0.03606     -0.02667     -0.00869
127     0.49219       0.49219      0.03824     -0.00040      0.00146      0.03825     -0.01034     -0.00334
128     0.49609       0.49609      0.03954      0.00072      0.00156      0.03955      0.01828      0.00587
129     0.50000       0.50000      0.03999      0.00000      0.00160      0.03999      0.00000      0.00000
NUMBER POINTS OUTPUT =         129
**** This image is ALIASED ****
INTEGRATED MTF AMPLITUDE FROM .25 TO .48 =       1.50719
if (mode = "nobatch" or mode = "inter")
end-if
otf1 inp=s1edgeN.img plotout=test13
Beginning VICAR task otf1
OTF1 version 2015-08-10
NUMBER OF BAD PIXELS ON RIGHT=          1
LINE NUMBER=          1
NUMBER OF BAD PIXELS ON RIGHT=          1
LINE NUMBER=          7
NUMBER OF LINES PROCESSED=         20

 PIXEL  CYCLES/      FREQUENCY    REAL(FT)     IMAG.(FT)     INTENSITY    AMPLITUDE     PHASE     WAVE LENGT
         PIXEL                                                                         RADIANS      SHIFT
  1     0.00000       0.00000      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
  2     0.00391       0.00391      0.99789      0.00000      0.99579      0.99789      0.00000      0.00000
  3     0.00781       0.00781      0.99187      0.00000      0.98380      0.99187      0.00000      0.00000
  4     0.01172       0.01172      0.98168      0.00003      0.96370      0.98168      0.00000      0.00000
  5     0.01563       0.01563      0.96770      0.00003      0.93644      0.96770      0.00000      0.00000
  6     0.01953       0.01953      0.94974      0.00010      0.90201      0.94974      0.00000      0.00000
  7     0.02344       0.02344      0.92821      0.00013      0.86158      0.92821      0.00000      0.00000
  8     0.02734       0.02734      0.90301      0.00024      0.81542      0.90301      0.00000      0.00000
  9     0.03125       0.03125      0.87456      0.00031      0.76486      0.87456      0.00000      0.00000
 10     0.03516       0.03516      0.84284      0.00048      0.71037      0.84284      0.00000      0.00000
 11     0.03906       0.03906      0.80830      0.00060      0.65334      0.80830      0.00000      0.00000
 12     0.04297       0.04297      0.77097      0.00083      0.59439      0.77097      0.00108      0.00400
 13     0.04688       0.04688      0.73133      0.00100      0.53484      0.73133      0.00137      0.00464
 14     0.05078       0.05078      0.68946      0.00130      0.47535      0.68946      0.00188      0.00590
 15     0.05469       0.05469      0.64584      0.00151      0.41711      0.64584      0.00234      0.00682
 16     0.05859       0.05859      0.60061      0.00187      0.36074      0.60062      0.00312      0.00847
 17     0.06250       0.06250      0.55424      0.00213      0.30718      0.55424      0.00385      0.00979
 18     0.06641       0.06641      0.50690      0.00254      0.25695      0.50691      0.00501      0.01201
 19     0.07031       0.07031      0.45903      0.00283      0.21072      0.45904      0.00618      0.01398
 20     0.07422       0.07422      0.41087      0.00328      0.16882      0.41088      0.00798      0.01712
 21     0.07813       0.07813      0.36279      0.00360      0.13163      0.36281      0.00992      0.02020
 22     0.08203       0.08203      0.31507      0.00406      0.09928      0.31509      0.01290      0.02502
 23     0.08594       0.08594      0.26803      0.00439      0.07186      0.26807      0.01637      0.03032
 24     0.08984       0.08984      0.22196      0.00485      0.04929      0.22201      0.02187      0.03874
 25     0.09375       0.09375      0.17713      0.00517      0.03140      0.17720      0.02919      0.04955
 26     0.09766       0.09766      0.13383      0.00562      0.01794      0.13395      0.04197      0.06839
 27     0.10156       0.10156      0.09227      0.00591      0.00855      0.09246      0.06398      0.10026
 28     0.10547       0.10547      0.05274      0.00632      0.00282      0.05312      0.11931      0.18005
 29     0.10938       0.10938      0.01537      0.00657      0.00028      0.01672      0.40424      0.58822
 30     0.11328       0.11328     -0.01956      0.00693      0.00043      0.02075      2.80094      3.93519
 31     0.11719       0.11719     -0.05199      0.00713      0.00275      0.05248      3.00528      4.08153
 32     0.12109       0.12109     -0.08166      0.00742      0.00672      0.08199      3.05095      4.00989
 33     0.12500       0.12500     -0.10860      0.00756      0.01185      0.10886      3.07209      3.91150
 34     0.12891       0.12891     -0.13254      0.00777      0.01763      0.13277      3.08300      3.80645
 35     0.13281       0.13281     -0.15363      0.00785      0.02366      0.15383      3.09057      3.70356
 36     0.13672       0.13672     -0.17160      0.00798      0.02951      0.17178      3.09513      3.60305
 37     0.14063       0.14063     -0.18667      0.00798      0.03491      0.18684      3.09885      3.50718
 38     0.14453       0.14453     -0.19862      0.00804      0.03952      0.19879      3.10116      3.41493
 39     0.14844       0.14844     -0.20774      0.00798      0.04322      0.20789      3.10321      3.32726
 40     0.15234       0.15234     -0.21382      0.00796      0.04578      0.21397      3.10441      3.24320
 41     0.15625       0.15625     -0.21722      0.00784      0.04724      0.21736      3.10550      3.16323
 42     0.16016       0.16016     -0.21776      0.00776      0.04748      0.21790      3.10599      3.08658
 43     0.16406       0.16406     -0.21586      0.00760      0.04665      0.21600      3.10638      3.01346
 44     0.16797       0.16797     -0.21138      0.00746      0.04474      0.21151      3.10631      2.94331
 45     0.17188       0.17188     -0.20475      0.00729      0.04198      0.20488      3.10602      2.87615
 46     0.17578       0.17578     -0.19588      0.00711      0.03842      0.19601      3.10532      2.81161
 47     0.17969       0.17969     -0.18523      0.00692      0.03436      0.18536      3.10423      2.74952
 48     0.18359       0.18359     -0.17272      0.00673      0.02988      0.17285      3.10268      2.68967
 49     0.18750       0.18750     -0.15885      0.00655      0.02528      0.15899      3.10038      2.63168
 50     0.19141       0.19141     -0.14354      0.00635      0.02064      0.14368      3.09738      2.57548
 51     0.19531       0.19531     -0.12730      0.00620      0.01624      0.12745      3.09291      2.52033
 52     0.19922       0.19922     -0.11007      0.00602      0.01215      0.11024      3.08699      2.46619
 53     0.20313       0.20313     -0.09236      0.00591      0.00856      0.09255      3.07771      2.41149
 54     0.20703       0.20703     -0.07410      0.00575      0.00552      0.07432      3.06417      2.35557
 55     0.21094       0.21094     -0.05579      0.00569      0.00314      0.05608      3.03992      2.29366
 56     0.21484       0.21484     -0.03737      0.00557      0.00143      0.03779      2.99365      2.21768
 57     0.21875       0.21875     -0.01931      0.00557      0.00040      0.02010      2.86073      2.08137
 58     0.22266       0.22266     -0.00156      0.00549      0.00003      0.00571      1.84726      1.32042
 59     0.22656       0.22656      0.01548      0.00555      0.00027      0.01645      0.34435      0.24190
 60     0.23047       0.23047      0.03183      0.00552      0.00104      0.03231      0.17162      0.11851
 61     0.23438       0.23438      0.04716      0.00563      0.00226      0.04749      0.11889      0.08073
 62     0.23828       0.23828      0.06148      0.00564      0.00381      0.06174      0.09147      0.06109
 63     0.24219       0.24219      0.07452      0.00580      0.00559      0.07475      0.07772      0.05107
 64     0.24609       0.24609      0.08630      0.00584      0.00748      0.08650      0.06759      0.04371
 65     0.25000       0.25000      0.09662      0.00604      0.00937      0.09681      0.06245      0.03976
 66     0.25391       0.25391      0.10549      0.00610      0.01116      0.10566      0.05777      0.03621
 67     0.25781       0.25781      0.11277      0.00632      0.01276      0.11295      0.05599      0.03456
 68     0.26172       0.26172      0.11850      0.00639      0.01408      0.11867      0.05385      0.03275
 69     0.26563       0.26563      0.12260      0.00661      0.01508      0.12278      0.05385      0.03227
 70     0.26953       0.26953      0.12510      0.00667      0.01569      0.12528      0.05323      0.03143
 71     0.27344       0.27344      0.12601      0.00687      0.01593      0.12620      0.05447      0.03171
 72     0.27734       0.27734      0.12534      0.00690      0.01576      0.12553      0.05501      0.03157
 73     0.28125       0.28125      0.12318      0.00707      0.01522      0.12339      0.05735      0.03246
 74     0.28516       0.28516      0.11953      0.00706      0.01434      0.11974      0.05902      0.03294
 75     0.28906       0.28906      0.11456      0.00718      0.01318      0.11479      0.06262      0.03448
 76     0.29297       0.29297      0.10824      0.00712      0.01177      0.10848      0.06569      0.03569
 77     0.29688       0.29688      0.10082      0.00718      0.01022      0.10107      0.07106      0.03809
 78     0.30078       0.30078      0.09224      0.00705      0.00856      0.09251      0.07630      0.04038
 79     0.30469       0.30469      0.08281      0.00703      0.00691      0.08311      0.08473      0.04426
 80     0.30859       0.30859      0.07247      0.00684      0.00530      0.07280      0.09416      0.04856
 81     0.31250       0.31250      0.06156      0.00675      0.00384      0.06193      0.10917      0.05560
 82     0.31641       0.31641      0.05000      0.00649      0.00254      0.05042      0.12913      0.06495
 83     0.32031       0.32031      0.03818      0.00632      0.00150      0.03870      0.16408      0.08153
 84     0.32422       0.32422      0.02598      0.00601      0.00071      0.02666      0.22728      0.11157
 85     0.32813       0.32813      0.01381      0.00577      0.00022      0.01497      0.39540      0.19179
 86     0.33203       0.33203      0.00155      0.00540      0.00003      0.00562      1.29077      0.61871
 87     0.33594       0.33594     -0.01037      0.00511      0.00013      0.01155      2.68384      1.27151
 88     0.33984       0.33984     -0.02212      0.00471      0.00051      0.02261      2.93164      1.37294
 89     0.34375       0.34375     -0.03325      0.00437      0.00112      0.03354      3.01080      1.39399
 90     0.34766       0.34766     -0.04397      0.00397      0.00195      0.04415      3.05158      1.39700
 91     0.35156       0.35156     -0.05382      0.00361      0.00291      0.05395      3.07465      1.39192
 92     0.35547       0.35547     -0.06305      0.00321      0.00399      0.06314      3.09068      1.38380
 93     0.35938       0.35938     -0.07120      0.00285      0.00508      0.07126      3.10154      1.37357
 94     0.36328       0.36328     -0.07856      0.00249      0.00618      0.07860      3.10990      1.36246
 95     0.36719       0.36719     -0.08467      0.00215      0.00717      0.08469      3.11615      1.35067
 96     0.37109       0.37109     -0.08986      0.00185      0.00808      0.08988      3.12103      1.33855
 97     0.37500       0.37500     -0.09369      0.00156      0.00878      0.09371      3.12499      1.32629
 98     0.37891       0.37891     -0.09653      0.00133      0.00932      0.09654      3.12786      1.31382
 99     0.38281       0.38281     -0.09796      0.00110      0.00960      0.09797      3.13040      1.30147
100     0.38672       0.38672     -0.09837      0.00096      0.00968      0.09838      3.13183      1.28891
101     0.39063       0.39063     -0.09738      0.00081      0.00948      0.09738      3.13329      1.27662
102     0.39453       0.39453     -0.09539      0.00078      0.00910      0.09539      3.13343      1.26404
103     0.39844       0.39844     -0.09205      0.00071      0.00847      0.09205      3.13385      1.25181
104     0.40234       0.40234     -0.08780      0.00080      0.00771      0.08781      3.13253      1.23913
105     0.40625       0.40625     -0.08231      0.00082      0.00678      0.08231      3.13163      1.22687
106     0.41016       0.41016     -0.07603      0.00102      0.00578      0.07604      3.12823      1.21386
107     0.41406       0.41406     -0.06866      0.00112      0.00472      0.06867      3.12521      1.20125
108     0.41797       0.41797     -0.06067      0.00143      0.00368      0.06068      3.11806      1.18730
109     0.42188       0.42188     -0.05177      0.00161      0.00268      0.05180      3.11052      1.17346
110     0.42578       0.42578     -0.04246      0.00201      0.00181      0.04251      3.09438      1.15666
111     0.42969       0.42969     -0.03246      0.00224      0.00106      0.03254      3.07264      1.13810
112     0.43359       0.43359     -0.02226      0.00271      0.00050      0.02243      3.02032      1.10864
113     0.43750       0.43750     -0.01162      0.00298      0.00014      0.01199      2.89073      1.05160
114     0.44141       0.44141     -0.00100      0.00350      0.00001      0.00364      1.84995      0.66703
115     0.44531       0.44531      0.00981      0.00376      0.00011      0.01050      0.36590      0.13077
116     0.44922       0.44922      0.02035      0.00430      0.00043      0.02080      0.20841      0.07384
117     0.45313       0.45313      0.03084      0.00452      0.00097      0.03117      0.14556      0.05113
118     0.45703       0.45703      0.04083      0.00506      0.00169      0.04115      0.12335      0.04296
119     0.46094       0.46094      0.05055      0.00520      0.00258      0.05081      0.10244      0.03537
120     0.46484       0.46484      0.05955      0.00571      0.00358      0.05982      0.09559      0.03273
121     0.46875       0.46875      0.06805      0.00571      0.00466      0.06829      0.08368      0.02841
122     0.47266       0.47266      0.07566      0.00618      0.00576      0.07591      0.08150      0.02744
123     0.47656       0.47656      0.08259      0.00598      0.00686      0.08281      0.07230      0.02415
124     0.48047       0.48047      0.08847      0.00642      0.00787      0.08871      0.07247      0.02401
125     0.48438       0.48438      0.09354      0.00593      0.00878      0.09373      0.06331      0.02080
126     0.48828       0.48828      0.09743      0.00643      0.00953      0.09765      0.06591      0.02148
127     0.49219       0.49219      0.10042      0.00538      0.01011      0.10056      0.05355      0.01732
128     0.49609       0.49609      0.10216      0.00654      0.01048      0.10237      0.06389      0.02050
129     0.50000       0.50000      0.10292      0.00000      0.01059      0.10292      0.00000      0.00000
NUMBER POINTS OUTPUT =         129
**** This image is ALIASED ****
INTEGRATED MTF AMPLITUDE FROM .25 TO .48 =       4.24324
if (mode = "nobatch" or mode = "inter")
end-if
otf1 inp=s2edgeN.img plotout=test14
Beginning VICAR task otf1
OTF1 version 2015-08-10
NUMBER OF BAD PIXELS ON RIGHT=          2
LINE NUMBER=          1
NUMBER OF BAD PIXELS ON LEFT=          2
LINE NUMBER=          1
NUMBER OF BAD PIXELS ON RIGHT=          1
LINE NUMBER=          2
NUMBER OF BAD PIXELS ON RIGHT=          1
LINE NUMBER=          3
NUMBER OF BAD PIXELS ON LEFT=          3
LINE NUMBER=          3
NUMBER OF BAD PIXELS ON RIGHT=          2
LINE NUMBER=          4
NUMBER OF BAD PIXELS ON LEFT=          4
LINE NUMBER=          4
NUMBER OF BAD PIXELS ON RIGHT=          2
LINE NUMBER=          5
NUMBER OF BAD PIXELS ON LEFT=          4
LINE NUMBER=          5
NUMBER OF BAD PIXELS ON RIGHT=          3
LINE NUMBER=          6
NUMBER OF BAD PIXELS ON LEFT=          5
LINE NUMBER=          6
NUMBER OF BAD PIXELS ON RIGHT=          2
LINE NUMBER=          7
NUMBER OF BAD PIXELS ON LEFT=          4
LINE NUMBER=          7
NUMBER OF BAD PIXELS ON RIGHT=          2
LINE NUMBER=          8
NUMBER OF BAD PIXELS ON LEFT=          5
LINE NUMBER=          8
NUMBER OF BAD PIXELS ON LEFT=          4
LINE NUMBER=          9
NUMBER OF BAD PIXELS ON RIGHT=          2
LINE NUMBER=         10
NUMBER OF BAD PIXELS ON LEFT=          4
LINE NUMBER=         10
NUMBER OF BAD PIXELS ON RIGHT=          1
LINE NUMBER=         11
NUMBER OF BAD PIXELS ON LEFT=          3
LINE NUMBER=         11
NUMBER OF BAD PIXELS ON RIGHT=          3
LINE NUMBER=         12
NUMBER OF BAD PIXELS ON LEFT=          6
LINE NUMBER=         12
NUMBER OF BAD PIXELS ON LEFT=          3
LINE NUMBER=         14
NUMBER OF BAD PIXELS ON RIGHT=          1
LINE NUMBER=         15
NUMBER OF BAD PIXELS ON LEFT=          4
LINE NUMBER=         15
NUMBER OF BAD PIXELS ON RIGHT=          1
LINE NUMBER=         16
NUMBER OF BAD PIXELS ON LEFT=          4
LINE NUMBER=         16
NUMBER OF BAD PIXELS ON RIGHT=          2
LINE NUMBER=         17
NUMBER OF BAD PIXELS ON LEFT=          4
LINE NUMBER=         17
NUMBER OF BAD PIXELS ON RIGHT=          3
LINE NUMBER=         18
NUMBER OF BAD PIXELS ON LEFT=          6
LINE NUMBER=         18
NUMBER OF BAD PIXELS ON RIGHT=          2
LINE NUMBER=         19
NUMBER OF BAD PIXELS ON LEFT=          5
LINE NUMBER=         19
NUMBER OF BAD PIXELS ON RIGHT=          1
LINE NUMBER=         20
NUMBER OF BAD PIXELS ON LEFT=          2
LINE NUMBER=         20
NUMBER OF LINES PROCESSED=         20

 PIXEL  CYCLES/      FREQUENCY    REAL(FT)     IMAG.(FT)     INTENSITY    AMPLITUDE     PHASE     WAVE LENGT
         PIXEL                                                                         RADIANS      SHIFT
  1     0.00000       0.00000      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
  2     0.00391       0.00391      0.99708     -0.00000      0.99416      0.99708      0.00000      0.00000
  3     0.00781       0.00781      0.98791      0.00001      0.97597      0.98791      0.00000      0.00000
  4     0.01172       0.01172      0.97306      0.00001      0.94685      0.97306      0.00000      0.00000
  5     0.01563       0.01563      0.95228      0.00005      0.90684      0.95228      0.00000      0.00000
  6     0.01953       0.01953      0.92628      0.00007      0.85800      0.92628      0.00000      0.00000
  7     0.02344       0.02344      0.89497      0.00016      0.80096      0.89497      0.00000      0.00000
  8     0.02734       0.02734      0.85917      0.00021      0.73817      0.85917      0.00000      0.00000
  9     0.03125       0.03125      0.81892      0.00036      0.67063      0.81892      0.00000      0.00000
 10     0.03516       0.03516      0.77516      0.00046      0.60087      0.77516      0.00000      0.00000
 11     0.03906       0.03906      0.72802      0.00068      0.53001      0.72802      0.00000      0.00000
 12     0.04297       0.04297      0.67849      0.00084      0.46034      0.67849      0.00124      0.00460
 13     0.04688       0.04688      0.62678      0.00115      0.39285      0.62678      0.00183      0.00621
 14     0.05078       0.05078      0.57390      0.00138      0.32936      0.57390      0.00241      0.00756
 15     0.05469       0.05469      0.52010      0.00178      0.27051      0.52011      0.00343      0.00997
 16     0.05859       0.05859      0.46637      0.00210      0.21750      0.46637      0.00450      0.01222
 17     0.06250       0.06250      0.41295      0.00259      0.17053      0.41295      0.00628      0.01599
 18     0.06641       0.06641      0.36076      0.00299      0.13016      0.36077      0.00830      0.01988
 19     0.07031       0.07031      0.31003      0.00358      0.09613      0.31005      0.01156      0.02617
 20     0.07422       0.07422      0.26157      0.00406      0.06843      0.26160      0.01553      0.03330
 21     0.07813       0.07813      0.21554      0.00474      0.04648      0.21559      0.02200      0.04481
 22     0.08203       0.08203      0.17263      0.00529      0.02983      0.17271      0.03063      0.05942
 23     0.08594       0.08594      0.13292      0.00604      0.01770      0.13306      0.04542      0.08411
 24     0.08984       0.08984      0.09694      0.00664      0.00944      0.09717      0.06838      0.12113
 25     0.09375       0.09375      0.06467      0.00744      0.00424      0.06509      0.11456      0.19449
 26     0.09766       0.09766      0.03648      0.00806      0.00140      0.03736      0.21755      0.35456
 27     0.10156       0.10156      0.01225      0.00889      0.00023      0.01513      0.62759      0.98348
 28     0.10547       0.10547     -0.00782      0.00950      0.00015      0.01230      2.25932      3.40937
 29     0.10938       0.10938     -0.02395      0.01030      0.00068      0.02607      2.73524      3.98013
 30     0.11328       0.11328     -0.03609      0.01087      0.00142      0.03769      2.84893      4.00261
 31     0.11719       0.11719     -0.04456      0.01162      0.00212      0.04605      2.88653      3.92026
 32     0.12109       0.12109     -0.04945      0.01210      0.00259      0.05091      2.90160      3.81360
 33     0.12500       0.12500     -0.05113      0.01274      0.00278      0.05269      2.89742      3.68910
 34     0.12891       0.12891     -0.04983      0.01309      0.00265      0.05152      2.88470      3.56162
 35     0.13281       0.13281     -0.04596      0.01358      0.00230      0.04793      2.85425      3.42037
 36     0.13672       0.13672     -0.03983      0.01376      0.00178      0.04214      2.80891      3.26986
 37     0.14063       0.14063     -0.03186      0.01407      0.00121      0.03483      2.72582      3.08500
 38     0.14453       0.14453     -0.02243      0.01404      0.00070      0.02646      2.58227      2.84355
 39     0.14844       0.14844     -0.01193      0.01413      0.00034      0.01849      2.27222      2.43628
 40     0.15234       0.15234     -0.00079      0.01387      0.00019      0.01390      1.62743      1.70019
 41     0.15625       0.15625      0.01068      0.01372      0.00030      0.01739      0.90957      0.92649
 42     0.16016       0.16016      0.02203      0.01323      0.00066      0.02570      0.54072      0.53734
 43     0.16406       0.16406      0.03303      0.01284      0.00126      0.03544      0.37064      0.35956
 44     0.16797       0.16797      0.04327      0.01211      0.00202      0.04493      0.27289      0.25857
 45     0.17188       0.17188      0.05259      0.01149      0.00290      0.05383      0.21505      0.19914
 46     0.17578       0.17578      0.06063      0.01055      0.00379      0.06154      0.17226      0.15597
 47     0.17969       0.17969      0.06737      0.00973      0.00463      0.06807      0.14344      0.12705
 48     0.18359       0.18359      0.07248      0.00862      0.00533      0.07299      0.11843      0.10266
 49     0.18750       0.18750      0.07606      0.00766      0.00584      0.07644      0.10033      0.08516
 50     0.19141       0.19141      0.07785      0.00644      0.00610      0.07811      0.08257      0.06865
 51     0.19531       0.19531      0.07806      0.00539      0.00612      0.07825      0.06898      0.05621
 52     0.19922       0.19922      0.07650      0.00415      0.00587      0.07662      0.05415      0.04326
 53     0.20313       0.20313      0.07350      0.00310      0.00541      0.07356      0.04212      0.03300
 54     0.20703       0.20703      0.06888      0.00190      0.00475      0.06891      0.02763      0.02124
 55     0.21094       0.21094      0.06310      0.00094      0.00398      0.06310      0.01493      0.01127
 56     0.21484       0.21484      0.05600     -0.00011      0.00314      0.05600     -0.00189     -0.00140
 57     0.21875       0.21875      0.04811     -0.00089      0.00232      0.04812     -0.01843     -0.01341
 58     0.22266       0.22266      0.03931     -0.00170      0.00155      0.03935     -0.04312     -0.03082
 59     0.22656       0.22656      0.03017     -0.00221      0.00091      0.03025     -0.07307     -0.05133
 60     0.23047       0.23047      0.02055     -0.00269      0.00043      0.02072     -0.13037     -0.09003
 61     0.23438       0.23438      0.01106     -0.00286      0.00013      0.01142     -0.25295     -0.17177
 62     0.23828       0.23828      0.00154     -0.00295      0.00001      0.00333     -1.08921     -0.72751
 63     0.24219       0.24219     -0.00739     -0.00271      0.00006      0.00787     -2.79070     -1.83393
 64     0.24609       0.24609     -0.01595     -0.00235      0.00026      0.01612     -2.99532     -1.93715
 65     0.25000       0.25000     -0.02353     -0.00166      0.00056      0.02359     -3.07116     -1.95516
 66     0.25391       0.25391     -0.03041     -0.00083      0.00093      0.03042     -3.11421     -1.95207
 67     0.25781       0.25781     -0.03601      0.00031      0.00130      0.03601      3.13290      1.93403
 68     0.26172       0.26172     -0.04068      0.00161      0.00166      0.04071      3.10210      1.88643
 69     0.26563       0.26563     -0.04388      0.00319      0.00194      0.04400      3.06905      1.83889
 70     0.26953       0.26953     -0.04605      0.00491      0.00214      0.04631      3.03527      1.79229
 71     0.27344       0.27344     -0.04668      0.00688      0.00223      0.04719      2.99530      1.74342
 72     0.27734       0.27734     -0.04629      0.00897      0.00222      0.04715      2.95026      1.69302
 73     0.28125       0.28125     -0.04444      0.01123      0.00210      0.04584      2.89406      1.63770
 74     0.28516       0.28516     -0.04169      0.01359      0.00192      0.04385      2.82657      1.57760
 75     0.28906       0.28906     -0.03765      0.01604      0.00167      0.04092      2.73889      1.50800
 76     0.29297       0.29297     -0.03294      0.01854      0.00143      0.03780      2.62904      1.42822
 77     0.29688       0.29688     -0.02722      0.02105      0.00118      0.03441      2.48322      1.33126
 78     0.30078       0.30078     -0.02112      0.02356      0.00100      0.03164      2.30172      1.21793
 79     0.30469       0.30469     -0.01433      0.02599      0.00088      0.02968      2.07470      1.08373
 80     0.30859       0.30859     -0.00752      0.02836      0.00086      0.02933      1.82994      0.94378
 81     0.31250       0.31250     -0.00036      0.03055      0.00093      0.03055      1.58244      0.80593
 82     0.31641       0.31641      0.00647      0.03264      0.00111      0.03327      1.37507      0.69167
 83     0.32031       0.32031      0.01330      0.03446      0.00136      0.03694      1.20236      0.59742
 84     0.32422       0.32422      0.01947      0.03612      0.00168      0.04104      1.07636      0.52837
 85     0.32813       0.32813      0.02535      0.03745      0.00204      0.04522      0.97575      0.47328
 86     0.33203       0.33203      0.03029      0.03858      0.00241      0.04905      0.90521      0.43390
 87     0.33594       0.33594      0.03470      0.03931      0.00275      0.05243      0.84759      0.40156
 88     0.33984       0.33984      0.03798      0.03982      0.00303      0.05503      0.80904      0.37889
 89     0.34375       0.34375      0.04059      0.03988      0.00324      0.05690      0.77667      0.35959
 90     0.34766       0.34766      0.04197      0.03974      0.00334      0.05780      0.75806      0.34703
 91     0.35156       0.35156      0.04262      0.03910      0.00335      0.05784      0.74237      0.33608
 92     0.35547       0.35547      0.04206      0.03828      0.00323      0.05687      0.73843      0.33062
 93     0.35938       0.35938      0.04081      0.03697      0.00303      0.05507      0.73604      0.32597
 94     0.36328       0.36328      0.03846      0.03551      0.00274      0.05235      0.74564      0.32667
 95     0.36719       0.36719      0.03555      0.03358      0.00239      0.04890      0.75680      0.32803
 96     0.37109       0.37109      0.03173      0.03156      0.00200      0.04475      0.78271      0.33569
 97     0.37500       0.37500      0.02757      0.02909      0.00161      0.04008      0.81221      0.34471
 98     0.37891       0.37891      0.02275      0.02663      0.00123      0.03502      0.86386      0.36286
 99     0.38281       0.38281      0.01785      0.02376      0.00088      0.02972      0.92645      0.38517
100     0.38672       0.38672      0.01258      0.02100      0.00060      0.02448      1.03098      0.42430
101     0.39063       0.39063      0.00753      0.01789      0.00038      0.01941      1.17242      0.47769
102     0.39453       0.39453      0.00240      0.01499      0.00023      0.01518      1.41223      0.56970
103     0.39844       0.39844     -0.00224      0.01180      0.00014      0.01201      1.75820      0.70231
104     0.40234       0.40234     -0.00667      0.00894      0.00012      0.01115      2.21186      0.87495
105     0.40625       0.40625     -0.01036      0.00585      0.00014      0.01189      2.62753      1.02938
106     0.41016       0.41016     -0.01361      0.00320      0.00020      0.01398      2.91050      1.12938
107     0.41406       0.41406     -0.01593      0.00038      0.00025      0.01594      3.11776      1.19838
108     0.41797       0.41797     -0.01766     -0.00189      0.00032      0.01776     -3.03502     -1.15568
109     0.42188       0.42188     -0.01834     -0.00430      0.00035      0.01884     -2.91140     -1.09834
110     0.42578       0.42578     -0.01835     -0.00605      0.00037      0.01932     -2.82304     -1.05524
111     0.42969       0.42969     -0.01730     -0.00793      0.00036      0.01903     -2.71163     -1.00438
112     0.43359       0.43359     -0.01557     -0.00906      0.00032      0.01802     -2.61460     -0.95971
113     0.43750       0.43750     -0.01286     -0.01033      0.00027      0.01649     -2.46453     -0.89655
114     0.44141       0.44141     -0.00957     -0.01077      0.00021      0.01441     -2.29705     -0.82823
115     0.44531       0.44531     -0.00544     -0.01140      0.00016      0.01263     -2.01559     -0.72037
116     0.44922       0.44922     -0.00091     -0.01113      0.00012      0.01117     -1.65246     -0.58545
117     0.45313       0.45313      0.00425     -0.01113      0.00014      0.01191     -1.20635     -0.42372
118     0.45703       0.45703      0.00956     -0.01016      0.00019      0.01394     -0.81577     -0.28408
119     0.46094       0.46094      0.01523     -0.00960      0.00032      0.01801     -0.56209     -0.19408
120     0.46484       0.46484      0.02080     -0.00797      0.00050      0.02227     -0.36609     -0.12534
121     0.46875       0.46875      0.02644     -0.00697      0.00075      0.02734     -0.25784     -0.08755
122     0.47266       0.47266      0.03169     -0.00477      0.00103      0.03204     -0.14950     -0.05034
123     0.47656       0.47656      0.03674     -0.00351      0.00136      0.03690     -0.09531     -0.03183
124     0.48047       0.48047      0.04113     -0.00080      0.00169      0.04114     -0.01936     -0.00641
125     0.48438       0.48438      0.04507      0.00045      0.00203      0.04508      0.00993      0.00326
126     0.48828       0.48828      0.04815      0.00374      0.00233      0.04829      0.07742      0.02524
127     0.49219       0.49219      0.05058      0.00436      0.00258      0.05077      0.08603      0.02782
128     0.49609       0.49609      0.05199      0.00918      0.00279      0.05279      0.17468      0.05604
129     0.50000       0.50000      0.05262      0.00000      0.00277      0.05262      0.00000      0.00000
NUMBER POINTS OUTPUT =         129
**** This image is ALIASED ****
INTEGRATED MTF AMPLITUDE FROM .25 TO .48 =       1.96299
if (mode = "nobatch" or mode = "inter")
end-if
otf1 inp=t2edge.img plotprof=t2edge plotout=test15
Beginning VICAR task otf1
OTF1 version 2015-08-10
NUMBER OF LINES PROCESSED=         20

 PIXEL  CYCLES/      FREQUENCY    REAL(FT)     IMAG.(FT)     INTENSITY    AMPLITUDE     PHASE     WAVE LENGT
         PIXEL                                                                         RADIANS      SHIFT
  1     0.00000       0.00000      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
  2     0.00391       0.00391      0.99971     -0.00000      0.99942      0.99971      0.00000      0.00000
  3     0.00781       0.00781      0.99798      0.00000      0.99597      0.99798      0.00000      0.00000
  4     0.01172       0.01172      0.99567     -0.00002      0.99137      0.99567      0.00000      0.00000
  5     0.01563       0.01563      0.99194     -0.00001      0.98395      0.99194      0.00000      0.00000
  6     0.01953       0.01953      0.98764     -0.00008      0.97543      0.98764      0.00000      0.00000
  7     0.02344       0.02344      0.98193     -0.00007      0.96418      0.98193      0.00000      0.00000
  8     0.02734       0.02734      0.97566     -0.00018      0.95192      0.97566      0.00000      0.00000
  9     0.03125       0.03125      0.96802     -0.00020      0.93706      0.96802      0.00000      0.00000
 10     0.03516       0.03516      0.95984     -0.00036      0.92129      0.95984      0.00000      0.00000
 11     0.03906       0.03906      0.95032     -0.00040      0.90311      0.95032      0.00000      0.00000
 12     0.04297       0.04297      0.94029     -0.00062      0.88415      0.94029      0.00000      0.00000
 13     0.04688       0.04688      0.92898     -0.00070      0.86300      0.92898      0.00000      0.00000
 14     0.05078       0.05078      0.91718     -0.00100      0.84122      0.91718     -0.00109     -0.00341
 15     0.05469       0.05469      0.90415     -0.00112      0.81749      0.90415     -0.00124     -0.00361
 16     0.05859       0.05859      0.89067     -0.00149      0.79330      0.89067     -0.00168     -0.00455
 17     0.06250       0.06250      0.87603     -0.00167      0.76743      0.87603     -0.00190     -0.00485
 18     0.06641       0.06641      0.86098     -0.00212      0.74130      0.86099     -0.00246     -0.00590
 19     0.07031       0.07031      0.84484     -0.00235      0.71376      0.84484     -0.00278     -0.00630
 20     0.07422       0.07422      0.82833     -0.00289      0.68615      0.82834     -0.00349     -0.00748
 21     0.07813       0.07813      0.81082     -0.00318      0.65743      0.81082     -0.00393     -0.00800
 22     0.08203       0.08203      0.79298     -0.00381      0.62884      0.79299     -0.00480     -0.00932
 23     0.08594       0.08594      0.77422     -0.00416      0.59944      0.77423     -0.00538     -0.00996
 24     0.08984       0.08984      0.75519     -0.00488      0.57034      0.75521     -0.00646     -0.01145
 25     0.09375       0.09375      0.73533     -0.00530      0.54074      0.73535     -0.00721     -0.01224
 26     0.09766       0.09766      0.71526     -0.00610      0.51163      0.71528     -0.00853     -0.01390
 27     0.10156       0.10156      0.69445     -0.00659      0.48230      0.69448     -0.00948     -0.01486
 28     0.10547       0.10547      0.67348     -0.00747      0.45363      0.67352     -0.01110     -0.01675
 29     0.10938       0.10938      0.65188     -0.00802      0.42501      0.65192     -0.01230     -0.01790
 30     0.11328       0.11328      0.63016     -0.00899      0.39719      0.63023     -0.01426     -0.02004
 31     0.11719       0.11719      0.60793     -0.00959      0.36967      0.60801     -0.01578     -0.02143
 32     0.12109       0.12109      0.58564     -0.01063      0.34309      0.58574     -0.01816     -0.02386
 33     0.12500       0.12500      0.56294     -0.01129      0.31703      0.56305     -0.02005     -0.02553
 34     0.12891       0.12891      0.54024     -0.01240      0.29201      0.54038     -0.02295     -0.02833
 35     0.13281       0.13281      0.51723     -0.01310      0.26770      0.51740     -0.02532     -0.03034
 36     0.13672       0.13672      0.49428     -0.01427      0.24452      0.49449     -0.02886     -0.03359
 37     0.14063       0.14063      0.47114     -0.01500      0.22220      0.47138     -0.03184     -0.03603
 38     0.14453       0.14453      0.44811     -0.01622      0.20106      0.44840     -0.03618     -0.03984
 39     0.14844       0.14844      0.42500     -0.01698      0.18091      0.42534     -0.03994     -0.04282
 40     0.15234       0.15234      0.40204     -0.01823      0.16197      0.40246     -0.04531     -0.04734
 41     0.15625       0.15625      0.37913     -0.01901      0.14410      0.37961     -0.05010     -0.05104
 42     0.16016       0.16016      0.35641     -0.02028      0.12744      0.35699     -0.05684     -0.05648
 43     0.16406       0.16406      0.33385     -0.02107      0.11190      0.33452     -0.06302     -0.06113
 44     0.16797       0.16797      0.31153     -0.02234      0.09755      0.31233     -0.07159     -0.06783
 45     0.17188       0.17188      0.28948     -0.02312      0.08433      0.29040     -0.07969     -0.07379
 46     0.17578       0.17578      0.26770     -0.02438      0.07226      0.26880     -0.09083     -0.08224
 47     0.17969       0.17969      0.24630     -0.02514      0.06129      0.24757     -0.10171     -0.09009
 48     0.18359       0.18359      0.22520     -0.02638      0.05141      0.22674     -0.11659     -0.10107
 49     0.18750       0.18750      0.20459     -0.02709      0.04259      0.20638     -0.13166     -0.11176
 50     0.19141       0.19141      0.18432     -0.02829      0.03477      0.18648     -0.15230     -0.12664
 51     0.19531       0.19531      0.16463     -0.02896      0.02794      0.16716     -0.17412     -0.14188
 52     0.19922       0.19922      0.14530     -0.03010      0.02202      0.14839     -0.20424     -0.16317
 53     0.20313       0.20313      0.12666     -0.03070      0.01698      0.13033     -0.23779     -0.18632
 54     0.20703       0.20703      0.10839     -0.03176      0.01276      0.11295     -0.28506     -0.21914
 55     0.21094       0.21094      0.09089     -0.03228      0.00930      0.09646     -0.34129     -0.25751
 56     0.21484       0.21484      0.07378     -0.03325      0.00655      0.08093     -0.42344     -0.31369
 57     0.21875       0.21875      0.05753     -0.03368      0.00444      0.06667     -0.52965     -0.38536
 58     0.22266       0.22266      0.04167     -0.03454      0.00293      0.05413     -0.69217     -0.49476
 59     0.22656       0.22656      0.02675     -0.03487      0.00193      0.04395     -0.91633     -0.64370
 60     0.23047       0.23047      0.01221     -0.03560      0.00142      0.03764     -1.24031     -0.85652
 61     0.23438       0.23438     -0.00130     -0.03581      0.00128      0.03583     -1.60719     -1.09138
 62     0.23828       0.23828     -0.01445     -0.03640      0.00153      0.03917     -1.94871     -1.30160
 63     0.24219       0.24219     -0.02651     -0.03648      0.00203      0.04509     -2.19931     -1.44529
 64     0.24609       0.24609     -0.03822     -0.03692      0.00282      0.05314     -2.37350     -1.53500
 65     0.25000       0.25000     -0.04878     -0.03686      0.00374      0.06114     -2.49459     -1.58811
 66     0.25391       0.25391     -0.05902     -0.03714      0.00486      0.06973     -2.57997     -1.61719
 67     0.25781       0.25781     -0.06806     -0.03692      0.00599      0.07743     -2.64450     -1.63253
 68     0.26172       0.26172     -0.07680     -0.03703      0.00727      0.08526     -2.69233     -1.63724
 69     0.26563       0.26563     -0.08429     -0.03666      0.00845      0.09192     -2.73136     -1.63655
 70     0.26953       0.26953     -0.09154     -0.03658      0.00972      0.09858     -2.76139     -1.63056
 71     0.27344       0.27344     -0.09750     -0.03606      0.01081      0.10395     -2.78736     -1.62239
 72     0.27734       0.27734     -0.10325     -0.03579      0.01194      0.10928     -2.80791     -1.61133
 73     0.28125       0.28125     -0.10768     -0.03510      0.01283      0.11326     -2.82647     -1.59946
 74     0.28516       0.28516     -0.11197     -0.03464      0.01374      0.11720     -2.84154     -1.58596
 75     0.28906       0.28906     -0.11491     -0.03379      0.01435      0.11977     -2.85557     -1.57225
 76     0.29297       0.29297     -0.11776     -0.03313      0.01496      0.12233     -2.86731     -1.55766
 77     0.29688       0.29688     -0.11925     -0.03213      0.01525      0.12350     -2.87843     -1.54313
 78     0.30078       0.30078     -0.12071     -0.03127      0.01555      0.12470     -2.88811     -1.52821
 79     0.30469       0.30469     -0.12081     -0.03011      0.01550      0.12450     -2.89733     -1.51343
 80     0.30859       0.30859     -0.12095     -0.02906      0.01547      0.12439     -2.90580     -1.49864
 81     0.31250       0.31250     -0.11972     -0.02775      0.01510      0.12289     -2.91380     -1.48399
 82     0.31641       0.31641     -0.11861     -0.02651      0.01477      0.12154     -2.92169     -1.46963
 83     0.32031       0.32031     -0.11613     -0.02507      0.01411      0.11880     -2.92899     -1.45534
 84     0.32422       0.32422     -0.11385     -0.02364      0.01352      0.11628     -2.93684     -1.44166
 85     0.32813       0.32813     -0.11021     -0.02207      0.01263      0.11240     -2.94392     -1.42793
 86     0.33203       0.33203     -0.10687     -0.02048      0.01184      0.10881     -2.95229     -1.41514
 87     0.33594       0.33594     -0.10217     -0.01880      0.01079      0.10388     -2.95964     -1.40217
 88     0.33984       0.33984     -0.09785     -0.01703      0.00986      0.09932     -2.96923     -1.39054
 89     0.34375       0.34375     -0.09219     -0.01526      0.00873      0.09345     -2.97751     -1.37857
 90     0.34766       0.34766     -0.08701     -0.01335      0.00775      0.08803     -2.98935     -1.36851
 91     0.35156       0.35156     -0.08052     -0.01151      0.00662      0.08134     -2.99962     -1.35795
 92     0.35547       0.35547     -0.07459     -0.00946      0.00565      0.07519     -3.01546     -1.35012
 93     0.35938       0.35938     -0.06738     -0.00757      0.00460      0.06780     -3.02974     -1.34177
 94     0.36328       0.36328     -0.06083     -0.00540      0.00373      0.06107     -3.05309     -1.33757
 95     0.36719       0.36719     -0.05302     -0.00348      0.00282      0.05313     -3.07602     -1.33328
 96     0.37109       0.37109     -0.04596     -0.00121      0.00211      0.04598     -3.11530     -1.33609
 97     0.37500       0.37500     -0.03768      0.00071      0.00142      0.03769      3.12284      1.32538
 98     0.37891       0.37891     -0.03026      0.00306      0.00092      0.03041      3.04070      1.27721
 99     0.38281       0.38281     -0.02164      0.00495      0.00049      0.02220      2.91672      1.21263
100     0.38672       0.38672     -0.01396      0.00737      0.00025      0.01579      2.65584      1.09302
101     0.39063       0.39063     -0.00513      0.00920      0.00011      0.01053      2.07963      0.84732
102     0.39453       0.39453      0.00267      0.01166      0.00014      0.01197      1.34616      0.54304
103     0.39844       0.39844      0.01158      0.01340      0.00031      0.01771      0.85817      0.34279
104     0.40234       0.40234      0.01937      0.01589      0.00063      0.02506      0.68719      0.27183
105     0.40625       0.40625      0.02824      0.01750      0.00110      0.03323      0.55488      0.21738
106     0.41016       0.41016      0.03590      0.02001      0.00169      0.04110      0.50847      0.19730
107     0.41406       0.41406      0.04461      0.02146      0.00245      0.04950      0.44836      0.17234
108     0.41797       0.41797      0.05203      0.02396      0.00328      0.05728      0.43159      0.16434
109     0.42188       0.42188      0.06045      0.02521      0.00429      0.06549      0.39508      0.14905
110     0.42578       0.42578      0.06751      0.02769      0.00532      0.07297      0.38931      0.14552
111     0.42969       0.42969      0.07554      0.02870      0.00653      0.08081      0.36310      0.13449
112     0.43359       0.43359      0.08213      0.03117      0.00772      0.08785      0.36269      0.13313
113     0.43750       0.43750      0.08967      0.03188      0.00906      0.09517      0.34164      0.12428
114     0.44141       0.44141      0.09570      0.03433      0.01034      0.10167      0.34446      0.12420
115     0.44531       0.44531      0.10264      0.03470      0.01174      0.10835      0.32606      0.11653
116     0.44922       0.44922      0.10802      0.03715      0.01305      0.11423      0.33128      0.11737
117     0.45313       0.45313      0.11428      0.03711      0.01444      0.12016      0.31400      0.11029
118     0.45703       0.45703      0.11893      0.03960      0.01571      0.12535      0.32140      0.11192
119     0.46094       0.46094      0.12444      0.03905      0.01701      0.13042      0.30409      0.10500
120     0.46484       0.46484      0.12829      0.04165      0.01819      0.13488      0.31392      0.10748
121     0.46875       0.46875      0.13297      0.04045      0.01932      0.13899      0.29533      0.10027
122     0.47266       0.47266      0.13597      0.04332      0.02036      0.14270      0.30843      0.10386
123     0.47656       0.47656      0.13978      0.04122      0.02124      0.14573      0.28677      0.09577
124     0.48047       0.48047      0.14187      0.04470      0.02212      0.14874      0.30521      0.10110
125     0.48438       0.48438      0.14476      0.04112      0.02265      0.15049      0.27676      0.09094
126     0.48828       0.48828      0.14592      0.04617      0.02342      0.15305      0.30644      0.09988
127     0.49219       0.49219      0.14787      0.03917      0.02340      0.15297      0.25893      0.08373
128     0.49609       0.49609      0.14806      0.05082      0.02450      0.15654      0.33061      0.10606
129     0.50000       0.50000      0.14903      0.00000      0.02221      0.14903      0.00000      0.00000
NUMBER POINTS OUTPUT =         129
**** This image is ALIASED ****
INTEGRATED MTF AMPLITUDE FROM .25 TO .48 =       5.30280
if (mode = "nobatch" or mode = "inter")
end-if
otf1 inp=t2edgeN.img plotprof=t2edgeN plotout=test16
Beginning VICAR task otf1
OTF1 version 2015-08-10
NUMBER OF BAD PIXELS ON RIGHT=          1
LINE NUMBER=          4
NUMBER OF BAD PIXELS ON LEFT=          1
LINE NUMBER=          4
NUMBER OF BAD PIXELS ON LEFT=          1
LINE NUMBER=          5
NUMBER OF BAD PIXELS ON RIGHT=          1
LINE NUMBER=         12
NUMBER OF BAD PIXELS ON LEFT=          1
LINE NUMBER=         12
NUMBER OF BAD PIXELS ON LEFT=          1
LINE NUMBER=         14
NUMBER OF BAD PIXELS ON RIGHT=          1
LINE NUMBER=         15
NUMBER OF BAD PIXELS ON LEFT=          1
LINE NUMBER=         15
NUMBER OF LINES PROCESSED=         20

 PIXEL  CYCLES/      FREQUENCY    REAL(FT)     IMAG.(FT)     INTENSITY    AMPLITUDE     PHASE     WAVE LENGT
         PIXEL                                                                         RADIANS      SHIFT
  1     0.00000       0.00000      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
  2     0.00391       0.00391      0.99974     -0.00001      0.99948      0.99974      0.00000      0.00000
  3     0.00781       0.00781      0.99796      0.00001      0.99593      0.99796      0.00000      0.00000
  4     0.01172       0.01172      0.99567     -0.00003      0.99135      0.99567      0.00000      0.00000
  5     0.01563       0.01563      0.99186     -0.00001      0.98379      0.99186      0.00000      0.00000
  6     0.01953       0.01953      0.98755     -0.00008      0.97525      0.98755      0.00000      0.00000
  7     0.02344       0.02344      0.98174     -0.00007      0.96382      0.98174      0.00000      0.00000
  8     0.02734       0.02734      0.97545     -0.00019      0.95150      0.97545      0.00000      0.00000
  9     0.03125       0.03125      0.96769     -0.00020      0.93643      0.96769      0.00000      0.00000
 10     0.03516       0.03516      0.95946     -0.00038      0.92057      0.95946      0.00000      0.00000
 11     0.03906       0.03906      0.94981     -0.00042      0.90215      0.94982      0.00000      0.00000
 12     0.04297       0.04297      0.93972     -0.00067      0.88307      0.93972      0.00000      0.00000
 13     0.04688       0.04688      0.92825     -0.00074      0.86165      0.92825      0.00000      0.00000
 14     0.05078       0.05078      0.91636     -0.00106      0.83973      0.91637     -0.00116     -0.00363
 15     0.05469       0.05469      0.90317     -0.00118      0.81572      0.90317     -0.00131     -0.00380
 16     0.05859       0.05859      0.88959     -0.00159      0.79137      0.88959     -0.00178     -0.00484
 17     0.06250       0.06250      0.87476     -0.00176      0.76522      0.87477     -0.00201     -0.00511
 18     0.06641       0.06641      0.85959     -0.00225      0.73890      0.85959     -0.00262     -0.00627
 19     0.07031       0.07031      0.84326     -0.00248      0.71109      0.84326     -0.00294     -0.00665
 20     0.07422       0.07422      0.82661     -0.00306      0.68329      0.82661     -0.00371     -0.00795
 21     0.07813       0.07813      0.80888     -0.00336      0.65431      0.80889     -0.00415     -0.00845
 22     0.08203       0.08203      0.79089     -0.00403      0.62553      0.79090     -0.00510     -0.00990
 23     0.08594       0.08594      0.77192     -0.00439      0.59587      0.77193     -0.00569     -0.01053
 24     0.08984       0.08984      0.75272     -0.00516      0.56661      0.75273     -0.00686     -0.01215
 25     0.09375       0.09375      0.73263     -0.00558      0.53678      0.73265     -0.00762     -0.01294
 26     0.09766       0.09766      0.71237     -0.00645      0.50751      0.71240     -0.00905     -0.01476
 27     0.10156       0.10156      0.69133     -0.00693      0.47798      0.69136     -0.01003     -0.01572
 28     0.10547       0.10547      0.67016     -0.00789      0.44917      0.67020     -0.01177     -0.01777
 29     0.10938       0.10938      0.64832     -0.00844      0.42038      0.64837     -0.01301     -0.01893
 30     0.11328       0.11328      0.62640     -0.00948      0.39246      0.62647     -0.01513     -0.02125
 31     0.11719       0.11719      0.60392     -0.01008      0.36482      0.60400     -0.01669     -0.02267
 32     0.12109       0.12109      0.58141     -0.01120      0.33817      0.58152     -0.01926     -0.02531
 33     0.12500       0.12500      0.55846     -0.01185      0.31202      0.55859     -0.02122     -0.02702
 34     0.12891       0.12891      0.53554     -0.01304      0.28697      0.53569     -0.02434     -0.03005
 35     0.13281       0.13281      0.51228     -0.01373      0.26262      0.51247     -0.02680     -0.03212
 36     0.13672       0.13672      0.48910     -0.01498      0.23944      0.48933     -0.03061     -0.03564
 37     0.14063       0.14063      0.46571     -0.01571      0.21714      0.46598     -0.03372     -0.03816
 38     0.14453       0.14453      0.44244     -0.01700      0.19604      0.44277     -0.03840     -0.04228
 39     0.14844       0.14844      0.41909     -0.01775      0.17595      0.41946     -0.04233     -0.04539
 40     0.15234       0.15234      0.39589     -0.01907      0.15709      0.39635     -0.04814     -0.05029
 41     0.15625       0.15625      0.37273     -0.01984      0.13932      0.37326     -0.05317     -0.05416
 42     0.16016       0.16016      0.34977     -0.02117      0.12279      0.35041     -0.06046     -0.06008
 43     0.16406       0.16406      0.32697     -0.02194      0.10739      0.32771     -0.06700     -0.06499
 44     0.16797       0.16797      0.30441     -0.02327      0.09321      0.30530     -0.07631     -0.07231
 45     0.17188       0.17188      0.28212     -0.02402      0.08017      0.28314     -0.08495     -0.07866
 46     0.17578       0.17578      0.26010     -0.02534      0.06829      0.26133     -0.09713     -0.08794
 47     0.17969       0.17969      0.23847     -0.02606      0.05755      0.23989     -0.10885     -0.09641
 48     0.18359       0.18359      0.21713     -0.02734      0.04789      0.21885     -0.12528     -0.10860
 49     0.18750       0.18750      0.19630     -0.02802      0.03932      0.19829     -0.14176     -0.12033
 50     0.19141       0.19141      0.17579     -0.02925      0.03176      0.17820     -0.16487     -0.13709
 51     0.19531       0.19531      0.15589     -0.02986      0.02519      0.15872     -0.18924     -0.15421
 52     0.19922       0.19922      0.13632     -0.03102      0.01955      0.13981     -0.22373     -0.17874
 53     0.20313       0.20313      0.11748     -0.03155      0.01480      0.12164     -0.26239     -0.20559
 54     0.20703       0.20703      0.09897     -0.03262      0.01086      0.10421     -0.31841     -0.24477
 55     0.21094       0.21094      0.08128     -0.03306      0.00770      0.08775     -0.38632     -0.29149
 56     0.21484       0.21484      0.06394     -0.03403      0.00525      0.07243     -0.48904     -0.36228
 57     0.21875       0.21875      0.04751     -0.03436      0.00344      0.05863     -0.62611     -0.45554
 58     0.22266       0.22266      0.03142     -0.03520      0.00223      0.04719     -0.84199     -0.60186
 59     0.22656       0.22656      0.01634     -0.03541      0.00152      0.03900     -1.13851     -0.79978
 60     0.23047       0.23047      0.00158     -0.03611      0.00131      0.03614     -1.52703     -1.05452
 61     0.23438       0.23438     -0.01209     -0.03619      0.00146      0.03815     -1.89325     -1.28563
 62     0.23828       0.23828     -0.02545     -0.03673      0.00200      0.04469     -2.17681     -1.45395
 63     0.24219       0.24219     -0.03766     -0.03666      0.00276      0.05256     -2.36954     -1.55716
 64     0.24609       0.24609     -0.04957     -0.03703      0.00383      0.06188     -2.49999     -1.61680
 65     0.25000       0.25000     -0.06026     -0.03681      0.00499      0.07062     -2.59322     -1.65089
 66     0.25391       0.25391     -0.07070     -0.03700      0.00637      0.07980     -2.65946     -1.66702
 67     0.25781       0.25781     -0.07985     -0.03662      0.00772      0.08784     -2.71163     -1.67397
 68     0.26172       0.26172     -0.08879     -0.03661      0.00922      0.09604     -2.75050     -1.67262
 69     0.26563       0.26563     -0.09638     -0.03606      0.01059      0.10291     -2.78359     -1.66785
 70     0.26953       0.26953     -0.10381     -0.03585      0.01206      0.10983     -2.80909     -1.65873
 71     0.27344       0.27344     -0.10985     -0.03512      0.01330      0.11533     -2.83212     -1.64844
 72     0.27734       0.27734     -0.11578     -0.03471      0.01461      0.12087     -2.85035     -1.63569
 73     0.28125       0.28125     -0.12029     -0.03381      0.01561      0.12495     -2.86758     -1.62272
 74     0.28516       0.28516     -0.12474     -0.03318      0.01666      0.12907     -2.88162     -1.60833
 75     0.28906       0.28906     -0.12773     -0.03211      0.01735      0.13171     -2.89531     -1.59413
 76     0.29297       0.29297     -0.13074     -0.03126      0.01807      0.13443     -2.90687     -1.57915
 77     0.29688       0.29688     -0.13227     -0.03003      0.01840      0.13563     -2.91836     -1.56454
 78     0.30078       0.30078     -0.13388     -0.02897      0.01876      0.13698     -2.92851     -1.54959
 79     0.30469       0.30469     -0.13400     -0.02757      0.01872      0.13681     -2.93869     -1.53504
 80     0.30859       0.30859     -0.13428     -0.02630      0.01872      0.13684     -2.94818     -1.52050
 81     0.31250       0.31250     -0.13306     -0.02475      0.01832      0.13534     -2.95769     -1.50634
 82     0.31641       0.31641     -0.13208     -0.02328      0.01799      0.13411     -2.96714     -1.49250
 83     0.32031       0.32031     -0.12959     -0.02159      0.01726      0.13137     -2.97654     -1.47896
 84     0.32422       0.32422     -0.12743     -0.01992      0.01663      0.12898     -2.98654     -1.46605
 85     0.32813       0.32813     -0.12376     -0.01810      0.01564      0.12508     -2.99636     -1.45337
 86     0.33203       0.33203     -0.12052     -0.01625      0.01479      0.12161     -3.00756     -1.44164
 87     0.33594       0.33594     -0.11577     -0.01433      0.01361      0.11666     -3.01847     -1.43004
 88     0.33984       0.33984     -0.11154     -0.01230      0.01259      0.11222     -3.03173     -1.41981
 89     0.34375       0.34375     -0.10583     -0.01029      0.01131      0.10633     -3.04463     -1.40965
 90     0.34766       0.34766     -0.10072     -0.00812      0.01021      0.10105     -3.06119     -1.40140
 91     0.35156       0.35156     -0.09415     -0.00604      0.00890      0.09434     -3.07751     -1.39321
 92     0.35547       0.35547     -0.08828     -0.00373      0.00781      0.08836     -3.09942     -1.38771
 93     0.35938       0.35938     -0.08097     -0.00161      0.00656      0.08098     -3.12168     -1.38249
 94     0.36328       0.36328     -0.07446      0.00082      0.00554      0.07446      3.13054      1.37150
 95     0.36719       0.36719     -0.06653      0.00295      0.00443      0.06659      3.09734      1.34252
 96     0.37109       0.37109     -0.05950      0.00548      0.00357      0.05975      3.04974      1.30797
 97     0.37500       0.37500     -0.05108      0.00758      0.00267      0.05164      2.99420      1.27078
 98     0.37891       0.37891     -0.04366      0.01020      0.00201      0.04483      2.91213      1.22321
 99     0.38281       0.38281     -0.03487      0.01225      0.00137      0.03696      2.80383      1.16570
100     0.38672       0.38672     -0.02718      0.01492      0.00096      0.03101      2.63969      1.08637
101     0.39063       0.39063     -0.01816      0.01688      0.00061      0.02480      2.39277      0.97490
102     0.39453       0.39453     -0.01034      0.01959      0.00049      0.02215      2.05638      0.82955
103     0.39844       0.39844     -0.00121      0.02143      0.00046      0.02146      1.62721      0.64999
104     0.40234       0.40234      0.00663      0.02415      0.00063      0.02505      1.30279      0.51534
105     0.40625       0.40625      0.01574      0.02583      0.00092      0.03025      1.02351      0.40098
106     0.41016       0.41016      0.02348      0.02855      0.00137      0.03697      0.88258      0.34247
107     0.41406       0.41406      0.03245      0.03003      0.00195      0.04421      0.74671      0.28702
108     0.41797       0.41797      0.03997      0.03273      0.00267      0.05166      0.68624      0.26131
109     0.42188       0.42188      0.04868      0.03397      0.00352      0.05936      0.60922      0.22983
110     0.42578       0.42578      0.05586      0.03664      0.00446      0.06681      0.58050      0.21699
111     0.42969       0.42969      0.06421      0.03758      0.00554      0.07440      0.52953      0.19614
112     0.43359       0.43359      0.07095      0.04022      0.00665      0.08156      0.51568      0.18929
113     0.43750       0.43750      0.07884      0.04082      0.00788      0.08878      0.47776      0.17380
114     0.44141       0.44141      0.08504      0.04343      0.00912      0.09549      0.47213      0.17023
115     0.44531       0.44531      0.09236      0.04362      0.01043      0.10214      0.44123      0.15769
116     0.44922       0.44922      0.09794      0.04622      0.01173      0.10830      0.44092      0.15621
117     0.45313       0.45313      0.10461      0.04593      0.01305      0.11424      0.41371      0.14531
118     0.45703       0.45703      0.10948      0.04856      0.01434      0.11977      0.41748      0.14538
119     0.46094       0.46094      0.11542      0.04768      0.01560      0.12488      0.39175      0.13527
120     0.46484       0.46484      0.11953      0.05044      0.01683      0.12974      0.39935      0.13673
121     0.46875       0.46875      0.12467      0.04881      0.01793      0.13389      0.37316      0.12670
122     0.47266       0.47266      0.12795      0.05188      0.01906      0.13807      0.38521      0.12971
123     0.47656       0.47656      0.13225      0.04920      0.01991      0.14110      0.35616      0.11895
124     0.48047       0.48047      0.13465      0.05298      0.02094      0.14470      0.37484      0.12417
125     0.48438       0.48438      0.13806      0.04859      0.02142      0.14636      0.33839      0.11119
126     0.48828       0.48828      0.13955      0.05419      0.02241      0.14970      0.37039      0.12073
127     0.49219       0.49219      0.14203      0.04583      0.02227      0.14924      0.31215      0.10094
128     0.49609       0.49609      0.14260      0.05908      0.02383      0.15436      0.39279      0.12601
129     0.50000       0.50000      0.14402      0.00000      0.02074      0.14402      0.00000      0.00000
NUMBER POINTS OUTPUT =         129
**** This image is ALIASED ****
INTEGRATED MTF AMPLITUDE FROM .25 TO .48 =       5.67107
if (mode = "nobatch" or mode = "inter")
end-if
otf1 inp=u2edge.img plotprof=u2edge plotout=test17
Beginning VICAR task otf1
OTF1 version 2015-08-10
NUMBER OF LINES PROCESSED=         20

 PIXEL  CYCLES/      FREQUENCY    REAL(FT)     IMAG.(FT)     INTENSITY    AMPLITUDE     PHASE     WAVE LENGT
         PIXEL                                                                         RADIANS      SHIFT
  1     0.00000       0.00000      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
  2     0.00391       0.00391      0.99866     -0.00000      0.99733      0.99866      0.00000      0.00000
  3     0.00781       0.00781      0.99467     -0.00001      0.98937      0.99467      0.00000      0.00000
  4     0.01172       0.01172      0.98804     -0.00002      0.97623      0.98804      0.00000      0.00000
  5     0.01563       0.01563      0.97882     -0.00005      0.95809      0.97882      0.00000      0.00000
  6     0.01953       0.01953      0.96706     -0.00009      0.93520      0.96706      0.00000      0.00000
  7     0.02344       0.02344      0.95283     -0.00016      0.90788      0.95283      0.00000      0.00000
  8     0.02734       0.02734      0.93621     -0.00025      0.87649      0.93621      0.00000      0.00000
  9     0.03125       0.03125      0.91732     -0.00037      0.84147      0.91732      0.00000      0.00000
 10     0.03516       0.03516      0.89625     -0.00052      0.80327      0.89625      0.00000      0.00000
 11     0.03906       0.03906      0.87314     -0.00070      0.76238      0.87314      0.00000      0.00000
 12     0.04297       0.04297      0.84813     -0.00092      0.71932      0.84813     -0.00108     -0.00400
 13     0.04688       0.04688      0.82135     -0.00117      0.67462      0.82135     -0.00142     -0.00484
 14     0.05078       0.05078      0.79297     -0.00146      0.62881      0.79298     -0.00184     -0.00575
 15     0.05469       0.05469      0.76316     -0.00178      0.58242      0.76316     -0.00233     -0.00679
 16     0.05859       0.05859      0.73208     -0.00214      0.53594      0.73208     -0.00292     -0.00793
 17     0.06250       0.06250      0.69991     -0.00253      0.48988      0.69991     -0.00362     -0.00921
 18     0.06641       0.06641      0.66683     -0.00295      0.44467      0.66684     -0.00443     -0.01061
 19     0.07031       0.07031      0.63304     -0.00341      0.40075      0.63305     -0.00538     -0.01218
 20     0.07422       0.07422      0.59870     -0.00388      0.35846      0.59872     -0.00648     -0.01390
 21     0.07813       0.07813      0.56403     -0.00438      0.31814      0.56404     -0.00777     -0.01583
 22     0.08203       0.08203      0.52918     -0.00490      0.28006      0.52921     -0.00925     -0.01795
 23     0.08594       0.08594      0.49437     -0.00543      0.24443      0.49440     -0.01097     -0.02032
 24     0.08984       0.08984      0.45975     -0.00596      0.21141      0.45979     -0.01295     -0.02295
 25     0.09375       0.09375      0.42551     -0.00649      0.18110      0.42556     -0.01526     -0.02590
 26     0.09766       0.09766      0.39182     -0.00702      0.15357      0.39188     -0.01791     -0.02918
 27     0.10156       0.10156      0.35884     -0.00753      0.12882      0.35891     -0.02099     -0.03289
 28     0.10547       0.10547      0.32671     -0.00803      0.10681      0.32681     -0.02456     -0.03706
 29     0.10938       0.10938      0.29560     -0.00850      0.08745      0.29572     -0.02873     -0.04181
 30     0.11328       0.11328      0.26563     -0.00893      0.07064      0.26578     -0.03359     -0.04720
 31     0.11719       0.11719      0.23692     -0.00932      0.05622      0.23710     -0.03932     -0.05340
 32     0.12109       0.12109      0.20959     -0.00966      0.04402      0.20981     -0.04606     -0.06054
 33     0.12500       0.12500      0.18373     -0.00995      0.03386      0.18400     -0.05410     -0.06888
 34     0.12891       0.12891      0.15944     -0.01017      0.02552      0.15976     -0.06371     -0.07865
 35     0.13281       0.13281      0.13678     -0.01033      0.01881      0.13717     -0.07536     -0.09030
 36     0.13672       0.13672      0.11581     -0.01040      0.01352      0.11628     -0.08958     -0.10428
 37     0.14063       0.14063      0.09659     -0.01040      0.00944      0.09715     -0.10726     -0.12139
 38     0.14453       0.14453      0.07914     -0.01031      0.00637      0.07981     -0.12951     -0.14261
 39     0.14844       0.14844      0.06349     -0.01013      0.00413      0.06429     -0.15817     -0.16959
 40     0.15234       0.15234      0.04963     -0.00984      0.00256      0.05060     -0.19583     -0.20458
 41     0.15625       0.15625      0.03756     -0.00947      0.00150      0.03873     -0.24693     -0.25152
 42     0.16016       0.16016      0.02726     -0.00898      0.00082      0.02870     -0.31838     -0.31639
 43     0.16406       0.16406      0.01869     -0.00840      0.00042      0.02049     -0.42234     -0.40970
 44     0.16797       0.16797      0.01182     -0.00771      0.00020      0.01411     -0.57786     -0.54754
 45     0.17188       0.17188      0.00658     -0.00691      0.00009      0.00954     -0.81025     -0.75029
 46     0.17578       0.17578      0.00291     -0.00601      0.00004      0.00668     -1.12051     -1.01452
 47     0.17969       0.17969      0.00072     -0.00501      0.00003      0.00506     -1.42800     -1.26483
 48     0.18359       0.18359     -0.00006     -0.00390      0.00002      0.00390     -1.58530     -1.37427
 49     0.18750       0.18750      0.00047     -0.00270      0.00001      0.00274     -1.39723     -1.18600
 50     0.19141       0.19141      0.00222     -0.00141      0.00001      0.00263     -0.56506     -0.46985
 51     0.19531       0.19531      0.00507     -0.00003      0.00003      0.00507     -0.00522     -0.00425
 52     0.19922       0.19922      0.00892      0.00144      0.00008      0.00903      0.15975      0.12762
 53     0.20313       0.20313      0.01364      0.00297      0.00019      0.01396      0.21468      0.16821
 54     0.20703       0.20703      0.01914      0.00458      0.00039      0.01968      0.23486      0.18055
 55     0.21094       0.21094      0.02527      0.00624      0.00068      0.02603      0.24216      0.18271
 56     0.21484       0.21484      0.03194      0.00795      0.00108      0.03292      0.24407      0.18081
 57     0.21875       0.21875      0.03901      0.00971      0.00162      0.04020      0.24387      0.17743
 58     0.22266       0.22266      0.04638      0.01149      0.00228      0.04778      0.24280      0.17355
 59     0.22656       0.22656      0.05392      0.01329      0.00308      0.05553      0.24164      0.16975
 60     0.23047       0.23047      0.06153      0.01510      0.00401      0.06335      0.24058      0.16614
 61     0.23438       0.23438      0.06910      0.01690      0.00506      0.07113      0.23989      0.16290
 62     0.23828       0.23828      0.07653      0.01869      0.00621      0.07878      0.23952      0.15999
 63     0.24219       0.24219      0.08372      0.02045      0.00743      0.08618      0.23962      0.15747
 64     0.24609       0.24609      0.09059      0.02218      0.00870      0.09326      0.24009      0.15527
 65     0.25000       0.25000      0.09705      0.02385      0.00999      0.09994      0.24102      0.15344
 66     0.25391       0.25391      0.10304      0.02547      0.01127      0.10614      0.24231      0.15189
 67     0.25781       0.25781      0.10847      0.02701      0.01250      0.11179      0.24406      0.15067
 68     0.26172       0.26172      0.11332      0.02847      0.01365      0.11684      0.24617      0.14970
 69     0.26563       0.26563      0.11751      0.02984      0.01470      0.12124      0.24872      0.14903
 70     0.26953       0.26953      0.12101      0.03111      0.01561      0.12495      0.25163      0.14859
 71     0.27344       0.27344      0.12380      0.03227      0.01637      0.12793      0.25500      0.14842
 72     0.27734       0.27734      0.12585      0.03331      0.01695      0.13018      0.25873      0.14847
 73     0.28125       0.28125      0.12715      0.03422      0.01734      0.13167      0.26294      0.14879
 74     0.28516       0.28516      0.12770      0.03500      0.01753      0.13241      0.26754      0.14932
 75     0.28906       0.28906      0.12750      0.03565      0.01753      0.13240      0.27266      0.15012
 76     0.29297       0.29297      0.12658      0.03615      0.01733      0.13164      0.27821      0.15114
 77     0.29688       0.29688      0.12494      0.03652      0.01694      0.13017      0.28434      0.15244
 78     0.30078       0.30078      0.12263      0.03672      0.01639      0.12801      0.29098      0.15397
 79     0.30469       0.30469      0.11967      0.03679      0.01567      0.12520      0.29828      0.15581
 80     0.30859       0.30859      0.11611      0.03670      0.01483      0.12177      0.30618      0.15791
 81     0.31250       0.31250      0.11199      0.03648      0.01387      0.11778      0.31486      0.16036
 82     0.31641       0.31641      0.10738      0.03609      0.01283      0.11328      0.32428      0.16312
 83     0.32031       0.32031      0.10232      0.03558      0.01173      0.10832      0.33465      0.16628
 84     0.32422       0.32422      0.09687      0.03492      0.01060      0.10297      0.34597      0.16983
 85     0.32813       0.32813      0.09110      0.03413      0.00946      0.09728      0.35846      0.17387
 86     0.33203       0.33203      0.08507      0.03321      0.00834      0.09133      0.37219      0.17840
 87     0.33594       0.33594      0.07886      0.03218      0.00725      0.08517      0.38745      0.18356
 88     0.33984       0.33984      0.07251      0.03103      0.00622      0.07887      0.40435      0.18937
 89     0.34375       0.34375      0.06611      0.02978      0.00526      0.07251      0.42331      0.19599
 90     0.34766       0.34766      0.05970      0.02844      0.00437      0.06613      0.44456      0.20352
 91     0.35156       0.35156      0.05337      0.02702      0.00358      0.05982      0.46866      0.21217
 92     0.35547       0.35547      0.04715      0.02552      0.00287      0.05362      0.49609      0.22212
 93     0.35938       0.35938      0.04113      0.02397      0.00227      0.04760      0.52766      0.23368
 94     0.36328       0.36328      0.03533      0.02236      0.00175      0.04181      0.56430      0.24722
 95     0.36719       0.36719      0.02982      0.02072      0.00132      0.03632      0.60728      0.26322
 96     0.37109       0.37109      0.02464      0.01906      0.00097      0.03115      0.65837      0.28236
 97     0.37500       0.37500      0.01983      0.01738      0.00070      0.02637      0.71979      0.30549
 98     0.37891       0.37891      0.01540      0.01570      0.00048      0.02199      0.79493      0.33390
 99     0.38281       0.38281      0.01142      0.01403      0.00033      0.01809      0.88775      0.36909
100     0.38672       0.38672      0.00787      0.01239      0.00022      0.01468      1.00457      0.41343
101     0.39063       0.39063      0.00481      0.01078      0.00014      0.01180      1.15136      0.46911
102     0.39453       0.39453      0.00220      0.00921      0.00009      0.00947      1.33625      0.53905
103     0.39844       0.39844      0.00009      0.00770      0.00006      0.00770      1.55937      0.62289
104     0.40234       0.40234     -0.00156      0.00626      0.00004      0.00645      1.81526      0.71806
105     0.40625       0.40625     -0.00273      0.00489      0.00003      0.00560      2.07993      0.81485
106     0.41016       0.41016     -0.00345      0.00360      0.00002      0.00499      2.33554      0.90627
107     0.41406       0.41406     -0.00373      0.00240      0.00002      0.00443      2.56999      0.98784
108     0.41797       0.41797     -0.00360      0.00129      0.00001      0.00383      2.79661      1.06490
109     0.42188       0.42188     -0.00307      0.00029      0.00001      0.00309      3.04843      1.15004
110     0.42578       0.42578     -0.00221     -0.00061      0.00001      0.00229     -2.87265     -1.07378
111     0.42969       0.42969     -0.00101     -0.00140      0.00000      0.00173     -2.19460     -0.81287
112     0.43359       0.43359      0.00045     -0.00208      0.00000      0.00213     -1.35676     -0.49801
113     0.43750       0.43750      0.00217     -0.00265      0.00001      0.00343     -0.88546     -0.32211
114     0.44141       0.44141      0.00406     -0.00310      0.00003      0.00511     -0.65225     -0.23518
115     0.44531       0.44531      0.00613     -0.00346      0.00005      0.00704     -0.51376     -0.18362
116     0.44922       0.44922      0.00828     -0.00369      0.00008      0.00906     -0.41906     -0.14847
117     0.45313       0.45313      0.01051     -0.00383      0.00013      0.01119     -0.34951     -0.12276
118     0.45703       0.45703      0.01274     -0.00385      0.00018      0.01331     -0.29367     -0.10227
119     0.46094       0.46094      0.01496     -0.00380      0.00024      0.01543     -0.24888     -0.08593
120     0.46484       0.46484      0.01709     -0.00363      0.00031      0.01747     -0.20954     -0.07174
121     0.46875       0.46875      0.01913     -0.00342      0.00038      0.01943     -0.17667     -0.05998
122     0.47266       0.47266      0.02101     -0.00309      0.00045      0.02123     -0.14583     -0.04911
123     0.47656       0.47656      0.02272     -0.00273      0.00052      0.02289     -0.11965     -0.03996
124     0.48047       0.48047      0.02421     -0.00227      0.00059      0.02431     -0.09346     -0.03096
125     0.48438       0.48438      0.02548     -0.00182      0.00065      0.02554     -0.07138     -0.02345
126     0.48828       0.48828      0.02647     -0.00126      0.00070      0.02650     -0.04739     -0.01545
127     0.49219       0.49219      0.02720     -0.00077      0.00074      0.02721     -0.02838     -0.00918
128     0.49609       0.49609      0.02763     -0.00010      0.00076      0.02763     -0.00348     -0.00112
129     0.50000       0.50000      0.02778      0.00000      0.00077      0.02778      0.00000      0.00000
NUMBER POINTS OUTPUT =         129
**** This image is ALIASED ****
INTEGRATED MTF AMPLITUDE FROM .25 TO .48 =       3.49077
if (mode = "nobatch" or mode = "inter")
end-if
otf1 inp=u2edgeN.img plotprof=u2edgeN plotout=test18
Beginning VICAR task otf1
OTF1 version 2015-08-10
NUMBER OF BAD PIXELS ON RIGHT=          1
LINE NUMBER=          7
NUMBER OF LINES PROCESSED=         20

 PIXEL  CYCLES/      FREQUENCY    REAL(FT)     IMAG.(FT)     INTENSITY    AMPLITUDE     PHASE     WAVE LENGT
         PIXEL                                                                         RADIANS      SHIFT
  1     0.00000       0.00000      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
  2     0.00391       0.00391      0.99863      0.00000      0.99726      0.99863      0.00000      0.00000
  3     0.00781       0.00781      0.99455      0.00000      0.98913      0.99455      0.00000      0.00000
  4     0.01172       0.01172      0.98777      0.00001      0.97568      0.98777      0.00000      0.00000
  5     0.01563       0.01563      0.97834      0.00003      0.95715      0.97834      0.00000      0.00000
  6     0.01953       0.01953      0.96632      0.00006      0.93377      0.96632      0.00000      0.00000
  7     0.02344       0.02344      0.95179      0.00010      0.90590      0.95179      0.00000      0.00000
  8     0.02734       0.02734      0.93483      0.00016      0.87391      0.93483      0.00000      0.00000
  9     0.03125       0.03125      0.91557      0.00023      0.83828      0.91557      0.00000      0.00000
 10     0.03516       0.03516      0.89412      0.00032      0.79945      0.89412      0.00000      0.00000
 11     0.03906       0.03906      0.87062      0.00043      0.75798      0.87062      0.00000      0.00000
 12     0.04297       0.04297      0.84521      0.00056      0.71439      0.84521      0.00000      0.00000
 13     0.04688       0.04688      0.81806      0.00070      0.66922      0.81806      0.00000      0.00000
 14     0.05078       0.05078      0.78933      0.00087      0.62304      0.78933      0.00111      0.00347
 15     0.05469       0.05469      0.75919      0.00105      0.57637      0.75919      0.00139      0.00404
 16     0.05859       0.05859      0.72783      0.00126      0.52973      0.72783      0.00173      0.00470
 17     0.06250       0.06250      0.69543      0.00147      0.48363      0.69543      0.00212      0.00539
 18     0.06641       0.06641      0.66219      0.00171      0.43850      0.66219      0.00258      0.00618
 19     0.07031       0.07031      0.62830      0.00195      0.39476      0.62830      0.00310      0.00701
 20     0.07422       0.07422      0.59395      0.00220      0.35278      0.59396      0.00371      0.00795
 21     0.07813       0.07813      0.55933      0.00246      0.31286      0.55934      0.00439      0.00895
 22     0.08203       0.08203      0.52464      0.00272      0.27526      0.52465      0.00519      0.01007
 23     0.08594       0.08594      0.49005      0.00299      0.24016      0.49006      0.00609      0.01128
 24     0.08984       0.08984      0.45576      0.00325      0.20773      0.45578      0.00714      0.01265
 25     0.09375       0.09375      0.42193      0.00351      0.17804      0.42194      0.00833      0.01414
 26     0.09766       0.09766      0.38873      0.00377      0.15113      0.38875      0.00971      0.01582
 27     0.10156       0.10156      0.35631      0.00402      0.12697      0.35633      0.01129      0.01770
 28     0.10547       0.10547      0.32484      0.00427      0.10554      0.32486      0.01314      0.01984
 29     0.10938       0.10938      0.29442      0.00450      0.08670      0.29446      0.01530      0.02226
 30     0.11328       0.11328      0.26521      0.00473      0.07036      0.26526      0.01784      0.02506
 31     0.11719       0.11719      0.23730      0.00495      0.05634      0.23735      0.02085      0.02831
 32     0.12109       0.12109      0.21081      0.00516      0.04447      0.21087      0.02446      0.03215
 33     0.12500       0.12500      0.18580      0.00536      0.03455      0.18587      0.02883      0.03671
 34     0.12891       0.12891      0.16236      0.00555      0.02639      0.16246      0.03419      0.04221
 35     0.13281       0.13281      0.14054      0.00575      0.01979      0.14066      0.04087      0.04898
 36     0.13672       0.13672      0.12040      0.00594      0.01453      0.12055      0.04926      0.05735
 37     0.14063       0.14063      0.10195      0.00613      0.01043      0.10214      0.06009      0.06801
 38     0.14453       0.14453      0.08523      0.00633      0.00730      0.08547      0.07414      0.08164
 39     0.14844       0.14844      0.07022      0.00655      0.00497      0.07053      0.09297      0.09968
 40     0.15234       0.15234      0.05693      0.00677      0.00329      0.05733      0.11835      0.12364
 41     0.15625       0.15625      0.04533      0.00702      0.00210      0.04587      0.15374      0.15660
 42     0.16016       0.16016      0.03539      0.00729      0.00131      0.03613      0.20321      0.20194
 43     0.16406       0.16406      0.02705      0.00760      0.00079      0.02810      0.27405      0.26585
 44     0.16797       0.16797      0.02028      0.00794      0.00047      0.02178      0.37313      0.35355
 45     0.17188       0.17188      0.01500      0.00833      0.00029      0.01716      0.50706      0.46953
 46     0.17578       0.17578      0.01115      0.00876      0.00020      0.01418      0.66591      0.60293
 47     0.17969       0.17969      0.00863      0.00925      0.00016      0.01265      0.82016      0.72644
 48     0.18359       0.18359      0.00737      0.00979      0.00015      0.01225      0.92525      0.80209
 49     0.18750       0.18750      0.00727      0.01040      0.00016      0.01269      0.96067      0.81544
 50     0.19141       0.19141      0.00824      0.01106      0.00019      0.01379      0.93078      0.77395
 51     0.19531       0.19531      0.01017      0.01181      0.00024      0.01558      0.86010      0.70088
 52     0.19922       0.19922      0.01295      0.01261      0.00033      0.01808      0.77204      0.61678
 53     0.20313       0.20313      0.01650      0.01350      0.00045      0.02132      0.68587      0.53740
 54     0.20703       0.20703      0.02068      0.01445      0.00064      0.02523      0.60964      0.46866
 55     0.21094       0.21094      0.02542      0.01548      0.00089      0.02977      0.54696      0.41269
 56     0.21484       0.21484      0.03059      0.01657      0.00121      0.03479      0.49636      0.36770
 57     0.21875       0.21875      0.03611      0.01774      0.00162      0.04023      0.45670      0.33228
 58     0.22266       0.22266      0.04185      0.01896      0.00211      0.04595      0.42541      0.30408
 59     0.22656       0.22656      0.04776      0.02026      0.00269      0.05188      0.40124      0.28186
 60     0.23047       0.23047      0.05369      0.02160      0.00335      0.05787      0.38241      0.26408
 61     0.23438       0.23438      0.05962      0.02299      0.00408      0.06390      0.36812      0.24997
 62     0.23828       0.23828      0.06541      0.02442      0.00487      0.06982      0.35731      0.23866
 63     0.24219       0.24219      0.07103      0.02589      0.00572      0.07560      0.34948      0.22966
 64     0.24609       0.24609      0.07638      0.02737      0.00658      0.08113      0.34405      0.22250
 65     0.25000       0.25000      0.08144      0.02887      0.00747      0.08640      0.34065      0.21687
 66     0.25391       0.25391      0.08609      0.03036      0.00833      0.09129      0.33903      0.21251
 67     0.25781       0.25781      0.09037      0.03185      0.00918      0.09581      0.33886      0.20919
 68     0.26172       0.26172      0.09415      0.03331      0.00997      0.09987      0.34009      0.20681
 69     0.26563       0.26563      0.09748      0.03474      0.01071      0.10348      0.34240      0.20516
 70     0.26953       0.26953      0.10026      0.03613      0.01136      0.10657      0.34589      0.20425
 71     0.27344       0.27344      0.10254      0.03746      0.01192      0.10917      0.35021      0.20384
 72     0.27734       0.27734      0.10425      0.03872      0.01237      0.11121      0.35560      0.20406
 73     0.28125       0.28125      0.10544      0.03989      0.01271      0.11273      0.36164      0.20465
 74     0.28516       0.28516      0.10605      0.04097      0.01293      0.11370      0.36869      0.20578
 75     0.28906       0.28906      0.10616      0.04194      0.01303      0.11415      0.37627      0.20717
 76     0.29297       0.29297      0.10572      0.04282      0.01301      0.11406      0.38484      0.20906
 77     0.29688       0.29688      0.10480      0.04355      0.01288      0.11349      0.39385      0.21114
 78     0.30078       0.30078      0.10337      0.04417      0.01264      0.11241      0.40386      0.21370
 79     0.30469       0.30469      0.10151      0.04463      0.01230      0.11089      0.41424      0.21638
 80     0.30859       0.30859      0.09920      0.04498      0.01186      0.10892      0.42566      0.21953
 81     0.31250       0.31250      0.09653      0.04514      0.01136      0.10656      0.43739      0.22276
 82     0.31641       0.31641      0.09349      0.04518      0.01078      0.10384      0.45020      0.22645
 83     0.32031       0.32031      0.09016      0.04504      0.01016      0.10078      0.46327      0.23019
 84     0.32422       0.32422      0.08654      0.04477      0.00949      0.09744      0.47746      0.23438
 85     0.32813       0.32813      0.08271      0.04431      0.00880      0.09383      0.49187      0.23858
 86     0.33203       0.33203      0.07868      0.04375      0.00810      0.09002      0.50745      0.24324
 87     0.33594       0.33594      0.07452      0.04298      0.00740      0.08603      0.52318      0.24786
 88     0.33984       0.33984      0.07026      0.04212      0.00671      0.08192      0.54008      0.25293
 89     0.34375       0.34375      0.06594      0.04107      0.00603      0.07769      0.55707      0.25792
 90     0.34766       0.34766      0.06161      0.03994      0.00539      0.07343      0.57519      0.26332
 91     0.35156       0.35156      0.05730      0.03864      0.00478      0.06911      0.59329      0.26859
 92     0.35547       0.35547      0.05306      0.03727      0.00420      0.06484      0.61239      0.27419
 93     0.35938       0.35938      0.04889      0.03575      0.00367      0.06057      0.63129      0.27958
 94     0.36328       0.36328      0.04488      0.03418      0.00318      0.05642      0.65091      0.28517
 95     0.36719       0.36719      0.04100      0.03249      0.00274      0.05231      0.67007      0.29044
 96     0.37109       0.37109      0.03733      0.03077      0.00234      0.04838      0.68939      0.29567
 97     0.37500       0.37500      0.03383      0.02895      0.00198      0.04453      0.70781      0.30040
 98     0.37891       0.37891      0.03061      0.02714      0.00167      0.04091      0.72547      0.30472
 99     0.38281       0.38281      0.02757      0.02525      0.00140      0.03739      0.74149      0.30827
100     0.38672       0.38672      0.02485      0.02339      0.00116      0.03413      0.75522      0.31081
101     0.39063       0.39063      0.02233      0.02148      0.00096      0.03099      0.76617      0.31217
102     0.39453       0.39453      0.02015      0.01963      0.00079      0.02813      0.77244      0.31160
103     0.39844       0.39844      0.01816      0.01776      0.00065      0.02541      0.77415      0.30923
104     0.40234       0.40234      0.01654      0.01596      0.00053      0.02298      0.76765      0.30366
105     0.40625       0.40625      0.01509      0.01418      0.00043      0.02071      0.75410      0.29543
106     0.41016       0.41016      0.01400      0.01247      0.00035      0.01875      0.72765      0.28235
107     0.41406       0.41406      0.01307      0.01082      0.00029      0.01696      0.69142      0.26576
108     0.41797       0.41797      0.01248      0.00925      0.00024      0.01553      0.63771      0.24283
109     0.42188       0.42188      0.01201      0.00776      0.00020      0.01430      0.57359      0.21639
110     0.42578       0.42578      0.01187      0.00635      0.00018      0.01346      0.49154      0.18374
111     0.42969       0.42969      0.01179      0.00506      0.00016      0.01283      0.40509      0.15004
112     0.43359       0.43359      0.01202      0.00384      0.00016      0.01262      0.30874      0.11333
113     0.43750       0.43750      0.01228      0.00275      0.00016      0.01258      0.22061      0.08025
114     0.44141       0.44141      0.01280      0.00172      0.00017      0.01291      0.13388      0.04827
115     0.44531       0.44531      0.01329      0.00086      0.00018      0.01331      0.06497      0.02322
116     0.44922       0.44922      0.01401      0.00003      0.00020      0.01401      0.00194      0.00069
117     0.45313       0.45313      0.01465     -0.00061      0.00021      0.01466     -0.04144     -0.01456
118     0.45703       0.45703      0.01549     -0.00126      0.00024      0.01554     -0.08143     -0.02836
119     0.46094       0.46094      0.01619     -0.00168      0.00027      0.01628     -0.10342     -0.03571
120     0.46484       0.46484      0.01706     -0.00218      0.00030      0.01720     -0.12704     -0.04350
121     0.46875       0.46875      0.01775     -0.00239      0.00032      0.01791     -0.13377     -0.04542
122     0.47266       0.47266      0.01858     -0.00277      0.00035      0.01878     -0.14780     -0.04977
123     0.47656       0.47656      0.01918     -0.00278      0.00038      0.01938     -0.14385     -0.04804
124     0.48047       0.48047      0.01990     -0.00309      0.00041      0.02013     -0.15400     -0.05101
125     0.48438       0.48438      0.02036     -0.00290      0.00042      0.02057     -0.14140     -0.04646
126     0.48828       0.48828      0.02092     -0.00324      0.00045      0.02117     -0.15383     -0.05014
127     0.49219       0.49219      0.02120     -0.00275      0.00046      0.02138     -0.12917     -0.04177
128     0.49609       0.49609      0.02157     -0.00352      0.00048      0.02186     -0.16163     -0.05185
129     0.50000       0.50000      0.02163      0.00000      0.00047      0.02163      0.00000      0.00000
NUMBER POINTS OUTPUT =         129
**** This image is ALIASED ****
INTEGRATED MTF AMPLITUDE FROM .25 TO .48 =       3.54962
if (mode = "nobatch" or mode = "inter")
end-if
otf1 inp=s1edge.img out=test19.img table=test19.tbl  plotout=test19
Beginning VICAR task otf1
OTF1 version 2015-08-10
NUMBER OF LINES PROCESSED=         20

 PIXEL  CYCLES/      FREQUENCY    REAL(FT)     IMAG.(FT)     INTENSITY    AMPLITUDE     PHASE     WAVE LENGT
         PIXEL                                                                         RADIANS      SHIFT
  1     0.00000       0.00000      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
  2     0.00391       0.00391      0.99799     -0.00000      0.99599      0.99799      0.00000      0.00000
  3     0.00781       0.00781      0.99199      0.00000      0.98404      0.99199      0.00000      0.00000
  4     0.01172       0.01172      0.98202      0.00000      0.96437      0.98202      0.00000      0.00000
  5     0.01563       0.01563      0.96818      0.00000      0.93736      0.96818      0.00000      0.00000
  6     0.01953       0.01953      0.95054      0.00000      0.90352      0.95054      0.00000      0.00000
  7     0.02344       0.02344      0.92924     -0.00000      0.86348      0.92924      0.00000      0.00000
  8     0.02734       0.02734      0.90443      0.00000      0.81799      0.90443      0.00000      0.00000
  9     0.03125       0.03125      0.87628      0.00000      0.76786      0.87628      0.00000      0.00000
 10     0.03516       0.03516      0.84498     -0.00000      0.71400      0.84498      0.00000      0.00000
 11     0.03906       0.03906      0.81077      0.00000      0.65735      0.81077      0.00000      0.00000
 12     0.04297       0.04297      0.77388      0.00000      0.59888      0.77388      0.00000      0.00000
 13     0.04688       0.04688      0.73455      0.00000      0.53957      0.73455      0.00000      0.00000
 14     0.05078       0.05078      0.69307     -0.00000      0.48035      0.69307      0.00000      0.00000
 15     0.05469       0.05469      0.64972      0.00000      0.42214      0.64972      0.00000      0.00000
 16     0.05859       0.05859      0.60479     -0.00000      0.36578      0.60479      0.00000      0.00000
 17     0.06250       0.06250      0.55859      0.00000      0.31203      0.55859      0.00000      0.00000
 18     0.06641       0.06641      0.51143     -0.00000      0.26156      0.51143      0.00000      0.00000
 19     0.07031       0.07031      0.46362     -0.00000      0.21494      0.46362      0.00000      0.00000
 20     0.07422       0.07422      0.41547      0.00000      0.17261      0.41547      0.00000      0.00000
 21     0.07813       0.07813      0.36729      0.00000      0.13491      0.36729      0.00000      0.00000
 22     0.08203       0.08203      0.31941     -0.00000      0.10202      0.31941      0.00000      0.00000
 23     0.08594       0.08594      0.27211     -0.00000      0.07404      0.27211      0.00000      0.00000
 24     0.08984       0.08984      0.22569     -0.00000      0.05094      0.22569      0.00000      0.00000
 25     0.09375       0.09375      0.18043      0.00000      0.03256      0.18043      0.00000      0.00000
 26     0.09766       0.09766      0.13661     -0.00000      0.01866      0.13661      0.00000      0.00000
 27     0.10156       0.10156      0.09447      0.00000      0.00893      0.09447      0.00000      0.00000
 28     0.10547       0.10547      0.05426     -0.00000      0.00294      0.05426      0.00000      0.00000
 29     0.10938       0.10938      0.01618      0.00000      0.00026      0.01618      0.00000      0.00000
 30     0.11328       0.11328     -0.01956     -0.00000      0.00038      0.01956      3.14159      4.41379
 31     0.11719       0.11719     -0.05278      0.00000      0.00279      0.05278      3.14159      4.26667
 32     0.12109       0.12109     -0.08334     -0.00000      0.00695      0.08334      3.14159      4.12903
 33     0.12500       0.12500     -0.11111      0.00000      0.01235      0.11111      3.14159      4.00000
 34     0.12891       0.12891     -0.13598     -0.00000      0.01849      0.13598      3.14159      3.87879
 35     0.13281       0.13281     -0.15788     -0.00000      0.02493      0.15788      3.14159      3.76471
 36     0.13672       0.13672     -0.17674     -0.00000      0.03124      0.17674      3.14159      3.65714
 37     0.14063       0.14063     -0.19256      0.00000      0.03708      0.19256      3.14159      3.55556
 38     0.14453       0.14453     -0.20531     -0.00000      0.04215      0.20531      3.14159      3.45946
 39     0.14844       0.14844     -0.21502      0.00000      0.04623      0.21502      3.14159      3.36842
 40     0.15234       0.15234     -0.22175     -0.00000      0.04917      0.22175      3.14159      3.28205
 41     0.15625       0.15625     -0.22556      0.00000      0.05088      0.22556      3.14159      3.20000
 42     0.16016       0.16016     -0.22654     -0.00000      0.05132      0.22654      3.14159      3.12195
 43     0.16406       0.16406     -0.22481      0.00000      0.05054      0.22481      3.14159      3.04762
 44     0.16797       0.16797     -0.22051     -0.00000      0.04863      0.22051      3.14159      2.97674
 45     0.17188       0.17188     -0.21379      0.00000      0.04570      0.21379      3.14159      2.90909
 46     0.17578       0.17578     -0.20481      0.00000      0.04195      0.20481      3.14159      2.84444
 47     0.17969       0.17969     -0.19377      0.00000      0.03755      0.19377      3.14159      2.78261
 48     0.18359       0.18359     -0.18086     -0.00000      0.03271      0.18086      3.14159      2.72340
 49     0.18750       0.18750     -0.16629      0.00000      0.02765      0.16629      3.14159      2.66667
 50     0.19141       0.19141     -0.15028     -0.00000      0.02258      0.15028      3.14159      2.61224
 51     0.19531       0.19531     -0.13306      0.00000      0.01770      0.13306      3.14159      2.56000
 52     0.19922       0.19922     -0.11485     -0.00000      0.01319      0.11485      3.14159      2.50980
 53     0.20313       0.20313     -0.09589      0.00000      0.00920      0.09589      3.14159      2.46154
 54     0.20703       0.20703     -0.07641      0.00000      0.00584      0.07641      3.14159      2.41509
 55     0.21094       0.21094     -0.05665      0.00000      0.00321      0.05665      3.14159      2.37037
 56     0.21484       0.21484     -0.03683      0.00000      0.00136      0.03683      3.14159      2.32727
 57     0.21875       0.21875     -0.01717      0.00000      0.00029      0.01717      3.14159      2.28571
 58     0.22266       0.22266      0.00212     -0.00000      0.00000      0.00212      0.00000      0.00000
 59     0.22656       0.22656      0.02082      0.00000      0.00043      0.02082      0.00000      0.00000
 60     0.23047       0.23047      0.03876     -0.00000      0.00150      0.03876      0.00000      0.00000
 61     0.23438       0.23438      0.05574      0.00000      0.00311      0.05574      0.00000      0.00000
 62     0.23828       0.23828      0.07161      0.00000      0.00513      0.07161      0.00000      0.00000
 63     0.24219       0.24219      0.08621      0.00000      0.00743      0.08621      0.00000      0.00000
 64     0.24609       0.24609      0.09941      0.00000      0.00988      0.09941      0.00000      0.00000
 65     0.25000       0.25000      0.11111      0.00000      0.01235      0.11111      0.00000      0.00000
 66     0.25391       0.25391      0.12120     -0.00000      0.01469      0.12120      0.00000      0.00000
 67     0.25781       0.25781      0.12961      0.00000      0.01680      0.12961      0.00000      0.00000
 68     0.26172       0.26172      0.13629      0.00000      0.01857      0.13629      0.00000      0.00000
 69     0.26563       0.26563      0.14119      0.00000      0.01994      0.14119      0.00000      0.00000
 70     0.26953       0.26953      0.14431      0.00000      0.02082      0.14431      0.00000      0.00000
 71     0.27344       0.27344      0.14563      0.00000      0.02121      0.14563      0.00000      0.00000
 72     0.27734       0.27734      0.14520      0.00000      0.02108      0.14520      0.00000      0.00000
 73     0.28125       0.28125      0.14305      0.00000      0.02046      0.14305      0.00000      0.00000
 74     0.28516       0.28516      0.13923      0.00000      0.01938      0.13923      0.00000      0.00000
 75     0.28906       0.28906      0.13383     -0.00000      0.01791      0.13383      0.00000      0.00000
 76     0.29297       0.29297      0.12693      0.00000      0.01611      0.12693      0.00000      0.00000
 77     0.29688       0.29688      0.11865      0.00000      0.01408      0.11865      0.00000      0.00000
 78     0.30078       0.30078      0.10911     -0.00000      0.01190      0.10911      0.00000      0.00000
 79     0.30469       0.30469      0.09843     -0.00000      0.00969      0.09843      0.00000      0.00000
 80     0.30859       0.30859      0.08675     -0.00000      0.00753      0.08675      0.00000      0.00000
 81     0.31250       0.31250      0.07424      0.00000      0.00551      0.07424      0.00000      0.00000
 82     0.31641       0.31641      0.06105     -0.00000      0.00373      0.06105      0.00000      0.00000
 83     0.32031       0.32031      0.04733     -0.00000      0.00224      0.04733      0.00000      0.00000
 84     0.32422       0.32422      0.03326     -0.00000      0.00111      0.03326      0.00000      0.00000
 85     0.32813       0.32813      0.01901      0.00000      0.00036      0.01901      0.00000      0.00000
 86     0.33203       0.33203      0.00473     -0.00000      0.00002      0.00473      0.00000      0.00000
 87     0.33594       0.33594     -0.00939     -0.00000      0.00009      0.00939      3.14159      1.48837
 88     0.33984       0.33984     -0.02321     -0.00000      0.00054      0.02321      3.14159      1.47126
 89     0.34375       0.34375     -0.03657      0.00000      0.00134      0.03657      3.14159      1.45455
 90     0.34766       0.34766     -0.04932     -0.00000      0.00243      0.04932      3.14159      1.43820
 91     0.35156       0.35156     -0.06131     -0.00000      0.00376      0.06131      3.14159      1.42222
 92     0.35547       0.35547     -0.07243     -0.00000      0.00525      0.07243      3.14159      1.40659
 93     0.35938       0.35938     -0.08254      0.00000      0.00681      0.08254      3.14159      1.39130
 94     0.36328       0.36328     -0.09155     -0.00000      0.00838      0.09155      3.14159      1.37634
 95     0.36719       0.36719     -0.09937     -0.00000      0.00987      0.09937      3.14159      1.36170
 96     0.37109       0.37109     -0.10591     -0.00000      0.01122      0.10591      3.14159      1.34737
 97     0.37500       0.37500     -0.11111      0.00000      0.01235      0.11111      3.14159      1.33333
 98     0.37891       0.37891     -0.11493      0.00000      0.01321      0.11493      3.14159      1.31959
 99     0.38281       0.38281     -0.11734     -0.00000      0.01377      0.11734      3.14159      1.30612
100     0.38672       0.38672     -0.11832     -0.00000      0.01400      0.11832      3.14159      1.29293
101     0.39063       0.39063     -0.11787      0.00000      0.01389      0.11787      3.14159      1.28000
102     0.39453       0.39453     -0.11601     -0.00000      0.01346      0.11601      3.14159      1.26733
103     0.39844       0.39844     -0.11278     -0.00000      0.01272      0.11278      3.14159      1.25490
104     0.40234       0.40234     -0.10822     -0.00000      0.01171      0.10822      3.14159      1.24272
105     0.40625       0.40625     -0.10240      0.00000      0.01049      0.10240      3.14159      1.23077
106     0.41016       0.41016     -0.09540      0.00000      0.00910      0.09540      3.14159      1.21905
107     0.41406       0.41406     -0.08730      0.00000      0.00762      0.08730      3.14159      1.20755
108     0.41797       0.41797     -0.07820     -0.00000      0.00612      0.07820      3.14159      1.19626
109     0.42188       0.42188     -0.06823      0.00000      0.00466      0.06823      3.14159      1.18519
110     0.42578       0.42578     -0.05750      0.00000      0.00331      0.05750      3.14159      1.17431
111     0.42969       0.42969     -0.04615     -0.00000      0.00213      0.04615      3.14159      1.16364
112     0.43359       0.43359     -0.03430     -0.00000      0.00118      0.03430      3.14159      1.15315
113     0.43750       0.43750     -0.02210      0.00000      0.00049      0.02210      3.14159      1.14286
114     0.44141       0.44141     -0.00970      0.00000      0.00009      0.00970      3.14159      1.13274
115     0.44531       0.44531      0.00277      0.00000      0.00001      0.00277      0.00000      0.00000
116     0.44922       0.44922      0.01515     -0.00000      0.00023      0.01515      0.00000      0.00000
117     0.45313       0.45313      0.02729      0.00000      0.00074      0.02729      0.00000      0.00000
118     0.45703       0.45703      0.03907     -0.00000      0.00153      0.03907      0.00000      0.00000
119     0.46094       0.46094      0.05034      0.00000      0.00253      0.05034      0.00000      0.00000
120     0.46484       0.46484      0.06096      0.00000      0.00372      0.06096      0.00000      0.00000
121     0.46875       0.46875      0.07083      0.00000      0.00502      0.07083      0.00000      0.00000
122     0.47266       0.47266      0.07982      0.00000      0.00637      0.07982      0.00000      0.00000
123     0.47656       0.47656      0.08783     -0.00000      0.00771      0.08783      0.00000      0.00000
124     0.48047       0.48047      0.09477      0.00000      0.00898      0.09477      0.00000      0.00000
125     0.48438       0.48438      0.10056      0.00000      0.01011      0.10056      0.00000      0.00000
126     0.48828       0.48828      0.10514      0.00000      0.01105      0.10514      0.00000      0.00000
127     0.49219       0.49219      0.10844     -0.00000      0.01176      0.10844      0.00000      0.00000
128     0.49609       0.49609      0.11044      0.00000      0.01220      0.11044      0.00000      0.00000
129     0.50000       0.50000      0.11111      0.00000      0.01235      0.11111      0.00000      0.00000
NUMBER POINTS OUTPUT =         129
**** This image is ALIASED ****
INTEGRATED MTF AMPLITUDE FROM .25 TO .48 =       4.94845
typetext test19.tbl
Beginning VICAR task typetext
 # FREQUENCY	   AMPLITUDE	   PHASE
    0.00000	   1.00000	   0.00000
    0.00391	   0.99799	   0.00000
    0.00781	   0.99199	   0.00000
    0.01172	   0.98202	   0.00000
    0.01563	   0.96818	   0.00000
    0.01953	   0.95054	   0.00000
    0.02344	   0.92924	   0.00000
    0.02734	   0.90443	   0.00000
    0.03125	   0.87628	   0.00000
    0.03516	   0.84498	   0.00000
    0.03906	   0.81077	   0.00000
    0.04297	   0.77388	   0.00000
    0.04688	   0.73455	   0.00000
    0.05078	   0.69307	   0.00000
    0.05469	   0.64972	   0.00000
    0.05859	   0.60479	   0.00000
    0.06250	   0.55859	   0.00000
    0.06641	   0.51143	   0.00000
    0.07031	   0.46362	   0.00000
    0.07422	   0.41547	   0.00000
    0.07813	   0.36729	   0.00000
    0.08203	   0.31941	   0.00000
    0.08594	   0.27211	   0.00000
    0.08984	   0.22569	   0.00000
    0.09375	   0.18043	   0.00000
    0.09766	   0.13661	   0.00000
    0.10156	   0.09447	   0.00000
    0.10547	   0.05426	   0.00000
    0.10938	   0.01618	   0.00000
    0.11328	   0.01956	   3.14159
    0.11719	   0.05278	   3.14159
    0.12109	   0.08334	   3.14159
    0.12500	   0.11111	   3.14159
    0.12891	   0.13598	   3.14159
    0.13281	   0.15788	   3.14159
    0.13672	   0.17674	   3.14159
    0.14063	   0.19256	   3.14159
    0.14453	   0.20531	   3.14159
    0.14844	   0.21502	   3.14159
    0.15234	   0.22175	   3.14159
    0.15625	   0.22556	   3.14159
    0.16016	   0.22654	   3.14159
    0.16406	   0.22481	   3.14159
    0.16797	   0.22051	   3.14159
    0.17188	   0.21379	   3.14159
    0.17578	   0.20481	   3.14159
    0.17969	   0.19377	   3.14159
    0.18359	   0.18086	   3.14159
    0.18750	   0.16629	   3.14159
    0.19141	   0.15028	   3.14159
    0.19531	   0.13306	   3.14159
    0.19922	   0.11485	   3.14159
    0.20313	   0.09589	   3.14159
    0.20703	   0.07641	   3.14159
    0.21094	   0.05665	   3.14159
    0.21484	   0.03683	   3.14159
    0.21875	   0.01717	   3.14159
    0.22266	   0.00212	   0.00000
    0.22656	   0.02082	   0.00000
    0.23047	   0.03876	   0.00000
    0.23438	   0.05574	   0.00000
    0.23828	   0.07161	   0.00000
    0.24219	   0.08621	   0.00000
    0.24609	   0.09941	   0.00000
    0.25000	   0.11111	   0.00000
    0.25391	   0.12120	   0.00000
    0.25781	   0.12961	   0.00000
    0.26172	   0.13629	   0.00000
    0.26563	   0.14119	   0.00000
    0.26953	   0.14431	   0.00000
    0.27344	   0.14563	   0.00000
    0.27734	   0.14520	   0.00000
    0.28125	   0.14305	   0.00000
    0.28516	   0.13923	   0.00000
    0.28906	   0.13383	   0.00000
    0.29297	   0.12693	   0.00000
    0.29688	   0.11865	   0.00000
    0.30078	   0.10911	   0.00000
    0.30469	   0.09843	   0.00000
    0.30859	   0.08675	   0.00000
    0.31250	   0.07424	   0.00000
    0.31641	   0.06105	   0.00000
    0.32031	   0.04733	   0.00000
    0.32422	   0.03326	   0.00000
    0.32813	   0.01901	   0.00000
    0.33203	   0.00473	   0.00000
    0.33594	   0.00939	   3.14159
    0.33984	   0.02321	   3.14159
    0.34375	   0.03657	   3.14159
    0.34766	   0.04932	   3.14159
    0.35156	   0.06131	   3.14159
    0.35547	   0.07243	   3.14159
    0.35938	   0.08254	   3.14159
    0.36328	   0.09155	   3.14159
    0.36719	   0.09937	   3.14159
    0.37109	   0.10591	   3.14159
    0.37500	   0.11111	   3.14159
    0.37891	   0.11493	   3.14159
    0.38281	   0.11734	   3.14159
    0.38672	   0.11832	   3.14159
    0.39063	   0.11787	   3.14159
    0.39453	   0.11601	   3.14159
    0.39844	   0.11278	   3.14159
    0.40234	   0.10822	   3.14159
    0.40625	   0.10240	   3.14159
    0.41016	   0.09540	   3.14159
    0.41406	   0.08730	   3.14159
    0.41797	   0.07820	   3.14159
    0.42188	   0.06823	   3.14159
    0.42578	   0.05750	   3.14159
    0.42969	   0.04615	   3.14159
    0.43359	   0.03430	   3.14159
    0.43750	   0.02210	   3.14159
    0.44141	   0.00970	   3.14159
    0.44531	   0.00277	   0.00000
    0.44922	   0.01515	   0.00000
    0.45313	   0.02729	   0.00000
    0.45703	   0.03907	   0.00000
    0.46094	   0.05034	   0.00000
    0.46484	   0.06096	   0.00000
    0.46875	   0.07083	   0.00000
    0.47266	   0.07982	   0.00000
    0.47656	   0.08783	   0.00000
    0.48047	   0.09477	   0.00000
    0.48438	   0.10056	   0.00000
    0.48828	   0.10514	   0.00000
    0.49219	   0.10844	   0.00000
    0.49609	   0.11044	   0.00000
    0.50000	   0.11111	   0.00000
if (mode = "nobatch" or mode = "inter")
end-if
otf1 inp=s1edge.img out=test20.img table=test20.tbl  plotout=test20  +
    columns=nocolhdr
Beginning VICAR task otf1
OTF1 version 2015-08-10
NUMBER OF LINES PROCESSED=         20

 PIXEL  CYCLES/      FREQUENCY    REAL(FT)     IMAG.(FT)     INTENSITY    AMPLITUDE     PHASE     WAVE LENGT
         PIXEL                                                                         RADIANS      SHIFT
  1     0.00000       0.00000      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
  2     0.00391       0.00391      0.99799     -0.00000      0.99599      0.99799      0.00000      0.00000
  3     0.00781       0.00781      0.99199      0.00000      0.98404      0.99199      0.00000      0.00000
  4     0.01172       0.01172      0.98202      0.00000      0.96437      0.98202      0.00000      0.00000
  5     0.01563       0.01563      0.96818      0.00000      0.93736      0.96818      0.00000      0.00000
  6     0.01953       0.01953      0.95054      0.00000      0.90352      0.95054      0.00000      0.00000
  7     0.02344       0.02344      0.92924     -0.00000      0.86348      0.92924      0.00000      0.00000
  8     0.02734       0.02734      0.90443      0.00000      0.81799      0.90443      0.00000      0.00000
  9     0.03125       0.03125      0.87628      0.00000      0.76786      0.87628      0.00000      0.00000
 10     0.03516       0.03516      0.84498     -0.00000      0.71400      0.84498      0.00000      0.00000
 11     0.03906       0.03906      0.81077      0.00000      0.65735      0.81077      0.00000      0.00000
 12     0.04297       0.04297      0.77388      0.00000      0.59888      0.77388      0.00000      0.00000
 13     0.04688       0.04688      0.73455      0.00000      0.53957      0.73455      0.00000      0.00000
 14     0.05078       0.05078      0.69307     -0.00000      0.48035      0.69307      0.00000      0.00000
 15     0.05469       0.05469      0.64972      0.00000      0.42214      0.64972      0.00000      0.00000
 16     0.05859       0.05859      0.60479     -0.00000      0.36578      0.60479      0.00000      0.00000
 17     0.06250       0.06250      0.55859      0.00000      0.31203      0.55859      0.00000      0.00000
 18     0.06641       0.06641      0.51143     -0.00000      0.26156      0.51143      0.00000      0.00000
 19     0.07031       0.07031      0.46362     -0.00000      0.21494      0.46362      0.00000      0.00000
 20     0.07422       0.07422      0.41547      0.00000      0.17261      0.41547      0.00000      0.00000
 21     0.07813       0.07813      0.36729      0.00000      0.13491      0.36729      0.00000      0.00000
 22     0.08203       0.08203      0.31941     -0.00000      0.10202      0.31941      0.00000      0.00000
 23     0.08594       0.08594      0.27211     -0.00000      0.07404      0.27211      0.00000      0.00000
 24     0.08984       0.08984      0.22569     -0.00000      0.05094      0.22569      0.00000      0.00000
 25     0.09375       0.09375      0.18043      0.00000      0.03256      0.18043      0.00000      0.00000
 26     0.09766       0.09766      0.13661     -0.00000      0.01866      0.13661      0.00000      0.00000
 27     0.10156       0.10156      0.09447      0.00000      0.00893      0.09447      0.00000      0.00000
 28     0.10547       0.10547      0.05426     -0.00000      0.00294      0.05426      0.00000      0.00000
 29     0.10938       0.10938      0.01618      0.00000      0.00026      0.01618      0.00000      0.00000
 30     0.11328       0.11328     -0.01956     -0.00000      0.00038      0.01956      3.14159      4.41379
 31     0.11719       0.11719     -0.05278      0.00000      0.00279      0.05278      3.14159      4.26667
 32     0.12109       0.12109     -0.08334     -0.00000      0.00695      0.08334      3.14159      4.12903
 33     0.12500       0.12500     -0.11111      0.00000      0.01235      0.11111      3.14159      4.00000
 34     0.12891       0.12891     -0.13598     -0.00000      0.01849      0.13598      3.14159      3.87879
 35     0.13281       0.13281     -0.15788     -0.00000      0.02493      0.15788      3.14159      3.76471
 36     0.13672       0.13672     -0.17674     -0.00000      0.03124      0.17674      3.14159      3.65714
 37     0.14063       0.14063     -0.19256      0.00000      0.03708      0.19256      3.14159      3.55556
 38     0.14453       0.14453     -0.20531     -0.00000      0.04215      0.20531      3.14159      3.45946
 39     0.14844       0.14844     -0.21502      0.00000      0.04623      0.21502      3.14159      3.36842
 40     0.15234       0.15234     -0.22175     -0.00000      0.04917      0.22175      3.14159      3.28205
 41     0.15625       0.15625     -0.22556      0.00000      0.05088      0.22556      3.14159      3.20000
 42     0.16016       0.16016     -0.22654     -0.00000      0.05132      0.22654      3.14159      3.12195
 43     0.16406       0.16406     -0.22481      0.00000      0.05054      0.22481      3.14159      3.04762
 44     0.16797       0.16797     -0.22051     -0.00000      0.04863      0.22051      3.14159      2.97674
 45     0.17188       0.17188     -0.21379      0.00000      0.04570      0.21379      3.14159      2.90909
 46     0.17578       0.17578     -0.20481      0.00000      0.04195      0.20481      3.14159      2.84444
 47     0.17969       0.17969     -0.19377      0.00000      0.03755      0.19377      3.14159      2.78261
 48     0.18359       0.18359     -0.18086     -0.00000      0.03271      0.18086      3.14159      2.72340
 49     0.18750       0.18750     -0.16629      0.00000      0.02765      0.16629      3.14159      2.66667
 50     0.19141       0.19141     -0.15028     -0.00000      0.02258      0.15028      3.14159      2.61224
 51     0.19531       0.19531     -0.13306      0.00000      0.01770      0.13306      3.14159      2.56000
 52     0.19922       0.19922     -0.11485     -0.00000      0.01319      0.11485      3.14159      2.50980
 53     0.20313       0.20313     -0.09589      0.00000      0.00920      0.09589      3.14159      2.46154
 54     0.20703       0.20703     -0.07641      0.00000      0.00584      0.07641      3.14159      2.41509
 55     0.21094       0.21094     -0.05665      0.00000      0.00321      0.05665      3.14159      2.37037
 56     0.21484       0.21484     -0.03683      0.00000      0.00136      0.03683      3.14159      2.32727
 57     0.21875       0.21875     -0.01717      0.00000      0.00029      0.01717      3.14159      2.28571
 58     0.22266       0.22266      0.00212     -0.00000      0.00000      0.00212      0.00000      0.00000
 59     0.22656       0.22656      0.02082      0.00000      0.00043      0.02082      0.00000      0.00000
 60     0.23047       0.23047      0.03876     -0.00000      0.00150      0.03876      0.00000      0.00000
 61     0.23438       0.23438      0.05574      0.00000      0.00311      0.05574      0.00000      0.00000
 62     0.23828       0.23828      0.07161      0.00000      0.00513      0.07161      0.00000      0.00000
 63     0.24219       0.24219      0.08621      0.00000      0.00743      0.08621      0.00000      0.00000
 64     0.24609       0.24609      0.09941      0.00000      0.00988      0.09941      0.00000      0.00000
 65     0.25000       0.25000      0.11111      0.00000      0.01235      0.11111      0.00000      0.00000
 66     0.25391       0.25391      0.12120     -0.00000      0.01469      0.12120      0.00000      0.00000
 67     0.25781       0.25781      0.12961      0.00000      0.01680      0.12961      0.00000      0.00000
 68     0.26172       0.26172      0.13629      0.00000      0.01857      0.13629      0.00000      0.00000
 69     0.26563       0.26563      0.14119      0.00000      0.01994      0.14119      0.00000      0.00000
 70     0.26953       0.26953      0.14431      0.00000      0.02082      0.14431      0.00000      0.00000
 71     0.27344       0.27344      0.14563      0.00000      0.02121      0.14563      0.00000      0.00000
 72     0.27734       0.27734      0.14520      0.00000      0.02108      0.14520      0.00000      0.00000
 73     0.28125       0.28125      0.14305      0.00000      0.02046      0.14305      0.00000      0.00000
 74     0.28516       0.28516      0.13923      0.00000      0.01938      0.13923      0.00000      0.00000
 75     0.28906       0.28906      0.13383     -0.00000      0.01791      0.13383      0.00000      0.00000
 76     0.29297       0.29297      0.12693      0.00000      0.01611      0.12693      0.00000      0.00000
 77     0.29688       0.29688      0.11865      0.00000      0.01408      0.11865      0.00000      0.00000
 78     0.30078       0.30078      0.10911     -0.00000      0.01190      0.10911      0.00000      0.00000
 79     0.30469       0.30469      0.09843     -0.00000      0.00969      0.09843      0.00000      0.00000
 80     0.30859       0.30859      0.08675     -0.00000      0.00753      0.08675      0.00000      0.00000
 81     0.31250       0.31250      0.07424      0.00000      0.00551      0.07424      0.00000      0.00000
 82     0.31641       0.31641      0.06105     -0.00000      0.00373      0.06105      0.00000      0.00000
 83     0.32031       0.32031      0.04733     -0.00000      0.00224      0.04733      0.00000      0.00000
 84     0.32422       0.32422      0.03326     -0.00000      0.00111      0.03326      0.00000      0.00000
 85     0.32813       0.32813      0.01901      0.00000      0.00036      0.01901      0.00000      0.00000
 86     0.33203       0.33203      0.00473     -0.00000      0.00002      0.00473      0.00000      0.00000
 87     0.33594       0.33594     -0.00939     -0.00000      0.00009      0.00939      3.14159      1.48837
 88     0.33984       0.33984     -0.02321     -0.00000      0.00054      0.02321      3.14159      1.47126
 89     0.34375       0.34375     -0.03657      0.00000      0.00134      0.03657      3.14159      1.45455
 90     0.34766       0.34766     -0.04932     -0.00000      0.00243      0.04932      3.14159      1.43820
 91     0.35156       0.35156     -0.06131     -0.00000      0.00376      0.06131      3.14159      1.42222
 92     0.35547       0.35547     -0.07243     -0.00000      0.00525      0.07243      3.14159      1.40659
 93     0.35938       0.35938     -0.08254      0.00000      0.00681      0.08254      3.14159      1.39130
 94     0.36328       0.36328     -0.09155     -0.00000      0.00838      0.09155      3.14159      1.37634
 95     0.36719       0.36719     -0.09937     -0.00000      0.00987      0.09937      3.14159      1.36170
 96     0.37109       0.37109     -0.10591     -0.00000      0.01122      0.10591      3.14159      1.34737
 97     0.37500       0.37500     -0.11111      0.00000      0.01235      0.11111      3.14159      1.33333
 98     0.37891       0.37891     -0.11493      0.00000      0.01321      0.11493      3.14159      1.31959
 99     0.38281       0.38281     -0.11734     -0.00000      0.01377      0.11734      3.14159      1.30612
100     0.38672       0.38672     -0.11832     -0.00000      0.01400      0.11832      3.14159      1.29293
101     0.39063       0.39063     -0.11787      0.00000      0.01389      0.11787      3.14159      1.28000
102     0.39453       0.39453     -0.11601     -0.00000      0.01346      0.11601      3.14159      1.26733
103     0.39844       0.39844     -0.11278     -0.00000      0.01272      0.11278      3.14159      1.25490
104     0.40234       0.40234     -0.10822     -0.00000      0.01171      0.10822      3.14159      1.24272
105     0.40625       0.40625     -0.10240      0.00000      0.01049      0.10240      3.14159      1.23077
106     0.41016       0.41016     -0.09540      0.00000      0.00910      0.09540      3.14159      1.21905
107     0.41406       0.41406     -0.08730      0.00000      0.00762      0.08730      3.14159      1.20755
108     0.41797       0.41797     -0.07820     -0.00000      0.00612      0.07820      3.14159      1.19626
109     0.42188       0.42188     -0.06823      0.00000      0.00466      0.06823      3.14159      1.18519
110     0.42578       0.42578     -0.05750      0.00000      0.00331      0.05750      3.14159      1.17431
111     0.42969       0.42969     -0.04615     -0.00000      0.00213      0.04615      3.14159      1.16364
112     0.43359       0.43359     -0.03430     -0.00000      0.00118      0.03430      3.14159      1.15315
113     0.43750       0.43750     -0.02210      0.00000      0.00049      0.02210      3.14159      1.14286
114     0.44141       0.44141     -0.00970      0.00000      0.00009      0.00970      3.14159      1.13274
115     0.44531       0.44531      0.00277      0.00000      0.00001      0.00277      0.00000      0.00000
116     0.44922       0.44922      0.01515     -0.00000      0.00023      0.01515      0.00000      0.00000
117     0.45313       0.45313      0.02729      0.00000      0.00074      0.02729      0.00000      0.00000
118     0.45703       0.45703      0.03907     -0.00000      0.00153      0.03907      0.00000      0.00000
119     0.46094       0.46094      0.05034      0.00000      0.00253      0.05034      0.00000      0.00000
120     0.46484       0.46484      0.06096      0.00000      0.00372      0.06096      0.00000      0.00000
121     0.46875       0.46875      0.07083      0.00000      0.00502      0.07083      0.00000      0.00000
122     0.47266       0.47266      0.07982      0.00000      0.00637      0.07982      0.00000      0.00000
123     0.47656       0.47656      0.08783     -0.00000      0.00771      0.08783      0.00000      0.00000
124     0.48047       0.48047      0.09477      0.00000      0.00898      0.09477      0.00000      0.00000
125     0.48438       0.48438      0.10056      0.00000      0.01011      0.10056      0.00000      0.00000
126     0.48828       0.48828      0.10514      0.00000      0.01105      0.10514      0.00000      0.00000
127     0.49219       0.49219      0.10844     -0.00000      0.01176      0.10844      0.00000      0.00000
128     0.49609       0.49609      0.11044      0.00000      0.01220      0.11044      0.00000      0.00000
129     0.50000       0.50000      0.11111      0.00000      0.01235      0.11111      0.00000      0.00000
NUMBER POINTS OUTPUT =         129
**** This image is ALIASED ****
INTEGRATED MTF AMPLITUDE FROM .25 TO .48 =       4.94845
typetext test20.tbl
Beginning VICAR task typetext
    0.00000	   1.00000	   0.00000
    0.00391	   0.99799	   0.00000
    0.00781	   0.99199	   0.00000
    0.01172	   0.98202	   0.00000
    0.01563	   0.96818	   0.00000
    0.01953	   0.95054	   0.00000
    0.02344	   0.92924	   0.00000
    0.02734	   0.90443	   0.00000
    0.03125	   0.87628	   0.00000
    0.03516	   0.84498	   0.00000
    0.03906	   0.81077	   0.00000
    0.04297	   0.77388	   0.00000
    0.04688	   0.73455	   0.00000
    0.05078	   0.69307	   0.00000
    0.05469	   0.64972	   0.00000
    0.05859	   0.60479	   0.00000
    0.06250	   0.55859	   0.00000
    0.06641	   0.51143	   0.00000
    0.07031	   0.46362	   0.00000
    0.07422	   0.41547	   0.00000
    0.07813	   0.36729	   0.00000
    0.08203	   0.31941	   0.00000
    0.08594	   0.27211	   0.00000
    0.08984	   0.22569	   0.00000
    0.09375	   0.18043	   0.00000
    0.09766	   0.13661	   0.00000
    0.10156	   0.09447	   0.00000
    0.10547	   0.05426	   0.00000
    0.10938	   0.01618	   0.00000
    0.11328	   0.01956	   3.14159
    0.11719	   0.05278	   3.14159
    0.12109	   0.08334	   3.14159
    0.12500	   0.11111	   3.14159
    0.12891	   0.13598	   3.14159
    0.13281	   0.15788	   3.14159
    0.13672	   0.17674	   3.14159
    0.14063	   0.19256	   3.14159
    0.14453	   0.20531	   3.14159
    0.14844	   0.21502	   3.14159
    0.15234	   0.22175	   3.14159
    0.15625	   0.22556	   3.14159
    0.16016	   0.22654	   3.14159
    0.16406	   0.22481	   3.14159
    0.16797	   0.22051	   3.14159
    0.17188	   0.21379	   3.14159
    0.17578	   0.20481	   3.14159
    0.17969	   0.19377	   3.14159
    0.18359	   0.18086	   3.14159
    0.18750	   0.16629	   3.14159
    0.19141	   0.15028	   3.14159
    0.19531	   0.13306	   3.14159
    0.19922	   0.11485	   3.14159
    0.20313	   0.09589	   3.14159
    0.20703	   0.07641	   3.14159
    0.21094	   0.05665	   3.14159
    0.21484	   0.03683	   3.14159
    0.21875	   0.01717	   3.14159
    0.22266	   0.00212	   0.00000
    0.22656	   0.02082	   0.00000
    0.23047	   0.03876	   0.00000
    0.23438	   0.05574	   0.00000
    0.23828	   0.07161	   0.00000
    0.24219	   0.08621	   0.00000
    0.24609	   0.09941	   0.00000
    0.25000	   0.11111	   0.00000
    0.25391	   0.12120	   0.00000
    0.25781	   0.12961	   0.00000
    0.26172	   0.13629	   0.00000
    0.26563	   0.14119	   0.00000
    0.26953	   0.14431	   0.00000
    0.27344	   0.14563	   0.00000
    0.27734	   0.14520	   0.00000
    0.28125	   0.14305	   0.00000
    0.28516	   0.13923	   0.00000
    0.28906	   0.13383	   0.00000
    0.29297	   0.12693	   0.00000
    0.29688	   0.11865	   0.00000
    0.30078	   0.10911	   0.00000
    0.30469	   0.09843	   0.00000
    0.30859	   0.08675	   0.00000
    0.31250	   0.07424	   0.00000
    0.31641	   0.06105	   0.00000
    0.32031	   0.04733	   0.00000
    0.32422	   0.03326	   0.00000
    0.32813	   0.01901	   0.00000
    0.33203	   0.00473	   0.00000
    0.33594	   0.00939	   3.14159
    0.33984	   0.02321	   3.14159
    0.34375	   0.03657	   3.14159
    0.34766	   0.04932	   3.14159
    0.35156	   0.06131	   3.14159
    0.35547	   0.07243	   3.14159
    0.35938	   0.08254	   3.14159
    0.36328	   0.09155	   3.14159
    0.36719	   0.09937	   3.14159
    0.37109	   0.10591	   3.14159
    0.37500	   0.11111	   3.14159
    0.37891	   0.11493	   3.14159
    0.38281	   0.11734	   3.14159
    0.38672	   0.11832	   3.14159
    0.39063	   0.11787	   3.14159
    0.39453	   0.11601	   3.14159
    0.39844	   0.11278	   3.14159
    0.40234	   0.10822	   3.14159
    0.40625	   0.10240	   3.14159
    0.41016	   0.09540	   3.14159
    0.41406	   0.08730	   3.14159
    0.41797	   0.07820	   3.14159
    0.42188	   0.06823	   3.14159
    0.42578	   0.05750	   3.14159
    0.42969	   0.04615	   3.14159
    0.43359	   0.03430	   3.14159
    0.43750	   0.02210	   3.14159
    0.44141	   0.00970	   3.14159
    0.44531	   0.00277	   0.00000
    0.44922	   0.01515	   0.00000
    0.45313	   0.02729	   0.00000
    0.45703	   0.03907	   0.00000
    0.46094	   0.05034	   0.00000
    0.46484	   0.06096	   0.00000
    0.46875	   0.07083	   0.00000
    0.47266	   0.07982	   0.00000
    0.47656	   0.08783	   0.00000
    0.48047	   0.09477	   0.00000
    0.48438	   0.10056	   0.00000
    0.48828	   0.10514	   0.00000
    0.49219	   0.10844	   0.00000
    0.49609	   0.11044	   0.00000
    0.50000	   0.11111	   0.00000
if (mode = "nobatch" or mode = "inter")
end-if
otf1 inp=s1edge.img size=(1,1,35,40) plotout=test21
Beginning VICAR task otf1
OTF1 version 2015-08-10
INL Truncated to Image Length
INS Truncated to Image Width
NUMBER OF LINES PROCESSED=         20

 PIXEL  CYCLES/      FREQUENCY    REAL(FT)     IMAG.(FT)     INTENSITY    AMPLITUDE     PHASE     WAVE LENGT
         PIXEL                                                                         RADIANS      SHIFT
  1     0.00000       0.00000      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
  2     0.00391       0.00391      0.99799     -0.00000      0.99599      0.99799      0.00000      0.00000
  3     0.00781       0.00781      0.99199      0.00000      0.98404      0.99199      0.00000      0.00000
  4     0.01172       0.01172      0.98202      0.00000      0.96437      0.98202      0.00000      0.00000
  5     0.01563       0.01563      0.96818      0.00000      0.93736      0.96818      0.00000      0.00000
  6     0.01953       0.01953      0.95054      0.00000      0.90352      0.95054      0.00000      0.00000
  7     0.02344       0.02344      0.92924     -0.00000      0.86348      0.92924      0.00000      0.00000
  8     0.02734       0.02734      0.90443      0.00000      0.81799      0.90443      0.00000      0.00000
  9     0.03125       0.03125      0.87628      0.00000      0.76786      0.87628      0.00000      0.00000
 10     0.03516       0.03516      0.84498     -0.00000      0.71400      0.84498      0.00000      0.00000
 11     0.03906       0.03906      0.81077      0.00000      0.65735      0.81077      0.00000      0.00000
 12     0.04297       0.04297      0.77388      0.00000      0.59888      0.77388      0.00000      0.00000
 13     0.04688       0.04688      0.73455      0.00000      0.53957      0.73455      0.00000      0.00000
 14     0.05078       0.05078      0.69307     -0.00000      0.48035      0.69307      0.00000      0.00000
 15     0.05469       0.05469      0.64972      0.00000      0.42214      0.64972      0.00000      0.00000
 16     0.05859       0.05859      0.60479     -0.00000      0.36578      0.60479      0.00000      0.00000
 17     0.06250       0.06250      0.55859      0.00000      0.31203      0.55859      0.00000      0.00000
 18     0.06641       0.06641      0.51143     -0.00000      0.26156      0.51143      0.00000      0.00000
 19     0.07031       0.07031      0.46362     -0.00000      0.21494      0.46362      0.00000      0.00000
 20     0.07422       0.07422      0.41547      0.00000      0.17261      0.41547      0.00000      0.00000
 21     0.07813       0.07813      0.36729      0.00000      0.13491      0.36729      0.00000      0.00000
 22     0.08203       0.08203      0.31941     -0.00000      0.10202      0.31941      0.00000      0.00000
 23     0.08594       0.08594      0.27211     -0.00000      0.07404      0.27211      0.00000      0.00000
 24     0.08984       0.08984      0.22569     -0.00000      0.05094      0.22569      0.00000      0.00000
 25     0.09375       0.09375      0.18043      0.00000      0.03256      0.18043      0.00000      0.00000
 26     0.09766       0.09766      0.13661     -0.00000      0.01866      0.13661      0.00000      0.00000
 27     0.10156       0.10156      0.09447      0.00000      0.00893      0.09447      0.00000      0.00000
 28     0.10547       0.10547      0.05426     -0.00000      0.00294      0.05426      0.00000      0.00000
 29     0.10938       0.10938      0.01618      0.00000      0.00026      0.01618      0.00000      0.00000
 30     0.11328       0.11328     -0.01956     -0.00000      0.00038      0.01956      3.14159      4.41379
 31     0.11719       0.11719     -0.05278      0.00000      0.00279      0.05278      3.14159      4.26667
 32     0.12109       0.12109     -0.08334     -0.00000      0.00695      0.08334      3.14159      4.12903
 33     0.12500       0.12500     -0.11111      0.00000      0.01235      0.11111      3.14159      4.00000
 34     0.12891       0.12891     -0.13598     -0.00000      0.01849      0.13598      3.14159      3.87879
 35     0.13281       0.13281     -0.15788     -0.00000      0.02493      0.15788      3.14159      3.76471
 36     0.13672       0.13672     -0.17674     -0.00000      0.03124      0.17674      3.14159      3.65714
 37     0.14063       0.14063     -0.19256      0.00000      0.03708      0.19256      3.14159      3.55556
 38     0.14453       0.14453     -0.20531     -0.00000      0.04215      0.20531      3.14159      3.45946
 39     0.14844       0.14844     -0.21502      0.00000      0.04623      0.21502      3.14159      3.36842
 40     0.15234       0.15234     -0.22175     -0.00000      0.04917      0.22175      3.14159      3.28205
 41     0.15625       0.15625     -0.22556      0.00000      0.05088      0.22556      3.14159      3.20000
 42     0.16016       0.16016     -0.22654     -0.00000      0.05132      0.22654      3.14159      3.12195
 43     0.16406       0.16406     -0.22481      0.00000      0.05054      0.22481      3.14159      3.04762
 44     0.16797       0.16797     -0.22051     -0.00000      0.04863      0.22051      3.14159      2.97674
 45     0.17188       0.17188     -0.21379      0.00000      0.04570      0.21379      3.14159      2.90909
 46     0.17578       0.17578     -0.20481      0.00000      0.04195      0.20481      3.14159      2.84444
 47     0.17969       0.17969     -0.19377      0.00000      0.03755      0.19377      3.14159      2.78261
 48     0.18359       0.18359     -0.18086     -0.00000      0.03271      0.18086      3.14159      2.72340
 49     0.18750       0.18750     -0.16629      0.00000      0.02765      0.16629      3.14159      2.66667
 50     0.19141       0.19141     -0.15028     -0.00000      0.02258      0.15028      3.14159      2.61224
 51     0.19531       0.19531     -0.13306      0.00000      0.01770      0.13306      3.14159      2.56000
 52     0.19922       0.19922     -0.11485     -0.00000      0.01319      0.11485      3.14159      2.50980
 53     0.20313       0.20313     -0.09589      0.00000      0.00920      0.09589      3.14159      2.46154
 54     0.20703       0.20703     -0.07641      0.00000      0.00584      0.07641      3.14159      2.41509
 55     0.21094       0.21094     -0.05665      0.00000      0.00321      0.05665      3.14159      2.37037
 56     0.21484       0.21484     -0.03683      0.00000      0.00136      0.03683      3.14159      2.32727
 57     0.21875       0.21875     -0.01717      0.00000      0.00029      0.01717      3.14159      2.28571
 58     0.22266       0.22266      0.00212     -0.00000      0.00000      0.00212      0.00000      0.00000
 59     0.22656       0.22656      0.02082      0.00000      0.00043      0.02082      0.00000      0.00000
 60     0.23047       0.23047      0.03876     -0.00000      0.00150      0.03876      0.00000      0.00000
 61     0.23438       0.23438      0.05574      0.00000      0.00311      0.05574      0.00000      0.00000
 62     0.23828       0.23828      0.07161      0.00000      0.00513      0.07161      0.00000      0.00000
 63     0.24219       0.24219      0.08621      0.00000      0.00743      0.08621      0.00000      0.00000
 64     0.24609       0.24609      0.09941      0.00000      0.00988      0.09941      0.00000      0.00000
 65     0.25000       0.25000      0.11111      0.00000      0.01235      0.11111      0.00000      0.00000
 66     0.25391       0.25391      0.12120     -0.00000      0.01469      0.12120      0.00000      0.00000
 67     0.25781       0.25781      0.12961      0.00000      0.01680      0.12961      0.00000      0.00000
 68     0.26172       0.26172      0.13629      0.00000      0.01857      0.13629      0.00000      0.00000
 69     0.26563       0.26563      0.14119      0.00000      0.01994      0.14119      0.00000      0.00000
 70     0.26953       0.26953      0.14431      0.00000      0.02082      0.14431      0.00000      0.00000
 71     0.27344       0.27344      0.14563      0.00000      0.02121      0.14563      0.00000      0.00000
 72     0.27734       0.27734      0.14520      0.00000      0.02108      0.14520      0.00000      0.00000
 73     0.28125       0.28125      0.14305      0.00000      0.02046      0.14305      0.00000      0.00000
 74     0.28516       0.28516      0.13923      0.00000      0.01938      0.13923      0.00000      0.00000
 75     0.28906       0.28906      0.13383     -0.00000      0.01791      0.13383      0.00000      0.00000
 76     0.29297       0.29297      0.12693      0.00000      0.01611      0.12693      0.00000      0.00000
 77     0.29688       0.29688      0.11865      0.00000      0.01408      0.11865      0.00000      0.00000
 78     0.30078       0.30078      0.10911     -0.00000      0.01190      0.10911      0.00000      0.00000
 79     0.30469       0.30469      0.09843     -0.00000      0.00969      0.09843      0.00000      0.00000
 80     0.30859       0.30859      0.08675     -0.00000      0.00753      0.08675      0.00000      0.00000
 81     0.31250       0.31250      0.07424      0.00000      0.00551      0.07424      0.00000      0.00000
 82     0.31641       0.31641      0.06105     -0.00000      0.00373      0.06105      0.00000      0.00000
 83     0.32031       0.32031      0.04733     -0.00000      0.00224      0.04733      0.00000      0.00000
 84     0.32422       0.32422      0.03326     -0.00000      0.00111      0.03326      0.00000      0.00000
 85     0.32813       0.32813      0.01901      0.00000      0.00036      0.01901      0.00000      0.00000
 86     0.33203       0.33203      0.00473     -0.00000      0.00002      0.00473      0.00000      0.00000
 87     0.33594       0.33594     -0.00939     -0.00000      0.00009      0.00939      3.14159      1.48837
 88     0.33984       0.33984     -0.02321     -0.00000      0.00054      0.02321      3.14159      1.47126
 89     0.34375       0.34375     -0.03657      0.00000      0.00134      0.03657      3.14159      1.45455
 90     0.34766       0.34766     -0.04932     -0.00000      0.00243      0.04932      3.14159      1.43820
 91     0.35156       0.35156     -0.06131     -0.00000      0.00376      0.06131      3.14159      1.42222
 92     0.35547       0.35547     -0.07243     -0.00000      0.00525      0.07243      3.14159      1.40659
 93     0.35938       0.35938     -0.08254      0.00000      0.00681      0.08254      3.14159      1.39130
 94     0.36328       0.36328     -0.09155     -0.00000      0.00838      0.09155      3.14159      1.37634
 95     0.36719       0.36719     -0.09937     -0.00000      0.00987      0.09937      3.14159      1.36170
 96     0.37109       0.37109     -0.10591     -0.00000      0.01122      0.10591      3.14159      1.34737
 97     0.37500       0.37500     -0.11111      0.00000      0.01235      0.11111      3.14159      1.33333
 98     0.37891       0.37891     -0.11493      0.00000      0.01321      0.11493      3.14159      1.31959
 99     0.38281       0.38281     -0.11734     -0.00000      0.01377      0.11734      3.14159      1.30612
100     0.38672       0.38672     -0.11832     -0.00000      0.01400      0.11832      3.14159      1.29293
101     0.39063       0.39063     -0.11787      0.00000      0.01389      0.11787      3.14159      1.28000
102     0.39453       0.39453     -0.11601     -0.00000      0.01346      0.11601      3.14159      1.26733
103     0.39844       0.39844     -0.11278     -0.00000      0.01272      0.11278      3.14159      1.25490
104     0.40234       0.40234     -0.10822     -0.00000      0.01171      0.10822      3.14159      1.24272
105     0.40625       0.40625     -0.10240      0.00000      0.01049      0.10240      3.14159      1.23077
106     0.41016       0.41016     -0.09540      0.00000      0.00910      0.09540      3.14159      1.21905
107     0.41406       0.41406     -0.08730      0.00000      0.00762      0.08730      3.14159      1.20755
108     0.41797       0.41797     -0.07820     -0.00000      0.00612      0.07820      3.14159      1.19626
109     0.42188       0.42188     -0.06823      0.00000      0.00466      0.06823      3.14159      1.18519
110     0.42578       0.42578     -0.05750      0.00000      0.00331      0.05750      3.14159      1.17431
111     0.42969       0.42969     -0.04615     -0.00000      0.00213      0.04615      3.14159      1.16364
112     0.43359       0.43359     -0.03430     -0.00000      0.00118      0.03430      3.14159      1.15315
113     0.43750       0.43750     -0.02210      0.00000      0.00049      0.02210      3.14159      1.14286
114     0.44141       0.44141     -0.00970      0.00000      0.00009      0.00970      3.14159      1.13274
115     0.44531       0.44531      0.00277      0.00000      0.00001      0.00277      0.00000      0.00000
116     0.44922       0.44922      0.01515     -0.00000      0.00023      0.01515      0.00000      0.00000
117     0.45313       0.45313      0.02729      0.00000      0.00074      0.02729      0.00000      0.00000
118     0.45703       0.45703      0.03907     -0.00000      0.00153      0.03907      0.00000      0.00000
119     0.46094       0.46094      0.05034      0.00000      0.00253      0.05034      0.00000      0.00000
120     0.46484       0.46484      0.06096      0.00000      0.00372      0.06096      0.00000      0.00000
121     0.46875       0.46875      0.07083      0.00000      0.00502      0.07083      0.00000      0.00000
122     0.47266       0.47266      0.07982      0.00000      0.00637      0.07982      0.00000      0.00000
123     0.47656       0.47656      0.08783     -0.00000      0.00771      0.08783      0.00000      0.00000
124     0.48047       0.48047      0.09477      0.00000      0.00898      0.09477      0.00000      0.00000
125     0.48438       0.48438      0.10056      0.00000      0.01011      0.10056      0.00000      0.00000
126     0.48828       0.48828      0.10514      0.00000      0.01105      0.10514      0.00000      0.00000
127     0.49219       0.49219      0.10844     -0.00000      0.01176      0.10844      0.00000      0.00000
128     0.49609       0.49609      0.11044      0.00000      0.01220      0.11044      0.00000      0.00000
129     0.50000       0.50000      0.11111      0.00000      0.01235      0.11111      0.00000      0.00000
NUMBER POINTS OUTPUT =         129
**** This image is ALIASED ****
INTEGRATED MTF AMPLITUDE FROM .25 TO .48 =       4.94845
if (mode = "nobatch" or mode = "inter")
end-if
otf1 inp=s1edge.img 'reflect plotout=test22
Beginning VICAR task otf1
OTF1 version 2015-08-10
NUMBER OF LINES PROCESSED=         20

 PIXEL  CYCLES/      FREQUENCY    REAL(FT)     IMAG.(FT)     INTENSITY    AMPLITUDE     PHASE     WAVE LENGT
         PIXEL                                                                         RADIANS      SHIFT
  1     0.00000       0.00000      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
  2     0.00391       0.00391      0.99377      0.00002      0.98758      0.99377      0.00000      0.00000
  3     0.00781       0.00781      0.97644      0.00015      0.95343      0.97644      0.00000      0.00000
  4     0.01172       0.01172      0.94713      0.00052      0.89705      0.94713      0.00000      0.00000
  5     0.01563       0.01563      0.90766      0.00120      0.82385      0.90766      0.00132      0.01345
  6     0.01953       0.01953      0.85762      0.00233      0.73551      0.85762      0.00271      0.02210
  7     0.02344       0.02344      0.79923      0.00391      0.63878      0.79924      0.00490      0.03324
  8     0.02734       0.02734      0.73243      0.00610      0.53650      0.73246      0.00832      0.04845
  9     0.03125       0.03125      0.65979      0.00882      0.43541      0.65985      0.01336      0.06807
 10     0.03516       0.03516      0.58151      0.01221      0.33830      0.58164      0.02099      0.09501
 11     0.03906       0.03906      0.50033      0.01612      0.25059      0.50059      0.03221      0.13125
 12     0.04297       0.04297      0.41659      0.02068      0.17398      0.41711      0.04961      0.18375
 13     0.04688       0.04688      0.33309      0.02567      0.11161      0.33408      0.07692      0.26118
 14     0.05078       0.05078      0.25014      0.03120      0.06354      0.25208      0.12407      0.38887
 15     0.05469       0.05469      0.17046      0.03697      0.03042      0.17442      0.21357      0.62154
 16     0.05859       0.05859      0.09420      0.04309      0.01073      0.10359      0.42900      1.16528
 17     0.06250       0.06250      0.02387      0.04922      0.00299      0.05470      1.11921      2.85005
 18     0.06641       0.06641     -0.04065      0.05546      0.00473      0.06877      2.20335      5.28073
 19     0.07031       0.07031     -0.09720      0.06143      0.01322      0.11498      2.57793      5.83524
 20     0.07422       0.07422     -0.14624      0.06727      0.02591      0.16097      2.71043      5.81226
 21     0.07813       0.07813     -0.18600      0.07256      0.03986      0.19965      2.76962      5.64223
 22     0.08203       0.08203     -0.21738      0.07749      0.05326      0.23078      2.79918      5.43089
 23     0.08594       0.08594     -0.23903      0.08162      0.06379      0.25258      2.81255      5.20881
 24     0.08984       0.08984     -0.25226      0.08519      0.07089      0.26625      2.81591      4.98828
 25     0.09375       0.09375     -0.25613      0.08779      0.07331      0.27076      2.81140      4.77279
 26     0.09766       0.09766     -0.25237      0.08972      0.07174      0.26784      2.80003      4.56335
 27     0.10156       0.10156     -0.24039      0.09056      0.06599      0.25688      2.78129      4.35846
 28     0.10547       0.10547     -0.22222      0.09073      0.05761      0.24003      2.75397      4.15581
 29     0.10938       0.10938     -0.19756      0.08980      0.04709      0.21701      2.71496      3.95062
 30     0.11328       0.11328     -0.16868      0.08826      0.03624      0.19037      2.65950      3.73648
 31     0.11719       0.11719     -0.13542      0.08572      0.02569      0.16027      2.57726      3.50023
 32     0.12109       0.12109     -0.10017      0.08273      0.01688      0.12991      2.45124      3.22170
 33     0.12500       0.12500     -0.06282      0.07890      0.01017      0.10085      2.24320      2.85613
 34     0.12891       0.12891     -0.02573      0.07485      0.00626      0.07915      1.90189      2.34819
 35     0.13281       0.13281      0.01126      0.07018      0.00505      0.07108      1.41167      1.69167
 36     0.13672       0.13672      0.04593      0.06557      0.00641      0.08005      0.95970      1.11719
 37     0.14063       0.14063      0.07862      0.06058      0.00985      0.09925      0.65650      0.74300
 38     0.14453       0.14453      0.10731      0.05592      0.01464      0.12101      0.48037      0.52897
 39     0.14844       0.14844      0.13261      0.05112      0.02020      0.14213      0.36795      0.39451
 40     0.15234       0.15234      0.15279      0.04691      0.02555      0.15983      0.29790      0.31122
 41     0.15625       0.15625      0.16874      0.04275      0.03030      0.17407      0.24815      0.25276
 42     0.16016       0.16016      0.17905      0.03938      0.03361      0.18333      0.21648      0.21512
 43     0.16406       0.16406      0.18494      0.03618      0.03551      0.18845      0.19317      0.18739
 44     0.16797       0.16797      0.18530      0.03388      0.03549      0.18838      0.18081      0.17133
 45     0.17188       0.17188      0.18164      0.03178      0.03400      0.18440      0.17321      0.16040
 46     0.17578       0.17578      0.17311      0.03061      0.03090      0.17580      0.17504      0.15848
 47     0.17969       0.17969      0.16146      0.02959      0.02695      0.16415      0.18125      0.16054
 48     0.18359       0.18359      0.14606      0.02943      0.02220      0.14900      0.19882      0.17236
 49     0.18750       0.18750      0.12881      0.02926      0.01745      0.13209      0.22338      0.18961
 50     0.19141       0.19141      0.10919      0.02981      0.01281      0.11318      0.26653      0.22162
 51     0.19531       0.19531      0.08918      0.03014      0.00886      0.09413      0.32589      0.26556
 52     0.19922       0.19922      0.06828      0.03098      0.00562      0.07497      0.42591      0.34026
 53     0.20313       0.20313      0.04845      0.03133      0.00333      0.05769      0.57403      0.44977
 54     0.20703       0.20703      0.02912      0.03197      0.00187      0.04325      0.83201      0.63960
 55     0.21094       0.21094      0.01216      0.03186      0.00116      0.03410      1.20631      0.91017
 56     0.21484       0.21484     -0.00316      0.03182      0.00102      0.03198      1.66979      1.23697
 57     0.21875       0.21875     -0.01515      0.03078      0.00118      0.03431      2.02817      1.47562
 58     0.22266       0.22266     -0.02472      0.02965      0.00149      0.03860      2.26567      1.61950
 59     0.22656       0.22656     -0.03041      0.02734      0.00167      0.04089      2.40931      1.69249
 60     0.23047       0.23047     -0.03335      0.02483      0.00173      0.04158      2.50147      1.72744
 61     0.23438       0.23438     -0.03231      0.02104      0.00149      0.03856      2.56441      1.74139
 62     0.23828       0.23828     -0.02866      0.01706      0.00111      0.03335      2.60474      1.73978
 63     0.24219       0.24219     -0.02139      0.01178      0.00060      0.02442      2.63822      1.73372
 64     0.24609       0.24609     -0.01206      0.00642      0.00019      0.01366      2.65277      1.71561
 65     0.25000       0.25000      0.00015     -0.00015      0.00000      0.00022      0.00000      0.00000
 66     0.25391       0.25391      0.01355     -0.00662      0.00023      0.01508     -0.45430     -0.28477
 67     0.25781       0.25781      0.02883     -0.01411      0.00103      0.03210     -0.45515     -0.28098
 68     0.26172       0.26172      0.04421     -0.02123      0.00240      0.04904     -0.44764     -0.27222
 69     0.26563       0.26563      0.06033     -0.02913      0.00449      0.06700     -0.44987     -0.26955
 70     0.26953       0.26953      0.07540     -0.03634      0.00701      0.08371     -0.44913     -0.26521
 71     0.27344       0.27344      0.09011     -0.04408      0.01006      0.10032     -0.45498     -0.26482
 72     0.27734       0.27734      0.10271     -0.05079      0.01313      0.11458     -0.45925     -0.26354
 73     0.28125       0.28125      0.11397     -0.05777      0.01633      0.12778     -0.46915     -0.26548
 74     0.28516       0.28516      0.12229     -0.06341      0.01897      0.13775     -0.47838     -0.26700
 75     0.28906       0.28906      0.12858     -0.06912      0.02131      0.14598     -0.49324     -0.27157
 76     0.29297       0.29297      0.13139     -0.07323      0.02263      0.15042     -0.50847     -0.27622
 77     0.29688       0.29688      0.13184     -0.07728      0.02335      0.15282     -0.53019     -0.28424
 78     0.30078       0.30078      0.12866     -0.07957      0.02288      0.15128     -0.55392     -0.29310
 79     0.30469       0.30469      0.12313     -0.08177      0.02185      0.14781     -0.58621     -0.30621
 80     0.30859       0.30859      0.11420     -0.08215      0.01979      0.14068     -0.62358     -0.32160
 81     0.31250       0.31250      0.10333     -0.08251      0.01748      0.13223     -0.67383     -0.34318
 82     0.31641       0.31641      0.08960     -0.08108      0.01460      0.12084     -0.73557     -0.37000
 83     0.32031       0.32031      0.07462     -0.07983      0.01194      0.10928     -0.81911     -0.40700
 84     0.32422       0.32422      0.05761     -0.07693      0.00924      0.09611     -0.92806     -0.45557
 85     0.32813       0.32813      0.04025     -0.07447      0.00717      0.08465     -1.07533     -0.52158
 86     0.33203       0.33203      0.02180     -0.07056      0.00545      0.07385     -1.27112     -0.60930
 87     0.33594       0.33594      0.00400     -0.06743      0.00456      0.06755     -1.51151     -0.71610
 88     0.33984       0.33984     -0.01390     -0.06308      0.00417      0.06459     -1.78772     -0.83722
 89     0.34375       0.34375     -0.03021     -0.05986      0.00450      0.06705     -2.03821     -0.94368
 90     0.34766       0.34766     -0.04575     -0.05565      0.00519      0.07204     -2.25884     -1.03408
 91     0.35156       0.35156     -0.05890     -0.05292      0.00627      0.07918     -2.40963     -1.09086
 92     0.35547       0.35547     -0.07060     -0.04937      0.00742      0.08615     -2.53135     -1.13337
 93     0.35938       0.35938     -0.07938     -0.04760      0.00857      0.09256     -2.60149     -1.15211
 94     0.36328       0.36328     -0.08633     -0.04510      0.00949      0.09740     -2.66016     -1.16543
 95     0.36719       0.36719     -0.09012     -0.04459      0.01011      0.10054     -2.68214     -1.16256
 96     0.37109       0.37109     -0.09200     -0.04334      0.01034      0.10170     -2.70134     -1.15855
 97     0.37500       0.37500     -0.09084     -0.04420      0.01021      0.10102     -2.68880     -1.14116
 98     0.37891       0.37891     -0.08803     -0.04418      0.00970      0.09850     -2.67642     -1.12420
 99     0.38281       0.38281     -0.08258     -0.04628      0.00896      0.09466     -2.63072     -1.09373
100     0.38672       0.38672     -0.07601     -0.04727      0.00801      0.08951     -2.58526     -1.06397
101     0.39063       0.39063     -0.06745     -0.05029      0.00708      0.08413     -2.50090     -1.01896
102     0.39453       0.39453     -0.05851     -0.05184      0.00611      0.07817     -2.41657     -0.97485
103     0.39844       0.39844     -0.04837     -0.05529      0.00540      0.07346     -2.28954     -0.91455
104     0.40234       0.40234     -0.03870     -0.05683      0.00473      0.06876     -2.16856     -0.85782
105     0.40625       0.40625     -0.02867     -0.06013      0.00444      0.06662     -2.01569     -0.78968
106     0.41016       0.41016     -0.01993     -0.06104      0.00412      0.06421     -1.88635     -0.73197
107     0.41406       0.41406     -0.01161     -0.06357      0.00418      0.06462     -1.75142     -0.67320
108     0.41797       0.41797     -0.00528     -0.06323      0.00403      0.06345     -1.65413     -0.62986
109     0.42188       0.42188      0.00001     -0.06447      0.00416      0.06447     -1.57062     -0.59253
110     0.42578       0.42578      0.00282     -0.06237      0.00390      0.06243     -1.52562     -0.57027
111     0.42969       0.42969      0.00423     -0.06192      0.00385      0.06207     -1.50262     -0.55656
112     0.43359       0.43359      0.00293     -0.05773      0.00334      0.05780     -1.52010     -0.55797
113     0.43750       0.43750      0.00015     -0.05544      0.00307      0.05544     -1.56800     -0.57041
114     0.44141       0.44141     -0.00525     -0.04903      0.00243      0.04931     -1.67756     -0.60487
115     0.44531       0.44531     -0.01192     -0.04497      0.00216      0.04652     -1.82989     -0.65400
116     0.44922       0.44922     -0.02086     -0.03646      0.00176      0.04201     -2.09052     -0.74066
117     0.45313       0.45313     -0.03058     -0.03100      0.00190      0.04355     -2.34931     -0.82517
118     0.45703       0.45703     -0.04198     -0.02071      0.00219      0.04681     -2.68326     -0.93441
119     0.46094       0.46094     -0.05348     -0.01447      0.00307      0.05540     -2.87735     -0.99351
120     0.46484       0.46484     -0.06591     -0.00286      0.00435      0.06598     -3.09818     -1.06077
121     0.46875       0.46875     -0.07766      0.00331      0.00604      0.07773      3.09897      1.05219
122     0.47266       0.47266     -0.08953      0.01576      0.00826      0.09091      2.96740      0.99920
123     0.47656       0.47656     -0.09993      0.02076      0.01042      0.10206      2.93675      0.98077
124     0.48047       0.48047     -0.10970      0.03380      0.01318      0.11479      2.84272      0.94165
125     0.48438       0.48438     -0.11730      0.03608      0.01506      0.12272      2.84322      0.93422
126     0.48828       0.48828     -0.12365      0.05042      0.01783      0.13354      2.75444      0.89781
127     0.49219       0.49219     -0.12736      0.04653      0.01838      0.13559      2.79134      0.90261
128     0.49609       0.49609     -0.12937      0.06891      0.02148      0.14658      2.65217      0.85086
129     0.50000       0.50000     -0.12889      0.00000      0.01661      0.12889      3.14159      1.00000
NUMBER POINTS OUTPUT =         129
INTEGRATED MTF AMPLITUDE FROM .25 TO .48 =       5.04667
if (mode = "nobatch" or mode = "inter")
end-if
otf1 inp=s1edge.img plotout=test23 'mean table=test23.tbl
Beginning VICAR task otf1
OTF1 version 2015-08-10
NUMBER OF LINES PROCESSED=         20

 PIXEL  CYCLES/      FREQUENCY    REAL(FT)     IMAG.(FT)     INTENSITY    AMPLITUDE     PHASE     WAVE LENGT
         PIXEL                                                                         RADIANS      SHIFT
  1     0.00000       0.00000      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
  2     0.00391       0.00391      0.99361      0.00005      0.98726      0.99361      0.00000      0.00000
  3     0.00781       0.00781      0.97732      0.00033      0.95516      0.97732      0.00000      0.00000
  4     0.01172       0.01172      0.94873      0.00113      0.90009      0.94873      0.00120      0.01624
  5     0.01563       0.01563      0.91117      0.00260      0.83023      0.91117      0.00286      0.02909
  6     0.01953       0.01953      0.86266      0.00504      0.74420      0.86267      0.00584      0.04759
  7     0.02344       0.02344      0.80695      0.00844      0.65124      0.80699      0.01046      0.07104
  8     0.02734       0.02734      0.74241      0.01310      0.55135      0.74253      0.01765      0.10272
  9     0.03125       0.03125      0.67311      0.01886      0.45343      0.67338      0.02801      0.14264
 10     0.03516       0.03516      0.59766      0.02596      0.35788      0.59823      0.04340      0.19648
 11     0.03906       0.03906      0.52031      0.03405      0.27188      0.52142      0.06536      0.26628
 12     0.04297       0.04297      0.43978      0.04336      0.19529      0.44192      0.09829      0.36405
 13     0.04688       0.04688      0.36037      0.05336      0.13271      0.36430      0.14701      0.49915
 14     0.05078       0.05078      0.28078      0.06424      0.08296      0.28804      0.22490      0.70488
 15     0.05469       0.05469      0.20520      0.07530      0.04778      0.21858      0.35169      1.02352
 16     0.05859       0.05859      0.13219      0.08673      0.02500      0.15810      0.58067      1.57725
 17     0.06250       0.06250      0.06569      0.09773      0.01387      0.11775      0.97898      2.49295
 18     0.06641       0.06641      0.00399      0.10850      0.01179      0.10857      1.53399      3.67650
 19     0.07031       0.07031     -0.04926      0.11816      0.01639      0.12802      1.96582      4.44971
 20     0.07422       0.07422     -0.09617      0.12699      0.02538      0.15930      2.21895      4.75833
 21     0.07813       0.07813     -0.13347      0.13407      0.03579      0.18918      2.35395      4.79543
 22     0.08203       0.08203     -0.16365      0.13982      0.04633      0.21525      2.43458      4.72352
 23     0.08594       0.08594     -0.18388      0.14329      0.05434      0.23312      2.47960      4.59219
 24     0.08984       0.08984     -0.19703      0.14509      0.05987      0.24469      2.50687      4.44083
 25     0.09375       0.09375     -0.20066      0.14430      0.06109      0.24716      2.51816      4.27496
 26     0.09766       0.09766     -0.19803      0.14170      0.05930      0.24351      2.52051      4.10779
 27     0.10156       0.10156     -0.18702      0.13646      0.05360      0.23151      2.51126      3.93531
 28     0.10547       0.10547     -0.17118      0.12954      0.04608      0.21467      2.49380      3.76321
 29     0.10938       0.10938     -0.14863      0.12016      0.03653      0.19113      2.46174      3.58215
 30     0.11328       0.11328     -0.12312      0.10950      0.02715      0.16477      2.41471      3.39255
 31     0.11719       0.11719     -0.09292      0.09681      0.01800      0.13418      2.33569      3.17215
 32     0.12109       0.12109     -0.06183      0.08344      0.01079      0.10385      2.20851      2.90268
 33     0.12500       0.12500     -0.02815      0.06866      0.00551      0.07421      1.95993      2.49546
 34     0.12891       0.12891      0.00432      0.05397      0.00293      0.05414      1.49084      1.84067
 35     0.13281       0.13281      0.03741      0.03858      0.00289      0.05373      0.80080      0.95963
 36     0.13672       0.13672      0.06744      0.02409      0.00513      0.07161      0.34311      0.39942
 37     0.14063       0.14063      0.09640      0.00965      0.00939      0.09688      0.09972      0.11286
 38     0.14453       0.14453      0.12086     -0.00311      0.01462      0.12090     -0.02577     -0.02838
 39     0.14844       0.14844      0.14303     -0.01519      0.02069      0.14383     -0.10583     -0.11347
 40     0.15234       0.15234      0.15975     -0.02494      0.02614      0.16168     -0.15485     -0.16177
 41     0.15625       0.15625      0.17352     -0.03353      0.03123      0.17673     -0.19089     -0.19444
 42     0.16016       0.16016      0.18145     -0.03935      0.03447      0.18567     -0.21358     -0.21224
 43     0.16406       0.16406      0.18632     -0.04381      0.03664      0.19141     -0.23092     -0.22401
 44     0.16797       0.16797      0.18553     -0.04532      0.03647      0.19098     -0.23958     -0.22701
 45     0.17188       0.17188      0.18209     -0.04552      0.03523      0.18769     -0.24498     -0.22685
 46     0.17578       0.17578      0.17361     -0.04289      0.03198      0.17883     -0.24221     -0.21930
 47     0.17969       0.17969      0.16333     -0.03930      0.02822      0.16799     -0.23611     -0.20913
 48     0.18359       0.18359      0.14899     -0.03324      0.02330      0.15266     -0.21953     -0.19031
 49     0.18750       0.18750      0.13395     -0.02681      0.01866      0.13661     -0.19751     -0.16766
 50     0.19141       0.19141      0.11605     -0.01850      0.01381      0.11752     -0.15806     -0.13143
 51     0.19531       0.19531      0.09867     -0.01056      0.00985      0.09924     -0.10661     -0.08687
 52     0.19922       0.19922      0.07966     -0.00146      0.00635      0.07967     -0.01831     -0.01463
 53     0.20313       0.20313      0.06236      0.00643      0.00393      0.06269      0.10281      0.08055
 54     0.20703       0.20703      0.04453      0.01475      0.00220      0.04691      0.31995      0.24596
 55     0.21094       0.21094      0.02942      0.02105      0.00131      0.03618      0.62118      0.46869
 56     0.21484       0.21484      0.01466      0.02713      0.00095      0.03084      1.07541      0.79665
 57     0.21875       0.21875      0.00334      0.03050      0.00094      0.03068      1.46180      1.06356
 58     0.22266       0.22266     -0.00708      0.03317      0.00115      0.03391      1.78115      1.27317
 59     0.22656       0.22656     -0.01369      0.03265      0.00125      0.03541      1.96776      1.38230
 60     0.23047       0.23047     -0.01920      0.03121      0.00134      0.03665      2.12231      1.46561
 61     0.23438       0.23438     -0.02089      0.02639      0.00113      0.03366      2.24041      1.52138
 62     0.23828       0.23828     -0.02164      0.02070      0.00090      0.02995      2.37841      1.58861
 63     0.24219       0.24219     -0.01889      0.01172      0.00049      0.02223      2.58606      1.69944
 64     0.24609       0.24609     -0.01565      0.00224      0.00025      0.01581      2.99934      1.93974
 65     0.25000       0.25000     -0.00947     -0.01016      0.00019      0.01389     -2.32098     -1.47758
 66     0.25391       0.25391     -0.00348     -0.02245      0.00052      0.02272     -1.72461     -1.08103
 67     0.25781       0.25781      0.00471     -0.03707      0.00140      0.03737     -1.44428     -0.89160
 68     0.26172       0.26172      0.01196     -0.05075      0.00272      0.05214     -1.33946     -0.81455
 69     0.26563       0.26563      0.02062     -0.06604      0.00479      0.06918     -1.26818     -0.75986
 70     0.26953       0.26953      0.02756     -0.07948      0.00708      0.08412     -1.23704     -0.73046
 71     0.27344       0.27344      0.03520     -0.09375      0.01003      0.10014     -1.21167     -0.70526
 72     0.27734       0.27734      0.04045     -0.10529      0.01272      0.11279     -1.20399     -0.69091
 73     0.28125       0.28125      0.04582     -0.11695      0.01578      0.12561     -1.19737     -0.67757
 74     0.28516       0.28516      0.04833     -0.12509      0.01798      0.13410     -1.20208     -0.67092
 75     0.28906       0.28906      0.05059     -0.13284      0.02021      0.14215     -1.20690     -0.66451
 76     0.29297       0.29297      0.04974     -0.13649      0.02110      0.14527     -1.22130     -0.66347
 77     0.29688       0.29688      0.04852     -0.13948      0.02181      0.14768     -1.23603     -0.66264
 78     0.30078       0.30078      0.04418     -0.13806      0.02101      0.14496     -1.26107     -0.66728
 79     0.30469       0.30469      0.03959     -0.13603      0.02007      0.14167     -1.28759     -0.67258
 80     0.30859       0.30859      0.03210     -0.12957      0.01782      0.13349     -1.32793     -0.68487
 81     0.31250       0.31250      0.02468     -0.12284      0.01570      0.12530     -1.37255     -0.69903
 82     0.31641       0.31641      0.01477     -0.11198      0.01276      0.11295     -1.43968     -0.72417
 83     0.32031       0.32031      0.00538     -0.10146      0.01032      0.10161     -1.51778     -0.75415
 84     0.32422       0.32422     -0.00597     -0.08733      0.00766      0.08753     -1.63905     -0.80459
 85     0.32813       0.32813     -0.01625     -0.07438      0.00580      0.07614     -1.78591     -0.86624
 86     0.33203       0.33203     -0.02795     -0.05849      0.00420      0.06483     -2.01662     -0.96664
 87     0.33594       0.33594     -0.03804     -0.04476      0.00345      0.05874     -2.27527     -1.07794
 88     0.33984       0.33984     -0.04903     -0.02879      0.00323      0.05686     -2.61068     -1.22263
 89     0.34375       0.34375     -0.05793     -0.01596      0.00361      0.06009     -2.87278     -1.33009
 90     0.34766       0.34766     -0.06732     -0.00155      0.00453      0.06734     -3.11853     -1.42764
 91     0.35156       0.35156     -0.07427      0.00882      0.00559      0.07479      3.02334      1.36869
 92     0.35547       0.35547     -0.08142      0.02029      0.00704      0.08391      2.89738      1.29725
 93     0.35938       0.35938     -0.08594      0.02702      0.00812      0.09009      2.83698      1.25640
 94     0.36328       0.36328     -0.09056      0.03461      0.00940      0.09695      2.77657      1.21642
 95     0.36719       0.36719     -0.09251      0.03700      0.00993      0.09963      2.76107      1.19677
 96     0.37109       0.37109     -0.09459      0.04033      0.01057      0.10283      2.73855      1.17451
 97     0.37500       0.37500     -0.09411      0.03829      0.01032      0.10160      2.75518      1.16934
 98     0.37891       0.37891     -0.09395      0.03758      0.01024      0.10119      2.76106      1.15975
 99     0.38281       0.38281     -0.09146      0.03160      0.00936      0.09676      2.80891      1.16781
100     0.38672       0.38672     -0.08954      0.02765      0.00878      0.09371      2.84205      1.16965
101     0.39063       0.39063     -0.08558      0.01874      0.00768      0.08761      2.92603      1.19217
102     0.39453       0.39453     -0.08251      0.01281      0.00697      0.08350      2.98755      1.20519
103     0.39844       0.39844     -0.07772      0.00236      0.00605      0.07776      3.11121      1.24277
104     0.40234       0.40234     -0.07411     -0.00399      0.00551      0.07422     -3.08777     -1.22143
105     0.40625       0.40625     -0.06906     -0.01441      0.00498      0.07055     -2.93594     -1.15020
106     0.41016       0.41016     -0.06546     -0.01955      0.00467      0.06831     -2.85135     -1.10642
107     0.41406       0.41406     -0.06062     -0.02839      0.00448      0.06694     -2.70362     -1.03920
108     0.41797       0.41797     -0.05740     -0.03081      0.00424      0.06515     -2.64893     -1.00867
109     0.42188       0.42188     -0.05308     -0.03679      0.00417      0.06459     -2.53550     -0.95653
110     0.42578       0.42578     -0.05047     -0.03532      0.00379      0.06160     -2.53092     -0.94605
111     0.42969       0.42969     -0.04679     -0.03760      0.00360      0.06002     -2.46466     -0.91290
112     0.43359       0.43359     -0.04480     -0.03154      0.00300      0.05479     -2.52822     -0.92801
113     0.43750       0.43750     -0.04171     -0.02984      0.00263      0.05128     -2.52063     -0.91696
114     0.44141       0.44141     -0.04023     -0.01906      0.00198      0.04452     -2.69920     -0.97323
115     0.44531       0.44531     -0.03754     -0.01374      0.00160      0.03998     -2.79072     -0.99740
116     0.44922       0.44922     -0.03634      0.00133      0.00132      0.03637      3.10509      1.10011
117     0.45313       0.45313     -0.03380      0.00927      0.00123      0.03505      2.87383      1.00940
118     0.45703       0.45703     -0.03261      0.02773      0.00183      0.04281      2.43694      0.84863
119     0.46094       0.46094     -0.02995      0.03676      0.00225      0.04742      2.25452      0.77845
120     0.46484       0.46484     -0.02853      0.05743      0.00411      0.06412      2.03186      0.69568
121     0.46875       0.46875     -0.02554      0.06553      0.00495      0.07033      1.94243      0.65951
122     0.47266       0.47266     -0.02371      0.08727      0.00818      0.09044      1.83602      0.61823
123     0.47656       0.47656     -0.02027      0.09196      0.00887      0.09417      1.78771      0.59703
124     0.48047       0.48047     -0.01796      0.11427      0.01338      0.11567      1.72667      0.57196
125     0.48438       0.48438     -0.01407      0.11213      0.01277      0.11301      1.69558      0.55713
126     0.48828       0.48828     -0.01132      0.13673      0.01882      0.13720      1.65341      0.53893
127     0.49219       0.49219     -0.00711      0.12024      0.01451      0.12045      1.62982      0.52702
128     0.49609       0.49609     -0.00400      0.16244      0.02640      0.16249      1.59539      0.51183
129     0.50000       0.50000     -0.00041      0.00000      0.00000      0.00041      0.00000      0.00000
NUMBER POINTS OUTPUT =         129
INTEGRATED MTF AMPLITUDE FROM .25 TO .48 =       4.92731
typetext test23.tbl
Beginning VICAR task typetext
 # FREQUENCY	   AMPLITUDE	   PHASE
    0.00000	   1.00000	   0.00000
    0.00391	   0.99361	   0.00000
    0.00781	   0.97732	   0.00000
    0.01172	   0.94873	   0.00120
    0.01563	   0.91117	   0.00286
    0.01953	   0.86267	   0.00584
    0.02344	   0.80699	   0.01046
    0.02734	   0.74253	   0.01765
    0.03125	   0.67338	   0.02801
    0.03516	   0.59823	   0.04340
    0.03906	   0.52142	   0.06536
    0.04297	   0.44192	   0.09829
    0.04688	   0.36430	   0.14701
    0.05078	   0.28804	   0.22490
    0.05469	   0.21858	   0.35169
    0.05859	   0.15810	   0.58067
    0.06250	   0.11775	   0.97898
    0.06641	   0.10857	   1.53399
    0.07031	   0.12802	   1.96582
    0.07422	   0.15930	   2.21895
    0.07813	   0.18918	   2.35395
    0.08203	   0.21525	   2.43458
    0.08594	   0.23312	   2.47960
    0.08984	   0.24469	   2.50687
    0.09375	   0.24716	   2.51816
    0.09766	   0.24351	   2.52051
    0.10156	   0.23151	   2.51126
    0.10547	   0.21467	   2.49380
    0.10938	   0.19113	   2.46174
    0.11328	   0.16477	   2.41471
    0.11719	   0.13418	   2.33569
    0.12109	   0.10385	   2.20851
    0.12500	   0.07421	   1.95993
    0.12891	   0.05414	   1.49084
    0.13281	   0.05373	   0.80080
    0.13672	   0.07161	   0.34311
    0.14063	   0.09688	   0.09972
    0.14453	   0.12090	  -0.02577
    0.14844	   0.14383	  -0.10583
    0.15234	   0.16168	  -0.15485
    0.15625	   0.17673	  -0.19089
    0.16016	   0.18567	  -0.21358
    0.16406	   0.19141	  -0.23092
    0.16797	   0.19098	  -0.23958
    0.17188	   0.18769	  -0.24498
    0.17578	   0.17883	  -0.24221
    0.17969	   0.16799	  -0.23611
    0.18359	   0.15266	  -0.21953
    0.18750	   0.13661	  -0.19751
    0.19141	   0.11752	  -0.15806
    0.19531	   0.09924	  -0.10661
    0.19922	   0.07967	  -0.01831
    0.20313	   0.06269	   0.10281
    0.20703	   0.04691	   0.31995
    0.21094	   0.03618	   0.62118
    0.21484	   0.03084	   1.07541
    0.21875	   0.03068	   1.46180
    0.22266	   0.03391	   1.78115
    0.22656	   0.03541	   1.96776
    0.23047	   0.03665	   2.12231
    0.23438	   0.03366	   2.24041
    0.23828	   0.02995	   2.37841
    0.24219	   0.02223	   2.58606
    0.24609	   0.01581	   2.99934
    0.25000	   0.01389	  -2.32098
    0.25391	   0.02272	  -1.72461
    0.25781	   0.03737	  -1.44428
    0.26172	   0.05214	  -1.33946
    0.26563	   0.06918	  -1.26818
    0.26953	   0.08412	  -1.23704
    0.27344	   0.10014	  -1.21167
    0.27734	   0.11279	  -1.20399
    0.28125	   0.12561	  -1.19737
    0.28516	   0.13410	  -1.20208
    0.28906	   0.14215	  -1.20690
    0.29297	   0.14527	  -1.22130
    0.29688	   0.14768	  -1.23603
    0.30078	   0.14496	  -1.26107
    0.30469	   0.14167	  -1.28759
    0.30859	   0.13349	  -1.32793
    0.31250	   0.12530	  -1.37255
    0.31641	   0.11295	  -1.43968
    0.32031	   0.10161	  -1.51778
    0.32422	   0.08753	  -1.63905
    0.32813	   0.07614	  -1.78591
    0.33203	   0.06483	  -2.01662
    0.33594	   0.05874	  -2.27527
    0.33984	   0.05686	  -2.61068
    0.34375	   0.06009	  -2.87278
    0.34766	   0.06734	  -3.11853
    0.35156	   0.07479	   3.02334
    0.35547	   0.08391	   2.89738
    0.35938	   0.09009	   2.83698
    0.36328	   0.09695	   2.77657
    0.36719	   0.09963	   2.76107
    0.37109	   0.10283	   2.73855
    0.37500	   0.10160	   2.75518
    0.37891	   0.10119	   2.76106
    0.38281	   0.09676	   2.80891
    0.38672	   0.09371	   2.84205
    0.39063	   0.08761	   2.92603
    0.39453	   0.08350	   2.98755
    0.39844	   0.07776	   3.11121
    0.40234	   0.07422	  -3.08777
    0.40625	   0.07055	  -2.93594
    0.41016	   0.06831	  -2.85135
    0.41406	   0.06694	  -2.70362
    0.41797	   0.06515	  -2.64893
    0.42188	   0.06459	  -2.53550
    0.42578	   0.06160	  -2.53092
    0.42969	   0.06002	  -2.46466
    0.43359	   0.05479	  -2.52822
    0.43750	   0.05128	  -2.52063
    0.44141	   0.04452	  -2.69920
    0.44531	   0.03998	  -2.79072
    0.44922	   0.03637	   3.10509
    0.45313	   0.03505	   2.87383
    0.45703	   0.04281	   2.43694
    0.46094	   0.04742	   2.25452
    0.46484	   0.06412	   2.03186
    0.46875	   0.07033	   1.94243
    0.47266	   0.09044	   1.83602
    0.47656	   0.09417	   1.78771
    0.48047	   0.11567	   1.72667
    0.48438	   0.11301	   1.69558
    0.48828	   0.13720	   1.65341
    0.49219	   0.12045	   1.62982
    0.49609	   0.16249	   1.59539
    0.50000	   0.00041	   0.00000
if (mode = "nobatch" or mode = "inter")
end-if
otf1 inp=s1edge.img plotout=test24 'nonormal table=test24.tbl
Beginning VICAR task otf1
OTF1 version 2015-08-10
NUMBER OF LINES PROCESSED=         20

 PIXEL  CYCLES/      FREQUENCY    REAL(FT)     IMAG.(FT)     INTENSITY    AMPLITUDE     PHASE     WAVE LENGT
         PIXEL                                                                         RADIANS      SHIFT
  1     0.00000       0.00000      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
  2     0.00391       0.00391      0.99799      0.00000      0.99599      0.99799      0.00000      0.00000
  3     0.00781       0.00781      0.99199     -0.00000      0.98404      0.99199      0.00000      0.00000
  4     0.01172       0.01172      0.98202      0.00000      0.96437      0.98202      0.00000      0.00000
  5     0.01563       0.01563      0.96817      0.00000      0.93736      0.96817      0.00000      0.00000
  6     0.01953       0.01953      0.95054     -0.00000      0.90352      0.95054      0.00000      0.00000
  7     0.02344       0.02344      0.92924      0.00000      0.86348      0.92924      0.00000      0.00000
  8     0.02734       0.02734      0.90443      0.00000      0.81799      0.90443      0.00000      0.00000
  9     0.03125       0.03125      0.87628      0.00000      0.76786      0.87628      0.00000      0.00000
 10     0.03516       0.03516      0.84498     -0.00000      0.71400      0.84498      0.00000      0.00000
 11     0.03906       0.03906      0.81077      0.00000      0.65735      0.81077      0.00000      0.00000
 12     0.04297       0.04297      0.77388      0.00000      0.59888      0.77388      0.00000      0.00000
 13     0.04688       0.04688      0.73455      0.00000      0.53957      0.73455      0.00000      0.00000
 14     0.05078       0.05078      0.69307     -0.00000      0.48035      0.69307      0.00000      0.00000
 15     0.05469       0.05469      0.64972     -0.00000      0.42214      0.64972      0.00000      0.00000
 16     0.05859       0.05859      0.60479     -0.00000      0.36578      0.60479      0.00000      0.00000
 17     0.06250       0.06250      0.55859     -0.00000      0.31203      0.55859      0.00000      0.00000
 18     0.06641       0.06641      0.51143     -0.00000      0.26156      0.51143      0.00000      0.00000
 19     0.07031       0.07031      0.46362     -0.00000      0.21494      0.46362      0.00000      0.00000
 20     0.07422       0.07422      0.41547     -0.00000      0.17261      0.41547      0.00000      0.00000
 21     0.07813       0.07813      0.36729      0.00000      0.13491      0.36729      0.00000      0.00000
 22     0.08203       0.08203      0.31941     -0.00000      0.10202      0.31941      0.00000      0.00000
 23     0.08594       0.08594      0.27211      0.00000      0.07404      0.27211      0.00000      0.00000
 24     0.08984       0.08984      0.22569     -0.00000      0.05094      0.22569      0.00000      0.00000
 25     0.09375       0.09375      0.18043      0.00000      0.03256      0.18043      0.00000      0.00000
 26     0.09766       0.09766      0.13661     -0.00000      0.01866      0.13661      0.00000      0.00000
 27     0.10156       0.10156      0.09447     -0.00000      0.00893      0.09447      0.00000      0.00000
 28     0.10547       0.10547      0.05426     -0.00000      0.00294      0.05426      0.00000      0.00000
 29     0.10938       0.10938      0.01618      0.00000      0.00026      0.01618      0.00000      0.00000
 30     0.11328       0.11328     -0.01956     -0.00000      0.00038      0.01956      3.14159      4.41379
 31     0.11719       0.11719     -0.05278     -0.00000      0.00279      0.05278      3.14159      4.26667
 32     0.12109       0.12109     -0.08334     -0.00000      0.00695      0.08334      3.14159      4.12903
 33     0.12500       0.12500     -0.11111     -0.00000      0.01235      0.11111      3.14159      4.00000
 34     0.12891       0.12891     -0.13598     -0.00000      0.01849      0.13598      3.14159      3.87879
 35     0.13281       0.13281     -0.15788     -0.00000      0.02493      0.15788      3.14159      3.76471
 36     0.13672       0.13672     -0.17674     -0.00000      0.03124      0.17674      3.14159      3.65714
 37     0.14063       0.14063     -0.19256      0.00000      0.03708      0.19256      3.14159      3.55556
 38     0.14453       0.14453     -0.20531     -0.00000      0.04215      0.20531      3.14159      3.45946
 39     0.14844       0.14844     -0.21502      0.00000      0.04623      0.21502      3.14159      3.36842
 40     0.15234       0.15234     -0.22175     -0.00000      0.04917      0.22175      3.14159      3.28205
 41     0.15625       0.15625     -0.22556      0.00000      0.05088      0.22556      3.14159      3.20000
 42     0.16016       0.16016     -0.22654      0.00000      0.05132      0.22654      3.14159      3.12195
 43     0.16406       0.16406     -0.22481      0.00000      0.05054      0.22481      3.14159      3.04762
 44     0.16797       0.16797     -0.22051     -0.00000      0.04863      0.22051      3.14159      2.97674
 45     0.17188       0.17188     -0.21379      0.00000      0.04570      0.21379      3.14159      2.90909
 46     0.17578       0.17578     -0.20481     -0.00000      0.04195      0.20481      3.14159      2.84444
 47     0.17969       0.17969     -0.19377     -0.00000      0.03755      0.19377      3.14159      2.78261
 48     0.18359       0.18359     -0.18086     -0.00000      0.03271      0.18086      3.14159      2.72340
 49     0.18750       0.18750     -0.16629      0.00000      0.02765      0.16629      3.14159      2.66667
 50     0.19141       0.19141     -0.15028     -0.00000      0.02258      0.15028      3.14159      2.61224
 51     0.19531       0.19531     -0.13306      0.00000      0.01770      0.13306      3.14159      2.56000
 52     0.19922       0.19922     -0.11485     -0.00000      0.01319      0.11485      3.14159      2.50980
 53     0.20313       0.20313     -0.09589      0.00000      0.00920      0.09589      3.14159      2.46154
 54     0.20703       0.20703     -0.07641     -0.00000      0.00584      0.07641      3.14159      2.41509
 55     0.21094       0.21094     -0.05665      0.00000      0.00321      0.05665      3.14159      2.37037
 56     0.21484       0.21484     -0.03683      0.00000      0.00136      0.03683      3.14159      2.32727
 57     0.21875       0.21875     -0.01717      0.00000      0.00029      0.01717      3.14159      2.28571
 58     0.22266       0.22266      0.00212      0.00000      0.00000      0.00212      0.00000      0.00000
 59     0.22656       0.22656      0.02082      0.00000      0.00043      0.02082      0.00000      0.00000
 60     0.23047       0.23047      0.03876      0.00000      0.00150      0.03876      0.00000      0.00000
 61     0.23438       0.23438      0.05574      0.00000      0.00311      0.05574      0.00000      0.00000
 62     0.23828       0.23828      0.07161      0.00000      0.00513      0.07161      0.00000      0.00000
 63     0.24219       0.24219      0.08621      0.00000      0.00743      0.08621      0.00000      0.00000
 64     0.24609       0.24609      0.09941      0.00000      0.00988      0.09941      0.00000      0.00000
 65     0.25000       0.25000      0.11111      0.00000      0.01235      0.11111      0.00000      0.00000
 66     0.25391       0.25391      0.12120      0.00000      0.01469      0.12120      0.00000      0.00000
 67     0.25781       0.25781      0.12961      0.00000      0.01680      0.12961      0.00000      0.00000
 68     0.26172       0.26172      0.13629      0.00000      0.01857      0.13629      0.00000      0.00000
 69     0.26563       0.26563      0.14119      0.00000      0.01994      0.14119      0.00000      0.00000
 70     0.26953       0.26953      0.14431     -0.00000      0.02082      0.14431      0.00000      0.00000
 71     0.27344       0.27344      0.14563      0.00000      0.02121      0.14563      0.00000      0.00000
 72     0.27734       0.27734      0.14520      0.00000      0.02108      0.14520      0.00000      0.00000
 73     0.28125       0.28125      0.14305      0.00000      0.02046      0.14305      0.00000      0.00000
 74     0.28516       0.28516      0.13923      0.00000      0.01938      0.13923      0.00000      0.00000
 75     0.28906       0.28906      0.13383      0.00000      0.01791      0.13383      0.00000      0.00000
 76     0.29297       0.29297      0.12693     -0.00000      0.01611      0.12693      0.00000      0.00000
 77     0.29688       0.29688      0.11865     -0.00000      0.01408      0.11865      0.00000      0.00000
 78     0.30078       0.30078      0.10911      0.00000      0.01190      0.10911      0.00000      0.00000
 79     0.30469       0.30469      0.09843      0.00000      0.00969      0.09843      0.00000      0.00000
 80     0.30859       0.30859      0.08675     -0.00000      0.00753      0.08675      0.00000      0.00000
 81     0.31250       0.31250      0.07424     -0.00000      0.00551      0.07424      0.00000      0.00000
 82     0.31641       0.31641      0.06105      0.00000      0.00373      0.06105      0.00000      0.00000
 83     0.32031       0.32031      0.04733     -0.00000      0.00224      0.04733      0.00000      0.00000
 84     0.32422       0.32422      0.03326      0.00000      0.00111      0.03326      0.00000      0.00000
 85     0.32813       0.32813      0.01901      0.00000      0.00036      0.01901      0.00000      0.00000
 86     0.33203       0.33203      0.00473     -0.00000      0.00002      0.00473      0.00000      0.00000
 87     0.33594       0.33594     -0.00939     -0.00000      0.00009      0.00939      3.14159      1.48837
 88     0.33984       0.33984     -0.02321     -0.00000      0.00054      0.02321      3.14159      1.47126
 89     0.34375       0.34375     -0.03657      0.00000      0.00134      0.03657      3.14159      1.45455
 90     0.34766       0.34766     -0.04932      0.00000      0.00243      0.04932      3.14159      1.43820
 91     0.35156       0.35156     -0.06131     -0.00000      0.00376      0.06131      3.14159      1.42222
 92     0.35547       0.35547     -0.07243     -0.00000      0.00525      0.07243      3.14159      1.40659
 93     0.35938       0.35938     -0.08254      0.00000      0.00681      0.08254      3.14159      1.39130
 94     0.36328       0.36328     -0.09155     -0.00000      0.00838      0.09155      3.14159      1.37634
 95     0.36719       0.36719     -0.09937     -0.00000      0.00987      0.09937      3.14159      1.36170
 96     0.37109       0.37109     -0.10591     -0.00000      0.01122      0.10591      3.14159      1.34737
 97     0.37500       0.37500     -0.11111     -0.00000      0.01235      0.11111      3.14159      1.33333
 98     0.37891       0.37891     -0.11493      0.00000      0.01321      0.11493      3.14159      1.31959
 99     0.38281       0.38281     -0.11734     -0.00000      0.01377      0.11734      3.14159      1.30612
100     0.38672       0.38672     -0.11832     -0.00000      0.01400      0.11832      3.14159      1.29293
101     0.39063       0.39063     -0.11787      0.00000      0.01389      0.11787      3.14159      1.28000
102     0.39453       0.39453     -0.11601     -0.00000      0.01346      0.11601      3.14159      1.26733
103     0.39844       0.39844     -0.11278     -0.00000      0.01272      0.11278      3.14159      1.25490
104     0.40234       0.40234     -0.10822     -0.00000      0.01171      0.10822      3.14159      1.24272
105     0.40625       0.40625     -0.10240      0.00000      0.01049      0.10240      3.14159      1.23077
106     0.41016       0.41016     -0.09540      0.00000      0.00910      0.09540      3.14159      1.21905
107     0.41406       0.41406     -0.08730     -0.00000      0.00762      0.08730      3.14159      1.20755
108     0.41797       0.41797     -0.07820     -0.00000      0.00612      0.07820      3.14159      1.19626
109     0.42188       0.42188     -0.06823      0.00000      0.00466      0.06823      3.14159      1.18519
110     0.42578       0.42578     -0.05750     -0.00000      0.00331      0.05750      3.14159      1.17431
111     0.42969       0.42969     -0.04615     -0.00000      0.00213      0.04615      3.14159      1.16364
112     0.43359       0.43359     -0.03430     -0.00000      0.00118      0.03430      3.14159      1.15315
113     0.43750       0.43750     -0.02210      0.00000      0.00049      0.02210      3.14159      1.14286
114     0.44141       0.44141     -0.00970      0.00000      0.00009      0.00970      3.14159      1.13274
115     0.44531       0.44531      0.00277     -0.00000      0.00001      0.00277      0.00000      0.00000
116     0.44922       0.44922      0.01515      0.00000      0.00023      0.01515      0.00000      0.00000
117     0.45313       0.45313      0.02729      0.00000      0.00074      0.02729      0.00000      0.00000
118     0.45703       0.45703      0.03907      0.00000      0.00153      0.03907      0.00000      0.00000
119     0.46094       0.46094      0.05034     -0.00000      0.00253      0.05034      0.00000      0.00000
120     0.46484       0.46484      0.06096     -0.00000      0.00372      0.06096      0.00000      0.00000
121     0.46875       0.46875      0.07083      0.00000      0.00502      0.07083      0.00000      0.00000
122     0.47266       0.47266      0.07982      0.00000      0.00637      0.07982      0.00000      0.00000
123     0.47656       0.47656      0.08783      0.00000      0.00771      0.08783      0.00000      0.00000
124     0.48047       0.48047      0.09477     -0.00000      0.00898      0.09477      0.00000      0.00000
125     0.48438       0.48438      0.10056      0.00000      0.01011      0.10056      0.00000      0.00000
126     0.48828       0.48828      0.10514     -0.00000      0.01105      0.10514      0.00000      0.00000
127     0.49219       0.49219      0.10844     -0.00000      0.01176      0.10844      0.00000      0.00000
128     0.49609       0.49609      0.11044      0.00000      0.01220      0.11044      0.00000      0.00000
129     0.50000       0.50000      0.11111      0.00000      0.01235      0.11111      0.00000      0.00000
NUMBER POINTS OUTPUT =         129
**** This image is ALIASED ****
INTEGRATED MTF AMPLITUDE FROM .25 TO .48 =       4.94845
typetext test24.tbl
Beginning VICAR task typetext
 # FREQUENCY	   AMPLITUDE	   PHASE
    0.00000	   1.00000	   0.00000
    0.00391	   0.99799	   0.00000
    0.00781	   0.99199	   0.00000
    0.01172	   0.98202	   0.00000
    0.01563	   0.96817	   0.00000
    0.01953	   0.95054	   0.00000
    0.02344	   0.92924	   0.00000
    0.02734	   0.90443	   0.00000
    0.03125	   0.87628	   0.00000
    0.03516	   0.84498	   0.00000
    0.03906	   0.81077	   0.00000
    0.04297	   0.77388	   0.00000
    0.04688	   0.73455	   0.00000
    0.05078	   0.69307	   0.00000
    0.05469	   0.64972	   0.00000
    0.05859	   0.60479	   0.00000
    0.06250	   0.55859	   0.00000
    0.06641	   0.51143	   0.00000
    0.07031	   0.46362	   0.00000
    0.07422	   0.41547	   0.00000
    0.07813	   0.36729	   0.00000
    0.08203	   0.31941	   0.00000
    0.08594	   0.27211	   0.00000
    0.08984	   0.22569	   0.00000
    0.09375	   0.18043	   0.00000
    0.09766	   0.13661	   0.00000
    0.10156	   0.09447	   0.00000
    0.10547	   0.05426	   0.00000
    0.10938	   0.01618	   0.00000
    0.11328	   0.01956	   3.14159
    0.11719	   0.05278	   3.14159
    0.12109	   0.08334	   3.14159
    0.12500	   0.11111	   3.14159
    0.12891	   0.13598	   3.14159
    0.13281	   0.15788	   3.14159
    0.13672	   0.17674	   3.14159
    0.14063	   0.19256	   3.14159
    0.14453	   0.20531	   3.14159
    0.14844	   0.21502	   3.14159
    0.15234	   0.22175	   3.14159
    0.15625	   0.22556	   3.14159
    0.16016	   0.22654	   3.14159
    0.16406	   0.22481	   3.14159
    0.16797	   0.22051	   3.14159
    0.17188	   0.21379	   3.14159
    0.17578	   0.20481	   3.14159
    0.17969	   0.19377	   3.14159
    0.18359	   0.18086	   3.14159
    0.18750	   0.16629	   3.14159
    0.19141	   0.15028	   3.14159
    0.19531	   0.13306	   3.14159
    0.19922	   0.11485	   3.14159
    0.20313	   0.09589	   3.14159
    0.20703	   0.07641	   3.14159
    0.21094	   0.05665	   3.14159
    0.21484	   0.03683	   3.14159
    0.21875	   0.01717	   3.14159
    0.22266	   0.00212	   0.00000
    0.22656	   0.02082	   0.00000
    0.23047	   0.03876	   0.00000
    0.23438	   0.05574	   0.00000
    0.23828	   0.07161	   0.00000
    0.24219	   0.08621	   0.00000
    0.24609	   0.09941	   0.00000
    0.25000	   0.11111	   0.00000
    0.25391	   0.12120	   0.00000
    0.25781	   0.12961	   0.00000
    0.26172	   0.13629	   0.00000
    0.26563	   0.14119	   0.00000
    0.26953	   0.14431	   0.00000
    0.27344	   0.14563	   0.00000
    0.27734	   0.14520	   0.00000
    0.28125	   0.14305	   0.00000
    0.28516	   0.13923	   0.00000
    0.28906	   0.13383	   0.00000
    0.29297	   0.12693	   0.00000
    0.29688	   0.11865	   0.00000
    0.30078	   0.10911	   0.00000
    0.30469	   0.09843	   0.00000
    0.30859	   0.08675	   0.00000
    0.31250	   0.07424	   0.00000
    0.31641	   0.06105	   0.00000
    0.32031	   0.04733	   0.00000
    0.32422	   0.03326	   0.00000
    0.32813	   0.01901	   0.00000
    0.33203	   0.00473	   0.00000
    0.33594	   0.00939	   3.14159
    0.33984	   0.02321	   3.14159
    0.34375	   0.03657	   3.14159
    0.34766	   0.04932	   3.14159
    0.35156	   0.06131	   3.14159
    0.35547	   0.07243	   3.14159
    0.35938	   0.08254	   3.14159
    0.36328	   0.09155	   3.14159
    0.36719	   0.09937	   3.14159
    0.37109	   0.10591	   3.14159
    0.37500	   0.11111	   3.14159
    0.37891	   0.11493	   3.14159
    0.38281	   0.11734	   3.14159
    0.38672	   0.11832	   3.14159
    0.39063	   0.11787	   3.14159
    0.39453	   0.11601	   3.14159
    0.39844	   0.11278	   3.14159
    0.40234	   0.10822	   3.14159
    0.40625	   0.10240	   3.14159
    0.41016	   0.09540	   3.14159
    0.41406	   0.08730	   3.14159
    0.41797	   0.07820	   3.14159
    0.42188	   0.06823	   3.14159
    0.42578	   0.05750	   3.14159
    0.42969	   0.04615	   3.14159
    0.43359	   0.03430	   3.14159
    0.43750	   0.02210	   3.14159
    0.44141	   0.00970	   3.14159
    0.44531	   0.00277	   0.00000
    0.44922	   0.01515	   0.00000
    0.45313	   0.02729	   0.00000
    0.45703	   0.03907	   0.00000
    0.46094	   0.05034	   0.00000
    0.46484	   0.06096	   0.00000
    0.46875	   0.07083	   0.00000
    0.47266	   0.07982	   0.00000
    0.47656	   0.08783	   0.00000
    0.48047	   0.09477	   0.00000
    0.48438	   0.10056	   0.00000
    0.48828	   0.10514	   0.00000
    0.49219	   0.10844	   0.00000
    0.49609	   0.11044	   0.00000
    0.50000	   0.11111	   0.00000
if (mode = "nobatch" or mode = "inter")
end-if
otf1 s1edgeN.img plotout=test25 noise=2 'noprint
Beginning VICAR task otf1
OTF1 version 2015-08-10
NUMBER OF BAD PIXELS ON RIGHT=          1
LINE NUMBER=          1
NUMBER OF BAD PIXELS ON RIGHT=          1
LINE NUMBER=          7
NUMBER OF LINES PROCESSED=         20
NUMBER POINTS OUTPUT =         129
**** This image is ALIASED ****
INTEGRATED MTF AMPLITUDE FROM .25 TO .48 =       4.24324
if (mode = "nobatch" or mode = "inter")
end-if
otf1 s1edgeN.img plotout=test26 noise=5 'noprint
Beginning VICAR task otf1
OTF1 version 2015-08-10
NUMBER OF BAD PIXELS ON RIGHT=          1
LINE NUMBER=          1
NUMBER OF BAD PIXELS ON RIGHT=          1
LINE NUMBER=          7
NUMBER OF LINES PROCESSED=         20
NUMBER POINTS OUTPUT =         129
**** This image is ALIASED ****
INTEGRATED MTF AMPLITUDE FROM .25 TO .48 =       4.24324
if (mode = "nobatch" or mode = "inter")
end-if
otf1 inp=s2edge.img plotout=test27 pzoom=4 'noprint
Beginning VICAR task otf1
OTF1 version 2015-08-10
NUMBER OF LINES PROCESSED=         20
NUMBER POINTS OUTPUT =         129
**** This image is ALIASED ****
INTEGRATED MTF AMPLITUDE FROM .25 TO .48 =       1.50719
if (mode = "nobatch" or mode = "inter")
end-if
otf1 inp=s1edge.img plotout=test28 'nonormal interval=0.5 table=test28.tbl
Beginning VICAR task otf1
OTF1 version 2015-08-10
NUMBER OF LINES PROCESSED=         20

 PIXEL  CYCLES/      FREQUENCY    REAL(FT)     IMAG.(FT)     INTENSITY    AMPLITUDE     PHASE     WAVE LENGT
         PIXEL                                                                         RADIANS      SHIFT
  1     0.00000       0.00000      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
  2     0.00391       0.00781      0.99799      0.00000      0.99599      0.99799      0.00000      0.00000
  3     0.00781       0.01563      0.99199     -0.00000      0.98404      0.99199      0.00000      0.00000
  4     0.01172       0.02344      0.98202      0.00000      0.96437      0.98202      0.00000      0.00000
  5     0.01563       0.03125      0.96817      0.00000      0.93736      0.96817      0.00000      0.00000
  6     0.01953       0.03906      0.95054     -0.00000      0.90352      0.95054      0.00000      0.00000
  7     0.02344       0.04688      0.92924      0.00000      0.86348      0.92924      0.00000      0.00000
  8     0.02734       0.05469      0.90443      0.00000      0.81799      0.90443      0.00000      0.00000
  9     0.03125       0.06250      0.87628      0.00000      0.76786      0.87628      0.00000      0.00000
 10     0.03516       0.07031      0.84498     -0.00000      0.71400      0.84498      0.00000      0.00000
 11     0.03906       0.07813      0.81077      0.00000      0.65735      0.81077      0.00000      0.00000
 12     0.04297       0.08594      0.77388      0.00000      0.59888      0.77388      0.00000      0.00000
 13     0.04688       0.09375      0.73455      0.00000      0.53957      0.73455      0.00000      0.00000
 14     0.05078       0.10156      0.69307     -0.00000      0.48035      0.69307      0.00000      0.00000
 15     0.05469       0.10938      0.64972     -0.00000      0.42214      0.64972      0.00000      0.00000
 16     0.05859       0.11719      0.60479     -0.00000      0.36578      0.60479      0.00000      0.00000
 17     0.06250       0.12500      0.55859     -0.00000      0.31203      0.55859      0.00000      0.00000
 18     0.06641       0.13281      0.51143     -0.00000      0.26156      0.51143      0.00000      0.00000
 19     0.07031       0.14063      0.46362     -0.00000      0.21494      0.46362      0.00000      0.00000
 20     0.07422       0.14844      0.41547     -0.00000      0.17261      0.41547      0.00000      0.00000
 21     0.07813       0.15625      0.36729      0.00000      0.13491      0.36729      0.00000      0.00000
 22     0.08203       0.16406      0.31941     -0.00000      0.10202      0.31941      0.00000      0.00000
 23     0.08594       0.17188      0.27211      0.00000      0.07404      0.27211      0.00000      0.00000
 24     0.08984       0.17969      0.22569     -0.00000      0.05094      0.22569      0.00000      0.00000
 25     0.09375       0.18750      0.18043      0.00000      0.03256      0.18043      0.00000      0.00000
 26     0.09766       0.19531      0.13661     -0.00000      0.01866      0.13661      0.00000      0.00000
 27     0.10156       0.20313      0.09447     -0.00000      0.00893      0.09447      0.00000      0.00000
 28     0.10547       0.21094      0.05426     -0.00000      0.00294      0.05426      0.00000      0.00000
 29     0.10938       0.21875      0.01618      0.00000      0.00026      0.01618      0.00000      0.00000
 30     0.11328       0.22656     -0.01956     -0.00000      0.00038      0.01956      3.14159      2.20690
 31     0.11719       0.23438     -0.05278     -0.00000      0.00279      0.05278      3.14159      2.13333
 32     0.12109       0.24219     -0.08334     -0.00000      0.00695      0.08334      3.14159      2.06452
 33     0.12500       0.25000     -0.11111     -0.00000      0.01235      0.11111      3.14159      2.00000
 34     0.12891       0.25781     -0.13598     -0.00000      0.01849      0.13598      3.14159      1.93939
 35     0.13281       0.26563     -0.15788     -0.00000      0.02493      0.15788      3.14159      1.88235
 36     0.13672       0.27344     -0.17674     -0.00000      0.03124      0.17674      3.14159      1.82857
 37     0.14063       0.28125     -0.19256      0.00000      0.03708      0.19256      3.14159      1.77778
 38     0.14453       0.28906     -0.20531     -0.00000      0.04215      0.20531      3.14159      1.72973
 39     0.14844       0.29688     -0.21502      0.00000      0.04623      0.21502      3.14159      1.68421
 40     0.15234       0.30469     -0.22175     -0.00000      0.04917      0.22175      3.14159      1.64103
 41     0.15625       0.31250     -0.22556      0.00000      0.05088      0.22556      3.14159      1.60000
 42     0.16016       0.32031     -0.22654      0.00000      0.05132      0.22654      3.14159      1.56098
 43     0.16406       0.32813     -0.22481      0.00000      0.05054      0.22481      3.14159      1.52381
 44     0.16797       0.33594     -0.22051     -0.00000      0.04863      0.22051      3.14159      1.48837
 45     0.17188       0.34375     -0.21379      0.00000      0.04570      0.21379      3.14159      1.45455
 46     0.17578       0.35156     -0.20481     -0.00000      0.04195      0.20481      3.14159      1.42222
 47     0.17969       0.35938     -0.19377     -0.00000      0.03755      0.19377      3.14159      1.39130
 48     0.18359       0.36719     -0.18086     -0.00000      0.03271      0.18086      3.14159      1.36170
 49     0.18750       0.37500     -0.16629      0.00000      0.02765      0.16629      3.14159      1.33333
 50     0.19141       0.38281     -0.15028     -0.00000      0.02258      0.15028      3.14159      1.30612
 51     0.19531       0.39063     -0.13306      0.00000      0.01770      0.13306      3.14159      1.28000
 52     0.19922       0.39844     -0.11485     -0.00000      0.01319      0.11485      3.14159      1.25490
 53     0.20313       0.40625     -0.09589      0.00000      0.00920      0.09589      3.14159      1.23077
 54     0.20703       0.41406     -0.07641     -0.00000      0.00584      0.07641      3.14159      1.20755
 55     0.21094       0.42188     -0.05665      0.00000      0.00321      0.05665      3.14159      1.18519
 56     0.21484       0.42969     -0.03683      0.00000      0.00136      0.03683      3.14159      1.16364
 57     0.21875       0.43750     -0.01717      0.00000      0.00029      0.01717      3.14159      1.14286
 58     0.22266       0.44531      0.00212      0.00000      0.00000      0.00212      0.00000      0.00000
 59     0.22656       0.45313      0.02082      0.00000      0.00043      0.02082      0.00000      0.00000
 60     0.23047       0.46094      0.03876      0.00000      0.00150      0.03876      0.00000      0.00000
 61     0.23438       0.46875      0.05574      0.00000      0.00311      0.05574      0.00000      0.00000
 62     0.23828       0.47656      0.07161      0.00000      0.00513      0.07161      0.00000      0.00000
 63     0.24219       0.48438      0.08621      0.00000      0.00743      0.08621      0.00000      0.00000
 64     0.24609       0.49219      0.09941      0.00000      0.00988      0.09941      0.00000      0.00000
 65     0.25000       0.50000      0.11111      0.00000      0.01235      0.11111      0.00000      0.00000
 66     0.25391       0.50781      0.12120      0.00000      0.01469      0.12120      0.00000      0.00000
 67     0.25781       0.51563      0.12961      0.00000      0.01680      0.12961      0.00000      0.00000
 68     0.26172       0.52344      0.13629      0.00000      0.01857      0.13629      0.00000      0.00000
 69     0.26563       0.53125      0.14119      0.00000      0.01994      0.14119      0.00000      0.00000
 70     0.26953       0.53906      0.14431     -0.00000      0.02082      0.14431      0.00000      0.00000
 71     0.27344       0.54688      0.14563      0.00000      0.02121      0.14563      0.00000      0.00000
 72     0.27734       0.55469      0.14520      0.00000      0.02108      0.14520      0.00000      0.00000
 73     0.28125       0.56250      0.14305      0.00000      0.02046      0.14305      0.00000      0.00000
 74     0.28516       0.57031      0.13923      0.00000      0.01938      0.13923      0.00000      0.00000
 75     0.28906       0.57813      0.13383      0.00000      0.01791      0.13383      0.00000      0.00000
 76     0.29297       0.58594      0.12693     -0.00000      0.01611      0.12693      0.00000      0.00000
 77     0.29688       0.59375      0.11865     -0.00000      0.01408      0.11865      0.00000      0.00000
 78     0.30078       0.60156      0.10911      0.00000      0.01190      0.10911      0.00000      0.00000
 79     0.30469       0.60938      0.09843      0.00000      0.00969      0.09843      0.00000      0.00000
 80     0.30859       0.61719      0.08675     -0.00000      0.00753      0.08675      0.00000      0.00000
 81     0.31250       0.62500      0.07424     -0.00000      0.00551      0.07424      0.00000      0.00000
 82     0.31641       0.63281      0.06105      0.00000      0.00373      0.06105      0.00000      0.00000
 83     0.32031       0.64063      0.04733     -0.00000      0.00224      0.04733      0.00000      0.00000
 84     0.32422       0.64844      0.03326      0.00000      0.00111      0.03326      0.00000      0.00000
 85     0.32813       0.65625      0.01901      0.00000      0.00036      0.01901      0.00000      0.00000
 86     0.33203       0.66406      0.00473     -0.00000      0.00002      0.00473      0.00000      0.00000
 87     0.33594       0.67188     -0.00939     -0.00000      0.00009      0.00939      3.14159      0.74419
 88     0.33984       0.67969     -0.02321     -0.00000      0.00054      0.02321      3.14159      0.73563
 89     0.34375       0.68750     -0.03657      0.00000      0.00134      0.03657      3.14159      0.72727
 90     0.34766       0.69531     -0.04932      0.00000      0.00243      0.04932      3.14159      0.71910
 91     0.35156       0.70313     -0.06131     -0.00000      0.00376      0.06131      3.14159      0.71111
 92     0.35547       0.71094     -0.07243     -0.00000      0.00525      0.07243      3.14159      0.70330
 93     0.35938       0.71875     -0.08254      0.00000      0.00681      0.08254      3.14159      0.69565
 94     0.36328       0.72656     -0.09155     -0.00000      0.00838      0.09155      3.14159      0.68817
 95     0.36719       0.73438     -0.09937     -0.00000      0.00987      0.09937      3.14159      0.68085
 96     0.37109       0.74219     -0.10591     -0.00000      0.01122      0.10591      3.14159      0.67368
 97     0.37500       0.75000     -0.11111     -0.00000      0.01235      0.11111      3.14159      0.66667
 98     0.37891       0.75781     -0.11493      0.00000      0.01321      0.11493      3.14159      0.65979
 99     0.38281       0.76563     -0.11734     -0.00000      0.01377      0.11734      3.14159      0.65306
100     0.38672       0.77344     -0.11832     -0.00000      0.01400      0.11832      3.14159      0.64646
101     0.39063       0.78125     -0.11787      0.00000      0.01389      0.11787      3.14159      0.64000
102     0.39453       0.78906     -0.11601     -0.00000      0.01346      0.11601      3.14159      0.63366
103     0.39844       0.79688     -0.11278     -0.00000      0.01272      0.11278      3.14159      0.62745
104     0.40234       0.80469     -0.10822     -0.00000      0.01171      0.10822      3.14159      0.62136
105     0.40625       0.81250     -0.10240      0.00000      0.01049      0.10240      3.14159      0.61538
106     0.41016       0.82031     -0.09540      0.00000      0.00910      0.09540      3.14159      0.60952
107     0.41406       0.82813     -0.08730     -0.00000      0.00762      0.08730      3.14159      0.60377
108     0.41797       0.83594     -0.07820     -0.00000      0.00612      0.07820      3.14159      0.59813
109     0.42188       0.84375     -0.06823      0.00000      0.00466      0.06823      3.14159      0.59259
110     0.42578       0.85156     -0.05750     -0.00000      0.00331      0.05750      3.14159      0.58716
111     0.42969       0.85938     -0.04615     -0.00000      0.00213      0.04615      3.14159      0.58182
112     0.43359       0.86719     -0.03430     -0.00000      0.00118      0.03430      3.14159      0.57658
113     0.43750       0.87500     -0.02210      0.00000      0.00049      0.02210      3.14159      0.57143
114     0.44141       0.88281     -0.00970      0.00000      0.00009      0.00970      3.14159      0.56637
115     0.44531       0.89063      0.00277     -0.00000      0.00001      0.00277      0.00000      0.00000
116     0.44922       0.89844      0.01515      0.00000      0.00023      0.01515      0.00000      0.00000
117     0.45313       0.90625      0.02729      0.00000      0.00074      0.02729      0.00000      0.00000
118     0.45703       0.91406      0.03907      0.00000      0.00153      0.03907      0.00000      0.00000
119     0.46094       0.92188      0.05034     -0.00000      0.00253      0.05034      0.00000      0.00000
120     0.46484       0.92969      0.06096     -0.00000      0.00372      0.06096      0.00000      0.00000
121     0.46875       0.93750      0.07083      0.00000      0.00502      0.07083      0.00000      0.00000
122     0.47266       0.94531      0.07982      0.00000      0.00637      0.07982      0.00000      0.00000
123     0.47656       0.95313      0.08783      0.00000      0.00771      0.08783      0.00000      0.00000
124     0.48047       0.96094      0.09477     -0.00000      0.00898      0.09477      0.00000      0.00000
125     0.48438       0.96875      0.10056      0.00000      0.01011      0.10056      0.00000      0.00000
126     0.48828       0.97656      0.10514     -0.00000      0.01105      0.10514      0.00000      0.00000
127     0.49219       0.98438      0.10844     -0.00000      0.01176      0.10844      0.00000      0.00000
128     0.49609       0.99219      0.11044      0.00000      0.01220      0.11044      0.00000      0.00000
129     0.50000       1.00000      0.11111      0.00000      0.01235      0.11111      0.00000      0.00000
NUMBER POINTS OUTPUT =         129
**** This image is ALIASED ****
INTEGRATED MTF AMPLITUDE FROM .25 TO .48 =       4.94845
if (mode = "nobatch" or mode = "inter")
end-if
otf1 inp=s1edge.img plotout=yes plotprof=yes
Beginning VICAR task otf1
OTF1 version 2015-08-10
NUMBER OF LINES PROCESSED=         20

 PIXEL  CYCLES/      FREQUENCY    REAL(FT)     IMAG.(FT)     INTENSITY    AMPLITUDE     PHASE     WAVE LENGT
         PIXEL                                                                         RADIANS      SHIFT
  1     0.00000       0.00000      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
  2     0.00391       0.00391      0.99799     -0.00000      0.99599      0.99799      0.00000      0.00000
  3     0.00781       0.00781      0.99199      0.00000      0.98404      0.99199      0.00000      0.00000
  4     0.01172       0.01172      0.98202      0.00000      0.96437      0.98202      0.00000      0.00000
  5     0.01563       0.01563      0.96818      0.00000      0.93736      0.96818      0.00000      0.00000
  6     0.01953       0.01953      0.95054      0.00000      0.90352      0.95054      0.00000      0.00000
  7     0.02344       0.02344      0.92924     -0.00000      0.86348      0.92924      0.00000      0.00000
  8     0.02734       0.02734      0.90443      0.00000      0.81799      0.90443      0.00000      0.00000
  9     0.03125       0.03125      0.87628      0.00000      0.76786      0.87628      0.00000      0.00000
 10     0.03516       0.03516      0.84498     -0.00000      0.71400      0.84498      0.00000      0.00000
 11     0.03906       0.03906      0.81077      0.00000      0.65735      0.81077      0.00000      0.00000
 12     0.04297       0.04297      0.77388      0.00000      0.59888      0.77388      0.00000      0.00000
 13     0.04688       0.04688      0.73455      0.00000      0.53957      0.73455      0.00000      0.00000
 14     0.05078       0.05078      0.69307     -0.00000      0.48035      0.69307      0.00000      0.00000
 15     0.05469       0.05469      0.64972      0.00000      0.42214      0.64972      0.00000      0.00000
 16     0.05859       0.05859      0.60479     -0.00000      0.36578      0.60479      0.00000      0.00000
 17     0.06250       0.06250      0.55859      0.00000      0.31203      0.55859      0.00000      0.00000
 18     0.06641       0.06641      0.51143     -0.00000      0.26156      0.51143      0.00000      0.00000
 19     0.07031       0.07031      0.46362     -0.00000      0.21494      0.46362      0.00000      0.00000
 20     0.07422       0.07422      0.41547      0.00000      0.17261      0.41547      0.00000      0.00000
 21     0.07813       0.07813      0.36729      0.00000      0.13491      0.36729      0.00000      0.00000
 22     0.08203       0.08203      0.31941     -0.00000      0.10202      0.31941      0.00000      0.00000
 23     0.08594       0.08594      0.27211     -0.00000      0.07404      0.27211      0.00000      0.00000
 24     0.08984       0.08984      0.22569     -0.00000      0.05094      0.22569      0.00000      0.00000
 25     0.09375       0.09375      0.18043      0.00000      0.03256      0.18043      0.00000      0.00000
 26     0.09766       0.09766      0.13661     -0.00000      0.01866      0.13661      0.00000      0.00000
 27     0.10156       0.10156      0.09447      0.00000      0.00893      0.09447      0.00000      0.00000
 28     0.10547       0.10547      0.05426     -0.00000      0.00294      0.05426      0.00000      0.00000
 29     0.10938       0.10938      0.01618      0.00000      0.00026      0.01618      0.00000      0.00000
 30     0.11328       0.11328     -0.01956     -0.00000      0.00038      0.01956      3.14159      4.41379
 31     0.11719       0.11719     -0.05278      0.00000      0.00279      0.05278      3.14159      4.26667
 32     0.12109       0.12109     -0.08334     -0.00000      0.00695      0.08334      3.14159      4.12903
 33     0.12500       0.12500     -0.11111      0.00000      0.01235      0.11111      3.14159      4.00000
 34     0.12891       0.12891     -0.13598     -0.00000      0.01849      0.13598      3.14159      3.87879
 35     0.13281       0.13281     -0.15788     -0.00000      0.02493      0.15788      3.14159      3.76471
 36     0.13672       0.13672     -0.17674     -0.00000      0.03124      0.17674      3.14159      3.65714
 37     0.14063       0.14063     -0.19256      0.00000      0.03708      0.19256      3.14159      3.55556
 38     0.14453       0.14453     -0.20531     -0.00000      0.04215      0.20531      3.14159      3.45946
 39     0.14844       0.14844     -0.21502      0.00000      0.04623      0.21502      3.14159      3.36842
 40     0.15234       0.15234     -0.22175     -0.00000      0.04917      0.22175      3.14159      3.28205
 41     0.15625       0.15625     -0.22556      0.00000      0.05088      0.22556      3.14159      3.20000
 42     0.16016       0.16016     -0.22654     -0.00000      0.05132      0.22654      3.14159      3.12195
 43     0.16406       0.16406     -0.22481      0.00000      0.05054      0.22481      3.14159      3.04762
 44     0.16797       0.16797     -0.22051     -0.00000      0.04863      0.22051      3.14159      2.97674
 45     0.17188       0.17188     -0.21379      0.00000      0.04570      0.21379      3.14159      2.90909
 46     0.17578       0.17578     -0.20481      0.00000      0.04195      0.20481      3.14159      2.84444
 47     0.17969       0.17969     -0.19377      0.00000      0.03755      0.19377      3.14159      2.78261
 48     0.18359       0.18359     -0.18086     -0.00000      0.03271      0.18086      3.14159      2.72340
 49     0.18750       0.18750     -0.16629      0.00000      0.02765      0.16629      3.14159      2.66667
 50     0.19141       0.19141     -0.15028     -0.00000      0.02258      0.15028      3.14159      2.61224
 51     0.19531       0.19531     -0.13306      0.00000      0.01770      0.13306      3.14159      2.56000
 52     0.19922       0.19922     -0.11485     -0.00000      0.01319      0.11485      3.14159      2.50980
 53     0.20313       0.20313     -0.09589      0.00000      0.00920      0.09589      3.14159      2.46154
 54     0.20703       0.20703     -0.07641      0.00000      0.00584      0.07641      3.14159      2.41509
 55     0.21094       0.21094     -0.05665      0.00000      0.00321      0.05665      3.14159      2.37037
 56     0.21484       0.21484     -0.03683      0.00000      0.00136      0.03683      3.14159      2.32727
 57     0.21875       0.21875     -0.01717      0.00000      0.00029      0.01717      3.14159      2.28571
 58     0.22266       0.22266      0.00212     -0.00000      0.00000      0.00212      0.00000      0.00000
 59     0.22656       0.22656      0.02082      0.00000      0.00043      0.02082      0.00000      0.00000
 60     0.23047       0.23047      0.03876     -0.00000      0.00150      0.03876      0.00000      0.00000
 61     0.23438       0.23438      0.05574      0.00000      0.00311      0.05574      0.00000      0.00000
 62     0.23828       0.23828      0.07161      0.00000      0.00513      0.07161      0.00000      0.00000
 63     0.24219       0.24219      0.08621      0.00000      0.00743      0.08621      0.00000      0.00000
 64     0.24609       0.24609      0.09941      0.00000      0.00988      0.09941      0.00000      0.00000
 65     0.25000       0.25000      0.11111      0.00000      0.01235      0.11111      0.00000      0.00000
 66     0.25391       0.25391      0.12120     -0.00000      0.01469      0.12120      0.00000      0.00000
 67     0.25781       0.25781      0.12961      0.00000      0.01680      0.12961      0.00000      0.00000
 68     0.26172       0.26172      0.13629      0.00000      0.01857      0.13629      0.00000      0.00000
 69     0.26563       0.26563      0.14119      0.00000      0.01994      0.14119      0.00000      0.00000
 70     0.26953       0.26953      0.14431      0.00000      0.02082      0.14431      0.00000      0.00000
 71     0.27344       0.27344      0.14563      0.00000      0.02121      0.14563      0.00000      0.00000
 72     0.27734       0.27734      0.14520      0.00000      0.02108      0.14520      0.00000      0.00000
 73     0.28125       0.28125      0.14305      0.00000      0.02046      0.14305      0.00000      0.00000
 74     0.28516       0.28516      0.13923      0.00000      0.01938      0.13923      0.00000      0.00000
 75     0.28906       0.28906      0.13383     -0.00000      0.01791      0.13383      0.00000      0.00000
 76     0.29297       0.29297      0.12693      0.00000      0.01611      0.12693      0.00000      0.00000
 77     0.29688       0.29688      0.11865      0.00000      0.01408      0.11865      0.00000      0.00000
 78     0.30078       0.30078      0.10911     -0.00000      0.01190      0.10911      0.00000      0.00000
 79     0.30469       0.30469      0.09843     -0.00000      0.00969      0.09843      0.00000      0.00000
 80     0.30859       0.30859      0.08675     -0.00000      0.00753      0.08675      0.00000      0.00000
 81     0.31250       0.31250      0.07424      0.00000      0.00551      0.07424      0.00000      0.00000
 82     0.31641       0.31641      0.06105     -0.00000      0.00373      0.06105      0.00000      0.00000
 83     0.32031       0.32031      0.04733     -0.00000      0.00224      0.04733      0.00000      0.00000
 84     0.32422       0.32422      0.03326     -0.00000      0.00111      0.03326      0.00000      0.00000
 85     0.32813       0.32813      0.01901      0.00000      0.00036      0.01901      0.00000      0.00000
 86     0.33203       0.33203      0.00473     -0.00000      0.00002      0.00473      0.00000      0.00000
 87     0.33594       0.33594     -0.00939     -0.00000      0.00009      0.00939      3.14159      1.48837
 88     0.33984       0.33984     -0.02321     -0.00000      0.00054      0.02321      3.14159      1.47126
 89     0.34375       0.34375     -0.03657      0.00000      0.00134      0.03657      3.14159      1.45455
 90     0.34766       0.34766     -0.04932     -0.00000      0.00243      0.04932      3.14159      1.43820
 91     0.35156       0.35156     -0.06131     -0.00000      0.00376      0.06131      3.14159      1.42222
 92     0.35547       0.35547     -0.07243     -0.00000      0.00525      0.07243      3.14159      1.40659
 93     0.35938       0.35938     -0.08254      0.00000      0.00681      0.08254      3.14159      1.39130
 94     0.36328       0.36328     -0.09155     -0.00000      0.00838      0.09155      3.14159      1.37634
 95     0.36719       0.36719     -0.09937     -0.00000      0.00987      0.09937      3.14159      1.36170
 96     0.37109       0.37109     -0.10591     -0.00000      0.01122      0.10591      3.14159      1.34737
 97     0.37500       0.37500     -0.11111      0.00000      0.01235      0.11111      3.14159      1.33333
 98     0.37891       0.37891     -0.11493      0.00000      0.01321      0.11493      3.14159      1.31959
 99     0.38281       0.38281     -0.11734     -0.00000      0.01377      0.11734      3.14159      1.30612
100     0.38672       0.38672     -0.11832     -0.00000      0.01400      0.11832      3.14159      1.29293
101     0.39063       0.39063     -0.11787      0.00000      0.01389      0.11787      3.14159      1.28000
102     0.39453       0.39453     -0.11601     -0.00000      0.01346      0.11601      3.14159      1.26733
103     0.39844       0.39844     -0.11278     -0.00000      0.01272      0.11278      3.14159      1.25490
104     0.40234       0.40234     -0.10822     -0.00000      0.01171      0.10822      3.14159      1.24272
105     0.40625       0.40625     -0.10240      0.00000      0.01049      0.10240      3.14159      1.23077
106     0.41016       0.41016     -0.09540      0.00000      0.00910      0.09540      3.14159      1.21905
107     0.41406       0.41406     -0.08730      0.00000      0.00762      0.08730      3.14159      1.20755
108     0.41797       0.41797     -0.07820     -0.00000      0.00612      0.07820      3.14159      1.19626
109     0.42188       0.42188     -0.06823      0.00000      0.00466      0.06823      3.14159      1.18519
110     0.42578       0.42578     -0.05750      0.00000      0.00331      0.05750      3.14159      1.17431
111     0.42969       0.42969     -0.04615     -0.00000      0.00213      0.04615      3.14159      1.16364
112     0.43359       0.43359     -0.03430     -0.00000      0.00118      0.03430      3.14159      1.15315
113     0.43750       0.43750     -0.02210      0.00000      0.00049      0.02210      3.14159      1.14286
114     0.44141       0.44141     -0.00970      0.00000      0.00009      0.00970      3.14159      1.13274
115     0.44531       0.44531      0.00277      0.00000      0.00001      0.00277      0.00000      0.00000
116     0.44922       0.44922      0.01515     -0.00000      0.00023      0.01515      0.00000      0.00000
117     0.45313       0.45313      0.02729      0.00000      0.00074      0.02729      0.00000      0.00000
118     0.45703       0.45703      0.03907     -0.00000      0.00153      0.03907      0.00000      0.00000
119     0.46094       0.46094      0.05034      0.00000      0.00253      0.05034      0.00000      0.00000
120     0.46484       0.46484      0.06096      0.00000      0.00372      0.06096      0.00000      0.00000
121     0.46875       0.46875      0.07083      0.00000      0.00502      0.07083      0.00000      0.00000
122     0.47266       0.47266      0.07982      0.00000      0.00637      0.07982      0.00000      0.00000
123     0.47656       0.47656      0.08783     -0.00000      0.00771      0.08783      0.00000      0.00000
124     0.48047       0.48047      0.09477      0.00000      0.00898      0.09477      0.00000      0.00000
125     0.48438       0.48438      0.10056      0.00000      0.01011      0.10056      0.00000      0.00000
126     0.48828       0.48828      0.10514      0.00000      0.01105      0.10514      0.00000      0.00000
127     0.49219       0.49219      0.10844     -0.00000      0.01176      0.10844      0.00000      0.00000
128     0.49609       0.49609      0.11044      0.00000      0.01220      0.11044      0.00000      0.00000
129     0.50000       0.50000      0.11111      0.00000      0.01235      0.11111      0.00000      0.00000
NUMBER POINTS OUTPUT =         129
**** This image is ALIASED ****
INTEGRATED MTF AMPLITUDE FROM .25 TO .48 =       4.94845
if (mode = "nobatch" or mode = "inter")
end-if
otf1 inp=a.img plotout=test30 plotprof=test30
Beginning VICAR task otf1
OTF1 version 2015-08-10
NUMBER OF LINES PROCESSED=          1

 PIXEL  CYCLES/      FREQUENCY    REAL(FT)     IMAG.(FT)     INTENSITY    AMPLITUDE     PHASE     WAVE LENGT
         PIXEL                                                                         RADIANS      SHIFT
  1     0.00000       0.00000      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
  2     0.00391       0.00391      0.99799     -0.00000      0.99599      0.99799      0.00000      0.00000
  3     0.00781       0.00781      0.99199      0.00000      0.98404      0.99199      0.00000      0.00000
  4     0.01172       0.01172      0.98202      0.00000      0.96437      0.98202      0.00000      0.00000
  5     0.01563       0.01563      0.96817      0.00000      0.93736      0.96817      0.00000      0.00000
  6     0.01953       0.01953      0.95054      0.00000      0.90352      0.95054      0.00000      0.00000
  7     0.02344       0.02344      0.92924     -0.00000      0.86348      0.92924      0.00000      0.00000
  8     0.02734       0.02734      0.90443      0.00000      0.81799      0.90443      0.00000      0.00000
  9     0.03125       0.03125      0.87628      0.00000      0.76786      0.87628      0.00000      0.00000
 10     0.03516       0.03516      0.84498     -0.00000      0.71400      0.84498      0.00000      0.00000
 11     0.03906       0.03906      0.81077      0.00000      0.65735      0.81077      0.00000      0.00000
 12     0.04297       0.04297      0.77388      0.00000      0.59888      0.77388      0.00000      0.00000
 13     0.04688       0.04688      0.73455      0.00000      0.53957      0.73455      0.00000      0.00000
 14     0.05078       0.05078      0.69307     -0.00000      0.48035      0.69307      0.00000      0.00000
 15     0.05469       0.05469      0.64972      0.00000      0.42214      0.64972      0.00000      0.00000
 16     0.05859       0.05859      0.60479     -0.00000      0.36578      0.60479      0.00000      0.00000
 17     0.06250       0.06250      0.55859      0.00000      0.31203      0.55859      0.00000      0.00000
 18     0.06641       0.06641      0.51143     -0.00000      0.26156      0.51143      0.00000      0.00000
 19     0.07031       0.07031      0.46362     -0.00000      0.21494      0.46362      0.00000      0.00000
 20     0.07422       0.07422      0.41547      0.00000      0.17261      0.41547      0.00000      0.00000
 21     0.07813       0.07813      0.36729      0.00000      0.13491      0.36729      0.00000      0.00000
 22     0.08203       0.08203      0.31941     -0.00000      0.10202      0.31941      0.00000      0.00000
 23     0.08594       0.08594      0.27211     -0.00000      0.07404      0.27211      0.00000      0.00000
 24     0.08984       0.08984      0.22569     -0.00000      0.05094      0.22569      0.00000      0.00000
 25     0.09375       0.09375      0.18043      0.00000      0.03256      0.18043      0.00000      0.00000
 26     0.09766       0.09766      0.13661     -0.00000      0.01866      0.13661      0.00000      0.00000
 27     0.10156       0.10156      0.09447      0.00000      0.00893      0.09447      0.00000      0.00000
 28     0.10547       0.10547      0.05426     -0.00000      0.00294      0.05426      0.00000      0.00000
 29     0.10938       0.10938      0.01618      0.00000      0.00026      0.01618      0.00000      0.00000
 30     0.11328       0.11328     -0.01956     -0.00000      0.00038      0.01956      3.14159      4.41379
 31     0.11719       0.11719     -0.05278      0.00000      0.00279      0.05278      3.14159      4.26667
 32     0.12109       0.12109     -0.08334     -0.00000      0.00695      0.08334      3.14159      4.12903
 33     0.12500       0.12500     -0.11111      0.00000      0.01235      0.11111      3.14159      4.00000
 34     0.12891       0.12891     -0.13598     -0.00000      0.01849      0.13598      3.14159      3.87879
 35     0.13281       0.13281     -0.15788     -0.00000      0.02493      0.15788      3.14159      3.76471
 36     0.13672       0.13672     -0.17674     -0.00000      0.03124      0.17674      3.14159      3.65714
 37     0.14063       0.14063     -0.19256      0.00000      0.03708      0.19256      3.14159      3.55556
 38     0.14453       0.14453     -0.20531     -0.00000      0.04215      0.20531      3.14159      3.45946
 39     0.14844       0.14844     -0.21502      0.00000      0.04623      0.21502      3.14159      3.36842
 40     0.15234       0.15234     -0.22175     -0.00000      0.04917      0.22175      3.14159      3.28205
 41     0.15625       0.15625     -0.22556      0.00000      0.05088      0.22556      3.14159      3.20000
 42     0.16016       0.16016     -0.22654     -0.00000      0.05132      0.22654      3.14159      3.12195
 43     0.16406       0.16406     -0.22481      0.00000      0.05054      0.22481      3.14159      3.04762
 44     0.16797       0.16797     -0.22051     -0.00000      0.04863      0.22051      3.14159      2.97674
 45     0.17188       0.17188     -0.21379      0.00000      0.04570      0.21379      3.14159      2.90909
 46     0.17578       0.17578     -0.20481      0.00000      0.04195      0.20481      3.14159      2.84444
 47     0.17969       0.17969     -0.19377      0.00000      0.03755      0.19377      3.14159      2.78261
 48     0.18359       0.18359     -0.18086     -0.00000      0.03271      0.18086      3.14159      2.72340
 49     0.18750       0.18750     -0.16629      0.00000      0.02765      0.16629      3.14159      2.66667
 50     0.19141       0.19141     -0.15028     -0.00000      0.02258      0.15028      3.14159      2.61224
 51     0.19531       0.19531     -0.13306      0.00000      0.01770      0.13306      3.14159      2.56000
 52     0.19922       0.19922     -0.11485     -0.00000      0.01319      0.11485      3.14159      2.50980
 53     0.20313       0.20313     -0.09589      0.00000      0.00920      0.09589      3.14159      2.46154
 54     0.20703       0.20703     -0.07641      0.00000      0.00584      0.07641      3.14159      2.41509
 55     0.21094       0.21094     -0.05665      0.00000      0.00321      0.05665      3.14159      2.37037
 56     0.21484       0.21484     -0.03683      0.00000      0.00136      0.03683      3.14159      2.32727
 57     0.21875       0.21875     -0.01717      0.00000      0.00029      0.01717      3.14159      2.28571
 58     0.22266       0.22266      0.00212     -0.00000      0.00000      0.00212      0.00000      0.00000
 59     0.22656       0.22656      0.02082      0.00000      0.00043      0.02082      0.00000      0.00000
 60     0.23047       0.23047      0.03876     -0.00000      0.00150      0.03876      0.00000      0.00000
 61     0.23438       0.23438      0.05574      0.00000      0.00311      0.05574      0.00000      0.00000
 62     0.23828       0.23828      0.07161      0.00000      0.00513      0.07161      0.00000      0.00000
 63     0.24219       0.24219      0.08621      0.00000      0.00743      0.08621      0.00000      0.00000
 64     0.24609       0.24609      0.09941      0.00000      0.00988      0.09941      0.00000      0.00000
 65     0.25000       0.25000      0.11111      0.00000      0.01235      0.11111      0.00000      0.00000
 66     0.25391       0.25391      0.12120     -0.00000      0.01469      0.12120      0.00000      0.00000
 67     0.25781       0.25781      0.12961      0.00000      0.01680      0.12961      0.00000      0.00000
 68     0.26172       0.26172      0.13629      0.00000      0.01857      0.13629      0.00000      0.00000
 69     0.26563       0.26563      0.14119      0.00000      0.01994      0.14119      0.00000      0.00000
 70     0.26953       0.26953      0.14431      0.00000      0.02082      0.14431      0.00000      0.00000
 71     0.27344       0.27344      0.14563      0.00000      0.02121      0.14563      0.00000      0.00000
 72     0.27734       0.27734      0.14520      0.00000      0.02108      0.14520      0.00000      0.00000
 73     0.28125       0.28125      0.14305      0.00000      0.02046      0.14305      0.00000      0.00000
 74     0.28516       0.28516      0.13923      0.00000      0.01938      0.13923      0.00000      0.00000
 75     0.28906       0.28906      0.13383     -0.00000      0.01791      0.13383      0.00000      0.00000
 76     0.29297       0.29297      0.12693      0.00000      0.01611      0.12693      0.00000      0.00000
 77     0.29688       0.29688      0.11865      0.00000      0.01408      0.11865      0.00000      0.00000
 78     0.30078       0.30078      0.10911     -0.00000      0.01190      0.10911      0.00000      0.00000
 79     0.30469       0.30469      0.09843     -0.00000      0.00969      0.09843      0.00000      0.00000
 80     0.30859       0.30859      0.08675     -0.00000      0.00753      0.08675      0.00000      0.00000
 81     0.31250       0.31250      0.07424      0.00000      0.00551      0.07424      0.00000      0.00000
 82     0.31641       0.31641      0.06105     -0.00000      0.00373      0.06105      0.00000      0.00000
 83     0.32031       0.32031      0.04733     -0.00000      0.00224      0.04733      0.00000      0.00000
 84     0.32422       0.32422      0.03326     -0.00000      0.00111      0.03326      0.00000      0.00000
 85     0.32813       0.32813      0.01901      0.00000      0.00036      0.01901      0.00000      0.00000
 86     0.33203       0.33203      0.00473     -0.00000      0.00002      0.00473      0.00000      0.00000
 87     0.33594       0.33594     -0.00939     -0.00000      0.00009      0.00939      3.14159      1.48837
 88     0.33984       0.33984     -0.02321     -0.00000      0.00054      0.02321      3.14159      1.47126
 89     0.34375       0.34375     -0.03657      0.00000      0.00134      0.03657      3.14159      1.45455
 90     0.34766       0.34766     -0.04932     -0.00000      0.00243      0.04932      3.14159      1.43820
 91     0.35156       0.35156     -0.06131     -0.00000      0.00376      0.06131      3.14159      1.42222
 92     0.35547       0.35547     -0.07243     -0.00000      0.00525      0.07243      3.14159      1.40659
 93     0.35938       0.35938     -0.08254      0.00000      0.00681      0.08254      3.14159      1.39130
 94     0.36328       0.36328     -0.09155     -0.00000      0.00838      0.09155      3.14159      1.37634
 95     0.36719       0.36719     -0.09937     -0.00000      0.00987      0.09937      3.14159      1.36170
 96     0.37109       0.37109     -0.10591     -0.00000      0.01122      0.10591      3.14159      1.34737
 97     0.37500       0.37500     -0.11111      0.00000      0.01235      0.11111      3.14159      1.33333
 98     0.37891       0.37891     -0.11493      0.00000      0.01321      0.11493      3.14159      1.31959
 99     0.38281       0.38281     -0.11734     -0.00000      0.01377      0.11734      3.14159      1.30612
100     0.38672       0.38672     -0.11832     -0.00000      0.01400      0.11832      3.14159      1.29293
101     0.39063       0.39063     -0.11787      0.00000      0.01389      0.11787      3.14159      1.28000
102     0.39453       0.39453     -0.11601     -0.00000      0.01346      0.11601      3.14159      1.26733
103     0.39844       0.39844     -0.11278     -0.00000      0.01272      0.11278      3.14159      1.25490
104     0.40234       0.40234     -0.10822     -0.00000      0.01171      0.10822      3.14159      1.24272
105     0.40625       0.40625     -0.10240      0.00000      0.01049      0.10240      3.14159      1.23077
106     0.41016       0.41016     -0.09540      0.00000      0.00910      0.09540      3.14159      1.21905
107     0.41406       0.41406     -0.08730      0.00000      0.00762      0.08730      3.14159      1.20755
108     0.41797       0.41797     -0.07820     -0.00000      0.00612      0.07820      3.14159      1.19626
109     0.42188       0.42188     -0.06823      0.00000      0.00466      0.06823      3.14159      1.18519
110     0.42578       0.42578     -0.05750      0.00000      0.00331      0.05750      3.14159      1.17431
111     0.42969       0.42969     -0.04615     -0.00000      0.00213      0.04615      3.14159      1.16364
112     0.43359       0.43359     -0.03430     -0.00000      0.00118      0.03430      3.14159      1.15315
113     0.43750       0.43750     -0.02210      0.00000      0.00049      0.02210      3.14159      1.14286
114     0.44141       0.44141     -0.00970      0.00000      0.00009      0.00970      3.14159      1.13274
115     0.44531       0.44531      0.00277      0.00000      0.00001      0.00277      0.00000      0.00000
116     0.44922       0.44922      0.01515     -0.00000      0.00023      0.01515      0.00000      0.00000
117     0.45313       0.45313      0.02729      0.00000      0.00074      0.02729      0.00000      0.00000
118     0.45703       0.45703      0.03907     -0.00000      0.00153      0.03907      0.00000      0.00000
119     0.46094       0.46094      0.05034      0.00000      0.00253      0.05034      0.00000      0.00000
120     0.46484       0.46484      0.06096      0.00000      0.00372      0.06096      0.00000      0.00000
121     0.46875       0.46875      0.07083      0.00000      0.00502      0.07083      0.00000      0.00000
122     0.47266       0.47266      0.07982      0.00000      0.00637      0.07982      0.00000      0.00000
123     0.47656       0.47656      0.08783     -0.00000      0.00771      0.08783      0.00000      0.00000
124     0.48047       0.48047      0.09477      0.00000      0.00898      0.09477      0.00000      0.00000
125     0.48438       0.48438      0.10056      0.00000      0.01011      0.10056      0.00000      0.00000
126     0.48828       0.48828      0.10514      0.00000      0.01105      0.10514      0.00000      0.00000
127     0.49219       0.49219      0.10844     -0.00000      0.01176      0.10844      0.00000      0.00000
128     0.49609       0.49609      0.11044      0.00000      0.01220      0.11044      0.00000      0.00000
129     0.50000       0.50000      0.11111      0.00000      0.01235      0.11111      0.00000      0.00000
NUMBER POINTS OUTPUT =         129
**** This image is ALIASED ****
INTEGRATED MTF AMPLITUDE FROM .25 TO .48 =       4.94845
if (mode = "nobatch" or mode = "inter")
end-if
otf1 inp=s2edge.img gsd=30. altitude=400000 psfpix=psfpix  +
    pixrad=pixrad pixdeg=pixdeg psfrad=psfrad psfdeg=psfdeg  +
    norangle=22.0 plotprof=test31
Beginning VICAR task otf1
OTF1 version 2015-08-10
NUMBER OF LINES PROCESSED=         20

 PIXEL  CYCLES/      FREQUENCY    REAL(FT)     IMAG.(FT)     INTENSITY    AMPLITUDE     PHASE     WAVE LENGT
         PIXEL                                                                         RADIANS      SHIFT
  1     0.00000       0.00000      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
  2     0.00391       0.00391      0.99698     -0.00000      0.99397      0.99698      0.00000      0.00000
  3     0.00781       0.00781      0.98793      0.00000      0.97601      0.98793      0.00000      0.00000
  4     0.01172       0.01172      0.97300      0.00000      0.94674      0.97300      0.00000      0.00000
  5     0.01563       0.01563      0.95236      0.00001      0.90700      0.95236      0.00000      0.00000
  6     0.01953       0.01953      0.92631      0.00001      0.85804      0.92631      0.00000      0.00000
  7     0.02344       0.02344      0.89515      0.00002      0.80129      0.89515      0.00000      0.00000
  8     0.02734       0.02734      0.85931      0.00003      0.73841      0.85931      0.00000      0.00000
  9     0.03125       0.03125      0.81923      0.00005      0.67114      0.81923      0.00000      0.00000
 10     0.03516       0.03516      0.77545      0.00007      0.60132      0.77545      0.00000      0.00000
 11     0.03906       0.03906      0.72848      0.00010      0.53069      0.72848      0.00000      0.00000
 12     0.04297       0.04297      0.67894      0.00012      0.46097      0.67894      0.00000      0.00000
 13     0.04688       0.04688      0.62742      0.00017      0.39365      0.62742      0.00000      0.00000
 14     0.05078       0.05078      0.57454      0.00021      0.33009      0.57454      0.00000      0.00000
 15     0.05469       0.05469      0.52092      0.00026      0.27135      0.52092      0.00000      0.00000
 16     0.05859       0.05859      0.46719      0.00032      0.21826      0.46719      0.00000      0.00000
 17     0.06250       0.06250      0.41393      0.00038      0.17134      0.41393      0.00000      0.00000
 18     0.06641       0.06641      0.36174      0.00045      0.13086      0.36174      0.00125      0.00300
 19     0.07031       0.07031      0.31116      0.00054      0.09682      0.31116      0.00172      0.00390
 20     0.07422       0.07422      0.26269      0.00062      0.06901      0.26269      0.00235      0.00504
 21     0.07813       0.07813      0.21678      0.00071      0.04700      0.21678      0.00329      0.00670
 22     0.08203       0.08203      0.17385      0.00080      0.03023      0.17386      0.00462      0.00897
 23     0.08594       0.08594      0.13423      0.00091      0.01802      0.13423      0.00676      0.01252
 24     0.08984       0.08984      0.09821      0.00100      0.00965      0.09821      0.01022      0.01810
 25     0.09375       0.09375      0.06599      0.00111      0.00436      0.06600      0.01681      0.02854
 26     0.09766       0.09766      0.03774      0.00120      0.00143      0.03776      0.03190      0.05199
 27     0.10156       0.10156      0.01352      0.00131      0.00018      0.01358      0.09631      0.15093
 28     0.10547       0.10547     -0.00664      0.00139      0.00005      0.00678      2.93503      4.42904
 29     0.10938       0.10938     -0.02280      0.00148      0.00052      0.02284      3.07679      4.47713
 30     0.11328       0.11328     -0.03505      0.00155      0.00123      0.03508      3.09753      4.35188
 31     0.11719       0.11719     -0.04358      0.00161      0.00190      0.04361      3.10466      4.21651
 32     0.12109       0.12109     -0.04859      0.00165      0.00236      0.04862      3.10770      4.08449
 33     0.12500       0.12500     -0.05037      0.00168      0.00254      0.05040      3.10828      3.95758
 34     0.12891       0.12891     -0.04920      0.00168      0.00242      0.04923      3.10751      3.83671
 35     0.13281       0.13281     -0.04544      0.00167      0.00207      0.04547      3.10494      3.72078
 36     0.13672       0.13672     -0.03944      0.00162      0.00156      0.03947      3.10059      3.60941
 37     0.14063       0.14063     -0.03158      0.00156      0.00100      0.03162      3.09233      3.49980
 38     0.14453       0.14453     -0.02225      0.00146      0.00050      0.02230      3.07626      3.38751
 39     0.14844       0.14844     -0.01185      0.00134      0.00014      0.01192      3.02892      3.24761
 40     0.15234       0.15234     -0.00075      0.00118      0.00000      0.00140      2.13833      2.23393
 41     0.15625       0.15625      0.01065      0.00101      0.00011      0.01070      0.09496      0.09672
 42     0.16016       0.16016      0.02202      0.00080      0.00049      0.02203      0.03654      0.03631
 43     0.16406       0.16406      0.03302      0.00059      0.00109      0.03302      0.01773      0.01720
 44     0.16797       0.16797      0.04335      0.00033      0.00188      0.04335      0.00761      0.00722
 45     0.17188       0.17188      0.05275      0.00007      0.00278      0.05275      0.00134      0.00125
 46     0.17578       0.17578      0.06100     -0.00022      0.00372      0.06100     -0.00356     -0.00322
 47     0.17969       0.17969      0.06792     -0.00050      0.00461      0.06792     -0.00737     -0.00653
 48     0.18359       0.18359      0.07336     -0.00080      0.00538      0.07337     -0.01095     -0.00949
 49     0.18750       0.18750      0.07724     -0.00109      0.00597      0.07724     -0.01411     -0.01198
 50     0.19141       0.19141      0.07949     -0.00138      0.00632      0.07950     -0.01740     -0.01447
 51     0.19531       0.19531      0.08013     -0.00165      0.00642      0.08014     -0.02057     -0.01676
 52     0.19922       0.19922      0.07917     -0.00191      0.00627      0.07919     -0.02407     -0.01923
 53     0.20313       0.20313      0.07671     -0.00212      0.00589      0.07674     -0.02764     -0.02166
 54     0.20703       0.20703      0.07283     -0.00231      0.00531      0.07287     -0.03176     -0.02441
 55     0.21094       0.21094      0.06770     -0.00245      0.00459      0.06774     -0.03615     -0.02728
 56     0.21484       0.21484      0.06146     -0.00255      0.00378      0.06152     -0.04142     -0.03068
 57     0.21875       0.21875      0.05433     -0.00257      0.00296      0.05439     -0.04733     -0.03444
 58     0.22266       0.22266      0.04648     -0.00255      0.00217      0.04655     -0.05480     -0.03917
 59     0.22656       0.22656      0.03816     -0.00244      0.00146      0.03823     -0.06392     -0.04490
 60     0.23047       0.23047      0.02956     -0.00227      0.00088      0.02964     -0.07676     -0.05301
 61     0.23438       0.23438      0.02092     -0.00201      0.00044      0.02102     -0.09586     -0.06509
 62     0.23828       0.23828      0.01244     -0.00168      0.00016      0.01256     -0.13427     -0.08968
 63     0.24219       0.24219      0.00435     -0.00125      0.00002      0.00453     -0.28037     -0.18424
 64     0.24609       0.24609     -0.00319     -0.00075      0.00001      0.00328     -2.91037     -1.88221
 65     0.25000       0.25000     -0.01000     -0.00015      0.00010      0.01000     -3.12620     -1.99020
 66     0.25391       0.25391     -0.01593      0.00051      0.00025      0.01594      3.10931      1.94900
 67     0.25781       0.25781     -0.02086      0.00127      0.00044      0.02090      3.08057      1.90173
 68     0.26172       0.26172     -0.02472      0.00210      0.00062      0.02481      3.05694      1.85897
 69     0.26563       0.26563     -0.02741      0.00300      0.00076      0.02758      3.03255      1.81702
 70     0.26953       0.26953     -0.02894      0.00395      0.00085      0.02921      3.00580      1.77489
 71     0.27344       0.27344     -0.02927      0.00497      0.00088      0.02969      2.97339      1.73067
 72     0.27734       0.27734     -0.02847      0.00602      0.00085      0.02910      2.93323      1.68325
 73     0.28125       0.28125     -0.02656      0.00711      0.00076      0.02749      2.88001      1.62975
 74     0.28516       0.28516     -0.02365      0.00821      0.00063      0.02503      2.80746      1.56693
 75     0.28906       0.28906     -0.01982      0.00932      0.00048      0.02190      2.70181      1.48759
 76     0.29297       0.29297     -0.01523      0.01042      0.00034      0.01845      2.54134      1.38058
 77     0.29688       0.29688     -0.00999      0.01151      0.00023      0.01524      2.28552      1.22527
 78     0.30078       0.30078     -0.00429      0.01255      0.00018      0.01327      1.90032      1.00553
 79     0.30469       0.30469      0.00173      0.01355      0.00019      0.01366      1.44385      0.75420
 80     0.30859       0.30859      0.00787      0.01449      0.00027      0.01649      1.07315      0.55347
 81     0.31250       0.31250      0.01400      0.01535      0.00043      0.02077      0.83136      0.42341
 82     0.31641       0.31641      0.01990      0.01611      0.00066      0.02561      0.68060      0.34235
 83     0.32031       0.32031      0.02547      0.01678      0.00093      0.03050      0.58263      0.28949
 84     0.32422       0.32422      0.03051      0.01734      0.00123      0.03509      0.51678      0.25368
 85     0.32813       0.32813      0.03494      0.01778      0.00154      0.03920      0.47066      0.22829
 86     0.33203       0.33203      0.03859      0.01808      0.00182      0.04261      0.43822      0.21006
 87     0.33594       0.33594      0.04142      0.01825      0.00205      0.04526      0.41510      0.19666
 88     0.33984       0.33984      0.04331      0.01829      0.00221      0.04701      0.39950      0.18709
 89     0.34375       0.34375      0.04427      0.01818      0.00229      0.04786      0.38955      0.18036
 90     0.34766       0.34766      0.04423      0.01792      0.00228      0.04773      0.38503      0.17626
 91     0.35156       0.35156      0.04324      0.01753      0.00218      0.04666      0.38513      0.17435
 92     0.35547       0.35547      0.04129      0.01700      0.00199      0.04465      0.39065      0.17490
 93     0.35938       0.35938      0.03847      0.01634      0.00175      0.04180      0.40159      0.17785
 94     0.36328       0.36328      0.03481      0.01555      0.00145      0.03813      0.42019      0.18409
 95     0.36719       0.36719      0.03047      0.01465      0.00114      0.03381      0.44813      0.19424
 96     0.37109       0.37109      0.02549      0.01365      0.00084      0.02892      0.49151      0.21080
 97     0.37500       0.37500      0.02007      0.01254      0.00056      0.02367      0.55854      0.23705
 98     0.37891       0.37891      0.01429      0.01137      0.00033      0.01826      0.67242      0.28244
 99     0.38281       0.38281      0.00834      0.01013      0.00017      0.01312      0.88192      0.36666
100     0.38672       0.38672      0.00232      0.00885      0.00008      0.00915      1.31405      0.54080
101     0.39063       0.39063     -0.00355      0.00752      0.00007      0.00832      2.01169      0.81964
102     0.39453       0.39453     -0.00920      0.00620      0.00012      0.01110      2.54827      1.02798
103     0.39844       0.39844     -0.01442      0.00487      0.00023      0.01522      2.81614      1.12490
104     0.40234       0.40234     -0.01915      0.00357      0.00038      0.01948      2.95720      1.16978
105     0.40625       0.40625     -0.02320      0.00229      0.00054      0.02331      3.04328      1.19226
106     0.41016       0.41016     -0.02655      0.00109      0.00071      0.02657      3.10072      1.20319
107     0.41406       0.41406     -0.02905     -0.00008      0.00084      0.02905     -3.13889     -1.20651
108     0.41797       0.41797     -0.03071     -0.00113      0.00094      0.03073     -3.10485     -1.18227
109     0.42188       0.42188     -0.03142     -0.00212      0.00099      0.03149     -3.07423     -1.15977
110     0.42578       0.42578     -0.03124     -0.00297      0.00098      0.03138     -3.04689     -1.13891
111     0.42969       0.42969     -0.03012     -0.00374      0.00092      0.03035     -3.01796     -1.11784
112     0.43359       0.43359     -0.02814     -0.00435      0.00081      0.02848     -2.98820     -1.09685
113     0.43750       0.43750     -0.02530     -0.00488      0.00066      0.02577     -2.95103     -1.07353
114     0.44141       0.44141     -0.02175     -0.00523      0.00050      0.02237     -2.90568     -1.04768
115     0.44531       0.44531     -0.01749     -0.00550      0.00034      0.01834     -2.83693     -1.01392
116     0.44922       0.44922     -0.01273     -0.00558      0.00019      0.01390     -2.72840     -0.96665
117     0.45313       0.45313     -0.00750     -0.00560      0.00009      0.00936     -2.49982     -0.87803
118     0.45703       0.45703     -0.00200     -0.00542      0.00003      0.00578     -1.92495     -0.67034
119     0.46094       0.46094      0.00368     -0.00521      0.00004      0.00638     -0.95644     -0.33025
120     0.46484       0.46484      0.00933     -0.00480      0.00011      0.01050     -0.47500     -0.16263
121     0.46875       0.46875      0.01490     -0.00440      0.00024      0.01553     -0.28708     -0.09747
122     0.47266       0.47266      0.02015     -0.00379      0.00042      0.02050     -0.18576     -0.06255
123     0.47656       0.47656      0.02504     -0.00324      0.00064      0.02525     -0.12885     -0.04303
124     0.48047       0.48047      0.02936     -0.00247      0.00087      0.02947     -0.08407     -0.02785
125     0.48438       0.48438      0.03309     -0.00186      0.00110      0.03314     -0.05621     -0.01847
126     0.48828       0.48828      0.03605     -0.00096      0.00130      0.03606     -0.02667     -0.00869
127     0.49219       0.49219      0.03824     -0.00040      0.00146      0.03825     -0.01034     -0.00334
128     0.49609       0.49609      0.03954      0.00072      0.00156      0.03955      0.01828      0.00587
129     0.50000       0.50000      0.03999      0.00000      0.00160      0.03999      0.00000      0.00000
NUMBER POINTS OUTPUT =         129
**** This image is ALIASED ****
INTEGRATED MTF AMPLITUDE FROM .25 TO .48 =       1.50719
 FREQUENCY         -1                                                0                                                +1
                    11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
   0.0000          A                                                 W                                                 R
   0.0039          A                                                IW                                                 R
   0.0078          A                                                 W                                                 R
   0.0117          A                                                 W                                                 R
   0.0156           A                                                W                                                 R
   0.0195            A                                               W                                                 R
   0.0234              A                                             W                                                 R
   0.0273                A                                           W                                                 R
   0.0312                  A                                         W                                                 R
   0.0351                    A                                       W                                                 R
   0.0390                      A                                     W                                                 R
   0.0429                         A                                  W                                                 R
   0.0468                            A                               W                                                 R
   0.0507                              A                             W                                                 R
   0.0546                                 A                          W                                                 R
   0.0585                                    A                       W                                                 R
   0.0625                                      A                     W                                                 R
   0.0664                                         A                  W                                                 R
   0.0703                                            A               W                                                 R
   0.0742                                              A             W                                                 R
   0.0781                                                A           W                                                 R
   0.0820                                                   A        W                                                 R
   0.0859                                                     A      W                                                 R
   0.0898                                                      A     WI                                                R
   0.0937                                                        A   WI                                                R
   0.0976                                                          A WI                                    R
   0.1015                                                           AWI           R
   0.1054                                                     R     AWI
   0.1093                                    R                     A WI
   0.1132                        R                                 A WI
   0.1171               R                                         A  WI
   0.1210          R                                              A  WI
   0.1250          R                                              A  WI
   0.1289          R                                              A  WI
   0.1328             R                                           A  WI
   0.1367                   R                                     A  WI
   0.1406                           R                              A WI
   0.1445                                     R                    A WI
   0.1484                                               R           AWI
   0.1523                                                           AWI
   0.1562                                                           AWI        R
   0.1601                                                          A W                     R
   0.1640                                                          A W                                R
   0.1679                                                         A  W                                           R
   0.1718                                                         A  W                                                 R
   0.1757                                                        A  IW                                                 R
   0.1796                                                        A  IW                                                 R
   0.1835                                                        A  IW                                                 R
   0.1875                                                        A I W                                                 R
   0.1914                                                       A  I W                                                 R
   0.1953                                                       A  I W                                                 R
   0.1992                                                       A  I W                                                 R
   0.2031                                                        AI  W                                                 R
   0.2070                                                        AI  W                                                 R
   0.2109                                                        AI  W                                                 R
   0.2148                                                        AI  W                                                 R
   0.2187                                                         A  W                                                 R
   0.2226                                                         A  W                                              R
   0.2265                                                         IA W                                     R
   0.2304                                                         IA W                             R
   0.2343                                                         IA W                    R
   0.2382                                                          IAW           R
   0.2421                                                          IAW   R
   0.2460                                                        R  AW
   0.2500                                                 R         AW
   0.2539                                           R               AW
   0.2578                                      R                   A WI
   0.2617                                  R                       A W I
   0.2656                                R                         A W  I
   0.2695                              R                           A W   I
   0.2734                              R                           A W    I
   0.2773                              R                           A W     I
   0.2812                                R                         A W      I
   0.2851                                   R                      A W       I
   0.2890                                       R                  A W        I
   0.2929                                            R              AW         I
   0.2968                                                 R         AW          I
   0.3007                                                       R   AW           I
   0.3046                                                           AWR           I
   0.3085                                                           AW       R     I
   0.3125                                                          A W             RI
   0.3164                                                          A W               I   R
   0.3203                                                          A W                I       R
   0.3242                                                          A W                I             R
   0.3281                                                          A W                 I                R
   0.3320                                                         A  W                 I                    R
   0.3359                                                         A  W                 I                       R
   0.3398                                                         A  W                 I                         R
   0.3437                                                         A  W                 I                          R
   0.3476                                                         A  W                 I                          R
   0.3515                                                         A  W                I                          R
   0.3554                                                         A  W                I                        R
   0.3593                                                         A  W               I                      R
   0.3632                                                          A W              I                   R
   0.3671                                                          A W             I                R
   0.3710                                                          A W            I            R
   0.3750                                                          A W           I       R
   0.3789                                                           AW          I  R
   0.3828                                                           AW       R I
   0.3867                                                           AW R      I
   0.3906                                                        R  AW      I
   0.3945                                                  R        AW     I
   0.3984                                             R             AW   I
   0.4023                                        R                  AW  I
   0.4062                                    R                     A W I
   0.4101                                R                         A WI
   0.4140                              R                           AIW
   0.4179                            R                             A W
   0.4218                           R                             IA W
   0.4257                            R                           I A W
   0.4296                             R                          I A W
   0.4335                               R                       I  A W
   0.4375                                  R                    I  A W
   0.4414                                     R                I   A W
   0.4453                                          R           I    AW
   0.4492                                               R      I    AW
   0.4531                                                    R I    AW
   0.4570                                                      I  R AW
   0.4609                                                      I    AW  R
   0.4648                                                       I   AW        R
   0.4687                                                       I   AW              R
   0.4726                                                        I A W                   R
   0.4765                                                        I A W                        R
   0.4804                                                         IA W                            R
   0.4843                                                          A W                                R
   0.4882                                                          AIW                                   R
   0.4921                                                          AIW                                      R
   0.4960                                                         A  W                                       R
   0.5000                                                         A  W                                       R
if (mode = "nobatch" or mode = "inter")
end-if
let $echo="no"
s2edge.img  gsd = 30 meters   altitude = 400,000 meters
          radians/pixel = 1.499999925727e-04  degrees/pixel = 8.594366721809e-03
   psf is 13 pixels  psf in radians = 9.749996825121e-04  psf in degrees = 5.586336553097e-02
otf1 inp=s2edge.img gsd=30. altitude=400000 psfpix=psfpix  +
    pixrad=pixrad pixdeg=pixdeg psfrad=psfrad psfdeg=psfdeg  +
    norangle=22.0 plotout=test32 plotprof=test32 plotfmt=eps
Beginning VICAR task otf1
OTF1 version 2015-08-10
NUMBER OF LINES PROCESSED=         20

 PIXEL  CYCLES/      FREQUENCY    REAL(FT)     IMAG.(FT)     INTENSITY    AMPLITUDE     PHASE     WAVE LENGT
         PIXEL                                                                         RADIANS      SHIFT
  1     0.00000       0.00000      1.00000      0.00000      1.00000      1.00000      0.00000      0.00000
  2     0.00391       0.00391      0.99698     -0.00000      0.99397      0.99698      0.00000      0.00000
  3     0.00781       0.00781      0.98793      0.00000      0.97601      0.98793      0.00000      0.00000
  4     0.01172       0.01172      0.97300      0.00000      0.94674      0.97300      0.00000      0.00000
  5     0.01563       0.01563      0.95236      0.00001      0.90700      0.95236      0.00000      0.00000
  6     0.01953       0.01953      0.92631      0.00001      0.85804      0.92631      0.00000      0.00000
  7     0.02344       0.02344      0.89515      0.00002      0.80129      0.89515      0.00000      0.00000
  8     0.02734       0.02734      0.85931      0.00003      0.73841      0.85931      0.00000      0.00000
  9     0.03125       0.03125      0.81923      0.00005      0.67114      0.81923      0.00000      0.00000
 10     0.03516       0.03516      0.77545      0.00007      0.60132      0.77545      0.00000      0.00000
 11     0.03906       0.03906      0.72848      0.00010      0.53069      0.72848      0.00000      0.00000
 12     0.04297       0.04297      0.67894      0.00012      0.46097      0.67894      0.00000      0.00000
 13     0.04688       0.04688      0.62742      0.00017      0.39365      0.62742      0.00000      0.00000
 14     0.05078       0.05078      0.57454      0.00021      0.33009      0.57454      0.00000      0.00000
 15     0.05469       0.05469      0.52092      0.00026      0.27135      0.52092      0.00000      0.00000
 16     0.05859       0.05859      0.46719      0.00032      0.21826      0.46719      0.00000      0.00000
 17     0.06250       0.06250      0.41393      0.00038      0.17134      0.41393      0.00000      0.00000
 18     0.06641       0.06641      0.36174      0.00045      0.13086      0.36174      0.00125      0.00300
 19     0.07031       0.07031      0.31116      0.00054      0.09682      0.31116      0.00172      0.00390
 20     0.07422       0.07422      0.26269      0.00062      0.06901      0.26269      0.00235      0.00504
 21     0.07813       0.07813      0.21678      0.00071      0.04700      0.21678      0.00329      0.00670
 22     0.08203       0.08203      0.17385      0.00080      0.03023      0.17386      0.00462      0.00897
 23     0.08594       0.08594      0.13423      0.00091      0.01802      0.13423      0.00676      0.01252
 24     0.08984       0.08984      0.09821      0.00100      0.00965      0.09821      0.01022      0.01810
 25     0.09375       0.09375      0.06599      0.00111      0.00436      0.06600      0.01681      0.02854
 26     0.09766       0.09766      0.03774      0.00120      0.00143      0.03776      0.03190      0.05199
 27     0.10156       0.10156      0.01352      0.00131      0.00018      0.01358      0.09631      0.15093
 28     0.10547       0.10547     -0.00664      0.00139      0.00005      0.00678      2.93503      4.42904
 29     0.10938       0.10938     -0.02280      0.00148      0.00052      0.02284      3.07679      4.47713
 30     0.11328       0.11328     -0.03505      0.00155      0.00123      0.03508      3.09753      4.35188
 31     0.11719       0.11719     -0.04358      0.00161      0.00190      0.04361      3.10466      4.21651
 32     0.12109       0.12109     -0.04859      0.00165      0.00236      0.04862      3.10770      4.08449
 33     0.12500       0.12500     -0.05037      0.00168      0.00254      0.05040      3.10828      3.95758
 34     0.12891       0.12891     -0.04920      0.00168      0.00242      0.04923      3.10751      3.83671
 35     0.13281       0.13281     -0.04544      0.00167      0.00207      0.04547      3.10494      3.72078
 36     0.13672       0.13672     -0.03944      0.00162      0.00156      0.03947      3.10059      3.60941
 37     0.14063       0.14063     -0.03158      0.00156      0.00100      0.03162      3.09233      3.49980
 38     0.14453       0.14453     -0.02225      0.00146      0.00050      0.02230      3.07626      3.38751
 39     0.14844       0.14844     -0.01185      0.00134      0.00014      0.01192      3.02892      3.24761
 40     0.15234       0.15234     -0.00075      0.00118      0.00000      0.00140      2.13833      2.23393
 41     0.15625       0.15625      0.01065      0.00101      0.00011      0.01070      0.09496      0.09672
 42     0.16016       0.16016      0.02202      0.00080      0.00049      0.02203      0.03654      0.03631
 43     0.16406       0.16406      0.03302      0.00059      0.00109      0.03302      0.01773      0.01720
 44     0.16797       0.16797      0.04335      0.00033      0.00188      0.04335      0.00761      0.00722
 45     0.17188       0.17188      0.05275      0.00007      0.00278      0.05275      0.00134      0.00125
 46     0.17578       0.17578      0.06100     -0.00022      0.00372      0.06100     -0.00356     -0.00322
 47     0.17969       0.17969      0.06792     -0.00050      0.00461      0.06792     -0.00737     -0.00653
 48     0.18359       0.18359      0.07336     -0.00080      0.00538      0.07337     -0.01095     -0.00949
 49     0.18750       0.18750      0.07724     -0.00109      0.00597      0.07724     -0.01411     -0.01198
 50     0.19141       0.19141      0.07949     -0.00138      0.00632      0.07950     -0.01740     -0.01447
 51     0.19531       0.19531      0.08013     -0.00165      0.00642      0.08014     -0.02057     -0.01676
 52     0.19922       0.19922      0.07917     -0.00191      0.00627      0.07919     -0.02407     -0.01923
 53     0.20313       0.20313      0.07671     -0.00212      0.00589      0.07674     -0.02764     -0.02166
 54     0.20703       0.20703      0.07283     -0.00231      0.00531      0.07287     -0.03176     -0.02441
 55     0.21094       0.21094      0.06770     -0.00245      0.00459      0.06774     -0.03615     -0.02728
 56     0.21484       0.21484      0.06146     -0.00255      0.00378      0.06152     -0.04142     -0.03068
 57     0.21875       0.21875      0.05433     -0.00257      0.00296      0.05439     -0.04733     -0.03444
 58     0.22266       0.22266      0.04648     -0.00255      0.00217      0.04655     -0.05480     -0.03917
 59     0.22656       0.22656      0.03816     -0.00244      0.00146      0.03823     -0.06392     -0.04490
 60     0.23047       0.23047      0.02956     -0.00227      0.00088      0.02964     -0.07676     -0.05301
 61     0.23438       0.23438      0.02092     -0.00201      0.00044      0.02102     -0.09586     -0.06509
 62     0.23828       0.23828      0.01244     -0.00168      0.00016      0.01256     -0.13427     -0.08968
 63     0.24219       0.24219      0.00435     -0.00125      0.00002      0.00453     -0.28037     -0.18424
 64     0.24609       0.24609     -0.00319     -0.00075      0.00001      0.00328     -2.91037     -1.88221
 65     0.25000       0.25000     -0.01000     -0.00015      0.00010      0.01000     -3.12620     -1.99020
 66     0.25391       0.25391     -0.01593      0.00051      0.00025      0.01594      3.10931      1.94900
 67     0.25781       0.25781     -0.02086      0.00127      0.00044      0.02090      3.08057      1.90173
 68     0.26172       0.26172     -0.02472      0.00210      0.00062      0.02481      3.05694      1.85897
 69     0.26563       0.26563     -0.02741      0.00300      0.00076      0.02758      3.03255      1.81702
 70     0.26953       0.26953     -0.02894      0.00395      0.00085      0.02921      3.00580      1.77489
 71     0.27344       0.27344     -0.02927      0.00497      0.00088      0.02969      2.97339      1.73067
 72     0.27734       0.27734     -0.02847      0.00602      0.00085      0.02910      2.93323      1.68325
 73     0.28125       0.28125     -0.02656      0.00711      0.00076      0.02749      2.88001      1.62975
 74     0.28516       0.28516     -0.02365      0.00821      0.00063      0.02503      2.80746      1.56693
 75     0.28906       0.28906     -0.01982      0.00932      0.00048      0.02190      2.70181      1.48759
 76     0.29297       0.29297     -0.01523      0.01042      0.00034      0.01845      2.54134      1.38058
 77     0.29688       0.29688     -0.00999      0.01151      0.00023      0.01524      2.28552      1.22527
 78     0.30078       0.30078     -0.00429      0.01255      0.00018      0.01327      1.90032      1.00553
 79     0.30469       0.30469      0.00173      0.01355      0.00019      0.01366      1.44385      0.75420
 80     0.30859       0.30859      0.00787      0.01449      0.00027      0.01649      1.07315      0.55347
 81     0.31250       0.31250      0.01400      0.01535      0.00043      0.02077      0.83136      0.42341
 82     0.31641       0.31641      0.01990      0.01611      0.00066      0.02561      0.68060      0.34235
 83     0.32031       0.32031      0.02547      0.01678      0.00093      0.03050      0.58263      0.28949
 84     0.32422       0.32422      0.03051      0.01734      0.00123      0.03509      0.51678      0.25368
 85     0.32813       0.32813      0.03494      0.01778      0.00154      0.03920      0.47066      0.22829
 86     0.33203       0.33203      0.03859      0.01808      0.00182      0.04261      0.43822      0.21006
 87     0.33594       0.33594      0.04142      0.01825      0.00205      0.04526      0.41510      0.19666
 88     0.33984       0.33984      0.04331      0.01829      0.00221      0.04701      0.39950      0.18709
 89     0.34375       0.34375      0.04427      0.01818      0.00229      0.04786      0.38955      0.18036
 90     0.34766       0.34766      0.04423      0.01792      0.00228      0.04773      0.38503      0.17626
 91     0.35156       0.35156      0.04324      0.01753      0.00218      0.04666      0.38513      0.17435
 92     0.35547       0.35547      0.04129      0.01700      0.00199      0.04465      0.39065      0.17490
 93     0.35938       0.35938      0.03847      0.01634      0.00175      0.04180      0.40159      0.17785
 94     0.36328       0.36328      0.03481      0.01555      0.00145      0.03813      0.42019      0.18409
 95     0.36719       0.36719      0.03047      0.01465      0.00114      0.03381      0.44813      0.19424
 96     0.37109       0.37109      0.02549      0.01365      0.00084      0.02892      0.49151      0.21080
 97     0.37500       0.37500      0.02007      0.01254      0.00056      0.02367      0.55854      0.23705
 98     0.37891       0.37891      0.01429      0.01137      0.00033      0.01826      0.67242      0.28244
 99     0.38281       0.38281      0.00834      0.01013      0.00017      0.01312      0.88192      0.36666
100     0.38672       0.38672      0.00232      0.00885      0.00008      0.00915      1.31405      0.54080
101     0.39063       0.39063     -0.00355      0.00752      0.00007      0.00832      2.01169      0.81964
102     0.39453       0.39453     -0.00920      0.00620      0.00012      0.01110      2.54827      1.02798
103     0.39844       0.39844     -0.01442      0.00487      0.00023      0.01522      2.81614      1.12490
104     0.40234       0.40234     -0.01915      0.00357      0.00038      0.01948      2.95720      1.16978
105     0.40625       0.40625     -0.02320      0.00229      0.00054      0.02331      3.04328      1.19226
106     0.41016       0.41016     -0.02655      0.00109      0.00071      0.02657      3.10072      1.20319
107     0.41406       0.41406     -0.02905     -0.00008      0.00084      0.02905     -3.13889     -1.20651
108     0.41797       0.41797     -0.03071     -0.00113      0.00094      0.03073     -3.10485     -1.18227
109     0.42188       0.42188     -0.03142     -0.00212      0.00099      0.03149     -3.07423     -1.15977
110     0.42578       0.42578     -0.03124     -0.00297      0.00098      0.03138     -3.04689     -1.13891
111     0.42969       0.42969     -0.03012     -0.00374      0.00092      0.03035     -3.01796     -1.11784
112     0.43359       0.43359     -0.02814     -0.00435      0.00081      0.02848     -2.98820     -1.09685
113     0.43750       0.43750     -0.02530     -0.00488      0.00066      0.02577     -2.95103     -1.07353
114     0.44141       0.44141     -0.02175     -0.00523      0.00050      0.02237     -2.90568     -1.04768
115     0.44531       0.44531     -0.01749     -0.00550      0.00034      0.01834     -2.83693     -1.01392
116     0.44922       0.44922     -0.01273     -0.00558      0.00019      0.01390     -2.72840     -0.96665
117     0.45313       0.45313     -0.00750     -0.00560      0.00009      0.00936     -2.49982     -0.87803
118     0.45703       0.45703     -0.00200     -0.00542      0.00003      0.00578     -1.92495     -0.67034
119     0.46094       0.46094      0.00368     -0.00521      0.00004      0.00638     -0.95644     -0.33025
120     0.46484       0.46484      0.00933     -0.00480      0.00011      0.01050     -0.47500     -0.16263
121     0.46875       0.46875      0.01490     -0.00440      0.00024      0.01553     -0.28708     -0.09747
122     0.47266       0.47266      0.02015     -0.00379      0.00042      0.02050     -0.18576     -0.06255
123     0.47656       0.47656      0.02504     -0.00324      0.00064      0.02525     -0.12885     -0.04303
124     0.48047       0.48047      0.02936     -0.00247      0.00087      0.02947     -0.08407     -0.02785
125     0.48438       0.48438      0.03309     -0.00186      0.00110      0.03314     -0.05621     -0.01847
126     0.48828       0.48828      0.03605     -0.00096      0.00130      0.03606     -0.02667     -0.00869
127     0.49219       0.49219      0.03824     -0.00040      0.00146      0.03825     -0.01034     -0.00334
128     0.49609       0.49609      0.03954      0.00072      0.00156      0.03955      0.01828      0.00587
129     0.50000       0.50000      0.03999      0.00000      0.00160      0.03999      0.00000      0.00000
NUMBER POINTS OUTPUT =         129
**** This image is ALIASED ****
INTEGRATED MTF AMPLITUDE FROM .25 TO .48 =       1.50719
ush gnuplot test32.eps.gpi
ush gnuplot test32.prof.eps.gpi
let $echo="no"
$ Return
$!#############################################################################
