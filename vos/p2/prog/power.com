$!****************************************************************************
$!
$! Build proc for MIPL module power
$! VPACK Version 1.9, Thursday, April 23, 2015, 17:15:35
$!
$! Execute by entering:		$ @power
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
$ write sys$output "*** module power ***"
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
$ write sys$output "Invalid argument given to power.com file -- ", primary
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
$   if F$SEARCH("power.imake") .nes. ""
$   then
$      vimake power
$      purge power.bld
$   else
$      if F$SEARCH("power.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake power
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @power.bld "STD"
$   else
$      @power.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create power.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack power.com -mixed -
	-s power.f -
	-i power.imake -
	-p power.pdf -
	-t tstpower.pdf tstpower.log.solos tstpower.log.linux
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create power.f
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create power.imake
#define  PROGRAM   power

#define MODULE_LIST power.f

#define MAIN_LANG_FORTRAN
#define R2LIB 
#define USES_FORTRAN
/*
#define LIB_MOTIF
#define LIB_XRT_GRAPH  no longer - Jun 25, 2012 
*/

#define LIB_RTL  
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create power.pdf
process help=*
 PARM INP         TYPE=STRING   COUNT=1
 PARM SIZE        TYPE=INTEGER  COUNT=4          DEFAULT=(1,1,0,0)
 PARM SL          TYPE=INTEGER  COUNT=1          DEFAULT=1
 PARM SS          TYPE=INTEGER  COUNT=1          DEFAULT=1
 PARM NL          TYPE=INTEGER  COUNT=1          DEFAULT=0
 PARM NS          TYPE=INTEGER  COUNT=1          DEFAULT=0
 PARM PLOTFMT     TYPE=STRING   COUNT=(0:1) VALID=(GNUPLOT,EPS) DEFAULT=--
 PARM PLOTOUT     TYPE=STRING   COUNT=0:1       DEFAULT=--
 PARM EXPONENT    TYPE=INTEGER  COUNT=(0:1) VALID=(3:10) DEFAULT=--
 PARM SCALE       TYPE=REAL     COUNT=1          DEFAULT=2.
 PARM DNSCALE     TYPE=REAL     COUNT=1          DEFAULT=1.0
 PARM FMAX        TYPE=REAL     COUNT=1          DEFAULT=0.5
 PARM YLEN        TYPE=REAL     COUNT=1          DEFAULT=7.0
 PARM TITLE       TYPE=(STRING,80)     COUNT=0:1 DEFAULT=--
 PARM TITLEX      TYPE=(STRING,80)     COUNT=0:1 DEFAULT="FREQUENCY (CPS)"
! PARM TABLE       TYPE=STRING	COUNT=(0:1)	     DEFAULT=--
! PARM COLUMNS     TYPE=KEYWORD  COUNT=(0:1) VALID=(COLHDR,NOCOLHDR) +
!                    DEFAULT=COLHDR
! PARM NODISP      TYPE=KEYWORD  COUNT=(0:1) VALID=NODISP   DEFAULT=--
 END-PROC
.TITLE
 VICAR Application program POWER - AFIDS Version with GNUPLOT
.HELP
 "POWER" computes the 1 - dimensional power spectrum of a specified portion of 
  a picture.  

  Although the output is called 'power spectrum', the value computed is
  the square root of the power spectrum (the peak-to-peak amplitude in DN).
  Therefore, strictly speaking the output is an 'amplitude spectrum'.
  
  This output can be:
  1.  Written to a plot file which can be plotted on a POSTSCRIPT printer 
      or other POSTSCRIPT device.
  2.  Plotted directly to an X image display device.
  3.  Written as a tab-delimited ASCII file for use by commmerical packages.

 OPERATION:
  
  POWER uses the fast Fourier Transform to produce the power spectrum of each
  line of a specified portion of an input picture.  POWER produces a single 
  resultant power spectrum by averaging these together as a function of 
  frequency. The desired portion of the picture is specified in the size field.

  If a transform size (EXPONENT) is specified larger than the input sample 
  size, the necessary additional samples are generated by averaging the 
  available input data so to minimize sin(x)/x ringing. If it is defaulted,
  the transform size will be the largest power of 2 less than the input
  sample size.

  POWER will take the image format from the input's system label.


 PARAMETERS:

  Formerly the parameter TABLE was used to specify the name of the file 
  to contain the tab-delimited ASCII table of Frequency and Power values 
  to be plotted. This is now automatically created by the name in the
  PLOTOUT parameter. TABLE  now has the name PLOTOUT.asc. The table
  is always created even if PLOTOUT is not entered.

  COLUMNS


  If PLOTFMT=EPS is selected, the plot is generate as a POSTSCRIPT (EPS) file.  (See 
  PLOTOUT parameter.)  

  PLOTOUT='filename' is used to name the output plot file.  Specifying PLOTOUT
  implies TYPE=GNUPLOT. The default value of PLOTOUT is power.eps.

  EXPONENT

  SCALE

  DNSCALE

  FMAX is the highest frequency in the plotted spectrum


  YLEN = length of Y-axis 

  TITLE and TITLEX are the Title to be placed on the graph and the X-axis units
  No entry for the y-axis. It is always "AMPLITUDE (DN P-P)". P-P is peak to peak. 

  


  Example to request only tab-delimited ASCII file output:

 	 power inp=a.img
  
  tab delimited file will be power.asc 

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
  of the power task this file creates an interactive plot on the desktop, e.g.,
  if PLOTOUT=mypower
     ush gnuplot mypower.gpi.
  This display stays active until mouse-clicked somewhere on the plot panel.
  The terminal window prompt is inactive until the panel is closed.

  If you wish, you can issue the following gnuplot command to keep the plot on
  the desktop.

  ush gnuplot -persist mypower.gpi

  Again the terminal prompt is inactive until the plot panel has been mouse-clicked.
  After the mouse click the panel remains but the terminal prompt is restored. This
  makes it easy to compare outputs from several power commands.

  A second output is derived from the PLOTOUT parameter. By default this parameter is
  set to "NONE" so no hardcopy output is produced.  If you give a filename such as
  mypower then two gpi files are created mypower.gpi and mypower.2.gpi.

  ush gnuplot mypower.2.gpi 
 
  then no interactive plot panel is created but it produces a file mypower.eps. This
  file can be displayed using ghostscript (the usual unix command is gs).

  gs mypopwer.eps

  or gimp, e.g.,

  gimp mypower.eps



HISTORY

  ORIGINAL PROGRAMMER: T. C. Rindfleisch
  CURRENT COGNIZANT PROGRAMMER: Ray Bambery - AFIDS/Vicar version  

  1984-09-01 FFM Convert from IBM to VAX
  1993-11-22 CCA Add ASCII table output, checks for open errors, fix test, mod 
                 selection of plot or print
  1994-02-22 CCA Major code cleanup, rework of label handling, test file and plotting 
                 of labels
  1994-04-11 CCA Added dnscale
  1994-06-14 CCA Fixed fmax, deleted histogram plot
  1994-10-20 CCA Corrected accumulation of amplitude
  1994-07-18 CCA Add option of no table column headers
  1995-11-16 CCA Made scal r*4
  1997-04-29  SP Made portable for UNIX.  Adapted for XRT pltting
                 package.  Changed COUNT for FILE parameter to 0:1
                 so that POWER can run without setting an X DISPLAY
                 by specifying neither 'PLOT nor FILE.  (Previously
                 the COUNT received by the program was always 1.)
  2011-06-09 RJB Changed from commercial plotting package, xrt, to gnuplot
                 create gnuplot commands and always create a table file
                 all xrt calls commented out. Completely rewrote test file.
  2012-07-02 RJB Renamed power2 for delivery to MIPL
                 power still uses XRT/Graph package, removed debug
                 statements, Removed <tab> in front of continuation
                 lines to make backward compatible with
                 32-bit Linux gfortran 4.2.1, otherwise
                 compatible 64-bit Linux gfortran 4.6.3</tab>
  2012-10-11 RJB Renamed back to power, in agreement
                 with Lucas Kamp of mipl. The XRT graph package
                 is to be removed from mipl. XRT was never used by
                 cartlab. 
  2012-10-14 RJB Revised the scheme for producing eps files
                 Old scheme used 1 gpi script to produce both
                 gnuplot panel and eps file. This scheme forced
                 power to be interactive when producing eps file
                 Scheme like qplot2 and statplt is now used.
  2013-07-08 RJB Redesigned plot naming conventions 
  2013-07-13 RJB Adjusted eps format to more readable fonts
                 Remove vestiges of debug statments

.LEVEL1

.VARIABLE INP
 input data set

.VARIABLE SIZE
Standard Vicar size field:
  (SL,SS,NL,NS)
You can enter SL,SS,NL,
and NS together as SIZE, OR
enter the SL,SS,NL, and NS
parameters separately.
.VARIABLE SL
Starting line number
.VARIABLE SS
Starting sample number
.VARIABLE NL
Number of lines
.VARIABLE NS
Number of samples

.VARIABLE EXPONENT
 transform size

.VARIABLE SCALE
 output plot amplitude scale

.VARIABLE DNSCALE
 scaling factor of input image

.VARIABLE FMAX
 highest frequency in the 
 plotted spectrum

.VARIABLE YLEN
 length of Y axis

.VARIABLE PLOTFMT
 specification 
 of plotter output
 Gnuplot or eps

.VARIABLE PLOTOUT
 specification of output 
 plot filename

.VARIABLE TITLE
 user specified label to both
 printer and plotter

.VARIABLE TITLEX
 user specified label of X
 axis to the plotter only

.VARIABLE COLUMNS
 selects table column
 headers or not

.LEVEL2

.VARIABLE INP
 string - input data set (only 1 input allowed)

.vari size
The size parameter determines the boundaries in the input
file from which POWER is to operate.  It is specified
as  (SL,SS,NL,NS), where
	SL is the starting line 
	SS is the starting sample
	NL is the number of lines to be copied
	NS is the number of samples (pixels) in each line

.VARIABLE EXPONENT
 EXPONENT=E where E is an integer between or equal to 3 and 10, specifying
 the exponent of 2 for the desired 1 - dimensional transform size. If half
 word data, E referes to the transform size in samples and not bytes.
 The default is the biggest power of 2 that is less than the number of input
 samples.

.VARIABLE SCALE
 REAL - DEFAULT=2.0
 SCALE = S where S is a real specifying the output plot amplitude scale.

.VARIABLE DNSCALE
 REAL - DEFAULT=1.0
 Specifies that the input DNs have been scaled up from the original values,
 and that the scale is to be removed to yield plots scaled to the original
 DNs.  An example would be scaling done during filtering.

.VARIABLE FMAX
 FMAX = F where F is a floating point number specifying the highest frequency
 in the spectrum (the nyquist or aliasing frequency). It is numerically equal
 to 1/2 of the reciprocal of the pixel-to-pixel spacing measured in whatever
 units are used. The length of the frequency axis on the line printer is
 2**(E-1) lines. It is printed 6 data points per inch, and the annotation 
 on the frequency axis is given by F/2**(E -1) DN every line, where F and E 
 are given by the FMAX and EXPONENT keywords, respectively. Default is F=0.5,
 corresponding to a pixel spacing of unity.  

 FMAX applies only to the plots, the output tables ignore it.

.VARIABLE YLEN
 specifies the length in inches of the Y axis.(default=7.0,max=30.)

.VARIABLE PLOTFMT
 KEYWORD - VALID=(GNUPLOT,EPS)
 EPS Specifies POSTSCRIPT plotting, else GNUPLOT is assumed.
 If neither 'PLOTFMT nor PLOTOUT are specified the output table
 will still be generated with the name power.asc

.VARIABLE PLOTOUT
 Specifies the filename of the file to which the POSTSCRIPT plot is written.
 If PLOTOUT is specified, PLOT is unnecessary.  Defaults to POWER.PLT.

.VARIABLE TITLE
 string - this parameter is used to add labeling to both the line printer
 graph and plotter graph. The maximum number of characters in the added
 label is 52. The title will be placed at the top of all the graphs gen-
 erated. No title is provided if TITLE is defaulted.

.VARIABLE TITLEX
 string - this parameter is used to change the X - axis label in the plotter
 graph. The maximum number of characters in the string is 52. The default
 is FREQUENCY(CPS) where CPS can be interpreted as cycles per second or 
 cycles per sample.


.END
$ Return
$!#############################################################################
$Test_File:
$ create tstpower.pdf
procedure
parm    mode    type=string count=(0:1) valid=(batch,nobatch,inter) default=batch
local   afidsroot   type=string count=1
local   aftestdata  type=string count=1
local DIR    TYPE=STRING 
local INPIC   TYPE=STRING

! Aug 23, 2013 - RJB
! TEST SCRIPT FOR POWER
! tests BYTE, HALF images
!
! Vicar Programs:
!       translog f2 typetext label-list 
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
! Requires external test data: 
!   cartlab or mipl dependent pointers
!
!
!   Cartlab defines env var $AFIDS_ROOT, mipl doesn't
!   The test data in cartlab is on /raid1/test_data 
!   but in other facilities it might be somewhere else. 
!   
!   To facilitate this test you can define an
!   environment variable $AFIDS_TESTDATA to point to
!   that data. The cartlab system does not. In the git archive
!   on pistol there is softlink to the test data in vdev that
!   allows this test to pass 

! 
!  the *.gpi data produced by power are gnuplot scripts
refgbl $echo
refgbl $syschar


body
!
let _onfail="stop"
let $echo="yes"

!check to see if mipl or cartlab for certain programs
!cartlab defines env var $AFIDS_ROOT, mipl doesm't
translog INP=AFIDS_ROOT TRANS=afidsroot
translog INP=AFIDS_TESTDATA TRANS=aftestdata
if (afidsroot = "")
!MIPL
    ush ln -s /project/test_work/testdata/cassini/iss cs
else
!CARTLAB
    if (aftestdata = "")
        ush ln -s /raid1/vicar_test_images/testdata/cassini/iss cs
    else
        ush ln -s $AFIDS_TESTDATA/vicar_test_images/testdata/cassini/iss cs
    end-if
end-if


! THIS IS A TEST SCRIPT FOR THE PROGRAM - POWER
let _onfail="goto rm"
let $echo="no"
write "TESTS USING AN IMAGE CONTAINING A ONE CYCLE SINE WAVE OVER 256 SAMPLES"
write "THE MEAN IS 100 DN AND THE AMPLITUDE (PEAK-TO-PEAK) IN DN IS 200."
write "So the first element of the table should be the mean:  100 and the" 
write "max value should be about 200 at a frequency of 1/256 = .0039"
let $echo="yes"
! TEST 1
f2 out=A.h size=(1,1,50,256) 'half func="100*(1+sin(3.1415926*samp/128.))"
power A.h plotfmt=eps plotout=sin1  scale=30 fmax=.1  +
    title="TEST 1 - RMS POWER SPECTRUM FOR FILE sin1" 
typetext sin1.asc
    ush gnuplot sin1.eps.gpi
if (mode = "nobatch" or mode = "inter") 
    ush gnuplot sin1.gpi
end-if

! TEST 2
f2 out=A.b1 size=(1,1,50,256) 'byte func="100*(1+sin(3.1415926*samp/128.))"
power A.b1  plotout=sin2 scale=30 fmax=.1  +
    TITLE="TEST 2 - RMS POWER SPECTRUM FOR BYTE FILE sin2"
typetext sin2.asc

if (mode = "nobatch" or mode = "inter")
    ush gnuplot sin2.gpi
end-if

let $echo="no"
write "TRY A SINE WAVE OVER 256 LINES"
let $echo="yes"
! TEST 3
f2 out=A.b2 size=(1,1,256,50) 'byte func="100*(1+sin(3.1415926*line/128.))"
power A.b2 plotout=sin3 scale=20 fmax=.1 +
    title="TEST 3 - RMS POWER SPECTRUM FOR BYTE FILE sin3"
typetext sin3.asc

if (mode = "nobatch" or mode = "inter")
    ush gnuplot sin3.gpi
end-if


let $echo="no"
write "HALFWORD DATA TESTS on Cassini data"
let $echo="yes"
! TEST 4
power cs/sum2.38 (11,11,50,140) plotout=case1  +
    title="TEST 4 - RMS POWER SPECTRUM FOR HALF FILE sum2.38"

if (mode = "nobatch" or mode = "inter")
    ush gnuplot case1.gpi
end-if

! TEST 5 - change x axis to FREQ 
power cs/sum2.38 (1,1,100,500) plotout=case2 EXPONENT=9 TITLEX="FREQ"  +
    title="TEST 5 - RMS POWER SPECTRUM FOR HALF FILE sum2.38"

if (mode = "nobatch" or mode = "inter")
    ush gnuplot case2.gpi
end-if
! TEST 6 - change title to FLORANCE (for Florance Moss)   - expand x-axis to 2.0
! although max plotted is 0.5
! xrtgraph only allowed title to be 52 chars
power cs/sum2.38 (1,1,150,64)  EXPONENT=6 FMAX=2.0 +
    TITLE="TEST 6 - FLORANCE CASE3 RMS POWER SPECTRUM FOR HAF FILE sum2.38" 

if (mode = "nobatch" or mode = "inter")
    ush gnuplot power.gpi
end-if

! TEST 7 - Title is FLORANCE - DNSCALE = 20.0 times 1.0 inches
f2 cs/sum2.38 T FUNC="IN1*20."
power T (1,1,150,64)  EXPONENT=6 FMAX=2.0  DNSCALE=20. +
          plotout=case4 TITLE="TEST 7 - FLORANCE CASE4"

if (mode = "nobatch" or mode = "inter")
    ush gnuplot case4.gpi
end-if
! TEST 8
power cs/sum2.38 (1,1,120,50) EXPONENT=6 TITLE="TEST 8 - PLOT ONLY - CASE5" plotout=case5

if (mode = "nobatch" or mode = "inter")
    ush gnuplot case5.gpi
end-if
! TEST 9
ush rm power.gpi
power cs/sum2.38 (1,1,120,50)  plotout=case6  +
    title="TEST 9 - CASE6 RMS POWER SPECTRUM FOR FILE sum2.38"
typetext case6.asc

if (mode = "nobatch" or mode = "inter")
    ush gnuplot case6.gpi
end-if

!ush gnuplot power.gpi

let $echo="no"
write "BYTE DATA TESTS"
let $echo="yes"

!LET INPIC = "&DIR"//"grid.byte"      
! TEST 10
label-li cs/grid.byte
!ush rm power.gpi
power cs/grid.byte (1,1,50,50) EXPONENT=6  TITLE="TEST 10 - BYTE TEST FOR FILE grid.byte" +
     plotout=grid0

if (mode = "nobatch" or mode = "inter")
    ush gnuplot grid0.gpi
end-if

! TEST 11
power cs/grid.byte (1,1,50,50) EXPO=6 PLOTOUT=grid1  +
    title="TEST 11 - RMS POWER SPECTRUN FOR FILE grid.byte"
typetext grid1.asc

if (mode = "nobatch" or mode = "inter")
    ush gnuplot grid1.gpi
end-if

! TEST 12 - should get "??W - Number of samples truncated" 
power cs/grid.byte (1,1,60,60) EXPO=6 PLOTOUT=grid2  +
    title="TEST 12 - RMS POWER SPECTRUN FOR FILE grid.byte"
typetext grid2.asc

if (mode = "nobatch" or mode = "inter")
    ush gnuplot grid2.gpi
end-if

rm>
ush rm cs
let $echo="no"

end-proc
$!-----------------------------------------------------------------------------
$ create tstpower.log.solos
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

translog INP=AFIDS_ROOT TRANS=afidsroot
translog INP=AFIDS_TESTDATA TRANS=aftestdata
if (afidsroot = "")
    ush ln -s /project/test_work/testdata/cassini/iss cs
else
    if (aftestdata = "")
    else
    end-if
end-if
let _onfail="goto rm"
let $echo="no"
TESTS USING AN IMAGE CONTAINING A ONE CYCLE SINE WAVE OVER 256 SAMPLES
THE MEAN IS 100 DN AND THE AMPLITUDE (PEAK-TO-PEAK) IN DN IS 200.
So the first element of the table should be the mean:  100 and the
max value should be about 200 at a frequency of 1/256 = .0039
f2 out=A.h size=(1,1,50,256) 'half func="100*(1+sin(3.1415926*samp/128.))"
Beginning VICAR task f2
F2 version 26-Jul-11
F2 calculating every pixel
FUNCTION EVALUATED 12800 TIMES
power A.h plotfmt=eps plotout=sin1  scale=30 fmax=.1   +
    title="TEST 1 - RMS POWER SPECTRUM FOR FILE sin1"
Beginning VICAR task power
power - Jul 13, 2013 (64-bit gnuplot) - rjb
MIPL RMS POWER SPECTRUM
   TRANSFORM  SL =    1   SS =    1   NL =   50   NS =  256
    256 POINT TRANSFORM
   MEAN (DN) = 100.0000
   SIGMA(DN) =  70.6929
typetext sin1.asc
Beginning VICAR task typetext
  0.0000	   100.000
  0.0039	   199.948
  0.0078	     0.000
  0.0116	     0.032
  0.0155	     0.000
  0.0194	     0.203
  0.0233	     0.000
  0.0271	     0.160
  0.0310	     0.000
  0.0349	     0.111
  0.0388	     0.000
  0.0426	     0.023
  0.0465	     0.000
  0.0504	     0.063
  0.0543	     0.000
  0.0581	     0.079
  0.0620	     0.000
  0.0659	     0.155
  0.0698	     0.000
  0.0736	     0.017
  0.0775	     0.000
  0.0814	     0.075
  0.0853	     0.000
  0.0891	     0.015
  0.0930	     0.000
  0.0969	     0.081
  0.1008	     0.000
  0.1047	     0.206
  0.1085	     0.000
  0.1124	     0.199
  0.1163	     0.000
  0.1202	     0.121
  0.1240	     0.000
  0.1279	     0.128
  0.1318	     0.000
  0.1357	     0.015
  0.1395	     0.000
  0.1434	     0.098
  0.1473	     0.000
  0.1512	     0.179
  0.1550	     0.000
  0.1589	     0.003
  0.1628	     0.000
  0.1667	     0.204
  0.1705	     0.000
  0.1744	     0.028
  0.1783	     0.000
  0.1822	     0.017
  0.1860	     0.000
  0.1899	     0.120
  0.1938	     0.000
  0.1977	     0.076
  0.2016	     0.000
  0.2054	     0.034
  0.2093	     0.000
  0.2132	     0.018
  0.2171	     0.000
  0.2209	     0.093
  0.2248	     0.000
  0.2287	     0.051
  0.2326	     0.000
  0.2364	     0.113
  0.2403	     0.000
  0.2442	     0.108
  0.2481	     0.000
  0.2519	     0.079
  0.2558	     0.000
  0.2597	     0.041
  0.2636	     0.000
  0.2674	     0.013
  0.2713	     0.000
  0.2752	     0.107
  0.2791	     0.000
  0.2829	     0.046
  0.2868	     0.000
  0.2907	     0.106
  0.2946	     0.000
  0.2984	     0.120
  0.3023	     0.000
  0.3062	     0.036
  0.3101	     0.000
  0.3140	     0.035
  0.3178	     0.000
  0.3217	     0.254
  0.3256	     0.000
  0.3295	     0.083
  0.3333	     0.000
  0.3372	     0.105
  0.3411	     0.000
  0.3450	     0.076
  0.3488	     0.000
  0.3527	     0.110
  0.3566	     0.000
  0.3605	     0.061
  0.3643	     0.000
  0.3682	     0.073
  0.3721	     0.000
  0.3760	     0.028
  0.3798	     0.000
  0.3837	     0.254
  0.3876	     0.000
  0.3915	     0.174
  0.3953	     0.000
  0.3992	     0.182
  0.4031	     0.000
  0.4070	     0.034
  0.4109	     0.000
  0.4147	     0.025
  0.4186	     0.000
  0.4225	     0.075
  0.4264	     0.000
  0.4302	     0.099
  0.4341	     0.000
  0.4380	     0.033
  0.4419	     0.000
  0.4457	     0.170
  0.4496	     0.000
  0.4535	     0.099
  0.4574	     0.000
  0.4612	     0.030
  0.4651	     0.000
  0.4690	     0.068
  0.4729	     0.000
  0.4767	     0.081
  0.4806	     0.000
  0.4845	     0.001
  0.4884	     0.000
  0.4922	     0.004
  0.4961	     0.000
    ush gnuplot sin1.eps.gpi
if (mode = "nobatch" or mode = "inter")
end-if
f2 out=A.b1 size=(1,1,50,256) 'byte func="100*(1+sin(3.1415926*samp/128.))"
Beginning VICAR task f2
F2 version 26-Jul-11
F2 calculating every pixel
FUNCTION EVALUATED 12800 TIMES
power A.b1  plotout=sin2 scale=30 fmax=.1   +
    TITLE="TEST 2 - RMS POWER SPECTRUM FOR BYTE FILE sin2"
Beginning VICAR task power
power - Jul 13, 2013 (64-bit gnuplot) - rjb
MIPL RMS POWER SPECTRUM
   TRANSFORM  SL =    1   SS =    1   NL =   50   NS =  256
    256 POINT TRANSFORM
   MEAN (DN) = 100.0000
   SIGMA(DN) =  70.6929
typetext sin2.asc
Beginning VICAR task typetext
  0.0000	   100.000
  0.0039	   199.948
  0.0078	     0.000
  0.0116	     0.032
  0.0155	     0.000
  0.0194	     0.203
  0.0233	     0.000
  0.0271	     0.160
  0.0310	     0.000
  0.0349	     0.111
  0.0388	     0.000
  0.0426	     0.023
  0.0465	     0.000
  0.0504	     0.063
  0.0543	     0.000
  0.0581	     0.079
  0.0620	     0.000
  0.0659	     0.155
  0.0698	     0.000
  0.0736	     0.017
  0.0775	     0.000
  0.0814	     0.075
  0.0853	     0.000
  0.0891	     0.015
  0.0930	     0.000
  0.0969	     0.081
  0.1008	     0.000
  0.1047	     0.206
  0.1085	     0.000
  0.1124	     0.199
  0.1163	     0.000
  0.1202	     0.121
  0.1240	     0.000
  0.1279	     0.128
  0.1318	     0.000
  0.1357	     0.015
  0.1395	     0.000
  0.1434	     0.098
  0.1473	     0.000
  0.1512	     0.179
  0.1550	     0.000
  0.1589	     0.003
  0.1628	     0.000
  0.1667	     0.204
  0.1705	     0.000
  0.1744	     0.028
  0.1783	     0.000
  0.1822	     0.017
  0.1860	     0.000
  0.1899	     0.120
  0.1938	     0.000
  0.1977	     0.076
  0.2016	     0.000
  0.2054	     0.034
  0.2093	     0.000
  0.2132	     0.018
  0.2171	     0.000
  0.2209	     0.093
  0.2248	     0.000
  0.2287	     0.051
  0.2326	     0.000
  0.2364	     0.113
  0.2403	     0.000
  0.2442	     0.108
  0.2481	     0.000
  0.2519	     0.079
  0.2558	     0.000
  0.2597	     0.041
  0.2636	     0.000
  0.2674	     0.013
  0.2713	     0.000
  0.2752	     0.107
  0.2791	     0.000
  0.2829	     0.046
  0.2868	     0.000
  0.2907	     0.106
  0.2946	     0.000
  0.2984	     0.120
  0.3023	     0.000
  0.3062	     0.036
  0.3101	     0.000
  0.3140	     0.035
  0.3178	     0.000
  0.3217	     0.254
  0.3256	     0.000
  0.3295	     0.083
  0.3333	     0.000
  0.3372	     0.105
  0.3411	     0.000
  0.3450	     0.076
  0.3488	     0.000
  0.3527	     0.110
  0.3566	     0.000
  0.3605	     0.061
  0.3643	     0.000
  0.3682	     0.073
  0.3721	     0.000
  0.3760	     0.028
  0.3798	     0.000
  0.3837	     0.254
  0.3876	     0.000
  0.3915	     0.174
  0.3953	     0.000
  0.3992	     0.182
  0.4031	     0.000
  0.4070	     0.034
  0.4109	     0.000
  0.4147	     0.025
  0.4186	     0.000
  0.4225	     0.075
  0.4264	     0.000
  0.4302	     0.099
  0.4341	     0.000
  0.4380	     0.033
  0.4419	     0.000
  0.4457	     0.170
  0.4496	     0.000
  0.4535	     0.099
  0.4574	     0.000
  0.4612	     0.030
  0.4651	     0.000
  0.4690	     0.068
  0.4729	     0.000
  0.4767	     0.081
  0.4806	     0.000
  0.4845	     0.001
  0.4884	     0.000
  0.4922	     0.004
  0.4961	     0.000
if (mode = "nobatch" or mode = "inter")
end-if
let $echo="no"
TRY A SINE WAVE OVER 256 LINES
f2 out=A.b2 size=(1,1,256,50) 'byte func="100*(1+sin(3.1415926*line/128.))"
Beginning VICAR task f2
F2 version 26-Jul-11
F2 calculating every pixel
FUNCTION EVALUATED 12800 TIMES
power A.b2 plotout=sin3 scale=20 fmax=.1  +
    title="TEST 3 - RMS POWER SPECTRUM FOR BYTE FILE sin3"
Beginning VICAR task power
power - Jul 13, 2013 (64-bit gnuplot) - rjb
MIPL RMS POWER SPECTRUM
   TRANSFORM  SL =    1   SS =    1   NL =  256   NS =   32
     32 POINT TRANSFORM
   MEAN (DN) = 100.0000
   SIGMA(DN) =  70.6929
typetext sin3.asc
Beginning VICAR task typetext
  0.0000	   100.000
  0.0294	     0.000
  0.0588	     0.000
  0.0882	     0.000
  0.1176	     0.000
  0.1471	     0.000
  0.1765	     0.000
  0.2059	     0.000
  0.2353	     0.000
  0.2647	     0.000
  0.2941	     0.000
  0.3235	     0.000
  0.3529	     0.000
  0.3824	     0.000
  0.4118	     0.000
  0.4412	     0.000
  0.4706	     0.000
if (mode = "nobatch" or mode = "inter")
end-if
let $echo="no"
HALFWORD DATA TESTS on Cassini data
power cs/sum2.38 (11,11,50,140) plotout=case1   +
    title="TEST 4 - RMS POWER SPECTRUM FOR HALF FILE sum2.38"
Beginning VICAR task power
power - Jul 13, 2013 (64-bit gnuplot) - rjb
MIPL RMS POWER SPECTRUM
   TRANSFORM  SL =   11   SS =   11   NL =   50   NS =  128
    128 POINT TRANSFORM
   MEAN (DN) = 154.9716
   SIGMA(DN) =   3.4179
if (mode = "nobatch" or mode = "inter")
end-if
power cs/sum2.38 (1,1,100,500) plotout=case2 EXPONENT=9 TITLEX="FREQ"   +
    title="TEST 5 - RMS POWER SPECTRUM FOR HALF FILE sum2.38"
Beginning VICAR task power
power - Jul 13, 2013 (64-bit gnuplot) - rjb
MIPL RMS POWER SPECTRUM
   TRANSFORM  SL =    1   SS =    1   NL =  100   NS =  500
    512 POINT TRANSFORM
   MEAN (DN) = 152.2320
   SIGMA(DN) =   5.1071
if (mode = "nobatch" or mode = "inter")
end-if
power cs/sum2.38 (1,1,150,64)  EXPONENT=6 FMAX=2.0  +
    TITLE="TEST 6 - FLORANCE CASE3 RMS POWER SPECTRUM FOR HAF FILE sum2.38"
Beginning VICAR task power
power - Jul 13, 2013 (64-bit gnuplot) - rjb
MIPL RMS POWER SPECTRUM
   TRANSFORM  SL =    1   SS =    1   NL =  150   NS =   64
     64 POINT TRANSFORM
   MEAN (DN) = 153.6343
   SIGMA(DN) =   9.4866
if (mode = "nobatch" or mode = "inter")
end-if
f2 cs/sum2.38 T FUNC="IN1*20."
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 133 TIMES
power T (1,1,150,64)  EXPONENT=6 FMAX=2.0  DNSCALE=20.  +
          plotout=case4 TITLE="TEST 7 - FLORANCE CASE4"
Beginning VICAR task power
power - Jul 13, 2013 (64-bit gnuplot) - rjb
MIPL RMS POWER SPECTRUM
   TRANSFORM  SL =    1   SS =    1   NL =  150   NS =   64
     64 POINT TRANSFORM
   MEAN (DN) = 153.6343
   SIGMA(DN) =   9.4866
if (mode = "nobatch" or mode = "inter")
end-if
power cs/sum2.38 (1,1,120,50) EXPONENT=6 TITLE="TEST 8 - PLOT ONLY - CASE5" plotout=case5
Beginning VICAR task power
power - Jul 13, 2013 (64-bit gnuplot) - rjb
MIPL RMS POWER SPECTRUM
   TRANSFORM  SL =    1   SS =    1   NL =  120   NS =   50
     64 POINT TRANSFORM
   MEAN (DN) = 152.8627
   SIGMA(DN) =  10.5103
if (mode = "nobatch" or mode = "inter")
end-if
ush rm power.gpi
power cs/sum2.38 (1,1,120,50)  plotout=case6   +
    title="TEST 9 - CASE6 RMS POWER SPECTRUM FOR FILE sum2.38"
Beginning VICAR task power
power - Jul 13, 2013 (64-bit gnuplot) - rjb
MIPL RMS POWER SPECTRUM
   TRANSFORM  SL =    1   SS =    1   NL =  120   NS =   32
     32 POINT TRANSFORM
   MEAN (DN) = 151.6779
   SIGMA(DN) =  12.7927
typetext case6.asc
Beginning VICAR task typetext
  0.0000	   151.678
  0.0294	     8.568
  0.0588	     9.138
  0.0882	     9.161
  0.1176	     8.661
  0.1471	     8.746
  0.1765	     9.059
  0.2059	     9.084
  0.2353	     8.957
  0.2647	     8.831
  0.2941	     8.955
  0.3235	     8.869
  0.3529	     8.841
  0.3824	     8.739
  0.4118	     9.127
  0.4412	     8.929
  0.4706	     8.859
if (mode = "nobatch" or mode = "inter")
end-if
let $echo="no"
BYTE DATA TESTS
label-li cs/grid.byte
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File cs/grid.byte ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a VAX-VMS host
                1 bands
                256 lines per band
                50 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: F2 -- User: SXP812 -- Tue Apr 15 09:35:56 1997 ----
FUNCTION='100*(1+sin(3.1415926*line/128.))'
 
************************************************************
power cs/grid.byte (1,1,50,50) EXPONENT=6  TITLE="TEST 10 - BYTE TEST FOR FILE grid.byte"  +
     plotout=grid0
Beginning VICAR task power
power - Jul 13, 2013 (64-bit gnuplot) - rjb
MIPL RMS POWER SPECTRUM
   TRANSFORM  SL =    1   SS =    1   NL =   50   NS =   50
     64 POINT TRANSFORM
   MEAN (DN) = 154.9200
   SIGMA(DN) =  27.7675
if (mode = "nobatch" or mode = "inter")
end-if
power cs/grid.byte (1,1,50,50) EXPO=6 PLOTOUT=grid1   +
    title="TEST 11 - RMS POWER SPECTRUN FOR FILE grid.byte"
Beginning VICAR task power
power - Jul 13, 2013 (64-bit gnuplot) - rjb
MIPL RMS POWER SPECTRUM
   TRANSFORM  SL =    1   SS =    1   NL =   50   NS =   50
     64 POINT TRANSFORM
   MEAN (DN) = 154.9200
   SIGMA(DN) =  27.7675
typetext grid1.asc
Beginning VICAR task typetext
  0.0000	   198.298
  0.0152	     0.000
  0.0303	     0.000
  0.0455	     0.000
  0.0606	     0.000
  0.0758	     0.000
  0.0909	     0.000
  0.1061	     0.000
  0.1212	     0.000
  0.1364	     0.000
  0.1515	     0.000
  0.1667	     0.000
  0.1818	     0.000
  0.1970	     0.000
  0.2121	     0.000
  0.2273	     0.000
  0.2424	     0.000
  0.2576	     0.000
  0.2727	     0.000
  0.2879	     0.000
  0.3030	     0.000
  0.3182	     0.000
  0.3333	     0.000
  0.3485	     0.000
  0.3636	     0.000
  0.3788	     0.000
  0.3939	     0.000
  0.4091	     0.000
  0.4242	     0.000
  0.4394	     0.000
  0.4545	     0.000
  0.4697	     0.000
  0.4848	     0.000
if (mode = "nobatch" or mode = "inter")
end-if
power cs/grid.byte (1,1,60,60) EXPO=6 PLOTOUT=grid2   +
    title="TEST 12 - RMS POWER SPECTRUN FOR FILE grid.byte"
Beginning VICAR task power
power - Jul 13, 2013 (64-bit gnuplot) - rjb
MIPL RMS POWER SPECTRUM
??W - Number of samples truncated
   TRANSFORM  SL =    1   SS =    1   NL =   60   NS =   50
     64 POINT TRANSFORM
   MEAN (DN) = 162.0500
   SIGMA(DN) =  29.9519
typetext grid2.asc
Beginning VICAR task typetext
  0.0000	   207.424
  0.0152	     0.000
  0.0303	     0.000
  0.0455	     0.000
  0.0606	     0.000
  0.0758	     0.000
  0.0909	     0.000
  0.1061	     0.000
  0.1212	     0.000
  0.1364	     0.000
  0.1515	     0.000
  0.1667	     0.000
  0.1818	     0.000
  0.1970	     0.000
  0.2121	     0.000
  0.2273	     0.000
  0.2424	     0.000
  0.2576	     0.000
  0.2727	     0.000
  0.2879	     0.000
  0.3030	     0.000
  0.3182	     0.000
  0.3333	     0.000
  0.3485	     0.000
  0.3636	     0.000
  0.3788	     0.000
  0.3939	     0.000
  0.4091	     0.000
  0.4242	     0.000
  0.4394	     0.000
  0.4545	     0.000
  0.4697	     0.000
  0.4848	     0.000
if (mode = "nobatch" or mode = "inter")
end-if
ush rm cs
let $echo="no"
$!-----------------------------------------------------------------------------
$ create tstpower.log.linux
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

translog INP=AFIDS_ROOT TRANS=afidsroot
translog INP=AFIDS_TESTDATA TRANS=aftestdata
if (afidsroot = "")
    ush ln -s /project/test_work/testdata/cassini/iss cs
else
    if (aftestdata = "")
    else
    end-if
end-if
let _onfail="goto rm"
let $echo="no"
TESTS USING AN IMAGE CONTAINING A ONE CYCLE SINE WAVE OVER 256 SAMPLES
THE MEAN IS 100 DN AND THE AMPLITUDE (PEAK-TO-PEAK) IN DN IS 200.
So the first element of the table should be the mean:  100 and the
max value should be about 200 at a frequency of 1/256 = .0039
f2 out=A.h size=(1,1,50,256) 'half func="100*(1+sin(3.1415926*samp/128.))"
Beginning VICAR task f2
F2 version 26-Jul-11
F2 calculating every pixel
FUNCTION EVALUATED 12800 TIMES
power A.h plotfmt=eps plotout=sin1  scale=30 fmax=.1   +
    title="TEST 1 - RMS POWER SPECTRUM FOR FILE sin1"
Beginning VICAR task power
power - Jul 13, 2013 (64-bit gnuplot) - rjb
MIPL RMS POWER SPECTRUM
   TRANSFORM  SL =    1   SS =    1   NL =   50   NS =  256
    256 POINT TRANSFORM
   MEAN (DN) = 100.0000
   SIGMA(DN) =  70.6929
typetext sin1.asc
Beginning VICAR task typetext
  0.0000	   100.000
  0.0039	   199.948
  0.0078	     0.000
  0.0116	     0.032
  0.0155	     0.000
  0.0194	     0.203
  0.0233	     0.000
  0.0271	     0.160
  0.0310	     0.000
  0.0349	     0.111
  0.0388	     0.000
  0.0426	     0.023
  0.0465	     0.000
  0.0504	     0.063
  0.0543	     0.000
  0.0581	     0.079
  0.0620	     0.000
  0.0659	     0.155
  0.0698	     0.000
  0.0736	     0.017
  0.0775	     0.000
  0.0814	     0.075
  0.0853	     0.000
  0.0891	     0.015
  0.0930	     0.000
  0.0969	     0.081
  0.1008	     0.000
  0.1047	     0.206
  0.1085	     0.000
  0.1124	     0.199
  0.1163	     0.000
  0.1202	     0.121
  0.1240	     0.000
  0.1279	     0.128
  0.1318	     0.000
  0.1357	     0.015
  0.1395	     0.000
  0.1434	     0.098
  0.1473	     0.000
  0.1512	     0.179
  0.1550	     0.000
  0.1589	     0.003
  0.1628	     0.000
  0.1667	     0.204
  0.1705	     0.000
  0.1744	     0.028
  0.1783	     0.000
  0.1822	     0.017
  0.1860	     0.000
  0.1899	     0.120
  0.1938	     0.000
  0.1977	     0.076
  0.2016	     0.000
  0.2054	     0.034
  0.2093	     0.000
  0.2132	     0.018
  0.2171	     0.000
  0.2209	     0.093
  0.2248	     0.000
  0.2287	     0.051
  0.2326	     0.000
  0.2364	     0.113
  0.2403	     0.000
  0.2442	     0.108
  0.2481	     0.000
  0.2519	     0.079
  0.2558	     0.000
  0.2597	     0.041
  0.2636	     0.000
  0.2674	     0.013
  0.2713	     0.000
  0.2752	     0.107
  0.2791	     0.000
  0.2829	     0.046
  0.2868	     0.000
  0.2907	     0.106
  0.2946	     0.000
  0.2984	     0.120
  0.3023	     0.000
  0.3062	     0.036
  0.3101	     0.000
  0.3140	     0.035
  0.3178	     0.000
  0.3217	     0.254
  0.3256	     0.000
  0.3295	     0.083
  0.3333	     0.000
  0.3372	     0.105
  0.3411	     0.000
  0.3450	     0.076
  0.3488	     0.000
  0.3527	     0.110
  0.3566	     0.000
  0.3605	     0.061
  0.3643	     0.000
  0.3682	     0.073
  0.3721	     0.000
  0.3760	     0.028
  0.3798	     0.000
  0.3837	     0.254
  0.3876	     0.000
  0.3915	     0.174
  0.3953	     0.000
  0.3992	     0.182
  0.4031	     0.000
  0.4070	     0.034
  0.4109	     0.000
  0.4147	     0.025
  0.4186	     0.000
  0.4225	     0.075
  0.4264	     0.000
  0.4302	     0.099
  0.4341	     0.000
  0.4380	     0.033
  0.4419	     0.000
  0.4457	     0.170
  0.4496	     0.000
  0.4535	     0.099
  0.4574	     0.000
  0.4612	     0.030
  0.4651	     0.000
  0.4690	     0.068
  0.4729	     0.000
  0.4767	     0.081
  0.4806	     0.000
  0.4845	     0.001
  0.4884	     0.000
  0.4922	     0.004
  0.4961	     0.000
    ush gnuplot sin1.eps.gpi
if (mode = "nobatch" or mode = "inter")
end-if
f2 out=A.b1 size=(1,1,50,256) 'byte func="100*(1+sin(3.1415926*samp/128.))"
Beginning VICAR task f2
F2 version 26-Jul-11
F2 calculating every pixel
FUNCTION EVALUATED 12800 TIMES
power A.b1  plotout=sin2 scale=30 fmax=.1   +
    TITLE="TEST 2 - RMS POWER SPECTRUM FOR BYTE FILE sin2"
Beginning VICAR task power
power - Jul 13, 2013 (64-bit gnuplot) - rjb
MIPL RMS POWER SPECTRUM
   TRANSFORM  SL =    1   SS =    1   NL =   50   NS =  256
    256 POINT TRANSFORM
   MEAN (DN) = 100.0000
   SIGMA(DN) =  70.6929
typetext sin2.asc
Beginning VICAR task typetext
  0.0000	   100.000
  0.0039	   199.948
  0.0078	     0.000
  0.0116	     0.032
  0.0155	     0.000
  0.0194	     0.203
  0.0233	     0.000
  0.0271	     0.160
  0.0310	     0.000
  0.0349	     0.111
  0.0388	     0.000
  0.0426	     0.023
  0.0465	     0.000
  0.0504	     0.063
  0.0543	     0.000
  0.0581	     0.079
  0.0620	     0.000
  0.0659	     0.155
  0.0698	     0.000
  0.0736	     0.017
  0.0775	     0.000
  0.0814	     0.075
  0.0853	     0.000
  0.0891	     0.015
  0.0930	     0.000
  0.0969	     0.081
  0.1008	     0.000
  0.1047	     0.206
  0.1085	     0.000
  0.1124	     0.199
  0.1163	     0.000
  0.1202	     0.121
  0.1240	     0.000
  0.1279	     0.128
  0.1318	     0.000
  0.1357	     0.015
  0.1395	     0.000
  0.1434	     0.098
  0.1473	     0.000
  0.1512	     0.179
  0.1550	     0.000
  0.1589	     0.003
  0.1628	     0.000
  0.1667	     0.204
  0.1705	     0.000
  0.1744	     0.028
  0.1783	     0.000
  0.1822	     0.017
  0.1860	     0.000
  0.1899	     0.120
  0.1938	     0.000
  0.1977	     0.076
  0.2016	     0.000
  0.2054	     0.034
  0.2093	     0.000
  0.2132	     0.018
  0.2171	     0.000
  0.2209	     0.093
  0.2248	     0.000
  0.2287	     0.051
  0.2326	     0.000
  0.2364	     0.113
  0.2403	     0.000
  0.2442	     0.108
  0.2481	     0.000
  0.2519	     0.079
  0.2558	     0.000
  0.2597	     0.041
  0.2636	     0.000
  0.2674	     0.013
  0.2713	     0.000
  0.2752	     0.107
  0.2791	     0.000
  0.2829	     0.046
  0.2868	     0.000
  0.2907	     0.106
  0.2946	     0.000
  0.2984	     0.120
  0.3023	     0.000
  0.3062	     0.036
  0.3101	     0.000
  0.3140	     0.035
  0.3178	     0.000
  0.3217	     0.254
  0.3256	     0.000
  0.3295	     0.083
  0.3333	     0.000
  0.3372	     0.105
  0.3411	     0.000
  0.3450	     0.076
  0.3488	     0.000
  0.3527	     0.110
  0.3566	     0.000
  0.3605	     0.061
  0.3643	     0.000
  0.3682	     0.073
  0.3721	     0.000
  0.3760	     0.028
  0.3798	     0.000
  0.3837	     0.254
  0.3876	     0.000
  0.3915	     0.174
  0.3953	     0.000
  0.3992	     0.182
  0.4031	     0.000
  0.4070	     0.034
  0.4109	     0.000
  0.4147	     0.025
  0.4186	     0.000
  0.4225	     0.075
  0.4264	     0.000
  0.4302	     0.099
  0.4341	     0.000
  0.4380	     0.033
  0.4419	     0.000
  0.4457	     0.170
  0.4496	     0.000
  0.4535	     0.099
  0.4574	     0.000
  0.4612	     0.030
  0.4651	     0.000
  0.4690	     0.068
  0.4729	     0.000
  0.4767	     0.081
  0.4806	     0.000
  0.4845	     0.001
  0.4884	     0.000
  0.4922	     0.004
  0.4961	     0.000
if (mode = "nobatch" or mode = "inter")
end-if
let $echo="no"
TRY A SINE WAVE OVER 256 LINES
f2 out=A.b2 size=(1,1,256,50) 'byte func="100*(1+sin(3.1415926*line/128.))"
Beginning VICAR task f2
F2 version 26-Jul-11
F2 calculating every pixel
FUNCTION EVALUATED 12800 TIMES
power A.b2 plotout=sin3 scale=20 fmax=.1  +
    title="TEST 3 - RMS POWER SPECTRUM FOR BYTE FILE sin3"
Beginning VICAR task power
power - Jul 13, 2013 (64-bit gnuplot) - rjb
MIPL RMS POWER SPECTRUM
   TRANSFORM  SL =    1   SS =    1   NL =  256   NS =   32
     32 POINT TRANSFORM
   MEAN (DN) = 100.0000
   SIGMA(DN) =  70.6929
typetext sin3.asc
Beginning VICAR task typetext
  0.0000	   100.000
  0.0294	     0.000
  0.0588	     0.000
  0.0882	     0.000
  0.1176	     0.000
  0.1471	     0.000
  0.1765	     0.000
  0.2059	     0.000
  0.2353	     0.000
  0.2647	     0.000
  0.2941	     0.000
  0.3235	     0.000
  0.3529	     0.000
  0.3824	     0.000
  0.4118	     0.000
  0.4412	     0.000
  0.4706	     0.000
if (mode = "nobatch" or mode = "inter")
end-if
let $echo="no"
HALFWORD DATA TESTS on Cassini data
power cs/sum2.38 (11,11,50,140) plotout=case1   +
    title="TEST 4 - RMS POWER SPECTRUM FOR HALF FILE sum2.38"
Beginning VICAR task power
power - Jul 13, 2013 (64-bit gnuplot) - rjb
MIPL RMS POWER SPECTRUM
   TRANSFORM  SL =   11   SS =   11   NL =   50   NS =  128
    128 POINT TRANSFORM
   MEAN (DN) = 154.9716
   SIGMA(DN) =   3.4178
if (mode = "nobatch" or mode = "inter")
end-if
power cs/sum2.38 (1,1,100,500) plotout=case2 EXPONENT=9 TITLEX="FREQ"   +
    title="TEST 5 - RMS POWER SPECTRUM FOR HALF FILE sum2.38"
Beginning VICAR task power
power - Jul 13, 2013 (64-bit gnuplot) - rjb
MIPL RMS POWER SPECTRUM
   TRANSFORM  SL =    1   SS =    1   NL =  100   NS =  500
    512 POINT TRANSFORM
   MEAN (DN) = 152.2320
   SIGMA(DN) =   5.1070
if (mode = "nobatch" or mode = "inter")
end-if
power cs/sum2.38 (1,1,150,64)  EXPONENT=6 FMAX=2.0  +
    TITLE="TEST 6 - FLORANCE CASE3 RMS POWER SPECTRUM FOR HAF FILE sum2.38"
Beginning VICAR task power
power - Jul 13, 2013 (64-bit gnuplot) - rjb
MIPL RMS POWER SPECTRUM
   TRANSFORM  SL =    1   SS =    1   NL =  150   NS =   64
     64 POINT TRANSFORM
   MEAN (DN) = 153.6343
   SIGMA(DN) =   9.4866
if (mode = "nobatch" or mode = "inter")
end-if
f2 cs/sum2.38 T FUNC="IN1*20."
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 133 TIMES
power T (1,1,150,64)  EXPONENT=6 FMAX=2.0  DNSCALE=20.  +
          plotout=case4 TITLE="TEST 7 - FLORANCE CASE4"
Beginning VICAR task power
power - Jul 13, 2013 (64-bit gnuplot) - rjb
MIPL RMS POWER SPECTRUM
   TRANSFORM  SL =    1   SS =    1   NL =  150   NS =   64
     64 POINT TRANSFORM
   MEAN (DN) = 153.6343
   SIGMA(DN) =   9.4866
if (mode = "nobatch" or mode = "inter")
end-if
power cs/sum2.38 (1,1,120,50) EXPONENT=6 TITLE="TEST 8 - PLOT ONLY - CASE5" plotout=case5
Beginning VICAR task power
power - Jul 13, 2013 (64-bit gnuplot) - rjb
MIPL RMS POWER SPECTRUM
   TRANSFORM  SL =    1   SS =    1   NL =  120   NS =   50
     64 POINT TRANSFORM
   MEAN (DN) = 152.8627
   SIGMA(DN) =  10.5103
if (mode = "nobatch" or mode = "inter")
end-if
ush rm power.gpi
power cs/sum2.38 (1,1,120,50)  plotout=case6   +
    title="TEST 9 - CASE6 RMS POWER SPECTRUM FOR FILE sum2.38"
Beginning VICAR task power
power - Jul 13, 2013 (64-bit gnuplot) - rjb
MIPL RMS POWER SPECTRUM
   TRANSFORM  SL =    1   SS =    1   NL =  120   NS =   32
     32 POINT TRANSFORM
   MEAN (DN) = 151.6779
   SIGMA(DN) =  12.7927
typetext case6.asc
Beginning VICAR task typetext
  0.0000	   151.678
  0.0294	     8.568
  0.0588	     9.138
  0.0882	     9.161
  0.1176	     8.661
  0.1471	     8.746
  0.1765	     9.059
  0.2059	     9.084
  0.2353	     8.957
  0.2647	     8.831
  0.2941	     8.955
  0.3235	     8.869
  0.3529	     8.841
  0.3824	     8.739
  0.4118	     9.127
  0.4412	     8.929
  0.4706	     8.859
if (mode = "nobatch" or mode = "inter")
end-if
let $echo="no"
BYTE DATA TESTS
label-li cs/grid.byte
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File cs/grid.byte ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a VAX-VMS host
                1 bands
                256 lines per band
                50 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: F2 -- User: SXP812 -- Tue Apr 15 09:35:56 1997 ----
FUNCTION='100*(1+sin(3.1415926*line/128.))'
 
************************************************************
power cs/grid.byte (1,1,50,50) EXPONENT=6  TITLE="TEST 10 - BYTE TEST FOR FILE grid.byte"  +
     plotout=grid0
Beginning VICAR task power
power - Jul 13, 2013 (64-bit gnuplot) - rjb
MIPL RMS POWER SPECTRUM
   TRANSFORM  SL =    1   SS =    1   NL =   50   NS =   50
     64 POINT TRANSFORM
   MEAN (DN) = 154.9200
   SIGMA(DN) =  27.7675
if (mode = "nobatch" or mode = "inter")
end-if
power cs/grid.byte (1,1,50,50) EXPO=6 PLOTOUT=grid1   +
    title="TEST 11 - RMS POWER SPECTRUN FOR FILE grid.byte"
Beginning VICAR task power
power - Jul 13, 2013 (64-bit gnuplot) - rjb
MIPL RMS POWER SPECTRUM
   TRANSFORM  SL =    1   SS =    1   NL =   50   NS =   50
     64 POINT TRANSFORM
   MEAN (DN) = 154.9200
   SIGMA(DN) =  27.7675
typetext grid1.asc
Beginning VICAR task typetext
  0.0000	   198.298
  0.0152	     0.000
  0.0303	     0.000
  0.0455	     0.000
  0.0606	     0.000
  0.0758	     0.000
  0.0909	     0.000
  0.1061	     0.000
  0.1212	     0.000
  0.1364	     0.000
  0.1515	     0.000
  0.1667	     0.000
  0.1818	     0.000
  0.1970	     0.000
  0.2121	     0.000
  0.2273	     0.000
  0.2424	     0.000
  0.2576	     0.000
  0.2727	     0.000
  0.2879	     0.000
  0.3030	     0.000
  0.3182	     0.000
  0.3333	     0.000
  0.3485	     0.000
  0.3636	     0.000
  0.3788	     0.000
  0.3939	     0.000
  0.4091	     0.000
  0.4242	     0.000
  0.4394	     0.000
  0.4545	     0.000
  0.4697	     0.000
  0.4848	     0.000
if (mode = "nobatch" or mode = "inter")
end-if
power cs/grid.byte (1,1,60,60) EXPO=6 PLOTOUT=grid2   +
    title="TEST 12 - RMS POWER SPECTRUN FOR FILE grid.byte"
Beginning VICAR task power
power - Jul 13, 2013 (64-bit gnuplot) - rjb
MIPL RMS POWER SPECTRUM
??W - Number of samples truncated
   TRANSFORM  SL =    1   SS =    1   NL =   60   NS =   50
     64 POINT TRANSFORM
   MEAN (DN) = 162.0500
   SIGMA(DN) =  29.9519
typetext grid2.asc
Beginning VICAR task typetext
  0.0000	   207.424
  0.0152	     0.000
  0.0303	     0.000
  0.0455	     0.000
  0.0606	     0.000
  0.0758	     0.000
  0.0909	     0.000
  0.1061	     0.000
  0.1212	     0.000
  0.1364	     0.000
  0.1515	     0.000
  0.1667	     0.000
  0.1818	     0.000
  0.1970	     0.000
  0.2121	     0.000
  0.2273	     0.000
  0.2424	     0.000
  0.2576	     0.000
  0.2727	     0.000
  0.2879	     0.000
  0.3030	     0.000
  0.3182	     0.000
  0.3333	     0.000
  0.3485	     0.000
  0.3636	     0.000
  0.3788	     0.000
  0.3939	     0.000
  0.4091	     0.000
  0.4242	     0.000
  0.4394	     0.000
  0.4545	     0.000
  0.4697	     0.000
  0.4848	     0.000
if (mode = "nobatch" or mode = "inter")
end-if
ush rm cs
let $echo="no"
$ Return
$!#############################################################################
