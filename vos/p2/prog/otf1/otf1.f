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

