$!****************************************************************************
$!
$! Build proc for MIPL module ccdrecip
$! VPACK Version 1.9, Thursday, October 24, 2013, 16:30:41
$!
$! Execute by entering:		$ @ccdrecip
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
$ write sys$output "*** module ccdrecip ***"
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
$ write sys$output "Invalid argument given to ccdrecip.com file -- ", primary
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
$   if F$SEARCH("ccdrecip.imake") .nes. ""
$   then
$      vimake ccdrecip
$      purge ccdrecip.bld
$   else
$      if F$SEARCH("ccdrecip.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake ccdrecip
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @ccdrecip.bld "STD"
$   else
$      @ccdrecip.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create ccdrecip.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack ccdrecip.com -mixed -
	-s ccdrecip.f -
	-i ccdrecip.imake -
	-p ccdrecip.pdf -
	-t tstccdrecip.pdf tstccdrecip.log_solos
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ccdrecip.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
C VICAR PROGRAM CCDRECIP
C
C Radiometric calibration routine:  Determines the camera sensitivity
C (in DN per foot-lambert-milliseconds or in DN per picoamp-milliseconds
C and shutter offset (in msec) for a CCD or vidicon
C camera system.
C   UNITS
c	15      LTFILE
c	IUNI	INP
c	66	UNCORR	Sensitivity values
C	67      CORR  Sensitivity values
C	ISO 	Shutter Offsets`
C	OUNI	OUT
C	11	Output table
c	12      Offsets
C	13 	AVG
C	18      gnuplot .asc "b" file
C	19      gnuplot .asc file

      SUBROUTINE MAIN44  

	implicit none
         COMMON/CP/VMES,ARMES,RMSG,LABEL

         REAL*8 DC(400),DN(30,400),AO(400),TOS(400)
         REAL*8 W(30),AVDN(30),DENOM,DNSUM
         REAL*8 SWX(400),SWXT(400),SX2(400)
         REAL*8 SW,SWT,SWT2
         REAL*4 LIGHT(30),LYAV(40),LXAV(40),YTOS(402),XTOS(402)
         REAL*4 SHUTTER_OFFSETS(1024), LORS_NUMBERS(1024)
         REAL*4 AVTOS(40),COLU(30,40),COLV(30,40),LT,LTC
         REAL*4 OUT(4320), OUTM(2,2000), EXPOS(30), BEXPO
         REAL*4 AVGOS,EXPO,U,V,X

         INTEGER*4 ATBL,OTBL,CTBL,VTBL,ASIZ
         INTEGER*4 SL,SS,SLI,SSI,NLI,NSI,NLI2,NSI2  ! size variables
         INTEGER*4 OUNI,STAT,CNT,GAREA,SIGTOL,BAD(400)
         INTEGER*4 AREA(4,400), NI
c         INTEGER*4 STATUS
         INTEGER*4 NPTS,NEXP,I,IB,ICNT,IDEF,IND,IPLOT,IREJCT,ISO,IUNI,IX
         INTEGER*4 J,JST,K,L,LCNT,N,NAREA,NCOL,NGOOD,NGR,NL,NLAB,NLO
         INTEGER*4 NLUM,NROW,NS,NSO,NUM,ninpfileb,ntblb
         INTEGER*4 nplotgpi,nplotgpi2,nploteps,ntbl,ninpfile,nplotname

         LOGICAL*4 epsplot

         CHARACTER*63 plotname
         character*80 plotgpi,plotgpi2,ploteps,tbl,tblb
         CHARACTER*256 PLOT_DS,OFFSET_FILE,inpfile,inpfileb,cbuf
         CHARACTER*40 CORFIL(2),ATFIL,OFFIL,AVFIL,LFILE
         CHARACTER*4 DIRECTN
         CHARACTER*8 UNITS,pformat
         CHARACTER*70 MS1, MS2
         CHARACTER*39 LUMMSG
         CHARACTER*34 RADMSG
         CHARACTER*1 TAB

C        global variables
         CHARACTER*54 VMES
         CHARACTER*4320 LABEL
         CHARACTER*51 ARMES
         CHARACTER*41 RMSG  

	character*4 gpi/'.gpi'/,eps/'.eps'/,asc/'.asc'/
	character*1 bee/'b'/

         TAB = CHAR(9)

        epsplot = .false.
        nplotgpi = 0
        nplotgpi2 = 0
        nplotname = 0
        nploteps = 0
        ntbl = 0
        ninpfile = 0
      
         VMES(1:54)=' '
         ARMES = 'NUMBER OF GOOD AREAS=**** OUT OF**** AREAS SAMPLED'
         RMSG = 'NUMBER REJECTED FOR               =     '

         MS1 = ' '
         MS2 = ' '
         MS2(1:26)='AREA   SL   SS   NL   NS  '
         MS2(27:47)='A0 (DN/ENERGY UNIT)  '
         MS2(48:65)='TOS (MILLISECONDS)'
         LUMMSG(1:39)='ENERGY UNIT = FOOT-LAMBERT-MILLISECONDS'
         RADMSG(1:34)='ENERGY UNIT = PICOAMP-MILLISECONDS'

         CALL IFMESSAGE ('CCDRECIP VERSION 06-Jul-2013')

        call xvparm('INP',cbuf,cnt,idef,1)
          inpfile = cbuf
          ninpfile=index(inpfile,'   ') - 1
	  inpfileb = inpfile(1:ninpfile)//bee
	  ninpfileb=index(inpfileb,'   ') - 1

         CALL XVP('SIGTOL',SIGTOL,CNT)
         CALL XVPARM('PLOT',PLOT_DS,IPLOT,IDEF,1)
         IF (IDEF.EQ.0) THEN
               plotname = PLOT_DS
               nplotname=index(plotname,'   ') - 1
               plotgpi=plotname(1:nplotname)//gpi
               nplotgpi=index(plotgpi,'  ') - 1
               plotgpi2=plotname(1:nplotname)//eps//gpi
               nplotgpi2=index(plotgpi2,'  ') - 1
               ploteps=plotname(1:nplotname)//eps
               nploteps=index(ploteps,'  ') - 1
               tbl = plotname(1:nplotname)//asc
               ntbl = index(tbl,'  ') - 1
		tblb = plotname(1:nplotname)//'.'//bee//asc
	       ntblb = index(tblb,'  ') - 1
ccc            CALL PLOTFN(PLOT_DS)
ccc            CALL XRTBEGIN(STATUS)
ccc            IF (STATUS.NE.1) CALL MABEND('Unable to OPEN plotter')
ccc            CALL DISPLAYAXES(1,1,0)! x,y1,y2 axes displayed 1=yes 0=no
ccc            CALL SETACTIVESET(1)   ! endpts on lines 0=no triangles, 1=triangles
            IPLOT = 1
         ELSE
            IPLOT = 0
         ENDIF

         call xvp ('PLOTFMT',pformat,cnt)
         if (pformat .eq. 'eps' .or. pformat .eq. 'EPS') epsplot = .true.

         CALL XVP('REJECT',IREJCT,CNT)

C-------get light data from file or parameters
	 CALL XVPARM('LTFILE',LFILE,ICNT,IDEF,1)
	 IF (ICNT .EQ. 1) THEN
	    OPEN(15,FILE=LFILE,STATUS='OLD',IOSTAT=JST,ERR=996)
	    DO J=1,30
	       READ(15,7,END=6) LIGHT(J)
	       NLUM=J
	    END DO
            CLOSE(15)
    6       CALL PRNT(4,1,NLUM,' LIGHT FROM FILE=.')
	 ELSE
	    CALL XVP('LIGHT',LIGHT,LCNT)
            NLUM = LCNT
	 END IF
	
    7    FORMAT(G20.12)

C-------any light entered?
	 IF (LCNT .EQ. 0 .AND. NLUM .EQ. 0) THEN
	    CALL XVMESSAGE('??E - NO LIGHT DATA INPUT',' ')
	    GO TO 999
	 END IF
	
	 CALL XVPARM('DIRECTIO',DIRECTN,ICNT,IDEF,1) 
         CALL XVPARM('ARRAYSIZ',ASIZ,ICNT,IDEF,1)
	 CALL XVPARM('AREATBL', ATFIL,ATBL,IDEF,1)
	 CALL XVPARM('OFFTBL',  OFFIL,OTBL,IDEF,1)
	 CALL XVPARM('AVOFFTBL',AVFIL,VTBL,IDEF,1)
	 CALL XVPARM('CORRTBL',CORFIL,CTBL,IDEF,2)
	 CALL XVPARM('UNITS',UNITS,ICNT,IDEF,1)

         CALL XVUNIT(IUNI,'INP',1,STAT,' ')
         CALL XVOPEN(IUNI,STAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
         CALL XVSIZE(SLI,SSI,NLI,NSI,NLI2,NSI2)
         CALL LABPROC(IUNI,NLAB,LABEL)

C        Read in area size fields...
         CALL XLGET (IUNI,'HISTORY','NUM_AREAS',NAREA,STAT,
     &               'FORMAT','INT','HIST','LTGEN',' ')
         CALL XLGET (IUNI,'HISTORY','AREAS',AREA,STAT,'NELEMENT',
     &               4*NAREA,'FORMAT','INT','HIST','LTGEN',' ')

C        Rectangular array of areas is expected, so determine number of columns
	NCOL = 0
         DO 111 I=2,NAREA
            if (area(2,i) .lt. area(2,i-1) ) then
               ncol=i-1
               go to 112
            end if
  111    CONTINUE

  112    CONTINUE   ! exit the loop

         NROW = NAREA/NCOL

         IF (NCOL*NROW.NE.NAREA) GOTO 998
         SW=0.0D0
         SWT=0.0D0
         SWT2=0.0D0

         DO I=1,NAREA
            SWX(I)=0.0D0
            SWXT(I)=0.0D0
            SX2(I)=0.0D0
         ENDDO

         CALL XLGET (IUNI,'HISTORY','NUM_EXPOS',NEXP,STAT,
     &               'FORMAT','INT','HIST','LTGEN',' ')
         CALL XLGET (IUNI,'HISTORY','EXPOSURES',EXPOS,STAT,
     &               'NELEMENT',NEXP,'FORMAT','REAL', 
     &               'HIST','LTGEN',' ')


C        Read data and compute weighted sums...
         DO 50 L = 1,NLI                     !Loop through each exposure...
            CALL XVREAD (IUNI,OUT,STAT,'LINE',L,'NSAMPS',NSI,' ')
            NI = NINT (OUT(1))
            IF (NI .EQ. 0) THEN
               BEXPO = EXPOS(L)
               GOTO 970
            ENDIF
            IF (L .GT. 1) THEN
               W(L-1) = DBLE(LIGHT(L)**2)          ! weights
               SW = SW + W(L-1)                         ! sum weights
               SWT = SWT + W(L-1)*DBLE(EXPOS(L))        ! sum weights * time
               SWT2 = SWT2 + W(L-1)*DBLE(EXPOS(L)**2)
            ENDIF
            IB = 0

            DO 50 K = 1,NAREA                    !Area loop
               SL = AREA(1,K)
               SS = AREA(2,K)
               NL = AREA(3,K)
               NS = AREA(4,K)
               N = NI*NL*NS                   !Total # pixels area*NI
               DNSUM = 0.0D0
               DO I = 1,NI
                  DNSUM = DNSUM + DBLE(OUT(IB+I+1))    !Sum dn across inputs
               ENDDO


               IF (L .EQ. 1) THEN
                  DC(K) = DNSUM/N             !Average Dark Current
               ELSE
                  DN(L-1,K) = DNSUM/N - DC(K)   !Average DN
                  X = DN(L-1,K)/LIGHT(L)
                  SWX(K) = SWX(K) + W(L-1)*DBLE(X)
                  SWXT(K) = SWXT(K) + W(L-1)*DBLE(X)*DBLE(EXPOS(L))
                  SX2(K) = SX2(K) + DBLE(X)**2
               ENDIF
   50    IB = IB + 3*NI

         CALL XVMESSAGE (' ',' ')
         DENOM = SWT**2 - SW*SWT2

C        Compute shutter offset and sensitivity for each area....
         IF (UNITS .EQ. 'RADIANCE') THEN
            CALL XVMESSAGE (RADMSG,' ')
         ELSE
            CALL XVMESSAGE (LUMMSG,' ')
         ENDIF
         CALL XVMESSAGE (' ',' ')
         CALL XVMESSAGE (MS2,' ')
         DO K=1,NAREA
            SL = AREA(1,K)
            SS = AREA(2,K)
            NL = AREA(3,K)
            NS = AREA(4,K)

C           calculate camera sensitivity  
            AO(K) = (SWX(K)*SWT-SWXT(K)*SW)/DENOM          

C           calculate shutter-offset 
            TOS(K) = (AO(K)*SWT-SWX(K))/(AO(K)*SW)

C           output AREA results 
            WRITE (MS1,
     &         '(I4,A1,I4,A1,I4,A1,I4,A1,I4,A5,F12.5,A9,F12.5)')
     &         K,' ',SL,' ',SS,' ',NL,' ',NS,'     ',AO(K),
     &         '         ',TOS(K)
            CALL XVMESSAGE (MS1,' ')
         ENDDO

C        Weed out bad areas...
         CALL ZIA(BAD,NAREA)
         IF (NAREA.GT.1) THEN
            CALL XVMESSAGE (' ',' ')
            CALL XVMESSAGE ('Global value for A0...',' ')
            CALL IMEAN(AO,1,NAREA,BAD,SIGTOL)
            CALL XVMESSAGE (' ',' ')
            CALL XVMESSAGE ('Global shutter offset...',' ')
            CALL IMEAN(TOS,2,NAREA,BAD,SIGTOL)
         ENDIF
         CALL XVMESSAGE (' ',' ')

C        Print out all bad areas...
         DO 100 K=1,NAREA
            IF (BAD(K).EQ.0) GOTO 100
            SL = AREA(1,K)
            SS = AREA(2,K)
            NL = AREA(3,K)
            NS = AREA(4,K)
            WRITE (MS1,'(A5,I3,A16,I4,A1,I4,A1,I4,A1,I4,A1)') 'AREA ',
     &             K,' (SL,SS,NL,NS)=(',SL,',',SS,',',NL,',',NS,')'
            IF (BAD(K).EQ.1) MS1(47:70)='***BAD SENSITIVITY******'
            IF (BAD(K).EQ.2) MS1(47:70)='***BAD SHUTTER OFFSET***'
            IF (BAD(K).EQ.4) MS1(47:70)='***BOTH BAD FIT*********'
            CALL XVMESSAGE (MS1,' ')
  100    CONTINUE

C        Get average DN at each exposure level (using only the
C        good areas)....
C         NPTS = NLI - 2		!Number of exposures (excluding DC)
         NPTS = NLI - 1                !Number of exposures (excluding DC)
         GAREA = 0			!Number of good areas
         CALL ZIA(AVDN,2*NPTS)	!Zero out averages
C
         DO 105 K=1,NAREA
            IF ((IREJCT .EQ. 1) .AND. 
     &          ((BAD(K) .EQ. 1) .OR. (BAD(K) .EQ. 4))) GOTO 105
            IF ((IREJCT .EQ. 2) .AND. 
     &          ((BAD(K) .EQ. 2) .OR. (BAD(K) .EQ. 4))) GOTO 105
            IF ((IREJCT .EQ. 3) .AND. (BAD(K) .NE. 0)) GOTO 105
            GAREA = GAREA + 1
            DO J=1,NPTS
               AVDN(J) = AVDN(J) + DN(J,K)
            ENDDO
  105    CONTINUE

         DO J=1,NPTS
            AVDN(J) = AVDN(J)/GAREA
         ENDDO

         CALL XVMESSAGE (' ',' ')
         IF (IREJCT.EQ.1) RMSG(21:34)='SENSITIVITY   '
         IF (IREJCT.EQ.2) RMSG(21:34)='SHUTTER OFFSET'
         IF (IREJCT.EQ.3) RMSG(21:34)='BOTH          '
         WRITE (RMSG(38:40),'(I3)') NAREA-GAREA
   	 IF (IREJCT.EQ.0) THEN
            RMSG(1:41)='NO REJECTION CRITERIA APPLIED            '
	 ENDIF
         CALL RTIMES(NPTS,LIGHT,EXPOS,AVDN,IPLOT,NLAB,
     1        NAREA,GAREA,UNITS,ATBL,ATFIL,plotgpi,nplotgpi,plotgpi2,
     2        nplotgpi2,epsplot,ploteps,nploteps,tbl,ntbl,tblb,ntblb,*999)
c`
cc         CALL RTIMES(NPTS,LIGHT(2),EXPOS(2),AVDN,IPLOT,NLAB,
cc     &               NAREA,GAREA,UNITS,ATBL,ATFIL,*999)
C-------------------------------------------------------
c	print *,'DIRECTN = ',directn
         IF (DIRECTN .EQ. 'LINE') THEN
C-----------    LINE DIRECTION    ---------
C              Get average offset for each row...
            NGR = 0			!Number of good rows

            DO 120 I=1,NROW
               AVGOS = 0.0		!Average offset for row I
               NGOOD = 0		!Number of good areas on row

               DO 110 J=1,NCOL
                  K = NCOL*(I-1) + J
                  IF ((IREJCT .EQ. 1) .AND. 
     &                ((BAD(K) .EQ. 1) .OR. (BAD(K) .EQ. 4))) GOTO 110
                  IF ((IREJCT .EQ. 2) .AND. 
     &                ((BAD(K) .EQ. 2) .OR. (BAD(K) .EQ. 4))) GOTO 110
                  IF ((IREJCT .EQ. 3) .AND. (BAD(K) .NE. 0)) GOTO 110
                  SL = AREA(1,K)
                  SS = AREA(2,K)
                  NL = AREA(3,K)
                  NS = AREA(4,K)
                  XTOS(K) = SL + NL/2
                  YTOS(K) = TOS(K)
                  AVGOS = AVGOS + TOS(K)
                  NGOOD = NGOOD + 1
  110          CONTINUE

               IF (NGOOD.GT.0) THEN
                  NGR = NGR + 1
                  LXAV(NGR) = SL + NL/2
                  LYAV(NGR) = AVGOS/NGOOD
               ENDIF
  120       CONTINUE
	 ELSE
C-----------    SAMPLE DIRECTION    ---------
C           Get average offset for each column...
            NGR = 0                     !Number of good cols

            DO 140 I=1,NCOL
               AVGOS = 0.0              !Average offset for col I
               NGOOD = 0                !Number of good areas on col

               DO 130 J=1,NROW
                  K = NCOL*(J-1) + I
                  IF ((IREJCT .EQ. 1) .AND. 
     &                ((BAD(K) .EQ. 1) .OR. (BAD(K) .EQ. 4))) GOTO 130
                  IF ((IREJCT .EQ. 2) .AND. 
     &                ((BAD(K) .EQ. 2) .OR. (BAD(K) .EQ. 4))) GOTO 130
                  IF ((IREJCT .EQ. 3) .AND. (BAD(K) .NE. 0)) GOTO 130
                  SL = AREA(1,K)
                  SS = AREA(2,K)
                  NL = AREA(3,K)
                  NS = AREA(4,K)
                  XTOS(K) = SS + NS/2
                  YTOS(K) = TOS(K)
                  AVGOS = AVGOS + TOS(K)
                  NGOOD = NGOOD + 1
  130          CONTINUE
               IF (NGOOD.GT.0) THEN
                  NGR = NGR + 1
                  LXAV(NGR) = SS + NS/2
                  LYAV(NGR) = AVGOS/NGOOD
                  AVTOS(I) = LYAV(NGR)      !save by column number
               ENDIF
  140       CONTINUE
	 ENDIF

         IF (CTBL .EQ. 2) THEN
C-------open table of uncorrected sensitivity values by grid column
            open(66,file=corfil(1),status='UNKNOWN',IOSTAT=JST,ERR=999)
C-------open table of corrected sensitivity values by grid column
            open(67,file=corfil(2),status='UNKNOWN',IOSTAT=JST,ERR=999)
            IF (DIRECTN .EQ. 'SAMP') THEN

C-----------  averaging grid columns    ---------
               num = ncol
               do ix=2,nlum
                  lt=light(ix)*expos(ix)
                  do i=1,ncol
                     ltc=light(ix)*(expos(ix) - avtos(i))
                     do j=1,nrow
       	                k=ncol*(j-1)+i
        	        u=dn(ix-1,k)/lt
       	                v=dn(ix-1,k)/ltc
       	                colu(ix-1,i) = colu(ix-1,i) + u
       	                colv(ix-1,i) = colv(ix-1,i) + v
                     enddo
                     colu(ix-1,i) = colu(ix-1,i)/nrow
                     colv(ix-1,i) = colv(ix-1,i)/nrow
                  enddo
                  write(66,661) expos(ix),(TAB,colu(ix-1,i),i=1,ncol)
                  write(67,661) expos(ix),(TAB,colv(ix-1,i),i=1,ncol)
               enddo
            ELSE
C------- averaging grid rows
	       num = nrow
               do ix=2,nlum
                  lt=light(ix)*expos(ix)
                  do i=1,nrow
                     ltc=light(ix)*(expos(ix) - avtos(i))
                     do j=1,ncol
                        k=nrow*(j-1)+i
                        u=dn(ix-1,k)/lt
                        v=dn(ix-1,k)/ltc
                        colu(ix-1,i) = colu(ix-1,i) + u
                        colv(ix-1,i) = colv(ix-1,i) + v
                     enddo
                     colu(ix-1,i) = colu(ix-1,i)/ncol
                     colv(ix-1,i) = colv(ix-1,i)/ncol
                  enddo
                  write(66,661) expos(ix),(TAB,colu(ix-1,i),i=1,nrow)
                  write(67,661) expos(ix),(TAB,colv(ix-1,i),i=1,nrow)
               enddo
            ENDIF 

  661       format(1x,F5.0,40(A1,F11.6)) ! the 40 is # of cols in COLU and COLV

            close(66)
            close(67)
         ENDIF
C-------------------------------------------------------
C-----Note:  Now, the xtos and ytos are 0.0 for bad areas
C-------------------------------------------------------
         IF (IPLOT.EQ.1) THEN
            CALL TPLOT(NLAB,YTOS,XTOS,GAREA,LYAV,LXAV,NGR,directn,
     1        asiz,plotgpi,nplotgpi,plotgpi2,
     2        nplotgpi2,epsplot,ploteps,nploteps,tbl,ntbl,tblb,ntblb)

ccc            CALL PLOT(0,0,999)
            CALL XVCLOSE(IUNI,STAT,' ')
         ENDIF

C-------Write out tables of 1) line/samp number vs. offset (all good areas)
C-------                    2) avg line/samp num. vs. avg. offset 
         if (otbl .eq. 1)
     &      call tbloff(xtos,ytos,narea,offil,directn,*999)
         if (vtbl .eq. 1)
     &      call tblav(lxav,lyav,ngr,avfil,directn,*999)
C
C	Calculate shutter offsets for each line or samp...
C
         CALL XVPARM('OFFSETS',OFFSET_FILE,CNT,IDEF,1)
         IF (IDEF.EQ.0) THEN
	    CALL XVMESSAGE ('Saving shutter offsets...',' ')
	    CALL LINSHUT( NGR, LXAV, LYAV, 1.0, 1.0, ASIZ,
     &                    LorS_Numbers, Shutter_Offsets )
            CALL XVUNIT(ISO,'X',1,IND,'U_NAME',OFFSET_FILE,' ')
            IF (IND.NE.1) GOTO 997
            CALL XVOPEN(ISO,IND,'OP','WRITE','U_NL',1,'U_NS',ASIZ,
     &                  'U_FORMAT','REAL','O_FORMAT','REAL',
     &                  'OPEN_ACT','SA','IO_ACT','SA',' ')
            CALL XLADD(ISO,'HISTORY','FILE','SHUTTER-OFFSET',
     &                 STAT,'FORMAT','STRING',' ')
            CALL XLADD(ISO,'HISTORY','SO_TYPE',DIRECTN//'-DEPENDENT',
     &                 STAT,'FORMAT','STRING',' ')
            CALL XVWRIT(ISO,Shutter_Offsets,IND,' ')
            CALL XVCLOSE(ISO,IND,' ')
         ENDIF

C        Output centers of all bad areas in MARK format file...
         CALL XVUNIT(OUNI,'OUT',1,STAT,' ')
         IF (STAT.NE.1) GOTO 250            !Skip if output file not specified

C        Put the centers into output buffer (OUTM)...
         I = 0
         DO K = 1,NAREA
            IF ((IREJCT .NE. 0) .AND. (BAD(K) .NE. 0) .AND.
     &          ((IREJCT .EQ. BAD(K)) .OR. (IREJCT .EQ. 3) .OR. 
     &           (BAD(K) .EQ. 4))) THEN
               SL = AREA(1,K)
               SS = AREA(2,K)
               NL = AREA(3,K)
               NS = AREA(4,K)
               I = I + 1
               OUTM(1,I)=SL+NL/2
               OUTM(2,I)=SS+NS/2
            ENDIF
         ENDDO

         NSO = 2*(NAREA-GAREA)
         NLO = 1
         CALL XVOPEN(OUNI,STAT,'OPEN_ACT','SA','IO_ACT','SA',
     &               'OP','WRITE','U_NL',NLO,'U_NS',NSO,'U_FORMAT',
     &               'REAL','O_FORMAT','REAL',' ')
         IF(IREJCT .EQ. 1)
     &      CALL XLADD(OUNI,'HISTORY','CCDRECIP',
     &                 ' REJECTED FOR SENSITIVITY',STAT,'FORMAT',
     &                 'STRING',' ')
         IF(IREJCT .EQ. 2)
     &      CALL XLADD(OUNI,'HISTORY','CCDRECIP',
     &                 ' REJECTED FOR SHUTTER OFFSET',STAT,'FORMAT',
     &                 'STRING',' ')
         IF(IREJCT .EQ. 3)
     &      CALL XLADD(OUNI,'HISTORY','CCDRECIP',
     &                 ' REJECTED FOR BOTH',STAT,'FORMAT','STRING',' ')
         CALL XVWRIT(OUNI,OUTM,STAT,'NSAMPS',NSO,' ')
         CALL XVCLOSE(OUNI,STAT,' ')

  250    CALL XVMESSAGE ('CCDRECIP task completed',' ')
         RETURN

C	 Error conditions...
  970    CALL PRNT(7,1,EXPO,'??E - No data for exposure=.')
         CALL XVMESSAGE (
     &      '***Run MOMGEN on this exposure and try again.',' ')
         GOTO 999
  996    CALL XVMESSAGE ('??E - Error opening light file',' ')
         CALL PRNT(4,1,JST,' IOSTAT=.')
         GOTO 999
  997    CALL XVMESSAGE ('??E - Error allocating offset file',' ')
         GOTO 999
  998    CALL XVMESSAGE ('??E - Error in grid',' ')
         CALL PRNT(4,1,NAREA,' ***Narea=.')
  999    CALL XVMESSAGE ('***CCDRECIP task cancelled',' ')
         CALL ABEND
      END

C************************************************************************
C Routine to flag all areas which differ from mean by more than SIGTOL
C sigma...
C Inputs: BUF(N) = input samples
C         TYPE = 1 for sensitivity, =2 for offset
C         N = number of samples
C         SIGTOL = 1 or 2 (sigma)
C Outputs: BAD(N) = TYPE if sample is bad type
C                 = 4 if both sensitivity and offset terms are bad
      SUBROUTINE IMEAN(BUF,TYPE,N,BAD,SIGTOL)
 
         implicit none
         INTEGER*4 N,I,K,NLOOP,NS
         REAL*4 MEAN,EPS,SAMP,SIGMA
         REAL*8 BUF(N),SUM,SSUM
         INTEGER*4 BAD(N),TYPE,SIGTOL
         CHARACTER*44 MSG
     &      /' N=**** MEAN=******.***** SIGMA=******.*****'/

         NLOOP = 4 - SIGTOL

         DO 50 I=1,NLOOP
            NS = 0
            SUM = 0.0D0
            SSUM = 0.0D0

            DO K=1,N
               IF ((BAD(K) .NE. TYPE) .AND. (BAD(K) .NE. 4)) THEN 
                  SAMP = BUF(K)
                  NS = NS+1
                  SUM = SUM+SAMP
                  SSUM = SSUM+SAMP*SAMP
               ENDIF
            ENDDO

            MEAN = SUM/NS
            SIGMA = SSUM/NS-MEAN*MEAN
            SIGMA = SQRT(SIGMA)
            WRITE (MSG,'(A2,I4,A6,F12.5,A7,F12.5)') 'N=',NS,
     &             ' MEAN=',MEAN,' SIGMA=',SIGMA
            IF (I .EQ. 1) 
     &         CALL XVMESSAGE ('Raw mean and sigma are...',' ')
            IF (I .EQ. 2) 
     &         CALL XVMESSAGE (
     &            'After throwing out samples differing by 2 sigma',' ')
            IF (I .EQ. 3) 
     &         CALL XVMESSAGE (
     &            'After throwing out samples differing by 1 sigma',' ')
            CALL XVMESSAGE (MSG,' ')
            IF (I .EQ. NLOOP) GOTO 50

C-------To avoid rejecting all points when perfect data is input
            if (sigma .eq. 0.0) sigma=1.0e-06

            EPS=(3-I)*SIGMA

            DO 20 K=1,N		!Weed out bad samples
               IF ((BAD(K) .EQ. TYPE) .OR. (BAD(K) .EQ. 4)) GOTO 20
               SAMP = BUF(K)
               SAMP = ABS(SAMP-MEAN)
               IF (SAMP .GE. EPS) THEN
                  IF (BAD(K) .NE. 0) THEN
                     BAD(K) = 4
                  ELSE
                     BAD(K) = TYPE
                  ENDIF
               ENDIF
   20       CONTINUE

   50    CONTINUE

         RETURN
      END         


C**************************************************************************************
      SUBROUTINE RTIMES(NPTS,L,T,D,IPLOT,NLAB,NAREA,GAREA,
     1           UNITS,atbl,atfil,plotgpi,nplotgpi,plotgpi2,
     2 nplotgpi2,epsplot,ploteps,nploteps,tbl,ntbl,tblb,ntblb,*)

	implicit none
         REAL*4 LT,TMT0,L(30),T(30)
         REAL*8 D(30)
         REAL*8 X(30),X2(30),X3(30),DL1(30),DL2(30),DL3(30)
         REAL*8 TOS,AO,STOS,SAO,DY,DZ,XYZ
         REAL*8 SW,SWT,SWX,SWXT,SWT2,W,RX,SX,SX2,SDY

         INTEGER*4 NAREA,GAREA,NPTS,IPLOT,NLAB,I,JST
         integer*4 atbl,nplotgpi,nplotgpi2,nploteps,ntbl,ntblb
	 integer*4 natfil,nyt1,nyt2,nyt3,nyt4
	 logical*4 epsplot

         COMMON/CP/VMES,ARMES,RMSG,LABEL
         CHARACTER*4320 LABEL,RMSG*41,VMES*54,ARMES*51

        character*80 plotgpi,plotgpi2,ploteps,tbl,tblb

         CHARACTER*30 YT1, YT2, YT3, YT4
         CHARACTER*132 MSG1
         CHARACTER*9 HD1, HD4
         CHARACTER*12 HD2
         CHARACTER*7 HD3
         CHARACTER*13 HD5
         CHARACTER*19 HD6
         CHARACTER*8 HD7
         CHARACTER*14 HD8
         CHARACTER*6 HD9L, HD9R, HD12B
         CHARACTER*10 HD12A
         CHARACTER*4 HD13, HD16
         CHARACTER*3 HD14, HD15
         CHARACTER*40 atfil
         CHARACTER*8 UNITS
         CHARACTER*1 TAB
c
c	YT is ytitle
C
         YT1 = '(DN-DC)/L'
	 nyt1 = index(yt1,'  ') - 1
         YT2 = '(DN-DC)/(L*T)'
	 nyt2 = index(yt2,'  ') - 1
         YT3 = '(DN-DC)/(L*(T-TOS))'
	 nyt3 = index(yt3,'  ') - 1
         YT4 = ' RESIDUAL FIT'
	 nyt4 = index(yt4,'  ') - 1

         HD1 = 'COMMANDED'
         HD2 = 'ILLUMINATION'
         HD3 = '(DN-DC)'
         HD4 = '(DN-DC)/L'
         HD5 = '(DN-DC)/(L*T)'
         HD6 = '(DN-DC)/[L*(T-TOS)]'
         HD7 = 'RESIDUAL'
         HD8 = 'EXPOSURE T(MS)'
         HD9L = 'L(LUM)'
         HD9R = 'L(RAD)'
         HD12A = '(DN/ENERGY'
         HD12B = ' UNIT)'
         HD13 = 'TOS='
         HD14 = 'SD='
         HD15 = 'AO='
         HD16 = 'RMS='

         TAB = CHAR(9)
         CALL XVMESSAGE (' ',' ')
         MSG1 = ' '
         MSG1(4:12)=HD1
         MSG1(17:28)=HD2
         MSG1(33:39)=HD3
         MSG1(46:54)=HD4
         MSG1(60:67)=HD7
         MSG1(74:86)=HD5
         MSG1(90:97)=HD7
         MSG1(100:118)=HD6
         MSG1(121:129)=HD7
         CALL XVMESSAGE (MSG1(2:132),' ')		!print 1st line header

         MSG1 = ' '
         MSG1(2:15)=HD8
         IF (UNITS .EQ. 'RADIANCE') THEN
           MSG1(19:24)=HD9R
         ELSE
           MSG1(19:24)=HD9L
         ENDIF
         MSG1(45:54)=HD12A
         MSG1(59:68)=HD12A
         MSG1(75:84)=HD12A
         MSG1(89:98)=HD12A
         MSG1(104:113)=HD12A
         MSG1(120:129)=HD12A
         CALL XVMESSAGE (MSG1(2:129),' ')		!print 2nd line header

         MSG1 = ' '
         MSG1(47:52)=HD12B
         MSG1(61:66)=HD12B
         MSG1(77:82)=HD12B
         MSG1(91:96)=HD12B
         MSG1(106:111)=HD12B
         MSG1(122:127)=HD12B
         CALL XVMESSAGE (MSG1(2:127),' ')		!print 3rd line header

c	print *,'here...'
         SX = 0.0D0
         SX2 = 0.0D0
         SW = 0.0D0
         SWX = 0.0D0
         SWT = 0.0D0
         SWXT = 0.0D0
         SWT2 = 0.0D0

         DO I=1,NPTS
             W = L(I+1)**2
             X(I) = D(I)/L(I+1)   ! calculate (DN-DC)/L
             SX = SX + X(I)
             SX2 = SX2 + X(I)**2
             SW = SW + W
             SWX = SWX + W*X(I)
             SWT = SWT + W*T(I+1)
             SWXT = SWXT + W*DBLE(T(I+1))*DBLE(X(I))
             SWT2 = SWT2 + W*DBLE(T(I+1))**2
         ENDDO

C        calculate camera sensitivity
         AO = (SWX*SWT-SWXT*SW)/(SWT**2-SW*SWT2)

C        calculate shutter-offset
         TOS = (AO*SWT-SWX)/(AO*SW)
         MSG1 = ' '
         RX = 0.0D0
         DZ = 0.0D0
         SDY = 0.0D0
	 natfil=index(atfil,'  ') - 1
C------OPEN OUTPUT TABLE IF NEEDED
	 if (atbl .eq. 1) THEN
            open(11,file=atfil(1:natfil),status='UNKNOWN',IOSTAT=JST,ERR=999)
 	    IF (UNITS .EQ. 'RADIANCE') THEN
               WRITE(11,10) 'MEAN_DN.aka.D',TAB,'RAD.aka.L',TAB,
     &               'EXP.aka.T',TAB,'L.times.T',TAB,'T.minus.T0.aka.Q',
     &               TAB,'D.over.L',TAB,'D.over.L.times.T',TAB,
     &               'D.over.L.times.Q'
            ELSE
               WRITE(11,10) 'MEAN_DN.aka.D',TAB,'LUM.aka.L',TAB,
     &               'EXP.aka.T',TAB,'L.times.T',TAB,'T.minus.T0.aka.Q',
     &               TAB,'D.over.L',TAB,'D.over.L.times.T',TAB,
     &               'D.over.L.times.Q'
            ENDIF
         ENDIF

   10    FORMAT(1x,A13,A1,A9,A1,A9,A1,A9,A1,A16,A1,A8,A1,A16,A1,A16)

	if (iplot.eq.1) then
	   OPEN(19,FILE=tbl(1:ntbl),STATUS='UNKNOWN',IOSTAT=JST,ERR=997)
	endif

C	 Get residuals
         DO I=1,NPTS

C           calculate (DN-DC)/(L*T)
            X2(I) = D(I)/(DBLE(L(I+1))*DBLE(T(I+1)))
C           calculate residual
            DL2(I) = X2(I) - AO

C           calculate (DN-DC)/[L*(T-TO)]
            X3(I) = D(I)/(DBLE(L(I+1))*(DBLE(T(I+1))-TOS))
C           calculate residual
            DL3(I) = X3(I) - AO

            RX = RX + DL2(I)**2
            DZ = DZ + DL3(I)**2
            DL1(I) = X(I) - AO*(DBLE(T(I+1))-TOS)
            SDY = SDY + DL1(I)**2

C           output results in table format
            WRITE (MSG1(3:10),'(F8.4)') T(I+1)
            WRITE (MSG1(17:24),'(F8.4)') L(I+1)
            WRITE (MSG1(31:38),'(F8.3)') D(I)
            WRITE (MSG1(45:52),'(F8.4)') X(I)
            WRITE (MSG1(59:66),'(F8.4)') DL1(I)
            WRITE (MSG1(75:82),'(F8.4)') X2(I)
            WRITE (MSG1(89:96),'(F8.4)') DL2(I)
            WRITE (MSG1(104:111),'(F8.4)') X3(I)
            WRITE (MSG1(120:127),'(F8.4)') DL3(I)
            CALL XVMESSAGE (MSG1,' ')
	if (iplot.eq.1) then
	    write (19,10100) MSG1
10100 format (A) 
	
	endif
C           write to table file if AREATBL is specified 
            IF (ATBL .EQ. 1) THEN
	       LT = L(I+1)*T(I+1)
	       TMT0 = T(I+1)-TOS
               WRITE(11,9) D(I),TAB,L(I+1),TAB,T(I+1),TAB,LT,TAB,TMT0,
     &               TAB,X(I),TAB,X2(I),TAB,X3(I)
            ENDIF
C
         ENDDO	!do i=1,npts
	  if (IPLOT .eq.1 ) close(19)
	 IF (ATBL .EQ. 1) CLOSE(11)

    9    FORMAT(1x,2(F12.4,A1),F10.1,A1,3(F16.4,A1),2(F16.6,A1))

         MSG1 = ' '

C        calculate standard deviations of the residuals
         RX = DSQRT(RX)
         DZ = DSQRT(DZ)
         DY = DSQRT(SDY/(NPTS-2.0D0))

         XYZ = NPTS*SX2 - SX**2
         SAO = DY*DSQRT(NPTS/XYZ)
         STOS = DY*DSQRT(SX2/XYZ)

         MSG1(55:58)=HD16
         MSG1(85:88)=HD16
         MSG1(116:119)=HD16
         WRITE (MSG1(60:66),'(F7.4)') DY
         WRITE (MSG1(90:96),'(F7.4)') RX
         WRITE (MSG1(121:127),'(F7.4)') DZ
         CALL XVMESSAGE (MSG1,' ')

         CALL XVMESSAGE (' ',' ')
         MSG1 = ' '
         MSG1(1:4)=HD13
         MSG1(15:17)=HD14
         MSG1(29:31)=HD15
         MSG1(43:45)=HD14
         WRITE (MSG1(5:11),'(F7.4)') TOS
         WRITE (MSG1(18:24),'(F7.4)') STOS
         WRITE (MSG1(32:38),'(F7.4)') AO
         WRITE (MSG1(46:52),'(F7.4)') SAO
         VMES=MSG1
         CALL XVMESSAGE (VMES,' ')
         CALL XVMESSAGE (RMSG,' ')
         WRITE (ARMES(22:25),'(I4)') GAREA
         WRITE (ARMES(33:36),'(I4)') NAREA
         CALL XVMESSAGE (ARMES,' ')

         IF (IPLOT.EQ.0) RETURN

C        PLOT 1 - plot (DN-DC)/L .vs. SHUTTER TIME
         CALL RPLOT(NLAB,YT1,nyt1,X,T,0,NPTS,AO,1,plotgpi,nplotgpi,
     1 plotgpi2,nplotgpi2,epsplot,ploteps,nploteps,tbl,ntbl,tblb,ntblb)
C        PLOT 2 - plot residual .vs. SHUTTER TIME
         CALL RPLOT(NLAB,YT4,nyt4,DL1,T,3,NPTS,AO,2,plotgpi,nplotgpi,
     1 plotgpi2,nplotgpi2,epsplot,ploteps,nploteps,tbl,ntbl,tblb,ntblb)

C        PLOT 3 - plot (DN-DC)/(L*T) .vs. SHUTTER TIME
         CALL RPLOT(NLAB,YT2,nyt2,X2,T,1,NPTS,AO,3,plotgpi,nplotgpi,
     1 plotgpi2,nplotgpi2,epsplot,ploteps,nploteps,tbl,ntbl,tblb,ntblb)
C        PLOT 4 - plot residual .vs. SHUTTER TIME
         CALL RPLOT(NLAB,YT4,nyt4,DL2,T,2,NPTS,AO,4,plotgpi,nplotgpi,
     1 plotgpi2,nplotgpi2,epsplot,ploteps,nploteps,tbl,ntbl,tblb,ntblb)
C        PLOT 5 - plot (DN-DC)/[L*(T-TO)] .vs. SHUTTER TIME
         CALL RPLOT(NLAB,YT3,nyt3,X3,T,1,NPTS,AO,5,plotgpi,nplotgpi,
     1 plotgpi2,nplotgpi2,epsplot,ploteps,nploteps,tbl,ntbl,tblb,ntblb)
C        PLOT 6 - plot residual .vs. SHUTTER TIME
         CALL RPLOT(NLAB,YT4,nyt4,DL3,T,2,NPTS,AO,6,plotgpi,nplotgpi,
     1 plotgpi2,nplotgpi2,epsplot,ploteps,nploteps,tbl,ntbl,tblb,ntblb)

         RETURN
997      CALL xvmessage('??E - ERROR OPENING PLOTTING DATA FILE',' ')
	 CALL PRNT(4,1,JST,' IOSTAT =.')
	RETURN 1
999	 CALL xvmessage('??E - ERROR OPENING AREA TABLE FILE',' ')
	 CALL PRNT(4,1,JST,' IOSTAT =.')
	 RETURN 1
      END

C***************************************************************************
C     This subroutine generates a plot of the inputs DY and DX
      SUBROUTINE RPLOT(NLAB,YTITLE,NYTITLE,DY,DX,IFLAG,NPTS,DAO,
     1        typlot,plotgpi,nplotgpi,plotgpi2,
     2        nplotgpi2,epsplot,ploteps,nploteps,tbl,ntbl,tblb,ntblb)

	implicit none
         COMMON/CP/VMES,ARMES,RMSG,LABEL
         COMMON/PLT1/LXT,LYT,LT
         COMMON/PLT2/XLEN,YLEN,XPAGE,YPAGE,XORIG,YORIG,XDELTA,YDELTA

ccc         INTEGER*4 STATUS
         INTEGER*4 ICNT,NLAB,IFLAG,NPTS,I,LT,LXT,LYT,N,N2,jj,typlot
	 INTEGER*4 nltitle,nxtitle,ntitle(80),plotwid,plotht,isize
         INTEGER*4 nytitle,nplotgpi,nplotgpi2,nploteps,ntbl,ntblb
         INTEGER*4 ncolx,ncoly(6)
         REAL*4 labstep,tmp,xmin,xmax,ymin,ymax,ydiff
         REAL*4 Y(30),X(30),DX(30),AO,ABUF(30),ZBUF(30),XC,YC
         REAL*8 DY(30),DAO
	REAL*4 XDELTA,YDELTA,XLEN,YLEN,XPAGE,YPAGE,XORIG,YORIG

	logical*4 epsplot,first/.true./
        save first
        character*80 plotgpi,plotgpi2,ploteps,tbl,tblb
         CHARACTER*80 ltitle
         CHARACTER*12 MSG(6)	
         CHARACTER*30 YTITLE 
         CHARACTER*4320 LABEL
         CHARACTER*41 RMSG
         CHARACTER*54 VMES
         CHARACTER*51 ARMES
         CHARACTER*86 TITLE(80)
         CHARACTER*30 XTITLE

         LTITLE = 'CCD RECIPROCITY ANALYSIS'
	nltitle = index(ltitle,'    ') - 1
         XTITLE = 'SHUTTER TIME(MS)'
	nxtitle = index(xtitle,'    ') - 1
         LXT = 17
         LYT = 19
         LT = 25
         YC = 12.50           ! set up for HEADER
         XC = 1.50
         YC = YC - 0.25       ! set up for LABEL
         N = MIN0(NLAB,25)

         ICNT = 1

C        insert TASK labels into TITLE
         DO I = 1,N
            TITLE(ICNT)=LABEL(1+(I-1)*72:1+(I-1)*72+71)
	    ntitle(icnt) = index(title(icnt),'    ') - 1
            ICNT = ICNT + 1
         ENDDO

C        insert more labels into TITLE
         TITLE(ICNT)=VMES
	 ntitle(icnt) = 52		!index(title(icnt),'    ') - 1
         ICNT = ICNT + 1
         TITLE(ICNT)=RMSG
         ntitle(icnt) = 40    		!index(title(icnt),'    ') - 1
         ICNT = ICNT + 1
         TITLE(ICNT)=ARMES
	ntitle(icnt) = 50		!index(title(icnt),'    ') - 1

C        display plot header and axis-titles
ccc         CALL HEADER (TITLE,ICNT,0) ! 0=left justify, 1=center justify, 2=right
ccc         CALL AXESTITLES(XTITLE,YTITLE,270,' ',0)
C--  GET THE SIZE OF THE PLOT.
c       default
        plotwid =  648  !640 @72dpi = 8.888.. inches    9 inch = 648
        plotht  =  648  !480 @72dpi = 6.666.. inches    7 inch = 504
        labstep = 0.02          !for size 9 font --- originally 0.04

        if (icnt .gt. 16) then
           tmp = icnt/16
           plotht = int(plotht * tmp)
           labstep = labstep/tmp
        endif

        isize = 10

c  added check on 'first' because of anomalous behaviour of "UNKNOWN" status on Solaris
c  -lwk-  24sep2013
        if (first) then
          first = .false.
          open(98,file=plotgpi(1:nplotgpi),status='UNKNOWN',iostat=jj,err=995)
          if (epsplot) then
             open(97,file=plotgpi2(1:nplotgpi2),status='UNKNOWN',iostat=jj,err=996)
          endif
        endif

            call write_gpi_1(98,tbl,ntbl,tblb,ntblb,ytitle,nytitle,
     1 xtitle,nxtitle,isize,plotwid,plotht,
     2 ploteps,nploteps)
            if (epsplot) then
                call write_gpi_1(97,tbl,ntbl,tblb,ntblb,ytitle,nytitle,
     1 xtitle,nxtitle,isize,plotwid,plotht,
     2 ploteps,nploteps)
            endif

         AO = SNGL(DAO)
         N2 = NPTS + 2

	XMAX = -1.0e20
	XMIN = 1.0
	YMAX = -1.0e20
	YMIN = 1.0e20
         DO I = 1,NPTS
            Y(I) = DY(I)
            if (y(i) .lt. 1.0e-6) y(i) = 0.0
	    if (y(i) .gt. ymax) ymax=y(i)
	    if (y(i) .lt. ymin) ymin=y(i)
            X(I) = DX(I+1)
	    if (x(i) .gt. xmax) xmax=x(i)
         ENDDO
c	print *, 'ymin,ymax = ',ymin,ymax
c        if (ymax .eq. -0.0) ymax = 0.
c        if (ymin .eq. -0.0) ymin = 0.

        if (typlot .eq. 3) then
            ymax = ymax + 0.01*ymax
            ymin = ymin - 0.01*ymin     
        endif
        if (typlot .eq. 4) then
            ymax = ymax - 0.1*ymax
            ymin = ymin + 0.1*ymin
        endif
	
	if (ymin .eq. ymax) then
	    if (ymax .eq. 0.0) then
		ymax = 1.0
                ymin = -1.0
	    else 
	        ymin = ymin - 0.1*ymin
	        ymax = ymax + 0.1*ymax
            endif
        endif
	if (typlot .eq. 1) then
		ymax = 40
		ymin = 0
	endif
c	print *, 'ymin,ynmax = ',ymin,ymax
         DO I = 1,NPTS
            ZBUF(I) = 0.0000
            ABUF(I) = AO
         ENDDO

	ydiff = ymax -  ymin
	if (ydiff .le. 0.001) then
		ymin = ymin - 1.0
		ymax = ymax + 1.0
	endif
	
         XLEN = 8.00
         YLEN = 10.00
         XORIG = 0.0000
         XDELTA = 15.0000
         X(NPTS+1) = XORIG
         X(NPTS+2) = XDELTA

c	print *,'IFLAG = ',IFLAG
         IF (IFLAG .EQ. 3) GOTO 30
         IF (IFLAG .EQ. 2) GOTO 20
         IF (IFLAG .EQ. 1) GOTO 10
c iflag=0    Y='(DN-DC)/L' plot (DN-DC)/L .vs. SHUTTER TIME
         YORIG = 0.0000
         YDELTA = 2.0000
         Y(NPTS+1) = YORIG
         Y(NPTS+2) = YDELTA
         GOTO 40
c iflag=1    Y='(DN-DC)/(L*T)'  plot (DN-DC)/(L*T) .vs. SHUTTER TIME 
c                           or  plot (DN-DC)/[L*(T-TO)] .vs. SHUTTER TIME
   10    YORIG = 0.120
         YDELTA = 0.0100
         Y(NPTS+1) = YORIG
         Y(NPTS+2) = YDELTA
         ABUF(NPTS+1) = YORIG
         ABUF(NPTS+2) = YDELTA
         GOTO 40
c iflag=2    Y=' RESIDUAL FIT'    plot residual .vs. SHUTTER TIME
   20    YORIG =  -0.100
         YDELTA = 0.020
         Y(NPTS+1) = YORIG
         Y(NPTS+2) = YDELTA
         ZBUF(NPTS+1) = YORIG
         ZBUF(NPTS+2) = YDELTA
         GOTO 40
c  iflag=3    Y=' RESIDUAL FIT'    plot residual .vs. SHUTTER TIME
   30    YORIG =  -1.000
         YDELTA = 0.200
         Y(NPTS+1) = YORIG
         Y(NPTS+2) = YDELTA
         ZBUF(NPTS+1) = YORIG
         ZBUF(NPTS+2) = YDELTA

   40    CONTINUE

         X(NPTS+1) = 0.0
         X(NPTS+2) = 1.0
         Y(NPTS+1) = 0.0
         Y(NPTS+2) = 1.0
         ABUF(NPTS+1) = 0.0
         ABUF(NPTS+2) = 1.0
         ZBUF(NPTS+1) = 0.0
         ZBUF(NPTS+2) = 1.0

cc	do i=1,n2		!npts + 2
cc	  print *,'i,x,y,abuf,zbuf = ',i,x(i),y(i),abuf(i),zbuf(i)
cc        enddo

	call write_gpi_2(98,ltitle,nltitle,xmin,xmax,
     1 ymin,ymax,isize)
	   if (epsplot) then
           call write_gpi_2(97,ltitle,nltitle,xmin,xmax,
     1 ymin,ymax,isize)

	   endif

	call write_gpi_lab(98,icnt,title,ntitle,isize,labstep)
        if (epsplot) then
           call write_gpi_lab(97,icnt,title,ntitle,isize,labstep)
        endif

	ncolx = 1
	do i=1,6
	   ncoly(i) = i + 3
        enddo
	msg(1)='PLOT 1'
	msg(2)='PLOT 2'
	msg(3)='PLOT 3'
	msg(4)='PLOT 4'
	msg(5)='PLOT 5'
	msg(6)='PLOT 6'
        call write_ls_gpi_pl(98,tbl,ntbl,ncolx,ncoly,typlot,msg)
        if (epsplot) then
           call write_ls_gpi_pl(97,tbl,ntbl,ncolx,ncoly,typlot,msg)
        endif

ccc         CALL LINE(X,Y,NPTS,1,1,1)
ccc         IF (IFLAG .EQ. 1) CALL LINE(X,ABUF,NPTS,1,1,11)
ccc         IF (IFLAG .EQ. 3) CALL LINE(X,ZBUF,NPTS,1,0,0)
cccc        IF (IFLAG .EQ. 2) CALL LINE(X,ZBUF,NPTS,1,0,0)
ccc         CALL XRTPAGE (STATUS)
cc         IF (STATUS .NE. 1) GOTO 960
         RETURN

ccc  960    CALL XVMESSAGE ('*** Incomplete Ploting Operation.',' ')
ccc         CALL XVMESSAGE ('XRT Window Button Definitions:',' ')
ccc         CALL XVMESSAGE ('SAVE = write to output PostScript file.',' ')
ccc         CALL XVMESSAGE ('PAGE = display next graph.',' ')
ccc         CALL XVMESSAGE ('EXIT = terminate application.',' ')
ccc         CALL MABEND('***CCDRECIP task cancelled',' ')
c   error returns
995     call xvmessage('??E - rplot: Error opening/writing gnuplot file',' ')
        call abend
        return
996     call xvmessage('??E - rplot: Error opening/writing gnuplot eps file',' ')
        call abend
        return

      END


C**********************************************************************
      SUBROUTINE TPLOT(NLAB,Y,X,NPTS,LYAV,LXAV,NRGD,DIR,
     1        asiz,plotgpi,nplotgpi,plotgpi2,
     2        nplotgpi2,epsplot,ploteps,nploteps,tbl,ntbl,tblb,ntblb)

	implicit none
         REAL*4 Y(402),X(402),LYAV(30),LXAV(30)
         REAL*4 XC,XLEN,YLEN,XORIG,YORIG,XPAGE,YPAGE,XDELTA,YDELTA
         REAL*4 YAV,YC,labstep,tmp,xmax,xmin,ymax,ymin

         INTEGER*4 ICNT,P,NLAB,NPTS,NRGD,I,K,LT,LXT,LYT,N,N2,nltitle
         INTEGER*4 nxtitle,nytitle,ntitle(80),asiz,ncolx,ncoly(6)
         INTEGER*4 nplotgpi,nplotgpi2,nploteps,ntbl,ntblb
	 INTEGER*4 plotwid,plotht,isize,jj,jst

         LOGICAL*4 epsplot

        character*80 plotgpi,plotgpi2,ploteps,tbl,tblb
         CHARACTER*12 MSG(6)
         CHARACTER*4320 LABEL
         CHARACTER*41 RMSG
         CHARACTER*54 VMES
         CHARACTER*51 ARMES
         CHARACTER*47 LMESG
	 CHARACTER*80 LTITLE
         CHARACTER*86 TITLE(80)
         CHARACTER*30 XTITLE
         CHARACTER*30 YTITLE
         CHARACTER*4 DIR

         COMMON/PLT1/LXT,LYT,LT
         COMMON/PLT2/XLEN,YLEN,XPAGE,YPAGE,XORIG,YORIG,XDELTA,YDELTA
         COMMON/CP/VMES,ARMES,RMSG,LABEL


         LMESG = 'LINE NUMBER=**** SHUTTER OFFSET=*********** MS'
         LTITLE = 'CCD RECIPROCITY ANALYSIS'
	 nltitle = index(ltitle,'   ')
         XTITLE = 'LINE NUMBER'
	 nxtitle = index(xtitle,'   ')
         YTITLE = 'GLOBAL SHUTTER OFFSET (MS)'
	 nytitle = index(ytitle,'   ') 

         IF (DIR(1:4) .EQ. 'SAMP') THEN
	      LMESG(1:4) = DIR(1:4)
	      XTITLE(1:4) = DIR(1:4)
         END IF

         ICNT = 1
         LXT = 11
         LYT = 26
         LT = 25
         YC = 12.50           !SET UP FOR HEADER
         XC = 1.50
C         CALL SYMBOL(XC,YC,0.21,TITLE,0,0.0,LT)
         YC = YC-0.25         !SET UP FOR LABEL
         N = MIN0(NLAB,25)

         DO P=1,N
            TITLE(ICNT)=LABEL(1+(P-1)*72:1+(P-1)*72+71)
	    ntitle(icnt) = index(title(icnt),'    ') - 1
            ICNT = ICNT + 1
         ENDDO

         TITLE(ICNT)=VMES
         ntitle(icnt) = 52		!index(title(icnt),'    ') - 1
         ICNT = ICNT + 1
         TITLE(ICNT)=RMSG
         ntitle(icnt) = 40		!index(title(icnt),'    ') - 1
         ICNT = ICNT + 1
         TITLE(ICNT)=ARMES
         ntitle(icnt) = 50		!index(title(icnt),'    ') - 1
CCC         CALL HEADER (TITLE,ICNT,0)
ccc         CALL AXESTITLES(XTITLE,YTITLE,270,' ',0)
C--  GET THE SIZE OF THE PLOT.
c       default
        plotwid =  648  !640 @72dpi = 8.888.. inches    9 inch = 648
        plotht  =  648  !480 @72dpi = 6.666.. inches    7 inch = 504
        labstep = 0.02          !for size 9 font --- originally 0.04

        if (icnt .gt. 16) then
           tmp = icnt/16
           plotht = int(plotht * tmp)
           labstep = labstep/tmp
        endif

        isize = 10

c  commented out because of anomalous behaviour of "UNKNOWN" status on Solaris
c  -lwk-  24sep2013
c       open(98,file=plotgpi(1:nplotgpi),status='UNKNOWN',iostat=jj,err=995)
c       if (epsplot) then
c          open(97,file=plotgpi2(1:nplotgpi2),status='UNKNOWN',iostat=jj,err=996)
c       endif

            call write_gpi_1(98,tbl,ntbl,tblb,ntblb,ytitle,nytitle,
     1 xtitle,nxtitle,isize,plotwid,plotht,
     2 ploteps,nploteps)
            if (epsplot) then
                call write_gpi_1(97,tbl,ntbl,tblb,ntblb,ytitle,nytitle,
     1 xtitle,nxtitle,isize,plotwid,plotht,
     2 ploteps,nploteps)
            endif

 
         N2 = NPTS + 2
         I = 1
         K = 1
         YAV = 0.0E0
         CALL XVMESSAGE (' ',' ')

        XMAX = real(asiz)
        XMIN = 0.0
c	print *,'xmax,xmin = ',xmax,xmin
        YMAX = -1e20
        YMIN = 1.0e20
c	print *,'NRGD = ',nrgd

c        if (iplot.eq.1) then
           OPEN(18,FILE=tblb(1:ntblb),STATUS='UNKNOWN',IOSTAT=JST,ERR=997)
c        endif

         DO N=1,NRGD
            WRITE (LMESG(13:16),'(I4)') NINT(LXAV(N))
            WRITE (LMESG(33:43),'(F11.6)') LYAV(N)
	    if (lyav(n) .gt. ymax) ymax=lyav(n)
	    if (lyav(n) .lt. ymin) ymin=lyav(n)
            CALL XVMESSAGE (LMESG,' ')
	    write (18,10100) NINT(LXAV(N)), LYAV(N)
10100 format (i4,1x,f11.6)

         ENDDO

c	if (iplot.eq.1) then
	    close(18)
c	endif
        if (ymin .eq. ymax) then
            if (ymax .eq. 0.0) then
                ymax = 1.0
            else
                ymin = ymin - 0.1*ymin
                ymax = ymax + 0.1*ymax
            endif
        endif
	
         X(NPTS+1) = 0.0
         X(NPTS+2) = 1.0
         Y(NPTS+1) = 0.0
         Y(NPTS+2) = 1.0

ccc         CALL LINE(X,Y,NPTS,1,-1,3)

         LXAV(NRGD+1) = 0.0
         LXAV(NRGD+2) = 1.0
         LYAV(NRGD+1) = 0.0
         LYAV(NRGD+2) = 1.0


        call write_gpi_2(98,ltitle,nltitle,xmin,xmax,
     1 ymin,ymax,isize)
           if (epsplot) then
           call write_gpi_2(97,ltitle,nltitle,xmin,xmax,
     1 ymin,ymax,isize)

           endif
ccc         CALL LINE(LXAV,LYAV,NRGD,1,0,0)
	call write_gpi_lab(98,icnt,title,ntitle,isize,labstep)
        if (epsplot) then
           call write_gpi_lab(97,icnt,title,ntitle,isize,labstep)
        endif

         DO I=1,6
            ncoly(i) = 2
            MSG(I)=' '
         ENDDO
        MSG(1)='PLOT 7'
	ncolx = 1
        call write_ls_gpi_pl(98,tblb,ntblb,ncolx,ncoly,1,msg)
	if (epsplot) then
	   call write_ls_gpi_pl(97,tblb,ntblb,ncolx,ncoly,1,msg)
        endif

         RETURN

c   error returns
995     call xvmessage('??E - tplot: Error opening/writing gnuplot file',' ')
        call abend
        return
996     call xvmessage('??E - tplot: Error opening/writing gnuplot eps file',' ')
        call abend
        return
997      CALL xvmessage('??E - ERROR OPENING PLOTTING B DATA FILE',' ')
         CALL PRNT(4,1,JST,' IOSTAT =.')
        RETURN 



      END


C************************************************************************
C     Performs a piece-wise linear interpolation of the calculated
C     shutter offsets to calculate all offsets.  The first two
C     and last two points are use to extrapolate for the first and
C     last few offsets.
      Subroutine LinShut( NIn, XIn, YIn, XStart, DX, NOut, XOut, YOut)
         Implicit None
         Integer*4  NIn, NOut
         Real*4 XIn(NIn), YIn(NIn), XStart, DX, XOut(NOut), YOut(NOut)
      
         Integer*4 I, IX1, IX2
         Real*4 X, M, B
      
         IX1 = 0
         IX2 = 1
         X = XStart
      
         DO I = 1, NOut
	    IF ( (IX1 .EQ. 0) .OR.		!) THEN
     &	         ( (X .GE. XIn(IX2)) .AND. (IX2 .LT. NIn) ) ) THEN
               IX1 = IX2
               IX2 = IX2 + 1
               M = (YIn(IX1) - YIn(IX2)) / (XIn(IX1) - XIn(IX2))
               B = YIn(IX1) - M*XIn(IX1)
            ENDIF
            XOut(I) = X
            YOut(I) = M*XOut(I) + B
            X = X + DX
         END DO
      
         RETURN
      END

C**************************************************************************
C     This subroutine retrives the VICAR 1 and history label.  It will 
C     display the TASK, USER, and TIME within each TASK entry.
      SUBROUTINE LABPROC(IUNI,NLAB,LABEL)
         IMPLICIT NONE 
         INTEGER*4 INSTANCES(20)
         INTEGER*4 NHIST, J, I             ! loop control variables
         INTEGER*4 NLAB, STAT              ! store # of labels and status
         INTEGER*4 IUNI                    ! store file id number

         CHARACTER*8 TASKS(20)           ! store task nume
         CHARACTER*4320 LABEL            ! store retrived VIC1 label
         CHARACTER*132 MSG               ! store message
         CHARACTER*12 UNAME              ! store user name
         CHARACTER*28 TIME               ! store time
         CHARACTER*65 HBUF

C        initialize display and storage buffers
         HBUF = '----TASK:------------USER:------------------------'
         MSG = ' '
         LABEL = ' '

         CALL VIC1LAB(IUNI,STAT,NLAB,LABEL,0) ! retrive VICAR 1 label
                                              ! NLAB=0 if no VIC1LAB
         NHIST = 20                           ! max # of tasks to retrive

C        retirve history tasks
         CALL XLHINFO(IUNI,TASKS,INSTANCES,NHIST,STAT,' ')

C        for each task, extract the USER name and the DATE_TIME, 
C        write the TASK name, USER name, and DATE_TIME onto buffer (HBUF)
         DO J=1,NHIST
            UNAME = ' '
            TIME = ' '
            CALL XLGET(IUNI,'HISTORY','USER',UNAME,STAT,'HIST',
     &                 TASKS(J),'INSTANCE',INSTANCES(J),'FORMAT',
     &                 'STRING',' ')  ! retrive user name from task entry
            CALL XLGET(IUNI,'HISTORY','DAT_TIM',TIME,STAT,'HIST',
     &                 TASKS(J),'INSTANCE',INSTANCES(J),'FORMAT',
     &                 'STRING',' ')  ! retrive DATE_TIME from task entry
            DO I=1,8                             !BLANKS NULLS
               IF(TASKS(J)(I:I) .LT. '0') TASKS(J)(I:I) = ' '
            ENDDO
            HBUF(10:17) = TASKS(J)
            HBUF(27:38) = UNAME
            HBUF(39:63) = TIME
            LABEL(1+(NLAB+J-1)*72:(1+(NLAB+J-1)*72)+64) = HBUF
         ENDDO

         NLAB=NLAB+NHIST   ! calculate number of labels to be display 

C        output history tasks retrived from the label
         DO I=1,NLAB
            MSG(1:72) = LABEL(1+(I-1)*72:(1+(I-1)*72)+71)
            CALL XVMESSAGE (MSG,' ')
         ENDDO

         RETURN
      END
C*********************************************************************
        subroutine write_gpi_1(unit,tbl,ntbl,tblb,ntblb,ylabel,nylabel,
     1 xlabel,nxlabel,isize,plotwid,plotht,
     2 eps,neps)
c            
c       write out the first part of gpi form
c
        implicit none
        integer*4 unit,isize,plotwid,plotht
        integer*4 jj,psize
        integer*4 ntbl,ntblb,nylabel,nxlabel,neps
        character*16 ylabel,xlabel
        character*80 tbl,tblb,eps
c
	psize = isize
	if (unit .eq. 97) psize = 16
10100 format('# Created by program ccdrecip')              !#'s are ignored in gnuplot
            write(unit,fmt=10100,iostat=jj,err=995)
10105 format('# Gnuplot commands for signal vs shutter time plot')
            write(unit,fmt=10105,iostat=jj,err=995)
10110 format('# Data in ',a,' and ',a)
            write(unit,fmt=10110,iostat=jj,err=995) tbl(1:ntbl),tblb(1:ntblb)

        if (unit.eq.97) then
10300 format('set terminal postscript eps enhanced color "Ariel" ',i2,'  size 11 ,8')
           write(unit,fmt=10300,iostat=jj,err=995) psize
10305 format("set output '",a,"'")
           write(unit,fmt=10305,iostat=jj,err=995) eps(1:neps)
        else
! size is X,Y
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

c10141 format("set clip points")                         !how to deal with points out of range
c            write(unit,fmt=10142,iostat=jj,err=995)
c10142 format("set clip one")                            !how to deal with connecting lines out of range
c            write(unit,fmt=10141,iostat=jj,err=995)
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
       subroutine write_gpi_2(unit,title,ntitle,xrang1,xrang2,
     1 yrang1,yrang2,isize)
c       ,mycol,nplots,iplot,fscale,nocol,nncol,
c     2 oldcol,newcol,nstart,npoint,ncontr,filename,nfilename)
c
c       write out the repeatable descriptive part of gpi form
c       for first, or only, plot
c
        implicit none
        integer*4 unit
        integer*4 jj,isize,psize
        integer*4 ntitle
        real*4 xrang1,xrang2,yrang1,yrang2
        character*80 title

        psize=isize
        if (unit .eq.97) psize = 16

10145 format('set title "',a,'" font "Ariel,',i2,'"')
             write(unit,fmt=10145,iostat=jj,err=995) title(1:ntitle),psize
C
C---- SET THE SCALING PARAMETERS.
C
C
c       set y range to be like vicar image - lines counting downward from top
c
10135 format("set yrange [",f10.4,":",f10.4,"]")
             write(unit,fmt=10135,iostat=jj,err=995) yrang1,yrang2
10140 format("set xrange [",f8.2,":",f8.2,"]")
             write(unit,fmt=10140,iostat=jj,err=995) xrang1,xrang2

        return
995   continue
        if (unit.eq.97) then
            call xvmessage('??E - write_gpi2_2: Error opening/writing gnuplot eps file',' ')
            call abend
        else
            call xvmessage('??E - write_gpi2_2: Error opening/writing gnuplot file',' ')
            call abend
        endif

        return
        end
c******************************************************************************
        subroutine write_gpi_lab(unit,nhdr,header,nheadr,isize,labstep)
c
c       write image labels to top of plot
c output labels for only top 60% of plot
c
c
        implicit none
        integer*4 unit,ii,jj,nhdr,isize,isize2,nheadr(80)
        real*4  fpos,labstep
        character*86 header(80)
c
	isize2 = isize - 1
	if (unit .eq. 97) isize2 =  16
       write (unit,fmt=10165,iostat=jj,err=995)
10165 format ('unset label')

        fpos=1.0
        do ii=1,nhdr
                fpos = fpos - labstep
c       print *,'ii = ',i,header(ii)(1:nheadr(ii)),fpos
10160 format('set label ',i2,' "',a,'" at graph .25 ,',f5.2,
     1 ' font "ariel,',i2,'" front nopoint tc def')
c       1 ' font "ariel 8" front nopoint tc def')
        write(unit,fmt=10160,iostat=jj,err=995) ii,header(ii)(1:nheadr(ii)), fpos,isize2
cc      print 10160, i,headr(ii)(1:nheadr(ii)), fpos
cc10155 format("set label 2 '",a,"' at graph 0.4, 0.90 front nopoint tc def")
cc        write(98,fmt=10155,iostat=jj,err=995) headr(3)
        enddo
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
C******************************************************************************
        subroutine write_ls_gpi_pl(unit,tbl,ntbl,ncolx,ncoly,iplot,msg)
c
c       write out data in gpi form for pts, lines or linepoints
c       for a 1 value in control column
c
        implicit none
        integer*4 unit,ntbl
        integer*4 gcol,jj,iplot
        integer*4 ncolx,ncoly(6)
	CHARACTER*12 MSG(6)
        character*80 tbl
        character*100 outline

c       bring in gunplot colors, lines, points, etc
        include 'gnuplotchar'

c	print *, iplot, ' ncolx = ',ncolx,' ncoly = ',ncoly(iplot)

        gcol = iplot
10253 format ("plot  '",a,"' u ",i2,":",i2," t '",a,"' w linespoints lt ",i2," pt ",i2,
     1 " ps 2 lw 2 lc rgb '",a,"'")
           write(unit,fmt=10253,iostat=jj,err=995) tbl(1:ntbl),ncolx,ncoly(gcol),msg(gcol),
     1 lntype(gcol),pttype(gcol),ptcolor(gcol)(1:ptcolorl(gcol))

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


C*********************************************************************
      subroutine tbloff(nums,offs,npts,table_ds,directn,*)
         
        implicit none
         INTEGER*4 NPTS,JST,L
         REAL*4 NUMS(NPTS),OFFS(NPTS)

         CHARACTER*40 TABLE_DS
         CHARACTER*4 DIRECTN
         CHARACTER*1 TAB

        TAB = CHAR(9)

C-------NUMS IS THE SET OF LINE OR SAMPLE NUMBERS FOR THE AREAS
C-------OFFS IS THE SET OF SHUTTER OFFSETS FOR THE AREAS
C-------both NUMS and OFFS are 0.0 for bad areas
         OPEN(12,FILE=TABLE_DS,STATUS='UNKNOWN',IOSTAT=JST,ERR=999)

         WRITE(12,10) 
     &      DIRECTN//'_OF_AREA',TAB,'CALCULATED.SHUTTER_OFFSET'

   10    FORMAT(1x,A12,A1,A25)

         DO L=1,NPTS
	   IF (NUMS(L) .NE. 0.0) 
     &        WRITE(12,9) NUMS(L),TAB,OFFS(L)
         END DO

         CLOSE(12)
         RETURN

    9    FORMAT(1x,F8.2,A1,F12.4)

  999    CALL xvmessage('??E - ERROR OPENING OFFSET TABLE FILE',' ')
         CALL PRNT(4,1,JST,' IOSTAT =.')
         RETURN 1
      END


C***********************************************************************
      subroutine tblav(nums,offs,npts,table_ds,directn,*)

	implicit none
	 INTEGER*4 NPTS,JST,L 
         REAL*4 NUMS(NPTS),OFFS(NPTS)

         CHARACTER*40 TABLE_DS
         CHARACTER*4 DIRECTN
         CHARACTER*1 TAB


        TAB = CHAR(9)
C-------NUMS IS THE SET OF AVG LINE OR SAMPLE NUMBERS FOR THE ROWS OR COLS
C-------OFFS IS THE SET OF AVG SHUTTER OFFSETS FOR THE ROWS OR COLS

         OPEN(13,FILE=TABLE_DS,STATUS='UNKNOWN',IOSTAT=JST,ERR=999)

         WRITE(13,10) DIRECTN,TAB,'MEAN.SHUTTER_OFFSET'
   10    FORMAT(1x,A4,A1,A19)

         DO L=1,NPTS
            WRITE(13,9) NUMS(L),TAB,OFFS(L)
         END DO

         CLOSE(13)
         RETURN

    9    FORMAT(1x,F8.2,A1,F12.4)

  999    CALL xvmessage('??E - ERROR OPENING AVERAGE TABLE FILE',' ')
	 CALL PRNT(4,1,JST,' IOSTAT =.')
	 RETURN 1
      END

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create ccdrecip.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM ccdrecip

   To Create the build file give the command:

		$ vimake ccdrecip			(VMS)
   or
		% vimake ccdrecip			(Unix)


************************************************************************/


#define PROGRAM	ccdrecip
#define R2LIB

#define MODULE_LIST ccdrecip.f

#define FTNINC_LIST gnuplotchar

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

/*#define DEBUG	/* remove on delivery */
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create ccdrecip.pdf
PROCESS HELP=*
PARM INP        TYPE=STRING     COUNT=1
PARM OUT        TYPE=STRING     COUNT=(0:1)     DEFAULT=--
PARM PLOT       TYPE=STRING     COUNT=(0:1)     DEFAULT=--
PARM PLOTFMT    TYPE=STRING     COUNT=(0:1) VALID=(GNUPLOT,EPS) DEFAULT=GNUPLOT
PARM SIGTOL     TYPE=INTEGER    COUNT=(0:1)     DEFAULT=2       VALID=(1,2)
PARM REJECT     TYPE=INTEGER    COUNT=(0:1)     DEFAULT=1
PARM UNITS      TYPE=KEYWORD    COUNT=(0:1)     DEFAULT=RADIANCE +
                                            VALID=(RADIANCE,LUMINANC)
PARM LIGHT      TYPE=REAL       COUNT=(0:30)    DEFAULT=--
PARM LTFILE     TYPE=STRING     COUNT=(0:1)     DEFAULT=--
PARM DIRECTIO   TYPE=KEYWORD    COUNT=(0:1) VALID=(LINE,SAMP) DEFAULT=LINE
PARM ARRAYSIZ   TYPE=INTEGER    COUNT=(0:1)     DEFAULT=1024
PARM AREATBL    TYPE=STRING     COUNT=(0:1)     DEFAULT=--
PARM OFFTBL     TYPE=STRING     COUNT=(0:1)     DEFAULT=--
PARM AVOFFTBL   TYPE=STRING     COUNT=(0:1)     DEFAULT=--
PARM CORRTBL    TYPE=STRING     COUNT=(0,2)     DEFAULT=--
PARM OFFSETS    TYPE=STRING     COUNT=(0:1)     DEFAULT=--
END-PROC
.TITLE
VICAR Application Program CCDRECIP
.HELP
PURPOSE:

CCDRECIP determines the shutter offset (in msec) and sensitivity
(in DN per foot-lambert-milliseconds (UNITS = LUMINANC) or
in DN per picoamp-milliseconds (UNITS = RADIANCE)) for a
camera system.  The program is one of a series of programs originally
developed to support radiometric calibration of the Galileo SSI camera system.
(UNITS should be LUMINANCE for Galileo and RADIANCE for Cassini).

Reference:
    D-4264  MIPL Software Structural Design for the Instrument
            Calibration of GLL SSI Science Processing.
    D-tbd   Software Design Document for Instrument Calibration -
            Cassini ISS

.PAGE
EXECUTION:
                CCDRECIP INP=RCP.DAT OUT=MARK.DAT PARAMS

The input is a Reciprocity File (RCP) containing statistical data for
specified areas in the image for each exposure of a reciprocity sequence.
The RCP must have been previously initialized via LTGEN and loaded with
data via MOMGEN.

The output is an optional MARK-format tiepoint data set containing the
centers of all areas rejected for producing values for SENSITIVITY or
SHUTTER OFFSET or either which differ by more than 2 sigma from the mean
values for all the areas.

.PAGE
MATHEMATICAL BACKGROUND:

The output camera signal is proportional to exposure as follows:

      DN-DC = A*L*(T-To)
where
      DN-DC is the output signal minus the dark current,
      A is the camera sensitivity (DN/foot-lambert-milliseconds (LUMINANC)) or
                                   DN/picoamp-milliseconds (RADIANCE))
      L is the light cannon setting (foot-lamberts (LUMINANC)) or
        is the spectral radiance of the source (picoamp (RADIANCE)),
      T is the commanded exposure time (milliseconds), and
      To is the shutter-offset (milliseconds).

CCDRECIP solves for the sensitivity A and shutter-offset To, given data
points DN acquired by varying the light cannon setting (or spectral radiance of
the source) and exposure time:
         i
                DN - DC = A*L *(T -To)
                  i          i   i

.PAGE
OPERATION:

CCDRECIP performs the following steps:

  1) Read data from the Reciprocity File.
  2) Compute the sensitivity and shutter offset for each area.
  3) Compute mean values for sensitivity and shutter-offset (by averaging
     the values extracted from each area) and flag all areas deviating
     by more than 1 or 2 sigma from the mean.
  4) Re-compute the mean value for sensitivity and shutter-offset,
     ignoring all flagged values as specified by the REJECT parameter.

If the REJECT parameter is specified (default=2), areas may be rejected
because of a bad value for sensitivity (REJECT=1), shutter-offset (REJECT=2),
or either (REJECT=3).  If REJECT=0, no area rejection is performed.

CCDRECIP prints out the following:

  1) Sensitivity and shutter-offset for each area.
  2) Summary of all areas with bad values for sensitivity or shutter-offset.
  3) Mean sensitivity as a function of exposure time.
  4) Global value for sensitivity and shutter-offset, obtained by combining
     data from all good areas.
  5) Shutter-offset as a function of image line or sample number.

Note that the sensitivity and offset are listed as AO and TOS in the printout.

If the PLOT keyword is specified, CCDRECIP produces the following plots:

  1) (DN-DC)/L vs SHUTTER TIME
  2) (DN-DC)/(L*T) vs SHUTTER TIME
  3) (DN-DC)/[L*(T-To)] vs SHUTTER TIME
  4) To vs image line or sample number.  The raw points are plotted with
     "+" and the average shutter offset at a given line or sample number
     is plotted as a solid line.

Four types of tabular output data are also available.  The AREATBL
parameter produces a tab-delimitted ASCII text file containing:
if UNITS = LUMINANC, MEAN_DN(D), LUM(L), EXP(T), L*T, ACTUAL(T-To), D/L,
D/L*T, and D/L*(T-To), and if UNITS = RADIANCE, MEAN_DN(D), RAD(L), EXP(T),
L*T, ACTUAL(T-To), D/L, D/L*T, and D/L*(T-To), for each exposure level.

The OFFTBL parameter produces a tab-delimitted ASCII text file containing:
LINE or SAMPLE and calculated SHUTTER_OFFSET for all good areas.

The AVOFFTBL parameter produces a tab-delimitted ASCII text file containing:
LINE or SAMPLE and mean SHUTTER_OFFSET for each row or column of grid areas.

The CORRTBL parameters produces two files tabulating the correction
achieved as a result of using the derived shutter-offset.  The first
of the two files holds the uncorrected sensitivity values averaged over
each column or row (see DIRECTIO parameter) of grid points.  The second
file holds the corrected values.  If DIRECTIO is LINE, then the values
of the grid rows are averaged.  If SAMP, then the values of the grid
columns are averaged.

If an output file is specified, the centers of all flagged areas (as
specified by the REJECT parameter) are stored in a MARK-format tiepoint
data set.  These areas can be subsequently displayed (see example below)
to indicate the spatial distribution of regions which give rise to bad
sensitivity or shutter-offset constants.

If an output shutter-offset file is specified via the OFFSET parameter,
then a file containing shutter offsets for each image line or sample is
generated.  These offsets are calculated by using the average shutter-offsets
as found above and then performing a piece-wise linear interpolation for the
lines or samples that fall between data points.  A linear extrapolation is
done at each end using the first and last two points.  These shutter-offsets
can later be used as inputs to GALGEN, see GALGEN's TUTOR and HELP files.

Instead of entering the light values of the exposure levels as a
multivalued parameter, they can be contained in an ASCII file.  This file
merely contains one light value per record (see procedure MOMGEN2 for
the format details).  The file is specified to CCDRECIP using the LTFILE
parameter.

NOTE:  The first value in the LIGHT parameter set or the LTFILE
       file should be 0.0 to correspond with EXPO=0.0 for the dark
       current frames.

The parameter DIRECTIO is used to tell CCDRECIP to derive a Line- or a
Sample-dependent shutter-offset.

Because CCDRECIP is dealing with the Light Transfer File and not the raw
images, it doesn't know how big they are.  Therefore, the user must tell
CCDRECIP how many elements the shutter-offset should contain.  This is
done with the ARRAYSIZ parameter.

NOTE:  CCDRECIP CANNOT be used with reciprocity sequences that contain
       extended dark current or extended exposure frames.
.PAGE
PLOT OUTPUTS

    The other type of output come from the PLOT and PLOTFMT parameters.
PLOT allows the user to display offset data in 4 formats on an x,y
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
  for the PLOT parameter.  The program will supply the extensions.

  For example, if the user has an input file of indata.dat and  PLOT=outplot
  then for the interactive plot the following files are produced:

     outplot.gpi
     indata.dat.asc

  The first file is the gnuplot instruction file and the second is the
  data file used by gnuplot.      

  If the user wanted an encapsulate postscript file with PLOTFMT=eps
  then the following files are produced:

     outplot.eps.gpi
     indata.dat.asc

  Remember entering the following command gives the eps file, outplot.eps

  ush gnuplot outplot.eps.gpi

  If you move the gpi file to another directory, you must also move the
  input data file, indata.dat.asc to the same directory.

  Note that the gpi file produced by this program has the name of the
  input file embedded in the plot command inside the gpi file, e.g..

  plot  'indata.dat.asc' u  1: 9 t .......


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

  Note: This program creates 7 output plots per run. You bring up each plot
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
EXAMPLE:

    CCDRECIP RCP.DAT MRK.DAT PLOT=RCP.PLT OFFSETS=OFFSETS.DAT
    MARK (PIC,MRK.DAT) OUT              !Scribe boxes around bad centers
.

.PAGE
ORIGINAL PROGRAMMER: Mike Morrill, Oct 84
COGNIZANT PROGRAMMER: Ray Bambery

REVISION HISTORY:
 13 Jul 2013  R. J. Bambery  Fix yrang1 and yrang2 values when their
                            diffs le 0.001.  Adjusted eps format to more 
                            readable fonts. Remove vestiges of debug
                            statements.
 08 Jul 2013  R. J. Bambery  Rename ascii output files
 06 Jul 2013  R. J. Bambery  Fix ymax, ymin ranges for plots
 13 Feb 2013  R. J. Bambery  Update documentation, tests
 25 Nov 2012  R. J. Bambery..Linux 64-bit changes, incorporate gnuplot
 27 Apr 99  gmy  Declared P as integer to avoid compiler error on SGI
 25 Mar 97...T.Huang........Ported from VAX to UNIX to support both
                            Cassini and Galileo data.
  1 Jan 97...c.c.avis.......allow rectangular grids
 16 Jul 96...c.c.avis.......added correction tables by row or column
 29 APR 96...c.c.avis.......changed decimal places in output table
 24 APR 96...c.c.avis.......change f12.5 to g20.12 in reading LTFILE
 22 AUG 95...c.c.avis.......Added tests involving noise
 02 JAN 95...J.R.YOSHIMIZU..Changed LUMINANC to LIGHT and LUMFILE to LTFILE.
                            Added UNITS
 21 DEC 94...C.C.Avis.......Clarified Help on Reject parameter.
 20 JUN 94...C.C.Avis.......Fixed xladd to SO file not mark file (bug),
                            added table outputs, added use of LUMFILE,
                            added sample-dependent shutter-offset
 26 APR 88...G.M.Yagi.......Added more documentation to help file.
 04 Nov 87...G.M.Yagi.......Shutter offset file changed to Image format.
 01 Nov 87...G.M.Yagi.......Convert to new CPLT plotting routines.
 14 JAN 87...G.M.Yagi.......Fix so plot is optional.
  1 AUG 86...G.M.Yagi.......Code and documentation clean-up.
 29 OCT 85...R.A.MORTENSEN..Added output of all 800 shutter offsets.
 26 FEB 85...M.E.MORRILL....ADD PLOT OF GLOBAL SHUTTER OFFSET.
 15 FEB 85...M.E.MORRILL....ADD SIGMA TOLERANCE PARAMETER.
 26 JAN 85...M.E.MORRILL....VERSION 1*A RELEASED FOR USE.
 14 JAN 85...M.E.MORRILL....ENLARGED BUFFERS FOR 400 AREAS.
  7 JAN 85...M.E.MORRILL....MARK OUTPUT FOR REJECTED AREAS.
 21 DEC 84...M.E.MORRILL....PLOTING PACKAGES ADDED.
 13 DEC 84...M.E.MORRILL....USES GOOD AREAS TO TABULATE RESULTS.
  2 NOV 84...M.E.MORRILL....TRACKS REJECTED AREAS WITH
                              3 CLASSES: AO,TOS, BOTH.
  8 OCT 84...M.E.MORRILL....INITIAL RELEASE.

.LEVEL1
.VARIABLE INP
The Reciprocity File
created by LTGEN/MOMGEN
.VARIABLE OUT
A MARK-format file
contining centers of
rejected areas.
.VARIABLE PLOT
Output plot file
.VARIABLE PLOTFMT
 Output plot format
 GNUPLOT or EPS
.VARIABLE SIGTOL
Specifies 1 or 2 Sigma
rejection from mean values.
.VARIABLE PLOTFMT
 Output plot format
 GNUPLOT or EPS
.VARIABLE REJECT
Specifies whether to
reject areas based on
bad sensitivity,
bad shutter-offset, or
either, or no rejection.
.VARIABLE UNITS
Specifies whether the
illumination values are
RADIANCE or LUMINANC
.VARIABLE LIGHT
Illumination values in
Relative Foot-Lamberts
or picoamp)
First value=0.0 for DC.
.VARIABLE LTFILE
Name of file containing
list of illumination
values in Relative
Foot-Lamberts or picoamp).
.VARIABLE DIRECTIO
Direction of shutter
movement.
.VARIABLE ARRAYSIZ
Number of pixels in
the direction of
shutter movement.
.VARIABLE AREATBL
File to receive table
of stats for each
area.
.VARIABLE OFFTBL
File to receive table
of pixel number vs.
calculated offset.
.VARIABLE AVOFFTBL
File to receive table
of pixel number vs.
calculated offset
averaged by row or
column.
.VARIABLE CORRTBL
Files to receive the
uncorrected and corrected
sensitivity values by
grid row or column.
.VARIABLE OFFSETS
Specifies the name
of an output file to
receive the shutter
offsets for each image
line or sample.
.LEVEL2
.VARIABLE INP
The Reciprocity File created by LTGEN and MOMGEN
containing area statistics for calculating the sensitivity
and shutter-offset.
.VARIABLE OUT
A MARK formatted data set containing rejected area locations.
.VARIABLE SIGTOL
Specifies the number of standard deviations from the mean which
signifies a bad area.  See REJECT parameter.
.VARIABLE REJECT
REJECT=0  No area rejection performed
      =1  Reject areas with bad sensitivity
      =2  Reject areas with bad shutter-offset
      =3  Reject areas with either bad sensitivity or offset
Areas with values differing from the mean by more than SIGTOL sigma are
rejected.
.VARIABLE UNITS
Specifies whether the illumination values are in LUMINANC (Relative-Foot-
Lamberts ) or RADIANCE (picoamp).
(UNITS should be LUMINANC for Galileo and RADIANCE for Cassini).
.VARIABLE LIGHT
The illumination (in Relative Foot Lamberts or picoamp)
for each exposure level of the reciprocity sequence.  The first entry should
be 0.0, corresponding to EXPO=0.0 in the reciprocity file for the Dark Current
frames.
.VARIABLE LTFILE
Name of file containing list of illumination values in Relative Foot-Lamberts
or picoamp).  This is an ASCII text file containing one
record for each exposure level from dark-current (record 1) to the highest
exposure level (the last record).  Each record contains one floating point
value denoting the illumination value for that exposure level.

.VARIABLE DIRECTIO
Specifies whether to derive a line-dependent or a sample-dependent shutter-
offset.  This corresponds to the direction of shutter movement (LINE or
SAMP).

.VARIABLE ARRAYSIZ
Specifies how many elements to calculate for the output shutter-offset file.
This correspondes to the number of image pixels in the direction of shutter
movement (i.e., the number of image lines or the number of image samples).

.VARIABLE PLOT
Specifies the name of a file to contain the plot data when the plotting
device is specified as the printer (i.e., PLOTTING 'PRINTRONX) or other
devices besides the display monitor.  For the printer, the file must be
printed using the /NOFEED qualifier.

.VARIABLE AREATBL
The AREATBL parameter produces a tab-delimitted ASCII text file containing:
MEAN_DN(D), LUM(L), EXP(T), L*T, (T-To), D/L, D/L*T, and D/L*(T-To)
for each exposure level.

.VARIABLE OFFTBL
The OFFTBL parameter produces a tab-delimitted ASCII text file containing:
For line-dependent shutter-offsets: Line number and calculated SHUTTER_OFFSET
for all good areas.
For sample-dependent shutter-offsets: Sample number and calculated SHUTTER_
OFFSET for all good areas.

.VARIABLE AVOFFTBL
The AVOFFTBL parameter produces a tab-delimitted ASCII text file containing:
For line-dependent shutter-offsets: Line number and mean SHUTTER_OFFSET for
each row of grid areas.
For sample-dependent shutter-offsets: Sample number and mean SHUTTER_OFFSET for
 each column of grid areas.

.VARIABLE CORRTBL
STRING - COUNT= 0:2 OPTIONAL
Specifies the two files two contain the uncorrected and the corrected
sensitivity values averaged by grid column or row.  The DIRECTIO parameter
specifies whether averaging is done by row or column.  If DIRECTIO is LINE,
then the values of the grid rows are averaged.  If SAMP, then the values
of the grid columns are averaged.

.VARIABLE OFFSETS
Specifies the name of an output file that will receive the shutter-offsets for
each image line or sample from 1 to ARRAYSIZ.  The file is in standard VICAR
image format and is used as an input to programs CCDSLOPE, GALGEN, and GALSOS.
.END

$ Return
$!#############################################################################
$Test_File:
$ create tstccdrecip.pdf
procedure
parm    mode    type=keyword count=(0:1) valid=(batch,nobatch,inter) default=inter
local   afidsroot   type=string count=1
! Feb 18, 2013 - RJB
! TEST SCRIPT FOR TSTCCDNOISE
! tests HALF images
!
! Vicar Programs:
!       f2 copy label-rep createfile addtofile
!       reset ltgen momgen2 typeit gausnois
!
! External programs
!       gnuplot 4.6.x 
!       gimp for eps file
!
! Parameters:
!   mode - method for processing: 
!       1) batch provides no xvd display
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

refgbl $autousage
refgbl $echo
refgbl $syschar
body
let $autousage="none"
let _onfail="goto rm"
let $echo="yes"

!check to see if mipl or cartlab for certain programs
!cartlab defines env var $AFIDS_ROOT, mipl doesm't
translog INP=AFIDS_ROOT TRANS=afidsroot

if (afidsroot = "")
!MIPL        
    ush ln -s /project/test_work/testdata/cassini/iss cas 
else         
!CARTLAB     
    ush ln -s /raid1/vicar_test_images/testdata/cassini/iss cas
end-if       
            
  defcmd-replace typeit "ush cat"
            

!=================================================================
!First, check the results from simulated data to verify algoritm.
!Second, see that the sample- and line-dependent modes get the
!same answer.
!=================================================================
 
!CASSINI TEST:
 
!---------------------------
! Make TEST.LTF a test reciprocity file which has exposure levels of
! 0,10,20,40 and each input frame was 10, 910, 960, 985 dn, with
! radiances of 0,100,50,25 respectively.
!
! A sequence with a shutter-offset of zero would have DNs of exactly
! Radiance*EXP + DC or (10, 1010, 1010, 1010) assuming a sensitivity of 1.0.
! The use of uniform images of (10, 910, 960, 985) DN should give a
! shutter-offset of 1.0.
!---------------------------
 
!Set dns to 10 and replicate - exposure = 0.0
f2 inp=cas/sum2.1 out=l1.a func=10
copy l1.a l1.b
copy l1.a l1.c
 
!Set dns to 910 and replicate - set exposure to 10, radiance to 100
f2 inp=cas/sum2.1 out=l2.a func=910
label-rep l2.a 'prop property="CASSINI-ISS" item="EXPOSURE_DURATION=10. +
            RADIANCE=100."
copy l2.a l2.b
copy l2.a l2.c
 
!Set dns to 960 and replicate - set exposure to 20, radiance to 50
f2 inp=cas/sum2.1 out=l3.a func=960
label-rep l3.a 'prop property="CASSINI-ISS" item="EXPOSURE_DURATION=20. +
            RADIANCE=50."
copy l3.a l3.b
copy l3.a l3.c
 
!Set dns to 985 and replicate - set exposure to 40, radiance to 25
f2 inp=cas/sum2.1 out=l4.a func=985
label-rep l4.a 'prop property="CASSINI-ISS" item="EXPOSURE_DURATION=40. +
            RADIANCE=25."
copy l4.a l4.b
copy l4.a l4.c
 
!Create list of the files created
createfile l.list
addtofile l.list "NEXT FILE=0001"
addtofile l.list "l1.a"
addtofile l.list "l1.b"
addtofile l.list "l1.c"
addtofile l.list "l2.a"
addtofile l.list "l2.b"
addtofile l.list "l2.c"
addtofile l.list "l3.a"
addtofile l.list "l3.b"
addtofile l.list "l3.c"
addtofile l.list "l4.a"
addtofile l.list "l4.b"
addtofile l.list "l4.c"
reset l.list
typeit l.list 
 
!Initialize Light Transfer File
ltgen l1.a out=test.ltf list=l.list 'grid
 
!Fill Light Transfer File with stats
momgen2 list=l.list ltfrcp=test.ltf

! TEST 1  - Raw Cassini data
ccdrecip test.ltf 'SAMP  light=(0,100,50,25) 'RADIANCE +
 arraysiz=512  offset=so.file  areatbl=area.tbl +
 offtbl=off.tbl  avofftbl=meanoff.tbl +
 plot=test1

typeit area.tbl
typeit off.tbl
typeit meanoff.tbl

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test1.gpi
end-if


!---------------------------
! Repeat with noise-added input data
!---------------------------
!Set dns to 10 and replicate - exposure = 0.0
 
gausnois a.dat mean=0 sigma=3 format=half seed=13 nl=512 ns=512
f2 (cas/sum2.1, a.dat) m1.a func=10+in2
gausnois a.dat mean=0 sigma=3 format=half seed=17 nl=512 ns=512
f2 (cas/sum2.1, a.dat) m1.b func=10+in2
gausnois a.dat mean=0 sigma=3 format=half seed=19 nl=512 ns=512
f2 (cas/sum2.1, a.dat) m1.c func=10+in2
 
!Set dns to 910 and replicate - set exposure to 10, radiance to 100
gausnois a.dat mean=0 sigma=30 format=half seed=13 nl=512 ns=512
f2 (cas/sum2.1, a.dat) m2.a func=910+in2
label-rep m2.a 'prop property="CASSINI-ISS" +
    item="EXPOSURE_DURATION=10. RADIANCE=100."
gausnois a.dat mean=0 sigma=30 format=half seed=17 nl=512 ns=512
f2 (cas/sum2.1, a.dat) m2.b func=910+in2
label-rep m2.b 'prop property="CASSINI-ISS" +
    item="EXPOSURE_DURATION=10. RADIANCE=100."
gausnois a.dat mean=0 sigma=30 format=half seed=19 nl=512 ns=512
f2 (cas/sum2.1, a.dat) m2.c func=910+in2
label-rep m2.c 'prop property="CASSINI-ISS" +
    item="EXPOSURE_DURATION=10. RADIANCE=100."
 
!Set dns to 960 and replicate - set exposure to 20, radiance to 50
gausnois a.dat mean=0 sigma=30 format=half seed=13 nl=512 ns=512
f2 (cas/sum2.1, a.dat) m3.a func=960+in2
label-rep m3.a 'prop property="CASSINI-ISS" +
    item="EXPOSURE_DURATION=20. RADIANCE=50."
gausnois a.dat mean=0 sigma=30 format=half seed=17 nl=512 ns=512
f2 (cas/sum2.1, a.dat) m3.b func=960+in2
label-rep m3.b 'prop property="CASSINI-ISS" +
    item="EXPOSURE_DURATION=20. RADIANCE=50."
gausnois a.dat mean=0 sigma=30 format=half seed=19 nl=512 ns=512
f2 (cas/sum2.1, a.dat) m3.c func=960+in2
label-rep m3.c 'prop property="CASSINI-ISS" +
    item="EXPOSURE_DURATION=20. RADIANCE=50."
 
!Set dns to 985 and replicate - set exposure to 40, radiance to 25
gausnois a.dat mean=0 sigma=30 format=half seed=13 nl=512 ns=512
f2 (cas/sum2.1, a.dat) m4.a func=985+in2
label-rep m4.a 'prop property="CASSINI-ISS" +
    item="EXPOSURE_DURATION=40. RADIANCE=25."
gausnois a.dat mean=0 sigma=30 format=half seed=17 nl=512 ns=512
f2 (cas/sum2.1, a.dat) m4.b func=985+in2
label-rep m4.b 'prop property="CASSINI-ISS" +
    item="EXPOSURE_DURATION=40. RADIANCE=25."
gausnois a.dat mean=0 sigma=30 format=half seed=19 nl=512 ns=512
f2 (cas/sum2.1, a.dat) m4.c func=985+in2
label-rep m4.c 'prop property="CASSINI-ISS" +
    item="EXPOSURE_DURATION=40. RADIANCE=25."
 
!Create list of the files created
createfile m.list
addtofile m.list "NEXT FILE=0001"
addtofile m.list "m1.a"
addtofile m.list "m1.b"
addtofile m.list "m1.c"
addtofile m.list "m2.a"
addtofile m.list "m2.b"
addtofile m.list "m2.c"
addtofile m.list "m3.a"
addtofile m.list "m3.b"
addtofile m.list "m3.c"
addtofile m.list "m4.a"
addtofile m.list "m4.b"
addtofile m.list "m4.c"
reset m.list 
typeit m.list
 
!Initialize Light Transfer File
ltgen m1.a out=testm.ltf list=m.list 'GRID
 
!Fill Light Transfer File with stats
momgen2 list=m.list ltfrcp=testm.ltf

! TEST 2 - Cassini with added noise
! 
ccdrecip testm.ltf 'SAMP  light=(0,100,50,25) 'RADIANCE +
 arraysiz=512  offset=so.file  areatbl=aream.tbl +
 offtbl=offm.tbl  avofftbl=meanoffm.tbl +
  plot=test2

typeit aream.tbl
typeit offm.tbl
typeit meanoffm.tbl
! Note xrt graph created a dynamic range for plot
! so small differences didn't show on plotting
! appeared to be a straight line at 1.0
! gnuplot shows the small differences in its plot
if (mode = "nobatch" or mode = "inter")
    ush gnuplot test2.gpi
end-if
!goto rm 
! Left the following code in for historical reasons

!---------------------------
! Deriving the sample-dependent shutter-offset
!---------------------------
!Introduce a shutter offset in the sample direction by creating a
! ramp in DN in the sample direction.  At each exposure level, use
! a different amount of ramping across the 512 samples.  A ramp of101 DN
! for exp=100, 51 for exp=50 and 26 for exp=26 will be used to generate
! a shutter-offset which goes from 1.0 at sample 1 to 2.0 at sample 512.
! (i.e., an average offset of 1.5)
!---------------------------
 
gen x.dat nl=512 ns=256 ival=0 linc=0 sinc=1
 
copy x.dat a.dat (1,1,512,101)
size a.dat xa.dat (1,1,512,512)
f2 (l2.a, xa.dat) l6.a func=in1-in2
maxmin l6.a (1,1,1,512)
copy l6.a l6.b
copy l6.a l6.c
 
copy x.dat a.dat (1,1,512,51)
size a.dat xa.dat (1,1,512,512)
f2 (l3.a, xa.dat) l7.a func=in1-in2
maxmin l7.a (1,1,1,512)
copy l7.a l7.b
copy l7.a l7.c
 
copy x.dat a.dat (1,1,512,26)
size a.dat xa.dat (1,1,512,512)
f2 (l4.a, xa.dat) l8.a func=in1-in2
maxmin l8.a (1,1,1,512)
copy l8.a l8.b
copy l8.a l8.c
 
!Create list of the files created
createfile n.list
addtofile n.list "NEXT FILE=0001"
addtofile n.list "l1.a"
addtofile n.list "l1.b"
addtofile n.list "l1.c"
addtofile n.list "l6.a"
addtofile n.list "l6.b"
addtofile n.list "l6.c"
addtofile n.list "l7.a"
addtofile n.list "l7.b"
addtofile n.list "l7.c"
addtofile n.list "l8.a"
addtofile n.list "l8.b"
addtofile n.list "l8.c"
reset n.list
typeit n.list
 
!Initialize Light Transfer File
ltgen l1.a out=test.ltf list=n.list 'GRID
 
!Fill Light Transfer File with stats
momgen2 list=n.list ltfrcp=test.ltf

! TEST 3  - Genned CCD data
ccdrecip test.ltf 'SAMP  light=(0,100,50,25) 'RADIANCE +
 arraysiz=512  offset=so.file  areatbl=area.tbl +
 offtbl=off.tbl  avofftbl=meanoff.tbl reject=0 +
 plot=test3

typeit area.tbl
typeit off.tbl
typeit meanoff.tbl

!
!  TEST 4 - Repeat Test 3, but eps
ccdrecip test.ltf 'SAMP  light=(0,100,50,25) 'RADIANCE +
 arraysiz=512  offset=so.file  areatbl=area.tbl +
 offtbl=off.tbl  avofftbl=meanoff.tbl reject=0 +
 plot=test4 plotfmt=eps

ush gnuplot test4.eps.gpi
goto rm 
if ($syschar(1) = "UNIX")
   ush rm l1.*
   ush rm l2.*
   ush rm l3.*
   ush rm l4.*
   ush rm l.list
   ush rm m.list
   ush rm n.list
   ush rm l6.*
   ush rm l7.*
   ush rm l8.*
else
   dcl del l1.*;*
   dcl del l2.*;*
   dcl del l3.*;*
   dcl del l4.*;*
   dcl del l.list;*
   dcl del l6.*;*
   dcl del l7.*;*
   dcl del l8.*;* 
end-if
 
!=================================================================
 
!make galileo style (line-dependent shutter-offset) RCP file
ltgen &"dir"rcp_5.byte rcp.tst 'GRID ni=3 +
   exp=(0,4,6,8,12,17,25,33,50,67,100)
 
momgen +
  (&"dir"rcp_1.byte,&"dir"rcp_2.byte,&"dir"rcp_3.byte) +
  rcp.tst exp=0
momgen +
  (&"dir"rcp_4.byte,&"dir"rcp_5.byte,&"dir"rcp_6.byte) +
  rcp.tst exp=4
momgen +
  (&"dir"rcp_7.byte,&"dir"rcp_8.byte,&"dir"rcp_9.byte) +
  rcp.tst exp=6
momgen +
  (&"dir"rcp_10.byte,&"dir"rcp_11.byte,&"dir"rcp_12.byte) +
  rcp.tst exp=8
momgen +
  (&"dir"rcp_13.byte,&"dir"rcp_14.byte,&"dir"rcp_15.byte) +
  rcp.tst exp=12
momgen +
  (&"dir"rcp_16.byte,&"dir"rcp_17.byte,&"dir"rcp_18.byte) +
  rcp.tst exp=17
momgen +
  (&"dir"rcp_19.byte,&"dir"rcp_20.byte,&"dir"rcp_21.byte) +
  rcp.tst exp=25
momgen +
  (&"dir"rcp_22.byte,&"dir"rcp_23.byte,&"dir"rcp_24.byte) +
  rcp.tst exp=33
momgen +
  (&"dir"rcp_25.byte,&"dir"rcp_26.byte,&"dir"rcp_27.byte) +
  rcp.tst exp=50
momgen +
  (&"dir"rcp_28.byte,&"dir"rcp_29.byte,&"dir"rcp_30.byte) +
  rcp.tst exp=67
momgen +
  (&"dir"rcp_31.byte,&"dir"rcp_32.byte,&"dir"rcp_33.byte) +
  rcp.tst exp=100
 
 
!make cassini style (sample-dependent shutter-offset) RCP file
!make sure the flotted pixels wind up in the proper place
!for the new grid to use them
 
 
flot &"dir"rcp_1.byte ax.dat 'coun
fastmos ax.dat a.dat off1=(-5,1)
flot &"dir"rcp_2.byte ax.dat 'coun
fastmos ax.dat b.dat off1=(-5,1)
flot &"dir"rcp_3.byte ax.dat 'coun
fastmos ax.dat c.dat off1=(-5,1)
 
ltgen b.dat rcps.tst 'GRID ni=3 exp=(0,4,6,8,12,17,25,33,50,67,100)
 
momgen (a.dat,b.dat,c.dat) rcps.tst exp=0
flot &"dir"rcp_4.byte ax.dat 'coun
fastmos ax.dat a.dat off1=(-5,1)
flot &"dir"rcp_5.byte ax.dat 'coun
fastmos ax.dat b.dat off1=(-5,1)
flot &"dir"rcp_6.byte ax.dat 'coun
fastmos ax.dat c.dat off1=(-5,1)
momgen (a.dat,b.dat,c.dat) rcps.tst exp=4
flot &"dir"rcp_7.byte ax.dat 'coun
fastmos ax.dat a.dat off1=(-5,1)
flot &"dir"rcp_8.byte ax.dat 'coun
fastmos ax.dat b.dat off1=(-5,1)
flot &"dir"rcp_9.byte ax.dat 'coun
fastmos ax.dat c.dat off1=(-5,1)
momgen (a.dat,b.dat,c.dat) rcps.tst exp=6
flot &"dir"rcp_10.byte ax.dat 'coun
fastmos ax.dat a.dat off1=(-5,1)
flot &"dir"rcp_11.byte ax.dat 'coun
fastmos ax.dat b.dat off1=(-5,1)
flot &"dir"rcp_12.byte ax.dat 'coun
fastmos ax.dat c.dat off1=(-5,1)
momgen (a.dat,b.dat,c.dat) rcps.tst exp=8
flot &"dir"rcp_13.byte ax.dat 'coun
fastmos ax.dat a.dat off1=(-5,1)
flot &"dir"rcp_14.byte ax.dat 'coun
fastmos ax.dat b.dat off1=(-5,1)
flot &"dir"rcp_15.byte ax.dat 'coun
fastmos ax.dat c.dat off1=(-5,1)
momgen (a.dat,b.dat,c.dat) rcps.tst exp=12
flot &"dir"rcp_16.byte ax.dat 'coun
fastmos ax.dat a.dat off1=(-5,1)
flot &"dir"rcp_17.byte ax.dat 'coun
fastmos ax.dat b.dat off1=(-5,1)
flot &"dir"rcp_18.byte ax.dat 'coun
fastmos ax.dat c.dat off1=(-5,1)
momgen (a.dat,b.dat,c.dat) rcps.tst exp=17
flot &"dir"rcp_19.byte ax.dat 'coun
fastmos ax.dat a.dat off1=(-5,1)
flot &"dir"rcp_20.byte ax.dat 'coun
fastmos ax.dat b.dat off1=(-5,1)
flot &"dir"rcp_21.byte ax.dat 'coun
fastmos ax.dat c.dat off1=(-5,1)
momgen (a.dat,b.dat,c.dat) rcps.tst exp=25
flot &"dir"rcp_22.byte ax.dat 'coun
fastmos ax.dat a.dat off1=(-5,1)
flot &"dir"rcp_23.byte ax.dat 'coun
fastmos ax.dat b.dat off1=(-5,1)
flot &"dir"rcp_24.byte ax.dat 'coun
fastmos ax.dat c.dat off1=(-5,1)
momgen (a.dat,b.dat,c.dat) rcps.tst exp=33
flot &"dir"rcp_25.byte ax.dat 'coun
fastmos ax.dat a.dat off1=(-5,1)
flot &"dir"rcp_26.byte ax.dat 'coun
fastmos ax.dat b.dat off1=(-5,1)
flot &"dir"rcp_27.byte ax.dat 'coun
fastmos ax.dat c.dat off1=(-5,1)
momgen (a.dat,b.dat,c.dat) rcps.tst exp=50
flot &"dir"rcp_28.byte ax.dat 'coun
fastmos ax.dat a.dat OFF1=(-5,1)
flot &"dir"rcp_29.byte ax.dat 'coun
fastmos ax.dat b.dat off1=(-5,1)
flot &"dir"rcp_30.byte ax.dat 'coun
fastmos ax.dat c.dat off1=(-5,1)
momgen (a.dat,b.dat,c.dat) rcps.tst exp=67
flot &"dir"rcp_31.byte ax.dat 'coun
fastmos ax.dat a.dat off1=(-5,1)
flot &"dir"rcp_32.byte ax.dat 'coun
fastmos ax.dat b.dat off1=(-5,1)
flot &"dir"rcp_33.byte ax.dat 'coun
fastmos ax.dat c.dat off1=(-5,1)
momgen (a.dat,b.dat,c.dat) rcps.tst exp=100

!TEST 4 -RUN CCDRECIP IN LINE- AND SAMPLE-DEPENDENT CASES AND COMPARE
 
 
!THE OUTPUT OF THE LINE-DEPENDENT RUNS AND THE SAMPLE-DEPENDENT RUNS
!MAY BE DIRECTLY COMPARED EXCEPT FOR THE AREA NUMBERS.  BECAUSE THE
!INPUT DATA HAS BEEN FLOTTED FOR THE SAMPLE-DEPENDENT CASE, THE PIXELS
!THAT WERE IN AREA X ARE NOW IN AREA Y.  SO THE RESULTS FOR AREAS X AND Y
!CAN BE COMPARED.
 
!            LINE CASE                   SAMP CASE
!      X = grid_row,grid_col  -->    11-grid_col,grid_row = Y
! e.g.
!      AREA 19 = (2,9)        -->    (2,2) = AREA 12
 
!MAKE THE LIGHT FILE FOR INPUT LATER
createfile lt.dat 
addtofile lt.dat "0.0"
addtofile lt.dat "74.5"
addtofile lt.dat "50.5"
addtofile lt.dat "38.7"
addtofile lt.dat "26.0"
addtofile lt.dat "20.0"
addtofile lt.dat "12.8"
addtofile lt.dat "9.9"
addtofile lt.dat "6.8"
addtofile lt.dat "5.2"
addtofile lt.dat "3.6"

typeit lt.dat
 
!RUN IN LINE-DEPENDENT MODE
! ---- rcp.tst is Recprocity file from ltgen/momgen containing
! ---- a line-dependent shutter-offset
ccdrecip rcp.tst rej=0 array=800 ltfile=lt.dat 'radiance +
 areatbl=al.tbl offtbl=ol.tbl avofftbl=vl.tbl
ccdrecip rcp.tst rej=1 array=800 ltfile=lt.dat 'radiance +
 areatbl=al1.tbl offtbl=ol1.tbl avofftbl=vl1.tbl
ccdrecip rcp.tst REJ=2 array=800 ltfile=lt.dat 'radiance +
 areatbl=al2.tbl offtbl=ol2.tbl avofftbl=vl2.tbl
ccdrecip rcp.tst REJ=3 areatbl=al3.tbl offtbl=ol3.tbl avofftbl=vl3.tbl +
 light=(0,74.5,50.5,38.7,26,20,12.8,9.9,6.8,5.2,3.6) 'radiance
 
!RUN IN SAMPLE-DEPENDENT MODE
! ---- rcps.tst is Recprocity file from ltgen/momgen containing
! ---- a sample-dependent shutter-offset.  It used flotted versions of the
! ---- inputs used in rcp.tst
ccdrecip rcps.tst 'samp ltfile=lt.dat 'radiance +
   rej=0 array=800 areatbl=as.tbl offtbl=os.tbl avofftbl=vs.tbl
ccdrecip rcps.tst  'samp ltfile=lt.dat 'radiance +
   rej=1 array=800 areatbl=as1.tbl offtbl=os1.tbl avofftbl=vs1.tbl
ccdrecip rcps.tst  'samp ltfile=lt.dat 'radiance +
   rej=2 array=800 areatbl=as2.tbl offtbl=os2.tbl avofftbl=vs2.tbl
ccdrecip rcps.tst  'samp +
   rej=3 areatbl=as3.tbl offtbl=os3.tbl avofftbl=vs3.tbl +
   light=(0,74.5,50.5,38.7,26,20,12.8,9.9,6.8,5.2,3.6) 'radiance
 
!COMPARE SOME TABLES BETWEEN THE MODES
if ($syschar(1) = "UNIX")
   ush diff al.tbl as.tbl
   ush diff vl.tbl vs.tbl
else
   dcl diff/para al.tbl,as.tbl
   dcl diff/para vl.tbl,vs.tbl
end-if
 
!TRY MARK OUTPUT AND PLOTS FOR BOTH MODES
! TEST 4A - line depedent mode
ccdrecip rcp.tst  out=ml.dat offset=sol.dat 'line rej=3 +
  ltfile=lt.dat array=1024 'radiance plot=line +
! TEST 4B - sample dependent mode
ccdrecip rcps.tst  out=ms.dat offset=sos.dat 'samp  REJ=3 +
  ltfile=lt.dat array=1024 'radiance plot=sample
 
!The plot files will have differences in the titles
!The mark files will have differences because the areas are flotted
!with respect to each other
 
!The output image files should be identical except for maybe
!roundoff error
difpic (sol.dat, sos.dat) sd

! TEST 5 - eps format
!
ccdrecip rcps.tst  out=ms.dat offset=sos.dat 'samp  REJ=3 +
  ltfile=lt.dat array=1024 'radiance plot=sample2 plotfmt=eps

let $echo="no"
rm>
  ush rm -f l.list
  ush rm -f m.list
  ush rm -f n.list
  ush rm -f cas
  ush rm -f test?.eps
  ush rm -f test*.gpi
  ush rm -f test*.asc
  ush rm -f list?.dat
  ush rm -f ?.dat
  ush rm -f xa.dat
  ush rm -f l?.?
  ush rm -f m?.?
  ush rm -f so.file
  ush rm -f *off*.tbl
  ush rm -f area*.tbl
  ush rm -f test*.ltf
end-proc

$!-----------------------------------------------------------------------------
$ create tstccdrecip.log_solos
tstccdrecip
translog INP=AFIDS_ROOT TRANS=afidsroot
if (afidsroot = "")
    ush ln -s /project/test_work/testdata/cassini/iss cas
else
end-if
  defcmd-replace typeit "ush cat"
f2 inp=cas/sum2.1 out=l1.a func=10
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 26 TIMES
copy l1.a l1.b
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
copy l1.a l1.c
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
f2 inp=cas/sum2.1 out=l2.a func=910
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 26 TIMES
label-rep l2.a 'prop property="CASSINI-ISS" item="EXPOSURE_DURATION=10.  +
            RADIANCE=100."
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword EXPOSURE_DURATION replaced
Keyword RADIANCE replaced
copy l2.a l2.b
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
copy l2.a l2.c
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
f2 inp=cas/sum2.1 out=l3.a func=960
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 26 TIMES
label-rep l3.a 'prop property="CASSINI-ISS" item="EXPOSURE_DURATION=20.  +
            RADIANCE=50."
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword EXPOSURE_DURATION replaced
Keyword RADIANCE replaced
copy l3.a l3.b
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
copy l3.a l3.c
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
f2 inp=cas/sum2.1 out=l4.a func=985
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 26 TIMES
label-rep l4.a 'prop property="CASSINI-ISS" item="EXPOSURE_DURATION=40.  +
            RADIANCE=25."
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword EXPOSURE_DURATION replaced
Keyword RADIANCE replaced
copy l4.a l4.b
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
copy l4.a l4.c
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
createfile l.list
refgbl  $syschar
write "CREATEFILE version 1-3-97"
CREATEFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  USH cp /dev/null  l.list
end-if
END-PROC
addtofile l.list "NEXT FILE=0001"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="NEXT FILE=0001"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile l.list "l1.a"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l1.a"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile l.list "l1.b"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l1.b"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile l.list "l1.c"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l1.c"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile l.list "l2.a"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l2.a"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile l.list "l2.b"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l2.b"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile l.list "l2.c"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l2.c"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile l.list "l3.a"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l3.a"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile l.list "l3.b"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l3.b"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile l.list "l3.c"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l3.c"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile l.list "l4.a"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l4.a"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile l.list "l4.b"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l4.b"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile l.list "l4.c"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l4.c"
Beginning VICAR task addtofil
end-if
END-PROC
reset l.list
Beginning VICAR task reset
ush cat l.list
ltgen l1.a out=test.ltf list=l.list 'grid
Beginning VICAR task ltgen
LTGEN Version 14-MAR-97
NUMBER OF FILES  =         12
NUMBER OF LEVELS =          4
MAX FRAMES/LEVEL =          3
NUMBER OF AREAS     =    100
NUMBER OF EXPOSURES =      4
EXPOSURES = 
            0.000E+00  1.000E+01  2.000E+01  4.000E+01
MAX FRAMES/LEVEL =           3
WRITING HALFWORD LIGHT TRANSFER FILE WITH
 NL (NREC) =           4
 NS        =         901
LTGEN task completed
momgen2 list=l.list ltfrcp=test.ltf
LOCAL (F,G,LUMS,EXS)      STRING
LOCAL (NLVL,I1,I2,I3,I,J) INTEGER
LOCAL (EXP,LM,X)          REAL
LOCAL EX                  REAL      COUNT=1:100
LOCAL LMS                 STRING    COUNT=1:100
if ($syschar(1)="UNIX")
   defcmd-replace typeit "ush cat"
else
end-if
reset l.list
Beginning VICAR task reset
nxt l.list F I1 I2 I3
Beginning VICAR task nxt
 Output 1 is l1.a
getlab l1.a lab_item="EXPOSURE_DURATION" itm_name=EXP 'PROPERTY+
         itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
let EX(1)=EXP
getlab l1.a lab_item="RADIANCE" itm_name=LM 'PROPERTY+
         itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
let LUMS = "5.099999904633e+00"
let LMS(1)=LUMS
createfile list1.dat
refgbl  $syschar
write "CREATEFILE version 1-3-97"
CREATEFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  USH cp /dev/null  list1.dat
end-if
END-PROC
addtofile list1.dat "NEXT FILE=0001"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="NEXT FILE=0001"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile list1.dat "l1.a"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l1.a"
Beginning VICAR task addtofil
end-if
END-PROC
let J=1
loop
   nxt l.list F I1 I2 I3
Beginning VICAR task nxt
 Output 2 is l1.b
   if (F="END_OF_FILE") break
   getlab l1.b lab_item="EXPOSURE_DURATION" itm_name=EXP 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   getlab l1.b lab_item="RADIANCE" itm_name=LM 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   let LUMS = "5.099999904633e+00"
   if (EXP=EX(J))
       let G="list"//"1"//".dat"
       addtofile list1.dat "l1.b"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l1.b"
Beginning VICAR task addtofil
end-if
END-PROC
   else
   end-if
end-loop
   nxt l.list F I1 I2 I3
Beginning VICAR task nxt
 Output 3 is l1.c
   if (F="END_OF_FILE") break
   getlab l1.c lab_item="EXPOSURE_DURATION" itm_name=EXP 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   getlab l1.c lab_item="RADIANCE" itm_name=LM 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   let LUMS = "5.099999904633e+00"
   if (EXP=EX(J))
       let G="list"//"1"//".dat"
       addtofile list1.dat "l1.c"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l1.c"
Beginning VICAR task addtofil
end-if
END-PROC
   else
   end-if
end-loop
   nxt l.list F I1 I2 I3
Beginning VICAR task nxt
 Output 4 is l2.a
   if (F="END_OF_FILE") break
   getlab l2.a lab_item="EXPOSURE_DURATION" itm_name=EXP 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   getlab l2.a lab_item="RADIANCE" itm_name=LM 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   let LUMS = "1.000000000000e+02"
   if (EXP=EX(J))
   else
       let J=J+1
       let EX(J)=EXP
       let LMS(J) = LUMS
       let G="list"//"2"//".dat"
       createfile list2.dat
refgbl  $syschar
write "CREATEFILE version 1-3-97"
CREATEFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  USH cp /dev/null  list2.dat
end-if
END-PROC
       addtofile list2.dat "NEXT FILE=0001"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="NEXT FILE=0001"
Beginning VICAR task addtofil
end-if
END-PROC
       addtofile list2.dat "l2.a"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l2.a"
Beginning VICAR task addtofil
end-if
END-PROC
   end-if
end-loop
   nxt l.list F I1 I2 I3
Beginning VICAR task nxt
 Output 5 is l2.b
   if (F="END_OF_FILE") break
   getlab l2.b lab_item="EXPOSURE_DURATION" itm_name=EXP 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   getlab l2.b lab_item="RADIANCE" itm_name=LM 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   let LUMS = "1.000000000000e+02"
   if (EXP=EX(J))
       let G="list"//"2"//".dat"
       addtofile list2.dat "l2.b"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l2.b"
Beginning VICAR task addtofil
end-if
END-PROC
   else
   end-if
end-loop
   nxt l.list F I1 I2 I3
Beginning VICAR task nxt
 Output 6 is l2.c
   if (F="END_OF_FILE") break
   getlab l2.c lab_item="EXPOSURE_DURATION" itm_name=EXP 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   getlab l2.c lab_item="RADIANCE" itm_name=LM 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   let LUMS = "1.000000000000e+02"
   if (EXP=EX(J))
       let G="list"//"2"//".dat"
       addtofile list2.dat "l2.c"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l2.c"
Beginning VICAR task addtofil
end-if
END-PROC
   else
   end-if
end-loop
   nxt l.list F I1 I2 I3
Beginning VICAR task nxt
 Output 7 is l3.a
   if (F="END_OF_FILE") break
   getlab l3.a lab_item="EXPOSURE_DURATION" itm_name=EXP 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   getlab l3.a lab_item="RADIANCE" itm_name=LM 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   let LUMS = "5.000000000000e+01"
   if (EXP=EX(J))
   else
       let J=J+1
       let EX(J)=EXP
       let LMS(J) = LUMS
       let G="list"//"3"//".dat"
       createfile list3.dat
refgbl  $syschar
write "CREATEFILE version 1-3-97"
CREATEFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  USH cp /dev/null  list3.dat
end-if
END-PROC
       addtofile list3.dat "NEXT FILE=0001"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="NEXT FILE=0001"
Beginning VICAR task addtofil
end-if
END-PROC
       addtofile list3.dat "l3.a"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l3.a"
Beginning VICAR task addtofil
end-if
END-PROC
   end-if
end-loop
   nxt l.list F I1 I2 I3
Beginning VICAR task nxt
 Output 8 is l3.b
   if (F="END_OF_FILE") break
   getlab l3.b lab_item="EXPOSURE_DURATION" itm_name=EXP 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   getlab l3.b lab_item="RADIANCE" itm_name=LM 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   let LUMS = "5.000000000000e+01"
   if (EXP=EX(J))
       let G="list"//"3"//".dat"
       addtofile list3.dat "l3.b"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l3.b"
Beginning VICAR task addtofil
end-if
END-PROC
   else
   end-if
end-loop
   nxt l.list F I1 I2 I3
Beginning VICAR task nxt
 Output 9 is l3.c
   if (F="END_OF_FILE") break
   getlab l3.c lab_item="EXPOSURE_DURATION" itm_name=EXP 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   getlab l3.c lab_item="RADIANCE" itm_name=LM 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   let LUMS = "5.000000000000e+01"
   if (EXP=EX(J))
       let G="list"//"3"//".dat"
       addtofile list3.dat "l3.c"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l3.c"
Beginning VICAR task addtofil
end-if
END-PROC
   else
   end-if
end-loop
   nxt l.list F I1 I2 I3
Beginning VICAR task nxt
 Output 10 is l4.a
   if (F="END_OF_FILE") break
   getlab l4.a lab_item="EXPOSURE_DURATION" itm_name=EXP 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   getlab l4.a lab_item="RADIANCE" itm_name=LM 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   let LUMS = "2.500000000000e+01"
   if (EXP=EX(J))
   else
       let J=J+1
       let EX(J)=EXP
       let LMS(J) = LUMS
       let G="list"//"4"//".dat"
       createfile list4.dat
refgbl  $syschar
write "CREATEFILE version 1-3-97"
CREATEFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  USH cp /dev/null  list4.dat
end-if
END-PROC
       addtofile list4.dat "NEXT FILE=0001"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="NEXT FILE=0001"
Beginning VICAR task addtofil
end-if
END-PROC
       addtofile list4.dat "l4.a"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l4.a"
Beginning VICAR task addtofil
end-if
END-PROC
   end-if
end-loop
   nxt l.list F I1 I2 I3
Beginning VICAR task nxt
 Output 11 is l4.b
   if (F="END_OF_FILE") break
   getlab l4.b lab_item="EXPOSURE_DURATION" itm_name=EXP 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   getlab l4.b lab_item="RADIANCE" itm_name=LM 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   let LUMS = "2.500000000000e+01"
   if (EXP=EX(J))
       let G="list"//"4"//".dat"
       addtofile list4.dat "l4.b"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l4.b"
Beginning VICAR task addtofil
end-if
END-PROC
   else
   end-if
end-loop
   nxt l.list F I1 I2 I3
Beginning VICAR task nxt
 Output 12 is l4.c
   if (F="END_OF_FILE") break
   getlab l4.c lab_item="EXPOSURE_DURATION" itm_name=EXP 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   getlab l4.c lab_item="RADIANCE" itm_name=LM 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   let LUMS = "2.500000000000e+01"
   if (EXP=EX(J))
       let G="list"//"4"//".dat"
       addtofile list4.dat "l4.c"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l4.c"
Beginning VICAR task addtofil
end-if
END-PROC
   else
   end-if
end-loop
   nxt l.list F I1 I2 I3
Beginning VICAR task nxt
 NXT encountered end of file
   if (F="END_OF_FILE") break
 break
   if (EXP=EX(J))
   end-if
end-loop
let NLVL=J
let $BECHO="NO"
write " NUMBER OF EXPOSURE LEVELS = 4"
 NUMBER OF EXPOSURE LEVELS = 4
write " "
 
write " "
 
write " EXPOSURE LEVEL ... EXPOSURE TIME ... RADIANCES"
 EXPOSURE LEVEL ... EXPOSURE TIME ... RADIANCES
let I=1
loop
   let X=EX(I)
   let EXS = "0.000000000000e+00"
   let LUMS=LMS(I)
   write "       1              0.000000000000e+00             5.099999904633e+00"
       1              0.000000000000e+00             5.099999904633e+00
   if (I=NLVL) break
   let I=I+1
end-loop
   let X=EX(I)
   let EXS = "1.000000000000e+01"
   let LUMS=LMS(I)
   write "       2              1.000000000000e+01             1.000000000000e+02"
       2              1.000000000000e+01             1.000000000000e+02
   if (I=NLVL) break
   let I=I+1
end-loop
   let X=EX(I)
   let EXS = "2.000000000000e+01"
   let LUMS=LMS(I)
   write "       3              2.000000000000e+01             5.000000000000e+01"
       3              2.000000000000e+01             5.000000000000e+01
   if (I=NLVL) break
   let I=I+1
end-loop
   let X=EX(I)
   let EXS = "4.000000000000e+01"
   let LUMS=LMS(I)
   write "       4              4.000000000000e+01             2.500000000000e+01"
       4              4.000000000000e+01             2.500000000000e+01
   if (I=NLVL) break
 break
end-loop
write " "
 
let $BECHO="YES"
let I=1
loop
   let X=EX(I)
   let EXS = "0.000000000000e+00"
   let G="list"//"1"//".dat"
   let $BECHO="NO"
   write "FRAME LIST FOR LEVEL 1    EXPOSURE TIME 0.000000000000e+00"
FRAME LIST FOR LEVEL 1    EXPOSURE TIME 0.000000000000e+00
ush cat list1.dat
   write " "
 
   let $BECHO="YES"
   momgen LIST="list"1".dat" out=test.ltf
Beginning VICAR task momgen
MOMGEN Version 19-MAR-1997

EXPOSURE TIME=    0.000E+00
INPUT FRAMES=             3
NUMBER OF AREAS=        100
   if (I=NLVL) break
   let I=I+1
end-loop
   let X=EX(I)
   let EXS = "1.000000000000e+01"
   let G="list"//"2"//".dat"
   let $BECHO="NO"
   write "FRAME LIST FOR LEVEL 2    EXPOSURE TIME 1.000000000000e+01"
FRAME LIST FOR LEVEL 2    EXPOSURE TIME 1.000000000000e+01
ush cat list2.dat
   write " "
 
   let $BECHO="YES"
   momgen LIST="list"2".dat" out=test.ltf
Beginning VICAR task momgen
MOMGEN Version 19-MAR-1997

EXPOSURE TIME=    10.000000
INPUT FRAMES=             3
NUMBER OF AREAS=        100
   if (I=NLVL) break
   let I=I+1
end-loop
   let X=EX(I)
   let EXS = "2.000000000000e+01"
   let G="list"//"3"//".dat"
   let $BECHO="NO"
   write "FRAME LIST FOR LEVEL 3    EXPOSURE TIME 2.000000000000e+01"
FRAME LIST FOR LEVEL 3    EXPOSURE TIME 2.000000000000e+01
ush cat list3.dat
   write " "
 
   let $BECHO="YES"
   momgen LIST="list"3".dat" out=test.ltf
Beginning VICAR task momgen
MOMGEN Version 19-MAR-1997

EXPOSURE TIME=    20.000000
INPUT FRAMES=             3
NUMBER OF AREAS=        100
   if (I=NLVL) break
   let I=I+1
end-loop
   let X=EX(I)
   let EXS = "4.000000000000e+01"
   let G="list"//"4"//".dat"
   let $BECHO="NO"
   write "FRAME LIST FOR LEVEL 4    EXPOSURE TIME 4.000000000000e+01"
FRAME LIST FOR LEVEL 4    EXPOSURE TIME 4.000000000000e+01
ush cat list4.dat
   write " "
 
   let $BECHO="YES"
   momgen LIST="list"4".dat" out=test.ltf
Beginning VICAR task momgen
MOMGEN Version 19-MAR-1997

EXPOSURE TIME=    40.000000
INPUT FRAMES=             3
NUMBER OF AREAS=        100
   if (I=NLVL) break
 break
end-loop
if ($count(LTFILE) = 1)
      if (EX(I)=0.0) let LUMS="0.0"
      if (I=NLVL) break
end-if
end-proc
ccdrecip test.ltf 'SAMP  light=(0,100,50,25) 'RADIANCE  +
 arraysiz=512  offset=so.file  areatbl=area.tbl  +
 offtbl=off.tbl  avofftbl=meanoff.tbl  +
 plot=test1
Beginning VICAR task ccdrecip
CCDRECIP VERSION 06-Jul-2013
----TASK:F2      ----USER:lwk         Tue Sep 24 13:48:29 2013
----TASK:LTGEN   ----USER:lwk         Tue Sep 24 13:48:35 2013

ENERGY UNIT = PICOAMP-MILLISECONDS

AREA   SL   SS   NL   NS  A0 (DN/ENERGY UNIT)  TOS (MILLISECONDS)
   1   15   15   20   20          1.00000              1.00000
   2   15   66   20   20          1.00000              1.00000
   3   15  117   20   20          1.00000              1.00000
   4   15  168   20   20          1.00000              1.00000
   5   15  219   20   20          1.00000              1.00000
   6   15  270   20   20          1.00000              1.00000
   7   15  321   20   20          1.00000              1.00000
   8   15  372   20   20          1.00000              1.00000
   9   15  423   20   20          1.00000              1.00000
  10   15  474   20   20          1.00000              1.00000
  11   66   15   20   20          1.00000              1.00000
  12   66   66   20   20          1.00000              1.00000
  13   66  117   20   20          1.00000              1.00000
  14   66  168   20   20          1.00000              1.00000
  15   66  219   20   20          1.00000              1.00000
  16   66  270   20   20          1.00000              1.00000
  17   66  321   20   20          1.00000              1.00000
  18   66  372   20   20          1.00000              1.00000
  19   66  423   20   20          1.00000              1.00000
  20   66  474   20   20          1.00000              1.00000
  21  117   15   20   20          1.00000              1.00000
  22  117   66   20   20          1.00000              1.00000
  23  117  117   20   20          1.00000              1.00000
  24  117  168   20   20          1.00000              1.00000
  25  117  219   20   20          1.00000              1.00000
  26  117  270   20   20          1.00000              1.00000
  27  117  321   20   20          1.00000              1.00000
  28  117  372   20   20          1.00000              1.00000
  29  117  423   20   20          1.00000              1.00000
  30  117  474   20   20          1.00000              1.00000
  31  168   15   20   20          1.00000              1.00000
  32  168   66   20   20          1.00000              1.00000
  33  168  117   20   20          1.00000              1.00000
  34  168  168   20   20          1.00000              1.00000
  35  168  219   20   20          1.00000              1.00000
  36  168  270   20   20          1.00000              1.00000
  37  168  321   20   20          1.00000              1.00000
  38  168  372   20   20          1.00000              1.00000
  39  168  423   20   20          1.00000              1.00000
  40  168  474   20   20          1.00000              1.00000
  41  219   15   20   20          1.00000              1.00000
  42  219   66   20   20          1.00000              1.00000
  43  219  117   20   20          1.00000              1.00000
  44  219  168   20   20          1.00000              1.00000
  45  219  219   20   20          1.00000              1.00000
  46  219  270   20   20          1.00000              1.00000
  47  219  321   20   20          1.00000              1.00000
  48  219  372   20   20          1.00000              1.00000
  49  219  423   20   20          1.00000              1.00000
  50  219  474   20   20          1.00000              1.00000
  51  270   15   20   20          1.00000              1.00000
  52  270   66   20   20          1.00000              1.00000
  53  270  117   20   20          1.00000              1.00000
  54  270  168   20   20          1.00000              1.00000
  55  270  219   20   20          1.00000              1.00000
  56  270  270   20   20          1.00000              1.00000
  57  270  321   20   20          1.00000              1.00000
  58  270  372   20   20          1.00000              1.00000
  59  270  423   20   20          1.00000              1.00000
  60  270  474   20   20          1.00000              1.00000
  61  321   15   20   20          1.00000              1.00000
  62  321   66   20   20          1.00000              1.00000
  63  321  117   20   20          1.00000              1.00000
  64  321  168   20   20          1.00000              1.00000
  65  321  219   20   20          1.00000              1.00000
  66  321  270   20   20          1.00000              1.00000
  67  321  321   20   20          1.00000              1.00000
  68  321  372   20   20          1.00000              1.00000
  69  321  423   20   20          1.00000              1.00000
  70  321  474   20   20          1.00000              1.00000
  71  372   15   20   20          1.00000              1.00000
  72  372   66   20   20          1.00000              1.00000
  73  372  117   20   20          1.00000              1.00000
  74  372  168   20   20          1.00000              1.00000
  75  372  219   20   20          1.00000              1.00000
  76  372  270   20   20          1.00000              1.00000
  77  372  321   20   20          1.00000              1.00000
  78  372  372   20   20          1.00000              1.00000
  79  372  423   20   20          1.00000              1.00000
  80  372  474   20   20          1.00000              1.00000
  81  423   15   20   20          1.00000              1.00000
  82  423   66   20   20          1.00000              1.00000
  83  423  117   20   20          1.00000              1.00000
  84  423  168   20   20          1.00000              1.00000
  85  423  219   20   20          1.00000              1.00000
  86  423  270   20   20          1.00000              1.00000
  87  423  321   20   20          1.00000              1.00000
  88  423  372   20   20          1.00000              1.00000
  89  423  423   20   20          1.00000              1.00000
  90  423  474   20   20          1.00000              1.00000
  91  474   15   20   20          1.00000              1.00000
  92  474   66   20   20          1.00000              1.00000
  93  474  117   20   20          1.00000              1.00000
  94  474  168   20   20          1.00000              1.00000
  95  474  219   20   20          1.00000              1.00000
  96  474  270   20   20          1.00000              1.00000
  97  474  321   20   20          1.00000              1.00000
  98  474  372   20   20          1.00000              1.00000
  99  474  423   20   20          1.00000              1.00000
 100  474  474   20   20          1.00000              1.00000

Global value for A0...
Raw mean and sigma are...
N= 100 MEAN=     1.00000 SIGMA=     0.00000
After throwing out samples differing by 2 sigma
N= 100 MEAN=     1.00000 SIGMA=     0.00000

Global shutter offset...
Raw mean and sigma are...
N= 100 MEAN=     1.00000 SIGMA=     0.00000
After throwing out samples differing by 2 sigma
N= 100 MEAN=     1.00000 SIGMA=     0.00000



  COMMANDED    ILLUMINATION    (DN-DC)      (DN-DC)/L     RESIDUAL      (DN-DC)/(L*T)   RESIDUAL  (DN-DC)/[L*(T-TOS)]  RESIDUAL
EXPOSURE T(MS)   L(RAD)                    (DN/ENERGY    (DN/ENERGY      (DN/ENERGY    (DN/ENERGY     (DN/ENERGY      (DN/ENERGY
                                              UNIT)         UNIT)           UNIT)         UNIT)          UNIT)           UNIT)
   10.0000      100.0000       900.000        9.0000        0.0000          0.9000       -0.1000         1.0000          0.0000
   20.0000       50.0000       950.000       19.0000        0.0000          0.9500       -0.0500         1.0000          0.0000
   40.0000       25.0000       975.000       39.0000        0.0000          0.9750       -0.0250         1.0000          0.0000
                                                      RMS=  0.0000                  RMS=  0.1146                   RMS=  0.0000

TOS= 1.0000   SD= 0.0000    AO= 1.0000    SD= 0.0000
NUMBER REJECTED FOR SENSITIVITY   =    0
NUMBER OF GOOD AREAS= 100 OUT OF 100 AREAS SAMPLED

SAMP NUMBER=  25 SHUTTER OFFSET=   1.000000 MS
SAMP NUMBER=  76 SHUTTER OFFSET=   1.000000 MS
SAMP NUMBER= 127 SHUTTER OFFSET=   1.000000 MS
SAMP NUMBER= 178 SHUTTER OFFSET=   1.000000 MS
SAMP NUMBER= 229 SHUTTER OFFSET=   1.000000 MS
SAMP NUMBER= 280 SHUTTER OFFSET=   1.000000 MS
SAMP NUMBER= 331 SHUTTER OFFSET=   1.000000 MS
SAMP NUMBER= 382 SHUTTER OFFSET=   1.000000 MS
SAMP NUMBER= 433 SHUTTER OFFSET=   1.000000 MS
SAMP NUMBER= 484 SHUTTER OFFSET=   1.000000 MS
Saving shutter offsets...
CCDRECIP task completed
ush cat area.tbl
ush cat off.tbl
ush cat meanoff.tbl
if (mode = "nobatch" or mode = "inter")
    ush gnuplot test1.gpi
end-if
gausnois a.dat mean=0 sigma=3 format=half seed=13 nl=512 ns=512
Beginning VICAR task gausnois
f2 (cas/sum2.1, a.dat) m1.a func=10+in2
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 434 TIMES
gausnois a.dat mean=0 sigma=3 format=half seed=17 nl=512 ns=512
Beginning VICAR task gausnois
f2 (cas/sum2.1, a.dat) m1.b func=10+in2
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 434 TIMES
gausnois a.dat mean=0 sigma=3 format=half seed=19 nl=512 ns=512
Beginning VICAR task gausnois
f2 (cas/sum2.1, a.dat) m1.c func=10+in2
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 438 TIMES
gausnois a.dat mean=0 sigma=30 format=half seed=13 nl=512 ns=512
Beginning VICAR task gausnois
f2 (cas/sum2.1, a.dat) m2.a func=910+in2
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 3499 TIMES
label-rep m2.a 'prop property="CASSINI-ISS"  +
    item="EXPOSURE_DURATION=10. RADIANCE=100."
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword EXPOSURE_DURATION replaced
Keyword RADIANCE replaced
gausnois a.dat mean=0 sigma=30 format=half seed=17 nl=512 ns=512
Beginning VICAR task gausnois
f2 (cas/sum2.1, a.dat) m2.b func=910+in2
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 3469 TIMES
label-rep m2.b 'prop property="CASSINI-ISS"  +
    item="EXPOSURE_DURATION=10. RADIANCE=100."
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword EXPOSURE_DURATION replaced
Keyword RADIANCE replaced
gausnois a.dat mean=0 sigma=30 format=half seed=19 nl=512 ns=512
Beginning VICAR task gausnois
f2 (cas/sum2.1, a.dat) m2.c func=910+in2
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 3496 TIMES
label-rep m2.c 'prop property="CASSINI-ISS"  +
    item="EXPOSURE_DURATION=10. RADIANCE=100."
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword EXPOSURE_DURATION replaced
Keyword RADIANCE replaced
gausnois a.dat mean=0 sigma=30 format=half seed=13 nl=512 ns=512
Beginning VICAR task gausnois
f2 (cas/sum2.1, a.dat) m3.a func=960+in2
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 3499 TIMES
label-rep m3.a 'prop property="CASSINI-ISS"  +
    item="EXPOSURE_DURATION=20. RADIANCE=50."
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword EXPOSURE_DURATION replaced
Keyword RADIANCE replaced
gausnois a.dat mean=0 sigma=30 format=half seed=17 nl=512 ns=512
Beginning VICAR task gausnois
f2 (cas/sum2.1, a.dat) m3.b func=960+in2
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 3469 TIMES
label-rep m3.b 'prop property="CASSINI-ISS"  +
    item="EXPOSURE_DURATION=20. RADIANCE=50."
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword EXPOSURE_DURATION replaced
Keyword RADIANCE replaced
gausnois a.dat mean=0 sigma=30 format=half seed=19 nl=512 ns=512
Beginning VICAR task gausnois
f2 (cas/sum2.1, a.dat) m3.c func=960+in2
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 3496 TIMES
label-rep m3.c 'prop property="CASSINI-ISS"  +
    item="EXPOSURE_DURATION=20. RADIANCE=50."
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword EXPOSURE_DURATION replaced
Keyword RADIANCE replaced
gausnois a.dat mean=0 sigma=30 format=half seed=13 nl=512 ns=512
Beginning VICAR task gausnois
f2 (cas/sum2.1, a.dat) m4.a func=985+in2
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 3499 TIMES
label-rep m4.a 'prop property="CASSINI-ISS"  +
    item="EXPOSURE_DURATION=40. RADIANCE=25."
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword EXPOSURE_DURATION replaced
Keyword RADIANCE replaced
gausnois a.dat mean=0 sigma=30 format=half seed=17 nl=512 ns=512
Beginning VICAR task gausnois
f2 (cas/sum2.1, a.dat) m4.b func=985+in2
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 3469 TIMES
label-rep m4.b 'prop property="CASSINI-ISS"  +
    item="EXPOSURE_DURATION=40. RADIANCE=25."
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword EXPOSURE_DURATION replaced
Keyword RADIANCE replaced
gausnois a.dat mean=0 sigma=30 format=half seed=19 nl=512 ns=512
Beginning VICAR task gausnois
f2 (cas/sum2.1, a.dat) m4.c func=985+in2
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 3496 TIMES
label-rep m4.c 'prop property="CASSINI-ISS"  +
    item="EXPOSURE_DURATION=40. RADIANCE=25."
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword EXPOSURE_DURATION replaced
Keyword RADIANCE replaced
createfile m.list
refgbl  $syschar
write "CREATEFILE version 1-3-97"
CREATEFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  USH cp /dev/null  m.list
end-if
END-PROC
addtofile m.list "NEXT FILE=0001"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="NEXT FILE=0001"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile m.list "m1.a"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="m1.a"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile m.list "m1.b"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="m1.b"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile m.list "m1.c"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="m1.c"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile m.list "m2.a"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="m2.a"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile m.list "m2.b"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="m2.b"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile m.list "m2.c"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="m2.c"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile m.list "m3.a"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="m3.a"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile m.list "m3.b"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="m3.b"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile m.list "m3.c"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="m3.c"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile m.list "m4.a"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="m4.a"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile m.list "m4.b"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="m4.b"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile m.list "m4.c"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="m4.c"
Beginning VICAR task addtofil
end-if
END-PROC
reset m.list
Beginning VICAR task reset
ush cat m.list
ltgen m1.a out=testm.ltf list=m.list 'GRID
Beginning VICAR task ltgen
LTGEN Version 14-MAR-97
NUMBER OF FILES  =         12
NUMBER OF LEVELS =          4
MAX FRAMES/LEVEL =          3
NUMBER OF AREAS     =    100
NUMBER OF EXPOSURES =      4
EXPOSURES = 
            0.000E+00  1.000E+01  2.000E+01  4.000E+01
MAX FRAMES/LEVEL =           3
WRITING HALFWORD LIGHT TRANSFER FILE WITH
 NL (NREC) =           4
 NS        =         901
LTGEN task completed
momgen2 list=m.list ltfrcp=testm.ltf
LOCAL (F,G,LUMS,EXS)      STRING
LOCAL (NLVL,I1,I2,I3,I,J) INTEGER
LOCAL (EXP,LM,X)          REAL
LOCAL EX                  REAL      COUNT=1:100
LOCAL LMS                 STRING    COUNT=1:100
if ($syschar(1)="UNIX")
   defcmd-replace typeit "ush cat"
else
end-if
reset m.list
Beginning VICAR task reset
nxt m.list F I1 I2 I3
Beginning VICAR task nxt
 Output 1 is m1.a
getlab m1.a lab_item="EXPOSURE_DURATION" itm_name=EXP 'PROPERTY+
         itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
let EX(1)=EXP
getlab m1.a lab_item="RADIANCE" itm_name=LM 'PROPERTY+
         itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
let LUMS = "5.099999904633e+00"
let LMS(1)=LUMS
createfile list1.dat
refgbl  $syschar
write "CREATEFILE version 1-3-97"
CREATEFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  USH cp /dev/null  list1.dat
end-if
END-PROC
addtofile list1.dat "NEXT FILE=0001"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="NEXT FILE=0001"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile list1.dat "m1.a"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="m1.a"
Beginning VICAR task addtofil
end-if
END-PROC
let J=1
loop
   nxt m.list F I1 I2 I3
Beginning VICAR task nxt
 Output 2 is m1.b
   if (F="END_OF_FILE") break
   getlab m1.b lab_item="EXPOSURE_DURATION" itm_name=EXP 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   getlab m1.b lab_item="RADIANCE" itm_name=LM 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   let LUMS = "5.099999904633e+00"
   if (EXP=EX(J))
       let G="list"//"1"//".dat"
       addtofile list1.dat "m1.b"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="m1.b"
Beginning VICAR task addtofil
end-if
END-PROC
   else
   end-if
end-loop
   nxt m.list F I1 I2 I3
Beginning VICAR task nxt
 Output 3 is m1.c
   if (F="END_OF_FILE") break
   getlab m1.c lab_item="EXPOSURE_DURATION" itm_name=EXP 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   getlab m1.c lab_item="RADIANCE" itm_name=LM 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   let LUMS = "5.099999904633e+00"
   if (EXP=EX(J))
       let G="list"//"1"//".dat"
       addtofile list1.dat "m1.c"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="m1.c"
Beginning VICAR task addtofil
end-if
END-PROC
   else
   end-if
end-loop
   nxt m.list F I1 I2 I3
Beginning VICAR task nxt
 Output 4 is m2.a
   if (F="END_OF_FILE") break
   getlab m2.a lab_item="EXPOSURE_DURATION" itm_name=EXP 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   getlab m2.a lab_item="RADIANCE" itm_name=LM 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   let LUMS = "1.000000000000e+02"
   if (EXP=EX(J))
   else
       let J=J+1
       let EX(J)=EXP
       let LMS(J) = LUMS
       let G="list"//"2"//".dat"
       createfile list2.dat
refgbl  $syschar
write "CREATEFILE version 1-3-97"
CREATEFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  USH cp /dev/null  list2.dat
end-if
END-PROC
       addtofile list2.dat "NEXT FILE=0001"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="NEXT FILE=0001"
Beginning VICAR task addtofil
end-if
END-PROC
       addtofile list2.dat "m2.a"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="m2.a"
Beginning VICAR task addtofil
end-if
END-PROC
   end-if
end-loop
   nxt m.list F I1 I2 I3
Beginning VICAR task nxt
 Output 5 is m2.b
   if (F="END_OF_FILE") break
   getlab m2.b lab_item="EXPOSURE_DURATION" itm_name=EXP 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   getlab m2.b lab_item="RADIANCE" itm_name=LM 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   let LUMS = "1.000000000000e+02"
   if (EXP=EX(J))
       let G="list"//"2"//".dat"
       addtofile list2.dat "m2.b"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="m2.b"
Beginning VICAR task addtofil
end-if
END-PROC
   else
   end-if
end-loop
   nxt m.list F I1 I2 I3
Beginning VICAR task nxt
 Output 6 is m2.c
   if (F="END_OF_FILE") break
   getlab m2.c lab_item="EXPOSURE_DURATION" itm_name=EXP 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   getlab m2.c lab_item="RADIANCE" itm_name=LM 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   let LUMS = "1.000000000000e+02"
   if (EXP=EX(J))
       let G="list"//"2"//".dat"
       addtofile list2.dat "m2.c"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="m2.c"
Beginning VICAR task addtofil
end-if
END-PROC
   else
   end-if
end-loop
   nxt m.list F I1 I2 I3
Beginning VICAR task nxt
 Output 7 is m3.a
   if (F="END_OF_FILE") break
   getlab m3.a lab_item="EXPOSURE_DURATION" itm_name=EXP 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   getlab m3.a lab_item="RADIANCE" itm_name=LM 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   let LUMS = "5.000000000000e+01"
   if (EXP=EX(J))
   else
       let J=J+1
       let EX(J)=EXP
       let LMS(J) = LUMS
       let G="list"//"3"//".dat"
       createfile list3.dat
refgbl  $syschar
write "CREATEFILE version 1-3-97"
CREATEFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  USH cp /dev/null  list3.dat
end-if
END-PROC
       addtofile list3.dat "NEXT FILE=0001"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="NEXT FILE=0001"
Beginning VICAR task addtofil
end-if
END-PROC
       addtofile list3.dat "m3.a"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="m3.a"
Beginning VICAR task addtofil
end-if
END-PROC
   end-if
end-loop
   nxt m.list F I1 I2 I3
Beginning VICAR task nxt
 Output 8 is m3.b
   if (F="END_OF_FILE") break
   getlab m3.b lab_item="EXPOSURE_DURATION" itm_name=EXP 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   getlab m3.b lab_item="RADIANCE" itm_name=LM 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   let LUMS = "5.000000000000e+01"
   if (EXP=EX(J))
       let G="list"//"3"//".dat"
       addtofile list3.dat "m3.b"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="m3.b"
Beginning VICAR task addtofil
end-if
END-PROC
   else
   end-if
end-loop
   nxt m.list F I1 I2 I3
Beginning VICAR task nxt
 Output 9 is m3.c
   if (F="END_OF_FILE") break
   getlab m3.c lab_item="EXPOSURE_DURATION" itm_name=EXP 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   getlab m3.c lab_item="RADIANCE" itm_name=LM 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   let LUMS = "5.000000000000e+01"
   if (EXP=EX(J))
       let G="list"//"3"//".dat"
       addtofile list3.dat "m3.c"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="m3.c"
Beginning VICAR task addtofil
end-if
END-PROC
   else
   end-if
end-loop
   nxt m.list F I1 I2 I3
Beginning VICAR task nxt
 Output 10 is m4.a
   if (F="END_OF_FILE") break
   getlab m4.a lab_item="EXPOSURE_DURATION" itm_name=EXP 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   getlab m4.a lab_item="RADIANCE" itm_name=LM 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   let LUMS = "2.500000000000e+01"
   if (EXP=EX(J))
   else
       let J=J+1
       let EX(J)=EXP
       let LMS(J) = LUMS
       let G="list"//"4"//".dat"
       createfile list4.dat
refgbl  $syschar
write "CREATEFILE version 1-3-97"
CREATEFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  USH cp /dev/null  list4.dat
end-if
END-PROC
       addtofile list4.dat "NEXT FILE=0001"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="NEXT FILE=0001"
Beginning VICAR task addtofil
end-if
END-PROC
       addtofile list4.dat "m4.a"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="m4.a"
Beginning VICAR task addtofil
end-if
END-PROC
   end-if
end-loop
   nxt m.list F I1 I2 I3
Beginning VICAR task nxt
 Output 11 is m4.b
   if (F="END_OF_FILE") break
   getlab m4.b lab_item="EXPOSURE_DURATION" itm_name=EXP 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   getlab m4.b lab_item="RADIANCE" itm_name=LM 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   let LUMS = "2.500000000000e+01"
   if (EXP=EX(J))
       let G="list"//"4"//".dat"
       addtofile list4.dat "m4.b"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="m4.b"
Beginning VICAR task addtofil
end-if
END-PROC
   else
   end-if
end-loop
   nxt m.list F I1 I2 I3
Beginning VICAR task nxt
 Output 12 is m4.c
   if (F="END_OF_FILE") break
   getlab m4.c lab_item="EXPOSURE_DURATION" itm_name=EXP 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   getlab m4.c lab_item="RADIANCE" itm_name=LM 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   let LUMS = "2.500000000000e+01"
   if (EXP=EX(J))
       let G="list"//"4"//".dat"
       addtofile list4.dat "m4.c"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="m4.c"
Beginning VICAR task addtofil
end-if
END-PROC
   else
   end-if
end-loop
   nxt m.list F I1 I2 I3
Beginning VICAR task nxt
 NXT encountered end of file
   if (F="END_OF_FILE") break
 break
   if (EXP=EX(J))
   end-if
end-loop
let NLVL=J
let $BECHO="NO"
write " NUMBER OF EXPOSURE LEVELS = 4"
 NUMBER OF EXPOSURE LEVELS = 4
write " "
 
write " "
 
write " EXPOSURE LEVEL ... EXPOSURE TIME ... RADIANCES"
 EXPOSURE LEVEL ... EXPOSURE TIME ... RADIANCES
let I=1
loop
   let X=EX(I)
   let EXS = "0.000000000000e+00"
   let LUMS=LMS(I)
   write "       1              0.000000000000e+00             5.099999904633e+00"
       1              0.000000000000e+00             5.099999904633e+00
   if (I=NLVL) break
   let I=I+1
end-loop
   let X=EX(I)
   let EXS = "1.000000000000e+01"
   let LUMS=LMS(I)
   write "       2              1.000000000000e+01             1.000000000000e+02"
       2              1.000000000000e+01             1.000000000000e+02
   if (I=NLVL) break
   let I=I+1
end-loop
   let X=EX(I)
   let EXS = "2.000000000000e+01"
   let LUMS=LMS(I)
   write "       3              2.000000000000e+01             5.000000000000e+01"
       3              2.000000000000e+01             5.000000000000e+01
   if (I=NLVL) break
   let I=I+1
end-loop
   let X=EX(I)
   let EXS = "4.000000000000e+01"
   let LUMS=LMS(I)
   write "       4              4.000000000000e+01             2.500000000000e+01"
       4              4.000000000000e+01             2.500000000000e+01
   if (I=NLVL) break
 break
end-loop
write " "
 
let $BECHO="YES"
let I=1
loop
   let X=EX(I)
   let EXS = "0.000000000000e+00"
   let G="list"//"1"//".dat"
   let $BECHO="NO"
   write "FRAME LIST FOR LEVEL 1    EXPOSURE TIME 0.000000000000e+00"
FRAME LIST FOR LEVEL 1    EXPOSURE TIME 0.000000000000e+00
ush cat list1.dat
   write " "
 
   let $BECHO="YES"
   momgen LIST="list"1".dat" out=testm.ltf
Beginning VICAR task momgen
MOMGEN Version 19-MAR-1997

EXPOSURE TIME=    0.000E+00
INPUT FRAMES=             3
NUMBER OF AREAS=        100
   if (I=NLVL) break
   let I=I+1
end-loop
   let X=EX(I)
   let EXS = "1.000000000000e+01"
   let G="list"//"2"//".dat"
   let $BECHO="NO"
   write "FRAME LIST FOR LEVEL 2    EXPOSURE TIME 1.000000000000e+01"
FRAME LIST FOR LEVEL 2    EXPOSURE TIME 1.000000000000e+01
ush cat list2.dat
   write " "
 
   let $BECHO="YES"
   momgen LIST="list"2".dat" out=testm.ltf
Beginning VICAR task momgen
MOMGEN Version 19-MAR-1997

EXPOSURE TIME=    10.000000
INPUT FRAMES=             3
NUMBER OF AREAS=        100
   if (I=NLVL) break
   let I=I+1
end-loop
   let X=EX(I)
   let EXS = "2.000000000000e+01"
   let G="list"//"3"//".dat"
   let $BECHO="NO"
   write "FRAME LIST FOR LEVEL 3    EXPOSURE TIME 2.000000000000e+01"
FRAME LIST FOR LEVEL 3    EXPOSURE TIME 2.000000000000e+01
ush cat list3.dat
   write " "
 
   let $BECHO="YES"
   momgen LIST="list"3".dat" out=testm.ltf
Beginning VICAR task momgen
MOMGEN Version 19-MAR-1997

EXPOSURE TIME=    20.000000
INPUT FRAMES=             3
NUMBER OF AREAS=        100
   if (I=NLVL) break
   let I=I+1
end-loop
   let X=EX(I)
   let EXS = "4.000000000000e+01"
   let G="list"//"4"//".dat"
   let $BECHO="NO"
   write "FRAME LIST FOR LEVEL 4    EXPOSURE TIME 4.000000000000e+01"
FRAME LIST FOR LEVEL 4    EXPOSURE TIME 4.000000000000e+01
ush cat list4.dat
   write " "
 
   let $BECHO="YES"
   momgen LIST="list"4".dat" out=testm.ltf
Beginning VICAR task momgen
MOMGEN Version 19-MAR-1997

EXPOSURE TIME=    40.000000
INPUT FRAMES=             3
NUMBER OF AREAS=        100
   if (I=NLVL) break
 break
end-loop
if ($count(LTFILE) = 1)
      if (EX(I)=0.0) let LUMS="0.0"
      if (I=NLVL) break
end-if
end-proc
ccdrecip testm.ltf 'SAMP  light=(0,100,50,25) 'RADIANCE  +
 arraysiz=512  offset=so.file  areatbl=aream.tbl  +
 offtbl=offm.tbl  avofftbl=meanoffm.tbl  +
  plot=test2
Beginning VICAR task ccdrecip
CCDRECIP VERSION 06-Jul-2013
----TASK:F2      ----USER:lwk         Tue Sep 24 13:49:19 2013
----TASK:LTGEN   ----USER:lwk         Tue Sep 24 13:49:31 2013

ENERGY UNIT = PICOAMP-MILLISECONDS

AREA   SL   SS   NL   NS  A0 (DN/ENERGY UNIT)  TOS (MILLISECONDS)
   1   15   15   20   20          1.00048              0.99952
   2   15   66   20   20          0.99967              1.00033
   3   15  117   20   20          0.99992              1.00008
   4   15  168   20   20          0.99891              1.00109
   5   15  219   20   20          0.99928              1.00072
   6   15  270   20   20          1.00026              0.99974
   7   15  321   20   20          0.99938              1.00062
   8   15  372   20   20          0.99938              1.00062
   9   15  423   20   20          1.00091              0.99909
  10   15  474   20   20          1.00011              0.99989
  11   66   15   20   20          1.00106              0.99895
  12   66   66   20   20          0.99914              1.00086
  13   66  117   20   20          0.99961              1.00039
  14   66  168   20   20          1.00017              0.99983
  15   66  219   20   20          1.00044              0.99956
  16   66  270   20   20          0.99946              1.00054
  17   66  321   20   20          0.99923              1.00077
  18   66  372   20   20          0.99974              1.00026
  19   66  423   20   20          1.00019              0.99981
  20   66  474   20   20          1.00020              0.99981
  21  117   15   20   20          1.00056              0.99944
  22  117   66   20   20          1.00039              0.99961
  23  117  117   20   20          0.99987              1.00013
  24  117  168   20   20          0.99916              1.00084
  25  117  219   20   20          0.99972              1.00028
  26  117  270   20   20          1.00045              0.99955
  27  117  321   20   20          1.00089              0.99912
  28  117  372   20   20          1.00150              0.99850
  29  117  423   20   20          0.99989              1.00011
  30  117  474   20   20          0.99882              1.00118
  31  168   15   20   20          1.00079              0.99921
  32  168   66   20   20          1.00092              0.99908
  33  168  117   20   20          1.00046              0.99954
  34  168  168   20   20          1.00068              0.99932
  35  168  219   20   20          1.00011              0.99989
  36  168  270   20   20          0.99903              1.00097
  37  168  321   20   20          0.99876              1.00124
  38  168  372   20   20          1.00015              0.99985
  39  168  423   20   20          1.00013              0.99987
  40  168  474   20   20          0.99969              1.00031
  41  219   15   20   20          0.99906              1.00094
  42  219   66   20   20          0.99968              1.00032
  43  219  117   20   20          0.99984              1.00016
  44  219  168   20   20          0.99988              1.00012
  45  219  219   20   20          0.99988              1.00012
  46  219  270   20   20          0.99965              1.00035
  47  219  321   20   20          1.00077              0.99923
  48  219  372   20   20          0.99975              1.00025
  49  219  423   20   20          1.00091              0.99909
  50  219  474   20   20          0.99959              1.00041
  51  270   15   20   20          0.99941              1.00060
  52  270   66   20   20          0.99913              1.00087
  53  270  117   20   20          0.99948              1.00052
  54  270  168   20   20          1.00008              0.99992
  55  270  219   20   20          1.00081              0.99919
  56  270  270   20   20          0.99885              1.00115
  57  270  321   20   20          1.00064              0.99936
  58  270  372   20   20          0.99997              1.00004
  59  270  423   20   20          1.00071              0.99929
  60  270  474   20   20          1.00009              0.99991
  61  321   15   20   20          1.00113              0.99887
  62  321   66   20   20          1.00106              0.99894
  63  321  117   20   20          1.00025              0.99975
  64  321  168   20   20          1.00045              0.99955
  65  321  219   20   20          1.00074              0.99926
  66  321  270   20   20          0.99995              1.00005
  67  321  321   20   20          1.00075              0.99925
  68  321  372   20   20          0.99924              1.00076
  69  321  423   20   20          0.99969              1.00031
  70  321  474   20   20          0.99943              1.00057
  71  372   15   20   20          0.99990              1.00010
  72  372   66   20   20          0.99972              1.00028
  73  372  117   20   20          1.00021              0.99979
  74  372  168   20   20          1.00002              0.99998
  75  372  219   20   20          1.00101              0.99899
  76  372  270   20   20          1.00009              0.99991
  77  372  321   20   20          0.99936              1.00065
  78  372  372   20   20          0.99960              1.00040
  79  372  423   20   20          0.99969              1.00031
  80  372  474   20   20          0.99826              1.00174
  81  423   15   20   20          0.99973              1.00027
  82  423   66   20   20          1.00058              0.99942
  83  423  117   20   20          0.99979              1.00022
  84  423  168   20   20          1.00034              0.99966
  85  423  219   20   20          1.00096              0.99905
  86  423  270   20   20          1.00063              0.99937
  87  423  321   20   20          1.00126              0.99874
  88  423  372   20   20          1.00108              0.99892
  89  423  423   20   20          1.00040              0.99960
  90  423  474   20   20          1.00000              1.00000
  91  474   15   20   20          0.99968              1.00032
  92  474   66   20   20          1.00015              0.99985
  93  474  117   20   20          1.00030              0.99970
  94  474  168   20   20          0.99878              1.00122
  95  474  219   20   20          1.00021              0.99979
  96  474  270   20   20          1.00004              0.99996
  97  474  321   20   20          0.99833              1.00167
  98  474  372   20   20          0.99909              1.00091
  99  474  423   20   20          1.00042              0.99958
 100  474  474   20   20          1.00055              0.99945

Global value for A0...
Raw mean and sigma are...
N= 100 MEAN=     1.00001 SIGMA=     0.00058
After throwing out samples differing by 2 sigma
N=  93 MEAN=     1.00005 SIGMA=     0.00061

Global shutter offset...
Raw mean and sigma are...
N= 100 MEAN=     0.99999 SIGMA=     0.00063
After throwing out samples differing by 2 sigma
N=  97 MEAN=     0.99998 SIGMA=     0.00066

AREA  28 (SL,SS,NL,NS)=( 117, 372,  20,  20)  ***BOTH BAD FIT*********
AREA  30 (SL,SS,NL,NS)=( 117, 474,  20,  20)  ***BAD SENSITIVITY******
AREA  37 (SL,SS,NL,NS)=( 168, 321,  20,  20)  ***BAD SENSITIVITY******
AREA  80 (SL,SS,NL,NS)=( 372, 474,  20,  20)  ***BOTH BAD FIT*********
AREA  87 (SL,SS,NL,NS)=( 423, 321,  20,  20)  ***BAD SENSITIVITY******
AREA  94 (SL,SS,NL,NS)=( 474, 168,  20,  20)  ***BAD SENSITIVITY******
AREA  97 (SL,SS,NL,NS)=( 474, 321,  20,  20)  ***BOTH BAD FIT*********


  COMMANDED    ILLUMINATION    (DN-DC)      (DN-DC)/L     RESIDUAL      (DN-DC)/(L*T)   RESIDUAL  (DN-DC)/[L*(T-TOS)]  RESIDUAL
EXPOSURE T(MS)   L(RAD)                    (DN/ENERGY    (DN/ENERGY      (DN/ENERGY    (DN/ENERGY     (DN/ENERGY      (DN/ENERGY
                                              UNIT)         UNIT)           UNIT)         UNIT)          UNIT)           UNIT)
   10.0000      100.0000       900.052        9.0005        0.0000          0.9001       -0.1000         1.0001          0.0000
   20.0000       50.0000       950.052       19.0010        0.0000          0.9501       -0.0500         1.0001          0.0000
   40.0000       25.0000       975.052       39.0021        0.0000          0.9751       -0.0250         1.0001          0.0000
                                                      RMS=  0.0000                  RMS=  0.1146                   RMS=  0.0000

TOS= 0.9999   SD= 0.0000    AO= 1.0001    SD= 0.0000
NUMBER REJECTED FOR SENSITIVITY   =    7
NUMBER OF GOOD AREAS=  93 OUT OF 100 AREAS SAMPLED

SAMP NUMBER=  25 SHUTTER OFFSET=   0.999821 MS
SAMP NUMBER=  76 SHUTTER OFFSET=   0.999957 MS
SAMP NUMBER= 127 SHUTTER OFFSET=   1.000027 MS
SAMP NUMBER= 178 SHUTTER OFFSET=   1.000035 MS
SAMP NUMBER= 229 SHUTTER OFFSET=   0.999686 MS
SAMP NUMBER= 280 SHUTTER OFFSET=   1.000158 MS
SAMP NUMBER= 331 SHUTTER OFFSET=   0.999857 MS
SAMP NUMBER= 382 SHUTTER OFFSET=   1.000222 MS
SAMP NUMBER= 433 SHUTTER OFFSET=   0.999706 MS
SAMP NUMBER= 484 SHUTTER OFFSET=   1.000042 MS
Saving shutter offsets...
CCDRECIP task completed
ush cat aream.tbl
ush cat offm.tbl
ush cat meanoffm.tbl
if (mode = "nobatch" or mode = "inter")
    ush gnuplot test2.gpi
end-if
gen x.dat nl=512 ns=256 ival=0 linc=0 sinc=1
Beginning VICAR task gen
GEN Version 6
GEN task completed
copy x.dat a.dat (1,1,512,101)
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
size a.dat xa.dat (1,1,512,512)
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,  512,  101)
     OUTPUT SIZE=    512 X    512
 PICTURE SIZE SCALED BY      1.00000*NL,      5.06931*NS
 SIZE task completed
f2 (l2.a, xa.dat) l6.a func=in1-in2
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 102 TIMES
maxmin l6.a (1,1,1,512)
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value:          810   at  (     1,   508)
Max. value:          910   at  (     1,     1)

copy l6.a l6.b
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
copy l6.a l6.c
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
copy x.dat a.dat (1,1,512,51)
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
size a.dat xa.dat (1,1,512,512)
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,  512,   51)
     OUTPUT SIZE=    512 X    512
 PICTURE SIZE SCALED BY      1.00000*NL,     10.03922*NS
 SIZE task completed
f2 (l3.a, xa.dat) l7.a func=in1-in2
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 52 TIMES
maxmin l7.a (1,1,1,512)
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value:          910   at  (     1,   503)
Max. value:          960   at  (     1,     1)

copy l7.a l7.b
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
copy l7.a l7.c
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
copy x.dat a.dat (1,1,512,26)
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
size a.dat xa.dat (1,1,512,512)
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,  512,   26)
     OUTPUT SIZE=    512 X    512
 PICTURE SIZE SCALED BY      1.00000*NL,     19.69231*NS
 SIZE task completed
f2 (l4.a, xa.dat) l8.a func=in1-in2
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 27 TIMES
maxmin l8.a (1,1,1,512)
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value:          960   at  (     1,   493)
Max. value:          985   at  (     1,     1)

copy l8.a l8.b
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
copy l8.a l8.c
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
createfile n.list
refgbl  $syschar
write "CREATEFILE version 1-3-97"
CREATEFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  USH cp /dev/null  n.list
end-if
END-PROC
addtofile n.list "NEXT FILE=0001"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="NEXT FILE=0001"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile n.list "l1.a"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l1.a"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile n.list "l1.b"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l1.b"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile n.list "l1.c"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l1.c"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile n.list "l6.a"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l6.a"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile n.list "l6.b"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l6.b"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile n.list "l6.c"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l6.c"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile n.list "l7.a"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l7.a"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile n.list "l7.b"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l7.b"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile n.list "l7.c"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l7.c"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile n.list "l8.a"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l8.a"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile n.list "l8.b"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l8.b"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile n.list "l8.c"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l8.c"
Beginning VICAR task addtofil
end-if
END-PROC
reset n.list
Beginning VICAR task reset
ush cat n.list
ltgen l1.a out=test.ltf list=n.list 'GRID
Beginning VICAR task ltgen
LTGEN Version 14-MAR-97
NUMBER OF FILES  =         12
NUMBER OF LEVELS =          4
MAX FRAMES/LEVEL =          3
NUMBER OF AREAS     =    100
NUMBER OF EXPOSURES =      4
EXPOSURES = 
            0.000E+00  1.000E+01  2.000E+01  4.000E+01
MAX FRAMES/LEVEL =           3
WRITING HALFWORD LIGHT TRANSFER FILE WITH
 NL (NREC) =           4
 NS        =         901
LTGEN task completed
momgen2 list=n.list ltfrcp=test.ltf
LOCAL (F,G,LUMS,EXS)      STRING
LOCAL (NLVL,I1,I2,I3,I,J) INTEGER
LOCAL (EXP,LM,X)          REAL
LOCAL EX                  REAL      COUNT=1:100
LOCAL LMS                 STRING    COUNT=1:100
if ($syschar(1)="UNIX")
   defcmd-replace typeit "ush cat"
else
end-if
reset n.list
Beginning VICAR task reset
nxt n.list F I1 I2 I3
Beginning VICAR task nxt
 Output 1 is l1.a
getlab l1.a lab_item="EXPOSURE_DURATION" itm_name=EXP 'PROPERTY+
         itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
let EX(1)=EXP
getlab l1.a lab_item="RADIANCE" itm_name=LM 'PROPERTY+
         itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
let LUMS = "5.099999904633e+00"
let LMS(1)=LUMS
createfile list1.dat
refgbl  $syschar
write "CREATEFILE version 1-3-97"
CREATEFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  USH cp /dev/null  list1.dat
end-if
END-PROC
addtofile list1.dat "NEXT FILE=0001"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="NEXT FILE=0001"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile list1.dat "l1.a"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l1.a"
Beginning VICAR task addtofil
end-if
END-PROC
let J=1
loop
   nxt n.list F I1 I2 I3
Beginning VICAR task nxt
 Output 2 is l1.b
   if (F="END_OF_FILE") break
   getlab l1.b lab_item="EXPOSURE_DURATION" itm_name=EXP 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   getlab l1.b lab_item="RADIANCE" itm_name=LM 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   let LUMS = "5.099999904633e+00"
   if (EXP=EX(J))
       let G="list"//"1"//".dat"
       addtofile list1.dat "l1.b"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l1.b"
Beginning VICAR task addtofil
end-if
END-PROC
   else
   end-if
end-loop
   nxt n.list F I1 I2 I3
Beginning VICAR task nxt
 Output 3 is l1.c
   if (F="END_OF_FILE") break
   getlab l1.c lab_item="EXPOSURE_DURATION" itm_name=EXP 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   getlab l1.c lab_item="RADIANCE" itm_name=LM 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   let LUMS = "5.099999904633e+00"
   if (EXP=EX(J))
       let G="list"//"1"//".dat"
       addtofile list1.dat "l1.c"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l1.c"
Beginning VICAR task addtofil
end-if
END-PROC
   else
   end-if
end-loop
   nxt n.list F I1 I2 I3
Beginning VICAR task nxt
 Output 4 is l6.a
   if (F="END_OF_FILE") break
   getlab l6.a lab_item="EXPOSURE_DURATION" itm_name=EXP 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   getlab l6.a lab_item="RADIANCE" itm_name=LM 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   let LUMS = "1.000000000000e+02"
   if (EXP=EX(J))
   else
       let J=J+1
       let EX(J)=EXP
       let LMS(J) = LUMS
       let G="list"//"2"//".dat"
       createfile list2.dat
refgbl  $syschar
write "CREATEFILE version 1-3-97"
CREATEFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  USH cp /dev/null  list2.dat
end-if
END-PROC
       addtofile list2.dat "NEXT FILE=0001"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="NEXT FILE=0001"
Beginning VICAR task addtofil
end-if
END-PROC
       addtofile list2.dat "l6.a"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l6.a"
Beginning VICAR task addtofil
end-if
END-PROC
   end-if
end-loop
   nxt n.list F I1 I2 I3
Beginning VICAR task nxt
 Output 5 is l6.b
   if (F="END_OF_FILE") break
   getlab l6.b lab_item="EXPOSURE_DURATION" itm_name=EXP 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   getlab l6.b lab_item="RADIANCE" itm_name=LM 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   let LUMS = "1.000000000000e+02"
   if (EXP=EX(J))
       let G="list"//"2"//".dat"
       addtofile list2.dat "l6.b"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l6.b"
Beginning VICAR task addtofil
end-if
END-PROC
   else
   end-if
end-loop
   nxt n.list F I1 I2 I3
Beginning VICAR task nxt
 Output 6 is l6.c
   if (F="END_OF_FILE") break
   getlab l6.c lab_item="EXPOSURE_DURATION" itm_name=EXP 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   getlab l6.c lab_item="RADIANCE" itm_name=LM 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   let LUMS = "1.000000000000e+02"
   if (EXP=EX(J))
       let G="list"//"2"//".dat"
       addtofile list2.dat "l6.c"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l6.c"
Beginning VICAR task addtofil
end-if
END-PROC
   else
   end-if
end-loop
   nxt n.list F I1 I2 I3
Beginning VICAR task nxt
 Output 7 is l7.a
   if (F="END_OF_FILE") break
   getlab l7.a lab_item="EXPOSURE_DURATION" itm_name=EXP 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   getlab l7.a lab_item="RADIANCE" itm_name=LM 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   let LUMS = "5.000000000000e+01"
   if (EXP=EX(J))
   else
       let J=J+1
       let EX(J)=EXP
       let LMS(J) = LUMS
       let G="list"//"3"//".dat"
       createfile list3.dat
refgbl  $syschar
write "CREATEFILE version 1-3-97"
CREATEFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  USH cp /dev/null  list3.dat
end-if
END-PROC
       addtofile list3.dat "NEXT FILE=0001"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="NEXT FILE=0001"
Beginning VICAR task addtofil
end-if
END-PROC
       addtofile list3.dat "l7.a"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l7.a"
Beginning VICAR task addtofil
end-if
END-PROC
   end-if
end-loop
   nxt n.list F I1 I2 I3
Beginning VICAR task nxt
 Output 8 is l7.b
   if (F="END_OF_FILE") break
   getlab l7.b lab_item="EXPOSURE_DURATION" itm_name=EXP 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   getlab l7.b lab_item="RADIANCE" itm_name=LM 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   let LUMS = "5.000000000000e+01"
   if (EXP=EX(J))
       let G="list"//"3"//".dat"
       addtofile list3.dat "l7.b"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l7.b"
Beginning VICAR task addtofil
end-if
END-PROC
   else
   end-if
end-loop
   nxt n.list F I1 I2 I3
Beginning VICAR task nxt
 Output 9 is l7.c
   if (F="END_OF_FILE") break
   getlab l7.c lab_item="EXPOSURE_DURATION" itm_name=EXP 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   getlab l7.c lab_item="RADIANCE" itm_name=LM 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   let LUMS = "5.000000000000e+01"
   if (EXP=EX(J))
       let G="list"//"3"//".dat"
       addtofile list3.dat "l7.c"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l7.c"
Beginning VICAR task addtofil
end-if
END-PROC
   else
   end-if
end-loop
   nxt n.list F I1 I2 I3
Beginning VICAR task nxt
 Output 10 is l8.a
   if (F="END_OF_FILE") break
   getlab l8.a lab_item="EXPOSURE_DURATION" itm_name=EXP 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   getlab l8.a lab_item="RADIANCE" itm_name=LM 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   let LUMS = "2.500000000000e+01"
   if (EXP=EX(J))
   else
       let J=J+1
       let EX(J)=EXP
       let LMS(J) = LUMS
       let G="list"//"4"//".dat"
       createfile list4.dat
refgbl  $syschar
write "CREATEFILE version 1-3-97"
CREATEFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  USH cp /dev/null  list4.dat
end-if
END-PROC
       addtofile list4.dat "NEXT FILE=0001"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="NEXT FILE=0001"
Beginning VICAR task addtofil
end-if
END-PROC
       addtofile list4.dat "l8.a"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l8.a"
Beginning VICAR task addtofil
end-if
END-PROC
   end-if
end-loop
   nxt n.list F I1 I2 I3
Beginning VICAR task nxt
 Output 11 is l8.b
   if (F="END_OF_FILE") break
   getlab l8.b lab_item="EXPOSURE_DURATION" itm_name=EXP 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   getlab l8.b lab_item="RADIANCE" itm_name=LM 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   let LUMS = "2.500000000000e+01"
   if (EXP=EX(J))
       let G="list"//"4"//".dat"
       addtofile list4.dat "l8.b"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l8.b"
Beginning VICAR task addtofil
end-if
END-PROC
   else
   end-if
end-loop
   nxt n.list F I1 I2 I3
Beginning VICAR task nxt
 Output 12 is l8.c
   if (F="END_OF_FILE") break
   getlab l8.c lab_item="EXPOSURE_DURATION" itm_name=EXP 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   getlab l8.c lab_item="RADIANCE" itm_name=LM 'PROPERTY+
            itm_type=REAL itm_task="CASSINI-ISS"
Beginning VICAR task getlab
   let LUMS = "2.500000000000e+01"
   if (EXP=EX(J))
       let G="list"//"4"//".dat"
       addtofile list4.dat "l8.c"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l8.c"
Beginning VICAR task addtofil
end-if
END-PROC
   else
   end-if
end-loop
   nxt n.list F I1 I2 I3
Beginning VICAR task nxt
 NXT encountered end of file
   if (F="END_OF_FILE") break
 break
   if (EXP=EX(J))
   end-if
end-loop
let NLVL=J
let $BECHO="NO"
write " NUMBER OF EXPOSURE LEVELS = 4"
 NUMBER OF EXPOSURE LEVELS = 4
write " "
 
write " "
 
write " EXPOSURE LEVEL ... EXPOSURE TIME ... RADIANCES"
 EXPOSURE LEVEL ... EXPOSURE TIME ... RADIANCES
let I=1
loop
   let X=EX(I)
   let EXS = "0.000000000000e+00"
   let LUMS=LMS(I)
   write "       1              0.000000000000e+00             5.099999904633e+00"
       1              0.000000000000e+00             5.099999904633e+00
   if (I=NLVL) break
   let I=I+1
end-loop
   let X=EX(I)
   let EXS = "1.000000000000e+01"
   let LUMS=LMS(I)
   write "       2              1.000000000000e+01             1.000000000000e+02"
       2              1.000000000000e+01             1.000000000000e+02
   if (I=NLVL) break
   let I=I+1
end-loop
   let X=EX(I)
   let EXS = "2.000000000000e+01"
   let LUMS=LMS(I)
   write "       3              2.000000000000e+01             5.000000000000e+01"
       3              2.000000000000e+01             5.000000000000e+01
   if (I=NLVL) break
   let I=I+1
end-loop
   let X=EX(I)
   let EXS = "4.000000000000e+01"
   let LUMS=LMS(I)
   write "       4              4.000000000000e+01             2.500000000000e+01"
       4              4.000000000000e+01             2.500000000000e+01
   if (I=NLVL) break
 break
end-loop
write " "
 
let $BECHO="YES"
let I=1
loop
   let X=EX(I)
   let EXS = "0.000000000000e+00"
   let G="list"//"1"//".dat"
   let $BECHO="NO"
   write "FRAME LIST FOR LEVEL 1    EXPOSURE TIME 0.000000000000e+00"
FRAME LIST FOR LEVEL 1    EXPOSURE TIME 0.000000000000e+00
ush cat list1.dat
   write " "
 
   let $BECHO="YES"
   momgen LIST="list"1".dat" out=test.ltf
Beginning VICAR task momgen
MOMGEN Version 19-MAR-1997

EXPOSURE TIME=    0.000E+00
INPUT FRAMES=             3
NUMBER OF AREAS=        100
   if (I=NLVL) break
   let I=I+1
end-loop
   let X=EX(I)
   let EXS = "1.000000000000e+01"
   let G="list"//"2"//".dat"
   let $BECHO="NO"
   write "FRAME LIST FOR LEVEL 2    EXPOSURE TIME 1.000000000000e+01"
FRAME LIST FOR LEVEL 2    EXPOSURE TIME 1.000000000000e+01
ush cat list2.dat
   write " "
 
   let $BECHO="YES"
   momgen LIST="list"2".dat" out=test.ltf
Beginning VICAR task momgen
MOMGEN Version 19-MAR-1997

EXPOSURE TIME=    10.000000
INPUT FRAMES=             3
NUMBER OF AREAS=        100
   if (I=NLVL) break
   let I=I+1
end-loop
   let X=EX(I)
   let EXS = "2.000000000000e+01"
   let G="list"//"3"//".dat"
   let $BECHO="NO"
   write "FRAME LIST FOR LEVEL 3    EXPOSURE TIME 2.000000000000e+01"
FRAME LIST FOR LEVEL 3    EXPOSURE TIME 2.000000000000e+01
ush cat list3.dat
   write " "
 
   let $BECHO="YES"
   momgen LIST="list"3".dat" out=test.ltf
Beginning VICAR task momgen
MOMGEN Version 19-MAR-1997

EXPOSURE TIME=    20.000000
INPUT FRAMES=             3
NUMBER OF AREAS=        100
   if (I=NLVL) break
   let I=I+1
end-loop
   let X=EX(I)
   let EXS = "4.000000000000e+01"
   let G="list"//"4"//".dat"
   let $BECHO="NO"
   write "FRAME LIST FOR LEVEL 4    EXPOSURE TIME 4.000000000000e+01"
FRAME LIST FOR LEVEL 4    EXPOSURE TIME 4.000000000000e+01
ush cat list4.dat
   write " "
 
   let $BECHO="YES"
   momgen LIST="list"4".dat" out=test.ltf
Beginning VICAR task momgen
MOMGEN Version 19-MAR-1997

EXPOSURE TIME=    40.000000
INPUT FRAMES=             3
NUMBER OF AREAS=        100
   if (I=NLVL) break
 break
end-loop
if ($count(LTFILE) = 1)
      if (EX(I)=0.0) let LUMS="0.0"
      if (I=NLVL) break
end-if
end-proc
ccdrecip test.ltf 'SAMP  light=(0,100,50,25) 'RADIANCE  +
 arraysiz=512  offset=so.file  areatbl=area.tbl  +
 offtbl=off.tbl  avofftbl=meanoff.tbl reject=0  +
 plot=test3
Beginning VICAR task ccdrecip
CCDRECIP VERSION 06-Jul-2013
----TASK:F2      ----USER:lwk         Tue Sep 24 13:48:29 2013
----TASK:LTGEN   ----USER:lwk         Tue Sep 24 13:50:16 2013

ENERGY UNIT = PICOAMP-MILLISECONDS

AREA   SL   SS   NL   NS  A0 (DN/ENERGY UNIT)  TOS (MILLISECONDS)
   1   15   15   20   20          1.00050              1.04748
   2   15   66   20   20          1.00035              1.14617
   3   15  117   20   20          1.00023              1.24501
   4   15  168   20   20          1.00020              1.34659
   5   15  219   20   20          1.00005              1.44536
   6   15  270   20   20          0.99995              1.54465
   7   15  321   20   20          0.99982              1.64414
   8   15  372   20   20          0.99975              1.74501
   9   15  423   20   20          0.99965              1.84407
  10   15  474   20   20          0.99950              1.94297
  11   66   15   20   20          1.00050              1.04748
  12   66   66   20   20          1.00035              1.14617
  13   66  117   20   20          1.00023              1.24501
  14   66  168   20   20          1.00020              1.34659
  15   66  219   20   20          1.00005              1.44536
  16   66  270   20   20          0.99995              1.54465
  17   66  321   20   20          0.99982              1.64414
  18   66  372   20   20          0.99975              1.74501
  19   66  423   20   20          0.99965              1.84407
  20   66  474   20   20          0.99950              1.94297
  21  117   15   20   20          1.00050              1.04748
  22  117   66   20   20          1.00035              1.14617
  23  117  117   20   20          1.00023              1.24501
  24  117  168   20   20          1.00020              1.34659
  25  117  219   20   20          1.00005              1.44536
  26  117  270   20   20          0.99995              1.54465
  27  117  321   20   20          0.99982              1.64414
  28  117  372   20   20          0.99975              1.74501
  29  117  423   20   20          0.99965              1.84407
  30  117  474   20   20          0.99950              1.94297
  31  168   15   20   20          1.00050              1.04748
  32  168   66   20   20          1.00035              1.14617
  33  168  117   20   20          1.00023              1.24501
  34  168  168   20   20          1.00020              1.34659
  35  168  219   20   20          1.00005              1.44536
  36  168  270   20   20          0.99995              1.54465
  37  168  321   20   20          0.99982              1.64414
  38  168  372   20   20          0.99975              1.74501
  39  168  423   20   20          0.99965              1.84407
  40  168  474   20   20          0.99950              1.94297
  41  219   15   20   20          1.00050              1.04748
  42  219   66   20   20          1.00035              1.14617
  43  219  117   20   20          1.00023              1.24501
  44  219  168   20   20          1.00020              1.34659
  45  219  219   20   20          1.00005              1.44536
  46  219  270   20   20          0.99995              1.54465
  47  219  321   20   20          0.99982              1.64414
  48  219  372   20   20          0.99975              1.74501
  49  219  423   20   20          0.99965              1.84407
  50  219  474   20   20          0.99950              1.94297
  51  270   15   20   20          1.00050              1.04748
  52  270   66   20   20          1.00035              1.14617
  53  270  117   20   20          1.00023              1.24501
  54  270  168   20   20          1.00020              1.34659
  55  270  219   20   20          1.00005              1.44536
  56  270  270   20   20          0.99995              1.54465
  57  270  321   20   20          0.99982              1.64414
  58  270  372   20   20          0.99975              1.74501
  59  270  423   20   20          0.99965              1.84407
  60  270  474   20   20          0.99950              1.94297
  61  321   15   20   20          1.00050              1.04748
  62  321   66   20   20          1.00035              1.14617
  63  321  117   20   20          1.00023              1.24501
  64  321  168   20   20          1.00020              1.34659
  65  321  219   20   20          1.00005              1.44536
  66  321  270   20   20          0.99995              1.54465
  67  321  321   20   20          0.99982              1.64414
  68  321  372   20   20          0.99975              1.74501
  69  321  423   20   20          0.99965              1.84407
  70  321  474   20   20          0.99950              1.94297
  71  372   15   20   20          1.00050              1.04748
  72  372   66   20   20          1.00035              1.14617
  73  372  117   20   20          1.00023              1.24501
  74  372  168   20   20          1.00020              1.34659
  75  372  219   20   20          1.00005              1.44536
  76  372  270   20   20          0.99995              1.54465
  77  372  321   20   20          0.99982              1.64414
  78  372  372   20   20          0.99975              1.74501
  79  372  423   20   20          0.99965              1.84407
  80  372  474   20   20          0.99950              1.94297
  81  423   15   20   20          1.00050              1.04748
  82  423   66   20   20          1.00035              1.14617
  83  423  117   20   20          1.00023              1.24501
  84  423  168   20   20          1.00020              1.34659
  85  423  219   20   20          1.00005              1.44536
  86  423  270   20   20          0.99995              1.54465
  87  423  321   20   20          0.99982              1.64414
  88  423  372   20   20          0.99975              1.74501
  89  423  423   20   20          0.99965              1.84407
  90  423  474   20   20          0.99950              1.94297
  91  474   15   20   20          1.00050              1.04748
  92  474   66   20   20          1.00035              1.14617
  93  474  117   20   20          1.00023              1.24501
  94  474  168   20   20          1.00020              1.34659
  95  474  219   20   20          1.00005              1.44536
  96  474  270   20   20          0.99995              1.54465
  97  474  321   20   20          0.99982              1.64414
  98  474  372   20   20          0.99975              1.74501
  99  474  423   20   20          0.99965              1.84407
 100  474  474   20   20          0.99950              1.94297

Global value for A0...
Raw mean and sigma are...
N= 100 MEAN=     1.00000 SIGMA=     0.00019
After throwing out samples differing by 2 sigma
N=  80 MEAN=     1.00000 SIGMA=     0.00012

Global shutter offset...
Raw mean and sigma are...
N= 100 MEAN=     1.49514 SIGMA=     0.28613
After throwing out samples differing by 2 sigma
N= 100 MEAN=     1.49514 SIGMA=     0.28613

AREA   1 (SL,SS,NL,NS)=(  15,  15,  20,  20)  ***BAD SENSITIVITY******
AREA  10 (SL,SS,NL,NS)=(  15, 474,  20,  20)  ***BAD SENSITIVITY******
AREA  11 (SL,SS,NL,NS)=(  66,  15,  20,  20)  ***BAD SENSITIVITY******
AREA  20 (SL,SS,NL,NS)=(  66, 474,  20,  20)  ***BAD SENSITIVITY******
AREA  21 (SL,SS,NL,NS)=( 117,  15,  20,  20)  ***BAD SENSITIVITY******
AREA  30 (SL,SS,NL,NS)=( 117, 474,  20,  20)  ***BAD SENSITIVITY******
AREA  31 (SL,SS,NL,NS)=( 168,  15,  20,  20)  ***BAD SENSITIVITY******
AREA  40 (SL,SS,NL,NS)=( 168, 474,  20,  20)  ***BAD SENSITIVITY******
AREA  41 (SL,SS,NL,NS)=( 219,  15,  20,  20)  ***BAD SENSITIVITY******
AREA  50 (SL,SS,NL,NS)=( 219, 474,  20,  20)  ***BAD SENSITIVITY******
AREA  51 (SL,SS,NL,NS)=( 270,  15,  20,  20)  ***BAD SENSITIVITY******
AREA  60 (SL,SS,NL,NS)=( 270, 474,  20,  20)  ***BAD SENSITIVITY******
AREA  61 (SL,SS,NL,NS)=( 321,  15,  20,  20)  ***BAD SENSITIVITY******
AREA  70 (SL,SS,NL,NS)=( 321, 474,  20,  20)  ***BAD SENSITIVITY******
AREA  71 (SL,SS,NL,NS)=( 372,  15,  20,  20)  ***BAD SENSITIVITY******
AREA  80 (SL,SS,NL,NS)=( 372, 474,  20,  20)  ***BAD SENSITIVITY******
AREA  81 (SL,SS,NL,NS)=( 423,  15,  20,  20)  ***BAD SENSITIVITY******
AREA  90 (SL,SS,NL,NS)=( 423, 474,  20,  20)  ***BAD SENSITIVITY******
AREA  91 (SL,SS,NL,NS)=( 474,  15,  20,  20)  ***BAD SENSITIVITY******
AREA 100 (SL,SS,NL,NS)=( 474, 474,  20,  20)  ***BAD SENSITIVITY******


  COMMANDED    ILLUMINATION    (DN-DC)      (DN-DC)/L     RESIDUAL      (DN-DC)/(L*T)   RESIDUAL  (DN-DC)/[L*(T-TOS)]  RESIDUAL
EXPOSURE T(MS)   L(RAD)                    (DN/ENERGY    (DN/ENERGY      (DN/ENERGY    (DN/ENERGY     (DN/ENERGY      (DN/ENERGY
                                              UNIT)         UNIT)           UNIT)         UNIT)          UNIT)           UNIT)
   10.0000      100.0000       850.495        8.5050        0.0000          0.8505       -0.1495         1.0000          0.0000
   20.0000       50.0000       925.245       18.5049        0.0000          0.9252       -0.0748         1.0000          0.0000
   40.0000       25.0000       962.625       38.5050        0.0001          0.9626       -0.0374         1.0000          0.0000
                                                      RMS=  0.0001                  RMS=  0.1713                   RMS=  0.0000

TOS= 1.4951   SD= 0.0001    AO= 1.0000    SD= 0.0000
NO REJECTION CRITERIA APPLIED
NUMBER OF GOOD AREAS= 100 OUT OF 100 AREAS SAMPLED

SAMP NUMBER=  25 SHUTTER OFFSET=   1.047477 MS
SAMP NUMBER=  76 SHUTTER OFFSET=   1.146170 MS
SAMP NUMBER= 127 SHUTTER OFFSET=   1.245006 MS
SAMP NUMBER= 178 SHUTTER OFFSET=   1.346588 MS
SAMP NUMBER= 229 SHUTTER OFFSET=   1.445356 MS
SAMP NUMBER= 280 SHUTTER OFFSET=   1.544648 MS
SAMP NUMBER= 331 SHUTTER OFFSET=   1.644145 MS
SAMP NUMBER= 382 SHUTTER OFFSET=   1.745007 MS
SAMP NUMBER= 433 SHUTTER OFFSET=   1.844074 MS
SAMP NUMBER= 484 SHUTTER OFFSET=   1.942971 MS
Saving shutter offsets...
CCDRECIP task completed
ush cat area.tbl
ush cat off.tbl
ush cat meanoff.tbl
ccdrecip test.ltf 'SAMP  light=(0,100,50,25) 'RADIANCE  +
 arraysiz=512  offset=so.file  areatbl=area.tbl  +
 offtbl=off.tbl  avofftbl=meanoff.tbl reject=0  +
 plot=test4 plotfmt=eps
Beginning VICAR task ccdrecip
CCDRECIP VERSION 06-Jul-2013
----TASK:F2      ----USER:lwk         Tue Sep 24 13:48:29 2013
----TASK:LTGEN   ----USER:lwk         Tue Sep 24 13:50:16 2013

ENERGY UNIT = PICOAMP-MILLISECONDS

AREA   SL   SS   NL   NS  A0 (DN/ENERGY UNIT)  TOS (MILLISECONDS)
   1   15   15   20   20          1.00050              1.04748
   2   15   66   20   20          1.00035              1.14617
   3   15  117   20   20          1.00023              1.24501
   4   15  168   20   20          1.00020              1.34659
   5   15  219   20   20          1.00005              1.44536
   6   15  270   20   20          0.99995              1.54465
   7   15  321   20   20          0.99982              1.64414
   8   15  372   20   20          0.99975              1.74501
   9   15  423   20   20          0.99965              1.84407
  10   15  474   20   20          0.99950              1.94297
  11   66   15   20   20          1.00050              1.04748
  12   66   66   20   20          1.00035              1.14617
  13   66  117   20   20          1.00023              1.24501
  14   66  168   20   20          1.00020              1.34659
  15   66  219   20   20          1.00005              1.44536
  16   66  270   20   20          0.99995              1.54465
  17   66  321   20   20          0.99982              1.64414
  18   66  372   20   20          0.99975              1.74501
  19   66  423   20   20          0.99965              1.84407
  20   66  474   20   20          0.99950              1.94297
  21  117   15   20   20          1.00050              1.04748
  22  117   66   20   20          1.00035              1.14617
  23  117  117   20   20          1.00023              1.24501
  24  117  168   20   20          1.00020              1.34659
  25  117  219   20   20          1.00005              1.44536
  26  117  270   20   20          0.99995              1.54465
  27  117  321   20   20          0.99982              1.64414
  28  117  372   20   20          0.99975              1.74501
  29  117  423   20   20          0.99965              1.84407
  30  117  474   20   20          0.99950              1.94297
  31  168   15   20   20          1.00050              1.04748
  32  168   66   20   20          1.00035              1.14617
  33  168  117   20   20          1.00023              1.24501
  34  168  168   20   20          1.00020              1.34659
  35  168  219   20   20          1.00005              1.44536
  36  168  270   20   20          0.99995              1.54465
  37  168  321   20   20          0.99982              1.64414
  38  168  372   20   20          0.99975              1.74501
  39  168  423   20   20          0.99965              1.84407
  40  168  474   20   20          0.99950              1.94297
  41  219   15   20   20          1.00050              1.04748
  42  219   66   20   20          1.00035              1.14617
  43  219  117   20   20          1.00023              1.24501
  44  219  168   20   20          1.00020              1.34659
  45  219  219   20   20          1.00005              1.44536
  46  219  270   20   20          0.99995              1.54465
  47  219  321   20   20          0.99982              1.64414
  48  219  372   20   20          0.99975              1.74501
  49  219  423   20   20          0.99965              1.84407
  50  219  474   20   20          0.99950              1.94297
  51  270   15   20   20          1.00050              1.04748
  52  270   66   20   20          1.00035              1.14617
  53  270  117   20   20          1.00023              1.24501
  54  270  168   20   20          1.00020              1.34659
  55  270  219   20   20          1.00005              1.44536
  56  270  270   20   20          0.99995              1.54465
  57  270  321   20   20          0.99982              1.64414
  58  270  372   20   20          0.99975              1.74501
  59  270  423   20   20          0.99965              1.84407
  60  270  474   20   20          0.99950              1.94297
  61  321   15   20   20          1.00050              1.04748
  62  321   66   20   20          1.00035              1.14617
  63  321  117   20   20          1.00023              1.24501
  64  321  168   20   20          1.00020              1.34659
  65  321  219   20   20          1.00005              1.44536
  66  321  270   20   20          0.99995              1.54465
  67  321  321   20   20          0.99982              1.64414
  68  321  372   20   20          0.99975              1.74501
  69  321  423   20   20          0.99965              1.84407
  70  321  474   20   20          0.99950              1.94297
  71  372   15   20   20          1.00050              1.04748
  72  372   66   20   20          1.00035              1.14617
  73  372  117   20   20          1.00023              1.24501
  74  372  168   20   20          1.00020              1.34659
  75  372  219   20   20          1.00005              1.44536
  76  372  270   20   20          0.99995              1.54465
  77  372  321   20   20          0.99982              1.64414
  78  372  372   20   20          0.99975              1.74501
  79  372  423   20   20          0.99965              1.84407
  80  372  474   20   20          0.99950              1.94297
  81  423   15   20   20          1.00050              1.04748
  82  423   66   20   20          1.00035              1.14617
  83  423  117   20   20          1.00023              1.24501
  84  423  168   20   20          1.00020              1.34659
  85  423  219   20   20          1.00005              1.44536
  86  423  270   20   20          0.99995              1.54465
  87  423  321   20   20          0.99982              1.64414
  88  423  372   20   20          0.99975              1.74501
  89  423  423   20   20          0.99965              1.84407
  90  423  474   20   20          0.99950              1.94297
  91  474   15   20   20          1.00050              1.04748
  92  474   66   20   20          1.00035              1.14617
  93  474  117   20   20          1.00023              1.24501
  94  474  168   20   20          1.00020              1.34659
  95  474  219   20   20          1.00005              1.44536
  96  474  270   20   20          0.99995              1.54465
  97  474  321   20   20          0.99982              1.64414
  98  474  372   20   20          0.99975              1.74501
  99  474  423   20   20          0.99965              1.84407
 100  474  474   20   20          0.99950              1.94297

Global value for A0...
Raw mean and sigma are...
N= 100 MEAN=     1.00000 SIGMA=     0.00019
After throwing out samples differing by 2 sigma
N=  80 MEAN=     1.00000 SIGMA=     0.00012

Global shutter offset...
Raw mean and sigma are...
N= 100 MEAN=     1.49514 SIGMA=     0.28613
After throwing out samples differing by 2 sigma
N= 100 MEAN=     1.49514 SIGMA=     0.28613

AREA   1 (SL,SS,NL,NS)=(  15,  15,  20,  20)  ***BAD SENSITIVITY******
AREA  10 (SL,SS,NL,NS)=(  15, 474,  20,  20)  ***BAD SENSITIVITY******
AREA  11 (SL,SS,NL,NS)=(  66,  15,  20,  20)  ***BAD SENSITIVITY******
AREA  20 (SL,SS,NL,NS)=(  66, 474,  20,  20)  ***BAD SENSITIVITY******
AREA  21 (SL,SS,NL,NS)=( 117,  15,  20,  20)  ***BAD SENSITIVITY******
AREA  30 (SL,SS,NL,NS)=( 117, 474,  20,  20)  ***BAD SENSITIVITY******
AREA  31 (SL,SS,NL,NS)=( 168,  15,  20,  20)  ***BAD SENSITIVITY******
AREA  40 (SL,SS,NL,NS)=( 168, 474,  20,  20)  ***BAD SENSITIVITY******
AREA  41 (SL,SS,NL,NS)=( 219,  15,  20,  20)  ***BAD SENSITIVITY******
AREA  50 (SL,SS,NL,NS)=( 219, 474,  20,  20)  ***BAD SENSITIVITY******
AREA  51 (SL,SS,NL,NS)=( 270,  15,  20,  20)  ***BAD SENSITIVITY******
AREA  60 (SL,SS,NL,NS)=( 270, 474,  20,  20)  ***BAD SENSITIVITY******
AREA  61 (SL,SS,NL,NS)=( 321,  15,  20,  20)  ***BAD SENSITIVITY******
AREA  70 (SL,SS,NL,NS)=( 321, 474,  20,  20)  ***BAD SENSITIVITY******
AREA  71 (SL,SS,NL,NS)=( 372,  15,  20,  20)  ***BAD SENSITIVITY******
AREA  80 (SL,SS,NL,NS)=( 372, 474,  20,  20)  ***BAD SENSITIVITY******
AREA  81 (SL,SS,NL,NS)=( 423,  15,  20,  20)  ***BAD SENSITIVITY******
AREA  90 (SL,SS,NL,NS)=( 423, 474,  20,  20)  ***BAD SENSITIVITY******
AREA  91 (SL,SS,NL,NS)=( 474,  15,  20,  20)  ***BAD SENSITIVITY******
AREA 100 (SL,SS,NL,NS)=( 474, 474,  20,  20)  ***BAD SENSITIVITY******


  COMMANDED    ILLUMINATION    (DN-DC)      (DN-DC)/L     RESIDUAL      (DN-DC)/(L*T)   RESIDUAL  (DN-DC)/[L*(T-TOS)]  RESIDUAL
EXPOSURE T(MS)   L(RAD)                    (DN/ENERGY    (DN/ENERGY      (DN/ENERGY    (DN/ENERGY     (DN/ENERGY      (DN/ENERGY
                                              UNIT)         UNIT)           UNIT)         UNIT)          UNIT)           UNIT)
   10.0000      100.0000       850.495        8.5050        0.0000          0.8505       -0.1495         1.0000          0.0000
   20.0000       50.0000       925.245       18.5049        0.0000          0.9252       -0.0748         1.0000          0.0000
   40.0000       25.0000       962.625       38.5050        0.0001          0.9626       -0.0374         1.0000          0.0000
                                                      RMS=  0.0001                  RMS=  0.1713                   RMS=  0.0000

TOS= 1.4951   SD= 0.0001    AO= 1.0000    SD= 0.0000
NO REJECTION CRITERIA APPLIED
NUMBER OF GOOD AREAS= 100 OUT OF 100 AREAS SAMPLED

SAMP NUMBER=  25 SHUTTER OFFSET=   1.047477 MS
SAMP NUMBER=  76 SHUTTER OFFSET=   1.146170 MS
SAMP NUMBER= 127 SHUTTER OFFSET=   1.245006 MS
SAMP NUMBER= 178 SHUTTER OFFSET=   1.346588 MS
SAMP NUMBER= 229 SHUTTER OFFSET=   1.445356 MS
SAMP NUMBER= 280 SHUTTER OFFSET=   1.544648 MS
SAMP NUMBER= 331 SHUTTER OFFSET=   1.644145 MS
SAMP NUMBER= 382 SHUTTER OFFSET=   1.745007 MS
SAMP NUMBER= 433 SHUTTER OFFSET=   1.844074 MS
SAMP NUMBER= 484 SHUTTER OFFSET=   1.942971 MS
Saving shutter offsets...
CCDRECIP task completed
ush gnuplot test4.eps.gpi
goto rm
if ($syschar(1) = "UNIX")
end-if
if ($syschar(1) = "UNIX")
end-if
  ush rm -f l.list
  ush rm -f m.list
  ush rm -f n.list
  ush rm -f cas
  ush rm -f test?.eps
  ush rm -f test*.gpi
  ush rm -f test*.asc
  ush rm -f list?.dat
  ush rm -f ?.dat
  ush rm -f xa.dat
  ush rm -f l?.?
  ush rm -f m?.?
  ush rm -f so.file
  ush rm -f *off*.tbl
  ush rm -f area*.tbl
  ush rm -f test*.ltf
end-proc
exit
slogoff
if ($RUNTYPE = "INTERACTIVE")
  if ($syschar(1) = "VAX_VMS")
  end-if
else
  if ($syschar(1) = "VAX_VMS")
  end-if
end-if
ulogoff
END-PROC
END-PROC
$ Return
$!#############################################################################
