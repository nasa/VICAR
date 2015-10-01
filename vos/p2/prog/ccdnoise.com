$!****************************************************************************
$!
$! Build proc for MIPL module ccdnoise
$! VPACK Version 1.9, Thursday, October 24, 2013, 16:28:07
$!
$! Execute by entering:		$ @ccdnoise
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
$ write sys$output "*** module ccdnoise ***"
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
$ write sys$output "Invalid argument given to ccdnoise.com file -- ", primary
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
$   if F$SEARCH("ccdnoise.imake") .nes. ""
$   then
$      vimake ccdnoise
$      purge ccdnoise.bld
$   else
$      if F$SEARCH("ccdnoise.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake ccdnoise
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @ccdnoise.bld "STD"
$   else
$      @ccdnoise.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create ccdnoise.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack ccdnoise.com -mixed -
	-s ccdnoise.f -
	-i ccdnoise.imake -
	-p ccdnoise.pdf -
	-t tstccdnoise.pdf tstccdnoise.log_solos
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ccdnoise.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
C VICAR PROGRAM CCDNOISE
C
C Radiometric calibration routine:  Determines system gain constant (in
C electrons per DN) and the read-noise floor (DN) for a CCD camera system.
C
C         CCDNOISE LTF MARK
C
C If no extended dark currents are present, the records (L) in the LTF
C contain the following:
C   L=1    LTF header record.
C   L=2    Data record for exposure level 0 (dark current)
C   L=3    Data record for exposure level 1
C   L=NLI  Data record for hightest exposure level
C If extended dark currents are present, then:
C   L=1    LTF header record.
C   L=2    Data record for exposure level -1 (extended dark current)
C   L=3    Data record for exposure level 0 (normal dark current)
C   L=4    Data record for exposure level 1
C   L=NLI  Data record for hightest exposure level
C
c	iuni		INP
c	ouni		OUT
c	11		TABLE_DS
C	12		plot. asc
c	97		plot eps.gpi file
c	98		plot gpi file
c
      SUBROUTINE MAIN44
  	implicit none 
         REAL*8 RMEAN,SIGMA,SSUM(10),SSUM2(10),SSUMXY(10)
         REAL*8 EPDNMEAN,RDNMEAN,UI,UI2,SI2,UISI2
         REAL*4 EXP(2),reticle(2,5),OUT(6400),OUTM(2,2000)
         REAL*4 RDNBUF(400),EXPOS(30),EPDBUF(400)
         REAL*4 RSIGNAL(30,5),RNOISE(30,5),REPDN(5),RRDN(5)
         REAL*4 EDC(400),U(30,400),S(30,400),bexpo
         REAL*4 D,DMIN,EPDN,EXP1,EXP2,EXPO,RDN
         INTEGER*4 AREA(4,400)
         INTEGER*4 SL,SS,SLI,SSI,STAT,CNT,BAD(400),GAREA,EFNUM
         INTEGER*4 NG(5),TABLE,NI
	 INTEGER*4 dummy1, dummy2,I,IB,IDEF,IEDC,IMIN,IPLOT,IREJCT
         INTEGER*4 IUNI,OUNI,J,K,L,L1,L2,MAXL,MAXL2,MAXS,MAXS2,N
         INTEGER*4 NAREA,NEXP,NL,NS,NLAB,NLI,NLO,NPTS,NREJCT
         INTEGER*4 NSI,NSO,nplotgpi,nplotgpi2,nploteps,nplotname
         INTEGER*4 ninpfile,ntbl,icount
         LOGICAL*4 DBUG, XVPTST,epsplot, plotdata

        CHARACTER*8 pformat
	CHARACTER*63 plotname
	character*80 plotgpi,plotgpi2,ploteps,tbl
         CHARACTER*40 table_ds
         CHARACTER*41 RMSG
         CHARACTER*41 MS2
         CHARACTER*51 ARMES
         CHARACTER*85 MS1
         CHARACTER*132 MSG
         CHARACTER*100 PLOT_DS,cbuf,inpfile
         CHARACTER*4320 LABEL

        character*4 gpi/'.gpi'/,eps/'.eps'/,asc/'.asc'/

         ARMES = 'NUMBER OF GOOD AREAS=**** OUT OF**** AREAS SAMPLED'
         RMSG = 'NUMBER REJECTED FOR               ='
         MS2 = 'K=XXXXXX.XXXXX E/DN RDN=******.***** DN'
         MS1 = 'AREA XXX (SL,SS,NL,NS)=(XXXX,XXXX,XXXX,XXXX)'

         CALL XVMESSAGE('CCDNOISE version 13-Jul-2013',' ')

        epsplot = .false.
        nplotgpi = 0
        nplotgpi2 = 0
        nplotname = 0
        nploteps = 0
	ntbl = 0
	ninpfile = 0

C        IREJECT=0  Do not reject areas
C               =1  Reject areas with gain terms 2 sigma from average
C               =2  Reject areas with noise terms 2 sigma from average
C	        =3  Reject areas with both bad gain and noise terms
         DBUG = XVPTST('DBUG')
	call xvparm('INP',cbuf,icount,idef,1)
	  inpfile = cbuf
	  ninpfile=index(inpfile,'   ') - 1

	plotdata = .false.
         CALL XVPARM('PLOT',PLOT_DS,IPLOT,IDEF,1)
         IF (IPLOT .GT. 0) THEN
c            CALL PLOTFN(PLOT_DS)
	      plotdata= .true.
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
         ENDIF

         call xvp ('PLOTFMT',pformat,cnt)
	 if (pformat .eq. 'eps' .or. pformat .eq. 'EPS') epsplot = .true.


	 call xvparm('TABLE',TABLE_DS,TABLE,IDEF,1)

         CALL XVP('REJECT',IREJCT,CNT)
         CALL XVP('LIMIT',EXP,CNT)
         IF (CNT .EQ. 2) THEN
            EXP1 = EXP(1)
            EXP2 = EXP(2)
         ELSE
            EXP1 = 0.
            EXP2 = 999999.
         ENDIF
         CALL XVP('EXTEXPO',EFNUM,IEDC)
         IF (EFNUM .EQ. 0) IEDC=0   !Check for extended dark current

         CALL XVUNIT(IUNI,'INP',1,STAT,' ')
         CALL XVOPEN(IUNI,STAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
         CALL XVSIZE(SLI,SSI,NLI,NSI,dummy1,dummy2)
         CALL LABPROC(IUNI,LABEL,NLAB)
C        Read in area size fields...
         CALL XLGET(IUNI,'HISTORY','NUM_AREAS',NAREA,STAT,
     &              'FORMAT','INT',' ')
         CALL XLGET(IUNI,'HISTORY','AREAS',AREA,STAT,'NELEMENT',4*NAREA,
     &              'FORMAT','INT',' ')
         CALL XLGET(IUNI,'HISTORY','NUM_EXPOS',NEXP,STAT,
     &              'FORMAT','INT',' ')
         CALL XLGET(IUNI,'HISTORY','EXPOSURES',EXPOS,STAT,
     &              'NELEMENT',NEXP,'FORMAT','REAL',' ')
         L1 =1 
         L2 = NLI
         NPTS = NLI - IEDC	!Number of points (exposures) of light transfer curve
C
C  Compute mean DN (U) and sigma (S) for each area...
C      U(1,K) = mean DC for area K
C  For L=2,3,4,...,NLI
C      U(L,K) = mean signal at exposure L-2 for area K (dark current subtracted)
C      S(L,K) = mean noise at exposure L-2
C  Note that U(2,K) = mean signal at exposure 0 = DC - DC = 0 
C
         DO 50 L=1,NLI        !Loop through each exposure record...
            CALL XVREAD(IUNI,OUT,STAT,'LINE',L,'NSAMPS',NSI,' ')
            NI=NINT(OUT(1))
            IF (NI .EQ. 0) THEN
               BEXPO=OUT(L)
               GOTO 970
            END IF
            IF (EXPOS(L) .LT. EXP1) L1=L+1
            IF (EXPOS(L) .LE. EXP2) L2=L
            IB=1

            DO 50 K=1,NAREA	   !Loop through each area...
               SL=AREA(1,K)                      !Get area size field
               SS=AREA(2,K)
               NL=AREA(3,K)
               NS=AREA(4,K)
               CALL MVE(9,NI,OUT(IB+1),SSUM,1,1)
               CALL MVE(9,NI,OUT(IB+NI+1),SSUM2,1,1)
               CALL MVE(9,NI,OUT(IB+2*NI+1),SSUMXY,1,1)
               N = NL*NS
               SIGMA = 0.D0
               RMEAN = SSUM(1)

               DO I=2,NI
                  RMEAN = RMEAN+SSUM(I)
                  SIGMA = SIGMA+(SSUM2(I)+SSUM2(I-1)-2*SSUMXY(I-1))/N
     &                    -((SSUM(I)-SSUM(I-1))/N)**2
               ENDDO

               RMEAN = RMEAN/(NI*N)

               IF (NI .GT. 1) SIGMA=SIGMA/(NI-1)
               IF (SIGMA .LT. 0.D0) SIGMA=0.D0

               IF (IEDC .EQ. 0) THEN
                  IF (L .EQ. 1) U(1,K)=RMEAN		!mean DC
c                  U(L+1,K) = RMEAN - U(1,K)		!signal = DN - DC
c                  S(L+1,K) = DSQRT(SIGMA/2.D0)
		    U(L,K) = RMEAN !- U(1,K)           !signal = DN - DC
		    S(L,K) = DSQRT(SIGMA/2.D0)
               ELSE
                  IF (L .EQ. 1) THEN
                     EDC(K) = RMEAN			!extended DC
                     GOTO 50
                  ENDIF
                  IF (L .EQ. 2) U(1,K)=RMEAN		!normal DC
                  IF (L .LT. EFNUM+3) THEN
                     U(L,K) = RMEAN - U(1,K)
                  ELSE
                     U(L,K) = RMEAN - EDC(K)
                  ENDIF
                  S(L,K) = DSQRT(SIGMA/2.D0)
               ENDIF
           
   50    IB = IB+3*NI

         IF (L1 .GE. L2) GOTO 998
         IF (IEDC .EQ. 1) THEN
             L1 = L1 - 1		!EDC is not stored in U or S
             L2 = L2 - 1
         ENDIF

         MAXL = 0			!Maximum line number encountered
         MAXS = 0			!Maximum sample number encountered
         MAXL2 = 0			!Maximum last line number encountered
         MAXS2 = 0			!Maximum last sample number encountered
C
C          Compute system gain constant in electrons per DN (EPDN)
C          and read noise...
C
         DO 100 K=1,NAREA
            SL=AREA(1,K)                      !Get area size field
            SS=AREA(2,K)
            NL=AREA(3,K)
            NS=AREA(4,K)
            MAXL = MAX0(MAXL,SL)
            MAXS = MAX0(MAXS,SS)
            MAXL2 = MAX0(MAXL2,SL+NL-1)
            MAXS2 = MAX0(MAXS2,SS+NS-1)
            N = L2 - L1 + 1		!Number of light levels in fit
            UI = 0.D0
            UI2 = 0.D0
            SI2 = 0.D0
            UISI2 = 0.D0
C           Least squares fit over signal/noise curve...
            DO L=L1,L2
               RMEAN = U(L+1,K)	!mean signal (DN)
               SIGMA = S(L+1,K)	!noise (DN)
               UI = UI + RMEAN
               UI2 = UI2 + RMEAN**2
               SI2 = SI2 + SIGMA**2
               UISI2 = UISI2 + RMEAN*SIGMA**2
            ENDDO

            D = N*UI2 - UI**2			!Calculate read noise and
            IF (D .ne. 0.) THEN
               RDN = (SI2*UI2-UISI2*UI)/D	!system gain constant (in
            else
               rdn = 0.
            endif
            IF (RDN .GT. 0.) RDN=SQRT(RDN)	!electrons per DN) via least
            EPDN = D/(N*UISI2-UI*SI2)		!squares fit...
            RDNBUF(K) = RDN
            EPDBUF(K) = EPDN

            WRITE (MS1(6:8),'(I3)') K
            WRITE (MS1(25:28),'(I4)') SL
            WRITE (MS1(30:33),'(I4)') SS
            WRITE (MS1(35:38),'(I4)') NL
            WRITE (MS1(40:43),'(I4)') NS
            WRITE (MS2(3:14),'(F12.5)') EPDN
            WRITE (MS2(25:36),'(F12.5)') RDN
            MS1(46:85) = MS2
            IF (DBUG) CALL XVMESSAGE(' ',' ')
            CALL XVMESSAGE(MS1,' ')
c	print out all areas	   
            IF (DBUG)
     &         CALL AREADATA(EXPOS(1),U(1,K),S(1,K),NPTS,EPDN,RDN)
  100    CONTINUE

         CALL ZIA(BAD,NAREA)
C        Calculate global system gain and noise constants and
C        weed out bad areas...
         IF (NAREA .GT. 1) THEN
            CALL XVMESSAGE(' ',' ')
            CALL XVMESSAGE('Global value for K...',' ')
            CALL IMEAN(EPDBUF,1,NAREA,BAD)
            CALL XVMESSAGE(' ',' ')
            CALL XVMESSAGE('Global noise floor...',' ')
            CALL IMEAN(RDNBUF,2,NAREA,BAD)
         ENDIF

         CALL XVMESSAGE(' ',' ')
         MS1(1:70) = ' '
         I = 0
C        Report all bad areas...              
         DO 200 K=1,NAREA
            SL=AREA(1,K)                      !Get area size field
            SS=AREA(2,K)
            NL=AREA(3,K)
            NS=AREA(4,K)

            MS1 = ' '
            WRITE (MS1,'(A4,I4,A16,I4,A1,I4,A1,I4,A1,I4,A1)')
     &        'AREA',K,'(SL,SS,NL,NS)=(',SL,',',SS,',',NL,',',NS,')'
            IF (BAD(K).EQ.0) GOTO 200
            IF (BAD(K).EQ.1) MS1(47:71) = '*****BAD SYSTEM GAIN*****'
            IF (BAD(K).EQ.2) MS1(47:71) = '*****BAD NOISE FLOOR*****'
            IF (BAD(K).EQ.4) MS1(47:71) = '****BOTH CONSTANTS BAD***'
            CALL XVMESSAGE(MS1,' ')

            IF (IREJCT .EQ. 0) GOTO 200
            IF ((IREJCT .NE. 3) .AND.
     &          (IREJCT .NE. BAD(K)) .AND.
     &          (BAD(K) .NE. 4)) GOTO 200
            I = I + 1
            OUTM(1,I)=SL+NL/2   !Store center of bad area in MARK output record
            OUTM(2,I)=SS+NS/2
  200    CONTINUE
C
C          Compute means of all good areas
C
         GAREA = 0         !number of good areas
         EPDNMEAN = 0.D0   !mean system gain constant
         RDNMEAN = 0.D0	   !mean read noise

         DO 210 K=1,NAREA
            IF ((IREJCT .GT. 0 .AND. IREJCT .EQ. BAD(K)) .OR.
     &          (IREJCT .EQ. 3 .AND. BAD(K) .GT. 0) .OR.
     &          (IREJCT .GT. 0 .AND. BAD(K) .EQ. 4)) GOTO 210
            GAREA = GAREA + 1
            EPDNMEAN = EPDNMEAN + EPDBUF(K)
            RDNMEAN = RDNMEAN + RDNBUF(K)
  210    CONTINUE

         EPDNMEAN = EPDNMEAN/GAREA
         RDNMEAN = RDNMEAN/GAREA
         NREJCT = NAREA - GAREA

         MSG(43:91)= ' '
         CALL XVMESSAGE(' ',' ')
         IF (IREJCT.EQ.1) RMSG(21:34)=' SYSTEM GAIN  '
         IF (IREJCT.EQ.2) RMSG(21:34)=' NOISE FLOOR  '
         IF (IREJCT.EQ.3) RMSG(21:34)=' NOISE OR GAIN'
	 if (irejct .eq. 0) then
            call xvmessage('No rejection criteria applied',' ')
	 else
            WRITE (RMSG(39:41),'(I3)') NREJCT
            CALL XVMESSAGE(RMSG,' ')
	 end if
         WRITE (ARMES(22:25),'(I4)') GAREA
         WRITE (ARMES(33:36),'(I4)') NAREA
         CALL XVMESSAGE(ARMES,' ')
         WRITE (MS2(3:14),'(F12.5)') EPDNMEAN
         WRITE (MS2(25:36),'(F12.5)') RDNMEAN
         CALL XVMESSAGE(MS2,' ')
         CALL XVMESSAGE(' ',' ')
C
C          Write centers of bad areas into MARK format file...
C
         CALL XVUNIT(OUNI,'OUT',1,STAT,' ')
         IF (STAT.NE.1) GOTO 300		!Skip if no output specified...
         NSO = 2*NREJCT
         NLO = 1
         CALL XVOPEN(OUNI,STAT,'OPEN_ACT','SA','IO_ACT','SA',
     &               'OP','WRITE','U_NL',NLO,'U_NS',NSO,'U_FORMAT',
     &               'REAL','O_FORMAT','REAL',' ')
         IF (IREJCT .EQ. 1)
     &      CALL XLADD(OUNI,'HISTORY','CCDNOISE',
     &                 ' REJECTED FOR SYSTEM GAIN',STAT,'FORMAT',
     &                 'STRING',' ')      
         IF (IREJCT .EQ. 2)
     &      CALL XLADD(OUNI,'HISTORY','CCDNOISE',
     &                 ' REJECTED FOR NOISE FLOOR',STAT,'FORMAT',
     &                 'STRING',' ')      
         IF (IREJCT .EQ. 3)
     &      CALL XLADD(OUNI,'HISTORY','CCDNOISE',
     &                 ' REJECTED FOR BOTH',STAT,'FORMAT',
     &                 'STRING',' ')
         CALL XVWRIT(OUNI,OUTM,STAT,'NSAMPS',NSO,' ')
         CALL XVCLOSE(OUNI,STAT,' ')

C        Plot or table ....
  300    IF ((plotdata) .and. (table .eq. 0)) GOTO 400
C
C-----Set Reticle points based upon the last line/samp used
	 reticle(1,1) = 1            !upper left
	 reticle(2,1) = 1
	 reticle(1,2) = 1            !upper right
	 reticle(2,2) = maxs2
	 reticle(1,3) = maxl2        !lower left
	 reticle(2,3) = 1
	 reticle(1,4) = maxl2        !lower right
	 reticle(2,4) = maxs2
	 reticle(1,5) = maxl2/2      !center
	 reticle(2,5) = maxs2/2
c  zero out ng
	do i=1,5
	  ng(i) = 0
	enddo
C    zero out new arrays
            DO I=1,5
                DO J=1,NPTS
                    RSIGNAL(J,I)=0.0
                    RNOISE(J,I)=0.0
                ENDDO
            ENDDO

c
C        Compute signal and noise at each of the reticle points...
         DO 305 K=1,NAREA
            IF ((IREJCT .EQ. 1) .AND.
     &          ((BAD(K) .EQ. 1) .OR. (BAD(K) .EQ. 4))) GOTO 305
            IF ((IREJCT .EQ. 2) .AND.
     &          ((BAD(K) .EQ. 2) .OR. (BAD(K) .EQ. 4))) GOTO 305
            IF ((IREJCT .EQ. 3) .AND. (BAD(K) .NE. 0)) GOTO 305
            SL=AREA(1,K)                      !Get area size field
            SS=AREA(2,K)
            NL=AREA(3,K)
            NS=AREA(4,K)
C           Find nearest reticle...
            DMIN = 99999.**2
	    IMIN=0
            DO I=1,5
               D = (RETICLE(1,I)-SL)**2 + (RETICLE(2,I)-SS)**2
               IF (D .LT. DMIN) THEN
                  DMIN = D
                  IMIN = I
               ENDIF
            ENDDO
C           Add signal and noise data to that reticle...
            NG(IMIN) = NG(IMIN) + 1
C    zero out new arrays
            DO J=1,NPTS
               RSIGNAL(J,IMIN) = RSIGNAL(J,IMIN) + U(J,K)	!U(J+1,K)
               RNOISE(J,IMIN) = RNOISE(J,IMIN) + S(J,K)		!S(J+1,K)
            ENDDO
  305    CONTINUE

         DO I=1,5
            IF (NG(I) .GT. 0) THEN
               DO J=1,NPTS
                  RSIGNAL(J,I) = RSIGNAL(J,I)/NG(I)
                  RNOISE(J,I) = RNOISE(J,I)/NG(I)
               ENDDO
            ENDIF
         ENDDO
C
C        Compute system gain constant and read noise at reticles...
         DO 350 I=1,5
            N = L2 - L1 + 1		!Number of light levels in fit
            UI = 0.D0
            UI2 = 0.D0
            SI2 = 0.D0
            UISI2 = 0.D0
C           Least squares fit over signal/noise curve...
            DO L=L1,L2
               RMEAN = RSIGNAL(L,I)  !mean signal (DN)
               SIGMA = RNOISE(L,I)   !noise (DN)
               UI = UI + RMEAN
               UI2 = UI2 + RMEAN**2
               SI2 = SI2 + SIGMA**2
               UISI2 = UISI2 + RMEAN*SIGMA**2
            ENDDO

            D = N*UI2 - UI**2			!Calculate read noise and
            RDN = (SI2*UI2-UISI2*UI)/D	!system gain constant (in
            IF (RDN .GT. 0.) RDN=SQRT(RDN)	!electrons per DN) via least
            EPDN = D/(N*UISI2-UI*SI2)		!squares fit...
            RRDN(I) = RDN
            REPDN(I) = EPDN
350      continue

         if (plotdata) then
            CALL NPLOT(NPTS,EXPOS(IEDC+1),RSIGNAL,RNOISE,REPDN,RRDN,
     &     IREJCT,NG,LABEL,NLAB,ARMES,RMSG,MS2,plotgpi,nplotgpi,plotgpi2,
     &     nplotgpi2,epsplot,ploteps,nploteps,tbl,ntbl,*997)
         end if

         if (table .eq. 1) then
            call wrttbl(npts,expos(iedc+2),rsignal,rnoise,repdn,rrdn,
     &             table_ds,*999)
         end if

         CALL XVCLOSE(IUNI,STAT,' ')

  400    CALL XVMESSAGE('CCDNOISE task completed',' ')
         RETURN
CCCCCCCCCCCCCCCC
C          ERROR CONDITIONS
  970    CALL PRNT(7,1,EXPO,'***No data for exposure=.')
         CALL XVMESSAGE('***Run MOMGEN on this exposure and try again.',
     &               ' ')
         GOTO 999
  997    CALL XVMESSAGE('??E - Plot Error.',' ')
         GOTO 999
  998    CALL XVMESSAGE('??E - Err in exposure ranges',' ')
  999    CALL XVMESSAGE('??E - CCDNOISE task cancelled',' ')
         CALL ABEND
       END


C***********************************************************************8
	SUBROUTINE WRTTBL(NPTS,EXP,RSIG,RNOI,REPDN,RRDN,TABLE_DS,*)
C---Routine to write signal and noise tab-delimitted table
c
	implicit none
	   INTEGER*4 NPTS,JST,L
           REAL*4 RSIG(30,5),RNOI(30,5),REPDN(5),RRDN(5)
           REAL*4 EXP(npts),CN1,CN2,CN3,CN4,CN5

           CHARACTER*1 TAB
           CHARACTER*40 TABLE_DS

C-------RSIG AND RNOI ARE (NPTS,NUMBER OF RETICLE AREAS)
C-------REPDN AND RRDN ARE ELECTRONS/DN AND READ NOISE AND ARE
C-------(NUMBER OF RETICLE AREAS)

           OPEN(11,FILE=TABLE_DS,STATUS='UNKNOWN',RECL=1200,
     &          IOSTAT=JST,ERR=999)
	   TAB=CHAR(9)
	
C-------write header for table identifying columns
	   WRITE(11,8) 'Exp.time',
     &       TAB,'Signal-UL',TAB,'Noise-UL',TAB,'Calc.noise-UL',
     &       TAB,'Signal-UR',TAB,'Noise-UR',TAB,'Calc.noise-UR',
     &       TAB,'Signal-LL',TAB,'Noise-LL',TAB,'Calc.noise-LL',
     &       TAB,'Signal-LR',TAB,'Noise-LR',TAB,'Calc.noise-LR',
     &       TAB,'Signal-CEN',TAB,'Noise-CEN',TAB,'Calc.noise-CEN'

	   DO L=1,NPTS
C-------compute the computed noise value for each area at this exp. level
	      CN1 = SQRT(RSIG(L,1)/REPDN(1) + RRDN(1)**2)
	      CN2 = SQRT(RSIG(L,2)/REPDN(2) + RRDN(2)**2)
	      CN3 = SQRT(RSIG(L,3)/REPDN(3) + RRDN(3)**2)
	      CN4 = SQRT(RSIG(L,4)/REPDN(4) + RRDN(4)**2)
	      CN5 = SQRT(RSIG(L,5)/REPDN(5) + RRDN(5)**2)
	      WRITE(11,9) EXP(L),TAB,RSIG(L,1),TAB,RNOI(L,1),TAB,CN1,
     &          TAB,RSIG(L,2),TAB,RNOI(L,2),TAB,CN2,
     &          TAB,RSIG(L,3),TAB,RNOI(L,3),TAB,CN3,
     &          TAB,RSIG(L,4),TAB,RNOI(L,4),TAB,CN4,
     &          TAB,RSIG(L,5),TAB,RNOI(L,5),TAB,CN5
	   ENDDO

	   CLOSE(11)
	   RETURN

8	   FORMAT(1X,A8,15(A1,A14))
9	   FORMAT(1X,F12.5,15(A1,F12.4))
999	   CALL xvmessage('??E - ERROR OPENING TABLE FILE',' ')
	   CALL PRNT(4,1,JST,' IOSTAT =.')
	   RETURN 1
	END
C**********************************************************************
       SUBROUTINE IMEAN(BUF,TYPE,N,BAD)

C Routine to flag all areas which differ from mean by more than 2 sigma.
C Inputs: BUF(N) = input samples
C         TYPE = 1 for system gain constant, 2 for read noise
C         N = number of samples
C Outputs: BAD(N) = TYPE if sample is bad type
C                 = 4 if sample has both bad gain and noise

	implicit none

         INTEGER*4 BAD(*),TYPE,N,NS
	 INTEGER*4 K,IMAX

         REAL*4 BUF(*),MEAN,MAXDIFF,SAMP,SIGMA
         REAL*8 SUM,SSUM

         CHARACTER*43 MSG

         MSG = 'N=**** MEAN=******.***** SIGMA=******.******'

	 IMAX=0
         NS = 0			!Count of number of good samples
         SUM = 0.0D0
         SSUM = 0.0D0
C	 Get sums and sums of squares of all good areas...
         DO K=1,N
            IF ((BAD(K) .NE. TYPE) .AND. (BAD(K) .NE. 4)) THEN 
               SAMP = BUF(K)
               NS = NS+1
               SUM = SUM+DBLE(SAMP)
               SSUM = SSUM+DBLE(SAMP)*DBLE(SAMP)
            ENDIF
         ENDDO
C          ...and use these to compute mean and standard deviation.
         MEAN = SNGL(SUM)/NS
         SIGMA = SNGL(SSUM)/NS-MEAN*MEAN
         IF (SIGMA .GT. 0.0) SIGMA = SQRT(SIGMA)
         WRITE (MSG(3:6),'(I4)') NS
         WRITE (MSG(13:24),'(F12.5)') MEAN
         WRITE (MSG(32:43),'(F12.5)') SIGMA
         CALL XVMESSAGE('Raw mean and sigma are...',' ')
         CALL XVMESSAGE(MSG,' ')
         IF (SIGMA .EQ. 0.0) RETURN

C        Weed out all samples differing by more than 2 sigma from mean...
   10    MAXDIFF = 0.
C        First find sample with largest difference...
         DO 20 K=1,N
            IF ((BAD(K) .EQ. TYPE) .OR. (BAD(K) .EQ. 4)) GOTO 20
            SAMP = ABS(BUF(K)-MEAN)
            IF (SAMP .LE. MAXDIFF) GOTO 20
            MAXDIFF = SAMP
            IMAX = K          
   20    CONTINUE

C        Continue if remaining samples are good.
         IF (MAXDIFF .LE. 2*SIGMA) GOTO 50  
         SAMP = BUF(IMAX)
         SUM = SUM - SAMP			!Delete worst sample from sums,
         SSUM = SSUM - SAMP**2
         NS = NS - 1
         IF (BAD(IMAX) .NE. 0) THEN		!and flag area as bad.
            BAD(IMAX) = 4
         ELSE
            BAD(IMAX) = TYPE
         ENDIF
         GOTO 10

   50    MEAN = SNGL(SUM)/NS
         SIGMA = SNGL(SSUM)/NS-MEAN**2
         IF (SIGMA .GT. 0.0) SIGMA = SQRT(SIGMA)
         WRITE (MSG(3:6),'(I4)') NS
         WRITE (MSG(13:24),'(F12.5)') MEAN
         WRITE (MSG(32:43),'(F12.5)') SIGMA
         CALL XVMESSAGE 
     &      ('After throwing out samples differing by 2 sigma',
     &               ' ')
         CALL XVMESSAGE(MSG,' ')
         RETURN
      END         
C********************************************************************
      SUBROUTINE AREADATA(EXPOS,U,S,NPTS,EPDN,RDN)

C Routine to print out data for area...
C Inputs: EXPOS = exposure times in milliseconds
C         U = mean signal in DN
C         S = RMS noise in DN
C         NPTS = number of points of curve
C         EPDN = system gain constant (electrons/DN)
C         RDN = read noise (DN)
C
	implicit none
	 INTEGER*4 NPTS,L
         REAL*4 EPDN,RDN,R,RATIO,RMEAN,SIGMA,TNOISE
         REAL*4 EXPOS(NPTS),U(NPTS),S(NPTS),EXPO

         CHARACTER*132 MSG

         MSG = ' '
         MSG(1:51) = '    EXPOSURE   MEAN       MEASURED   LOG'          
         MSG(52:132) = 'LOG         COMPUTED       RATIO'
         CALL XVMESSAGE (MSG, ' ')
         MSG = ' '
         MSG(1:51) = '    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)'
         MSG(52:132) = 'NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)'
         CALL XVMESSAGE (MSG, ' ')

         DO L=1,NPTS
            EXPO = EXPOS(L)
            RMEAN = U(L)
            SIGMA = S(L)
            IF ((RMEAN .LT. 0.) .OR. (EPDN .LE. 0)) THEN
               TNOISE = 0.
               RATIO = 0.
            ELSE
               TNOISE = SQRT(RMEAN/EPDN + RDN**2)
               RATIO = SIGMA/TNOISE
            ENDIF
            MSG = ' '
            WRITE (MSG(2:13),'(F12.5)') EXPO !Exposure time (msec)
            WRITE (MSG(15:24),'(F10.4)') RMEAN !Mean signal (DN)
            WRITE (MSG(26:36),'(F11.5)') SIGMA !Noise (DN)
            IF (RMEAN .GT. .1D0) THEN
               R = LOG10(RMEAN)
               WRITE (MSG(38:47),'(F10.5)') R !LOG signal (DN)
            ENDIF
            IF (SIGMA .GT. 0.D0) THEN
               R = LOG10(SIGMA)
               WRITE (MSG(49:58),'(F10.5)') R !LOG noise (DN)
            ENDIF
            WRITE (MSG(60:69),'(F10.5)') TNOISE !Computed noise (DN)
            WRITE (MSG(71:80),'(F10.5)') RATIO !Ratio measured/computed
            CALL XVMESSAGE(MSG,' ')
         ENDDO

         RETURN
      END
C********************************************************************
      SUBROUTINE NPLOT(NPTS,EXPOS,RSIGNAL,RNOISE,REPDN,RRDN,IREJCT,
     &     NG,LABEL,NLAB,ARMES,RMSG,MS2,plotgpi,nplotgpi,plotgpi2,
     &     nplotgpi2,epsplot,ploteps,nploteps,tbl,ntbl,*)
C
C Routine to plot signal vs noise for the 5 reticles...
C All arguments are inputs.
C
        implicit none
         INTEGER*4 NPTS,NLAB,I,II,J,JJ,JST,L,N,IREJCT
	 INTEGER*4 plotwid,plotht,isize,nxtitle,nytitle
	 INTEGER*4 nplotgpi,nplotgpi2,nploteps,ntbl,nltitle
         INTEGER*4 ntitle(80),ncolx(5),ncoly(5)
         REAL*4 RMEAN,SIGMA,labstep,tmp,xrang1,xrang2,yrang1,yrang2
	 REAL*4 maxrmean,minrmean,maxsigma,minsigma
         REAL*4 EXPOS(30),RSIGNAL(30,5),RNOISE(30,5),REPDN(5),RRDN(5)
         REAL*4 X(30),Y(30) !Buffers for log signal and log noise

         INTEGER*4 NG(5),ICNT    !NG(30) befor 18-Nov-2012

	 LOGICAL*4 epsplot

	 CHARACTER*12 MSG(5), LMSG(6)
         CHARACTER*20 YTITLE
         CHARACTER*20 XTITLE
         CHARACTER*41 RMSG,MS2
         CHARACTER*44 REJSTR(3)
         CHARACTER*51 ARMES
         CHARACTER*62 MSG2
	 CHARACTER*80 ltitle
	 character*80 plotgpi,plotgpi2,ploteps,tbl
	CHARACTER*86 TITLE(80)
         CHARACTER*4320 LABEL

	 REJSTR(1)='Rejected for SYSTEM GAIN'
	 REJSTR(2)='Rejected for NOISE FLOOR'
	 REJSTR(3)='Rejected for SYSTEM GAIN and NOISE FLOOR'
         MSG2(1:52)='UPPER-RIGHT CORNER  K=*****.* E/DN  RDN=***.*** DN'
         MSG2(53:62) = 'NAREA=*** '
         ltitle = 'CCD NOISE ANALYSIS'
	 nltitle = index(ltitle,'    ') - 1
         icnt = 1
         ytitle = 'LOG NOISE (DN)'
	 nytitle = index(ytitle,'    ') - 1 
         xtitle = 'LOG SIGNAL (DN)'
	 nxtitle = index(xtitle,'    ') - 1
c
cnplotname=index(plotname,'   ') - 1
         MSG(1)='=UPPER-LEFT '
         MSG(2)='=UPPER-RIGHT'
         MSG(3)='=LOWER-LEFT '
         MSG(4)='=LOWER-RIGHT'
         MSG(5)='=CENTER     '
c
c  process labels
c
         N = MIN0(NLAB,25)
         DO L=1,N
            TITLE(ICNT)=LABEL((L-1)*72+1:(L-1)*72+72)
            ntitle(icnt) = index(title(icnt),'       ') - 1
            ICNT=ICNT+1
         ENDDO
	 if (irejct .eq. 0) then
	    TITLE(ICNT) = "No Rejection Criteria Applied"
	    ntitle(icnt) = index(title(icnt),'       ') - 1
	    ICNT=ICNT+1
	 else
	    TITLE(ICNT) = REJSTR(IREJCT)
	    ntitle(icnt) = index(title(icnt),'       ') - 1
            ICNT=ICNT+1
         endif
         TITLE(ICNT)=ARMES
	 ntitle(icnt) = index(title(icnt),'       ') - 1
         ICNT=ICNT+1
c         TITLE(ICNT)=MS2
c	 ntitle(icnt) = index(title(icnt),'       ') - 1

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

	open(98,file=plotgpi(1:nplotgpi),status='UNKNOWN',iostat=jj,err=995)
	if (epsplot) then
           open(97,file=plotgpi2(1:nplotgpi2),status='UNKNOWN',iostat=jj,err=996)
	endif

            call write_gpi_1(98,tbl,ntbl,ytitle,nytitle,
     1 xtitle,nxtitle,isize,plotwid,plotht,
     2 ploteps,nploteps)
	    if (epsplot) then
		call write_gpi_1(97,tbl,ntbl,ytitle,nytitle,
     1 xtitle,nxtitle,isize,plotwid,plotht,
     2 ploteps,nploteps)
	    endif
	do j=1,5
		ncolx(j) = j
		ncoly(j) = j + 5
c		print *,'j ncolx(j) ncoly(j) = ',j,ncolx(j),ncoly(j)
	enddo
	
       OPEN(12,FILE=tbl(1:ntbl),STATUS='UNKNOWN',IOSTAT=JST,ERR=999)

	do j=1,npts
	   write (12,10010) (LOG10(RSIGNAL(J,I)), i=1,5),(LOG10(RNOISE(J,I)), i=1,5) 
	enddo
10010   format (10e10.3,1x)
	close (12)
C
C	PLOTS LOOP	   
C        Generate plot for each reticle...
	maxrmean = -1.0e20
        minrmean = 1.0e20
        maxsigma = -1.0e20
	minsigma = 1.0e20
         DO 100 I=1,5
            IF (NG(I) .EQ. 0) GOTO 20
            DO J=1,NPTS
               RMEAN = RSIGNAL(J,I)
               IF (RMEAN .GT. 0.) RMEAN=LOG10(RMEAN)
               if (rmean .gt. maxrmean) maxrmean = rmean
	       if (rmean .lt. minrmean) minrmean = rmean
               X(J) = RMEAN
               SIGMA = RNOISE(J,I)
               IF (SIGMA .GT. 0.) SIGMA=LOG10(SIGMA)
	       if (sigma .gt. maxsigma) maxsigma = sigma
               if (sigma .lt. minsigma) minsigma = sigma
               Y(J) = SIGMA
            ENDDO
   20    CONTINUE
c         icnt=2
         MSG2(1:11) = MSG(I)(2:12)
         IF (I .EQ. 5) MSG2(13:19) = ' '
         WRITE (MSG2(23:29),'(F7.1)') REPDN(I)
         WRITE (MSG2(41:47),'(F7.3)') RRDN(I)
         WRITE (MSG2(59:61),'(I3)') NG(I)
         CALL XVMESSAGE(' ',' ')
         CALL XVMESSAGE(MSG2,' ')
         IF (NG(I) .EQ. 0) THEN
            CALL XVMESSAGE(' ',' ')
            CALL XVMESSAGE
     &         ('??E - No good areas in this part of frame',' ')
            GOTO 100
         ENDIF
         CALL AREADATA(EXPOS(1),RSIGNAL(1,I),RNOISE(1,I),NPTS,		!NPTS
     &          REPDN(I),RRDN(I))

         TITLE(ICNT)=MSG2
	if (i .eq. 5) then
	   ntitle(icnt) = 61
	else
           ntitle(icnt) = index(title(icnt),'       ') - 1
	endif
	yrang2 = maxsigma + 0.1
	yrang1 = minsigma - 0.1
	xrang2 = maxrmean + 0.1
	xrang1 = minrmean - 0.1
           call write_gpi_2(98,ltitle,nltitle,xrang1,xrang2,
     1 yrang1,yrang2,isize)
	   if (epsplot) then
           call write_gpi_2(97,ltitle,nltitle,xrang1,xrang2,
     1 yrang1,yrang2,isize)

	   endif

        call write_gpi_lab(98,icnt,title,ntitle,labstep,isize)
	if (epsplot) then
	   call write_gpi_lab(97,icnt,title,ntitle,labstep,isize)
        endif
ccc program power reads label
         DO II=1,6
            LMSG(II)=' '
         ENDDO
         LMSG(I+1)=MSG(I)

        call write_ls_gpi_1(98,tbl,ntbl,ncolx,ncoly,i,msg)
	if (epsplot) then
	   call write_ls_gpi_1(97,tbl,ntbl,ncolx,ncoly,i,msg)
        endif
100      continue
c	END PLOTS LOOP


	RETURN
c   error returns
995     call xvmessage('??E - nplot: Error opening/writing gnuplot file',' ')
        call abend
	return
996	call xvmessage('??E - nplot: Error opening/writing gnuplot eps file',' ')
        call abend
	return

999      CALL XVMESSAGE ('??E - ERROR OPENING PLOT data file',' ')
         CALL PRNT(4,1,JST,'IOSTAT=.')
         CALL ABEND
	RETURN

	END
C********************************************************************
	subroutine write_gpi_1(unit,tbl,ntbl,ylabel,nylabel,
     1 xlabel,nxlabel,isize,plotwid,plotht,
     2 eps,neps)
c            
c       write out the first part of gpi form
c
        implicit none
        integer*4 unit,isize,plotwid,plotht
        integer*4 jj,psize
        integer*4 ntbl,nylabel,nxlabel,neps
        character*16 ylabel,xlabel
        character*80 tbl,eps

c
	psize = isize
	if (unit .eq. 97) psize = 16
10100 format('# Created by program ccdnoise')              !#'s are ignored in gnuplot
            write(unit,fmt=10100,iostat=jj,err=995)
10105 format('# Gnuplot commands for log(signal)vs log(noise) plot')
            write(unit,fmt=10105,iostat=jj,err=995)
10110 format('# Data in ',a)
            write(unit,fmt=10110,iostat=jj,err=995) tbl(1:ntbl)
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
C**************************************************************************
       subroutine write_gpi_2(unit,title,ntitle,xrang1,xrang2,
     1 yrang1,yrang2,isize)
c	,mycol,nplots,iplot,fscale,nocol,nncol,
c     2 oldcol,newcol,nstart,npoint,ncontr,filename,nfilename)
c
c       write out the repeatable descriptive part of gpi form
c       for first, or only, plot
c
        implicit none
        integer*4 unit,isize,psize
        integer*4 jj
        integer*4 ntitle
        real*4 xrang1,xrang2,yrang1,yrang2
        character*80 title
c
        psize = isize
        if (unit .eq. 97) psize = 16

10145 format('set title "',a,'" font "Ariel,',i2,'"')
             write(unit,fmt=10145,iostat=jj,err=995) title(1:ntitle), psize
C
C---- SET THE SCALING PARAMETERS.
C
c       set y range to be like vicar image - lines counting downward from top
c	min = rang1      max =  rang2
10135 format("set yrange [",f10.2,":",f10.2,"]")
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
        subroutine write_gpi_lab(unit,nhdr,header,nheadr,labstep,isize)
c
c       write image labels to top of plot
c output labels for only top 60% of plot
c
c
        implicit none
        integer*4 unit,ii,jj,nhdr,isize,psize,nheadr(80)
        real*4  fpos,labstep
        character*86 header(80)
c
        psize = isize - 1
        if (unit .eq. 97) psize = 16

       write (unit,fmt=10165,iostat=jj,err=995)
10165 format ('unset label')

        fpos=1.0
        do ii=1,nhdr
                fpos = fpos - labstep
10160 format('set label ',i2,' "',a,'" at graph .25 ,',f5.2,
     1 ' font "ariel,',i2,'" front nopoint tc def')
        write(unit,fmt=10160,iostat=jj,err=995) ii,header(ii)(1:nheadr(ii)), fpos, psize
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
        subroutine write_ls_gpi_1(unit,tbl,ntbl,ncolx,ncoly,iplot,msg)
c
c       write out data in gpi form for pts, lines or linepoints
c       for a 1 value in control column
c
        implicit none
        integer*4 unit,ntbl
        integer*4 gcol,jj,iplot
        integer*4 ncolx(5),ncoly(5)
	CHARACTER*12 MSG(5)
        character*80 tbl
        character*100 outline

c       bring in gunplot colors, lines, points, etc
        include 'gnuplotchar'


        gcol = iplot
10253 format ("plot  '",a,"' u ",i2,":",i2," t '",a,"' w linespoints lt ",i2," pt ",i2,
     1 " ps 2 lw 2 lc rgb '",a,"'")
           write(unit,fmt=10253,iostat=jj,err=995) tbl(1:ntbl),ncolx(gcol),ncoly(gcol),msg(gcol),
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

C**************************************************************************
      SUBROUTINE LABPROC(IUNI,LABEL,NLAB)
c
         IMPLICIT NONE
 
         INTEGER INSTANCES(20),STAT,NLAB,IUNI,CNT,I,J

         CHARACTER*8 TASKS(20)
         CHARACTER*4320 LABEL
         CHARACTER*72 MSG
         CHARACTER*12 UNAME
         CHARACTER*28 TIME
         CHARACTER*65 HBUF
 
         hbuf='----TASK:------------USER:'
         MSG= ' '
         LABEL= ' '
         CALL VIC1LAB(IUNI,STAT,NLAB,LABEL,60)
         CNT=20                             !EXTRACTS VIC*2 LAB
         CALL XLHINFO(IUNI,TASKS,INSTANCES,CNT,STAT,' ')
         DO 801 J=1,CNT
            UNAME=' '
            TIME =' '
            CALL XLGET(IUNI,'HISTORY','USER',UNAME,STAT,'HIST',TASKS(J),
     &                 'INSTANCE',INSTANCES(J),'FORMAT','STRING',' ')
            CALL XLGET(IUNI,'HISTORY','DAT_TIM',TIME,STAT,'HIST',
     &                 TASKS(J),'INSTANCE',INSTANCES(J),'FORMAT',
     &                 'STRING',' ')
            DO 802 I=1,8                             !BLANKS NULLS
802            IF (TASKS(J)(I:I) .LT. '0') TASKS(J)(I:I)=' '
            HBUF(11:18)  = TASKS(J)(1:8)
            HBUF(28:39) = UNAME(1:12)
            HBUF(40:64) = TIME(1:25)
801      LABEL(1+(NLAB+J-1)*72:65+(NLAB+J-1)*72) =  HBUF(1:65)
         NLAB=NLAB+CNT
         DO 800 I=1,NLAB
            MSG = LABEL(1+(I-1)*72:1+(I-1)*72+71)
            CALL XVMESSAGE(MSG,' ')
800      MSG=' '
         RETURN
      END


$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create ccdnoise.imake
#define  PROGRAM   ccdnoise
#define R2LIB 

#define MODULE_LIST ccdnoise.f

#define FTNINC_LIST gnuplotchar

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create ccdnoise.pdf
PROCESS HELP=*
PARM INP     TYPE=STRING  COUNT=1
PARM OUT     TYPE=STRING  COUNT=(0:1)			DEFAULT=--
PARM LIMIT   TYPE=REAL    COUNT=(0:2)			DEFAULT=--
PARM EXTEXPO TYPE=INTEGER COUNT=(0:1) VALID=(2:30)	DEFAULT=--
PARM REJECT  TYPE=INTEGER COUNT=(0:1) VALID=(0:3)	DEFAULT=3
PARM PLOT    TYPE=STRING  COUNT=(0:1) 			DEFAULT=--
PARM PLOTFMT TYPE=STRING  COUNT=(0:1) VALID=(GNUPLOT,EPS) DEFAULT=GNUPLOT
PARM TABLE   TYPE=STRING  COUNT=(0:1) 			DEFAULT=--
PARM DBUG    TYPE=KEYWORD COUNT=(0:1) VALID=DBUG	DEFAULT=--
!PARM NODISP  TYPE=KEYWORD COUNT=(0:1) VALID=NODISP   DEFAULT=--
END-PROC
.TITLE
VICAR2 Program CCDNOISE
.HELP
PURPOSE:

CCDNOISE determines the system gain constant (in electrons per DN) and
read-noise floor (DN) for a CCD camera system.  The program is one of
a series of programs designed to support radiometric calibration of flight
imaging systems.

.PAGE
EXECUTION:

	      CCDNOISE INP=LTF OUT=MARK PARMS

The input is a Light Transfer File (LTF) containing statistical data for
specified areas in the image for each exposure of a light transfer
sequence.  The LTF must have been previously initialized via LTGEN and
loaded with data via MOMGEN.

The output is an optional MARK-format tiepoint data set containing the
centers of all rejected areas (see VICAR program MARK).

.PAGE 
OPERATION:

CCDNOISE performs the following steps:

  1) Read data from the Light Transfer File.
  2) Compute system gain constant and read noise for each area.
  3) Compute mean value for system gain constant and read noise
     and flag all areas deviating by more than 2 sigma from the mean.
  4) Re-compute the mean value for system gain constant and read
     noise, ignoring all flagged values as specified by the REJECT
     parameter.

The mean signal for a frame is computed by subtracting the dark current
from the frame and computing the mean of the result.  If extended
exposure mode frames are present in the light transfer sequence (possible
for Galileo data), then the extended exposure dark current is subtracted 
from these frames.

If extended exposure mode frames exist, the EXTEXPO parameter must normally
be specified to indicate the exposure level at which the extended exposures
begin.

   Example:  EXTEXPO=7 specifies that the 7th exposure level (above the
	     dark current) begins the extended exposures.

However, light-transfer sequences consisting entirely of extend-exposure
frames should be input as if they were normal exposures, i.e. the extended-
exposure dark-current should be inserted in place of the normal dark-current
and the EXTEXPO parameter should not be used.

CCDNOISE prints out the following:

  1) System gain constant and read noise for each area specified.
  2) Summary of all areas flagged for bad system gain or read noise.
  3) If DBUG is specified, a summary of the mean signal and noise (DN)
     at each exposure for each area specified.

If an output file is specified, then the centers of all flagged values
as specified by the REJECT parameter are stored in a MARK-format data
set.

If a PLOT file is specified, a signal vs. noise plot is generated for
each of the five reticles of the frame.  The reticles are (1) the upper-
left corner, (2) upper-right corner, (3) lower-left corner, (4) lower-right
corner, and (5) center of the frame.  These reticles divide the frame into
five regions.  Signal and noise data for the reticles are computed by 
averaging the good areas, where each area is assigned to the region of
the nearest reticle.  The plot file should be printed using the NOFEED
option (see example below).

A table of the values used to create each plot is also printed out.  This
table includes the ratio of measured vs. computed noise at each exposure.
The noise (DN) is computed as follows:

	NOISE = SQRT(S/K + RDN**2)

where S = signal (DN)
      K = system gain constant (electrons per DN)
      RDN = read noise (DN)

This ratio provides a useful criteria for evaluating the quality of the
input data (ratio should be near 1 for all exposures). 

If a TABLE file is provided, the above plots will be output as columnar
data in a tab-delimitted ASCII file.  The columns are:

  exposure time in msec
  signal reticle 1, 
  noise ret. 1, 
  computed noise ret. 1,
  ....,  
  signal ret. 5, 
  noise ret. 5,
  computed noise ret. 5

The rows are for each exposure level.
.PAGE
PLOT OUTPUTS

    The other type of output come from the PLOT and PLOTFMT parameters.
PLOT allows the user to display data from 5 areas on the CCD on an x,y 
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

    DEVELOPER Notes:
   
  1.  This program used to link to the XRT plot library -lxrt. Calls to
  that library were mitigated through a Calcomp conversion library,
  xrtps located in the p2 subroutine library. With the conversion to
  gnuplot, neither of these packages are used.

  2.  The original xrt graph package had the X-axis data plotted wrong.
        The lowest X value was always 0. It gave a bend in the plot.
        The lowest X should have always had a value ne 0

.PAGE
EXAMPLE:

   CCDNOISE LTF MRK PLOT=NOISE.DAT TABLE=TAB.DAT
   MARK (PIC,MRK) OUT			!Scribe boxes around bad centers
   JDISP OUT				!Display the bad areas
   DCL PRINT/NOFEED NOISE.DAT		!Print the noise plot

.PAGE
ORIGINAL PROGRAMMER: Gary Yagi, circa 1982
COGNIZANT PROGRAMMER: Gary Yagi, April 88

REVISION HISTORY

 13 Jul 2013 R. J. Bambery  Adjusted eps format to more readable fonts
 08 Jul 2013 R. J. Bambery  Rename ascii output file 
 05 Jul 2013 R. J. Bambery  Fixed bug in scaling values for xrang yrang
 18 Feb 2013 R. J. Bambery  Fix plot labels, test script
 13 Feb 2013 R. J. Bambery  Documentation and test updates
 16 Nov 2012 R. J. Bambery  Linux 64-bit, Gnuplot
 22 Apr 97...T.Huang........Ported from VAX to Unix.
 14 Nov 94...C.C.Avis.......Added decent test file
  6 Jun 94...C.C.Avis.......Added tabular output
 20 Apr 88...G.M.Yagi.......Fix bug in EXTEXPO, LABPROC.
  3 Mar 88...G.M.Yagi.......Change PDF to treat all EXTEXPO call.
 01 Nov 87...G.M.Yagi.......Convert to new CPLT plotting routines.
 10 Dec 86...G.M.Yagi.......Changed plot to signal vs noise.
 20 JUL 86...G.M.YAGI.......Code and documentation clean-up.
 26 JAN 85...M.E.MORRILL....VERSION 2*A RELEASED.
 22 JAN 85...M.E.MORRILL....ADDED PLOT OUTPUT FOR RATIO SHOT/THEORY. 
 15 JAN 85...M.E.MORRILL....ADDED SECOND PASS TO REJECT BAD AREAS
  5 OCT 84...M.E.MORRILL....VICAR*2 CONVERSION.
        82...G.M.YAGI.......INITIAL RELEASE.

.LEVEL1
.VARIABLE INP
 STRING COUNT=1
 The Light Transfer File.
.VARIABLE OUT
 STRING--OPTIONAL
 Mark-format file
 containing centers
 of rejected areas.
.VARIABLE PLOT
 STRING--OPTIONAL
 Output plot file.
.VARIABLE PLOTFMT
 Output plot format
 GNUPLOT or EPS
.VARIABLE TABLE
 STRING--OPTIONAL
 Output table file.
.VARIABLE DBUG
 KEYWORD--OPTIONAL.
 Diagnostic printout.
.VARIABLE REJECT
 INTEGER--OPTIONAL
 REJECT=0 No area rejection
       =1 Reject bad system gain
       =2 Reject bad noise floor
       =3 Reject either
.VARIABLE LIMIT
 REAL COUNT=0:2--OPTIONAL
 LIMIT=(loexp,hiexp)
 Only exposures between
 these values are used.
.VARIABLE EXTEXPO
 INTEGER--OPTIONAL--For Galileo only
 Specifies exposure level
 at which extended exposure
 mode frames begins.
.VARIABLE NODISP
 If present, no display
 is shown
.LEVEL2
.VARIABLE INP
 STRING COUNT=1
 The Light Transfer File created by LTGEN and MOMGEN containing
 area statistics for calculating the system gain constant K and
 the noise floor.
.VARIABLE OUT
 STRING--OPTIONAL
 Mark-format data set with (line,sample) centers of rejected areas
 (see REJECT parameter).
.VARIABLE PLOT
 STRING--OPTIONAL
 Output signal vs. noise plot file
 Print with PRINT/NOFEED option.
 E.g.   DCL PRINT/NOFEED CCDNOISE.PLT
.VARIABLE TABLE
 STRING--OPTIONAL
 Output tab-delimitted ASCII table file containing, for each exposure
 level:
  exposure time in msec
  signal at reticle 1, 
  noise at reticle 1, 
  computed at noise reticle 1,
  ....,  
  signal at reticle 5, 
  noise at reticle 5,
  computed at noise reticle 5

 Reticle 1 is Upper left corner, 2 is upper right, 3 is lower left,
 4 is lower right, and 5 is the center of the image.
.VARIABLE LIMIT
 REAL COUNT=0:2--OPTIONAL
 LIMIT=(loexp,hiexp)
 Only data in the Light Transfer File with exposures (in msec) between 
 these values are used for the calculations.
.VARIABLE EXTEXPO
 INTEGER--OPTIONAL--For Galileo only
 Specifies exposure level number (1 thru number of inputs) at which 
 extended exposure mode frames begins.
.VARIABLE REJECT
 INTEGER--OPTIONAL
 Reject areas with values differing from mean by more than 2-sigma.
 REJECT=0 No area rejection
       =1 Reject areas with bad system gain
       =2 Reject areas with bad noise floor
       =3 Reject areas with either term bad
.VARIABLE DBUG
 KEYWORD--OPTIONAL.
 Specifies diagnostic printout.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstccdnoise.pdf
procedure               !TSTCCDNOISE
parm    mode    type=keyword count=(0:1) valid=(batch,nobatch,inter) default=inter
! Feb 18, 2013 - RJB
! TEST SCRIPT FOR TSTCCDNOISE
! tests HALF images
!
! Vicar Programs:
!       hist flot ltgen momgen
!
! External programs
!       gnuplot 4.6.x 
!       gimp or ghostview for eps file
!
! Parameters:
!   mode - method for processing: 
!       1) batch provides no display
!       2) interactive or nobatch is used for display requiring
!       user interaction. 
!           
!   In batch mode it produces files testx.gpi by default, which can be
!       viewed by running gnuplot.  If plotfmt=eps is specified, then
!       it produces files testx.eps.gpi, which are converted to testx.eps
!       files by calling gnuplot.  The .eps files can be viewed with
!       ghostview or gimp.
!   In interactive or nobatch mode gnuplot is called with a window
!       manager for X11. The gnuplot display is killed by
!       a mouse click anywhere on the plot panel
!            
!
! Requires NO external test data: 
!
!  NOTE: The original xrt graph package had the X-axis data plotted wrong
!        The lowest X value was always 0. It gave a bend in the plot.
!        The lowest X should have always had a value ne 0
!
refgbl $autousage
refgbl $echo
refgbl $syschar
body
let $autousage="none"
let _onfail="stop"
let $echo="yes"

if ($syschar(1)="UNIX")
   defcmd-replace typeit "ush cat"
else
   defcmd-replace typeit "dcl type"
end-if
 
!=========================================================
!TRY TO MAKE NOISE IMAGES AT DN=0 SIGMA=0
!                            DN=100 SIGMA=SQRT(10)
!                            DN=1000 SIGMA=SQRT(100)
!THIS SHOULD YIELD SYS GAIN CONST K=10 AND READ NOISE=0
!
!GAUSNOIS APPARENTLY NEEDS THE MEAN PARAMETER TO BE .5 MORE THAN DESIRED
!gen d1.img nl=500 ns=500 ival=0 linc=0 sinc=0 'half
!hist d1.img 'nohist
!copy d1.img d2.img
gausnois d1.img mean=10.5 sigma=1.023  nl=500 ns=500 form=half seed=82651
hist d1.img 'nohist
flot d1.img d2.img 'coun


gausnois a1.img mean=100.5 sigma=3.1623 nl=500 ns=500 form=half seed=987654
hist a1.img 'nohist
flot a1.img a2.img 'coun
gausnois b1.img mean=1000.5 sigma=10 nl=500 ns=500 form=half seed=876543
hist b1.img 'nohist
flot b1.img b2.img 'coun
ltgen a1.img ltf1.dat 'grid ni=2 exp=(0,10,20)  !CREATE LTF
momgen (d1.img,d2.img) ltf1.dat  exp=0                    !LOAD LTF
momgen (a1.img,a2.img) ltf1.dat  exp=10                   !LOAD LTF
momgen (b1.img,b2.img) ltf1.dat  exp=20                   !LOAD LTF

! TEST 1 - NO Plot
! GAIN CONSTANT SHOULD = 10  READ NOISE SHOULD = 0
ccdnoise ltf1.dat table=gain1.tbl reject=0
typeit gain1.tbl

! TEST 2 - 5 Plots - No rejection Criteria - DEBUG
! SHOW ALL SUBAREAS
! GAIN CONSTANT SHOULD = 10  READ NOISE SHOULD = 0
ccdnoise ltf1.dat table=gain2.tbl plot=gain2 reject=0 'dbug
if (mode = "nobatch" or mode = "inter")
    ush gnuplot gain2.gpi
end-if
! TEST 3 - 5 Plots - ALLOW REJECTION OF BAD DATA
!  Reject bad system gain
ccdnoise ltf1.dat table=gain3.tbl plot=gain3 reject=1 
if (mode = "nobatch" or mode = "inter")
    ush gnuplot gain3.gpi
end-if

! TEST 4 - 5 Plots - ALLOW REJECTION OF BAD DATA
!  Reject bad noise floor
ccdnoise ltf1.dat table=gain4.tbl plot=gain4 reject=2
if (mode = "nobatch" or mode = "inter")
    ush gnuplot gain4.gpi
end-if

! TEST 5 - 5 Plots - ALLOW REJECTION OF BAD DATA
!  Reject either
ccdnoise ltf1.dat table=gain5.tbl plot=gain5 reject=3
if (mode = "nobatch" or mode = "inter")
    ush gnuplot gain5.gpi
end-if

! TEST 6 - 5 Plots - output a mark data set 
ccdnoise ltf1.dat out=mark.dat table=gain6.tbl plot=gain6  
if (mode = "nobatch" or mode = "inter")
    ush gnuplot gain6.gpi
end-if

! TEST 7 - 5 Plots - in encapsulated postscript format 
ccdnoise ltf1.dat  table=gain7.tbl plot=gain7 +
    plotfmt=eps
ush gnuplot gain7.eps.gpi
!if (mode = "nobatch" or mode = "inter")
!   ush gimp gain7.eps
! no 'gimp' on mipl
! on Solaris, ghostview can be used to display gain7.eps,
!   ush ghostview gain7.eps
! on Linux, there doesn't seem to be any tool to do this!
! just show that file was created:
   ush ls gain7.eps
!end-if

!No tests for LIMIT or EXTEXPO

! clean up
ush rm -f gain*.gpi
ush rm -f gain*.eps
ush rm -f gain*.asc
ush rm -f gain*.tbl
ush rm -f a?.img
ush rm -f b?.img
ush rm -f d?.img
ush rm -f mark.dat
ush rm -f ltf1.dat

let $echo="no"
end-proc
 

$!-----------------------------------------------------------------------------
$ create tstccdnoise.log_solos
tstccdnoise
if ($syschar(1)="UNIX")
   defcmd-replace typeit "ush cat"
else
end-if
gausnois d1.img mean=10.5 sigma=1.023  nl=500 ns=500 form=half seed=82651
Beginning VICAR task gausnois
hist d1.img 'nohist
Beginning VICAR task hist
*** HIST version 17 Dec 2012 ***


AVERAGE GRAY LEVEL=9.999316       STANDARD DEVIATION=1.063144       NUMBER ELEMENTS=    250000
MIN. DN=         6
MAX. DN=        14

flot d1.img d2.img 'coun
Beginning VICAR task flot
gausnois a1.img mean=100.5 sigma=3.1623 nl=500 ns=500 form=half seed=987654
Beginning VICAR task gausnois
hist a1.img 'nohist
Beginning VICAR task hist
*** HIST version 17 Dec 2012 ***


AVERAGE GRAY LEVEL=99.99788       STANDARD DEVIATION=3.176533       NUMBER ELEMENTS=    250000
MIN. DN=        87
MAX. DN=       112

flot a1.img a2.img 'coun
Beginning VICAR task flot
gausnois b1.img mean=1000.5 sigma=10 nl=500 ns=500 form=half seed=876543
Beginning VICAR task gausnois
hist b1.img 'nohist
Beginning VICAR task hist
*** HIST version 17 Dec 2012 ***


AVERAGE GRAY LEVEL=1000.000       STANDARD DEVIATION=10.00253       NUMBER ELEMENTS=    250000
MIN. DN=       959
MAX. DN=      1039

flot b1.img b2.img 'coun
Beginning VICAR task flot
ltgen a1.img ltf1.dat 'grid ni=2 exp=(0,10,20)
Beginning VICAR task ltgen
LTGEN Version 14-MAR-97
NUMBER OF AREAS     =    100
NUMBER OF EXPOSURES =      3
EXPOSURES = 
            0.000E+00  1.000E+01  2.000E+01
MAX FRAMES/LEVEL =           2
WRITING HALFWORD LIGHT TRANSFER FILE WITH
 NL (NREC) =           3
 NS        =         601
LTGEN task completed
momgen (d1.img,d2.img) ltf1.dat  exp=0
Beginning VICAR task momgen
MOMGEN Version 19-MAR-1997

EXPOSURE TIME=    0.000E+00
INPUT FRAMES=             2
NUMBER OF AREAS=        100
momgen (a1.img,a2.img) ltf1.dat  exp=10
Beginning VICAR task momgen
MOMGEN Version 19-MAR-1997

EXPOSURE TIME=    10.000000
INPUT FRAMES=             2
NUMBER OF AREAS=        100
momgen (b1.img,b2.img) ltf1.dat  exp=20
Beginning VICAR task momgen
MOMGEN Version 19-MAR-1997

EXPOSURE TIME=    20.000000
INPUT FRAMES=             2
NUMBER OF AREAS=        100
ccdnoise ltf1.dat table=gain1.tbl reject=0
Beginning VICAR task ccdnoise
CCDNOISE version 13-Jul-2013
----TASK:-GAUSNOIS---USER: lwk         Thu Oct 10 12:27:37 2013
----TASK:-LTGEN   ---USER: lwk         Thu Oct 10 12:27:39 2013
AREA   1 (SL,SS,NL,NS)=(  15,  15,  20,  20) K=    10.30742 E/DN RDN=     0.62024 DN
AREA   2 (SL,SS,NL,NS)=(  15,  65,  20,  20) K=     9.93566 E/DN RDN=     0.54951 DN
AREA   3 (SL,SS,NL,NS)=(  15, 115,  20,  20) K=     9.79330 E/DN RDN=    -0.87523 DN
AREA   4 (SL,SS,NL,NS)=(  15, 165,  20,  20) K=     9.83507 E/DN RDN=    -0.49816 DN
AREA   5 (SL,SS,NL,NS)=(  15, 215,  20,  20) K=     9.23651 E/DN RDN=    -0.73807 DN
AREA   6 (SL,SS,NL,NS)=(  15, 265,  20,  20) K=    10.10108 E/DN RDN=    -0.10256 DN
AREA   7 (SL,SS,NL,NS)=(  15, 315,  20,  20) K=     9.33507 E/DN RDN=    -0.80116 DN
AREA   8 (SL,SS,NL,NS)=(  15, 365,  20,  20) K=    10.44968 E/DN RDN=     0.74109 DN
AREA   9 (SL,SS,NL,NS)=(  15, 415,  20,  20) K=     9.63899 E/DN RDN=     0.69265 DN
AREA  10 (SL,SS,NL,NS)=(  15, 465,  20,  20) K=    10.40750 E/DN RDN=    -0.35611 DN
AREA  11 (SL,SS,NL,NS)=(  65,  15,  20,  20) K=     9.25803 E/DN RDN=    -0.32982 DN
AREA  12 (SL,SS,NL,NS)=(  65,  65,  20,  20) K=     9.80003 E/DN RDN=     0.32338 DN
AREA  13 (SL,SS,NL,NS)=(  65, 115,  20,  20) K=    10.14556 E/DN RDN=     0.69539 DN
AREA  14 (SL,SS,NL,NS)=(  65, 165,  20,  20) K=    10.95564 E/DN RDN=     0.74933 DN
AREA  15 (SL,SS,NL,NS)=(  65, 215,  20,  20) K=    11.66815 E/DN RDN=     1.11472 DN
AREA  16 (SL,SS,NL,NS)=(  65, 265,  20,  20) K=    10.27847 E/DN RDN=     0.30297 DN
AREA  17 (SL,SS,NL,NS)=(  65, 315,  20,  20) K=     9.89986 E/DN RDN=     0.47651 DN
AREA  18 (SL,SS,NL,NS)=(  65, 365,  20,  20) K=    10.60149 E/DN RDN=     0.88050 DN
AREA  19 (SL,SS,NL,NS)=(  65, 415,  20,  20) K=    10.34882 E/DN RDN=     0.73474 DN
AREA  20 (SL,SS,NL,NS)=(  65, 465,  20,  20) K=    10.58272 E/DN RDN=     0.45417 DN
AREA  21 (SL,SS,NL,NS)=( 115,  15,  20,  20) K=    11.42757 E/DN RDN=     0.85727 DN
AREA  22 (SL,SS,NL,NS)=( 115,  65,  20,  20) K=     9.65102 E/DN RDN=    -0.14995 DN
AREA  23 (SL,SS,NL,NS)=( 115, 115,  20,  20) K=     9.21765 E/DN RDN=    -0.55244 DN
AREA  24 (SL,SS,NL,NS)=( 115, 165,  20,  20) K=     9.42420 E/DN RDN=    -0.19428 DN
AREA  25 (SL,SS,NL,NS)=( 115, 215,  20,  20) K=     9.20208 E/DN RDN=    -0.33420 DN
AREA  26 (SL,SS,NL,NS)=( 115, 265,  20,  20) K=    10.07218 E/DN RDN=    -0.56563 DN
AREA  27 (SL,SS,NL,NS)=( 115, 315,  20,  20) K=     8.99965 E/DN RDN=    -0.63568 DN
AREA  28 (SL,SS,NL,NS)=( 115, 365,  20,  20) K=     9.33415 E/DN RDN=    -0.65101 DN
AREA  29 (SL,SS,NL,NS)=( 115, 415,  20,  20) K=     9.33027 E/DN RDN=    -0.31300 DN
AREA  30 (SL,SS,NL,NS)=( 115, 465,  20,  20) K=     9.37539 E/DN RDN=     0.28048 DN
AREA  31 (SL,SS,NL,NS)=( 165,  15,  20,  20) K=    10.18426 E/DN RDN=     0.45960 DN
AREA  32 (SL,SS,NL,NS)=( 165,  65,  20,  20) K=     9.80777 E/DN RDN=    -0.62880 DN
AREA  33 (SL,SS,NL,NS)=( 165, 115,  20,  20) K=     9.46085 E/DN RDN=    -0.41605 DN
AREA  34 (SL,SS,NL,NS)=( 165, 165,  20,  20) K=     9.94927 E/DN RDN=     0.56484 DN
AREA  35 (SL,SS,NL,NS)=( 165, 215,  20,  20) K=    10.05241 E/DN RDN=     0.18650 DN
AREA  36 (SL,SS,NL,NS)=( 165, 265,  20,  20) K=     9.38524 E/DN RDN=    -0.00774 DN
AREA  37 (SL,SS,NL,NS)=( 165, 315,  20,  20) K=    11.86728 E/DN RDN=     0.82450 DN
AREA  38 (SL,SS,NL,NS)=( 165, 365,  20,  20) K=     9.39617 E/DN RDN=     0.52621 DN
AREA  39 (SL,SS,NL,NS)=( 165, 415,  20,  20) K=     9.37466 E/DN RDN=     0.37411 DN
AREA  40 (SL,SS,NL,NS)=( 165, 465,  20,  20) K=    10.77020 E/DN RDN=     0.86076 DN
AREA  41 (SL,SS,NL,NS)=( 215,  15,  20,  20) K=     9.45240 E/DN RDN=    -0.35441 DN
AREA  42 (SL,SS,NL,NS)=( 215,  65,  20,  20) K=    10.50862 E/DN RDN=     0.77067 DN
AREA  43 (SL,SS,NL,NS)=( 215, 115,  20,  20) K=    10.40969 E/DN RDN=    -0.03484 DN
AREA  44 (SL,SS,NL,NS)=( 215, 165,  20,  20) K=     9.76946 E/DN RDN=     0.58446 DN
AREA  45 (SL,SS,NL,NS)=( 215, 215,  20,  20) K=    10.41095 E/DN RDN=     0.42936 DN
AREA  46 (SL,SS,NL,NS)=( 215, 265,  20,  20) K=    10.28357 E/DN RDN=     0.59148 DN
AREA  47 (SL,SS,NL,NS)=( 215, 315,  20,  20) K=     9.86580 E/DN RDN=    -0.08678 DN
AREA  48 (SL,SS,NL,NS)=( 215, 365,  20,  20) K=     9.24523 E/DN RDN=    -0.37095 DN
AREA  49 (SL,SS,NL,NS)=( 215, 415,  20,  20) K=    10.01096 E/DN RDN=     0.87833 DN
AREA  50 (SL,SS,NL,NS)=( 215, 465,  20,  20) K=     9.98657 E/DN RDN=    -0.32773 DN
AREA  51 (SL,SS,NL,NS)=( 265,  15,  20,  20) K=     9.09666 E/DN RDN=    -0.81045 DN
AREA  52 (SL,SS,NL,NS)=( 265,  65,  20,  20) K=     9.83588 E/DN RDN=    -0.13778 DN
AREA  53 (SL,SS,NL,NS)=( 265, 115,  20,  20) K=    10.40543 E/DN RDN=     0.46613 DN
AREA  54 (SL,SS,NL,NS)=( 265, 165,  20,  20) K=    10.68326 E/DN RDN=     0.46152 DN
AREA  55 (SL,SS,NL,NS)=( 265, 215,  20,  20) K=    10.39200 E/DN RDN=    -0.29464 DN
AREA  56 (SL,SS,NL,NS)=( 265, 265,  20,  20) K=    10.67752 E/DN RDN=     0.67597 DN
AREA  57 (SL,SS,NL,NS)=( 265, 315,  20,  20) K=    10.88648 E/DN RDN=     0.66295 DN
AREA  58 (SL,SS,NL,NS)=( 265, 365,  20,  20) K=     8.80985 E/DN RDN=    -0.38740 DN
AREA  59 (SL,SS,NL,NS)=( 265, 415,  20,  20) K=    10.24336 E/DN RDN=     0.00376 DN
AREA  60 (SL,SS,NL,NS)=( 265, 465,  20,  20) K=     9.74251 E/DN RDN=     0.37287 DN
AREA  61 (SL,SS,NL,NS)=( 315,  15,  20,  20) K=     9.92099 E/DN RDN=    -0.30064 DN
AREA  62 (SL,SS,NL,NS)=( 315,  65,  20,  20) K=    10.36872 E/DN RDN=     0.85959 DN
AREA  63 (SL,SS,NL,NS)=( 315, 115,  20,  20) K=     8.90701 E/DN RDN=    -0.78957 DN
AREA  64 (SL,SS,NL,NS)=( 315, 165,  20,  20) K=    10.20292 E/DN RDN=     0.70946 DN
AREA  65 (SL,SS,NL,NS)=( 315, 215,  20,  20) K=     9.11692 E/DN RDN=    -1.00175 DN
AREA  66 (SL,SS,NL,NS)=( 315, 265,  20,  20) K=     9.78322 E/DN RDN=    -0.53489 DN
AREA  67 (SL,SS,NL,NS)=( 315, 315,  20,  20) K=    10.11680 E/DN RDN=     0.15311 DN
AREA  68 (SL,SS,NL,NS)=( 315, 365,  20,  20) K=     9.57502 E/DN RDN=     0.47486 DN
AREA  69 (SL,SS,NL,NS)=( 315, 415,  20,  20) K=    11.36624 E/DN RDN=     0.44566 DN
AREA  70 (SL,SS,NL,NS)=( 315, 465,  20,  20) K=     8.72955 E/DN RDN=    -0.59912 DN
AREA  71 (SL,SS,NL,NS)=( 365,  15,  20,  20) K=     9.38370 E/DN RDN=    -0.78727 DN
AREA  72 (SL,SS,NL,NS)=( 365,  65,  20,  20) K=     9.71555 E/DN RDN=    -0.55521 DN
AREA  73 (SL,SS,NL,NS)=( 365, 115,  20,  20) K=    10.07417 E/DN RDN=    -0.07161 DN
AREA  74 (SL,SS,NL,NS)=( 365, 165,  20,  20) K=    10.18708 E/DN RDN=     0.60991 DN
AREA  75 (SL,SS,NL,NS)=( 365, 215,  20,  20) K=     9.51882 E/DN RDN=     0.41943 DN
AREA  76 (SL,SS,NL,NS)=( 365, 265,  20,  20) K=    10.09547 E/DN RDN=    -0.20599 DN
AREA  77 (SL,SS,NL,NS)=( 365, 315,  20,  20) K=     9.25589 E/DN RDN=    -0.33873 DN
AREA  78 (SL,SS,NL,NS)=( 365, 365,  20,  20) K=     9.28078 E/DN RDN=    -0.24020 DN
AREA  79 (SL,SS,NL,NS)=( 365, 415,  20,  20) K=    11.47499 E/DN RDN=     1.00377 DN
AREA  80 (SL,SS,NL,NS)=( 365, 465,  20,  20) K=    10.55617 E/DN RDN=     0.50581 DN
AREA  81 (SL,SS,NL,NS)=( 415,  15,  20,  20) K=     9.64604 E/DN RDN=     0.73388 DN
AREA  82 (SL,SS,NL,NS)=( 415,  65,  20,  20) K=     9.94912 E/DN RDN=    -0.13542 DN
AREA  83 (SL,SS,NL,NS)=( 415, 115,  20,  20) K=     9.64859 E/DN RDN=     0.49659 DN
AREA  84 (SL,SS,NL,NS)=( 415, 165,  20,  20) K=     9.99765 E/DN RDN=    -0.41223 DN
AREA  85 (SL,SS,NL,NS)=( 415, 215,  20,  20) K=     9.47067 E/DN RDN=     0.31530 DN
AREA  86 (SL,SS,NL,NS)=( 415, 265,  20,  20) K=     9.84201 E/DN RDN=     0.76710 DN
AREA  87 (SL,SS,NL,NS)=( 415, 315,  20,  20) K=     9.34843 E/DN RDN=     0.46536 DN
AREA  88 (SL,SS,NL,NS)=( 415, 365,  20,  20) K=     8.77626 E/DN RDN=    -0.61075 DN
AREA  89 (SL,SS,NL,NS)=( 415, 415,  20,  20) K=     9.72006 E/DN RDN=    -0.30946 DN
AREA  90 (SL,SS,NL,NS)=( 415, 465,  20,  20) K=     9.50211 E/DN RDN=     0.57434 DN
AREA  91 (SL,SS,NL,NS)=( 465,  15,  20,  20) K=    10.31689 E/DN RDN=     0.97256 DN
AREA  92 (SL,SS,NL,NS)=( 465,  65,  20,  20) K=     9.99379 E/DN RDN=     0.37400 DN
AREA  93 (SL,SS,NL,NS)=( 465, 115,  20,  20) K=     9.41280 E/DN RDN=    -0.16952 DN
AREA  94 (SL,SS,NL,NS)=( 465, 165,  20,  20) K=     9.66141 E/DN RDN=     0.42355 DN
AREA  95 (SL,SS,NL,NS)=( 465, 215,  20,  20) K=     9.40913 E/DN RDN=    -0.25293 DN
AREA  96 (SL,SS,NL,NS)=( 465, 265,  20,  20) K=    10.15835 E/DN RDN=    -0.09245 DN
AREA  97 (SL,SS,NL,NS)=( 465, 315,  20,  20) K=    10.31491 E/DN RDN=     0.38093 DN
AREA  98 (SL,SS,NL,NS)=( 465, 365,  20,  20) K=     9.78226 E/DN RDN=    -0.14110 DN
AREA  99 (SL,SS,NL,NS)=( 465, 415,  20,  20) K=    11.68719 E/DN RDN=     0.78994 DN
AREA 100 (SL,SS,NL,NS)=( 465, 465,  20,  20) K=    10.46812 E/DN RDN=    -0.03306 DN

Global value for K...
Raw mean and sigma are...
N= 100 MEAN=     9.94313 SIGMA=     0.64317
After throwing out samples differing by 2 sigma
N=  94 MEAN=     9.83853 SIGMA=     0.50575

Global noise floor...
Raw mean and sigma are...
N= 100 MEAN=     0.12105 SIGMA=     0.54112
After throwing out samples differing by 2 sigma
N=  99 MEAN=     0.13239 SIGMA=     0.53189

AREA  15 (SL,SS,NL,NS)=(  65, 215,  20,  20)  *****BAD SYSTEM GAIN*****
AREA  21 (SL,SS,NL,NS)=( 115,  15,  20,  20)  *****BAD SYSTEM GAIN*****
AREA  37 (SL,SS,NL,NS)=( 165, 315,  20,  20)  *****BAD SYSTEM GAIN*****
AREA  65 (SL,SS,NL,NS)=( 315, 215,  20,  20)  *****BAD NOISE FLOOR*****
AREA  69 (SL,SS,NL,NS)=( 315, 415,  20,  20)  *****BAD SYSTEM GAIN*****
AREA  79 (SL,SS,NL,NS)=( 365, 415,  20,  20)  *****BAD SYSTEM GAIN*****
AREA  99 (SL,SS,NL,NS)=( 465, 415,  20,  20)  *****BAD SYSTEM GAIN*****

No rejection criteria applied
NUMBER OF GOOD AREAS= 100 OUT OF 100 AREAS SAMPLED
K=     9.94313 E/DN RDN=     0.12105 DN

CCDNOISE task completed
ush cat gain1.tbl
ccdnoise ltf1.dat table=gain2.tbl plot=gain2 reject=0 'dbug
Beginning VICAR task ccdnoise
CCDNOISE version 13-Jul-2013
----TASK:-GAUSNOIS---USER: lwk         Thu Oct 10 12:27:37 2013
----TASK:-LTGEN   ---USER: lwk         Thu Oct 10 12:27:39 2013

AREA   1 (SL,SS,NL,NS)=(  15,  15,  20,  20) K=    10.30742 E/DN RDN=     0.62024 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9788     1.06400    0.99908    0.02694    1.16311    0.91479
     10.00000   100.0725     3.24360    2.00031    0.51103    3.17702    1.02096
     20.00000  1000.3175     9.86865    3.00014    0.99426    9.87081    0.99978

AREA   2 (SL,SS,NL,NS)=(  15,  65,  20,  20) K=     9.93566 E/DN RDN=     0.54951 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9700     1.08623    0.99870    0.03592    1.14255    0.95070
     10.00000   100.0025     3.27147    2.00001    0.51474    3.21978    1.01605
     20.00000  1000.1638    10.04652    3.00007    1.00202   10.04818    0.99983

AREA   3 (SL,SS,NL,NS)=(  15, 115,  20,  20) K=     9.79330 E/DN RDN=    -0.87523 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9438     1.04070    0.99755    0.01732    1.33468    0.77973
     10.00000   100.0488     2.89279    2.00021    0.46132    3.31392    0.87292
     20.00000   999.4000    10.06337    2.99974    1.00274   10.13979    0.99246

AREA   4 (SL,SS,NL,NS)=(  15, 165,  20,  20) K=     9.83507 E/DN RDN=    -0.49816 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9775     1.03177    0.99902    0.01358    1.12367    0.91821
     10.00000   100.0587     3.02026    2.00026    0.48004    3.22829    0.93556
     20.00000  1000.4500    10.06379    3.00020    1.00276   10.09807    0.99661

AREA   5 (SL,SS,NL,NS)=(  15, 215,  20,  20) K=     9.23651 E/DN RDN=    -0.73807 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000    10.0275     1.05261    1.00119    0.02227    1.27687    0.82437
     10.00000   100.0350     3.04503    2.00015    0.48359    3.37271    0.90284
     20.00000   999.8588    10.37279    2.99994    1.01590   10.43051    0.99447

AREA   6 (SL,SS,NL,NS)=(  15, 265,  20,  20) K=    10.10108 E/DN RDN=    -0.10256 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9750     1.08042    0.99891    0.03359    0.99902    1.08148
     10.00000    99.9837     3.11156    1.99993    0.49298    3.14783    0.98848
     20.00000   999.7587     9.94405    2.99990    0.99756    9.94917    0.99949

AREA   7 (SL,SS,NL,NS)=(  15, 315,  20,  20) K=     9.33507 E/DN RDN=    -0.80116 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9862     1.04660    0.99940    0.01978    1.30829    0.79998
     10.00000    99.9638     3.00285    1.99984    0.47753    3.36901    0.89131
     20.00000  1000.1287    10.31623    3.00006    1.01352   10.38164    0.99370

AREA   8 (SL,SS,NL,NS)=(  15, 365,  20,  20) K=    10.44968 E/DN RDN=     0.74109 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000    10.0750     1.01242    1.00324    0.00536    1.23018    0.82298
     10.00000    99.8400     3.27318    1.99930    0.51497    3.17861    1.02975
     20.00000   999.9525     9.80717    2.99998    0.99154    9.81027    0.99968

AREA   9 (SL,SS,NL,NS)=(  15, 415,  20,  20) K=     9.63899 E/DN RDN=     0.69265 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9600     1.02956    0.99826    0.01265    1.23007    0.83699
     10.00000    99.9075     3.37308    1.99960    0.52803    3.29313    1.02428
     20.00000  1000.2062    10.20751    3.00009    1.00892   10.21012    0.99974

AREA  10 (SL,SS,NL,NS)=(  15, 465,  20,  20) K=    10.40750 E/DN RDN=    -0.35611 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9812     1.07138    0.99918    0.02994    1.04204    1.02815
     10.00000   100.0850     2.97738    2.00037    0.47383    3.12145    0.95385
     20.00000  1000.0300     9.78626    3.00001    0.99062    9.80888    0.99769

AREA  11 (SL,SS,NL,NS)=(  65,  15,  20,  20) K=     9.25803 E/DN RDN=    -0.32982 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9312     1.03010    0.99700    0.01288    1.08697    0.94768
     10.00000   100.0287     3.17935    2.00012    0.50234    3.30353    0.96241
     20.00000  1000.1350    10.37959    3.00006    1.01618   10.39893    0.99814

AREA  12 (SL,SS,NL,NS)=(  65,  65,  20,  20) K=     9.80003 E/DN RDN=     0.32338 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000    10.0013     1.05404    1.00005    0.02286    1.06071    0.99371
     10.00000    99.9650     3.22820    1.99985    0.50896    3.21015    1.00562
     20.00000   999.9775    10.10600    2.99999    1.00458   10.10657    0.99994

AREA  13 (SL,SS,NL,NS)=(  65, 115,  20,  20) K=    10.14556 E/DN RDN=     0.69539 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9900     1.02820    0.99957    0.01208    1.21171    0.84855
     10.00000    99.8962     3.29653    1.99955    0.51806    3.21401    1.02567
     20.00000   999.9113     9.94919    2.99996    0.99779    9.95189    0.99973

AREA  14 (SL,SS,NL,NS)=(  65, 165,  20,  20) K=    10.95564 E/DN RDN=     0.74933 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9613     1.11422    0.99831    0.04697    1.21274    0.91877
     10.00000    99.9837     3.21117    1.99993    0.50666    3.11251    1.03170
     20.00000  1000.1775     9.58084    3.00008    0.98140    9.58409    0.99966

AREA  15 (SL,SS,NL,NS)=(  65, 215,  20,  20) K=    11.66815 E/DN RDN=     1.11472 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9925     1.06766    0.99967    0.02843    1.44879    0.73693
     10.00000   100.1250     3.34732    2.00054    0.52470    3.13427    1.06797
     20.00000   999.7000     9.31568    2.99987    0.96921    9.32310    0.99920

AREA  16 (SL,SS,NL,NS)=(  65, 265,  20,  20) K=    10.27847 E/DN RDN=     0.30297 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000    10.0413     1.16669    1.00179    0.06696    1.03378    1.12856
     10.00000    99.9650     3.14952    1.99985    0.49824    3.13328    1.00518
     20.00000   999.7562     9.86654    2.99989    0.99417    9.86706    0.99995

AREA  17 (SL,SS,NL,NS)=(  65, 315,  20,  20) K=     9.89986 E/DN RDN=     0.47651 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000    10.0200     1.09535    1.00087    0.03955    1.11319    0.98398
     10.00000    99.9938     3.25267    1.99997    0.51224    3.21366    1.01214
     20.00000   999.8488    10.05972    2.99993    1.00259   10.06098    0.99988

AREA  18 (SL,SS,NL,NS)=(  65, 365,  20,  20) K=    10.60149 E/DN RDN=     0.88050 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000    10.0025     1.00285    1.00011    0.00123    1.31102    0.76493
     10.00000    99.9187     3.32590    1.99965    0.52191    3.19378    1.04137
     20.00000   999.8312     9.74677    2.99993    0.98886    9.75119    0.99955

AREA  19 (SL,SS,NL,NS)=(  65, 415,  20,  20) K=    10.34882 E/DN RDN=     0.73474 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9800     1.06719    0.99913    0.02824    1.22646    0.87014
     10.00000    99.7925     3.28365    1.99910    0.51636    3.19104    1.02902
     20.00000  1000.4163     9.85645    3.00018    0.99372    9.85948    0.99969

AREA  20 (SL,SS,NL,NS)=(  65, 465,  20,  20) K=    10.58272 E/DN RDN=     0.45417 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9425     1.02917    0.99750    0.01249    1.07041    0.96148
     10.00000   100.0888     3.14535    2.00039    0.49767    3.10870    1.01179
     20.00000   999.7863     9.72917    2.99991    0.98808    9.73035    0.99988

AREA  21 (SL,SS,NL,NS)=( 115,  15,  20,  20) K=    11.42757 E/DN RDN=     0.85727 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000    10.0763     1.02152    1.00330    0.00925    1.27148    0.80341
     10.00000    99.9887     3.20956    1.99995    0.50644    3.07972    1.04216
     20.00000  1000.2175     9.39042    3.00009    0.97269    9.39477    0.99954

AREA  22 (SL,SS,NL,NS)=( 115,  65,  20,  20) K=     9.65102 E/DN RDN=    -0.14995 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000    10.0025     1.07473    1.00011    0.03130    1.02903    1.04441
     10.00000   100.2363     3.17324    2.00102    0.50150    3.22623    0.98358
     20.00000   999.9363    10.17232    2.99997    1.00742   10.17997    0.99925

AREA  23 (SL,SS,NL,NS)=( 115, 115,  20,  20) K=     9.21765 E/DN RDN=    -0.55244 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9950     1.02408    0.99978    0.01034    1.17878    0.86877
     10.00000   100.2412     3.11585    2.00105    0.49358    3.34367    0.93186
     20.00000  1000.0175    10.39223    3.00001    1.01671   10.43046    0.99633

AREA  24 (SL,SS,NL,NS)=( 115, 165,  20,  20) K=     9.42420 E/DN RDN=    -0.19428 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9900     1.07317    0.99957    0.03067    1.04775    1.02426
     10.00000   100.0675     3.19499    2.00029    0.50447    3.26434    0.97876
     20.00000  1000.2125    10.29367    3.00009    1.01257   10.30389    0.99901

AREA  25 (SL,SS,NL,NS)=( 115, 215,  20,  20) K=     9.20208 E/DN RDN=    -0.33420 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000    10.0100     1.10788    1.00043    0.04449    1.09521    1.01156
     10.00000    99.9638     3.18710    1.99984    0.50340    3.31283    0.96205
     20.00000   999.4562    10.40745    2.99976    1.01734   10.42706    0.99812

AREA  26 (SL,SS,NL,NS)=( 115, 265,  20,  20) K=    10.07218 E/DN RDN=    -0.56563 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9775     1.01779    0.99902    0.00766    1.14479    0.88906
     10.00000    99.9475     2.95450    1.99977    0.47048    3.20048    0.92314
     20.00000  1000.0325     9.93900    3.00001    0.99734    9.98031    0.99586

AREA  27 (SL,SS,NL,NS)=( 115, 315,  20,  20) K=     8.99965 E/DN RDN=    -0.63568 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9600     1.03923    0.99826    0.01671    1.22915    0.84549
     10.00000   100.0612     3.12671    2.00027    0.49509    3.39447    0.92112
     20.00000  1000.3837    10.51632    3.00017    1.02186   10.56230    0.99565

AREA  28 (SL,SS,NL,NS)=( 115, 365,  20,  20) K=     9.33415 E/DN RDN=    -0.65101 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9637     1.05628    0.99842    0.02378    1.22118    0.86497
     10.00000   100.2438     3.06022    2.00106    0.48575    3.34115    0.91592
     20.00000  1000.2288    10.32373    3.00010    1.01384   10.37216    0.99533

AREA  29 (SL,SS,NL,NS)=( 115, 415,  20,  20) K=     9.33027 E/DN RDN=    -0.31300 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000    10.0150     1.12103    1.00065    0.04962    1.08229    1.03579
     10.00000   100.0863     3.17273    2.00037    0.50143    3.29014    0.96432
     20.00000   999.7850    10.33812    2.99991    1.01444   10.35630    0.99824

AREA  30 (SL,SS,NL,NS)=( 115, 465,  20,  20) K=     9.37539 E/DN RDN=     0.28048 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9738     1.08448    0.99886    0.03522    1.06887    1.01460
     10.00000   100.0537     3.29212    2.00023    0.51748    3.27881    1.00406
     20.00000   999.5300    10.32870    2.99980    1.01405   10.32912    0.99996

AREA  31 (SL,SS,NL,NS)=( 165,  15,  20,  20) K=    10.18426 E/DN RDN=     0.45960 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9875     1.13815    0.99946    0.05620    1.09175    1.04250
     10.00000    99.8363     3.20139    1.99929    0.50534    3.16453    1.01165
     20.00000  1000.4625     9.92089    3.00020    0.99655    9.92207    0.99988

AREA  32 (SL,SS,NL,NS)=( 165,  65,  20,  20) K=     9.80777 E/DN RDN=    -0.62880 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000    10.0138     1.08662    1.00060    0.03608    1.19012    0.91303
     10.00000    99.9912     2.97786    1.99996    0.47390    3.25430    0.91505
     20.00000   999.6387    10.06798    2.99984    1.00294   10.11526    0.99533

AREA  33 (SL,SS,NL,NS)=( 165, 115,  20,  20) K=     9.46085 E/DN RDN=    -0.41605 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9700     0.98351    0.99870   -0.00722    1.10766    0.88792
     10.00000   100.0175     3.11342    2.00008    0.49324    3.27793    0.94981
     20.00000  1000.1337    10.26368    3.00006    1.01130   10.29009    0.99743

AREA  34 (SL,SS,NL,NS)=( 165, 165,  20,  20) K=     9.94927 E/DN RDN=     0.56484 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9838     1.06327    0.99929    0.02664    1.15001    0.92458
     10.00000    99.8913     3.27316    1.99953    0.51497    3.21856    1.01697
     20.00000   999.2900    10.03604    2.99969    1.00156   10.03781    0.99982

AREA  35 (SL,SS,NL,NS)=( 165, 215,  20,  20) K=    10.05241 E/DN RDN=     0.18650 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000    10.0650     1.07898    1.00281    0.03301    1.01786    1.06005
     10.00000    99.9013     3.16409    1.99957    0.50025    3.15798    1.00194
     20.00000   999.7313     9.97411    2.99988    0.99887    9.97430    0.99998

AREA  36 (SL,SS,NL,NS)=( 165, 265,  20,  20) K=     9.38524 E/DN RDN=    -0.00774 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000    10.0275     1.06017    1.00119    0.02537    1.03368    1.02562
     10.00000   100.0950     3.26325    2.00041    0.51365    3.26576    0.99923
     20.00000   999.7238    10.32056    2.99988    1.01370   10.32090    0.99997

AREA  37 (SL,SS,NL,NS)=( 165, 315,  20,  20) K=    11.86728 E/DN RDN=     0.82450 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000    10.0075     1.09542    1.00033    0.03958    1.23413    0.88761
     10.00000   100.0013     3.14035    2.00001    0.49698    3.01769    1.04065
     20.00000   999.5262     9.21030    2.99979    0.96427    9.21440    0.99955

AREA  38 (SL,SS,NL,NS)=( 165, 365,  20,  20) K=     9.39617 E/DN RDN=     0.52621 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000    10.0288     1.02541    1.00125    0.01090    1.15941    0.88443
     10.00000   100.1150     3.35253    2.00050    0.52537    3.30632    1.01398
     20.00000   999.8488    10.32746    2.99993    1.01399   10.32895    0.99986

AREA  39 (SL,SS,NL,NS)=( 165, 415,  20,  20) K=     9.37466 E/DN RDN=     0.37411 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9738     1.14504    0.99886    0.05882    1.09721    1.04360
     10.00000   100.0863     3.31236    2.00037    0.52014    3.28880    1.00716
     20.00000  1000.7087    10.33782    3.00031    1.01443   10.33857    0.99993

AREA  40 (SL,SS,NL,NS)=( 165, 465,  20,  20) K=    10.77020 E/DN RDN=     0.86076 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000    10.0025     1.04642    1.00011    0.01971    1.29214    0.80984
     10.00000   100.1225     3.29553    2.00053    0.51792    3.16815    1.04021
     20.00000  1000.1175     9.67048    3.00005    0.98545    9.67474    0.99956

AREA  41 (SL,SS,NL,NS)=( 215,  15,  20,  20) K=     9.45240 E/DN RDN=    -0.35441 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9700     1.08775    0.99870    0.03653    1.08645    1.00120
     10.00000    99.8112     3.13229    1.99918    0.49586    3.26879    0.95824
     20.00000   999.9888    10.27020    3.00000    1.01158   10.29163    0.99792

AREA  42 (SL,SS,NL,NS)=( 215,  65,  20,  20) K=    10.50862 E/DN RDN=     0.77067 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000    10.0425     1.14825    1.00184    0.06004    1.24482    0.92243
     10.00000    99.8637     3.27976    1.99941    0.51584    3.17757    1.03216
     20.00000   999.8600     9.78134    2.99994    0.99040    9.78471    0.99966

AREA  43 (SL,SS,NL,NS)=( 215, 115,  20,  20) K=    10.40969 E/DN RDN=    -0.03484 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000    10.0112     1.04899    1.00049    0.02077    0.98129    1.06898
     10.00000   100.1363     3.08966    2.00059    0.48991    3.10173    0.99611
     20.00000  1000.2988     9.80113    3.00013    0.99128    9.80277    0.99983

AREA  44 (SL,SS,NL,NS)=( 215, 165,  20,  20) K=     9.76946 E/DN RDN=     0.58446 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000    10.0475     1.07703    1.00206    0.03223    1.17049    0.92015
     10.00000    99.9413     3.30924    1.99974    0.51973    3.25139    1.01779
     20.00000   999.5975    10.13026    2.99983    1.00562   10.13213    0.99982

AREA  45 (SL,SS,NL,NS)=( 215, 215,  20,  20) K=    10.41095 E/DN RDN=     0.42936 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000    10.0175     1.02927    1.00076    0.01253    1.07078    0.96123
     10.00000    99.9613     3.16081    1.99983    0.49980    3.12824    1.01041
     20.00000  1000.1588     9.80978    3.00007    0.99166    9.81082    0.99989

AREA  46 (SL,SS,NL,NS)=( 215, 265,  20,  20) K=    10.28357 E/DN RDN=     0.59148 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9875     1.09036    0.99946    0.03757    1.14937    0.94866
     10.00000   100.1250     3.23652    2.00054    0.51008    3.17589    1.01909
     20.00000   999.6150     9.87502    2.99983    0.99454    9.87699    0.99980

AREA  47 (SL,SS,NL,NS)=( 215, 315,  20,  20) K=     9.86580 E/DN RDN=    -0.08678 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000    10.0450     1.08706    1.00195    0.03625    1.01277    1.07336
     10.00000    99.9313     3.15370    1.99970    0.49882    3.18380    0.99055
     20.00000   999.5750    10.06181    2.99982    1.00268   10.06602    0.99958

AREA  48 (SL,SS,NL,NS)=( 215, 365,  20,  20) K=     9.24523 E/DN RDN=    -0.37095 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000    10.0138     1.06583    1.00060    0.02769    1.10486    0.96467
     10.00000   100.0625     3.16860    2.00027    0.50087    3.31070    0.95708
     20.00000   999.7725    10.38314    2.99990    1.01633   10.40562    0.99784

AREA  49 (SL,SS,NL,NS)=( 215, 415,  20,  20) K=    10.01096 E/DN RDN=     0.87833 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9812     1.08207    0.99918    0.03425    1.32985    0.81368
     10.00000    99.8725     3.40659    1.99945    0.53232    3.27838    1.03911
     20.00000   999.3438    10.02551    2.99971    1.00111   10.02978    0.99957

AREA  50 (SL,SS,NL,NS)=( 215, 465,  20,  20) K=     9.98657 E/DN RDN=    -0.32773 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000    10.0437     1.02585    1.00190    0.01108    1.05505    0.97232
     10.00000   100.0713     3.05429    2.00031    0.48491    3.18245    0.95973
     20.00000  1000.0862     9.99259    3.00004    0.99968   10.01252    0.99801

AREA  51 (SL,SS,NL,NS)=( 265,  15,  20,  20) K=     9.09666 E/DN RDN=    -0.81045 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9637     1.12650    0.99842    0.05173    1.32369    0.85103
     10.00000    99.8463     3.04390    1.99933    0.48343    3.41071    0.89245
     20.00000  1000.0137    10.45042    3.00001    1.01913   10.51612    0.99375

AREA  52 (SL,SS,NL,NS)=( 265,  65,  20,  20) K=     9.83588 E/DN RDN=    -0.13778 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9975     1.07119    0.99989    0.02987    1.01755    1.05271
     10.00000    99.9725     3.14216    1.99988    0.49723    3.19109    0.98467
     20.00000  1000.0725    10.07738    3.00003    1.00335   10.08439    0.99930

AREA  53 (SL,SS,NL,NS)=( 265, 115,  20,  20) K=    10.40543 E/DN RDN=     0.46613 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000    10.0125     1.02176    1.00054    0.00935    1.08605    0.94080
     10.00000    99.8913     3.17153    1.99953    0.50127    3.13324    1.01222
     20.00000   999.3925     9.81012    2.99974    0.99167    9.81135    0.99987

AREA  54 (SL,SS,NL,NS)=( 265, 165,  20,  20) K=    10.68326 E/DN RDN=     0.46152 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9663     1.07702    0.99853    0.03222    1.07046    1.00613
     10.00000    99.8737     3.13022    1.99945    0.49557    3.09219    1.01230
     20.00000   999.9975     9.68470    3.00000    0.98609    9.68592    0.99987

AREA  55 (SL,SS,NL,NS)=( 265, 215,  20,  20) K=    10.39200 E/DN RDN=    -0.29464 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000    10.0250     1.06301    1.00108    0.02654    1.02542    1.03666
     10.00000    99.9625     2.99953    1.99984    0.47705    3.11544    0.96279
     20.00000  1000.2238     9.79732    3.00010    0.99111    9.81510    0.99819

AREA  56 (SL,SS,NL,NS)=( 265, 265,  20,  20) K=    10.67752 E/DN RDN=     0.67597 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9712     1.03676    0.99875    0.01568    1.17932    0.87912
     10.00000   100.0675     3.21504    2.00029    0.50719    3.13508    1.02551
     20.00000   999.8200     9.69763    2.99992    0.98667    9.70025    0.99973

AREA  57 (SL,SS,NL,NS)=( 265, 315,  20,  20) K=    10.88648 E/DN RDN=     0.66295 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000    10.0100     1.09653    1.00043    0.04002    1.16576    0.94062
     10.00000   100.1113     3.18180    2.00048    0.50267    3.10410    1.02503
     20.00000   999.4662     9.60201    2.99977    0.98236    9.60456    0.99973

AREA  58 (SL,SS,NL,NS)=( 265, 365,  20,  20) K=     8.80985 E/DN RDN=    -0.38740 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000    10.0413     0.98158    1.00179   -0.00807    1.13572    0.86428
     10.00000    99.9112     3.24392    1.99961    0.51107    3.38983    0.95696
     20.00000  1000.0450    10.63814    3.00002    1.02687   10.66135    0.99782

AREA  59 (SL,SS,NL,NS)=( 265, 415,  20,  20) K=    10.24336 E/DN RDN=     0.00376 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9712     1.13813    0.99875    0.05619    0.98664    1.15355
     10.00000    99.8012     3.12139    1.99914    0.49435    3.12138    1.00000
     20.00000  1000.5150     9.88304    3.00022    0.99489    9.88304    1.00000

AREA  60 (SL,SS,NL,NS)=( 265, 465,  20,  20) K=     9.74251 E/DN RDN=     0.37287 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000    10.0200     0.98050    1.00087   -0.00855    1.08052    0.90744
     10.00000   100.2400     3.25308    2.00104    0.51229    3.22924    1.00738
     20.00000   999.3287    10.13398    2.99971    1.00578   10.13475    0.99992

AREA  61 (SL,SS,NL,NS)=( 315,  15,  20,  20) K=     9.92099 E/DN RDN=    -0.30064 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9663     1.05645    0.99853    0.02385    1.04640    1.00961
     10.00000    99.9600     3.07261    1.99983    0.48751    3.18842    0.96368
     20.00000  1000.2188    10.02752    3.00009    1.00119   10.04534    0.99823

AREA  62 (SL,SS,NL,NS)=( 315,  65,  20,  20) K=    10.36872 E/DN RDN=     0.85959 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000    10.0138     1.08480    1.00060    0.03535    1.30563    0.83087
     10.00000    99.9238     3.34617    1.99967    0.52455    3.22117    1.03881
     20.00000  1000.0125     9.85403    3.00001    0.99361    9.85819    0.99958

AREA  63 (SL,SS,NL,NS)=( 315, 115,  20,  20) K=     8.90701 E/DN RDN=    -0.78957 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000    10.0425     1.07046    1.00184    0.02957    1.32321    0.80899
     10.00000   100.1975     3.09551    2.00086    0.49073    3.44568    0.89838
     20.00000   999.6038    10.56054    2.99983    1.02369   10.62309    0.99411

AREA  64 (SL,SS,NL,NS)=( 315, 165,  20,  20) K=    10.20292 E/DN RDN=     0.70946 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9775     1.11512    0.99902    0.04732    1.21706    0.91624
     10.00000    99.8850     3.29430    1.99950    0.51776    3.20830    1.02680
     20.00000   999.8387     9.92184    2.99993    0.99659    9.92465    0.99972

AREA  65 (SL,SS,NL,NS)=( 315, 215,  20,  20) K=     9.11692 E/DN RDN=    -1.00175 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000    10.0350     1.14619    1.00152    0.05926    1.45059    0.79016
     10.00000   100.0300     2.97607    2.00013    0.47364    3.46055    0.86000
     20.00000   999.3762    10.42724    2.99973    1.01817   10.51766    0.99140

AREA  66 (SL,SS,NL,NS)=( 315, 265,  20,  20) K=     9.78322 E/DN RDN=    -0.53489 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9937     1.06386    0.99973    0.02688    1.14351    0.93034
     10.00000    99.8138     3.01222    1.99919    0.47889    3.23862    0.93009
     20.00000  1000.1562    10.08743    3.00007    1.00378   10.12511    0.99628

AREA  67 (SL,SS,NL,NS)=( 315, 315,  20,  20) K=    10.11680 E/DN RDN=     0.15311 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000    10.0387     1.14288    1.00168    0.05800    1.00783    1.13400
     10.00000    99.9962     3.15177    1.99998    0.49855    3.14764    1.00131
     20.00000  1000.3162     9.94473    3.00014    0.99759    9.94486    0.99999

AREA  68 (SL,SS,NL,NS)=( 315, 365,  20,  20) K=     9.57502 E/DN RDN=     0.47486 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9812     1.12854    0.99918    0.05252    1.12602    1.00224
     10.00000   100.0675     3.30559    2.00029    0.51925    3.26747    1.01167
     20.00000  1000.0687    10.22966    3.00003    1.00986   10.23089    0.99988

AREA  69 (SL,SS,NL,NS)=( 315, 415,  20,  20) K=    11.36624 E/DN RDN=     0.44566 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9762     1.07413    0.99897    0.03106    1.03746    1.03535
     10.00000    99.8962     3.03449    1.99955    0.48209    2.99791    1.01220
     20.00000   999.7338     9.38792    2.99988    0.97257    9.38909    0.99987

AREA  70 (SL,SS,NL,NS)=( 315, 465,  20,  20) K=     8.72955 E/DN RDN=    -0.59912 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000    10.0100     1.07726    1.00043    0.03232    1.22704    0.87793
     10.00000    99.8163     3.18898    1.99920    0.50365    3.43413    0.92861
     20.00000   999.7475    10.67669    2.99989    1.02844   10.71837    0.99611

AREA  71 (SL,SS,NL,NS)=( 365,  15,  20,  20) K=     9.38370 E/DN RDN=    -0.78727 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9950     1.06522    0.99978    0.02744    1.29806    0.82063
     10.00000   100.0038     2.99919    2.00002    0.47700    3.35812    0.89311
     20.00000   999.6663    10.28749    2.99986    1.01231   10.35143    0.99382

AREA  72 (SL,SS,NL,NS)=( 365,  65,  20,  20) K=     9.71555 E/DN RDN=    -0.55521 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9350     1.01356    0.99717    0.00585    1.15362    0.87859
     10.00000    99.7425     3.01569    1.99888    0.47939    3.25185    0.92738
     20.00000  1000.8088    10.12508    3.00035    1.00540   10.16461    0.99611

AREA  73 (SL,SS,NL,NS)=( 365, 115,  20,  20) K=    10.07417 E/DN RDN=    -0.07161 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9663     1.04211    0.99853    0.01791    0.99720    1.04503
     10.00000   100.0775     3.12776    2.00034    0.49523    3.15265    0.99211
     20.00000  1000.7800     9.96381    3.00034    0.99843    9.96726    0.99965

AREA  74 (SL,SS,NL,NS)=( 365, 165,  20,  20) K=    10.18708 E/DN RDN=     0.60991 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9475     1.01590    0.99771    0.00685    1.16124    0.87484
     10.00000    99.9187     3.25479    1.99965    0.51252    3.19067    1.02010
     20.00000   999.7162     9.92302    2.99988    0.99664    9.92510    0.99979

AREA  75 (SL,SS,NL,NS)=( 365, 215,  20,  20) K=     9.51882 E/DN RDN=     0.41943 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000    10.0825     1.05200    1.00357    0.02201    1.11137    0.94658
     10.00000   100.1162     3.29987    2.00050    0.51850    3.27011    1.00910
     20.00000  1000.1125    10.25784    3.00005    1.01106   10.25879    0.99991

AREA  76 (SL,SS,NL,NS)=( 365, 265,  20,  20) K=    10.09547 E/DN RDN=    -0.20599 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000    10.0350     1.05466    1.00152    0.02311    1.01806    1.03595
     10.00000   100.1400     3.07967    2.00061    0.48850    3.15622    0.97575
     20.00000   999.7600     9.94221    2.99990    0.99748    9.95354    0.99886

AREA  77 (SL,SS,NL,NS)=( 365, 315,  20,  20) K=     9.25589 E/DN RDN=    -0.33873 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000    10.0863     1.02852    1.00373    0.01221    1.09748    0.93717
     10.00000   100.1550     3.17891    2.00067    0.50228    3.30687    0.96130
     20.00000   999.4525    10.37686    2.99976    1.01607   10.39687    0.99808

AREA  78 (SL,SS,NL,NS)=( 365, 365,  20,  20) K=     9.28078 E/DN RDN=    -0.24020 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9462     1.06000    0.99766    0.02531    1.06273    0.99743
     10.00000   100.0837     3.20576    2.00036    0.50593    3.29267    0.97360
     20.00000  1000.5312    10.37272    3.00023    1.01589   10.38578    0.99874

AREA  79 (SL,SS,NL,NS)=( 365, 415,  20,  20) K=    11.47499 E/DN RDN=     1.00377 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9937     1.00277    0.99973    0.00120    1.37058    0.73164
     10.00000    99.9238     3.29164    1.99967    0.51741    3.11697    1.05604
     20.00000  1000.1425     9.38372    3.00006    0.97238    9.38968    0.99937

AREA  80 (SL,SS,NL,NS)=( 365, 465,  20,  20) K=    10.55617 E/DN RDN=     0.50581 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9850     1.06534    0.99935    0.02749    1.09624    0.97182
     10.00000    99.9538     3.16368    1.99980    0.50019    3.11843    1.01451
     20.00000  1000.0950     9.74514    3.00004    0.98879    9.74660    0.99985

AREA  81 (SL,SS,NL,NS)=( 415,  15,  20,  20) K=     9.64604 E/DN RDN=     0.73388 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9425     1.04880    0.99750    0.02069    1.25272    0.83722
     10.00000   100.0425     3.39240    2.00018    0.53051    3.30302    1.02706
     20.00000   999.9400    10.20500    2.99997    1.00881   10.20793    0.99971

AREA  82 (SL,SS,NL,NS)=( 415,  65,  20,  20) K=     9.94912 E/DN RDN=    -0.13542 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9563     1.07178    0.99810    0.03011    1.00948    1.06172
     10.00000    99.9413     3.12400    1.99974    0.49471    3.17231    0.98477
     20.00000   999.7062    10.01806    2.99987    1.00078   10.02498    0.99931

AREA  83 (SL,SS,NL,NS)=( 415, 115,  20,  20) K=     9.64859 E/DN RDN=     0.49659 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9850     1.08970    0.99935    0.03731    1.13202    0.96262
     10.00000   100.0950     3.30071    2.00041    0.51861    3.25894    1.01282
     20.00000  1000.1325    10.19191    3.00006    1.00826   10.19325    0.99987

AREA  84 (SL,SS,NL,NS)=( 415, 165,  20,  20) K=     9.99765 E/DN RDN=    -0.41223 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9888     1.07155    0.99951    0.03001    1.08122    0.99105
     10.00000    99.9287     3.02076    1.99969    0.48012    3.18828    0.94746
     20.00000   999.7800     9.98173    2.99990    0.99921   10.00857    0.99732

AREA  85 (SL,SS,NL,NS)=( 415, 215,  20,  20) K=     9.47067 E/DN RDN=     0.31530 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9913     1.07903    0.99962    0.03303    1.07442    1.00429
     10.00000    99.6438     3.27584    1.99845    0.51532    3.25894    1.00518
     20.00000  1000.3613    10.28182    3.00016    1.01207   10.28235    0.99995

AREA  86 (SL,SS,NL,NS)=( 415, 265,  20,  20) K=     9.84201 E/DN RDN=     0.76710 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000    10.0112     1.05974    1.00049    0.02520    1.26714    0.83633
     10.00000    99.7475     3.37298    1.99890    0.52801    3.27465    1.03003
     20.00000   999.5575    10.10364    2.99981    1.00448   10.10687    0.99968

AREA  87 (SL,SS,NL,NS)=( 415, 315,  20,  20) K=     9.34843 E/DN RDN=     0.46536 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000    10.0000     1.06873    1.00000    0.02887    1.13413    0.94233
     10.00000   100.0188     3.34009    2.00008    0.52376    3.30387    1.01096
     20.00000  1000.2288    10.35311    3.00010    1.01507   10.35427    0.99989

AREA  88 (SL,SS,NL,NS)=( 415, 365,  20,  20) K=     8.77626 E/DN RDN=    -0.61075 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9700     1.03629    0.99870    0.01548    1.22843    0.84359
     10.00000    99.9362     3.17771    1.99972    0.50211    3.42930    0.92664
     20.00000  1000.8900    10.65374    3.00039    1.02750   10.69664    0.99599

AREA  89 (SL,SS,NL,NS)=( 415, 415,  20,  20) K=     9.72006 E/DN RDN=    -0.30946 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9462     1.04144    0.99766    0.01763    1.05784    0.98449
     10.00000    99.8975     3.10229    1.99955    0.49168    3.22075    0.96322
     20.00000   999.8862    10.12883    2.99995    1.00556   10.14712    0.99820

AREA  90 (SL,SS,NL,NS)=( 415, 465,  20,  20) K=     9.50211 E/DN RDN=     0.57434 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9563     1.05481    0.99810    0.02317    1.17374    0.89868
     10.00000    99.9013     3.34812    1.99957    0.52480    3.29294    1.01676
     20.00000  1000.0812    10.27334    3.00004    1.01171   10.27513    0.99983

AREA  91 (SL,SS,NL,NS)=( 465,  15,  20,  20) K=    10.31689 E/DN RDN=     0.97256 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9575     1.02460    0.99815    0.01055    1.38240    0.74117
     10.00000   100.2363     3.42240    2.00102    0.53433    3.26521    1.04814
     20.00000  1000.0037     9.88784    3.00000    0.99510    9.89316    0.99946

AREA  92 (SL,SS,NL,NS)=( 465,  65,  20,  20) K=     9.99379 E/DN RDN=     0.37400 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.8988     1.03017    0.99558    0.01291    1.06319    0.96894
     10.00000    99.9688     3.20911    1.99986    0.50638    3.18480    1.00763
     20.00000  1000.2225    10.01043    3.00010    1.00045   10.01121    0.99992

AREA  93 (SL,SS,NL,NS)=( 465, 115,  20,  20) K=     9.41280 E/DN RDN=    -0.16952 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000    10.0225     1.03546    1.00098    0.01514    1.04571    0.99020
     10.00000   100.0150     3.20430    2.00007    0.50573    3.26407    0.98169
     20.00000  1000.3725    10.30181    3.00016    1.01291   10.31051    0.99916

AREA  94 (SL,SS,NL,NS)=( 465, 165,  20,  20) K=     9.66141 E/DN RDN=     0.42355 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9575     1.05098    0.99815    0.02159    1.10002    0.95542
     10.00000    99.7650     3.27182    1.99898    0.51479    3.24122    1.00944
     20.00000  1000.2562    10.18286    3.00011    1.00787   10.18384    0.99990

AREA  95 (SL,SS,NL,NS)=( 465, 215,  20,  20) K=     9.40913 E/DN RDN=    -0.25293 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9987     1.04280    0.99995    0.01820    1.06143    0.98244
     10.00000   100.0788     3.17842    2.00034    0.50221    3.27113    0.97166
     20.00000   999.5350    10.29590    2.99980    1.01266   10.30991    0.99864

AREA  96 (SL,SS,NL,NS)=( 465, 265,  20,  20) K=    10.15835 E/DN RDN=    -0.09245 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9750     1.08814    0.99891    0.03669    0.99524    1.09335
     10.00000    99.7937     3.10301    1.99910    0.49178    3.13566    0.98959
     20.00000   999.9788     9.91750    2.99999    0.99640    9.92208    0.99954

AREA  97 (SL,SS,NL,NS)=( 465, 315,  20,  20) K=    10.31491 E/DN RDN=     0.38093 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9688     1.10493    0.99864    0.04334    1.05430    1.04802
     10.00000   100.0462     3.16315    2.00020    0.50012    3.13756    1.00816
     20.00000   999.6188     9.85084    2.99983    0.99347    9.85166    0.99992

AREA  98 (SL,SS,NL,NS)=( 465, 365,  20,  20) K=     9.78226 E/DN RDN=    -0.14110 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9825     1.08441    0.99924    0.03519    1.01999    1.06316
     10.00000    99.9675     3.14982    1.99986    0.49829    3.19987    0.98436
     20.00000   999.8287    10.10361    2.99993    1.00448   10.11080    0.99929

AREA  99 (SL,SS,NL,NS)=( 465, 415,  20,  20) K=    11.68719 E/DN RDN=     0.78994 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9537     1.02038    0.99799    0.00876    1.21478    0.83997
     10.00000   100.1762     3.14467    2.00076    0.49758    3.03240    1.03702
     20.00000   999.5463     9.27790    2.99980    0.96745    9.28165    0.99960

AREA 100 (SL,SS,NL,NS)=( 465, 465,  20,  20) K=    10.46812 E/DN RDN=    -0.03306 DN
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9675     1.08706    0.99859    0.03625    0.97636    1.11339
     10.00000   100.1725     3.08212    2.00075    0.48885    3.09360    0.99629
     20.00000  1000.0025     9.77236    3.00000    0.99000    9.77392    0.99984

Global value for K...
Raw mean and sigma are...
N= 100 MEAN=     9.94313 SIGMA=     0.64317
After throwing out samples differing by 2 sigma
N=  94 MEAN=     9.83853 SIGMA=     0.50575

Global noise floor...
Raw mean and sigma are...
N= 100 MEAN=     0.12105 SIGMA=     0.54112
After throwing out samples differing by 2 sigma
N=  99 MEAN=     0.13239 SIGMA=     0.53189

AREA  15 (SL,SS,NL,NS)=(  65, 215,  20,  20)  *****BAD SYSTEM GAIN*****
AREA  21 (SL,SS,NL,NS)=( 115,  15,  20,  20)  *****BAD SYSTEM GAIN*****
AREA  37 (SL,SS,NL,NS)=( 165, 315,  20,  20)  *****BAD SYSTEM GAIN*****
AREA  65 (SL,SS,NL,NS)=( 315, 215,  20,  20)  *****BAD NOISE FLOOR*****
AREA  69 (SL,SS,NL,NS)=( 315, 415,  20,  20)  *****BAD SYSTEM GAIN*****
AREA  79 (SL,SS,NL,NS)=( 365, 415,  20,  20)  *****BAD SYSTEM GAIN*****
AREA  99 (SL,SS,NL,NS)=( 465, 415,  20,  20)  *****BAD SYSTEM GAIN*****

No rejection criteria applied
NUMBER OF GOOD AREAS= 100 OUT OF 100 AREAS SAMPLED
K=     9.94313 E/DN RDN=     0.12105 DN


UPPER-LEFT  CORNER  K=    9.9 E/DN  RDN= -0.026 DN  NAREA= 15
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9884     1.06231    0.99950    0.02625    1.00408    1.05800
     10.00000   100.0131     3.14657    2.00006    0.49784    3.17629    0.99064
     20.00000  1000.0436    10.04299    3.00002    1.00186   10.04357    0.99994

UPPER-RIGHT CORNER  K=   10.0 E/DN  RDN=  0.377 DN  NAREA= 15
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9930     1.06094    0.99970    0.02569    1.07052    0.99104
     10.00000   100.0158     3.19552    2.00007    0.50454    3.19201    1.00110
     20.00000  1000.0276    10.02965    3.00001    1.00129   10.02975    0.99999

LOWER-LEFT  CORNER  K=    9.8 E/DN  RDN= -0.002 DN  NAREA= 15
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9698     1.05697    0.99869    0.02406    1.01028    1.04621
     10.00000    99.9750     3.18195    1.99989    0.50269    3.19921    0.99461
     20.00000  1000.0966    10.11893    3.00004    1.00513   10.11853    1.00004

LOWER-RIGHT CORNER  K=   10.0 E/DN  RDN=  0.361 DN  NAREA= 15
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9768     1.05641    0.99899    0.02383    1.06170    0.99502
     10.00000    99.9883     3.18324    1.99995    0.50287    3.18131    1.00061
     20.00000   999.9760    10.00209    2.99999    1.00009   10.00215    0.99999

CENTER              K=    9.9 E/DN  RDN=  0.351 DN  NAREA= 40
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000    10.0104     1.07127    1.00045    0.02990    1.06358    1.00723
     10.00000    99.9837     3.18937    1.99993    0.50371    3.19220    0.99911
     20.00000   999.8469    10.03965    2.99993    1.00172   10.03957    1.00001
CCDNOISE task completed
if (mode = "nobatch" or mode = "inter")
    ush gnuplot gain2.gpi
end-if
ccdnoise ltf1.dat table=gain3.tbl plot=gain3 reject=1
Beginning VICAR task ccdnoise
CCDNOISE version 13-Jul-2013
----TASK:-GAUSNOIS---USER: lwk         Thu Oct 10 12:27:37 2013
----TASK:-LTGEN   ---USER: lwk         Thu Oct 10 12:27:39 2013
AREA   1 (SL,SS,NL,NS)=(  15,  15,  20,  20) K=    10.30742 E/DN RDN=     0.62024 DN
AREA   2 (SL,SS,NL,NS)=(  15,  65,  20,  20) K=     9.93566 E/DN RDN=     0.54951 DN
AREA   3 (SL,SS,NL,NS)=(  15, 115,  20,  20) K=     9.79330 E/DN RDN=    -0.87523 DN
AREA   4 (SL,SS,NL,NS)=(  15, 165,  20,  20) K=     9.83507 E/DN RDN=    -0.49816 DN
AREA   5 (SL,SS,NL,NS)=(  15, 215,  20,  20) K=     9.23651 E/DN RDN=    -0.73807 DN
AREA   6 (SL,SS,NL,NS)=(  15, 265,  20,  20) K=    10.10108 E/DN RDN=    -0.10256 DN
AREA   7 (SL,SS,NL,NS)=(  15, 315,  20,  20) K=     9.33507 E/DN RDN=    -0.80116 DN
AREA   8 (SL,SS,NL,NS)=(  15, 365,  20,  20) K=    10.44968 E/DN RDN=     0.74109 DN
AREA   9 (SL,SS,NL,NS)=(  15, 415,  20,  20) K=     9.63899 E/DN RDN=     0.69265 DN
AREA  10 (SL,SS,NL,NS)=(  15, 465,  20,  20) K=    10.40750 E/DN RDN=    -0.35611 DN
AREA  11 (SL,SS,NL,NS)=(  65,  15,  20,  20) K=     9.25803 E/DN RDN=    -0.32982 DN
AREA  12 (SL,SS,NL,NS)=(  65,  65,  20,  20) K=     9.80003 E/DN RDN=     0.32338 DN
AREA  13 (SL,SS,NL,NS)=(  65, 115,  20,  20) K=    10.14556 E/DN RDN=     0.69539 DN
AREA  14 (SL,SS,NL,NS)=(  65, 165,  20,  20) K=    10.95564 E/DN RDN=     0.74933 DN
AREA  15 (SL,SS,NL,NS)=(  65, 215,  20,  20) K=    11.66815 E/DN RDN=     1.11472 DN
AREA  16 (SL,SS,NL,NS)=(  65, 265,  20,  20) K=    10.27847 E/DN RDN=     0.30297 DN
AREA  17 (SL,SS,NL,NS)=(  65, 315,  20,  20) K=     9.89986 E/DN RDN=     0.47651 DN
AREA  18 (SL,SS,NL,NS)=(  65, 365,  20,  20) K=    10.60149 E/DN RDN=     0.88050 DN
AREA  19 (SL,SS,NL,NS)=(  65, 415,  20,  20) K=    10.34882 E/DN RDN=     0.73474 DN
AREA  20 (SL,SS,NL,NS)=(  65, 465,  20,  20) K=    10.58272 E/DN RDN=     0.45417 DN
AREA  21 (SL,SS,NL,NS)=( 115,  15,  20,  20) K=    11.42757 E/DN RDN=     0.85727 DN
AREA  22 (SL,SS,NL,NS)=( 115,  65,  20,  20) K=     9.65102 E/DN RDN=    -0.14995 DN
AREA  23 (SL,SS,NL,NS)=( 115, 115,  20,  20) K=     9.21765 E/DN RDN=    -0.55244 DN
AREA  24 (SL,SS,NL,NS)=( 115, 165,  20,  20) K=     9.42420 E/DN RDN=    -0.19428 DN
AREA  25 (SL,SS,NL,NS)=( 115, 215,  20,  20) K=     9.20208 E/DN RDN=    -0.33420 DN
AREA  26 (SL,SS,NL,NS)=( 115, 265,  20,  20) K=    10.07218 E/DN RDN=    -0.56563 DN
AREA  27 (SL,SS,NL,NS)=( 115, 315,  20,  20) K=     8.99965 E/DN RDN=    -0.63568 DN
AREA  28 (SL,SS,NL,NS)=( 115, 365,  20,  20) K=     9.33415 E/DN RDN=    -0.65101 DN
AREA  29 (SL,SS,NL,NS)=( 115, 415,  20,  20) K=     9.33027 E/DN RDN=    -0.31300 DN
AREA  30 (SL,SS,NL,NS)=( 115, 465,  20,  20) K=     9.37539 E/DN RDN=     0.28048 DN
AREA  31 (SL,SS,NL,NS)=( 165,  15,  20,  20) K=    10.18426 E/DN RDN=     0.45960 DN
AREA  32 (SL,SS,NL,NS)=( 165,  65,  20,  20) K=     9.80777 E/DN RDN=    -0.62880 DN
AREA  33 (SL,SS,NL,NS)=( 165, 115,  20,  20) K=     9.46085 E/DN RDN=    -0.41605 DN
AREA  34 (SL,SS,NL,NS)=( 165, 165,  20,  20) K=     9.94927 E/DN RDN=     0.56484 DN
AREA  35 (SL,SS,NL,NS)=( 165, 215,  20,  20) K=    10.05241 E/DN RDN=     0.18650 DN
AREA  36 (SL,SS,NL,NS)=( 165, 265,  20,  20) K=     9.38524 E/DN RDN=    -0.00774 DN
AREA  37 (SL,SS,NL,NS)=( 165, 315,  20,  20) K=    11.86728 E/DN RDN=     0.82450 DN
AREA  38 (SL,SS,NL,NS)=( 165, 365,  20,  20) K=     9.39617 E/DN RDN=     0.52621 DN
AREA  39 (SL,SS,NL,NS)=( 165, 415,  20,  20) K=     9.37466 E/DN RDN=     0.37411 DN
AREA  40 (SL,SS,NL,NS)=( 165, 465,  20,  20) K=    10.77020 E/DN RDN=     0.86076 DN
AREA  41 (SL,SS,NL,NS)=( 215,  15,  20,  20) K=     9.45240 E/DN RDN=    -0.35441 DN
AREA  42 (SL,SS,NL,NS)=( 215,  65,  20,  20) K=    10.50862 E/DN RDN=     0.77067 DN
AREA  43 (SL,SS,NL,NS)=( 215, 115,  20,  20) K=    10.40969 E/DN RDN=    -0.03484 DN
AREA  44 (SL,SS,NL,NS)=( 215, 165,  20,  20) K=     9.76946 E/DN RDN=     0.58446 DN
AREA  45 (SL,SS,NL,NS)=( 215, 215,  20,  20) K=    10.41095 E/DN RDN=     0.42936 DN
AREA  46 (SL,SS,NL,NS)=( 215, 265,  20,  20) K=    10.28357 E/DN RDN=     0.59148 DN
AREA  47 (SL,SS,NL,NS)=( 215, 315,  20,  20) K=     9.86580 E/DN RDN=    -0.08678 DN
AREA  48 (SL,SS,NL,NS)=( 215, 365,  20,  20) K=     9.24523 E/DN RDN=    -0.37095 DN
AREA  49 (SL,SS,NL,NS)=( 215, 415,  20,  20) K=    10.01096 E/DN RDN=     0.87833 DN
AREA  50 (SL,SS,NL,NS)=( 215, 465,  20,  20) K=     9.98657 E/DN RDN=    -0.32773 DN
AREA  51 (SL,SS,NL,NS)=( 265,  15,  20,  20) K=     9.09666 E/DN RDN=    -0.81045 DN
AREA  52 (SL,SS,NL,NS)=( 265,  65,  20,  20) K=     9.83588 E/DN RDN=    -0.13778 DN
AREA  53 (SL,SS,NL,NS)=( 265, 115,  20,  20) K=    10.40543 E/DN RDN=     0.46613 DN
AREA  54 (SL,SS,NL,NS)=( 265, 165,  20,  20) K=    10.68326 E/DN RDN=     0.46152 DN
AREA  55 (SL,SS,NL,NS)=( 265, 215,  20,  20) K=    10.39200 E/DN RDN=    -0.29464 DN
AREA  56 (SL,SS,NL,NS)=( 265, 265,  20,  20) K=    10.67752 E/DN RDN=     0.67597 DN
AREA  57 (SL,SS,NL,NS)=( 265, 315,  20,  20) K=    10.88648 E/DN RDN=     0.66295 DN
AREA  58 (SL,SS,NL,NS)=( 265, 365,  20,  20) K=     8.80985 E/DN RDN=    -0.38740 DN
AREA  59 (SL,SS,NL,NS)=( 265, 415,  20,  20) K=    10.24336 E/DN RDN=     0.00376 DN
AREA  60 (SL,SS,NL,NS)=( 265, 465,  20,  20) K=     9.74251 E/DN RDN=     0.37287 DN
AREA  61 (SL,SS,NL,NS)=( 315,  15,  20,  20) K=     9.92099 E/DN RDN=    -0.30064 DN
AREA  62 (SL,SS,NL,NS)=( 315,  65,  20,  20) K=    10.36872 E/DN RDN=     0.85959 DN
AREA  63 (SL,SS,NL,NS)=( 315, 115,  20,  20) K=     8.90701 E/DN RDN=    -0.78957 DN
AREA  64 (SL,SS,NL,NS)=( 315, 165,  20,  20) K=    10.20292 E/DN RDN=     0.70946 DN
AREA  65 (SL,SS,NL,NS)=( 315, 215,  20,  20) K=     9.11692 E/DN RDN=    -1.00175 DN
AREA  66 (SL,SS,NL,NS)=( 315, 265,  20,  20) K=     9.78322 E/DN RDN=    -0.53489 DN
AREA  67 (SL,SS,NL,NS)=( 315, 315,  20,  20) K=    10.11680 E/DN RDN=     0.15311 DN
AREA  68 (SL,SS,NL,NS)=( 315, 365,  20,  20) K=     9.57502 E/DN RDN=     0.47486 DN
AREA  69 (SL,SS,NL,NS)=( 315, 415,  20,  20) K=    11.36624 E/DN RDN=     0.44566 DN
AREA  70 (SL,SS,NL,NS)=( 315, 465,  20,  20) K=     8.72955 E/DN RDN=    -0.59912 DN
AREA  71 (SL,SS,NL,NS)=( 365,  15,  20,  20) K=     9.38370 E/DN RDN=    -0.78727 DN
AREA  72 (SL,SS,NL,NS)=( 365,  65,  20,  20) K=     9.71555 E/DN RDN=    -0.55521 DN
AREA  73 (SL,SS,NL,NS)=( 365, 115,  20,  20) K=    10.07417 E/DN RDN=    -0.07161 DN
AREA  74 (SL,SS,NL,NS)=( 365, 165,  20,  20) K=    10.18708 E/DN RDN=     0.60991 DN
AREA  75 (SL,SS,NL,NS)=( 365, 215,  20,  20) K=     9.51882 E/DN RDN=     0.41943 DN
AREA  76 (SL,SS,NL,NS)=( 365, 265,  20,  20) K=    10.09547 E/DN RDN=    -0.20599 DN
AREA  77 (SL,SS,NL,NS)=( 365, 315,  20,  20) K=     9.25589 E/DN RDN=    -0.33873 DN
AREA  78 (SL,SS,NL,NS)=( 365, 365,  20,  20) K=     9.28078 E/DN RDN=    -0.24020 DN
AREA  79 (SL,SS,NL,NS)=( 365, 415,  20,  20) K=    11.47499 E/DN RDN=     1.00377 DN
AREA  80 (SL,SS,NL,NS)=( 365, 465,  20,  20) K=    10.55617 E/DN RDN=     0.50581 DN
AREA  81 (SL,SS,NL,NS)=( 415,  15,  20,  20) K=     9.64604 E/DN RDN=     0.73388 DN
AREA  82 (SL,SS,NL,NS)=( 415,  65,  20,  20) K=     9.94912 E/DN RDN=    -0.13542 DN
AREA  83 (SL,SS,NL,NS)=( 415, 115,  20,  20) K=     9.64859 E/DN RDN=     0.49659 DN
AREA  84 (SL,SS,NL,NS)=( 415, 165,  20,  20) K=     9.99765 E/DN RDN=    -0.41223 DN
AREA  85 (SL,SS,NL,NS)=( 415, 215,  20,  20) K=     9.47067 E/DN RDN=     0.31530 DN
AREA  86 (SL,SS,NL,NS)=( 415, 265,  20,  20) K=     9.84201 E/DN RDN=     0.76710 DN
AREA  87 (SL,SS,NL,NS)=( 415, 315,  20,  20) K=     9.34843 E/DN RDN=     0.46536 DN
AREA  88 (SL,SS,NL,NS)=( 415, 365,  20,  20) K=     8.77626 E/DN RDN=    -0.61075 DN
AREA  89 (SL,SS,NL,NS)=( 415, 415,  20,  20) K=     9.72006 E/DN RDN=    -0.30946 DN
AREA  90 (SL,SS,NL,NS)=( 415, 465,  20,  20) K=     9.50211 E/DN RDN=     0.57434 DN
AREA  91 (SL,SS,NL,NS)=( 465,  15,  20,  20) K=    10.31689 E/DN RDN=     0.97256 DN
AREA  92 (SL,SS,NL,NS)=( 465,  65,  20,  20) K=     9.99379 E/DN RDN=     0.37400 DN
AREA  93 (SL,SS,NL,NS)=( 465, 115,  20,  20) K=     9.41280 E/DN RDN=    -0.16952 DN
AREA  94 (SL,SS,NL,NS)=( 465, 165,  20,  20) K=     9.66141 E/DN RDN=     0.42355 DN
AREA  95 (SL,SS,NL,NS)=( 465, 215,  20,  20) K=     9.40913 E/DN RDN=    -0.25293 DN
AREA  96 (SL,SS,NL,NS)=( 465, 265,  20,  20) K=    10.15835 E/DN RDN=    -0.09245 DN
AREA  97 (SL,SS,NL,NS)=( 465, 315,  20,  20) K=    10.31491 E/DN RDN=     0.38093 DN
AREA  98 (SL,SS,NL,NS)=( 465, 365,  20,  20) K=     9.78226 E/DN RDN=    -0.14110 DN
AREA  99 (SL,SS,NL,NS)=( 465, 415,  20,  20) K=    11.68719 E/DN RDN=     0.78994 DN
AREA 100 (SL,SS,NL,NS)=( 465, 465,  20,  20) K=    10.46812 E/DN RDN=    -0.03306 DN

Global value for K...
Raw mean and sigma are...
N= 100 MEAN=     9.94313 SIGMA=     0.64317
After throwing out samples differing by 2 sigma
N=  94 MEAN=     9.83853 SIGMA=     0.50575

Global noise floor...
Raw mean and sigma are...
N= 100 MEAN=     0.12105 SIGMA=     0.54112
After throwing out samples differing by 2 sigma
N=  99 MEAN=     0.13239 SIGMA=     0.53189

AREA  15 (SL,SS,NL,NS)=(  65, 215,  20,  20)  *****BAD SYSTEM GAIN*****
AREA  21 (SL,SS,NL,NS)=( 115,  15,  20,  20)  *****BAD SYSTEM GAIN*****
AREA  37 (SL,SS,NL,NS)=( 165, 315,  20,  20)  *****BAD SYSTEM GAIN*****
AREA  65 (SL,SS,NL,NS)=( 315, 215,  20,  20)  *****BAD NOISE FLOOR*****
AREA  69 (SL,SS,NL,NS)=( 315, 415,  20,  20)  *****BAD SYSTEM GAIN*****
AREA  79 (SL,SS,NL,NS)=( 365, 415,  20,  20)  *****BAD SYSTEM GAIN*****
AREA  99 (SL,SS,NL,NS)=( 465, 415,  20,  20)  *****BAD SYSTEM GAIN*****

NUMBER REJECTED FOR  SYSTEM GAIN  =     6
NUMBER OF GOOD AREAS=  94 OUT OF 100 AREAS SAMPLED
K=     9.83853 E/DN RDN=     0.07521 DN


UPPER-LEFT  CORNER  K=    9.8 E/DN  RDN= -0.088 DN  NAREA= 14
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9821     1.06523    0.99922    0.02744    1.01225    1.05234
     10.00000   100.0148     3.14207    2.00006    0.49722    3.19308    0.98402
     20.00000  1000.0311    10.08960    3.00001    1.00387   10.09334    0.99963

UPPER-RIGHT CORNER  K=   10.0 E/DN  RDN=  0.377 DN  NAREA= 15
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9930     1.06094    0.99970    0.02569    1.07052    0.99104
     10.00000   100.0158     3.19552    2.00007    0.50454    3.19201    1.00110
     20.00000  1000.0276    10.02965    3.00001    1.00129   10.02975    0.99999

LOWER-LEFT  CORNER  K=    9.8 E/DN  RDN= -0.002 DN  NAREA= 15
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9698     1.05697    0.99869    0.02406    1.01028    1.04621
     10.00000    99.9750     3.18195    1.99989    0.50269    3.19921    0.99461
     20.00000  1000.0966    10.11893    3.00004    1.00513   10.11853    1.00004

LOWER-RIGHT CORNER  K=    9.7 E/DN  RDN= -0.024 DN  NAREA= 12
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9773     1.06241    0.99901    0.02629    1.01569    1.04600
     10.00000    99.9856     3.18982    1.99994    0.50377    3.21452    0.99232
     20.00000  1000.0182    10.16516    3.00001    1.00711   10.16576    0.99994

CENTER              K=    9.8 E/DN  RDN=  0.261 DN  NAREA= 38
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000    10.0110     1.07073    1.00048    0.02968    1.04152    1.02805
     10.00000    99.9795     3.18651    1.99991    0.50331    3.19714    0.99667
     20.00000   999.8592    10.08053    2.99994    1.00348   10.08022    1.00003
CCDNOISE task completed
if (mode = "nobatch" or mode = "inter")
    ush gnuplot gain3.gpi
end-if
ccdnoise ltf1.dat table=gain4.tbl plot=gain4 reject=2
Beginning VICAR task ccdnoise
CCDNOISE version 13-Jul-2013
----TASK:-GAUSNOIS---USER: lwk         Thu Oct 10 12:27:37 2013
----TASK:-LTGEN   ---USER: lwk         Thu Oct 10 12:27:39 2013
AREA   1 (SL,SS,NL,NS)=(  15,  15,  20,  20) K=    10.30742 E/DN RDN=     0.62024 DN
AREA   2 (SL,SS,NL,NS)=(  15,  65,  20,  20) K=     9.93566 E/DN RDN=     0.54951 DN
AREA   3 (SL,SS,NL,NS)=(  15, 115,  20,  20) K=     9.79330 E/DN RDN=    -0.87523 DN
AREA   4 (SL,SS,NL,NS)=(  15, 165,  20,  20) K=     9.83507 E/DN RDN=    -0.49816 DN
AREA   5 (SL,SS,NL,NS)=(  15, 215,  20,  20) K=     9.23651 E/DN RDN=    -0.73807 DN
AREA   6 (SL,SS,NL,NS)=(  15, 265,  20,  20) K=    10.10108 E/DN RDN=    -0.10256 DN
AREA   7 (SL,SS,NL,NS)=(  15, 315,  20,  20) K=     9.33507 E/DN RDN=    -0.80116 DN
AREA   8 (SL,SS,NL,NS)=(  15, 365,  20,  20) K=    10.44968 E/DN RDN=     0.74109 DN
AREA   9 (SL,SS,NL,NS)=(  15, 415,  20,  20) K=     9.63899 E/DN RDN=     0.69265 DN
AREA  10 (SL,SS,NL,NS)=(  15, 465,  20,  20) K=    10.40750 E/DN RDN=    -0.35611 DN
AREA  11 (SL,SS,NL,NS)=(  65,  15,  20,  20) K=     9.25803 E/DN RDN=    -0.32982 DN
AREA  12 (SL,SS,NL,NS)=(  65,  65,  20,  20) K=     9.80003 E/DN RDN=     0.32338 DN
AREA  13 (SL,SS,NL,NS)=(  65, 115,  20,  20) K=    10.14556 E/DN RDN=     0.69539 DN
AREA  14 (SL,SS,NL,NS)=(  65, 165,  20,  20) K=    10.95564 E/DN RDN=     0.74933 DN
AREA  15 (SL,SS,NL,NS)=(  65, 215,  20,  20) K=    11.66815 E/DN RDN=     1.11472 DN
AREA  16 (SL,SS,NL,NS)=(  65, 265,  20,  20) K=    10.27847 E/DN RDN=     0.30297 DN
AREA  17 (SL,SS,NL,NS)=(  65, 315,  20,  20) K=     9.89986 E/DN RDN=     0.47651 DN
AREA  18 (SL,SS,NL,NS)=(  65, 365,  20,  20) K=    10.60149 E/DN RDN=     0.88050 DN
AREA  19 (SL,SS,NL,NS)=(  65, 415,  20,  20) K=    10.34882 E/DN RDN=     0.73474 DN
AREA  20 (SL,SS,NL,NS)=(  65, 465,  20,  20) K=    10.58272 E/DN RDN=     0.45417 DN
AREA  21 (SL,SS,NL,NS)=( 115,  15,  20,  20) K=    11.42757 E/DN RDN=     0.85727 DN
AREA  22 (SL,SS,NL,NS)=( 115,  65,  20,  20) K=     9.65102 E/DN RDN=    -0.14995 DN
AREA  23 (SL,SS,NL,NS)=( 115, 115,  20,  20) K=     9.21765 E/DN RDN=    -0.55244 DN
AREA  24 (SL,SS,NL,NS)=( 115, 165,  20,  20) K=     9.42420 E/DN RDN=    -0.19428 DN
AREA  25 (SL,SS,NL,NS)=( 115, 215,  20,  20) K=     9.20208 E/DN RDN=    -0.33420 DN
AREA  26 (SL,SS,NL,NS)=( 115, 265,  20,  20) K=    10.07218 E/DN RDN=    -0.56563 DN
AREA  27 (SL,SS,NL,NS)=( 115, 315,  20,  20) K=     8.99965 E/DN RDN=    -0.63568 DN
AREA  28 (SL,SS,NL,NS)=( 115, 365,  20,  20) K=     9.33415 E/DN RDN=    -0.65101 DN
AREA  29 (SL,SS,NL,NS)=( 115, 415,  20,  20) K=     9.33027 E/DN RDN=    -0.31300 DN
AREA  30 (SL,SS,NL,NS)=( 115, 465,  20,  20) K=     9.37539 E/DN RDN=     0.28048 DN
AREA  31 (SL,SS,NL,NS)=( 165,  15,  20,  20) K=    10.18426 E/DN RDN=     0.45960 DN
AREA  32 (SL,SS,NL,NS)=( 165,  65,  20,  20) K=     9.80777 E/DN RDN=    -0.62880 DN
AREA  33 (SL,SS,NL,NS)=( 165, 115,  20,  20) K=     9.46085 E/DN RDN=    -0.41605 DN
AREA  34 (SL,SS,NL,NS)=( 165, 165,  20,  20) K=     9.94927 E/DN RDN=     0.56484 DN
AREA  35 (SL,SS,NL,NS)=( 165, 215,  20,  20) K=    10.05241 E/DN RDN=     0.18650 DN
AREA  36 (SL,SS,NL,NS)=( 165, 265,  20,  20) K=     9.38524 E/DN RDN=    -0.00774 DN
AREA  37 (SL,SS,NL,NS)=( 165, 315,  20,  20) K=    11.86728 E/DN RDN=     0.82450 DN
AREA  38 (SL,SS,NL,NS)=( 165, 365,  20,  20) K=     9.39617 E/DN RDN=     0.52621 DN
AREA  39 (SL,SS,NL,NS)=( 165, 415,  20,  20) K=     9.37466 E/DN RDN=     0.37411 DN
AREA  40 (SL,SS,NL,NS)=( 165, 465,  20,  20) K=    10.77020 E/DN RDN=     0.86076 DN
AREA  41 (SL,SS,NL,NS)=( 215,  15,  20,  20) K=     9.45240 E/DN RDN=    -0.35441 DN
AREA  42 (SL,SS,NL,NS)=( 215,  65,  20,  20) K=    10.50862 E/DN RDN=     0.77067 DN
AREA  43 (SL,SS,NL,NS)=( 215, 115,  20,  20) K=    10.40969 E/DN RDN=    -0.03484 DN
AREA  44 (SL,SS,NL,NS)=( 215, 165,  20,  20) K=     9.76946 E/DN RDN=     0.58446 DN
AREA  45 (SL,SS,NL,NS)=( 215, 215,  20,  20) K=    10.41095 E/DN RDN=     0.42936 DN
AREA  46 (SL,SS,NL,NS)=( 215, 265,  20,  20) K=    10.28357 E/DN RDN=     0.59148 DN
AREA  47 (SL,SS,NL,NS)=( 215, 315,  20,  20) K=     9.86580 E/DN RDN=    -0.08678 DN
AREA  48 (SL,SS,NL,NS)=( 215, 365,  20,  20) K=     9.24523 E/DN RDN=    -0.37095 DN
AREA  49 (SL,SS,NL,NS)=( 215, 415,  20,  20) K=    10.01096 E/DN RDN=     0.87833 DN
AREA  50 (SL,SS,NL,NS)=( 215, 465,  20,  20) K=     9.98657 E/DN RDN=    -0.32773 DN
AREA  51 (SL,SS,NL,NS)=( 265,  15,  20,  20) K=     9.09666 E/DN RDN=    -0.81045 DN
AREA  52 (SL,SS,NL,NS)=( 265,  65,  20,  20) K=     9.83588 E/DN RDN=    -0.13778 DN
AREA  53 (SL,SS,NL,NS)=( 265, 115,  20,  20) K=    10.40543 E/DN RDN=     0.46613 DN
AREA  54 (SL,SS,NL,NS)=( 265, 165,  20,  20) K=    10.68326 E/DN RDN=     0.46152 DN
AREA  55 (SL,SS,NL,NS)=( 265, 215,  20,  20) K=    10.39200 E/DN RDN=    -0.29464 DN
AREA  56 (SL,SS,NL,NS)=( 265, 265,  20,  20) K=    10.67752 E/DN RDN=     0.67597 DN
AREA  57 (SL,SS,NL,NS)=( 265, 315,  20,  20) K=    10.88648 E/DN RDN=     0.66295 DN
AREA  58 (SL,SS,NL,NS)=( 265, 365,  20,  20) K=     8.80985 E/DN RDN=    -0.38740 DN
AREA  59 (SL,SS,NL,NS)=( 265, 415,  20,  20) K=    10.24336 E/DN RDN=     0.00376 DN
AREA  60 (SL,SS,NL,NS)=( 265, 465,  20,  20) K=     9.74251 E/DN RDN=     0.37287 DN
AREA  61 (SL,SS,NL,NS)=( 315,  15,  20,  20) K=     9.92099 E/DN RDN=    -0.30064 DN
AREA  62 (SL,SS,NL,NS)=( 315,  65,  20,  20) K=    10.36872 E/DN RDN=     0.85959 DN
AREA  63 (SL,SS,NL,NS)=( 315, 115,  20,  20) K=     8.90701 E/DN RDN=    -0.78957 DN
AREA  64 (SL,SS,NL,NS)=( 315, 165,  20,  20) K=    10.20292 E/DN RDN=     0.70946 DN
AREA  65 (SL,SS,NL,NS)=( 315, 215,  20,  20) K=     9.11692 E/DN RDN=    -1.00175 DN
AREA  66 (SL,SS,NL,NS)=( 315, 265,  20,  20) K=     9.78322 E/DN RDN=    -0.53489 DN
AREA  67 (SL,SS,NL,NS)=( 315, 315,  20,  20) K=    10.11680 E/DN RDN=     0.15311 DN
AREA  68 (SL,SS,NL,NS)=( 315, 365,  20,  20) K=     9.57502 E/DN RDN=     0.47486 DN
AREA  69 (SL,SS,NL,NS)=( 315, 415,  20,  20) K=    11.36624 E/DN RDN=     0.44566 DN
AREA  70 (SL,SS,NL,NS)=( 315, 465,  20,  20) K=     8.72955 E/DN RDN=    -0.59912 DN
AREA  71 (SL,SS,NL,NS)=( 365,  15,  20,  20) K=     9.38370 E/DN RDN=    -0.78727 DN
AREA  72 (SL,SS,NL,NS)=( 365,  65,  20,  20) K=     9.71555 E/DN RDN=    -0.55521 DN
AREA  73 (SL,SS,NL,NS)=( 365, 115,  20,  20) K=    10.07417 E/DN RDN=    -0.07161 DN
AREA  74 (SL,SS,NL,NS)=( 365, 165,  20,  20) K=    10.18708 E/DN RDN=     0.60991 DN
AREA  75 (SL,SS,NL,NS)=( 365, 215,  20,  20) K=     9.51882 E/DN RDN=     0.41943 DN
AREA  76 (SL,SS,NL,NS)=( 365, 265,  20,  20) K=    10.09547 E/DN RDN=    -0.20599 DN
AREA  77 (SL,SS,NL,NS)=( 365, 315,  20,  20) K=     9.25589 E/DN RDN=    -0.33873 DN
AREA  78 (SL,SS,NL,NS)=( 365, 365,  20,  20) K=     9.28078 E/DN RDN=    -0.24020 DN
AREA  79 (SL,SS,NL,NS)=( 365, 415,  20,  20) K=    11.47499 E/DN RDN=     1.00377 DN
AREA  80 (SL,SS,NL,NS)=( 365, 465,  20,  20) K=    10.55617 E/DN RDN=     0.50581 DN
AREA  81 (SL,SS,NL,NS)=( 415,  15,  20,  20) K=     9.64604 E/DN RDN=     0.73388 DN
AREA  82 (SL,SS,NL,NS)=( 415,  65,  20,  20) K=     9.94912 E/DN RDN=    -0.13542 DN
AREA  83 (SL,SS,NL,NS)=( 415, 115,  20,  20) K=     9.64859 E/DN RDN=     0.49659 DN
AREA  84 (SL,SS,NL,NS)=( 415, 165,  20,  20) K=     9.99765 E/DN RDN=    -0.41223 DN
AREA  85 (SL,SS,NL,NS)=( 415, 215,  20,  20) K=     9.47067 E/DN RDN=     0.31530 DN
AREA  86 (SL,SS,NL,NS)=( 415, 265,  20,  20) K=     9.84201 E/DN RDN=     0.76710 DN
AREA  87 (SL,SS,NL,NS)=( 415, 315,  20,  20) K=     9.34843 E/DN RDN=     0.46536 DN
AREA  88 (SL,SS,NL,NS)=( 415, 365,  20,  20) K=     8.77626 E/DN RDN=    -0.61075 DN
AREA  89 (SL,SS,NL,NS)=( 415, 415,  20,  20) K=     9.72006 E/DN RDN=    -0.30946 DN
AREA  90 (SL,SS,NL,NS)=( 415, 465,  20,  20) K=     9.50211 E/DN RDN=     0.57434 DN
AREA  91 (SL,SS,NL,NS)=( 465,  15,  20,  20) K=    10.31689 E/DN RDN=     0.97256 DN
AREA  92 (SL,SS,NL,NS)=( 465,  65,  20,  20) K=     9.99379 E/DN RDN=     0.37400 DN
AREA  93 (SL,SS,NL,NS)=( 465, 115,  20,  20) K=     9.41280 E/DN RDN=    -0.16952 DN
AREA  94 (SL,SS,NL,NS)=( 465, 165,  20,  20) K=     9.66141 E/DN RDN=     0.42355 DN
AREA  95 (SL,SS,NL,NS)=( 465, 215,  20,  20) K=     9.40913 E/DN RDN=    -0.25293 DN
AREA  96 (SL,SS,NL,NS)=( 465, 265,  20,  20) K=    10.15835 E/DN RDN=    -0.09245 DN
AREA  97 (SL,SS,NL,NS)=( 465, 315,  20,  20) K=    10.31491 E/DN RDN=     0.38093 DN
AREA  98 (SL,SS,NL,NS)=( 465, 365,  20,  20) K=     9.78226 E/DN RDN=    -0.14110 DN
AREA  99 (SL,SS,NL,NS)=( 465, 415,  20,  20) K=    11.68719 E/DN RDN=     0.78994 DN
AREA 100 (SL,SS,NL,NS)=( 465, 465,  20,  20) K=    10.46812 E/DN RDN=    -0.03306 DN

Global value for K...
Raw mean and sigma are...
N= 100 MEAN=     9.94313 SIGMA=     0.64317
After throwing out samples differing by 2 sigma
N=  94 MEAN=     9.83853 SIGMA=     0.50575

Global noise floor...
Raw mean and sigma are...
N= 100 MEAN=     0.12105 SIGMA=     0.54112
After throwing out samples differing by 2 sigma
N=  99 MEAN=     0.13239 SIGMA=     0.53189

AREA  15 (SL,SS,NL,NS)=(  65, 215,  20,  20)  *****BAD SYSTEM GAIN*****
AREA  21 (SL,SS,NL,NS)=( 115,  15,  20,  20)  *****BAD SYSTEM GAIN*****
AREA  37 (SL,SS,NL,NS)=( 165, 315,  20,  20)  *****BAD SYSTEM GAIN*****
AREA  65 (SL,SS,NL,NS)=( 315, 215,  20,  20)  *****BAD NOISE FLOOR*****
AREA  69 (SL,SS,NL,NS)=( 315, 415,  20,  20)  *****BAD SYSTEM GAIN*****
AREA  79 (SL,SS,NL,NS)=( 365, 415,  20,  20)  *****BAD SYSTEM GAIN*****
AREA  99 (SL,SS,NL,NS)=( 465, 415,  20,  20)  *****BAD SYSTEM GAIN*****

NUMBER REJECTED FOR  NOISE FLOOR  =     1
NUMBER OF GOOD AREAS=  99 OUT OF 100 AREAS SAMPLED
K=     9.95148 E/DN RDN=     0.13239 DN


UPPER-LEFT  CORNER  K=    9.9 E/DN  RDN= -0.026 DN  NAREA= 15
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9884     1.06231    0.99950    0.02625    1.00408    1.05800
     10.00000   100.0131     3.14657    2.00006    0.49784    3.17629    0.99064
     20.00000  1000.0436    10.04299    3.00002    1.00186   10.04357    0.99994

UPPER-RIGHT CORNER  K=   10.0 E/DN  RDN=  0.377 DN  NAREA= 15
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9930     1.06094    0.99970    0.02569    1.07052    0.99104
     10.00000   100.0158     3.19552    2.00007    0.50454    3.19201    1.00110
     20.00000  1000.0276    10.02965    3.00001    1.00129   10.02975    0.99999

LOWER-LEFT  CORNER  K=    9.8 E/DN  RDN= -0.002 DN  NAREA= 15
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9698     1.05697    0.99869    0.02406    1.01028    1.04621
     10.00000    99.9750     3.18195    1.99989    0.50269    3.19921    0.99461
     20.00000  1000.0966    10.11893    3.00004    1.00513   10.11853    1.00004

LOWER-RIGHT CORNER  K=   10.0 E/DN  RDN=  0.361 DN  NAREA= 15
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9768     1.05641    0.99899    0.02383    1.06170    0.99502
     10.00000    99.9883     3.18324    1.99995    0.50287    3.18131    1.00061
     20.00000   999.9760    10.00209    2.99999    1.00009   10.00215    0.99999

CENTER              K=   10.0 E/DN  RDN=  0.387 DN  NAREA= 39
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000    10.0098     1.06935    1.00043    0.02912    1.07486    0.99488
     10.00000    99.9825     3.19484    1.99992    0.50445    3.19281    1.00064
     20.00000   999.8589    10.02971    2.99994    1.00129   10.02977    0.99999
CCDNOISE task completed
if (mode = "nobatch" or mode = "inter")
    ush gnuplot gain4.gpi
end-if
ccdnoise ltf1.dat table=gain5.tbl plot=gain5 reject=3
Beginning VICAR task ccdnoise
CCDNOISE version 13-Jul-2013
----TASK:-GAUSNOIS---USER: lwk         Thu Oct 10 12:27:37 2013
----TASK:-LTGEN   ---USER: lwk         Thu Oct 10 12:27:39 2013
AREA   1 (SL,SS,NL,NS)=(  15,  15,  20,  20) K=    10.30742 E/DN RDN=     0.62024 DN
AREA   2 (SL,SS,NL,NS)=(  15,  65,  20,  20) K=     9.93566 E/DN RDN=     0.54951 DN
AREA   3 (SL,SS,NL,NS)=(  15, 115,  20,  20) K=     9.79330 E/DN RDN=    -0.87523 DN
AREA   4 (SL,SS,NL,NS)=(  15, 165,  20,  20) K=     9.83507 E/DN RDN=    -0.49816 DN
AREA   5 (SL,SS,NL,NS)=(  15, 215,  20,  20) K=     9.23651 E/DN RDN=    -0.73807 DN
AREA   6 (SL,SS,NL,NS)=(  15, 265,  20,  20) K=    10.10108 E/DN RDN=    -0.10256 DN
AREA   7 (SL,SS,NL,NS)=(  15, 315,  20,  20) K=     9.33507 E/DN RDN=    -0.80116 DN
AREA   8 (SL,SS,NL,NS)=(  15, 365,  20,  20) K=    10.44968 E/DN RDN=     0.74109 DN
AREA   9 (SL,SS,NL,NS)=(  15, 415,  20,  20) K=     9.63899 E/DN RDN=     0.69265 DN
AREA  10 (SL,SS,NL,NS)=(  15, 465,  20,  20) K=    10.40750 E/DN RDN=    -0.35611 DN
AREA  11 (SL,SS,NL,NS)=(  65,  15,  20,  20) K=     9.25803 E/DN RDN=    -0.32982 DN
AREA  12 (SL,SS,NL,NS)=(  65,  65,  20,  20) K=     9.80003 E/DN RDN=     0.32338 DN
AREA  13 (SL,SS,NL,NS)=(  65, 115,  20,  20) K=    10.14556 E/DN RDN=     0.69539 DN
AREA  14 (SL,SS,NL,NS)=(  65, 165,  20,  20) K=    10.95564 E/DN RDN=     0.74933 DN
AREA  15 (SL,SS,NL,NS)=(  65, 215,  20,  20) K=    11.66815 E/DN RDN=     1.11472 DN
AREA  16 (SL,SS,NL,NS)=(  65, 265,  20,  20) K=    10.27847 E/DN RDN=     0.30297 DN
AREA  17 (SL,SS,NL,NS)=(  65, 315,  20,  20) K=     9.89986 E/DN RDN=     0.47651 DN
AREA  18 (SL,SS,NL,NS)=(  65, 365,  20,  20) K=    10.60149 E/DN RDN=     0.88050 DN
AREA  19 (SL,SS,NL,NS)=(  65, 415,  20,  20) K=    10.34882 E/DN RDN=     0.73474 DN
AREA  20 (SL,SS,NL,NS)=(  65, 465,  20,  20) K=    10.58272 E/DN RDN=     0.45417 DN
AREA  21 (SL,SS,NL,NS)=( 115,  15,  20,  20) K=    11.42757 E/DN RDN=     0.85727 DN
AREA  22 (SL,SS,NL,NS)=( 115,  65,  20,  20) K=     9.65102 E/DN RDN=    -0.14995 DN
AREA  23 (SL,SS,NL,NS)=( 115, 115,  20,  20) K=     9.21765 E/DN RDN=    -0.55244 DN
AREA  24 (SL,SS,NL,NS)=( 115, 165,  20,  20) K=     9.42420 E/DN RDN=    -0.19428 DN
AREA  25 (SL,SS,NL,NS)=( 115, 215,  20,  20) K=     9.20208 E/DN RDN=    -0.33420 DN
AREA  26 (SL,SS,NL,NS)=( 115, 265,  20,  20) K=    10.07218 E/DN RDN=    -0.56563 DN
AREA  27 (SL,SS,NL,NS)=( 115, 315,  20,  20) K=     8.99965 E/DN RDN=    -0.63568 DN
AREA  28 (SL,SS,NL,NS)=( 115, 365,  20,  20) K=     9.33415 E/DN RDN=    -0.65101 DN
AREA  29 (SL,SS,NL,NS)=( 115, 415,  20,  20) K=     9.33027 E/DN RDN=    -0.31300 DN
AREA  30 (SL,SS,NL,NS)=( 115, 465,  20,  20) K=     9.37539 E/DN RDN=     0.28048 DN
AREA  31 (SL,SS,NL,NS)=( 165,  15,  20,  20) K=    10.18426 E/DN RDN=     0.45960 DN
AREA  32 (SL,SS,NL,NS)=( 165,  65,  20,  20) K=     9.80777 E/DN RDN=    -0.62880 DN
AREA  33 (SL,SS,NL,NS)=( 165, 115,  20,  20) K=     9.46085 E/DN RDN=    -0.41605 DN
AREA  34 (SL,SS,NL,NS)=( 165, 165,  20,  20) K=     9.94927 E/DN RDN=     0.56484 DN
AREA  35 (SL,SS,NL,NS)=( 165, 215,  20,  20) K=    10.05241 E/DN RDN=     0.18650 DN
AREA  36 (SL,SS,NL,NS)=( 165, 265,  20,  20) K=     9.38524 E/DN RDN=    -0.00774 DN
AREA  37 (SL,SS,NL,NS)=( 165, 315,  20,  20) K=    11.86728 E/DN RDN=     0.82450 DN
AREA  38 (SL,SS,NL,NS)=( 165, 365,  20,  20) K=     9.39617 E/DN RDN=     0.52621 DN
AREA  39 (SL,SS,NL,NS)=( 165, 415,  20,  20) K=     9.37466 E/DN RDN=     0.37411 DN
AREA  40 (SL,SS,NL,NS)=( 165, 465,  20,  20) K=    10.77020 E/DN RDN=     0.86076 DN
AREA  41 (SL,SS,NL,NS)=( 215,  15,  20,  20) K=     9.45240 E/DN RDN=    -0.35441 DN
AREA  42 (SL,SS,NL,NS)=( 215,  65,  20,  20) K=    10.50862 E/DN RDN=     0.77067 DN
AREA  43 (SL,SS,NL,NS)=( 215, 115,  20,  20) K=    10.40969 E/DN RDN=    -0.03484 DN
AREA  44 (SL,SS,NL,NS)=( 215, 165,  20,  20) K=     9.76946 E/DN RDN=     0.58446 DN
AREA  45 (SL,SS,NL,NS)=( 215, 215,  20,  20) K=    10.41095 E/DN RDN=     0.42936 DN
AREA  46 (SL,SS,NL,NS)=( 215, 265,  20,  20) K=    10.28357 E/DN RDN=     0.59148 DN
AREA  47 (SL,SS,NL,NS)=( 215, 315,  20,  20) K=     9.86580 E/DN RDN=    -0.08678 DN
AREA  48 (SL,SS,NL,NS)=( 215, 365,  20,  20) K=     9.24523 E/DN RDN=    -0.37095 DN
AREA  49 (SL,SS,NL,NS)=( 215, 415,  20,  20) K=    10.01096 E/DN RDN=     0.87833 DN
AREA  50 (SL,SS,NL,NS)=( 215, 465,  20,  20) K=     9.98657 E/DN RDN=    -0.32773 DN
AREA  51 (SL,SS,NL,NS)=( 265,  15,  20,  20) K=     9.09666 E/DN RDN=    -0.81045 DN
AREA  52 (SL,SS,NL,NS)=( 265,  65,  20,  20) K=     9.83588 E/DN RDN=    -0.13778 DN
AREA  53 (SL,SS,NL,NS)=( 265, 115,  20,  20) K=    10.40543 E/DN RDN=     0.46613 DN
AREA  54 (SL,SS,NL,NS)=( 265, 165,  20,  20) K=    10.68326 E/DN RDN=     0.46152 DN
AREA  55 (SL,SS,NL,NS)=( 265, 215,  20,  20) K=    10.39200 E/DN RDN=    -0.29464 DN
AREA  56 (SL,SS,NL,NS)=( 265, 265,  20,  20) K=    10.67752 E/DN RDN=     0.67597 DN
AREA  57 (SL,SS,NL,NS)=( 265, 315,  20,  20) K=    10.88648 E/DN RDN=     0.66295 DN
AREA  58 (SL,SS,NL,NS)=( 265, 365,  20,  20) K=     8.80985 E/DN RDN=    -0.38740 DN
AREA  59 (SL,SS,NL,NS)=( 265, 415,  20,  20) K=    10.24336 E/DN RDN=     0.00376 DN
AREA  60 (SL,SS,NL,NS)=( 265, 465,  20,  20) K=     9.74251 E/DN RDN=     0.37287 DN
AREA  61 (SL,SS,NL,NS)=( 315,  15,  20,  20) K=     9.92099 E/DN RDN=    -0.30064 DN
AREA  62 (SL,SS,NL,NS)=( 315,  65,  20,  20) K=    10.36872 E/DN RDN=     0.85959 DN
AREA  63 (SL,SS,NL,NS)=( 315, 115,  20,  20) K=     8.90701 E/DN RDN=    -0.78957 DN
AREA  64 (SL,SS,NL,NS)=( 315, 165,  20,  20) K=    10.20292 E/DN RDN=     0.70946 DN
AREA  65 (SL,SS,NL,NS)=( 315, 215,  20,  20) K=     9.11692 E/DN RDN=    -1.00175 DN
AREA  66 (SL,SS,NL,NS)=( 315, 265,  20,  20) K=     9.78322 E/DN RDN=    -0.53489 DN
AREA  67 (SL,SS,NL,NS)=( 315, 315,  20,  20) K=    10.11680 E/DN RDN=     0.15311 DN
AREA  68 (SL,SS,NL,NS)=( 315, 365,  20,  20) K=     9.57502 E/DN RDN=     0.47486 DN
AREA  69 (SL,SS,NL,NS)=( 315, 415,  20,  20) K=    11.36624 E/DN RDN=     0.44566 DN
AREA  70 (SL,SS,NL,NS)=( 315, 465,  20,  20) K=     8.72955 E/DN RDN=    -0.59912 DN
AREA  71 (SL,SS,NL,NS)=( 365,  15,  20,  20) K=     9.38370 E/DN RDN=    -0.78727 DN
AREA  72 (SL,SS,NL,NS)=( 365,  65,  20,  20) K=     9.71555 E/DN RDN=    -0.55521 DN
AREA  73 (SL,SS,NL,NS)=( 365, 115,  20,  20) K=    10.07417 E/DN RDN=    -0.07161 DN
AREA  74 (SL,SS,NL,NS)=( 365, 165,  20,  20) K=    10.18708 E/DN RDN=     0.60991 DN
AREA  75 (SL,SS,NL,NS)=( 365, 215,  20,  20) K=     9.51882 E/DN RDN=     0.41943 DN
AREA  76 (SL,SS,NL,NS)=( 365, 265,  20,  20) K=    10.09547 E/DN RDN=    -0.20599 DN
AREA  77 (SL,SS,NL,NS)=( 365, 315,  20,  20) K=     9.25589 E/DN RDN=    -0.33873 DN
AREA  78 (SL,SS,NL,NS)=( 365, 365,  20,  20) K=     9.28078 E/DN RDN=    -0.24020 DN
AREA  79 (SL,SS,NL,NS)=( 365, 415,  20,  20) K=    11.47499 E/DN RDN=     1.00377 DN
AREA  80 (SL,SS,NL,NS)=( 365, 465,  20,  20) K=    10.55617 E/DN RDN=     0.50581 DN
AREA  81 (SL,SS,NL,NS)=( 415,  15,  20,  20) K=     9.64604 E/DN RDN=     0.73388 DN
AREA  82 (SL,SS,NL,NS)=( 415,  65,  20,  20) K=     9.94912 E/DN RDN=    -0.13542 DN
AREA  83 (SL,SS,NL,NS)=( 415, 115,  20,  20) K=     9.64859 E/DN RDN=     0.49659 DN
AREA  84 (SL,SS,NL,NS)=( 415, 165,  20,  20) K=     9.99765 E/DN RDN=    -0.41223 DN
AREA  85 (SL,SS,NL,NS)=( 415, 215,  20,  20) K=     9.47067 E/DN RDN=     0.31530 DN
AREA  86 (SL,SS,NL,NS)=( 415, 265,  20,  20) K=     9.84201 E/DN RDN=     0.76710 DN
AREA  87 (SL,SS,NL,NS)=( 415, 315,  20,  20) K=     9.34843 E/DN RDN=     0.46536 DN
AREA  88 (SL,SS,NL,NS)=( 415, 365,  20,  20) K=     8.77626 E/DN RDN=    -0.61075 DN
AREA  89 (SL,SS,NL,NS)=( 415, 415,  20,  20) K=     9.72006 E/DN RDN=    -0.30946 DN
AREA  90 (SL,SS,NL,NS)=( 415, 465,  20,  20) K=     9.50211 E/DN RDN=     0.57434 DN
AREA  91 (SL,SS,NL,NS)=( 465,  15,  20,  20) K=    10.31689 E/DN RDN=     0.97256 DN
AREA  92 (SL,SS,NL,NS)=( 465,  65,  20,  20) K=     9.99379 E/DN RDN=     0.37400 DN
AREA  93 (SL,SS,NL,NS)=( 465, 115,  20,  20) K=     9.41280 E/DN RDN=    -0.16952 DN
AREA  94 (SL,SS,NL,NS)=( 465, 165,  20,  20) K=     9.66141 E/DN RDN=     0.42355 DN
AREA  95 (SL,SS,NL,NS)=( 465, 215,  20,  20) K=     9.40913 E/DN RDN=    -0.25293 DN
AREA  96 (SL,SS,NL,NS)=( 465, 265,  20,  20) K=    10.15835 E/DN RDN=    -0.09245 DN
AREA  97 (SL,SS,NL,NS)=( 465, 315,  20,  20) K=    10.31491 E/DN RDN=     0.38093 DN
AREA  98 (SL,SS,NL,NS)=( 465, 365,  20,  20) K=     9.78226 E/DN RDN=    -0.14110 DN
AREA  99 (SL,SS,NL,NS)=( 465, 415,  20,  20) K=    11.68719 E/DN RDN=     0.78994 DN
AREA 100 (SL,SS,NL,NS)=( 465, 465,  20,  20) K=    10.46812 E/DN RDN=    -0.03306 DN

Global value for K...
Raw mean and sigma are...
N= 100 MEAN=     9.94313 SIGMA=     0.64317
After throwing out samples differing by 2 sigma
N=  94 MEAN=     9.83853 SIGMA=     0.50575

Global noise floor...
Raw mean and sigma are...
N= 100 MEAN=     0.12105 SIGMA=     0.54112
After throwing out samples differing by 2 sigma
N=  99 MEAN=     0.13239 SIGMA=     0.53189

AREA  15 (SL,SS,NL,NS)=(  65, 215,  20,  20)  *****BAD SYSTEM GAIN*****
AREA  21 (SL,SS,NL,NS)=( 115,  15,  20,  20)  *****BAD SYSTEM GAIN*****
AREA  37 (SL,SS,NL,NS)=( 165, 315,  20,  20)  *****BAD SYSTEM GAIN*****
AREA  65 (SL,SS,NL,NS)=( 315, 215,  20,  20)  *****BAD NOISE FLOOR*****
AREA  69 (SL,SS,NL,NS)=( 315, 415,  20,  20)  *****BAD SYSTEM GAIN*****
AREA  79 (SL,SS,NL,NS)=( 365, 415,  20,  20)  *****BAD SYSTEM GAIN*****
AREA  99 (SL,SS,NL,NS)=( 465, 415,  20,  20)  *****BAD SYSTEM GAIN*****

NUMBER REJECTED FOR  NOISE OR GAIN=     7
NUMBER OF GOOD AREAS=  93 OUT OF 100 AREAS SAMPLED
K=     9.84629 E/DN RDN=     0.08679 DN


UPPER-LEFT  CORNER  K=    9.8 E/DN  RDN= -0.088 DN  NAREA= 14
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9821     1.06523    0.99922    0.02744    1.01225    1.05234
     10.00000   100.0148     3.14207    2.00006    0.49722    3.19308    0.98402
     20.00000  1000.0311    10.08960    3.00001    1.00387   10.09334    0.99963

UPPER-RIGHT CORNER  K=   10.0 E/DN  RDN=  0.377 DN  NAREA= 15
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9930     1.06094    0.99970    0.02569    1.07052    0.99104
     10.00000   100.0158     3.19552    2.00007    0.50454    3.19201    1.00110
     20.00000  1000.0276    10.02965    3.00001    1.00129   10.02975    0.99999

LOWER-LEFT  CORNER  K=    9.8 E/DN  RDN= -0.002 DN  NAREA= 15
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9698     1.05697    0.99869    0.02406    1.01028    1.04621
     10.00000    99.9750     3.18195    1.99989    0.50269    3.19921    0.99461
     20.00000  1000.0966    10.11893    3.00004    1.00513   10.11853    1.00004

LOWER-RIGHT CORNER  K=    9.7 E/DN  RDN= -0.024 DN  NAREA= 12
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9773     1.06241    0.99901    0.02629    1.01569    1.04600
     10.00000    99.9856     3.18982    1.99994    0.50377    3.21452    0.99232
     20.00000  1000.0182    10.16516    3.00001    1.00711   10.16576    0.99994

CENTER              K=    9.9 E/DN  RDN=  0.307 DN  NAREA= 37
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000    10.0103     1.06869    1.00045    0.02885    1.05306    1.01484
     10.00000    99.9781     3.19219    1.99991    0.50409    3.19790    0.99822
     20.00000   999.8723    10.07116    2.99994    1.00308   10.07099    1.00002
CCDNOISE task completed
if (mode = "nobatch" or mode = "inter")
    ush gnuplot gain5.gpi
end-if
ccdnoise ltf1.dat out=mark.dat table=gain6.tbl plot=gain6
Beginning VICAR task ccdnoise
CCDNOISE version 13-Jul-2013
----TASK:-GAUSNOIS---USER: lwk         Thu Oct 10 12:27:37 2013
----TASK:-LTGEN   ---USER: lwk         Thu Oct 10 12:27:39 2013
AREA   1 (SL,SS,NL,NS)=(  15,  15,  20,  20) K=    10.30742 E/DN RDN=     0.62024 DN
AREA   2 (SL,SS,NL,NS)=(  15,  65,  20,  20) K=     9.93566 E/DN RDN=     0.54951 DN
AREA   3 (SL,SS,NL,NS)=(  15, 115,  20,  20) K=     9.79330 E/DN RDN=    -0.87523 DN
AREA   4 (SL,SS,NL,NS)=(  15, 165,  20,  20) K=     9.83507 E/DN RDN=    -0.49816 DN
AREA   5 (SL,SS,NL,NS)=(  15, 215,  20,  20) K=     9.23651 E/DN RDN=    -0.73807 DN
AREA   6 (SL,SS,NL,NS)=(  15, 265,  20,  20) K=    10.10108 E/DN RDN=    -0.10256 DN
AREA   7 (SL,SS,NL,NS)=(  15, 315,  20,  20) K=     9.33507 E/DN RDN=    -0.80116 DN
AREA   8 (SL,SS,NL,NS)=(  15, 365,  20,  20) K=    10.44968 E/DN RDN=     0.74109 DN
AREA   9 (SL,SS,NL,NS)=(  15, 415,  20,  20) K=     9.63899 E/DN RDN=     0.69265 DN
AREA  10 (SL,SS,NL,NS)=(  15, 465,  20,  20) K=    10.40750 E/DN RDN=    -0.35611 DN
AREA  11 (SL,SS,NL,NS)=(  65,  15,  20,  20) K=     9.25803 E/DN RDN=    -0.32982 DN
AREA  12 (SL,SS,NL,NS)=(  65,  65,  20,  20) K=     9.80003 E/DN RDN=     0.32338 DN
AREA  13 (SL,SS,NL,NS)=(  65, 115,  20,  20) K=    10.14556 E/DN RDN=     0.69539 DN
AREA  14 (SL,SS,NL,NS)=(  65, 165,  20,  20) K=    10.95564 E/DN RDN=     0.74933 DN
AREA  15 (SL,SS,NL,NS)=(  65, 215,  20,  20) K=    11.66815 E/DN RDN=     1.11472 DN
AREA  16 (SL,SS,NL,NS)=(  65, 265,  20,  20) K=    10.27847 E/DN RDN=     0.30297 DN
AREA  17 (SL,SS,NL,NS)=(  65, 315,  20,  20) K=     9.89986 E/DN RDN=     0.47651 DN
AREA  18 (SL,SS,NL,NS)=(  65, 365,  20,  20) K=    10.60149 E/DN RDN=     0.88050 DN
AREA  19 (SL,SS,NL,NS)=(  65, 415,  20,  20) K=    10.34882 E/DN RDN=     0.73474 DN
AREA  20 (SL,SS,NL,NS)=(  65, 465,  20,  20) K=    10.58272 E/DN RDN=     0.45417 DN
AREA  21 (SL,SS,NL,NS)=( 115,  15,  20,  20) K=    11.42757 E/DN RDN=     0.85727 DN
AREA  22 (SL,SS,NL,NS)=( 115,  65,  20,  20) K=     9.65102 E/DN RDN=    -0.14995 DN
AREA  23 (SL,SS,NL,NS)=( 115, 115,  20,  20) K=     9.21765 E/DN RDN=    -0.55244 DN
AREA  24 (SL,SS,NL,NS)=( 115, 165,  20,  20) K=     9.42420 E/DN RDN=    -0.19428 DN
AREA  25 (SL,SS,NL,NS)=( 115, 215,  20,  20) K=     9.20208 E/DN RDN=    -0.33420 DN
AREA  26 (SL,SS,NL,NS)=( 115, 265,  20,  20) K=    10.07218 E/DN RDN=    -0.56563 DN
AREA  27 (SL,SS,NL,NS)=( 115, 315,  20,  20) K=     8.99965 E/DN RDN=    -0.63568 DN
AREA  28 (SL,SS,NL,NS)=( 115, 365,  20,  20) K=     9.33415 E/DN RDN=    -0.65101 DN
AREA  29 (SL,SS,NL,NS)=( 115, 415,  20,  20) K=     9.33027 E/DN RDN=    -0.31300 DN
AREA  30 (SL,SS,NL,NS)=( 115, 465,  20,  20) K=     9.37539 E/DN RDN=     0.28048 DN
AREA  31 (SL,SS,NL,NS)=( 165,  15,  20,  20) K=    10.18426 E/DN RDN=     0.45960 DN
AREA  32 (SL,SS,NL,NS)=( 165,  65,  20,  20) K=     9.80777 E/DN RDN=    -0.62880 DN
AREA  33 (SL,SS,NL,NS)=( 165, 115,  20,  20) K=     9.46085 E/DN RDN=    -0.41605 DN
AREA  34 (SL,SS,NL,NS)=( 165, 165,  20,  20) K=     9.94927 E/DN RDN=     0.56484 DN
AREA  35 (SL,SS,NL,NS)=( 165, 215,  20,  20) K=    10.05241 E/DN RDN=     0.18650 DN
AREA  36 (SL,SS,NL,NS)=( 165, 265,  20,  20) K=     9.38524 E/DN RDN=    -0.00774 DN
AREA  37 (SL,SS,NL,NS)=( 165, 315,  20,  20) K=    11.86728 E/DN RDN=     0.82450 DN
AREA  38 (SL,SS,NL,NS)=( 165, 365,  20,  20) K=     9.39617 E/DN RDN=     0.52621 DN
AREA  39 (SL,SS,NL,NS)=( 165, 415,  20,  20) K=     9.37466 E/DN RDN=     0.37411 DN
AREA  40 (SL,SS,NL,NS)=( 165, 465,  20,  20) K=    10.77020 E/DN RDN=     0.86076 DN
AREA  41 (SL,SS,NL,NS)=( 215,  15,  20,  20) K=     9.45240 E/DN RDN=    -0.35441 DN
AREA  42 (SL,SS,NL,NS)=( 215,  65,  20,  20) K=    10.50862 E/DN RDN=     0.77067 DN
AREA  43 (SL,SS,NL,NS)=( 215, 115,  20,  20) K=    10.40969 E/DN RDN=    -0.03484 DN
AREA  44 (SL,SS,NL,NS)=( 215, 165,  20,  20) K=     9.76946 E/DN RDN=     0.58446 DN
AREA  45 (SL,SS,NL,NS)=( 215, 215,  20,  20) K=    10.41095 E/DN RDN=     0.42936 DN
AREA  46 (SL,SS,NL,NS)=( 215, 265,  20,  20) K=    10.28357 E/DN RDN=     0.59148 DN
AREA  47 (SL,SS,NL,NS)=( 215, 315,  20,  20) K=     9.86580 E/DN RDN=    -0.08678 DN
AREA  48 (SL,SS,NL,NS)=( 215, 365,  20,  20) K=     9.24523 E/DN RDN=    -0.37095 DN
AREA  49 (SL,SS,NL,NS)=( 215, 415,  20,  20) K=    10.01096 E/DN RDN=     0.87833 DN
AREA  50 (SL,SS,NL,NS)=( 215, 465,  20,  20) K=     9.98657 E/DN RDN=    -0.32773 DN
AREA  51 (SL,SS,NL,NS)=( 265,  15,  20,  20) K=     9.09666 E/DN RDN=    -0.81045 DN
AREA  52 (SL,SS,NL,NS)=( 265,  65,  20,  20) K=     9.83588 E/DN RDN=    -0.13778 DN
AREA  53 (SL,SS,NL,NS)=( 265, 115,  20,  20) K=    10.40543 E/DN RDN=     0.46613 DN
AREA  54 (SL,SS,NL,NS)=( 265, 165,  20,  20) K=    10.68326 E/DN RDN=     0.46152 DN
AREA  55 (SL,SS,NL,NS)=( 265, 215,  20,  20) K=    10.39200 E/DN RDN=    -0.29464 DN
AREA  56 (SL,SS,NL,NS)=( 265, 265,  20,  20) K=    10.67752 E/DN RDN=     0.67597 DN
AREA  57 (SL,SS,NL,NS)=( 265, 315,  20,  20) K=    10.88648 E/DN RDN=     0.66295 DN
AREA  58 (SL,SS,NL,NS)=( 265, 365,  20,  20) K=     8.80985 E/DN RDN=    -0.38740 DN
AREA  59 (SL,SS,NL,NS)=( 265, 415,  20,  20) K=    10.24336 E/DN RDN=     0.00376 DN
AREA  60 (SL,SS,NL,NS)=( 265, 465,  20,  20) K=     9.74251 E/DN RDN=     0.37287 DN
AREA  61 (SL,SS,NL,NS)=( 315,  15,  20,  20) K=     9.92099 E/DN RDN=    -0.30064 DN
AREA  62 (SL,SS,NL,NS)=( 315,  65,  20,  20) K=    10.36872 E/DN RDN=     0.85959 DN
AREA  63 (SL,SS,NL,NS)=( 315, 115,  20,  20) K=     8.90701 E/DN RDN=    -0.78957 DN
AREA  64 (SL,SS,NL,NS)=( 315, 165,  20,  20) K=    10.20292 E/DN RDN=     0.70946 DN
AREA  65 (SL,SS,NL,NS)=( 315, 215,  20,  20) K=     9.11692 E/DN RDN=    -1.00175 DN
AREA  66 (SL,SS,NL,NS)=( 315, 265,  20,  20) K=     9.78322 E/DN RDN=    -0.53489 DN
AREA  67 (SL,SS,NL,NS)=( 315, 315,  20,  20) K=    10.11680 E/DN RDN=     0.15311 DN
AREA  68 (SL,SS,NL,NS)=( 315, 365,  20,  20) K=     9.57502 E/DN RDN=     0.47486 DN
AREA  69 (SL,SS,NL,NS)=( 315, 415,  20,  20) K=    11.36624 E/DN RDN=     0.44566 DN
AREA  70 (SL,SS,NL,NS)=( 315, 465,  20,  20) K=     8.72955 E/DN RDN=    -0.59912 DN
AREA  71 (SL,SS,NL,NS)=( 365,  15,  20,  20) K=     9.38370 E/DN RDN=    -0.78727 DN
AREA  72 (SL,SS,NL,NS)=( 365,  65,  20,  20) K=     9.71555 E/DN RDN=    -0.55521 DN
AREA  73 (SL,SS,NL,NS)=( 365, 115,  20,  20) K=    10.07417 E/DN RDN=    -0.07161 DN
AREA  74 (SL,SS,NL,NS)=( 365, 165,  20,  20) K=    10.18708 E/DN RDN=     0.60991 DN
AREA  75 (SL,SS,NL,NS)=( 365, 215,  20,  20) K=     9.51882 E/DN RDN=     0.41943 DN
AREA  76 (SL,SS,NL,NS)=( 365, 265,  20,  20) K=    10.09547 E/DN RDN=    -0.20599 DN
AREA  77 (SL,SS,NL,NS)=( 365, 315,  20,  20) K=     9.25589 E/DN RDN=    -0.33873 DN
AREA  78 (SL,SS,NL,NS)=( 365, 365,  20,  20) K=     9.28078 E/DN RDN=    -0.24020 DN
AREA  79 (SL,SS,NL,NS)=( 365, 415,  20,  20) K=    11.47499 E/DN RDN=     1.00377 DN
AREA  80 (SL,SS,NL,NS)=( 365, 465,  20,  20) K=    10.55617 E/DN RDN=     0.50581 DN
AREA  81 (SL,SS,NL,NS)=( 415,  15,  20,  20) K=     9.64604 E/DN RDN=     0.73388 DN
AREA  82 (SL,SS,NL,NS)=( 415,  65,  20,  20) K=     9.94912 E/DN RDN=    -0.13542 DN
AREA  83 (SL,SS,NL,NS)=( 415, 115,  20,  20) K=     9.64859 E/DN RDN=     0.49659 DN
AREA  84 (SL,SS,NL,NS)=( 415, 165,  20,  20) K=     9.99765 E/DN RDN=    -0.41223 DN
AREA  85 (SL,SS,NL,NS)=( 415, 215,  20,  20) K=     9.47067 E/DN RDN=     0.31530 DN
AREA  86 (SL,SS,NL,NS)=( 415, 265,  20,  20) K=     9.84201 E/DN RDN=     0.76710 DN
AREA  87 (SL,SS,NL,NS)=( 415, 315,  20,  20) K=     9.34843 E/DN RDN=     0.46536 DN
AREA  88 (SL,SS,NL,NS)=( 415, 365,  20,  20) K=     8.77626 E/DN RDN=    -0.61075 DN
AREA  89 (SL,SS,NL,NS)=( 415, 415,  20,  20) K=     9.72006 E/DN RDN=    -0.30946 DN
AREA  90 (SL,SS,NL,NS)=( 415, 465,  20,  20) K=     9.50211 E/DN RDN=     0.57434 DN
AREA  91 (SL,SS,NL,NS)=( 465,  15,  20,  20) K=    10.31689 E/DN RDN=     0.97256 DN
AREA  92 (SL,SS,NL,NS)=( 465,  65,  20,  20) K=     9.99379 E/DN RDN=     0.37400 DN
AREA  93 (SL,SS,NL,NS)=( 465, 115,  20,  20) K=     9.41280 E/DN RDN=    -0.16952 DN
AREA  94 (SL,SS,NL,NS)=( 465, 165,  20,  20) K=     9.66141 E/DN RDN=     0.42355 DN
AREA  95 (SL,SS,NL,NS)=( 465, 215,  20,  20) K=     9.40913 E/DN RDN=    -0.25293 DN
AREA  96 (SL,SS,NL,NS)=( 465, 265,  20,  20) K=    10.15835 E/DN RDN=    -0.09245 DN
AREA  97 (SL,SS,NL,NS)=( 465, 315,  20,  20) K=    10.31491 E/DN RDN=     0.38093 DN
AREA  98 (SL,SS,NL,NS)=( 465, 365,  20,  20) K=     9.78226 E/DN RDN=    -0.14110 DN
AREA  99 (SL,SS,NL,NS)=( 465, 415,  20,  20) K=    11.68719 E/DN RDN=     0.78994 DN
AREA 100 (SL,SS,NL,NS)=( 465, 465,  20,  20) K=    10.46812 E/DN RDN=    -0.03306 DN

Global value for K...
Raw mean and sigma are...
N= 100 MEAN=     9.94313 SIGMA=     0.64317
After throwing out samples differing by 2 sigma
N=  94 MEAN=     9.83853 SIGMA=     0.50575

Global noise floor...
Raw mean and sigma are...
N= 100 MEAN=     0.12105 SIGMA=     0.54112
After throwing out samples differing by 2 sigma
N=  99 MEAN=     0.13239 SIGMA=     0.53189

AREA  15 (SL,SS,NL,NS)=(  65, 215,  20,  20)  *****BAD SYSTEM GAIN*****
AREA  21 (SL,SS,NL,NS)=( 115,  15,  20,  20)  *****BAD SYSTEM GAIN*****
AREA  37 (SL,SS,NL,NS)=( 165, 315,  20,  20)  *****BAD SYSTEM GAIN*****
AREA  65 (SL,SS,NL,NS)=( 315, 215,  20,  20)  *****BAD NOISE FLOOR*****
AREA  69 (SL,SS,NL,NS)=( 315, 415,  20,  20)  *****BAD SYSTEM GAIN*****
AREA  79 (SL,SS,NL,NS)=( 365, 415,  20,  20)  *****BAD SYSTEM GAIN*****
AREA  99 (SL,SS,NL,NS)=( 465, 415,  20,  20)  *****BAD SYSTEM GAIN*****

NUMBER REJECTED FOR  NOISE OR GAIN=     7
NUMBER OF GOOD AREAS=  93 OUT OF 100 AREAS SAMPLED
K=     9.84629 E/DN RDN=     0.08679 DN


UPPER-LEFT  CORNER  K=    9.8 E/DN  RDN= -0.088 DN  NAREA= 14
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9821     1.06523    0.99922    0.02744    1.01225    1.05234
     10.00000   100.0148     3.14207    2.00006    0.49722    3.19308    0.98402
     20.00000  1000.0311    10.08960    3.00001    1.00387   10.09334    0.99963

UPPER-RIGHT CORNER  K=   10.0 E/DN  RDN=  0.377 DN  NAREA= 15
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9930     1.06094    0.99970    0.02569    1.07052    0.99104
     10.00000   100.0158     3.19552    2.00007    0.50454    3.19201    1.00110
     20.00000  1000.0276    10.02965    3.00001    1.00129   10.02975    0.99999

LOWER-LEFT  CORNER  K=    9.8 E/DN  RDN= -0.002 DN  NAREA= 15
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9698     1.05697    0.99869    0.02406    1.01028    1.04621
     10.00000    99.9750     3.18195    1.99989    0.50269    3.19921    0.99461
     20.00000  1000.0966    10.11893    3.00004    1.00513   10.11853    1.00004

LOWER-RIGHT CORNER  K=    9.7 E/DN  RDN= -0.024 DN  NAREA= 12
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9773     1.06241    0.99901    0.02629    1.01569    1.04600
     10.00000    99.9856     3.18982    1.99994    0.50377    3.21452    0.99232
     20.00000  1000.0182    10.16516    3.00001    1.00711   10.16576    0.99994

CENTER              K=    9.9 E/DN  RDN=  0.307 DN  NAREA= 37
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000    10.0103     1.06869    1.00045    0.02885    1.05306    1.01484
     10.00000    99.9781     3.19219    1.99991    0.50409    3.19790    0.99822
     20.00000   999.8723    10.07116    2.99994    1.00308   10.07099    1.00002
CCDNOISE task completed
if (mode = "nobatch" or mode = "inter")
    ush gnuplot gain6.gpi
end-if
ccdnoise ltf1.dat  table=gain7.tbl plot=gain7  +
    plotfmt=eps
Beginning VICAR task ccdnoise
CCDNOISE version 13-Jul-2013
----TASK:-GAUSNOIS---USER: lwk         Thu Oct 10 12:27:37 2013
----TASK:-LTGEN   ---USER: lwk         Thu Oct 10 12:27:39 2013
AREA   1 (SL,SS,NL,NS)=(  15,  15,  20,  20) K=    10.30742 E/DN RDN=     0.62024 DN
AREA   2 (SL,SS,NL,NS)=(  15,  65,  20,  20) K=     9.93566 E/DN RDN=     0.54951 DN
AREA   3 (SL,SS,NL,NS)=(  15, 115,  20,  20) K=     9.79330 E/DN RDN=    -0.87523 DN
AREA   4 (SL,SS,NL,NS)=(  15, 165,  20,  20) K=     9.83507 E/DN RDN=    -0.49816 DN
AREA   5 (SL,SS,NL,NS)=(  15, 215,  20,  20) K=     9.23651 E/DN RDN=    -0.73807 DN
AREA   6 (SL,SS,NL,NS)=(  15, 265,  20,  20) K=    10.10108 E/DN RDN=    -0.10256 DN
AREA   7 (SL,SS,NL,NS)=(  15, 315,  20,  20) K=     9.33507 E/DN RDN=    -0.80116 DN
AREA   8 (SL,SS,NL,NS)=(  15, 365,  20,  20) K=    10.44968 E/DN RDN=     0.74109 DN
AREA   9 (SL,SS,NL,NS)=(  15, 415,  20,  20) K=     9.63899 E/DN RDN=     0.69265 DN
AREA  10 (SL,SS,NL,NS)=(  15, 465,  20,  20) K=    10.40750 E/DN RDN=    -0.35611 DN
AREA  11 (SL,SS,NL,NS)=(  65,  15,  20,  20) K=     9.25803 E/DN RDN=    -0.32982 DN
AREA  12 (SL,SS,NL,NS)=(  65,  65,  20,  20) K=     9.80003 E/DN RDN=     0.32338 DN
AREA  13 (SL,SS,NL,NS)=(  65, 115,  20,  20) K=    10.14556 E/DN RDN=     0.69539 DN
AREA  14 (SL,SS,NL,NS)=(  65, 165,  20,  20) K=    10.95564 E/DN RDN=     0.74933 DN
AREA  15 (SL,SS,NL,NS)=(  65, 215,  20,  20) K=    11.66815 E/DN RDN=     1.11472 DN
AREA  16 (SL,SS,NL,NS)=(  65, 265,  20,  20) K=    10.27847 E/DN RDN=     0.30297 DN
AREA  17 (SL,SS,NL,NS)=(  65, 315,  20,  20) K=     9.89986 E/DN RDN=     0.47651 DN
AREA  18 (SL,SS,NL,NS)=(  65, 365,  20,  20) K=    10.60149 E/DN RDN=     0.88050 DN
AREA  19 (SL,SS,NL,NS)=(  65, 415,  20,  20) K=    10.34882 E/DN RDN=     0.73474 DN
AREA  20 (SL,SS,NL,NS)=(  65, 465,  20,  20) K=    10.58272 E/DN RDN=     0.45417 DN
AREA  21 (SL,SS,NL,NS)=( 115,  15,  20,  20) K=    11.42757 E/DN RDN=     0.85727 DN
AREA  22 (SL,SS,NL,NS)=( 115,  65,  20,  20) K=     9.65102 E/DN RDN=    -0.14995 DN
AREA  23 (SL,SS,NL,NS)=( 115, 115,  20,  20) K=     9.21765 E/DN RDN=    -0.55244 DN
AREA  24 (SL,SS,NL,NS)=( 115, 165,  20,  20) K=     9.42420 E/DN RDN=    -0.19428 DN
AREA  25 (SL,SS,NL,NS)=( 115, 215,  20,  20) K=     9.20208 E/DN RDN=    -0.33420 DN
AREA  26 (SL,SS,NL,NS)=( 115, 265,  20,  20) K=    10.07218 E/DN RDN=    -0.56563 DN
AREA  27 (SL,SS,NL,NS)=( 115, 315,  20,  20) K=     8.99965 E/DN RDN=    -0.63568 DN
AREA  28 (SL,SS,NL,NS)=( 115, 365,  20,  20) K=     9.33415 E/DN RDN=    -0.65101 DN
AREA  29 (SL,SS,NL,NS)=( 115, 415,  20,  20) K=     9.33027 E/DN RDN=    -0.31300 DN
AREA  30 (SL,SS,NL,NS)=( 115, 465,  20,  20) K=     9.37539 E/DN RDN=     0.28048 DN
AREA  31 (SL,SS,NL,NS)=( 165,  15,  20,  20) K=    10.18426 E/DN RDN=     0.45960 DN
AREA  32 (SL,SS,NL,NS)=( 165,  65,  20,  20) K=     9.80777 E/DN RDN=    -0.62880 DN
AREA  33 (SL,SS,NL,NS)=( 165, 115,  20,  20) K=     9.46085 E/DN RDN=    -0.41605 DN
AREA  34 (SL,SS,NL,NS)=( 165, 165,  20,  20) K=     9.94927 E/DN RDN=     0.56484 DN
AREA  35 (SL,SS,NL,NS)=( 165, 215,  20,  20) K=    10.05241 E/DN RDN=     0.18650 DN
AREA  36 (SL,SS,NL,NS)=( 165, 265,  20,  20) K=     9.38524 E/DN RDN=    -0.00774 DN
AREA  37 (SL,SS,NL,NS)=( 165, 315,  20,  20) K=    11.86728 E/DN RDN=     0.82450 DN
AREA  38 (SL,SS,NL,NS)=( 165, 365,  20,  20) K=     9.39617 E/DN RDN=     0.52621 DN
AREA  39 (SL,SS,NL,NS)=( 165, 415,  20,  20) K=     9.37466 E/DN RDN=     0.37411 DN
AREA  40 (SL,SS,NL,NS)=( 165, 465,  20,  20) K=    10.77020 E/DN RDN=     0.86076 DN
AREA  41 (SL,SS,NL,NS)=( 215,  15,  20,  20) K=     9.45240 E/DN RDN=    -0.35441 DN
AREA  42 (SL,SS,NL,NS)=( 215,  65,  20,  20) K=    10.50862 E/DN RDN=     0.77067 DN
AREA  43 (SL,SS,NL,NS)=( 215, 115,  20,  20) K=    10.40969 E/DN RDN=    -0.03484 DN
AREA  44 (SL,SS,NL,NS)=( 215, 165,  20,  20) K=     9.76946 E/DN RDN=     0.58446 DN
AREA  45 (SL,SS,NL,NS)=( 215, 215,  20,  20) K=    10.41095 E/DN RDN=     0.42936 DN
AREA  46 (SL,SS,NL,NS)=( 215, 265,  20,  20) K=    10.28357 E/DN RDN=     0.59148 DN
AREA  47 (SL,SS,NL,NS)=( 215, 315,  20,  20) K=     9.86580 E/DN RDN=    -0.08678 DN
AREA  48 (SL,SS,NL,NS)=( 215, 365,  20,  20) K=     9.24523 E/DN RDN=    -0.37095 DN
AREA  49 (SL,SS,NL,NS)=( 215, 415,  20,  20) K=    10.01096 E/DN RDN=     0.87833 DN
AREA  50 (SL,SS,NL,NS)=( 215, 465,  20,  20) K=     9.98657 E/DN RDN=    -0.32773 DN
AREA  51 (SL,SS,NL,NS)=( 265,  15,  20,  20) K=     9.09666 E/DN RDN=    -0.81045 DN
AREA  52 (SL,SS,NL,NS)=( 265,  65,  20,  20) K=     9.83588 E/DN RDN=    -0.13778 DN
AREA  53 (SL,SS,NL,NS)=( 265, 115,  20,  20) K=    10.40543 E/DN RDN=     0.46613 DN
AREA  54 (SL,SS,NL,NS)=( 265, 165,  20,  20) K=    10.68326 E/DN RDN=     0.46152 DN
AREA  55 (SL,SS,NL,NS)=( 265, 215,  20,  20) K=    10.39200 E/DN RDN=    -0.29464 DN
AREA  56 (SL,SS,NL,NS)=( 265, 265,  20,  20) K=    10.67752 E/DN RDN=     0.67597 DN
AREA  57 (SL,SS,NL,NS)=( 265, 315,  20,  20) K=    10.88648 E/DN RDN=     0.66295 DN
AREA  58 (SL,SS,NL,NS)=( 265, 365,  20,  20) K=     8.80985 E/DN RDN=    -0.38740 DN
AREA  59 (SL,SS,NL,NS)=( 265, 415,  20,  20) K=    10.24336 E/DN RDN=     0.00376 DN
AREA  60 (SL,SS,NL,NS)=( 265, 465,  20,  20) K=     9.74251 E/DN RDN=     0.37287 DN
AREA  61 (SL,SS,NL,NS)=( 315,  15,  20,  20) K=     9.92099 E/DN RDN=    -0.30064 DN
AREA  62 (SL,SS,NL,NS)=( 315,  65,  20,  20) K=    10.36872 E/DN RDN=     0.85959 DN
AREA  63 (SL,SS,NL,NS)=( 315, 115,  20,  20) K=     8.90701 E/DN RDN=    -0.78957 DN
AREA  64 (SL,SS,NL,NS)=( 315, 165,  20,  20) K=    10.20292 E/DN RDN=     0.70946 DN
AREA  65 (SL,SS,NL,NS)=( 315, 215,  20,  20) K=     9.11692 E/DN RDN=    -1.00175 DN
AREA  66 (SL,SS,NL,NS)=( 315, 265,  20,  20) K=     9.78322 E/DN RDN=    -0.53489 DN
AREA  67 (SL,SS,NL,NS)=( 315, 315,  20,  20) K=    10.11680 E/DN RDN=     0.15311 DN
AREA  68 (SL,SS,NL,NS)=( 315, 365,  20,  20) K=     9.57502 E/DN RDN=     0.47486 DN
AREA  69 (SL,SS,NL,NS)=( 315, 415,  20,  20) K=    11.36624 E/DN RDN=     0.44566 DN
AREA  70 (SL,SS,NL,NS)=( 315, 465,  20,  20) K=     8.72955 E/DN RDN=    -0.59912 DN
AREA  71 (SL,SS,NL,NS)=( 365,  15,  20,  20) K=     9.38370 E/DN RDN=    -0.78727 DN
AREA  72 (SL,SS,NL,NS)=( 365,  65,  20,  20) K=     9.71555 E/DN RDN=    -0.55521 DN
AREA  73 (SL,SS,NL,NS)=( 365, 115,  20,  20) K=    10.07417 E/DN RDN=    -0.07161 DN
AREA  74 (SL,SS,NL,NS)=( 365, 165,  20,  20) K=    10.18708 E/DN RDN=     0.60991 DN
AREA  75 (SL,SS,NL,NS)=( 365, 215,  20,  20) K=     9.51882 E/DN RDN=     0.41943 DN
AREA  76 (SL,SS,NL,NS)=( 365, 265,  20,  20) K=    10.09547 E/DN RDN=    -0.20599 DN
AREA  77 (SL,SS,NL,NS)=( 365, 315,  20,  20) K=     9.25589 E/DN RDN=    -0.33873 DN
AREA  78 (SL,SS,NL,NS)=( 365, 365,  20,  20) K=     9.28078 E/DN RDN=    -0.24020 DN
AREA  79 (SL,SS,NL,NS)=( 365, 415,  20,  20) K=    11.47499 E/DN RDN=     1.00377 DN
AREA  80 (SL,SS,NL,NS)=( 365, 465,  20,  20) K=    10.55617 E/DN RDN=     0.50581 DN
AREA  81 (SL,SS,NL,NS)=( 415,  15,  20,  20) K=     9.64604 E/DN RDN=     0.73388 DN
AREA  82 (SL,SS,NL,NS)=( 415,  65,  20,  20) K=     9.94912 E/DN RDN=    -0.13542 DN
AREA  83 (SL,SS,NL,NS)=( 415, 115,  20,  20) K=     9.64859 E/DN RDN=     0.49659 DN
AREA  84 (SL,SS,NL,NS)=( 415, 165,  20,  20) K=     9.99765 E/DN RDN=    -0.41223 DN
AREA  85 (SL,SS,NL,NS)=( 415, 215,  20,  20) K=     9.47067 E/DN RDN=     0.31530 DN
AREA  86 (SL,SS,NL,NS)=( 415, 265,  20,  20) K=     9.84201 E/DN RDN=     0.76710 DN
AREA  87 (SL,SS,NL,NS)=( 415, 315,  20,  20) K=     9.34843 E/DN RDN=     0.46536 DN
AREA  88 (SL,SS,NL,NS)=( 415, 365,  20,  20) K=     8.77626 E/DN RDN=    -0.61075 DN
AREA  89 (SL,SS,NL,NS)=( 415, 415,  20,  20) K=     9.72006 E/DN RDN=    -0.30946 DN
AREA  90 (SL,SS,NL,NS)=( 415, 465,  20,  20) K=     9.50211 E/DN RDN=     0.57434 DN
AREA  91 (SL,SS,NL,NS)=( 465,  15,  20,  20) K=    10.31689 E/DN RDN=     0.97256 DN
AREA  92 (SL,SS,NL,NS)=( 465,  65,  20,  20) K=     9.99379 E/DN RDN=     0.37400 DN
AREA  93 (SL,SS,NL,NS)=( 465, 115,  20,  20) K=     9.41280 E/DN RDN=    -0.16952 DN
AREA  94 (SL,SS,NL,NS)=( 465, 165,  20,  20) K=     9.66141 E/DN RDN=     0.42355 DN
AREA  95 (SL,SS,NL,NS)=( 465, 215,  20,  20) K=     9.40913 E/DN RDN=    -0.25293 DN
AREA  96 (SL,SS,NL,NS)=( 465, 265,  20,  20) K=    10.15835 E/DN RDN=    -0.09245 DN
AREA  97 (SL,SS,NL,NS)=( 465, 315,  20,  20) K=    10.31491 E/DN RDN=     0.38093 DN
AREA  98 (SL,SS,NL,NS)=( 465, 365,  20,  20) K=     9.78226 E/DN RDN=    -0.14110 DN
AREA  99 (SL,SS,NL,NS)=( 465, 415,  20,  20) K=    11.68719 E/DN RDN=     0.78994 DN
AREA 100 (SL,SS,NL,NS)=( 465, 465,  20,  20) K=    10.46812 E/DN RDN=    -0.03306 DN

Global value for K...
Raw mean and sigma are...
N= 100 MEAN=     9.94313 SIGMA=     0.64317
After throwing out samples differing by 2 sigma
N=  94 MEAN=     9.83853 SIGMA=     0.50575

Global noise floor...
Raw mean and sigma are...
N= 100 MEAN=     0.12105 SIGMA=     0.54112
After throwing out samples differing by 2 sigma
N=  99 MEAN=     0.13239 SIGMA=     0.53189

AREA  15 (SL,SS,NL,NS)=(  65, 215,  20,  20)  *****BAD SYSTEM GAIN*****
AREA  21 (SL,SS,NL,NS)=( 115,  15,  20,  20)  *****BAD SYSTEM GAIN*****
AREA  37 (SL,SS,NL,NS)=( 165, 315,  20,  20)  *****BAD SYSTEM GAIN*****
AREA  65 (SL,SS,NL,NS)=( 315, 215,  20,  20)  *****BAD NOISE FLOOR*****
AREA  69 (SL,SS,NL,NS)=( 315, 415,  20,  20)  *****BAD SYSTEM GAIN*****
AREA  79 (SL,SS,NL,NS)=( 365, 415,  20,  20)  *****BAD SYSTEM GAIN*****
AREA  99 (SL,SS,NL,NS)=( 465, 415,  20,  20)  *****BAD SYSTEM GAIN*****

NUMBER REJECTED FOR  NOISE OR GAIN=     7
NUMBER OF GOOD AREAS=  93 OUT OF 100 AREAS SAMPLED
K=     9.84629 E/DN RDN=     0.08679 DN


UPPER-LEFT  CORNER  K=    9.8 E/DN  RDN= -0.088 DN  NAREA= 14
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9821     1.06523    0.99922    0.02744    1.01225    1.05234
     10.00000   100.0148     3.14207    2.00006    0.49722    3.19308    0.98402
     20.00000  1000.0311    10.08960    3.00001    1.00387   10.09334    0.99963

UPPER-RIGHT CORNER  K=   10.0 E/DN  RDN=  0.377 DN  NAREA= 15
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9930     1.06094    0.99970    0.02569    1.07052    0.99104
     10.00000   100.0158     3.19552    2.00007    0.50454    3.19201    1.00110
     20.00000  1000.0276    10.02965    3.00001    1.00129   10.02975    0.99999

LOWER-LEFT  CORNER  K=    9.8 E/DN  RDN= -0.002 DN  NAREA= 15
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9698     1.05697    0.99869    0.02406    1.01028    1.04621
     10.00000    99.9750     3.18195    1.99989    0.50269    3.19921    0.99461
     20.00000  1000.0966    10.11893    3.00004    1.00513   10.11853    1.00004

LOWER-RIGHT CORNER  K=    9.7 E/DN  RDN= -0.024 DN  NAREA= 12
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000     9.9773     1.06241    0.99901    0.02629    1.01569    1.04600
     10.00000    99.9856     3.18982    1.99994    0.50377    3.21452    0.99232
     20.00000  1000.0182    10.16516    3.00001    1.00711   10.16576    0.99994

CENTER              K=    9.9 E/DN  RDN=  0.307 DN  NAREA= 37
    EXPOSURE   MEAN       MEASURED   LOG           LOG         COMPUTED       RATIO
    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)    NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)
      0.00000    10.0103     1.06869    1.00045    0.02885    1.05306    1.01484
     10.00000    99.9781     3.19219    1.99991    0.50409    3.19790    0.99822
     20.00000   999.8723    10.07116    2.99994    1.00308   10.07099    1.00002
CCDNOISE task completed
ush gnuplot gain7.eps.gpi
   ush ls gain7.eps
ush rm gain*.gpi
ush rm gain*.eps
ush rm gain*.asc
ush rm gain*.tbl
ush rm a?.img
ush rm b?.img
ush rm d?.img
ush rm mark.dat
ush rm ltf1.dat
let $echo="no"
exit
slogoff
$ Return
$!#############################################################################
