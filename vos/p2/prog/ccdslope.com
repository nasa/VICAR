$!****************************************************************************
$!
$! Build proc for MIPL module ccdslope
$! VPACK Version 1.9, Monday, June 09, 2014, 16:37:00
$!
$! Execute by entering:		$ @ccdslope
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
$ write sys$output "*** module ccdslope ***"
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
$ write sys$output "Invalid argument given to ccdslope.com file -- ", primary
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
$   if F$SEARCH("ccdslope.imake") .nes. ""
$   then
$      vimake ccdslope
$      purge ccdslope.bld
$   else
$      if F$SEARCH("ccdslope.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake ccdslope
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @ccdslope.bld "STD"
$   else
$      @ccdslope.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create ccdslope.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack ccdslope.com -mixed -
	-s ccdslope.f -
	-i ccdslope.imake -
	-p ccdslope.pdf -
	-t tstccdslope.pdf tstccdslope_linux.log tstccdslope_sun.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ccdslope.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
C VICAR PROGRAM CCDSLOPE
C
C Radiometric calibration routine:  Determines light transfer curve
C slope and offset for a linear camera system.
C
c   UNITS
C	ISO	Shutter Offset
C	IUNI	INP
C	OUNI	OUT
C	12	TABLE_DS
C	13	gpi data file
C	97	gpi eps file
C	98	gpi file
      SUBROUTINE MAIN44
	implicit none
         COMMON/C2/UNITS,LUMMSG,RADMSG

         REAL*8 SDN,SDN2,SXDN,SX,SX2,DENOM,DC(400),EDC(400)
         REAL*8 DN(30,400),W(30,400),DNSUM,DLTX
	 REAL*8 ocol(30,6)
         REAL*4 LIGHT,DNS(6400),OUTM(2,2000),TOS
         REAL*4 EXPOS(30,400),SLOPE(400)
         REAL*4 OFFS(400),CEXPO(30),SHUTTER_OFFSETS(1024),BEXPO

         INTEGER*4 AREA(4,400), NI,CNTRL,I,IB,IBA,ICOUNT,IDEF
         INTEGER*4 SL,SS,SLI,SSI,OUNI,STAT,CNT,BAD(400),SIGTOL
         INTEGER*4 EFNUM,IDCSUB,IEDC,IND,IPLOT,IREJCT,ISO
ccc         INTEGER*4 STATUS
         INTEGER*4 IUNI,J,jj,K,L,LCNT,LOOP,MAXL,MAXS,N,NAREA,NEXP,NL
         INTEGER*4 IST,NLAB,NLI,NLO,NLUM,NPTS,NS,NSI,NSL,NSO
         INTEGER*4 nplotgpi,nplotgpi2,nploteps,nplotname,ntbl
	INTEGER*4 ninpfile,ncoly,jst,unit97,unit98
         LOGICAL*4 XVPTST,epsplot

	
        CHARACTER*63 plotname
        character*80 plotgpi,plotgpi2,ploteps,tbl

         CHARACTER*4320 LABEL
         CHARACTER*4 DIRECTN
         CHARACTER*92 MS1
         CHARACTER*14 VAL
         CHARACTER*256 PLOT_DS,OFFSET_FILE,inpfile
         CHARACTER*51 ARMES
         CHARACTER*41 RMSG
         CHARACTER*82 cbuf,MS2
         CHARACTER*40 MS3
         CHARACTER*62 MS4
         CHARACTER*40 LUMMSG
         CHARACTER*34 RADMSG
         CHARACTER*8  UNITS,pformat

	character*4 gpi/'.gpi'/,eps/'.eps'/,asc/'.asc'/

         CALL IFMESSAGE ('CCDSLOPE version 07-Jul-2013  (64-bit) - rjb')

         ARMES = 'NUMBER OF GOOD AREAS=**** OUT OF**** AREAS SAMPLED'
         RMSG = 'NUMBER REJECTED FOR               =     '
         MS3 = 'DELTA CORRECTION=****.***** ENERGY UNIT'

         MS1(1:41)=' AREA XXX (SL,SS,NL,NS)=(XXX,XXX,XXX,XXX)'
         MS1(42:92)=' '                
         MS2(1:58)=' '                
         MS4(1:49)='AREA    SL   SS   NL   NS  SLOPE (DN/ENERGY UNIT)'
         MS4(50:62)='  OFFSET (DN)'
         LUMMSG='ENERGY UNIT = FOOT-LAMBERT-MILLISECONDS'
         RADMSG='ENERGY UNIT = PICOAMP-MILLISECONDS'

        epsplot = .false.
        nplotgpi = 0
        nplotgpi2 = 0
        nplotname = 0
        nploteps = 0
        ntbl = 0
        ninpfile = 0

         CNTRL = 0

         IF (XVPTST('DELTAX')) CNTRL=1

        call xvparm('INP',cbuf,cnt,idef,1)
          inpfile = cbuf
          ninpfile=index(inpfile,'   ') - 1


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
		iplot = 1
ccc            CALL PLOTFN(PLOT_DS)
ccc            CALL XRTBEGIN(STATUS)
ccc            IF (STATUS.NE.1) CALL MABEND('Unable to OPEN plotter')
ccc            CALL DISPLAYAXES(1,1,0) ! x,y1,y2 axes displayed 1=yes 0=no
ccc            CALL SETACTIVESET(1)   ! endpts on lines 0=no triangles, 1=triangles
         ELSE
            IPLOT = 0
         ENDIF

         call xvp ('PLOTFMT',pformat,cnt)
         if (pformat .eq. 'eps' .or. pformat .eq. 'EPS') epsplot = .true.


         CALL XVP('REJECT',IREJCT,CNT)
         CALL XVP('SIGTOL',SIGTOL,CNT)
         CALL PRNT(4,1,SIGTOL,' SIGMA REJ TOL=. ')
         CALL XVPARM('MOFSET',TOS,CNT,IDEF,1)
         CALL XVPARM('UNITS',UNITS,ICOUNT,IDEF,1)
         CALL XVPARM('OFFSETS',OFFSET_FILE,ICOUNT,IDEF,1)

C-----If shutter-offset file is supplied, read it and calculate
c-----mean shutter-offset
	DIRECTN(1:4) = 'LINE'
         IF (IDEF.EQ.0) THEN
            CALL XVUNIT(ISO,'X',1,IND,'U_NAME',OFFSET_FILE,' ')
            IF (IND.NE.1) GOTO 998
            CALL XVOPEN(ISO,IND,'OPEN_ACT','SA','IO_ACT','SA',' ')
	    CALL XVGET(ISO,IND,'NS',NSL,' ')

            CALL XLGET(ISO,'HISTORY','FILE',VAL,IST,'FORMAT',
     &                 'STRING',' ')
	    IF (VAL .NE. 'SHUTTER-OFFSET') THEN
	       CALL XVMESSAGE 
     &            ('No SHUTTER-OFFSET in label of offset file',' ')
	       CALL XVMESSAGE(' Continuing anyway',' ')
	    END IF
            CALL XVREAD(ISO,SHUTTER_OFFSETS,IND,' ')
            TOS = 0.0
            DO I=1,NSL
               TOS = TOS + SHUTTER_OFFSETS(I)
            ENDDO
            TOS = TOS/FLOAT(NSL)

c-----While we're at it, check the label to see if the SO is Line-
c-----or Sample-dependent.  If it doesn't say, assume Line-
	    DIRECTN(1:4) = 'LINE'
            CALL XLGET (ISO,'HISTORY','SO_TYPE',VAL,IST, 
     &                  'FORMAT','STRING',' ')
	    IF (IST .NE. 1 .OR. INDEX(VAL,'LINE') .NE. 0) THEN
               DIRECTN(1:4) = 'LINE'
	    ELSE
               DIRECTN(1:4) = 'SAMP'
            END IF
            CALL XVCLOSE(ISO,IND,' ')

         ELSE

c-----No SO file supplied, so fill buffer with mean value from parameters
	    CALL MVE(7,1024,TOS,SHUTTER_OFFSETS,0,1)
         ENDIF

	 CALL PRNT(7,1,TOS,'Mean shutter-offset =.')
	 CALL XVMESSAGE 
     &      ('Shutter-offset is '//DIRECTN(1:4)//'-dependent',' ')

         IF (XVPTST('SUBDC')) THEN
            IDCSUB = 1
            CALL XVMESSAGE ('DC subtraction mode',' ')
         ELSE
            IDCSUB = 0
            CALL XVMESSAGE ('DC included as data point on curve',' ')
         ENDIF

         CALL XVP('EXTEXPO',EFNUM,IEDC)
         IF (EFNUM .EQ. 0) IEDC=0     !Check for EDC Data (10 - OCT - 85)
         IF (IEDC .EQ. 1) 
     &      CALL XVMESSAGE ('EXTENDED EXPOSURE MODE DATA',' ')
         IF ((IDCSUB .EQ. 0) .AND. (IEDC .EQ. 1))
     &      EFNUM=EFNUM+1  !10 POINT CURVE!!

C-----Open the input Light Transfer File
         CALL XVUNIT(IUNI,'INP',1,STAT,' ')
         CALL XVOPEN(IUNI,STAT,'OPEN_ACT','SA','IO_ACT','SA',
     &             'U_FORMAT','REAL',' ')
         CALL XVSIZE(SLI,SSI,NLI,NSI,NLO,NSO)
         CALL LABPROC(IUNI,LABEL,NLAB)
C             Get area size fields...
         CALL XLGET(IUNI,'HISTORY','NUM_AREAS',NAREA,STAT,
     &              'FORMAT','INT',' ')
         CALL XLGET(IUNI,'HISTORY','AREAS',AREA,STAT,'NELEMENT',4*NAREA,
     &              'FORMAT','INT',' ')

c-----Get the radiance value for this test from the LTF label
         CALL XLGET(IUNI,'PROPERTY','RADIANCE',LIGHT,NLUM,
     &             'PROPERTY','CASSINI-ISS','FORMAT','REAL',' ')

c-----Override with parameter
         CALL XVP('LIGHT',LIGHT,LCNT)

C-------any luminances entered?
	 IF ((LCNT .EQ. 0) .AND. (NLUM .NE. 1)) THEN
	    CALL XVMESSAGE('??E - NO LIGHT VALUE INPUT',' ')
	    GOTO 999
	 END IF

         NPTS = NLI-IDCSUB-IEDC  !Number of points on light transfer curve
         J = -(IDCSUB+IEDC)      !J is index for point on curve
C
C        Get average DN for each area and at each exposure and
C        remove dark current if specified...

         CALL XLGET(IUNI,'HISTORY','NUM_EXPOS',NEXP,STAT,
     &              'FORMAT','INT',' ')
         CALL XLGET(IUNI,'HISTORY','EXPOSURES',EXPOS,STAT,
     &              'NELEMENT',NEXP,'FORMAT','REAL',' ')
         DO 50 L=1,NLI			!Exposure loop
            CALL XVREAD(IUNI,DNS,STAT,'LINE',L,'NSAMPS',NSI,' ')
            NI = NINT(DNS(1))
            IF (NI .EQ. 0) THEN
               BEXPO=EXPOS(L,1)
               GOTO 970
            END IF
            J = J + 1
            IF (J .GT. 0) CEXPO(J)=EXPOS(L,1)  !Store commanded exposure
            IB = 0

            DO 50 K=1,NAREA            	!Area loop
               SL=AREA(1,K)             !Get area size field
               SS=AREA(2,K)
               NL=AREA(3,K)
               NS=AREA(4,K)
               N = NI*NL*NS             !Total number of pixels in area*inputs
               DNSUM = 0.0D0
               DO I=1,NI
                  DNSUM = DNSUM + DBLE(DNS(IB+I+1))  !Sum of DNs across inputs
               ENDDO

               IF (IDCSUB .EQ. 0) THEN		!Do not subtract dark current...
                  IF (IEDC .EQ. 0) THEN
                     DN(J,K) = DNSUM/N	!Average DN for area
                  ELSE
                     IF (L .EQ. 1) THEN
                        EDC(K) = DNSUM/N	!Average extended DC for area
                        GOTO 50
                     ENDIF
                     IF (J .LT. EFNUM) THEN
                        DN(J,K) = DNSUM/N
                     ELSE
                        DN(J,K) = DNSUM/N - EDC(K) + DN(1,K)
                     ENDIF
                  ENDIF
               ELSE           !Subtract DC
                  IF (IEDC .EQ. 0) THEN
                     IF (L .EQ. 1) THEN
                        DC(K) = DNSUM/N	!Average dark current
                        GOTO 50
                     ENDIF
                     DN(J,K) = DNSUM/N - DC(K)
                  ELSE
                     IF (L .EQ. 1) EDC(K)=DNSUM/N
                     IF (L .EQ. 2) DC(K)=DNSUM/N
                     IF (L .LE. 2) GOTO 50
                     IF (J .LT. EFNUM) THEN
                        DN(J,K) = DNSUM/N - DC(K)
                     ELSE
                        DN(J,K) = DNSUM/N - EDC(K)
                     ENDIF
                  ENDIF
               ENDIF
   50    IB = IB + 3*NI

	 CALL XVCLOSE(IUNI,IST,' ')
C
C        Correct exposure for shutter offset and convert to energy units...
C
         MAXL = 0
         MAXS = 0

         DO K=1,NAREA
            SL=AREA(1,K)
            SS=AREA(2,K)
            NL=AREA(3,K)
            NS=AREA(4,K)

	    IF (K .EQ. NAREA) THEN
               MAXL = SL + NL/2       !finding last (l,s) of last area
               MAXS = SS + NS/2       !for defining reticle areas later
	    ENDIF

c---------Find line or sample of center of this area
	    IF (DIRECTN(1:4) .EQ. 'LINE') THEN
               L = SL + NL/2
	    ELSE
               L = SS + NS/2
	    ENDIF

c---------Correct exposure time for shutter-offset
            DO J=1,NPTS
               EXPOS(J,K) = AMAX1(CEXPO(J)-SHUTTER_OFFSETS(L),0.0)
               W(J,K) = LIGHT*EXPOS(J,K)
            ENDDO
         ENDDO

         LOOP = 0
         DLTX = 0.0D0

C           Main control loop: Repeat once if DLTX energy correction
C           has been specified (DELTX).

   60    IF (LOOP.EQ.1) THEN
            CALL XVMESSAGE (' ',' ')
            CALL XVMESSAGE (' ',' ')
            CALL XVMESSAGE 
     &         ('-----------------------------------------',' ')
            CALL XVMESSAGE 
     &         ('Processing repeated with DELTA correction',' ')
            CALL XVMESSAGE 
     &         ('-----------------------------------------',' ')
         ENDIF
C    Find slope and offsets for each area...
         DO K=1,NAREA
            SDN = 0.0D0
            SX = 0.0D0
            SDN2 = 0.0D0
            SX2 = 0.0D0
            SXDN = 0.0D0
            DO J=1,NPTS
               SDN = SDN + DN(J,K)             !SUM DNS
               SX = SX + W(J,K)	  	       !SUM ENERGY
               SDN2 = SDN2 + DN(J,K)**2        !SUM DN**2
               SX2 = SX2 + W(J,K)**2	       !SUM ENERGY**2
               SXDN = SXDN + W(J,K)*DN(J,K)    !SUM DN*ENERGY
            ENDDO
            DENOM = DBLE(NPTS)*SX2 - SX**2
            IF (DENOM .EQ. 0.0) GOTO 996 
            SLOPE(K) = (DBLE(NPTS)*SXDN-SX*SDN)/DENOM
            OFFS(K)  =  (SX2*SDN-SX*SXDN)/DENOM
         ENDDO
C    Weed out bad areas...
         CALL ZIA(BAD,NAREA)
         IF (NAREA.GT.1) THEN
            CALL XVMESSAGE (' ',' ')
            CALL XVMESSAGE ('GLOBAL VALUE FOR SLOPE...',' ')
            CALL IMEAN(SLOPE,1,NAREA,BAD,SIGTOL)
            CALL XVMESSAGE (' ',' ')
            CALL XVMESSAGE ('GLOBAL VALUE FOR OFFSET...',' ')
            CALL IMEAN(OFFS,2,NAREA,BAD,SIGTOL)
         ENDIF
C    Print out bad areas
         IBA = 0                  !flag for any bad areas
         CALL XVMESSAGE (' ',' ')
         CALL XVMESSAGE ('Summary of bad areas....',' ')
         DO 65 K=1,NAREA
            IF (BAD(K) .EQ. 0) GOTO 65
            SL=AREA(1,K)
            SS=AREA(2,K)
            NL=AREA(3,K)
            NS=AREA(4,K)

            MS1 = ' '
            WRITE (MS1,'(A4,I4,A16,I4,A1,I4,A1,I4,A1,I4,A1)')
     &        'AREA',K,'(SL,SS,NL,NS)=(',SL,',',SS,',',NL,',',NS,')'
            IF (BAD(K).EQ.1) MS1(47:74)='****BAD SLOPE FIT********'
            IF (BAD(K).EQ.2) MS1(47:74)='****BAD OFFSET FIT*******'
            IF (BAD(K).EQ.4) MS1(47:74)='****BOTH BAD FIT*********'
            CALL XVMESSAGE (MS1,' ')
            IBA = 1
   65    CONTINUE

         CALL XVMESSAGE (' ',' ')
         IF (IBA .eq. 0) CALL XVMESSAGE ('    No rejected areas',' ')
         CALL XVMESSAGE (' ',' ')
         CALL XVMESSAGE ('Slopes and offsets for each area...',' ')
         MS1(43:80) = ' '

         CALL XVMESSAGE (' ',' ')
         IF (UNITS .EQ. 'RADIANCE') THEN
            CALL XVMESSAGE (RADMSG,' ')
         ELSE
            CALL XVMESSAGE (LUMMSG,' ')
         ENDIF
         CALL XVMESSAGE (' ',' ')
         CALL XVMESSAGE (MS4,' ')

        ncoly = 2               !initialize before bump

C-----Print out slope and offset for each area...
         DO 100 K=1,NAREA
            SL=AREA(1,K)
            SS=AREA(2,K)
            NL=AREA(3,K)
            NS=AREA(4,K)
            WRITE 
     &         (MS2,'(I4,2X,I4,1X,I4,1X,I4,1X,I4,7X,F10.5,9X,F10.5)')
     &         K,SL,SS,NL,NS,SLOPE(K),OFFS(K)
            CALL XVMESSAGE (MS2,' ')
  100    CONTINUE
	unit97 = 97
	unit98 = 98

        open(98,file=plotgpi(1:nplotgpi),status='UNKNOWN',iostat=jj,err=960)
        if (epsplot) then
           open(97,file=plotgpi2(1:nplotgpi2),status='UNKNOWN',iostat=jj,err=961)
        endif

         CALL LTCPLOT(AREA,BAD,CEXPO,DN,EXPOS,W,LABEL,SLOPE,OFFS,
     1        ARMES,RMSG,NPTS,NAREA,IREJCT,LIGHT,epsplot,TOS,LOOP,
     2        DLTX,IDCSUB,NLAB,IPLOT,MAXL,MAXS,ncoly,ocol,   
     3       plotgpi,nplotgpi,plotgpi2,nplotgpi2,ploteps,nploteps,
     4           tbl,ntbl,unit97,unit98)
         IF ((CNTRL .EQ. 0) .OR. (LOOP .EQ. 1)) GOTO 200
         WRITE (MS3,'(A17,F10.5,A12)')
     &          'DELTA CORRECTION=',DLTX,' ENERGY UNIT'
         CALL XVMESSAGE (MS3,' ')
         LOOP = 1

c-----Adjust energy terms by DLTX and loop back
         DO K=1,NAREA
            DO J=1,NPTS
               W(J,K) = W(J,K) - DLTX
            ENDDO
         ENDDO
         GOTO 60 

200	continue
	close (98)
	if (epsplot) close(97)
c	print *, 'NPTS = ',npts
	if (iplot .eq. 1) then
c	write 20 columns - NPTS rows
c                ocol(1,i) = T(i)
c                ocol(2,i) = W(i)
c                ocol(3,i) = D(i)
c                ocol(4,i) = DEL(i)
c                ocol(5,i) = PCDX(i)
c
	    OPEN(13,FILE=tbl(1:ntbl),STATUS='UNKNOWN',IOSTAT=JST,ERR=950)

            do j=1,npts
               write (13,10010) (ocol(i,j), i=1,20)
            enddo
10010   format (20e12.3,1x)
            close (13)
	endif
         CALL XVUNIT(OUNI,'OUT',1,STAT,' ')
         IF (STAT .NE. 1) GOTO 300
         CALL ZIA(OUTM,2*NAREA)
         I = 0

         DO 220 K=1,NAREA
            IF ((IREJCT .EQ. 0) .OR. (BAD(K) .EQ. 0) .OR.
     &          (IREJCT .NE. 3) .AND. (IREJCT .NE. BAD(K)) .AND.
     &          (BAD(K) .NE. 4)) GOTO 220
            SL=AREA(1,K)
            SS=AREA(2,K)
            NL=AREA(3,K)
            NS=AREA(4,K)
            I = I + 1
            OUTM(1,I) = SL + NL/2
            OUTM(2,I) = SS + NS/2
  220    CONTINUE

         NSO = 2*I
         NLO = 1
         CALL XVOPEN(OUNI,STAT,'OPEN_ACT','SA','IO_ACT','SA',
     &        'OP','WRITE','U_NL',NLO,'U_NS',NSO,'U_FORMAT','REAL',
     &        'O_FORMAT','REAL',' ')

         IF (IREJCT .EQ. 1) 
     &      CALL XLADD (OUNI,'HISTORY','CCDSLOPE',
     &                  ' REJECTED FOR SLOPE ',STAT,'FORMAT',
     &                  'STRING',' ')

         IF (IREJCT .EQ. 2) 
     &      CALL XLADD(OUNI,'HISTORY','CCDSLOPE',
     &                 ' REJECTED FOR OFFSET ',STAT,'FORMAT',
     &                 'STRING',' ')

         IF (IREJCT .EQ. 3)
     &      CALL XLADD(OUNI,'HISTORY','CCDSLOPE',
     &                 ' REJECTED FOR BOTH ',STAT,'FORMAT',
     &                 'STRING',' ')

         CALL XVWRIT(OUNI,OUTM,STAT,'NSAMPS',NSO,' ')
         CALL XVCLOSE(OUNI,STAT,' ')

  300    IF (IPLOT .EQ. 0) RETURN
ccc
ccc         CALL PLOT(0.00,0.00,999)
         CALL XVMESSAGE ('CCDSLOPE task completed',' ')
         RETURN
  950  continue
         CALL XVMESSAGE ('??E - ERROR OPENING PLOT data file',' ')
         CALL PRNT(4,1,JST,'IOSTAT=.')
	 goto 999

c   error returns
960     call xvmessage('??E - splot: Error opening/writing gnuplot file',' ')
	CALL PRNT(4,1,JJ,'IOSTAT=.')
        goto 999  
961     call xvmessage('??E - splot: Error opening/writing gnuplot eps file',' ')
	CALL PRNT(4,1,JJ,'IOSTAT=.')
        goto 999  

  970    CALL PRNT(7,1,BEXPO,'??E - No data for exposure=.')
         CALL XVMESSAGE 
     &      ('***Run MOMGEN on this exposure and try again.',' ')
         GOTO 999
  996    CALL XVMESSAGE ('??E - Error: SUM ENERGY = 0.0',' ')
         GOTO 999
  998    CALL XVMESSAGE ('??E - Error opening shutter offset file',' ') 
  999    CALL XVMESSAGE ('***CCDSLOPE task cancelled',' ')
         CALL ABEND
         RETURN
       END

C*************************************************************************
       SUBROUTINE IMEAN(BUF,TYPE,N,BAD,SIGTOL)
C Routine to flag all areas which differ from mean by more than 2 sigma.
C Inputs: BUF(N) = input samples
C         TYPE = 1 for slope, 2 for offset
C         N = number of samples
C Outputs: BAD(N) = TYPE if sample is bad type
C                 = 4 if sample has both bad slope and offset
C
	implicit none

         INTEGER*4 BAD(1),TYPE,SIGTOL,IMAX,K,N,NS
         
         REAL*4 BUF(1)
         REAL*4 MAXDIFF,SAMP,SIGMA
         REAL*8 SUM,SSUM,MEAN
         CHARACTER*44 MSG

         MSG = 'N=**** MEAN=******.***** SIGMA=******.***** '
  
         MEAN = 0.0d0 
         IMAX = 0 
         NS = 0			!Count of number of good samples
         SUM = 0.0D0
         SSUM = 0.0D0

C        Get sums and sums of squares of all good areas...
         DO K=1,N
            SAMP = BUF(K)
            NS = NS+1
            SUM = SUM+DBLE(SAMP)
            SSUM = SSUM+DBLE(SAMP*SAMP)
         ENDDO

C        ...and use these to compute mean and standard deviation.
         MEAN = SUM/NS
         SIGMA = DSQRT(SSUM/NS-MEAN*MEAN)
         WRITE (MSG,'(A2,I4,A6,F12.5,A7,F12.5)')
     &          'N=',NS,' MEAN=',MEAN,' SIGMA=',SIGMA
         CALL XVMESSAGE ('Raw mean and sigma are...',' ')
         CALL XVMESSAGE (MSG,' ')
         IF (SIGMA .EQ. 0.0) RETURN
C
C         Weed out all samples differing by more than 2 sigma from mean...
   10    MAXDIFF = 0.

C        First find sample with largest difference...
         DO 20 K=1,N
            IF ((BAD(K) .EQ. TYPE) .OR. (BAD(K) .EQ. 4)) GOTO 20
            SAMP = ABS(BUF(K)-SNGL(MEAN))
            IF (SAMP .LE. MAXDIFF) GOTO 20
            MAXDIFF = SAMP
            IMAX = K          
   20    CONTINUE

C        Continue if remaining samples are good.
         IF (MAXDIFF .LE. SIGTOL*SIGMA) GOTO 50	
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

   50    MEAN = SUM/NS
         SIGMA = DSQRT(SSUM/NS-MEAN**2)
         WRITE (MSG,'(A2,I4,A6,F12.5,A7,F12.5)')
     &          'N=',NS,' MEAN=',MEAN,' SIGMA=',SIGMA
         IF (SIGTOL .EQ. 1) THEN
            CALL XVMESSAGE 
     &         ('After throwing out samples differing by 1 sigma',' ')
         ELSE
            CALL XVMESSAGE 
     &         ('After throwing out samples differing by 2 sigma',' ')
         ENDIF
         CALL XVMESSAGE (MSG,' ')
         RETURN
       END         
C****************************************************************************
      SUBROUTINE LTCPLOT (AREA,BAD,CEXPO,DN,EXPOS,W,LABEL,SLOPE,
     1            OFFS,ARMES,RMSG,NPTS,NAREA,IREJCT,LIGHT,epsplot,
     2            TOS,LOOP,DLTX,IDCSUB,NLAB,IPLOT,MAXL,MAXS,ncoly,ocol,
     3           plotgpi,nplotgpi,plotgpi2,nplotgpi2,ploteps,nploteps,
     4           tbl,ntbl,unit97,unit98)

C Routine to plot the light-transfer curve (signal vs exposure) for each of
C the 5 reticles...
C All arguments are inputs.
C
	implicit none
         INTEGER*4 AREA(4,400)	  !Area size fields: (sl,ss,nl,ns)
         INTEGER*4 BAD(400)	  !1=bad slope, 2=bad offset, 4=both bad

	 INTEGER*4 nplotgpi,nplotgpi2,nploteps,ntbl,ncoly
         INTEGER*4 NPTS,NAREA,IREJCT,LOOP,IDCSUB,NLAB,IPLOT,MAXL,MAXS 
         INTEGER*4 I,ICNT,IDEF,IMIN,J,K,NGOOD,NL,NS
	integer*4 unit97,unit98
         REAL*4 CEXPO(30)         !Commanded exposure times (msec)
         REAL*8 DN(30,400)        !Mean DN of each (exposure,area)
         REAL*4 EXPOS(30,400)     !Exposure of each (exposure,area)
         REAL*8 W(30,400)         !Energy of each (exposure,area)
         REAL*4 LIGHT             !Light values for each exposure
         REAL*4 SLOPE(400),OFFS(400),TOS,D,DMIN

         CHARACTER*4320 LABEL
         CHARACTER*51 ARMES
         CHARACTER*41 RMSG  !labels & annotation
         CHARACTER*40 TBLFILE
         character*80 plotgpi,plotgpi2,ploteps,tbl
         REAL*8 ocol(30,6) 		!output data for gnuplot
         REAL*8 DLTX,DX(30),WX(30),COMPDNX(30),SMEAN
         REAL*8 AVDN(30,5),AVW(30,5),COMPDN(30,5),ASLOPE,AOFFS
         REAL*8 SSLOPE(5),SOFF(5)      !Sums of slopes and offsets 
         REAL*8 S2SLOPE(5),S2OFF(5)    !Sums of sqrs of slopes & offsets 
         REAL*8 SLOPESIG(5),OFFSIG(5)  !Sigmas for each reticle
         REAL*8 TSLOPESIG,TOFFSIG      !Framewide sigmas
         REAL*8 TSSLOPE,TSOFF,TS2SLOPE,TS2OFF   !framewide sums of areas
         REAL*4 AVEXPO(30,5),TX(30),RETICLES(2,5)
         INTEGER*4 NG(5)		  !number of good areas in each reticle
         INTEGER*4 NA(5)		  !number of areas near reticle
         INTEGER*4 SL,SS
         LOGICAL*4 LASTFLAG,epsplot

         LASTFLAG = .FALSE.
         IMIN = 0
         CALL ZIA(NG,5)
         CALL ZIA(NA,5)
         CALL ZIA(SSLOPE,10)
         CALL ZIA(SOFF,10)
         CALL ZIA(S2SLOPE,10)
         CALL ZIA(S2OFF,10)
         CALL ZIA(SLOPESIG,10)
         CALL ZIA(OFFSIG,10)

C-------NPTS is the number of exposure levels

         DO I=1,5
           CALL ZIA(AVDN(1,I),NPTS*2)
           CALL ZIA(AVEXPO(1,I),NPTS)
           CALL ZIA(AVW(1,I),NPTS*2)
           CALL ZIA(COMPDN(1,I),NPTS*2)
         ENDDO

C-----Set Reticle points based upon the last line/samp used
         RETICLES(1,1) = 1           !upper left
         RETICLES(2,1) = 1
         RETICLES(1,2) = 1           !upper right
         RETICLES(2,2) = maxs
         RETICLES(1,3) = maxl        !lower left
         RETICLES(2,3) = 1
         RETICLES(1,4) = maxl        !lower right
         RETICLES(2,4) = maxs
         RETICLES(1,5) = maxl/2      !center
         RETICLES(2,5) = maxs/2
C
C           Compute signal, exposure and energy at each reticle...
         DO 10 K=1,NAREA
            SL = AREA(1,K)
            SS = AREA(2,K)
            NL = AREA(3,K)
            NS = AREA(4,K)
C           Find nearest reticle...
            DMIN = 99999.**2

            DO I=1,5
               D = (RETICLES(1,I)-SL)**2 + (RETICLES(2,I)-SS)**2
               IF (D .LT. DMIN) THEN
                  DMIN = D
                  IMIN = I
               ENDIF
            ENDDO

            NA(IMIN) = NA(IMIN) + 1
            IF ((IREJCT .EQ. 1) .AND. 
     &          ((BAD(K) .EQ. 1) .OR. (BAD(K) .EQ. 4))) GOTO 10
            IF ((IREJCT .EQ. 2) .AND. 
     &          ((BAD(K) .EQ. 2) .OR. (BAD(K) .EQ. 4))) GOTO 10
            IF ((IREJCT .EQ. 3) .AND. (BAD(K) .NE. 0)) GOTO 10
C           Add data to that reticle...
            NG(IMIN) = NG(IMIN) + 1      !number of good areas in reticle

C-----Sum the DN, EXP and ENERGY of area into reticle sums for each
C-----exposure level
            DO J=1,NPTS
               AVDN(J,IMIN) = AVDN(J,IMIN) + DN(J,K)
               AVEXPO(J,IMIN) = AVEXPO(J,IMIN) + EXPOS(J,K)
               AVW(J,IMIN) = AVW(J,IMIN) + W(J,K)
            ENDDO

c-----Accumulate the sums and squares to calculate the sigmas 
            SSLOPE(IMIN) = SSLOPE(IMIN) + SLOPE(K)
            S2SLOPE(IMIN) = S2SLOPE(IMIN) + SLOPE(K)*SLOPE(K)
            SOFF(IMIN) = SOFF(IMIN) + OFFS(K)
            S2OFF(IMIN) = S2OFF(IMIN) +OFFS(K)*OFFS(K)

  10     CONTINUE

         IF (IREJCT .EQ. 1) RMSG(22:35)='    SLOPE     '
         IF (IREJCT .EQ. 2) RMSG(22:35)='    OFFSET    '
         IF (IREJCT .EQ. 3) RMSG(22:35)='    BOTH      '

         CALL ZIA(TX,NPTS)
         CALL ZIA(DX,2*NPTS)
         CALL ZIA(COMPDNX,2*NPTS)
         CALL ZIA(WX,2*NPTS)
         NGOOD = 0             !number of good areas framewide
         TSSLOPE = 0.D0
         TSOFF = 0.D0
         TS2SLOPE = 0.D0
         TS2OFF = 0.D0

C-----Process and plot data for each reticle
C-----Also, accumulate data to enable whole image processing
         DO 20 I=1,5
            IF (NG(I) .EQ. 0) THEN
               CALL XVMESSAGE 
     &            ('***No good areas in this part of frame',' ')
               GOTO 20
            ENDIF
            NGOOD = NGOOD + NG(I)      !increment number of good areas
            DO J=1,NPTS
               DX(J) = DX(J) + AVDN(J,I)      !Framewide sums
               TX(J) = TX(J) + AVEXPO(J,I)
               WX(J) = WX(J) + AVW(J,I)
               AVDN(J,I) = AVDN(J,I)/NG(I)    !Reticle means
               AVEXPO(J,I) = AVEXPO(J,I)/NG(I)
               AVW(J,I) = AVW(J,I)/NG(I)
            ENDDO

            WRITE (RMSG(38:40),'(I3)') NA(I)-NG(I)

            IF (IREJCT.EQ.0) THEN
               RMSG(1:41)='NO REJECTION CRITERIA APPLIED            '
            END IF

            WRITE (ARMES(22:25),'(I4)') NG(I)
            WRITE (ARMES(33:36),'(I4)') NA(I)

C-------Find mean of slopes and offsets of all areas allocated to
c-------this reticle.  This is necessary in order to derive a
c-------sigma for them.  The means are derived from the already-
c-------calculated slopes and offsets for each area.
            SMEAN = SSLOPE(I)/DBLE(NG(I))
            SLOPESIG(I) = DSQRT(S2SLOPE(I)/DBLE(NG(I)) - SMEAN*SMEAN)
            CALL PRNT(4,1,I,'Reticle .')
            CALL XVMESSAGE ('Means from averaging values of areas',' ')
            CALL PRNT(8,1,SMEAN,' SLOPE  =.')
            SMEAN = SOFF(I)/DBLE(NG(I))
            OFFSIG(I) = DSQRT(S2OFF(I)/DBLE(NG(I)) - SMEAN*SMEAN)
            CALL PRNT(8,1,SMEAN,' OFFSET =.')
            CALL XVMESSAGE (' ',' ')

C-------Accumulate the sums and squares of sums of all areas in
c-------order to derive the frame-wide sigma later.
            TSSLOPE = TSSLOPE + SSLOPE(I)
            TSOFF = TSOFF + SOFF(I)
            TS2SLOPE = TS2SLOPE + S2SLOPE(I)
            TS2OFF = TS2OFF + S2OFF(I)

C------Derive the slope and offset for each reticle by using the
c------exposure, dn and energy values at each exposure level accumulated
c------from all areas allocated to this reticle.  Does not just
c------average the slopes and offsets for each allocated area that have
c------already been calculated.

            CALL RTIMES(NPTS,LIGHT,CEXPO,AVEXPO(1,I),AVDN(1,I),AVW(1,I),
     1                  SLOPESIG(I),OFFSIG(I),IPLOT,I,LABEL,NLAB,ARMES,
     2                  RMSG,TOS,LOOP,ocol,ASLOPE,AOFFS,COMPDN(1,I),
     3                  DLTX,IDCSUB,LASTFLAG,ncoly,epsplot,
     4           plotgpi,nplotgpi,plotgpi2,nplotgpi2,ploteps,nploteps,
     5           tbl,ntbl,unit97,unit98)

   20    CONTINUE

C-------Find mean of slopes and offsets of all areas.
c-------This is necessary in order to derive a
c-------sigma for the framewide values.  The means are derived from 
c-------the already-accumulated sums of slopes and offsets for each area.
         SMEAN = TSSLOPE/DBLE(NGOOD)
         TSLOPESIG = DSQRT(TS2SLOPE/DBLE(NGOOD) - SMEAN*SMEAN)
         CALL XVMESSAGE ('Framewide',' ')
         CALL XVMESSAGE ('Means from averaging values of areas',' ')
         CALL PRNT(8,1,SMEAN,' SLOPE=.')
         SMEAN = TSOFF/DBLE(NGOOD)
         TOFFSIG = DSQRT(TS2OFF/DBLE(NGOOD) - SMEAN*SMEAN)
         CALL PRNT(8,1,SMEAN,' OFFSET=.')
         CALL XVMESSAGE (' ',' ')

c-----Make the framewide means to use in deriving the slope and
c-----offset for the entire frame.
         DO J=1,NPTS
            DX(J) = DX(J)/DBLE(NGOOD)
            TX(J) = TX(J)/DBLE(NGOOD)
            WX(J) = WX(J)/DBLE(NGOOD)
         ENDDO

C-----Process and plot framewide data, returning the best fit
c-----slope and offset in ASLOPE and AOFFS.
         LASTFLAG = .TRUE.
         CALL RTIMES(NPTS,LIGHT,CEXPO,TX,DX,WX,TSLOPESIG,TOFFSIG,
     1           IPLOT,6,LABEL,NLAB,ARMES,RMSG,TOS,LOOP,ocol,ASLOPE,
     2           AOFFS,COMPDNX,DLTX,IDCSUB,LASTFLAG,ncoly,epsplot,
     3           plotgpi,nplotgpi,plotgpi2,nplotgpi2,ploteps,nploteps,
     4           tbl,ntbl,unit97,unit98)

         DLTX = -AOFFS/ASLOPE

         CALL XVPARM('TABLE',TBLFILE,ICNT,IDEF,1)
         IF (ICNT .EQ. 0) RETURN


C-----Use reticle and whole frame data to create output table

         CALL GENTBL(AVDN,COMPDN,AVW,DX,COMPDNX,WX,NPTS,TBLFILE,*999)

999	 CONTINUE
         RETURN
      END
C****************************************************************************
      SUBROUTINE RTIMES(NPTS,L,CEXPO,T,D,W,SLOPESIG,OFFSSIG,IPLOT,
     1           IRETICLE,LABEL,NLAB,ARMES,RMSG,TOS,LOOP,ocol,
     2           SLOPE,OFFS,DNP,DLTX,IDCSUB,LASTFLAG,ncoly,epsplot,
     3           plotgpi,nplotgpi,plotgpi2,nplotgpi2,ploteps,nploteps,
     4           tbl,ntbl,unit97,unit98)


C Routine to plot:
C	1) Light transfer curve (DN vs Energy)
C       2) Residuals to curve
C       3) Percent residuals
C The light transfer curve has the form: DN = SLOPE*ENERGY + OFFSET
C
C Inputs: NPTS=Number of data points on curve
C            L=light intensity in relative foot lamberts or nanowatts/cm**2/sr/
C              nanometer
C	     CEXPO=commanded exposure time (msec)
C            T=corrected exposure time (msec)
C            D=DN value
C	     W=energy 
C            SLOPESIG=simga of slope
C            OFFSSIG=sigma of offset
C
C Outputs: SLOPE,OFFS=slope and offset of light transfer curve
C
	implicit none

         INTEGER*4 NPTS,IPLOT,IRETICLE,NLAB,LOOP,IDCSUB,I
	INTEGER*4 nploteps,nplotgpi,nplotgpi2,ntbl,ncoly
	 integer*4 nytitle,unit97,unit98
         REAL*4 CEXPO(30),T(30),L,TOS
         REAL*8 D(30),W(30),ocol(30,6)
         REAL*8 SLOPE,OFFS,DLTX
         REAL*8 PCDX(30),DNP(30),DEL(30)
         REAL*8 SLOPESIG,OFFSSIG,DENOM,RMS,RX
         REAL*8 SDN,SDN2,SXDN,SDNP,SX,SX2,XC1,XC2
	 real*4 delmax,dmax,pcdxmax,wmax,dnpmax
         real*4 delmin,dmin,pcdxmin,wmin,dnpmin
	  real*4 delmax1,dmax1,pcdxmax1,wmax1,dnpmax1
         real*4 delmin1,dmin1,pcdxmin1,wmin1,dnpmin1
         LOGICAL*4 LASTFLAG,TEMPFLAG,epsplot

         character*80 plotgpi,plotgpi2,ploteps,tbl
         CHARACTER*12 MSG(6)
         CHARACTER*34 MSG2
         CHARACTER*62 GMS2
         CHARACTER*132 MSG1
         CHARACTER*9 HD1, HD16
         CHARACTER*6 HD2, HD7, HD12
         CHARACTER*8 HD3, HD4, HD5
         CHARACTER*18 DC8, HD8, HD44, HD45
         CHARACTER*12 HD6
         CHARACTER*7 HD14
         CHARACTER*5 HD9
         CHARACTER*6 HD10L, HD10R
         CHARACTER*13 HD11
         CHARACTER*3 HD13
         CHARACTER*4 HD15

         CHARACTER*4320 LABEL 
         CHARACTER*51 ARMES 
         CHARACTER*41 RMSG
         CHARACTER*8  UNITS
         CHARACTER*40 LUMMSG
         CHARACTER*34 RADMSG
         COMMON/C2/UNITS,LUMMSG,RADMSG
 
C Assigning values to array MSG  (Also start of executable code)

         TEMPFLAG = LASTFLAG
         LASTFLAG = .FALSE.
 
         HD1 = 'COMMANDED'
         HD2 = 'ACTUAL'
         HD3 = 'BEST FIT'
         HD4 = 'RESIDUAL'
         HD44 = 'RESIDUAL (DN)     '
         HD45 = 'RESIDUAL (PERCENT)'     
         HD5 = 'EXPOSURE'
         HD6 = 'ILLUMINATION'
         HD7 = 'ENERGY'
         DC8 = '(DN-DC)           '
         HD8 = '   DN             '
         HD9 = 'T(MS)'
         HD10L = 'L(LUM)'
         HD10R = 'L(RAD)'
         HD11 = '(ENERGY UNIT)'
         HD12 = 'SLOPE='
         HD13 = 'SD='
         HD14 = 'OFFSET='
         HD15 = 'RMS='
         HD16 = 'CORRECTED'

         MSG(1)='=UPPER-LEFT '
         MSG(2)='=UPPER-RIGHT'
         MSG(3)='=LOWER-LEFT '
         MSG(4)='=LOWER-RIGHT'
         MSG(5)='=CENTER     '
         MSG(6)='=FULL FRAME '   

         GMS2(1:35)='GALGEN:C1=****.***** ENERGY UNIT/DN'
         GMS2(36:62)= ' C2=****.***** ENERGY UNIT '
      
C-----Print which reticle is being done
         MSG2='STATISTICS FOR UPPER-RIGHT CORNER'
         MSG2(16:26)=MSG(IRETICLE)(2:12)
         IF ((IRETICLE .EQ. 5) .OR. (IRETICLE .EQ. 6))
     &      MSG2(28:33) = ' '
         CALL XVMESSAGE (MSG2,' ')

C-----Construct line 1 of header
         MSG1 = ' '
         MSG1(1:9)=HD1
         MSG1(13:18)=HD2
         IF (LOOP .EQ. 1) MSG1(39:47)=HD16
         MSG1(60:67)=HD3
         MSG1(71:78)=HD4 
         CALL XVMESSAGE (' ',' ')
         CALL XVMESSAGE (MSG1,' ')        !print header line 1

C-----Construct line 2 of header
         MSG1 = ' '
         MSG1(1:8)=HD5
         MSG1(12:19)=HD5
         MSG1(22:33)=HD6
         MSG1(39:44)=HD7
         IF (IDCSUB .EQ. 0) THEN
            MSG1(49:55)=HD8(1:7)
            MSG1(60:66)=HD8(1:7)
            MSG1(71:77)=HD8(1:7)
         ELSE
            MSG1(49:55)=DC8(1:7)
            MSG1(60:66)=DC8(1:7)
            MSG1(71:77)=DC8(1:7)
         ENDIF
         CALL XVMESSAGE (MSG1,' ')        !print header line 2

C-----Construct line 3 of header
         MSG1 = ' '
         MSG1(2:6)=HD9
         MSG1(13:17)=HD9
         IF (UNITS .EQ. 'RADIANCE') THEN
            MSG1(25:30)=HD10R
         ELSE
            MSG1(25:30)=HD10L
         ENDIF
         MSG1(35:47)=HD11 
         CALL XVMESSAGE (MSG1,' ')        !print header line 3
c
c	COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
c	EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY    (DN-DC)    (DN-DC)    (DN-DC)
c	 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
c

c-----Perform least squares fit to derive slope and offset
         SX=0.0D0
         SX2=0.0D0
         SDN=0.0D0
         SDN2=0.0D0
         SXDN=0.0D0
         SDNP=0.0D0

         DO I=1,NPTS
            SX = SX + W(I)
            SDN = SDN + D(I)
            SX2 = SX2 + W(I)**2
            SXDN = SXDN + W(I)*D(I)
            SDN2 = SDN2 + D(I)**2
         ENDDO

         DENOM = DBLE(NPTS)*SX2 - SX**2
         SLOPE = (DBLE(NPTS)*SXDN-SX*SDN)/DENOM
         OFFS =  (SX2*SDN-SX*SXDN)/DENOM

C-----Set up residuals...
         RX=0.0D0
         MSG1 = ' '

	dnpmax = -1.0d20
        dnpmin = 1.0d20
	wmax = -1.0d20
        wmin = 1.0d20
	delmax = -1.0d20
        delmin = 1.0d20
	dmax = -1.0d20
        dmin = 1.0d20
	pcdxmax = -1.0d20
	pcdxmin = 1.0d20
         DO I=1,NPTS
            DNP(I) = W(I)*SLOPE + OFFS
            DEL(I) = D(I) - DNP(I)		!Residual error in DN
            PCDX(I) = 0.
            IF (D(I) .NE. 0.) PCDX(I)=100.0D0*DEL(I)/D(I) !Percentage error
            RX = RX + DEL(I)**2			!Accumulate RMS error
            WRITE (MSG1,'(F10.4, 2F11.4, F14.4, 2F11.4, F10.4)')
     &             CEXPO(I),T(I),L,W(I),D(I),DNP(I),DEL(I)
            CALL XVMESSAGE (MSG1,' ')
            if (ireticle .eq. 1) then
	        ocol(1,i) = T(i)
                ocol(2,i) = W(i)
	        ocol(3,i) = D(i)
	        ocol(4,i) = DEL(i)
                ocol(5,i) = PCDX(i)
	    else
		ocol(ireticle*3,i) = D(i)
		ocol(ireticle*3+1,i) = DEL(i)
		ocol(ireticle*3+2,i) = PCDX(i)
	    endif
	    if (dnpmax .lt. real(DNP(i))) dnpmax = real(DNP(i))
            if (dnpmin .gt. real(DNP(i))) dnpmin = real(DNP(i))
	    if (wmax .lt. real(W(i))) wmax = real(W(i))
            if (wmin .gt. real(W(i))) wmin = real(W(i))
	    if (delmax .lt. real(DEL(i))) delmax = real(DEL(i))
            if (delmin .gt. real(DEL(i))) delmin = real(DEL(i))
	    if (dmax .lt. real(D(i))) dmax = real(D(i))
            if (dmin .gt. real(D(i))) dmin = real(D(i))
	    if (pcdxmax .lt. real(PCDX(i))) pcdxmax = real(PCDX(i))
	    if (pcdxmin .gt. real(PCDX(i))) pcdxmin = real(PCDX(i))
         ENDDO
c
c	X-boundaries
	wmax1 = wmax
	wmax1 = 100.
	if (wmax .gt. 100.) wmax1 = 225.
	if (wmax .gt. 200.) wmax1 = 325.
	if (wmax .gt. 300.) wmax1 = 425.
	if (wmax .gt. 400.) wmax1 = 525.
	
	wmin1 = 0.
c
c	Y-Boundaries
	dmax1 = dmax
	if (dmax .lt. 400.0) dmax1 = 400.0
        if (dmax .gt. 400.) dmax1 = 500.
	dmin1 = 0.
 	delmax1 = delmax
	delmin1 = delmin
	if (delmax .lt. 1.0) delmax1 = 1.0
	if (delmin .gt. -1.0) delmin1 = -1.0
        pcdxmax1 = pcdxmax
	pcdxmin1 = pcdxmin
        if (pcdxmax .lt. 1.0) pcdxmax1 = 1.0
        if (pcdxmin .gt. -1.0) pcdxmin1 = -1.0

	dnpmax1 = dnpmax
	dnpmin1 = 0.
        if (dnpmax .gt. 100.) dnpmax1 = 200.
	if (dnpmax .gt. 200.) dnpmax1 = 300.
	if (dnpmax .gt. 300.) dnpmax1 = 400.
	if (dnpmax .gt. 400.) dnpmax1 = 500.
        if (dnpmax .eq. 0.0 .and. dnpmin .eq. 0.0) then
           dnpmax1 = 1.0
	   dnpmin1 = -1.0
        endif
        if (dmax .eq. 0.0 .and. dmin .eq. 0.0) then
           dmax1 = 1.0
           dmin1 = -1.0
        endif
        if (delmax .eq. 0.0 .and. delmin .eq. 0.0) then
           delmax1 = 1.0
           delmin1 = -1.0
        endif
        if (pcdxmax .eq. 0.0 .and. pcdxmin .eq. 0.0) then
           pcdxmax1 = 1.0
           pcdxmin1 = -1.0
        endif 


c	if (pcdxmax .eq. 0.0) pcdxmax = dmax

c	print *, 'DELMAX DMAX PCDXMAX,PCDXMIN = ',DELMAX,DMAX,PCDXMAX,PCDXMIN
c------Commenting all this out 'cause noone can figure it out and
c------have now input SLOPESIG and OFFSSIG from calling routine.

         RMS = DSQRT(RX/DBLE(NPTS))
         XC1 = 1.0D0/SLOPE          !GALGEN COEFICIENT C1
         XC2 = -(OFFS/SLOPE)        !GALGEN COEFICIENT C2
         MSG1 = ' '
         MSG1(65:68)=HD15
         WRITE (MSG1(69:78),'(F10.4)') RMS !print RMS
         CALL XVMESSAGE (MSG1,' ')

         MSG1 = ' '
         MSG1(1:6)=HD12
         WRITE (MSG1(7:17),'(F11.6)') SLOPE !calc'd slope
         MSG1(19:21)=HD13
         WRITE (MSG1(22:30),'(F9.6)') SLOPESIG !std dev of slope
         MSG1(32:38)=HD14      
         WRITE (MSG1(39:49),'(F11.6)') OFFS !calc'd offset
         MSG1(51:53)=HD13      
         WRITE (MSG1(54:62),'(F9.6)') OFFSSIG !std dev of offset
         CALL XVMESSAGE (MSG1,' ')

c-----Number of good areas and rejection reasons
         CALL XVMESSAGE (ARMES,' ')
         CALL XVMESSAGE (RMSG,' ')
         WRITE (GMS2(11:20),'(F10.5)') XC1 !GALGEN COEF.
         WRITE (GMS2(40:49),'(F10.5)') XC2 !GALGEN COEF.
         CALL XVMESSAGE (GMS2,' ')

         IF (IPLOT .EQ. 0) RETURN
         IF (LOOP .EQ. 1) RETURN


c	print *, 'IDCSUB = ',IDCSUB

         IF (IDCSUB .EQ. 0) THEN
c	print *,'-----------SPLOT 1A   dnpmax,dnpmax1 = ',dnpmax,dnpmax1	
            CALL SPLOT(LABEL,NLAB,HD8,D,DNP,W,0,NPTS,ARMES,
     1                 MSG(IRETICLE),RMSG,MSG1,GMS2,TOS,DLTX,nytitle,
     2                 IRETICLE,RMS,LASTFLAG,ncoly,plotgpi,nplotgpi,plotgpi2,
     3        nplotgpi2,epsplot,ploteps,nploteps,tbl,ntbl,unit97,unit98,
     4        wmin1,wmax1,dnpmax1,dnpmin1)
         ELSE
c	print *,'-----------SPLOT 1B   dmax,dmax1 = ',dmax,dmax1
            CALL SPLOT(LABEL,NLAB,DC8,D,DNP,W,0,NPTS,ARMES,
     1                 MSG(IRETICLE),RMSG,MSG1,GMS2,TOS,DLTX,nytitle,
     2                 IRETICLE,RMS,LASTFLAG,ncoly,plotgpi,nplotgpi,plotgpi2,
     3        nplotgpi2,epsplot,ploteps,nploteps,tbl,ntbl,unit97,unit98,
     4        wmin1,wmax1,dmax1,dmin1)
         ENDIF
c	print *,'-----------SPLOT 2   delmin,delmax,delmax1 = ',delmin,delmax,delmax1
         CALL SPLOT(LABEL,NLAB,HD44,DEL,DEL,W,2,NPTS,ARMES,
     1              MSG(IRETICLE),RMSG,MSG1,GMS2,TOS,DLTX,nytitle,
     2              IRETICLE,RMS,LASTFLAG,ncoly,plotgpi,nplotgpi,plotgpi2,
     3        nplotgpi2,epsplot,ploteps,nploteps,tbl,ntbl,unit97,unit98,
     4        wmin1,wmax1,delmax1,delmin1)

         IF (TEMPFLAG) LASTFLAG=.TRUE.
c	print *,'-----------SPLOT 3  pcdxmin,pcdxmax,pcdxmax1 = ',pcdxmin,pcdxmax,pcdxmax1
         CALL SPLOT(LABEL,NLAB,HD45,PCDX,PCDX,W,2,NPTS,ARMES,
     1              MSG(IRETICLE),RMSG,MSG1,GMS2,TOS,DLTX,nytitle,
     2              IRETICLE,RMS,LASTFLAG,ncoly,plotgpi,nplotgpi,plotgpi2,
     3        nplotgpi2,epsplot,ploteps,nploteps,tbl,ntbl,unit97,unit98,
     4        wmin1,wmax1,pcdxmax1,pcdxmin1)
         RETURN
      END
C*******************************************************************
      SUBROUTINE SPLOT(LABEL,NLAB,YTITLE,DY,DY2,DX,IFLAG,NPTS,
     1       ARMES,MSG,RMSG,MS2,GMS2,TOS,DLTX,nytitle,
     2       IRETICLE,RMS,LASTFLAG,ncoly,plotgpi,nplotgpi,plotgpi2,
     3        nplotgpi2,epsplot,ploteps,nploteps,tbl,ntbl,unit97,unit98,
     4       xrang1,xrang2,yrang2,yrang1)

	implicit none
         COMMON/PLT1/LXT,LYT,LT
         COMMON/C2/UNITS,LUMMSG,RADMSG

         INTEGER*4 ICNT,P,NLAB,IFLAG,NPTS,IRETICLE
         INTEGER*4 I,II,IMAX,IMIN,LT,LXT,LYT,NN,ntbl
	 INTEGER*4 nltitle,nxtitle,ntitle(80),plotwid,plotht,isize
	INTEGER*4 nytitle,nplotgpi,nplotgpi2,nploteps
         integer*4 ncolx,ncoly,unit97,unit98
         REAL*4 Y(30),X(30),Y2(30),ZBUF(30),TOS,labstep,tmp
         REAL*8 DY(30),DX(30),DY2(30),DLTX,RMS
	real*4 xrang1,xrang2,yrang1,yrang2
         LOGICAL*4 LASTFLAG,epsplot

	character*80 plotgpi,plotgpi2,ploteps,tbl
         CHARACTER*8  UNITS
         CHARACTER*40 LUMMSG
         CHARACTER*34 RADMSG
         CHARACTER*18 YTITLE
         CHARACTER*4320 LABEL
         CHARACTER*51 ARMES
         CHARACTER*12 MSG, LMSG(7)
         CHARACTER*41 RMSG
         CHARACTER*63 MS2
         CHARACTER*62 GMS2
         CHARACTER*80 CMS1
         CHARACTER*80 ltitle
         CHARACTER*86 TITLE(80)
         CHARACTER*20 XTITLE
         CHARACTER*28 OMES
         CHARACTER*30 DLTAX
         CHARACTER*30 RESMG

         ICNT = 1
         CMS1 = ' '
         LTITLE = 'CCD LIGHT TRANSFER ANALYSIS'
         nltitle = index(ltitle,'    ') - 1
         XTITLE = 'ENERGY'
         nxtitle = index(xtitle,'    ') - 1
         LUMMSG = 'ENERGY UNIT = FOOT-LAMBERT-MILLISECONDS'
         RADMSG = 'ENERGY UNIT = PICOAMP-MILLISECONDS'
	
	 nytitle = 18
         IMIN=0
         IMAX=0
         LXT=6
         LYT=7
         IF (IFLAG .EQ. 2) LYT=18
         LT=31

         NN = MIN0(NLAB,25)

         DO P=1,NN
            TITLE(ICNT)=LABEL(1+(P-1)*72:1+(P-1)*72+71)
	    ntitle(icnt) = index(title(icnt),'    ') - 1
            ICNT = ICNT + 1
         ENDDO

         WRITE(RESMG,'(A17,F9.4,A3)') 'RMS RESIDUAL FIT=',RMS,' DN'
         WRITE(DLTAX,'(A8,F9.5,A12)') 'DELTA X=',DLTX,' ENERGY UNIT'
         CMS1(31:60)=DLTAX
         WRITE(OMES,'(A15,F9.5,A3)') 'SHUTTER OFFSET=',TOS,' MS'
         IF (UNITS .EQ. 'RADIANCE') THEN
            TITLE(ICNT)=RADMSG
	    ntitle(icnt) = index(title(icnt),'    ') - 1
            ICNT = ICNT + 1
         ELSE
            TITLE(ICNT)=LUMMSG
            ntitle(icnt) = index(title(icnt),'    ') - 1
            ICNT = ICNT + 1
         ENDIF
         CMS1(1:27)=OMES
         TITLE(ICNT)=CMS1
	ntitle(icnt) = index(title(icnt),'    ') - 1
         ICNT = ICNT + 1
         TITLE(ICNT)=RMSG
         ntitle(icnt) = index(title(icnt),'    ') - 1
         ICNT = ICNT + 1
         TITLE(ICNT)=ARMES
	ntitle(icnt) = index(title(icnt),'    ') - 1
         ICNT = ICNT + 1
         TITLE(ICNT)=MS2
	ntitle(icnt) = index(title(icnt),'    ') - 1
         ICNT = ICNT + 1
         TITLE(ICNT)=RESMG
	ntitle(icnt) = index(title(icnt),'    ') - 1
         ICNT = ICNT + 1
         TITLE(ICNT)=GMS2
	ntitle(icnt) = index(title(icnt),'    ') - 1
ccc         CALL HEADER (TITLE,ICNT,0) ! 0=left justify, 1=center justify, 2=right
ccc         CALL AXESTITLES(XTITLE,YTITLE,270,' ',0)

C--  GET THE SIZE OF THE PLOT.
c       default
        plotwid =  648  !640 @72dpi = 8.888.. inches    9 inch = 648
        plotht  =  648  !480 @72dpi = 6.666.. inches    7 inch = 504
        labstep = 0.02          !for size 9 font --- originally 0.04

        if (icnt .gt. 8) then
           tmp = icnt/8
           plotht = int(plotht * tmp)
           labstep = labstep/tmp
        endif

        isize = 10
        ncoly = ncoly + 1
c	print *,'xrang1,xrang2,yrang1,yrang2 = ',xrang1,xrang2,yrang1,yrang2

            call write_gpi_1(unit98,tbl,ntbl,ytitle,nytitle,
     1 xtitle,nxtitle,isize,plotwid,plotht,
     2 ploteps,nploteps)
        if (epsplot) then
                call write_gpi_1(unit97,tbl,ntbl,ytitle,nytitle,
     1 xtitle,nxtitle,isize,plotwid,plotht,
     2 ploteps,nploteps)
        endif

c	xrang1 = 0.0
c	yrang1 = 0.0
	    call write_gpi_2(unit98,ltitle,nltitle,xrang1,xrang2,
     1 yrang1,yrang2,isize)
	if (epsplot) then
	    call write_gpi_2(unit97,ltitle,nltitle,xrang1,xrang2,
     1 yrang1,yrang2,isize)
	endif


        call write_gpi_lab(unit98,icnt,title,ntitle,labstep,isize)
        if (epsplot) then
           call write_gpi_lab(unit97,icnt,title,ntitle,labstep,isize)
        endif

	ncolx = 2

	call write_data_gpi(unit98,tbl,ntbl,ncolx,ncoly,ireticle,isize,msg)
	if (epsplot) then
	    call write_data_gpi(unit97,tbl,ntbl,ncolx,ncoly,ireticle,isize,msg)
	endif
	
	goto 899
         DO I=1,NPTS
            Y(I)  = DY(I)
            Y2(I) = DY2(I)
            X(I)  = DX(I)
            ZBUF(I)=0.0000
c	print *,'i,Y(I),Y2(I),X(I) = ',i,Y(I),Y2(I),X(I)
         ENDDO

         X(NPTS+1)=0.0
         X(NPTS+2)=1.0

         IF (IFLAG .EQ. 2) THEN
            DO I=1,NPTS
               IF (Y(I) .GT. 1.00) IMAX=1
               IF (Y(I) .LT. -1.00) IMIN=1
            ENDDO
            IF ((IMIN .EQ. 1) .OR. (IMAX .EQ. 1)) THEN
               ZBUF(NPTS+1)=0.0
               ZBUF(NPTS+2)=1.0
            ELSE
               Y(NPTS+1)=0.0 
               Y(NPTS+2)=1.0
               ZBUF(NPTS+1)=0.0
               ZBUF(NPTS+2)=1.0
            ENDIF
         ELSE
            Y(NPTS+1)=0.00
            Y(NPTS+2)=1.0
            Y2(NPTS+1)=0.0
            Y2(NPTS+2)=1.0
         ENDIF

	DO I=1,NPTS
c	print *,'i,Y(I),Y2(I),X(I) = ',i,Y(I),Y2(I),X(I)

	ENDDO
         DO II=1,7
            LMSG(II)=' '
         ENDDO
         LMSG(IRETICLE+1)=MSG
ccc         call setlabel (LMSG,7,1,2) ! 7=# of symbols 1=verticle 2=position east
ccc         CALL SETACTIVESET(IRETICLE)
 
ccc         CALL LINE(X,Y,NPTS,1,1,1)             !DATA LINE
ccc         IF (IFLAG .EQ. 2) CALL LINE(X,ZBUF,NPTS,1,0,0) !RESIDUAL
ccc         IF (IFLAG .EQ. 0) CALL LINE(X,Y2,NPTS,1,1,4)   !RESIDUAL
ccc         IF (.NOT. LASTFLAG) THEN
ccc            CALL XRTPAGE(STATUS)
ccc            IF (STATUS .NE. 1) THEN
ccc               CALL XVMESSAGE ('*** Incomplete Ploting Operation.',' ')
ccc               CALL XVMESSAGE ('XRT Window Button Definitions:',' ')
ccc               CALL XVMESSAGE
ccc     &            ('SAVE = write to output PostScript file.',' ')
ccc               CALL XVMESSAGE ('PAGE = display next graph.',' ')
ccc               CALL XVMESSAGE ('EXIT = terminate application.',' ')
ccc               CALL MABEND('***Unable to PAGE plotter',' ')
ccc            ENDIF
ccc         ENDIF
899	continue
         RETURN

      END
C**************************************************************************
      SUBROUTINE LABPROC(IUNI,LABEL,NLAB)

         IMPLICIT NONE 

         INTEGER*4 INSTANCES(20)
         INTEGER*4 NHIST, J, I             ! loop control variables
         INTEGER*4 NLAB, STAT              ! store # of labels and status
         INTEGER*4 IUNI                    ! store file id number
 
         CHARACTER*8 TASKS(20)
         CHARACTER*4320 LABEL 
         CHARACTER*132 MSG
         CHARACTER*12 UNAME
         CHARACTER*28 TIME
         CHARACTER*65 HBUF

C        initialize display and storage buffers
         HBUF = '----TASK:------------USER:------------------------'
         MSG = ' '
         LABEL = ' '

         CALL VIC1LAB(IUNI,STAT,NLAB,LABEL,0)   ! retrive VICAR 1 label
                                                ! NLAB=0 if no VIC1LAB

         NHIST = 20                             ! EXTRACTS VIC*2 LAB

C        retirve history tasks
         CALL XLHINFO(IUNI,TASKS,INSTANCES,NHIST,STAT,' ')
 
C        for each task, extract the USER name and the DATE_TIME,
C        write the TASK name, USER name, and DATE_TIME onto buffer (HBUF)
         DO J=1,NHIST
            UNAME = ' '
            TIME = ' '
            CALL XLGET(IUNI,'HISTORY','USER',UNAME,STAT,'HIST',
     &                 TASKS(J),'INSTANCE',INSTANCES(J),'FORMAT',
     &                 'STRING',' ')
            CALL XLGET(IUNI,'HISTORY','DAT_TIM',TIME,STAT,'HIST',
     &                 TASKS(J),'INSTANCE',INSTANCES(J),'FORMAT',
     &                 'STRING',' ')
            DO I=1,8                             !BLANKS NULLS
               IF (TASKS(J)(I:I) .LT. '0') TASKS(J)(I:I)=' '
            ENDDO
            HBUF(10:17)=TASKS(J)
            HBUF(27:38)=UNAME
            HBUF(39:63)=TIME
            LABEL(1+(NLAB+J-1)*72:(1+(NLAB+J-1)*72)+64)=HBUF
         ENDDO

         NLAB=NLAB+NHIST   ! calculate number of labels to be display

C        output history tasks retrived from the label
         DO I=1,NLAB
            MSG(1:72) = LABEL((1+(I-1)*72):(1+(I-1)*72)+71)
            CALL XVMESSAGE (MSG,' ')
            MSG = ' '
         ENDDO
         RETURN
      END
C*********************************************************************
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
10100 format('# Created by program ccdslope')              !#'s are ignored in gnuplot
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
c	want to write on boundaries
c

10141 format("unset clip")                         !how to deal with points out of range
!            write(unit,fmt=10142,iostat=jj,err=995)
c10142 format("set clip one")                            !how to deal with connecting lines out of range
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

        psize = isize
        if (unit .eq. 97) psize = 16

10145 format('set title "',a,'" font "Ariel,',i2,'"')
             write(unit,fmt=10145,iostat=jj,err=995) title(1:ntitle), psize
C
C---- SET THE SCALING PARAMETERS.
C
c       set y range to be like vicar image - lines counting downward from top
c
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
C**************************************************************************
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
        subroutine write_data_gpi(unit,tbl,ntbl,ncolx,ncoly,iplot,isize,msg)
c
c       write out data in gpi form for pts, lines or linepoints
c       for a 1 value in control column
c
        implicit none
        integer*4 unit,ntbl,isize,psize
        integer*4 gcol,jj,iplot
        integer*4 ncolx,ncoly
        CHARACTER*12 MSG
        character*80 tbl
        character*100 outline

c       bring in gunplot colors, lines, points, etc
        include 'gnuplotchar'

        psize = isize
        if (unit .eq. 97) psize = 16

        gcol = iplot
10253 format ("plot  '",a,"' u ",i2,":",i2," t '",a,"' w linespoints lt ",i2," pt ",i2,
     1 " ps 2 lw 2 lc rgb '",a,"'")
           write(unit,fmt=10253,iostat=jj,err=995) tbl(1:ntbl),ncolx,ncoly,msg,
     1 lntype(gcol),pttype(gcol),ptcolor(gcol)(1:ptcolorl(gcol))


        if (unit.eq.98) then
10255 format("pause mouse any")                 !allows plot to display on screen until mouse click
           write(unit,fmt=10255,iostat=jj,err=995)
        endif

        return
995     continue
        if (unit.eq.97) then
            call xvmessage('??E - write_data_gpi: Error writing gnuplot eps file',' ')
        write (outline,19999) jj
19999 format ('iostat = ',i8)
            call xvmessage (outline,' ')
            call abend
        else
            call xvmessage('??E - write_data_gpi: Error writing gnuplot file',' ')
        write (outline,19999) jj
            call xvmessage (outline,' ')
            call abend
        endif
        return
        end

C**************************************************************************

      subroutine GENTBL(DN,CDN,W,DNX,CDNX,WX,npts,table_ds,*)
	implicit none

         INTEGER*4 NPTS,JST,L

         REAL*8 DN(30,5),CDN(30,5),W(30,5)
         REAL*8 DNX(NPTS),CDNX(NPTS),WX(NPTS)

         CHARACTER*40 TABLE_DS
         CHARACTER*1 TAB
C
C-------use last point of full frame buffer to calculate format
c        val = max(dnx(npts),wx(npts),cdnx(npts))
c        lod = ifix(log10(val)) + 1  !number of places left of decimal
c        nc = lod + 5      !add 1 for pad, 1 for sign, 1 for point,
C                          ! and 2 for places right of decimal
c  VARIABLES IN FORMAT STATEMENTS DON'T WORK ON LINUX (SEE STATEMENT 9,
C  BELOW), SO ABOVE CODE HAS BEEN DISABLED.  -LWK / 28JAN05

C** Note:
C   RECL should be ignored for sequential file in FORTRAN 77.  
C   However, it will crash on VMS if the output string is too long.  
C   Therefore, RECL is specified to patch this problem.  UNIX will 
C   generate a Warrning message when it sees RECL, but it does 
C   not affect the output and the numerical computation of this program.
C   A notice has been summited to DEC in regard to this problem. 
C   -- TXH
C**
c        OPEN(12,FILE=TABLE_DS,STATUS='UNKNOWN',RECL=300,
c    &        IOSTAT=JST,ERR=999)
c   The above form causes a crash on Linix, replace with:
        OPEN(12,FILE=TABLE_DS,STATUS='UNKNOWN',
     &        IOSTAT=JST,ERR=999)

         TAB = CHAR(9)

         WRITE(12,10)
     &      'UL_MEAN_DN',TAB,'UL_COMPUTED_DN',TAB,'UL_ENERGY',TAB,
     &      'UR_MEAN_DN',TAB,'UR_COMPUTED_DN',TAB,'UR_ENERGY',TAB,
     &      'LL_MEAN_DN',TAB,'LL_COMPUTED_DN',TAB,'LL_ENERGY',TAB,
     &      'LR_MEAN_DN',TAB,'LR_COMPUTED_DN',TAB,'LR_ENERGY',TAB,
     &      'CENT_MEAN_DN',TAB,'CENT_COMPUTED_DN',TAB,'CENT_ENERGY',TAB,
     &      'FRAME_MEAN_DN',TAB,'FRAME_COMPUTED_DN',TAB,'FRAME_ENERGY'
10       FORMAT(1X,4(A10,A1,A14,A1,A9,A1),A12,A1,A16,A1,A11,A1,
     &          A13,A1,A17,A1,A12)
	
         DO L=1,NPTS
            WRITE(12,9) 
     &         DN(L,1),TAB,CDN(L,1),TAB,W(L,1),TAB,
     &         DN(L,2),TAB,CDN(L,2),TAB,W(L,2),TAB,
     &         DN(L,3),TAB,CDN(L,3),TAB,W(L,3),TAB,
     &         DN(L,4),TAB,CDN(L,4),TAB,W(L,4),TAB,
     &         DN(L,5),TAB,CDN(L,5),TAB,W(L,5),TAB,
     &         DNX(L),TAB, CDNX(L), TAB,WX(L)
	 ENDDO

	 CLOSE(12)
	 RETURN

c9        FORMAT(18(F<nc>.2,A1))
9        FORMAT(18(F8.2,A1))

999      CALL XVMESSAGE('??E - ERROR OPENING TABLE FILE',' ')
         CALL PRNT(4,1,JST,' IOSTAT =.')
         RETURN 1
      END

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create ccdslope.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM ccdslope

   To Create the build file give the command:

		$ vimake ccdslope			(VMS)
   or
		% vimake ccdslope			(Unix)


************************************************************************/


#define PROGRAM	ccdslope
#define R2LIB

#define MODULE_LIST ccdslope.f

#define FTNINC_LIST gnuplotchar

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

/* #define DEBUG	 disable on delivery */
$ Return
$!#############################################################################
$PDF_File:
$ create ccdslope.pdf
PROCESS HELP=*
PARM INP     TYPE=STRING  COUNT=1
PARM OUT     TYPE=STRING  COUNT=(0:1)                 DEFAULT=--
PARM PLOT    TYPE=STRING  COUNT=(0:1)                 DEFAULT=--
PARM PLOTFMT TYPE=STRING  COUNT=(0:1) VALID=(GNUPLOT,EPS) DEFAULT=GNUPLOT
PARM OFFSETS TYPE=STRING  COUNT=(0:1)                 DEFAULT=--
PARM MOFSET  TYPE=REAL    COUNT=(0:1)                 DEFAULT=1.359
PARM SIGTOL  TYPE=INTEGER COUNT=(0:1) VALID=(1:2)     DEFAULT=2
PARM SUBDC   TYPE=KEYWORD COUNT=(0:1) VALID=SUBDC     DEFAULT=--
PARM EXTEXPO TYPE=INTEGER COUNT=(0:1) VALID=(2:30)    DEFAULT=30
PARM REJECT  TYPE=INTEGER COUNT=(0:1) VALID=(0:3)     DEFAULT=1
PARM DELTAX  TYPE=KEYWORD COUNT=(0:1) VALID=DELTAX    DEFAULT=--
PARM UNITS   TYPE=KEYWORD COUNT=(0:1)                 DEFAULT=RADIANCE +
                                      VALID=(RADIANCE,LUMINANC)
PARM LIGHT   TYPE=REAL    COUNT=(0:1)                 DEFAULT=--
PARM TABLE   TYPE=STRING  COUNT=(0:1)                 DEFAULT=--
!PARM NODISP  TYPE=KEYWORD COUNT=(0:1) VALID=NODISP   DEFAULT=--
END-PROC
.TITLE
VICAR PROGRAM CCDSLOPE
.HELP
PURPOSE:
 
CCDSLOPE measures the light transfer properties of a CCD camera system.
The camera's response to light is assumed to be linear, and the slope
and offset of the light transfer curve are determined by the program.
 
The program is one of a series of programs originally developed to support
radiometric calibration of the Galileo SSI camera system.
 
References:
 D-4264  MIPL Software Structural Design for the Instrument
         Calibration of GLL SSI Science Processing.
 D-tbd   Software Design Document for Instrument Calibration -
         Cassini ISS
 

.PAGE
EXECUTION:
 
         CCDSLOPE INP=LTF.DAT OUT=MARK.DAT user-parameters...
 
The input is a Light Transfer File (LTF) containing statistical data for
specified areas in the image for each exposure of a light transfer
sequence.  The LTF must have been previously initialized via LTGEN and
loaded with data via MOMGEN.
 
The output is an optional MARK-format tiepoint data set containing the
centers of all rejected areas.
 
.PAGE
MATHEMATICAL BACKGROUND:
 
For a linear camera system, the light transfer curve has the following
form:
                d = m*e + b
 
where
        d = output signal (DN)
        e = incident light energy (foot-lambert-milliseconds (UNITS=LUMINANC) or
                    picoamp-milliseconds (UNITS=RADIANCE))
 
and m and b are the slope and offset terms to be determined.
The incident light energy is computed from
 
                e = L*t
 
where
        L = light canon setting (foot-lamberts (UNITS=LUMINANC) or
                                 picoamp (UNITS=RADIANCE))
        t = exposure time (milliseconds)
 
CCDSLOPE performs a least squares fit to solve for m and b, given data
points d  acquired at exposure times t .
        i                             i
 
.PAGE
OPERATION:

CCDSLOPE performs the following steps:
 
  1) Read data from the Light Transfer File.
  2) Compute slope and offset for each area.
  3) Compute mean value for slope and offset and flag all areas
     deviating by more than 2 sigma from the mean.
  4) Re-compute the mean value for slope and offset, ignoring all
     flagged values as specified by the REJECT parameter.
 
The light transfer curve slope and offset for a given area is determined
by computing the mean DN and energy at each exposure time, and fitting
the resulting data points (d ,t ) via least squares.
                            i  i
 
The dark current frame (EXPO=0.0) is normally included as a data point
on the curve.  For GLL light transfer sequences which contain extended
exposure mode frames, an adjustment is required to correct for differences
between the normal and extended exposure dark current.  This correction
consists of subtracting the extended exposure dark current and adding
the normal dark current to the mean DN's of all extended exposure frames.
 
If the keyword SUBDC is specified, the dark current is subtracted from the
mean DN at each exposure.  If extended exposure frames exist, then the
extended exposure dark current is subtracted the the mean DN's at these
exposures.
 
If extended exposure mode frames exist (possible for Galileo data), the
EXTEXPO parameter must normally be specified to indicate the exposure
level at which the extended exposures begin.
 
   Example:  EXTEXPO=7 specifies that the 7th exposure level (above the
             dark current) begins the extended exposures.
 
However, light-transfer sequences consisting entirely of extend-exposure
frames should be input as if they were normal exposures, i.e. the extended-
exposure dark-current should be inserted in place of the normal dark-current
and the EXTEXPO parameter should not be used.
 
The OFFSETS parameter is used to specify a shutter-offset file (as generated
by program CCDRECIP) containing the shutter-offset for each line or sample
of the image (800 values for GLL or 1024 for Cassini).  These shutter-offsets
are used to correct for differences between the actual and commanded
exposures.  If the OFFSETS parameter is not specified, then a mean shutter
offset is used (see MOFSET parameter).
 
Note:  The shutter-offset file supplied must contain one and only one
entry for each line or sample of the image data.  The 800 entries in
Galileo's line-dependent shutter-offset file correspond to the 800 lines
of a Galileo image.  The 1024 entries in the Cassini sample-dependent
shutter-offset file correspond to the 1024 samples of a Cassini image.
 
The MOFSET parameter may be used to specify a mean shutter-offset.  If
line- or sample-dependent shutter-offsets are input via the OFFSETS
parameter, then the mean shutter-offset is computed by averaging these
offsets (overriding the MOFSET parameter).
 
If the REJECT parameter is specified (default=1), areas may be rejected
because of a bad value for the slope (REJECT=1), offset (REJECT=2),
or either (REJECT=3).  If REJECT=0, no area rejection is performed.
 
If the keyword DELTAX is specified, the above steps are repeated
after adjusting the energy levels so that the resulting curve
passes through the origin.  This is accomplished by adding the
ratio OFFSET/SLOPE to each energy term.
 
CCDSLOPE prints out the following:
 
  1) Slope and offset for each area specified.
  2) Summary of all areas flagged for bad slope or offset.
  3) A table listing mean DN vs input light energy for 6 regions.
  4) A global value for the slope and offset, computed by combining
     the data for all the good areas (and performing a fit.
 
If an output file is specified, then the centers of all flagged values
as specified by the REJECT parameter are stored in a MARK format data
set.
 
If a PLOT file is specified, a plot of mean DN vs input light energy
is output to the file.  The plot is generated for the four corners and
center of the frame, and for the entire frame.
 
The illumination value specified by the LIGHT parameter is used to convert the
exposure times (msec) into incident energy (Foot-Lambert-msec (UNITS=LUMINANC)
or picoamp (UNITS=RADIANCE)).  The illumination value of for this Light 
Transfer File may be defaulted such that it is read from the label of the 
file (for Cassini).  Parameter input will always override the value in the 
label.
 
If an output file is specified for the TABLE parameter, an ASCII file of
tab-delimitted text is output.  This file will contain a tabular list of the
data contained in the plots.  For each of 6 areas (Upper left, Upper right,
Lower left, Lower right, Center and Whole frame), the following values are
tabulated for all exposure levels present in the LT file:
 
 Mean DN, DN computed from derived Slope and Offset, Energy
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
EXAMPLE:
 
   CCDSLOPE LTF.DAT OUT=MRK.DAT OFFSETS=OFFSETS.DAT PLOT=CCDSLOPE.PLT
   MARK (PIC,MRK.DAT) A                 !Scribe boxes around bad centers
 
where PIC is one of the flat field frames used in the calibration.
 
.PAGE
HISTORY:

  13 Jul 2013 R. J. Bambery  Adjusted eps format to more readable fonts
                             Remove vestiges of debug statments   
  07 Jul 2013 R. J. Bambery  Refine plot ranges
  13 Feb 2013 R. J. Bambery  Documentation and test updates
  23 Nov 2012 R. J. Bambery  Linux 64-bit, Gnuplot
  28 Jan 2005 -lwk- Changed & to * in alternate returns, disabled variable
                 use in format statement in subr.GENTBL, for Linux.
 27 Apr 99  gmy  Declared P an integer to avoid compiler err on SGI.
  8 Apr 97...T.Huang.......Ported from VAX to UNIX to support CASSINI
 30 Jan 96...c.c.avis......Clarified table headers
 22 Aug 95...c.c.avis......Added tests with noise
 24 Jul 95...J.R.Yoshimizu.Look for RADIANCE not LUMINANCE in label.  Changed
                           format of statistics.
 14 Jan 95...J.R.Yoshimizu.changed LUMINANC to LIGHT.  Added UNITS
 23 Jan 95...C.C.Avis......changed calculation of SIGMAs
 25 Jun 94...C.C.Avis......Add tabular output, read Cassini labels for
                           Luminance, change LIGHT to LUMINANC, allow
                           for line- or sample-dependent shutter-offset
                           file, add comments
    Jun 91...W.P.Lee ......Strengthen Level-2 Variable HELP Descriptions
                           (FR #64654)
                           Also, resolve the EMPTY PLOT file problem that
                           LKW encountered
  3 Mar 88...G.M.Yagi......Change PDF to treat all EXTEXPO call.
 01 Nov 87...G.M.Yagi......Convert to new CPLT plotting routines
 25 Sep 87...G.M.Yagi......Fix summation mode plots
 01 Jun 87...G.M.Yagi......Add plot of RETICLES
 25 AUG 86...G.M.Yagi......Code and documentation clean-up
 19 FEB 85...M.E.MORRILL...Redefined iteration for extended expo modes.
 18 FEB 85...M.E.MORRILL...Adding sigma tolerance parameter
 27 JAN 85...M.E.MORRILL...Adding iterative loop control
 17 JAN 85...M.E.MORRILL...Adding mark output for rejected areas
 31 OCT 84...M.E.MORRILL...INITIAL RELEASE
 
.LEVEL1
.VARIABLE INP
 STRING COUNT=1
 The Light Transfer File.
.VARIABLE OUT
 STRING--OPTIONAL
 MARK formated locations
 of rejected areas.
.VARIABLE PLOT
 STRING--OPTIONAL
 Specifies output file for
 DN vs energy plot.
.VARIABLE PLOTFMT
 Output plot format
 GNUPLOT or EPS
.VARIABLE SUBDC
 KEYWORD--OPTIONAL
 Specifies dark current
 subtraction from mean DN's.
.VARIABLE EXTEXPO
 INTEGER--OPTIONAL
 Specifies extended-exposure
 level
.VARIABLE SIGTOL
 INTEGER--OPTIONAL
 Specifies Sigma Tolerance
 level
.VARIABLE REJECT
 INTEGER--OPTIONAL
 The index for REJECTION
.VARIABLE MOFSET
 REAL--OPTIONAL
 Mean shutter offset
.VARIABLE OFFSETS
 STRING--OPTIONAL
 Specifies shutter-offset file.
.VARIABLE DELTAX
 KEYWORD--OPTIONAL
 Specifies whether repetitive
 adjustment of energy levels is
 needed to force the LT Curve
 passing through origin
.VARIABLE UNITS
 Specifies whether the
 illumination values are
 RADIANCE or LUMINANC
.VARIABLE LIGHT
 REAL OPTIONAL
 Illumination value in
 Relative Foot-Lamberts
 or picoamp
.VARIABLE TABLE
 Specifies output table
 of plot data
.LEVEL2
.VARIABLE INP
 STRING COUNT=1
 The Light Transfer File created by LTGEN and MOMGEN containing area statistics
 for calculating the slope and offset.
.VARIABLE OUT
 STRING--OPTIONAL
 Mark formated tiepoint data set containing (line,sample) locations of centers
 of all rejected areas (see REJECT parameter).
.VARIABLE OFFSETS
 STRING-OPTIONAL
 Specifies shutter-offset file (as generated by program CCDRECIP) containing
 the shutter-offsets for each line or sample of the image.  These
 shutter-offsets are used to correct for differences between the actual and
 commanded exposures.  If the OFFSETS parameter is not specified, then a
 mean shutter offset is used (see MOFSET parameter).
.VARIABLE MOFSET
 REAL--OPTIONAL
 The mean shutter offset as determined by CCDRECIP.
 If line- or sample-dependent shutter offsets are input via the OFFSETS
 parameter, then the mean shutter offset is computed by averaging these
 offsets (overriding the MOFSET parameter).
.Variable SIGTOL
 Integer--Optional
 Tolerance for Sigma Rejection from the mean values.
 Default SIGTOL=2.
.Variable SUBDC
 Keyword--Optional
 Specifies if the dark current is to be subtracted.
 If the keyword SUBDC is enabled, the dark current is subtracted from the
 mean DN at each exposure.  If extended exposure frames exist, then the
 extended exposure dark current is subtracted the the mean DN's at these
 exposures.
 Default is --, i.e., NO Dark Current Subtraction.
.Variable EXTEXPO
 Integer--Optional
 If extended exposure frames exist, the EXTEXPO parameter must normally be
 specified to indicate the exposure level at which the extended exposures
 begin.  For example, EXTEXPO=7 specifies that the 7th exposure level (above
 the dark current) begins the extended exposures.
 Default is EXTEXPO=30
.Variable REJECT
 Integer--Optional
 The value of REJECT specifies how the "bad areas" shall be rejected:
   REJECT=0  No area rejection applied
   REJECT=1  Reject areas with bad slopes
   REJECT=2  Reject areas with bad offsets
   REJECT=3  Reject areas with either bad slope or offset
 The Default is REJECT=1.
.Variable DELTAX
 Keyword--Optional
 If the keyword DELTAX is specified, the processing steps are repeated
 after adjusting the energy levels so that the resulting curve passes
 through the origin.  This is accomplished by adding the ratio OFFSET/SLOPE
 to each energy term.
.Variable PLOT
 String--Optional
 If PLOT filename is specified, a plot of mean DN vs input light energy will
 be generated.  The plot is generated for the four corners and center of the
 frame, and for the entire frame.
.VARIABLE PLOTFMT
 String--Optional
 Output plot format
 GNUPLOT or EPS
.Variable UNITS
 Keyword--Optional
Specifies whether the illumination value is in LUMINANC (Relative-Foot-
Lamberts ) or RADIANCE (picoamp).
(UNITS should be LUMINANC for Galileo and RADIANCE for Cassini).
.Variable LIGHT
 Real--Optional
 A light intensity scaling parameter in units of Relative Foot Lamberts
 (UNITS= LUMINANC) or  picoamp (UNITS=RADIANCE))
 Typical value is on the order of ~1.  This value describes the incident
 light energy present during the Light Transfer test.  This value may be
 read from the label of the Light Transfer File, but will be overridden by
 the value given by parameters.
.VARIABLE TABLE
 String--Optional
 If TABLE an output table of plot data will be written to the file specified.
 The table is tab-delimitted ASCII text.  It contains data for the four
 corners, the center of the frame, and for the entire frame.  It lists Mean_DN,
 Computed_DN, and Energy for each exposure level.
.end

$ Return
$!#############################################################################
$Test_File:
$ create tstccdslope.pdf
procedure
parm    mode    type=keyword count=(0:1) valid=(batch,nobatch,inter) default=batch

local   afidsroot   type=string count=1

! Feb 18, 2013 - RJB
! TEST SCRIPT FOR TSTCCDSLOPE
! tests HALF images
!
! Vicar Programs:
!       translog f2 copy label-replace createfile
!       addtofile reset ltgen momgen2 gausnois
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
!            
! Requires external test data: 
!   cartlab or mipl dependent pointers
!
!local dir string
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
 
!CASSINI TEST:
!---------------------------
! Make TEST.LTF a test light transfer file which has exposure levels of
! 0,10,20,40 and each input frame was 10,110,210,410 dn respectively.
! So, with a 0.0 shutter_offset entered and a LIGHT of 10, the slopes
! should be a perfect 1.0 and the offsets 0.0 with residuals of 0.0.
 
!Set dns to 10 and replicate
copy cas/sum2.1 l1.ax
f2 l1.ax l1.a func=10
copy l1.a l1.b
copy l1.a l1.c
 
!Set dns to 110 and replicate - set exposure to 10
f2 cas/sum2.1 l2.a func=110
label-rep l2.a 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=10."
copy l2.a l2.b
copy l2.a l2.c
 
!Set dns to 210 and replicate - set exposure to 20
f2 cas/sum2.1 l3.a func=210
label-rep l3.a 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=20."
copy l3.a l3.b
copy l3.a l3.c
 
!Set dns to 410 and replicate - set exposure to 40
f2 cas/sum2.1 l4.a func=410
label-rep l4.a 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=40."
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
ltgen l1.a out=test.ltf list=l.list 'GRID
 
!Fill Light Transfer File with stats
momgen2 list=l.list ltfrcp=test.ltf

! TEST 1 
! 18 plots - each upper left, upper right, etc.
ccdslope test.ltf table=ccdslope1.tbl mofset=0.0 light=10. rej=0 'SUBDC +
    plot=test1

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test1.gpi
end-if

!---------------------------
! Repeat using noise-added input data.
! Make TEST.LTF a test light transfer file which has exposure levels of
! 0,10,20,40 and each input frame was 10,110,210,410 dn respectively.
! So, with a 0.0 shutter_offset entered and a LIGHT of 10, the slopes
! should be a perfect 1.0 and the offsets 0.0 with residuals of 0.0.
 
!Set dns to 10 and replicate
gausnois a.img mean=0 sigma=3 format=HALF nl=512 ns=512 seed=512
f2 (cas/sum2.1, a.img) m1.a func=10+IN2
gausnois a.img mean=0 sigma=3 format=HALF nl=512 ns=512 seed=612
f2 (cas/sum2.1, a.img) m1.b func=10+IN2
gausnois a.img mean=0 sigma=3 format=HALF nl=512 ns=512 seed=712
f2 (cas/sum2.1, a.img) m1.c func=10+IN2
 
!Set dns to 110 and replicate - set exposure to 10
gausnois a.img mean=0 sigma=10 format=HALF nl=512 ns=512 seed=812
f2 (cas/sum2.1, a.img) m2.a func=110+IN2
label-rep m2.a 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=10."
gausnois a.img mean=0 sigma=10 format=HALF nl=512 ns=512 seed=912
f2 (cas/sum2.1, a.img) m2.b func=110+IN2
label-rep m2.b 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=10."
gausnois a.img mean=0 sigma=10 format=HALF nl=512 ns=512 seed=112
f2 (cas/sum2.1, a.img) m2.c func=110+IN2
label-rep m2.c 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=10."
 
!Set dns to 210 and replicate - set exposure to 20
gausnois a.img mean=0 sigma=14 format=HALF nl=512 ns=512 seed=212
f2 (cas/sum2.1, a.img) m3.a func=210+IN2
label-rep m3.a 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=20."
gausnois a.img mean=0 sigma=14 format=HALF nl=512 ns=512 seed=312
f2 (cas/sum2.1, a.img) m3.b func=210+IN2
label-rep m3.b 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=20."
gausnois a.img mean=0 sigma=14 format=HALF nl=512 ns=512 seed=412
f2 (cas/sum2.1, a.img) m3.c func=210+IN2
label-rep m3.c 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=20."
 
!Set dns to 410 and replicate - set exposure to 40
gausnois a.img mean=0 sigma=20 format=HALF nl=512 ns=512 seed=544
f2 (cas/sum2.1, a.img) m4.a func=410+IN2
label-rep m4.a 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=40."
gausnois a.img mean=0 sigma=20 format=HALF nl=512 ns=512 seed=566
f2 (cas/sum2.1, a.img) m4.b func=410+IN2
label-rep m4.b 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=40."
gausnois a.img mean=0 sigma=20 format=HALF nl=512 ns=512 seed=588
f2 (cas/sum2.1, a.img) m4.c func=410+IN2
label-rep m4.c 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=40."
 
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
ltgen m1.a out=test.ltfn list=m.list 'GRID
 
!Fill Light Transfer File with stats
momgen2 list=m.list ltfrcp=test.ltfn
 
if ($syschar(1)="UNIX")
   ush rm m1.*
   ush rm m2.*
   ush rm m3.*
   ush rm m4.*
   ush rm m.list
else
   dcl del m1.*;*
   dcl del m2.*;*
   dcl del m3.*;*
   dcl del m4.*;*
   dcl del m.list;*
end-if

! 
! TEST 2 - synthetic data reject 
ccdslope test.ltfn table=ccdslope2n.tbl mofset=0.0 light=10. rej=0 'SUBDC +
    plot=test2
if (mode = "nobatch" or mode = "inter")
    ush gnuplot test2.gpi
end-if

!---------------------------
! TEST 3
!go to real data and the sample-dependent shutter-offset file
 
ccdslope test.ltf mark.out table=ccdslope3.tbl offsets=cas/sos.dat rej=3 +
    plot=test3
if (mode = "nobatch" or mode = "inter")
    ush gnuplot test3.gpi
end-if

! TEST 4 - real data deltax  and sigtol
 
ccdslope test.ltf  offsets=cas/sol.dat rej=2  +
   'deltax sigtol=1.0 plot=test4

if (mode = "nobatch" or mode = "inter")
    ush gnuplot test4.gpi
end-if

! TEST 5 - encapsulated postscript file

ccdslope test.ltf  offsets=cas/sol.dat rej=1  +
   'deltax sigtol=1.0 plot=test5 plotfmt=eps
ush gnuplot test5.eps.gpi
if (mode = "nobatch" or mode = "inter")
    ush gimp test5.eps
end-if


let $echo="no"
rm>
ush rm cas
ush rm l.list
end-proc

$!-----------------------------------------------------------------------------
$ create tstccdslope_linux.log
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
if (afidsroot = "")
    ush ln -s /project/test_work/testdata/cassini/iss cas
else
end-if
  defcmd-replace typeit "ush cat"
copy cas/sum2.1 l1.ax
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
f2 l1.ax l1.a func=10
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
f2 cas/sum2.1 l2.a func=110
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 26 TIMES
label-rep l2.a 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=10."
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword EXPOSURE_DURATION replaced
copy l2.a l2.b
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
copy l2.a l2.c
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
f2 cas/sum2.1 l3.a func=210
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 26 TIMES
label-rep l3.a 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=20."
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword EXPOSURE_DURATION replaced
copy l3.a l3.b
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
copy l3.a l3.c
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
f2 cas/sum2.1 l4.a func=410
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 26 TIMES
label-rep l4.a 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=40."
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword EXPOSURE_DURATION replaced
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
NEXT FILE =     1
l1.a
l1.b
l1.c
l2.a
l2.b
l2.c
l3.a
l3.b
l3.c
l4.a
l4.b
l4.c
ltgen l1.a out=test.ltf list=l.list 'GRID
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
   let LUMS = "5.099999904633e+00"
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
   let LUMS = "5.099999904633e+00"
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
   let LUMS = "5.099999904633e+00"
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
   let LUMS = "5.099999904633e+00"
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
   let LUMS = "5.099999904633e+00"
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
   let LUMS = "5.099999904633e+00"
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
   let LUMS = "5.099999904633e+00"
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
   let LUMS = "5.099999904633e+00"
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
   let LUMS = "5.099999904633e+00"
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
   write "       2              1.000000000000e+01             5.099999904633e+00"
       2              1.000000000000e+01             5.099999904633e+00
   if (I=NLVL) break
   let I=I+1
end-loop
   let X=EX(I)
   let EXS = "2.000000000000e+01"
   let LUMS=LMS(I)
   write "       3              2.000000000000e+01             5.099999904633e+00"
       3              2.000000000000e+01             5.099999904633e+00
   if (I=NLVL) break
   let I=I+1
end-loop
   let X=EX(I)
   let EXS = "4.000000000000e+01"
   let LUMS=LMS(I)
   write "       4              4.000000000000e+01             5.099999904633e+00"
       4              4.000000000000e+01             5.099999904633e+00
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
NEXT FILE=0001
l1.a
l1.b
l1.c
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
NEXT FILE=0001
l2.a
l2.b
l2.c
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
NEXT FILE=0001
l3.a
l3.b
l3.c
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
NEXT FILE=0001
l4.a
l4.b
l4.c
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
ccdslope test.ltf table=ccdslope1.tbl mofset=0.0 light=10. rej=0 'SUBDC  +
    plot=test1
Beginning VICAR task ccdslope
CCDSLOPE version 07-Jul-2013  (64-bit) - rjb
 SIGMA REJ TOL=          2
Mean shutter-offset =  0.000E+00
Shutter-offset is LINE-dependent
DC subtraction mode
----TASK:COPY    ----USER:wlb         Mon Jun  9 15:14:37 2014
----TASK:F2      ----USER:wlb         Mon Jun  9 15:14:37 2014
----TASK:LTGEN   ----USER:wlb         Mon Jun  9 15:14:38 2014

GLOBAL VALUE FOR SLOPE...
Raw mean and sigma are...
N= 100 MEAN=     1.00000 SIGMA=     0.00000

GLOBAL VALUE FOR OFFSET...
Raw mean and sigma are...
N= 100 MEAN=     0.00000 SIGMA=     0.00000

Summary of bad areas....

    No rejected areas

Slopes and offsets for each area...

ENERGY UNIT = PICOAMP-MILLISECONDS

AREA    SL   SS   NL   NS  SLOPE (DN/ENERGY UNIT)  OFFSET (DN)
   1    15   15   20   20          1.00000            0.00000
   2    15   66   20   20          1.00000            0.00000
   3    15  117   20   20          1.00000            0.00000
   4    15  168   20   20          1.00000            0.00000
   5    15  219   20   20          1.00000            0.00000
   6    15  270   20   20          1.00000            0.00000
   7    15  321   20   20          1.00000            0.00000
   8    15  372   20   20          1.00000            0.00000
   9    15  423   20   20          1.00000            0.00000
  10    15  474   20   20          1.00000            0.00000
  11    66   15   20   20          1.00000            0.00000
  12    66   66   20   20          1.00000            0.00000
  13    66  117   20   20          1.00000            0.00000
  14    66  168   20   20          1.00000            0.00000
  15    66  219   20   20          1.00000            0.00000
  16    66  270   20   20          1.00000            0.00000
  17    66  321   20   20          1.00000            0.00000
  18    66  372   20   20          1.00000            0.00000
  19    66  423   20   20          1.00000            0.00000
  20    66  474   20   20          1.00000            0.00000
  21   117   15   20   20          1.00000            0.00000
  22   117   66   20   20          1.00000            0.00000
  23   117  117   20   20          1.00000            0.00000
  24   117  168   20   20          1.00000            0.00000
  25   117  219   20   20          1.00000            0.00000
  26   117  270   20   20          1.00000            0.00000
  27   117  321   20   20          1.00000            0.00000
  28   117  372   20   20          1.00000            0.00000
  29   117  423   20   20          1.00000            0.00000
  30   117  474   20   20          1.00000            0.00000
  31   168   15   20   20          1.00000            0.00000
  32   168   66   20   20          1.00000            0.00000
  33   168  117   20   20          1.00000            0.00000
  34   168  168   20   20          1.00000            0.00000
  35   168  219   20   20          1.00000            0.00000
  36   168  270   20   20          1.00000            0.00000
  37   168  321   20   20          1.00000            0.00000
  38   168  372   20   20          1.00000            0.00000
  39   168  423   20   20          1.00000            0.00000
  40   168  474   20   20          1.00000            0.00000
  41   219   15   20   20          1.00000            0.00000
  42   219   66   20   20          1.00000            0.00000
  43   219  117   20   20          1.00000            0.00000
  44   219  168   20   20          1.00000            0.00000
  45   219  219   20   20          1.00000            0.00000
  46   219  270   20   20          1.00000            0.00000
  47   219  321   20   20          1.00000            0.00000
  48   219  372   20   20          1.00000            0.00000
  49   219  423   20   20          1.00000            0.00000
  50   219  474   20   20          1.00000            0.00000
  51   270   15   20   20          1.00000            0.00000
  52   270   66   20   20          1.00000            0.00000
  53   270  117   20   20          1.00000            0.00000
  54   270  168   20   20          1.00000            0.00000
  55   270  219   20   20          1.00000            0.00000
  56   270  270   20   20          1.00000            0.00000
  57   270  321   20   20          1.00000            0.00000
  58   270  372   20   20          1.00000            0.00000
  59   270  423   20   20          1.00000            0.00000
  60   270  474   20   20          1.00000            0.00000
  61   321   15   20   20          1.00000            0.00000
  62   321   66   20   20          1.00000            0.00000
  63   321  117   20   20          1.00000            0.00000
  64   321  168   20   20          1.00000            0.00000
  65   321  219   20   20          1.00000            0.00000
  66   321  270   20   20          1.00000            0.00000
  67   321  321   20   20          1.00000            0.00000
  68   321  372   20   20          1.00000            0.00000
  69   321  423   20   20          1.00000            0.00000
  70   321  474   20   20          1.00000            0.00000
  71   372   15   20   20          1.00000            0.00000
  72   372   66   20   20          1.00000            0.00000
  73   372  117   20   20          1.00000            0.00000
  74   372  168   20   20          1.00000            0.00000
  75   372  219   20   20          1.00000            0.00000
  76   372  270   20   20          1.00000            0.00000
  77   372  321   20   20          1.00000            0.00000
  78   372  372   20   20          1.00000            0.00000
  79   372  423   20   20          1.00000            0.00000
  80   372  474   20   20          1.00000            0.00000
  81   423   15   20   20          1.00000            0.00000
  82   423   66   20   20          1.00000            0.00000
  83   423  117   20   20          1.00000            0.00000
  84   423  168   20   20          1.00000            0.00000
  85   423  219   20   20          1.00000            0.00000
  86   423  270   20   20          1.00000            0.00000
  87   423  321   20   20          1.00000            0.00000
  88   423  372   20   20          1.00000            0.00000
  89   423  423   20   20          1.00000            0.00000
  90   423  474   20   20          1.00000            0.00000
  91   474   15   20   20          1.00000            0.00000
  92   474   66   20   20          1.00000            0.00000
  93   474  117   20   20          1.00000            0.00000
  94   474  168   20   20          1.00000            0.00000
  95   474  219   20   20          1.00000            0.00000
  96   474  270   20   20          1.00000            0.00000
  97   474  321   20   20          1.00000            0.00000
  98   474  372   20   20          1.00000            0.00000
  99   474  423   20   20          1.00000            0.00000
 100   474  474   20   20          1.00000            0.00000
Reticle           1
Means from averaging values of areas
 SLOPE  =  1.0000000
 OFFSET =  0.000E+00

STATISTICS FOR UPPER-LEFT  CORNER

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY    (DN-DC)    (DN-DC)    (DN-DC)
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
   10.0000    10.0000    10.0000      100.0000   100.0000   100.0000    0.0000
   20.0000    20.0000    10.0000      200.0000   200.0000   200.0000    0.0000
   40.0000    40.0000    10.0000      400.0000   400.0000   400.0000    0.0000
                                                                RMS=    0.0000
SLOPE=   1.000000 SD= 0.000000 OFFSET=   0.000000 SD= 0.000000
NUMBER OF GOOD AREAS=  15 OUT OF  15 AREAS SAMPLED
NO REJECTION CRITERIA APPLIED
GALGEN:C1=   1.00000 ENERGY UNIT/DN C2=   0.00000 ENERGY UNIT
Reticle           2
Means from averaging values of areas
 SLOPE  =  1.0000000
 OFFSET =  0.000E+00

STATISTICS FOR UPPER-RIGHT CORNER

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY    (DN-DC)    (DN-DC)    (DN-DC)
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
   10.0000    10.0000    10.0000      100.0000   100.0000   100.0000    0.0000
   20.0000    20.0000    10.0000      200.0000   200.0000   200.0000    0.0000
   40.0000    40.0000    10.0000      400.0000   400.0000   400.0000    0.0000
                                                                RMS=    0.0000
SLOPE=   1.000000 SD= 0.000000 OFFSET=   0.000000 SD= 0.000000
NUMBER OF GOOD AREAS=  15 OUT OF  15 AREAS SAMPLED
NO REJECTION CRITERIA APPLIED
GALGEN:C1=   1.00000 ENERGY UNIT/DN C2=   0.00000 ENERGY UNIT
Reticle           3
Means from averaging values of areas
 SLOPE  =  1.0000000
 OFFSET =  0.000E+00

STATISTICS FOR LOWER-LEFT  CORNER

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY    (DN-DC)    (DN-DC)    (DN-DC)
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
   10.0000    10.0000    10.0000      100.0000   100.0000   100.0000    0.0000
   20.0000    20.0000    10.0000      200.0000   200.0000   200.0000    0.0000
   40.0000    40.0000    10.0000      400.0000   400.0000   400.0000    0.0000
                                                                RMS=    0.0000
SLOPE=   1.000000 SD= 0.000000 OFFSET=   0.000000 SD= 0.000000
NUMBER OF GOOD AREAS=  15 OUT OF  15 AREAS SAMPLED
NO REJECTION CRITERIA APPLIED
GALGEN:C1=   1.00000 ENERGY UNIT/DN C2=   0.00000 ENERGY UNIT
Reticle           4
Means from averaging values of areas
 SLOPE  =  1.0000000
 OFFSET =  0.000E+00

STATISTICS FOR LOWER-RIGHT CORNER

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY    (DN-DC)    (DN-DC)    (DN-DC)
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
   10.0000    10.0000    10.0000      100.0000   100.0000   100.0000    0.0000
   20.0000    20.0000    10.0000      200.0000   200.0000   200.0000    0.0000
   40.0000    40.0000    10.0000      400.0000   400.0000   400.0000    0.0000
                                                                RMS=    0.0000
SLOPE=   1.000000 SD= 0.000000 OFFSET=   0.000000 SD= 0.000000
NUMBER OF GOOD AREAS=  15 OUT OF  15 AREAS SAMPLED
NO REJECTION CRITERIA APPLIED
GALGEN:C1=   1.00000 ENERGY UNIT/DN C2=   0.00000 ENERGY UNIT
Reticle           5
Means from averaging values of areas
 SLOPE  =  1.0000000
 OFFSET =  0.000E+00

STATISTICS FOR CENTER

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY    (DN-DC)    (DN-DC)    (DN-DC)
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
   10.0000    10.0000    10.0000      100.0000   100.0000   100.0000    0.0000
   20.0000    20.0000    10.0000      200.0000   200.0000   200.0000    0.0000
   40.0000    40.0000    10.0000      400.0000   400.0000   400.0000    0.0000
                                                                RMS=    0.0000
SLOPE=   1.000000 SD= 0.000000 OFFSET=   0.000000 SD= 0.000000
NUMBER OF GOOD AREAS=  40 OUT OF  40 AREAS SAMPLED
NO REJECTION CRITERIA APPLIED
GALGEN:C1=   1.00000 ENERGY UNIT/DN C2=   0.00000 ENERGY UNIT
Framewide
Means from averaging values of areas
 SLOPE=  1.0000000
 OFFSET=  0.000E+00

STATISTICS FOR FULL FRAME

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY    (DN-DC)    (DN-DC)    (DN-DC)
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
   10.0000    10.0000    10.0000      100.0000   100.0000   100.0000    0.0000
   20.0000    20.0000    10.0000      200.0000   200.0000   200.0000    0.0000
   40.0000    40.0000    10.0000      400.0000   400.0000   400.0000    0.0000
                                                                RMS=    0.0000
SLOPE=   1.000000 SD= 0.000000 OFFSET=   0.000000 SD= 0.000000
NUMBER OF GOOD AREAS=  40 OUT OF  40 AREAS SAMPLED
NO REJECTION CRITERIA APPLIED
GALGEN:C1=   1.00000 ENERGY UNIT/DN C2=   0.00000 ENERGY UNIT
CCDSLOPE task completed
if (mode = "nobatch" or mode = "inter")
end-if
gausnois a.img mean=0 sigma=3 format=HALF nl=512 ns=512 seed=512
Beginning VICAR task gausnois
f2 (cas/sum2.1, a.img) m1.a func=10+IN2
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 443 TIMES
gausnois a.img mean=0 sigma=3 format=HALF nl=512 ns=512 seed=612
Beginning VICAR task gausnois
f2 (cas/sum2.1, a.img) m1.b func=10+IN2
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 440 TIMES
gausnois a.img mean=0 sigma=3 format=HALF nl=512 ns=512 seed=712
Beginning VICAR task gausnois
f2 (cas/sum2.1, a.img) m1.c func=10+IN2
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 435 TIMES
gausnois a.img mean=0 sigma=10 format=HALF nl=512 ns=512 seed=812
Beginning VICAR task gausnois
f2 (cas/sum2.1, a.img) m2.a func=110+IN2
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 1312 TIMES
label-rep m2.a 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=10."
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword EXPOSURE_DURATION replaced
gausnois a.img mean=0 sigma=10 format=HALF nl=512 ns=512 seed=912
Beginning VICAR task gausnois
f2 (cas/sum2.1, a.img) m2.b func=110+IN2
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 1314 TIMES
label-rep m2.b 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=10."
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword EXPOSURE_DURATION replaced
gausnois a.img mean=0 sigma=10 format=HALF nl=512 ns=512 seed=112
Beginning VICAR task gausnois
f2 (cas/sum2.1, a.img) m2.c func=110+IN2
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 1304 TIMES
label-rep m2.c 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=10."
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword EXPOSURE_DURATION replaced
gausnois a.img mean=0 sigma=14 format=HALF nl=512 ns=512 seed=212
Beginning VICAR task gausnois
f2 (cas/sum2.1, a.img) m3.a func=210+IN2
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 1786 TIMES
label-rep m3.a 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=20."
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword EXPOSURE_DURATION replaced
gausnois a.img mean=0 sigma=14 format=HALF nl=512 ns=512 seed=312
Beginning VICAR task gausnois
f2 (cas/sum2.1, a.img) m3.b func=210+IN2
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 1802 TIMES
label-rep m3.b 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=20."
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword EXPOSURE_DURATION replaced
gausnois a.img mean=0 sigma=14 format=HALF nl=512 ns=512 seed=412
Beginning VICAR task gausnois
f2 (cas/sum2.1, a.img) m3.c func=210+IN2
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 1798 TIMES
label-rep m3.c 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=20."
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword EXPOSURE_DURATION replaced
gausnois a.img mean=0 sigma=20 format=HALF nl=512 ns=512 seed=544
Beginning VICAR task gausnois
f2 (cas/sum2.1, a.img) m4.a func=410+IN2
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 2448 TIMES
label-rep m4.a 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=40."
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword EXPOSURE_DURATION replaced
gausnois a.img mean=0 sigma=20 format=HALF nl=512 ns=512 seed=566
Beginning VICAR task gausnois
f2 (cas/sum2.1, a.img) m4.b func=410+IN2
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 2441 TIMES
label-rep m4.b 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=40."
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword EXPOSURE_DURATION replaced
gausnois a.img mean=0 sigma=20 format=HALF nl=512 ns=512 seed=588
Beginning VICAR task gausnois
f2 (cas/sum2.1, a.img) m4.c func=410+IN2
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 2444 TIMES
label-rep m4.c 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=40."
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword EXPOSURE_DURATION replaced
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
NEXT FILE =     1
m1.a
m1.b
m1.c
m2.a
m2.b
m2.c
m3.a
m3.b
m3.c
m4.a
m4.b
m4.c
ltgen m1.a out=test.ltfn list=m.list 'GRID
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
momgen2 list=m.list ltfrcp=test.ltfn
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
   let LUMS = "5.099999904633e+00"
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
   let LUMS = "5.099999904633e+00"
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
   let LUMS = "5.099999904633e+00"
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
   let LUMS = "5.099999904633e+00"
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
   let LUMS = "5.099999904633e+00"
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
   let LUMS = "5.099999904633e+00"
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
   let LUMS = "5.099999904633e+00"
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
   let LUMS = "5.099999904633e+00"
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
   let LUMS = "5.099999904633e+00"
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
   write "       2              1.000000000000e+01             5.099999904633e+00"
       2              1.000000000000e+01             5.099999904633e+00
   if (I=NLVL) break
   let I=I+1
end-loop
   let X=EX(I)
   let EXS = "2.000000000000e+01"
   let LUMS=LMS(I)
   write "       3              2.000000000000e+01             5.099999904633e+00"
       3              2.000000000000e+01             5.099999904633e+00
   if (I=NLVL) break
   let I=I+1
end-loop
   let X=EX(I)
   let EXS = "4.000000000000e+01"
   let LUMS=LMS(I)
   write "       4              4.000000000000e+01             5.099999904633e+00"
       4              4.000000000000e+01             5.099999904633e+00
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
NEXT FILE=0001
m1.a
m1.b
m1.c
   write " "
 
   let $BECHO="YES"
   momgen LIST="list"1".dat" out=test.ltfn
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
NEXT FILE=0001
m2.a
m2.b
m2.c
   write " "
 
   let $BECHO="YES"
   momgen LIST="list"2".dat" out=test.ltfn
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
NEXT FILE=0001
m3.a
m3.b
m3.c
   write " "
 
   let $BECHO="YES"
   momgen LIST="list"3".dat" out=test.ltfn
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
NEXT FILE=0001
m4.a
m4.b
m4.c
   write " "
 
   let $BECHO="YES"
   momgen LIST="list"4".dat" out=test.ltfn
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
if ($syschar(1)="UNIX")
   ush rm m1.*
   ush rm m2.*
   ush rm m3.*
   ush rm m4.*
   ush rm m.list
else
end-if
ccdslope test.ltfn table=ccdslope2n.tbl mofset=0.0 light=10. rej=0 'SUBDC  +
    plot=test2
Beginning VICAR task ccdslope
CCDSLOPE version 07-Jul-2013  (64-bit) - rjb
 SIGMA REJ TOL=          2
Mean shutter-offset =  0.000E+00
Shutter-offset is LINE-dependent
DC subtraction mode
----TASK:F2      ----USER:wlb         Mon Jun  9 15:14:41 2014
----TASK:LTGEN   ----USER:wlb         Mon Jun  9 15:14:42 2014

GLOBAL VALUE FOR SLOPE...
Raw mean and sigma are...
N= 100 MEAN=     0.99986 SIGMA=     0.00220
After throwing out samples differing by 2 sigma
N=  97 MEAN=     0.99977 SIGMA=     0.00193

GLOBAL VALUE FOR OFFSET...
Raw mean and sigma are...
N= 100 MEAN=     0.02747 SIGMA=     0.44247
After throwing out samples differing by 2 sigma
N=  97 MEAN=     0.04438 SIGMA=     0.39222

Summary of bad areas....
AREA  30 (SL,SS,NL,NS)=( 117, 474,  20,  20)  ****BOTH BAD FIT*********
AREA  86 (SL,SS,NL,NS)=( 423, 270,  20,  20)  ****BOTH BAD FIT*********
AREA 100 (SL,SS,NL,NS)=( 474, 474,  20,  20)  ****BOTH BAD FIT*********


Slopes and offsets for each area...

ENERGY UNIT = PICOAMP-MILLISECONDS

AREA    SL   SS   NL   NS  SLOPE (DN/ENERGY UNIT)  OFFSET (DN)
   1    15   15   20   20          1.00016           -0.13708
   2    15   66   20   20          1.00235           -0.64333
   3    15  117   20   20          1.00243           -0.15500
   4    15  168   20   20          0.99905           -0.06375
   5    15  219   20   20          0.99747           -0.01625
   6    15  270   20   20          0.99768            0.25958
   7    15  321   20   20          1.00364           -0.52500
   8    15  372   20   20          0.99941            0.23958
   9    15  423   20   20          0.99571            0.87292
  10    15  474   20   20          1.00158           -0.50250
  11    66   15   20   20          1.00009           -0.05417
  12    66   66   20   20          0.99906            0.03083
  13    66  117   20   20          1.00140           -0.14583
  14    66  168   20   20          0.99903            0.04917
  15    66  219   20   20          1.00044           -0.03292
  16    66  270   20   20          1.00210           -0.46792
  17    66  321   20   20          0.99794            0.39083
  18    66  372   20   20          0.99999            0.11708
  19    66  423   20   20          0.99953            0.08083
  20    66  474   20   20          1.00066           -0.03750
  21   117   15   20   20          1.00069           -0.17000
  22   117   66   20   20          0.99914            0.35458
  23   117  117   20   20          0.99811            0.45708
  24   117  168   20   20          0.99966           -0.02750
  25   117  219   20   20          0.99796            0.42833
  26   117  270   20   20          0.99810            0.40917
  27   117  321   20   20          0.99800            0.47167
  28   117  372   20   20          1.00331           -0.63583
  29   117  423   20   20          0.99997           -0.08333
  30   117  474   20   20          0.99464            1.02292
  31   168   15   20   20          0.99660            0.62792
  32   168   66   20   20          1.00128           -0.20667
  33   168  117   20   20          0.99756            0.51208
  34   168  168   20   20          0.99942            0.22125
  35   168  219   20   20          0.99961            0.05208
  36   168  270   20   20          1.00043            0.26833
  37   168  321   20   20          0.99863            0.16750
  38   168  372   20   20          1.00250           -0.68958
  39   168  423   20   20          1.00204           -0.46250
  40   168  474   20   20          0.99974            0.26000
  41   219   15   20   20          0.99919           -0.15333
  42   219   66   20   20          1.00226           -0.41125
  43   219  117   20   20          1.00017            0.20000
  44   219  168   20   20          1.00149           -0.72583
  45   219  219   20   20          0.99704            0.44542
  46   219  270   20   20          1.00259           -0.42792
  47   219  321   20   20          1.00065           -0.15333
  48   219  372   20   20          0.99967           -0.01750
  49   219  423   20   20          1.00099           -0.03833
  50   219  474   20   20          1.00074           -0.49750
  51   270   15   20   20          0.99997            0.43833
  52   270   66   20   20          0.99686            0.90542
  53   270  117   20   20          0.99659            0.41833
  54   270  168   20   20          1.00200           -0.15250
  55   270  219   20   20          0.99650            0.90375
  56   270  270   20   20          1.00021           -0.20542
  57   270  321   20   20          1.00060           -0.05375
  58   270  372   20   20          0.99990            0.06208
  59   270  423   20   20          0.99934            0.13167
  60   270  474   20   20          0.99852            0.52167
  61   321   15   20   20          1.00265           -0.73083
  62   321   66   20   20          1.00015            0.25083
  63   321  117   20   20          0.99822           -0.02458
  64   321  168   20   20          1.00130           -0.21000
  65   321  219   20   20          1.00260           -0.43333
  66   321  270   20   20          0.99839            0.48000
  67   321  321   20   20          0.99902           -0.15417
  68   321  372   20   20          1.00265           -0.32708
  69   321  423   20   20          0.99865            0.54625
  70   321  474   20   20          1.00256           -0.21417
  71   372   15   20   20          1.00148           -0.36750
  72   372   66   20   20          0.99967           -0.21625
  73   372  117   20   20          0.99799            0.08958
  74   372  168   20   20          1.00079           -0.39000
  75   372  219   20   20          0.99945            0.40417
  76   372  270   20   20          0.99950            0.03542
  77   372  321   20   20          0.99694            0.67500
  78   372  372   20   20          1.00281           -0.51875
  79   372  423   20   20          1.00032            0.22292
  80   372  474   20   20          0.99552            0.88292
  81   423   15   20   20          0.99872            0.38042
  82   423   66   20   20          0.99832            0.39875
  83   423  117   20   20          0.99829            0.61208
  84   423  168   20   20          1.00285           -0.55542
  85   423  219   20   20          0.99943            0.50500
  86   423  270   20   20          1.00811           -1.57625
  87   423  321   20   20          1.00346           -0.37500
  88   423  372   20   20          1.00171           -0.22250
  89   423  423   20   20          1.00297           -0.34875
  90   423  474   20   20          1.00016            0.05958
  91   474   15   20   20          0.99920            0.34375
  92   474   66   20   20          0.99875            0.34208
  93   474  117   20   20          0.99829           -0.05958
  94   474  168   20   20          0.99715            0.22417
  95   474  219   20   20          0.99600            0.64667
  96   474  270   20   20          0.99693           -0.14958
  97   474  321   20   20          0.99913           -0.03375
  98   474  372   20   20          1.00048           -0.23958
  99   474  423   20   20          0.99761            0.37208
 100   474  474   20   20          1.00522           -1.00458
Reticle           1
Means from averaging values of areas
 SLOPE  = 0.99973634
 OFFSET = -0.01505555

STATISTICS FOR UPPER-LEFT  CORNER

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY    (DN-DC)    (DN-DC)    (DN-DC)
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
   10.0000    10.0000    10.0000      100.0000    99.9775    99.9586    0.0189
   20.0000    20.0000    10.0000      200.0000   199.9038   199.9322   -0.0284
   40.0000    40.0000    10.0000      400.0000   399.8889   399.8795    0.0095
                                                                RMS=    0.0204
SLOPE=   0.999736 SD= 0.001636 OFFSET=  -0.015056 SD= 0.295437
NUMBER OF GOOD AREAS=  15 OUT OF  15 AREAS SAMPLED
NO REJECTION CRITERIA APPLIED
GALGEN:C1=   1.00026 ENERGY UNIT/DN C2=   0.01506 ENERGY UNIT
Reticle           2
Means from averaging values of areas
 SLOPE  = 0.99977156
 OFFSET = 0.03330556

STATISTICS FOR UPPER-RIGHT CORNER

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY    (DN-DC)    (DN-DC)    (DN-DC)
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
   10.0000    10.0000    10.0000      100.0000    99.9583   100.0105   -0.0522
   20.0000    20.0000    10.0000      200.0000   200.0659   199.9876    0.0783
   40.0000    40.0000    10.0000      400.0000   399.9158   399.9419   -0.0261
                                                                RMS=    0.0564
SLOPE=   0.999772 SD= 0.002428 OFFSET=   0.033306 SD= 0.487118
NUMBER OF GOOD AREAS=  15 OUT OF  15 AREAS SAMPLED
NO REJECTION CRITERIA APPLIED
GALGEN:C1=   1.00023 ENERGY UNIT/DN C2=  -0.03331 ENERGY UNIT
Reticle           3
Means from averaging values of areas
 SLOPE  = 0.99929750
 OFFSET = 0.11980555

STATISTICS FOR LOWER-LEFT  CORNER

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY    (DN-DC)    (DN-DC)    (DN-DC)
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
   10.0000    10.0000    10.0000      100.0000   100.0802   100.0496    0.0306
   20.0000    20.0000    10.0000      200.0000   199.9334   199.9793   -0.0459
   40.0000    40.0000    10.0000      400.0000   399.8541   399.8388    0.0153
                                                                RMS=    0.0331
SLOPE=   0.999297 SD= 0.001840 OFFSET=   0.119806 SD= 0.404782
NUMBER OF GOOD AREAS=  15 OUT OF  15 AREAS SAMPLED
NO REJECTION CRITERIA APPLIED
GALGEN:C1=   1.00070 ENERGY UNIT/DN C2=  -0.11989 ENERGY UNIT
Reticle           4
Means from averaging values of areas
 SLOPE  =  1.0004043
 OFFSET = -0.03341667

STATISTICS FOR LOWER-RIGHT CORNER

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY    (DN-DC)    (DN-DC)    (DN-DC)
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
   10.0000    10.0000    10.0000      100.0000   100.0731   100.0070    0.0661
   20.0000    20.0000    10.0000      200.0000   199.9483   200.0474   -0.0992
   40.0000    40.0000    10.0000      400.0000   400.1613   400.1283    0.0331
                                                                RMS=    0.0714
SLOPE=   1.000404 SD= 0.002625 OFFSET=  -0.033417 SD= 0.464890
NUMBER OF GOOD AREAS=  15 OUT OF  15 AREAS SAMPLED
NO REJECTION CRITERIA APPLIED
GALGEN:C1=   0.99960 ENERGY UNIT/DN C2=   0.03340 ENERGY UNIT
Reticle           5
Means from averaging values of areas
 SLOPE  = 0.99994215
 OFFSET = 0.02943750

STATISTICS FOR CENTER

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY    (DN-DC)    (DN-DC)    (DN-DC)
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
   10.0000    10.0000    10.0000      100.0000   100.0266   100.0237    0.0030
   20.0000    20.0000    10.0000      200.0000   200.0134   200.0179   -0.0045
   40.0000    40.0000    10.0000      400.0000   400.0078   400.0063    0.0015
                                                                RMS=    0.0032
SLOPE=   0.999942 SD= 0.002196 OFFSET=   0.029437 SD= 0.468997
NUMBER OF GOOD AREAS=  40 OUT OF  40 AREAS SAMPLED
NO REJECTION CRITERIA APPLIED
GALGEN:C1=   1.00006 ENERGY UNIT/DN C2=  -0.02944 ENERGY UNIT
Framewide
Means from averaging values of areas
 SLOPE= 0.99985831
 OFFSET= 0.02747083

STATISTICS FOR FULL FRAME

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY    (DN-DC)    (DN-DC)    (DN-DC)
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
   10.0000    10.0000    10.0000      100.0000   100.0240   100.0133    0.0107
   20.0000    20.0000    10.0000      200.0000   199.9831   199.9991   -0.0161
   40.0000    40.0000    10.0000      400.0000   399.9761   399.9708    0.0054
                                                                RMS=    0.0116
SLOPE=   0.999858 SD= 0.002203 OFFSET=   0.027471 SD= 0.442467
NUMBER OF GOOD AREAS=  40 OUT OF  40 AREAS SAMPLED
NO REJECTION CRITERIA APPLIED
GALGEN:C1=   1.00014 ENERGY UNIT/DN C2=  -0.02747 ENERGY UNIT
CCDSLOPE task completed
if (mode = "nobatch" or mode = "inter")
end-if
ccdslope test.ltf mark.out table=ccdslope3.tbl offsets=cas/sos.dat rej=3  +
    plot=test3
Beginning VICAR task ccdslope
CCDSLOPE version 07-Jul-2013  (64-bit) - rjb
 SIGMA REJ TOL=          2
Mean shutter-offset = 0.93262106
Shutter-offset is SAMP-dependent
DC included as data point on curve
----TASK:COPY    ----USER:wlb         Mon Jun  9 15:14:37 2014
----TASK:F2      ----USER:wlb         Mon Jun  9 15:14:37 2014
----TASK:LTGEN   ----USER:wlb         Mon Jun  9 15:14:38 2014

GLOBAL VALUE FOR SLOPE...
Raw mean and sigma are...
N= 100 MEAN=     1.99551 SIGMA=     0.00163
After throwing out samples differing by 2 sigma
N=  90 MEAN=     1.99508 SIGMA=     0.00104

GLOBAL VALUE FOR OFFSET...
Raw mean and sigma are...
N= 100 MEAN=    13.68307 SIGMA=     0.17982
After throwing out samples differing by 2 sigma
N=  90 MEAN=    13.63543 SIGMA=     0.11503

Summary of bad areas....
AREA   1 (SL,SS,NL,NS)=(  15,  15,  20,  20)  ****BOTH BAD FIT*********
AREA  11 (SL,SS,NL,NS)=(  66,  15,  20,  20)  ****BOTH BAD FIT*********
AREA  21 (SL,SS,NL,NS)=( 117,  15,  20,  20)  ****BOTH BAD FIT*********
AREA  31 (SL,SS,NL,NS)=( 168,  15,  20,  20)  ****BOTH BAD FIT*********
AREA  41 (SL,SS,NL,NS)=( 219,  15,  20,  20)  ****BOTH BAD FIT*********
AREA  51 (SL,SS,NL,NS)=( 270,  15,  20,  20)  ****BOTH BAD FIT*********
AREA  61 (SL,SS,NL,NS)=( 321,  15,  20,  20)  ****BOTH BAD FIT*********
AREA  71 (SL,SS,NL,NS)=( 372,  15,  20,  20)  ****BOTH BAD FIT*********
AREA  81 (SL,SS,NL,NS)=( 423,  15,  20,  20)  ****BOTH BAD FIT*********
AREA  91 (SL,SS,NL,NS)=( 474,  15,  20,  20)  ****BOTH BAD FIT*********


Slopes and offsets for each area...

ENERGY UNIT = PICOAMP-MILLISECONDS

AREA    SL   SS   NL   NS  SLOPE (DN/ENERGY UNIT)  OFFSET (DN)
   1    15   15   20   20          1.99939           14.11181
   2    15   66   20   20          1.99717           13.86542
   3    15  117   20   20          1.99495           13.62012
   4    15  168   20   20          1.99386           13.50086
   5    15  219   20   20          1.99396           13.51184
   6    15  270   20   20          1.99421           13.53926
   7    15  321   20   20          1.99462           13.58382
   8    15  372   20   20          1.99509           13.63587
   9    15  423   20   20          1.99563           13.69576
  10    15  474   20   20          1.99627           13.76591
  11    66   15   20   20          1.99939           14.11181
  12    66   66   20   20          1.99717           13.86542
  13    66  117   20   20          1.99495           13.62012
  14    66  168   20   20          1.99386           13.50086
  15    66  219   20   20          1.99396           13.51184
  16    66  270   20   20          1.99421           13.53926
  17    66  321   20   20          1.99462           13.58382
  18    66  372   20   20          1.99509           13.63587
  19    66  423   20   20          1.99563           13.69576
  20    66  474   20   20          1.99627           13.76591
  21   117   15   20   20          1.99939           14.11181
  22   117   66   20   20          1.99717           13.86542
  23   117  117   20   20          1.99495           13.62012
  24   117  168   20   20          1.99386           13.50086
  25   117  219   20   20          1.99396           13.51184
  26   117  270   20   20          1.99421           13.53926
  27   117  321   20   20          1.99462           13.58382
  28   117  372   20   20          1.99509           13.63587
  29   117  423   20   20          1.99563           13.69576
  30   117  474   20   20          1.99627           13.76591
  31   168   15   20   20          1.99939           14.11181
  32   168   66   20   20          1.99717           13.86542
  33   168  117   20   20          1.99495           13.62012
  34   168  168   20   20          1.99386           13.50086
  35   168  219   20   20          1.99396           13.51184
  36   168  270   20   20          1.99421           13.53926
  37   168  321   20   20          1.99462           13.58382
  38   168  372   20   20          1.99509           13.63587
  39   168  423   20   20          1.99563           13.69576
  40   168  474   20   20          1.99627           13.76591
  41   219   15   20   20          1.99939           14.11181
  42   219   66   20   20          1.99717           13.86542
  43   219  117   20   20          1.99495           13.62012
  44   219  168   20   20          1.99386           13.50086
  45   219  219   20   20          1.99396           13.51184
  46   219  270   20   20          1.99421           13.53926
  47   219  321   20   20          1.99462           13.58382
  48   219  372   20   20          1.99509           13.63587
  49   219  423   20   20          1.99563           13.69576
  50   219  474   20   20          1.99627           13.76591
  51   270   15   20   20          1.99939           14.11181
  52   270   66   20   20          1.99717           13.86542
  53   270  117   20   20          1.99495           13.62012
  54   270  168   20   20          1.99386           13.50086
  55   270  219   20   20          1.99396           13.51184
  56   270  270   20   20          1.99421           13.53926
  57   270  321   20   20          1.99462           13.58382
  58   270  372   20   20          1.99509           13.63587
  59   270  423   20   20          1.99563           13.69576
  60   270  474   20   20          1.99627           13.76591
  61   321   15   20   20          1.99939           14.11181
  62   321   66   20   20          1.99717           13.86542
  63   321  117   20   20          1.99495           13.62012
  64   321  168   20   20          1.99386           13.50086
  65   321  219   20   20          1.99396           13.51184
  66   321  270   20   20          1.99421           13.53926
  67   321  321   20   20          1.99462           13.58382
  68   321  372   20   20          1.99509           13.63587
  69   321  423   20   20          1.99563           13.69576
  70   321  474   20   20          1.99627           13.76591
  71   372   15   20   20          1.99939           14.11181
  72   372   66   20   20          1.99717           13.86542
  73   372  117   20   20          1.99495           13.62012
  74   372  168   20   20          1.99386           13.50086
  75   372  219   20   20          1.99396           13.51184
  76   372  270   20   20          1.99421           13.53926
  77   372  321   20   20          1.99462           13.58382
  78   372  372   20   20          1.99509           13.63587
  79   372  423   20   20          1.99563           13.69576
  80   372  474   20   20          1.99627           13.76591
  81   423   15   20   20          1.99939           14.11181
  82   423   66   20   20          1.99717           13.86542
  83   423  117   20   20          1.99495           13.62012
  84   423  168   20   20          1.99386           13.50086
  85   423  219   20   20          1.99396           13.51184
  86   423  270   20   20          1.99421           13.53926
  87   423  321   20   20          1.99462           13.58382
  88   423  372   20   20          1.99509           13.63587
  89   423  423   20   20          1.99563           13.69576
  90   423  474   20   20          1.99627           13.76591
  91   474   15   20   20          1.99939           14.11181
  92   474   66   20   20          1.99717           13.86542
  93   474  117   20   20          1.99495           13.62012
  94   474  168   20   20          1.99386           13.50086
  95   474  219   20   20          1.99396           13.51184
  96   474  270   20   20          1.99421           13.53926
  97   474  321   20   20          1.99462           13.58382
  98   474  372   20   20          1.99509           13.63587
  99   474  423   20   20          1.99563           13.69576
 100   474  474   20   20          1.99627           13.76591
Reticle           1
Means from averaging values of areas
 SLOPE  =  1.9955198
 OFFSET =  13.683558

STATISTICS FOR UPPER-LEFT  CORNER

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY       DN         DN         DN
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
    0.0000     0.0000     5.1000        0.0000    10.0000    13.6833   -3.6833
   10.0000     9.1113     5.1000       46.4675   110.0000   106.4102    3.5898
   20.0000    19.1113     5.1000       97.4675   210.0000   208.1817    1.8183
   40.0000    39.1113     5.1000      199.4675   410.0000   411.7248   -1.7248
                                                                RMS=    2.8607
SLOPE=   1.995520 SD= 0.001407 OFFSET=  13.683339 SD= 0.155116
NUMBER OF GOOD AREAS=  10 OUT OF  15 AREAS SAMPLED
NUMBER REJECTED FOR      BOTH          5
GALGEN:C1=   0.50112 ENERGY UNIT/DN C2=  -6.85703 ENERGY UNIT
Reticle           2
Means from averaging values of areas
 SLOPE  =  1.9955057
 OFFSET =  13.681805

STATISTICS FOR UPPER-RIGHT CORNER

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY       DN         DN         DN
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
    0.0000     0.0000     5.1000        0.0000    10.0000    13.6818   -3.6818
   10.0000     9.1116     5.1000       46.4694   110.0000   106.4117    3.5883
   20.0000    19.1116     5.1000       97.4694   210.0000   208.1825    1.8175
   40.0000    39.1116     5.1000      199.4694   410.0000   411.7241   -1.7241
                                                                RMS=    2.8595
SLOPE=   1.995506 SD= 0.000671 OFFSET=  13.681755 SD= 0.073935
NUMBER OF GOOD AREAS=  15 OUT OF  15 AREAS SAMPLED
NUMBER REJECTED FOR      BOTH          0
GALGEN:C1=   0.50113 ENERGY UNIT/DN C2=  -6.85628 ENERGY UNIT
Reticle           3
Means from averaging values of areas
 SLOPE  =  1.9955198
 OFFSET =  13.683558

STATISTICS FOR LOWER-LEFT  CORNER

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY       DN         DN         DN
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
    0.0000     0.0000     5.1000        0.0000    10.0000    13.6833   -3.6833
   10.0000     9.1113     5.1000       46.4675   110.0000   106.4102    3.5898
   20.0000    19.1113     5.1000       97.4675   210.0000   208.1817    1.8183
   40.0000    39.1113     5.1000      199.4675   410.0000   411.7248   -1.7248
                                                                RMS=    2.8607
SLOPE=   1.995520 SD= 0.001407 OFFSET=  13.683339 SD= 0.155116
NUMBER OF GOOD AREAS=  10 OUT OF  15 AREAS SAMPLED
NUMBER REJECTED FOR      BOTH          5
GALGEN:C1=   0.50112 ENERGY UNIT/DN C2=  -6.85703 ENERGY UNIT
Reticle           4
Means from averaging values of areas
 SLOPE  =  1.9955057
 OFFSET =  13.681805

STATISTICS FOR LOWER-RIGHT CORNER

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY       DN         DN         DN
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
    0.0000     0.0000     5.1000        0.0000    10.0000    13.6818   -3.6818
   10.0000     9.1116     5.1000       46.4694   110.0000   106.4117    3.5883
   20.0000    19.1116     5.1000       97.4694   210.0000   208.1825    1.8175
   40.0000    39.1116     5.1000      199.4694   410.0000   411.7241   -1.7241
                                                                RMS=    2.8595
SLOPE=   1.995506 SD= 0.000671 OFFSET=  13.681755 SD= 0.073935
NUMBER OF GOOD AREAS=  15 OUT OF  15 AREAS SAMPLED
NUMBER REJECTED FOR      BOTH          0
GALGEN:C1=   0.50113 ENERGY UNIT/DN C2=  -6.85628 ENERGY UNIT
Reticle           5
Means from averaging values of areas
 SLOPE  =  1.9945500
 OFFSET =  13.576578

STATISTICS FOR CENTER

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY       DN         DN         DN
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
    0.0000     0.0000     5.1000        0.0000    10.0000    13.5765   -3.5765
   10.0000     9.1362     5.1000       46.5946   110.0000   106.5118    3.4882
   20.0000    19.1362     5.1000       97.5946   210.0000   208.2338    1.7662
   40.0000    39.1362     5.1000      199.5946   410.0000   411.6779   -1.6779
                                                                RMS=    2.7791
SLOPE=   1.994550 SD= 0.000781 OFFSET=  13.576511 SD= 0.086066
NUMBER OF GOOD AREAS=  40 OUT OF  40 AREAS SAMPLED
NUMBER REJECTED FOR      BOTH          0
GALGEN:C1=   0.50137 ENERGY UNIT/DN C2=  -6.80680 ENERGY UNIT
Framewide
Means from averaging values of areas
 SLOPE=  1.9950841
 OFFSET=  13.635427

STATISTICS FOR FULL FRAME

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY       DN         DN         DN
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
    0.0000     0.0000     5.1000        0.0000    10.0000    13.6353   -3.6353
   10.0000     9.1225     5.1000       46.5246   110.0000   106.4558    3.5442
   20.0000    19.1225     5.1000       97.5246   210.0000   208.2051    1.7949
   40.0000    39.1225     5.1000      199.5246   410.0000   411.7037   -1.7037
                                                                RMS=    2.8240
SLOPE=   1.995084 SD= 0.001044 OFFSET=  13.635307 SD= 0.115031
NUMBER OF GOOD AREAS=  40 OUT OF  40 AREAS SAMPLED
NUMBER REJECTED FOR      BOTH          0
GALGEN:C1=   0.50123 ENERGY UNIT/DN C2=  -6.83445 ENERGY UNIT
CCDSLOPE task completed
if (mode = "nobatch" or mode = "inter")
end-if
ccdslope test.ltf  offsets=cas/sol.dat rej=2   +
   'deltax sigtol=1.0 plot=test4
Beginning VICAR task ccdslope
CCDSLOPE version 07-Jul-2013  (64-bit) - rjb
 SIGMA REJ TOL=          1
Mean shutter-offset = 0.93262106
Shutter-offset is LINE-dependent
DC included as data point on curve
----TASK:COPY    ----USER:wlb         Mon Jun  9 15:14:37 2014
----TASK:F2      ----USER:wlb         Mon Jun  9 15:14:37 2014
----TASK:LTGEN   ----USER:wlb         Mon Jun  9 15:14:38 2014

GLOBAL VALUE FOR SLOPE...
Raw mean and sigma are...
N= 100 MEAN=     1.99551 SIGMA=     0.00163
After throwing out samples differing by 1 sigma
N=  70 MEAN=     1.99496 SIGMA=     0.00074

GLOBAL VALUE FOR OFFSET...
Raw mean and sigma are...
N= 100 MEAN=    13.68307 SIGMA=     0.17982
After throwing out samples differing by 1 sigma
N=  70 MEAN=    13.62179 SIGMA=     0.08175

Summary of bad areas....
AREA   1 (SL,SS,NL,NS)=(  15,  15,  20,  20)  ****BOTH BAD FIT*********
AREA   2 (SL,SS,NL,NS)=(  15,  66,  20,  20)  ****BOTH BAD FIT*********
AREA   3 (SL,SS,NL,NS)=(  15, 117,  20,  20)  ****BOTH BAD FIT*********
AREA   4 (SL,SS,NL,NS)=(  15, 168,  20,  20)  ****BOTH BAD FIT*********
AREA   5 (SL,SS,NL,NS)=(  15, 219,  20,  20)  ****BOTH BAD FIT*********
AREA   6 (SL,SS,NL,NS)=(  15, 270,  20,  20)  ****BOTH BAD FIT*********
AREA   7 (SL,SS,NL,NS)=(  15, 321,  20,  20)  ****BOTH BAD FIT*********
AREA   8 (SL,SS,NL,NS)=(  15, 372,  20,  20)  ****BOTH BAD FIT*********
AREA   9 (SL,SS,NL,NS)=(  15, 423,  20,  20)  ****BOTH BAD FIT*********
AREA  10 (SL,SS,NL,NS)=(  15, 474,  20,  20)  ****BOTH BAD FIT*********
AREA  11 (SL,SS,NL,NS)=(  66,  15,  20,  20)  ****BOTH BAD FIT*********
AREA  12 (SL,SS,NL,NS)=(  66,  66,  20,  20)  ****BOTH BAD FIT*********
AREA  13 (SL,SS,NL,NS)=(  66, 117,  20,  20)  ****BOTH BAD FIT*********
AREA  14 (SL,SS,NL,NS)=(  66, 168,  20,  20)  ****BOTH BAD FIT*********
AREA  15 (SL,SS,NL,NS)=(  66, 219,  20,  20)  ****BOTH BAD FIT*********
AREA  16 (SL,SS,NL,NS)=(  66, 270,  20,  20)  ****BOTH BAD FIT*********
AREA  17 (SL,SS,NL,NS)=(  66, 321,  20,  20)  ****BOTH BAD FIT*********
AREA  18 (SL,SS,NL,NS)=(  66, 372,  20,  20)  ****BOTH BAD FIT*********
AREA  19 (SL,SS,NL,NS)=(  66, 423,  20,  20)  ****BOTH BAD FIT*********
AREA  20 (SL,SS,NL,NS)=(  66, 474,  20,  20)  ****BOTH BAD FIT*********
AREA  31 (SL,SS,NL,NS)=( 168,  15,  20,  20)  ****BOTH BAD FIT*********
AREA  32 (SL,SS,NL,NS)=( 168,  66,  20,  20)  ****BOTH BAD FIT*********
AREA  33 (SL,SS,NL,NS)=( 168, 117,  20,  20)  ****BOTH BAD FIT*********
AREA  34 (SL,SS,NL,NS)=( 168, 168,  20,  20)  ****BOTH BAD FIT*********
AREA  35 (SL,SS,NL,NS)=( 168, 219,  20,  20)  ****BOTH BAD FIT*********
AREA  36 (SL,SS,NL,NS)=( 168, 270,  20,  20)  ****BOTH BAD FIT*********
AREA  37 (SL,SS,NL,NS)=( 168, 321,  20,  20)  ****BOTH BAD FIT*********
AREA  38 (SL,SS,NL,NS)=( 168, 372,  20,  20)  ****BOTH BAD FIT*********
AREA  39 (SL,SS,NL,NS)=( 168, 423,  20,  20)  ****BOTH BAD FIT*********
AREA  40 (SL,SS,NL,NS)=( 168, 474,  20,  20)  ****BOTH BAD FIT*********


Slopes and offsets for each area...

ENERGY UNIT = PICOAMP-MILLISECONDS

AREA    SL   SS   NL   NS  SLOPE (DN/ENERGY UNIT)  OFFSET (DN)
   1    15   15   20   20          1.99939           14.11181
   2    15   66   20   20          1.99939           14.11181
   3    15  117   20   20          1.99939           14.11181
   4    15  168   20   20          1.99939           14.11181
   5    15  219   20   20          1.99939           14.11181
   6    15  270   20   20          1.99939           14.11181
   7    15  321   20   20          1.99939           14.11181
   8    15  372   20   20          1.99939           14.11181
   9    15  423   20   20          1.99939           14.11181
  10    15  474   20   20          1.99939           14.11181
  11    66   15   20   20          1.99717           13.86542
  12    66   66   20   20          1.99717           13.86542
  13    66  117   20   20          1.99717           13.86542
  14    66  168   20   20          1.99717           13.86542
  15    66  219   20   20          1.99717           13.86542
  16    66  270   20   20          1.99717           13.86542
  17    66  321   20   20          1.99717           13.86542
  18    66  372   20   20          1.99717           13.86542
  19    66  423   20   20          1.99717           13.86542
  20    66  474   20   20          1.99717           13.86542
  21   117   15   20   20          1.99495           13.62012
  22   117   66   20   20          1.99495           13.62012
  23   117  117   20   20          1.99495           13.62012
  24   117  168   20   20          1.99495           13.62012
  25   117  219   20   20          1.99495           13.62012
  26   117  270   20   20          1.99495           13.62012
  27   117  321   20   20          1.99495           13.62012
  28   117  372   20   20          1.99495           13.62012
  29   117  423   20   20          1.99495           13.62012
  30   117  474   20   20          1.99495           13.62012
  31   168   15   20   20          1.99386           13.50086
  32   168   66   20   20          1.99386           13.50086
  33   168  117   20   20          1.99386           13.50086
  34   168  168   20   20          1.99386           13.50086
  35   168  219   20   20          1.99386           13.50086
  36   168  270   20   20          1.99386           13.50086
  37   168  321   20   20          1.99386           13.50086
  38   168  372   20   20          1.99386           13.50086
  39   168  423   20   20          1.99386           13.50086
  40   168  474   20   20          1.99386           13.50086
  41   219   15   20   20          1.99396           13.51184
  42   219   66   20   20          1.99396           13.51184
  43   219  117   20   20          1.99396           13.51184
  44   219  168   20   20          1.99396           13.51184
  45   219  219   20   20          1.99396           13.51184
  46   219  270   20   20          1.99396           13.51184
  47   219  321   20   20          1.99396           13.51184
  48   219  372   20   20          1.99396           13.51184
  49   219  423   20   20          1.99396           13.51184
  50   219  474   20   20          1.99396           13.51184
  51   270   15   20   20          1.99421           13.53926
  52   270   66   20   20          1.99421           13.53926
  53   270  117   20   20          1.99421           13.53926
  54   270  168   20   20          1.99421           13.53926
  55   270  219   20   20          1.99421           13.53926
  56   270  270   20   20          1.99421           13.53926
  57   270  321   20   20          1.99421           13.53926
  58   270  372   20   20          1.99421           13.53926
  59   270  423   20   20          1.99421           13.53926
  60   270  474   20   20          1.99421           13.53926
  61   321   15   20   20          1.99462           13.58382
  62   321   66   20   20          1.99462           13.58382
  63   321  117   20   20          1.99462           13.58382
  64   321  168   20   20          1.99462           13.58382
  65   321  219   20   20          1.99462           13.58382
  66   321  270   20   20          1.99462           13.58382
  67   321  321   20   20          1.99462           13.58382
  68   321  372   20   20          1.99462           13.58382
  69   321  423   20   20          1.99462           13.58382
  70   321  474   20   20          1.99462           13.58382
  71   372   15   20   20          1.99509           13.63587
  72   372   66   20   20          1.99509           13.63587
  73   372  117   20   20          1.99509           13.63587
  74   372  168   20   20          1.99509           13.63587
  75   372  219   20   20          1.99509           13.63587
  76   372  270   20   20          1.99509           13.63587
  77   372  321   20   20          1.99509           13.63587
  78   372  372   20   20          1.99509           13.63587
  79   372  423   20   20          1.99509           13.63587
  80   372  474   20   20          1.99509           13.63587
  81   423   15   20   20          1.99563           13.69576
  82   423   66   20   20          1.99563           13.69576
  83   423  117   20   20          1.99563           13.69576
  84   423  168   20   20          1.99563           13.69576
  85   423  219   20   20          1.99563           13.69576
  86   423  270   20   20          1.99563           13.69576
  87   423  321   20   20          1.99563           13.69576
  88   423  372   20   20          1.99563           13.69576
  89   423  423   20   20          1.99563           13.69576
  90   423  474   20   20          1.99563           13.69576
  91   474   15   20   20          1.99627           13.76591
  92   474   66   20   20          1.99627           13.76591
  93   474  117   20   20          1.99627           13.76591
  94   474  168   20   20          1.99627           13.76591
  95   474  219   20   20          1.99627           13.76591
  96   474  270   20   20          1.99627           13.76591
  97   474  321   20   20          1.99627           13.76591
  98   474  372   20   20          1.99627           13.76591
  99   474  423   20   20          1.99627           13.76591
 100   474  474   20   20          1.99627           13.76591
Reticle           1
Means from averaging values of areas
 SLOPE  =  1.9947002
 OFFSET =  13.593047

STATISTICS FOR UPPER-LEFT  CORNER

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY       DN         DN         DN
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
    0.0000     0.0000     5.1000        0.0000    10.0000    13.5930   -3.5930
   10.0000     9.1323     5.1000       46.5749   110.0000   106.4960    3.5040
   20.0000    19.1323     5.1000       97.5749   210.0000   208.2258    1.7742
   40.0000    39.1323     5.1000      199.5749   410.0000   411.6852   -1.6852
                                                                RMS=    2.7917
SLOPE=   1.994700 SD= 0.000426 OFFSET=  13.593027 SD= 0.046888
NUMBER OF GOOD AREAS=   4 OUT OF  15 AREAS SAMPLED
NUMBER REJECTED FOR      OFFSET       11
GALGEN:C1=   0.50133 ENERGY UNIT/DN C2=  -6.81457 ENERGY UNIT
Reticle           2
Means from averaging values of areas
 SLOPE  =  1.9947002
 OFFSET =  13.593047

STATISTICS FOR UPPER-RIGHT CORNER

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY       DN         DN         DN
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
    0.0000     0.0000     5.1000        0.0000    10.0000    13.5930   -3.5930
   10.0000     9.1323     5.1000       46.5749   110.0000   106.4960    3.5040
   20.0000    19.1323     5.1000       97.5749   210.0000   208.2258    1.7742
   40.0000    39.1323     5.1000      199.5749   410.0000   411.6852   -1.6852
                                                                RMS=    2.7917
SLOPE=   1.994700 SD= 0.000426 OFFSET=  13.593027 SD= 0.046888
NUMBER OF GOOD AREAS=   4 OUT OF  15 AREAS SAMPLED
NUMBER REJECTED FOR      OFFSET       11
GALGEN:C1=   0.50133 ENERGY UNIT/DN C2=  -6.81457 ENERGY UNIT
Reticle           3
Means from averaging values of areas
 SLOPE  =  1.9955057
 OFFSET =  13.681805

STATISTICS FOR LOWER-LEFT  CORNER

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY       DN         DN         DN
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
    0.0000     0.0000     5.1000        0.0000    10.0000    13.6818   -3.6818
   10.0000     9.1116     5.1000       46.4694   110.0000   106.4117    3.5883
   20.0000    19.1116     5.1000       97.4694   210.0000   208.1825    1.8175
   40.0000    39.1116     5.1000      199.4694   410.0000   411.7241   -1.7241
                                                                RMS=    2.8595
SLOPE=   1.995506 SD= 0.000671 OFFSET=  13.681755 SD= 0.073935
NUMBER OF GOOD AREAS=  15 OUT OF  15 AREAS SAMPLED
NUMBER REJECTED FOR      OFFSET        0
GALGEN:C1=   0.50113 ENERGY UNIT/DN C2=  -6.85628 ENERGY UNIT
Reticle           4
Means from averaging values of areas
 SLOPE  =  1.9955057
 OFFSET =  13.681805

STATISTICS FOR LOWER-RIGHT CORNER

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY       DN         DN         DN
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
    0.0000     0.0000     5.1000        0.0000    10.0000    13.6818   -3.6818
   10.0000     9.1116     5.1000       46.4694   110.0000   106.4117    3.5883
   20.0000    19.1116     5.1000       97.4694   210.0000   208.1825    1.8175
   40.0000    39.1116     5.1000      199.4694   410.0000   411.7241   -1.7241
                                                                RMS=    2.8595
SLOPE=   1.995506 SD= 0.000671 OFFSET=  13.681755 SD= 0.073935
NUMBER OF GOOD AREAS=  15 OUT OF  15 AREAS SAMPLED
NUMBER REJECTED FOR      OFFSET        0
GALGEN:C1=   0.50113 ENERGY UNIT/DN C2=  -6.85628 ENERGY UNIT
Reticle           5
Means from averaging values of areas
 SLOPE  =  1.9945154
 OFFSET =  13.572723

STATISTICS FOR CENTER

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY       DN         DN         DN
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
    0.0000     0.0000     5.1000        0.0000    10.0000    13.5727   -3.5727
   10.0000     9.1371     5.1000       46.5991   110.0000   106.5154    3.4846
   20.0000    19.1371     5.1000       97.5991   210.0000   208.2357    1.7643
   40.0000    39.1371     5.1000      199.5991   410.0000   411.6762   -1.6762
                                                                RMS=    2.7762
SLOPE=   1.994515 SD= 0.000494 OFFSET=  13.572696 SD= 0.054362
NUMBER OF GOOD AREAS=  32 OUT OF  40 AREAS SAMPLED
NUMBER REJECTED FOR      OFFSET        8
GALGEN:C1=   0.50137 ENERGY UNIT/DN C2=  -6.80501 ENERGY UNIT
Framewide
Means from averaging values of areas
 SLOPE=  1.9949609
 OFFSET=  13.621795

STATISTICS FOR FULL FRAME

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY       DN         DN         DN
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
    0.0000     0.0000     5.1000        0.0000    10.0000    13.6217   -3.6217
   10.0000     9.1256     5.1000       46.5408   110.0000   106.4687    3.5313
   20.0000    19.1256     5.1000       97.5408   210.0000   208.2118    1.7882
   40.0000    39.1256     5.1000      199.5408   410.0000   411.6978   -1.6978
                                                                RMS=    2.8137
SLOPE=   1.994961 SD= 0.000742 OFFSET=  13.621734 SD= 0.081754
NUMBER OF GOOD AREAS=  32 OUT OF  40 AREAS SAMPLED
NUMBER REJECTED FOR      OFFSET        8
GALGEN:C1=   0.50126 ENERGY UNIT/DN C2=  -6.82807 ENERGY UNIT
DELTA CORRECTION=  -6.82807 ENERGY UNIT


-----------------------------------------
Processing repeated with DELTA correction
-----------------------------------------

GLOBAL VALUE FOR SLOPE...
Raw mean and sigma are...
N= 100 MEAN=     1.99551 SIGMA=     0.00163
After throwing out samples differing by 1 sigma
N=  70 MEAN=     1.99496 SIGMA=     0.00074

GLOBAL VALUE FOR OFFSET...
Raw mean and sigma are...
N= 100 MEAN=     0.05755 SIGMA=     0.16870
After throwing out samples differing by 1 sigma
N=  70 MEAN=     0.00006 SIGMA=     0.07669

Summary of bad areas....
AREA   1 (SL,SS,NL,NS)=(  15,  15,  20,  20)  ****BOTH BAD FIT*********
AREA   2 (SL,SS,NL,NS)=(  15,  66,  20,  20)  ****BOTH BAD FIT*********
AREA   3 (SL,SS,NL,NS)=(  15, 117,  20,  20)  ****BOTH BAD FIT*********
AREA   4 (SL,SS,NL,NS)=(  15, 168,  20,  20)  ****BOTH BAD FIT*********
AREA   5 (SL,SS,NL,NS)=(  15, 219,  20,  20)  ****BOTH BAD FIT*********
AREA   6 (SL,SS,NL,NS)=(  15, 270,  20,  20)  ****BOTH BAD FIT*********
AREA   7 (SL,SS,NL,NS)=(  15, 321,  20,  20)  ****BOTH BAD FIT*********
AREA   8 (SL,SS,NL,NS)=(  15, 372,  20,  20)  ****BOTH BAD FIT*********
AREA   9 (SL,SS,NL,NS)=(  15, 423,  20,  20)  ****BOTH BAD FIT*********
AREA  10 (SL,SS,NL,NS)=(  15, 474,  20,  20)  ****BOTH BAD FIT*********
AREA  11 (SL,SS,NL,NS)=(  66,  15,  20,  20)  ****BOTH BAD FIT*********
AREA  12 (SL,SS,NL,NS)=(  66,  66,  20,  20)  ****BOTH BAD FIT*********
AREA  13 (SL,SS,NL,NS)=(  66, 117,  20,  20)  ****BOTH BAD FIT*********
AREA  14 (SL,SS,NL,NS)=(  66, 168,  20,  20)  ****BOTH BAD FIT*********
AREA  15 (SL,SS,NL,NS)=(  66, 219,  20,  20)  ****BOTH BAD FIT*********
AREA  16 (SL,SS,NL,NS)=(  66, 270,  20,  20)  ****BOTH BAD FIT*********
AREA  17 (SL,SS,NL,NS)=(  66, 321,  20,  20)  ****BOTH BAD FIT*********
AREA  18 (SL,SS,NL,NS)=(  66, 372,  20,  20)  ****BOTH BAD FIT*********
AREA  19 (SL,SS,NL,NS)=(  66, 423,  20,  20)  ****BOTH BAD FIT*********
AREA  20 (SL,SS,NL,NS)=(  66, 474,  20,  20)  ****BOTH BAD FIT*********
AREA  31 (SL,SS,NL,NS)=( 168,  15,  20,  20)  ****BOTH BAD FIT*********
AREA  32 (SL,SS,NL,NS)=( 168,  66,  20,  20)  ****BOTH BAD FIT*********
AREA  33 (SL,SS,NL,NS)=( 168, 117,  20,  20)  ****BOTH BAD FIT*********
AREA  34 (SL,SS,NL,NS)=( 168, 168,  20,  20)  ****BOTH BAD FIT*********
AREA  35 (SL,SS,NL,NS)=( 168, 219,  20,  20)  ****BOTH BAD FIT*********
AREA  36 (SL,SS,NL,NS)=( 168, 270,  20,  20)  ****BOTH BAD FIT*********
AREA  37 (SL,SS,NL,NS)=( 168, 321,  20,  20)  ****BOTH BAD FIT*********
AREA  38 (SL,SS,NL,NS)=( 168, 372,  20,  20)  ****BOTH BAD FIT*********
AREA  39 (SL,SS,NL,NS)=( 168, 423,  20,  20)  ****BOTH BAD FIT*********
AREA  40 (SL,SS,NL,NS)=( 168, 474,  20,  20)  ****BOTH BAD FIT*********


Slopes and offsets for each area...

ENERGY UNIT = PICOAMP-MILLISECONDS

AREA    SL   SS   NL   NS  SLOPE (DN/ENERGY UNIT)  OFFSET (DN)
   1    15   15   20   20          1.99939            0.45984
   2    15   66   20   20          1.99939            0.45984
   3    15  117   20   20          1.99939            0.45984
   4    15  168   20   20          1.99939            0.45984
   5    15  219   20   20          1.99939            0.45984
   6    15  270   20   20          1.99939            0.45984
   7    15  321   20   20          1.99939            0.45984
   8    15  372   20   20          1.99939            0.45984
   9    15  423   20   20          1.99939            0.45984
  10    15  474   20   20          1.99939            0.45984
  11    66   15   20   20          1.99717            0.22861
  12    66   66   20   20          1.99717            0.22861
  13    66  117   20   20          1.99717            0.22861
  14    66  168   20   20          1.99717            0.22861
  15    66  219   20   20          1.99717            0.22861
  16    66  270   20   20          1.99717            0.22861
  17    66  321   20   20          1.99717            0.22861
  18    66  372   20   20          1.99717            0.22861
  19    66  423   20   20          1.99717            0.22861
  20    66  474   20   20          1.99717            0.22861
  21   117   15   20   20          1.99495           -0.00152
  22   117   66   20   20          1.99495           -0.00152
  23   117  117   20   20          1.99495           -0.00152
  24   117  168   20   20          1.99495           -0.00152
  25   117  219   20   20          1.99495           -0.00152
  26   117  270   20   20          1.99495           -0.00152
  27   117  321   20   20          1.99495           -0.00152
  28   117  372   20   20          1.99495           -0.00152
  29   117  423   20   20          1.99495           -0.00152
  30   117  474   20   20          1.99495           -0.00152
  31   168   15   20   20          1.99386           -0.11337
  32   168   66   20   20          1.99386           -0.11337
  33   168  117   20   20          1.99386           -0.11337
  34   168  168   20   20          1.99386           -0.11337
  35   168  219   20   20          1.99386           -0.11337
  36   168  270   20   20          1.99386           -0.11337
  37   168  321   20   20          1.99386           -0.11337
  38   168  372   20   20          1.99386           -0.11337
  39   168  423   20   20          1.99386           -0.11337
  40   168  474   20   20          1.99386           -0.11337
  41   219   15   20   20          1.99396           -0.10308
  42   219   66   20   20          1.99396           -0.10308
  43   219  117   20   20          1.99396           -0.10308
  44   219  168   20   20          1.99396           -0.10308
  45   219  219   20   20          1.99396           -0.10308
  46   219  270   20   20          1.99396           -0.10308
  47   219  321   20   20          1.99396           -0.10308
  48   219  372   20   20          1.99396           -0.10308
  49   219  423   20   20          1.99396           -0.10308
  50   219  474   20   20          1.99396           -0.10308
  51   270   15   20   20          1.99421           -0.07736
  52   270   66   20   20          1.99421           -0.07736
  53   270  117   20   20          1.99421           -0.07736
  54   270  168   20   20          1.99421           -0.07736
  55   270  219   20   20          1.99421           -0.07736
  56   270  270   20   20          1.99421           -0.07736
  57   270  321   20   20          1.99421           -0.07736
  58   270  372   20   20          1.99421           -0.07736
  59   270  423   20   20          1.99421           -0.07736
  60   270  474   20   20          1.99421           -0.07736
  61   321   15   20   20          1.99462           -0.03557
  62   321   66   20   20          1.99462           -0.03557
  63   321  117   20   20          1.99462           -0.03557
  64   321  168   20   20          1.99462           -0.03557
  65   321  219   20   20          1.99462           -0.03557
  66   321  270   20   20          1.99462           -0.03557
  67   321  321   20   20          1.99462           -0.03557
  68   321  372   20   20          1.99462           -0.03557
  69   321  423   20   20          1.99462           -0.03557
  70   321  474   20   20          1.99462           -0.03557
  71   372   15   20   20          1.99509            0.01326
  72   372   66   20   20          1.99509            0.01326
  73   372  117   20   20          1.99509            0.01326
  74   372  168   20   20          1.99509            0.01326
  75   372  219   20   20          1.99509            0.01326
  76   372  270   20   20          1.99509            0.01326
  77   372  321   20   20          1.99509            0.01326
  78   372  372   20   20          1.99509            0.01326
  79   372  423   20   20          1.99509            0.01326
  80   372  474   20   20          1.99509            0.01326
  81   423   15   20   20          1.99563            0.06944
  82   423   66   20   20          1.99563            0.06944
  83   423  117   20   20          1.99563            0.06944
  84   423  168   20   20          1.99563            0.06944
  85   423  219   20   20          1.99563            0.06944
  86   423  270   20   20          1.99563            0.06944
  87   423  321   20   20          1.99563            0.06944
  88   423  372   20   20          1.99563            0.06944
  89   423  423   20   20          1.99563            0.06944
  90   423  474   20   20          1.99563            0.06944
  91   474   15   20   20          1.99627            0.13525
  92   474   66   20   20          1.99627            0.13525
  93   474  117   20   20          1.99627            0.13525
  94   474  168   20   20          1.99627            0.13525
  95   474  219   20   20          1.99627            0.13525
  96   474  270   20   20          1.99627            0.13525
  97   474  321   20   20          1.99627            0.13525
  98   474  372   20   20          1.99627            0.13525
  99   474  423   20   20          1.99627            0.13525
 100   474  474   20   20          1.99627            0.13525
Reticle           1
Means from averaging values of areas
 SLOPE  =  1.9947002
 OFFSET = -0.02690629

STATISTICS FOR UPPER-LEFT  CORNER

COMMANDED   ACTUAL                    CORRECTED            BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY       DN         DN         DN
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
    0.0000     0.0000     5.1000        6.8281    10.0000    13.5930   -3.5930
   10.0000     9.1323     5.1000       53.4030   110.0000   106.4960    3.5040
   20.0000    19.1323     5.1000      104.4030   210.0000   208.2258    1.7742
   40.0000    39.1323     5.1000      206.4030   410.0000   411.6852   -1.6852
                                                                RMS=    2.7917
SLOPE=   1.994700 SD= 0.000426 OFFSET=  -0.026927 SD= 0.043977
NUMBER OF GOOD AREAS=   4 OUT OF  15 AREAS SAMPLED
NUMBER REJECTED FOR      OFFSET       11
GALGEN:C1=   0.50133 ENERGY UNIT/DN C2=   0.01350 ENERGY UNIT
Reticle           2
Means from averaging values of areas
 SLOPE  =  1.9947002
 OFFSET = -0.02690629

STATISTICS FOR UPPER-RIGHT CORNER

COMMANDED   ACTUAL                    CORRECTED            BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY       DN         DN         DN
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
    0.0000     0.0000     5.1000        6.8281    10.0000    13.5930   -3.5930
   10.0000     9.1323     5.1000       53.4030   110.0000   106.4960    3.5040
   20.0000    19.1323     5.1000      104.4030   210.0000   208.2258    1.7742
   40.0000    39.1323     5.1000      206.4030   410.0000   411.6852   -1.6852
                                                                RMS=    2.7917
SLOPE=   1.994700 SD= 0.000426 OFFSET=  -0.026927 SD= 0.043977
NUMBER OF GOOD AREAS=   4 OUT OF  15 AREAS SAMPLED
NUMBER REJECTED FOR      OFFSET       11
GALGEN:C1=   0.50133 ENERGY UNIT/DN C2=   0.01350 ENERGY UNIT
Reticle           3
Means from averaging values of areas
 SLOPE  =  1.9955057
 OFFSET = 0.05635187

STATISTICS FOR LOWER-LEFT  CORNER

COMMANDED   ACTUAL                    CORRECTED            BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY       DN         DN         DN
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
    0.0000     0.0000     5.1000        6.8281    10.0000    13.6818   -3.6818
   10.0000     9.1116     5.1000       53.2975   110.0000   106.4117    3.5883
   20.0000    19.1116     5.1000      104.2975   210.0000   208.1825    1.8175
   40.0000    39.1116     5.1000      206.2975   410.0000   411.7241   -1.7241
                                                                RMS=    2.8595
SLOPE=   1.995506 SD= 0.000671 OFFSET=   0.056302 SD= 0.069355
NUMBER OF GOOD AREAS=  15 OUT OF  15 AREAS SAMPLED
NUMBER REJECTED FOR      OFFSET        0
GALGEN:C1=   0.50113 ENERGY UNIT/DN C2=  -0.02821 ENERGY UNIT
Reticle           4
Means from averaging values of areas
 SLOPE  =  1.9955057
 OFFSET = 0.05635187

STATISTICS FOR LOWER-RIGHT CORNER

COMMANDED   ACTUAL                    CORRECTED            BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY       DN         DN         DN
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
    0.0000     0.0000     5.1000        6.8281    10.0000    13.6818   -3.6818
   10.0000     9.1116     5.1000       53.2975   110.0000   106.4117    3.5883
   20.0000    19.1116     5.1000      104.2975   210.0000   208.1825    1.8175
   40.0000    39.1116     5.1000      206.2975   410.0000   411.7241   -1.7241
                                                                RMS=    2.8595
SLOPE=   1.995506 SD= 0.000671 OFFSET=   0.056302 SD= 0.069355
NUMBER OF GOOD AREAS=  15 OUT OF  15 AREAS SAMPLED
NUMBER REJECTED FOR      OFFSET        0
GALGEN:C1=   0.50113 ENERGY UNIT/DN C2=  -0.02821 ENERGY UNIT
Reticle           5
Means from averaging values of areas
 SLOPE  =  1.9945154
 OFFSET = -0.04596875

STATISTICS FOR CENTER

COMMANDED   ACTUAL                    CORRECTED            BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY       DN         DN         DN
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
    0.0000     0.0000     5.1000        6.8281    10.0000    13.5727   -3.5727
   10.0000     9.1371     5.1000       53.4272   110.0000   106.5154    3.4846
   20.0000    19.1371     5.1000      104.4272   210.0000   208.2357    1.7643
   40.0000    39.1371     5.1000      206.4272   410.0000   411.6762   -1.6762
                                                                RMS=    2.7762
SLOPE=   1.994515 SD= 0.000494 OFFSET=  -0.045996 SD= 0.050989
NUMBER OF GOOD AREAS=  32 OUT OF  40 AREAS SAMPLED
NUMBER REJECTED FOR      OFFSET        8
GALGEN:C1=   0.50137 ENERGY UNIT/DN C2=   0.02306 ENERGY UNIT
Framewide
Means from averaging values of areas
 SLOPE=  1.9949609
 OFFSET=  6.151E-05

STATISTICS FOR FULL FRAME

COMMANDED   ACTUAL                    CORRECTED            BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY       DN         DN         DN
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
    0.0000     0.0000     5.1000        6.8281    10.0000    13.6217   -3.6217
   10.0000     9.1256     5.1000       53.3688   110.0000   106.4687    3.5313
   20.0000    19.1256     5.1000      104.3688   210.0000   208.2118    1.7882
   40.0000    39.1256     5.1000      206.3688   410.0000   411.6978   -1.6978
                                                                RMS=    2.8137
SLOPE=   1.994961 SD= 0.000742 OFFSET=   0.000000 SD= 0.076686
NUMBER OF GOOD AREAS=  32 OUT OF  40 AREAS SAMPLED
NUMBER REJECTED FOR      OFFSET        8
GALGEN:C1=   0.50126 ENERGY UNIT/DN C2=   0.00000 ENERGY UNIT
CCDSLOPE task completed
if (mode = "nobatch" or mode = "inter")
end-if
ccdslope test.ltf  offsets=cas/sol.dat rej=1   +
   'deltax sigtol=1.0 plot=test5 plotfmt=eps
Beginning VICAR task ccdslope
CCDSLOPE version 07-Jul-2013  (64-bit) - rjb
 SIGMA REJ TOL=          1
Mean shutter-offset = 0.93262106
Shutter-offset is LINE-dependent
DC included as data point on curve
----TASK:COPY    ----USER:wlb         Mon Jun  9 15:14:37 2014
----TASK:F2      ----USER:wlb         Mon Jun  9 15:14:37 2014
----TASK:LTGEN   ----USER:wlb         Mon Jun  9 15:14:38 2014

GLOBAL VALUE FOR SLOPE...
Raw mean and sigma are...
N= 100 MEAN=     1.99551 SIGMA=     0.00163
After throwing out samples differing by 1 sigma
N=  70 MEAN=     1.99496 SIGMA=     0.00074

GLOBAL VALUE FOR OFFSET...
Raw mean and sigma are...
N= 100 MEAN=    13.68307 SIGMA=     0.17982
After throwing out samples differing by 1 sigma
N=  70 MEAN=    13.62179 SIGMA=     0.08175

Summary of bad areas....
AREA   1 (SL,SS,NL,NS)=(  15,  15,  20,  20)  ****BOTH BAD FIT*********
AREA   2 (SL,SS,NL,NS)=(  15,  66,  20,  20)  ****BOTH BAD FIT*********
AREA   3 (SL,SS,NL,NS)=(  15, 117,  20,  20)  ****BOTH BAD FIT*********
AREA   4 (SL,SS,NL,NS)=(  15, 168,  20,  20)  ****BOTH BAD FIT*********
AREA   5 (SL,SS,NL,NS)=(  15, 219,  20,  20)  ****BOTH BAD FIT*********
AREA   6 (SL,SS,NL,NS)=(  15, 270,  20,  20)  ****BOTH BAD FIT*********
AREA   7 (SL,SS,NL,NS)=(  15, 321,  20,  20)  ****BOTH BAD FIT*********
AREA   8 (SL,SS,NL,NS)=(  15, 372,  20,  20)  ****BOTH BAD FIT*********
AREA   9 (SL,SS,NL,NS)=(  15, 423,  20,  20)  ****BOTH BAD FIT*********
AREA  10 (SL,SS,NL,NS)=(  15, 474,  20,  20)  ****BOTH BAD FIT*********
AREA  11 (SL,SS,NL,NS)=(  66,  15,  20,  20)  ****BOTH BAD FIT*********
AREA  12 (SL,SS,NL,NS)=(  66,  66,  20,  20)  ****BOTH BAD FIT*********
AREA  13 (SL,SS,NL,NS)=(  66, 117,  20,  20)  ****BOTH BAD FIT*********
AREA  14 (SL,SS,NL,NS)=(  66, 168,  20,  20)  ****BOTH BAD FIT*********
AREA  15 (SL,SS,NL,NS)=(  66, 219,  20,  20)  ****BOTH BAD FIT*********
AREA  16 (SL,SS,NL,NS)=(  66, 270,  20,  20)  ****BOTH BAD FIT*********
AREA  17 (SL,SS,NL,NS)=(  66, 321,  20,  20)  ****BOTH BAD FIT*********
AREA  18 (SL,SS,NL,NS)=(  66, 372,  20,  20)  ****BOTH BAD FIT*********
AREA  19 (SL,SS,NL,NS)=(  66, 423,  20,  20)  ****BOTH BAD FIT*********
AREA  20 (SL,SS,NL,NS)=(  66, 474,  20,  20)  ****BOTH BAD FIT*********
AREA  31 (SL,SS,NL,NS)=( 168,  15,  20,  20)  ****BOTH BAD FIT*********
AREA  32 (SL,SS,NL,NS)=( 168,  66,  20,  20)  ****BOTH BAD FIT*********
AREA  33 (SL,SS,NL,NS)=( 168, 117,  20,  20)  ****BOTH BAD FIT*********
AREA  34 (SL,SS,NL,NS)=( 168, 168,  20,  20)  ****BOTH BAD FIT*********
AREA  35 (SL,SS,NL,NS)=( 168, 219,  20,  20)  ****BOTH BAD FIT*********
AREA  36 (SL,SS,NL,NS)=( 168, 270,  20,  20)  ****BOTH BAD FIT*********
AREA  37 (SL,SS,NL,NS)=( 168, 321,  20,  20)  ****BOTH BAD FIT*********
AREA  38 (SL,SS,NL,NS)=( 168, 372,  20,  20)  ****BOTH BAD FIT*********
AREA  39 (SL,SS,NL,NS)=( 168, 423,  20,  20)  ****BOTH BAD FIT*********
AREA  40 (SL,SS,NL,NS)=( 168, 474,  20,  20)  ****BOTH BAD FIT*********


Slopes and offsets for each area...

ENERGY UNIT = PICOAMP-MILLISECONDS

AREA    SL   SS   NL   NS  SLOPE (DN/ENERGY UNIT)  OFFSET (DN)
   1    15   15   20   20          1.99939           14.11181
   2    15   66   20   20          1.99939           14.11181
   3    15  117   20   20          1.99939           14.11181
   4    15  168   20   20          1.99939           14.11181
   5    15  219   20   20          1.99939           14.11181
   6    15  270   20   20          1.99939           14.11181
   7    15  321   20   20          1.99939           14.11181
   8    15  372   20   20          1.99939           14.11181
   9    15  423   20   20          1.99939           14.11181
  10    15  474   20   20          1.99939           14.11181
  11    66   15   20   20          1.99717           13.86542
  12    66   66   20   20          1.99717           13.86542
  13    66  117   20   20          1.99717           13.86542
  14    66  168   20   20          1.99717           13.86542
  15    66  219   20   20          1.99717           13.86542
  16    66  270   20   20          1.99717           13.86542
  17    66  321   20   20          1.99717           13.86542
  18    66  372   20   20          1.99717           13.86542
  19    66  423   20   20          1.99717           13.86542
  20    66  474   20   20          1.99717           13.86542
  21   117   15   20   20          1.99495           13.62012
  22   117   66   20   20          1.99495           13.62012
  23   117  117   20   20          1.99495           13.62012
  24   117  168   20   20          1.99495           13.62012
  25   117  219   20   20          1.99495           13.62012
  26   117  270   20   20          1.99495           13.62012
  27   117  321   20   20          1.99495           13.62012
  28   117  372   20   20          1.99495           13.62012
  29   117  423   20   20          1.99495           13.62012
  30   117  474   20   20          1.99495           13.62012
  31   168   15   20   20          1.99386           13.50086
  32   168   66   20   20          1.99386           13.50086
  33   168  117   20   20          1.99386           13.50086
  34   168  168   20   20          1.99386           13.50086
  35   168  219   20   20          1.99386           13.50086
  36   168  270   20   20          1.99386           13.50086
  37   168  321   20   20          1.99386           13.50086
  38   168  372   20   20          1.99386           13.50086
  39   168  423   20   20          1.99386           13.50086
  40   168  474   20   20          1.99386           13.50086
  41   219   15   20   20          1.99396           13.51184
  42   219   66   20   20          1.99396           13.51184
  43   219  117   20   20          1.99396           13.51184
  44   219  168   20   20          1.99396           13.51184
  45   219  219   20   20          1.99396           13.51184
  46   219  270   20   20          1.99396           13.51184
  47   219  321   20   20          1.99396           13.51184
  48   219  372   20   20          1.99396           13.51184
  49   219  423   20   20          1.99396           13.51184
  50   219  474   20   20          1.99396           13.51184
  51   270   15   20   20          1.99421           13.53926
  52   270   66   20   20          1.99421           13.53926
  53   270  117   20   20          1.99421           13.53926
  54   270  168   20   20          1.99421           13.53926
  55   270  219   20   20          1.99421           13.53926
  56   270  270   20   20          1.99421           13.53926
  57   270  321   20   20          1.99421           13.53926
  58   270  372   20   20          1.99421           13.53926
  59   270  423   20   20          1.99421           13.53926
  60   270  474   20   20          1.99421           13.53926
  61   321   15   20   20          1.99462           13.58382
  62   321   66   20   20          1.99462           13.58382
  63   321  117   20   20          1.99462           13.58382
  64   321  168   20   20          1.99462           13.58382
  65   321  219   20   20          1.99462           13.58382
  66   321  270   20   20          1.99462           13.58382
  67   321  321   20   20          1.99462           13.58382
  68   321  372   20   20          1.99462           13.58382
  69   321  423   20   20          1.99462           13.58382
  70   321  474   20   20          1.99462           13.58382
  71   372   15   20   20          1.99509           13.63587
  72   372   66   20   20          1.99509           13.63587
  73   372  117   20   20          1.99509           13.63587
  74   372  168   20   20          1.99509           13.63587
  75   372  219   20   20          1.99509           13.63587
  76   372  270   20   20          1.99509           13.63587
  77   372  321   20   20          1.99509           13.63587
  78   372  372   20   20          1.99509           13.63587
  79   372  423   20   20          1.99509           13.63587
  80   372  474   20   20          1.99509           13.63587
  81   423   15   20   20          1.99563           13.69576
  82   423   66   20   20          1.99563           13.69576
  83   423  117   20   20          1.99563           13.69576
  84   423  168   20   20          1.99563           13.69576
  85   423  219   20   20          1.99563           13.69576
  86   423  270   20   20          1.99563           13.69576
  87   423  321   20   20          1.99563           13.69576
  88   423  372   20   20          1.99563           13.69576
  89   423  423   20   20          1.99563           13.69576
  90   423  474   20   20          1.99563           13.69576
  91   474   15   20   20          1.99627           13.76591
  92   474   66   20   20          1.99627           13.76591
  93   474  117   20   20          1.99627           13.76591
  94   474  168   20   20          1.99627           13.76591
  95   474  219   20   20          1.99627           13.76591
  96   474  270   20   20          1.99627           13.76591
  97   474  321   20   20          1.99627           13.76591
  98   474  372   20   20          1.99627           13.76591
  99   474  423   20   20          1.99627           13.76591
 100   474  474   20   20          1.99627           13.76591
Reticle           1
Means from averaging values of areas
 SLOPE  =  1.9947002
 OFFSET =  13.593047

STATISTICS FOR UPPER-LEFT  CORNER

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY       DN         DN         DN
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
    0.0000     0.0000     5.1000        0.0000    10.0000    13.5930   -3.5930
   10.0000     9.1323     5.1000       46.5749   110.0000   106.4960    3.5040
   20.0000    19.1323     5.1000       97.5749   210.0000   208.2258    1.7742
   40.0000    39.1323     5.1000      199.5749   410.0000   411.6852   -1.6852
                                                                RMS=    2.7917
SLOPE=   1.994700 SD= 0.000426 OFFSET=  13.593027 SD= 0.046888
NUMBER OF GOOD AREAS=   4 OUT OF  15 AREAS SAMPLED
NUMBER REJECTED FOR      SLOPE        11
GALGEN:C1=   0.50133 ENERGY UNIT/DN C2=  -6.81457 ENERGY UNIT
Reticle           2
Means from averaging values of areas
 SLOPE  =  1.9947002
 OFFSET =  13.593047

STATISTICS FOR UPPER-RIGHT CORNER

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY       DN         DN         DN
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
    0.0000     0.0000     5.1000        0.0000    10.0000    13.5930   -3.5930
   10.0000     9.1323     5.1000       46.5749   110.0000   106.4960    3.5040
   20.0000    19.1323     5.1000       97.5749   210.0000   208.2258    1.7742
   40.0000    39.1323     5.1000      199.5749   410.0000   411.6852   -1.6852
                                                                RMS=    2.7917
SLOPE=   1.994700 SD= 0.000426 OFFSET=  13.593027 SD= 0.046888
NUMBER OF GOOD AREAS=   4 OUT OF  15 AREAS SAMPLED
NUMBER REJECTED FOR      SLOPE        11
GALGEN:C1=   0.50133 ENERGY UNIT/DN C2=  -6.81457 ENERGY UNIT
Reticle           3
Means from averaging values of areas
 SLOPE  =  1.9955057
 OFFSET =  13.681805

STATISTICS FOR LOWER-LEFT  CORNER

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY       DN         DN         DN
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
    0.0000     0.0000     5.1000        0.0000    10.0000    13.6818   -3.6818
   10.0000     9.1116     5.1000       46.4694   110.0000   106.4117    3.5883
   20.0000    19.1116     5.1000       97.4694   210.0000   208.1825    1.8175
   40.0000    39.1116     5.1000      199.4694   410.0000   411.7241   -1.7241
                                                                RMS=    2.8595
SLOPE=   1.995506 SD= 0.000671 OFFSET=  13.681755 SD= 0.073935
NUMBER OF GOOD AREAS=  15 OUT OF  15 AREAS SAMPLED
NUMBER REJECTED FOR      SLOPE         0
GALGEN:C1=   0.50113 ENERGY UNIT/DN C2=  -6.85628 ENERGY UNIT
Reticle           4
Means from averaging values of areas
 SLOPE  =  1.9955057
 OFFSET =  13.681805

STATISTICS FOR LOWER-RIGHT CORNER

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY       DN         DN         DN
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
    0.0000     0.0000     5.1000        0.0000    10.0000    13.6818   -3.6818
   10.0000     9.1116     5.1000       46.4694   110.0000   106.4117    3.5883
   20.0000    19.1116     5.1000       97.4694   210.0000   208.1825    1.8175
   40.0000    39.1116     5.1000      199.4694   410.0000   411.7241   -1.7241
                                                                RMS=    2.8595
SLOPE=   1.995506 SD= 0.000671 OFFSET=  13.681755 SD= 0.073935
NUMBER OF GOOD AREAS=  15 OUT OF  15 AREAS SAMPLED
NUMBER REJECTED FOR      SLOPE         0
GALGEN:C1=   0.50113 ENERGY UNIT/DN C2=  -6.85628 ENERGY UNIT
Reticle           5
Means from averaging values of areas
 SLOPE  =  1.9945154
 OFFSET =  13.572723

STATISTICS FOR CENTER

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY       DN         DN         DN
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
    0.0000     0.0000     5.1000        0.0000    10.0000    13.5727   -3.5727
   10.0000     9.1371     5.1000       46.5991   110.0000   106.5154    3.4846
   20.0000    19.1371     5.1000       97.5991   210.0000   208.2357    1.7643
   40.0000    39.1371     5.1000      199.5991   410.0000   411.6762   -1.6762
                                                                RMS=    2.7762
SLOPE=   1.994515 SD= 0.000494 OFFSET=  13.572696 SD= 0.054362
NUMBER OF GOOD AREAS=  32 OUT OF  40 AREAS SAMPLED
NUMBER REJECTED FOR      SLOPE         8
GALGEN:C1=   0.50137 ENERGY UNIT/DN C2=  -6.80501 ENERGY UNIT
Framewide
Means from averaging values of areas
 SLOPE=  1.9949609
 OFFSET=  13.621795

STATISTICS FOR FULL FRAME

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY       DN         DN         DN
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
    0.0000     0.0000     5.1000        0.0000    10.0000    13.6217   -3.6217
   10.0000     9.1256     5.1000       46.5408   110.0000   106.4687    3.5313
   20.0000    19.1256     5.1000       97.5408   210.0000   208.2118    1.7882
   40.0000    39.1256     5.1000      199.5408   410.0000   411.6978   -1.6978
                                                                RMS=    2.8137
SLOPE=   1.994961 SD= 0.000742 OFFSET=  13.621734 SD= 0.081754
NUMBER OF GOOD AREAS=  32 OUT OF  40 AREAS SAMPLED
NUMBER REJECTED FOR      SLOPE         8
GALGEN:C1=   0.50126 ENERGY UNIT/DN C2=  -6.82807 ENERGY UNIT
DELTA CORRECTION=  -6.82807 ENERGY UNIT


-----------------------------------------
Processing repeated with DELTA correction
-----------------------------------------

GLOBAL VALUE FOR SLOPE...
Raw mean and sigma are...
N= 100 MEAN=     1.99551 SIGMA=     0.00163
After throwing out samples differing by 1 sigma
N=  70 MEAN=     1.99496 SIGMA=     0.00074

GLOBAL VALUE FOR OFFSET...
Raw mean and sigma are...
N= 100 MEAN=     0.05755 SIGMA=     0.16870
After throwing out samples differing by 1 sigma
N=  70 MEAN=     0.00006 SIGMA=     0.07669

Summary of bad areas....
AREA   1 (SL,SS,NL,NS)=(  15,  15,  20,  20)  ****BOTH BAD FIT*********
AREA   2 (SL,SS,NL,NS)=(  15,  66,  20,  20)  ****BOTH BAD FIT*********
AREA   3 (SL,SS,NL,NS)=(  15, 117,  20,  20)  ****BOTH BAD FIT*********
AREA   4 (SL,SS,NL,NS)=(  15, 168,  20,  20)  ****BOTH BAD FIT*********
AREA   5 (SL,SS,NL,NS)=(  15, 219,  20,  20)  ****BOTH BAD FIT*********
AREA   6 (SL,SS,NL,NS)=(  15, 270,  20,  20)  ****BOTH BAD FIT*********
AREA   7 (SL,SS,NL,NS)=(  15, 321,  20,  20)  ****BOTH BAD FIT*********
AREA   8 (SL,SS,NL,NS)=(  15, 372,  20,  20)  ****BOTH BAD FIT*********
AREA   9 (SL,SS,NL,NS)=(  15, 423,  20,  20)  ****BOTH BAD FIT*********
AREA  10 (SL,SS,NL,NS)=(  15, 474,  20,  20)  ****BOTH BAD FIT*********
AREA  11 (SL,SS,NL,NS)=(  66,  15,  20,  20)  ****BOTH BAD FIT*********
AREA  12 (SL,SS,NL,NS)=(  66,  66,  20,  20)  ****BOTH BAD FIT*********
AREA  13 (SL,SS,NL,NS)=(  66, 117,  20,  20)  ****BOTH BAD FIT*********
AREA  14 (SL,SS,NL,NS)=(  66, 168,  20,  20)  ****BOTH BAD FIT*********
AREA  15 (SL,SS,NL,NS)=(  66, 219,  20,  20)  ****BOTH BAD FIT*********
AREA  16 (SL,SS,NL,NS)=(  66, 270,  20,  20)  ****BOTH BAD FIT*********
AREA  17 (SL,SS,NL,NS)=(  66, 321,  20,  20)  ****BOTH BAD FIT*********
AREA  18 (SL,SS,NL,NS)=(  66, 372,  20,  20)  ****BOTH BAD FIT*********
AREA  19 (SL,SS,NL,NS)=(  66, 423,  20,  20)  ****BOTH BAD FIT*********
AREA  20 (SL,SS,NL,NS)=(  66, 474,  20,  20)  ****BOTH BAD FIT*********
AREA  31 (SL,SS,NL,NS)=( 168,  15,  20,  20)  ****BOTH BAD FIT*********
AREA  32 (SL,SS,NL,NS)=( 168,  66,  20,  20)  ****BOTH BAD FIT*********
AREA  33 (SL,SS,NL,NS)=( 168, 117,  20,  20)  ****BOTH BAD FIT*********
AREA  34 (SL,SS,NL,NS)=( 168, 168,  20,  20)  ****BOTH BAD FIT*********
AREA  35 (SL,SS,NL,NS)=( 168, 219,  20,  20)  ****BOTH BAD FIT*********
AREA  36 (SL,SS,NL,NS)=( 168, 270,  20,  20)  ****BOTH BAD FIT*********
AREA  37 (SL,SS,NL,NS)=( 168, 321,  20,  20)  ****BOTH BAD FIT*********
AREA  38 (SL,SS,NL,NS)=( 168, 372,  20,  20)  ****BOTH BAD FIT*********
AREA  39 (SL,SS,NL,NS)=( 168, 423,  20,  20)  ****BOTH BAD FIT*********
AREA  40 (SL,SS,NL,NS)=( 168, 474,  20,  20)  ****BOTH BAD FIT*********


Slopes and offsets for each area...

ENERGY UNIT = PICOAMP-MILLISECONDS

AREA    SL   SS   NL   NS  SLOPE (DN/ENERGY UNIT)  OFFSET (DN)
   1    15   15   20   20          1.99939            0.45984
   2    15   66   20   20          1.99939            0.45984
   3    15  117   20   20          1.99939            0.45984
   4    15  168   20   20          1.99939            0.45984
   5    15  219   20   20          1.99939            0.45984
   6    15  270   20   20          1.99939            0.45984
   7    15  321   20   20          1.99939            0.45984
   8    15  372   20   20          1.99939            0.45984
   9    15  423   20   20          1.99939            0.45984
  10    15  474   20   20          1.99939            0.45984
  11    66   15   20   20          1.99717            0.22861
  12    66   66   20   20          1.99717            0.22861
  13    66  117   20   20          1.99717            0.22861
  14    66  168   20   20          1.99717            0.22861
  15    66  219   20   20          1.99717            0.22861
  16    66  270   20   20          1.99717            0.22861
  17    66  321   20   20          1.99717            0.22861
  18    66  372   20   20          1.99717            0.22861
  19    66  423   20   20          1.99717            0.22861
  20    66  474   20   20          1.99717            0.22861
  21   117   15   20   20          1.99495           -0.00152
  22   117   66   20   20          1.99495           -0.00152
  23   117  117   20   20          1.99495           -0.00152
  24   117  168   20   20          1.99495           -0.00152
  25   117  219   20   20          1.99495           -0.00152
  26   117  270   20   20          1.99495           -0.00152
  27   117  321   20   20          1.99495           -0.00152
  28   117  372   20   20          1.99495           -0.00152
  29   117  423   20   20          1.99495           -0.00152
  30   117  474   20   20          1.99495           -0.00152
  31   168   15   20   20          1.99386           -0.11337
  32   168   66   20   20          1.99386           -0.11337
  33   168  117   20   20          1.99386           -0.11337
  34   168  168   20   20          1.99386           -0.11337
  35   168  219   20   20          1.99386           -0.11337
  36   168  270   20   20          1.99386           -0.11337
  37   168  321   20   20          1.99386           -0.11337
  38   168  372   20   20          1.99386           -0.11337
  39   168  423   20   20          1.99386           -0.11337
  40   168  474   20   20          1.99386           -0.11337
  41   219   15   20   20          1.99396           -0.10308
  42   219   66   20   20          1.99396           -0.10308
  43   219  117   20   20          1.99396           -0.10308
  44   219  168   20   20          1.99396           -0.10308
  45   219  219   20   20          1.99396           -0.10308
  46   219  270   20   20          1.99396           -0.10308
  47   219  321   20   20          1.99396           -0.10308
  48   219  372   20   20          1.99396           -0.10308
  49   219  423   20   20          1.99396           -0.10308
  50   219  474   20   20          1.99396           -0.10308
  51   270   15   20   20          1.99421           -0.07736
  52   270   66   20   20          1.99421           -0.07736
  53   270  117   20   20          1.99421           -0.07736
  54   270  168   20   20          1.99421           -0.07736
  55   270  219   20   20          1.99421           -0.07736
  56   270  270   20   20          1.99421           -0.07736
  57   270  321   20   20          1.99421           -0.07736
  58   270  372   20   20          1.99421           -0.07736
  59   270  423   20   20          1.99421           -0.07736
  60   270  474   20   20          1.99421           -0.07736
  61   321   15   20   20          1.99462           -0.03557
  62   321   66   20   20          1.99462           -0.03557
  63   321  117   20   20          1.99462           -0.03557
  64   321  168   20   20          1.99462           -0.03557
  65   321  219   20   20          1.99462           -0.03557
  66   321  270   20   20          1.99462           -0.03557
  67   321  321   20   20          1.99462           -0.03557
  68   321  372   20   20          1.99462           -0.03557
  69   321  423   20   20          1.99462           -0.03557
  70   321  474   20   20          1.99462           -0.03557
  71   372   15   20   20          1.99509            0.01326
  72   372   66   20   20          1.99509            0.01326
  73   372  117   20   20          1.99509            0.01326
  74   372  168   20   20          1.99509            0.01326
  75   372  219   20   20          1.99509            0.01326
  76   372  270   20   20          1.99509            0.01326
  77   372  321   20   20          1.99509            0.01326
  78   372  372   20   20          1.99509            0.01326
  79   372  423   20   20          1.99509            0.01326
  80   372  474   20   20          1.99509            0.01326
  81   423   15   20   20          1.99563            0.06944
  82   423   66   20   20          1.99563            0.06944
  83   423  117   20   20          1.99563            0.06944
  84   423  168   20   20          1.99563            0.06944
  85   423  219   20   20          1.99563            0.06944
  86   423  270   20   20          1.99563            0.06944
  87   423  321   20   20          1.99563            0.06944
  88   423  372   20   20          1.99563            0.06944
  89   423  423   20   20          1.99563            0.06944
  90   423  474   20   20          1.99563            0.06944
  91   474   15   20   20          1.99627            0.13525
  92   474   66   20   20          1.99627            0.13525
  93   474  117   20   20          1.99627            0.13525
  94   474  168   20   20          1.99627            0.13525
  95   474  219   20   20          1.99627            0.13525
  96   474  270   20   20          1.99627            0.13525
  97   474  321   20   20          1.99627            0.13525
  98   474  372   20   20          1.99627            0.13525
  99   474  423   20   20          1.99627            0.13525
 100   474  474   20   20          1.99627            0.13525
Reticle           1
Means from averaging values of areas
 SLOPE  =  1.9947002
 OFFSET = -0.02690629

STATISTICS FOR UPPER-LEFT  CORNER

COMMANDED   ACTUAL                    CORRECTED            BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY       DN         DN         DN
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
    0.0000     0.0000     5.1000        6.8281    10.0000    13.5930   -3.5930
   10.0000     9.1323     5.1000       53.4030   110.0000   106.4960    3.5040
   20.0000    19.1323     5.1000      104.4030   210.0000   208.2258    1.7742
   40.0000    39.1323     5.1000      206.4030   410.0000   411.6852   -1.6852
                                                                RMS=    2.7917
SLOPE=   1.994700 SD= 0.000426 OFFSET=  -0.026927 SD= 0.043977
NUMBER OF GOOD AREAS=   4 OUT OF  15 AREAS SAMPLED
NUMBER REJECTED FOR      SLOPE        11
GALGEN:C1=   0.50133 ENERGY UNIT/DN C2=   0.01350 ENERGY UNIT
Reticle           2
Means from averaging values of areas
 SLOPE  =  1.9947002
 OFFSET = -0.02690629

STATISTICS FOR UPPER-RIGHT CORNER

COMMANDED   ACTUAL                    CORRECTED            BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY       DN         DN         DN
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
    0.0000     0.0000     5.1000        6.8281    10.0000    13.5930   -3.5930
   10.0000     9.1323     5.1000       53.4030   110.0000   106.4960    3.5040
   20.0000    19.1323     5.1000      104.4030   210.0000   208.2258    1.7742
   40.0000    39.1323     5.1000      206.4030   410.0000   411.6852   -1.6852
                                                                RMS=    2.7917
SLOPE=   1.994700 SD= 0.000426 OFFSET=  -0.026927 SD= 0.043977
NUMBER OF GOOD AREAS=   4 OUT OF  15 AREAS SAMPLED
NUMBER REJECTED FOR      SLOPE        11
GALGEN:C1=   0.50133 ENERGY UNIT/DN C2=   0.01350 ENERGY UNIT
Reticle           3
Means from averaging values of areas
 SLOPE  =  1.9955057
 OFFSET = 0.05635187

STATISTICS FOR LOWER-LEFT  CORNER

COMMANDED   ACTUAL                    CORRECTED            BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY       DN         DN         DN
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
    0.0000     0.0000     5.1000        6.8281    10.0000    13.6818   -3.6818
   10.0000     9.1116     5.1000       53.2975   110.0000   106.4117    3.5883
   20.0000    19.1116     5.1000      104.2975   210.0000   208.1825    1.8175
   40.0000    39.1116     5.1000      206.2975   410.0000   411.7241   -1.7241
                                                                RMS=    2.8595
SLOPE=   1.995506 SD= 0.000671 OFFSET=   0.056302 SD= 0.069355
NUMBER OF GOOD AREAS=  15 OUT OF  15 AREAS SAMPLED
NUMBER REJECTED FOR      SLOPE         0
GALGEN:C1=   0.50113 ENERGY UNIT/DN C2=  -0.02821 ENERGY UNIT
Reticle           4
Means from averaging values of areas
 SLOPE  =  1.9955057
 OFFSET = 0.05635187

STATISTICS FOR LOWER-RIGHT CORNER

COMMANDED   ACTUAL                    CORRECTED            BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY       DN         DN         DN
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
    0.0000     0.0000     5.1000        6.8281    10.0000    13.6818   -3.6818
   10.0000     9.1116     5.1000       53.2975   110.0000   106.4117    3.5883
   20.0000    19.1116     5.1000      104.2975   210.0000   208.1825    1.8175
   40.0000    39.1116     5.1000      206.2975   410.0000   411.7241   -1.7241
                                                                RMS=    2.8595
SLOPE=   1.995506 SD= 0.000671 OFFSET=   0.056302 SD= 0.069355
NUMBER OF GOOD AREAS=  15 OUT OF  15 AREAS SAMPLED
NUMBER REJECTED FOR      SLOPE         0
GALGEN:C1=   0.50113 ENERGY UNIT/DN C2=  -0.02821 ENERGY UNIT
Reticle           5
Means from averaging values of areas
 SLOPE  =  1.9945154
 OFFSET = -0.04596875

STATISTICS FOR CENTER

COMMANDED   ACTUAL                    CORRECTED            BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY       DN         DN         DN
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
    0.0000     0.0000     5.1000        6.8281    10.0000    13.5727   -3.5727
   10.0000     9.1371     5.1000       53.4272   110.0000   106.5154    3.4846
   20.0000    19.1371     5.1000      104.4272   210.0000   208.2357    1.7643
   40.0000    39.1371     5.1000      206.4272   410.0000   411.6762   -1.6762
                                                                RMS=    2.7762
SLOPE=   1.994515 SD= 0.000494 OFFSET=  -0.045996 SD= 0.050989
NUMBER OF GOOD AREAS=  32 OUT OF  40 AREAS SAMPLED
NUMBER REJECTED FOR      SLOPE         8
GALGEN:C1=   0.50137 ENERGY UNIT/DN C2=   0.02306 ENERGY UNIT
Framewide
Means from averaging values of areas
 SLOPE=  1.9949609
 OFFSET=  6.151E-05

STATISTICS FOR FULL FRAME

COMMANDED   ACTUAL                    CORRECTED            BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY       DN         DN         DN
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
    0.0000     0.0000     5.1000        6.8281    10.0000    13.6217   -3.6217
   10.0000     9.1256     5.1000       53.3688   110.0000   106.4687    3.5313
   20.0000    19.1256     5.1000      104.3688   210.0000   208.2118    1.7882
   40.0000    39.1256     5.1000      206.3688   410.0000   411.6978   -1.6978
                                                                RMS=    2.8137
SLOPE=   1.994961 SD= 0.000742 OFFSET=   0.000000 SD= 0.076686
NUMBER OF GOOD AREAS=  32 OUT OF  40 AREAS SAMPLED
NUMBER REJECTED FOR      SLOPE         8
GALGEN:C1=   0.50126 ENERGY UNIT/DN C2=   0.00000 ENERGY UNIT
CCDSLOPE task completed
ush gnuplot test5.eps.gpi
if (mode = "nobatch" or mode = "inter")
end-if
let $echo="no"
$!-----------------------------------------------------------------------------
$ create tstccdslope_sun.log
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
if (afidsroot = "")
    ush ln -s /project/test_work/testdata/cassini/iss cas
else
end-if
  defcmd-replace typeit "ush cat"
copy cas/sum2.1 l1.ax
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
f2 l1.ax l1.a func=10
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
f2 cas/sum2.1 l2.a func=110
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 26 TIMES
label-rep l2.a 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=10."
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword EXPOSURE_DURATION replaced
copy l2.a l2.b
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
copy l2.a l2.c
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
f2 cas/sum2.1 l3.a func=210
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 26 TIMES
label-rep l3.a 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=20."
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword EXPOSURE_DURATION replaced
copy l3.a l3.b
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
copy l3.a l3.c
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
f2 cas/sum2.1 l4.a func=410
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 26 TIMES
label-rep l4.a 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=40."
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword EXPOSURE_DURATION replaced
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
NEXT FILE =     1
l1.a
l1.b
l1.c
l2.a
l2.b
l2.c
l3.a
l3.b
l3.c
l4.a
l4.b
l4.c
ltgen l1.a out=test.ltf list=l.list 'GRID
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
   let LUMS = "5.099999904633e+00"
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
   let LUMS = "5.099999904633e+00"
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
   let LUMS = "5.099999904633e+00"
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
   let LUMS = "5.099999904633e+00"
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
   let LUMS = "5.099999904633e+00"
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
   let LUMS = "5.099999904633e+00"
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
   let LUMS = "5.099999904633e+00"
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
   let LUMS = "5.099999904633e+00"
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
   let LUMS = "5.099999904633e+00"
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
   write "       2              1.000000000000e+01             5.099999904633e+00"
       2              1.000000000000e+01             5.099999904633e+00
   if (I=NLVL) break
   let I=I+1
end-loop
   let X=EX(I)
   let EXS = "2.000000000000e+01"
   let LUMS=LMS(I)
   write "       3              2.000000000000e+01             5.099999904633e+00"
       3              2.000000000000e+01             5.099999904633e+00
   if (I=NLVL) break
   let I=I+1
end-loop
   let X=EX(I)
   let EXS = "4.000000000000e+01"
   let LUMS=LMS(I)
   write "       4              4.000000000000e+01             5.099999904633e+00"
       4              4.000000000000e+01             5.099999904633e+00
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
NEXT FILE=0001
l1.a
l1.b
l1.c
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
NEXT FILE=0001
l2.a
l2.b
l2.c
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
NEXT FILE=0001
l3.a
l3.b
l3.c
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
NEXT FILE=0001
l4.a
l4.b
l4.c
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
ccdslope test.ltf table=ccdslope1.tbl mofset=0.0 light=10. rej=0 'SUBDC  +
    plot=test1
Beginning VICAR task ccdslope
CCDSLOPE version 07-Jul-2013  (64-bit) - rjb
 SIGMA REJ TOL=          2
Mean shutter-offset =  0.000E+00
Shutter-offset is LINE-dependent
DC subtraction mode
----TASK:COPY    ----USER:wlb         Mon Jun  9 16:32:32 2014
----TASK:F2      ----USER:wlb         Mon Jun  9 16:32:32 2014
----TASK:LTGEN   ----USER:wlb         Mon Jun  9 16:32:35 2014

GLOBAL VALUE FOR SLOPE...
Raw mean and sigma are...
N= 100 MEAN=     1.00000 SIGMA=     0.00000

GLOBAL VALUE FOR OFFSET...
Raw mean and sigma are...
N= 100 MEAN=     0.00000 SIGMA=     0.00000

Summary of bad areas....

    No rejected areas

Slopes and offsets for each area...

ENERGY UNIT = PICOAMP-MILLISECONDS

AREA    SL   SS   NL   NS  SLOPE (DN/ENERGY UNIT)  OFFSET (DN)
   1    15   15   20   20          1.00000            0.00000
   2    15   66   20   20          1.00000            0.00000
   3    15  117   20   20          1.00000            0.00000
   4    15  168   20   20          1.00000            0.00000
   5    15  219   20   20          1.00000            0.00000
   6    15  270   20   20          1.00000            0.00000
   7    15  321   20   20          1.00000            0.00000
   8    15  372   20   20          1.00000            0.00000
   9    15  423   20   20          1.00000            0.00000
  10    15  474   20   20          1.00000            0.00000
  11    66   15   20   20          1.00000            0.00000
  12    66   66   20   20          1.00000            0.00000
  13    66  117   20   20          1.00000            0.00000
  14    66  168   20   20          1.00000            0.00000
  15    66  219   20   20          1.00000            0.00000
  16    66  270   20   20          1.00000            0.00000
  17    66  321   20   20          1.00000            0.00000
  18    66  372   20   20          1.00000            0.00000
  19    66  423   20   20          1.00000            0.00000
  20    66  474   20   20          1.00000            0.00000
  21   117   15   20   20          1.00000            0.00000
  22   117   66   20   20          1.00000            0.00000
  23   117  117   20   20          1.00000            0.00000
  24   117  168   20   20          1.00000            0.00000
  25   117  219   20   20          1.00000            0.00000
  26   117  270   20   20          1.00000            0.00000
  27   117  321   20   20          1.00000            0.00000
  28   117  372   20   20          1.00000            0.00000
  29   117  423   20   20          1.00000            0.00000
  30   117  474   20   20          1.00000            0.00000
  31   168   15   20   20          1.00000            0.00000
  32   168   66   20   20          1.00000            0.00000
  33   168  117   20   20          1.00000            0.00000
  34   168  168   20   20          1.00000            0.00000
  35   168  219   20   20          1.00000            0.00000
  36   168  270   20   20          1.00000            0.00000
  37   168  321   20   20          1.00000            0.00000
  38   168  372   20   20          1.00000            0.00000
  39   168  423   20   20          1.00000            0.00000
  40   168  474   20   20          1.00000            0.00000
  41   219   15   20   20          1.00000            0.00000
  42   219   66   20   20          1.00000            0.00000
  43   219  117   20   20          1.00000            0.00000
  44   219  168   20   20          1.00000            0.00000
  45   219  219   20   20          1.00000            0.00000
  46   219  270   20   20          1.00000            0.00000
  47   219  321   20   20          1.00000            0.00000
  48   219  372   20   20          1.00000            0.00000
  49   219  423   20   20          1.00000            0.00000
  50   219  474   20   20          1.00000            0.00000
  51   270   15   20   20          1.00000            0.00000
  52   270   66   20   20          1.00000            0.00000
  53   270  117   20   20          1.00000            0.00000
  54   270  168   20   20          1.00000            0.00000
  55   270  219   20   20          1.00000            0.00000
  56   270  270   20   20          1.00000            0.00000
  57   270  321   20   20          1.00000            0.00000
  58   270  372   20   20          1.00000            0.00000
  59   270  423   20   20          1.00000            0.00000
  60   270  474   20   20          1.00000            0.00000
  61   321   15   20   20          1.00000            0.00000
  62   321   66   20   20          1.00000            0.00000
  63   321  117   20   20          1.00000            0.00000
  64   321  168   20   20          1.00000            0.00000
  65   321  219   20   20          1.00000            0.00000
  66   321  270   20   20          1.00000            0.00000
  67   321  321   20   20          1.00000            0.00000
  68   321  372   20   20          1.00000            0.00000
  69   321  423   20   20          1.00000            0.00000
  70   321  474   20   20          1.00000            0.00000
  71   372   15   20   20          1.00000            0.00000
  72   372   66   20   20          1.00000            0.00000
  73   372  117   20   20          1.00000            0.00000
  74   372  168   20   20          1.00000            0.00000
  75   372  219   20   20          1.00000            0.00000
  76   372  270   20   20          1.00000            0.00000
  77   372  321   20   20          1.00000            0.00000
  78   372  372   20   20          1.00000            0.00000
  79   372  423   20   20          1.00000            0.00000
  80   372  474   20   20          1.00000            0.00000
  81   423   15   20   20          1.00000            0.00000
  82   423   66   20   20          1.00000            0.00000
  83   423  117   20   20          1.00000            0.00000
  84   423  168   20   20          1.00000            0.00000
  85   423  219   20   20          1.00000            0.00000
  86   423  270   20   20          1.00000            0.00000
  87   423  321   20   20          1.00000            0.00000
  88   423  372   20   20          1.00000            0.00000
  89   423  423   20   20          1.00000            0.00000
  90   423  474   20   20          1.00000            0.00000
  91   474   15   20   20          1.00000            0.00000
  92   474   66   20   20          1.00000            0.00000
  93   474  117   20   20          1.00000            0.00000
  94   474  168   20   20          1.00000            0.00000
  95   474  219   20   20          1.00000            0.00000
  96   474  270   20   20          1.00000            0.00000
  97   474  321   20   20          1.00000            0.00000
  98   474  372   20   20          1.00000            0.00000
  99   474  423   20   20          1.00000            0.00000
 100   474  474   20   20          1.00000            0.00000
Reticle           1
Means from averaging values of areas
 SLOPE  =  1.0000000
 OFFSET =  0.000E+00

STATISTICS FOR UPPER-LEFT  CORNER

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY    (DN-DC)    (DN-DC)    (DN-DC)
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
   10.0000    10.0000    10.0000      100.0000   100.0000   100.0000    0.0000
   20.0000    20.0000    10.0000      200.0000   200.0000   200.0000    0.0000
   40.0000    40.0000    10.0000      400.0000   400.0000   400.0000    0.0000
                                                                RMS=    0.0000
SLOPE=   1.000000 SD= 0.000000 OFFSET=   0.000000 SD= 0.000000
NUMBER OF GOOD AREAS=  15 OUT OF  15 AREAS SAMPLED
NO REJECTION CRITERIA APPLIED
GALGEN:C1=   1.00000 ENERGY UNIT/DN C2=   0.00000 ENERGY UNIT
Reticle           2
Means from averaging values of areas
 SLOPE  =  1.0000000
 OFFSET =  0.000E+00

STATISTICS FOR UPPER-RIGHT CORNER

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY    (DN-DC)    (DN-DC)    (DN-DC)
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
   10.0000    10.0000    10.0000      100.0000   100.0000   100.0000    0.0000
   20.0000    20.0000    10.0000      200.0000   200.0000   200.0000    0.0000
   40.0000    40.0000    10.0000      400.0000   400.0000   400.0000    0.0000
                                                                RMS=    0.0000
SLOPE=   1.000000 SD= 0.000000 OFFSET=   0.000000 SD= 0.000000
NUMBER OF GOOD AREAS=  15 OUT OF  15 AREAS SAMPLED
NO REJECTION CRITERIA APPLIED
GALGEN:C1=   1.00000 ENERGY UNIT/DN C2=   0.00000 ENERGY UNIT
Reticle           3
Means from averaging values of areas
 SLOPE  =  1.0000000
 OFFSET =  0.000E+00

STATISTICS FOR LOWER-LEFT  CORNER

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY    (DN-DC)    (DN-DC)    (DN-DC)
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
   10.0000    10.0000    10.0000      100.0000   100.0000   100.0000    0.0000
   20.0000    20.0000    10.0000      200.0000   200.0000   200.0000    0.0000
   40.0000    40.0000    10.0000      400.0000   400.0000   400.0000    0.0000
                                                                RMS=    0.0000
SLOPE=   1.000000 SD= 0.000000 OFFSET=   0.000000 SD= 0.000000
NUMBER OF GOOD AREAS=  15 OUT OF  15 AREAS SAMPLED
NO REJECTION CRITERIA APPLIED
GALGEN:C1=   1.00000 ENERGY UNIT/DN C2=   0.00000 ENERGY UNIT
Reticle           4
Means from averaging values of areas
 SLOPE  =  1.0000000
 OFFSET =  0.000E+00

STATISTICS FOR LOWER-RIGHT CORNER

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY    (DN-DC)    (DN-DC)    (DN-DC)
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
   10.0000    10.0000    10.0000      100.0000   100.0000   100.0000    0.0000
   20.0000    20.0000    10.0000      200.0000   200.0000   200.0000    0.0000
   40.0000    40.0000    10.0000      400.0000   400.0000   400.0000    0.0000
                                                                RMS=    0.0000
SLOPE=   1.000000 SD= 0.000000 OFFSET=   0.000000 SD= 0.000000
NUMBER OF GOOD AREAS=  15 OUT OF  15 AREAS SAMPLED
NO REJECTION CRITERIA APPLIED
GALGEN:C1=   1.00000 ENERGY UNIT/DN C2=   0.00000 ENERGY UNIT
Reticle           5
Means from averaging values of areas
 SLOPE  =  1.0000000
 OFFSET =  0.000E+00

STATISTICS FOR CENTER

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY    (DN-DC)    (DN-DC)    (DN-DC)
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
   10.0000    10.0000    10.0000      100.0000   100.0000   100.0000    0.0000
   20.0000    20.0000    10.0000      200.0000   200.0000   200.0000    0.0000
   40.0000    40.0000    10.0000      400.0000   400.0000   400.0000    0.0000
                                                                RMS=    0.0000
SLOPE=   1.000000 SD= 0.000000 OFFSET=   0.000000 SD= 0.000000
NUMBER OF GOOD AREAS=  40 OUT OF  40 AREAS SAMPLED
NO REJECTION CRITERIA APPLIED
GALGEN:C1=   1.00000 ENERGY UNIT/DN C2=   0.00000 ENERGY UNIT
Framewide
Means from averaging values of areas
 SLOPE=  1.0000000
 OFFSET=  0.000E+00

STATISTICS FOR FULL FRAME

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY    (DN-DC)    (DN-DC)    (DN-DC)
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
   10.0000    10.0000    10.0000      100.0000   100.0000   100.0000    0.0000
   20.0000    20.0000    10.0000      200.0000   200.0000   200.0000    0.0000
   40.0000    40.0000    10.0000      400.0000   400.0000   400.0000    0.0000
                                                                RMS=    0.0000
SLOPE=   1.000000 SD= 0.000000 OFFSET=   0.000000 SD= 0.000000
NUMBER OF GOOD AREAS=  40 OUT OF  40 AREAS SAMPLED
NO REJECTION CRITERIA APPLIED
GALGEN:C1=   1.00000 ENERGY UNIT/DN C2=   0.00000 ENERGY UNIT
CCDSLOPE task completed
if (mode = "nobatch" or mode = "inter")
end-if
gausnois a.img mean=0 sigma=3 format=HALF nl=512 ns=512 seed=512
Beginning VICAR task gausnois
f2 (cas/sum2.1, a.img) m1.a func=10+IN2
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 443 TIMES
gausnois a.img mean=0 sigma=3 format=HALF nl=512 ns=512 seed=612
Beginning VICAR task gausnois
f2 (cas/sum2.1, a.img) m1.b func=10+IN2
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 440 TIMES
gausnois a.img mean=0 sigma=3 format=HALF nl=512 ns=512 seed=712
Beginning VICAR task gausnois
f2 (cas/sum2.1, a.img) m1.c func=10+IN2
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 435 TIMES
gausnois a.img mean=0 sigma=10 format=HALF nl=512 ns=512 seed=812
Beginning VICAR task gausnois
f2 (cas/sum2.1, a.img) m2.a func=110+IN2
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 1312 TIMES
label-rep m2.a 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=10."
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword EXPOSURE_DURATION replaced
gausnois a.img mean=0 sigma=10 format=HALF nl=512 ns=512 seed=912
Beginning VICAR task gausnois
f2 (cas/sum2.1, a.img) m2.b func=110+IN2
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 1314 TIMES
label-rep m2.b 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=10."
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword EXPOSURE_DURATION replaced
gausnois a.img mean=0 sigma=10 format=HALF nl=512 ns=512 seed=112
Beginning VICAR task gausnois
f2 (cas/sum2.1, a.img) m2.c func=110+IN2
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 1304 TIMES
label-rep m2.c 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=10."
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword EXPOSURE_DURATION replaced
gausnois a.img mean=0 sigma=14 format=HALF nl=512 ns=512 seed=212
Beginning VICAR task gausnois
f2 (cas/sum2.1, a.img) m3.a func=210+IN2
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 1786 TIMES
label-rep m3.a 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=20."
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword EXPOSURE_DURATION replaced
gausnois a.img mean=0 sigma=14 format=HALF nl=512 ns=512 seed=312
Beginning VICAR task gausnois
f2 (cas/sum2.1, a.img) m3.b func=210+IN2
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 1802 TIMES
label-rep m3.b 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=20."
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword EXPOSURE_DURATION replaced
gausnois a.img mean=0 sigma=14 format=HALF nl=512 ns=512 seed=412
Beginning VICAR task gausnois
f2 (cas/sum2.1, a.img) m3.c func=210+IN2
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 1798 TIMES
label-rep m3.c 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=20."
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword EXPOSURE_DURATION replaced
gausnois a.img mean=0 sigma=20 format=HALF nl=512 ns=512 seed=544
Beginning VICAR task gausnois
f2 (cas/sum2.1, a.img) m4.a func=410+IN2
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 2448 TIMES
label-rep m4.a 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=40."
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword EXPOSURE_DURATION replaced
gausnois a.img mean=0 sigma=20 format=HALF nl=512 ns=512 seed=566
Beginning VICAR task gausnois
f2 (cas/sum2.1, a.img) m4.b func=410+IN2
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 2441 TIMES
label-rep m4.b 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=40."
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword EXPOSURE_DURATION replaced
gausnois a.img mean=0 sigma=20 format=HALF nl=512 ns=512 seed=588
Beginning VICAR task gausnois
f2 (cas/sum2.1, a.img) m4.c func=410+IN2
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 2444 TIMES
label-rep m4.c 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=40."
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword EXPOSURE_DURATION replaced
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
NEXT FILE =     1
m1.a
m1.b
m1.c
m2.a
m2.b
m2.c
m3.a
m3.b
m3.c
m4.a
m4.b
m4.c
ltgen m1.a out=test.ltfn list=m.list 'GRID
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
momgen2 list=m.list ltfrcp=test.ltfn
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
   let LUMS = "5.099999904633e+00"
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
   let LUMS = "5.099999904633e+00"
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
   let LUMS = "5.099999904633e+00"
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
   let LUMS = "5.099999904633e+00"
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
   let LUMS = "5.099999904633e+00"
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
   let LUMS = "5.099999904633e+00"
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
   let LUMS = "5.099999904633e+00"
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
   let LUMS = "5.099999904633e+00"
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
   let LUMS = "5.099999904633e+00"
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
   write "       2              1.000000000000e+01             5.099999904633e+00"
       2              1.000000000000e+01             5.099999904633e+00
   if (I=NLVL) break
   let I=I+1
end-loop
   let X=EX(I)
   let EXS = "2.000000000000e+01"
   let LUMS=LMS(I)
   write "       3              2.000000000000e+01             5.099999904633e+00"
       3              2.000000000000e+01             5.099999904633e+00
   if (I=NLVL) break
   let I=I+1
end-loop
   let X=EX(I)
   let EXS = "4.000000000000e+01"
   let LUMS=LMS(I)
   write "       4              4.000000000000e+01             5.099999904633e+00"
       4              4.000000000000e+01             5.099999904633e+00
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
NEXT FILE=0001
m1.a
m1.b
m1.c
   write " "
 
   let $BECHO="YES"
   momgen LIST="list"1".dat" out=test.ltfn
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
NEXT FILE=0001
m2.a
m2.b
m2.c
   write " "
 
   let $BECHO="YES"
   momgen LIST="list"2".dat" out=test.ltfn
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
NEXT FILE=0001
m3.a
m3.b
m3.c
   write " "
 
   let $BECHO="YES"
   momgen LIST="list"3".dat" out=test.ltfn
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
NEXT FILE=0001
m4.a
m4.b
m4.c
   write " "
 
   let $BECHO="YES"
   momgen LIST="list"4".dat" out=test.ltfn
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
if ($syschar(1)="UNIX")
   ush rm m1.*
   ush rm m2.*
   ush rm m3.*
   ush rm m4.*
   ush rm m.list
else
end-if
ccdslope test.ltfn table=ccdslope2n.tbl mofset=0.0 light=10. rej=0 'SUBDC  +
    plot=test2
Beginning VICAR task ccdslope
CCDSLOPE version 07-Jul-2013  (64-bit) - rjb
 SIGMA REJ TOL=          2
Mean shutter-offset =  0.000E+00
Shutter-offset is LINE-dependent
DC subtraction mode
----TASK:F2      ----USER:wlb         Mon Jun  9 16:32:38 2014
----TASK:LTGEN   ----USER:wlb         Mon Jun  9 16:32:42 2014

GLOBAL VALUE FOR SLOPE...
Raw mean and sigma are...
N= 100 MEAN=     0.99986 SIGMA=     0.00220
After throwing out samples differing by 2 sigma
N=  97 MEAN=     0.99977 SIGMA=     0.00193

GLOBAL VALUE FOR OFFSET...
Raw mean and sigma are...
N= 100 MEAN=     0.02747 SIGMA=     0.44247
After throwing out samples differing by 2 sigma
N=  97 MEAN=     0.04438 SIGMA=     0.39222

Summary of bad areas....
AREA  30 (SL,SS,NL,NS)=( 117, 474,  20,  20)  ****BOTH BAD FIT*********
AREA  86 (SL,SS,NL,NS)=( 423, 270,  20,  20)  ****BOTH BAD FIT*********
AREA 100 (SL,SS,NL,NS)=( 474, 474,  20,  20)  ****BOTH BAD FIT*********


Slopes and offsets for each area...

ENERGY UNIT = PICOAMP-MILLISECONDS

AREA    SL   SS   NL   NS  SLOPE (DN/ENERGY UNIT)  OFFSET (DN)
   1    15   15   20   20          1.00016           -0.13708
   2    15   66   20   20          1.00235           -0.64333
   3    15  117   20   20          1.00243           -0.15500
   4    15  168   20   20          0.99905           -0.06375
   5    15  219   20   20          0.99747           -0.01625
   6    15  270   20   20          0.99768            0.25958
   7    15  321   20   20          1.00364           -0.52500
   8    15  372   20   20          0.99941            0.23958
   9    15  423   20   20          0.99571            0.87292
  10    15  474   20   20          1.00158           -0.50250
  11    66   15   20   20          1.00009           -0.05417
  12    66   66   20   20          0.99906            0.03083
  13    66  117   20   20          1.00140           -0.14583
  14    66  168   20   20          0.99903            0.04917
  15    66  219   20   20          1.00044           -0.03292
  16    66  270   20   20          1.00210           -0.46792
  17    66  321   20   20          0.99794            0.39083
  18    66  372   20   20          0.99999            0.11708
  19    66  423   20   20          0.99953            0.08083
  20    66  474   20   20          1.00066           -0.03750
  21   117   15   20   20          1.00069           -0.17000
  22   117   66   20   20          0.99914            0.35458
  23   117  117   20   20          0.99811            0.45708
  24   117  168   20   20          0.99966           -0.02750
  25   117  219   20   20          0.99796            0.42833
  26   117  270   20   20          0.99810            0.40917
  27   117  321   20   20          0.99800            0.47167
  28   117  372   20   20          1.00331           -0.63583
  29   117  423   20   20          0.99997           -0.08333
  30   117  474   20   20          0.99464            1.02292
  31   168   15   20   20          0.99660            0.62792
  32   168   66   20   20          1.00128           -0.20667
  33   168  117   20   20          0.99756            0.51208
  34   168  168   20   20          0.99942            0.22125
  35   168  219   20   20          0.99961            0.05208
  36   168  270   20   20          1.00043            0.26833
  37   168  321   20   20          0.99863            0.16750
  38   168  372   20   20          1.00250           -0.68958
  39   168  423   20   20          1.00204           -0.46250
  40   168  474   20   20          0.99974            0.26000
  41   219   15   20   20          0.99919           -0.15333
  42   219   66   20   20          1.00226           -0.41125
  43   219  117   20   20          1.00017            0.20000
  44   219  168   20   20          1.00149           -0.72583
  45   219  219   20   20          0.99704            0.44542
  46   219  270   20   20          1.00259           -0.42792
  47   219  321   20   20          1.00065           -0.15333
  48   219  372   20   20          0.99967           -0.01750
  49   219  423   20   20          1.00099           -0.03833
  50   219  474   20   20          1.00074           -0.49750
  51   270   15   20   20          0.99997            0.43833
  52   270   66   20   20          0.99686            0.90542
  53   270  117   20   20          0.99659            0.41833
  54   270  168   20   20          1.00200           -0.15250
  55   270  219   20   20          0.99650            0.90375
  56   270  270   20   20          1.00021           -0.20542
  57   270  321   20   20          1.00060           -0.05375
  58   270  372   20   20          0.99990            0.06208
  59   270  423   20   20          0.99934            0.13167
  60   270  474   20   20          0.99852            0.52167
  61   321   15   20   20          1.00265           -0.73083
  62   321   66   20   20          1.00015            0.25083
  63   321  117   20   20          0.99822           -0.02458
  64   321  168   20   20          1.00130           -0.21000
  65   321  219   20   20          1.00260           -0.43333
  66   321  270   20   20          0.99839            0.48000
  67   321  321   20   20          0.99902           -0.15417
  68   321  372   20   20          1.00265           -0.32708
  69   321  423   20   20          0.99865            0.54625
  70   321  474   20   20          1.00256           -0.21417
  71   372   15   20   20          1.00148           -0.36750
  72   372   66   20   20          0.99967           -0.21625
  73   372  117   20   20          0.99799            0.08958
  74   372  168   20   20          1.00079           -0.39000
  75   372  219   20   20          0.99945            0.40417
  76   372  270   20   20          0.99950            0.03542
  77   372  321   20   20          0.99694            0.67500
  78   372  372   20   20          1.00281           -0.51875
  79   372  423   20   20          1.00032            0.22292
  80   372  474   20   20          0.99552            0.88292
  81   423   15   20   20          0.99872            0.38042
  82   423   66   20   20          0.99832            0.39875
  83   423  117   20   20          0.99829            0.61208
  84   423  168   20   20          1.00285           -0.55542
  85   423  219   20   20          0.99943            0.50500
  86   423  270   20   20          1.00811           -1.57625
  87   423  321   20   20          1.00346           -0.37500
  88   423  372   20   20          1.00171           -0.22250
  89   423  423   20   20          1.00297           -0.34875
  90   423  474   20   20          1.00016            0.05958
  91   474   15   20   20          0.99920            0.34375
  92   474   66   20   20          0.99875            0.34208
  93   474  117   20   20          0.99829           -0.05958
  94   474  168   20   20          0.99715            0.22417
  95   474  219   20   20          0.99600            0.64667
  96   474  270   20   20          0.99693           -0.14958
  97   474  321   20   20          0.99913           -0.03375
  98   474  372   20   20          1.00048           -0.23958
  99   474  423   20   20          0.99761            0.37208
 100   474  474   20   20          1.00522           -1.00458
Reticle           1
Means from averaging values of areas
 SLOPE  = 0.99973634
 OFFSET = -0.01505555

STATISTICS FOR UPPER-LEFT  CORNER

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY    (DN-DC)    (DN-DC)    (DN-DC)
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
   10.0000    10.0000    10.0000      100.0000    99.9775    99.9586    0.0189
   20.0000    20.0000    10.0000      200.0000   199.9038   199.9322   -0.0284
   40.0000    40.0000    10.0000      400.0000   399.8889   399.8795    0.0095
                                                                RMS=    0.0204
SLOPE=   0.999736 SD= 0.001636 OFFSET=  -0.015056 SD= 0.295437
NUMBER OF GOOD AREAS=  15 OUT OF  15 AREAS SAMPLED
NO REJECTION CRITERIA APPLIED
GALGEN:C1=   1.00026 ENERGY UNIT/DN C2=   0.01506 ENERGY UNIT
Reticle           2
Means from averaging values of areas
 SLOPE  = 0.99977156
 OFFSET = 0.03330556

STATISTICS FOR UPPER-RIGHT CORNER

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY    (DN-DC)    (DN-DC)    (DN-DC)
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
   10.0000    10.0000    10.0000      100.0000    99.9583   100.0105   -0.0522
   20.0000    20.0000    10.0000      200.0000   200.0659   199.9876    0.0783
   40.0000    40.0000    10.0000      400.0000   399.9158   399.9419   -0.0261
                                                                RMS=    0.0564
SLOPE=   0.999772 SD= 0.002428 OFFSET=   0.033306 SD= 0.487118
NUMBER OF GOOD AREAS=  15 OUT OF  15 AREAS SAMPLED
NO REJECTION CRITERIA APPLIED
GALGEN:C1=   1.00023 ENERGY UNIT/DN C2=  -0.03331 ENERGY UNIT
Reticle           3
Means from averaging values of areas
 SLOPE  = 0.99929750
 OFFSET = 0.11980555

STATISTICS FOR LOWER-LEFT  CORNER

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY    (DN-DC)    (DN-DC)    (DN-DC)
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
   10.0000    10.0000    10.0000      100.0000   100.0802   100.0496    0.0306
   20.0000    20.0000    10.0000      200.0000   199.9334   199.9793   -0.0459
   40.0000    40.0000    10.0000      400.0000   399.8541   399.8388    0.0153
                                                                RMS=    0.0331
SLOPE=   0.999298 SD= 0.001840 OFFSET=   0.119806 SD= 0.404782
NUMBER OF GOOD AREAS=  15 OUT OF  15 AREAS SAMPLED
NO REJECTION CRITERIA APPLIED
GALGEN:C1=   1.00070 ENERGY UNIT/DN C2=  -0.11989 ENERGY UNIT
Reticle           4
Means from averaging values of areas
 SLOPE  =  1.0004043
 OFFSET = -0.03341667

STATISTICS FOR LOWER-RIGHT CORNER

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY    (DN-DC)    (DN-DC)    (DN-DC)
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
   10.0000    10.0000    10.0000      100.0000   100.0731   100.0070    0.0661
   20.0000    20.0000    10.0000      200.0000   199.9483   200.0474   -0.0992
   40.0000    40.0000    10.0000      400.0000   400.1613   400.1283    0.0331
                                                                RMS=    0.0714
SLOPE=   1.000404 SD= 0.002627 OFFSET=  -0.033417 SD= 0.464890
NUMBER OF GOOD AREAS=  15 OUT OF  15 AREAS SAMPLED
NO REJECTION CRITERIA APPLIED
GALGEN:C1=   0.99960 ENERGY UNIT/DN C2=   0.03340 ENERGY UNIT
Reticle           5
Means from averaging values of areas
 SLOPE  = 0.99994215
 OFFSET = 0.02943750

STATISTICS FOR CENTER

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY    (DN-DC)    (DN-DC)    (DN-DC)
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
   10.0000    10.0000    10.0000      100.0000   100.0266   100.0237    0.0030
   20.0000    20.0000    10.0000      200.0000   200.0134   200.0179   -0.0045
   40.0000    40.0000    10.0000      400.0000   400.0078   400.0063    0.0015
                                                                RMS=    0.0032
SLOPE=   0.999942 SD= 0.002197 OFFSET=   0.029437 SD= 0.468997
NUMBER OF GOOD AREAS=  40 OUT OF  40 AREAS SAMPLED
NO REJECTION CRITERIA APPLIED
GALGEN:C1=   1.00006 ENERGY UNIT/DN C2=  -0.02944 ENERGY UNIT
Framewide
Means from averaging values of areas
 SLOPE= 0.99985831
 OFFSET= 0.02747083

STATISTICS FOR FULL FRAME

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY    (DN-DC)    (DN-DC)    (DN-DC)
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
   10.0000    10.0000    10.0000      100.0000   100.0240   100.0133    0.0107
   20.0000    20.0000    10.0000      200.0000   199.9831   199.9991   -0.0161
   40.0000    40.0000    10.0000      400.0000   399.9761   399.9708    0.0054
                                                                RMS=    0.0116
SLOPE=   0.999858 SD= 0.002204 OFFSET=   0.027471 SD= 0.442467
NUMBER OF GOOD AREAS=  40 OUT OF  40 AREAS SAMPLED
NO REJECTION CRITERIA APPLIED
GALGEN:C1=   1.00014 ENERGY UNIT/DN C2=  -0.02747 ENERGY UNIT
CCDSLOPE task completed
if (mode = "nobatch" or mode = "inter")
end-if
ccdslope test.ltf mark.out table=ccdslope3.tbl offsets=cas/sos.dat rej=3  +
    plot=test3
Beginning VICAR task ccdslope
CCDSLOPE version 07-Jul-2013  (64-bit) - rjb
 SIGMA REJ TOL=          2
Mean shutter-offset = 0.93261701
Shutter-offset is SAMP-dependent
DC included as data point on curve
----TASK:COPY    ----USER:wlb         Mon Jun  9 16:32:32 2014
----TASK:F2      ----USER:wlb         Mon Jun  9 16:32:32 2014
----TASK:LTGEN   ----USER:wlb         Mon Jun  9 16:32:35 2014

GLOBAL VALUE FOR SLOPE...
Raw mean and sigma are...
N= 100 MEAN=     1.99551 SIGMA=     0.00164
After throwing out samples differing by 2 sigma
N=  90 MEAN=     1.99508 SIGMA=     0.00105

GLOBAL VALUE FOR OFFSET...
Raw mean and sigma are...
N= 100 MEAN=    13.68306 SIGMA=     0.17981
After throwing out samples differing by 2 sigma
N=  90 MEAN=    13.63543 SIGMA=     0.11503

Summary of bad areas....
AREA   1 (SL,SS,NL,NS)=(  15,  15,  20,  20)  ****BOTH BAD FIT*********
AREA  11 (SL,SS,NL,NS)=(  66,  15,  20,  20)  ****BOTH BAD FIT*********
AREA  21 (SL,SS,NL,NS)=( 117,  15,  20,  20)  ****BOTH BAD FIT*********
AREA  31 (SL,SS,NL,NS)=( 168,  15,  20,  20)  ****BOTH BAD FIT*********
AREA  41 (SL,SS,NL,NS)=( 219,  15,  20,  20)  ****BOTH BAD FIT*********
AREA  51 (SL,SS,NL,NS)=( 270,  15,  20,  20)  ****BOTH BAD FIT*********
AREA  61 (SL,SS,NL,NS)=( 321,  15,  20,  20)  ****BOTH BAD FIT*********
AREA  71 (SL,SS,NL,NS)=( 372,  15,  20,  20)  ****BOTH BAD FIT*********
AREA  81 (SL,SS,NL,NS)=( 423,  15,  20,  20)  ****BOTH BAD FIT*********
AREA  91 (SL,SS,NL,NS)=( 474,  15,  20,  20)  ****BOTH BAD FIT*********


Slopes and offsets for each area...

ENERGY UNIT = PICOAMP-MILLISECONDS

AREA    SL   SS   NL   NS  SLOPE (DN/ENERGY UNIT)  OFFSET (DN)
   1    15   15   20   20          1.99939           14.11181
   2    15   66   20   20          1.99717           13.86541
   3    15  117   20   20          1.99495           13.62012
   4    15  168   20   20          1.99386           13.50086
   5    15  219   20   20          1.99396           13.51183
   6    15  270   20   20          1.99421           13.53925
   7    15  321   20   20          1.99462           13.58382
   8    15  372   20   20          1.99509           13.63587
   9    15  423   20   20          1.99563           13.69575
  10    15  474   20   20          1.99627           13.76591
  11    66   15   20   20          1.99939           14.11181
  12    66   66   20   20          1.99717           13.86541
  13    66  117   20   20          1.99495           13.62012
  14    66  168   20   20          1.99386           13.50086
  15    66  219   20   20          1.99396           13.51183
  16    66  270   20   20          1.99421           13.53925
  17    66  321   20   20          1.99462           13.58382
  18    66  372   20   20          1.99509           13.63587
  19    66  423   20   20          1.99563           13.69575
  20    66  474   20   20          1.99627           13.76591
  21   117   15   20   20          1.99939           14.11181
  22   117   66   20   20          1.99717           13.86541
  23   117  117   20   20          1.99495           13.62012
  24   117  168   20   20          1.99386           13.50086
  25   117  219   20   20          1.99396           13.51183
  26   117  270   20   20          1.99421           13.53925
  27   117  321   20   20          1.99462           13.58382
  28   117  372   20   20          1.99509           13.63587
  29   117  423   20   20          1.99563           13.69575
  30   117  474   20   20          1.99627           13.76591
  31   168   15   20   20          1.99939           14.11181
  32   168   66   20   20          1.99717           13.86541
  33   168  117   20   20          1.99495           13.62012
  34   168  168   20   20          1.99386           13.50086
  35   168  219   20   20          1.99396           13.51183
  36   168  270   20   20          1.99421           13.53925
  37   168  321   20   20          1.99462           13.58382
  38   168  372   20   20          1.99509           13.63587
  39   168  423   20   20          1.99563           13.69575
  40   168  474   20   20          1.99627           13.76591
  41   219   15   20   20          1.99939           14.11181
  42   219   66   20   20          1.99717           13.86541
  43   219  117   20   20          1.99495           13.62012
  44   219  168   20   20          1.99386           13.50086
  45   219  219   20   20          1.99396           13.51183
  46   219  270   20   20          1.99421           13.53925
  47   219  321   20   20          1.99462           13.58382
  48   219  372   20   20          1.99509           13.63587
  49   219  423   20   20          1.99563           13.69575
  50   219  474   20   20          1.99627           13.76591
  51   270   15   20   20          1.99939           14.11181
  52   270   66   20   20          1.99717           13.86541
  53   270  117   20   20          1.99495           13.62012
  54   270  168   20   20          1.99386           13.50086
  55   270  219   20   20          1.99396           13.51183
  56   270  270   20   20          1.99421           13.53925
  57   270  321   20   20          1.99462           13.58382
  58   270  372   20   20          1.99509           13.63587
  59   270  423   20   20          1.99563           13.69575
  60   270  474   20   20          1.99627           13.76591
  61   321   15   20   20          1.99939           14.11181
  62   321   66   20   20          1.99717           13.86541
  63   321  117   20   20          1.99495           13.62012
  64   321  168   20   20          1.99386           13.50086
  65   321  219   20   20          1.99396           13.51183
  66   321  270   20   20          1.99421           13.53925
  67   321  321   20   20          1.99462           13.58382
  68   321  372   20   20          1.99509           13.63587
  69   321  423   20   20          1.99563           13.69575
  70   321  474   20   20          1.99627           13.76591
  71   372   15   20   20          1.99939           14.11181
  72   372   66   20   20          1.99717           13.86541
  73   372  117   20   20          1.99495           13.62012
  74   372  168   20   20          1.99386           13.50086
  75   372  219   20   20          1.99396           13.51183
  76   372  270   20   20          1.99421           13.53925
  77   372  321   20   20          1.99462           13.58382
  78   372  372   20   20          1.99509           13.63587
  79   372  423   20   20          1.99563           13.69575
  80   372  474   20   20          1.99627           13.76591
  81   423   15   20   20          1.99939           14.11181
  82   423   66   20   20          1.99717           13.86541
  83   423  117   20   20          1.99495           13.62012
  84   423  168   20   20          1.99386           13.50086
  85   423  219   20   20          1.99396           13.51183
  86   423  270   20   20          1.99421           13.53925
  87   423  321   20   20          1.99462           13.58382
  88   423  372   20   20          1.99509           13.63587
  89   423  423   20   20          1.99563           13.69575
  90   423  474   20   20          1.99627           13.76591
  91   474   15   20   20          1.99939           14.11181
  92   474   66   20   20          1.99717           13.86541
  93   474  117   20   20          1.99495           13.62012
  94   474  168   20   20          1.99386           13.50086
  95   474  219   20   20          1.99396           13.51183
  96   474  270   20   20          1.99421           13.53925
  97   474  321   20   20          1.99462           13.58382
  98   474  372   20   20          1.99509           13.63587
  99   474  423   20   20          1.99563           13.69575
 100   474  474   20   20          1.99627           13.76591
Reticle           1
Means from averaging values of areas
 SLOPE  =  1.9955198
 OFFSET =  13.683555

STATISTICS FOR UPPER-LEFT  CORNER

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY       DN         DN         DN
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
    0.0000     0.0000     5.1000        0.0000    10.0000    13.6833   -3.6833
   10.0000     9.1113     5.1000       46.4675   110.0000   106.4102    3.5898
   20.0000    19.1113     5.1000       97.4675   210.0000   208.1817    1.8183
   40.0000    39.1113     5.1000      199.4675   410.0000   411.7248   -1.7248
                                                                RMS=    2.8607
SLOPE=   1.995520 SD= 0.001416 OFFSET=  13.683336 SD= 0.155105
NUMBER OF GOOD AREAS=  10 OUT OF  15 AREAS SAMPLED
NUMBER REJECTED FOR      BOTH          5
GALGEN:C1=   0.50112 ENERGY UNIT/DN C2=  -6.85703 ENERGY UNIT
Reticle           2
Means from averaging values of areas
 SLOPE  =  1.9955057
 OFFSET =  13.681805

STATISTICS FOR UPPER-RIGHT CORNER

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY       DN         DN         DN
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
    0.0000     0.0000     5.1000        0.0000    10.0000    13.6818   -3.6818
   10.0000     9.1116     5.1000       46.4694   110.0000   106.4117    3.5883
   20.0000    19.1116     5.1000       97.4694   210.0000   208.1825    1.8175
   40.0000    39.1116     5.1000      199.4694   410.0000   411.7241   -1.7241
                                                                RMS=    2.8595
SLOPE=   1.995506 SD= 0.000657 OFFSET=  13.681756 SD= 0.073921
NUMBER OF GOOD AREAS=  15 OUT OF  15 AREAS SAMPLED
NUMBER REJECTED FOR      BOTH          0
GALGEN:C1=   0.50113 ENERGY UNIT/DN C2=  -6.85628 ENERGY UNIT
Reticle           3
Means from averaging values of areas
 SLOPE  =  1.9955198
 OFFSET =  13.683555

STATISTICS FOR LOWER-LEFT  CORNER

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY       DN         DN         DN
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
    0.0000     0.0000     5.1000        0.0000    10.0000    13.6833   -3.6833
   10.0000     9.1113     5.1000       46.4675   110.0000   106.4102    3.5898
   20.0000    19.1113     5.1000       97.4675   210.0000   208.1817    1.8183
   40.0000    39.1113     5.1000      199.4675   410.0000   411.7248   -1.7248
                                                                RMS=    2.8607
SLOPE=   1.995520 SD= 0.001416 OFFSET=  13.683336 SD= 0.155105
NUMBER OF GOOD AREAS=  10 OUT OF  15 AREAS SAMPLED
NUMBER REJECTED FOR      BOTH          5
GALGEN:C1=   0.50112 ENERGY UNIT/DN C2=  -6.85703 ENERGY UNIT
Reticle           4
Means from averaging values of areas
 SLOPE  =  1.9955057
 OFFSET =  13.681805

STATISTICS FOR LOWER-RIGHT CORNER

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY       DN         DN         DN
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
    0.0000     0.0000     5.1000        0.0000    10.0000    13.6818   -3.6818
   10.0000     9.1116     5.1000       46.4694   110.0000   106.4117    3.5883
   20.0000    19.1116     5.1000       97.4694   210.0000   208.1825    1.8175
   40.0000    39.1116     5.1000      199.4694   410.0000   411.7241   -1.7241
                                                                RMS=    2.8595
SLOPE=   1.995506 SD= 0.000657 OFFSET=  13.681756 SD= 0.073921
NUMBER OF GOOD AREAS=  15 OUT OF  15 AREAS SAMPLED
NUMBER REJECTED FOR      BOTH          0
GALGEN:C1=   0.50113 ENERGY UNIT/DN C2=  -6.85628 ENERGY UNIT
Reticle           5
Means from averaging values of areas
 SLOPE  =  1.9945501
 OFFSET =  13.576577

STATISTICS FOR CENTER

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY       DN         DN         DN
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
    0.0000     0.0000     5.1000        0.0000    10.0000    13.5765   -3.5765
   10.0000     9.1362     5.1000       46.5946   110.0000   106.5118    3.4882
   20.0000    19.1362     5.1000       97.5946   210.0000   208.2338    1.7662
   40.0000    39.1362     5.1000      199.5946   410.0000   411.6779   -1.6779
                                                                RMS=    2.7791
SLOPE=   1.994550 SD= 0.000805 OFFSET=  13.576509 SD= 0.086070
NUMBER OF GOOD AREAS=  40 OUT OF  40 AREAS SAMPLED
NUMBER REJECTED FOR      BOTH          0
GALGEN:C1=   0.50137 ENERGY UNIT/DN C2=  -6.80680 ENERGY UNIT
Framewide
Means from averaging values of areas
 SLOPE=  1.9950841
 OFFSET=  13.635426

STATISTICS FOR FULL FRAME

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY       DN         DN         DN
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
    0.0000     0.0000     5.1000        0.0000    10.0000    13.6353   -3.6353
   10.0000     9.1225     5.1000       46.5246   110.0000   106.4558    3.5442
   20.0000    19.1225     5.1000       97.5246   210.0000   208.2051    1.7949
   40.0000    39.1225     5.1000      199.5246   410.0000   411.7037   -1.7037
                                                                RMS=    2.8240
SLOPE=   1.995084 SD= 0.001052 OFFSET=  13.635305 SD= 0.115026
NUMBER OF GOOD AREAS=  40 OUT OF  40 AREAS SAMPLED
NUMBER REJECTED FOR      BOTH          0
GALGEN:C1=   0.50123 ENERGY UNIT/DN C2=  -6.83445 ENERGY UNIT
CCDSLOPE task completed
if (mode = "nobatch" or mode = "inter")
end-if
ccdslope test.ltf  offsets=cas/sol.dat rej=2   +
   'deltax sigtol=1.0 plot=test4
Beginning VICAR task ccdslope
CCDSLOPE version 07-Jul-2013  (64-bit) - rjb
 SIGMA REJ TOL=          1
Mean shutter-offset = 0.93261701
Shutter-offset is LINE-dependent
DC included as data point on curve
----TASK:COPY    ----USER:wlb         Mon Jun  9 16:32:32 2014
----TASK:F2      ----USER:wlb         Mon Jun  9 16:32:32 2014
----TASK:LTGEN   ----USER:wlb         Mon Jun  9 16:32:35 2014

GLOBAL VALUE FOR SLOPE...
Raw mean and sigma are...
N= 100 MEAN=     1.99551 SIGMA=     0.00164
After throwing out samples differing by 1 sigma
N=  70 MEAN=     1.99496 SIGMA=     0.00075

GLOBAL VALUE FOR OFFSET...
Raw mean and sigma are...
N= 100 MEAN=    13.68306 SIGMA=     0.17981
After throwing out samples differing by 1 sigma
N=  70 MEAN=    13.62179 SIGMA=     0.08175

Summary of bad areas....
AREA   1 (SL,SS,NL,NS)=(  15,  15,  20,  20)  ****BOTH BAD FIT*********
AREA   2 (SL,SS,NL,NS)=(  15,  66,  20,  20)  ****BOTH BAD FIT*********
AREA   3 (SL,SS,NL,NS)=(  15, 117,  20,  20)  ****BOTH BAD FIT*********
AREA   4 (SL,SS,NL,NS)=(  15, 168,  20,  20)  ****BOTH BAD FIT*********
AREA   5 (SL,SS,NL,NS)=(  15, 219,  20,  20)  ****BOTH BAD FIT*********
AREA   6 (SL,SS,NL,NS)=(  15, 270,  20,  20)  ****BOTH BAD FIT*********
AREA   7 (SL,SS,NL,NS)=(  15, 321,  20,  20)  ****BOTH BAD FIT*********
AREA   8 (SL,SS,NL,NS)=(  15, 372,  20,  20)  ****BOTH BAD FIT*********
AREA   9 (SL,SS,NL,NS)=(  15, 423,  20,  20)  ****BOTH BAD FIT*********
AREA  10 (SL,SS,NL,NS)=(  15, 474,  20,  20)  ****BOTH BAD FIT*********
AREA  11 (SL,SS,NL,NS)=(  66,  15,  20,  20)  ****BOTH BAD FIT*********
AREA  12 (SL,SS,NL,NS)=(  66,  66,  20,  20)  ****BOTH BAD FIT*********
AREA  13 (SL,SS,NL,NS)=(  66, 117,  20,  20)  ****BOTH BAD FIT*********
AREA  14 (SL,SS,NL,NS)=(  66, 168,  20,  20)  ****BOTH BAD FIT*********
AREA  15 (SL,SS,NL,NS)=(  66, 219,  20,  20)  ****BOTH BAD FIT*********
AREA  16 (SL,SS,NL,NS)=(  66, 270,  20,  20)  ****BOTH BAD FIT*********
AREA  17 (SL,SS,NL,NS)=(  66, 321,  20,  20)  ****BOTH BAD FIT*********
AREA  18 (SL,SS,NL,NS)=(  66, 372,  20,  20)  ****BOTH BAD FIT*********
AREA  19 (SL,SS,NL,NS)=(  66, 423,  20,  20)  ****BOTH BAD FIT*********
AREA  20 (SL,SS,NL,NS)=(  66, 474,  20,  20)  ****BOTH BAD FIT*********
AREA  31 (SL,SS,NL,NS)=( 168,  15,  20,  20)  ****BOTH BAD FIT*********
AREA  32 (SL,SS,NL,NS)=( 168,  66,  20,  20)  ****BOTH BAD FIT*********
AREA  33 (SL,SS,NL,NS)=( 168, 117,  20,  20)  ****BOTH BAD FIT*********
AREA  34 (SL,SS,NL,NS)=( 168, 168,  20,  20)  ****BOTH BAD FIT*********
AREA  35 (SL,SS,NL,NS)=( 168, 219,  20,  20)  ****BOTH BAD FIT*********
AREA  36 (SL,SS,NL,NS)=( 168, 270,  20,  20)  ****BOTH BAD FIT*********
AREA  37 (SL,SS,NL,NS)=( 168, 321,  20,  20)  ****BOTH BAD FIT*********
AREA  38 (SL,SS,NL,NS)=( 168, 372,  20,  20)  ****BOTH BAD FIT*********
AREA  39 (SL,SS,NL,NS)=( 168, 423,  20,  20)  ****BOTH BAD FIT*********
AREA  40 (SL,SS,NL,NS)=( 168, 474,  20,  20)  ****BOTH BAD FIT*********


Slopes and offsets for each area...

ENERGY UNIT = PICOAMP-MILLISECONDS

AREA    SL   SS   NL   NS  SLOPE (DN/ENERGY UNIT)  OFFSET (DN)
   1    15   15   20   20          1.99939           14.11181
   2    15   66   20   20          1.99939           14.11181
   3    15  117   20   20          1.99939           14.11181
   4    15  168   20   20          1.99939           14.11181
   5    15  219   20   20          1.99939           14.11181
   6    15  270   20   20          1.99939           14.11181
   7    15  321   20   20          1.99939           14.11181
   8    15  372   20   20          1.99939           14.11181
   9    15  423   20   20          1.99939           14.11181
  10    15  474   20   20          1.99939           14.11181
  11    66   15   20   20          1.99717           13.86541
  12    66   66   20   20          1.99717           13.86541
  13    66  117   20   20          1.99717           13.86541
  14    66  168   20   20          1.99717           13.86541
  15    66  219   20   20          1.99717           13.86541
  16    66  270   20   20          1.99717           13.86541
  17    66  321   20   20          1.99717           13.86541
  18    66  372   20   20          1.99717           13.86541
  19    66  423   20   20          1.99717           13.86541
  20    66  474   20   20          1.99717           13.86541
  21   117   15   20   20          1.99495           13.62012
  22   117   66   20   20          1.99495           13.62012
  23   117  117   20   20          1.99495           13.62012
  24   117  168   20   20          1.99495           13.62012
  25   117  219   20   20          1.99495           13.62012
  26   117  270   20   20          1.99495           13.62012
  27   117  321   20   20          1.99495           13.62012
  28   117  372   20   20          1.99495           13.62012
  29   117  423   20   20          1.99495           13.62012
  30   117  474   20   20          1.99495           13.62012
  31   168   15   20   20          1.99386           13.50086
  32   168   66   20   20          1.99386           13.50086
  33   168  117   20   20          1.99386           13.50086
  34   168  168   20   20          1.99386           13.50086
  35   168  219   20   20          1.99386           13.50086
  36   168  270   20   20          1.99386           13.50086
  37   168  321   20   20          1.99386           13.50086
  38   168  372   20   20          1.99386           13.50086
  39   168  423   20   20          1.99386           13.50086
  40   168  474   20   20          1.99386           13.50086
  41   219   15   20   20          1.99396           13.51183
  42   219   66   20   20          1.99396           13.51183
  43   219  117   20   20          1.99396           13.51183
  44   219  168   20   20          1.99396           13.51183
  45   219  219   20   20          1.99396           13.51183
  46   219  270   20   20          1.99396           13.51183
  47   219  321   20   20          1.99396           13.51183
  48   219  372   20   20          1.99396           13.51183
  49   219  423   20   20          1.99396           13.51183
  50   219  474   20   20          1.99396           13.51183
  51   270   15   20   20          1.99421           13.53925
  52   270   66   20   20          1.99421           13.53925
  53   270  117   20   20          1.99421           13.53925
  54   270  168   20   20          1.99421           13.53925
  55   270  219   20   20          1.99421           13.53925
  56   270  270   20   20          1.99421           13.53925
  57   270  321   20   20          1.99421           13.53925
  58   270  372   20   20          1.99421           13.53925
  59   270  423   20   20          1.99421           13.53925
  60   270  474   20   20          1.99421           13.53925
  61   321   15   20   20          1.99462           13.58382
  62   321   66   20   20          1.99462           13.58382
  63   321  117   20   20          1.99462           13.58382
  64   321  168   20   20          1.99462           13.58382
  65   321  219   20   20          1.99462           13.58382
  66   321  270   20   20          1.99462           13.58382
  67   321  321   20   20          1.99462           13.58382
  68   321  372   20   20          1.99462           13.58382
  69   321  423   20   20          1.99462           13.58382
  70   321  474   20   20          1.99462           13.58382
  71   372   15   20   20          1.99509           13.63587
  72   372   66   20   20          1.99509           13.63587
  73   372  117   20   20          1.99509           13.63587
  74   372  168   20   20          1.99509           13.63587
  75   372  219   20   20          1.99509           13.63587
  76   372  270   20   20          1.99509           13.63587
  77   372  321   20   20          1.99509           13.63587
  78   372  372   20   20          1.99509           13.63587
  79   372  423   20   20          1.99509           13.63587
  80   372  474   20   20          1.99509           13.63587
  81   423   15   20   20          1.99563           13.69575
  82   423   66   20   20          1.99563           13.69575
  83   423  117   20   20          1.99563           13.69575
  84   423  168   20   20          1.99563           13.69575
  85   423  219   20   20          1.99563           13.69575
  86   423  270   20   20          1.99563           13.69575
  87   423  321   20   20          1.99563           13.69575
  88   423  372   20   20          1.99563           13.69575
  89   423  423   20   20          1.99563           13.69575
  90   423  474   20   20          1.99563           13.69575
  91   474   15   20   20          1.99627           13.76591
  92   474   66   20   20          1.99627           13.76591
  93   474  117   20   20          1.99627           13.76591
  94   474  168   20   20          1.99627           13.76591
  95   474  219   20   20          1.99627           13.76591
  96   474  270   20   20          1.99627           13.76591
  97   474  321   20   20          1.99627           13.76591
  98   474  372   20   20          1.99627           13.76591
  99   474  423   20   20          1.99627           13.76591
 100   474  474   20   20          1.99627           13.76591
Reticle           1
Means from averaging values of areas
 SLOPE  =  1.9947002
 OFFSET =  13.593047

STATISTICS FOR UPPER-LEFT  CORNER

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY       DN         DN         DN
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
    0.0000     0.0000     5.1000        0.0000    10.0000    13.5930   -3.5930
   10.0000     9.1323     5.1000       46.5749   110.0000   106.4960    3.5040
   20.0000    19.1323     5.1000       97.5749   210.0000   208.2258    1.7742
   40.0000    39.1323     5.1000      199.5749   410.0000   411.6852   -1.6852
                                                                RMS=    2.7917
SLOPE=   1.994700 SD= 0.000405 OFFSET=  13.593026 SD= 0.046898
NUMBER OF GOOD AREAS=   4 OUT OF  15 AREAS SAMPLED
NUMBER REJECTED FOR      OFFSET       11
GALGEN:C1=   0.50133 ENERGY UNIT/DN C2=  -6.81457 ENERGY UNIT
Reticle           2
Means from averaging values of areas
 SLOPE  =  1.9947002
 OFFSET =  13.593047

STATISTICS FOR UPPER-RIGHT CORNER

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY       DN         DN         DN
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
    0.0000     0.0000     5.1000        0.0000    10.0000    13.5930   -3.5930
   10.0000     9.1323     5.1000       46.5749   110.0000   106.4960    3.5040
   20.0000    19.1323     5.1000       97.5749   210.0000   208.2258    1.7742
   40.0000    39.1323     5.1000      199.5749   410.0000   411.6852   -1.6852
                                                                RMS=    2.7917
SLOPE=   1.994700 SD= 0.000405 OFFSET=  13.593026 SD= 0.046898
NUMBER OF GOOD AREAS=   4 OUT OF  15 AREAS SAMPLED
NUMBER REJECTED FOR      OFFSET       11
GALGEN:C1=   0.50133 ENERGY UNIT/DN C2=  -6.81457 ENERGY UNIT
Reticle           3
Means from averaging values of areas
 SLOPE  =  1.9955057
 OFFSET =  13.681805

STATISTICS FOR LOWER-LEFT  CORNER

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY       DN         DN         DN
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
    0.0000     0.0000     5.1000        0.0000    10.0000    13.6818   -3.6818
   10.0000     9.1116     5.1000       46.4694   110.0000   106.4117    3.5883
   20.0000    19.1116     5.1000       97.4694   210.0000   208.1825    1.8175
   40.0000    39.1116     5.1000      199.4694   410.0000   411.7241   -1.7241
                                                                RMS=    2.8595
SLOPE=   1.995506 SD= 0.000657 OFFSET=  13.681756 SD= 0.073921
NUMBER OF GOOD AREAS=  15 OUT OF  15 AREAS SAMPLED
NUMBER REJECTED FOR      OFFSET        0
GALGEN:C1=   0.50113 ENERGY UNIT/DN C2=  -6.85628 ENERGY UNIT
Reticle           4
Means from averaging values of areas
 SLOPE  =  1.9955057
 OFFSET =  13.681805

STATISTICS FOR LOWER-RIGHT CORNER

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY       DN         DN         DN
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
    0.0000     0.0000     5.1000        0.0000    10.0000    13.6818   -3.6818
   10.0000     9.1116     5.1000       46.4694   110.0000   106.4117    3.5883
   20.0000    19.1116     5.1000       97.4694   210.0000   208.1825    1.8175
   40.0000    39.1116     5.1000      199.4694   410.0000   411.7241   -1.7241
                                                                RMS=    2.8595
SLOPE=   1.995506 SD= 0.000657 OFFSET=  13.681756 SD= 0.073921
NUMBER OF GOOD AREAS=  15 OUT OF  15 AREAS SAMPLED
NUMBER REJECTED FOR      OFFSET        0
GALGEN:C1=   0.50113 ENERGY UNIT/DN C2=  -6.85628 ENERGY UNIT
Reticle           5
Means from averaging values of areas
 SLOPE  =  1.9945154
 OFFSET =  13.572721

STATISTICS FOR CENTER

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY       DN         DN         DN
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
    0.0000     0.0000     5.1000        0.0000    10.0000    13.5727   -3.5727
   10.0000     9.1371     5.1000       46.5991   110.0000   106.5154    3.4846
   20.0000    19.1371     5.1000       97.5991   210.0000   208.2357    1.7643
   40.0000    39.1371     5.1000      199.5991   410.0000   411.6762   -1.6762
                                                                RMS=    2.7762
SLOPE=   1.994515 SD= 0.000537 OFFSET=  13.572694 SD= 0.054375
NUMBER OF GOOD AREAS=  32 OUT OF  40 AREAS SAMPLED
NUMBER REJECTED FOR      OFFSET        8
GALGEN:C1=   0.50137 ENERGY UNIT/DN C2=  -6.80501 ENERGY UNIT
Framewide
Means from averaging values of areas
 SLOPE=  1.9949609
 OFFSET=  13.621794

STATISTICS FOR FULL FRAME

COMMANDED   ACTUAL                                         BEST FIT   RESIDUAL
EXPOSURE   EXPOSURE  ILLUMINATION     ENERGY       DN         DN         DN
 T(MS)      T(MS)       L(RAD)    (ENERGY UNIT)
    0.0000     0.0000     5.1000        0.0000    10.0000    13.6217   -3.6217
   10.0000     9.1256     5.1000       46.5408   110.0000   106.4687    3.5313
   20.0000    19.1256     5.1000       97.5408   210.0000   208.2118    1.7882
   40.0000    39.1256     5.1000      199.5408   410.0000   411.6978   -1.6978
                                                                RMS=    2.8137
SLOPE=   1.994961 SD= 0.000749 OFFSET=  13.621734 SD= 0.081753
NUMBER OF GOOD AREAS=  32 OUT OF  40 AREAS SAMPLED
NUMBER REJECTED FOR      OFFSET        8
GALGEN:C1=   0.50126 ENERGY UNIT/DN C2=  -6.82807 ENERGY UNIT
DELTA CORRECTION=  -6.82807 ENERGY UNIT


-----------------------------------------
Processing repeated with DELTA correction
-----------------------------------------

GLOBAL VALUE FOR SLOPE...
Raw mean and sigma are...
N= 100 MEAN=     1.99551 SIGMA=     0.00164
After throwing out samples differing by 1 sigma
N=  70 MEAN=     1.99496 SIGMA=     0.00075

GLOBAL VALUE FOR OFFSET...
Raw mean and sigma are...
N= 100 MEAN=     0.05755 SIGMA=     0.16870
After throwing out samples differing by 1 sigma
N=  70 MEAN=     0.00006 SIGMA=     0.07669

Summary of bad areas....
AREA   1 (SL,SS,NL,NS)=(  15,  15,  20,  20)  ****BOTH BAD FIT*********
AREA   2 (SL,SS,NL,NS)=(  15,  66,  20,  20)  ****BOTH BAD FIT*********
AREA   3 (SL,SS,NL,NS)=(  15, 117,  20,  20)  ****BOTH BAD FIT*********
AREA   4 (SL,SS,NL,NS)=(  15, 168,  20,  20)  ****BOTH BAD FIT*********
AREA   5 (SL,SS,NL,NS)=(  15, 219,  20,  20)  ****BOTH BAD FIT*********
AREA   6 (SL,SS,NL,NS)=(  15, 270,  20,  20)  ****BOTH BAD FIT*********
AREA   7 (SL,SS,NL,NS)=(  15, 321,  20,  20)  ****BOTH BAD FIT*********
AREA   8 (SL,SS,NL,NS)=(  15, 372,  20,  20)  ****BOTH BAD FIT*********
AREA   9 (SL,SS,NL,NS)=(  15, 423,  20,  20)  ****BOTH BAD FIT*********
AREA  10 (SL,SS,NL,NS)=(  15, 474,  20,  20)  ****BOTH BAD FIT*********
AREA  11 (SL,SS,NL,NS)=(  66,  15,  20,  20)  ****BOTH BAD FIT*********
AREA  12 (SL,SS,NL,NS)=(  66,  66,  20,  20)  ****BOTH BAD FIT*********
AREA  13 (SL,SS,NL,NS)=(  66, 117,  20,  20)  ****BOTH BAD FIT*********
AREA  14 (SL,SS,NL,NS)=(  66, 168,  20,  20)  ****BOTH BAD FIT*********
AREA  15 (SL,SS,NL,NS)=(  66, 219,  20,  20)  ****BOTH BAD FIT*********
AREA  16 (SL,SS,NL,NS)=(  66, 270,  20,  20)  ****BOTH BAD FIT*********
AREA  17 (SL,SS,NL,NS)=(  66, 321,  20,  20)  ****BOTH BAD FIT*********
AREA  18 (SL,SS,NL,NS)=(  66, 372,  20,  20)  ****BOTH BAD FIT*********
AREA  19 (SL,SS,NL,NS)=(  66, 423,  20,  20)  ****BOTH BAD FIT*********
AREA  20 (SL,SS,NL,NS)=(  66, 474,  20,  20)  ****BOTH BAD FIT*********
AREA  31 (SL,SS,NL,NS)=( 168,  15,  20,  20)  ****BOTH BAD FIT*********
AREA  32 (SL,SS,NL,NS)=( 168,  66,  20,  20)  ****BOTH BAD FIT*********
AREA  33 (SL,SS,NL,NS)=( 168, 117,  20,  20)  ****BOTH BAD FIT*********
AREA  34 (SL,SS,NL,NS)=( 168, 168,  20,  20)  ****BOTH BAD FIT*********
AREA  35 (SL,SS,NL,NS)=( 168, 219,  20,  20)  ****BOTH BAD FIT*********
AREA  36 (SL,SS,NL,NS)=( 168, 270,  20,  20)  ****BOTH BAD FIT*********
AREA  37 (SL,SS,NL,NS)=( 168, 321,  20,  20)  ****BOTH BAD FIT*********
AREA  38 (SL,SS,NL,NS)=( 168, 372,  20,  20)  ****BOTH BAD FIT*********
AREA  39 (SL,SS,NL,NS)=( 168, 423,  20,  20)  ****BOTH BAD FIT*********
AREA  40 (SL,SS,NL,NS)=( 168, 474,  20,  20)  ****BOTH BAD FIT*********


Slopes and offsets for each area...

ENERGY UNIT = PICOAMP-MILLISECONDS

AREA    SL   SS   NL   NS  SLOPE (DN/ENERGY UNIT)  OFFSET (DN)
   1    15   15   20   20          1.99939            0.45985
   2    15   66   20   20          1.99939            0.45985
   3    15  117   20   20          1.99939            0.45985
   4    15  168   20   20          1.99939            0.45985
   5    15  219   20   20          1.99939            0.45985
   6    15  270   20   20          1.99939            0.45985
   7    15  321   20   20          1.99939            0.45985
   8    15  372   20   20          1.99939            0.45985
   9    15  423   20   20          1.99939            0.45985
  10    15  474   20   20          1.99939            0.45985
  11    66   15   20   20          1.99717            0.22860
  12    66   66   20   20          1.99717            0.22860
  13    66  117   20   20          1.99717            0.22860
  14    66  168   20   20          1.99717            0.22860
  15    66  219   20   20          1.99717            0.22860
  16    66  270   20   20          1.99717            0.22860
  17    66  321   20   20          1.99717            0.22860
  18    66  372   20   20          1.99717            0.22860
  19    66  423   20   20          1.99717            0.22860
  20    66  474   20   20          1.99717            0.22860
  21   117   15   20   20          1.99495           -0.00151
  22   117   66   20   20          1.99495           -0.00151
  23   117  117   20   20          1.99495           -0.00151
  24   117  168   20   20          1.99495           -0.00151
  25   117  219   20   20          1.99495           -0.00151
  26   117  270   20   20          1.99495           -0.00151
  27   117  321   20   20          1.99495           -0.00151
  28   117  372   20   20          1.99495           -0.00151
  29   117  423   20   20          1.99495           -0.00151
  30   117  474   20   20          1.99495           -0.00151
  31   168   15   20   20          1.99386           -0.11336
  32   168   66   20   20          1.99386           -0.11336
  33   168  117   20   20          1.99386           -0.11336
  34   168  168   20   20          1.99386           -0.11336
  35   168  219   20   20          1.99386           -0.11336
  36   168  270   20   20          1.99386           -0.11336
  37   168  321   20   20          1.99386           -0.11336
  38   168  372   20   20          1.99386           -0.11336
  39   168  423   20   20          1.99386           -0.11336
  40   168  474   20   20          1.99386           -0.11336
  41   219   15   20   20          1.99396           -0.10308
  42   219   66   20   20          1.99396           -0.10308
  43   219  117   20   20          1.99396           -0.10308
  44   219  168   20   20          1.99396           -0.10308
  45   219  219   20   20          1.99396           -0.10308
  46   219  270   20   20          1.99396           -0.10308
  47   219  321   20   20          1.99396           -0.10308
  48   219  372   20   20          1.99396           -0.10308
  49   219  423   20   20          1.99396           -0.10308
  50   219  474   20   20          1.99396           -0.10308
  51   270   15   20   20          1.99421           -0.07736
  52   270   66   20   20          1.99421           -0.07736
  53   270  117   20   20          1.99421           -0.07736
  54   270  168   20   20          1.99421           -0.07736
  55   270  219   20   20          1.99421           -0.07736
  56   270  270   20   20          1.99421           -0.07736
  57   270  321   20   20          1.99421           -0.07736
  58   270  372   20   20          1.99421           -0.07736
  59   270  423   20   20          1.99421           -0.07736
  60   270  474   20   20          1.99421           -0.07736
  61   321   15   20   20          1.99462           -0.03556
  62   321   66   20   20          1.99462           -0.03556
  63   321  117   20   20          1.99462           -0.03556
  64   321  168   20   20          1.99462           -0.03556
  65   321  219   20   20          1.99462           -0.03556
  66   321  270   20   20          1.99462           -0.03556
  67   321  321   20   20          1.99462           -0.03556
  68   321  372   20   20          1.99462           -0.03556
  69   321  423   20   20          1.99462           -0.03556
  70   321  474   20   20          1.99462           -0.03556
  71   372   15   20   20          1.99509            0.01326
  72   372   66   20   20          1.99509            0.01326
  73   372  117   20   20          1.99509            0.01326
  74   372  168   20   20          1.99509            0.01326
  75   372  219   20   20          1.99509            0.01326
  76   372  270   20   20          1.99509            0.01326
  77   372  321   20   20          1.99509            0.01326
  78   372  372   20   20          1.99509            0.01326
  79   372  423   20   20          1.99509            0.01326
  80   372  474   20   20          1.99509            0.01326
  81   423   15   20   20          1.99563            0.06943
  82   423   66   20   20          1.99563            0.06943
  83   423  117   20   20          1.99563            0.06943
  84   423  168   20   20          1.99563            0.06943
  85   423  219   20   20          1.99563            0.06943
  86   423  270   20   20          1.99563            0.06943
  87   423  321   20   20          1.99563            0.06943
  88   423  372   20   20          1.99563            0.06943
  89   423  423   20   20          1.99563            0.06943
  90   423  474   20   20          1.99563            0.06943
  91   474   15   20   20          1.99627            0.13525
  92   474   66   20   20          1.99627            0.13525
  93   474  117   20   20          1.99627            0.13525
  94   474  168   20   20          1.99627            0.13525
  95   474  219   20   20          1.99627            0.13525
  96   474  270   20   20          1.99627            0.13525
  97   474  321   20   20          1.99627            0.13525
  98   474  372   20   20          1.99627            0.13525
  99   474  423   20   20          1.99627            0.13525
 100   474  474   20   20          1.99627            0.13525
??E - splot: Error opening/writing gnuplot file
IOSTAT=       1141
***CCDSLOPE task cancelled
 ** ABEND called **
goto rm
if (mode = "nobatch" or mode = "inter")
end-if
if (mode = "nobatch" or mode = "inter")
end-if
ush rm cas
ush rm l.list
end-proc
$ Return
$!#############################################################################
