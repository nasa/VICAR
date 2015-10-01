$!****************************************************************************
$!
$! Build proc for MIPL module maplabprog
$! VPACK Version 1.9, Monday, June 12, 2000, 14:24:24
$!
$! Execute by entering:		$ @maplabprog
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
$ write sys$output "*** module maplabprog ***"
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
$ write sys$output "Invalid argument given to maplabprog.com file -- ", primary
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
$   if F$SEARCH("maplabprog.imake") .nes. ""
$   then
$      vimake maplabprog
$      purge maplabprog.bld
$   else
$      if F$SEARCH("maplabprog.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake maplabprog
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @maplabprog.bld "STD"
$   else
$      @maplabprog.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create maplabprog.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack maplabprog.com -
	-s maplabprog.f common.fin -
	-p maplabprog.pdf -
	-i maplabprog.imake -
	-t tstmaplabprog.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create maplabprog.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      include 'VICMAIN_FOR'
      SUBROUTINE MAIN44  ! maplabprog
      IMPLICIT INTEGER(A-Z)

      include 'common.fin'

      common /error/ mp,err 	! for mp 
      integer*4 err
      real*8 mp

c      data FORMAT/'BYTE'/
      DATA INCR/1/,SWTCH/1/

!  start code 
      call xvmessage('*** program MAPLABPROG, version 29-Mar-2000 ***',
     & 0)

      CALL INIT_SPICE

      err = 0                   ! mp errors
      call mp_init( mp, status )
      if ( status .ne. 0 ) then 
          call xvmessage(' MP error on init',0) 
          call xvmessage(' Program terminated',0)
          return
      end if

      CALL MAPA
      if ( err .ne. 0 ) then
          call xvmessage(' MP error on read',0) 
          call xvmessage(' Program terminated',0)
          return
      end if

      err = 0                   ! mp errors
      CALL MAPB
      if ( err .ne. 0 ) then
          call xvmessage(' MP error on label-write',0)
      end if

      RETURN
      END


C**********************************************************


      SUBROUTINE MAPA

      include 'common.fin'

      INTEGER*4 DEF,COUNT
      character*80 fnam
      REAL*4 PBBUF(20)

      REAL*8 FIC,LO,SO,PIXPMM,PHI,BL,LSSP,SSSP,THT,VABS,om(9),rs(3)
      LOGICAL*4 LABFLAG/.FALSE./,XVPTST

      common /error/ mp,err 	! for mp 
      integer*4 err
      real*8 mp
      integer*4 status

      CHARACTER*12 NAME/'        '/

      DATA ISTER/0/,IPOLE/0/,IORTH/0/,P11ST/0/,
     *  P12ST/0/,P21ST/0/,P22ST/0/,LO/500./,SO/500./,IN/0/,
     *   THT/0./,VABS/0./,FIC/1500./,PIXPMM/84.821/,BL/0./,PHI/0./,
     *   ILO/0/,ISO/0/

      nl = 0           !Initialize
      ns = 0
      do i=1,40
	rdata(i) = -9999.0	! to check completeness of project'n spec
      enddo

C  -lwk- moved these before open of input, as user should not override
c  input format
      IF(XVPTST('HALF'))THEN
           FORMAT='HALF'
           PAR(9)=2
      ENDIF
      IF(XVPTST('BYTE'))THEN
         FORMAT='BYTE'
         PAR(9)=1
      ENDIF

      CALL XVPARM('INP',fnam,count,def,0)

      PAR(7) = COUNT

      IF(PAR(7).GT.0)THEN

         input = 1
c        open input and get label data and put in rdata for output file
c        we'll update it using user parameters

         CALL XVUNIT(UNIT2,'INP',1,ind,' ')
         CALL CHKSTAT(IND,' ERR IN XVUNIT,IND=',1,ind,1)
         CALL XVOPEN(UNIT2,IND,'U_FORMAT','HALF', ' ')
         CALL CHKSTAT(IND,' ERR IN XVOPEN,IND=',1,ind,1)
         CALL XVGET(UNIT2,IND,'FORMAT',FORMAT,'PIX_SIZE',PAR(9), ' ')
         CALL CHKSTAT(IND,' ERR IN XVGET,IND=',1,ind,1)


!	for porting to UNIX - BAM 10/95 
!		remove call to searcv2 and insert mp routines
!               as follows
!
!         CALL SEARCV2(UNIT2,NLR2,WORK,IDATA,RDATA)

         call mp_label_read(mp,unit2,status)
         if ( status .eq. 0 ) then

             call mp_mpo2buf(mp,rdata,status)
             if ( status .ne. 0 ) then 
    	         err = -1
 	         return
             end if

	     do l = 1, 40		! move into integer buffer
	         idata(l) = rdata(l)
	     end do

         end if
! 		end searcv2 code

         IF(IDATA(39).EQ.5)P11ST=1
         CALL XVSIZE(PAR(1),PAR(2),PAR(3),PAR(4),PAR(5),PAR(6),' ')

c        labflag indicates whether a map2 label was found

         IF(IDATA(39).NE.7.AND.IDATA(39).NE.8)LABLFAG=.TRUE.
      ENDIF


      CALL XVPARM('NL',IVAL,count,def,0)
      IF (COUNT.GT.0) then
	PAR(3)=IVAL(1)
        if (ival(1).le.0) call mabend(' Invalid NL value.')
	nl = ival(1)
      end if

      CALL XVPARM('NS',IVAL,count,def,0)
      IF (COUNT.GT.0) then
	PAR(4)=IVAL(1)
        if (ival(1).le.0) call mabend(' Invalid NS value.')
	ns = ival(1)
      end if

c  note that the usual SIZE parameter is supported, but SL and SS are ignored
      CALL XVPARM('SIZE',IVAL,count,def,0)
      if (count.eq.4) then	! only count=0 or 4 allowed, see .PDF
	nl = ival(3)
	ns = ival(4)
	PAR(3) = nl
	PAR(4) = ns
        if (nl.le.0 .or. ns.le.0) call mabend(' Invalid SIZE value.')
      endif

      RDATA(33)=PAR(3)/2
      RDATA(34)=PAR(4)/2
      RDATA(28)=PAR(5)/2
      RDATA(29)=PAR(6)/2

C      IF(NUMPAR.EQ.10)GO TO 1000
C      M=-2
C      J=7
C  80  J=J+1
C      M=M+1
C  90  j=j+1
C      m=m+1
C 100  j=j+2
C      m=m+1
C      IF(J.GT.NUMPAR)GO TO 1000
c      CALL KEYGO(4,PAR(J),
C     1'HALF',&110,'REQU',&120,'RPOL',&130,'RADI',&140,'MERC',&150,
C     *'LAMB',&160,'STER',&170,'ORTH',&180,'POLE',&190,'SCAL',&200,
C     *'LINE',&210,'SAMP',&220,'LATI',&230,'LONG',&240,'PAR1',&250,
C     *'PAR2',&260,'LIN1',&270,'LIN2',&280,'NORT',&290,'INCR',&300,
C     6'INC ',&300,'VENU',&310,'CYLI',&330,'JUPI',&340,'IO  ',&350,
C     *'EURO',&360,'GANY',&370,'CALL',&380,'MCRY',&390,'MARS',&400,
C     *'MOON',&410,            'EART',&430,'RECT',&440,'LATL',&440,
C     *'OS  ',&450,'SSCP',&460,'NORA',&470,'RMAG',&480,'FOCL',&490,
C     *'FARE',&500,'PSCA',&510,'SLAT',&520,'SLON',&530,'OBJE',&540,
C     3'LAXI',&550,'SAXI',&560,'BYTE',&570,'TARG',&580)
C      CALL PRNT(4,1,M,'0***ABEND,ERROR IN  USERS PARAMETER.')
C      CALL PARERR(PAR(J))
C      CALL ABEND

      CALL XVPARM('REQUATOR',RVAL,count,def,0)
      IF(COUNT.GT.0)RDATA(26)=RVAL(1)

      CALL XVPARM('RPOLE',RVAL,count,def,0)
      IF(COUNT.GT.0)RDATA(25)=RVAL(1)

      CALL XVPARM('RADIUS',RVAL,count,def,0)
      IF(COUNT.GT.0)THEN
         RDATA(26)=RVAL(1)
         RDATA(25)=RVAL(1)
      ENDIF

      IF(XVPTST('STEREOGR'))ISTER=1
      IF(XVPTST('ORTHOGR'))IORTH=1
      IF(XVPTST('POLE'))   IPOLE=1
      IF(XVPTST('LAMBERT'))IDATA(39)=5
      IF(XVPTST('MERCATOR'))IDATA(39)=6
      IF(XVPTST('CYLINDR'))IDATA(39)=9   ! cylindrical projection
      IF(XVPTST('RECTANG'))IDATA(39)=10  ! latlon simple cylindrical projection
      IF(XVPTST('LATLON'))IDATA(39)=10
      IF(XVPTST('OBCYLIND'))IDATA(39)=11
      IF(XVPTST('SINUSOID'))IDATA(39)=12
      IF(XVPTST('OBSINUS'))IDATA(39)=13
      IF(XVPTST('MOLLWEID'))IDATA(39)=14
      IF(XVPTST('TMERCATO'))IDATA(39)=15
      IF(XVPTST('OS'))IDATA(39)=16        ! object space
      IF(XVPTST('OBJECT'))IDATA(39)=16    ! object space
      IF(XVPTST('PERSPECT'))IDATA(39)=16 
      IF(ISTER.EQ.1.AND.IPOLE.EQ.0)IDATA(39)=4
      IF(ISTER.EQ.1.AND.IPOLE.EQ.1)IDATA(39)=3
      IF(IORTH.EQ.1.AND.IPOLE.EQ.0)IDATA(39)=2
      IF(IORTH.EQ.1.AND.IPOLE.EQ.1)IDATA(39)=1

      CALL XVPARM('SCALE',RVAL,count,def,0)
      IF(COUNT.GT.0)RDATA(7)=RVAL(1)

      CALL XVPARM('LINE',RVAL,count,def,0)
      IF(COUNT.GT.0)RDATA(2)=RVAL(1)

      CALL XVPARM('SAMPLE',RVAL,count,def,0)
      IF(COUNT.GT.0)THEN
         RDATA(1)=RVAL(1)
         ISAMP=1
      ENDIF

      CALL XVPARM('LATITUDE',RVAL,count,def,0)
      IF(COUNT.GT.0)RDATA(3)=RVAL(1)

      CALL XVPARM('LONGITUD',RVAL,count,def,0)
      IF(COUNT.GT.0)THEN
         RDATA(6)=RVAL(1)
         IF(RDATA(6).LT.0.)RDATA(6)=RDATA(6)+360.
         ILONG=1
      ENDIF

      CALL XVPARM('PAR1',RVAL,count,def,0)
      IF(COUNT.GT.0)THEN
         RDATA(4)=RVAL(1)
         P11ST=1
      ENDIF
      ! special fix for bug in mpbuf2mpo:
      if (idata(39).eq.10) then
	rdata(4) = 0.
      endif

      CALL XVPARM('PAR2',RVAL,count,def,0)
      IF(COUNT.GT.0)THEN
         RDATA(5)=RVAL(1)
         P21ST=1
      ENDIF

c  not used:  (lwk)
c      CALL XVPARM('LIN1',RVAL,count,def,0)
c      IF(COUNT.GT.0)RLIN11=RVAL(1)
c      CALL XVPARM('LIN2',RVAL,count,def,0)
c      IF(COUNT.GT.0)RLIN22=RVAL(1)

      CALL XVPARM('NORTH',RVAL,count,def,0)
      IF(COUNT.GT.0)RDATA(9)=RVAL(1)

      CALL XVPARM('INCR',ival, count,def,0)
      IF(COUNT.GT.0)INCR=IVAL(1)
c     if ( ival(1).le. 0 ) then   ! fr 88287 - fixed bam 1/96
      if (incr.le.0) call mabend( ' Invalid INCR value.')

      CALL XVPARM('TARGET',CVAL,count,def,0)
      IF(COUNT.GT.0)NAME=CVAL

      CALL XVPARM('SSCPT',RVAL,count,def,0)        ! subspacecraft point
      IF(COUNT.GT.0)THEN
         RDATA(33)=RVAL(1)
         RDATA(34)=RVAL(2)
      ENDIF

      CALL XVPARM('NORANGLE',RVAL,count,def,0)
      IF(COUNT.GT.0)RDATA(35)=RVAL(1)

      CALL XVPARM('RMAG',RVAL,count,def,0)
      IF(COUNT.GT.0)RDATA(38)=RVAL(1)

      CALL XVPARM('FOCL',RVAL,count,def,0)
      IF(COUNT.GT.0)RDATA(27)=RVAL(1)

      CALL XVPARM('PSCALE',RVAL,count,def,0)
      IF(COUNT.GT.0)RDATA(30)=RVAL(1)      ! camera scale pixels/mm

      CALL XVPARM('SLATITUD',RVAL,count,def,0)
      IF(COUNT.GT.0)RDATA(31)=RVAL(1)      !sub spacecraft latitude

      CALL XVPARM('SLONGITU',RVAL,count,def,0)
      IF(COUNT.GT.0)RDATA(32)=RVAL(1)      ! subspacecraft longitude

      CALL XVPARM('LAXIS',RVAL,count,def,0)
      IF(COUNT.GT.0)THEN
         RDATA(28)=RVAL(1)               ! line of optical axis
         ILO=1
      ELSEIF (IDATA(39).EQ.16) THEN
	RDATA(28) = NL/2
      ENDIF

      CALL XVPARM('SAXIS',RVAL,count,def,0)
      IF(COUNT.GT.0)THEN
         RDATA(29)=RVAL(1)                ! sample of optical axis
         ISO=1
      ELSEIF (IDATA(39).EQ.16) THEN
	RDATA(29) = NS/2
      ENDIF

      IF(XVPTST('MCRY'))    NAME='MERCURY'
      IF(XVPTST('MERCURY')) NAME='MERCURY'
      IF(XVPTST('VENUS'))   NAME='VENUS'
      IF(XVPTST('EARTH'))   NAME='EARTH'
      IF(XVPTST('MOON'))    NAME='MOON'
      IF(XVPTST('MARS'))    NAME='MARS'
      IF(XVPTST('PHOBOS'))  NAME='PHOBOS'
      IF(XVPTST('DEIMOS'))  NAME='DEIMOS'

      IF(XVPTST('JUPITER')) NAME='JUPITER'
      IF(XVPTST('IO'))      NAME='IO'
      IF(XVPTST('EUROPA'))  NAME='EUROPA'
      IF(XVPTST('GANYMEDE'))NAME='GANYMEDE'
      IF(XVPTST('CALLISTO'))NAME='CALLISTO'
      IF(XVPTST('AMALTHEA'))NAME='AMALTHEA'

      IF(XVPTST('SATURN'))  NAME='SATURN'
      IF(XVPTST('MIMAS'))   NAME='MIMAS'
      IF(XVPTST('ENCELADU'))NAME='ENCELADU'
      IF(XVPTST('THETHYS')) NAME='THETHYS'
      IF(XVPTST('DIONE'))   NAME='DIONE'
      IF(XVPTST('RHEA'))    NAME='RHEA'
      IF(XVPTST('DIONE'))   NAME='DIONE'
      IF(XVPTST('TITAN'))   NAME='TITAN'
      IF(XVPTST('HYPERION'))NAME='HYPERION'
      IF(XVPTST('IAPETUS')) NAME='IAPETUS'
      IF(XVPTST('PHOEBE'))  NAME='PHOEBE'

      IF(XVPTST('URANUS'))  NAME='URANUS'
      IF(XVPTST('MIRANDA')) NAME='MIRANDA'
      IF(XVPTST('ARIEL'))   NAME='ARIEL'
      IF(XVPTST('UMBRIEL')) NAME='UMBRIEL'
      IF(XVPTST('TITANIA')) NAME='TITANIA'
      IF(XVPTST('OBERON'))  NAME='OBERON'

      IF(XVPTST('NEPTUNE')) NAME='NEPTUNE'
      IF(XVPTST('TRITON'))  NAME='TRITON'
      IF(XVPTST('NEREID'))  NAME='NEREID'

      IF(XVPTST('PLUTO'))   NAME='PLUTO'
      IF(XVPTST('CHARON'))  NAME='CHARON'

      IF(NAME.NE.'            ')THEN
         CALL xvmessage('TARGET='//NAME,0)
         CALL PBDATA(NAME,PBBUF,*1200)
         RDATA(26)=PBBUF(1)
         RDATA(25)=PBBUF(3)
      ENDIF
      
      IF(IDATA(39).EQ.9.AND.ILONG.EQ.1.AND.ISAMP.EQ.1)THEN
          CALL xvmessage(' RECALCULATING LONGITUDE, SAMPLE',0)
          CALL CYLPATCH(RDATA)
          CALL PRNT(7,1,RDATA(6),' at sample=1,long=.')
          CALL PRNT(7,1,RDATA(1),' long 0 at sample=.')
      ENDIF

      RDATA(8)=1.
      IF((IDATA(39).EQ.1.OR.IDATA(39).EQ.3).AND.RDATA(3).LT.0.0)
     1      RDATA(8)=-1.0
      CALL PRNT(7,2,RDATA(25),'RADII=.')
      CALL PRNT(4,1,IDATA(39),'OUTPUT PIX PROJECTION TYPE=.')

      IF(IDATA(39).LE.0)CALL MABEND(' ERROR IN PROJECTION TYPE')
      IF (IDATA(39).EQ.7) CALL mabend('IMAGE SPACE IS NOT SUPPORTED')
      IF(IDATA(39).EQ.5)THEN
         IF(P11ST.NE.1.OR.P21ST.NE.1) CALL mabend(
     1			 'STANDARD OUTPUT PARALLELS NOT SET')
      ENDIF
      IF(RDATA(25).EQ.0..OR.RDATA(26).EQ.0.) call mabend(
     & '***RADII NOT SPECIFIED',0)

      IF (IDATA(39).EQ.8 .OR. IDATA(39).EQ.16) THEN
	! Perspective, calculate ommatrix and rsvector
	CALL PRNT(7,12,RDATA(27),' DATA(27-38)=.')
	FIC=RDATA(27)		! focal length
	if (fic.le.0.0) call mabend(
     1		 ' FOCL required for Perspective projection')
        LO=RDATA(28)
        SO=RDATA(29)
	if (lo.le.-9999. .or. so.le.-9999.) call mabend(
     1		 ' LAXIS,SAXIS required for Perspective projection')
        PIXPMM=RDATA(30)	! camera scale
	if (pixpmm.le.0.0) call mabend(
     1		 ' PSCAL required for Perspective projection')
        PHI=RDATA(31)		! s/c latitude
        BL=RDATA(32)		! s/c longitude
	if (phi.le.-9999. .or. bl.le.-9999.) call mabend(
     1		 ' LATI,LONG required for Perspective projection')
        LSSP=RDATA(33)
        SSSP=RDATA(34)
	if (lssp.le.-9999. .or. sssp.le.-9999.) call mabend(
     1		 ' SSCPT required for Perspective projection')
        THT=RDATA(35)		! North angle
        VABS=RDATA(38)		! range
	if (vabs.le.0.0) call mabend(
     1		 ' RMAG required for Perspective projection')
        CALL MOMATI(LO,SO,LSSP,SSSP,PIXPMM,FIC,BL,PHI,THT,VABS,OM,RS)
        CALL PRNT(8,9,OM ,' OMMATRIX =.')
        CALL PRNT(8,3,RS ,' RSVECTOR =.')
      ELSEIF (IDATA(39).NE.8) THEN
	CALL PRNT(7,10,RDATA,' RDATA=.')
      ENDIF

      RETURN

1200  CALL xvmessage(' PBDATA ERROR',0)
      CALL ABEND
      END


C**********************************************************


      SUBROUTINE MAPB
      IMPLICIT INTEGER(A-Z)

      include 'common.fin'

      REAL CIRCUM
      INTEGER NPAR(5000)
      INTEGER*2 BUF(30000)
      CHARACTER*32 MSG

      EQUIVALENCE (NPAR,BUF)

      common /error/ mp,err 	! for mp 
      integer*4 err
      real*8 mp
      integer*4 status


      CALL XVSIZE(SL,SS,NL,NS,NLI,NSI,' ')
      IF (NL.LE.0 .OR. NS.LE.0) CALL MABEND(
     & 'MUST SPECIFY OUTPUT SIZE IF NO INPUT IMAGE')

      if (incr.gt.nl/2 .or. incr.gt.ns/2) 
     &  call mabend( ' Invalid INCR value.')

      IF (IDATA(39).NE.7 .AND. IDATA(39).NE.8 .AND.
     & IDATA(39).NE.16) THEN
         IF(RDATA(7).EQ.0.)CALL MABEND(' SCALE NOT SPECIFIED')
         CIRCUM=2.*3.14159*RDATA(25)/RDATA(7)
         MSG=' CIRCUMFERENCE=           PIXELS'
         WRITE (MSG(16:25),'(F10.1)') CIRCUM
         CALL xvmessage(MSG,0)
      ENDIF

      CALL XVUNIT(UNIT1,'OUT',1,ind,' ')
      CALL CHKSTAT(IND,' ERR IN XVUNIT,IND=',1,ind,1)
      CALL XVOPEN(UNIT1,ind,'OP','WRITE','U_NL',NL/INCR,'U_NS',NS/INCR,
     *   'U_FORMAT','HALF','O_FORMAT',FORMAT, ' ')
      CALL CHKSTAT(IND,' ERR IN XVOPEN,IND=',1,ind,1)

c     update map2 labels
      IF(IDATA(39).EQ.10) CALL RECTPATCH(IDATA)

! 	Ported to UNIX - eliminated maplabv2
!		inserted mp routines    bam 10/95

!      CALL MAPLABV2(IDATA,IDATA,UNIT1,NLR1,NPAR)


      call mp_buf2mpo( rdata, mp, status )
      if ( status .ne. 0 ) then 
	  err = -1
          return
      end if

      call mp_label_write( mp, unit1, 'HISTORY', status )
      if ( status .ne. 0 ) then 
	  err = -1
          return
      end if

      call mp_label_write( mp, unit1, 'PROPERTY', status )
      if ( status .ne. 0 ) then 
	  err = -1
          return
      end if

c     copy file
      OREC=0
      EL=SL+NL-1
      IF(EL.GT.PAR(3)) EL=PAR(3)+SL-1
      CALL ZIA(BUF,30000/4)
      I5=(NL+4)/5
      IF(I5.EQ.0)I5=1
      IF(FORMAT.EQ.'BYTE')SWTCH=1
      IF(FORMAT.EQ.'HALF')SWTCH=2
      IF(FORMAT.EQ.'WORD')SWTCH=2

      if (ns*swtch.gt.60000) call mabend(' NS too large for buffer!')

      DO I=SL,EL,INCR
        IF(PAR(7).GT.0)THEN
C           CALL READ(IND,2,NLR2+I,SWTCH,SB-1,NBI,BUF,0)
C           CALL JJLCHK(IND,2,I,0)
           CALL XVREAD(UNIT2,BUF,IND,'LINE',I,'SAMP',SS,'NSAMPS',NS,' ')
           CALL CHKSTAT(IND,' ERR IN XVREAD,IND=',1,ind,1)
           IF(INCR.NE.1) CALL MVE(SWTCH,NBI,BUF,BUF,INCR,1)
        ENDIF

        OREC=OREC+1
C        CALL WRITE(IND,1,NLR1+OREC,SWTCH,0,NBO,BUF,0)
C        CALL JJLCHK(IND,1,I,1)
        CALL XVWRIT(UNIT1,BUF,IND,'LINE',OREC,'NSAMPS',NS/INCR,' ')
        CALL CHKSTAT(IND,' ERR IN XVWRIT,IND=',1,ind,1)
        IF(MOD(OREC,I5).EQ.0)CALL PRNT(4,1,OREC,' RECORD WRITTEN=.')
      ENDDO

C     CLOSE FILES
      IF(PAR(7).GT.0)CALL XVCLOSE(UNIT2,IND, ' ')

      CALL XVCLOSE(UNIT1,ind,' ')

      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create common.fin
$ DECK/DOLLARS="$ VOKAGLEVE"
      COMMON/C2/PAR,RDATA,INCR,SWTCH,UNIT1,UNIT2,FORMAT

      INTEGER*4 SWTCH,UNIT1,UNIT2

      INTEGER*4 NUMPAR
      INTEGER*4 PAR(100)
      REAL*4 FAR(100)

      INTEGER*4 IDATA(40)
      REAL*4 RDATA(40)

      REAL*4 RVAL(10)
      INTEGER*4 IVAL(10)

      CHARACTER*12 CVAL
      CHARACTER*32 FORMAT

      EQUIVALENCE(FAR(1),PAR(1)),(NUMPAR,PAR(10)),(IDATA(1),RDATA(1)),
     *  (IVAL,RVAL,CVAL)

      INTEGER*4 SL,SB,NL,NBI
      EQUIVALENCE (SL,PAR(1)),(SB,PAR(2)),(NL,PAR(3)),(NBI,PAR(4))
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create maplabprog.pdf
process help=*
PARM INP      TYPE=STRING  COUNT=0:1   DEFAULT=--
PARM OUT      TYPE=STRING  COUNT=1 
PARM SIZE     TYPE=INTEGER COUNT=(0,4) DEFAULT=--
PARM NL       TYPE=INTEGER COUNT=0:1   valid=1:1000000 DEFAULT=--
PARM NS       TYPE=INTEGER COUNT=0:1   valid=1:60000 DEFAULT=--
PARM FORMAT   KEYWORD      COUNT=0:1   VALID=(BYTE,HALF) DEFAULT=BYTE
PARM INCR     TYPE=INTEGER COUNT=(0:1) valid=1:30000 DEFAULT=--
PARM TARGET   TYPE=KEYWORD COUNT=(0:1)   DEFAULT=-- +
 VALID=(MERCURY,MCRY,VENUS,EARTH,MOON,MARS,PHOBOS,DEIMOS,+
 JUPITER,IO,EUROPA,GANYMEDE,CALLISTO,AMALTHEA,+
 SATURN,MIMAS,ENCELADU,THETHYS,DIONE,RHEA,TITAN,HYPERION,IAPETUS,PHOEBE,+
 URANUS,MIRANDA,ARIEL,UMBRIEL,TITANIA,OBERON,+
 NEPTUNE,TRITON,NEREID,PLUTO,CHARON)
PARM PROJECTN TYPE=KEYWORD COUNT=1 VALID=(CYLINDR,OBCYLIND,MERCATOR,TMERCATO,+
 STEREOGR,ORTHOGR,LAMBERT,LATLON,RECTANG,SINUSOID,OBSINUS,MOLLWEID,PERSPECT,+
 OBJECT,OS)
PARM POLE     TYPE=KEYWORD COUNT=(0:1) DEFAULT=-- VALID=POLE
PARM NORTH    TYPE=REAL    COUNT=(0:1) DEFAULT=-- 
PARM PAR1     TYPE=REAL    COUNT=(0:1) DEFAULT=-- 
PARM PAR2     TYPE=REAL    COUNT=(0:1) DEFAULT=-- 
PARM SAXIS    TYPE=REAL    COUNT=(0:1) DEFAULT=-- 
PARM LAXIS    TYPE=REAL    COUNT=(0:1) DEFAULT=-- 
PARM SLATITUD TYPE=REAL    COUNT=(0:1) DEFAULT=-- 
PARM SLONGITU TYPE=REAL    COUNT=(0:1) DEFAULT=-- 
PARM PSCALE   TYPE=REAL    COUNT=(0:1) DEFAULT=-- 
PARM FOCL     TYPE=REAL    COUNT=(0:1) DEFAULT=-- 
PARM RMAG     TYPE=REAL    COUNT=(0:1) DEFAULT=-- 
PARM NORANGLE TYPE=REAL    COUNT=(0:1) DEFAULT=-- 
PARM SSCPT    TYPE=REAL    COUNT=(0:2) DEFAULT=--
PARM REQUATOR TYPE=REAL    COUNT=(0:1) DEFAULT=--
PARM RPOLE    TYPE=REAL    COUNT=(0:1) DEFAULT=-- 
PARM RADIUS   TYPE=REAL    COUNT=(0:1) DEFAULT=-- 
PARM SCALE    TYPE=REAL    COUNT=(0:1) DEFAULT=-- 
PARM LINE     TYPE=REAL    COUNT=(0:1) DEFAULT=-- 
PARM SAMPLE   TYPE=REAL    COUNT=(0:1) DEFAULT=-- 
PARM LATITUDE TYPE=REAL    COUNT=(0:1) DEFAULT=-- 
PARM LONGITUD TYPE=REAL    COUNT=(0:1) DEFAULT=-- 
END-PROC
.TITLE
PROGRAM MAPLABPROG
.HELP
PURPOSE:
   Program to add MAP2 labels to image. MAPLABPROG does nothing else.

EXECUTION:
  MAPLABPROG  [input-file-spec] output-file-spec PARAMETERS

OPERATION:
MAPLABPROG uses the subroutine SEARCV2 to read the input file's labels.
The user's input PARAMETERS are then used to update the label
parameters. A new MAP2 label is added to the output image and the
image data is copied from the input file to the output file. If a
PARAMETER is defaulted, the value from the input file MAP2 label is
used for the new label. 

RESTRICTIONS:

 Only byte and halfword data are processed.

 If no input picture is specified, NL and NS or SIZE must be specified
 (and a file with all pixels of 0 DN is written).
.page
EXAMPLE:
 MAPLABPROG IN OUT  RMAG=107593.9 SLAT=5.42 SLON=173.78 FOCL=1500. PSCAL=10.+
 SSCPT=(250.,250.) NORA=0. 'OS LAXIS=400. SAXIS=400.
  
 In this example a MAP2 label is added which indicates the image is 
in the object space projection (perspective) 

TIMING:
  MAPLAB takes as much time as is necessary to copy the input file.

HISTORY:
  Written By:  Joel Mosher 28-JUN-1986
  Cognizant Programmer: Jean Lorre
  Revisions
    25-JUN-1987 new parameter processing
    11 May 92  GMY  Link to SPICE (for PBDATA).

.LEVEL1
.VARI INP
  OPTIONAL INPUT FILE
.VARI OUT
  OUTPUT FILE
.VARI SIZE
  4 INTEGERS (1,1,NL,NS)
.VARI NL
 NUMBER OF LINES IN OUTPUT
.VARI NS
 NUMBER OF SAMPLES IN OUTPUT
.VARI INCR
 INCR=n specifies 
  copy every nth line and sample
.VARI REQUATOR
 REAL planet's equatorial radius 
.VARI RPOLE
 REAL planet's polar radius 
.VARI RADIUS
 REAL - planet's radius 
.VARI SCALE
scale of the output projection 
.VARI LINE
special line 
.VARI SAMPLE
special sample
.VARI LATITUDE
special latitude 
.VARI LONGITUDE
special longitude 
.VARI PROJECTN
Map projection
.VARI POLE
KEYWORD Polar case
.VARI NORTH
REAL - angle to north pole
.VARI PAR1
REAL - latitude of Lambert
standard parallel; or, special
longitude for Oblique.
.VARI PAR2
real - latitude of Lambert
standart parallel
.VARI SAXIS
 REAL - sample of the optical
 axis of the camera
.VARI LAXIS
REAL - line of the optical
axis of the camera
.VARI SLATITUDE
REAL - subspacecraft latitude
.VARI SLONGITUDE
real - subspacecraft longitude
.VARI PSCALE
real - pixels per millimeter
at the camera focal plane
.VARI FOCL
camera focal length
.VARI RMAG
distance from the spacecraft
to the center of the planet
.VARI NORANGLE
REAL - clockwise angle offset
of the rotation axis of
the planet
.VARI SSCPT
 2 REALS - planet center line and sample
.VARI TARGET
name of picture body
.LEVEL2
.VARI INP
 Optional input file. 
 If omitted, then the output file has all pixels of 0 DN.
.VARI OUT
 Required output file name.
 If there is no input file, then a SIZE (or NL,NS) parameter is required,
 and the output image will have all pixels of 0 DN
.VARI SIZE
This is the standard VICAR SIZE parameter, consisting of 4 integers:
Starting line, starting sample, number of lines, number of samples of input
to process.  However, starting line and sample are not supported in this
program:  they should be set to (1,1), although the actual value specified
is irrelevant.
Default is the entire input picture.
This parameter or (NL,NS) is required if there is no input image.
.VARI NL
 NUMBER OF LINES IN OUTPUT
.VARI NS
NUMBER OF SAMPLES IN OUTPUT
.VARI INCREMENT
 INCREMENT=n specifies that every n'th line and sample in the input
will be written to the output. The other map projection qualifiers
should refer to the resampled output file.
.VARI REQUATOR
 REAL - The planet's equatorial radius in kilometers. 
.VARI RPOLE
 REAL - The planet's polar radius in kilometers. 
.VARI RADIUS
 REAL - Set both the planet's equatorial and polar radius to the
given value (in kilometers). 

.VARI TARGET
TARGET=picture-body-name specifies the name of the object in the picture.
This sets the equatorial and polar radii to that stored in subroutine PBDATA. 
Valid names are the names of the planets and their satellites.
.VARI SCALE
 The scale of the output projection in Km/pixel. 
.VARI LINE
The center line of the output projection.  
.VARI SAMPLE
The center sample of the output projection. 
.VARI LATITUDE
The latitude of the center of the output projection in degrees. 

.VARI LONGITUDE
The longitude of the center of the output projection in degrees.
 
.VARI PROJECTN
 This keyword specifies the map projection.  A list of supported projections
 with their keywords is given below.  For most projections, the following
 additional parameters are required:  the location of any one point on the
 planet must be specified using LINE, SAMP, LATI and LONG. The default is
 taken from the input picture. The scale must be set by specifying SCAL. 
 A few projections have special restrictions or parameters, and these
 are explained below.

CYLINDR:  invokes the Cylindrical projection.  LATI is assumed to be zero
 and SAMPle is assumed to be one.  (If LATI or SAMP are specified, they are
 recomputed by the program.)

OBCYLIND:  invokes the Oblique Cylindrical projection.  LINE specifies
 the location of latitude=0,  SAMP specifies that of longitude=180.
 LATI and LONG specify the position of the North pole.  PAR1 is the
 longitude to which LONG will move.

MERCATOR:  invokes the Mercator projection. 

TMERCATO:  invokes the Transverse Mercator projection.  

STEREOGR:  invokes the Stereographic projection.  Specifying 'POLE implies
 the Polar case, in which LONG specifies the meridian which points 'up' in
 the picture, and LATI must be either +90.0 or -90.0.

ORTHOGR: invokes the Orthographic projection.  Specifying 'POLE implies
 the Polar case, in which LONG specifies the meridian which points 'up',
 and LATI must be either +90.0 or -90.0. 

LAMBERT:  invokes the Lambert projection. The scale specified by SCAL is
 that at the standard parallels, which are specified by PAR1 and PAR2.
 LINE specifies the line into which the visible pole is mapped. LONG
 specifies the longitude which appears vertical and SAMP indicates the
 sample into which this longitude is mapped. The cone will be cut at
 longitude LONG + 180. 

SINUSOID:  invokes the Sinusoidal projection.

OBSINUS:  invokes the Oblique Sinusoidal projection.  LINE specifies
 the location of latitude=0,  SAMP specifies that of longitude=180.
 LATI and LONG specify the position of the North pole.  PAR1 is the
 longitude to which LONG will move.

MOLLWEID:  invokes the Mollweide projection.

LATLON or RECTANG: invokes the Simple Cylindrical projection. 

OS, OBJECT, or PERSPECT:  invokes the Point Perspective output projection.
 (This is also known to VICAR as "Object Space".)  For this projection,
 the program also needs the sub-spacecraft point (latitude/longitude),
 target radii and distance (range), and camera scale and focal length.
 These are normally obtained from the label, but can all be specified
 by user parameter.  The OM matrix and RS vector are computed from these
 parameters and are printed out for informational purposes (e.g., for
 comparison with those printed by MAP3).

.VARI POLE
KEYWORD - Specifies the Polar case for either Stereographic or
Orthographic ouput projection types. The default is not Polar. 

.VARI NORTH
REAL - In the non-polar case of a Stereographic or Orthographic
output projection, the north pole will be displaced clockwise from
'up' (decreasing line direction). The default is 0.0; the units are
degrees. 

.VARI PAR1
REAL - Defines the upper latitude which will cut the cone in a
Lambert output projection; it must be greater than PAR2. Units are degrees. 

This parameter is also used to specify the special longitude in the
Oblique projections (see Help PROJECTN).

.VARI PAR2
REAL - Defines the lower latitude which will cut the cone in a
Lambert output projection; it must be less than PAR1.  Units are degrees. 

.VARI SAXIS
 REAL - Specifies the output sample of the optical axis of the camera
system.  Required by the Perspective projection (only).  The default is 
the center of the image. 

.VARI LAXIS
REAL - Specifies the output line of the optical axis of the camera
system.  Required by the Perspective projection (only).The default is 
the center of the image. 

.VARI SLATITUDE
Specifies the output subspacecraft latitude in degrees. 

.VARI SLONGITUDE
Specifies the output subspacecraft longitude in degrees. 

.VARI PSCALE
Specifies the number of output pixels per millimeter at the camera
focal plane. (Picture scale.)  No default, required by the Perspective
projection (only).

.VARI FOCL
 Specifies the output camera focal length in millimeters. No default,
 required by the Perspective projection (only).

.VARI RMAG
Specifies the output distance from the spacecraft to the center of
the planet in kilometers. No default, required by the Perspective
projection (only).

.VARI NORANGLE
REAL - Specifies the output clockwise angle offset of the rotation
axis of the planet from 'up' in degrees.  The default is zero.

.VARI SSCPT
 2 REALS - Specifies the output planet center (sub-spacecraft point)
line and sample.  Required by the Perspective projection (only).
Default is the center of the image.
$ Return
$!#############################################################################
$Imake_File:
$ create maplabprog.imake
#define PROGRAM maplabprog

#define MODULE_LIST maplabprog.f

#define MAIN_LANG_FORTRAN
#define R2LIB


#define INCLUDE_LIST common.fin

#define USES_FORTRAN
#define LIB_P2SUB
#define LIB_RTL
#define LIB_TAE
#define LIB_MATH77
#define LIB_SPICE
#define LIB_NETWORK
/*#define LIB_LOCAL	/* disable on delivery */
$ Return
$!#############################################################################
$Test_File:
$ create tstmaplabprog.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"

! create map labels for various projections:

! RECTANGULAR 
maplabprog out=a nl=10 ns=10 line=1. samp=1. lati=50. long=360. scal=100. +
	'io 'rect
label-list a

! MERCATOR
maplabprog out=a nl=10 ns=10 line=1. samp=1. lati=50. long=360. scal=100. +
	'io 'MERCAT
label-list a

! TRANSVERSE MERCATOR
maplabprog out=a nl=10 ns=10 line=1. samp=1. lati=50. long=360. scal=100. +
	'io 'TMERC
label-list a

! CYLINDRICAL
maplabprog out=a nl=10 ns=10 line=1. samp=1. lati=50. long=360. scal=100. +
	'io 'CYLIN
label-list a

! OBLIQUE CYLINDRICAL  --- omit for now, due to MP bug
!maplabprog out=a nl=10 ns=10 line=1. samp=1. lati=50. long=360. scal=100. +
!	par1=40 'io 'OBCYL
!label-list a

! SINUSOIDAL
maplabprog out=a nl=10 ns=10 line=1. samp=1. lati=50. long=360. scal=100. +
	'io 'SINUS
label-list a

! OBLIQUE SINUSOIDAL 
maplabprog out=a nl=10 ns=10 line=1. samp=1. lati=50. long=360. scal=100. +
	par1=40 'io 'OBSINU
label-list a

! LAMBERT
maplabprog out=a nl=10 ns=10 line=1. samp=1. lati=50. long=360. scal=100. +
	par1=30 par2=60 'io 'LAMB
label-list a

! MOLLWEIDE
maplabprog out=a nl=10 ns=10 line=1. samp=1. lati=50. long=360. scal=100. +
	'io 'MOLL
label-list a

! ORTHOGRAPHIC
maplabprog out=a nl=10 ns=10 'half radius=1738.09 lati=10. long=20. +
	north=180. scal=2. line=3. samp=4. 'orth 
label-list a

! POLAR ORTHOGRAPHIC
maplabprog out=a nl=10 ns=10 'half radius=1738.09 lati=10. long=20. +
	north=180. scal=2. line=3. samp=4. 'orth 'POLE
label-list a

! STEREOGRAPHIC
maplabprog out=a nl=10 ns=10 'half radius=1738.09 lati=10. long=20. +
	north=180. scal=2. line=3. samp=4. 'STEREO
label-list a

! POLAR STEREOGRAPHIC
maplabprog out=a nl=10 ns=10 'half radius=1738.09 lati=10. long=20. +
	north=180. scal=2. line=3. samp=4. 'STEREO 'POLE
label-list a

! PERSPECTIVE
gen c nl=10 ns=10 
maplabprog c d 'obj nl=10 ns=10 Req=2439. rpol=2439. sscp=(10,20) nora=30. +
	rmag=123456. focl=1500. pscal=84. slat=40. slon=50. laxis=400. +
	saxis=500. 
label-list d
list c
list d

! check response to some errors ...

! no input or SIZE spec:
maplabprog out=a line=1. samp=1. lati=50. long=360. scal=100. 'io 'rect

!  zero size param.
maplabprog out=a size=(1 1 0 100) line=1. samp=1. lati=50. long=360. scal=100. +
 'io 'rect
maplabprog out=a ns=0 nl=100 line=1. samp=1. lati=50. long=360. scal=100. +
 'io 'rect

! NS exceeds pgm buffer:
maplabprog out=a size=(1 1 100 100000) line=1. samp=1. lati=50. long=360. +
 scal=100. 'io 'rect

! insufficient inputs for Perspective mode ...
maplabprog c d 'obj nl=10 ns=10 Req=2439. rpol=2439. sscp=(10,20) nora=30. +
	rmag=123456. pscal=84. slat=40. slon=50. laxis=400. saxis=500. 

! try halfword image
maplabprog out=a nl=10 ns=10 line=1. samp=1. lati=50. long=360. scal=100. +
	'io 'rect 'half
label-list a
gen c nl=10 ns=10 'half
maplabprog c a nl=10 ns=10 line=1. samp=1. lati=50. long=360. scal=100. +
	'io 'rect
label-list a
list a

end-proc
$ Return
$!#############################################################################
