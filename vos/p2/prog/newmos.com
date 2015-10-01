$!****************************************************************************
$!
$! Build proc for MIPL module newmos
$! VPACK Version 1.9, Friday, October 10, 2003, 17:00:21
$!
$! Execute by entering:		$ @newmos
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
$ write sys$output "*** module newmos ***"
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
$ write sys$output "Invalid argument given to newmos.com file -- ", primary
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
$   if F$SEARCH("newmos.imake") .nes. ""
$   then
$      vimake newmos
$      purge newmos.bld
$   else
$      if F$SEARCH("newmos.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake newmos
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @newmos.bld "STD"
$   else
$      @newmos.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create newmos.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack newmos.com -mixed -
	-s newmos.f -
	-i newmos.imake -
	-p newmos.pdf -
	-t tstnewmos.pdf new_session_3d.log old_session_3d.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create newmos.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
C
C VICAR Program NEWMOS: Mosaicking program
C 10-July-95...CRI... MSTP S/W CONVERSION (VICAR PORTING)
C 30-July-96 ..lwk..  fixed projection type check; replaced MP_XY2LL with
c			CONVEV for Perspective case (temporary!);  fixed
c			some bugs in Perspective case
C 15-Oct-96  ..lwk..  fixed format problem when NUMIPT>6, write History
c			map labels as well as Property

      SUBROUTINE MAIN44


      IMPLICIT NONE

      EXTERNAL MOSB
c      include 'fortport'
      COMMON/C1/PAR,NUMIPT,NUMLIN,LSET,RSET,LTHRES,RTHRES,LSEQ,
     + RSEQ,LNIB,RNIB,LNIBST,RNIBST,NSEQ,ITHRES,ITHRST,BLKSIZ,NSI,
     + LINE,SAMP,SCP,NSO,NIBB,NIBBST,NSEQST,RSEQST,LSEQST,LCP,
     + LTHRST,RTHRST,NOSAMP,NOLINE,REFIN,DCLE,INCR,ISWTCH,NLO,
     + IAVER,ISMOOTH,NLR,REQ,RPOLE,LAB,weight,NOBAND,JSB,NBO,NBI
      COMMON /FILES/ INFIL, OUTFIL
      real*4 weight(80),REQ,RPOLE 
      INTEGER REFIN(80),LENGTH,NUMIPT,NUMLIN,LSET,LTHRES
      INTEGER LSEQ,NIBB,LNIB,LNIBST,NSEQ,ITHRES,ITHRST
      INTEGER NSI,NSO,NIBBST,NSEQST,LSEQST,LCP,LTHRST,INCR,ISWTCH
      INTEGER NLO,IAVER,ISMOOTH,ISB,INDEX,I,ISTAT,NPXL,JSL,JSS,JNSI
      INTEGER NSB,NS4,NLR,LENCHK
	 INTEGER JSB,NBO,NBI
      INTEGER PAR(100),RTHRST,RSEQST,RSEQ,RNIB,RNIBST,RTHRES,RSET,BLKSIZ
      INTEGER LINE(80),SAMP(80),SCP,DCLE,NOLINE(80),
     1	    NOSAMP(80),NOBAND(80),LAB(80)
      INTEGER INFIL(80),OUTFIL
	 CHARACTER*3 ORGIN

c  tell user what version we have:
      call ifmessage('*** NEWMOS version 18-Aug-03 ***')

C     Map Routine Functions
C     INITIALIZE ALL VARIABLES TO PREVENT ANY UNEXPECTED
C     BEHAVIOUR.

      CALL ZIA(PAR,100)
      CALL ZIA(REFIN,80)
      CALL ZIA(LINE,80)
      CALL ZIA(SAMP,80)
      CALL ZIA(NOLINE,80)
      CALL ZIA(NOSAMP,80)
	 CALL ZIA(NOBAND,80)
      CALL ZIA(LAB,80)
      CALL ZIA(INFIL,80)
      LENGTH=0
      NUMIPT=0
      NUMLIN=0
      LSET=0
      LTHRES=0
      LSEQ=0
      NIBB=0
      LNIB=0
      LNIBST=0
      NSEQ=0
      ITHRES=0
      ITHRST=0
      NSI=0
      NSO=0
      NIBBST=0
      NSEQST=0
      LSEQST=0
      LCP=0
      LTHRST=0
      INCR=0
      ISWTCH=0
      NLO=0
      IAVER=0
      ISMOOTH=0
      ISB=0
      INDEX=0
      I=0
      ISTAT=0
      NPXL=0
      JSL=0
      JSS=0
      JNSI=0
      NSB=0
      NS4=0
      NLR=0
      LENCHK=0
      RTHRST=0
      RSEQST=0
      RSEQ=0
      RNIB=0
      RNIBST=0
      RTHRES=0
      RSET=0
      BLKSIZ=0
      SCP=0
      DCLE=0
      OUTFIL=0
      CALL ZIA(weight,80)
      REQ=0.0
      RPOLE=0.0

      ISB = 4
      LENGTH = 1
      INDEX = 1
      nsi = 0
c
      call xvpcnt( 'INP', numipt)
      do i=1,numipt
        weight(i)=1.0
      enddo
C
C  PRIMARY INPUT:
      CALL XVUNIT(INFIL(1),'INP',1,ISTAT,' ')
      CALL XVOPEN(INFIL(1),ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +          'U_FORMAT','HALF',' ')

c     Check organization of image, prohibit BIP
      CALL XVGET(INFIL(1),ISTAT,'ORG',ORGIN, ' ')
      IF (ORGIN.EQ.'BIP') CALL MABEND(
     +  'BIP files not supported, use program TRAN to convert to BSQ')

      CALL XVGET(INFIL(1),ISTAT,'NL',NOLINE(1),'NS',NOSAMP(1),
     +          'NB',NOBAND(1),'PIX_SIZE',NPXL,' ')
      NSI = NSI+NOSAMP(1)	! NSI is the number of samples total in input
      REFIN(1) = INDEX
      INDEX = NOSAMP(1) + INDEX
      ISWTCH = 1
      IF (NPXL.EQ.2) ISWTCH = 0
      IF (NPXL.GT.2) CALL MABEND( '** ILLEGAL FORMAT **')
      IF (NPXL .EQ. 1)
     *          CALL XVMESSAGE('SYSTEM LABEL SAYS INPUT IS BYTE',' ')
      IF (NPXL .EQ. 2)
     *      CALL XVMESSAGE('SYSTEM LABEL SAYS INPUT IS HALFWORD',' ')

      CALL XVCLOSE(INFIL(1),ISTAT,' ')

C  OTHER INPUTS:
      DO I = 2,NUMIPT
         CALL XVUNIT(INFIL(I),'INP',I,ISTAT,' ')
         CALL XVOPEN(INFIL(I),ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +          'U_FORMAT','HALF',' ')

c     Check organization of image, prohibit BIP
      CALL XVGET(INFIL(I),ISTAT,'ORG',ORGIN, ' ')
      IF (ORGIN.EQ.'BIP') CALL MABEND(
     +  'BIP files not supported, use program TRAN to convert to BSQ')

         CALL XVGET(INFIL(I),ISTAT,'NL',NOLINE(I),'NS',NOSAMP(I),
     +          'NB',NOBAND(I),'PIX_SIZE',NPXL,' ')
         NSI = NSI+NOSAMP(I)      ! NSI is the number of samples total in input
         REFIN(I) = INDEX
         INDEX = NOSAMP(I) + INDEX

	 CALL XVCLOSE(INFIL(I),ISTAT,' ')

      ENDDO
C
      CALL XVSIZE( JSL, JSS, NLO, NSO, NUMLIN, JNSI)
      CALL XVBANDS(JSB, NBO, NBI)
	
      IF ( JSB .GT. NBI ) CALL MABEND(
     +  'SB is greater than the total number of bands')
                 
      IF ( JSB + NBO - 1 .GT. NBI) THEN
	 CALL XVMESSAGE('***Number of bands truncated', ' ') 
	 NBO = NBI + 1 - JSB
      ENDIF
	
C
      CALL XVUNIT(OUTFIL,'OUT',1,ISTAT,' ')		! output file
C
C  OUTPUT FILE IS OPENED IN SUBROUTINE MOSA, ONCE ITS SIZE IS FOUND.
C
      CALL MOSA
C
C  REFIN is starting byte of each pix in input buffer INBUF
C  NSB is length of  the output buffer in bytes
C
      NSB = NSO * 2
      LENGTH = NSI * 2
      NS4 = NSO*4
      LENCHK = LENGTH + NSB + NS4 + NS4
C
      CALL STACKA(8,MOSB,5,LENGTH,NSB,NS4,NS4,NS4,LENCHK)
C
C      DO I = 1, NUMIPT			! Close files
C	 CALL XVCLOSE(INFIL(I),ISTAT,' ')
C      END DO
      CALL XVCLOSE(OUTFIL,ISTAT,' ')

   11 RETURN
      END

C******************************************************************************
      SUBROUTINE MOSA

      COMMON/C1/PAR,NUMIPT,NUMLIN,LSET,RSET,LTHRES,RTHRES,LSEQ,
     + RSEQ,LNIB,RNIB,LNIBST,RNIBST,NSEQ,ITHRES,ITHRST,BLKSIZ,NSI,
     + LINE,SAMP,SCP,NSO,NIBB,NIBBST,NSEQST,RSEQST,LSEQST,LCP,
     + LTHRST,RTHRST,NOSAMP,NOLINE,REFIN,DCLE,INCR,ISWTCH,NLO,
     + IAVER,ISMOOTH,NLR,REQ,RPOLE,LAB,weight,NOBAND,JSB,NBO,NBI
      COMMON /FILES/ INFIL, OUTFIL
      real*4 weight(80)
      INTEGER*4 REFIN(80)
      INTEGER PAR(100),RTHRST,RSEQST,RSEQ,RNIB,RNIBST,RTHRES,RSET,BLKSIZ
	 INTEGER JSB,NBO,NBI
      INTEGER LINE(80),SAMP(80),SCP,DCLE,NOLINE(80),NOSAMP(80),LAB(80),
     1	 NOBAND(80)
      INTEGER INFIL(80), OUTFIL
      CHARACTER*4 OFORM
      character*80 msgout
      logical xvptst
      LOGICAL NOMIN
c
C         PARAMETER PROCESSER
C
c threshold
	ithrst = 0
	lthrst = 0
	rthrst = 0
	call xvparm( 'THRESH', ithres, i ,j,1)
	if (ithres.ne.1) ithrst = 1
	call xvparm( 'LTHRESH', lthres, i, j,1)
	if (lthres.ne.1) lthrst = 1
	call xvparm( 'RTHRESH', rthres, i, j,1)
	if (rthres.ne.1) rthrst = 1
c
	call xvparm( 'ETHRESH', ipar, i, j,1)
	if (i.gt.0) then
	  lthres = ipar
	  rthres = ipar
	endif
c
c  no. of threshold points for trigger
	nseqst = 0
	call xvparm( 'NSEQ', nseq, i, j,1)
	if (nseq.ne.1) nseqst = 1
c
	lseqst = 0
	call xvparm( 'LSEQ', lseq, i, j,1)
	if (lseq.ne.1) lseqst = 1
c
	rseqst = 0
	call xvparm( 'RSEQ', rseq, i, j,1)
	if (rseq.ne.1) rseqst = 1
c
c  nibbles in from edge of picture data
	nibbst= 0
	call xvparm( 'NIBB', nibb, i, j,1)
	if (nibb.ne.0) nibbst = 1
c
	lnibst = 0
	call xvparm( 'LNIB', lnib, i, j,1)
	if (lnib.ne.0) lnibst = 1
c
	rnibst = 0
	call xvparm( 'RNIB', rnib, i, j,1)
	if (rnib.ne.0) rnibst = 1
c
c  increment in edge search
	call xvparm( 'INCR', incr, i, j,1)
c
c  (line,samp) for common points for all inputs
	call xvparm( 'PIXL',  par, icnt, j,100)
	nls = icnt/2
	if (nls*2.ne.icnt) call mabend(' Error in PIXL count')
	do ii = 1,nls
	  line(ii) = par(2*ii-1)
	  samp(ii) = par(2*ii)
	enddo
	numpix = nls
c
c  (line,samp) for common point in output
	call xvparm( 'LCP', lcp, i, j,1)
	call xvparm( 'SCP', scp, i, j,1)
c
c  set dclevel of background
	call xvparm( 'DCLEV', dcle, i, j,1)
c
c  output format
	if (xvptst( 'BYTE')) iswtch = 1
	if (xvptst( 'HALF')) iswtch = 0
c
c  radii specified by target body or directly:
	if (xvptst( 'JUPI')) then
	  req = 71400.
	  rpole = 66773.
	elseif (xvptst( 'IO') .or. xvptst( 'J1')) then
	  req = 1816.
	  rpole = req
	elseif (xvptst( 'EURO') .or. xvptst( 'J2')) then
	  req = 1569.
	  rpole = req
	elseif (xvptst( 'GANY') .or. xvptst( 'J3')) then
	  req = 2631.
	  rpole = req
	elseif (xvptst( 'CALL') .or. xvptst( 'J4')) then
	  req = 2400.
	  rpole = req
	elseif (xvptst( 'MOON')) then
	  req = 1738.09
	  rpole = req
	elseif (xvptst( 'MCRY')) then
	  req = 2439.
	  rpole = req
	else
	  call xvparm( 'RADIUS', req, i, j,1)
	  if (req.eq.0.) then
	    call xvparm( 'REQ', req, i, j,1)
	    call xvparm( 'RPOL', rpole, i, j,1)
	  else
	    rpole = req
	  endif
	endif
c
c  other params
        call xvparm('WEIGHT',weight,i,j,80)

	iaver= 0
	if (xvptst( 'AVER')) iaver = 1

        ismooth=0
	if (xvptst( 'SMOOTH')) ismooth=1
c
	iadapt = 0
	imap2 = 0
	if (xvptst( 'ADAPT')) then
	  iadapt = 1
	  imap2 = 1
	endif
	if (xvptst( 'MAP2')) imap2 = 1
        NOMIN = XVPTST('NOMIN')
c
      IF (IMAP2 .EQ. 1) THEN	!FIND OFFSETS IN PROJECTIONS & OPEN OUTPUT
	CALL OFFSET( NUMIPT, REQ, RPOLE, LINE(1), SAMP(1), LCP, SCP,
     &   NLO, NSO, NBO, NOLINE(1), NOSAMP(1), IADAPT, NLR, LAB, 
     &   ISWTCH,NOMIN)
      ELSE			!JUST OPEN OUTPUT
	OFORM = 'BYTE'
	IF (ISWTCH.EQ.0) OFORM = 'HALF'
	CALL XVOPEN( OUTFIL, ISTAT, 'U_NL', NLO, 'U_NS', NSO, 'OPEN_ACT',
     &   'SA', 'IO_ACT', 'SA', 'OP', 'WRITE', 'U_FORMAT', 'HALF',
     &	 'O_FORMAT', OFORM,'U_NB',NBO,' ')
      ENDIF
C
      call xvmessage(' ',' ')
      nblocks = (numipt+5)/6
      do iblock=1,nblocks
	i1 = (iblock-1)*6+1
	i2 = iblock*6
	if (i2.gt.numipt) i2 = numipt
        msgout ='LINE= '
        j=6
        do i=i1,i2
	  j=j+11
	  write(msgout(j:j+3), '(I4)') line(i)
        enddo
        call xvmessage(msgout,' ')
        msgout ='SAMP= '
        j=6
        do i=i1,i2
	  j=j+11
	  write(msgout(j:j+3), '(I4)') samp(i)
        enddo
        call xvmessage(msgout,' ')
        msgout ='NOLINE= '
        j=6
        do i=i1,i2
	  j=j+11
	  write(msgout(j:j+3), '(I4)') noline(i)
        enddo
        call xvmessage(msgout,' ')
        msgout ='NOSAMP= '
        j=6
        do i=i1,i2
	  j=j+11
	  write(msgout(j:j+3), '(I4)') nosamp(i)
        enddo
        call xvmessage(msgout,' ')
        call xvmessage(' ',' ')
      enddo
      IF (IADAPT .EQ. 1) RETURN
      IF (NUMPIX .EQ. NUMIPT) RETURN
      msgout='***NUMBER OF OFFSETS= '
      write(msgout(23:26), '(I4)') numpix
      call xvmessage(msgout,' ')	
      msgout='***NUMBER OF INPUTS= '
      write(msgout(23:26), '(I4)') numipt
      call xvmessage(msgout,' ')	

      RETURN
      END

C***************************************************************************
      SUBROUTINE OFFSET(NUMIPT,REQ,RPOLE,LINE,SAMP,LCP,SCP,NLO,NSO,NBO,
     1NOLINE,NOSAMP,IADAPT,NLR,LAB,ISWTCH, NOMIN)

      INCLUDE 'mp_for_defs'

      COMMON /FILES/ INFIL,OUTFIL
      INTEGER CIRCUM,SSTART(80),SSTOP(80),TPROJ(80),NOLINE(80)
      INTEGER NOSAMP(80),LINE(80),SAMP(80)
      LOGICAL NOMIN
      REAL RDATA(40),TSAMP(80),TLINE(80),TLAT(80),TLAT1(80),TLAT2(80)
      REAL TLONG(80),TSCALE(80),TNOANG(80),RDATAO(40)
      REAL REQCK(80),RPOLCK(80)
      integer idata(40)
      REAL*8 XC,ZC,TH,TH1,TH2,LAM,F,PSI,RP   ! ARGUMENTS FOR TRANV
      REAL*8 FLINE,FSAMP,FLAT,FLON          !arguments for mp_ll2xy
      real*8 MAP_RSLTN
C        INTEGER MODE
	 INTEGER*4 NBO
      INTEGER SCP,LPAR1(80),LPAR2(80),LAB(80)
      integer number_keywords
      integer types(mp_number_of_keywords)
      integer classes(mp_number_of_keywords)
      INTEGER INFIL(80), OUTFIL
      integer status,i
      real*8 mp,RLCP,RSCP

      CHARACTER*4 OFORM
      character*(mp_max_keywd_length) keys(mp_number_of_keywords)
      character*(mp_max_keywd_length) mpt(mp_number_of_keywords)
      character*(mp_max_keywd_length) keytype, mkey
      logical blank
      CHARACTER*132 MSG3,MSG5
      character*100 msgout
      EQUIVALENCE (RDATA(7),SCALE)
      EQUIVALENCE (RDATA,IDATA)
C
      CALL ZIA(SSTART,80)
      CALL ZIA(SSTOP,80)
      CALL ZIA(TPROJ,80)
      CIRCUM=0
      CALL ZIA(IDATA,40)
      CALL ZIA(TSAMP,80)
      CALL ZIA(TLINE,80)
      CALL ZIA(TLAT,80)
      CALL ZIA(TLAT1,80)
      CALL ZIA(TLAT2,80)
      CALL ZIA(TLONG,80)
      
      CALL ZIA(TSCALE,80)
      CALL ZIA(TNOANG,80)
      CALL ZIA(RDATAO,40)
      CALL ZIA(REQCK,80)
      CALL ZIA(RPOLCK,80)
      XC=0.0
      ZC=0.0
      TH=0.0
      TH1=0.0
      TH2=0.0
      LAM=0.0
      F=0.0
      PSI=0.0
      RP =0.0  ! ARGUMENTS FOR TRANV
      FLINE=0.0
      FSAMP=0.0
      FLAT=0.0
      FLON =0.0         !arguments for mp_ll2xy
      MAP_RSLTN=0.0
      do i=1,mp_number_of_keywords
         mpt(i)=' '
      enddo
      
      IFLAG = 0
      RLCP = LCP
      RSCP = SCP
      RAD = 57.29578
C
C     PROJECTION TYPES
      mpt(1) = 'POLAR_ORTHOGRAPHIC'
      mpt(2) = 'OBLIQUE_ORTHOGRAPHIC'
      mpt(3) = 'POLAR_STEREOGRAPHIC'
      mpt(4) = 'OBLIQUE_STEREOGRAPHIC'
      mpt(5) = 'LAMBERT'
      mpt(6) = 'MERCATOR'
      mpt(9) = 'NORMAL_CYLINDRICAL' 
      mpt(10) = 'SIMPLE_CYLINDRICAL'   !   (RECTANGULAR)
      mpt(11) = 'OBLIQUE_SIMPLE_CYLINDRICAL' 
      mpt(12) = 'SINUSOIDAL'
      mpt(13) = 'OBLIQUE_SINUSOIDAL'
      mpt(14) = 'MOLLWEIDE'
      mpt(15) = 'TRANSVERSE_MERCATOR'
      mpt(16) = 'POINT_PERSPECTIVE'

      DO 99 J = 1, NUMIPT
         CALL ZIA(RDATA,40)
c         CALL ZIA(WORK,1800)

c  map routine conversion
         CALL XVUNIT(INFIL(J),'INP',J,ISTAT,' ')
         CALL XVOPEN(INFIL(J),ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +          'U_FORMAT','HALF',' ')

         call mp_init(mp,status)
         call mp_label_read(mp,infil(j),status)

c         call xvp('PCK_PATH',pck_file,count)
c         call mp_get_par(mp,pdf_parms,pds_keys,pck_file)

         call mp_get_keywords(mp,keys,number_keywords,types,classes,
     +                        status)
         call mp_get_value_str(mp,'MAP_PROJECTION_TYPE',keytype,
     +				status)
         if (status.ne.mp_success) then
           call mabend('*** MIPS STATUS ERROR ***')
         endif
         tproj(J) = 0
         do i=1,mp_number_of_keywords
	   mkey = mpt(i)
	   ! find length of keyword, excluding blanks
	   len = mp_max_keywd_length
	   blank = .true.
           do while (blank)
	     if (mkey(len:len).ne.' ' .or. len.eq.1) then
		blank = .false.
	     else
		len = len-1
	     endif
	   enddo
           if (mkey.eq.keytype(:len))  tproj(J) = i
         enddo
         
	 L = tproj(J)
         if ((L .le.0).or.(L .gt.16)) then
           call mabend('** MAP PROJECTION TYPE ERROR **')         
         endif
         IF (L.EQ.7 .OR.L.EQ.8) THEN
           CALL XVMESSAGE(
     +      '***  THE FOLLOWING DATA SET IS NOT A PROJECTION:',' ')
           CALL PRNT(4,1,J,'FILE = .')
         else
	   call mp_mpo2buf( mp, rdata, status)
	   if (status.ne.mp_success) call mabend(
     +	    ' *** ERROR IN MPO2BUF ***')
         END IF

	if (l.ne.16) then
C     Redundant variables:      also equivalent:      name:
	 xc = RDATA(1) 		!  TSAMP(J)		SAMPLE POINT
	 zc = RDATA(2) 		!  TLINE(J)		LINE POINT
	 th = RDATA(3) 		!  TLAT(J)		LATITUDE POINT
	 th1 = RDATA(4) 	!  TLAT1(J)		LATITUDE ATA PARALLEL
	 th2 = RDATA(5) 	!  TLAT2(J)
	 lam = RDATA(6) 	!  TLONG(J)		LONGITUDE WEST 
	 tlong(j)=lam         
	 f = RDATA(7) 		!  TSCALE(J)		SCALE
	 MAP_RSLTN = RDATA(7)
c	 RDATA(8) = 0 ! visible pole not implemented in new MIPS, was: TPOLE(J)
	 psi = RDATA(9) 	!   TNOANG(J)		NORTH ANGLE
	endif
	 rp = RDATA(25) 	!   RPOLCK(J)
	 req = RDATA(26) 	!   REQCK(J) 
         TPROJ(J) = idata(39) 	!   PROJECTION TYPE 

C
C     IF RADIUS HAS NOT BEEN SPECIFIED BY USER,
C     USE IT IF IT IS IN MAP2 LABEL OF FIRST INPUT
         IF (REQ .EQ. 0.)   REQ = REQCK(J)
         IF (RPOLE .EQ. 0.) RPOLE = RPOLCK(J)
         IF (REQ .NE. 0.)   RDATA(26) = REQ
         IF (RPOLE .NE. 0.) RDATA(25) = RPOLE
         IF (REQ .NE. 0.)   REQCK(J) = REQ
         IF (RPOLE .NE. 0.) RPOLCK(J) = RPOLE
C	     IF USER SPECIFIED RADIUS INFO, IT OVERRIDES SEARCW
C         IF(REQ.NE.0.)RDATA(26) = REQ
C         IF(RPOLE.NE.0.)RDATA(25) = RPOLE
C
C     CIRCUM IS CIRCUMFERENCE OF PLANET IN PIXELS AT EQUATOR
         IF (SCALE .NE. 0.) CIRCUM = REQ*6.283185/SCALE  +.5
 	
         GO TO (8,8,8,8,8,7,9,2,6,5,8,8,8,8,8,2), L
         CALL MABEND('** PROGRAM ERROR **')	! (SHOULD BE CAUGHT ABOVE)

C     OBJECT SPACE:  CHECK FOCAL, OPT LINE,OPT SAMP TO SEE IF MAP2 LABELS
C     WERE USED
                  
 2	DO I = 27,30
            IF(RDATA(I).EQ.0.)GO TO 9
         ENDDO
         LINE(J) = RDATA(33) + .5
         IF (RDATA(33) .LT. 0.) LINE(J) = RDATA(33)-.5
         SAMP(J) = RDATA(34) + .5
         IF (RDATA(34) .LT. 0.) SAMP(J) = RDATA(34) - .5
         GO TO 9

    5    CONTINUE
C     SIMPLE CYLINDRICAL PROJECTION:  LONGITUDE CORRESPONDS TO SAMP = 1

         FLAT = 0.0      ! USE CONVEV TO FIND THE LINE,SAMP FOR 
         FLON = 0.0      ! LAT=0 AND LONG=0.
c  map routine canversion
         call mp_ll2xy(mp,fline,fsamp,flat,flon,1,status)
	 IF(status .NE. mp_success) THEN 
           if (status .EQ. -1008) then
              msg3='*************************************************'
              call xvmessage(msg3,' ')
              write(msg3(5:22),'(A)' ) ' MAP PROJECTION:'
              call xvmessage(msg3,' ')
              msg3='******  '
              write(msg3(8:),'(A)' ) keytype
              call xvmessage(msg3,' ')
              msg3='******  is NOT supported by MP_LL2XY ************'      
              call xvmessage(msg3,' ')
              msg3='*************************************************'
              call xvmessage(msg3,' ')
                call abend()     
            else
	       msg3='mp_ll2xy returned status = '
	       write(msg3(30:35),'(I6)') status   
               call xvmessage(msg3,' ')
               CALL MABEND('ERROR COMPUTING OFFSET')
            endif
         END IF
         LINE(J) = NINT( FLINE )
         SAMP(J) = NINT( FSAMP )
	 IF (SAMP(J).LT.1) SAMP(J) = SAMP(J) + CIRCUM
         IF (IADAPT.EQ.1) GO TO 9
         SSTART(J) = SCP-SAMP(J)
         SSTOP(J) = SCP-SAMP(J)+NOSAMP(J)-1
         IF(SSTOP(J).LT.0)SAMP(J) = SAMP(J)-CIRCUM
         GO TO 9

    6    CONTINUE

C     NORMAL CYLINDRICAL PROJECTION:  SAMP CORRESPONDS TO LONGITUDE = 0
         LINE(J) = NINT( RDATA(2) )
	 IF (NOMIN) THEN
            samp(j) = lam * MAP_RSLTN + 1! CENTER_LONGITUDE * MAP_RSLTN
	 ELSE
           RSAMP = (REQ/RDATA(7))*((RDATA(6))/RAD) + 1.
           SAMP(J) = RSAMP+.5
	 ENDIF
         IF(RSAMP.LT.0.)SAMP(J) = RSAMP-.5

C...........usually lat should be 0;  if not call CONVEV/TRANV to
c...........get line and samp for lat=0, long=0.

         IF (th.NE.0. .AND. .NOT.NOMIN)  THEN
           FLAT = 0.0      ! USE CONVEV TO FIND THE LINE,SAMP FOR 
           FLON = 0.0      ! LAT=0 AND LONG=0.

c  map routine conversion

c
           call mp_set_value(mp,'SAMPLE_PROJECTION_OFFSET',xc-1,
     1     status)
           call mp_ll2xy(mp,fline,fsamp,flat,flon,1,status)
           IF (status.NE.mp_success) THEN 
               CALL MABEND('TP1 ERROR COMPUTING OFFSET')
           END IF
           LINE(J) = NINT( FLINE )
           SAMP(J) = NINT( FSAMP )
         END IF

         GO TO 9
C
    7    CONTINUE
C     MERCATOR

         RLAT = RDATA(3)/RAD
         E = SQRT(1-(RPOLE/REQ)**2.)
         RLINE = (REQ/RDATA(7))*ALOG((1.-E*SIN(RLAT)/1.+E*SIN(RLAT))
     *        **(E/2.)*TAN(3.14159/4.+RLAT/2.))
         RSAMP = 1. + (REQ/RDATA(7)) * ((RDATA(6))/RAD)
         LINE(J) = RLINE+.5
         SAMP(J) = RSAMP+.5
         IF (RLINE .LT. 0.) LINE(J) = RLINE-.5
         IF (RSAMP .LT. 0.) SAMP(J) = RSAMP-.5
         IF (IADAPT .EQ. 1) GO TO 9
         SSTART(J) = SCP - SAMP(J)
         SSTOP(J) = SCP - SAMP(J) + NOSAMP(J) - 1
         IF (SSTOP(J) .LT. 0) SAMP(J) = SAMP(J) - CIRCUM
         GO TO 9

    8    CONTINUE

C     ORTHOGRAPHIC AND STEREOGRAPHIC PROJECTIONS

         LINE(J) = RDATA(2)+.5
         SAMP(J) = RDATA(1)+.5
         IF(RDATA(2).LT.0.)LINE(J) = RDATA(2)-.5
         IF(RDATA(1).LT.0.)SAMP(J) = RDATA(1)-.5
C     LPAR'S ARE LINES OF STANDARD PARALLELS
         IF(L.NE.5)GO TO 9
         LPAR1(J) = RDATA(4)+.5
         LPAR2(J) = RDATA(5)+.5
         IF(RDATA(4).LT.0.)LPAR1(J) = RDATA(4)-.5
         IF(RDATA(5).LT.0.)LPAR2(J) = RDATA(5)-.5

    9 call mp_free(mp)
      CALL XVCLOSE(INFIL(J),ISTAT,' ')

   99 CONTINUE
C
      nblocks = (numipt+5)/6
      do iblock=1,nblocks
        msgout='LONG= '
        ij=0
	i1 = (iblock-1)*6+1
	i2 = iblock*6
	if (i2.gt.numipt) i2 = numipt
        do i=i1,i2
          ij=ij+12
          write(msgout(ij:),'(E10.4)' ) tlong(i)
        enddo
        call xvmessage(msgout,' ')
        if (L.ne.6) then
	  msgout='SEARCW FOUND SAMP= '
	  ij=10
	  do i=i1,i2
	    ij=ij+10
	    write(msgout(ij:ij+3),'(I4)') samp(i)
	  enddo
	  call xvmessage(msgout,' ')
        endif
      enddo
C
C
C        CHECK FOR PICTURE COMPATABILITY
C
      CALL CHICK(IFLAG,REQCK,1,NUMIPT)
      CALL CHICK(IFLAG,RPOLCK,1,NUMIPT)
      CALL CHICK(IFLAG,TPROJ,1,NUMIPT)
      GO TO (10,10,10,10,20,30,40,40,35,35),L
C         ORTHOGRAPHIC AND STEREOGRAPHIC CHECK
   10 CALL CHICK(IFLAG,TLAT,2,NUMIPT)
      CALL CHICK(IFLAG,TLONG,2,NUMIPT)
      CALL CHICK(IFLAG,TSCALE,2,NUMIPT)
      CALL CHICK(IFLAG,TNOANG,2,NUMIPT)
      GO TO 40
C         LAMBERT CHECK
   20 CALL CHICK(IFLAG,TLAT1,2,NUMIPT)
      CALL CHICK(IFLAG,TLAT2,2,NUMIPT)
      CALL CHICK(IFLAG,TLONG,2,NUMIPT)
      CALL CHICK(IFLAG,TSCALE,2,NUMIPT)
      GO TO 40
C         MERCATOR CHECK
   30 CALL CHICK(IFLAG,TSAMP,2,NUMIPT)
      CALL CHICK(IFLAG,TLINE,2,NUMIPT)
      CALL CHICK(IFLAG,TSCALE,2,NUMIPT)
      GO TO 40
C         CYLINDRICAL CHECK
   35 CALL CHICK(IFLAG,TSCALE,2,NUMIPT)

   40 CONTINUE
      IF (IADAPT.EQ.0)GO TO 90
      CALL ADAPT(LINE,SAMP,NUMIPT,NLO,NSO,LCP,SCP,NOLINE,NOSAMP,L,
     +      CIRCUM, NOMIN)
      RLCP = LCP
      RSCP = SCP
      WRITE (MSG5,9900) LCP,SCP
9900  FORMAT (' THE PROGRAM CHOSE LCP=',I6,' AND SCP=',I6)
      CALL XVMESSAGE(MSG5(2:44),' ')
   90 CONTINUE
C
C  OPEN OUTPUT & ADD NEW MAP2 LABELS
C

	OFORM = 'BYTE'
	IF (ISWTCH.EQ.0) OFORM = 'HALF'
	CALL XVOPEN( OUTFIL, ISTAT, 'U_NL', NLO, 'U_NS', NSO, 'OPEN_ACT',
     &   'SA', 'IO_ACT', 'SA', 'OP', 'WRITE', 'U_FORMAT', 'HALF',
     &	 'O_FORMAT', OFORM,'U_NB',NBO,' ')
C
      DO  I = 1,39
         RDATAO(I) = RDATA(I)
      ENDDO
      if (l.ne.16) then
	RDATAO(1) = RSCP
	RDATAO(2) = RLCP
      endif

      call mp_init(mp,status)
      call mp_buf2mpo(rdatao,mp,status)

c  in Perspective case, if we change SCL,LCP, must change the OM-matrix!
      if (l.eq.16 .and. (rscp.ne.rdatao(34) .or.
     & rlcp.ne.rdatao(33))) then
	call mp_set_value( mp, 'PLANET_CENTER_LINE', rlcp, ind)
	call mp_set_value( mp, 'PLANET_CENTER_SAMPLE', rscp, ind)
	call mp_mpo2buf(mp,rdatao,status)
      endif

      call mp_label_write(mp,outfil,'PROPERTY',status)
      call mp_label_write(mp,outfil,'HISTORY',status)

c      IF (L .EQ. 6) THEN
C        Fixup Mercator label , find lat,long for line = samp = 1.
c        RDATAO(6) = RSCP/(REQ/(RDATAO(7)))*RAD
c        IF(RDATAO(6).GT.360.)RDATAO(6) = RDATAO(6)-360.
c        IF(RDATAO(6).LT.0.)RDATAO(6) = RDATAO(6)+360.
c        RDATAO(3) = RAD*(2*ATAN(EXP(RLCP/(REQ/RDATAO(7))))-3.14159/2)

c      ELSE IF ( L .EQ. 9 .OR. L .EQ. 10)  THEN
c        RDATAO(6) = 0.               ! FOR NORMAL OR SIMPLE CYLINDRICAL,
c        RDATAO(3) = 0.               ! LCP,SCP IS LAT 0, LONG 0.
c      ELSE
c        FLINE = LINE(NUMIPT)     ! USE CONVEV TO FIND THE LAT,LONG FOR LCP,SCP.
c        FSAMP = SAMP(NUMIPT)      

c  the above special cases were for peculiar requirements of maplabv2 
c  ... it makes more sense to apply the following to all projections:

	if (l.eq.16) then
	  fsamp = rdatao(34) 
	  fline = rdatao(33) 
	else
	  fsamp = RDATAO(1) 
	  fline = RDATAO(2) 
	endif

C        MODE = 2
c  map conversion routine -- CONVEV replaced by MP calls

c  for now use CONVEV for Perspective case as MP doesn't work yet
c	 if (l.eq.16) then
c	   call convev(ind,RDATAO,RDATAO,fline,fsamp,flat,flon,2,dum) 
c	   if (ind.ne.0) then
c             CALL MABEND('TP2 ERROR COMPUTING OFFSET')
c	   endif
c	 else
           call mp_xy2ll(mp,fline,fsamp,flat,flon,1,status)
	   IF(status .NE. mp_success) THEN 
	     msgout='Status returned by mp_xy2ll: '
	     write(msgout(30:35),'(I6)') status
	     call xvmessage(msgout,' ')
             CALL MABEND('TP2 ERROR COMPUTING OFFSET')
           END IF
c	 endif
         RDATAO(3) = FLAT
         RDATAO(6) = FLON
c      END IF
      call mp_free(mp)

C         Add new map2 labels to output
c  map routine conversion
C
      msgout='RPOL,REQ USED= '
      WRITE(MSGOUT(21:),'(E10.4)') RDATA(25)
      WRITE(MSGOUT(32:),'(E10.4)') RDATA(26)
      call xvmessage(msgout,' ')
 800  CONTINUE
C        Tell user how many lines and samples will be in output
      WRITE (MSG3,9910) NLO,NSO,NBO
9910  FORMAT (' THE OUTPUT PICTURE WILL CONTAIN',I6,' LINES, ',I6,
     +' SAMPLES AND ',I4,' BAND(S)')
      CALL XVMESSAGE(MSG3(2:83),' ') !63

 
  900 RETURN
      END

C*****************************************************************************
      SUBROUTINE CHICK(IND,INDATA,DCODE,NUMIPT)
C     THIS SUBROUTINE COMPARES ALL THE INPUTS TO THE FIRST TO SEE IF
C     THEY ARE MOSAICKABLE
      INTEGER INDATA(80),DCODE

      IF(NUMIPT.EQ.1) RETURN
      DO  I = 2,NUMIPT
         IF(INDATA(1).NE.INDATA(I))THEN
            CALL PRNT(4,1,I,'***DATA INCOMPATIBLE,INPUT NUMBER=.')
            IND = 1
         ENDIF
      ENDDO
      RETURN
      END

C*****************************************************************************
      SUBROUTINE MOSB(INBUF,LENGTH,OBUF,NSB,NUMBUF,NS4,
     &	   AVGBUF,NS4j,INARR,NS4I,LENCHK)
      INTEGER*2 INBUF(LENGTH),OBUF(NSO)
      REAL*4 AVGBUF(NSO),NUMBUF(NSO)

      COMMON/C1/PAR,NUMIPT,NUMLIN,LSET,RSET,LTHRES,RTHRES,LSEQ,
     + RSEQ,LNIB,RNIB,LNIBST,RNIBST,NSEQ,ITHRES,ITHRST,BLKSIZ,NSI,
     + LINE,SAMP,SCP,NSO,NIBB,NIBBST,NSEQST,RSEQST,LSEQST,LCP,
     + LTHRST,RTHRST,NOSAMP,NOLINE,REFIN,DCLE,INCR,ISWTCH,NLO,
     + IAVER,ISMOOTH,NLR,REQ,RPOLE,LAB,weight,NOBAND,JSB,NBO,NBI
      INTEGER RSET,RTHRES,RSEQ,RNIB,RNIBST,BLKSIZ,RSEQST,RTHRST
      real*4 weight(80)
	 INTEGER*4 JSB,NBO,NBI
      COMMON/C2/N,I,KK,LABO,J,IND,OREC,NK,M,K,NUML,NUMR,NSAM,REFINL,
     +	II,IL,REFINR,IR,REND,LEND,L,LL,LINO,RDN,RNUM
      INTEGER OREC,REFINL,REFINR,REND
      INTEGER LINE(80),SAMP(80),SCP,DCLE,NOLINE(80),PAR(100),
     +	    NOSAMP(80),NOBAND(80),LAB(80),REFIN(80)
      COMMON/C3/LSTART(80),LSTOP(80),SSTART(80),SSTOP(80),TSTART(80)
      INTEGER SSTART,SSTOP,TSTART
	 INTEGER*4 BAND
      COMMON/FILES/INFIL,OUTFIL
      INTEGER infil(80), OUTFIL

      CHARACTER*132 MSG
      character*80 msgout
      INTEGER FLAG(80), FILEOPEN(80), NUM_FILESOPEN
	 INTEGER*4 BANDOUT,LINEOUT

      INTEGER INARR(NSO), INOVER(80)
 1111	format(A13,I6)

      NUM_FILESOPEN = 0
      CALL ZIA(INOVER,80)
      CALL ZIA(FLAG,80)
      CALL ZIA(FILEOPEN,80)
      CALL ZIA(INARR,NSO)
      IF (LENCHK.NE.LENGTH+NSB+NS4+NS4) GOTO 217
C
C     ....Resolve nibble parameter hierarchy
      IF (NIBBST.NE.0) THEN
         IF (LNIBST.EQ.0) LNIB=NIBB
         IF (RNIBST.EQ.0) RNIB=NIBB
         IF (NIBBST.EQ.1) LNIBST=1
         IF (NIBBST.EQ.1) RNIBST=1
      ENDIF
C
C     ....Resolve threshold parameter hierarchy
      IF (ITHRST.NE.0) THEN
         IF (LTHRST.EQ.0 .AND. ITHRST.EQ.1) LTHRES=ITHRES
         IF (RTHRST.EQ.0 .AND. ITHRST.EQ.1) RTHRES=ITHRES
      ENDIF
      IF (NSEQST.NE.0) THEN
         IF (LSEQST.EQ.0) LSEQ=NSEQ
         IF (RSEQST.EQ.0) RSEQ=NSEQ
      ENDIF
C     ....Compute amount of nibbling
      IF (LNIBST.NE.1) THEN
         LSET = (LSEQ-1)*INCR
      ELSE
         LSET = (LSEQ-1)*INCR-LNIB
      ENDIF
      IF (RNIBST.NE.1) THEN
         RSET = (RSEQ-1)*INCR
      ELSE
         RSET = (RSEQ-1)*INCR-RNIB
      ENDIF
      msgout='LTHRES,RTHRE= '
      write(msgout(17:22),'(I6)' ) LTHRES
      write(msgout(27:32),'(I6)' ) RTHRES
      call xvmessage(msgout,' ')
	
      msgout='ITHRES= '
      write(msgout(17:22), '(I6)' ) ITHRES
      call xvmessage(msgout,' ')
      msgout='DCLE= '
      write(msgout(15:20), '(I6)' ) DCLE
      call xvmessage(msgout,' ')
C

C     ....Determine offsets of inputs in output picture
      DO J=1,NUMIPT
         LSTART(J) = LCP-LINE(J)+1	!OUTPUT LINE FOR LINE 1 OF INPUT J
         LSTOP(J) = LCP-LINE(J)+NOLINE(J) !LINE NL(J)
         SSTART(J) = SCP-SAMP(J)+1	!SAMP 1 OF J
         SSTOP(J) = SCP-SAMP(J)+NOSAMP(J) !SAMP NS(J)
         TSTART(J) = SSTART(J)-REFIN(J)
         IF (LSTART(J).GT.NLO .OR. LSTOP(J).LT.1 .OR.
     &		SSTOP(J).LT.1 .OR. SSTART(J).GT.NSO) THEN
            WRITE (MSG,9920) J,LSTART(J),SSTART(J)
9920  FORMAT (' INPUT ',I2,' DOES NOT LIE IN OUTPUT FIRST LINE =',I6,
     +' FIRST SAMPLE =',I6)
            CALL XVMESSAGE(MSG(2:72),' ')
         ENDIF
      ENDDO
C
      call xvmessage(' ',' ')
      nblocks = (numipt+5)/6
      do iblock=1,nblocks
	i1 = (iblock-1)*6+1
	i2 = iblock*6
	if (i2.gt.numipt) i2 = numipt
        j=6
        msgout='LSTART= '
        do i=i1,i2
	  j=j+11
          write(msgout(j:J+3), '(I4)') LSTART(i)
        enddo
        call xvmessage(msgout,' ')
        j=6
        msgout='LSTOP= '
        do i=i1,i2
	  j=j+11
          write(msgout(j:J+3), '(I4)') LSTOP(i)
        enddo
        call xvmessage(msgout,' ')
        j=6
        msgout='SSTART= '
        do i=i1,i2
	  j=j+11
          write(msgout(j:J+3), '(I4)') SSTART(i)
        enddo
        call xvmessage(msgout,' ')
        j=6
        msgout='SSTOP= '
        do i=i1,i2
	  j=j+11
          write(msgout(j:J+3), '(I4)') SSTOP(i)
        enddo
        call xvmessage(msgout,' ')
        call xvmessage(' ',' ')
      enddo
C
      numip=numipt
      if(ismooth.eq.1) numip=numipt/2 ! Only use half the input images
C

C     ....Start of band loop
	 BANDOUT = 0
	 DO 250 BAND=JSB,JSB+NBO-1
	    BANDOUT = BANDOUT + 1
	    LINEOUT = 0
C     ....Start of line loop
      DO 200 LINO=1,NLO			!LINO is line number in output
         OREC = LINO + NLR
	    LINEOUT = LINEOUT + 1
	 do ii=1,nso
	    obuf(ii)=DCLE
	    numbuf(ii)=0.0
	    avgbuf(ii)=0.0
	 enddo
C
C     ....Read in the input lines
      DO J=1,NUMIPT
C         CALL XVUNIT(INFIL(J),'INP',J,ISTAT,' ')
C         CALL XVOPEN(INFIL(J),ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
C     +          'U_FORMAT','HALF',' ')
         IF (SSTOP(J).GE.1 .AND. SSTART(J).LE.NSO .AND.
     &        LINO.GE.LSTART(J) .AND. LINO.LE.LSTOP(J)) THEN
            IF (FILEOPEN(J) .EQ. 0) THEN
              
              CALL XVOPEN(INFIL(J),ISTAT,'OPEN_ACT','SA',
     +          'IO_ACT','SA','U_FORMAT','HALF',' ')
              FILEOPEN(J) = 1
              NUM_FILESOPEN = NUM_FILESOPEN + 1
            ENDIF
            FLAG(J) = 0		!Output line is in input j
            CALL XVREAD(INFIL(J),INBUF(REFIN(J)),ISTAT,
     &        'NSAMPS',NOSAMP(J),'LINE',LINO-LSTART(J)+1,
     &	    'BAND',BAND,' ')
         ELSE
            FLAG(J) = 1		!Output line does not lie in input
         ENDIF
C
C 45 is the max number of file that can be open. This will actually
C vary on different machines, but 45 is a good number to start with

C IF OUTPUT LINE DOES NOT LIE IN INPUT IMAGE AND ALSO THAT IMAGE IS
C OPEN THEN CLOSE THAT IMAGE FILE. ELSE IF 45 FILES HAVE BEEN OPEN
C THEN FIND A FILE THAT IS OPEN AND CLOSE IT.

         IF (FLAG(J) .EQ. 1 .AND. FILEOPEN(J) .EQ. 1) THEN
           CALL XVCLOSE(INFIL(J),ISTAT,' ')
           FILEOPEN(J) = 0
           NUM_FILESOPEN = NUM_FILESOPEN - 1
         ELSE IF (NUM_FILESOPEN .GE. 45) THEN
           DO M=1,NUMIP
             IF (FILEOPEN(M) .EQ. 1) GOTO 101
           ENDDO
           GOTO 299
 101	   CALL XVCLOSE(INFIL(M),ISTAT,' ')
           FILEOPEN(M)=0
           NUM_FILESOPEN=NUM_FILESOPEN-1
         ENDIF
      ENDDO
      if (num_filesopen .lt. 0) then
        call xvmessage('*******************It is here',' ')
      endif
C

C     ....All input data sets are now in buffer
      DO 80 M=1,NUMIP
      K = NUMIP - M + 1	!Look at last input data set first
      IF (FLAG(K).EQ.1) GOTO 80 !Skip if output line is not in input
      NUML = 0
      NUMR = 0
      NSAM = NOSAMP(K)
C     ....Search for left threshold trigger
      REFINL = REFIN(K) - 1
      DO I=1,NSAM,INCR
         IL = I + REFINL
         IF (INBUF(IL).LT.LTHRES) THEN
            NUML = 0
         ELSE
            NUML = NUML+1
            IF (NUML.GE.LSEQ) GOTO 30
         ENDIF
      ENDDO
      GOTO 80		!Skip if trigger not found
C

C     ....Look for right threshold trigger
   30 REFINR = NSAM + REFIN(K)
      DO I=1,NSAM,INCR
         IR = REFINR - I
         IF (INBUF(IR).LT.RTHRES) THEN
            NUMR = 0
         ELSE
            NUMR = NUMR + 1
            IF (NUMR.GE.RSEQ) GOTO 50
         ENDIF
      ENDDO
      GOTO 80		!Skip if trigger not found
C

C     ...,Both left and right triggers have been found
C     ....Check that more than 2*nseq triggers exist
   50 IF (IR-IL.LT.LSEQ+RSEQ) GOTO 80
C     ....A valid threshold has been found
      REND = IR+RSET
      LEND = IL-LSET

      IF (IAVER.EQ.1) THEN	!Use averaging routine
	DO L=LEND,REND
          LL = L+TSTART(K)
          IF (LL.GT.0 .AND. LL.LE.NSO .AND. INBUF(L).GE.ITHRES) THEN
	    AVGBUF(LL) = INBUF(L)*weight(k) + AVGBUF(LL)
	    NUMBUF(LL) = NUMBUF(LL) + weight(k)
	  ENDIF
	ENDDO

      ELSE IF (ISMOOTH.EQ.1) THEN	!Use weighting routine
	DO L=LEND,REND
          LL = L+TSTART(K)
          IF (LL.GT.0 .AND. LL.LE.NSO .AND. INBUF(L).GE.ITHRES) THEN
            lmask=L-refin(k)+refin(k+numip) ! corresponding mask pixel
	    AVGBUF(LL) = real(INBUF(L))*real(inbuf(lmask))*weight(k) + 
     +                   AVGBUF(LL)
	    NUMBUF(LL) = NUMBUF(LL) + inbuf(lmask)*weight(k)
	  ENDIF
	ENDDO

      ELSE                      ! First in gets priority option
	DO L=LEND,REND
	  LL = L+TSTART(K)	!Add in offsets
	  IF (LL.GT.0.AND.LL.LE.NSO) THEN
	    IF (INBUF(L).GE.ITHRES) OBUF(LL)=INBUF(L)
	  ENDIF
	ENDDO
      ENDIF

   80 CONTINUE

c  now do smoothing and/or averaging ...
      IF (IAVER.EQ.1.or.ismooth.eq.1) THEN	!Find weighted average
	DO I=1,NSO
	  IF (NUMBUF(I).NE.0.0) THEN
	    OBUF(I) = nint(avgbuf(i)/numbuf(i))
	  ELSE
	    OBUF(I) = DCLE
	  ENDIF
	ENDDO
      ENDIF

C     ....Write output data set
c      CALL XVWRIT(OUTFIL,OBUF,ISTAT,'LINE',OREC,' ')
	 CALL XVWRIT(OUTFIL,OBUF,ISTAT,'LINE',LINEOUT,'BAND',BANDOUT,' ')
C
  200 CONTINUE
 250	 CONTINUE
      RETURN

  217 CALL XVMESSAGE('***Not enough memory',' ')
      CALL ABEND
 299	CALL XVMESSAGE('***UNKNOWN ERROR***',' ')
        CALL XVMESSAGE('***ACCORDING TO NUM_FILESOPEN THERE',' ')
        CALL XVMESSAGE('   THE LIMIT OF OPEN FILES HAVE BEEN',' ')
        CALL XVMESSAGE('   REACHED. BUT ACCORDING TO FILEOPEN',' ')
        CALL XVMESSAGE('   THERE IS NO OPEN FILE!!!!!',' ')
        CALL ABEND
      END

C*****************************************************************************
      SUBROUTINE ADAPT(LINE,SAMP,NUMIPT,NLO,NSO,LCP,SCP,NOLINE,NOSAMP,
     & L,CIRCUM, NOMIN)
C
C 12/29/82 -JAM- ORIGINAL CODE BEFORE MODS
C 15-JAN-87 ...LWK... FIXED CODE FOR CASE OF LONGITUDE OVERLAP
C 11-NOV-87 ...LWK... FIXED BUGS IN THE 'IRANK' DETERMINATION
C 12/30/87  ...SP.... ADDED NOMIN KEYWORD, ADDED 1 TO SMAX FOR CASE OF NO GAP.
C 10-July-95...CRI... MSTP S/W CONVERSION (VICAR PORTING)
C
C  THIS SUBROUTINE DETERMINES THE SIZE OF A MOSAIC.
C  CHOOSE LCP AND SCP, NLO, AND NSO TO CONTAIN INPUT PICTURES
C  COMMON POINT IS CHOSEN TO BE AT THE MAXIMUM (LINE,SAMP) IN THE OUTPUT.
C
      COMMON/C4/I,J,INLO,INSO,LMAX,LMIN,SMAX,SMIN,ISTART
      INTEGER CIRCUM
      LOGICAL NOMIN
      INTEGER LINE(80),SAMP(80),NOSAMP(80),NOLINE(80),SCP,SMAX,SMIN,
     & IRANK(80)
C
      CALL ZIA(I,9)
      IFLAG = 0
      IF (L.LT.6 .OR. L.EQ.7 .OR. L.EQ.8 .OR. L .GT. 10 ) GOTO 40
      IF (NOMIN) GO TO 40

C  CHECK CYLINDRICAL TYPE PROJECTIONS FOR WRAPAROUND.
C  DETERMINE GAPS IN OVERLAP IN SAMPLE (LONGITUDE) DIRECTION,
C  AND PUT START OF OUTPUT IMAGE AT EDGE OF (LARGEST) GAP:
C
C  INITIALIZE RANKING ARRAY, AND ADD OFFSET TO ANY IMAGES THAT
C  CONTAIN LONG=0.
C  (IRANK(I) = IMAGE NUMBER THAT IS RANKED NUMBER I)
      DO I = 1,NUMIPT
	IRANK(I) = I
      ENDDO
C  REMOVE FROM CONSIDERATION ANY IMAGES THAT LIE ENTIRELY
C  INSIDE ANOTHER IMAGE (IN LONG.).
      NUMI = NUMIPT
      DO I = 1,NUMIPT
	IF (I.GT.NUMI) GO TO 20
	II = IRANK(I)
        DO J = 1,NUMI
	  IF (I.EQ.J) GO TO 10
5	  JJ = IRANK(J)
          IF ((SAMP(II).LE.SAMP(JJ)) .AND.
     &     (SAMP(II)-NOSAMP(II).GE.SAMP(JJ)-NOSAMP(JJ))) THEN
	    IF (I.EQ.NUMI) THEN	! IF IT'S THE LAST IMAGE
	      NUMI = NUMI-1	! ... JUST REMOVE IT
	      GO TO 20		! ... AND QUIT
	    ENDIF
	    ISAVE = IRANK(NUMI)
	    IRANK(NUMI) = II	! ASSIGN TO II RANK NUMI
C  ASSIGN RANK I TO JJ RATHER THAN SIMPLY SWITCHING NUMI & I, BECAUSE
C  WE CAN INFER THAT IMAGE JJ DOES NOT LIE (IN SAMP DIMENSION) INSIDE
C  IMAGES OF RANKS 1,..,J-1, BUT THIS IS NOT NECESSARILY TRUE OF ISAVE.
	    IRANK(I) = JJ	
	    II = JJ		! REDEFINE THIS TEMPORARY VARIABLE
	    IRANK(J) = ISAVE
	    NUMI = NUMI-1	! EXCLUDE RANK NUMI
	    GO TO 5		! NOW WE MUST CHECK THE NEW J
	  ENDIF
10	  CONTINUE
	ENDDO
      ENDDO

C  SORT IMAGES IN DECR'G SAMPLE DIRECTION:
20    DO I = 1,NUMI-1
	II = IRANK(I)
	DO J = I+1, NUMI
	  JJ = IRANK(J)
	  IF (SAMP(II) .LT. SAMP(JJ)) THEN
	    IRANK(I) = JJ
	    IRANK(J) = II
	    II = IRANK(I)
	  ENDIF
	ENDDO
      ENDDO
C  NOW FIND LARGEST GAP (LGAP) AND THE IMAGE RANK # FOLLOWING IT (IGAP):
      LGAP = 0
      IGAP = 0
      DO I = 1,NUMI
	IF (I.EQ.1) THEN
	  J1 = IRANK(NUMI)
	ELSE
	  J1 = IRANK(I-1)
	ENDIF
	J2 = IRANK(I)
	LG = SAMP(J1)-NOSAMP(J1)-SAMP(J2)
	IF (SAMP(J1).LT.SAMP(J2)) LG = LG+CIRCUM
	IF (LG.GT.LGAP) THEN
	  LGAP = LG
	  IGAP = I
	ENDIF
      ENDDO
C
      IF (IGAP.EQ.0) THEN		!NO LONG. GAPS:  SET EDGE AT LONG.=0
	SMAX = CIRCUM+1
	SMIN = 1
      ELSE
	SMAX = SAMP(IRANK(IGAP))
	SMIN = SMAX+LGAP
	IF (SMIN.GT.SMAX) THEN
	  SMIN = SMIN-CIRCUM
	  DO I = 1,NUMIPT
            IF (SAMP(I).GT.SMAX) SAMP(I) = SAMP(I)-CIRCUM
	  ENDDO
	ENDIF
      ENDIF
C
C  DETERMINE LINE RANGE:
      LMAX = LINE(1)
      LMIN = LINE(1) - NOLINE(1)
      DO  I = 2, NUMIPT
        IF (LINE(I).GT.LMAX) LMAX = LINE(I)
        IF (LINE(I)-NOLINE(I).LT.LMIN) LMIN = LINE(I)-NOLINE(I)
      ENDDO
C
      GO TO 50
C
C  ALIGN ALL INPUTS RELATIVE TO COMMON POINT TO DETERMINE TOTAL
C  SIZE OF OUTPUT:
   40 LMAX = LINE(1)
      LMIN = LINE(1) - NOLINE(1)
      SMAX = SAMP(1)
      SMIN = SAMP(1) - NOSAMP(1)
      DO  I = 2, NUMIPT
        IF (LINE(I)-NOLINE(I).LT.LMIN) LMIN = LINE(I)-NOLINE(I)
        IF (LINE(I).GT.LMAX) LMAX = LINE(I)
        IF (SAMP(I)-NOSAMP(I).LT.SMIN) SMIN = SAMP(I)-NOSAMP(I)
        IF (SAMP(I).GT.SMAX) SMAX = SAMP(I)
      ENDDO
C
C  FIND LCP AND SCP.  IMAGE FURTHEST TO THE LEFT SHOULD START IN SAMPLE 1.

   50 LCP = LMAX
      SCP = SMAX
C
c  FIND NLO AND NSO NEEDED TO HOLD ALL FRAMES, SUBJECT TO MAXIMA
C  SPECIFIED BY USER:
      INLO = LMAX - LMIN
      INSO = SMAX - SMIN
      IF (NLO.LT.INLO) THEN
	CALL XVMESSAGE('***LINES TRUNCATED',' ')
      ELSE
	NLO = INLO
      ENDIF
      IF (NSO.LT.INSO) THEN
	CALL XVMESSAGE('***SAMPLES TRUNCATED',' ')
      ELSE
	NSO = INSO
      ENDIF
C
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create newmos.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM newmos

   To Create the build file give the command:

		$ vimake newmos			(VMS)
   or
		% vimake newmos			(Unix)


************************************************************************/


#define PROGRAM	newmos
#define R2LIB

#define MODULE_LIST newmos.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST fortport mp_for_defs

#define LIB_MATH77
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_SPICE
/* #define LIB_LOCAL */
/************************* End of Imake file ***************************/





$ Return
$!#############################################################################
$PDF_File:
$ create newmos.pdf
process help=*
PARM INP    TYPE=STRING      COUNT=(1:80)
PARM OUT    TYPE=STRING      COUNT=1
PARM SIZE   TYPE=INTEGER     COUNT=4                         DEFAULT=(1,1,0,0)
PARM SL     TYPE=INTEGER     COUNT=1                         DEFAULT=1
PARM SS     TYPE=INTEGER     COUNT=1                         DEFAULT=1
PARM SB     TYPE=INTEGER     COUNT=1                         DEFAULT=1
PARM NL     TYPE=INTEGER     COUNT=1                         DEFAULT=0
PARM NS     TYPE=INTEGER     COUNT=1                         DEFAULT=0
PARM NB     TYPE=INTEGER     COUNT=1                         DEFAULT=0
PARM PIXL   TYPE=INTEGER     COUNT=(2:80)                    DEFAULT=(0,0)
PARM LCP    TYPE=INTEGER                                     DEFAULT=0
PARM SCP    TYPE=INTEGER                                     DEFAULT=0
PARM THRESH TYPE=INTEGER     VALID=(-32767:32768)            DEFAULT=1
PARM LTHRESH TYPE=INTEGER    VALID=(-32767:32768)            DEFAULT=1
PARM RTHRESH TYPE=INTEGER    VALID=(-32767:32768)            DEFAULT=1
PARM ETHRESH TYPE=INTEGER  COUNT=0:1  VALID=(0,-32767:32768) DEFAULT=--
PARM NSEQ   TYPE=INTEGER                                     DEFAULT=1
PARM LSEQ   TYPE=INTEGER                                     DEFAULT=1
PARM RSEQ   TYPE=INTEGER                                     DEFAULT=1
PARM ADAPT  TYPE=KEYWORD     COUNT=0:1     VALID=ADAPT       DEFAULT= --
PARM NOMIN  TYPE=KEYWORD     COUNT=0:1     VALID=NOMIN       DEFAULT= --
PARM NIBB   TYPE=INTEGER                                     DEFAULT=0
PARM LNIB   TYPE=INTEGER                                     DEFAULT=0 
PARM RNIB   TYPE=INTEGER                                     DEFAULT=0
PARM DCLEV  TYPE=INTEGER                                     DEFAULT=0
PARM INCR   TYPE=INTEGER                   VALID=(1:9999999) DEFAULT=10
PARM FORMAT TYPE=KEYWORD     COUNT=0:2     VALID=(BYTE,HALF) DEFAULT=--
PARM AVER   TYPE=KEYWORD     COUNT=0:1     VALID=AVER        DEFAULT= --
PARM MAP2   TYPE=KEYWORD     COUNT=0:1     VALID=MAP2        DEFAULT= --
PARM RADIUS TYPE=REAL                                        DEFAULT=0.
PARM REQ    TYPE=REAL                                        DEFAULT=0.
PARM RPOL   TYPE=REAL                                        DEFAULT=0.
PARM TARGET TYPE=KEYWORD     COUNT=0:1      DEFAULT= --  +
 VALID=(MOON,MCRY,IO,J1,EURO,J2,GANY,J3,CALL,J4,JUPI)
PARM SMOOTH TYPE=KEYWORD     COUNT=0:1      VALID=SMOOTH    DEFAULT=--
PARM WEIGHT TYPE=REAL        COUNT=(0:80)                   DEFAULT=--
!
!# parm inp(3-80) hints=default
!
END-PROC
.TITLE
  NEWMOS
.HELP
 PURPOSE:

 NEWMOS is a VICAR application program which is able to mosaic up to
 thirty pictures.  If the frames have been map- projected using VICAR
 program MAP3, then NEWMOS can automatically determine the required
 size of the output image and the offsets of all the input images.

 EXECUTION:
   There are two ways to call newmos. The simplest mosaics or averages
   all of the inputs:

        NEWMOS  INP=(inputs...)  OUT=output  SIZE  PARAM

   The second mode includes an additional set of mask images:

        MASKMOS INP=(inputs...)  OUT=(outputs...)
        NEWMOS  INP=(inputs...,outputs...)  OUT=output  SIZE  PARAM

.page  
 OPERATION:

 NEWMOS calculates where each input picture is to be replaced in the 
 output frame from the offset parameters. The program takes each line
 of the output picture and determines which input frames have lines
 which map to this line in the output. The program scans along these
 input lines from left to the right and right to the left using the 
 edge detection parameters to find the left and right extents of
 valid picture data in each of the inputs. The data between these
 points whose DN are greater than THRESH, N1 are copied to the output.

 There are three methods of weighting the input images:

Default method:

 In reverse input data set order with an earlier input overlaying
 a subsequent input much as a person mosaicking prints by hand
 will put one print on top of another (with IN1 on top). 
 No weighting is performed.
 In this method ALL THE INPUT IMAGES ARE MOSAICKED which is NOT the case
 for the SMOOTH option.

Average method:  (AVER keyword)
 
 All the images which overlap are averaged to produce the output. 
 Weighting is performed based upon the WEIGHT keyword.
 DNout=sum(DNin*WEIGHTin)/sum(WEIGHTin).
 In this method ALL THE INPUT IMAGES ARE MOSAICKED which is NOT the case
 for the SMOOTH option.

Smooth method:  (SMOOTH keyword)

 Only THE FIRST HALF OF THE INPUT IMAGES ARE MOSAICKED. The second half 
 are masks which control the weighting for the first half. The masks
 can be generated automatically with the MASKMOS program. There is
 a 1:1 correspondence between images such that for M inputs image
 M/2+1 is the mask for image 1, image M/2+2 is the mask for image 2, etc.
 The output DN value is the sum of overlapping frames weighted by their
 mask values at that point including the WEIGHT keyword.
 DNout=sum(DNin*MASKin*WEIGHTin)/sum(MASKin*WEIGHTin).
 Note that the MAP3 labels must be present for the masks too.

In general:

 All data
 in an input picture which lies outside of the output frame are
 ignored.  If from the offset parameters and the size field any
 input has been determined to lie entirely outside the output 
 frame, a warning is printed and that frame is discarded. If
 no input data has been provided to cover a particular pixel
 in the output, the program will set that output pixel to DCLEVEL.

 NEWMOS can compute the output image size and the offsets of the input
 images automatically, if the inputs have been map-projected using
 MAP3.  This option is invoked by the keyword ADAPT.

.PAGE
 
 PROGRAM HISTORY:

 WRITTEN BY: Joel Mosher        March, 12 1976
 CONVERTED TO VAX BY:  Florance Moss   Feburary, 1 1984
 CONVERTED TO VICAR2 BY:  L.W.Kamp,    27 Oct. 1985
 COGNIZANT PROGRAMMER:  L. W. Kamp
 REVISIONS:
  18 AUG 03  ntt  Enabled 3D image capability.
  02 FEB 99  rrp/hbm Modified newmos to work with upto 80 input images.
                  The current solaris system only supports a maximum
                  of 64 input files so newmos was modified to close
                  unnecessary files or close file if a max of 45 have
                  been reached.
  04 Jun 98  rrp  Removed implicit variables from main44. Declared RLCP and
                  RSCP as real*8. Also changed loop parameter in OFFSET from
                  mp_max_keywd_length to mp_number_of_keywords. Prevented
                  the use of tlat vector in any test other then CHICK.
                  Updated the call to xvparm for weight to have maxcnt of
                  60 and not 1. Initialized all variables to avoid unexpected
                  program behavior.
  10 Jul 95  crs (CRI) made portable for UNIX
  1  mar 93  jjl  New smooth option.
  22 Feb 93  LWK  Fixed AVER (when no SMOOTH) bug
  20 Feb 93  LWK  Use TRANV vice CONVEV for Simple Cylindrical because of
                  RECTPATCH problem when ADD360 has been used
  11 Dec 92  LWK  Call CONVEV instead of TRANV in order to support PERSPECTIVE
  16 Oct 92  LWK  Enabled map codes 10-16 (but NO special code added to
                  handle these -- use at your own risk!);  revised SMOOTH
		  algorithm to ignore regions where >2 images overlap
  20 Nov 91  LWK  Removed NOMIN code from Simple Cylindrical (not needed as
                   TRANV does not truncate). Also added 1 to SAMP for Mercator.
  13 Nov 91  LWK  Used NOMIN to suppress call to TRANV, in order to allow 
                   range beyond (0,360) for Cylindrical
  ???        JRY  Added parameter SMOOTH
  07 Aug 89  GMY  Fix integer overflow when averaging pixels
  22 Feb 88  SP   COMPLETED CHANGE OF 87-12-8.  ADDED 1 TO SAMP FOR 
                      NORMAL CYLINDRICAL.
  22 Feb 88  SP   CORRECTED MAP LABEL INFO FOR OUTPUT FILE.
  22 Feb 88  SP   ADDED NOMIN KEYWORD TO ALLOW MORE THAN 360 DEGREES
                      OF LONGITUDE FOR CERTAIN PROJECTIONS.  
  11 Dec 87  SP   CORRECTED SUBR. OFFSET TO USE TRANV IF LAT AND LONG
                      IN MAP2 LABEL ARE VARIABLE.
  08 Dec 87  LWK  FIXED PROBLEM WITH HALFWORD DATA:  SINCE ALL WORK
                      IS DONE IN HALFWORD, REMOVE ALL REFERENCE TO BYTES!
  30 Oct 87  LWK  INITIALIZE 'WORK' ARRAY FOR SEARCV2, REMOVED TBUF STUFF
  11 Oct 87  LWK  FIXED BUGS READING IMAGE WITH LSTART<0, & IN ADAPT
  27 Mar 87  SP   FIXED PROBLEM WHERE INPUT IMAGES WERE ALL OFFSET 1 PIXEL
                      TO THE RIGHT OF CORRECT POSITION.
  15 Jan 87  LWK  FIXED BUG IN SUBR. ADAPT
  26 Mar 86  LWK  FIXED ISWTCH BUG IN SUBR.OFFSET
  27 Oct 85  LWK  CONVERTED PARAMETER PROCESSING TO VICAR2 
                      (REQUIRED BECAUSE NI>10)
  15 Oct 85  LWK  FINISHED VICAR2 CONVERSION (I/O ONLY) 
                      INCREASE # INPUT FILES TO 30
  26 Sep 85  HBD  CONVERTED I/O TO VICAR2
  24 Jan 84  FFM  CONVERT TO VAX
  Joel Mosher changes:
     10/8/75 FIX BUG IN WRITEING LABEL INTO PIX DATA
     10/16/75 PUT IN INCREMENT IN EDGE SEARCH
     10/30/75 FIX BUG IN BCDBIN(NOSAMP CONFUSED WOTH NOLINE)
     12/12/75 OS VERSION SET ALL VARIABLES TO ZERO
     1/26/76 FIX BUG IN INITIALIZING NSI
     1/26/76 PUT IN ERROR MESSAGE FOR NUMBER OF OFFSETS VS INPUTS
     6/2/76 FIX BUG IN ALLOCATION OF BUFFER SPACE
     1/29/78 PUT IN LABEL SEARCHING FOR OFFSETS
     1/30/78 LET SYSTEM ALLOCATE OUTPUT ALTERNATE BUFFER
     2/7/78 FIXED NIBBLE MISTAKE ,LNIB&RNIB REVERSED
     2/7/78 PUT IN THRESH TEST FOR OUTPUT BUFFER
     2/7/78 CHANGED THRESHOLDING
     2/19/78 PUT IN MAP2 LABEL UPDATE
     2/20/78 PUT IN PICTURE COMPATABILITY CHECKING
     3/26/78 PUT IN AVERAGING OPTION
     8/9/78 OVERLAID
     8/14/78 FIXED HALFWORD WRITE BUG
     12/31/78 PUT IN JUPITER RADIUS
     2/10/79 PUT IN CYLINDRICAL PROJECTION
     5/27/80 FIXED BUG IN OVERRIDE OF RADII
     12/28/80 USE 77 LABEL FORMAT TO GET NL AND NS
     12/29/80 CHANGE ALL CLOSES FROM 0 TO 1 TO BACKSPACE POSSIBLE
             TAPE INPUT DATA SETS
     4/19/81 PUT IN NORMAL CYLINDRICAL PROJECTION
     7/17/81 CHANGE RADII OF JUPITERS SATELLITES
     11/25/81 READ FORMAT PARAMTER IN SYSTEM LABEL AGAIN
     12/23/81 FIX ERROR IN HALF/BYTE SWITCH
     6/20/82 FIX WRAPAROUND PROBLEM IN ADAPT SUBROUTINE
     12/23/82 PUT IN FIX TO GIVE SCP LESS THAN 1 CIRCUMFERENCE
     2/20/83 FIX WRAPAROUND PROBLEM IN ADAPT AGAIN

.LEVEL1 
.VARIABLE INP
  input VICAR labelled files
.VARIABLE OUT
  output VICAR labelled file
.VARIABLE SIZE
  standard VICAR size field
.VARIABLE SL
  starting line of the SIZE
  field
.VARIABLE SS
  starting sample of the size
  field
.VARIABLE SB
  starting band of the size
  field
.VARIABLE NL
  number of lines of the size 
  field
.VARIABLE NS
  number of samples of the size
  field.
.VARIABLE NB
  number of bands of the size
  field.
.VARIABLE PIXL
  locations of the common point
  in the input files
.VARIABLE LCP
  the line of the common point
  in OUT
.VARIABLE SCP
  the sample of the common point
  in OUT
.VARIABLE THRESH
  the lower DN used in finding
  the input's edge
.VARIABLE LTHRESH
  left threshold DN value
.VARIABLE RTHRESH
  right threshold DN value
.VARIABLE ETHRESH
  both the left and right
  thresholds
.VARIABLE NSEQ
  the number of consecutive
  elements >= THRE
.VARIABLE LSEQ
  the number of consecutive
  elements >= LTHR 
.VARIABLE RSEQ
  the number of consecutive
  elements >= RTHR
.VARIABLE ADAPT
 Common points and output image
 size are computed automatically.
.VARIABLE NOMIN
 Used to prevent minimizing the
 output number of samples.
.VARIABLE NIBB
  the number of edge pixels to
  be nibbled
.VARIABLE LNIB
  the number of left edge pixels
  to be nibbled
.VARIABLE RNIB
  the number of right edge 
  pixeles to be nibbled
.VARIABLE DCLEV
  the background DN value
.VARIABLE INCR
  sample increment to find
  an edge
.VARIABLE FORMAT
  output file data format
.VARIABLE AVER
  average the DN in the output
  overlapping area
.VARIABLE MAP2
  indicates all inputs have MAP2
  labels.
.VARIABLE RADIUS
  specifies the radius of the
  target planet.
.VARIABLE REQ
  specifies the equatorial
  radius of the body.
.VARIABLE RPOL
  specifies the polar radius of
  the body.
.VARIABLE TARGET
  Target body.
  Valid: MOON,MCRY,IO,J1,
  EURO,J2,GANY,J3,CALL,J4.
.VARIABLE SMOOTH
  the overlapping area is
  weighted in its calculation
.VARIABLE WEIGHT
  Weight for each input.
  Defaults to 1.
.LEVEL2
.VARIABLE INP
  input file names (can be byte,halfword). The order
  in which the input data sets are specified determines
  the order of preference of the data at any pixel, i.,e.,
  the data in INP(1) has precedence over all other data
.VARIABLE OUT
  output file name (can be byte, halfword)
.VARIABLE SIZE
  the standard VICAR size field (only NL and NS are used
  and refer to the output picture size.) In the adaptive
  mode NL and NS refer to the maximum size of the output
  picture
.VARIABLE SL
  Starting line of the size field--ignored
.VARIABLE SS
  Starting sample of the size field--ignored
.VARIABLE NL
  Number of lines in the size field or maximum size of the
  output picture.
.VARIABLE NS
  Number of samples in the size field or maximum size of the
  output picture.
.VARIABLE PIXL
  PIXL=(line1,samp1,line2,samp2,....,lineN,sampN)  

  where (lineI,sampI) are the location in integer line and sample
  in the Ith input picture of a point common to all the pictures.

  (See LCP, SCP, which give the corresponding location in the output
  picture.)
.VARIABLE LCP
  lcp=line     LCP is a keyword followed by an integer specifying
  the line position in the output picture of the common point referred
  to by the PIXL param
.VARIABLE SCP
  scp=samp     SCP is a keyword followed by an integer specifying the 
  sample position in the output picture of the common point referred
  to by the PIXL param
.VARIABLE THRESH
  thre=n1      the integer N1 is the lower threshold DN value used
  in finding the edge of the input picture. (the default is 1) 
  a valid picture edge is not found until NSEQ=N4 consequtive
  pixels of DN greater than THRE=N1 have been found. 
.VARIABLE LTHRESH
  lthr=n2      control the left/right threshold DN values individually.
  The program searches the picture inward from each edge to find
  a specified number of consecutive pixels above a certain threshold
  DN. The nominal threshold is specified by the keyword THRESH. If
  either of the keywords LTHRESH/RTHRESH is specified, it overrides
  the nominal value only for the left/right edge of the picture. 
.VARIABLE RTHRESH
  rthr=n3      control the left/right threshold DN values individually.
  The program searches the picture inward from each edge to find
  a specified number of consecutive pixels above a certain threshold
  DN. The nominal threshold is specified by the keyword THRESH. If
  either of the keywords LTHRESH/RTHRESH is specified, it overrides
  the nominal value only for the left/right edge of the picture. 
.VARIABLE ETHRESH
  ethr=n3       sets both the left and right thresholds. ETHRESH can be
  used when the edge detection thresholds are to be different from 
  the picture threshold THRESH=N1.
.VARIABLE NSEQ
  nseq=n4       the integer N4 is the number of consecutive picture 
  elements which must be greater than or equal to the lower threshold
  DN for a valid picture edge to be found. 
.VARIABLE LSEQ
  lseq=n5       the parameters LSEQ/RSEQ allow the user to control the
  left/right threshold triggers individually. The program searches each
  input picture inward from the left/right edge until it finds LSEQ/RSEQ
  consecutive pixels which are greater than the threshold DN LTHR/RTHR. 
  The nominal search values are specified by the parameter NSEQ. If either
  of the keywords LSEQ/RSEQ is specified, then it overrides the nomial
  (NSEQ) search value only for the left/right edge of the picture.
.VARIABLE RSEQ
  rseq=n6       the parameters LSEQ/RSEQ allow the user to control the
  left/right threshold triggers individuallt. The program searches each
  input picture inward from the left/right edge until it finds LSEQ/RSEQ
  consecutive pixels which are greater than the threshold DN LTHR/RTHR. 
  The nominal search values are specified by the parameter NSEQ. If either
  of the keywords LSEQ/RSEQ is specified, then it overrides the nomial
  (NSEQ) search value only for the left/right edge of the picture.
.VARIABLE ADAPT
  ADAPT is a keyword which specifies that the input pictures have
  been map projected by MAP2 and that NEWMOS should attempt to just fit
  all the input pictures into the output picture.   The ADAPT keyword is
  similar to  the MAP2 keyword and implies the MAP2 keyword.  The difference
  between the ADAPT and MAP2 keywords is that when ADAPT is specified, NEWMOS 
  will determine the LCP and SCP parameters and change the size of the output
  picture to whatever size is necessary to fit all input pictures.  This 
  size will not be greater than the size specified in the VICAR size field.
  The default is to specify LCP, and SCP and create an output picture 
  of the size specified in the VICAR size field. If the output picture 
  should be bigger than NL * NS to contain all the input pictures the number 
  of lines and samples will be truncated.

  For Mercator, normal cylindrical, and simple cylindrical projections,
  the number of samples for the output image is determined by one of two
  ways.  By default, the program searches for the largest gap between the
  input images in the longitude direction.  It then uses the ends of the gap
  as the left and right edges of the output image.  Thus the output image
  contains all longitudes except for those in the largest gap.  This is done to
  minimize the size of the output image.  If there is no gap, the number of
  samples is set to correspond to 360 degrees of longitude.  However, if the
  NOMIN keyword is specified, no attempt to minimize the number of samples is
  made, and it is possible to have more than 360 degrees of longitude in the
  output image. 
.VARIABLE NOMIN
  If the NOMIN keyword is specified, no attempt to minimize the number of
  samples is made, and it is possible to have more than 360 degrees of
  longitude in the output image.  This applies to both adaptive algorithm
  (see HELP ADAPT) and to the Normal Cylindrical case.  For other map
  projections, the range of longitudes can exceed 0-360 degrees by 
  adjusting the SAMPLE value of the MAP labels, but for Normal Cyl., this
  does not suffice:  NOMIN must also be specified.
.VARIABLE NIBB
  nibb=n7            The integer N7 specifies the number of pixels in from
  the edge of the picture to be set to a DCLEVEL. This parameter is designed
  to remove the interpolation at picture edges caused when LGEOM interpolates
  across a step function. (The default=0)
.VARIABLE LNIB
  lnib=n8            The integers N8/N9 specify the number of pixels to be 
  set to zero or to DCLEVEL at the left/right edge of the picture.  These para-
  meters override the parameter NIBB. (The default is N8=N9=N7) 
.VARIABLE RNIB
  rnib=n9            The integers N8/N9 specify the number of pixels to be 
  set to zero or to DCLEVEL at the left/right edge of the picture.  These para-
  meters override the parameter NIBB. (The default is N8=N9=N7) 
.VARIABLE DCLEV
  dcle=n10           The integer N10 specifies the background DN level N10 
  of all the pixels in the output picture which are not overlaid by input
  picture data. (The default=0)
.VARIABLE INCR
  incr=n11           The integer N11 specifies the frequency of sampling
  pixels in each line of each input picture to test for an edge. (The default
  is to sample every tenth pixel.) This option was put in to speed up the
  program.

.VARIABLE FORMAT
  FORMAT is a keyword which specifies the data type of the output
  files (HALF or BYTE). Default is that the output file has the same
  format as the input files.

.VARIABLE AVER
  AVER is a keyword specifying that the output mosaic should use the average
  of the DN's of pixels which have DN's greater then THRESH.
  Weighting is performed according to the weight keyword.
  DNout=sum(DNin*WEIGHTin)/sum(WEIGHTin).

.VARIABLE SMOOTH
  Invokes the smooth option. In this mode the first half of the images
 are mosaicked using the second half of the images as weighting masks.
 See program MASKMOS to create the masks for you.
 DN_out= sum( DN_in * DN_mask *Weight_in) / sum( DN_mask * Weight_in)

.VARIABLE WEIGHT
  Specifies the weighting of input images in the mosaic.
  Only used in the AVER and SMOOTH options.
  Specify only one weight per input image. 
  In the SMOOTH option weights need only be
  provided for the first 1/2 of the images.
  Defaults to 1.0 for each input.
  For usage see the AVER and SMOOTH keywords.
  Example:  weight=(1.0,2.0,1.2)

.VARIABLE MAP2
  MAP2 is a keyword specifying that all the inputs have VICAR labels which
 contain all the information necessary to define the cartography of the input
 pictures. If this keyword is specified the user should not have done any
 other geometric transformation on the pictures after the MAP2 projection
 (including size field operations). The output label will be updated. 
 (See also the ADAPT parameter.)  If MAP2 is specified but ADAPT is not 
 specified, then LCP and SCP should be specified but PIXL should not be
 specified.   (For normal and simple cylindrical projections, LCP and SCP
 correspond to LAT=0 and LONG=0.)
 The default is to specify all the offsets for all the input pictures.
.VARIABLE RADIUS
  radi=r1              RADIUS is a keyword and R1 is a floating point number
  specifying the radius (in km.) of the planet being mosaicked. This is
  necessary only when the MAP2 keyword is specified and the frames are
  Mercator projected. 
.VARIABLE REQ
  req=r2              REQ and RPOL are keywords similar to RADIUS but refer to 
  the Equatorial and Polar Radius. 
.VARIABLE RPOL
  rpol=r2              REQ and RPOL arre keywords similar to RADIUS but refer
  to the Equatorial and Polar Radius. 
.VARIABLE TARGET
 Specifies the target body and thereby the radii used by the program.
 Valid: MOON,MCRY,IO,J1,EURO,J2,GANY,J3,CALL,J4,JUPI.

  MOON: specifies that REQ and RPOL are 1738.09 km 
  MCRY: specifies that REQ and RPOL are 2439.0 km (the radius of Mercury)
  IO or J1: specify that REQ and RPOL are set to 1829. km (the radius of Io)
  EURO or J2: specify that REQ and RPOLE are set to 1500. km (the radius
    of Europa)
  GANY or J3: specify that REQ and RPOL are set to 2635. km (the radius
    of Ganymede)
  CALL or J4: specify that REQ and RPOL are set to 2500. km (the radius
    of Callisto)
  JUPI: specified that REQ = 71,400 km, and RPOL = 66,773 (the values for
    Jupiter)
.end
$ Return
$!#############################################################################
$Test_File:
$ create tstnewmos.pdf
procedure
refgbl $echo
body
let _onfail="continue"
refgbl $syschar

LOCAL PATH INIT = "/project/test_work/testdata/mipl/vgr/"
if ($syschar(1) = "UNIX")
  ush rm -f geo
  ush cp /project/test_work/testdata/mipl/vgr/f1636832.geo geo
else                            ! on Alpha:
  DCL ASS WMS_TEST_WORK:[TESTDATA.MIPL.VGR]f1636832.geo geo
  LET PATH = "WMS_TEST_WORK:[TESTDATA.MIPL.VGR]"
end-if
write "THIS IS A TEST FOR THE PROGRAM NEWMOS"
write ""
let $echo="yes"
gen A NL=10 NS=10 IVAL=50 LINC=0 SINC=0
gen B NL=10 NS=10 IVAL=100 LINC=0 SINC=0
gen C NL=10 NS=10 IVAL=200 LINC=0 SINC=0
gen AA NL=10 NS=10 IVAL=50 LINC=0 SINC=0  'HALF
gen BB NL=10 NS=10 IVAL=100 LINC=0 SINC=0 'HALF
gen CC NL=10 NS=10 IVAL=200 LINC=0 SINC=0 'HALF
let $echo = "NO"
write ""
write "TEST THE OFFSET PARAMETERS"
write ""
let $echo="yes"
newmos (A,B,C) D NL=10 NS=30 LCP=1 SCP=30 PIXL=(1,30,1,20,1,10) INC=1
list D
let $echo = "NO"
write ""
write "TEST THE PARAM AVERAGE"
write ""
let $echo = "yes"
newmos (A,B) D NL=10 NS=15 LCP=1 SCP=15 PIXL=(1,15,1,10) INC=1 'AVER
LIST D
let $echo = "NO"
write ""
write "TEST THE PARAM HALF"
write ""
let $echo = "yes"
newmos (AA,BB,CC) E NL=10 NS=30 LCP=1  SCP=15 PIXL=(1,15,1,10,1,5) INC=1 'HALF
LIST E
gen A NL=10 NS=10 IVAL=0 
gen B NL=10 NS=10 IVAL=0
gen C NL=10 NS=10 IVAL=0
let $echo="no"
write ""
write "TEST THE EDGE DETECTION PARAM THRE"
write ""
let $echo="yes"
newmos (A,B,C) D NL=10 NS=30 LCP=1 SCP=30 PIXL=(1,30,1,20,1,10) INC=1 THRE=10
list D
let $echo= "no"
write ""
write "TEST THE PARM DCLE"
write ""
let $echo="yes"
newmos (A,B,C) D NL=10 NS=30 LCP=1 SCP=30 PIXL=(1,30,1,20,1,10) INC=1 THRE=10 DCLE=5
list D
write ""
write "TEST THE PARAMETERS MAP2, ADAPT, NOMIN.  SHOULD GET 0 DIFFERENCES"
write ""
MAP3 geo cylmap NL=360 NS=500 'CYLI 'VGR1 'HALF SCALE=10. +
    LINE=100. SAMP=101. LATI=80. LONG=240. 'remote target=io
MAP3 geo cylmap2 NL=360 NS=500 'CYLI 'VGR1 'HALF SCALE=10. +
    LINE=0. SAMP=1. LATI=80. LONG=240. 'remote target=io
newmos (cylmap cylmap2) mos (1 1 800 800) 'ADAPT  THRESH=-32767
f2 (cylmap mos) DIF FUNC="IN1-IN2"
hist DIF 'NOHIS
MAP3 geo cylmap3 NL=360 NS=500 'RECT 'VGR1 'HALF SCALE=10. +
    LINE=100. SAMP=101. LATI=80. LONG=240. 'remote target=io
MAP3 geo cylmap4 NL=360 NS=500 'RECT 'VGR1 'HALF SCALE=10. +
    LINE=0. SAMP=1. LATI=80. LONG=240. 'remote target=io
newmos (cylmap3 cylmap4) mos1 (1 1 800 800) 'MAP2 LCP=354 SCP=867 +
        THRESH=-32767
f2 (cylmap3 mos1) DIF FUNC="IN1-IN2"
hist DIF 'NOHIS

MAP3 geo cylmap5 NL=360 NS=500 'MERC 'VGR1 'HALF SCALE=10. +
    LINE=100. SAMP=101. LATI=70. LONG=240. 'remote target=io
MAP3 geo cylmap6 NL=360 NS=500 'MERC 'VGR1 'HALF SCALE=10. +
    LINE=0. SAMP=1. LATI=70. LONG=240. 'remote target=io
newmos (cylmap5 cylmap6) mos2 (1 1 800 800) 'ADAPT 'NOMIN +
    weight=(1.,1.) THRESH=-32767
f2 (cylmap5 mos2) DIF FUNC="IN1-IN2"
hist DIF 'NOHIS

! test SMOOTH keyword -- generate 2 halves of geo image, reduce DNs in one
! 50%, and then try to smooth overlap area 
! (overlap area is 50 pixels)
MAP3 geo cylmap7 NL=360 NS=300 'RECT 'VGR1 'HALF SCALE=10. +
    LINE=100. SAMP=101. LATI=80. LONG=240. 'remote target=io
MAP3 geo cylmap8 NL=360 NS=300 'RECT 'VGR1 'HALF SCALE=10. +
    LINE=100. SAMP=-149. LATI=80. LONG=240. 'remote target=io
f2 cylmap8 cylmap2x FUNC="0.5*IN1"
maskmos inp=(cylmap7,cylmap2x) out=(a,b)
newmos (cylmap7 cylmap2x a b) mos3 (1 1 800 800) +
        'MAP2 LCP=353 SCP=861 THRESH=-32767 'smooth
list mos3 INC=40 SL=300 NL=200 NS=800

write "==========================================================="
write "ADDED TEST FOR PERSPERCTIVE PROJECTION."
write "==========================================================="
map3 &"PATH"io.map3 a12.map3  +
  nl=500 ns=500 scale=10. 'perspect
map3 &"PATH"io.map3 a20.map3  +
  nl=500 ns=500 scale=10. 'perspect +
  north=45. latitude=80. longitud=150. line=200 samp=200
newmos inp=(a12.map3,a20.map3, +
            a12.map3,a20.map3, +
            a12.map3,a20.map3, +
            a12.map3,a20.map3, +
            a12.map3,a20.map3, +
            a12.map3,a20.map3, +
            a12.map3,a20.map3, +
            a12.map3,a20.map3, +
            a12.map3,a20.map3, +
            a12.map3,a20.map3, +
            a12.map3,a20.map3, +
            a12.map3,a20.map3, +
            a12.map3,a20.map3, +
            a12.map3,a20.map3, +
            a12.map3,a20.map3, +
            a12.map3,a20.map3, +
            a12.map3,a20.map3, +
            a12.map3,a20.map3, +
            a12.map3,a20.map3, +
            a12.map3,a20.map3, +
            a12.map3,a20.map3, +
            a12.map3,a20.map3, +
            a12.map3,a20.map3, +
            a12.map3,a20.map3, +
            a12.map3,a20.map3, +
            a12.map3,a20.map3, +
            a12.map3,a20.map3, +
            a12.map3,a20.map3, +
            a12.map3,a20.map3, +
            a12.map3,a20.map3, +
            a12.map3,a20.map3, +
            a12.map3,a20.map3, +
            a12.map3,a20.map3, +
            a12.map3,a20.map3, +
            a12.map3,a20.map3, +
            a12.map3,a20.map3, +
            a12.map3,a20.map3, +
            a12.map3,a20.map3, +
            a12.map3,a20.map3, +
            a12.map3,a20.map3) +
            out=newmos.out 'adapt 'smooth

!
! Test 3D Images
!
gen A3D NL=10 NS=10 IVAL=50 LINC=0 SINC=0 NB=10
gen B3D NL=10 NS=10 IVAL=100 LINC=0 SINC=0 NB=10
gen C3D NL=10 NS=10 IVAL=200 LINC=0 SINC=0 NB=10
newmos (A3D,B3D,C3D) D3D NL=10 NS=30 LCP=1 SCP=30 PIXL=(1,30,1,20,1,10) INC=1 'AVER
list D3D
end-proc
$!-----------------------------------------------------------------------------
$ create new_session_3d.log
tstnewmos
THIS IS A TEST FOR THE PROGRAM NEWMOS

gen A NL=10 NS=10 IVAL=50 LINC=0 SINC=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen B NL=10 NS=10 IVAL=100 LINC=0 SINC=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen C NL=10 NS=10 IVAL=200 LINC=0 SINC=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen AA NL=10 NS=10 IVAL=50 LINC=0 SINC=0  'HALF
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen BB NL=10 NS=10 IVAL=100 LINC=0 SINC=0 'HALF
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen CC NL=10 NS=10 IVAL=200 LINC=0 SINC=0 'HALF
Beginning VICAR task gen
GEN Version 6
GEN task completed
let $echo = "NO"

TEST THE OFFSET PARAMETERS

newmos (A,B,C) D NL=10 NS=30 LCP=1 SCP=30 PIXL=(1,30,1,20,1,10) INC=1
Beginning VICAR task newmos
*** NEWMOS version 18-Aug-03 ***
SYSTEM LABEL SAYS INPUT IS BYTE

LINE=              1          1          1
SAMP=             30         20         10
NOLINE=           10         10         10
NOSAMP=           10         10         10

LTHRES,RTHRE=        1         1
ITHRES=              1
DCLE=              0

LSTART=            1          1          1
LSTOP=            10         10         10
SSTART=            1         11         21
SSTOP=            10         20         30

list D
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Mon Aug 18 15:17:57 2003
 Task:NEWMOS    User:ntt       Date_Time:Mon Aug 18 15:17:58 2003
     Samp     1       3       5       7       9      11      13      15      17      19      21      23      25      27      29
   Line
      1      50  50  50  50  50  50  50  50  50  50 100 100 100 100 100 100 100 100 100 100 200 200 200 200 200 200 200 200 200 200
      2      50  50  50  50  50  50  50  50  50  50 100 100 100 100 100 100 100 100 100 100 200 200 200 200 200 200 200 200 200 200
      3      50  50  50  50  50  50  50  50  50  50 100 100 100 100 100 100 100 100 100 100 200 200 200 200 200 200 200 200 200 200
      4      50  50  50  50  50  50  50  50  50  50 100 100 100 100 100 100 100 100 100 100 200 200 200 200 200 200 200 200 200 200
      5      50  50  50  50  50  50  50  50  50  50 100 100 100 100 100 100 100 100 100 100 200 200 200 200 200 200 200 200 200 200
      6      50  50  50  50  50  50  50  50  50  50 100 100 100 100 100 100 100 100 100 100 200 200 200 200 200 200 200 200 200 200
      7      50  50  50  50  50  50  50  50  50  50 100 100 100 100 100 100 100 100 100 100 200 200 200 200 200 200 200 200 200 200
      8      50  50  50  50  50  50  50  50  50  50 100 100 100 100 100 100 100 100 100 100 200 200 200 200 200 200 200 200 200 200
      9      50  50  50  50  50  50  50  50  50  50 100 100 100 100 100 100 100 100 100 100 200 200 200 200 200 200 200 200 200 200
     10      50  50  50  50  50  50  50  50  50  50 100 100 100 100 100 100 100 100 100 100 200 200 200 200 200 200 200 200 200 200
let $echo = "NO"

TEST THE PARAM AVERAGE

newmos (A,B) D NL=10 NS=15 LCP=1 SCP=15 PIXL=(1,15,1,10) INC=1 'AVER
Beginning VICAR task newmos
*** NEWMOS version 18-Aug-03 ***
SYSTEM LABEL SAYS INPUT IS BYTE

LINE=              1          1
SAMP=             15         10
NOLINE=           10         10
NOSAMP=           10         10

LTHRES,RTHRE=        1         1
ITHRES=              1
DCLE=              0

LSTART=            1          1
LSTOP=            10         10
SSTART=            1          6
SSTOP=            10         15

LIST D
Beginning VICAR task LIST

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Mon Aug 18 15:17:57 2003
 Task:NEWMOS    User:ntt       Date_Time:Mon Aug 18 15:17:58 2003
     Samp     1       3       5       7       9      11      13      15
   Line
      1      50  50  50  50  50  75  75  75  75  75 100 100 100 100 100
      2      50  50  50  50  50  75  75  75  75  75 100 100 100 100 100
      3      50  50  50  50  50  75  75  75  75  75 100 100 100 100 100
      4      50  50  50  50  50  75  75  75  75  75 100 100 100 100 100
      5      50  50  50  50  50  75  75  75  75  75 100 100 100 100 100
      6      50  50  50  50  50  75  75  75  75  75 100 100 100 100 100
      7      50  50  50  50  50  75  75  75  75  75 100 100 100 100 100
      8      50  50  50  50  50  75  75  75  75  75 100 100 100 100 100
      9      50  50  50  50  50  75  75  75  75  75 100 100 100 100 100
     10      50  50  50  50  50  75  75  75  75  75 100 100 100 100 100
let $echo = "NO"

TEST THE PARAM HALF

newmos (AA,BB,CC) E NL=10 NS=30 LCP=1  SCP=15 PIXL=(1,15,1,10,1,5) INC=1 'HALF
Beginning VICAR task newmos
*** NEWMOS version 18-Aug-03 ***
SYSTEM LABEL SAYS INPUT IS HALFWORD

LINE=              1          1          1
SAMP=             15         10          5
NOLINE=           10         10         10
NOSAMP=           10         10         10

LTHRES,RTHRE=        1         1
ITHRES=              1
DCLE=              0

LSTART=            1          1          1
LSTOP=            10         10         10
SSTART=            1          6         11
SSTOP=            10         15         20

LIST E
Beginning VICAR task LIST

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:ntt       Date_Time:Mon Aug 18 15:17:57 2003
 Task:NEWMOS    User:ntt       Date_Time:Mon Aug 18 15:17:58 2003
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14    15
   Line
      1        50    50    50    50    50    50    50    50    50    50   100   100   100   100   100
      2        50    50    50    50    50    50    50    50    50    50   100   100   100   100   100
      3        50    50    50    50    50    50    50    50    50    50   100   100   100   100   100
      4        50    50    50    50    50    50    50    50    50    50   100   100   100   100   100
      5        50    50    50    50    50    50    50    50    50    50   100   100   100   100   100
      6        50    50    50    50    50    50    50    50    50    50   100   100   100   100   100
      7        50    50    50    50    50    50    50    50    50    50   100   100   100   100   100
      8        50    50    50    50    50    50    50    50    50    50   100   100   100   100   100
      9        50    50    50    50    50    50    50    50    50    50   100   100   100   100   100
     10        50    50    50    50    50    50    50    50    50    50   100   100   100   100   100

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:ntt       Date_Time:Mon Aug 18 15:17:57 2003
 Task:NEWMOS    User:ntt       Date_Time:Mon Aug 18 15:17:58 2003
     Samp      16    17    18    19    20    21    22    23    24    25    26    27    28    29    30
   Line
      1       200   200   200   200   200     0     0     0     0     0     0     0     0     0     0
      2       200   200   200   200   200     0     0     0     0     0     0     0     0     0     0
      3       200   200   200   200   200     0     0     0     0     0     0     0     0     0     0
      4       200   200   200   200   200     0     0     0     0     0     0     0     0     0     0
      5       200   200   200   200   200     0     0     0     0     0     0     0     0     0     0
      6       200   200   200   200   200     0     0     0     0     0     0     0     0     0     0
      7       200   200   200   200   200     0     0     0     0     0     0     0     0     0     0
      8       200   200   200   200   200     0     0     0     0     0     0     0     0     0     0
      9       200   200   200   200   200     0     0     0     0     0     0     0     0     0     0
     10       200   200   200   200   200     0     0     0     0     0     0     0     0     0     0
gen A NL=10 NS=10 IVAL=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen B NL=10 NS=10 IVAL=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen C NL=10 NS=10 IVAL=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
let $echo="no"

TEST THE EDGE DETECTION PARAM THRE

newmos (A,B,C) D NL=10 NS=30 LCP=1 SCP=30 PIXL=(1,30,1,20,1,10) INC=1 THRE=10
Beginning VICAR task newmos
*** NEWMOS version 18-Aug-03 ***
SYSTEM LABEL SAYS INPUT IS BYTE

LINE=              1          1          1
SAMP=             30         20         10
NOLINE=           10         10         10
NOSAMP=           10         10         10

LTHRES,RTHRE=       10        10
ITHRES=             10
DCLE=              0

LSTART=            1          1          1
LSTOP=            10         10         10
SSTART=            1         11         21
SSTOP=            10         20         30

list D
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Mon Aug 18 15:17:59 2003
 Task:NEWMOS    User:ntt       Date_Time:Mon Aug 18 15:17:59 2003
     Samp     1       3       5       7       9      11      13      15      17      19      21      23      25      27      29
   Line

      4       0   0   0   0   0   0   0  10  11  12   0   0   0   0   0   0   0  10  11  12   0   0   0   0   0   0   0  10  11  12
      5       0   0   0   0   0   0  10  11  12  13   0   0   0   0   0   0  10  11  12  13   0   0   0   0   0   0  10  11  12  13
      6       0   0   0   0   0  10  11  12  13  14   0   0   0   0   0  10  11  12  13  14   0   0   0   0   0  10  11  12  13  14
      7       0   0   0   0  10  11  12  13  14  15   0   0   0   0  10  11  12  13  14  15   0   0   0   0  10  11  12  13  14  15
      8       0   0   0  10  11  12  13  14  15  16   0   0   0  10  11  12  13  14  15  16   0   0   0  10  11  12  13  14  15  16
      9       0   0  10  11  12  13  14  15  16  17   0   0  10  11  12  13  14  15  16  17   0   0  10  11  12  13  14  15  16  17
     10       0  10  11  12  13  14  15  16  17  18   0  10  11  12  13  14  15  16  17  18   0  10  11  12  13  14  15  16  17  18
let $echo= "no"

TEST THE PARM DCLE

newmos (A,B,C) D NL=10 NS=30 LCP=1 SCP=30 PIXL=(1,30,1,20,1,10) INC=1 THRE=10 DCLE=5
Beginning VICAR task newmos
*** NEWMOS version 18-Aug-03 ***
SYSTEM LABEL SAYS INPUT IS BYTE

LINE=              1          1          1
SAMP=             30         20         10
NOLINE=           10         10         10
NOSAMP=           10         10         10

LTHRES,RTHRE=       10        10
ITHRES=             10
DCLE=              5

LSTART=            1          1          1
LSTOP=            10         10         10
SSTART=            1         11         21
SSTOP=            10         20         30

list D
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Mon Aug 18 15:17:59 2003
 Task:NEWMOS    User:ntt       Date_Time:Mon Aug 18 15:18:00 2003
     Samp     1       3       5       7       9      11      13      15      17      19      21      23      25      27      29
   Line
      1       5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5
      2       5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5
      3       5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5
      4       5   5   5   5   5   5   5  10  11  12   5   5   5   5   5   5   5  10  11  12   5   5   5   5   5   5   5  10  11  12
      5       5   5   5   5   5   5  10  11  12  13   5   5   5   5   5   5  10  11  12  13   5   5   5   5   5   5  10  11  12  13
      6       5   5   5   5   5  10  11  12  13  14   5   5   5   5   5  10  11  12  13  14   5   5   5   5   5  10  11  12  13  14
      7       5   5   5   5  10  11  12  13  14  15   5   5   5   5  10  11  12  13  14  15   5   5   5   5  10  11  12  13  14  15
      8       5   5   5  10  11  12  13  14  15  16   5   5   5  10  11  12  13  14  15  16   5   5   5  10  11  12  13  14  15  16
      9       5   5  10  11  12  13  14  15  16  17   5   5  10  11  12  13  14  15  16  17   5   5  10  11  12  13  14  15  16  17
     10       5  10  11  12  13  14  15  16  17  18   5  10  11  12  13  14  15  16  17  18   5  10  11  12  13  14  15  16  17  18
write ""

write "TEST THE PARAMETERS MAP2, ADAPT, NOMIN.  SHOULD GET 0 DIFFERENCES"
TEST THE PARAMETERS MAP2, ADAPT, NOMIN.  SHOULD GET 0 DIFFERENCES
write ""

MAP3 geo cylmap NL=360 NS=500 'CYLI 'VGR1 'HALF SCALE=10.  +
    LINE=100. SAMP=101. LATI=80. LONG=240. 'remote target=io
Beginning VICAR task MAP3
Map3 version 6-Dec-1999
 LABEL SAYS INPUT PICTURE IS OBJECT SPACE
    CAMERA=          7
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
GETLABCON: warning ind=          1
flight label
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Actual source of SEDR is: DAVI
OM matrix obtained from SEDR
RS vector obtained from SEDR
Target radii obtained from SEDR
 JULIAN DATE OF EVENT FRAME=  2443937.3
/CAM FOC L/    RA   /    RB   /    RC   /   RMAG  /LA SSC PT/LO SSC PT/
/ 127248.2/ 1829.400/ 1819.300/ 1815.700/ 806029.9/   -0.032/  156.474/
    OM MATRIX
/ 0.225295/-0.516547/ 0.826088
/ 0.330342/-0.757163/-0.563541
/ 0.916579/ 0.399854/ 0.000052
    RS VECTOR (TARGET COORDINATES)
       -739030.7       -321741.5          -456.1
COORDINATES OF CENTER OF INPUT PICTURE--LATITUDE = -13.09, LONGITUDE = 176.23
PROJECTION IS NORMAL CYLINDRICAL
 DATA=
            8.650E+02  2.790E+02  0.000E+00  0.000E+00  0.000E+00  2.713E+02  1.000E+01  1.000E+00  5.570E+01  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  1.816E+03  1.829E+03  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
MAP3 geo cylmap2 NL=360 NS=500 'CYLI 'VGR1 'HALF SCALE=10.  +
    LINE=0. SAMP=1. LATI=80. LONG=240. 'remote target=io
Beginning VICAR task MAP3
Map3 version 6-Dec-1999
 LABEL SAYS INPUT PICTURE IS OBJECT SPACE
    CAMERA=          7
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
GETLABCON: warning ind=          1
flight label
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Actual source of SEDR is: DAVI
OM matrix obtained from SEDR
RS vector obtained from SEDR
Target radii obtained from SEDR
 JULIAN DATE OF EVENT FRAME=  2443937.3
/CAM FOC L/    RA   /    RB   /    RC   /   RMAG  /LA SSC PT/LO SSC PT/
/ 127248.2/ 1829.400/ 1819.300/ 1815.700/ 806029.9/   -0.032/  156.474/
    OM MATRIX
/ 0.225295/-0.516547/ 0.826088
/ 0.330342/-0.757163/-0.563541
/ 0.916579/ 0.399854/ 0.000052
    RS VECTOR (TARGET COORDINATES)
       -739030.7       -321741.5          -456.1
COORDINATES OF CENTER OF INPUT PICTURE--LATITUDE = -13.09, LONGITUDE = 176.23
PROJECTION IS NORMAL CYLINDRICAL
 DATA=
            7.650E+02  1.790E+02  0.000E+00  0.000E+00  0.000E+00  2.399E+02  1.000E+01  1.000E+00  5.570E+01  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  1.816E+03  1.829E+03  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
newmos (cylmap cylmap2) mos (1 1 800 800) 'ADAPT  THRESH=-32767
Beginning VICAR task newmos
*** NEWMOS version 18-Aug-03 ***
SYSTEM LABEL SAYS INPUT IS HALFWORD
LONG=      0.2713E+03  0.2399E+03
SEARCW FOUND SAMP=  867       767
THE PROGRAM CHOSE LCP=   279 AND SCP=   867
RPOL,REQ USED=      0.1816E+04 0.1829E+04
THE OUTPUT PICTURE WILL CONTAIN   460 LINES,    600 SAMPLES AND    1 BAND(S)

LINE=            279        179
SAMP=            867        767
NOLINE=          360        360
NOSAMP=          500        500

LTHRES,RTHRE=   -32767    -32767
ITHRES=         -32767
DCLE=              0

LSTART=            1        101
LSTOP=           360        460
SSTART=            1        101
SSTOP=           500        600

f2 (cylmap mos) DIF FUNC="IN1-IN2"
Beginning VICAR task f2
F2 version 2-04-94
LINES TRUNCATED
SAMPLES TRUNCATED
F2 using hash table lookup
FUNCTION EVALUATED 5728 TIMES
hist DIF 'NOHIS
Beginning VICAR task hist
HIST version 30-JUL-03


AVERAGE GRAY LEVEL=0.000000       STANDARD DEVIATION=0.000000       NUMBER ELEMENTS=  180000
MIN. DN=         0
MAX. DN=         0

MAP3 geo cylmap3 NL=360 NS=500 'RECT 'VGR1 'HALF SCALE=10.  +
    LINE=100. SAMP=101. LATI=80. LONG=240. 'remote target=io
Beginning VICAR task MAP3
Map3 version 6-Dec-1999
 LABEL SAYS INPUT PICTURE IS OBJECT SPACE
    CAMERA=          7
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
GETLABCON: warning ind=          1
flight label
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Actual source of SEDR is: DAVI
OM matrix obtained from SEDR
RS vector obtained from SEDR
Target radii obtained from SEDR
 JULIAN DATE OF EVENT FRAME=  2443937.3
/CAM FOC L/    RA   /    RB   /    RC   /   RMAG  /LA SSC PT/LO SSC PT/
/ 127248.2/ 1829.400/ 1819.300/ 1815.700/ 806029.9/   -0.032/  156.474/
    OM MATRIX
/ 0.225295/-0.516547/ 0.826088
/ 0.330342/-0.757163/-0.563541
/ 0.916579/ 0.399854/ 0.000052
    RS VECTOR (TARGET COORDINATES)
       -739030.7       -321741.5          -456.1
COORDINATES OF CENTER OF INPUT PICTURE--LATITUDE = -13.09, LONGITUDE = 176.23
PROJECTION IS SIMPLE CYLINDRICAL
 DATA=
            1.000E+00  3.550E+02  0.000E+00  0.000E+00  0.000E+00  2.713E+02  1.000E+01  1.000E+00  5.570E+01  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  1.816E+03  1.829E+03  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
MAP3 geo cylmap4 NL=360 NS=500 'RECT 'VGR1 'HALF SCALE=10.  +
    LINE=0. SAMP=1. LATI=80. LONG=240. 'remote target=io
Beginning VICAR task MAP3
Map3 version 6-Dec-1999
 LABEL SAYS INPUT PICTURE IS OBJECT SPACE
    CAMERA=          7
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
GETLABCON: warning ind=          1
flight label
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Actual source of SEDR is: DAVI
OM matrix obtained from SEDR
RS vector obtained from SEDR
Target radii obtained from SEDR
 JULIAN DATE OF EVENT FRAME=  2443937.3
/CAM FOC L/    RA   /    RB   /    RC   /   RMAG  /LA SSC PT/LO SSC PT/
/ 127248.2/ 1829.400/ 1819.300/ 1815.700/ 806029.9/   -0.032/  156.474/
    OM MATRIX
/ 0.225295/-0.516547/ 0.826088
/ 0.330342/-0.757163/-0.563541
/ 0.916579/ 0.399854/ 0.000052
    RS VECTOR (TARGET COORDINATES)
       -739030.7       -321741.5          -456.1
COORDINATES OF CENTER OF INPUT PICTURE--LATITUDE = -13.09, LONGITUDE = 176.23
PROJECTION IS SIMPLE CYLINDRICAL
 DATA=
            1.000E+00  2.550E+02  0.000E+00  0.000E+00  0.000E+00  2.399E+02  1.000E+01  1.000E+00  5.570E+01  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  1.816E+03  1.829E+03  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
newmos (cylmap3 cylmap4) mos1 (1 1 800 800) 'MAP2 LCP=354 SCP=867  +
        THRESH=-32767
Beginning VICAR task newmos
*** NEWMOS version 18-Aug-03 ***
SYSTEM LABEL SAYS INPUT IS HALFWORD
LONG=      0.2713E+03  0.2399E+03
SEARCW FOUND SAMP=  867       767
RPOL,REQ USED=      0.1816E+04 0.1829E+04
THE OUTPUT PICTURE WILL CONTAIN   800 LINES,    800 SAMPLES AND    1 BAND(S)

LINE=            355        255
SAMP=            867        767
NOLINE=          360        360
NOSAMP=          500        500

***NUMBER OF OFFSETS=    1
***NUMBER OF INPUTS=     2
LTHRES,RTHRE=   -32767    -32767
ITHRES=         -32767
DCLE=              0

LSTART=            0        100
LSTOP=           359        459
SSTART=            1        101
SSTOP=           500        600

f2 (cylmap3 mos1) DIF FUNC="IN1-IN2"
Beginning VICAR task f2
F2 version 2-04-94
LINES TRUNCATED
SAMPLES TRUNCATED
F2 using hash table lookup
FUNCTION EVALUATED 119391 TIMES
hist DIF 'NOHIS
Beginning VICAR task hist
HIST version 30-JUL-03


AVERAGE GRAY LEVEL=-9.36664       STANDARD DEVIATION=119.5696       NUMBER ELEMENTS=  180000
MIN. DN=     -1677
MAX. DN=      3544

MAP3 geo cylmap5 NL=360 NS=500 'MERC 'VGR1 'HALF SCALE=10.  +
    LINE=100. SAMP=101. LATI=70. LONG=240. 'remote target=io
Beginning VICAR task MAP3
Map3 version 6-Dec-1999
 LABEL SAYS INPUT PICTURE IS OBJECT SPACE
    CAMERA=          7
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
GETLABCON: warning ind=          1
flight label
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Actual source of SEDR is: DAVI
OM matrix obtained from SEDR
RS vector obtained from SEDR
Target radii obtained from SEDR
 JULIAN DATE OF EVENT FRAME=  2443937.3
/CAM FOC L/    RA   /    RB   /    RC   /   RMAG  /LA SSC PT/LO SSC PT/
/ 127248.2/ 1829.400/ 1819.300/ 1815.700/ 806029.9/   -0.032/  156.474/
    OM MATRIX
/ 0.225295/-0.516547/ 0.826088
/ 0.330342/-0.757163/-0.563541
/ 0.916579/ 0.399854/ 0.000052
    RS VECTOR (TARGET COORDINATES)
       -739030.7       -321741.5          -456.1
COORDINATES OF CENTER OF INPUT PICTURE--LATITUDE = -13.09, LONGITUDE = 176.23
PROJECTION SPECIFIED IS MERCATOR
 DATA=
            1.000E+00  1.000E+00  7.830E+01  0.000E+00  0.000E+00  2.714E+02  1.000E+01  1.000E+00  5.570E+01  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  1.816E+03  1.829E+03  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
MAP3 geo cylmap6 NL=360 NS=500 'MERC 'VGR1 'HALF SCALE=10.  +
    LINE=0. SAMP=1. LATI=70. LONG=240. 'remote target=io
Beginning VICAR task MAP3
Map3 version 6-Dec-1999
 LABEL SAYS INPUT PICTURE IS OBJECT SPACE
    CAMERA=          7
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
GETLABCON: warning ind=          1
flight label
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Actual source of SEDR is: DAVI
OM matrix obtained from SEDR
RS vector obtained from SEDR
Target radii obtained from SEDR
 JULIAN DATE OF EVENT FRAME=  2443937.3
/CAM FOC L/    RA   /    RB   /    RC   /   RMAG  /LA SSC PT/LO SSC PT/
/ 127248.2/ 1829.400/ 1819.300/ 1815.700/ 806029.9/   -0.032/  156.474/
    OM MATRIX
/ 0.225295/-0.516547/ 0.826088
/ 0.330342/-0.757163/-0.563541
/ 0.916579/ 0.399854/ 0.000052
    RS VECTOR (TARGET COORDINATES)
       -739030.7       -321741.5          -456.1
COORDINATES OF CENTER OF INPUT PICTURE--LATITUDE = -13.09, LONGITUDE = 176.23
PROJECTION SPECIFIED IS MERCATOR
 DATA=
            1.000E+00  1.000E+00  6.989E+01  0.000E+00  0.000E+00  2.400E+02  1.000E+01  1.000E+00  5.570E+01  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  1.816E+03  1.829E+03  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
newmos (cylmap5 cylmap6) mos2 (1 1 800 800) 'ADAPT 'NOMIN  +
    weight=(1.,1.) THRESH=-32767
Beginning VICAR task newmos
*** NEWMOS version 18-Aug-03 ***
SYSTEM LABEL SAYS INPUT IS HALFWORD
LONG=      0.2714E+03  0.2400E+03
THE PROGRAM CHOSE LCP=   417 AND SCP=   868
RPOL,REQ USED=      0.1816E+04 0.1829E+04
THE OUTPUT PICTURE WILL CONTAIN   461 LINES,    601 SAMPLES AND    1 BAND(S)

LINE=            417        316
SAMP=            868        767
NOLINE=          360        360
NOSAMP=          500        500

LTHRES,RTHRE=   -32767    -32767
ITHRES=         -32767
DCLE=              0

LSTART=            1        102
LSTOP=           360        461
SSTART=            1        102
SSTOP=           500        601

f2 (cylmap5 mos2) DIF FUNC="IN1-IN2"
Beginning VICAR task f2
F2 version 2-04-94
LINES TRUNCATED
SAMPLES TRUNCATED
F2 using hash table lookup
FUNCTION EVALUATED 5718 TIMES
hist DIF 'NOHIS
Beginning VICAR task hist
HIST version 30-JUL-03


AVERAGE GRAY LEVEL=0.000000       STANDARD DEVIATION=0.000000       NUMBER ELEMENTS=  180000
MIN. DN=         0
MAX. DN=         0

MAP3 geo cylmap7 NL=360 NS=300 'RECT 'VGR1 'HALF SCALE=10.  +
    LINE=100. SAMP=101. LATI=80. LONG=240. 'remote target=io
Beginning VICAR task MAP3
Map3 version 6-Dec-1999
 LABEL SAYS INPUT PICTURE IS OBJECT SPACE
    CAMERA=          7
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
GETLABCON: warning ind=          1
flight label
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Actual source of SEDR is: DAVI
OM matrix obtained from SEDR
RS vector obtained from SEDR
Target radii obtained from SEDR
 JULIAN DATE OF EVENT FRAME=  2443937.3
/CAM FOC L/    RA   /    RB   /    RC   /   RMAG  /LA SSC PT/LO SSC PT/
/ 127248.2/ 1829.400/ 1819.300/ 1815.700/ 806029.9/   -0.032/  156.474/
    OM MATRIX
/ 0.225295/-0.516547/ 0.826088
/ 0.330342/-0.757163/-0.563541
/ 0.916579/ 0.399854/ 0.000052
    RS VECTOR (TARGET COORDINATES)
       -739030.7       -321741.5          -456.1
COORDINATES OF CENTER OF INPUT PICTURE--LATITUDE = -13.09, LONGITUDE = 176.23
PROJECTION IS SIMPLE CYLINDRICAL
 DATA=
            1.000E+00  3.550E+02  0.000E+00  0.000E+00  0.000E+00  2.713E+02  1.000E+01  1.000E+00  5.570E+01  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  1.816E+03  1.829E+03  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
MAP3 geo cylmap8 NL=360 NS=300 'RECT 'VGR1 'HALF SCALE=10.  +
    LINE=100. SAMP=-149. LATI=80. LONG=240. 'remote target=io
Beginning VICAR task MAP3
Map3 version 6-Dec-1999
 LABEL SAYS INPUT PICTURE IS OBJECT SPACE
    CAMERA=          7
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
GETLABCON: warning ind=          1
flight label
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Actual source of SEDR is: DAVI
OM matrix obtained from SEDR
RS vector obtained from SEDR
Target radii obtained from SEDR
 JULIAN DATE OF EVENT FRAME=  2443937.3
/CAM FOC L/    RA   /    RB   /    RC   /   RMAG  /LA SSC PT/LO SSC PT/
/ 127248.2/ 1829.400/ 1819.300/ 1815.700/ 806029.9/   -0.032/  156.474/
    OM MATRIX
/ 0.225295/-0.516547/ 0.826088
/ 0.330342/-0.757163/-0.563541
/ 0.916579/ 0.399854/ 0.000052
    RS VECTOR (TARGET COORDINATES)
       -739030.7       -321741.5          -456.1
COORDINATES OF CENTER OF INPUT PICTURE--LATITUDE = -13.09, LONGITUDE = 176.23
PROJECTION IS SIMPLE CYLINDRICAL
 DATA=
            1.000E+00  3.550E+02  0.000E+00  0.000E+00  0.000E+00  1.928E+02  1.000E+01  1.000E+00  5.570E+01  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  1.816E+03  1.829E+03  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
f2 cylmap8 cylmap2x FUNC="0.5*IN1"
Beginning VICAR task f2
F2 version 2-04-94
F2 using hash table lookup
FUNCTION EVALUATED 5863 TIMES
maskmos inp=(cylmap7,cylmap2x) out=(a,b)
Beginning VICAR task maskmos
newmos (cylmap7 cylmap2x a b) mos3 (1 1 800 800)  +
        'MAP2 LCP=353 SCP=861 THRESH=-32767 'smooth
Beginning VICAR task newmos
*** NEWMOS version 18-Aug-03 ***
SYSTEM LABEL SAYS INPUT IS HALFWORD
LONG=      0.2713E+03  0.1928E+03  0.2713E+03  0.1928E+03
SEARCW FOUND SAMP=  867       616       867       616
RPOL,REQ USED=      0.1816E+04 0.1829E+04
THE OUTPUT PICTURE WILL CONTAIN   800 LINES,    800 SAMPLES AND    1 BAND(S)

LINE=            355        355        355        355
SAMP=            867        616        867        616
NOLINE=          360        360        360        360
NOSAMP=          300        300        300        300

***NUMBER OF OFFSETS=    1
***NUMBER OF INPUTS=     4
LTHRES,RTHRE=   -32767    -32767
ITHRES=         -32767
DCLE=              0

LSTART=           -1         -1         -1         -1
LSTOP=           358        358        358        358
SSTART=           -5        246         -5        246
SSTOP=           294        545        294        545

list mos3 INC=40 SL=300 NL=200 NS=800
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:CONV12    User:DFS       Date_Time:Thu May  3 11:15:53 1984
 Task:NEWMOS    User:ntt       Date_Time:Mon Aug 18 15:18:20 2003
     Samp       1    41    81   121   161   201   241   281   321   361   401   441   481   521   561
   Line
    300         0     0  2081  3658  3465  4102  3246  2165  1969  2172  1925  2828  2249  2267     0
    340         0     0  2156  2976  3058  2900  3936  3752  2419  2627  2682  2639  2409  2256     0
write "==========================================================="
===========================================================
write "ADDED TEST FOR PERSPERCTIVE PROJECTION."
ADDED TEST FOR PERSPERCTIVE PROJECTION.
write "==========================================================="
===========================================================
map3 /project/test_work/testdat+
a/mipl/vgr/io.map3 a12.map3     nl=500 ns=500 scale=10. 'perspect
Beginning VICAR task map3
Map3 version 6-Dec-1999
Label specifies map3 perspective input mode
    CAMERA=          7
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
GETLABCON: warning ind=          1
flight label
 JULIAN DATE OF EVENT FRAME=  2443937.3
/CAM FOC L/    RA   /    RB   /    RC   /   RMAG  /LA SSC PT/LO SSC PT/
/ 127248.2/ 1824.350/ 1824.350/ 1815.300/ 638045.9/    0.000/  150.000/
    OM MATRIX
/ 0.500000/-0.866025/ 0.000000
/ 0.000000/ 0.000000/-1.000000
/ 0.866025/ 0.500000/ 0.000000
    RS VECTOR (TARGET COORDINATES)
       -552563.9       -319022.9             0.0
COORDINATES OF CENTER OF INPUT PICTURE--LATITUDE =   0.00, LONGITUDE = 150.00
Projection is Perspective.
 DATA(25-38)=
            1.815E+03  1.824E+03  1.500E+03  2.500E+02  2.500E+02  8.482E+01  0.000E+00  1.500E+02  2.500E+02  2.500E+02
            0.000E+00  0.000E+00  0.000E+00  1.274E+06
map3 /project/test_work/testdat+
a/mipl/vgr/io.map3 a20.map3     nl=50+
0 ns=500 scale=10. 'perspect    north=45. latitude=80. longitud=150. line=200 samp=200
Beginning VICAR task map3
Map3 version 6-Dec-1999
Label specifies map3 perspective input mode
    CAMERA=          7
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
GETLABCON: warning ind=          1
flight label
 JULIAN DATE OF EVENT FRAME=  2443937.3
/CAM FOC L/    RA   /    RB   /    RC   /   RMAG  /LA SSC PT/LO SSC PT/
/ 127248.2/ 1824.350/ 1824.350/ 1815.300/ 638045.9/    0.000/  150.000/
    OM MATRIX
/ 0.500000/-0.866025/ 0.000000
/ 0.000000/ 0.000000/-1.000000
/ 0.866025/ 0.500000/ 0.000000
    RS VECTOR (TARGET COORDINATES)
       -552563.9       -319022.9             0.0
COORDINATES OF CENTER OF INPUT PICTURE--LATITUDE =   0.00, LONGITUDE = 150.00
Projection is Perspective.
 DATA(25-38)=
            1.815E+03  1.824E+03  1.500E+03  2.000E+02  2.000E+02  8.482E+01  8.000E+01  1.500E+02  2.000E+02  2.000E+02
            4.500E+01  0.000E+00  0.000E+00  1.274E+06
newmos inp=(a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3)  +
            out=newmos.out 'adapt 'smooth
Beginning VICAR task newmos
*** NEWMOS version 18-Aug-03 ***
SYSTEM LABEL SAYS INPUT IS BYTE
LONG=      0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00
SEARCW FOUND SAMP=  250       200       250       200       250       200
LONG=      0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00
SEARCW FOUND SAMP=  250       200       250       200       250       200
LONG=      0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00
SEARCW FOUND SAMP=  250       200       250       200       250       200
LONG=      0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00
SEARCW FOUND SAMP=  250       200       250       200       250       200
LONG=      0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00
SEARCW FOUND SAMP=  250       200       250       200       250       200
LONG=      0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00
SEARCW FOUND SAMP=  250       200       250       200       250       200
LONG=      0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00
SEARCW FOUND SAMP=  250       200       250       200       250       200
LONG=      0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00
SEARCW FOUND SAMP=  250       200       250       200       250       200
LONG=      0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00
SEARCW FOUND SAMP=  250       200       250       200       250       200
LONG=      0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00
SEARCW FOUND SAMP=  250       200       250       200       250       200
LONG=      0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00
SEARCW FOUND SAMP=  250       200       250       200       250       200
LONG=      0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00
SEARCW FOUND SAMP=  250       200       250       200       250       200
LONG=      0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00
SEARCW FOUND SAMP=  250       200       250       200       250       200
LONG=      0.0000E+00  0.0000E+00
SEARCW FOUND SAMP=  250       200
***LINES TRUNCATED
***SAMPLES TRUNCATED
THE PROGRAM CHOSE LCP=   250 AND SCP=   250
RPOL,REQ USED=      0.1815E+04 0.1824E+04
THE OUTPUT PICTURE WILL CONTAIN   500 LINES,    500 SAMPLES AND    1 BAND(S)

LINE=            250        200        250        200        250        200
SAMP=            250        200        250        200        250        200
NOLINE=          500        500        500        500        500        500
NOSAMP=          500        500        500        500        500        500

LINE=            250        200        250        200        250        200
SAMP=            250        200        250        200        250        200
NOLINE=          500        500        500        500        500        500
NOSAMP=          500        500        500        500        500        500

LINE=            250        200        250        200        250        200
SAMP=            250        200        250        200        250        200
NOLINE=          500        500        500        500        500        500
NOSAMP=          500        500        500        500        500        500

LINE=            250        200        250        200        250        200
SAMP=            250        200        250        200        250        200
NOLINE=          500        500        500        500        500        500
NOSAMP=          500        500        500        500        500        500

LINE=            250        200        250        200        250        200
SAMP=            250        200        250        200        250        200
NOLINE=          500        500        500        500        500        500
NOSAMP=          500        500        500        500        500        500

LINE=            250        200        250        200        250        200
SAMP=            250        200        250        200        250        200
NOLINE=          500        500        500        500        500        500
NOSAMP=          500        500        500        500        500        500

LINE=            250        200        250        200        250        200
SAMP=            250        200        250        200        250        200
NOLINE=          500        500        500        500        500        500
NOSAMP=          500        500        500        500        500        500

LINE=            250        200        250        200        250        200
SAMP=            250        200        250        200        250        200
NOLINE=          500        500        500        500        500        500
NOSAMP=          500        500        500        500        500        500

LINE=            250        200        250        200        250        200
SAMP=            250        200        250        200        250        200
NOLINE=          500        500        500        500        500        500
NOSAMP=          500        500        500        500        500        500

LINE=            250        200        250        200        250        200
SAMP=            250        200        250        200        250        200
NOLINE=          500        500        500        500        500        500
NOSAMP=          500        500        500        500        500        500

LINE=            250        200        250        200        250        200
SAMP=            250        200        250        200        250        200
NOLINE=          500        500        500        500        500        500
NOSAMP=          500        500        500        500        500        500

LINE=            250        200        250        200        250        200
SAMP=            250        200        250        200        250        200
NOLINE=          500        500        500        500        500        500
NOSAMP=          500        500        500        500        500        500

LINE=            250        200        250        200        250        200
SAMP=            250        200        250        200        250        200
NOLINE=          500        500        500        500        500        500
NOSAMP=          500        500        500        500        500        500

LINE=            250        200
SAMP=            250        200
NOLINE=          500        500
NOSAMP=          500        500

LTHRES,RTHRE=        1         1
ITHRES=              1
DCLE=              0

LSTART=            1         51          1         51          1         51
LSTOP=           500        550        500        550        500        550
SSTART=            1         51          1         51          1         51
SSTOP=           500        550        500        550        500        550

LSTART=            1         51          1         51          1         51
LSTOP=           500        550        500        550        500        550
SSTART=            1         51          1         51          1         51
SSTOP=           500        550        500        550        500        550

LSTART=            1         51          1         51          1         51
LSTOP=           500        550        500        550        500        550
SSTART=            1         51          1         51          1         51
SSTOP=           500        550        500        550        500        550

LSTART=            1         51          1         51          1         51
LSTOP=           500        550        500        550        500        550
SSTART=            1         51          1         51          1         51
SSTOP=           500        550        500        550        500        550

LSTART=            1         51          1         51          1         51
LSTOP=           500        550        500        550        500        550
SSTART=            1         51          1         51          1         51
SSTOP=           500        550        500        550        500        550

LSTART=            1         51          1         51          1         51
LSTOP=           500        550        500        550        500        550
SSTART=            1         51          1         51          1         51
SSTOP=           500        550        500        550        500        550

LSTART=            1         51          1         51          1         51
LSTOP=           500        550        500        550        500        550
SSTART=            1         51          1         51          1         51
SSTOP=           500        550        500        550        500        550

LSTART=            1         51          1         51          1         51
LSTOP=           500        550        500        550        500        550
SSTART=            1         51          1         51          1         51
SSTOP=           500        550        500        550        500        550

LSTART=            1         51          1         51          1         51
LSTOP=           500        550        500        550        500        550
SSTART=            1         51          1         51          1         51
SSTOP=           500        550        500        550        500        550

LSTART=            1         51          1         51          1         51
LSTOP=           500        550        500        550        500        550
SSTART=            1         51          1         51          1         51
SSTOP=           500        550        500        550        500        550

LSTART=            1         51          1         51          1         51
LSTOP=           500        550        500        550        500        550
SSTART=            1         51          1         51          1         51
SSTOP=           500        550        500        550        500        550

LSTART=            1         51          1         51          1         51
LSTOP=           500        550        500        550        500        550
SSTART=            1         51          1         51          1         51
SSTOP=           500        550        500        550        500        550

LSTART=            1         51          1         51          1         51
LSTOP=           500        550        500        550        500        550
SSTART=            1         51          1         51          1         51
SSTOP=           500        550        500        550        500        550

LSTART=            1         51
LSTOP=           500        550
SSTART=            1         51
SSTOP=           500        550

gen A3D NL=10 NS=10 IVAL=50 LINC=0 SINC=0 NB=10
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen B3D NL=10 NS=10 IVAL=100 LINC=0 SINC=0 NB=10
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen C3D NL=10 NS=10 IVAL=200 LINC=0 SINC=0 NB=10
Beginning VICAR task gen
GEN Version 6
GEN task completed
newmos (A3D,B3D,C3D) D3D NL=10 NS=30 LCP=1 SCP=30 PIXL=(1,30,1,20,1,10) INC=1 'AVER
Beginning VICAR task newmos
*** NEWMOS version 18-Aug-03 ***
SYSTEM LABEL SAYS INPUT IS BYTE

LINE=              1          1          1
SAMP=             30         20         10
NOLINE=           10         10         10
NOSAMP=           10         10         10

LTHRES,RTHRE=        1         1
ITHRES=              1
DCLE=              0

LSTART=            1          1          1
LSTOP=            10         10         10
SSTART=            1         11         21
SSTOP=            10         20         30

list D3D
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Mon Aug 18 15:19:04 2003
 Task:NEWMOS    User:ntt       Date_Time:Mon Aug 18 15:19:04 2003
 ***********
 Band =     1
 ***********
     Samp     1       3       5       7       9      11      13      15      17      19      21      23      25      27      29
   Line
      1      50  50  50  50  50  50  50  50  50  50 100 100 100 100 100 100 100 100 100 100 200 200 200 200 200 200 200 200 200 200
      2      50  50  50  50  50  50  50  50  50  50 100 100 100 100 100 100 100 100 100 100 200 200 200 200 200 200 200 200 200 200
      3      50  50  50  50  50  50  50  50  50  50 100 100 100 100 100 100 100 100 100 100 200 200 200 200 200 200 200 200 200 200
      4      50  50  50  50  50  50  50  50  50  50 100 100 100 100 100 100 100 100 100 100 200 200 200 200 200 200 200 200 200 200
      5      50  50  50  50  50  50  50  50  50  50 100 100 100 100 100 100 100 100 100 100 200 200 200 200 200 200 200 200 200 200
      6      50  50  50  50  50  50  50  50  50  50 100 100 100 100 100 100 100 100 100 100 200 200 200 200 200 200 200 200 200 200
      7      50  50  50  50  50  50  50  50  50  50 100 100 100 100 100 100 100 100 100 100 200 200 200 200 200 200 200 200 200 200
      8      50  50  50  50  50  50  50  50  50  50 100 100 100 100 100 100 100 100 100 100 200 200 200 200 200 200 200 200 200 200
      9      50  50  50  50  50  50  50  50  50  50 100 100 100 100 100 100 100 100 100 100 200 200 200 200 200 200 200 200 200 200
     10      50  50  50  50  50  50  50  50  50  50 100 100 100 100 100 100 100 100 100 100 200 200 200 200 200 200 200 200 200 200


 Task:GEN       User:ntt       Date_Time:Mon Aug 18 15:19:04 2003
 Task:NEWMOS    User:ntt       Date_Time:Mon Aug 18 15:19:04 2003
 ***********
 Band =     2
 ***********
     Samp     1       3       5       7       9      11      13      15      17      19      21      23      25      27      29
   Line
      1      51  51  51  51  51  51  51  51  51  51 101 101 101 101 101 101 101 101 101 101 201 201 201 201 201 201 201 201 201 201
      2      51  51  51  51  51  51  51  51  51  51 101 101 101 101 101 101 101 101 101 101 201 201 201 201 201 201 201 201 201 201
      3      51  51  51  51  51  51  51  51  51  51 101 101 101 101 101 101 101 101 101 101 201 201 201 201 201 201 201 201 201 201
      4      51  51  51  51  51  51  51  51  51  51 101 101 101 101 101 101 101 101 101 101 201 201 201 201 201 201 201 201 201 201
      5      51  51  51  51  51  51  51  51  51  51 101 101 101 101 101 101 101 101 101 101 201 201 201 201 201 201 201 201 201 201
      6      51  51  51  51  51  51  51  51  51  51 101 101 101 101 101 101 101 101 101 101 201 201 201 201 201 201 201 201 201 201
      7      51  51  51  51  51  51  51  51  51  51 101 101 101 101 101 101 101 101 101 101 201 201 201 201 201 201 201 201 201 201
      8      51  51  51  51  51  51  51  51  51  51 101 101 101 101 101 101 101 101 101 101 201 201 201 201 201 201 201 201 201 201
      9      51  51  51  51  51  51  51  51  51  51 101 101 101 101 101 101 101 101 101 101 201 201 201 201 201 201 201 201 201 201
     10      51  51  51  51  51  51  51  51  51  51 101 101 101 101 101 101 101 101 101 101 201 201 201 201 201 201 201 201 201 201


 Task:GEN       User:ntt       Date_Time:Mon Aug 18 15:19:04 2003
 Task:NEWMOS    User:ntt       Date_Time:Mon Aug 18 15:19:04 2003
 ***********
 Band =     3
 ***********
     Samp     1       3       5       7       9      11      13      15      17      19      21      23      25      27      29
   Line
      1      52  52  52  52  52  52  52  52  52  52 102 102 102 102 102 102 102 102 102 102 202 202 202 202 202 202 202 202 202 202
      2      52  52  52  52  52  52  52  52  52  52 102 102 102 102 102 102 102 102 102 102 202 202 202 202 202 202 202 202 202 202
      3      52  52  52  52  52  52  52  52  52  52 102 102 102 102 102 102 102 102 102 102 202 202 202 202 202 202 202 202 202 202
      4      52  52  52  52  52  52  52  52  52  52 102 102 102 102 102 102 102 102 102 102 202 202 202 202 202 202 202 202 202 202
      5      52  52  52  52  52  52  52  52  52  52 102 102 102 102 102 102 102 102 102 102 202 202 202 202 202 202 202 202 202 202
      6      52  52  52  52  52  52  52  52  52  52 102 102 102 102 102 102 102 102 102 102 202 202 202 202 202 202 202 202 202 202
      7      52  52  52  52  52  52  52  52  52  52 102 102 102 102 102 102 102 102 102 102 202 202 202 202 202 202 202 202 202 202
      8      52  52  52  52  52  52  52  52  52  52 102 102 102 102 102 102 102 102 102 102 202 202 202 202 202 202 202 202 202 202
      9      52  52  52  52  52  52  52  52  52  52 102 102 102 102 102 102 102 102 102 102 202 202 202 202 202 202 202 202 202 202
     10      52  52  52  52  52  52  52  52  52  52 102 102 102 102 102 102 102 102 102 102 202 202 202 202 202 202 202 202 202 202


 Task:GEN       User:ntt       Date_Time:Mon Aug 18 15:19:04 2003
 Task:NEWMOS    User:ntt       Date_Time:Mon Aug 18 15:19:04 2003
 ***********
 Band =     4
 ***********
     Samp     1       3       5       7       9      11      13      15      17      19      21      23      25      27      29
   Line
      1      53  53  53  53  53  53  53  53  53  53 103 103 103 103 103 103 103 103 103 103 203 203 203 203 203 203 203 203 203 203
      2      53  53  53  53  53  53  53  53  53  53 103 103 103 103 103 103 103 103 103 103 203 203 203 203 203 203 203 203 203 203
      3      53  53  53  53  53  53  53  53  53  53 103 103 103 103 103 103 103 103 103 103 203 203 203 203 203 203 203 203 203 203
      4      53  53  53  53  53  53  53  53  53  53 103 103 103 103 103 103 103 103 103 103 203 203 203 203 203 203 203 203 203 203
      5      53  53  53  53  53  53  53  53  53  53 103 103 103 103 103 103 103 103 103 103 203 203 203 203 203 203 203 203 203 203
      6      53  53  53  53  53  53  53  53  53  53 103 103 103 103 103 103 103 103 103 103 203 203 203 203 203 203 203 203 203 203
      7      53  53  53  53  53  53  53  53  53  53 103 103 103 103 103 103 103 103 103 103 203 203 203 203 203 203 203 203 203 203
      8      53  53  53  53  53  53  53  53  53  53 103 103 103 103 103 103 103 103 103 103 203 203 203 203 203 203 203 203 203 203
      9      53  53  53  53  53  53  53  53  53  53 103 103 103 103 103 103 103 103 103 103 203 203 203 203 203 203 203 203 203 203
     10      53  53  53  53  53  53  53  53  53  53 103 103 103 103 103 103 103 103 103 103 203 203 203 203 203 203 203 203 203 203


 Task:GEN       User:ntt       Date_Time:Mon Aug 18 15:19:04 2003
 Task:NEWMOS    User:ntt       Date_Time:Mon Aug 18 15:19:04 2003
 ***********
 Band =     5
 ***********
     Samp     1       3       5       7       9      11      13      15      17      19      21      23      25      27      29
   Line
      1      54  54  54  54  54  54  54  54  54  54 104 104 104 104 104 104 104 104 104 104 204 204 204 204 204 204 204 204 204 204
      2      54  54  54  54  54  54  54  54  54  54 104 104 104 104 104 104 104 104 104 104 204 204 204 204 204 204 204 204 204 204
      3      54  54  54  54  54  54  54  54  54  54 104 104 104 104 104 104 104 104 104 104 204 204 204 204 204 204 204 204 204 204
      4      54  54  54  54  54  54  54  54  54  54 104 104 104 104 104 104 104 104 104 104 204 204 204 204 204 204 204 204 204 204
      5      54  54  54  54  54  54  54  54  54  54 104 104 104 104 104 104 104 104 104 104 204 204 204 204 204 204 204 204 204 204
      6      54  54  54  54  54  54  54  54  54  54 104 104 104 104 104 104 104 104 104 104 204 204 204 204 204 204 204 204 204 204
      7      54  54  54  54  54  54  54  54  54  54 104 104 104 104 104 104 104 104 104 104 204 204 204 204 204 204 204 204 204 204
      8      54  54  54  54  54  54  54  54  54  54 104 104 104 104 104 104 104 104 104 104 204 204 204 204 204 204 204 204 204 204
      9      54  54  54  54  54  54  54  54  54  54 104 104 104 104 104 104 104 104 104 104 204 204 204 204 204 204 204 204 204 204
     10      54  54  54  54  54  54  54  54  54  54 104 104 104 104 104 104 104 104 104 104 204 204 204 204 204 204 204 204 204 204


 Task:GEN       User:ntt       Date_Time:Mon Aug 18 15:19:04 2003
 Task:NEWMOS    User:ntt       Date_Time:Mon Aug 18 15:19:04 2003
 ***********
 Band =     6
 ***********
     Samp     1       3       5       7       9      11      13      15      17      19      21      23      25      27      29
   Line
      1      55  55  55  55  55  55  55  55  55  55 105 105 105 105 105 105 105 105 105 105 205 205 205 205 205 205 205 205 205 205
      2      55  55  55  55  55  55  55  55  55  55 105 105 105 105 105 105 105 105 105 105 205 205 205 205 205 205 205 205 205 205
      3      55  55  55  55  55  55  55  55  55  55 105 105 105 105 105 105 105 105 105 105 205 205 205 205 205 205 205 205 205 205
      4      55  55  55  55  55  55  55  55  55  55 105 105 105 105 105 105 105 105 105 105 205 205 205 205 205 205 205 205 205 205
      5      55  55  55  55  55  55  55  55  55  55 105 105 105 105 105 105 105 105 105 105 205 205 205 205 205 205 205 205 205 205
      6      55  55  55  55  55  55  55  55  55  55 105 105 105 105 105 105 105 105 105 105 205 205 205 205 205 205 205 205 205 205
      7      55  55  55  55  55  55  55  55  55  55 105 105 105 105 105 105 105 105 105 105 205 205 205 205 205 205 205 205 205 205
      8      55  55  55  55  55  55  55  55  55  55 105 105 105 105 105 105 105 105 105 105 205 205 205 205 205 205 205 205 205 205
      9      55  55  55  55  55  55  55  55  55  55 105 105 105 105 105 105 105 105 105 105 205 205 205 205 205 205 205 205 205 205
     10      55  55  55  55  55  55  55  55  55  55 105 105 105 105 105 105 105 105 105 105 205 205 205 205 205 205 205 205 205 205


 Task:GEN       User:ntt       Date_Time:Mon Aug 18 15:19:04 2003
 Task:NEWMOS    User:ntt       Date_Time:Mon Aug 18 15:19:04 2003
 ***********
 Band =     7
 ***********
     Samp     1       3       5       7       9      11      13      15      17      19      21      23      25      27      29
   Line
      1      56  56  56  56  56  56  56  56  56  56 106 106 106 106 106 106 106 106 106 106 206 206 206 206 206 206 206 206 206 206
      2      56  56  56  56  56  56  56  56  56  56 106 106 106 106 106 106 106 106 106 106 206 206 206 206 206 206 206 206 206 206
      3      56  56  56  56  56  56  56  56  56  56 106 106 106 106 106 106 106 106 106 106 206 206 206 206 206 206 206 206 206 206
      4      56  56  56  56  56  56  56  56  56  56 106 106 106 106 106 106 106 106 106 106 206 206 206 206 206 206 206 206 206 206
      5      56  56  56  56  56  56  56  56  56  56 106 106 106 106 106 106 106 106 106 106 206 206 206 206 206 206 206 206 206 206
      6      56  56  56  56  56  56  56  56  56  56 106 106 106 106 106 106 106 106 106 106 206 206 206 206 206 206 206 206 206 206
      7      56  56  56  56  56  56  56  56  56  56 106 106 106 106 106 106 106 106 106 106 206 206 206 206 206 206 206 206 206 206
      8      56  56  56  56  56  56  56  56  56  56 106 106 106 106 106 106 106 106 106 106 206 206 206 206 206 206 206 206 206 206
      9      56  56  56  56  56  56  56  56  56  56 106 106 106 106 106 106 106 106 106 106 206 206 206 206 206 206 206 206 206 206
     10      56  56  56  56  56  56  56  56  56  56 106 106 106 106 106 106 106 106 106 106 206 206 206 206 206 206 206 206 206 206


 Task:GEN       User:ntt       Date_Time:Mon Aug 18 15:19:04 2003
 Task:NEWMOS    User:ntt       Date_Time:Mon Aug 18 15:19:04 2003
 ***********
 Band =     8
 ***********
     Samp     1       3       5       7       9      11      13      15      17      19      21      23      25      27      29
   Line
      1      57  57  57  57  57  57  57  57  57  57 107 107 107 107 107 107 107 107 107 107 207 207 207 207 207 207 207 207 207 207
      2      57  57  57  57  57  57  57  57  57  57 107 107 107 107 107 107 107 107 107 107 207 207 207 207 207 207 207 207 207 207
      3      57  57  57  57  57  57  57  57  57  57 107 107 107 107 107 107 107 107 107 107 207 207 207 207 207 207 207 207 207 207
      4      57  57  57  57  57  57  57  57  57  57 107 107 107 107 107 107 107 107 107 107 207 207 207 207 207 207 207 207 207 207
      5      57  57  57  57  57  57  57  57  57  57 107 107 107 107 107 107 107 107 107 107 207 207 207 207 207 207 207 207 207 207
      6      57  57  57  57  57  57  57  57  57  57 107 107 107 107 107 107 107 107 107 107 207 207 207 207 207 207 207 207 207 207
      7      57  57  57  57  57  57  57  57  57  57 107 107 107 107 107 107 107 107 107 107 207 207 207 207 207 207 207 207 207 207
      8      57  57  57  57  57  57  57  57  57  57 107 107 107 107 107 107 107 107 107 107 207 207 207 207 207 207 207 207 207 207
      9      57  57  57  57  57  57  57  57  57  57 107 107 107 107 107 107 107 107 107 107 207 207 207 207 207 207 207 207 207 207
     10      57  57  57  57  57  57  57  57  57  57 107 107 107 107 107 107 107 107 107 107 207 207 207 207 207 207 207 207 207 207


 Task:GEN       User:ntt       Date_Time:Mon Aug 18 15:19:04 2003
 Task:NEWMOS    User:ntt       Date_Time:Mon Aug 18 15:19:04 2003
 ***********
 Band =     9
 ***********
     Samp     1       3       5       7       9      11      13      15      17      19      21      23      25      27      29
   Line
      1      58  58  58  58  58  58  58  58  58  58 108 108 108 108 108 108 108 108 108 108 208 208 208 208 208 208 208 208 208 208
      2      58  58  58  58  58  58  58  58  58  58 108 108 108 108 108 108 108 108 108 108 208 208 208 208 208 208 208 208 208 208
      3      58  58  58  58  58  58  58  58  58  58 108 108 108 108 108 108 108 108 108 108 208 208 208 208 208 208 208 208 208 208
      4      58  58  58  58  58  58  58  58  58  58 108 108 108 108 108 108 108 108 108 108 208 208 208 208 208 208 208 208 208 208
      5      58  58  58  58  58  58  58  58  58  58 108 108 108 108 108 108 108 108 108 108 208 208 208 208 208 208 208 208 208 208
      6      58  58  58  58  58  58  58  58  58  58 108 108 108 108 108 108 108 108 108 108 208 208 208 208 208 208 208 208 208 208
      7      58  58  58  58  58  58  58  58  58  58 108 108 108 108 108 108 108 108 108 108 208 208 208 208 208 208 208 208 208 208
      8      58  58  58  58  58  58  58  58  58  58 108 108 108 108 108 108 108 108 108 108 208 208 208 208 208 208 208 208 208 208
      9      58  58  58  58  58  58  58  58  58  58 108 108 108 108 108 108 108 108 108 108 208 208 208 208 208 208 208 208 208 208
     10      58  58  58  58  58  58  58  58  58  58 108 108 108 108 108 108 108 108 108 108 208 208 208 208 208 208 208 208 208 208


 Task:GEN       User:ntt       Date_Time:Mon Aug 18 15:19:04 2003
 Task:NEWMOS    User:ntt       Date_Time:Mon Aug 18 15:19:04 2003
 ***********
 Band =    10
 ***********
     Samp     1       3       5       7       9      11      13      15      17      19      21      23      25      27      29
   Line
      1      59  59  59  59  59  59  59  59  59  59 109 109 109 109 109 109 109 109 109 109 209 209 209 209 209 209 209 209 209 209
      2      59  59  59  59  59  59  59  59  59  59 109 109 109 109 109 109 109 109 109 109 209 209 209 209 209 209 209 209 209 209
      3      59  59  59  59  59  59  59  59  59  59 109 109 109 109 109 109 109 109 109 109 209 209 209 209 209 209 209 209 209 209
      4      59  59  59  59  59  59  59  59  59  59 109 109 109 109 109 109 109 109 109 109 209 209 209 209 209 209 209 209 209 209
      5      59  59  59  59  59  59  59  59  59  59 109 109 109 109 109 109 109 109 109 109 209 209 209 209 209 209 209 209 209 209
      6      59  59  59  59  59  59  59  59  59  59 109 109 109 109 109 109 109 109 109 109 209 209 209 209 209 209 209 209 209 209
      7      59  59  59  59  59  59  59  59  59  59 109 109 109 109 109 109 109 109 109 109 209 209 209 209 209 209 209 209 209 209
      8      59  59  59  59  59  59  59  59  59  59 109 109 109 109 109 109 109 109 109 109 209 209 209 209 209 209 209 209 209 209
      9      59  59  59  59  59  59  59  59  59  59 109 109 109 109 109 109 109 109 109 109 209 209 209 209 209 209 209 209 209 209
     10      59  59  59  59  59  59  59  59  59  59 109 109 109 109 109 109 109 109 109 109 209 209 209 209 209 209 209 209 209 209
end-proc
disable-log
$!-----------------------------------------------------------------------------
$ create old_session_3d.log
tstnewmos
THIS IS A TEST FOR THE PROGRAM NEWMOS

gen A NL=10 NS=10 IVAL=50 LINC=0 SINC=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen B NL=10 NS=10 IVAL=100 LINC=0 SINC=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen C NL=10 NS=10 IVAL=200 LINC=0 SINC=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen AA NL=10 NS=10 IVAL=50 LINC=0 SINC=0  'HALF
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen BB NL=10 NS=10 IVAL=100 LINC=0 SINC=0 'HALF
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen CC NL=10 NS=10 IVAL=200 LINC=0 SINC=0 'HALF
Beginning VICAR task gen
GEN Version 6
GEN task completed
let $echo = "NO"

TEST THE OFFSET PARAMETERS

newmos (A,B,C) D NL=10 NS=30 LCP=1 SCP=30 PIXL=(1,30,1,20,1,10) INC=1
Beginning VICAR task newmos
*** NEWMOS version 28-Jan-99 ***
SYSTEM LABEL SAYS INPUT IS BYTE

LINE=              1          1          1
SAMP=             30         20         10
NOLINE=           10         10         10
NOSAMP=           10         10         10

LTHRES,RTHRE=        1         1
ITHRES=              1
DCLE=              0

LSTART=            1          1          1
LSTOP=            10         10         10
SSTART=            1         11         21
SSTOP=            10         20         30

list D
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Mon Aug 18 15:19:42 2003
 Task:NEWMOS    User:ntt       Date_Time:Mon Aug 18 15:19:43 2003
     Samp     1       3       5       7       9      11      13      15      17      19      21      23      25      27      29
   Line
      1      50  50  50  50  50  50  50  50  50  50 100 100 100 100 100 100 100 100 100 100 200 200 200 200 200 200 200 200 200 200
      2      50  50  50  50  50  50  50  50  50  50 100 100 100 100 100 100 100 100 100 100 200 200 200 200 200 200 200 200 200 200
      3      50  50  50  50  50  50  50  50  50  50 100 100 100 100 100 100 100 100 100 100 200 200 200 200 200 200 200 200 200 200
      4      50  50  50  50  50  50  50  50  50  50 100 100 100 100 100 100 100 100 100 100 200 200 200 200 200 200 200 200 200 200
      5      50  50  50  50  50  50  50  50  50  50 100 100 100 100 100 100 100 100 100 100 200 200 200 200 200 200 200 200 200 200
      6      50  50  50  50  50  50  50  50  50  50 100 100 100 100 100 100 100 100 100 100 200 200 200 200 200 200 200 200 200 200
      7      50  50  50  50  50  50  50  50  50  50 100 100 100 100 100 100 100 100 100 100 200 200 200 200 200 200 200 200 200 200
      8      50  50  50  50  50  50  50  50  50  50 100 100 100 100 100 100 100 100 100 100 200 200 200 200 200 200 200 200 200 200
      9      50  50  50  50  50  50  50  50  50  50 100 100 100 100 100 100 100 100 100 100 200 200 200 200 200 200 200 200 200 200
     10      50  50  50  50  50  50  50  50  50  50 100 100 100 100 100 100 100 100 100 100 200 200 200 200 200 200 200 200 200 200
let $echo = "NO"

TEST THE PARAM AVERAGE

newmos (A,B) D NL=10 NS=15 LCP=1 SCP=15 PIXL=(1,15,1,10) INC=1 'AVER
Beginning VICAR task newmos
*** NEWMOS version 28-Jan-99 ***
SYSTEM LABEL SAYS INPUT IS BYTE

LINE=              1          1
SAMP=             15         10
NOLINE=           10         10
NOSAMP=           10         10

LTHRES,RTHRE=        1         1
ITHRES=              1
DCLE=              0

LSTART=            1          1
LSTOP=            10         10
SSTART=            1          6
SSTOP=            10         15

LIST D
Beginning VICAR task LIST

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Mon Aug 18 15:19:42 2003
 Task:NEWMOS    User:ntt       Date_Time:Mon Aug 18 15:19:43 2003
     Samp     1       3       5       7       9      11      13      15
   Line
      1      50  50  50  50  50  75  75  75  75  75 100 100 100 100 100
      2      50  50  50  50  50  75  75  75  75  75 100 100 100 100 100
      3      50  50  50  50  50  75  75  75  75  75 100 100 100 100 100
      4      50  50  50  50  50  75  75  75  75  75 100 100 100 100 100
      5      50  50  50  50  50  75  75  75  75  75 100 100 100 100 100
      6      50  50  50  50  50  75  75  75  75  75 100 100 100 100 100
      7      50  50  50  50  50  75  75  75  75  75 100 100 100 100 100
      8      50  50  50  50  50  75  75  75  75  75 100 100 100 100 100
      9      50  50  50  50  50  75  75  75  75  75 100 100 100 100 100
     10      50  50  50  50  50  75  75  75  75  75 100 100 100 100 100
let $echo = "NO"

TEST THE PARAM HALF

newmos (AA,BB,CC) E NL=10 NS=30 LCP=1  SCP=15 PIXL=(1,15,1,10,1,5) INC=1 'HALF
Beginning VICAR task newmos
*** NEWMOS version 28-Jan-99 ***
SYSTEM LABEL SAYS INPUT IS HALFWORD

LINE=              1          1          1
SAMP=             15         10          5
NOLINE=           10         10         10
NOSAMP=           10         10         10

LTHRES,RTHRE=        1         1
ITHRES=              1
DCLE=              0

LSTART=            1          1          1
LSTOP=            10         10         10
SSTART=            1          6         11
SSTOP=            10         15         20

LIST E
Beginning VICAR task LIST

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:ntt       Date_Time:Mon Aug 18 15:19:42 2003
 Task:NEWMOS    User:ntt       Date_Time:Mon Aug 18 15:19:44 2003
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14    15
   Line
      1        50    50    50    50    50    50    50    50    50    50   100   100   100   100   100
      2        50    50    50    50    50    50    50    50    50    50   100   100   100   100   100
      3        50    50    50    50    50    50    50    50    50    50   100   100   100   100   100
      4        50    50    50    50    50    50    50    50    50    50   100   100   100   100   100
      5        50    50    50    50    50    50    50    50    50    50   100   100   100   100   100
      6        50    50    50    50    50    50    50    50    50    50   100   100   100   100   100
      7        50    50    50    50    50    50    50    50    50    50   100   100   100   100   100
      8        50    50    50    50    50    50    50    50    50    50   100   100   100   100   100
      9        50    50    50    50    50    50    50    50    50    50   100   100   100   100   100
     10        50    50    50    50    50    50    50    50    50    50   100   100   100   100   100

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:ntt       Date_Time:Mon Aug 18 15:19:42 2003
 Task:NEWMOS    User:ntt       Date_Time:Mon Aug 18 15:19:44 2003
     Samp      16    17    18    19    20    21    22    23    24    25    26    27    28    29    30
   Line
      1       200   200   200   200   200     0     0     0     0     0     0     0     0     0     0
      2       200   200   200   200   200     0     0     0     0     0     0     0     0     0     0
      3       200   200   200   200   200     0     0     0     0     0     0     0     0     0     0
      4       200   200   200   200   200     0     0     0     0     0     0     0     0     0     0
      5       200   200   200   200   200     0     0     0     0     0     0     0     0     0     0
      6       200   200   200   200   200     0     0     0     0     0     0     0     0     0     0
      7       200   200   200   200   200     0     0     0     0     0     0     0     0     0     0
      8       200   200   200   200   200     0     0     0     0     0     0     0     0     0     0
      9       200   200   200   200   200     0     0     0     0     0     0     0     0     0     0
     10       200   200   200   200   200     0     0     0     0     0     0     0     0     0     0
gen A NL=10 NS=10 IVAL=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen B NL=10 NS=10 IVAL=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen C NL=10 NS=10 IVAL=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
let $echo="no"

TEST THE EDGE DETECTION PARAM THRE

newmos (A,B,C) D NL=10 NS=30 LCP=1 SCP=30 PIXL=(1,30,1,20,1,10) INC=1 THRE=10
Beginning VICAR task newmos
*** NEWMOS version 28-Jan-99 ***
SYSTEM LABEL SAYS INPUT IS BYTE

LINE=              1          1          1
SAMP=             30         20         10
NOLINE=           10         10         10
NOSAMP=           10         10         10

LTHRES,RTHRE=       10        10
ITHRES=             10
DCLE=              0

LSTART=            1          1          1
LSTOP=            10         10         10
SSTART=            1         11         21
SSTOP=            10         20         30

list D
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Mon Aug 18 15:19:44 2003
 Task:NEWMOS    User:ntt       Date_Time:Mon Aug 18 15:19:45 2003
     Samp     1       3       5       7       9      11      13      15      17      19      21      23      25      27      29
   Line

      4       0   0   0   0   0   0   0  10  11  12   0   0   0   0   0   0   0  10  11  12   0   0   0   0   0   0   0  10  11  12
      5       0   0   0   0   0   0  10  11  12  13   0   0   0   0   0   0  10  11  12  13   0   0   0   0   0   0  10  11  12  13
      6       0   0   0   0   0  10  11  12  13  14   0   0   0   0   0  10  11  12  13  14   0   0   0   0   0  10  11  12  13  14
      7       0   0   0   0  10  11  12  13  14  15   0   0   0   0  10  11  12  13  14  15   0   0   0   0  10  11  12  13  14  15
      8       0   0   0  10  11  12  13  14  15  16   0   0   0  10  11  12  13  14  15  16   0   0   0  10  11  12  13  14  15  16
      9       0   0  10  11  12  13  14  15  16  17   0   0  10  11  12  13  14  15  16  17   0   0  10  11  12  13  14  15  16  17
     10       0  10  11  12  13  14  15  16  17  18   0  10  11  12  13  14  15  16  17  18   0  10  11  12  13  14  15  16  17  18
let $echo= "no"

TEST THE PARM DCLE

newmos (A,B,C) D NL=10 NS=30 LCP=1 SCP=30 PIXL=(1,30,1,20,1,10) INC=1 THRE=10 DCLE=5
Beginning VICAR task newmos
*** NEWMOS version 28-Jan-99 ***
SYSTEM LABEL SAYS INPUT IS BYTE

LINE=              1          1          1
SAMP=             30         20         10
NOLINE=           10         10         10
NOSAMP=           10         10         10

LTHRES,RTHRE=       10        10
ITHRES=             10
DCLE=              5

LSTART=            1          1          1
LSTOP=            10         10         10
SSTART=            1         11         21
SSTOP=            10         20         30

list D
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Mon Aug 18 15:19:44 2003
 Task:NEWMOS    User:ntt       Date_Time:Mon Aug 18 15:19:45 2003
     Samp     1       3       5       7       9      11      13      15      17      19      21      23      25      27      29
   Line
      1       5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5
      2       5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5
      3       5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5
      4       5   5   5   5   5   5   5  10  11  12   5   5   5   5   5   5   5  10  11  12   5   5   5   5   5   5   5  10  11  12
      5       5   5   5   5   5   5  10  11  12  13   5   5   5   5   5   5  10  11  12  13   5   5   5   5   5   5  10  11  12  13
      6       5   5   5   5   5  10  11  12  13  14   5   5   5   5   5  10  11  12  13  14   5   5   5   5   5  10  11  12  13  14
      7       5   5   5   5  10  11  12  13  14  15   5   5   5   5  10  11  12  13  14  15   5   5   5   5  10  11  12  13  14  15
      8       5   5   5  10  11  12  13  14  15  16   5   5   5  10  11  12  13  14  15  16   5   5   5  10  11  12  13  14  15  16
      9       5   5  10  11  12  13  14  15  16  17   5   5  10  11  12  13  14  15  16  17   5   5  10  11  12  13  14  15  16  17
     10       5  10  11  12  13  14  15  16  17  18   5  10  11  12  13  14  15  16  17  18   5  10  11  12  13  14  15  16  17  18
write ""

write "TEST THE PARAMETERS MAP2, ADAPT, NOMIN.  SHOULD GET 0 DIFFERENCES"
TEST THE PARAMETERS MAP2, ADAPT, NOMIN.  SHOULD GET 0 DIFFERENCES
write ""

MAP3 geo cylmap NL=360 NS=500 'CYLI 'VGR1 'HALF SCALE=10.  +
    LINE=100. SAMP=101. LATI=80. LONG=240. 'remote target=io
Beginning VICAR task MAP3
Map3 version 6-Dec-1999
 LABEL SAYS INPUT PICTURE IS OBJECT SPACE
    CAMERA=          7
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
GETLABCON: warning ind=          1
flight label
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Actual source of SEDR is: DAVI
OM matrix obtained from SEDR
RS vector obtained from SEDR
Target radii obtained from SEDR
 JULIAN DATE OF EVENT FRAME=  2443937.3
/CAM FOC L/    RA   /    RB   /    RC   /   RMAG  /LA SSC PT/LO SSC PT/
/ 127248.2/ 1829.400/ 1819.300/ 1815.700/ 806029.9/   -0.032/  156.474/
    OM MATRIX
/ 0.225295/-0.516547/ 0.826088
/ 0.330342/-0.757163/-0.563541
/ 0.916579/ 0.399854/ 0.000052
    RS VECTOR (TARGET COORDINATES)
       -739030.7       -321741.5          -456.1
COORDINATES OF CENTER OF INPUT PICTURE--LATITUDE = -13.09, LONGITUDE = 176.23
PROJECTION IS NORMAL CYLINDRICAL
 DATA=
            8.650E+02  2.790E+02  0.000E+00  0.000E+00  0.000E+00  2.713E+02  1.000E+01  1.000E+00  5.570E+01  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  1.816E+03  1.829E+03  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
MAP3 geo cylmap2 NL=360 NS=500 'CYLI 'VGR1 'HALF SCALE=10.  +
    LINE=0. SAMP=1. LATI=80. LONG=240. 'remote target=io
Beginning VICAR task MAP3
Map3 version 6-Dec-1999
 LABEL SAYS INPUT PICTURE IS OBJECT SPACE
    CAMERA=          7
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
GETLABCON: warning ind=          1
flight label
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Actual source of SEDR is: DAVI
OM matrix obtained from SEDR
RS vector obtained from SEDR
Target radii obtained from SEDR
 JULIAN DATE OF EVENT FRAME=  2443937.3
/CAM FOC L/    RA   /    RB   /    RC   /   RMAG  /LA SSC PT/LO SSC PT/
/ 127248.2/ 1829.400/ 1819.300/ 1815.700/ 806029.9/   -0.032/  156.474/
    OM MATRIX
/ 0.225295/-0.516547/ 0.826088
/ 0.330342/-0.757163/-0.563541
/ 0.916579/ 0.399854/ 0.000052
    RS VECTOR (TARGET COORDINATES)
       -739030.7       -321741.5          -456.1
COORDINATES OF CENTER OF INPUT PICTURE--LATITUDE = -13.09, LONGITUDE = 176.23
PROJECTION IS NORMAL CYLINDRICAL
 DATA=
            7.650E+02  1.790E+02  0.000E+00  0.000E+00  0.000E+00  2.399E+02  1.000E+01  1.000E+00  5.570E+01  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  1.816E+03  1.829E+03  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
newmos (cylmap cylmap2) mos (1 1 800 800) 'ADAPT  THRESH=-32767
Beginning VICAR task newmos
*** NEWMOS version 28-Jan-99 ***
SYSTEM LABEL SAYS INPUT IS HALFWORD
LONG=      0.2713E+03  0.2399E+03
SEARCW FOUND SAMP=  867       767
THE PROGRAM CHOSE LCP=   279 AND SCP=   867
RPOL,REQ USED=      0.1816E+04 0.1829E+04
THE OUTPUT PICTURE WILL CONTAIN   460 LINES AND    600 SAMPLES

LINE=            279        179
SAMP=            867        767
NOLINE=          360        360
NOSAMP=          500        500

LTHRES,RTHRE=   -32767    -32767
ITHRES=         -32767
DCLE=              0

LSTART=            1        101
LSTOP=           360        460
SSTART=            1        101
SSTOP=           500        600

f2 (cylmap mos) DIF FUNC="IN1-IN2"
Beginning VICAR task f2
F2 version 2-04-94
LINES TRUNCATED
SAMPLES TRUNCATED
F2 using hash table lookup
FUNCTION EVALUATED 5728 TIMES
hist DIF 'NOHIS
Beginning VICAR task hist
HIST version 30-JUL-03


AVERAGE GRAY LEVEL=0.000000       STANDARD DEVIATION=0.000000       NUMBER ELEMENTS=  180000
MIN. DN=         0
MAX. DN=         0

MAP3 geo cylmap3 NL=360 NS=500 'RECT 'VGR1 'HALF SCALE=10.  +
    LINE=100. SAMP=101. LATI=80. LONG=240. 'remote target=io
Beginning VICAR task MAP3
Map3 version 6-Dec-1999
 LABEL SAYS INPUT PICTURE IS OBJECT SPACE
    CAMERA=          7
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
GETLABCON: warning ind=          1
flight label
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Actual source of SEDR is: DAVI
OM matrix obtained from SEDR
RS vector obtained from SEDR
Target radii obtained from SEDR
 JULIAN DATE OF EVENT FRAME=  2443937.3
/CAM FOC L/    RA   /    RB   /    RC   /   RMAG  /LA SSC PT/LO SSC PT/
/ 127248.2/ 1829.400/ 1819.300/ 1815.700/ 806029.9/   -0.032/  156.474/
    OM MATRIX
/ 0.225295/-0.516547/ 0.826088
/ 0.330342/-0.757163/-0.563541
/ 0.916579/ 0.399854/ 0.000052
    RS VECTOR (TARGET COORDINATES)
       -739030.7       -321741.5          -456.1
COORDINATES OF CENTER OF INPUT PICTURE--LATITUDE = -13.09, LONGITUDE = 176.23
PROJECTION IS SIMPLE CYLINDRICAL
 DATA=
            1.000E+00  3.550E+02  0.000E+00  0.000E+00  0.000E+00  2.713E+02  1.000E+01  1.000E+00  5.570E+01  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  1.816E+03  1.829E+03  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
MAP3 geo cylmap4 NL=360 NS=500 'RECT 'VGR1 'HALF SCALE=10.  +
    LINE=0. SAMP=1. LATI=80. LONG=240. 'remote target=io
Beginning VICAR task MAP3
Map3 version 6-Dec-1999
 LABEL SAYS INPUT PICTURE IS OBJECT SPACE
    CAMERA=          7
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
GETLABCON: warning ind=          1
flight label
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Actual source of SEDR is: DAVI
OM matrix obtained from SEDR
RS vector obtained from SEDR
Target radii obtained from SEDR
 JULIAN DATE OF EVENT FRAME=  2443937.3
/CAM FOC L/    RA   /    RB   /    RC   /   RMAG  /LA SSC PT/LO SSC PT/
/ 127248.2/ 1829.400/ 1819.300/ 1815.700/ 806029.9/   -0.032/  156.474/
    OM MATRIX
/ 0.225295/-0.516547/ 0.826088
/ 0.330342/-0.757163/-0.563541
/ 0.916579/ 0.399854/ 0.000052
    RS VECTOR (TARGET COORDINATES)
       -739030.7       -321741.5          -456.1
COORDINATES OF CENTER OF INPUT PICTURE--LATITUDE = -13.09, LONGITUDE = 176.23
PROJECTION IS SIMPLE CYLINDRICAL
 DATA=
            1.000E+00  2.550E+02  0.000E+00  0.000E+00  0.000E+00  2.399E+02  1.000E+01  1.000E+00  5.570E+01  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  1.816E+03  1.829E+03  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
newmos (cylmap3 cylmap4) mos1 (1 1 800 800) 'MAP2 LCP=354 SCP=867  +
        THRESH=-32767
Beginning VICAR task newmos
*** NEWMOS version 28-Jan-99 ***
SYSTEM LABEL SAYS INPUT IS HALFWORD
LONG=      0.2713E+03  0.2399E+03
SEARCW FOUND SAMP=  867       767
RPOL,REQ USED=      0.1816E+04 0.1829E+04
THE OUTPUT PICTURE WILL CONTAIN   800 LINES AND    800 SAMPLES

LINE=            355        255
SAMP=            867        767
NOLINE=          360        360
NOSAMP=          500        500

***NUMBER OF OFFSETS=    1
***NUMBER OF INPUTS=     2
LTHRES,RTHRE=   -32767    -32767
ITHRES=         -32767
DCLE=              0

LSTART=            0        100
LSTOP=           359        459
SSTART=            1        101
SSTOP=           500        600

f2 (cylmap3 mos1) DIF FUNC="IN1-IN2"
Beginning VICAR task f2
F2 version 2-04-94
LINES TRUNCATED
SAMPLES TRUNCATED
F2 using hash table lookup
FUNCTION EVALUATED 119391 TIMES
hist DIF 'NOHIS
Beginning VICAR task hist
HIST version 30-JUL-03


AVERAGE GRAY LEVEL=-9.36664       STANDARD DEVIATION=119.5696       NUMBER ELEMENTS=  180000
MIN. DN=     -1677
MAX. DN=      3544

MAP3 geo cylmap5 NL=360 NS=500 'MERC 'VGR1 'HALF SCALE=10.  +
    LINE=100. SAMP=101. LATI=70. LONG=240. 'remote target=io
Beginning VICAR task MAP3
Map3 version 6-Dec-1999
 LABEL SAYS INPUT PICTURE IS OBJECT SPACE
    CAMERA=          7
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
GETLABCON: warning ind=          1
flight label
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Actual source of SEDR is: DAVI
OM matrix obtained from SEDR
RS vector obtained from SEDR
Target radii obtained from SEDR
 JULIAN DATE OF EVENT FRAME=  2443937.3
/CAM FOC L/    RA   /    RB   /    RC   /   RMAG  /LA SSC PT/LO SSC PT/
/ 127248.2/ 1829.400/ 1819.300/ 1815.700/ 806029.9/   -0.032/  156.474/
    OM MATRIX
/ 0.225295/-0.516547/ 0.826088
/ 0.330342/-0.757163/-0.563541
/ 0.916579/ 0.399854/ 0.000052
    RS VECTOR (TARGET COORDINATES)
       -739030.7       -321741.5          -456.1
COORDINATES OF CENTER OF INPUT PICTURE--LATITUDE = -13.09, LONGITUDE = 176.23
PROJECTION SPECIFIED IS MERCATOR
 DATA=
            1.000E+00  1.000E+00  7.830E+01  0.000E+00  0.000E+00  2.714E+02  1.000E+01  1.000E+00  5.570E+01  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  1.816E+03  1.829E+03  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
MAP3 geo cylmap6 NL=360 NS=500 'MERC 'VGR1 'HALF SCALE=10.  +
    LINE=0. SAMP=1. LATI=70. LONG=240. 'remote target=io
Beginning VICAR task MAP3
Map3 version 6-Dec-1999
 LABEL SAYS INPUT PICTURE IS OBJECT SPACE
    CAMERA=          7
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
GETLABCON: warning ind=          1
flight label
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Actual source of SEDR is: DAVI
OM matrix obtained from SEDR
RS vector obtained from SEDR
Target radii obtained from SEDR
 JULIAN DATE OF EVENT FRAME=  2443937.3
/CAM FOC L/    RA   /    RB   /    RC   /   RMAG  /LA SSC PT/LO SSC PT/
/ 127248.2/ 1829.400/ 1819.300/ 1815.700/ 806029.9/   -0.032/  156.474/
    OM MATRIX
/ 0.225295/-0.516547/ 0.826088
/ 0.330342/-0.757163/-0.563541
/ 0.916579/ 0.399854/ 0.000052
    RS VECTOR (TARGET COORDINATES)
       -739030.7       -321741.5          -456.1
COORDINATES OF CENTER OF INPUT PICTURE--LATITUDE = -13.09, LONGITUDE = 176.23
PROJECTION SPECIFIED IS MERCATOR
 DATA=
            1.000E+00  1.000E+00  6.989E+01  0.000E+00  0.000E+00  2.400E+02  1.000E+01  1.000E+00  5.570E+01  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  1.816E+03  1.829E+03  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
newmos (cylmap5 cylmap6) mos2 (1 1 800 800) 'ADAPT 'NOMIN  +
    weight=(1.,1.) THRESH=-32767
Beginning VICAR task newmos
*** NEWMOS version 28-Jan-99 ***
SYSTEM LABEL SAYS INPUT IS HALFWORD
LONG=      0.2714E+03  0.2400E+03
THE PROGRAM CHOSE LCP=   417 AND SCP=   868
RPOL,REQ USED=      0.1816E+04 0.1829E+04
THE OUTPUT PICTURE WILL CONTAIN   461 LINES AND    601 SAMPLES

LINE=            417        316
SAMP=            868        767
NOLINE=          360        360
NOSAMP=          500        500

LTHRES,RTHRE=   -32767    -32767
ITHRES=         -32767
DCLE=              0

LSTART=            1        102
LSTOP=           360        461
SSTART=            1        102
SSTOP=           500        601

f2 (cylmap5 mos2) DIF FUNC="IN1-IN2"
Beginning VICAR task f2
F2 version 2-04-94
LINES TRUNCATED
SAMPLES TRUNCATED
F2 using hash table lookup
FUNCTION EVALUATED 5718 TIMES
hist DIF 'NOHIS
Beginning VICAR task hist
HIST version 30-JUL-03


AVERAGE GRAY LEVEL=0.000000       STANDARD DEVIATION=0.000000       NUMBER ELEMENTS=  180000
MIN. DN=         0
MAX. DN=         0

MAP3 geo cylmap7 NL=360 NS=300 'RECT 'VGR1 'HALF SCALE=10.  +
    LINE=100. SAMP=101. LATI=80. LONG=240. 'remote target=io
Beginning VICAR task MAP3
Map3 version 6-Dec-1999
 LABEL SAYS INPUT PICTURE IS OBJECT SPACE
    CAMERA=          7
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
GETLABCON: warning ind=          1
flight label
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Actual source of SEDR is: DAVI
OM matrix obtained from SEDR
RS vector obtained from SEDR
Target radii obtained from SEDR
 JULIAN DATE OF EVENT FRAME=  2443937.3
/CAM FOC L/    RA   /    RB   /    RC   /   RMAG  /LA SSC PT/LO SSC PT/
/ 127248.2/ 1829.400/ 1819.300/ 1815.700/ 806029.9/   -0.032/  156.474/
    OM MATRIX
/ 0.225295/-0.516547/ 0.826088
/ 0.330342/-0.757163/-0.563541
/ 0.916579/ 0.399854/ 0.000052
    RS VECTOR (TARGET COORDINATES)
       -739030.7       -321741.5          -456.1
COORDINATES OF CENTER OF INPUT PICTURE--LATITUDE = -13.09, LONGITUDE = 176.23
PROJECTION IS SIMPLE CYLINDRICAL
 DATA=
            1.000E+00  3.550E+02  0.000E+00  0.000E+00  0.000E+00  2.713E+02  1.000E+01  1.000E+00  5.570E+01  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  1.816E+03  1.829E+03  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
MAP3 geo cylmap8 NL=360 NS=300 'RECT 'VGR1 'HALF SCALE=10.  +
    LINE=100. SAMP=-149. LATI=80. LONG=240. 'remote target=io
Beginning VICAR task MAP3
Map3 version 6-Dec-1999
 LABEL SAYS INPUT PICTURE IS OBJECT SPACE
    CAMERA=          7
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
GETLABCON: warning ind=          1
flight label
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Actual source of SEDR is: DAVI
OM matrix obtained from SEDR
RS vector obtained from SEDR
Target radii obtained from SEDR
 JULIAN DATE OF EVENT FRAME=  2443937.3
/CAM FOC L/    RA   /    RB   /    RC   /   RMAG  /LA SSC PT/LO SSC PT/
/ 127248.2/ 1829.400/ 1819.300/ 1815.700/ 806029.9/   -0.032/  156.474/
    OM MATRIX
/ 0.225295/-0.516547/ 0.826088
/ 0.330342/-0.757163/-0.563541
/ 0.916579/ 0.399854/ 0.000052
    RS VECTOR (TARGET COORDINATES)
       -739030.7       -321741.5          -456.1
COORDINATES OF CENTER OF INPUT PICTURE--LATITUDE = -13.09, LONGITUDE = 176.23
PROJECTION IS SIMPLE CYLINDRICAL
 DATA=
            1.000E+00  3.550E+02  0.000E+00  0.000E+00  0.000E+00  1.928E+02  1.000E+01  1.000E+00  5.570E+01  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  1.816E+03  1.829E+03  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
f2 cylmap8 cylmap2x FUNC="0.5*IN1"
Beginning VICAR task f2
F2 version 2-04-94
F2 using hash table lookup
FUNCTION EVALUATED 5863 TIMES
maskmos inp=(cylmap7,cylmap2x) out=(a,b)
Beginning VICAR task maskmos
newmos (cylmap7 cylmap2x a b) mos3 (1 1 800 800)  +
        'MAP2 LCP=353 SCP=861 THRESH=-32767 'smooth
Beginning VICAR task newmos
*** NEWMOS version 28-Jan-99 ***
SYSTEM LABEL SAYS INPUT IS HALFWORD
LONG=      0.2713E+03  0.1928E+03  0.2713E+03  0.1928E+03
SEARCW FOUND SAMP=  867       616       867       616
RPOL,REQ USED=      0.1816E+04 0.1829E+04
THE OUTPUT PICTURE WILL CONTAIN   800 LINES AND    800 SAMPLES

LINE=            355        355        355        355
SAMP=            867        616        867        616
NOLINE=          360        360        360        360
NOSAMP=          300        300        300        300

***NUMBER OF OFFSETS=    1
***NUMBER OF INPUTS=     4
LTHRES,RTHRE=   -32767    -32767
ITHRES=         -32767
DCLE=              0

LSTART=           -1         -1         -1         -1
LSTOP=           358        358        358        358
SSTART=           -5        246         -5        246
SSTOP=           294        545        294        545

list mos3 INC=40 SL=300 NL=200 NS=800
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:CONV12    User:DFS       Date_Time:Thu May  3 11:15:53 1984
 Task:NEWMOS    User:ntt       Date_Time:Mon Aug 18 15:20:07 2003
     Samp       1    41    81   121   161   201   241   281   321   361   401   441   481   521   561
   Line
    300         0     0  2081  3658  3465  4102  3246  2165  1969  2172  1925  2828  2249  2267     0
    340         0     0  2156  2976  3058  2900  3936  3752  2419  2627  2682  2639  2409  2256     0
write "==========================================================="
===========================================================
write "ADDED TEST FOR PERSPERCTIVE PROJECTION."
ADDED TEST FOR PERSPERCTIVE PROJECTION.
write "==========================================================="
===========================================================
map3 /project/test_work/testdat+
a/mipl/vgr/io.map3 a12.map3     nl=500 ns=500 scale=10. 'perspect
Beginning VICAR task map3
Map3 version 6-Dec-1999
Label specifies map3 perspective input mode
    CAMERA=          7
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
GETLABCON: warning ind=          1
flight label
 JULIAN DATE OF EVENT FRAME=  2443937.3
/CAM FOC L/    RA   /    RB   /    RC   /   RMAG  /LA SSC PT/LO SSC PT/
/ 127248.2/ 1824.350/ 1824.350/ 1815.300/ 638045.9/    0.000/  150.000/
    OM MATRIX
/ 0.500000/-0.866025/ 0.000000
/ 0.000000/ 0.000000/-1.000000
/ 0.866025/ 0.500000/ 0.000000
    RS VECTOR (TARGET COORDINATES)
       -552563.9       -319022.9             0.0
COORDINATES OF CENTER OF INPUT PICTURE--LATITUDE =   0.00, LONGITUDE = 150.00
Projection is Perspective.
 DATA(25-38)=
            1.815E+03  1.824E+03  1.500E+03  2.500E+02  2.500E+02  8.482E+01  0.000E+00  1.500E+02  2.500E+02  2.500E+02
            0.000E+00  0.000E+00  0.000E+00  1.274E+06
map3 /project/test_work/testdat+
a/mipl/vgr/io.map3 a20.map3     nl=50+
0 ns=500 scale=10. 'perspect    north=45. latitude=80. longitud=150. line=200 samp=200
Beginning VICAR task map3
Map3 version 6-Dec-1999
Label specifies map3 perspective input mode
    CAMERA=          7
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
GETLABCON: warning ind=          1
flight label
 JULIAN DATE OF EVENT FRAME=  2443937.3
/CAM FOC L/    RA   /    RB   /    RC   /   RMAG  /LA SSC PT/LO SSC PT/
/ 127248.2/ 1824.350/ 1824.350/ 1815.300/ 638045.9/    0.000/  150.000/
    OM MATRIX
/ 0.500000/-0.866025/ 0.000000
/ 0.000000/ 0.000000/-1.000000
/ 0.866025/ 0.500000/ 0.000000
    RS VECTOR (TARGET COORDINATES)
       -552563.9       -319022.9             0.0
COORDINATES OF CENTER OF INPUT PICTURE--LATITUDE =   0.00, LONGITUDE = 150.00
Projection is Perspective.
 DATA(25-38)=
            1.815E+03  1.824E+03  1.500E+03  2.000E+02  2.000E+02  8.482E+01  8.000E+01  1.500E+02  2.000E+02  2.000E+02
            4.500E+01  0.000E+00  0.000E+00  1.274E+06
newmos inp=(a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3,  +
            a12.map3,a20.map3)  +
            out=newmos.out 'adapt 'smooth
Beginning VICAR task newmos
*** NEWMOS version 28-Jan-99 ***
SYSTEM LABEL SAYS INPUT IS BYTE
LONG=      0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00
SEARCW FOUND SAMP=  250       200       250       200       250       200
LONG=      0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00
SEARCW FOUND SAMP=  250       200       250       200       250       200
LONG=      0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00
SEARCW FOUND SAMP=  250       200       250       200       250       200
LONG=      0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00
SEARCW FOUND SAMP=  250       200       250       200       250       200
LONG=      0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00
SEARCW FOUND SAMP=  250       200       250       200       250       200
LONG=      0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00
SEARCW FOUND SAMP=  250       200       250       200       250       200
LONG=      0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00
SEARCW FOUND SAMP=  250       200       250       200       250       200
LONG=      0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00
SEARCW FOUND SAMP=  250       200       250       200       250       200
LONG=      0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00
SEARCW FOUND SAMP=  250       200       250       200       250       200
LONG=      0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00
SEARCW FOUND SAMP=  250       200       250       200       250       200
LONG=      0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00
SEARCW FOUND SAMP=  250       200       250       200       250       200
LONG=      0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00
SEARCW FOUND SAMP=  250       200       250       200       250       200
LONG=      0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00
SEARCW FOUND SAMP=  250       200       250       200       250       200
LONG=      0.0000E+00  0.0000E+00
SEARCW FOUND SAMP=  250       200
***LINES TRUNCATED
***SAMPLES TRUNCATED
THE PROGRAM CHOSE LCP=   250 AND SCP=   250
RPOL,REQ USED=      0.1815E+04 0.1824E+04
THE OUTPUT PICTURE WILL CONTAIN   500 LINES AND    500 SAMPLES

LINE=            250        200        250        200        250        200
SAMP=            250        200        250        200        250        200
NOLINE=          500        500        500        500        500        500
NOSAMP=          500        500        500        500        500        500

LINE=            250        200        250        200        250        200
SAMP=            250        200        250        200        250        200
NOLINE=          500        500        500        500        500        500
NOSAMP=          500        500        500        500        500        500

LINE=            250        200        250        200        250        200
SAMP=            250        200        250        200        250        200
NOLINE=          500        500        500        500        500        500
NOSAMP=          500        500        500        500        500        500

LINE=            250        200        250        200        250        200
SAMP=            250        200        250        200        250        200
NOLINE=          500        500        500        500        500        500
NOSAMP=          500        500        500        500        500        500

LINE=            250        200        250        200        250        200
SAMP=            250        200        250        200        250        200
NOLINE=          500        500        500        500        500        500
NOSAMP=          500        500        500        500        500        500

LINE=            250        200        250        200        250        200
SAMP=            250        200        250        200        250        200
NOLINE=          500        500        500        500        500        500
NOSAMP=          500        500        500        500        500        500

LINE=            250        200        250        200        250        200
SAMP=            250        200        250        200        250        200
NOLINE=          500        500        500        500        500        500
NOSAMP=          500        500        500        500        500        500

LINE=            250        200        250        200        250        200
SAMP=            250        200        250        200        250        200
NOLINE=          500        500        500        500        500        500
NOSAMP=          500        500        500        500        500        500

LINE=            250        200        250        200        250        200
SAMP=            250        200        250        200        250        200
NOLINE=          500        500        500        500        500        500
NOSAMP=          500        500        500        500        500        500

LINE=            250        200        250        200        250        200
SAMP=            250        200        250        200        250        200
NOLINE=          500        500        500        500        500        500
NOSAMP=          500        500        500        500        500        500

LINE=            250        200        250        200        250        200
SAMP=            250        200        250        200        250        200
NOLINE=          500        500        500        500        500        500
NOSAMP=          500        500        500        500        500        500

LINE=            250        200        250        200        250        200
SAMP=            250        200        250        200        250        200
NOLINE=          500        500        500        500        500        500
NOSAMP=          500        500        500        500        500        500

LINE=            250        200        250        200        250        200
SAMP=            250        200        250        200        250        200
NOLINE=          500        500        500        500        500        500
NOSAMP=          500        500        500        500        500        500

LINE=            250        200
SAMP=            250        200
NOLINE=          500        500
NOSAMP=          500        500

LTHRES,RTHRE=        1         1
ITHRES=              1
DCLE=              0

LSTART=            1         51          1         51          1         51
LSTOP=           500        550        500        550        500        550
SSTART=            1         51          1         51          1         51
SSTOP=           500        550        500        550        500        550

LSTART=            1         51          1         51          1         51
LSTOP=           500        550        500        550        500        550
SSTART=            1         51          1         51          1         51
SSTOP=           500        550        500        550        500        550

LSTART=            1         51          1         51          1         51
LSTOP=           500        550        500        550        500        550
SSTART=            1         51          1         51          1         51
SSTOP=           500        550        500        550        500        550

LSTART=            1         51          1         51          1         51
LSTOP=           500        550        500        550        500        550
SSTART=            1         51          1         51          1         51
SSTOP=           500        550        500        550        500        550

LSTART=            1         51          1         51          1         51
LSTOP=           500        550        500        550        500        550
SSTART=            1         51          1         51          1         51
SSTOP=           500        550        500        550        500        550

LSTART=            1         51          1         51          1         51
LSTOP=           500        550        500        550        500        550
SSTART=            1         51          1         51          1         51
SSTOP=           500        550        500        550        500        550

LSTART=            1         51          1         51          1         51
LSTOP=           500        550        500        550        500        550
SSTART=            1         51          1         51          1         51
SSTOP=           500        550        500        550        500        550

LSTART=            1         51          1         51          1         51
LSTOP=           500        550        500        550        500        550
SSTART=            1         51          1         51          1         51
SSTOP=           500        550        500        550        500        550

LSTART=            1         51          1         51          1         51
LSTOP=           500        550        500        550        500        550
SSTART=            1         51          1         51          1         51
SSTOP=           500        550        500        550        500        550

LSTART=            1         51          1         51          1         51
LSTOP=           500        550        500        550        500        550
SSTART=            1         51          1         51          1         51
SSTOP=           500        550        500        550        500        550

LSTART=            1         51          1         51          1         51
LSTOP=           500        550        500        550        500        550
SSTART=            1         51          1         51          1         51
SSTOP=           500        550        500        550        500        550

LSTART=            1         51          1         51          1         51
LSTOP=           500        550        500        550        500        550
SSTART=            1         51          1         51          1         51
SSTOP=           500        550        500        550        500        550

LSTART=            1         51          1         51          1         51
LSTOP=           500        550        500        550        500        550
SSTART=            1         51          1         51          1         51
SSTOP=           500        550        500        550        500        550

LSTART=            1         51
LSTOP=           500        550
SSTART=            1         51
SSTOP=           500        550

gen A3D NL=10 NS=10 IVAL=50 LINC=0 SINC=0 NB=10
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen B3D NL=10 NS=10 IVAL=100 LINC=0 SINC=0 NB=10
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen C3D NL=10 NS=10 IVAL=200 LINC=0 SINC=0 NB=10
Beginning VICAR task gen
GEN Version 6
GEN task completed
newmos (A3D,B3D,C3D) D3D NL=10 NS=30 LCP=1 SCP=30 PIXL=(1,30,1,20,1,10) INC=1 'AVER
Beginning VICAR task newmos
*** NEWMOS version 28-Jan-99 ***
SYSTEM LABEL SAYS INPUT IS BYTE

LINE=              1          1          1
SAMP=             30         20         10
NOLINE=           10         10         10
NOSAMP=           10         10         10

LTHRES,RTHRE=        1         1
ITHRES=              1
DCLE=              0

LSTART=            1          1          1
LSTOP=            10         10         10
SSTART=            1         11         21
SSTOP=            10         20         30

[VIC2-GENERR] Exception in XVREAD, processing file: A3D
[VIC2-STRTREC] Bad starting record for read or write operation; program error.
 Current line in image = 0
 ** ABEND called **
continue
list D3D
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Mon Aug 18 15:20:51 2003
 Task:NEWMOS    User:ntt       Date_Time:Mon Aug 18 15:20:51 2003
 ***********
 Band =     3
 ***********
     Samp     1       3       5       7       9      11      13      15      17      19      21      23      25      27      29
   Line

      9       0   0   0   0   0   0   0   0   0   8 162 192   0   0   0   0   0   0   0   3   0   0   0   0   0   0   0   0   0   0
end-proc
disable-log
$ Return
$!#############################################################################
