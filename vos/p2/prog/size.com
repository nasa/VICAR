$!****************************************************************************
$!
$! Build proc for MIPL module size
$! VPACK Version 1.9, Sunday, July 29, 2012, 13:36:52
$!
$! Execute by entering:		$ @size
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
$ write sys$output "*** module size ***"
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
$ write sys$output "Invalid argument given to size.com file -- ", primary
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
$   if F$SEARCH("size.imake") .nes. ""
$   then
$      vimake size
$      purge size.bld
$   else
$      if F$SEARCH("size.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake size
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @size.bld "STD"
$   else
$      @size.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create size.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack size.com -mixed -
	-s size.f getsize.f update_label.f snoin.f sintrp.f sread.f magnify.f -
	   compress.f -
	-i size.imake -
	-p size.pdf -
	-t tstsize.pdf tstsize.log_solos tstsize.log_linux
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create size.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
C Definition of global constants:
C ICODE,OCODE are the input and output data format codes
C   =1 for byte, =2 for 16-bit integer, =3 for 32-bit integer,
C   =4 for REAL*4
C
C GSCALE optionally used to scale the output (see SCALE parameter).
C		OUTDN=OUTDN*GSCALEC
C
C ILO,IHI = low and high limits of the output DN (see LIMITS parameter).
C              ILO .LE. out-DN .LE. IHI
C LFLAG=1 if a check for saturation is necessary (ILO,IHI)
C
C           SIZE  IN  OUT  (1,1,NLO,NSO)  user-parameters...
C       or  SIZE  IN  OUT  ZOOM=2.5  user-parameters...

c  29Jul2012 -lwk- the arrays of size 100000 were put into a common block,
c		as otherwize the program crashes when the the output line
c		length exceeds 50000 words;  if compiler changes or Solaris
c		is no longer supported, this can probably be removed.
C
      SUBROUTINE MAIN44
      IMPLICIT NONE
      COMMON/CP/IUNIT,ICODE,SLI,SSI,NLI,NSI
      COMMON/CP/OUNIT,OCODE,SLO,SSO,NLO,NSO,NLOUT,NSOUT
      COMMON/CP/ZOOML,ZOOMS,LZOOM,IZOOM,GSCALE,ILO,IHI,LFLAG
      INTEGER*4 IUNIT,OUNIT	!Input and output logical unit numbers
      INTEGER*4 ICODE,OCODE	!Input and ouput image format
      INTEGER*4 SLI,SSI,NLI,NSI	!Input image area (see AREA parameter)
      INTEGER*4 SLO,SSO,NLO,NSO !Output area mapped from input area
      INTEGER*4 NLOUT,NSOUT     !Size of ouput image
      INTEGER*4 IB,SB,EB	!variables for 3rd dimension
      REAL*4 ZOOML,ZOOMS	!Floating point zoom factors
      INTEGER*4 LZOOM,IZOOM	!Integer zoom factors (=0 if not integer)
      INTEGER*4 ILO,IHI		!Min and max DN limits for output image
      INTEGER*4 LFLAG		!=1 if need to check for ILO,IHI saturation
      REAL*4 GSCALE		!Output DN scale factor

      COMMON/C2/RBUF(100000),BUF(100000,2),OBUF(100000)
      COMMON/C2/SAMP(100000),WGHT(100000)
      INTEGER*4 BUF,SAMP
      REAL*4 RBUF,OBUF,WGHT
c     INTEGER*4 SAMP(100000)
c     REAL*4 BUF(100000),RBUF(100000),OBUF(100000),WGHT(100000)

      INTEGER*4 I,L,N,IND,INCODE
      LOGICAL*4 INTERP
      CHARACTER*4 FMT(4)/'BYTE','HALF','FULL','REAL'/
  
      CALL XVMESSAGE(' SIZE version 18-Jul-2012',' ')

C     ....Open input picture
      CALL IPOPEN(iunit,icode,sb,eb)
C     ....Determine zoom factor and size of input and output images
      CALL GETSIZE(IUNIT,interp,sli,ssi,nli,nsi,slo,sso,
     & nlo,nso,nlout,nsout,zooml,zooms,lzoom,izoom)
C     ....Open output picture

      CALL OPOPEN(INTERP,ZOOML,ZOOMS,LZOOM,IZOOM,NLOUT,NSOUT,
     & sb,eb,ICODE,ocode,ounit)
C     ....Get scale and limits parameters
      CALL GETSCALE(ICODE,OCODE,gscale,ilo,ihi,lflag)

C     ....If format conversion is required, reopen input
      INCODE = MAX0(ICODE,OCODE)
      IF (INTERP) INCODE=4
c	force all input to be converted to real on the fly
         CALL XVCLOSE(IUNIT,IND,' ')
         CALL XVOPEN(IUNIT,IND,'OPEN_ACT','SA','IO_ACT','SA',
     &		'I_FORMAT',FMT(ICODE),'U_FORMAT',FMT(4),' ')		!FMT(INCODE),' ')
c	Label upate errors do not abort size program
      CALL UPDATE_LABEL(IUNIT,OUNIT,SLI,SSI,SLO,SSO,ZOOML,ZOOMS)

C     ... start loop over bands
      DO 100 IB=SB,EB

C     ....If there is an offset, write top lines before offset
      DO I=1,NSOUT
         OBUF(I) = 0.
      ENDDO
      IF (ICODE.LT.4) CALL ZIA(OBUF,NSOUT)
      DO L=1,SLO-1
         CALL XVWRIT(OUNIT,OBUF,IND,'LINE',L,'BAND',IB,' ')
      ENDDO 

C     ....Perform the magnification or reduction
      IF (INTERP) THEN
         CALL SINTRP(ib,NSO,buf,rbuf,obuf,samp,wght)	!Interpolation
      ELSE
         CALL SNOIN(ib,nso,buf,rbuf,obuf,samp)		!No interpolation
      ENDIF

C     ....If output size is larger than sized input, write remaining lines
      N = NLOUT - (SLO+NLO-1)
      IF (N.LE.0) GOTO 100
      DO I=1,NSOUT
         OBUF(I) = 0.
      ENDDO
      IF (OCODE.LT.4) CALL ZIA(OBUF,NSOUT)
      DO L=1,N
         CALL XVWRIT(OUNIT,OBUF,IND,'LINE',SLO+NLO+L-1,'BAND',IB,' ')
      ENDDO 

  100 CONTINUE		! end loop over bands

      CALL XVMESSAGE(' SIZE task completed',' ')
      RETURN

  999 CALL XVMESSAGE('??E - SIZE task cancelled',' ')
      CALL ABEND
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Open input image and determine data format
C Also, process 3-D parameters
C removed return 1 and * param 2-9-2010 - RJB (call abends substituted)
      SUBROUTINE IPOPEN(iunit,icode,sb,eb)
      IMPLICIT NONE
      INTEGER*4 IUNIT
      INTEGER*4 ICODE		!1=byte, 2=half, 3=full, 4=real

      INTEGER*4 IND,SB,EB,NB,NBI
      CHARACTER*5 FORMAT
      CHARACTER*8 ORG

      CALL XVUNIT(iunit,'INP',1,IND,' ')
      CALL XVSIGNAL(IUNIT,IND,.TRUE.)
      CALL XVOPEN(IUNIT,IND,'OPEN_ACT','SA','IO_ACT','SA',' ')

      CALL XVGET(IUNIT,ind,'FORMAT',format,'ORG',org,' ')	
      call xvbands( sb, nb, nbi)
      ! nbi is from input label
      ! nb is from param NB or BANDS, whichever is non-zero;  else zero

      if (nbi.gt.1) then
	if (org.eq.'BIP') then
          CALL XVMESSAGE(
     &  ' BIP files not supported, use program TRAN to convert to BSQ',
     &    ' ')
	  call abend
        endif
	if (org.eq.'BIL') call xvmessage(
     &   '??W - BIL format may cause performance degradation',' ')
      endif
      if ((sb+nb-1).gt.nbi) then
	call xvmessage(' ??W - NB too large, reduced to fit input',' ')
	nb = nbi-sb+1
      endif
      if (nb.le.0) nb = nbi-sb+1
      eb = sb+nb-1

      ICODE = 0
      IF (FORMAT.EQ.'BYTE') ICODE=1
      IF (FORMAT.EQ.'HALF'.OR.FORMAT.EQ.'WORD') ICODE=2
      IF (FORMAT.EQ.'FULL') ICODE=3
      IF (FORMAT.EQ.'REAL') ICODE=4
      IF (ICODE.NE.0) RETURN
      CALL XVMESSAGE('??E - Unknown data format for input image',' ')
	call abend  
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Determine output data format, open output file, and put zoom
C factors in label. 
C
C For the no-interpolation option, mixed data formats (e.g. byte input
C and halfword output) are handled by using VICAR I/O to convert them
C to the same format as follows:
C    If ICODE.LT.OCODE, then the input is converted to the output format
C       by XVREAD.  The input file is closed and re-opened to set this
C       option, and ICODE is set equal to OCODE.
C    If ICODE.GT.OCODE, then the input is converted to the output format
C       by XVWRITE.
C
C Outputs:  OCODE=output image data format code
C           OUNIT=output image logical unit number
C           MGN=1 if magellan DN scaling is requested
C           ICODE=input data format (may be modified due to conversion)
C
      SUBROUTINE OPOPEN(INTERP,ZOOML,ZOOMS,LZOOM,IZOOM,NLOUT,NSOUT,
     & sb,eb,ICODE,ocode,ounit)
      IMPLICIT NONE
	BYTE byt(80)
	INTEGER*4 LZOOM,IZOOM,NLOUT,NSOUT,ICODE,OCODE,OUNIT
	INTEGER*4 I,IND,NCHAR,OUTCODE,sb,eb,nb
	LOGICAL*4 INTERP
	LOGICAL*4 XVPTST
	REAL*4 ZOOML,ZOOMS
	REAL*4 F1,F2
	CHARACTER*4 FMT(4)/'BYTE','HALF','FULL','REAL'/
	CHARACTER*72 LAB
	CHARACTER*80 MSG

      DATA LAB /' '/                    !initialize character buffer

  100 FORMAT(' Input data format=',A4,'  Output data format=',A4)
C	these go in label as a comment
  102 FORMAT(' PICTURE SIZE SCALED BY ',F12.5,'*NL, ',F12.5,'*NS')
  103 FORMAT(' PICTURE SIZE SCALED BY ',I6,'*NL, ',I6,'*NS')

C     ....Determine output picture format
      OCODE = ICODE			!Default is same format as input
      IF (XVPTST('BYTE')) OCODE=1
      IF (XVPTST('HALF')) OCODE=2
      IF (XVPTST('FULL')) OCODE=3
      IF (XVPTST('REAL')) OCODE=4
c      WRITE (MSG,100) FMT(ICODE),FMT(OCODE)
c      CALL XVMESSAGE(MSG,' ')

      OUTCODE = MAX0(ICODE,OCODE)
      IF (INTERP) THEN
         OUTCODE = 3
         IF (OCODE.EQ.4) OUTCODE=4
      ENDIF
      CALL XVUNIT(OUNIT,'OUT',1,IND,' ')
      CALL XVSIGNAL(OUNIT,IND,.TRUE.)
      nb = eb-sb+1
c Feb 5, 2010 - internal format is real - convert to ouput of ocoide
      CALL XVOPEN(OUNIT,IND,'OP','WRITE','U_NL',NLOUT,'U_NS',NSOUT,
     & 'U_NB',NB,'OPEN_ACT','SA','IO_ACT','SA','O_FORMAT',FMT(OCODE),
     & 'U_FORMAT',FMT(4),' ')				!,FMT(OUTCODE),' ')

      IF (IZOOM.EQ.0.OR.LZOOM.EQ.0) THEN	!Print real zoom factor
         WRITE(MSG,102) ZOOML,ZOOMS
         NCHAR = 36
         IF (ZOOML.NE.ZOOMS) NCHAR=56
         CALL XVMESSAGE(MSG,' ')
      elseif (izoom.lt.0 .and. lzoom.lt.0) then
	  ! (don't worry about case where one is <0 and other >0)
	 f1 = -1.0/float(lzoom)
	 f2 = -1.0/float(izoom)
         write(msg,102) f1, f2
         nchar = 36
         if (zooml.ne.zooms) nchar=56
         call xvmessage(msg,' ')
      ELSE
         WRITE(MSG,103) LZOOM,IZOOM
         NCHAR = 30
         IF (LZOOM.NE.IZOOM) NCHAR=44
         CALL XVMESSAGE(MSG,' ')
      ENDIF

      do i=2,nchar
           byt(i-1) = ichar(msg(i:i))
      end do

      CALL MVLC(byt,LAB,NCHAR-1)
c update label
      CALL XLADD(OUNIT,'HISTORY','COMMENT',LAB,IND,
     &            'FORMAT','STRING','ULEN',NCHAR-1,' ')
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Get optional scaling and DN limits of output image.
C Outputs: GSCALE=optional scaling of output DN (ODN = GSCALE*ODN).
C          ILO,IHI=low and high limits of output DN values
C          LFLAG=1 if a check for saturation (ILO,IHI) is required.
C
      SUBROUTINE GETSCALE(ICODE,OCODE,gscale,ilo,ihi,lflag)			!removed ,*)
	IMPLICIT NONE
	INTEGER*4 ICODE,OCODE     !Input and ouput image format
	REAL*4 GSCALE             !Output DN scale factor
      INTEGER*4 ILO,IHI         !Min and max DN limits for output image
      INTEGER*4 LFLAG           !=1 if need to check for ILO,IHI saturation

      INTEGER*4 INUM,IDEF,PAR(2)
C      REAL*4 EPS
      INTEGER*4 LOLIM(3)
      INTEGER*4 HILIM(3)

C      DATA EPS/1.E-6/
      DATA LOLIM(1)/0/,LOLIM(2)/-32768/,LOLIM(3)/-2147483648/
      DATA HILIM(1)/255/,HILIM(2)/32767/,HILIM(3)/2147483647/

C     ....Determine max and min limits of output DN
      IF (OCODE.LT.4) THEN
         ILO = LOLIM(OCODE)
         IHI = HILIM(OCODE)
      ENDIF

      IF (ICODE.GT.OCODE) THEN	!If the output DN range is smaller than
         LFLAG = 1		!the input range, check for saturation.
      ELSE				
         LFLAG = 0		!Else, no check is necessary.
      ENDIF

      CALL XVP('LIMITS',PAR,INUM)
      IF (INUM.EQ.2) THEN 
          ILO = MAX0(PAR(1),ILO)
          IHI = MIN0(PAR(2),IHI)
          LFLAG = 1
      ENDIF

      CALL XVPARM('SCALE',GSCALE,INUM,IDEF,1)  !Optional scaling of output DNs
      IF (IDEF.NE.1) THEN 
          IF (GSCALE.EQ.0.) GOTO 990
          LFLAG = 1
      ENDIF
      RETURN

  990 CALL XVMESSAGE('??E - Zero is an invalid SCALE value',' ')
      call abend
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create getsize.f
$ DECK/DOLLARS="$ VOKAGLEVE"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Determine size of output picture.
C Inputs:  NLI,NSI
C          LZFLG,IZFLG=1 if zoom factor is specified, =0 otherwise
C          INTERP=.TRUE. if interpolation is specified
C Outputs: NLOUT,NSOUT
C	   ZOOML,ZOOMS
C          LZOOM,IZOOM
C
c removed return 1 and * param 2-9-2010 - RJB (call abends substituted)
	SUBROUTINE GETSIZE(iunit,interp,sli,ssi,nli,nsi,slo,sso,nlo,nso,
     &		nlout,nsout,zooml,zooms,lzoom,izoom)
      IMPLICIT NONE
      INTEGER*4 IUNIT
      INTEGER*4 SLI,SSI,NLI,NSI !Input image size field (see AREA parameter)
      INTEGER*4 SLO,SSO,NLO,NSO !Output area mapped from input area
      INTEGER*4 NLOUT,NSOUT     !Size of ouput image
      REAL*4 ZOOML,ZOOMS	!Floating point zoom factors
      INTEGER*4 LZOOM,IZOOM	!Integer zoom factors

      INTEGER*4 PAR(4),N,IND,IDEF0,IDEF,LZFLG,IZFLG
      INTEGER*4 NLIN,NSIN
      LOGICAL*4 INTERP
      LOGICAL*4 XVPTST

      REAL*4 S,EPS/1.E-6/
      CHARACTER*42 MSG

  160 FORMAT ('      INPUT AREA=(',I5,',',I5,',',I5,',',I5,')')
  170 FORMAT ('     OUTPUT SIZE= ',I6,' X ',I6)

C     ....Get input and output size
      CALL XVPARM('SIZE',PAR,N,IDEF0,4)
      IF (IDEF0.NE.1 .AND. N.EQ.4) THEN
         NLOUT = PAR(3)
         NSOUT = PAR(4)
         CALL XVGET(iunit,ind,'NL',nlin,'NS',nsin,' ')
      ELSE
         CALL XVSIZE(sli,ssi,nlout,nsout,nlin,nsin)
      ENDIF

C     ....Get output offset
      CALL XVPARM('IOFFSET',PAR,N,IDEF,2)
      IF (N.EQ.2) THEN
         SLO = PAR(1)
         SSO = PAR(2)
      ELSE
         SLO = 1
         SSO = 1
      ENDIF

C     ....Get input image size field
      CALL XVPARM('AREA',PAR,N,IDEF,4)
      IF (N.EQ.4) THEN 
         SLI = PAR(1)	!(SLI,SSI,NLI,NSI) maps to (SLO,SSO,NLO,NSO)
         SSI = PAR(2)
         NLI = PAR(3)
         NSI = PAR(4)
         IF (SLI.LT.1.OR.SSI.LT.1) GOTO 980
         IF (NLI.LT.1.OR.NSI.LT.1) GOTO 980
         IF (NLI.GT.NLIN-SLI+1) GOTO 980
         IF (NSI.GT.NSIN-SSI+1) GOTO 980
      ELSE
         SLI = 1
         SSI = 1
         NLI = NLIN
         NSI = NSIN
      ENDIF
      WRITE (MSG,160) SLI,SSI,NLI,NSI
      CALL XVMESSAGE(MSG,' ')

      CALL GETZOOM(lzflg,izflg,zooml,zooms,lzoom,izoom)
      INTERP = .NOT.XVPTST('NOIN')
C
C    ....If horizontal zoom is not specified, compute it from size field
      IF (IZFLG.EQ.0) THEN	
         CALL XVPARM('NS',par,n,idef,1)
	 IF (NSOUT.EQ.NSIN.AND.IDEF0.EQ.1.AND.IDEF.EQ.1) GOTO 950
         NSO = NSOUT - SSO + 1
	 ZOOMS = (1.0*NSO)/NSI
         IF (NSO.LT.NSI) THEN
	    N = NSI/NSO
            S = 1./ZOOMS - N
            IF (ABS(S).LT.1.E-6) IZOOM=-N
	 ELSE
            N = NSO/NSI
            S = ZOOMS - N
            IF (ABS(S).LT.1.E-6) IZOOM=N
	 ENDIF
         GOTO 40
      ENDIF

C     ....Here if horizontal zoom is specified.  Compute NSO from it.
      IF (IZOOM.NE.0) THEN
         IF (IZOOM.LT.0) THEN
            IF (INTERP) THEN
	       NSO = -NSI/IZOOM
            ELSE
               NSO = -(NSI-1)/IZOOM + 1
            ENDIF
	 ELSE
   	    NSO = NSI*IZOOM
	 ENDIF
      ELSE			!Floating point zoom is specified.
         IF (.NOT.INTERP.AND.ZOOMS.LT.1.0) THEN
            NSO = (NSI-1)*ZOOMS + EPS + 1
         ELSE
	    NSO = ZOOMS*NSI + EPS
         ENDIF
      ENDIF

      CALL XVPARM('NS',par,n,idef,1)
      IF (IDEF.NE.1) NSOUT=PAR(1)
      IF (IDEF.EQ.1. AND. IDEF0.EQ.1) NSOUT=SSO+NSO-1

C    ....Vertical zoom
C     ....If vertical zoom is not specified, compute it from size field
   40 IF (LZFLG.EQ.0) THEN
         CALL XVPARM('NL',par,n,idef,1)
	 IF (NLOUT.EQ.NLIN.AND.IDEF0.EQ.1.AND.IDEF.EQ.1) GOTO 960
         NLO = NLOUT - SLO + 1
         ZOOML = (1.*NLO)/NLI
         IF (NLO.LT.NLI) THEN
            N = NLI/NLO
            S = 1.0/ZOOML - N
            IF (ABS(S).LT.1.E-6) LZOOM=-N
	 ELSE
            N = NLO/NLI
            S = ZOOML - N
            IF (ABS(S).LT.1.E-6) LZOOM=N
	 ENDIF
         GOTO 80
      ENDIF

C     ....Here if vertical zoom is specified.  Compute NLO from it.
      IF (LZOOM.NE.0) THEN	!Integral zoom is specified,
         IF (LZOOM.LT.0) THEN
            IF (INTERP) THEN
	       NLO = -NLI/LZOOM
            ELSE
               NLO = -(NLI-1)/LZOOM + 1
            ENDIF
	 ELSE
            NLO = NLI*LZOOM
  	 ENDIF
      ELSE			!Floating point zoom is specified.
         IF (.NOT.INTERP.AND.ZOOML.LT.1.0) THEN
            NLO = (NLI-1)*ZOOMS + 1.0 + EPS
         ELSE
	    NLO = ZOOML*NLI + EPS
         ENDIF
      ENDIF

      CALL XVPARM('NL',par,n,idef,1)
      IF (IDEF.NE.1) NLOUT=PAR(1)
      IF (IDEF.EQ.1 .AND. IDEF0.EQ.1) NLOUT=SLO+NLO-1

   80 WRITE(MSG,170) NLOUT,NSOUT
      CALL XVMESSAGE(MSG,' ')
      IF (NSOUT.GT.100000) GOTO 970
      IF (NLO.EQ.1) THEN
         LZOOM = 1
         ZOOML = 1.
      ENDIF
      IF (NSO.EQ.1) THEN
         IZOOM = 1
         ZOOMS = 1.
      ENDIF
      IF (LZOOM.EQ.1.AND.IZOOM.EQ.1) INTERP=.FALSE.
      RETURN

  950 CALL XVMESSAGE('??E - Must specify either SZOOM or NS',' ')
      call abend
  960 CALL XVMESSAGE('??E - Must specify either LZOOM or NL',' ')
      call abend
  970 CALL XVMESSAGE('??E - Output sample size exceeds 100,000',' ')
      call abend
  980 CALL XVMESSAGE('??E - Invalid input picture area',' ')
	call abend
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Check for ZOOM, LZOOM, or SZOOM parameters
C Outputs: ZOOML,ZOOMS = floating pt zoom factors
C          LZOOM,IZOOM = integral zoom factors (=0 if not an integer)
C          LZFLG,IZFLG = 1 if zoom factors are specified, =0 otherwise
C
C removed return 1 and * param 2-9-2010 - RJB (call abends substituted)
      SUBROUTINE GETZOOM(lzflg,izflg,zooml,zooms,lzoom,izoom)
	IMPLICIT NONE
	INTEGER*4 N,INUM,IDEF,LZFLG,IZFLG
	INTEGER*4 LZOOM,IZOOM     !Integer zoom factors

	REAL*4 ZOOML,ZOOMS        !Floating point zoom factors
	REAL*4 R

      IZFLG = 0
      LZFLG = 0

      CALL XVPARM('ZOOM',R,INUM,IDEF,1)
      IF (INUM.GT.0) THEN 
          IF (R.EQ.0.0) GOTO 998
          CALL QPAR(n,r)		!Check for integral zoom
          ZOOML = R
          ZOOMS = R
          LZOOM = N
          IZOOM = N
          IF (IZOOM.EQ.-1) IZOOM=1
          IF (LZOOM.EQ.-1) LZOOM=1
          IZFLG = 1
          LZFLG = 1
      ENDIF

      CALL XVPARM('LZOOM',R,INUM,IDEF,1)
      IF (INUM.GT.0) THEN 
          IF (R.EQ.0.0) GOTO 999
          CALL QPAR(n,r)
          ZOOML = R
          LZOOM = N
          IF (LZOOM.EQ.-1) LZOOM=1
          LZFLG = 1
      ENDIF

      CALL XVPARM('SZOOM',R,INUM,IDEF,1)
      IF (INUM.GT.0) THEN 
          IF (R.EQ.0.0) GOTO 999
          CALL QPAR(n,r)
          ZOOMS = R
          IZOOM = N
          IF (IZOOM.EQ.-1) IZOOM=1
          IZFLG = 1
      ENDIF

      RETURN

  998 CALL XVMESSAGE('??E - ZOOM cannot be zero',' ')
      call abend
  999 CALL XVMESSAGE('??E - LZOOM and SZOOM cannot be zero',' ')
      call abend
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Check for integer or real zoom factor.   An integer value is returned
C if possible.
C
C Input: R=real zoom factor specified via ZOOM=R parameter.
C Outputs:  N=integer zoom factor.  This has same meaning as in VIDS.
C            =0 if zoom factor is not an integer.
C If R is negative, it is converted to a fraction.
C E.g. if R=-2, then R=0.5 on output.
C
      SUBROUTINE QPAR(n,r)
	IMPLICIT NONE
	INTEGER*4 N
	REAL*4 R, S
	REAL*4 EPS

      DATA EPS/1.E-6/

      IF (R.GE.1.0) GOTO 20
C     ....Here for compression.  Compute shrink factor S.
      IF (R.GT.0.) THEN
         S = 1./R
      ELSE
         S = -R
         R = 1./S		!Convert negative zoom to fraction
      ENDIF

      N = S + EPS		!Check for integer shrink factor
      S = S - N
      IF (ABS(S).LE.EPS) THEN
         N = -N
      ELSE
         N = 0
      ENDIF
      RETURN
C
C     ....Here for magnification (R.GT.1.0)
   20 N = R + EPS		!Check for integer zoom factor
      S = R - N
      IF (ABS(S).LE.EPS) THEN
         R = N
      ELSE
         N = 0
      ENDIF

      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create update_label.f
$ DECK/DOLLARS="$ VOKAGLEVE"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C If present, update map history and property labels to reflect change in
C picture scale.
C
c	Label update errors do not abort size program
      subroutine update_label(iunit,ounit,sli,ssi,slo,sso,zooml,zooms)
      implicit none
      include 'mp_for_defs' 
      integer*4 iunit,ounit,sli,ssi,slo,sso
      real*4 zooml,zooms

      real*8 offs,offl,fscale,cl,cs,pcl,pcs,scale,res,radius,mp
      character*40 maptype
      integer istat
      logical xvptst
C
C     ....get map label data, if any (only if aspect ratio is constant)
      call mp_init(mp,istat)
      if (istat.lt.MP_SUCCESS) goto 900
      call mp_label_read( mp, iunit, istat)
      if (istat.lt. MP_SUCCESS) goto 901
      if (zooml.ne.zooms) goto 902
      call mp_get_value_str(mp,'MAP_PROJECTION_TYPE',maptype,istat)
      if(istat.le.MP_FAILURE) goto 903

! when correcting Line/Sample items, recall that integer L/S
! values refer to pixel centers, whereas in a zoom only the
! top left corner (0.5,0.5) remains constant;  so, for
! X = Line or Sample and Z = Zoom:
!		X' = (X-0.5)*Z + 0.5
! If there is also an sub-area offset SX, then (SX-1) must
! be subtracted from X.

      if (maptype.ne.'POINT_PERSPECTIVE') goto 20
      call mp_get_value(mp,'FOCAL_PLANE_SCALE',fscale,istat)
      if (istat.gt.MP_FAILURE) then
         fscale = fscale*zooml	! camera scale (pix/mm)
         call mp_set_value(mp,'FOCAL_PLANE_SCALE',fscale,istat)
         if (istat.lt.MP_SUCCESS) call xvmessage
     &             ('error setting FOCAL_PLANE_SCALE',' ')
      else
         call xvmessage('error getting FOCAL_PLANE_SCALE',' ')
      endif

      call mp_get_value(mp,'OPT_AXIS_INTERCEPT_LINE',cl,istat)
      if (istat.gt.MP_FAILURE) then
         cl = zooml*(cl-sli+0.5) + slo - 0.5	! OAL
         call mp_set_value(mp,'OPT_AXIS_INTERCEPT_LINE',cl,istat)
         if (istat.lt.MP_SUCCESS) call xvmessage
     &             ('error setting OPT_AXIS_INTERCEPT_LINE',' ')
      else
         call xvmessage('error getting OPT_AXIS_INTERCEPT_LINE',' ')
      endif

      call mp_get_value(mp,'OPT_AXIS_INTERCEPT_SAMPLE',cs,istat)
      if (istat.gt.MP_FAILURE) then
         cs = zooml*(cs-ssi+0.5) + sso - 0.5	! OAS
         call mp_set_value(mp,'OPT_AXIS_INTERCEPT_SAMPLE',cs,istat)
         if (istat .lt. MP_SUCCESS) call xvmessage
     &             ('error setting OPT_AXIS_INTERCEPT_SAMPLE',' ')
      else
         call xvmessage('error getting OPT_AXIS_INTERCEPT_SAMPLE',' ')
      endif

      call mp_get_value(mp,'PLANET_CENTER_LINE',pcl,istat)
      if (istat.gt.MP_FAILURE) then
         pcl = zooml*(pcl-sli+0.5) + slo - 0.5	! special line
         call mp_set_value(mp,'PLANET_CENTER_LINE',pcl,istat)
         if(istat.lt.MP_SUCCESS) call xvmessage
     &             ('error setting PLANET_CENTER_LINE',' ')
      else
         call xvmessage('error getting PLANET_CENTER_LINE',' ')
      endif

      call mp_get_value(mp,'PLANET_CENTER_SAMPLE',pcs,istat)
      if (istat.gt. MP_FAILURE) then
         pcs = zooml*(pcs-ssi+0.5) + sso - 0.5	! special sample
         call mp_set_value(mp,'PLANET_CENTER_SAMPLE',pcs,istat)
         if (istat.lt.MP_SUCCESS) call xvmessage
     &             ('error setting PLANET_CENTER_SAMPLE',' ')
      else
         call xvmessage('error getting PLANET_CENTER_SAMPLE',' ')
      endif
      goto 50

C     ....Here if not point-perspective
   20 call mp_get_value(mp,'SAMPLE_PROJECTION_OFFSET',offs,istat)
      if (istat.gt.MP_FAILURE) then
         offs = zooml*(offs-ssi+1.5) + sso - 1.5	! special sample
         call mp_set_value(mp,'SAMPLE_PROJECTION_OFFSET',offs,istat)
         if (istat .lt. MP_SUCCESS) call xvmessage
     &             ('error setting SAMPLE_PROJECTION_OFFSET',' ')
      else
         call xvmessage('error getting SAMPLE_PROJECTION_OFFSET',' ')
      endif

      call mp_get_value(mp,'LINE_PROJECTION_OFFSET',offl,istat)
      if (istat .gt. MP_FAILURE) then
         offl = zooml*(offl-sli+1.5) + slo - 1.5	! special line
         call mp_set_value(mp,'LINE_PROJECTION_OFFSET',offl,istat)
         if (istat .lt. MP_SUCCESS) call xvmessage
     &             ('error setting LINE_PROJECTION_OFFSET',' ')
      else
         call xvmessage('error getting LINE_PROJECTION_OFFSET',' ')
      endif

      call mp_get_value(mp,'MAP_SCALE',scale,istat)
      if (istat.gt. MP_FAILURE) then
         scale = scale/zooml		! map scale (km/pix)
      else
         call xvmessage ('***MAP_SCALE not found',' ')
         scale = -1. 
      endif

      call mp_get_value(mp,'MAP_RESOLUTION',res,istat)
      if (istat .gt. MP_FAILURE) then
         res = res*zooml		! map resolution (pix/deg)
      else
         call xvmessage('***MAP_RESOLUTION not found',' ')
         res = -1.
      endif

      if (scale.ge.0. .and. res.ge.0.) goto 40		!Go on if both are found
      if (scale.lt.0. .and. res.lt.0.) goto 40		!Go on if both missing

C     ....If scale or res is missing, compute it from the other
      call mp_get_value(mp,'A_AXIS_RADIUS',radius,istat)
      if (istat .le. MP_FAILURE) goto 50
      if (scale.lt.0.) then
         call xvmessage('Computing scale from resolution',' ')
         scale = 0.0174533*radius/res
      else
         call xvmessage('Computing resolution from scale',' ')
         res = 0.0174533*radius/scale
      endif

   40 if (scale.ge.0.) then
         call mp_set_value(mp,'MAP_SCALE',scale,istat)
         if (istat .lt. MP_SUCCESS) call xvmessage
     &             ('***Err setting MAP_SCALE',' ')
      endif
      if (res.ge.0.) then
         call mp_set_value(mp,'MAP_RESOLUTION',res,istat)
         if (istat .lt. MP_SUCCESS) call xvmessage
     &             ('***Err setting MAP_RESOLUTION',' ')
      endif

   50 call mp_label_write( mp,ounit,'PROPERTY',istat)
      if (istat.gt.MP_FAILURE) then
         call xvmessage('MAP property label updated',' ')
      else
         call xvmessage('***Err updating MAP property label',' ')
      endif
      call mp_label_write(mp,ounit,'HISTORY',istat)
      if (istat.gt. MP_FAILURE) then
         call xvmessage('MAP history label updated',' ')
      else
         call xvmessage('***Err updating MAP history label',' ')
      endif
      call mp_free(mp)
      return

  900 call xvmessage('err in mp_init,map label not found',' ')
      return
  901 if (XVPTST('DEBUG')) call xvmessage
     &     ('err reading MP labels or MP labels do not exist',' ')
      return
  902 call xvmessage('ASPECT change: MAP labels NOT updated',' ')
      return
  903 call xvmessage('error getting projection type',' ')
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create snoin.f
$ DECK/DOLLARS="$ VOKAGLEVE"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Expand or reduce an image without interpolation.
C Input and output data formats are either both byte or both halfword.
C Mixed data modes (e.g. byte input, halfword output) are handled by
C enabling VICAR I/O data conversion via the XVOPEN calls.
C
c	buf,rbuf,obuf,samp are returned, n=nso
	SUBROUTINE SNOIN(ib,n,buf,rbuf,obuf,samp)
      IMPLICIT NONE
      INTEGER*4 IB,N
c      INTEGER*2 BUF(1)		!size=NSI
c      LOGICAL*1 OBUF(1)		!size=NSO
	
      INTEGER*4 SAMP(*)		!size=NSO
	REAL*4 BUF(N),OBUF(N),RBUF(N)
      COMMON/CP/IUNIT,ICODE,SLI,SSI,NLI,NSI
      COMMON/CP/OUNIT,OCODE,SLO,SSO,NLO,NSO,NLOUT,NSOUT
      COMMON/CP/ZOOML,ZOOMS,LZOOM,IZOOM,GSCALE,ILO,IHI,LFLAG
      INTEGER*4 IUNIT,ICODE,SLI,SSI,NLI,NSI
      INTEGER*4 OUNIT,OCODE,SLO,SSO,NLO,NSO,NLOUT,NSOUT
      INTEGER*4 LZOOM,IZOOM,ILO,IHI,LFLAG
      REAL*4 ZOOML,ZOOMS,GSCALE

      COMMON/C1/TBL(0:255),TBLH(-32768:32767)
      BYTE TBL
      INTEGER*2 TBLH

c      COMMON/C2/RBUF(100000)
c      REAL*4 RBUF(N)
c	BYTE BBUF(100000)
c      INTEGER*2 HBUF(100000)
c      INTEGER*4 FBUF(100000)
C      EQUIVALENCE (RBUF,HBUF,FBUF)

      INTEGER*4 I,J,L,ILINE,ILINE0,IND
      REAL*4 OFFSET,RSLI,R

      R = 1./ZOOMS
      OFFSET = 0.5
      IF (ZOOMS.LT.1.0) OFFSET=1.0
C
C     ....Assign input pixel to each pixel on output image line.
      DO I=1,NSO			    !I=output pixel index
         J = R*(I-OFFSET) + 1.00002 !J=corresponding input pixel index
         IF (J.LT.1) J=1
         IF (J.GT.NSI) J=NSI
         SAMP(I) = J		    !Build sample look-up table
c	print *,"samp(i) ",SAMP(I)
      ENDDO
C
C     ....Set up stretch table for byte and halfword input
      IF (LFLAG.EQ.1) THEN
         IF (ICODE.EQ.1.and.OCODE.EQ.1) CALL TBLGEN(GSCALE,ILO,IHI,tbl)
	 IF (ICODE.EQ.1.and.OCODE.EQ.2) CALL TBLGENH(GSCALE,ILO,IHI,tblh)
	 IF (ICODE.EQ.1.and.OCODE.EQ.3) CALL TBLGENH(GSCALE,ILO,IHI,tblh)
         IF (ICODE.EQ.2) CALL TBLGENH(GSCALE,ILO,IHI,tblh)
      ENDIF
C
      R = 1./ZOOML			!Input line increment
      RSLI = SLI + .000002		!Float input starting-line
      OFFSET = 0.5
      IF (ZOOML.LT.1.0) OFFSET=1.
      ILINE0 = 0

      DO 100 L=1,NLO
      ILINE = R*(L-OFFSET) + RSLI	!Compute input line number
      IF (ILINE.NE.ILINE0) CALL XVREAD(IUNIT,buf,IND,'LINE',ILINE,
     & 'SAMP',SSI,'NSAMPS',NSI,'BAND',IB,' ')
c	print *, l,ind, sso, buf(1),buf(2),buf(3)
c call expand - buf is from xvread, samp is for lut, last param is buffer to output
      IF (ICODE.EQ.1.and.OCODE.EQ.1) THEN
         CALL EXPAND(NSO,BUF,SAMP,LFLAG,TBL,obuf(sso))
      ELSE IF (ICODE.EQ.1.and.OCODE.EQ.2) THEN
	CALL EXPANDH(NSO,BUF,SAMP,LFLAG,TBLH,obuf(sso))	
      ELSE IF (ICODE.EQ.1.and.OCODE.EQ.3) THEN
	CALL EXPANDF2(NSO,BUF,SAMP,LFLAG,TBLH,obuf(sso))
      ELSE IF (ICODE.EQ.2.and.OCODE.EQ.2) THEN
         CALL EXPANDH(NSO,BUF,SAMP,LFLAG,TBLH,obuf(sso))
      ELSE IF (ICODE.EQ.2.and.OCODE.EQ.3) THEN
	 CALL EXPANDF(NSO,BUF,SAMP,LFLAG,GSCALE,ILO,IHI,obuf(sso))
      ELSE IF (ICODE.EQ.2.and.OCODE.EQ.4) THEN
	CALL EXPANDR(NSO,BUF,SAMP,GSCALE,obuf(sso))
      ELSE IF (ICODE.EQ.3) THEN
         CALL EXPANDF(NSO,BUF,SAMP,LFLAG,GSCALE,ILO,IHI,obuf(sso))
      ELSE
         CALL EXPANDR(NSO,BUF,SAMP,GSCALE,obuf(sso))
      ENDIF
	
      CALL XVWRIT(OUNIT,OBUF,IND,'LINE',SLO+L-1,'BAND',IB,' ')
  100 ILINE0 = ILINE

      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Generate look-up table (TBL) for byte input.  Table causes scaling of input
C by GSCALE and truncation of output at ILO and IHI.
C
      SUBROUTINE TBLGEN(GSCALE,ILO,IHI,tbl)
	IMPLICIT NONE
	INTEGER*4 ILO,IHI,IDN,I
	REAL*4 gscale,D
      BYTE TBL(0:255)
c      INCLUDE 'fortport.fin'
	INCLUDE 'fortport'
      DO I=0,255			!Do DN=0 to 255
         D = GSCALE*I			!Scale DN by GSCALE
         IF (D.LT.0.) D=D-.5		!Round off to nearest integer
         IF (D.GT.0.) D=D+.5
         IDN = D
         IDN = MAX0(IDN,ILO)
         IDN = MIN0(IDN,IHI)
         TBL(I) = INT2BYTE(IDN)
      ENDDO

      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Generate look-up table (TBLH) for halfword input.  Table causes scaling of
C input by GSCALE and truncation of output at ILO and IHI.
C
      SUBROUTINE TBLGENH(GSCALE,ILO,IHI,tblh)
	IMPLICIT NONE
      INTEGER*2 TBLH(-32768:32767)
	INTEGER*4 ILO,IHI,I,IDN
	REAL*4 gscale,D

      DO I=-32768,32767
         D = GSCALE*I
         IF (D.LT.0.) D=D-.5		!Round-off to nearest integer
         IF (D.GT.0.) D=D+.5
         IDN = D
         IDN = MAX0(IDN,ILO)
         IDN = MIN0(IDN,IHI)
         TBLH(I) = IDN
      ENDDO

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Magnify a byte image line without interpolation.
C
      SUBROUTINE EXPAND(NSO,BUF,SAMP,LFLAG,TBL,obuf)
      IMPLICIT NONE
        INTEGER*4 NSO,LFLAG,I,SAMP(*)
	REAL*4 BUF(*),OBUF(NSO)
	BYTE TBL(0:255)
c      INCLUDE 'fortport.fin'
	INCLUDE 'fortport'
C
      IF (LFLAG.EQ.0) THEN
         DO I=1,NSO
            OBUF(I) = BUF(SAMP(I))
         ENDDO
      ELSE
         DO I=1,NSO
            OBUF(I) = TBL(BYTE2INT(INT(BUF(SAMP(I)))))
c               print *, OBUF(I), SAMP(I), BUF(SAMP(I))
         ENDDO
      ENDIF
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Magnify a halfword image line without interpolation.
C
      SUBROUTINE EXPANDH(NSO,BUF,SAMP,LFLAG,TBLH,obuf)
      IMPLICIT NONE
      INTEGER*4 NSO,LFLAG,I
      INTEGER*2 HBUF(100000),TBLH(-32768:32767)
      INTEGER*4 SAMP(NSO)
	REAL*4 BUF(*)
	REAL*4 OBUF(nso)

      IF (LFLAG.EQ.0) THEN
         DO I=1,NSO
            HBUF(I) = BUF(SAMP(I))
	    OBUF(I) = FLOAT(HBUF(I))
c		print *, ">", OBUF(I), HBUF(i), SAMP(I), buf(samp(i))
         ENDDO
      ELSE
         DO I=1,NSO
            HBUF(I) = TBLH(INT(BUF(SAMP(I))))
	    OBUF(I) = FLOAT(HBUF(I))
c		print *, OBUF(I), SAMP(I)
         ENDDO
      ENDIF
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Magnify a halfword image line without interpolation.
C
      SUBROUTINE EXPANDF2(NSO,BUF,SAMP,LFLAG,TBLH,obuf)
      IMPLICIT NONE
      INTEGER*4 NSO,LFLAG,I
      INTEGER*2 TBLH(-32768:32767)
      INTEGER*4 FBUF(100000)
      INTEGER*4 SAMP(NSO)
        REAL*4 BUF(*)
        REAL*4 OBUF(nso)
      IF (LFLAG.EQ.0) THEN
         DO I=1,NSO
            FBUF(I) = BUF(SAMP(I))
            OBUF(I) = FLOAT(FBUF(I))
c               print *, ">", OBUF(I), HBUF(i), SAMP(I), buf(samp(i))
         ENDDO
      ELSE
         DO I=1,NSO
            FBUF(I) = TBLH(INT(BUF(SAMP(I))))
            OBUF(I) = FLOAT(FBUF(I))
c               print *, OBUF(I), SAMP(I)
         ENDDO
      ENDIF

      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Magnify a fullword image line without interpolation.
C
      SUBROUTINE EXPANDF(NSO,BUF,SAMP,LFLAG,SCALE,ILO,IHI,obuf)
      IMPLICIT NONE
      INTEGER*4 NSO,LFLAG,ILO,IHI
      INTEGER*4 I,DN,FBUF(100000),SAMP(NSO)
	REAL*4 BUF(*),OBUF(nso)
      REAL*4 SCALE

      IF (LFLAG.EQ.0) THEN
         DO I=1,NSO
            FBUF(I) = BUF(SAMP(I))
	    OBUF(I) = FLOAT(FBUF(I))
c               print *, ">", OBUF(I), FBUF(i), SAMP(I), buf(samp(i))
         ENDDO
      ELSE
         DO I=1,NSO
            DN = NINT(SCALE*BUF(SAMP(I)))
            IF (DN.GT.IHI) THEN
               DN = IHI
            ELSE IF (DN.LT.ILO) THEN
               DN = ILO
            ENDIF
            FBUF(I) = DN
	    OBUF(I) = FLOAT(FBUF(I))
         ENDDO
      ENDIF   
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Magnify a REAL*4 image line without interpolation.
C
      SUBROUTINE EXPANDR(NSO,BUF,SAMP,SCALE,rbuf)
      IMPLICIT NONE
      INTEGER*4 NSO,I
	INTEGER*4 SAMP(NSO)
      REAL*4 SCALE,BUF(*),RBUF(NSO)

      IF (SCALE.NE.1.0) THEN
         DO I=1,NSO
            RBUF(I) = SCALE*BUF(SAMP(I))
        ENDDO
      ELSE
         DO I=1,NSO
           RBUF(I) = BUF(SAMP(I))
         ENDDO
      ENDIF
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create sintrp.f
$ DECK/DOLLARS="$ VOKAGLEVE"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Magnify or reduce an image via interpolation.
C
	SUBROUTINE SINTRP(ib,n,buf,rbuf,obuf,samp,wght)
      IMPLICIT NONE
	INTEGER*4 N     !Dummy since nso is put here but available at CP
      INTEGER*4 IB,SAMP(N)					!,OBUF(N),BUF(N)
      REAL*4 RBUF(N,2),WGHT(N),OBUF(N),BUF(N)

      COMMON/CP/IUNIT,ICODE,SLI,SSI,NLI,NSI
      COMMON/CP/OUNIT,OCODE,SLO,SSO,NLO,NSO,NLOUT,NSOUT
      COMMON/CP/ZOOML,ZOOMS,LZOOM,IZOOM,GSCALE,ILO,IHI,LFLAG
      INTEGER*4 IUNIT,OUNIT     !Input and output logical unit numbers
      INTEGER*4 ICODE,OCODE     !Input and ouput image format
      INTEGER*4 SLI,SSI,NLI,NSI !Input image area (see AREA parameter)
      INTEGER*4 SLO,SSO,NLO,NSO !Output area mapped from input area
      INTEGER*4 NLOUT,NSOUT     !Size of ouput image
      REAL*4 ZOOML,ZOOMS        !Floating point zoom factors
      INTEGER*4 LZOOM,IZOOM     !Integer zoom factors (=0 if not integer)
      INTEGER*4 ILO,IHI         !Min and max DN limits for output image
      INTEGER*4 LFLAG           !=1 if need to check for ILO,IHI saturation
      REAL*4 GSCALE             !Output DN scale factor

      INTEGER*4 I,J,L,I1,I2,IND
      REAL*4 R,SCALE,X0,X

      R = 1./ZOOMS
C
C     ....Set up correspondence between input and output samples
      IF (ZOOMS.GT.1.0) THEN	!Magnify in horizontal direction
         SCALE = 1.0 
         DO I=1,NSO
            X0 = R*(I-.5) + .5  !Translate center of pixel
            I1 = X0		!I1 = left neighbor
            I1 = MAX0(I1,1)
            I2 = MIN0(I1+1,NSI) !I2 = right neighbor
            WGHT(I) =  I2 - X0  !Store weight A
            SAMP(I) = I2	!Store index of right neighbor
         ENDDO
      ELSE			!Compress in horizontal direction
         SCALE = ZOOMS
         DO I=1,NSO
            X = R*I		!Translate right margin of pixel
            I1 = X 		!I1 = left neighbor
            I2 = MIN0(I1+1,NSI) !I2 = right neighbor
            WGHT(I) = 1.0 - (I2-X) !Compute weight 1.0-A
            SAMP(I) = I2	!Store index of right neighbor
         ENDDO
      ENDIF
C
      SCALE = SCALE*GSCALE
      IF (NLI.EQ.1) GOTO 60
      IF (LZOOM.EQ.1) GOTO 50
      IF (ZOOML.GT.1.) THEN	!If vertical zoom .gt. 1, magnify it.
         CALL MAGNIFY(SCALE,NSO,SAMP,WGHT,ib,buf,rbuf,obuf(sso))
      ELSE 			!Else, compress it.
         CALL COMPRESS(SCALE,NSO,SAMP,WGHT,ib,buf,rbuf,obuf(sso))
      ENDIF
      RETURN         

C     ....Special case: LZOOM=1
   50 J = SLI - 1
      DO L=1,NLO
         CALL SREAD(IUNIT,L+J,SSI,NSI,NSO,IZOOM,ZOOMS,SAMP,WGHT,
     &	  ib,buf,rbuf)
         CALL OUT_SCALE(OCODE,SCALE,ILO,IHI,NSO,RBUF,
     &	  obuf(sso))
         CALL XVWRIT(OUNIT,OBUF,IND,'LINE',SLO+L-1,'BAND',IB,' ')
      ENDDO
      RETURN

C     ....Special case: NLI=1
   60 CALL SREAD(IUNIT,SLI,SSI,NSI,NSO,IZOOM,ZOOMS,SAMP,WGHT,
     & ib,buf,rbuf)
      CALL OUT_SCALE(OCODE,SCALE,ILO,IHI,NSO,RBUF,obuf(sso))
      DO L=1,NLO
         CALL XVWRIT(OUNIT,OBUF,IND,'LINE',SLO+L-1,'BAND',IB,' ')
      ENDDO
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE OUT_SCALE(OCODE,SCALE,ILO,IHI,NSO,RBUF,obuf)
      IMPLICIT NONE
      INTEGER*4 OCODE,ILO,IHI,NSO
      REAL*4 SCALE,RBUF(NSO),OBUF(NSO)

      INTEGER*4 I,DN

      IF (OCODE.NE.4) THEN
         DO I=1,NSO
            DN = NINT(SCALE*RBUF(I))         
            IF (DN.GT.IHI) THEN
               DN = IHI
            ELSE IF (DN.LT.ILO) THEN
               DN = ILO
            ENDIF
            OBUF(I) = REAL(DN)
         ENDDO
      ELSE
         DO I=1,NSO
            OBUF(I) = SCALE*RBUF(I)
         ENDDO
      ENDIF

      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create sread.f
$ DECK/DOLLARS="$ VOKAGLEVE"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Read an image line and magnify or reduce it via interpolation.
C The input line (BUF) may be byte, halfword, fullword, or real*4.
C The output line (RBUF) is REAL*4.
C
c	merged from sread.F (build 562) and size.com sread.f (build 804)
	SUBROUTINE SREAD(IUNIT,LINE,SSI,NSI,NSO,IZOOM,ZOOMS,
     &		SAMP,WGHT,ib,buf,rbuf)
	IMPLICIT NONE
      INTEGER*4 IUNIT,LINE,SSI,NSI,NSO,IZOOM,IB
	INTEGER*4 IND,SAMP(NSO)
      REAL*4 ZOOMS,WGHT(NSO),BUF(NSI),RBUF(NSO)

      IF (IZOOM.EQ.1) THEN
         CALL XVREAD(IUNIT,rbuf,ind,'LINE',LINE,'SAMP',SSI,
     &		   'NSAMPS',NSI,'BAND',IB,' ')
         RETURN
      ENDIF

      CALL XVREAD(IUNIT,buf,ind,'LINE',LINE,'SAMP',SSI,
     &   'NSAMPS',NSI,'BAND',IB,' ')

      IF (NSI.EQ.1) THEN		
         CALL MVE(7,NSO,BUF,RBUF,0,1)		!Replicate pixel NSO times.
      ELSE IF (ZOOMS.GT.1.) THEN
         CALL MAG(NSO,NSI,BUF,SAMP,WGHT,rbuf)	!Magnify the line
      ELSE IF (IZOOM.LT.0) THEN
         CALL SHRINZ(IZOOM,NSO,NSI,BUF,rbuf)	!Compress line via integral zoom
      ELSE				
         CALL SHRINK(NSO,NSI,BUF,SAMP,WGHT,rbuf) !Compress line via real zoom
      ENDIF
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Magnifies an image line via interpolation.
C
      SUBROUTINE MAG(NSO,NSI,BUF,SAMP,WGHT,rbuf)
	IMPLICIT NONE
      INTEGER*4 NSO,NSI
	INTEGER*4 I,I0,I2,SAMP(NSO)
      REAL*4 BUF(NSI),RBUF(NSO),WGHT(NSO)
	REAL*4 A,D1,D2

      I0 = 0
      D1 = 0
      D2 = BUF(1)

      DO I=1,NSO
         I2 = SAMP(I)
         IF (I2.NE.I0) THEN
            I0 = I2
            D1 = D2
            D2 = BUF(I2)
         ENDIF
         A = WGHT(I)
         RBUF(I) = A*D1 + (1.0-A)*D2
      ENDDO
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compress a line by a floating pt zoom factor
C
      SUBROUTINE SHRINK(NSO,NSI,BUF,SAMP,WGHT,rbuf)
	IMPLICIT NONE
      INTEGER*4 NSO,NSI
	INTEGER*4 I,I1,I2,SAMP(NSO)
	REAL*4 D,D0,D1,RSUM
      REAL*4 BUF(NSI),RBUF(NSO),WGHT(NSO)

      I1 = 1
      D1 = BUF(1)
      D0 = 0.0
 
      DO I=1,NSO
         D = D1 - D0
         I2 = SAMP(I)
         I1 = I1 + 1
         IF (I1.LT.I2) THEN
            RSUM = 0
            DO WHILE (I1.LT.I2)
               RSUM = RSUM + BUF(I1)
               I1 = I1 + 1
            ENDDO
            D = D + RSUM
         ENDIF
         D1 = BUF(I2)
         D0 = WGHT(I)*D1
         RBUF(I) = D + D0
      ENDDO
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compresses a line by an integer zoom factor
C
      SUBROUTINE SHRINZ(IZOOM,NSO,NSI,BUF,RBUF)
	IMPLICIT NONE
      INTEGER*4 IZOOM,NSO,NSI,INC
	INTEGER*4 I,II,J
      REAL*4 BUF(NSI),RBUF(NSO)

      INC = -IZOOM
      II = 1

      DO I=1,NSO
         RBUF(I) = 0
         DO J=1,INC
            RBUF(I) = RBUF(I) + BUF(II)
            II = II + 1
         ENDDO
      ENDDO
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create magnify.f
$ DECK/DOLLARS="$ VOKAGLEVE"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Magnify an image.  The vertical direction is magnified.  The
C horizontal dimension may be magnified or compressed.
C Input and output images may be byte, halfword, fullword, or real*4.
C Mixed data types (e.g. byte input, halfword output) are permitted.
C
      SUBROUTINE MAGNIFY(SCALE,N,SAMP,WGHT,ib,buf,rbuf,obuf)
      IMPLICIT NONE
      INTEGER*4 N,IB,SAMP(N)
	REAL*4 BUF(*),OBUF(N)
      REAL*4 SCALE,RBUF(N,2),WGHT(N)

      COMMON/CP/IUNIT,ICODE,SLI,SSI,NLI,NSI
      COMMON/CP/OUNIT,OCODE,SLO,SSO,NLO,NSO,NLOUT,NSOUT
      COMMON/CP/ZOOML,ZOOMS,LZOOM,IZOOM,GSCALE,ILO,IHI,LFLAG
      INTEGER*4 IUNIT,ICODE,SLI,SSI,NLI,NSI
      INTEGER*4 OUNIT,OCODE,SLO,SSO,NLO,NSO,NLOUT,NSOUT
      INTEGER*4 LZOOM,IZOOM,ILO,IHI,LFLAG
      REAL*4 ZOOML,ZOOMS,GSCALE

      INTEGER*4 ELI,IND,L
      INTEGER*4 I1,I2,J1,J2,ITEMP,JSAVE
      REAL*4 REC,R,Y0,D,C
      
      R = 1.0/ZOOML
      REC = SLI - .5
      ELI = SLI + NLI - 1
      I1 = 1
      I2 = 2
      JSAVE = 0
      J2 = 0

      DO 50 L=1,NLO
      Y0 = R*(L-.5) + REC
      J1 = Y0
      J1 = MAX0(J1,SLI)
      J1 = MIN0(J1,ELI-1)
      IF (J1.EQ.JSAVE) GOTO 40	!Skip read (image lines already in memory)
      JSAVE = J1
      ITEMP = I1
      I1 = I2
      I2 = ITEMP
      IF (J1.GT.J2) CALL SREAD(IUNIT,J1,SSI,NSI,NSO,IZOOM,ZOOMS,
     & SAMP,WGHT,ib,buf,rbuf(1,i1))
      J2 = J1 + 1
      CALL SREAD(IUNIT,J2,SSI,NSI,NSO,IZOOM,ZOOMS,
     & SAMP,WGHT,ib,buf,rbuf(1,i2))
   40 D = Y0 - J1		!Compute vertical weights
      C = (1.-D)*SCALE
      D = D*SCALE
      CALL INTRPV(OCODE,NSO,ILO,IHI,C,D,RBUF(1,I1),RBUF(1,I2),
     & obuf(sso))
   50 CALL XVWRIT(OUNIT,OBUF,ind,'LINE',SLO+L-1,'BAND',IB,' ')
         
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compute an output image line by iterpolating between two image
C lines BUF1 and BUF2.
C If OCODE=4, the output line is REAL*4.  Otherwise it is INTEGER*4.
C
      SUBROUTINE INTRPV(OCODE,NSO,ILO,IHI,C,D,BUF1,BUF2,obuf)
      IMPLICIT NONE
      INTEGER*4 OCODE,NSO,ILO,IHI
      REAL*4 C,D,BUF1(NSO),BUF2(NSO),OBUF(NSO)

      INTEGER*4 I,DN

      IF (OCODE.EQ.4) GOTO 50
      DO I=1,NSO
         DN = NINT(BUF1(I)*C + BUF2(I)*D)
         IF (DN.GT.IHI) THEN
            DN = IHI
         ELSE IF (DN.LT.ILO) THEN
            DN = ILO
 	 ENDIF
         OBUF(I) = REAL(DN)
      ENDDO
      RETURN
C
C    ....Here for real*4 output
   50 DO I=1,NSO
         OBUF(I) = BUF1(I)*C + BUF2(I)*D
      ENDDO
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create compress.f
$ DECK/DOLLARS="$ VOKAGLEVE"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compress an image.  The vertical direction is compressed.  The
C horizontal direction may be magnified or compressed.
C 
C
	SUBROUTINE COMPRESS(SCALE,NS,SAMP,WGHT,ib,buf,rbuf,obuf)
      IMPLICIT NONE
      INTEGER*4 NS,IB,SAMP(NS)
      REAL*4 SCALE,BUF(1),RBUF(NS,2),WGHT(NS),OBUF(NS)

      COMMON/CP/IUNIT,ICODE,SLI,SSI,NLI,NSI
      COMMON/CP/OUNIT,OCODE,SLO,SSO,NLO,NSO,NLOUT,NSOUT
      COMMON/CP/ZOOML,ZOOMS,LZOOM,IZOOM,GSCALE,ILO,IHI,LFLAG
      INTEGER*4 IUNIT,ICODE,SLI,SSI,NLI,NSI
      INTEGER*4 OUNIT,OCODE,SLO,SSO,NLO,NSO,NLOUT,NSOUT
      INTEGER*4 LZOOM,IZOOM,ILO,IHI,LFLAG
      REAL*4 ZOOML,ZOOMS,GSCALE

      INTEGER*4 I1,I2,J1,J2
      INTEGER*4 J,L,N,ELI,LINE,IND,INC
      REAL*4 R,C,D,Y2

      R = 1.0/ZOOML
      ELI = SLI + NLI - 1
      I1 = 1
      I2 = 2
      IF (LZOOM.LT.0) GOTO 40
C
C     ....Here to compress an image using a real zoom in the vertical
C     ....direction.  The horizontal zoom may be real or integral valued.
      J1 = SLI
      C = ZOOML*SCALE
      D = 0.
      CALL SREAD(IUNIT,J1,SSI,NSI,NSO,IZOOM,ZOOMS,
     & SAMP,WGHT,ib,buf,rbuf(1,i2))
C
      DO 20 L=1,NLO
      CALL SMUL(NSO,RBUF(1,I2),RBUF,1.-D)
      Y2 = L*R + SLI
      J2 = Y2
      J2 = MIN0(J2,ELI)
      D = Y2 - J2
      J1 = MIN0(J1+1,ELI)
      DO WHILE (J1.NE.J2)
         CALL SREAD(IUNIT,J1,SSI,NSI,NSO,IZOOM,ZOOMS,
     &    SAMP,WGHT,ib,buf,rbuf(1,i2))
         CALL ADDV(7,NSO,RBUF(1,I2),RBUF,1,1)
         J1 = MIN0(J1+1,ELI)
      ENDDO
      CALL SREAD(IUNIT,J1,SSI,NSI,NSO,IZOOM,ZOOMS,
     & SAMP,WGHT,ib,buf,rbuf(1,i2))
      CALL INTRPV(OCODE,NSO,ILO,IHI,C,C*D,RBUF,RBUF(1,I2),
     & obuf(sso))
   20 CALL XVWRIT(OUNIT,OBUF,ind,'LINE',SLO+L-1,'BAND',IB,' ')
      RETURN

C     ....Here to compress vertical scale by an integral lzoom
   40 INC = -LZOOM
      N = INC - 1
      LINE = SLI
      C = ZOOML*SCALE

      DO 50 L=1,NLO
      CALL SREAD(IUNIT,LINE,SSI,NSI,NSO,IZOOM,ZOOMS,
     & SAMP,WGHT,ib,buf,rbuf(1,i2))
      DO J=1,N
         CALL SREAD(IUNIT,LINE+J,SSI,NSI,NSO,IZOOM,ZOOMS,
     &    SAMP,WGHT,ib,buf,rbuf)
         CALL ADDV(7,NSO,RBUF,RBUF(1,I2),1,1)
      ENDDO
      LINE = LINE + INC
      CALL OUT_SCALE(OCODE,C,ILO,IHI,NSO,RBUF(1,I2),obuf(sso))
   50 CALL XVWRIT(OUNIT,OBUF,ind,'LINE',SLO+L-1,'BAND',IB,' ')

      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Multiply a line by a constant C
C
      SUBROUTINE SMUL(NSO,RIN,ROUT,C)
	IMPLICIT NONE
	INTEGER*4 NSO,I
      REAL*4 RIN(NSO),ROUT(NSO),C
C
      DO I=1,NSO
         ROUT(I) = C*RIN(I)
      ENDDO
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create size.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM size

   To Create the build file give the command:

		$ vimake size			(VMS)
   or
		% vimake size			(Unix)

************************************************************************/
#define PROGRAM	size

#define MODULE_LIST size.f getsize.f update_label.f snoin.f sintrp.f \
           sread.f magnify.f compress.f 
#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define R2LIB
#define FTNINC_LIST fortport mp_for_defs

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

/*#define DEBUG	/* comment out on delivery */
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create size.pdf
PROCESS HELP=*
PARM INP     TYPE=STRING
PARM OUT     TYPE=STRING
PARM SIZE    TYPE=INTEGER COUNT=4   VALID=(0:100000)	DEFAULT=(1,1,0,0)
PARM NL      TYPE=INTEGER COUNT=0:1 VALID=(0:100000)	DEFAULT=0
PARM NS      TYPE=INTEGER COUNT=0:1 VALID=(0:100000)	DEFAULT=0
PARM SB      TYPE=INTEGER DEFAULT=1
PARM NB      TYPE=INTEGER DEFAULT=0
PARM BANDS   TYPE=INTEGER COUNT=2 DEFAULT=(1,0)
PARM AREA    TYPE=INTEGER COUNT=0:4 VALID=(0:100000)	DEFAULT=--
PARM IOFFSET TYPE=INTEGER COUNT=0:2 VALID=(-100000:100000) DEFAULT=--
PARM NOIN    TYPE=KEYWORD COUNT=0:1 VALID=NOIN		DEFAULT=--
PARM ZOOM    TYPE=REAL    COUNT=0:1 VALID=(-100000:100000) DEFAULT=--
PARM LZOOM   TYPE=REAL    COUNT=0:1 VALID=(-100000:100000) DEFAULT=--
PARM SZOOM   TYPE=REAL    COUNT=0:1 VALID=(-100000:100000) DEFAULT=--
PARM SCALE   TYPE=REAL    COUNT=1			DEFAULT=1.0
PARM LIMITS  TYPE=INTEGER COUNT=0:2			DEFAULT=--
PARM OFORM   TYPE=KEYWORD COUNT=0:1 VALID=(BYTE,HALF,FULL,REAL)	DEFAULT=--
PARM DEBUG   TYPE=KEYWORD           VALID=(DEBUG,NODEBUG) DEFAULT=NODEBUG
END-PROC
.TITLE
VICAR program SIZE
.HELP
PURPOSE:
SIZE is a VICAR applications program which may be used to magnify or compress
the size of an image and/or change its aspect ratio.  The program may also
be used to re-scale the input DN values and to change the data format
(e.g. byte-to-halfword).  Note, however, that if image size or aspect ratio
manipulation is not required, these latter two operations are more efficiently
accomplished via program CFORM.  The program will work on 3-dimensional (cube)
files (unless they are in BIP format), but no magnification or compression is
allowed in the third dimension;  the same operation is performed on each band.

EXECUTION STATEMENT:

      SIZE  INP=IPIC  OUT=OPIC  user-parameters...

where IPIC is the input image and OPIC is the output image.  IPIC and OPIC
may be in byte, halfword (16-bit integer), fullword (32-bit integer), or
floating point (REAL*4) data format.

OPIC will normally have the same data format as IPIC.  Use the OFORM parameter
to change the output format.

SIZE performs  bilinear interpolation to magnify or compress the image.
The 'NOIN keyword may be used to suppress interpolation.  See sections on image
magnification and reduction below.

.page
OPERATION:

The size of the output image is specified in one of two ways:

(1) Explicitly specifying it with the NL and NS parameters:

        SIZE  INP  OUT  NL=500  NS=500
    or  SIZE  INP  OUT  SIZE=(1,1,500,500)

(2) Specifying a magnification or compression (zoom) factor:

        SIZE  INP  OUT  ZOOM=3		! 3x magnification
        SIZE  INP  OUT  ZOOM=-2         ! 2x compression
    or  SIZE  INP  OUT  ZOOM=0.5        ! 2x compression

The ZOOM factor can be a floating point value.
A negative ZOOM results in image compression.  I.e. ZOOM= -2.5 is the
same as ZOOM=0.4

Note the if both NL,NS ans ZOOM are specified, the ZOOM parameter will
determine the magnification/compression factor and NL,NS will determine the
size of the output image.

Independent zooms may be specified in the line (vertical) and sample
(horizontal) directions:

        SIZE  INP  OUT  LZOOM=2  SZOOM=3
        SIZE  INP  OUT  LZOOM=3  SZOOM=-2

As the last example implies, the image may be magnified in one direction
and compressed in the other.

The AREA parameter may be used to restrict processing to an area of the
input image.  For example:

		SIZE  IPIC  OPIC  ZOOM=-3  AREA=(10,10,100,100)
is equivalent to
		COPY  IPIC  IDS  (10,10,100,100)
		SIZE  IDS  OPIC  ZOOM=-3

The output data format may be different from the input format (see OFORM
keyword).  The image DNs may be optionally rescaled at this point via parameter
SCALE.  Note that rescaling may be necessary when converting to a smaller data
format (e.g. halfword-to-byte).  If a sample value is outside the range of the
output format (e.g. 0 to 255 DN for byte data), the output DN will be truncated.
The output DN range may be further limited via the LIMITS parameter.

.page
SPECIFYING THE SIZE OF THE OUTPUT IMAGE:

The size of the output image is specified either by entering the number of
lines and samples in the VICAR size field, or by specifying a magnification
or compression factor via the ZOOM, LZOOM, or SZOOM parameters:

Ex:  Enlarging a 3x3 image to a 9x9 image can be achieved in the
     following equivalent ways:

		SIZE  A  B  SIZE=(1,1,9,9)
        or      SIZE  A  B  NL=9  NS=9
	or	SIZE  A  B  ZOOM=3
        or      SIZE  A  B  LZOOM=3  SZOOM=3

If the output image size is specified by entering the number of lines and
samples, then the ZOOM factor is determined by computing the ratio between
the output and input picture dimensions.  Independent ZOOM factors are
computed in the line and sample dimensions, and may result in a change in
the image aspect ratio:

	z1=NLO/NLI		z2=NSO/NSI

where the input image size is NLI x NSI and the output image size is NLO x NSO.
The computations are performed in floating point, so that the output picture
size is not necessarily a multiple of the input picture size (i.e. z1 and
z2 are floating-point numbers).

The ZOOM factors may be specified via the ZOOM parameter,
		    ZOOM=z
where z1=z2=z, or independently via the LZOOM and SZOOM parameters:
		LZOOM=z1  SZOOM=z2

When the zoom factor is an integer, it is identical in function to the ZOOM
option in program XVD.  If z is positive, the input picture size
is multiplied by z.  If z is negative, the picture size is divided by -z.
Note that z=-2 is equivalent to z=0.5.

Specification of a zoom factor does not override the corresponding NL and/or 
NS values in the VICAR SIZE field.  If a value of NL or NS is specified that 
is smaller than ZOOM*(input NL or NS), then the image will be truncated in
that dimension.  If greater, then a blank area will be appended on the
bottom or right edge, respectively.

The IOFFSET parameter may be used to start the output image at a (line,sample)
coordinate other than (1,1).  Let the input image be:

                           1  2  3
                           2  3  4
                           3  4  5

then
		SIZE  INP  OUT  NL=8  NS=8  ZOOM=2  IOFFSET=(2,2)  'NOIN

will result in the following:

                    0  0  0  0  0  0  0  0
                    0  1  1  2  2  3  3  0
                    0  1  1  2  2  3  3  0
                    0  2  2  3  3  4  4  0
                    0  2  2  3  3  4  4  0
                    0  3  3  4  4  5  5  0
                    0  3  3  4  4  5  5  0
                    0  0  0  0  0  0  0  0

.page
IMAGE MAGNIFICATION:

The following example illustrates how image magnification is treated. Let
the input picture A be a 3x3 image as follows:

			       2  5  8
			A  =   5  8 11
			       8 11 14

The statement
		SIZE  A  B  SIZE=(1,1,9,9)  'NOIN
	or	SIZE  A  B  ZOOM=3  'NOIN

will produce a 9x9 output image B by replicating each input sample into a
3x3 pixel area:

		       2  2  2  5  5  5  8  8  8
		       2  2  2  5  5  5  8  8  8
		       2  2  2  5  5  5  8  8  8
		       5  5  5  8  8  8 11 11 11
		B  =   5  5  5  8  8  8 11 11 11
		       5  5  5  8  8  8 11 11 11
		       8  8  8 11 11 11 14 14 14
		       8  8  8 11 11 11 14 14 14
		       8  8  8 11 11 11 14 14 14

The statement
		SIZE  A  B  SIZE=(1,1,9,9)  ZOOM=3

will produce a 9x9 output image by interpolating between the four nearest
neighbors and extrapolating around the picture borders.

		       0  1  2  3  4  5  6  7  8
		       1  2  3  4  5  6  7  8  9
		       2  3  4  5  6  7  8  9 10
		       3  4  5  6  7  8  9 10 11
		B  =   4  5  6  7  8  9 10 11 12
		       5  6  7  8  9 10 11 12 13
		       6  7  8  9 10 11 12 13 14
		       7  8  9 10 11 12 13 14 15
		       8  9 10 11 12 13 14 15 16

As in the no-interpolation case above, each input sample has been "blown up"
to fill a 3x3 area.  However, because interpolation is performed, the input
sample values equal the output sample values only at the geometric centers
of these 3x3 areas.

Note that the magnified image output by SIZE differs slightly from what one
might obtain using GEOM, MGEOM, or GEOMA. The user may find it instructive
to attempt to achieve an identical result as the example above by using GEOM
or GEOMA.

.page
IMAGE COMPRESSION:

Image compression is treated as the functional inverse of image magnification.
Unless the keyword 'NOIN is specified, the compression is performed via area
averaging.  To illustrate, let picture B be the 9x9 image of our previous
example:

		       0  1  2  3  4  5  6  7  8
		       1  2  3  4  5  6  7  8  9
		       2  3  4  5  6  7  8  9 10
		       3  4  5  6  7  8  9 10 11
		B  =   4  5  6  7  8  9 10 11 12
		       5  6  7  8  9 10 11 12 13
		       6  7  8  9 10 11 12 13 14
		       7  8  9 10 11 12 13 14 15
		       8  9 10 11 12 13 14 15 16

The statement:
		SIZE  B  C  ZOOM=-3

will cause each output pixel to be computed by averaging a 3x3 area of
the input image.  For example, output pixel (1,1) = (0+1+2+1+2+3+2+3+4)/9
The resulting output image C will be identical to our original input image A:

			       2  5  8
			C  =   5  8 11
			       8 11 14

The area averaging operation is extended to non-itegral zoom factors by
assigning fractional weights to samples around the area margins, and unit
weights to interior samples. The output sample is then the weighted samples
divided by the sum of their weights.

If the keyword 'NOIN is specified,

		SIZE  IPIC  OPIC  ZOOM=-N  'NOIN

then no pixel interpolation is performed.  The output image is generated
by selecting every Nth image line from IPIC, and every Nth pixel of each of
these lines, begining with pixel (1,1).  Note that when the output picture
is several times smaller than the input picture, most of the samples in the
input image are ignored in the generation of the output image.

To illustrate, let picture B be the 9x9 image of our previous example:

		       0  1  2  3  4  5  6  7  8
		       1  2  3  4  5  6  7  8  9
		       2  3  4  5  6  7  8  9 10
		       3  4  5  6  7  8  9 10 11
		B  =   4  5  6  7  8  9 10 11 12
		       5  6  7  8  9 10 11 12 13
		       6  7  8  9 10 11 12 13 14
		       7  8  9 10 11 12 13 14 15
		       8  9 10 11 12 13 14 15 16

The statements:
		SIZE  B  C  ZOOM=-3  'NOIN
		SIZE  B  D  ZOOM=-3  'NOIN  AREA=(2,2,8,8)

will generate 3x3 output images C and D of the form:

	     	0  3  6		       2  5  8
	 C  =	3  6  9		D  =   5  8 11
	     	6  9 12		       8 11 14

Note the use of the AREA parameter to begin the resampling at a point other
than pixel (1,1).

The input image may be compressed by a non-integral zoom factor r:

		SIZE  IPIC  OPIC  ZOOM=r  'NOIN

where r is a floating point number between 0 and 1.  Each output sample is
generated by determining where it comes from in the input image and selecting
the sample closest to this point.

.page
UPDATING OF MAP PROJECTION LABEL INFORMATION:

If the input image has map projection labels, the projection information
will be updated to reflect changes in the geometry as a result of size
changes and image offsets (see AREA and IOFFSET parameters).  A new map
projection history label is added and the map projection property label
is updated.

If the map projection is POINT PERSPECTIVE, the following label items are
recomputed:

    FOCAL_PLANE_SCALE
    OPT_AXIS_INTERCEPT_LINE
    OPT_AXIS_INTERCEPT_SAMPLE
    PLANET_CENTER_LINE
    PLANET_CENTER_SAMPLE

For all other projections, the following label items are recomputed:

    LINE_PROJECTION_OFFSET
    SAMPLE_PROJECTION_OFFSET
    MAP_SCALE
    MAP_RESOLUTION

If either MAP_SCALE or MAP_RESOLUTION is missing from the input projection
label, it is computed from the other using the relationship: 

                      PI
     MAP_RESOLUTION = --- * A_AXIS_RADIUS/MAP_SCALE
                      180

.page
EXAMPLES:

Let the input image be a 100 x 100 byte picture. The following equivalent
statements will magnify the input image by a factor of 2.5:

		SIZE  A  B  SIZE=(1,1,250,250)
		SIZE  A  B  ZOOM=2.5

To blow up a 50x50 area from the center of the picture by a factor of 4:

		SIZE  A  B  ZOOM=4  AREA=(26,26,50,50)

To average all the lines of an image together, use

		SIZE  A  B  NL=1

The following equivalent statements magnify the line direction by 2 and
shrinks the sample direction by 2:

		SIZE  A  B  SIZE=(1,1,200,50)
		SIZE  A  B  LZOOM=2  SZOOM=-2

.page
PROGRAM RESTRICTIONS:

Internal buffers allow up to 100.000 samples in byte, half, full or real.
NO support for Double

PROGRAM HISTORY:

Written by: Gary Yagi, 26 January 1976
Cognizant programmer: Lucas Kamp
Revision history:

13 Jul 2012 - LWK - Minor change to fix compilation on Solaris
Jun 06, 2012 - R. J. Bambery - gfortran 4.6.3 changed all dimension (1) values to (*)
             in subroutines to avoid "Fortran runtime error: Index '2' of dimension 1
             of array 'id' above upper bound of 1"
May 06, 2011 - R. J. Bambery - Remove warning messages from gfortran 4.4.4 compiler
Mar 07, 2010 - R. J. Bambery - Remove residual ocode/icode debug messages for 'noin
Feb 14, 2010 - R. J. Bambery - Fix scale parameter for case where input
             is BYTE but output scale requires HALF, FULL, or REAL (signed BYTE 
             to INTEGER/REAL conversion) in 'noin or for HALF to FULL
             and HALF to REAL in 'noin
Feb 10, 2010 - R. J. Bambery - extensive reworking of internal code to
             remove final conflicts between real/full buffers on
             Linux systems (g77 version 3.4.6) 
Feb 05, 2010 - R. J. Bambery - full 64-bit compliance on MacOSX (intel/PowerPC)
             gfortran version 4.4.2
             reworked internals to perform in real*4 processing,
             Massive failures for half and full word images in 'noin
             calls due to equivalencing internal buffers to byte, half, full
             and real. Apparently, the internal routines called via snoin.f
             had gotten older code mixed in with new after afids build 562
Aug 24, 2009 - smyth - fixed nsi parameters in several routines
Apr 14, 2009 - PKim - Increased image size to 100,000 from 50000
Jan 04  lwk  Added support for 3-D (cube) files;  removed Magellan option
             (which will live on as pgm. SIZEMGN)
18 Dec 03 -  DLR   INCREASED BUFFER SIZES TO ALLOW LINES UP TO 100000.
              FIXED PDF.
29 May 03 -  AXC  Fixed ABEND when LZOOM is specified but not SZOOM.
             (AR-108538)
		     Initialized a character buffer. (AR-104344) 
22 Aug 00 -  GMY  Fix bug when ZOOM not specified and NL is same as input.
             (AR 104590)
24 Apr 00 -  GMY  Fix bug when ZOOM, NL, NS are specified for case where
             NL,NS specifies output is to be same size as input.
07 Dec 99 -  GMY  (Day of infamy)  Major reorganization of code and
             rewritting of test script.  Fixed IOFFSET problem for
             interpolation case.  When updating map projection labels,
             if resolution or scale is missing, it is computed from the
             other.
19 Aug 99 -  EMS - fixed bug that was giving invalid values around the edges
		     of the output image
 9 jul 98 -  LWK - changed printout of zoom factor for integer reduction from
		     "ZOOM -N" to "ZOOM 1/N"
24 jun 98 -  LWK - corrected MAP_RESOLUTION update:  it should be multiplied
		     by ZOOM, but pgm was dividing!
21 MAY 98 -  BAM   INCREASED BUFFER SIZES TO ALLOW LINES UP TO 50000.
              FIXED PDF.
04 Feb 98 -  GMY  Changed SAMP from I*2 to I*4 to fix bug when NS > 32767
 3 dec 97 -  LWK - changed ENTRY's to SUBROUTINE to avoid Alpha compiler bug
04 Sep 97 -  LWK   corrected computation of effect of zoom on MP line/samp 
		     items, since only (0.5,0.5) is fixed in zoom.
06 Jul 97 -  LWK   added IOFFSET parameter
26 Jun 95 -  SMC  FR 89394: Undo FR 89272/89275, because it caused problems.
                        and fixed it another way
14 Jun 95 - SMC  FR 89272: fixed ABEND when LZOOM=-2 on odd number line images
              FR 89275: fixed image output so that LZOOM=-2 will begin
                        processing on the first line instead of the second
12 sep 95 - LWK - corrected the scaling of the LINE/SAMP_PROJECTION_OFFSET
		    label items:  these are defined with respect to (1,1),
		    not (0,0)!
28 Apr 95 - FFM  - Fix FR 82982:
              1. Fixed dcl delete statement in test pdf.
              2. Corrected error in test #2 about m.dat.
              3. Removed a test case in test #7 which needs too
                 much disk space.
              4. Fixed an error in xvmessage in routine GETSIZE.
                 So it will not print out a blank line or a line 
                 has meaningless infor on ANDES.
                 
25 Oct 94 - FFM   Fix FR 85697:
              1. Add keyword DEBUG to print the informational message
                 for routine mp_label_read.
              2. enlarge buffer to handle output image up to 50,000 
                 samples. If larger than 50,000 samples, SIZE will 
                 abend with a message to inform user the cause of the
                 abend.
16 Nov 93 -  FFM   ported to unix. The major changes are :
              1. modify XV,XL routines
              2. change int, byte equivalence to INT2BYTE, BYTE2INT,
                 add "include fortport" to all related subroutines
              3. change logical*1 to byte (if pixel)
              4. divide real*4 EPS/1.e-6/ into real*4, and data statement
              5. change hex number to integer in data statement
              6. change mvl to mvlc, add to addv
              7. make optional arguments to required arguments for addv,
                 mvlc, and mve
              8. change QPRINT to XVMESSAGE
              9. remove /LIST from include statement
             10. add all include files to imake file
             11. remove "implicit statement" from slookup include file
             12. create common/c3/iunit, so iunit will pass to subroutine 
                 sread properly, because SGI doesn't default value to 0.
             13. add new MP interface
             14. modify test pdf to be automated for VAX, SUNOS, & SGI.

09 Nov 91 -  LWK   update map labels if present
26 Nov 89 -  GMY   Fix bug in VOLTS option (max index=2249)
26 Nov 89 -  GMY   Added VOLTS keyword.  Program now uses size field to
		     determine size of LOOKUP tables.
11 Feb 89 -  GMY   Fixed image compression with no interpolation algorithm
                to agree with documentation (start with pixel 1,1).
22 DEC 88 -  GMY   MGN lookup table resolution increased 10x.
              Added SMGN1 to handle ZOOM=-3 as special case.
22 NOV 88 -  GMY   Major code modification and reorganization:
		    ...Consolidated Magellan option to subroutines SMGN
	        ...and MGNSIZEINIT.  LOOKUP now a separate keyword.
		    ...Added fullword and REAL*4 capability.
		    ...Rewrote help file.
22 JAN 88 -  AXW   Changed keyword OFORM from 'MAGELLAN' to 'LOOKUP'.
26 JAN 87 -  AXW   Added fullword capability and translation tables
                  for the Magellan project.  Specified by the
                  SIZE.PDF file keyword OFORM='MAGELLAN'.
                  Code modifications are marked by '! AXW'.
 1 JAN 85 -  FFM   CONVERTED SUBROUTINE INTRP,INTRPV,IOUT,SHRINK,
                  SHRINZ FROM FORTRAN TO ASSEMBLY LANGUAGE
 4 SEP 84 -  SP    DELETED FORMAT PARAMETER (ALWAYS USING FORMAT 
                  FROM LABEL.)
 4 SEP 84 -  SP    ADDED STATEMENT TO CHANGE SZOOM=-1 TO SZOOM=1.
 4 SEP 84 -  SP    CONVERTED TO USE VICAR2 CALLS (XVREAD...)
 4 SEP 84 -  SP    DOUBLED BUFFER SIZES TO ALLOW LINES UP TO 20000.
11 JUL 84 -  HBD   FIX BUG IN SHRINK AND SHRINZ
18 NOV 83 -  HBD   DELETED STACKA AND RESTRUCTURED CODE
                  MADE VAX COMPATIBLE
.LEVEL1
.VARI INP
Input image.
.VARI OUT
Output image.
.VARI SIZE
Output picture size.
.VARI NL
Output number of lines
picture.
.VARI NS
Output number of samples
picture.
.VARIABLE BANDS
Standard Vicar Bands field:
  (SB,NB)
You can enter SB and NB together
as BANDS, OR enter the SB and NB
parameters separately.
By default, the entire input
image is used if these
.VARIABLE SB
Starting band number
.VARIABLE NB
Number of bands
.VARI OFORM
Output data format.
Valid keywords are BYTE, HALF,
FULL, or REAL.
.VARI AREA
Input image area to be SIZEd
.VARI IOFFSET
Offset output image:
  OFFSET=(SLO,SSO)
.VARI NOIN
Specifies no interpolation.
.VARI ZOOM
Specifies the ZOOM factor.
.VARI LZOOM
Vertical zoom factor.
.VARI SZOOM
Horizontal zoom factor.
.VARI SCALE
Scale applied to output values.
.VARI LIMITS
Lower and upper limits of DN
range.
.VARI DEBUG
Print status message.

.LEVEL2
.VARI INP
    Ex:  INP=IPIC
where IPIC is the input image file specification.  IPIC may be
in byte, halfword (16-bit integer), fullword (32-bit integer), or floating
point (REAL*4) data format.  IPIC may be up to 20,000 pixels in width (sample
size) and of arbitrary length (line or vertical dimension), and may be
located on disk or magnetic tape.
.VARI OUT
    Ex:  OUT=OPIC
where OPIC is the output image file specification.  The output data format is
specified by the OFORM parameter.  OPIC may be up to 20,000 pixels in width
(sample size) and of arbitrary length (line or vertical dimension), and must
be located on random-access disk storage.
.VARI SIZE
    SIZE=(1,1,NL,NS) 
where NL and NS specify the number of lines and samples in the output image.
If the SIZE parameter is ommitted, the output image size should be specified
via the ZOOM, LZOOM, and/or SZOOM parameters.

    Ex: Enlarging a 3x3 image to a 9x9 image can be achieved in the
        following ways:

		SIZE  A  B  SIZE=(1,1,9,9)
        or      SIZE  A  B  NL=9  NS=9
	or	SIZE  A  B  ZOOM=3
        or      SIZE  A  B  LZOOM=3  SZOOM=3
.VARI NL
    NL=n
where n is an integer specifying the number of lines in the output image.
The parameters NL and NS may be used instead of the SIZE parameter to specify
the size of the output image.
.VARI NS
    NS=n
where n is an integer specifying the number of samples in the output image.
The parameters NL and NS may be used instead of the SIZE parameter to specify
the size of the output image.
NOTE: The same restrictions that pertain to the SIZE parameter also pertain
to NL and NS.(See HELP SIZE)
.VARI OFORM
Keyword OFORM specifies the data format of the output image.  Valid values
are BYTE for byte output, HALF for halfword (16-bit integer) output, FULL
for fullword (32-bit integer) output, and REAL for 32-bit floating-point
output.  If defaulted, the output data format is set equal to the input
data format.
.VARI AREA
    AREA=(SL,SS,NL,NS)
where SL, SS, NL, and NS are all integers specifying the starting line,
starting sample, number of lines, and number of samples, respectively, of
the input area. The input area defaults to (1,1,NLI,NSI) where NLI and NSI
are the number of lines and number of samples in the input image.

.VARI IOFFSET

The IOFFSET parameter may be used to start the output image at a (line,sample)
coordinate other than (1,1).  Let the input image be:

                           1  2  3
                           2  3  4
                           3  4  5

then
		SIZE  INP  OUT  NL=8  NS=8  ZOOM=2  IOFFSET=(2,2)  'NOIN

will result in the following:

                    0  0  0  0  0  0  0  0
                    0  1  1  2  2  3  3  0
                    0  1  1  2  2  3  3  0
                    0  2  2  3  3  4  4  0
                    0  2  2  3  3  4  4  0
                    0  3  3  4  4  5  5  0
                    0  3  3  4  4  5  5  0
                    0  0  0  0  0  0  0  0

Generating blank margins may be useful in a mosaic or for writing an OVERLAY
grid.

.VARI NOIN
NOIN specifies that no interpolation is to be performed.  A nearest
neighbor is used to compute each output sample value. The default is to
perform interpolation.
.VARI ZOOM
    ZOOM=z
where z is an integer or floating point number specifying the output image
size as a ratio of the input image size (the zoom factor).  The image size
is scaled by the factor z in both line and sample dimensions.  Note that
the line and sample zoom factors may be specified independently via the
LZOOM and SZOOM parameters.  I.e. ZOOM=2 is equivalent to LZOOM=2 SZOOM=2.

The zoom factor is identical in function as in VIDS or IDX. If z is
positive, the input picture size is multiplied by z.  If z is negative,
the picture size is divided by -z.  Note that z=-2 is equivalent to z=0.5.
Specification of a zoom factor overrides the corresponding NL and/or NS
values in the VICAR SIZE field.  If a zoom factor is not specified the
output picture size defaults to the corresponding NL and/or NS value in
the SIZE field.
.VARI LZOOM
    LZOOM=z
where z is an integer or floating point number specifying the magnification
or reduction factor of the image in the line (vertical) dimension.  If z is
positive, the line dimension is multiplied by z.  If z is negative, the line
dimension is divided by -z.  Note that z=-2 is equivalent to z=0.5.

Specifying LZOOM overrides the corresponding NL value in the VICAR SIZE
field.  If LZOOM is not specified the output line dimension defaults to the
corresponding NL value in the SIZE field.
.VARI SZOOM
    SZOOM=z
where z is an integer or floating point number specifying the magnification
or reduction factor of the image in the sample (horizontal) dimension.  If z
is positive, the sample dimension is multiplied by z.  If z is negative, the
sample dimension is divided by -z.  Note that z=-2 is equivalent to z=0.5.

Specifying SZOOM overrides the corresponding NS value in the VICAR SIZE
field.  If SZOOM is not specified the output line dimension defaults to the
corresponding NS value in the SIZE field.
.VARI SCALE
    SCALE=s
where s is an integer or floating point number specfying an optional
scaling to be applied to the output DN values: OUTDN=s*OUTDN.  If SCALE
is not specified, no scaling is performed.
.VARI LIMITS
  LIMITS=(I1,I2)
where I1 and I2 are integers specifying the lower and upper limits of the
output DN range. All DN's outside this range are set equal to ILO and IHI.
The defaults are 0 and 255 for byte output, -32768 and 32767 for halfword
output, and -2147483648 and 2147483647 for fullword data.  This DN range
check is applied to floating point output.
.VARI DEBUG
Print the following message while the status returned from routine MP_LABLE_READ
is less than MP_SUCCESS: "error reading MP labels or MP labels do not exist"
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstsize.pdf
procedure
!Test file for program SIZE
local   afidsroot   type=string count=1

  refgbl $echo
  refgbl $syschar

! Jun 25, 2012 - RJB
! TEST SCRIPT FOR SIZE
! tests BYTE, HALF, FULL, REAL images
!
! Vicar Programs:
!       gen list difpic hist label-list 
!       stretch concat
! 
! parameters:
!   <none>
!
! Requires external test data: 
!   cartlab or mipl dependent pointers

body
  let _onfail="goto rm"
  let $echo="no"

!check to see if mipl or cartlab for certain programs
!cartlab defines env var $AFIDS_ROOT, mipl doesm't
translog INP=AFIDS_ROOT TRANS=afidsroot

if (afidsroot = "")
!MIPL
    ush ln -s /project/test_work/testdata/mipl/vgr vt
else
!CARTLAB
    ush ln -s /raid1/vicar_test_images/testdata/mipl/vgr vt
end-if

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Interpolation mode
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
write "!!!!!!!!!!!!!!!!!!!!"
write "! Interpolation mode"
write "!!!!!!!!!!!!!!!!!!!!"
let $echo="yes"
!....First, test all options on a byte image
gen a 3 3 ival=2 sinc=3 linc=3
list a

!....3x3 magnification, three different ways
size a b nl=9 ns=9	!Use size field to specify magnification
list b
size a c zoom=3		!Use ZOOM parameter to specify magnification
difpic (b,c)		!Results should be identical
size a c lzoom=3 szoom=3
difpic (b,c)		!Results should be identical

!....3x3 image compression, three different ways
size b c nl=3 ns=3		!Zoom factor specified by size field
list c
size b d zoom=-3		!Zoom factor specified by ZOOM parameter
difpic (c,d)
size b d lzoom=-3 szoom=-3	!Zoom factor specified by LZOOM,SZOOM
difpic (c,d)

size b c zoom=-3	!Integral compression
list b
size b c zoom=-2.5	!Non-integral compression
list b

size a b (1,1,11,11) ioffset=(2,2) zoom=3	!IOFFSET parameter
list b 'zeroes

size a b zoom=3 limits=(1,14)			!LIMITS parameter
list b

!...Special cases
size a b zoom=3	area=(1,1,1,3)		!nli=1
list b
size b c lzoom=1 szoom=-3 area=(1,1,1,9)
list c

size a b zoom=3	area=(1,1,3,1)		!nsi=1
list b
size b c lzoom=-3 szoom=1 area=(1,1,9,1)
list c

size a b lzoom=3 szoom=1		!SZOOM=1
list b
size b c lzoom=-3 szoom=1
difpic (a,c)

size a b lzoom=1 szoom=3		!LZOOM=1
list b
size b c lzoom=1 szoom=-3
difpic (a,c)

!...Mixed magnifications/compressions
size b c lzoom=3 szoom=-3		!vertical mag, horizontal compression
list c
size c d lzoom=-3 szoom=3		!horizontal mag, vertical compression
difpic (b,d)				!no differences

!...Mixed data modes, outputs greater than inputs
size a b zoom=3 'half scale=100 	!byte to halfword
list b
size b c zoom=-3 'full scale=1000	!half to full
list c
size b d zoom=-3 'real scale=1000	!half to real
list d

!...Mixed data modes, outputs smaller than inputs
size d e zoom=3 'byte scale=0.00001	!real to byte
list e
size c e zoom=3 'half scale=0.001	!full to half
list e
size b e zoom=-3 'byte scale=0.01	!half to byte
list e

!...Half word extract smaller image - Interpolation
gen     out=d nl=3375 ns=3648 ival=-32768 linc=10 sinc=10 'half
hist    d
size    d e size=(1,1,337,364)
hist    e

size    d f size=(300,200,200,200)
hist    f

!...3D file
gen a 3 3 3 ival=2 sinc=3 linc=3
size a b nb=2 zoom=2
list b
size a b zoom=2
list b nb=1 sb=3

!... increased line size
gen a 2 50000
size a b zoom=2
list b ss=99991 ns=10
let $echo="no"
write "!!!!!!!!!!!!!!!!!!!!!!!!!"
write "! Non-interpolation mode"
write "!!!!!!!!!!!!!!!!!!!!!!!!!"
let $echo="yes"
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Non-interpolation mode
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
gen a 3 3 ival=2 sinc=3 linc=3

!....First, test all options on a byte image
size a b zoom=3 'noin		!Zoom up
list b
size b c zoom=-3 'noin		!Zoom down
difpic (a,c)

size a c zoom=2.5 'noin		!Non-integral magnification
list c
size b c zoom=-2.5 'noin	!Non-integral compression
list c

size a b (1,1,11,11) ioffset=(2,2) zoom=3 'noin	!IOFFSET parameter
list b 'zeroes

size a b zoom=3 limits=(1,14) 'noin		!LIMITS parameter
list b

!...Mixed magnifications/compressions
size a b lzoom=1 szoom=3 'noin
size b c lzoom=3 szoom=-3 'noin		!vertical mag, horizontal compression
list c
size c d lzoom=-3 szoom=3 'noin		!horizontal mag, vertical compression
difpic (b,d)				!no differences

!...Mixed data modes, outputs greater than inputs
size a b zoom=3 'half scale=100 'noin 		!byte to halfword
list b
size b c zoom=-3 'full scale=1000 'noin		!half to full
list c
size b d zoom=-3 'real scale=1000 'noin		!half to real
list d

!...Mixed data modes, outputs smaller than inputs
size d e zoom=3 'byte scale=0.00001 'noin	!real to byte
list e
size c e zoom=3 'half scale=0.001 'noin		!full to half
list e
size b e zoom=-3 'byte scale=0.01 'noin		!half to byte
list e

!...Half word extract smaller image - No interpolation
gen     out=g nl=3375 ns=3648 ival=-32768 linc=10 sinc=10 'half
hist    g
size    g h size=(1,1,337,364) 'noin
hist    h

size    g h size=(300,200,200,200) 'noin
hist    h

!...3D file
gen a 3 3 3 ival=2 sinc=3 linc=3
size a b nb=2 zoom=2 'noin
list b
size a b zoom=2 'noin
list b nb=1 sb=3

!... increased line size
gen a 2 50000
size a b zoom=2 'noin
list b ss=99991 ns=10
let $echo="no"
write "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
write "****Test for fail on size 'noin ****"
write "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
let $echo="yes"
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!            ****Test for fail on size 'noin ****                  !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
gen x 10 10                     !Create a 10x10 byte image
list x
stretch x y func="in1+18"       !Create image with bimodal histogram
list y
concat (x,y) z ns=20 'nost
list z sl=1 ss=1 nl=10 ns=10
let $echo="no"
let _onfail="continue"
write " ==============================================  should FAIL here ==========="
let $echo="yes"
size z w zoom=10 'noin
let $echo="no"
let _onfail="goto rm"
write "================================================= NO FAIL here =============="
let $echo="yes"
size w xx zoom=10
list w sl=1 ss=1 nl=10 ns=10
list xx sl=1 ss=1 nl=10 ns=10

let $echo="no"
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Test updating of map projection labels
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
write "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
write "! Test updating of map projection labels"
write "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
let $echo="yes"

!....Test on old map projection labels
label-list vt/m.dat 
size vt/m.dat a zoom=-2 area=(2,2,400,400)
label-list a

!...Repeat map label test with non-integral zoom:
size vt/m.dat a zoom=-1.5 area=(2,2,400,400)
label-list a

!...Test on new map projection labels
size vt/tst1.dat a zoom=20 ioffset=(81,81)	!normal cylindrical projection
label-list a

size vt/tst10.dat a zoom=20 ioffset=(81,81)	!point perspective projection
label-list a

rm>
  ush rm -f vt
  ush rm -f a
  ush rm -f b
  ush rm -f c
  ush rm -f d
  ush rm -f e
  ush rm -f f
  ush rm -f g
  ush rm -f h
  ush rm -f x
  ush rm -f y
  ush rm -f w
  ush rm -f z
  ush rm -f xx
let $echo="no"
end-proc
$!-----------------------------------------------------------------------------
$ create tstsize.log_solos
tstsize
!!!!!!!!!!!!!!!!!!!!
! Interpolation mode
!!!!!!!!!!!!!!!!!!!!
gen a 3 3 ival=2 sinc=3 linc=3
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:32:31 2012
     Samp     1       3
   Line
      1       2   5   8
      2       5   8  11
      3       8  11  14
size a b nl=9 ns=9
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      9 X      9
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZE task completed
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:32:31 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:32:31 2012
     Samp     1       3       5       7       9
   Line
      1       0   1   2   3   4   5   6   7   8
      2       1   2   3   4   5   6   7   8   9
      3       2   3   4   5   6   7   8   9  10
      4       3   4   5   6   7   8   9  10  11
      5       4   5   6   7   8   9  10  11  12
      6       5   6   7   8   9  10  11  12  13
      7       6   7   8   9  10  11  12  13  14
      8       7   8   9  10  11  12  13  14  15
      9       8   9  10  11  12  13  14  15  16
size a c zoom=3
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      9 X      9
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZE task completed
difpic (b,c)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
size a c lzoom=3 szoom=3
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      9 X      9
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZE task completed
difpic (b,c)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
size b c nl=3 ns=3
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    9,    9)
     OUTPUT SIZE=      3 X      3
 PICTURE SIZE SCALED BY      0.33333*NL,      0.33333*NS
 SIZE task completed
list c
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:32:31 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:32:34 2012
     Samp     1       3
   Line
      1       2   5   8
      2       5   8  11
      3       8  11  14
size b d zoom=-3
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    9,    9)
     OUTPUT SIZE=      3 X      3
 PICTURE SIZE SCALED BY      0.33333*NL,      0.33333*NS
 SIZE task completed
difpic (c,d)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
size b d lzoom=-3 szoom=-3
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    9,    9)
     OUTPUT SIZE=      3 X      3
 PICTURE SIZE SCALED BY      0.33333*NL,      0.33333*NS
 SIZE task completed
difpic (c,d)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
size b c zoom=-3
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    9,    9)
     OUTPUT SIZE=      3 X      3
 PICTURE SIZE SCALED BY      0.33333*NL,      0.33333*NS
 SIZE task completed
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:32:31 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:32:31 2012
     Samp     1       3       5       7       9
   Line
      1       0   1   2   3   4   5   6   7   8
      2       1   2   3   4   5   6   7   8   9
      3       2   3   4   5   6   7   8   9  10
      4       3   4   5   6   7   8   9  10  11
      5       4   5   6   7   8   9  10  11  12
      6       5   6   7   8   9  10  11  12  13
      7       6   7   8   9  10  11  12  13  14
      8       7   8   9  10  11  12  13  14  15
      9       8   9  10  11  12  13  14  15  16
size b c zoom=-2.5
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    9,    9)
     OUTPUT SIZE=      3 X      3
 PICTURE SIZE SCALED BY      0.40000*NL,      0.40000*NS
 SIZE task completed
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:32:31 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:32:31 2012
     Samp     1       3       5       7       9
   Line
      1       0   1   2   3   4   5   6   7   8
      2       1   2   3   4   5   6   7   8   9
      3       2   3   4   5   6   7   8   9  10
      4       3   4   5   6   7   8   9  10  11
      5       4   5   6   7   8   9  10  11  12
      6       5   6   7   8   9  10  11  12  13
      7       6   7   8   9  10  11  12  13  14
      8       7   8   9  10  11  12  13  14  15
      9       8   9  10  11  12  13  14  15  16
size a b (1,1,11,11) ioffset=(2,2) zoom=3
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=     11 X     11
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZE task completed
list b 'zeroes
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:32:31 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:32:38 2012
     Samp     1       3       5       7       9      11
   Line
      1       0   0   0   0   0   0   0   0   0   0   0
      2       0   0   1   2   3   4   5   6   7   8   0
      3       0   1   2   3   4   5   6   7   8   9   0
      4       0   2   3   4   5   6   7   8   9  10   0
      5       0   3   4   5   6   7   8   9  10  11   0
      6       0   4   5   6   7   8   9  10  11  12   0
      7       0   5   6   7   8   9  10  11  12  13   0
      8       0   6   7   8   9  10  11  12  13  14   0
      9       0   7   8   9  10  11  12  13  14  15   0
     10       0   8   9  10  11  12  13  14  15  16   0
     11       0   0   0   0   0   0   0   0   0   0   0
size a b zoom=3 limits=(1,14)
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      9 X      9
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZE task completed
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:32:31 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:32:38 2012
     Samp     1       3       5       7       9
   Line
      1       1   1   2   3   4   5   6   7   8
      2       1   2   3   4   5   6   7   8   9
      3       2   3   4   5   6   7   8   9  10
      4       3   4   5   6   7   8   9  10  11
      5       4   5   6   7   8   9  10  11  12
      6       5   6   7   8   9  10  11  12  13
      7       6   7   8   9  10  11  12  13  14
      8       7   8   9  10  11  12  13  14  14
      9       8   9  10  11  12  13  14  14  14
size a b zoom=3	area=(1,1,1,3)
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    1,    3)
     OUTPUT SIZE=      3 X      9
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZE task completed
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:32:31 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:32:39 2012
     Samp     1       3       5       7       9
   Line
      1       1   2   3   4   5   6   7   8   9
      2       1   2   3   4   5   6   7   8   9
      3       1   2   3   4   5   6   7   8   9
size b c lzoom=1 szoom=-3 area=(1,1,1,9)
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    1,    9)
     OUTPUT SIZE=      1 X      3
 PICTURE SIZE SCALED BY      1*NL,     -3*NS
 SIZE task completed
list c
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:32:31 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:32:40 2012
     Samp     1       3
   Line
      1       2   5   8
size a b zoom=3	area=(1,1,3,1)
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    1)
     OUTPUT SIZE=      9 X      3
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZE task completed
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:32:31 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:32:40 2012
     Samp     1       3
   Line
      1       1   1   1
      2       2   2   2
      3       3   3   3
      4       4   4   4
      5       5   5   5
      6       6   6   6
      7       7   7   7
      8       8   8   8
      9       9   9   9
size b c lzoom=-3 szoom=1 area=(1,1,9,1)
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    9,    1)
     OUTPUT SIZE=      3 X      1
 PICTURE SIZE SCALED BY     -3*NL,      1*NS
 SIZE task completed
list c
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:32:31 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:32:41 2012
     Samp     1
   Line
      1       2
      2       5
      3       8
size a b lzoom=3 szoom=1
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      9 X      3
 PICTURE SIZE SCALED BY      3*NL,      1*NS
 SIZE task completed
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:32:31 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:32:42 2012
     Samp     1       3
   Line
      1       1   4   7
      2       2   5   8
      3       3   6   9
      4       4   7  10
      5       5   8  11
      6       6   9  12
      7       7  10  13
      8       8  11  14
      9       9  12  15
size b c lzoom=-3 szoom=1
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    9,    3)
     OUTPUT SIZE=      3 X      3
 PICTURE SIZE SCALED BY     -3*NL,      1*NS
 SIZE task completed
difpic (a,c)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
size a b lzoom=1 szoom=3
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      3 X      9
 PICTURE SIZE SCALED BY      1*NL,      3*NS
 SIZE task completed
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:32:31 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:32:43 2012
     Samp     1       3       5       7       9
   Line
      1       1   2   3   4   5   6   7   8   9
      2       4   5   6   7   8   9  10  11  12
      3       7   8   9  10  11  12  13  14  15
size b c lzoom=1 szoom=-3
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    9)
     OUTPUT SIZE=      3 X      3
 PICTURE SIZE SCALED BY      1*NL,     -3*NS
 SIZE task completed
difpic (a,c)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
size b c lzoom=3 szoom=-3
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    9)
     OUTPUT SIZE=      9 X      3
 PICTURE SIZE SCALED BY      3*NL,     -3*NS
 SIZE task completed
list c
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:32:31 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:32:45 2012
     Samp     1       3
   Line
      1       1   4   7
      2       2   5   8
      3       3   6   9
      4       4   7  10
      5       5   8  11
      6       6   9  12
      7       7  10  13
      8       8  11  14
      9       9  12  15
size c d lzoom=-3 szoom=3
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    9,    3)
     OUTPUT SIZE=      3 X      9
 PICTURE SIZE SCALED BY     -3*NL,      3*NS
 SIZE task completed
difpic (b,d)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
size a b zoom=3 'half scale=100
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      9 X      9
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZE task completed
list b
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:32:31 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:32:46 2012
     Samp       1     2     3     4     5     6     7     8     9
   Line
      1         0   100   200   300   400   500   600   700   800
      2       100   200   300   400   500   600   700   800   900
      3       200   300   400   500   600   700   800   900  1000
      4       300   400   500   600   700   800   900  1000  1100
      5       400   500   600   700   800   900  1000  1100  1200
      6       500   600   700   800   900  1000  1100  1200  1300
      7       600   700   800   900  1000  1100  1200  1300  1400
      8       700   800   900  1000  1100  1200  1300  1400  1500
      9       800   900  1000  1100  1200  1300  1400  1500  1600
size b c zoom=-3 'full scale=1000
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    9,    9)
     OUTPUT SIZE=      3 X      3
 PICTURE SIZE SCALED BY      0.33333*NL,      0.33333*NS
 SIZE task completed
list c
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:32:31 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:32:47 2012
     Samp            1          2          3
   Line
      1         200000     500000     800000
      2         500000     800000    1100000
      3         800000    1100000    1400000
size b d zoom=-3 'real scale=1000
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    9,    9)
     OUTPUT SIZE=      3 X      3
 PICTURE SIZE SCALED BY      0.33333*NL,      0.33333*NS
 SIZE task completed
list d
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:32:31 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:32:47 2012
     Samp             1           2           3
   Line
      1       2.000E+05   5.000E+05   8.000E+05
      2       5.000E+05   8.000E+05   1.100E+06
      3       8.000E+05   1.100E+06   1.400E+06
size d e zoom=3 'byte scale=0.00001
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      9 X      9
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZE task completed
list e
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:32:31 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:32:48 2012
     Samp     1       3       5       7       9
   Line
      1       0   1   2   3   4   5   6   7   8
      2       1   2   3   4   5   6   7   8   9
      3       2   3   4   5   6   7   8   9  10
      4       3   4   5   6   7   8   9  10  11
      5       4   5   6   7   8   9  10  11  12
      6       5   6   7   8   9  10  11  12  13
      7       6   7   8   9  10  11  12  13  14
      8       7   8   9  10  11  12  13  14  15
      9       8   9  10  11  12  13  14  15  16
size c e zoom=3 'half scale=0.001
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      9 X      9
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZE task completed
list e
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:32:31 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:32:49 2012
     Samp       1     2     3     4     5     6     7     8     9
   Line
      1         0   100   200   300   400   500   600   700   800
      2       100   200   300   400   500   600   700   800   900
      3       200   300   400   500   600   700   800   900  1000
      4       300   400   500   600   700   800   900  1000  1100
      5       400   500   600   700   800   900  1000  1100  1200
      6       500   600   700   800   900  1000  1100  1200  1300
      7       600   700   800   900  1000  1100  1200  1300  1400
      8       700   800   900  1000  1100  1200  1300  1400  1500
      9       800   900  1000  1100  1200  1300  1400  1500  1600
size b e zoom=-3 'byte scale=0.01
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    9,    9)
     OUTPUT SIZE=      3 X      3
 PICTURE SIZE SCALED BY      0.33333*NL,      0.33333*NS
 SIZE task completed
list e
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:32:31 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:32:49 2012
     Samp     1       3
   Line
      1       2   5   8
      2       5   8  11
      3       8  11  14
gen     out=d nl=3375 ns=3648 ival=-32768 linc=10 sinc=10 'half
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist    d
Beginning VICAR task hist
*** HIST version 13 Jul 2012 ***

Bin Width =      256.0
     -32768     6097   ***
     -32512    11764   ******
     -32256    12155   ******
     -32000    12194   ******
     -31744    11841   ******
     -31488    12078   ******
     -31232    11892   ******
     -30976    12027   ******
     -30720    12194   ******
     -30464    11969   ******
     -30208    11950   ******
     -29952    12020   ******
     -29696    11899   ******
     -29440    12194   ******
     -29184    12097   ******
     -28928    11822   ******
     -28672    12148   ******
     -28416    11771   ******
     -28160    12209   ******
     -27904    12675   *******
     -27648    12825   *******
     -27392    14001   *******
     -27136    14100   ********
     -26880    15327   ********
     -26624    16003   *********
     -26368    16025   *********
     -26112    17329   *********
     -25856    17300   *********
     -25600    18655   **********
     -25344    19331   ***********
     -25088    19225   **********
     -24832    20657   ***********
     -24576    20500   ***********
     -24320    21983   ************
     -24064    22659   ************
     -23808    22425   ************
     -23552    23985   *************
     -23296    23700   *************
     -23040    25311   **************
     -22784    25987   **************
     -22528    25625   **************
     -22272    27313   ***************
     -22016    26900   ***************
     -21760    28639   ****************
     -21504    29315   ****************
     -21248    28825   ****************
     -20992    30641   *****************
     -20736    30100   *****************
     -20480    31967   ******************
     -20224    32643   ******************
     -19968    32025   ******************
     -19712    33969   *******************
     -19456    33300   ******************
     -19200    35295   ********************
     -18944    35971   ********************
     -18688    35225   ********************
     -18432    37297   *********************
     -18176    36500   ********************
     -17920    38623   **********************
     -17664    39299   **********************
     -17408    38425   *********************
     -17152    40625   ***********************
     -16896    39700   **********************
     -16640    41951   ***********************
     -16384    42627   ************************
     -16128    41625   ***********************
     -15872    43953   *************************
     -15616    42900   ************************
     -15360    45279   *************************
     -15104    45955   **************************
     -14848    44825   *************************
     -14592    47281   **************************
     -14336    46100   **************************
     -14080    48607   ***************************
     -13824    49283   ****************************
     -13568    48025   ***************************
     -13312    50609   ****************************
     -13056    49300   ****************************
     -12800    51935   *****************************
     -12544    52611   *****************************
     -12288    51225   *****************************
     -12032    53937   ******************************
     -11776    52500   *****************************
     -11520    55263   *******************************
     -11264    55939   *******************************
     -11008    54425   *******************************
     -10752    57265   ********************************
     -10496    55700   *******************************
     -10240    58591   *********************************
      -9984    59267   *********************************
      -9728    57625   ********************************
      -9472    60593   **********************************
      -9216    58900   *********************************
      -8960    61919   ***********************************
      -8704    62595   ***********************************
      -8448    60825   **********************************
      -8192    63921   ************************************
      -7936    62100   ***********************************
      -7680    65247   *************************************
      -7424    65923   *************************************
      -7168    64025   ************************************
      -6912    67249   **************************************
      -6656    65300   *************************************
      -6400    68575   ***************************************
      -6144    69251   ***************************************
      -5888    67225   **************************************
      -5632    70577   ****************************************
      -5376    68500   ***************************************
      -5120    71903   ****************************************
      -4864    72579   *****************************************
      -4608    70425   ****************************************
      -4352    73905   ******************************************
      -4096    71700   ****************************************
      -3840    75231   ******************************************
      -3584    75907   *******************************************
      -3328    73625   *****************************************
      -3072    77233   ********************************************
      -2816    74900   ******************************************
      -2560    78559   ********************************************
      -2304    79235   *********************************************
      -2048    76825   *******************************************
      -1792    80561   *********************************************
      -1536    78100   ********************************************
      -1280    81887   **********************************************
      -1024    82563   ***********************************************
       -768    80025   *********************************************
       -512    83889   ***********************************************
       -256    81300   **********************************************
          0    85215   ************************************************
        256    85891   ************************************************
        512    83225   ***********************************************
        768    87217   *************************************************
       1024    84347   ************************************************
       1280    87750   **************************************************  1
       1536    87750   **************************************************  2
       1792    84375   ************************************************
       2048    87750   **************************************************
       2304    84375   ************************************************
       2560    87750   **************************************************
       2816    87750   **************************************************
       3072    84375   ************************************************
       3328    87750   **************************************************
       3584    84375   ************************************************
       3840    87399   *************************************************
       4096    86723   *************************************************
       4352    82750   ***********************************************
       4608    85397   ************************************************
       4864    81475   **********************************************
       5120    84071   ***********************************************
       5376    83395   ***********************************************
       5632    79550   *********************************************
       5888    82069   **********************************************
       6144    78275   ********************************************
       6400    80743   **********************************************
       6656    80067   *********************************************
       6912    76350   *******************************************
       7168    78741   ********************************************
       7424    75075   ******************************************
       7680    77415   ********************************************
       7936    76739   *******************************************
       8192    73150   *****************************************
       8448    75413   ******************************************
       8704    71875   ****************************************
       8960    74087   ******************************************
       9216    73411   *****************************************
       9472    69950   ***************************************
       9728    72085   *****************************************
       9984    68675   ***************************************
      10240    70759   ****************************************
      10496    70083   ***************************************
      10752    66750   **************************************
      11008    68757   ***************************************
      11264    65475   *************************************
      11520    67431   **************************************
      11776    66755   **************************************
      12032    63550   ************************************
      12288    65429   *************************************
      12544    62275   ***********************************
      12800    64103   ************************************
      13056    63427   ************************************
      13312    60350   **********************************
      13568    62101   ***********************************
      13824    59075   *********************************
      14080    60775   **********************************
      14336    60099   **********************************
      14592    57150   ********************************
      14848    58773   *********************************
      15104    55875   *******************************
      15360    57447   ********************************
      15616    56771   ********************************
      15872    53950   ******************************
      16128    55445   *******************************
      16384    52675   ******************************
      16640    54119   ******************************
      16896    53443   ******************************
      17152    50750   ****************************
      17408    52117   *****************************
      17664    49475   ****************************
      17920    50791   ****************************
      18176    50115   ****************************
      18432    47550   ***************************
      18688    48789   ***************************
      18944    46275   **************************
      19200    47463   ***************************
      19456    46787   **************************
      19712    44350   *************************
      19968    45461   *************************
      20224    43075   ************************
      20480    44135   *************************
      20736    43459   ************************
      20992    41150   ***********************
      21248    42133   ************************
      21504    39875   **********************
      21760    40807   ***********************
      22016    40131   **********************
      22272    37950   *********************
      22528    38805   **********************
      22784    36675   ********************
      23040    37479   *********************
      23296    36803   ********************
      23552    34750   *******************
      23808    35477   ********************
      24064    33475   *******************
      24320    34151   *******************
      24576    33475   *******************
      24832    31550   *****************
      25088    32149   ******************
      25344    30275   *****************
      25600    30823   *****************
      25856    30147   *****************
      26112    28350   ****************
      26368    28821   ****************
      26624    27075   ***************
      26880    27495   ***************
      27136    26819   ***************
      27392    25150   **************
      27648    25493   **************
      27904    23875   *************
      28160    24167   *************
      28416    23491   *************
      28672    21950   ************
      28928    22165   ************
      29184    20675   ***********
      29440    20839   ***********
      29696    20163   ***********
      29952    18750   **********
      30208    18837   **********
      30464    17475   *********
      30720    17511   *********
      30976    16835   *********
      31232    15550   ********
      31488    15509   ********
      31744    14275   ********
      32000    14183   ********
      32256    13507   *******
      32512    12350   *******
      32768     6175   ***

AVERAGE GRAY LEVEL=1752.829
STANDARD DEVIATION=14366.17
NUMBER ELEMENTS=  1231200
MIN. DN=    -32768
MAX. DN=     32762

size    d e size=(1,1,337,364)
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1, 3375, 3648)
     OUTPUT SIZE=    337 X    364
 PICTURE SIZE SCALED BY      0.09985*NL,      0.09978*NS
 SIZE task completed
hist    e
Beginning VICAR task hist
*** HIST version 13 Jul 2012 ***

Bin Width =      256.0
     -32678       48   **
     -32422       94   ****
     -32166      141   ******
     -31910      131   ******
     -31654      104   *****
     -31398      141   ******
     -31142      100   ****
     -30886      162   ********
     -30630      113   *****
     -30374      141   ******
     -30118       94   ****
     -29862      141   ******
     -29606      108   *****
     -29350      127   ******
     -29094      141   ******
     -28838       94   ****
     -28582      141   ******
     -28326       94   ****
     -28070      142   *******
     -27814       99   ****
     -27558      156   *******
     -27302      109   *****
     -27046      171   ********
     -26790      180   ********
     -26534      125   ******
     -26278      195   *********
     -26022      135   ******
     -25766      210   **********
     -25510      145   *******
     -25254      225   ***********
     -24998      155   *******
     -24742      240   ***********
     -24486      249   ************
     -24230      171   ********
     -23974      264   *************
     -23718      181   ********
     -23462      279   *************
     -23206      191   *********
     -22950      294   **************
     -22694      201   *********
     -22438      309   ***************
     -22182      317   ***************
     -21926      218   **********
     -21670      333   ****************
     -21414      227   ***********
     -21158      348   *****************
     -20902      237   ***********
     -20646      363   *****************
     -20390      247   ************
     -20134      378   ******************
     -19878      373   ******************
     -19622      277   *************
     -19366      402   *******************
     -19110      273   *************
     -18854      417   ********************
     -18598      283   *************
     -18342      432   *********************
     -18086      293   **************
     -17830      447   **********************
     -17574      426   *********************
     -17318      339   ****************
     -17062      471   ***********************
     -16806      319   ***************
     -16550      486   ************************
     -16294      329   ****************
     -16038      501   ************************
     -15782      339   ****************
     -15526      516   *************************
     -15270      480   ***********************
     -15014      400   *******************
     -14758      540   **************************
     -14502      365   ******************
     -14246      555   ***************************
     -13990      375   ******************
     -13734      570   ****************************
     -13478      385   *******************
     -13222      585   ****************************
     -12966      534   **************************
     -12710      461   **********************
     -12454      609   ******************************
     -12198      411   ********************
     -11942      624   ******************************
     -11686      421   ********************
     -11430      639   *******************************
     -11174      431   *********************
     -10918      654   ********************************
     -10662      589   *****************************
     -10406      521   *************************
     -10150      678   *********************************
      -9894      457   **********************
      -9638      693   **********************************
      -9382      467   ***********************
      -9126      708   ***********************************
      -8870      477   ***********************
      -8614      723   ***********************************
      -8358      643   *******************************
      -8102      582   ****************************
      -7846      747   ************************************
      -7590      503   ************************
      -7334      762   *************************************
      -7078      513   *************************
      -6822      777   **************************************
      -6566      530   **************************
      -6310      785   **************************************
      -6054      697   **********************************
      -5798      643   *******************************
      -5542      816   ****************************************
      -5286      549   ***************************
      -5030      831   *****************************************
      -4774      559   ***************************
      -4518      846   *****************************************
      -4262      583   ****************************
      -4006      847   *****************************************
      -3750      751   *************************************
      -3494      704   **********************************
      -3238      885   *******************************************
      -2982      595   *****************************
      -2726      900   ********************************************
      -2470      605   *****************************
      -2214      915   *********************************************
      -1958      638   *******************************
      -1702      907   ********************************************
      -1446      806   ***************************************
      -1190      764   *************************************
       -934      954   ***********************************************
       -678      641   *******************************
       -422      969   ***********************************************
       -166      651   ********************************
         90      984   ************************************************
        346      692   **********************************
        602      968   ***********************************************
        858      860   ******************************************
       1114      822   ****************************************
       1370     1011   **************************************************  1
       1626      674   *********************************
       1882     1011   **************************************************  2
       2138      674   *********************************
       2394     1011   **************************************************
       2650      696   **********************************
       2906      989   ************************************************
       3162      848   *****************************************
       3418      837   *****************************************
       3674     1001   *************************************************
       3930      678   *********************************
       4186      996   *************************************************
       4442      659   ********************************
       4698      981   ************************************************
       4954      655   ********************************
       5210      960   ***********************************************
       5466      798   ***************************************
       5722      792   ***************************************
       5978      940   **********************************************
       6234      625   ******************************
       6490      927   *********************************************
       6746      613   ******************************
       7002      912   *********************************************
       7258      603   *****************************
       7514      897   ********************************************
       7770      738   ************************************
       8026      737   ************************************
       8282      873   *******************************************
       8538      577   ****************************
       8794      858   ******************************************
       9050      567   ****************************
       9306      843   *****************************************
       9562      557   ***************************
       9818      828   ****************************************
      10074      677   *********************************
      10330      683   *********************************
      10586      804   ***************************************
      10842      531   **************************
      11098      789   ***************************************
      11354      521   *************************
      11610      774   **************************************
      11866      511   *************************
      12122      759   *************************************
      12378      616   ******************************
      12634      629   *******************************
      12890      743   ************************************
      13146      493   ************************
      13402      727   ***********************************
      13658      482   ***********************
      13914      711   ***********************************
      14170      471   ***********************
      14426      695   **********************************
      14682      555   ***************************
      14938      575   ****************************
      15194      666   ********************************
      15450      439   *********************
      15706      651   ********************************
      15962      429   *********************
      16218      636   *******************************
      16474      419   ********************
      16730      621   ******************************
      16986      494   ************************
      17242      521   *************************
      17498      597   *****************************
      17754      393   *******************
      18010      582   ****************************
      18266      383   ******************
      18522      567   ****************************
      18778      373   ******************
      19034      552   ***************************
      19290      435   *********************
      19546      465   **********************
      19802      528   **************************
      20058      347   *****************
      20314      513   *************************
      20570      337   ****************
      20826      498   ************************
      21082      327   ****************
      21338      483   ***********************
      21594      374   ******************
      21850      411   ********************
      22106      459   **********************
      22362      301   **************
      22618      444   *********************
      22874      291   **************
      23130      429   *********************
      23386      281   *************
      23642      414   ********************
      23898      312   ***************
      24154      358   *****************
      24410      390   *******************
      24666      255   ************
      24922      375   ******************
      25178      245   ************
      25434      360   *****************
      25690      235   ***********
      25946      345   *****************
      26202      251   ************
      26458      304   ***************
      26714      321   ***************
      26970      209   **********
      27226      306   ***************
      27482      199   *********
      27738      291   **************
      27994      189   *********
      28250      276   *************
      28506      190   *********
      28762      250   ************
      29018      252   ************
      29274      163   ********
      29530      237   ***********
      29786      153   *******
      30042      222   **********
      30298      143   *******
      30554      207   **********
      30810      135   ******
      31066      190   *********
      31322      183   *********
      31578      117   *****
      31834      168   ********
      32090      107   *****
      32346      153   *******
      32602       97   ****

AVERAGE GRAY LEVEL=1752.829
STANDARD DEVIATION=14352.74
NUMBER ELEMENTS=    12266
MIN. DN=    -32678
MAX. DN=     32646

size    d f size=(300,200,200,200)
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1, 3375, 3648)
     OUTPUT SIZE=    200 X    200
 PICTURE SIZE SCALED BY      0.05926*NL,      0.05482*NS
 SIZE task completed
hist    f
Beginning VICAR task hist
*** HIST version 13 Jul 2012 ***

Bin Width =      256.0
     -32602       20   ***
     -32346       41   *******
     -32090       40   *******
     -31834       38   ******
     -31578       41   *******
     -31322       39   ******
     -31066       41   *******
     -30810       37   ******
     -30554       41   *******
     -30298       39   ******
     -30042       38   ******
     -29786       42   *******
     -29530       37   ******
     -29274       41   *******
     -29018       38   ******
     -28762       39   ******
     -28506       41   *******
     -28250       39   ******
     -27994       40   *******
     -27738       43   *******
     -27482       43   *******
     -27226       47   ********
     -26970       48   ********
     -26714       52   *********
     -26458       52   *********
     -26202       55   *********
     -25946       57   **********
     -25690       59   **********
     -25434       62   **********
     -25178       63   ***********
     -24922       66   ***********
     -24666       67   ***********
     -24410       69   ************
     -24154       74   *************
     -23898       73   ************
     -23642       75   *************
     -23386       79   **************
     -23130       80   **************
     -22874       86   ***************
     -22618       81   **************
     -22362       88   ***************
     -22106       87   ***************
     -21850       92   ****************
     -21594       93   ****************
     -21338       97   *****************
     -21082       97   *****************
     -20826      100   *****************
     -20570      100   *****************
     -20314      107   ******************
     -20058      103   ******************
     -19802      110   *******************
     -19546      109   *******************
     -19290      112   *******************
     -19034      115   ********************
     -18778      116   ********************
     -18522      119   *********************
     -18266      121   *********************
     -18010      123   *********************
     -17754      126   **********************
     -17498      126   **********************
     -17242      130   ***********************
     -16986      132   ***********************
     -16730      133   ***********************
     -16474      137   ************************
     -16218      135   ***********************
     -15962      142   *************************
     -15706      140   ************************
     -15450      146   *************************
     -15194      146   *************************
     -14938      148   **************************
     -14682      151   **************************
     -14426      153   ***************************
     -14170      153   ***************************
     -13914      160   ****************************
     -13658      157   ***************************
     -13402      163   ****************************
     -13146      163   ****************************
     -12890      166   *****************************
     -12634      167   *****************************
     -12378      170   ******************************
     -12122      172   ******************************
     -11866      173   ******************************
     -11610      177   *******************************
     -11354      179   *******************************
     -11098      179   *******************************
     -10842      183   ********************************
     -10586      185   ********************************
     -10330      187   *********************************
     -10074      189   *********************************
      -9818      190   *********************************
      -9562      195   **********************************
      -9306      193   **********************************
      -9050      200   ***********************************
      -8794      198   ***********************************
      -8538      202   ***********************************
      -8282      205   ************************************
      -8026      205   ************************************
      -7770      207   ************************************
      -7514      212   *************************************
      -7258      210   *************************************
      -7002      217   **************************************
      -6746      214   *************************************
      -6490      221   ***************************************
      -6234      220   ***************************************
      -5978      223   ***************************************
      -5722      226   ****************************************
      -5466      226   ****************************************
      -5210      229   ****************************************
      -4954      233   *****************************************
      -4698      232   *****************************************
      -4442      238   ******************************************
      -4186      237   ******************************************
      -3930      241   ******************************************
      -3674      241   ******************************************
      -3418      245   *******************************************
      -3162      247   *******************************************
      -2906      247   *******************************************
      -2650      252   ********************************************
      -2394      251   ********************************************
      -2138      255   *********************************************
      -1882      258   *********************************************
      -1626      258   *********************************************
      -1370      263   **********************************************
      -1114      263   **********************************************
       -858      265   **********************************************
       -602      269   ***********************************************
       -346      267   ***********************************************
        -90      275   ************************************************
        166      271   ************************************************
        422      278   *************************************************
        678      279   *************************************************
        934      281   *************************************************
       1190      280   *************************************************
       1446      281   *************************************************
       1702      280   *************************************************
       1958      280   *************************************************
       2214      283   **************************************************  1
       2470      280   *************************************************
       2726      280   *************************************************
       2982      282   **************************************************  2
       3238      280   *************************************************
       3494      281   *************************************************
       3750      281   *************************************************
       4006      278   *************************************************
       4262      277   *************************************************
       4518      272   ************************************************
       4774      274   ************************************************
       5030      267   ***********************************************
       5286      270   ***********************************************
       5542      265   **********************************************
       5798      263   **********************************************
       6054      261   **********************************************
       6310      259   *********************************************
       6566      258   *********************************************
       6822      254   *********************************************
       7078      253   ********************************************
       7334      251   ********************************************
       7590      246   *******************************************
       7846      248   *******************************************
       8102      246   *******************************************
       8358      241   ******************************************
       8614      241   ******************************************
       8870      235   *****************************************
       9126      238   ******************************************
       9382      233   *****************************************
       9638      233   *****************************************
       9894      228   ****************************************
      10150      227   ****************************************
      10406      226   ****************************************
      10662      223   ***************************************
      10918      219   **************************************
      11174      221   ***************************************
      11430      213   *************************************
      11686      217   **************************************
      11942      211   *************************************
      12198      212   *************************************
      12454      207   ************************************
      12710      206   ************************************
      12966      204   ************************************
      13222      201   ***********************************
      13478      200   ***********************************
      13734      198   ***********************************
      13990      194   **********************************
      14246      195   **********************************
      14502      190   *********************************
      14758      189   *********************************
      15014      187   *********************************
      15270      184   ********************************
      15526      184   ********************************
      15782      178   *******************************
      16038      181   ********************************
      16294      175   *******************************
      16550      174   ******************************
      16806      173   ******************************
      17062      169   *****************************
      17318      168   *****************************
      17574      167   *****************************
      17830      161   ****************************
      18086      163   ****************************
      18342      157   ***************************
      18598      160   ****************************
      18854      153   ***************************
      19110      153   ***************************
      19366      150   **************************
      19622      148   **************************
      19878      146   *************************
      20134      145   *************************
      20390      141   *************************
      20646      142   *************************
      20902      136   ************************
      21158      137   ************************
      21414      132   ***********************
      21670      133   ***********************
      21926      130   ***********************
      22182      125   **********************
      22438      127   **********************
      22694      122   *********************
      22950      121   *********************
      23206      118   ********************
      23462      116   ********************
      23718      115   ********************
      23974      114   ********************
      24230      109   *******************
      24486      110   *******************
      24742      103   ******************
      24998      106   ******************
      25254       99   *****************
      25510      101   *****************
      25766       97   *****************
      26022       96   *****************
      26278       93   ****************
      26534       91   ****************
      26790       89   ***************
      27046       88   ***************
      27302       82   **************
      27558       84   **************
      27814       79   **************
      28070       81   **************
      28326       75   *************
      28582       73   ************
      28838       73   ************
      29094       69   ************
      29350       68   ************
      29606       66   ***********
      29862       63   ***********
      30118       62   **********
      30374       59   **********
      30630       57   **********
      30886       55   *********
      31142       52   *********
      31398       54   *********
      31654       46   ********
      31910       49   ********
      32166       44   *******
      32422       43   *******
      32678        8   *

AVERAGE GRAY LEVEL=1752.829
STANDARD DEVIATION=14342.64
NUMBER ELEMENTS=     4000
MIN. DN=    -32602
MAX. DN=     32589

gen a 3 3 3 ival=2 sinc=3 linc=3
Beginning VICAR task gen
GEN Version 6
GEN task completed
size a b nb=2 zoom=2
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      6 X      6
 PICTURE SIZE SCALED BY      2*NL,      2*NS
 SIZE task completed
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:32:58 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:32:58 2012
 ***********
 Band =     1
 ***********
     Samp     1       3       5
   Line
      1       1   2   4   5   7   8
      2       2   4   5   7   8  10
      3       4   5   7   8  10  11
      4       5   7   8  10  11  13
      5       7   8  10  11  13  14
      6       8  10  11  13  14  16


 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:32:58 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:32:58 2012
 ***********
 Band =     2
 ***********
     Samp     1       3       5
   Line
      1       2   3   5   6   8   9
      2       3   5   6   8   9  11
      3       5   6   8   9  11  12
      4       6   8   9  11  12  14
      5       8   9  11  12  14  15
      6       9  11  12  14  15  17
size a b zoom=2
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      6 X      6
 PICTURE SIZE SCALED BY      2*NL,      2*NS
 SIZE task completed
list b nb=1 sb=3
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:32:58 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:32:59 2012
 ***********
 Band =     3
 ***********
     Samp     1       3       5
   Line
      1       3   4   6   7   9  10
      2       4   6   7   9  10  12
      3       6   7   9  10  12  13
      4       7   9  10  12  13  15
      5       9  10  12  13  15  16
      6      10  12  13  15  16  18
gen a 2 50000
Beginning VICAR task gen
GEN Version 6
GEN task completed
size a b zoom=2
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    2,50000)
     OUTPUT SIZE=      4 X 100000
 Warning: NSOUT > 50,000 fails on Solaris!
 PICTURE SIZE SCALED BY      2*NL,      2*NS
 SIZE task completed
list b ss=99991 ns=10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:32:59 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:33:00 2012
     Samp 99991   99993   99995   99997   99999
   Line

      2      20  20  20  20  20  20  20  20  21  21
      3      57  57  58  58  59  59  59  60  60  60
      4      94  95  96  96  97  98  98  99  99 100
let $echo="no"
!!!!!!!!!!!!!!!!!!!!!!!!!
! Non-interpolation mode
!!!!!!!!!!!!!!!!!!!!!!!!!
gen a 3 3 ival=2 sinc=3 linc=3
Beginning VICAR task gen
GEN Version 6
GEN task completed
size a b zoom=3 'noin
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      9 X      9
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZE task completed
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:33:00 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:33:01 2012
     Samp     1       3       5       7       9
   Line
      1       2   2   2   5   5   5   8   8   8
      2       2   2   2   5   5   5   8   8   8
      3       2   2   2   5   5   5   8   8   8
      4       5   5   5   8   8   8  11  11  11
      5       5   5   5   8   8   8  11  11  11
      6       5   5   5   8   8   8  11  11  11
      7       8   8   8  11  11  11  14  14  14
      8       8   8   8  11  11  11  14  14  14
      9       8   8   8  11  11  11  14  14  14
size b c zoom=-3 'noin
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    9,    9)
     OUTPUT SIZE=      3 X      3
 PICTURE SIZE SCALED BY      0.33333*NL,      0.33333*NS
 SIZE task completed
difpic (a,c)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
size a c zoom=2.5 'noin
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      7 X      7
 PICTURE SIZE SCALED BY      2.50000*NL,      2.50000*NS
 SIZE task completed
list c
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:33:00 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:33:02 2012
     Samp     1       3       5       7
   Line
      1       2   2   5   5   5   8   8
      2       2   2   5   5   5   8   8
      3       5   5   8   8   8  11  11
      4       5   5   8   8   8  11  11
      5       5   5   8   8   8  11  11
      6       8   8  11  11  11  14  14
      7       8   8  11  11  11  14  14
size b c zoom=-2.5 'noin
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    9,    9)
     OUTPUT SIZE=      4 X      4
 PICTURE SIZE SCALED BY      0.40000*NL,      0.40000*NS
 SIZE task completed
list c
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:33:00 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:33:03 2012
     Samp     1       3
   Line
      1       2   2   5   8
      2       2   2   5   8
      3       5   5   8  11
      4       8   8  11  14
size a b (1,1,11,11) ioffset=(2,2) zoom=3 'noin
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=     11 X     11
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZE task completed
list b 'zeroes
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:33:00 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:33:03 2012
     Samp     1       3       5       7       9      11
   Line
      1       0   0   0   0   0   0   0   0   0   0   0
      2       0   2   2   2   5   5   5   8   8   8   0
      3       0   2   2   2   5   5   5   8   8   8   0
      4       0   2   2   2   5   5   5   8   8   8   0
      5       0   5   5   5   8   8   8  11  11  11   0
      6       0   5   5   5   8   8   8  11  11  11   0
      7       0   5   5   5   8   8   8  11  11  11   0
      8       0   8   8   8  11  11  11  14  14  14   0
      9       0   8   8   8  11  11  11  14  14  14   0
     10       0   8   8   8  11  11  11  14  14  14   0
     11       0   0   0   0   0   0   0   0   0   0   0
size a b zoom=3 limits=(1,14) 'noin
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      9 X      9
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZE task completed
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:33:00 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:33:04 2012
     Samp     1       3       5       7       9
   Line
      1       2   2   2   5   5   5   8   8   8
      2       2   2   2   5   5   5   8   8   8
      3       2   2   2   5   5   5   8   8   8
      4       5   5   5   8   8   8  11  11  11
      5       5   5   5   8   8   8  11  11  11
      6       5   5   5   8   8   8  11  11  11
      7       8   8   8  11  11  11  14  14  14
      8       8   8   8  11  11  11  14  14  14
      9       8   8   8  11  11  11  14  14  14
size a b lzoom=1 szoom=3 'noin
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      3 X      9
 PICTURE SIZE SCALED BY      1*NL,      3*NS
 SIZE task completed
size b c lzoom=3 szoom=-3 'noin
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    9)
     OUTPUT SIZE=      9 X      3
 PICTURE SIZE SCALED BY      3*NL,     -3*NS
 SIZE task completed
list c
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:33:00 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:33:05 2012
     Samp     1       3
   Line
      1       2   5   8
      2       2   5   8
      3       2   5   8
      4       5   8  11
      5       5   8  11
      6       5   8  11
      7       8  11  14
      8       8  11  14
      9       8  11  14
size c d lzoom=-3 szoom=3 'noin
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    9,    3)
     OUTPUT SIZE=      3 X      9
 PICTURE SIZE SCALED BY     -3*NL,      3*NS
 SIZE task completed
difpic (b,d)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
size a b zoom=3 'half scale=100 'noin
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      9 X      9
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZE task completed
list b
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:33:00 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:33:07 2012
     Samp       1     2     3     4     5     6     7     8     9
   Line
      1       200   200   200   500   500   500   800   800   800
      2       200   200   200   500   500   500   800   800   800
      3       200   200   200   500   500   500   800   800   800
      4       500   500   500   800   800   800  1100  1100  1100
      5       500   500   500   800   800   800  1100  1100  1100
      6       500   500   500   800   800   800  1100  1100  1100
      7       800   800   800  1100  1100  1100  1400  1400  1400
      8       800   800   800  1100  1100  1100  1400  1400  1400
      9       800   800   800  1100  1100  1100  1400  1400  1400
size b c zoom=-3 'full scale=1000 'noin
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    9,    9)
     OUTPUT SIZE=      3 X      3
 PICTURE SIZE SCALED BY      0.33333*NL,      0.33333*NS
 SIZE task completed
list c
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:33:00 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:33:08 2012
     Samp            1          2          3
   Line
      1         200000     500000     800000
      2         500000     800000    1100000
      3         800000    1100000    1400000
size b d zoom=-3 'real scale=1000 'noin
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    9,    9)
     OUTPUT SIZE=      3 X      3
 PICTURE SIZE SCALED BY      0.33333*NL,      0.33333*NS
 SIZE task completed
list d
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:33:00 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:33:08 2012
     Samp             1           2           3
   Line
      1       2.000E+05   5.000E+05   8.000E+05
      2       5.000E+05   8.000E+05   1.100E+06
      3       8.000E+05   1.100E+06   1.400E+06
size d e zoom=3 'byte scale=0.00001 'noin
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      9 X      9
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZE task completed
list e
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:33:00 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:33:09 2012
     Samp     1       3       5       7       9
   Line
      1       2   2   2   5   5   5   8   8   8
      2       2   2   2   5   5   5   8   8   8
      3       2   2   2   5   5   5   8   8   8
      4       5   5   5   8   8   8  11  11  11
      5       5   5   5   8   8   8  11  11  11
      6       5   5   5   8   8   8  11  11  11
      7       8   8   8  11  11  11  14  14  14
      8       8   8   8  11  11  11  14  14  14
      9       8   8   8  11  11  11  14  14  14
size c e zoom=3 'half scale=0.001 'noin
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      9 X      9
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZE task completed
list e
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:33:00 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:33:09 2012
     Samp       1     2     3     4     5     6     7     8     9
   Line
      1       200   200   200   500   500   500   800   800   800
      2       200   200   200   500   500   500   800   800   800
      3       200   200   200   500   500   500   800   800   800
      4       500   500   500   800   800   800  1100  1100  1100
      5       500   500   500   800   800   800  1100  1100  1100
      6       500   500   500   800   800   800  1100  1100  1100
      7       800   800   800  1100  1100  1100  1400  1400  1400
      8       800   800   800  1100  1100  1100  1400  1400  1400
      9       800   800   800  1100  1100  1100  1400  1400  1400
size b e zoom=-3 'byte scale=0.01 'noin
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    9,    9)
     OUTPUT SIZE=      3 X      3
 PICTURE SIZE SCALED BY      0.33333*NL,      0.33333*NS
 SIZE task completed
list e
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:33:00 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:33:10 2012
     Samp     1       3
   Line
      1       2   5   8
      2       5   8  11
      3       8  11  14
gen     out=g nl=3375 ns=3648 ival=-32768 linc=10 sinc=10 'half
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist    g
Beginning VICAR task hist
*** HIST version 13 Jul 2012 ***

Bin Width =      256.0
     -32768     6097   ***
     -32512    11764   ******
     -32256    12155   ******
     -32000    12194   ******
     -31744    11841   ******
     -31488    12078   ******
     -31232    11892   ******
     -30976    12027   ******
     -30720    12194   ******
     -30464    11969   ******
     -30208    11950   ******
     -29952    12020   ******
     -29696    11899   ******
     -29440    12194   ******
     -29184    12097   ******
     -28928    11822   ******
     -28672    12148   ******
     -28416    11771   ******
     -28160    12209   ******
     -27904    12675   *******
     -27648    12825   *******
     -27392    14001   *******
     -27136    14100   ********
     -26880    15327   ********
     -26624    16003   *********
     -26368    16025   *********
     -26112    17329   *********
     -25856    17300   *********
     -25600    18655   **********
     -25344    19331   ***********
     -25088    19225   **********
     -24832    20657   ***********
     -24576    20500   ***********
     -24320    21983   ************
     -24064    22659   ************
     -23808    22425   ************
     -23552    23985   *************
     -23296    23700   *************
     -23040    25311   **************
     -22784    25987   **************
     -22528    25625   **************
     -22272    27313   ***************
     -22016    26900   ***************
     -21760    28639   ****************
     -21504    29315   ****************
     -21248    28825   ****************
     -20992    30641   *****************
     -20736    30100   *****************
     -20480    31967   ******************
     -20224    32643   ******************
     -19968    32025   ******************
     -19712    33969   *******************
     -19456    33300   ******************
     -19200    35295   ********************
     -18944    35971   ********************
     -18688    35225   ********************
     -18432    37297   *********************
     -18176    36500   ********************
     -17920    38623   **********************
     -17664    39299   **********************
     -17408    38425   *********************
     -17152    40625   ***********************
     -16896    39700   **********************
     -16640    41951   ***********************
     -16384    42627   ************************
     -16128    41625   ***********************
     -15872    43953   *************************
     -15616    42900   ************************
     -15360    45279   *************************
     -15104    45955   **************************
     -14848    44825   *************************
     -14592    47281   **************************
     -14336    46100   **************************
     -14080    48607   ***************************
     -13824    49283   ****************************
     -13568    48025   ***************************
     -13312    50609   ****************************
     -13056    49300   ****************************
     -12800    51935   *****************************
     -12544    52611   *****************************
     -12288    51225   *****************************
     -12032    53937   ******************************
     -11776    52500   *****************************
     -11520    55263   *******************************
     -11264    55939   *******************************
     -11008    54425   *******************************
     -10752    57265   ********************************
     -10496    55700   *******************************
     -10240    58591   *********************************
      -9984    59267   *********************************
      -9728    57625   ********************************
      -9472    60593   **********************************
      -9216    58900   *********************************
      -8960    61919   ***********************************
      -8704    62595   ***********************************
      -8448    60825   **********************************
      -8192    63921   ************************************
      -7936    62100   ***********************************
      -7680    65247   *************************************
      -7424    65923   *************************************
      -7168    64025   ************************************
      -6912    67249   **************************************
      -6656    65300   *************************************
      -6400    68575   ***************************************
      -6144    69251   ***************************************
      -5888    67225   **************************************
      -5632    70577   ****************************************
      -5376    68500   ***************************************
      -5120    71903   ****************************************
      -4864    72579   *****************************************
      -4608    70425   ****************************************
      -4352    73905   ******************************************
      -4096    71700   ****************************************
      -3840    75231   ******************************************
      -3584    75907   *******************************************
      -3328    73625   *****************************************
      -3072    77233   ********************************************
      -2816    74900   ******************************************
      -2560    78559   ********************************************
      -2304    79235   *********************************************
      -2048    76825   *******************************************
      -1792    80561   *********************************************
      -1536    78100   ********************************************
      -1280    81887   **********************************************
      -1024    82563   ***********************************************
       -768    80025   *********************************************
       -512    83889   ***********************************************
       -256    81300   **********************************************
          0    85215   ************************************************
        256    85891   ************************************************
        512    83225   ***********************************************
        768    87217   *************************************************
       1024    84347   ************************************************
       1280    87750   **************************************************  1
       1536    87750   **************************************************  2
       1792    84375   ************************************************
       2048    87750   **************************************************
       2304    84375   ************************************************
       2560    87750   **************************************************
       2816    87750   **************************************************
       3072    84375   ************************************************
       3328    87750   **************************************************
       3584    84375   ************************************************
       3840    87399   *************************************************
       4096    86723   *************************************************
       4352    82750   ***********************************************
       4608    85397   ************************************************
       4864    81475   **********************************************
       5120    84071   ***********************************************
       5376    83395   ***********************************************
       5632    79550   *********************************************
       5888    82069   **********************************************
       6144    78275   ********************************************
       6400    80743   **********************************************
       6656    80067   *********************************************
       6912    76350   *******************************************
       7168    78741   ********************************************
       7424    75075   ******************************************
       7680    77415   ********************************************
       7936    76739   *******************************************
       8192    73150   *****************************************
       8448    75413   ******************************************
       8704    71875   ****************************************
       8960    74087   ******************************************
       9216    73411   *****************************************
       9472    69950   ***************************************
       9728    72085   *****************************************
       9984    68675   ***************************************
      10240    70759   ****************************************
      10496    70083   ***************************************
      10752    66750   **************************************
      11008    68757   ***************************************
      11264    65475   *************************************
      11520    67431   **************************************
      11776    66755   **************************************
      12032    63550   ************************************
      12288    65429   *************************************
      12544    62275   ***********************************
      12800    64103   ************************************
      13056    63427   ************************************
      13312    60350   **********************************
      13568    62101   ***********************************
      13824    59075   *********************************
      14080    60775   **********************************
      14336    60099   **********************************
      14592    57150   ********************************
      14848    58773   *********************************
      15104    55875   *******************************
      15360    57447   ********************************
      15616    56771   ********************************
      15872    53950   ******************************
      16128    55445   *******************************
      16384    52675   ******************************
      16640    54119   ******************************
      16896    53443   ******************************
      17152    50750   ****************************
      17408    52117   *****************************
      17664    49475   ****************************
      17920    50791   ****************************
      18176    50115   ****************************
      18432    47550   ***************************
      18688    48789   ***************************
      18944    46275   **************************
      19200    47463   ***************************
      19456    46787   **************************
      19712    44350   *************************
      19968    45461   *************************
      20224    43075   ************************
      20480    44135   *************************
      20736    43459   ************************
      20992    41150   ***********************
      21248    42133   ************************
      21504    39875   **********************
      21760    40807   ***********************
      22016    40131   **********************
      22272    37950   *********************
      22528    38805   **********************
      22784    36675   ********************
      23040    37479   *********************
      23296    36803   ********************
      23552    34750   *******************
      23808    35477   ********************
      24064    33475   *******************
      24320    34151   *******************
      24576    33475   *******************
      24832    31550   *****************
      25088    32149   ******************
      25344    30275   *****************
      25600    30823   *****************
      25856    30147   *****************
      26112    28350   ****************
      26368    28821   ****************
      26624    27075   ***************
      26880    27495   ***************
      27136    26819   ***************
      27392    25150   **************
      27648    25493   **************
      27904    23875   *************
      28160    24167   *************
      28416    23491   *************
      28672    21950   ************
      28928    22165   ************
      29184    20675   ***********
      29440    20839   ***********
      29696    20163   ***********
      29952    18750   **********
      30208    18837   **********
      30464    17475   *********
      30720    17511   *********
      30976    16835   *********
      31232    15550   ********
      31488    15509   ********
      31744    14275   ********
      32000    14183   ********
      32256    13507   *******
      32512    12350   *******
      32768     6175   ***

AVERAGE GRAY LEVEL=1752.829
STANDARD DEVIATION=14366.17
NUMBER ELEMENTS=  1231200
MIN. DN=    -32768
MAX. DN=     32762

size    g h size=(1,1,337,364) 'noin
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1, 3375, 3648)
     OUTPUT SIZE=    337 X    364
 PICTURE SIZE SCALED BY      0.09985*NL,      0.09978*NS
 SIZE task completed
hist    h
Beginning VICAR task hist
*** HIST version 13 Jul 2012 ***

Bin Width =      256.0
     -32768       48   **
     -32512      136   ******
     -32256       99   ****
     -32000      131   ******
     -31744      104   *****
     -31488      141   ******
     -31232       94   ****
     -30976      141   ******
     -30720      118   *****
     -30464      117   *****
     -30208      113   *****
     -29952      122   ******
     -29696      108   *****
     -29440      127   ******
     -29184      141   ******
     -28928       94   ****
     -28672      141   ******
     -28416       95   ****
     -28160      141   ******
     -27904       99   ****
     -27648      156   *******
     -27392      155   *******
     -27136      125   ******
     -26880      180   ********
     -26624      125   ******
     -26368      195   *********
     -26112      135   ******
     -25856      210   **********
     -25600      145   *******
     -25344      225   ***********
     -25088      190   *********
     -24832      205   **********
     -24576      249   ************
     -24320      171   ********
     -24064      264   *************
     -23808      181   ********
     -23552      279   *************
     -23296      191   *********
     -23040      294   **************
     -22784      292   **************
     -22528      218   **********
     -22272      318   ***************
     -22016      217   **********
     -21760      333   ****************
     -21504      227   ***********
     -21248      348   *****************
     -20992      237   ***********
     -20736      363   *****************
     -20480      327   ****************
     -20224      298   **************
     -19968      387   *******************
     -19712      263   *************
     -19456      402   *******************
     -19200      273   *************
     -18944      417   ********************
     -18688      283   *************
     -18432      432   *********************
     -18176      337   ****************
     -17920      403   *******************
     -17664      456   **********************
     -17408      309   ***************
     -17152      471   ***********************
     -16896      319   ***************
     -16640      486   ************************
     -16384      344   *****************
     -16128      486   ************************
     -15872      464   **********************
     -15616      391   *******************
     -15360      525   *************************
     -15104      355   *****************
     -14848      540   **************************
     -14592      365   ******************
     -14336      555   ***************************
     -14080      375   ******************
     -13824      570   ****************************
     -13568      474   ***********************
     -13312      496   ************************
     -13056      577   ****************************
     -12800      418   ********************
     -12544      609   ******************************
     -12288      411   ********************
     -12032      624   ******************************
     -11776      421   ********************
     -11520      639   *******************************
     -11264      601   *****************************
     -11008      484   ***********************
     -10752      663   ********************************
     -10496      447   **********************
     -10240      678   *********************************
      -9984      457   **********************
      -9728      693   **********************************
      -9472      481   ***********************
      -9216      694   **********************************
      -8960      610   ******************************
      -8704      590   *****************************
      -8448      715   ***********************************
      -8192      510   *************************
      -7936      747   ************************************
      -7680      503   ************************
      -7424      762   *************************************
      -7168      513   *************************
      -6912      777   **************************************
      -6656      609   ******************************
      -6400      706   **********************************
      -6144      801   ***************************************
      -5888      539   **************************
      -5632      816   ****************************************
      -5376      549   ***************************
      -5120      831   *****************************************
      -4864      607   ******************************
      -4608      798   ***************************************
      -4352      747   ************************************
      -4096      683   *********************************
      -3840      852   ******************************************
      -3584      603   *****************************
      -3328      885   *******************************************
      -3072      595   *****************************
      -2816      900   ********************************************
      -2560      617   ******************************
      -2304      903   ********************************************
      -2048      745   ************************************
      -1792      800   ***************************************
      -1536      880   *******************************************
      -1280      690   **********************************
      -1024      954   ***********************************************
       -768      641   *******************************
       -512      969   ***********************************************
       -256      651   ********************************
          0      984   ************************************************
        256      883   *******************************************
        512      777   **************************************
        768      990   ************************************************
       1024      692   **********************************
       1280     1011   **************************************************  1
       1536      674   *********************************
       1792     1011   **************************************************  2
       2048      718   ***********************************
       2304      967   ***********************************************
       2560      831   *****************************************
       2816      854   ******************************************
       3072      951   ***********************************************
       3328      734   ************************************
       3584     1010   *************************************************
       3840      669   *********************************
       4096      996   *************************************************
       4352      669   *********************************
       4608      971   ************************************************
       4864      735   ************************************
       5120      880   *******************************************
       5376      957   ***********************************************
       5632      633   *******************************
       5888      942   **********************************************
       6144      623   ******************************
       6400      927   *********************************************
       6656      656   ********************************
       6912      869   ******************************************
       7168      758   *************************************
       7424      742   ************************************
       7680      869   ******************************************
       7936      606   *****************************
       8192      873   *******************************************
       8448      577   ****************************
       8704      858   ******************************************
       8960      576   ****************************
       9216      834   *****************************************
       9472      642   *******************************
       9728      743   ************************************
       9984      756   *************************************
      10240      604   *****************************
      10496      804   ***************************************
      10752      531   **************************
      11008      789   ***************************************
      11264      521   *************************
      11520      774   **************************************
      11776      665   ********************************
      12032      605   *****************************
      12288      750   *************************************
      12544      495   ************************
      12800      735   ************************************
      13056      485   ***********************
      13312      720   ***********************************
      13568      484   ***********************
      13824      696   **********************************
      14080      548   ***************************
      14336      607   ******************************
      14592      660   ********************************
      14848      470   ***********************
      15104      666   ********************************
      15360      439   *********************
      15616      651   ********************************
      15872      429   *********************
      16128      636   *******************************
      16384      450   **********************
      16640      590   *****************************
      16896      612   ******************************
      17152      403   *******************
      17408      597   *****************************
      17664      393   *******************
      17920      582   ****************************
      18176      391   *******************
      18432      559   ***************************
      18688      455   **********************
      18944      470   ***********************
      19200      543   **************************
      19456      357   *****************
      19712      528   **************************
      19968      347   *****************
      20224      513   *************************
      20480      337   ****************
      20736      498   ************************
      20992      357   *****************
      21248      453   **********************
      21504      451   **********************
      21760      334   ****************
      22016      459   **********************
      22272      301   **************
      22528      444   *********************
      22784      291   **************
      23040      429   *********************
      23296      361   *****************
      23552      334   ****************
      23808      405   ********************
      24064      265   *************
      24320      390   *******************
      24576      255   ************
      24832      375   ******************
      25088      245   ************
      25344      360   *****************
      25600      264   *************
      25856      316   ***************
      26112      335   ****************
      26368      220   **********
      26624      321   ***************
      26880      209   **********
      27136      306   ***************
      27392      199   *********
      27648      291   **************
      27904      191   *********
      28160      274   *************
      28416      267   *************
      28672      173   ********
      28928      252   ************
      29184      163   ********
      29440      237   ***********
      29696      153   *******
      29952      222   **********
      30208      171   ********
      30464      179   ********
      30720      198   *********
      30976      127   ******
      31232      183   *********
      31488      117   *****
      31744      168   ********
      32000      107   *****
      32256      153   *******
      32512       99   ****
      32768       91   ****

AVERAGE GRAY LEVEL=1683.932
STANDARD DEVIATION=14361.92
NUMBER ELEMENTS=    12266
MIN. DN=    -32768
MAX. DN=     32742

size    g h size=(300,200,200,200) 'noin
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1, 3375, 3648)
     OUTPUT SIZE=    200 X    200
 PICTURE SIZE SCALED BY      0.05926*NL,      0.05482*NS
 SIZE task completed
hist    h
Beginning VICAR task hist
*** HIST version 13 Jul 2012 ***

Bin Width =      256.0
     -32768       19   ***
     -32512       40   ******
     -32256       38   ******
     -32000       41   *******
     -31744       37   ******
     -31488       39   ******
     -31232       40   ******
     -30976       38   ******
     -30720       40   ******
     -30464       39   ******
     -30208       37   ******
     -29952       42   *******
     -29696       36   ******
     -29440       42   *******
     -29184       37   ******
     -28928       39   ******
     -28672       40   ******
     -28416       39   ******
     -28160       40   ******
     -27904       43   *******
     -27648       42   *******
     -27392       48   ********
     -27136       46   *******
     -26880       53   *********
     -26624       53   *********
     -26368       52   ********
     -26112       60   **********
     -25856       55   *********
     -25600       66   ***********
     -25344       61   **********
     -25088       67   ***********
     -24832       66   ***********
     -24576       70   ************
     -24320       72   ************
     -24064       73   ************
     -23808       76   *************
     -23552       79   *************
     -23296       78   *************
     -23040       85   **************
     -22784       84   **************
     -22528       86   **************
     -22272       88   ***************
     -22016       91   ***************
     -21760       93   ***************
     -21504       99   *****************
     -21248       92   ***************
     -20992      105   ******************
     -20736       95   ****************
     -20480      111   *******************
     -20224      104   *****************
     -19968      109   ******************
     -19712      109   ******************
     -19456      111   *******************
     -19200      116   *******************
     -18944      115   *******************
     -18688      120   ********************
     -18432      119   ********************
     -18176      123   *********************
     -17920      125   *********************
     -17664      131   **********************
     -17408      125   *********************
     -17152      134   ***********************
     -16896      129   **********************
     -16640      139   ***********************
     -16384      138   ***********************
     -16128      138   ***********************
     -15872      143   ************************
     -15616      141   ************************
     -15360      150   *************************
     -15104      149   *************************
     -14848      149   *************************
     -14592      153   **************************
     -14336      152   **************************
     -14080      160   ***************************
     -13824      157   **************************
     -13568      163   ****************************
     -13312      161   ***************************
     -13056      166   ****************************
     -12800      167   ****************************
     -12544      177   ******************************
     -12288      164   ****************************
     -12032      179   ******************************
     -11776      170   *****************************
     -11520      182   *******************************
     -11264      185   *******************************
     -11008      176   ******************************
     -10752      190   ********************************
     -10496      178   ******************************
     -10240      198   **********************************
      -9984      189   ********************************
      -9728      195   *********************************
      -9472      192   ********************************
      -9216      198   **********************************
      -8960      199   **********************************
      -8704      204   ***********************************
      -8448      201   **********************************
      -8192      206   ***********************************
      -7936      206   ***********************************
      -7680      211   ************************************
      -7424      218   *************************************
      -7168      209   ***********************************
      -6912      219   *************************************
      -6656      214   ************************************
      -6400      223   **************************************
      -6144      231   ***************************************
      -5888      215   ************************************
      -5632      236   ****************************************
      -5376      218   *************************************
      -5120      242   *****************************************
      -4864      233   ****************************************
      -4608      235   ****************************************
      -4352      237   ****************************************
      -4096      238   ****************************************
      -3840      244   *****************************************
      -3584      245   ******************************************
      -3328      245   ******************************************
      -3072      247   ******************************************
      -2816      251   *******************************************
      -2560      251   *******************************************
      -2304      263   *********************************************
      -2048      248   ******************************************
      -1792      265   *********************************************
      -1536      254   *******************************************
      -1280      268   **********************************************
      -1024      268   **********************************************
       -768      264   *********************************************
       -512      272   **********************************************
       -256      267   *********************************************
          0      277   ***********************************************
        256      278   ***********************************************
        512      275   ***********************************************
        768      282   ************************************************
       1024      277   ***********************************************
       1280      283   ************************************************
       1536      284   ************************************************
       1792      276   ***********************************************
       2048      282   ************************************************
       2304      278   ***********************************************
       2560      281   ************************************************
       2816      292   **************************************************  1
       3072      267   *********************************************
       3328      291   **************************************************  2
       3584      269   **********************************************
       3840      286   *************************************************
       4096      281   ************************************************
       4352      265   *********************************************
       4608      280   ************************************************
       4864      258   ********************************************
       5120      278   ***********************************************
       5376      263   *********************************************
       5632      263   *********************************************
       5888      261   ********************************************
       6144      256   *******************************************
       6400      259   ********************************************
       6656      258   ********************************************
       6912      247   ******************************************
       7168      253   *******************************************
       7424      244   *****************************************
       7680      248   ******************************************
       7936      253   *******************************************
       8192      230   ***************************************
       8448      249   ******************************************
       8704      226   **************************************
       8960      245   ******************************************
       9216      236   ****************************************
       9472      225   **************************************
       9728      235   ****************************************
       9984      217   *************************************
      10240      234   ****************************************
      10496      222   **************************************
      10752      220   *************************************
      11008      219   *************************************
      11264      213   ************************************
      11520      217   *************************************
      11776      212   ************************************
      12032      208   ***********************************
      12288      208   ***********************************
      12544      204   ***********************************
      12800      204   ***********************************
      13056      207   ***********************************
      13312      192   ********************************
      13568      204   ***********************************
      13824      187   ********************************
      14080      199   **********************************
      14336      191   ********************************
      14592      186   *******************************
      14848      189   ********************************
      15104      179   ******************************
      15360      187   ********************************
      15616      178   ******************************
      15872      180   ******************************
      16128      174   *****************************
      16384      174   *****************************
      16640      171   *****************************
      16896      172   *****************************
      17152      164   ****************************
      17408      167   ****************************
      17664      160   ***************************
      17920      163   ****************************
      18176      164   ****************************
      18432      151   *************************
      18688      160   ***************************
      18944      145   ************************
      19200      157   **************************
      19456      148   *************************
      19712      144   ************************
      19968      146   *************************
      20224      136   ***********************
      20480      145   ************************
      20736      136   ***********************
      20992      137   ***********************
      21248      132   **********************
      21504      131   **********************
      21760      129   **********************
      22016      128   *********************
      22272      124   *********************
      22528      122   ********************
      22784      120   ********************
      23040      118   ********************
      23296      122   ********************
      23552      108   ******************
      23808      118   ********************
      24064      103   *****************
      24320      114   *******************
      24576      104   *****************
      24832      103   *****************
      25088      101   *****************
      25344       97   ****************
      25600      100   *****************
      25856       95   ****************
      26112       93   ***************
      26368       90   ***************
      26624       88   ***************
      26880       87   **************
      27136       84   **************
      27392       83   **************
      27648       78   *************
      27904       79   *************
      28160       75   ************
      28416       76   *************
      28672       70   ************
      28928       70   ************
      29184       66   ***********
      29440       67   ***********
      29696       63   **********
      29952       61   **********
      30208       58   *********
      30464       56   *********
      30720       56   *********
      30976       51   ********
      31232       52   ********
      31488       46   *******
      31744       48   ********
      32000       42   *******
      32256       44   *******
      32512       38   ******
      32768       20   ***

AVERAGE GRAY LEVEL=1621.578
STANDARD DEVIATION=14360.94
NUMBER ELEMENTS=     4000
MIN. DN=    -32768
MAX. DN=     32762

gen a 3 3 3 ival=2 sinc=3 linc=3
Beginning VICAR task gen
GEN Version 6
GEN task completed
size a b nb=2 zoom=2 'noin
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      6 X      6
 PICTURE SIZE SCALED BY      2*NL,      2*NS
 SIZE task completed
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:33:16 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:33:17 2012
 ***********
 Band =     1
 ***********
     Samp     1       3       5
   Line
      1       2   2   5   5   8   8
      2       2   2   5   5   8   8
      3       5   5   8   8  11  11
      4       5   5   8   8  11  11
      5       8   8  11  11  14  14
      6       8   8  11  11  14  14


 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:33:16 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:33:17 2012
 ***********
 Band =     2
 ***********
     Samp     1       3       5
   Line
      1       3   3   6   6   9   9
      2       3   3   6   6   9   9
      3       6   6   9   9  12  12
      4       6   6   9   9  12  12
      5       9   9  12  12  15  15
      6       9   9  12  12  15  15
size a b zoom=2 'noin
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      6 X      6
 PICTURE SIZE SCALED BY      2*NL,      2*NS
 SIZE task completed
list b nb=1 sb=3
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:33:16 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:33:17 2012
 ***********
 Band =     3
 ***********
     Samp     1       3       5
   Line
      1       4   4   7   7  10  10
      2       4   4   7   7  10  10
      3       7   7  10  10  13  13
      4       7   7  10  10  13  13
      5      10  10  13  13  16  16
      6      10  10  13  13  16  16
gen a 2 50000
Beginning VICAR task gen
GEN Version 6
GEN task completed
size a b zoom=2 'noin
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    2,50000)
     OUTPUT SIZE=      4 X 100000
 Warning: NSOUT > 50,000 fails on Solaris!
 PICTURE SIZE SCALED BY      2*NL,      2*NS
 SIZE task completed
list b ss=99991 ns=10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:33:17 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:33:18 2012
     Samp 99991   99993   99995   99997   99999
   Line
      1      75  75  76  76  77  77  78  78  79  79
      2      75  75  76  76  77  77  78  78  79  79
      3      76  76  77  77  78  78  79  79  80  80
      4      76  76  77  77  78  78  79  79  80  80
let $echo="no"
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
****Test for fail on size 'noin ****
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
gen x 10 10
Beginning VICAR task gen
GEN Version 6
GEN task completed
list x
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:33:18 2012
     Samp     1       3       5       7       9
   Line
      1       0   1   2   3   4   5   6   7   8   9
      2       1   2   3   4   5   6   7   8   9  10
      3       2   3   4   5   6   7   8   9  10  11
      4       3   4   5   6   7   8   9  10  11  12
      5       4   5   6   7   8   9  10  11  12  13
      6       5   6   7   8   9  10  11  12  13  14
      7       6   7   8   9  10  11  12  13  14  15
      8       7   8   9  10  11  12  13  14  15  16
      9       8   9  10  11  12  13  14  15  16  17
     10       9  10  11  12  13  14  15  16  17  18
stretch x y func="in1+18"
Beginning VICAR task stretch
STRETCH version Oct 17 2002
*** USER SPECIFIED FUNCTION MODE ***
Function Stretch: FUNCTION = in1+18
list y
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:33:18 2012
 Task:STRETCH   User:lwk       Date_Time:Sun Jul 29 13:33:19 2012
     Samp     1       3       5       7       9
   Line
      1      18  19  20  21  22  23  24  25  26  27
      2      19  20  21  22  23  24  25  26  27  28
      3      20  21  22  23  24  25  26  27  28  29
      4      21  22  23  24  25  26  27  28  29  30
      5      22  23  24  25  26  27  28  29  30  31
      6      23  24  25  26  27  28  29  30  31  32
      7      24  25  26  27  28  29  30  31  32  33
      8      25  26  27  28  29  30  31  32  33  34
      9      26  27  28  29  30  31  32  33  34  35
     10      27  28  29  30  31  32  33  34  35  36
concat (x,y) z ns=20 'nost
Beginning VICAR task concat
list z sl=1 ss=1 nl=10 ns=10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:33:18 2012
 Task:CONCAT    User:lwk       Date_Time:Sun Jul 29 13:33:20 2012
     Samp     1       3       5       7       9
   Line
      1       0   1   2   3   4   5   6   7   8   9
      2       1   2   3   4   5   6   7   8   9  10
      3       2   3   4   5   6   7   8   9  10  11
      4       3   4   5   6   7   8   9  10  11  12
      5       4   5   6   7   8   9  10  11  12  13
      6       5   6   7   8   9  10  11  12  13  14
      7       6   7   8   9  10  11  12  13  14  15
      8       7   8   9  10  11  12  13  14  15  16
      9       8   9  10  11  12  13  14  15  16  17
     10       9  10  11  12  13  14  15  16  17  18
let $echo="no"
 ==============================================  should FAIL here ===========
size z w zoom=10 'noin
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,   10,   20)
     OUTPUT SIZE=    100 X    200
 PICTURE SIZE SCALED BY     10*NL,     10*NS
 SIZE task completed
let $echo="no"
================================================= NO FAIL here ==============
size w xx zoom=10
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,  100,  200)
     OUTPUT SIZE=   1000 X   2000
 PICTURE SIZE SCALED BY     10*NL,     10*NS
 SIZE task completed
list w sl=1 ss=1 nl=10 ns=10
Beginning VICAR task list
 ** The specified window is all zero.
list xx sl=1 ss=1 nl=10 ns=10
Beginning VICAR task list
 ** The specified window is all zero.
let $echo="no"
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Test updating of map projection labels
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
label-list vt/m.dat
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File vt/m.dat ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in WORD format from a VAX-VMS host
                1 bands
                500 lines per band
                500 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: CONV12 -- User: DFS -- Thu May  3 11:15:53 1984 ----
LAB01=
'77                  1000    200010002000 I 2                          SC'
LAB02=
'VGR-1   FDS 16368.32   PICNO 0548J1-001   SCET 79.063 19:23:00         C'
LAB03=
'NA CAMERA  EXP 00360.0 MSEC  FILT 0(CLEAR )  LO GAIN  SCAN RATE  1:1   C'
LAB04=
'ERT 79.063 20:00:36   1/ 1 FULL    RES   VIDICON TEMP  -81.00 DEG C    C'
LAB05=
'IN/107971/07 OUT/******/**                 DSS #**   BIT SNR   34.133  C'
LAB06=
' 01865 A/9F000BC0 B/7082 C/C201 D/007F0000 ETLM/CC2BA1CA736ADEB02512F AC'
LAB07=
'NA OPCAL 00(*****.* MSEC) PIXAVG 192/0 OPERATIONAL MODE 4(BOTALT)     AC'
LAB08=
'CAM ECAL CYCLE BEAM  RESET OPEN  CLOSE FLOOD AEXPM  FIL G1 SHUT MODE  AC'
LAB09=
'NA   NO   READ  YES   NO    NO    NO    NO    NO    1 P  * NORMAL     AC'
LAB10=
'WA   NO   PREP  YES   YES   YES   YES   YES   NO    4 P  3 NORMAL     AC'
LAB11=
'ADESPIKE                                                             1HC'
LAB12=
'ADJACENT LINE PIXELS CHANGED    184. SAME LINE PIXELS CHANGED   124  1HC'
LAB13=
'RESSAR77 - FICOR77  MINSAT= 3765 NUMSAT=  7674                       1HC'
LAB14=
'TO CONVERT TO NANOWATTS/CM**2/STER/NM MULTIPLY DN VALUE BY    0.20224  C'
LAB15=
'TO CONVERT TO (I/F)*10000., MULTIPLY DN VALUE BY              1.00000  C'
LAB16=
'INSERT            09 11 82 19:09:07          JAM                      GC'
LAB17=
'GEOMA             09 13 83 16:49:05          JAM                      GC'
LAB18=
'HALF                                                                  PC'
LAB19=
'GEOMA                                                                 HC'
LAB20=
'INSERT            09 13 83 17:13:48          JAM                      GC'
LAB21=
'PGM=SWAPPER                                                           HL'
NLABS=21
---- Task: MAP -- User: FFM059 -- Wed Jun  1 11:34:14 1994 ----
MAP001='MAP2 LABEL GENERATED BY MAPLAB SUBROUTINE'
MAP002=' RADII=(  1830.0,  1830.0,  1815.3),LORANGLE= 0.0'
MAP003='*** POLAR ORTHOGRAPHIC PROJECTION ***'
MAP004='AT PROJ. CENTER L=   500.0,S=   250.0,LAT= 90.000,LONG=150.000  W'
MAP005='SCALE=   7.000 KM/PXL, NORTH= ******* DEG CLOCKWISE FROM UP'
---- Task: LABSWTCH -- User: FFM059 -- Wed Jun  1 11:35:07 1994 ----
 
************************************************************
size vt/m.dat a zoom=-2 area=(2,2,400,400)
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    2,    2,  400,  400)
     OUTPUT SIZE=    200 X    200
 PICTURE SIZE SCALED BY      0.50000*NL,      0.50000*NS
***MAP_RESOLUTION not found
Computing resolution from scale
MAP property label updated
MAP history label updated
 SIZE task completed
label-list a
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File a ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in HALF format from a SUN-SOLR host
                1 bands
                200 lines per band
                200 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Property: MAP ----
MAP_PROJECTION_TYPE='POLAR_ORTHOGRAPHIC'
COORDINATE_SYSTEM_NAME='PLANETOCENTRIC'
POSITIVE_LONGITUDE_DIRECTION='WEST'
A_AXIS_RADIUS=1830.0
B_AXIS_RADIUS=1830.0
C_AXIS_RADIUS=1815.300048828125
MAP_SCALE=14.0
MAP_RESOLUTION=2.281395642857143
CENTER_LATITUDE=90.0
CENTER_LONGITUDE=150.0
SPHERICAL_AZIMUTH=0.0
CARTESIAN_AZIMUTH=0.0
LINE_PROJECTION_OFFSET=248.75
SAMPLE_PROJECTION_OFFSET=123.75
MAP_PROJECTION_DESC=(
'An azimuthal projection that is neither conformal nor equal-area.  All', 
'meridians and parallels are ellipses, circles, or straight lines.  This', 
'projection resembles a globe in appearance and has much distortion near the', 
'edges of the hemisphere shown.  There is no distortion at the center only,', 
'and directions from the center are true.  Radial scale factor decreases as', 
'distance increases from the center.  Scale in the direction of the lines', 
'of latitude is true in the polar aspect.', 
'In spherical form, the Equatorial aspect equations (20-3),(20-13) through', 
'(20-19) of USGS Paper 1395 (pp 149,150) were used.', 
'For the Oblate Spheroid, code from VICAR subroutine TRANV (q.v.) was used.', 
'The value of the COORDINATE_SYSTEM_NAME item determines whether latitudes are', 
'planetographic or planetocentric;  if this  keyword is absent, then', 
'the default is planetographic.', 
'The direction of increasing longitude is defined by the POSITIVE_LONGITUDE_DIRECTION', 
'item;  if this keyword is absent, then the direction is determined by', 
'COORDINATE_SYSTEM_NAME:  it is East if the system is planetographic, West if it is', 
'planetocentric.', 
'NOTE: Portions of above text taken from U.S. Geological Survey Professional Paper 1395,', 
'second printing 1989, ''Map Projections - A Working Manual'' by John Snyder.', 
'See text for detailed formulae.')
---- Task: CONV12 -- User: DFS -- Thu May  3 11:15:53 1984 ----
LAB01=
'77                  1000    200010002000 I 2                          SC'
LAB02=
'VGR-1   FDS 16368.32   PICNO 0548J1-001   SCET 79.063 19:23:00         C'
LAB03=
'NA CAMERA  EXP 00360.0 MSEC  FILT 0(CLEAR )  LO GAIN  SCAN RATE  1:1   C'
LAB04=
'ERT 79.063 20:00:36   1/ 1 FULL    RES   VIDICON TEMP  -81.00 DEG C    C'
LAB05=
'IN/107971/07 OUT/******/**                 DSS #**   BIT SNR   34.133  C'
LAB06=
' 01865 A/9F000BC0 B/7082 C/C201 D/007F0000 ETLM/CC2BA1CA736ADEB02512F AC'
LAB07=
'NA OPCAL 00(*****.* MSEC) PIXAVG 192/0 OPERATIONAL MODE 4(BOTALT)     AC'
LAB08=
'CAM ECAL CYCLE BEAM  RESET OPEN  CLOSE FLOOD AEXPM  FIL G1 SHUT MODE  AC'
LAB09=
'NA   NO   READ  YES   NO    NO    NO    NO    NO    1 P  * NORMAL     AC'
LAB10=
'WA   NO   PREP  YES   YES   YES   YES   YES   NO    4 P  3 NORMAL     AC'
LAB11=
'ADESPIKE                                                             1HC'
LAB12=
'ADJACENT LINE PIXELS CHANGED    184. SAME LINE PIXELS CHANGED   124  1HC'
LAB13=
'RESSAR77 - FICOR77  MINSAT= 3765 NUMSAT=  7674                       1HC'
LAB14=
'TO CONVERT TO NANOWATTS/CM**2/STER/NM MULTIPLY DN VALUE BY    0.20224  C'
LAB15=
'TO CONVERT TO (I/F)*10000., MULTIPLY DN VALUE BY              1.00000  C'
LAB16=
'INSERT            09 11 82 19:09:07          JAM                      GC'
LAB17=
'GEOMA             09 13 83 16:49:05          JAM                      GC'
LAB18=
'HALF                                                                  PC'
LAB19=
'GEOMA                                                                 HC'
LAB20=
'INSERT            09 13 83 17:13:48          JAM                      GC'
LAB21=
'PGM=SWAPPER                                                           HL'
NLABS=21
---- Task: MAP -- User: FFM059 -- Wed Jun  1 11:34:14 1994 ----
MAP001='MAP2 LABEL GENERATED BY MAPLAB SUBROUTINE'
MAP002=' RADII=(  1830.0,  1830.0,  1815.3),LORANGLE= 0.0'
MAP003='*** POLAR ORTHOGRAPHIC PROJECTION ***'
MAP004='AT PROJ. CENTER L=   500.0,S=   250.0,LAT= 90.000,LONG=150.000  W'
MAP005='SCALE=   7.000 KM/PXL, NORTH= ******* DEG CLOCKWISE FROM UP'
---- Task: LABSWTCH -- User: FFM059 -- Wed Jun  1 11:35:07 1994 ----
---- Task: SIZE -- User: lwk -- Sun Jul 29 13:33:22 2012 ----
COMMENT='PICTURE SIZE SCALED BY      0.50000'
MAP_PROJECTION_TYPE='POLAR_ORTHOGRAPHIC'
COORDINATE_SYSTEM_NAME='PLANETOCENTRIC'
POSITIVE_LONGITUDE_DIRECTION='WEST'
A_AXIS_RADIUS=1830.0
B_AXIS_RADIUS=1830.0
C_AXIS_RADIUS=1815.300048828125
MAP_SCALE=14.0
MAP_RESOLUTION=2.281395642857143
CENTER_LATITUDE=90.0
CENTER_LONGITUDE=150.0
SPHERICAL_AZIMUTH=0.0
CARTESIAN_AZIMUTH=0.0
LINE_PROJECTION_OFFSET=248.75
SAMPLE_PROJECTION_OFFSET=123.75
 
************************************************************
size vt/m.dat a zoom=-1.5 area=(2,2,400,400)
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    2,    2,  400,  400)
     OUTPUT SIZE=    266 X    266
 PICTURE SIZE SCALED BY      0.66667*NL,      0.66667*NS
***MAP_RESOLUTION not found
Computing resolution from scale
MAP property label updated
MAP history label updated
 SIZE task completed
label-list a
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File a ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in HALF format from a SUN-SOLR host
                1 bands
                266 lines per band
                266 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Property: MAP ----
MAP_PROJECTION_TYPE='POLAR_ORTHOGRAPHIC'
COORDINATE_SYSTEM_NAME='PLANETOCENTRIC'
POSITIVE_LONGITUDE_DIRECTION='WEST'
A_AXIS_RADIUS=1830.0
B_AXIS_RADIUS=1830.0
C_AXIS_RADIUS=1815.300048828125
MAP_SCALE=10.49999968707562
MAP_RESOLUTION=3.041860947797375
CENTER_LATITUDE=90.0
CENTER_LONGITUDE=150.0
SPHERICAL_AZIMUTH=0.0
CARTESIAN_AZIMUTH=0.0
LINE_PROJECTION_OFFSET=331.8333432376385
SAMPLE_PROJECTION_OFFSET=165.1666716039181
MAP_PROJECTION_DESC=(
'An azimuthal projection that is neither conformal nor equal-area.  All', 
'meridians and parallels are ellipses, circles, or straight lines.  This', 
'projection resembles a globe in appearance and has much distortion near the', 
'edges of the hemisphere shown.  There is no distortion at the center only,', 
'and directions from the center are true.  Radial scale factor decreases as', 
'distance increases from the center.  Scale in the direction of the lines', 
'of latitude is true in the polar aspect.', 
'In spherical form, the Equatorial aspect equations (20-3),(20-13) through', 
'(20-19) of USGS Paper 1395 (pp 149,150) were used.', 
'For the Oblate Spheroid, code from VICAR subroutine TRANV (q.v.) was used.', 
'The value of the COORDINATE_SYSTEM_NAME item determines whether latitudes are', 
'planetographic or planetocentric;  if this  keyword is absent, then', 
'the default is planetographic.', 
'The direction of increasing longitude is defined by the POSITIVE_LONGITUDE_DIRECTION', 
'item;  if this keyword is absent, then the direction is determined by', 
'COORDINATE_SYSTEM_NAME:  it is East if the system is planetographic, West if it is', 
'planetocentric.', 
'NOTE: Portions of above text taken from U.S. Geological Survey Professional Paper 1395,', 
'second printing 1989, ''Map Projections - A Working Manual'' by John Snyder.', 
'See text for detailed formulae.')
---- Task: CONV12 -- User: DFS -- Thu May  3 11:15:53 1984 ----
LAB01=
'77                  1000    200010002000 I 2                          SC'
LAB02=
'VGR-1   FDS 16368.32   PICNO 0548J1-001   SCET 79.063 19:23:00         C'
LAB03=
'NA CAMERA  EXP 00360.0 MSEC  FILT 0(CLEAR )  LO GAIN  SCAN RATE  1:1   C'
LAB04=
'ERT 79.063 20:00:36   1/ 1 FULL    RES   VIDICON TEMP  -81.00 DEG C    C'
LAB05=
'IN/107971/07 OUT/******/**                 DSS #**   BIT SNR   34.133  C'
LAB06=
' 01865 A/9F000BC0 B/7082 C/C201 D/007F0000 ETLM/CC2BA1CA736ADEB02512F AC'
LAB07=
'NA OPCAL 00(*****.* MSEC) PIXAVG 192/0 OPERATIONAL MODE 4(BOTALT)     AC'
LAB08=
'CAM ECAL CYCLE BEAM  RESET OPEN  CLOSE FLOOD AEXPM  FIL G1 SHUT MODE  AC'
LAB09=
'NA   NO   READ  YES   NO    NO    NO    NO    NO    1 P  * NORMAL     AC'
LAB10=
'WA   NO   PREP  YES   YES   YES   YES   YES   NO    4 P  3 NORMAL     AC'
LAB11=
'ADESPIKE                                                             1HC'
LAB12=
'ADJACENT LINE PIXELS CHANGED    184. SAME LINE PIXELS CHANGED   124  1HC'
LAB13=
'RESSAR77 - FICOR77  MINSAT= 3765 NUMSAT=  7674                       1HC'
LAB14=
'TO CONVERT TO NANOWATTS/CM**2/STER/NM MULTIPLY DN VALUE BY    0.20224  C'
LAB15=
'TO CONVERT TO (I/F)*10000., MULTIPLY DN VALUE BY              1.00000  C'
LAB16=
'INSERT            09 11 82 19:09:07          JAM                      GC'
LAB17=
'GEOMA             09 13 83 16:49:05          JAM                      GC'
LAB18=
'HALF                                                                  PC'
LAB19=
'GEOMA                                                                 HC'
LAB20=
'INSERT            09 13 83 17:13:48          JAM                      GC'
LAB21=
'PGM=SWAPPER                                                           HL'
NLABS=21
---- Task: MAP -- User: FFM059 -- Wed Jun  1 11:34:14 1994 ----
MAP001='MAP2 LABEL GENERATED BY MAPLAB SUBROUTINE'
MAP002=' RADII=(  1830.0,  1830.0,  1815.3),LORANGLE= 0.0'
MAP003='*** POLAR ORTHOGRAPHIC PROJECTION ***'
MAP004='AT PROJ. CENTER L=   500.0,S=   250.0,LAT= 90.000,LONG=150.000  W'
MAP005='SCALE=   7.000 KM/PXL, NORTH= ******* DEG CLOCKWISE FROM UP'
---- Task: LABSWTCH -- User: FFM059 -- Wed Jun  1 11:35:07 1994 ----
---- Task: SIZE -- User: lwk -- Sun Jul 29 13:33:23 2012 ----
COMMENT='PICTURE SIZE SCALED BY      0.66667'
MAP_PROJECTION_TYPE='POLAR_ORTHOGRAPHIC'
COORDINATE_SYSTEM_NAME='PLANETOCENTRIC'
POSITIVE_LONGITUDE_DIRECTION='WEST'
A_AXIS_RADIUS=1830.0
B_AXIS_RADIUS=1830.0
C_AXIS_RADIUS=1815.300048828125
MAP_SCALE=10.49999968707562
MAP_RESOLUTION=3.041860947797375
CENTER_LATITUDE=90.0
CENTER_LONGITUDE=150.0
SPHERICAL_AZIMUTH=0.0
CARTESIAN_AZIMUTH=0.0
LINE_PROJECTION_OFFSET=331.8333432376385
SAMPLE_PROJECTION_OFFSET=165.1666716039181
 
************************************************************
size vt/tst1.dat a zoom=20 ioffset=(81,81)
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,   10,   10)
     OUTPUT SIZE=    280 X    280
 PICTURE SIZE SCALED BY     20*NL,     20*NS
***MAP_RESOLUTION not found
Computing resolution from scale
MAP property label updated
MAP history label updated
 SIZE task completed
label-list a
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File a ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                280 lines per band
                280 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Property: MAP ----
MAP_PROJECTION_TYPE='NORMAL_CYLINDRICAL'
COORDINATE_SYSTEM_NAME='PLANETOGRAPHIC'
POSITIVE_LONGITUDE_DIRECTION='WEST'
A_AXIS_RADIUS=6051.0
C_AXIS_RADIUS=6051.0
MAP_SCALE=0.66005
MAP_RESOLUTION=160.0029062949777
CENTER_LATITUDE=0.0
CENTER_LONGITUDE=124.998
LINE_PROJECTION_OFFSET=109.5
SAMPLE_PROJECTION_OFFSET=20109.5
MAP_PROJECTION_DESC=(
'An equal-area, cylindrical projection where the meridians on normal aspect', 
'are equally spaced straight lines.  Parallels on normal aspect are', 
'unequally spaced straight lines, closest near the poles, cutting meridians at', 
'right angles.  On normal aspect, true scale is along the equator, or along', 
'two parallels equidistant from the equator.  This is an orthographic projection', 
'of sphere onto a cylinder.  There is substantial shape and scale distortion', 
'near points 90 degress from the central line. Equations (10-1),(10-2),(10-6),(10-7) from USGS', 
'Paper 1395 (pp 79,80) were used.', 
'The value of the COORDINATE_SYSTEM_NAME item determines whether latitudes are', 
'planetographic or planetocentric;  if this  keyword is absent, then', 
'the default is planetographic.', 
'The direction of increasing longitude is defined by the POSITIVE_LONGITUDE_DIRECTION', 
'item;  if this keyword is absent, then the direction is determined by', 
'COORDINATE_SYSTEM_NAME:  it is East if the system is planetographic, West if it is', 
'planetocentric.', 
'NOTE: Portions of above text taken from U.S. Geological Survey Professional Paper 1395,', 
'second printing 1989, ''Map Projections - A Working Manual'' by John Snyder.', 
'See text for detailed formulae.')
---- Task: MAPTRANS -- User: DAA345 -- Tue May 22 09:52:22 1990 ----
MAP001='MAP2 LABEL GENERATED BY MAPLAB SUBROUTINE'
MAP002=' RADII=(  6051.0,  6051.0,  6051.0),LORANGLE= 0.0'
MAP003='*** SIMPLE CYLINDRICAL PROJECTION ***'
MAP004='AT LINE=     1.00 ,SAMPLE=  1001.00 ,LATI=  90.000 ,LONG=   0.000  W'
MAP005='SCALE AT EQUATOR =      8.000 PXLS/DEG OR  13.201 KM/PXL'
---- Task: SIZE -- User: LWK346 -- Fri Oct  4 13:15:51 1991 ----
COMMENT='PICTURE SIZE SCALED BY     -4'
---- Task: LABSWTCH -- User: LWK059 -- Sun Sep 19 12:02:41 1993 ----
---- Task: MAPTRAN3 -- User: LWK059 -- Tue Sep 21 09:46:55 1993 ----
MAP006='MAP2 LABEL GENERATED BY MAPLAB SUBROUTINE'
MAP007=' RADII=(  6051.0,  6051.0,  6051.0),LORANGLE= 0.0'
MAP008=' *** CYLINDRICAL (NORMAL)   PROJECTION ***'
MAP009='AT S=  1001.0,L=     1.0,LAT=0.0,LON=0.0 FOR S=1,LONG=  124.998'
MAP010='SCALE AT EQUATOR =      8.000 PXLS/DEG OR  13.201 KM/PXL'
---- Task: MPTESTF -- User: FFM059 -- Tue Jan  4 10:42:49 1994 ----
MAP_PROJECTION_TYPE='NORMAL_CYLINDRICAL'
A_AXIS_RADIUS=6051.0
C_AXIS_RADIUS=6051.0
MAP_SCALE=13.201
POSITIVE_LONGITUDE_DIRECTION='WEST'
CENTER_LATITUDE=0.0
CENTER_LONGITUDE=124.998
LINE_PROJECTION_OFFSET=1.0
SAMPLE_PROJECTION_OFFSET=1001.0
---- Task: SIZE -- User: lwk -- Sun Jul 29 13:33:24 2012 ----
COMMENT='PICTURE SIZE SCALED BY     20'
MAP_PROJECTION_TYPE='NORMAL_CYLINDRICAL'
COORDINATE_SYSTEM_NAME='PLANETOGRAPHIC'
POSITIVE_LONGITUDE_DIRECTION='WEST'
A_AXIS_RADIUS=6051.0
C_AXIS_RADIUS=6051.0
MAP_SCALE=0.66005
MAP_RESOLUTION=160.0029062949777
CENTER_LATITUDE=0.0
CENTER_LONGITUDE=124.998
LINE_PROJECTION_OFFSET=109.5
SAMPLE_PROJECTION_OFFSET=20109.5
 
************************************************************
size vt/tst10.dat a zoom=20 ioffset=(81,81)
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,   10,   10)
     OUTPUT SIZE=    280 X    280
 PICTURE SIZE SCALED BY     20*NL,     20*NS
MAP property label updated
MAP history label updated
 SIZE task completed
label-list a
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File a ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in HALF format from a SUN-SOLR host
                1 bands
                280 lines per band
                280 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Property: MAP ----
MAP_PROJECTION_TYPE='POINT_PERSPECTIVE'
COORDINATE_SYSTEM_NAME='PLANETOGRAPHIC'
A_AXIS_RADIUS=16.8
C_AXIS_RADIUS=10.5
FOCAL_LENGTH=800.0
FOCAL_PLANE_SCALE=100.0
NORTH_ANGLE=152.0
OPT_AXIS_INTERCEPT_LINE=-369.5
OPT_AXIS_INTERCEPT_SAMPLE=70.5
PLANET_CENTER_LINE=1070.5
PLANET_CENTER_SAMPLE=287.9
SUB_SPACECRAFT_LATITUDE=-35.4729
SUB_SPACECRAFT_LONGITUDE=344.335
TARGET_CENTER_DISTANCE=3656.65
MAP_PROJECTION_DESC=(
'Projection used to show target bodies as seen by the observer (camera).', 
'Central meridian and a particular parallel (if shown) are straight ', 
'lines.  Other meridians and parallels are usually arcs of circles or ellipses,', 
'but some may be parabolas or hyperbolas.  This projection is neither conformal', 
'nor equal-area.', 
'The algorithm is described in the JPL memo: ''Planet-to-Camera geometry for flight images''', 
'Gary Yagi, 8 Oct.1985', 
'The value of the COORDINATE_SYSTEM_NAME item determines whether latitudes are', 
'planetographic or planetocentric;  if this  keyword is absent, then', 
'the default is planetographic.', 
'The direction of increasing longitude is defined by the POSITIVE_LONGITUDE_DIRECTION', 
'item;  if this keyword is absent, then the direction is determined by', 
'COORDINATE_SYSTEM_NAME:  it is East if the system is planetographic, West if it is', 
'planetocentric.', 
'NOTE: Portions of above text taken from U.S. Geological Survey Professional Paper 1395,', 
'second printing 1989, ''Map Projections - A Working Manual'' by John Snyder.', 
'See text for detailed formulae.')
---- Task: NIMSCMM -- User: LWK059 -- Sat Sep 18 12:19:35 1993 ----
MAP001='MAP2 LABEL GENERATED BY MAPLAB SUBROUTINE'
MAP002=' RADII=(    16.8,    16.8,    10.5),LORANGLE= 0.0'
MAP003='*** PERSPECTIVE PROJECTION ***'
MAP004='S/C LAT=-35.4729 S/C LONG=344.3351 S/C LINE=   50.00'
MAP005='S/C SAMPLE=   10.87 S/C RANGE= .365665088E+04'
MAP006='FOCAL= 800.0000 PIXEL/MM=  5.0000 NORTH ANGLE=152.000'
MAP007='OPTICAL AXIS LINE= -22.000 SAMPLE=   0.000'
MINLAT=-74.6746
MINLON=185.019
MAXLAT=29.0725
MAXLON=44.8861
EDRS='edrs:N10202562302.e'
PROJECT='GLL'
INSTRMNT='NIMS'
PHASE='IDA_ENCOUNTER'
TARGET='IDA'
OBSNAME='IDA'
OBSNOTE='Ida hi-res'
PROD_ID='IDA_LWK001'
PRODNOTE='Ida double-scale Footprint'
POINTSRC='aacs:ck93240a.plt'
IKERNEL='SPICE$NIMS:NIMS_IKERNEL.DAT'
SPKERNEL='spice$ker:gll930823.bsp_1'
DPOINT=(6.000000e-03, 1.000000e-02)
CAL_TYPE='NOCAL'
DARK_TYP='NOUPDAT'
SATURATD='FLAGGED'
CAL_FILE='ndat:NIMSGS1_63HZ_HTH.CAL'
SOL_FIL=''
DSPK_FIL=''
DBM_FIL='DUMMY_DSPK.DAT'
PHOT_FNC='NOPCOR'
SLEW_TOL=3.000000e-03
ERAD_EXP=83.75
INS_MODE='FIXED MAP'
GAIN=0
CHOP_MOD=0
G_OFFSET=4
G_START=0
G_DELTA=0
G_STEPS=12
ERTDT_B=1993251
ERTDT_E=1993251
ERTTM_B=2006
ERTTM_E=2006
BEG_SCLK=(2025623, 3, 0)
BEG_SCET='1993 AUG 28 16:47:59'
END_SCLK=(2025623, 51, 0)
END_SCET='1993 AUG 28 16:48:31'
FILL_SIZ=0
FILL_NUM=0
DN_SDEV=9
GEO_SDEV=5
BINNING='FOOTPRNT'
THRESHLD=0.0
FPGRID=10
INCI_ANG=140.013
EMIS_ANG=89.9197
PHAS_ANG=51.2632
SUNAZ=155.99
SCAZ=-20.8896
MINRANGE=3509.68
MAXRANGE=3818.94
MINSUN_D=4.411018e+08
MAXSUN_D=4.411021e+08
B_SSCLAT=-34.7489
B_SSCLON=342.271
E_SSCLAT=-36.0934
E_SSCLON=346.294
T_FOCAL_=0.0
T_RADIAT=0.0
T_TELESC=0.0
T_GRATIN=0.0
T_CHOPPE=0.0
T_ELECTR=0.0
WAVLNTHS=(0.69449, 0.833435, 0.971753, 1.24962, 1.5279, 1.80822, 2.08949, 
2.36948, 2.65469, 2.93414, 3.21749, 3.50011, 3.78314, 4.06636, 4.35029, 
4.63229, 4.91466)
RAD_BASE=0.0
RAD_CONV=1.0
---- Task: COPY -- User: LWK059 -- Tue Sep 21 17:15:01 1993 ----
---- Task: MPTESTF -- User: LWK059 -- Tue Jan 18 15:56:41 1994 ----
MAP_PROJECTION_TYPE='POINT_PERSPECTIVE'
A_AXIS_RADIUS=16.8
C_AXIS_RADIUS=10.5
FOCAL_LENGTH=800.0
FOCAL_PLANE_SCALE=5.0
NORTH_ANGLE=152.0
OPT_AXIS_INTERCEPT_LINE=-22.0
OPT_AXIS_INTERCEPT_SAMPLE=0.0
PLANET_CENTER_LINE=50.0
PLANET_CENTER_SAMPLE=10.87
SUB_SPACECRAFT_LATITUDE=-35.4729
SUB_SPACECRAFT_LONGITUDE=344.335
SPACECRAFT_DISTANCE=3656.65
---- Task: SIZE -- User: lwk -- Sun Jul 29 13:33:25 2012 ----
COMMENT='PICTURE SIZE SCALED BY     20'
MAP_PROJECTION_TYPE='POINT_PERSPECTIVE'
COORDINATE_SYSTEM_NAME='PLANETOGRAPHIC'
A_AXIS_RADIUS=16.8
C_AXIS_RADIUS=10.5
FOCAL_LENGTH=800.0
FOCAL_PLANE_SCALE=100.0
NORTH_ANGLE=152.0
OPT_AXIS_INTERCEPT_LINE=-369.5
OPT_AXIS_INTERCEPT_SAMPLE=70.5
PLANET_CENTER_LINE=1070.5
PLANET_CENTER_SAMPLE=287.9
SUB_SPACECRAFT_LATITUDE=-35.4729
SUB_SPACECRAFT_LONGITUDE=344.335
TARGET_CENTER_DISTANCE=3656.65
 
************************************************************
  ush rm -f vt
  ush rm -f a
  ush rm -f b
  ush rm -f c
  ush rm -f d
  ush rm -f e
  ush rm -f f
  ush rm -f g
  ush rm -f h
  ush rm -f x
  ush rm -f y
  ush rm -f w
  ush rm -f z
  ush rm -f xx
let $echo="no"
exit
slogoff
$!-----------------------------------------------------------------------------
$ create tstsize.log_linux
tstsize
!!!!!!!!!!!!!!!!!!!!
! Interpolation mode
!!!!!!!!!!!!!!!!!!!!
gen a 3 3 ival=2 sinc=3 linc=3
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:35:20 2012
     Samp     1       3
   Line
      1       2   5   8
      2       5   8  11
      3       8  11  14
size a b nl=9 ns=9
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      9 X      9
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZE task completed
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:35:20 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:35:20 2012
     Samp     1       3       5       7       9
   Line
      1       0   1   2   3   4   5   6   7   8
      2       1   2   3   4   5   6   7   8   9
      3       2   3   4   5   6   7   8   9  10
      4       3   4   5   6   7   8   9  10  11
      5       4   5   6   7   8   9  10  11  12
      6       5   6   7   8   9  10  11  12  13
      7       6   7   8   9  10  11  12  13  14
      8       7   8   9  10  11  12  13  14  15
      9       8   9  10  11  12  13  14  15  16
size a c zoom=3
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      9 X      9
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZE task completed
difpic (b,c)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
size a c lzoom=3 szoom=3
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      9 X      9
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZE task completed
difpic (b,c)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
size b c nl=3 ns=3
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    9,    9)
     OUTPUT SIZE=      3 X      3
 PICTURE SIZE SCALED BY      0.33333*NL,      0.33333*NS
 SIZE task completed
list c
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:35:20 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:35:20 2012
     Samp     1       3
   Line
      1       2   5   8
      2       5   8  11
      3       8  11  14
size b d zoom=-3
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    9,    9)
     OUTPUT SIZE=      3 X      3
 PICTURE SIZE SCALED BY      0.33333*NL,      0.33333*NS
 SIZE task completed
difpic (c,d)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
size b d lzoom=-3 szoom=-3
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    9,    9)
     OUTPUT SIZE=      3 X      3
 PICTURE SIZE SCALED BY      0.33333*NL,      0.33333*NS
 SIZE task completed
difpic (c,d)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
size b c zoom=-3
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    9,    9)
     OUTPUT SIZE=      3 X      3
 PICTURE SIZE SCALED BY      0.33333*NL,      0.33333*NS
 SIZE task completed
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:35:20 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:35:20 2012
     Samp     1       3       5       7       9
   Line
      1       0   1   2   3   4   5   6   7   8
      2       1   2   3   4   5   6   7   8   9
      3       2   3   4   5   6   7   8   9  10
      4       3   4   5   6   7   8   9  10  11
      5       4   5   6   7   8   9  10  11  12
      6       5   6   7   8   9  10  11  12  13
      7       6   7   8   9  10  11  12  13  14
      8       7   8   9  10  11  12  13  14  15
      9       8   9  10  11  12  13  14  15  16
size b c zoom=-2.5
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    9,    9)
     OUTPUT SIZE=      3 X      3
 PICTURE SIZE SCALED BY      0.40000*NL,      0.40000*NS
 SIZE task completed
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:35:20 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:35:20 2012
     Samp     1       3       5       7       9
   Line
      1       0   1   2   3   4   5   6   7   8
      2       1   2   3   4   5   6   7   8   9
      3       2   3   4   5   6   7   8   9  10
      4       3   4   5   6   7   8   9  10  11
      5       4   5   6   7   8   9  10  11  12
      6       5   6   7   8   9  10  11  12  13
      7       6   7   8   9  10  11  12  13  14
      8       7   8   9  10  11  12  13  14  15
      9       8   9  10  11  12  13  14  15  16
size a b (1,1,11,11) ioffset=(2,2) zoom=3
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=     11 X     11
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZE task completed
list b 'zeroes
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:35:20 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:35:20 2012
     Samp     1       3       5       7       9      11
   Line
      1       0   0   0   0   0   0   0   0   0   0   0
      2       0   0   1   2   3   4   5   6   7   8   0
      3       0   1   2   3   4   5   6   7   8   9   0
      4       0   2   3   4   5   6   7   8   9  10   0
      5       0   3   4   5   6   7   8   9  10  11   0
      6       0   4   5   6   7   8   9  10  11  12   0
      7       0   5   6   7   8   9  10  11  12  13   0
      8       0   6   7   8   9  10  11  12  13  14   0
      9       0   7   8   9  10  11  12  13  14  15   0
     10       0   8   9  10  11  12  13  14  15  16   0
     11       0   0   0   0   0   0   0   0   0   0   0
size a b zoom=3 limits=(1,14)
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      9 X      9
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZE task completed
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:35:20 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:35:20 2012
     Samp     1       3       5       7       9
   Line
      1       1   1   2   3   4   5   6   7   8
      2       1   2   3   4   5   6   7   8   9
      3       2   3   4   5   6   7   8   9  10
      4       3   4   5   6   7   8   9  10  11
      5       4   5   6   7   8   9  10  11  12
      6       5   6   7   8   9  10  11  12  13
      7       6   7   8   9  10  11  12  13  14
      8       7   8   9  10  11  12  13  14  14
      9       8   9  10  11  12  13  14  14  14
size a b zoom=3	area=(1,1,1,3)
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    1,    3)
     OUTPUT SIZE=      3 X      9
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZE task completed
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:35:20 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:35:20 2012
     Samp     1       3       5       7       9
   Line
      1       1   2   3   4   5   6   7   8   9
      2       1   2   3   4   5   6   7   8   9
      3       1   2   3   4   5   6   7   8   9
size b c lzoom=1 szoom=-3 area=(1,1,1,9)
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    1,    9)
     OUTPUT SIZE=      1 X      3
 PICTURE SIZE SCALED BY      1*NL,     -3*NS
 SIZE task completed
list c
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:35:20 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:35:20 2012
     Samp     1       3
   Line
      1       2   5   8
size a b zoom=3	area=(1,1,3,1)
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    1)
     OUTPUT SIZE=      9 X      3
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZE task completed
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:35:20 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:35:20 2012
     Samp     1       3
   Line
      1       1   1   1
      2       2   2   2
      3       3   3   3
      4       4   4   4
      5       5   5   5
      6       6   6   6
      7       7   7   7
      8       8   8   8
      9       9   9   9
size b c lzoom=-3 szoom=1 area=(1,1,9,1)
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    9,    1)
     OUTPUT SIZE=      3 X      1
 PICTURE SIZE SCALED BY     -3*NL,      1*NS
 SIZE task completed
list c
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:35:20 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:35:20 2012
     Samp     1
   Line
      1       2
      2       5
      3       8
size a b lzoom=3 szoom=1
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      9 X      3
 PICTURE SIZE SCALED BY      3*NL,      1*NS
 SIZE task completed
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:35:20 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:35:20 2012
     Samp     1       3
   Line
      1       1   4   7
      2       2   5   8
      3       3   6   9
      4       4   7  10
      5       5   8  11
      6       6   9  12
      7       7  10  13
      8       8  11  14
      9       9  12  15
size b c lzoom=-3 szoom=1
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    9,    3)
     OUTPUT SIZE=      3 X      3
 PICTURE SIZE SCALED BY     -3*NL,      1*NS
 SIZE task completed
difpic (a,c)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
size a b lzoom=1 szoom=3
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      3 X      9
 PICTURE SIZE SCALED BY      1*NL,      3*NS
 SIZE task completed
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:35:20 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:35:20 2012
     Samp     1       3       5       7       9
   Line
      1       1   2   3   4   5   6   7   8   9
      2       4   5   6   7   8   9  10  11  12
      3       7   8   9  10  11  12  13  14  15
size b c lzoom=1 szoom=-3
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    9)
     OUTPUT SIZE=      3 X      3
 PICTURE SIZE SCALED BY      1*NL,     -3*NS
 SIZE task completed
difpic (a,c)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
size b c lzoom=3 szoom=-3
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    9)
     OUTPUT SIZE=      9 X      3
 PICTURE SIZE SCALED BY      3*NL,     -3*NS
 SIZE task completed
list c
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:35:20 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:35:20 2012
     Samp     1       3
   Line
      1       1   4   7
      2       2   5   8
      3       3   6   9
      4       4   7  10
      5       5   8  11
      6       6   9  12
      7       7  10  13
      8       8  11  14
      9       9  12  15
size c d lzoom=-3 szoom=3
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    9,    3)
     OUTPUT SIZE=      3 X      9
 PICTURE SIZE SCALED BY     -3*NL,      3*NS
 SIZE task completed
difpic (b,d)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
size a b zoom=3 'half scale=100
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      9 X      9
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZE task completed
list b
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:35:20 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:35:20 2012
     Samp       1     2     3     4     5     6     7     8     9
   Line
      1         0   100   200   300   400   500   600   700   800
      2       100   200   300   400   500   600   700   800   900
      3       200   300   400   500   600   700   800   900  1000
      4       300   400   500   600   700   800   900  1000  1100
      5       400   500   600   700   800   900  1000  1100  1200
      6       500   600   700   800   900  1000  1100  1200  1300
      7       600   700   800   900  1000  1100  1200  1300  1400
      8       700   800   900  1000  1100  1200  1300  1400  1500
      9       800   900  1000  1100  1200  1300  1400  1500  1600
size b c zoom=-3 'full scale=1000
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    9,    9)
     OUTPUT SIZE=      3 X      3
 PICTURE SIZE SCALED BY      0.33333*NL,      0.33333*NS
 SIZE task completed
list c
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:35:20 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:35:20 2012
     Samp            1          2          3
   Line
      1         200000     500000     800000
      2         500000     800000    1100000
      3         800000    1100000    1400000
size b d zoom=-3 'real scale=1000
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    9,    9)
     OUTPUT SIZE=      3 X      3
 PICTURE SIZE SCALED BY      0.33333*NL,      0.33333*NS
 SIZE task completed
list d
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:35:20 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:35:21 2012
     Samp             1           2           3
   Line
      1       2.000E+05   5.000E+05   8.000E+05
      2       5.000E+05   8.000E+05   1.100E+06
      3       8.000E+05   1.100E+06   1.400E+06
size d e zoom=3 'byte scale=0.00001
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      9 X      9
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZE task completed
list e
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:35:20 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:35:21 2012
     Samp     1       3       5       7       9
   Line
      1       0   1   2   3   4   5   6   7   8
      2       1   2   3   4   5   6   7   8   9
      3       2   3   4   5   6   7   8   9  10
      4       3   4   5   6   7   8   9  10  11
      5       4   5   6   7   8   9  10  11  12
      6       5   6   7   8   9  10  11  12  13
      7       6   7   8   9  10  11  12  13  14
      8       7   8   9  10  11  12  13  14  15
      9       8   9  10  11  12  13  14  15  16
size c e zoom=3 'half scale=0.001
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      9 X      9
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZE task completed
list e
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:35:20 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:35:21 2012
     Samp       1     2     3     4     5     6     7     8     9
   Line
      1         0   100   200   300   400   500   600   700   800
      2       100   200   300   400   500   600   700   800   900
      3       200   300   400   500   600   700   800   900  1000
      4       300   400   500   600   700   800   900  1000  1100
      5       400   500   600   700   800   900  1000  1100  1200
      6       500   600   700   800   900  1000  1100  1200  1300
      7       600   700   800   900  1000  1100  1200  1300  1400
      8       700   800   900  1000  1100  1200  1300  1400  1500
      9       800   900  1000  1100  1200  1300  1400  1500  1600
size b e zoom=-3 'byte scale=0.01
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    9,    9)
     OUTPUT SIZE=      3 X      3
 PICTURE SIZE SCALED BY      0.33333*NL,      0.33333*NS
 SIZE task completed
list e
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:35:20 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:35:21 2012
     Samp     1       3
   Line
      1       2   5   8
      2       5   8  11
      3       8  11  14
gen     out=d nl=3375 ns=3648 ival=-32768 linc=10 sinc=10 'half
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist    d
Beginning VICAR task hist
*** HIST version 13 Jul 2012 ***

Bin Width =      256.0
     -32768     6097   ***
     -32512    11764   ******
     -32256    12155   ******
     -32000    12194   ******
     -31744    11841   ******
     -31488    12078   ******
     -31232    11892   ******
     -30976    12027   ******
     -30720    12194   ******
     -30464    11969   ******
     -30208    11950   ******
     -29952    12020   ******
     -29696    11899   ******
     -29440    12194   ******
     -29184    12097   ******
     -28928    11822   ******
     -28672    12148   ******
     -28416    11771   ******
     -28160    12209   ******
     -27904    12675   *******
     -27648    12825   *******
     -27392    14001   *******
     -27136    14100   ********
     -26880    15327   ********
     -26624    16003   *********
     -26368    16025   *********
     -26112    17329   *********
     -25856    17300   *********
     -25600    18655   **********
     -25344    19331   ***********
     -25088    19225   **********
     -24832    20657   ***********
     -24576    20500   ***********
     -24320    21983   ************
     -24064    22659   ************
     -23808    22425   ************
     -23552    23985   *************
     -23296    23700   *************
     -23040    25311   **************
     -22784    25987   **************
     -22528    25625   **************
     -22272    27313   ***************
     -22016    26900   ***************
     -21760    28639   ****************
     -21504    29315   ****************
     -21248    28825   ****************
     -20992    30641   *****************
     -20736    30100   *****************
     -20480    31967   ******************
     -20224    32643   ******************
     -19968    32025   ******************
     -19712    33969   *******************
     -19456    33300   ******************
     -19200    35295   ********************
     -18944    35971   ********************
     -18688    35225   ********************
     -18432    37297   *********************
     -18176    36500   ********************
     -17920    38623   **********************
     -17664    39299   **********************
     -17408    38425   *********************
     -17152    40625   ***********************
     -16896    39700   **********************
     -16640    41951   ***********************
     -16384    42627   ************************
     -16128    41625   ***********************
     -15872    43953   *************************
     -15616    42900   ************************
     -15360    45279   *************************
     -15104    45955   **************************
     -14848    44825   *************************
     -14592    47281   **************************
     -14336    46100   **************************
     -14080    48607   ***************************
     -13824    49283   ****************************
     -13568    48025   ***************************
     -13312    50609   ****************************
     -13056    49300   ****************************
     -12800    51935   *****************************
     -12544    52611   *****************************
     -12288    51225   *****************************
     -12032    53937   ******************************
     -11776    52500   *****************************
     -11520    55263   *******************************
     -11264    55939   *******************************
     -11008    54425   *******************************
     -10752    57265   ********************************
     -10496    55700   *******************************
     -10240    58591   *********************************
      -9984    59267   *********************************
      -9728    57625   ********************************
      -9472    60593   **********************************
      -9216    58900   *********************************
      -8960    61919   ***********************************
      -8704    62595   ***********************************
      -8448    60825   **********************************
      -8192    63921   ************************************
      -7936    62100   ***********************************
      -7680    65247   *************************************
      -7424    65923   *************************************
      -7168    64025   ************************************
      -6912    67249   **************************************
      -6656    65300   *************************************
      -6400    68575   ***************************************
      -6144    69251   ***************************************
      -5888    67225   **************************************
      -5632    70577   ****************************************
      -5376    68500   ***************************************
      -5120    71903   ****************************************
      -4864    72579   *****************************************
      -4608    70425   ****************************************
      -4352    73905   ******************************************
      -4096    71700   ****************************************
      -3840    75231   ******************************************
      -3584    75907   *******************************************
      -3328    73625   *****************************************
      -3072    77233   ********************************************
      -2816    74900   ******************************************
      -2560    78559   ********************************************
      -2304    79235   *********************************************
      -2048    76825   *******************************************
      -1792    80561   *********************************************
      -1536    78100   ********************************************
      -1280    81887   **********************************************
      -1024    82563   ***********************************************
       -768    80025   *********************************************
       -512    83889   ***********************************************
       -256    81300   **********************************************
          0    85215   ************************************************
        256    85891   ************************************************
        512    83225   ***********************************************
        768    87217   *************************************************
       1024    84347   ************************************************
       1280    87750   **************************************************  1
       1536    87750   **************************************************  2
       1792    84375   ************************************************
       2048    87750   **************************************************
       2304    84375   ************************************************
       2560    87750   **************************************************
       2816    87750   **************************************************
       3072    84375   ************************************************
       3328    87750   **************************************************
       3584    84375   ************************************************
       3840    87399   *************************************************
       4096    86723   *************************************************
       4352    82750   ***********************************************
       4608    85397   ************************************************
       4864    81475   **********************************************
       5120    84071   ***********************************************
       5376    83395   ***********************************************
       5632    79550   *********************************************
       5888    82069   **********************************************
       6144    78275   ********************************************
       6400    80743   **********************************************
       6656    80067   *********************************************
       6912    76350   *******************************************
       7168    78741   ********************************************
       7424    75075   ******************************************
       7680    77415   ********************************************
       7936    76739   *******************************************
       8192    73150   *****************************************
       8448    75413   ******************************************
       8704    71875   ****************************************
       8960    74087   ******************************************
       9216    73411   *****************************************
       9472    69950   ***************************************
       9728    72085   *****************************************
       9984    68675   ***************************************
      10240    70759   ****************************************
      10496    70083   ***************************************
      10752    66750   **************************************
      11008    68757   ***************************************
      11264    65475   *************************************
      11520    67431   **************************************
      11776    66755   **************************************
      12032    63550   ************************************
      12288    65429   *************************************
      12544    62275   ***********************************
      12800    64103   ************************************
      13056    63427   ************************************
      13312    60350   **********************************
      13568    62101   ***********************************
      13824    59075   *********************************
      14080    60775   **********************************
      14336    60099   **********************************
      14592    57150   ********************************
      14848    58773   *********************************
      15104    55875   *******************************
      15360    57447   ********************************
      15616    56771   ********************************
      15872    53950   ******************************
      16128    55445   *******************************
      16384    52675   ******************************
      16640    54119   ******************************
      16896    53443   ******************************
      17152    50750   ****************************
      17408    52117   *****************************
      17664    49475   ****************************
      17920    50791   ****************************
      18176    50115   ****************************
      18432    47550   ***************************
      18688    48789   ***************************
      18944    46275   **************************
      19200    47463   ***************************
      19456    46787   **************************
      19712    44350   *************************
      19968    45461   *************************
      20224    43075   ************************
      20480    44135   *************************
      20736    43459   ************************
      20992    41150   ***********************
      21248    42133   ************************
      21504    39875   **********************
      21760    40807   ***********************
      22016    40131   **********************
      22272    37950   *********************
      22528    38805   **********************
      22784    36675   ********************
      23040    37479   *********************
      23296    36803   ********************
      23552    34750   *******************
      23808    35477   ********************
      24064    33475   *******************
      24320    34151   *******************
      24576    33475   *******************
      24832    31550   *****************
      25088    32149   ******************
      25344    30275   *****************
      25600    30823   *****************
      25856    30147   *****************
      26112    28350   ****************
      26368    28821   ****************
      26624    27075   ***************
      26880    27495   ***************
      27136    26819   ***************
      27392    25150   **************
      27648    25493   **************
      27904    23875   *************
      28160    24167   *************
      28416    23491   *************
      28672    21950   ************
      28928    22165   ************
      29184    20675   ***********
      29440    20839   ***********
      29696    20163   ***********
      29952    18750   **********
      30208    18837   **********
      30464    17475   *********
      30720    17511   *********
      30976    16835   *********
      31232    15550   ********
      31488    15509   ********
      31744    14275   ********
      32000    14183   ********
      32256    13507   *******
      32512    12350   *******
      32768     6175   ***

AVERAGE GRAY LEVEL=1752.829
STANDARD DEVIATION=14366.17
NUMBER ELEMENTS=  1231200
MIN. DN=    -32768
MAX. DN=     32762

size    d e size=(1,1,337,364)
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1, 3375, 3648)
     OUTPUT SIZE=    337 X    364
 PICTURE SIZE SCALED BY      0.09985*NL,      0.09978*NS
 SIZE task completed
hist    e
Beginning VICAR task hist
*** HIST version 13 Jul 2012 ***

Bin Width =      256.0
     -32678       48   **
     -32422       94   ****
     -32166      141   ******
     -31910      131   ******
     -31654      104   *****
     -31398      141   ******
     -31142      100   ****
     -30886      162   ********
     -30630      113   *****
     -30374      141   ******
     -30118       94   ****
     -29862      141   ******
     -29606      108   *****
     -29350      127   ******
     -29094      141   ******
     -28838       94   ****
     -28582      141   ******
     -28326       94   ****
     -28070      142   *******
     -27814       99   ****
     -27558      156   *******
     -27302      109   *****
     -27046      171   ********
     -26790      180   ********
     -26534      125   ******
     -26278      195   *********
     -26022      135   ******
     -25766      210   **********
     -25510      145   *******
     -25254      225   ***********
     -24998      155   *******
     -24742      240   ***********
     -24486      249   ************
     -24230      171   ********
     -23974      264   *************
     -23718      181   ********
     -23462      279   *************
     -23206      191   *********
     -22950      294   **************
     -22694      201   *********
     -22438      309   ***************
     -22182      318   ***************
     -21926      217   **********
     -21670      333   ****************
     -21414      227   ***********
     -21158      348   *****************
     -20902      237   ***********
     -20646      363   *****************
     -20390      247   ************
     -20134      378   ******************
     -19878      372   ******************
     -19622      278   *************
     -19366      402   *******************
     -19110      273   *************
     -18854      417   ********************
     -18598      283   *************
     -18342      432   *********************
     -18086      293   **************
     -17830      447   **********************
     -17574      426   *********************
     -17318      339   ****************
     -17062      471   ***********************
     -16806      319   ***************
     -16550      486   ************************
     -16294      329   ****************
     -16038      501   ************************
     -15782      339   ****************
     -15526      516   *************************
     -15270      481   ***********************
     -15014      399   *******************
     -14758      540   **************************
     -14502      365   ******************
     -14246      555   ***************************
     -13990      375   ******************
     -13734      570   ****************************
     -13478      385   *******************
     -13222      585   ****************************
     -12966      535   **************************
     -12710      460   **********************
     -12454      609   ******************************
     -12198      411   ********************
     -11942      624   ******************************
     -11686      421   ********************
     -11430      639   *******************************
     -11174      431   *********************
     -10918      654   ********************************
     -10662      589   *****************************
     -10406      521   *************************
     -10150      678   *********************************
      -9894      457   **********************
      -9638      693   **********************************
      -9382      467   ***********************
      -9126      708   ***********************************
      -8870      477   ***********************
      -8614      723   ***********************************
      -8358      643   *******************************
      -8102      582   ****************************
      -7846      747   ************************************
      -7590      503   ************************
      -7334      762   *************************************
      -7078      513   *************************
      -6822      777   **************************************
      -6566      530   **************************
      -6310      785   **************************************
      -6054      697   **********************************
      -5798      643   *******************************
      -5542      816   ****************************************
      -5286      549   ***************************
      -5030      831   *****************************************
      -4774      559   ***************************
      -4518      846   *****************************************
      -4262      584   ****************************
      -4006      846   *****************************************
      -3750      752   *************************************
      -3494      703   **********************************
      -3238      885   *******************************************
      -2982      595   *****************************
      -2726      900   ********************************************
      -2470      605   *****************************
      -2214      915   *********************************************
      -1958      638   *******************************
      -1702      907   ********************************************
      -1446      806   ***************************************
      -1190      764   *************************************
       -934      954   ***********************************************
       -678      641   *******************************
       -422      969   ***********************************************
       -166      651   ********************************
         90      984   ************************************************
        346      692   **********************************
        602      968   ***********************************************
        858      860   ******************************************
       1114      822   ****************************************
       1370     1011   **************************************************  1
       1626      674   *********************************
       1882     1011   **************************************************  2
       2138      674   *********************************
       2394     1011   **************************************************
       2650      696   **********************************
       2906      989   ************************************************
       3162      848   *****************************************
       3418      837   *****************************************
       3674     1001   *************************************************
       3930      678   *********************************
       4186      996   *************************************************
       4442      659   ********************************
       4698      981   ************************************************
       4954      656   ********************************
       5210      959   ***********************************************
       5466      799   ***************************************
       5722      791   ***************************************
       5978      941   **********************************************
       6234      624   ******************************
       6490      927   *********************************************
       6746      613   ******************************
       7002      912   *********************************************
       7258      603   *****************************
       7514      897   ********************************************
       7770      738   ************************************
       8026      737   ************************************
       8282      873   *******************************************
       8538      577   ****************************
       8794      858   ******************************************
       9050      567   ****************************
       9306      843   *****************************************
       9562      557   ***************************
       9818      828   ****************************************
      10074      677   *********************************
      10330      683   *********************************
      10586      804   ***************************************
      10842      531   **************************
      11098      789   ***************************************
      11354      521   *************************
      11610      774   **************************************
      11866      511   *************************
      12122      759   *************************************
      12378      616   ******************************
      12634      629   *******************************
      12890      743   ************************************
      13146      493   ************************
      13402      727   ***********************************
      13658      482   ***********************
      13914      711   ***********************************
      14170      471   ***********************
      14426      695   **********************************
      14682      555   ***************************
      14938      575   ****************************
      15194      666   ********************************
      15450      439   *********************
      15706      651   ********************************
      15962      429   *********************
      16218      636   *******************************
      16474      419   ********************
      16730      621   ******************************
      16986      495   ************************
      17242      520   *************************
      17498      597   *****************************
      17754      393   *******************
      18010      582   ****************************
      18266      383   ******************
      18522      567   ****************************
      18778      373   ******************
      19034      552   ***************************
      19290      434   *********************
      19546      466   ***********************
      19802      528   **************************
      20058      347   *****************
      20314      513   *************************
      20570      337   ****************
      20826      498   ************************
      21082      327   ****************
      21338      483   ***********************
      21594      373   ******************
      21850      412   ********************
      22106      459   **********************
      22362      301   **************
      22618      444   *********************
      22874      291   **************
      23130      429   *********************
      23386      281   *************
      23642      414   ********************
      23898      312   ***************
      24154      358   *****************
      24410      390   *******************
      24666      255   ************
      24922      375   ******************
      25178      245   ************
      25434      360   *****************
      25690      235   ***********
      25946      345   *****************
      26202      251   ************
      26458      304   ***************
      26714      321   ***************
      26970      209   **********
      27226      306   ***************
      27482      199   *********
      27738      291   **************
      27994      189   *********
      28250      276   *************
      28506      191   *********
      28762      249   ************
      29018      252   ************
      29274      163   ********
      29530      237   ***********
      29786      153   *******
      30042      222   **********
      30298      143   *******
      30554      207   **********
      30810      133   ******
      31066      192   *********
      31322      183   *********
      31578      117   *****
      31834      168   ********
      32090      107   *****
      32346      153   *******
      32602       97   ****

AVERAGE GRAY LEVEL=1752.829
STANDARD DEVIATION=14352.74
NUMBER ELEMENTS=    12266
MIN. DN=    -32678
MAX. DN=     32645

size    d f size=(300,200,200,200)
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1, 3375, 3648)
     OUTPUT SIZE=    200 X    200
 PICTURE SIZE SCALED BY      0.05926*NL,      0.05482*NS
 SIZE task completed
hist    f
Beginning VICAR task hist
*** HIST version 13 Jul 2012 ***

Bin Width =      256.0
     -32602       20   ***
     -32346       41   *******
     -32090       40   *******
     -31834       38   ******
     -31578       41   *******
     -31322       39   ******
     -31066       41   *******
     -30810       37   ******
     -30554       41   *******
     -30298       39   ******
     -30042       38   ******
     -29786       42   *******
     -29530       37   ******
     -29274       41   *******
     -29018       38   ******
     -28762       39   ******
     -28506       41   *******
     -28250       39   ******
     -27994       40   *******
     -27738       43   *******
     -27482       43   *******
     -27226       47   ********
     -26970       48   ********
     -26714       52   *********
     -26458       52   *********
     -26202       55   *********
     -25946       57   **********
     -25690       59   **********
     -25434       62   **********
     -25178       63   ***********
     -24922       66   ***********
     -24666       67   ***********
     -24410       69   ************
     -24154       74   *************
     -23898       73   ************
     -23642       75   *************
     -23386       79   **************
     -23130       80   **************
     -22874       86   ***************
     -22618       81   **************
     -22362       88   ***************
     -22106       87   ***************
     -21850       92   ****************
     -21594       93   ****************
     -21338       97   *****************
     -21082       97   *****************
     -20826      100   *****************
     -20570      100   *****************
     -20314      107   ******************
     -20058      103   ******************
     -19802      109   *******************
     -19546      110   *******************
     -19290      112   *******************
     -19034      115   ********************
     -18778      116   ********************
     -18522      119   *********************
     -18266      121   *********************
     -18010      123   *********************
     -17754      126   **********************
     -17498      126   **********************
     -17242      130   ***********************
     -16986      132   ***********************
     -16730      133   ***********************
     -16474      137   ************************
     -16218      135   ***********************
     -15962      142   *************************
     -15706      140   ************************
     -15450      146   *************************
     -15194      146   *************************
     -14938      148   **************************
     -14682      151   **************************
     -14426      153   ***************************
     -14170      153   ***************************
     -13914      160   ****************************
     -13658      157   ***************************
     -13402      163   ****************************
     -13146      163   ****************************
     -12890      166   *****************************
     -12634      167   *****************************
     -12378      170   ******************************
     -12122      172   ******************************
     -11866      173   ******************************
     -11610      177   *******************************
     -11354      179   *******************************
     -11098      179   *******************************
     -10842      183   ********************************
     -10586      185   ********************************
     -10330      187   *********************************
     -10074      189   *********************************
      -9818      190   *********************************
      -9562      195   **********************************
      -9306      193   **********************************
      -9050      200   ***********************************
      -8794      198   ***********************************
      -8538      202   ***********************************
      -8282      205   ************************************
      -8026      205   ************************************
      -7770      207   ************************************
      -7514      212   *************************************
      -7258      210   *************************************
      -7002      217   **************************************
      -6746      214   *************************************
      -6490      221   ***************************************
      -6234      220   ***************************************
      -5978      224   ***************************************
      -5722      225   ***************************************
      -5466      226   ****************************************
      -5210      229   ****************************************
      -4954      233   *****************************************
      -4698      232   *****************************************
      -4442      238   ******************************************
      -4186      237   ******************************************
      -3930      241   ******************************************
      -3674      241   ******************************************
      -3418      245   *******************************************
      -3162      247   *******************************************
      -2906      247   *******************************************
      -2650      252   ********************************************
      -2394      251   ********************************************
      -2138      255   *********************************************
      -1882      258   *********************************************
      -1626      258   *********************************************
      -1370      263   **********************************************
      -1114      263   **********************************************
       -858      265   **********************************************
       -602      269   ***********************************************
       -346      267   ***********************************************
        -90      275   ************************************************
        166      271   ************************************************
        422      278   *************************************************
        678      279   *************************************************
        934      281   *************************************************
       1190      280   *************************************************
       1446      281   *************************************************
       1702      280   *************************************************
       1958      280   *************************************************
       2214      283   **************************************************  1
       2470      280   *************************************************
       2726      280   *************************************************
       2982      282   **************************************************  2
       3238      280   *************************************************
       3494      281   *************************************************
       3750      281   *************************************************
       4006      278   *************************************************
       4262      277   *************************************************
       4518      272   ************************************************
       4774      274   ************************************************
       5030      267   ***********************************************
       5286      270   ***********************************************
       5542      265   **********************************************
       5798      263   **********************************************
       6054      261   **********************************************
       6310      259   *********************************************
       6566      258   *********************************************
       6822      254   *********************************************
       7078      253   ********************************************
       7334      251   ********************************************
       7590      246   *******************************************
       7846      249   ********************************************
       8102      245   *******************************************
       8358      241   ******************************************
       8614      241   ******************************************
       8870      235   *****************************************
       9126      238   ******************************************
       9382      233   *****************************************
       9638      233   *****************************************
       9894      228   ****************************************
      10150      227   ****************************************
      10406      226   ****************************************
      10662      223   ***************************************
      10918      219   **************************************
      11174      221   ***************************************
      11430      213   *************************************
      11686      217   **************************************
      11942      211   *************************************
      12198      212   *************************************
      12454      207   ************************************
      12710      206   ************************************
      12966      204   ************************************
      13222      201   ***********************************
      13478      200   ***********************************
      13734      198   ***********************************
      13990      194   **********************************
      14246      194   **********************************
      14502      191   *********************************
      14758      189   *********************************
      15014      187   *********************************
      15270      184   ********************************
      15526      184   ********************************
      15782      178   *******************************
      16038      181   ********************************
      16294      175   *******************************
      16550      174   ******************************
      16806      173   ******************************
      17062      169   *****************************
      17318      168   *****************************
      17574      167   *****************************
      17830      161   ****************************
      18086      163   ****************************
      18342      157   ***************************
      18598      160   ****************************
      18854      153   ***************************
      19110      153   ***************************
      19366      150   **************************
      19622      148   **************************
      19878      146   *************************
      20134      145   *************************
      20390      141   *************************
      20646      142   *************************
      20902      136   ************************
      21158      137   ************************
      21414      132   ***********************
      21670      133   ***********************
      21926      130   ***********************
      22182      125   **********************
      22438      127   **********************
      22694      122   *********************
      22950      121   *********************
      23206      118   ********************
      23462      116   ********************
      23718      115   ********************
      23974      114   ********************
      24230      109   *******************
      24486      110   *******************
      24742      103   ******************
      24998      106   ******************
      25254       99   *****************
      25510      101   *****************
      25766       97   *****************
      26022       96   *****************
      26278       93   ****************
      26534       91   ****************
      26790       89   ***************
      27046       88   ***************
      27302       82   **************
      27558       84   **************
      27814       79   **************
      28070       81   **************
      28326       75   *************
      28582       73   ************
      28838       73   ************
      29094       69   ************
      29350       68   ************
      29606       65   ***********
      29862       64   ***********
      30118       62   **********
      30374       59   **********
      30630       57   **********
      30886       55   *********
      31142       52   *********
      31398       54   *********
      31654       46   ********
      31910       49   ********
      32166       44   *******
      32422       43   *******
      32678        8   *

AVERAGE GRAY LEVEL=1752.829
STANDARD DEVIATION=14342.64
NUMBER ELEMENTS=     4000
MIN. DN=    -32602
MAX. DN=     32589

gen a 3 3 3 ival=2 sinc=3 linc=3
Beginning VICAR task gen
GEN Version 6
GEN task completed
size a b nb=2 zoom=2
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      6 X      6
 PICTURE SIZE SCALED BY      2*NL,      2*NS
 SIZE task completed
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:35:22 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:35:22 2012
 ***********
 Band =     1
 ***********
     Samp     1       3       5
   Line
      1       1   2   4   5   7   8
      2       2   4   5   7   8  10
      3       4   5   7   8  10  11
      4       5   7   8  10  11  13
      5       7   8  10  11  13  14
      6       8  10  11  13  14  16


 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:35:22 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:35:22 2012
 ***********
 Band =     2
 ***********
     Samp     1       3       5
   Line
      1       2   3   5   6   8   9
      2       3   5   6   8   9  11
      3       5   6   8   9  11  12
      4       6   8   9  11  12  14
      5       8   9  11  12  14  15
      6       9  11  12  14  15  17
size a b zoom=2
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      6 X      6
 PICTURE SIZE SCALED BY      2*NL,      2*NS
 SIZE task completed
list b nb=1 sb=3
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:35:22 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:35:22 2012
 ***********
 Band =     3
 ***********
     Samp     1       3       5
   Line
      1       3   4   6   7   9  10
      2       4   6   7   9  10  12
      3       6   7   9  10  12  13
      4       7   9  10  12  13  15
      5       9  10  12  13  15  16
      6      10  12  13  15  16  18
gen a 2 50000
Beginning VICAR task gen
GEN Version 6
GEN task completed
size a b zoom=2
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    2,50000)
     OUTPUT SIZE=      4 X 100000
 PICTURE SIZE SCALED BY      2*NL,      2*NS
 SIZE task completed
list b ss=99991 ns=10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:35:22 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:35:22 2012
     Samp 99991   99993   99995   99997   99999
   Line

      2      20  20  20  20  20  20  20  20  21  21
      3      57  57  58  58  59  59  59  60  60  60
      4      94  95  96  96  97  98  98  99  99 100
let $echo="no"
!!!!!!!!!!!!!!!!!!!!!!!!!
! Non-interpolation mode
!!!!!!!!!!!!!!!!!!!!!!!!!
gen a 3 3 ival=2 sinc=3 linc=3
Beginning VICAR task gen
GEN Version 6
GEN task completed
size a b zoom=3 'noin
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      9 X      9
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZE task completed
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:35:22 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:35:22 2012
     Samp     1       3       5       7       9
   Line
      1       2   2   2   5   5   5   8   8   8
      2       2   2   2   5   5   5   8   8   8
      3       2   2   2   5   5   5   8   8   8
      4       5   5   5   8   8   8  11  11  11
      5       5   5   5   8   8   8  11  11  11
      6       5   5   5   8   8   8  11  11  11
      7       8   8   8  11  11  11  14  14  14
      8       8   8   8  11  11  11  14  14  14
      9       8   8   8  11  11  11  14  14  14
size b c zoom=-3 'noin
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    9,    9)
     OUTPUT SIZE=      3 X      3
 PICTURE SIZE SCALED BY      0.33333*NL,      0.33333*NS
 SIZE task completed
difpic (a,c)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
size a c zoom=2.5 'noin
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      7 X      7
 PICTURE SIZE SCALED BY      2.50000*NL,      2.50000*NS
 SIZE task completed
list c
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:35:22 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:35:22 2012
     Samp     1       3       5       7
   Line
      1       2   2   5   5   5   8   8
      2       2   2   5   5   5   8   8
      3       5   5   8   8   8  11  11
      4       5   5   8   8   8  11  11
      5       5   5   8   8   8  11  11
      6       8   8  11  11  11  14  14
      7       8   8  11  11  11  14  14
size b c zoom=-2.5 'noin
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    9,    9)
     OUTPUT SIZE=      4 X      4
 PICTURE SIZE SCALED BY      0.40000*NL,      0.40000*NS
 SIZE task completed
list c
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:35:22 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:35:22 2012
     Samp     1       3
   Line
      1       2   2   5   8
      2       2   2   5   8
      3       5   5   8  11
      4       8   8  11  14
size a b (1,1,11,11) ioffset=(2,2) zoom=3 'noin
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=     11 X     11
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZE task completed
list b 'zeroes
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:35:22 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:35:22 2012
     Samp     1       3       5       7       9      11
   Line
      1       0   0   0   0   0   0   0   0   0   0   0
      2       0   2   2   2   5   5   5   8   8   8   0
      3       0   2   2   2   5   5   5   8   8   8   0
      4       0   2   2   2   5   5   5   8   8   8   0
      5       0   5   5   5   8   8   8  11  11  11   0
      6       0   5   5   5   8   8   8  11  11  11   0
      7       0   5   5   5   8   8   8  11  11  11   0
      8       0   8   8   8  11  11  11  14  14  14   0
      9       0   8   8   8  11  11  11  14  14  14   0
     10       0   8   8   8  11  11  11  14  14  14   0
     11       0   0   0   0   0   0   0   0   0   0   0
size a b zoom=3 limits=(1,14) 'noin
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      9 X      9
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZE task completed
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:35:22 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:35:22 2012
     Samp     1       3       5       7       9
   Line
      1       2   2   2   5   5   5   8   8   8
      2       2   2   2   5   5   5   8   8   8
      3       2   2   2   5   5   5   8   8   8
      4       5   5   5   8   8   8  11  11  11
      5       5   5   5   8   8   8  11  11  11
      6       5   5   5   8   8   8  11  11  11
      7       8   8   8  11  11  11  14  14  14
      8       8   8   8  11  11  11  14  14  14
      9       8   8   8  11  11  11  14  14  14
size a b lzoom=1 szoom=3 'noin
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      3 X      9
 PICTURE SIZE SCALED BY      1*NL,      3*NS
 SIZE task completed
size b c lzoom=3 szoom=-3 'noin
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    9)
     OUTPUT SIZE=      9 X      3
 PICTURE SIZE SCALED BY      3*NL,     -3*NS
 SIZE task completed
list c
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:35:22 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:35:22 2012
     Samp     1       3
   Line
      1       2   5   8
      2       2   5   8
      3       2   5   8
      4       5   8  11
      5       5   8  11
      6       5   8  11
      7       8  11  14
      8       8  11  14
      9       8  11  14
size c d lzoom=-3 szoom=3 'noin
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    9,    3)
     OUTPUT SIZE=      3 X      9
 PICTURE SIZE SCALED BY     -3*NL,      3*NS
 SIZE task completed
difpic (b,d)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
size a b zoom=3 'half scale=100 'noin
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      9 X      9
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZE task completed
list b
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:35:22 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:35:22 2012
     Samp       1     2     3     4     5     6     7     8     9
   Line
      1       200   200   200   500   500   500   800   800   800
      2       200   200   200   500   500   500   800   800   800
      3       200   200   200   500   500   500   800   800   800
      4       500   500   500   800   800   800  1100  1100  1100
      5       500   500   500   800   800   800  1100  1100  1100
      6       500   500   500   800   800   800  1100  1100  1100
      7       800   800   800  1100  1100  1100  1400  1400  1400
      8       800   800   800  1100  1100  1100  1400  1400  1400
      9       800   800   800  1100  1100  1100  1400  1400  1400
size b c zoom=-3 'full scale=1000 'noin
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    9,    9)
     OUTPUT SIZE=      3 X      3
 PICTURE SIZE SCALED BY      0.33333*NL,      0.33333*NS
 SIZE task completed
list c
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:35:22 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:35:22 2012
     Samp            1          2          3
   Line
      1         200000     500000     800000
      2         500000     800000    1100000
      3         800000    1100000    1400000
size b d zoom=-3 'real scale=1000 'noin
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    9,    9)
     OUTPUT SIZE=      3 X      3
 PICTURE SIZE SCALED BY      0.33333*NL,      0.33333*NS
 SIZE task completed
list d
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:35:22 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:35:22 2012
     Samp             1           2           3
   Line
      1       2.000E+05   5.000E+05   8.000E+05
      2       5.000E+05   8.000E+05   1.100E+06
      3       8.000E+05   1.100E+06   1.400E+06
size d e zoom=3 'byte scale=0.00001 'noin
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      9 X      9
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZE task completed
list e
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:35:22 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:35:22 2012
     Samp     1       3       5       7       9
   Line
      1       2   2   2   5   5   5   8   8   8
      2       2   2   2   5   5   5   8   8   8
      3       2   2   2   5   5   5   8   8   8
      4       5   5   5   8   8   8  11  11  11
      5       5   5   5   8   8   8  11  11  11
      6       5   5   5   8   8   8  11  11  11
      7       8   8   8  11  11  11  14  14  14
      8       8   8   8  11  11  11  14  14  14
      9       8   8   8  11  11  11  14  14  14
size c e zoom=3 'half scale=0.001 'noin
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      9 X      9
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZE task completed
list e
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:35:22 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:35:22 2012
     Samp       1     2     3     4     5     6     7     8     9
   Line
      1       200   200   200   500   500   500   800   800   800
      2       200   200   200   500   500   500   800   800   800
      3       200   200   200   500   500   500   800   800   800
      4       500   500   500   800   800   800  1100  1100  1100
      5       500   500   500   800   800   800  1100  1100  1100
      6       500   500   500   800   800   800  1100  1100  1100
      7       800   800   800  1100  1100  1100  1400  1400  1400
      8       800   800   800  1100  1100  1100  1400  1400  1400
      9       800   800   800  1100  1100  1100  1400  1400  1400
size b e zoom=-3 'byte scale=0.01 'noin
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    9,    9)
     OUTPUT SIZE=      3 X      3
 PICTURE SIZE SCALED BY      0.33333*NL,      0.33333*NS
 SIZE task completed
list e
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:35:22 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:35:22 2012
     Samp     1       3
   Line
      1       2   5   8
      2       5   8  11
      3       8  11  14
gen     out=g nl=3375 ns=3648 ival=-32768 linc=10 sinc=10 'half
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist    g
Beginning VICAR task hist
*** HIST version 13 Jul 2012 ***

Bin Width =      256.0
     -32768     6097   ***
     -32512    11764   ******
     -32256    12155   ******
     -32000    12194   ******
     -31744    11841   ******
     -31488    12078   ******
     -31232    11892   ******
     -30976    12027   ******
     -30720    12194   ******
     -30464    11969   ******
     -30208    11950   ******
     -29952    12020   ******
     -29696    11899   ******
     -29440    12194   ******
     -29184    12097   ******
     -28928    11822   ******
     -28672    12148   ******
     -28416    11771   ******
     -28160    12209   ******
     -27904    12675   *******
     -27648    12825   *******
     -27392    14001   *******
     -27136    14100   ********
     -26880    15327   ********
     -26624    16003   *********
     -26368    16025   *********
     -26112    17329   *********
     -25856    17300   *********
     -25600    18655   **********
     -25344    19331   ***********
     -25088    19225   **********
     -24832    20657   ***********
     -24576    20500   ***********
     -24320    21983   ************
     -24064    22659   ************
     -23808    22425   ************
     -23552    23985   *************
     -23296    23700   *************
     -23040    25311   **************
     -22784    25987   **************
     -22528    25625   **************
     -22272    27313   ***************
     -22016    26900   ***************
     -21760    28639   ****************
     -21504    29315   ****************
     -21248    28825   ****************
     -20992    30641   *****************
     -20736    30100   *****************
     -20480    31967   ******************
     -20224    32643   ******************
     -19968    32025   ******************
     -19712    33969   *******************
     -19456    33300   ******************
     -19200    35295   ********************
     -18944    35971   ********************
     -18688    35225   ********************
     -18432    37297   *********************
     -18176    36500   ********************
     -17920    38623   **********************
     -17664    39299   **********************
     -17408    38425   *********************
     -17152    40625   ***********************
     -16896    39700   **********************
     -16640    41951   ***********************
     -16384    42627   ************************
     -16128    41625   ***********************
     -15872    43953   *************************
     -15616    42900   ************************
     -15360    45279   *************************
     -15104    45955   **************************
     -14848    44825   *************************
     -14592    47281   **************************
     -14336    46100   **************************
     -14080    48607   ***************************
     -13824    49283   ****************************
     -13568    48025   ***************************
     -13312    50609   ****************************
     -13056    49300   ****************************
     -12800    51935   *****************************
     -12544    52611   *****************************
     -12288    51225   *****************************
     -12032    53937   ******************************
     -11776    52500   *****************************
     -11520    55263   *******************************
     -11264    55939   *******************************
     -11008    54425   *******************************
     -10752    57265   ********************************
     -10496    55700   *******************************
     -10240    58591   *********************************
      -9984    59267   *********************************
      -9728    57625   ********************************
      -9472    60593   **********************************
      -9216    58900   *********************************
      -8960    61919   ***********************************
      -8704    62595   ***********************************
      -8448    60825   **********************************
      -8192    63921   ************************************
      -7936    62100   ***********************************
      -7680    65247   *************************************
      -7424    65923   *************************************
      -7168    64025   ************************************
      -6912    67249   **************************************
      -6656    65300   *************************************
      -6400    68575   ***************************************
      -6144    69251   ***************************************
      -5888    67225   **************************************
      -5632    70577   ****************************************
      -5376    68500   ***************************************
      -5120    71903   ****************************************
      -4864    72579   *****************************************
      -4608    70425   ****************************************
      -4352    73905   ******************************************
      -4096    71700   ****************************************
      -3840    75231   ******************************************
      -3584    75907   *******************************************
      -3328    73625   *****************************************
      -3072    77233   ********************************************
      -2816    74900   ******************************************
      -2560    78559   ********************************************
      -2304    79235   *********************************************
      -2048    76825   *******************************************
      -1792    80561   *********************************************
      -1536    78100   ********************************************
      -1280    81887   **********************************************
      -1024    82563   ***********************************************
       -768    80025   *********************************************
       -512    83889   ***********************************************
       -256    81300   **********************************************
          0    85215   ************************************************
        256    85891   ************************************************
        512    83225   ***********************************************
        768    87217   *************************************************
       1024    84347   ************************************************
       1280    87750   **************************************************  1
       1536    87750   **************************************************  2
       1792    84375   ************************************************
       2048    87750   **************************************************
       2304    84375   ************************************************
       2560    87750   **************************************************
       2816    87750   **************************************************
       3072    84375   ************************************************
       3328    87750   **************************************************
       3584    84375   ************************************************
       3840    87399   *************************************************
       4096    86723   *************************************************
       4352    82750   ***********************************************
       4608    85397   ************************************************
       4864    81475   **********************************************
       5120    84071   ***********************************************
       5376    83395   ***********************************************
       5632    79550   *********************************************
       5888    82069   **********************************************
       6144    78275   ********************************************
       6400    80743   **********************************************
       6656    80067   *********************************************
       6912    76350   *******************************************
       7168    78741   ********************************************
       7424    75075   ******************************************
       7680    77415   ********************************************
       7936    76739   *******************************************
       8192    73150   *****************************************
       8448    75413   ******************************************
       8704    71875   ****************************************
       8960    74087   ******************************************
       9216    73411   *****************************************
       9472    69950   ***************************************
       9728    72085   *****************************************
       9984    68675   ***************************************
      10240    70759   ****************************************
      10496    70083   ***************************************
      10752    66750   **************************************
      11008    68757   ***************************************
      11264    65475   *************************************
      11520    67431   **************************************
      11776    66755   **************************************
      12032    63550   ************************************
      12288    65429   *************************************
      12544    62275   ***********************************
      12800    64103   ************************************
      13056    63427   ************************************
      13312    60350   **********************************
      13568    62101   ***********************************
      13824    59075   *********************************
      14080    60775   **********************************
      14336    60099   **********************************
      14592    57150   ********************************
      14848    58773   *********************************
      15104    55875   *******************************
      15360    57447   ********************************
      15616    56771   ********************************
      15872    53950   ******************************
      16128    55445   *******************************
      16384    52675   ******************************
      16640    54119   ******************************
      16896    53443   ******************************
      17152    50750   ****************************
      17408    52117   *****************************
      17664    49475   ****************************
      17920    50791   ****************************
      18176    50115   ****************************
      18432    47550   ***************************
      18688    48789   ***************************
      18944    46275   **************************
      19200    47463   ***************************
      19456    46787   **************************
      19712    44350   *************************
      19968    45461   *************************
      20224    43075   ************************
      20480    44135   *************************
      20736    43459   ************************
      20992    41150   ***********************
      21248    42133   ************************
      21504    39875   **********************
      21760    40807   ***********************
      22016    40131   **********************
      22272    37950   *********************
      22528    38805   **********************
      22784    36675   ********************
      23040    37479   *********************
      23296    36803   ********************
      23552    34750   *******************
      23808    35477   ********************
      24064    33475   *******************
      24320    34151   *******************
      24576    33475   *******************
      24832    31550   *****************
      25088    32149   ******************
      25344    30275   *****************
      25600    30823   *****************
      25856    30147   *****************
      26112    28350   ****************
      26368    28821   ****************
      26624    27075   ***************
      26880    27495   ***************
      27136    26819   ***************
      27392    25150   **************
      27648    25493   **************
      27904    23875   *************
      28160    24167   *************
      28416    23491   *************
      28672    21950   ************
      28928    22165   ************
      29184    20675   ***********
      29440    20839   ***********
      29696    20163   ***********
      29952    18750   **********
      30208    18837   **********
      30464    17475   *********
      30720    17511   *********
      30976    16835   *********
      31232    15550   ********
      31488    15509   ********
      31744    14275   ********
      32000    14183   ********
      32256    13507   *******
      32512    12350   *******
      32768     6175   ***

AVERAGE GRAY LEVEL=1752.829
STANDARD DEVIATION=14366.17
NUMBER ELEMENTS=  1231200
MIN. DN=    -32768
MAX. DN=     32762

size    g h size=(1,1,337,364) 'noin
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1, 3375, 3648)
     OUTPUT SIZE=    337 X    364
 PICTURE SIZE SCALED BY      0.09985*NL,      0.09978*NS
 SIZE task completed
hist    h
Beginning VICAR task hist
*** HIST version 13 Jul 2012 ***

Bin Width =      256.0
     -32768       48   **
     -32512      136   ******
     -32256       99   ****
     -32000      131   ******
     -31744      104   *****
     -31488      141   ******
     -31232       94   ****
     -30976      141   ******
     -30720      118   *****
     -30464      117   *****
     -30208      113   *****
     -29952      122   ******
     -29696      108   *****
     -29440      127   ******
     -29184      141   ******
     -28928       94   ****
     -28672      141   ******
     -28416       95   ****
     -28160      141   ******
     -27904       99   ****
     -27648      156   *******
     -27392      155   *******
     -27136      125   ******
     -26880      180   ********
     -26624      125   ******
     -26368      195   *********
     -26112      135   ******
     -25856      210   **********
     -25600      145   *******
     -25344      225   ***********
     -25088      190   *********
     -24832      205   **********
     -24576      249   ************
     -24320      171   ********
     -24064      264   *************
     -23808      181   ********
     -23552      279   *************
     -23296      191   *********
     -23040      294   **************
     -22784      292   **************
     -22528      218   **********
     -22272      318   ***************
     -22016      217   **********
     -21760      333   ****************
     -21504      227   ***********
     -21248      348   *****************
     -20992      237   ***********
     -20736      363   *****************
     -20480      327   ****************
     -20224      298   **************
     -19968      387   *******************
     -19712      263   *************
     -19456      402   *******************
     -19200      273   *************
     -18944      417   ********************
     -18688      283   *************
     -18432      432   *********************
     -18176      337   ****************
     -17920      403   *******************
     -17664      456   **********************
     -17408      309   ***************
     -17152      471   ***********************
     -16896      319   ***************
     -16640      486   ************************
     -16384      344   *****************
     -16128      486   ************************
     -15872      464   **********************
     -15616      391   *******************
     -15360      525   *************************
     -15104      355   *****************
     -14848      540   **************************
     -14592      365   ******************
     -14336      555   ***************************
     -14080      375   ******************
     -13824      570   ****************************
     -13568      474   ***********************
     -13312      496   ************************
     -13056      577   ****************************
     -12800      418   ********************
     -12544      609   ******************************
     -12288      411   ********************
     -12032      624   ******************************
     -11776      421   ********************
     -11520      639   *******************************
     -11264      601   *****************************
     -11008      484   ***********************
     -10752      663   ********************************
     -10496      447   **********************
     -10240      678   *********************************
      -9984      457   **********************
      -9728      693   **********************************
      -9472      481   ***********************
      -9216      694   **********************************
      -8960      610   ******************************
      -8704      590   *****************************
      -8448      715   ***********************************
      -8192      510   *************************
      -7936      747   ************************************
      -7680      503   ************************
      -7424      762   *************************************
      -7168      513   *************************
      -6912      777   **************************************
      -6656      609   ******************************
      -6400      706   **********************************
      -6144      801   ***************************************
      -5888      539   **************************
      -5632      816   ****************************************
      -5376      549   ***************************
      -5120      831   *****************************************
      -4864      607   ******************************
      -4608      798   ***************************************
      -4352      747   ************************************
      -4096      683   *********************************
      -3840      852   ******************************************
      -3584      603   *****************************
      -3328      885   *******************************************
      -3072      595   *****************************
      -2816      900   ********************************************
      -2560      617   ******************************
      -2304      903   ********************************************
      -2048      745   ************************************
      -1792      800   ***************************************
      -1536      880   *******************************************
      -1280      690   **********************************
      -1024      954   ***********************************************
       -768      641   *******************************
       -512      969   ***********************************************
       -256      651   ********************************
          0      984   ************************************************
        256      883   *******************************************
        512      777   **************************************
        768      990   ************************************************
       1024      692   **********************************
       1280     1011   **************************************************  1
       1536      674   *********************************
       1792     1011   **************************************************  2
       2048      718   ***********************************
       2304      967   ***********************************************
       2560      831   *****************************************
       2816      854   ******************************************
       3072      951   ***********************************************
       3328      734   ************************************
       3584     1010   *************************************************
       3840      669   *********************************
       4096      996   *************************************************
       4352      669   *********************************
       4608      971   ************************************************
       4864      735   ************************************
       5120      880   *******************************************
       5376      957   ***********************************************
       5632      633   *******************************
       5888      942   **********************************************
       6144      623   ******************************
       6400      927   *********************************************
       6656      656   ********************************
       6912      869   ******************************************
       7168      758   *************************************
       7424      742   ************************************
       7680      869   ******************************************
       7936      606   *****************************
       8192      873   *******************************************
       8448      577   ****************************
       8704      858   ******************************************
       8960      576   ****************************
       9216      834   *****************************************
       9472      642   *******************************
       9728      743   ************************************
       9984      756   *************************************
      10240      604   *****************************
      10496      804   ***************************************
      10752      531   **************************
      11008      789   ***************************************
      11264      521   *************************
      11520      774   **************************************
      11776      665   ********************************
      12032      605   *****************************
      12288      750   *************************************
      12544      495   ************************
      12800      735   ************************************
      13056      485   ***********************
      13312      720   ***********************************
      13568      484   ***********************
      13824      696   **********************************
      14080      548   ***************************
      14336      607   ******************************
      14592      660   ********************************
      14848      470   ***********************
      15104      666   ********************************
      15360      439   *********************
      15616      651   ********************************
      15872      429   *********************
      16128      636   *******************************
      16384      450   **********************
      16640      590   *****************************
      16896      612   ******************************
      17152      403   *******************
      17408      597   *****************************
      17664      393   *******************
      17920      582   ****************************
      18176      391   *******************
      18432      559   ***************************
      18688      455   **********************
      18944      470   ***********************
      19200      543   **************************
      19456      357   *****************
      19712      528   **************************
      19968      347   *****************
      20224      513   *************************
      20480      337   ****************
      20736      498   ************************
      20992      357   *****************
      21248      453   **********************
      21504      451   **********************
      21760      334   ****************
      22016      459   **********************
      22272      301   **************
      22528      444   *********************
      22784      291   **************
      23040      429   *********************
      23296      361   *****************
      23552      334   ****************
      23808      405   ********************
      24064      265   *************
      24320      390   *******************
      24576      255   ************
      24832      375   ******************
      25088      245   ************
      25344      360   *****************
      25600      264   *************
      25856      316   ***************
      26112      335   ****************
      26368      220   **********
      26624      321   ***************
      26880      209   **********
      27136      306   ***************
      27392      199   *********
      27648      291   **************
      27904      191   *********
      28160      274   *************
      28416      267   *************
      28672      173   ********
      28928      252   ************
      29184      163   ********
      29440      237   ***********
      29696      153   *******
      29952      222   **********
      30208      171   ********
      30464      179   ********
      30720      198   *********
      30976      127   ******
      31232      183   *********
      31488      117   *****
      31744      168   ********
      32000      107   *****
      32256      153   *******
      32512       99   ****
      32768       91   ****

AVERAGE GRAY LEVEL=1683.932
STANDARD DEVIATION=14361.92
NUMBER ELEMENTS=    12266
MIN. DN=    -32768
MAX. DN=     32742

size    g h size=(300,200,200,200) 'noin
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1, 3375, 3648)
     OUTPUT SIZE=    200 X    200
 PICTURE SIZE SCALED BY      0.05926*NL,      0.05482*NS
 SIZE task completed
hist    h
Beginning VICAR task hist
*** HIST version 13 Jul 2012 ***

Bin Width =      256.0
     -32768       19   ***
     -32512       40   ******
     -32256       38   ******
     -32000       41   *******
     -31744       37   ******
     -31488       39   ******
     -31232       40   ******
     -30976       38   ******
     -30720       40   ******
     -30464       39   ******
     -30208       37   ******
     -29952       42   *******
     -29696       36   ******
     -29440       42   *******
     -29184       37   ******
     -28928       39   ******
     -28672       40   ******
     -28416       39   ******
     -28160       40   ******
     -27904       43   *******
     -27648       42   *******
     -27392       48   ********
     -27136       46   *******
     -26880       53   *********
     -26624       53   *********
     -26368       52   ********
     -26112       60   **********
     -25856       55   *********
     -25600       66   ***********
     -25344       61   **********
     -25088       67   ***********
     -24832       66   ***********
     -24576       70   ************
     -24320       72   ************
     -24064       73   ************
     -23808       76   *************
     -23552       79   *************
     -23296       78   *************
     -23040       85   **************
     -22784       84   **************
     -22528       86   **************
     -22272       88   ***************
     -22016       91   ***************
     -21760       93   ***************
     -21504       99   *****************
     -21248       92   ***************
     -20992      105   ******************
     -20736       95   ****************
     -20480      111   *******************
     -20224      104   *****************
     -19968      109   ******************
     -19712      109   ******************
     -19456      111   *******************
     -19200      116   *******************
     -18944      115   *******************
     -18688      120   ********************
     -18432      119   ********************
     -18176      123   *********************
     -17920      125   *********************
     -17664      131   **********************
     -17408      125   *********************
     -17152      134   ***********************
     -16896      129   **********************
     -16640      139   ***********************
     -16384      138   ***********************
     -16128      138   ***********************
     -15872      143   ************************
     -15616      141   ************************
     -15360      150   *************************
     -15104      149   *************************
     -14848      149   *************************
     -14592      153   **************************
     -14336      152   **************************
     -14080      160   ***************************
     -13824      157   **************************
     -13568      163   ****************************
     -13312      161   ***************************
     -13056      166   ****************************
     -12800      167   ****************************
     -12544      177   ******************************
     -12288      164   ****************************
     -12032      179   ******************************
     -11776      170   *****************************
     -11520      182   *******************************
     -11264      185   *******************************
     -11008      176   ******************************
     -10752      190   ********************************
     -10496      178   ******************************
     -10240      198   **********************************
      -9984      189   ********************************
      -9728      195   *********************************
      -9472      192   ********************************
      -9216      198   **********************************
      -8960      199   **********************************
      -8704      204   ***********************************
      -8448      201   **********************************
      -8192      206   ***********************************
      -7936      206   ***********************************
      -7680      211   ************************************
      -7424      218   *************************************
      -7168      209   ***********************************
      -6912      219   *************************************
      -6656      214   ************************************
      -6400      223   **************************************
      -6144      231   ***************************************
      -5888      215   ************************************
      -5632      236   ****************************************
      -5376      218   *************************************
      -5120      242   *****************************************
      -4864      233   ****************************************
      -4608      235   ****************************************
      -4352      237   ****************************************
      -4096      238   ****************************************
      -3840      244   *****************************************
      -3584      245   ******************************************
      -3328      245   ******************************************
      -3072      247   ******************************************
      -2816      251   *******************************************
      -2560      251   *******************************************
      -2304      263   *********************************************
      -2048      248   ******************************************
      -1792      265   *********************************************
      -1536      254   *******************************************
      -1280      268   **********************************************
      -1024      268   **********************************************
       -768      264   *********************************************
       -512      272   **********************************************
       -256      267   *********************************************
          0      277   ***********************************************
        256      278   ***********************************************
        512      275   ***********************************************
        768      282   ************************************************
       1024      277   ***********************************************
       1280      283   ************************************************
       1536      284   ************************************************
       1792      276   ***********************************************
       2048      282   ************************************************
       2304      278   ***********************************************
       2560      281   ************************************************
       2816      292   **************************************************  1
       3072      267   *********************************************
       3328      291   **************************************************  2
       3584      269   **********************************************
       3840      286   *************************************************
       4096      281   ************************************************
       4352      265   *********************************************
       4608      280   ************************************************
       4864      258   ********************************************
       5120      278   ***********************************************
       5376      263   *********************************************
       5632      263   *********************************************
       5888      261   ********************************************
       6144      256   *******************************************
       6400      259   ********************************************
       6656      258   ********************************************
       6912      247   ******************************************
       7168      253   *******************************************
       7424      244   *****************************************
       7680      248   ******************************************
       7936      253   *******************************************
       8192      230   ***************************************
       8448      249   ******************************************
       8704      226   **************************************
       8960      245   ******************************************
       9216      236   ****************************************
       9472      225   **************************************
       9728      235   ****************************************
       9984      217   *************************************
      10240      234   ****************************************
      10496      222   **************************************
      10752      220   *************************************
      11008      219   *************************************
      11264      213   ************************************
      11520      217   *************************************
      11776      212   ************************************
      12032      208   ***********************************
      12288      208   ***********************************
      12544      204   ***********************************
      12800      204   ***********************************
      13056      207   ***********************************
      13312      192   ********************************
      13568      204   ***********************************
      13824      187   ********************************
      14080      199   **********************************
      14336      191   ********************************
      14592      186   *******************************
      14848      189   ********************************
      15104      179   ******************************
      15360      187   ********************************
      15616      178   ******************************
      15872      180   ******************************
      16128      174   *****************************
      16384      174   *****************************
      16640      171   *****************************
      16896      172   *****************************
      17152      164   ****************************
      17408      167   ****************************
      17664      160   ***************************
      17920      163   ****************************
      18176      164   ****************************
      18432      151   *************************
      18688      160   ***************************
      18944      145   ************************
      19200      157   **************************
      19456      148   *************************
      19712      144   ************************
      19968      146   *************************
      20224      136   ***********************
      20480      145   ************************
      20736      136   ***********************
      20992      137   ***********************
      21248      132   **********************
      21504      131   **********************
      21760      129   **********************
      22016      128   *********************
      22272      124   *********************
      22528      122   ********************
      22784      120   ********************
      23040      118   ********************
      23296      122   ********************
      23552      108   ******************
      23808      118   ********************
      24064      103   *****************
      24320      114   *******************
      24576      104   *****************
      24832      103   *****************
      25088      101   *****************
      25344       97   ****************
      25600      100   *****************
      25856       95   ****************
      26112       93   ***************
      26368       90   ***************
      26624       88   ***************
      26880       87   **************
      27136       84   **************
      27392       83   **************
      27648       78   *************
      27904       79   *************
      28160       75   ************
      28416       76   *************
      28672       70   ************
      28928       70   ************
      29184       66   ***********
      29440       67   ***********
      29696       63   **********
      29952       61   **********
      30208       58   *********
      30464       56   *********
      30720       56   *********
      30976       51   ********
      31232       52   ********
      31488       46   *******
      31744       48   ********
      32000       42   *******
      32256       44   *******
      32512       38   ******
      32768       20   ***

AVERAGE GRAY LEVEL=1621.578
STANDARD DEVIATION=14360.94
NUMBER ELEMENTS=     4000
MIN. DN=    -32768
MAX. DN=     32762

gen a 3 3 3 ival=2 sinc=3 linc=3
Beginning VICAR task gen
GEN Version 6
GEN task completed
size a b nb=2 zoom=2 'noin
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      6 X      6
 PICTURE SIZE SCALED BY      2*NL,      2*NS
 SIZE task completed
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:35:23 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:35:23 2012
 ***********
 Band =     1
 ***********
     Samp     1       3       5
   Line
      1       2   2   5   5   8   8
      2       2   2   5   5   8   8
      3       5   5   8   8  11  11
      4       5   5   8   8  11  11
      5       8   8  11  11  14  14
      6       8   8  11  11  14  14


 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:35:23 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:35:23 2012
 ***********
 Band =     2
 ***********
     Samp     1       3       5
   Line
      1       3   3   6   6   9   9
      2       3   3   6   6   9   9
      3       6   6   9   9  12  12
      4       6   6   9   9  12  12
      5       9   9  12  12  15  15
      6       9   9  12  12  15  15
size a b zoom=2 'noin
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      6 X      6
 PICTURE SIZE SCALED BY      2*NL,      2*NS
 SIZE task completed
list b nb=1 sb=3
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:35:23 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:35:23 2012
 ***********
 Band =     3
 ***********
     Samp     1       3       5
   Line
      1       4   4   7   7  10  10
      2       4   4   7   7  10  10
      3       7   7  10  10  13  13
      4       7   7  10  10  13  13
      5      10  10  13  13  16  16
      6      10  10  13  13  16  16
gen a 2 50000
Beginning VICAR task gen
GEN Version 6
GEN task completed
size a b zoom=2 'noin
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,    2,50000)
     OUTPUT SIZE=      4 X 100000
 PICTURE SIZE SCALED BY      2*NL,      2*NS
 SIZE task completed
list b ss=99991 ns=10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:35:23 2012
 Task:SIZE      User:lwk       Date_Time:Sun Jul 29 13:35:23 2012
     Samp 99991   99993   99995   99997   99999
   Line
      1      75  75  76  76  77  77  78  78  79  79
      2      75  75  76  76  77  77  78  78  79  79
      3      76  76  77  77  78  78  79  79  80  80
      4      76  76  77  77  78  78  79  79  80  80
let $echo="no"
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
****Test for fail on size 'noin ****
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
gen x 10 10
Beginning VICAR task gen
GEN Version 6
GEN task completed
list x
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:35:23 2012
     Samp     1       3       5       7       9
   Line
      1       0   1   2   3   4   5   6   7   8   9
      2       1   2   3   4   5   6   7   8   9  10
      3       2   3   4   5   6   7   8   9  10  11
      4       3   4   5   6   7   8   9  10  11  12
      5       4   5   6   7   8   9  10  11  12  13
      6       5   6   7   8   9  10  11  12  13  14
      7       6   7   8   9  10  11  12  13  14  15
      8       7   8   9  10  11  12  13  14  15  16
      9       8   9  10  11  12  13  14  15  16  17
     10       9  10  11  12  13  14  15  16  17  18
stretch x y func="in1+18"
Beginning VICAR task stretch
STRETCH version Oct 17 2002
*** USER SPECIFIED FUNCTION MODE ***
Function Stretch: FUNCTION = in1+18
list y
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:35:23 2012
 Task:STRETCH   User:lwk       Date_Time:Sun Jul 29 13:35:23 2012
     Samp     1       3       5       7       9
   Line
      1      18  19  20  21  22  23  24  25  26  27
      2      19  20  21  22  23  24  25  26  27  28
      3      20  21  22  23  24  25  26  27  28  29
      4      21  22  23  24  25  26  27  28  29  30
      5      22  23  24  25  26  27  28  29  30  31
      6      23  24  25  26  27  28  29  30  31  32
      7      24  25  26  27  28  29  30  31  32  33
      8      25  26  27  28  29  30  31  32  33  34
      9      26  27  28  29  30  31  32  33  34  35
     10      27  28  29  30  31  32  33  34  35  36
concat (x,y) z ns=20 'nost
Beginning VICAR task concat
list z sl=1 ss=1 nl=10 ns=10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Jul 29 13:35:23 2012
 Task:CONCAT    User:lwk       Date_Time:Sun Jul 29 13:35:23 2012
     Samp     1       3       5       7       9
   Line
      1       0   1   2   3   4   5   6   7   8   9
      2       1   2   3   4   5   6   7   8   9  10
      3       2   3   4   5   6   7   8   9  10  11
      4       3   4   5   6   7   8   9  10  11  12
      5       4   5   6   7   8   9  10  11  12  13
      6       5   6   7   8   9  10  11  12  13  14
      7       6   7   8   9  10  11  12  13  14  15
      8       7   8   9  10  11  12  13  14  15  16
      9       8   9  10  11  12  13  14  15  16  17
     10       9  10  11  12  13  14  15  16  17  18
let $echo="no"
 ==============================================  should FAIL here ===========
size z w zoom=10 'noin
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,   10,   20)
     OUTPUT SIZE=    100 X    200
 PICTURE SIZE SCALED BY     10*NL,     10*NS
 SIZE task completed
let $echo="no"
================================================= NO FAIL here ==============
size w xx zoom=10
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,  100,  200)
     OUTPUT SIZE=   1000 X   2000
 PICTURE SIZE SCALED BY     10*NL,     10*NS
 SIZE task completed
list w sl=1 ss=1 nl=10 ns=10
Beginning VICAR task list
 ** The specified window is all zero.
list xx sl=1 ss=1 nl=10 ns=10
Beginning VICAR task list
 ** The specified window is all zero.
let $echo="no"
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Test updating of map projection labels
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
label-list vt/m.dat
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File vt/m.dat ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in WORD format from a VAX-VMS host
                1 bands
                500 lines per band
                500 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: CONV12 -- User: DFS -- Thu May  3 11:15:53 1984 ----
LAB01=
'77                  1000    200010002000 I 2                          SC'
LAB02=
'VGR-1   FDS 16368.32   PICNO 0548J1-001   SCET 79.063 19:23:00         C'
LAB03=
'NA CAMERA  EXP 00360.0 MSEC  FILT 0(CLEAR )  LO GAIN  SCAN RATE  1:1   C'
LAB04=
'ERT 79.063 20:00:36   1/ 1 FULL    RES   VIDICON TEMP  -81.00 DEG C    C'
LAB05=
'IN/107971/07 OUT/******/**                 DSS #**   BIT SNR   34.133  C'
LAB06=
' 01865 A/9F000BC0 B/7082 C/C201 D/007F0000 ETLM/CC2BA1CA736ADEB02512F AC'
LAB07=
'NA OPCAL 00(*****.* MSEC) PIXAVG 192/0 OPERATIONAL MODE 4(BOTALT)     AC'
LAB08=
'CAM ECAL CYCLE BEAM  RESET OPEN  CLOSE FLOOD AEXPM  FIL G1 SHUT MODE  AC'
LAB09=
'NA   NO   READ  YES   NO    NO    NO    NO    NO    1 P  * NORMAL     AC'
LAB10=
'WA   NO   PREP  YES   YES   YES   YES   YES   NO    4 P  3 NORMAL     AC'
LAB11=
'ADESPIKE                                                             1HC'
LAB12=
'ADJACENT LINE PIXELS CHANGED    184. SAME LINE PIXELS CHANGED   124  1HC'
LAB13=
'RESSAR77 - FICOR77  MINSAT= 3765 NUMSAT=  7674                       1HC'
LAB14=
'TO CONVERT TO NANOWATTS/CM**2/STER/NM MULTIPLY DN VALUE BY    0.20224  C'
LAB15=
'TO CONVERT TO (I/F)*10000., MULTIPLY DN VALUE BY              1.00000  C'
LAB16=
'INSERT            09 11 82 19:09:07          JAM                      GC'
LAB17=
'GEOMA             09 13 83 16:49:05          JAM                      GC'
LAB18=
'HALF                                                                  PC'
LAB19=
'GEOMA                                                                 HC'
LAB20=
'INSERT            09 13 83 17:13:48          JAM                      GC'
LAB21=
'PGM=SWAPPER                                                           HL'
NLABS=21
---- Task: MAP -- User: FFM059 -- Wed Jun  1 11:34:14 1994 ----
MAP001='MAP2 LABEL GENERATED BY MAPLAB SUBROUTINE'
MAP002=' RADII=(  1830.0,  1830.0,  1815.3),LORANGLE= 0.0'
MAP003='*** POLAR ORTHOGRAPHIC PROJECTION ***'
MAP004='AT PROJ. CENTER L=   500.0,S=   250.0,LAT= 90.000,LONG=150.000  W'
MAP005='SCALE=   7.000 KM/PXL, NORTH= ******* DEG CLOCKWISE FROM UP'
---- Task: LABSWTCH -- User: FFM059 -- Wed Jun  1 11:35:07 1994 ----
 
************************************************************
size vt/m.dat a zoom=-2 area=(2,2,400,400)
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    2,    2,  400,  400)
     OUTPUT SIZE=    200 X    200
 PICTURE SIZE SCALED BY      0.50000*NL,      0.50000*NS
***MAP_RESOLUTION not found
Computing resolution from scale
MAP property label updated
MAP history label updated
 SIZE task completed
label-list a
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File a ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in HALF format from a X86-LINUX host
                1 bands
                200 lines per band
                200 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Property: MAP ----
MAP_PROJECTION_TYPE='POLAR_ORTHOGRAPHIC'
COORDINATE_SYSTEM_NAME='PLANETOCENTRIC'
POSITIVE_LONGITUDE_DIRECTION='WEST'
A_AXIS_RADIUS=1830.0
B_AXIS_RADIUS=1830.0
C_AXIS_RADIUS=1815.300048828125
MAP_SCALE=14.00000013209307
MAP_RESOLUTION=2.281395621331675
CENTER_LATITUDE=90.0
CENTER_LONGITUDE=150.0
SPHERICAL_AZIMUTH=0.0
CARTESIAN_AZIMUTH=0.0
LINE_PROJECTION_OFFSET=248.75
SAMPLE_PROJECTION_OFFSET=123.75
MAP_PROJECTION_DESC=(
'An azimuthal projection that is neither conformal nor equal-area.  All', 
'meridians and parallels are ellipses, circles, or straight lines.  This', 
'projection resembles a globe in appearance and has much distortion near the', 
'edges of the hemisphere shown.  There is no distortion at the center only,', 
'and directions from the center are true.  Radial scale factor decreases as', 
'distance increases from the center.  Scale in the direction of the lines', 
'of latitude is true in the polar aspect.', 
'In spherical form, the Equatorial aspect equations (20-3),(20-13) through', 
'(20-19) of USGS Paper 1395 (pp 149,150) were used.', 
'For the Oblate Spheroid, code from VICAR subroutine TRANV (q.v.) was used.', 
'The value of the COORDINATE_SYSTEM_NAME item determines whether latitudes are', 
'planetographic or planetocentric;  if this  keyword is absent, then', 
'the default is planetographic.', 
'The direction of increasing longitude is defined by the POSITIVE_LONGITUDE_DIRECTION', 
'item;  if this keyword is absent, then the direction is determined by', 
'COORDINATE_SYSTEM_NAME:  it is East if the system is planetographic, West if it is', 
'planetocentric.', 
'NOTE: Portions of above text taken from U.S. Geological Survey Professional Paper 1395,', 
'second printing 1989, ''Map Projections - A Working Manual'' by John Snyder.', 
'See text for detailed formulae.')
---- Task: CONV12 -- User: DFS -- Thu May  3 11:15:53 1984 ----
LAB01=
'77                  1000    200010002000 I 2                          SC'
LAB02=
'VGR-1   FDS 16368.32   PICNO 0548J1-001   SCET 79.063 19:23:00         C'
LAB03=
'NA CAMERA  EXP 00360.0 MSEC  FILT 0(CLEAR )  LO GAIN  SCAN RATE  1:1   C'
LAB04=
'ERT 79.063 20:00:36   1/ 1 FULL    RES   VIDICON TEMP  -81.00 DEG C    C'
LAB05=
'IN/107971/07 OUT/******/**                 DSS #**   BIT SNR   34.133  C'
LAB06=
' 01865 A/9F000BC0 B/7082 C/C201 D/007F0000 ETLM/CC2BA1CA736ADEB02512F AC'
LAB07=
'NA OPCAL 00(*****.* MSEC) PIXAVG 192/0 OPERATIONAL MODE 4(BOTALT)     AC'
LAB08=
'CAM ECAL CYCLE BEAM  RESET OPEN  CLOSE FLOOD AEXPM  FIL G1 SHUT MODE  AC'
LAB09=
'NA   NO   READ  YES   NO    NO    NO    NO    NO    1 P  * NORMAL     AC'
LAB10=
'WA   NO   PREP  YES   YES   YES   YES   YES   NO    4 P  3 NORMAL     AC'
LAB11=
'ADESPIKE                                                             1HC'
LAB12=
'ADJACENT LINE PIXELS CHANGED    184. SAME LINE PIXELS CHANGED   124  1HC'
LAB13=
'RESSAR77 - FICOR77  MINSAT= 3765 NUMSAT=  7674                       1HC'
LAB14=
'TO CONVERT TO NANOWATTS/CM**2/STER/NM MULTIPLY DN VALUE BY    0.20224  C'
LAB15=
'TO CONVERT TO (I/F)*10000., MULTIPLY DN VALUE BY              1.00000  C'
LAB16=
'INSERT            09 11 82 19:09:07          JAM                      GC'
LAB17=
'GEOMA             09 13 83 16:49:05          JAM                      GC'
LAB18=
'HALF                                                                  PC'
LAB19=
'GEOMA                                                                 HC'
LAB20=
'INSERT            09 13 83 17:13:48          JAM                      GC'
LAB21=
'PGM=SWAPPER                                                           HL'
NLABS=21
---- Task: MAP -- User: FFM059 -- Wed Jun  1 11:34:14 1994 ----
MAP001='MAP2 LABEL GENERATED BY MAPLAB SUBROUTINE'
MAP002=' RADII=(  1830.0,  1830.0,  1815.3),LORANGLE= 0.0'
MAP003='*** POLAR ORTHOGRAPHIC PROJECTION ***'
MAP004='AT PROJ. CENTER L=   500.0,S=   250.0,LAT= 90.000,LONG=150.000  W'
MAP005='SCALE=   7.000 KM/PXL, NORTH= ******* DEG CLOCKWISE FROM UP'
---- Task: LABSWTCH -- User: FFM059 -- Wed Jun  1 11:35:07 1994 ----
---- Task: SIZE -- User: lwk -- Sun Jul 29 13:35:24 2012 ----
COMMENT='PICTURE SIZE SCALED BY      0.50000'
MAP_PROJECTION_TYPE='POLAR_ORTHOGRAPHIC'
COORDINATE_SYSTEM_NAME='PLANETOCENTRIC'
POSITIVE_LONGITUDE_DIRECTION='WEST'
A_AXIS_RADIUS=1830.0
B_AXIS_RADIUS=1830.0
C_AXIS_RADIUS=1815.300048828125
MAP_SCALE=14.00000013209307
MAP_RESOLUTION=2.281395621331675
CENTER_LATITUDE=90.0
CENTER_LONGITUDE=150.0
SPHERICAL_AZIMUTH=0.0
CARTESIAN_AZIMUTH=0.0
LINE_PROJECTION_OFFSET=248.75
SAMPLE_PROJECTION_OFFSET=123.75
 
************************************************************
size vt/m.dat a zoom=-1.5 area=(2,2,400,400)
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    2,    2,  400,  400)
     OUTPUT SIZE=    266 X    266
 PICTURE SIZE SCALED BY      0.66667*NL,      0.66667*NS
***MAP_RESOLUTION not found
Computing resolution from scale
MAP property label updated
MAP history label updated
 SIZE task completed
label-list a
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File a ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in HALF format from a X86-LINUX host
                1 bands
                266 lines per band
                266 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Property: MAP ----
MAP_PROJECTION_TYPE='POLAR_ORTHOGRAPHIC'
COORDINATE_SYSTEM_NAME='PLANETOCENTRIC'
POSITIVE_LONGITUDE_DIRECTION='WEST'
A_AXIS_RADIUS=1830.0
B_AXIS_RADIUS=1830.0
C_AXIS_RADIUS=1815.300048828125
MAP_SCALE=10.49999978614543
MAP_RESOLUTION=3.04186091909675
CENTER_LATITUDE=90.0
CENTER_LONGITUDE=150.0
SPHERICAL_AZIMUTH=0.0
CARTESIAN_AZIMUTH=0.0
LINE_PROJECTION_OFFSET=331.8333432376385
SAMPLE_PROJECTION_OFFSET=165.1666716039181
MAP_PROJECTION_DESC=(
'An azimuthal projection that is neither conformal nor equal-area.  All', 
'meridians and parallels are ellipses, circles, or straight lines.  This', 
'projection resembles a globe in appearance and has much distortion near the', 
'edges of the hemisphere shown.  There is no distortion at the center only,', 
'and directions from the center are true.  Radial scale factor decreases as', 
'distance increases from the center.  Scale in the direction of the lines', 
'of latitude is true in the polar aspect.', 
'In spherical form, the Equatorial aspect equations (20-3),(20-13) through', 
'(20-19) of USGS Paper 1395 (pp 149,150) were used.', 
'For the Oblate Spheroid, code from VICAR subroutine TRANV (q.v.) was used.', 
'The value of the COORDINATE_SYSTEM_NAME item determines whether latitudes are', 
'planetographic or planetocentric;  if this  keyword is absent, then', 
'the default is planetographic.', 
'The direction of increasing longitude is defined by the POSITIVE_LONGITUDE_DIRECTION', 
'item;  if this keyword is absent, then the direction is determined by', 
'COORDINATE_SYSTEM_NAME:  it is East if the system is planetographic, West if it is', 
'planetocentric.', 
'NOTE: Portions of above text taken from U.S. Geological Survey Professional Paper 1395,', 
'second printing 1989, ''Map Projections - A Working Manual'' by John Snyder.', 
'See text for detailed formulae.')
---- Task: CONV12 -- User: DFS -- Thu May  3 11:15:53 1984 ----
LAB01=
'77                  1000    200010002000 I 2                          SC'
LAB02=
'VGR-1   FDS 16368.32   PICNO 0548J1-001   SCET 79.063 19:23:00         C'
LAB03=
'NA CAMERA  EXP 00360.0 MSEC  FILT 0(CLEAR )  LO GAIN  SCAN RATE  1:1   C'
LAB04=
'ERT 79.063 20:00:36   1/ 1 FULL    RES   VIDICON TEMP  -81.00 DEG C    C'
LAB05=
'IN/107971/07 OUT/******/**                 DSS #**   BIT SNR   34.133  C'
LAB06=
' 01865 A/9F000BC0 B/7082 C/C201 D/007F0000 ETLM/CC2BA1CA736ADEB02512F AC'
LAB07=
'NA OPCAL 00(*****.* MSEC) PIXAVG 192/0 OPERATIONAL MODE 4(BOTALT)     AC'
LAB08=
'CAM ECAL CYCLE BEAM  RESET OPEN  CLOSE FLOOD AEXPM  FIL G1 SHUT MODE  AC'
LAB09=
'NA   NO   READ  YES   NO    NO    NO    NO    NO    1 P  * NORMAL     AC'
LAB10=
'WA   NO   PREP  YES   YES   YES   YES   YES   NO    4 P  3 NORMAL     AC'
LAB11=
'ADESPIKE                                                             1HC'
LAB12=
'ADJACENT LINE PIXELS CHANGED    184. SAME LINE PIXELS CHANGED   124  1HC'
LAB13=
'RESSAR77 - FICOR77  MINSAT= 3765 NUMSAT=  7674                       1HC'
LAB14=
'TO CONVERT TO NANOWATTS/CM**2/STER/NM MULTIPLY DN VALUE BY    0.20224  C'
LAB15=
'TO CONVERT TO (I/F)*10000., MULTIPLY DN VALUE BY              1.00000  C'
LAB16=
'INSERT            09 11 82 19:09:07          JAM                      GC'
LAB17=
'GEOMA             09 13 83 16:49:05          JAM                      GC'
LAB18=
'HALF                                                                  PC'
LAB19=
'GEOMA                                                                 HC'
LAB20=
'INSERT            09 13 83 17:13:48          JAM                      GC'
LAB21=
'PGM=SWAPPER                                                           HL'
NLABS=21
---- Task: MAP -- User: FFM059 -- Wed Jun  1 11:34:14 1994 ----
MAP001='MAP2 LABEL GENERATED BY MAPLAB SUBROUTINE'
MAP002=' RADII=(  1830.0,  1830.0,  1815.3),LORANGLE= 0.0'
MAP003='*** POLAR ORTHOGRAPHIC PROJECTION ***'
MAP004='AT PROJ. CENTER L=   500.0,S=   250.0,LAT= 90.000,LONG=150.000  W'
MAP005='SCALE=   7.000 KM/PXL, NORTH= ******* DEG CLOCKWISE FROM UP'
---- Task: LABSWTCH -- User: FFM059 -- Wed Jun  1 11:35:07 1994 ----
---- Task: SIZE -- User: lwk -- Sun Jul 29 13:35:24 2012 ----
COMMENT='PICTURE SIZE SCALED BY      0.66667'
MAP_PROJECTION_TYPE='POLAR_ORTHOGRAPHIC'
COORDINATE_SYSTEM_NAME='PLANETOCENTRIC'
POSITIVE_LONGITUDE_DIRECTION='WEST'
A_AXIS_RADIUS=1830.0
B_AXIS_RADIUS=1830.0
C_AXIS_RADIUS=1815.300048828125
MAP_SCALE=10.49999978614543
MAP_RESOLUTION=3.04186091909675
CENTER_LATITUDE=90.0
CENTER_LONGITUDE=150.0
SPHERICAL_AZIMUTH=0.0
CARTESIAN_AZIMUTH=0.0
LINE_PROJECTION_OFFSET=331.8333432376385
SAMPLE_PROJECTION_OFFSET=165.1666716039181
 
************************************************************
size vt/tst1.dat a zoom=20 ioffset=(81,81)
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,   10,   10)
     OUTPUT SIZE=    280 X    280
 PICTURE SIZE SCALED BY     20*NL,     20*NS
***MAP_RESOLUTION not found
Computing resolution from scale
MAP property label updated
MAP history label updated
 SIZE task completed
label-list a
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File a ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                280 lines per band
                280 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Property: MAP ----
MAP_PROJECTION_TYPE='NORMAL_CYLINDRICAL'
COORDINATE_SYSTEM_NAME='PLANETOGRAPHIC'
POSITIVE_LONGITUDE_DIRECTION='WEST'
A_AXIS_RADIUS=6051.0
C_AXIS_RADIUS=6051.0
MAP_SCALE=0.6600500062277166
MAP_RESOLUTION=160.0029047853151
CENTER_LATITUDE=0.0
CENTER_LONGITUDE=124.998
LINE_PROJECTION_OFFSET=109.5
SAMPLE_PROJECTION_OFFSET=20109.5
MAP_PROJECTION_DESC=(
'An equal-area, cylindrical projection where the meridians on normal aspect', 
'are equally spaced straight lines.  Parallels on normal aspect are', 
'unequally spaced straight lines, closest near the poles, cutting meridians at', 
'right angles.  On normal aspect, true scale is along the equator, or along', 
'two parallels equidistant from the equator.  This is an orthographic projection', 
'of sphere onto a cylinder.  There is substantial shape and scale distortion', 
'near points 90 degress from the central line. Equations (10-1),(10-2),(10-6),(10-7) from USGS', 
'Paper 1395 (pp 79,80) were used.', 
'The value of the COORDINATE_SYSTEM_NAME item determines whether latitudes are', 
'planetographic or planetocentric;  if this  keyword is absent, then', 
'the default is planetographic.', 
'The direction of increasing longitude is defined by the POSITIVE_LONGITUDE_DIRECTION', 
'item;  if this keyword is absent, then the direction is determined by', 
'COORDINATE_SYSTEM_NAME:  it is East if the system is planetographic, West if it is', 
'planetocentric.', 
'NOTE: Portions of above text taken from U.S. Geological Survey Professional Paper 1395,', 
'second printing 1989, ''Map Projections - A Working Manual'' by John Snyder.', 
'See text for detailed formulae.')
---- Task: MAPTRANS -- User: DAA345 -- Tue May 22 09:52:22 1990 ----
MAP001='MAP2 LABEL GENERATED BY MAPLAB SUBROUTINE'
MAP002=' RADII=(  6051.0,  6051.0,  6051.0),LORANGLE= 0.0'
MAP003='*** SIMPLE CYLINDRICAL PROJECTION ***'
MAP004='AT LINE=     1.00 ,SAMPLE=  1001.00 ,LATI=  90.000 ,LONG=   0.000  W'
MAP005='SCALE AT EQUATOR =      8.000 PXLS/DEG OR  13.201 KM/PXL'
---- Task: SIZE -- User: LWK346 -- Fri Oct  4 13:15:51 1991 ----
COMMENT='PICTURE SIZE SCALED BY     -4'
---- Task: LABSWTCH -- User: LWK059 -- Sun Sep 19 12:02:41 1993 ----
---- Task: MAPTRAN3 -- User: LWK059 -- Tue Sep 21 09:46:55 1993 ----
MAP006='MAP2 LABEL GENERATED BY MAPLAB SUBROUTINE'
MAP007=' RADII=(  6051.0,  6051.0,  6051.0),LORANGLE= 0.0'
MAP008=' *** CYLINDRICAL (NORMAL)   PROJECTION ***'
MAP009='AT S=  1001.0,L=     1.0,LAT=0.0,LON=0.0 FOR S=1,LONG=  124.998'
MAP010='SCALE AT EQUATOR =      8.000 PXLS/DEG OR  13.201 KM/PXL'
---- Task: MPTESTF -- User: FFM059 -- Tue Jan  4 10:42:49 1994 ----
MAP_PROJECTION_TYPE='NORMAL_CYLINDRICAL'
A_AXIS_RADIUS=6051.0
C_AXIS_RADIUS=6051.0
MAP_SCALE=13.201
POSITIVE_LONGITUDE_DIRECTION='WEST'
CENTER_LATITUDE=0.0
CENTER_LONGITUDE=124.998
LINE_PROJECTION_OFFSET=1.0
SAMPLE_PROJECTION_OFFSET=1001.0
---- Task: SIZE -- User: lwk -- Sun Jul 29 13:35:24 2012 ----
COMMENT='PICTURE SIZE SCALED BY     20'
MAP_PROJECTION_TYPE='NORMAL_CYLINDRICAL'
COORDINATE_SYSTEM_NAME='PLANETOGRAPHIC'
POSITIVE_LONGITUDE_DIRECTION='WEST'
A_AXIS_RADIUS=6051.0
C_AXIS_RADIUS=6051.0
MAP_SCALE=0.6600500062277166
MAP_RESOLUTION=160.0029047853151
CENTER_LATITUDE=0.0
CENTER_LONGITUDE=124.998
LINE_PROJECTION_OFFSET=109.5
SAMPLE_PROJECTION_OFFSET=20109.5
 
************************************************************
size vt/tst10.dat a zoom=20 ioffset=(81,81)
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,   10,   10)
     OUTPUT SIZE=    280 X    280
 PICTURE SIZE SCALED BY     20*NL,     20*NS
MAP property label updated
MAP history label updated
 SIZE task completed
label-list a
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File a ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in HALF format from a X86-LINUX host
                1 bands
                280 lines per band
                280 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Property: MAP ----
MAP_PROJECTION_TYPE='POINT_PERSPECTIVE'
COORDINATE_SYSTEM_NAME='PLANETOGRAPHIC'
A_AXIS_RADIUS=16.8
C_AXIS_RADIUS=10.5
FOCAL_LENGTH=800.0
FOCAL_PLANE_SCALE=100.0
NORTH_ANGLE=152.0
OPT_AXIS_INTERCEPT_LINE=-369.5
OPT_AXIS_INTERCEPT_SAMPLE=70.5
PLANET_CENTER_LINE=1070.5
PLANET_CENTER_SAMPLE=287.9
SUB_SPACECRAFT_LATITUDE=-35.4729
SUB_SPACECRAFT_LONGITUDE=344.335
TARGET_CENTER_DISTANCE=3656.65
MAP_PROJECTION_DESC=(
'Projection used to show target bodies as seen by the observer (camera).', 
'Central meridian and a particular parallel (if shown) are straight ', 
'lines.  Other meridians and parallels are usually arcs of circles or ellipses,', 
'but some may be parabolas or hyperbolas.  This projection is neither conformal', 
'nor equal-area.', 
'The algorithm is described in the JPL memo: ''Planet-to-Camera geometry for flight images''', 
'Gary Yagi, 8 Oct.1985', 
'The value of the COORDINATE_SYSTEM_NAME item determines whether latitudes are', 
'planetographic or planetocentric;  if this  keyword is absent, then', 
'the default is planetographic.', 
'The direction of increasing longitude is defined by the POSITIVE_LONGITUDE_DIRECTION', 
'item;  if this keyword is absent, then the direction is determined by', 
'COORDINATE_SYSTEM_NAME:  it is East if the system is planetographic, West if it is', 
'planetocentric.', 
'NOTE: Portions of above text taken from U.S. Geological Survey Professional Paper 1395,', 
'second printing 1989, ''Map Projections - A Working Manual'' by John Snyder.', 
'See text for detailed formulae.')
---- Task: NIMSCMM -- User: LWK059 -- Sat Sep 18 12:19:35 1993 ----
MAP001='MAP2 LABEL GENERATED BY MAPLAB SUBROUTINE'
MAP002=' RADII=(    16.8,    16.8,    10.5),LORANGLE= 0.0'
MAP003='*** PERSPECTIVE PROJECTION ***'
MAP004='S/C LAT=-35.4729 S/C LONG=344.3351 S/C LINE=   50.00'
MAP005='S/C SAMPLE=   10.87 S/C RANGE= .365665088E+04'
MAP006='FOCAL= 800.0000 PIXEL/MM=  5.0000 NORTH ANGLE=152.000'
MAP007='OPTICAL AXIS LINE= -22.000 SAMPLE=   0.000'
MINLAT=-74.6746
MINLON=185.019
MAXLAT=29.0725
MAXLON=44.8861
EDRS='edrs:N10202562302.e'
PROJECT='GLL'
INSTRMNT='NIMS'
PHASE='IDA_ENCOUNTER'
TARGET='IDA'
OBSNAME='IDA'
OBSNOTE='Ida hi-res'
PROD_ID='IDA_LWK001'
PRODNOTE='Ida double-scale Footprint'
POINTSRC='aacs:ck93240a.plt'
IKERNEL='SPICE$NIMS:NIMS_IKERNEL.DAT'
SPKERNEL='spice$ker:gll930823.bsp_1'
DPOINT=(6.000000e-03, 1.000000e-02)
CAL_TYPE='NOCAL'
DARK_TYP='NOUPDAT'
SATURATD='FLAGGED'
CAL_FILE='ndat:NIMSGS1_63HZ_HTH.CAL'
SOL_FIL=''
DSPK_FIL=''
DBM_FIL='DUMMY_DSPK.DAT'
PHOT_FNC='NOPCOR'
SLEW_TOL=3.000000e-03
ERAD_EXP=83.75
INS_MODE='FIXED MAP'
GAIN=0
CHOP_MOD=0
G_OFFSET=4
G_START=0
G_DELTA=0
G_STEPS=12
ERTDT_B=1993251
ERTDT_E=1993251
ERTTM_B=2006
ERTTM_E=2006
BEG_SCLK=(2025623, 3, 0)
BEG_SCET='1993 AUG 28 16:47:59'
END_SCLK=(2025623, 51, 0)
END_SCET='1993 AUG 28 16:48:31'
FILL_SIZ=0
FILL_NUM=0
DN_SDEV=9
GEO_SDEV=5
BINNING='FOOTPRNT'
THRESHLD=0.0
FPGRID=10
INCI_ANG=140.013
EMIS_ANG=89.9197
PHAS_ANG=51.2632
SUNAZ=155.99
SCAZ=-20.8896
MINRANGE=3509.68
MAXRANGE=3818.94
MINSUN_D=4.411018e+08
MAXSUN_D=4.411021e+08
B_SSCLAT=-34.7489
B_SSCLON=342.271
E_SSCLAT=-36.0934
E_SSCLON=346.294
T_FOCAL_=0.0
T_RADIAT=0.0
T_TELESC=0.0
T_GRATIN=0.0
T_CHOPPE=0.0
T_ELECTR=0.0
WAVLNTHS=(0.69449, 0.833435, 0.971753, 1.24962, 1.5279, 1.80822, 2.08949, 
2.36948, 2.65469, 2.93414, 3.21749, 3.50011, 3.78314, 4.06636, 4.35029, 
4.63229, 4.91466)
RAD_BASE=0.0
RAD_CONV=1.0
---- Task: COPY -- User: LWK059 -- Tue Sep 21 17:15:01 1993 ----
---- Task: MPTESTF -- User: LWK059 -- Tue Jan 18 15:56:41 1994 ----
MAP_PROJECTION_TYPE='POINT_PERSPECTIVE'
A_AXIS_RADIUS=16.8
C_AXIS_RADIUS=10.5
FOCAL_LENGTH=800.0
FOCAL_PLANE_SCALE=5.0
NORTH_ANGLE=152.0
OPT_AXIS_INTERCEPT_LINE=-22.0
OPT_AXIS_INTERCEPT_SAMPLE=0.0
PLANET_CENTER_LINE=50.0
PLANET_CENTER_SAMPLE=10.87
SUB_SPACECRAFT_LATITUDE=-35.4729
SUB_SPACECRAFT_LONGITUDE=344.335
SPACECRAFT_DISTANCE=3656.65
---- Task: SIZE -- User: lwk -- Sun Jul 29 13:35:24 2012 ----
COMMENT='PICTURE SIZE SCALED BY     20'
MAP_PROJECTION_TYPE='POINT_PERSPECTIVE'
COORDINATE_SYSTEM_NAME='PLANETOGRAPHIC'
A_AXIS_RADIUS=16.8
C_AXIS_RADIUS=10.5
FOCAL_LENGTH=800.0
FOCAL_PLANE_SCALE=100.0
NORTH_ANGLE=152.0
OPT_AXIS_INTERCEPT_LINE=-369.5
OPT_AXIS_INTERCEPT_SAMPLE=70.5
PLANET_CENTER_LINE=1070.5
PLANET_CENTER_SAMPLE=287.9
SUB_SPACECRAFT_LATITUDE=-35.4729
SUB_SPACECRAFT_LONGITUDE=344.335
TARGET_CENTER_DISTANCE=3656.65
 
************************************************************
  ush rm -f vt
  ush rm -f a
  ush rm -f b
  ush rm -f c
  ush rm -f d
  ush rm -f e
  ush rm -f f
  ush rm -f g
  ush rm -f h
  ush rm -f x
  ush rm -f y
  ush rm -f w
  ush rm -f z
  ush rm -f xx
let $echo="no"
exit
slogoff
$ Return
$!#############################################################################
