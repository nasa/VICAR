$!****************************************************************************
$!
$! Build proc for MIPL module sizemgn
$! VPACK Version 1.9, Friday, April 05, 2013, 14:18:56
$!
$! Execute by entering:		$ @sizemgn
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
$ write sys$output "*** module sizemgn ***"
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
$ write sys$output "Invalid argument given to sizemgn.com file -- ", primary
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
$   if F$SEARCH("sizemgn.imake") .nes. ""
$   then
$      vimake sizemgn
$      purge sizemgn.bld
$   else
$      if F$SEARCH("sizemgn.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake sizemgn
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @sizemgn.bld "STD"
$   else
$      @sizemgn.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create sizemgn.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack sizemgn.com -mixed -
	-s sizemgn.f getsize.f update_label.f snoin.f sintrp.f sread.f -
	   magnify.f compress.f mgnsize.f lookup.f slookup.inc -
	-i sizemgn.imake -
	-p sizemgn.pdf -
	-t tstsizemgn.pdf tstsizemgn.log_solos
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create sizemgn.f
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
C           SIZEMGN  IN  OUT  (1,1,NLO,NSO)  user-parameters...
C       or  SIZEMGN  IN  OUT  ZOOM=2.5  user-parameters...
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
	INTEGER*4 IB,SB,EB        !variables for 3rd dimension
      REAL*4 ZOOML,ZOOMS	!Floating point zoom factors
      INTEGER*4 LZOOM,IZOOM	!Integer zoom factors (=0 if not integer)
      INTEGER*4 ILO,IHI		!Min and max DN limits for output image
      INTEGER*4 LFLAG		!=1 if need to check for ILO,IHI saturation
      REAL*4 GSCALE		!Output DN scale factor

      COMMON/C1/TBL(0:255),TBLH(-32768:32767),TABLE(0:255)
      LOGICAL*1 TBL
      INTEGER*2 TBLH
      REAL*4 TABLE		!Scale DN to Volts (Magellan only)

c      COMMON/C2/RBUF(100000),BUF(100000,2),OBUF(100000)
c      COMMON/C2/SAMP(100000),WGHT(100000)
      INTEGER*4 SAMP(100000)
      REAL*4 BUF(100000),RBUF(100000),OBUF(100000),WGHT(100000)

      INTEGER*4 I,L,N,IND,INCODE
      REAL*4 SLOPE,OFFSET
      LOGICAL*4 XVPTST,INTERP
      CHARACTER*4 FMT(4)/'BYTE','HALF','FULL','REAL'/
  
      CALL XVMESSAGE(' SIZEMGN version 06 Jun 2012',' ')
C     ....Open input picture
      CALL IPOPEN(iunit,icode,sb,eb)
C     ....Determine zoom factor and size of input and output images
      CALL GETSIZE(IUNIT,interp,sli,ssi,nli,nsi,slo,sso,nlo,nso,
     &		nlout,nsout,zooml,zooms,lzoom,izoom)
C     ....Open output picture
      CALL OPOPEN(INTERP,ZOOML,ZOOMS,LZOOM,IZOOM,NLOUT,NSOUT,
     &	sb,eb,ICODE,ocode,ounit)
C     ....Get scale and limits parameters
      CALL GETSCALE(ICODE,OCODE,gscale,ilo,ihi,lflag)
C     ....If format conversion is required, reopen input
      INCODE = MAX0(ICODE,OCODE)
      IF (INTERP) INCODE=4
c       force all input to be converted to real on the fly
         CALL XVCLOSE(IUNIT,IND,' ')
         CALL XVOPEN(IUNIT,IND,'OPEN_ACT','SA','IO_ACT','SA',
     &          'I_FORMAT',FMT(ICODE),'U_FORMAT',FMT(4),' ')            !FMT(INCODE),' ')

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
	print *,'before lookup'
      IF (XVPTST('LOOKUP')) THEN
         CALL LOOKUP(INTERP,nsi,nso,buf,rbuf)		!Table look-up
      ELSE IF (XVPTST('VOLTS')) THEN		!Do calculations in volts
         CALL DTVTABLE(slope,offset,table)	!Compute DN-to-volts table
         CALL SMGN(INTERP,TABLE,SLOPE,OFFSET,NSI,NSO,
     &		buf,rbuf)
      ELSE IF (INTERP) THEN
         CALL SINTRP(ib,NSO,buf,rbuf,obuf,samp,wght) !Interpolation
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

  100 CALL XVMESSAGE(' SIZEMGN task completed',' ')
      RETURN

      CALL XVMESSAGE('??E - SIZEMGN task cancelled',' ')
      CALL ABEND
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Open input image and determine data format
C
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
      CALL XVGET(IUNIT,ind,'FORMAT',format,'ORG',org,' ')	!Determine data format
      call xvbands( sb, nb, nbi)
      ! nbi is from input label
      ! nb is from param NB or BANDS, whichever is non-zero;  else zero
      if (nbi.gt.1) then
        if (org.eq.'BIP') then
          CALL XVMESSAGE(
     &  '??E - BIP files not supported, use program TRAN to convert to BSQ',
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
     &	sb,eb,ICODE,ocode,ounit)
      IMPLICIT NONE
      LOGICAL*4 INTERP
      REAL*4 ZOOML,ZOOMS
      INTEGER*4 LZOOM,IZOOM,NLOUT,NSOUT,ICODE,OCODE,OUNIT

      INTEGER*4 I,IND,NCHAR,OUTCODE,sb,eb,nb
      REAL*4 F1,F2
      LOGICAL*4 XVPTST
      CHARACTER*72 LAB
      CHARACTER*4 FMT(4)/'BYTE','HALF','FULL','REAL'/
      CHARACTER*80 MSG
      byte byt(80)
      DATA LAB /' '/                    !initialize character buffer
C  100 FORMAT(' Input data format=',A4,'  Output data format=',A4)
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
     & 'U_FORMAT',FMT(4),' ')                           !,FMT(OUTCODE),' ')

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
      SUBROUTINE GETSCALE(ICODE,OCODE,gscale,ilo,ihi,lflag)
	IMPLICIT NONE
      INTEGER*4 ICODE,OCODE
	REAL*4 GSCALE
	INTEGER*4 ILO,IHI         !Min and max DN limits for output image
      INTEGER*4 LFLAG           !=1 if need to check for ILO,
      INTEGER*4 INUM,IDEF,PAR(2)
c      REAL*4 EPS
      INTEGER*4 LOLIM(3)
      INTEGER*4 HILIM(3)
c      DATA EPS/1.E-6/
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
$!-----------------------------------------------------------------------------
$ create mgnsize.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C***************************************************************
C Originally implemented for Magellan, this is the 'LOOKUP option of SIZE
C
C	Include 'r2lib:main.fin'
C
C Dummy main for testing arrays
C
C	Subroutine Main44
C	Include 'SLOOKUP.Inc/list'
C	Integer*4 Full(256)
C	Byte Bits(256)
C
C	Do I=0,127,1
C	   Bits( I+1 ) = I
C	   Bits( I+129 ) = -128 + I
C	EndDo
C
C	Call MGNSIZE
C
C	Length = 256
C	Call MgnDntoVo( Bits, Full, Length )
C	Write(6,20)
C20	Format( ' *** Dn ---> Vo Conversion ***', /' ' )
C	Write(6,10) ((I,(Bits(I).And.255),Full(I)),I=1,256)
C10	Format( ' Bits( ', I3, ' ) = ', I3, '    Full = ', I6 )
C
C	Write(6,30)
C30	Format( /'0*** Vo ---> Dn Conversion ***', /' ' )
C	Call MgnVotoDn( Full, Bits, Length )
C	Write(6,10) ((I,(Bits(I).And.255),Full(I)),I=1,256)
C	Call Exit
C	End
C*************
C Initialize
C*************
	Subroutine MGNSIZE
	implicit none
c	Implicit Integer*4 (A-Z)
	Integer*4 in1, in2, IOStatus,MaxOutLength
	Integer*4 I,i1,i2,i3,ind,nlit,nsit,nlot,nsot
	Character*40 FilNam
        INCLUDE 'fortport'		!for byte2int
	INCLUDE 'slookup.inc'

C************************************************************
C  Table values generated with the following equations:
C  ------------------------------------------------------------
C  	DN = 1 + 5*(20*ALOG10[((LINE-1)*ns + SAMP)/scale] + 20)
C  	Vo = scale * 10 ** [((SAMP-2)/100) - 1]
C  where
C  Line = [ 1..nl ], Sample = [ 1..25000 ], ns = max # samples
C  ------------------------------------------------------------
C  Non-scaled equations are:
C  	Vo = 10 ** [ {(Dn-1)/100} - 1  ],  
C  	Dn = 1 + 5*(20 log(V) + 20)	Dn: 0-255
C---------------------------------------------------------------
C  Get info about dn-volts table here and open it (256 int array, 1024 bytes)
	Call xvp( 'DVDATA', FilNam, IOStatus)
	Call xvunit( in1, 'DVDATA', 1, IOStatus,
     *               'U_NAME', FilNam,' ' )
	CALL XVSIGNAL(in1,IOStatus,.TRUE.)
	Call xvopen(in1, IOStatus, 'OPEN_ACT', 'SA', 'IO_ACT', 'SA',' ')
        CALL XVGET(in1,IND,'NL',NLIT,'NS',NSIT,' ')
        IF (NLIT.NE.1.AND.NSIT.NE.256) GOTO 991
        Call xvread(in1, InpTable, IOStatus,' ')
	Call xvclose(in1, IOStatus,' ')

C  Get info about volts-dn table here and open it (25000 bytes per line)
	Call xvp( 'VDDATA', FilNam, IOStatus )
	Call xvunit(in2, 'VDDATA', 1, IOStatus, 'U_NAME', FilNam,' ')
	CALL XVSIGNAL(in2,IOStatus,.TRUE.)
	Call xvopen(in2, IOStatus, 'OPEN_ACT', 'SA', 'IO_ACT', 'SA',' ')
        CALL XVGET(in2,IND,'NL',NLOT,'NS',NSOT,' ')
        MaxOutLength = NLOT * NSOT
	if (MaxOutLength.gt.1250000) GOTO 992
	Do I=1,MaxOutLength,NSOT
	   Call xvread(in2, OutTable(I), IOStatus,' ')
	EndDo
	Call xvclose(in2, IOStatus,' ')

C  Check tables :  InpTable = dn to volts, OutTable = volts to dn
C  See if what goes in will come back out!

      I1 = BYTE2INT(OutTable(InpTable(255)))
      I2 = BYTE2INT(OutTable(InpTable(128)))
      I3 = BYTE2INT(OutTable(InpTable(0)))

      if (  I1 .NE.255. or . 
     *      I2 .NE.128. or .
     *      I3 .NE.0 ) GOTO 995

C  List out arrays for debug
C	Write(6,30)		! "D" debug marker does not work (?!) in Vicar
C30	Format( ' ' )
C
C	Do I=1,255
C	   Write(6,20) I, InpTable( I )
C20	   Format( ' DN(', I3, ') = ', I6 )
C	EndDo
C
C	Write(6,30)
C	Inc = 10
C	Do I=1,MaxOutLength,Inc
C	   Write(6,10) I, ((OutTable( J ) .And. 255),J=I,I+Inc-1)
C10	   Format( ' I = ', I6, '   D = ', <Inc>(' ', I3) )
C	EndDo
C
	Return

  991 CALL XVMESSAGE('??E - Invalid DN-to-Volts file format',' ')
      GOTO 999
  992 CALL XVMESSAGE( '??E - Volts-to-DN table exceeds allowed size',' ')
      goto 999
  995 CALL XVMESSAGE('??E - Bad DN-volt-DN conversion found',' ')
      CALL XVMESSAGE('??E -Check DN-to-Volts and Volts-to-DN tables',' ')
  999 call abend 
      END

C*****************************
C Convert from DN to Vo value 
C*****************************

	Subroutine MgnDNtoVo(InBuf,OutBuf,InLen)
	implicit none
c	Implicit Integer*4 (A-Z)
	Integer*4 InBuf(*)
	Integer*4 OutBuf(*)
	Integer*4 InLen
	Integer*4 I
	INCLUDE 'slookup.inc'

	Do I=1,InLen
	   OutBuf(I) = InpTable(InBuf(I))
	EndDo
	Return
	End

C*****************************
C Convert from Vo to DN value 
C*****************************

C	Subroutine MgnVotoDN(InBuf,OutBuf,InLen) 
C	Implicit Integer*4 (A-Z)
C	Integer*4 InBuf(1)
C	Logical*1 OutBuf(1)
C	Integer*4 InLen
C	Integer*4 I
C	INCLUDE 'slookup.inc'
C
C	Do I=1,InLen
C	   If (InBuf(I) .Gt. MaxOutLength) InBuf(I) = MaxOutLength
C	   OutBuf(I) = OutTable(InBuf(I))
C	EndDo
C	Return
C	End
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create lookup.f
$ DECK/DOLLARS="$ VOKAGLEVE"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compress an image via integral LZOOM, IZOOM by first converting the
C DN to some unspecified units via a table look-up, computing the average
C value for each area, and then converting back to DN via the inverse table.
C Input and output images may be byte.
C
      SUBROUTINE LOOKUP(INTERP,nsix,nsox,buf,rbuf)
	IMPLICIT NONE
      
	integer*4 nsix,nsox
      COMMON/CP/IUNIT,ICODE,SLI,SSI,NLI,NSI
      COMMON/CP/OUNIT,OCODE,SLO,SSO,NLO,NSO,NLOUT,NSOUT
      COMMON/CP/ZOOML,ZOOMS,LZOOM,IZOOM,GSCALE,ILO,IHI,LFLAG

      INTEGER*4 IUNIT,OUNIT     !Input and output logical unit numbers
      INTEGER*4 ICODE,OCODE     !Input and ouput image format
      INTEGER*4 SLI,SSI,NLI,NSI !Input image area (see AREA parameter)
      INTEGER*4 SLO,SSO,NLO,NSO !Output area mapped from input area
      INTEGER*4 IND
      INTEGER*4 NLOUT,NSOUT     !Size of ouput image
c      INTEGER*4 SLI0,SSI0
      INTEGER*4 ILO,IHI         !Min and max DN limits for output image
      INTEGER*4 LFLAG           !=1 if need to check for ILO,IHI saturation
	INTEGER*4 LZOOM,IZOOM     !Integer zoom factors (=0 if not integer)
	INTEGER*4 I,II,J,K,L,M,N,ISUM
	BYTE BUF(NSIX)
      LOGICAL*4 INTERP
      REAL*4 RBUF(NSOX)

c      COMMON/C3/IUNIT
      REAL*4 ZOOML,ZOOMS
	REAL*4 GSCALE

      INTEGER*4 LINC,SINC
      REAL*4 C
      INCLUDE 'slookup.inc'
      INCLUDE 'fortport'		!byte2int

      IF (.NOT.INTERP.OR.LZOOM.GE.0.OR.IZOOM.GE.0.OR.NLI.EQ.1
     &		.OR.GSCALE.NE.1.0) GOTO 990
      CALL MGNSIZE		!Initialize voltage to dn arrays
      LINC = -LZOOM
      SINC = -IZOOM
      M = LINC - 1
      N = SINC - 1
      C = ZOOML*ZOOMS
      IF (SLI.GT.1) CALL XVREAD(IUNIT,BUF,IND,'LINE',SLI-1,' ')

      DO L=1,NLO
         CALL XVREAD(IUNIT,BUF,IND,' ')
         II = SSI
         DO I=1,NSO
            ISUM = INPTABLE(BYTE2INT(BUF(II)))
            II = II +1
            DO J=1,N
               ISUM = ISUM + INPTABLE(BYTE2INT(BUF(II)))
               II = II + 1
            ENDDO
            RBUF(I) = ISUM
         ENDDO

         DO K=1,M
            CALL XVREAD(IUNIT,BUF,IND,' ')
            II = SSI
            DO I=1,NSO
               ISUM = INPTABLE(BYTE2INT(BUF(II)))
               II = II +1
               DO J=1,N
                  ISUM = ISUM + INPTABLE(BYTE2INT(BUF(II)))
                  II = II + 1
               ENDDO
               RBUF(I) = RBUF(I) + ISUM
            ENDDO
         ENDDO

         DO I=1,NSO
            J = RBUF(I)*C + 0.5
            BUF(I) = OUTTABLE(J)
         ENDDO
         CALL XVWRIT(OUNIT,BUF,IND,' ')
      ENDDO
      RETURN

  990 CALL XVMESSAGE('??E - Invalid parameter option for LOOKUP',' ')
      IF (.NOT.INTERP) THEN
         CALL XVMESSAGE('??E - Interpolation cannot be suppressed',' ')
      ELSE IF (LZOOM.EQ.0.OR.IZOOM.EQ.0) THEN
         CALL XVMESSAGE('??E- Integral ZOOM must be specified',' ')
      ELSE IF (LZOOM.GT.0.OR.IZOOM.GT.0) THEN
         CALL XVMESSAGE('??E - Image magnification not permitted',' ')
      ELSE IF (GSCALE.NE.1.0) THEN
         CALL XVMESSAGE('??E - SCALE parameter cannot be specified',' ')
      ELSE
         CALL XVMESSAGE('??E- Input image contains only one line',' ')
      ENDIF
	call abend
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Get slope and offset and compute DN-to-volts table.
C
      SUBROUTINE DTVTABLE(slope,offset,table)
	IMPLICIT NONE
      REAL*4 TABLE(0:255)
	real*4 slope,offset,dn,vo
	integer*4 icnt,i

      CALL XVP('SLOPE',slope,icnt)
      CALL XVP('OFFSET',offset,icnt)

      DO I=0,255
         Dn = I
         Vo = 10.0**((Dn-OFFSET)/SLOPE)
         TABLE(I) = Vo
      ENDDO

      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compress an image via integral LZOOM, IZOOM for Magellan.
C Input and output images may be byte.
C Note: In this version of SIZE, all Magellan specific code is
C consolidated here and in subroutines SMGN1 and MgnSizeInit.
C The subroutines MgnDNtoVo and MgnVotoDN are never called.
C
      SUBROUTINE SMGN(INTERP,TABLE,SLOPE,OFFSET,NSIX,NSOX,
     &		buf,rbuf)
	IMPLICIT NONE
	INTEGER*4 NSIX,NSOX
      BYTE BUF(NSIX)
      LOGICAL*4 INTERP
      REAL*4 RBUF(NSOX),TABLE(0:255)

      COMMON/CP/IUNIT,ICODE,SLI,SSI,NLI,NSI
      COMMON/CP/OUNIT,OCODE,SLO,SSO,NLO,NSO,NLOUT,NSOUT
      COMMON/CP/ZOOML,ZOOMS,LZOOM,IZOOM,GSCALE,ILO,IHI,LFLAG
c      COMMON/CP/ICODE,SLI0,SSI0,SLI,SSI,NLI,NSIX,NLI0,NSI0,OUNIT,OCODE,

      INTEGER*4 IUNIT,OUNIT     !Input and output logical unit numbers
      INTEGER*4 ICODE,OCODE     !Input and ouput image format
      INTEGER*4 SLI,SSI,NLI,NSI !Input image area (see AREA parameter)
      INTEGER*4 SLO,SSO,NLO,NSO !Output area mapped from input area
      INTEGER*4 NLOUT,NSOUT     !Size of ouput image
c      INTEGER*4 SLI0,SSI0
	INTEGER*4 LZOOM,IZOOM     !Integer zoom factors (=0 if not integer)
      INTEGER*4 ILO,IHI         !Min and max DN limits for output image
      INTEGER*4 LFLAG           !=1 if need to check for ILO,IHI saturation
	REAL*4 ZOOML,ZOOMS        !Floating point zoom factors
	REAL*4 GSCALE             !Output DN scale factor
c      COMMON/C3/IUNIT
	INTEGER*4 I,II,IX,J,K,L,M,N,LINC,IND,IDN

      REAL*4 LOGTBL(0:2249)

      INTEGER*4 SINC
      REAL*4 C,V,SLOPE,OFFSET,RSUM,P
      INCLUDE 'fortport'

      IF (.NOT.INTERP.OR.LZOOM.GE.0.OR.IZOOM.GE.0.OR.NLI.EQ.1
     &		.OR.GSCALE.NE.1.0) GOTO 990
C     ....Generate log table
      DO I=0,2249
         V = 1.0 + 0.004*I
         LOGTBL(I) = ALOG10(V)
      ENDDO

      LINC = -LZOOM
      SINC = -IZOOM
      M = LINC - 1
      N = SINC - 1
      C = ZOOML*ZOOMS
      OFFSET = OFFSET + 0.5
      IF (SLI.GT.1) CALL XVREAD(IUNIT,BUF,IND,'LINE',SLI-1,' ')

      DO L=1,NLO
         CALL XVREAD(IUNIT,BUF,IND,' ')
         II = SSI
         DO I=1,NSO
            RSUM = TABLE(BYTE2INT(BUF(II)))
            II = II +1
            DO J=1,N
               RSUM = RSUM + TABLE(BYTE2INT(BUF(II)))
               II = II + 1
            ENDDO
            RBUF(I) = RSUM
         ENDDO

         DO K=1,M
            CALL XVREAD(IUNIT,BUF,IND,' ')
            II = SSI
            DO I=1,NSO
               RSUM = TABLE(BYTE2INT(BUF(II)))
               II = II +1
               DO J=1,N
                  RSUM = RSUM + TABLE(BYTE2INT(BUF(II)))
                  II = II + 1
               ENDDO
               RBUF(I) = RBUF(I) + RSUM
            ENDDO
         ENDDO

         DO I=1,NSO
            V = RBUF(I)*C
            IF (V.LT.100.0) THEN
               IF (V.LT.1.0) THEN
                  IF (V.LT.0.1) THEN
                     IF (V.LT.0.01) THEN !Here if V < 0.01
                        IDN = SLOPE*ALOG10(V) + OFFSET  !Convert to DN
                        GOTO 50
                     ELSE		!Here if 0.01 < V < 0.1
                        V = 100.0*V
                        P = -2.
                     ENDIF
                  ELSE			!Here if 0.1 < V < 1
                     V = 10.0*V
                     P = -1.
                  ENDIF
               ELSE
                  IF (V.LT.10.0) THEN	!Here if 1 < V < 10
                      P = 0.
                  ELSE			!Here if 10 < V < 100
                      V = 0.1*V
                      P = 1.
                  ENDIF
               ENDIF
            ELSE			!Here if 100 < V
               IF (V.LT.1000.0) THEN	!Here if 100 < V < 1000
                  V = 0.01*V
                  P = 2.
               ELSE
                  IF (V.LT.10000.) THEN !Here if 1000 < V < 10000
                     V = 0.001*V
                     P = 3.
                  ELSE   
                     IDN = SLOPE*ALOG10(V) + OFFSET  !Convert to DN
                     GOTO 50
                  ENDIF
               ENDIF
            ENDIF     
            IX = 250.*(V-1.) + 0.5
            IX = MIN0(IX,2249)
            IDN = SLOPE*(LOGTBL(IX)+P) + OFFSET  !Convert to DN
   50       BUF(I) = INT2BYTE(IDN)
         ENDDO
         CALL XVWRIT(OUNIT,BUF,IND,' ')
      ENDDO
      RETURN

  990 CALL XVMESSAGE('??E - Invalid parameter option for Magellan',' ')
      IF (.NOT.INTERP) THEN
         CALL XVMESSAGE(' ***Interpolation cannot be suppressed',' ')
      ELSE IF (LZOOM.EQ.0.OR.IZOOM.EQ.0) THEN
         CALL XVMESSAGE('??E- Integral ZOOM must be specified',' ')
      ELSE IF (LZOOM.GT.0.OR.IZOOM.GT.0) THEN
         CALL XVMESSAGE('??E - Image magnification not permitted',' ')
      ELSE IF (GSCALE.NE.1.0) THEN
         CALL XVMESSAGE('??E- SCALE parameter cannot be specified',' ')
      ELSE
         CALL XVMESSAGE('??E- Input image contains only one line',' ')
      ENDIF
      call abend
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compress an image for Magellan.
C Special case: ZOOM=-3 (exact solution)
C Input and output images may be byte.
C
      SUBROUTINE SMGN1(INTERP,TABLE,SLOPE,OFFSET,NSIX,NSOX,buf,rbuf)
	IMPLICIT NONE
	INTEGER*4 NSIX,NSOX
      BYTE BUF(NSIX)
      LOGICAL*4 INTERP
      REAL*4 RBUF(NSOX),TABLE(0:255)

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
	INTEGER*4 I,II,K,L,IND,IDN
	REAL*4 slope,offset,Vo
c      INTEGER SLI0,SSI0,SLI,SSI,OUNIT,OCODE

c      COMMON/C3/IUNIT

      INCLUDE 'fortport'

      IF (.NOT.INTERP.OR.NLI.EQ.1.OR.GSCALE.NE.1.0) GOTO 990
      IF (SLI.GT.1) CALL XVREAD(IUNIT,BUF,IND,'LINE',SLI-1,' ')
      OFFSET = OFFSET + 0.5		!For rounding off result

      DO L=1,NLO
         CALL XVREAD(IUNIT,BUF,IND,' ')
         II = SSI
         DO I=1,NSO
            RBUF(I) = TABLE(BYTE2INT(BUF(II)))
     &           + TABLE(BYTE2INT(BUF(II+1)))
     &           + TABLE(BYTE2INT(BUF(II+2)))
            II = II + 3
         ENDDO

         DO K=1,2
            CALL XVREAD(IUNIT,BUF,IND,' ')
            II = SSI
            DO I=1,NSO
               RBUF(I) = RBUF(I) + TABLE(BYTE2INT(BUF(II)))
     &           + TABLE(BYTE2INT(BUF(II+1)))
     &           + TABLE(BYTE2INT(BUF(II+2)))
               II = II + 3
            ENDDO
         ENDDO

         DO I=1,NSO
            Vo = RBUF(I)/9.0
            IDN = SLOPE*ALOG10(Vo) + OFFSET
            BUF(I) = INT2BYTE(IDN)
         ENDDO
         CALL XVWRIT(OUNIT,BUF,IND,' ')
      ENDDO
      RETURN

  990 CALL XVMESSAGE('??E - Invalid parameter option for Magellan',' ')
      IF (.NOT.INTERP) THEN
         CALL XVMESSAGE('??E - Interpolation cannot be suppressed',' ')
      ELSE IF (GSCALE.NE.1.0) THEN
         CALL XVMESSAGE('??E - SCALE parameter cannot be specified',' ')
      ELSE
         CALL XVMESSAGE('??E - Input image contains only one line',' ')
      ENDIF
      call abend
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create slookup.inc
$ DECK/DOLLARS="$ VOKAGLEVE"
C File SLOOKUP.Inc
C	Implicit Integer*4 (A-Z)
	Common/MGNAddSize/ InpTable, OutTable
	Integer*4 InpTable(0:255)
	Byte OutTable(1250000)
C
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create sizemgn.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM sizemgn

   To Create the build file give the command:

		$ vimake sizemgn			(VMS)
   or
		% vimake sizemgn			(Unix)

************************************************************************/
#define PROGRAM	sizemgn

#define MODULE_LIST sizemgn.f getsize.f update_label.f snoin.f sintrp.f \
           sread.f magnify.f compress.f mgnsize.f lookup.f
#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define R2LIB
#define FTNINC_LIST fortport mp_for_defs
#define INCLUDE_LIST  slookup.inc   /* local include  */

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create sizemgn.pdf
PROCESS HELP=*
PARM INP     TYPE=STRING
PARM OUT     TYPE=STRING
PARM SIZE    TYPE=INTEGER COUNT=4   VALID=(0:50000)	DEFAULT=(1,1,0,0)
PARM NL      TYPE=INTEGER COUNT=0:1 VALID=(0:50000)	DEFAULT=0
PARM NS      TYPE=INTEGER COUNT=0:1 VALID=(0:50000)	DEFAULT=0
PARM NB      TYPE=INTEGER COUNT=0:1 DEFAULT=--
PARM AREA    TYPE=INTEGER COUNT=0:4 VALID=(0:50000)	DEFAULT=--
PARM IOFFSET TYPE=INTEGER COUNT=0:2 VALID=(-50000:50000) DEFAULT=--
PARM NOIN    TYPE=KEYWORD COUNT=0:1 VALID=NOIN		DEFAULT=--
PARM ZOOM    TYPE=REAL    COUNT=0:1 VALID=(-50000:50000) DEFAULT=--
PARM LZOOM   TYPE=REAL    COUNT=0:1 VALID=(-50000:50000) DEFAULT=--
PARM SZOOM   TYPE=REAL    COUNT=0:1 VALID=(-50000:50000) DEFAULT=--
PARM SCALE   TYPE=REAL    COUNT=1			DEFAULT=1.0
PARM LIMITS  TYPE=INTEGER COUNT=0:2			DEFAULT=--
PARM OFORM   TYPE=KEYWORD COUNT=0:1 VALID=(BYTE,HALF,FULL,REAL)	DEFAULT=--
PARM LOOKUP  TYPE=KEYWORD COUNT=0:1 VALID=LOOKUP	DEFAULT=--
PARM VOLTS   TYPE=KEYWORD COUNT=0:1 VALID=VOLTS		DEFAULT=--
PARM SLOPE   TYPE=REAL    COUNT=1			DEFAULT=50.0
PARM OFFSET  TYPE=REAL    COUNT=1			DEFAULT=101.0
PARM DVDATA  TYPE=(STRING,40) COUNT=1 DEFAULT=MGNSIZED.DAT
PARM VDDATA  TYPE=(STRING,40) COUNT=1 DEFAULT=MGNSIZEV.DAT
PARM DEBUG   TYPE=KEYWORD           VALID=(DEBUG,NODEBUG) DEFAULT=NODEBUG
END-PROC
.TITLE
VICAR program SIZEMGN
.HELP
PURPOSE:
SIZEMGN is a VICAR applications program which may be used to magnify or compress
the size of an image and/or change its aspect ratio.  This program is largely
identical to program SIZE, but contains special features to support Magellan
(MGN) processing not present in the latter program.  

EXECUTION STATEMENT:

      SIZEMGN  INP=IPIC  OUT=OPIC  user-parameters...

where IPIC is the input image and OPIC is the output image.  IPIC and OPIC
may be in byte, halfword (16-bit integer), fullword (32-bit integer), or
floating point (REAL*4) data format.

OPIC will normally have the same data format as IPIC.  Use the OFORM parameter
to change the output format.

SIZEMGN performs  bilinear interpolation to magnify or compress the image.
The 'NOIN keyword may be used to suppress interpolation.  See sections on image
magnification and reduction below.

.page
OPERATION:

The size of the output image is specified in one of two ways:

(1) Explicitly specifying it with the NL and NS parameters:

        SIZEMGN  INP  OUT  NL=500  NS=500
    or  SIZEMGN  INP  OUT  SIZE=(1,1,500,500)

(2) Specifying a magnification or compression (zoom) factor:

        SIZEMGN  INP  OUT  ZOOM=3		! 3x magnification
        SIZEMGN  INP  OUT  ZOOM=-2         ! 2x compression
    or  SIZEMGN  INP  OUT  ZOOM=0.5        ! 2x compression

The ZOOM factor can be a floating point value.
A negative ZOOM results in image compression.  I.e. ZOOM= -2.5 is the
same as ZOOM=0.4

Note the if both NL,NS ans ZOOM are specified, the ZOOM parameter will
determine the magnification/compression factor and NL,NS will determine the
size of the output image.

Independent zooms may be specified in the line (vertical) and sample
(horizontal) directions:

        SIZEMGN  INP  OUT  LZOOM=2  SZOOM=3
        SIZEMGN  INP  OUT  LZOOM=3  SZOOM=-2

As the last example implies, the image may be magnified in one direction
and compressed in the other.

The AREA parameter may be used to restrict processing to an area of the
input image.  For example:

		SIZEMGN  IPIC  OPIC  ZOOM=-3  AREA=(10,10,100,100)
is equivalent to
		COPY  IPIC  IDS  (10,10,100,100)
		SIZEMGN  IDS  OPIC  ZOOM=-3

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

		SIZEMGN  A  B  SIZE=(1,1,9,9)
        or      SIZEMGN  A  B  NL=9  NS=9
	or	SIZEMGN  A  B  ZOOM=3
        or      SIZEMGN  A  B  LZOOM=3  SZOOM=3

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
		SIZEMGN  INP  OUT  NL=8  NS=8  ZOOM=2  IOFFSET=(2,2)  'NOIN

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
		SIZEMGN  A  B  SIZE=(1,1,9,9)  'NOIN
	or	SIZEMGN  A  B  ZOOM=3  'NOIN

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
		SIZEMGN  A  B  SIZE=(1,1,9,9)  ZOOM=3

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

Note that the magnified image output by SIZEMGN differs slightly from what one
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
		SIZEMGN  B  C  ZOOM=-3

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

		SIZEMGN  IPIC  OPIC  ZOOM=-N  'NOIN

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
		SIZEMGN  B  C  ZOOM=-3  'NOIN
		SIZEMGN  B  D  ZOOM=-3  'NOIN  AREA=(2,2,8,8)

will generate 3x3 output images C and D of the form:

		0  3  6		       2  5  8
	 C  =	3  6  9		D  =   5  8 11
		6  9 12		       8 11 14

Note the use of the AREA parameter to begin the resampling at a point other
than pixel (1,1).

The input image may be compressed by a non-integral zoom factor r:

		SIZEMGN  IPIC  OPIC  ZOOM=r  'NOIN

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

		SIZEMGN  A  B  SIZE=(1,1,250,250)
		SIZEMGN  A  B  ZOOM=2.5

To blow up a 50x50 area from the center of the picture by a factor of 4:

		SIZEMGN  A  B  ZOOM=4  AREA=(26,26,50,50)

To average all the lines of an image together, use

		SIZEMGN  A  B  NL=1

The following equivalent statements magnify the line direction by 2 and
shrinks the sample direction by 2:

		SIZEMGN  A  B  SIZE=(1,1,200,50)
		SIZEMGN  A  B  LZOOM=2  SZOOM=-2

.page
MAGELLAN SPECIFIC OPTIONS:

Special code has been added to SIZEMGN to convert Magellan data to units of
volts before compressing the image.  The pixels are subsequently converted
back to DN prior to output.  Both input and output images must be in byte
format.  The input image must contain more than one sample per line.
The Magellan option is implemented for image compression and integral ZOOM
values only.  The NOIN, LIMITS and SCALE parameters are illegal with this
option.

The Magellan option is currently implemented using two competing algorithms:
If keyword 'LOOKUP is specified, the program uses tables input from external
files to convert the input samples from DNs to voltages and to convert the
output samples from voltages to DNs.  If the keyword 'VOLTS is specified,
the program performs the conversions using tables computed internally.  The
'LOOKUP algorithm is 20 percent faster, but introduces a random error of 0.5 DN
with a maximum error of 20 DN.  The error is greatest at low DN levels.

The next two sections describe the LOOKUP and VOLTS keywords.
.page
LOOKUP KEYWORD:

If the 'LOOKUP keyword is specified, the DN-to-volts and volts-to-DN tables
are read from files specified by the DVDATA and VDDATA parameters.  Both lookup
tables may be generated via VICAR procedure SIZELOOKUP.  If defaulted, files
MGNSIZED.DAT and MGNSIZEV.DAT are read from MGNLIB.

The DN-to-volts table is in fullword (INTEGER*4) data format and contains
256 entries (for DNs 0 to 255).  The entries in this table are assumed to be
non-negative.  The table must be input from a file in standard VICAR image
format, consisting of one line of 256 samples.

The Volts-to-DN table is in byte format and contains at least N entries, where
N is the maximum value stored in the DN-to-volts table (N < 1250000).

Ex:    SIZEMGN  IPIC  OPIC  ZOOM=-3  'LOOKUP
  Lookup tables MGNSIZED.DAT and MGNSIZEV.DAT are automatically accessed from
  directory MGNLIB.

Ex:    SIZELOOKUP  OUT_DIR=DEV:[JOE123]
       SIZEMGN  IPIC  OPIC  ZOOM=-4  'LOOKUP +
	  DVDATA=DEV:[JOE123]MGNSIZED.DAT +
	  VDDATA=DEV:[JOE123]MGNSIZEV.DAT

  The procedure SIZELOOKUP is used to compute external lookup tables
  MGNSIZED.DAT and MGNSIZEV.DAT in directory DEV:[JOE123].  The DVDATA and
  VDDATA parameters are used to input these files to SIZEMGN.

.page
VOLTS KEYWORD:

If the 'VOLTS keyword is specified, conversion tables are computed internally
using the following formulas:
	a)  v = 10**((d-b)/a)
        b)  d = a*logv + b
where v=volts, d=DN, a=slope, b=offset.  The slope and offset may be specified
via the SLOPE and OFFSET parameters.

The volts-to-DN conversion is accomplished by computing a 2250 entry logrithmic
table.  When the default slope (50) is used, this table introduces a 0.1 DN
random error, with a maximum error of 1 DN.  Larger slopes will result in 
correspondingly larger errors.

.page
PROGRAM RESTRICTIONS:

Both IPIC and OPIC may be up to 20,000 pixels in width (sample size) and of
arbitrary length (line or vertical dimension).

The input image may be on tape or disk.  However, the output image must be on
random-access disk storage.


PROGRAM HISTORY:

Written by: Gary Yagi, 26 January 1976
Cognizant programmer: Ray Bambery

Revision history:

    Jun 06, 2012 - R. J. Bambery - gfortran 4.6.3 changed all dimension (1) values to (*)
                                in subroutines to avoid
        "Fortran runtime error: Index '2' of dimension 1 of array 'id' above upper bound of 1"
    06 May 1011 R.J. Bambery - removed warning messages by gfortran 4.4.4 
                    compiler
    10 Feb 04  lwk  Renamed SIZE to SIZEMGN in order to preserve Magellan
		            functionality removed from new version of SIZE.

For previous history, see SIZE.PDF.

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
.VARI LOOKUP
Magellan specific option to
compress an image.
.VARIABLE VDDATA
Magellan specific option.
Specifies lookup table for
converting volts to DNs.
.VARIABLE DVDATA
Magellan specific option.
Specifies lookup table for
converting DNs to volts.
.VARI VOLTS
Magellan specific option to
compress an image.
.VARI SLOPE
Magellan specific option
Specifies slope of volts-to-dn
conversion
.VARI OFFSET
Magellan specific option
Specifies offset of volts-to-dn
conversion.
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

		SIZEMGN  A  B  SIZE=(1,1,9,9)
        or      SIZEMGN  A  B  NL=9  NS=9
	or	SIZEMGN  A  B  ZOOM=3
        or      SIZEMGN  A  B  LZOOM=3  SZOOM=3
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
.VARI LOOKUP
Keyword 'LOOKUP is a Magellan specific option specifying the use of lookup
tables to convert the input samples from DNs to voltages and to convert the
output samples from voltages to DNs.  The lookup tables are input from files
specified via parameters DVDATA and VDDATA (see below).

Both input and output images must be in byte format.  The input image must
contain more than one sample per line.

LOOKUP is implemented for image compression and integral ZOOM values only.
The NOIN keyword is illegal with this option.

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
		SIZEMGN  INP  OUT  NL=8  NS=8  ZOOM=2  IOFFSET=(2,2)  'NOIN

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

This option is not allowed with the Magellan-specific LOOKUP and VOLTS
keywords.

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
.VARIABLE VDDATA
VDDATA specifies a table lookup file for converting voltages to DNs.
This is a Magellan-specific option.  See help file for details.
.VARIABLE DVDATA
DVDATA specifies a table lookup file for converting DNs to voltages.
This is a Magellan-specific option.  See help file for details.
.VARI VOLTS
Keyword 'VOLTS is a Magellan specific option which causes the input DN values
to be converted to volts prior to image compression and the results to be
converted back to DN prior to output.  Both input and output images must be in
byte format.  The input image must contain more than one sample per line.

The conversion formulas are as follows:
	a)  v = 10**((d-b)/a)
        b)  d = a*logv + b
where v=volts, d=DN, a=slope, b=offset.  The slope and offset may be specified
via the SLOPE and OFFSET parameters.

VOLTS is implemented for image compression and integral ZOOM values only.
The NOIN keyword is illegal with this option.
.VARI SLOPE
See VOLTS for further details
.VARI OFFSET
See VOLTS for further details
.VARI DEBUG
Print the following message while the status returned from routine MP_LABLE_READ
is less than MP_SUCCESS: "error reading MP labels or MP labels do not exist"
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstsizemgn.pdf
procedure                   !Test file for program SIZEMGN
local   afidsroot   type=string count=1


  refgbl $echo
  refgbl $syschar

! Jun 25, 2012 - RJB
! TEST SCRIPT FOR SIZEMGN
! tests BYTE, HALF images
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
sizemgn a b nl=9 ns=9	!Use size field to specify magnification
list b
sizemgn a c zoom=3		!Use ZOOM parameter to specify magnification
difpic (b,c)		!Results should be identical
sizemgn a c lzoom=3 szoom=3
difpic (b,c)		!Results should be identical

!....3x3 image compression, three different ways
sizemgn b c nl=3 ns=3		!Zoom factor specified by size field
list c
sizemgn b d zoom=-3		!Zoom factor specified by ZOOM parameter
difpic (c,d)
sizemgn b d lzoom=-3 szoom=-3	!Zoom factor specified by LZOOM,SZOOM
difpic (c,d)

sizemgn b c zoom=-3	!Integral compression
list b
sizemgn b c zoom=-2.5	!Non-integral compression
list b

sizemgn a b (1,1,11,11) ioffset=(2,2) zoom=3	!IOFFSET parameter
list b 'zeroes

sizemgn a b zoom=3 limits=(1,14)			!LIMITS parameter
list b

!...Special cases
sizemgn a b zoom=3	area=(1,1,1,3)		!nli=1
list b
sizemgn b c lzoom=1 szoom=-3 area=(1,1,1,9)
list c

sizemgn a b zoom=3	area=(1,1,3,1)		!nsi=1
list b
sizemgn b c lzoom=-3 szoom=1 area=(1,1,9,1)
list c

sizemgn a b lzoom=3 szoom=1		!SZOOM=1
list b
sizemgn b c lzoom=-3 szoom=1
difpic (a,c)

sizemgn a b lzoom=1 szoom=3		!LZOOM=1
list b
sizemgn b c lzoom=1 szoom=-3
difpic (a,c)

!...Mixed magnifications/compressions
sizemgn b c lzoom=3 szoom=-3		!vertical mag, horizontal compression
list c
sizemgn c d lzoom=-3 szoom=3		!horizontal mag, vertical compression
difpic (b,d)				!no differences

!...Mixed data modes, outputs greater than inputs
sizemgn a b zoom=3 'half scale=100 	!byte to halfword
list b
sizemgn b c zoom=-3 'full scale=1000	!half to full
list c
sizemgn b d zoom=-3 'real scale=1000	!half to real
list d

!...Mixed data modes, outputs smaller than inputs
sizemgn d e zoom=3 'byte scale=0.00001	!real to byte
list e
sizemgn c e zoom=3 'half scale=0.001	!full to half
list e
sizemgn b e zoom=-3 'byte scale=0.01	!half to byte
list e

let $echo="no"
write "!!!!!!!!!!!!!!!!!!!!!!!!!"
write "! Non-interpolation mode"
write "!!!!!!!!!!!!!!!!!!!!!!!!!"
let $echo="yes"
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Non-nterpolation mode
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
gen a 3 3 ival=2 sinc=3 linc=3

!....First, test all options on a byte image
sizemgn a b zoom=3 'noin		!Zoom up
list b
sizemgn b c zoom=-3 'noin		!Zoom down
difpic (a,c)

sizemgn a c zoom=2.5 'noin		!Non-integral magnification
list c
sizemgn b c zoom=-2.5 'noin	!Non-integral compression
list c

sizemgn a b (1,1,11,11) ioffset=(2,2) zoom=3 'noin	!IOFFSET parameter
list b 'zeroes

sizemgn a b zoom=3 limits=(1,14) 'noin		!LIMITS parameter
list b

!...Mixed magnifications/compressions
sizemgn a b lzoom=1 szoom=3 'noin
sizemgn b c lzoom=3 szoom=-3 'noin		!vertical mag, horizontal compression
list c
sizemgn c d lzoom=-3 szoom=3 'noin		!horizontal mag, vertical compression
difpic (b,d)				!no differences

!...Mixed data modes, outputs greater than inputs
sizemgn a b zoom=3 'half scale=100 'noin 		!byte to halfword
list b
sizemgn b c zoom=-3 'full scale=1000 'noin		!half to full
list c
sizemgn b d zoom=-3 'real scale=1000 'noin		!half to real
list d

!...Mixed data modes, outputs smaller than inputs
sizemgn d e zoom=3 'byte scale=0.00001 'noin	!real to byte
list e
sizemgn c e zoom=3 'half scale=0.001 'noin		!full to half
list e
sizemgn b e zoom=-3 'byte scale=0.01 'noin		!half to byte
list e

let $echo="yes"
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Test updating of map projection labels
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
write "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
write "! Test updating of map projection labels"
write "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
let $echo="yes"

!....Test on old map projection labels
label-list vt/m.dat 
sizemgn vt/m.dat a zoom=-2 area=(2,2,400,400)
label-list a

!...Repeat map label test with non-integral zoom:
sizemgn vt/m.dat a zoom=-1.5 area=(2,2,400,400)
label-list a

!...Test on new map projection labels
sizemgn vt/tst1.dat a zoom=20 ioffset=(81,81)	!normal cylindrical projection
label-list a

sizemgn vt/tst10.dat a zoom=20 ioffset=(81,81)	!point perspective projection
label-list a

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! test on multi-band image:
gen a 3 3 3 ival=2 sinc=3 linc=3 binc=3
sizemgn a b zoom=2

rm>
   ush rm -f vt
   ush rm -f ?
let $echo="no"
end-proc
$!-----------------------------------------------------------------------------
$ create tstsizemgn.log_solos
tstsizemgn
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
 Task:GEN       User:lwk       Date_Time:Wed Feb  6 15:03:12 2013
     Samp     1       3
   Line
      1       2   5   8
      2       5   8  11
      3       8  11  14
sizemgn a b nl=9 ns=9
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      9 X      9
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZEMGN task completed
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb  6 15:03:12 2013
 Task:SIZEMGN   User:lwk       Date_Time:Wed Feb  6 15:03:12 2013
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
sizemgn a c zoom=3
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      9 X      9
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZEMGN task completed
difpic (b,c)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
sizemgn a c lzoom=3 szoom=3
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      9 X      9
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZEMGN task completed
difpic (b,c)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
sizemgn b c nl=3 ns=3
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    1,    1,    9,    9)
     OUTPUT SIZE=      3 X      3
 PICTURE SIZE SCALED BY      0.33333*NL,      0.33333*NS
 SIZEMGN task completed
list c
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb  6 15:03:12 2013
 Task:SIZEMGN   User:lwk       Date_Time:Wed Feb  6 15:03:15 2013
     Samp     1       3
   Line
      1       2   5   8
      2       5   8  11
      3       8  11  14
sizemgn b d zoom=-3
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    1,    1,    9,    9)
     OUTPUT SIZE=      3 X      3
 PICTURE SIZE SCALED BY      0.33333*NL,      0.33333*NS
 SIZEMGN task completed
difpic (c,d)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
sizemgn b d lzoom=-3 szoom=-3
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    1,    1,    9,    9)
     OUTPUT SIZE=      3 X      3
 PICTURE SIZE SCALED BY      0.33333*NL,      0.33333*NS
 SIZEMGN task completed
difpic (c,d)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
sizemgn b c zoom=-3
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    1,    1,    9,    9)
     OUTPUT SIZE=      3 X      3
 PICTURE SIZE SCALED BY      0.33333*NL,      0.33333*NS
 SIZEMGN task completed
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb  6 15:03:12 2013
 Task:SIZEMGN   User:lwk       Date_Time:Wed Feb  6 15:03:12 2013
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
sizemgn b c zoom=-2.5
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    1,    1,    9,    9)
     OUTPUT SIZE=      3 X      3
 PICTURE SIZE SCALED BY      0.40000*NL,      0.40000*NS
 SIZEMGN task completed
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb  6 15:03:12 2013
 Task:SIZEMGN   User:lwk       Date_Time:Wed Feb  6 15:03:12 2013
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
sizemgn a b (1,1,11,11) ioffset=(2,2) zoom=3
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=     11 X     11
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZEMGN task completed
list b 'zeroes
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb  6 15:03:12 2013
 Task:SIZEMGN   User:lwk       Date_Time:Wed Feb  6 15:03:19 2013
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
sizemgn a b zoom=3 limits=(1,14)
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      9 X      9
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZEMGN task completed
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb  6 15:03:12 2013
 Task:SIZEMGN   User:lwk       Date_Time:Wed Feb  6 15:03:19 2013
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
sizemgn a b zoom=3	area=(1,1,1,3)
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    1,    1,    1,    3)
     OUTPUT SIZE=      3 X      9
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZEMGN task completed
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb  6 15:03:12 2013
 Task:SIZEMGN   User:lwk       Date_Time:Wed Feb  6 15:03:20 2013
     Samp     1       3       5       7       9
   Line
      1       1   2   3   4   5   6   7   8   9
      2       1   2   3   4   5   6   7   8   9
      3       1   2   3   4   5   6   7   8   9
sizemgn b c lzoom=1 szoom=-3 area=(1,1,1,9)
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    1,    1,    1,    9)
     OUTPUT SIZE=      1 X      3
 PICTURE SIZE SCALED BY      1*NL,     -3*NS
 SIZEMGN task completed
list c
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb  6 15:03:12 2013
 Task:SIZEMGN   User:lwk       Date_Time:Wed Feb  6 15:03:21 2013
     Samp     1       3
   Line
      1       2   5   8
sizemgn a b zoom=3	area=(1,1,3,1)
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    1,    1,    3,    1)
     OUTPUT SIZE=      9 X      3
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZEMGN task completed
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb  6 15:03:12 2013
 Task:SIZEMGN   User:lwk       Date_Time:Wed Feb  6 15:03:21 2013
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
sizemgn b c lzoom=-3 szoom=1 area=(1,1,9,1)
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    1,    1,    9,    1)
     OUTPUT SIZE=      3 X      1
 PICTURE SIZE SCALED BY     -3*NL,      1*NS
 SIZEMGN task completed
list c
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb  6 15:03:12 2013
 Task:SIZEMGN   User:lwk       Date_Time:Wed Feb  6 15:03:22 2013
     Samp     1
   Line
      1       2
      2       5
      3       8
sizemgn a b lzoom=3 szoom=1
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      9 X      3
 PICTURE SIZE SCALED BY      3*NL,      1*NS
 SIZEMGN task completed
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb  6 15:03:12 2013
 Task:SIZEMGN   User:lwk       Date_Time:Wed Feb  6 15:03:22 2013
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
sizemgn b c lzoom=-3 szoom=1
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    1,    1,    9,    3)
     OUTPUT SIZE=      3 X      3
 PICTURE SIZE SCALED BY     -3*NL,      1*NS
 SIZEMGN task completed
difpic (a,c)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
sizemgn a b lzoom=1 szoom=3
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      3 X      9
 PICTURE SIZE SCALED BY      1*NL,      3*NS
 SIZEMGN task completed
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb  6 15:03:12 2013
 Task:SIZEMGN   User:lwk       Date_Time:Wed Feb  6 15:03:24 2013
     Samp     1       3       5       7       9
   Line
      1       1   2   3   4   5   6   7   8   9
      2       4   5   6   7   8   9  10  11  12
      3       7   8   9  10  11  12  13  14  15
sizemgn b c lzoom=1 szoom=-3
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    1,    1,    3,    9)
     OUTPUT SIZE=      3 X      3
 PICTURE SIZE SCALED BY      1*NL,     -3*NS
 SIZEMGN task completed
difpic (a,c)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
sizemgn b c lzoom=3 szoom=-3
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    1,    1,    3,    9)
     OUTPUT SIZE=      9 X      3
 PICTURE SIZE SCALED BY      3*NL,     -3*NS
 SIZEMGN task completed
list c
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb  6 15:03:12 2013
 Task:SIZEMGN   User:lwk       Date_Time:Wed Feb  6 15:03:26 2013
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
sizemgn c d lzoom=-3 szoom=3
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    1,    1,    9,    3)
     OUTPUT SIZE=      3 X      9
 PICTURE SIZE SCALED BY     -3*NL,      3*NS
 SIZEMGN task completed
difpic (b,d)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
sizemgn a b zoom=3 'half scale=100
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      9 X      9
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZEMGN task completed
list b
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Wed Feb  6 15:03:12 2013
 Task:SIZEMGN   User:lwk       Date_Time:Wed Feb  6 15:03:27 2013
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
sizemgn b c zoom=-3 'full scale=1000
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    1,    1,    9,    9)
     OUTPUT SIZE=      3 X      3
 PICTURE SIZE SCALED BY      0.33333*NL,      0.33333*NS
 SIZEMGN task completed
list c
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Wed Feb  6 15:03:12 2013
 Task:SIZEMGN   User:lwk       Date_Time:Wed Feb  6 15:03:28 2013
     Samp            1          2          3
   Line
      1         200000     500000     800000
      2         500000     800000    1100000
      3         800000    1100000    1400000
sizemgn b d zoom=-3 'real scale=1000
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    1,    1,    9,    9)
     OUTPUT SIZE=      3 X      3
 PICTURE SIZE SCALED BY      0.33333*NL,      0.33333*NS
 SIZEMGN task completed
list d
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Wed Feb  6 15:03:12 2013
 Task:SIZEMGN   User:lwk       Date_Time:Wed Feb  6 15:03:28 2013
     Samp             1           2           3
   Line
      1       2.000E+05   5.000E+05   8.000E+05
      2       5.000E+05   8.000E+05   1.100E+06
      3       8.000E+05   1.100E+06   1.400E+06
sizemgn d e zoom=3 'byte scale=0.00001
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      9 X      9
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZEMGN task completed
list e
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb  6 15:03:12 2013
 Task:SIZEMGN   User:lwk       Date_Time:Wed Feb  6 15:03:29 2013
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
sizemgn c e zoom=3 'half scale=0.001
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      9 X      9
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZEMGN task completed
list e
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Wed Feb  6 15:03:12 2013
 Task:SIZEMGN   User:lwk       Date_Time:Wed Feb  6 15:03:30 2013
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
sizemgn b e zoom=-3 'byte scale=0.01
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    1,    1,    9,    9)
     OUTPUT SIZE=      3 X      3
 PICTURE SIZE SCALED BY      0.33333*NL,      0.33333*NS
 SIZEMGN task completed
list e
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb  6 15:03:12 2013
 Task:SIZEMGN   User:lwk       Date_Time:Wed Feb  6 15:03:30 2013
     Samp     1       3
   Line
      1       2   5   8
      2       5   8  11
      3       8  11  14
let $echo="no"
!!!!!!!!!!!!!!!!!!!!!!!!!
! Non-interpolation mode
!!!!!!!!!!!!!!!!!!!!!!!!!
gen a 3 3 ival=2 sinc=3 linc=3
Beginning VICAR task gen
GEN Version 6
GEN task completed
sizemgn a b zoom=3 'noin
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      9 X      9
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZEMGN task completed
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb  6 15:03:31 2013
 Task:SIZEMGN   User:lwk       Date_Time:Wed Feb  6 15:03:31 2013
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
sizemgn b c zoom=-3 'noin
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    1,    1,    9,    9)
     OUTPUT SIZE=      3 X      3
 PICTURE SIZE SCALED BY      0.33333*NL,      0.33333*NS
 SIZEMGN task completed
difpic (a,c)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
sizemgn a c zoom=2.5 'noin
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      7 X      7
 PICTURE SIZE SCALED BY      2.50000*NL,      2.50000*NS
 SIZEMGN task completed
list c
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb  6 15:03:31 2013
 Task:SIZEMGN   User:lwk       Date_Time:Wed Feb  6 15:03:33 2013
     Samp     1       3       5       7
   Line
      1       2   2   5   5   5   8   8
      2       2   2   5   5   5   8   8
      3       5   5   8   8   8  11  11
      4       5   5   8   8   8  11  11
      5       5   5   8   8   8  11  11
      6       8   8  11  11  11  14  14
      7       8   8  11  11  11  14  14
sizemgn b c zoom=-2.5 'noin
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    1,    1,    9,    9)
     OUTPUT SIZE=      4 X      4
 PICTURE SIZE SCALED BY      0.40000*NL,      0.40000*NS
 SIZEMGN task completed
list c
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb  6 15:03:31 2013
 Task:SIZEMGN   User:lwk       Date_Time:Wed Feb  6 15:03:33 2013
     Samp     1       3
   Line
      1       2   2   5   8
      2       2   2   5   8
      3       5   5   8  11
      4       8   8  11  14
sizemgn a b (1,1,11,11) ioffset=(2,2) zoom=3 'noin
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=     11 X     11
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZEMGN task completed
list b 'zeroes
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb  6 15:03:31 2013
 Task:SIZEMGN   User:lwk       Date_Time:Wed Feb  6 15:03:34 2013
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
sizemgn a b zoom=3 limits=(1,14) 'noin
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      9 X      9
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZEMGN task completed
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb  6 15:03:31 2013
 Task:SIZEMGN   User:lwk       Date_Time:Wed Feb  6 15:03:35 2013
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
sizemgn a b lzoom=1 szoom=3 'noin
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      3 X      9
 PICTURE SIZE SCALED BY      1*NL,      3*NS
 SIZEMGN task completed
sizemgn b c lzoom=3 szoom=-3 'noin
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    1,    1,    3,    9)
     OUTPUT SIZE=      9 X      3
 PICTURE SIZE SCALED BY      3*NL,     -3*NS
 SIZEMGN task completed
list c
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb  6 15:03:31 2013
 Task:SIZEMGN   User:lwk       Date_Time:Wed Feb  6 15:03:36 2013
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
sizemgn c d lzoom=-3 szoom=3 'noin
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    1,    1,    9,    3)
     OUTPUT SIZE=      3 X      9
 PICTURE SIZE SCALED BY     -3*NL,      3*NS
 SIZEMGN task completed
difpic (b,d)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
sizemgn a b zoom=3 'half scale=100 'noin
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      9 X      9
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZEMGN task completed
list b
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Wed Feb  6 15:03:31 2013
 Task:SIZEMGN   User:lwk       Date_Time:Wed Feb  6 15:03:37 2013
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
sizemgn b c zoom=-3 'full scale=1000 'noin
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    1,    1,    9,    9)
     OUTPUT SIZE=      3 X      3
 PICTURE SIZE SCALED BY      0.33333*NL,      0.33333*NS
 SIZEMGN task completed
list c
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Wed Feb  6 15:03:31 2013
 Task:SIZEMGN   User:lwk       Date_Time:Wed Feb  6 15:03:38 2013
     Samp            1          2          3
   Line
      1         200000     500000     800000
      2         500000     800000    1100000
      3         800000    1100000    1400000
sizemgn b d zoom=-3 'real scale=1000 'noin
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    1,    1,    9,    9)
     OUTPUT SIZE=      3 X      3
 PICTURE SIZE SCALED BY      0.33333*NL,      0.33333*NS
 SIZEMGN task completed
list d
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Wed Feb  6 15:03:31 2013
 Task:SIZEMGN   User:lwk       Date_Time:Wed Feb  6 15:03:39 2013
     Samp             1           2           3
   Line
      1       2.000E+05   5.000E+05   8.000E+05
      2       5.000E+05   8.000E+05   1.100E+06
      3       8.000E+05   1.100E+06   1.400E+06
sizemgn d e zoom=3 'byte scale=0.00001 'noin
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      9 X      9
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZEMGN task completed
list e
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb  6 15:03:31 2013
 Task:SIZEMGN   User:lwk       Date_Time:Wed Feb  6 15:03:39 2013
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
sizemgn c e zoom=3 'half scale=0.001 'noin
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      9 X      9
 PICTURE SIZE SCALED BY      3*NL,      3*NS
 SIZEMGN task completed
list e
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Wed Feb  6 15:03:31 2013
 Task:SIZEMGN   User:lwk       Date_Time:Wed Feb  6 15:03:40 2013
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
sizemgn b e zoom=-3 'byte scale=0.01 'noin
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    1,    1,    9,    9)
     OUTPUT SIZE=      3 X      3
 PICTURE SIZE SCALED BY      0.33333*NL,      0.33333*NS
 SIZEMGN task completed
list e
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb  6 15:03:31 2013
 Task:SIZEMGN   User:lwk       Date_Time:Wed Feb  6 15:03:41 2013
     Samp     1       3
   Line
      1       2   5   8
      2       5   8  11
      3       8  11  14
let $echo="yes"
write "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
write "! Test updating of map projection labels"
! Test updating of map projection labels
write "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
let $echo="yes"
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
sizemgn vt/m.dat a zoom=-2 area=(2,2,400,400)
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    2,    2,  400,  400)
     OUTPUT SIZE=    200 X    200
 PICTURE SIZE SCALED BY      0.50000*NL,      0.50000*NS
***MAP_RESOLUTION not found
Computing resolution from scale
MAP property label updated
MAP history label updated
 SIZEMGN task completed
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
---- Task: SIZEMGN -- User: lwk -- Wed Feb  6 15:03:41 2013 ----
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
sizemgn vt/m.dat a zoom=-1.5 area=(2,2,400,400)
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    2,    2,  400,  400)
     OUTPUT SIZE=    266 X    266
 PICTURE SIZE SCALED BY      0.66667*NL,      0.66667*NS
***MAP_RESOLUTION not found
Computing resolution from scale
MAP property label updated
MAP history label updated
 SIZEMGN task completed
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
---- Task: SIZEMGN -- User: lwk -- Wed Feb  6 15:03:42 2013 ----
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
sizemgn vt/tst1.dat a zoom=20 ioffset=(81,81)
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    1,    1,   10,   10)
     OUTPUT SIZE=    280 X    280
 PICTURE SIZE SCALED BY     20*NL,     20*NS
***MAP_RESOLUTION not found
Computing resolution from scale
MAP property label updated
MAP history label updated
 SIZEMGN task completed
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
---- Task: SIZEMGN -- User: lwk -- Wed Feb  6 15:03:43 2013 ----
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
sizemgn vt/tst10.dat a zoom=20 ioffset=(81,81)
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    1,    1,   10,   10)
     OUTPUT SIZE=    280 X    280
 PICTURE SIZE SCALED BY     20*NL,     20*NS
MAP property label updated
MAP history label updated
 SIZEMGN task completed
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
---- Task: SIZEMGN -- User: lwk -- Wed Feb  6 15:03:44 2013 ----
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
gen a 3 3 3 ival=2 sinc=3 linc=3 binc=3
Beginning VICAR task gen
GEN Version 6
GEN task completed
sizemgn a b zoom=2
Beginning VICAR task sizemgn
 SIZEMGN version 06 Jun 2012
      INPUT AREA=(    1,    1,    3,    3)
     OUTPUT SIZE=      6 X      6
 PICTURE SIZE SCALED BY      2*NL,      2*NS
 SIZEMGN task completed
 SIZEMGN task completed
 SIZEMGN task completed
   ush rm -f vt
   ush rm -f ?
let $echo="no"
exit
slogoff
$ Return
$!#############################################################################
