$!****************************************************************************
$!
$! Build proc for MIPL module adespike
$! VPACK Version 1.9, Tuesday, January 15, 2013, 17:06:32
$!
$! Execute by entering:		$ @adespike
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
$ write sys$output "*** module adespike ***"
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
$ write sys$output "Invalid argument given to adespike.com file -- ", primary
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
$   if F$SEARCH("adespike.imake") .nes. ""
$   then
$      vimake adespike
$      purge adespike.bld
$   else
$      if F$SEARCH("adespike.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake adespike
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @adespike.bld "STD"
$   else
$      @adespike.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create adespike.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack adespike.com -mixed -
	-s adespike.f -
	-i adespike.imake -
	-p adespike.pdf -
	-t tstadespike.pdf tstadespike.log_solos
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create adespike.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
C VICAR PROGRAM ADESPIKE -- Detect and remove single-pixel errors.fmt
C          ADESPIKE  INP  OUT  user-parameters...
C    REVISION HISTORY
C       7-95    CRI    fixed xvtrans_inb incorrect variable as per FR87387
C	6-94    CRI    MSTP S/W CONVERSION (VICAR PORTING)
c  10-Jan-2013 -lwk- changed write statements for VTOTAL/HTOTAL due to
c                    new compiler flag on Solaris
C
      SUBROUTINE MAIN44
      IMPLICIT INTEGER(A-Z)
      CHARACTER*8  LMT
      INTEGER*4 INSO,NXX,NYY,NSO,NLO,NBB
      INTEGER I,ISTAT

      COMMON/CB/TOT,HTOT,DCODE,IBITS,IDN,BINARY
      COMMON/CA/ISUTO,ISLTO,ICUTO,ICLTO,IDSUT,IDSLT,IDCUT,IDCLT,STDIO

      COMMON/SIZE/SLO,SSO,NSO,NLO,IUN,OUN,NBB,NLB,NLI,NSI

      LOGICAL STDIO,BINARY
      EXTERNAL WORKA	!  Called by STACKA
      EXTERNAL WORKB	!  Called by STACKA

      CHARACTER*132 VTOTAL
      CHARACTER*132 HTOTAL

      HTOTAL = ' '
      I   = 0
      ISTAT = 0
      LMT = ' '
      NLI = 0
      NSI = 0
      VTOTAL = ' '


      CALL IFMESSAGE ('ADESPIKE version 21-JUL-95')
      CALL XVEACTION ('SA',' ')


c
c   First, Open assuming NON-BINARY
c
      CALL XVUNIT(IUN,'INP',1,I,' ')
      CALL XVOPEN(IUN,I, 'U_FORMAT', 'HALF',' ')
      CALL XVSIZE(SLO,SSO,NLO,NSO,NLI,NSI)
      CALL XVGET(IUN,I,'NLB',NLB,'NBB',NBB,'FORMAT',LMT,' ')
c
c   This portion will be repeated in Subroutine APARAM
c
      IF (LMT.EQ.'BYTE') DCODE = 1  
      IF (LMT.EQ.'HALF') DCODE = 2
      IF ((LMT.NE.'HALF') .AND. (LMT.NE.'BYTE'))  Then
         CALL XVMESSAGE(
     &        '***Illegal format, only byte/half allowed.',' ')
         GOto 999
      ENDIF

      CALL APARAM(*999)

C	Check for binary header and image size .. Max is 800x800
c
      BINARY = .FALSE.

      IF (NLB .NE. 0 .OR. NBB .NE. 0) THEN

        BINARY = .TRUE.
        IF (SLO .NE. 1 .OR. SSO .NE. 1) THEN
            CALL XVMESSAGE 
     &         ('Windowing binary labelled image not allowed',' ')
            GOTO 999
        ENDIF
        IF (NLO .GT. 800 .OR. NSO .GT. 800) THEN
            CALL XVMESSAGE
     &         ('Image too large to process binary label',' ')
            GOTO 999
        ENDIF
        CALL XVCLOSE(IUN,ISTAT,' ')
 
c
c    Open BINARY  INPUT  after CLOSE !     
c
         CALL XVUNIT(IUN,'INP',1,I,' ')

         IF (LMT .EQ. 'BYTE') THEN
         CALL XVOPEN(IUN,I,'COND','BINARY','U_FORMAT','BYTE',' ')
         ENDIF

         IF (LMT .EQ. 'HALF') THEN
         CALL XVOPEN(IUN,I,'COND','BINARY','U_FORMAT','HALF',' ')
         ENDIF

      ENDIF                       !  If (BINARY)
c
c    Open OUTPUT now !
c

      CALL XVUNIT(OUN,'OUT',1,I,' ')

      IF (BINARY) THEN
        IF (LMT .EQ. 'BYTE') THEN
	CALL XVOPEN(OUN,I,'O_FORMAT',
     &	    'BYTE','OP','WRITE','U_NS',NSO,'U_NLB',NLB,'U_NBB',NBB,
     &      'U_FORMAT','BYTE',' ')
        ENDIF
        IF (LMT .EQ. 'HALF') THEN
	CALL XVOPEN(OUN,I,'O_FORMAT',
     &	    'HALF','OP','WRITE','U_NS',NSO,'U_NLB',NLB,'U_NBB',NBB,
     &      'U_FORMAT','HALF',' ')
        ENDIF

      ELSE
	CALL XVOPEN(OUN,I,
     &	 'OP','WRITE','U_NS',NSO,'U_FORMAT','HALF',' ')
      ENDIF

      CALL AUTSET(NSO,ISUTO,ISLTO,ICUTO,ICLTO,IDSUT,IDSLT,IDCUT,IDCLT,
     &		DCODE,IBITS,IDN)

      IF (BINARY) THEN
        NXX = (NLO+2) * (2*NSO + NBB)    ! (800+2) * (2*800 + 200) 
        NYY = NSO + NBB                  ! 800 + 200 
        CALL STACKA(7,WORKB,2,NXX,NYY,NSO,NLO,NBB)
      ELSE
        INSO = 8*NSO
        CALL STACKA(3,WORKA,1,INSO)
      ENDIF

      WRITE (VTOTAL,9900) TOT
9900  FORMAT (
     +'     TOTAL NUMBER OF PIXELS CORRECTED USING ADJACENT SCAN LINE TEST       ',I6)
      CALL XVMESSAGE(VTOTAL(2:80),' ')
      WRITE (HTOTAL,9910) HTOT
9910  FORMAT (
     +'     TOTAL NUMBER OF PIXELS CORRECTED USING SAME SCAN LINE TEST          ',I6)
      CALL XVMESSAGE(HTOTAL(2:80),' ')
c      WRITE (LAB,9920) TOT,HTOT
c9920  FORMAT (
c     +'     ADJACENT LINE PIXELS CHANGED ',I6,'. SAME LINE PIXELS
c     +CHANGED ',I5)
C     CALL XVMESSAGE(LAB(2:80),' ')


CCCC	CALL XLADD(OUN,'HISTORY','PGM_LAB',LAB,ISTAT,
CCCC     &	 'FORMAT','STRING','ULEN',68)
      CALL XLADD(OUN,'HISTORY','ADJ_LINE',TOT,ISTAT,
     &	 'FORMAT','INT',' ')
      CALL XLADD(OUN,'HISTORY','SAM_LINE',HTOT,ISTAT,
     &	 'FORMAT','INT',' ')
      RETURN
CCCCCCCCC
C          ERROR CONDITIONS
  999 CALL XVMESSAGE('***ADESPIKE task cancelled',' ')
      CALL ABEND
      END


C Process the image.
C
      SUBROUTINE WORKA(BUF,NBYT)
      IMPLICIT INTEGER (A-Z)
      INTEGER ISTAT
      INTEGER*4 NBYT
      BYTE BUF(NBYT/4,4)

      COMMON/CB/TOT,HTOT,DCODE,IBITS,IDN,BINARY
      COMMON/CA/ISUTO,ISLTO,ICUTO,ICLTO,IDSUT,IDSLT,IDCUT,IDCLT,STDIO

      COMMON/SIZE/SLO,SSO,NSO,NLO,IUN,OUN,NBB,NLB,NLI,NSI

      LOGICAL STDIO,BINARY

      ISTAT = 0
      ELO = SLO + NLO - 1
      I1 = 1		!Index to upper adjacent line
      I2 = 2		!Index to current line
      I3 = 3		!Index to lower adjacent line
      CALL XVREAD(IUN,BUF(1,I2),ISTAT,'SAMP',SSO,'NSAMPS',
     &	   NSO,'LINE',SLO,' ')	!Read in first line

      DO 100 L=SLO,ELO

      IF (L.LT.ELO) THEN	!Read next image line
         IF (STDIO) THEN
            CALL XVREAD(IUN,BUF(1,I3),ISTAT,' ')
         ELSE
            CALL XVREAD(IUN,BUF(1,I3),ISTAT,'SAMP',SSO,
     &           'NSAMPS',NSO,' ')
         ENDIF
      ENDIF

      IF (IDN.GE.0) THEN	!If replacement DN is specified,
         I0 = 4			!we need a separate output buffer.
         CALL MVE(2,NSO,BUF(1,I2),BUF(1,I0),1,1)
      ELSE			!Otherwise,set output buffer
         I0 = I2		!equal to input buffer.
      ENDIF

      IF (L.EQ.SLO.OR.L.EQ.ELO) THEN
         CALL HUTSAR(BUF(1,I2),BUF(1,I0),HTOT,1,NSO)
      ELSE
         CALL AUTSAR(BUF(1,I1),BUF(1,I2),BUF(1,I3),BUF(1,I0),HTOT,TOT,
     &               1,NSO)
      ENDIF

      CALL XVWRIT(OUN,BUF(1,I0),ISTAT,' ')
      I1 = I2
      I2 = I3
  100 I3 = MOD(I3,3) + 1

      RETURN
      END

c
C Process the binary-header image.
C
      SUBROUTINE WORKB(BUF,BUF_SIZE,BBUF,BBSIZE,NSAMPS,NLINES,NPIX)
      IMPLICIT INTEGER (A-Z)
      INTEGER*4 BUF_SIZE,BBSIZE,NSAMPS,NLINES,NPIX
c
      BYTE  BUF(2*NSAMPS+NPIX,NLINES+2)   ! for HALF 
      BYTE  BBUF(NSAMPS+NPIX)             ! for BYTE

      COMMON/CC/LINE,REC_CNT,BDV_REC
      COMMON/CB/TOT,HTOT,DCODE,IBITS,IDN,BINARY
      COMMON/CA/ISUTO,ISLTO,ICUTO,ICLTO,IDSUT,IDSLT,IDCUT,IDCLT,STDIO

      COMMON/SIZE/SLO,SSO,NSO,NLO,IUN,OUN,NBB,NLB,NLI,NSI
      
      LOGICAL STDIO,BINARY

      INTEGER INBUF(12)
      INTEGER LASTPIX(800),ISTAT,II
      INTEGER*2 HREC_H(1800),HREC_B(1000)      ! For HALF and BYTE 
      INTEGER*2 HREC_HOUT(1800),HREC_BOUT(1000)
      INTEGER*2 BDV_REC(1000)   ! 2000 BYTES, more than 1800+ or 1000+ !

      II = 0
      ISTAT = 0
      DO 2 II = 1,1800
        HREC_H(II) = 0
2     CONTINUE
      DO 4 II = 1,1000
        HREC_B(II) = 0
4     CONTINUE

C        Zero array with the 4-byte count
      CALL ZIA(BDV_REC,500)  ! Zero out Bad data value buffer.

      REC_CNT = NLB	!BDV record counter
      BDV_REC(1) = 6	!Bad data type (single pixel spikes)
      BDV_REC(2) = 1	!Object type (single pixel)
      BDV_REC(3) = 0	!Counter
      ELO = NLO
      I0 = 0
      I1 = 2		!Index to upper adjacent line
      I2 = 3		!Index to current line
      I3 = 4		!Index to lower adjacent line

      DO LINE=1, 800
         LASTPIX(LINE) = NSO
      ENDDO

      DO BHDR=1, NLB
       IF (DCODE .EQ. 2)   then               ! Half
         CALL XVREAD(IUN,HREC_H(1),ISTAT,' ') !Read in binary labels
         CALL XVWRIT(OUN,HREC_H(1),ISTAT,' ') ! 1800+ Bytes/line
         CALL XVTRANS_INB(INBUF,'HALF','HALF',IUN,ISTAT)
         IF (ISTAT .NE. 1) THEN
            CALL XVMESSAGE(' INBUF SETUP UNSUCCESSFUL',' ')
            CALL ABEND
         ENDIF
         CALL XVTRANS(INBUF,HREC_H,HREC_HOUT,1800)
         IF (HREC_HOUT(1) .EQ. 3) THEN	      ! Data-Drop-Out record
            DO BDVCNT=1, HREC_HOUT(3)
               LASTPIX(HREC_HOUT(BDVCNT*3+1)) = 
     &                 HREC_HOUT(BDVCNT*3+2) - 1
            ENDDO
         ENDIF
       ENDIF

       IF (DCODE .EQ. 1)   then               ! BYTE Format
         CALL XVREAD(IUN,HREC_B(1),ISTAT,' ') !Read in binary labels
         CALL XVWRIT(OUN,HREC_B(1),ISTAT,' ') ! 1000+ Bytes/line
         CALL XVTRANS_INB(INBUF,'HALF','HALF',IUN,ISTAT)
         IF (ISTAT .NE. 1) THEN
            CALL XVMESSAGE(' INBUF SETUP UNSUCCESSFUL',' ')
            CALL ABEND
         ENDIF
         CALL XVTRANS(INBUF,HREC_B,HREC_BOUT,1000)
         IF (HREC_BOUT(1) .EQ. 3) THEN	      ! Data-Drop-Out record
            DO BDVCNT=1, HREC_BOUT(3)
               LASTPIX(HREC_BOUT(BDVCNT*3+1)) =
     &                 HREC_BOUT(BDVCNT*3+2) - 1
            ENDDO
         ENDIF
       ENDIF

      ENDDO              !  End of BINARY-HEADER COPYing

      IDX = NBB+1
      RECLTH = 2 * NSO + NBB    ! 1800+ 
      IF (DCODE.EQ.2) THEN                !  HALF
	CALL XVREAD(IUN,BUF(1,I2),ISTAT,' ')  !  READing in 1800+ 
      ELSE
	CALL XVREAD(IUN,BBUF,ISTAT,' ')       ! BYTE, Read in 1000+
c             MOVE first 200, BYTE to BYTE
        Call MVE(1,NBB,BBUF(1),BUF(1,I2),1,1)   
C             Move NEXT 800, BYTE TO HALF 
	CALL MVE(3,NSO,BBUF(IDX),BUF(IDX,I2),1,1) 
      ENDIF

      DO 100 LINE=3,ELO
	IF (LINE.LT.ELO) THEN
	  IF (DCODE.EQ.2) THEN
	    CALL XVREAD(IUN,BUF(1,I3),ISTAT,' ')
	  ELSE
	   CALL XVREAD(IUN,BBUF,ISTAT,' ')
C             Move first 200, BYTE to BYTE 
           Call MVE(1,NBB,BBUF(1),BUF(1,I3),1,1)   
C             Move NEXT 800, BYTE TO HALF
	   CALL MVE(3,NSO,BBUF(IDX),BUF(IDX,I3),1,1) 
	  ENDIF
	ENDIF

        IF (IDN.GE.0) THEN	!If replacement DN is specified,
           I0 = I0 + 1
           CALL MVE(1,RECLTH,BUF(1,I2),BUF(1,I0),1,1)
        ELSE			!Otherwise,set output buffer
           I0 = I2		!equal to input buffer.
        ENDIF

c
CCC	SET LINE LENGTH BASED ON LAST VALID PIX FROM EDR LINE HEADER
c
         IF (LINE .EQ. 3 .OR. LINE .EQ. ELO) THEN

            CALL HUTSAR(BUF(IDX,I2),BUF(IDX,I0),HTOT,1,LASTPIX(I2))
         ELSE
            IF (LASTPIX(I2) .LE. LASTPIX(I1) .AND.
     &          LASTPIX(I2) .LE. LASTPIX(I3)) THEN
               CALL AUTSAR(BUF(IDX,I1),BUF(IDX,I2),BUF(IDX,I3),
     &             BUF(IDX,I0),HTOT,TOT,1,LASTPIX(I2))
            ELSE
               IF (LASTPIX(I1) .LE. LASTPIX(I3)) THEN
                  CALL AUTSAR(BUF(IDX,I1),BUF(IDX,I2),BUF(IDX,I3),
     &             BUF(IDX,I0),HTOT,TOT,1,LASTPIX(I1))
                  CALL HUTSAR(BUF(IDX,I2),BUF(IDX,I0),HTOT,
     &                 LASTPIX(I1)+1,LASTPIX(I2))
               ELSE
                  CALL AUTSAR(BUF(IDX,I1),BUF(IDX,I2),BUF(IDX,I3),
     &           BUF(IDX,I0),HTOT,TOT,1,LASTPIX(I3))
                  CALL HUTSAR(BUF(IDX,I2),BUF(IDX,I0),HTOT,
     &                 LASTPIX(I3)+1,LASTPIX(I2))
               ENDIF
            ENDIF
         ENDIF

         I1 = I2
         I2 = I3
         I3 = I3 + 1
100   CONTINUE

      CALL DUMP_BDV(0)
      I0 = 2
      IF (IDN .GE. 0) I0 = 0
      DO I2=I0+1, ELO+I0
	IF (DCODE.EQ.2) THEN
	  CALL XVWRIT(OUN,BUF(1,I2),ISTAT,' ')
	ELSE                                    
C            Move first 200 BYTEs
          Call MVE(1,NBB,BUF(1,I2),BBUF(1),1,1)      
C            Move the REst 800 Bytes
	  CALL MVE(-3,NSO,BUF(IDX,I2),BBUF(IDX),1,1) 
	  CALL XVWRIT(OUN,BBUF,ISTAT,' ')           
	ENDIF
      ENDDO
c
C	UPDATE NLB WITH XLDEL & XLADD
c
      CALL XLDEL(OUN,'SYSTEM','NLB',ISTAT,' ')
      CALL XLADD(OUN,'SYSTEM','NLB',REC_CNT,ISTAT,'FORMAT',
     &     'INT',' ')


      RETURN
      END
C
C
C
      SUBROUTINE DUMP_BDV(SAMP)
      IMPLICIT INTEGER(A-Z)

      COMMON/CC/LINE,REC_CNT,BDV_REC
      COMMON/CB/TOT,HTOT,DCODE,IBITS,IDN,BINARY
      COMMON/CA/ISUTO,ISLTO,ICUTO,ICLTO,IDSUT,IDSLT,IDCUT,IDCLT,STDIO

      COMMON/SIZE/SLO,SSO,NSO,NLO,IUN,OUN,NBB,NLB,NLI,NSI

      INTEGER outbuf(12)
      INTEGER ISTAT
      INTEGER*2 BDV_REC(1000),TRANS_OUT(1000)
      LOGICAL STDIO,BINARY

      call xvtrans_out(outbuf,'half','half','low','vax',istat)
      IF (ISTAT .NE. 1) THEN
         CALL XVMESSAGE(' OUTBUF SETUP UNSUCCESSFUL',' ')
         CALL ABEND
      ENDIF
      ISTAT = 0
      IF (.NOT. BINARY) RETURN

      IF (SAMP .LE. 0) THEN
         IF (BDV_REC(3) .GT. 0) THEN
            call xvtrans(outbuf,bdv_rec,trans_out,1000)
            CALL XVWRIT(OUN,TRANS_OUT,ISTAT,' ')
            REC_CNT = REC_CNT + 1
         ENDIF
      ELSE
         IF (BDV_REC(3) .GE. ((NBB+DCODE*NSO)-6)/4) THEN
            call xvtrans(outbuf,bdv_rec,trans_out,1000)
            CALL XVWRIT(OUN,TRANS_OUT,ISTAT,' ')
            REC_CNT = REC_CNT + 1
C            BDV_REC(3) = 0
            CALL ZIA(BDV_REC(3),499)
         ENDIF
         BDV_REC(BDV_REC(3)*2+4) = LINE
         BDV_REC(BDV_REC(3)*2+5) = SAMP
         BDV_REC(3) = BDV_REC(3) + 1
      ENDIF

      RETURN
      END
C Process ADESPIKE user-parameters...
C All values in common areas CA and CB are set.
C
      SUBROUTINE APARAM(*)
      IMPLICIT INTEGER (A-Z)
      COMMON/SIZE/SLO,SSO,NSO,NLO,IUN,OUN,NBB,NLB,NLI,NSI

      COMMON/CB/TOT,HTOT,DCODE,IBITS,IDN,BINARY
      COMMON/CA/ISUTO,ISLTO,ICUTO,ICLTO,IDSUT,IDSLT,IDCUT,IDCLT,STDIO

      LOGICAL STDIO,BINARY,PRINT,XVPTST
      CHARACTER*132 TOL1
      CHARACTER*132 TOL2
      CHARACTER*8 FMT
      INTEGER I,ICNT,IDEF,J,N

      INTEGER POWERS(17)

      DATA POWERS/1,2,4,8,16,32,64,128,256,512,1024,2048,4096,
     +		8192,16384,32768,65536/

      I = 0
      ICNT = 0
      IDEF = 0
      J = 0
      FMT  = ' '
      HTOT = 0		!Total number of horizontal spikes
      N = 0
      TOL1 = ' '
      TOL2 = ' '
      TOT = 0		!Total number of vertical spikes

      IF (SLO.NE.1.OR.SSO.NE.1.OR.NSO.NE.NSI) THEN
         STDIO = .FALSE.
      ELSE
         STDIO = .TRUE.
      ENDIF

      CALL XVGET(IUN,I,'FORMAT',FMT,' ')
      IF (FMT.EQ.'BYTE') THEN
         DCODE = 1
      ELSE IF (FMT.EQ.'HALF') THEN
         DCODE = 2
      ELSE
         CALL XVMESSAGE
     &        ('***Illegal format, only byte/half allowed.',' ')
         RETURN1
      ENDIF

      CALL XVPARM('TOL',N,ICNT,IDEF,1)

      IF (IDEF.EQ.0) THEN
          CALL MVE(4,8,N,ISUTO,0,1)
      ELSE
         CALL XVPARM('SUTOL',ISUTO,I,J,1)
         CALL XVPARM('SLTOL',ISLTO,I,J,1)
         CALL XVPARM('CUTOL',ICUTO,I,J,1)
         CALL XVPARM('CLTOL',ICLTO,I,J,1)
         CALL XVPARM('DSUTOL',IDSUT,I,J,1)
         CALL XVPARM('DSLTOL',IDSLT,I,J,1)
         CALL XVPARM('DCUTOL',IDCUT,I,J,1)
         CALL XVPARM('DCLTOL',IDCLT,I,J,1)
      END IF

      PRINT = XVPTST('PRINT')
      if (PRINT) then
        WRITE (TOL1,9920) ISLTO,ISUTO,IDSLT,IDSUT
9920  FORMAT ('     SLTOL=',I6,'    SUTOL=',I6,'    DSLTOL=',I6,
     +'    DSUTOL=',I6)
        CALL XVMESSAGE(TOL1(2:67),' ')
        WRITE (TOL2,9930) ICLTO,ICUTO,IDCLT,IDCUT
9930  FORMAT ('     CLTOL=',I6,'    CUTOL=',I6,'    DCLTOL=',I6,
     +'    DCUTOL=',I6)
        CALL XVMESSAGE(TOL2(2:67),' ')
        CALL XVMESSAGE(' ',' ')
      end if

      CALL XVPARM('DN',IDN,I,J,1)

      CALL XVPARM('BITS',IBITS,I,J,1)
      IF (IBITS.EQ.0) THEN
         IAVG = (ISUTO+ISLTO+ICUTO+ICLTO+IDSUT+IDSLT+IDCUT+IDCLT)/8
         DO I=1,16
            IF (IAVG.GE.POWERS(I).AND.IAVG.LT.POWERS(I+1)) THEN
               IF (DCODE.EQ.1) IBITS=8-I
               IF (DCODE.EQ.2) IBITS=16-I
               GOTO 6
            ENDIF
         ENDDO
         RETURN1
      ELSEIF (IBITS.LT.0) THEN
         IBITS=0
      ENDIF

    6 IF (DCODE.EQ.1) THEN
         IF (IBITS.EQ.8) THEN
            IBITS = 0
         ELSEIF (IBITS.GT.8) THEN
            CALL XVMESSAGE
     &         ('***Invalid BITS for BYTE data ignored',' ')
            IBITS = 0
         ELSEIF (IBITS.NE.0) THEN
            IBITS = IBITS+8		!for conversion to halfword
         ENDIF
      ENDIF
	
      RETURN
      END
C Original FORTRAN version is saved here for conversion to other machines.
C Search and delete spikes on a line.
C 83-4-12  ...LWK...  Modified ucl routine: create 3 entry points:
C 94-6-2   ...CRI(MAC)... replaced assembly language for UNIX portability
C
C   AUTSET:  Set constants for AUTSAR and HUTSAR
C   AUTSAR:  Perform horizontal and vertical tests to detect and delete
C            spikes.
C   HUTSAR:  Perform horizontal test only.
C
      SUBROUTINE AUTSET (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12)
      IMPLICIT INTEGER(A - Z)
      INTEGER*2 BUF1(*),BUF2(*),BUF3(*),OBUF(*),HMASK,LMASK
      SAVE NSO,ESU,ESL,ECU,ECL,DSU,DSL,DCU,DCL,DCODE,IBITS,IDN
      SAVE HMASK,LMASK


      NSO = P1
      ESU = P2
      ESL = P3
      ECU = P4
      ECL = P5
      DSU = P6
      DSL = P7
      DCU = P8
      DCL = P9
      DCODE = P10
      IBITS = P11
      IDN = P12
      IF (IBITS .GT. 0) THEN
         LMASK = 2**(16-IBITS)-1
         HMASK = NOT(LMASK)
      ENDIF
      RETURN
C AUTO-SAR (Perform hor.+vert. despiking)
C Inputs: 3 consecutive input lines BUF1,BUF2,and BUF3.
C Outpts: OBUF = despiked version of BUF2.
C         HTOT = running count of horizontal spikes.
C         TOT  = running count of vertical spikes.
C
      ENTRY AUTSAR(BUF1,BUF2,BUF3,obuf,
     & htot,tot,ssamp,nsamp)
      I = ssamp

C-----------------------------------------------------------------C
C   Added the following statement (6/4/94) so that the FORTRAN    C
C      code yields same result as the assembly code that it is    C
C      replacing.                                                 C
C-----------------------------------------------------------------C
      nso = nsamp

      X0 = BUF2(I)
      X2 = BUF2(1+I)
      GOTO 30		!Skip horizontal test at left margin.
C X0 is the current pixel.  It is compared against its four neighbors:
C           X3                      BUF1(I)
C        X1 X0 X2        BUF2(I-1)  BUF2(I)  BUF2(I+1)
C           X4                      BUF3(I)
C     ....Horizontal bit-error test.
   10 X2 = BUF2(I+1)
      AVG = (X1+X2)/2
      IF (X0.LT.AVG-ESL) GOTO 25
C     ....Here if AVG-ESL .LE. X0
      IF (X0.LE.AVG+ESU) GOTO 30
      IF (X0.LE.X1+DSU) GOTO 30
      IF (X0.LE.X2+DSU) GOTO 30
      HTOT = HTOT + 1			!Horizontal bit error.
      GOTO 40

   25 IF (X0.GE.X1-DSL) GOTO 30
      IF (X0.GE.X2-DSL) GOTO 30
      HTOT = HTOT + 1			!Horizontal bit error.
      GOTO 40
C
C     ....Vertical bit-error test.
   30 X3 = BUF1(I)
      X4 = BUF3(I)
      AVG = (X3+X4)/2

      IF (X0.LT.AVG-ECL) GOTO 35
C     ....Here if AVG-ECL .LE. X0.
      IF (X0.LE.AVG+ECU) GOTO 50
      IF (X0.LE.X3+DCU) GOTO 50
      IF (X0.LE.X4+DCU) GOTO 50
      TOT = TOT + 1			!Vertical bit error
      GOTO 40
C     ....Here if X0.LT.AVG-ECL
   35 IF (X0.GE.X3-DCL) GOTO 50
      IF (X0.GE.X4-DCL) GOTO 50
      TOT = TOT + 1			!Vertical bit error.
C     ....Replace pixel.
   40 IF (IBITS.GT.0) THEN	!If BITS is specified,replace
         X0 = IOR(IAND(LMASK,X0),IAND(HMASK,AVG)) !high-order bits.
      ELSE
         X0 = AVG		!Otherwise,replace all bits
      ENDIF
      IF (IDN.GT.0) THEN	!If user has specified replacement
         OBUF(I) = IDN		!pixel,then use it.
      ELSE			!Otherwise,
         OBUF(I) = X0		!replace pixel with average.
      ENDIF

C-----------------------------------------------------------------C
C   Added the following statement (6/4/94) so that the FORTRAN    C
C      code yields same result as the assembly code that it is    C
C      replacing.                                                 C
C-----------------------------------------------------------------C
      call dump_bdv(i)

   50 X1 = X0			!Update to next pixel
      X0 = X2
      I = I + 1
      IF (I.LT.NSO) GOTO 10
      IF (I.EQ.NSO) GOTO 30
      RETURN
C Horizontal de-spiking only.
C
      ENTRY HUTSAR(BUF2,obuf,htot,ssamp,nsamp)
      NSO = NSAMP	! Redundant due to change requiring BDV processing
      NS = NSO - 1
      X0 = BUF2(SSAMP)
      X2 = BUF2(SSAMP+1)
      DO 60 I=SSAMP+1,NS
      X1 = X0
      X0 = X2
      X2 = BUF2(I+1)
      AVG = (X1+X2)/2
      IF (X0.LT.AVG-ESL) GOTO 55
C     ....Here if AVG-ESL .LE. X0
      IF (X0.LE.AVG+ESU) GOTO 60
      IF (X0.LE.X1+DSU) GOTO 60
      IF (X0.LE.X2+DSU) GOTO 60


      GOTO 59
C     ....Here if X0.LT.AVG-ESL
   55 IF (X0.GE.X1-DSL) GOTO 60
      IF (X0.GE.X2-DSL) GOTO 60

C
   59 CONTINUE
      HTOT = HTOT + 1			!Horizontal bit error
      IF (IBITS.GT.0) THEN
         X0 = IOR(IAND(LMASK,X0),IAND(HMASK,AVG))
      ELSE
         X0 = AVG
      ENDIF
      IF (IDN.GT.0) THEN
         OBUF(I) = IDN
      ELSE
         OBUF(I) = X0
      ENDIF

      CALL DUMP_BDV(I)


   60 CONTINUE

      RETURN
      END
c$!
c$! create adespike.mar
c	.psect autset
c	.title autset
c;                     4   8   12  16  20  24  28  32  36   40    44   48
c;  subroutine autset(nso,esu,esl,ecu,ecl,dsu,dsl,dcu,dcl,dcode,ibits,idn)
c	.entry 	autset,^m<>
c	movl	@4(ap),nso
c	movl	@8(ap),esu
c	movl	@12(ap),esl
c	movl	@16(ap),ecu
c	movl	@20(ap),ecl
c	movl	@24(ap),dsu
c	movl	@28(ap),dsl
c	movl	@32(ap),dcu
c	movl	@36(ap),dcl
c	movl	@40(ap),dcode
c	movl	@44(ap),ibits
c	movl	@48(ap),idn
c	movl	ibits,r0
c	bleq	900$
c	subl3	r0,#16,r0
c	movl	#1,r1
c	ashl	r0,r1,r1
c	decl	r1
c	movw	r1,lmask
c	mcomw	r1,hmask
c;masks are complemented to work with "bic"
c	mcomw	lmask,lmask
c	mcomw	hmask,hmask
c900$:	ret
c;
cnso:	.blkl	1
cesu:	.blkl	1
cesl:	.blkl	1
cecu:	.blkl	1
cecl:	.blkl	1
cdsu:	.blkl	1
cdsl:	.blkl	1
cdcu:	.blkl	1
cdcl:	.blkl	1
cdcode:	.blkl	1
cibits:	.blkl	1
cidn:	.blkl	1
clmask:	.blkw	1
chmask:	.blkw	1
ctmp1:	.blkw	1
ctmp2:	.blkw	1
carg_list:
c	.long	1
c	.address  samp
csamp:	.blkl	1
c;                      4    8   12   16   20   24
c;  subroutine autsar(buf1,buf2,buf3,obuf,htot,tot,ssamp,nsamp)
c	.entry	autsar,^m<r2,r3,r4,r5,r6,r7,r8,r9,r10,r11>
c	movl	4(ap),r5	;address of buf1
c	movl	8(ap),r6	;address of buf2
c	movl	12(ap),r7	;address of buf3
c	movl	16(ap),r8	;address of obuf
c	movl	@28(ap),r9	; i = ssamp
c	decl	r9		
c	movl	@32(ap),nso	; nso = nsamp
c	decl	nso		; *** fixed 15oct90
c	cvtwl	(r6)[r9],r0	; x0 = buf2(1)
c	cvtwl	2(r6)[r9],r2	; x2 = buf2(2)
c	brb	30$		; goto 30
c;	....Horizontal bit-error test
c10$:	cvtwl	2(r6)[r9],r2	;10 x2 = buf2(i+1)
c	addl3	r1,r2,r10
c	ashl	#-1,r10,r10	;   avg = (x1+x2)/2
c	subl3	esl,r10,r11
c	cmpl	r0,r11
c	blss	25$		;   if (x0.lt.avg-esl) goto 25
c	addl3	esu,r10,r11
c	cmpl	r0,r11
c	bleq	30$		;   if (x0.le.avg+esu) goto 30
c	addl3	dsu,r1,r11
c	cmpl	r0,r11
c	bleq	30$		;   if (x0.le.x1+dsu) goto 30
c	addl3	dsu,r2,r11
c	cmpl	r0,r11
c	bleq	30$		;   if (x0.le.x2+dsu) goto 30
c	incl	@20(ap)		;   htot = htot + 1
c	brb	40$		;   goto 40
c25$:
c	subl3	dsl,r1,r11
c	cmpl	r0,r11
c	bgeq	30$		;25 if (x0.ge.x1-dsl) goto 30
c	subl3	dsl,r2,r11
c	cmpl	r0,r11
c	bgeq	30$		;   if (x0.ge.x2-dsl) goto 30
c	incl	@20(ap)		;   htot = htot + 1
c        brb	40$		;   goto 40
c;       ....Vertical bit-error test
c30$:	cvtwl	(r5)[r9],r3	;   x3 = buf1(i)
c	cvtwl	(r7)[r9],r4	;   x4 = buf3(i)
c	addl3	r3,r4,r10
c	ashl	#-1,r10,r10	;   avg = (x3+x4)/2
c	subl3	ecl,r10,r11
c	cmpl	r0,r11
c	blss	35$		;   if (x0.lt.avg-ecl) goto 35
c	addl3	ecu,r10,r11
c	cmpl	r0,r11
c	bleq	50$		;   if (x0.le.avg+ecu) goto 50
c	addl3	dcu,r3,r11
c	cmpl	r0,r11
c	bleq	50$		;   if (x0.le.x3+dcu) goto 50
c	addl3	dcu,r4,r11
c	cmpl	r0,r11
c	bleq	50$		;   if (x0.le.x4+dcu) goto 50
c	incl	@24(ap)		;   tot = tot + 1
c	brb	40$		;   goto 40
c;
c35$:	subl3	dcl,r3,r11
c	cmpl	r0,r11
c	bgeq	50$		;35 if (x0.ge.x3-dcl) goto 50
c	subl3	dcl,r4,r11
c	cmpl	r0,r11
c	bgeq	50$		;   if (x0.ge.x4-dcl) goto 50
c	incl	@24(ap)		;   tot = tot + 1
c;
c40$:	cmpl	ibits,#0
c	bleq	42$		;40 if (ibits.le.0) goto 42
c	bicw3	lmask,r0,tmp1
c	bicw3	hmask,r10,tmp2  ;   x0 = ior(iand(lmask,x0),
c	bisw3	tmp1,tmp2,r0    ;          iand(hmask,avg))
c       brb     43$		;   goto 43
c42$:	movl	r10,r0		;42 x0 = avg
c;
c43$:	cmpl	idn,#0		;
c	bleq	45$		;43 if (idn.le.0) goto 45
c	cvtlw	idn,(r8)[r9]	;   obuf(i) = idn
C-----------------------------------------------------------------C
C   Changed the following statement (6/4/94) so that the ASSMBLY  C
C      code is correct                                            C
C-----------------------------------------------------------------C
c	brb	50$		;   goto 47
c45$:	cvtlw	r0,(r8)[r9]	;45 obuf(i) = x0
c;
C-----------------------------------------------------------------C
C   Registers must be saved and restored before and after the     C
C      call to dump_bdv                                           C
C-----------------------------------------------------------------C
c47$:	movl	r9,samp		; call dump_bdv(i)
c	incl	samp
c	callg	arg_list,dump_bdv
c;
c50$:	movl	r0,r1		; x1 = x0
c	movl	r2,r0		; x0 = x2
c	incl	r9		; i = i + 1
c	cmpl	r9,nso
c	blss	1010$		; if (i.lt.nso) goto 10
c	beql	3030$		; if (i.eq.nso) goto 30
c	ret			; return
c1010$:	jmp	10$
c3030$:	jmp	30$
c;                      4    8   12
c;  subroutine hutsar(buf2,obuf,htot,ssamp,nsamp)
c	.entry	hutsar,^m<r2,r3,r4,r5,r6,r7,r8,r9,r10,r11>
c	movl	4(ap),r6	;address of buf2
c	movl	8(ap),r8	;address of obuf
c	movl	@16(ap),r9	; i = ssamp
c	decl	r9		;
c	movl	@20(ap),nso	; nso = nsamp
c	cvtwl	(r6)[r9],r0	; x0 = buf2(ssamp)
c	cvtwl	2(r6)[r9],r2	; x2 = buf2(ssamp+1)
c	incl	r9		; i = i + 1
c	decl	nso		; ns = nso - 1
c;
c54$:				; do i=2,ns
c	movl 	r0,r1		; x1 = x0
c	movl	r2,r0		; x2 = x0
c	cvtwl	2(r6)[r9],r2
c	addl3	r1,r2,r10
c	ashl	#-1,r10,r10	; avg = (x1+x2)/2
c	subl3	esl,r10,r11
c	cmpl	r0,r11
c	blss	55$		; if (x0.lt.avg-esl) goto 55
c	addl3	esu,r10,r11
c	cmpl	r0,r11
c	bleq	90$		; if (x0.le.avg+esu) goto 90
c	addl3	dsu,r1,r11
c	cmpl	r0,r11
c	bleq	90$		; if (x0.le.x1+dsu) goto 90
c	addl3	dsu,r2,r11
c	cmpl	r0,r11
c	bleq	90$		; if (x0.le.x2+dsu) goto 90
c	brb	59$		; goto 59
c;
c55$:	subl3	dsl,r1,r11
c	cmpl	r0,r11
c	bgeq	90$		; if (x0.ge.x1-dsl) goto 90
c	subl3	dsl,r2,r11
c	cmpl	r0,r11
c	bgeq	90$		; if (x0.ge.x2-dsl) goto 90
c;
c59$:	incl	@12(ap)		;59 htot = htot + 1
c	cmpl	ibits,#0
c	bleq	60$		;   if (ibits.le.0) goto 60
c	bicw3	lmask,r0,tmp1
c	bicw3	hmask,r10,tmp2  ;   x0 = ior(iand(lmask,x0),
c	bisw3	tmp1,tmp2,r0    ;          iand(hmask,avg))
c        brb     62$		;   goto 62
c60$:	movl	r10,r0		;60 x0 = avg
c;
c62$:	cmpl	idn,#0		;
c	bleq	65$		;62 if (idn.le.0) goto 65
c	cvtlw	idn,(r8)[r9]	;   obuf(i) = idn
c	brb	70$		;   goto 70
c65$:	cvtlw	r0,(r8)[r9]	;65 obuf(i) = x0
c
C-----------------------------------------------------------------C
C   Registers must be saved and restored before and after the     C
C      call to dump_bdv                                           C
C-----------------------------------------------------------------C
c70$:	movl	r9,samp		; call dump_bdv(i)
c	incl	samp
c	callg	arg_list,dump_bdv
c
c90$:	aoblss	nso,r9,5654$	; enddo
c	ret			; return
c5654$:	jmp	54$
c	.end
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create adespike.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM adespike

   To Create the build file give the command:

		$ vimake adespike			(VMS)
   or
		% vimake adespike			(Unix)


************************************************************************/


#define PROGRAM	adespike
#define R2LIB

#define MODULE_LIST adespike.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_MATH77
/*    #define lib_local       remove for delivery      /*
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create adespike.pdf
process help=*
PARM INP TYPE=STRING
PARM OUT TYPE=STRING
PARM SIZE TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SUTOL TYPE=INTEGER DEFAULT=20
PARM SLTOL TYPE=INTEGER DEFAULT=20
PARM CUTOL TYPE=INTEGER DEFAULT=20
PARM CLTOL TYPE=INTEGER DEFAULT=20
PARM DSUTOL TYPE=INTEGER DEFAULT=20
PARM DSLTOL TYPE=INTEGER DEFAULT=20
PARM DCUTOL TYPE=INTEGER DEFAULT=20
PARM DCLTOL TYPE=INTEGER DEFAULT=20
PARM TOL TYPE=INTEGER DEFAULT=20
PARM DN TYPE=INTEGER DEFAULT=-1
PARM BITS TYPE=INTEGER DEFAULT=-1 VALID=(-1:16)
PARM PRINTTOL TYPE=KEYWORD VALID=(PRINT,NOPRINT) COUNT=0:1 DEFAULT=NOPRINT
END-PROC
!# ANNOT ICON = adespike
.TITLE
VICAR2 Program ADESPIKE
.HELP
PURPOSE:

ADESPIKE will detect and remove single-pixel errors (spikes).  A pixel
is determined to be in error if it differs from its adjacent neighbors
by more than certain specifiable thresholds.  Pixel errors are replaced
by the average of adjacent pixels.

ADESPIKE was originally developed to remove random telemetry transmission
errors (bit-errors) from flight images.  However, the algorithm is
completely general and can be applied to any image containing random noise
affecting individual pixels.  Independent tests are applied in the horizontal
and vertical directions.  Consequently, ADESPIKE can also detect and remove
lines or columns one pixel in width which deviate significantly from
neighboring lines or columns (e.g. line drop-outs).

.page
EXECUTION:

         ADESPIKE  INP  OUT  user-parameters...
where
    INP is the input image (byte or halfword).
    OUT is the output image.
The input image may be of arbitrary size.  The output image will be the
same data format as the input.

.page
OPERATION:
  Let x0 be the DN value of a given pixel, and let x1, x2, x3, and x4 be
the DN values of its neighbors:

                          x3
                       x1 x0 x2
                          x4

A horizontal test is first applied by comparing x0 with x1 and x2.  If x0
fails this test, it is replaced by the average of x1 and x2 and processing
continues (however, see BITS and DN parameters below).  Otherwise a vertical
test is applied by comparing x0 with x3 and x4.  If x0 fails this test, it
is replaced by the average of x3 and x4 and processing continues.  Otherwise
x0 is accepted as a valid pixel.

The horizontal test is as follows:  Let avg=(x1+x2)/2.  Then x0 is in error
if:
                 x0 < A     or   B < x0
where
       A = min(avg-SLTOL, x1-DSLTOL, x2-DSLTOL)
       B = max(avg+SUTOL, x1+DSUTOL, x2+DSUTOL)
The thresholds SLTOL, SUTOL, DSLTOL, and DSUTOL are specifiable parameters.

The vertical test is as follows:  Let avg=(x3+x4)/2.  Then x0 is in error
if:
                 x0 < C     or   D < x0
where
       C = min(avg-CLTOL, x3-DCLTOL, x4-DCLTOL)
       D = max(avg+CUTOL, x3+DCUTOL, x4+DCUTOL)
The thresholds CLTOL, CUTOL, DCLTOL, and DCUTOL are specifiable parameters.

ADESPIKE processes the pixels in the same order as they are stored in the
input file.  I.e., the image lines are processed in ascending order, and
the pixels for a given line are processed in ascending sample order.  Note
that when a pixel is corrected, the corrected value is used when testing
and replacing adjacent pixels (to the immediate right and below).

The pixel replacement value may be modified via the BITS or DN parameters.
Specifying BITS=n (where n is a positive integer) specifies that only the
n most-significant-bits of error-pixels are to be replaced.  For example,
BITS=3 specifies that the 3 most-significant-bits of x0 are to
be replaced, while the 5 least-significant-bits (assuming byte data)
remain unchanged.  If BITS=0 is specified, then the appropriate number
of bits to replace is automatically determined from the specified
thresholds.  The default (BITS=-1) is to replace all bits.

Specifying DN=m causes all error-pixels to be replaced by m.

.page
BINARY LABEL SUPPORT

References:  1) "Tracking GLL SSI Bad-Data Values, Binary Label Design"
  G. Yagi, MSD:384-89-152, May 22, 1989.
             2) "Tracking GLL SSI Bad-Data Values, Software Requirements"
  G. Yagi, MSD:384-89-157, June 30, 1989.

If the input image contains binary labels (Ref. 1), then ADESPIKE will copy
the binary labels to the output file and add information regarding all spikes
that it has identified and removed. If the binary labels contain information
regarding data drop-outs, ADESPIKE will use this information to avoid using
data drop-outs in the detection and interpolation process.

.page
TIMING:

The following times were measured for an 800x800 byte image:
    5.8 CPU sec on 8650
    32.5 CPU sec on 780

WRITTEN BY: P. L. Jepsen	12 March 74
CONVERTED TO VAX BY: L. W. Kamp,  12 April 1983
COGNIZANT PROGRAMMER: Gary Yagi
REVISIONS:
 21 JUL 95  A. Scop      (CRI) As per FR87387 misspelled variable INU
                         corrected to IUN in XVTRANS call.
 16 MAR 95  A. Scop      (CRI) updated tst pdf as per FR85748.  Tst pdf
                         presently can't run on ANDES because galsos not
                         ported to ANDES because sybase is not present.
  1 JUL 94  M. Cox       (CRI) Made portable for UNIX
  6 APR 93  H. Mortensen Initialize LASTPIX buffer with nso and not 0.
                         Added printing of tolerance values. Zeroed out
                         bdv_rec after each XVWRIT. Added call to DUMP_BDV
          	         from the subroutine AUTSAR and fixed branching to 
                         to the subroutine DUMP_BDV call in the subroutine 
                         HUTSAR.
 21 Aug 91  W.P. Lee     Modified Test PDF for REDR processing 
 28 Mar 91  W.P. Lee     Enabled ADESPIKE to work on BYTE image with BINARY
                         Labels retained and updated 
 15 Oct 90  L.W.Kamp     Fixed bug in end-of-line test in AUTSAR;  removed
			 U_FORMAT=HALF for binary labels & added byte buffer
			 to WORKB.
 30 Aug 89  A. Runkle	 Read, copy and generate binary label information.
			 Ignore data drop-outs when enumerated in binary label.
 15 Feb 89  G. Yagi	 Major revisions to help file.
			 Fixed minor bug in BITS parameter.
			 Fixed processing of second line.
 31 May 85  L. Kamp	 Conversion to Vicar2; added STACKA
  7 MAY 85  B. Barkan	 ASSEMBLERIZED PART OF CODE
 10 JAN 84  C. Avis	 BUG FIXED FOR FR
 12 APR 83  L. Kamp	 CONVERT TO VAX: REMOVE BAL CODE (USE UCL AUTSAR)
		         MOVE PARAM CALL BEFORE OPEN CALL
		         READ BYTE DATA AS HALFWORD
 12 MAR 80  G. Yagi      REWRITE TO SPEED UP EXECUTION
 27 JUN 75  DAH		 CHANGES FOR CONVERSION TO 360/OS
 16 MAR 74               MVM73 MODIFICATIONS
 12 MAR 74   ...VIKING LANDER VERSION...   INITIAL RELEASE
.LEVEL1
.VARI INP
Input filename.
.VARIABLE OUT
Output filename.
.VARI SIZE
Image size field.
.VARI SUTOL
Hor. upper dev. from mean
.VARI SLTOL
Hor. lower dev. from mean
.VARI CUTOL
Vert. upper dev. from mean
.VARI CLTOL
Vert. lower dev. from mean
.VARI DSUTOL
Upper deviation from left or
right adjacent pixels.
.VARI DSLTOL
Lower deviation from left or
right adjacent pixels.
.VARI DCUTOL
Upper deviation from top or
bottom adjacent pixel.
.VARI DCLTOL
Lower deviation from top or
bottom adjacent pixel.
.VARI TOL
Ex: TOL=n
Set all thresholds to n.
.VARI DN
Replacement DN
.VARI BITS
# of bits replaced
.VARI PRINTTOL
Prints tolerance values.
For debugging purposes.
.LEVEL2
.VARI INP
Ex:	INP=A
where A is the input image (byte or halfword) in standard VICAR format
and of arbitrary size.
.VARIABLE OUT
Ex:	OUT=B
where B is the output image.  The output image will be the same data format
and size as the input image.
.VARI SIZE
	SIZE=(sl,ss,nl,ns).
is the standard VICAR2 image size field, specifying the area of the input
on which processing is to be applied (starting-line, starting-sample,
number-of-lines, number-of-samples).  The output image size will be nlxns.
If defaulted, the entire input image is processed.
.VARI SUTOL
Horizontal upper deviation from mean (default=20).  Parameters SUTOL,
SLTOL, DSUTOL, and DSLTOL are thresholds applied to the horizontal pixel test:
Let x0 be the pixel to be tested, and let x1 and x2 be its left and right
adjacent neighbors.  Let avg=(x1+x2)/2.  Then x0 is in error if:
                 x0 < A     or   B < x0
where
       A = min(avg-SLTOL, x1-DSLTOL, x2-DSLTOL)
       B = max(avg+SUTOL, x1+DSUTOL, x2+DSUTOL)
.VARI SLTOL
Horizontal lower deviation from mean (default=20).  See SUTOL for details.
.VARI DSUTOL
Horizontal upper deviation from neighbor (default=20).  See SUTOL for details.
.VARI DSLTOL
Horizontal lower deviation from neighbor (default=20).  See SUTOL for details.
.VARI CUTOL
Vertical upper deviation from mean (default=20).  Parameters CUTOL
CLTOL, DCUTOL, and DCLTOL are thresholds applied to the vertical pixel test:
Let x0 be the pixel to be tested, and let x3 and x4 be its upper and lower
adjacent neighbors.  Let avg=(x3+x4)/2.  Then x0 is in error if:
                 x0 < C     or   D < x0
where
       C = min(avg-CLTOL, x3-DCLTOL, x4-DCLTOL)
       D = max(avg+CUTOL, x3+DCUTOL, x4+DCUTOL)
The thresholds CLTOL, CUTOL, DCLTOL, and DCUTOL are specifiable parameters.
.VARI CLTOL
Vertical lower deviation from mean (default=20).  See CUTOL for details.
.VARI DCUTOL
Vertical upper deviation from neighbor (default=20).  See CUTOL for details.
.VARI DCLTOL
Vertical lower deviation from neighbor (default=20).  See CUTOL for details.
.VARI TOL
All eight thresholds (SUTOL,...,DCLTOL) are set to this value.
.VARI DN
Ex:	DN=n
Specifies the DN value for replacement pixels.  All pixel errors are
replaced by n (rather then the average of adjacent pixels).
A negative value is ignored (default of -1), i.e. the average of adjacent
pixels is used to replace pixel errors.
.VARI BITS
This parameter determines number of bits to replace in spikes. 
The valid range is -1 to 16 (values from 9-16 are meaningful
only for HALF format, and are ignored for BYTE).

Let x0 be the erroneous pixel, and let avg=(x1+x2)/2 where x1 and x2 are
its adjacent neighbors.  If BITS>0, then the BITS most significant bits
(MSB) of pixel x0 are replaced with the BITS MSBs of avg and the remaining
LSBs of x0 are not changed.

If BITS=0, then the number of MSBs to be changed when a pixel spike is
detected is determined by the mean deviation allowed.

If BITS=-1, then the entire pixel DN is changed.  (This is equivalent
to BITS=8 for BYTE data.)

For example (using byte data):
If BITS=3, the 3 MSBs of x0 are replaced with the 3 MSBs of avg.
If BITS=0, then:
  If the default tolerances are used (20 DN), then the 4 MSBs will be
  replaced, because 2**4-1 < 20 < 2**5-1.
  If all tolerances are 40 DN, then the 3 MSBs will be replaced, because
  2**5-1 < 40 < 2**6-1.
.VARI PRINTTOL
Prints all 8 tolerance thresholds.  For debugging purposes.  Default is NOPRINT.
.end
$ Return
$!#############################################################################
$Test_File:
$ create tstadespike.pdf
Procedure
refgbl $echo
refgbl $autousage
Body
let $autousage="none"
Let _onfail="continue"

local dirg string initial="/project/test_work/testdata/sitod1/test_data/images/"
local dirv string initial="/project/test_work/testdata/mipl/vgr/"

Write " "
refgbl $syschar
!
Let $echo="yes"
! test of program ADESPIKE
! BYTE data:
!
gen a 10 10 IVAL=100 LINC=2 SINC=0
sargonb a c ZERO 0 (6,5,6,7,7,5)
insert c b LINE=8 DN=130 'TRUNC
list b
adespike b c TOL=10 'print
list c
!
! line fill mode
!
sargonb a d ZERO 0 (6,1,5,5,6,10,7,6)
list d
adespike d e 
list e
!
! parameter BITS:
!
adespike d e BITS=3
list e
! HALF data:
gen h 10 20 IVAL=100 LINC=2 SINC=0 'HALF
sargonb h f ZERO 0 (6,1,5,5,6,10,7,6)
adespike f g 
list g
! Parameter DN:
adespike f g DN=1234
list g
! parameter BITS with HALF data:
adespike f g BITS=11
list g
!
!  Test on byte image with binary headers:
!
adespike &"dirv"e2575251.6 vgrdespike.fil
list &"dirv"e2575251.6 (201,21,10,10)
list vgrdespike.fil (201,21,10,10)
!
!      Test REDR   (8-21-1991   WPL)
!
!   BADLABEL first to create REDR,  followed by  Adespike
! (this test had to be removed as pgm. badlabels is no longer in P2
! 2010-05-11 lwk)
!badlabels &"dirg"s0061509700.1 redr.1 ENTROPY=NO
!label-list  |stdout=before.vic| redr.1
!  
!   Run ADESPIKE  on REDR
!
!gedrlist |stdout=before.bin| redr.1 'bad
!adespike  redr.1  redr.2  
!gedrlist |stdout=after.bin| redr.2 'bad
!
!if ($syschar(1) = "VAX_VMS")
!   dcl diff before.bin after.bin /output=diff.bin
!else
!   ush diff before.bin after.bin >diff.bin
!end-if
!
!label-list  |stdout=after.vic| redr.2
!
!if ($syschar(1) = "VAX_VMS")
!   dcl diff before.vic after.vic /output=diff.vic
!else
!   ush diff before.vic after.vic >diff.vic
!end-if
!
!      Test EDR   
!
! As of March 1995, the call to galsos will not work on ANDES because
! it is not ported on that platform.  To test adespike on andes, the
! following should be commented out and the output file should be used
! from the wasatch (or any other platform) run.
!
!if ($syschar(1) = "VAX_VMS")
!    galsos redr.1 galsos_out_edr.1 dir="dev:[gmy059.cal]"  +
!           offset=calibration.so 'nocheck cal=vlt100.cal   +
!           dc=2f30.dc blem=vlt2f.blm solrange=108208799.
!else
!    galsos redr.1 galsos_out_edr.1 dir="/home/gmy/cal"    +
!           offset=calibration.so 'nocheck cal=vlt100.cal  +
!           dc=2f30.dc blem=vlt2f.blm solrange=108208799.
!end-if
!
!label-list  |stdout=before.vic| galsos_out_edr.1
!  
!   Run ADESPIKE  on REDR
!
!gedrlist |stdout=before.bin| galsos_out_edr.1 'bad
!adespike  galsos_out_edr.1  edr.2 tol=600
!gedrlist |stdout=after.bin| edr.2 'bad
!
!if ($syschar(1) = "VAX_VMS")
!   dcl diff before.bin after.bin /output=diff1.bin
!else
!   ush diff before.bin after.bin >diff1.bin
!end-if
!
!label-list  |stdout=after.vic| edr.2
!
!if ($syschar(1) = "VAX_VMS")
!   dcl diff before.vic after.vic /output=diff1.vic
!else
!   ush diff before.vic after.vic >diff1.vic
!end-if
!
if ($syschar(1) = "VAX_VMS")
   dcl delete *.z*;*
   dcl delete vgrdespike.fil;*
   dcl delete redr*.*;*
   dcl delete edr*.*;*
else
   ush rm a
   ush rm b
   ush rm c
   ush rm d
   ush rm e
   ush rm f
   ush rm g
   ush rm h
   ush rm vgrdespike.fil
!   ush rm redr*.*
!   ush rm edr*.*
end-if
!
Let $Echo="NO"
End-proc

$!-----------------------------------------------------------------------------
$ create tstadespike.log_solos
tstadespike
 
gen a 10 10 IVAL=100 LINC=2 SINC=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
sargonb a c ZERO 0 (6,5,6,7,7,5)
Beginning VICAR task sargonb
SARGONB version 01-JUL-94
insert c b LINE=8 DN=130 'TRUNC
Beginning VICAR task insert
INSERT version 02-MAY-94
LINE INSERTED          8
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Jan 15 17:05:37 2013
 Task:INSERT    User:lwk       Date_Time:Tue Jan 15 17:05:38 2013
     Samp     1       3       5       7       9
   Line
      1     100 100 100 100 100 100 100 100 100 100
      2     102 102 102 102 102 102 102 102 102 102
      3     104 104 104 104 104 104 104 104 104 104
      4     106 106 106 106 106 106 106 106 106 106
      5     108 108 108 108 108 108 108 108 108 108
      6     110 110 110 110   0   0   0 110 110 110
      7     112 112 112 112   0 112 112 112 112 112
      8     130 130 130 130 130 130 130 130 130 130
      9     114 114 114 114 114 114 114 114 114 114
     10     116 116 116 116 116 116 116 116 116 116
adespike b c TOL=10 'print
Beginning VICAR task adespike
ADESPIKE version 21-JUL-95
    SLTOL=    10    SUTOL=    10    DSLTOL=    10    DSUTOL=    10
    CLTOL=    10    CUTOL=    10    DCLTOL=    10    DCUTOL=    10

    TOTAL NUMBER OF PIXELS CORRECTED USING ADJACENT SCAN LINE TEST           11
    TOTAL NUMBER OF PIXELS CORRECTED USING SAME SCAN LINE TEST               2
list c
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Jan 15 17:05:37 2013
 Task:ADESPIKE  User:lwk       Date_Time:Tue Jan 15 17:05:39 2013
     Samp     1       3       5       7       9
   Line
      1     100 100 100 100 100 100 100 100 100 100
      2     102 102 102 102 102 102 102 102 102 102
      3     104 104 104 104 104 104 104 104 104 104
      4     106 106 106 106 106 106 106 106 106 106
      5     108 108 108 108 108 108 108 108 108 108
      6     110 110 110 110   0 110 110 110 110 110
      7     112 112 112 112 112 112 112 112 112 112
      8     113 113 113 113 113 113 113 113 113 113
      9     114 114 114 114 114 114 114 114 114 114
     10     116 116 116 116 116 116 116 116 116 116
sargonb a d ZERO 0 (6,1,5,5,6,10,7,6)
Beginning VICAR task sargonb
SARGONB version 01-JUL-94
list d
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Jan 15 17:05:37 2013
 Task:SARGONB   User:lwk       Date_Time:Tue Jan 15 17:05:39 2013
     Samp     1       3       5       7       9
   Line
      1     100 100 100 100 100 100 100 100 100 100
      2     102 102 102 102 102 102 102 102 102 102
      3     104 104 104 104 104 104 104 104 104 104
      4     106 106 106 106 106 106 106 106 106 106
      5     108 108 108 108   0 108 108 108 108 108

      7     112 112 112 112 112   0 112 112 112 112
      8     114 114 114 114 114 114 114 114 114 114
      9     116 116 116 116 116 116 116 116 116 116
     10     118 118 118 118 118 118 118 118 118 118
adespike d e
Beginning VICAR task adespike
ADESPIKE version 21-JUL-95
    TOTAL NUMBER OF PIXELS CORRECTED USING ADJACENT SCAN LINE TEST            9
    TOTAL NUMBER OF PIXELS CORRECTED USING SAME SCAN LINE TEST               2
list e
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Jan 15 17:05:37 2013
 Task:ADESPIKE  User:lwk       Date_Time:Tue Jan 15 17:05:40 2013
     Samp     1       3       5       7       9
   Line
      1     100 100 100 100 100 100 100 100 100 100
      2     102 102 102 102 102 102 102 102 102 102
      3     104 104 104 104 104 104 104 104 104 104
      4     106 106 106 106 106 106 106 106 106 106
      5     108 108 108 108 108 108 108 108 108 108
      6     110 110 110 110 110   0 110 110 110 110
      7     112 112 112 112 112 112 112 112 112 112
      8     114 114 114 114 114 114 114 114 114 114
      9     116 116 116 116 116 116 116 116 116 116
     10     118 118 118 118 118 118 118 118 118 118
adespike d e BITS=3
Beginning VICAR task adespike
ADESPIKE version 21-JUL-95
    TOTAL NUMBER OF PIXELS CORRECTED USING ADJACENT SCAN LINE TEST            9
    TOTAL NUMBER OF PIXELS CORRECTED USING SAME SCAN LINE TEST               2
list e
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Jan 15 17:05:37 2013
 Task:ADESPIKE  User:lwk       Date_Time:Tue Jan 15 17:05:41 2013
     Samp     1       3       5       7       9
   Line
      1     100 100 100 100 100 100 100 100 100 100
      2     102 102 102 102 102 102 102 102 102 102
      3     104 104 104 104 104 104 104 104 104 104
      4     106 106 106 106 106 106 106 106 106 106
      5     108 108 108 108  96 108 108 108 108 108
      6      96  96  96  96  96   0  96  96  96  96
      7     112 112 112 112 112  96 112 112 112 112
      8     114 114 114 114 114 114 114 114 114 114
      9     116 116 116 116 116 116 116 116 116 116
     10     118 118 118 118 118 118 118 118 118 118
gen h 10 20 IVAL=100 LINC=2 SINC=0 'HALF
Beginning VICAR task gen
GEN Version 6
GEN task completed
sargonb h f ZERO 0 (6,1,5,5,6,10,7,6)
Beginning VICAR task sargonb
SARGONB version 01-JUL-94
adespike f g
Beginning VICAR task adespike
ADESPIKE version 21-JUL-95
    TOTAL NUMBER OF PIXELS CORRECTED USING ADJACENT SCAN LINE TEST            8
    TOTAL NUMBER OF PIXELS CORRECTED USING SAME SCAN LINE TEST               3
list g
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Jan 15 17:05:41 2013
 Task:ADESPIKE  User:lwk       Date_Time:Tue Jan 15 17:05:42 2013
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14    15
   Line
      1       100   100   100   100   100   100   100   100   100   100   100   100   100   100   100
      2       102   102   102   102   102   102   102   102   102   102   102   102   102   102   102
      3       104   104   104   104   104   104   104   104   104   104   104   104   104   104   104
      4       106   106   106   106   106   106   106   106   106   106   106   106   106   106   106
      5       108   108   108   108   108   108   108   108   108   108   108   108   108   108   108
      6       110   110   110   110   110     0   110   110   110   110   110   110   110   110   110
      7       112   112   112   112   112   112   112   112   112   112   112   112   112   112   112
      8       114   114   114   114   114   114   114   114   114   114   114   114   114   114   114
      9       116   116   116   116   116   116   116   116   116   116   116   116   116   116   116
     10       118   118   118   118   118   118   118   118   118   118   118   118   118   118   118

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Jan 15 17:05:41 2013
 Task:ADESPIKE  User:lwk       Date_Time:Tue Jan 15 17:05:42 2013
     Samp      16    17    18    19    20
   Line
      1       100   100   100   100   100
      2       102   102   102   102   102
      3       104   104   104   104   104
      4       106   106   106   106   106
      5       108   108   108   108   108
      6       110   110   110   110   110
      7       112   112   112   112   112
      8       114   114   114   114   114
      9       116   116   116   116   116
     10       118   118   118   118   118
adespike f g DN=1234
Beginning VICAR task adespike
ADESPIKE version 21-JUL-95
    TOTAL NUMBER OF PIXELS CORRECTED USING ADJACENT SCAN LINE TEST            7
    TOTAL NUMBER OF PIXELS CORRECTED USING SAME SCAN LINE TEST               3
list g
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Jan 15 17:05:41 2013
 Task:ADESPIKE  User:lwk       Date_Time:Tue Jan 15 17:05:43 2013
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14    15
   Line
      1       100   100   100   100   100   100   100   100   100   100   100   100   100   100   100
      2       102   102   102   102   102   102   102   102   102   102   102   102   102   102   102
      3       104   104   104   104   104   104   104   104   104   104   104   104   104   104   104
      4       106   106   106   106   106   106   106   106   106   106   106   106   106   106   106
      5       108   108   108   108  1234   108   108   108   108   108   108   108   108   108   108
      6      1234  1234  1234  1234     0     0  1234  1234  1234  1234   110   110   110   110   110
      7       112   112   112   112   112  1234   112   112   112   112   112   112   112   112   112
      8       114   114   114   114   114   114   114   114   114   114   114   114   114   114   114
      9       116   116   116   116   116   116   116   116   116   116   116   116   116   116   116
     10       118   118   118   118   118   118   118   118   118   118   118   118   118   118   118

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Jan 15 17:05:41 2013
 Task:ADESPIKE  User:lwk       Date_Time:Tue Jan 15 17:05:43 2013
     Samp      16    17    18    19    20
   Line
      1       100   100   100   100   100
      2       102   102   102   102   102
      3       104   104   104   104   104
      4       106   106   106   106   106
      5       108   108   108   108   108
      6       110   110   110   110   110
      7       112   112   112   112   112
      8       114   114   114   114   114
      9       116   116   116   116   116
     10       118   118   118   118   118
adespike f g BITS=11
Beginning VICAR task adespike
ADESPIKE version 21-JUL-95
    TOTAL NUMBER OF PIXELS CORRECTED USING ADJACENT SCAN LINE TEST            8
    TOTAL NUMBER OF PIXELS CORRECTED USING SAME SCAN LINE TEST               3
list g
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Jan 15 17:05:41 2013
 Task:ADESPIKE  User:lwk       Date_Time:Tue Jan 15 17:05:43 2013
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14    15
   Line
      1       100   100   100   100   100   100   100   100   100   100   100   100   100   100   100
      2       102   102   102   102   102   102   102   102   102   102   102   102   102   102   102
      3       104   104   104   104   104   104   104   104   104   104   104   104   104   104   104
      4       106   106   106   106   106   106   106   106   106   106   106   106   106   106   106
      5       108   108   108   108    96   108   108   108   108   108   108   108   108   108   108
      6        96    96    96    96    96     0    96    96    96    96   110   110   110   110   110
      7       112   112   112   112   112    96   112   112   112   112   112   112   112   112   112
      8       114   114   114   114   114   114   114   114   114   114   114   114   114   114   114
      9       116   116   116   116   116   116   116   116   116   116   116   116   116   116   116
     10       118   118   118   118   118   118   118   118   118   118   118   118   118   118   118

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Jan 15 17:05:41 2013
 Task:ADESPIKE  User:lwk       Date_Time:Tue Jan 15 17:05:43 2013
     Samp      16    17    18    19    20
   Line
      1       100   100   100   100   100
      2       102   102   102   102   102
      3       104   104   104   104   104
      4       106   106   106   106   106
      5       108   108   108   108   108
      6       110   110   110   110   110
      7       112   112   112   112   112
      8       114   114   114   114   114
      9       116   116   116   116   116
     10       118   118   118   118   118
adespike /project/test_work/testdata/mipl/vgr/e2575251.6 vgrdespike.fil
Beginning VICAR task adespike
ADESPIKE version 21-JUL-95
    TOTAL NUMBER OF PIXELS CORRECTED USING ADJACENT SCAN LINE TEST        40783
    TOTAL NUMBER OF PIXELS CORRECTED USING SAME SCAN LINE TEST             240
list /project/test_work/testdata/mipl/vgr/e2575251.6 (201,21,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:VEDR      User:SMT059    Date_Time:Fri Aug  5 09:35:04 1988
     Samp    21      23      25      27      29
   Line
    201      19  19  21  19  19  20  19  19  19  20

    203      19  20  19  20  20  19  20  20  19  19

    205      19  19  20  20  20  18  19  21  20  21

    207      20  19  19  19  20  19  19  19  19  19

    209      19  19  19  19  19  21  19  20  20  19
list vgrdespike.fil (201,21,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:VEDR      User:SMT059    Date_Time:Fri Aug  5 09:35:04 1988
 Task:ADESPIKE  User:lwk       Date_Time:Tue Jan 15 17:05:44 2013
     Samp    21      23      25      27      29
   Line
    201      19  19   0  19  19  20  19  19  19  20

    203      19  20  19  20  20  19  20  20  19  19

    205      19  19  20  20  20  18  19   0  20   0

    207      20  19  19  19  20  19  19  19  19  19

    209      19  19  19  19  19   0  19  20  20  19
if ($syschar(1) = "VAX_VMS")
else
   ush rm a
   ush rm b
   ush rm c
   ush rm d
   ush rm e
   ush rm f
   ush rm g
   ush rm h
   ush rm vgrdespike.fil
end-if
Let $Echo="NO"
exit
slogoff
$ Return
$!#############################################################################
