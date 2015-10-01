$!****************************************************************************
$!
$! Build proc for MIPL module vgrfillin
$! VPACK Version 1.8, Thursday, January 19, 1995, 11:26:04
$!
$! Execute by entering:		$ @vgrfillin
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
$ write sys$output "*** module vgrfillin ***"
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
$ write sys$output "Invalid argument given to vgrfillin.com file -- ", primary
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
$   if F$SEARCH("vgrfillin.imake") .nes. ""
$   then
$      vimake vgrfillin
$      purge vgrfillin.bld
$   else
$      if F$SEARCH("vgrfillin.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake vgrfillin
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @vgrfillin.bld "STD"
$   else
$      @vgrfillin.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create vgrfillin.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack vgrfillin.com -
	-s vgrfillin.f -
	-i vgrfillin.imake -
	-p vgrfillin.pdf -
	-t tstvgrfillin.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create vgrfillin.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
C  PROGRAM VGRFILLIN:  VOYAGER line-fill program.....
C           VGRFILLIN  IN  OUT
C where IN is a Disk EDR (with engineering data attached) and OUT
C is a standard VICAR image.
C
      SUBROUTINE MAIN44
      COMMON/C1/HDR(248),HIST(1024)
      COMMON/C1/BUF(512,22),MFP(10,22),VPP(2,22),SS,NS
      COMMON/LCOUNT/ NLFILL, NLTRUNC, NLGORE
      byte	HDR
      INTEGER*2 BUF,MFP,VPP
      INTEGER*2 SS(10),NS(10)
      INTEGER	IMCTEMP,IMCODE,SCAN_RATE,SLT,ELT,MAXBUF/22/
      CHARACTER*5	FORMAT
      LOGICAL	TRUNC    
      CHARACTER*80  PMSG
C
      CALL IFMESSAGE('VGRFILLIN Version 6-Mar-95')
      NL     = 800
      NLFILL = 0
      TRUNC  = .FALSE.
C
      CALL XVUNIT(IUNIT,'INP',1,IND,' ')
      CALL XVOPEN(IUNIT,IND,'OPEN_ACT','SA','IO_ACT','SA',
     &        'COND','BINARY',' ')
      CALL XVGET(IUNIT,IND,'NL',NLI,'NS',NSI,'NLB',
     &                   NLB,'NBB',NBB,' ')
      IF (NLI.NE.800.OR.NSI.NE.800.OR.NLB.NE.2.OR.NBB.NE.224)
     &                    GOTO 980

      CALL XVUNIT(OUNIT,'OUT',1,IND,' ')
      CALL XVOPEN(OUNIT,IND,'OPEN_ACT','SA','IO_ACT','SA',
     &        'OP','WRITE',' ')
      CALL XVREAD(IUNIT,HDR,IND,'NSAMPS',248,' ')
c      CALL BITS(HDR(60),1,6,IMCODE)
      if (hdr(119).LT.0) then
        imctemp =((256)+HDR(119))/2
      else 
        imctemp = HDR(119)/2
      endif
      imcode = mod(imctemp,64)
      CALL VGRIMFMT(IMCODE,FORMAT,SCAN_RATE,SS,NS,IND)
      IF (IND.NE.1) THEN
            CALL XVMESSAGE('***it1it1Invalid Image Format',' ')
            GOTO 990
      ENDIF
      CALL XVREAD(IUNIT,HIST,IND,' ')

C     ....First check for some easy cases:
      IF (IMCODE.EQ.15 .OR. IMCODE.EQ.17) THEN
	CALL XVMESSAGE('PWS/PRA DATA: NO FILLIN DONE',' ')
	CALL VGCOPY(IUNIT,OUNIT,BUF,NL)
	GO TO 100
      ELSEIF (IMCODE.EQ.2) THEN		!IMS
	CALL XVMESSAGE('FILLIN HAS BEEN DONE BY IMBUILD',' ')
	CALL VGCOPY(IUNIT,OUNIT,BUF,NL)
	GO TO 100
      ENDIF
C
C     ....Unless old minor-frame format, no partial lines.
      IF (IMCODE.NE.9 .AND. IMCODE.NE.11
     &   .AND. IMCODE.NE.20 .AND. IMCODE.NE.21 .AND. IMCODE.NE.22
     &   .AND. IMCODE.NE.27 .AND. IMCODE.NE.31) SCAN_RATE=1
C
C     ....For alternate pixel editing, fix NS
      IF (IMCODE.EQ.5) NS(1)=740	!IM-21
      IF (IMCODE.EQ.7) NS(1)=800	!IM-22
      IF (IMCODE.EQ.13) NS(1)=762	!IM-23
      IF (IMCODE.EQ.19) NS(1)=87	!OC-3
      IF (IMCODE.EQ.26) NS(1)=646	!IM-2D
C
C     ....Editing of top and bottom of frame
      IF (IMCODE.EQ.6) THEN		!IM-Q
         SLT = 1			!Starting-line transmitted
         NLT = 480 			!Number-of-lines transmitted
      ELSE IF (IMCODE.EQ.14) THEN	!IM-24
         SLT = 301
         NLT = 195
      ELSE IF (IMCODE.EQ.25) THEN	!IM-25
         SLT = 101
         NLT = 585
      ELSE IF (IMCODE.EQ.23) THEN	!IM-26
         SLT = 271
         NLT = 266
      ELSE IF (IMCODE.EQ.16) THEN	!IM-2W
         SLT = 1
         NLT = 720
      ELSE
         SLT = 1
         NLT = 800
      ENDIF
C
C     ....Data compression modes IMO,IMQ,IMK,IM2c,IM26
      IF (IMCODE.EQ.4 .OR. IMCODE.EQ.6 .OR. IMCODE.EQ.8 .OR.
     & IMCODE.EQ.12 .OR. IMCODE.EQ.23) TRUNC=.TRUE.

      IF (SLT.GT.1) CALL VGCOPY(IUNIT,OUNIT,BUF,SLT-1)

      I1 = 1
      I2 = 2
      NLFILL = 0
      NLTRUNC = 0
      NLGORE = 0
      ELT = SLT + NLT - 1
      NLX = ELT + MAXBUF - 2
      N = 0
      DO L=SLT,NLX
         N = N + 1
         I3 = I1
         I1 = I2
         I2 = MOD(I2,MAXBUF) + 1
         IF (L.LE.ELT) THEN
            CALL XVREAD(IUNIT,BUF(1,I3),ind,' ')
            CALL GETMFP(BUF(3,I3),mfp(1,I3),SCAN_RATE,vpp(1,i3))
         ELSE
            CALL ZIA(mfp(1,i3),5)
         ENDIF
         IF (N.GE.MAXBUF-1) THEN
           CALL FILLIN(buf(113,1),I1,I2,SCAN_RATE,TRUNC)
           CALL XVWRIT(OUNIT,BUF(113,I2),ind,' ')
         ENDIF
      ENDDO
      IF (ELT.NE.800) CALL VGCOPY(IUNIT,OUNIT,BUF,800-ELT)

C     ....Write filled line count to label
  100 CALL XLADD( OUNIT, 'HISTORY', 'LIN_CNT', NLFILL, STAT, 
     . 'FORMAT', 'INT', 'ERR_ACT', 'SA',' ')
	PMSG='# MISSING LINES = '
	WRITE(PMSG(20:25), '(I6)' )  NLFILL
	CALL XVMESSAGE(PMSG,' ')
      IF (TRUNC) THEN
	PMSG='# TRUNCATED LINES = '
	WRITE(PMSG(22:27), '(I6)' )  NLTRUNC
	CALL XVMESSAGE(PMSG,' ')
	PMSG='# LINES WITH GORES = '
	WRITE(PMSG(22:27), '(I6)' )  NLGORE
	CALL XVMESSAGE(PMSG,' ')
      ENDIF

      CALL XVCLOSE(IUNIT,IND,' ')
      CALL XVCLOSE(OUNIT,IND,' ')
      RETURN
  980 CALL XVMESSAGE('***Input image is not a valid DEDR',' ')
      CALL ABEND
  990 CALL XVMESSAGE('***VGRFILLIN task cancelled',' ')
      CALL ABEND
      END
C
C Determine if minor-frame is present by examining EDR line-header (LHDR).
C Outputs: MFP(I)=1  if minor-frame is present  (I=1 to SCAN_RATE)
C                =0  if missing
C          VPP(1)=first valid pixel
C          VPP(2)=last valid pixel
C VPP is for edited frames where the left and right margins of the picture are
C not transmitted.
C
      SUBROUTINE GETMFP(LHDR,mfp,SCAN_RATE,vpp)
      INTEGER*2 MFP(10),VPP(2),ITP1,ITP2
      INTEGER SCAN_RATE
      byte    LHDR(220)
C
       itp1= LHDR(217)
       itp2= LHDR(218)
       if (itp1.lt.0) itp1 = 256+itp1
       if (itp2.lt.0) itp1 = 256+itp2
       VPP(1) = 256*itp2 + itp1
       itp1= LHDR(219)
       itp2= LHDR(220)
       if (itp1.lt.0) itp1 = 256+itp1
       if (itp2.lt.0) itp1 = 256+itp2
       VPP(2) = 256*itp2 + itp1
           DO I=1,SCAN_RATE
       itp1= LHDR(2*(I+85)-1)
       itp2= LHDR(2*(I+85))
       if (itp1.lt.0) itp1 = 256+itp1
       if (itp2.lt.0) itp1 = 256+itp2
       itp1 = 256*itp2 + itp1
c              IF (LHDR(I+85).GT.0) THEN    
               if (itp1.gt.0) then
                  MFP(I) = 1		!DATA PRESENT
              ELSE
                  MFP(I) = 0		!DATA MISSING
              ENDIF
           ENDDO
C     ...Check for possible bad data from IMBUILD:
      IF (VPP(1).LT.1) VPP(1)=1
      IF (VPP(2).GT.800) VPP(2)=800
      RETURN
      END
C Fill in missing lines and missing pixels on a line.
C
      SUBROUTINE FILLIN(IN,I1,I2,SCAN_RATE,TRUNC)
      COMMON/C1/HDR(248),HIST(1024)
      COMMON/C1/BUF(512,22),MFP(10,22),VPP(2,22),SS,NS
      COMMON/LCOUNT/ NLFILL, NLTRUNC, NLGORE
      byte      HDR
      INTEGER*2 BUF,MFP,VPP
      INTEGER*2 SS(10),NS(10)
      INTEGER	SCAN_RATE
      INTEGER	MAXBUF/22/	!MUST BE EVEN FOR TRUNCATION-FILL ALGORITHM
      BYTE	IN(1024,22)
      LOGICAL	FILLED,GORE,TRUNC

C     ....First do truncation-fill if necessary:
      IF (.NOT.TRUNC) GOTO 20  !Skip if no truncation
      IF (VPP(1,I2).EQ.1 .AND. VPP(2,I2).EQ.800) GOTO 20  !Skip if no trunc
      IF (MFP(1,I2).EQ.0 .OR. MFP(1,I1).EQ.0) GOTO 20     !Skip if no data
      GORE = .FALSE.
      K = 0
      J = I2 - 1
C     ....Search for next even/odd line
   10 J = MOD(J,MAXBUF) + 1
      J = MOD(J,MAXBUF) + 1	!Every other line
      IF (J.EQ.I1) GOTO 20
      K = K + 2
      IF (MFP(1,J).EQ.0) GOTO 10

      IF (VPP(1,I2).NE.1) THEN	!LINE IS RIGHT-ALIGNED (EVEN)
         ISS = 1
         INS = VPP(1,I2)-1
         IF (VPP(2,I1).LT.INS .OR. VPP(2,J).LT.INS) THEN
            GORE = .TRUE.
            INS = MIN(VPP(2,I1),VPP(2,J))
            JSS = INS
            JNS = VPP(1,I2)-JSS+1
         ENDIF
         VPP(1,I2) = 1
      ELSE			!LINE IS LEFT-ALIGNED (ODD)
         ISS = VPP(2,I2)+1
         INS = 800-VPP(2,I2)
         IF (VPP(1,I1).GT.ISS .OR. VPP(1,J).GT.ISS) THEN
            GORE = .TRUE.
            ISS = MAX(VPP(1,I1),VPP(1,J))
            INS = 801-ISS
            JSS = VPP(2,I2)
            JNS = ISS-JSS+1
         ENDIF
         VPP(2,I2) = 800
      ENDIF
      NLTRUNC = NLTRUNC+1
      CALL INTERP(IN(ISS,I1),IN(ISS,I2),IN(ISS,J),K,INS)
      IF (GORE) THEN
         CALL SINTERP(IN(JSS,I2),JNS)
         NLGORE = NLGORE+1
      ENDIF

C     ....Now do line-fill:
   20 FILLED = .FALSE.
      GORE = .FALSE.
C
      DO 60 I=1,SCAN_RATE	!Loop through each minor-frame on line I2
      IF (MFP(I,I2).EQ.1.OR.MFP(I,I1).EQ.0) GOTO 60
      K = 1			!Here if minor-frame for line I2 is missing
      J = I2
C     ....Scan down for next valid minor-frame (line=J)
   30 J = MOD(J,MAXBUF) + 1
      IF (J.EQ.I1) GOTO 60	!Give up if no data
      K = K + 1
      IF (MFP(I,J).EQ.0) GOTO 30
C
C     ....Make a one-shot attempt to perform truncation-fill on line j.
      IF (.NOT.TRUNC) GOTO 40	!Skip if no truncation
      IF (VPP(1,J).EQ.1 .AND. VPP(2,J).EQ.800) GOTO 40  !Skip if no trunc
      IF (J.NE.I1-1 .AND. MFP(I,J+1).NE.0) THEN
         IF (VPP(1,J).NE.1) THEN
            ISS = 1
            INS = VPP(1,J)-1
            IF (VPP(2,I1).LT.INS .OR. VPP(2,J).LT.INS) THEN
               GORE = .TRUE.
               INS = MIN(VPP(2,I1),VPP(2,J))
               JSS = INS
               JNS = VPP(1,I2)-JSS+1
            ENDIF
            VPP(1,J) = 1
         ELSE
            ISS = VPP(2,J)+1
            INS = 800-VPP(2,J)
            IF (VPP(1,I1).GT.ISS .OR. VPP(1,J).GT.ISS) THEN
               GORE = .TRUE.
               ISS = MAX(VPP(1,I1),VPP(1,J))
               INS = 801-ISS
               JSS = VPP(2,I2)
               JNS = ISS-JSS+1
            ENDIF
            VPP(2,J) = 800
         ENDIF
         CALL INTERP(IN(ISS,J+1),IN(ISS,J),IN(ISS,I1),K+1,INS)
         IF (GORE) THEN
            CALL SINTERP(IN(JSS,I2),JNS)
            NLGORE = NLGORE+1
         ENDIF
      ELSE		!Can't interpolate: copy data from line I1
         IF (VPP(1,J).NE.1) THEN
            ISS = VPP(1,J)-1
            INS = ISS
            VPP(1,J) = 1
            CALL MVE(1,INS,IN(ISS,I1),IN(ISS,J),1,1)
         ELSE
            ISS = VPP(2,J)+1
            INS = 800-VPP(2,J)
            VPP(2,J) = 800
            CALL MVE(1,INS,IN(ISS,I1),IN(ISS,J))
         ENDIF
      ENDIF
      NLTRUNC = NLTRUNC+1

   40 ISS = SS(I)
      INS = NS(I)
      IF (.NOT.FILLED) THEN		!ONLY 1 COUNT PER LINE
         NLFILL = NLFILL+1
         FILLED = .TRUE.
      ENDIF
      CALL INTERP(IN(ISS,I1),IN(ISS,I2),IN(ISS,J),K,INS)
      MFP(I,I2) = 1
   60 CONTINUE
C
      RETURN
      END
C Fill-in missing line B2 by interpolating between two existing lines
C B1 and B3.  Line B1 is assumed to be adjacent to B2.  K is the number
C of lines between B1 and B3 (e.g: if B1,B2,B3 are consecutive lines,
C then K=2).
C
      SUBROUTINE INTERP(B1,B2,B3,K,NS)
      BYTE	B1(NS),B2(NS),B3(NS)
      INTEGER   I,NS
      INTEGER*4	IVAL

      INCLUDE 'fortport'

      K1 = K - 1
C
      DO I=1,NS
           IVAL = (K1*BYTE2INT(B1(I)) + BYTE2INT(B3(I)))/K
           IF (IVAL.GE.128) THEN
              B2(I) = (255-IVAL)*(-1)
           ELSE
               B2(I) = IVAL
           ENDIF
      ENDDO
C
      RETURN
      END
C Fill-in missing pixels on a line by interpolating between existing
C pixels B(1) and B(N).
C
      SUBROUTINE SINTERP(BUF,N)
      BYTE	BUF(N)
      INTEGER   IVAL

      INCLUDE 'fortport'

      IVAL = 0
      D1 = BYTE2INT(BUF(1))
      D2 = BYTE2INT(BUF(N))
      DB = (D2-D1)/(N-1)
      DN = D1 + 0.5

      DO I=2,N-1
         DN = DN + DB
         IVAL = DN
	 IVAL = MOD (IVAL,256)
         IF (IVAL.GE.128) THEN
            BUF(I)= -(256-IVAL)
         ELSE   
            BUF(I) = IVAL
         ENDIF    
      ENDDO
      RETURN
      END
C Copy pixel data to output.  No fill-in is performed.
C
      SUBROUTINE VGCOPY( IN, OUT, BUF, NL)
      INTEGER*2 BUF(*)

      DO L=1,NL
	CALL XVREAD( IN, BUF, IND,' ')
	CALL XVWRIT( OUT, BUF(113), IND,' ')
      ENDDO
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create vgrfillin.imake
#define  PROGRAM vgrfillin

#define MODULE_LIST vgrfillin.f

#define MAIN_LANG_FORTRAN
#define R2LIB 

#define FTNINC_LIST fortport

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create vgrfillin.pdf
process help=*
PARM INP    TYPE=STRING
PARM OUT    TYPE=STRING
END-PROC
.title
VICAR PROGRAM VGRFILLIN
.help
PURPOSE:

VGRFILLIN will fill in all missing lines, partial lines, or truncated
lines for Voyager images stored as disk EDRs.  Disk EDRs are created by
the Voyager Real-Time Subsystem or via program VEDR.

.PAGE
EXECUTION:

	vgrfillin  IN  OUT
where
  IN is the input Voyager disk EDR.
  OUT is the output image.

The output image will be in standard VICAR format (with all binary header
information discarded).

.page
OPERATION:

  Data gaps are detected by examining data presence indicators in each
line header of the disk EDR.  The fill performed is an interpolation between
good lines.  If the gap is larger than 20 lines wide, or if it occurs at the
beginning or the end of the frame, it will not be filled.

In addition, VGRFILLIN will fill in lines truncated prior to transmission by
the data compressor on-board the spacecraft.  The compressor truncates pixels
on the right side of odd-numbered lines and on the left side of even-numbered
lines.  Truncated pixels are filled via interpolation using the nearest
available data above and below the line.  Sometimes the line truncation is
so severe as to leave a vertical gap down the middle of the image (called a
"gore"), where no data is available in either odd or even lines.  These
gores are filled via interpolation using the nearest available pixels to
the left and right of the gore.

Timing for VAX 780:
	4.7 cpu sec  filling 9 lines
	5.1          filling 40 lines
	5.8          filling 60 lines
	10           filling 800 lines truncated by about 200 samples
.page
PROGRAM HISTORY:

Written by: Gary Yagi     12/1/84
Current cognizant programmer: Gary Yagi
Revisions:
   1 MAR 85  L. Kamp   ...write line fill count to label
                       ...fill in truncated lines for data compression modes
   1 FEB 86  L. Kamp   ...Skip processing after line 480 for IMQ
  22 JAN 89  G. Yagi   ...Implement formats for Neptune encounter
  15 Feb 89  G. Yagi   ...Documented 20-line gap limitation i help file.
   2 Jun 89  G. Yagi   ...Check for valid input DEDR
   6 Mar 95  R. Schenk (CRI) ...Ported for use on UNIX
.LEVEL1
.VARI INP
STRING - input VGR disk EDR
.VARI OUT
STRING - output image 
.LEVEL2
.VARI INP
The input to VGRFILLIN must be a VGR disk EDR which is the output from the
Real-Time Subsystem.  The binary labels must be present.
Therefore, no program can be run on the data between IMBUILD and VGRFILLIN.
.VARI OUT
The output is a normal Vicar2 image (i.e. it no longer has binary labels,
but still has VGR labels.
.end
$ Return
$!#############################################################################
$Test_File:
$ create tstvgrfillin.pdf
procedure
!
! TEST INSTRUCTIONS:
!      To RUN on VAX:	TSTVGRFILLIN
!      
!      To RUN on UNIX:	1.  FTP the following files from the VAX:
!				MIPL:[MIPL.VGR]vgrfil.tst 
!				MIPL:[MIPL.VGR]vgrfil1.tst 
!				MIPL:[MIPL.VGR]vgrfil2.tst
!				MIPL:[MIPL.VGR]vgrfil3.tst
!				MIPL:[MIPL.VGR]vgrfil4.tst
!			2.  tstvgrfillin DIR=""
!			3.  Note:  After the test has run succesfully 
!			    above files may be removed.
refgbl $echo
!refgbl $autousage
PARM DIR TYPE=STRING DEFAULT="MIPL:[MIPL.VGR]"
LOCAL 	a    TYPE=STRING
LOCAL   a1   TYPE=STRING
LOCAL   a2   TYPE=STRING
LOCAL   a3   TYPE=STRING
LOCAL   a4   TYPE=STRING
body                                          
let _onfail="return"
let $echo="yes"
!let $autousage="none"
write "test files contain missing data"
let a = "&DIR"//"vgrfil.tst"
let a1 = "&DIR"//"vgrfil1.tst"
let a2 = "&DIR"//"vgrfil2.tst"
let a3 = "&DIR"//"vgrfil3.tst"
let a4 = "&DIR"//"vgrfil4.tst"
write "list those areas on input and output"
write "test on 1 to 1 scan rate data"
vgrfillin &a b
list &a (1,1,15,10) 'zero
list b (1,1,15,10) 'zero
list &a (540,1,20,10) 'zero
list b (540,1,20,10) 'zero
list &a (750,1,10,10) 'zero
list b (750,1,10,10) 'zero
list &a (798,1,3,10) 'zero
list b (798,1,3,10) 'zero
write "test on IM-K mode, 5:1 scan rate data"
vgrfillin &a1 b
list &a1 (770,1,10,10) 'zero
list b (770,1,10,10) 'zero
list &a1 (797,1,4,10) 'zero
list b (797,1,4,10) 'zero
write "test on 3 to 1 scan rate data"
vgrfillin &a2 b
list &a2 (265,1,25,10) 'zero
list b (265,1,25,10) 'zero
write "test on 5 to 1 scan rate editted data"
vgrfillin &a3 b
list &a3 (145,400,50,10) 'zero
list b (145,400,50,10) 'zero
write "test on IM-K mode, 10:1 scan rate data, with gore"
vgrfillin &a4 b
list &a4 (100,1,1,800) 'zero
list b (100,1,1,800) 'zero
end-proc
$ Return
$!#############################################################################
