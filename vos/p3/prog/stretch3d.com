$!****************************************************************************
$!
$! Build proc for MIPL module stretch3d
$! VPACK Version 1.8, Friday, April 06, 2001, 14:32:21
$!
$! Execute by entering:		$ @stretch3d
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
$ write sys$output "*** module stretch3d ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
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
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Imake .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to stretch3d.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
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
$   if F$SEARCH("stretch3d.imake") .nes. ""
$   then
$      vimake stretch3d
$      purge stretch3d.bld
$   else
$      if F$SEARCH("stretch3d.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake stretch3d
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @stretch3d.bld "STD"
$   else
$      @stretch3d.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create stretch3d.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack stretch3d.com -
	-s stretch3d.f -
	-i stretch3d.imake -
	-p stretch3d.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create stretch3d.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
      IMPLICIT NONE
C
      INTEGER*2    HALFBUF(265376)

      INTEGER*4    I, ILOC, INUNIT, J, NB,
     1             NL, NS, STATUS, OUTUNIT(30), ISAMPS, LCNTR, 
     2             IBVAL(224),ICNT,IDEF, SL, SS, NLIN, NSIN,
     3             IPIX(265376),NSDEV,JSTART,NCHAN,IEL,KSTART,
     4             NBANDS 
      REAL*8       ASAMPS(224), 
     1             BRSUM(224), BRSUM2(224), 
     2             BRMEAN(224), BRVAR(224), BRSDEV(224) 
      REAL         BUFIN(265376), BMAX(224),BMIN(224),
     1             BRANGE(224),SMAX,SMIN,SRANGE,BADL,BADH,USERMAX(40),
     2             USERMIN(40), USERANGE(40)
      BYTE         BYTEBUF(265376)
      BYTE         RUBOUT(4)/4*8/
      CHARACTER*2  NUM(30)
      CHARACTER*3  INORG
      CHARACTER*4  FORMAT
      CHARACTER*28 ROOT
      CHARACTER*36 NAME
      LOGICAL      XVPTST,DTYPE,SCALE,OUTPUT,USER
C
C*****INITIALIZE VARIABLES
      DATA IBVAL/224*0/
      DATA BMAX,BMIN,BRANGE,SMAX,SMIN,SRANGE/224*-1.0E6,224*1.0E6,
     1     224*0.0,-1.0E6,1.0E6,0.0/
      DATA USERMAX,USERMIN,USERANGE/40*-99.0,40*99.0,40*0.0/         
      DATA ASAMPS,BRSUM,BRSUM2,BRMEAN,BRVAR,
     1     BRSDEV/224*0.0D0,224*0.0D0,224*0.0D0,
     2     224*0.0D0,224*0.0D0,224*0.0D0/
      DATA NUM/'1','2','3','4','5','6','7','8','9','10',
     1         '11','12','13','14','15','16','17','18','19','20',
     2         '21','22','23','24','25','26','27','28','29','30'/  
C
C  OPEN INPUT FILE 
C
      CALL XVUNIT(INUNIT,'INP',1,STATUS,' ')
      CALL XVOPEN(INUNIT,STATUS,'OPEN_ACT','SA','IO_ACT','SA',
     &            'U_FORMAT','REAL',' ')
C
      CALL XVGET(INUNIT,STATUS,'NL',NL,'NS',NS,'NB',NB,'ORG',
     &           INORG,'FORMAT',FORMAT,' ')
C
      CALL XVSIZE(SL,SS,NL,NS,NLIN,NSIN)
      CALL XVPARM('JSTART',JSTART,ICNT,IDEF,0)
      CALL XVPARM('NCHAN',NCHAN,ICNT,IDEF,0)
      CALL XVPARM('NSDEV',NSDEV,ICNT,IDEF,0)
      CALL XVPARM('USERMAX',USERMAX,ICNT,IDEF,0)
      CALL XVPARM('USERMIN',USERMIN,ICNT,IDEF,0)
      CALL XVPARM('BADL',BADL,ICNT,IDEF,0)
      CALL XVPARM('BADH',BADH,ICNT,IDEF,0)
C
C     KEYWORDS AND DEFAULTS    
C
      USER=.FALSE.
      DTYPE=.FALSE.
      SCALE=.FALSE.
      IF(NCHAN.EQ.0)NCHAN=NB
      OUTPUT=XVPTST('SINGLE') 
      DTYPE=XVPTST('HALF')
      SCALE=XVPTST('ALL')
      USER=XVPTST('YES')
      
C     OPEN OUTPUT FILE
C
      IF(OUTPUT)THEN
        IF(DTYPE)THEN
          CALL XVUNIT(OUTUNIT(1),'OUT',1,STATUS,' ')
          CALL XVOPEN(OUTUNIT(1),STATUS,'OPEN_ACT','SA','IO_ACT','SA',
     &                'O_FORMAT','HALF','U_NB',NCHAN,'U_NL',NL,'U_NS',
     &                NS,'U_FORMAT','HALF','OP','WRITE','U_ORG','BIL',
     &                ' ')
        ELSE
          CALL XVUNIT(OUTUNIT(1),'OUT',1,STATUS,' ')
          CALL XVOPEN(OUTUNIT(1),STATUS,'OPEN_ACT','SA','IO_ACT','SA',
     &                'O_FORMAT','BYTE','U_NB',NCHAN,'U_NL',NL,'U_NS',
     &                NS,'U_FORMAT','BYTE','OP','WRITE','U_ORG','BIL',
     &                ' ')
        ENDIF
      ELSE
        CALL XVPARM('OUT',ROOT,ICNT,IDEF,0)
        DO I=1,NCHAN 
           NAME= ROOT(1:INDEX(ROOT,' ')-1) // '.' // NUM(I)
          IF(DTYPE)THEN
            CALL XVUNIT(OUTUNIT(I),'XXX',I,STATUS,'U_NAME',NAME,' ')
            CALL XVOPEN(OUTUNIT(I),STATUS,'OPEN_ACT','SA','IO_ACT','SA',
     &                  'O_FORMAT','HALF','U_NB',1,'U_NL',NL,'U_NS',NS,
     &                  'U_FORMAT','HALF','OP','WRITE','U_ORG','BSQ',
     &                  ' ')
          ELSE
            CALL XVUNIT(OUTUNIT(I),'XXX',I,STATUS,'U_NAME',NAME,' ')
            CALL XVOPEN(OUTUNIT(I),STATUS,'OPEN_ACT','SA','IO_ACT','SA',
     &                  'O_FORMAT','BYTE','U_NB',1,'U_NL',NL,'U_NS',NS,
     &                  'U_FORMAT','BYTE','OP','WRITE','U_ORG','BSQ',
     &                  ' ')
          ENDIF
        ENDDO
      ENDIF 
C
C*****INITIALIZE THE COUNTERS      
      ISAMPS=NS*NCHAN
      LCNTR=0
C
C*****SET UP OUTPUT PARAMETERS
C
      WRITE(6,'('' FIRST PASS OF TWO '',/,'' Processing Line     '',$)')
      IEL = SL+NL-1
      DO I= SL,IEL
        KSTART=1    
        DO J = JSTART,JSTART+(NCHAN-1)
          ILOC=(NS * (KSTART-1)) + 1
          CALL XVREAD(INUNIT,BUFIN(ILOC),STATUS,'LINE',I,
     &                'SAMP',SS,'NSAMPS',NS,'BAND',J,' ')
          KSTART=KSTART+1
        ENDDO
c
        CALL MAXMIN(BUFIN, NS, NCHAN, ISAMPS, BMAX, BMIN, BADL, BADH)
        CALL RSTAT1(BUFIN, NS, NCHAN, ISAMPS, BRSUM, BRSUM2, 
     1           IBVAL, BADL, BADH)
        LCNTR=LCNTR+1
        WRITE(6,101) RUBOUT,LCNTR
  101   FORMAT(4A1,I4,$)
      ENDDO
      WRITE (6,*)
      CALL RSTAT2(BRSUM,BRSUM2,NS,NCHAN,ASAMPS,LCNTR,IBVAL,
     1            BRMEAN,BRVAR,BRSDEV)
      CALL SSTAT(BMAX, BMIN, BRANGE, NCHAN, SMAX, SMIN, SRANGE)
C
      PRINT*, 'CHANNEL MEANS -  STANDARD DEVIATIONS - BAD VALUES'
      DO I= 1,NCHAN 
      WRITE(6,'(F20.10,2X,F20.10,2X,I6)')BRMEAN(I),BRSDEV(I),
     1                                   IBVAL(I)
      ENDDO
      PRINT*, 'CHANNEL  MAX  - MIN - RANGES'
      DO I = 1,NCHAN 
      WRITE(6,'(F20.10,2X,F20.10,2X,F20.10)')BMAX(I),BMIN(I),BRANGE(I)
      ENDDO
      PRINT*, 'SCENE MAXIMUM =',SMAX
      PRINT*, 'SCENE MINIMUM =',SMIN
      PRINT*, 'SCENE RANGE   =',SRANGE
C
C*****REDO THE STATS IF A USER SPECIFIED SET OF MAX AND MINS
      IF(USER)THEN
        IF(NSDEV.NE.0.0)THEN
          PRINT*, 'SORRY YOU CANNOT SPECIFY YOUR OWN MAX MINS'
          PRINT*, 'AND RESCALE USING DATA MEAN +/- NSDEV'
          PRINT*, 'DEFAULTING TO SCALING USING MEAN +/- NSDEV'
          USER=.FALSE.
        ELSE
          PRINT*, 'RESCALING USING USER DEFINED MAX MINS'
          CALL USERSTAT(USERMAX, USERMIN, NCHAN, BMAX, BMIN, BRANGE)
          IF(SCALE)THEN
            NBANDS=2
          ELSE
            NBANDS=NCHAN
          ENDIF
          CALL SSTAT(BMAX, BMIN, BRANGE, NBANDS, SMAX, SMIN, SRANGE)
        ENDIF
      ENDIF
C
C*****SECOND PASS WRITE OUT THE DATA
C
      CALL RESCAL(SCALE, NCHAN, BRMEAN, NSDEV, BRSDEV, 
     1            BMAX, BMIN, BRANGE, SMAX, SMIN, SRANGE)
      PRINT*, 'MAX - MIN - RANGE USED FOR RESCALING '
      DO I=1,NCHAN
        PRINT*, BMAX(I),BMIN(I),BRANGE(I)
      ENDDO  
      LCNTR=0
      WRITE(6,'('' SECOND PASS OF TWO'',/,'' Processing Line     '',$)')
      DO I= SL,IEL 
        KSTART=1
        DO J = JSTART,JSTART+(NCHAN-1)
          ILOC=(NS * (KSTART-1)) + 1
          CALL XVREAD(INUNIT,BUFIN(ILOC),STATUS,'LINE',I,
     &                'SAMP',SS,'NSAMPS',NS,'BAND',J,' ')
        KSTART=KSTART+1 
        ENDDO
c
C       WANT HALFWORD DATA 
        IF(DTYPE)THEN
C         WANT TO SCALE USING OVERALL MAXMIN
          IF(SCALE)THEN   
            CALL OUTHA1(BUFIN,ISAMPS,SMAX,SMIN,SRANGE,HALFBUF
     +                  ,BADL,BADH)
          ELSE
C                SCALE EACH CHANNEL INDIVIDUALLY        
            CALL OUTHA2(BUFIN,ISAMPS,NS,NCHAN,BMAX,BMIN,BRANGE,HALFBUF,
     +                  BADL,BADH) 
          ENDIF
        ELSE
C         WANT TO SCALE TO BYTE
          IF(SCALE)THEN
C           SCALE USING OVERALL MAXMIN
            CALL OUTBY1(BUFIN,ISAMPS,SMAX,SMIN,SRANGE,IPIX,BYTEBUF,
     +                  BADL,BADH)
          ELSE
C           SCALE EACH CHANNEL INDIVIDUALLY
            CALL OUTBY2(BUFIN,ISAMPS,NS,NCHAN,BMAX,BMIN,
     1                  BRANGE,IPIX,BYTEBUF,BADL,BADH)
          ENDIF
        ENDIF
C
        IF(OUTPUT)THEN 
          DO J = 1,NCHAN 
              ILOC=(NS * (J-1)) + 1
            IF(DTYPE)THEN
              CALL XVWRIT(OUTUNIT(1),HALFBUF(ILOC),STATUS,' ')
            ELSE 
              CALL XVWRIT(OUTUNIT(1),BYTEBUF(ILOC),STATUS,' ')
            ENDIF 
          ENDDO
        ELSE
          DO J = 1,NCHAN 
            ILOC=(NS * (J-1)) + 1
            IF(DTYPE)THEN
              CALL XVWRIT(OUTUNIT(J),HALFBUF(ILOC),STATUS,' ')
            ELSE 
              CALL XVWRIT(OUTUNIT(J),BYTEBUF(ILOC),STATUS,' ')
            ENDIF 
          ENDDO
        ENDIF  
        LCNTR=LCNTR+1
        WRITE(6,101) RUBOUT,LCNTR
      ENDDO 
      WRITE (6,*)
C
C     CLOSE FILES
C
      CALL XVCLOSE(INUNIT,STATUS,' ')
      IF(OUTPUT)THEN
        CALL XVCLOSE(OUTUNIT(1),STATUS,' ')
      ELSE
        DO I=1,NCHAN
          CALL XVCLOSE(OUTUNIT(I),STATUS,' ')
        ENDDO
      ENDIF  
      RETURN
      END
C
C*****SUBROUTINE TO INPUT USER MAX MINS
C
      SUBROUTINE USERSTAT(USERMAX, USERMIN, NCHAN, BMAX, BMIN, BRANGE) 
     1                    
      REAL USERMAX(NCHAN), USERMIN(NCHAN), BMAX(NCHAN), BMIN(NCHAN), 
     1     BRANGE(NCHAN)
C
      IF(NCHAN.GT.10)THEN
        PRINT*, 'WARNING MORE CHANNELS STRETCHED THAN USER '
        PRINT*, 'SPECIFIED MAX MINS'
      ENDIF
      DO I=1,NCHAN
        BMAX(I)=USERMAX(I)
        BMIN(I)=USERMIN(I)
        BRANGE(I)=BMAX(I)-BMIN(I)
      ENDDO
      RETURN
      END
C
C*****SUBROUTINE TO FIND MAX AND MIN OF CALIBRATED VALUES 
C 
      SUBROUTINE MAXMIN(BUFIN, NS, NB, ISAMPS, BMAX, BMIN, 
     +                  BADL, BADH) 
      REAL BUFIN(ISAMPS), BMAX(NB), BMIN(NB), BADL, BADH 
C 
      K = 1
      DO I = 1, NB
        DO J = 1, NS
          IF (BUFIN(K).NE.BADL.AND.BUFIN(K).NE.BADH
     1        .AND.BUFIN(K).GT.BMAX(I))BMAX(I) = BUFIN(K)
          IF (BUFIN(K).NE.BADL.AND.BUFIN(K).NE.BADH
     1        .AND.BUFIN(K).LT.BMIN(I))BMIN(I) = BUFIN(K)
          K = K + 1
        ENDDO 
      ENDDO 
C
      RETURN 
      END 
C 
C*****SUBROUTINE TO FIND THE SUM AND SUM2 OF EACH BAND OF REAL 
C*****DATA 
C 
C*****CHECK IBVAL IS RESET TO ZERO IF CALCULATING REAL DATA AND 
C*****BYTE DATA STATS 
C 
      SUBROUTINE RSTAT1(BUFIN, NS, NB, ISAMPS, BRSUM, BRSUM2, 
     1           IBVAL, BADL, BADH)
      REAL BUFIN(ISAMPS), BADL, BADH
      REAL*8 BRSUM(NB), BRSUM2(NB)
      INTEGER IBVAL(NB)
      JSTART = 1
      JEND = NS
      DO I = 1, NB
        DO J = JSTART, JEND
          IF (BUFIN(J).NE.BADL.AND.BUFIN(J).NE.BADH) THEN 
            BRSUM(I) = (DBLE(BUFIN(J))) + BRSUM(I)
            BRSUM2(I) = ((DBLE(BUFIN(J)))*(DBLE(BUFIN(J)))) 
     1      + BRSUM2(I)
          END IF 
          IF(BUFIN(J).EQ.BADL.OR.BUFIN(J).EQ.BADH)IBVAL(I)=IBVAL(I)+1
        ENDDO 
        JSTART = JSTART + NS
        JEND = JEND + NS
      ENDDO 
      RETURN 
      END 
C 
C*****SUBROUTINE TO CALCULATE THE MEAN, VARIANCE AND STD DEV OF THE REAL
C*****DATA 
      SUBROUTINE RSTAT2(BRSUM, BRSUM2, NS, NB, ASAMPS, LCNTR, 
     1           IBVAL, BRMEAN, BRVAR, BRSDEV)
      REAL*8 BRSUM(NB), BRSUM2(NB), BRMEAN(NB), 
     1       BRVAR(NB), BRSDEV(NB), ASAMPS(NB) 
      INTEGER IBVAL(NB)
C 
      DO I = 1, NB
        ASAMPS(I) = 0.0D0
        ASAMPS(I) = (DFLOAT((NS*LCNTR) - IBVAL(I)))
      ENDDO 
      DO I = 1, NB
        IF(ASAMPS(I).NE.0.0)THEN
          BRMEAN(I) = BRSUM(I) / ASAMPS(I)
          BRVAR(I) = (ASAMPS(I)*BRSUM2(I) - BRSUM(I)*BRSUM(I)) /
     1    (ASAMPS(I)*(ASAMPS(I) - 1))
          BRSDEV(I) = DSQRT(BRVAR(I))
        ENDIF
      ENDDO 
C 
      RETURN 
      END 
C
C*****SUBROUTINE TO FIND THE RANGE OF EACH BAND AND THE OVERALL SCENE 
C*****SMAX,SMIN,SRANGE 
C 
      SUBROUTINE SSTAT(BMAX, BMIN, BRANGE, NB, SMAX, SMIN, SRANGE)
      REAL BMAX(NB), BMIN(NB), BRANGE(NB), SMAX, SMIN, 
     1     SRANGE
C 
      DO I = 1, NB
        BRANGE(I) = 0.0
        BRANGE(I) = BMAX(I) - BMIN(I)
      ENDDO 
C 
      SMAX = -1.0E6
      SMIN = 1.0E6
C 
      DO I = 1, NB
        IF (BMAX(I) .GT. SMAX) SMAX = BMAX(I)
        IF (BMIN(I) .LT. SMIN) SMIN = BMIN(I)
      ENDDO 
C 
      SRANGE = SMAX - SMIN
C
      RETURN 
      END  
C
C*****SUBROUTINE TO SET MIN AND RANGE FOR RESCALING 
C*****ACCORDING TO ACTUAL MAX MIN AND RANGE OR THE 
C*****MEAN +/- N STD DEVIATIONS 
      SUBROUTINE RESCAL(SCALE, NB, BRMEAN, NSDEV, 
     1           BRSDEV, BMAX, BMIN, BRANGE, SMAX, SMIN, SRANGE)
C 
      REAL BMAX(NB), BMIN(NB), BRANGE(NB), NEWMAX, NEWMIN
      REAL*8 BRMEAN(NB), BRSDEV(NB)
      LOGICAL SCALE 
C 
C     LOGICAL - SCALE - ALL - TRUE
C
      IF (NSDEV.GT.0) THEN 
        IF (SCALE) THEN
          DO I = 1, NB
            BMAX(I) = SNGL(BRMEAN(I) + (DBLE(NSDEV)*BRSDEV(I)))
            BMIN(I) = SNGL(BRMEAN(I) - (DBLE(NSDEV)*BRSDEV(I)))
            BRANGE(I) = BMAX(I) - BMIN(I)
            IF(BMAX(I).GT.SMAX)SMAX=BMAX(I)
            IF(BMIN(I).LT.SMIN)SMIN=BMIN(I)
          ENDDO
          SRANGE=SMAX-SMIN
        ELSE
          DO I=1, NB
            NEWMAX=0
            NEWMIN=0
            NEWMAX = SNGL(BRMEAN(I) + (DBLE(NSDEV)*BRSDEV(I)))
            NEWMIN = SNGL(BRMEAN(I) - (DBLE(NSDEV)*BRSDEV(I)))
            IF(NEWMAX.LT.BMAX(I))BMAX(I)=NEWMAX
            IF(NEWMIN.GT.BMIN(I))BMIN(I)=NEWMIN
            BRANGE(I) = BMAX(I) - BMIN(I)
          ENDDO
        ENDIF
      ENDIF 
C 
      RETURN 
      END 
C
C*****SUBROUTINE TO SCALE THE DATA TO HALF RANGE USING  
C*****OVERALL MAX MIN 
C 
      SUBROUTINE OUTHA1(BUFIN,ISAMPS,SMAX,SMIN,SRANGE,HALFBUF,
     +                  BADL, BADH)
      REAL BUFIN(ISAMPS)
      INTEGER*2 HALFBUF(ISAMPS)
C 
      DO I = 1, ISAMPS
        HALFBUF(I) = 0
      ENDDO 
C 
C*****SCALE DATA 
C 
      DO I = 1, ISAMPS
        IF(BUFIN(I).EQ.BADL)THEN
          HALFBUF(I) = -30000
        ELSEIF(BUFIN(I).EQ.BADH)THEN 
          HALFBUF(I) = 30000
        ELSEIF(BUFIN(I).GE.SMAX)THEN
          BUFIN(I) = -29999
        ELSEIF(BUFIN(I).LE.SMIN)THEN
          BUFIN(I) = 29999 
        ELSE 
          DATA = 0.0
          DATA = (BUFIN(I) - SMIN) / SRANGE
          HALFBUF(I) = NINT((DATA*59996) - 29999.0)
        ENDIF 
      ENDDO 
C 
      RETURN 
      END 
C 
C*****SUBROUTINE TO SCALE EACH CHANNEL TO HALFWORD USING 
C*****THE MAX MIN RANGE OF EACH CHANNEL 
      SUBROUTINE OUTHA2(BUFIN, ISAMPS, NS, NB, BMAX, BMIN, 
     1           BRANGE, HALFBUF, BADL, BADH)
      REAL BUFIN(ISAMPS), BMAX(NB), BMIN(NB), BRANGE(NB),
     +     BADL, BADH
      INTEGER*2 HALFBUF(ISAMPS)
C 
C*****INITIALIZE IPIX 
      DO I = 1, ISAMPS
        HALFBUF(I) = 0
      ENDDO 
C 
C*****SET UP COUNTERS 
      K = 1
C 
      DO I = 1, NB
        DO J = 1, NS
C 
          IF(BUFIN(K).EQ.BADL)THEN 
            HALFBUF(K) = -30000 
          ELSEIF(BUFIN(K).EQ.BADH)THEN
            HALFBUF(K) = 30000 
          ELSEIF(BUFIN(K).GE.BMAX(I))THEN
            HALFBUF(K) = -29999
          ELSEIF(BUFIN(K).LE.BMIN(I))THEN
            HALFBUF(K) = 30000   
          ELSE 
            DATA = 0.0
            DATA = (BUFIN(K) - BMIN(I)) / BRANGE(I)
            HALFBUF(K) = NINT((DATA*60000) - 30000)
          ENDIF 
          K = K + 1
        ENDDO 
      ENDDO 
C 
      RETURN 
      END
C 
C*****SUBROUTINE TO SCALE EACH CHANNEL TO BYTE FORM USING 
C*****THE OVERALL MAX MIN AND RANGE 
      SUBROUTINE OUTBY1(BUFIN,ISAMPS,SMAX,SMIN,SRANGE,IPIX,BYTEBUF,
     +                  BADL, BADH)
      REAL BUFIN(ISAMPS), BADL, BADH 
      INTEGER IPIX(ISAMPS)
      BYTE BYTEBUF(ISAMPS)
C 
C*****INITIALIZE IPIX 
      DO I = 1, ISAMPS
        IPIX(I) = 0
      ENDDO 
C 
C*****SCALE DATA 
C 
      DO I = 1, ISAMPS 
        IF(BUFIN(I).EQ.BADL)THEN
          IPIX(I) = 0
        ELSEIF(BUFIN(I).EQ.BADH)THEN
          IPIX(I) = 255
        ELSEIF(BUFIN(I).GE.SMAX)THEN 
          IPIX(I)=254 
        ELSEIF(BUFIN(I).LE.SMIN)THEN 
          IPIX(I)=1 
        ELSE 
          DATA = 0.0
          DATA = (BUFIN(I) - SMIN) / SRANGE
          IPIX(I) = (NINT(DATA*252.0))+1
        ENDIF
        CALL ITL(IPIX(I),BYTEBUF(I))
      ENDDO 
C 
      RETURN 
      END
C 
C*****SUBROUTINE TO SCALE EACH CHANNEL TO BYTE FORM USING 
C*****THE MAX MIN AND RANGE OF EACH CHANNEL 
      SUBROUTINE OUTBY2(BUFIN, ISAMPS, NS, NB, BMAX, BMIN, 
     1           BRANGE, IPIX, BYTEBUF, BADL, BADH)
      REAL BUFIN(ISAMPS), BMAX(NB), BMIN(NB), BRANGE(NB),
     +           BADL, BADH   
      INTEGER IPIX(ISAMPS)
      BYTE BYTEBUF(ISAMPS)
C 
C*****INITIALIZE IPIX 
      DO I = 1, ISAMPS
        IPIX(I) = 0
      ENDDO 
C 
C*****SET UP COUNTERS 
      K = 1
C 
      DO I = 1, NB
        DO J = 1, NS
C 
          IF(BUFIN(K).EQ.BADL)THEN
            IPIX(K) = 0
          ELSEIF(BUFIN(K).EQ.BADH)THEN
            IPIX(K) = 255
          ELSEIF(BUFIN(K).GT.BMAX(I))THEN 
            IPIX(K) = 254 
          ELSEIF(BUFIN(K).LT.BMIN(I))THEN 
            IPIX(K) = 1 
          ELSE 
            DATA = 0.0
            DATA = (BUFIN(K) - BMIN(I)) / BRANGE(I)
            IPIX(K) = (NINT(DATA*252.0))+1
          ENDIF
          CALL ITL(IPIX(K),BYTEBUF(K))
          K = K + 1
        ENDDO 
      ENDDO 
C 
      RETURN 
      END 
C 
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create stretch3d.imake
#define  PROGRAM   stretch3d

#define MODULE_LIST stretch3d.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
$PDF_File:
$ create stretch3d.pdf
process help=*
PARM INP         TYPE=(STRING,40)     
PARM OUT         TYPE=(STRING,40)
PARM OUTFILES    TYPE=KEYWORD DEFAULT=MULTI VALID=(SINGLE,MULTI)  
PARM SIZE        TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SL          TYPE=INTEGER DEFAULT=1
PARM SS          TYPE=INTEGER DEFAULT=1
PARM NL          TYPE=INTEGER DEFAULT=0
PARM NS          TYPE=INTEGER DEFAULT=0
PARM JSTART      TYPE=INTEGER DEFAULT=1
PARM NCHAN       TYPE=INTEGER DEFAULT=0
PARM NSDEV       TYPE=INTEGER DEFAULT=0
PARM USER        TYPE=KEYWORD DEFAULT=NO VALID=(YES,NO)
PARM USERMAX     TYPE=REAL COUNT=10 DEFAULT=(-99.0,-99.0,-99.0,-99.0, +
-99.0,-99.0,-99.0,-99.0,-99.0,-99.0)
PARM USERMIN     TYPE=REAL COUNT=10 DEFAULT=(99.0,99.0,99.0,99.0,99.0, +
99.0,99.0,99.0,99.0,99.0)
PARM BADL        TYPE=REAL    DEFAULT=0.0
PARM BADH        TYPE=REAL    DEFAULT=0.0  
PARM DATATYPE    TYPE=KEYWORD DEFAULT=BYTE VALID=(BYTE,HALF)
PARM SCALING     TYPE=KEYWORD DEFAULT=BYCHAN VALID=(ALL,BYCHAN)
END-PROC
.HELP
This program allows the user to rescale their data to either 
halfword or byte using either the max and min or the mean +/-
N standard deviations. The output can either be to separate files 
or to a single BIL file.
Cognizant programmer S. J. Hook Extn 4-0974 
Geology Group Section 326.
.LEVEL1
.VARIABLE INP
Input dataset
.VARIABLE OUTFILES
If keyword SINGLE is selected 
all channels will be written 
out in a single BIL file. If 
keyword MULTI is selected each 
channel will be output in 
separate files. The names for 
these files are assigned in 
variable OUT
.VARIABLE OUT
Output dataset. If KEYWORD 
MULTI was selected DO NOT 
provide an extension to the 
filename in OUT. The filename 
will be automatically appended 
with the channel number of 
the output
.VARIABLE SIZE
The standard vicar size field 
(sl,ss,nl,ns)
.VARIABLE SL
Starting line
.VARIABLE SS
Starting sample
.VARIABLE NL
Number of lines
.VARIABLE NS 
Number of samples
.VARIABLE JSTART
Start Channnel
.VARIABLE NCHAN
Number of channels 
.VARIABLE NSDEV
If NSDEV is greater than 0 
data will be rescaled using 
the mean +/- NSDEV std dev., 
where NSDEV is the number of 
std dev. 
.VARIABLE USER
If you select YES for this 
variable your data will be 
rescaled using the usermax 
and usermin values.
.VARIABLE USERMAX
If usermax is not equal to 
-99.0 and "USER = YES",
the usermax value will 
be used instead of the 
calculated max value.
.VARIABLE USERMIN
if usermin is not equal to 
99.0 and "USER = YES", 
the usermin value will 
be used instead of the 
calculated min value 
.VARIABLE BADL
Any pixels with this value 
will be set to zero if 
output is byte or -30000 if
the output is half.
.VARIABLE BADH
Any pixels with this value 
will be set to 255 if 
output is byte or 30000 if
the output is half.

The defaults for BADL and 
BADH are 0.0 and any zero 
values in the input will 
be set to zero in the 
output.

.VARIABLE DATATYPE 
If keyword BYTE selected 
data rescaled to byte. If 
keyword HALF selected data 
rescaled to halfword
.VARIABLE SCALING
If keyword ALL selected data 
are rescaled using max/min 
or mean+/- std dev. of entire 
data set. If BYCHAN is selected 
data rescaled using different 
scaling parameters for each 
channel.

NB. values of 0 and 255 are 
excluded when determining 
the statistics used for 
rescaling. The combined number 
of 0 and 255's is reported as 
the number of bad values in 
the screen output after the 
first pass.
$ Return
$!#############################################################################
