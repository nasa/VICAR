$!****************************************************************************
$!
$! Build proc for MIPL module astrtchr
$! VPACK Version 1.8, Wednesday, September 20, 1995, 14:46:36
$!
$! Execute by entering:		$ @astrtchr
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
$ write sys$output "*** module astrtchr ***"
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
$ write sys$output "Invalid argument given to astrtchr.com file -- ", primary
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
$   if F$SEARCH("astrtchr.imake") .nes. ""
$   then
$      vimake astrtchr
$      purge astrtchr.bld
$   else
$      if F$SEARCH("astrtchr.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake astrtchr
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @astrtchr.bld "STD"
$   else
$      @astrtchr.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create astrtchr.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack astrtchr.com -
	-s astrtchr.f -
	-i astrtchr.imake -
	-p astrtchr.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create astrtchr.f
$ DECK/DOLLARS="$ VOKAGLEVE"
       PROGRAM  ASTRTCHR
C#######################################################################
C  NAME OF ROUTINE
C      astrtchr ( Automatic STReTCH Real )
C
C  PURPOSE
C      THIS IS THE STANDARD MAIN PROGRAM USED FOR TAE/VICAR PROGRAMS.
C      THIS MODULE CALLS SUBROUTINE MAIN44 TO ENTER INTO THE BODY OF THE
C      PROGRAM.
C      astrtchr is a VICAR applications program which performs automatic
C      linear stretches on floating point and fullword integer pictures.
C  PREPARED FOR USE ON MIPL SYSTEM BY
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION    JANUARY 1984
C  FOR
C      EARTH RESOURCES APPLICATIONS
C
C  ORIGINAL ASTRTCHR PROGRAM BY
C      J. J. LORRE
C
C  ENVIRONMENT
C      VAX 11/780    VMS  with TAE/VICAR2 EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C     1-84  SP   CONVERTED FROM IBM VICAR VERSION: CHANGED FROM PLI TO 
C                FORTRAN-77 AND MISCELLANEOUS CLEANUP.
C     1-84  SP   ADDED LABEL PROCESSING OF INPUT FILE BY USING LABELC
C                TO FIND THE DATA FORMAT AND USING FORMAT IN LABEL AS DEFAULT.
C     1-84  SP   ADDED INFMT AND OUTFMT PARAMETERS TO ALLOW USER TO OVERRIDE
C                IMAGE FORMAT IN VICAR LABEL.
C     1-84  SP   USED STACKA FOR DYNAMIC MEMORY ALLOCATION.
C     1-84  SP   MODIFIED SO THAT RDCHEK & WRCHEK USED FOR I/O ERROR MESSAGES.
C     1-84  SP   CHANGED LABEL PROCESSING OF OUTPUT FILE TO NOT USE JJLLAB.
C     1-84  SP   CHANGED OUTMIN FROM -32766 TO -32768 & OUTMAX FROM 32766 TO
C                32767.
C     1-84  SP   REMOVED CHECKING OF AREA & PROCESS PARAMETERS AGAINST IMAGE
C                SIZE IN VICAR LABEL TO ALLOW USER TO OVERRIDE SIZE IN LABEL.
C     1-84  SP   SIMPLIFIED COMPUTATION OF NL AND NS FOR OUTPUT IMAGE LABEL.
C     1-84  SP   LENGTHENED SOME KEYWORDS: LIMI -> LIMITS,...
C     1-84  SP   CORRECTED ROUNDING OF NEGATIVE OUTPUT DNS TO ROUND TO
C                NEAREST INTEGER. (USED NINT FUNCTION.)
C     1-84  SP   CORRECTED FORMULA FOR FINDING VALLHI FROM
C                I=(1.0-PERHI)*N+1.5  TO  I=(1.0-PERHI)*N+.5
C     5-84  SP   INCREASED BUFFER SIZE FOR LABELC SINCE NLRECOUT NOW IS 0.
C    10-84  SP   CHANGED TO PERFORM RANGE CHECKING ON OUTPUT DNS BEFORE
C                CONVERTING TO INTEGER TO PREVENT INTEGER OVERFLOWS.
C     2-85  JES  CORRECTED CODE THAT CHECKS OUTPUT LIMITS SO THAT ACTUAL
C                "LIMITS" ARE USED, NOT THE LIMIT RANGE OF DATA FORMAT.
C                CORRECTIONS WERE IN THE "LINE-BY-LINE STRETCH LOOP"
C     4-85  JRH  CONVERTED TO VICAR2.
C      		 REPLACED THE PROCESS PARAMETER WITH THE SIZE PARAMETER.
C    		 CHANGED INFMT TO IFORMAT AND OUTFMT TO OFORMAT.
C                FOR IFORMAT, CHANGED THE KEYWORD FROM INTEGER TO FULL.
C                CORRECTED FORMULA FOR FINDING VALLHI ON FULLWORD DATA
C                FROM I=(1.0-PERHI)*N+1.5  TO  I=(1.0-PERHI)*N+.5 
C     4-87  SP   CORRECTED INTEGER OVERFLOW PROBLEM AGAIN.  (THE ABOVE CHANGE
C                FROM 2-85 REMOVED THE CHANGE FROM 10-84.)  ADDED NOCLIP
C                PARAMETER TO ALLOW OUTPUT DN RANGE TO BE HANDLED AS BEFORE
C                2-85.  OUTPUT DN RANGE DEFAULTS TO THE LIMITS PARAMETER AS IT
C                HAS SINCE 2-85.
C     4-87  SP   CORRECTED PROBLEM OF STRETCH PARAMETERS NOT GOING INTO LABEL.
C                (CORRECTED LENGTH OF STRING IN MVL CALL.)
C     5-92  LWK  ADDED 'STREXCL' AND 'REPLACE' PARAMETERS FOR USE IN NIMS
C                PROCESSING
C     1-94  JFM  ADDED TO VICAR HISTORY LABEL THE PDS STANDARD KEYWORDS
C		 STRETCH_MINIMUM AND STRETCH_MAXIMUM AS AN ALTERNATE WAY OF
C		 EXPRESSING INPUT AND OUTPUT STRETCH LIMITS.
C	         (FR 82901 FOR GLL NIMS)	    
C     4-94  CRI  MSTP (S/W CONVERSION) VICAR PORTING
C     9-95  rea  Removed MATH77 library references
C
C    CALLING SEQUENCE (TAE COMMAND LINE)
C      The following command line formats show the major allowable forms:
C
C      astrtchr INP=a OUT=b optional parameters
C      astrtchr a b optional parameters
C
C       Here 'a' represents the input image file name,
C       'b' represents the output image file name.
C
C  INPUT PARAMETERS (listed by keyword)
C      INP    - Input file names.
C      OUT    - Output file name.
C      SIZE   - Standard Vicar size field:  
C               SIZE field in terms of fullword pixels for performing stretch.
C      AREA   - SIZE field in terms of fullword pixels for sampling pixels to
C               determine range of input DNs.
C      IFORMAT- The input data format -- REAL or FULL.
C      OFORMAT- The output data format -- BYTE or HALF. 
C      LIMITS - Lower and upper stretch limits (output DNs).
C      NOCLIP - Causes the range of output DNs to be limited by the output data
C               format instead of by the LIMITS parameter.
C      PERCENT- Percentage of sampled pixels to be stretched to or beyond the
C               stretch limits.
C      HPERCENT-Percentage of sampled pixels to be stretched to or beyond the
C               upper stretch limit.
C      LPERCENT-Percentage of sampled pixels to be stretched to or beyond the
C               lower stretch limit.
C      SORT   - The maximum number of pixels which will be sampled in the
C               area specified for AREA.
C      SINC   - If SINC=n, then output file will contain just every nth pixel 
C               in each line.
C      LINC   - If LINC=n, then output file will contain just every nth line.
C      EXCL   - Intervals of input DNs to be excluded from the list of
C               sampled pixels.
C      PRINT  - To print the sorted list of DNs of sampled pixels.
C  OUTPUT PARAMETERS
C      The output image produced is written to the output file.
C  PROGRAM LIMITATIONS
C      1. The input image must be REAL*4 or fullword (INTEGER*4) data.
C      2. The output image must be byte or halfword data.
C  SUBROUTINES CALLED
C      MAIN44
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      INCLUDE 'VICMAIN_FOR'
C
      SUBROUTINE MAIN44 
C
C#######################################################################
C  NAME OF ROUTINE
C     MAIN44 (name for top level subroutine by VICAR convention)
C
C  PURPOSE
C      MAIN44 processes parameters entered by user and calls STRETCHIT
C      via STACKA to perform stretch.
C      
C  CONVERTED FOR USE ON MIPL SYSTEM BY   
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION     JANUARY 1984
C  FOR
C      EARTH RESOURCES APPLICATIONS
C  ENVIRONMENT
C      VAX 11/780    VMS  with TAE/VICAR2 EXECUTIVE       FORTRAN-77
C  CALLING SEQUENCE
C      Standard subroutine call and return.
C  INPUT AND OUTPUT PARAMETERS     
C      SEE UNDER PROGRAM ASTRTCHR.
C      
C  CALLED BY
C      ASTRTCHR
C  SUBROUTINES CALLED
C      The library routines  ABEND, STACKA.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C    17 JULY 79   ...JJL...    INITIAL RELEASE
C
      IMPLICIT NONE
      EXTERNAL STRETCHIT
C
C   PARAMETERS
C
      INTEGER      INTCODE_PAR, REALCODE_PAR
      PARAMETER    (INTCODE_PAR = 4)     ! FORMAT CODE FOR INTEGER*4
      PARAMETER    (REALCODE_PAR = 7)    ! FORMAT CODE FOR REAL*4
C
C   GLOBAL VARIABLES
C
      INTEGER*4    EVALS, INCS, INCL, INCODE, LIMLO, LIMHI, NPIXEL
      INTEGER*4    NPIXOUT, NSORT, OUTMIN, OUTMAX, INUNIT, OUTUNIT
      INTEGER*4    STATUS, SLW, SSW, NLW, NSW
      INTEGER*4    AREA(4)
      REAL*4       EXCL(600)
      REAL*4       PERHI, PERLO, REPVAL
      CHARACTER*4  OUTFMT
      LOGICAL      INREAL, IPRNT, OUTHALF, EXCLUD
      COMMON /C1/  AREA, EVALS, INCS, INCODE, INCL, INREAL, IPRNT, 
     +             LIMLO, LIMHI, NPIXEL, NPIXOUT, NSORT, OUTMIN, 
     +             OUTMAX, OUTHALF, PERHI, PERLO, SLW, SSW, NLW, NSW,
     +             INUNIT, OUTUNIT, OUTFMT, EXCL, EXCLUD, REPVAL
C
C    LOCAL VARIABLES
C
      INTEGER*4    ICNT, ISIZE, NL, NS, NNLI, NNSI
      INTEGER*4    LIMT(2)
      REAL*4       PERC
      CHARACTER*4  INFMT
      CHARACTER*32 FORMT
      CHARACTER*80 MSG1
      LOGICAL      XVPTST
      LOGICAL      ABORTFLAG, NOTIORR
C
C
C=================START OF EXECUTABLE CODE===============================
C
C
C  OPEN THE INPUT
C
      CALL IFMESSAGE('ASTRTCHR version 02-MAY-94')
      CALL XVEACTION('SA',' ')
      CALL XVUNIT(INUNIT,'INP',1,STATUS,' ')
      CALL XVOPEN(INUNIT,STATUS,' ')
      CALL XVGET(INUNIT,STATUS,'NL',NL,'NS',NS,'FORMAT',FORMT,' ')
C
C  CHECK IF THE INPUT IMAGE IS REAL OR FULLWORD DATA.
C
      IF (FORMT.EQ.'FULL') THEN
           INREAL = .FALSE.
           INCODE = INTCODE_PAR
      ELSE IF (FORMT.EQ.'REAL') THEN
           INREAL = .TRUE.
           INCODE = REALCODE_PAR
      ELSE
           NOTIORR=.TRUE.
      END IF
C
C  NOW LOOK AT PARAMETERS ENTERED BY USER.
C
      CALL XVP('IFORMAT',INFMT,ICNT)
      IF ((ICNT.EQ.0).AND.NOTIORR) THEN
         GOTO 5700
      ELSE IF ((ICNT.NE.0).AND.(INFMT.EQ.'FULL')) THEN
         INREAL = .FALSE.
         INCODE = INTCODE_PAR
      ELSE IF ((ICNT.NE.0).AND.(INFMT.EQ.'REAL')) THEN
         INREAL = .TRUE.
         INCODE = REALCODE_PAR
      ENDIF
C
      CALL XVP('OFORMAT',OUTFMT,ICNT)
      IF (OUTFMT.EQ.'HALF') THEN
         OUTHALF = .TRUE.
         OUTMIN = -32768
         OUTMAX =  32767
      ELSE
         OUTHALF = .FALSE.
         OUTMIN = 0
         OUTMAX = 255
      ENDIF
C
      CALL XVSIZE(SLW,SSW,NLW,NSW,NNLI,NNSI)
C
      CALL XVP('AREA',AREA,ICNT)
      IF (ICNT.EQ.0) THEN
         AREA(1) = 1
         AREA(2) = 1
         AREA(3) = NL
         AREA(4) = NS
      ENDIF
C
      CALL XVP('LIMITS',LIMT,ICNT)
      IF (ICNT.NE.0) THEN
         LIMLO = LIMT(1)
         LIMHI = LIMT(2)
      ELSE IF (OUTHALF) THEN
         LIMLO = 0
         LIMHI = 10000
      ELSE
         LIMLO = 0
         LIMHI = 255
      END IF
C
      CALL XVP('PERCENT',PERHI,ICNT)
      PERHI = PERHI/(100.0*2.0)
      PERLO = PERHI
C
      CALL XVP('HPERCENT',PERC,ICNT)
      IF (ICNT.NE.0) PERHI = PERC/100.0
C
      CALL XVP('LPERCENT',PERC,ICNT)
      IF (ICNT.NE.0) PERLO = PERC/100.0
C
      CALL XVP('SORT',NSORT,ICNT)
C
      CALL XVP('SINC',INCS,ICNT)
C
      CALL XVP('LINC',INCL,ICNT)
C
      CALL XVP('EXCL',EXCL,EVALS)
      IF (MOD(EVALS,2).NE.0) GO TO 6300
C
      EXCLUD = XVPTST('STREXCL')
      CALL XVP( 'REPLACE', REPVAL, ICNT)
C
      IPRNT = XVPTST('PRINT')
C
      IF (LIMLO.LT.OUTMIN .OR. LIMLO.GT.OUTMAX) GOTO 6400
      IF (LIMHI.LT.OUTMIN .OR. LIMHI.GT.OUTMAX) GOTO 6400
C
      IF ( .NOT. XVPTST('NOCLIP') )   THEN
           OUTMIN = LIMLO                      ! DEFAULT IS TO USE LIMLO
           OUTMAX = LIMHI                      ! AND LIMHI AS OUTPUT DN RANGE.
      END IF
C
      CALL XVMESSAGE (' ',' ')
      WRITE (MSG1,5000) FLOAT(LIMLO)
5000  FORMAT ('LOWER OUTPUT LIMIT = ',E10.4)
      CALL XVMESSAGE(MSG1,' ')
C
      WRITE (MSG1,5100) FLOAT(LIMHI)
5100  FORMAT ('UPPER OUTPUT LIMIT = ',E10.4)
      CALL XVMESSAGE(MSG1,' ')
C
C  CALL SPECIAL LIBRARY SUBROUTINE STACKA TO ALLOCATE THE NECESSARY BUFFERS
C  AND TO CALL SUBROUTINE STRETCHIT.
C
      NPIXEL = MAX0(AREA(4), NSW)
      ISIZE = 4*NPIXEL
      NPIXOUT = (NSW + INCS - 1) / INCS 
C
      CALL STACKA(6, STRETCHIT, 3, ISIZE, NSORT*4, 2*NPIXOUT, ABORTFLAG)
C
      IF (ABORTFLAG) GOTO 7000      ! DID STRETCHIT HAVE AN ERROR IN
                                    ! EXECUTING SUBROUTINE STRETCHIT.
      CALL XVCLOSE(INUNIT,STATUS,' ')   ! IF NO ERROR, THEN DONE.
      CALL XVCLOSE(OUTUNIT,STATUS,' ')
      RETURN
C
C
CCC ERROR PROCESSING
C
5700  CONTINUE
      CALL XVMESSAGE('INPUT MUST BE FULLWORD OR REAL*4 DATA',' ')
      GOTO 7100            ! CALL ABEND 
C
6300  CONTINUE
      CALL XVMESSAGE(
     .  'ERROR: ODD NUMBER OF EXCL PARAMETER VALUES ENTERED',' ')
      GOTO 7100            ! CALL ABEND 
C
6400  CONTINUE
      CALL XVMESSAGE('ERROR: LIMIT PARAMETER VALUE OUT OF RANGE',' ')
      GOTO 7100            ! CALL ABEND 
C
CCCCCCC
C
7000  CONTINUE
      CALL XVCLOSE(OUTUNIT,STATUS,' ')
7100  CONTINUE
      CALL XVCLOSE(INUNIT,STATUS,' ')
      CALL ABEND                 ! ABNORMAL END. (NO RETURN FROM ABEND.)
C
      END
C
C
      SUBROUTINE STRETCHIT(IN, INL, SORT, SORTL, OUT, OUTL, ABORTFLAG)
C#######################################################################
C  NAME OF ROUTINE
C     STRETCHIT ( STRETCH IT )
C
C  PURPOSE
C      STRETCHIT performs automatic linear stretches on floating point
C      and fullword integer pictures.
C      
C  PREPARED FOR USE ON MIPL SYSTEM BY   
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION     JANUARY 1984
C  FOR
C      EARTH RESOURCES APPLICATIONS
C  ENVIRONMENT
C      VAX 11/780    VMS  with TAE/VICAR1 EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C     1-84  SP   CONVERTED FROM IBM VICAR VERSION: MISCELLANEOUS CLEANUP.
C  CALLING SEQUENCE
C      CALLED VIA LIBRARY SUBROUTINE STACKA.
C  INPUT PARAMETERS 
C      IN(J)             - LINE BUFFER FOR THE INPUT IMAGE.
C       array              THE PIXEL INDEX GOES FROM 1 TO NPIXEL.
C                          IN IS AN REAL*4 ARRAY.
C      INL               - NUMBER OF BYTES ALLOCATED BY STACKA FOR IN.
C      SORT(N)           - BUFFER FOR SORTING DNS.  SORT IS A REAL*4
C       array              ARRAY.
C      SORTL             - NUMBER OF BYTES ALLOCATED BY STACKA FOR SORT.
C      OUT(J)            - LINE BUFFER FOR THE OUTPUT IMAGE.
C       array              THE PIXEL INDEX GOES FROM 1 TO NPIXOUT.
C                          OUT IS AN INTEGER*2 ARRAY.
C      OUTL              - NUMBER OF BYTES ALLOCATED BY STACKA FOR OUT.
C
C  OUTPUT PARAMETERS
C      ABORTFLAG - .TRUE. IF STRETCHIT FAILED. .FALSE. IF SUCCESSFUL. ABORTFLAG
C                  IS A LOGICAL*4 VARIABLE.
C  CALLED BY
C      STACKA
C  SUBROUTINES CALLED 
C      The library routine PRNT
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C   GLOBAL VARIABLES
C
      INTEGER*4    EVALS, INCS, INCL, LIMLO, LIMHI, NPIXEL
      INTEGER*4    NPIXOUT, NSORT, OUTMIN, OUTMAX, INUNIT, OUTUNIT
      INTEGER*4    STATUS, SLW, SSW, NLW, NSW
      INTEGER*4    AREA(4)
      REAL*4       EXCL(600)
      REAL*4       PERHI, PERLO, REPVAL
      CHARACTER*4  OUTFMT
      LOGICAL      INREAL, IPRNT, OUTHALF, REPLAC, EXCLUD
      COMMON /C1/  AREA, EVALS, INCS, INCODE, INCL, INREAL, IPRNT, 
     +             LIMLO, LIMHI, NPIXEL, NPIXOUT, NSORT, OUTMIN, 
     +             OUTMAX, OUTHALF, PERHI, PERLO, SLW, SSW, NLW, NSW,
     +             INUNIT, OUTUNIT, OUTFMT, EXCL, EXCLUD, REPVAL
C
C    SUBROUTINE PARAMETERS
C
      REAL*4       IN(NPIXEL), SORT(NSORT)
      INTEGER*2    OUT(NPIXOUT)
      INTEGER*4    OUTL, INL, SORTL
      LOGICAL      ABORTFLAG
C
C    LOCAL VARIABLES
C
      INTEGER*4    I, IE, IEXHI, IEXLO, INTVAL, ISKIP, J, K
      INTEGER*4    L, LINC, LINE, M, N, NLOUT
      REAL*4       FOUT,     FOUTMAX,  FOUTMIN
      REAL*4       OFFSET, REALVAL, SLOPE, VALLO, VALHI
      REAL*4	   STRETCH_MIN(2), STRETCH_MAX(2)
      CHARACTER*73 LABL
      CHARACTER*80 MSG2, MSG3, MSG4
C
C     DATA STATEMENTS
C
      DATA LABL    / ' ' /
C
      EQUIVALENCE (REALVAL, INTVAL)
C
C=================START OF EXECUTABLE CODE===============================     
C
      ABORTFLAG = .FALSE.               ! INITIALIZE TO SUCCESSFUL-SO-FAR.
C
C....CHECK IF STACKA GOT ENOUGH MEMORY. 
C
      IF ( OUTL .LT. 2*NPIXOUT)  THEN
         ABORTFLAG = .TRUE.             ! INDICATE SUBROUTINE FAILURE.
         RETURN				! EXIT SUBROUTINE.
      END IF
C
C    COMPUTE GRID INTERVALS AT WHICH TO SAMPLE FOR SORTING 
C
      ISKIP = SQRT(FLOAT( AREA(3)*AREA(4) ) / NSORT) / INCS + 0.5
      IF (ISKIP .LT. 1)   ISKIP=1
      LINC = INCS*ISKIP
C
C    LOAD PIXELS INTO SORT BUFFER 'SORT'.  THE IDEA IS TO SAMPLE
C    UNIFORMLY OVER THE PICTURE AND STORE THE DNS IN THE SORT BUFFER.
C
      N=0 
      I=0
      DO LINE=AREA(1), AREA(1)+AREA(3)-1, LINC
        CALL XVREAD(INUNIT,IN,STATUS,'NSAMPS',AREA(4),'SAMP',AREA(2),
     +              'LINE',LINE,' ')
        DO J = 1, AREA(4), INCS
           I=I+1
           IF (I .EQ. ISKIP)   THEN 
               N=N+1
               SORT(N)=IN(J)
               IF (N .GE. NSORT)  GOTO 2000
               I=0
           END IF
        END DO
      END DO
2000  CONTINUE
C
C    APPLY EXCLUDING IF ANY 
C
      IF (EVALS .GT. 0) THEN
          M = 0
          DO 2100  K = 1, N
             IF (INREAL) THEN
                DO IE = 1, EVALS, 2
                   IF (EXCL(IE) .LE. SORT(K) .AND.                 
     +               SORT(K) .LE. EXCL(IE+1))  GOTO 2100          
                END DO 
             ELSE                                                 
                DO IE = 1, EVALS, 2
                   IEXLO = NINT(EXCL(IE))
                   IEXHI = NINT(EXCL(IE+1))
                   REALVAL = SORT(K)
                   IF (IEXLO .LE. INTVAL .AND.         
     +               INTVAL .LE. IEXHI) GOTO 2100
                END DO		
             ENDIF      	 
             M = M + 1             ! SKIP THE BAD VALUES AND
             SORT(M) = SORT(K)     ! COPY THE GOOD VALUES
2100      CONTINUE
          N = M
          IF (N .LT. 2)   THEN 
              CALL XVMESSAGE('ENTIRE RANGE EXCLUDED',' ') 
              ABORTFLAG = .TRUE.               ! INDICATE SUBROUTINE FAILURE.
              RETURN			       ! EXIT SUBROUTINE.
          END IF
      END IF
C
      CALL XVMESSAGE (' ',' ')
      WRITE (MSG2, 2110) N
2110  FORMAT ('NUMBER OF SORTED PIXELS = ', I11)
      CALL XVMESSAGE (MSG2,' ')
C
      CALL XVMESSAGE (' ',' ')
      WRITE (MSG3, 2120) LINC
2120  FORMAT ('LINE INTERVAL =   ', I11)
      CALL XVMESSAGE (MSG3,' ')
C
      WRITE (MSG3, 2130) ISKIP*INCS
2130  FORMAT ('SAMPLE INTERVAL = ', I11)
      CALL XVMESSAGE (MSG3,' ')
C
C    PERFORM THE SORT 
C
      IF (INREAL) THEN
         CALL SORTR(SORT,N)
      ELSE
         CALL SORTIN(SORT,N)
      ENDIF
      IF (IPRNT) THEN
         CALL XVMESSAGE (' ',' ')
         CALL PRNT(INCODE,N,SORT,'SORTED INTENSITIES .')
         CALL XVMESSAGE (' ',' ')
      ENDIF
C
C    COMPUTE PERCENT LIMITS 
C
      IF (INREAL) THEN
         I = PERLO*N+1.5
         VALLO = SORT(I)
         I = (1.0-PERHI)*N+.5
         IF (I .GT. N)   I=N
         VALHI = SORT(I)
      ELSE
         I = PERLO*N+1.5
         REALVAL = SORT(I)
         VALLO = INTVAL
         I = (1.0-PERHI)*N+.5
         IF (I .GT. N)   I=N
         REALVAL = SORT(I)
         VALHI = INTVAL
      ENDIF
C
      CALL XVMESSAGE (' ',' ')
      WRITE (MSG4, 2150) VALLO
2150  FORMAT ('LOWER HISTOGRAM BOUND = ', E10.4)
      CALL XVMESSAGE (MSG4,' ')
C
      WRITE (MSG4, 2160) VALHI
2160  FORMAT ('UPPER HISTOGRAM BOUND = ', E10.4)
      CALL XVMESSAGE (MSG4,' ')
C
C   COMPUTE INTENSITY TRANSFORMATION 
C
      IF (VALLO .NE. VALHI)  THEN 
          SLOPE = FLOAT(LIMHI-LIMLO)/(VALHI-VALLO)
          OFFSET = LIMHI - SLOPE*VALHI 
      ELSE
          SLOPE = 1.0 
          OFFSET = 0.0
          CALL XVMESSAGE('NO TRANSFORMATION PERFORMED',' ')
      END IF
C
C   OPEN OUTPUT FILE.
C
      NLOUT = (NLW+INCL-1)/INCL
      CALL XVUNIT(OUTUNIT,'OUT',1,STATUS,' ')
      CALL XVOPEN(OUTUNIT,STATUS,'OP','WRITE','U_NL',NLOUT,'U_NS',
     +            NPIXOUT,'O_FORMAT',OUTFMT,'U_FORMAT','HALF',' ')
C
      WRITE (LABL, 2170) VALLO, VALHI, FLOAT(LIMLO), FLOAT(LIMHI)
2170  FORMAT ('INPUT LIMITS ',E10.4,' ',E10.4,'  MAP TO OUTPUT LIMITS'
     .        ,F8.1,' ',F8.1)
C
      CALL XLADD(OUTUNIT,'HISTORY','COMMENT',LABL,STATUS,'ERR_ACT',
     +           'S','FORMAT','STRING','ULEN',73,' ')
C
C
C  ADD STRETCH_MINIMUM AND STRETCH_MAXIMUM KEYWORDS TO VICAR LABEL
C
      STRETCH_MIN(1) = VALLO
      STRETCH_MIN(2) = REAL(LIMLO)
      STRETCH_MAX(1) = VALHI
      STRETCH_MAX(2) = REAL(LIMHI)
C
      CALL XLADD(OUTUNIT,'HISTORY','STRETCH_MINIMUM',STRETCH_MIN,
     + 		 STATUS,'ERR_ACT','S','FORMAT','REAL','NELEMENT',2,' ')
      CALL XLADD(OUTUNIT,'HISTORY','STRETCH_MAXIMUM',STRETCH_MAX,
     + 		 STATUS,'ERR_ACT','S','FORMAT','REAL','NELEMENT',2,' ')
C
C  STRETCHING LOOP:  STRETCH LINE BY LINE.
C
      L = 0
      FOUTMIN = OUTMIN
      FOUTMAX = OUTMAX
C
      DO LINE = SLW, SLW+NLW-1, INCL
C
	L = L + 1
	CALL XVREAD( INUNIT, IN, STATUS, 'NSAMPS', NSW, 'SAMP', SSW,
     1			 'LINE',LINE,' ')
C
	N = 0
	IF (INREAL) THEN
	  DO J = 1, NSW, INCS
	    N = N + 1
	    REPLAC = .FALSE.
	    IF (EXCLUD .AND. EVALS.GT.0) THEN
	      DO IE = 1, EVALS, 2
		IF (EXCL(IE).LE.IN(J) .AND. IN(J).LE.EXCL(IE+1)) THEN
		  FOUT = REPVAL
		  REPLAC = .TRUE.
		ENDIF
	      END DO 
	    ENDIF
	    IF (.NOT.REPLAC) THEN
	      FOUT = IN(J) * SLOPE + OFFSET 
	      IF (FOUT .LT. FOUTMIN) FOUT = FOUTMIN     
	      IF (FOUT .GT. FOUTMAX) FOUT = FOUTMAX
	    ENDIF
	    OUT(N) = NINT(FOUT)       ! NINT ROUNDS TO NEAREST INTEGER.
	  END DO
C
	ELSE
	  DO J = 1, NSW, INCS
	    N = N + 1
	    REALVAL = IN(J)
	    REPLAC = .FALSE.
	    IF (EXCLUD .AND. EVALS.GT.0) THEN
	      DO IE = 1, EVALS, 2
		IEXLO = NINT(EXCL(IE))
		IEXHI = NINT(EXCL(IE+1))
		IF (IEXLO.LE.INTVAL .AND. INTVAL.LE.IEXHI) THEN
		  FOUT = REPVAL
		  REPLAC = .TRUE.
		ENDIF
	      END DO 
	    ENDIF
	    IF (.NOT.REPLAC) THEN
	      FOUT = INTVAL * SLOPE + OFFSET 
              IF (FOUT .LT. FOUTMIN) FOUT = FOUTMIN     
              IF (FOUT .GT. FOUTMAX) FOUT = FOUTMAX
	    ENDIF
	    OUT(N) = NINT(FOUT)       ! NINT ROUNDS TO NEAREST INTEGER.
	  END DO
	ENDIF
C
        CALL XVWRIT(OUTUNIT,OUT,STATUS,'NSAMPS',NPIXOUT,' ')
C
      END DO              ! END OF STRETCHING LOOP.
C
      RETURN
C
C  RETURN TO MAIN44 VIA STACKA.
C
      END

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create astrtchr.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM astrtchr

   To Create the build file give the command:

		$ vimake astrtchr			(VMS)
   or
		% vimake astrtchr			(Unix)


************************************************************************/


#define PROGRAM	astrtchr
#define R2LIB

#define MODULE_LIST astrtchr.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create astrtchr.pdf
process help=*
!  FILE NAMES      
!
PARM INP     TYPE=STRING   COUNT=1
PARM OUT     TYPE=STRING   COUNT=1
!
PARM SIZE    TYPE=INTEGER  COUNT=4       DEFAULT=(1,1,0,0)
!
PARM AREA    TYPE=INTEGER  COUNT=4       DEFAULT=(1,1,0,0)
!
PARM IFORMAT TYPE=KEYWORD  COUNT=1       DEFAULT=REAL     VALID=(FULL,REAL)
PARM OFORMAT TYPE=KEYWORD  COUNT=1       DEFAULT=BYTE     VALID=(BYTE,HALF)
!
PARM LIMITS  TYPE=INTEGER  COUNT=2       DEFAULT=(0,0)    VALID=(-32768:32767)
PARM NOCLIP  TYPE=KEYWORD  COUNT=(0:1)   DEFAULT=--       VALID=NOCLIP
!
PARM PERCENT  TYPE=REAL    COUNT=1       DEFAULT=0        VALID=(0.0:100.0)
PARM HPERCENT TYPE=REAL    COUNT=1       DEFAULT=0        VALID=(0.0:100.0)
PARM LPERCENT TYPE=REAL    COUNT=1       DEFAULT=0        VALID=(0.0:100.0)
!
PARM SORT     TYPE=INTEGER COUNT=1       DEFAULT=10000    VALID=(1:999999)
!
PARM SINC     TYPE=INTEGER COUNT=1       DEFAULT=1        VALID=(1:999999)
PARM LINC     TYPE=INTEGER COUNT=1       DEFAULT=1        VALID=(1:999999)
PARM EXCL     TYPE=REAL    COUNT=(2:600) DEFAULT=(0.0,0.0)
!  Current TAE limit for COUNT is 600.
PARM STREXCL  TYPE=KEYWORD COUNT=(0:1)   DEFAULT=--       VALID=STREXCL
PARM REPLACE  TYPE=REAL    COUNT=1       DEFAULT=0
!
PARM PRINT   TYPE=KEYWORD  COUNT=(0:1)   DEFAULT=--       VALID=PRINT
!
END-PROC
.TITLE
""astrtchr""
.HELP
 PURPOSE:

"astrtchr" is a general purpose VICAR applications program which performs 
automatic linear stretches on floating point and fullword integer pictures.
"astrtchr" can also be used to make pictures of GALGEN light transfer
calibration files.
.PAGE
 EXECUTION:

The input image may either be fullword floating point or fullword integer
data.  The SIZE parameter specifies the portion of the image that will be 
stretched and then written to the output file. The AREA parameter specifies 
the portion of the input image to use in determining the stretch limits. 
The default for both the AREA and SIZE parameters is to use the full input 
image.  Both AREA and SIZE give the starting pixel number and number of 
pixels per line instead of values measured in bytes. 

The output image may either be byte or halfword data.  The size of the output
image is determined by the SIZE parameter. 

Program "astrtchr" uses dynamic memory allocation (using subroutine STACKA)
to avoid imposing restrictions on the image size.
.PAGE
TAE COMMAND LINE FORMAT
      The following command line formats show the major allowable forms:
      "astrtchr" INP=a OUT=b optional parameters
      "astrtchr" a b optional parameters

       Here 'a' represents the input image file name, and
       'b' represents the output image file name.
.PAGE
EXAMPLE

      "astrtchr" INP=A OUT=B  PERCENT=2 EXCL=(1000 1.0E10)

      In this example the output file B is a byte data image derived
      from the input image A.  The data format of A is obtained from the
      VICAR label for A.  The image is stretched to the default limits of 
      0 to 255. The linear stretch is defined to produce 1 percent saturation
      at each end of the output DN range.  The EXCL parameter effectively
      excludes input DNs greater than or equal to 1000 in the determination
      of the linear function used for stretching.  If the STREXCL keyword
      is specified, then the values supplied in the EXCL parameter are
      also excluded from the actual stretch operation, and are instead set
      to the REPLACE value.

.PAGE
RESTRICTIONS

1. The input image must be floating point (REAL*4) or fullword
   integer (INTEGER*4) data.
2. The output image must be byte or halfword data.

 OPERATION:

"astrtchr" is called an automatic linear stretch program because the user
does not have to specify the range of DNs in the input image to use in
defining the linear function used for the stretch.  Program "astrtchr" follows
a two step process.  The first step determines the linear function (stretch)
to use based on the contents of the input image file and options specified
by the user.  The second step applies the linear function to the DNs of
the input image and writes the result to the output file.  The net effect
is that the image is stretched to the specified limits without requiring
the user to know the range of DNs in the input image.

The way in which the linear function is determined depends on several
parameters. The AREA parameter specifies the portion of the input image
to examine in determining (perhaps approximately) the range of input DNs
for the linear function.  The range is determined as follows.  From the
portion of the image determined by the AREA parameter, the program selects
N evenly spaced pixels, where N is the value of the SORT parameter.  The
DNs for these pixels are examined and any DNs excluded by the EXCL parameter
are removed from the list of sample DNs.  This list is then sorted to produce
a sort of histogram.  The PERCENT parameter ( or the LPERCENT and HPERCENT
parameters) are then applied to the sorted list to determine the range of
DNs to use for the linear function.  If the PERCENT parameter is 0 or is
defaulted, then the minimum and maximum DNs in the list are used as the range.
Otherwise, the range is obtained by skipping the specified percentage of
DNs at the ends of the sorted list.

For example, suppose the AREA parameter specified a full 1000 by 1000 image,
the SORT parameter was 10000, no EXCL parameter was entered, and PERCENT=2
was specified.  The program would sample DNs at 10000 pixels spaced 10 lines
and 10 pixels apart.  The program would then select the 101st and 9900th DNs
in the sorted list as the range of DNs for the linear function.

Once the range of input DNs is determined, the linear function that maps
the range to the stretch limits (specified through the LIMITS parameter)
can be determined algebraicly.  This function is then applied to the input
DNs in the region specified by the SIZE parameter to produce the output
image.  Since the range of input DNs used to determine the linear function
is not necessarily the minimum and maximum DNs in the input image, output
DNs outside of the range of the stretch limits are possible.   (See the
NOCLIP parameter.)  The output data format may restrict the output DN values,
however.  For byte data output files, output DN values less than 0 are changed
to 0 and output DN values greater than 255 are set to 255. For halfword data
output files, output DN values less than -32768 are changed to -32768 and
output DN values greater than 32767 are set to 32767. 

"astrtchr" writes a history label for the output file to show what linear
function (stretch) was used to produce the output file. VICAR keywords
STRETCH_MINIMUM and STRETCH_MAXIMUM correspond to the limits of the 
input data and the output DN values to which they map. For example,
if input image DN values ranged from -1,204 to 32,431 in halfword format,
and the output limits were specified as (0,255), then the values of 
STRETCH_MINIMUM would be (-1204,0) and the values of STRETCH_MAXIMUM would
be (32431,255). These keywords are also PDS Data Dictionary keywords and 
their VICAR/PDS definitions are as follows:

STRETCH_MINIMUM
The stretch_minimum element provides a sequence of values, the first value 
representing the lowest sample value in an input image to be mapped to the
lower output limit and the second value representing the lowest output
sample value in the output image (first value in the LIMITS parameter.) 
Sample values between stretch_minimum and stretch_maximum are interpolated 
over the range of DNs as specified by other parameters of "astrtchr".

STRETCH_MAXIMUM
The stretch_maximum element provides a sequence of values, the first value 
representing the highest sample value in an input image to be mapped to the
upper output limit and the second value representing the highest output
sample value in the output image (second value in the LIMITS parameter.) 
Sample values between stretch_minimum and stretch_maximum are interpolated 
over the range of DNs as specified by other parameters of "astrtchr".

.PAGE
 WRITTEN BY:             Steve Pohorsky              11 Jan 1984
 COGNIZANT PROGRAMMER:   Steve Pohorsky              11 Jan 1984

 MIPS TRACEABILITY:	 MIPS FRD D-4419 Rev. C, Req. 4.1.2.3 <205>

 REVISION:               6                           13 Apr 1987

 January 17, 1994	JFM	Added to VICAR history label the PDS standard
				keywords STRETCH_MINIMUM and STRETCH_MAXIMUM
				as an alternate way of expressing input and
				output stretch limits. (FR 82901 for GLL NIMS)
 April 4, 1994          CRI     Ported to UNIX 
.LEVEL1
.VARIABLE INP
Input file name
.VARIABLE OUT
Output file name
.VARIABLE SIZE
standard VICAR size field
SIZE in terms of fullword
pixels for performing stretch.
.VARIABLE AREA    
SIZE field in terms of fullword
pixels for sampling pixels to
determine range of input DNs.
.VARIABLE IFORMAT
The input data format -- 
REAL or FULL.
.VARIABLE OFORMAT 
The output data format -- 
BYTE or HALF. 
.VARIABLE LIMITS  
Lower and upper stretch limits
(output DNs).
.VARIABLE NOCLIP 
Causes the range of output DNs 
to be limited by output format
instead of LIMITS parameter.
.VARIABLE PERCENT  
Percentage of sampled pixels to
be stretched to or beyond the
stretch limits.
.VARIABLE HPERCENT 
Percentage of sampled pixels to
be stretched to or beyond the
upper stretch limit.
.VARIABLE LPERCENT 
Percentage of sampled pixels to
be stretched to or beyond the
lower stretch limit.
.VARIABLE SORT
The maximum number of pixels
which will be sampled in the
area specified for AREA.
.VARIABLE SINC
If SINC=n, then output file will
contain just every nth pixel in
each line.
.VARIABLE LINC     
If LINC=n, then output file will
contain just every nth line.
.VARIABLE EXCL
Intervals of input DNs to be
excluded from the list of
sampled pixels.
.VARI STREXCL 
Exclude EXCL DNs from the stretch
operation itself
.vari REPLACE
Replaces excluded DNs if STREXCL
.VARIABLE PRINT
Enter PRINT=PRINT to print the
sorted list of DNs of sampled
pixels.
.LEVEL2
.VARIABLE SIZE
Four integer values are entered for SIZE. These determine the position
and size of the portion (of the image) that will be stretched and then written
to the output file.  The default is to stretch the full input image.  The first
two values for the SIZE parameter are the starting line and starting pixel
number within the line.  The last two values for the SIZE parameter are the
number of lines and number of pixels per line.  The second and fourth values 
are specified in terms of fullword pixels instead of in terms of bytes.
Although the SIZE and AREA values are often the same in practice, they
are seen as mutually independent by the program.
.VARIABLE AREA    
Four integer values are entered for AREA.  These determine the position and
size of the portion (of the image) that will be used for sampling pixels to
determine the range of input DNs. The default is to use the full input image. 
The first two values for the AREA parameter are the starting line and starting
pixel number within the line.  The last two values for the AREA parameter are
the number of lines and number of pixels per line.  The AREA parameter differs
from the standard VICAR SIZE parameter in that the second and fourth values are
specified in terms of fullword pixels instead of in terms of bytes. Although
the SIZE and AREA values are often the same in practice, they are seen as
mutually independent by the program.  Thus the AREA parameter does not affect
the size of the output file. 
.VARIABLE IFORMAT
If INFMT is not specified, the format is obtained from the input image label. 
If the format is not in the label, real data is assumed.   In TAE command 
mode the user should enter 'FULL or 'REAL as stand-alone keywords.
.VARIABLE OFORMAT
The default format is byte data.  If HALF is specified, the output pixels
may have any valid halfword value.  In TAE command mode the user should 
enter 'BYTE or 'HALF as stand-alone keywords.
.VARIABLE LIMITS  
This parameter is entered in the form  LIMITS=(l,u) where l is the lower
stretch limit and u is the upper stretch limit.  The linear function for the
stretch is determined so that lower and upper percent values from the
sorted list of sampled pixels are mapped respectively to the lower and
upper stretch limits.  For byte output data, the default limits are 0 and 255.
For halfword output data, the default limits are 0 and 10000.  l and u must 
be valid values for the output data format.  For either data format, output
DNs are allowed outside of the range from l to u as long as they are valid for
the output data type and if NOCLIP is specified.  The image may be 'inverted' by
specifying l greater than u and specifying NOCLIP. 
.VARIABLE NOCLIP 
The NOCLIP parameter causes the range of output DNs to be limited by the 
output data format instead of by the LIMITS parameter.
.VARIABLE PERCENT  
The PERCENT parameter (or the LPERCENT and HPERCENT parameters) determines
which input DN gets mapped to the lower stretch limit and which input DN gets
mapped to the upper stretch limit.  Each of these parameters is refers 
to a percentage of the elements in the sorted list of sampled pixels.
Each of these parameters is a real number in the range from 0.0 to 100.0.  
PERCENT=p is equivalent to LPERCENT=p/2 and HPERCENT=p/2.  If LPERCENT=l,
then the input DN which gets mapped to the lower stretch limit is the element
in the sorted list l percent from the beginning of the list.  If HPERCENT=h,
then the input DN which gets mapped to the upper stretch limit is the element
in the sorted list h percent from the end of the list.  The default for these
parameters is 0 percent.  If the percent parameters are 0 or are
defaulted, then the minimum and maximum DNs in the list are used.

For example, suppose the AREA parameter specified a full 1000 by 1000 image,
the SORT parameter was 10000, no EXCL parameter was entered, and PERCENT=2
was specified.  The program would sample DNs at 10000 pixels spaced 10 lines
and 10 pixels apart.  The program would then select the 101st and 9900th DNs
in the sorted list as the range of DNs for the linear function.
.VARIABLE HPERCENT
(See under PERCENT.)
.VARIABLE LPERCENT 
(See under PERCENT.)
.VARIABLE SORT
The SORT parameter tells the program how much space to allocate for the list
of sampled pixels.  The number of pixels sampled is the minimum of the SORT
value and the number of pixels in the AREA area.  The default for SORT is 
10000.
.VARIABLE SINC     
The default for SINC is 1.  SINC is of use in generating pictures of GALGEN
calibration files. If SINC=N, then for a given line in the SIZE area the
program processes the starting pixel and every Nth pixel thereafter. Thus the
number of pixels per output line is smaller than the number of pixels per line
in the SIZE area by a factor of N.  Also, if SINC=N, the sampling is
restricted in a given line (in the AREA area) to the starting pixel and every 
Nth pixel thereafter.  This restriction is important for GALGEN files to assure
that the sorted list contains only pertinent DNs. 
.VARIABLE LINC
The default for LINC is 1.  If LINC=N, then the program processes the starting
line and every Nth line thereafter in the SIZE area.  Thus the number of 
output lines is smaller than the number of lines in the SIZE area by a 
factor of N.
.VARIABLE EXCL
One or more pairs of numbers can be entered for the EXCL parameter.  Each
pair specifies a range of input DNs from the first value in the pair to the
second value in the pair.  Any DNs of sampled pixels that lie in one of the
EXCL ranges are removed from the list of sample pixels.  The values can be 
entered as real numbers or integers to match the input file data format.
.VARI STREXCL 
Specifying this keyword causes DN values that are in the range(s) specified
by the EXCL parameter to be excluded from the stretch operation itself, not
just from the computation of the stretch parameters.  These DNs are set to
the value specified by the REPLACE parameter.
.vari REPLACE
This parameter is only used if keyword STREXCL is specified, and it supplies
the replacement value to be used for DNs excluded from the stretch operation.
.VARIABLE PRINT
This can yield a lot of numbers.  The default is to not print the numbers.
In TAE command mode the user should enter 'PRINT as a stand-alone keyword.
.END
$ Return
$!#############################################################################
