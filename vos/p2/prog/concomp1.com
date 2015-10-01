$!****************************************************************************
$!
$! Build proc for MIPL module concomp1
$! VPACK Version 1.7, Wednesday, July 20, 1994, 06:45:37
$!
$! Execute by entering:		$ @concomp1
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
$ write sys$output "*** module concomp1 ***"
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
$ write sys$output "Invalid argument given to concomp1.com file -- ", primary
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
$   if F$SEARCH("concomp1.imake") .nes. ""
$   then
$      vimake concomp1
$      purge concomp1.bld
$   else
$      if F$SEARCH("concomp1.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake concomp1
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @concomp1.bld "STD"
$   else
$      @concomp1.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create concomp1.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack concomp1.com -
	-s concomp1.f -
	-i concomp1.imake -
	-p concomp1.pdf -
	-t tstconcom.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create concomp1.f
$ DECK/DOLLARS="$ VOKAGLEVE"
       PROGRAM  CONCOMP1
C#######################################################################
C  NAME OF ROUTINE
C      CONCOMP1 ( CONnected COMPonents )
C
C  PURPOSE
C      THIS IS THE STANDARD MAIN PROGRAM USED FOR TAE/VICAR PROGRAMS.
C      THIS MODULE CALLS SUBROUTINE MAIN44 TO ENTER INTO THE BODY OF THE
C      PROGRAM.
C      Program CONCOMP1 removes high frequency noise from classified or
C      stratified images such as those produced by program FASTCLAS.
C  PREPARED FOR USE ON MIPL SYSTEM BY
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION    DECEMBER 1983
C  FOR
C      EARTH RESOURCES APPLICATIONS
C
C  ORIGINAL CONCOMP1 PROGRAM BY
C      STEVE FRIEDMAN 
C
C  ENVIRONMENT
C      VAX 11/780    VMS  with TAE/VICAR2 EXECUTIVE       FORTRAN-77
C     
C  CALLING SEQUENCE (TAE COMMAND LINE)
C      The following command line formats show the major allowable forms:
C
C      CONCOMP1 INP=a OUT=b SIZE=(sl,ss,nl,ns) optional parameters
C      CONCOMP1 INP=a OUT=b SL=sl SS=ss NL=nl NS=ns optional parameters
C      CONCOMP1 a b (sl,ss,nl,ns) optional parameters
C      CONCOMP1 a b optional parameters
C 
C      CONCOMP1 INP=a OUT=(b,m) SIZE=(sl,ss,nl,ns) optional parameters
C      CONCOMP1 INP=a OUT=(b,m) SL=sl SS=ss NL=nl NS=ns optional parameters
C      CONCOMP1 a (b,m) (sl,ss,nl,ns) optional parameters
C      CONCOMP1 a (b,m) optional parameters
C 
C       Here 'a' represents the input image file names,
C       'b' represents the output image file name,
C       and 'm' represents the mask file name.
C
C  INPUT PARAMETERS (listed by keyword)
C      INP    - Input file names.
C      OUT    - Output file name.
C      SIZE   - Standard Vicar size field:  (SL,SS,NL,NS)
C               SL = Starting line number.
C               SS = Starting sample number.
C               NL = Number of lines.
C               NS = Number of samples.
C      THRESH - Threshold of connectivity factor for pixel replacement.
C      REPLACE- New DN value for pixels that are replaced.
C      MODE   - To use the mode of DNs in the window as the new DN
C               value for pixels that are replaced.
C      RANGE  - Range of DNs to consider in finding the mode.
C  OUTPUT PARAMETERS
C      The output image produced is written to the output file.
C      The mask file is produced if requesed.
C  PROGRAM LIMITATIONS
C      1. The input, output, and mask images must be byte data.
C  SUBROUTINES CALLED
C      MAIN44
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                      ORIGINAL HEADER COMMENTS
C  VICAR PROGRAM CONCOMP1        S.Z. FRIEDMAN      FEBRUARY 1980
C
C   13 JAN 80   ...SZF...   INITIAL RELEASE
C
C   03 SEP 94   ...CRI...   MSTP S/W CONVERSION
C
C  PROGRAM CONCOMP1 IS A BASIC CONNECTED COMPONENTS ROUTINE THAT
C  SEARCHES FOR AND REMOVES ISOLATED PIXELS AND/OR SMALL GROUPS OF
C  PIXELS.  A 3 BY 3 MOVING WINDOW IS USED TO FIND ISOLATED
C  PIXELS BY COMPARING THE CENTRAL PIXEL OF THE WINDOW WITH ITS ADJACENT
C  PIXELS.  THE NUMBER OF SIMILAR PIXELS ARE COUNTED TO DETERMINE
C  THE CONNECTIVITY FACTOR.  IF THE FREQUENCY OF OCCURANCE IS LESS THAN
C  A SUPPLIED THRESHOLD CONNECTIVITY FACTOR, THE CENTRAL PIXEL
C  IS REPLACED EITHER BY A GIVEN VALUE OR BY THE MODE OF THE
C  DISTRIBUTION WITHIN THE WINDOW.
C  A MASK OF CHANGED PIXELS CAN BE GENERATED ON REQUEST.
C
C
C  PARAMETERS
C     THRESH(VAL)      THRESHOLD LIMIT OR MINIMUM CONNECTIVITY FACTOR
C                      TO BE MET FOR NO REPLACEMENT.
C                      A CONNECTIVITY FACTOR OF LESS THAN THRESH WILL
C                      CAUSE A REPLACEMENT.  (ACCEPTABLE VALUES
C                      FOR THRESH ARE 0-8,DEFAULT=1)
C     RANGE(LO,HI)     DN RANGE:  LODN,HIDN (DEFAULT=0,255)
C     REPLACE(NEW)     REPLACEMENT DN SPECIFICATION:  IF THRESH IS
C                      NOT MET, THE CENTRAL PIXEL IS REPLACED WITH 'NEW'
C                      (DEFAULT=255)
C     REPLACE MODE     REPLACEMENT MODE SPECIFICATION:  SAME
C                      AS ABOVE EXCEPT THAT WHEN THRESH IS NOT MET, THE
C                      REPLACEMENT IS MADE WITH THE MODE OF THE
C                      DISTRIBUTION (NO DEFAULT)
C     DBUG(L)          DIAGNOSTIC PARAMETER AND PRINT LEVEL  (**removed)
C
C                      END OF ORIGINAL HEADER COMMENTS
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44 
C#######################################################################
C  NAME OF ROUTINE
C     MAIN44 (name for top level subroutine by VICAR convention)
C
C  PURPOSE
C      MAIN44 processes parameters entered by the user and calls
C      WORK via STACKA to make any changes to the image.
C      
C  CONVERTED FOR USE ON MIPL SYSTEM BY   
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION     DECEMBER 1983
C  FOR
C      EARTH RESOURCES APPLICATIONS
C  ENVIRONMENT
C      VAX 11/780    VMS  with TAE/VICAR2 EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C
C    12-83  SP   CONVERTED FROM IBM VICAR VERSION: MISCELLANEOUS CLEANUP.
C    12-83  SP   MODIFIED SO THAT RDCHEK AND WRCHEK USED FOR I/O ERROR MESSAGES
C    12-83  SP   CHANGED ALL LINE BUFFERS TO INTEGER*2 TO AVOID IV AND ITL.
C    12-83  SP   MADE IN ARRAY TWO-DIMENSIONAL FOR READABILITY. DECREASED THE
C                NUMBER OF BUFFERS IN IN ARRAY FROM NLW+2 TO NLW.
C    12-83  SP   CHANGED WRITES TO ASYNCHRONOUS I/O.
C     5-84  SP   INCREASED BUFFER SIZE FOR LABELC SINCE OREC NOW IS 0.
C     6-87  REA  CONVERT TO FULL VICAR2
C    03-SEP-94   CRI... MSTP S/W CONVERSION
C
C  CALLING SEQUENCE
C      Standard subroutine call and return.
C  INPUT AND OUTPUT PARAMETERS     
C      SEE UNDER PROGRAM CONCOMP1.
C      
C  CALLED BY
C      CONCOMP1
C  SUBROUTINES CALLED
C      WORK plus the library routines STACKA, XVOPEN, XVPARM, XVPCNT,
C      XVPTST, XVSIZE, XVUNIT
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IMPLICIT INTEGER(A-Z)

      ! THE PROGRAM IS GENERALIZED FOR MODIFIABILITY SO THAT ONLY THE FOLLOWING
      ! TWO PARAMETERS NEED TO BE CHANGED TO CHANGE THE WINDOW DIMENSIONS. 
      ! EACH DIMENSION SHOULD BE AN ODD INTEGER.  (RANGE OF VALID THRESH VALUES
      ! WOULD NEED CHANGING TOO.  FOR LARGER WINDOWS, CHECKING IS NOT BASED ON
      ! GEOMETRICAL CONNECTIVITY SINCE JUST USING A HISTOGRAM.)

      PARAMETER (WWIDTH_PAR=3)    ! WINDOW WIDTH IN SAMPLES.
      PARAMETER (WLENGTH_PAR=3)   ! WINDOW LENGTH IN LINES.
      EXTERNAL WORK

      COMMON /VAL/ SL,SS,NL,NS,NLW,NSW,LODNP1,HIDNP1,
     &             DELTAS,NEWVAL,THRESH, INSIZE, HALFW
      COMMON /LOGCAL/ MASK,MODE
      LOGICAL MASK,MODE,XVPTST
      INTEGER IPARM(2)
C
      CALL IFMESSAGE('CONCOMP1 version 31-OCT-94')
C
C=================START OF EXECUTABLE CODE===============================
C  INITIALIZATION

      NLW = WLENGTH_PAR       ! STORE WINDOW DIMENSIONS.
      NSW = WWIDTH_PAR
      HALFW = NLW * NSW / 2   ! HALF THE NUMBER OF PIXELS IN THE WINDOW.

C  PARAMETER PROCESSING

      MODE = XVPTST('MODE')
      CALL XVPARM('REPLACE',NEWVAL,ICNT,IDEF,1)
      CALL XVPARM('THRESH',THRESH,ICNT,IDEF,1)
      CALL XVPARM('RANGE',IPARM,ICNT,IDEF,2)
      LODNP1 = IPARM(1)+1
      HIDNP1 = IPARM(2)+1
C
C  OPEN DATA SETS
C
      CALL XVPCNT('OUT',NDSOUT)
      MASK = (NDSOUT.EQ.2)
      CALL XVUNIT(INUNIT,'INP',1,ISTAT,' ')
      CALL XVOPEN(INUNIT,ISTAT,'U_FORMAT','HALF',' ')
      CALL XVSIZE(SL,SS,NL,NS,NLIN,NSIN)
      CALL XVUNIT(IOUT1,'OUT',1,ISTAT,' ')
      CALL XVOPEN(IOUT1,ISTAT,'U_NL',NL,'U_NS',NS,
     +            'U_FORMAT','HALF','OP','WRITE',' ')
      IF (MASK) THEN
	  CALL XVUNIT(IOUT2,'OUT',2,ISTAT,' ')
	  CALL XVOPEN(IOUT2,ISTAT,'U_NL',NL,'U_NS',NS,
     +                'U_FORMAT','HALF','OP','WRITE',' ')
      END IF
C
C  DETERMINE MINIMUM SIZES NEEDED FOR DATA SETS
C  NOTE THAT IN BUFFER IS USED FOR INPUT AND LABEL PROCESSING
C
      INSIZE = (NS+NSW-1)*NLW*2
      OSIZE = 2 * NS
      MSIZE = 4
      IF(MASK) MSIZE = OSIZE

C  CALL SPECIAL LIBRARY SUBROUTINE STACKA TO ALLOCATE THE NECESSARY BUFFERS
C  AND TO CALL SUBROUTINE WORK. (WE ALLOCATE NLW LINE BUFFERS TOGETHER IN A
C  TWO-DIMENSIONAL ARRAY.

      CALL STACKA(9,WORK,3,OSIZE,MSIZE,INSIZE,INUNIT,IOUT1,IOUT2)
      RETURN
      END
      SUBROUTINE WORK(OUT,IOUT,MSK,IMSK,IN,IIN,INUNIT,IOUT1,IOUT2)
C#######################################################################
C  NAME OF ROUTINE
C     WORK ( do the WORK )
C
C  PURPOSE
C      WORK removes high frequency noise from classified or
C      stratified images such as those produced by program FASTCLAS.
C      
C  CONVERTED FOR USE ON MIPL SYSTEM BY   
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION     DECEMBER 1983
C  FOR
C      EARTH RESOURCES APPLICATIONS
C  ENVIRONMENT
C      VAX 11/780    VMS  with TAE/VICAR2 EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C    12-83  SP   CONVERTED FROM IBM VICAR VERSION: MISCELLANEOUS CLEANUP.
C                (SEE UNDER MAIN44.)
C    03-SEP-94   CRI... MSTP S/W CONVERSION
C  CALLING SEQUENCE
C      CALLED VIA LIBRARY SUBROUTINE STACKA.
C  INPUT PARAMETERS 
C      IN(I,J)           - LINE BUFFERS FOR NLW LINES WHERE NLW IS THE
C       array              WINDOW LENGTH. (I = 1 TO NLW.)  THE PIXEL INDEX 
C                          GOES FROM 1 TO NS+NSW-1.  
C                          THE DATA FROM A LINE OF THE INPUT IMAGE
C                          BEGINS WITH IN( (NSW+1)/2, I ) AND GOES THROUGH
C                          IN(NS + NSW/2, I).  SPACE IS LEFT AT BOTH ENDS OF A
C                          LINE BUFFER SO THAT A FULL WINDOW CAN BE DEFINED FOR
C                          THE ENDPOINTS OF THE LINE. THIS SPACE IS FILLED BY
C                          REFLECTING DATA ABOUT THE ENDPOINTS.  
C                          FOR NSW=3,
C                          THE LINE DATA IS IN  IN(2,I) THROUGH IN(NS+1,I).
C                          IN(1,I) IS THE SAME AS IN(3,I), AND IN(NS+2,I) IS
C                          THE SAME AS IN(NS,I).
C      IIN               - NUMBER OF BYTES ALLOCATED BY STACKA FOR IN.
C      OUT(J) array      - line buffer for output image.
C      IOUT              - NUMBER OF BYTES ALLOCATED BY STACKA FOR OUT.
C      MSK(J) array      - line buffer for mask output image.
C      IMSK              - NUMBER OF BYTES ALLOCATED BY STACKA FOR MSK.
C      INUNIT		 - UNIT NUMBER OF THE INPUT DATASET
C      OUT1		 - UNIT NUMBER OF THE FIRST OUTPUT DATASET
C      OUT2		 - UNIT NUMBER OF THE SECOND OUTPUT DATASET (IF USED)
C  CALLED BY
C      STACKA
C  SUBROUTINES CALLED 
C      CONCHK plus the library routines MVE, OUTCON, XLADD, XVREAD, XVWRIT
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                 COMMENTS FROM ORIGINAL HEADER
C
C  VICAR PROGRAM CONCOMP1      S.Z. FRIEDMAN       FEBRUARY 1980
C
C  SUBROUTINE WORK HANDLES ALL I/O & CALLS CONCHK ROUTINE
C
C     IN      INPUT BUFFER
C     OUT     OUTPUT ARRAY FOR LINE N
C     MSK     OPTIONAL ARRAY FOR MASK OUTPUT DS FOR LINE N
C     CURRLN  BUFFER NUMBER, CURRENT LINE
C     RELLIN  RELATIVE LINE READ (W/O REFERENCE TO LABEL)
C     DELTAS,SAM1,FROML,FILLL,FROMR,FILLR
C             ARE ALL SAMPLE POINTERS
CCCCCCCCCCC

      IMPLICIT INTEGER(A-Z)
 
      COMMON /VAL/ SL,SS,NL,NS,NLW,NSW,LODNP1,HIDNP1,
     &             DELTAS,NEWVAL,THRESH, INSIZE, HALFW
      COMMON /LOGCAL/ MASK,MODE
      LOGICAL MASK,MODE

      INTEGER*2 IN(NS+NSW-1,NLW),  OUT(NS), MSK(NS)
      CHARACTER*80 L1
C      LOGICAL*1 MSG1(72),L1(69)
C      DATA L1(69)/69*' '/
C
C=================START OF EXECUTABLE CODE===============================     
C
C  PROCESS LABEL INFORMATION
C
C      CALL OUTCON(THRESH,L1,1)
      L1 = ' '
      WRITE (L1(69:69),'(I1)') THRESH
      CALL XLADD(IOUT1,'HISTORY','THRESHLD',L1,ISTAT,'ERR_ACT',
     +           'SA','FORMAT','STRING','ULEN',1,' ')
      IF(MASK)  THEN
	  CALL XLADD(IOUT2,'HISTORY','MASK',
     +		     '0 for changed; =255 for no change',ISTAT,
     +		     'ERR_ACT','SA','FORMAT',
     +               'STRING','ULEN',33,' ')
      END IF
C
C SET VARIABLES CONTROLLING INPUT AND BUFFERING
C
                         ! VALUE FOR 3x3 WINDOW      REMARKS
      STRTRD = SL-1
      RELLIN = 1-NLW/2      !     0
      DELTAS = NSW/2        !     1    LINE DATA IS OFFSET DELTAS SAMPLES FROM
                            !          BEGINNING OF BUFFER.
      SAM1 = (NSW+1)/2      !     2    LINE DATA STARTS IN IN(SAM1,I).
      FROML = SAM1+1        !     3    FOR MIRRORING DATA AT BEGINNING OF LINE.
      FILLL = SAM1-1        !     1    FOR MIRRORING DATA AT BEGINNING OF LINE.
      FROMR = SAM1+NS-2     !     NS   FOR MIRRORING DATA AT END OF LINE.
      FILLR = SAM1+NS       !     NS+2 FOR MIRRORING DATA AT END OF LINE.
      CURRLN = (NLW+1)/2    !     2    LINE BUFFER NUMBER FOR CENTER OF WINDOW.
      IBUF  = 0             !          LINE BUFFER NUMBER FOR READS.
C
C  READ INITIAL LINES INTO CORE, MIRROR IMAGE AT BOUNDARIES.  (MIRRORING
C  DEFINES THE WINDOW FOR PIXELS ALONG THE EDGES OF THE IMAGE AND MAKES
C  SUCH WINDOWS SYMMETRIC WITH RESPECT TO THE EDGES.)

      DO 100 I = 1, NLW-1
      ILINE = RELLIN
      RELLIN = RELLIN+1
70    IF(ILINE .LT. 1) ILINE = 2-ILINE
      IF(ILINE .GT. NL) ILINE = NL+NL-ILINE
      IF(ILINE .LT. 1) GOTO 70
      IBUF = 1 + MOD(IBUF,NLW) ! FORMULA FOR CYCLING THROUGH THE BUFFERS OF IN.
C  READ
      CALL XVREAD(INUNIT,IN(SAM1,IBUF),ISTAT,'LINE',STRTRD+ILINE,
     +	          'SAMP',SS,'NSAMPS',NS,' ')

C  MIRROR
      IF (NSW .EQ. 3) THEN        ! FOR 3x3 WINDOW, MIRRORING IS SIMPLE.
          IN(1,IBUF) = IN(3,IBUF)
          IN(NS+2,IBUF) = IN(NS,IBUF)
      ELSE IF (NSW .GT. 3)  THEN
         CALL MVE(2,DELTAS,IN(FROML,IBUF),IN(FILLL,IBUF),1,-1)
         CALL MVE(2,DELTAS,IN(FROMR,IBUF),IN(FILLR,IBUF),-1,1)
      END IF

100   CONTINUE

C     MAIN LOOP, LOOPING THROUGH EACH OUTPUT LINE.  SINCE THE ALGORITHM DOES
C     NOT DEPEND ON THE GEOMETRY OF PIXELS IN THE WINDOW, WE DO NOT HAVE TO
C     KEEP THE LINES OF THE WINDOW IN ANY ORDER.  WE JUST READ THE NEXT LINE
C     INTO THE OLDEST BUFFER.  CURRLN IS THE LINE INDEX FOR THE IN ARRAY OF THE
C     CENTER LINE.
C
C  BEGIN CONNECTED COMPONENTS CHECK
C  READING IN LAST LINE OF WINDOW BEFORE CALLING CONCHK.
      DO 200 I = 1,NL
      ILINE = RELLIN
      RELLIN = RELLIN+1
150   IF(ILINE .LT. 1) ILINE = 2-ILINE
      IF(ILINE .GT. NL) ILINE = NL+NL-ILINE
      IF(ILINE .LT. 1) GOTO 150
      IBUF = 1 + MOD(IBUF,NLW) ! FORMULA FOR CYCLING THROUGH THE BUFFERS OF IN.
C  READ
      CALL XVREAD(INUNIT,IN(SAM1,IBUF),ISTAT,'LINE',STRTRD+ILINE,
     +	          'SAMP',SS,'NSAMPS',NS,' ')

C  MIRROR
      IF (NSW .EQ. 3) THEN
          IN(1,IBUF) = IN(3,IBUF)
          IN(NS+2,IBUF) = IN(NS,IBUF)
      ELSE IF (NSW .GT. 3)  THEN
         CALL MVE(2,DELTAS,IN(FROML,IBUF),IN(FILLL,IBUF),1,-1)
         CALL MVE(2,DELTAS,IN(FROMR,IBUF),IN(FILLR,IBUF),-1,1)
      END IF

      CALL CONCHK(IN, CURRLN, OUT, MSK)    ! CHECK CONNECTIVITY FOR EACH 
                                           ! PIXEL IN THE LINE.
      CURRLN = MOD(CURRLN,NLW)+1           ! SET FOR NEXT LINE.

C  OUTPUT DATA

      CALL XVWRIT(IOUT1,OUT,ISTAT,'NSAMPS',NS,' ')
      IF (MASK) CALL XVWRIT(IOUT2,MSK,ISTAT,'NSAMPS',NS,' ')

200   CONTINUE

      RETURN
      END
      SUBROUTINE CONCHK(IN, CURRLN, OUT, MSK)
C#######################################################################
C  NAME OF ROUTINE
C     CONCHK (CONnectivity CHecK)
C
C  VICAR PROGRAM CONCOMP1      S.Z. FRIEDMAN      FEBRUARY 1980
C  PURPOSE:
C  SUBROUTINE CONCHK BUILDS AND MAINTAINS A HISTOGRAM TO DETERMINE
C  THE CONNECTIVITY FACTOR FOR EACH PIXEL WITH ITS SURROUNDING
C  PIXELS  AND CHANGES DNS FOR PIXELS NOT MEETING THE THRESHOLD CRITERIA.
C      
C  CONVERTED FOR USE ON MIPL SYSTEM BY   
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION     DECEMBER 1983
C  FOR
C      EARTH RESOURCES APPLICATIONS
C  ENVIRONMENT
C      VAX 11/780    VMS  with TAE/VICAR2 EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C    12-83  SP   CONVERTED FROM IBM VICAR VERSION: MISCELLANEOUS RESTRUCTURING.
C                (SEE UNDER MAIN44.)
C    12-83  SP   CORRECTED TO NOT PUT A -2 (=254) IN IMAGE IF MODALV DOES NOT
C                FIND A VALUE.
C    03-SEP-94   CRI... MSTP S/W CONVERSION
C  CALLING SEQUENCE
C      Standard subroutine call and return.
C  INPUT AND OUTPUT PARAMETERS     
C      SEE UNDER WORK.
C      
C  CALLED BY
C      WORK
C  SUBROUTINES CALLED
C      MODALV plus the library routines MVE, ZIA.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     CURRLN     BUFFER NUMBER, CURRENT LINE
C     CURSAM,ADDSAM,SUBSAM
C                SAMPLE POINTERS
C     CURDNP1    DN VALUE+1 AT CURRENT SAMPLE (CURSAM)
C
C
      IMPLICIT INTEGER(A-Z)

      COMMON /VAL/ SL,SS,NL,NS,NLW,NSW,LODNP1,HIDNP1,
     &             DELTAS,NEWVAL,THRESH, INSIZE, HALFW
      COMMON /LOGCAL/ MASK,MODE
      LOGICAL MASK,MODE

      INTEGER*2 IN(NS+NSW-1,NLW),  OUT(NS), MSK(NS)
      DIMENSION HIST(256)
      INTEGER*2 NOCHNG
      DATA NOCHNG / 255 /

C=================START OF EXECUTABLE CODE===============================
C  CLEAR HISTOGRAM AND BUILD OUTPUT LINES
C  SUBROUTINE MODE CHANGES A PIXELS VALUE ONLY
C  WHEN CHANGES ARE NOTED BY THE MODEL FUNCTION CHECK
C  ALL OTHER SAMPLES REMAIN UNCHANGED
C
      CALL ZIA(HIST,256)
      CALL MVE(2,NS,IN(DELTAS+1,CURRLN),OUT(1),1,1)
      IF(MASK) CALL MVE(2,NS,NOCHNG,MSK(1),0,1)
C
C  FOR EACH PIXEL IN THE LINE MAKE HISTOGRAM FOR WINDOW CENTERED AT THAT PIXEL.
C  THEN CHECK CONNECTIVITY FACTOR.
C  IF THRESH IS NOT MET, REPLACE PIXEL WITH REPLACEMENT VALUE
C  OR MODE OF DISTRIBUTION.
      DO 200 IS = 1,NS
      CURSAM = IS+DELTAS

C  MAKE HISTOGRAM FOR WINDOW. (FOR FIRST SAMPLE, MAKE HISTOGRAM DIRECTLY.
C  FOR SUBSEQUENT SAMPLES, UPDATE PREVIOUS HISTOGRM.)  ( HIST(N+1) IS
C  THE COUNTER FOR DN OF N.)   

      IF ( IS .EQ. 1 )  THEN
        DO I = 1,NLW
        DO J = 1,NSW                  ! MAKE HISTOGRAM FOR FIRST SAMPLE.
          ADD = IN(J,I)+1
          HIST(ADD) = HIST(ADD)+1
        END DO
        END DO

      ELSE
        ADDSAM = CURSAM+DELTAS
        SUBSAM = CURSAM-DELTAS-1
        DO I = 1,NLW
          ADD = IN(ADDSAM,I) + 1      ! UPDATE HIST. FOR SUBSEQUENT SAMPLES.
          SUB = IN(SUBSAM,I) + 1   
          IF(ADD .NE. SUB) THEN
            HIST(ADD) = HIST(ADD)+1
            HIST(SUB) = HIST(SUB)-1
          END IF
        END DO

      END IF

C  CHECK CONNECTIVITY. Compare number of OTHER pixels with same DN with THRESH.
      CURDNP1 = IN(CURSAM,CURRLN) + 1
      IF ( HIST(CURDNP1)-1  .LT. THRESH) THEN     
         IF (MODE)  THEN                        ! IF CONNECTIVITY < THRESH,
            CALL MODALV(HIST,VAL)               ! CHANGE TO MODAL VALUE OR
            IF (VAL .GE. 0) THEN                ! NEWVAL. 
                OUT(IS) = VAL                   ! DO NOT CHANGE IF NO
                IF (MASK)  MSK(IS) = 0          ! MODAL VALUE FOUND.
            END IF

         ELSE
            OUT(IS) = NEWVAL
            IF (MASK)  MSK(IS) = 0

         END IF
      END IF

200   CONTINUE
      RETURN
      END
      SUBROUTINE MODALV(HIST,VAL)
C#######################################################################
C  NAME OF ROUTINE
C     MODALV (MODAL Value)
C
C  VICAR PROGRAM CONCOMP1       S.Z. FRIEDMAN      FEBRUARY 1980
C
C  PURPOSE:
C  SUBROUTINE MODALV IS CALLED WITH THE 'REPLACE MODE' OPTION ONLY
C  AND DETERMINES THE MODAL VALUE OF THE DISTRIBUTION
C
C  CONVERTED FOR USE ON MIPL SYSTEM BY   
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION     DECEMBER 1983
C  FOR
C      EARTH RESOURCES APPLICATIONS
C  ENVIRONMENT
C      VAX 11/780    VMS  with TAE/VICAR2 EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C    12-83  SP   TOOK HIST OUT OF COMMON AND MADE A FORMAL PARAMETER.
C    12-83  SP   ADDED CHECK FOR FREQ > HALFW FOR SPEED IN FINDING MAXIMUM.
C    03-SEP-94   CRI... MSTP S/W CONVERSION
C  CALLING SEQUENCE
C      Standard subroutine call and return.
C  INPUT PARAMETER
C      HIST array  - HISTOGRAM FOR WINDOW.
C  OUTPUT PARAMETER
C      VAL = -2 IF NO DNS IN THE RANGE LODN TO HIDN OCCUR IN WINDOW.
C          = MODAL VALUE OTHERWISE.
C  CALLED BY
C      CONCHK 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      IMPLICIT INTEGER(A-Z)


      COMMON /VAL/ SL,SS,NL,NS,NLW,NSW,LODNP1,HIDNP1,
     &             DELTAS,NEWVAL,THRESH, INSIZE, HALFW
      COMMON /LOGCAL/ MASK,MODE
      LOGICAL MASK,MODE

      DIMENSION HIST(256)
C
C=================START OF EXECUTABLE CODE===============================
      VAL = -1
      FREQ = 0
C
C
C  FIND THE MODE OF THE DISTRIBUTION
C
      DO 100 I = LODNP1,HIDNP1
      IF(HIST(I) .LE. FREQ) GOTO 100
C  POSSIBLE REPLACEMENT
      VAL = I
      FREQ = HIST(I)
      IF ( FREQ .GT. HALFW ) GOTO 200    ! IF FREQ > HALF OF WINDOW, THEN THIS
100   CONTINUE                           ! IS THE MOST FREQUENT.
C
200   CONTINUE
      VAL = VAL-1
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create concomp1.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM concomp1

   To Create the build file give the command:

		$ vimake concomp1		(VMS)
   or
		% vimake concomp1		(Unix)


************************************************************************/


#define PROGRAM	concomp1
#define R2LIB

#define MODULE_LIST concomp1.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST fortport

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create concomp1.pdf
process help=*
!  FILE NAMES      
!
PARM INP     TYPE=STRING   COUNT=1
PARM OUT     TYPE=STRING   COUNT=(1:2)
!
PARM SIZE    TYPE=INTEGER  COUNT=4       DEFAULT=(1,1,0,0)
PARM SL      TYPE=INTEGER  COUNT=1       DEFAULT=1
PARM SS      TYPE=INTEGER  COUNT=1       DEFAULT=1
PARM NL      TYPE=INTEGER  COUNT=1       DEFAULT=0
PARM NS      TYPE=INTEGER  COUNT=1       DEFAULT=0
!
PARM THRESH  TYPE=INTEGER  COUNT=1       DEFAULT=1          VALID=(0:8)
PARM REPLACE TYPE=INTEGER  COUNT=1       DEFAULT=255        VALID=(0:255)
PARM MODE    TYPE=KEYWORD  COUNT=(0:1)   DEFAULT=--         VALID=MODE
PARM RANGE   TYPE=INTEGER  COUNT=2       DEFAULT=(0,255)    VALID=(0:255) 
!
END-PROC
.TITLE
concomp1
.HELP
 PURPOSE:

concomp1 is a VICAR applications program used to remove high frequency
information (noise) components from digital images.  It is usually used as a
post-processing procedure to remove visually distracting detail from classified
or stratified images such as images produced by VICAR programs FASTCLAS or
USTATS.  Such images usually are composed of a number of regions, where each
pixel in a region has the same DN except possibly for some isolated noise
pixels. CONCOMP1 changes the DNs of these isolated noise pixels to match the DN
of the surrounding pixels and writes the resulting image to an output file. 
A mask of changed pixels can be generated on request. 
.PAGE
 EXECUTION:

In order to determine which DNs to change, concomp1 applies a test to each
pixel in the input image.  The test involves looking at a 3 by 3 (pixel)
window centered around the pixel being tested.  The number of other pixels
in the window with the same DN as the pixel being tested is found.  If this
number is less than the threshold value specified by the THRESH parameter,
then the DN for the pixel being tested is changed.  The new value of the DN
is determined by the MODE or REPLACE parameters specified by the user.  If
REPLACE is specified, the new value is just the value specified for the REPLACE
parameter.  If MODE is specified, the new value is generally the mode of the DN
distribution within the window.  More precisely, if MODE is specified, the
new DN is the most frequent DN in the window that lies in the range of DNs
specified for the RANGE parameter.  In case of a tie in determining the 
most frequent DN, the lowest of the DNs in the tie is used.  If no DN in
the specified range occurs in the window, the DN of the pixel tested is not
changed.  The range is commonly set according to the range of DNs for the
regions of the image.  When a DN is changed, the change is not included in 
the subsequent windows. 

A mask file is specified by entering a second output file name for the OUT
parameter.  The mask file is the same size as the (first) output file.  A
pixel in the mask file has a DN of 0 if the DN for the corresponding pixel
in the output file was changed.  All other pixels in the mask file have a
DN of 255.   It is possible that the DN of a pixel in the image be change to
itself, particularly if the value of the THRESH parameter is greater than 3.
In this case, the corresponding pixel in the mask file will be 0.


.PAGE
TAE COMMAND LINE FORMAT
      The following command line formats show the major allowable forms:
      concomp1 INP=a OUT=b SIZE=(sl,ss,nl,ns) optional parameters
      concomp1 INP=a OUT=b  SL=sl SS=ss NL=nl NS=ns optional parameters
      concomp1 a b (sl,ss,nl,ns) optional parameters
      concomp1 a b optional parameters

      concomp1 INP=a OUT=(b,m) SIZE=(sl,ss,nl,ns) optional parameters
      concomp1 INP=a OUT=(b,m)  SL=sl SS=ss NL=nl NS=ns optional parameters
      concomp1 a (b,m) (sl,ss,nl,ns) optional parameters
      concomp1 a (b,m) optional parameters

       Here 'a' represents the input image file name, 
       'b' represents the output image file name,
       and 'm' represents the mask file name.
.PAGE
EXAMPLE

      concomp1 INP=A OUT=B THRESH=1 'MODE

      In this example the center pixel of the window will be changed unless it
      is connected to at least 1 other pixel with the same value.  The new
      value is found by taking the mode of the distribution of values in the
      window.


RESTRICTIONS
1. The input, output, and mask images must be byte data.
.PAGE
 OPERATION:

The operation of concomp1 is similar to the operation of a box-filtering
routine as it uses a moving window.  However, the pixel replacements are
based on a different decision rule.  concomp1 searches for and removes 
isolated pixels  and/or groups of pixels.  A 3 by 3 moving window is used
to find isolated pixels by comparing the central pixel of the window with
its adjacent pixels.  The number of other pixels in the window with the
same value is the 'connectivity factor'.  If the connectivity factor is
less than the THRESH parameter, the central pixel is replaced either by a 
user specified value or by the mode of the distribution within the window.

The window for pixels along the edges of the image is defined by conceptually
reflecting the image data about the edges.  Thus, the second line of the image
is conceptually duplicated above the first line, and the second pixel of each
line is conceptually duplicated to the left of the first pixel in a line.  The
next to last line of the image and the next to the last pixel in each line are
handled analogously.  This method compensates for the fact that edge pixels
have fewer real neighbors. 

The following window operations will demonstrate the operation of concomp1
on various windows.

Example 1:  specifying THRESH=1 and 'MODE

                    5   5   5                   5   5   5 

            INPUT:  3   2   5          OUTPUT:  3   5   5

                    4   3   3                   4   3   3

Example 2:  specifying THRESH=4

                    5   5   5

            INPUT:  3   2   2          OUTPUT:  no change

                    2   2   2 

Example 3:  specifying THRESH=6, 'MODE, and RANGE=(1,5)

                    0   0   0                   0   0   0

            INPUT:  0   0   0          OUTPUT:  0   2   0

                    2   2   1                   2   2   1


 WRITTEN BY:             Steve Pohorsky              14 Dec 1983

 COGNIZANT PROGRAMMER:   S. Z. Friedman              19 Dec 1980

 REVISION:               ORIGINAL                    19 Dec 1980

 MADE PORTABLE FOR UNIX: CRI                         03 OCT 1994

.LEVEL1
.VARIABLE INP
Input file name
.VARIABLE OUT
Output file name optionally
followed by mask file name
.VARIABLE SIZE
Standard Vicar size field:
  (SL,SS,NL,NS)
You can enter SL,SS,NL,
and NS together as SIZE, OR
enter the SL,SS,NL, and NS
parameters separately.
By default, the entire input
image is used if these
parameters are not entered.
.VARIABLE SL
Starting line number
.VARIABLE SS
Starting sample number
.VARIABLE NL
Number of lines
.VARIABLE NS
Number of samples
.VARIABLE THRESH
Threshold of connectivity factor
for pixel replacement.
.VARIABLE REPLACE
New DN value for pixels that
are replaced.
.VARIABLE MODE
Enter to use the mode of
DNs in the window as the new DN
value for pixels that are 
replaced.
.VARIABLE RANGE
Range of DNs to consider in 
finding the mode.
.LEVEL2
.VARIABLE THRESH
In order to determine which DNs to change, concomp1 applies a test to each
pixel in the input image.  The test involves looking at a 3 by 3 (pixel) window
centered around the pixel being tested. The number of OTHER pixels in the
window with the same value is the 'connectivity factor'.  If the connectivity
factor is less than the THRESH parameter, the central pixel is replaced either
by a user specified value or by the mode of the distribution within the window.
The default value for THRESH is 1.  Valid values for THRESH are integers from
0 to 8, inclusive.
.VARIABLE REPLACE
Specify either REPLACE or MODE as the method for determining the new DN value 
for pixels that are to be replaced.  If REPLACE is specified, the new value 
is just the value specified for the REPLACE parameter.  The default value
for REPLACE is 255.  This default is used if neither REPLACE or MODE is
specified.
.VARIABLE MODE
Specify either REPLACE or MODE as the method for determining the new DN value
for pixels that are to be replaced.  If MODE is specified, the new value is
generally the mode of the DN distribution within the window.  More precisely,
if MODE is specified, the new DN is the most frequent DN in the window that
lies in the range of DNs specified for the RANGE parameter.  In case of a tie
in determining the most frequent DN, the lowest of the DNs in the tie is used. 
If no DN in the specified range occurs in the window, the DN of the pixel
tested is not changed.  REPLACE is the default.
.VARIABLE RANGE
The RANGE parameter is used only when MODE is selected. This parameter is
entered in the form RANGE=(LO,HI) where LO and HI are selected by the user. 
This defines a range from a DN of LO to a DN of HI, inclusive.  When a pixel is
changed, the new DN is the most frequent DN in the window that lies in the
range of DNs specified for the RANGE parameter. The range is commonly set
according to the range of DNs for the regions of the image.  The range should
be made as small as the input image allows in order to reduce execution time. 
RANGE=(0,255) is the default. 
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstconcom.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
!
!  THIS IS A TEST OF PROGRAM CONCOMP1
!
!      FIRST BUILD AN INPUT IMAGE USING DNSYMBOL.
!
!GEN CONA NL=5 NS=1
gen CONA NL=100 NS=10 IVAL=0 LINC=0 SINC=0
qsar INP=CONA OUT=CONA1 AREA=(2 4 8 3 90)
!AL CONA1 NL=100 NS=10
!DNSYMBOL CONA CONA1 DIVIDE=1 
list CONA1
!
concomp1 INP=CONA1 OUT=(CONAO,CONAM) THRESH=4
list CONAO
list CONAM
!
!    try with SL and SS not 1.
!
concomp1 INP=CONA1 OUT=CONAO2 SIZE=(66,3,10,6) THRESH=4 'MODE
list CONAO2
!
!    now try various parameter combinations on a file built from GEN and QSAR.
!
gen CONB NL=10 NS=10 SINC=0
qsar CONB CONB1 AREA=( 3,4,1,5,200  5,2,1,1,99   6,5,2,3,100     +
                       1,1,1,1,1    9,10,1,1,100 10,9,1,1,100      )
list CONB1 'ZEROES
!
concomp1 INP=CONB1 OUT=CONB2 
list CONB2 'ZEROES
!
concomp1 INP=CONB1 OUT=CONB3 THRESH=1 REPLACE=99 
list CONB3 'ZEROES
!
concomp1 INP=CONB1 OUT=CONB4 THRESH=2 REPLACE=99 
list CONB4 'ZEROES
!
concomp1 INP=CONB1 OUT=CONB5 THRESH=3 REPLACE=99 
list CONB5 'ZEROES
!
concomp1 INP=CONB1 OUT=CONBR THRESH=2 'MODE
list CONBR 'ZEROES
!
concomp1 INP=CONB1 OUT=CONBV THRESH=2 'MODE RANGE=(1,7)
list CONBV 'ZEROES
!
!    try another file built from GEN and QSAR.
!
gen CONE NL=30 NS=10 SINC=0.3 LINC=.3
qsar CONE CONE1 AREA=( 3,4,1,5,200  5,2,1,1,99   6,5,2,3,100     +
                       13,4,1,5,-2  15,2,1,1,-1  16,5,2,3,1      +
                       23,5,1,2,2   25,2,1,1,3   26,5,2,3,100     +
                       1,1,1,1,1    9,10,1,1,100 10,9,1,1,100      )
list CONE1 'ZEROES
!
concomp1 INP=CONE1 OUT=CONE2 
list CONE2 'ZEROES
!
!    clean up
!
!DCL DELETE CON*.Z*;*
end-proc
$ Return
$!#############################################################################
