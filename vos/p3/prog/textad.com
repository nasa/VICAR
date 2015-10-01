$!****************************************************************************
$!
$! Build proc for MIPL module textad
$! VPACK Version 1.9, Wednesday, March 10, 2010, 12:33:18
$!
$! Execute by entering:		$ @textad
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
$ write sys$output "*** module textad ***"
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
$ write sys$output "Invalid argument given to textad.com file -- ", primary
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
$   if F$SEARCH("textad.imake") .nes. ""
$   then
$      vimake textad
$      purge textad.bld
$   else
$      if F$SEARCH("textad.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake textad
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @textad.bld "STD"
$   else
$      @textad.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create textad.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack textad.com -mixed -
	-s textad.f -
	-p textad.pdf -
	-i textad.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create textad.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44     !TEXTAD
C
C     10 MAY 91    ...REA...  CONVERT TO UNIX/VICAR
C     11 NOV 88    ...CCM...  HANDLED NCHAR=0 CASE (I.E. DO NOTHING!)
C     26 JULY 85   ...JHR...  CONVERTED TO VICAR2
C     10 NOV 83    ...JAM...  CONVERTED TO VAX
C     22 JULY 81   ...JAM...  ADD KEYWORDS 'ADD' AND 'MODULATE'
C     18 JUNE 80   ...JAM...  INCREASE BUFFER SIZE TO 10000
C     18 OCT 79    ...JDA...  INITIAL RELEASE
C
      IMPLICIT INTEGER (A-Z)
      INTEGER*4 OUNIT,STAT,IPARM(1000),BACKDN,DN
      INTEGER*4 SL(100),SS(100),EL(100),ES(100)
      INTEGER*2 HBUF(20000)
      LOGICAL*4 XVPTST
      LOGICAL*1 LBUF(40000),CBUF(4000),BBUF(100)
      CHARACTER*80 PRT
      CHARACTER*100 STRING(100)
C
C        INITIALIZE AND SET DEFAULTS
      SIZKEY=1
      SIZE=6
      DN=255
      MODU=0
      MODE=1
      IBACK=0
      NTEXT=0

C          OPEN INPUT DATA SET
      CALL XVUNIT(IUNIT,'INP',1,STAT,' ')
      CALL XVOPEN(IUNIT,STAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
C
C        GET SIZE INFORMATION AND CHECK
      CALL XVGET(IUNIT,STAT,'PIX_SIZE',NBPP,' ')
      IF(NBPP .NE. 1) CALL MYABORT(' TEXTAD accepts byte data only')
      CALL XVSIZE(SLO,SSO,NLO,NSO,NLI,NSI)
      IF(SLO+NLO-1 .GT. NLI) CALL MYABORT(
     +			' NUMBER OF LINES REQUESTED EXCEEDS INPUT SIZE')
      IF(SSO+NSO-1 .GT. NSI) CALL MYABORT(
     +		      ' NUMBER OF SAMPLES REQUESTED EXCEEDS INPUT SIZE')
      IF(NSO.GT.20000) CALL MYABORT(
     +			       ' NUMBER OF SAMPLES EXCEEDS BUFFER SIZE')
C
C        OPEN OUTPUT DATA SET
      CALL XVUNIT(OUNIT,'OUT',1,STAT,' ')
      CALL XVOPEN(OUNIT,STAT,'OPEN_ACT','SA','IO_ACT','SA','OP','WRITE',
     +		  'U_NL',NLO,'U_NS',NSO,' ')
C
C        COPY INPUT AREA TO OUTPUT
      ELO=SLO+NLO-1
      DO LINE=SLO,ELO
        CALL XVREAD(IUNIT,LBUF,STAT,'LINE',LINE,'SAMP',SSO,
     +		    'NSAMPS',NSO,' ')
        CALL XVWRIT(OUNIT,LBUF,STAT,'NSAMPS',NSO,' ')
      ENDDO
C
C        RE-OPEN OUTPUT FOR UPDATE AND WITH HALFWORD BUFFER
      CALL XVCLOSE(OUNIT,STAT,' ')
      CALL XVOPEN(OUNIT,STAT,'OP','UPDATE','U_FORMAT','HALF',
     +		  'OPEN_ACT','SA','IO_ACT','SA',' ')
C
C        PROCESS PARAMETERS
C     NOTE ... PARAMS ARE RELATIVE TO INPUT IMAGE
C
C        'LARGE'
      IF(XVPTST('LARGE')) THEN
         SIZE=12
         SIZKEY=2
      END IF
C        'WHITE'
      IF(XVPTST('WHITE')) THEN
         MODE=1
         DN=255
      END IF
C        'BLACK'
      IF(XVPTST('BLACK')) THEN
         MODE=1
         DN=0
      END IF
C        'ADD'
      CALL XVPARM('ADD',IPARM,ICOUNT,IDEF,1)
      IF(ICOUNT.EQ.1) THEN
         MODE=2
         DN=IPARM(1)
      END IF
C        'BACKGROUND'
      CALL XVPARM('BACKGRND',IPARM,ICOUNT,IDEF,1)
      IF(ICOUNT.EQ.1) THEN
         IBACK=1
         BACKDN=IPARM(1)
      END IF
C        'MODULATE'
      IF(XVPTST('MODULATE')) MODU=1

C        'TEXT' AND 'STRING'
      CALL XVPARM('TEXT',IPARM,ICOUNT,IDEF,200)
      IF(ICOUNT.NE.0) THEN
         NTEXT=ICOUNT/2
         IF(2*NTEXT.NE.ICOUNT) CALL MYABORT(
     +				  ' INVALID COUNT FOR PARAMETER "TEXT"')
C           PUT LINE/SAMP COORDINATES IN TERMS OF OUTPUT IMAGE
         DO I=1,NTEXT
            SL(I)=IPARM(2*(I-1)+1)-SLO+1
            SS(I)=IPARM(2*(I-1)+2)-SSO+1
         END DO
      END IF
      CALL XVPARM('STRING',STRING,ICOUNT,IDEF,100)
      IF(ICOUNT.NE.NTEXT) CALL MYABORT(
     +			' INCONSISTENT TEXT/STRING COUNTS')
C        GET LENGTH OF STRING
      DO 300 I=1,NTEXT
         CALL ADD0(STRING(I),100,NCHAR)
	 IF(NCHAR.EQ.0)  GO TO 300
C           CHECK THAT TEXT FITS INSIDE IMAGE AREA
         NLSTR=7*SIZKEY
         IF(IBACK.EQ.1) NLSTR=9*SIZKEY
         IF(SL(I).LT.1 .OR. SL(I)+NLSTR-1.GT.NLO) THEN 
	    WRITE (PRT,100) SL(I)
  100	    FORMAT(' ***STRING TOO TALL TO BE LOCATED AT LINE',I5)
	    CALL MYABORT(PRT)
         END IF
         NSSTR=NCHAR*SIZE
         IF(IBACK.EQ.1) NSSTR=NSSTR+2*SIZKEY
         IF(SS(I).LT.1 .OR. SS(I)+NSSTR-1.GT.NSO) THEN
	    WRITE (PRT,150) SS(I)
  150	    FORMAT(' ***STRING TOO WIDE TO BE LOCATED AT SAMPLE',I5)
	    CALL MYABORT(PRT)
         END IF
C           IMPLEMENT MODULATE IF SPECIFIED
         IF(MODU .NE. 0) THEN
            NUM=0
            ITOT=0
            REC=SL(I)
            IF(IBACK.EQ.1) REC=SL(I)+SIZKEY
            SAMP=SS(I)
            IF(IBACK.EQ.1) SAMP=SS(I)+SIZKEY
            ESAMP=SAMP+NCHAR*SIZE-1
            DO IL1=1,7
               DO IL2=1,SIZKEY
                  CALL XVREAD(OUNIT,HBUF,STAT,'LINE',REC,' ')
                  DO IS=SAMP,ESAMP
                     ITOT=ITOT+HBUF(IS)
                     NUM=NUM+1
                  ENDDO
                  REC=REC+1
               ENDDO
            ENDDO
            AVE=ITOT/NUM
            DN=255
            IF(AVE.GT.128)  DN=0
         END IF
C
         REC=SL(I)
C           SET LINE(S) ABOVE TEXT TO BACKGROUND DN
         IF(IBACK.EQ.1) THEN
            DO IL2=1,SIZKEY
               CALL XVREAD(OUNIT,HBUF,STAT,'LINE',REC,'NSAMPS',NSO,' ')
               CALL MVE(-6,NSSTR,BACKDN,HBUF(SS(I)),0,1)
               CALL XVWRIT(OUNIT,HBUF,STAT,'LINE',REC,'NSAMPS',NSO,' ')
               REC=REC+1
            END DO
         END IF            
C           PROCESS TEXT LINES
         SSTEXT=SS(I)
         IF(IBACK.EQ.1) SSTEXT=SS(I)+SIZKEY
	 CALL MVL(STRING(I),BBUF,NCHAR)
         DO IL1=1,7
            CALL TEXT(BBUF,NCHAR,IL1-1,CBUF,SIZE,255)
            DO IL2=1,SIZKEY
               CALL XVREAD(OUNIT,HBUF,STAT,'LINE',REC,'NSAMPS',NSO,' ')
               IF(IBACK.EQ.1) THEN
                  CALL MVE(-6,NSSTR,BACKDN,HBUF(SS(I)),0,1)
               END IF
               CALL DNADD(HBUF,CBUF,SSTEXT,NCHAR,SIZE,MODE,DN)
               CALL XVWRIT(OUNIT,HBUF,STAT,'LINE',REC,'NSAMPS',NSO,' ')
               REC=REC+1
            END DO
         END DO
C           SET LINE(S) BELOW TEXT TO BACKGROUND DN
         IF(IBACK.EQ.1) THEN
            DO IL2=1,SIZKEY
               CALL XVREAD(OUNIT,HBUF,STAT,'LINE',REC,'NSAMPS',NSO,' ')
               CALL MVE(-6,NSSTR,BACKDN,HBUF(SS(I)),0,1)
               CALL XVWRIT(OUNIT,HBUF,STAT,'LINE',REC,'NSAMPS',NSO,' ')
               REC=REC+1
            END DO
         END IF            
  300 CONTINUE
C
C        'AREA'
      CALL XVPARM('AREA',IPARM,ICOUNT,IDEF,400)
      IF(ICOUNT.NE.0) THEN
         NRECT=ICOUNT/4
         IF(4*NRECT.NE.ICOUNT) CALL MYABORT(
     +				  ' INVALID COUNT FOR PARAMETER "AREA"')
C           PUT COORDINATES IN TERMS OF OUTPUT IMAGE 
         DO I=1,NRECT
            SL(I)=IPARM(4*(I-1)+1)-SLO+1
            SS(I)=IPARM(4*(I-1)+2)-SSO+1
            EL(I)=IPARM(4*(I-1)+3)-SLO+1
            ES(I)=IPARM(4*(I-1)+4)-SSO+1
         END DO
      END IF
      DO I=1,NRECT
         NL=EL(I)-SL(I)+1
         NS=ES(I)-SS(I)+1
         IF (SL(I).LT.1.OR.EL(I).GT.NLO) CALL MYABORT(
     +				' RECTANGLE EXCEEDS IMAGE SIZE (LINES)')
         IF (SS(I).LT.1.OR.ES(I).GT.NSO) CALL MYABORT(
     +			      ' RECTANGLE EXCEEDS IMAGE SIZE (SAMPLES)')
         CALL ITLA(255,CBUF,NS)
C           DRAW TOP OF RECTANGLE
         CALL XVREAD(OUNIT,HBUF,STAT,'LINE',SL(I),'NSAMPS',NSO,' ')
         CALL DNADD(HBUF,CBUF,SS(I),NS,1,MODE,DN)
         CALL XVWRIT(OUNIT,HBUF,STAT,'LINE',SL(I),'NSAMPS',NSO,' ')
C           DRAW BOTTOM OF RECTANGLE
         CALL XVREAD(OUNIT,HBUF,STAT,'LINE',EL(I),'NSAMPS',NSO,' ')
         CALL DNADD(HBUF,CBUF,SS(I),NS,1,MODE,DN)
         CALL XVWRIT(OUNIT,HBUF,STAT,'LINE',EL(I),'NSAMPS',NSO,' ')
C           DRAW SIDES OF RECTANGLE
         CALL ITLA(0,CBUF(2),NS-2)
         REC=SL(I)+1
         NL=NL-2
         DO IL=1,NL
            CALL XVREAD(OUNIT,HBUF,STAT,'LINE',REC,'NSAMPS',NSO,' ')
            CALL DNADD(HBUF,CBUF,SS(I),NS,1,MODE,DN)
            CALL XVWRIT(OUNIT,HBUF,STAT,'LINE',REC,'NSAMPS',NSO,' ')
            REC=REC+1
         ENDDO
      END DO
C
C        CLOSE DATA SETS
      CALL XVCLOSE(IUNIT,STAT,' ')
      CALL XVCLOSE(OUNIT,STAT,' ')
C
      RETURN
      END
C******************************************************************************
      SUBROUTINE DNADD(HBUF,CBUF,SS,NCHAR,SIZE,MODE,DN)
C
      IMPLICIT INTEGER (A-Z)
      INTEGER*2 HBUF(10000)
      LOGICAL*1 CBUF(4000)
C
      N=SIZE*NCHAR
      LP=SS-1
C
      IF (MODE .EQ. 1) THEN				!  SET LETTERS TO DN
	  DO I=1,N
	      IF (IV(CBUF(I)).GT.0) HBUF(LP+I)=DN
	  END DO
      ELSE						!  ADD DN TO LETTERS
	  DO I=1,N
	      IF (IV(CBUF(I)).GT.0) HBUF(LP+I)=HBUF(LP+I)+DN
	  END DO
      END IF
      RETURN
      END

C*************************************************************************
        subroutine MYABORT(msg)
        character*(*) msg
        call xvmessage(msg, ' ')
        call abend
        return
        end

$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create textad.pdf
process help=*
PARM INP      TYPE=(STRING,60)  COUNT=1  
PARM OUT      TYPE=(STRING,60)  COUNT=1
PARM SIZE     TYPE=INTEGER COUNT=4                  DEFAULT=(1,1,0,0)
PARM SL       TYPE=INTEGER DEFAULT=1
PARM SS       TYPE=INTEGER DEFAULT=1
PARM NL       TYPE=INTEGER DEFAULT=0
PARM NS       TYPE=INTEGER DEFAULT=0
PARM DN       TYPE=KEYWORD VALID=(WHITE,BLACK,MODULATE) DEFAULT=WHITE
PARM ADD      TYPE=INTEGER COUNT=0:1                DEFAULT=--
PARM LARGE    TYPE=KEYWORD COUNT=0:1 VALID=LARGE    DEFAULT=--
PARM BACKGRND TYPE=INTEGER COUNT=0:1                DEFAULT=--
PARM TEXT     TYPE=INTEGER COUNT=(0,2:200)          DEFAULT=--
PARM STRING   TYPE=STRING  COUNT=(0,1:100)          DEFAULT=--
PARM AREA     TYPE=INTEGER COUNT=(0,4:300)          DEFAULT=--
END-PROC
.TITLE
 TEXTAD
.HELP
 PURPOSE AND OPERATION:
 TEXTAD is an application program which allows a user to overwrite
 an image with text and/or rectangles.

 The section of the input specified in the SIZE parameter is first copied
 and then operated on by the program. 

 COMMAND FORMAT:

 TEXTAD  INP  OUT  SIZE  PARAMS
 
 where INP, OUT, and SIZE are standard Vicar parameters and PARAMS includes
 special parameters for TEXTAD. All parameters are described in their
 respective parameter section in TUTOR mode.
.PAGE 
 Example:
 TEXTAD A B SIZE=(1,1,4000,4000) 'LARGE 'WHITE +
 TEXT=(1200,600,1220,607) +
 STRING=("HOW CAN YOU BE IN TWO PLACES AT ONCE", +
        "WHEN YOU ARE REALLY NOWHERE AT ALL")

 This will add two lines of text to an output picture.  The first 
 line of text will be located with the upper left corner at pixel
 number 1200,600.  The second line will be located relative to 
 1220,607.
.PAGE
 RESTRICTIONS:

 A maximum of 100 text strings may be specified.
 Each string has a maximum of 100 characters.
 A maximum of 75 rectangles may be specified.

 Line/Sample coordinates entered as parameters are in terms of the 
 input image.

 The characters that TEXTAD recognizes are all the alphabetic and numeric
 characters, and most special characters. The special (non-alphanumeric) 
 characters recognized are:

 $ . , < > ( ) + - _ ! ? : ; / * # ' " = & %

 All alphabetic characters are written as upper case, regardless of the
 case of the input string parameters.

 The quote (") and ampersand (&) characters have some special restrictions
 due due their syntactic significance for TAE:

  To include a quote in a string, the string must be inside quotes and the
  quote character to be included must be doubled. E.g.:
        string="He said ""Hi""" yields the string: He said "Hi".

  To include an ampersand character in a string, the ampersand must be
  doubled; the string need not be quoted (unless required because of some
  other character). E.g.:
        string=&&out or string="&&out" both yield the string: &out.
.PAGE
*************************
 WRITTEN BY:  M. BLACKSTONE
 COGNIZANT PROGRAMMER: C. C. AVIS    SEPT. 1984
 DOCUMENTATION AUTHOR: SUE LAVOIE
 REVISION: 3
 
.LEVEL1
.VARIABLE INP
 input data set
.VARIABLE OUT
 output data set
.VARIABLE SIZE
standard VICAR  size field
.VARIABLE SL
starting line
.VARIABLE SS
starting sample
.VARIABLE NL
number of lines
.VARIABLE NS
number of samples
.VARIABLE LARGE
'LARGE - large characters
.VARIABLE DN
The DN used to form characters
Valid: WHITE, BLACK, MODULATE 
.VARIABLE BACKGRND
Set the text background 
to this DN
.VARIABLE ADD
add this DN to create the text
.VARIABLE AREA
 Defines rectangle areas
 SL1,SS1,EL1,ES1, 
 SL2,SS2,EL2,ES2, ...
.VARIABLE TEXT
 Defines starting coordinates 
 for strings
 SL1,SS1, SL2,SS2, ...
.VARIABLE STRING
 Character strings to be written
 "string1","string2", ...
.LEVEL2
.VARIABLE INP
 a standard VICAR input dataset
.VARIABLE OUT
 a standard VICAR output dataset
.VARIABLE SIZE
 (SL,SS,NL,NS)  starting line,starting sample,number
  of lines, number of samples
.VARIABLE SL
 The starting line of the size field
.VARIABLE SS
 The starting sample of the size field
.VARIABLE NL
 The number of lines of the size field
.VARIABLE NS
 The number of samples of the size field
.VARIABLE LARGE
 Large characters, 14 lines by 12 samples, are used
 Default is that characters 7 lines by 6 samples are drawn.
.VARIABLE DN
 The three possible keywords (BLACK, WHITE, and MODULATE) control the DN
 used in character and line definition.  BLACK produces 0 DN characters;
 WHITE produces 255 DN characters; MODULATE produces 0 DN characters if the
 average DN under the text is greater than 128, 255 DN characters otherwise.
 If the parameter ADD is used, it supercedes these keywords.  The default
 is WHITE.
.VARIABLE BACKGRND
 BACKGRND=DNVALUE
 A one pixel (two pixels if large is specified) border around 
 the text string and the background for the text string is set 
 to DNVALUE.  
.VARIABLE ADD
 ADD=DN      The DN specified will be added to the data
 to create the text.  If specified, this value supercedes WHITE, BLACK, or 
 MODULATE keywords.
.VARIABLE TEXT
 TEXT=(SL1,SS1, SL2,SS2, ...)
 These are the coordinates for positioning the associated string of text.
 Each pair of numbers specify the line and sample where the upper left
 corner of the associated string is to be located. If the parameter 
 BACKGRND is specified, the upper left corner of the border area around
 the text string is positioned at these coordinates. This means the
 text string itself will start 1 pixel (2 if LARGE is specified) down
 and to the right of these coordinates.  Up to 100 pairs of numbers
 may be specified.
.VARIABLE STRING
 STRING=("string1","string2", ...)
 These are the text strings to be inserted into the image at the positions
 specified by the associated TEXT values. Up to 100 strings may be
 specified. Each string may have up to 100 characters.
.VARIABLE AREA
 AREA=(SL1,SS1,EL1,EL1, SL2,SS2,EL2,ES2, ...) 
 Specifies that rectangles with upper left corners at SLn,SSn and
 lower right corners at ELn,ESn will be drawn. Up to 75 rectangles
 may be specified, each rectangle being specified by a group of
 four numbers.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create textad.imake
#define  PROGRAM   textad

#define MODULE_LIST textad.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
