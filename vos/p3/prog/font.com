$!****************************************************************************
$!
$! Build proc for MIPL module font
$! VPACK Version 1.8, Tuesday, March 11, 1997, 16:11:42
$!
$! Execute by entering:		$ @font
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
$ write sys$output "*** module font ***"
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
$ write sys$output "Invalid argument given to font.com file -- ", primary
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
$   if F$SEARCH("font.imake") .nes. ""
$   then
$      vimake font
$      purge font.bld
$   else
$      if F$SEARCH("font.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake font
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @font.bld "STD"
$   else
$      @font.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create font.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack font.com -
	-s font.f -
	-i font.imake -
	-p font.pdf -
	-t tstfont.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create font.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C  REVISION HISTORY
C     2-95  VRU  ... CRI ... MSTP S/W CONVERSION (VICAR PORTING)
C     3-95  rea  make "loc" parameter work; permit blanks within text strings
      PROGRAM font
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      PARAMETER (NP=30)      
      PARAMETER (MAX_BYTES=524 288)	! 0.5 MBytes
      BYTE IMAGE(MAX_BYTES)
      CHARACTER*100 TEXT(2000)
      BYTE TEXT1(100)
      INTEGER FONT(NP),TALL(NP),DN(NP),SIZE(4),START(60),THICK(NP),
     &        STATUS,TallCnt,WideCnt,LocCnt,RotCnt,DNCnt,FontCnt,
     &        LOC(NP),INU,OUTU,NC(1000),DEF,COUNT,SL,SS,
     &        ThickCnt,SCOUNT,TCOUNT,FAT,NL,NS,
     &        NLCHUNK,SLCHUNK
      REAL WIDE(NP), ROTATE(NP)
c....flag to determine if a text string has been written, in part or
c    whole, off the image.
      LOGICAL FLAG
      INTEGER F2HBUF(12)
      INTEGER*2 FONT1, DN1, TALL1, NL1, NS1, ISS1, ISL1, NC1, LOC1
      LOGICAL TXTTEXT, TXTFONT, TXTSIZE, TXTCOLOR, STATUS1
C 
      CALL XVMESSAGE('FONT version 3-MAR-95',' ')
      call XVTRANS_SET(F2HBUF,'FULL','HALF',STATUS)
      CALL XVUNIT(INU,'INP',1,STATUS,' ')
      CALL XVUNIT(OUTU,'OUT',1,STATUS,' ')
C  
c...get the number of lines and samples of the input image from its internal
c   control block
      CALL XVOPEN(INU,STATUS,' ')
      CALL XVGET(INU,STATUS,'NL',NL,'NS',NS,' ')
 
      CALL XVPARM('SIZE',SIZE,COUNT,DEF,4)
      IF (DEF.EQ.1) THEN
         SIZE(3)=NL
         SIZE(4)=NS
         SIZE(1)=1
         SIZE(2)=1
      ENDIF

      CALL XVOPEN(OUTU,STATUS,'OP','WRITE',
     &     'U_NL',SIZE(3),'U_NS',SIZE(4),' ')
 
      CALL XVPARM('POSITION',START,SCOUNT,DEF,60)
      CALL XVPARM('TEXT',TEXT,TCOUNT,DEF,30)
      IF ( SCOUNT .NE. 2*TCOUNT ) THEN
         CALL XVMESSAGE( 'Number of TEXT strings does not match ',' ' )
         CALL XVMESSAGE( 'number of POSITIONS.',' ' )
         CALL ABEND
      END IF
 
      CALL XVPARM('TALL',TALL,TallCnt,DEF,NP)
      CALL XVPARM('WIDE',WIDE,WideCnt,DEF,NP)
      CALL XVPARM('LOC',LOC,LocCnt,DEF,NP)
      CALL XVPARM('ROTATE',ROTATE,RotCnt,DEF,NP)
      CALL XVPARM('DN',DN,DNCnt,DEF,NP)
      CALL XVPARM('FONT',FONT,FontCnt,DEF,NP)
      CALL XVPARM('THICK',THICK,ThickCnt,DEF,NP)

c...read the DESIRED PORTION OF THE input image into THE output image....

      DO I=1,SIZE(3)
         CALL XVREAD(INU,IMAGE(1),STATUS,'LINE',SIZE(1)+I-1,'SAMP',
     &        SIZE(2),'NSAMPS',SIZE(4),' ')
         CALL XVWRIT(OUTU,IMAGE(1),STATUS,'LINE',I,'SAMP',SIZE(2),
     &        'NSAMPS',SIZE(4),' ')
      ENDDO
      CALL XVCLOSE(INU,STATUS,' ')
      CALL XVCLOSE(OUTU,STATUS,' ')

c......both input and output data sets were closed above and the output
c      is now being re-opened for both reading and writing.     

      CALL XVOPEN(OUTU,STATUS,'OP','UPDATE',' ')

C	Divide the image into "chucks" where each chunk contains
C	NS samples and NLCHUNK lines.

      NLCHUNK = MAX_BYTES/SIZE(4)
      NLCHUNK = MIN(NLCHUNK,SIZE(3))

C	Now for each chunk....
C	  read it from the image
C	  write all strings into that chunk (sort of)
C	  write chunck to image

      DO SLCHUNK = 1,SIZE(3),NLCHUNK

C	   Calculate chunk boundary

	 NL = MIN(NLCHUNK, SIZE(3)-SLCHUNK+1)
	 NS = SIZE(4)
	 SS = 1
	 SL = SLCHUNK

C	   Read data from image file

	 DO J = 1,NL
            CALL XVREAD(OUTU,IMAGE((J-1)*NS+1),STATUS,
     &           'LINE',SL+J-1,'SAMP',SS,'NSAMPS',NS,' ')
	 END DO

C	   Draw each text string to image...
         DO I=1,TCOUNT
            DO K = 1, 100
              TEXT1(K) = 0
            END DO
            CALL MVCL(TEXT(I),TEXT1(1),100)
            NC(I) = 100
            DO K = 100,1,-1
              IF (TEXT1(K) .NE. 32) GO TO 99
              NC(I) = NC(I) - 1
            END DO
99        CONTINUE

c...Add missing parameters
            IF ( I .GT. TallCnt  ) TALL(I)   = TALL  (TallCnt)
            IF ( I .GT. WideCnt  ) WIDE(I)   = WIDE  (WideCnt)
            IF ( I .GT. LocCnt   ) LOC(I)    = LOC   (LocCnt)
            IF ( I .GT. RotCnt   ) ROTATE(I) = ROTATE(RotCnt)
            IF ( I .GT. DNCnt    ) DN(I)     = DN    (DNCnt)
            IF ( I .GT. FontCnt  ) FONT(I)   = FONT  (FontCnt)
            IF ( I .GT. ThickCnt ) THICK(I)  = THICK (ThickCnt)

C...SET STARTING COORDDINATE REFERENCE TO OUTPUT IMAGE..
            II = I*2-1
            JJ = II+1
            START(II)=START(II)-SIZE(1)+1
            START(JJ)=START(JJ)-SIZE(2)+1

C	      Skip if no characters in string

            IF(NC(I).EQ.0) GOTO 1

C	      Set up all parameters for this string

            CALL XVTRANS(F2HBUF,FONT(I),FONT1,1)
            STATUS1 = TXTFONT(FONT1)
            CALL XVTRANS(F2HBUF,TALL(I),TALL1,1)
            STATUS1 = TXTSIZE(TALL1,WIDE(I))
            CALL TXTROTATE(ROTATE(I))
            CALL XVTRANS(F2HBUF,DN(I),DN1,1)
            STATUS1 = TXTCOLOR(DN1)
 
c...the starting line and sample within the chunk are calculated
c   and the subroutine that writes the text is called after we
c   assign the thickness of each line in the characters, based on
c   the users request. Thickening is done be re-writing the string with
c   changing the iss and isl as needed.

            FAT=THICK(I)
            FLAG = .TRUE.
            DO J=1,FAT
               ISL=START(II)-SL+1-(J-1)
               DO K=1,FAT
                  ISS=START(JJ)-SS+K
                  CALL XVTRANS(F2HBUF,NL,NL1,1)
                  CALL XVTRANS(F2HBUF,NS,NS1,1)
                  CALL XVTRANS(F2HBUF,ISS,ISS1,1)
                  CALL XVTRANS(F2HBUF,ISL,ISL1,1)
                  CALL XVTRANS(F2HBUF,LOC(I),LOC1,1)
                  CALL XVTRANS(F2HBUF,NC(I),NC1,1)
              status1 = TXTTEXT(IMAGE,NL1,NS1,ISS1,ISL1,LOC1,NC1,
     &                 TEXT1(1),FLAG)
               ENDDO
            ENDDO

 1          CONTINUE
         END DO 
            ! Text drawing loop

c....data to now be placed in the output data set....but in order to
c    to write into the output in the update mode a XVREAD must be performed
c    on the data set to be written to before the XVWRIT can be.

         DO J=1,NL
            CALL XVWRIT(OUTU,IMAGE((J-1)*NS+1),STATUS,
     &           'LINE',SL+J-1,'SAMP',SS,'NSAMPS',NS,' ')
         ENDDO
      
      ENDDO          ! Chunking loop

      CALL XVCLOSE(OUTU,STATUS,' ')
c.........all format statements below except for # 1.
 2    FORMAT( ' PART OR ALL OF TEXT STRING',I3,
     &     ' WAS WRITTEN OFF THE IMAGE')
 4    FORMAT( ' TEXT STRING',I3,
     &     ' EXCEEDS PROGRAMS SIZE LIMITATIONS.')
 
 9999 RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create font.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM font

   To Create the build file give the command:

		$ vimake font			(VMS)
   or
		% vimake font			(Unix)


************************************************************************/


#define PROGRAM	font
#define R2LIB

#define MODULE_LIST font.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create font.pdf
PROCESS HELP=*
PARM INP    TYPE=STRING COUNT=1
PARM OUT    TYPE=STRING COUNT=1
PARM SIZE   TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM FONT   TYPE=INTEGER COUNT=(0:30) VALID=(1:131) DEFAULT=3
PARM TALL   TYPE=INTEGER COUNT=(0:30) DEFAULT=15
PARM WIDE   TYPE=REAL COUNT=(0:30) VALID=(.5:2.) DEFAULT=.8
PARM THICK  TYPE=INTEGER COUNT=(0:30) VALID=(1:100) DEFAULT=1
PARM POSITION  TYPE=INTEGER COUNT=(0:60) DEFAULT=(1,1)
PARM TEXT   TYPE=(STRING,100)  COUNT=(0:30) DEFAULT=" "
PARM ROTATE TYPE=REAL COUNT=(0:30) VALID=(-180.:180.) DEFAULT=0.
PARM DN     TYPE=INTEGER COUNT=(0:30) VALID=(0:255) DEFAULT=255
PARM LOC    TYPE=INTEGER COUNT=(0:30) VALID=(1,2,3) DEFAULT=1
PARM PARMS  TYPE=(STRING,40) COUNT=0:1 DEFAULT=--
END-PROC
.TITLE
VICAR2 program "font"--
       Writes text on an image allowing choices of FONT type, size and dn.
.HELP
PURPOSE:

"font" is a VICAR2 applications program which may be used to write text onto
any  VICAR2 byte image with a choice of seven available FONT styles.  Along
with  varying  the  FONT  style  the user can specify the height, width, dn
value  and  line  thickness  of each character and whether to left justify,
right  justify  or  center  the  text string above a specified point.  "font"
operates  in  a  completely  different  manner  than  "textad" and is no way
related.

.page 
EXECUTION STATEMENT:
		  font in out SIZE=(SL,SS,NL,NS) PARAMS
				    or
			    font in out PARMS

OPERATION:

"font" first  checks  the user entered parameters to insure that they are of
the  correct  format  and  number.   The  area  of  the input image that is
specified  by  the size field is copied from the input to the output image.
If  no  size field is specified, the entire input is copied to output.  The
input  image  is  closed and all subsequent work is performed on the output
image.   Due  to  the  fact that the various characters within a particular
FONT  have different sizes, it becomes extremely difficult to determine the
exact  dimensions  of  a text string.  (If exact text string dimensions and
character  positioning  are  required,  the user is encouraged to preform a
test  on  a  image  and  then  use IDISPLAY to measure the results.) If any
portion  of  a  text string is to written off the output image, the user is
notified  and  given  the  string number.  That portion of the string which
will  fit  in  the  output image is written and subsequent strings are then
processed.

EXAMPLES:
	font in out SIZE=(1,1,400,600) POSITION=(100,20,130,20) +
	     TEXT=("How goes it Laddy?","Just fine, Thanks!")
 
In  the  above  example,  both  text strings would be written in the output
image  with all six parameters being defaulted.  The results would have the
following  characteristics:  Both text strings, would be written in a Roman
style  FONT  (FONT=3),  the upper case letters are 15 pixels tall (tall=15)
and  12  pixels  wide (wide=.8,width is based on the height selected), both
upper  and lower case letters are allowed, the DN of the pixels in the text
is  255  (dn=255), the thickness of lines in the characters is 1 (thick=1),
and  the  starting  line  and  sample  coordinates given (POSITION(1,2) and
POSITION(3,4) indicated where the bottom left of the first character in the
text  strings TEXT(1,2) is to be written (loc=1).  The output data set will
be 400 lines by 600 samples.

      font in out SIZE=(100,100,300,500) POSITION=(300,20,350,20) +
	  TEXT=("How goes it Bobby?","None of your business.") +
			FONT=(11) TALL=(40)

The  example  above  illustrates  three  features  of program "font" that are
important to understand:

1) The size field determines the portion of the input data set that will be
copied  to  the  output  and consequently determines the size of the output
image.

2)  The  line  and sample coordinates given for text positioning (values in
POSITION(1,2)  POSITION(3,4))  are locations in the input image.  POSITIONs
are  always  related  to  the output image.  This means that the first text
string  TEXT(1) ("How goes it Bobby?") will start at line 200 and sample 20
in the output image.

3)  The  number  of values for the parameters TEXT, FONT, WIDE, THICK, etc.
need  not  match  exactly.   If  there are more TEXT strings than there are
values  for  FONT,  WIDE, THICK, etc., the last value given is used for all
remaining  strings.   Thus  in this example FONT=11 and TALL=40 is used for
both strings.

.page
TIMING:

WRITTEN BY: Bob Mortensen and Kevin Hussey

           (Parameter modification by Rich Walker AUG85)

COGNIZANT PROGRAMMER: nghia

REVISION:  1.0 -- Original Version
	   2.0 -- Added ROTATE parameter and removed many restrictions
           3.0 Made portable for UNIX ... V. Unruh ... (CRI) (Jan  2, 1995)
           3.1 fixed "loc" bug and space in text strings bug, added to help

.LEVEL1
.VARI INP
The data set into which the
text is to be scribed

Example: INP=raw.img
.VARI OUT
Output data set containing
image input plus text added.

Example: OUT=annotated.img
.VARI SIZE
Output data set dimensions.

Example: SIZE=(1,1,400,3360)
.VARI FONT
An integer code referring to
the nth (based on position)
text string letter type.

Example: FONT=(1,1,1,3,2,1,2)
.VARI TALL
The number of pixels tall the
characters of the nth text
are to be.

Example: 
TALL=(20,20,20,40,10,10,20)
.VARI WIDE
The number of pixels wide of
the characters in the nth
string will be wide(n)*tall(n).

Example: 
WIDE=(.8,.8,.5,.8,.9,.3,.6,.9)
.VARI THICK
The thickness in pixels of
the characters in the nth
text string

Example: THICK=(1,2,3,2,2,3,4,2)
.VARI POSITION
The sl  and ss  locations of the
nth text  string  given.   There
should be 2*n entries for this.
If LOC is 2, this indicates the
pixel position of the bottom
center of the string, otherwise
it is the lower left/right cor-
ner of the string, according to
the justification given (LOC).

Example: 
POSITION=(201,301,501,701, +
          101,101,1001,551)
.VARI TEXT
The text strings to be
written into the output
image.

Example:  
TEXT=("This is string 1", +
      "This is string 2")
.VARI ROTATE
The counterclockwise angle to 
rotate the text string using 
the given position as the 
center of rotation.
range = -180 to 180

Example:
ROTATE=(-90,0,180)
.VARI DN
The DN values of the pixels
of each of the strings to be
written.

Example: DN=(255,254,253)
.VARI LOC
This parameter indicates the
point in the text string to
be located at the 'position'
parameter value.  Use
 1 for lower left corner
 2 for center bottom, or
 3 for lower right corner.

Example:  LOC=(1,2,3,2,3)
.vari parms
This parameter is created in
order to handle more than one
datum passed to font by conlab

.LEVEL2 
.VARI FONT
The  following  values for  font 
point to the font-type indicated

1  = Simplex
2  = Duplex
3  = Roman
5  = Standard 2
7  = Script
11 = English Gothic
13 = Italian Gothic

.END
$ Return
$!#############################################################################
$Test_File:
$ create tstfont.pdf
procedure
refgbl $autousage
body
let $autousage="none"
refgbl $echo
!
!
write "This is a unit test for the program FONT."
write "FONT places text into a VICAR file."
write "The program is simple and is simply tested."
!
write " "
write "A file is generated with FONT and five strings"
write "are placed into the file with various parameters."
write " "
write "The tester is left to verify with the VIDS command,"
write "JSHOW-CURSOR, that the strings are correct."
write " "
write "The correct values are evident from the parameters"
write "to FONT in the test."
write " "
write "This test will use the JDISP command so the tester"
write "must allocate a device and invoke VIDS. "
!
let $echo="yes"
gen a 512 512 ival=128 sinc=0 linc=0
!
font a b font=(1,2,3,4,5) tall=(10,15,20,25,40) +
         wide=(1.,.8,.7,.5,2.) thick=(1,2,3,4,5) +
         position=(20,20 20,200 100,50, 300,100 400,140) +
         text=("one","two","three","four","five") +
         rotate=(0,0,10,80,0) +
         dn=(200,10,100,40,255)
!
let $echo="NO"
!
end-proc
$ Return
$!#############################################################################
