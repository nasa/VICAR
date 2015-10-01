$!****************************************************************************
$!
$! Build proc for MIPL module hstretch
$! VPACK Version 1.7, Tuesday, August 02, 1994, 15:01:02
$!
$! Execute by entering:		$ @hstretch
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
$ write sys$output "*** module hstretch ***"
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
$ write sys$output "Invalid argument given to hstretch.com file -- ", primary
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
$   if F$SEARCH("hstretch.imake") .nes. ""
$   then
$      vimake hstretch
$      purge hstretch.bld
$   else
$      if F$SEARCH("hstretch.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake hstretch
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @hstretch.bld "STD"
$   else
$      @hstretch.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create hstretch.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack hstretch.com -
	-s hstretch.f -
	-i hstretch.imake -
	-p hstretch.pdf -
	-t tsthstretch.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create hstretch.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C  5 SEP 1994  ...CRI... MSTP S/W CONVERSION (VICAR PORTING)

      INCLUDE 'VICMAIN_FOR'

      SUBROUTINE MAIN44
C  MILUS IMAGE ARITHMETIC ROUTINE  A. ZOBRIST
      IMPLICIT INTEGER(A-Z)
      CHARACTER*16 OUTFORMAT
      INTEGER*2 PIC1(100000), QTABLE(-32768:32767)
      INTEGER*4 TABLE(500), QTABAR(500)
C
      CALL IFMESSAGE('HSTRETCH version 5-SEP-94')
      CALL XVEACTION('SA',' ')

      CALL XVPARM('FORMAT',OUTFORMAT,NVALUE,VALUDF,1)
      CALL XVPARM('VALUE',VALUE,NVALUE,VALUDF,1)
      CALL XVPARM('BVALUE',BVAL,NBVAL,BVALDF,1)
      CALL XVPARM('TABLE',TABLE,NTABL,TABLDF,500)
      CALL XVPARM('QTABLE',QTABAR,NQTAB,QTABDF,500)

C Sanity Check: Must set either TABLE or QTABLE
      IF (QTABDF.EQ.1.AND.TABLDF.EQ.1) THEN
         CALL XVMESSAGE('Must set either TABLE or QTABLE',' ')
         CALL ABEND
         RETURN
      ENDIF

      IF (BVALDF.EQ.0) THEN     ! Initialize table to background
         DO 22 I=-32767,32768
 22      QTABLE(I-1) = BVAL
      ELSE                      ! Initialize table to identity
          DO 1 I=-32767,32768
 1        QTABLE(I-1) = I-1
      ENDIF

      IF(TABLDF.EQ.0) THEN      ! Use TABLE
         DO 33 I=1,NTABL,2
 33      QTABLE(TABLE(I)) = TABLE(I+1)
      ELSE                      ! Use foreground VALUE
         DO 2 I=1,NQTAB
 2       QTABLE(QTABAR(I)) = VALUE
      ENDIF
C
      CALL XVUNIT(RUNIT,'INP',1,STATUS,' ')
      CALL XVOPEN(RUNIT,STATUS,'U_FORMAT','HALF',' ')
      IF (STATUS .NE. 1) CALL XVSIGNAL(RUNIT,STATUS,1)
      IF (OUTFORMAT(1:4).EQ.'SAME') THEN
	CALL XVGET(RUNIT,STATUS,'FORMAT',OUTFORMAT,' ')
      ENDIF
      CALL XVSIZE(SL,SS,NL,NS,NLI,NSI)
      CALL XVUNIT(WUNIT,'OUT',1,STATUS,' ')
      CALL XVOPEN(WUNIT,STATUS,'U_FORMAT','HALF','U_NL',NL,
     +  'U_NS',NS, 'O_FORMAT',OUTFORMAT,'OP','WRITE',' ')
      IF (STATUS .NE. 1) CALL XVSIGNAL(WUNIT,STATUS,1)
      DO 100 I=1,NL
         CALL XVREAD(RUNIT,PIC1,STATUS,'SAMP',SS,
     +      'NSAMPS',NS,'LINE',SL+I-1,' ')
         IF (STATUS .NE. 1) CALL XVSIGNAL(RUNIT,STATUS,1)
         DO 9 J=1,NS
            K = PIC1(J)
            PIC1(J) = QTABLE(K)
   9     CONTINUE
         CALL XVWRIT(WUNIT,PIC1,STATUS,' ')
         IF (STATUS .NE. 1) CALL XVSIGNAL(WUNIT,STATUS,1)
 100  CONTINUE
      CALL XVCLOSE(RUNIT,STATUS,' ')
      CALL XVCLOSE(WUNIT,STATUS,' ')
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create hstretch.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM hstretch

   To Create the build file give the command:

		$ vimake hstretch			(VMS)
   or
		% vimake hstretch			(Unix)


************************************************************************/


#define PROGRAM	hstretch
#define R2LIB

#define MODULE_LIST hstretch.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create hstretch.pdf
PROCESS      HELP=*
! "hstretch" PDF - VICAR/IBIS MOSAIC SOFTWARE
! VICAR2/MIPL VERSION
PARM INP TYPE=(STRING,72)
PARM OUT TYPE=(STRING,72)
PARM SIZE TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SL      TYPE=INTEGER  COUNT=1       DEFAULT=1
PARM SS      TYPE=INTEGER  COUNT=1       DEFAULT=0
PARM NL      TYPE=INTEGER  COUNT=1       DEFAULT=0
PARM NS      TYPE=INTEGER  COUNT=1       DEFAULT=0
PARM TABLE TYPE=INTEGER COUNT=(0:100) DEFAULT=0
PARM QTABLE TYPE=INTEGER  COUNT=(0:100) DEFAULT=0
PARM VALUE TYPE=INTEGER DEFAULT=0
PARM BVALUE TYPE=INTEGER DEFAULT=0
PARM FORMAT TYPE=KEYWORD VALID=(BYTE,HALF,SAME) DEFAULT=SAME
END-PROC
.TITLE
VICAR Program  "hstretch"
.HELP
PURPOSE

     "hstretch"  is a VICAR applications program used for  the 
     production  of binary masks and for modifying  specific 
     DN  values  of  images.   Operation of the  program  is 
     similar to the table stretch option of STRETCH, however 
     the parameter structure is often simplified.  The major 
     feature of "hstretch" is that only those DN values  which 
     are to be modified need to be listed as parameters.
.PAGE
TAE COMMAND LINE FORMAT

     hstretch INP=A OUT=B SIZE PARAMS

     where


     INP                 is an input file.

     OUT                 is an output file.

     SIZE                is a standard VICAR size field.

     PARAMS              is a standard VICAR parameter field.
.PAGE
OPERATION          

     A  stretch  table  is  set  up  based  upon  parameters 
     specified  via  the QTABLE or TABLE  keyword.   All  DN 
     values  not  specified either remain  unchanged  (TABLE 
     option)  or are stretched to the background DN  (QTABLE 
     option).

EXAMPLES

     hstretch INP=A OUT=B HALF VALUE=100 BVALUE=0
              QTABLE=(32      1      2067,+
                      96      4        39)

     In this example, a halfword dataset is input to yield a 
     halfword format binary mask.   The primary value of the 
     mask  will be 100 DN while the background value will be 
     0   DN.    Only   the  values   listed   after   QTABLE 
     (32,1,2067,96,4,39)  will be stretched to 100 DN.   All 
     other DN values of the input image will be stretched to 
     0 DN.

     hstretch INP=A INP=B
          TABLE=(24   1    38    0    117    1     25     3    
                 36   2    50    3     51    4     52     5)

     In this example both input and output datasets are byte 
     format  and the table transfer mode has been specified.  
     Only the following values will be stretched,  all other 
     will remain unchanged.

            OLD DN          NEW DN

               24              1
               38              0
              117              1
                .              .
                .              .
                .              .
               51              4
               52              5

     When using the table mode all values to be changed must 
     be specified.  Consequently,  if the primary purpose of 
     the  stretch is to enhannce the contrast of  an  image, 
     VICAR  program "stretch" will be easier to use,  as  only 
     the "posts" of the stretch must be specified.  However, 
     when  the  purpose  of  the stretch is  to  compress  a 
     dataset such as a land cover classification, or if only 
     a  few DN values are to be changed while others are  to 
     be unchanged, "hstretch" may be more useful.

RESTRICTIONS:
     Both the QTABLE and TABLE parameters 
     may  not appear in  the  same  parameter 
     dataset.

WRITTEN BY:            A. L. Zobrist            1 May 1978
COGNIZANT PROGRAMMER:  N. D. Ritter
REVISION:              4                       22 Jun 1994

REVISION SUMMARY:
     Made portable for UNIX  AMS (CRI)  5 Sep 1994
     Added Test Proc         NDR       22 Jun 1994

.LEVEL1
.VARIABLE INP
Input image
.VARIABLE OUT
Output image
.VARIABLE SIZE
Image Size
.VARIABLE SL
Starting Line
.VARIABLE SS
Starting Sample
.VARIABLE NL
Number of Lines
.VARIABLE NS
Number of Samples
.VARIABLE VALUE
Primary DN of binary mask
.VARIABLE BVALUE
Background DN of binary mask
.VARIABLE QTABLE
Table to be stretched to VALUE
.VARIABLE TABLE
Conventional stretch table
.LEVEL2
.VARIABLE INP
          INP=A          input image.
.VARIABLE OUT
          OUT=B          output image.
                         files are halfword.
.VARIABLE SIZE
          Image Size
.VARIABLE SL
Starting Line
.VARIABLE SS
Starting Sample
.VARIABLE NL
Number of Lines
.VARIABLE NS
Number of Samples
.VARIABLE VALUE
          VALUE=I        The DN value "I" is used to specify 
                         the  primary DN value of the binary 
                         mask.   All  values listed  in  the 
                         QTABLE  list  will be stretched  to 
                         "I" DN.
.VARIABLE BVALUE
          BVALUE=J       The Dn value "J" is used to specify 
                         the  background  Dn  value  of  the 
                         binary mask.  All values not listed 
                         in   the   QTABLE  list   will   be 
                         stretched to "J" DN if BVALUE is
                         specified; otherwise the values are
                         not changed.
.VARIABLE QTABLE
          QTABLE=        The  QTABLE  keyword  is  used   to 
         (N1,N2,         denote  a list of DN values "N1  to 
          N3,...         Nn"  which  are to be stretched  to 
          Nn)            the  primary  DN  value  "I"   (see 
                         VALUE).   The sequence of DN values 
                         is  not restricted to any numerical 
                         order.   All values not listed with 
                         QTABLE  will  be stretched  to  the 
                         background BVALUE if specified, or
                         unchanged if BVALUE is defaulted.
.VARIABLE TABLE
          TABLE=         The TABLE keyword is used to denote 
         (01,N1,         all  input values "01 to 0n" to  be 
          02,N2          stretched  to corresponding  output 
          ...            values "N1 to Nn".  Data is entered 
          01,N1          as pairs 01,N1 where 01 denotes  an 
          ...            old  DN value to be stretched to  a 
          On,Nn)         new  DN value,  N1.   ONLY THOSE DN 
                         VALUES  LISTED WILL  BE  STRETCHED, 
                         AND  ALL  VALUES  NOT  LISTED  WILL 
                         REMAIN  UNCHANGED.   The  order  of 
                         stretch   pairs   01,N1   are   not 
                         restricted    to    any    specific 
                         numerical order.
.end

$ Return
$!#############################################################################
$Test_File:
$ create tsthstretch.pdf
procedure
refgbl $autousage
refgbl $echo
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
!
!  THIS IS A TEST OF PROGRAM HSTRETCH
!
!      first work on byte images.
!
gen HSTRA NL=10 NS=10 IVAL=0 
hstretch INP=HSTRA OUT=HSTRAO SIZE=(1,1,10,10) QTABLE=(2 12 17)
list HSTRAO 'ZEROES
!
!     repeat but swap background and foreground using VALUE and BVALUE.
!
hstretch INP=HSTRA OUT=HSTRAO1 QTABLE=(2 12 17) VALUE=0 BVALUE=1
list HSTRAO1 'ZEROES
!
!    try SL and SS not equal to 1.
!
hstretch INP=HSTRA OUT=HSTRAO2 SIZE=(2,3,8,7) 'BYTE TABLE=(4 10,  10 4)
list HSTRAO2
!
!      now work on halfword images.
!
gen HSTRB NL=10 NS=10 IVAL=0 'HALF
hstretch INP=HSTRB OUT=HSTRBO SIZE=(1,1,10,10) QTABLE=(2 12 17)
list HSTRBO 'ZEROES
!
!     repeat but swap background and foreground using VALUE and BVALUE.
!
hstretch INP=HSTRB OUT=HSTRBO1 QTABLE=(2 12 17) VALUE=0 BVALUE=1
list HSTRBO1 'ZEROES
!
!    try SL and SS not equal to 1.
!
hstretch INP=HSTRB OUT=HSTRBO2 SIZE=(2,3,8,7) 'HALF TABLE=(4 10,  10 4)
list HSTRBO2
!
!      now try byte input and halfword output. 
!
hstretch INP=HSTRA OUT=HSTRCO SIZE=(1,1,10,10) QTABLE=(2 12 17) 'HALF
list HSTRCO 'ZEROES
!
!     repeat but swap background and foreground using VALUE and BVALUE.
!
hstretch INP=HSTRA OUT=HSTRCO1 QTABLE=(2 12 17) VALUE=0 BVALUE=1 'HALF
list HSTRCO1 'ZEROES
!
!    try SL and SS not equal to 1.
!
hstretch INP=HSTRA OUT=HSTRCO2 SIZE=(2,3,8,7) TABLE=(4 10,  10 4) 'HALF
list HSTRCO2
!
!      now try halfword input and byte output
!
hstretch INP=HSTRB OUT=HSTRDO SIZE=(1,1,10,10) QTABLE=(2 12 17) 'BYTE
list HSTRDO 'ZEROES
!
!     repeat but swap background and foreground using VALUE and BVALUE.
!
hstretch INP=HSTRB OUT=HSTRDO1 QTABLE=(2 12 17) VALUE=0 BVALUE=1 'BYTE
list HSTRDO1 'ZEROES
!
!    try SL and SS not equal to 1.
!
hstretch INP=HSTRB OUT=HSTRDO2 SIZE=(2,3,8,7) TABLE=(4 10,  10 4) 'BYTE
list HSTRDO2
!
!    try hstretch on a bigger halfword image with negative DNs.
!
gen HSTRBIG NL=15 NS=1000 'HALF IVAL=-500
hstretch HSTRBIG HSTRBIGO QTABLE=(-490 0 490)
list HSTRBIGO
!
end-proc
$ Return
$!#############################################################################
