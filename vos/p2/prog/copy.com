$!****************************************************************************
$!
$! Build proc for MIPL module copy
$! VPACK Version 1.9, Monday, December 07, 2009, 16:04:18
$!
$! Execute by entering:		$ @copy
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
$ write sys$output "*** module copy ***"
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
$ write sys$output "Invalid argument given to copy.com file -- ", primary
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
$   if F$SEARCH("copy.imake") .nes. ""
$   then
$      vimake copy
$      purge copy.bld
$   else
$      if F$SEARCH("copy.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake copy
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @copy.bld "STD"
$   else
$      @copy.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create copy.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack copy.com -mixed -
	-s copy.f -
	-i copy.imake -
	-p copy.pdf -
	-t tstcopy.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create copy.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
CCCC      IMPLICIT NONE
C
C  6FEB89 -FFM - WRITE ERROR MSG IF SL OR SS GREATER THAN NL/NS. TRUNCATES
C		IF OUTPUT > INPUT
C 17MAY89 -RGD- Allow records up to 200,00 bytes, and print error message 
C		if record is too big
C 15JAN90 -MAG- Added support for binary label copy
C 10SEP91 -RGD- Ported to Unix
C 25NOV91 -RGD- Application ported to Unix
C 29JUN93 -RGD- FIXED PROBLEM WITH NL AND NS WHEN ONE OF THEM EXCEEDED
C	        SIZE OF FILE
C 12JUL93 -LWK- FIXED PROBLEM WHEN 'BINARY AND WINDOW SPECIFIED
C
      CHARACTER*7 FORMAT
      CHARACTER*3 ORG
      INTEGER SL,SS,SB,NL,NS,NB,NLI,NSI,NBI,COUNT,DEF,INPUT,OUTPUT,STAT
      COMMON /INSZ/ NL,NS,NB
      INTEGER BUFFER_SIZE
      PARAMETER (BUFFER_SIZE=200000)
      INTEGER EB,BAND,EL,LINE,ES,SAMP,BUF(BUFFER_SIZE)
      LOGICAL XVPTST
      INTEGER PIXSIZE
      LOGICAL BINARY
      INTEGER NLBI, NSBI
      INTEGER REC, NRECS, DUMMY

      CALL XVMESSAGE(' COPY VERSION 12-JUL-1993',' ')

      NLBI = 0
      NSBI = 0

      BINARY = XVPTST('BINARY')

      CALL XVUNIT(INPUT,'INP',1,STAT, ' ')
      CALL XVUNIT(OUTPUT,'OUT',1,STAT, ' ')
      IF ( BINARY ) THEN 
           CALL XVOPEN(INPUT,STAT,'OPEN_ACT',' ','IO_ACT','SA',
     *                          'COND','BINARY', ' ')
      ELSE 
           CALL XVOPEN(INPUT,STAT,'OPEN_ACT',' ','IO_ACT','SA', ' ')
      ENDIF

      IF (STAT .NE. 1) THEN
C
C	NO LABELS
C
	  CALL XVPARM('FORMAT',FORMAT,COUNT,DEF,0)
	  IF (DEF .NE. 0) THEN
	      CALL XVADD(OUTPUT,STAT,'U_FORMAT',FORMAT,
     2                   'O_FORMAT',FORMAT, ' ')
	      CALL XVADD(INPUT,STAT,'U_FORMAT',FORMAT,
     2        		 'I_FORMAT',FORMAT, ' ')
	  ENDIF

	  CALL XVPARM('INSIZE',NL,COUNT,DEF,0)
	  IF (DEF .NE. 0) CALL XVADD(INPUT,STAT,'U_NL',NL,
     2                              'U_NS',NS,'U_NB',NB, ' ')

	  CALL XVPARM('ORG',ORG,COUNT,DEF,0)
	  IF (DEF .NE. 0) CALL XVADD(INPUT,STAT,'U_ORG',ORG, ' ')

	  CALL XVOPEN(INPUT,STAT,'OPEN_ACT','SA','IO_ACT','SA',
     2                    'COND','NOLABELS', ' ')
	  CALL XVADD(OUTPUT,STAT,'COND','NOLABELS', ' ')
      ENDIF

      IF (XVPTST('NOBLOCK'))
     2    CALL XVADD(OUTPUT,STAT,'COND','NOBLOCK', ' ')

      CALL XVGET(INPUT,STAT,'ORG',ORG, ' ')
      CALL XVSIZE(SL,SS,NL,NS,DUMMY,DUMMY)
      CALL XVBANDS(SB,NB,DUMMY)
      CALL XVGET(INPUT,STAT,'NL',NLI,'NS',NSI,'NB',NBI,
     *           'PIX_SIZE',PIXSIZE, ' ')
      CALL XVGET(INPUT,STAT,'NLB',NLBI,'NBB',NSBI, ' ')
      IF (SL.GT.NLI .OR. SS.GT.NSI) THEN
          CALL XVMESSAGE('SL/SS cannot exceed NL/NS of input image',' ')
          CALL ABEND()
      ENDIF
      IF ((SL+NL-1).GT.NLI .OR. (SS+NS-1).GT.NSI .OR. 
     2(SB+NB-1).GT.NBI) THEN
          CALL XVMESSAGE(
     2        'Copy window too large, truncating output image...', ' ')
      ENDIF
      IF ((SL+NL-1) .GT. NLI) THEN
          NL = NLI-SL+1
      ENDIF
      IF ((SS+NS-1) .GT. NSI) THEN
          NS = NSI-SS+1
      ENDIF
      IF ((SB+NB-1) .GT. NBI) THEN
          NB = NBI-SB+1
      ENDIF

      IF ( (( SL .NE. 1 ) .OR. ( SS .NE. 1 ) .OR. ( SB .NE. 1 ) .OR.
     *	   ( NL .NE. NLI) .OR. ( NS .NE. NSI) .OR. ( NB .NE. NBI ))
     *     .AND. BINARY ) THEN

         NLBI = 0
         NSBI = 0
         BINARY = .FALSE.
         CALL XVCLOSE(INPUT,STAT, ' ')
         CALL XVOPEN(INPUT,STAT,'OPEN_ACT',' ','IO_ACT','SA', ' ')

C        CAN NOT COPY BINARY PARTS UNAMBIGUOUSLY IF OUTPUT IS A
C        SUB-WINDOW OF INPUT, SO CANCEL ANY BINARY PART COPY.

         CALL XVMESSAGE('Output is a sub-window of input, so', ' ')
         CALL XVMESSAGE('binary parts can not be copied.', ' ')
         CALL XVMESSAGE('Continuing with copy of non-binary parts.',' ')

      ENDIF

      IF ((ORG .NE. 'BIP' .AND. NS*PIXSIZE .GT. BUFFER_SIZE) .OR.
     *    (ORG .EQ. 'BIP' .AND. NB*PIXSIZE .GT. BUFFER_SIZE)) THEN
          CALL XVMESSAGE('Record is too big for internal buffer', ' ')
          CALL XVMESSAGE('Please notify cognizant programmer for COPY',
     *                   ' ')
          CALL ABEND
      END IF

      IF ( BINARY ) THEN
           CALL XVOPEN(OUTPUT,STAT,'OP','WRITE',
     2		       'U_NL',NL,'U_NS',NS,'U_NB',NB,
     3		       'U_ORG',ORG,'OPEN_ACT','SA','IO_ACT','SA',
     4                 'COND','BINARY',
     5		       'U_NBB',NSBI,'U_NLB',NLBI, ' ')
      ELSE 
           CALL XVOPEN(OUTPUT,STAT,'OP','WRITE',
     2		      'U_NL',NL,'U_NS',NS,'U_NB',NB,
     3		      'U_ORG',ORG,'OPEN_ACT','SA','IO_ACT','SA', ' ')
      ENDIF

C	IF BINARY COPY REQUESTED THEN COPY THE BINARY
C	HEADERS AND DATA IN A SPECIAL SIMPLE COPY LOOP.
C	THIS WORKS BECAUSE WE HAVE ALREADY REJECTED A
C	BINARY COPY WHEN WINDOWING IS SELECTED SO THAT
C	WE DON'T HAVE TO WORRY ABOUT KEEPING TRACK OF
C	LINES, BANDS, AND SAMPLES AS IS DONE BELOW.
C
      IF ( BINARY ) THEN
      
        IF ( ORG .EQ. 'BSQ' ) NRECS = NLBI + NL*NB
        IF ( ORG .EQ. 'BIL' ) NRECS = NLBI + NL*NB
        IF ( ORG .EQ. 'BIP' ) NRECS = NLBI + NL*NS

        DO REC = 1,NRECS
            CALL XVREAD(INPUT,BUF,STAT, ' ')
            CALL XVWRIT(OUTPUT,BUF,STAT, ' ')
         ENDDO 

        CALL XVCLOSE(INPUT,STAT, ' ')
        CALL XVCLOSE(OUTPUT,STAT, ' ')
        RETURN

      ENDIF

      IF (ORG .EQ. 'BSQ') THEN
	  EB = SB+NB-1
	  EL = SL+NL-1
	  DO BAND=SB,EB
	      DO LINE=SL,EL
	  	  CALL XVREAD(INPUT,BUF,STAT,'SAMP',SS,'NSAMPS',NS,
     2			      'LINE',LINE,'BAND',BAND, ' ')
		  CALL XVWRIT(OUTPUT,BUF,STAT, ' ')
	      ENDDO
	  ENDDO
      ELSE IF (ORG .EQ. 'BIL') THEN
	  EL = SL+NL-1
	  EB = SB+NB-1
	  DO LINE=SL,EL
	      DO BAND=SB,EB
		  CALL XVREAD(INPUT,BUF,STAT,'SAMP',SS,'NSAMPS',NS,
     2			      'BAND',BAND,'LINE',LINE, ' ')
		  CALL XVWRIT(OUTPUT,BUF,STAT, ' ')
	      ENDDO
	  ENDDO
      ELSE
C					ORG .EQ. 'BIP'
	  EL = SL+NL-1
	  ES = SS+NS-1
	  DO LINE=SL,EL
	      DO SAMP=SS,ES
	 	  CALL XVREAD(INPUT,BUF,STAT,'BAND',SB,'NBANDS',NB,
     2			      'SAMP',SAMP,'LINE',LINE, ' ')
	    	  CALL XVWRIT(OUTPUT,BUF,STAT, ' ')
	      ENDDO
	  ENDDO
      ENDIF

      CALL XVCLOSE(INPUT,STAT, ' ')
      CALL XVCLOSE(OUTPUT,STAT, ' ')
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create copy.imake
#define PROGRAM copy

#define MODULE_LIST copy.f

#define MAIN_LANG_FORTRAN
#define R2LIB

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE

$ Return
$!#############################################################################
$PDF_File:
$ create copy.pdf
process help=*
  PARM INP  TYPE=STRING
  PARM OUT  TYPE=STRING
  PARM SIZE TYPE=INTEGER COUNT=0:4 DEFAULT=--
  PARM BANDS TYPE=INTEGER COUNT=0:2 DEFAULT=--
  PARM SL INTEGER DEFAULT=1
  PARM SS INTEGER DEFAULT=1
  PARM SB INTEGER DEFAULT=1
  PARM NL INTEGER DEFAULT=0
  PARM NS INTEGER DEFAULT=0
  PARM NB INTEGER DEFAULT=0
  PARM FORMAT KEYWORD VALID=(BYTE,HALF,FULL,REAL,DOUB,COMP) DEFAULT=BYTE
  PARM INSIZE INTEGER COUNT=0:3 DEFAULT=--
  PARM ORG KEYWORD VALID=(BSQ,BIL,BIP) DEFAULT=BSQ
  PARM BLOCK TYPE=KEYWORD VALID=(BLOCK,NOBLOCK) DEFAULT=BLOCK
  PARM BINARY TYPE=KEYWORD VALID=(BINARY,NOBINARY) DEFAULT=NOBINARY

!# annot function="VICAR Utilities"
!# annot keywords=("copy all",image,size, bands,INSIZE,format,org,+
!# NOBLOCK)
END-PROC
.title
Copies all or part of a labeled or unlabeled image
!# ANNOT ICON = copy
.help
Copy is a simple program which can be used to copy all or
part of an image to another file.  The size and bands fields
are used to determine a window into the input file.

Copy works both on labeled and unlabeled images, although for
an unlabeled image it is usually best to specify INSIZE,
FORMAT, and ORG to ensure that the proper size image is read.

To write an unblocked tape file, the NOBLOCK keyword may be given.

The maximum record size that may be copied is 200,000 bytes.
.level1
.vari inp
Input file name
.vari out
Output file name
.vari size
Window into input
.vari bands
Window into input
in band dimension
.vari sl
Starting line
= size(1)
.vari ss
Starting sample
= size(2)
.vari sb
Starting band
= bands(1)
.vari nl
Number of lines
= size(3)
.vari ns
Number of samples
= size(4)
.vari nb
Number of bands
= bands(2)
.vari insize
(nl,ns,nb) of input
if unlabeled
.VARI FORMAT
Data format iff input
is unlabeled
.vari org
File organization iff
input is unlabeled
.vari block
Block output?
.vari binary
Copy binary labels?
.level2
.vari inp
Name of a single input file.
.vari out
Name of a single output file.
.vari size
The size parameter determines the boundaries in the input
file from which the copy is to take place.  It is specified
as  (SL,SS,NL,NS), where
	SL is the starting line 
	SS is the starting sample
	NL is the number of lines to be copied
	NS is the number of samples (pixels) in each line
.vari bands
The bands parameter determines the bands in the input
file from which the copy is to take place.  It is specified
as (SB,NB), where
	SB is the starting band
	NB is the number of bands to be copied
.VARI INSIZE
The size of the input file if it has no label.  If the input file
has a label, then this parameter is ignored, and the actual file
size is used.
.vari format
The data format of the input file if it has no label.  If the input
file has a label, then this parameter is ignored, and the actual
file format is used.
.vari org
The file organization of the input file if it has no label.  If the
input file has a label, then this parameter is ignored, and the
actual file organization is used.
.vari block
If NOBLOCK is specified, then the image will not be blocked on
output, thus having one record per block.  This feature is useful
for writing tapes for foreign systems.
.vari binary
This keyword parameter may be used to tell COPY that binary labels
and prefixes must be copied along with image data.
.end
$ Return
$!#############################################################################
$Test_File:
$ create tstcopy.pdf
procedure
refgbl $echo
refgbl $autousage
body
let _onfail="continue"
let $autousage="none"
let $echo="yes"
putmsg "2D files compatibility check" ""
gen a 10 10
list a
label-list a
copy a b
list b
label-list b
copy a b (3,3,3,3)
list b
label-list b
copy a b (5,5,10,10)
list b
label-list b
putmsg "3D BSQ files" ""
gen a 10 10 nb=5 org=bsq
copy a b
list b
label-list b
copy a b (3,3,3,3) (2,3)
list b
label-list b
putmsg "3D BIL files" ""
gen a 10 10 nb=5 org=bil
copy a b
list b
label-list b
copy a b (3,3,3,3) sb=2 nb=3
list b
label-list b
putmsg "3D BIP files" ""
gen a 10 10 nb=5 org=bip
copy a b
list b
label-list b
copy a b (3,3,3,3) (2,3)
list b
label-list b
!
!
! Now test the mode of COPY which copies binary items.
!
! First GEN a 3-D image.
!
gen a nl=5 ns=4 nb=3
!
! list the contents and the label.
!
list a
label-l a 'dump
!
!
! Now change certain label items to make the
! file have binary headers and a binary prefix.
!
label-replace a items="nb=2 nlb=5 ns=3 nbb=1" 'system
!
! List contents and label to show the difference.
!
list a
label-l a 'dump
!
! Now copy the file to show that in default mode
! COPY will drop the binary items.
!
copy a b
list b
label-l b 'dump
!
! Now copy with BINARY set on the COPY and
! show that the binary information has been
! retained. The dump of the label will show
! that the binary items are correct and that
! the appropriate data areas agree with the
! non-binary copy indicating that the binary
! items are consistent and allow the listing
! of the correct data areas.
!
copy a b 'binary
list b
label-l b 'dump
!
! test that the fix-up when a window is specified works:
copy a b 'binary size=(2,2,2,2)
list b
!
! Binary mode has been checked.
!
end-proc
$ Return
$!#############################################################################
