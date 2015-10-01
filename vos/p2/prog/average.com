$!****************************************************************************
$!
$! Build proc for MIPL module average
$! VPACK Version 1.9, Friday, November 02, 2012, 11:31:08
$!
$! Execute by entering:		$ @average
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
$ write sys$output "*** module average ***"
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
$ write sys$output "Invalid argument given to average.com file -- ", primary
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
$   if F$SEARCH("average.imake") .nes. ""
$   then
$      vimake average
$      purge average.bld
$   else
$      if F$SEARCH("average.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake average
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @average.bld "STD"
$   else
$      @average.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create average.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack average.com -mixed -
	-s average.f -
	-i average.imake -
	-p average.pdf -
	-t tstaverage.pdf tstaverage.log_solos
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create average.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C      PROGRAM  AVERAGE
C#######################################################################
C  NAME OF ROUTINE
C      "AVERAGE"  ( AVERAGE images )
C  PURPOSE
C      THIS IS THE STANDARD MAIN PROGRAM USED FOR TAE/VICAR PROGRAMS.
C      THIS MODULE CALLS SUBROUTINE MAIN44 TO ENTER INTO THE BODY OF THE
C      PROGRAM.
C  PREPARED FOR USE ON MIPL SYSTEM BY
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION     JULY 1983
C  ENVIRONMENT
C      UNIX or VMS  with TAE/VICAR2 EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C     3-94  CRI  MSTP S/W CONVERSION (VICAR PORTING)
C  CALLING SEQUENCE (TAE COMMAND LINE)
C      The following command line formats show the major allowable forms:
C      average INP=(a...) OUT=b SIZE=(sl,ss,nl,ns) optional parameters
C      average INP=(a...) OUT=b SL=sl SS=ss NL=nl NS=ns optional parameters
C      average (a...) b (sl,ss,nl,ns) optional parameters
C      average (a...) b optional parameters
C
C       Here (a...) represents a list of 2 to 48 file names.
C       b represents an output image file name.
C
C  INPUT PARAMETERS (listed by keyword)
C      INP    - Input file names.
C      OUT    - Output file names.
C      SIZE   - Standard Vicar size field:  (SL,SS,NL,NS)
C               SL = Starting line number.
C               SS = Starting sample number.
C               NL = Number of lines.
C               NS = Number of samples.
C               The same SIZE parameters apply to each of the input
C               image files.
C  SUBROUTINES CALLED
C      MAIN44 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C#######################################################################
C  NAME OF ROUTINE
C      MAIN44   (name of top level subroutine by VICAR convention)
C      
C  CONVERTED FOR USE ON MIPL SYSTEM BY   
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION     JULY 1983
C  CALLING SEQUENCE
C      Standard subroutine call and return.
C  INPUT AND OUTPUT PARAMETERS     
C      SEE UNDER PROGRAM AVERAGE.
C      
C  CALLED BY
C      "average"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c      IMPLICIT INTEGER (A-Z)
	implicit none
      INCLUDE 'fortport'
      integer*4 temp
c      BYTE IBUF(120000),OBUF(20000)
      integer*4 idsn(48), is(48)
	integer*4 ind,ni,sl,ss,nl,ns,nli,nsi,el
	integer*4 i,ii,j,isp,outfile,icode
	real*8 dibuf(120000),dobuf(20000)
	character*4 fmt(4)/'BYTE','HALF','FULL','REAL'/
	character*5 format
	character*8 org

C
C
C
C=================START OF EXECUTABLE CODE===============================     
C
      call ifmessage ('AVERAGE  16-MAY-2011')
      call xveaction ('SA', ' ')
      call xvpcnt( 'INP', ni )

         call xvunit( idsn(1), 'INP', 1, ind, ' ' )
         call xvopen( idsn(1), ind, 'OP', 'READ', ' ' )
	call xvget(idsn(1),ind,'FORMAT',format,'ORG',org,' ')
         call xvsize( sl, ss, nl, ns, nli, nsi )   ! GET SIZE PARAMETER.

	icode = 0
	if (format.eq.'BYTE') icode=1
	if (format.eq.'HALF'.or.format.eq.'WORD') icode=2
	if (format.eq.'FULL') icode=3
	if (format.eq.'REAL') icode=4
	if (icode.eq.0) then
		call xvmessage('??E - Unknown data format for input image',' ')
		call abend  
	endif
	call xvclose(idsn(1),ind,' ')

C
C  OPEN DATA SETS
      DO I=1,NI
         call xvunit( idsn(i), 'INP', i, ind, ' ' )
         call xvopen( idsn(i), ind, 'OP', 'READ', 
     & 		'I_FORMAT',fmt(icode),'U_FORMAT','DOUB',' ' )
      END DO

         call xvunit( outfile, 'OUT', 1, ind, ' ' )
         call xvopen( outfile, ind, 'OP', 'WRITE',
     .         'O_FORMAT',fmt(icode),'U_FORMAT','DOUB', 
     .         'U_NL', nl, 'U_NS', ns, ' ' )
C
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCC  MAIN LOOP

      EL = SL+NL-1

      DO 491 II=SL,EL           ! LOOP OVER LINES.
              IF ( II .EQ. SL )   THEN
                 ISP = 1
                 DO I = 1, NI

                  call xvread( idsn(i), dibuf(isp), ind, 'LINE', 
     .                  SL, ' ' )
                  ISP = ISP + NSI
                 END DO
              ELSE
                 ISP = 1
                 DO I = 1, NI

                  call xvread( idsn(i), dibuf(isp), ind, ' ' )
                  ISP = ISP + NSI
                 END DO
              END IF
                 IS(1) = SS
                 DO I = 2,NI
                    IS(I) = IS(I-1) + NSI
                 END DO

                 DO  J = 1, NS              ! THE SAMPLE LOOP.
                     TEMP = 0
                     DO I = 1, NI
c                        TEMP = TEMP + BYTE2INT( IBUF(  IS(I) ) )
			 temp = temp + dibuf(  is(i) )
                        IS(I) = IS(I) + 1
                     END DO
                     TEMP = TEMP / NI              ! AVERAGE
c                     OBUF(J) = INT2BYTE(TEMP)      ! LOW ORDER BYTE.   
		     dobuf(J) = temp	
                 END DO

                 call xvwrit( outfile, dobuf, ind, ' ' )

491   CONTINUE

      RETURN          ! NORMAL END.
      END

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create average.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM average

   To Create the build file give the command:

		$ vimake average			(VMS)
   or
		% vimake average			(Unix)


************************************************************************/


#define PROGRAM	average
#define R2LIB

#define MODULE_LIST average.f

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
$ create average.pdf
process help=*
PARM INP TYPE=STRING COUNT=2:48
PARM OUT TYPE=STRING
PARM SIZE TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SL TYPE=INTEGER DEFAULT=1
PARM SS TYPE=INTEGER DEFAULT=1
PARM NL TYPE=INTEGER DEFAULT=0
PARM NS TYPE=INTEGER DEFAULT=0
!# parm inp(3-48) hints=default
END-PROC
.TITLE
AVERAGE
.HELP
PURPOSE:
AVERAGE takes up to 48 input images and averages them together to make an
output image

If the size parameter is entered then only that portion of the image
is averaged and the image produced is the size of the SIZE parameter.
That is, if you enter size=(10,10,50,50) then the output will be 
50 lines by 50 samples

Internally, the buffers are of DOUB format so that precision is preserved.

All inputs must be the same format. Multiband images not supported.

EXECUTION:

Example

AVERAGE INP=(A,B,C) OUT=D  will average images A, B, and C to form D.

If the size parameter is used (SIZE=(SL,SS,NL,NS)), only the defined area in
each input image will be used to create the new file.  

PROGRAM LIMITATIONS

     1. The input and output images can be BYTE, HALF, FULL or REAL data.
     2. Maximum number of samples is 20000 per line.  The sum of the number
        of samples per line for the input images must not exceed 120000.
     3. Multi-band images are not supported


WRITTEN BY:                  Steve Pohorsky                1 Jul 1983
COGNIZANT PROGRAMMER:        Ray Bambery    
REVISION:  1                                               5 Oct 1984

    3-94         CRI            MSTP S/W CONVERSION (VICAR PORTING)
    16-May-2011  Ray Bambery - Changed internals to DOUB format
                    Allow BYTE, HALF, FULL, or REAL data for input

.LEVEL1
.VARIABLE INP
STRING - Input image files
.VARIABLE OUT
STRING - Output image file
.VARIABLE SIZE
INTEGER - Region of input files
to be concatenated
.VARIABLE SL
INTEGER - Starting line
.VARIABLE SS
INTEGER - Starting sample
.VARIABLE NS
INTEGER - Number of lines
.VARIABLE NL
INTEGER - Number of samples
.LEVEL2
.VARIABLE INP
INP specifies the input data sets.  Up to 48 are allowed.
.VARIABLE SIZE
The SIZE parameter may be used when only a sub-region of each image is to
be concatenated; it has the format SIZE=(SL,SS,NL,NS), where the parameters
are starting line, starting sample, number of lines, and number of samples,
respectively.  SIZE=(1,1,10,10), for example, will cause AVERAGE to only look
at the first ten samples of each of the first ten lines in each image, when
performing the concatenation. 
.VARIABLE SL
INTEGER - Starting line (see SIZE)
.VARIABLE SS
INTEGER - Starting sample (see SIZE)
.VARIABLE NS
INTEGER - Number of lines (see SIZE)
.VARIABLE NL
INTEGER - Number of samples (see SIZE)
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstaverage.pdf
procedure
refgbl $echo
refgbl $autousage
! Jun 19, 2012 - RJB
! TEST SCRIPT FOR AVERAGE
! tests BYTE, HALF, FULL, REAL images
!
! Vicar Programs:
!       gen list label-list
! 
! parameters:
!   <none>
!
! Requires NO external test data: 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

body
let $autousage="none"
let _onfail="stop"
let $echo="yes"
!
!     BUILD 3 INPUT BYTE IMAGES 
!
gen LOOK1  NL=10 NS=10  IVAL=0
gen LOOK2  NL=10 NS=10  IVAL=5
gen LOOK3  NL=10 NS=10  IVAL=10
!
list LOOK1
list LOOK2
list LOOK3
!
average INP=(LOOK1, LOOK2, LOOK3) OUT=(LOOK11)
list LOOK11
average INP=(LOOK1, LOOK2, LOOK3) OUT=(LOOK12)  SIZE=(2,2,2,5)
list LOOK12

!
!     BUILD 3 INPUT HALF IMAGES 
!
gen half1   NL=10 NS=10  IVAL=100 format=half
gen half2   NL=10 NS=10  IVAL=500 format=half
gen half3   NL=10 NS=10  IVAL=1000 format=half

list half1
list half2
list half3

average INP=(half1, half2, half3) OUT=(half11)
list half11
average INP=(half1, half2, half3) OUT=(half12)  SIZE=(2,2,2,5)
list half12
label-li half12

!
!     BUILD 3 INPUT FULL IMAGES 
!
gen full1   NL=10 NS=10  IVAL=1000 format=full
gen full2   NL=10 NS=10  IVAL=5000 format=full
gen full3   NL=10 NS=10  IVAL=10000 format=full

average INP=(full1, full2, full3) OUT=(full11)
list full11

average INP=(full1, full2, full3) OUT=(full12)  SIZE=(2,2,2,5)
list full12
label-li full12

!
!     BUILD 3 INPUT REAL IMAGES 
!
gen real1   NL=10 NS=10  IVAL=1000 format=real
gen real2   NL=10 NS=10  IVAL=5000 format=real
gen real3   NL=10 NS=10  IVAL=10000 format=real

average INP=(real1, real2, real3) OUT=(real11)
list real11

average INP=(real1, real2, real3) OUT=(real12)  SIZE=(2,2,2,5)
list real12
label-li real12



!   CLEAN UP
! 
ush rm -f LOOK*
ush rm -f half*
ush rm -f full*
ush rm -f real*
!
let $echo="no"
end-proc
$!-----------------------------------------------------------------------------
$ create tstaverage.log_solos
tstaverage
gen LOOK1  NL=10 NS=10  IVAL=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen LOOK2  NL=10 NS=10  IVAL=5
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen LOOK3  NL=10 NS=10  IVAL=10
Beginning VICAR task gen
GEN Version 6
GEN task completed
list LOOK1
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Fri Nov  2 11:23:02 2012
     Samp     1       3       5       7       9
   Line
      1       0   1   2   3   4   5   6   7   8   9
      2       1   2   3   4   5   6   7   8   9  10
      3       2   3   4   5   6   7   8   9  10  11
      4       3   4   5   6   7   8   9  10  11  12
      5       4   5   6   7   8   9  10  11  12  13
      6       5   6   7   8   9  10  11  12  13  14
      7       6   7   8   9  10  11  12  13  14  15
      8       7   8   9  10  11  12  13  14  15  16
      9       8   9  10  11  12  13  14  15  16  17
     10       9  10  11  12  13  14  15  16  17  18
list LOOK2
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Fri Nov  2 11:23:02 2012
     Samp     1       3       5       7       9
   Line
      1       5   6   7   8   9  10  11  12  13  14
      2       6   7   8   9  10  11  12  13  14  15
      3       7   8   9  10  11  12  13  14  15  16
      4       8   9  10  11  12  13  14  15  16  17
      5       9  10  11  12  13  14  15  16  17  18
      6      10  11  12  13  14  15  16  17  18  19
      7      11  12  13  14  15  16  17  18  19  20
      8      12  13  14  15  16  17  18  19  20  21
      9      13  14  15  16  17  18  19  20  21  22
     10      14  15  16  17  18  19  20  21  22  23
list LOOK3
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Fri Nov  2 11:23:02 2012
     Samp     1       3       5       7       9
   Line
      1      10  11  12  13  14  15  16  17  18  19
      2      11  12  13  14  15  16  17  18  19  20
      3      12  13  14  15  16  17  18  19  20  21
      4      13  14  15  16  17  18  19  20  21  22
      5      14  15  16  17  18  19  20  21  22  23
      6      15  16  17  18  19  20  21  22  23  24
      7      16  17  18  19  20  21  22  23  24  25
      8      17  18  19  20  21  22  23  24  25  26
      9      18  19  20  21  22  23  24  25  26  27
     10      19  20  21  22  23  24  25  26  27  28
average INP=(LOOK1, LOOK2, LOOK3) OUT=(LOOK11)
Beginning VICAR task average
AVERAGE  16-MAY-2011
list LOOK11
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Fri Nov  2 11:23:02 2012
 Task:AVERAGE   User:lwk       Date_Time:Fri Nov  2 11:23:03 2012
     Samp     1       3       5       7       9
   Line
      1       5   6   7   8   9  10  11  12  13  14
      2       6   7   8   9  10  11  12  13  14  15
      3       7   8   9  10  11  12  13  14  15  16
      4       8   9  10  11  12  13  14  15  16  17
      5       9  10  11  12  13  14  15  16  17  18
      6      10  11  12  13  14  15  16  17  18  19
      7      11  12  13  14  15  16  17  18  19  20
      8      12  13  14  15  16  17  18  19  20  21
      9      13  14  15  16  17  18  19  20  21  22
     10      14  15  16  17  18  19  20  21  22  23
average INP=(LOOK1, LOOK2, LOOK3) OUT=(LOOK12)  SIZE=(2,2,2,5)
Beginning VICAR task average
AVERAGE  16-MAY-2011
list LOOK12
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Fri Nov  2 11:23:02 2012
 Task:AVERAGE   User:lwk       Date_Time:Fri Nov  2 11:23:03 2012
     Samp     1       3       5
   Line
      1       7   8   9  10  11
      2       8   9  10  11  12
gen half1   NL=10 NS=10  IVAL=100 format=half
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen half2   NL=10 NS=10  IVAL=500 format=half
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen half3   NL=10 NS=10  IVAL=1000 format=half
Beginning VICAR task gen
GEN Version 6
GEN task completed
list half1
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Fri Nov  2 11:23:03 2012
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1       100   101   102   103   104   105   106   107   108   109
      2       101   102   103   104   105   106   107   108   109   110
      3       102   103   104   105   106   107   108   109   110   111
      4       103   104   105   106   107   108   109   110   111   112
      5       104   105   106   107   108   109   110   111   112   113
      6       105   106   107   108   109   110   111   112   113   114
      7       106   107   108   109   110   111   112   113   114   115
      8       107   108   109   110   111   112   113   114   115   116
      9       108   109   110   111   112   113   114   115   116   117
     10       109   110   111   112   113   114   115   116   117   118
list half2
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Fri Nov  2 11:23:03 2012
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1       500   501   502   503   504   505   506   507   508   509
      2       501   502   503   504   505   506   507   508   509   510
      3       502   503   504   505   506   507   508   509   510   511
      4       503   504   505   506   507   508   509   510   511   512
      5       504   505   506   507   508   509   510   511   512   513
      6       505   506   507   508   509   510   511   512   513   514
      7       506   507   508   509   510   511   512   513   514   515
      8       507   508   509   510   511   512   513   514   515   516
      9       508   509   510   511   512   513   514   515   516   517
     10       509   510   511   512   513   514   515   516   517   518
list half3
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Fri Nov  2 11:23:04 2012
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1      1000  1001  1002  1003  1004  1005  1006  1007  1008  1009
      2      1001  1002  1003  1004  1005  1006  1007  1008  1009  1010
      3      1002  1003  1004  1005  1006  1007  1008  1009  1010  1011
      4      1003  1004  1005  1006  1007  1008  1009  1010  1011  1012
      5      1004  1005  1006  1007  1008  1009  1010  1011  1012  1013
      6      1005  1006  1007  1008  1009  1010  1011  1012  1013  1014
      7      1006  1007  1008  1009  1010  1011  1012  1013  1014  1015
      8      1007  1008  1009  1010  1011  1012  1013  1014  1015  1016
      9      1008  1009  1010  1011  1012  1013  1014  1015  1016  1017
     10      1009  1010  1011  1012  1013  1014  1015  1016  1017  1018
average INP=(half1, half2, half3) OUT=(half11)
Beginning VICAR task average
AVERAGE  16-MAY-2011
list half11
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Fri Nov  2 11:23:03 2012
 Task:AVERAGE   User:lwk       Date_Time:Fri Nov  2 11:23:04 2012
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1       533   534   535   536   537   538   539   540   541   542
      2       534   535   536   537   538   539   540   541   542   543
      3       535   536   537   538   539   540   541   542   543   544
      4       536   537   538   539   540   541   542   543   544   545
      5       537   538   539   540   541   542   543   544   545   546
      6       538   539   540   541   542   543   544   545   546   547
      7       539   540   541   542   543   544   545   546   547   548
      8       540   541   542   543   544   545   546   547   548   549
      9       541   542   543   544   545   546   547   548   549   550
     10       542   543   544   545   546   547   548   549   550   551
average INP=(half1, half2, half3) OUT=(half12)  SIZE=(2,2,2,5)
Beginning VICAR task average
AVERAGE  16-MAY-2011
list half12
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Fri Nov  2 11:23:03 2012
 Task:AVERAGE   User:lwk       Date_Time:Fri Nov  2 11:23:05 2012
     Samp       1     2     3     4     5
   Line
      1       535   536   537   538   539
      2       536   537   538   539   540
label-li half12
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File half12 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in HALF format from a SUN-SOLR host
                1 bands
                2 lines per band
                5 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: lwk -- Fri Nov  2 11:23:03 2012 ----
IVAL=100.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: AVERAGE -- User: lwk -- Fri Nov  2 11:23:05 2012 ----
 
************************************************************
gen full1   NL=10 NS=10  IVAL=1000 format=full
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen full2   NL=10 NS=10  IVAL=5000 format=full
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen full3   NL=10 NS=10  IVAL=10000 format=full
Beginning VICAR task gen
GEN Version 6
GEN task completed
average INP=(full1, full2, full3) OUT=(full11)
Beginning VICAR task average
AVERAGE  16-MAY-2011
list full11
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Fri Nov  2 11:23:05 2012
 Task:AVERAGE   User:lwk       Date_Time:Fri Nov  2 11:23:06 2012
     Samp            1          2          3          4          5          6          7          8          9         10
   Line
      1           5333       5334       5335       5336       5337       5338       5339       5340       5341       5342
      2           5334       5335       5336       5337       5338       5339       5340       5341       5342       5343
      3           5335       5336       5337       5338       5339       5340       5341       5342       5343       5344
      4           5336       5337       5338       5339       5340       5341       5342       5343       5344       5345
      5           5337       5338       5339       5340       5341       5342       5343       5344       5345       5346
      6           5338       5339       5340       5341       5342       5343       5344       5345       5346       5347
      7           5339       5340       5341       5342       5343       5344       5345       5346       5347       5348
      8           5340       5341       5342       5343       5344       5345       5346       5347       5348       5349
      9           5341       5342       5343       5344       5345       5346       5347       5348       5349       5350
     10           5342       5343       5344       5345       5346       5347       5348       5349       5350       5351
average INP=(full1, full2, full3) OUT=(full12)  SIZE=(2,2,2,5)
Beginning VICAR task average
AVERAGE  16-MAY-2011
list full12
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Fri Nov  2 11:23:05 2012
 Task:AVERAGE   User:lwk       Date_Time:Fri Nov  2 11:23:07 2012
     Samp            1          2          3          4          5
   Line
      1           5335       5336       5337       5338       5339
      2           5336       5337       5338       5339       5340
label-li full12
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File full12 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in FULL format from a SUN-SOLR host
                1 bands
                2 lines per band
                5 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: lwk -- Fri Nov  2 11:23:05 2012 ----
IVAL=1000.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: AVERAGE -- User: lwk -- Fri Nov  2 11:23:07 2012 ----
 
************************************************************
gen real1   NL=10 NS=10  IVAL=1000 format=real
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen real2   NL=10 NS=10  IVAL=5000 format=real
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen real3   NL=10 NS=10  IVAL=10000 format=real
Beginning VICAR task gen
GEN Version 6
GEN task completed
average INP=(real1, real2, real3) OUT=(real11)
Beginning VICAR task average
AVERAGE  16-MAY-2011
list real11
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Fri Nov  2 11:23:07 2012
 Task:AVERAGE   User:lwk       Date_Time:Fri Nov  2 11:23:07 2012
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       5.333E+03   5.334E+03   5.335E+03   5.336E+03   5.337E+03   5.338E+03   5.339E+03   5.340E+03   5.341E+03   5.342E+03
      2       5.334E+03   5.335E+03   5.336E+03   5.337E+03   5.338E+03   5.339E+03   5.340E+03   5.341E+03   5.342E+03   5.343E+03
      3       5.335E+03   5.336E+03   5.337E+03   5.338E+03   5.339E+03   5.340E+03   5.341E+03   5.342E+03   5.343E+03   5.344E+03
      4       5.336E+03   5.337E+03   5.338E+03   5.339E+03   5.340E+03   5.341E+03   5.342E+03   5.343E+03   5.344E+03   5.345E+03
      5       5.337E+03   5.338E+03   5.339E+03   5.340E+03   5.341E+03   5.342E+03   5.343E+03   5.344E+03   5.345E+03   5.346E+03
      6       5.338E+03   5.339E+03   5.340E+03   5.341E+03   5.342E+03   5.343E+03   5.344E+03   5.345E+03   5.346E+03   5.347E+03
      7       5.339E+03   5.340E+03   5.341E+03   5.342E+03   5.343E+03   5.344E+03   5.345E+03   5.346E+03   5.347E+03   5.348E+03
      8       5.340E+03   5.341E+03   5.342E+03   5.343E+03   5.344E+03   5.345E+03   5.346E+03   5.347E+03   5.348E+03   5.349E+03
      9       5.341E+03   5.342E+03   5.343E+03   5.344E+03   5.345E+03   5.346E+03   5.347E+03   5.348E+03   5.349E+03   5.350E+03
     10       5.342E+03   5.343E+03   5.344E+03   5.345E+03   5.346E+03   5.347E+03   5.348E+03   5.349E+03   5.350E+03   5.351E+03
average INP=(real1, real2, real3) OUT=(real12)  SIZE=(2,2,2,5)
Beginning VICAR task average
AVERAGE  16-MAY-2011
list real12
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Fri Nov  2 11:23:07 2012
 Task:AVERAGE   User:lwk       Date_Time:Fri Nov  2 11:23:08 2012
     Samp             1           2           3           4           5
   Line
      1       5.335E+03   5.336E+03   5.337E+03   5.338E+03   5.339E+03
      2       5.336E+03   5.337E+03   5.338E+03   5.339E+03   5.340E+03
label-li real12
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File real12 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in REAL format from a SUN-SOLR host
                1 bands
                2 lines per band
                5 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: lwk -- Fri Nov  2 11:23:07 2012 ----
IVAL=1000.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: AVERAGE -- User: lwk -- Fri Nov  2 11:23:08 2012 ----
 
************************************************************
ush rm -f LOOK*
ush rm -f half*
ush rm -f full*
ush rm -f real*
let $echo="no"
exit
slogoff
$ Return
$!#############################################################################
