$!****************************************************************************
$!
$! Build proc for MIPL module rotate2
$! VPACK Version 1.9, Wednesday, January 09, 2013, 19:26:44
$!
$! Execute by entering:		$ @rotate2
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
$ write sys$output "*** module rotate2 ***"
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
$ write sys$output "Invalid argument given to rotate2.com file -- ", primary
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
$   if F$SEARCH("rotate2.imake") .nes. ""
$   then
$      vimake rotate2
$      purge rotate2.bld
$   else
$      if F$SEARCH("rotate2.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake rotate2
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @rotate2.bld "STD"
$   else
$      @rotate2.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create rotate2.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack rotate2.com -mixed -
	-s rotate2.f -
	-i rotate2.imake -
	-p rotate2.pdf -
	-t tstrotate2.pdf tstrotate2.log_solos
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create rotate2.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C     PROGRAM ROTATE2
c     09 Jan 2013 ...lwk...   fixed OUT1/OUT2 assignments due to different behaviour
c                             with the -e compiler flag on Solaris
c     20 APR 2011 ...RJB...   Changed call qprint to call xvmessage under AFIDS
C      9 JUN 93   ...SP....   MADE PORTABLE FOR UNIX.  CHANGED TO USE XVEACTION
C                             INSTEAD OF XVCHECK.
C      4 NOV 88   ...SP....   ADDED CODE TO HANDLE CASE OF NLO=1 OR NSO=1.
C      1 MAY 84   ...CCA...   CONVERT TO VICAR2
C      1 JAN 84   ...CCA...   CONVERT TO VAX, NO VXCTL
C     25 JUL 72   ...ARG...   INITIAL VERSIONM
C      1 MAR 73   ...FGS...   CORRECT CENTER OPTION IN OUTPUT PICTURE
C     22 MAR 73   ...FGS...   INCLUDE FAKIBCOM
C     15 MAY 73   ...FGS...   DELETE PARAMETER DATA SET
C     27 JUN 75   ...DAH...   CHANGES FOR CONVERSION TO 360/OS
C     29 MAR 79   ...JAM...   INCORPORATE HALF AND SPLINE OPTIONS
C      1 APR 79   ...JAM...   SET DEFAULT ANGLE TO ZERO
C      1 APR 79   ...JAM...   REMOVE EQUIVALENCE BETWEEN KWD AND PAR
C
C ** PURPOSE... TO GENERATE GEOM PARAMETER SETS DESCRIBING PICTURE
C               ROTATIONS AND TO FETCH LGEOM. 
C
C ** TO  'GEOM' USE...
C     ROTATE2 INP SIZE=() LINE=X SAMP=X ANGL=X CENT=(L,S)
C
C         WHERE
C   LINE, SAMP, ARE THE CENTER OF ROTATION FOR THE INPUT PICTURE
C   THESE MAY BE DEFAULTED TO THE CENTER OF THE INPUT PICTURE
C
C   ANGL IS THE ROTATION IN DEGREES CLOCKWISE FROM 'UP'.
C
C   CENT IS THE LINE AND SAMPLE LOCATION OF THE CENTER OF ROTATION
C   IN THE OUTPUT PICTURE. IT CAN BE DEFAULTED TO THE CENTER OF THE
C   OUTPUT PICTURE.
C
C   THE KEYWORD 'NOIN' MAY BE USED FOR TRANSMISSION TO LGEOM
C
	implicit none
      	real*4 rparm(16),ll,ls,ang
	real*4 dl,ds,fl,fs,s,c,csi,cso,cli,clo
      	integer*4 sl,ss,nlo,nso,nli,nsi,stat,nlo2,nso2,icent,iline,isamp
	integer*4 iunit,icount,idef,idummy,i,npds
	logical*4 XVPTST
      	character*256 pds
      	character*8 format
      	character*132 out1
      	character*132 out2

c     	DATA OUT1/' REGION (    ,     ,     ,     ) OF THE INPUT PICTURE I
c    +S ROTATED         DEGREES ABOUT       ,        '/

C==================================================================
	call xvmessage ("ROTATE2 - 09-Jan-2013"," ")
      CALL XVEACTION('SA',' ') ! SET XV ERROR ACTION

      icent = 0
      iline = 0
      isamp = 0
      ang=0.
      call zia(RPARM,16)
C        OPEN INPUT DATA SET
      CALL XVUNIT(IUNIT,'INP',1,STAT,' ')
      CALL XVOPEN(IUNIT,STAT,' ')
C        GET DATA FORMAT AND CHECK
      CALL XVGET(IUNIT,STAT,'FORMAT',FORMAT,' ')
      if (format.ne.'BYTE' .and. format.ne.'HALF') then
	call xvmessage('??E - ROTATE2 accepts BYTE and HALF data only',' ')
        call abend
      endif

C        GET SIZE INFORMATION 
      CALL XVSIZE(SL,SS,NLO,NSO,NLI,NSI)

C        CLOSE INPUT DATA SET
      CALL XVCLOSE(IUNIT,STAT,' ')

C        GET NAME OF PARAMETER DATA SET
      CALL XVP('PDS',PDS,NPDS)

C           PROCESS PARAMETERS
C        'LINE'
      CALL XVPARM('LINE',RPARM,ICOUNT,IDEF,1)
      if (icount .ne. 0) then
         cli = RPARM(1)
	 clo = cli - sl + 1
	 iline = 1
      endif

C        'SAMPLE'
      CALL XVPARM('SAMPLE',RPARM,ICOUNT,IDEF,1)
      if (icount .ne. 0) then
	 csi = RPARM(1)
	 cso = csi - ss + 1
	 isamp = 1
      endif

C        'CENTER'
      CALL XVPARM('CENTER',RPARM,ICOUNT,IDEF,2)
      if (icount .ne. 0) then
	 clo = RPARM(1)
	 cso = RPARM(2)
	 icent = 1
      endif

C        'ANGLE'
      CALL XVPARM('ANGLE',RPARM,ICOUNT,IDEF,1)
      if (icount .ne. 0) ang = RPARM(1)

      OUT1(1:53) = ' REGION (    ,     ,     ,     ) OF THE INPUT PICTURE'
      OUT1(54:102) = ' IS ROTATED         DEGREES ABOUT       ,        '
      nli = min0(nli+1-sl,nlo)
      nsi = min0(nsi+1-ss,nso)
      write (out1(10:13),'(i4)') sl
      write (out1(16:19),'(i4)') ss
      write (out1(22:25),'(i4)') nli
      write (out1(28:31),'(i4)') nsi
      if (iline .eq. 0) cli=.5*(sl+nli)
      if (isamp .eq. 0) csi=.5*(ss+nsi)
      if (icent .eq. 0) clo=.5*(1+nlo)
      if (icent .eq. 0) cso=.5*(1+nso)
      write (out1(66:72),'(f7.1)') ang
      write (out1(87:92),'(f6.1)') cli
      write (out1(95:100),'(f6.1)') csi
      call xvmessage(out1(2:102),' ')
      write (out2,9900) clo,cso
9900  format (
     +' THE CENTER OF ROTATION IN THE OUTPUT PICTURE IS LOCATED AT PIXEL     ',
     + F6.1,',  ',F6.1)
      call xvmessage(out2(2:85),' ')

      ang=(-ang)*3.14159/180.
      c=cos(ang)
      s=sin(ang)


      nlo2 = max( nlo, 2 )   ! IF NLO OR NSO IS 1, THEN USE 2 AS THE
      nso2 = max( nso, 2 )   ! ENDING TIEPOINT LOCATION SO RECTANGLE
                             ! WILL NOT BE DEGENERATE.

      ds=csi-cso
      dl=cli-clo
      fl=1.-clo
      fs=1.-cso
      ll=nlo2-clo
      ls=nso2-cso

C        Open the parameter data set
      i = 3
      IF (XVPTST('NOINTERP')) I = I + 1
      IF (FORMAT .EQ. 'HALF') I = I + 1
      CALL XVPOPEN(STAT,I,64,PDS,'SA',IDUMMY)

C        Write out the parameters
      CALL XVPOUT(STAT,'NAH',1,'INT',1)
      CALL XVPOUT(STAT,'NAV',1,'INT',1)

      RPARM(1) = 1
      RPARM(2) = 1
      RPARM(3) = S*FS+C*FL-FL+DL + 1
      RPARM(4) = C*FS-S*FL-FS+DS + 1
      RPARM(5) = 1
      RPARM(6) = NSO2
      RPARM(7) = S*LS+C*FL-FL+DL + 1
      RPARM(8) = C*LS-S*FL-LS+DS + NSO2
      RPARM(9) = NLO2
      RPARM(10) = 1
      RPARM(11) = S*FS+C*LL-LL+DL + NLO2
      RPARM(12) = C*FS-S*LL-FS+DS + 1
      RPARM(13) = NLO2
      RPARM(14) = NSO2
      RPARM(15) = S*LS+C*LL-LL+DL + NLO2
      RPARM(16) = C*LS-S*LL-LS+DS + NSO2

      CALL XVPOUT(STAT,'TIEPOINT',RPARM,'REAL',16)

      IF(XVPTST('NOINTERP')) THEN
         CALL XVPOUT(STAT,'INTERP','NOIN','STRING',1)
      ENDIF

      IF(FORMAT .EQ. 'HALF') THEN
         CALL XVPOUT(STAT,'FORMAT','HALF','STRING',1)
      ENDIF

C        Close the parameter data set
      CALL XVPCLOSE(STAT)

      return
      end
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create rotate2.imake
#define  PROGRAM   rotate2

#define MODULE_LIST rotate2.f

#define MAIN_LANG_FORTRAN
#define R2LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create rotate2.pdf
PROCESS HELP=*
PARM 	INP	TYPE=STRING	COUNT=1
PARM	PDS	TYPE=STRING	COUNT=1         DEFAULT=ZZPAR
PARM 	SIZE	TYPE=INTEGER	COUNT=(0:4) 	DEFAULT=--
PARM 	SL	TYPE=INTEGER	COUNT=(0:1)	DEFAULT=--
PARM 	SS	TYPE=INTEGER	COUNT=(0:1)	DEFAULT=--
PARM	NL	TYPE=INTEGER	COUNT=(0:1)	DEFAULT=--
PARM	NS	TYPE=INTEGER	COUNT=(0:1)	DEFAULT=--
PARM	ANGLE	TYPE=REAL	COUNT=(0:1)	DEFAULT=--
PARM	LINE	TYPE=REAL	COUNT=(0:1)	DEFAULT=--
PARM	SAMPLE	TYPE=REAL	COUNT=(0:1)	DEFAULT=--
PARM	CENTER	TYPE=REAL	COUNT=(0:2)	DEFAULT=--
PARM	NOINTERP   TYPE=KEYWORD	COUNT=(0:1) VALID="NOINTERP" DEFAULT=--
END-PROC
.title
vicar Program ROTATE2
.help
PURPOSE:

ROTATE2 will compute the geometric transformation parameters for rotating a 
picture by any amount about a specified point.
ROTATE2 is typically not called directly by the user but rather from
procedure ROTATE, which will rotate a picture by any amount about a 
specified point.

EXECUTION:
   For the typical usage, see the HELP for procedure ROTATE.

   The following is the execution statement format for ROTATE2:
		rotate2 inp  params
   where INP,  and PARAMS	 are parameters discussed in their 
   respective sections.

OPERATION:

ROTATE2 generates parameters for LGEOM or MGEOM to rotate a picture.  These 
parameters are passed via ROTATE2's generated parameter data set.

The rotation is about an axis normal to the picture and intersecting it at
the specified pixel center of rotation.

The size field should take into account any increase in the number 
of lines and samples due to the rotation.
examples:

1) rotate2 IN par size=(1,1,100,160) line=15. samp=35. angl=24.2
----This example will set up to rotate the 100x160 sample file by 24.2 degrees
    about the pixel at line 15 and sample 35.

2) rotate2 IN par size=(1,1,100,160) angl=24.2
----This example does the same but about the center of the picture.

3) rotate2 IN par angl=-1. center=(50.,30.)
----This example will set up to  rotate IN by -1. degrees about its center and
    translate the rotated picture so that the center of rotation in the 
    output occupies line 50, sample 30.
.page
 LIMITATIONS
  1. The input file must be either BYTE or HALFWORD.
.page

 ORIGINAL PROGRAMMER:    A. R. Gillespie, 25 Jul 1972
 COGNIZANT PROGRAMMER:  L. W. Kamp
 PORTED TO UNIX: Steve Pohorsky

 REVISION HISTORY
  93-6-8    SP   Made portable for UNIX.
  10-Apr-2011  RJB  changed qprint call to xvmessage
  09 Jan 2013 ...lwk... fixed OUT1/OUT2 assignments due to different behaviour
                        with the -e compiler flag on Solaris

.level1
.vari inp
The data file to be rotated.
.vari pds
The output parameter data set.
.vari size
The area to be rotated.
.vari sl
The starting line of the size 
field.
.vari ss
The starting  sample of the 
size field.
.vari nl
The number of lines in the 
size field.
.vari ns 
The number of samples in the 
size field.
.vari angle
Amount of rotation in degrees.
.vari line
The line number of the center 
of rotation.
.vari sample
The sample of the center of 
rotation.
.vari center
The location of the 
output center of rotation.
.vari nointerp
Indicates no interpolation.
.level2
.vari inp
A VICAR labelled image to be rotated.
.vari pds
An output file of GEOM parameters in "parms" format.  See LGEOM or MGEOM 
documentation.  (The default for this parameter data set is a temporary file 
with the name ZZPAR)
.vari size
The size field indicates which area of the input image is to be 
rotated.  The NL and NS parameters specify the size of the output image.
.vari sl
The starting line of the size field.
.vari ss
The starting sample of the size field.
.vari nl
The number of lines in the size field.  Also, the number of lines in the
output image.
.vari ns
The number of bytes in the size field.  Also, the number of bytes in the 
output image.
.vari angle
This specifies the number of degrees clockwise from up to rotate the image. 
May be positive or negative. 
Default = 0.
.vari line
This is the line number of the center of rotation in the input image.
Default = .5 * (sl + nl) ...the center line of the picture.
.vari sample
This is the sample number of the center of rotation in the input image.
Default = .5 * (ss + ns) ...the center sample of the picture.  Values are
in samples, not bytes.
.vari center
This specifies the center of rotation in the output image.  Default is the 
same as that specified for or defaulted for the input image.  (Note that the
sample values are expressed in units of pixels, not bytes.)
.vari nointerp
This specifies that no interpolation is to be performed during the GEOM.
In this case, the DN value of the point closest to the fractional line and
sample is used.  This method ("nearest neighbor") is somewhat faster, but is
not as accurate as the four point interpolation.  Default = NOINTERP.
$ Return
$!#############################################################################
$Test_File:
$ create tstrotate2.pdf
procedure
refgbl $echo
! Jun 25, 2012 - RJB
! TEST SCRIPT FOR ROTATE2
! tests BYTE, HALF images
!
! Vicar Programs:
!       gen list rotate   
! 
! parameters:
!   <none>
!
! Requires NO external test data: 
body
let _onfail="stop"
let $echo="no"
write "THIS IS A TEST OF MODULE ROTATE2"
write "ROTATE calls rotate2"
write "We will rotate a gen'd image by -45 deg such that"
write "shading should appear in the sample direction only"
let $echo="yes"
gen A nl=15 ns=15 ival=90
list A
rotate A B angle=-45. idsnam=ids.dat idsns=1000
list B
let $echo="no"
write "Shift the output center of rotation and use nointerp"
let $echo="yes"
rotate A B angle=-45. 'noin center=(8,4)
list B
let $echo="no"
write "Now lets rotate about (10,6) [104 dn]"
write " and make it end up at (3,3)....and in halfword"
let $echo="yes"
gen C nl=15 ns=16 ival=90 'half
list C
rotate C D angle=-45. line=10 samp=6 center=(3,3)
list D
let $echo="no"

! clean up:
ush rm -f ?
ush rm ZZPAR

end-proc
$!-----------------------------------------------------------------------------
$ create tstrotate2.log_solos
tstrotate2
THIS IS A TEST OF MODULE ROTATE2
ROTATE calls rotate2
We will rotate a gen'd image by -45 deg such that
shading should appear in the sample direction only
gen A nl=15 ns=15 ival=90
Beginning VICAR task gen
GEN Version 6
GEN task completed
list A
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Jan  9 18:52:13 2013
     Samp     1       3       5       7       9      11      13      15
   Line
      1      90  91  92  93  94  95  96  97  98  99 100 101 102 103 104
      2      91  92  93  94  95  96  97  98  99 100 101 102 103 104 105
      3      92  93  94  95  96  97  98  99 100 101 102 103 104 105 106
      4      93  94  95  96  97  98  99 100 101 102 103 104 105 106 107
      5      94  95  96  97  98  99 100 101 102 103 104 105 106 107 108
      6      95  96  97  98  99 100 101 102 103 104 105 106 107 108 109
      7      96  97  98  99 100 101 102 103 104 105 106 107 108 109 110
      8      97  98  99 100 101 102 103 104 105 106 107 108 109 110 111
      9      98  99 100 101 102 103 104 105 106 107 108 109 110 111 112
     10      99 100 101 102 103 104 105 106 107 108 109 110 111 112 113
     11     100 101 102 103 104 105 106 107 108 109 110 111 112 113 114
     12     101 102 103 104 105 106 107 108 109 110 111 112 113 114 115
     13     102 103 104 105 106 107 108 109 110 111 112 113 114 115 116
     14     103 104 105 106 107 108 109 110 111 112 113 114 115 116 117
     15     104 105 106 107 108 109 110 111 112 113 114 115 116 117 118
rotate A B angle=-45. idsnam=ids.dat idsns=1000
ROTATE2	INP=@INP PDS=ZZPAR SIZE=@SIZE	SL=@SL	SS=@SS	NL=@NL	NS=@NS	 +
	ANGLE=@ANGLE	NOINTERP=@NOINTERP	 +
	LINE=@LINE	SAMPLE=@SAMPLE	CENTER=@CENTER
Beginning VICAR task ROTATE2
ROTATE2 - 09-Jan-2013
REGION (   1,    1,   15,   15) OF THE INPUT PICTURE IS ROTATED   -45.0 DEGREES ABOUT   8.0 ,   8.0
THE CENTER OF ROTATION IN THE OUTPUT PICTURE IS LOCATED AT PIXEL        8.0,     8.0
IF ($COUNT(OUT) = 0) RETURN
LGEOM INP=A OUT=B SIZE=@SIZE NL=@NL NS=@NS        +
  IDSNAM=@IDSNAM IDSNS=@IDSNS PARMS=ZZPAR
Beginning VICAR task LGEOM
END-PROC
list B
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Jan  9 18:52:13 2013
 Task:LGEOM     User:lwk       Date_Time:Wed Jan  9 18:52:15 2013
     Samp     1       3       5       7       9      11      13      15
   Line
      1       0   0   0   0   0 101 103 104 105 107   0   0   0   0   0
      2       0   0   0   0 100 101 103 104 105 107 108   0   0   0   0
      3       0   0   0  98 100 101 103 104 105 107 108 110   0   0   0
      4       0   0  97  98 100 101 103 104 105 107 108 110 111   0   0
      5       0  96  97  98 100 101 103 104 105 107 108 110 111 112   0
      6      94  96  97  98 100 101 103 104 105 107 108 110 111 112 114
      7      94  96  97  98 100 101 103 104 105 107 108 110 111 112 114
      8      94  96  97  98 100 101 103 104 105 107 108 110 111 112 114
      9      94  96  97  98 100 101 103 104 105 107 108 110 111 112 114
     10      94  96  97  98 100 101 103 104 105 107 108 110 111 112 114
     11       0  96  97  98 100 101 103 104 105 107 108 110 111 112   0
     12       0   0  97  98 100 101 103 104 105 107 108 110 111   0   0
     13       0   0   0  98 100 101 103 104 105 107 108 110   0   0   0
     14       0   0   0   0 100 101 103 104 105 107 108   0   0   0   0
     15       0   0   0   0   0 101 103 104 105 107   0   0   0   0   0
let $echo="no"
Shift the output center of rotation and use nointerp
rotate A B angle=-45. 'noin center=(8,4)
ROTATE2	INP=@INP PDS=ZZPAR SIZE=@SIZE	SL=@SL	SS=@SS	NL=@NL	NS=@NS	 +
	ANGLE=@ANGLE	NOINTERP=@NOINTERP	 +
	LINE=@LINE	SAMPLE=@SAMPLE	CENTER=@CENTER
Beginning VICAR task ROTATE2
ROTATE2 - 09-Jan-2013
REGION (   1,    1,   15,   15) OF THE INPUT PICTURE IS ROTATED   -45.0 DEGREES ABOUT   8.0 ,   8.0
THE CENTER OF ROTATION IN THE OUTPUT PICTURE IS LOCATED AT PIXEL        8.0,     4.0
IF ($COUNT(OUT) = 0) RETURN
LGEOM INP=A OUT=B SIZE=@SIZE NL=@NL NS=@NS        +
  IDSNAM=@IDSNAM IDSNS=@IDSNS PARMS=ZZPAR
Beginning VICAR task LGEOM
END-PROC
list B
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Jan  9 18:52:13 2013
 Task:LGEOM     User:lwk       Date_Time:Wed Jan  9 18:52:16 2013
     Samp     1       3       5       7       9      11      13      15
   Line
      1     100 102 102 104 106 106 108   0   0   0   0   0   0   0   0
      2     100 101 103 104 105 107 108 110   0   0   0   0   0   0   0
      3      99 101 103 104 105 107 109 109 111   0   0   0   0   0   0
      4     100 101 102 104 106 107 108 110 111 112   0   0   0   0   0
      5     100 101 102 104 106 107 108 110 111 112 114   0   0   0   0
      6      99 101 103 104 105 107 109 109 111 113 114 115   0   0   0
      7     100 101 103 104 105 107 108 110 111 113 114 115 117   0   0
      8     100 102 102 104 106 106 108 110 112 112 114 116 116 118   0
      9     100 101 103 104 105 107 108 110 111 113 114 115 117   0   0
     10      99 101 103 104 105 107 109 109 111 113 114 115   0   0   0
     11     100 101 102 104 106 107 108 110 111 112 114   0   0   0   0
     12     100 101 102 104 106 107 108 110 111 112   0   0   0   0   0
     13      99 101 103 104 105 107 109 109 111   0   0   0   0   0   0
     14     100 101 103 104 105 107 108 110   0   0   0   0   0   0   0
     15     100 102 102 104 106 106 108   0   0   0   0   0   0   0   0
let $echo="no"
Now lets rotate about (10,6) [104 dn]
 and make it end up at (3,3)....and in halfword
gen C nl=15 ns=16 ival=90 'half
Beginning VICAR task gen
GEN Version 6
GEN task completed
list C
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Wed Jan  9 18:52:16 2013
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14    15
   Line
      1        90    91    92    93    94    95    96    97    98    99   100   101   102   103   104
      2        91    92    93    94    95    96    97    98    99   100   101   102   103   104   105
      3        92    93    94    95    96    97    98    99   100   101   102   103   104   105   106
      4        93    94    95    96    97    98    99   100   101   102   103   104   105   106   107
      5        94    95    96    97    98    99   100   101   102   103   104   105   106   107   108
      6        95    96    97    98    99   100   101   102   103   104   105   106   107   108   109
      7        96    97    98    99   100   101   102   103   104   105   106   107   108   109   110
      8        97    98    99   100   101   102   103   104   105   106   107   108   109   110   111
      9        98    99   100   101   102   103   104   105   106   107   108   109   110   111   112
     10        99   100   101   102   103   104   105   106   107   108   109   110   111   112   113
     11       100   101   102   103   104   105   106   107   108   109   110   111   112   113   114
     12       101   102   103   104   105   106   107   108   109   110   111   112   113   114   115
     13       102   103   104   105   106   107   108   109   110   111   112   113   114   115   116
     14       103   104   105   106   107   108   109   110   111   112   113   114   115   116   117
     15       104   105   106   107   108   109   110   111   112   113   114   115   116   117   118

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Wed Jan  9 18:52:16 2013
     Samp      16
   Line
      1       105
      2       106
      3       107
      4       108
      5       109
      6       110
      7       111
      8       112
      9       113
     10       114
     11       115
     12       116
     13       117
     14       118
     15       119
rotate C D angle=-45. line=10 samp=6 center=(3,3)
ROTATE2	INP=@INP PDS=ZZPAR SIZE=@SIZE	SL=@SL	SS=@SS	NL=@NL	NS=@NS	 +
	ANGLE=@ANGLE	NOINTERP=@NOINTERP	 +
	LINE=@LINE	SAMPLE=@SAMPLE	CENTER=@CENTER
Beginning VICAR task ROTATE2
ROTATE2 - 09-Jan-2013
REGION (   1,    1,   15,   16) OF THE INPUT PICTURE IS ROTATED   -45.0 DEGREES ABOUT  10.0 ,   6.0
THE CENTER OF ROTATION IN THE OUTPUT PICTURE IS LOCATED AT PIXEL        3.0,     3.0
IF ($COUNT(OUT) = 0) RETURN
LGEOM INP=C OUT=D SIZE=@SIZE NL=@NL NS=@NS        +
  IDSNAM=@IDSNAM IDSNS=@IDSNS PARMS=ZZPAR
Beginning VICAR task LGEOM
END-PROC
list D
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Wed Jan  9 18:52:16 2013
 Task:LGEOM     User:lwk       Date_Time:Wed Jan  9 18:52:17 2013
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14    15
   Line
      1       101   103   104   105   107   108   110   111   112   114   115   117     0     0     0
      2       101   103   104   105   107   108   110   111   112   114   115     0     0     0     0
      3       101   103   104   105   107   108   110   111   112   114     0     0     0     0     0
      4       101   103   104   105   107   108   110   111   112     0     0     0     0     0     0
      5       101   103   104   105   107   108   110   111     0     0     0     0     0     0     0
      6       101   103   104   105   107   108   110     0     0     0     0     0     0     0     0
      7       101   103   104   105   107   108     0     0     0     0     0     0     0     0     0
      8       101   103   104   105   107     0     0     0     0     0     0     0     0     0     0
      9         0   103   104   105     0     0     0     0     0     0     0     0     0     0     0
     10         0     0   104     0     0     0     0     0     0     0     0     0     0     0     0
let $echo="no"
exit
slogoff
$ Return
$!#############################################################################
