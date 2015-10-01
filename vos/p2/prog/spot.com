$!****************************************************************************
$!
$! Build proc for MIPL module spot
$! VPACK Version 1.9, Thursday, May 02, 2013, 11:00:28
$!
$! Execute by entering:		$ @spot
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
$ write sys$output "*** module spot ***"
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
$ write sys$output "Invalid argument given to spot.com file -- ", primary
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
$   if F$SEARCH("spot.imake") .nes. ""
$   then
$      vimake spot
$      purge spot.bld
$   else
$      if F$SEARCH("spot.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake spot
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @spot.bld "STD"
$   else
$      @spot.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create spot.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack spot.com -mixed -
	-s spot.f -
	-i spot.imake -
	-p spot.pdf -
	-t tstspot.pdf tstspot.log_solos
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create spot.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      include 'VICMAIN_FOR'
      subroutine main44
C**** VERSION 3 VICAR-2 PARAMS 20 AUGUST 1984 MEM
C     VERSION 2 VICAR-2 IO     13 AUGUST 1984 MEM
C     VERSION 1 VAX CONVERSION 7 AUGUST 1984 MEM
c      INCLUDE 'fortport'
	implicit none
c      COMMON/C1/DN
	character*8 string
	integer*4 outunit,ind,cnt,nl,ns,sl,ss
	integer*4 ix,iy,ishape,ival,count,def
	integer*4 dnmax,sigmax,sigmay,x0,y0
	integer*4 size(4)
	real*4 af,dxmx,eps,fact,r,r2,tmpf,xarg,yarg,yargs
	byte dn(2048)
C
C**** PARAMETER INITIALIZATION
c      nl=1024				!default
c      ns=1024
      sl=1
      ss=1
      dnmax=255
      sigmax=125
      sigmay=125
      x0=512
      y0=512
C**** VICAR*2 OPENS
      call xvmessage('SPOT version 04-Nov-2010',' ')
c      call xvsize(sl,ss,nl,ns,nli,nsi)
	call xvparm ('SIZE',size,count,def,4)
c	print *, 'def = ',def
	if (size(3).eq.0) then
	    call xvp ('NL',nl,cnt)
            call xvp ('NS',ns,cnt)
	else
	    nl=size(3)
	    ns=size(4)
        end if
c	print *, 'nl = ns = ',nl, ns
      call xvunit(outunit,'OUT',1,ind,' ')
      call xvopen(outunit,ind,'U_NL',nl,'U_NS',ns,'OP','WRITE',' ')
C**** NOW PROCESS THE TAE/PDF
C     TO GET THE PARAMETER VALUES
C     USE DEFAULTS IF "CNT"=0
      call xvp('DNMAX',dnmax,cnt)
      call xvp('SIGMAX',sigmax,cnt)
      if(cnt.eq.0)sigmax=ns/8
      call xvp('SIGMAY',sigmay,cnt)
      if(cnt.eq.0)sigmay=nl/8
      call xvp('X0',x0,cnt)
      if(cnt.eq.0)x0=ns/2
      call xvp('Y0',y0,cnt)
      if(cnt.eq.0)y0=nl/2

c
c     bam 7/98 AR-9267
c
c     make sure we don't have a divide by 0
c
      if (dnmax .ne. 0 ) then
          dxmx=255/dnmax
      else
          dxmx = 0 
      end if

	eps=1.E-5
	ishape = 1			!default is GAUSSIAN
	call xvp('SHAPE',string,cnt)
	if(string.eq.'GAUSSIAN') ishape=1
	if(string.eq.'CONICAL') ishape=2
	if(string.eq.'RECPROCL') ishape=3
	if(string.eq.'RECPSQRE') ishape=4
	if(string.eq.'EXPONENT') ishape=5
	if(string.eq.'DOME') ishape=6
	if(string.eq.'DISK') ishape=7


C**** COMPUTE RADIAL DISTANCE

      do 100 iy=1,nl
c
c     bam 7/98 AR-9267
c
c     make sure we don't have a divide by 0
c     for both sigmay and sigmax
c
      if (sigmay .ne. 0 ) then
          yarg=(iy-y0)*1./(sigmay*1.)
      else
          yarg = 0 
      end if

      yargs=yarg**2

      do 99 ix=1,ns
          if (sigmax .ne. 0 ) then
              xarg=(ix-x0)*1./(sigmax*1.)
          else
              xarg = 0 
          end if

      r2=xarg**2+yargs
      
      r=sqrt(r2)
	tmpf = 0
C**** BRANCH TO SHAPE
               GO TO (51,52,53,54,55,56,57),ISHAPE
C     GAUSSIAN SPOT
   51 if (r2.ge.100.) r2=100.
      fact=exp((-0.5)*(r2))
      go to 60
C     CONICAL SPOT
   52 fact=1. - r
      if (fact.lt.0.) fact=0.
      go to 60
C     RECIPROCAL SPOT
   53 if (r.gt.eps) tmpf=1./r
      if ((r.eq.0.).or.(tmpf.ge.dxmx)) tmpf=dxmx
      fact=tmpf
      go to 60
C     RECIPROCAL SQUARED SPOT
   54 if (r2.gt.eps) tmpf=1./r2
      if ((r2.eq.0.).or.(tmpf.gt.dxmx)) tmpf=dxmx
      fact=tmpf
      go to 60
C     EXPONENTIAL SPOT
   55 if (r.ge.100.) r=100.
      fact=exp((-1.)*(r))
      go to 60
C     DOME SPOT
   56 af=1. - r2
      if (af.le.0.) af=0.
      fact=sqrt(af)
	go to 60
C	UNIFORM DISK
   57 af=1. - r
	if (af.le.0) af=0.
	if (af.gt.0) af=1.
	fact=af
C**** CALCULATE OUTPUT VALUE AT SAMPLE LOCATION OF EACH PIXEL
   60 ival = dnmax*fact
c      dn(ix)=int2byte(ival)
	dn(ix) = ival
   99 continue
      call xvwrit(outunit,dn,ind,' ')
  100 continue
	if(ishape.eq.1)call xvmessage('****GAUSSIAN PATTERN GENERATED ',' ')
	if(ishape.eq.2)call xvmessage('****CONICAL PATTERN GENERATED ',' ')
	if(ishape.eq.3)call xvmessage('****RECIPROCAL PATTERN GENERATED ',' ')
	if(ishape.eq.4)call xvmessage('****RECP SQUARED PATTERN GENERATED',' ')
	if(ishape.eq.5)call xvmessage('****EXPONENTIAL PATTERN GENERATED',' ')
	if(ishape.eq.6)call xvmessage('****DOME PATTERN GENERATED ',' ')
	if(ishape.eq.7)call xvmessage('****UNIFORM DISK PATTERN GENERATED ',' ')
      call xvclose(outunit,ind,' ')
      return
      end
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create spot.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM spot

   To Create the build file give the command:

		$ vimake spot			(VMS)
   or
		% vimake spot			(Unix)


************************************************************************/


#define PROGRAM	spot
#define R2LIB

#define MODULE_LIST spot.f

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
$ create spot.pdf
process help=*
PARM OUT      TYPE=STRING  
PARM SIZE     TYPE=INTEGER COUNT=4 VALID=(0:2048) DEFAULT=(1,1,0,0)
PARM SL       TYPE=INTEGER DEFAULT=1
PARM SS       TYPE=INTEGER DEFAULT=1
PARM NL       TYPE=INTEGER VALID=(0:2048) DEFAULT=2048
PARM NS       TYPE=INTEGER VALID=(0:2048) DEFAULT=2048
PARM SHAPE    TYPE=KEYWORD VALID=(GAUSSIAN,CONICAL,RECPROCL,+
                                  RECPSQRE,EXPONENT,DOME,DISK)+
                           DEFAULT=GAUSSIAN+
                           COUNT=(0:1)
PARM DNMAX    TYPE=INTEGER VALID=(0:255) DEFAULT=255
PARM SIGMAX   TYPE=INTEGER DEFAULT=2
PARM SIGMAY   TYPE=INTEGER DEFAULT=2
PARM X0       TYPE=INTEGER DEFAULT=6
PARM Y0       TYPE=INTEGER DEFAULT=6

!# annot function="Generating Synthetic Images"
!# annot keywords=(profiles,size,BYTE,shape,OUT,DN)
END-PROC
.TITLE
Synthesizes images of spots of various sizes and profiles
.HELP
PURPOSE

   SPOT is a VICAR*2 applications program to generate spots of various
   profiles and sizes. SPOT currently creates only byte images. If you
   want HALF, FULL or real follow the SPOT call with a cform to translate
   it to those formats.

    To make a uniform disk of DN =  255 you have to call spot with
    with DISK shape

 EXECUTION:
  
  spot OUT=sp.dat SIZE=(1,1,24,24) SHAPE=GAUSSIAN X0=10 Y0=10+
  SIGMAX=1 SIGMAY=1

   This example will create a 24 x 24 data set 'SP.DAT' with
   a gaussian profile centered at line=10, sample=10.

  NOTE that non-circular symetrical spots can be created by making
  SIGMAX not equal to SIGMAY.

   The SHAPE keyword has six values;
   'GAUSSIAN' FOR GAUSSIAN spot
   'CONICAL'  FOR CONICAL spot
   'RECPROCL' FOR RECIPROCAL spot
   'RECPSQRE' FOR RECIPROCAL SQUARED spot
   'EXPONENT' FOR EXPONENTIAL spot
   'DOME'     FOR DOME spot
   'DISK'     FOR UNIFORM DISK spot
   Other keywords are defined in the TUTOR mode.
.page
 RESTRICTIONS:
  1. BYTE DATA ONLY. Follow SPOT with cform to convert to other formats
  2. MAX SIZE IS 2048 LINE BY 2048 SAMPLES.
  3. SL=1 SS=1 ARE FIXED; THEY ARE IGNORED IN PROGRAM.

 PROGRAM HISTORY:

  04 Nov 2010   R. J. Bambery - added uniform disk model
  02 Nov 2010   R. J. Bambery - expand to 2048x2048, fix
                bug that prevented nl and ns from overriding
                size parameter, changed default nl,ns = 1024
                to nl,ns = 0
  10 Mar 2009   M. Smyth - add to core vicar - 64 bit
  28 MAR  1994  CRI  MSTP S/W CONVERSION (VICAR PORTING) 
  15 AUG  1984...M.E.MORRILL...CONVERTED TO VAX-VICAR*2
  27 JUNE 1975...D.A.HASS...CHANGES FOR CONVERSION TO 360/OS
  12 APRIL 1973...F.G.STAUDHAMMER...DOCUMENTED
  15 MARCH 1973...Y.K.CHEN...ORIGINAL RELEASE

  CURRENT COGNIZANT PROGRAMMER:  L. W. Kamp
.LEVEL1
.VARIABLE OUT
 An output data set
.VARIABLE SIZE
 The standard Vicar size field
.VARIABLE SL
 Starting line:
INTERNALLY SET=1
.VARIABLE SS
 Starting sample:
INTERNALLY SET=1
.VARIABLE NL
 Number of lines
 Valid: 1-2048
.VARIABLE NS
 Number of samples
 Valid: 1-2048
.VARIABLE SHAPE
 KEYWORD-OPTIONAL
 Valid: GAUSSIAN,CONICAL,DISK,
RECPROCAL,RECPSQRE,EXPONENT,DOME
.VARIABLE DNMAX
 INTEGER-OPTIONAL
 - Maximum DN (255)
.VARIABLE SIGMAX
 KEYWORD-OPTIONAL
 - Variance in Sample direction
 - Default NS/8
.VARIABLE SIGMAY
 KEYWORD-OPTIONAL
 - Variance in Line direction
 - Default NL/8
.VARIABLE X0
 KEYWORD-OPTIONAL
 - Sample coordinate of spot
 center relative to 1,1
 - Default NS/2
.VARIABLE Y0
 KEYWORD-OPTIONAL
 - Line coordinate of spot
 center relative to 1,1
 -Default NL/2
.LEVEL2
.VARIABLE OUT
 An output data set
.VARIABLE SIZE
 The standard Vicar size field
.VARIABLE SL
 Starting line-INTERNALLY SET=1
.VARIABLE SS
 Starting sample-INTERNALLY SET=1
.VARIABLE NL
 Number of lines
 Valid: 1-2048
.VARIABLE NS
 Number of samples
 Valid: 1-2048
.VARIABLE SHAPE
There are six options avalable. In the following 'X' is the sample
coordinate and 'Y' is the line coordinate.
  A. GAUSSIAN (DEFAULT): This specifies the spot shape to be Gaussian.
     The DN(X,Y) of the output dtat set will be given by;
     
     DN(X,Y)= DNMAX*EXP(-(X-X0)**2/(2.0*SIGMAX**2))
              *EXP(-(Y-Y0)**2/(2.0*SIGMAY**2))
  
  B. CONICAL: This specifies the spot shape to be conical;
    
     DN(X,Y)=DNMAX*(1.0-R)   Where;
    
     R=SQRT(((X-X0)/SIGMAX)**2+((Y-Y0)/SIGMAY)**2)

  C. RECPROCL: This specifies the spot shape to be reciprocal;
 
     DN(X,Y)=DNMAX*(1.0/R)   Where R is defined above.

  D. RECPSQURE: This specifies the spot shape to be the reciprocal
     squared;
 
     DN(X,Y)=DNMAX*(1.0/R**2)  Where R is defined above.

  E. EXPONENT: This specifies the spot shape to be exponential;
    
     DN(X,Y)=DNMAX*EXP(-R)  Where R is defined above.

  F. DOME: This specifies the spot shape to be dome shaped;
    
     DN(X,Y)=DNMAX*SQRT(1.0-R**2)  Where R is defined above.

  G. DISK: This specfies the spot shape to be a uniform disk;

     DN(X,Y)=DNMAX*(1.0-R)   Where;

     R=SQRT(((X-X0)/SIGMAX)**2+((Y-Y0)/SIGMAY)**2)
      where if lt 0, the equals zero and where gt 0 equals dnmax



NOTE THAT FOR NEGATIVE COMPUTED DN'S THE OUTPUT IS TRUNCATED TO ZERO.
.VARIABLE SIGMAX
  This specifies the variance of the DN(X,Y) distribution in the sample
  direction. For the GAUSSIAN shape, SIGMAX is the Sigma (half-width)
  of the spot. For all other spot shapes 2*SIGMAX is the entire extent
  of the spot shape in the sample direction. SIGMAX must be greater
  than 0. The default value is NS/8.
.VARIABLE SIGMAY
  This is the equivalent of SIGMAX in the line direction.
.VARIABLE X0
  This specifies the center of the spot relative to SS=1: Center=X0.
  The default is X0=NS/2.
.VARIABLE Y0
  This specifies the center of the spot relative to SL=1: Center=Y0.
  The default is Y0=NL/2.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstspot.pdf
procedure
refgbl $echo
refgbl $autousage
! Jun 26, 2012 - RJB
! TEST SCRIPT FOR SPOT   
! tests BYTE images
!
! Vicar Programs:
!       label-list list cform
! 
! parameters:
!   <none>
!
! Requires NO external test data: 

body
let $autousage="none"
let _onfail="stop"
write "A more extensive test than that used for GALILEO"
let $echo="yes"
!
spot out=sp.dat size=(1,1,24,24)
label-li sp.dat
list sp.dat sl=2 ss=2 nl=22 ns=22
!
spot out=sp2.dat nl=256 ns=256 shape=dome sigmax=20 sigmay=20
list sp2.dat sl=108 ss=108 nl=40 ns=40
label-li sp2.dat

spot out=sp3.dat nl=50 ns=50 shape=dome sigmax=10 sigmay=5
list sp3.dat sl=20 ss=20 nl=10 ns=10

spot out=spx.dat nl=50 ns=50 shape=dome sigmax=7 sigmay=7
cform spx.dat spy.dat irange=(0,1) orange=(0.,255.) oform=byte
list spy.dat sl=15 ss=20 nl=15 ns=15

spot out=sp4.dat nl=50 ns=50 shape=conical sigmax=10 sigmay=10
list sp4.dat sl=20 ss=20 nl=10 ns=10

spot out=sp5.dat nl=50 ns=50 shape=recprocl sigmax=10 sigmay=10
list sp5.dat sl=14 ss=14 nl=10 ns=10

spot out=sp6.dat nl=50 ns=50 shape=recpsqre sigmax=10 sigmay=10
list sp6.dat sl=14 ss=14 nl=10 ns=10

spot out=sp7.dat nl=50 ns=50 shape=exponent sigmax=10 sigmay=10
list sp7.dat sl=20 ss=20 nl=10 ns=10

spot out=sp8.dat nl=50 ns=50 shape=disk sigmax=10 sigmay=10
list sp8.dat sl=14 ss=14 nl=10 ns=10

spot out=sp9.dat nl=256 ns=256 shape=disk sigmax=50 sigmay=50
list sp9.dat sl=98 ss=80 nl=10 ns=10
let $echo="no"

! clean up:
ush rm -f sp*.dat

end-proc

$!-----------------------------------------------------------------------------
$ create tstspot.log_solos
tstspot
A more extensive test than that used for GALILEO
spot out=sp.dat size=(1,1,24,24)
Beginning VICAR task spot
SPOT version 04-Nov-2010
****GAUSSIAN PATTERN GENERATED
label-li sp.dat
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File sp.dat ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                24 lines per band
                24 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: SPOT -- User: lwk -- Fri Nov 30 14:38:42 2012 ----
 
************************************************************
list sp.dat sl=2 ss=2 nl=22 ns=22
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:SPOT      User:lwk       Date_Time:Fri Nov 30 14:38:42 2012
     Samp     2       4       6       8      10      12      14      16      18      20      22
   Line

      3       0   0   0   0   0   0   1   1   2   2   2   2   2   1   1   0   0   0   0   0   0   0
      4       0   0   0   0   0   1   2   4   5   6   7   6   5   4   2   1   0   0   0   0   0   0
      5       0   0   0   1   2   4   6  10  13  15  16  15  13  10   6   4   2   1   0   0   0   0
      6       0   0   0   2   4   8  14  20  27  32  34  32  27  20  14   8   4   2   0   0   0   0
      7       0   0   1   4   8  15  26  38  50  60  63  60  50  38  26  15   8   4   1   0   0   0
      8       0   1   2   6  14  26  43  63  83  99 104  99  83  63  43  26  14   6   2   1   0   0
      9       0   1   4  10  20  38  63  93 123 146 154 146 123  93  63  38  20  10   4   1   0   0
     10       0   2   5  13  27  50  83 123 163 193 204 193 163 123  83  50  27  13   5   2   0   0
     11       0   2   6  15  32  60  99 146 193 228 241 228 193 146  99  60  32  15   6   2   0   0
     12       0   2   7  16  34  63 104 154 204 241 255 241 204 154 104  63  34  16   7   2   0   0
     13       0   2   6  15  32  60  99 146 193 228 241 228 193 146  99  60  32  15   6   2   0   0
     14       0   2   5  13  27  50  83 123 163 193 204 193 163 123  83  50  27  13   5   2   0   0
     15       0   1   4  10  20  38  63  93 123 146 154 146 123  93  63  38  20  10   4   1   0   0
     16       0   1   2   6  14  26  43  63  83  99 104  99  83  63  43  26  14   6   2   1   0   0
     17       0   0   1   4   8  15  26  38  50  60  63  60  50  38  26  15   8   4   1   0   0   0
     18       0   0   0   2   4   8  14  20  27  32  34  32  27  20  14   8   4   2   0   0   0   0
     19       0   0   0   1   2   4   6  10  13  15  16  15  13  10   6   4   2   1   0   0   0   0
     20       0   0   0   0   0   1   2   4   5   6   7   6   5   4   2   1   0   0   0   0   0   0
     21       0   0   0   0   0   0   1   1   2   2   2   2   2   1   1   0   0   0   0   0   0   0
spot out=sp2.dat nl=256 ns=256 shape=dome sigmax=20 sigmay=20
Beginning VICAR task spot
SPOT version 04-Nov-2010
****DOME PATTERN GENERATED
list sp2.dat sl=108 ss=108 nl=40 ns=40
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:SPOT      User:lwk       Date_Time:Fri Nov 30 14:38:43 2012
     Samp   108     110     112     114     116     118     120     122     124     126     128     130     132     134     136
   Line

    109       0   0   0   0   0   0   0   0   0   0   0   0   0   0  22  47  61  69  75  78  79  78  75  69  61  47  22   0   0   0
    110       0   0   0   0   0   0   0   0   0   0   0   0  44  66  80  91  98 104 108 110 111 110 108 104  98  91  80  66  44   0
    111       0   0   0   0   0   0   0   0   0   0  42  69  87 100 110 118 124 128 131 133 134 133 131 128 124 118 110 100  87  69
    112       0   0   0   0   0   0   0   0   0  61  84 101 114 124 132 139 144 148 150 152 152 152 150 148 144 139 132 124 114 101
    113       0   0   0   0   0   0   0  31  70  93 110 123 134 143 150 156 160 164 166 168 168 168 166 164 160 156 150 143 134 123
    114       0   0   0   0   0   0  36  75  98 116 130 141 150 158 165 170 174 178 180 181 182 181 180 178 174 170 165 158 150 141
    115       0   0   0   0   0  31  75 100 118 133 145 156 164 172 178 182 186 189 192 193 193 193 192 189 186 182 178 172 164 156
    116       0   0   0   0   0  70  98 118 134 148 159 168 176 183 189 193 197 200 202 203 204 203 202 200 197 193 189 183 176 168
    117       0   0   0   0  61  93 116 133 148 160 170 179 186 193 198 203 206 209 211 212 212 212 211 209 206 203 198 193 186 179
    118       0   0   0  42  84 110 130 145 159 170 180 188 195 201 207 211 214 217 219 220 220 220 219 217 214 211 207 201 195 188
    119       0   0   0  69 101 123 141 156 168 179 188 196 203 209 214 218 221 224 226 227 227 227 226 224 221 218 214 209 203 196
    120       0   0  44  87 114 134 150 164 176 186 195 203 210 215 220 224 228 230 232 233 233 233 232 230 228 224 220 215 210 203
    121       0   0  66 100 124 143 158 172 183 193 201 209 215 221 226 230 233 235 237 238 238 238 237 235 233 230 226 221 215 209
    122       0  22  80 110 132 150 165 178 189 198 207 214 220 226 230 234 237 240 241 242 243 242 241 240 237 234 230 226 220 214
    123       0  47  91 118 139 156 170 182 193 203 211 218 224 230 234 238 241 243 245 246 246 246 245 243 241 238 234 230 224 218
    124       0  61  98 124 144 160 174 186 197 206 214 221 228 233 237 241 244 246 248 249 249 249 248 246 244 241 237 233 228 221
    125       0  69 104 128 148 164 178 189 200 209 217 224 230 235 240 243 246 249 250 251 252 251 250 249 246 243 240 235 230 224
    126       0  75 108 131 150 166 180 192 202 211 219 226 232 237 241 245 248 250 252 253 253 253 252 250 248 245 241 237 232 226
    127       0  78 110 133 152 168 181 193 203 212 220 227 233 238 242 246 249 251 253 254 254 254 253 251 249 246 242 238 233 227
    128       0  79 111 134 152 168 182 193 204 212 220 227 233 238 243 246 249 252 253 254 255 254 253 252 249 246 243 238 233 227
    129       0  78 110 133 152 168 181 193 203 212 220 227 233 238 242 246 249 251 253 254 254 254 253 251 249 246 242 238 233 227
    130       0  75 108 131 150 166 180 192 202 211 219 226 232 237 241 245 248 250 252 253 253 253 252 250 248 245 241 237 232 226
    131       0  69 104 128 148 164 178 189 200 209 217 224 230 235 240 243 246 249 250 251 252 251 250 249 246 243 240 235 230 224
    132       0  61  98 124 144 160 174 186 197 206 214 221 228 233 237 241 244 246 248 249 249 249 248 246 244 241 237 233 228 221
    133       0  47  91 118 139 156 170 182 193 203 211 218 224 230 234 238 241 243 245 246 246 246 245 243 241 238 234 230 224 218
    134       0  22  80 110 132 150 165 178 189 198 207 214 220 226 230 234 237 240 241 242 243 242 241 240 237 234 230 226 220 214
    135       0   0  66 100 124 143 158 172 183 193 201 209 215 221 226 230 233 235 237 238 238 238 237 235 233 230 226 221 215 209
    136       0   0  44  87 114 134 150 164 176 186 195 203 210 215 220 224 228 230 232 233 233 233 232 230 228 224 220 215 210 203
    137       0   0   0  69 101 123 141 156 168 179 188 196 203 209 214 218 221 224 226 227 227 227 226 224 221 218 214 209 203 196
    138       0   0   0  42  84 110 130 145 159 170 180 188 195 201 207 211 214 217 219 220 220 220 219 217 214 211 207 201 195 188
    139       0   0   0   0  61  93 116 133 148 160 170 179 186 193 198 203 206 209 211 212 212 212 211 209 206 203 198 193 186 179
    140       0   0   0   0   0  70  98 118 134 148 159 168 176 183 189 193 197 200 202 203 204 203 202 200 197 193 189 183 176 168
    141       0   0   0   0   0  31  75 100 118 133 145 156 164 172 178 182 186 189 192 193 193 193 192 189 186 182 178 172 164 156
    142       0   0   0   0   0   0  36  75  98 116 130 141 150 158 165 170 174 178 180 181 182 181 180 178 174 170 165 158 150 141
    143       0   0   0   0   0   0   0  31  70  93 110 123 134 143 150 156 160 164 166 168 168 168 166 164 160 156 150 143 134 123
    144       0   0   0   0   0   0   0   0   0  61  84 101 114 124 132 139 144 148 150 152 152 152 150 148 144 139 132 124 114 101
    145       0   0   0   0   0   0   0   0   0   0  42  69  87 100 110 118 124 128 131 133 134 133 131 128 124 118 110 100  87  69
    146       0   0   0   0   0   0   0   0   0   0   0   0  44  66  80  91  98 104 108 110 111 110 108 104  98  91  80  66  44   0
    147       0   0   0   0   0   0   0   0   0   0   0   0   0   0  22  47  61  69  75  78  79  78  75  69  61  47  22   0   0   0

   BYTE     samples are interpreted as   BYTE   data
 Task:SPOT      User:lwk       Date_Time:Fri Nov 30 14:38:43 2012
     Samp   138     140     142     144     146
   Line

    111      42   0   0   0   0   0   0   0   0   0
    112      84  61   0   0   0   0   0   0   0   0
    113     110  93  70  31   0   0   0   0   0   0
    114     130 116  98  75  36   0   0   0   0   0
    115     145 133 118 100  75  31   0   0   0   0
    116     159 148 134 118  98  70   0   0   0   0
    117     170 160 148 133 116  93  61   0   0   0
    118     180 170 159 145 130 110  84  42   0   0
    119     188 179 168 156 141 123 101  69   0   0
    120     195 186 176 164 150 134 114  87  44   0
    121     201 193 183 172 158 143 124 100  66   0
    122     207 198 189 178 165 150 132 110  80  22
    123     211 203 193 182 170 156 139 118  91  47
    124     214 206 197 186 174 160 144 124  98  61
    125     217 209 200 189 178 164 148 128 104  69
    126     219 211 202 192 180 166 150 131 108  75
    127     220 212 203 193 181 168 152 133 110  78
    128     220 212 204 193 182 168 152 134 111  79
    129     220 212 203 193 181 168 152 133 110  78
    130     219 211 202 192 180 166 150 131 108  75
    131     217 209 200 189 178 164 148 128 104  69
    132     214 206 197 186 174 160 144 124  98  61
    133     211 203 193 182 170 156 139 118  91  47
    134     207 198 189 178 165 150 132 110  80  22
    135     201 193 183 172 158 143 124 100  66   0
    136     195 186 176 164 150 134 114  87  44   0
    137     188 179 168 156 141 123 101  69   0   0
    138     180 170 159 145 130 110  84  42   0   0
    139     170 160 148 133 116  93  61   0   0   0
    140     159 148 134 118  98  70   0   0   0   0
    141     145 133 118 100  75  31   0   0   0   0
    142     130 116  98  75  36   0   0   0   0   0
    143     110  93  70  31   0   0   0   0   0   0
    144      84  61   0   0   0   0   0   0   0   0
    145      42   0   0   0   0   0   0   0   0   0
label-li sp2.dat
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File sp2.dat ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                256 lines per band
                256 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: SPOT -- User: lwk -- Fri Nov 30 14:38:43 2012 ----
 
************************************************************
spot out=sp3.dat nl=50 ns=50 shape=dome sigmax=10 sigmay=5
Beginning VICAR task spot
SPOT version 04-Nov-2010
****DOME PATTERN GENERATED
list sp3.dat sl=20 ss=20 nl=10 ns=10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:SPOT      User:lwk       Date_Time:Fri Nov 30 14:38:44 2012
     Samp    20      22      24      26      28
   Line

     21      84 114 132 144 150 152 150 144 132 114
     22     159 176 189 197 202 204 202 197 189 176
     23     195 210 220 228 232 233 232 228 220 210
     24     214 228 237 244 248 249 248 244 237 228
     25     220 233 243 249 253 255 253 249 243 233
     26     214 228 237 244 248 249 248 244 237 228
     27     195 210 220 228 232 233 232 228 220 210
     28     159 176 189 197 202 204 202 197 189 176
     29      84 114 132 144 150 152 150 144 132 114
spot out=spx.dat nl=50 ns=50 shape=dome sigmax=7 sigmay=7
Beginning VICAR task spot
SPOT version 04-Nov-2010
****DOME PATTERN GENERATED
cform spx.dat spy.dat irange=(0,1) orange=(0.,255.) oform=byte
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *   255.000+     0.000
INPUT FORMAT = BYTE
OUTPUT FORMAT = BYTE
CONVERSION COMPLETE
list spy.dat sl=15 ss=20 nl=15 ns=15
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:SPOT      User:lwk       Date_Time:Fri Nov 30 14:38:44 2012
 Task:CFORM     User:lwk       Date_Time:Fri Nov 30 14:38:45 2012
     Samp    20      22      24      26      28      30      32      34
   Line

     19       0   0 255 255 255 255 255 255 255   0   0   0   0   0   0
     20       0 255 255 255 255 255 255 255 255 255   0   0   0   0   0
     21     255 255 255 255 255 255 255 255 255 255 255   0   0   0   0
     22     255 255 255 255 255 255 255 255 255 255 255 255   0   0   0
     23     255 255 255 255 255 255 255 255 255 255 255 255   0   0   0
     24     255 255 255 255 255 255 255 255 255 255 255 255   0   0   0
     25     255 255 255 255 255 255 255 255 255 255 255 255   0   0   0
     26     255 255 255 255 255 255 255 255 255 255 255 255   0   0   0
     27     255 255 255 255 255 255 255 255 255 255 255 255   0   0   0
     28     255 255 255 255 255 255 255 255 255 255 255 255   0   0   0
     29     255 255 255 255 255 255 255 255 255 255 255   0   0   0   0
spot out=sp4.dat nl=50 ns=50 shape=conical sigmax=10 sigmay=10
Beginning VICAR task spot
SPOT version 04-Nov-2010
****CONICAL PATTERN GENERATED
list sp4.dat sl=20 ss=20 nl=10 ns=10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:SPOT      User:lwk       Date_Time:Fri Nov 30 14:38:45 2012
     Samp    20      22      24      26      28
   Line
     20      74  91 106 117 124 127 124 117 106  91
     21      91 110 127 140 149 153 149 140 127 110
     22     106 127 146 163 174 178 174 163 146 127
     23     117 140 163 182 197 204 197 182 163 140
     24     124 149 174 197 218 229 218 197 174 149
     25     127 153 178 204 229 255 229 204 178 153
     26     124 149 174 197 218 229 218 197 174 149
     27     117 140 163 182 197 204 197 182 163 140
     28     106 127 146 163 174 178 174 163 146 127
     29      91 110 127 140 149 153 149 140 127 110
spot out=sp5.dat nl=50 ns=50 shape=recprocl sigmax=10 sigmay=10
Beginning VICAR task spot
SPOT version 04-Nov-2010
****RECIPROCAL PATTERN GENERATED
list sp5.dat sl=14 ss=14 nl=10 ns=10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:SPOT      User:lwk       Date_Time:Fri Nov 30 14:38:46 2012
     Samp    14      16      18      20      22
   Line
     14     163 171 179 187 195 203 211 217 223 228
     15     171 180 189 199 208 218 228 236 244 250
     16     179 189 200 211 223 235 247 255 255 255
     17     187 199 211 225 239 255 255 255 255 255
     18     195 208 223 239 255 255 255 255 255 255
     19     203 218 235 255 255 255 255 255 255 255
     20     211 228 247 255 255 255 255 255 255 255
     21     217 236 255 255 255 255 255 255 255 255
     22     223 244 255 255 255 255 255 255 255 255
     23     228 250 255 255 255 255 255 255 255 255
spot out=sp6.dat nl=50 ns=50 shape=recpsqre sigmax=10 sigmay=10
Beginning VICAR task spot
SPOT version 04-Nov-2010
****RECP SQUARED PATTERN GENERATED
list sp6.dat sl=14 ss=14 nl=10 ns=10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:SPOT      User:lwk       Date_Time:Fri Nov 30 14:38:47 2012
     Samp    14      16      18      20      22
   Line
     14     105 115 126 137 149 162 174 186 196 204
     15     115 127 140 155 171 187 204 219 233 245
     16     126 140 157 175 196 217 240 255 255 255
     17     137 155 175 199 225 255 255 255 255 255
     18     149 171 196 225 255 255 255 255 255 255
     19     162 187 217 255 255 255 255 255 255 255
     20     174 204 240 255 255 255 255 255 255 255
     21     186 219 255 255 255 255 255 255 255 255
     22     196 233 255 255 255 255 255 255 255 255
     23     204 245 255 255 255 255 255 255 255 255
spot out=sp7.dat nl=50 ns=50 shape=exponent sigmax=10 sigmay=10
Beginning VICAR task spot
SPOT version 04-Nov-2010
****EXPONENTIAL PATTERN GENERATED
list sp7.dat sl=20 ss=20 nl=10 ns=10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:SPOT      User:lwk       Date_Time:Fri Nov 30 14:38:47 2012
     Samp    20      22      24      26      28
   Line
     20     125 134 142 148 153 154 153 148 142 134
     21     134 144 154 163 168 170 168 163 154 144
     22     142 154 166 177 185 188 185 177 166 154
     23     148 163 177 192 203 208 203 192 177 163
     24     153 168 185 203 221 230 221 203 185 168
     25     154 170 188 208 230 255 230 208 188 170
     26     153 168 185 203 221 230 221 203 185 168
     27     148 163 177 192 203 208 203 192 177 163
     28     142 154 166 177 185 188 185 177 166 154
     29     134 144 154 163 168 170 168 163 154 144
spot out=sp8.dat nl=50 ns=50 shape=disk sigmax=10 sigmay=10
Beginning VICAR task spot
SPOT version 04-Nov-2010
****UNIFORM DISK PATTERN GENERATED
list sp8.dat sl=14 ss=14 nl=10 ns=10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:SPOT      User:lwk       Date_Time:Fri Nov 30 14:38:48 2012
     Samp    14      16      18      20      22
   Line

     16       0   0   0   0   0   0   0 255 255 255
     17       0   0   0   0   0   0 255 255 255 255
     18       0   0   0   0 255 255 255 255 255 255
     19       0   0   0   0 255 255 255 255 255 255
     20       0   0   0 255 255 255 255 255 255 255
     21       0   0 255 255 255 255 255 255 255 255
     22       0   0 255 255 255 255 255 255 255 255
     23       0   0 255 255 255 255 255 255 255 255
spot out=sp9.dat nl=256 ns=256 shape=disk sigmax=50 sigmay=50
Beginning VICAR task spot
SPOT version 04-Nov-2010
****UNIFORM DISK PATTERN GENERATED
list sp9.dat sl=98 ss=80 nl=10 ns=10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:SPOT      User:lwk       Date_Time:Fri Nov 30 14:38:49 2012
     Samp    80      82      84      86      88
   Line
     98       0   0   0   0   0   0   0   0   0 255
     99       0   0   0   0   0   0   0   0 255 255
    100       0   0   0   0   0   0   0 255 255 255
    101       0   0   0   0   0   0 255 255 255 255
    102       0   0   0   0   0   0 255 255 255 255
    103       0   0   0   0   0 255 255 255 255 255
    104       0   0   0   0   0 255 255 255 255 255
    105       0   0   0   0 255 255 255 255 255 255
    106       0   0   0   0 255 255 255 255 255 255
    107       0   0   0 255 255 255 255 255 255 255
let $echo="no"
exit
slogoff
$ Return
$!#############################################################################
