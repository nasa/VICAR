$!****************************************************************************
$!
$! Build proc for MIPL module polarect2
$! VPACK Version 1.9, Thursday, February 28, 2002, 11:36:17
$!
$! Execute by entering:		$ @polarect2
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
$ write sys$output "*** module polarect2 ***"
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
$ write sys$output "Invalid argument given to polarect2.com file -- ", primary
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
$   if F$SEARCH("polarect2.imake") .nes. ""
$   then
$      vimake polarect2
$      purge polarect2.bld
$   else
$      if F$SEARCH("polarect2.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake polarect2
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @polarect2.bld "STD"
$   else
$      @polarect2.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create polarect2.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack polarect2.com -mixed -
	-s polarect2.f -
	-i polarect2.imake -
	-p polarect2.pdf -
	-t tstpolarect2.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create polarect2.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c
c program polarect2
c
      include 'VICMAIN_FOR'
      subroutine main44

      parameter (maxnl=3002,maxns=3002)

      integer*4 ounit,def,count,status,unit,samp
      real*4 buf(maxns,maxnl),obuf(maxns)
      real*4 center(2)
      logical xvptst,interp

c checks
      call xveaction('SA',' ')

c open input
      call xvunit(unit,'INP',1,status,' ')
      call xvopen(unit,status,'U_FORMAT','REAL',' ')
      call xvget(unit,status,'NL',nl,'NS',ns,' ')
      if(ns.gt.maxns)then
        call xvmessage('Input image line too long',' ')
        call abend
      endif
      if(nl.gt.maxnl)then
        call xvmessage('Input image column too long',' ')
        call abend
      endif

c parameters
      pi=3.14159265
      call xvparm('NL',nlo,count,def,1)
      call xvparm('NS',nso,count,def,1)
      call xvparm('CENTER',center,count,def,2)
      if(xvptst('INTERP'))then
        interp=.true.
        write(*,*)'Using bilinear interpolation'
      else
        interp=.false.
        write(*,*)'Using nearest neighbor'
      endif

c open output
      call xvunit(ounit,'OUT',1,status,' ')
      call xvopen(ounit,status,'U_FORMAT','REAL','OP','WRITE',
     +  'U_NL',nlo,'U_NS',nso,' ')

c copy input into memory
      do line=1,nl
        call xvread(unit,buf(1,line),status,' ')
      enddo

c create polar image
      if(xvptst('direct'))then
        nl1=nl-1
        ns1=ns-1
        degpline=2.0*pi/nlo
        do line=1,nlo
          theta=(line-1)*degpline
          sinth=sin(theta)
          costh=cos(theta)
          do samp=1,nso
            y=(samp-1)*sinth+center(1)
            x=(samp-1)*costh+center(2)
            if(interp)then ! interpolate
              j=y
              i=x
              if((j.gt.0).and.(j.le.nl1).and.(i.gt.0).and.(i.le.ns1))
     +        then
                top=buf(i+1,j)*(x-i)+buf(i,j)*(i+1.-x)
                bot=buf(i+1,j+1)*(x-i)+buf(i,j+1)*(i+1.-x)
                obuf(samp)=top*(j+1.-y)+bot*(y-j)
              else
                obuf(samp)=0
              endif
            else ! no interpolate
              j=nint(y)
              i=nint(x)
              if((j.gt.0).and.(j.le.nl1).and.(i.gt.0).and.(i.le.ns1))
     +        then
                obuf(samp)=buf(i,j)
              else
                obuf(samp)=0
              endif
            endif
          enddo
          call xvwrit(ounit,obuf,status,' ')
        enddo
      endif

c create cartesian image
      if(xvptst('inverse'))then
        do i=1,ns
          buf(i,nl+1)=buf(i,1)  ! copy first line at end
        enddo
        nl1=nl
        ns1=ns-1
        degpline=2.0*pi/nl
        do line=1,nlo
          do samp=1,nso
            radius=sqrt((line-center(1))**2+(samp-center(2))**2)
            if(radius.gt..001)then
              theta=atan2(line-center(1),samp-center(2))
            else
              theta=pi
            endif
            if(theta.lt.0.0)theta=2.0*pi+theta
            y=theta/degpline+1.0
            x=radius+1.0
            if(interp)then ! interpolate
              j=y
              i=x
              if((j.gt.0).and.(j.le.nl1).and.(i.gt.0).and.(i.le.ns1))
     +        then
                top=buf(i+1,j)*(x-i)+buf(i,j)*(i+1.-x)
                bot=buf(i+1,j+1)*(x-i)+buf(i,j+1)*(i+1.-x)
                obuf(samp)=top*(j+1.-y)+bot*(y-j)
              else
                obuf(samp)=0
              endif
            else  ! no interpolate
              j=nint(y)
              i=nint(x)
              if((j.gt.0).and.(j.le.nl1).and.(i.gt.0).and.(i.le.ns1))
     +        then
                obuf(samp)=buf(i,j)
              else
                obuf(samp)=0
              endif
            endif
          enddo
          call xvwrit(ounit,obuf,status,' ')
        enddo
      endif

      return
      end
     
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create polarect2.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM polarect2

   To Create the build file give the command:

		$ vimake polarect2			(VMS)
   or
		% vimake polarect2			(Unix)


************************************************************************/


#define PROGRAM	polarect2
#define R2LIB

#define MODULE_LIST polarect2.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create polarect2.pdf
process help=*
PARM INP TYPE=STRING COUNT=1
PARM OUT TYPE=STRING COUNT=1
PARM NL TYPE=INTEGER COUNT=1
PARM NS TYPE=INTEGER COUNT=1
PARM CENTER TYPE=REAL COUNT=2
PARM MODE TYPE=KEYWORD COUNT=1 VALID=(DIRECT,INVERSE) DEFAULT=DIRECT
PARM RESAMP TYPE=KEYWORD COUNT=(0,1) VALID=(INTERP,NOINTERP) DEFAULT=INTERP
END-PROC

.TITLE
VICAR program POLARECT2

.HELP
PURPOSE:
To convert images to polar coordinates and back.

METHOD:
In DIRECT mode:
output lines are radial profiles of fixed scale (1:1 pixel scale).
Lines always extend from 0 to 360 degrees in azimuth clockwise from the right.
In INVERSE mode:
Lines and samples are cartesian coordinates.

EXECUTION:
(Initial image is 400 by 400)
polarect2 inp=a out=b nl=100 ns=100 center=(200,200) 'direct
polarect2 inp=b out=c nl=400 ns=400 center=(200,200) 'inverse

.PAGE

METHOD:
HISTORY:
8-1-2001 J Lorre. 
COGNIZANT PROGRAMMER:  Jean Lorre

.LEVEL1
.VARI INP
1 input image

.VARI OUT
1 Output image

.VARI NL
Number output lines

.VARI NS
Number output samples

.VARI CENTER
Center line,sample

.VARI MODE
Direct or Inverse

.VARI RESAMP
Interp or Nointerp

.LEVEL2
.VARI INP
1 input image

.VARI OUT
1 Output image.
Image has NL lines and NS samples.
In DIRECT mode each line is a radial profile from the
center (left) to the edge of the image (right). Line 1 is zero degrees azimuth.

.VARI NL
Number of output lines.
You always get 360 degrees of azimuth for any value of NL.

.VARI NS
Number output samples.
The radial scale is fixed for a 1:1 aspect ratio. So if you set ns=100 you
only get the first 100 radial samples.

.VARI CENTER
Center (line,sample)
Specifies the center of the polar coordinate transformation in the cartesian
image.

.VARI MODE
Specifies the projection type:
Direct or Inverse
Direct means cartesian to polar
Inverse means polar to cartesian

.VARI RESAMP
Whether to interpolate or not.
Interp means to use bilinear interpolation.
Nointerp means to use nearest neighbor

$ Return
$!#############################################################################
$Test_File:
$ create tstpolarect2.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
!
gen out=a.img nl=100 ns=100 linc=50 sinc=50
xvd a.img
polarect2 inp=a.img out=p.img nl=200 ns=50 center=(50,50) +
 'direct
xvd p.img
polarect2 inp=p.img out=a.img nl=100 ns=100 center=(50,50) +
 'inverse
xvd a.img
!
end-proc
$ Return
$!#############################################################################
