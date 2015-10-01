$!****************************************************************************
$!
$! Build proc for MIPL module ellipse
$! VPACK Version 1.8, Thursday, January 23, 1997, 09:01:54
$!
$! Execute by entering:		$ @ellipse
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
$ write sys$output "*** module ellipse ***"
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
$ write sys$output "Invalid argument given to ellipse.com file -- ", primary
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
$   if F$SEARCH("ellipse.imake") .nes. ""
$   then
$      vimake ellipse
$      purge ellipse.bld
$   else
$      if F$SEARCH("ellipse.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake ellipse
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @ellipse.bld "STD"
$   else
$      @ellipse.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create ellipse.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack ellipse.com -
	-s ellipse.f -
	-i ellipse.imake -
	-p ellipse.pdf -
	-t tstellipse.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ellipse.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c
c program mapiso
c
      include 'VICMAIN_FOR'
      subroutine main44
      include 'mp_for_defs'

      integer*2 obuf(1002,502)
      integer*4 ounit,def,count
      integer*4 status,nl,ns,samp
      real*8 mp
      integer*4 idata(40)
      real*4 data(40)
      real*8 a,b,r2d,d2r,x,y,t,x_center,y_center
      equivalence (data,idata)

c parameters
      call xvparm('NL',nl,count,def,1)
      call xvparm('NS',ns,count,def,1)
      a=(ns-2)/2.d0
      b=(nl-2)/2.d0
      r2d=45.d0/datan(1.d0)
      d2r=1.d0/r2d
      x_center=ns/2.0
      y_center=nl/2.0

c open output
      call xvunit(ounit,'OUT',1,status,' ')
      call xvsignal(ounit,status,1)
      call xvopen(ounit,status,'U_FORMAT','HALF',
     +        'U_NL',nl,'U_NS',ns,'OP','WRITE',
     +        'O_FORMAT','BYTE',' ')
      call xvsignal(ounit,status,1)

c initialize mp routines
      call mp_init( mp,istat) 
      if(istat.ne.mp_success) call mabend('error in mp_init')
c write mp label
      data(25)=b         !r pole in km
      data(26)=a         !r equator in km
      data(31)=0.0       !sclat
      data(32)=0.0       !sclon
      data(33)=y_center  !centline
      data(34)=x_center  !centsamp
      data(38)=1000.*b   !range in km
      data(27)=1000.*b   !focal in pixels or mm
      data(30)=1.0       !scale
      data(35)=0.0       !north
      data(28)=y_center  !oaline
      data(29)=x_center  !oasamp
      idata(39)=16       ! perspective type
      call mp_buf2mpo( idata, mp, status )
      if ( status .ne. 0 ) then
          call xvmessage ('MP error on buffer transfer',0)
          return
      end if
      call mp_label_write( mp, ounit, 'HISTORY', status )
      if ( status .ne. 0 ) then
          call xvmessage ('MP label write error',0)
          return
      end if
      call mp_label_write( mp, ounit, 'PROPERTY', status )
      if ( status .ne. 0 ) then
          call xvmessage ('MP label write error',0)
          return
      end if
 
c process image

c set background.
      do line=1,nl                         ! line loop
        do samp=1,ns                          ! pixel loop
          obuf(samp,line)=1
        enddo
      enddo
  
c draw limb.
      dangle=(1.d0/a)*r2d
      do angle=0,89.999,dangle
        t=tan(angle*d2r)
        x=a*b*dsqrt(1.d0/(b*b+a*a*t*t))
        y=x*t
        i=nint(x + x_center)
        j=nint(y + y_center)
        if((i.ge.1).and.(i.le.ns).and.(j.ge.1).and.(j.le.nl))
     +    obuf(i,j)=255
        i=nint(-x + x_center)
        j=nint(y + y_center)
        if((i.ge.1).and.(i.le.ns).and.(j.ge.1).and.(j.le.nl))
     +    obuf(i,j)=255
        i=nint(x + x_center)
        j=nint(-y + y_center)
        if((i.ge.1).and.(i.le.ns).and.(j.ge.1).and.(j.le.nl))
     +    obuf(i,j)=255
        i=nint(-x + x_center)
        j=nint(-y + y_center)
        if((i.ge.1).and.(i.le.ns).and.(j.ge.1).and.(j.le.nl))
     +    obuf(i,j)=255
      enddo

c draw latitudes.
      dangle=10.
      do angle=0,89.999,dangle
        t=tan(angle*d2r)
        x=a*b*dsqrt(1.d0/(b*b+a*a*t*t))
        y=x*t
        i1=nint(-x + x_center)
        i2=nint(x + x_center)
        j1=nint(-y + y_center)
        j2=nint(y + y_center)
        if(i1.lt.1)i1=1
        if(i2.gt.ns)i2=ns
        if(j1.ge.1.and.j1.le.nl)then
          do i=i1,i2
            obuf(i,j1)=255
          enddo
        endif
        if(j2.ge.1.and.j2.le.nl)then
          do i=i1,i2
            obuf(i,j2)=255
          enddo
        endif
      enddo

c write image
      do line=1,nl                         ! line loop
        call xvwrit(ounit,obuf(1,line),status,' ')
        call xvsignal(ounit,status,1)
      enddo

      return
      end
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create ellipse.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM ellipse

   To Create the build file give the command:

		$ vimake ellipse			(VMS)
   or
		% vimake ellipse			(Unix)


************************************************************************/


#define PROGRAM	ellipse
#define R2LIB

#define MODULE_LIST ellipse.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define USES_C

#define FTNINC_LIST mp_for_defs
#define R2LIB

#define LIB_FORTRAN
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create ellipse.pdf
process help=*
PARM OUT TYPE=STRING COUNT=1
PARM NL TYPE=INTEGER COUNT=(0,1) VALID=(10:502) DEFAULT=502
PARM NS TYPE=INTEGER COUNT=(0,1) VALID=(10:1002) DEFAULT=1002
END-PROC

.TITLE
VICAR program ellipse

.HELP
PURPOSE:
To create images of oblate spheroids for testing map projections.

EXECUTION:
To create an ellipse with a polar to equatorial radius ratio of 1/2.
ellipse out=a.img nl=500 ns=1000

To test map3 and overlay.
ellipse out=z.img nl=502 ns=1002
map3 inp=z.img out=e.img 'noproj 'nosedr +
  nl=502 ns=1002 scale=1. 'perspect line=251 samp=501 +
  north=0. latitude=45. longitud=0
overlay inp=e.img out=f.img dla1=10 dlo1=30
xvd f.img

METHOD:
Ellipse draws an ellipse viewed from the equatorial plane from infinity.
North is up. It is centered in the output image and fills the entire image.
The background is set to dn=1 and the ellipse is drawn as dn=255.
Planetocentric latitudes are drawn every 10 degrees.
An mp perspective projection label is placed on the image where the
range to the ellipse is 1000 radii.
The ellipse oblateness is the ratio of nl/ns.

HISTORY:
2-1-97  J Lorre. 
COGNIZANT PROGRAMMER:  Jean Lorre

.LEVEL1

.VARI OUT
Output image

.VARI NL
Number of lines

.VARI NS
Number of samples

.LEVEL2

.VARI OUT
Byte output image.

.VARI NL
Number of lines in the output image.
This will be the polar diameter of the drawn ellipse.

.VARI NS
Number of samples in the output image.
This will be the equatorial diameter of the drawn ellipse.
$ Return
$!#############################################################################
$Test_File:
$ create tstellipse.pdf
procedure
refgbl $echo
body
let $echo="yes"
!
ellipse out=a.img nl=502 ns=1002
!
end-proc
$ Return
$!#############################################################################
