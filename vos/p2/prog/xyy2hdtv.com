$!****************************************************************************
$!
$! Build proc for MIPL module xyy2hdtv
$! VPACK Version 1.9, Thursday, January 12, 2006, 11:37:52
$!
$! Execute by entering:		$ @xyy2hdtv
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
$ write sys$output "*** module xyy2hdtv ***"
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
$ write sys$output "Invalid argument given to xyy2hdtv.com file -- ", primary
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
$   if F$SEARCH("xyy2hdtv.imake") .nes. ""
$   then
$      vimake xyy2hdtv
$      purge xyy2hdtv.bld
$   else
$      if F$SEARCH("xyy2hdtv.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake xyy2hdtv
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @xyy2hdtv.bld "STD"
$   else
$      @xyy2hdtv.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create xyy2hdtv.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack xyy2hdtv.com -mixed -
	-s xyy2hdtv.f -
	-i xyy2hdtv.imake -
	-p xyy2hdtv.pdf -
	-t tstxyy2hdtv.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create xyy2hdtv.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c
c program xyy2hdtv
c
      include 'VICMAIN_FOR'
      subroutine main44

      parameter (maxsamp=40000)

      integer*2 red(maxsamp),green(maxsamp),blue(maxsamp)
      integer*4 ounit(3)
      integer*4 unit(3),status
      logical use_macbeth,xvptst
      real*4 x(maxsamp),y(maxsamp),yy(maxsamp)
      real*4 macbeth(3,24)
      real*4 aa(3,3),bb(3,3),cc(3,3)
      character*80 msg
      
      data aa/3.240479, -1.537150, -0.498535,
     +        -0.969256, 1.875992, 0.041556,
     +        0.055648, -0.204043, 1.057311/
      data bb/0.,0.,0., 0.,0.,0., 0.,0.,0./
      data cc/0.,0.,0., 0.,0.,0., 0.,0.,0./

c table of macbeth Yxy values to test algorithm accuracy.
c only used with the MACBETH keyword.
      data macbeth/
     +  10.1,.400,.350,  35.8,.377,.345,  19.3,.247,.251,
     +  13.3,.337,.422,  24.3,.265,.240,  43.1,.261,.343,
     +  30.1,.506,.407,  12.0,.211,.175,  19.8,.453,.306,
     +   6.6,.285,.202,  44.3,.380,.489,  43.1,.473,.438,
     +   6.1,.187,.129,  23.4,.305,.478,  12.0,.539,.313,
     +  59.1,.448,.470,  19.8,.364,.233,  19.8,.196,.252,
     +  90.0,.310,.316,  59.1,.310,.316,  36.2,.310,.316,
     +  19.8,.310,.316,   9.0,.310,.316,   3.1,.310,.316/


c parameters
      use_macbeth=xvptst('MACBETH')
      if(use_macbeth)then
        nl=4
        ns=6
        kmac=0
      endif


c open all inputs
      if(.not.use_macbeth)then
        do i=1,3
          call xvunit(unit(i),'INP',i,status,' ')
          call xvopen(unit(i),status,'U_FORMAT','REAL',' ')
          call xvget(unit(i),status,'NL',nl,'NS',ns,' ')
          if(ns.gt.maxsamp)then
            call xvmessage('Line length too long',' ')
            call abend
          endif
        enddo
      endif

c open all outputs
      do i=1,3
        call xvunit(ounit(i),'OUT',i,status,' ')
        call xvopen(ounit(i),status,'O_FORMAT','BYTE','U_FORMAT','HALF',
     +              'U_NL',nl,'U_NS',ns,'OP','WRITE',' ')
      enddo

      if(use_macbeth)then
        call xvmessage('Macbeth color table',' ')
        call xvmessage('step    x         y         Y     r    g    b'
     +   ,' ')
      endif
      
c process images
      do line=1,nl
                                   ! line loop
        if(.not.use_macbeth)then
          call xvread(unit(1),x,status,'LINE',line,' ')
          call xvread(unit(2),y,status,'LINE',line,' ')
          call xvread(unit(3),yy,status,'LINE',line,' ')
        endif
        
        do ii=1,ns                              ! pixel loop

c         if macbeth cycle through the 24 colors
          if(use_macbeth)then
            kmac=kmac+1
            x(ii)=macbeth(2,kmac)
            y(ii)=macbeth(3,kmac)
            yy(ii)=macbeth(1,kmac)
          endif

c         convert xyY to XYZ normalized from 0 to 1.
          y_tristim=yy(ii)
          x_tristim=yy(ii)*x(ii)/y(ii)
          z_tristim=yy(ii)/y(ii)-x_tristim-yy(ii)
          x_tristim=x_tristim/100.
          y_tristim=y_tristim/100.
          z_tristim=z_tristim/100.
          if(x_tristim.lt.0.)x_tristim=0.
          if(x_tristim.gt.1.)x_tristim=1.
          if(y_tristim.lt.0.)y_tristim=0.
          if(y_tristim.gt.1.)y_tristim=1.
          if(z_tristim.lt.0.)z_tristim=0.
          if(z_tristim.gt.1.)z_tristim=1.
          
c         convert XYZ to sRGB normalized 0 to 1.
   	  bb(1,1) = x_tristim
 	  bb(1,2) = y_tristim
 	  bb(1,3) = z_tristim
          do i=1,3
            do j=1,3
              cc(j,i)=0.
              do k=1,3
                cc(j,i)=cc(j,i)+aa(k,i)*bb(j,k)
              enddo
            enddo
          enddo
          r=cc(1,1) 
          g=cc(1,2)
          b=cc(1,3)
          if(r.lt.0.)r=0.
          if(r.gt.1.)r=1.
          if(g.lt.0.)g=0.
          if(g.gt.1.)g=1.
          if(b.lt.0.)b=0.
          if(b.gt.1.)b=1.
          
c         convert sRGB to spRGB
          if(r.le..00304)then
            r=12.92*r
          else
            r=1.055*r**(1./2.4) - .055
          endif
          if(g.le..00304)then
            g=12.92*g
          else
            g=1.055*g**(1./2.4) - .055
          endif
          if(b.le..00304)then
            b=12.92*b
          else
            b=1.055*b**(1./2.4) - .055
          endif
          red(ii)=nint(r*255.)
          green(ii)=nint(g*255.)
          blue(ii)=nint(b*255.)
          if(red(ii).lt.0)red(ii)=0
          if(red(ii).gt.255)red(ii)=255
          if(green(ii).lt.0)green(ii)=0
          if(green(ii).gt.255)green(ii)=255
          if(blue(ii).lt.0)blue(ii)=0
          if(blue(ii).gt.255)blue(ii)=255


          if(use_macbeth)then
            write(msg,*)kmac,x(ii),y(ii),yy(ii),
     +        red(ii),green(ii),blue(ii)
            call xvmessage(msg,' ')
          endif

        enddo                                  ! pixel loop
        call xvwrit(ounit(1),red,status,' ')
        call xvwrit(ounit(2),green,status,' ')
        call xvwrit(ounit(3),blue,status,' ')
      enddo                                    ! line loop

      RETURN
      END
 
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create xyy2hdtv.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM xyy2hdtv

   To Create the build file give the command:

		$ vimake xyy2hdtv			(VMS)
   or
		% vimake xyy2hdtv			(Unix)


************************************************************************/


#define PROGRAM	xyy2hdtv
#define R2LIB

#define MODULE_LIST xyy2hdtv.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create xyy2hdtv.pdf
process help=*
PARM INP TYPE=STRING COUNT=(0:3) DEFAULT=--
PARM OUT TYPE=STRING COUNT=3
PARM MACBETH TYPE=KEYWORD VALID=("MACBETH","NOMAC") DEFAULT="NOMAC"
END-PROC

.TITLE
VICAR program XYY2HDTV

.HELP
PURPOSE:
To convert color images from xyY to RGB coordinates for the High Definition
TV (HDTV).
Note: the HDTV must be set to a color temperature of D65.

EXECUTION:
xyy2hdtv inp=(x,y,Y) out=(r,g,b)
or
xyy2hdtv out=(r,g,b) 'macbeth

.PAGE

METHOD:
xyy2hdtv does the following:
1. Convert xyY to XYZ tristimulus.
2. Convert XYZ to "Standard RGB" sRGB.
3. Convert sRGB to "Standard Nonlinear RGB" spRGB.

HISTORY:
11-25-1999  J Lorre. 
COGNIZANT PROGRAMMER:  Jean Lorre

.LEVEL1
.VARI INP
0 or 3 input images

.VARI OUT
3 Output images

.VARI MACBETH
write macbeth
chart

.LEVEL2
.VARI INP
Three input images in the order:
1. Chromaticity coordinate x.
2. Chromaticity coordinate y.
3. Tristimulus coordinate Y.
These images are in device independent coordinates.

.VARI OUT
Three Output RGB images in the order:
1. Red dn value in BYTE format.
2. Green dn value in BYTE format.
3. Blue dn value in BYTE format.
These images are in HDTV calibrated coordinates.

.VARI MACBETH
Write a macbeth image.
Print the Macbeth dn values for this device.
No input files are needed.
The output r g b files will contain a tiny picture six
samples by four lines containing the Macbeth color chart where each
patch is a single pixel. You can zoom it up to see it. 

.end
$ Return
$!#############################################################################
$Test_File:
$ create tstxyy2hdtv.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
!
spectoxyy inp=( +
  /project/test_work/testdata/gll/earth.red,  +
  /project/test_work/testdata/gll/earth.grn, +
  /project/test_work/testdata/gll/earth.blu) +
         out=(x.img,y.img,yy.img,hist.img) mode=reflect +
         convert=(.00392,.00392,.00392) lamda=(660,560,430) illumin=sun
xyy2hdtv inp=(x.img,y.img,yy.img) out=(r.img,g.img,b.img)
xvd inp=(+
  /project/test_work/testdata/gll/earth.red,  +
  /project/test_work/testdata/gll/earth.grn, +
  /project/test_work/testdata/gll/earth.blu)
xvd inp=(r.img,g.img,b.img)
!
xyy2hdtv out=(r.img,g.img,b.img) 'macbeth
xvd inp=(r.img,g.img,b.img)
!
end-proc
$ Return
$!#############################################################################
