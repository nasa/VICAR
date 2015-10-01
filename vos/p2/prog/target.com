$!****************************************************************************
$!
$! Build proc for MIPL module target
$! VPACK Version 1.9, Friday, March 26, 1999, 09:23:33
$!
$! Execute by entering:		$ @target
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
$ write sys$output "*** module target ***"
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
$ write sys$output "Invalid argument given to target.com file -- ", primary
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
$   if F$SEARCH("target.imake") .nes. ""
$   then
$      vimake target
$      purge target.bld
$   else
$      if F$SEARCH("target.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake target
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @target.bld "STD"
$   else
$      @target.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create target.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack target.com -
	-s target.f -
	-i target.imake -
	-p target.pdf -
	-t tsttarget.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create target.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c
c program target
c
      include 'VICMAIN_FOR'
      subroutine main44

      parameter (maxsamp=25000)
      integer*2 obufr(maxsamp),obufg(maxsamp),obufb(maxsamp)
      integer*4 unit(3),status,count,def,samp,dn,target

c parameters
      call xvpcnt('OUT',nods)      
      call xvparm('CYCLE',cycle,count,def,1)
      call xvparm('NL',nl,count,def,1)
      call xvparm('NS',ns,count,def,1)
      call xvparm('TARGET',target,count,def,1)
      pi=3.141592654
      
c open outputs
      nli=nl/2
      nsi=ns/2
      nl=nli*2
      ns=nsi*2
      do image=1,nods
        call xvunit(unit(image),'OUT',image,status,' ')
        call xvopen(unit(image),status,'U_FORMAT','HALF',
     +              'U_NL',nl,'U_NS',ns,'OP','WRITE',
     +              'O_FORMAT','BYTE',' ')
      enddo
      
      if(target.gt.1)goto 10
      
c radial spoke target
      alias=(cycle/pi)**2
      do line=1,nli
        y=line-(nli+1)/2
        do samp=1,nsi
          x=samp-(nsi+1)/2
          r=x*x+y*y
          if(r.gt.alias)then
            theta=atan2(y,x)
            dn=nint(127.*cos(theta*cycle)+127.)
            obufr(samp)=dn
            obufr(samp+nsi)=dn
            obufg(samp)=dn
            obufg(samp+nsi)=0
            obufb(samp)=dn
            obufb(samp+nsi)=0
          else
            obufr(samp)=0
            obufr(samp+nsi)=0
            obufg(samp)=0
            obufg(samp+nsi)=0
            obufb(samp)=0
            obufb(samp+nsi)=0
          endif
        enddo
        if(nods.ge.1)
     +    call xvwrit(unit(1),obufr,status,' ')  
        if(nods.ge.2)
     +    call xvwrit(unit(2),obufg,status,' ')  
        if(nods.ge.3)
     +    call xvwrit(unit(3),obufb,status,' ')  
      enddo
      do line=1,nli
        y=line-(nli+1)/2
        do samp=1,nsi
          x=samp-(nsi+1)/2
          r=x*x+y*y
          if(r.gt.alias)then
            theta=atan2(y,x)
            dn=nint(127.*cos(theta*cycle)+127.)
            obufr(samp)=0
            obufr(samp+nsi)=0
            obufg(samp)=dn
            obufg(samp+nsi)=0
            obufb(samp)=0
            obufb(samp+nsi)=dn
          else
            obufr(samp)=0
            obufr(samp+nsi)=0
            obufg(samp)=0
            obufg(samp+nsi)=0
            obufb(samp)=0
            obufb(samp+nsi)=0
          endif
        enddo
        if(nods.ge.1)
     +    call xvwrit(unit(1),obufr,status,' ')  
        if(nods.ge.2)
     +    call xvwrit(unit(2),obufg,status,' ')  
        if(nods.ge.3)
     +    call xvwrit(unit(3),obufb,status,' ')  
      enddo
      goto 20

c radial frequency target
10    alias=max((nli+1)/2,(nsi+1)/2)
      do line=1,nli
        y=line-(nli+1)/2
        do samp=1,nsi
          x=samp-(nsi+1)/2
          r=sqrt(x*x+y*y)
          if(r.eq.0.0)r=.00001
          if(r.lt.alias)then
            wavelength=2.0*alias/r
            dn=nint(127.*cos(r*pi/wavelength)+127.)
            obufr(samp)=dn
            obufr(samp+nsi)=dn
            obufg(samp)=dn
            obufg(samp+nsi)=0
            obufb(samp)=dn
            obufb(samp+nsi)=0
          else
            obufr(samp)=0
            obufr(samp+nsi)=0
            obufg(samp)=0
            obufg(samp+nsi)=0
            obufb(samp)=0
            obufb(samp+nsi)=0
          endif
        enddo
        if(nods.ge.1)
     +    call xvwrit(unit(1),obufr,status,' ')  
        if(nods.ge.2)
     +    call xvwrit(unit(2),obufg,status,' ')  
        if(nods.ge.3)
     +    call xvwrit(unit(3),obufb,status,' ')  
      enddo
      do line=1,nli
        y=line-(nli+1)/2
        do samp=1,nsi
          x=samp-(nsi+1)/2
          r=sqrt(x*x+y*y)
          if(r.eq.0.0)r=.00001
          if(r.lt.alias)then
            wavelength=2.0*alias/r
            dn=nint(127.*cos(r*pi/wavelength)+127.)
            obufr(samp)=0
            obufr(samp+nsi)=0
            obufg(samp)=dn
            obufg(samp+nsi)=0
            obufb(samp)=0
            obufb(samp+nsi)=dn
          else
            obufr(samp)=0
            obufr(samp+nsi)=0
            obufg(samp)=0
            obufg(samp+nsi)=0
            obufb(samp)=0
            obufb(samp+nsi)=0
          endif
        enddo
        if(nods.ge.1)
     +    call xvwrit(unit(1),obufr,status,' ')  
        if(nods.ge.2)
     +    call xvwrit(unit(2),obufg,status,' ')  
        if(nods.ge.3)
     +    call xvwrit(unit(3),obufb,status,' ')  
      enddo

20    return
      end

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create target.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM target

   To Create the build file give the command:

		$ vimake target			(VMS)
   or
		% vimake target			(Unix)


************************************************************************/


#define PROGRAM	target
#define R2LIB

#define MODULE_LIST target.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define R2LIB

#define LIB_FORTRAN
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create target.pdf
process help=*
PARM OUT TYPE=STRING COUNT=(1:3)
PARM CYCLE TYPE=REAL COUNT=(0:1) VALID=(1.:10000.) DEFAULT=100.
PARM NL TYPE=INTEGER COUNT=(0:1) VALID=(1:25000) DEFAULT=1000
PARM NS TYPE=INTEGER COUNT=(0:1) VALID=(1:25000) DEFAULT=1000
PARM TARGET TYPE=INTEGER COUNT=(0:1) VALID=(1:2) DEFAULT=1
END-PROC

.TITLE
VICAR program TARGET

.HELP
PURPOSE:
To create test targets for optical systems from which mtf's can be extracted.

EXECUTION:
target out=(r.img,g.img,b.img) nl=1000 ns=1000

.PAGE
METHOD:

HISTORY:
4-1-99  J Lorre. 
COGNIZANT PROGRAMMER:  Jean Lorre

.LEVEL1

.VARI OUT
1-3 target images

.VARI NL
Number of output lines.

.VARI NS
Number of output samples.

.VARI CYCLE
Number of wavelengths
per circumference.
Only used for target # 1.

.VARI TARGET
1 = radial spokes
2 = radial frequencies

.LEVEL2

.VARI NL
Number of output lines.

.VARI NS
Number of output samples.

.VARI CYCLE
Number of wavelengths
per circumference.

.VARI TARGET
1 = radial spokes
2 = radial frequencies

.END
$ Return
$!#############################################################################
$Test_File:
$ create tsttarget.pdf
procedure
refgbl $echo
body
let $echo="yes"
!
target out=(r.img,g.img,b.img) nl=512 ns=512 cycle=50 target=1
xvd inp=(r.img,g.img,b.img)
target out=(r.img,g.img,b.img) nl=512 ns=512 target=2
xvd inp=(r.img,g.img,b.img)
!
target out=(/project/imax/jjl/r1.img, +
 /project/imax/jjl/g1.img,/project/imax/jjl/b1.img) +
 nl=16000 ns=16000 cycle=2000 target=1
target out=(/project/imax/jjl/r2.img, +
 /project/imax/jjl/g2.img,/project/imax/jjl/b2.img) +
 nl=16000 ns=16000 target=2
!
end-proc
$ Return
$!#############################################################################
