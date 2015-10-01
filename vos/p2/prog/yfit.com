$!****************************************************************************
$!
$! Build proc for MIPL module yfit
$! VPACK Version 1.8, Monday, June 09, 1997, 14:05:25
$!
$! Execute by entering:		$ @yfit
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
$ write sys$output "*** module yfit ***"
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
$ write sys$output "Invalid argument given to yfit.com file -- ", primary
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
$   if F$SEARCH("yfit.imake") .nes. ""
$   then
$      vimake yfit
$      purge yfit.bld
$   else
$      if F$SEARCH("yfit.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake yfit
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @yfit.bld "STD"
$   else
$      @yfit.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create yfit.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack yfit.com -
	-s yfit.f -
	-i yfit.imake -
	-p yfit.pdf -
	-t tstyfit.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create yfit.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c
c program yfit
c
      include 'VICMAIN_FOR'
      subroutine main44

      parameter (maxsamp=20000)
      integer*4 ounit,def,count,hist(0:32767)
      integer*4 inunit,status
      real*4 buf(maxsamp),obuf(maxsamp)

c parameters
      call xvparm('PERCENT',percent,count,def,1)
      call xvparm('MAXY',ymax,count,def,1)

c checks
      call xveaction('SA',' ')

c open inputs & outputs
      call xvunit(inunit,'INP',1,status,' ')
      call xvopen(inunit,status,'U_FORMAT','REAL',' ')
      call xvget(inunit,status,'NL',nl,'NS',ns,' ')
      if(ns.gt.maxsamp)then
        call xvmessage('Line length too long',' ')
        call abend
      endif
      call xvunit(ounit,'OUT',1,status,' ')
      call xvopen(ounit,status,'O_FORMAT','REAL','U_FORMAT','REAL',
     +              'OP','WRITE',' ')

c compute max dn
      bigy=0.0
      do line=1,nl
        call xvread(inunit,buf,status,'LINE',line,' ')
        do i=1,ns
          if((buf(i).ge.0.).and.(buf(i).ne.32767.))then
            if(bigy.lt.buf(i))bigy=buf(i)
          endif
        enddo
      enddo
      scale=32767./bigy
      write(msg,*)'max Y value is',bigy
      call xvmessage(msg,' ')

c compute histogram
      m=0
      do i=0,32767
        hist(i)=0
      enddo
      do line=1,nl
        call xvread(inunit,buf,status,'LINE',line,' ')
        do i=1,ns
          if((buf(i).ge.0.).and.(buf(i).ne.32767.))then
            j=nint(buf(i)*scale)
            hist(j)=hist(j)+1
            m=m+1
          endif
        enddo
      enddo
      count=percent*m/100.

c compute percent location
      m=0
      do i=32766,0,-1
        m=m+hist(i)
        if(m.ge.count)then
          location=i
          goto 10
        endif
      enddo
10    continue
      truelocation=location/scale

c compute scaling factor
      scale=ymax/truelocation
      write(msg,*)truelocation,' becomes ',ymax
      call xvmessage(msg,' ')

c process image
      m=0
      do line=1,nl
        call xvread(inunit,buf,status,'LINE',line,' ')
        do i=1,ns
          if(buf(i).eq.32767.)then
            obuf(i)=100.
          else
            obuf(i)=buf(i)*scale
            if(obuf(i).gt.100.)then
              obuf(i)=100.
              m=m+1
            endif
          endif
        enddo
        call xvwrit(ounit,obuf,status,' ')
      enddo

      write(msg,*)m,' pixels saturated to 100'
      call xvmessage(msg,' ')

      end
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create yfit.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM yfit

   To Create the build file give the command:

		$ vimake yfit			(VMS)
   or
		% vimake yfit			(Unix)


************************************************************************/


#define PROGRAM	yfit
#define R2LIB

#define MODULE_LIST yfit.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create yfit.pdf
process help=*
PARM INP TYPE=STRING COUNT=1
PARM OUT TYPE=STRING COUNT=1
PARM MAXY TYPE=REAL COUNT=(0:1) VALID=(0.:100.) DEFAULT=50.
PARM PERCENT TYPE=REAL COUNT=(0:1) VALID=(0.:100.) DEFAULT=2.
END-PROC

.TITLE
VICAR program yfit

.HELP
PURPOSE:
To autostretch a real image representing tristimulus Y.
The output is a real image from 0 to 100.
Used to convert luminance Y images into reasonable ranges for display devices.

EXECUTION:
yfit inp=Y.img out=YY.img percent=1. maxy=60.
Note: 32767 will be set to 100 in all cases.

HISTORY:
6-30-97  J Lorre. 
COGNIZANT PROGRAMMER:  Jean Lorre

.LEVEL1
.VARI INP
input image

.VARI OUT
Output image

.VARI PERCENT
percent to saturate
on upper end only.

.VARI MAXY
The output Y value
that percent goes to.

.LEVEL2
.VARI INP
input image

.VARI OUT
Output image

.VARI PERCENT
percent to saturate on upper end only.

.VARI MAXY
The output Y value that the input Y value corresponding to "percent" goes to.
Must be between 0 and 100.
$ Return
$!#############################################################################
$Test_File:
$ create tstyfit.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
!
gen out=cc.img nl=100 ns=100 linc=1 sinc=1 ival=0 'real
hist cc.img
yfit inp=cc.img out=ccc.img maxy=60.
hist ccc.img
!
end-proc
$ Return
$!#############################################################################
