$!****************************************************************************
$!
$! Build proc for MIPL module disparity
$! VPACK Version 1.9, Tuesday, March 09, 2004, 14:04:33
$!
$! Execute by entering:		$ @disparity
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
$ write sys$output "*** module disparity ***"
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
$ write sys$output "Invalid argument given to disparity.com file -- ", primary
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
$   if F$SEARCH("disparity.imake") .nes. ""
$   then
$      vimake disparity
$      purge disparity.bld
$   else
$      if F$SEARCH("disparity.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake disparity
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @disparity.bld "STD"
$   else
$      @disparity.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create disparity.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack disparity.com -mixed -
	-s disparity.f -
	-i disparity.imake -
	-p disparity.pdf -
	-t tstdisparity.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create disparity.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c program disparity

      include 'VICMAIN_FOR'
      subroutine main44
      
      parameter (nlmax=1100,nsmax=1100)
      real*4 ldis(nsmax,nlmax),sdis(nsmax,nlmax)
      real*4 dis(nsmax,nlmax)
      real*4 unitv(2),rd(2),bounds(2)
      integer*4 count,def,status,unit(2),ounit
c      logical xvptst

      flag=-10

c parameters
      call xveaction('SA',' ')
      call xvparm('BOUNDS',bounds,count,def,2)
      if(flag.gt.bounds(1))flag=bounds(1)

c open inputs
      do i=1,2
        call xvunit(unit(i),'INP',i,status,' ')
        call xvopen(unit(i),status,'U_FORMAT','REAL',' ')
        call xvget(unit(i),status,'NL',nl,'NS',ns,' ')
        if(ns.gt.nsmax)then
          call xvmessage('Line length too long',' ')
          call abend
        endif
        if(nl.gt.nlmax)then
          call xvmessage('Line column too long',' ')
          call abend
        endif
      enddo

c open output
      call xvunit(ounit,'OUT',1,status,' ')
        call xvopen(ounit,status,'U_FORMAT','REAL',
     +              'OP','WRITE',' ')

c read input images
      do j=1,nl
        call xvread(unit(1),ldis(1,j),status,'LINE',j,' ')
      enddo
      do j=1,nl
        call xvread(unit(2),sdis(1,j),status,'LINE',j,' ')
      enddo

c compute radial disparity & put into buf.
      cs=ns/2.0
      cl=nl/2.0
      do j=1,nl
        do i=1,ns

          dis(i,j)=flag ! bad point flag
          if(ldis(i,j).eq.0.0.and.sdis(i,j).eq.0.0)goto 10
          
          rd(1)=sdis(i,j)-i
          rd(2)=ldis(i,j)-j

c         Radial disparity
          unitv(1)=i-cs
          unitv(2)=j-cl
          a=sqrt(unitv(1)**2+unitv(2)**2)
          if(a.eq.0.0)a=1.
          unitv(1)=unitv(1)/a
          unitv(2)=unitv(2)/a
c          disr=sqrt((rd(1)*unitv(1))**2+(rd(2)*unitv(2))**2)
          disr=sqrt(rd(1)**2+rd(2)**2)

c         Check angle between disparity vector and radial direction.
          a=sqrt(rd(1)**2+rd(2)**2)
          rd(1)=rd(1)/a
          rd(2)=rd(2)/a
          costh=rd(1)*unitv(1)+rd(2)*unitv(2)
          th=acos(costh)*57.3
          if(th.gt.20.)goto 10

          dis(i,j)=disr
10        continue          
        enddo
      enddo
      
c remove radial geom
      j=nl/2
      n=0
      slope=0.0
      do j=1,nl,10
        do i=1,ns,10
          rad=sqrt((i-cs)**2+(j-cl)**2)
          if(dis(i,j).ne.flag.and.rad.gt.0.0)then
            slope=slope+dis(i,j)/rad
            n=n+1
          endif
        enddo
      enddo
      slope=slope/n
      do j=1,nl
        do i=1,ns
          rad=sqrt((i-cs)**2+(j-cl)**2)
          if(dis(i,j).ne.flag)then
            dis(i,j)=dis(i,j)-slope*rad
          endif
        enddo
      enddo

c fill in the out of bounds dn's 
20    continue
      nc=0
      do j=2,nl-1
        do i=2,ns-1
          if(dis(i,j).gt.bounds(1).and.dis(i,j).lt.bounds(2))goto 21
          sum=0.0
          n=0
          do jj=j-1,j+1
            do ii=i-1,i+1
              if(i.eq.ii.and.j.eq.jj)goto 22
              if(dis(ii,jj).gt.bounds(1).and.dis(ii,jj).lt.
     +          bounds(2))then
                sum=sum+dis(ii,jj)
                n=n+1
              endif
22            continue
            enddo
          enddo
          if(n.gt.0)then
            dis(i,j)=sum/n
            nc=nc+1
          endif
21        continue
        enddo
      enddo
      if(nc.gt.0)then
        write(*,*)nc,' pixels interpolated'
        goto 20
      endif
      
c write radial disparity
      do j=1,nl
        call xvwrit(ounit,dis(1,j),status,' ')
      enddo

      end





$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create disparity.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM disparity

   To Create the build file give the command:

		$ vimake disparity			(VMS)
   or
		% vimake disparity			(Unix)


************************************************************************/


#define PROGRAM	disparity
#define R2LIB

#define MODULE_LIST disparity.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create disparity.pdf
process help=*
PARM INP TYPE=STRING COUNT=2
PARM OUT TYPE=STRING COUNT=1
PARM BOUNDS TYPE=REAL COUNT=(0,2) VALID=(-1000.:1000.,-1000.:1000.) +
 DEFAULT=(-2.,2.)
END-PROC

.TITLE
VICAR program disparity

.HELP
Disparity combines two disparity images (line and sample disparity images)
into a single image representing radial disparity. The input images come 
from a pair of microscope images which are at the same camera location but
at different focus positions.
Restriction: The disparity must be radially outward, not inward.

EXAMPLE:

I used marscor3.  It wants an input disparity map.  Easiest thing to do is
to give it an identity map at really reduced resolution.

I used a 64x64 map.  That equates to a pyramid level of 4 (128,256,512,1024
is 4 steps).  Use gen to make it.  From memory (not tried):

$R2LIB/gen fake.line 64 64 linc=1 sinc=0 ival=1 -real
$R2LIB/gen fake.samp 64 64 linc=0 sinc=1 ival=1 -real
$R2LIB/viccub \(fake.line fake.samp\) fake.disp

$MARSLIB/marscor3 inp=\( img1 img2 \) out=out.disp in_disp=fake.disp
  templ=\(13,13\) search=29 q=.25 -gore gore_q=.25 gore_pass=4
  disp_pyramid=4 -amoeba8 ftol=.002 -multipass

copy inp=out.disp out=linedisp sb=1 nb=1
copy inp=out.disp out=sampdisp sb=2 nb=1
disparity inp=(linedisp,sampdisp) out=dtm bounds=(-2.,2.)

dtm is a REAL format file.

METHOD:
Two images obtained at different focus locations display radial disparity.
This program converts line & sample disparity into radial disparity.
It also removes the overall average radial motion in order to reveal the
differential radial disparity which betrays topography.
Any differential disparity which exceeds BOUNDS is interpolated over. This
includes non correlated points and bad correlations.

HISTORY:
3-2004  J Lorre. 
COGNIZANT PROGRAMMER:  Jean Lorre

.LEVEL1
.VARI INP
2 input images

.VARI OUT
Radial disparity

.VARI BOUNDS
1. Low threshold
2. High threshold

.LEVEL2
.VARI INP
2 input images.
1. line disparity.
2. sample disparity

.VARI OUT
Topo map (dtm). REAL format

.VARI BOUNDS
1. Low threshold
2. High threshold
Differential disparity bounds limits.
Any differential disparity outside these bounds is interpolated over.
Defaults to bounds=(-2.,+2.)

end-proc                                                                     
        
$ Return
$!#############################################################################
$Test_File:
$ create tstdisparity.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
!
disparity inp=(c3_c5_ldis.img,c3_c5_sdis.img) out=dtm.img
!
end-proc
$ Return
$!#############################################################################
