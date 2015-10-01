$!****************************************************************************
$!
$! Build proc for MIPL module concat
$! VPACK Version 1.9, Monday, May 11, 1998, 16:08:39
$!
$! Execute by entering:		$ @concat
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
$ write sys$output "*** module concat ***"
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
$ write sys$output "Invalid argument given to concat.com file -- ", primary
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
$   if F$SEARCH("concat.imake") .nes. ""
$   then
$      vimake concat
$      purge concat.bld
$   else
$      if F$SEARCH("concat.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake concat
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @concat.bld "STD"
$   else
$      @concat.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create concat.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack concat.com -
	-s concat.f -
	-i concat.imake -
	-p concat.pdf -
	-t tstconcat.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create concat.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c
c program auxiliary
c
      include 'VICMAIN_FOR'
      subroutine main44

      parameter (maxsamp=20000, maxpix=50, maxint=10000)
      real*4 obuf(maxsamp),inbuf(maxsamp)
      real*4 slop(maxpix),ofst(maxpix)
      integer*4 unit(maxpix),nl(maxpix),ns(maxpix),status
      integer ounit,hist(0:maxint)
      character*100 msg
      logical xvptst,no_stretch

c parameters
      call xvpcnt('INP',nids)      
      no_stretch=xvptst('NOSTR')
      call xvparm('NS',nso,count,def,1)
      call xvparm('PERCENT',percent,count,def,1)
      if(nids.gt.maxpix)then
        call xvmessage('Too many inputs',' ')
        call abend
      endif

c open inputs
      do i=1,nids
        call xvunit(unit(i),'INP',i,status,' ')
        call xvsignal(unit(i),status,1)
        call xvopen(unit(i),status,'U_FORMAT','REAL',' ')
        call xvsignal(unit(i),status,1)
        call xvget(unit(i),status,'NL',nl(i),'NS',ns(i),' ')
        call xvsignal(unit(i),status,1)
        if(ns(i).gt.maxsamp)then
          call xvmessage('Input picture line too long',' ')
          call abend
        endif
        if(ns(i).gt.ns(1))then
          call xvmessage('Input picture lines differ',' ')
          call abend
        endif
        if(nl(i).gt.nl(1))then
          call xvmessage('Input pictures columns differ',' ')
          call abend
        endif
      enddo

c get limits of intensity on inputs
c get percent stretch parameters.
      if(no_stretch)goto 101
      do 100 image=1,nids
        do i=0,maxint
          hist(i)=0
        enddo
        rmin=1.0e+30
        rmax=-1.0e+30
        do line=1,nl(image)
          call xvread(unit(image),inbuf,status,'LINE',line,' ')
          call xvsignal(unit(image),status,1)
          do i=1,ns(image)
            if(inbuf(i).lt.rmin)rmin=inbuf(i)
            if(inbuf(i).gt.rmax)rmax=inbuf(i)
          enddo
        enddo
        write(msg,*)'image= ',image,' min dn= ',rmin,' max dn= ',rmax
        call xvmessage(msg,' ')
        if(rmax.eq.rmin)then
          slop(image)=0.0
          ofst(image)=128.0
          goto 100
        endif
        if(rmax .le. rmin) then
          slope=maxint/1.0
          offset = 0.0
        else
          slope=maxint/(rmax-rmin)
          offset=maxint-slope*rmax
        endif
        do line=1,nl(image)
          call xvread(unit(image),inbuf,status,'LINE',line,' ')
          call xvsignal(unit(image),status,1)
          do i=1,ns(image)
            j=nint(inbuf(i)*slope+offset)
            hist(j)=hist(j)+1
          enddo
        enddo
        ncounts=nl(image)*ns(image)*percent/100.
        n=0
        do i=0,maxint
          n=n+hist(i)
          ii=i
          if(n.ge.ncounts)goto 10
        enddo
10      rmin=(ii-offset)/slope
        n=0
        do i=maxint,0,-1
          n=n+hist(i)
          ii=i
          if(n.ge.ncounts)goto 11
        enddo
11      rmax=(ii-offset)/slope
        if(rmax .le. rmin) then
          slop(image)=255.0
          ofst(image)=0.0
        else
          slop(image)=255./(rmax-rmin)
          ofst(image)=255.-slop(image)*rmax
        endif
100   continue
101   continue

c determine size of output
      if((nso/ns(1))*ns(1).ne.nso)then
        call xvmessage('NS not a multiple of input line length',' ')
        call abend
      endif
      npic_per_row=nso/ns(1)
      npic_per_col=nids/npic_per_row
      if(npic_per_col*npic_per_row.lt.nids)
     +   npic_per_col=npic_per_col+1
      nlo=npic_per_col*nl(1)

c open output
      call xvunit(ounit,'OUT',1,status,' ')
      call xvsignal(ounit,status,1)
      call xvopen(ounit,status,'U_FORMAT','REAL','O_FORMAT','BYTE',
     +              'U_NL',nlo,'U_NS',nso,'OP','WRITE',' ')
      call xvsignal(ounit,status,1)

c write data
      left_image=1-npic_per_row
      do irow=1,npic_per_col
        left_image=left_image+npic_per_row

        do line=1,nl(1)
          k=0
          image=left_image-1

          do icol=1,npic_per_row
            image=image+1

            if(image.le.nids)then
              call xvread(unit(image),inbuf,status,'LINE',line,' ')
              call xvsignal(unit(image),status,1)
              do i=1,ns(image)
                k=k+1
                if(no_stretch)then
                  dn=inbuf(i)
                else
                  dn=inbuf(i)*slop(image)+ofst(image)+0.5
                endif
                if(dn.lt.0.0)dn=0.0
                if(dn.gt.255.0)dn=255.0
                obuf(k)=dn
              enddo
            else
              do i=1,ns(1)
                k=k+1
                obuf(k)=0.0
              enddo
            endif

          enddo
          call xvwrit(ounit,obuf,status,' ')
          call xvsignal(ounit,status,1)
        enddo
      enddo

      return
      end

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create concat.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM concat

   To Create the build file give the command:

		$ vimake concat			(VMS)
   or
		% vimake concat			(Unix)


************************************************************************/


#define PROGRAM	concat
#define R2LIB

#define MODULE_LIST concat.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define R2LIB

#define LIB_FORTRAN
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define DEBUG
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create concat.pdf
process help=*
PARM INP TYPE=STRING COUNT=(2:50)
PARM OUT TYPE=STRING COUNT=1
PARM NS TYPE=INTEGER COUNT=1 valid=(1:20000)
PARM PERCENT TYPE=REAL COUNT=(0:1) DEFAULT=0.0 VALID=(0.:100.)
PARM NOSTR TYPE=KEYWORD VALID=("STRETCH","NOSTR") DEFAULT="STRETCH"
END-PROC

.TITLE
VICAR program CONCAT

.HELP
PURPOSE:
To easily concatenate images into a regular mosaic grid.

EXECUTION:
gen out=a1.img nl=50 ns=100 ival=0
gen out=a2.img nl=50 ns=100 ival=50
gen out=a3.img nl=50 ns=100 ival=100
gen out=a4.img nl=50 ns=100 ival=150
concat inp=(a1.img,a2.img,a3.img,a4.img) out=a.img ns=200

where (in this example):
a.img looks like:

   a1.img   a2.img

   a3.img   a4.img

The inputs can be of any format type.
The output is byte.
The default is to stretch each input image so that it appears from 0 to 
255 dn in the output.

.PAGE
METHOD:
Concat assumes the input images are the same size.
It stretches all inputs from 0 to 255.
It then orders the inputs into rows on the output beginning from left to 
right starting at the top row and working down. When one row is filled it
begins on the next row. The NS keyword determins the number of pixels per
output row and thus the number of images per row.
Empty mosaic grids are filled with zero's.

HISTORY:
10-1-97  J Lorre. 
COGNIZANT PROGRAMMER:  Jean Lorre

.LEVEL1

.VARI INP
Input images

.VARI OUT
Output image

.VARI NS
samples in output

.VARI PERCENT
Percent of histogram
to saturate at each
end.

.VARI NOSTR
No stretch applied.

.LEVEL2

.VARI INP
Input images
From 2 to 50.

.VARI OUT
Output image

.VARI NS
samples in output
Maximum is 20000

.VARI PERCENT
Percent of histogram to saturate at each end.
Default is 0.0

.VARI NOSTR
No stretch is to be applied to any input image.
$ Return
$!#############################################################################
$Test_File:
$ create tstconcat.pdf
procedure
refgbl $echo
body
let $echo="yes"
!
gen out=a1.img nl=50 ns=100 ival=0
gen out=a2.img nl=50 ns=100 ival=50
gen out=a3.img nl=50 ns=100 ival=100
gen out=a4.img nl=50 ns=100 ival=150
concat inp=(a1.img,a2.img,a3.img,a4.img) out=a.img ns=200
list a.img
xvd a.img
!
end-proc
$ Return
$!#############################################################################
