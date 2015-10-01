$!****************************************************************************
$!
$! Build proc for MIPL module jpegfix
$! VPACK Version 1.9, Thursday, February 04, 1999, 13:51:22
$!
$! Execute by entering:		$ @jpegfix
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
$ write sys$output "*** module jpegfix ***"
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
$ write sys$output "Invalid argument given to jpegfix.com file -- ", primary
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
$   if F$SEARCH("jpegfix.imake") .nes. ""
$   then
$      vimake jpegfix
$      purge jpegfix.bld
$   else
$      if F$SEARCH("jpegfix.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake jpegfix
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @jpegfix.bld "STD"
$   else
$      @jpegfix.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create jpegfix.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack jpegfix.com -
	-s jpegfix.f -
	-i jpegfix.imake -
	-p jpegfix.pdf -
	-t tstjpegfix.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create jpegfix.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c
c program smooth
c
      include 'VICMAIN_FOR'
      subroutine main44

      parameter(npix=2000000)
      integer*2 buf1(npix),buf2(npix),t
      integer*4 ounit,inunit,status
      integer*4 def,count,thresh
      character*32 format
      logical roundoff
      
c get parameters
      call xvparm('ITER',iter,count,def,1)
      call xvparm('THRESH',thresh,count,def,1)
      t=thresh
            
c open input
      call xvunit(inunit,'INP',1,status,' ')
      call xvopen(inunit,status,'U_FORMAT','HALF',' ')
      call xvget(inunit,status,'NL',nl,'NS',ns,'FORMAT',format,' ')
      roundoff=.false.
      if(ns*nl.gt.npix)then
        write(*,*)'Max number of input pixels',npix
        call abend
      endif

c open output
      call xvunit(ounit,'OUT',1,status,' ')
      call xvopen(ounit,status,'U_FORMAT','HALF','OP','WRITE',' ')
      
c read image into memory
      do j=1,nl
        call xvread(inunit,buf1((j-1)*ns+1),status,'LINE',j,' ')
      enddo 

c process data
      do j=1,iter      
        call grow_h(buf1,nl,ns,buf2,t)
        call grow_v(buf2,nl,ns,buf1,t)
      enddo

c write image from memory
      do j=1,nl
        call xvwrit(ounit,buf1((j-1)*ns+1),status,' ')
      enddo
      
      return
      end

c**************************************************************************
      subroutine grow_v(inbuf,nl,ns,obuf,t)
      integer*2 inbuf(ns,nl),obuf(ns,nl),t

      do i=1,ns
        do j=1,nl
          obuf(i,j)=inbuf(i,j)
        enddo
        do j=2,nl-2
          if((inbuf(i,j).ge.inbuf(i,j-1)).and.
     +       (inbuf(i,j)-inbuf(i,j-1).le.t))then          
            if((inbuf(i,j).lt.inbuf(i,j+1)).and.
     +         (inbuf(i,j+1).lt.inbuf(i,j+2)))then
               obuf(i,j)=(inbuf(i,j)+inbuf(i,j+1))/2
               obuf(i,j+1)=(inbuf(i,j+1)+inbuf(i,j+2))/2
            endif
          endif
          if((inbuf(i,j).le.inbuf(i,j-1)).and.
     +       (inbuf(i,j-1)-inbuf(i,j).le.t))then          
            if((inbuf(i,j).gt.inbuf(i,j+1)).and.
     +         (inbuf(i,j+1).gt.inbuf(i,j+2)))then
               obuf(i,j)=(inbuf(i,j)+inbuf(i,j+1))/2
               obuf(i,j+1)=(inbuf(i,j+1)+inbuf(i,j+2))/2
            endif
          endif
        enddo
        do j=1,nl
          inbuf(i,j)=obuf(i,j)
        enddo
        do j=nl-1,3,-1
          if((inbuf(i,j).ge.inbuf(i,j+1)).and.
     +       (inbuf(i,j)-inbuf(i,j+1).le.t))then          
            if((inbuf(i,j).lt.inbuf(i,j-1)).and.
     +         (inbuf(i,j-1).lt.inbuf(i,j-2)))then
               obuf(i,j)=(inbuf(i,j)+inbuf(i,j-1))/2
               obuf(i,j-1)=(inbuf(i,j-1)+inbuf(i,j-2))/2
            endif
          endif
          if((inbuf(i,j).le.inbuf(i,j+1)).and.
     +       (inbuf(i,j+1)-inbuf(i,j).le.t))then          
            if((inbuf(i,j).gt.inbuf(i,j-1)).and.
     +         (inbuf(i,j-1).gt.inbuf(i,j-2)))then
               obuf(i,j)=(inbuf(i,j)+inbuf(i,j-1))/2
               obuf(i,j-1)=(inbuf(i,j-1)+inbuf(i,j-2))/2
            endif
          endif
        enddo
      enddo

      return
      end        
      
c**************************************************************************
      subroutine grow_h(inbuf,nl,ns,obuf,t)
      integer*2 inbuf(ns,nl),obuf(ns,nl),t

      do j=1,nl
        do i=1,ns
          obuf(i,j)=inbuf(i,j)
        enddo
        do i=2,ns-2
          if((inbuf(i,j).ge.inbuf(i-1,j)).and.
     +       (inbuf(i,j)-inbuf(i-1,j).le.t))then          
            if((inbuf(i,j).lt.inbuf(i+1,j)).and.
     +         (inbuf(i+1,j).lt.inbuf(i+2,j)))then
               obuf(i,j)=(inbuf(i,j)+inbuf(i+1,j))/2
               obuf(i+1,j)=(inbuf(i+1,j)+inbuf(i+2,j))/2
            endif
          endif
          if((inbuf(i,j).le.inbuf(i-1,j)).and.
     +       (inbuf(i-1,j)-inbuf(i,j).le.t))then          
            if((inbuf(i,j).gt.inbuf(i+1,j)).and.
     +         (inbuf(i+1,j).gt.inbuf(i+2,j)))then
               obuf(i,j)=(inbuf(i,j)+inbuf(i+1,j))/2
               obuf(i+1,j)=(inbuf(i+1,j)+inbuf(i+2,j))/2
            endif
          endif
        enddo
        do i=1,ns
          inbuf(i,j)=obuf(i,j)
        enddo
        do i=ns-1,3,-1
          if((inbuf(i,j).ge.inbuf(i+1,j)).and.
     +       (inbuf(i,j)-inbuf(i+1,j).le.t))then          
            if((inbuf(i,j).lt.inbuf(i-1,j)).and.
     +         (inbuf(i-1,j).lt.inbuf(i-2,j)))then
               obuf(i,j)=(inbuf(i,j)+inbuf(i-1,j))/2
               obuf(i-1,j)=(inbuf(i-1,j)+inbuf(i-2,j))/2
            endif
          endif
          if((inbuf(i,j).le.inbuf(i+1,j)).and.
     +       (inbuf(i+1,j)-inbuf(i,j).le.t))then          
            if((inbuf(i,j).gt.inbuf(i-1,j)).and.
     +         (inbuf(i-1,j).gt.inbuf(i-2,j)))then
               obuf(i,j)=(inbuf(i,j)+inbuf(i-1,j))/2
               obuf(i-1,j)=(inbuf(i-1,j)+inbuf(i-2,j))/2
            endif
          endif
        enddo
      enddo

      return
      end        
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create jpegfix.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM jpegfix

   To Create the build file give the command:

		$ vimake jpegfix			(VMS)
   or
		% vimake jpegfix			(Unix)


************************************************************************/


#define PROGRAM	jpegfix
#define R2LIB

#define MODULE_LIST jpegfix.f

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
$ create jpegfix.pdf
process help=*
PARM INP TYPE=STRING COUNT=1
PARM OUT TYPE=STRING COUNT=1
PARM ITER TYPE=INTEGER COUNT=(0:1) VALID=(1:1000) DEFAULT=6
PARM THRESH TYPE=INTEGER COUNT=(0:1) VALID=(0:1000) DEFAULT=2
END-PROC

.TITLE
VICAR program jpegfix

.HELP
PURPOSE:
Jpegfix smoothes edges in images in order to reduce the patchiness introduced
by severe jpeg data compression. It will not affect small features.

EXECUTION:
jpegfix inp=in out=out

NOTES:
1. The image must reside in memory (limited to 2,000,000 pixels).
2. The image is converted to HALF format internally.
3. The algorith is effective with data compressions above about 30:1.
4. The algorithm is ineffective on ICT compression images such as used by 
   Galileo.

METHOD:
Pixels are compared in groups of 4 pixels along lines and columns independently.
If 4 consecutive pixels are labelled I1,I2,I3,I4 then if:
I2 > I1 and I2-I1 <= THRESH and I3 > I2 and I4 > I3 then:
I2=(I2+I3)/2 and I3=(I3+I4)/2

This logic is performed both positively (as above) and negatively along
 both rows and columns in both directions for ITER times each.

HISTORY:
2-1-99  J Lorre. 
COGNIZANT PROGRAMMER:  Jean Lorre

.LEVEL1
.VARI INP
 Input image.

.VARI OUT
output image.

.VARI ITER
Number of iterations.

.VARI THRESH
Threshold


.LEVEL2
.VARI INP
 Input image

.VARI OUT
output image with jpeg contours smoothed.

.VARI ITER
Number of iterations for the algorithm.

.VARI THRESH
Threshold. See method.

.END
$ Return
$!#############################################################################
$Test_File:
$ create tstjpegfix.pdf
procedure
refgbl $echo
body
let $echo="yes"
!
gen out=a.img nl=4 ns=4 linc=50 sinc=50
size inp=a.img out=b.img zoom=50 'noin
boxflt2 inp=b.img out=a.img nlw=3 nsw=3
qsar inp=a.img out=b.img area=(125,125,1,5,100,75,75,5,1,200)
jpegfix inp=b.img out=a.img
xvd inp=a.img
xvd inp=b.img
!
end-proc
$ Return
$!#############################################################################
