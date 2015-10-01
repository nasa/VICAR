$!****************************************************************************
$!
$! Build proc for MIPL module maskmos
$! VPACK Version 1.8, Wednesday, February 12, 1997, 17:08:18
$!
$! Execute by entering:		$ @maskmos
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
$ write sys$output "*** module maskmos ***"
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
$ write sys$output "Invalid argument given to maskmos.com file -- ", primary
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
$   if F$SEARCH("maskmos.imake") .nes. ""
$   then
$      vimake maskmos
$      purge maskmos.bld
$   else
$      if F$SEARCH("maskmos.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake maskmos
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @maskmos.bld "STD"
$   else
$      @maskmos.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create maskmos.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack maskmos.com -
	-s maskmos.f -
	-i maskmos.imake -
	-p maskmos.pdf -
	-t tstmaskmos.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create maskmos.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C
C     PROGRAM maskmos
C
      INCLUDE 'VICMAIN_FOR'

      SUBROUTINE MAIN44

      parameter (nsmax=1502,nlmax=1502) ! image storage array size
      implicit integer(a-z)
      integer*2 buf(nsmax,nlmax),code
      character*80 MSG

      call xvpcnt('INP',nids)

      do n=1,nids
C
C  OPEN INPUT & OUTPUT FOR SEQUENTIAL I/O
C
      CALL XVSELPI(n)  ! copy corresponding label
      CALL XVUNIT(INUNIT,'INP',n,STATUS,' ')
      CALL XVOPEN(INUNIT,STATUS,'OPEN_ACT','SA','IO_ACT','SA',
     +            'U_FORMAT','HALF',' ')
      CALL XVGET(INUNIT,STATUS,'NL',nl,'NS',ns,' ')
      if(nl.gt.nlmax-2)then
         write(msg,100) nlmax
100      format(' Max # lines permitted= ',i5)
         call xvmessage(msg,' ')
         call xvmessage(' Can recompile with new nlmax value',' ')
         call abend
      endif
      if(ns.gt.nsmax-2)then
         write(msg,101) nsmax
101      format(' Max # samples permitted= ',i5)
         call xvmessage(msg,' ')
         call xvmessage(' Can recompile with new nsmax value',' ')
         call abend
      endif
      CALL XVUNIT(OUTUNIT,'OUT',n,STATUS,' ')
      CALL XVOPEN(OUTUNIT,STATUS,'OP','WRITE','OPEN_ACT','SA',
     +          'IO_ACT','SA','U_FORMAT','HALF','O_FORMAT','HALF',
     +          'U_NL',nl,'U_NS',ns,' ')

c load picture into memory leaving a zero border 1 pixel wide
      do line=1,nl
         CALL XVREAD(INUNIT,BUF(2,line+1),STATUS,'LINE',LINE,' ')
      enddo

c zero border
      do i=1,ns+2
         buf(i,1)=0
         buf(i,nl+2)=0
      enddo
      do j=1,nl+2
         buf(1,j)=0
         buf(ns+2,j)=0
      enddo

c set all non zero pixels to 32767
      do j=2,nl+1
         do i=2,ns+1
            if(buf(i,j).ne.0) buf(i,j)=32767
         enddo
      enddo

c set all border points to one.
c border points are non zero with a zero adjacent pixel.
      do j=2,nl+1
         do i=2,ns+1
            if(buf(i,j).ne.0) then
               if(buf(i-1,j-1).eq.0) goto 10
               if(buf(i  ,j-1).eq.0) goto 10
               if(buf(i+1,j-1).eq.0) goto 10
               if(buf(i-1,j  ).eq.0) goto 10
               if(buf(i  ,j  ).eq.0) goto 10
               if(buf(i+1,j  ).eq.0) goto 10
               if(buf(i-1,j+1).eq.0) goto 10
               if(buf(i  ,j+1).eq.0) goto 10
               if(buf(i+1,j+1).eq.0) goto 10
               goto 20
10             buf(i,j)=1
20             continue
            endif
         enddo
      enddo

c grow a region from the border incrementing 1 each time.
      code=0
      ltop=2
      lbot=nl+1
      sleft=2
      sright=ns+1
30    continue
      count=0
      code=code+1
      lmin=nl
      lmax=1
      smin=ns
      smax=1
      do j=ltop,lbot
         do i=sleft,sright
            if(buf(i,j).eq.code) then
               if(buf(i-1,j-1).eq.32767) then
                  buf(i-1,j-1)=code+1
                  count=count+1
                  if(i-1.lt.smin) smin=i-1
                  if(i-1.gt.smax) smax=i-1
                  if(j-1.lt.lmin) lmin=j-1
                  if(j-1.gt.lmax) lmax=j-1
               endif
               if(buf(i  ,j-1).eq.32767) then
                  buf(i  ,j-1)=code+1
                  count=count+1
                  if(i  .lt.smin) smin=i
                  if(i  .gt.smax) smax=i
                  if(j-1.lt.lmin) lmin=j-1
                  if(j-1.gt.lmax) lmax=j-1
               endif
               if(buf(i+1,j-1).eq.32767) then
                  buf(i+1,j-1)=code+1
                  count=count+1
                  if(i+1.lt.smin) smin=i+1
                  if(i+1.gt.smax) smax=i+1
                  if(j-1.lt.lmin) lmin=j-1
                  if(j-1.gt.lmax) lmax=j-1
               endif
               if(buf(i-1,j  ).eq.32767) then
                  buf(i-1,j  )=code+1
                  count=count+1
                  if(i-1.lt.smin) smin=i-1
                  if(i-1.gt.smax) smax=i-1
                  if(j  .lt.lmin) lmin=j
                  if(j  .gt.lmax) lmax=j
               endif
               if(buf(i  ,j  ).eq.32767) then
                  buf(i  ,j  )=code+1
                  count=count+1
                  if(i  .lt.smin) smin=i
                  if(i  .gt.smax) smax=i
                  if(j  .lt.lmin) lmin=j
                  if(j  .gt.lmax) lmax=j
               endif
               if(buf(i+1,j  ).eq.32767) then
                  buf(i+1,j  )=code+1
                  count=count+1
                  if(i+1.lt.smin) smin=i+1
                  if(i+1.gt.smax) smax=i+1
                  if(j  .lt.lmin) lmin=j
                  if(j  .gt.lmax) lmax=j
               endif
               if(buf(i-1,j+1).eq.32767) then
                  buf(i-1,j+1)=code+1
                  count=count+1
                  if(i-1.lt.smin) smin=i-1
                  if(i-1.gt.smax) smax=i-1
                  if(j+1.lt.lmin) lmin=j+1
                  if(j+1.gt.lmax) lmax=j+1
               endif
               if(buf(i  ,j+1).eq.32767) then
                  buf(i  ,j+1)=code+1
                  count=count+1
                  if(i  .lt.smin) smin=i
                  if(i  .gt.smax) smax=i
                  if(j+1.lt.lmin) lmin=j+1
                  if(j+1.gt.lmax) lmax=j+1
               endif
               if(buf(i+1,j+1).eq.32767) then
                  buf(i+1,j+1)=code+1
                  count=count+1
                  if(i+1.lt.smin) smin=i+1
                  if(i+1.gt.smax) smax=i+1
                  if(j+1.lt.lmin) lmin=j+1
                  if(j+1.gt.lmax) lmax=j+1
               endif
            endif
         enddo
      enddo
      ltop=lmin
      lbot=lmax
      sleft=smin
      sright=smax
      if(count.gt.0) goto 30

c load data into output.
      do line=1,nl
         CALL XVWRIT(OUTUNIT,BUF(2,line+1),STATUS,'LINE',LINE,' ')
      enddo
C
C  CLOSE DATA SETS
C
      CALL XVCLOSE(INUNIT,STATUS,'CLOS_ACT','FREE',' ')
      CALL XVCLOSE(OUTUNIT,STATUS,'CLOS_ACT','FREE',' ')

      enddo ! end picture loop
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create maskmos.imake
#define  PROGRAM   maskmos

#define MODULE_LIST maskmos.f

#define MAIN_LANG_FORTRAN
#define R2LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create maskmos.pdf
process help=*
PARM INP	TYPE = STRING   COUNT=(1:100)
PARM OUT	TYPE = STRING 	COUNT=(1:100)
!
!# parm inp(3-100) hints=default
!# parm out(3-100) hints=noconnection
!
END-PROC
.TITLE
VICAR program maskmos

.HELP
PURPOSE:
Converts map projected images into masks for input to NEWMOS.

EXECUTION:

maskmos may be executed in the following manner:

		maskmos INP=(A1,A2,...An) OUT=(B1,B2,...Bn)

.PAGE
OPERATION:

Maskmos creates masks from input projected images. Later they will be used
by Newmos to mosaic images in the "smooth" option.
Provide to Maskmos the same set of images that would be input to Newmos.
Maskmos will create another set of images.  All these images, original
 and mask images, will then be given to Newmos in the 'smooth mode.

Maskmos converts an input image into a HALF format image with the 
following properties:
1. Zero dn input values will be zero dn on output.
2. Points on the edge of the input projected data or on the edge of
   the input frame if the projected data reaches the picture border
   are set to one. These are Border points.
3. All pixels which are not zero will be set to a dn value which is 
   their distance from the Border points.

ASSUMPTIONS:
(1) That MAP3 has been run on the input images, generating
    a zero border .
(2) That the image can be fit into memory. Current buffer limits
    permit up to 1500 by 1500 pixel inputs. This is easily changed.
(3) Newmos uses the image labels to compute offfsets. Make sure that
    Maskmos gets projected inputs so it can preserve the label.

EXAMPLE:
(1)
         map3 inp=p1 out=a1 ...
         map3 inp=p2 out=a2 ...
         map3 inp=p3 out=a3 ...
         maskmos inp=(a1,a2,a3) out=(b1,b2,b3)
         newmos inp=(a1,a2,a3,b1,b2,b3) out=m 'smooth
(2)
     An input image which looks like this:
           0 0 0 0 0 0 0 0 0 0
           0 0 0 9 9 9 9 9 0 0
           0 0 0 9 9 9 9 9 0 0
           0 0 0 9 9 9 9 9 0 0
           0 0 0 9 9 9 9 9 0 0
           0 0 0 9 9 9 9 9 0 0

     would be converted to this:
           0 0 0 0 0 0 0 0 0 0
           0 0 0 1 1 1 1 1 0 0
           0 0 0 1 2 2 2 1 0 0
           0 0 0 1 2 3 2 1 0 0
           0 0 0 1 2 2 2 1 0 0
           0 0 0 1 1 1 1 1 0 0

         
.PAGE
HISTORY:

ORIGINALLY WRITTEN BY: J Lorre 2/30/93
COGNIZANT PROGRAMMER:  Jean Lorre

05.06.98.....RRP....Changed calls to qprint to xvmessages.
.LEVEL1
.VARI INP
STRING-input datasets.
.VARI OUT
STRING-output datasets.

.LEVEL2
.VARI INP
Up to 100 input images.
.VARI OUT
The same number or outputs as inputs.
These are the masks of the corresponding inputs.
All masks are half format.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstmaskmos.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage = "none"
let _onfail="continue"
let $echo="yes"
!This is a test file for maskmos
gen out=a.img nl=15 ns=15 ival=0 linc=0 sinc=0
qsar inp=a.img out=c.img area=(3,3,5,5,100)
qsar inp=a.img out=d.img area=(9,9,5,5,100)
maskmos inp=(c.img,d.img) out=(a.img,b.img)
list a.img
list b.img
end-proc
$ Return
$!#############################################################################
