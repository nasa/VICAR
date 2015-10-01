$!****************************************************************************
$!
$! Build proc for MIPL module despike
$! VPACK Version 1.9, Tuesday, October 11, 2005, 16:49:40
$!
$! Execute by entering:		$ @despike
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
$ write sys$output "*** module despike ***"
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
$ write sys$output "Invalid argument given to despike.com file -- ", primary
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
$   if F$SEARCH("despike.imake") .nes. ""
$   then
$      vimake despike
$      purge despike.bld
$   else
$      if F$SEARCH("despike.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake despike
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @despike.bld "STD"
$   else
$      @despike.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create despike.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack despike.com -mixed -
	-s despike.f -
	-i despike.imake -
	-p despike.pdf -
	-t tstdespike.pdf tstdespike.log_solos tstdespike.log_linux
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create despike.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c
c program rdespike
c
      include 'VICMAIN_FOR'
      subroutine main44
      implicit none

      integer*4 iunit,ounit,def,count,status,nl,ns,nb
      integer*4 line,i,j
      integer*4 jx(-1:2),jsave
      integer*4 band

      integer*4 maxns
      parameter(maxns=20000)		!maximum number of samples per line
      real*4 obuf(0:20000)		!output despiked image line

      common/c1/ibuf,mbuf,flag
      real*4 ibuf(0:20000,-1:2)	!4 consecutive image lines              
      real*4 mbuf(0:20000,-1:2)	!3 consecutive median lines              
      logical*1 flag(-1:20000,-1:2)	!=.true. if pixel is a spike

      common/c2/scale,tol
      real*4 scale,tol

      common/c3/nspikes,n1,posonly
      integer*4 nspikes,n1
      logical*4 posonly, xvptst

      integer*4 lineout, bandout
      CHARACTER*3  ORGIN

      call xvmessage('DESPIKE version 11-Oct-2005',' ')

      call xvparm('SCALE',scale,count,def,1)
      call xvparm('TOL',tol,count,def,1)
      posonly = xvptst('POSONLY')

c     ....open raw image
      call xvunit(iunit,'INP',1,status,' ')
      call xvopen(iunit,status,'U_FORMAT','REAL',
     $		'OPEN_ACT','SA','IO_ACT','SA',' ')
      call xvget(iunit,status,'NL',nl,'NS',ns,'NB',nb,' ')

c     Check organization of image, prohibit BIP
      CALL XVGET(iunit,status,'ORG',ORGIN, ' ')
      IF (ORGIN.EQ.'BIP') CALL MABEND(
     +  'BIP files not supported, use program TRAN to convert to BSQ')

      if (ns.gt.maxns) goto 998
c     ....open output
      call xvunit(ounit,'OUT',1,status,' ')
      call xvopen(ounit,status,'U_FORMAT','REAL',
     $		'OPEN_ACT','SA','IO_ACT','SA','OP','WRITE',' ')

c  for 3-D (cube) files, need an outer loop:
      bandout = 0
      do band=1,nb
      bandout = bandout + 1
      lineout = 0

c     ...The despike algorithm operates on 3x3 image areas, with the
c     ...central pixel being modified if it is a spike.  Since we compute
c     ...the median for each pixel in this area, an additional image line
c     ...is needed.

c     ...The image goes from line 0 to nl-1 and sample 0 to ns-1
c     ...The image is surrounded by a 1-pixel margin of invalid pixels.
c     ...Initialize the flag buffer for the first 4 lines:
      do i=-1,ns
         flag(i,-1) = .true.		!Top margin of invalid pixels
      enddo
      do j=0,2				!For the bottom three lines
         flag(-1,j) = .true.		!the left-most and right-most
         flag(ns,j) = .true.		!pixels are invalid.
      enddo

c     ...Indices to 4-line rotating image buffer
      do j=-1,2
         jx(j) = j
      enddo

c     ....Read lines 0 and 1
      call xvread(iunit,ibuf(0,0),status,' ')
      call xvread(iunit,ibuf(0,1),status,' ')
      call median(ibuf,mbuf(0,0),jx(-1),0,nl,ns)	!median for line=0
      n1 = 0			!number of pixels pass 1st gate
      nspikes = 0		!number of spikes found

c     ....line loop
      do 100 line=0,nl-2
      lineout = lineout + 1
      if (line.lt.nl-2) call xvread(iunit,ibuf(0,jx(2)),status,' ')
      call median3(ibuf,mbuf(0,jx(1)),jx(0),ns)  ! 0,1,2 -> 1
      call find_spike2(ibuf,mbuf,obuf,flag,jx(-1),nl,ns,line)!-1,0,1 -> 0
c      call xvwrit(ounit,obuf,status,' ')
      call xvwrit(ounit,obuf,status,'LINE',lineout,'BAND',bandout,' ')
      jsave = jx(-1)
      do j=-1,1
         jx(j) = jx(j+1)
      enddo
      jx(2) = jsave
  100 continue
      lineout = lineout + 1
      call median(ibuf,mbuf(0,jx(1)),jx(0),nl-1,nl,ns)  ! 0,1,2 -> 1
      call find_spike2(ibuf,mbuf,obuf,flag,jx(-1),nl,ns,line)!-1,0,1 -> 0
c      call xvwrit(ounit,obuf,status,' ')
      call xvwrit(ounit,obuf,status,'LINE',lineout,'BAND',bandout,' ')
      call prnt(4,1,n1,'pixels pass first gate=.')
      call prnt(4,1,nspikes,'spikes removed=.')

      enddo	! end of outer loop over bands
      return

  998 call xvmessage('***Maximum number of samples exceeded',' ')
      call abend
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Given a nxn pixel area, determine if the central pixel is bad.
c
      subroutine median(ibuf,mbuf,jx,line,nl,ns)
      implicit none
      real*4 ibuf(0:20000,-1:2)     !4 consecutive image lines              
      real*4 mbuf(0:20000)	!output median
      integer*4 jx(-1:1)	!index to previous, current, and next lines
      integer*4 line		!current image line (0 to nl-1)
      integer*4 nl,ns		!number of lines and samples

      integer*4 i,j,s,n,dns(9),nlw,midpt
      integer*4 ibeg,iend,jbeg,jend,jj

c     ...normally, all three lines are present: -1,0,+1
      jbeg = -1
      jend = +1
c     ...adjust for lines at the top and bottom of the image
      if (line.eq.0) jbeg=0
      if (line.eq.nl-1) jend=0
      nlw = jend - jbeg + 1	!height of window (number of lines)
      midpt = (3*nlw+1)/2

c     ...compute median for first sample of line
      n = 0
      do 5 j=jbeg,jend
      jj = jx(j)
      do 5 i=0,1
      n = n + 1
      dns(n) = ibuf(i,jj)
    5 continue
      call ssort(dns,1,n)	!sort the DN values
      mbuf(0) = dns(nlw)

      ibeg = -1
      iend = +1
c     ....loop through the samples in middle of line
      do 10 s=1,ns-2
      n = 0
      do 8 j=jbeg,jend
      jj = jx(j)
      do 8 i=ibeg,iend
      n = n + 1
      dns(n) = ibuf(s+i,jj)
    8 continue
      call ssort(dns,1,n)	!sort the DN values
      mbuf(s) = dns(midpt)
   10 continue

c     ...compute median for last sample of line
      n = 0
      do 18 j=jbeg,jend
      jj = jx(j)
      do 18 i=ns-2,ns-1
      n = n + 1
      dns(n) = ibuf(i,jj)
   18 continue
      call ssort(dns,1,n)	!sort the DN values
      mbuf(ns-1) = dns(nlw)
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Compute the 3x3 median for a line.  All 3 lines must exist.
c
      subroutine median3(ibuf,mbuf,jx,ns)
      implicit none
      real*4 ibuf(0:20000,-1:2)	!4 consecutive image lines              
      real*4 mbuf(0:20000)	!output median
      integer*4 jx(-1:1)	!index to previous, current, and next lines
      integer*4 ns		!number of samples

      integer*4 i,j,s,n,i1,i2,i3
      integer*4 ibeg,iend,jbeg,jend,jj,jm1,j0,jp1
      real*4 d(9),d1,d2,d3
      real*4 c(9)		!sorted array

      jbeg = -1
      jend = +1
      jm1 = jx(-1)
      j0 = jx(0)
      jp1 = jx(1)

c     ...compute median for first sample of line
      n = 1
      do 5 i=0,1
      do 5 j=jbeg,jend
      d(n) = ibuf(i,jx(j))
      c(n) = d(n)
      n = n + 1
    5 continue
      call ssort(d,1,3)
      call ssort(d(4),1,3)
      call ssort(c,1,6)
      mbuf(0) = c(3)

      ibeg = -1
      iend = +1
c     ....loop through the samples in middle of line
      do 100 s=2,ns-1
      d1 = ibuf(s,jm1)		!update array d by inserting right-most column
      d2 = ibuf(s,j0)
      d3 = ibuf(s,jp1)
      if (d1.gt.d2) then
         if (d2.gt.d3) then	!d3 < d2 < d1
            d(n) = d3
            d(n+1) = d2
            d(n+2) = d1
         else
            if (d1.gt.d3) then	!d2 < d3 < d1
               d(n) = d2
               d(n+1) = d3
               d(n+2) = d1
            else		!d2 < d1 < d3
               d(n) = d2
               d(n+1) = d3
               d(n+2) = d1
            endif
         endif
      else
         if (d2.lt.d3) then	!d1 < d2 < d3
            d(n) = d1
            d(n+1) = d2
            d(n+2) = d3
         else
            if (d1.lt.d3) then	!d1 < d3 < d2
               d(n) = d1
               d(n+1) = d3
               d(n+2) = d2
            else		!d3 < d1 < d2
               d(n) = d3
               d(n+1) = d1
               d(n+2) = d2
            endif
         endif
      endif

c     ....Sort array d to compute median.  Note that only the 5 lowest
c     ....values need to be determined.
      i = 1		!index to output array
      i1 = 1		!index to first three elements of d (sorted)
      i2 = 4		!index to middle three elements of d (sorted)
      i3 = 7		!index to last three elements of d (sorted)

   10 if (d(i1).gt.d(i2)) goto 30
   20 if (d(i1).gt.d(i3)) then
         c(i) = d(i3)		!d3<d1<d2     
         i = i + 1
         if (i3.ge.9) goto 70
         i3 = i3 + 1
         goto 20
      else
         c(i) = d(i1)		!d1<d2 & d1<d3
         i = i + 1
         if (i1.ge.3) goto 50
         i1 = i1 + 1
         goto 10
      endif

   30 if (d(i2).gt.d(i3)) then
         c(i) = d(i3)		!d3<d2<d1
         i = i + 1
         if (i3.ge.9) goto 70
         i3 = i3 + 1
         goto 30
      else
         c(i) = d(i2)		!d2<d3<d1     
         i = i + 1
         if (i2.ge.6) goto 60
         i2 = i2 + 1
         goto 10
      endif

c     ....here if i1 is expended
   50 if (d(i2).gt.d(i3)) then
         c(i) = d(i3)
         i3 = i3 + 1
      else
         c(i) = d(i2)
         i2 = i2 + 1
      endif
      if (i.ge.5) goto 95
      i = i + 1
      goto 50
           
c     ....here if i2 is expended
   60 if (d(i1).gt.d(i3)) then
         c(i) = d(i3)
         i3 = i3 + 1
      else
         c(i) = d(i1)
         i1 = i1 + 1
      endif
      if (i.ge.5) goto 95
      i = i + 1
      goto 60

c     ....here if i3 is expended
   70 if (d(i1).gt.d(i2)) then
         c(i) = d(i2)
         i2 = i2 + 1
      else
         c(i) = d(i1)
         i1 = i1 + 1
      endif
      if (i.ge.5) goto 95
      i = i + 1
      goto 70
           
   95 mbuf(s-1) = c(5)
      n = mod(n+3,9)
  100 continue

c     ...compute median for last sample of line
      n = 1
      do 110 j=jbeg,jend
      jj = jx(j)
      do 110 i=ns-2,ns-1
      c(n) = ibuf(i,jj)
      n = n + 1
  110 continue
      call ssort(c,1,6)	!sort the DN values
      mbuf(ns-1) = c(3)
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Given the medians of an nxn pixel area, determine if the central pixel
c is bad.
c
      subroutine find_spike2(ibuf,mbuf,obuf,flag,jx,nl,ns,line)
      implicit none
      real*4 ibuf(0:20000,-1:2)     !4 consecutive image lines              
      real*4 mbuf(0:20000,-1:2)     !3 consecutive median lines            `
      real*4 obuf(0:20000)           !output despiked image line
      logical*1 flag(-1:20000,-1:2)     !=.true. if pixel is a spike
      integer*4 jx(-1:1)	!index to previous, current, and next lines
      integer*4 nl,ns		!number of lines and samples
      integer*4 line		!current image line

      integer*4 i,j,jj,n,s,dn
      integer*4 ibeg,iend,jbeg,jend,nlw,nsw,narea
      integer*4 j0,jm1,jp1,ndif
      real*4 dif0,dif,sum0,summ1,sump1,sum
      real*4 err,maxerr,avg,sdif
      real*4 x,y,x2,y2,xy,d,xd,yd
      real*4 dc,det,c
      logical jflag

      common/c2/scale,tol
      real*4 scale,tol

      common/c3/nspikes,n1,posonly
      integer*4 nspikes,n1
      logical*4 posonly

      j0 = jx(0)	!index to current image line
      jm1 = jx(-1)
      jp1 = jx(+1)
      jbeg = -1
      jend = +1
      if (line.eq.0) jbeg=0
      if (line.eq.nl-1) jend=0
      nlw = jend - jbeg + 1	!height of filter window
      ibeg = 0
      iend = +1
      nsw = 2			!width of filter window

      sum0 = 0.0
      summ1 = 0.0
      sump1 = 0.0
      do j=jbeg,jend
         jj = jx(j)
         sum0 = sum0 + mbuf(0,jj)
      enddo
      sum = sum0

c     ....loop through each sample of current line
      do 100 s=0,ns-1
      obuf(s) = ibuf(s,j0)
      if (s.eq.ns-1) then
         iend = 0
         nsw = 2
      else
         sump1 = 0
         i = s + 1
         do j=jbeg,jend
            sump1 = sump1 + mbuf(i,jx(j))
         enddo
         sum = sum + sump1
      endif
      narea = nlw*nsw

c     ....reset flags in 4x4 area in bottom-right corner
      do i=0,iend
         flag(s+i,j0) = .false.
      enddo
      jflag = .false.
      if (jend.eq.0) jflag=.true.
      do i=0,iend
         flag(s+i,jp1) = jflag	!bottom line
      enddo

      avg = sum/narea
      sdif = 0
      do 6 j=jbeg,jend
      jj = jx(j)
      do 6 i=ibeg,iend
    6 sdif = sdif + abs(mbuf(s+i,jj)-avg)
      maxerr = scale*sdif/narea + tol

c     ....Flag all suspicious pixels in lower-right quadrant
      if (abs(ibuf(s,jp1)-mbuf(s,jp1)).gt.maxerr) flag(s,jp1)=.true.
      dif0 = abs(ibuf(s,j0)-mbuf(s,j0))
      if (dif0.le.maxerr) goto 90	!skip if central pixel is good
      flag(s,j0)=.true.
      n1 = n1 + 1
      if (iend.eq.0) goto 20
      i = s + 1
      do jj=0,jend
         j = jx(jj)
         if (abs(ibuf(i,j)-mbuf(i,j)).gt.maxerr) flag(i,j)=.true.
      enddo
c     ....fit the valid data to the surface d = a*i + b*j + c
   20 n = 0
      x = 0.
      y = 0.
      x2 = 0.
      y2 = 0.
      xy = 0.
      d = 0.
      xd = 0.
      yd = 0.

      do 35 j=jbeg,jend
      jj = jx(j)
      do 35 i=ibeg,iend
      if (.not.flag(s+i,jj)) then
         n = n + 1
         x = x + i
         y = y + j
         x2 = x2 + i**2
         y2 = y2 + j**2
         xy = xy + i*j
         dn = ibuf(s+i,jj)	!DN value at pixel (i,j)
         d = d + dn
         xd = xd + i*dn
         yd = yd + j*dn
      endif
   35 continue

      if (n.eq.0) goto 90	!bail out if no hope

c     ....Use Kramer's rule to solve linear equation
      if (n.gt.3) then
         det = x2*(y2*n-y*y)  - xy*(xy*n-x*y)  + x*(xy*y-x*y2)
         dc  = x2*(y2*d-y*yd) - xy*(xy*d-y*xd) + x*(xy*yd-xd*y2)
         c = dc/det
      else
         c = d/n	!if not enough points, use mean
      endif

c     ....compute a threshold to determine if error is significant
      dif = 0.
      ndif = 0

      if (.not.flag(s,jm1)) then
         if (.not.flag(s-1,jm1)) then
            dif = dif + abs(ibuf(s-1,jm1)-ibuf(s,jm1))
            ndif = ndif + 1
         endif
         if (.not.flag(s+1,jm1)) then
            dif = dif + abs(ibuf(s+1,jm1)-ibuf(s,jm1))
            ndif = ndif + 1
         endif
      endif

      if (.not.flag(s+1,j0)) then
         if (.not.flag(s+1,jm1)) then
            dif = dif + abs(ibuf(s+1,jm1)-ibuf(s+1,j0))
            ndif = ndif + 1
         endif
         if (.not.flag(s+1,jp1)) then
            dif = dif + abs(ibuf(s+1,jp1)-ibuf(s+1,j0))
            ndif = ndif + 1
         endif
      endif

      if (.not.flag(s,jp1)) then
         if (.not.flag(s+1,jp1)) then
            dif = dif + abs(ibuf(s+1,jp1)-ibuf(s,jp1))
            ndif = ndif + 1
         endif
         if (.not.flag(s-1,jp1)) then
            dif = dif + abs(ibuf(s-1,jp1)-ibuf(s,jp1))
            ndif = ndif + 1
         endif
      endif

      if (.not.flag(s-1,j0)) then
         if (.not.flag(s-1,jp1)) then
            dif = dif + abs(ibuf(s-1,jp1)-ibuf(s-1,j0))
            ndif = ndif + 1
         endif
         if (.not.flag(s-1,jm1)) then
            dif = dif + abs(ibuf(s-1,jm1)-ibuf(s-1,j0))
            ndif = ndif + 1
         endif
      endif

      if (ndif.gt.0) maxerr=(scale*dif)/ndif + tol
      if (posonly) then
	err = ibuf(s,j0)-c
      else
	err = abs(ibuf(s,j0)-c)
      endif
      if (err.gt.maxerr) then
         obuf(s) = nint(c)
         nspikes = nspikes + 1
      else
         flag(s,j0)=.false.
      endif
   90 ibeg = -1
      nsw = 3
      sum = sum - summ1
      summ1 = sum0
      sum0 = sump1
  100 continue

      return
      end
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create despike.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM despike
   To Create the build file give the command:
		$ vimake despike			(VMS)
   or
		% vimake despike			(Unix)

************************************************************************/

#define PROGRAM	despike
#define R2LIB

#define MODULE_LIST despike.f
#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define R2LIB
#define LIB_FORTRAN
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_MATH77

/*#define DEBUG	/* comment out on delivery */

/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create despike.pdf
process help=*
PARM INP   TYPE=STRING COUNT=1
PARM OUT   TYPE=STRING COUNT=1
PARM SCALE TYPE=REAL COUNT=(0:1) DEFAULT=3.
PARM TOL   TYPE=REAL COUNT=(0:1) DEFAULT=3.
PARM POSONLY TYPE=KEYWORD VALID=POSONLY COUNT=(0:1) DEFAULT=--
!# annot function="Image Cosmetics"
!# annot keywords=("Image Restoration","Noise Reduction")
END-PROC

.TITLE
Remove single-pixel errors

.HELP
PURPOSE:
DESPIKE identifies and removes single-pixel errors (e.g. telemetry bit errors)
from images.

EXECUTION:

   DESPIKE  INP  OUT
or DESPIKE  INP  OUT  SCALE=3  TOL=3

where

   INP and OUT are the input and output images 
   SCALE and TOL are optional scale and offset parameters for controlling the
   DN threshold.

The program converts the input to floating-point (real*4) and converts the 
results back to the format of the input.  Therefore, all VICAR formats are
accepted, but some loss of precision may occur with longword integer or
double precision formats.

REFERENCE:

  "A comparison of image despiking algorithms", Gary Yagi, IOM, June 1, 1999.

.page
OPERATION:

DESPIKE identifies single-pixel errors by comparing the DN value of a pixel
with that of its eight surrounding neighbors.  If the pixel differs from its
neighbors by some specifiable threshold (see SCALE and TOL parameters) the
pixel is removed by interpolation.

DESPIKE will actually remove errors affecting multiple pixels as long as no
more than 3 noisy pixels are included in any 3x3 area.  Thus, small radiation
noise events and single line or column dropouts may be successfully removed.

See also programs REMNOISE and REMRAY.

.page
METHOD:

The algorithm consists of the following steps:

  1) Use a median filter to identify all "suspicious" pixels in the 3x3 area.
  2) If the central pixel is one of the suspects, use only the reliable pixels
     in the area to determine if it is a spike.
  3) If the central pixel is determined to be a spike, replace it by fitting
     all the reliable pixels to a surface.

In the following diagram, let Do represent the DN value of the pixel being
tested, and D1, D2, D3, ..., D8 represent the DN values of the pixels in the
3x3 area surrounding it:


		D1  D2  D3			M1  M2  M3
			      median filter
		D8  Do  D4    ------------->	M8  Mo  M4

		D7  D6  D5			M7  M6  M5

A 3X3 median filter (see program MEDIAN) is applied (to the entire image),
resulting in median values Mo, M1, M2, ..., M8 for each of the nine pixels in
the area.  The median filter is used to provide an estimate of what the
"correct" value of each pixel would be if there were no noise.  A pixel Di
is considered suspicious if it differs from its median by an amount
significantly more than the mean difference in the area:

			 	 8
				---
			     S  \
		  |Di-Mi| > --- /  |Dj-Mj| + To			(1)
			     9  ---
                                j=0

where S and To are scale and offset constants specified via the SCALE and TOL
parameters.

If, after the above screening, the central pixel is one of the suspects, it is
subjected to a second test, based only on reliable pixels within the area:

			|Do - Eo| > S x A + To			(2)
where
    Eo is an estimate of the "correct" value of Do,
    A is a scene activity measure,
    S and To are scale and offset constants specified via the SCALE and TOL
      parameters.
    
The estimator Eo is computed by fitting the 3x3 DN surface to a plane.  The
scene activity measure A is the mean absolute difference between adjacent pixels
along the perimeter.  Only valid pixels are used in these calculation.  If the
central pixel is a spike, it is replaced by Eo.

.page
USING THE SCALE AND TOL PARAMETERS:

As described above, the threshold for determining whether a pixel is a spike
is computed from some linear function of the local scene activity.  The scale
and offset terms (S and To) of this linear function are specified via the
SCALE and TOL parameters.

In general, the useful range for S is between 1.5 and 3.  The useful range for
To is between 2 and 6.  Setting To=0 or 1 is likely to cause large numbers of
valid pixels to be destroyed (this is similar to setting the lawn mower blades
too low and chewing up large chunks of root and dirt).  The following values
are recommended:

	SCALE=3  TOL=3  for bit-error rates of 10**-3 or less
	SCALE=2  TOL=2  for bit-error rates of 10**-2 or greater

Note that it is more fruitful to adjust SCALE rather than the TOL.  To get
some feel for what effect a particular bit error rate has on an image,
experiment with program ADDNOISE.

The effectiveness of the program in removing noise may be determined by
applying the following test on a noise-free image:

   addnoise clean noisy rate=1000	  !add 10**-3 BER of noise to image
   despike  noisy   d   scale=3  tol=3    !despike the noisy image
   f2 (clean,d) diff func="abs(in1-in2)"  !compare despiked and clean images
   hist  diff  'nohist			  !and print residual noise

In the test, we add noise to the clean image (Bit-Error-Rate=10**-3) and
attempt to remove the noise using despike.  By subtracting the despiked image
from the original image, we obtain a measure of the residual noise, i.e. the
noise that DESPIKE failed to remove.  By varying the SCALE and TOL parameters,
we can determine optimal values which minimize the residual noise.

The following data was obtained by applying the above test to an image of the
moon after adding 4.7 DN of noise:

	                residual noise (DN)
	      To   s=1.5    s=2    s=2.5    s=3
	       1  1.4160  1.0983  0.8762  0.8196
	       2  1.1476  0.9032  0.7785  0.7605
	       3  1.0062  0.8215  0.7463  0.7620
	       4  0.9190  0.7865 *0.7380  0.7602
	       5  0.8608  0.7674  0.7459  0.7779

In this example, the minimum occurs when SCALE=2.5 and TOL=4.  In general,
lowering the value of S or To causes more noise spikes to be detected and
removed, but causes more valid pixels to be destroyed as well.  The minumum
occurs approximately at that point where lowering either value any further
causes more valid pixels to be destroyed than noise spikes.

To answer the question of how much harm DESPIKE does to an image, we can
apply it to the clean image (assumed to be noise-free) at various combinations of
S and To.  The following data was obtained by applying DESPIKE to the Earth:

		    Valid pixels destroyed
		To  S=1.5   S=2  S=2.5  S=3
		1   7070   2161   598   232
		2   3075    769   162    48
		3   1498    329    66    18
		4    771    162    32    12
		5    394     75    24     7

Note that the above data is skewed by the fact that the original image
(assumed to be noise-free) contains radiation noise (Galileo was traveling
through the Van Allen radiation belt at the time).


.page
PROGRAMMING NOTES:

The following comments are addressed to the programmer charged with maintaining
the program:

The median of each 3x3 area is computed by sorting the 9 pixels and selecting
the 5th lowest value.  The sort algorithm uses the fact that it is only
necessary to find the 5 lowest values to determine the median.  The resulting
algorithm is twice as fast as a conventional sort routine.

Because the filter window scans the image from left-to-right and from top-to-
bottom, at any given position of the window only the four pixels in the
lower right quadrant must be screened for suspicious pixels:

			D1  D2  D3

			D8  Do?	D4?

			D7  D6? D5?

The remaining pixels in the area have already received a more accurate
screening at previous positions of the filter window.

Note that the same SCALE and TOL values are used in two separate tests (see
equations 1 and 2 above).  Originally, independent SCALE and TOL values were
used for each test.  However, this provided only marginal improvement in
performance at the expense of greatly increasing the difficulty of determining
optimal values.

The current algorithm is not optimal.  However, further marginal improvements in
accuracy was only achieved at the expense of greatly increased computational
cost.  It is a matter of how hard you are willing to squeeze a lemon in order
to get out that last drop.


.page
PROGRAM HISTORY:

Written by:  Gary Yagi, June 4, 1999
Cognizant programmer:  Gary Yagi
Revisions: 
  2003-08-14   NTT  Enabled despike for 3D images
  2005-10-11   lwk  converted operations to float, added POSONLY keyword

.LEVEL1
.VARI INP
Input raw image

.VARI OUT
output despike image.

.VARI SCALE
Scale for computing threshold

.VARI TOL
Offset for computing threshold

.VARI POSONLY
Only allow positive spikes.

.LEVEL2
.VARI INP
Input raw image

.VARI OUT
output despike image.

.VARI SCALE
The DESPIKE algorithm consists of two tests.  The first test searches for all
"suspicious" pixels in each 3x3 pixel area by comparing its difference from
its median to the mean difference for all 9 pixels:

			 	 8
				---
			     S  \
		  |Di-Mi| > --- /  |Dj-Mj| + To			(1)
			     9  ---
                                j=0


The second test is applied if the central pixel Do is "suspicious":

			|Do - Eo| > S x A + To			(2)
where
    Eo is an estimate of the "correct" value of Do,
    A is a scene activity measure.

In both tests, the right side of the inequality represents a threshold used
to determine whether the difference is significant.  In both cases, the
threshold is computed as a linear function where the scale and offset constants
are specified via the SCALE and TOL parameters.

See help file for further details, including suggestions for selecting these
parameters.

.VARI TOL
See help for SCALE parameter.

.VARI POSONLY
When this is specified, ony positive spikes are allowed.  A motivation might
be if you know that the source of the noise always increases the DN.

$ Return
$!#############################################################################
$Test_File:
$ create tstdespike.pdf
!Test for program DESPIKE
procedure
refgbl $echo
refgbl $syschar
body
let $echo="yes"
local path type=string init="wms_test_work:[testdata.mipl.gll]"
if ($syschar(1) = "UNIX")
    let path="/project/test_work/testdata/mipl/gll/"
end-if

!	Halfword image test
addnoise &"path"venus2.img n rate=100 seed=9699691 !add 10**-2 BER noise
despike  n  d					!despike with defaults
f2 (&"path"venus.img,d) diff func="abs(in1-in2)"
hist diff 'nohist

!	Byte image test
fit &"path"venus2.img byte perc=0.1 'byte	!convert to byte
addnoise byte n rate=100 seed=9699691		!add 10**-2 BER noise
despike  n  d  scale=2 tol=4			!despike with parameters
f2 (byte,d) diff func="abs(in1-in2)"
hist diff 'nohist

!	Real image test
f2 n nr 'real
despike nr d
f2 (&"path"venus2.img,d) diff func="(in1-in2)" 'real
hist diff nlin=21 lim=(-100 300)

!	test POSONLY keyword:
despike nr d 'posonly
f2 (&"path"venus2.img,d) diff func="(in1-in2)" 'real
hist diff nlin=21 lim=(-100 300)

!	Test 3D capability
gen a3d 100 100 3 BINC=10 LINC=20 SINC=10
addnoise a3d b3d rate=10 seed=9699691
despike b3d c3d
f2 (a3d,c3d) diff func="abs(in1-in2)"
hist diff 'nohist

!	Test other BIL
gen a3d 100 100 3 ORG="BIL"
addnoise a3d b3d rate=20 seed=9699691
despike b3d c3d
f2 (a3d,c3d) diff func="abs(in1-in2)"
hist diff 'nohist

end-proc
$!-----------------------------------------------------------------------------
$ create tstdespike.log_solos
ena-log
tstdespike
local path type=string init="wms_test_work:[testdata.mipl.gll]"
if ($syschar(1) = "UNIX")
    let path="/project/test_work/testdata/mipl/gll/"
end-if
addnoise /project/test_work/testdata/mipl/gll/venus2.img n rate=100 seed=9699691
Beginning VICAR task addnoise
despike  n  d
Beginning VICAR task despike
DESPIKE version 11-Oct-2005
pixels pass first gate=      89319
spikes removed=      57699
f2 (/project/test_work/testdata/mipl/gll/venus.img,d) diff func="abs(in1-in2)"
Beginning VICAR task f2
F2 version 2-04-94
F2 using hash table lookup
FUNCTION EVALUATED 217035 TIMES
hist diff 'nohist
Beginning VICAR task hist
HIST version 17-SEP-03


AVERAGE GRAY LEVEL=14675.18       STANDARD DEVIATION=15808.74       NUMBER ELEMENTS=  640000
MIN. DN=         0
MAX. DN=     32767

fit /project/test_work/testdata/mipl/gll/venus2.img byte perc=0.1 'byte
Beginning VICAR task fit

FIT version 5 August, 2003

     RAW HISTOGRAM STATISTICS...
AVERAGE GRAY LEVEL=   429.898 STANDARD DEVIATION=  2550.827 NUMBER OF ELEMENTS=  640000

EXCLUDED HISTOGRAM STATISTICS...
AVERAGE GRAY LEVEL=   606.208 STANDARD DEVIATION=   810.603 NUMBER OF ELEMENTS=  636619

MINIMUM DN OF      0   SCALED TO     0

MAXIMUM DN OF   2453   SCALED TO   255
FIT task completed
addnoise byte n rate=100 seed=9699691
Beginning VICAR task addnoise
despike  n  d  scale=2 tol=4
Beginning VICAR task despike
DESPIKE version 11-Oct-2005
pixels pass first gate=      32300
spikes removed=      31049
f2 (byte,d) diff func="abs(in1-in2)"
Beginning VICAR task f2
F2 version 2-04-94
F2 using byte table lookup
FUNCTION EVALUATED 65536 TIMES
hist diff 'nohist
Beginning VICAR task hist
HIST version 17-SEP-03


AVERAGE GRAY LEVEL=0.117423       STANDARD DEVIATION=0.734609       NUMBER ELEMENTS=  640000
MIN. DN=         0
MAX. DN=       253

f2 n nr 'real
Beginning VICAR task f2
F2 version 2-04-94
F2 calculating every pixel
FUNCTION EVALUATED 640000 TIMES
despike nr d
Beginning VICAR task despike
DESPIKE version 11-Oct-2005
pixels pass first gate=      35166
spikes removed=      32089
f2 (/project/test_work/testdata/mipl/gll/venus2.img,d) diff func="(in1-in2)" 'real
Beginning VICAR task f2
F2 version 2-04-94
F2 calculating every pixel
FUNCTION EVALUATED 640000 TIMES
hist diff nlin=21 lim=(-100 300)
Beginning VICAR task hist
HIST version 17-SEP-03

< LOW LIMIT    3381
        -20*    225
          0   21687    *****
         20  213719    **************************************************  2
         40  113637    **************************
         60     492
         80   11530    **
        100    7961    *
        120    2160
        140    2535
        160    2622
        180    2854
        200    2678
        220    2217
        240    2409
        260    1095
        280    1815
        300    1765
>HIGH LIMIT  245218    **************************************************  1

AVERAGE GRAY LEVEL=367.2977
STANDARD DEVIATION=2521.069
NUMBER ELEMENTS=  640000
MIN. DN=-32776.0
MAX. DN=2234.000

despike nr d 'posonly
Beginning VICAR task despike
DESPIKE version 11-Oct-2005
pixels pass first gate=      35166
spikes removed=      24891
f2 (/project/test_work/testdata/mipl/gll/venus2.img,d) diff func="(in1-in2)" 'real
Beginning VICAR task f2
F2 version 2-04-94
F2 calculating every pixel
FUNCTION EVALUATED 640000 TIMES
hist diff nlin=21 lim=(-100 300)
Beginning VICAR task hist
HIST version 17-SEP-03

< LOW LIMIT    3381
        -20*    225
          0   21684    *****
         20  213714    **************************************************  2
         40  113633    **************************
         60     499
         80   11519    **
        100    7918    *
        120    2209
        140    2526
        160    2629
        180    2840
        200    2670
        220    2228
        240    2392
        260    1123
        280    1809
        300    1764
>HIGH LIMIT  245237    **************************************************  1

AVERAGE GRAY LEVEL=367.8822
STANDARD DEVIATION=2521.357
NUMBER ELEMENTS=  640000
MIN. DN=-32776.0
MAX. DN=2362.000

gen a3d 100 100 3 BINC=10 LINC=20 SINC=10
Beginning VICAR task gen
GEN Version 6
GEN task completed
addnoise a3d b3d rate=10 seed=9699691
Beginning VICAR task addnoise
despike b3d c3d
Beginning VICAR task despike
DESPIKE version 11-Oct-2005
pixels pass first gate=       1317
spikes removed=        803
pixels pass first gate=          0
spikes removed=          0
pixels pass first gate=          0
spikes removed=          0
f2 (a3d,c3d) diff func="abs(in1-in2)"
Beginning VICAR task f2
F2 version 2-04-94
F2 using hash table lookup
FUNCTION EVALUATED 1749 TIMES
hist diff 'nohist
Beginning VICAR task hist
HIST version 17-SEP-03


AVERAGE GRAY LEVEL=90.40090       STANDARD DEVIATION=81.32819       NUMBER ELEMENTS=   30000
MIN. DN=         0
MAX. DN=       254

gen a3d 100 100 3 ORG="BIL"
Beginning VICAR task gen
GEN Version 6
GEN task completed
addnoise a3d b3d rate=20 seed=9699691
Beginning VICAR task addnoise
despike b3d c3d
Beginning VICAR task despike
DESPIKE version 11-Oct-2005
pixels pass first gate=       2359
spikes removed=       2102
pixels pass first gate=          0
spikes removed=          0
pixels pass first gate=          0
spikes removed=          0
f2 (a3d,c3d) diff func="abs(in1-in2)"
Beginning VICAR task f2
F2 version 2-04-94
F2 using hash table lookup
FUNCTION EVALUATED 5084 TIMES
hist diff 'nohist
Beginning VICAR task hist
HIST version 17-SEP-03


AVERAGE GRAY LEVEL=77.79263       STANDARD DEVIATION=47.62738       NUMBER ELEMENTS=   30000
MIN. DN=         0
MAX. DN=       233

end-proc
exit
slogoff
if ($RUNTYPE = "INTERACTIVE")
  if ($syschar(1) = "VAX_VMS")
  end-if
else
  if ($syschar(1) = "VAX_VMS")
  end-if
end-if
ulogoff
END-PROC
END-PROC
$!-----------------------------------------------------------------------------
$ create tstdespike.log_linux
tstdespike
local path type=string init="wms_test_work:[testdata.mipl.gll]"
if ($syschar(1) = "UNIX")
    let path="/project/test_work/testdata/mipl/gll/"
end-if
addnoise /project/test_work/testdata/mipl/gll/venus2.img n rate=100 seed=9699691
Beginning VICAR task addnoise
despike  n  d
Beginning VICAR task despike
DESPIKE version 11-Oct-2005
pixels pass first gate=      89041
spikes removed=      57557
f2 (/project/test_work/testdata/mipl/gll/venus.img,d) diff func="abs(in1-in2)"
Beginning VICAR task f2
F2 version 2-04-94
F2 using hash table lookup
FUNCTION EVALUATED 217034 TIMES
hist diff 'nohist
Beginning VICAR task hist
HIST version 17-SEP-03


AVERAGE GRAY LEVEL=14675.19       STANDARD DEVIATION=15808.74       NUMBER ELEMENTS=  640000
MIN. DN=         0
MAX. DN=     32767

fit /project/test_work/testdata/mipl/gll/venus2.img byte perc=0.1 'byte
Beginning VICAR task fit

FIT version 5 August, 2003

     RAW HISTOGRAM STATISTICS...
AVERAGE GRAY LEVEL=   429.898 STANDARD DEVIATION=  2550.827 NUMBER OF ELEMENTS=  640000

EXCLUDED HISTOGRAM STATISTICS...
AVERAGE GRAY LEVEL=   606.208 STANDARD DEVIATION=   810.603 NUMBER OF ELEMENTS=  636619

MINIMUM DN OF      0   SCALED TO     0

MAXIMUM DN OF   2453   SCALED TO   255
FIT task completed
addnoise byte n rate=100 seed=9699691
Beginning VICAR task addnoise
despike  n  d  scale=2 tol=4
Beginning VICAR task despike
DESPIKE version 11-Oct-2005
pixels pass first gate=      32353
spikes removed=      31068
f2 (byte,d) diff func="abs(in1-in2)"
Beginning VICAR task f2
F2 version 2-04-94
F2 using byte table lookup
FUNCTION EVALUATED 65536 TIMES
hist diff 'nohist
Beginning VICAR task hist
HIST version 17-SEP-03


AVERAGE GRAY LEVEL=0.117609       STANDARD DEVIATION=0.772524       NUMBER ELEMENTS=  640000
MIN. DN=         0
MAX. DN=       253

f2 n nr 'real
Beginning VICAR task f2
F2 version 2-04-94
F2 calculating every pixel
FUNCTION EVALUATED 640000 TIMES
despike nr d
Beginning VICAR task despike
DESPIKE version 11-Oct-2005
pixels pass first gate=      35209
spikes removed=      32128
f2 (/project/test_work/testdata/mipl/gll/venus2.img,d) diff func="(in1-in2)" 'real
Beginning VICAR task f2
F2 version 2-04-94
F2 calculating every pixel
FUNCTION EVALUATED 640000 TIMES
hist diff nlin=21 lim=(-100 300)
Beginning VICAR task hist
HIST version 17-SEP-03

< LOW LIMIT    3381
        -20*    225
          0   21690    *****
         20  213678    **************************************************  2
         40  113668    **************************
         60     498
         80   11544    **
        100    7953    *
        120    2161
        140    2525
        160    2621
        180    2858
        200    2672
        220    2224
        240    2409
        260    1092
        280    1807
        300    1774
>HIGH LIMIT  245220    **************************************************  1

AVERAGE GRAY LEVEL=367.2969
STANDARD DEVIATION=2521.067
NUMBER ELEMENTS=  640000
MIN. DN=-32776.0
MAX. DN=2234.000

despike nr d 'posonly
Beginning VICAR task despike
DESPIKE version 11-Oct-2005
pixels pass first gate=      35209
spikes removed=      24905
f2 (/project/test_work/testdata/mipl/gll/venus2.img,d) diff func="(in1-in2)" 'real
Beginning VICAR task f2
F2 version 2-04-94
F2 calculating every pixel
FUNCTION EVALUATED 640000 TIMES
hist diff nlin=21 lim=(-100 300)
Beginning VICAR task hist
HIST version 17-SEP-03

< LOW LIMIT    3381
        -20*    225
          0   21688    *****
         20  213677    **************************************************  2
         40  113664    **************************
         60     502
         80   11520    **
        100    7929    *
        120    2200
        140    2516
        160    2629
        180    2845
        200    2668
        220    2229
        240    2393
        260    1116
        280    1806
        300    1770
>HIGH LIMIT  245242    **************************************************  1

AVERAGE GRAY LEVEL=367.8811
STANDARD DEVIATION=2521.356
NUMBER ELEMENTS=  640000
MIN. DN=-32776.0
MAX. DN=2346.000

gen a3d 100 100 3 BINC=10 LINC=20 SINC=10
Beginning VICAR task gen
GEN Version 6
GEN task completed
addnoise a3d b3d rate=10 seed=9699691
Beginning VICAR task addnoise
despike b3d c3d
Beginning VICAR task despike
DESPIKE version 11-Oct-2005
pixels pass first gate=       1317
spikes removed=        803
pixels pass first gate=          0
spikes removed=          0
pixels pass first gate=          0
spikes removed=          0
f2 (a3d,c3d) diff func="abs(in1-in2)"
Beginning VICAR task f2
F2 version 2-04-94
F2 using hash table lookup
FUNCTION EVALUATED 1749 TIMES
hist diff 'nohist
Beginning VICAR task hist
HIST version 17-SEP-03


AVERAGE GRAY LEVEL=90.40090       STANDARD DEVIATION=81.32819       NUMBER ELEMENTS=   30000
MIN. DN=         0
MAX. DN=       254

gen a3d 100 100 3 ORG="BIL"
Beginning VICAR task gen
GEN Version 6
GEN task completed
addnoise a3d b3d rate=20 seed=9699691
Beginning VICAR task addnoise
despike b3d c3d
Beginning VICAR task despike
DESPIKE version 11-Oct-2005
pixels pass first gate=       2357
spikes removed=       2102
pixels pass first gate=          0
spikes removed=          0
pixels pass first gate=          0
spikes removed=          0
f2 (a3d,c3d) diff func="abs(in1-in2)"
Beginning VICAR task f2
F2 version 2-04-94
F2 using hash table lookup
FUNCTION EVALUATED 5084 TIMES
hist diff 'nohist
Beginning VICAR task hist
HIST version 17-SEP-03


AVERAGE GRAY LEVEL=77.79263       STANDARD DEVIATION=47.62738       NUMBER ELEMENTS=   30000
MIN. DN=         0
MAX. DN=       233

end-proc
exit
slogoff
if ($RUNTYPE = "INTERACTIVE")
  if ($syschar(1) = "VAX_VMS")
  end-if
else
  if ($syschar(1) = "VAX_VMS")
  end-if
end-if
ulogoff
END-PROC
END-PROC
$ Return
$!#############################################################################
