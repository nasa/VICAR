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
