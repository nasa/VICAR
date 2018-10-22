cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Correct a line segment of a box by computing the brightness correction at
c each pixel of the segment.
c
      subroutine compute_segment(buf,cmbuf,line,samp,nsamp,numcor)
      implicit none
      integer nsamp		!pixel length of segment
      real*4 buf(nsamp)		!input and output line segment
      integer*2 cmbuf(nsamp)	!classification map for segment
      integer line,samp		!pixel coords at left end of segment
      integer numcor		!number of pixels corrected

      common/pf0/linc,sinc,grid,term,limb,const,maxcor
      integer linc,sinc,grid
      real term,limb,const,maxcor

      common/cm/cmap,class      !classification map unit number and class
      integer cmap,class

      integer j,ind
      real value,rdn,truncate
      real*8 v(3)

      do 10 j=1,nsamp		!loop thru samples of a box
      rdn = buf(j)
      buf(j) = 0.
      if (cmap.gt.0 .and. cmbuf(j).eq.0) goto 10	!skip pixel not in class
      call toplanet(line,samp,v,ind)
      if (ind.ne.1) goto 10		!skip if point off the target
      call phot_sub(v,0.,0.,value)	!compute brightness correction
      value = value*const
      if (value.lt.maxcor) goto 10
      buf(j) = truncate(rdn/value)	!correct the sample
      numcor = numcor+1
   10 samp = samp + 1			!move to next pixel of segment

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Correct a line segment of a box by interpolating between the brightness
c corrections for the left and right ends.
c
      subroutine interp_segment(buf,cmbuf,vall,valr,nsamp,numcor)
      implicit none
      integer nsamp		!pixel length of segment
      real*4 buf(nsamp)		!input and output line segment
      integer*2 cmbuf(nsamp)	!classification map for segment
      real vall,valr		!corrections for left and right ends
      integer numcor		!number of pixels corrected

      common/pf0/linc,sinc,grid,term,limb,const,maxcor
      integer linc,sinc,grid
      real term,limb,const,maxcor

      common/cm/cmap,class      !classification map unit number and class
      integer cmap,class

      integer j
      real value,dval,rdn,truncate

      dval=(valr-vall)/sinc     !delta brightness correction per sample
      value = vall		!brightness correction at current pixel

      do 10 j=1,nsamp		!loop thru samples of a box
      rdn = buf(j)
      buf(j) = 0.
      if (cmap.gt.0 .and. cmbuf(j).eq.0) goto 10	!skip pixel not in class
      if (value.lt.maxcor) goto 10	!skip if too large a correction
      buf(j) = truncate(rdn/value)	!correct the sample
      numcor = numcor + 1
   10 value = value + dval	!update correction for next pixel

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Truncate dn value to size of output format.
c
      function truncate(rdn)
      implicit none
      real truncate,rdn

      common/units/iunit,ounit,maxdn,fmt
      integer iunit,ounit,maxdn
      character*5 fmt

      common/cmax/maxfull
      real maxfull        !largest 32-bit integer (approximate)

      integer idn

      if (fmt.eq.'REAL') then
         truncate = rdn
         return
      endif

      if (rdn.gt.maxfull) rdn=maxfull	!make sure we don't overflow
         
      idn = rdn + 0.5
      if (idn.gt.maxdn) idn=maxdn	!and truncate to output range.
      if (idn.lt.0) idn=0
      truncate = idn
      return
      end
