cccccccccccccccccccccccccccccccccccc
c Estimate the dn contribution of the camera at each reseau mark.

      subroutine compute_dc(l,s,dn,nbr,rtype,dc)
      implicit none
c Inputs...
      real*4 l(3,202),s(3,202)	!line-sample coordinates of candidates
      real*4 dn(3,202)		!dn values of candidates
      integer nbr(8,202)	!8 neighbors of each reseau mark
      integer rtype(202)	!mark type (corner,edge,interior)
c Output...
      real*4 dc(202)		!dn contribution from the camera
c Local variables...
      real*4 ldc(202),sdc(202)	!coordinates where dc was measured
      integer k,k1,k2,ik
      integer i,n,imin
      integer vnk(8)		!up to 8 valid neighbors of k
      real dnmin
      real*4 edck		!estimated value of dc at mark k
      real*4 dcthresh		
      character*80 msg

      dcthresh = 20.

c Set dc equal to the lowest DN among the three candidates...

      do k=1,202
         dnmin = 256
         do i=1,3		!find the min of the 3
            if (dn(i,k).ge.0 .and. dn(i,k).lt.dnmin) then
               dnmin = dn(i,k)
               imin = i
            endif
         enddo
         if (dnmin.lt.256) then
            dc(k) = dnmin
            ldc(k) = l(imin,k)
            sdc(k) = s(imin,k)
         endif
      enddo

c
c Detect and reject any bad values of dc(k)...
c
      do 50 k=1,202

c     ...Estimate what dc(k) should be if consistent with its neighbors.
c     ...First, collect all the valid neighbors of reseau mark k.
      n = 0				!number of valid neighbors

      do 40 i=1,8
      ik = nbr(i,k)			!ith neighbor of k
      if (ik.eq.0) goto 40		!reject if ik does not exist
      if (rtype(ik).ne.0) goto 40	!reject if ik is on the margin
      if (dc(ik).eq.-999) goto 40	!reject if dc(ik) is invalid
      n = n + 1	
      vnk(n) = ik			!add ik to list of valid neighbor
   40 continue

ccc      if (n.ne.0) then
ccc      write(msg,101) k,(vnk(i),i=1,n)
ccc  101 format('k=',i3,' vnk=',8i4)
ccc      call xvmessage(msg,0)
ccc      endif

      if (n.eq.0) then
	 edck = -999
      elseif (n.eq.1) then
         k1 = vnk(1)
         edck = dc(k1)			!if 1 valid neighbor, copy the value
      elseif (n.eq.2) then
         k1 = vnk(1)
         k2 = vnk(2)
         edck = 0.5*(dc(k1)+dc(k2))	!if 2 valid neighbors, take the mean
      elseif (n.eq.3) then
         call interp_3(k,vnk,sdc,ldc,dc,edck)	!if 3, use an affine fit
      elseif (n.ge.4) then
         call interp_4(k,vnk,sdc,ldc,dc,edck)	!if 4 or more, use bilinear fit
      endif
      if (edck.lt.0) edck=0		!this should never happen

      if (rtype(k).ne.0) then
         if (edck.lt.dc(k)) dc(k)=edck	!along margin, use lower value
      elseif (dc(k)-edck.gt.dcthresh) then
         dc(k) = edck	!elsewhere, replace if dc(k) much larger than estimate.
      endif
   50 continue

      return
      end
