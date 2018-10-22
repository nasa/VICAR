ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Compute the mean offsets from the nominals.

      subroutine compute_offsets(l,s,lnom,snom,rtype,e,f)
      implicit none
c Inputs...
      real*4 l(3,202),s(3,202)
      real*4 lnom(202),snom(202)	
      integer rtype(202)

c Outputs...
      real e,f			!mean offsets from nominals

c Local variables...
      integer n
      real dl,ds
      real sumdl,sumds
      real err,maxerr,ethresh
      integer k,kmax
      logical reject(202)
ccc      character*80 msg
ccc  101 format('Reseau',i4,' not used in fit. eps=',
ccc     &	f5.1,' pixels')

      ethresh = 3	!set the distance error threshold to 3 pixels

c First, we reject all marks on the margins or that are not found
c from the computations...

      do k=1,202
         if (rtype(k).ne.0 .or.l(1,k).eq.-999) then
            reject(k) = .true.
         else
            reject(k) = .false.
         endif
      enddo

c Since the actual mark is usually the first candidate, we compute an
c estimate for e and f using the locations of the first candidates only..

      sumds = 0
      sumdl = 0
      n = 0

      do k=1,202
         if (.not.reject(k)) then
            sumdl = sumdl + (l(1,k) - lnom(k))	!line displacements
            sumds = sumds + (s(1,k) - snom(k))	!sample displacements
            n = n + 1
         endif
      enddo

      if (n.eq.0) goto 990		!punt on fourth down

c This is our best estimate for e and f so far...

      e = sumds/n
      f = sumdl/n

c We now iterate thru a loop, rejecting the mark furthest from its
c expected value and obtaining a better esimate for e and f...

c Top of elimination loop...

   20 continue

c Find the mark with the largest residue from the model...
c	s = snom + e
c	l = lnom + f

      maxerr = 0

      do 40 k=1,202
      if (reject(k)) goto 40		!skip rejects
      dl = l(1,k) - lnom(k) - f		!line displacement
      ds = s(1,k) - snom(k) - e		!sample displacement
      err = dl**2 + ds**2
      if (err.gt.maxerr) then
         maxerr = err
         kmax = k
      endif
   40 continue

      maxerr = sqrt(maxerr)

      if (maxerr.lt.ethresh) return	!return if error is less than threshold
      reject(kmax) = .true.		!reject kmax
ccc      write(msg,101) kmax,maxerr
ccc      call xvmessage(msg,0)
      n = n - 1
      if (n.eq.0) goto 990		!this should never happen.
      sumdl = sumdl - (l(1,kmax)-lnom(kmax))
      sumds = sumds - (s(1,kmax)-snom(kmax))
      e = sumds/n
      f = sumdl/n
      goto 20

c Punt onf fourth down...
  990 call xvmessage('Warning: error solving for e and f',0)
      call xvmessage('Values set to 0',0)
      e = 0
      f = 0
      return
      end
