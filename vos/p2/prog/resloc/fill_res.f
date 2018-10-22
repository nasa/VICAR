ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Fill in all missing (l,s) coordinates. 
c
      subroutine fill_res(lnom,snom,nbr,rtype,e,f,l,s)
      implicit none
c Inputs...
      real*4 lnom(202),snom(202)  !nominal reseau coordinates
      integer nbr(8,202)        !8 neighbors of each reseau mark
      integer rtype(202)
      real*4 e,f		!global offsets
c Updated...
      real*4 l(202),s(202)
c Local variables...
      logical found(202)	!true if mark k was found
      integer vnk(8)		!valid neighbors of k
      integer i,n
      integer k,k1,k2,kn

c rtype is 1,2,3,4 for the UL,UR,LL and LR corners,
c 5,6,7,8 for the top,left,bottom and right margins,
c and 0 for the interior.

c Remembering which reseau marks were actually found...

      do k=1,202
         found(k) = l(k).ne.-999	!found if not invalid
      enddo

c Fill in the missing marks...

      do 50 k=1,202
      if (l(k).ne.-999) goto 50		!skip if it already exists
      n = 0				!number of valid neighbors

      do 40 i=1,8	!build list of valid neighbors of k...
      kn = nbr(i,k)                     !ith neighbor of k
      if (kn.eq.0) goto 40              !reject if kn does not exist
      if (rtype(kn).ne.0) goto 40       !reject if kn is on margin
      if (.not.found(kn)) goto 40       !reject if kn was not found
      n = n + 1 
      vnk(n) = kn                       !add kn to list of valid neighbor
   40 continue

      if (n.eq.0) then		!if no valid neighbors, use global offsets
         l(k) = lnom(k) + f
         s(k) = snom(k) + e
      elseif (n.eq.1) then	!use the same offsets
         k1 = vnk(1)
         l(k) = lnom(k) + (l(k1)- lnom(k1))
         s(k) = snom(k) + (s(k1)- snom(k1))
      elseif (n.eq.2) then	!use mean offsets
         k1 = vnk(1)
         k2 = vnk(2)
         l(k) = lnom(k) + 0.5*((l(k1)-lnom(k1))+(l(k2)-lnom(k2)))
         s(k) = snom(k) + 0.5*((s(k1)-snom(k1))+(s(k2)-snom(k2)))
      elseif (n.eq.3) then	!interpolate using an affine transformation 
         call interp_3(k,vnk,lnom,snom,l,l(k))
         call interp_3(k,vnk,lnom,snom,s,s(k))
      elseif (n.ge.4) then	!interpolate using a bilinear transformation
         call interp_4(k,vnk,lnom,snom,l,l(k))
         call interp_4(k,vnk,lnom,snom,s,s(k))
      endif
   50 continue

      return
      end
