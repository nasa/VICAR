ccccccccccccccccccc
c Compute the mean dn of the pixels along the edges of the window...

      subroutine compute_dedge(W,ia,ja,dedge)
      implicit none
c Inputs...
      integer ia,ja
      integer*2 W(-ia:ia,-ja:ja)
c Output...
      real*4 dedge	!mean dn of the edge pixels
c Local variables...
      integer i,j
      integer dn,sumdn,n

      sumdn = 0
      n = 0

      do i=-ia,ia	!pixels along the top and bottom edges of W
         dn = W(i,-ja)			!dn along top edge
         if (dn.ge.0) then
            sumdn = sumdn + dn
            n = n + 1
         endif
         dn = W(i,ja)			!dn along bottom edge
         if (dn.ge.0) then
            sumdn = sumdn + dn
            n = n + 1
         endif
      enddo

      do j=-ja+1,ja-1	!pixels along the left and right edges of W
         dn = w(-ia,j)			!dn along left edge
         if (dn.ge.0) then
            sumdn = sumdn + dn
            n = n + 1
         endif
         dn = W(ia,j)			!dn along right edge
         if (dn.ge.0) then
            sumdn = sumdn + dn
            n = n + 1
         endif
      enddo

      if (n.gt.0) then
         dedge = float(sumdn)/n
      else
         dedge = -999
      endif
      return
      end
