cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Get the reseau mark type...

      subroutine get_rtype(rtype)
      implicit none
      integer rtype(202)	!reseau mark type

      integer k

      do k=1,202
         rtype(k) = 0		!0=interior mark
      enddo

c Set the four corners...
      rtype(1) = 1
      rtype(12) = 2
      rtype(190) = 3
      rtype(201) = 4

c Set the top edge...
      do k=2,11
         rtype(k) = 5
      enddo

c Set the left edge...
      do k=47,152,15
         rtype(k) = 6
      enddo
      rtype(167) = 6

c Set the bottom edge...
      do k=191,200
         rtype(k) = 7
      enddo

c Set the right edge...
      do k=50,155,15
         rtype(k) = 8
      enddo
      rtype(178) = 8
      return
      end
