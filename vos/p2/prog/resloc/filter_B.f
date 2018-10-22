ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Filter B to remove the hot lines along the top edge of the image,
c and the hot columns along the left edge.

      subroutine filter_B(B,ib,jb,lk,sk,rtype)
      implicit none
c Inputs...
      integer ib,jb
      integer*2 B(-ib:ib,-jb:jb)	!B(i,j) = d(sk+i,lk+j)
      integer lk,sk
      integer rtype

c Local variables...
      integer i,j

      if (rtype.eq.1 .or. rtype.eq.2 .or. rtype.eq.5) then
         call filter5(ib,jb,lk,sk,B)
      elseif (rtype.eq.3 .or. rtype.eq.6) then
         call filter6(ib,jb,lk,sk,B)
      endif

c Note: Using filter5 for rtype=1 is an incomplete solution.
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Filter a search area on the top edge.

      subroutine filter5(ib,jb,lk,sk,B)
      implicit none
c Inputs...
      integer ib,jb
      integer lk,sk
c Filtered output...
      integer*2 B(-ib:ib,-jb:jb)        !B(i,j) = d(sk+i,lk+j)
c Local variables...
      integer dn
      real sum,scale
      real*4 mean(4)		        !mean DNs of columns 1,2 and 3
      integer i,j,l,n

c Compute the mean values of lines 1 to 4...

      do 20 l=1,4
      mean(l) = -999
      j = l - lk
      if (j.lt.-jb .or. j.gt.jb) goto 20
      n = 0
      sum = 0

      do 10 i=-ib,ib
      dn = B(i,j)
      if (dn.lt.0) goto 10
      n = n + 1
      sum = sum + dn
   10 continue

      if (n.gt.0) mean(l)=sum/n
   20 continue

c Scale lines 1,2 and 3 so that their new means are the same as mean(4).

      do l=1,3
         if (mean(l).gt.0) then
            j = l - lk
            scale = mean(4)/mean(l)
            do i=-ib,ib
               dn = B(i,j) 
               if (dn.ge.0) B(i,j)=dn*scale
            enddo
         endif
      enddo

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Filter a search area on the left edge.

      subroutine filter6(ib,jb,lk,sk,B)
      implicit none
c Inputs...
      integer ib,jb
      integer lk,sk
c Filtered output...
      integer*2 B(-ib:ib,-jb:jb)        !B(i,j) = d(sk+i,lk+j)
c Local variables...
      integer dn
      real sum,scale
      real*4 mean(3)		!mean DNs of columns 1,2 and 3
      integer s,i,j,n

c Compute the mean values of columns 1,2 and 3...

      do 20 s=1,3
      mean(s) = -999
      i = s - sk
      if (i.lt.-ib .or. i.gt.ib) goto 20
      n = 0
      sum = 0

      do 10 j=-jb,jb
      dn = B(i,j)
      if (dn.lt.0) goto 10
      n = n + 1
      sum = sum + dn
   10 continue

      if (n.gt.0) mean(s)=sum/n
   20 continue

c Scale columns 1 and 2 so that their new means are the same as mean(3).

      do s=1,2
         if (mean(s).gt.0) then
            i = s - sk
            scale = mean(3)/mean(s)
            do j=-jb,jb
               dn = B(i,j) 
               if (dn.ge.0) B(i,j)=dn*scale
            enddo
         endif
      enddo

      return
      end
