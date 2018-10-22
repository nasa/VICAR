ccccccccccccccccccccccccccccccccccccccccccc
c Return a complete row of triangular grid by copying existing values
c and filling in the gaps.
c From Jean Lorre's subroutine interg.

      subroutine interg(res,os_res,row,is,os)
      implicit none
c Inputs...
      real*4 res(2,202),os_res(2,202)
      integer row
c Outputs...
      real*4 is(2,13),os(2,13)	!IS and OS (line,samp) for entire row.
c Local variables...
      real*4 a(16),b(4)
      integer k,k1,k2,k3,k4
      integer i,j,ind
c     ...left-most and right-most reseau mark on each row.
      integer*2 ltabl(23)/1,13,24,36,47,51,62,66,77,81,92,96,107,111,
     *  122,126,137,141,152,156,167,179,190/
      integer*2 rtabl(23)/12,23,35,46,50,61,65,76,80,91,95,106,110,
     *  121,125,136,140,151,155,166,178,189,201/

      if (row.ne.2*(row/2)) goto 20	!branch if row is odd

c Here if row is even...

c Even rows all begin and end with an x, with no gaps in between.

c   1     2     3     4     5     6     7     8     9    10    11    12
c   x  13    14    15    16    17    18    19    20    21    22    23 x
c  24    25    26    27    28    29    30    31    32    33    34    35

c Copy all 11 existing coordinates to outputs (e.g. 13 to 23).

      k = ltabl(row)		!left-most reseau mark on row
      do j=2,12
         is(1,j) = res(1,k)
         is(2,j) = res(2,k)
         os(1,j) = os_res(1,k)
         os(2,j) = os_res(2,k)
         k = k + 1
      enddo

c Compute pseudo tiepoint x on the left side of the row by fitting
c to the 3 closest neighbors...
c             k1
c             x  k2
c             k3
      k1 = ltabl(row-1)		!left-most mark on row above
      k2 = ltabl(row)		!left-most mark on row
      k3 = ltabl(row+1)		!left-most mark on row below

c In object space, place x midway between k1 and k2...

      os(1,1) = (os_res(1,k1)+os_res(1,k3))/2	!OS line
      os(2,1) = (os_res(2,k1)+os_res(2,k3))/2	!OS samp

c Compute corresponding image space cooordinates is(1,1) and is(2,1) by
c fitting marks k, k1, and k2 to an affine transformation...
      
c   is_line = c0 + c1*os_samp + c2*os_line
c   is_samp = d0 + d1*os_samp + d2*os_line

c Note: matrix A is destroyed by simq, and C is returned in B.

      do 5 i=1,2
      a(1) = 1
      a(2) = 1
      a(3) = 1
      a(4) = os_res(2,k1)	!os samp coordinate of mark k1
      a(5) = os_res(2,k2)	!os samp coordinate of mark k
      a(6) = os_res(2,k3)	!os samp coordinate of mark k2
      a(7) = os_res(1,k1)	!os line coordinate for mark k1
      a(8) = os_res(1,k2)	!os line coordinate for mark k
      a(9) = os_res(1,k3)	!os line coordinate for mark k2
      b(1) = res(i,k1)
      b(2) = res(i,k2)
      b(3) = res(i,k3)
      call simq(a,b,3,ind)
      if (ind.ne.0) call prnt(2,1,k,'***simq err at mark.')
      is(i,1) = b(1) + b(2)*os(2,1) + b(3)*os(1,1)
    5 continue

c Compute pseudo tiepoint on right side of row...
      k1 = rtabl(row-1)		!right-most mark of row above
      k2 = rtabl(row)		!right-most mark on row
      k3 = rtabl(row+1)		!right-most mark on row below

      os(1,13) = (os_res(1,k1)+os_res(1,k3))/2	!os line
      os(2,13) = (os_res(2,k1)+os_res(2,k3))/2	!os samp

      do 8 i=1,2
      a(1) = 1
      a(2) = 1
      a(3) = 1
      a(4) = os_res(2,k1)
      a(5) = os_res(2,k2)
      a(6) = os_res(2,k3)
      a(7) = os_res(1,k1)
      a(8) = os_res(1,k2)
      a(9) = os_res(1,k3)
      b(1) = res(i,k1)
      b(2) = res(i,k2)
      b(3) = res(i,k3)
      call simq(a,b,3,ind)
      if (ind.ne.0) call prnt(2,1,k2,'***simq err at mark.')
      is(i,13) = b(1) + b(2)*os(2,13) + b(3)*os(1,13)
    8 continue
      return

c Here if row is odd...
   20 continue
      if (row.gt.3 .and. row.lt.21) goto 30

c Here if row is 1,3,21 or 23.  For these rows, all 12 coordinates exits:
c   1     2     3     4     5     6     7     8     9    10    11    12

c Copy coordinates to output row...
      k = ltabl(row)		!left-most reseau mark on row
      do j=1,12
         is(1,j) = res(1,k)
         is(2,j) = res(2,k)
         os(1,j) = os_res(1,k)
         os(2,j) = os_res(2,k)
         k = k + 1
      enddo
      return

   30 continue
c For rows 7,...,19, only the first 2 and last 2 marks exist...
c     51    52    53    54    55    56    57    58    59    60    61
c  62    63    x     x     x     x     x     x     x     x     64    65
c     66    67    68    69    70    71    72    73    74    75    76
c Row 5 is similar, except for the existance of mark 202.

c Copy these four marks to output buffers:
      k = ltabl(row)	!left-most mark
      do j=1,2
         is(1,j) = res(1,k)
         is(2,j) = res(2,k)
         os(1,j) = os_res(1,k)
         os(2,j) = os_res(2,k)
         k = k + 1
      enddo

      k = rtabl(row) - 1
      do j=11,12
         is(1,j) = res(1,k)
         is(2,j) = res(2,k)
         os(1,j) = os_res(1,k)
         os(2,j) = os_res(2,k)
         k = k + 1
      enddo
         
c Fill each x in the middle of the row by fitting its 4 neighbors to a 
c bilinear transformation: 

c         k1     k2
c             x
c         k3    k4

      k1 = ltabl(row-1) + 1
      k2 = k1 + 1
      k3 = ltabl(row+1) + 1
      k4 = k3 + 1

      do 40 j=3,10
      os(1,j)=(os_res(1,k1)+os_res(1,k2)+os_res(1,k3)+os_res(1,k4))/4. 
      os(2,j)=(os_res(2,k1)+os_res(2,k2)+os_res(2,k3)+os_res(2,k4))/4.
      do 38 i=1,2
      a(1) = 1.0
      a(2) = 1.0
      a(3) = 1.0
      a(4) = 1.0
      a(5) = os_res(2,k1)
      a(6) = os_res(2,k2)
      a(7) = os_res(2,k3)
      a(8) = os_res(2,k4)
      a(9) = os_res(1,k1)
      a(10) = os_res(1,k2)
      a(11) = os_res(1,k3)
      a(12) = os_res(1,k4)
      a(13) = a(5)*a(9)
      a(14) = a(6)*a(10)
      a(15) = a(7)*a(11)
      a(16) = a(8)*a(12)
      b(1) = res(i,k1)
      b(2) = res(i,k2)
      b(3) = res(i,k3)
      b(4) = res(i,k4)
      call simq(a,b,4,ind)
      if (ind.ne.0) call prnt(2,1,k1,'***simq err at mark.')
      is(i,j) = b(1) + b(2)*os(2,j) + b(3)*os(1,j)
     &	  	+ b(4)*os(2,j)*os(1,j)
   38 continue
      k1 = k1 + 1
      k2 = k2 + 1
      k3 = k3 + 1
      k4 = k4 + 1
   40 continue

c Restore reseau mark 202 which is inadvertently filled by this algorithm...

      if (row.eq.24) then
         is(1,9) = res(1,202)
         is(2,9) = res(2,202)
         os(1,9) = os_res(1,202)
         os(2,9) = os_res(2,202)
      endif
      return
      end
