c*****************************************************************************
c
c	Subroutine RFT2
c
c Brute force utilization of sri's fft and realtr packages
c to get a real, 2-d, in-main transform.
c
c M and N are considered to be the line and sample dimensions
c of the given real array, respectively. Samples are adjacent
c in main store so N is the first fortran dimension.
c
c The real transform is taken in the line direction. This has
c the advantage that the extra space required for the transform
c due to the represented, but zero, sine coefficients of dc
c and the highest frequency is tacked on at the end of the array
c rather than at the end of each line. Thus the dimensions can
c be more or less consistant everywhere. It has the disadvantage
c that the real and imaginary parts of each complex number of the
c transform are not adjacent, but are separated by one line.
c
c       2 june 93  M. O'Shaughnessy    ported to UNIX
c      22 sept 83  ...cca...           convert to vax 
c      15 june 77  ..jek..             initial release
c*****************************************************************************
      subroutine rft2(a,m,n,isn,status)
      integer*4 m,n,isn,status,m2,mp2,n2
      real*4    a(n,m)

c a - the matrix to be transformed. Dimensions: a(n,m+2)
c m - number of lines
c n - number of samples
c isn - flag to do inverse transform.
c status - new parm to replace old calculated label statements
c m2  - half the value of m
c mp2 - m + 2
c n2  - double the value of n
      status = 1 !default status = success
  
      m2 = m/2

c If the dimension isn't even in the real transform direction, return an error.
      if (2*m2 .ne. m) then
         call xvmessage(
     +    'RFT2> dimension of the input matrix is not even!',' ')
         status = -3
         return
      endif

      mp2 = m + 2
      n2  = 2*n
      if (isn .lt. 0) go to 200

c********************
c forward transform..
c********************
      do 120 i=1,n
        call dfft(a(i,1),a(i,2),m2,m2,m2,n2,*950,*960)
        call realtr(a(i,1),a(i,2),m2,n2)
120   continue

      do 140 i=2,mp2,2
        call dfft(a(1,i-1),a(1,i),n,n,n,1,*950,*960)
140   continue

      return

c************************
c the inverse transform..
c************************
200   continue

      do 220 i=2,mp2,2
        call dfft(a(1,i-1),a(1,i),n,n,n,-1,*950,*960)
220   continue

      do 240 i=1,n
        call realtr(a(i,1),a(i,2),m2,-n2)
        call dfft(a(i,1),a(i,2),m2,m2,m2,-n2,*950,*960)
240   continue

      return

c************ Returns *******************************************************
c return (1) if m or n has too large a prime factor..
950   call xvmessage(
     + 'RFT2> a dimension of the input matrix has too large a ' //
     + 'prime factor!',' ')
      status = -1
      return
c
c return (2) if the product of the square-free factors of m or n is too large..
960   call xvmessage(
     + 'RFT2> product of one of the square-free factors of matrix ' //
     + 'dimensions is too large!',' ')
      status = -2
      return
      end
c***end module***************************************************************

