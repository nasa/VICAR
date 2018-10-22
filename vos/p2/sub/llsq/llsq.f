c*****************************************************************************
c Subroutine LLSQ - Calculates the least squares solution using the 
c                   math77 routine shfti.
c
c 12/7/1992 - M. O'Shaughnessy - Ported LLSQ to UNIX.
c*****************************************************************************
      subroutine llsq(a,b,m,n,l,x,ipiv,eps,ier,aux)

      real*4    a(m,n),b(m,l),x(n,l),aux(n,2),eps
      integer*4 m,n,l,ier,ipiv(n)

c local arrays and variables.

      integer*4 rn_dim
      parameter (rn_dim = 500)          ! max number of right hand sides
                                        ! = max value for l.
      real*4    rnorm(rn_dim)
      integer*4 krank,i,j

c==================================================================

      if (l .gt. rn_dim) then
	call xvmessage('LLSQ> local arrays need to be enlarged!', ' ')
        call abend
      end if

c  call math77 routine to solve least squares problem

      if ( m .ge. n ) then
        call shfti(a,m,m,n,b,m,l,eps,krank,rnorm,aux(1,1),
     .             aux(1,2), ipiv )
        if (krank .eq. 0)  then
          ier = - 1
        else if (krank .lt. n)  then
          ier = krank
        else
          ier = 0         ! error unless pseudo-rank = n.
          do j = 1, l     ! now copy answers to output array.
            do i = 1, n
              x(i,j) = b(i,j)
            end do
          end do
        end if

      else
        ier = -2        ! since shfti uses b array for both input and output,
                        ! there is no convenient way i see to handle the
                        ! case of n gt m with the llsq calling sequence.
        call xvmessage('LLSQ> column number must be <= row number',' ')
      end if

      return
      end
