C*****************************************************************************
C
C Subroutine DGELG
C
C Solves a general system of simultaneous linear equations
C 11/18/1992 - M. O'Shaughnessy		Ported DGELG to UNIX
C*****************************************************************************
      subroutine dgelg( r, a, m, n, eps, ifail)

c Work arrays are ipvt and info. The input parameter EPS is NOT used 
c in this subroutine.
      double precision r(m,n),a(m,m)
      integer n_eqns
      parameter (n_eqns = 100)
      integer i,m,n,ifail,eps,ipvt(n_eqns),info
      character*80 msg

      ifail = 0
      if (m .gt. n_eqns) then
         write (msg,10) m
10    format(' DGELG> Internal array needs to be enlarged >= to ',i4)
         call xvmessage(msg,' ')
         call abend
      end if

c..get the lu decomposition of matrix a.

      call dgefa(a,m,m,ipvt,info)

c..if not singular, then solve for each right hand side

      if ( info .ne. 0 )  then
         call xvmessage('DGELG> Matrix is singular',' ')
         ifail = 1
      else
         do 100 i = 1, n
            call dgesld(a,m,m,ipvt,r(1,i))
100      continue
      end if

      return
      end
