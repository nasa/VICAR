C*****************************************************************************
C Unit test program TRFT2.F for subroutine RFT2
C input image (gen'd) is real*4.
C
C Ported to UNIX 6/3/1993
C*****************************************************************************
        include 'VICMAIN_FOR'
        subroutine main44

        dimension a(16,18)
        integer*4 n,m,iunit,stat,status,mp2
        real*4    s

C FORTRAN-callable
        call xvmessage('**** FORTRAN-callable RFT2 ****',' ')
        m = 16
	n = 16
        call xvunit(iunit,'inp',1,stat,' ')
        call xvopen(iunit,stat,' ')
        do i=1,m
           call xvread(iunit,a(1,i),stat,'line',i,'nsamps',n,' ')
        end do
     
c       ***********************************************
c       forward transform..
c       Resulting transform contained in lines m=1 to 18

        call xvmessage('before forward transform:',' ')
	mp2 = m + 2
	do 110 i=1,m
110	call prnt(7,n,a(1,i),' ')

	call rft2(a,m,n,1,status)
        if (status.ne.1) then
          if (status.eq.1) goto 3
          if (status.eq.2) goto 4
          if (status.eq.3) goto 5
        endif

        call xvmessage('after forward transform:',' ')
	do 130 i=1,mp2
130	call prnt(7,n,a(1,i),' ')

c       ***********************************************
c       the inverse transform..

	call rft2(a,m,n,-1,status)
        if (status.ne.1) then
          if (status.eq.1) goto 3
          if (status.eq.2) goto 4
          if (status.eq.3) goto 5
        endif

        call xvmessage('after reverse transform:',' ')       
	s = 2*m*n

	do 160 i=1,m
	call divv(7,n,s,a(1,i),0,1)
160	call prnt(7,n,a(1,i),' ')

      call xvclose(iunit,stat,' ')

c ****************************************************************
c C-callable
        call xvmessage('**** C-callable RFT2 ****',' ')
        m = 16
	n = 16
        call xvunit(iunit,'inp',1,stat,' ')
        call xvopen(iunit,stat,' ')
        do i=1,m
           call xvread(iunit,a(1,i),stat,'line',i,'nsamps',n,' ')
        end do
     
c       ***********************************************
c       forward transform..
c       Resulting transform contained in lines m=1 to 18

        call xvmessage('before forward transform:',' ')
	mp2 = m + 2
	do 210 i=1,m
210	call prnt(7,n,a(1,i),' ')

	call tzrft2(a,m,n,1,status)

        if (status.ne.1) then
          if (status.eq.1) goto 3
          if (status.eq.2) goto 4
          if (status.eq.3) goto 5
        endif

        call xvmessage('after forward transform:',' ')
	do 230 i=1,mp2
230	call prnt(7,n,a(1,i),' ')

c       ***********************************************
c       the inverse transform..

	call tzrft2(a,m,n,-1,status)

        if (status.ne.1) then
          if (status.eq.1) goto 3
          if (status.eq.2) goto 4
          if (status.eq.3) goto 5
        endif

        call xvmessage('after reverse transform:',' ')       
	s = 2*m*n

	do 260 i=1,m
	call divv(7,n,s,a(1,i),0,1)
260	call prnt(7,n,a(1,i),' ')

      call xvclose(iunit,stat,' ')
      return
c**************************************************************
3	call xvmessage('m or n has too large a prime factor',' ')
	return
4	call xvmessage(
     +    'product of square-free factors of m or n too big',' ')
	return
5	call xvmessage('number of lines must be even',' ')
	return

      end
