ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Print out the Voyager reseau locations for an image in a format that
c  simulates the positioning of the reseau marks in Voyager images.

      subroutine pmjs(u,n)
      implicit none
      integer n			!1 if u is real*4, 2 if u is real*8
      real*4 u(n,2,202)

      integer i,j,k,ii,jj
      character*132 rbuf
      character*132 qbuf
      character*132 pbuf

      j = 1

      do 10 ii=1,12

      do jj=1,132
         pbuf(jj:jj) = ' '
         qbuf(jj:jj) = ' '
         rbuf(jj:jj) = ' '
      enddo

      k = 7

      if (ii.le.2. .or. ii.ge.11) then
         do i=1,12
            write (rbuf(k-2-2:k-2),'(i3)') j
            write (pbuf(k-5:k),'(f6.1)') u(1,1,j)
            write (qbuf(k-5:k),'(f6.1)') u(1,2,j)
            j = j + 1
            k = k + 11
         enddo
      else
         do i=1,12
            if (i.le.2 .or. i.ge.11) then
               write (rbuf(k-2-2:k-2),'(i3)') j
               write (pbuf(k-5:k),'(f6.1)') u(1,1,j)
               write (qbuf(k-5:k),'(f6.1)') u(1,2,j)
               j = j + 1
            endif
            k = k + 11
         enddo
         if (ii.eq.3) then
            write (rbuf(91:93),'(i3)') 202
            write (pbuf(90:95),'(f6.1)') u(1,1,202)
            write (qbuf(90:95),'(f6.1)') u(1,2,202)
         endif
      endif

      call xvmessage(rbuf(2:132),' ')
      call xvmessage(pbuf(2:132),' ')
      call xvmessage(qbuf(2:132),' ')

      if (ii.lt.12) then
         do jj=1,132
            pbuf(jj:jj) = ' '
            qbuf(jj:jj) = ' '
            rbuf(jj:jj) = ' '
	 enddo
         k = 13

         do i=1,11
            write (rbuf(k-2-2:k-2),'(i3)') j
            write (pbuf(k-5:k),'(f6.1)') u(1,1,j)
            write (qbuf(k-5:k),'(f6.1)') u(1,2,j)
            j = j + 1
            k = k + 11
         enddo

         call xvmessage(rbuf(2:132),' ')
         call xvmessage(pbuf(2:132),' ')
         call xvmessage(qbuf(2:132),' ')
      endif
   10 continue

      return
      end
