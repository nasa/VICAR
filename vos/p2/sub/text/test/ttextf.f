C  Subroutine to test the Fortran interface to TEXT

      subroutine ttextf()

      character*4 inbuf(4) /'ABCD', '2468', '[\]^', 'abcd'/
      character*80 msg
      byte outbuf(80)
      integer size, line, inchr, test, dn, i, nbytes

      call xvmessage(' ', ' ')
      call xvmessage('Testing TEXT subroutine Fortran interface.', ' ')
      call xvmessage(' ', ' ')

      inchr = 4
      do i = 1, 80
         outbuf(i) = 32
      enddo

      do test = 1, 4
         do size = 1, 3
            dn = size * 35
            nbytes = size * 6 * inchr
            do line = 0, 6
               call text(inbuf(test), inchr, line, outbuf, size*6, dn)
               do i = 1, nbytes
                  if (outbuf(i) .eq. 0) then
                     outbuf(i) = 32
                  endif
               enddo
               write(msg, 100) (outbuf(i), i = 1, nbytes)
               call xvmessage(msg, ' ')
            enddo
            call xvmessage(' ', ' ')
         enddo
      enddo

      return
 100  format(80a1)
      end
