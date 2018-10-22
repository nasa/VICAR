ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Print the array v(202) in reseau grid format.

      subroutine p_stats(v,scale)
      implicit none
c Inputs...
      real*4 v(202)		!v may be rho,sigma,dn,dc, or eps
      real*4 scale		!1,10,or 100
c Local variables...
      integer k			!mark index
      integer loop,i,ival,ichar
      character*132 msg

c This loop prints two consecutive rows of reseau marks so that after
c completing the 12th loop, all 202 values are printed in reseau grid
c format. Since there are only 23 rows in the grid, only one row is printed
c on the last looop.

c We start at the top...
      k = 1				!reseau mark 1

      do 100 loop=1,12	

c     ...Print the first row.
c     ...This row starts at mark 1,24,47,62,77,92,107,122,137,152,167,or 190

      do ichar=1,132
         msg(ichar:ichar) = ' '		!blank out the message
      enddo

      ichar = 6
      if (loop.le.2 .or. loop.ge.11) then
c					!Rows that start at 1,24,167,190
         do i=1,12			!have 12 marks.
            if (v(k).eq.-999) then
               ival = -99
            else
               ival = scale*v(k) + 0.5
            endif
            write (msg(ichar-5:ichar),'(i6)') ival
            k = k + 1
            ichar = ichar + 6
         enddo
      else				!All other rows have 4 marks.
         do i=1,12
            if (i.le.2 .or. i.ge.11) then	!left and right two marks.
               if (v(k).eq.-999) then
                  ival = -99
               else
                  ival = scale*v(k) + 0.5
               endif
               write (msg(ichar-5:ichar),'(i6)') ival
               k = k + 1
            endif
            ichar = ichar + 6
         enddo
         if (loop.eq.3) then	!mark 202 is on row 3
            if (v(202).eq.-999) then
               ival = -99
            else
               ival = scale*v(202) + 0.5
            endif
            ichar = 54
            write (msg(ichar-5:ichar),'(i6)') ival
         endif
      endif
      call xvmessage(msg(2:132),' ')

c     ...Print the second row.
c     ...This row starts at mark 13,36,51,66,81,96,111,126,141,156,or 179

      if (loop.eq.12) goto 100		!the last loop has no second row.

      do ichar=1,132
         msg(ichar:ichar) = ' '		!blank out the print line
      enddo

      ichar = 9
      do i=1,11
         if (v(k).eq.-999) then
            ival = -99
         else
            ival = scale*v(k) + 0.5
         endif
         write (msg(ichar-5:ichar),'(i6)') ival
         k = k + 1
         ichar = ichar + 6
      enddo
      call xvmessage(msg(2:132),' ')

  100 continue

      return
      end
