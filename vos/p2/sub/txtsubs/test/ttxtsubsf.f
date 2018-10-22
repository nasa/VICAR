C FORTRAN subroutine to test FORTRAN bridge to subroutines in TXTSUBS

      SUBROUTINE ttxtsubsf(text_height, text_length, char_height,
     &     sample, line, scale, dn, angle, text_loc, font, out_msg,
     &     cnt, image_block)

      integer*2 text_height, text_length, char_height
      integer*2 sample, line, text_loc, font, cnt, dn
      real scale, angle
      byte out_msg(cnt), image_block(text_length, text_height)

      integer i, j, lth, flag
      character*80 buf
      logical status, txtfont, txtsize, txtrotate, txtcolor
      logical txtlength, txttext

      do 10 i = 1, text_height
         do 10 j = 1, text_length
            image_block(j, i) = 32
   10 continue

      call xvmessage('Fortran Interface:', ' ')
      status = txtfont(font)
      if (status .ne. 1) then
         write(buf, 101) status
         call xvmessage(buf, ' ')
         goto 999
      endif
      status = txtsize(char_height, scale)
      if (status .ne. 1) then
         write(buf, 102) status
         call xvmessage(buf, ' ')
         goto 999
      endif
      status = txtrotate(angle)
      if (status .ne. 1) then
         write(buf, 103) status
         call xvmessage(buf, ' ')
         goto 999
      endif
      status = txtcolor(dn)
      if (status .ne. 1) then
         write(buf, 104) status
         call xvmessage(buf, ' ')
         goto 999
      endif
      status = txtlength(lth, cnt, out_msg)
      if (status .ne. 1) then
         write(buf, 105) status
         call xvmessage(buf, ' ')
         goto 999
      endif
      status = txttext(image_block, text_height, text_length,
     &     sample, line, text_loc, cnt, out_msg, flag)
      if (status .ne. 1) then
         write(buf, 106) status
         call xvmessage(buf, ' ')
         goto 999
      endif

      do 20 i = 1, text_height
         write(buf, 100) (image_block(j, i), j = 1, text_length)
         call xvmessage(buf, ' ')
   20 continue

  100 format(79a1)
  101 format('Error (', i1, ') reading font file.')
  102 format('Error (', i1, ') setting text size.')
  103 format('Error (', i1, ') setting text angle.')
  104 format('Error (', i1, ') setting text color.')
  105 format('Error (', i1, ') getting length of text string.')
  106 format('Error (', i1, ') writing text to image array.')

  999 continue
      return
      end
