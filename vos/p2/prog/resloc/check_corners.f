cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Check the corner marks.

      subroutine check_corners(ceps,cl,cs)
      implicit none
c Input...
      real*4 ceps(202)
c Updated...
      real*4 cl(202),cs(202)

      if (cl(1).eq.cl(13) .and. cs(1).eq.cs(13)) then
         call xvmessage('mark 1 same as mark 13',0)
         if (ceps(1).gt.ceps(13)) then
            cl(1) = -999
            cs(1) = -999
            call xvmessage('mark 1 rejected',0)
         else
            cl(13) = -999
            cs(13) = -999
            call xvmessage('mark 13 rejected',0)
         endif
      endif

      if (cl(12).eq.cl(23) .and. cs(12).eq.cs(23)) then
         call xvmessage('mark 12 same as mark 23',0)
         if (ceps(12).gt.ceps(23)) then
            cl(12) = -999
            cs(12) = -999
            call xvmessage('mark 12 rejected',0)
         else
            cl(23) = -999
            cs(23) = -999
            call xvmessage('mark 23 rejected',0)
         endif
      endif

      if (cl(190).eq.cl(179) .and. cs(190).eq.cs(179)) then
         call xvmessage('mark 190 same as mark 179',0)
         if (ceps(179).gt.ceps(190)) then
            cl(179) = -999
            cs(179) = -999
            call xvmessage('mark 179 rejected',0)
         else
            cl(190) = -999
            cs(190) = -999
            call xvmessage('mark 190 rejected',0)
         endif
      endif

      if (cl(201).eq.cl(189) .and. cs(201).eq.cs(189)) then
         call xvmessage('mark 201 same as mark 189',0)
         if (ceps(189).gt.ceps(201)) then
            cl(189) = -999
            cs(189) = -999
            call xvmessage('mark 189 rejected',0)
         else
            cl(201) = -999
            cs(201) = -999
            call xvmessage('mark 201 rejected',0)
         endif
      endif

      return
      end

