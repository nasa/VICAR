c test pgm for function BATCH

      include 'VICMAIN_FOR'

      SUBROUTINE main44

      implicit integer (a-z)

      i = batch()
      if (i.eq.0) then
         call xvmessage(' Job is interactive', ' ')
      elseif (i.eq.1) then
         call xvmessage(' Job is batch', ' ')
      elseif (i.eq.2) then
         call xvmessage(' Job is asynchronous', ' ')
      else
         call xvmessage(' BATCH could not decide', ' ')
      endif

      return
      end
