C FORTRAN ROUTINE TO TEST FORTRAN BRIDGE TO SUBROUTINE GETHOST

       SUBROUTINE tgethostf()
       character*40 hostname
       character*4  trunchost
       integer status, gethost

       call xvmessage('Testing GETHOST FORTRAN interface', ' ')
       status = gethost(hostname)
       if (status .ne. 1) then
          call xvmessage('   GETHOST cannot determine host name.', ' ')
       else
          call xvmessage('   Full Host Name = '//hostname, ' ')
       endif
       status = gethost(trunchost)
       if (status .ne. 1) then
          call xvmessage('   GETHOST cannot determine host name.', ' ')
       else
          call xvmessage('   Truncated Host Name = '//trunchost, ' ')
       endif

       return
       end
