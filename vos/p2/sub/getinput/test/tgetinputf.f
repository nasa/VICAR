C Fortran routine to test Fortran bridge to subroutines pollinput() and
C waitinput()

       SUBROUTINE tgetinputf()
       integer inpchar, waitforinput, pollinput
       integer done

       call xvmessage('Testing GETINPUT Fortran interface.', ' ')
       call xvmessage(' ', ' ')

       call xvmessage('Testing wait mode input handling.', ' ')
       call xvmessage('Type "q" to quit.', ' ')
       call xvmessage(' ', ' ')

       done = 0
       do while (done .eq. 0)
          inpchar = waitforinput(1)
          call printchar(inpchar)
          if (inpchar .eq. 113 .or. inpchar .eq. 81) then
             done = 1
          endif
       enddo
       inpchar = waitforinput(0)

       call xvmessage(' ', ' ')
       call xvmessage('Testing polling mode input handling.', ' ')
       call xvmessage('Type "q" to quit.', ' ')
       call xvmessage(' ', ' ')

       done = 0
       do while (done .eq. 0)
          inpchar = pollinput(1)
          if (inpchar .ne. 0) then
             call printchar(inpchar)
          endif
          if (inpchar .eq. 113 .or. inpchar .eq. 81) then
             done = 1
          endif
       enddo
       inpchar = pollinput(0)

       call xvmessage(' ', ' ')
       call xvmessage('Done testing GETINPUT Fortran interface.' , ' ')
       call xvmessage(' ', ' ')

       return
       end
