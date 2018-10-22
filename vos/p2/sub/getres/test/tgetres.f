      INCLUDE 'VICMAIN_FOR'
c
c   Test Program for subroutine GETRES
c
      Subroutine Main44
      Real*4  Rloc(404)
      Integer*4  Camera
C
      Call Xvmessage(' ******  Testing FORTRAN version  ******', ' ')
      Call Xvmessage(' ',' ')
      Call Xvmessage(' ',' ')
c
      Camera = 1
      Call Getres(Rloc, Camera)
      Call Prnt(4, 1, Camera, '  Camera # = .') 
      Call Xvmessage('     *****   Reseau Locations   ***** ',' ')
      Call Prnt(7, 404, Rloc(1), '.')
      Call Xvmessage(' ', ' ')
c
      Camera = 2
      Call Getres(Rloc, Camera)
      Call Prnt(4, 1, Camera, '  Camera # = .') 
      Call Xvmessage('     *****   Reseau Locations   ***** ',' ')
      Call Prnt(7, 404, Rloc(1), '.')
      Call Xvmessage(' ', ' ')
c
      Camera = 3
      Call Getres(Rloc, Camera)
      Call Prnt(4, 1, Camera, '  Camera # = .') 
      Call Xvmessage('     *****   Reseau Locations   ***** ',' ')
      Call Prnt(7, 404, Rloc(1), '.')
      Call Xvmessage(' ', ' ')
c
      Camera = 4
      Call Getres(Rloc, Camera)
      Call Prnt(4, 1, Camera, '  Camera # = .') 
      Call Xvmessage('     *****   Reseau Locations   ***** ',' ')
      Call Prnt(7, 404, Rloc(1), '.')
      Call Xvmessage(' ', ' ')
c
      Camera = 5
      Call Getres(Rloc, Camera)
      Call Prnt(4, 1, Camera, '  Camera # = .') 
      Call Xvmessage('     *****   Reseau Locations   ***** ',' ')
      Call Prnt(7, 404, Rloc(1), '.')
      Call Xvmessage(' ', ' ')
c
      Camera = 6
      Call Getres(Rloc, Camera)
      Call Prnt(4, 1, Camera, '  Camera # = .') 
      Call Xvmessage('     *****   Reseau Locations   ***** ',' ')
      Call Prnt(7, 404, Rloc(1), '.')
      Call Xvmessage(' ', ' ')
c
      Camera = 7
      Call Getres(Rloc, Camera)
      Call Prnt(4, 1, Camera, '  Camera # = .') 
      Call Xvmessage('     *****   Reseau Locations   ***** ',' ')
      Call Prnt(7, 404, Rloc(1), '.')
      Call Xvmessage(' ', ' ')
c
      Camera = 8
      Call Getres(Rloc, Camera)
      Call Prnt(4, 1, Camera, '  Camera # = .') 
      Call Xvmessage('     *****   Reseau Locations   ***** ',' ')
      Call Prnt(7, 404, Rloc(1), '.')
      Call Xvmessage(' ', ' ')
c
      Call tzgetres 
c
      Return
      End
