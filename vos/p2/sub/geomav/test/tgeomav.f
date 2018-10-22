      INCLUDE 'VICMAIN_FOR'
c
      Subroutine  Main44
c
C   Program  TGEOMAV
C   This is a TEST Program for subroutine GEOMAV
C   GEOMAV prodcues GEOMA parameters from image space RESEAU
C   locations for a voyager image.
c
	Real*4  LOC(2,202)
	Real*4  IRES(404)
	Real*4  CONV(2216)
	INTEGER I, J
c
      Call Xvmessage(' ******  Testing FORTRAN version  ******', ' ')
      Call Xvmessage(' ',' ')
      Call Xvmessage(' ',' ')
c
	DO I=1,8
         Call  GETRES(LOC,I)
	 Call  PRNT(4,1,I,' CAMERA SN.')
	 Call  PRNT(7,404, LOC,' LOC(L,S).')
c
         Do 50  J=1,404
           IRES(J) = 0.0
50       Continue
	 Call  GEOMAV(CONV,I,IRES)
c
	 Call  Prnt(0,4,conv(1),'nah=.')
	 Call  Prnt(0,4,conv(2),'   =.')
         Call  prnt(4,1,conv(3),' nah=.')
	 Call  prnt(0,4,conv(4),' nav=.')
	 Call  prnt(0,4,conv(5),'    =.')
	 Call  prnt(4,1,conv(6),' nav=.')
	 Call  prnt(0,4,conv(7),' tiep=.')
	 Call  prnt(0,4,conv(8),'    =.')
c
	 Call  PRNT(4,1,I,' VIDICON SN.')
	 Call  PRNT(7,2208,CONV(9),' geoma prameters=.')
	EndDO
c
c    Test C-Bridge version:   Zgeomav
c
        Call  tzgeomav 
c
	Return 
	End
C************START PDF************
Cprocess
Cend-proc
C*********END PDF****************
