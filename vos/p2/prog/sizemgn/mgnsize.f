C***************************************************************
C Originally implemented for Magellan, this is the 'LOOKUP option of SIZE
C
C	Include 'r2lib:main.fin'
C
C Dummy main for testing arrays
C
C	Subroutine Main44
C	Include 'SLOOKUP.Inc/list'
C	Integer*4 Full(256)
C	Byte Bits(256)
C
C	Do I=0,127,1
C	   Bits( I+1 ) = I
C	   Bits( I+129 ) = -128 + I
C	EndDo
C
C	Call MGNSIZE
C
C	Length = 256
C	Call MgnDntoVo( Bits, Full, Length )
C	Write(6,20)
C20	Format( ' *** Dn ---> Vo Conversion ***', /' ' )
C	Write(6,10) ((I,(Bits(I).And.255),Full(I)),I=1,256)
C10	Format( ' Bits( ', I3, ' ) = ', I3, '    Full = ', I6 )
C
C	Write(6,30)
C30	Format( /'0*** Vo ---> Dn Conversion ***', /' ' )
C	Call MgnVotoDn( Full, Bits, Length )
C	Write(6,10) ((I,(Bits(I).And.255),Full(I)),I=1,256)
C	Call Exit
C	End
C*************
C Initialize
C*************
	Subroutine MGNSIZE
	implicit none
c	Implicit Integer*4 (A-Z)
	Integer*4 in1, in2, IOStatus,MaxOutLength
	Integer*4 I,i1,i2,i3,ind,nlit,nsit,nlot,nsot
	Character*40 FilNam
        INCLUDE 'fortport'		!for byte2int
	INCLUDE 'slookup.inc'

C************************************************************
C  Table values generated with the following equations:
C  ------------------------------------------------------------
C  	DN = 1 + 5*(20*ALOG10[((LINE-1)*ns + SAMP)/scale] + 20)
C  	Vo = scale * 10 ** [((SAMP-2)/100) - 1]
C  where
C  Line = [ 1..nl ], Sample = [ 1..25000 ], ns = max # samples
C  ------------------------------------------------------------
C  Non-scaled equations are:
C  	Vo = 10 ** [ {(Dn-1)/100} - 1  ],  
C  	Dn = 1 + 5*(20 log(V) + 20)	Dn: 0-255
C---------------------------------------------------------------
C  Get info about dn-volts table here and open it (256 int array, 1024 bytes)
	Call xvp( 'DVDATA', FilNam, IOStatus)
	Call xvunit( in1, 'DVDATA', 1, IOStatus,
     *               'U_NAME', FilNam,' ' )
	CALL XVSIGNAL(in1,IOStatus,.TRUE.)
	Call xvopen(in1, IOStatus, 'OPEN_ACT', 'SA', 'IO_ACT', 'SA',' ')
        CALL XVGET(in1,IND,'NL',NLIT,'NS',NSIT,' ')
        IF (NLIT.NE.1.AND.NSIT.NE.256) GOTO 991
        Call xvread(in1, InpTable, IOStatus,' ')
	Call xvclose(in1, IOStatus,' ')

C  Get info about volts-dn table here and open it (25000 bytes per line)
	Call xvp( 'VDDATA', FilNam, IOStatus )
	Call xvunit(in2, 'VDDATA', 1, IOStatus, 'U_NAME', FilNam,' ')
	CALL XVSIGNAL(in2,IOStatus,.TRUE.)
	Call xvopen(in2, IOStatus, 'OPEN_ACT', 'SA', 'IO_ACT', 'SA',' ')
        CALL XVGET(in2,IND,'NL',NLOT,'NS',NSOT,' ')
        MaxOutLength = NLOT * NSOT
	if (MaxOutLength.gt.1250000) GOTO 992
	Do I=1,MaxOutLength,NSOT
	   Call xvread(in2, OutTable(I), IOStatus,' ')
	EndDo
	Call xvclose(in2, IOStatus,' ')

C  Check tables :  InpTable = dn to volts, OutTable = volts to dn
C  See if what goes in will come back out!

      I1 = BYTE2INT(OutTable(InpTable(255)))
      I2 = BYTE2INT(OutTable(InpTable(128)))
      I3 = BYTE2INT(OutTable(InpTable(0)))

      if (  I1 .NE.255. or . 
     *      I2 .NE.128. or .
     *      I3 .NE.0 ) GOTO 995

C  List out arrays for debug
C	Write(6,30)		! "D" debug marker does not work (?!) in Vicar
C30	Format( ' ' )
C
C	Do I=1,255
C	   Write(6,20) I, InpTable( I )
C20	   Format( ' DN(', I3, ') = ', I6 )
C	EndDo
C
C	Write(6,30)
C	Inc = 10
C	Do I=1,MaxOutLength,Inc
C	   Write(6,10) I, ((OutTable( J ) .And. 255),J=I,I+Inc-1)
C10	   Format( ' I = ', I6, '   D = ', <Inc>(' ', I3) )
C	EndDo
C
	Return

  991 CALL XVMESSAGE('??E - Invalid DN-to-Volts file format',' ')
      GOTO 999
  992 CALL XVMESSAGE( '??E - Volts-to-DN table exceeds allowed size',' ')
      goto 999
  995 CALL XVMESSAGE('??E - Bad DN-volt-DN conversion found',' ')
      CALL XVMESSAGE('??E -Check DN-to-Volts and Volts-to-DN tables',' ')
  999 call abend 
      END

C*****************************
C Convert from DN to Vo value 
C*****************************

	Subroutine MgnDNtoVo(InBuf,OutBuf,InLen)
	implicit none
c	Implicit Integer*4 (A-Z)
	Integer*4 InBuf(*)
	Integer*4 OutBuf(*)
	Integer*4 InLen
	Integer*4 I
	INCLUDE 'slookup.inc'

	Do I=1,InLen
	   OutBuf(I) = InpTable(InBuf(I))
	EndDo
	Return
	End

C*****************************
C Convert from Vo to DN value 
C*****************************

C	Subroutine MgnVotoDN(InBuf,OutBuf,InLen) 
C	Implicit Integer*4 (A-Z)
C	Integer*4 InBuf(1)
C	Logical*1 OutBuf(1)
C	Integer*4 InLen
C	Integer*4 I
C	INCLUDE 'slookup.inc'
C
C	Do I=1,InLen
C	   If (InBuf(I) .Gt. MaxOutLength) InBuf(I) = MaxOutLength
C	   OutBuf(I) = OutTable(InBuf(I))
C	EndDo
C	Return
C	End
