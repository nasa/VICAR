      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      INTEGER MODE,NPV,NPH,IND,I,J,PAR(100),SLO,SSO
      REAL LINE,SAMPLE
      REAL CONV(5000)

C  TEST1 FOR TRITRA
      NPH = 24
      NPV = 23
      IND = 0
      IPTR=0
      CALL XVUNIT(IUNIT,'INP',1,STAT,' ')
      CALL XVOPEN(IUNIT,STAT,'OPEN_ACT', 'SA',
     .      'IO_ACT', 'SA', ' ' )
      CALL XVSIZE(SLO,SSO,NLO,NSO,NLI,NSI)
      NL=NLI
      DO I=1,NL
         CALL XVREAD(IUNIT,CONV(I*900-900+1),STAT,'LINE',I,
     &               'NSAMPS',3600,' ')
      ENDDO
      LINE = 400.
      SAMP = 400.
      MODE = 1
      CALL PRNT(7,1,LINE,'LINE =.')
      CALL PRNT(7,1,SAMP,' SAMP =.')
      CALL TRITRA(IND,CONV(10),NPH,NPV,LINE,SAMP,MODE)
      CALL PRNT(7,1,LINE,' LINE =.')
      CALL PRNT(7,1,SAMP,' SAMP =.')

C  TEST2 FOR TRITRA

      LINE = 498.56
      SAMP = 498.59
      MODE = 0
      CALL PRNT(7,1,LINE,' LINE =.')
      CALL PRNT(7,1,SAMP,' SAMP =.')
      CALL TRITRA(IND,CONV(10),NPH,NPV,LINE,SAMP,MODE)
      CALL PRNT(7,1,LINE,' LINE =.')
      CALL PRNT(7,1,SAMP,' SAMP =.')
      CALL XVCLOSE(IUNIT,STAT,' ')

      CALL XVMESSAGE(
     . 'Repeat test cases in C to test C interface: ztritra', ' ')

      call tztritra(CONV(10) )

      return
      END

