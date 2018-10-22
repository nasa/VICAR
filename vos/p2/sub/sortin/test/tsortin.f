c  test subroutines SORTIN and I2SORT
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      IMPLICIT INTEGER*4 (A-Z)
      INTEGER*4 KEY(10)/2,10,3,8,15,16,1,20,7,19/,K(10)
      INTEGER*4 PTR(10)/1,2,3,4,5,6,7,8,9,10/,P(10)

      N=10
      CALL XVMESSAGE('TEST OF SORTIN',' ')
      CALL MVE(4,N,KEY,K,1,1)
      CALL PRNT(4,N,K,' SORTIN input= .')
      CALL SORTIN(K,N)
      CALL PRNT(4,N,K,' SORTIN output=.')

      CALL XVMESSAGE('TEST OF I2SORT',' ')
      CALL MVE(-6,N,KEY,K,1,1)
      CALL MVE(-6,N,PTR,P,1,1)
      CALL PRNT(2,N,K,' I2SORT input= .')
      CALL I2SORT(K,P,N)
      CALL PRNT(2,N,K,' I2SORT output=.')
      CALL PRNT(2,N,P,' POINTER=.')

      CALL XVMESSAGE('TEST C BRIDGES',' ')
      CALL TZSORTIN
      RETURN
      END
