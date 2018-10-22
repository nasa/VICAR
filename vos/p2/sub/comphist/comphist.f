C Compute a histogram of a byte image.
C
      SUBROUTINE COMPHIST(IUNIT,SL,SS,NL,NS,ohist,ibuf)
      INTEGER*4 OHIST(256)
      INTEGER*2 IBUF(32768)
      INTEGER*4 SL,SS,EL

      CALL ZIA(OHIST,256)
      EL = SL+NL-1
C     ....Generate histogram
      DO LINE=SL,EL
         CALL XVREAD(IUNIT,ibuf,ind,'LINE',LINE,'SAMP',SS,'NSAMPS',
     &               NS,' ')
         CALL HSUB(1,NS,IBUF,OHIST,0,255)
      ENDDO
      RETURN
      END
C Compute a histogram of a halfword image.
C
      SUBROUTINE COMPHIST2(IUNIT,SL,SS,NL,NS,hist,ibuf)
      INTEGER*4 HIST(-32768:32767)
      INTEGER*2 IBUF(32768)
      INTEGER*4 SL,SS,EL

      CALL ZIA(HIST,65536)
      EL = SL+NL-1
C     ....Generate histogram
      DO LINE=SL,EL
         CALL XVREAD(IUNIT,ibuf,ind,'LINE',LINE,'SAMP',SS,'NSAMPS',
     &               NS,' ')
         DO J=1,NS
            IDN = IBUF(J)
            HIST(IDN) = HIST(IDN) + 1
         ENDDO
      ENDDO

      RETURN
      END
