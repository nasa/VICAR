c  test subroutine VWAIT
      SUBROUTINE TVWAITF
C==================================================================
      CALL vwait(1000)    ! 10 seconds * 100 ticks/sec
      RETURN
      END
