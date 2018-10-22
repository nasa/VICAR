CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Copy SPICE data into CONVEV data buffer.
C
      SUBROUTINE SPICE2CONVEV(RBUF,FL,OAL,OAS,SCALE,MPTYPE,data)
      REAL*8 RBUF(100)
      REAL*4 DATA(40)
      REAL*4 FL,OAL,OAS,SCALE
      INTEGER*4 MPTYPE
      CALL MVE(1,72,RBUF(59),data,1,1)!OM-matrix
      CALL MVE(1,27,RBUF(22),data(19),1,1)!RS-vector
      DATA(25) = RBUF(15)!Polar radius
      DATA(26) = RBUF(13)!Equatorial radius
      DATA(27) = FL
      DATA(28) = OAL
      DATA(29) = OAS
      DATA(30) = SCALE
      DATA(38) = RBUF(27)!Target range
      CALL MVE(4,1,MPTYPE,data(39),1,1)!Projection type
      RETURN
      END
