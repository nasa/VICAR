      INCLUDE 'VICMAIN_FOR'

C VICAR program GRIDGEN
C Generates a perfect grid: either a MARK format grid location file, or an
C image.
C
      SUBROUTINE MAIN44

!     Program GRIDGEN local variables 
      IMPLICIT INTEGER(A-Z)
      COMMON/C1/OBUF(1200)

      INTEGER*4 GSL,GSS,DNG,DNB
      LOGICAL XVPTST,IMAGE

      INTEGER INC, CNT, NCOL, NROW, DUM, OUNIT, STAT

!     Initialize GRIDGEN local variables 
      DATA INC /0/, CNT /0/, GSL /0/, GSS /0/, NROW /0/, DNG /0/
     &     NCOL /0/, DNB /0/, DUM /0/, OUNIT /0/, STAT /0/
      DATA OBUF /1200*0/


!     Begin program GRIDGEN

!     14apr2012 -lwk- changed IFMESSAGE to XVMESSAGE to avoid garbage output
!     on Linux
      CALL XVMESSAGE ('GRIDGEN version 1 July 1994', ' ')

!     Set default error handling action
      CALL XVEACTION ('SA', ' ')
 
!     Determine if the keyword 'IMAGE' was set on input parameters
      IMAGE = XVPTST('IMAGE')

!     Obtain information about values for the indicated parameters
      CALL XVP('INC',INC,CNT)
      CALL XVP('GSL',GSL,CNT)
      CALL XVP('GSS',GSS,CNT)
      CALL XVP('NROW',NROW,CNT)
      CALL XVP('NCOL',NCOL,CNT)
      CALL XVP('DNGRID',DNG,CNT)
      CALL XVP('DNBACK',DNB,CNT)

!     If the keyword 'IMAGE' was set in the input parameters
      IF (IMAGE) GOTO 100
      NSO = NROW*NCOL*2
      IF (NSO.GT.1200) THEN
           CALL XVMESSAGE ('***Too many grid intersections', ' ')
           CALL XVMESSAGE ('***Reduce NROW or NCOL', ' ')
           GOTO 999
      ENDIF

      CALL GENGRID (OBUF,NROW,NCOL,GSL,GSS,INC)
      CALL PGRID   (OBUF,NROW,NCOL,DUM,0)
      CALL XVUNIT  (OUNIT,'OUT',1,STAT, ' ')
      CALL XVOPEN(OUNIT,STAT,'U_NL',1,'U_NS',NSO,
     *     'O_FORMAT','REAL','OP','WRITE', ' ')
      CALL XVWRIT(OUNIT,OBUF,STAT, ' ')
      GOTO 200

  100 NLO = GSL + NROW*INC - INC/2
      NSO = GSS + NCOL*INC - INC/2
      CALL XVUNIT (OUNIT,'OUT',1,STAT, ' ')
      CALL XVOPEN (OUNIT,STAT,'U_NL',NLO,'U_NS',NSO,
     *            'O_FORMAT','BYTE','OP','WRITE', ' ')

      CALL GENIMAGE (OUNIT,DNB,DNG,NLO,NSO,NROW,NCOL,GSL,GSS,INC)

  200 CALL XVMESSAGE ('GRIDGEN task completed', ' ')
      RETURN
  999 CALL XVMESSAGE ('***GRIDGEN task canceled', ' ')
      CALL ABEND
      END
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                 Subroutine to generate grid locations...
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
C
      SUBROUTINE GENGRID (OBUF,NROW,NCOL,GSL,GSS,INC)

      IMPLICIT INTEGER(A-Z)

!     Subroutine GENGRID passed parameteres
      REAL*4 OBUF(1200)
      INTEGER*4 GSL,  GSS
      INTEGER   NROW, NCOL, INC

!     Subroutine GENGRID local parameteres
      INTEGER*4 GSS0
      INTEGER   I, J, K

!     Subroutine GENGRID local parameter initialization
      DATA      I /0/, J /0/, K /0/
      DATA      GSS0 /0/

!     Begin Subroutine GENGRID
      GSS0 = GSS
      K = 1

      DO J=1,NROW	!ROWS
          DO I=1,NCOL   !COLUMNS
              OBUF(K) = GSL
              OBUF(K+1) = GSS
              K = K + 2
              GSS = GSS + INC
          ENDDO
          GSS = GSS0
          GSL = GSL + INC
      ENDDO
      RETURN
      END
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                 Subroutine GENIMAGE
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
C
      SUBROUTINE GENIMAGE(OUNIT,DNB,DNG,NLO,NSO,NROW,NCOL,GSL,GSS,INC)

      IMPLICIT INTEGER(A-Z)

!     Subroutine GENIMAGE passed parameteres
      INTEGER*4 GSL,GSS,DNG,DNB
      INTEGER   OUNIT, NLO, NSO, NROW, NCOL, INC

!     Subroutine GENIMAGE local parameteres
      COMMON/C2/PIC(1200),PIC2(1200)
      BYTE PIC,PIC2
      INTEGER   L, STAT

!     Initialize Subroutine GENIMAGE local parameters
      DATA      STAT /0/, L /0/
      DATA      PIC /1200*0/, PIC2 /1200*0/


!     Begin Subroutine GENIMAGE
      CALL ITLA(DNB,PIC,NSO)			! Start with background
      CALL MVE(-5,NCOL,DNG,PIC(GSS),0,INC)	! Add vertical grid rulings
      CALL ITLA(DNG,PIC2,NSO)			! Generate horizontal grid

      DO L=1,NLO
          IF (L.EQ.GSL) THEN
              CALL XVWRIT(OUNIT,PIC2,STAT, ' ')
              GSL = GSL + INC
          ELSE
              CALL XVWRIT(OUNIT,PIC,STAT, ' ')
          ENDIF
      ENDDO

      RETURN
      END
