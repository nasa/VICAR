C Test for routine PBDATA
      INCLUDE 'VICMAIN_FOR'

      SUBROUTINE MAIN44
      CHARACTER*80 BUF
      REAL*4 D(20)
      INTEGER NTARGETS
      character*12 name(65)
      data ntargets/65/
      data name/   'MERCURY     ','VENUS       ','EARTH       ',
     + 'MOON        ','MARS        ','PHOBOS      ','DEIMOS      ',
     + 'JUPITER     ','IO          ','EUROPA      ','GANYMEDE    ',
     + 'CALLISTO    ','AMALTHEA    ','HIMALIA     ','ELARA       ',
     + 'PASIPHAE    ','SINOPE      ','LYSITHEA    ','CARME       ',
     + 'ANANKE      ','LEDA        ','THEBE       ','ADRASTEA    ',
     + 'METIS       ','SATURN      ','MIMAS       ','ENCELADUS   ',
     + 'TETHYS      ','DIONE       ','RHEA        ','TITAN       ',
     + 'HYPERION    ','IAPETUS     ','PHOEBE      ','JANUS       ',
     + 'EPIMETHEUS  ','HELENE      ','TELESTO     ','CALYPSO     ',
     + 'ATLAS       ','PROMETHEUS  ','PANDORA     ','URANUS      ',
     + 'ARIEL       ','UMBRIEL     ','TITANIA     ','OBERON      ',
     + 'MIRANDA     ','CORDELIA    ','OPHELIA     ','BIANCA      ',
     + 'CRESSIDA    ','DESDEMONA   ','JULIET      ','PORTIA      ',
     + 'ROSALIND    ','BELINDA     ','PUCK        ','NEPTUNE     ',
     + 'TRITON      ','NEREID      ','PLUTO       ','CHARON      ',
     + 'GASPRA      ','IDA'/

      call xvmessage('********FORTRAN CALLABLE VERSION****',' ')
      call xvmessage('1       TARGET  TARGET                  
     +         ROTATION     SOLAR',' ')
      call xvmessage('        NUMBER   NAME       A       B   
     + C       PERIOD      RANGE',' ')
      BUF(1:80) = ' '

      call init_spice

      DO 10 I=1,65
      CALL PBDATA(NAME(I),D,*8)
      call reset()
    8 CALL PBID(NAME(I),ID,*10)		!SEDR ID
      call reset()
      WRITE (BUF(4:11),'(I8)') ID !Target number
      buf(16:23) = name(i)             !Target name
      WRITE (BUF(26:32),'(F7.1)') D(1) !RA
      WRITE (BUF(34:40),'(F7.1)') D(2) !RB
      WRITE (BUF(42:48),'(F7.1)') D(3) !RC
      WRITE (BUF(50:61),'(F12.7)') D(5) !Rotation period
      WRITE (BUF(63:78),'(E16.10)') D(6) !Solar range
      CALL XVMESSAGE(BUF,' ')
   10 CONTINUE
C
c********C CALLABLE VERSION****
      call tzpbdata()

   20 RETURN
      END
