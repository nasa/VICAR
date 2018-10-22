c
      Subroutine PBNAME(ID,PAR,*)
c
C  ROUTINE TO RETURN PLANET NAME GIVEN ID
c
C ID NUMBERING SCHEME IS CONSISTENT WITH GLL SPICE
C  LOOKUP TABLE FOR PLANET I.D.  FROM SEDR WORD # 9
c
      INTEGER  N/65/
       Character*12  Par
       Character*12  p8
       Character*12  pname(65) 
      INTEGER  LOOK(65)
      Data  LOOK /
c mercury
     + 199,
c venus
     + 299,
c earth
     + 399,301,
c mars
     + 499,401,402,
c jupiter
     + 599,501,502,503,504,505,506,507,508,509,510,511,512,513,
     + 514,515,516,
c saturn
     + 699,601,602,603,604,605,606,607,608,609,610,611,612,613,
     + 614,615,616,617,
c uranus
     + 799,701,702,703,704,705,706,707,708,709,710,711,712,713,
     + 714,715,
c neptune
     + 899,801,802,
c pluto
     + 999,901,
c gaspra
     + 9511010,
c ida
     + 2431010 /
c
       Data Pname /   'MERCURY     ','VENUS       ','EARTH       ',
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
     + 'GASPRA      ','IDA         ' /
c
C
      Do 5  I=1,N
       If (ID .EQ. LOOK(I)) GoTo  6
5     Continue
      RETURN1
6     Par = pname(i)      
      Return
C
C ROUTINE TO RETURN PLANET ID GIVEN PLANET NAME (PAR)
c
      ENTRY PBID(PAR,ID,*)
C
      p8=par
C
      call ccase(p8,1,-1)      ! ensure case is UPPER
      DO 10 I=1,N
      If (PNAME(I) .EQ. P8) GoTo 11
10     CONTINUE
      RETURN1
C
11    ID = LOOK(I)
c
      Return
      End
