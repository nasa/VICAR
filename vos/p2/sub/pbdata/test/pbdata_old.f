C Returns picture body data given pb name (12 characters)
C
      SUBROUTINE PBDATA(name,BUF,*)

      REAL*4 BUF(*)
      INTEGER N,J,K
      INTEGER*4 id(65)
      REAL*4 SRANGE(11)
      REAL*4 RADPE(4,65),
     +       radpe_1(4,16),radpe_2(4,16),radpe_3(4,16),radpe_4(4,17)
      equivalence(radpe_1,radpe(1,1))
      equivalence(radpe_2,radpe(1,17))
      equivalence(radpe_3,radpe(1,33))
      equivalence(radpe_4,radpe(1,49))
      
      REAL*4 ROT(65)
      character*12 p8,pname(65)
      character*12 name
      data n/65/
      data id/
c mercury
     + 1,
c venus
     + 2,
c earth
     + 3,3,
c mars
     + 4,4,4,
c jupiter
     + 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
c saturn
     + 6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
c uranus
     + 7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
c neptune
     + 8,8,8,
c pluto
     + 9,9,
c gaspra
     + 10,
c ida
     + 11/

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
C
C Planet radii ra,rb,rc,lora in km & degrees
C where ra = (major) equatorial radius at longitude lora
C       rb = equatorial radius at longitude lora + 90 degrees
C       rc = polar radius (km)
      data RADPE_1/
C        MERCURY                         VENUS
     *2439.7,2439.7,2439.7,0.,        6137.,6137.,6137.,0.,
C        EARTH                           MOON
     *6378.14,6378.14,6356.75,0.,     1737.4,1737.4,1737.4,0.,
C        MARS                            PHOBOS
     *3397.,3397.,3375.,0.,             13.4,11.2,9.2,0.,
C        DEIMOS
     *7.5,6.1,5.2,0.,
C        JUPITER                         IO
     *71492.,71492.,66854.,0.,         1830.,1818.7,1815.3,0.,
C        EUROPA                         GANYMEDE
     *1565.,1565.,1565.,0.,            2634.,2634.,2634.,0.,
C        CALLISTO                       AMALTHEA
     *2403.,2403.,2403.,0.,            131.,73.,67.,0.,
C        HIMALIA                        ELARA
     *85.,85.,85.,0.,                  40.,40.,40.,0.,
C        PASIPHAE
     *18.,18.,18.,0./

      data radpe_2/
c        SINOPE
     *14.,14.,14.,0.,
C        LYSITHEA                       CARME
     *12.,12.,12.,0.,                  15.,15.,15.,0.,
C        ANANKE                         LEDA
     *10.,10.,10.,0.,                   5.,5.,5.,0.,
C        THEBE                          ADRASTEA
     *55.,55.,45.,0.,                    13.,10.,8.,0.,
C        METIS
     *20.,20.,20.,0.,
C        SATURN
     *60268.,60268.,54364.,0.,
C        MIMAS                          ENCELADUS
     *210.3,197.4,192.6,0.,            256.2,247.3,244.,0.,
C        TETHYS                         DIONE
     *523.,523.,523.,0.,               560.,560.,560.,0.,
C        RHEA                           TITAN
     *764.,764.,764.,0.,               2575.,2575.,2575.,0.,
C        HYPERION             
     *180.,140.,112.5,0./

      data radpe_3/
c          IAPETUS
     *718.,718.,718.,0.,
C        PHEOBE                         JANUS
     *115.,110.,105.,0.,                97.,95.,77.,0.,
C       EPIMETHEUS                      HELENE
     *69.,55.,55.,0.,                    17.5,17.5,17.5,0.,
C       TELESTO                         CALYPSO
     *15.2,12.5,7.5,0.,                  15.,8.,8.,0.,
C       ATLAS                           PROMETHEUS
     *18.5,17.2,13.5,0.,                74.,50.,34.,0.,
C       PANDORA
     *55.,44.,31.,0.,
C       URANUS 			        ARIEL
     *25559.0,25559.0,24973.0,0.,      581.1,577.9,577.7,0.,
C       UMBRIEL			         TITANIA
     *584.7,584.7,584.7,0.,            788.9,788.9,788.9,0.,
C       OBERON                          MIRANDA
     *761.4,761.4,761.4,0.,            240.4,234.2,232.9,0./

      data radpe_4/
C       CORDELIA                        OPHELIA
     *13.0,13.0,13.0,0.,               15.0,15.0,15.0,0.,
C       BIANCA                          CRESSIDA
     *21.,21.,21.,0.,                  31.,31.,31.,0.,
C       DESDEMONA                       JULIET
     *27.,27.,27.,0.,                  42.,42.,42.,0.,
C       PORTIA                          ROSALIND
     *54.,54.,54.,0.,                  27.,27.,27.,0.,
C       BELINDA                         PUCK
     *33.,33.,33.,0.,                  77.,77.,77.,0.,
C       NEPTUNE                         TRITON
     *25269.,25269.,24800.,0.,         1352.6,1352.6,1352.6,0.,
C       NEREID
     *170.,170.,170.,0.,
C       PLUTO                           CHARON
     *1162.,1162.,1162.,0.,            606.,606.,606.,0.,
C       GASPRA                          IDA
     *9.,5.5,5.,0.,                    28.,12.,10.5,0./


C AXIAL ROTATION RATE (DEGREES PER DAY)
      data ROT/
C MERCURY,VENUS,EARTH,MOON
     &6.1385025,-1.4813291,360.9856235,13.1763581,
C MARS,PHOBOS,DEIMOS
     &350.9819830,1128.8444790,285.161903,
C JUPITER,IO,EUROPA,GANYMEDE,CALLISTO,
     &870.536,203.4889538,101.3747235,50.3176081,21.5710715,
C AMALTHEA,HIMALIA,ELARA,PASIPHAE,SINOPE,LYSITHEA,
     &722.6303746,1.4365522,1.384083,0.4897959,0.474934,1.3846153,
C CARME,ANANKE,LEDA,THEBE,ADRASTEA,METIS
     &0.5202312,0.5834683,1.5,0.,0.,0.,
C SATURN,MIMAS,ENDELADUS,TETHYS,DIONE,
     &810.7939024,381.994555,262.7318996,190.6979085,131.5349316,
C RHEA,TITAN,HYPERION,IAPETUS,PHOEBE,JANUS,EPIMETHEUS,HELENE,TELESTO,
     &79.6900478,22.5769768,16.91729,4.5379572,0.6540697,0.,0.,0.,0.,
C CALYPSO,ATLAS,PROMETHEUS,PANDORA,
     &0.,0.,0.,0.,
C URANUS,ARIEL,UMBRIEL,TITANIA,OBERON,
     &-501.1600928,-142.8356681,-86.8688923,-41.3514316,-26.7394932,
C MIRANDA,CORDELIA,OPHELIA,BIANCA,CRESSIDA,DESDEMONA,JULIET,PORTIA,
     &-254.6906892,0.,0.,0.,0.,0.,0.,0.,
C ROSALIND,BELINDA,PUCK,NEPTUNE,TRITON,NEREID,PLUTO,CHARON
     &0.,0.,0.,483.7625981,-61.2572684,0.999549,-56.364,-56.3624607,
C GASPRA,IDA
     &0.,0./

C Mean solar ranges of the planets (AU)...
      data SRANGE/0.387098,0.723331,1.0,1.523679,5.2027,9.546,
     &   19.2,30.09,39.5,2.2016,2.9485/
C

      p8=name
C
      DO 50 J=1,N
      IF(PNAME(J).EQ.P8) GOTO 51
   50 CONTINUE
      RETURN1
C
   51 CALL MVE(7,4,RADPE(1,J),BUF,1,1)
      IF (ROT(J) .EQ. 0) THEN 
        BUF(5) = 0
      ELSE 
        BUF(5) = 360.D0 / ROT(J)	!Rotation period in days
      ENDIF
      K = ID(J)
      BUF(6) = 149597871.D0*SRANGE(K)	!Solar range in km
      RETURN
      END
