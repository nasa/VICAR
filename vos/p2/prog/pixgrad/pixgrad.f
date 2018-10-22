C***************************************************************************
C 
C    PIXGRAD calculates the magnitude of the gradient of an image.
C    The original comments (below) are wrong; the default calculation
C    computes delta_x and delta_y by the following algorithm using a
C    3 x 3 pixel window around point e:
C
C          a   b  c             METHOD #1 (default):
C          d   e  f         delta_x(e) =  (f - d)/ 2
C          g   h  j         delta_y(e) =  (b - h)/ 2 
C
C    The new parameter METHOD allows for more precise computations
C    using the following:
C
C      First-order ERDAS method:
C
C           delta_x = [(c + f + j) - (a + d + g)] / 6 
C           delta_y = [(g + h + j) - (a + b + c)] / 6 
C
C      Second-order ARCINFO method:
C
C           delta_x = [(c + 2*f + j) - (a + 2*d + g)] / 8 
C           delta_y = [(g + 2*h + j) - (a + 2*b + c)] / 8 
C
C     From the deltas the "dip" angle of the normal vector, and its
C     azimuthal orientation with respect to north  is computed by:
C
C          z = sqrt ( (delta_x**2) + (delta_y**2) )
C
C          dip angle =  ATAN(SQRT(z))
C          azimuth   =  ATAN2(-delta_x, delta_4) MOD 2*PI
C
C
C  Here is the original comment:
C
C    PIXGRAD calculates the magnitude of the gradiend of a picture.
C    The absolute value of the difference between the current pixel
C    and its eight adjacent neighbors is calculated.  For the current
C    output pixel location, the largest difference is produced on the
C    primary ourput data set; the secondary output data set will
C    contain the index of the largest difference.  The differences 
C    are labelled 1 through 8 counterclockwise starting with the
C    difference of the current pixel (I,J) and the neighbor on the
C    left (I,J-1).
C
C    VARIABLE INP
C             Input filename - may be byte of halfword.
C    VARIABLE OUT
C             Output filenames - one for dip and the 
C             other for azimuth. Byte data output.
C             Optionally, the user may have an output
C             centered data set and the X,Y derivatives
C             data set.  The centered data set, if present,
C             is output file 3; the X,Y data set follow.
C    VARIABLE SIZE
C             Vicar size field.
C             -----------------
C             Starting line          default - input image
C             Starting sample
C             Number of lines
C             Number of samples
C    VARIABLE FORMAT
C
C     The format is obtained from the input image label. 
C     Also, it is used to determine the output format for the
C     x and y derivative files.  Default is REAL.
C
C    VARIABLE NORTH
C             Direction of North in degrees clockwise from 
C                           top of image.
C             Default = 0.0
C    VARIABLE ADIP
C             Scale factor for slope.
C             DN(DIP) = ADIP * (DIP+BDIP)
C             where
C             adip - scale factor
C             dip  - computed dip in degrees
C             bdip - offset factor ( see BDIP )
C             Default = 255./90.
C    VARIABLE BDIP
C             Offset factor for slope.
C             DN(DIP) = ADIP * (DIP+BDIP)
C             where
C             adip - scale factor ( see ADIP )
C             dip  - computed dip in degrees
C             bdip - offset factor 
C             Default = 0.0
C    VARIABLE AAZ
C             Scale factor for azimuth.
C             DN(AZIM) = AAZ * (AZIMUTH+BAZ)
C             where
C             aaz      - scale factor 
C             azimuth  - computed azimuth in degrees from North
C             baz      - offset factor ( see BAZ )
C             Default = 256./360.
C    VARIABLE BAZ
C             Offset factor for azimuth.
C             DN(AZIM) = AAZ * (AZIMUTH+BAZ)
C             where
C             aaz      - scale factor ( see AAZ )
C             azimuth  - computed azimuth in degrees from North
C             baz      - offset factor 
C             Default = 0.0
C    VARIABLE AXDER
C             Scale factor for xderivative.
C             DN(XDER) = AXDER * (XDERIV+BXDER)
C             where
C             axder    - scale factor 
C             xderiv   - computed x derivative
C             bxder    - offset factor ( see BXDER )
C             Default = 256./360.
C    VARIABLE BXDER
C             Offset factor for x derivative.
C             DN(XDER) = AXDER * (XDERIV+BXDER)
C             where
C             axder    - scale factor ( see AXDER ) 
C             xderiv   - computed x derivative
C             bxder    - offset factor 
C             Default = 256./360.
C    VARIABLE AYDER
C             Scale factor for y derivative.
C             DN(XDER) = AYDER * (YDERIV+BYDER)
C             where
C             ayder    - scale factor 
C             yderiv   - computed y derivative
C             byder    - offset factor ( see BYDER )
C             Default = 256./360.
C    VARIABLE BYDER
C             Offset factor for x derivative.
C             DN(YDER) = AYDER * (YDERIV+BYDER)
C             where
C             ayder    - scale factor ( see AYDER ) 
C             yderiv   - computed x derivative
C             byder     - offset factor 
C             Default = 256./360.
C    VARIABLE LSCALE
C             Line scale in miles per line.
C             Default = 0.01
C    VARIABLE SSCALE
C             Sample scale in miles per sample.
C             Default = 0.01
C    VARIABLE AALT
C             Altitude scale in feet per dn of input.
C             Default = 1.0 ft/dn
C    VARIABLE CENTERED
C             If centered output is required.
C***************************************************************************
C
C     10-10-88  ...SP...   CHANGED TO IGNORE FORMAT PARAMETER BECAUSE VICAR2 
C                          USES ONLY THE FORMAT IN LABEL.
C     08-05-94   (CRI)     MSTP S/W Conversion (VICAR porting) 
C     09-06-96    NDR      Added ARCINFO,ERDAS options, buffer sized 32000
C     10-10-96    BAM      1. added optional outputs for x and y derivarives
C                          2. added optional output for centered data set
C     01-08-97    NDR      Fixed ARCINFO,ERDAS horizontal sign-error bug
C     02-07-97    BAM      merged REA pdf changes into pdf
C
C
C    A = INPUT PICTURE (EG ALTITUDE)  HALFWORD OR BYTE
C    B = GRADIENT MAGNITUDE PICTURE (EG DIP)      BYTE ONLY
C    C = GRADIENT DIRECTION PICTURE (EGAZIMUTH)   BYTE ONLY
C
C    SIZE FIELD REFERS TO THE INPUT PICTURE
C
C    PARAMETERS ARE POSITIONALLY INDEPENDENT...
C    KEYWORD  VARIABLE  FORMAT  DEFAULT   DESCRIPTION
C    'NORTH'  NORTH     FLOAT   0.        DIR OF NRTH DEG CLKWS FRM UP
C    'ADIP'   ADIP      FLOAT   255./90.  DN(DIP) = ADIP*(DIP+BDIP)
C    'BDIP'   BDIP      FLOAT   0.        
C    'AAZ '   AAZ       FLOAT   256./360. DN(AZIM)=AAZ*(AZIMUTH+BAZ)
C    'BAZ '   BAZ       FLOAT   0.        
C    'LSCA'LE LSCALE    FLOAT   .01       LINE SCALE, IN MILES/LINE
C    'SSCA'LE SSCALE    FLOAT   .01       SAMPLE SCALE, IN MILES/SAMPLE
C    'AALT'   AALT      FLOAT   1.0 FT/DN ALTITUDE=AALT*DN(ALT)
C***************************************************************************
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44

      IMPLICIT NONE

      COMMON ALT,DIP,AZIM,XSSC,YLSC,TADIP,TAAZ,TBDIP,TBAZ,NSO,
     +        TWOPI,RNRTH

      INTEGER   MAXSAMP                   !If changed, be sure to change the
      PARAMETER (MAXSAMP=32000)           !  MAXSAMP parameter in CMPT, too!

      INTEGER*2  ALT(3*MAXSAMP+8)         ! altitude array 
      INTEGER*2  DIP(MAXSAMP)             ! gradient magnitude array
      INTEGER*2  AZIM(MAXSAMP)            ! gradient direction array
      INTEGER*2  TDIP(MAXSAMP/2)          ! TEMPORARY ARRAYS
      INTEGER*2  TAZI(MAXSAMP/2) 
      REAL*4  TXBU(MAXSAMP/2)          
      REAL*4  TYBU(MAXSAMP/2)          

      COMMON/DERIVATIVE/XBUF,YBUF,FORMAT
      REAL*4     XBUF(MAXSAMP)            ! x partial derivative
      REAL*4     YBUF(MAXSAMP)            ! y partial derivative
      INTEGER*2  FORMAT                   ! format type

       
      INTEGER*4  NLO1, NBO1, NLO_CENT, NBO_CENT, J, K, INC, NOW
 
      CHARACTER*72 LBL

      REAL*4  PI                     ! value of PI
      REAL*4  NORTH                  ! DIR OF NRTH, DEG CLKWS FRM UP
      REAL*4  ADIP                   ! DN(DIP) = ADIP*(DIP+BDIP)
      REAL*4  BDIP                   ! DN(DIP) = ADIP*(DIP+BDIP)
      REAL*4  AAZ                    ! DN(AZIM)=AAZ*(AZIMUTH+BAZ)
      REAL*4  BAZ                    ! DN(AZIM)=AAZ*(AZIMUTH+BAZ)
      REAL*4  LSCALE                 ! LINE SCALE, IN MILES/LINE
      REAL*4  SSCALE                 ! SAMPLE SCALE, IN MILES/SAMPLE
      REAL*4  AALT                   ! ALTITUDE=AALT*DN(ALT)+BALT
      REAL*4  VAL(8)                 ! FOR SUBROUTINE EQUIVALENCE
      REAL*4  XSSC,YLSC,TWOPI,RNRTH  ! FOR SUBROUTINE COMMONALITY
      REAL*4  TADIP,TAAZ,TBDIP,TBAZ  ! FOR VARIABLE INITIALIZATION

      EQUIVALENCE (VAL(1),NORTH),(VAL(2),ADIP),(VAL(3),BDIP)
      EQUIVALENCE (VAL(4),AAZ),(VAL(5),BAZ),(VAL(6),LSCALE)
      EQUIVALENCE (VAL(7),SSCALE),(VAL(8),AALT)

      CHARACTER*132 input(2)         ! input files
      CHARACTER*132 output(4)        ! output files
      CHARACTER*12 IFORMAT           ! input format byte or half ? 
      CHARACTER*12 DFORMAT           ! x,y derivative output format 

      INTEGER*4 SL                   ! starting line
      INTEGER*4 SB                   ! starting sample
      INTEGER*4 NLO                  ! number of lines output
      INTEGER*4 NBO                  ! number of samples output
      INTEGER*4 NLI                  ! number of lines input
      INTEGER*4 NBI                  ! number of samples input
      INTEGER*4 LINE, LIN, NB, NSO   ! temp variables
      INTEGER*4 NDSI, IDEF, NDSO     ! temp variables
      INTEGER*4 IPIXSIZ, ICOUNT      ! temp variables
      INTEGER*4 IPNT,JPNT,KPNT,N,I   ! temp variables
      INTEGER*4 NI,NJ,NK,N1,N2,N3,N4 ! temp variables

      INTEGER*4 inunit               ! unit of input data set
      INTEGER*4 outunit(4)           ! up to 4 units for output data sets
      INTEGER*4 status               ! status of xv returns

      LOGICAL*1 HALF                 ! default for halfword input
      LOGICAL VERBOSE,CENTERED,XVPTST

      DATA PI /3.1415927/            ! value of pi
      DATA HALF /.TRUE./             ! default for halfword input
      DATA input(1) /' '/
      DATA input(2) /' '/
      DATA VAL /8*0.0/               ! initialize array
      DATA IPIXSIZ /0/
      DATA IFORMAT /' '/
      DATA DFORMAT /' '/


C------------------------------------------------------------------------
C
C                PROCESS PARAMETERS, CHECK FOR ERROR CONDITIONS, 
C                            AND OPEN DATA SETS
C
C------------------------------------------------------------------------

      LINE = 0
      LIN  = 0

      VERBOSE = XVPTST('VERBOSE')
      CALL xveaction ('SA',' ')
      CALL ifmessage('PIXGRAD version 05-SEP-94')

      call xvparm ('INP', input, NDSI, IDEF, 1)
      IF ( NDSI .NE. 1 ) THEN
         CALL xvmessage
     +           ('$$$ INCORRECT NUMBER OF INPUT DATA SETS $$$',' ')
         CALL abend
      END IF

      call xvunit ( inunit, 'INP', 1, status, ' ' ) ! get unit for input data
      call xvopen  ( inunit, status, 'U_NL', 512, 'U_NS', 512,
     -                   'U_FORMAT','HALF','I_FORMAT','HALF', ' ')      

      call xvparm ( 'OUT', output, NDSO, IDEF, 4 )
      IF ( NDSO .lt. 2 ) THEN
         CALL xvmessage
     +           ('$$$ INCORRECT NUMBER OF OUTPUT DATA SETS $$$',' ')
         CALL abend
      END IF

      centered = XVPTST('CENTER') ! FOR CENTERED OUTPUT DATA SET

      call xvsize ( SL, SB, NLO, NBO, NLI, NBI )
 
      if ( .not. centered ) then 
          IF ( SL + NLO .GT. 1 + NLI ) THEN
             CALL xvmessage('$$$ REQUESTED SIZE FIELD EXCEEDS',' ')
             CALL xvmessage('    INPUT PICTURE LENGTH. $$$',' ')
             CALL abend      
          END IF
      else              ! reset output sample size
          nlo_cent = nlo / 3
          nbo_cent = nbo / 3
      end if

      CALL xvget(inunit, status, 'PIX_SIZE', IPIXSIZ,
     +             'FORMAT', IFORMAT,' ')! BYTES PER PIXEL AND FORMAT.
      HALF = IPIXSIZ .EQ. 2
      IF ( IPIXSIZ .NE. 1 .AND. IPIXSIZ .NE. 2 )
     .     CALL mabend('ERROR:INP. IMAGE IS NOT BYTE OR HALF FORMAT')

      call xvp ( 'NORTH',  NORTH,  icount )
      call xvp ( 'ADIP',   ADIP,   icount )
      call xvp ( 'BDIP',   BDIP,   icount )
      call xvp ( 'AAZ',    AAZ,    icount )
      call xvp ( 'BAZ',    BAZ,    icount )
      call xvp ( 'LSCALE', LSCALE, icount )
      call xvp ( 'SSCALE', SSCALE, icount )
      call xvp ( 'AALT',   AALT,     icount )
      call xvp ( 'FORMAT', DFORMAT,  icount )


      if ( dformat .eq. 'REAL' ) then  ! x,y derivative output format
          format = 1
      else
          format = 0 
      end if


      call xvclose(inunit, status,' ')       ! close input
      if ( IFORMAT .eq. 'BYTE' ) then          ! reopen properly
         call xvopen(inunit, status, 'U_NL', NLI, 'U_NS', NBI,
     -                   'U_FORMAT','HALF','I_FORMAT','BYTE',' ')      
      else 
         call xvopen(inunit, status, 'U_NL', NLI, 'U_NS', NBI,
     -                   'U_FORMAT','HALF','I_FORMAT','HALF',' ')      
      end if

c
c     Open output files
c
      call xvunit(outunit(1), 'OUT', 1, status,' ') ! first output data set
      call xvunit(outunit(2), 'OUT', 2, status,' ') ! second output data set
      if ( ndso .gt. 2 ) then
          call xvunit(outunit(3), 'OUT', 3, status,' ') ! x derivative
          call xvunit(outunit(4), 'OUT', 4, status,' ') ! y derivative
      end if


      if ( centered ) then  ! set output lines and samples
          nlo1 = NLO_CENT
          nbo1 = NBO_CENT
      else
          nlo1 = nlo
          nbo1 = nbo
      end if

      call xvopen(outunit(1), status,   ! OPEN THE FIRST TWO DATA SETS
     -           'U_NL', NLO1, 'U_NS', NBO1,
     -           'U_FORMAT','HALF','O_FORMAT','BYTE','OP','WRITE',' ')  

      write (LBL,50) AAZ, BAZ
      if (VERBOSE) CALL xvmessage(LBL,' ')

      call xvopen(outunit(2), status,
     -           'U_NL', NLO1, 'U_NS', NBO1,
     -           'U_FORMAT','HALF','O_FORMAT','BYTE','OP','WRITE',' ')  

C
C     check for derivative outputs
C

      if ( ndso .gt. 2 ) then
          if (  FORMAT .eq. 0 ) then           ! byte data output
              call xvopen(outunit(3), status, 
     -           'U_NL', NLO1, 'U_NS', NBO1,
     -           'U_FORMAT','REAL','O_FORMAT','REAL','OP','WRITE',' ')  

              write (LBL,50) AAZ, BAZ
              if (VERBOSE) CALL xvmessage(LBL,' ')

              call xvopen(outunit(4), status, 
     -           'U_NL', NLO1, 'U_NS', NBO1,
     -           'U_FORMAT','REAL','O_FORMAT','REAL','OP','WRITE',' ')  
          else
              call xvopen(outunit(3), status, 
     -           'U_NL', NLO1, 'U_NS', NBO1,
     -           'U_FORMAT','REAL','O_FORMAT','REAL','OP','WRITE',' ')  

              write (LBL,50) AAZ, BAZ
              if (VERBOSE) CALL xvmessage(LBL,' ')

              call xvopen(outunit(4), status, 
     -           'U_NL', NLO1, 'U_NS', NBO1,
     -           'U_FORMAT','REAL','O_FORMAT','REAL','OP','WRITE',' ')  
          end if
      end if

      CALL INITL(VAL,VERBOSE)  ! initialize and print initial/default values

C------------------------------------------------------------------------
C
C                             PROCESS DATA
C
C------------------------------------------------------------------------

      BDIP=BDIP*ADIP
      BAZ=BAZ*AAZ

      NB=NBO
      NSO=NB

      IF ( ( SB-1 + NB ) .GT. NBI ) THEN
         CALL xvmessage('$$$ REQUESTED SIZE FIELD EXCEEDS',' ')
         CALL xvmessage('      INPUT PICTURE WIDTH $$$',' ')
         CALL abend
      END IF

      NBI=NSO

      write (LBL,50) ADIP, BDIP
      if (VERBOSE) CALL xvmessage(LBL,' ')

      IF ( SL .eq. 1 ) THEN                      ! read input
         call xvread(inunit,ALT(MAXSAMP+4),status,'LINE',SL,'SAMP',SB,
     -                  'NSAMPS',NB,' ')
         if ( status .ne. 1 ) then
            call xvmessage('Input error read. Program terminated.',' ')
            call abend
         end if
      else
         N = SL - 1
         call xvread(inunit,ALT(2),status,'LINE',N,'SAMP',SB,
     -                  'NSAMPS',NB,' ')
         if ( status .ne. 1 ) then
            call xvmessage('Input error read. Program terminated.',' ')
            call abend
         end if
      END IF

      ALT((MAXSAMP+3)) = ALT((MAXSAMP+4))
      ALT((MAXSAMP+4)+NSO) = ALT((MAXSAMP+3)+NSO)

      IF( SL .EQ. 1 ) THEN
         N = NSO + 2
         DO I=1,N
            ALT(I) = ALT(I+(MAXSAMP+2))
         END DO
      END IF

      ALT(1) = ALT(2)
      ALT(NSO+2) = ALT(NSO+1)

      IPNT=1
      JPNT=2
      NI=0
      NJ=(MAXSAMP+2)

C***************************************************************************
C        IPNT  POINTS TO CURRENT LINE (LINE TO WRITE)
C        JPNT  POINTS TO CURRENT+1 LINE
C        KPNT  POINTS TO CURRENT-1 LINE
C        LINE REFERS TO LINE BEING READ
C***************************************************************************

      now  = 2

      DO LINE = 2, NLO                              ! loop for lines out
         LIN=LINE-1
         IPNT=IPNT+1
         KPNT=IPNT-1
         JPNT=JPNT+1

         IF(IPNT.EQ.4) IPNT=1
         IF(JPNT.EQ.4) JPNT=1

         NK=NI
         NI=NJ
         NJ=(MAXSAMP+2)*(JPNT-1)
         N1=NI+2
         N2=NJ+1
         N3=NK+1
         N4=NJ+2

         call xvread(inunit,ALT(N4),status,'LINE',LINE,
     -              'SAMP',SB,'NSAMPS',NB,' ')
         if ( status .ne. 1 ) then
            call xvmessage('Input error read. Program terminated.',' ')
            call abend
         end if

         ALT(N4-1)=ALT(N4)
         ALT(N4+NSO)=ALT(N4+NSO-1)
 
         CALL CMPT(NI,N1,N2,N3)

         write(LBL,100) LINE
         if (VERBOSE) CALL xvmessage (LBL,' ')

         IF ( CENTERED ) THEN
             IF ( NOW .EQ. LINE ) THEN 
                 NOW = NOW + 3          ! UPDATE NEXT REQUIRED LINE #
                 K = 1                  ! START OF RESAMPLED ARRAYS
                 INC = 3                ! FOR CENTERED STUFF
                 DO J = 2,NBO,INC  ! RESAMPLE ARRAYS
                     TDIP(K) = DIP(J) 
                     TAZI(K) = AZIM(J)
                     TXBU(K) = XBUF(J)
                     TYBU(K) = YBUF(J)
                     K = K + 1                 
                 END DO
                 call xvwrit(outunit(1),TDIP,status,
     -                       'NSAMPS',NBO1,' ')
                 if ( status .ne. 1 ) then
                      call xvmessage('Input error read.',' ')
                      call abend
                 end if
                 call xvwrit(outunit(2),TAZI,status,
     -                       'NSAMPS',NBO1,' ')
                 if ( status .ne. 1 ) then
                      call xvmessage('Input error read.',' ')
                      call abend
                 end if
                 if ( NDSO .gt. 2 ) then    ! derivatives
                     call xvwrit(outunit(3),TXBU,status,
     -                       'NSAMPS',NBO1,' ')
                     if ( status .ne. 1 ) then
                          call abend
                     end if
                     call xvwrit(outunit(4),TYBU,status,
     -                       'NSAMPS',NBO1,' ')
                     if ( status .ne. 1 ) then
                         call xvmessage('Input error read.',' ')
                         call abend
                     end if
                 END IF
             end if
         ELSE
             call xvwrit(outunit(1),DIP,status,'NSAMPS',NSO,' ')
             if ( status .ne. 1 ) then
                 call xvmessage('Input error read.',' ')
                 call abend
             end if
             call xvwrit(outunit(2),AZIM,status,'NSAMPS',NSO,' ')
             if ( status .ne. 1 ) then
                 call xvmessage('Input error read.',' ')
                 call abend
             end if
             if ( NDSO .gt. 2 ) then    ! derivatives
                 call xvwrit(outunit(3),XBUF,status,'NSAMPS',NSO,' ')
                 if ( status .ne. 1 ) then
                     call xvmessage('Input error read.',' ')
                     call abend
                 end if
                 call xvwrit(outunit(4),YBUF,status,'NSAMPS',NSO,' ')
                 if ( status .ne. 1 ) then
                     call xvmessage('Input error read.',' ')
                     call abend
                 end if
             end if
         end if
      END DO

      LIN=LIN+1
      IPNT=IPNT+1
      JPNT=JPNT+1
      KPNT=IPNT-1
      IF(IPNT.EQ.4) IPNT=1
      IF(JPNT.EQ.4) JPNT=1
      NK=NI
      NI=NJ
      NJ=(MAXSAMP+2)*(JPNT-1)
      N1=NI+2
      N2=NJ+1
      N3=NK+1
      N4=NJ+2

      N=NSO+2
      DO I=1,N
         ALT(N2+I) = ALT(NI+1+I)
      END DO

      CALL CMPT(NI,N1,N2,N3)

      call xvwrit(outunit(1),DIP,status,'NSAMPS',NSO,' ')
      if ( status .ne. 1 ) then
         call xvmessage('Input error read. Program terminated.',' ')
         call abend
      end if
      call xvwrit(outunit(2),AZIM,status,'NSAMPS',NSO,' ')
      if ( status .ne. 1 ) then
         call xvmessage('Input error read. Program terminated.',' ')
         call abend
      end if

C**********************************************************************
C
C                       CLOSE DATA SETS
C
C**********************************************************************

      call xvclose(inunit, status,' ')          ! close input data set
      if ( status .ne. 1 ) call xvmessage('Input close error',' ')

      call xvclose(outunit(1), status,' ')    ! close output data sets
      if ( status .ne. 1 ) call xvmessage('Output close error',' ')
      call xvclose(outunit(2), status,' ')
      if ( status .ne. 1 ) call xvmessage('Output close error',' ')

      if ( ndso .gt. 2 ) then
          call xvclose(outunit(3), status,' ')
          if ( status .ne. 1 ) call xvmessage('Output close error',' ')
          call xvclose(outunit(4), status,' ')
          if ( status .ne. 1 ) call xvmessage('Output close error',' ')
      end if
 
50    format('GRADIENT DIRECTION PICTURE... DN=',F7.3,
     +       '*AZIMUTH(DEG).',F7.3)
100   format(' WRITING TO DISK',I12)

      RETURN
      END

C**************************************************************************
C  This subroutine computes the X, Y, Z for each sample and then uses
C  the results or defaults and computes the DIP and AZIMUTH arrays.
C***************************************************************************
      SUBROUTINE CMPT(NI,N1,N2,N3)
   
      IMPLICIT NONE

      COMMON ALT,DIP,AZIM,XSSC,YLSC,TADIP,TAAZ,TBDIP,TBAZ,NSO,
     +        TWOPI,RNRTH

      INTEGER   MAXSAMP              !If changed, be sure to change the
      PARAMETER (MAXSAMP=32000)      !  MAXSAMP parameter in CMPT, too!


      INTEGER*2 ALT(3*MAXSAMP+8),DIP(MAXSAMP),AZIM(MAXSAMP)

      REAL*4  XSSC                   ! init AALT/(5280.*LSCALE*N) N=(2,6,8)
      REAL*4  YLSC                   ! init AALT/(5280.*SSCALE*N) N=(2,6,8)
      REAL*4  TADIP                  ! init DN(DIP) = ADIP*(DIP+BDIP)
      REAL*4  TAAZ                   ! init DN(AZIM)=AAZ*(AZIMUTH+BAZ)
      REAL*4  TBDIP                  ! init DN(DIP) = ADIP*(DIP+BDIP)
      REAL*4  TBAZ                   ! init DN(AZIM)=AAZ*(AZIMUTH+BAZ)
      REAL*4  TWOPI                  ! 2.*PI
      REAL*4  RNRTH                  ! TWOPI-PIF*NORTH  
      REAL*4  X,Y,Z                  ! temporary variables
      REAL*4  PI                     ! value of PI
      REAL*4  A,B,C,D,E,F,G,H,J

      LOGICAL XVPTST

      INTEGER*4 NI,N1,N2,N3,NSO,ISMPL   ! temp variables

      DATA PI /3.1415927/

      COMMON/DERIVATIVE/XBUF,YBUF,FORMAT
      REAL*4     XBUF(MAXSAMP)            ! x partial derivative
      REAL*4     YBUF(MAXSAMP)            ! y partial derivative
      INTEGER*2  FORMAT                   ! format type


C   Picture:
C
C                 N3+ISMPL
C       NI+ISMPL            N1+ISMPL
C                 N2+ISMPL
C
C

      IF (XVPTST('ARCINFO')) THEN
        DO ISMPL=1,NSO
	   A = ALT(N3+ISMPL-1)
	   B = ALT(N3+ISMPL)
	   C = ALT(N3+ISMPL+1)
	   D = ALT(NI+ISMPL)
	   E = ALT(NI+ISMPL+1)
	   F = ALT(N1+ISMPL)
	   G = ALT(N2+ISMPL-1)
	   H = ALT(N2+ISMPL)
	   J = ALT(N2+ISMPL+1)
           X=((C+2*F+J) - (A+2*D+G))*XSSC  !Fixed 01/08/97 --ndr
           Y=((g+2*h+j) - (a+2*b+c))*YLSC  ! swapped bam - 12/98
           Z=X*X+Y*Y


                                     ! GET THE PARTIAL DERIVATIVES
           XBUF(ISMPL) = X 
           YBUF(ISMPL) = Y 
c           IF ( FORMAT .EQ. 0 ) THEN      ! SHRINK INTO A BYTE
c               XBUF(ISMPL) = XBUF(ISMPL) 
c               YBUF(ISMPL) = YBUF(ISMPL) 
c           END IF

           IF ( Z .LE. .0001 ) THEN
             DIP(ISMPL) = TBDIP
             AZIM(ISMPL)= TBAZ
           ELSE
             DIP(ISMPL)=TADIP*ATAN(SQRT(Z))+TBDIP
             AZIM(ISMPL)= TAAZ*AMOD(ATAN2(-X,Y)+RNRTH,TWOPI)+TBAZ
           END IF

        END DO

      ELSE IF (XVPTST('ERDAS')) THEN

        DO ISMPL=1,NSO
	   A = ALT(N3+ISMPL-1)
	   B = ALT(N3+ISMPL)
	   C = ALT(N3+ISMPL+1)
	   D = ALT(NI+ISMPL)
	   E = ALT(NI+ISMPL+1)
	   F = ALT(N1+ISMPL)
	   G = ALT(N2+ISMPL-1)
	   H = ALT(N2+ISMPL)
	   J = ALT(N2+ISMPL+1)
           X=((C+F+J) - (A+D+G))*XSSC  !Fixed 01/08/97 --ndr
           Y=((g+h+j) - (a+b+c))*YLSC  ! swapped bam - 12/98
           Z=X*X+Y*Y

           XBUF(ISMPL) = X 
           YBUF(ISMPL) = Y 
c           IF ( FORMAT .EQ. 0 ) THEN      ! SHRINK INTO A BYTE
c               XBUF(ISMPL) = XBUF(ISMPL) 
c               YBUF(ISMPL) = YBUF(ISMPL) 
c           END IF


           IF ( Z .LE. .0001 ) THEN
             DIP(ISMPL)=TBDIP
             AZIM(ISMPL)=TBAZ
           ELSE
             DIP(ISMPL)=TADIP*ATAN(SQRT(Z))+TBDIP
             AZIM(ISMPL)= TAAZ*AMOD(ATAN2(-X,Y)+RNRTH,TWOPI)+TBAZ
           END IF
        END DO
      ELSE
        DO ISMPL=1,NSO
           X=(ALT(N1+ISMPL)-ALT(NI+ISMPL))*XSSC
           Y=(ALT(N2+ISMPL)-ALT(N3+ISMPL))*YLSC
           Z=X*X+Y*Y

           XBUF(ISMPL) = X 
           YBUF(ISMPL) = Y 
c           IF ( FORMAT .EQ. 0 ) THEN      ! SHRINK INTO A BYTE
c               XBUF(ISMPL) = XBUF(ISMPL) 
c               YBUF(ISMPL) = YBUF(ISMPL) 
c           END IF

           IF ( Z .LE. .0001 ) THEN
             DIP(ISMPL)=TBDIP
             AZIM(ISMPL)=TBAZ
           ELSE
             DIP(ISMPL)=TADIP*ATAN(SQRT(Z))+TBDIP
             AZIM(ISMPL)= TAAZ*AMOD(ATAN2(-X,Y)+RNRTH,TWOPI)+TBAZ
           END IF
        END DO
      END IF
      RETURN
      END

C------------------------------------------------------------------------
C  INITL is an entry point to subroutine CMPT.  THis entry point is used
C  only once to intiialize or set the default values for user parameters.
C  The user parameters or defaults are printed out and saved via common
C  for subsequent calls to the CMPT subroutine. 

C  BAM - MODIFIED TO A SUBROUTINE SO THAT IT CAN BE LOOKED AT IN THE DEBUGGER

C------------------------------------------------------------------------

      SUBROUTINE INITL(VAL,VERBOSE)

      implicit none

      COMMON ALT,DIP,AZIM,XSSC,YLSC,TADIP,TAAZ,TBDIP,TBAZ,NSO,
     +        TWOPI,RNRTH

      INTEGER   MAXSAMP              !If changed, be sure to change the
      PARAMETER (MAXSAMP=32000)      !  MAXSAMP parameter in CMPT, too!

      INTEGER   NSO

      INTEGER*2 ALT(3*MAXSAMP+8),DIP(MAXSAMP),AZIM(MAXSAMP)


      REAL*4  XSSC                   ! init AALT/(5280.*LSCALE*N) N=(2,6,8)
      REAL*4  YLSC                   ! init AALT/(5280.*SSCALE*N) N=(2,6,8)
      REAL*4  TADIP                  ! init DN(DIP) = ADIP*(DIP+BDIP)
      REAL*4  TAAZ                   ! init DN(AZIM)=AAZ*(AZIMUTH+BAZ)
      REAL*4  TBDIP                  ! init DN(DIP) = ADIP*(DIP+BDIP)
      REAL*4  TBAZ                   ! init DN(AZIM)=AAZ*(AZIMUTH+BAZ)
      REAL*4  TWOPI                  ! 2.*PI
      REAL*4  RNRTH                  ! TWOPI-PIF*NORTH  
      REAL*4  VAL(8)                 ! FOR SUBROUTINE EQUIVALENCE
      REAL*4  PI                     ! value of PI
      REAL*4  PIF                    ! value of PI/180
      REAL*4  VAL9                   ! To print zero

      LOGICAL VERBOSE,XVPTST
      DATA PI /3.1415927/            ! value of pi
      CHARACTER*80 MSG1

      PIF = PI / 180. 
      VAL9 = 0.0
      WRITE (MSG1,200) VAL(8), VAL9
      if (VERBOSE) call xvmessage(MSG1,' ')
      WRITE (MSG1,500) VAL(6)
      if (VERBOSE) call xvmessage(MSG1,' ')
      WRITE (MSG1,600) VAL(7)
      if (VERBOSE) call xvmessage(MSG1,' ')
      WRITE (MSG1,300) VAL(2), VAL(3)
      if (VERBOSE) call xvmessage(MSG1,' ')
      WRITE (MSG1,400) VAL(4), VAL(5)
      if (VERBOSE) call xvmessage(MSG1,' ')

      XSSC  = VAL(8)/(5280.*VAL(6))
      YLSC  = VAL(8)/(5280.*VAL(7))

      if (XVPTST('ARCINFO')) then
        XSSC  = XSSC/8.0
        YLSC  = YLSC/8.0
      else if (XVPTST('ERDAS')) then
        XSSC  = XSSC/6.0
        YLSC  = YLSC/6.0
      else  ! standard
        XSSC  = XSSC/2.0
        YLSC  = YLSC/2.0
      end if

      TADIP  = VAL(2)/PIF
      TAAZ   = VAL(4)/PIF
      TWOPI = 2.*PI
      RNRTH = TWOPI-PIF*VAL(1)
      TBDIP  = VAL(3)+.5
      TBAZ   = VAL(5)+.5

200   format('             ALTITUDE PICTURE...      ALT=', F8.3,
     +       ' * DN   +',F9.3,' FT')
300   format('   GRADIENT MAGNITUDE PICTURE...       DN=', F8.3,
     +       ' * DIP  +',F9.3)
400   format('   GRADIENT DIRECTION PICTURE...       DN=', F8.3,
     +       '*AZIMUTH+',F9.3)
500   format('         SAMPLE SCALE IS ',F8.4,' MILES/PXL')
600   format('           LINE SCALE IS ',F8.4,' MILES/PXL')
      RETURN
      END

