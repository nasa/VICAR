C 2 January 1995 ... CRI ... MSTP S/W Conversion (VICAR Porting)
C
      INCLUDE 'VICMAIN_FOR'

      SUBROUTINE MAIN44

      integer F2RBUF(12)            ! Full to Real translation buffer 
      integer OFFS,N,TYPE

      integer   i2offs,i4offs, R4OFFS1, R4OFFS2, workoffs1, workoffs2

      integer*2 I2BUF (100)
      integer   I4BUF (100)
      real      R4BUF1 (100), R4BUF2 (100)
      common/buffers/I2BUF,I4BUF,R4BUF1,R4BUF2

      data    I2BUF /100*0/,I4BUF /100*0/, OFFS/0/, N/0/, TYPE/0/
      data    R4BUF1 /100*0.0/, R4BUF2 /100*0.0/
      
!     Initialize offsets into AP memory array      
      i2offs = 1000               ! I*2 Working area
      i4offs = 2000               ! I*4 Working area
      R4OFFS1 = 3000              ! R*4 Working area #1
      R4OFFS2 = 4000              ! R*4 Working area #2
      workoffs1 = 5000            ! Out put work offset
      workoffs2 = 6000            ! Out put work offset


      call xvtrans_set (F2RBUF,'FULL','REAL',ISTAT)
      if (ISTAT .ne. 1) then
         call mabend ('TFPSE: Full to Real translation error',' ')
      endif

      call IFMESSAGE ('TFPSE version 2-Jan-95')

! Call FPSE subroutine APINIT
      CALL XVMESSAGE ('Call APINIT',' ')
      CALL  APINIT()

! Call FPSE subroutine APWD
      CALL XVMESSAGE ('Call  APWD',' ')
      CALL  APWD()


! Call FPSE subroutine APWR
      CALL XVMESSAGE ('Call  APWR',' ')
      CALL  APWR()

!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Begin test of FPSE

!    
!     Clear segmentS of memory to be used for test
      CALL XVMESSAGE ('VCLR  - Clear segments of AP memory',' ')
      CALL VCLR (I2OFFS,1,100)    !! Clear each word of INTEGER*2 AREA
      CALL VCLR (I4OFFS,1,100)    !! Clear each word of INTEGER*4 AREA
      CALL VCLR (R4OFFS1,1,100)   !! Clear each word of REAL*4 AREA
      CALL VCLR (R4OFFS2,1,100)   !! Clear each word of REAL*4 AREA
      CALL VCLR (WORKOFFS1,1,100) !! Clear each word of WORK AREA #1
      CALL VCLR (WORKOFFS2,1,100) !! Clear each word of WORK AREA #2

! Get and display cleared AP memory segments of
      CALL XVMESSAGE 
     &('APGET - Get and display cleared AP memory segments',' ')

! Get 100 entries from AP memory in I*2 format into I2BUF 
      TYPE = 1                     !! Return I*2 format
      CALL APGET (I2BUF, I2OFFS, 100, TYPE)
      CALL DISPLAY_I2BUF()

! Get 100 entries from AP memory in I*4 format into I4BUF 
      TYPE = 0                     !! Return I*4 format
      CALL APGET (I4BUF, I4OFFS, 100, TYPE)
      call display_i4buf()

! Get 100 entries from AP memory in R*4 format into R4BUF1 
      TYPE = 2                     !! Return R*4 format
      CALL APGET (R4BUF1, R4OFFS1, 100, TYPE)
      call display_R4BUF1()

! initialize SEGMENTS of local memory
      DO I = 1,100
        I2BUF(I) = I
        I4BUF(I) = I
        CALL XVTRANS (F2RBUF,I,R4BUF1(I),1)
        R4BUF1(I) = R4BUF1(I) * 0.1
      END DO

! Put local memory to AP memory
c Reformatting will be performed by FPSE as specified by TYPE:
c	TYPE = 0: in/out = I*4 (then use VPK8, VFIX32)
c	       1: in = I*4, out = I*2 ( then use VFIX)
c	       2: in/out = R*4
c	       3: in/out = R*4 (ignore "IBM format")
c
      CALL XVMESSAGE 
     & ('APPUT - Put initialized memory segments into AP memory',' ')
      CALL APPUT (I2BUF, I2OFFS,100,1)
      CALL APPUT (I4BUF, I4OFFS,100,0)
      CALL APPUT (R4BUF1,R4OFFS1,100,2)

! GET segments of AP memory to check for initialized values

! But first clear local memory segments to zero
      DO I = 1,100
        I2BUF(I) = 0
        I4BUF(I) = 0
        R4BUF1(I) = 0.0
      END DO

! Get 100 entries from AP memory in I*2 format into I2BUF
      CALL XVMESSAGE 
     &('APGET - Get and display initialized AP memory segments',' ')

      TYPE = 1                     !! Return I*2 format
      CALL APGET (I2BUF, I2OFFS, 100, TYPE)
      call display_i2buf()

! Get 100 entries from AP memory in I*4 format into I4BUF 
      TYPE = 0                     !! Return I*4 format
      CALL APGET (I4BUF, I4OFFS, 100, TYPE)
      call display_I4buf()

! Get 100 entries from AP memory in R*4 format into R4BUF1 
      TYPE = 2                     !! Return R*4 format
      CALL APGET (R4BUF1, R4OFFS1, 100, TYPE)
      call display_R4BUF1()
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Correlate or convolute arrays A and B to obtain C
!
! initialize SEGMENTS of local memory
      DO I = 1,100
        RTEMP = REAL(I)
        R4BUF1(I) = RTEMP * 0.25 + 50.0
        R4BUF2(I) = 50.0 - RTEMP * 0.25
      END DO

! Put local memory to AP memory
c Reformatting will be performed by FPSE as specified by TYPE:
c	TYPE = 0: in/out = I*4 (then use VPK8, VFIX32)
c	       1: in = I*4, out = I*2 ( then use VFIX)
c	       2: in/out = R*4
c	       3: in/out = R*4 (ignore "IBM format")
c
      CALL XVMESSAGE 
     & ('APPUT - Put initialized memory segments into AP memory',' ')
      CALL APPUT (R4BUF1,R4OFFS1,100,2)
      CALL APPUT (R4BUF2,R4OFFS2,100,2)

      call xvmessage('CONV  - Correlate arrays',' ')
      CALL CONV (R4OFFS1,1,R4OFFS2,1,WORKOFFS1,1,90,10)

! Get100 entries from AP memory in R*4 format into R4BUF1 
      call xvmessage('APGET - Display Correlated array',' ')
      TYPE = 2                     !! Return R*4 format
      CALL APGET (R4BUF1, WORKOFFS1, 100, TYPE)
      call display_R4BUF1()

! Clear AP memory working segment 
      CALL VCLR (WORKOFFS1,1,100)    !! Clear each word of WORK AREA

! Convulate arrays
      call xvmessage('CONV  - Convolute arrays',' ')
      CALL CONV (R4OFFS1,1,R4OFFS2,-1,WORKOFFS1,1,90,10)

! Get100 entries from AP memory in R*4 format into R4BUF1 
      call xvmessage('APGET - Display convoluted array',' ')
      TYPE = 2                     !! Return R*4 format
      CALL APGET (R4BUF1, WORKOFFS1, 100, TYPE)
      call display_R4BUF1()
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Generate histogram of an array starting at A, increment I
! with limits AMAX, AMIN, and stored in array C

      call xvmessage('HIST  - Create histogram',' ')
! Clear AP memory working area
      CALL VCLR (WORKOFFS1,1,100)    !! Clear work area

! initialize SEGMENTS of local memory to known values
      DO I = 1,100
        RTEMP = REAL(I)
        R4BUF1(I) = RTEMP * 2.5 + 100.0
        R4BUF2(I) = RTEMP * 2.5 + 100.0
      END DO

! Put segments in AP memory
      CALL APPUT (R4BUF1, R4OFFS1,100,2)
      CALL APPUT (R4BUF2, R4OFFS2,100,2)

! Create histogram
      CALL HIST (R4OFFS1,1,WORKOFFS1,100,70,R4OFFS2+70,R4OFFS2+20)

! Get100 entries from AP memory (in R*4 format) into R4BUF1 
      call xvmessage('APGET - Get and display histogram array',' ')

! Call APGET to retrieve working array from AP memory
      TYPE = 2                     !! Return R*4 format
      CALL APGET (R4BUF1, WORKOFFS1,100, TYPE)
      call display_R4BUF1()
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! MMUL32 - Matrix multiply arrays A and B to obtain C
! 
      call xvmessage('MMUL32- Matrix multiply arrays A and B',' ')
! Clear AP memory working area
      CALL VCLR (WORKOFFS1,1,100)    !! Clear work area

! initialize SEGMENTS of local memory to known values
      DO I = 1,100
        RTEMP = REAL(I)
        R4BUF1(I) = RTEMP * 0.1 + 100.0
        R4BUF2(I) = 100.0 - RTEMP * 0.1
      END DO

! Put segments in AP memory
      CALL APPUT (R4BUF1, R4OFFS1,100,2)
      CALL APPUT (R4BUF2, R4OFFS2,100,2)

! Perform Matrix Multioplication
      CALL MMUL32 (R4OFFS1,1,R4OFFS2,1,WORKOFFS1,1,100,10,10)

! Get100 entries from AP memory (in R*4 format) into R4BUF1 
      call xvmessage('APGET - Get and display MMUL32 results',' ')

! Call APGET to retrieve working array from AP memory
      TYPE = 2                     !! Return R*4 format
      CALL APGET (R4BUF1, WORKOFFS1,100, TYPE)
      call display_R4BUF1()
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Add arrays A and B to obtain C
! 

      call xvmessage('VADDEM  - Add arrays A and B',' ')
! Clear AP memory working area
      CALL VCLR (WORKOFFS1,1,100)    !! Clear work area

! initialize SEGMENTS of local memory to known values
      DO I = 1,100
        RTEMP = REAL(I)
        R4BUF1(I) = (100.0 + RTEMP * 0.1) * 100.0
        R4BUF2(I) = 100.0 + RTEMP * 0.1
      END DO

! Put segments in AP memory
      CALL APPUT (R4BUF1, R4OFFS1,100,2)
      CALL APPUT (R4BUF2, R4OFFS2,100,2)

! Add arrays A and B to obtain C
      CALL VADDEM (R4OFFS1,1,R4OFFS2,1,WORKOFFS1,1,100)

! Get100 entries from AP memory (in R*4 format) into R4BUF1 
      call xvmessage('APGET - Get and display VADDEM results',' ')

! Call APGET to retrieve working array from AP memory
      TYPE = 2                     !! Return R*4 format
      CALL APGET (R4BUF1, WORKOFFS1,100, TYPE)
      call display_R4BUF1()
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! VCLIP - Move Array A to D, clipping it to range (B to C)
! 
      call xvmessage('VCLIP - Move array A to D & clip',' ')
! Clear AP memory working area
      CALL VCLR (WORKOFFS1,1,100)    !! Clear work area
      CALL VCLR (R4OFFS2  ,1,10)     !! Clear work area

! initialize SEGMENTS of local memory to known values
      DO I = 1,100
        RTEMP = REAL(I)
        R4BUF1(I) = RTEMP * 0.25 + 100.0
      END DO
      R4BUF2(1) =  105.5
      R4BUF2(2) =  120.3

! Put segments into AP memory
      CALL APPUT (R4BUF1, R4OFFS1,100,2)
      CALL APPUT (R4BUF2, R4OFFS2,2,2)

! Move array and clip
      CALL VCLIP (R4OFFS1,1,R4OFFS2,R4OFFS2+1,WORKOFFS1,1,100)

! Get100 entries from AP memory (in R*4 format) into R4BUF1 
      call xvmessage('APGET - Get and display VCLIP results',' ')

! Call APGET to retrieve working array from AP memory
      TYPE = 2                     !! Return R*4 format
      CALL APGET (R4BUF1, WORKOFFS1,100, TYPE)
      call display_R4BUF1()
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! VFIX - Convert Elements from floating-point to Integer
! 

      call xvmessage
     & ('VFIX  - Convert Elements from floating-point to Integer',' ')
! Clear AP memory working area
      CALL VCLR (WORKOFFS1,1,100)    !! Clear work area

! initialize SEGMENTS of local memory to known real values
      DO I = 1,100
        R4BUF1(I) = real(I) * 1.25
      END DO

! Put segments in AP memory
      CALL APPUT (R4BUF1, R4OFFS1,100,2)

! Convert float to integer
      CALL VFIX (R4OFFS1,1,WORKOFFS1,1,100)

! Get100 entries from AP memory into I4BUF
      call xvmessage('APGET - Get and display VFIX results',' ')

! Call APGET to retrieve working array from AP memory
      TYPE = 0                     !! Return I*4 format
      CALL APGET (I4BUF, WORKOFFS1,100, TYPE)
      call display_I4BUF()
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! VFIX32 - Convert elements from floating point to integer
! 

      call xvmessage
     & ('VFIX32  - Convert from floating point to integer',' ')

! Clear AP memory working area
      CALL VCLR (WORKOFFS1,1,100)    !! Clear work area

! initialize SEGMENTS of local memory to known real values
      DO I = 1,100
        R4BUF1(I) = real(I) * 1.25
      END DO

! Put segments in AP memory
      CALL APPUT (R4BUF1, R4OFFS1,100,2)

! Convert float to integer
      CALL VFIX32 (R4OFFS1,1,WORKOFFS1,1,100)

! Get100 entries from AP memory into I4BUF
      call xvmessage('APGET - Get and display VFIX32 results',' ')

! Call APGET to retrieve working array from AP memory
      TYPE = 0                     !! Return I*4 format
      CALL APGET (I4BUF, WORKOFFS1,100, TYPE)
      call display_I4BUF()

!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! VFLT - Convert elements from integer to floating point
! 

      call xvmessage
     & ('VFLT  - Convert elements from integer to floating pointr',' ')
! Clear AP memory working area
      CALL VCLR (WORKOFFS1,1,100)    !! Clear work area

! initialize SEGMENTS of local memory to known values
      DO I = 1,100
        I4BUF(I) = I * 100 
      END DO

! Put segments in AP memory
      CALL APPUT (I4BUF, I4OFFS,100,0)

! Perform Integer to Floating point conversion
      CALL VFLT (I4OFFS,1,WORKOFFS1+20,1,20)

! Get100 entries from AP memory (in R*4 format) into R4BUF1 
      call xvmessage('APGET - Get and display VFLT results',' ')

! Call APGET to retrieve working array from AP memory
      TYPE = 2                     !! Return R*4 format
      CALL APGET (R4BUF1, WORKOFFS1,100, TYPE)
      call display_R4BUF1()
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! VFLT32 - Convert elements from integer to floating point
! 

      call xvmessage
     & ('VFLT32  - Convert from integer to floating point',' ')
! Clear AP memory working area
      CALL VCLR (WORKOFFS1,1,100)    !! Clear work area

! initialize SEGMENTS of local memory to known values
      DO I = 1,100
        I4BUF(I) = I * 100
      END DO

! Put segments into AP memory
      CALL APPUT (I4BUF, I4OFFS,100,0)

! Perform Integer to Floating point conversion
      CALL VFLT32 (I4OFFS,1,WORKOFFS1+20,1,20)

! Get100 entries from AP memory (in R*4 format) into R4BUF1 
      call xvmessage('APGET - Get and display VFLT32 results',' ')

! Call APGET to retrieve working array from AP memory
      TYPE = 2                     !! Return R*4 format
      CALL APGET (R4BUF1, WORKOFFS1,100, TYPE)
      call display_R4BUF1()
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! VMOV - Move array A to Array C
! 

      call xvmessage
     & ('VMOV  - Move array A to C',' ')

! Clear AP memory working area
      CALL VCLR (WORKOFFS1,1,100)    !! Clear work area

! initialize SEGMENTS of local memory to known values
      DO I = 1,100
        R4BUF1(I) = real(I) * 123.5
      END DO

! Put real segments into AP memory
      CALL APPUT (R4BUF1, R4OFFS1,100,2)

! Move data
      CALL VMOV (R4OFFS1,1,WORKOFFS1,1,100)

! Get100 entries from AP memory (in R*4 format) into R4BUF1 
      call xvmessage('APGET - Get and display VMOV results',' ')

! Call APGET to retrieve working array from AP memory
      TYPE = 2                     !! Return I*4 format
      CALL APGET (R4BUF1, WORKOFFS1,100, TYPE)
      call display_R4BUF1()
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! VPK8 - Packs LSB from 4 words of A into a single word C.
! 

      call xvmessage
     & ('VPK8  - Pack low bytes of 4 words of A into 1 word of C',' ')
! Clear AP memory working area
      CALL VCLR (WORKOFFS1,1,100)    !! Clear work area

! initialize SEGMENTS of local memory to known values
      DO I = 1,100
        R4BUF1(I) = 16909060.0   !! '01020304'X
      END DO

! Put segments in AP memory
      CALL APPUT (R4BUF1, R4OFFS1,100,0)

! Pack data
      CALL VPK8 (R4OFFS1,1,WORKOFFS1,1,100)

! Get packed entries from AP memory
      call xvmessage('APGET - Get and display VPK8 results',' ')

! Call APGET to retrieve working array from AP memory
      TYPE = 0                     !! Return I*4 format
      CALL APGET (I4BUF, WORKOFFS1,100, TYPE)
      call display_I4BUF5()
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! VSADD - Add array A to Scaler B to obtain C
! 
      call xvmessage
     & ('VSADD - Add array A to Scaler B',' ')
! Clear AP memory working area
      CALL VCLR (WORKOFFS1,1,100)    !! Clear work area

! initialize SEGMENTS of local memory to known values
      DO I = 1,100
        RealTemp = real(i)
        R4BUF1(I) = RealTemp * 1.1 + 100.0
        R4BUF2(I) = 100.0 - RealTemp * 1.1
      END DO

! Put segments in AP memory
      CALL APPUT (R4BUF1, R4OFFS1,100,2)
      CALL APPUT (R4BUF2, R4OFFS2,100,2)

! Move data
      CALL VSADD (R4OFFS1,1,R4OFFS2+50,WORKOFFS1,1,100)

! Get 100 entries from AP memory
      call xvmessage('APGET - Get and display VSADD results',' ')

! Call APGET to retrieve working array from AP memory
      TYPE = 2                     !! Return R*4 format
      CALL APGET (R4BUF1, WORKOFFS1,100, TYPE)
      call display_R4BUF1()
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! VSMUL - Multiply array A and Scaler B to obtain C
! 
      call xvmessage
     & ('VSMUL - Multiply array A by Scaler B',' ')
! Clear AP memory working area
      CALL VCLR (WORKOFFS1,1,100)    !! Clear work area
      DO I = 1,100
         R4BUF2(I) = 0.0
      END DO
      CALL APPUT (R4BUF2,R4OFFS1-50, 100,2)
      CALL APPUT (R4BUF2,R4OFFS1+50, 100,2)
      CALL APPUT (R4BUF2,R4OFFS2-50, 100,2)
      CALL APPUT (R4BUF2,R4OFFS2+50, 100,2)
      CALL APPUT (R4BUF2,WORKOFFS1-50, 100,2)
      CALL APPUT (R4BUF2,WORKOFFS2+50, 100,2)

! initialize SEGMENTS of local memory to known values
      DO I = 1,100
         RealTemp = real(I+100) * 1.1       
         R4BUF1(I) = RealTemp
      END DO

! Put segments in AP memory
      CALL APPUT (R4BUF1,R4OFFS1, 100,2)
      CALL APPUT (50.2,  R4OFFS2, 100,2)

! Multiply data 
      CALL VSMUL (R4OFFS1,1,R4OFFS2,WORKOFFS1,1,100)

! Get 100 entries from AP memory
      call xvmessage('APGET - Get and display VSMUL results',' ')

! Call APGET to retrieve working array from AP memory
      TYPE = 2                     !! Return R*4 format
      CALL APGET (R4BUF2, WORKOFFS1,100, TYPE)
      call display_R4BUF2()

!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! VUP8 - Unpacks 4 bytes, from four bytes of word A, into 4 words of C
! 
      call xvmessage
     & ('VUP8 - Unpack four bytes of A into four words of C',' ')

! Clear AP memory working area
      CALL VCLR (WORKOFFS1,1,100)    !! Clear work area

! initialize SEGMENTS of local memory to known values
      
      DO I = 1,100
         I4BUF(I) = '01020304'X     !! Assign with => (01020304) hex 
      END DO

! Put segments in AP memory
      CALL APPUT (I4BUF,I4OFFS,100,0)

! Unpack data
      CALL VUP8 (I4OFFS,1,WORKOFFS1,1,25)

! Get 100 entries from AP memory
      call xvmessage('APGET - Get and display VUP8 results',' ')

! Call APGET to retrieve working array from AP memory
      TYPE = 2                     !! Return I*4 format
      CALL APGET (R4BUF1, WORKOFFS1,100, TYPE)
      call display_R4BUF1()

! Convert floating point to fixed point format
      call VFIX (WORKOFFS1,1,WORKOFFS2,1,100)
      call APGET (I4BUF,WORKOFFS2,100,0)
      CALL DISPLAY_I4BUF()

      return
      end

!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      SUBROUTINE DISPLAY_I2BUF()

      integer*2 I2BUF (100)
      integer   I4BUF (100)
      real      R4BUF1 (100), R4BUF2 (100)
      common/buffers/I2BUF,I4BUF,R4BUF1,R4BUF2

      character*132 string
      integer J

      CALL XVMESSAGE ('I2BUF',' ')
      DO J = 1,100,10
      WRITE (STRING, 90010) 
     &  I2BUF(J+00), I2BUF(J+01), I2BUF(J+02), I2BUF(J+03), I2BUF(J+04),
     &  I2BUF(J+05), I2BUF(J+06), I2BUF(J+07), I2BUF(J+08), I2BUF(J+09)
90010 FORMAT (10I8)
      CALL XVMESSAGE (STRING,' ')
      END DO

      RETURN
      END

!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      
      SUBROUTINE DISPLAY_I4BUF()

      integer*2 I2BUF (100)
      integer   I4BUF (100)
      real      R4BUF1 (100), R4BUF2 (100)
      common/buffers/I2BUF,I4BUF,R4BUF1,R4BUF2

      character*132 string
      integer I, J

      CALL XVMESSAGE ('I4BUF',' ')
      DO I = 1,100,10
      J = I
      WRITE (STRING, 90010) 
     &  I4BUF(J+00), I4BUF(J+01), I4BUF(J+02), I4BUF(J+03), I4BUF(J+04),
     &  I4BUF(J+05), I4BUF(J+06), I4BUF(J+07), I4BUF(J+08), I4BUF(J+09)
90010 FORMAT (10I8)
      CALL XVMESSAGE (STRING,' ')
      END DO

      RETURN
      END
                           
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      
      SUBROUTINE DISPLAY_I4BUF5()

      integer*2 I2BUF (100)
      integer   I4BUF (100)
      real      R4BUF1 (100), R4BUF2 (100)
      common/buffers/I2BUF,I4BUF,R4BUF1,R4BUF2

      character*132 string
      integer J

      CALL XVMESSAGE ('I4BUF',' ')
      DO J = 1,25,5
      WRITE (STRING, 90010) 
     &  I4BUF(J+00), I4BUF(J+01), I4BUF(J+02), I4BUF(J+03), I4BUF(J+04)
90010 FORMAT (5I16)
      CALL XVMESSAGE (STRING,' ')
      END DO

      RETURN
      END
                           
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      SUBROUTINE DISPLAY_R4BUF1()

      integer*2 I2BUF (100)
      integer   I4BUF (100)
      real      R4BUF1 (100), R4BUF2 (100)
      common/buffers/I2BUF,I4BUF,R4BUF1,R4BUF2

      character*132 string
      integer J

      CALL XVMESSAGE ('R4BUF1',' ')
      DO J = 1,100,5
      WRITE (STRING, 90010) 
     &  R4BUF1(J+00), R4BUF1(J+01), R4BUF1(J+02), R4BUF1(J+03), 
     &  R4BUF1(J+04)
90010 FORMAT (5F16.4)
      CALL XVMESSAGE (STRING,' ')
      END DO

      RETURN
      END

!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      SUBROUTINE DISPLAY_R4BUF2()

      integer*2 I2BUF (100)
      integer   I4BUF (100)
      real      R4BUF1 (100), R4BUF2 (100)
      common/buffers/I2BUF,I4BUF,R4BUF1,R4BUF2

      character*132 string
      integer J

      CALL XVMESSAGE ('R4BUF2',' ')
      DO J = 1,100,5       !corrected step to 5; was skipping parts of R4BUF2
      WRITE (STRING, 90010) 
     &  R4BUF2(J+00), R4BUF2(J+01), R4BUF2(J+02), R4BUF2(J+03), 
     &  R4BUF2(J+04)
90010 FORMAT (5F16.4)
      CALL XVMESSAGE (STRING,' ')
      END DO

      RETURN
      END

