      INCLUDE 'VICMAIN_FOR'
C
C         TEST PROGRAM FOR CONVEV SUBPROGRAM
      SUBROUTINE MAIN44
      IMPLICIT NONE
      REAL CONV(4000),RLOC(404)
      INTEGER I,J

      COMMON IND,LINE,SAMP,LAT,LONG,MODE,DATA
      REAL LINE,SAMP,LAT,LONG,DATA(40)
      REAL*8 OM1(3,3),RS1(3)
      INTEGER IND,MODE,IDATA(40)
      EQUIVALENCE (DATA,IDATA,OM1),(DATA(19),RS1)

      DATA ((OM1(I,J),I=1,3),J=1,3)/0.40706,0.11232,0.90647,-0.87481,
     1                 -0.23747,0.42227,0.26269,-0.96488,0.0015944/,
     2     (RS1(I),I=1,3)/-730950.0,-339690.0,-1209.3/,
     3     (DATA(I),I=25,35),DATA(38)/1815.0,1815.0,1502.4,500.0,
     4           500.0,84.821,-0.02489,155.07,400.,400.,15.056,
     5           806060.0/,
     6     IDATA(36),IDATA(37),IDATA(39),IDATA(40)/6,1,7,0/

      CALL XVMESSAGE('   ',' ')
      CALL XVMESSAGE(
     .     '********** TEST VGR IMAGE SPACE/ OBJECT SPACE ******',' ')
      CALL GETRES(RLOC,7)
      CALL GEOMAV(CONV,7,RLOC)
      MODE=1
      LINE = 400.0
      SAMP = 400.0
      LAT = -13.8018
      LONG = 150.1259
      CALL XVMESSAGE('BEFORE LAT/LONG TO LINE/SAMP - RAW PICTURE',' ')
      CALL DUMP(1)
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER LAT/LONG TO LINE/SAMP - IMAGE SPACE',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(1)
      MODE=2
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER LINE/SAMP TO LAT/LONG - OBJECT SPACE',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(1)
      CALL XVMESSAGE(
     *'ANSWER: IMAGE SPACE LINE 500 SAMP 500 = LAT-13.8 LONG 150.1',' ')
      CALL XVMESSAGE('AND OBJECT SPACE LINE 614.4, SAMP 615.1',' ')

      CALL XVMESSAGE('   ',' ')
      CALL XVMESSAGE(
     .     '********** TEST GLL IMAGE SPACE/ OBJECT SPACE ******',' ')
      CALL MVCL('GLL  ',conv,5)
      CALL MVE(4,1,1,conv(3),1,1)
      MODE=1
      LINE = 400.0
      SAMP = 400.0
      LAT = -13.8018
      LONG = 150.1259
      CALL XVMESSAGE('BEFORE LAT/LONG TO LINE/SAMP - RAW PICTURE',' ')
      CALL DUMP(1)
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER LAT/LONG TO LINE/SAMP - IMAGE SPACE',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(1)
      MODE=2
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER LINE/SAMP TO LAT/LONG - OBJECT SPACE',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(1)
      CALL XVMESSAGE(
     *'ANSWER: IMAGE SPACE LINE 500 SAMP 500 = LAT-13.8 LONG 150.1',' ')
      CALL XVMESSAGE('AND OBJECT SPACE LINE 614.4, SAMP 615.1',' ')

      call XVMESSAGE('  ',' ')
      CALL XVMESSAGE('********** TEST PERSPECTIVE ******',' ')
      idata(39)=16
      MODE=1
      LINE = 400.0
      SAMP = 400.0
      LAT = -13.8018
      LONG = 150.1259
      CALL XVMESSAGE('BEFORE LAT/LONG TO LINE/SAMP ',' ')
      CALL DUMP(1)
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER LAT/LONG TO LINE/SAMP - OBJECT SPACE',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(1)
      MODE=2
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER LINE/SAMP TO LAT/LONG - OBJECT SPACE',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(1)
      CALL XVMESSAGE(
     *'ANSWER: IMAGE SPACE LINE 500 SAMP 500 = LAT-13.8 LONG 150.1',' ')
      CALL XVMESSAGE('AND OBJECT SPACE LINE 614.4, SAMP 615.1',' ')


      call XVMESSAGE('  ',' ')
      CALL XVMESSAGE('************** TEST MERCATOR ***************',' ')
      IDATA(39)=6
      MODE=1
      LAT=-10.0
      LONG=200.
      DO I=1,24
         DATA(I) = 1.0
      ENDDO
      DATA(3)=50.
      DATA(6)=360.
      DATA(7)=100.
      CALL XVMESSAGE(
     .     'BEFORE:LAT/LONG TO LINE/SAMP W/MERCATOR PROJECTION',' ')
      LINE=0.
      SAMP=0.
      CALL DUMP(2)
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE(
     .     'AFTER:LAT/LONG TO LINE/SAMP W/MERCATOR PROJECTION',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)
      MODE=2
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL XVMESSAGE(
     .     'AFTER:LINE/SAMP TO LAT/LONG W/MERCATOR PROJECTION',' ')
      CALL DUMP(2)
      CALL XVMESSAGE(
     .     'ANSWER: LATI=-10,LONG=200,LINE=22.53,SAMP=51.68',' ')

      call XVMESSAGE('  ',' ')
      CALL XVMESSAGE('********* TEST NORMAL CYLINDIRICAL ********',' ')
      IDATA(39)=9
      MODE=1
      LAT=-10.0
      LONG=200.
      DO I=1,24
         DATA(I) = 1.0
      ENDDO
      DATA(1)=1.
      DATA(2)=1.
      DATA(3)=85.7461
      DATA(6)=239.916
      DATA(7) = 10.
      CALL CYLPATCH(DATA) !computes data(2,3,1,6)
      CALL XVMESSAGE(
     .     'BEFORE:LAT/LONG TO LINE/SAMP W/NORM CYL PROJECTION',' ')
      LINE=0.
      SAMP=0.
      CALL DUMP(2)
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE(
     .     'AFTER:LAT/LONG TO LINE/SAMP W/NORM CYL PROJECTION',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)
      MODE=2
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE(
     .     'AFTER:LINE/SAMP TO LAT/LONG W/NORM CYL PROJECTION',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)
      CALL XVMESSAGE(
     .     'ANSWER: LINE=213.5,SAMP=127.4,LATI=-10.,LONG=200.',' ')

      call XVMESSAGE('  ',' ')
      CALL XVMESSAGE('*********TEST SIMPLE CYLINDRICAL *********',' ')
      IDATA(39)=10             
      MODE=1
      LAT=-10.0
      LONG=200.
      DO I=1,24
         DATA(I) = 1.0
      ENDDO
      DATA(1)=50.
      DATA(2)=50.
      DATA(3)=-64.6828
      DATA(6)=205.31716    ! changed from 205.3172 so SAMP 1 is long. 360 
      DATA(7) = 100.       ! instead of long.  0.00006. See correction in
      CALL XVMESSAGE(      ! RECTPATCH.
     .     'BEFORE:LAT/LONG TO LINE/SAMP W/LATLON PROJECTION',' ')
      LINE=0.
      SAMP=0.
      CALL DUMP(2)
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE(
     .     'AFTER:LAT/LONG TO LINE/SAMP W/LATLON PROJECTION',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)
      MODE=2
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE(
     .     'AFTER:LINE/SAMP TO LAT/LONG W/LATLON PROJECTION',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)
      CALL XVMESSAGE(
     .     'ANSWER: LINE=32.7,SAMP=51.7,LATI=-10.,LONG=200.',' ')

      call XVMESSAGE('  ',' ')
      CALL XVMESSAGE('***************POLAR ORTHOGRAPHIC*********',' ')
      IDATA(39)=1
      MODE=1
      LAT=30.0
      LONG=200.
      DO I=1,24
         DATA(I) = 1.0
      ENDDO
      DATA(1)=300.
      DATA(2)=300.
      DATA(3)=90.
      DATA(6)=167.
      DATA(7) =7.89
      CALL XVMESSAGE('BEFORE:LAT/LONG TO LINE/SAMP ',' ')
      LINE=400.
      SAMP=400.
      CALL DUMP(2)
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER:LAT/LONG TO LINE/SAMP ',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)
      MODE=2
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER:LINE/SAMP TO LAT/LONG ',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)

      call XVMESSAGE('  ',' ')
      CALL XVMESSAGE('***************POLAR STEREOGRAPHIC*********',' ')
      IDATA(39)=3
      MODE=1
      LAT=-10.0
      LONG=200.
      DO I=1,24
         DATA(I) = 1.0
      ENDDO
      DATA(1)=300.
      DATA(2)=300.
      DATA(3)=-90.
      DATA(6)=167.
      DATA(7) =7.89
      CALL XVMESSAGE('BEFORE:LAT/LONG TO LINE/SAMP ',' ')
      LINE=400.
      SAMP=400.
      CALL DUMP(2)
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER:LAT/LONG TO LINE/SAMP ',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.')
      CALL DUMP(2)
      MODE=2
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER:LINE/SAMP TO LAT/LONG ',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)

      call XVMESSAGE(' ',' ')
      CALL XVMESSAGE('***************OBLIQUE ORTHOGRAPHIC*********',' ')
      IDATA(39)=2
      MODE=1
      LAT=-10.0
      LONG=200.
      DO I=1,24
         DATA(I) = 1.0
      ENDDO
      DATA(1)=300.
      DATA(2)=300.
      DATA(3)=-34.
      DATA(6)=167.
      DATA(7) =7.89
      DATA(9)=40.
      CALL XVMESSAGE('BEFORE:LAT/LONG TO LINE/SAMP ',' ')
      LINE=400.
      SAMP=400.
      CALL DUMP(2)
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER:LAT/LONG TO LINE/SAMP ',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)
      MODE=2
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER:LINE/SAMP TO LAT/LONG ',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)

      call XVMESSAGE('  ',' ')
      CALL XVMESSAGE(
     .     '***************OBLIQUE STEREOGRAPHIC************',' ')
      IDATA(39)=4
      MODE=1
      LAT=-10.0
      LONG=200.
      DO I=1,24
         DATA(I) = 1.0
      ENDDO
      DATA(1)=300.
      DATA(2)=300.
      DATA(3)=34.
      DATA(6)=67.
      DATA(7) =7.89
      DATA(9)=40.
      CALL XVMESSAGE('BEFORE:LAT/LONG TO LINE/SAMP ',' ')
      LINE=400.
      SAMP=400.
      CALL DUMP(2)
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER:LAT/LONG TO LINE/SAMP ',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)
      MODE=2
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER:LINE/SAMP TO LAT/LONG ',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)

      call XVMESSAGE('  ',' ')
      CALL XVMESSAGE('***************LAMBERT*********',' ')
      IDATA(39)=5
      MODE=1
      LAT=-10.0
      LONG=200.
      DO I=1,24
         DATA(I) = 1.0
      ENDDO
      DATA(1)=300.
      DATA(2)=300.
      DATA(3)=34.
      DATA(4)=30.
      DATA(5)=40.
      DATA(6)=67.
      DATA(7) =7.89
      CALL XVMESSAGE('BEFORE:LAT/LONG TO LINE/SAMP ',' ')
      LINE=400.
      SAMP=400.
      CALL DUMP(2)
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER:LAT/LONG TO LINE/SAMP ',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)
      MODE=2
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER:LINE/SAMP TO LAT/LONG ',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)

      call XVMESSAGE('  ',' ')
      CALL XVMESSAGE('*********OBLIQUE SIMPLE CYLINDRICAL*********',' ')
      IDATA(39)=11
      MODE=1
      LAT=-10.0
      LONG=200.
      DO I=1,24
         DATA(I) = 1.0
      ENDDO
      DATA(1)=300.
      DATA(2)=300.
      DATA(3)=34.
      DATA(4)=30.
      DATA(6)=67.
      DATA(7) =7.89
      CALL XVMESSAGE('BEFORE:LAT/LONG TO LINE/SAMP ',' ')
      LINE=400.
      SAMP=400.
      CALL DUMP(2)
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER:LAT/LONG TO LINE/SAMP ',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)
      MODE=2
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER:LINE/SAMP TO LAT/LONG ',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)

      call XVMESSAGE('  ',' ')
      CALL XVMESSAGE('***************SINUSOIDAL*********',' ')
      IDATA(39)=12
      MODE=1
      LAT=-10.0
      LONG=200.
      DO I=1,24
         DATA(I) = 1.0
      ENDDO
      DATA(1)=300.
      DATA(2)=300.
      DATA(3)=34.
      DATA(6)=67.
      DATA(7) =7.89
      CALL XVMESSAGE('BEFORE:LAT/LONG TO LINE/SAMP ',' ')
      LINE=400.
      SAMP=400.
      CALL DUMP(2)
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER:LAT/LONG TO LINE/SAMP ',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)
      MODE=2
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER:LINE/SAMP TO LAT/LONG ',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)


      call XVMESSAGE('  ',' ')
      CALL XVMESSAGE('***********OBLIQUE SINUSOIDAL*********',' ')
      IDATA(39)=13
      MODE=1
      LAT=-10.0
      LONG=200.
      DO I=1,24
         DATA(I) = 1.0
      ENDDO
      DATA(1)=300.
      DATA(2)=300.
      DATA(3)=34.
      DATA(4)=30.
      DATA(6)=67.
      DATA(7) =7.89
      CALL XVMESSAGE('BEFORE:LAT/LONG TO LINE/SAMP ',' ')
      LINE=400.
      SAMP=400.
      CALL DUMP(2)
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER:LAT/LONG TO LINE/SAMP ',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)
      MODE=2
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER:LINE/SAMP TO LAT/LONG ',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)

      call XVMESSAGE('  ',' ')
      CALL XVMESSAGE('*************MOLLWEIDE*********',' ')
      IDATA(39)=14
      MODE=1
      LAT=-10.0
      LONG=200.
      DO I=1,24
         DATA(I) = 1.0
      ENDDO
      DATA(1)=300.
      DATA(2)=300.
      DATA(7) =7.89
      CALL XVMESSAGE('BEFORE:LAT/LONG TO LINE/SAMP ',' ')
      LINE=400.
      SAMP=400.
      CALL DUMP(2)
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER:LAT/LONG TO LINE/SAMP ',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)
      MODE=2
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER:LINE/SAMP TO LAT/LONG ',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)

      call XVMESSAGE('  ',' ')
      CALL XVMESSAGE('************TRANSVERSE MERCATOR*********',' ')
      IDATA(39)=15
      MODE=1
      LAT=-10.0
      LONG=200.
      DO I=1,24
         DATA(I) = 1.0
      ENDDO
      DATA(1)=300.
      DATA(2)=300.
      DATA(3)=34.
      DATA(7) =7.89
      CALL XVMESSAGE('BEFORE:LAT/LONG TO LINE/SAMP ',' ')
      LINE=400.
      SAMP=400.
      CALL DUMP(2)
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER:LAT/LONG TO LINE/SAMP ',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)
      MODE=2
      CALL CONVEV(IND,DATA,DATA,LINE,SAMP,LAT,LONG,MODE,CONV)
      CALL XVMESSAGE('AFTER:LINE/SAMP TO LAT/LONG ',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)

      CALL XVMESSAGE(
     . 'Repeat a test case in C to test C interface: zconvev', ' ')

      MODE=1
      LAT=-10.0
      LONG=200.
      DO I=1,24
         DATA(I) = 1.0
      ENDDO
      DATA(1)=300.
      DATA(2)=300.
      DATA(3)=34.
      DATA(7) =7.89
      LINE=400.
      SAMP=400.
      call tzconvev(IND,LINE,SAMP,LAT,LONG,MODE,DATA,CONV)
      CALL XVMESSAGE('AFTER:LAT/LONG TO LINE/SAMP ',' ')
      if(ind.ne.0) call XVMESSAGE('CONVEV: returns bad indicator.',' ')
      CALL DUMP(2)

      RETURN
      END

      SUBROUTINE DUMP(WHICH)
      COMMON IND,LINE,SAMP,LAT,LONG,MODE,DATA
      DOUBLE PRECISION OM1(3,3),RS1(3)
      REAL DATA(40),LINE,LAT,LONG
      INTEGER IDATA(40),WHICH
      EQUIVALENCE (IDATA,DATA),(DATA,OM1),(DATA(19),RS1)

      CHARACTER*640 BUFFER

      IF (WHICH .EQ. 1) WRITE(BUFFER,110)IND,LINE,SAMP,LAT,LONG,MODE,
     1                  ((OM1(I,J),J=1,3),I=1,3),(RS1(I),I=1,3),
     2                  (DATA(I),I=25,35),IDATA(36),IDATA(37),
     3                  DATA(38),IDATA(39),IDATA(40)
      IF (WHICH .EQ. 2) WRITE(BUFFER,100)IND,LINE,SAMP,LAT,LONG,MODE,
     1                  (DATA(I),I=1,35),(IDATA(I),I=36,37),DATA(38),
     2                  (IDATA(I),I=39,40)
      CALL XVMESSAGE(BUFFER(  2: 80),' ')
      CALL XVMESSAGE(BUFFER( 82:160),' ')
      CALL XVMESSAGE(BUFFER(162:240),' ')
      CALL XVMESSAGE(BUFFER(242:320),' ')
      CALL XVMESSAGE(BUFFER(322:400),' ')
      CALL XVMESSAGE(BUFFER(402:480),' ')
      CALL XVMESSAGE(BUFFER(482:560),' ')
      CALL XVMESSAGE(' ',' ')
      RETURN
  100 FORMAT(' IND: ',I2,'  LINE: ',F7.1,'  SAMP: ',F7.1,'  LAT: ',
     1       F5.1,'  LONG: ',F7.1,'  MODE: ',I1,6X,' DATA: ',2(7
     2       (F8.4,2X),10X),4(F8.6,2X),3(F8.0,2X),10X,2(7(F8.2,
     3       2X),10X),I8,2X,10X,I8,2X,F8.1,2X,2(I8,2X),30X)
  110 FORMAT(' IND: ',I2,'  LINE: ',F7.1,'  SAMP: ',F7.1,'  LAT: ',
     1       F5.1,'  LONG: ',F7.1,'  MODE: ',I1,6X,' OM: ',5(E12.5,3X),
     2       5X,4(E12.5,3X),15X,' RS: ',3(E20.6,5X),' DATA: ',
     3       7(F8.2,2X),10X,4(F8.2,2X),I8,2X,I8,2X,F8.1,12X,
     4       2(I8,2X),30X)

      END
