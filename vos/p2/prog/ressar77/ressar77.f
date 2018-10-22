      INCLUDE 'VICMAIN_FOR'
C  RESSAR77  INP=(IN,RES,BLEM)  OUT

      SUBROUTINE MAIN44
      implicit none

      COMMON/C1/A(800,45),LOC(2,1250),VPTBUF(3,90,2)
      COMMON/C1/XBUF(1250),YBUF(1250),RBUF(1250),IORD(1250)

      INTEGER*2 A,VPTBUF,XBUF,YBUF,IORD
      REAL*4 LOC,RBUF,R

      INTEGER*2 WORK(1250)

      common /radius/CIRCLE_RAD2,FIDUCIAL_RAD2
      integer CIRCLE_RAD2,FIDUCIAL_RAD2

      COMMON/NPIX/NPIX
      integer NPIX

      INTEGER*2 HLINE(4),VLINE(4),HORD(4)
      INTEGER*2 VRES(2,4)
      INTEGER*2 HRES(2,4)
      INTEGER*2 VRES2(2,4)
      INTEGER*2 HRES2(2,4)

      integer   icnt, icam, jcam, idef, nlic, nlic2, bi, HI, HII, LLIC
      integer   dbug, reseau, fiducl, nloc, nmlocs, NI, IND
      integer   SL, SS, NL, NS, NLI, NSI, NLOCS, NSA, NO
      integer   J, K, I, INDEX, S, L, LLINE, BLINE, NLINE

      integer XVPTST

      INTEGER*2    HBUF(2,2,4), VBUF(2,2,4)
      INTEGER*2    V(2,2,4,8) 
      INTEGER*2    H(2,2,4,8) 

      INTEGER IPAR(100)
      REAL*4  PAR (100)

      data icnt/0/, icam/0/, jcam/0/, idef/0/, nlic/0/, nlic2/0/
      data bi/0/, HI/0/, HII/0/, LLIC/0/, dbug/0/, reseau/0/
      data fiducl/0/, nloc/0/, nmlocs/0/, NI/0/, IND/0/
      data SL/0/, SS/0/, NL/0/, NS/0/, NLI/0/, NSI/0/, NLOCS/0/
      data NSA/0/, NO/0/, j/0/, k/0/, i/0/
      data INDEX/0/, S/0/, L/0/, LLINE/0/, BLINE/0/, NLINE/0/

      data VRES /13,36,23,46,156,179,166,189/
      data HRES /13,14,22,23,179,180,188,189/
      data VRES2 /24,0,35,0,167,0,178,0/
      data HRES2 /2,0,11,0,191,0,200,0/
      
      data V/-6,11,-18,-5,9,7,18,-5,-19,4,-10,-8,18,8,10,-6,
     *       -9,6,-18,-8,10,6,19,-4,-18,6,-10,-6,18,4,9,-8,
     *       -10,7,-19,-7,9,7,18,-7,-19,5,-9,-5,19,3,9,-7,
     *       -9,8,-17,-5,9,8,17,-7,-18,7,-10,-8,19,6,10,-7,
     *        999,999,999,999,10,9,19,-8,-17,8,-10,-9,18,7,10,-9,
     *       -9,6,-17,-8,10,9,18,-7,-18,8,-10,-6,17,7,10,-8,
     *       -10,7,-18,-7,9,8,19,-5,-19,6,-9,-7,18,6,10,-5,
     *       -9,7,-18,-7,10,7,19,-6,-18,5,-9,-8,19,4,9,-7/

      data H/10,-5,-9,-14,8,-18 ,-8,-10,9,10,-8,18,999,999,
     *       999,999,999,999,999,999,8,-17,-7,-10,6,7,-6,16,7,17,-3,7,
     *       1,-10,-7,-17,8,-17,-9,-10,8,10,-7,18,6,18,-8,9,
     *       7,-10,-7,-18,7,-17,-7,-9,5,7,-4,15,4,18,-6,10,
     *       8,-9,-7,-18,999,999,999,999,999,999,999,999,7,18,-8,10,
     *       8,-10,-7,-18,8,-18,-7,-10,8,10,-8,17,9,18,-3,10,
     *       8,-9,-7,-19,7,-18,-8,-10,9,10,-7,18,5,18,-7,10,
     *       9,-10,-8,-19,7,-18,-7,-10,3,9,-7,18,7,18,-2,10/


        character *6 format(409) /5*'FULL',404*'REAL'/
        integer status
        integer record1,record2

      common/units/ in1,in2,out1,out2,ibis
      integer in1,in2,out1,out2,ibis
c***********************************************************************



!!    RAD2 = 10000000  !! Original EXTRAP default value
      CIRCLE_RAD2   = 20**2 ! For calls to EXTRAP-Radius value for circles 
      FIDUCIAL_RAD2 = 15**2 ! For calls to EXTRAP-Radius value for fiducials

      in1 = 0
      in2 = 0
      out1 = 0
      CALL ZIA (A,800*45/2)
      CAll zia (loc,2*1250)
      CAll zia (rbuf,1250)
      call zia (vptbuf, 3*90*2/2)
      call zia (xbuf, 1250/2)
      call zia (ybuf, 1250/2)
      call zia (iord, 1250/2)
      call zia (hbuf, 2*2*4/2)
      call zia (vbuf, 2*2*4/2)
      call zia (ipar, 100)
      call zia (par,  100)


C  **WARNING... IF NLIC IS CHANGED,VERTICAL FIDUCIAL MARK ROUTINE WILL
C     BE AFFECTED
      NLIC = 45			!Number of image lines in array A
      NLIC2 = NLIC/2

      DBUG   = XVPTST('DBUG')
      RESEAU = 1 - XVPTST('KEEPRESE')
      FIDUCL = 1 - XVPTST('KEEPFIDU')
      NLOC   = 0
      NLOCS  = 0
      NPIX   = 0


      CALL ifmessage ('RESSAR77 version 2016-02-24')


      CALL XVPCNT('INP',NI)    ! get no of inputs
      CALL XVUNIT(IN1,'INP',1,IND,' ')
      CALL XVOPEN(IN1,IND,'OPEN_ACT','SA','IO_ACT','SA',
     &           'U_FORMAT','HALF',' ')
      CALL XVSIZE(SL,SS,NL,NS,NLI,NSI)

      CALL XVPARM('CAMERA', JCAM, ICNT, IDEF, 1)   ! get camera
      IF (ICNT.EQ.1 .AND. IDEF.EQ.0) THEN          ! set default camera
         ICAM = JCAM
      ELSE
         IPAR(1) = 6
         CALL ABLE77V2(IND,IN1,IPAR)
         IF (IPAR(1).NE.3) GOTO 991
         ICAM = IPAR(6)
      ENDIF
      IF (ICAM.LT.4.OR.ICAM.GT.8) GOTO 992   ! bad camera #

      IF (NI.EQ.1) THEN    ! no reseau file was input
         FIDUCL = 0
      ELSE                 ! open single reseau file and get data
         CALL XVUNIT(IN2,'INP',2,IND,' ')    
         call ibis_file_open(in2,ibis,'read',409,99999,
     -                        format,0,status)
         if ( status .ne. 1 ) call ibis_signal_u(in2,status,1)
         call ibis_record_open(ibis,record1,'format:FULL',
     -                               0,0,'FULL',status)
         if ( status .ne. 1 ) call ibis_signal(ibis,status,1)

         call ibis_record_open(ibis,record2,'format:REAL',
     -                               0,0,'REAL',status)
         if ( status .ne. 1 ) call ibis_signal(ibis,status,1)
         call ibis_record_read(record2,loc,l,status) ! read reseau   
         nsa = 404
         NLOC = NSA/2
         NLOCS = NLOC
         call ibis_file_close(ibis,' ',status)
       ENDIF

      CALL XVPCNT('OUT',NO)
      CALL XVUNIT(OUT1,'OUT',1,IND,' ')
      CALL XVOPEN(OUT1,IND,'OPEN_ACT','SA','IO_ACT','SA',
     &           'U_FORMAT','HALF','OP','WRITE',' ')

      CALL XVP('RADIUS',R,ICNT)
      CALL MVE(7,NLOC,R,RBUF,0,1)
      CALL XVPARM('CENTERS',PAR,ICNT,IDEF,100)
      IF (ICNT.GE.3.AND.PAR(3).GT.0.) THEN
         DO J=1,ICNT,3
            NLOC        = NLOC + 1
            LOC(1,NLOC) = PAR(J)
            LOC(2,NLOC) = PAR(J+1)
            RBUF(NLOC)  = PAR(J+2)
         ENDDO
      ENDIF
C
      IF (NI.EQ.3) then
         CALL BLMLOC(R,ICAM,NLOC,DBUG,loc,rbuf)
      endif

C     ....A dummy center is included to space past end of picture
      NLOC = NLOC + 1
      LOC(1,NLOC) = 9999
C

C     ....Convert centers to halfword and get line order
      DO K=1,NLOC
         XBUF(K) = LOC(2,K) + .5
         YBUF(K) = LOC(1,K) + .5
         WORK(K) = YBUF(K)
         IORD(K) = K
      ENDDO
C
C     ....If fiducials are not removed,ignore associated reseau marks
      IF (FIDUCL.NE.1) THEN
         DO I=1,4
            K = HRES2(1,I)
            XBUF(K) = 999
            YBUF(K) = 999
            WORK(K) = 999
            K = VRES2(1,I)
            XBUF(K) = 999
            YBUF(K) = 999
            WORK(K) = 999
         ENDDO
      ENDIF
C

      CALL I2SORT(WORK,IORD,NLOC)	!Sort reseaux by line
C
      IF (FIDUCL .EQ. 0) GO TO 43
C     ....Get starting and ending coordinates of each fiducial mark
      DO J=1,4
         DO I=1,2
            K = HRES(I,J)
            HBUF(1,I,J) = XBUF(K) + H(1,I,J,ICAM)
            HBUF(2,I,J) = YBUF(K) + H(2,I,J,ICAM)
            K = VRES(I,J)
            VBUF(1,I,J) = XBUF(K) + V(1,I,J,ICAM)
	    VBUF(2,I,J) = YBUF(K) + V(2,I,J,ICAM)
	 ENDDO
         HLINE(J) = HBUF(2,1,J) + 6
         IF(HBUF(2,2,J).GT.HBUF(2,1,J)) HLINE(J)=HBUF(2,2,J)+6
         VLINE(J) = VBUF(2,1,J) + NLIC2 + 1
         WORK(J) = HLINE(J)
	 HORD(J) = J
      ENDDO
C
      CALL I2SORT(WORK,HORD,4)		!Sort horizontal fiducials by line
C
      DO I=1,4
         K = HRES2(1,I)
         HRES2(1,I) = XBUF(K)
         HRES2(2,I) = YBUF(K)
         K = VRES2(1,I)
         VRES2(1,I) = XBUF(K)
         VRES2(2,I) = YBUF(K)
      ENDDO
C
   43 IF (DBUG.EQ.1) THEN
         CALL PRNT(2,NLOC,IORD,'IORD.')
         IF (FIDUCL.EQ.1) THEN
            CALL PRNT(2,16,HBUF,'HBUF.')
            CALL PRNT(2,4,HORD, 'HORD.')
            CALL PRNT(2,16,VBUF,'VBUF.')
            CALL PRNT(2,4,HLINE,'HLINE=.')
            CALL PRNT(2,4,VLINE,'VLINE=.')
         ENDIF
      ENDIF

C     ....Store radius for each reseau mark
      HI = 1
      HII = HORD(HI)
      BI = 0
      LLIC = 0

C
      DO 100 INDEX=1,NLOC
      K = IORD(INDEX)
      S = XBUF(K)
      L = YBUF(K)
      R = RBUF(K)
      LLINE = L + NLIC2 + 11
      IF (LLINE.LT.1) GOTO 100
      IF (LLINE.EQ.LLIC) GOTO 88
      BLINE = LLIC + 1
C     ....Position to line L

      DO 80 NLINE=BLINE,LLINE
      BI = MOD(BI,NLIC) + 1
      IF (NLINE.LE.NLIC) GOTO 48
      IF (NLINE .GT. NL+NLIC) GOTO 110
      CALL XVWRIT(OUT1,A(1,BI),IND,' ')
   48 IF (NLINE.LE.NL) then
         CALL XVREAD(IN1,A(1,BI),IND,' ')
      endif
      IF (FIDUCL.EQ.0) GOTO 80
C     ....Routine to delete horizontal fiducial mark
   50 IF (NLINE.NE.HLINE(HII)) GOTO 60
      CALL HFIDUC(NL,NS,NLINE,NLIC,BI,DBUG,HBUF(1,1,HII),HRES2(1,HII),a)
      HI = HI + 1
      HII = HORD(HI)
      GOTO 50
C     ....DELETE VERTICAL FIDUCIAL MARKS
   60 continue
      CALL VFIDUC(NL,NS,NLINE,NLIC,BI,DBUG,VBUF,VLINE,VRES2,vptbuf,a)
   80 CONTINUE
C
   88 LLIC = LLINE
C     ...Remove reseaus,circles & blemishes.
      IF (RESEAU.NE.0 .OR. K.GT.NLOCS) then
     	CALL RESREM(L,S,R,NL,NS,NLIC,BI,a)
      endif
      IF (DBUG.NE.1) GOTO 100
      CALL PRNT(4,1,K,'NUMBER.')
      CALL PRNT(4,1,L,'LINE.')
      CALL PRNT(4,1,S,'SAMPLE.')
      CALL PRNT(7,1,R,'RADIUS.')

  100 CONTINUE
C
  110 CONTINUE

C     ....Write corrected pixel count to label
      CALL XLADD(OUT1,'HISTORY','PIX_CNT',NPIX,IND,
     &		 'FORMAT','INT','ERR_ACT','SA',' ')
      CALL PRNT(4,1,NPIX,'TOTAL # CORRECTED PIXELS =.')
      RETURN

  991 CALL XVMESSAGE ('***Err reading input picture label',' ')
      GOTO 999
  992 CALL XVMESSAGE('***Invalid camera s/n',' ')
  999 CALL XVMESSAGE('***RESSAR77 task cancelled',' ')
      CALL ABEND
      END
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
C Remove a reseau mark at center (L,S) and radius R from the image A
C
      SUBROUTINE RESREM(L,S,R,NL,NS,NLIC,BI,a)
      IMPLICIT INTEGER(A-Z)
      INTEGER*2 A(800,45)
      REAL*4 R
      
      common /radius/CIRCLE_RAD2,FIDUCIAL_RAD2
      integer CIRCLE_RAD2,FIDUCIAL_RAD2
                       
      INTEGER*2 PTBUF(3,150)

      COMMON/NPIX/NPIX
      integer NPIX

      REAL*4 THETA,DTHETA,R2,PI2/6.2831853072/
C
      OFFSET = NLIC/2 + 11

C     ....Compute starting and ending line of circle
      SLINE = L - R
      ELINE = L + R
      SLINE = MAX0(SLINE,2)
      ELINE = MIN0(ELINE,NL)
      IF (ELINE.LT.SLINE) RETURN
      DTHETA = 1./R
      N = 0
      THETA = 0.
C
C     ....Get all samples at radius r from center
   10 X = R*COS(THETA) + S + .5
      IF (X.LT.1.OR.X.GT.NS) GOTO 20
      Y = R*SIN(THETA) + L + .5
      IF (Y.LT.2.OR.Y.GE. NL) GOTO 20
      IF (X.EQ.PTBUF(1,N).AND.Y.EQ.PTBUF(2,N)) GOTO 20
      J = BI - OFFSET + Y - L
      IF (J.LT.1) J=J+NLIC
      N = N + 1
      PTBUF(1,N) = X
      PTBUF(2,N) = Y
      PTBUF(3,N) = A(X,J)
   20 THETA = THETA + DTHETA
      IF (THETA.LT.PI2) GOTO 10
C
      IF (N.EQ.0) RETURN
      R2 = R*R
      J = BI - OFFSET + SLINE - L
      IF (J.LT.1) J=J+NLIC
C
C     ....Replace pixels in circle by interpolating over circumference
      DO LINE=SLINE,ELINE
         DY = LINE - L
         DX = SQRT(R2-DY*DY)
         SSAMP = MAX0(S-DX,1)
         ESAMP = MIN0(S+DX,NS)
	 IF (ESAMP.GE.SSAMP) THEN
            CALL EXTRAP(N,LINE,SSAMP,ESAMP,PTBUF,A(SSAMP,J),CIRCLE_RAD2)
            NPIX = NPIX + ESAMP - SSAMP
	 ENDIF
         J = MOD(J,NLIC) + 1
      ENDDO
C
      RETURN
      END
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
C Remove horizontal fiducial marks
C
      SUBROUTINE HFIDUC(NL,NS,NLINE,NLIC,BI,DBUG,HBUF,HRES2,a)
      IMPLICIT   INTEGER(A-Z)
      INTEGER*2  HBUF(2,2), HRES2(2), A(800,45)

      INTEGER*2 PTBUF(3,150)
      integer   MAXNPT

      common /radius/CIRCLE_RAD2,FIDUCIAL_RAD2
      integer CIRCLE_RAD2,FIDUCIAL_RAD2
                       
      COMMON/NPIX/NPIX
      integer NPIX

      REAL*4 SLOPE,B

      DATA   MAXNPT/150/

C     ....(X1,Y1),(X2,Y2) are the end points of fiducial
      X1 = HBUF(1,1)
      Y1 = HBUF(2,1)
      X2 = HBUF(1,2)
      Y2 = HBUF(2,2)
      DY = IABS(Y2-Y1)
      IF (DY.GT.30) RETURN
      IF (X1.GE.X2) RETURN
      SLOPE = (1.*(Y2-Y1))/(X2-X1)
      B = Y1 - SLOPE*X1

      X1 = MAX0(X1,1)
      X2 = MIN0(X2,NS)
      IF (X1.GT.X2) RETURN
      N = 0
C
C     ....Get points along ends of fiducial
      DO 50 I=1,2
      X = HBUF(1,I)
      Y = HBUF(2,I) - 5
      IF (X.LT.1.OR.X.GT.NS) GOTO 50

      DO 40 JJ=1,11
      IF (Y.LT.2.OR.Y.GT.NL) GOTO 40
      J = BI + Y - NLINE
      IF (J.LT.1) J=J+NLIC
      IF (J.GT.NLIC) J=J-NLIC
      N = N + 1
      PTBUF(1,N) = X
      PTBUF(2,N) = Y
      PTBUF(3,N) = A(X,J)
   40 Y = Y + 1

   50 CONTINUE
C
C     ....(X0,Y0) is center of neighboring reseau mark
      X0 = HRES2(1)
      Y0 = HRES2(2)

C     ....Get points along sides of fiducial
      DO 54 X=X1,X2
      Y = SLOPE*X + B + 6.5

      DO 53 I=1,2
      IF (Y.LT.2.OR.Y.GE. NL) GOTO 53
      J = BI + Y - NLINE
      IF (J.LT.1) J=J+NLIC
      IF (J.GT.NLIC) J=J-NLIC
      DX = X - X0
      DY = Y - Y0
      R2 = DX*DX + DY*DY
      IF (R2.LT.49) GOTO 53
      N = MIN0(N+1,MAXNPT)
      PTBUF(1,N) = X
      PTBUF(2,N) = Y
      PTBUF(3,N) = A(X,J)
   53 Y = Y - 12

   54 CONTINUE
C
      IF(DBUG.EQ.1) CALL PRNT(2,3*N,PTBUF,'PTBUF.')
      SLINE = MIN0(Y1,Y2) - 5
      ELINE = MAX0(Y1,Y2) + 5
      SLINE = MAX0(SLINE,2)
      ELINE = MIN0(ELINE,NL)
      IF (SLINE.GT.ELINE) RETURN
      J = BI + SLINE - NLINE
      IF (J.LT.1) J=J+NLIC
      IF (J.GT.NLIC) J=J-NLIC
      SSAMP = X1
      ESAMP = X2
C
C     ....Now remove the fiducial line-by-line
      DO 100 LINE=SLINE,ELINE
      IF (SLOPE.NE.0.) THEN
         SSAMP = (LINE-5-B)/SLOPE + .5
         ESAMP = (LINE+5-B)/SLOPE + .5
         IF (SSAMP.GT.ESAMP) THEN
            I = SSAMP
            SSAMP = ESAMP
            ESAMP = I
         ENDIF
         SSAMP = MAX0(SSAMP,X1)
         ESAMP = MIN0(ESAMP,X2)
      ENDIF

      IF (SSAMP.LT.1) SSAMP=1
      IF (ESAMP.GT.NS) ESAMP=NS
      IF (ESAMP.GE.SSAMP) THEN
         CALL EXTRAP(N,LINE,SSAMP,ESAMP,PTBUF,A(SSAMP,J),FIDUCIAL_RAD2)
         NPIX = NPIX+ESAMP-SSAMP
      ENDIF
      J = MOD(J,NLIC) + 1
  100 CONTINUE
C
      RETURN
      END
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
C Remove vertical fiducial mark
C
      SUBROUTINE VFIDUC(NL,NS,NLINE,NLIC,BI,DBUG,VBUF,VLINE,VRES2,
     &		vptbuf,a)

      implicit none
! Passed Parameters
      INTEGER NL, NS, NLINE,NLIC,BI,DBUG      
      INTEGER*2 VBUF(2,2,4),VLINE(4),VRES2(2,4),A(800,45),VPTBUF(3,90,2)

! Common parameters
      common /radius/CIRCLE_RAD2,FIDUCIAL_RAD2
      integer CIRCLE_RAD2,FIDUCIAL_RAD2

      COMMON/NPIX/NPIX
      integer NPIX

! Local parameters 
      REAL*4 VSLOPE(2),VB(2)
      integer NLIC2,PI,VII,ELINE,X1,X2,Y1,Y2,N,YY1,YY2,J,INC
      integer Y,XX1,XX2,X,M,N2,I,X0,Y0,DX,DY,R2,JJ,LINE,SSAMP
      integer ESAMP

      DATA    VSLOPE/0.0,0.0/ ,VB/0.0,0.0/
      DATA    NLIC2/0/,PI/0/,VII/0/,ELINE/0/,X1/0/
      DATA    X2/0/,Y1/0/,Y2/0/,N/0/,YY1/0/,YY2/0/,J/0/,INC/0/
      DATA    Y/0/,XX1/0/,XX2/0/,X/0/,M/0/,N2/0/,I/0/,X0/0/
      DATA    Y0/0/,DX/0/,DY/0/,R2/0/,JJ/0/,LINE/0/,SSAMP/0/
      DATA    ESAMP/0/

      NLIC2 = NLIC/2
C
      DO 100 VII=1,4
      PI = MOD(VII,2) + 1
      IF (NLINE.LT.VLINE(VII)) GOTO 100
      ELINE = VBUF(2,2,VII)
      IF (NLINE.EQ.ELINE+NLIC2) VLINE(VII)=999
      IF (NLINE.GT.ELINE+3) GOTO 75
      IF (NLINE.EQ.ELINE+3) GOTO 70
      IF (NLINE.GT.VLINE(VII)) GOTO 66
      X1 = VBUF(1,1,VII)
      Y1 = VBUF(2,1,VII)
      X2 = VBUF(1,2,VII)
      Y2 = VBUF(2,2,VII)
      VSLOPE(PI) = (1.*(X2-X1))/(Y2-Y1)
      VB(PI) = X1 - VSLOPE(PI)*Y1

      CALL MVE(2,2*NLIC,-1,VPTBUF(3,1,PI),0,3)
      N = 2*BI - 1
      YY1 = Y1 - 3
      YY2 = Y1 + NLIC2 + 1
      J = BI + YY1 - NLINE
      IF (J.LT.1) J=J+NLIC
      IF (J.GT.NLIC) J=J-NLIC
      INC = 1
C
C     ....Get points along top and sides of fiducial
      DO 20 Y=YY1,YY2
      XX1 = VSLOPE(PI)*Y + VB(PI) - 4.5
      XX2 = XX1 + 10
      IF (Y.GE.YY1+4) THEN
         XX1 = XX1 - 1
         XX2 = XX2 + 1
         INC = 12
      ENDIF

      DO 15 X=XX1,XX2,INC
      N = MOD(N,2*NLIC) + 1
      IF (Y.LT.2.OR.Y.GE. NL) GOTO 15
      IF (X.LT.1.OR.X.GT.NS) GOTO 15
      VPTBUF(1,N,PI) = X
      VPTBUF(2,N,PI) = Y
      VPTBUF(3,N,PI) = A(X,J)
   15 CONTINUE

   20 J = MOD(J,NLIC) + 1

C
      IF (DBUG.EQ.1) CALL PRNT(2,6*NLIC,VPTBUF(1,1,PI),'VPTBUF.')
      M = N
      N2 = 2*NLIC
C     ....This fidicual is a real problem, ignore it
      DO I=1,N2
         IF (VPTBUF(3,M,PI).NE.-1) GOTO 30
         M = M - 1
         IF (M.LT.1) M=N2
      ENDDO
      VLINE(VII) = 999
      GOTO 100
C
   30 continue
      CALL MVE(2,3,VPTBUF(1,M,PI),VPTBUF(1,N,PI),1,1)
C     ....Fill in missing points
      DO I=1,N2
         M = N
         N = N - 1
         IF (N.LT.1)N=N2
         IF (VPTBUF(3,N,PI).EQ.-1) then
            CALL MVE(2,3,VPTBUF(1,M,PI),VPTBUF(1,N,PI),1,1)
         endif
      ENDDO
      IF (DBUG.EQ.1) CALL PRNT(2,6*NLIC,VPTBUF(1,1,PI),'VPTBUF.')
      GOTO 75
C
C     ....Update vptbuf with point on each side of fiducial
   66 Y = NLINE
      X = VSLOPE(PI)*Y + VB(PI) - 5.5
      N = 2*BI - 1
      M = N - 1
      IF (M.LT.1) M=2*NLIC
      X0 = VRES2(1,VII)		!(X0,y0) is the center
      Y0 = VRES2(2,VII)		!of neighboring reseau
C
      DO 68 I=1,2
      CALL MVE(2,3,VPTBUF(1,M,PI),VPTBUF(1,N,PI),1,1)
      IF (Y.LT.2.OR.Y.GE.NL) GOTO 67
      IF (X.LT.1.OR.X.GT.NS) GOTO 67
      DX = X - X0
      DY = Y - Y0
      R2 = DX*DX + DY*DY
      IF (R2.LT.49) GOTO 67
      VPTBUF(1,N,PI) = X
      VPTBUF(2,N,PI) = Y
      VPTBUF(3,N,PI) = A(X,BI)
   67 M = N
      N = N + 1
   68 X = X + 12
      GOTO 75
C
C     ....Get points along bottom of fiducial mark
   70 Y = NLINE - 3
      N = 2*BI - 2
      IF (N.LT.1) N=2*NLIC
      J = BI - 3
      IF (J.LT.1) J=J+NLIC
C
      DO 72 JJ=1,4
      X = VSLOPE(PI)*Y + VB(PI) - 4.5

      DO 71 I=1,11
      M = N
      N = MOD(N,2*NLIC) + 1
      CALL MVE(2,3,VPTBUF(1,M,PI),VPTBUF(1,N,PI),1,1)
      IF (Y.LT.2.OR.Y.GE.NL) GOTO 71
      IF (X.LT.1.OR.X.GT.NS) GOTO 71
      VPTBUF(1,N,PI) = X
      VPTBUF(2,N,PI) = Y
      VPTBUF(3,N,PI) = A(X,J)
   71 X = X + 1

      J = MOD(J,NLIC) + 1
   72 Y = Y + 1
C
      IF (DBUG.EQ.1) CALL PRNT(2,6*NLIC,VPTBUF(1,1,PI),'VPTBUF.')
C
   75 LINE = NLINE - NLIC2
      J = BI + LINE - NLINE
      IF (J.LT.1)J=J+NLIC
      SSAMP = VSLOPE(PI)*LINE + VB(PI) - 4.5
      ESAMP = SSAMP + 10
      SSAMP = MAX0(SSAMP,1)
      IF (SSAMP.GT.ESAMP) GOTO 100
      IF (SSAMP.LT.1) SSAMP = 1
      IF (ESAMP.GT.NS) ESAMP=NS
      CALL EXTRAP(2*NLIC,LINE,SSAMP,ESAMP,VPTBUF(1,1,PI),
     &            A(SSAMP,J),FIDUCIAL_RAD2)
      NPIX = NPIX+ESAMP-SSAMP

C
  100 CONTINUE
C
      RETURN
      END
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
C Get blemishes from Blemish Location File
C
      SUBROUTINE BLMLOC(R,ICAM,NLOC,DBUG,loc,rbuf)
      REAL*4 LOC(2,1250),RBUF(1250)
      INTEGER*4 DBUG

      character*720 lab
      real*4 blem(4,1000)

      CALL XVUNIT(IN3,'INP',3,IND,' ')
      CALL XVOPEN(IN3,IND,'OPEN_ACT','SA','IO_ACT','SA',' ')
      CALL XLGET(IN3,'HISTORY','CAMNUM',ncam,ind,' ')
      IF (IND.NE.1) THEN
         CALL VIC1LAB(IN3,IND,ILAB,LAB,10)
         read (lab,90001) ncam
90001    format (104x,i1)
      ENDIF
      IF (NCAM.NE.ICAM) GOTO 998
      CALL XVGET(IN3,IND,'NS',NSB,'PIX_SIZE',IPS,' ')
      NSB = NSB*IPS
      NBLM = NSB/16		!Number of blemishes in file
      CALL XVREAD(IN3,BLEM,IND,' ')
      CALL XVCLOSE(IN3,IND,' ')
      CALL MVE(7,NBLM,BLEM(4,1),RBUF(NLOC+1),4,1)
C     ....Calculate line-sample coordinates of each blemish
      DO I=1,NBLM
         M = BLEM(1,I)
         LOC(1,NLOC+I) = BLEM(2,I) + LOC(1,M)
         LOC(2,NLOC+I) = BLEM(3,I) + LOC(2,M)
      ENDDO
C
      NLOC = NLOC + NBLM
      IF (DBUG.EQ.0) RETURN
      CALL PRNT(4,1,NBLM,     'NBLM.')
      CALL PRNT(7,NBLM*4,BLEM,'BLEM.')
      CALL PRNT(4,1,NLOC,     'NLOC.')
      CALL PRNT(7,NLOC*2,LOC, 'LOCS.')
      RETURN

  998 CALL XVMESSAGE ('***Camera S/N DO NOT MATCH',' ')
      CALL PRNT(4,1,NCAM,'***BLEM SN.')
      CALL PRNT(4,1,ICAM,'***RES  SN.')
  999 CALL ABEND
      END
