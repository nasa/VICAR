C
C	REVISION HISTORY
C	7-94  CRI  MSTP S/W CONVERSION (VICAR PORTING)
C
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      IMPLICIT NONE
      INTEGER K,IJ,II,HI,LLIC,J,NLIC2,RECI,OREC,INDEX
      INTEGER S,L,LLINE,CONST,BLINE,STAT,IREC,NPIX
      INTEGER DCODE,ICAM,IN1,OUT1,NLOC,NLOCS
      LOGICAL DBUG,BLEM,RESEAU

      COMMON/CP/DCODE,ICAM,IN1,OUT1,NLOC,NLOCS,R,DBUG,BLEM,RESEAU

      INTEGER NLIC,NLINE,BI,NL,NS,NI,NO
      REAL*4 R,RBUF,LOC(2,413)
      INTEGER*2 A,PTBUF,VPTBUF,XBUF,YBUF,IORD,WORK(2,800)
      INTEGER*2 WORK1(800),WORK2(800),WORK3(800),WORKNDX(800)
      COMMON/C1/NLIC,NLINE,BI,NL,NS,NI,NO
      COMMON/C2/A(1204,1056),PTBUF(3,150),VPTBUF(3,90,2)
      COMMON/C3/XBUF(500),YBUF(500),RBUF(500),IORD(500)

      EQUIVALENCE (LOC,PTBUF),(WORK,A)

      COMMON/NPIX/NPIX,CONST

      CHARACTER*50 MSG
      CHARACTER*132 MSG1

      CALL IFMESSAGE('RESSAR75 version April 6, 1997')
      CALL XVEACTION('SA',' ')
      MSG(1:50)=' '
      MSG(1:7)='NUMBER='
      MSG(14:18)='LINE='
      MSG(25:31)='SAMPLE='
      MSG(38:44)='RADIUS='

      CALL RPARAM(LOC,RBUF,*999)
      IF (BLEM) CALL BLMLOC(LOC,RBUF,WORK,R,ICAM,NLOC,NLOCS,DBUG)
      NLOC=NLOC+1	! Dummy center included to space past end of picture
      LOC(1,NLOC)=9999

C          Convert centers to halfword and get line order
      DO K=1,NLOC
         XBUF(K)=LOC(2,K)+.5
         YBUF(K)=LOC(1,K)+.5
         WORK(1,K)=YBUF(K)
         WORK(2,K)=K
      ENDDO

      CALL SORTX(WORK,WORK1,WORK2,WORK3,WORKNDX,NLOC)
      
      if (nloc.gt.0) CALL MVE(2,NLOC,WORK(2,1),IORD,2,1)

      IF (DBUG) THEN
C         CALL PRNT(4,20,NLAB,'NLAB=.')
C         CALL PRNT(2,NLOC,IORD,'IORD.')
        WRITE(MSG1,401) IORD(1),IORD(3),IORD(5),IORD(7),IORD(9),
     &                  IORD(11),IORD(13),IORD(15)
401     FORMAT('IORD',5X,8(3X,I5))
        CALL XVMESSAGE(MSG1,' ')
        IJ = 17
        DO 420 II=1,(NLOC/8)-1
        WRITE(MSG1,405) IORD(IJ),IORD(IJ+2),IORD(IJ+4),IORD(IJ+6),
     &       IORD(IJ+8),IORD(IJ+10),IORD(IJ+12),IORD(IJ+14)
405     FORMAT(' ',8X,8(3X,I5))
        CALL XVMESSAGE(MSG1,' ')
        IJ = IJ + 16
420     CONTINUE
      ENDIF
      HI=1
      BI=0
      LLIC=0
C-----STORE RADIUS FOR EACH RESEAU MARK:1

      IF (NLOCS .GT. 0) THEN
        DO 450  II = 1,NLOCS
         RBUF(II) = R
450     CONTINUE
      ENDIF


c      IF (DBUG) CALL PRNT(7,NLOC,RBUF,'RADII=.')
      IF (DBUG) then
        write(msg1,550) rbuf(1),rbuf(2),rbuf(3),rbuf(4),rbuf(5),rbuf(6)
550     format('RADII=',3x,6(2x,f9.7))
        call xvmessage(msg1,' ')
        j = 7
        do 560 ii=1,16
          write(msg1,555) rbuf(j),rbuf(j+1),rbuf(j+2),rbuf(j+3),
     &                    rbuf(j+4),rbuf(j+5)
555       format(9x,6(2x,f9.7))
          call xvmessage(msg1,' ')
          j= j + 6
560     continue
        write(msg1,565) rbuf(j),rbuf(j+1)
565     format(9x,2x,f9.7,1x,f10.8)
        call xvmessage(msg1,' ')
      ENDIF
      NLIC2 = NLIC/2
      reci=0
      orec=0

      DO 100 INDEX=1,NLOC
      K=IORD(INDEX)
      S=XBUF(K)
      L=YBUF(K)
      R=NINT(RBUF(K))
      LLINE=L+NLIC2+CONST
      IF(LLINE.LT.1) GOTO 100
      IF(LLINE.EQ.LLIC) GOTO 88
      BLINE=LLIC+1

C          POSITION TO LINE L
      DO 80 NLINE=BLINE,LLINE
         BI=MOD(BI,NLIC)+1
         IF(NLINE.LE.NLIC) GOTO 48
            IF(NLINE.GT.NS+NLIC) GOTO 110
               orec=orec+1
	    CALL XVWRIT( OUT1, A(1,BI), STAT,' ')
   48    IF(NLINE.LE.NL) then
                 irec=irec+1
                CALL XVREAD( IN1, A(1,BI), STAT,' ')
         endif
   80 CONTINUE

   88 CONTINUE
      LLIC=LLINE
      CALL RESREM(A,PTBUF,NL,NS,NLIC,BI,L,S,R)
      IF (DBUG) THEN
         WRITE (MSG(8:12),'(I5)') K
         WRITE (MSG(19:23),'(I5)') L
         WRITE (MSG(32:36),'(I5)') S
         WRITE (MSG(45:49),'(F5.1)') R
         CALL XVMESSAGE(MSG(1:50),' ')
      ENDIF
  100 CONTINUE

  110 CONTINUE

C  WRITE CORRECTED PIXEL COUNT TO LABEL
      CALL XLADD( OUT1, 'HISTORY', 'PIX_CNT', NPIX, STAT, 
     . 'FORMAT', 'INT', 'ERR_ACT', 'SA',' ')
      WRITE(MSG1,800) NPIX
800   FORMAT('TOTAL # CORRECTED PIXELS =',7X,I8)
      CALL XVMESSAGE(MSG1,' ')
      CALL XVMESSAGE('RESSAR75 task completed',' ')
      RETURN

  999 CALL XVMESSAGE('***RESSAR75 task cancelled',' ')
      CALL ABEND
      END

      SUBROUTINE RPARAM(LOC,RBUF,*)
      IMPLICIT NONE
      INTEGER DCODE,ICAM,IN1,OUT1,NLOC,NLOCS
      LOGICAL DBUG,BLEM,RESEAU
      REAL*4 R
      COMMON/CP/DCODE,ICAM,IN1,OUT1,NLOC,NLOCS,R,DBUG,BLEM,RESEAU

      INTEGER NLIC,NLINE,BI,NL,NS,NI,NO
      COMMON/C1/NLIC,NLINE,BI,NL,NS,NI,NO

      INTEGER NPIX,CONST
      COMMON/NPIX/NPIX,CONST

      INTEGER STAT,IND,IVAL,ICOUNT,IDEF,IN2,NSA,N,I

      REAL*4 LOC(2,413),RBUF(500)
      INTEGER APAR(40)
      REAL*4 FPAR(27)
      CHARACTER*8 IFMT
      character*255 filename(2)
      LOGICAL XVPTST

C  **WARNING... IF NLIC IS CHANGED,VERTICAL FIDUCIAL MARK ROUTINE WILL
C  BE AFFECTED
      NLIC = 1056	!Number of lines in core (entire image)
      CONST = 11
      NPIX = 0

c     CALL XVP('INP',LOC,NI)	! Linux does not like this!  (lwk / aug2010)
c     CALL XVP('OUT',LOC,NO)
      CALL XVP('INP',filename,NI)
      CALL XVP('OUT',filename,NO)

      CALL XVUNIT(OUT1,'OUT',1,STAT,' ')
      CALL XVOPEN(OUT1,STAT,'U_FORMAT','HALF','OP','WRITE',' ')

      CALL XVUNIT(IN1,'INP',1,STAT,' ')
      CALL XVOPEN(IN1,STAT,'U_FORMAT','HALF',' ')

      CALL VOLABV2(IND,IN1,APAR)	!Read label to get camera S/N
      ICAM = APAR(2)
      IF(IND.GT.0) THEN
         CALL XVPARM('CAMERA',icam,ICOUNT,IDEF,1)
         IF (IDEF.EQ.1) THEN
            CALL XVMESSAGE('**Err reading camera s/n from label',' ')
            CALL XVMESSAGE('**Use CAMERA parameter',' ')            
            RETURN1
         ENDIF
      ENDIF
      CALL XVPARM('CAMERA',IVAL,ICOUNT,IDEF,1)
      IF (IDEF.EQ.0) ICAM=IVAL

      CALL XVGET(IN1,STAT,'FORMAT',IFMT,'NL',NL,'NS',NS,' ')
      IF (IFMT.EQ.'WORD'.OR.IFMT.EQ.'HALF') THEN
          DCODE = 2		!Halfword input
      ELSE
          DCODE = 1		!Byte input
      ENDIF

      CALL XVUNIT(IN2,'INP',2,STAT,' ')
      CALL XVOPEN(IN2,STAT,' ')
      CALL XVGET(IN2,STAT,'NS',NSA,' ')
      NLOC = NSA/2	     !Number of reseau marks
      NLOCS = NLOC
      CALL XVREAD(IN2,LOC,STAT,' ')
      CALL XVCLOSE(IN2,STAT,' ')

      CALL XVP('RADIUS',R,ICOUNT)

      CALL XVPARM('CENTERS',FPAR,ICOUNT,IDEF,27)
      IF (IDEF.EQ.0) THEN
          N = ICOUNT/3
          IF (ICOUNT.NE.3*N) GOTO 998
          DO I=1,ICOUNT,3
              NLOC = NLOC + 1
              LOC(1,NLOC) = FPAR(I)
              LOC(2,NLOC) = FPAR(I+1)
              RBUF(NLOC) = FPAR(I+2)
          ENDDO
      ENDIF

      RESEAU = .NOT.XVPTST('KEEPRESE')
      BLEM = .NOT.XVPTST('KEEPBLEM')
      DBUG = XVPTST('DBUG')

      IF (.NOT.RESEAU) THEN
          NLOC = 0
          NLOCS = 0
      ENDIF

      RETURN

  998 CALL XVMESSAGE('***Err in CENTERS parameter',' ')
      RETURN1
      END

      SUBROUTINE RESREM(A,PTBUF,NL,NS,NLIC,BI,L,S,R)
C          CIRCLE INTERPOLATION ROUTINE
      IMPLICIT NONE
      INTEGER NPIX,CONST
      INTEGER OFFSET,NLIC,SLINE,L,ELINE,MAX0,NL,MIN0
      INTEGER N,X,S,NS,Y,J,BI,LINE,DY,DX,SSAMP,ESAMP
      COMMON/NPIX/ NPIX,CONST
      INTEGER*2 A(1204,1056),PTBUF(3,150)
      INTEGER*4  MAX1

      REAL*4 R,R2,THETA,DTHETA,PI2

      DATA PI2/6.2831853072/

      MAX1=10000

      OFFSET=NLIC/2+CONST
      R2=R*R
C          COMPUTE STARTING AND ENDING LINE OF CIRCLE
      SLINE=L-R
      ELINE=L+R
      SLINE=MAX0(SLINE,2)
      ELINE=MIN0(ELINE,NL)
      IF(ELINE.LT.SLINE) GOTO 100
      N=0
      THETA=0.
      DTHETA=1./R
C          GET ALL SAMPLES AT RADIUS R FROM CENTER
   90 X=R*COS(THETA)+S+.5
      IF(X.LT.1.OR.X.GT.NS) GOTO 92
      Y=R*SIN(THETA)+L+.5
      IF(Y.LT.2.OR.Y.GT.NL) GOTO 92
      IF(N.NE.0)THEN
         IF(X.EQ.PTBUF(1,N).AND.Y.EQ.PTBUF(2,N)) GOTO 92
      ENDIF
      J=BI-OFFSET+Y-L
      IF(J.LT.1) J=J+NLIC
      IF(J.LT.1)THEN
            PRINT *,' ERROR'
      ENDIF
      N=N+1
      PTBUF(1,N)=X
      PTBUF(2,N)=Y
      PTBUF(3,N)=A(X,J)
   92 THETA=THETA+DTHETA
      IF(THETA.LT.PI2) GOTO 90

      IF(N.EQ.0) GOTO 100
      J=BI-OFFSET+SLINE-L
      IF(J.LT.1) J=J+NLIC
      IF(J.LT.1)THEN
            PRINT *,' ERROR'
      ENDIF

C          REPLACE PIXELS IN CIRCLE BY INTERPOLATING SAMPLES
C          ON CIRCUMFERENCE
      DO 95 LINE=SLINE,ELINE
         DY=LINE-L
         DX=SQRT(R2-DY*DY)
         SSAMP=S-DX
         ESAMP=S+DX
         IF(SSAMP.LT.1) SSAMP=1
         IF(ESAMP.GT.NS) ESAMP=NS
	 IF (ESAMP.GE.SSAMP) THEN
	   CALL EXTRAP(N,LINE,SSAMP,ESAMP,PTBUF,A(SSAMP,J),MAX1)
	   NPIX=NPIX+ESAMP-SSAMP
	 ENDIF
         J=MOD(J,NLIC)+1
   95 CONTINUE

  100 RETURN
      END
C Returns blemish locations (added on to LOC and RBUF)
C Outputs: LOC, RBUF
C Scratch buffer: BLEM
C
      SUBROUTINE BLMLOC(LOC,RBUF,BLEM,R,ICAM,NLOC,NLOCS,DBUG)
      IMPLICIT NONE
      INTEGER ICAM,NBLM,I,NLOC,NLOCS
      REAL*4 LOC(2,400),RBUF(500),BLEM(4,200),R,R1,R2
      LOGICAL DBUG
C      INTEGER APAR(40)

C      CALL XVUNIT( IN3, 'INP', 3, STAT)
C      CALL XVOPEN( IN3, STAT, 'OPEN_ACT', 'SA', 'IO_ACT', 'SA')
C      CALL VOLABV2(IND,IN3,APAR)
C      NCAM = APAR(2)
C	IF(NCAM .NE. ICAM) THEN
C	      CALL XVMESSAGE('S/N DO NOT MATCH',' ')
C	      CALL PRNT(4,1,NCAM,'BLEM SN.')
C	      CALL PRNT(4,1,ICAM,'RES  SN.')
C	      CALL ABEND
C	ENDIF
C      CALL XVGET(IN3,STAT,'NS',NSB,'PIX_SIZE',PS)
C      NSB=NSB*PS
C      NBLM=NSB/16

      CALL VOBLEM(ICAM,NBLM,BLEM)

C        SAR PARAMETERS ARE RETURNED AS FLOATING POINT IN BUFFER BLEM 
C        IN SL,SS,NL,NS ORDER
      NBLM=NBLM/4
      IF (DBUG) CALL PRNT(4,1,NBLM,'NBLM.')

      DO I=1,NBLM
         R1=BLEM(3,I)/2.  ! NL/2
         R2=BLEM(4,I)/2.  ! NS/2
         LOC(1,NLOC+I)=BLEM(1,I)+R1 ! FIND CENTER LINE
         LOC(2,NLOC+I)=BLEM(2,I)+R2 ! FIND CENTER SAMPLE
         RBUF(NLOC+I) = SQRT(R1*R1+R2*R2) ! FIND RADIUS
      ENDDO

C      CALL XVREAD( IN3, BLEM, STAT)
C      IF (DBUG) CALL PRNT(7,NBLM*4,BLEM,'BLEM.')
C      CALL XVCLOSE( IN3, STAT)
C      CALL MVE(7,NLOCS,R,RBUF,1,1)
C      CALL MVE(7,NBLM,BLEM(4,1),RBUF(NLOC+1),4,1)
CC--- CALCULATE BLEMISH LOCATIONS
C      DO I=1,NBLM
C         M=BLEM(1,I)
C         LOC(1,NLOC+I)=BLEM(2,I)+LOC(1,M)
C         LOC(2,NLOC+I)=BLEM(3,I)+LOC(2,M)
C      ENDDO
      NLOC=NLOC+NBLM
      IF (DBUG) THEN
           CALL PRNT(4,1,NLOC,'NLOC.')
           CALL PRNT(7,NLOC*2,LOC,'LOCS.')
      ENDIF
      RETURN
      END


C	SUBROUTINE SORTX(BUF,N)
C-----THIS ROUTINE WILL SWAP THE HALFWORDS OF THE FULLWORD BUFFER
C-----SO THAT VAX WILL SORT LIKE THE IBM.
C	INTEGER*2 BUF(2,N),J
c	DO I=1,N
c           J=BUF(1,I)
c	   BUF(1,I)=BUF(2,I)
c	   BUF(2,I)=J
c        ENDDO
C	CALL I4SORT(BUF,BUF,N)
c	DO I=1,N
c	   J=BUFOUT(1,I)
c	   BUFOUT(1,I)=BUFOUT(2,I)
c	   BUFOUT(2,I)=J
c        ENDDO
c	RETURN
c	END



C********************************************C
C RESSAR75 halfword sort subroutine          C
C********************************************C
	SUBROUTINE SORTX(BUF,BUFVAL1,BUFVAL2,BUFVAL3,BUFNDX,N)
C
C
        INTEGER II,JJ,N
	INTEGER*2 BUF(2,N)
        INTEGER*2 BUFVAL1(N), BUFVAL2(N)
        INTEGER*4 BUFVAL3(N), BUFNDX(N)        

        DO 90055 II=1,N
        BUFVAL3(II) = ((BUF(1,II) * 32768) + BUF(2,II))
        BUFVAL2(II) = BUF(2,II)
        BUFVAL1(II) = BUF(1,II)
        BUFNDX(II)  = II
90055   CONTINUE


	CALL ISORTP(BUFVAL3,1,N,BUFNDX)

        DO  90065 II=1,N
        JJ = BUFNDX(II)
        BUF(1,II) = BUFVAL1(JJ)
        BUF(2,II) = BUFVAL2(JJ)
90065   CONTINUE


	RETURN
	END




      SUBROUTINE VOBLEM(SN,NUM,BUF)
C        VIKING ORBITER NOMINAL BLEMISH RETRIEVAL SUBROUTINE
      IMPLICIT NONE
      INTEGER SN,NUM
      INTEGER BUF(1)
      REAL SN4(96)
      REAL SN6(156)
      REAL SN7(152)
      REAL SN8(256)
      REAL SN8A(128)
      REAL SN8B(128)

      EQUIVALENCE(SN8(1),SN8A)
      EQUIVALENCE(SN8(129),SN8B)


      DATA SN4/
     *    25.5,  38.,  2.,2.,  84.5,1115.5,6.,4., 163.0,1000., 5.,5.,
     *   172.,  725.5, 5.,4., 379.5,  97.5,2.,2., 384.,  488.,5.,3.,
     *   700.5, 422.5, 6.,4., 714.,  351.5,9.,8., 762.5, 430., 4.,3.,
     *   783.5, 726.5, 4.,2., 806.5, 351., 4.,3., 821.5, 258.5,2.,2.,
     *   834.5, 945.,  6.,3., 846.,  645., 5.,3., 857.,  308., 5.,3.,
     *   895.5, 214.5, 2.,2., 921.5, 513., 4.,3., 981.5, 481., 6.,5.,
     *   998.,  425.,  5.,3.,1026.5, 403.5,2.,4.,1030.,  797., 7.,3.,
     *  1043.5,1007.5, 8.,4.,
     *   297.,  185.5, 3.,2.,1055., 1047., 3.,7./  ! ETR ADDITIONS

      DATA SN6/
     *    74.5, 315., 12.,5., 114.5,1033.5,6.,4., 128.,  600., 5.,5.,
     *   159., 1028.,  2.,5., 167.,  775., 2.,3., 181.,   59., 5.,4.,
     *   165.,  542.,  4.,5., 292.,  447., 3.,3., 338.,  995.5,3.,6.,
     *   377.5, 280.5, 2.,2., 385., 1129., 5.,7., 444., 1155., 3.,3.,
     *   453.5, 325.5,24.,6., 502.,  206.,19.,5., 615.,  178., 4.,3.,
     *   690., 1041.5, 5.,6., 704.,  363., 3.,3., 710.5, 363., 2.,3.,
     *   711.5, 653.,  4.,5., 788.,  432., 5.,5., 840.5, 263., 2.,3.,
     *   841.5, 198.,  2.,3., 843.5, 514., 6.,7., 860.,  132.5,4.,4.,
     *   950.,  684.,  7.,7., 975.,  955., 4.,3.,1012.5, 481.5,2.,2.,
     *  1014.5, 914.5, 6.,6.,1017.5, 209., 2.,3.,1025.5, 532., 2.,3.,
     *  1034.5, 384.,  2.,3.,
C               ETR ADDITIONS
     *    23.,  638.5, 3.,6., 435.,   72.5,6.,4., 451.5, 46.,  4.,3.,
     *   460.,  633.,  9.,3., 658.,  245.5,3.,2., 862.,1145.5, 3.,2.,
     *   877.5, 962.5, 2.,2., 894., 1179.5,3.,4./

      DATA SN7/
     *    42.5, 514.5, 4.,6.,  56., 891.5, 5.,8., 180., 220.,  5.,8.,
     *   216.,  979.,  5.,7., 222.5,454.5,30.,18.,253.5,790.5, 4.,4.,
     *   261.5, 165.,  4.,5., 263.5,714.5, 6.,6., 267.,1064.,  5.,9.,
     *   270.5,1101.5, 2.,4., 299.,1049.5, 3.,4., 331.5,537.5, 8.,8.,
     *   373.5, 447.5, 2.,4., 373.5,632.5, 4.,4., 400.,1025.5, 5.,6.,
     *   423.,  910.5, 3.,4., 473., 257.,  5.,5., 541.5,382.,  4.,3.,
     *   556.,  290.5, 5.,6., 627.5,874.,  4.,5., 629., 413.5, 3.,4.,
     *   682.,  755.,  3.,5., 731., 871.,  3.,3., 733.5,779.5, 4.,4.,
     *   807.,  227.,  3.,4., 870.5,978., 10.,11.,906., 894.,  5.,5.,
     *   930.,  887.5, 7.,6., 980.5,201.5, 4.,6.,1000., 592.5, 3.,4.,
C               ETR ADDITIONS
     *    50.5, 294.,  4.,3., 163. ,163.5, 7.,8., 208., 266.5,13.,8.,
     *   274.,  589.,  3.,3., 316.5,165.5, 4.,6., 414., 504.5, 3.,4.,
     *   695.,  646.,  2.,3.,1037.,1094. , 3.,2./

      DATA SN8A/
     *    70.5,1167.,  4.,5., 139., 807.5,5.,6.,  184.,150.,  3.,5.,
     *   192.5, 781.,  2.,5., 249., 478.5,5.,4.,  263.,237.,  3.,5.,
     *   270.,  625.,  3.,5., 310.5,217.5,2.,4.,  320.5,451., 6.,9., 
     *   337.,  510.,  5.,9., 338.5,853., 2.,3.,  339.,910.5, 3.,4.,
     *   342.,  351.,  5.,7., 345., 244.5,7.,8.,  350.5,182., 2.,5.,
     *   354.5, 816.5, 4.,4., 382., 660.5,1.,4.,  387.5,333.5,2.,4.,
     *   390.5, 288.5, 2.,2., 391., 520.,11.,15., 414.5,339., 4.,5.,
     *   455.5, 528.5, 2.,4., 457., 436.5,3.,4.,  478.5,610., 4.,5.,
     *   480.,  622.,  3.,7., 495.5,649., 2.,3.,  507.5,55.5, 2.,4.,
     *   527.5,1159.5, 2.,4., 565.5,271.5,4.,4.,  572.,111.5, 3.,6.,
     *   570.,  208.5, 5.,6., 605.5,542.5,2.,6./

      DATA SN8B/
     *   606.5,332., 4.,7.,
     *   628.,  961.5, 3.,4., 639.5,871.5,4.,8.,  650.5,441.5,2.,4.,
     *   662., 1110.5, 5.,6., 669.5,348., 2.,5.,  679.5,463., 6.,11.,
     *   690.5, 769.,  2.,5., 737.5,585., 2.,5.,  751.5,762.5,2.,6.,
     *   766.5, 671.5, 2.,6., 778.5,549.5,4.,6.,  784.5,885.5,2.,6.,
     *   800.5, 761.5, 2.,4., 824., 204., 3.,7.,  829.,584.,  5.,5.,
     *   861.5, 694.,  2.,5., 867.5,881., 2.,5.,  868.5,651., 2.,5., 
     *   871.,  482.5, 3.,4., 903.5,502.5,2.,6.,  951.,1129., 3.,5.,
     *   955.,  682.5, 3.,6., 988., 855., 3.,7.,  992.5,166.5,5.,8.,
     *  1006.5, 239.,  4.,9.,1023.5,263., 6.,13.,1035.5,760., 2.,7.,
     *  1038., 1135.,  7.,9., 348., 898.5,5.,10.,
     *   321.,  908.,  3.,3., 486., 432.5,2.,4./


      IF(SN.EQ.4)THEN
         NUM=96
         CALL GETBLEM(NUM,SN4,BUF)
      ELSE IF(SN.EQ.6)THEN
         NUM=156
         CALL GETBLEM(NUM,SN6,BUF)
      ELSE IF(SN.EQ.7)THEN
         NUM=152
         CALL GETBLEM(NUM,SN7,BUF)
      ELSE IF(SN.EQ.8)THEN
         NUM=256
         CALL GETBLEM(NUM,SN8,BUF)
      ELSE
         CALL PRNT(4,1,SN,'voblem:ILLEGAL SERIAL NUMBER,SN=.')
         CALL ABEND
      ENDIF
      RETURN
      END

      SUBROUTINE GETBLEM(NUM,INBUF,OUTBUF)
      IMPLICIT NONE
      INTEGER NUM,I
      REAL INBUF(4,NUM/4),OUTBUF(4,NUM/4)
C     INBUF(1,I)IS CENTER LINE OF BLEMISH
C     INBUF(2,I)IS CENTER SAMPLE
C     INBUF(3,I)IS NUMBER OF LINES
C     INBUF(4,I)IS NUMBER OF SAMPLES
      DO I=1,NUM/4
         OUTBUF(1,I)=INBUF(1,I)-INBUF(3,I)/2.
         OUTBUF(2,I)=INBUF(2,I)-INBUF(4,I)/2.
         OUTBUF(3,I)=INBUF(3,I)
         OUTBUF(4,I)=INBUF(4,I)
      ENDDO
      RETURN
      END

