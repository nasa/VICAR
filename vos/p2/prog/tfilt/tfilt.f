      Include   'VICMAIN_FOR'
      Subroutine  Main44
      External    FiltB
      Common/C1/RUNIT,wunit3,wunit1,NLO,NSO,NX,NS,THRESH,SLIN,SSAM,
     * MODE,NLW,NSW,K,KEY,NSUM,DNSUM,nb,wunit2,
     * LOWER,UPPER,NLW1,NLW2,NSW1,NSW2,NSW3,TOTLEN,NPIX,TOTL1,TOTL2
c
      COMMON/C2/SCALE,DCTRAN,OFFSET,BOOST,DCLVL
      INTEGER totlen
c
      call xvmessage(' *** program TFILT version 19-Dec-2012 ***',' ')

      call FILTA
      L1 = Totlen * 4
      L2 = Totlen * 4
      L3 = Totlen * 4
      L4 = Totlen * 4
      L5 = Totlen * 4
      L6 = NPIX * 4
      LTOT = L1 + L2 + L3 + L4 + L5 + L6
      call STACKA(9,FiltB,6,L1,L2,L3,L4,L5,L6,LTOT)
c
999   Continue
c
      Return
      End
c
c********************************************************************
c
      Subroutine FiltB(SUMDN,L1,COUNT,L2,IN,L3,LAST,L4,NEXT,L5,OUT,L6,
     *LTOT)
      COMMON/C1/RUNIT,wunit3,wunit1,NLO,NSO,NX,NS,THRESH,SLIN,SSAM,
     * MODE,NLW,NSW,K,KEY,NSUM,DNSUM,nb,wunit2,
     * LOWER,UPPER,NLW1,NLW2,NSW1,NSW2,NSW3,TOTLEN,NPIX,TOTL1,TOTL2
      COMMON/C2/SCALE,DCTRAN,OFFSET,BOOST,DCLVL
      INTEGER COUNT(L2/4)
      real*4 SUMDN(L1/4), IN(L3/4), LAST(L4/4), NEXT(L5/4), OUT(NPIX)
      INTEGER SLIN,TOTLEN,SSAM,TOTL1,TOTL2,status,
     * wunit1,RUNIT,wunit2,wunit3
      real*4 thresh,dnsum,lower,upper

      if (L1+L2+L3+L4+L5+L6 .NE. LTOT) then
        call xvmessage('*** STACKA Error ***', ' ')
	call Abend
      endif
c
c   Copy RUNIT to wunit1 and wunit2.  Rearranging such that Line I of wunit1
c   Contins the Next (Highest) Line in the Filter Window for Line I of RUNIT,
c   and Line I of wunit2 the Last (Lowest) Line in the Window for Line I-1

c  band loop, over entire subroutine (not reflected in indentation!):
      do ib=1,nb

c  for multi-band case, re-initialize wunit2 and wunit1:
      if (ib.gt.1) then
        call xvclose( wunit2, status, ' ')
        call xvopen( wunit2, status, 'OP','WRITE','U_FORMAT','REAL',
     *   'O_FORMAT','REAL','U_NL',NLO,'U_NS',NSO,'U_NB',1,'OPEN_ACT',
     *   'SA','IO_ACT','SA', ' ')
        call xvclose( wunit1, status, ' ')
        call xvopen( wunit1, status, 'OP','WRITE','U_FORMAT','REAL',
     *   'U_NL',NLO,'U_NS',NSO,'U_NB',1,'OPEN_ACT','SA','IO_ACT','SA',
     *   ' ')
      endif

      do LINE=2,NLW2
          IREC2 = NLW2+SLIN+1-LINE
          call xvread( RUNIT, IN, status, 'LINE',IREC2, 'SAMP',SSAM,
     &	   'NSAMPS',NSO, 'band', ib, ' ')
          call xvwrit( wunit2, IN, status, ' ')
      enddo

      NEND = NLO-NLW1
      call xvread( RUNIT, IN , status, 'LINE',SLIN, 'SAMP',SSAM,
     &	'NSAMPS',NSO, 'band', ib, ' ')
      LINE=1
      if (LINE.LE.NEND) call xvwrit( wunit2, IN, status,' ')  ! USU. DONE
      if (LINE.GT.NLW2) call xvwrit( wunit1,  IN, status,' ')  ! USU. SKIPPED
c
      do LINE=2,NLO
	 call xvread( RUNIT, IN, status, 'SAMP',SSAM, 'NSAMPS',NSO,
     &    'line', slin+line-1, 'band', ib, ' ')
	 if (LINE.LE.NEND) call xvwrit( wunit2, IN, status, ' ')
	 if (LINE.GT.NLW2) call xvwrit( wunit1, IN, status, ' ')
      enddo

      do I=1,NLW2-1
          LINE=NLO-1+SLIN-I
          call xvread( RUNIT, IN, status, 'LINE',LINE, 'SAMP',SSAM,
     &	  'NSAMPS',NSO, 'band', ib, ' ')
          call xvwrit( wunit1, IN, status, ' ')
      enddo
c
C  Close  Scratch DISK & Re-Open for INPUT:
c
      call xvclose( wunit2, status, ' ')
      call xvopen( wunit2, status, 'OP','READ','U_FORMAT','REAL',
     *   'O_FORMAT','REAL','U_NL',NLO,'U_NS',NSO,'U_NB',1,'OPEN_ACT',
     *   'SA','IO_ACT','SA', ' ')
c
C  Close  OUTPUT File  & Re-Open  for UPDATE
c
      call xvclose( wunit1, status, ' ')
      call xvopen( wunit1, status, 'OP','UPDATE', 'U_FORMAT','REAL',
     * 'U_NL',NLO,'U_NS',NSO,'U_NB',1,'OPEN_ACT','SA','IO_ACT','SA',
     * ' ')
c
c   Compute the Initial SUMDN and Count buffers from the 
c   first NLW  lines 
c   SUMDN = SUM of DN'S  GE  THRESH in each column 
c   COUNT = SUM of PIXELS GE THRESH in each column 

c     call ZIA(SUMDN, L1/4)
c     call ZIA(COUNT, L2/4)
      do i=1,l1/4
	sumdn(i) = 0.0
      enddo
      do i=1,l2/4
	count(i) = 0.0
      enddo
      do LINE=1,NLW
	  call xvread( wunit2, IN(NSW2), status, 'LINE',LINE,
     &	 'NSAMPS',NSO, ' ')
	call RFLCT(IN)
	do J=1,TOTLEN
	  if (IN(J) .GE. THRESH) then
            SUMDN(J) = SUMDN(J) + IN(J)
            COUNT(J) = COUNT(J) + 1
	  endif
	enddo
      enddo
c
c    Initialize the xvreads:
c
      call xvread( wunit1,NEXT(NSW2),status,'LINE',1,'NSAMPS',NSO,' ')
      call RFLCT(NEXT)
      call xvread( wunit2,LAST(NSW2),status,'LINE',1,'NSAMPS',NSO,' ')
      call RFLCT(LAST)
      call xvread( RUNIT,IN(NSW2),status,'LINE',SLIN,'NSAMPS',NSO,
     &      'SAMP',SSAM, 'band', ib, ' ')
      call RFLCT(IN)
c
C   MAIN  LINE Loop for  OUTPUT Picture
c
      do LINE = 1,NLO
          DNSUM = 0
          NSUM  = 0
c
c    Add up the First NSW DN:
c 
          DO J = 1,NSW
            DNSUM = DNSUM + SUMDN(J)
            NSUM  = NSUM  + COUNT(J)
 	  enddo
c
c    Apply the appropriate Filter to Current LINE 
c
         if (MODE .EQ. 1) call HPFILT(IN,OUT,SUMDN,COUNT)
         if (MODE .EQ. 2) call SDFILT(IN,OUT,SUMDN,COUNT)
         if (MODE .EQ. 3) call LPFILT(IN,OUT,SUMDN,COUNT)
         if (MODE .EQ. 4) call DFILT (IN,OUT,SUMDN,COUNT)
	 call xvwrit( wunit1, OUT, status, ' ')
         if (LINE .EQ. NLO) Go To 2000
c
c   Update SUMDN & COUNT buffers by deleting LAST line & Adding NEXT:
c
	 DO J = 1,TOTLEN
	   if (LAST(J) .GE. THRESH) then
	     SUMDN(J) = SUMDN(J) - LAST(J)
	     COUNT(J) = COUNT(J) - 1
	   endif
	   if (NEXT(J) .GE. THRESH) then
	     SUMDN(J) = SUMDN(J) + NEXT(J)
	     COUNT(J) = COUNT(J) + 1
	   endif
	 enddo

	 if (SSAM .EQ. 1) then	   ! READ NEXT LINE
	   call xvread( RUNIT, IN(NSW2), status,'NSAMPS',NSO,
     &      'line', slin+line, 'band', ib,  ' ')
	 else
	   call xvread( RUNIT, IN(NSW2), status, 'SAMP',SSAM,
     &	    'NSAMPS',NSO, 'line', slin+line, 'band', ib, ' ')
	 endif
c
         call RFLCT(IN)
c
c    LAST   Line in Window  
c
         call xvread( wunit2,LAST(NSW2),status, 'NSAMPS',NSO, ' ')
         call RFLCT(LAST)
c
c    NEXT  Line in Window  
c
         call xvread( wunit1,NEXT(NSW2),status,'NSAMPS',NSO, ' ')
         call RFLCT(NEXT)
c
      enddo
c
C   Close Files and Exit
c
2000  Continue

c  multi-band case:  copy wunit1 to band IB of wunit3
      if (nb.gt.1) then
        do il=1,nlo
          call xvread( wunit1, in, status, 'line', il, 'nsamps',nso,
     &     ' ')
          call xvwrit( wunit3, in, status, 'line', il, 'nsamps',nso,
     &     'band', ib, ' ')
        enddo
      endif

      enddo  ! END OF BAND LOOP

      call xvclose( RUNIT, status, ' ')
      call xvclose( wunit2, status, ' ')
      call xvclose( wunit1, status, ' ')
c
      Return 
      End
c
c*************************************************************
c
      Subroutine RFLCT(BUF)
      COMMON/C1/RUNIT,wunit3,wunit1,NLO,NSO,NX,NS,THRESH,SLIN,SSAM,
     * MODE,NLW,NSW,K,KEY,NSUM,DNSUM,nb,wunit2,
     * LOWER,UPPER,NLW1,NLW2,NSW1,NSW2,NSW3,TOTLEN,NPIX,TOTL1,TOTL2
      COMMON/C2/SCALE,DCTRAN,OFFSET,BOOST,DCLVL
      real*4 BUF(1)
      INTEGER SLIN,TOTLEN,SSAM,TOTL1,TOTL2
      real*4 thresh,dnsum,lower,upper

c     call MVE(2,NSW1,BUF(NSW3),BUF(NSW1),1,-1)

      I = 1
      J = 1
      DO K=1,NSW1
        BUF(NSW1+J-1) = BUF(NSW3+I-1)
        I = I + 1
        J = J - 1
      ENDDO

c     call MVE(2,NSW1,BUF(TOTL2),BUF(TOTL1),-1,1)

      I = 1
      J = 1
      DO K=1,NSW1
        BUF(TOTL1+J-1) = BUF(TOTL2+I-1)
        I = I - 1
        J = J + 1
      ENDDO

      Return
      End
c
c******************************************************************
c
      Subroutine FILTA
      COMMON/C1/RUNIT,wunit3,wunit1,NLO,NSO,NX,NS,THRESH,SLIN,SSAM,
     * MODE,NLW,NSW,K,KEY,NSUM,DNSUM,nb,wunit2,
     * LOWER,UPPER,NLW1,NLW2,NSW1,NSW2,NSW3,TOTLEN,NPIX,TOTL1,TOTL2
      COMMON/C2/SCALE,DCTRAN,OFFSET,BOOST,DCLVL
      INTEGER SLIN,TOTLEN,SSAM,TOTL1,TOTL2,status,limits(2),
     * wunit1,RUNIT,wunit2,wunit3
      real*4 thresh,dnsum,lower,upper
      LOGICAL xvptst,HIGH,LOW,SCENE,DIVIDE

      call xvunit( RUNIT,'INP',1,status, ' ')
      call xvopen( RUNIT, status,'U_FORMAT','REAL','OPEN_ACT','SA',
     * 'IO_ACT','SA', ' ')
      call xvsize(SLIN, SSAM,NLO,NSO,NX,NS, ' ')
      call xvbands(sb,nb,idum)
      call xvget( RUNIT,status,'PIX_SIZE',N, ' ')
      if (n.gt.4) call mabend(' Input pixel size may not be > 4 bytes!')

      call xvpcnt('OUT',i,j)
      if (nb.gt.1 .and.	i.lt.3) call mabend(
     *	 ' 3 output files required when input is multi-band')

      if (nb.eq.1) then
	call xvunit( WUNIT1,'OUT',1,status,' ')
	call xvopen( WUNIT1, status, 'OP','WRITE','U_FORMAT','REAL',
     *   'U_NL',NLO,'U_NS',NSO,'U_NB',1,'OPEN_ACT','SA','IO_ACT','SA',
     *   ' ')
      else
	call xvunit( WUNIT3, 'OUT', 1, status, ' ')
	call xvopen( WUNIT3, status, 'OP','WRITE','U_FORMAT','REAL',
     *   'U_NL',NLO,'U_NS',NSO,'U_NB',NB,'OPEN_ACT','SA','IO_ACT','SA',
     *   ' ')
        call xvunit( WUNIT1, 'OUT', 3, status, ' ')
        call xvopen( WUNIT1, status, 'OP','WRITE','U_FORMAT','REAL',
     *   'U_NL',NLO,'U_NS',NSO,'U_NB',1,'OPEN_ACT','SA','IO_ACT','SA',
     *   ' ')
      endif

      call xvunit( WUNIT2, 'OUT', 2, status, ' ')
      call xvopen( WUNIT2, status,'OP','WRITE','U_FORMAT','REAL',
     * 'O_FORMAT','REAL','U_NL',NLO,'U_NS',NSO,'U_NB',1,'OPEN_ACT',
     * 'SA','IO_ACT','SA', ' ')

      KEY = 4-N
      call Prnt(4,1,N,' #BYTES/PXL=.')
      MODE = 4
      NLW  = NLO/10
      NSW  = NLW
      call xvparm('NLW', NLW, NNLW, NLWDF, 0)
      call xvparm('NSW', NSW, NNSW, NSWDF, 0)
      call xvparm('THRESH', THRESH, NTHRE, THREDF, 0)
      call xvparm('SCALE' ,SCALE, NSCAL, SCALDF, 0)
      call xvparm('DCTRAN', DCTRAN, NDCTR, DCTRDF, 0)
      call xvparm('BOOST', BOOST, NBOOS, BOOSDF, 0)
      call xvparm('DCLEVEL', DCLVL, NDCLE, DCLEDF, 0)
      call xvparm('OFFSET', OFFSET, NOFFS, OFSDF, 0)
      HIGH  =  xvptst('HIGH')
      LOW   =  xvptst('LOW')
      SCENE =  xvptst('SCENE')
      DIVIDE=  xvptst('DIVIDE')

      if (HIGH)   MODE = 1
      if (LOW)    MODE = 3
      if (SCENE)  MODE = 2
      if (DIVIDE) MODE = 4

      if ((NLW/2)*2 .EQ. NLW) NLW = NLW + 1
      if ((NSW/2)*2 .EQ. NSW) NSW = NSW + 1

      NPIX = NSO
      if (KEY .EQ. 0) then
        call xvparm( 'LIMITS', limits, i, j, 0)
	if (i.eq.0) then
	  lower = -1.0e10
	  upper = 1.0e10
	else
	  lower = limits(1)
	  upper = limits(2)
	endif
      elseif (key.eq.2) then
	LOWER = -32768
	UPPER = 32767
      else
	LOWER = 0
	UPPER = 255
      endif

      NLW2 = (NLW+1) / 2
      NLW1 = NLW2 - 1
      NSW2 = (NSW+1) / 2
      NSW1 = NSW2 - 1
      TOTLEN = NPIX +  2*NSW1
      NSW3 = NSW2 + 1
      TOTL1 = TOTLEN - NSW1 + 1
      TOTL2 = TOTLEN - NSW2
      call PRNT(4,1,NLW,' NLW=.')
      call PRNT(4,1,NSW,' NSW=.')
c
c     Get  Number  Pixels/Line
c   
      Return
      End
c
c******************************************************************
c
      Subroutine HPFILT(IN,OUT,SUMDN,COUNT)
      COMMON/C1/RUNIT,wunit3,wunit1,NLO,NSO,NX,NS,THRESH,SLIN,SSAM,
     * MODE,NLW,NSW,K,KEY,NSUM,DNSUM,nb,wunit2,
     * LOWER,UPPER,NLW1,NLW2,NSW1,NSW2,NSW3,TOTLEN,NPIX,TOTL1,TOTL2
      COMMON/C2/SCALE,DCTRAN,OFFSET,BOOST,DCLVL
c
c
      real*4 IN(1),OUT(1),sumdn(1)
      INTEGER COUNT(1),UPLIM
      real*4 thresh,dnsum,lower,upper
      INTEGER SLIN,TOTLEN,SSAM,TOTL1,TOTL2
c
      if (IN(NSW2) .LT. THRESH) Go To 90
      if (NSUM .NE. 0) X = DNSUM/float(NSUM)
      if (NSUM .EQ. 0) X = DNSUM
      X = (IN(NSW2) - X) * BOOST + DCTRAN*IN(NSW2) + DCLVL
      if (X .GT. UPPER) then
	OUT(1) = UPPER
      elseif (X .LE. LOWER) then
	OUT(1) = LOWER
      else
	OUT(1) = X
      endif
      Go  To 100
90    OUT(1)=0
100   Continue
c
      JJ = 1
      LOWLIM = NSW2 + 1
      UPLIM  = NPIX + NSW1
c
      Do 200  J = LOWLIM,UPLIM
         JJ  = JJ + 1
         JK1 = J - NSW2
         JK2 = J + NSW1
         DNSUM = DNSUM - SUMDN(JK1) + SUMDN(JK2)
         NSUM  = NSUM - COUNT(JK1) + COUNT(JK2)
         if (IN(J) .LT. THRESH) Go To 160
         if (NSUM .NE. 0) X = DNSUM/Float(NSUM)
         if (NSUM .EQ. 0) X = DNSUM
         X = (IN(J) - X) * BOOST + DCTRAN * IN(J) + DCLVL
         if (X .GT. UPPER)  then
	   OUT(JJ)=UPPER
         elseif (X .LE. LOWER) then
	   OUT(JJ)=LOWER
         else
	   OUT(JJ) = X
         endif
c
         Go To  200
160      OUT(JJ)=0
200   Continue
c
      Return
      End
c
c******************************************************************
c
      Subroutine DFILT(IN,OUT,SUMDN,COUNT)
      COMMON/C1/RUNIT,wunit3,wunit1,NLO,NSO,NX,NS,THRESH,SLIN,SSAM,
     * MODE,NLW,NSW,K,KEY,NSUM,DNSUM,nb,wunit2,
     * LOWER,UPPER,NLW1,NLW2,NSW1,NSW2,NSW3,TOTLEN,NPIX,TOTL1,TOTL2
      COMMON/C2/SCALE,DCTRAN,OFFSET,BOOST,DCLVL
      real*4 IN(1),OUT(1),sumdn(1)
      real*4 thresh,dnsum,lower,upper
      INTEGER COUNT(1),UPLIM
      INTEGER SLIN,TOTLEN,SSAM,TOTL1,TOTL2

      IF(IN(NSW2).LT.THRESH)GO TO 90
      IF(DNSUM.NE.0)X=FLOAT(NSUM)/DNSUM
      IF(NSUM.EQ.0)X=FLOAT(NSUM)
      X = IN(NSW2)*(SCALE*X+DCTRAN)+OFFSET
      if (X.GT.UPPER) then
	OUT(1)=UPPER
      elseif (X.LE.LOWER) then
	OUT(1)=LOWER
      else
	OUT(1) = X
      endif
      GO TO 100
 90   OUT(1)=0
 100  CONTINUE
      JJ=1
      LOWLIM=NSW2+1
      UPLIM =NPIX+NSW1
      DO 200 J=LOWLIM,UPLIM
      JJ=JJ+1
      JK1=J-NSW2
      JK2=J+NSW1
      DNSUM=DNSUM-SUMDN(JK1)+SUMDN(JK2)
      NSUM=NSUM-COUNT(JK1)+COUNT(JK2)
      IF(IN(J).LT.THRESH)GO TO 160
      IF(DNSUM.NE.0)X=FLOAT(NSUM)/DNSUM
      IF(DNSUM.EQ.0)X=FLOAT(NSUM)
      X=IN(J)*(SCALE*X+DCTRAN)+OFFSET
      if (X.GT.UPPER) then
	OUT(JJ)=UPPER
      elseif (X.LE.LOWER) then
	OUT(JJ)=LOWER
      else
	OUT(JJ) = X
      endif
      GO TO 150
 160  OUT(JJ)=0
 150  CONTINUE
 200  CONTINUE
      RETURN
      END
c
c******************************************************************
c
      Subroutine SDFILT(IN,OUT,SUMDN,COUNT)
      COMMON/C1/RUNIT,wunit3,wunit1,NLO,NSO,NX,NS,THRESH,SLIN,SSAM,
     * MODE,NLW,NSW,K,KEY,NSUM,DNSUM,nb,wunit2,
     * LOWER,UPPER,NLW1,NLW2,NSW1,NSW2,NSW3,TOTLEN,NPIX,TOTL1,TOTL2
      COMMON/C2/SCALE,DCTRAN,OFFSET,BOOST,DCLVL
      real*4 IN(1),OUT(1),sumdn(1)
      real*4 thresh,dnsum,lower,upper
      INTEGER COUNT(1),UPLIM
      INTEGER SLIN,TOTLEN,SSAM,TOTL1,TOTL2

c     call MVE(2,NPIX,IFIX(DCLVL),OUT,0,1)
      J = 1
      DO K=1,NPIX
        OUT(J) = dclvl
        J = J + 1
      ENDDO
      if (IN(NSW2) .LT. THRESH) Go To  90
      if (NSUM .NE. 0) X = DNSUM/Float(NSUM)
      if (NSUM .EQ. 0) X = DNSUM
      if (X .NE. 0) X = (IN(NSW2)-X)*SCALE/X + DCTRAN*IN(NSW2) + DCLVL
      if (X .GT. UPPER) then
	OUT(1) = UPPER
      elseif (X .LE. LOWER) then
	OUT(1)=LOWER
      else
	OUT(1) = X
      endif
      go to 100
90    OUT(1) = 0
100   Continue
c
      JJ = 1
      LOWLIM = NSW2 + 1
      UPLIM  = NPIX + NSW1
      Do 200  J = LOWLIM,UPLIM
      JJ = JJ + 1
      JK1 = J - NSW2
      JK2 = J + NSW1
      DNSUM = DNSUM - SUMDN(JK1) + SUMDN(JK2)
      NSUM = NSUM - COUNT(JK1) + COUNT(JK2)
      if (IN(J) .LT. THRESH) Go To  160
      if (NSUM .NE. 0) X = DNSUM/Float(NSUM)
      if (NSUM .EQ. 0) X = DNSUM
      if (X .NE. 0) X = (IN(J)-X)*SCALE / X + DCTRAN*IN(J) + DCLVL
      if (X .GT. UPPER) then
	OUT(JJ)=UPPER
      elseif (X .LE. LOWER) then
	OUT(JJ)=LOWER
      else
	OUT(JJ) = X
      endif
      go to 150
160   OUT(JJ) = 0
150   Continue
200   Continue
c
      RETURN
      END
c
c******************************************************************
c
      Subroutine  LPFILT(IN,OUT,SUMDN,COUNT)
      COMMON/C1/RUNIT,wunit3,wunit1,NLO,NSO,NX,NS,THRESH,SLIN,SSAM,
     * MODE,NLW,NSW,K,KEY,NSUM,DNSUM,nb,wunit2,
     * LOWER,UPPER,NLW1,NLW2,NSW1,NSW2,NSW3,TOTLEN,NPIX,TOTL1,TOTL2
      COMMON/C2/SCALE,DCTRAN,OFFSET,BOOST,DCLVL
c
      real*4 IN(1),OUT(1),sumdn(1)
      real*4 thresh,dnsum,lower,upper
      INTEGER COUNT(1),UPLIM
      INTEGER SLIN,TOTLEN,SSAM,TOTL1,TOTL2
c
c
      if (IN(NSW2) .LT. THRESH) Go To 90
      if (NSUM .NE. 0) X = DNSUM/Float(NSUM)
      if (NSUM .EQ. 0) X = DNSUM
      X = BOOST*X + OFFSET
      if (X .GT. UPPER) then
	OUT(1) = UPPER
      elseif  (X .LE. LOWER) then
	OUT(1) = LOWER
      else
	OUT(1) = X
      endif
      Go To  100
90    OUT(1) = 0
100   Continue
c
c
      JJ=1
      LOWLIM=NSW2+1
      UPLIM =NPIX+NSW1
      Do 200  J = LOWLIM,UPLIM
      JJ = JJ + 1
      JK1 = J - NSW2
      JK2 = J + NSW1
      DNSUM = DNSUM - SUMDN(JK1) + SUMDN(JK2)
      NSUM = NSUM - COUNT(JK1) + COUNT(JK2)
      if (IN(J) .LT. THRESH) Go To 160
      if (NSUM .NE. 0) X = DNSUM/Float(NSUM)
      if (NSUM .EQ. 0) X = DNSUM
      X = BOOST*X + OFFSET
      if (X .GT. UPPER) then
	OUT(JJ) = UPPER
      elseif (X .LE. LOWER) then
	OUT(JJ) = LOWER
      else
	OUT(JJ) = X
      endif
      Go to 150 
160   OUT(JJ) = 0
150   Continue
200   Continue
c
      Return
      End
