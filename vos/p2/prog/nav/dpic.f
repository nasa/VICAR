CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Write image area to video display.  Area starts at (SL,SS).  Size of
C area is function of size of display (NLDS X NSDS) and ZOOM factor.
C
      SUBROUTINE DPIC(PIC,SL,SS,NL,NS)
      IMPLICIT INTEGER(A-V)
      BYTE PIC(NS,NL)
      COMMON/DEV/IDEV,VID,G,TB,NLDS,NSDS,ZOOM,IZOOM,STBL(256)
      INTEGER*4 VID,G,TB,STBL
      REAL*4 ZOOM

      COMMON/NAVDV2/XDW,XDB
      INTEGER XDW,XDB

      BYTE BUF(8192)
      INTEGER*2 BUF1(1024,2)
      LOGICAL XST,XDIFILL
      CHARACTER*132 MSG
      REAL*4 EPS/1.001/
C
      WRITE (MSG,9900,IOSTAT=IOSTAT) SL,SS,IZOOM
9900  FORMAT (' (SL,SS)=(',I4,',',I4,')  ZOOM=',I2)
      CALL XVMESSAGE(MSG(2:29),' ')

C     ....Input image area is (SL,SS,NLI,NSI)
      IF (IZOOM.GE.1) THEN
         NLI = NLDS/IZOOM
         NSI = NSDS/IZOOM
      ELSE
         NLI = -NLDS*IZOOM
         NSI = -NSDS*IZOOM
      ENDIF
      NLI = MIN(NLI,NL-SL+1)
      NSI = MIN(NSI,NS-SS+1)

C     ....Output display area is (1,1,NLO,NSO)
      IF (IZOOM.GE.1) THEN
         NLO = (NLI-1)*IZOOM + 1
         NSO = (NSI-1)*IZOOM + 1
      ELSE
         NLO = -NLI/IZOOM
         NSO = -NSI/IZOOM
      ENDIF

      CALL ZIA(BUF,2048)
      CALL ZIA(BUF1,1024)
      XST = XDIFILL(IDEV,G,XDB)
      XST = XDIFILL(IDEV,VID,0)

      IF (IZOOM.EQ.1) THEN
         LL = SL
         DO L=1,NLO
            CALL AVLINE(PIC(SS,LL),L,1,1,NSO)
            LL = LL + 1
         ENDDO
         CALL AVLINE(BUF,1,1,0,0)	!flush buffer
         RETURN
      ENDIF

      IF (IZOOM.LT.1) THEN
         INC = -IZOOM
         LL = SL
         DO L=1,NLO
            CALL MVE(1,NSI,PIC(SS,LL),BUF,INC,1)
            CALL AVLINE(BUF,L,1,1,NSO)
            LL = LL + INC
         ENDDO
         CALL AVLINE(BUF,1,1,0,0)
         RETURN
      ENDIF

      IF (IZOOM.GT.1) THEN
         INC = IZOOM
         LL = SL
         ISW = 1
         CALL MAGS(PIC(SS,LL),BUF1(1,ISW),NSI,INC,NSO)
         DO L=1,NLO-1,INC
            ISW = 3 - ISW
            LL = LL + 1
            CALL MAGS(PIC(SS,LL),BUF1(1,ISW),NSI,INC,NSO)
            CALL MAGLD(BUF1(1,3-ISW),BUF1(1,ISW),BUF,NSO,INC)
            CALL AVLINE(BUF,L,1,INC,NSO)
         ENDDO
         CALL AVLINE(BUF,1,1,0,0)	!flush buffer
         RETURN
      ENDIF

      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  Routine to magnify the image resolution by interpolating between
C  video lines.
C  Inputs: BUF1,BUF2 - two successive video lines
C  Ouput:  OBUF      - INC interpolated lines
C
      SUBROUTINE MAGLD(BUF1,BUF2,OBUF,NSO,INC)
      IMPLICIT INTEGER(A-Z)
      BYTE OBUF(NSO,INC)
      INTEGER*2 BUF1(NSO),BUF2(NSO)
      include 'fortport'

      NAREA = INC*INC
      DO I=1,NSO
         D1 = BUF1(I)
         D2 = BUF2(I)
         R = INC*D1
         DO J=1,INC
	    VAL = R/NAREA
            OBUF(I,J) = INT2BYTE(VAL)
            R = R - D1 + D2
         ENDDO
      ENDDO
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to magnify a video line by a factor of INC
C
      SUBROUTINE MAGS(BUF,OBUF,NSI,INC,NSO)
      IMPLICIT INTEGER(A-Z)
      BYTE BUF(NSI)
      INTEGER*2 OBUF(NSO)
      include 'fortport'

      DN2 = BYTE2INT(BUF(1))
      R = DN2*INC
      II = 1

      DO I=2,NSI
         DN1 = DN2
	 DN2 = BYTE2INT(BUF(I))
         DO J=1,INC
            OBUF(II) = R
            R = R - DN1 + DN2
            II = II + 1
         ENDDO
      ENDDO
      OBUF(II) = R
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c Write from 1 to n lines to a video device.
c All arguments are inputs:
c	buf(insds,inlds)     - input pixel array
c	islds  - starting line of display window
c	issds  - starting sample of display window
c	inlds   - no. of lines to be written
c	insds   - no. of samples per line
c
      subroutine avline(buf,islds,issds,inlds,insds)
      implicit integer(a-w),logical(x)
      byte buf		!Input image area inlds x insds

      common/dev/idev,vid,g,tb,nlds,nsds,zoom,izoom,stbl(256)
      integer*4 vid,g,tb,stbl
      real*4 zoom

      logical xst,xdiawlocation,xdiawset,xdiawwrite

c     ....Up to 100,000 bytes if data will be buffered before writing to
c     ....display device.
      integer lbufsiz
      parameter (lbufsiz=100000)
      byte lbuf(lbufsiz)

      data csl/-999/,css/-999/	!save starting l,s of current display area
      data cnl/-1/,cns/-1/	!save nl,ns of current display area
      data bufptr/1/		!save the current pointer into the local buffer
      data errcnt/0/		!save display error count
      save

c     ....If new display area, or if no more data will fit in buffer
c     ....flush the buffer and start again
      flush = 0
      if (css.ne.issds) flush=1
      if (csl+cnl.ne.islds) flush=1
      if (bufptr+inlds*insds-1.gt.lbufsiz) flush=1
      if (flush.eq.0) goto 50
      if (cnl.gt.0 .and. cns.gt.0) then
         xst = xdiawlocation(idev,vid,le,to,ri,bo)
         xst = xdiawset(idev,vid,css,csl,css+cns-1,csl+cnl-1)
         xst = xdiawwrite(idev,vid,bufptr-1,lbuf)
         if (.not. xst) goto 999
         xst = xdiawset(idev,vid,le,to,ri,bo)
      endif
      csl = islds
      css = issds
      cnl = 0
      cns = 0
      bufptr = 1

c     ....move the data into the local buffer
   50 if (inlds.eq.0 .or. insds.eq.0) return
      call mve(1,inlds*insds,buf,lbuf(bufptr),1,1)
      bufptr = bufptr + inlds*insds
      cnl = cnl + inlds
      cns = insds
      return

  999 if (errcnt.gt.0) return
      call xvmessage('AVLINE: Error in xdiawwrite',' ')
      errcnt = errcnt + 1
      return
      end
