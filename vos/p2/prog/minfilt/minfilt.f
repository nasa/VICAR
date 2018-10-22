C
C     PROGRAM minfilt
c        jan 93       jjl      
C
      INCLUDE 'VICMAIN_FOR'

      SUBROUTINE MAIN44
	implicit none
      integer*4 nsmax
      parameter (nsmax=10000)
c      implicit integer(a-z)
      integer*4 sl,ss,nl,ns,nlin,nsin,nlw,nsw,icode
      integer*4 curr,i,j,l,l1,l2,l3,l4,l5,m,iline,line
c      integer*2 outbuf(nsmax), inbuf(51*nsmax)
	real*4	outbuf(nsmax), inbuf(51*nsmax)
      integer*4 outunit,inunit,cnt,nlwm1,imax,ipos,inc
      integer*4 stat
	character*3 orgin 
	character*8 fmt(4)/'BYTE','HALF','FULL','REAL'/
      	character*8 format
	
	call xvmessage ('** MINFILT - 18-May-2011',' ')
C
C  OPEN INPUT & OUTPUT FOR SEQUENTIAL I/O
C
      call xvunit(inunit,'INP',1,stat,' ')
	call xvopen(inunit,stat,'OPEN_ACT','SA','IO_ACT','SA',' ')
      call xvget(inunit,stat,'FORMAT',FORMAT,'ORG',orgin,' ')

	icode = 0
	if (format.eq.'BYTE') icode=1
	if (format.eq.'HALF'.or.format.eq.'WORD') icode=2
	if (format.eq.'FULL') icode=3
	if (format.eq.'REAL') icode=4
	if (icode.eq.0) then
		call xvmessage('??E - Unknown data format for input image',' ')
		call abend  
	endif
        if (orgin.eq.'BIP') call mabend(
     +  '??E - BIP files not supported, use TRAN to convert to BSQ')

	call xvclose(inunit,stat,' ')

      call xvopen(inunit,stat,'OPEN_ACT','SA','IO_ACT','SA',
     +            'I_FORMAT',fmt(icode),'U_FORMAT',fmt(4),' ')

      call xvsize(sl,ss,nl,ns,nlin,nsin)
c	the following calls were never implemented
c	call xvp('ONL',onl,cnt)
c	call xvp('ONS',ons,cnt)
      call xvunit(outunit,'OUT',1,STAT,' ')
      call xvopen(outunit,stat,'OP','WRITE','OPEN_ACT','SA',
     +          'IO_ACT','SA','O_FORMAT',fmt(icode),'U_FORMAT',fmt(4),' ')
      SL = SL - 1

C
C  GET NLW AND NSW VALUES
      call xvp('NLW',nlw,cnt)
      call xvp('NSW',nsw,cnt)
      NLW = NLW / 2 * 2 + 1
      NSW = NSW / 2 * 2 + 1

      if(nsmax.lt.ns+nsw-1)then
         call xvmessage('??E - input line too long',' ')
         call abend
      endif
      if(51*nsmax.lt.(ns+nsw-1)*nlw)then
         call xvmessage('??E - insufficient memory available',' ')
         call abend
      endif
C
C  DETERMINE STARTING LINE AND INCREMENT VALUES
      LINE = 1 - NLW / 2
      L = NSW / 2
      L1 = (NSW + 1) / 2
      L2 = L1 + 1
      L3 = L1 - 1
      L4 = L1 + NS - 2
      L5 = L1 + NS
      M = NL + NL
      CURR = (NLW + 1) / 2
      IPOS = 0
      INC = NS + NSW - 1
      IMAX = (NLW) * INC
      NLWM1 = NLW - 1
      IF (NLWM1 .NE. 0) THEN
C
C  READ INITIAL NLW LINES INTO CORE, REFLECTING AT BOUNDARIES
C
         DO 200 I=1,NLWM1
            ILINE=LINE
            LINE=LINE+1
  201       IF (ILINE .LT. 1) ILINE = 2 - ILINE
            IF (ILINE .GT. NL) ILINE = M - ILINE
            IF (ILINE .LT. 1) GO TO 201
            call xvread(inunit,inbuf(ipos+l1),stat,'NSAMPS',ns,'SAMP',ss,
     &                'LINE',sl+iline,' ')
            IF (NSW .NE. 1) THEN
                do j=1,L
                   inbuf(ipos+L3+1-j)=inbuf(ipos+L2-1+j)
                   inbuf(ipos+L5-1+j)=inbuf(ipos+L4+1-j)
                enddo
            ENDIF
            IPOS=IPOS+INC
200      CONTINUE
      ENDIF


      DO 300 I = 1, NL
         ILINE = LINE
         LINE = LINE + 1
301      IF (ILINE .LT. 1) ILINE = 2 - ILINE
         IF(ILINE .GT. NL) ILINE = M - ILINE
         IF (ILINE .LT. 1) GOTO 301
         call xvread(inunit,inbuf(ipos+l1),stat,'NSAMPS',ns,'SAMP',ss,
     &               'LINE',sl+iline,' ')
         IF(NSW .NE. 1) THEN
             do j=1,L
                inbuf(ipos+L3+1-j)=inbuf(ipos+L2-1+j)
                inbuf(ipos+L5-1+j)=inbuf(ipos+L4+1-j)
             enddo
         ENDIF

         call min2d(inbuf,outbuf,nlw,nsw,ns)

         call xvwrit(outunit,outbuf,stat,' ')
         IPOS=IPOS+INC
         IF(IPOS .GE. IMAX) IPOS=0
         CURR=MOD(CURR,NLW)+1
300   CONTINUE
C
C *****         CLOSE DATA SETS
C
      call xvclose(inunit,stat,' ')
      call xvclose(outunit,stat,' ')
      RETURN
      END
c==============================================================
      subroutine min2d(in,out,nlw,nsw,ns)
	implicit none
c      integer*2 in(1),out(1),mindn
	integer*4 nlw,nsw,ns
	real*4 in(ns),out(ns),mindn 
      integer*4 i,j,k,len,m,mincol
      len=ns+nsw-1

c find min dn for left most block
      k=0
      mindn=in(1)
      mincol=1
      do j=1,nlw
        do i=1,nsw
          k=k+1
          if(in(k).lt.mindn)then
             mincol=i
             mindn=in(k)      
          endif
        enddo
        k=j*len
      enddo
      out(1)=mindn

c do the rest
      do i=2,ns
        if(mincol.ge.i)then                ! check only right column
           k=i+nsw-1
           do j=1,nlw
             if(in(k).lt.mindn)then
                mincol=i+nsw-1
                mindn=in(k)      
             endif
             k=k+len
           enddo
           out(i)=mindn
        else                               ! check all columns
           mindn=in(i)
           mincol=i
           k=i
           do j=1,nlw
             do m=i,i+nsw-1
               if(in(k).lt.mindn)then
                  mincol=m
                  mindn=in(k)      
               endif
               k=k+1
             enddo
             k=j*len+i
           enddo
           out(i)=mindn
        endif
      enddo
      return
      end
