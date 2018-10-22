      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C
C     MODIFIED FOR VAX CONVERSION BY ALAN S MAZER, 27 SEPT 1983
C     JHR: CONVERTED TO VICAR2   1 JULY 1985
C     AS(CRI): MSTP S/W CONVERSION (VICAR PORTING) 1 JULY 1994
C     ENABLED 3D IMAGE CAPABILITY (N TOOLE), 16 SEPT 2003
C
C     PUTS A SINE WAVE AROUND PICTURE EDGES TO STOP FFT2 LEAKAGE.
C     KEYWORD FOR THE EDGE IS 'EDGE', DEFAULT VALUE IS 10.
C
	implicit none
      	real*4 r1(8192),r2(8192)
      	integer*4 ounit,stat,ss,sl,sb,nbo,band,bandout,lineout,nbi
	integer*4 iunit,npix,icode,nlo,nso,nli,nsi,icount,idef
	integer*4 ii,j,l,m,line,ln
c      	integer*2 HBUF(8192)
	real*4 hbuf(8192)
	real*4  p,a,rat,diff,diff1,rr,rl
	character*8 fmt(4)/'BYTE','HALF','FULL','REAL'/
      	character*8 format

      	CHARACTER*3 ORGIN
C
      COMMON/C1/R1,R2,HBUF
C
C        SET DEFAULTS AND INITIALIZE
      	npix=10
C
      call ifmessage('APODIZE - 28-Jun-2012')
      CALL XVEACTION('SA',' ')
C          OPEN INPUT DATA SET
      call xvunit(iunit,'INP',1,stat,' ')
      call xvopen(iunit,stat,'OPEN_ACT','SA','IO_ACT','SA',' ')
C
C        GET DATA FORMAT AND CHECK
      call xvget(iunit,stat,'FORMAT',FORMAT,'ORG',orgin,' ')

	icode = 0
	if (format.eq.'BYTE') icode=1
	if (format.eq.'HALF'.or.format.eq.'WORD') icode=2
	if (format.eq.'FULL') icode=3
	if (format.eq.'REAL') icode=4
	if (icode.eq.0) then
		call xvmessage('??E - Unknown data format for input image',' ')
		call abend  
	endif
	call xvclose(iunit,stat,' ')
	call xvopen(iunit,stat,'OPEN_ACT','SA','IO_ACT','SA',
     &		'I_FORMAT',fmt(icode),'U_FORMAT',fmt(4),' ')		!FMT(INCODE),' ')

c     Check organization of image, prohibit BIP
      if (orgin.eq.'BIP') call mabend(
     +  '??E - BIP files not supported, use TRAN to convert to BSQ')

C
C        GET SIZE INFORMATION AND CHECK
      call xvsize(sl,ss,nlo,nso,nli,nsi)
      if(sl+nlo-1 .gt. nli) then
         call xvmessage
     &    ('??E - Number of lines requested exceeds input size',' ')
         call abend
      endif
      if(ss+nso-1 .gt. nsi) then
         call xvmessage
     &    ('??E - Number of samples requested exceeds input size',' ')
         call abend
      endif
      if(nso .gt. 8192) then
         call xvmessage
     &    ('??E - Number of samples exceeds buffer size of 8192',' ')
         call abend
      endif

      call xvbands(sb,nbo,nbi)

      if (sb.gt.nbi) call mabend(
     & '??E - SB is greater than the total number of bands')
                 
      if ( sb + nbo - 1 .gt. nbi) then
         call xvmessage('***Number of bands truncated', ' ')
         nbo = nbi + 1 - sb     
      endif

C
C        OPEN OUTPUT DATA SET
      call xvunit(ounit,'OUT',1,stat,' ')
      call xvopen(ounit,stat,'OP','WRITE','O_FORMAT',fmt(icode),
     & 'U_FORMAT',fmt(4),'U_NL',nlo,'U_NS',nso,'U_NB',nbo,' ')
C
C           PROCESS PARAMETERS
C        'EDGE'
      call xvparm('EDGE',npix,icount,idef,1)
C      
C        SETUP FOR MAIN PROCESSING
      p=npix*2
      rat=3.14159/p
      ii=npix*2
C
C        PROCESS LEFT AND RIGHT BORDERS
      bandout = 0
      do 35 band = sb,nbo+sb-1
        bandout = bandout + 1
        lineout = 0
        do 40 l=1,nlo
          line=l+sl-1
          lineout = lineout + 1
          call xvread(iunit,hbuf,stat,'LINE',line,'BAND',
     &            band,'SAMP',ss,'NSAMPS',nso,' ')
          rr=hbuf(npix)
          rl=hbuf(nso-npix+1)
          diff1=rl-rr
          diff=abs(diff1)/2.0
          a=1.0
          if (diff.gt.1.0e-20) a=(-diff1)/(abs(diff1))
          do 50 m=1,ii
            ln=m-npix
            if (ln.le.0) then
              hbuf(nso+ln)=diff*sin(rat*ln*a)+amin1(rl,rr)+diff+0.5
            else
              hbuf(ln)=diff*sin(rat*ln*a)+amin1(rl,rr)+diff+0.5
            endif
50        continue
          call xvwrit(ounit,hbuf,stat,'NSAMPS',nso,'LINE',lineout,
     +           'BAND',bandout,' ')
40      continue
35    continue
C
C        RE-OPEN OUTPUT FOR UPDATE
      call xvclose(ounit,stat,' ')
      call xvopen(ounit,stat,'OP','UPDATE','O_FORMAT',fmt(icode),
     & 'U_FORMAT', fmt(4), 'U_NL',nlo,'U_NS',nso,'U_NB',nbo,' ')
C
C        PROCESS TOP AND BOTTOM BORDERS
      do 5 band=1,nbo
         line=npix 
         call xvread(ounit,hbuf,stat,'LINE',line,'BAND',band,
     &            'SAMP',ss,'NSAMPS',nso,' ')
         do 71 j=1,nso
            r1(j)=hbuf(j)
71       continue
         line=nlo-npix+1
         call xvread(ounit,hbuf,stat,'LINE',line,'BAND',band,
     &            'SAMP',ss,'NSAMPS',nso,' ')
         do 70 j=1,nso
            r2(j)=hbuf(j)
70       continue
         do 10 l=1,ii
            ln=npix-l
            do 20 m=1,nso
               diff1=r2(m)-r1(m)
               diff=abs(diff1)/2.0
               a=1.0
               if (diff.gt.1.0e-20) a=diff1/abs(diff1)
               hbuf(m)=diff*sin(rat*ln*a)+amin1(r2(m),r1(m))+diff+0.5
20          continue
            if (ln.ge.0) then
               line=nlo-ln
            else
               line=iabs(ln)
            endif
            call xvwrit(ounit,hbuf,stat,'LINE',line,'NSAMPS',nso,
     +         'BAND',band,' ')
10       continue
5     continue
C
C        CLOSE DATA SETS
      call xvclose(iunit,stat,' ')
      call xvclose(ounit,stat,' ')
C
      return
      end
