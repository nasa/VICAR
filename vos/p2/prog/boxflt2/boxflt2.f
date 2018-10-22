      INCLUDE 'VICMAIN_FOR'
C**********************************************************************
      subroutine main44
C
C        MODIFIED FOR VAX CONVERSION BY ALAN MAZER 28-JUL-83
C        CONVERTED TO VICAR2 BY J. REIMER 14-AUG-85
C
C        9-88  SP   MODIFIED BECAUSE DIV HAS BEEN RENAMED TO DIVV.
C        4-94  CRI  MSTP S/W CONVERSION (VICAR PORTING)
C
	implicit none
      	external main
      COMMON /C1/ IUNIT,OUNIT,SL,SS,NLO,NSO,FORMAT,fmt,HIGH,NLW,NSW,
     &            ICYCLE,JCYCLE,IDC 

      	integer*4 iunit,ounit,stat,sl,ss,high
	integer*4 nlo,nso,nlw,nsw,icycle,jcycle,icode
	integer*4 ns,nli,nsi,idef,icount
	real*4 idc
	character*8 fmt(4)/'BYTE','HALF','FULL','REAL'/
      	character*8 format
      	logical*4 XVPTST

C        SET DEFAULTS AND INITIALIZE
      nlw=11
      nsw=11
      icycle=0
      jcycle=0
      idc=128.

C
      call ifmessage('BOXFLT2  02-May-2011 (64-bit) RJB ')
C
C          OPEN INPUT DATA SET
      call xveaction('SA',' ')
      call xvunit(iunit,'INP',1,stat,' ')
      call xvopen(iunit,stat,'OPEN_ACT','SA','IO_ACT','SA',' ')
C
C        GET DATA FORMAT AND CHECK
      call xvget(iunit,stat,'FORMAT',format,' ')

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

C
C        GET SIZE INFORMATION AND CHECK
      call xvsize(sl,ss,nlo,nso,nli,nsi)
      if(sl+nlo-1 .gt. nli) then
         call xvmessage('??E - Number of lines requested exceeds input size',' ')
         call abend
      endif
      if(ss+nso-1 .gt. nsi) then
         call xvmessage('??E - Number of samples requested exceeds input size',' ')
         call abend
      endif
C
C        OPEN OUTPUT DATA SET
      call xvunit(ounit,'OUT',1,stat,' ')
	call xvopen(ounit,stat,'OP','WRITE','U_NL',nlo,'U_NS',nso,
     & 'OPEN_ACT','SA','IO_ACT','SA','O_FORMAT',fmt(icode),
     & 'U_FORMAT',fmt(4),' ')				!,FMT(OUTCODE),' ')

C           PROCESS PARAMETERS
C        'HIGHPASS'
	high = 0
      if(xvptst('HIGHPASS')) high=1
C        'NLW'
      call xvparm('NLW',nlw,icount,idef,1)
      if(nlw/2*2 .eq. nlw) call xvmessage('??W - WARNING-nlw is an even integer',' ')
C        'NSW'
      call xvparm('NSW',nsw,icount,idef,1)
      if(nsw/2*2 .eq. nsw) call xvmessage('??W - WARNING-nsw is an even integer',' ')
C        'CYCLE'
      if (xvptst('CYCLE')) then
         icycle=1
         jcycle=1
      endif
C        'SCYCLE'
      if (xvptst('SCYCLE')) icycle=1
C        'LCYCLE'
      if (xvptst('LCYCLE')) jcycle=1
C        'DCLEVEL'
      call xvparm('DCLEVEL',idc,icount,idef,1)

      ns=nso+nsw
c 4*ns is number of bytes to reserve
c 			  ISUM,TBUF,INBUF,OUTBUF,IDBUF
      CALL STACKA(7,MAIN,5,4*ns,4*ns,4*ns,4*ns,4*ns)	!(7,MAIN,5,4*NS,4*NS,2*NS,2*NS,2*NS)

C        CLOSE DATA SETS
      call xvclose(iunit,stat,' ')
      call xvclose(ounit,stat,' ')
C
      return
      end
C**********************************************************************
      SUBROUTINE MAIN(ISUM,LX,TBUF,MX,INBUF,IX,OUTBUF,JX,IDBUF,KX)

	implicit none
      COMMON /C1/ IUNIT,OUNIT,SL,SS,NLO,NSO,FORMAT,fmt,HIGH,NLW,NSW,
     &            ICYCLE,JCYCLE,IDC 
	
        integer*4 iunit,ounit,stat,ss,sl,nlo,nso,high,nlw,nsw
	integer*4 icycle,jcycle,lx,mx,ix,jx,kx,icode
	integer*4 i,l,m,n,iline,jline
c	integer*4 nlx,nsx		! for xvget now commented out
c      INTEGER*2 INBUF(1),OUTBUF(1),IDBUF(1)
	real*4 isum(lx),tbuf(mx),idc,nlwnsw
	real*4 inbuf(ix),outbuf(jx),idbuf(kx)
	character*8 fmt(4)
      	character*8 format
	
	icode = 0
        if (format.eq.'BYTE') icode=1
        if (format.eq.'HALF'.or.format.eq.'WORD') icode=2
        if (format.eq.'FULL') icode=3
        if (format.eq.'REAL') icode=4

      m=nlw/2+1
      n=nsw/2+1
      l=n-1
c      CALL ZIA(ISUM,NSO+NSW-1)  		!need to replace 
c	zero out the expanded (padded) buffer
	do i=1,nso+nsw-1
	   isum(i)=0.0
	enddo	
c
c	goes thru file, reflects or cycles and writes out
c	to ounit temporarily

      do i=1,nlo
          iline=1-m+i
          if (jcycle .eq. 0) then
              if (iline .lt. 1) iline=2-iline
              if (iline .gt. nlo) iline=nlo+nlo-iline
          else              	      
              if (iline .lt. 1) iline=nlo+iline
              if (iline .gt. nlo) iline=iline-nlo
          endif
c          call xvread(iunit,inbuf(n),stat,'LINE',sl+iline-1,
	    call xvread(iunit,inbuf(n),stat,'LINE',sl+iline-1,
     &                'SAMP',ss,'NSAMPS',nso,' ')  
 
          if (i.le.nlw) then
              if (n.gt.1) then
                  if (icycle .eq. 0) then
                      call rflct(n,nso,inbuf)
                  else 
                      call cycle(n,nso,inbuf)
                  endif
              endif
c	isum is output - it is the expanded buffer
              call addv(7,nso+nsw-1,inbuf,isum,1,1)
          endif
          call xvwrit(ounit,inbuf(n),stat,' ')
      enddo

      call zaire(tbuf,isum,nso,nsw)
	nlwnsw = nlw*nsw
      call divv(7,nso,nlwnsw,tbuf,0,1)				!formerly 7 was 4 call divv(4,nso,nlw*nsw,tbuf,0,1)
      call mve(7,nso,tbuf,outbuf,1,1)				!formerly 7 was -6
c	now we have finished updating output (same size as oiginal but with 
C        RE-OPEN OUTPUT TO REREAD
      call xvclose(ounit,stat,' ')
      call xvopen(ounit,stat,'OP','UPDATE','I_FORMAT',fmt(icode),'U_FORMAT',fmt(4),' ')
c	call xvget(iunit,stat,'NL',nlx,'NS',nsx,' ')
      iline=(nlw+1)/2

c
      do i=2,nlo
c          Call xvread(ounit,idbuf(n),stat,'LINE',i-1,' ')
	  call xvread(ounit,idbuf(n),stat,'LINE',i-1,' ')
          call xvwrit(ounit,outbuf,stat,'LINE',i-1,' ')

          if (n.gt.1) then
              if (icycle .eq. 0) then 
                  call rflct(n,nso,idbuf)
              else 
                  call cycle(n,nso,idbuf)
              endif
          endif
          call rsubv(7,nso+nsw-1,idbuf,isum,1,1)		!formerly 7 was 6

          jline=iline+1
          if (jcycle .eq. 0.and.jline.gt.nlo) jline=nlo+nlo-jline
          if (jcycle.ne.0 .and. jline .gt. nlo) jline=jline-nlo
c          call xvread(iunit,inbuf(n),stat,'LINE',sl+jline-1,
	   call xvread(iunit,inbuf(n),stat,'LINE',sl+jline-1,
     &                'SAMP',SS,'NSAMPS',nso,' ')        
          if (n.gt.1) then
              if (icycle .eq. 0) then 
                  call rflct(n,nso,inbuf)
              else 
                  call cycle(n,nso,inbuf)
              endif
          endif
          call addv(7,nso+nsw-1,inbuf,isum,1,1)		!formerly 7 was 6
          call zaire(tbuf,isum,nso,nsw)	
          call divv(7,nso,nlwnsw,tbuf,0,1)		!formerly 7 was 4	call divv(4,nso,nlw*nsw,tbuf,0,1)
          call mve(7,nso,tbuf,outbuf,1,1)		!formerly 7 ws -6
          iline=iline+1
      enddo

      call xvwrit(ounit,outbuf,stat,'LINE',i-1,' ')

      if (high .ne. 1) return

C        DO HIGHPASS OPERATION
      do i=1,nlo
          call xvread(iunit,inbuf,stat,'LINE',sl+i-1,
     &                'SAMP',SS,'NSAMPS',nso,' ')
          call xvread(ounit,outbuf,stat,'LINE',i,' ')
          call rsubv(7,nso,outbuf,inbuf,1,1)		!formerly 7 was 2
          call addv(7,nso,idc,inbuf,0,1)		!formerly 7 was -6
          if (format.eq.'BYTE') call cutoff(inbuf,nso)
          call xvwrit(ounit,inbuf,stat,'LINE',I,' ')
      enddo

      call xvmessage('HIGH PASS FILTER PERFORMED.',' ')

      return
      end
C**********************************************************************
      SUBROUTINE RFLCT(N,NSO,INBUF)
	implicit none
	real*4 inbuf(1)
	integer*4 l,n,nso
c      INTEGER*2 INBUF(1)

      l=n-1
      call mve(7,l,inbuf(n+1),inbuf(n-1),1,-1)		!formerly 7 was 2
      call mve(7,l,inbuf(nso+l-1),inbuf(nso+n),-1,1)	!formerly 7 was 2

      return
      end
C**********************************************************************
      SUBROUTINE CYCLE(N,NSO,INBUF)
        implicit none
        real*4 inbuf(1)
        integer*4 l,n,nso

c      INTEGER*2 INBUF(1)

      l=n-1
      call mve(7,l,inbuf(nso+1),inbuf(1),1,1)		!formerly 7 was 2
      call mve(7,l,inbuf(n),inbuf(n+nso),1,1)		!formerly 7 was 2

      return
      end
C**********************************************************************
      SUBROUTINE CUTOFF(INBUF,NSO)
        implicit none
        real*4 inbuf(1)
        integer*4 i,nso
c      INTEGER*2 INBUF(1)

      do i=1,nso
          if (inbuf(i).gt.255) inbuf(i)=255
          if (inbuf(i).lt.0) inbuf(i)=0
      enddo

      return
      end
