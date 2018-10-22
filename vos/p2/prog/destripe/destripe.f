C
      INCLUDE 'VICMAIN_FOR'

      SUBROUTINE MAIN44
	implicit none

	external horzavg,vertavg			!stacka 

	integer*4 stat,icode,cnt,idef,nb
      	integer*4 ounit,iunit,nl,ns,i
	integer*4 window(4)
	integer*8 i8window(4)
 	integer*8 jj,kk,ll
	
	character*4 orgin 
	character*6 orient
	character*8 fmt(5)/'BYTE','HALF','FULL','REAL','DOUB'/
      	character*8 format
	
	call xvmessage ('** DESTRIPE version 2016-05-26',' ')

C
C  OPEN INPUT & OUTPUT FOR SEQUENTIAL I/O
C
      call xvunit(iunit,'INP',1,stat,' ')
	call xvopen(iunit,stat,'OPEN_ACT','SA','IO_ACT','SA',' ')
      call xvget(iunit,stat,'FORMAT',FORMAT,'ORG',orgin,' ')
	call xvget(iunit,stat,'NL',nl,'NS',ns,'NB',nb,' ')
	icode = 0
	if (format.eq.'BYTE') icode=1
	if (format.eq.'HALF'.or.format.eq.'WORD') icode=2
	if (format.eq.'FULL') icode=3
	if (format.eq.'REAL') icode=4
	if (format.eq.'DOUB') icode=5
	if (icode.eq.0) then
		call xvmessage('??E - Unknown data format for input image',' ')
		call abend  
	endif
        if (orgin.eq.'BIP') call mabend(
     +  '??E - BIP files not supported, use TRAN to convert to BSQ')

	call xvclose(iunit,stat,' ')

      call xvopen(iunit,stat,'OPEN_ACT','SA','IO_ACT','SA',
     +            'I_FORMAT',fmt(icode),'U_FORMAT',fmt(4),' ')

c      call xvsize(sl,ss,nl,ns,nlin,nsin)
c	the following calls were never implemented
c	call xvp('ONL',onl,cnt)
c	call xvp('ONS',ons,cnt)
      call xvunit(ounit,'OUT',1,STAT,' ')
      call xvopen(ounit,stat,'OP','WRITE','OPEN_ACT','SA',
     +          'IO_ACT','SA','O_FORMAT',fmt(icode),'U_FORMAT',fmt(4),' ')

C
	call xvparm ('WINDOW',window,cnt,idef,4)
	if (window(3) .eq. 0 .and. window(4) .eq. 0) then
		window(3) = nl
		window(4) = ns
	endif
C	HORIZ is in line direction.
C	VERT is in sample direction.

	do i=1,4
	   i8window(i) = window(i)
	enddo
	call xvparm ('ORIENT',orient,cnt,idef,1)
c	convert stacka_big variable to int*8
	jj = ns*8		!real*4
	kk = nl*8		!real*4
	ll = nl*ns*4	!real*4 size in bytes
c 	print *, "nl   ns  = ll,kk,jj = ",nl,ns, ll,kk,jj	
	if (orient.eq.'HORIZ') then
	     call stacka_big (10,horzavg,3,ll,kk,kk,i8window,nl,ns,iunit,ounit)
c		call horzavg (iunit,ounit,nl,ns,window)
	else
	     call stacka_big (10,vertavg,3,ll,jj,jj,i8window,nl,ns,iunit,ounit)
c		call vertavg (iunit,ounit,nl,ns,window)
	endif

	call xvclose(iunit,stat,' ')
	call xvclose(ounit,stat,' ')

	return
	end
c==================================================================================
        subroutine horzavg (iimage,ll,avg,jj,adj,kk,
	1 i8window,nl,ns,iunit,ounit)
c
c	computation for horizontal striping - rows
c	you see stripes vary from row to row
        implicit none
c
	integer*4 iunit,ounit,stat,nl,ns,elw,esw,i,j
	integer*4 window(4)
	integer*8 jj,kk,ll,i8nl,i8ns
	integer*8 mem1,mem2,mem3
	integer*8 i8window(4)
	real*4 iimage(ns,nl)
	real*8 avg(nl),adj(nl),avglmax,savg
	character*80 msg
c
        call xvmessage('HORIZ',' ')
	j=0
	do i=1,4
            window(i) = i8window(i)
	enddo
	i8nl = int8(nl)
	i8ns = int8(ns)
        mem1 = i8nl * i8ns * 4
	mem2 = i8nl * 8
	mem3 = i8nl * 8

        write (msg,10100), mem1, mem2, mem3
10100 format ("memory allocated  1 = ",i12, ", 2 = ",i12,", 3 = ",i12 )
	call xvmessage (msg,' ')

	if (ll .lt. mem1 .or. jj .lt. mem2 .or. kk .lt. mem3) then
	   call xvmessage ('??E - Unable to allocate enough memory in stacka',' ')
	   call abend
	endif
c	iunit = i8unit
c	ounit = o8unit

c	stop
c	write (msg,10100) jj,kk,ll,mm,nn,ind
c10100 format (6i5)			!to prevent messages
	elw = window(3) - window(1) + 1		!nl - sl + 1
	esw = window(4) - window(2) + 1		!ns - ss + 1
c	print *, ' sl,ss,nl,ns elw,esw = ',window(1),window(2),nl,ns,elw,esw
        do i = 1,nl
	    call xvread (iunit,iimage(1,i),stat,'NSAMPS',ns,'LINE',i,' ')		!1,ia
	enddo
	savg = 0.0d0
        do i=window(1),elw
	     savg = 0.0d0
             do j=window(2),esw
                savg = savg + dble(iimage(j,i))             !j,i    go thru samples
             enddo
	     avg(i) = savg/window(4)                     !average per row - sample direction
        enddo
c	do i=1,nl
c	   print *,'avg(i) = ',avg(i)
c	enddo
	
	avglmax = -1.00d38
c
c	go across each column
	do i=window(1),elw	!sl,nl
	    if (avg(i) .gt. avglmax) avglmax = avg(i) 	!get the maximum/col
	enddo
	do i=window(1),elw
	    adj(i) = (avglmax - avg(i))			!subtract 
	enddo
	do i=1,nl
	    iimage(j,i)  = iimage(j,i) + real(adj(i))	!j,i
	    call xvwrit(ounit,iimage(1,i),stat,'NSAMPS',ns,' ')	
	enddo

        return
        end

c==================================================================================
        subroutine vertavg (iimage,ll,avg,jj,adj,kk,
	1 i8window,nl,ns,iunit,ounit)
c
        implicit none
c
        integer*4 iunit,ounit,stat,nl,ns,elw,esw,i,j
        integer*4 window(4)
        integer*8 jj,kk,ll,i8nl,i8ns
        integer*8 mem1,mem2,mem3
        integer*8 i8window(4)
        real*4 iimage(ns,nl)
        real*8 avg(ns),adj(ns),avgmax,savg
        character*80 msg

C	4*12 + 4*4 + 4*4 = 80 
c
        call xvmessage('VERT',' ')
        do i=1,4
            window(i) = i8window(i)
        enddo
        i8nl = nl
        i8ns = ns
        mem1 = i8nl * i8ns * 4
        mem2 = i8nl * 8
        mem3 = i8nl * 8
        write (msg,10100), mem1, mem2, mem3
10100 format ("memory allocated  1 = ",i12, ", 2 = ",i12,", 3 = ",i12 )
        call xvmessage (msg,' ')
c        if (nl*ns .gt. 536200000) then
c           call xvmessage ('??E - nl*ns gt 536,200,000 full words (2,144,800,000 bytes)',' ')
c           call abend
c        endif
        if (ll .lt. mem1 .or. jj .lt. mem2 .or. kk .lt. mem3) then
           call xvmessage ('??E - Unable to allocate enough memory in stacka',' ')
           call abend
        endif
c        iunit = i8unit
c        ounit = o8unit
        elw = window(3) - window(1) + 1		!nl - sl + 1
        esw = window(4) - window(2) + 1		!ns - ss + 1
	 print *,'windows 1,2,3,4 = ',window(1), window(2), window(3), window(4)
        do i = 1,nl
            call xvread (iunit,iimage(1,i),stat,'NSAMPS',ns,' ')		!1,i
        enddo

	do i=window(2),esw		!pick sample
	    savg = 0.0d0
            do j=window(1),elw		!go thru lines
                savg = savg + dble(iimage(i,j))	!i,j 
            enddo
	    avg(i) = savg/window(3)	!average per column - line direction
	enddo
        avgmax = -1.00d38
        do i=window(2),esw
            if (avg(i) .gt. avgmax) avgmax = avg(i)
        enddo

        do i=window(2),esw
            adj(i) = avgmax - avg(i) 
        enddo

        do i=1,ns 
            do j=1,nl
                iimage(i,j)  = iimage(i,j)  + real(adj(i))		!i,j
            enddo
        enddo
        do i=1,nl
	    call xvwrit(ounit,iimage(1,i),stat,'NSAMPS',ns,' ')		!1,i
	enddo
        return
        end

