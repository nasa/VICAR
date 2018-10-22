C
C	VICAR PROGRAM IMGSTAT
C
C-------------------------------------------------------------------
C Edit History:
c
C        BAM 2/95
c        BAM 3/96
c        BAM 10/96
C        NDR 1/97  -- Added Slope deviation output options.
c	 RJB 4-13-2011 - internals to real*4
c	TBD: This may need a x and y scaling feature for slopedev.
c
C-------------------------------------------------------------------


	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
	implicit none  !Dont use implicit integer
	integer maxin,maxout,maxwind
	parameter (MAXIN=14000,MAXOUT=MAXIN/2,MAXWIND=36) !ndr 3/96

c        integer*2 databuf(MAXIN*MAXWIND)
c        integer*2 windowbuf(MAXWIND*MAXWIND)
c        integer*2 out1(MAXOUT), out2(MAXOUT), out3(MAXOUT), out4(MAXOUT)
        real*4    sout(MAXOUT) ! for MSSD mode
        integer*4 nli,nsi,cur_line,ninput,dcode
	integer*4 imin,imax,ipt,mm,begin,ist
	integer*4 i,j,k,l,m,status
	integer*4 nl,ns,nlw,nodset,icount
	integer*4 sl,ss,lines,samps,windowsize
	integer*4 iunit(3),out(5)
	real*4    dxbuf(maxout),dybuf(maxout),dx,dy
	real*4    databuf(MAXIN*MAXWIND)
	real*4    out1(MAXOUT), out2(MAXOUT), out3(MAXOUT), out4(MAXOUT)
	real*4    windowbuf(MAXWIND*MAXWIND)
	real*4 	  cmin,cmax
        integer*4 nn,next,icode
        
        real*8 mean, sd, sum, sldev, slerr, center
        logical*4 XVPTST,KMIN,KMAX,KMEAN,KSD,KMSSD
        integer outs(5)

	character*4 fmt(4)/'BYTE','HALF','FULL','REAL'/
	character*5 format
	character*8 org

	call xvmessage ('IMGSTAT version 13-Apr-2011',' ')
! PARAMETER PROCESSING 

        call xvpcnt ( 'INP',  ninput )  ! the input data sets
! Note all inputs must be of same format
	call xvunit(iunit(1),'INP',1,status,' ')
        call xvopen(iunit(1),status,'OPEN_ACT','SA','IO_ACT','SA',' ')

        call xvget(iunit(1),status,'FORMAT',format,'ORG',org,' ')

        icode = 0
        if (format.eq.'BYTE') icode=1
        if (format.eq.'HALF' .or. format.eq.'WORD') icode=2
        if (format.eq.'FULL') icode=3
        if (format.eq.'REAL') icode=4
        if (icode.eq.0) then
                call xvmessage('??E - Unknown data format for input image',' ')
                call abend
        endif
        call xvclose(iunit(1),status,' ')

	do i=1,ninput                   ! open the input
	  call xvunit(iunit(i),'INP',i,status,' ')
	  if (status.ne.1) call xvsignal(iunit(i),status,1)
	end do
	
	call xvopen(iunit(1),status,'I_FORMAT',fmt(icode),'U_FORMAT',fmt(4),' ')
	if (status.ne.1) call xvsignal(iunit(1),status,1)
	
	if (ninput.eq.3) then           ! check for derivative inputs
	  call xvopen(iunit(2),status,'I_FORMAT',fmt(icode),'U_FORMAT',fmt(4),' ')
	  if (status.ne.1) call xvsignal(iunit(2),status,1)
	  call xvopen(iunit(3),status,'I_FORMAT',fmt(icode),'U_FORMAT',fmt(4),' ')
	  if (status.ne.1) call xvsignal(iunit(3),status,1)
	end if
	
        call xvpcnt ( 'OUT',  nodset )  ! the output data sets

	call xvp('WINDOW',nlw,icount) ! get window size

	call xvsize( sl, ss, nl, ns, nli,nsi)

	if(ns .gt. maxin) then    ! bam 2/95 !! ndr 3/96
	  call xvmessage('??E - Image has too many samples',' ')
	  call abend
	endif

        kmin  = XVPTST('MIN')    ! SEE WHAT WE WISH TO PROCESS
        kmax  = XVPTST('MAX')
        kmean = XVPTST('MEAN')
        ksd   = XVPTST('SD')
        kmssd = XVPTST('MSSD')

        lines = nl / nlw                ! get # of lines and samples
        samps = ns / nlw
        windowsize = nlw * nlw
	center = (1.0 + nlw)/2.0

        do i = 1,5                      ! initialize output files required
            outs(i) = 1                 ! default = all on
        end do

        if ( .not. kmean ) outs(1) = 0
        if ( .not. kmin  ) outs(2) = 0
        if ( .not. kmax  ) outs(3) = 0
        if ( .not. ksd   ) outs(4) = 0
        if ( .not. kmssd ) outs(5) = 0


!       check we have enough output files specified for the work required

        sum = outs(1) + outs(2) + outs(3) + outs(4) + outs(5)
        if ( sum .ne. nodset ) then
          call xvmessage('??E - Outputs requested do not match number of output data sets.',' ')
          call abend
        endif


        do i = 1,nodset
	    call xvunit(out(i),'OUT',i,status,' ')
	    if (status.ne.1) call xvsignal(out(i),status,1)
	    call xvopen(out(i),status,'OP','WRITE','OPEN_ACT','SA',
     -       'O_FORMAT',fmt(icode),'U_FORMAT',fmt(4),'U_NL',lines, 'U_NS',samps,' ')
	    if (status.ne.1) call xvsignal(out(i),status,1)
        end do
c	decode 1=BYTE, 2=HALF, 4=FULL, 7=REAL, 8=DOUBLE, 10=COMPLEX
        dcode = 7
	cur_line = 1

        do i = 1, lines

            do j = 1, nlw              ! read a window of lines - all samples
               ipt = 1 + ( j - 1 ) * ns
               call xvread(iunit(1),databuf(ipt),status,'NSAMPS',ns,
     +              'line',cur_line,' ')
	       cur_line = cur_line+1
	       if (status.ne.1) call xvsignal(iunit(1),status,1)
            end do

	    if (ninput.eq.3) then  ! Read in a line of DX and DY slopes
               call xvread(iunit(2),dxbuf,status, ' ')
	       if (status.ne.1) call xvsignal(iunit(2),status,1)
               call xvread(iunit(3),dybuf,status, ' ')
	       if (status.ne.1) call xvsignal(iunit(3),status,1)
	    end if
	    
            do k = 1, samps                     ! loop through the windows
                mm = 1
                begin = ( k - 1 ) * nlw 
                sum = 0.0d0
                do l = 1, nlw                   ! # of lines
                    ist = begin + ( l - 1 ) * ns
                    do m = 1, nlw               ! # of samples
                        sum = sum + databuf(ist+m)
                        windowbuf(mm) = databuf(ist+m)
                        mm = mm + 1
                    end do                      !Loop for one window-line
                end do                 !Loop for one window over a sample

                out1(k) = sum / windowsize
c  minmax is sub in P2
c	all parms integer*4 except windowbuf
                call minmax (dcode, windowsize, windowbuf, 
     +            cmin, cmax, imin, imax)
                out2(k) = cmin
                out3(k) = cmax

                mean = out1(k)
                sum = 0.0d0
                do nn = 1, windowsize
                    sum = sum + (windowbuf(nn) - mean )**2
                end do
                sd = sum / windowsize
                out4(k) = dsqrt(sd)
		
		if (ninput.ne.3) continue  !with k-samp loop
		
		!compute mean slope deviation
		dx = dxbuf(k)  !slope with increasing x (to right)
		dy = dybuf(k)  !slope with increasing y (up)
		sldev = 0.0d0
		mm = 1
                do l = 1, nlw                   ! # of lines
                    do m = 1, nlw               ! # of samples
                        slerr = windowbuf(mm) 
			slerr = slerr - 
     +				(mean + dx*(m-center) + dy*(center-l))
                        sldev = sldev + slerr*slerr
                        mm = mm + 1
                    end do  !Loop for one window-line
                end do !Loop for one window over a sample
		
		sldev = dsqrt( sldev / (windowsize -1) )   ! standard deviation
                sout(k) = sldev
		
            end do


            next = 1                       ! output files
            if ( outs(1) .eq. 1 ) then     ! mean required
                call xvwrit(out(next),out1,status,' ')
                if (status.ne.1) call xvsignal(out(next),status,1) 
                next = next + 1
            end if
            if ( outs(2) .eq. 1 ) then      ! min required
                call xvwrit(out(next),out2,status,' ')
                if (status.ne.1) call xvsignal(out(next),status,1) 
                next = next + 1
            end if
            if ( outs(3) .eq. 1 ) then ! max required
                call xvwrit(out(next),out3,status,' ')
                if (status.ne.1) call xvsignal(out(next),status,1) 
                next = next + 1
            end if
            if ( outs(4) .eq. 1 ) then ! sd required
                call xvwrit(out(next),out4,status,' ')
                if (status.ne.1) call xvsignal(out(next),status,1) 
                next = next + 1
            end if
            if ( outs(5) .eq. 1 ) then ! sd required
                call xvwrit(out(next),sout,status,' ')
    	        if (status.ne.1) call xvsignal(sout,status,1)
            end if


        end do   	! end of main loop



        do i = i, ninput   ! close input
   	        call xvclose(iunit(i),status,' ')
		if (status.ne.1) call xvsignal(out(i),status,1)
	enddo

	do i=1,nodset      ! and output
		call xvclose(out(i),status,' ')
		if (status.ne.1) call xvsignal(out(i),status,1)
	enddo


	return
 	end

