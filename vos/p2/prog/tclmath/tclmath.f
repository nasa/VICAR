	include 'VICMAIN_FOR'
	subroutine main44
c
c	realmath
c
	implicit none
	integer*4 numinvals,cnt,numoutvals,i
	real*4 invals(20),outvals(20),suminval
	character*12 function
	character*80 outline
c
c
	call xvmessage('*** tclmath version 2016-06-09 ***',' ')
	call xvpcnt ('INVALS',numinvals)
	call xvp ('INVALS',invals,cnt)
	call xvp ('FUNC',function,cnt)
c	!.or.function.eq.'average') then
c	print *, 'function = ',function
	if (function.eq.'AVERAGE') then			!.or.function.eq.'average') then
		suminval=0
		do i=1,numinvals
			suminval=invals(i)+suminval
		enddo
		outvals(1)=suminval/numinvals
		numoutvals=1
		call putparm (numoutvals,outvals)
	else if (function.eq.'SQROOT') then
		do i=1,numinvals
			outvals(i)=sqrt(invals(i))
		enddo
		numoutvals=numinvals
		call putparm (numoutvals,outvals)
	else if (function.eq.'NATLOG' .or. function.eq.'LN') then
		do i=1,numinvals
			outvals(i)=alog(invals(i))
		enddo
		numoutvals=numinvals
		call putparm (numoutvals,outvals)
	else if (function.eq.'LOG'.or. function.eq.'LOG10') then
		do i=1,numinvals
			outvals(i)=alog10(invals(i))
		enddo
		numoutvals=numinvals
		call putparm (numoutvals,outvals)
	else if (function.eq.'EXP') then
		do i=1,numinvals
			outvals(i)=exp(invals(i))
		enddo
		numoutvals=numinvals
		call putparm (numoutvals,outvals)
	else if (function.eq.'SQUARE') then
		do i=1,numinvals
			outvals(i)=invals(i)*invals(i)
		enddo
		numoutvals=numinvals
		call putparm (numoutvals,outvals)
	else if (function.eq.'CUBE') then
		do i=1,numinvals
			outvals(i)=invals(i)*invals(i)*invals(i)
		enddo
		numoutvals=numinvals
		call putparm (numoutvals,outvals)
	else
		write(outline,10100) function
10100 format('??E - Function ',a12,' not recognized',' ')
		call xvmessage(outline,' ')
		call abend
	endif
	return
	end
c===========================================================================
      subroutine putparm (numoutvals,outvals)
c
      implicit none
      integer*4 numoutvals
      integer*4 parb(1000),xcont,xadd,stat
	real*4 outvals(20)
c
      call xqini (parb,1000,xcont)
      call xqintg (parb,'OUTCOUNT',1,numoutvals,xadd,stat)
      call xqreal (parb,'OUTVALS',20,outvals,xadd,stat)
      call xqout (parb,stat)
      call chkstat (stat,'??E - XQout error',0,0,0)
c
      return
      end

