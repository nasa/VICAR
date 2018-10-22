      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44

c EXAMPLE # 1
c Purpose: To test the Metropolis algorithm in three dimensions.
c The example below is to fit a circle to 6 data points, two
c of which do not lie on the circle.
c Notice the cost function rewards for including lots of data
c points and is allowed to reject points which fall far from the
c fitted function.
      real range(3),answer(3),array(500),array2(500),i2,i3,i4,i5,d(500)
      real sum,sres,sumres,serror,terror
      real*8 PI
      data PI/3.141592653589793D0/
      integer i1,npts,nnd
      character*80 msg
      character*255 polartbl
      external cost
      numten=1000
      limit=5000
      iprint=0
      narg=3
      norm=330


c (1)=a (2)=b (3)=theta

      call xvparm('POLARTBL', polartbl, icnt, idef, 1)
c     xvfilename generated error in VMS so took it out

      do 60 ii=1,5
      npts=0
      sum=0.
      sumres=0.
      sres=0.
      serror=0.
      nnd=0.	
      answer(1)=4095.
      answer(2)=0.
      answer(3)=90.
      data range/1000.,1000.,90./

c      open(11,name=polartbl,type='old',err=999)
      open(11,file=polartbl,status='old',err=999)
      read(11,1)
      read(11,1)
1     format(a)
      do 10 I=1,500
	read(11,2,end=50,err=999)I1,I2,I3,I4,I5
2       format(I6,1X,E18.12,1X,E18.12,1X,E18.12,1X,E18.12)
        npts=npts+1
        array(I)=I4
        array2(I)=I2
        if (I4 .lt. answer(1)) then
		answer(1)=I4
        endif
	if (I4 .gt. answer(2)) then
		answer(2)=I4
        endif
10    continue
	answer(2)=answer(2)-answer(1)
50    close(11,err=999)

      call metropolis(cost,narg,array,array2,range,numten,answer,
     +                 limit,norm,npts,iprint,ind)
      if(ind.eq.0)then
        write(msg,*)'guess number:',(ii)
        call xvmessage(msg,' ')
         write(msg,*)'answer=',(answer(j),j=1,narg)
         call xvmessage(msg,' ')

        do j=1,npts
        
	 d(j)=abs(answer(1)+answer(2)*
     &		(cos((array2(j)-answer(3))*PI/180.)**2)  -array(j))
         sres=sres+abs(d(j))
	 sumres=sumres+d(j)*d(j)
        
	enddo

        mean=sres/npts
        sigma=sqrt(sumres/npts - mean*mean)
        write(msg,*)'sigma=',(sigma)
        call xvmessage(msg,' ')   
	do j=1,npts
	   if (d(j) .lt. sigma) then
		nnd=nnd+1
		serror=serror+d(j)/array(j)
	   endif
	enddo
	terror=serror/nnd
        write(msg,*)'error=',(terror)
        call xvmessage(msg,' ')   

      else
         call xvmessage('no solution',' ')
      endif
60    continue
      return

999   call xvmessage('Error reading input file',' ')
      call abend
      end


      subroutine  cost(x,array,array2,n,error,ind)
c Returns the cost function resulting from guessing x (answer).
c This could be any function or logical operation.

c X     is the solution vector.
c ARRAY is an array of N data points if needed.
c ARRAY2 is another array of N elements if needed.
c N      is the number of elements/data points in ARRAY & ARRAY2.
c ERROR is the returned cost.
c IND   is 0 for normal return, 1 for abnormal return.
c        If METROPOLIS senses an indicator of 1 it will generate another
c        guess somewhere else & try again.

c (1)=radius (2)=x_center (3)=y_center
      real array(n),array2(n),x(4),dr(500)

      ind=1
      if(x(1).lt.0.)return
      if(x(1).gt.4095.)return
      if(x(2).gt.4095.)return
      if(x(3).gt.180.)return
      if(x(2).lt.0.)return
      if(x(3).lt.0.)return
      sumdr=0.
      sum=0.
      range=1000
      m=0
      do j=1,n
        dr(j)=abs(x(1)+x(2)*(cos((array2(j)-x(3))*PI/180.)**2)
     &					- array(j))
        sum=sum+dr(j)
        if(dr(j).lt.range)then
           m=m+1
           sumdr=sumdr+dr(j)
        endif
      enddo
      if(m.eq.0)then
         error=sum/n + range/n
      else
         error=sumdr/m + range/m
      endif
      ind=0
      return
      end
