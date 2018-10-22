      include 'VICMAIN_FOR'
      subroutine main44
C**** VERSION 3 VICAR-2 PARAMS 20 AUGUST 1984 MEM
C     VERSION 2 VICAR-2 IO     13 AUGUST 1984 MEM
C     VERSION 1 VAX CONVERSION 7 AUGUST 1984 MEM
c      INCLUDE 'fortport'
	implicit none
c      COMMON/C1/DN
	character*8 string
	integer*4 outunit,ind,cnt,nl,ns,sl,ss
	integer*4 ix,iy,ishape,ival,count,def
	integer*4 dnmax,sigmax,sigmay,x0,y0
	integer*4 size(4)
	real*4 af,dxmx,eps,fact,r,r2,tmpf,xarg,yarg,yargs
	byte dn(2048)
C
C**** PARAMETER INITIALIZATION
c      nl=1024				!default
c      ns=1024
      sl=1
      ss=1
      dnmax=255
      sigmax=125
      sigmay=125
      x0=512
      y0=512
C**** VICAR*2 OPENS
      call xvmessage('SPOT version 04-Nov-2010',' ')
c      call xvsize(sl,ss,nl,ns,nli,nsi)
	call xvparm ('SIZE',size,count,def,4)
c	print *, 'def = ',def
	if (size(3).eq.0) then
	    call xvp ('NL',nl,cnt)
            call xvp ('NS',ns,cnt)
	else
	    nl=size(3)
	    ns=size(4)
        end if
c	print *, 'nl = ns = ',nl, ns
      call xvunit(outunit,'OUT',1,ind,' ')
      call xvopen(outunit,ind,'U_NL',nl,'U_NS',ns,'OP','WRITE',' ')
C**** NOW PROCESS THE TAE/PDF
C     TO GET THE PARAMETER VALUES
C     USE DEFAULTS IF "CNT"=0
      call xvp('DNMAX',dnmax,cnt)
      call xvp('SIGMAX',sigmax,cnt)
      if(cnt.eq.0)sigmax=ns/8
      call xvp('SIGMAY',sigmay,cnt)
      if(cnt.eq.0)sigmay=nl/8
      call xvp('X0',x0,cnt)
      if(cnt.eq.0)x0=ns/2
      call xvp('Y0',y0,cnt)
      if(cnt.eq.0)y0=nl/2

c
c     bam 7/98 AR-9267
c
c     make sure we don't have a divide by 0
c
      if (dnmax .ne. 0 ) then
          dxmx=255/dnmax
      else
          dxmx = 0 
      end if

	eps=1.E-5
	ishape = 1			!default is GAUSSIAN
	call xvp('SHAPE',string,cnt)
	if(string.eq.'GAUSSIAN') ishape=1
	if(string.eq.'CONICAL') ishape=2
	if(string.eq.'RECPROCL') ishape=3
	if(string.eq.'RECPSQRE') ishape=4
	if(string.eq.'EXPONENT') ishape=5
	if(string.eq.'DOME') ishape=6
	if(string.eq.'DISK') ishape=7


C**** COMPUTE RADIAL DISTANCE

      do 100 iy=1,nl
c
c     bam 7/98 AR-9267
c
c     make sure we don't have a divide by 0
c     for both sigmay and sigmax
c
      if (sigmay .ne. 0 ) then
          yarg=(iy-y0)*1./(sigmay*1.)
      else
          yarg = 0 
      end if

      yargs=yarg**2

      do 99 ix=1,ns
          if (sigmax .ne. 0 ) then
              xarg=(ix-x0)*1./(sigmax*1.)
          else
              xarg = 0 
          end if

      r2=xarg**2+yargs
      
      r=sqrt(r2)
	tmpf = 0
C**** BRANCH TO SHAPE
               GO TO (51,52,53,54,55,56,57),ISHAPE
C     GAUSSIAN SPOT
   51 if (r2.ge.100.) r2=100.
      fact=exp((-0.5)*(r2))
      go to 60
C     CONICAL SPOT
   52 fact=1. - r
      if (fact.lt.0.) fact=0.
      go to 60
C     RECIPROCAL SPOT
   53 if (r.gt.eps) tmpf=1./r
      if ((r.eq.0.).or.(tmpf.ge.dxmx)) tmpf=dxmx
      fact=tmpf
      go to 60
C     RECIPROCAL SQUARED SPOT
   54 if (r2.gt.eps) tmpf=1./r2
      if ((r2.eq.0.).or.(tmpf.gt.dxmx)) tmpf=dxmx
      fact=tmpf
      go to 60
C     EXPONENTIAL SPOT
   55 if (r.ge.100.) r=100.
      fact=exp((-1.)*(r))
      go to 60
C     DOME SPOT
   56 af=1. - r2
      if (af.le.0.) af=0.
      fact=sqrt(af)
	go to 60
C	UNIFORM DISK
   57 af=1. - r
	if (af.le.0) af=0.
	if (af.gt.0) af=1.
	fact=af
C**** CALCULATE OUTPUT VALUE AT SAMPLE LOCATION OF EACH PIXEL
   60 ival = dnmax*fact
c      dn(ix)=int2byte(ival)
	dn(ix) = ival
   99 continue
      call xvwrit(outunit,dn,ind,' ')
  100 continue
	if(ishape.eq.1)call xvmessage('****GAUSSIAN PATTERN GENERATED ',' ')
	if(ishape.eq.2)call xvmessage('****CONICAL PATTERN GENERATED ',' ')
	if(ishape.eq.3)call xvmessage('****RECIPROCAL PATTERN GENERATED ',' ')
	if(ishape.eq.4)call xvmessage('****RECP SQUARED PATTERN GENERATED',' ')
	if(ishape.eq.5)call xvmessage('****EXPONENTIAL PATTERN GENERATED',' ')
	if(ishape.eq.6)call xvmessage('****DOME PATTERN GENERATED ',' ')
	if(ishape.eq.7)call xvmessage('****UNIFORM DISK PATTERN GENERATED ',' ')
      call xvclose(outunit,ind,' ')
      return
      end
