c ********************************************************************
c process photometric function parameters.
c
      subroutine phot_parms
      implicit none

      common/pf0/linc,sinc,grid,term,limb,const,maxcor
      integer linc,sinc,grid
      real term,limb,const,maxcor

      common/pf1/minn,icook,ihapke
      integer minn,icook,ihapke

      common/pf2/k,w,b,h,c,ck,d,xlg2
      real k,w,b,h,c,ck,d(6),xlg2

      common/pf4/data
      real data(7)

      common/hapke_norm/bb1  ! the hapke function at zero phase & i & e
      common/buratti_norm/bur_norm  ! the buratti function at zero phase
      real bb1,bur_norm

      common/cm/cmap,class      !classification map unit number and class
      integer cmap,class

      common/konst/rad
      real rad

      real r,far(10)
      integer i,j,n,def,count

c set some defaults
      bb1 = 0.0		!hapke normalizing factor
      bur_norm=0.0	! "

      d(1) = 1.
      do i=2,6		!veverka-squyres phase angle correction
         d(i) = 0.0	! elements 5&6 are for mosher function
      enddo

      call xvp('LINC',linc,count)		!line spacing of grid
      call xvp('SINC',sinc,count)		!sample spacing of grid
      call xvparm('INCR',n,count,def,' ')
      if (def.eq.0) then
         linc = n
         sinc = n
      endif

      minn = 1		!minnaert function is the default
      call xvparm('MINNAERT',k,count,def,' ')
      if (k.eq.1.0) minn = 3	! lambert function

c  1978 hapke fcn.
      ihapke=0
      call xvparm('HAPKE',far,count,def,' ')
      if (count.eq.4) then
        minn=2
        ihapke=1
	w=far(1)
        b=far(2)
        h=far(3)
        c=far(4)

c  cook'S MODIFICATION TO HAPKES FUNCTION
	call xvparm('COOK',far,count,def,' ')
	if (def.eq.0) then
	  icook=1
	  ck=far(1)
	endif
      elseif (count.eq.5) then
c  1984 hapke fcn., henyey-greenstein phase fcn.
        minn=2
        ihapke=2
	w = far(1)
        h = far(2)
        ck = far(3)	! theta-bar
        c = far(4)	! s0
	b = far(5)	! henyey-greenstein
      elseif (count.eq.6) then
c  1984 hapke fcn., legendre phase fcn.
        minn=2
        ihapke=3
	w = far(1)
        h = far(2)
        ck = far(3)	! theta-bar
        c = far(4)	! s0
	b = far(5)	! first legendre coefficient
	xlg2 = far(6)	! second legendre coef.
      endif

c  squyres-veverka function
      call xvparm('VEVERKA',far,count,def,' ')
      if (def.eq.0) then
	minn = 4
        d(1)=far(1)
        d(2)=far(2)
        d(3)=far(3)
        d(4)=far(4)
      endif

c  mosher modification of veverka function
      call xvparm('MOSHER',far,count,def,' ')
      if(def.eq.0)then
        minn=5
        d(1)=far(1)
        d(2)=far(2)
        d(3)=far(3)
        d(4)=far(4)
        d(5)=far(5)
        d(6)=far(6)
      endif

c  buratti-veverka function
      call xvparm('BURATTI',far,count,def,' ')
      if(def.eq.0)then
        minn=6
        d(1)=far(1)
        d(2)=far(2)
        d(3)=far(3)
        d(4)=far(4)
        d(5)=far(5)
        d(6)=far(6)
      endif

c irvine function
      call xvparm('IRVINE',far,count,def,' ')
      if(def.eq.0)then
         minn=7
         d(1)=far(1)  ! k
         d(2)=far(2)  ! a
         d(3)=far(3)  ! b
      endif

      if (cmap.ne.0) call xvparm( 'CLASS', class, i, j,' ')

c this parameter used to zero areas near terminator with low signal
      term=0.
      call xvparm('TERMINAT',r,count,def,' ')
      if (def.eq.0) term=cos((90.-r)/rad)

c this parameter used to zero areas near the limb with foreshortening
      limb=0.
      call xvparm('LIMB',r,count,def,' ')
      if (def.eq.0) limb=cos((90.-r)/rad)

      call xvp('MULTIPLY',const,count)	!multiply corrected dn by constant
      const=1./const

      call xvp('MAXCOR',maxcor,count)
      maxcor = 1.0/maxcor
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c add label to output image specifying photometric function used
c
      subroutine phot_label(ounit)
      implicit none
      integer ounit

      common/pf0/linc,sinc,grid,term,limb,const,maxcor
      integer linc,sinc,grid
      real term,limb,const,maxcor

      common/pf1/minn,icook,ihapke
      integer minn,icook,ihapke

      common/pf2/k,w,b,h,c,ck,d,xlg2
      real k,w,b,h,c,ck,d(6),xlg2

      character stars(4)/'*','*','*','*'/
      integer ind

      character*36 lablam/
     *' LAMBERT PHOTOMETRIC FUNCTION, COS I'/                             
      character*140  labmos/     
     *' MOSHER PHOTOMETRIC FUNCTION(0.00-0.00000a+0.00e0p-0.00a)(ci**(0.00+0.0000a)(ce**(00.000.0000a)) '/
      character*70 labvev/
     *' VEVERKA PHOTOMETRIC FUNCTION(00000+0000000A+ 0000E0P-0000A)(CI/Ci+ce)'/
      character*62 labmin/
     *' MINNAERT PHOTOMETRIC FUNCTION COSI**000000*(COSE**000000    )'/
      character*84 labbur/
     *' BURATTI PHOTOMETRIC FUNCTION  A=000000 B=000000 C=000000 D=000000 e=000000 f=000000'/
      character*70 labhap/
     *' HAPKE PHOTOMETRIC FUNCTION,W=000000,B= 000000,H=000000,C= 000000     '/
      character*75 labhap1/
     *' HAPKE PHOTOMETRIC FUNCTION,W=000000,H=000000,TB=000000,S0=000000,hg=000000'/
      character*83 labhap2/
     *' HAPKE PHOTOMETRIC FUNCTION,W=000000,H=000000,TB=000000,S0=000000,b=000000,c=000000'/
      character*70 labcok/
     *' COOK PHOTOMETRIC FUNCTION,W=00000,B= 00000,H=00000,C= 00000,CK= 0    '/
      character *56 labirv/
     *' IRVINE Photometric function A=0.00000 B=0.00000 K=0.000'/

      if (grid.gt.0) then	!if grid option is used,
         call grid_labels(grid)
ccc         k=stars		!put asterisks in label
ccc         w=stars		!failed to compile on linux
ccc         b=stars
ccc         h=stars
ccc         c=stars
ccc         ck=stars
         call mve(4,6,stars,k,0,1)!put asterisks in label
      endif 

      if (ounit.eq.0) return		!skip if no output image

      if(minn.eq.1)then      
	   write(labmin(39:43), '(f5.4)') k		! new
	   write(labmin(52:58), '(f7.4)') k-1		! new
           call xladd(ounit,'HISTORY','PHOT',labmin,ind,
     *            'FORMAT','STRING',' ')
           call chkstat(ind,' ERR XLADD,STAT=',1,ind)
           call xvmessage(labmin,' ')
      endif

      if(minn.eq.2.and.icook.ne.1)then
	if (ihapke.eq.1) then
	   write(labhap(31:36), '(f6.3)') w		! new
	   write(labhap(40:46), '(f7.3)') b		! new
	   write(labhap(50:55), '(f6.3)') h		! new
	   write(labhap(59:65), '(f7.3)') c		! new
           call xladd(ounit,'HISTORY','PHOT',labhap(2:),ind,
     *            'FORMAT','STRING',' ')
           call chkstat(ind,' ERR XLADD,STAT=',1,ind)
 	   call xvmessage(labhap,' ')
	elseif (ihapke.eq.2) then
	   write(labhap1(31:36), '(f6.3)') w		! new
	   write(labhap1(40:45), '(f6.3)') h		! new
	   write(labhap1(50:55), '(f6.2)') ck		! new
	   write(labhap1(60:65), '(f6.3)') c		! new
	   write(labhap1(65:70), '(f6.3)') b		! new
           call xladd(ounit,'HISTORY','PHOT',labhap1(2:),ind,
     *            'FORMAT','STRING',' ')
           call chkstat(ind,' ERR XLADD,STAT=',1,ind)
 	   call xvmessage(labhap1,' ')
	elseif (ihapke.eq.3) then
	   write(labhap2(31:36), '(f6.3)') w		! new
	   write(labhap2(40:45), '(f6.3)') h		! new
	   write(labhap2(50:55), '(f6.2)') ck		! new
	   write(labhap2(60:65), '(f6.3)') c		! new
	   write(labhap2(69:74), '(f6.3)') b		! new
	   write(labhap2(78:83), '(f6.3)') xlg2		! new
           call xladd(ounit,'HISTORY','PHOT',labhap2(2:),ind,
     *            'FORMAT','STRING',' ')
           call chkstat(ind,' ERR XLADD,STAT=',1,ind)
 	   call xvmessage(labhap2,' ')
	endif
      endif

      if(minn.eq.2.and.icook.eq.1)then
	   write(labcok(30:34), '(f5.3)') w		! new
	   write(labcok(38:43), '(f6.3)') b		! new
	   write(labcok(47:51), '(f5.3)') h		! new
	   write(labcok(55:60), '(f6.3)') c		! new
	   write(labcok(64:69), '(f6.3)') ck		! new
           call xladd(ounit,'HISTORY','PHOT',labcok(2:),ind,
     *            'FORMAT','STRING',' ')
           call chkstat(ind,' ERR XLADD,STAT=',1,ind)
           call xvmessage(labcok,' ')
      endif         

      if(minn.eq.3)then
           call xladd(ounit,'HISTORY','PHOT',lablam(2:),ind,
     *            'FORMAT','STRING',' ')
           call chkstat(ind,' ERR XLADD,STAT=',1,ind)
	   call xvmessage(lablam,' ')
      endif      

      if(minn.eq.4)then
	   write(labvev(31:35), '(f5.3)') d(1)		! new
	   write(labvev(37:43), '(f7.4)') d(2)		! new
	   write(labvev(47:50), '(f4.3)') d(3)		! new
	   write(labvev(56:58), '(f3.2)') d(4)		! new
           call xladd(ounit,'HISTORY','PHOT',labvev(2:),ind,
     *            'FORMAT','STRING',' ')
           call chkstat(ind,' ERR XLADD,STAT=',1,ind)
      	   call xvmessage(labvev,' ')
      endif      

      if(minn.eq.5) then   
	   write(labmos(30:33), '(f4.2)') d(1)		! new
	   write(labmos(35:51), '(f7.4)') -d(2)		! new
	   write(labmos(44:47), '(f4.2)') d(3)		! new
	   write(labmos(51:56), '(f5.2)') d(4)		! new
	   write(labmos(64:67), '(f4.2)') d(5)	! new
	   write(labmos(69:74), '(f6.4)') d(6)	! new
	   write(labmos(83:87), '(f5.2)') d(5)-1	! new
	   write(labmos(89:94), '(f6.4)') d(6)	! new
           call xladd(ounit,'HISTORY','PHOT',labmos(2:),ind,
     *            'FORMAT','STRING',' ')
           call chkstat(ind,' ERR XLADD,STAT=',1,ind)
      	   call xvmessage(labmos,' ')
      endif

      if(minn.eq.6) then   
	   write(labbur(34:39), '(f6.3)') d(1)		! new
	   write(labbur(43:48), '(f6.3)') d(2)		! new
	   write(labbur(52:57), '(f6.4)') d(3)		! new
	   write(labbur(61:66), '(f6.3)') d(4)		! new
	   write(labbur(69:75), '(f6.3)') d(5)		! new
	   write(labbur(79:84), '(f6.3)') d(6)		! new
           call xladd(ounit,'HISTORY','PHOT',labbur(2:),ind,
     *            'FORMAT','STRING',' ')
           call chkstat(ind,' ERR XLADD,STAT=',1,ind)
      	   call xvmessage(labbur,' ')
      endif

      if(minn.eq.7)then
	   write(labirv(52:56), '(f5.2)') d(1)		! new
	   write(labirv(32:38), '(f7.5)') d(2)		! new
	   write(labirv(42:48), '(f7.5)') d(3)		! new
           call xladd(ounit,'HISTORY','PHOT',labirv(2:),ind,
     *            'FORMAT','STRING',' ')
           call chkstat(ind,' ERR XLADD,STAT=',1,ind)
           call xvmessage(labirv,' ')
      endif

      return
      end
