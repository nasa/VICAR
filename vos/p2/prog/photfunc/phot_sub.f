ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Compute normalized brightness correction for a given lat,lon of an image
c
      subroutine phot_sub(v,limb,term,bbrite)
      implicit none
      real*8 v(3)		!vector from target-center to surface point
      real limb,term		!limb and terminator thresholds
      real bbrite		!output brightness correction

      common/pf1/minn,icook,ihapke
      integer minn,icook,ihapke

      common/pf2/k,w,b,h,c,ck,d,xlg2
      real k,w,b,h,c,ck,d(6),xlg2

      common/hapke_norm/bb1             !hapke function at zero phase & i & e
      common/buratti_norm/bur_norm      !buratti function at zero phase
      real bb1,bur_norm

      real bb0,phase,result
      real data1(5)/0.,1.,1.,1.,0./     !normal angles for hapke

      common/konst/rad
      real rad

      common/pf4/refl,ci,ce,cg,tg
      real refl,ci,ce,cg,tg,data(5)
      equivalence (data,refl)

      real fhapke
      external fhapke

c  statement function for several photometric functions:
      real pac,a,dd
      pac(a,b,c,dd,phase)=a+b*phase+c*exp(-dd*phase)/(a+c)

c compute tg,cg,ci,ce
      call light_angles(v,tg,cg,ci,ce)
      bbrite = 0.0
      if (ci.le.0.0.or.ce.le.0.0) goto 580
      if (ci.le.term.or.ce.le.limb) goto 580

c             1   2   3   4   5   6   7
      go to (560,565,570,571,572,573,574),minn

c  minnaert function:
  560 if(ce.ne.0.0)then 
        bbrite = ((ci*ce)**k)/ce	!(ci**k)*ce**(k-1.)
      else
        bbrite=0.0
      endif
      go to 580

c  hapke functions:
  565 bb0 = fhapke(rad,data,w,b,h,ck,c,xlg2,icook,ihapke)
      if (bb1.eq.0.0) then	!compute normalizing factor
        bb1 = fhapke(rad,data1,w,b,h,ck,c,xlg2,icook,ihapke)
      endif
      bbrite = bb0/bb1
      go to 580

c  lambert function:
  570 bbrite = ci
      go to 580

c  veverka function:
  571 phase=abs(acos(cg))*rad
      bbrite = pac(d(1),d(2),d(3),d(4),phase)*2.*ci/(ce+ci)
      go to 580

c  mosher function:
  572 phase=abs(acos(cg))*rad
      bbrite = pac(d(1),d(2),d(3),d(4),phase) * 
     & (ci**(d(5)+d(6)*phase)*(ce**(d(5)+d(6)*phase-1.)))
      go to 580

c  buratti function
c  note the term: 2/(1+d(1)) is the ci=ce=0 normalizing term.
  573 phase=abs(acos(cg))*rad
      call buratti(phase,ci,ce,rad,d,result)
      bbrite=result
      goto 580

c irvine function
574   continue
      bbrite=(((ci*ce)**d(1))/ce)*(1.0-exp(-ci/d(2)))/
     + (1.0-exp(-ce/d(3)))
      goto 580

c  put other functions in here
c  ...

  580 continue
      return
      end

c *******************************************************************
      function fhapke(rad,data,w,b,h,ck,c,xlg2,icook,ihapke)

c  ihapke = 1:  old (1978) hapke fcn.
c               icook=1:  cook modification of above
c  ihapke = 2:  new (1984) hapke fcn. with henyey-greenstein phase fcn.
c  ihapke = 3:  new (1984) hapke fcn. with legendre polynomial phase fcn.

c     w=single scattering albedo 
c     h=porosity (shadowing, angular spread of internally reflected light,
c       larger h = larger width of opposition peak)
c     b,c,ck,xlg2 determine particle phase function and opposition term
c       amplitude in varying ways.

c  data(1)=reflectance
c  data(2)=cos(incidence)
c  data(3)=cos(emission)
c  data(4)=cos(phase)
c  data(5)=tan(phase)

      real*4 data(5),rad,mu

c  statement function definitions:
      hh(w,mu)=(1.+2.*mu)/(1.+2.*mu*sqrt(1.-w))
      cook(a,b)=sqrt(1.0-a*a*(1.0-b*b))

      if (ihapke.ge.2) then
	xi = acos(data(2))*rad
	xe = acos(data(3))*rad
	xp = acos(data(4))*rad
C To call hapke with 8 params; set 1st arg to 8 and put a value in for xlg2
C it simply does not get used int the function
	if (ihapke.eq.2) then
		xlg2 = -1.0
		fhapke = hapke(8,xi,xe,xp,w,h,ck,c,b,xlg2)
	endif
	if (ihapke.eq.3) fhapke = hapke(9,xi,xe,xp,w,h,ck,c,b,xlg2)
	return
      endif

      ci=data(2)
      ce=data(3)
      if(icook.eq.0)go to 20
      ci=cook(ck,ci)
      ce=cook(ck,ce)
   20 continue
      fhapke=data(1)
      x=data(2)+data(3)
      if(x.eq.0.0) return
      x1=data(2)*w/x
      bb=0.0
      if(data(5).le.0.0) go to 10
      x2=exp(-w*w/2.)
      x3=exp(-h/data(5))
      bb=x2*(1.0-(data(5)/(2.*h))*(3.-x3)*(1.-x3))
10    x=data(2)
      hi=hh(w,x)
      x=data(3)
      he=hh(w,x)
      p=1.0+b*data(4)+c*(3.*data(4)*data(4)-1.)/2.
      fhapke=x1*((1.0+bb)*p+hi*he-1.0)
      return
      end

c ********************************************************************
	real function hapke(nargs,i,e,g,w,h,tbar,opmag,aa,bb)
c ------------------------------------------------------------------ 
c fcn.hapke calculates rough-surface reflectance using a new version
c           of hapke'S PHOTOMETRIC FUNCTION.
c this version is a simplified version of that used by paul helfenstein
c in his program 'GENERAL_HAPKE'.   the simplification lies in the
c scattering phase function, which can here only be a 1-parameter
c henyey-greenstein function or a 2-parameter legendre polynomial.
c
c inputs:
C      nargs = number of arguments to use: 8 or 9
c      i = incidence angle in degrees
c      e = emergence angle in degrees
c      g = phase angle in degrees
c      w = single scattering coefficient
c      h = backscatter parameter related to soil porosity
c   tbar = average macaroscopic slope angle (in degrees)
c  opmag = s(0) term in oppostion magnitude coefficient, b0
c  aa,bb = constants in the phase function.
c ** note **: if only aa is specified, then the henyey-greenstein phase
c             function is used.  if both aa and bb are specified, then
c             a two-parameter legendre polynomial is used.
c ------------------------------------------------------------------ 
c
	real mu0,mu01,mu02,mu1,mu11,mu12,i,i0
c
c------> define statement function for hapke'S H-FUNCTION <-------------
c
	hfnctn(x0,x1)=(1.0+2.0*x0)/(1.0+2.0*x0*sqrt(1.0-x1))
c
c       note: x0 = cos(i) or cos(e) and x1 = w
c
c  phase function:
        bb1 = -999.9			! henyey-greenstein
        if (nargs .eq. 9) bb1 = bb	! legendre polynomial
C
C I think nargs() does not exist so passed in nargs above - dpp
C        if (nargs().eq.9) bb1 = bb	! legendre polynomial
c
c------- determine principal variable values -------------------------
c
	pi = 3.141592654
	raddeg = pi/180.
	i0 = i*raddeg
	e0 = e*raddeg
	g0 = g*raddeg
	thebar = tbar*raddeg
c
	cosi = cos(i0)
	cose = cos(e0)
	cosg = cos(g0)
	sini = sin(i0)
	sine = sin(e0)
	sing = sin(g0)
c
c  determine the azimuthal angle psi and related quantities:
	if (i0.eq.0.0 .or. e0.eq.0.0) then
	  cospsi=1.0
	else
	  cospsi = (cosg-cosi*cose)/(sini*sine)
	endif
	if (cospsi.ge.1.0) cospsi=1.0
	if (cospsi.le.-1.0) cospsi=-1.0
	psi = acos(cospsi)
	sinps2 = sin(0.5*psi)
	sinps2 = sinps2*sinps2
	if (abs(psi).gt.3.10) then
	  f = 0.0
	else
	  f = tan(0.5*psi)
	  f = exp(-2.0*f)
	endif
c
c  determine the backscatter function b(g) 
c
	tstmag = w*sppf(0.0,aa,bb1)
	bg = bsf(w,h,opmag,tstmag,g0)
c
c  determine p(g): average single particle scattering function
c
	pg = sppf(g0,aa,bb1)
c
c---- values for delta variables: first non-case dependant terms---
c
c  if average macroscopic slopes=0, then all delta'S=0 
	if (thebar.gt.0.0) go to 105
	mu0  = cosi
	mu01 = cosi
	mu02 = cosi
	mu1  = cose
	mu11 = cose
	mu12 = cose
	y = 1.0
	go to 500
c
  105	if (thebar.gt.1.4) thebar = 1.4
	tant = tan(thebar)
	cott = 1.0/tant
	beta = sqrt(1.0 + pi*tant*tant)
c
c------- exponential terms first --------------------------
c
	if (abs(sine).lt.1.e-04) then
	  ee1 = 0.0
	  ee2 = 0.0
	else
	  cote = cose/sine
	  ee2 = -2.0*cott*cote/pi
	  ee1 = -0.25*pi*ee2*ee2
	  if(ee1.gt.(87.0))ee1=87.0
	  if(ee2.gt.(87.0))ee2=87.0
	  ee1 = exp(ee1)
	  ee2 = exp(ee2)
	  if(ee1.gt.(1.0e+20))ee1=1.0e+20
	  if(ee2.gt.(1.0e+20))ee2=1.0e+20
	endif
c
	if (abs(sini).lt.1.e-04) then
	  ei1 = 0.0
	  ei2 = 0.0
	else
	  coti = cosi/sini
	  ei2 = -2.0*cott*coti/pi
	  ei1 = -0.25*pi*ei2*ei2
	  if(ei1.gt.(87.0))ei1 = 87.0
	  if(ei2.gt.(87.0))ei2 = 87.0
	  ei1 = exp(ei1)
	  ei2 = exp(ei2)
	  if(ei1.gt.(1.0e+20))ei1=1.0e+20
	  if(ei2.gt.(1.0e+20))ei2=1.0e+20
	endif
c
	mu0 = cosi
	mu1 = cose
	mu02 = (cosi + sini*tant*(ei1/(2.0-ei2)))/beta
	mu12 = (cose + sine*tant*(ee1/(2.0-ee2)))/beta
c
c------------------------------------------------------------------
c---- now, determine which case is appropriate for the model ------
c----      case #1: i<=e      case #2: i>=e                  ------
c------------------------------------------------------------------
	if (i.le.e) then
	  xi2 = 2.0 - ee2 - (psi/pi)*ei2
	  xi1 = (cospsi*ee1 + sinps2*ei1)
	  mu01 = (cosi + sini*tant*(xi1/xi2))/beta
	  xi3 = (ee1 - sinps2*ei1)
	  mu11 = (cose + sine*tant*(xi3/xi2))/beta
	  y = beta - f*beta + f*(mu0/mu02)
	else
	  xi2 = 2.0 - ei2 - (psi/pi)*ee2
	  xi1 = (ei1 - sinps2*ee1)
	  mu01 = (cosi + sini*tant*(xi1/xi2))/beta
	  xi3 = (cospsi*ei1 + sinps2*ee1)
	  mu11 = (cose + sine*tant*(xi3/xi2))/beta
	  y = beta - f*beta + f*(mu1/mu12)
	endif
c
c--------- now, calculate the complete photometric reflectance ------
c
  500   if (w.ge.0.99998) w=0.99998
	hapcof = (0.25*w)*(mu01/(mu01+mu11))
	sfnct = (mu0/mu02)*(mu11/mu12)/y
	bgfnct = bg
	pgfnct = pg
	hfncti = hfnctn(mu01,w)
	hfncte = hfnctn(mu11,w)
c
	hapke = hapcof*((1.0+bgfnct)*pgfnct-1.0+hfncti*hfncte)*sfnct
c
	return
	end

c-------------------------------------------------------------------------
c-- function bsf:  computes the regolith backscatter function b(g)     ---
c--                                                                    ---
c-- inputs:  w:       single-scattering albedo                         ---
c--          h:       particle compaction parameter                    ---
c--          opmag:   s0 parameter of hapke'S NEW B(G)                 ---
c--          tstmag:  particle phase function                          ---
c--          g0:      phase angle                                      ---
c-------------------------------------------------------------------------
c
c ***********************************************************************
        function bsf(w,h,opmag,tstmag,g0)

	bsf = 0.0
        if (h.le.0.0) return

	if (tstmag.lt.0.000001) tstmag=0.000001
	b0 = opmag/tstmag
	if (abs(g0).le.0.0001) then
	  bsf = b0*h/(h+abs(0.5*g0))
	else
	  bsf = b0*h/(h+tan(abs(0.5*g0)))
	endif

        return
	end
c---------------------------------------------------------------------
c--- sppf - this routine computes the average single-particle phase function
c   using either a one-parameter henyey-greenstein function or a two-
c   parameter legendre polynomial.
c---------------------------------------------------------------------
c--- input parameters:                                         -
c---  phangl   phase angle in radians.
c---  pt1,pt2  phase function parameters
c---------------------------------------------------------------------
c
c ******************************************************************
	function sppf(phangl,pt1,pt2)
c
	pi = 3.141592654
c
        if (pt2.le.-999.) then		! henyey-greenstein
	  g1 = pt1
	  x = cos(pi-phangl)
	  bottom = 1.5*log(1.0+g1*g1-2.0*g1*x)
	  bottom = exp(bottom)
	  sppf = (1.0-g1*g1)/bottom

	else				! legendre polynomial
	  g1 = cos(phangl)
	  sppf = 1.0+pt1*g1+0.5*pt2*(3.*g1*g1-1.)

	endif
c
	return
	end

c **************************************************************
      subroutine buratti(phase,ci,ce,rad,d,result)
      real*4 d(6)      
      common/buratti_norm/bur_norm  ! the buratti function at zero phase

      pi=3.141592654
      if(bur_norm.eq.0.0)then
         bur_norm=(d(2)+d(4)-1.0)*(1.0-d(1))*(2.0/3.0) +
     +             d(1)*(d(2)+d(4))*d(6) + (1.0-d(1))
      endif
      bur1=d(2)+d(3)*phase+d(4)*exp(-d(5)*phase) ! surface phase fcn
      ph = phase/rad		!convert to radians

      if (phase.ne.0.0) then
         bur2=(d(1)*pi/2.0)*(1.0-sin(ph/2.0)*tan(ph/2.0)*
     +      log(1.0/tan(ph/4.0)))
      else
         bur2=d(1)*pi/2.0
      endif

      bur3=(2.0/3.0)*(1.0-d(1))*(sin(ph)+(pi-ph)*cos(ph))
      bur4=bur1*pi*((2.0/3.0)*(1.0-d(1))+d(1)*d(6))
      burf=(bur4-bur3)/bur2
      bur=d(1)*(ci/(ce+ci))*burf+(1.0-d(1))*ci
      result=bur/bur_norm
      return
      end
