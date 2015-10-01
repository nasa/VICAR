$!****************************************************************************
$!
$! Build proc for MIPL module tricoef
$! VPACK Version 1.9, Monday, January 28, 2002, 15:06:43
$!
$! Execute by entering:		$ @tricoef
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   PDF         Only the PDF file is created.
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!
$!   The default is to use the SYS parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module tricoef ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Test = ""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = "SYS"
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to tricoef.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("tricoef.imake") .nes. ""
$   then
$      vimake tricoef
$      purge tricoef.bld
$   else
$      if F$SEARCH("tricoef.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake tricoef
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @tricoef.bld "STD"
$   else
$      @tricoef.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create tricoef.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack tricoef.com -u -mixed -
	-s tricoef.f tztriaxtran.c ztriaxtran.c -
	-i tricoef.imake -
	-p tricoef.pdf -
	-t tsttricoef.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create tricoef.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c
c program tricoef
c
      include 'VICMAIN_FOR'
      subroutine main44

      implicit real*8 (a-h,o-z)
      parameter (maxpts=800) ! Changes must be made in other places too !
      parameter (maxeqs=400) ! maxeqs < maxpts
      parameter (latpts=72,lonpts=72)  ! Changes in other places too
      parameter (ibis_column_length=1024)
      character*80 msg
      character*12 planet
      character*80 archive_filename
      logical xvptst,test_triaxtran
      integer*4 count,def,idum,ix1,ix2,ix3
      real*4 planet_data(20),ran1,r(97)
c     real*4 rannum
      real*8 radii(3),lati,long
      real*8 lat(maxpts),lon(maxpts)
      real*8 costheta(maxpts),sintheta(maxpts),f(maxpts)
      real*8 amatrix(maxeqs*maxeqs),bvector(maxeqs)
      real*8 authcoef1(maxpts),authcoef2(maxpts)


c ibis variables
      logical archive_exists
      integer*4 unit,status,nrow,ncol,icol,ibis_out
      real*8 outbuffer(ibis_column_length)

      common/c1/a,b,c,lat,lon,costheta,sintheta,f

c initialize spice for planet radii retrieval.
      call xvmessage('Tricoef version 09-11-98',' ')
      call init_spice

c parameters
      planet='            '
      a=0.d0
      call xvparm('ARCHIVE',archive_filename,count,def,1)
      call xvparmd('RADIUS',radii,count,def,3)
      if(count.gt.0)then
        a=radii(1)
        b=radii(2)
        c=radii(3)
      endif
      call xvparm('PLANET',planet,count,def,1)
      if(count.eq.1)then
        call ccase(planet,1,12)
        call getplacon(planet,id,planet_data,ind)
        if(ind.ne.0)then
           call xvmessage('GETPLACON: unrecognizable planet',' ')
           call xvmessage('Input name is:'//planet,' ')
           if(a.eq.0.d0) call abend
        endif
        call xvmessage('Name will be placed in coefficnt file',' ')
        a=planet_data(1)
        b=planet_data(2)
        c=planet_data(3)
      endif
      call xvparmd('RADIUS',radii,count,def,3)
      if(count.gt.0)then
        a=radii(1)
        b=radii(2)
        c=radii(3)
      endif
      call xvparm('MLIMIT',mlimit,count,def,1)
      call xvparm('KLIMIT',klimit,count,def,1)
      call xvparm('NLIMIT',nlimit,count,def,1)
      test_triaxtran=xvptst('TEST')

      write(msg,108)a,b,c
108   format('Planet radii a,b,c:',3f10.4)
      call xvmessage(msg,' ')

      if(a.eq.0.d0)then
         call xvmessage('Planet radii unspecified',' ')
         call abend
      endif
      if((c.ge.b).or.(b.ge.a))then
         call xvmessage('Radii out of order, should be decreasing.',' ')
         call xvmessage('Oblate spheroids not supported.',' ')
         call abend
      endif
      c=c/a
      b=b/a
      a=1.d0

      nequations=nlimit*mlimit*2
      if(nequations.gt.maxeqs)then
         call xvmessage('Too many equations requested',' ')
         write(msg,100) maxeqs
100      format(' Maximum number of equations = ',i5)
         call xvmessage(msg,' ')
         call abend
      else
         write(msg,110) nequations
110      format(i4,' simultaneous equations.')
         call xvmessage(msg,' ')
      endif

      npoints=nequations*2           ! npoints > nequations/2 (Snyder).
      if(npoints.gt.maxpts)then
         npoints=maxpts
         call xvmessage('Warning: #points must be > #equations/2',' ')
         write(msg,102) nequations
102      format(' Number of equations = ',i5)
         call xvmessage(msg,' ')
      endif
      write(msg,111) npoints
111   format(i4,' random points selected to fit equations.')
      call xvmessage(msg,' ')

c compute the fit points from a random grid in the range lat=0 to 85
c long=0 to 90.
c      call time(kbuf)         ! get time of day
c      call get_seconds(kbuf(2))
c      idum=-kbuf(2)           ! establish a seed for random numbers
      call get_seconds(idum)
      write(msg,112) idum
112   format(' The random number seed = ',i10)
      call xvmessage(msg,' ')
      degtorad=datan(1.0d0)/45.d0
      idum = -idum ! initializes ran1
      do i=1,npoints
         lat(i)=degtorad*85.*ran1(idum,ix1,ix2,ix3,r)
         lon(i)=degtorad*90.*ran1(idum,ix1,ix2,ix3,r)
c         call rangen(idum,rannum)
c         lat(i)=degtorad*85.*rannum
c         call rangen(idum,rannum)
c         lon(i)=degtorad*90.*rannum
      enddo

      call xvmessage('Beginning to compute matrix elements.',' ')

c compute & save values unique to each point.
      do i=1,npoints
         flon=(dsin(lon(i)))**2+((b*b)/(a*a))*(dcos(lon(i)))**2    ! eqn 9
         flat=(dsin(lat(i)))**2+((c*c)/(b*b))*(dcos(lat(i)))**2    ! eqn 10
         t1=(1.d0-(b*b)/(a*a))*dsin(lon(i))*dcos(lon(i))*dsin(lat(i))
         t2=(dsin(lat(i)))**2+(c**4/b**4)*flon*(dcos(lat(i)))**2
         t3=(dsin(lon(i)))**2+(b**4/a**4)*(dcos(lon(i)))**2
         costheta(i)=t1/(dsqrt(t2)*dsqrt(t3))                      ! eqn 48
         sintheta(i)=dsin(dacos(costheta(i)))
         f(i)=dcos(lat(i))*dsqrt(t3)*flat/(flon*dsqrt(t2))         ! eqn 50
      enddo

c load matrices to do the linear fit.                              ! eqn 80
      ip=1
      iq=0
      do i=1,mlimit*nlimit       ! first 1/2 of equations loop

         j=i
         m=1
         n=0
         do k=1,mlimit*nlimit    ! first 1/2 of terms in equation
            t=0.d0
            do ii=1,npoints
               t=t+t_p1(m,n,ii)*t_p1(ip,iq,ii)+
     &			t_p3(m,n,ii)*t_p3(ip,iq,ii)
            enddo
            if(n.eq.nlimit-1)then
               n=0
               m=m+1
            else
               n=n+1
            endif
            amatrix(j)=t
            j=j+nequations
         enddo

         m=0
         n=1
         do k=mlimit*nlimit+1,nequations         ! second 1/2 of terms 
            t=0.d0
            do ii=1,npoints
               t=t+t_p2(m,n,ii)*t_p1(ip,iq,ii)+
     &			t_p4(m,n,ii)*t_p3(ip,iq,ii)
            enddo
            if(n.eq.nlimit)then
               n=1
               m=m+1
            else
               n=n+1
            endif
            amatrix(j)=t
            j=j+nequations
         enddo

         t=0.d0
         do ii=1,npoints
            t=t+(f(ii)/dcos(lat(ii))-sintheta(ii))*t_p1(ip,iq,ii)+
     +        (1.d0-f(ii)*sintheta(ii)/dcos(lat(ii)))*t_p3(ip,iq,ii)
         enddo
         bvector(i)=t
         if(iq.eq.nlimit-1)then
            iq=0
            ip=ip+1
         else
            iq=iq+1
         endif

      enddo

c      ip=1
c      iq=0
      ip=0
      iq=1
      do i=mlimit*nlimit+1,nequations       ! second 1/2 of equations loop

         j=i
         m=1
         n=0
         do k=1,mlimit*nlimit    ! first 1/2 of terms in equation
            t=0.d0
            do ii=1,npoints
               t=t+t_p1(m,n,ii)*t_p2(ip,iq,ii)+
     &			t_p3(m,n,ii)*t_p4(ip,iq,ii)
            enddo
            if(n.eq.nlimit-1)then
               n=0
               m=m+1
            else
               n=n+1
            endif
            amatrix(j)=t
            j=j+nequations
         enddo

         m=0
         n=1
         do k=mlimit*nlimit+1,nequations         ! second 1/2 of terms 
            t=0.d0
            do ii=1,npoints
               t=t+t_p2(m,n,ii)*t_p2(ip,iq,ii)+
     &			t_p4(m,n,ii)*t_p4(ip,iq,ii)
            enddo
            if(n.eq.nlimit)then
               n=1
               m=m+1
            else
               n=n+1
            endif
            amatrix(j)=t
            j=j+nequations
         enddo

         t=0.d0
         do ii=1,npoints
c            if(i.eq.5)write(*,*)ip,iq,t_p2(ip,iq,ii),t_p4(ip,iq,ii)
            t=t+(f(ii)/dcos(lat(ii))-sintheta(ii))*t_p2(ip,iq,ii)+
     +        (1.d0-f(ii)*sintheta(ii)/dcos(lat(ii)))*t_p4(ip,iq,ii)
         enddo
         bvector(i)=t
c         if(iq.eq.nlimit-1)then
c            iq=0
         if(iq.eq.nlimit)then
            iq=1
            ip=ip+1
         else
            iq=iq+1
         endif

      enddo

c perform the solution.
      call xvmessage('Beginning matrix solution.',' ')
      call dsimq(amatrix,bvector,nequations,ks)
      if(ks.ne.0)then
         call xvmessage('Ill conditioned equations, no solution.',' ')
         return
      endif

c print terms
      call xvmessage('CONFORMAL CASE:',' ')
      call xvmessage('C(m,n) coefficients. Rows m=1,2,3,...',' ')
      call xvmessage('Columns begin at n=0,1,2,3...',' ')
      m=0
      do i=1,mlimit*nlimit,nlimit
         m=m+1
         do j=i,i+nlimit-1,5
            l=j+4
            if(l.gt.i+nlimit-1) l=i+nlimit-1
            write(msg,105)(bvector(k),k=j,l)
105         format(5f16.11)
            call xvmessage(msg,' ')
         enddo
      enddo
      call xvmessage('C(m,n)primed coefficients. Rows m=0,1,2,...',' ')
      call xvmessage('Columns begin at n=1,2,3...',' ')
      m=-1
      do i=mlimit*nlimit+1,nequations,nlimit
         m=m+1
         do j=i,i+nlimit-1,5
            l=j+4
            if(l.gt.i+nlimit-1) l=i+nlimit-1
            write(msg,105)(bvector(k),k=j,l)
            call xvmessage(msg,' ')
         enddo
      enddo

c Recompute NPOINTS random points for RMS error computation.
c The same points won't do since we've fitted to them.
      do i=1,npoints
         lat(i)=degtorad*85.*ran1(idum,ix1,ix2,ix3,r)
         lon(i)=degtorad*90.*ran1(idum,ix1,ix2,ix3,r)
c         call rangen(idum,rannum)
c         lat(i)=degtorad*85.*rannum
c         call rangen(idum,rannum)
c         lon(i)=degtorad*90.*rannum
      enddo
      do i=1,npoints
         flon=(dsin(lon(i)))**2+((b*b)/(a*a))*(dcos(lon(i)))**2    ! eqn 9
         flat=(dsin(lat(i)))**2+((c*c)/(b*b))*(dcos(lat(i)))**2    ! eqn 10
         t1=(1.d0-(b*b)/(a*a))*dsin(lon(i))*dcos(lon(i))*dsin(lat(i))
         t2=(dsin(lat(i)))**2+(c**4/b**4)*flon*(dcos(lat(i)))**2
         t3=(dsin(lon(i)))**2+(b**4/a**4)*(dcos(lon(i)))**2
         costheta(i)=t1/(dsqrt(t2)*dsqrt(t3))                      ! eqn 48
         sintheta(i)=dsin(dacos(costheta(i)))
         f(i)=dcos(lat(i))*dsqrt(t3)*flat/(flon*dsqrt(t2))         ! eqn 50
      enddo

c compute rms error for the coefficient set.
      call xvmessage('    ',' ')
      call xvmessage('The RMS error is the constraint violation',' ')
      call conformal_rmserror(bvector,bvector(mlimit*nlimit+1),
     +              nlimit,mlimit,npoints,rms)
      write(msg,106)rms
106   format('Conformal Rms error using these coefficients is ',f15.11)
      call xvmessage(msg,' ')

c convert a grid of lat,lon to see the effect.
      call xvmessage('    ',' ')
      call xvmessage('Snyder lat,lon            Conformal lat,lon' //
     +'                   Mercator x,y',' ')
      do i=0,90,30
         lati=i
         if(i.eq.90) lati=80.d0
         do j=0,90,30
            long=j
            call conftran(bvector,bvector(mlimit*nlimit+1),nlimit,
     +                    mlimit,lati,long,clat,clon,1,ind)
            xp=clon*degtorad
            yp=dlog(dtan((45.d0+clat/2.d0)*degtorad))
            write(msg,103)lati,long,clat,clon,xp,yp
103         format(2f10.3,2f15.8,2f15.8)
            call xvmessage(msg,' ')
         enddo
      enddo

c compute authalic coefficients: authcoef1 & 2
      call authalic_coef(a,b,c,mlimit,klimit,nlimit,
     +   amatrix(1),                          ! areafun scr space
     +   amatrix((latpts+1)*(lonpts+1)+1),    ! autlon   "
     +   amatrix((latpts+1)*(lonpts+1)*2+1),  ! acoef    "
     +   authcoef1,authcoef2,radsph)

c print terms
      call xvmessage('   ',' ')
      call xvmessage('AUTHALIC CASE:',' ')
      call xvmessage('COEF(m,k) coefficients. Rows m=0,1,2...',' ')
      call xvmessage('Columns begin at k=0,1,2,...',' ')
      do k=1,(klimit+1)*(mlimit+1),mlimit+1
        do i=k,k+mlimit,5
          ii=i+4
          if(ii.gt.k+mlimit) ii=k+mlimit
          write (msg,105)(authcoef1(j),j=i,ii)
          call xvmessage(msg,' ')
        enddo
      enddo
      call xvmessage('COEFP(n) coefficient array. n=0,1,2...',' ')
      do i=1,nlimit,5
        ii=i+4
        if(ii.gt.nlimit) ii=nlimit
        write (msg,105)(authcoef2(j),j=i,ii)
        call xvmessage(msg,' ')
      enddo
      write(msg,117)radsph
117   format('Radius of equivalent area sphere is ',f16.8)
      call xvmessage(msg,' ')

c compute rms error for the coefficient set.
      call xvmessage('    ',' ')
      call xvmessage('The RMS error is the constraint violation',' ')
      call authalic_rmserror(authcoef1,authcoef2,nlimit,klimit,mlimit,
     +              radsph,degtorad,a,b,c,rms)
      write(msg,116)rms
116   format('Authalic Rms error using these coefficients is ',f15.11)
      call xvmessage(msg,' ')

c convert a grid of lat,lon to see the effect.
      call xvmessage('    ',' ')
      call xvmessage('Snyder lat,lon            Authalic  lat,lon'//
     +'                   Cylindrical x,y',' ')
      do i=0,90,30
         lati=i
         if(i.eq.90) lati=80.d0
         do j=0,90,30
            long=j
            call authtran(authcoef1,authcoef2,nlimit,klimit,
     +                    mlimit,lati,long,alat,alon,1,ind)
            xp=alon*radsph*degtorad
            yp=radsph*dsin(alat*degtorad)
            write(msg,103)lati,long,alat,alon,xp,yp
            call xvmessage(msg,' ')
         enddo
      enddo

c testing of triaxtran subroutine for special points for centric & detic
      if(test_triaxtran)then
       call xvmessage('   ',' ')
       call xvmessage('Test of triaxtran subroutine.',' ')
       call xvmessage('Test of code exceptions.',' ')
       call xvmessage('Ordering of the data is:',' ')
       call xvmessage('in:lat,lon,fmt out:lat,lon,fmt status',' ')
       call xvmessage('out:lat,lon,fmt in:lat,lon,fmt status',' ')
       call xvmessage('The left two lat & lons should equal.',' ')
       call xvmessage('Fmt codes are: 1=centric 2=detic ',' ')
         call xvmessage('   ',' ')
         call xvmessage('input_lat & lon  --->  output_lat & lon',' ')
         call xvmessage('input_lat & lon  <---  output_lat & lon',' ')
         infmt=1
         iofmt=2
         do i=-50,50,100
          do j=0,270,90 
            lati=i
            long=j
            call triaxtran(a,b,c,bvector,bvector(mlimit*nlimit+1),
     +           authcoef1,authcoef2,nlimit,klimit,mlimit,
     +           lati,long,infmt,olat,olon,iofmt,ind)
            write(msg,107)lati,long,infmt,olat,olon,iofmt,ind
            call xvmessage(msg,' ')
            call triaxtran(a,b,c,bvector,bvector(mlimit*nlimit+1),
     +           authcoef1,authcoef2,nlimit,klimit,mlimit,
     +           olat,olon,iofmt,lati,long,infmt,ind)
            write(msg,107)lati,long,infmt,olat,olon,iofmt,ind
            call xvmessage(msg,' ')
            lati=i
            long=j+.01
            call triaxtran(a,b,c,bvector,bvector(mlimit*nlimit+1),
     +           authcoef1,authcoef2,nlimit,klimit,mlimit,
     +           lati,long,infmt,olat,olon,iofmt,ind)
            write(msg,107)lati,long,infmt,olat,olon,iofmt,ind
            call xvmessage(msg,' ')
            call triaxtran(a,b,c,bvector,bvector(mlimit*nlimit+1),
     +           authcoef1,authcoef2,nlimit,klimit,mlimit,
     +           olat,olon,iofmt,lati,long,infmt,ind)
            write(msg,107)lati,long,infmt,olat,olon,iofmt,ind
            call xvmessage(msg,' ')
            call xvmessage('   ',' ')
          enddo
         enddo 
      endif

c testing of triaxtran subroutine for special points in lat * lon
      if(test_triaxtran)then
       call xvmessage('   ',' ')
       call xvmessage('Test of triaxtran subroutine.',' ')
       call xvmessage('Test of special angles.',' ')
       call xvmessage('Ordering of the data is:',' ')
       call xvmessage('in:lat,lon,fmt out:lat,lon,fmt status',' ')
       call xvmessage('out:lat,lon,fmt in:lat,lon,fmt status',' ')
       call xvmessage('The left two lat & lons should equal.',' ')
       call xvmessage('Fmt codes are: 1=centric 2=detic ',' ')
       call xvmessage('3=snyder 4=conformal 5=authalic',' ')
       do infmt=1,5
        do iofmt=1,5
         call xvmessage('   ',' ')
         call xvmessage('input_lat & lon  --->  output_lat & lon',' ')
         call xvmessage('input_lat & lon  <---  output_lat & lon',' ')
         do i=-90,90,90
          do j=0,270,90 
            lati=i
            long=j
            call triaxtran(a,b,c,bvector,bvector(mlimit*nlimit+1),
     +           authcoef1,authcoef2,nlimit,klimit,mlimit,
     +           lati,long,infmt,olat,olon,iofmt,ind)
            write(msg,107)lati,long,infmt,olat,olon,iofmt,ind
            call xvmessage(msg,' ')
            call triaxtran(a,b,c,bvector,bvector(mlimit*nlimit+1),
     +           authcoef1,authcoef2,nlimit,klimit,mlimit,
     +           olat,olon,iofmt,lati,long,infmt,ind)
            write(msg,107)lati,long,infmt,olat,olon,iofmt,ind
            call xvmessage(msg,' ')
            call xvmessage('   ',' ')
          enddo
         enddo 
        enddo
       enddo
      endif

c testing of triaxtran subroutine for the 8 quadrants
      if(test_triaxtran)then
       call xvmessage('   ',' ')
       call xvmessage('Test of triaxtran subroutine.',' ')
       call xvmessage('Test of 8 quadrants.',' ')
       call xvmessage('Ordering of the data is:',' ')
       call xvmessage('in:lat,lon,fmt out:lat,lon,fmt status',' ')
       call xvmessage('out:lat,lon,fmt in:lat,lon,fmt status',' ')
       call xvmessage('The left two lat & lons should equal.',' ')
       call xvmessage('Fmt codes are: 1=centric 2=detic ',' ')
       call xvmessage('3=snyder 4=conformal 5=authalic',' ')
       do infmt=1,5
        do iofmt=1,5
         call xvmessage('   ',' ')
         call xvmessage('input_lat & lon  --->  output_lat & lon',' ')
         call xvmessage('input_lat & lon  <---  output_lat & lon',' ')
         do i=-40,40,80
          do j=20,290,90 
            lati=i
            long=j
            call triaxtran(a,b,c,bvector,bvector(mlimit*nlimit+1),
     +           authcoef1,authcoef2,nlimit,klimit,mlimit,
     +           lati,long,infmt,olat,olon,iofmt,ind)
            write(msg,107)lati,long,infmt,olat,olon,iofmt,ind
            call xvmessage(msg,' ')
            call triaxtran(a,b,c,bvector,bvector(mlimit*nlimit+1),
     +           authcoef1,authcoef2,nlimit,klimit,mlimit,
     +           olat,olon,iofmt,lati,long,infmt,ind)
            write(msg,107)lati,long,infmt,olat,olon,iofmt,ind
107         format(2f10.5,i3,3x,2f10.5,2i3)
            call xvmessage(msg,' ')
            call xvmessage('   ',' ')
          enddo
         enddo 
        enddo
       enddo
      endif

c test c bridge of triaxtran
      if(test_triaxtran)then
        call xvmessage('   ',' ')
        call xvmessage('Test of c bridge calls.',' ')
        infmt=1
        iofmt=4
        lati=40.d0
        long=20.d0
        call tztriaxtran(a,b,c,bvector,bvector(mlimit*nlimit+1),
     +    authcoef1,authcoef2,nlimit,
     +    klimit,mlimit,lati,long,infmt,olat,olon,iofmt,ind)
        call xvmessage('Input lat,lon,fmt  Output lat,lon,fmt,stat',' ')
        call xvmessage('Test of conformal case.',' ')
        write(msg,107)lati,long,infmt,olat,olon,iofmt,ind
        call xvmessage(msg,' ')
        iofmt=5
        call tztriaxtran(a,b,c,bvector,bvector(mlimit*nlimit+1),
     +    authcoef1,authcoef2,nlimit,
     +    klimit,mlimit,lati,long,infmt,olat,olon,iofmt,ind)
        call xvmessage('Test of authalic case.',' ')
        write(msg,107)lati,long,infmt,olat,olon,iofmt,ind
        call xvmessage(msg,' ')
      endif

c abandon coefficients
      if(planet.eq.'            ') then
         call xvmessage('Coefficients discarded',' ')
         return
      endif

c save coefficients
      outbuffer(1)=mlimit              ! save array dimensions
      outbuffer(2)=klimit
      outbuffer(3)=nlimit
      outbuffer(4)=a                   ! save normalized planet radii
      outbuffer(5)=b
      outbuffer(6)=c
      k=6
      do i=1,nlimit*mlimit*2           ! load conformal coefficients
        outbuffer(i+k)=bvector(i)
      enddo
      k=k+nlimit*mlimit*2
      do i=1,(mlimit+1)*(klimit+1)     ! load authalic coefficients
        outbuffer(i+k)=authcoef1(i)
      enddo
      k=k+(mlimit+1)*(klimit+1)
      do i=1,nlimit
        outbuffer(i+k)=authcoef2(i)
      enddo
      k=k+nlimit
      if(k.gt.ibis_column_length) then
         call xvmessage('Too many coefficients',' ')
         call xvmessage('Coefficients discarded',' ')
         return
      endif

      inquire(file=archive_filename,exist=archive_exists)

      if(archive_exists)then  ! update existing archive
        call xvmessage('Updating archive',' ')

c       open archive
        call xvunit(unit,'old',1,status,'U_NAME',archive_filename,' ')
        call xvsignal(unit,status,1)
        call ibis_file_open(unit,ibis_out,'update',0,0,' ',' ',status)      
        if(status.ne.1) call ibis_signal_u(unit,status,1)

c       get file size
        count=ibis_file_get(ibis_out,'nc',ncol,1,1)! cols
        if(count.ne.1) call ibis_signal(ibis_out,count,1)
        count=ibis_file_get(ibis_out,'nr',nrow,1,1)! rows
        if(count.ne.1) call ibis_signal(ibis_out,count,1)

c       get/create the column with this planet name
        count=ibis_column_find(ibis_out,'group',planet,icol,1,1)
        if(count.lt.0) call ibis_signal(ibis_out,count,1)

        if(count.eq.0)then
c          cannot find existing column with same name
c          create new column & place after last column
           call ibis_column_new(ibis_out,ncol,1,'doub',status)
           if(status.ne.1) call ibis_signal(ibis_out,status,1)
c          name the new column after the planet
           count=ibis_group_new(ibis_out,'group',planet,ncol+1,1,' ')
           if(count.lt.0) call ibis_signal(ibis_out,count,1)
           icol=ncol+1
         endif

c       write out data
        call ibis_column_write(ibis_out,outbuffer,icol,1,nrow,status)
        if(status.ne.1) call ibis_signal(ibis_out,status,1)
        call ibis_file_close(ibis_out,' ',status)       ! close file

c     create a new archive with 1 column
      else

        call xvmessage('Creating new archive',' ')
c       open archive
        nrow=ibis_column_length
        call xvunit(unit,'new',1,status,'U_NAME',archive_filename,' ')
        call xvsignal(unit,status,1)
        call ibis_file_unit(unit,ibis_out,'write',1,nrow,' ',
     +                      'column',status)      
        if(status.ne.1) call ibis_signal_u(unit,status,1)
c       make default 'double' before opening
        call ibis_file_set(ibis_out,'fmt_default','doub',status)
        if(status.ne.1) call ibis_signal(ibis_out,status,1)
        call ibis_file_unit_open(ibis_out,status)
        if(status.ne.1) call ibis_signal_u(unit,status,1)

c       name column 1 after the planet
        count=ibis_group_new(ibis_out,'group',planet,1,1,' ')
        if(count.lt.0) call ibis_signal(ibis_out,count,1)

c       write data
        call ibis_column_write(ibis_out,outbuffer,1,1,nrow,status)
        if(status.ne.1) call ibis_signal(ibis_out,status,1)
        call ibis_file_close(ibis_out,' ',status)       ! close file
        if(status.ne.1) call ibis_signal(ibis_out,status,1)
      endif

c test archive
      call xvmessage('   ',' ')
      call xvmessage('Reading new archive entry for verification.',' ')      
      call get_ellipsoid(archive_filename,planet,a,b,c,
     +       bvector,f,authcoef1,authcoef2,n,k,m,ind)
      if(ind.ne.0)then
        call xvmessage('get_ellipsoid error status',' ')
        call abend
      endif
      infmt=1
      iofmt=4
      lati=40.d0
      long=20.d0
      call triaxtran(a,b,c,bvector,f,authcoef1,authcoef2,nlimit,
     +    klimit,mlimit,lati,long,infmt,olat,olon,iofmt,ind)
      call xvmessage('Input lat,lon,fmt  Output lat,lon,fmt,stat',' ')
      call xvmessage('Test of conformal case.',' ')
      write(msg,107)lati,long,infmt,olat,olon,iofmt,ind
      call xvmessage(msg,' ')
      iofmt=5
      call triaxtran(a,b,c,bvector,f,authcoef1,authcoef2,nlimit,
     +    klimit,mlimit,lati,long,infmt,olat,olon,iofmt,ind)
      call xvmessage('Test of authalic case.',' ')
      write(msg,107)lati,long,infmt,olat,olon,iofmt,ind
      call xvmessage(msg,' ')

      call xvmessage('Normal complition of Triceof.',' ')
      return
      end

c *********************************************************************
      subroutine get_ellipsoid(archive_filename,planet,a,b,c,
     +        cc,cp,ac,ap,nlimit,klimit,mlimit,ind)

c routine to return the conformal & authalic buffers from the archive
c for a specific planet name in a format to match the coordinate conversion
c subroutine triaxtran.
c archive_filename   archive name             input     character*80
c planet             planet name              input     character*12
c a                  planet major radius      returned  real*8
c b                  planet middle radius     returned  real*8
c c                  planet minor radius      returned  real*8
c cc                 first conformal buffer   returned  real*8
c                    (length nlimit*mlimit)
c cp                 second conformal buffer  returned  real*8
c                    (length nlimit*mlimit)
c ac                 first authalic  buffer   returned  real*8
c                    (length (mlimit+1)*(klimit+1)
c ap                 second authalic buffer   returned  real*8
c                    (length nlimit)
c nlimit             buffer dimension         returned  integer*4
c klimit             buffer dimension         returned  integer*4
c mlimit             buffer dimension         returned  integer*4
c ind                status: ok=0 error=1     returned  integer*4

      implicit real*8 (a-h,o-z)
      parameter(ibis_column_length=1024)
      character*80 archive_filename
      character*12 planet
      logical archive_exists
      integer*4 status,unit,count
      real*8 buffer(ibis_column_length),cc(1),cp(1),ac(1),ap(1)

c locate the planet column & return data in buffer
      ind=0
      inquire(file=archive_filename,exist=archive_exists)

      if(archive_exists)then  ! update existing archive

c       open archive
        call xvunit(unit,'old',1,status,'U_NAME',archive_filename,' ')
        call xvsignal(unit,status,1)
        call ibis_file_open(unit,ibis_out,'update',0,0,' ',' ',status)      
        if(status.ne.1) call ibis_signal_u(unit,status,1)

c       get file size
        count=ibis_file_get(ibis_out,'nc',ncol,1,1)! cols
        if(count.ne.1) then
           call ibis_signal(ibis_out,count,1)
           ind=1
           return
        endif
        count=ibis_file_get(ibis_out,'nr',nrow,1,1)! rows
        if(count.ne.1) then
           call ibis_signal(ibis_out,count,1)
           ind=1
           return
        endif

c       get the column with this planet name
        count=ibis_column_find(ibis_out,'group',planet,icol,1,1)
        if(count.lt.0) then
           call ibis_signal(ibis_out,count,1)
           ind=1
           return
        else if(count.eq.0)then
c          cannot find existing column with same name
           call xvmessage('No column exists with this planet name',' ')
           ind=1
           return
        else
c          read data
           call ibis_column_read(ibis_out,buffer,icol,1,nrow,status)
           if(status.ne.1) then
              call ibis_signal(ibis_out,status,1)
              ind=1
              return
           endif
           call ibis_file_close(ibis_out,' ',status)       ! close file
        endif

      else
        call xvmessage('Coefficient archive does not exist',' ')
        ind=1
        return
      endif

c Load output arguments
      mlimit=nint(buffer(1))
      klimit=nint(buffer(2))
      nlimit=nint(buffer(3))
      a=buffer(4)
      b=buffer(5)
      c=buffer(6)
      k=6
      do i=1,nlimit*mlimit
        k=k+1
        cc(i)=buffer(k)
      enddo
      do i=1,nlimit*mlimit
        k=k+1
        cp(i)=buffer(k)
      enddo
      do i=1,(mlimit+1)*(klimit+1)
        k=k+1
        ac(i)=buffer(k)
      enddo
      do i=1,nlimit
        k=k+1
        ap(i)=buffer(k)
      enddo

      return
      end

c *********************************************************************
      subroutine authalic_coef(a,b,c,mlimit,klimit,nlimit,
     +        areafun,autlon,acoef,coef,coefp,radsph)

c To compute coefficients permitting the snyder to authalic computation.
c a= major axis normalized to 1              real*8  input
c b= middle axis normalized                  real*8 input
c c= minor axis normalized                   real*8 input
c mlimit= m maximum                          integer*4 input
c klimit= k maximum                          integer*4 input
c nlimit= n maximum                          integer*4 input
c arefun= scratch space for buffer areafun   real*8    input
c autlon= scratch space for buffer autlon    real*8    input
c acoef= scratch space for buffer acoef      real*8    input
c coef= longitude computation coefficients   real*8  returned
c coefp= latitude computation coefficients   real*8 returned
c radsph= radius of equivalent sphere        real*8 returned

      implicit real*8 (a-h,o-z)
      parameter (latpts=72,lonpts=72)
      real*8 elllat(0:latpts),elllon(0:lonpts)
      real*8 areafun(0:latpts,0:lonpts)
      real*8 acoef(0:latpts,0:mlimit),autlat(0:latpts)
      real*8 autlon(0:latpts,0:lonpts)
      real*8 coef(0:mlimit,0:klimit),coefp(nlimit)

      do i=0,latpts
        do j=0,mlimit
          acoef(i,j)=0.0
        enddo
      enddo
      do i=0,latpts
        do j=0,lonpts
          autlon(i,j)=0.0
        enddo
      enddo
      do i=0,mlimit
        do j=0,klimit
          coef(i,j)=0.0
        enddo
      enddo
      do i=0,latpts
        autlat(i)=0.0
      enddo
      do i=1,nlimit
        coefp(i)=0.0
      enddo

      dg1=datan(1.d0)/45.d0
      pi=datan(1.d0)*4.d0
      pih=pi/2.d0
      cb2=c*c/(b*b)
      ba2=b*b

      do i=0,latpts-1
        elllat(i)=i*dg1*90.d0/latpts
c        write(*,*)'elllat',elllat(i),i
      enddo
      elllat(latpts)=(latpts-.001)*dg1*90.d0/latpts
      do j=0,lonpts
        elllon(j)=j*dg1*90.d0/lonpts
c        write(*,*)'elllon',elllon(j),j
      enddo

c compute area function for integration
      do i=0,latpts
        do j=0,lonpts
          slon=dsin(elllon(j))
          clon=dcos(elllon(j))
          slat=dsin(elllat(i))
          clat=dcos(elllat(i))
          flon=slon*slon+ba2*clon*clon       ! eqn 9
          flat=slat*slat+cb2*clat*clat       ! eqn 10
          t1=(1.d0-ba2)*slon*clon*slat
          t2=dsqrt(slat*slat+cb2*cb2*flon*clat*clat)
          t3=dsqrt(slon*slon+ba2*ba2*clon*clon)
          costheta=t1/(t2*t3)                ! eqn 48
          sintheta=dsqrt(dabs(1.d0-costheta*costheta))
          areafun(i,j)=clat*t3*t2*sintheta/(flon*flon*flat*flat)
        enddo
      enddo

c compute total area of triaxial ellipsoid
      latlmt=latpts
      call simp2(lonlmt,lonpts,fsimlon,latlmt,c,latpts,
     +           elllat,areafun,pih,area)
      tarea=area

c compute authalic latitudes
      latlmt=0
      autlat(0)=0.d0
      do latlmt=6,latpts,6
        call simp2(lonlmt,lonpts,fsimlon,latlmt,c,latpts,
     +             elllat,areafun,pih,area)
        sphiaut=area/tarea
        if(dabs(sphiaut).gt. 0.9999999)then 
          autlat(latlmt)=pih
        else
          autlat(latlmt)=
     +        datan(sphiaut/dsqrt(dabs(1.d0-sphiaut*sphiaut)))
        endif
      enddo

c compute coefficients for authalic latitude using simpson integration
      do n=1,nlimit
        sum1=0.d0
        do i=6,latpts-6,12
          autfunc=(autlat(i)-elllat(i))*dsin(2*n*elllat(i))
          sum1=sum1+4.d0*autfunc
        enddo
        do i=12,latpts-12,12
          autfunc=(autlat(i)-elllat(i))*dsin(2*n*elllat(i))
          sum1=sum1+2.d0*autfunc
        enddo
        coefp(n)=4.d0*sum1/latpts
      enddo

      radsph=a*dsqrt(8.d0*tarea/(4.d0*pi)) !radius of equivalent sphere

c compute authalic longitude
      do i=0,latpts,6
c       first compute area of zone of quadrant along latitude
        lonlmt=lonpts
        call simp1(latpts,lonpts,areafun,i,lonlmt,pih,fsimlon)
        tcirc=fsimlon
c       now compute portion of zone for each step of longitude
        lonlmt=0
        autlon(i,0)=0.d0
        do lonlmt=6,lonpts,6
          call simp1(latpts,lonpts,areafun,i,lonlmt,pih,fsimlon)
          autlon(i,lonlmt)=(fsimlon/tcirc)*pih
        enddo
      enddo

c compute elements for simpson integration for acoef of authalic longitude
      do m=0,mlimit
        do i=0,latpts,6
          sum1=0.d0
          do j=6,lonpts-6,12
            autfunc=(autlon(i,j)-elllon(j))*dsin(2*m*elllon(j))
            sum1=sum1+4.d0*autfunc
          enddo
          do j=12,lonpts-12,12
            autfunc=(autlon(i,j)-elllon(j))*dsin(2*m*elllon(j))
            sum1=sum1+2.d0*autfunc
          enddo
          acoef(i,m)=4.d0*sum1/lonpts
        enddo
      enddo

c compute elements for simpson integration for C coef of authalic longitude
      do m=0,mlimit
        k=0
        sum1=acoef(0,m)
        do j=6,latpts-6,12
          sum1=sum1+4.d0*acoef(j,m)
        enddo
        do j=12,latpts-12,12
          sum1=sum1+2.d0*acoef(j,m)
        enddo
        sum1=sum1+acoef(latpts,m)
        coef(m,0)=2.d0*sum1/latpts
      enddo

      do m=1,mlimit
        do k=1,klimit
          sum1=acoef(0,m)
          do j=6,latpts-6,12
            autfunc=acoef(j,m)*dcos(2*k*elllat(j))
            sum1=sum1+4.d0*autfunc
          enddo
          do j=12,latpts-12,12
            autfunc=acoef(j,m)*dcos(2*k*elllat(j))
            sum1=sum1+2.d0*autfunc
          enddo
          sum1=sum1+acoef(latpts,m)*dcos(2*k*elllat(latpts))
          coef(m,k)=4.d0*sum1/latpts
        enddo
      enddo

      return
      end


c *********************************************************************
      subroutine simp1(latpts,lonpts,areafun,i,lonlmt,pih,fsimlon)

c Compute elements for Simpsons integration.
c fsimlon is returned
      implicit real*8 (a-h,o-z)
      real*8 areafun(0:latpts,0:lonpts)

      sum1=areafun(i,0)+areafun(i,lonlmt)
      do j=1,lonlmt-1,2
         sum1=sum1+4.d0*areafun(i,j)
      enddo
      do j=2,lonlmt-2,2
         sum1=sum1+2.d0*areafun(i,j)
      enddo
      fsimlon=sum1*pih/(lonpts*3)
      return
      end

c ******************************************************************
      subroutine simp2(lonlmt,lonpts,fsimlon,latlmt,c,latpts,
     +                  elllat,areafun,pih,area)

c Computes areas of zones
c lonlmt & area are returned
      implicit real*8 (a-h,o-z)

      real*8 elllat(0:latpts)
      real*8 areafun(0:latpts,0:lonpts)

      i=0
      lonlmt=lonpts
      call simp1(latpts,lonpts,areafun,i,lonlmt,pih,fsimlon)
      sum2=fsimlon
      do i=1,latlmt-1,2
         call simp1(latpts,lonpts,areafun,i,lonlmt,pih,fsimlon)
         sum2=sum2+4.d0*fsimlon
      enddo
      do i=2,latlmt-2,2
         call simp1(latpts,lonpts,areafun,i,lonlmt,pih,fsimlon)
         sum2=sum2+2.d0*fsimlon
      enddo
      i=latlmt
      call simp1(latpts,lonpts,areafun,i,lonlmt,pih,fsimlon)
      sum2=sum2+fsimlon
      area=c*c*sum2*elllat(latlmt)/(latlmt*3)
      return
      end
                
c********************************************************************
      subroutine authalic_rmserror(coef,coefp,nlimit,klimit,mlimit,
     +     radsph,degtorad,a,b,c,rmse)

c Routine to compute authalic solution rms error equation.
c Reference: Snyder, Survey Review Vol28, 217, July 1985

c COEF =  matrix of coefficients              input      real*8
c COEFP = primed matrix of coefficients      input      real*8
c nlimit= n dimension limit                 input      integer*4
c klimit= k dimension limit                 input      integer*4
c mlimit= m dimension limit                 input      integer*4
c radsph= radius of equivalent sphere       input      real*8
c degtorad=degrees/radian                   input      real*8
c a= major ellipsoid radius                 input      real*8
c b= middle ellipsoid radius                 input      real*8
c c= small ellipsoid radius                 input      real*8
c rms= rms error                            returned   real*8

      implicit real*8 (a-h,o-z)
      real*8 coef(0:mlimit,0:klimit),coefp(nlimit)

      bige=0.d0
      lptcount=0
      cb2=c*c/(b*b)
      ba2=b*b
      pi=datan(1.d0)*4.d0
      pih=pi/2.d0

      do i=0,90,5
        do j=90,0,-5
          elat=j
          elon=i
          call authtran(coef,coefp,nlimit,klimit,mlimit,
     +          elat,elon,alat,alon,1,ind)
          elat=elat*degtorad
          elon=elon*degtorad
          alat=alat*degtorad
          alon=alon*degtorad
          sp=dsin(elat)
          cp=dcos(elat)
          sl=dsin(elon)
          cl=dcos(elon)
          f1p=sp*sp+cb2*cp*cp
          f1l=sl*sl+ba2*cl*cl
          f2p=dsqrt(sp*sp+f1l*cb2*cb2*cp*cp)
          f2l=dsqrt(sl*sl+ba2*ba2*cl*cl)
          dxdp=0.d0
          dydp=1.d0
          dxdl=1.d0
          dydl=0.d0
          do m=1,mlimit
            dxdll=0.d0
            dxdpl=0.d0
            do k=0,klimit
              dxdll=dxdll+coef(m,k)*dcos(2*k*elat)
              dxdpl=dxdpl+2*k*coef(m,k)*dsin(2*k*elat)
            enddo
            dxdl=dxdl+dxdll*2.d0*m*dcos(2*m*elon)
            dxdp=dxdp-dxdpl*dsin(2*m*elon)
          enddo
          do n=1,nlimit
            dydp=dydp+n*2*coefp(n)*dcos(2*n*elat)
          enddo
          dxdl=dxdl*radsph/a
          dxdp=dxdp*radsph/a
          dydp=(dydp*radsph/a)*dcos(alat)
          dsdp=c*f2p/(dsqrt(f1l)*f1p*dsqrt(f1p))
          dsdl=c*cp*f2l/(dsqrt(f1p)*f1l*dsqrt(f1l))
          sinth=dydp/dsqrt(dxdp*dxdp+dydp*dydp)
          sh=dsqrt(dxdp*dxdp+dydp*dydp)/dsdp  ! scale factr on meridian
          sk=dxdl/dsdl                        ! scale factr on parallel
          th=(pih-datan(dxdp/dydp))/degtorad
          arerr=sh*sk*sinth-1.d0
          if(j.lt.89) then
            bige=bige+arerr*arerr
            lptcount=lptcount+1
          endif
        enddo    ! j
      enddo      ! i
      rmse=dsqrt(bige/lptcount)

      return
      end


c ********************************************************************
      subroutine conformal_rmserror(c,cp,nlimit,mlimit,npoints,rms)

c Routine to compute conformal solution rms error equation #73.
c Reference: Snyder, Survey Review Vol28, 217, July 1985

c C = C matrix of coefficients              input      real*8
c CP = C primed matrix of coefficients      input      real*8
c nlimit= n dimension limit                 input      integer*4
c mlimit= m dimension limit                 input      integer*4
c rms= rms error                            returned   real*8

      implicit real*8 (a-h,o-z)
      parameter (maxpts=800) ! Changes must be made in other places too !
      real*8 c(nlimit,mlimit),cp(nlimit,mlimit)
      real*8 lat(maxpts),lon(maxpts)
      real*8 costheta(maxpts),sintheta(maxpts),f(maxpts)
      common/c1/a,b,cc,lat,lon,costheta,sintheta,f

      rms=0.d0
      do i=1,npoints ! loop over random points used for fitting

c        Compute equation 67
         t=0.d0
         do m=1,mlimit
            t1=0.d0
            do n=0,nlimit-1
               t1=t1+n*c(n+1,m)*dsin(n*lat(i))
            enddo
            t=t+t1*dsin(2*m*lon(i))
         enddo
         eq67=a*(-t)

c        Compute equation 68
         t=0.d0
         do m=1,mlimit
            t1=0.d0
            do n=0,nlimit-1
               t1=t1+c(n+1,m)*dcos(n*lat(i))
            enddo
            t=t+m*t1*dcos(2*m*lon(i))
         enddo
         eq68=a*(1+2*t)

c        Compute equation 69
         t=0.d0
         do m=0,mlimit-1
            t1=0.d0
            do n=1,nlimit
               t1=t1+n*cp(n,m+1)*dcos(n*lat(i))
            enddo
            t=t+t1*dcos(2*m*lon(i))
         enddo
         eq69=a*(1.d0/dcos(lat(i))+t)

c        Compute equation 70
         t=0.d0
         do m=0,mlimit-1
            t1=0.d0
            do n=1,nlimit
               t1=t1+cp(n,m+1)*dsin(n*lat(i))
            enddo
            t=t+m*t1*dsin(2*m*lon(i))
         enddo
         eq70=a*(-2.d0*t)

         e1=eq68*sintheta(i)+eq70*costheta(i)-eq69*f(i)        !eqn 71
         e2=eq69*f(i)*sintheta(i)+eq67*f(i)*costheta(i)-eq68   !eqn 72
         rms=rms+(e1*e1+e2*e2)                                 !eqn 73
         
      enddo
      rms=dsqrt(rms/npoints)           ! equation 81

      return
      end


c*********************************************************************
c functions used often
      function t_p1(m,n,i)      ! equation 76
      implicit real*8 (a-h,o-z)
      parameter (maxpts=800) ! Changes must be made in other places too !
      real*8 lat(maxpts),lon(maxpts)
      real*8 costheta(maxpts),sintheta(maxpts),f(maxpts)
      common/c1/a,b,c,lat,lon,costheta,sintheta,f
      t_p1=2.d0*a*m*sintheta(i)*dcos(n*lat(i))*dcos(2.d0*m*lon(i))
      return
      end

      function t_p2(m,n,i)      ! equation 77
      implicit real*8 (a-h,o-z)
      parameter (maxpts=800) ! Changes must be made in other places too !
      real*8 lat(maxpts),lon(maxpts)
      real*8 costheta(maxpts),sintheta(maxpts),f(maxpts)
      common/c1/a,b,c,lat,lon,costheta,sintheta,f
      t_p2=-2.d0*a*m*costheta(i)*dsin(n*lat(i))*dsin(2.d0*m*lon(i))-
     +   a*n*f(i)*dcos(n*lat(i))*dcos(2.d0*m*lon(i))
      return
      end

      function t_p3(m,n,i)      ! equation 78
      implicit real*8 (a-h,o-z)
      parameter (maxpts=800) ! Changes must be made in other places too !
      real*8 lat(maxpts),lon(maxpts)
      real*8 costheta(maxpts),sintheta(maxpts),f(maxpts)
      common/c1/a,b,c,lat,lon,costheta,sintheta,f
      t_p3=-2.d0*a*m*dcos(n*lat(i))*dcos(2.d0*m*lon(i))-
     +   a*n*f(i)*dsin(n*lat(i))*dsin(2.d0*m*lon(i))*costheta(i)
      return
      end

      function t_p4(m,n,i)      ! equation 79
      implicit real*8 (a-h,o-z)
      parameter (maxpts=800) ! Changes must be made in other places too !
      real*8 lat(maxpts),lon(maxpts)
      real*8 costheta(maxpts),sintheta(maxpts),f(maxpts)
      common/c1/a,b,c,lat,lon,costheta,sintheta,f
      t_p4=a*n*f(i)*dcos(n*lat(i))*dcos(2.d0*m*lon(i))*sintheta(i)
      return
      end

c*********************************************************************
      REAL*4 FUNCTION RAN1(IDUM,ix1,ix2,ix3,r)
c     Returns random number between 0.0 and 1.0. To initialize provide
c     negative idum value.
      DIMENSION R(97)
      PARAMETER (M1=259200,IA1=7141,IC1=54773)
      PARAMETER (M2=134456,IA2=8121,IC2=28411)
      PARAMETER (M3=243000,IA3=4561,IC3=51349)
      parameter (RM1=1./M1,RM2=1./M2)
      IF (IDUM.NE.1) THEN
        IX1=MOD(IC1-IDUM,M1)
        IX1=MOD(IA1*IX1+IC1,M1)
        IX2=MOD(IX1,M2)
        IX1=MOD(IA1*IX1+IC1,M1)
        IX3=MOD(IX1,M3)
        DO 11 J=1,97
          IX1=MOD(IA1*IX1+IC1,M1)
          IX2=MOD(IA2*IX2+IC2,M2)
          R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
11      CONTINUE
        IDUM=1
      ENDIF
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      IX3=MOD(IA3*IX3+IC3,M3)
      J=1+(97*IX3)/M3
      IF(J.GT.97.OR.J.LT.1) call xvmessage('error in ran1',' ')
      RAN1=R(J)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      RETURN
      END

C*********************************************************************
      SUBROUTINE DSIMQ(A,B,N,KS)
C        PURPOSE
C           OBTAIN SOLUTION OF A SET OF SIMULTANEOUS LINEAR EQUATIONS,
C           AX=B
C
C        USAGE
C           CALL DSIMQ(A,B,N,KS)
C
C        DESCRIPTION OF PARAMETERS
C           A - MATRIX OF COEFFICIENTS STORED COLUMNWISE.  THESE ARE
C               DESTROYED IN THE COMPUTATION.  THE SIZE OF MATRIX A IS
C               N BY N.
C           B - VECTOR OF ORIGINAL CONSTANTS (LENGTH N). THESE ARE
C               REPLACED BY FINAL SOLUTION VALUES, VECTOR X.
C           N - NUMBER OF EQUATIONS AND VARIABLES. N MUST BE .GT. ONE.
C           KS - OUTPUT DIGIT
C                0 FOR A NORMAL SOLUTION
C                1 FOR A SINGULAR SET OF EQUATIONS
      real*8 A(1),B(1),biga,save,tol
C
C        FORWARD SOLUTION
C
      TOL=0.d0
      KS=0
      JJ=-N
      DO 65 J=1,N
      JY=J+1
      JJ=JJ+N+1
      BIGA=0.d0
      IT=JJ-J
      DO 30 I=J,N
C
C        SEARCH FOR MAXIMUM COEFFICIENT IN COLUMN
C
      IJ=IT+I
      IF(dabs(BIGA)-dabs(A(IJ))) 20,30,30
   20 BIGA=A(IJ)
      IMAX=I
   30 CONTINUE
C
C        TEST FOR PIVOT LESS THAN TOLERANCE (SINGULAR MATRIX)
C
      IF(dabs(BIGA)-TOL) 35,35,40
   35 KS=1
      RETURN
C
C        INTERCHANGE ROWS IF NECESSARY
C
   40 I1=J+N*(J-2)
      IT=IMAX-J
      DO 50 K=J,N
      I1=I1+N
      I2=I1+IT
      SAVE=A(I1)
      A(I1)=A(I2)
      A(I2)=SAVE
C
C        DIVIDE EQUATION BY LEADING COEFFICIENT
C
   50 A(I1)=A(I1)/BIGA
      SAVE=B(IMAX)
      B(IMAX)=B(J)
      B(J)=SAVE/BIGA
C
C        ELIMINATE NEXT VARIABLE
C
      IF(J-N) 55,70,55
   55 IQS=N*(J-1)
      DO 65 IX=JY,N
      IXJ=IQS+IX
      IT=J-IX
      DO 60 JX=JY,N
      IXJX=N*(JX-1)+IX
      JJX=IXJX+IT
   60 A(IXJX)=A(IXJX)-(A(IXJ)*A(JJX))
   65 B(IX)=B(IX)-(B(J)*A(IXJ))
C
C        BACK SOLUTION
C
   70 NY=N-1
      IT=N*N
      DO 80 J=1,NY
      IA=IT-J
      IB=N-J
      IC=N
      DO 80 K=1,J
      B(IB)=B(IB)-A(IA)*B(IC)
      IA=IA-N
   80 IC=IC-1
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create tztriaxtran.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* Unit test C-bridge for TTRIAXTRAN.F                                         */
/************************************************************************/

void FTN_NAME(tztriaxtran)(a,b,c,cc,cp,ac,ap,nlimit,klimit,mlimit,inlat,
     inlon,infmt,outlat,outlon,outfmt,status)
double *a;
double *b;
double *c;
double *cc;
double *cp;
double *ac;
double *ap;
int *nlimit;
int *klimit;
int *mlimit;
double *inlat;
double *inlon;
int *infmt;
double *outlat;
double *outlon;
int *outfmt;
int *status;
{
     ztriaxtran(*a,*b,*c,cc,cp,ac,ap,*nlimit,*klimit,*mlimit,*inlat,
     *inlon,*infmt, outlat, outlon, *outfmt, status);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ztriaxtran.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* C-Callable Version of TRIAXTRAN */
/************************************************************************/

void ztriaxtran(a,b,c,cc,cp,ac,ap,nlimit,klimit,mlimit,inlat,
     inlon,infmt,outlat,outlon,outfmt,status)
double a;
double b;
double c;
double *cc;
double *cp;
double *ac;
double *ap;
int nlimit;
int klimit;
int mlimit;
double inlat;
double inlon;
int infmt;
double *outlat;
double *outlon;
int outfmt;
int *status;

{
FTN_NAME(triaxtran)(&a,&b,&c,cc,cp,ac,ap,&nlimit,&klimit,&mlimit,&inlat,&inlon,
                    &infmt,outlat,outlon,&outfmt,status);
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create tricoef.imake
#define  PROGRAM   tricoef

#define MODULE_LIST tricoef.f tztriaxtran.c  ztriaxtran.c

#define MAIN_LANG_FORTRAN
#define R2LIB 

#define USES_FORTRAN
#define USES_ANSI_C

#define LIB_RTL
#define LIB_TAE
#define LIB_SPICE
#define LIB_P2SUB
#define LIB_NETWORK

/*#define LIB_LOCAL	/* for development, remove on delivery */ 
/*#define DEBUG		/* for development, remove on delivery */ 
$ Return
$!#############################################################################
$PDF_File:
$ create tricoef.pdf
process help=*
PARM ARCHIVE    TYPE=(STRING,80) COUNT=(0:1) +
   DEFAULT=TRIAXIAL_ARCHIVE.IBIS
PARM PLANET     TYPE=(STRING,12) COUNT=(0:1)              DEFAULT=--
PARM RADIUS     TYPE = REAL      COUNT=(0:3)              DEFAULT=--
PARM MLIMIT     TYPE = INTEGER   COUNT=(0:1) VALID=(1:15) DEFAULT=5
PARM KLIMIT     TYPE = INTEGER   COUNT=(0:1) VALID=(1:15) DEFAULT=5
PARM NLIMIT     TYPE = INTEGER   COUNT=(0:1) VALID=(1:15) DEFAULT=5
PARM TEST       TYPE = KEYWORD   COUNT=(0:1) VALID=(TEST,NOTEST) +
   DEFAULT=NOTEST
END-PROC
.TITLE
VICAR program TRICOEF.

.HELP
PURPOSE:
TRICOEF is used to compute and store for later retrieval the conformal
and authalic coefficients permitting computation of conformal 
(angle preserving) and authalic (equal area) projections of 
triaxial ellipsoids.
Tricoef computes the triaxial ellipsoid coefficients for computing:
1. Conformal latitude and longitude  from Snyder latitude and longitude.
2. Authalic  latitude and longitude  from Snyder latitude and longitude.
Coefficients are stored in an IBIS archive whose name is defined via the
ARCHIVE parameter.
To use these coefficients see the subroutine TRIAXTRAN.

Warning:
1. The mp routines compute the same coefficients but without a random number
generator to permit you to observe how sensitive the solution is to the
KLIMIT, MLIMIT, & NLIMIT parameters. You can run tricoef several times to
check the repeatability of lat & lon for the conformal case.

2. Occasionally the archive gets corrupted, You can tell when the computed
lat & lon's are erroneous. To clear this condition "rm triaxial_archive.ibis".

.PAGE
USAGE & EXECUTION examples:

tricoef radius=(1000.,998.,900.)   Coefficients are computed and
                                   then discarded since no planet was
                                   specified.

tricoef planet=IO mlimit=7 nlimit=7 klimit=7
                                   Radii come from the spice. 
                                   Coefficients are saved in the 
                                   archive under the name IO. If no
                                   archive exists one is created.
 
tricoef planet=XYZ radius=(1.,.9,.8) 
                                   A Synthetic planet XYZ is created and 
                                   the coefficients are saved under XYZ 
                                   in the same archive as above.
where:
 radius=(a,b,c)
 a = X radius (largest)  defining prime meridian (longitude=0).
 b = Y radius (middle)   defining +90 degrees east longitude.
 c = Z radius (smallest) defining spin axis.


.PAGE
OPERATION:
See: Snyder J P, Conformal Mapping of the Triaxial Ellipsoid, 
     Survey Review Vol.28, 217, July 1985
     Also private communications for the authalic case.

The coordinate systems here are all right handed. Longitude increases 
towards the east. Holding the planet before you with north up and the
prime meridian in front of you east increases to your right. 
Mother will help you distinguish between left and right.
Snyder describes the Snyder system in his article. I have derived the triaxial
definition of planetodetic so see me for clarification (j Lorre).

The purpose of this program is to compute an adequate set of coefficients
permitting accurate computation of conformal & authalic lat & lon. Precision
depends upon the MLIMIT & NLIMIT parameters for conformal and the MLIMIT
& KLIMIT & NLIMIT parameters for authalic projections. 
The larger they are the
more accurate the results but the longer the computation time.
Each computation of lat & lon requires MLIMIT*NLIMIT*2 trigonometric
computations for conformal and MLIMIT*KLIMIT+NLIMIT computations for 
authalic.
.PAGE
The more eccentric the object the larger MLIMIT & NLIMIT & KLIMIT
must be. To determine when you have arrived at good values:

1. Scrutinize the RMS error term printed for a measure of 
   conformality or authalicity constraint violation.
   An rms of .001 is a 0.1 percent violation of the constraint.
   The number should look like: .00000000854 but not like: .00456.....

2. Compare runs with increasing values of MLIMIT & NLIMIT &KLIMIT until the
   computed lat & lon's don't change. TRICOEF uses a random number 
   ( only in the conformal case where ill conditioning is critical)
   generator to produce points for fitting, thus assuring that each
   run is unique. You can run the same command line repeatedly to 
   assure that computed lat,lon values are repeatable. Note that
   different coefficients can give you similar lat,lon values but if
   the coefficients are large or vary widely between runs it might
   indicate ill conditioning.

.PAGE
The archive is an IBIS file in the new IBIS format.
The ibis coefficient storage file is used to store coefficients for 
other programs to retrieve. This file has columns of 1024 elements
per column. Each planet has one column with the planet name as a header.
All values are real*8.
The order of coefficients in each column is as follows:
Word 1  Contains the constant MLIMIT
Word 2  Contains the constant KLIMIT
Word 3  Contains the constant NLIMIT
Word 4  Contains the planet major radius A normalized to 1.0
Word 5  Contains the planet middle radius B normalized to B/A
Word 6  Contains the planet minor radius C normalized to C/A
Word 7  Begins the NLIMIT*MLIMIT CC matrix coefficients for conformal.
        k=7+nlimit*mlimit
Word k  Begins the NLIMIT*MLIMIT CP matrix coefficients for conformal.
        k=k+nlimit*mlimit
Word k  Begins the (MLIMIT+1)*(KLIMIT+1) COEF coefficients for authalic.
        k=k+(mlimit+1)*(klimit+1)
Word k  Begins the NLIMIT coefficients for authalic.
        k=k+nlimit-1
( k is the total number of words used )

These values are needed to operate the coordinate conversion subroutine
Triaxtran. When a planet is added a new column will be created.
If a planet is specified which is already in the archive
the old column will be overwritten by the new data. Thus the archive
grows to accomodate all planets of interest.
Notice that the archive defaults to your local directory. Change the
archive default if you want it elsewhere.

.PAGE
For your convenience the subroutine GET_ELLIPSOID is provided in the 
source code for extracting the data from the archive in a format 
required by Triaxtran. You can copy the source to your own application
to read the archive.

A WARNING TO ALL:
The subroutine TRIAXTRAN uses ALL the coefficients in the appropriate
archive column to perform authalic and conformat transformations. This
includes the normalized planet radii stored as column elements 4,5, and 6.
If you try using other radii than the ones stored with their column
coefficients the results will be incorrect.

HISTORY:
7-15-93  J Lorre. 
COGNIZANT PROGRAMMER:  Jean Lorre

05-11-98   RRP   Updated pdf to constrain mlimit, klimit, and nlimit to have
                 positive values less then 15.

.LEVEL1

.VARI ARCHIVE
Coefficients
archive.

.VARI PLANET
Planet name

.VARI RADIUS
Three planet radii.
A,B, and C.

.VARI MLIMIT
Number of longitude
terms.

.VARI KLIMIT
Number of latitude
terms. Authalic only.

.VARI NLIMIT
Number of latitude
terms.

.VARI TEST
Test TRIAXTRAN
subroutine using
coefficients

.LEVEL2

.VARI ARCHIVE
The name of the IBIS archive containing the conformal and authalic
coefficients. If no archive exists one will be created with the name
provided via the ARCHIVE parameter. New archives have 1 column
and 1024 points/column. Each planet uses one column. Each column
is headed by the name of the planet provided via the PLANET parameter.
Default is:   ARCHIVE=TRIAXIAL_ARCHIVE.IBIS

.VARI PLANET
Specifies the planet for which a model is desired. To later retrieve this
model the same planet name will be required. If no planet is specified
the coefficients are not saved in the archive.

.VARI RADIUS
Three planet radii (a,b,c) in arbitrary units.
where:
 a = X radius (largest)  defining prime meridian (longitude=0).
 b = Y radius (middle)   defining +90 degrees east longitude.
 c = Z radius (smallest) defining spin axis.
If PLANET is specified RADIUS is not necessary since the planet radii 
are retrieved from the SPICE. You can still specify RADIUS to either:
1. Change the radii for an existing planet.
2. Permit specification of a fictitious planet for testing purposes.

.VARI MLIMIT
Coefficient matrix width for modelling the longitude conversion.
Highly eccentric objects require more terms.
MLIMIT is typically about 6. The maximum is about 14.

.VARI KLIMIT
Coefficient array length for modelling the latitude conversion for
authalic only.
Highly eccentric objects require more terms.
NLIMIT is typically about 6. The maximum is about 14.

.VARI NLIMIT
Coefficient matrix width for modelling the latitude conversion.
Highly eccentric objects require more terms.
NLIMIT is typically about 6. The maximum is about 14.

.VARI TEST
(This is provided more as a test of the triaxtran subroutine than as a
test of the tricoef program.)
Test the coefficients just computed by calling the subroutine
TRIAXTRAN in every combination of lat/lon modes in all 8 octants of
the sphere. Default is NOTEST.
At the end of the program the coefficients will be re-read from the
catalogue and two points computed which can be checked against the
previously printed test. This verifies that the coefficients are
read properly.

.END
$ Return
$!#############################################################################
$Test_File:
$ create tsttricoef.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
!
! create coefficients but don't update the archive
tricoef radius=(3399.3,3394.0,3376.4) mlimit=3 nlimit=3 klimit=3
!
! create archive and coefficients for planet1
! test triaxtran subroutine
! test the ability to read from the archive & compute above test values
tricoef radius=(1.,.8,.6) 'test planet=planet1 +
  archive=triaxial_archive.ibis
!
! add to the archive coefficients for planet IO
! test reading radii from SPICE
tricoef planet=IO +
  archive=triaxial_archive.ibis
!
! replace planet1 with new coefficients 
tricoef radius=(1.,.9,.8) planet=planet1 mlimit=3 nlimit=3 klimit=3 +
  archive=triaxial_archive.ibis
end-proc
$ Return
$!#############################################################################
