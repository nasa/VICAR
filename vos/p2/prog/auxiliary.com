$!****************************************************************************
$!
$! Build proc for MIPL module auxiliary
$! VPACK Version 1.9, Wednesday, September 02, 1998, 16:26:16
$!
$! Execute by entering:		$ @auxiliary
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
$!   The default is to use the STD parameter if none is provided.
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
$ write sys$output "*** module auxiliary ***"
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
$ if (primary.eqs."") then primary = " "
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
$ write sys$output "Invalid argument given to auxiliary.com file -- ", primary
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
$   if F$SEARCH("auxiliary.imake") .nes. ""
$   then
$      vimake auxiliary
$      purge auxiliary.bld
$   else
$      if F$SEARCH("auxiliary.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake auxiliary
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @auxiliary.bld "STD"
$   else
$      @auxiliary.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create auxiliary.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack auxiliary.com -
	-s auxiliary.f -
	-i auxiliary.imake -
	-p auxiliary.pdf -
	-t tstauxiliary.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create auxiliary.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c
c program auxiliary
c
      include 'VICMAIN_FOR'
      subroutine main44

      parameter (maxsamp=360, maxline=180)
      real*4 obuf(maxsamp,5),inbuf(maxsamp,maxline,6)
      real*4 lat,lon,line,sample
      real*4 ee(maxsamp,maxline),ff(maxsamp,maxline)
      real*4 gg(maxsamp,maxline)
      real*4 conf_lat(maxsamp,maxline),conf_lon(maxsamp,maxline)
      real*4 cent_lat(maxsamp,maxline),cent_lon(maxsamp,maxline)
      real*4 values(2),candidate(2,4),violation(4)
      real*8 viol
      integer*4 ounit(5),def,count,passes,pass
      integer*4 unit(6),status,nl(6),ns(6)
      logical xvptst,conformal,authalic

c parameters
      call xvpcnt('INP',nids)      
      passes=nids/3-1    ! number of groups of EFG inputs
      conformal=xvptst('CONFORMAL')
      authalic=xvptst('AUTHALIC')
      call xvparm('NL',nlo,count,def,1)
      call xvparm('NS',nso,count,def,1)
      call xvparm('LOOP',nloop,count,def,1)
      call xvparm('RANGE',range,count,def,1)
      call xvparm('ANGLE',angle_weight,count,def,1)
      call xvparm('SCALE',scale_weight,count,def,1)

c set auxiliary conversions
      if(conformal)then
        call xvmessage('Conformal auxiliary coord conversions',' ')
      endif
      if(authalic)then
        call xvmessage('Authalic auxiliary coord conversions',' ')
      endif

c read EFG maps into memory
      do i=1,3
        call xvunit(unit(i),'INP',i,status,' ')
        call xvsignal(unit(i),status,1)
        call xvopen(unit(i),status,'U_FORMAT','REAL',' ')
        call xvsignal(unit(i),status,1)
        call xvget(unit(i),status,'NL',nl(i),'NS',ns(i),' ')
        call xvsignal(unit(i),status,1)
        if(ns(i).gt.maxsamp)then
          call xvmessage(' Model storage buffer too small',' ')
          call abend
        endif
        if(nl(i).gt.maxline)then
          call xvmessage(' Model storage buffer too small',' ')
          call abend
        endif
        do j=1,nl(i)                    ! line loop
          call xvread(unit(i),inbuf(1,j,i),status,'LINE',j,' ')
          call xvsignal(unit(i),status,1)
        enddo
      enddo

c read efg maps into memory
      do n=1,3
        i=n+3
        k=nids-3+n
        call xvunit(unit(i),'INP',k,status,' ')
        call xvsignal(unit(i),status,1)
        call xvopen(unit(i),status,'U_FORMAT','REAL',' ')
        call xvsignal(unit(i),status,1)
        call xvget(unit(i),status,'NL',nl(i),'NS',ns(i),' ')
        call xvsignal(unit(i),status,1)
        if(ns(i).gt.maxsamp)then
          call xvmessage(' Model storage buffer too small',' ')
          call abend
        endif
        if(nl(i).gt.maxline)then
          call xvmessage(' Model storage buffer too small',' ')
          call abend
        endif
        do j=1,nl(i)                    ! line loop
          call xvread(unit(i),inbuf(1,j,i),status,'LINE',j,' ')
          call xvsignal(unit(i),status,1)
        enddo
      enddo

c open 5 outputs
      if(nlo.eq.0)nlo=nl(1)
      if(nso.eq.0)nso=ns(1)
      do i=1,5
        call xvunit(ounit(i),'OUT',i,status,' ')
        call xvsignal(ounit(i),status,1)
        call xvopen(ounit(i),status,'U_FORMAT','REAL','O_FORMAT','REAL',
     +              'U_NL',nlo,'U_NS',nso,'OP','WRITE',' ')
        call xvsignal(ounit(i),status,1)
      enddo


c renormalize the e,f,g data for comparability with the E,F,G equivalents.
      constraint_iso=0.0
      constraint_map=0.0
      do j=1,nl(4)                         ! line loop
        do i=1,ns(4)                          ! pixel loop

          if(conformal)then
            constraint_iso=constraint_iso+
     +         abs(inbuf(i,j,1))+abs(inbuf(i,j,3)) ! E + G
            constraint_map=constraint_map+
     +         abs(inbuf(i,j,4))+abs(inbuf(i,j,6)) ! e + g

          else if(authalic)then
            constraint_iso=constraint_iso+
     +      abs(inbuf(i,j,1)*inbuf(i,j,3)-(inbuf(i,j,2))**2) ! E*G-F*F
            constraint_map=constraint_map+
     +      abs(inbuf(i,j,4)*inbuf(i,j,6))  ! e*g
          endif
        enddo
      enddo

      efg_scale=constraint_iso/constraint_map
      write(*,*)'e,f,g rescaling factor= ',efg_scale
      do j=1,nl(4)                         ! line loop
        do i=1,ns(4)                          ! pixel loop
          inbuf(i,j,4)=inbuf(i,j,4)*efg_scale
          inbuf(i,j,5)=inbuf(i,j,5)*efg_scale
          inbuf(i,j,6)=inbuf(i,j,6)*efg_scale
        enddo
      enddo

c store ee,ff,gg buffers for the sphere

      do j=1,nlo                         ! line loop
        do i=1,nso                          ! pixel loop
  
c         Convert output image coordinate to auxiliary lat lon .
c         Auxiliary is either conformal or authalic.
          line=j
          sample=i
          call xy2ll(line,sample,nlo,nso,lat,lon)
          conf_lat(i,j)=lat
          conf_lon(i,j)=lon
          cent_lat(i,j)=lat
          cent_lon(i,j)=lon

c         Compute e,f,g for the sphere. They are called ee, ff, gg
c         Note: the efg sphere is in auxiliary coordinates already.
          call ll2xy(lat,lon,nl(4),ns(4),line,sample)
          call get_dn(line,sample,inbuf(1,1,4),nl(4),ns(4),
     +      ee(i,j),ind,maxline,maxsamp)
          call ll2xy(lat,lon,nl(5),ns(5),line,sample)
          call get_dn(line,sample,inbuf(1,1,5),nl(5),ns(5),
     +      ff(i,j),ind,maxline,maxsamp)
          if(conformal) ff(i,j)=0.0
          call ll2xy(lat,lon,nl(6),ns(6),line,sample)
          call get_dn(line,sample,inbuf(1,1,6),nl(6),ns(6),
     +      gg(i,j),ind,maxline,maxsamp)
        enddo
      enddo
      grid=conf_lat(1,1)-conf_lat(1,2)
      write(*,*)'grid spacing is ',grid,' degrees'

      pass=0
100   pass=pass+1

c compute largest violation of the constraint to normalize motion.
      biggest=-1.0e+30
      do j=2,nlo-1
        do i=1,nso
          candidate(1,1)=cent_lat(i,j)
          candidate(2,1)=cent_lon(i,j)
          violation(1) = 
     +           constraint(candidate(1,1),ee(i,j),ff(i,j),
     +            gg(i,j),inbuf,nl,ns,
     +            maxline,maxsamp,conformal,authalic,E,F,G,
     +            angle_weight,scale_weight,ind)
          if(ind.gt.0)then
            write(*,*)'Constraint 1'
            call abend
          endif
          if(violation(1).gt.biggest)biggest=violation(1)
        enddo
      enddo
      write(*,*)'largest constraint violation=',biggest

c iteration

      delta=range
      d=0.1
      do loop=1,nloop
        delta=delta*0.9
        viol=0.d0
        do j=1,nlo
          do i=1,nso

c           Evaluate constraint at 4 locations
            candidate(1,1)=cent_lat(i,j)+d  ! up
            candidate(2,1)=cent_lon(i,j)
            if(candidate(1,1).gt.90.0)candidate(1,1)=90.0
            candidate(1,2)=cent_lat(i,j)-d  ! down
            candidate(2,2)=cent_lon(i,j)
            if(candidate(1,2).lt.-90.0)candidate(1,2)=-90.0
            candidate(1,3)=cent_lat(i,j)
            candidate(2,3)=cent_lon(i,j)+d  ! left
            if(candidate(2,3).gt.360.0)
     +         candidate(2,3)=candidate(2,3)-360.
            candidate(1,4)=cent_lat(i,j)
            candidate(2,4)=cent_lon(i,j)-d  ! right
            if(candidate(2,4).lt.0.0)
     +         candidate(2,4)=candidate(2,4)+360.

            do k=1,4
              violation(k) = 
     +           constraint(candidate(1,k),ee(i,j),ff(i,j),
     +            gg(i,j),inbuf,nl,ns,
     +            maxline,maxsamp,conformal,authalic,E,F,G,
     +            angle_weight,scale_weight,ind)
              if(ind.gt.0)then
                write(*,*)'Constraint 2'
                write(*,*)'loop,i,j',loop,i,j
                write(*,*)'cand',candidate(1,k),candidate(2,k)
                write(*,*)'cent',cent_lat(i,j),cent_lon(i,j)
                call abend
              endif
            enddo
            v=(violation(1)+violation(2)+violation(3)+
     +         violation(4))/4.
            viol=viol+v

c           Compute gradient components
            dvdy=(violation(1)-violation(2))/
     +         (candidate(1,1)-candidate(1,2))
            dvdx=(violation(3)-violation(4))/(2.0*d)

c           Reset point
            scale=sqrt(dvdx*dvdx+dvdy*dvdy)+1.0e-6
            dvdx=dvdx/scale
            dvdy=dvdy/scale 
            cent_lat(i,j)=cent_lat(i,j)-delta*dvdy*v/biggest
            cent_lon(i,j)=cent_lon(i,j)-delta*dvdx*v/biggest
            if(cent_lat(i,j).gt.90.)cent_lat(i,j)=90.
            if(cent_lat(i,j).lt.-90.)cent_lat(i,j)=-90.

          enddo
        enddo
        viol=viol/(nlo*nso)
        write(*,*)'iteration ',loop,' violation ',viol

c       detect point swapping
        k1=0
        do j=1,nlo
          do i=2,nso
            x=cent_lon(i-1,j)-cent_lon(i,j)
            if(x.lt.0.0)then
              if(x.gt.-180.) k1=k1+1
            endif
          enddo
        enddo
        k2=0
        do i=1,nso
          do j=2,nlo
            if(cent_lat(i,j-1)-cent_lat(i,j).lt.0.0) k2=k2+1
          enddo
        enddo
        if(k1+k2.gt.0)then
           write(*,*)k1+k2,' crossovers detected on iteration ',loop
           write(*,*)'   ',k1,' in longitude ',k2,' in latitude '
        endif
      enddo

      call xvclose(unit(1),status,'CLOS_ACT','FREE',' ')
      call xvclose(unit(2),status,'CLOS_ACT','FREE',' ')
      call xvclose(unit(3),status,'CLOS_ACT','FREE',' ')

c read next EFG maps into memory
      if(pass.lt.passes)then
        do i=1,3
          k=pass*3+i
          call xvunit(unit(i),'INP',k,status,' ')
          call xvsignal(unit(i),status,1)
          call xvopen(unit(i),status,'U_FORMAT','REAL',' ')
          call xvsignal(unit(i),status,1)
          call xvget(unit(i),status,'NL',nl(i),'NS',ns(i),' ')
          call xvsignal(unit(i),status,1)
          if(ns(i).gt.maxsamp)then
            call xvmessage(' Model storage buffer too small',' ')
            call abend
          endif
          if(nl(i).gt.maxline)then
            call xvmessage(' Model storage buffer too small',' ')
            call abend
          endif
          do j=1,nl(i)                    ! line loop
            call xvread(unit(i),inbuf(1,j,i),status,'LINE',j,' ')
            call xvsignal(unit(i),status,1)
          enddo
        enddo
        goto 100
      endif

c Write output files
      do j=1,nlo
        do i=1,nso
          obuf(i,1)=cent_lat(i,j)
          obuf(i,2)=cent_lon(i,j)
          values(1)=cent_lat(i,j)
          values(2)=cent_lon(i,j)
          x = constraint(values,ee(i,j),ff(i,j),gg(i,j),inbuf,nl,ns,
     +          maxline,maxsamp,conformal,authalic,E,F,G,
     +          angle_weight,scale_weight,ind)
          if(ind.gt.0)then
            write(*,*)'Constraint 3'
            call abend
          endif
          if(conformal)then
c           Compute tissot's angle
            call tissot(E,F,G,ee(i,j),ff(i,j),gg(i,j),dn,ind)
            obuf(i,3)=dn
          else
c           Compute area ratio
            top=ee(i,j)*gg(i,j)
            if(top.le.0.0)then
              top=0.0
            else
              top=sqrt(top)
            endif
            bottom=E*G-F*F
            if(bottom.le.0.0)then
              bottom=top
              if(bottom.eq.0.0)bottom=1.0
            else
              bottom=sqrt(bottom)
            endif
            obuf(i,3)=top/bottom
          endif
          obuf(i,4)=cent_lat(i,j)-conf_lat(i,j)
          obuf(i,5)=cent_lon(i,j)-conf_lon(i,j)
          if(obuf(i,5).lt.-180.)obuf(i,5)=obuf(i,5)+360.
          if(obuf(i,5).gt.180.)obuf(i,5)=obuf(i,5)-360.
        enddo
        do k=1,5
          call xvwrit(ounit(k),obuf(1,k),status,' ')
          call xvsignal(ounit(k),status,1)
        enddo
      enddo                                  ! line loop

      return
      end

c*************************************************************************
      function constraint(values,ee,ff,gg,inbuf,nl,ns,
     +  maxline,maxsamp,conformal,authalic,E,F,G,
     +  angle_weight,scale_weight,ind)

c returns the constraint given:
c  ee,ff,gg for the sphere,
c  centric lat & lon,
c  inbuf(*,*,3) containing the 1=E,2=F,3=G values

      real*4 inbuf(maxsamp,maxline,6),lat,lon,line,sample
      real*4 values(2) ! 1=centric lat, 2=centric lon
      integer*4 nl(6),ns(6)
      logical conformal,authalic

c     Compute E,F,G for the ISO. They are called E, F, G.
c     Note: the EFG maps are in centric coordinates already.

      lat=values(1)
      lon=values(2)
      call ll2xy(lat,lon,nl(2),ns(2),line,sample)
      call get_dn(line,sample,inbuf(1,1,2),nl(2),ns(2),F,ind,
     +            maxline,maxsamp)
      if(ind.ne.0)goto 10
      call ll2xy(lat,lon,nl(1),ns(1),line,sample)
      call get_dn(line,sample,inbuf(1,1,1),nl(1),ns(1),E,ind,
     +            maxline,maxsamp)
      if(ind.ne.0)goto 10
      call ll2xy(lat,lon,nl(3),ns(3),line,sample)
      call get_dn(line,sample,inbuf(1,1,3),nl(3),ns(3),G,ind,
     +            maxline,maxsamp)
      if(ind.ne.0)goto 10

      if(conformal)then
c        angle_constraint=abs(F/(sqrt(E*G)+1.0))  ! ff=0
c        scale_constraint=abs(E/ee-G/(gg+1.0))
        angle_constraint=abs(F*sqrt(ee*gg))  ! ff=0
        scale_constraint=abs(E*gg-ee*G)
        constraint=angle_weight*angle_constraint +
     +             scale_weight*scale_constraint
      else if(authalic)then
        constraint=abs( E*G-F*F-ee*gg ) + abs(F*sqrt(ee*gg)) ! ff=0
c        constraint=abs( (E*G-F*F)-(ee*gg-ff*ff) )
      else
        write(*,*)'must be conformal or authalic'
        ind=1
        return
      endif
      constraint=(constraint)**0.5
      ind=0
      return

10    continue
      write(*,*)'constraint error'
      write(*,*)'lat,lon=',lat,lon
      write(*,*)'line,sample=',line,sample
      ind=1
      return
      end

c*************************************************************************
      subroutine tissot(E,F,G,ee,ff,gg,dn,ind)
c returns as dn the Tissot indicatrix in degrees.
      implicit real*4(a-z)
      integer*4 ind

          ind=1
          dn=0.0
          degtorad=57.2957795

c             Solve quadratic for two roots t1 & t2
              a1=G*ff-F*gg
              if(a1.eq.0.d0)return
              b1=G*ee-E*gg
              c1=F*ee-E*ff
              d1=b1*b1-4.d0*a1*c1
              if(d1.lt.0.d0)return
              d1=sqrt(d1)
              t1=(-b1+d1)/(2.d0*a1)
              t2=(-b1-d1)/(2.d0*a1)

c             Solve for two mu values
              a1=E+2.d0*F*t1+G*t1*t1
              if(a1.eq.0.d0)return
              mu1=(ee+2.d0*ff*t1+gg*t1*t1)/a1
              if(mu1.lt.0.d0)return
              mu1=sqrt(mu1)
              a1=E+2.d0*F*t2+G*t2*t2
              if(a1.eq.0.d0)return
              mu2=(ee+2.d0*ff*t2+gg*t2*t2)/a1
              if(mu2.lt.0.d0)return
              mu2=sqrt(mu2)

c             Compute tissot's ellipse dimensions
              tissot_a = max(mu1,mu2)
              tissot_b = min(mu1,mu2)

c             Compute the angle deformation in degrees
              dn=(tissot_a - tissot_b)/(tissot_a + tissot_b)
              dn=2.0*asin(dn)*degtorad

          ind=0
          return
          end


c********************************************************************
      subroutine ll2xy(lat,lon,nl,ns,line,sample)
c Convert lat & lon into image coordinates for the object map

      real*4 lat,lon
      real*4 line,sample

c convert lat/lon to line/sample
      lon=mod(lon,360.)
      if(lon.lt.0.) lon=360.-lon
      t=(ns+1.0)/2.0
      if(lon.gt.180.)then
         sample=-(ns-t)*lon/180. + 2.0*ns - t
      else
         sample=-(t-1.0)*lon/180.+t
      endif
      line=(1.0-nl)*(lat-90.0)/180. + 1.0

      return
      end

c********************************************************************
      subroutine xy2ll(line,sample,nl,ns,lat,lon)
c convert line sample to lat lon.

      real*4 lat,lon
      real*4 line,sample

      lat=((line-1.0)*180./(1.0-nl))+90.0
      t=(ns+1.0)/2.0
      if(sample .gt. t)then
        lon=(sample+t-2.0*ns)*180./(t-ns)
      else
        lon=(sample-t)*180./(1.0-t)
      endif

      return
      end


c********************************************************************
      subroutine get_dn(line,sample,object,nl,ns,dn,ind,
     +          maxline,maxsamp)
c return the DN value at this image coordinate.
c ind=0 ok
c ind=1 off map, dn=0

      real*4 object(maxsamp,maxline),dn
      real*4 sample,line

c if are inside picture interpolate
      i=sample
      j=line
      if((i.ge.1).and.(i.lt.ns).and.(j.ge.1).and.
     + (j.lt.nl))then
         wt=sample-i
         dntop=wt*object(i+1,j)+(1.0-wt)*object(i,j)
         dnbot=wt*object(i+1,j+1)+(1.0-wt)*object(i,j+1)
         dn=(line-j)*dnbot+(j+1-line)*dntop
         ind=0
         return
      endif          

c if are outside picture return a zero
      ind=1
      dn=0.0
      i=nint(sample)
      j=nint(line)
      if(i.lt.1)return
      if(i.gt.ns)return
      if(j.lt.1)return
      if(j.gt.nl)return

c if are on picture border take nearest pixel
      ind=0
      dn=object(i,j)

      return
      end


$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create auxiliary.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM auxiliary

   To Create the build file give the command:

		$ vimake auxiliary			(VMS)
   or
		% vimake auxiliary			(Unix)


************************************************************************/


#define PROGRAM	auxiliary
#define R2LIB

#define MODULE_LIST auxiliary.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define R2LIB

#define LIB_FORTRAN
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define DEBUG
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create auxiliary.pdf
process help=*
PARM INP TYPE=STRING COUNT=(6:30)
PARM OUT TYPE=STRING COUNT=5
PARM PROPERTY TYPE=KEYWORD VALID=("CONFORMAL","AUTHALIC") +
  DEFAULT="CONFORMAL"
PARM NL TYPE=INTEGER COUNT=(0:1) DEFAULT=0
PARM NS TYPE=INTEGER COUNT=(0:1) DEFAULT=0
PARM LOOP TYPE=INTEGER COUNT=(0:1) DEFAULT=10
PARM RANGE TYPE=REAL COUNT=(0:1) DEFAULT=2.0
PARM SCALE TYPE=REAL COUNT=(0:1) DEFAULT=1.0
PARM ANGLE TYPE=REAL COUNT=(0:1) DEFAULT=1.0
END-PROC

.TITLE
VICAR program AUXILIARY

.HELP
PURPOSE:
To compute the conformal or authalic to planetocentric auxiliary coordinate
mapping which optimizes map projections of irregularly shaped objects.
Note: AREAISO is the alternative method.

EXECUTION:
auxiliary inp=(E1,F1,G1,E2,F2,G2,..e,f,g) out=(lat,lon,residual,dlat,dlon)

where:

E,F,G are the output of EFGISO for the iso.
E1,F1,G1 are smoothed more than En,Fn,Gn.
e,f,g are the output of EFGISO for the sphere.

lat and lon are maps in rectangular projections the same size and format as
the input images. These images contain the planetocentric lat and lon
respectively which map to the conformal or authalic lat and lon of their pixel
coordinates. ie: the contents are the centric coordinates of the auxiliary
arguments.

residual is the Tissot indicatrix angle in degrees for the conformal case:
 sin(residual/2)=(a-b)/(a+b) 
or the area ratio for the authalic case:
 residual=sqrt(eg-f*f)/sqrt(EG-F*F)
 
residual is in the same format as lat & lon.

dlat and dlon are images of the displacement of the computed centric 
coordinates from their initial auxiliary values.

.PAGE

METHOD:

The following steps are executed for every output pixel:

1. Compute the conformat or authalic auxiliary coordinate from the output pixel 
   location.

2. Convert this to input image coordinates.

3. Read the values for e, f, g from input images 4-6.

4. Guess that the centric lat & lon is the same as the auxiliary lat & lon.

5. Convert the centric lat & lon to input image coordinates.

6. This gives values for E, F, G from input images 1-3.

7. From efg & EFG compute the constraint violation.

8. Estimate a new centric location by moving along the gradient downhill
   by a bit.

9. Iterate steps 5-8 to find the smallest constraint violation.


These steps are repeated for each set of three input images.


 the Tissot indicatrix angle omega
where sin(omega/2)=(a-b)/(a+b)
and a and b are the axes of an ellipse on the projection resulting from 
the incorrect projection of a circle on the ISO.

where E,F,and G are measured on the iso (irregularly shaped object).
and e,f,and g are measured from the sphere.

e = (dx/dp)**2+(dy/dp)**2+(dz/dp)**2
f = (dx/dp)*(dx/dl)+(dy/dp)*(dy/dl)+(dz/dp)*(dz/dl)
g = (dx/dl)**2+(dy/dl)**2+(dz/dl)**2

where:
x and y are cartesian coordinates on the map projection
( sample is x, and line is -y).
p is latitude
l is longitude

HISTORY:
9-1-98  J Lorre. 
COGNIZANT PROGRAMMER:  Jean Lorre

.LEVEL1
.VARI INP
Input fundamental
forms:
E1, F1, G1, E2, F2, G2,...En, Fn, Gn, e, f, g

.VARI OUT
Output images
LAT (latitude)
LON (longitude)
Residual
Delta_lat
Delta_lon

.VARI PROPERTY
conformal or
authalic

.VARI NL
lines in output

.VARI NS
samples in output

.VARI LOOP
Number of iterations
per group of EFG inputs.

.VARI RANGE
Maximum excursion
per iteration

.VARI SCALE
The scale weight.
Only in CONFORMAL mode.

.VARI ANGLE
The angle weight.
Only in CONFORMAL mode.

.LEVEL2
.VARI INP
Input images:
E1, F1, G1, E2, F2, G2,...En, Fn, Gn, e, f, g
where
E, F, G are measures of the ISO for decreasing amounts of smoothing.
e,f,g are for the sphere.

EFG are created by EFGISO for the iso.
efg are created by EFGISO for the sphere.
The inputs can be different sizes.

.VARI OUT
Five outputs: Lat & Lon & Residual & dlat & dlon
Lat is the planetocentric latitude at the auxiliary argument location.
Lon is the planetocentric longitude at the auxiliary argument location.
Residual is the Tissot angle for conformal and the area ratio for authalic.
dlat and dlon are images of the displacement of the computed centric 
coordinates from their initial auxiliary values.

.VARI PROPERTY
Map property
CONFORMAL (default)
AUTHALIC

.VARI NL
lines in output
Defaults to the first input image size.

.VARI NS
samples in output
Defaults to the first input image size.

.VARI LOOP
Number of iterations

.VARI RANGE
Maximum excursion per iteration.
Is reduced by 0.9 each iteration.

.VARI SCALE
The scale weight.
Only in CONFORMAL mode.

.VARI ANGLE
The angle weight.
Only in CONFORMAL mode.
$ Return
$!#############################################################################
$Test_File:
$ create tstauxiliary.pdf
procedure
refgbl $echo
body
let $echo="yes"
!
! for conformal phobos 
efgiso out=(E.img,F.img,G.img,R.img) planet=phobos nl=180 ns=360
efgiso out=(E6.img,F6.img,G6.img,R6.img) planet=phobos nl=180 ns=360 +
 nlw=19 nsw=37
efgiso out=(E5.img,F5.img,G5.img,R5.img) planet=phobos nl=180 ns=360 +
 nlw=11 nsw=19
efgiso out=(E4.img,F4.img,G4.img,R4.img) planet=phobos nl=180 ns=360 +
 nlw=7 nsw=9
efgiso out=(E3.img,F3.img,G3.img,R3.img) planet=phobos nl=180 ns=360 +
 nlw=5 nsw=5
efgiso out=(E2.img,F2.img,G2.img,R2.img) planet=phobos nl=180 ns=360 +
 nlw=3 nsw=3
efgiso out=(SE.img,SF.img,SG.img,SR.img) planet=phobos nl=180 ns=360 +
  triaxial=(11.7,11.7,11.7)
auxiliary inp=(E.img,F.img,G.img,SE.img,SF.img,SG.img) +
 nl=180 ns=360 out=(a1.img,a2.img,tissot_raw.img,a3.img,a4.img) +
 loop=0 range=.2
auxiliary inp=( +
 E6.img,F6.img,G6.img, +
 E5.img,F5.img,G5.img, +
 E4.img,F4.img,G4.img, +
 E3.img,F3.img,G3.img, +
 E2.img,F2.img,G2.img, +
 E.img,F.img,G.img,SE.img,SF.img,SG.img) +
 nl=180 ns=360 out=(cen_lat.img,cen_lon.img,tissot.img,+
 del_lat.img,del_lon.img) loop=10 range=.2
!
! for authalic phobos 
!auxiliary inp=(E.img,F.img,G.img,SE.img,SF.img,SG.img) +
! nl=180 ns=360 out=(a1.img,a2.img,sinu_resid_raw.img,a3.img,a4.img) +
! loop=0 range=.2 'authalic
!xvd sinu_resid_raw.img
!auxiliary inp=( +
! E6.img,F6.img,G6.img, +
! E5.img,F5.img,G5.img, +
! E4.img,F4.img,G4.img, +
! E3.img,F3.img,G3.img, +
! E2.img,F2.img,G2.img, +
! E.img,F.img,G.img,SE.img,SF.img,SG.img) +
! nl=180 ns=360 out=(sinu_cen_lat.img,sinu_cen_lon.img,sinu_resid.img,+
! sinu_del_lat.img,sinu_del_lon.img) loop=10 range=.2 'authalic
!xvd sinu_del_lat.img
!xvd sinu_del_lon.img
!xvd sinu_resid.img
!
! For conformal triaxial
!auxiliary inp=(TE.img,TF.img,TG.img,SE.img,SF.img,SG.img) +
! nl=180 ns=360 out=(a1.img,a2.img,triax_tissot_raw.img,a3.img,a4.img) +
! loop=0 range=.2
!hist inp=triax_tissot_raw.img 'nohist
!xvd triax_tissot_raw.img
!auxiliary inp=(TE.img,TF.img,TG.img,SE.img,SF.img,SG.img) +
! nl=180 ns=360 out=(triax_cen_lat.img,triax_cen_lon.img,triax_tissot.img,+
! triax_del_lat.img,triax_del_lon.img) loop=10 range=.4 +
! scale=1. angle=1.
!hist inp=triax_tissot.img 'nohist
!xvd triax_del_lat.img
!xvd triax_del_lon.img
!xvd triax_tissot.img
!xvd triax_cen_lat.img
!xvd triax_cen_lon.img
!
! For authalic triaxial
!auxiliary inp=(TE.img,TF.img,TG.img,SE.img,SF.img,SG.img) +
! nl=180 ns=360 out=(a1.img,a2.img,sinu_triax_resid_raw.img,a3.img,a4.img) +
! loop=0 range=.2 'authalic
!hist inp=sinu_triax_resid_raw.img 'nohist
!xvd sinu_triax_resid_raw.img
!auxiliary inp=(TE.img,TF.img,TG.img,SE.img,SF.img,SG.img) +
! nl=180 ns=360 out=(sinu_triax_cen_lat.img,sinu_triax_cen_lon.img, +
! sinu_triax_resid.img,sinu_triax_del_lat.img,sinu_triax_del_lon.img) +
! loop=10 range=2. 'authalic
!hist inp=sinu_triax_resid.img 'nohist
!xvd sinu_triax_del_lat.img
!xvd sinu_triax_del_lon.img
!xvd sinu_triax_resid.img
!xvd sinu_triax_cen_lat.img
!xvd sinu_triax_cen_lon.img
!
end-proc
$ Return
$!#############################################################################
