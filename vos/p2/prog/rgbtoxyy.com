$!****************************************************************************
$!
$! Build proc for MIPL module rgbtoxyy
$! VPACK Version 1.8, Tuesday, April 25, 1995, 09:46:46
$!
$! Execute by entering:		$ @rgbtoxyy
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
$ write sys$output "*** module rgbtoxyy ***"
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
$ write sys$output "Invalid argument given to rgbtoxyy.com file -- ", primary
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
$   if F$SEARCH("rgbtoxyy.imake") .nes. ""
$   then
$      vimake rgbtoxyy
$      purge rgbtoxyy.bld
$   else
$      if F$SEARCH("rgbtoxyy.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake rgbtoxyy
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @rgbtoxyy.bld "STD"
$   else
$      @rgbtoxyy.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create rgbtoxyy.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack rgbtoxyy.com -
	-s rgbtoxyy.f -
	-i rgbtoxyy.imake -
	-p rgbtoxyy.pdf -
	-t tstrgbtoxyy.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create rgbtoxyy.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c
      include 'VICMAIN_FOR'
      subroutine main44
      implicit NONE

      integer maxin,maxcolors,mapxy
      parameter (maxin=10,maxcolors=10,mapxy=256)

c maxin=     the largest number of input images
c maxcolors= the largest number of special colors
c mapxy=     the nl and ns of the 4th output chromaticity map.
c
c WARNING: change array sizes in DLSQP if you change MAXIN or MAXCOLORS

      character*5 project
      character*80 msg

      integer*4 def,colors(maxcolors),filter(maxin),status
      integer*4 unit(maxin),camera,fds,label_data(80),ncolors,nconv
      integer*4 ounit(3),iproject
      integer*4 nli(maxin),nsi(maxin),nin,nout
      integer*4 nioverf,nfilter,nxyz,nres,i,ns,nl,ind,j,k
      integer*4 nequations,nunknowns,maxhist
      integer*4 n,l,m
      integer*4 lbounds,ii

      integer*2 xybuf(mapxy,mapxy)

      real*4 buf(8000,maxin)
      real*4 conv(maxin),ioverf(maxin)
      real*4 special_response(maxin,maxcolors)
      real*4 special_XYZ(3,maxcolors),solution(maxin,3),xyunit,xy_slope
      real*4 units(maxin),storage(maxin*maxcolors)
      real*4 radiance_XYZ(3,3),xy_offset
      real*4 reflectance_response(0:7,3),radiance_response(0:7,3)
      real*4 xy_sl,dnscale
      real*4 tristim_X,tristim_Y,tristim_Z,sum,x,y

      real*8 c(maxcolors*2,maxin),cl(maxcolors*2),soln(maxin,3)
      real*8 resid(maxcolors*2),err,err_soln(maxin),wts(maxcolors*2)

      logical radiance,xvptst,print

     
c radiance tristimulus values for special colors
      data radiance_XYZ/
c my table
     + 24.72,24.44,21.28, ! x=.351 y=.347 desert 1
     + 8.131,8.114,11.274, ! x=.295 y=.295 water  2
     + 75.15,77.36,80.00/ ! x=.323 y=.332 cloud  3
c klaasen table
c     + 14.32,14.16,12.33, ! x=.351 y=.347 desert 1 
c     + 4.711,4.701,6.532, ! x=.295 y=.295 water  2
c     + 43.54,44.82,46.35/ ! x=.323 y=.332 cloud  3

c dn * I/F  (10000 means 100% reflectance ) for special colors
c filter#1=green filter#2=red filter#3=blue
      data reflectance_response/            ! filters 0-7
c my table
c     + 0.,3558.,4309.,3725.,0.,0.,0.,0.,        ! desert 1
c     + 0.,1362.,834.,3697.,0.,0.,0.,0.,         ! water 2
c     + 0.,5893.,5921.,6450.,0.,0.,0.,0./        ! cloud 3
c klaasen table
     + 2530.,2280.,2750.,1970.,2400.,2880.,2660.,3200.,! desert 1
     + 721.,745.,667.,1260.,559.,520.,608.,676.,       ! water  2
     + 7110.,7220.,7110.,7240.,7100.,6690.,7080.,6600./! cloud 3

c dn * CONV ( nanowatts/cm**2 ster nano ) for special colors
      data radiance_response/    ! filters 0-7
c my table
c     + 0.,17671.,21398.,18499.,0.,0.,0.,0.,     ! desert 1
c     + 0.,6762.,4140.,17802.,0.,0.,0.,0.,       ! water 2
c     + 0.,29267.,29405.,32028.,0.,0.,0.,0./     ! cloud 3
c klaasen table
     + 12100.,13400.,13500.,9820.,9540.,7150.,12200.,9800.,! desert 1
     + 3440.,4380.,3270.,6260.,2220.,1290.,2560.,2080.,    ! water 2
     + 33900.,42500.,34900.,36100.,28200.,16600.,29800.,20300./! cloud 3


      call xveaction('SA',' ')
      call xvpcnt('INP',nin)
      if(nin.lt.3)then
         call mabend('Require at least 3 inputs')
      endif
      call xvpcnt('OUT',nout)
      if(nout.lt.3)then
         call mabend('Require 3 outputs')
      endif
      call xvparm('COLORS',colors,ncolors,def,maxcolors)
      if(ncolors.lt.nin.and.ncolors.gt.0)then
         call mabend('#special colors >= #inputs')
      endif
      call xvparm('CONV',conv,nconv,def,maxin)
      call xvparm('IOVF',ioverf,nioverf,def,maxin)
      call xvparm('FILTER',filter,nfilter,def,maxin)
      call xvparm('XYZ',special_XYZ,nxyz,def,3*maxcolors)
      call xvparm('RESPONSE',storage,nres,def,maxin*maxcolors)
      call xvparm('PROJECT',project,i,def,1)
      call xvparm('DNSCALE',dnscale,i,def,1)
      radiance=xvptst('RADIANCE')
      print=xvptst('PRINT')

c determine # special colors
      if(ncolors.eq.0)then
        if(nfilter.gt.0) ncolors=nfilter
        if(nxyz.gt.0)    ncolors=nxyz/3
        if(nres.gt.0)    ncolors=nres/nin
        if(ncolors.eq.0)then
          call xvmessage('Must specify either special colors or',' ')
          call xvmessage('XYZ and RESPONSE keywords.',' ')
          call abend
        endif
      endif

c open all inputs
      do i=1,nin
        call xvunit(unit(i),'INP',i,status,' ')
        call xvopen(unit(i),status,'U_FORMAT','REAL',' ')
        call xvget(unit(i),status,'NL',nli(i),'NS',nsi(i),' ')
      enddo

c determine output image size
      ns=nsi(1)
      nl=nli(1)
      do i=2,nin
         nl=min(nl,nli(i))
         ns=min(ns,nsi(i))
      enddo

c extract project ID and label information
      if(project.eq.'NONE')goto 102
      iproject=0
      if(project.eq.'LABEL') iproject=1
      do i=1,nin
        if(iproject.eq.1)then
          call getproj(unit(i),project,camera,fds,ind)
        endif
        call getlabcon(unit(i),project,label_data,ind)
        if(ind.le.1)then
          if(i.gt.nfilter) filter(i)=label_data(4)        
          if(i.gt.nconv) conv(i)=float(label_data(29)) * 1.0e+12
          if(i.gt.nioverf) ioverf(i)=float(label_data(28)) * 1.0e+4  
        else
          if(i.gt.nfilter) filter(i)=-1        
          if(i.gt.nconv) conv(i)=-1.
          if(i.gt.nioverf) ioverf(i)=-1.  
        endif
      enddo
102   continue

c print status
      call xvmessage(' ',' ')
      call xvmessage('input filter        conv        ioverf',' ')
      do i=1,nin
        write(msg,100) i,filter(i),conv(i),ioverf(i)
100     format(3x,i2,5x,i2,8x,e10.3,e10.3)
        call xvmessage(msg,' ')
      enddo

c check filter and color
      do i=1,ncolors
        if(colors(i).lt.1.or.colors(i).gt.3)then
          call xvmessage('Valid colors are 1,2,3 ',' ')
          call abend
        endif
      enddo
      do i=1,nin
        if(filter(i).lt.1.or.filter(i).gt.3)then
          call xvmessage('Valid filters are 1,2,3 ',' ')
          call abend
        endif
      enddo


c extract the XYZ values to be used in the fit
      if(nxyz.eq.0)then
        do i=1,ncolors
          do j=1,3
            special_XYZ(j,i)=radiance_XYZ(j,colors(i))
          enddo
        enddo        
      endif

c extract the response values to be used in the fit
      if(nres.eq.0)then
        do i=1,ncolors
          do j=1,nin
            if(radiance)then
              special_response(j,i)=
     +                    radiance_response(filter(j),colors(i))
            else
              special_response(j,i)=
     +                    reflectance_response(filter(j),colors(i))
            endif
          enddo
        enddo        
      else
        k=0
        do i=1,ncolors
          do j=1,nin
            k=k+1
            special_response(j,i)=storage(k)
          enddo
        enddo
      endif


c extract the scaling values to use (I/F or CONV).
      do i=1,nin
        if(radiance)then
          units(i)=conv(i)*dnscale
        else
          units(i)=ioverf(i)*dnscale
        endif
      enddo


c set up and solve the least squares problem relating camera response 
c through each of the input filters to special color XYZ tristimulus
c values. The equations are like:
c  X(color1)=Ax*response(filter1)+Bx*response(filter2)+...
c  X(color2)=Ax*response(filter1)+Bx*response(filter2)+...
c  X.....................................................
c solve for Ax,Bx...
c  Y(color1)=Ay*response(filter1)+By*response(filter2)+...
c  Y(color2)=Ay*response(filter1)+By*response(filter2)+...
c  Y.....................................................
c solve for Ay,By...
c  the same for Z
      
      nequations=ncolors
      nunknowns=nin
      if(nequations.lt.nunknowns)then
        call mabend('#special colors must be > #inputs')
      else if(nequations.eq.nunknowns)then
        nequations=2*nequations     ! to trick DLSQP
      endif

c     Solve for X coefficients   soln(1-nin,1)
      do i=1,nequations
        ii=mod(i,ncolors)
        if(ii.eq.0)ii=ncolors
        cl(i)=special_XYZ(1,ii)
        do j=1,nunknowns
          c(i,j)=special_response(j,ii)
        enddo
        wts(i)=1.d0
      enddo
      call dlsqp(nequations,nunknowns,c,cl,
     +           soln(1,1),resid,err,err_soln,wts,ind)
      if(ind.eq.1)then
        call mabend('DLSQP: divide by zero in X tristimulus fit')
      endif
      if(print)then
        call xvmessage(' ',' ')
        call
     *   xvmessage('Solution for DN to Tristimulus transformation.',' ')
        call xvmessage('X solutions',' ')
        call xvmessage('input_file  coefficient  uncertainty',' ')
        do i=1,nunknowns
          write(msg,101)i,soln(i,1),err_soln(i)
101       format(4x,i2,6x,d11.4,4x,d11.4)
          call xvmessage(msg,' ')
        enddo
        call xvmessage('input_point tristim_value  residual',' ')
        do i=1,ncolors
          write(msg,101)i,cl(i),resid(i)
          call xvmessage(msg,' ')
        enddo
      endif

c     Solve for Y coefficients   soln(1-nin,2)
      do i=1,nequations
        ii=mod(i,ncolors)
        if(ii.eq.0)ii=ncolors
        cl(i)=special_XYZ(2,ii)
      enddo
      call dlsqp(nequations,nunknowns,c,cl,
     +           soln(1,2),resid,err,err_soln,wts,ind)
      if(ind.eq.1)then
        call mabend('DLSQP: divide by zero in Y tristimulus fit')
      endif
      if(print)then
        call xvmessage('Y solutions',' ')
        call xvmessage('input_file  coefficient  uncertainty',' ')
        do i=1,nunknowns
          write(msg,101)i,soln(i,2),err_soln(i)
          call xvmessage(msg,' ')
        enddo
        call xvmessage('input_point tristim_value  residual',' ')
        do i=1,ncolors
          write(msg,101)i,cl(i),resid(i)
          call xvmessage(msg,' ')
        enddo
      endif

c     Solve for Z coefficients   soln(1-nin,3)
      do i=1,nequations
        ii=mod(i,ncolors)
        if(ii.eq.0)ii=ncolors
        cl(i)=special_XYZ(3,ii)
      enddo
      call dlsqp(nequations,nunknowns,c,cl,
     +           soln(1,3),resid,err,err_soln,wts,ind)
      if(ind.eq.1)then
        call mabend('DLSQP: divide by zero in Z tristimulus fit')
      endif
      if(print)then
        call xvmessage('Z solutions',' ')
        call xvmessage('input_file  coefficient  uncertainty',' ')
        do i=1,nunknowns
          write(msg,101)i,soln(i,3),err_soln(i)
          call xvmessage(msg,' ')
        enddo
        call xvmessage('input_point tristim_value  residual',' ')
        do i=1,ncolors
          write(msg,101)i,cl(i),resid(i)
          call xvmessage(msg,' ')
        enddo
      endif

c convert solutions from real*8 to real*4
      do j=1,3
        do i=1,nin
          solution(i,j)=soln(i,j)
        enddo
      enddo


c open outputs
      do i=1,3
        call xvunit(ounit(i),'OUT',i,status,' ')
        call xvopen(ounit(i),status,'O_FORMAT','REAL','U_FORMAT','REAL',
     +              'U_NL',nl,'U_NS',ns,'OP','WRITE',' ')
      enddo

c prepare 4th output
      if(nout.eq.4)then
        call xvunit(xyunit,'OUT',4,status,' ')
        call xvopen(xyunit,status,'O_FORMAT','BYTE','U_FORMAT','HALF',
     +              'U_NL',mapxy,'U_NS',mapxy,'OP','WRITE',' ')
        xy_slope=mapxy/1.2
        xy_offset=mapxy- xy_slope*1.1
	do i=1,mapxy
	  do j=1,mapxy
	    xybuf(i,j)=100
	  enddo
	enddo
c        call mve(2,mapxy*mapxy,100,xybuf,0,1)  ! set buffer to 100
        k=nint(xy_offset)
        l=mapxy- nint(xy_offset) +1
        do i=1,mapxy
          xybuf(k,i)=0 ! draw y axis
          xybuf(i,l)=0 ! draw x axis
          xybuf(i,i)=0 ! draw diagonal
        enddo
      endif

c process the image**************************************************

      lbounds=0
      do j=1,nl                         ! line loop
        do k=1,nin                      ! picture loop
          call xvread(unit(k),buf(1,k),status,'LINE',j,' ')
        enddo
        do i=1,ns                       ! pixel loop

c         Compute tristimulus values from coefficients.
          tristim_X=0.
          tristim_Y=0.
          tristim_Z=0.
          do n=1,nin                    ! picture loop
            tristim_X = tristim_X + solution(n,1)*buf(i,n)*units(n)
            tristim_Y = tristim_Y + solution(n,2)*buf(i,n)*units(n)
            tristim_Z = tristim_Z + solution(n,3)*buf(i,n)*units(n)
          enddo
          
c         Convert to Y,x,y
          sum=tristim_X + tristim_Y + tristim_Z
          if(sum.ne.0.0)then
            x=tristim_X/sum
            y=tristim_Y/sum
          else
            x=0.0
            y=0.0
          endif

c         update the 4th output image if desired
          if(nout.eq.4)then
            l=mapxy- nint(y*xy_slope+xy_offset) +1 ! line
            m=nint(x*xy_slope+xy_offset)           ! sample
            if(l.lt.1) l=1
            if(m.lt.1) m=1
            if(l.gt.mapxy) l=mapxy
            if(m.gt.mapxy) m=mapxy
            if(xybuf(m,l).lt.32767) xybuf(m,l)=xybuf(m,l)+1
          endif

c         check for negative luminance
          if(tristim_Y.lt.0.0)then
            lbounds=lbounds+1
            buf(i,1)=x
            buf(i,2)=y
            buf(i,3)=0.
          else
            buf(i,1)=x
            buf(i,2)=y
            buf(i,3)=tristim_Y
          endif


        enddo   ! pixel loop      

        do k=1,3             ! picture loop
          call xvwrit(ounit(k),buf(1,k),status,' ')
        enddo

      enddo     ! line loop

      call xvmessage(' ',' ')
      call prnt(4,1,lbounds,'number of negative luminance pixels=.')

c stretch and write out the 4th output image
      if(nout.eq.4)then
c       stretch xybuf
        maxhist=100
        do l=2,mapxy-1
          do m=2,mapxy-1
            if(xybuf(m,l).gt.maxhist) maxhist=xybuf(m,l)
          enddo
        enddo
        if(maxhist.gt.100)then
          xy_sl=155./(maxhist-100)
          do l=1,mapxy
            do m=1,mapxy
              if(xybuf(m,l).gt.100)then
                k=(xybuf(m,l)-100)*xy_sl +100.5
                if(k.gt.255) k=255
                xybuf(m,l)=k
              endif
            enddo
          enddo
        endif                    

        do l=1,mapxy             ! picture loop
          call xvwrit(xyunit,xybuf(1,l),status,' ')
        enddo
      endif

      end


c**************************************************************
      SUBROUTINE DLSQP(NE,NU,C,CL,X1,V,E,EX,wts,ind)
C
C1    GENERAL LEAST SQUARES SOLUTION OF NE EQUATIONS WITH NU UNKNOWNS,
C     C(I,1)*X1(1)+C(I,2)*X1(2)+...+C(I,NU)=CL(I) OF EQUAL WEIGHTS,WITH
C     I RANGING FROM 1 TO NE.
C
C2    THE INFORMATION FROM THE MAIN PROGRAM IS:
C          C(I,J) = COEFFICIENT MATRIX
C          CL(I) = ARRAY OF FREE TERMS
C          NE = NUMBER OF EQUATIONS
C          NU=NUMBER OF UNKNOWNS
c          wts(i)=weights
C
C3    THE INFORMATION RETURNED TO THE MAIN PROGRAM IS:
C          X1(J) = COMPUTED VALUES OF THE UNKNOWNS
C          V(I) = RESIDUALS  (I.E. OBSERVED MINUS COMPUTED)
C          E = MEAN ERROR OF THE UNIT WEIGHT
C          EX(J) = MEAN ERRORS OF THE UNKNOWNS
c          ind=0 if OK  =1 if division by zero
C
C5    ALL THE STATEMENTS BELOW ARE VALID FOR ANY NU LARGER THAN 1 AND
C     ANY NE LARGER THAN NU.
C
      integer s,ss,ind
c      parameter (s=8,ss=20)
      parameter (s=10,ss=20)
      REAL*8  A(s,s),AL(s),R(s,s),RL(s),Q(s,s),X(s),SL,SQ,P,SUM
      REAL*8 C(ss,s),CL(ss),X1(s),V(ss),EX(s),wts(ss),E
C
      ind=0
      DO 57 J = 1,NU
      DO 57 I=1,NU
      A(I,J)=0.
      R(I,J)=0.
57    Q(I,J)=0.
      DO 100 I=1,NU
      DO 100 J=1,NU
      DO 100 K=1,NE
100   A(I,J)=A(I,J)+C(K,I)*C(K,J)*wts(k)
      DO 102 I=1,NU
      AL(I)=0.
      DO 102 K=1,NE
102   AL(I)=AL(I)+C(K,I)*CL(K)*wts(k)
      NUM=NU-1
      NUP=NU+1
      DO 110 I=1,NUM
      K=I+1
      DO 110 J=K,NU
      if(a(i,i).ne.0.d0)then
        R(I,J)=A(I,J)/A(I,I)
      else
        ind=1
        return
      endif
      DO 110 L=1,I
110   A(K,J)=A(K,J)-R(L,K)*A(L,J)
      if(a(1,1).ne.0.d0)then
        RL(1)=AL(1)/A(1,1)
      else
        ind=1
        return
      endif
      DO 125 I=2,NU
      DO 122 J=1,I
122   AL(I)=AL(I)-R(J,I)*AL(J)
      if(a(i,i).ne.0.d0)then
        RL(I)=AL(I)/A(I,I)
      else
        ind=1
        return
      endif
125   continue
      X(NU)=RL(NU)
      DO 131 I=1,NUM
      IX=NU-I
      IXI=IX+1
      SUM=0.
      DO 130 J=IXI,NU
130   SUM=SUM-R(IX,J)* X(J)
131    X(IX)=RL(IX)+SUM
      DO 200 J=1,NU
200   X1(J)=X(J)
      if(a(nu,nu).ne.0.d0)then
        Q(NU,NU)=1./A(NU,NU)
      else
        ind=1
        return
      endif
      DO 150 I=1,NUM
      NP=NUP-1
      DO 135 J=I,NUM
      NM=NU-J
      JP=NM+1
      P=0.
      DO 135 K=JP,NU
      P=P-R(NM,K)*Q(NP,K)
      Q(NP,NM)=P
135   Q(NM,NP)=P
      NPM=NP-1
      SQ=0.
      DO 145 L=NP,NU
145   SQ=SQ-R(NPM,L)*Q(L,NPM)
      if(a(npm,npm).ne.0.d0)then
        Q(NPM,NPM)=1./A(NPM,NPM)+SQ
      else
        ind=1
        return
      endif
150   continue
      DO 151 I=1,NE
      V(I)=0.
      DO 151 J=1,NU
151   V(I)=V(I)+C(I,J)* X(J)
      SL=0.
      DO 153 I=1,NE
      V(I)=CL(I)-V(I)
153   SL=SL+V(I)*V(I)
      FNE=NE
      FNU=NU
      if(fne.ne.fnu)then
        E=DSQRT(SL/(FNE-FNU))
      else
        ind=1
        return
      endif
      DO 160 I=1,NU
        IF ( Q(I,I) .GE. 0.D0 ) THEN
          EX(I)=E*DSQRT(Q(I,I))
        ELSE
          EX(I)= 0.0                ! HANDLE NEGATIVES DUE TO ROUNDOFF.
        END IF
160   CONTINUE      
      RETURN
      END


$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create rgbtoxyy.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM rgbtoxyy

   To Create the build file give the command:

		$ vimake rgbtoxyy			(VMS)
   or
		% vimake rgbtoxyy			(Unix)


************************************************************************/


#define PROGRAM	rgbtoxyy
#define R2LIB

#define MODULE_LIST rgbtoxyy.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_MATH77
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create rgbtoxyy.pdf
process help=*

parm INP	count = 3:10
parm OUT	count = 3:4
parm COLORS	type=integer +
                count = 0:10 +			! special colors
                default = --
parm CONV	type = real +
  		count = 0:10 +
  		default = --
parm IOVF	type = real +
  		count = 0:10 +
  		default = --
parm FILTER	type = integer +
  		count = 0:10 +
  		default = --
parm XYZ	type = real +
  		count = 0:30 +
  		default = --
parm RESPONSE	type = real +
  		count = 0:100 +
  		default = --
parm MODE       type= keyword +
                valid=("RADIANCE","REFLECT") +
                default="RADIANCE"
parm print      type=keyword +
                valid=("PRINT","NOPRINT") +
                default="NOPRINT"
parm project    type=(string,5) +
                valid=("GLL","VGR-1","VGR-2","VIKOR","MAR10","MAR-9", +
                       "WFPC1","WFPC2","NONE","LABEL") +
                default="LABEL"
parm dnscale    type=real +
		count=0:1 +
		default=1.0
end-proc

.title
VICAR2 program "rgbtoxyy"

.help
PURPOSE
"rgbtoxyy" converts images from radiance or I/F units into color space
coordinates. The input images must be registered, at least three in number,
and all from separate spectral bands. Three output real files are
generated, representing: chromaticity x, chromaticity y, and 
tristimulus value Y.

EXECUTION
  The program is executed by specifying up to 10 images
taken through different filters, up to 10 special
colors, and, three output files representing x,y and Y.

OUTPUT FILES:
The first three outputs are in REAL format.

File 1 is chromaticity x (0 to 1).
File 2 is chromaticity y (0 to 1)
File 3 is tristimulus Y  (0 to 100)
 
  The fourth output (optional) contains a chromaticity 2-d histogram.
The file is 256 by 256 byte format on a 100 dn background.
The x chromaticity axis is in the sample direction.
The y chromaticity axis is in the line direction.
Each input dn value in the input pictures
is converted to Yxy and the (x,y) portion is accumulated above the
100 dn gray level as a 2-d histogram. This histogram will reveal the
clustering of colors in the images. 

EXAMPLE
In the following examples the user creates output files, OUT.*, using input
images INP.*, and special colors, 1, 2, and 3.

  VICAR>trucolor INP=(INP.ORA,INP.CLR,INP.GRE) OUT=(x,y,yy) +
  VICAR>+ COLORS=(1,2,3)
 
WRITTEN BY: 		J Lorre 4/1/95

THEORY:
                          Color Calibration

   This document describes the process of color calibration for the
purpose of producing colorimetric products.

   We first address the camera signal (dn) and the computation of U 
and I/F which are the decalibrated camera response. The response (dn) 
of a ccd detector is:

    t*O*a
dn= ----- integral(S*T*F*U*P*dl) + dn0                        (1)
     g*c
 
where:
dn=camera output number
g =gain state ratio factor
t =exposure time (sec)
c =high gain conversion factor (e/pixel/dn)
O =optics solid angle (sr)
a =area of ccd pixel (cm**2/pixel)
S =ccd spectral sensitivity (e/photon)
T =optics spectral transmission
F =filter spectral transmission
U =spectral scene radiance (watts/cm**2/sr/nm)
P =wavelength (nm) /1.9862e-16 (photons/watt-sec)
l =wavelength (nm)
dn0=dark current output number

   We wish to decalibrate this sensor before using the response (dn).
To do this we must extract the radiance U from the integral and solve
for it. The only way to extract U is to assume it is independent of 
wavelength. This is a serious error but one which must be made if
only one filter position is available. Typically we have three or more
filters if we are to perform color reconstruction, but for now we will
ignore this problem.

   Solving (1) for U we have:

      (dn-dn0)*g*c
U=-------------------------         watts/cm**2/sr/nm         (2)
  t*O*a*integral(S*T*F*P*dl)

or, in terms of reflectance I/F we have:

      (dn-dn0)*g*c*pi
I/F=-----------------------------     dimensionless albedo     (3)
     t*O*a*integral(S*T*F*H*P*dl)

where H is the solar irradiance incident on the surface.

To simplify things we denote from now on in the text both U
and I/F as symbol Q. In general U is used to produce color
for an additive device like a color TV monitor and I/F is used
for a subtractive device like a film recorder. 

We adopt a set of i special colors with known spectra C(i).
These spectra represent colors which we intend the camera to
reproduce with some precision. For example Kodak uses sky blue,
white cloud, green grass, and skin color as it's four special
colors for film process control. We could use Jupiter 
characteristic spectra.
The tristimulus values of these colors are computed from:

X(i)=integral(C(i)*x*dl)
Y(i)=integral(C(i)*y*dl)     1<=i<=I                           (4)
Z(i)=integral(C(i)*z*dl)

where: x,y,z are the standard CIE color matching functions.

We expose the camera to each of the colors to record the Q(ij) 
responses given by equation(2 or 3) in each of J filter positions.

Because the form of equations (1) and (4) are similar and linear it
is possible to represent the tristimulus values as a linear
combination of the camera responses Q(ij) to each special color i
in filter position j:

X(i)=A1*Q(i1)+A2*Q(i2)+A3*Q(i3)...AJ*Q(iJ)    
Y(i)=B1*Q(i1)+B2*Q(i2)+B3*Q(i3)...BJ*Q(iJ)   1<=i<=I        (5)
Z(i)=C1*Q(i1)+C2*Q(i2)+C3*Q(i3)...CJ*Q(iJ)    

There are as many X equations as there are special colors and as many
A terms as filters. We can solve for the A,B,C coefficients from each
group of the above equations. For input Q(ij) values taken through the
same filters used to solve equations (5) we can compute the tristimulus
values X,Y,Z. These are the values the eye should see.


To create a color product from a set of J input filters we do:

First:
1. Select a set of I representative special colors for which
   camera responses are known (from equations 2 or 3) and
   tristimulus values are known (from equation 4).
2. Solve equation (5) for the 3*J coefficients A(j),B(j),C(j).

Then, for each pixel:
3. From equation (5) compute the X,Y,Z tristimulus values.
4. Convert from tristimulus to chromaticity.
      x=X/(X+Y+Z) and y=Y/(X+Y+Z)



   Discussion

   We must present an argument for the validity of using U or I/F
since we have approximated these functions as independent of
wavelength. Equation (5) is justified because it is a linear
representation of responses which are themselves linear integrals.
Any approximation which is linear such as substituting a flat
function for the scene radiance can be compensated for by the
three sets of least squares fit coefficients A,B, and C.
The important criterion is that I/F be a measurable quantity.
Since dn is proportional with exposure and I/F is proportional
with dn then I/F is measurable and correlatable to X,Y, and Z.
Precisely the same method is used in determining chemical
abundances from spectra such as in NIMS. Equation (5) locks
the camera response to XYZ for the colors of interest.
   One way to get into trouble with equation (5) is to include
filter positions which fall partially out of the spectral
band of the color matching functions x,y,z. Any IR filter would
cause this problem. Another way is to include filters which 
overlap so much spectrally that the images they produce are
nearly identical. Both these cases can be mitigated to some
extent by using the first three terms in the principal 
component transformation of the input images. We must use the
same rotation matrix obtained from the special colors to rotate
the images of unknown scenes. 
   Equation (5) provides a least squares fit between 
the U or I/F values and tristimulus values, both obtained from
special color targets. Later on these same A,B,C coefficients
will be used to convert from U or I/F to tristimulus values for
each pixel in the image. It is imperative that identical
means be used to compute U or I/F (Q) in both cases. Thus the
cameras must be exposed to each of the special colors rather than
U or I/F being computed theoretically. The point is that both U
and I/F are wrong ! They are wrong because of the limitations imposed
by broad band radiometry. To get consistent results we must make the
same error at both the calibration and the evaluation ends.
   One can compute the I/F values theoretically for the special
colors instead of exposing the camera to them. As discussed this
is not correct but, for the sake of completeness, we present
the formula:

      integral(S*T*F*U*dl)
I/F =-------------------------
      integral(S*T*F*H*dl)

where H= The incident spectral irradiance to a surface.
      U= The reflected spectral radiance from the surface.
      U= H*R/pi, where R= The spectral reflectance of the surface.

   Alternative color method.

   There is an alternative method to color reconstruction.
This is to estimate the true spectral distribution of U or
I/F from a synthesis of the U or I/F values obtained through a
set of different camera filters (the very case we have). In this
scenario we attempt to guess the spectral radiance U which 
solves equation (1) for the J dn values AT EACH PIXEL.
Then we can integrate this function using equation (6) to
achieve the tristimulus values at each pixel directly
without using equation (5). This has been done by
Steve Wall JGR,Vol 8,No 28,pp 4401-4411. In this case special
colors are used more as a means of refining the method of
estimating U (since it is known) and of verifying results.

   Required calibration data:

1. Special colors targets imaged by the camera.
   This includes knowledge of the illuminant and target reflectance
   or transmittance curves, and the camera I/F or U responses.

2. A matrix spanning all dn ranges for calibrating the film recorder
   and TV monitor. This includes computations of the tristimulus
   values at each matrix point. A 5 by 5 by 5 cube would be adequate.



.level1

.vari INP
input image files

.vari OUT
output image files

.vari COLORS
special colors

.vari CONV
input image conversion factors
for radiance.

.vari IOVF
input image conversion factors
for reflectance.

.vari FILTER
GLL filter positions.

.vari XYZ
Tristimulus values
for special colors

.vari RESPONSE
The radiance or reflectance 
for special colors

.vari MODE
reflectance or radiance

.vari PRINT
print least squares fit

.vari PROJECT
override project type

.vari dnscale
multiply the input
dn's by dnscale

.level2
.vari INP
INP = (input1,...,inputN), where the inputs are up to ten 
images decalibrated by the program GALSOS.  The images must have been taken
with different filters, must all be the same size, and registered.

.vari OUT
OUT = (x chromaticity, y chromaticity, Y tristimulus), optional_2d_histogram
Outputs 1-3 are always REAL format.
There is an optional fourth BYTE output file containing a 2-d histogram of
the chromaticity space. See help file for specifics.

.vari COLORS
COLORS = (1,...,10), where the colors are up to 10 special 
colors.  There must be at least as many colors named as there are input images.
Three valid colors have been defined for GLL, they have the names:
NAME     number       description                 comments
desert     1       Earth desert yellow  
water      2       Earth ocean blue     
cloud      3       Earth clouds         
Tables of XYZ tristimulus values and response in both radiance and
reflectance for these 3 colors are built into the program already.
If you specify the XYZ and RADIANCE values by parameter input then
you need not specify special colors.
Example: colors=(1,2,3)

.vari CONV
CONV=(factor1,...,factorN), where each conversion factor corresponds to an 
input image (as they are ordered in INP).  CONV allows all input images to be
scaled in the same units, and should be the same as those specified in the
program GALSOS.  Each factor is multiplied by each pixel in the corresponding 
input image.  The number of factors specified must be at most equal to the 
number of inputs specified.
The default is to use CONV values in the picture labels.
Only used in the 'RADIANCE mode.

.vari IOVF
IOVF=(factor1,...,factorN), where each conversion factor corresponds to an 
input image (as they are ordered in INP).  IOVF allows all input images to be
scaled in the same units, and should be the same as those specified in the
program GALSOS.  Each factor is multiplied by each pixel in the corresponding 
input image.  The number of factors specified must be at most equal to the 
number of inputs specified.
The default is to use IOVF values in the picture labels.
Only used in the 'REFLECT mode.

.vari FILTER
The GLL filter position numbers (0 to 7) for each of the input images
in the order of input. Defaults to the filter position in the image
label.

.vari XYZ
Tristimulus values in the order X, Y, Z for each special color.
These offer a manual input. Usually they come from internal
tables pointed to by the COLORS keyword.

.vari RESPONSE
The radiance or reflectance values for each special color and for
each filter position input. In the order of each input for the
first special color followed by the order of input for the second
color...
These offer a manual input. Usually they come from internal
tables pointed to by the COLORS keyword and the FILTER keyword.
The I/F and CONV values are included in the RESPONSE so you must
provide either I/F*dn or CONV*dn.

.vari MODE
Specifies either reflectance or radiance units are to be used.
Specify either: 'RADIANCE or 'REFLECT (default is radiance).
Decalibrated images come with both I/F and CONV values so you must
tell the program which to use.

.vari PRINT
print least squares fit coefficients and coefficient residuals.

.vari PROJECT
Determines how to read the label to get information.

If you want to override project type in picture label and force the program 
to read that type of label then you can specify one of:
 GLL VGR-1 VGR-2 VIKOR MAR10 MAR-9 WFPC1 WFPC2

To not read any label specity : NONE

To let the program decide, specify: LABEL ( this is the default)

.vari dnscale
multiply the input dn's by dnscale. Used to rescale images compressed to byte 
format in order to restore the units to radiance or to reflectance.
 Defaults to 1.

.end
$ Return
$!#############################################################################
$Test_File:
$ create tstrgbtoxyy.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
!
rgbtoxyy inp=( +
  /project/it/testdata/gll/earth.red,  +
  /project/it/testdata/gll/earth.grn, +
  /project/it/testdata/gll/earth.blu) +
         out=(x.img,y.img,yy.img,hist.img) 'radiance +
         colors=(1,2,3) filter=(2,1,3) conv=(4.96,4.96,4.96) +
         dnscale=27.8 project=none 'print
! desert
list inp=x.img sl=241 ss=520 nl=5 ns=5 
list inp=y.img sl=241 ss=520 nl=5 ns=5 
list inp=yy.img sl=241 ss=520 nl=5 ns=5 
! cloud
list inp=x.img sl=542 ss=397 nl=5 ns=5 
list inp=y.img sl=542 ss=397 nl=5 ns=5 
list inp=yy.img sl=542 ss=397 nl=5 ns=5 
! ocean
list inp=x.img sl=367 ss=361 nl=5 ns=5 
list inp=y.img sl=367 ss=361 nl=5 ns=5 
list inp=yy.img sl=367 ss=361 nl=5 ns=5 
!
rgbtoxyy inp=( +
  /project/it/testdata/gll/earth.red,  +
  /project/it/testdata/gll/earth.grn, +
  /project/it/testdata/gll/earth.blu) +
        out=(x.img,y.img,yy.img,hist.img) 'reflect +
        colors=(1,2,3) filter=(2,1,3) iovf=(1.,1.,1.) +
         dnscale=27.8 project=none 'print
! desert
list inp=x.img sl=241 ss=520 nl=5 ns=5 
list inp=y.img sl=241 ss=520 nl=5 ns=5 
list inp=yy.img sl=241 ss=520 nl=5 ns=5 
! cloud
list inp=x.img sl=542 ss=397 nl=5 ns=5 
list inp=y.img sl=542 ss=397 nl=5 ns=5 
list inp=yy.img sl=542 ss=397 nl=5 ns=5 
! ocean
list inp=x.img sl=367 ss=361 nl=5 ns=5 
list inp=y.img sl=367 ss=361 nl=5 ns=5 
list inp=yy.img sl=367 ss=361 nl=5 ns=5 
!
end-proc
$ Return
$!#############################################################################
