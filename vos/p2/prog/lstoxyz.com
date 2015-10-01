$!****************************************************************************
$!
$! Build proc for MIPL module lstoxyz
$! VPACK Version 1.9, Tuesday, April 03, 2001, 10:36:00
$!
$! Execute by entering:		$ @lstoxyz
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
$ write sys$output "*** module lstoxyz ***"
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
$ write sys$output "Invalid argument given to lstoxyz.com file -- ", primary
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
$   if F$SEARCH("lstoxyz.imake") .nes. ""
$   then
$      vimake lstoxyz
$      purge lstoxyz.bld
$   else
$      if F$SEARCH("lstoxyz.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake lstoxyz
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @lstoxyz.bld "STD"
$   else
$      @lstoxyz.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create lstoxyz.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack lstoxyz.com -mixed -
	-s lstoxyz.f -
	-i lstoxyz.imake -
	-p lstoxyz.pdf -
	-t tstlstoxyz.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create lstoxyz.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c
c program lstoxyz
c
      include 'VICMAIN_FOR'
      subroutine main44

      include 'mp_for_defs'  !needed for MP software
      character*12 planet/'            '/
      character*5 project(2),source(2)
      character*4 csource1,csource2
      integer*4 fdsno,ilab(100,2),scr(1800),buf(200,2),camera(2)
      integer*4 idata(40,4),tpt,inunit(2),count,def
      real*4 data(40,4),image_coords(4,32)
      real*4 lat1,lat2,long1,long2,line1,line2,samp1,samp2
      real*4 xyz(4,32)
      real*8 buf8(100,2),r8,rs_vector(3,2),om_mat(3,3,2)
      real*8 focal1,focal2,scale1,scale2
      real*8 x1p,y1p,x2p,y2p,mat1(3,3),mat2(3,3)
      real*8 x,y,z,raddeg,range,rad,rpole,req
      real*8 lat,long,latrad
      real*8 MP(2)
      logical xvptst,debug/.false./,xyzmode/.false./
      equivalence (csource1,buf(11,1)),(csource2,buf(11,2))
      equivalence(buf,buf8),(r8,idata,data)      

      raddeg=180.0d0/3.141592653589d0

c open files
      call xvunit(inunit(1),'INP',1,status,' ')
      call xvopen(inunit(1),status,'IO_ACT','AS'
     +            ,'OPEN_ACT','AS',' ')
      call xvget(inunit(1),status,'NL',nl,'NS',np,
     +  'FORMAT',oformat,' ')

      call xvunit(inunit(2),'INP',2,status,' ')
      call xvopen(inunit(2),status,'IO_ACT','AS'
     +            ,'OPEN_ACT','AS',' ')

      call xvunit(inmark,'INP',3,status,' ')
      call xvopen(inmark,status,'IO_ACT','AS','U_FORMAT','REAL'
     +            ,'OPEN_ACT','AS',' ')
      call xvget(inmark,status,'NL',nl3,'NS',ns3,' ')

      call xvunit(outunit,'OUT',1,status,' ')
      call xvopen(outunit,status,'U_FORMAT','REAL','O_FORMAT',
     +            'REAL','OP','WRITE','OPEN_ACT','AS',
     +            'IO_ACT','AS','U_NS',128,'U_NL',nl3*32/42,
     +            ' ')

c Get parameters:
      call xvparm('SOURCEL',source(1),count,def,1)
      call xvparm('SOURCER',source(2),count,def,1)
      if(xvptst('DEBUG')) debug=.true.
      if(xvptst('XYZ')) xyzmode=.true.

c Extract the navigation for the input images and put it in DATA.
c DATA(1-40,1) is for the Map projected first input image.
c DATA(1-40,2) is for the Map projected second input image.
c DATA(1-40,3) is for the object space first input image.
c DATA(1-40,4) is for the object space second input image.
      do image=1,2

c        Get project names
         call getproj(inunit(image),project(image),camera(image),
     +                fdsno,ind)
         if(ind.ne.0)then
            call mabend('Unknown flight project')
         endif
      
c        Get label contents
         call getlabcon(inunit(image),project(image),ilab(1,image),
     +                  ind)
         if(ind.gt.1)then
            call mabend('Getlabcon: Fatal error')
         endif

c        Retrieve the MAP labels from the projections for navigating
c        around in projection space.
c         call searcv2(inunit(image),i,%descr(scr),data(1,image),
c     +                data(1,image))
         call mp_init(mp(image),istat)
         if(istat.ne.mp_success) call mabend('error in mp_init')
         call mp_label_read(mp(image),inunit(image),istat)
         if(istat.ne.mp_success) call mabend('error in mp_read')
         call mp_mpo2buf( mp(image),data(1,image),istat)    !C MP
         if(istat.ne.mp_success) call mabend('error in mpo2buf')
        
c        Check input types
         if((idata(39,image).eq.7).or.(idata(39,image).eq.8))then
            call mabend('Inputs must be map projections')
         endif

c        Retrieve the OM and RS vectors and target radii.
c         if((project(image).eq.'VGR-1').or.
c     +      (project(image).eq.'VGR-2')) planet='            '
c         call getspice(project(image),buf8(1,image),source(image),
c     +                 planet,ilab(1,image),ind)
         call getspice2(inunit(image),.false.,buf8(1,image),ind)
         if(ind.ne.1)then
            write(*,*)'getspice2 indicator:',ind
            call mabend('Getspice2: error ')
         endif
         call mve(8,9,buf8(59,image),data(1,image+2),1,1)
         call mve(8,3,buf8(22,image),data(19,image+2),1,1)
         data(25,image+2)=buf8(15,image)         
         data(26,image+2)=(buf8(14,image)+buf8(13,image))/2.0
         data(38,image+2)=buf8(27,image)         
         idata(39,image+2)=8         !set to object space

c        Get camera constants
         call getcamcon(project(image),camera(image),
     +                  data(27,image+2),data(28,image+2),
     +                  data(29,image+2),data(30,image+2),ind)
         if(ind.ne.0) then
            call qprint('Getcamcon bad ind')
            call abend
         endif

      enddo

c close first 2 inputs.
      call xvclose(inunit(1),ind,' ')
      call xvclose(inunit(2),ind,' ')

c Extract pertinent constants for clarity.
c Focal lengths are negative for the spacecraft data. For
c Manual Of Photogrammetry aircraft data they should be positive.
      focal1=-data(27,3)/1.0d+06 ! focal length camera1 in KM.
      focal2=-data(27,4)/1.0d+06 ! focal length camera2 in KM.
      scale1=data(30,3)*1.0d+06 ! pixels/KM camera1 image plane.
      scale2=data(30,4)*1.0d+06 ! pixels/KM camera2 image plane.
      x1p=data(29,3)/scale1     ! optical axis sample in KM.
      y1p=data(28,3)/scale1     ! optical axis line in KM
      x2p=data(29,4)/scale2     ! optical axis sample in KM
      y2p=data(28,4)/scale2     ! optical axis line in KM
      call mve(8,3,buf8(22,1),rs_vector(1,1),1,1)
      call mve(8,3,buf8(22,2),rs_vector(1,2),1,1)
      call mve(8,9,buf8(59,1),om_mat(1,1,1),1,1)
      call mve(8,9,buf8(59,2),om_mat(1,1,2),1,1)
      do i=1,3
        do j=1,3
          mat1(i,j)=om_mat(j,i,1)
          mat2(i,j)=om_mat(j,i,2)
        enddo
      enddo

c      call qprint('   ')
c      call qprint('Picture 1 project is: '//project(1))
c      call qprint('Navigation source for first input is: '//csource1)
c      call qprint('Picture # 1 RS vector:')
c      call prnt2(8,3,rs_vector(1,1),'XYZ=.')
c      call qprint('Picture # 1 OM matrix:')
c      call prnt2(8,3,om_mat(1,1,1),'row 1=.')
c      call prnt2(8,3,om_mat(1,2,1),'row 2=.')
c      call prnt2(8,3,om_mat(1,3,1),'row 3=.')
c      call qprint('   ')
c      call qprint('Picture 2 project is: '//project(2))
c      call qprint('Navigation source for second input is: '//csource2)
c      call qprint('Picture # 2 RS vector:')
c      call prnt2(8,3,rs_vector(1,2),'XYZ=.')
c      call qprint('Picture # 2 OM matrix:')
c      call prnt2(8,3,om_mat(1,1,2),'row 1=.')
c      call prnt2(8,3,om_mat(1,2,2),'row 2=.')
c      call prnt2(8,3,om_mat(1,3,2),'row 3=.')
c      call qprint('   ')

c     Loop on tiepoint records
      lout=0
      iout=0
      rpole=data(25,3) ! planet polar radius
      req=data(26,3)   ! planet equatorial radius
      do 140 lrec=1,nl3
        call xvread(inmark,image_coords,status,'LINE',lrec,' ')

c       Loop on tiepoints
        do 150 tpt=1,32
          if((image_coords(1,tpt).eq.0.0).and.
     +       (image_coords(2,tpt).eq.0.0)) goto 151

c         Convert map projection line,samp to lat,lon
          call convev(ind,data(1,1),data(1,1),image_coords(1,tpt),
     +                image_coords(2,tpt),lat1,long1,2,scr)
          if(ind.ne.0) goto 151
          call convev(ind,data(1,2),data(1,2),image_coords(3,tpt),
     +                image_coords(4,tpt),lat2,long2,2,scr)
          if(ind.ne.0) goto 151

          if(debug)then
             write(*,*)'left projection: line,samp,->lat,long'
             write(*,99) image_coords(1,tpt),image_coords(2,tpt),
     +                     lat1,long1
             write(*,*)'right projection: line,samp,->lat,long'
             write(*,99) image_coords(3,tpt),image_coords(4,tpt),
     +                     lat2,long2
          endif

c         Convert lat,lon to object space image line,samp
          call convev(ind,data(1,3),data(1,3),line1,samp1,lat1,long1,
     +                1,scr)
          if(ind.ne.0) goto 151
          call convev(ind,data(1,4),data(1,4),line2,samp2,lat2,long2,
     +                1,scr)
          if(ind.ne.0) goto 151

          if(debug)then
             write(*,*)'left object space: lat,long,->line,samp'
             write(*,99) lat1,long1,line1,samp1
             write(*,*)'right object space: lat,long,->line,samp'
             write(*,99) lat2,long2,line2,samp2
          endif

c         convert image coords to Km.
          samp1=samp1/scale1
          line1=line1/scale1
          samp2=samp2/scale2
          line2=line2/scale2

c         convert image coords to xyz in planet coordinates in KM.
          call xvector(mat1,mat2,focal1,focal2,rs_vector(1,1),
     +                rs_vector(1,2),x1p,y1p,x2p,y2p,
     +                dble(samp1),dble(line1),dble(samp2),
     +                dble(line2),x,y,z,error,ind)
          if(ind.ne.0) goto 151

c         update output buffer
          iout=iout+1
          if(xyzmode)then
             xyz(1,iout)=x
             xyz(2,iout)=y
             xyz(3,iout)=z
             xyz(4,iout)=error
          else
             latrad=datan2(z,dsqrt(x*x+y*y)) ! centric latitude
             lat=raddeg*latrad
             long=360.d0-raddeg*datan2(y,x)  ! W longitude
             if(long.gt.360.d0) long=long-360.d0
             range=dsqrt(x*x+y*y+z*z)
             rad=rpole*req/dsqrt(rpole*rpole*(dcos(latrad))**2+
     +                           req*req*(dsin(latrad))**2)
             xyz(1,iout)=lat        ! Geocentric lat. deg.
             xyz(2,iout)=long       ! W long deg.
             xyz(3,iout)=range-rad  ! Elevation above geoid Km.
             xyz(4,iout)=error
          endif


          if(debug)then
             if(xyzmode)then
                write(*,*)'X, Y, Z, error(km)'
                write(*,98) x,y,z,error
             else
                write(*,*)'Lat, Long, Range-Radius, Error(km)'
                write(*,98) lat,long,range-rad,error
             endif
          endif

c         Error handling.
c         Assures a 1:1 correspondence of input & output
          goto 152
151       iout=iout+1
          xyz(1,iout)=0.0
          xyz(2,iout)=0.0
          xyz(3,iout)=0.0
          xyz(4,iout)=0.0
152       continue

c         Occasionally write out a full record.
          if(iout.eq.32) then
             lout=lout+1
             call xvwrit(outunit,xyz,status,'LINE',lout,' ')
             iout=0
          endif

150     continue
140   continue

c write out partially filled buffer.
      if(iout.gt.0) then
         call mve(7,(32-iout)*4,0.0,xyz(1,iout+1),0,1)
         lout=lout+1
         call xvwrit(outunit,xyz,status,'LINE',lout,' ')
      endif

c update nl in the label
      call xldel(outunit,'SYSTEM','NL',status,' ')
      call xladd(outunit,'SYSTEM','NL',lout,status,
     + 'FORMAT','INT',' ')

98    format(3d15.5,e15.5)
99    format(4f10.5)

      return
      END

       subroutine xvector(mat1,mat2,focal1,focal2,cam1,cam2,
     +     x1p,y1p,x2p,y2p,x1,y1,x2,y2,x,y,z,error,ind)
c Convert from image coordinates to xyz coordinates given two
c images forming a stereo pair.
c mat1=rotation matrix for camera 1 
c mat2=rotation matrix for camera 2
c focal1=camera1 focal length
c focal2=camera2 focal length
c cam1=x,y,z object space position of camera 1
c cam2=x,y,z object space position of camera 2
c x1p,y1p= x & y image plane coord of optical axis, camera 1
c x2p,y2p= x & y image plane coord of optical axis, camera 2
c x1,y1= x & y image plane coord of common point of interest, camera 1
c x2,y2= x & y image plane coord of common point of interest, camera 2
c x,y,z= xyz object space coord of object (returned)
c ind=0 OK, ind=1 no solution (returned)
c Reference: Manual of Photogrammetry, page 64.
       implicit real*8 (a-z)
       real*8 mat1(9),mat2(9),cam1(3),cam2(3)
       real*8 a(9),b(3),c(9)
       real*4 error
       integer*4 ind

c compute direction cosines u,v,w for ray1 and ray2
       ind=0
       dx=x1-x1p
       dy=y1-y1p
       u1=mat1(1)*dx+mat1(4)*dy+mat1(7)*(-focal1)
       v1=mat1(2)*dx+mat1(5)*dy+mat1(8)*(-focal1)
       w1=mat1(3)*dx+mat1(6)*dy+mat1(9)*(-focal1)
       d=dsqrt(u1*u1+v1*v1+w1*w1)
       u1=u1/d
       v1=v1/d
       w1=w1/d
       dx=x2-x2p
       dy=y2-y2p
       u2=mat2(1)*dx+mat2(4)*dy+mat2(7)*(-focal2)
       v2=mat2(2)*dx+mat2(5)*dy+mat2(8)*(-focal2)
       w2=mat2(3)*dx+mat2(6)*dy+mat2(9)*(-focal2)
       d=dsqrt(u2*u2+v2*v2+w2*w2)
       u2=u2/d
       v2=v2/d
       w2=w2/d

c solve for x,y,z point on ray1 nearest to ray2
       as=v1*w2-w1*v2
       bs=u2*w1-u1*w2
       cs=u1*v2-v1*u2
       as1=bs*w1-v1*cs
       bs1=u1*cs-as*w1
       cs1=as*v1-u1*bs
       as2=bs*w2-v2*cs
       bs2=u2*cs-as*w2
       cs2=as*v2-u2*bs
       a(1)=as
       a(2)=as1
       a(3)=as2
       a(4)=bs
       a(5)=bs1
       a(6)=bs2
       a(7)=cs
       a(8)=cs1
       a(9)=cs2
       do 10 i=1,9
          c(i)=a(i)
10     continue
       b(1)=as*cam1(1)+bs*cam1(2)+cs*cam1(3)
       b(2)=as1*cam1(1)+bs1*cam1(2)+cs1*cam1(3)
       b(3)=as2*cam2(1)+bs2*cam2(2)+cs2*cam2(3)
       call dsimq(a,b,3,ind)
       x=b(1)
       y=b(2)
       z=b(3)
       if(ind.gt.0) return

c solve for xx,yy,zz point on ray2 nearest to ray1
       b(1)=as*cam2(1)+bs*cam2(2)+cs*cam2(3)
       b(2)=as1*cam1(1)+bs1*cam1(2)+cs1*cam1(3)
       b(3)=as2*cam2(1)+bs2*cam2(2)+cs2*cam2(3)
       call dsimq(c,b,3,ind)
       if(ind.gt.0) return
       xx=b(1)
       yy=b(2)
       zz=b(3)

c point inbetween is the closest approach point to both vectors
       error=dsqrt((z-zz)**2+(y-yy)**2+(x-xx)**2)
       x=(x+xx)/2.d0
       y=(y+yy)/2.d0
       z=(z+zz)/2.d0
       return
       end

      SUBROUTINE DSIMQ(A,B,N,KS)
C        USAGE
C           CALL DSIMQ(A,B,N,KS)
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
$ Return
$!#############################################################################
$Imake_File:
$ create lstoxyz.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM lstoxyz

   To Create the build file give the command:

		$ vimake lstoxyz			(VMS)
   or
		% vimake lstoxyz			(Unix)


************************************************************************/


#define PROGRAM	lstoxyz

#define MODULE_LIST lstoxyz.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define USES_ANSI_C

#define FTNINC_LIST mp_for_defs  
#define R2LIB

#define LIB_FORTRAN
#define LIB_SPICE
#define LIB_NETWORK
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_MATH77
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create lstoxyz.pdf
process help=*
PARM INP          TYPE=STRING       COUNT=3
PARM OUT          TYPE=STRING       COUNT=1
PARM SOURCEL TYPE=(STRING,4) COUNT=(0:1) +
 VALID=("DAVI ","NAV  ","FARE ","NAV2 ","NEAR ","AMOS ","NTEL ", +
        "AACS ","NPRE ","NAIF ","SEDR ") DEFAULT=--
PARM SOURCER TYPE=(STRING,4) COUNT=(0:1) +
 VALID=("DAVI ","NAV  ","FARE ","NAV2 ","NEAR ","AMOS ","NTEL ", +
        "AACS ","NPRE ","NAIF ","SEDR ") DEFAULT=--
PARM DEBUG TYPE=KEYWORD VALID=(DEBUG,NODEBUG) DEFAULT=NODEBUG
PARM XYZ   TYPE=KEYWORD VALID=(XYZ,LATLON) DEFAULT=LATLON
END-PROC

.TITLE
VICAR2 program LSTOXYZ

.HELP
Converts the tiepoint locations stored in a Mark file and written by
program TRACKER either to:

 X,Y,Z,Error   coordinates in the planet reference frame
or
 Lat,Long,Range-radius,error   coordinates on the planet

and writes them into another Mark file. The Mark file can be converted
into a topomap by program TOTOPO.

This program does ranging on two flight project images with 
high quality navigation and generates XYZ coordinates in Km.
in the planet reference frame. It is a member of the MIPL
stereo program suite.

If you have no camera models or spice pointing you can use TOPOMAP
instead to generate relative topography.

.page
Example:
       LSTOXYZ left,right,points out parameters

  where: 
         LEFT is the first input image. This is the first input image
              to program TRACKER. In this case it must be a map projected
              image.
         RIGHT is the second input image. This is the second input image
              to program TRACKER. The second input must also be a map
              projection preferably the identical projection as the first
              input image (non-identical projections are OK except that
              TRACKER will work better if they are identical).
         POINTS is a MARK file containing the tiepoints written by
              program tracker.
              Mark files contain 512 byte records of real*4 data.
              Coordinates are stored in groups of 4 words in the
              order left_line,left_sample,right_line,right_sample...
         OUT is a Mark file containing either:
                  The groups of x,y,z,error
                  values computed by program LSTOXYZ.
                  (see keyword XYZ)
            or
                  The groups of latitude,longitude,range-radius,error
                  values computed by program LSTOXYZ.
                  (the default)
              Mark files contain 512 byte records of real*4 data.
              Coordinates are stored in groups of 4 words in the
              order X,Y,Z,ERROR,X,Y,Z,ERROR...
                 or LAT,LON,RANGE-RADIUS,ERROR...

.page
Important note to users of the output file:
There is a 1:1 correspondence between input Mark and output Mark files.
If an input coordinate is zero then ignore that input and output.
If an input coordinate is not zero but the output coordinate is
zero then the input could not be processed for some reason so 
ignore both input & output for that point. The correspondence
assures that the input line,samp coordinates match the output XYZ's.

Outputs can be zero because:
1. input was zero.
2. the mapping transformation placed a point off or behind the planet.
3. the 3-space vectors were parallel.

.page
METHOD:
LSTOXYZ performs the following steps:
1. Gets the transformation for both input projections (line,samp ->lat,lon)
2. Gets the transformation for both object space images(lat,lon->line,samp)
  For each tiepoint:
3. Convert line,samp to lat,lon in projection space.
4. Convert lat,lon to line,samp in original image object space.
5. Construct two vectors from the spacecraft positions towards
   those line,samp coordinates in the image plane.
6. Compute the closest approach point between the two vectors.
   This is the X,Y,Z value.
7. The miss distance is the 4th value ERROR.

Note the Spice gives the RS vector (s/c position in planet coords)
and the OM matrix (rotation of planet to camera coords).

See Manual Of Photogrammetry , Sec Ed, P50.

.page
OUTPUT UNITS:

  XYZ values are in Kilometers in the planet reference frame.
    Z is the polar spin axis
    X & Y form the equatorial plane.
    X pierces the geoid at the prime meridian (0 longitude).

  LAT is in degrees planetocentric.
  LONG is in degrees West.
  RANGE-RADIUS is in kilometers. It is the difference between the
    range from the XYZ point to planet center and the local
    planetocentric radius of the geoid at that latitude.

  The fourth value (error) is the closest approach point of the
  two vectors in Km. It is NOT the error in elevation.

.page
EXAMPLE: 

         farenc in1
         farenc in2
         map3 in1 a
         map3 in2 b
         tracker a,b m
        LSTOXYZ a,b,m xyz 'XYZ

HISTORY
Written By: J Lorre			jan 91
Cognizant Programmer: J Lorre

.LEVEL1
.VARI INP
Three inputs:
#1 is the left image
#2 is the right image
#3 is the MARK file.

.VARI OUT
Mark file output
containing xyz coords.
or lat,lon's

.VARI SOURCEL
The source of the 
SEDR/SPICE for the 
left image.

.VARI SOURCER
The source of the 
SEDR/SPICE for the 
right image.

.VARI DEBUG
Prints debugging data.

.VARI XYZ
Specifies the output
will contain  
(X,Y,Z,error)

.LEVEL2

.VARI INP
There are three input files.
File#1: This is the left or first input image given to
        program TRACKER.
File#2: This is the right or second input image given to
        program TRACKER.
File#3: This is the MARK file written by program TRACKER.
        It is 'REAL' format with 512 byte records containing
        pairs of tiepoints in the order:
        left_line,left_samp,right_line,right_samp.

.VARI OUT
The output file is a Mark file containing the XYZ,error values
or the lat,lon,range-radius,error values
corresponding to each input line,samp,line,samp quartet of
tiepoints computed by TRACKER. There are 32 sets of values
per record. All are REAL*4.

.VARI SOURCEL
The source of the SEDR/SPICE for the left image.
 VALID="DAVI ","NAV  ","FARE ","NAV2 ","NEAR ","AMOS ","NTEL ",
        "AACS ","NPRE ","NAIF ","SEDR "

.VARI SOURCER
The source of the SEDR/SPICE for the right image.
 VALID="DAVI ","NAV  ","FARE ","NAV2 ","NEAR ","AMOS ","NTEL ",
        "AACS ","NPRE ","NAIF ","SEDR "

.VARI DEBUG
Prints the line,sample and lat,long for each point in the input
Mark file as it is processed.

.VARI XYZ
Specifies that the output Mark file will contain the (X,Y,Z,error)
values in planet coordinates rather than the
 (lat,long,range-radius,error) values which are the default.

$ Return
$!#############################################################################
$Test_File:
$ create tstlstoxyz.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
!
!FARENC INP=moon2.img OUT=g2.img +
! 'update below=10 dnth=10 activi=35 GEOM=3 +
!  area=(3,2,790,790) cluster=(30,10)
!FARENC INP=moon1.img OUT=g1.img +
! 'update below=10 dnth=10 activi=45 GEOM=3 +
!  area=(3,2,790,790)
!map3 INP=moon2.img OUT=moon2.map +
!  'MERC nl=512 ns=512 sedrsrc=FARE scale=10. +
!   line=256 samp=256 lat=0 long=70.
!map3 INP=moon1.img OUT=moon1.map +
!  'MERC nl=512 ns=512 sedrsrc=FARE scale=10. +
!   line=256 samp=256 lat=0 long=70.
!
! try select o of lstoxyz fails
! short test
genthis out=testmark.img nl=1 ns=20 format=real4 +
      dn=(272.,262.,272.,262. +
      200.,200.,200.,200., 200.,300.,200.,300., +
      300.,200.,300.,200., 300.,300.,300.,300.)
LSTOXYZ inp=( +
  /project/test_work/testdata/sitod1/test_data/images/moon1.map, +
  /project/test_work/testdata/sitod1/test_data/images/moon2.map, +
  testmark.img) out=xyz.img +
  sourcel=FARE sourcer=FARE 'debug
list xyz.img
! VERY LONG TEST
tracker3 inp=( +
  /project/test_work/testdata/sitod1/test_data/images/moon1.map, +
  /project/test_work/testdata/sitod1/test_data/images/moon2.map) +
  out=mark.img nlw=31 nsw=31 nlarea=37 nsarea=37 quality=.5 +
  'zero grid=3
LSTOXYZ inp=( +
  /project/test_work/testdata/sitod1/test_data/images/moon1.map, +
  /project/test_work/testdata/sitod1/test_data/images/moon2.map, +
  mark.img) out=xyz.img +
  sourcel=FARE sourcer=FARE
!
end-proc
$ Return
$!#############################################################################
