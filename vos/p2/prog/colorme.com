$!****************************************************************************
$!
$! Build proc for MIPL module colorme
$! VPACK Version 1.8, Friday, June 20, 1997, 13:52:00
$!
$! Execute by entering:		$ @colorme
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
$ write sys$output "*** module colorme ***"
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
$ write sys$output "Invalid argument given to colorme.com file -- ", primary
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
$   if F$SEARCH("colorme.imake") .nes. ""
$   then
$      vimake colorme
$      purge colorme.bld
$   else
$      if F$SEARCH("colorme.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake colorme
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @colorme.bld "STD"
$   else
$      @colorme.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create colorme.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack colorme.com -
	-s colorme.f -
	-i colorme.imake -
	-p colorme.pdf -
	-t tstcolorme.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create colorme.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c
c program colorme
c
      include 'VICMAIN_FOR'
      subroutine main44

      integer ntable,maxsamp,ndevices,nnearest
      parameter (ntable=1000,maxsamp=20000,ndevices=13)

c maxbins= max number of bins/dimension in the Luv cube.
c ntable= max number of device calibration table entries
c maxsamp= max line length
c ndevices= number of supported devices

      character*100 path,filename,msg
      character*15 device,monitor,caldevices(ndevices)
      character*15 calnames_left(ndevices),calnames_right(ndevices)
      integer*2 red(maxsamp),green(maxsamp),blue(maxsamp)
      integer*4 ounit(3),def,count,nearpts(ndevices)
      integer*4 unit(3),status,nl,ns,thresh,area(4)
      real*4 r(ntable),g(ntable),b(ntable),nearest(30)
      real*4 L(ntable),u(ntable),v(ntable),range(ntable)
      real*4 x(ntable),y(ntable),yy(ntable)
      real*8 coefr(4)
      real*8 coefg(4)
      real*8 coefb(4)
      real*4 targetxyy(3)
      real*8 c(30,4),cl(30),vv(30),ex(4),wts(30),er,eg,eb
      real*8 sum_red,sum_green,sum_blue

      data caldevices/'mda','fujix','alps','temblor','wasatch',
     +  'kodak168','kodak230','asutek','codonics','csi_1',
     +  'ntsc','hdtv','octane'/
      data calnames_left/'mda','fjx','alt','temlt','wlt',
     +  'xl168','xl230','asutek','codonics','csi_1',
     +  'ntsc','hdtv','octane'/
      data calnames_right/'mda','fjx','art','temrt','wrt',
     +  'xl168','xl230','asutek','codonics','csi_1',
     +  'ntsc','hdtv','octane'/
      data nearpts/8,8,8,8,8,8,8,8,8,8,8,8,8/

c parameters
      call xvparm('PATH',path,count,def,1)
      call xvparm('DEVICE',device,count,def,1)
      call xvparm('MONITOR',monitor,count,def,1)
      call xvparm('THRESH',thresh,count,def,1)
      call xvparm('XYY',targetxyy,count,def,3)
      call xvparm('AREA',area,count,def,4)

c checks
      call xveaction('SA',' ')
      call xvpcnt('INP',nin)
      if(nin.eq.0)then
        call xvmessage('No inputs',' ')
        call abend()
      endif

c build calibration filename
      k=0
      do i=1,ndevices
        if(device.eq.caldevices(i)) k=i
      enddo
      if(k.eq.0) call mabend('unrecognized device')
      do i=1,100
        filename(i:i)=' '
      enddo
      do i=1,100
        if(path(i:i).ne.' ')then
           filename(i:i)=path(i:i)
           j=i
        else
           goto 1
        endif
      enddo
1     if(filename(j:j).ne.'/')then
        j=j+1
        filename(j:j)='/'
      endif
      if(monitor.eq.'right')then
        do i=1,15
          if(calnames_right(k)(i:i).ne.' ')then
            j=j+1
            filename(j:j)=calnames_right(k)(i:i)
          else
            goto 2
          endif
        enddo
2       continue
      else
        do i=1,15
          if(calnames_left(k)(i:i).ne.' ')then
            j=j+1
            filename(j:j)=calnames_left(k)(i:i)
          else
            goto 3
          endif
        enddo
3       continue
      endif

c get NEAREST number of points to use in least squares fit
      nnearest=nearpts(k)
      call xvparm('NEAREST',nnearest,count,def,1)
      write(msg,*)'Using nearest ',nnearest,' points for fitting'
      call xvmessage(msg,' ')

      ntab=0
      n=ntab
c read color table for display device
      filename(j+1:j+2)='.2'
      call xvmessage('reading color table '//filename,' ')
      call read_table(filename,x,y,yy,r,g,b,ntable,maxsamp,ntab,2)
      write(msg,*)ntab-n,' points located in color table'
      call xvmessage(msg,' ')

c convert xyY in table to Luv
      do k=1,ntab
        call xyytoluv(x(k),y(k),yy(k),L(k),u(k),v(k),ind)
        if(ind.eq.1)call mabend('xyytoluv ind=1')
      enddo

c convert xyY in target parameter to Luv
      call xyytoluv(targetxyy(1),targetxyy(2),targetxyy(3),
     + rrL,rru,rrv,ind)
        if(ind.eq.1)call mabend('xyytoluv ind=1')
      write(msg,*)'Target Luv=',rrL,rru,rrv
      call xvmessage(msg,' ')

c Compute the rgb value from the target xyy value

c           determine the nearest N table points to our Luv coordinate
            do k=1,ntab
              range(k)=(rrl-L(k))**2+(rru-u(k))**2+(rrv-v(k))**2
            enddo

            n=0
200         continue
c           quadrant 1
            small=1.0e+20
            m=0
            do k=1,ntab
              if(small.gt.range(k))then
                if((rrl.lt.L(k)).and.(rru.lt.u(k)).and.(rrv.lt.v(k)))
     +            then
                    m=k
                    small=range(k)
                endif
              endif
            enddo
            if(m.gt.0)then
              n=n+1
              nearest(n)=m
              wts(n)=10.0/(sqrt(range(m))+1.0)  ! weighting
              range(m)= 1.0e+20 ! so won't find again
              if(n.eq.nnearest)goto 201
            endif

c           quadrant 2
            small=1.0e+20
            m=0
            do k=1,ntab
              if(small.gt.range(k))then
                if((rrl.ge.L(k)).and.(rru.lt.u(k)).and.(rrv.lt.v(k)))
     +            then
                    m=k
                    small=range(k)
                endif
              endif
            enddo
            if(m.gt.0)then
              n=n+1
              nearest(n)=m
              wts(n)=10.0/(sqrt(range(m))+1.0)  ! weighting
              range(m)= 1.0e+20 ! so won't find again
              if(n.eq.nnearest)goto 201
            endif

c           quadrant 3
            small=1.0e+20
            m=0
            do k=1,ntab
              if(small.gt.range(k))then
                if((rrl.lt.L(k)).and.(rru.ge.u(k)).and.(rrv.lt.v(k)))
     +            then
                    m=k
                    small=range(k)
                endif
              endif
            enddo
            if(m.gt.0)then
              n=n+1
              nearest(n)=m
              wts(n)=10.0/(sqrt(range(m))+1.0)  ! weighting
              range(m)= 1.0e+20 ! so won't find again
              if(n.eq.nnearest)goto 201
            endif

c           quadrant 4
            small=1.0e+20
            m=0
            do k=1,ntab
              if(small.gt.range(k))then
                if((rrl.ge.L(k)).and.(rru.ge.u(k)).and.(rrv.lt.v(k)))
     +            then
                    m=k
                    small=range(k)
                endif
              endif
            enddo
            if(m.gt.0)then
              n=n+1
              nearest(n)=m
              wts(n)=10.0/(sqrt(range(m))+1.0)  ! weighting
              range(m)= 1.0e+20 ! so won't find again
              if(n.eq.nnearest)goto 201
            endif

c           quadrant 5
            small=1.0e+20
            m=0
            do k=1,ntab
              if(small.gt.range(k))then
                if((rrl.lt.L(k)).and.(rru.lt.u(k)).and.(rrv.ge.v(k)))
     +            then
                    m=k
                    small=range(k)
                endif
              endif
            enddo
            if(m.gt.0)then
              n=n+1
              nearest(n)=m
              wts(n)=10.0/(sqrt(range(m))+1.0)  ! weighting
              range(m)= 1.0e+20 ! so won't find again
              if(n.eq.nnearest)goto 201
            endif

c           quadrant 6
            small=1.0e+20
            m=0
            do k=1,ntab
              if(small.gt.range(k))then
                if((rrl.ge.L(k)).and.(rru.lt.u(k)).and.(rrv.ge.v(k)))
     +            then
                    m=k
                    small=range(k)
                endif
              endif
            enddo
            if(m.gt.0)then
              n=n+1
              nearest(n)=m
              wts(n)=10.0/(sqrt(range(m))+1.0)  ! weighting
              range(m)= 1.0e+20 ! so won't find again
              if(n.eq.nnearest)goto 201
            endif

c           quadrant 7
            small=1.0e+20
            m=0
            do k=1,ntab
              if(small.gt.range(k))then
                if((rrl.lt.L(k)).and.(rru.ge.u(k)).and.(rrv.ge.v(k)))
     +            then
                    m=k
                    small=range(k)
                endif
              endif
            enddo
            if(m.gt.0)then
              n=n+1
              nearest(n)=m
              wts(n)=10.0/(sqrt(range(m))+1.0)  ! weighting
              range(m)= 1.0e+20 ! so won't find again
              if(n.eq.nnearest)goto 201
            endif

c           quadrant 8
            small=1.0e+20
            m=0
            do k=1,ntab
              if(small.gt.range(k))then
                if((rrl.ge.L(k)).and.(rru.ge.u(k)).and.(rrv.ge.v(k)))
     +            then
                    m=k
                    small=range(k)
                endif
              endif
            enddo
            if(m.gt.0)then
              n=n+1
              nearest(n)=m
              wts(n)=10.0/(sqrt(range(m))+1.0)  ! weighting
              range(m)= 1.0e+20 ! so won't find again
              if(n.eq.nnearest)goto 201
            endif

            if(n.lt.nnearest)goto 200
201         continue

c           compute red polynomial
            do k=1,nnearest
              c(k,1)=L(nearest(k))
              c(k,2)=u(nearest(k))
              c(k,3)=v(nearest(k))
              c(k,4)=1.d0
              cl(k)=r(nearest(k))
            enddo
            call dlsqp(nnearest,4,c,cl,coefr,vv,er,ex,wts,ind)
            if(ind.ne.0)then
              call mabend('red lsqp solution error')
            endif

c           compute green polynomial
            do k=1,nnearest
              cl(k)=g(nearest(k))
            enddo
            call dlsqp(nnearest,4,c,cl,coefg,vv,eg,ex,wts,ind)
            if(ind.ne.0)call mabend('green lsqp solution error')

c           compute blue polynomial
            do k=1,nnearest
              cl(k)=b(nearest(k))
            enddo
            call dlsqp(nnearest,4,c,cl,coefb,vv,eb,ex,wts,ind)
            if(ind.ne.0)call mabend('blue lsqp solution error')

c         compute desired mean output rgb values
          target_red=coefr(1)*rrl+coefr(2)*rru+
     +        coefr(3)*rrv+coefr(4)
          target_green=coefg(1)*rrl+coefg(2)*rru+
     +        coefg(3)*rrv+coefg(4)
          target_blue=coefb(1)*rrl+coefb(2)*rru+
     +        coefb(3)*rrv+coefb(4)
          write(msg,*)'Target rgb=',target_red,target_green,target_blue
          call xvmessage(msg,' ')

c open all inputs
      do i=1,nin
        call xvunit(unit(i),'INP',i,status,' ')
        call xvopen(unit(i),status,'U_FORMAT','HALF',' ')
        call xvget(unit(i),status,'NL',nl,'NS',ns,' ')
        if(ns.gt.maxsamp)then
          call xvmessage('Line length too long',' ')
          call abend
        endif
      enddo

c open all outputs
      do i=1,3
        call xvunit(ounit(i),'OUT',i,status,' ')
        call xvopen(ounit(i),status,'O_FORMAT','BYTE','U_FORMAT','HALF',
     +              'U_NL',nl,'U_NS',ns,'OP','WRITE',' ')
      enddo

c set size field
      if(area(1).eq.0)then
        area(1)=1
        area(2)=1
        area(3)=nl
        area(4)=ns
      endif
      if(area(1).lt.1)area(1)=1
      if(area(1).gt.nl)area(1)=nl
      if(area(2).lt.1)area(2)=1
      if(area(2).gt.ns)area(2)=ns
      if(area(3).lt.1)area(3)=1
      if(area(3)+area(1)-1.gt.nl)area(3)=nl-area(1)+1
      if(area(4).lt.1)area(4)=1
      if(area(4)+area(2)-1.gt.ns)area(4)=ns-area(2)+1

c get the input image means
      j=0
      sum_red=0.0
      sum_green=0.0
      sum_blue=0.0
      do line=area(1),area(1)+area(3)-1                         ! line loop
        call xvread(unit(1),red,status,'LINE',line,' ')
        if(nin.gt.1)then
          call xvread(unit(2),green,status,'LINE',line,' ')
        endif
        if(nin.gt.2)then
          call xvread(unit(3),blue,status,'LINE',line,' ')
        endif

        if(nin.eq.1)then
          do k=area(2),area(2)+area(4)-1
            green(k)=red(k)
            blue(k)=red(k)
          enddo
        endif

        if(nin.eq.2)then
          do k=area(2),area(2)+area(4)-1
            blue(k)=green(k)
            green(k)=(red(k)+blue(k))/2
          enddo
        endif

        do i=area(2),area(2)+area(4)-1                       ! pixel loop
          if((red(i).gt.thresh).and.(green(i).gt.thresh).and.
     +       (blue(i).gt.thresh))then
            if((red(i).ne.32767).and.(green(i).ne.32767).and.
     +         (blue(i).ne.32767))then
              j=j+1
              sum_red=sum_red+red(i)
              sum_green=sum_green+green(i)
              sum_blue=sum_blue+blue(i)
            endif
          endif
        enddo   ! pixel loop

      enddo     ! line loop

c compute scaling factors
      rf=target_red/(sum_red/j)
      gf=target_green/(sum_green/j)
      bf=target_blue/(sum_blue/j)
      write(msg,*)'Input means rgb=',sum_red/j,sum_green/j,sum_blue/j
      call xvmessage(msg,' ')

c process images
      nsatr=0
      nsatg=0
      nsatb=0
      do line=1,nl                         ! line loop
        call xvread(unit(1),red,status,'LINE',line,' ')
        if(nin.gt.1)then
          call xvread(unit(2),green,status,'LINE',line,' ')
        endif
        if(nin.gt.2)then
          call xvread(unit(3),blue,status,'LINE',line,' ')
        endif

        if(nin.eq.1)then
          do k=1,ns
            green(k)=red(k)
            blue(k)=red(k)
          enddo
        endif

        if(nin.eq.2)then
          do k=1,ns
            blue(k)=green(k)
            green(k)=(red(k)+blue(k))/2
          enddo
        endif

        do i=1,ns                       ! pixel loop

c         compute rgb values
          scrr=red(i)*rf
          scrg=green(i)*gf
          scrb=blue(i)*bf
          bigdn=max(scrr,scrg,scrb)
          if(bigdn.gt.255.)then
            if(scrr.gt.255.0)nsatr=nsatr+1
            if(scrg.gt.255.0)nsatg=nsatg+1
            if(scrb.gt.255.0)nsatb=nsatb+1
            scrr=scrr*255./bigdn
            scrg=scrg*255./bigdn
            scrb=scrb*255./bigdn
          endif
          red(i)=nint(scrr)
          green(i)=nint(scrg)
          blue(i)=nint(scrb)

        enddo   ! pixel loop
        call xvwrit(ounit(1),red,status,' ')
        call xvwrit(ounit(2),green,status,' ')
        call xvwrit(ounit(3),blue,status,' ')
      enddo     ! line loop

      write(msg,*)nsatr,' red saturations'
      call xvmessage(msg,' ')
      write(msg,*)nsatg,' green saturations'
      call xvmessage(msg,' ')
      write(msg,*)nsatb,' blue saturations'
      call xvmessage(msg,' ')

      end

c*******************************************************************
      subroutine read_table(filename,x,y,yy,r,g,b,ntable,maxsamp,k,n)
      real*4 r(ntable),g(ntable),b(ntable)
      real*4 x(maxsamp),y(maxsamp),yy(maxsamp)
      character*100 filename

      open(unit=10,file=filename,access='SEQUENTIAL',
     + form='FORMATTED',iostat=ios,status='OLD')
      if(ios.gt.0)goto 6

      do i=1,5-n
        read(unit=10,fmt=*,iostat=ios)
        if(ios.gt.0)then
          call xvmessage('read error on headers',' ')
          goto 1
        endif
      enddo

3     k=k+1
      if(k.gt.ntable)goto 4
      read(unit=10,fmt=*,iostat=ios) i,yy(k),x(k),y(k),r(k),g(k),b(k)
      if((abs(r(k)-255.).lt..01).and.(abs(g(k)-255.).lt..01).and.
     +   (abs(b(k)-255.).lt..01))goto 2
      if(ios.eq.0)goto 3
      if(ios.lt.0)then  ! EOF
        k=k-1
        return
      endif

      call xvmessage('read error on table file',' ')
      goto 1
6     call xvmessage('cannot open table file',' ')
      goto 1
4     call xvmessage('Too many table entries',' ')
1     call mabend('Cannot read calibration file')
2     close(unit=10)
      return
      end
c******************************************************************
      subroutine xyytoluv(x,y,yy,L,u,v,ind)
      implicit real*4(a-z)
      integer*4 ind
      ind=0
      if(yy.le..00001)then
        L=0.
        u=0.
        v=0.
        ind=1
        return
      endif      
      ty=yy
      tx=x*ty/y
      tz=(tx-x*tx-x*ty)/x
      yoy=ty/100.
      if(yoy.gt..008856)then
        L=116.0*(yoy**.33333) - 16.0
      else
        L=903.3*yoy
      endif
      temp=tx+15.0*ty+3.0*tz
      up=4.0*tx/temp
      vp=9.0*ty/temp
      u=13.0*L*(up-0.2105263d0)
      v=13.0*L*(vp-0.4736842d0)
      return
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
      parameter (s=4,ss=30)
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
c      DO 160 I=1,NU
c        IF ( Q(I,I) .GE. 0.D0 ) THEN
c          EX(I)=E*DSQRT(Q(I,I))
c        ELSE
c          EX(I)= 0.0                ! HANDLE NEGATIVES DUE TO ROUNDOFF.
c        END IF
c160   CONTINUE      
      RETURN
      END
 
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create colorme.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM colorme

   To Create the build file give the command:

		$ vimake colorme			(VMS)
   or
		% vimake colorme			(Unix)


************************************************************************/


#define PROGRAM	colorme
#define R2LIB

#define MODULE_LIST colorme.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create colorme.pdf
process help=*
PARM INP TYPE=STRING COUNT=(0:3) DEFAULT=--
PARM OUT TYPE=STRING COUNT=3
PARM PATH  TYPE=STRING COUNT=1 +
   DEFAULT="/usr/local/share/calibration/"
!/home/cib/
PARM DEVICE  TYPE=STRING COUNT=1 +
  VALID=("mda","fujix","alps","temblor","wasatch","kodak168", +
  "kodak230","asutek","codonics","csi_1","ntsc","hdtv","octane", +
  "none") DEFAULT="none"
PARM MONITOR  TYPE=STRING COUNT=1 +
  VALID=("left","right","none") DEFAULT="none"
PARM NEAREST TYPE=INTEGER COUNT=(0:1) VALID=(5:30) DEFAULT=--
PARM THRESH TYPE=INTEGER COUNT=(0:1) DEFAULT=0
PARM XYY TYPE=REAL COUNT=3 DEFAULT=(.44,.4,30.) +
  VALID=(0.:1.,0.:1.,0.:100.)
PARM AREA TYPE=INTEGER COUNT=(0,4) DEFAULT=(0,0,0,0)
END-PROC

.TITLE
VICAR program colorme

.HELP
PURPOSE:
To convert uncalibrated rgb images into rgb images whose mean color is 
specified via a parameter.

EXECUTION:
typically:
colorme inp=(r,g,b) out=(ro,go,bo) device=alps monitor=left xyy=(.44,.4,35.)
or:
colorme inp=(r,b) out=(ro,go,bo) device=alps monitor=left xyy=(.44,.4,35.)
or:
colorme inp=(g) out=(ro,go,bo) device=alps monitor=left xyy=(.44,.4,35.)

WARNING:
The program lists the number of saturations. Every saturated pixel
is adjusted down in intensity to preserve the initial color balance.
To avoid this reduce the target Y tristimulus value (the
third value you specified with the XYY keyword) and try it again.
Y represents the scene brightness. 

.PAGE

METHOD:
   The program first computes the mean input rgb values above zero.
It then computes the target rgb values which resulted from the 
desired parameter xyY color for the specified device.
It then computes linear factors mapping the input dn's to the output such
that the mean input rgb's map to the target rgb's.

   The program reads in a calibration file for the specified device. This is 
an ascii file with data in records like this:
STEP#                Yn      x       y       R-DN    G-DN    B-DN
1               0.122616  0.278170  0.298730   0.0   0.0   0.0
2               1.267030  0.595390  0.329050  64.0   0.0   0.0
3               5.354223  0.627000  0.332000  128.0   0.0   0.0
4               12.874659  0.632610  0.333000  192.0   0.0   0.0

The first 3 records are skipped by the program so the first record read is:
1               0.122616  0.278170  0.298730   0.0   0.0   0.0

These correspond to a calibration giving the xyY values for a data cube of
all combinations of rgb dn values in some coarse interval like 32 or 64
dn steps. The file must end with a .2

   The method used is to create a 40 by 40 by 40 cube in Luv coordinates
spanning the range of xyY values in the calibration file. Each input pixel
in xyY is converted to Luv space and compared with the cube to see if
a mapping polynomial exists there. If it exists then the Luv value is
converted to rgb using the polynomial.
r=AL+Bu+Cv+D
g=EL+Fu+Gv+H
b=IL+Ju+Kv+M
If no polynomial exists it is computed and stored into the cube.
Equations (as above) are solved by least squares using the 8 nearest Luv
points (in the cube, one in each octant) to the cube coordinate.

Note:
This is not rigorous color processing. It is to be used when you don't know
anything but the average color of the result.

HISTORY:
6-30-96  J Lorre. 
COGNIZANT PROGRAMMER:  Jean Lorre

.LEVEL1
.VARI INP
1-3 input images
ordered as r g b.

.VARI OUT
3 Output images

.VARI DEVICE
Output device name

.VARI PATH
Path name for 
calibration files.

.VARI MONITOR
Whether the monitor is
left or right.

.VARI NEAREST
Nearest # points
for fitting

.VARI THRESH
Threshold for computing
mean input dn's.
dn_in > thresh.

.VARI XYY
target output color.
x is x chromaticity.
y is y chromaticity.
Y is Luminance Y

.VARI AREA
size field from which
to compute histogram

.LEVEL2
.VARI INP
One to Three input images in the order:
R G B
If only two are specified they represent R and B, G=(R+B)/2.
If only one is specified it represents G. R=G=B.

.VARI OUT
Three Output RGB images in the order:
1. Red dn value in BYTE format.
2. Green dn value in BYTE format.
3. Blue dn value in BYTE format.
These images are in DEVICE calibrated coordinates.

.VARI DEVICE
Target device name upon which the output RGB images are to be 
displayed.
  VALID=("mda","fujix","alps","temblor","wasatch","kodak168", +
  "kodak230","asutek","codonics","csi_1","ntsc","hdtv","octane", +
  "none") DEFAULT="none"
Defaults to none.

.VARI PATH
Path name for the location of color calibration files for the DEVICE.
Defaults to /usr/local/share/calibration/
Note the ending slash !

.VARI MONITOR
Whether the monitor is the left or the right one.
Specify left or right. Defaults to none.
Only used if the device is a TV monitor.
  VALID=("left","right","none") DEFAULT="none"

.VARI NEAREST
The nearest number of points in the color space to use for each least
squares fit mapping rgb dn values to Luv color coordinates. Defaults to
an internal table which is device dependent.
At the moment all devices use 8 points.
More points produces steadier colors but they are less accurate.

.VARI THRESH
Only dn values greater than thresh are used in computing the mean brightness
of each of the input images. Defaults to 0.
Useful if you have a background above zero which is unrelated to the image.

.VARI XYY
The target output color. Specify 3 numbers: x y Y.
x is x chromaticity.
y is y chromaticity.
Y is Luminance Y
Default is a color selected by the Viking Imaging Team for Mars.

.VARI AREA
size field from which to compute histogram in each image.
Defaults to entire input image.
$ Return
$!#############################################################################
$Test_File:
$ create tstcolorme.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
!
gen out=aa.img nl=100 ns=100 linc=0 sinc=0 ival=100
gen out=bb.img nl=100 ns=100 linc=0 sinc=0 ival=90
gen out=cc.img nl=100 ns=100 linc=0 sinc=0 ival=80
colorme inp=(aa.img,bb.img,cc.img) out=(r.img,g.img,b.img) device=alps +
 monitor=left xyy=(.44,.4,35.)
list inp=r.img linc=10 sinc=10
list inp=g.img linc=10 sinc=10
list inp=b.img linc=10 sinc=10
colorme inp=(aa.img,bb.img) out=(r.img,g.img,b.img) device=alps +
 monitor=left xyy=(.44,.4,35.)
list inp=r.img linc=10 sinc=10
list inp=g.img linc=10 sinc=10
list inp=b.img linc=10 sinc=10
colorme inp=(aa.img) out=(r.img,g.img,b.img) device=alps +
 monitor=left xyy=(.44,.4,35.)
list inp=r.img linc=10 sinc=10
list inp=g.img linc=10 sinc=10
list inp=b.img linc=10 sinc=10
!xvd (earth.red,earth.green,earth.blue)
!colorme inp=(earth.red,earth.green,earth.blue) +
! out=(r.img,g.img,b.img) device=alps +
! monitor=left xyy=(.44,.4,25.) area=(550,350,50,50)
!xvd inp=(r.img,g.img,b.img)
!xvd inp=(/home/rdb/mars_norm.r,/home/rdb/mars_norm.g, +
! /home/rdb/mars_norm.b)
!
end-proc
$ Return
$!#############################################################################
