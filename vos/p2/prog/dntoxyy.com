$!****************************************************************************
$!
$! Build proc for MIPL module dntoxyy
$! VPACK Version 1.9, Thursday, April 05, 2012, 14:29:32
$!
$! Execute by entering:		$ @dntoxyy
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
$ write sys$output "*** module dntoxyy ***"
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
$ write sys$output "Invalid argument given to dntoxyy.com file -- ", primary
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
$   if F$SEARCH("dntoxyy.imake") .nes. ""
$   then
$      vimake dntoxyy
$      purge dntoxyy.bld
$   else
$      if F$SEARCH("dntoxyy.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake dntoxyy
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @dntoxyy.bld "STD"
$   else
$      @dntoxyy.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create dntoxyy.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack dntoxyy.com -mixed -
	-s dntoxyy.f -
	-i dntoxyy.imake -
	-p dntoxyy.pdf -
	-t tstdntoxyy.pdf tstdntoxyy.log_solos
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create dntoxyy.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c
c program dntoxyy
c
      include 'VICMAIN_FOR'
      subroutine main44

      parameter (maxns=30000, maxpix=10, maxcolors=50)

      integer*4 inunit(maxpix),outunit(3),def,nl,ns,status
      integer*4 wtcount,colorcount,maccount,line,samp
      real*4 inbuf(maxns,maxpix),data(500)
      real*4 xcolor(maxcolors),ycolor(maxcolors),yycolor(maxcolors)
      real*4 xxcolor(maxcolors),zzcolor(maxcolors)
      real*4 dn(maxpix,maxcolors)
      real*4 x(maxns),y(maxns),yy(maxns),macloc(2,4)
      real*8 c(maxcolors,maxpix+1),cl(maxcolors),wts(maxcolors)
      real*8 coefx(maxpix+1),coefy(maxpix+1),coefz(maxpix+1)
      real*8 wts2(maxcolors)
      REAL*8  A(maxpix+1,maxpix+1),AL(maxpix+1)
      REAL*8 R(maxpix+1,maxpix+1),RL(maxpix+1)
      REAL*8 Q(maxpix+1,maxpix+1),LSQPX(maxpix+1)

c macbeth D65 color table measured 4/25/2001 w. Spectrolina at MIPL.
c data are ordered in rows top left to bottom right with gray row
c on bottom as last row. data are in x,y,Y for each of 24 patches.
      real*4 macbeth(3,24)/
     + .4024,.357,9.98,
     + .3841,.3556,36.3088,
     + .253,.2706,19.4492,
     + .3424,.4355,12.7141,
     + .2732,.2638, 25.0737, 
     + .2683,.366,43.5551,
     + .4986,.4054,28.6264,
     + .2196,.204,12.7054,
     + .4493,.3143,19.7654,
     + .2914,.2278,7.2636,
     + .383,.485,40.4741,
     + .4764,.4377,41.9126,
     + .1955,.1607,7.3632,
     + .3099,.4772,23.75,
     + .5281,.3161,12.0682,
     + .4533,.4743,59.4811,
     + .3659,.2504,20.0266,
     + .2043,.2824,20.8024,
     + .3221,.342,88.8639,
     + .317,.3348,59.9815,
     + .3158,.3335,35.9724,
     + .3168,.3335,20.5227,
     + .3159,.3323,9.691,
     + .3126,.328,3.4519/

c coordinates of macbeth color patches in same order as above in
c the order sample,line.
      real*4 macbeth_coord(2,24)/
     + 1.,1.,  2.,1.,  3.,1.,  4.,1.,  5.,1.,  6.,1.,
     + 1.,2.,  2.,2.,  3.,2.,  4.,2.,  5.,2.,  6.,2.,
     + 1.,3.,  2.,3.,  3.,3.,  4.,3.,  5.,3.,  6.,3.,
     + 1.,4.,  2.,4.,  3.,4.,  4.,4.,  5.,4.,  6.,4./

c parameters
      call xveaction('SA',' ')
      call xvpcnt('INP',nids)
      call xvparm('COLORS',data,colorcount,def,500)
      call xvparmd('WEIGHTS',wts,wtcount,def,maxcolors)
      call xvparm('MACBETH',macloc,maccount,def,8)

c open all inputs
      do i=1,nids
        call xvunit(inunit(i),'INP',i,status,' ')
        call xvopen(inunit(i),status,'U_FORMAT','REAL',' ')
        if(i.eq.1)then
          call xvget(inunit(i),status,'NL',nl,'NS',ns,' ')
          if(ns.gt.maxns)then
            call xvmessage('Input image line too long',' ')
            call abend
          endif
        endif
      enddo

c open all outputs
      do i=1,3
        call xvunit(outunit(i),'OUT',i,status,' ')
        call xvopen(outunit(i),status,'O_FORMAT','REAL',
     +    'U_FORMAT','REAL','OP','WRITE',' ')
      enddo

c Optionally read calibration file & bypass it's computation if no
c parameters are input.
      if(colorcount+maccount.eq.0)then
        call xvmessage('reading color_calibration.txt',' ')
        call read_calibration_file(
     +   coefX,coefY,coefZ,nids)
        goto 100
      endif

c sort out the color information from the COLORS keyword.
      if(colorcount.gt.0)then
        ncolors=colorcount/(3+nids)
        if(ncolors*(3+nids).ne.colorcount)then
          write(*,*)'incorrect number of parameters'
          call abend()
        endif
        if(ncolors.le.nids)then
          write(*,*)'colors must exceed input files'
          call abend()
        endif
        write(*,*)'Input data:'
        write(*,*)'      x           y           Y           dn--->'
        k=1
        do i=1,ncolors
          xcolor(i)=data(k)
          ycolor(i)=data(k+1)
          yycolor(i)=data(k+2)
          k=k+3
          do j=1,nids
            dn(j,i)=data(k+j-1)
          enddo
          k=k+nids
          write(*,*)xcolor(i),ycolor(i),yycolor(i),
     +             (dn(j,i),j=1,nids)
        enddo
      endif

c Extract the macbeth dn values from the image.
      if(maccount.gt.0)then
        ncolors=24
        ne=4
        nu=3
c       line solution: cl=c(1)*line+c(2)*samp+c(3)
        c(1,1)=1.d0
        c(1,2)=1.d0
        c(1,3)=1.d0
        cl(1)=macloc(1,1)
        wts2(1)=1.d0
        c(2,1)=1.d0
        c(2,2)=6.d0
        c(2,3)=1.d0
        cl(2)=macloc(1,2)
        wts2(2)=1.d0
        c(3,1)=4.d0
        c(3,2)=1.d0
        c(3,3)=1.d0
        cl(3)=macloc(1,3)
        wts2(3)=1.d0
        c(4,1)=4.d0
        c(4,2)=6.d0
        c(4,3)=1.d0
        cl(4)=macloc(1,4)
        wts2(4)=1.d0
        call lsqp(ind,ne,nu,c,cl,wts2,coefy,maxpix+1,maxcolors,
     +  A,AL,R,RL,Q,LSQPX)
        if(ind.eq.1)then
          write(*,*)'LSQP: error in y solution for geometry'
          write(*,*)'of the Macbeth target location.'
          call abend()
        endif
c       sample solution: cl=c(1)*line+c(2)*samp+c(3)
        cl(1)=macloc(2,1)
        cl(2)=macloc(2,2)
        cl(3)=macloc(2,3)
        cl(4)=macloc(2,4)
        call lsqp(ind,ne,nu,c,cl,wts2,coefx,maxpix+1,maxcolors,
     +  A,AL,R,RL,Q,LSQPX)
        if(ind.eq.1)then
          write(*,*)'LSQP: error in x solution for geometry'
          write(*,*)'of the Macbeth target location.'
          call abend()
        endif
c       Extraction of dn's.
        width=sqrt((macloc(1,1)-macloc(1,2))**2+
     +             (macloc(2,1)-macloc(2,2))**2)
        patchsize=width/5.0
        nsw2=patchsize/6.0
        write(*,*)'Averaging patches',2*nsw2+1,' pixels square'
        area=(2*nsw2+1)**2
        write(*,*)'Input data:'
        write(*,*)'      x           y           Y           dn--->'
        do i=1,ncolors
          xcolor(i)=macbeth(1,i)
          ycolor(i)=macbeth(2,i)
          yycolor(i)=macbeth(3,i)
          line=nint(coefy(1)*macbeth_coord(1,i)+
     +         coefy(2)*macbeth_coord(2,i)+coefy(3))
          samp=nint(coefx(1)*macbeth_coord(1,i)+
     +         coefx(2)*macbeth_coord(2,i)+coefx(3))
          if((line.lt.1+nsw2).or.(samp.lt.1+nsw2).or.
     +       (line.gt.nl-nsw2).or.(samp.gt.ns-nsw2))then
             call xvmessage('Macbeth chart outside image',' ')
             call xvmessage('MACBETH keywords in error',' ')
             call abend()
          endif
          do j=1,nids
            sum=0.0
            do k1=line-nsw2,line+nsw2
              call xvread(inunit(j),inbuf(1,1),
     +        status,'LINE',k1,' ')
              do k2=samp-nsw2,samp+nsw2
                sum=sum+inbuf(k2,1)
              enddo
            enddo
            dn(j,i)=sum/area
          enddo
          write(*,*)xcolor(i),ycolor(i),yycolor(i),
     +             (dn(j,i),j=1,nids)
        enddo
      endif

c compute X and Z tristimulus from chromaticities x and y
      write(*,*)'Computed tristimulus colors (X,Y,Z):'
      do i=1,ncolors
        xxcolor(i)=yycolor(i)*xcolor(i)/ycolor(i)
        zzcolor(i)=yycolor(i)/ycolor(i)-xxcolor(i)-yycolor(i)
        write(*,*)'color',i,xxcolor(i),yycolor(i),zzcolor(i)
      enddo

c assign weights & dimensions
      ne=ncolors
c     nu=nids+1
      nu=nids
      if(wtcount.eq.0)then
        do i=1,ncolors
          wts(i)=1.d0
        enddo
      else if(wtcount.ne.ncolors)then
        write(*,*)'Incorrect number of weights'
        write(*,*)'There should be ',ncolors,' weights'
        call abend()
      endif

c solve for coefficients relating dn to X tristumulus
      do i=1,ncolors
        cl(i)=xxcolor(i)
        do j=1,nids
          c(i,j)=dn(j,i)   ! the C terms
        enddo
c       c(i,nids+1)=1.d0   ! the K term
      enddo
      call lsqp(ind,ne,nu,c,cl,wts,coefx,maxpix+1,maxcolors,
     +  A,AL,R,RL,Q,LSQPX)
      if(ind.eq.1)then
        write(*,*)'LSQP: error in X solution'
        call abend()
      endif

c solve for coefficients relating dn to Y tristumulus
      do i=1,ncolors
        cl(i)=yycolor(i)
      enddo
      call lsqp(ind,ne,nu,c,cl,wts,coefy,maxpix+1,maxcolors,
     +  A,AL,R,RL,Q,LSQPX)
      if(ind.eq.1)then
        write(*,*)'LSQP: error in Y solution'
        call abend()
      endif

c solve for coefficients relating dn to Z tristumulus
      do i=1,ncolors
        cl(i)=zzcolor(i)
      enddo
      call lsqp(ind,ne,nu,c,cl,wts,coefz,maxpix+1,maxcolors,
     +  A,AL,R,RL,Q,LSQPX)
      if(ind.eq.1)then
        write(*,*)'LSQP: error in Z solution'
        call abend()
      endif

      call write_calibration_file(coefx,coefy,coefz,nids)

      write(*,*)'Solution for tristimulus X'
      write(*,*)'X=',(coefx(i),i=1,nids)
      write(*,*)'Solution for tristimulus Y'
      write(*,*)'Y=',(coefy(i),i=1,nids)
      write(*,*)'Solution for tristimulus Z'
      write(*,*)'Z=',(coefz(i),i=1,nids)

100   continue

c process images.
      do line=1,nl
        do image=1,nids
          call xvread(inunit(image),inbuf(1,image),
     +      status,'LINE',line,' ')
        enddo

        do i=1,ns
c         trix=coefx(nids+1)
c         triy=coefy(nids+1)
c         triz=coefz(nids+1)
          trix=0.0
          triy=0.0
          triz=0.0
          do j=1,nids
            trix=trix+coefx(j)*inbuf(i,j)
            triy=triy+coefy(j)*inbuf(i,j)
            triz=triz+coefz(j)*inbuf(i,j)
          enddo
          yy(i)=triy
          x(i)=trix/(trix+triy+triz)
          y(i)=triy/(trix+triy+triz)
        enddo

        call xvwrit(outunit(1),x,status,' ')
        call xvwrit(outunit(2),y,status,' ')
        call xvwrit(outunit(3),yy,status,' ')
      enddo

      return
      end

c***********************************************************************
      subroutine read_calibration_file(
     + coefX,coefY,coefZ,ntable)
      real*8 coefX(ntable),coefY(ntable),coefZ(ntable)

      open(unit=10,file='color_calibration.txt',
     + access='SEQUENTIAL',
     + form='FORMATTED',iostat=ios,status='OLD')
      if(ios.gt.0)then
        call xvmessage('cannot open calibration file:',' ')
        call xvmessage('color_calibration.txt',' ')
        call abend()
      endif

      read(unit=10,fmt=*,iostat=ios) (coefX(i),i=1,ntable)
      if(ios.gt.0)then
        call xvmessage('read error on calibration file',' ')
        call abend()
      endif
      if(ios.lt.0)then  ! EOF
        call xvmessage('EOF error on calibration file',' ')
        call abend()
      endif

      read(unit=10,fmt=*,iostat=ios) (coefY(i),i=1,ntable)
      if(ios.gt.0)then
        call xvmessage('read error on calibration file',' ')
        call abend()
      endif
      if(ios.lt.0)then  ! EOF
        call xvmessage('EOF error on calibration file',' ')
        call abend()
      endif

      read(unit=10,fmt=*,iostat=ios) (coefZ(i),i=1,ntable)
      if(ios.gt.0)then
        call xvmessage('read error on calibration file',' ')
        call abend()
      endif
      if(ios.lt.0)then  ! EOF
        call xvmessage('EOF error on calibration file',' ')
        call abend()
      endif

      close(unit=10)
      return
      end

c***********************************************************************
      subroutine write_calibration_file(
     +   coefX,coefY,coefZ,ntable)
      real*8 coefX(ntable),coefY(ntable),coefZ(ntable)

      open(unit=10,file='color_calibration.txt',
     + access='SEQUENTIAL',
     + form='FORMATTED',iostat=ios,status='UNKNOWN')
      if(ios.gt.0)then
        call xvmessage('cannot open calibration file',' ')
        call abend()
      endif

      write(unit=10,fmt=*,iostat=ios) (coefX(i),i=1,ntable)
      if(ios.gt.0)then
        call xvmessage('read error on calibration file',' ')
        call abend()
      endif
      if(ios.lt.0)then  ! EOF
        call xvmessage('EOF error on calibration file',' ')
        call abend()
      endif

      write(unit=10,fmt=*,iostat=ios) (coefY(i),i=1,ntable)
      if(ios.gt.0)then
        call xvmessage('read error on calibration file',' ')
        call abend()
      endif
      if(ios.lt.0)then  ! EOF
        call xvmessage('EOF error on calibration file',' ')
        call abend()
      endif

      write(unit=10,fmt=*,iostat=ios) (coefZ(i),i=1,ntable)
      if(ios.gt.0)then
        call xvmessage('read error on calibration file',' ')
        call abend()
      endif
      if(ios.lt.0)then  ! EOF
        call xvmessage('EOF error on calibration file',' ')
        call abend()
      endif

      close(unit=10)
      return
      end

c**************************************************************
      SUBROUTINE LSQP(ind,NE,NU,C,CL,wts,X1,n,m,
     +  A,AL,R,RL,Q,X)
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
c          wts=weight for each input point
C
C3    THE INFORMATION RETURNED TO THE MAIN PROGRAM IS:
C          X1(J) = COMPUTED VALUES OF THE UNKNOWNS
C
C5    ALL THE STATEMENTS BELOW ARE VALID FOR ANY NU LARGER THAN 1 AND
C     ANY NE LARGER THAN NU.
C
      REAL*8  A(n,n),AL(n),R(n,n),RL(n),Q(n,n),X(n),SUM
      REAL*8 C(m,n),CL(m),X1(n),wts(m)
      
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
      if(a(i,i).eq.0.d0)goto 999
      R(I,J)=A(I,J)/A(I,I)
      DO 110 L=1,I
110   A(K,J)=A(K,J)-R(L,K)*A(L,J)
      if(a(1,1).eq.0.d0)goto 999
      RL(1)=AL(1)/A(1,1)
      DO 125 I=2,NU
      DO 122 J=1,I
122   AL(I)=AL(I)-R(J,I)*AL(J)
      if(a(i,i).eq.0.d0)goto 999
125   RL(I)=AL(I)/A(I,I)
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
      RETURN
999   ind=1
      return
      END
 

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create dntoxyy.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM dntoxyy

   To Create the build file give the command:

		$ vimake dntoxyy			(VMS)
   or
		% vimake dntoxyy			(Unix)


************************************************************************/


#define PROGRAM	dntoxyy
#define R2LIB

#define MODULE_LIST dntoxyy.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create dntoxyy.pdf
process help=*
PARM INP TYPE=STRING COUNT=(3:10)
PARM OUT TYPE=STRING COUNT=3
PARM COLORS TYPE=REAL COUNT=(0:500) DEFAULT=--
PARM WEIGHTS TYPE=REAL COUNT=(0:50) DEFAULT=--
PARM MACBETH TYPE=REAL COUNT=(0,8) DEFAULT=--
END-PROC

.TITLE
VICAR program DNTOXYY

.HELP
PURPOSE:
To generate xyY images (chromaticity x and y and tristimulus Y) from
unknown input multispectral imagery where the scenes contain known
colors. Use this program if the images are not in radiance units or
the bands are outside the visible or of unknown wavelength. If you
know these things then use spectoxyy instead. Once xyY images exist 
they can be converted to rgb for a particular device using xyytospec
or xyy2hdtv.

METHOD:
This method solves for a linear relation between input dn and XYY
tristimulus values. You specify the xyY's and dn's of special colors in the
scene and a solution is made for the C's from the simultaneous
equations:
X(color1)=C1*dn(color1,band1)+C2*dn(color1,band2),...
X(color2)=C1*dn(color2,band1)+C2*dn(color2,band2),...
..etc..
This is done three times once for X, then Y, and then Z.
Once the C's are known you can substitute any input dn's to derive
the corresponding tristimulus values XYZ. The XYZ's are converted to
xyY's and written out.

All color data are input either after the keyword COLORS or after the
keyword MACBETH. These are the only mechanisms for inputting colors. Each time
you use these keywords the program writes a color calibration file called:
color_calibration.txt. Once this file exists you can cause the program to
read it for processing other images by running dntoxyy without any parameters.
See the example below.

WARNING: If you read a color calibration file your images must retain the same
order and gains as the images from which the file was created.

EXECUTION:

To process images and to create a calibration file:

dntoxyy inp=(band1,band2,...bandn) out=(x,y,Y) colors=(...)
xyy2hdtv inp=(x,y,Y) out=(r,g,b)
or
dntoxyy inp=(band1,band2,...bandn) out=(x,y,Y) macbeth=(...)
xyy2hdtv inp=(x,y,Y) out=(r,g,b)

To process images using an existing calibration file:

dntoxyy inp=(band1,band2,...bandn) out=(x,y,Y)
xyy2hdtv inp=(x,y,Y) out=(r,g,b)

.PAGE

METHOD:
HISTORY:
5-1-2001 J Lorre. 
COGNIZANT PROGRAMMER:  Jean Lorre

.LEVEL1
.VARI INP
N input images

.VARI OUT
3 Output images

.VARI COLORS
Color coordinates
and dn values.

.VARI WEIGHTS
Priorities of the
colors.

.VARI MACBETH
Location of
macbeth chart.

.LEVEL2
.VARI INP
Up to 10 multispectral registered input images.

.VARI OUT
Three Output REAL images in the order:
x    x chromaticity.
y    y chromaticity.
Y    Y tristimulus.

.VARI COLORS
(Only required to create a new color calibration file.)
Colors of the known reference colors.
We list here all data for all the reference colors strung out in a long
list from color 1 to color M. Each color must contain (in this order):
1. x chromaticity in D65 (0 to 1.0).
2. y chromaticity in D65.(0 to 1.0).
3. Y tristimulus in D65. (0 to 100.0).
4. DN of this color in input image 1.
5. DN of this color in input image 2.   
6. .....etc....
 . DN of this color in last input image.
Repeat above for each reference color.
The number of reference colors must exceed the number of input images by 
at least 1. If COLORS is specified the MACBETH keyword is not required.

.VARI WEIGHTS
(Optional)
A table of the weights (priorities) of each color for the least squares fit.
The default is 1.0 for each color. If WEIGHTS is specified you must list a
weight for each color. If MACBETH is specified there are 24 colors.

.VARI MACBETH
(Only required to create a new color calibration file.)
The image location of the macbeth chart.
There are 8 input numbers in the order line,sample,line,sample,...
These are, in input order, the image coordinates of the centers of the upper
left, upper right, lower left, & lower right patches on the macbeth color
chart. The chart is oriented with the gray row at the bottom.
 With this information the program will go to the images and extract the
dn values for each of the 24 macbeth color patches. The program already knows
the Macbeth D65 colors. If MACBETH is specified the COLORS keyword is not
required.
NOTE: The coordinates are the image locations of the CENTERS of the corner 
patches.


$ Return
$!#############################################################################
$Test_File:
$ create tstdntoxyy.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"

! the old test proc left by Jean Lorre contained the following tests:
!!
!! create calibration file using COLORS & process image.
!dntoxyy inp=(red.img,grn.img,blu.img) out=(x.img,y.img,yy.img) +
! colors=(.319,.336,82.17,    75.8,84.9,90.7, +
!         .405,.354,8.93,    32.8,23.,25.3, +
!         .322,.337,9.32,   24.8,13.,22.2, +
!         .354,.409,19.93,  90.9,90.4,20.4, +
!         .404,.383,25.73,    55.3,60.,63.8, +
!         .377,.367,26.,  71.,79.5,82.1, +
!         .433,.454,45.,  72.,84.,90.) +
! weights=(1.,5.,1.,1.,5.,5.,1.)
!! these are measured D65 values.
!! white paint
!! red lava rock
!! gray lava rock
!! green leaf
!! mural
!! sand
!! yellow tape
!xyy2hdtv inp=(x.img,y.img,yy.img) out=(r.img,g.img,b.img)
!xvd inp=(r.img,g.img,b.img)
!! create calibration file using MACBETH & process image.
!dntoxyy inp=(red.img,grn.img,blu.img) out=(x.img,y.img,yy.img) +
! macbeth=(100,100, 100,150, 130,100, 130,150)
!xyy2hdtv inp=(x.img,y.img,yy.img) out=(r.img,g.img,b.img)
!xvd inp=(r.img,g.img,b.img)
!! read calibration file test
!dntoxyy inp=(red.img,grn.img,blu.img) out=(x.img,y.img,yy.img)
!xyy2hdtv inp=(x.img,y.img,yy.img) out=(r.img,g.img,b.img)
!xvd inp=(r.img,g.img,b.img)

! (LWK / 5-Apr-2012:)
! Since the test files used above are long gone from the system, I
! constructed a new case using chromaticity and tristimulus values
! read from files generated by spectoxyy from files available.
! These will not give accurate results, but should serve to demonstrate
! the the program is working.
! The following command was used to generate the test case:
!spectoxyy (/project/test_work/testdata/gll/earth.red +
! /project/test_work/testdata/gll/earth.grn +
! /project/test_work/testdata/gll/earth.blu) (xsun.img ysun.img yysun.img hist.img) +
! lamda=(660,560,430) conv=(1 1 1) 'radiance 'sun 
! The DN values were measured at the following points:
! (L,S) = (218 399), (337 499), (557 389), (276 516).

dntoxyy (/project/test_work/testdata/gll/earth.red +
 /project/test_work/testdata/gll/earth.grn +
 /project/test_work/testdata/gll/earth.blu) +
 out=(x.img,y.img,yy.img) +
 colors=(0.3348 0.3252 44.48 139 121 133 +
         0.2132 0.2204 16.04   9  41 134 +
         0.3274 0.3258 72.91 202 201 220 +
         0.3223 0.3035 36.07 121  95 134)

xyy2hdtv inp=(x.img,y.img,yy.img) out=(r.img,g.img,b.img)

! the originals and derived images can be compared visually:
!xvd (/project/test_work/testdata/gll/earth.red +
! /project/test_work/testdata/gll/earth.grn +
! /project/test_work/testdata/gll/earth.blu)
!xvd inp=(r.img,g.img,b.img)

let $echo="no"
end-proc
$!-----------------------------------------------------------------------------
$ create tstdntoxyy.log_solos
tstdntoxyy
dntoxyy (/project/test_work/testdata/gll/earth.red  +
 /project/test_work/testdata/gll/earth.grn  +
 /project/test_work/testdata/gll/earth.blu)  +
 out=(x.img,y.img,yy.img)  +
 colors=(0.3348 0.3252 44.48 139 121 133  +
         0.2132 0.2204 16.04   9  41 134  +
         0.3274 0.3258 72.91 202 201 220  +
         0.3223 0.3035 36.07 121  95 134)
Beginning VICAR task dntoxyy
xyy2hdtv inp=(x.img,y.img,yy.img) out=(r.img,g.img,b.img)
Beginning VICAR task xyy2hdtv
let $echo="no"
exit
slogoff
$ Return
$!#############################################################################
