$!****************************************************************************
$!
$! Build proc for MIPL module superres
$! VPACK Version 1.9, Friday, March 26, 1999, 08:33:55
$!
$! Execute by entering:		$ @superres
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
$ write sys$output "*** module superres ***"
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
$ write sys$output "Invalid argument given to superres.com file -- ", primary
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
$   if F$SEARCH("superres.imake") .nes. ""
$   then
$      vimake superres
$      purge superres.bld
$   else
$      if F$SEARCH("superres.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake superres
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @superres.bld "STD"
$   else
$      @superres.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create superres.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack superres.com -
	-s superres.f -
	-i superres.imake -
	-p superres.pdf -
	-t tstsuperres.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create superres.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c
c program superres
c
      include 'VICMAIN_FOR'
      subroutine main44

      parameter (maxsamp=10000,maxpix=50,maxns=256,maxnl=256,maxtpts=9)
      real*4 obuf(maxsamp)
      integer*2 inbuf(maxns,maxnl,maxpix)
      integer*4 unit(maxpix),nl(maxpix),ns(maxpix),pixel(2,4)
      integer*4 samp,status,count,def,ounit,ntpts(maxpix)
      real*4 s_out,l_out,intensity(maxpix),residuals(maxpix)
      real*8 sum,xcoef(4,maxpix),ycoef(4,maxpix)
      real*8 ixcoef(4,maxpix),iycoef(4,maxpix)
      real*8 c(10,4),cl(10),wts(10),c1,c2 
      real*4 tpts(5,9,maxpix)
      character*100 tiepoints,listoffiles,filenames,data
      
c parameters
      call xveaction('SA','error')
      call xvpcnt('INP',nids)      
      call xvparm('SCALE',scale,count,def,1)
      call xvparm('RESOLUTION',resolution,count,def,1)
      resolution=resolution*resolution
      call xvparm('MODE',mode,count,def,1)

c read listoffiles & open inputs.
      call xvpone('INP',listoffiles,1,100)
      open(unit=10,file=listoffiles,status='old',
     + iostat=i,access='sequential')
      if(i.gt.0)then
        write(*,*)'Error opening listoffiles file ',listoffiles
        call abend()
      endif
      do image=1,maxpix
        read(10,30,iostat=i)filenames
30      format(A100)
        if(i.gt.0)then
          call xvmessage('read error on listoffiles',' ')
          call abend()
        endif
        if(i.lt.0)  goto 10
        call xvunit(unit(image),'OLD',image,status,
     +   'U_NAME',filenames,' ')
        call xvopen(unit(image),status,'U_FORMAT','HALF',' ')
        call xvget(unit(image),status,'NL',nl(image),
     +             'NS',ns(image),' ')
       if(ns(image).gt.maxns)then
          call xvmessage('Input line length too big',' ')
          call abend()
        endif
        if(nl(image).gt.maxnl)then
          call xvmessage('Too many input lines',' ')
          call abend()
        endif
      enddo
10    nids=image-1
      close(10)
      write(*,*)nids,' input files located'


c read tiepoints file
      call xvpone('INP',tiepoints,2,100)
      open(unit=10,file=tiepoints,status='old',
     + iostat=i,access='sequential')
      if(i.gt.0)then
        write(*,*)'Error opening tiepoints file ',tiepoints
        call abend()
      endif
      k=0
11    read(10,30,iostat=i)data
      if(i.ne.0)then
        call xvmessage('Error reading tiepoints file',' ')
        call abend()
      endif
      if(data(1:7).eq.'Picture')then
        k=k+1
        if(k.gt.maxpix)then
          write(*,*)'Read too many tiepoint sets'
          call abend()
        endif
        j=0
        goto 11
      endif
      if(data(1:3).eq.'end')goto 12
      j=j+1
      if(j.gt.maxtpts)then
        write(*,*)'Read too many tiepoints for image ',k+1
        call abend()
      endif
      read(data,*)(tpts(n,j,k),n=1,5)
c     write(*,*)j,k,(tpts(n,j,k),n=1,5)
      ntpts(k)=j
      goto 11
12    if(k.ne.nids-1)then
        write(*,*)'Unexpected # tiepoint sets.'
        write(*,*)'Read ',k,' expected ',nids-1
        call abend()
      endif     
      close(10)
      
c Compute polynomials mapping first image to all the others.
c tpts(1,*)=reference line
c tpts(2,*)=reference sample
c tpts(3,*)=line
c tpts(4,*)=samp
c tpts(5,*)=quality
c line=ycoef(1)*refline+ycoef(2)*refsamp+ycoef(3)
c samp=xcoef(1)*refline+xcoef(2)*refsamp+xcoef(3)

      if(mode.eq.2)then            ! affine polynomial
        do image=1,nids-1
          do i=1,ntpts(image)
            c(i,1)=tpts(1,i,image)
            c(i,2)=tpts(2,i,image)
            c(i,3)=1.d0
            cl(i)=tpts(3,i,image)
            wts(i)=tpts(5,i,image)
          enddo
          call lsqp(ind,ntpts(image),3,c,cl,wts,ycoef(1,image+1))
          if(ind.ne.0)write(*,*)'LSQP error'
          do i=1,ntpts(image)
            c(i,1)=tpts(1,i,image)
            c(i,2)=tpts(2,i,image)
            c(i,3)=1.d0
            cl(i)=tpts(4,i,image)
            wts(i)=tpts(5,i,image)
          enddo
          call lsqp(ind,ntpts(image),3,c,cl,wts,xcoef(1,image+1))
          if(ind.ne.0)write(*,*)'LSQP error'
        enddo
      endif

      if(mode.eq.1)then            ! offset only
        do image=1,nids-1
          dy=0.
          dx=0.
          do i=1,ntpts(image)
            dy=dy+tpts(3,i,image)-tpts(1,i,image)
            dx=dx+tpts(4,i,image)-tpts(2,i,image)
          enddo
          dx=dx/ntpts(image)
          dy=dy/ntpts(image)
          xcoef(1,image+1)=0.d0
          xcoef(2,image+1)=1.d0
          xcoef(3,image+1)=dx
          ycoef(1,image+1)=1.d0
          ycoef(2,image+1)=0.d0
          ycoef(3,image+1)=dy
        enddo
      endif

      xcoef(1,1)=0.d0 ! reference image to itself.
      xcoef(2,1)=1.d0
      xcoef(3,1)=0.d0
      ycoef(1,1)=1.d0
      ycoef(2,1)=0.d0
      ycoef(3,1)=0.d0

c check least squares for precision.
      do image=2,nids
        sum=0.d0
        do i=1,ntpts(image-1)
          y1=tpts(1,i,image-1)
          x1=tpts(2,i,image-1)
          y=ycoef(1,image)*y1+ycoef(2,image)*x1+ycoef(3,image)
          x=xcoef(1,image)*y1+xcoef(2,image)*x1+xcoef(3,image)
          dy=tpts(3,i,image-1)-y
          dx=tpts(4,i,image-1)-x
          sum=sum+sqrt(dy*dy+dx*dx)
        enddo
        residuals(image)=sum/ntpts(image-1)
      enddo
      residuals(1)=0.
      
c invert the polynomials
      do image=1,nids
        c1=ycoef(1,image)-ycoef(2,image)*xcoef(1,image)/xcoef(2,image)
        c2=ycoef(3,image)-ycoef(2,image)*xcoef(3,image)/xcoef(2,image)
        iycoef(1,image)=1.d0/c1
        iycoef(2,image)=-ycoef(2,image)/(xcoef(2,image)*c1)
        iycoef(3,image)=-c2/c1
        ixcoef(1,image)=-xcoef(1,image)/xcoef(2,image)
        ixcoef(2,image)=1.d0/xcoef(2,image)
        ixcoef(3,image)=-xcoef(3,image)/xcoef(2,image)
      enddo

c read inputs into memory
      do image=1,nids
        do j=1,nl(image)
          call xvread(unit(image),inbuf(1,j,image),status,' ')
        enddo
      enddo
      
c determine size of output as largest input * scale
      nlo=0
      nso=0
      do image=1,nids
        nlo=max(nlo,int(scale*nl(image)))
        nso=max(nso,int(scale*ns(image)))
      enddo
      if(nso.gt.maxsamp)then
        call xvmessage('Output line length too big',' ')
        call abend()
      endif
            
c determine intensities of each input
      do image=1,nids
        sum=0.d0
        count=0
        do j=1,nl(1)
          do i=1,ns(1)
            iy=nint(ycoef(1,image)*j+ycoef(2,image)*i+ycoef(3,image))
            ix=nint(xcoef(1,image)*j+xcoef(2,image)*i+xcoef(3,image))
            if((ix.ge.1).and.(ix.le.ns(image)).and.(iy.ge.1).and.
     +         (iy.le.nl(image)))then
              count=count+1
              sum=sum+inbuf(ix,iy,image)
            endif
          enddo
          count=count+k
        enddo
        intensity(image)=sum/count
      enddo
      do image=nids,1,-1
        intensity(image)=intensity(1)/intensity(image)
      enddo
      
c print out status
      call xvmessage('   least squares   intensity',' ')
      call xvmessage('   residuals       correction',' ')
      do image=1,nids
        write(*,15)residuals(image),intensity(image)
15      format(2f13.6)
      enddo  
      
c open output
      call xvunit(ounit,'OUT',1,status,' ')
      call xvopen(ounit,status,'U_FORMAT','REAL',
     +   'O_FORMAT','HALF','U_NL',nlo,'U_NS',nso,'OP','WRITE',' ')
        
c compute offset relating output to reference input
      offset=1.0-(scale+1.0)/(2.0*scale)
      offset=0.
      
c process image data.
c input picture 1 is the reference image.
      do line=1,nlo                     ! output line 
        do samp=1,nso                   ! output sample

c         convert from output to location in first input.
          y1=line/scale+offset          ! reference input line
          x1=samp/scale+offset          ! reference input sample
          k=0
          sum1=0.0
          sum2=0.0
          
          do image=1,nids               ! input image

c           get sub pixel location in the input images.
            y=ycoef(1,image)*y1+ycoef(2,image)*x1+ycoef(3,image)
            x=xcoef(1,image)*y1+xcoef(2,image)*x1+xcoef(3,image)

c           get 4 nearest input pixels.
            ix=x
            iy=y
            pixel(1,1)=iy
            pixel(2,1)=ix
            pixel(1,2)=iy+1
            pixel(2,2)=ix
            pixel(1,3)=iy
            pixel(2,3)=ix+1
            pixel(1,4)=iy+1
            pixel(2,4)=ix+1

            do kk=1,4
              iy=pixel(1,kk)
              ix=pixel(2,kk)
              if((ix.ge.1).and.(ix.le.ns(image)).and.(iy.ge.1).and.
     +           (iy.le.nl(image)))then
     
                dnin=inbuf(ix,iy,image)*intensity(image)  ! input dn's

c               get sub pixel location of nearest pixel in first input.
c               the lone y below is not a syntax error !
                y=iycoef(1,image)*iy+iycoef(2,image)*ix+iycoef(3,image)
                x=ixcoef(1,image)*y+ixcoef(2,image)*ix+ixcoef(3,image)

c               get output pixel location.
                s_out=scale*(x-offset)    ! output sub pixel samp
                l_out=scale*(y-offset)    ! output sub pixel line
                r=(s_out-samp)**2+(l_out-line)**2 ! range
                if(r.lt.resolution)r=resolution
                sum1=sum1+dnin/r
                sum2=sum2+1.0/r
                k=k+1
              endif
            enddo
          enddo
          if(k.gt.0)then
            obuf(samp)=sum1/sum2
          else
            obuf(samp)=0.0
          endif
        enddo
        call xvwrit(ounit,obuf,status,' ')       ! output line  

      enddo      
      return
      end

c**************************************************************
      SUBROUTINE LSQP(ind,NE,NU,C,CL,wts,X1)
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
      REAL*8  A(4,4),AL(4),R(4,4),RL(4),Q(4,4),X(4),SUM
      REAL*8 C(10,4),CL(10),X1(4),wts(10)
      
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
$ create superres.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM superres

   To Create the build file give the command:

		$ vimake superres			(VMS)
   or
		% vimake superres			(Unix)


************************************************************************/


#define PROGRAM	superres
#define R2LIB

#define MODULE_LIST superres.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define R2LIB

#define LIB_FORTRAN
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create superres.pdf
process help=*
PARM INP TYPE=STRING COUNT=2
PARM OUT TYPE=STRING COUNT=1
PARM SCALE TYPE=REAL COUNT=(0:1) VALID=(1:20) DEFAULT=4.9
PARM RESOLUTION TYPE=REAL COUNT=(0:1) VALID=(.00001:10.) DEFAULT=0.5
PARM MODE TYPE=INTEGER COUNT=(0:1) VALID=(1,2) DEFAULT=2
END-PROC

.TITLE
VICAR program SUPERRES

.HELP
PURPOSE:
To combine many images into a larger image which can be processed to give
super resolution.

EXECUTION:
register2 inp=listoffiles out=tiepoints
superres inp=(listoffiles,tiepoints) out=superimage
( & optionally ...)
filter2 inp=superimage out=evenbettersuperimage nlw=15 nsw=15 +
 mtf=(1.,0.,5.,.35,5.,.5)

where:
Listoffiles is an ascii list of filenames, one per record.
Tiepoints is an ascii list of tiepoints created by vicar program REGISTER2.

.PAGE
METHOD:
1. Superres reads the image and tiepoints file.
   It creates polynomials from the tiepoints mapping each input to the output. 
2. An output is created SCALE times the inputs in size.
3. For each output pixel..
4. All the nearest input pixels are identified (4 per input image).
5. These pixel centers are mapped to the output sub pixel location.
6. output_dn=sum(dn_in/r_in**2)/sum(1/r_in**2)
     where r_in is the distance between the input pixels mapped to the output
     and the output pixel location. 

HISTORY:
3-1-99  J Lorre. 
COGNIZANT PROGRAMMER:  Jean Lorre

.LEVEL1

.VARI INP
1. Filenames list.
2. Tiepoints list.

.VARI OUT
Superesolution image.

.VARI SCALE
Image magnification
factor.

.VARI RESOLUTION
Output pixel resolution.

.VARI MODE
1 = offset only
2 = 1st order polynomial

.LEVEL2

.VARI INP
Two input files:
1. An ascii list of filenames, one per record.
2. An ascii list of tiepoints created by vicar program REGISTER2.

.VARI OUT
The superresolution image.
You may need to deconvolve this image to make higher resolution apparent.
HALF format.

.VARI SCALE
Image magnification factor.
Sometimes an irrational number is better than an integer because it breaks up
standing patterns in the image. 
Defaults to 4.9

.VARI RESOLUTION
Output pixel resolution.
Each input pixel is projected into the output. It's distance R is measured from
the output target pixel. If R < RESOLUTION it is reset to = RESOLUTION. This
limits the sharpness of the output image but it stabilizes the output dn
values by limiting artifact due to poor registration.
Defaults to 0.5 (0.5 output pixel diameter). 

.VARI MODE
Type of registration polynomial permitted.
1 = offset only
2 = 1st order polynomial (affine)
(default is 2)

.END
$ Return
$!#############################################################################
$Test_File:
$ create tstsuperres.pdf
procedure
refgbl $echo
body
let $echo="yes"
!
copy inp=/project/test_work/testdata/mars/cleveland.vicar +
 out=a.img sl=1000 ss=1 nl=1026 ns=1026
size inp=a.img out=b.img zoom=-2 'noin
copy inp=b.img out=a.img sl=1 ss=1 nl=512 ns=512
size inp=a.img out=ul.img 'noin zoom=-2
copy inp=b.img out=a.img sl=1 ss=2 nl=512 ns=512
size inp=a.img out=ur.img 'noin zoom=-2
copy inp=b.img out=a.img sl=2 ss=1 nl=512 ns=512
size inp=a.img out=ll.img 'noin zoom=-2
copy inp=b.img out=a.img sl=2 ss=2 nl=512 ns=512
size inp=a.img out=lr.img 'noin zoom=-2
! create a listoffiles called clevelandlist that looks like:
! ul.img
! ur.img
! ll.img
! lr.img
! with an editor. Don't forget the carriage return on the last record.
register2 inp=clevelandlist out=tiepoints
superres inp=(clevelandlist,tiepoints) out=sup.img scale=2 resolution=.5 mode=1
filter2 inp=sup.img out=restored.img nlw=15 nsw=15 mtf=(1.,0.,6.,.35,6.,0.5)
size inp=ul.img out=bigul.img zoom=2 'noin
size inp=ul.img out=bigul2.img zoom=2
copy inp=b.img out=c.img nl=512 ns=512
concat inp=(c.img,restored.img,bigul.img,bigul2.img) out=a.img ns=1024 perc=.5
xvd a.img
!barne_r inp=a.img miplbox=21 primary=42057 account="250302"
!
end-proc
$ Return
$!#############################################################################
