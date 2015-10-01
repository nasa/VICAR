$!****************************************************************************
$!
$! Build proc for MIPL module correlate1d
$! VPACK Version 1.9, Tuesday, April 24, 2012, 18:45:04
$!
$! Execute by entering:		$ @correlate1d
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
$ write sys$output "*** module correlate1d ***"
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
$ write sys$output "Invalid argument given to correlate1d.com file -- ", primary
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
$   if F$SEARCH("correlate1d.imake") .nes. ""
$   then
$      vimake correlate1d
$      purge correlate1d.bld
$   else
$      if F$SEARCH("correlate1d.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake correlate1d
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @correlate1d.bld "STD"
$   else
$      @correlate1d.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create correlate1d.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack correlate1d.com -mixed -
	-s correlate1d.f -
	-i correlate1d.imake -
	-p correlate1d.pdf -
	-t tstcorrelate1d.pdf tstcorrelate1d.log_solos tstcorrelate1d.log_linux
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create correlate1d.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c program correlate1d
      include 'VICMAIN_FOR'
      subroutine main44

      parameter (nlmax=2000,nsmax=2000)
      real*4 lbuf(nsmax,nlmax),rbuf(nsmax,nlmax)
      real*4 ldis(nsmax,nlmax),sdis(nsmax,nlmax)
      real*4 quality2(nsmax)
      real*4 sorted(100),rbestsamp(-100:nlmax+100)
      real*8 sumx,sumy,sumx2,sumy2,sumxy,rn
      real*4 sumxl,sumyl,sumx2l,sumy2l,sumxyl
      real*4 sumxr,sumyr,sumx2r,sumy2r,sumxyr
      integer*4 bestl,bests,bestsamp(nlmax),fw2
      integer*4 hist(-50:50)
      logical set_offset
      
      integer*4 count,def,status,geom(2),unit(2),ounit(2)


c parameters
      call xvparm('NLW',nlw,count,def,1)
      if(nlw.eq.(nlw/2)*2)nlw=nlw+1
      call xvparm('NSW',nsw,count,def,1)
      if(nsw.eq.(nsw/2)*2)nsw=nsw+1
      call xvparm('MOTION',motion,count,def,1)
      call xvparm('GEOM',geom,count,def,2)
      linedif=geom(1)
      fw2=geom(2)
      call xvparm('OFFSET',jbias,count,def,1)
      set_offset=.false.
      if(count.gt.0)set_offset=.true.
      call xvparm('THRESH',thresh,count,def,1)
      call xvparm('QUALITY',tquality,count,def,1)
      tquality=tquality**2

      call xveaction('SA',' ')

c open 2 inputs
        call xvunit(unit(1),'INP',1,status,' ')
        call xvopen(unit(1),status,'U_FORMAT','REAL',' ')
        call xvget(unit(1),status,'NL',nlleft,'NS',nsleft,' ')
        call xvunit(unit(2),'INP',2,status,' ')
        call xvopen(unit(2),status,'U_FORMAT','REAL',' ')
        call xvget(unit(2),status,'NL',nlright,'NS',nsright,' ')
        if(nsleft.gt.nsmax)then
          call xvmessage('Line length too long',' ')
          call abend
        endif
        if(nlleft.gt.nlmax)then
          call xvmessage('Too many lines',' ')
          call abend
        endif

c read inputs into memory
      do line=1,nlleft                         ! line loop
        call xvread(unit(1),lbuf(1,line),status,'LINE',line,' ')
      enddo
      do line=1,nlright                         ! line loop
        call xvread(unit(2),rbuf(1,line),status,'LINE',line,' ')
      enddo

c open 2 outputs
      do i=1,2
        call xvunit(ounit(i),'OUT',i,status,' ')
        call xvopen(ounit(i),status,'O_FORMAT','REAL',
     +   'U_FORMAT','REAL','OP','WRITE',' ')
      enddo


c*************** Establish geometry relating the two input images **********
      if(set_offset)goto 80

      write(*,*)'Begin determination of vertical shift'

      do i=-linedif,linedif
        hist(i)=0
      enddo

c correlate every linc lines
      linc=nlleft/25
      icen=nsleft/2
      rn=fw2*2+1.
      do kk=linedif+6,nlleft-linedif-6,linc                             ! line centers
        bestquality=-1.
        do line=kk-linedif,kk+linedif                 ! local line
          do il=fw2+4,nsleft-fw2-4                    ! sample centers
            sumx=0.d0
            sumy=0.d0
            sumx2=0.d0
            sumy2=0.d0
            sumxy=0.d0
            k=icen-il
            do i=il-fw2,il+fw2                        ! convolution
                x=lbuf(i+k,kk)                        ! template
                y=rbuf(i,line)                        ! search area
                sumx=sumx+x
                sumy=sumy+y
                sumx2=sumx2+x*x
                sumy2=sumy2+y*y
                sumxy=sumxy+x*y
            enddo
            quality=(sumxy-sumx*sumy/rn)**2/
     +          ((sumx2-sumx*sumx/rn)*(sumy2-sumy*sumy/rn))
            if(quality.gt.bestquality)then
              bestquality=quality
              bestl=line
              bests=il
            endif
          enddo
        enddo
        hist(bestl-kk)=hist(bestl-kk)+1
      enddo

      k=-1                    ! locate most frequent line shift
      do i=-linedif,linedif
        if(hist(i).gt.k)then
          k=hist(i)
          jbias=i
        endif
      enddo
      write(*,*)'Line offset parameter set to ',jbias

80    continue

c***************** Get the horizontal shift of each line *****************

      write(*,*)'Begin construction of horizontal shift model'

c perform 1-d correlation of line center with rest of line
      do line=1,nlleft                              ! line centers
        bestsamp(line)=0                            ! no data flag
        bestquality=-1.
        do il=fw2+4,nsleft-fw2-4                    ! sample centers
          sumx=0.d0
          sumy=0.d0
          sumx2=0.d0
          sumy2=0.d0
          sumxy=0.d0
          k=icen-il
          kk=line+jbias
          if(kk.lt.1)goto 70
          if(kk.gt.nlright)goto 70
          do i=il-fw2,il+fw2                        ! convolution
              x=lbuf(i+k,line)                      ! template
              y=rbuf(i,kk)                          ! search area
              sumx=sumx+x
              sumy=sumy+y
              sumx2=sumx2+x*x
              sumy2=sumy2+y*y
              sumxy=sumxy+x*y
          enddo
          quality=(sumxy-sumx*sumy/rn)**2/
     +        ((sumx2-sumx*sumx/rn)*(sumy2-sumy*sumy/rn))
          if(quality.gt.bestquality)then
            bestquality=quality
            bestsamp(line)=il
          endif
        enddo
        bestsamp(line)=bestsamp(line)-icen
70      continue
      enddo

c Try to patch errors by replacing shift by local median.
      n=15
      m=2*n+1
      do i=1,nlleft
        rbestsamp(i)=bestsamp(i)
      enddo
      do i=0,-n,-1
        rbestsamp(i)=rbestsamp(1-i)
      enddo
      do i=1,n
        rbestsamp(nlleft+i)=rbestsamp(nlleft-i)
      enddo
      do j=1,nlleft
        k=0
        do i=j-n,j+n
          k=k+1
          sorted(k)=rbestsamp(i)
        enddo
        call heapsort(m,sorted)
        bestsamp(j)=sorted(n+1)
      enddo

c Fill zeroes

c********************** begin correlation ************************

      write(*,*)'Begin correlations'

c set constants
      nlwc=nlw/2+1
      nswc=nsw/2+1
      nlw2=nlw/2
      nsw2=nsw/2

c clear disparity images
      do j=1,nlleft
        do i=1,nsleft
          ldis(i,j)=0.0
          sdis(i,j)=0.0
        enddo
      enddo

      ncorel=0
      avecor=0.
      rn=nswc*nlw
      line_top=nlwc
      line_bot=nlleft-nlw2
      if(jbias.lt.0)line_top=line_top+abs(jbias)
      if(jbias.gt.0)line_bot=line_bot-abs(jbias)

      do jl=line_top,line_bot                 ! center line
        ibias=bestsamp(jl)           ! offset sample bias

        do il=nswc+motion,nsleft-nsw2-motion  ! center sample

          sumxl=0.0               ! precompute template sums
          sumx2l=0.0
          sumxr=0.0
          sumx2r=0.0
          do j=jl-nlw2,jl+nlw2    ! each line of nlw lines
            do i=il-nsw2,il   
              x=lbuf(i,j)         ! left half of template
              sumxl=sumxl+x
              sumx2l=sumx2l+x*x
            enddo
            do i=il,il+nsw2
              x=lbuf(i,j)         ! right half of template
              sumxr=sumxr+x
              sumx2r=sumx2r+x*x
            enddo
          enddo

          do is=il-motion,il+motion      ! center sample in search
            k=is-il+ibias
            if(il-nsw2+k.lt.1)goto 100        ! outside right image
            if(il+nsw2+k.gt.nsright)goto 100  ! "
            sumyl=0.0
            sumy2l=0.0
            sumxyl=0.0
            sumyr=0.0
            sumy2r=0.0
            sumxyr=0.0

            do j=jl-nlw2,jl+nlw2               ! each line of nlw lines
              jr=j+jbias                       ! line in right image
              do i=il-nsw2,il             ! correlate 1 line
                x=lbuf(i,j)     ! left half of template
                y=rbuf(i+k,jr)  ! search area
                sumyl=sumyl+y
                sumy2l=sumy2l+y*y
                sumxyl=sumxyl+x*y
              enddo
              do i=il,il+nsw2             ! correlate 1 line
                x=lbuf(i,j)     ! right half of template
                y=rbuf(i+k,jr)  ! search area
                sumyr=sumyr+y
                sumy2r=sumy2r+y*y
                sumxyr=sumxyr+x*y
              enddo
            enddo
            qleft=(sumxyl-sumxl*sumyl/rn)**2/
     +          ((sumx2l-sumxl*sumxl/rn)*(sumy2l-sumyl*sumyl/rn))
            qright=(sumxyr-sumxr*sumyr/rn)**2/
     +          ((sumx2r-sumxr*sumxr/rn)*(sumy2r-sumyr*sumyr/rn))
            quality2(is)=max(qleft,qright)
          enddo

          bestquality=-1.0                    ! get best quality at kk
          do is=il-motion,il+motion
            if(quality2(is).gt.bestquality)then
              bestquality=quality2(is)
              kk=is
            endif
          enddo

          if(bestquality.lt.tquality)goto 100 ! abort if quality is poor
          if(kk.eq.il-motion) goto 100        ! abort if is at the end
          if(kk.eq.il+motion) goto 100

          if((quality2(kk-1).lt.quality2(kk)).and.
     +       (quality2(kk+1).lt.quality2(kk)))then  ! interpolate for s
c            fa=sqrt(quality2(kk-1))
c            fb=sqrt(quality2(kk))
c            fc=sqrt(quality2(kk+1))
            fa=quality2(kk-1)
            fb=quality2(kk)
            fc=quality2(kk+1)
            s=kk-0.5*(fa-fc)/(2.0*fb-fc-fa)
          else
            s=kk
          endif
            
          ncorel=ncorel+1
          ldis(il,jl)=jl+jbias       ! line disparity
          sdis(il,jl)=s+ibias        ! sample disparity
          avecor=avecor+fb

100       continue
        enddo

      enddo
      write(*,*)'Performed ',ncorel,' correlations'
      write(*,*)'Average correlation quality was ',sqrt(avecor/ncorel)

c********************** Check correlations in reverse *************


      write(*,*)'Begin inverse correlation check'
      nrejected=0
      do jline=1,nlleft
        do isamp=1,nsleft
          if(sdis(isamp,jline).eq.0.0)goto 120

          jl=nint(ldis(isamp,jline)) ! right line
          il=nint(sdis(isamp,jline)) ! right sample
          ibias=isamp-il
          jbias=jline-jl

          sumxl=0.0               ! precompute template sums
          sumx2l=0.0
          sumxr=0.0
          sumx2r=0.0
          do j=jl-nlw2,jl+nlw2    ! each line of nlw lines
            do i=il-nsw2,il   
              x=rbuf(i,j)         ! left half of template
              sumxl=sumxl+x
              sumx2l=sumx2l+x*x
            enddo
            do i=il,il+nsw2
              x=rbuf(i,j)         ! right half of template
              sumxr=sumxr+x
              sumx2r=sumx2r+x*x
            enddo
          enddo

          do is=il-motion,il+motion      ! center sample in search
            k=is-il+ibias
            if(il-nsw2+k.lt.1)goto 131        ! outside left image
            if(il+nsw2+k.gt.nsleft)goto 131  ! "
            sumyl=0.0
            sumy2l=0.0
            sumxyl=0.0
            sumyr=0.0
            sumy2r=0.0
            sumxyr=0.0

            do j=jl-nlw2,jl+nlw2               ! each line of nlw lines
              jr=j+jbias                       ! line in left image
              do i=il-nsw2,il             ! correlate 1 line
                x=rbuf(i,j)     ! left half of template
                y=lbuf(i+k,jr)  ! search area
                sumyl=sumyl+y
                sumy2l=sumy2l+y*y
                sumxyl=sumxyl+x*y
              enddo
              do i=il,il+nsw2             ! correlate 1 line
                x=rbuf(i,j)     ! right half of template
                y=lbuf(i+k,jr)  ! search area
                sumyr=sumyr+y
                sumy2r=sumy2r+y*y
                sumxyr=sumxyr+x*y
              enddo
            enddo
            qleft=(sumxyl-sumxl*sumyl/rn)**2/
     +          ((sumx2l-sumxl*sumxl/rn)*(sumy2l-sumyl*sumyl/rn))
            qright=(sumxyr-sumxr*sumyr/rn)**2/
     +          ((sumx2r-sumxr*sumxr/rn)*(sumy2r-sumyr*sumyr/rn))
            quality2(is)=max(qleft,qright)
          enddo

          bestquality=-1.0                 ! get best quality at kk
          do is=il-motion,il+motion
            if(quality2(is).gt.bestquality)then
              bestquality=quality2(is)
              kk=is
            endif
          enddo

          if(bestquality.lt.tquality)goto 130 ! abort if quality is poor
          if(kk.eq.il-motion) goto 130        ! abort if is at the end
          if(kk.eq.il+motion) goto 130

          if((quality2(kk-1).lt.quality2(kk)).and.
     +       (quality2(kk+1).lt.quality2(kk)))then  ! interpolate for s
c            fa=sqrt(quality2(kk-1))
c            fb=sqrt(quality2(kk))
c            fc=sqrt(quality2(kk+1))
            fa=quality2(kk-1)
            fb=quality2(kk)
            fc=quality2(kk+1)
            s=kk-0.5*(fa-fc)/(2.0*fb-fc-fa)
          else
            s=kk
          endif

          if(abs(s+ibias-isamp).gt.thresh) goto 130 ! check inverse

          goto 120
130       continue  ! failed the test
          nrejected=nrejected+1
131       continue
          ldis(isamp,jline)=0.0
          sdis(isamp,jline)=0.0

120       continue
        enddo
      enddo
      write(*,*)'Inverse rejected ',nrejected,' points'


c write disparity images
      do line=1,nlleft
        call xvwrit(ounit(1),ldis(1,line),status,' ')
        call xvwrit(ounit(2),sdis(1,line),status,' ')
      enddo

      end

      SUBROUTINE heapsort(N,RA)
c Heapsort algorithm (Numerical Recipes SORT).
c Real array RA of length N.
c RA is replaced upon return with a sorted version of itself.
      DIMENSION RA(N)
      L=N/2+1
      IR=N
10    CONTINUE
        IF(L.GT.1)THEN
          L=L-1
          RRA=RA(L)
        ELSE
          RRA=RA(IR)
          RA(IR)=RA(1)
          IR=IR-1
          IF(IR.EQ.1)THEN
            RA(1)=RRA
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
20      IF(J.LE.IR)THEN
          IF(J.LT.IR)THEN
            IF(RA(J).LT.RA(J+1))J=J+1
          ENDIF
          IF(RRA.LT.RA(J))THEN
            RA(I)=RA(J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 20
        ENDIF
        RA(I)=RRA
      GO TO 10
      END



$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create correlate1d.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM correlate1d

   To Create the build file give the command:

		$ vimake correlate1d			(VMS)
   or
		% vimake correlate1d			(Unix)


************************************************************************/


#define PROGRAM	correlate1d
#define R2LIB

#define MODULE_LIST correlate1d.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create correlate1d.pdf
process help=*
PARM INP TYPE=STRING COUNT=2
PARM OUT TYPE=STRING COUNT=2
PARM NLW TYPE=INTEGER COUNT=(0:1) VALID=(1:51) DEFAULT=1
PARM NSW TYPE=INTEGER COUNT=(0:1) VALID=(3:201) DEFAULT=45
PARM MOTION TYPE=INTEGER COUNT=(0:1) VALID=(1:201) DEFAULT=10
PARM QUALITY TYPE=REAL COUNT=(0:1) VALID=(0.:1.) DEFAULT=0.7
PARM OFFSET TYPE=INTEGER COUNT=(0:1) VALID=(0:50) DEFAULT=--
PARM GEOM TYPE=INTEGER COUNT=(0,2) VALID=(0:50,10:250) DEFAULT=(6,120)
PARM THRESH TYPE=REAL COUNT=(0:1) VALID=(0.:100.) DEFAULT=1.0
END-PROC

.TITLE
VICAR program CORRELATE1D

.HELP
PURPOSE:
  To compute 1-d correlated tiepoints between two images.
  The input stereo pair must have horizontal epipolar lines.
  The left image is fixed. The right image is variable.

EXECUTION:
   correlate1d inp=left,right out=ldis,sdis parameters
where:
   left is an image where templates are selected at every pixel.
   right is an image which is searched over to refine tiepoint locations.

   ldis and sdis are disparity images. The integer index
   in these images corresponds to the left image coordinate. The value
   at the index corresponds to the right image location. ldis is the
   line disparity and sdis is the sample disparity.

   for example: (sample,line)
   if sdis(10,100) is 11.3 and ldis(10,100) is 102.1 then the left image
   coordinate at (10,100) lies at sample 11.3 and line 102.1 in the right
   image. Inspect the "delta" keyword for an alternative format. A value of
   zero for ldis or sdis means the pixel could not be correlated well.

.PAGE

METHOD:
  This is a simple 1-d correlator which is executed on each input pixel
location. You can include more than one line at a time. At each input pixel a
template NSW by NLW pixels is correlated within a search area of width +-
MOTION pixels. When the highest correlation is located a quadratic is fitted
to the 3 nearest points to give a sub pixel estimate. 
   One of the hazards of stereo pair correlation from rover images is the
occlusion of background by foreground rocks. To avoid the correlator including
terrain at different distances within the same window the left and right
halves of the NSW template is correlated independently. The resulting
correlation quality is the larger of the two. Thus if NSW=41 then the
actual correlation width will be 21.
   An analysis is performed to determine the vertical offset between the
two input images. 20 lines are selected and a patch N pixels wide in the 
center is correlated across the entire image width and up and down by
+- M lines ( see GEOM=M,N parameter). The best vertical offset is added to
a histogram of offsets. The offset with the most counts gives the vertical
shift between images. Parameter OFFSET overrides this step. 
   In order to minimize the search an analysis is performed to locate the 
horizontal displacement between lines as a function of line number. 
This is performed by correlating the central N pixels (see GEOM=M,N parameter)
of every line across the entire image. The result gives the horizontal offset
for each line. This is used to start correlations near the correct offset
so that the correct correlation match will lie within +- MOTION pixels.
Note that this will not work well if the cameras are severely tilted.
To erase bad results the offset table is convolved by a median filter
31 lines long. 
   The correlator relies upon a single figure of merit which is maximized. This
is the correlation value, sometimes called Pearson's r. It is the quality of a
least squares fit between the dn (intensity) vales in the template and the
search area. If x and y are the template and search dn's then r is computed
from:                 sum(x*y)-sum(x)*sum(y)/n
  r^2=--------------------------------------------------
        [sum(x*x)-(sum(x))^2/n][sum(y*y)-(sum(y))^2/n]
Notice that the correlation quality is independent of scale or offset.
   After all correlations are performed the disparity images (outputs) are
used to permit a second correlation in the reverse direction, from right to 
left. This should give results similar to the left to right operation, but 
never exactly, since the correlation is sub pixel and the template is 
always begun on an integer pixel boundary. Those points which match to within
THRESH pixels are kept. The rest are set to zero. This test will often locate
image areas where terrain is visible to one eye but not to the other.
Tiepoints in error are unlikely to match in both directions.
   Finally the user can invoke a gore removal scheme (see NOGORES keyword).
This step only interpolates over gores which are 1 line or 1 sample thick.

HISTORY:
11-1-2003  J Lorre. 
COGNIZANT PROGRAMMER:  Jean Lorre

.LEVEL1
.VARI INP
2 input images

.VARI OUT
2 Output images

.VARI NLW
template height

.VARI NSW
template width

.VARI MOTION
horizontal motion

.VARI QUALITY
minimum acceptable
quality

.VARI OFFSET
known vertical
image offset

.VARI GEOM
2 inputs:
1) vertical search
2) correlation width

.VARI THRESH
Direct-inverse
error limit.

.LEVEL2

.VARI INP
Two input images in the order:
1. Left stereo image.
2. Right stereo image.

.VARI OUT
Two Output disparity images in the order:
1. Line disparity.
2. Sample disparity.

.VARI NLW
nlw=  The line length of the template kernel.
      The template is the area selected from the left image and correlated
      over the right image search area. 
      The template is only moved horizontally.
      Must be an odd integer. 
      Defaults to 1

.VARI NSW
nsw=  The sample width of the template kernel. 
      The template is the area selected from the left image and correlated
      over the right image search area. Note that the left and right
      halves of the template are correlated independently. The resulting
      correlation quality is the larger of the two. This means that NSW
      is only 1/2 of the actual width used.
      The template is only moved horizontally.
      Must be an odd integer. 
      Defaults to 45

.VARI MOTION
motion= The maximum permitted horizontal motion of the template in the 
        search area from it's initial location in each direction.
        Thus the search area will be of width nsw+2*smotion.
        Searching begins from the initial offset and skew.
        Defaults to 10.

.VARI QUALITY
quality= The minimum acceptable correlation quality for a point to have
         in order to be accepted. 
         0 means no correlation at all.
         1 means perfect correlation.
         Defaults to 0.7

.VARI OFFSET
offset= The integer line offset between the 2 input images. 
        A positive offset of n indicate the right search area is to be shifted 
        down n lines relative to the left template in order to align the two
        input images.
        If defaulted, this parameter is computed by the program.

.VARI GEOM
geom=M,N
      Geom controls the way the program computes the initial geometric
      relationship between the 2 input images, resulting in the automatic
      computation of the vertica OFFSET parameter and of a table of
      horizontal shifts for each image line.
      M is the maximum permitted vertical offset search used to determine the
      OFFSET parameter. The search is limited to +- M lines.
      N is the template width used to correlate the data at the center of 
      each image line.
      The defaults are geom=6,120. 
      The vertical image shift operation is performed only if the 
      OFFSET keyword is defaulted.

.VARI THRESH
thresh= The threshold which determines the permissible disagreement in 
        horizontal shift between the direct and inverse correlations.
        Tiepoints with a disagreement of over THRESH are set to zero in the 
        output images.
        Note: When thresh gets below about 0.8 then many rejections 
              occur. This implies that the precision of the algorithm is
              beginning to be tested rather than the precision of the data.
        Defaults to 1.0

$ Return
$!#############################################################################
$Test_File:
$ create tstcorrelate1d.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"

! use MER Pancam images:
correlate1d inp=(/project/test_work/testdata/mer/l.img +
 /project/test_work/testdata/mer/r.img) +
 out=(ldis.img,sdis.img) geom=(6,80) quality=.7 nsw=41

! show distribution of offsets:
hist ldis.img nlin=17 lim=(0 1024)
hist sdis.img nlin=17 lim=(0 1024)

! this was the command that JJL had:
!correlate1d inp=(mer.ll,mer.rr) out=(ldis.img,sdis.img) geom=(6,80) +
!quality=.7 nsw=41

let $echo="no"
end-proc
$!-----------------------------------------------------------------------------
$ create tstcorrelate1d.log_solos
tstcorrelate1d
correlate1d inp=(/project/test_work/testdata/mer/l.img  +
 /project/test_work/testdata/mer/r.img)  +
 out=(ldis.img,sdis.img) geom=(6,80) quality=.7 nsw=41
Beginning VICAR task correlate1d
hist ldis.img nlin=17 lim=(0 1024)
Beginning VICAR task hist
HIST version 27-JUL-11

          0  534058    **************************************************  1
         64   11351    *************
        128   24325    *****************************
        192   31939    **************************************
        256   33463    ****************************************
        320   35412    *******************************************
        384   35658    *******************************************
        448   38017    **********************************************
        512   31581    **************************************
        576   36595    ********************************************
        640   40411    *************************************************
        704   35572    *******************************************
        768   38833    ***********************************************
        832   41163    **************************************************  2
        896   34799    ******************************************
        960   32356    ***************************************
       1024   13043    ***************

AVERAGE GRAY LEVEL=274.7704
STANDARD DEVIATION=335.5002
NUMBER ELEMENTS= 1048576
MIN. DN=0.000000
MAX. DN=1011.000

hist sdis.img nlin=17 lim=(0 1024)
Beginning VICAR task hist
HIST version 27-JUL-11

          0  533821    **************************************************  1
         64   10589    **********
        128   29571    ****************************
        192   41389    ****************************************
        256   48598    ***********************************************
        320   50767    *************************************************
        384   51289    **************************************************  2
        448   50047    ************************************************
        512   50235    ************************************************
        576   48284    ***********************************************
        640   41379    ****************************************
        704   34181    *********************************
        768   26199    *************************
        832   19782    *******************
        896   12445    ************

AVERAGE GRAY LEVEL=224.1262
STANDARD DEVIATION=271.8018
NUMBER ELEMENTS= 1048576
MIN. DN=0.000000
MAX. DN=926.0000

let $echo="no"
exit
slogoff
$!-----------------------------------------------------------------------------
$ create tstcorrelate1d.log_linux
tstcorrelate1d
correlate1d inp=(/project/test_work/testdata/mer/l.img  +
 /project/test_work/testdata/mer/r.img)  +
 out=(ldis.img,sdis.img) geom=(6,80) quality=.7 nsw=41
Beginning VICAR task correlate1d
hist ldis.img nlin=17 lim=(0 1024)
Beginning VICAR task hist
HIST version 27-JUL-11

          0  534076    **************************************************  1
         64   11347    *************
        128   24329    *****************************
        192   31941    **************************************
        256   33460    ****************************************
        320   35408    *******************************************
        384   35659    *******************************************
        448   38018    **********************************************
        512   31579    **************************************
        576   36594    ********************************************
        640   40411    *************************************************
        704   35572    *******************************************
        768   38830    ***********************************************
        832   41162    **************************************************  2
        896   34796    ******************************************
        960   32353    ***************************************
       1024   13041    ***************

AVERAGE GRAY LEVEL=274.7581
STANDARD DEVIATION=335.4951
NUMBER ELEMENTS= 1048576
MIN. DN=0.000000
MAX. DN=1011.000

hist sdis.img nlin=17 lim=(0 1024)
Beginning VICAR task hist
HIST version 27-JUL-11

          0  533842    **************************************************  1
         64   10586    **********
        128   29571    ****************************
        192   41388    ****************************************
        256   48594    ***********************************************
        320   50763    *************************************************
        384   51286    **************************************************  2
        448   50047    ************************************************
        512   50233    ************************************************
        576   48286    ***********************************************
        640   41379    ****************************************
        704   34179    *********************************
        768   26192    *************************
        832   19783    *******************
        896   12447    ************

AVERAGE GRAY LEVEL=224.1187
STANDARD DEVIATION=271.8013
NUMBER ELEMENTS= 1048576
MIN. DN=0.000000
MAX. DN=926.0000

let $echo="no"
exit
slogoff
$ Return
$!#############################################################################
