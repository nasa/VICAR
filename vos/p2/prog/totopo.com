$!****************************************************************************
$!
$! Build proc for MIPL module totopo
$! VPACK Version 1.9, Tuesday, April 03, 2001, 10:32:36
$!
$! Execute by entering:		$ @totopo
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
$ write sys$output "*** module totopo ***"
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
$ write sys$output "Invalid argument given to totopo.com file -- ", primary
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
$   if F$SEARCH("totopo.imake") .nes. ""
$   then
$      vimake totopo
$      purge totopo.bld
$   else
$      if F$SEARCH("totopo.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake totopo
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @totopo.bld "STD"
$   else
$      @totopo.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create totopo.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack totopo.com -mixed -
	-s totopo.f -
	-i totopo.imake -
	-p totopo.pdf -
	-t tsttotopo.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create totopo.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c
c program totopo
c
      include 'VICMAIN_FOR'
      subroutine main44

      include 'mp_for_defs'  !needed for MP software
      parameter (maxbuf=1000)
      character*12 planet/'            '/
      character*5 project(2),mode
      integer*4 fdsno,ilab(100,2),scr(1800),camera(2)
      integer*4 idata(40,2),tpt,inunit(2),count,def
      real*4 data(40,2),image_coords(4,32)
      real*4 lat,long,line,samp
      real*4 xyz(4,32),nodata,inten
      real*8 latrad,x,y,z,r8
      real*8 raddeg,rpole,req,MP(2)
      logical xvptst,debug/.false./,xyzmode/.false./
      real*4 inbuf(maxbuf*maxbuf)
      real*4 outbuf(maxbuf*maxbuf)
      equivalence (r8,idata,data)      

      raddeg=180.0d0/3.141592653589d0

c Get parameters:
      if(xvptst('DEBUG')) debug=.true.
      if(xvptst('XYZ')) xyzmode=.true.
      call xvparm('MODE',mode,count,def,1)
      call xvparm('NODATA',nodata,count,def,1)
      call xvparm('RADIUS',nw,count,def,1)
      call xvparm('MINPTS',minpts,count,def,1)
      call xvparm('MAXPASS',maxpass,count,def,1)

c open files
      call xvunit(inunit(1),'INP',1,status,' ')
      call xvopen(inunit(1),status,'IO_ACT','AS'
     +            ,'OPEN_ACT','AS','U_FORMAT','REAL',' ')
      call xvget(inunit(1),status,'NL',nl,'NS',np,' ')
      if(nl*np.gt.maxbuf*maxbuf)then
        write(*,*)'Max # input pixels is:',maxbuf*maxbuf
        call abend
      endif

      call xvunit(inunit(2),'INP',2,status,' ')
      call xvopen(inunit(2),status,'IO_ACT','AS'
     +            ,'OPEN_ACT','AS','U_FORMAT','REAL',' ')

      call xvunit(inmark,'INP',3,status,' ')
      call xvopen(inmark,status,'IO_ACT','AS','U_FORMAT','REAL'
     +            ,'OPEN_ACT','AS',' ')
      call xvget(inmark,status,'NL',nl3,'NS',ns3,' ')

      call xvunit(inxyz,'INP',4,status,' ')
      call xvopen(inxyz,status,'IO_ACT','AS','U_FORMAT','REAL'
     +            ,'OPEN_ACT','AS',' ')
      call xvget(inxyz,status,'NL',nl3,'NS',ns3,' ')

      call xvunit(outunit,'OUT',1,status,' ')
      call xvopen(outunit,status,'U_FORMAT','REAL','O_FORMAT',
     +            'REAL','OP','WRITE','OPEN_ACT','AS',
     +            'IO_ACT','AS',' ')


c Extract the navigation for the input images and put it in DATA.
c DATA(1-40,1) is for the Map projected first input image.
c DATA(1-40,2) is for the Map projected second input image.
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
            write(*,*)'Getlabcon: Fatal error',ind
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

      enddo

c Load input into memory
      if(mode.eq.'ORTHL')then
        image=1
        index=1
        do 160 lrec=1,nl
          i=(lrec-1)*np+1
          call xvread(inunit(1),inbuf(i),status,'LINE',lrec,' ')
160     continue
      else if(mode.eq.'ORTHR')then
        image=2
        index=3
        do 161 lrec=1,nl
          i=(lrec-1)*np+1
          call xvread(inunit(2),inbuf(i),status,'LINE',lrec,' ')
161     continue
      else if(mode.eq.'TOPO ')then
c       no image needed !
        image=1
      else if(mode.eq.'ERROR')then
c       no image needed !
        image=1
      else
        call mabend('Unrecognized mode')
      endif

c clear output buffer.
      call mve(7,nl*np,nodata,outbuf,0,1)

c close first 2 inputs.
      call xvclose(inunit(1),ind,' ')
      call xvclose(inunit(2),ind,' ')

      rpole=data(25,1) ! planet polar radius
      req=data(26,1)   ! planet equatorial radius
      slope=1.0
      offset=0.0

c Loop on coordinate records.
      do 140 lrec=1,nl3
        call xvread(inxyz,xyz,status,'LINE',lrec,' ')
        call xvread(inmark,image_coords,status,'LINE',lrec,' ')

c       Loop on tiepoints
        do 150 tpt=1,32
          if((xyz(1,tpt).eq.0.0).and.
     +       (xyz(2,tpt).eq.0.0)) goto 150

          if(xyzmode)then  ! convert from XYZ to LAT,LON
             x=xyz(1,tpt)
             y=xyz(2,tpt)
             z=xyz(3,tpt)
             latrad=datan2(z,dsqrt(x*x+y*y)) ! centric latitude
             lat=raddeg*latrad
             long=360.d0-raddeg*datan2(y,x)  ! W longitude
             if(long.gt.360.d0) long=long-360.d0
             range=dsqrt(x*x+y*y+z*z)
             rad=rpole*req/dsqrt(rpole*rpole*(dcos(latrad))**2+
     +                           req*req*(dsin(latrad))**2)
             xyz(1,tpt)=lat        ! Geocentric lat. deg.
             xyz(2,tpt)=long       ! W long deg.
             xyz(3,tpt)=range-rad  ! Elevation above geoid Km.
          endif

c         Convert lat,lon to input projection line,samp
c         This is the really correct line,sample.
          lat=xyz(1,tpt)
          long=xyz(2,tpt)
          call convev(ind,data(1,image),data(1,image),
     +                line,samp,lat,long,1,scr)
          if(ind.ne.0) goto 150

          if(debug)then
             write(*,*)'Projection lat,long,->line,samp'
             write(*,99) lat,long,line,samp
          endif

c         Check if in picture.
          iline=nint(line)
          isamp=nint(samp)
          if(isamp.lt.1.or.isamp.gt.np) goto 150
          if(iline.lt.1.or.iline.gt.nl) goto 150

          if(mode.eq.'TOPO ')then

c           Write the elevation to the output at the true location.
            i=(iline-1)*np+isamp
            elev=slope*xyz(3,tpt)+offset
            outbuf(i)=elev

          else if(mode.eq.'ERROR')then

c           Write the error to the output at the true location.
            i=(iline-1)*np+isamp
            elev=slope*xyz(4,tpt)+offset
            outbuf(i)=elev

          else

c           Get the DN value from the input location designated by the
c           original tiepoint line,sample pair.
            i=(image_coords(index,tpt)-1)*np+image_coords(index+1,tpt)
            inten=inbuf(i)

c           Write this intensity to the output at the true location.
            i=(iline-1)*np+isamp
            outbuf(i)=inten

          endif

150     continue
140   continue

c fillin gores
      call fill_gores(outbuf,inbuf,nl,np,nodata,
     +  nw,minpts,maxpass)

c     Write output.
      i=1-np
      do lrec=1,nl
        i=i+np
        call xvwrit(outunit,outbuf(i),status,'LINE',lrec,' ')
      enddo

99    format(4f10.5)

      return
      END

c**************************************************************
      subroutine fill_gores(buf,bufnew,nl,np,nodata,
     + nw,minpts,maxpass)
      real*4 buf(np,nl),bufnew(np,nl),nodata
      real*4 dn(1000),r(1000)
      integer*4 count,pass

c     Fillin the missing pixels.

      do j=1,nl
        do i=1,np
          bufnew(i,j)=nodata
        enddo
      enddo
      nl1=nl
      ns1=np
      rnw=nw*nw+.1 ! radius squared of collection circle + a bit
      pass=0
100   count=0
      pass=pass+1
      do j=1,nl1
        do i=1,ns1
          if(buf(i,j).ne.nodata)goto 10
          k=0
          do jj=max(j-nw,1),min(j+nw,nl1)
            j2=(j-jj)**2
            do ii=max(i-nw,1),min(i+nw,ns1)
              if(buf(ii,jj).ne.nodata)then
c               rad=(i-ii)**2+(j-jj)**2
                rad=(i-ii)**2+j2
                if(rad.le.rnw)then
                  k=k+1
                  dn(k)=buf(ii,jj)
                  r(k)=rad
                endif
              endif
            enddo
          enddo
          if(k.lt.minpts)goto 10
          sum1=0.0
          sum2=0.0
          do n=1,k
            sum1=sum1+dn(n)/r(n)
            sum2=sum2+1.0/r(n)
          enddo
          bufnew(i,j)=sum1/sum2
          count=count+1
10        continue
        enddo
      enddo
      do j=1,nl1
        do i=1,ns1
          if(bufnew(i,j).ne.nodata)then
            buf(i,j)=bufnew(i,j)
            bufnew(i,j)=nodata
          endif
        enddo
      enddo
      write(*,*)count,' pixels interpolated'
      if((count.gt.0).and.(pass.lt.maxpass))then
        goto 100
      endif

      return
      end

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create totopo.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM totopo

   To Create the build file give the command:

		$ vimake totopo			(VMS)
   or
		% vimake totopo			(Unix)


************************************************************************/


#define PROGRAM	totopo

#define MODULE_LIST totopo.f

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
$ create totopo.pdf
process help=*
PARM INP          TYPE=STRING       COUNT=4
PARM OUT          TYPE=STRING       COUNT=1
PARM DEBUG TYPE=KEYWORD VALID=(DEBUG,NODEBUG) DEFAULT=NODEBUG
PARM XYZ   TYPE=KEYWORD VALID=(XYZ,LATLON) DEFAULT=LATLON
PARM MODE  TYPE=(STRING,5) VALID=("TOPO ","ORTHL","ORTHR", +
           "ERROR")   DEFAULT="TOPO "
PARM NODATA TYPE=REAL DEFAULT=0.
PARM RADIUS TYPE=INTEGER COUNT=(0:1) VALID=(1:17) DEFAULT=13
PARM MINPTS TYPE=INTEGER COUNT=(0:1) VALID=(1:1000) DEFAULT=4
PARM MAXPASS TYPE=INTEGER COUNT=(0:1) VALID=(1:100000) DEFAULT=4
END-PROC

.TITLE
VICAR2 program TOTOPO

.HELP
Converts the tiepoint locations stored in a Mark file and written by
program TRACKER3 and XYZ coordinates created by program LSTOXYZ to one of:
1. A topomap.
2. An orthonormal view of the left image.
3. An orthonormal view of the right image.
4. An error map

A topomap is a map of elevation above a surface
(in this case above the geoid or oblate spheroid).
In this mode the correlator (tracker3) can be run with a wide grid spacing
such as : GRID=3 NLW=15 NSW=15.

An orthonormal image is an image viewed from directly above
(in the planetocentric sense) at every pixel. Orthonormal
views have no parallax. If you want good resolution on orthonormal
maps then the correlator (tracker3) should be executed on every pixel,
ie: parameter GRID=1.

An error map is a map of the miss distance of the two vectors
in the XYZ solution in program LSTOXYZ.

All output images are REAL.

.page
Example:
       TOTOPO left,right,points,xyz out parameters

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
         XYZ is a MARK file containing the xyz coordinates written by
              program LSTOXYZ.
              Mark files contain 512 byte records of real*4 data.
              Coordinates are stored in groups of 4 words in the
              order of either:lat,long,range-radius,error....
                or
                              x,y,z,error...
                  (see keyword XYZ)

          OUT is identical to LEFT but contains one of:
              1. A topomap.
              2. An orthonormal view of the left image.
              3. An orthonormal view of the right image.
              4. An error map.
              
All output images are REAL.

.page
METHOD:

The correct output location for any tiepoint is the lat,long
computed by program LSTOXYZ. This coordinate is converted 
to line,sample for the input projection. Then what goes to
that coordinate depends upon the mode:

In the TOPO mode the elevation of the surface above the geoid
is written to the output.

In the ORTHL or ORTHR modes the original line,samp (from the
output of TRACKER) point to the input DN value which is to be
placed into the output.

In the ERROR mode a map of the miss distance of the two vectors
in the XYZ solution in program LSTOXYZ is written to the output
after scaling.

.page
LIMITATIONS:

At the moment input & output pictures (not Mark files) 
cannot each exceed a total number of pixels of 1000 by 1000.

The ORTHR image will have the label of the left (first) input.

.page
OUTPUT UNITS:

  Topomaps are in kilometers (one DN = 1 kilometer) above the
planet oblate spheroid. Zero DN is at the geoid (zero elevation).
  The orthonormal images are just geometrically undistorted
replicas of the first two input images.

.page
EXAMPLE: 

         farenc in1
         farenc in2
         map3 in1 a
         map3 in2 b
         tracker a,b m
         lstoxyz a,b,m xyz
         totopo a,b,m,xyz out

! EXAMPLE OF BATCH STEREO RUN:
!
! First correct navigation.
FARENC INP=moon1.img OUT=g1.img +
 'update below=10 dnth=10 activi=45 GEOM=3 +
  area=(3,2,790,790)
dcl del g1.img;1
FARENC INP=moon2.img OUT=g2.img +
 'update below=10 dnth=10 activi=35 GEOM=3 +
  area=(3,2,790,790) cluster=(30,10)
dcl del g2.img;1
!
! Next convert the perspective views to IDENTICAL projections.
map3 INP=moon1.img OUT=moon1.map +
  'MERC nl=512 ns=512 sedrsrc=FARE scale=10. +
   line=256 samp=256 lat=0 long=70.
map3 INP=moon2.img OUT=moon2.map +
  'MERC nl=512 ns=512 sedrsrc=FARE scale=10. +
   line=256 samp=256 lat=0 long=70.
!
! Then collect a dense tiepoint grid (every point).
tracker inp=(gll1:[jjl320]moon1.map,gll1:[jjl320]moon2.map) +
   out=gll1:[jjl320]tiep.img +
   nlw=13 nsw=13 grid=1 'fit window=(2,2) +
   limit=(6,6) look=(2,2)
!
! Then convert tiepoints to lat,long,Del-radius,error values.
lstoxyz inp=(gll1:[jjl320]moon1.map,gll1:[jjl320]moon2.map, +
      gll1:[jjl320]tiep.img) out=gll1:[jjl320]xyz.img +
      sourcel=FARE sourcer=FARE

! Then create the topomap (meters above the geoid).
totopo inp=(gll1:[jjl320]moon1.map,gll1:[jjl320]moon2.map, +
      gll1:[jjl320]tiep.img, +
      gll1:[jjl320]xyz.img) out=gll1:[jjl320]topo.img mode=TOPO
!
! Last create left orthonormal view
totopo inp=(gll1:[jjl320]moon1.map,gll1:[jjl320]moon2.map, +
      gll1:[jjl320]tiep.img, +
      gll1:[jjl320]xyz.img) out=gll1:[jjl320]orth.img +
      mode=ORTHL


HISTORY
Written By: J Lorre			feb 91
Cognizant Programmer: J Lorre

.LEVEL1
.VARI INP
Four inputs:
#1 is the left image
#2 is the right image
#3 is the MARK file.
#4 is the XYZ file

.VARI OUT
Same size as input.
REAL format.

.VARI MODE
This dictates the 
nature of the output 
MODE=TOPO
MODE=ORTHL
MODE=ORTHR
MODE=ERROR
Default is TOPO.

.VARI DEBUG
Prints debugging data.

.VARI XYZ
Specifies the fourth
input is in XYZ.
(X,Y,Z,error)

.VARI NODATA
DN to initialize output.

.VARI RADIUS
Radius of circle
collecting points.
should be larger
than mark grid size.

.VARI MINPTS
Minimum number of points
within circle to permit
interpolation.

.VARI MAXPASS
Number of passes
through the image.

.LEVEL2

.VARI INP
There are Four input files.
File#1: This is the left or first input image given to
        program TRACKER.
File#2: This is the right or second input image given to
        program TRACKER.
File#3: This is the MARK file written by program TRACKER.
        It is 'REAL' format with 512 byte records containing
        pairs of tiepoints in the order:
        left_line,left_samp,right_line,right_samp.
File#4: This is the output of program LSTOXYZ containing either:
        X,Y,Z,ERROR or Lat,Lon,Range-radius,Error

.VARI OUT
The same size as the first input but contains either:
1. The topomap
2. The left input orthonormal view.
3. The right input orthonormal view.
REAL format.

.VARI MODE
This dictates the nature of the output file. Three outputs are
possible. These are:
MODE=TOPO  Which writes out an elevation map.
MODE=ORTHL Which writes an orthonormal version of the first (left)
           input.
MODE=ORTHR Which writes an orthonormal version of the second (right)
           input.
MODE=ERROR Which writes the miss distance between the two vectors
           solution for XYZ.
Default is TOPO.

.VARI DEBUG
Prints the line,sample and lat,long for each point in the input
Mark file as it is processed.

.VARI XYZ
Specifies that the Fourth input file will contain the (X,Y,Z,error)
values in planet coordinates rather than the
 (lat,long,range-radius,error) values which are the default.

.VARI NODATA
If no data is assigned to any pixel in the output file
(because of a sparse tiepoint grid) the un-assigned
pixels will be set to NODATA. Default is 0.0

.VARI RADIUS
Radius of circle within which points are collected to be used to
interpolate over the central pixel (if it is zero).
RADIUS should be greater than the grid size used in tracker3.

.VARI MINPTS
For interpolation to be performed a minimum of MINPTS must be
collected within the circle.

.VARI MAXPASS
Number of passes to be performed through the image. If no points
are interpolated then iteration ceases.

end-proc                                                                     
$ Return
$!#############################################################################
$Test_File:
$ create tsttotopo.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
!
! The following is included to clarify and to provide someone
! the means of restoring the SPICE Farenc navigation in case
! it goes away.
! Optical disk SITOD1 required...
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
!genthis out=mark.img nl=1 ns=20 format=real +
!      dn=(272.,262.,272.,262. +
!      200.,200.,200.,200., 200.,300.,200.,300., +
!      300.,200.,300.,200., 300.,300.,300.,300.)
! create topomap
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
! create topo map
totopo inp=( +
  /project/test_work/testdata/sitod1/test_data/images/moon1.map, +
  /project/test_work/testdata/sitod1/test_data/images/moon2.map, +
  mark.img,xyz.img) out=topo.img mode=TOPO
xvd topo.img
! create left orthonormal view
totopo inp=(+
  /project/test_work/testdata/sitod1/test_data/images/moon1.map, +
  /project/test_work/testdata/sitod1/test_data/images/moon2.map, +
  mark.img,xyz.img) out=ortho.img mode=ORTHL
xvd ortho.img
!
end-proc
$ Return
$!#############################################################################
