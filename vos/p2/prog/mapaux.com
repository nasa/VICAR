$!****************************************************************************
$!
$! Build proc for MIPL module mapaux
$! VPACK Version 1.9, Wednesday, September 02, 1998, 16:25:53
$!
$! Execute by entering:		$ @mapaux
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
$ write sys$output "*** module mapaux ***"
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
$ write sys$output "Invalid argument given to mapaux.com file -- ", primary
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
$   if F$SEARCH("mapaux.imake") .nes. ""
$   then
$      vimake mapaux
$      purge mapaux.bld
$   else
$      if F$SEARCH("mapaux.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake mapaux
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @mapaux.bld "STD"
$   else
$      @mapaux.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create mapaux.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack mapaux.com -
	-s mapaux.f -
	-i mapaux.imake -
	-p mapaux.pdf -
	-t tstmapaux.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create mapaux.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c
c program mapiso
c
      include 'VICMAIN_FOR'
      subroutine main44
      include 'mp_for_defs'

      parameter ( maxsamp=2000, max_ns_model=1440, max_nl_model=720)
      parameter ( max_nl=180, max_ns=360)
      character*100 filename,path,msg
      character*12 planet
      character*30 projection_name
      integer*2 obuf1(maxsamp),object(max_ns_model,max_nl_model)
      integer*4 ounit(2),def,count
      integer*4 unit,inunit(4),status,nl(4),ns(4)
      real*4 cent_lat(max_ns,max_nl),cent_lon(max_ns,max_nl)
      real*4 tissot(max_ns,max_nl),obuf2(maxsamp)
      real*8 map_lin,map_sam,map_lat,map_lon
      real*8 radius(3),mp
      logical xvptst,north,south

c parameters
      call xvpcnt('INP',nids)      
      call xvpcnt('OUT',nods)      
      call xvparm('PATH',path,count,def,1)
      call xvparm('PLANET',planet,count,def,1)
      call xvparm('GRID',grid,count,def,1)
      north=xvptst('NORTH')
      south=xvptst('SOUTH')

c read object projection into memory
c This is the DN map of the object stored as a rectangular projection
c with west longitude and zero longitude and latitude in the center..
      call projectionmodel(filename,path,planet)
      call xvmessage('Reading object projection '//filename,' ')
      call xvunit(unit,'OLD',1,status,'U_NAME',filename,' ')
      call xvsignal(unit,status,1)
      call xvopen(unit,status,'U_FORMAT','HALF',' ')
      call xvsignal(unit,status,1)
      call xvget(unit,status,'NL',nl_map,'NS',ns_map,' ')
      call xvsignal(unit,status,1)
      if(ns_map.gt.max_ns_model)then
        call xvmessage('Model storage buffer too small',' ')
        call abend
      endif
      if(nl_map.gt.max_nl_model)then
        call xvmessage('Model storage buffer too small',' ')
        call abend
      endif
      do line=1,nl_map                         ! line loop
        call xvread(unit,object(1,line),status,'LINE',line,' ')
        call xvsignal(unit,status,1)
      enddo
      call xvclose(unit,status,'CLOS_ACT','FREE',' ')
      write(msg,*)'Map dimensions nl=',nl_map,' ns=',ns_map
      call xvmessage(msg,' ')

c draw grid on map
      if(grid.gt..001) call grid_it(grid,object,nl_map,ns_map,
     +                              max_nl_model,max_ns_model)

c open inputs
      do i=1,nids
        call xvunit(inunit(i),'INP',i,status,' ')
        call xvsignal(inunit(i),status,1)
        call xvopen(inunit(i),status,'U_FORMAT','REAL',' ')
        call xvsignal(inunit(i),status,1)
        call xvget(inunit(i),status,'NL',nl(i),'NS',ns(i),' ')
        call xvsignal(inunit(i),status,1)
        if(i.gt.1)then
          if(ns(i).gt.max_ns)then
            call xvmessage('Line length too long on files 2-4',' ')
            call abend
          endif
          if(nl(i).gt.max_nl)then
            call xvmessage('Too many lines on files 2-4',' ')
            call abend
          endif
        endif
      enddo

c load inputs 2-3 into buffers
      do line=1,nl(2)                         ! line loop
        call xvread(inunit(2),cent_lat(1,line),status,'LINE',line,' ')
        call xvsignal(inunit(2),status,1)
      enddo
      do line=1,nl(3)                         ! line loop
        call xvread(inunit(3),cent_lon(1,line),status,'LINE',line,' ')
        call xvsignal(inunit(3),status,1)
      enddo
      do line=1,nl(4)                         ! line loop
        call xvread(inunit(4),tissot(1,line),status,'LINE',line,' ')
        call xvsignal(inunit(4),status,1)
      enddo

c open outputs
      nlo=nl(1)
      nso=ns(1)
      do i=1,nods
        call xvunit(ounit(i),'OUT',i,status,' ')
        call xvsignal(ounit(i),status,1)
        if(i.eq.1)
     +    call xvopen(ounit(i),status,'U_FORMAT','HALF',
     +              'U_NL',nlo,'U_NS',nso,'OP','WRITE',' ')
        if(i.eq.2)
     +    call xvopen(ounit(i),status,'U_FORMAT','REAL',
     +          'O_FORMAT','REAL','U_NL',nlo,'U_NS',nso,
     +          'OP','WRITE',' ')
        call xvsignal(ounit(i),status,1)
      enddo

c initialize mp routines
      if(north.or.south) goto 11
      call mp_init( mp,istat) 
      if(istat.ne.mp_success) call mabend('error in mp_init')
      call mp_label_read( mp, inunit(1), istat)
      if(istat.ne.mp_success) call mabend(' mp_label_read error')

c extract output projection properties
      call mp_get_value_str(mp,'MAP_PROJECTION_TYPE',
     +  projection_name,istat)
      if(istat.ne.mp_success) call mabend(' mp_get_value_str error')
      call mp_get_value(mp,'A_AXIS_RADIUS',radius(1),istat)
      if(istat.ne.mp_success) call mabend(' mp_get_value error')
      call mp_get_value(mp,'B_AXIS_RADIUS',radius(2),istat)
      if(istat.ne.mp_success) call mabend(' mp_get_value error')
      call mp_get_value(mp,'C_AXIS_RADIUS',radius(3),istat)
      if(istat.ne.mp_success) call mabend(' mp_get_value error')

c check for equal radii in map3 picture label
      if((radius(1).ne.radius(2)).or.(radius(1).ne.radius(3)))then
        call xvmessage('Input label radii must be equal',' ')
        call abend
      endif
11    continue

c process image

c     Process every pixel
      radtodeg=57.2957795
      scale=nso/180.
      do line=1,nlo                         ! line loop
        map_lin=line
        do i=1,nso                        ! pixel loop
          map_sam=i
  
c         Convert output line & sample to west auxiliary lat & lon.
          if(north.or.south)then
            radiuss=sqrt((nlo/2.0-line)**2+(nso/2.0-i)**2)
c            if(radiuss.gt.nso/2)then
c                dn=0
c                obuf2(i)=0.0
c                ind=1
c                goto 10
c            endif
            aux_lon=radtodeg*atan2(nso/2.0-i,line-nlo/2.0)
            if(aux_lon.lt.0.0)aux_lon=aux_lon + 360.
            aux_lat=90.-radiuss/scale
            if(south)then
              aux_lon=360.-aux_lon+180.
              if(aux_lon.gt.360.)aux_lon=aux_lon-360.
              aux_lat=-aux_lat
            endif
          else
            call mp_xy2ll(mp,map_lin,map_sam,map_lat,map_lon,1,status)
            if(status.ne.0)then
              if(status.eq.-1)then
                dn=0
                obuf2(i)=0.0
                ind=1
                goto 10
              endif
              call xvmessage('mp_xy2ll: fatal status',' ')
              write(msg,*)'Status =',status
              call xvmessage(msg,' ')
              call abend
            endif
            aux_lat=map_lat
            aux_lon=map_lon
          endif

c         Convert auxiliary lat & lon to line & sample in the lat/lon
c         lookup table (input images 2 & 3).
          call ll2xy(aux_lat,aux_lon,nl(2),ns(2),rline,sample)

c         Convert auxiliary lat & lon to planetocentric lat & lon
c         Latitude
          call get_dnr(rline,sample,cent_lat,nl(2),ns(2),dn,ind,
     +          max_nl,max_ns)
          centric_lat=dn
c         Longitude
          call get_dnlon(rline,sample,cent_lon,nl(3),ns(3),dn,ind,
     +          max_nl,max_ns)
          centric_lon=dn

c         Get the tissot angle at this location.
          call get_dnr(rline,sample,tissot,nl(4),ns(4),dn,ind,
     +          max_nl,max_ns)
          obuf2(i)=dn

c         Convert centric lat & lon into image coordinates for the object map
          call ll2xy(centric_lat,centric_lon,nl_map,ns_map,
     +         rline,sample)
 
c         and return the DN value at this planetocentric coordinate.
          call get_dni(rline,sample,object,nl_map,ns_map,
     +         dn,ind,max_nl_model,max_ns_model)

10        obuf1(i)=nint(dn)

        enddo                                ! pixel loop
        call xvwrit(ounit(1),obuf1,status,' ')
        call xvsignal(ounit(1),status,1)
        call xvwrit(ounit(2),obuf2,status,' ')
        call xvsignal(ounit(2),status,1)
      enddo                                  ! line loop

      return
      end


c********************************************************************
      subroutine grid_it(grid,object,nl_map,ns_map,
     +       max_nl_model,max_ns_model)
c draw grid on image

      integer*2 object(max_ns_model,max_nl_model)
      real*4 sample,line,lat,lon

      lon=-grid
10    lon=lon+grid

c       convert lat/lon to line/sample
        t=(ns_map+1.0)/2.0
        if(lon.gt.180.)then
           sample=-(ns_map-t)*lon/180. + 2.0*ns_map - t
        else
           sample=-(t-1.0)*lon/180.+t
        endif
        i=nint(sample)
        if((i.ge.1).and.(i.le.ns_map))then
          do k=1,nl_map
             object(i,k)=255
          enddo
        endif
      if(lon+grid.lt.360.)goto 10

      lat=-90-grid
20    lat=lat+grid

c       convert lat/lon to line/sample
        line=(1.0-nl_map)*(lat-90.0)/180. + 1.0
        j=nint(line)
        if((j.ge.1).and.(j.le.nl_map))then
          do k=1,ns_map
             object(k,j)=255
          enddo
        endif
      if(lat+grid.lt.90.)goto 20

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
      subroutine get_dnlon(line,sample,object,nl,ns,dn,ind,
     +          maxline,maxsamp)
c return the longitude value at this image coordinate.
c ind=0 ok
c ind=1 off map, dn=0
 
      real*4 object(maxsamp,maxline),dn
      real*4 sample,line
 
c if are inside picture interpolate
      i=sample
      j=line
      if((i.ge.1).and.(i.lt.ns).and.(j.ge.1).and.
     + (j.lt.nl))then
         if(abs(object(i,j)-object(i+1,j)).gt.180.)goto 10
         if(abs(object(i,j)-object(i,j+1)).gt.180.)goto 10
         if(abs(object(i,j)-object(i+1,j+1)).gt.180.)goto 10

c        No 360 wraparound condition.
         wt=sample-i
         dntop=wt*object(i+1,j)+(1.0-wt)*object(i,j)
         dnbot=wt*object(i+1,j+1)+(1.0-wt)*object(i,j+1)
         dn=(line-j)*dnbot+(j+1-line)*dntop
         ind=0
         return

c        Have a 360 wraparound condition.
10       continue
         t1=object(i,j)
         t2=object(i+1,j)
         t3=object(i,j+1)
         t4=object(i+1,j+1)
         if(t1.lt.180.)t1=t1+360.
         if(t2.lt.180.)t2=t2+360.
         if(t3.lt.180.)t3=t3+360.
         if(t4.lt.180.)t4=t4+360.
         wt=sample-i
         dntop=wt*t2+(1.0-wt)*t1
         dnbot=wt*t4+(1.0-wt)*t3
         dn=(line-j)*dnbot+(j+1-line)*dntop
         if(dn.gt.360.)dn=dn-360.
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

c********************************************************************
      subroutine get_dnr(line,sample,object,nl,ns,dn,ind,
     +          maxline,maxsamp)
c return the REAL DN value at this image coordinate.
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

c********************************************************************
      subroutine get_dni(line,sample,object,nl,ns,dn,ind,
     +          maxline,maxsamp)
c return the INTEGER DN value at this image coordinate.
c ind=0 ok
c ind=1 off map, dn=0
 
      integer*2 object(maxsamp,maxline)
      real*4 sample,line,dn
 
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


c********************************************************************
      subroutine projectionmodel(filename,path,planet)
c build projection filename
      character*100 filename,path
      character*12 planet
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
      do i=1,12
        if(planet(i:i).ne.' ')then
           j=j+1
           filename(j:j)=planet(i:i)
        else
           goto 2
        endif
      enddo
2     filename(j+1:j+5)='.img'
      return
      end

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create mapaux.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM mapaux

   To Create the build file give the command:

		$ vimake mapaux			(VMS)
   or
		% vimake mapaux			(Unix)


************************************************************************/


#define PROGRAM	mapaux
#define R2LIB

#define MODULE_LIST mapaux.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define USES_C

#define FTNINC_LIST mp_for_defs
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
$ create mapaux.pdf
process help=*
PARM INP TYPE=STRING COUNT=4
PARM OUT TYPE=STRING COUNT=2
PARM PLANET     TYPE=(STRING,12) COUNT=1 +
 VALID=("phobos","deimos","gaspra") DEFAULT="phobos"
PARM PATH    TYPE=(STRING,80) COUNT=(0:1) +
   DEFAULT=/project/test_work/testdata/general
PARM GRID TYPE=REAL COUNT=(0,1) DEFAULT=0
PARM MODE TYPE=KEYWORD VALID=(north,south,none) DEFAULT=none
END-PROC

.TITLE
VICAR program MAPAUX

.HELP
PURPOSE:
To map project irregularly shaped objects (ISO's) such as asteroids, 
comet nuclei, and planetesimals.
Support programs include: TRICOEF SNYDER or
                          EFGISO AUXILIARY or
                          AREAISO

EXECUTION:
mapaux inp=(target,lat,lon,tissot) out=(projection,proj_of_tissot)

where:

target is the desired output image label created by map3.
lat is the auxiliary to centric latitude mapping created by program auxiliary.
lon is the auxiliary to centric longitude mapping created by program auxiliary.
tissot is the mapping residual tissot angle.

projection is the generated projection with the identical label to target.map
proj_of_tissot is the tissot angle in degrees projected the same as projection
but is a REAL image.

If the first input has no map3 label then the Cheng equal area projection
will be made with mode of north or south. This image covers the north or
south hemispheres and uses the size of the input file.
You must specify either north or south keywords in this case.

.PAGE

METHOD:

HISTORY:
9-1-98  J Lorre. 
COGNIZANT PROGRAMMER:  Jean Lorre

.LEVEL1
.VARI INP
Input projection label.
latitude mapping.
longitude mapping.
tissot angle.

.VARI OUT
Output projection.
Output tissot angle.

.VARI PLANET
Planet name

.VARI PATH
Directory path for
planet models

.VARI GRID
Lat & lon grid
interval.

.VARI MODE
mode=north or
mode=south
no input projection.
Cheng equal area.

.LEVEL2
.VARI INP
Four inputs:
1. The desired output image label created by map3.
2. The auxiliary to centric latitude mapping created by program auxiliary.
3. The auxiliary to centric longitude mapping created by program auxiliary.
4. The mapping residual tissot angle.

.VARI OUT
Two outputs:
1. Output projection with the identical label to the input.
2. The same projection but of the tissot angle.

.VARI PLANET
The planet or object name.

.VARI PATH
The directory name where the planet models reside.

.VARI GRID
Lat & lon grid interval to draw on the planetocentric data before projection..

.VARI MODE
If the input has no map3 label you must specify either 'north or 'south
to specify the hemisphere to be projected using Chen's polar equal area
projection.
$ Return
$!#############################################################################
$Test_File:
$ create tstmapaux.pdf
procedure
refgbl $echo
body
let $echo="yes"
!
!gen out=b.img nl=10 ns=10
!
!MAP3 b.img ster_target.img NL=500 NS=500 'STEREOGR SCAL=.1 +
!   LINE=250 SAMP=250 LATI=90 LONG=0 +
!  nora=0 'NOGEOM 'NOSEDR 'NOPROJEC radii=(10,10,10) +
! ommatrix=(1.,0.,0.,0.,1.,0.,0.,0.,1.) rsvector=(1.,1.,1.)
!
! conformal using snyder values
!mapaux inp=(ster_target.img,snyder_lat.img,snyder_lon.img,tissot.img) +
! out=(a.img,angle.img) planet=phobos grid=30.
!overlay inp=a.img out=ster_aux.img maxdn=0 dla1=30 dlo1=30 dla2=30 dlo2=30 +
! 'nonumber
!xvd ster_aux.img
!
! conformal using auxiliary values
!mapaux inp=(ster_target.img,cen_lat.img,cen_lon.img,tissot.img) +
! out=(a.img,angle.img) planet=phobos grid=10.
!overlay inp=a.img out=ster_aux.img maxdn=0 dla1=10 dlo1=10 dla2=10 dlo2=10 +
! 'nonumber
!concat inp=(ster_aux.img,angle.img) out=ster_mos.img percent=3. ns=1000
!xvd ster_mos.img
!
!MAP3 b.img sinu_target.img NL=350 NS=750 'SINUSOID SCAL=.1 +
!   LINE=175 SAMP=375 LATI=0 LONG=0 NORANGLE=0 +
!  'NOGEOM 'NOSEDR 'NOPROJEC radii=(11.144,11.144,11.144) +
! ommatrix=(1.,0.,0.,0.,1.,0.,0.,0.,1.) rsvector=(1.,1.,1.)
!
! authalic using snyder values
!mapaux inp=(sinu_target.img,auth_snyder_lat.img,auth_snyder_lon.img, +
! sinu_triax_resid.img) +
! out=(a.img,sinu_resid_proj.img) planet=phobos grid=10.
!overlay inp=a.img out=sinu_aux.img maxdn=0 dla1=10 dlo1=10 dla2=10 dlo2=10 +
! 'nonumber
!concat inp=(sinu_aux.img,sinu_resid_proj.img) +
! out=sinu_mos.img percent=3. ns=750
!xvd sinu_mos.img
!barne_r inp=sinu_mos.img miplbox=21 primary=42057
!
! authalic using auxiliary values
!mapaux inp=(sinu_target.img,sinu_cen_lat.img,sinu_cen_lon.img,sinu_resid.img) +
! out=(a.img,sinu_resid_proj.img) planet=phobos grid=10.
!overlay inp=a.img out=sinu_aux.img maxdn=0 dla1=10 dlo1=10 dla2=10 dlo2=10 +
! 'nonumber
!concat inp=(sinu_aux.img,sinu_resid_proj.img) +
! out=sinu_mos.img percent=3. ns=750
!xvd sinu_mos.img
!barne_r inp=sinu_mos.img miplbox=21 primary=42057
!
! authalic from Cheng's solution
gen out=b.img nl=500 ns=500
areaiso out= +
 (area_lat.img,area_lon.img,area_map.img,angle_map.img,del_area_lat.img, +
 del_area_lon.img) +
 planet=phobos 'north
mapaux inp=(b.img,area_lat.img,area_lon.img,area_map.img) +
 out=(n_area_projection.img,n_area_residual.img) grid=10 'north
areaiso out= +
 (area_lat.img,area_lon.img,area_map.img,angle_map.img,del_area_lat.img, +
 del_area_lon.img) +
 planet=phobos 'south
mapaux inp=(b.img,area_lat.img,area_lon.img,area_map.img) +
 out=(s_area_projection.img,s_area_residual.img) grid=10 'south
astrtchr inp=n_area_residual.img out=x.img excl=(-1000.,.1)
astrtchr inp=s_area_residual.img out=y.img excl=(-1000.,.1)
concat inp=(n_area_projection.img,x.img, +
 s_area_projection.img,y.img) out=area_projection.img +
 ns=1000
xvd area_projection.img
!
! Phobos auxiliary conformal solution
gen out=b.img nl=10 ns=10
MAP3 b.img north_target.img NL=500 NS=500 'STEREOGR SCAL=.1 +
   NORANGLE=0 LINE=250 SAMP=250 LATI=90 LONG=0 +
  'NOGEOM 'NOSEDR 'NOPROJEC radii=(10,10,10) +
 ommatrix=(1.,0.,0.,0.,1.,0.,0.,0.,1.) rsvector=(1.,1.,1.) +
 cscale=1. focl=1.
MAP3 b.img south_target.img NL=500 NS=500 'STEREOGR SCAL=.1 +
   NORANGLE=0 LINE=250 SAMP=250 LATI=-90 LONG=0 +
  'NOGEOM 'NOSEDR 'NOPROJEC radii=(10,10,10) +
 ommatrix=(1.,0.,0.,0.,1.,0.,0.,0.,1.) rsvector=(1.,1.,1.) +
 cscale=1. focl=1.
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
auxiliary inp=( +
 E6.img,F6.img,G6.img, +                   
 E5.img,F5.img,G5.img, +
 E4.img,F4.img,G4.img, +
 E3.img,F3.img,G3.img, +
 E2.img,F2.img,G2.img, +
 E.img,F.img,G.img,SE.img,SF.img,SG.img) +
 nl=180 ns=360 out=(cen_lat.img,cen_lon.img,tissot.img,+
 del_lat.img,del_lon.img) loop=10 range=.2
mapaux inp=(north_target.img,cen_lat.img,cen_lon.img,tissot.img) +
 out=(a.img,n_tissot.img) planet=phobos grid=10.
overlay inp=a.img out=n_ster_aux.img maxdn=0 dla1=10 dlo1=10 dla2=10 dlo2=10 +
 'nonumber
mapaux inp=(south_target.img,cen_lat.img,cen_lon.img,tissot.img) +
 out=(b.img,s_tissot.img) planet=phobos grid=10.
overlay inp=b.img out=s_ster_aux.img maxdn=0 dla1=10 dlo1=10 dla2=10 dlo2=10 +
 'nonumber
concat inp=(a.img,b.img,n_ster_aux.img,s_ster_aux.img, +
 n_tissot.img,s_tissot.img) +
 out=ster_mos.img percent=3. ns=1000
xvd ster_mos.img
!
! Phobos auxiliary authalic solution
gen out=b.img nl=10 ns=10
MAP3 b.img sinu_target.img NL=350 NS=750 'SINUSOID SCAL=.1 +
   LINE=175 SAMP=375 LATI=0 LONG=0 NORANGLE=0 +
  'NOGEOM 'NOSEDR 'NOPROJEC radii=(11.7,11.7,11.7) +
 ommatrix=(1.,0.,0.,0.,1.,0.,0.,0.,1.) rsvector=(1.,1.,1.) +
 cscale=1. focl=1.
efgiso out=(E.img,F.img,G.img,R.img) planet=phobos nl=180 ns=360
efgiso out=(SE.img,SF.img,SG.img,SR.img) planet=phobos nl=180 ns=360 +
  triaxial=(11.7,11.7,11.7)
auxiliary inp=( +
 E6.img,F6.img,G6.img, +
 E5.img,F5.img,G5.img, +
 E4.img,F4.img,G4.img, +
 E3.img,F3.img,G3.img, +
 E2.img,F2.img,G2.img, +
 E.img,F.img,G.img,SE.img,SF.img,SG.img) +
 nl=180 ns=360 out=(sinu_cen_lat.img,sinu_cen_lon.img,sinu_resid.img,+
 sinu_del_lat.img,sinu_del_lon.img) loop=10 range=.2 'authalic
mapaux inp=(sinu_target.img,sinu_cen_lat.img,sinu_cen_lon.img,sinu_resid.img) +
 out=(a.img,sinu_resid_proj.img) planet=phobos grid=10.
overlay inp=a.img out=sinu_aux.img maxdn=0 dla1=10 dlo1=10 dla2=10 dlo2=10 +
 'nonumber
concat inp=(a.img,sinu_aux.img,sinu_resid_proj.img) +
 out=sinu_mos.img percent=3. ns=750
xvd sinu_mos.img
!
end-proc
$ Return
$!#############################################################################
