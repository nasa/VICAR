$!****************************************************************************
$!
$! Build proc for MIPL module snyder
$! VPACK Version 1.9, Wednesday, September 02, 1998, 16:25:39
$!
$! Execute by entering:		$ @snyder
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
$ write sys$output "*** module snyder ***"
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
$ write sys$output "Invalid argument given to snyder.com file -- ", primary
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
$   if F$SEARCH("snyder.imake") .nes. ""
$   then
$      vimake snyder
$      purge snyder.bld
$   else
$      if F$SEARCH("snyder.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake snyder
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @snyder.bld "STD"
$   else
$      @snyder.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create snyder.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack snyder.com -
	-s snyder.f -
	-i snyder.imake -
	-p snyder.pdf -
	-t tstsnyder.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create snyder.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c
c program auxiliary
c
      include 'VICMAIN_FOR'
      subroutine main44

      parameter (maxsamp=3600)
      parameter (max_nlimit=15, max_mlimit=15, max_klimit=15)
      real*4 obuf(maxsamp,4)
      real*4 lat,lon,line,sample
      integer*4 ounit(4),def,count,status
      logical xvptst,conformal,authalic
      character*100 archive
      character*12 planet
      real*8 a,b,c,inlat,inlon,outlat,outlon
      real*8 confcoef1(max_nlimit*max_mlimit)
      real*8 confcoef2(max_nlimit*max_mlimit)
      real*8 authcoef1((max_mlimit+1)*(max_klimit+1))
      real*8 authcoef2(max_nlimit)
 

c parameters
      call xvpcnt('INP',nids)      
      conformal=xvptst('CONFORMAL')
      authalic=xvptst('AUTHALIC')
      call xvparm('NL',nlo,count,def,1)
      call xvparm('NS',nso,count,def,1)
      call xvparm('ARCHIVE',archive,count,def,1)
      call xvparm('PATH',path,count,def,1)
      call xvparm('PLANET',planet,count,def,1)

c read coefficient file generated by TRICOEF for converting from
c conformal & authalic to centric coordinates
      call xvmessage('Reading TRICOEF archive '//archive,' ')
      call get_ellipsoid(archive,planet,a,b,c,
     +       confcoef1,confcoef2,authcoef1,authcoef2,
     +       nlimit,klimit,mlimit,ind)
      if(ind.ne.0)then
        call xvmessage('get_ellipsoid: error status',' ')
        call abend
      endif
      if(nlimit.gt.max_nlimit)then
        call xvmessage('get_ellipsoid: nlimit too large',' ')
        call abend
      endif
      if(klimit.gt.max_klimit)then
        call xvmessage('get_ellipsoid: klimit too large',' ')
        call abend
      endif
      if(mlimit.gt.max_mlimit)then
        call xvmessage('get_ellipsoid: mlimit too large',' ')
        call abend
      endif
      write(*,*)'Archive limits=',nlimit,klimit,mlimit
      write(*,*)'Archive radii=',a,b,c
 

c open 4 outputs
      do i=1,4
        call xvunit(ounit(i),'OUT',i,status,' ')
        call xvsignal(ounit(i),status,1)
        call xvopen(ounit(i),status,'U_FORMAT','REAL','O_FORMAT','REAL',
     +              'U_NL',nlo,'U_NS',nso,'OP','WRITE',' ')
        call xvsignal(ounit(i),status,1)
      enddo

c set auxiliary conversions
      if(conformal)then
        call xvmessage('Conformal auxiliary coord conversions',' ')
        infmt=4
        iofmt=1
       endif
      if(authalic)then
        call xvmessage('Authalic auxiliary coord conversions',' ')
        infmt=5
        iofmt=1
      endif
 
c Write output files
      c=c/a
      b=b/a
      a=1.d0
      do j=1,nlo
        line=j
        do i=1,nso
          sample=i

c         convert l,s to lat,lon (conformal or authalic)
          call xy2ll(line,sample,nlo,nso,lat,lon)

c         compute centric lat & lon from snyder coefficients
          inlat=lat
          inlon=360.d0-lon       ! convert to east lon
          call triaxtran(a,b,c,confcoef1,confcoef2,
     +     authcoef1,authcoef2,nlimit,
     +     klimit,mlimit,inlat,inlon,infmt,
     +     outlat,outlon,iofmt,ind)
          if(ind.gt.0)then
            call xvmessage('Triaxtran error',' ')
            call abend
          endif
          outlon=360.d0-outlon   ! convert to west lon

c         fix poles
          if(lat.gt.89.99)then
            outlat=90.
            outlon=lon
          endif
          if(lat.lt.-89.99)then
            outlat=-90.
            outlon=lon
          endif

          obuf(i,1)=outlat
          obuf(i,2)=outlon
          obuf(i,3)=outlat-lat
          obuf(i,4)=outlon-lon
          if(obuf(i,4).lt.-180.)obuf(i,4)=obuf(i,4)+360.
          if(obuf(i,4).gt.180.)obuf(i,4)=obuf(i,4)-360.
        enddo
        do k=1,4
          call xvwrit(ounit(k),obuf(1,k),status,' ')
          call xvsignal(ounit(k),status,1)
        enddo
      enddo                                  ! line loop

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

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create snyder.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM snyder

   To Create the build file give the command:

		$ vimake snyder			(VMS)
   or
		% vimake snyder			(Unix)


************************************************************************/


#define PROGRAM	snyder
#define R2LIB

#define MODULE_LIST snyder.f

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
$ create snyder.pdf
process help=*
PARM OUT TYPE=STRING COUNT=4
PARM PROPERTY TYPE=KEYWORD VALID=("CONFORMAL","AUTHALIC") +
  DEFAULT="CONFORMAL"
PARM NL TYPE=INTEGER COUNT=(0:1) DEFAULT=180
PARM NS TYPE=INTEGER COUNT=(0:1) DEFAULT=360
PARM PLANET     TYPE=(STRING,12) COUNT=1 +
 VALID=("phobos","deimos","gaspra") DEFAULT="phobos"
PARM ARCHIVE    TYPE=(STRING,80) COUNT=(0:1) +
   DEFAULT=TRIAXIAL_ARCHIVE.IBIS
END-PROC

.TITLE
VICAR program SNYDER

.HELP
PURPOSE:
To make images of the centric coordinates resulting from original auxiliary
coordinates.
Part of a package of programs including TRICOEF and MAPAUX to map project
irregularly shaped objects.

EXECUTION:
snyder out=(lat,lon,dlat,dlon)

where:

lat and lon are maps in rectangular projections the same size and format as
the input images. These images contain the planetocentric lat and lon
respectively which map to the conformal or authalic lat and lon of their pixel
coordinates. ie: the contents are the centric coordinates of the auxiliary
arguments.

dlat and dlon are images of the displacement of the computed centric 
coordinates from their initial auxiliary values.

.PAGE

METHOD:

The following steps are executed for every output pixel:
The auxiliary coordinates are computed from the image location.
Triaxtran computes the centric locations.

HISTORY:
9-1-98  J Lorre. 
COGNIZANT PROGRAMMER:  Jean Lorre

.LEVEL1

.VARI OUT
Output images
LAT (latitude)
LON (longitude)
Delta_lat
Delta_lon

.VARI PROPERTY
conformal or
authalic

.VARI NL
lines in output

.VARI NS
samples in output

.VARI PLANET
Planet name
 
.VARI ARCHIVE
Path name for
tricoef file.

.LEVEL2

.VARI OUT
Four outputs: Lat, Lon, Dlat, Dlon
Lat is the planetocentric latitude at the auxiliary argument location.
Lon is the planetocentric longitude at the auxiliary argument location.
Dlat is the latitude shift from auxiliary to centric
Dlon is the longitude shift from auxiliary to centric

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

.VARI PLANET
The planet or object name.
 
.VARI ARCHIVE
Path and filename for the Tricoef coefficient ibis file.

$ Return
$!#############################################################################
$Test_File:
$ create tstsnyder.pdf
procedure
refgbl $echo
body
let $echo="yes"
!
! For triaxial
tricoef radius=(13.1,11.2,9.2) +
 planet=phobos mlimit=7 nlimit=7 klimit=7
!
! conformal
snyder +
 out=(snyder_lat.img,snyder_lon.img,del_snyder_lat.img,del_snyder_lon.img) +
 nl=180 ns=360 planet=phobos 'conformal
!xvd snyder_lat.img
!xvd snyder_lon.img
!xvd del_snyder_lat.img
!xvd del_snyder_lon.img
!
! authalic
!snyder +
! out=(auth_snyder_lat.img,auth_snyder_lon.img,auth_del_snyder_lat.img, +
! auth_del_snyder_lon.img) +
! nl=180 ns=360 planet=phobos 'authalic
!xvd auth_snyder_lat.img
!xvd auth_snyder_lon.img
!xvd auth_del_snyder_lat.img
!xvd auth_del_snyder_lon.img
!
end-proc
$ Return
$!#############################################################################
