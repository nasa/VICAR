$!****************************************************************************
$!
$! Build proc for MIPL module ephemeris
$! VPACK Version 1.9, Wednesday, March 10, 2010, 12:13:15
$!
$! Execute by entering:		$ @ephemeris
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
$ write sys$output "*** module ephemeris ***"
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
$ write sys$output "Invalid argument given to ephemeris.com file -- ", primary
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
$   if F$SEARCH("ephemeris.imake") .nes. ""
$   then
$      vimake ephemeris
$      purge ephemeris.bld
$   else
$      if F$SEARCH("ephemeris.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake ephemeris
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @ephemeris.bld "STD"
$   else
$      @ephemeris.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create ephemeris.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack ephemeris.com -mixed -
	-s ephemeris.f -
	-i ephemeris.imake -
	-p ephemeris.pdf -
	-t tstephemeris.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ephemeris.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c
c program ephemeris
c
      include 'VICMAIN_FOR'
      subroutine main44

      implicit real*8(a-h,o-z)
      character*100 msg,epoch,spicefile,object,viewer
      integer*4 def,count,object_num,viewer_num,h(2)
      integer*4 num,vnum
      logical failed
      real*8 lon,lat,light_time,state(6),tibf(3,3)

      call xvmessage('EPHEMERIS version 13 Sept 1998',' ')
      call init_spice

      call xvparm('SPICEFILE',spicefile,count,def,1)
      call spklef(spicefile,h(1))
      if (failed()) goto 900
c get parameters
      call xvparm('EPOCH',epoch,count,def,1)
      call xvparm('OBJECT',object,count,def,1)
      call xvparm('VIEWER',viewer,count,def,1)

c reads times like: asfd19920112044820a.vic
c with the time-stamp:  yyyymmddhhmmss
      i=index(epoch,'.vic')
      if(i.gt.0)then
        msg(1:2)=epoch(9:10) ! month
        msg(3:3)='/'
        msg(4:5)=epoch(11:12) ! day
        msg(6:6)='/'
        msg(7:10)=epoch(5:8) ! year
        msg(11:11)=' '
        msg(12:13)=epoch(13:14) ! hour
        msg(14:14)=':'
        msg(15:16)=epoch(15:16) ! min
        msg(17:17)=':'
        msg(18:19)=epoch(17:18) ! sec
        msg(20:100)='.'
        epoch(1:100)=msg(1:100)
      endif

c convert epoch to ephemeris time
      call utc2et(epoch,ephemeris_time)
      if (failed()) goto 901
c get the planet id #
      if(object.eq.'sun')object_num=10
      if(object.eq.'mercury')object_num=199
      if(object.eq.'venus')object_num=299
      if(object.eq.'earth')object_num=399
      if(object.eq.'moon')object_num=301
      if(object.eq.'mars')object_num=499
      if(object.eq.'jupiter')object_num=599
      if(object.eq.'saturn')object_num=699
      if(object.eq.'uranus')object_num=799
      if(object.eq.'neptune')object_num=899
      if(object.eq.'pluto')object_num=999
      if(viewer.eq.'sun')viewer_num=10
      if(viewer.eq.'mercury')viewer_num=199
      if(viewer.eq.'venus')viewer_num=299
      if(viewer.eq.'earth')viewer_num=399
      if(viewer.eq.'moon')viewer_num=301
      if(viewer.eq.'mars')viewer_num=499
      if(viewer.eq.'jupiter')viewer_num=599
      if(viewer.eq.'saturn')viewer_num=699
      if(viewer.eq.'uranus')viewer_num=799
      if(viewer.eq.'neptune')viewer_num=899
      if(viewer.eq.'pluto')viewer_num=999

c compute target state light time corrected at epoch from observer
      call spkez(object_num,ephemeris_time,'J2000','LT',viewer_num,
     +   state,light_time)
      if (failed()) then
         num = object_num
         vnum = viewer_num
         if (object_num.gt.10) num=object_num/100
         if (viewer_num.gt.10) vnum=viewer_num/100
         call xvmessage('Planet ephemerides not found',' ')
         call xvmessage('Using barycenter ephemerides',' ')
         call reset1
         call spkez(num,ephemeris_time,'J2000','LT',vnum,
     +      state,light_time)
         if (failed()) goto 902
      endif
c compute inertial to body-fixed rotation matrix tibf
      call bodmat(object_num,ephemeris_time - light_time,tibf)

c reverse state vector to from target to observer
      call vminus(state,state)

c rotate state into body-fixed
      call mxv(tibf,state,state)

c compute range to target, latitude & longitude of sub point
      call reclat(state,range,lon,lat)
      sclat=lat*dpr()
      sclon=lon*dpr()
      sclon=360.-sclon               ! convert to west
      if(sclon.gt.360.) sclon=sclon-360.
      if(sclon.lt.0.) sclon=sclon+360.

      write(msg,*)'Object center centric latitude=',sclat,' degrees'
      call xvmessage(msg,' ')
      write(msg,*)'Object center west longitude=',sclon,' degrees'
      call xvmessage(msg,' ')
      write(msg,*)'Object range= ',range,' km'
      call xvmessage(msg,' ') 
      return

  900 call xvmessage('***Err loading SP kernel',' ')
      goto 999
  901 call xvmessage('***Err converting UTC to ET',' ')
      goto 999
  902 call xvmessage('***Err getting ephemeris data',' ')
  999 call xvmessage('***Ephemeris task canceled',' ')
      call abend
      end
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create ephemeris.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM ephemeris

   To Create the build file give the command:

		$ vimake ephemeris			(VMS)
   or
		% vimake ephemeris			(Unix)


************************************************************************/


#define PROGRAM	ephemeris
#define R2LIB

#define MODULE_LIST ephemeris.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_NETWORK
#define LIB_SPICE
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create ephemeris.pdf
process help=*
PARM OBJECT TYPE=STRING COUNT=(0:1) +
 VALID=("sun","mercury","venus","moon","earth","mars","jupiter", +
        "saturn","uranus","neptune","pluto") +
 DEFAULT="sun"
PARM VIEWER TYPE=STRING COUNT=(0:1) +
 VALID=("sun","mercury","venus","moon","earth","mars","jupiter", +
        "saturn","uranus","neptune","pluto") +
 DEFAULT="earth"
PARM EPOCH TYPE=(STRING,80) COUNT=1
PARM SPICEFILE TYPE=(STRING,80) COUNT=(0:1) +
 DEFAULT=/project/spice/ops/generic_kernels/spk/de405.bsp
END-PROC

.TITLE
VICAR program EPHEMERIS

.HELP
PURPOSE:
To compute information about the way one planet looks when viewed from another.
Computed quantities are:
Object range in km.
Object planetocentric west longitude in degrees.
Object planetocentric latitude in degrees.

EXECUTION:
Example:
To see the orientation of jupiter as viewed from the earth on February 21 1996
at 12 hours 23 minutes 14.6 seconds GMT.

ephemeris viewer=earth object=jupiter epoch="21 Feb 1996 12 23 14.6"

.PAGE

METHOD:
Spice file de405.bsp is consulted for the ephemeris information. 
A light time correction is included.

The approximate time coverage for the bodies listed is:
 
   From: 1 January 1950
   To  : 1 January 2050

   (The original version of this program used the kernel de403s.
   As of November 13, 1995 this was the official ephemeris planned
   for use by the flight projects: Cassini, Mars Pathfinder, Mars
   Global Surveyor, Near Earth Asteroid Rendezvous.)

Accuracy
 
   The JPL "DE" series of planetary ephemerides have been widely used
   throughout the astronomical community for over 20 years.  They have
   been put to many different uses.  A short list of the applications
   of these ephemerides includes:  the construction of the tables in
   the "Astronomical Almanac" (for that matter all almanac producers
   world wide), planning of solar system observations with the Hubble
   Space Telescope, navigation of interplanetary missions, lunar laser
   ranging, solar system radar ranging, and solar system observations
   via VLBI.
 
   As of 1995, the ephemeris DE403 represented state-of-the-art planetary
   and lunar positions.
 
   One method of describing the accuracy of the positions provided in
   DE403 is to consider the angles between various ephemeris objects
   as viewed from Earth.
 
   If the two objects observed are taken from the list (Sun, Mercury,
   Venus, Mars, Moon) the angles computed from the ephemeris positions
   are accurate to one or two milli-arcsecond.(This assumes all
   appropriate corrections are applied for light time, stellar
   aberration, and relativistic effects to the ephemeris derived
   positions.)  If you add the barycenter of Jupiter to this list,
   uncertainty in ephemeris derived angles may grow to a few hundredths
   of an arcsecond.  Adding Saturn, Uranus and Neptune will raise the
   uncertainty level to approximately 0.1 arcseconds.  Finally, adding
   Pluto to the list raises the observable errors to 0.3 arcseconds
   for the present and increasing into the future.
 
   Radial distances to the centers of objects follow a similar trend.
   The radial distances between the inner objects of the solar system
   as computed via the ephemeris are accurate to 1 to 2 km.  The
   distance between the Earth and the Jupiter Barycenter is accurate
   to better than 10 km.
 
   The uncertainty in the distances to Saturn, Uranus and Neptune are
   approximately 1000, 2000 and 4000 km respectively.  For Pluto, the
   radial distance from earth may be in error up to 10000 km for the
   present and growing into the future.
 
   The above statements refer to the internal consistency of the
   ephemeris.  These statements of accuracy also hold when comparing
   positions with the J2000 radio source reference frame adopted by
   the IAU and IERS.
   When comparing the position a planet (other than Pluto) as seen
   from earth  with  catalogue positions of stars,  the DE403 positions
   of the planets are very likely to be more accurate than knowledge
   of the positions of the stars in the catalog (with the possible
   exception of the Hipparchos catalog). In the case of Pluto, the
   error in the DE403 position of Pluto is likely to be close to the
   error in the catalogue positions of stars.

   (NOTE: the above text was written by Jean Lorre in 1995.  I have
   updated it slightly to reflect the later kernel currently used
   by the program, DE405, but am unsure whether all the above 
   statements also apply to it.  - L.Kamp / 2010.)
 
References
 
   Jet Propulsion Laboratory Interoffice Memorandum IOM 314.10-127
   by E.M. Standish, X.X. Newhall, J.G. Williams and W.M. Folkner
   "JPL Planetary and Lunar Ephemerides, DE403/LE403"
 
Inquiries
 
   If you have any questions regarding this ephemeris contact
   NAIF team member
 
   Bill Taber
   (818) 354-4279
   btaber@spice.jpl.nasa.gov
 
   or NAIF group head
 
   Chuck Acton
   (818) 354-3869
   cacton@spice.jpl.nasa.gov
 
HISTORY:
3-1-97  J Lorre. 
COGNIZANT PROGRAMMER:  Jean Lorre
REVISIONS:
  10 MAR 2010 LWK Replaced the default value of spicefile (which is no 
                  longer on the system) with a newer kernel, DE405, which
                  has a much longer time range.
  10 SEP 98  GMY  Allow for input of planet or barycenter SPK data.
  21 JUL 1998 TIH Replace xgetenv_vic, clpool and ldpool with init_spice
                  which uses new spice subroutines (AR-100456)
  20 MAR 98  GMY  Fix parsing of epoch (AR 9682)

.LEVEL1
.VARI OBJECT
The body being
viewed.

.VARI VIEWER
The body from which
one is viewing

.VARI EPOCH
The viewers date &
time in GMT.

.VARI SPICEFILE
Name of solar system
sp kernel

.LEVEL2

.VARI OBJECT
The body being viewed.

.VARI VIEWER
The body from which one is viewing

.VARI EPOCH
The viewers date and time in GMT.
Valid date and time strings are:

             EPOCH STRING                   TRANSLATION
             ----------                   -----------
 
        '1/9/1986 3:12:59.2   '    (  9 JAN 1986 03:12:59.2 )
        '1 9 1986 3,12,59.2   '    (  9 JAN 1986 03:12:59.2 )
        '2 jan 1991 3:00 12.2 '    (  2 JAN 1991 03:00:12.2 )
        '1991 MAR 10 12:00:00 '    ( 10 MAR 1991 12:00:00   )
        '29 February 1975 3:00'    (  1 MAR 1975 03:00:00   )
        '2010 October 29 3:58 '    ( 29 OCT 2010 03:58:00   )
        'dec 31 1986 12       '    ( 31 DEC 1986 12:00:00   )
        '1986-365 // 12       '    ( 31 DEC 1986 12:00:00   )
        'JUL 1977             '    (  1 JUL 1977 00:00:00   )
        'JD 2451545.          '    (  1 JAN 2000 12:00:00   )
        'jd 2451545.          '    (  1 JAN 2000 12:00:00   )
        'JD2451545.           '    (  1 JAN 2000 12:00:00   )
 
For the convenience of the solar xray stereo mission the following time is
also permitted:
 asfd19920112044820a.vic
 with the time-stamp:  yyyymmddhhmmss


.VARI SPICEFILE
The name of the solar system spice sp kernel. Defaults to:
/project/spice/ops/generic_kernels/spk/de405.bsp.
This is a binary kernel operating under solaris operating system.
It is assumed that the LEAPSECONDS and CONSTANTS kernels are defined by
these 2 logical names and exist in the standard GLL spice directory.
$ Return
$!#############################################################################
$Test_File:
$ create tstephemeris.pdf
!Unit test for VICAR program EPHEMERIS
procedure
refgbl $echo
body
refgbl $syschar
let _onfail="continue"
let $echo="yes"
#if ($syschar(1) = "UNIX")
   ephemeris viewer=earth object=sun epoch="12 JAN 1992 4:48:20.0"
   ephemeris viewer=earth object=sun epoch=asfd19920112044820a.vic
   ephemeris viewer=earth object=venus epoch="13 FEB 1990 5:58:16.962"
   ephemeris viewer=earth object=jupiter epoch="13 FEB 1990 5:58:16.962"
   ephemeris viewer=pluto object=jupiter epoch="13 FEB 1990 5:58:16.962"
#else
#   ephemeris viewer=earth object=venus epoch="13 FEB 1990 5:58:16.962" +
#      spicefile="GLL:[SPICE_KER.DATA]t900129.bsp_1"
#end-if
# (above option was for VMS, no longer supported)
end-proc
$ Return
$!#############################################################################
