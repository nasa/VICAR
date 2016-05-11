$!****************************************************************************
$!
$! Build proc for MIPL module gtlist
$! VPACK Version 2.1, Wednesday, January 13, 2016, 14:45:49
$!
$! Execute by entering:		$ @gtlist
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
$ write sys$output "*** module gtlist ***"
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
$ write sys$output "Invalid argument given to gtlist.com file -- ", primary
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
$   if F$SEARCH("gtlist.imake") .nes. ""
$   then
$      vimake gtlist
$      purge gtlist.bld
$   else
$      if F$SEARCH("gtlist.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake gtlist
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @gtlist.bld "STD"
$   else
$      @gtlist.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create gtlist.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack gtlist.com -mixed -
	-s gtlist.c -
	-i gtlist.imake -
	-p gtlist.pdf -
	-t tstgtlist.pdf tstgtlist.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create gtlist.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <math.h>
#include <string.h>
#include <ctype.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"

#include "cartoStrUtils.h"
#include "cartoMemUtils.h"
#include "cartoLsqUtils.h"
#include "cartoGtUtils.h"

/*  GeoTIFF file list routine   A. Zobrist    8/16/99   */

static char msgBuf[10000];

void main44(void)
{
   int i,gtholder,nl,ns,rot,elen,nlen,pcount,pdef,status,len;
   char *labelstr,*p,*printlabel;
   double map[6],voff,vl,vs,gl,gs,east,north;
   double mapunitm,mapinch,scalefrac,horizpix=0;
   double mapinv[6],corner[4];
   
   /* initialize, fetch params */

   zifmessage("gtlist version 2016-01-13");
   
   status = gtgetlab("inp",1,&labelstr,&nl,&ns);
   if (status!=1)
      zmabend("Failed to read GeoTIFF label");
      
   len = strlen(labelstr);
   if ((printlabel=(char *)malloc(len+1))==NULL) zmabend("malloc failed");
   for (i=0;i<=len;i++)
      {
      printlabel[i] = labelstr[i];
      labelstr[i] = toupper(labelstr[i]);
      }
   if (nl==1&&ns==1)
      {
      zvparm("listnl",&nl,&pcount,&pdef,1,0);
      zvparm("listns",&ns,&pcount,&pdef,1,0);
      gtholder = 0;
      }
   else gtholder = 1;
   status = geofix(labelstr,map,mapinv,nl,ns,corner);
   if (status!=1)
      {
	zifmessage("No mapping in GeoTIFF label");
	zifmessage("The GeoTIFF label is:");
	zifmessage(printlabel);
      return;
      }
   rot = gtgetrot(labelstr);
      
   zvparmd("mapunitm",&mapunitm,&pcount,&pdef,1,0);
   zvparmd("mapinch",&mapinch,&pcount,&pdef,1,0);
     
   p = ms_find(labelstr,"GTRASTERTYPEGEOKEY=2");
   if (p!=0) voff = 1.0; else voff = 0.5;     /* 0.5 is the default also */
   
   /* printing section */
   
   zifmessage("VICAR GeoTIFF LABEL LIST");
   if (!gtholder)
      {
      zifmessage("The file is a standalone VICAR GeoTIFF label");
      sprintf(msgBuf, "A hypothetical %d x %d VICAR image will", nl, ns);
      zifmessage(msgBuf);
      zifmessage("be used to illustrate the mapping of corner points.");
      }
   
   zifmessage("The VICAR GeoTIFF label is:");
   zifmessage(printlabel);
   
   if (voff<0.75)
     zifmessage("The image raster is an 'area' type");
   else
     zifmessage("The image raster is a 'point' or 'post' type");
   
   zifmessage("The centers of the corner pixels are:");
   zifmessage("VICAR-line    -samp GeoTIFF-samp    -line            East           North");
   
   for (i=0;i<4;i++)
      {
      vl = (double)(i/2)*((double)nl-1.0)+1.0;
      vs = (double)(i%2)*((double)ns-1.0)+1.0;
      gl = vl-voff;
      gs = vs-voff;
      east = map[0]*vl+map[1]*vs+map[2];
      elen = MAX(12-(int)(log10(fabs(east)+.9)),1);
      north = map[3]*vl+map[4]*vs+map[5];
      nlen = MAX(12-(int)(log10(fabs(north)+.9)),1);
      sprintf(msgBuf, "%10.1f%9.1f%13.1f%9.1f %15.*f %15.*f",
         vl,vs,gs,gl,elen,east,nlen,north);
      zifmessage(msgBuf);
      }          
   
   zifmessage("The outer corners of the corner pixels are:");
   zifmessage("VICAR-line    -samp GeoTIFF-samp    -line            East           North");
   
   for (i=0;i<4;i++)
      {
      vl = (double)(i/2)*(double)nl+0.5;
      vs = (double)(i%2)*(double)ns+0.5;
      gl = vl-voff;
      gs = vs-voff;
      east = map[0]*vl+map[1]*vs+map[2];
      elen = MAX(12-(int)(log10(fabs(east)+.9)),1);
      north = map[3]*vl+map[4]*vs+map[5];
      nlen = MAX(12-(int)(log10(fabs(north)+.9)),1);
      sprintf(msgBuf, "%10.1f%9.1f%13.1f%9.1f %15.*f %15.*f",
         vl,vs,gs,gl,elen,east,nlen,north);
      zifmessage(msgBuf);
      }          
   
   zifmessage("The rotation of the image relative to an E-N geographic frame is:");
   switch (rot)
      {
      case 0:  zifmessage("rotation 0\n369\n258\n147"); break;
      case 1:  zifmessage("rotation 1\n123\n456\n789"); break;
      case 2:  zifmessage("rotation 2\n741\n852\n963"); break;
      case 3:  zifmessage("rotation 3\n987\n654\n321"); break;
      case 4:  zifmessage("rotation 4\n963\n852\n741"); break;
      case 5:  zifmessage("rotation 5\n789\n456\n123"); break;
      case 6:  zifmessage("rotation 6\n147\n258\n369"); break;
      case 7:  zifmessage("rotation 7\n321\n654\n987"); break;
      default:
         zifmessage("NOT ALIGNED WITH EAST-NORTH COORDINATE SYSTEM");
      }
   
   zifmessage("The scale units of the image are (ignoring sign):");
   switch (rot)
      {
      case 0: case 2: case 4: case 6:
         elen = MAX(13-(int)(log10(fabs(map[4])+.9)),1);
         sprintf(msgBuf, "1 sample = %15.*f map units north",elen,fabs(map[4]));
	 zifmessage(msgBuf);
         sprintf(msgBuf, "1 line   = %15.*f map units east",elen,fabs(map[0]));
	 zifmessage(msgBuf);
         horizpix = (double)nl*map[0];
         break;
      case 1: case 3: case 5: case 7:
         elen = MAX(13-(int)(log10(fabs(map[1])+.9)),1);
         sprintf(msgBuf, "1 sample = %15.*f map units east",elen,fabs(map[1]));
	 zifmessage(msgBuf);
         sprintf(msgBuf, "1 line   = %15.*f map units north",elen,fabs(map[3]));
	 zifmessage(msgBuf);
         horizpix = (double)ns*map[1];
         break;
      default:
         zifmessage("SEE TRANSFORMATION MATRIX");
      }
   
   scalefrac = fabs((horizpix*mapunitm*39.0)/mapinch);
   sprintf(msgBuf, "The scale fraction is 1 /%9.1f",scalefrac);
   zifmessage(msgBuf);
   sprintf(msgBuf, "(assuming mapunit = %f meters and the map is %f inches)",
           mapunitm,mapinch);
   zifmessage(msgBuf);
      
   return;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create gtlist.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM gtlist

   To Create the build file give the command:

		$ vimake gtlist			(VMS)
   or
		% vimake gtlist			(Unix)


************************************************************************/


#define PROGRAM	gtlist

#define MODULE_LIST gtlist.c

#define MAIN_LANG_C
#define USES_ANSI_C

#define LIB_CARTO
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create gtlist.pdf
process help=*
 !
 PARM INP       TYPE=STRING COUNT=1
 PARM LISTNL      TYPE=INTEGER COUNT=1 DEFAULT=1000
 PARM LISTNS      TYPE=INTEGER COUNT=1 DEFAULT=2000
 PARM MAPUNITM    TYPE=REAL COUNT=1 DEFAULT=1.
 PARM MAPINCH     TYPE=REAL COUNT=1 DEFAULT=10.
!
 END-PROC
!
! HELP TEXT FOR GTLIST
.TITLE
GTLIST - Program for listing image mapping information in a GeoTIFF label
.HELP
PURPOSE
     GTLIST is a VICAR applications program which lists information
     in a VICAR GeoTIFF label.  Besides listing the label contents,
     derived information about the corner point coordinates and the
     image rotation are calculated.
     
     A non-image GeoTIFF file can also be listed by this program,
     but information about the corner points is omitted.
    	
CALL
     gtform INP PARAMS
  WHERE:
     INP            is the input data set.
     PARAMS         is a standard VICAR parameter field.
OPERATION

GTLIST reads the GeoTIFF label and the VICAR NL,NS parameters.  The
printout is based on these.  Note that the GeoTIFF image coordinate
system will differ systematically from the VICAR image coordinate
(line,sample) system by a half a pixel or a whole pixel (depending 
on whether the GeoTIFF file is "area" format or "post" format.  An
example of a "post" formatted file is DMA DTED -- there is an
elevation reading exactly at the initial lon-lat.

If the GeoTIFF label is not attached to a VICAR image, the raster
coordinates are applied to an hypothetical 1000 x 2000 image to help
the user understand the mapping.  If the label does not contain raster
coordinates (for example, just a map spec), then the label will simply
be listed.  The user can input another hypothetical size using the
parameters LISTNL and LISTNS.  These values enter into the map
scale calculation as well.

In addition, rotation is explained.  Rotation refers to the order
of pixels in the file.  The eight possible rotation are (here, pixel
value represents the order of data in the file):

   369    123    741    987
0: 258 1: 456 2: 852 3: 654
   147    789    963    321

   963    789    147    321
4: 852 5: 456 6: 258 7: 654
   741    123    369    987

Rotation 1 is the VICAR standard rotation.  It is used also by
systems such as LAS, IDIMS, LARSYS, ERDAS, I2S, etc. and was
adopted by LANDSAT, SPOT, and all of the planetary probes.
It is also produced by frame grabbers since it is the
ordering of a TV scan.  Rotation 0 is the USGS standard and is
used, for example, for DMA DTED.

The fact that an image is "rotated" is due to the different ways
that line-sample ordering can represent a geographic east-north
coordinate system.


The transitions among these rotations are given by the
following table:

	   no flip              flip

rotate->0  90 180 270      0  90 180 270

   0)   0   3   2   1      4   5   6   7
   1)   1   0   3   2      5   6   7   4
   2)   2   1   0   3      6   7   4   5
   3)   3   2   1   0      7   4   5   6
   4)   4   5   6   7      0   3   2   1
   5)   5   6   7   4      1   0   3   2
   6)   6   7   4   5      2   1   0   3
   7)   7   4   5   6      3   2   1   0


VICAR can process images with any rotation (basically by ignoring
the fact that rotation means anything).  The exception would be
for programs that combine images such as F2, OVERLAY, APPEND, MSS,
FASTMOS, etc.  These correctly operate only when the images have
the same rotation.

Map scale is printed out, but this depends upon the display inches
and map unit size in meters.  The parameters MAPINCH AND MAPUNITM
are provided for inutting these values.


PERFORMANCE

Less than 1 second.

.PAGE
RESTRICTIONS
------------

REFERENCES

     Ritter, N., Ruth, M. "GeoTIFF Format Specification, Revision 1.0",
     JPL Cartographic Applications Group.
     
.PAGE
Original Programmer: A. L. Zobrist, 16 Aug. 1999
Current Cognizant Programmer: B. A. McGuffie
Revisions:
  2008-01-02 WLB Switched to USES_ANSI_C AND LIB_CARTO; misc cleanup  
  2015-11-01 WLB Migrated to MIPL
  2016-01-13 WLB Replaced printfs with zifmessages

.LEVEL1
.VARI INP
Input file name
.VARI LISTNL
when no image use this for nl
.VARI LISTNS
when no image use this for ns
.VARI MAPUNITM
meters per map unit for scale
.VARI MAPINCH
inches on display or paper
for scale
.LEVEL2
.VARI LISTNL
A standalone GeoTIFF label does not have to have a VICAR image.
So, to help the user work with the mapping, a hypothetical image
is generated.   This parameter can give the size of the hypothetical
image.  If a VICAR image is present, this parameter is ignored.
.VARI LISTNS
A standalone GeoTIFF label does not have to have a VICAR image.
So, to help the user work with the mapping, a hypothetical image
is generated.   This parameter can give the size of the hypothetical
image.  If a VICAR image is present, this parameter is ignored.
.VARI MAPUNITM
Inputting this value allows the list program to calculate the
map scale fraction.  Also needed is the number of inches subtended
by the map on paper or screen.  Use the horizontal direction if
there is a difference with the vertical.  Also, use the middle of
the map to determine this value.  

Note that the map unit may be "meters", but in the middle of a map,
a map unit meter may be something like .978 meters.  Mapping
software is a good source of this value if exactness is desired.
.VARI MAPINCH
Inputting this value allows the list program to calculate the
map scale fraction.  Also needed is the size of the map unit in
meters (see MAPUNITM parameter).  Use the horizontal direction if
there is a difference with the vertical.
.VARI INP
GTLIST reads the GeoTIFF label and the VICAR NL,NS parameters.  The
printout is based on these.  Note that the GeoTIFF image coordinate
system will differ systematically from the VICAR image coordinate
(line,sample) system by a half a pixel or a whole pixel (depending 
on whether the GeoTIFF file is "area" format or "post" format.  An
example of a "post" formatted file is DMA DTED -- there is an
elevation reading exactly at the initial lon-lat.

In addition, rotation is explained.  Rotation refers to the order
of pixels in the file.  The eight possible rotation are (here, pixel
value represents the order of data in the file):

   369    123    741    987
0: 258 1: 456 2: 852 3: 654
   147    789    963    321

   963    789    147    321
4: 852 5: 456 6: 258 7: 654
   741    123    369    987

Rotation 1 is the VICAR standard rotation.  It is used also by
systems such as LAS, IDIMS, LARSYS, ERDAS, I2S, etc. and was
adopted by LANDSAT, SPOT, and all of the planetary probes.
It is also produced by frame grabbers since it is the
ordering of a TV scan.  Rotation 0 is the USGS standard and is
used, for example, for DMA DTED.

The fact that an image is "rotated" is due to the different ways
that line-sample ordering can represent a geographic east-north
coordinate system.


The transitions among these rotations are given by the
following table:

	   no flip              flip

rotate->0  90 180 270      0  90 180 270

   0)   0   3   2   1      4   5   6   7
   1)   1   0   3   2      5   6   7   4
   2)   2   1   0   3      6   7   4   5
   3)   3   2   1   0      7   4   5   6
   4)   4   5   6   7      0   3   2   1
   5)   5   6   7   4      1   0   3   2
   6)   6   7   4   5      2   1   0   3
   7)   7   4   5   6      3   2   1   0


VICAR can process images with any rotation (basically by ignoring
the fact that rotation means anything).  The exception would be
for programs that combine images such as F2, OVERLAY, APPEND, MSS,
FASTMOS, etc.  These correctly operate only when the images have
the same rotation.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstgtlist.pdf
procedure
refgbl $echo
parm version string def="ibis-1"
parm org string def="column"
body
enable-log

!let _onfail="continue"
let $echo="yes"

!goto curr

!   TEST SCRIPT FOR GTLIST

!ush rm xxxlab*
!ush rm xxxim*


! basic case, vicar rotation, also known as rotation = 1

gtgen out=xxxlab1 'tiecnvrt +
   geotiff=("ModelTiePointTag=(0,0,0,350000.4,5307000.3,0.0)", +
          "ModelTiePointTag=(100,100,0,351000.4,5306000.3,0.0)", +
          "ModelTiePointTag=(100,0,0,351000.4,5307000.3,0.0)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
   
gtlist xxxlab1 
 
 
!  flip case, rot=3, all 4 flip cases will use scale form of GeoTIFF

gtgen out=xxxlab2 'tiecnvrt +
   geotiff=("ModelTiePointTag=(0,0,0,351000.4,5306000.3,0.0)", +
          "ModelTiePointTag=(100,100,0,350000.4,5307000.3,0.0)", +
          "ModelTiePointTag=(100,0,0,350000.4,5306000.3,0.0)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
   
gtlist xxxlab2

!  flip case, rot=5

gtgen out=xxxlab2 'tiecnvrt +
   geotiff=("ModelTiePointTag=(0,0,0,350000.4,5306000.3,0.0)", +
          "ModelTiePointTag=(100,100,0,351000.4,5307000.3,0.0)", +
          "ModelTiePointTag=(100,0,0,351000.4,5306000.3,0.0)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
   
gtlist xxxlab2

!  flip case, rot=5 other corner given

gtgen out=xxxlab2 'tiecnvrt +
   geotiff=("ModelTiePointTag=(0,0,0,350000.4,5306000.3,0.0)", +
          "ModelTiePointTag=(100,100,0,351000.4,5307000.3,0.0)", +
          "ModelTiePointTag=(0,100,0,350000.4,5307000.3,0.0)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
   
gtlist xxxlab2

!  flip case, rot=7

gtgen out=xxxlab2 'tiecnvrt +
   geotiff=("ModelTiePointTag=(0,0,0,351000.4,5307000.3,0.0)", +
          "ModelTiePointTag=(100,100,0,350000.4,5306000.3,0.0)", +
          "ModelTiePointTag=(100,0,0,350000.4,5307000.3,0.0)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
   
gtlist xxxlab2


!  matrix case, rot=0

gtgen out=xxxlab2 'tiecnvrt +
   geotiff=("ModelTiePointTag=(0,0,0,350000.4,5306000.3,0.0)", +
          "ModelTiePointTag=(100,100,0,351000.4,5307000.3,0.0)", +
          "ModelTiePointTag=(0,100,0,351000.4,5306000.3,0.0)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
   
gtlist xxxlab2

!  matrix case, rot=2

gtgen out=xxxlab2 'tiecnvrt +
   geotiff=("ModelTiePointTag=(0,0,0,351000.4,5307000.3,0.0)", +
          "ModelTiePointTag=(100,100,0,350000.4,5306000.3,0.0)", +
          "ModelTiePointTag=(0,100,0,350000.4,5307000.3,0.0)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
   
gtlist xxxlab2

!  matrix case, rot=6

gtgen out=xxxlab2 'tiecnvrt +
   geotiff=("ModelTiePointTag=(0,0,0,350000.4,5307000.3,0.0)", +
          "ModelTiePointTag=(100,100,0,351000.4,5306000.3,0.0)", +
          "ModelTiePointTag=(0,100,0,351000.4,5307000.3,0.0)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
   
gtlist xxxlab2


!  matrix case, rot=4

gtgen out=xxxlab2 'tiecnvrt +
   geotiff=("ModelTiePointTag=(0,0,0,351000.4,5306000.3,0.0)", +
          "ModelTiePointTag=(100,100,0,350000.4,5307000.3,0.0)", +
          "ModelTiePointTag=(0,100,0,350000.4,5306000.3,0.0)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
   
gtlist xxxlab2

! need case for post type image

gtgen out=xxxlab3 'tiecnvrt +
   geotiff=("ModelTiePointTag=(1,1,0,350000.4,5306000.3,0.0)", +
          "ModelTiePointTag=(101,101,0,351000.4,5307000.3,0.0)", +
          "ModelTiePointTag=(101,1,0,350000.4,5307000.3,0.0)", +
          "GTRasterTypeGeoKey=2(RasterPixelIsPoint)")

gtlist xxxlab3


! DMA DTED case

gtgen out=xxxlab4 'tiecnvrt+
   geotiff=("ModelTiePointTag=(0,0,0,121.0,25.0,0.0)", +
        "ModelTiePointTag=(1200,0,0,121.0,26.0,0.0)", +
        "ModelTiePointTag=(0,1200,0,122.0,25.0,0.0)", +
        "GTRasterTypeGeoKey=2(RasterPixelIsPoint)")

gtlist xxxlab4 mapunitm=75500.0 mapinch=20.0 listnl=1201 listns=1201

!   non-square pixel case

gtgen out=xxxlab5 'tiecnvrt +
   geotiff=("ModelTiePointTag=(2,3,0,350000.4,5307000.3,0.0)", +
          "ModelTiePointTag=(102,103,0,351000.4,5305000.3,0.0)", +
          "ModelTiePointTag=(102,3,0,351000.4,5307000.3,0.0)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
   
gtlist xxxlab5

! vicar image input case

gen xxxim1 nl=500 ns=500

gtgen inp=xxxim1 out=xxxim2 'tiecnvrt +
   geotiff=("ModelTiePointTag=(0,0,0,350807.4,5317081.3,0.0)", +
          "ModelTiePointTag=(100,100,0,351807.4,5316081.3,0.0)", +
          "ModelTiePointTag=(100,0,0,351807.4,5317081.3,0.0)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")

label-list xxxim2
gtlist xxxim2

! vicar case - add

gtgen inp=xxxim1 out=xxxim2 'add geotiff=( +
  "PCSCitationGeoKey=""UTM Zone 60 N with WGS84""")

label-list xxxim2
gtlist xxxim2


! repeated key case, also should delete the stuff in old xxxim1

gtgen inp=xxxim1 out=xxxim2 +
   geotiff=("ModelTiePointTag=(0,0,0,350807.4,5317081.3,0.0)", +
          "ModelTiePointTag=(100,100,0,351807.4,5316081.3,0.0)", +
          "ModelTiePointTag=(100,0,0,351807.4,5317081.3,0.0)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")

label-list xxxim2
gtlist xxxim2

!   no image coord case

gtgen out=xxxlab6  +
  geotiff=("GTRasterTypeGeoKey=1(RasterPixelIsArea)")
   
gtlist xxxlab6

theend>

disable-log
end-proc
$!-----------------------------------------------------------------------------
$ create tstgtlist.log
gtgen out=xxxlab1 'tiecnvrt  +
   geotiff=("ModelTiePointTag=(0,0,0,350000.4,5307000.3,0.0)",  +
          "ModelTiePointTag=(100,100,0,351000.4,5306000.3,0.0)",  +
          "ModelTiePointTag=(100,0,0,351000.4,5307000.3,0.0)",  +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
gtlist xxxlab1
Beginning VICAR task gtlist
gtlist version 2016-01-13
VICAR GeoTIFF LABEL LIST
The file is a standalone VICAR GeoTIFF label
A hypothetical 1000 x 2000 VICAR image will
be used to illustrate the mapping of corner points.
The VICAR GeoTIFF label is:
MODELTIEPOINTTAG=(0,0,0,350000.4,5307000.3,0.0)
MODELPIXELSCALETAG=(10.0,10.0,0.0)
GTRASTERTYPEGEOKEY=1(RasterPixelIsArea)

The image raster is an 'area' type
The centers of the corner pixels are:
VICAR-line    -samp GeoTIFF-samp    -line            East           North
       1.0      1.0          0.5      0.5  350005.4000000  5306995.300000
       1.0   2000.0       1999.5      0.5  369995.4000000  5306995.300000
    1000.0      1.0          0.5    999.5  350005.4000000  5297005.300000
    1000.0   2000.0       1999.5    999.5  369995.4000000  5297005.300000
The outer corners of the corner pixels are:
VICAR-line    -samp GeoTIFF-samp    -line            East           North
       0.5      0.5          0.0      0.0  350000.4000000  5307000.300000
       0.5   2000.5       2000.0      0.0  370000.4000000  5307000.300000
    1000.5      0.5          0.0   1000.0  350000.4000000  5297000.300000
    1000.5   2000.5       2000.0   1000.0  370000.4000000  5297000.300000
The rotation of the image relative to an E-N geographic frame is:
rotation 1
123
456
789
The scale units of the image are (ignoring sign):
1 sample = 10.000000000000 map units east
1 line   = 10.000000000000 map units north
The scale fraction is 1 /  78000.0
(assuming mapunit = 1.000000 meters and the map is 10.000000 inches)
gtgen out=xxxlab2 'tiecnvrt  +
   geotiff=("ModelTiePointTag=(0,0,0,351000.4,5306000.3,0.0)",  +
          "ModelTiePointTag=(100,100,0,350000.4,5307000.3,0.0)",  +
          "ModelTiePointTag=(100,0,0,350000.4,5306000.3,0.0)",  +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
gtlist xxxlab2
Beginning VICAR task gtlist
gtlist version 2016-01-13
VICAR GeoTIFF LABEL LIST
The file is a standalone VICAR GeoTIFF label
A hypothetical 1000 x 2000 VICAR image will
be used to illustrate the mapping of corner points.
The VICAR GeoTIFF label is:
MODELTIEPOINTTAG=(0,0,0,351000.4,5306000.3,0.0)
MODELPIXELSCALETAG=(-10.0,-10.0,0.0)
GTRASTERTYPEGEOKEY=1(RasterPixelIsArea)

The image raster is an 'area' type
The centers of the corner pixels are:
VICAR-line    -samp GeoTIFF-samp    -line            East           North
       1.0      1.0          0.5      0.5  350995.4000000  5306005.300000
       1.0   2000.0       1999.5      0.5  331005.4000000  5306005.300000
    1000.0      1.0          0.5    999.5  350995.4000000  5315995.300000
    1000.0   2000.0       1999.5    999.5  331005.4000000  5315995.300000
The outer corners of the corner pixels are:
VICAR-line    -samp GeoTIFF-samp    -line            East           North
       0.5      0.5          0.0      0.0  351000.4000000  5306000.300000
       0.5   2000.5       2000.0      0.0  331000.4000000  5306000.300000
    1000.5      0.5          0.0   1000.0  351000.4000000  5316000.300000
    1000.5   2000.5       2000.0   1000.0  331000.4000000  5316000.300000
The rotation of the image relative to an E-N geographic frame is:
rotation 3
987
654
321
The scale units of the image are (ignoring sign):
1 sample = 10.000000000000 map units east
1 line   = 10.000000000000 map units north
The scale fraction is 1 /  78000.0
(assuming mapunit = 1.000000 meters and the map is 10.000000 inches)
gtgen out=xxxlab2 'tiecnvrt  +
   geotiff=("ModelTiePointTag=(0,0,0,350000.4,5306000.3,0.0)",  +
          "ModelTiePointTag=(100,100,0,351000.4,5307000.3,0.0)",  +
          "ModelTiePointTag=(100,0,0,351000.4,5306000.3,0.0)",  +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
gtlist xxxlab2
Beginning VICAR task gtlist
gtlist version 2016-01-13
VICAR GeoTIFF LABEL LIST
The file is a standalone VICAR GeoTIFF label
A hypothetical 1000 x 2000 VICAR image will
be used to illustrate the mapping of corner points.
The VICAR GeoTIFF label is:
MODELTIEPOINTTAG=(0,0,0,350000.4,5306000.3,0.0)
MODELPIXELSCALETAG=(10.0,-10.0,0.0)
GTRASTERTYPEGEOKEY=1(RasterPixelIsArea)

The image raster is an 'area' type
The centers of the corner pixels are:
VICAR-line    -samp GeoTIFF-samp    -line            East           North
       1.0      1.0          0.5      0.5  350005.4000000  5306005.300000
       1.0   2000.0       1999.5      0.5  369995.4000000  5306005.300000
    1000.0      1.0          0.5    999.5  350005.4000000  5315995.300000
    1000.0   2000.0       1999.5    999.5  369995.4000000  5315995.300000
The outer corners of the corner pixels are:
VICAR-line    -samp GeoTIFF-samp    -line            East           North
       0.5      0.5          0.0      0.0  350000.4000000  5306000.300000
       0.5   2000.5       2000.0      0.0  370000.4000000  5306000.300000
    1000.5      0.5          0.0   1000.0  350000.4000000  5316000.300000
    1000.5   2000.5       2000.0   1000.0  370000.4000000  5316000.300000
The rotation of the image relative to an E-N geographic frame is:
rotation 5
789
456
123
The scale units of the image are (ignoring sign):
1 sample = 10.000000000000 map units east
1 line   = 10.000000000000 map units north
The scale fraction is 1 /  78000.0
(assuming mapunit = 1.000000 meters and the map is 10.000000 inches)
gtgen out=xxxlab2 'tiecnvrt  +
   geotiff=("ModelTiePointTag=(0,0,0,350000.4,5306000.3,0.0)",  +
          "ModelTiePointTag=(100,100,0,351000.4,5307000.3,0.0)",  +
          "ModelTiePointTag=(0,100,0,350000.4,5307000.3,0.0)",  +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
gtlist xxxlab2
Beginning VICAR task gtlist
gtlist version 2016-01-13
VICAR GeoTIFF LABEL LIST
The file is a standalone VICAR GeoTIFF label
A hypothetical 1000 x 2000 VICAR image will
be used to illustrate the mapping of corner points.
The VICAR GeoTIFF label is:
MODELTIEPOINTTAG=(0,0,0,350000.4,5306000.3,0.0)
MODELPIXELSCALETAG=(10.0,-10.0,0.0)
GTRASTERTYPEGEOKEY=1(RasterPixelIsArea)

The image raster is an 'area' type
The centers of the corner pixels are:
VICAR-line    -samp GeoTIFF-samp    -line            East           North
       1.0      1.0          0.5      0.5  350005.4000000  5306005.300000
       1.0   2000.0       1999.5      0.5  369995.4000000  5306005.300000
    1000.0      1.0          0.5    999.5  350005.4000000  5315995.300000
    1000.0   2000.0       1999.5    999.5  369995.4000000  5315995.300000
The outer corners of the corner pixels are:
VICAR-line    -samp GeoTIFF-samp    -line            East           North
       0.5      0.5          0.0      0.0  350000.4000000  5306000.300000
       0.5   2000.5       2000.0      0.0  370000.4000000  5306000.300000
    1000.5      0.5          0.0   1000.0  350000.4000000  5316000.300000
    1000.5   2000.5       2000.0   1000.0  370000.4000000  5316000.300000
The rotation of the image relative to an E-N geographic frame is:
rotation 5
789
456
123
The scale units of the image are (ignoring sign):
1 sample = 10.000000000000 map units east
1 line   = 10.000000000000 map units north
The scale fraction is 1 /  78000.0
(assuming mapunit = 1.000000 meters and the map is 10.000000 inches)
gtgen out=xxxlab2 'tiecnvrt  +
   geotiff=("ModelTiePointTag=(0,0,0,351000.4,5307000.3,0.0)",  +
          "ModelTiePointTag=(100,100,0,350000.4,5306000.3,0.0)",  +
          "ModelTiePointTag=(100,0,0,350000.4,5307000.3,0.0)",  +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
gtlist xxxlab2
Beginning VICAR task gtlist
gtlist version 2016-01-13
VICAR GeoTIFF LABEL LIST
The file is a standalone VICAR GeoTIFF label
A hypothetical 1000 x 2000 VICAR image will
be used to illustrate the mapping of corner points.
The VICAR GeoTIFF label is:
MODELTIEPOINTTAG=(0,0,0,351000.4,5307000.3,0.0)
MODELPIXELSCALETAG=(-10.0,10.0,0.0)
GTRASTERTYPEGEOKEY=1(RasterPixelIsArea)

The image raster is an 'area' type
The centers of the corner pixels are:
VICAR-line    -samp GeoTIFF-samp    -line            East           North
       1.0      1.0          0.5      0.5  350995.4000000  5306995.300000
       1.0   2000.0       1999.5      0.5  331005.4000000  5306995.300000
    1000.0      1.0          0.5    999.5  350995.4000000  5297005.300000
    1000.0   2000.0       1999.5    999.5  331005.4000000  5297005.300000
The outer corners of the corner pixels are:
VICAR-line    -samp GeoTIFF-samp    -line            East           North
       0.5      0.5          0.0      0.0  351000.4000000  5307000.300000
       0.5   2000.5       2000.0      0.0  331000.4000000  5307000.300000
    1000.5      0.5          0.0   1000.0  351000.4000000  5297000.300000
    1000.5   2000.5       2000.0   1000.0  331000.4000000  5297000.300000
The rotation of the image relative to an E-N geographic frame is:
rotation 7
321
654
987
The scale units of the image are (ignoring sign):
1 sample = 10.000000000000 map units east
1 line   = 10.000000000000 map units north
The scale fraction is 1 /  78000.0
(assuming mapunit = 1.000000 meters and the map is 10.000000 inches)
gtgen out=xxxlab2 'tiecnvrt  +
   geotiff=("ModelTiePointTag=(0,0,0,350000.4,5306000.3,0.0)",  +
          "ModelTiePointTag=(100,100,0,351000.4,5307000.3,0.0)",  +
          "ModelTiePointTag=(0,100,0,351000.4,5306000.3,0.0)",  +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
gtlist xxxlab2
Beginning VICAR task gtlist
gtlist version 2016-01-13
VICAR GeoTIFF LABEL LIST
The file is a standalone VICAR GeoTIFF label
A hypothetical 1000 x 2000 VICAR image will
be used to illustrate the mapping of corner points.
The VICAR GeoTIFF label is:
MODELTRANSFORMATIONTAG=(-0.0,10.0,0,350000.4,10.0,-0.0,0,5306000.3,0,0,0,0,0,0,0,1)
GTRASTERTYPEGEOKEY=1(RasterPixelIsArea)

The image raster is an 'area' type
The centers of the corner pixels are:
VICAR-line    -samp GeoTIFF-samp    -line            East           North
       1.0      1.0          0.5      0.5  350005.4000000  5306005.300000
       1.0   2000.0       1999.5      0.5  350005.4000000  5325995.300000
    1000.0      1.0          0.5    999.5  359995.4000000  5306005.300000
    1000.0   2000.0       1999.5    999.5  359995.4000000  5325995.300000
The outer corners of the corner pixels are:
VICAR-line    -samp GeoTIFF-samp    -line            East           North
       0.5      0.5          0.0      0.0  350000.4000000  5306000.300000
       0.5   2000.5       2000.0      0.0  350000.4000000  5326000.300000
    1000.5      0.5          0.0   1000.0  360000.4000000  5306000.300000
    1000.5   2000.5       2000.0   1000.0  360000.4000000  5326000.300000
The rotation of the image relative to an E-N geographic frame is:
rotation 0
369
258
147
The scale units of the image are (ignoring sign):
1 sample = 10.000000000000 map units north
1 line   = 10.000000000000 map units east
The scale fraction is 1 /  39000.0
(assuming mapunit = 1.000000 meters and the map is 10.000000 inches)
gtgen out=xxxlab2 'tiecnvrt  +
   geotiff=("ModelTiePointTag=(0,0,0,351000.4,5307000.3,0.0)",  +
          "ModelTiePointTag=(100,100,0,350000.4,5306000.3,0.0)",  +
          "ModelTiePointTag=(0,100,0,350000.4,5307000.3,0.0)",  +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
gtlist xxxlab2
Beginning VICAR task gtlist
gtlist version 2016-01-13
VICAR GeoTIFF LABEL LIST
The file is a standalone VICAR GeoTIFF label
A hypothetical 1000 x 2000 VICAR image will
be used to illustrate the mapping of corner points.
The VICAR GeoTIFF label is:
MODELTRANSFORMATIONTAG=(0.0,-10.0,0,351000.4,-10.0,-0.0,0,5307000.3,0,0,0,0,0,0,0,1)
GTRASTERTYPEGEOKEY=1(RasterPixelIsArea)

The image raster is an 'area' type
The centers of the corner pixels are:
VICAR-line    -samp GeoTIFF-samp    -line            East           North
       1.0      1.0          0.5      0.5  350995.4000000  5306995.300000
       1.0   2000.0       1999.5      0.5  350995.4000000  5287005.300000
    1000.0      1.0          0.5    999.5  341005.4000000  5306995.300000
    1000.0   2000.0       1999.5    999.5  341005.4000000  5287005.300000
The outer corners of the corner pixels are:
VICAR-line    -samp GeoTIFF-samp    -line            East           North
       0.5      0.5          0.0      0.0  351000.4000000  5307000.300000
       0.5   2000.5       2000.0      0.0  351000.4000000  5287000.300000
    1000.5      0.5          0.0   1000.0  341000.4000000  5307000.300000
    1000.5   2000.5       2000.0   1000.0  341000.4000000  5287000.300000
The rotation of the image relative to an E-N geographic frame is:
rotation 2
741
852
963
The scale units of the image are (ignoring sign):
1 sample = 10.000000000000 map units north
1 line   = 10.000000000000 map units east
The scale fraction is 1 /  39000.0
(assuming mapunit = 1.000000 meters and the map is 10.000000 inches)
gtgen out=xxxlab2 'tiecnvrt  +
   geotiff=("ModelTiePointTag=(0,0,0,350000.4,5307000.3,0.0)",  +
          "ModelTiePointTag=(100,100,0,351000.4,5306000.3,0.0)",  +
          "ModelTiePointTag=(0,100,0,351000.4,5307000.3,0.0)",  +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
gtlist xxxlab2
Beginning VICAR task gtlist
gtlist version 2016-01-13
VICAR GeoTIFF LABEL LIST
The file is a standalone VICAR GeoTIFF label
A hypothetical 1000 x 2000 VICAR image will
be used to illustrate the mapping of corner points.
The VICAR GeoTIFF label is:
MODELTRANSFORMATIONTAG=(-0.0,10.0,0,350000.4,-10.0,-0.0,0,5307000.3,0,0,0,0,0,0,0,1)
GTRASTERTYPEGEOKEY=1(RasterPixelIsArea)

The image raster is an 'area' type
The centers of the corner pixels are:
VICAR-line    -samp GeoTIFF-samp    -line            East           North
       1.0      1.0          0.5      0.5  350005.4000000  5306995.300000
       1.0   2000.0       1999.5      0.5  350005.4000000  5287005.300000
    1000.0      1.0          0.5    999.5  359995.4000000  5306995.300000
    1000.0   2000.0       1999.5    999.5  359995.4000000  5287005.300000
The outer corners of the corner pixels are:
VICAR-line    -samp GeoTIFF-samp    -line            East           North
       0.5      0.5          0.0      0.0  350000.4000000  5307000.300000
       0.5   2000.5       2000.0      0.0  350000.4000000  5287000.300000
    1000.5      0.5          0.0   1000.0  360000.4000000  5307000.300000
    1000.5   2000.5       2000.0   1000.0  360000.4000000  5287000.300000
The rotation of the image relative to an E-N geographic frame is:
rotation 6
147
258
369
The scale units of the image are (ignoring sign):
1 sample = 10.000000000000 map units north
1 line   = 10.000000000000 map units east
The scale fraction is 1 /  39000.0
(assuming mapunit = 1.000000 meters and the map is 10.000000 inches)
gtgen out=xxxlab2 'tiecnvrt  +
   geotiff=("ModelTiePointTag=(0,0,0,351000.4,5306000.3,0.0)",  +
          "ModelTiePointTag=(100,100,0,350000.4,5307000.3,0.0)",  +
          "ModelTiePointTag=(0,100,0,350000.4,5306000.3,0.0)",  +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
gtlist xxxlab2
Beginning VICAR task gtlist
gtlist version 2016-01-13
VICAR GeoTIFF LABEL LIST
The file is a standalone VICAR GeoTIFF label
A hypothetical 1000 x 2000 VICAR image will
be used to illustrate the mapping of corner points.
The VICAR GeoTIFF label is:
MODELTRANSFORMATIONTAG=(0.0,-10.0,0,351000.4,10.0,-0.0,0,5306000.3,0,0,0,0,0,0,0,1)
GTRASTERTYPEGEOKEY=1(RasterPixelIsArea)

The image raster is an 'area' type
The centers of the corner pixels are:
VICAR-line    -samp GeoTIFF-samp    -line            East           North
       1.0      1.0          0.5      0.5  350995.4000000  5306005.300000
       1.0   2000.0       1999.5      0.5  350995.4000000  5325995.300000
    1000.0      1.0          0.5    999.5  341005.4000000  5306005.300000
    1000.0   2000.0       1999.5    999.5  341005.4000000  5325995.300000
The outer corners of the corner pixels are:
VICAR-line    -samp GeoTIFF-samp    -line            East           North
       0.5      0.5          0.0      0.0  351000.4000000  5306000.300000
       0.5   2000.5       2000.0      0.0  351000.4000000  5326000.300000
    1000.5      0.5          0.0   1000.0  341000.4000000  5306000.300000
    1000.5   2000.5       2000.0   1000.0  341000.4000000  5326000.300000
The rotation of the image relative to an E-N geographic frame is:
rotation 4
963
852
741
The scale units of the image are (ignoring sign):
1 sample = 10.000000000000 map units north
1 line   = 10.000000000000 map units east
The scale fraction is 1 /  39000.0
(assuming mapunit = 1.000000 meters and the map is 10.000000 inches)
gtgen out=xxxlab3 'tiecnvrt  +
   geotiff=("ModelTiePointTag=(1,1,0,350000.4,5306000.3,0.0)",  +
          "ModelTiePointTag=(101,101,0,351000.4,5307000.3,0.0)",  +
          "ModelTiePointTag=(101,1,0,350000.4,5307000.3,0.0)",  +
          "GTRasterTypeGeoKey=2(RasterPixelIsPoint)")
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
gtlist xxxlab3
Beginning VICAR task gtlist
gtlist version 2016-01-13
VICAR GeoTIFF LABEL LIST
The file is a standalone VICAR GeoTIFF label
A hypothetical 1000 x 2000 VICAR image will
be used to illustrate the mapping of corner points.
The VICAR GeoTIFF label is:
MODELTRANSFORMATIONTAG=(-0.0,10.0,0,349990.4,10.0,0.0,0,5305990.3,0,0,0,0,0,0,0,1)
GTRASTERTYPEGEOKEY=2(RasterPixelIsPoint)

The image raster is a 'point' or 'post' type
The centers of the corner pixels are:
VICAR-line    -samp GeoTIFF-samp    -line            East           North
       1.0      1.0          0.0      0.0  349990.4000000  5305990.300000
       1.0   2000.0       1999.0      0.0  349990.4000000  5325980.300000
    1000.0      1.0          0.0    999.0  359980.4000000  5305990.300000
    1000.0   2000.0       1999.0    999.0  359980.4000000  5325980.300000
The outer corners of the corner pixels are:
VICAR-line    -samp GeoTIFF-samp    -line            East           North
       0.5      0.5         -0.5     -0.5  349985.4000000  5305985.300000
       0.5   2000.5       1999.5     -0.5  349985.4000000  5325985.300000
    1000.5      0.5         -0.5    999.5  359985.4000000  5305985.300000
    1000.5   2000.5       1999.5    999.5  359985.4000000  5325985.300000
The rotation of the image relative to an E-N geographic frame is:
rotation 0
369
258
147
The scale units of the image are (ignoring sign):
1 sample = 10.000000000000 map units north
1 line   = 10.000000000000 map units east
The scale fraction is 1 /  39000.0
(assuming mapunit = 1.000000 meters and the map is 10.000000 inches)
gtgen out=xxxlab4 'tiecnvrt +
   geotiff=("ModelTiePointTag=(0,0,0,121.0,25.0,0.0)",  +
        "ModelTiePointTag=(1200,0,0,121.0,26.0,0.0)",  +
        "ModelTiePointTag=(0,1200,0,122.0,25.0,0.0)",  +
        "GTRasterTypeGeoKey=2(RasterPixelIsPoint)")
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
gtlist xxxlab4 mapunitm=75500.0 mapinch=20.0 listnl=1201 listns=1201
Beginning VICAR task gtlist
gtlist version 2016-01-13
VICAR GeoTIFF LABEL LIST
The file is a standalone VICAR GeoTIFF label
A hypothetical 1201 x 1201 VICAR image will
be used to illustrate the mapping of corner points.
The VICAR GeoTIFF label is:
MODELTRANSFORMATIONTAG=(-0.0,0.0008333333333,0,121.0,0.0008333333333,-0.0,0,25.0,0,0,0,0,0,0,0,1)
GTRASTERTYPEGEOKEY=2(RasterPixelIsPoint)

The image raster is a 'point' or 'post' type
The centers of the corner pixels are:
VICAR-line    -samp GeoTIFF-samp    -line            East           North
       1.0      1.0          0.0      0.0  121.0000000000  25.00000000000
       1.0   1201.0       1200.0      0.0  121.0000000000  25.99999999996
    1201.0      1.0          0.0   1200.0  122.0000000000  25.00000000000
    1201.0   1201.0       1200.0   1200.0  122.0000000000  25.99999999996
The outer corners of the corner pixels are:
VICAR-line    -samp GeoTIFF-samp    -line            East           North
       0.5      0.5         -0.5     -0.5  120.9995833333  24.99958333333
       0.5   1201.5       1200.5     -0.5  120.9995833333  26.00041666663
    1201.5      0.5         -0.5   1200.5  122.0004166666  24.99958333333
    1201.5   1201.5       1200.5   1200.5  122.0004166666  26.00041666663
The rotation of the image relative to an E-N geographic frame is:
rotation 0
369
258
147
The scale units of the image are (ignoring sign):
1 sample = 0.0008333333333 map units north
1 line   = 0.0008333333333 map units east
The scale fraction is 1 / 147347.7
(assuming mapunit = 75500.000000 meters and the map is 20.000000 inches)
gtgen out=xxxlab5 'tiecnvrt  +
   geotiff=("ModelTiePointTag=(2,3,0,350000.4,5307000.3,0.0)",  +
          "ModelTiePointTag=(102,103,0,351000.4,5305000.3,0.0)",  +
          "ModelTiePointTag=(102,3,0,351000.4,5307000.3,0.0)",  +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
gtlist xxxlab5
Beginning VICAR task gtlist
gtlist version 2016-01-13
VICAR GeoTIFF LABEL LIST
The file is a standalone VICAR GeoTIFF label
A hypothetical 1000 x 2000 VICAR image will
be used to illustrate the mapping of corner points.
The VICAR GeoTIFF label is:
MODELTIEPOINTTAG=(2,3,0,350000.4,5307000.3,0.0)
MODELPIXELSCALETAG=(10.0,20.0,0.0)
GTRASTERTYPEGEOKEY=1(RasterPixelIsArea)

The image raster is an 'area' type
The centers of the corner pixels are:
VICAR-line    -samp GeoTIFF-samp    -line            East           North
       1.0      1.0          0.5      0.5  349985.4000000  5307050.300000
       1.0   2000.0       1999.5      0.5  369975.4000000  5307050.300000
    1000.0      1.0          0.5    999.5  349985.4000000  5287070.300000
    1000.0   2000.0       1999.5    999.5  369975.4000000  5287070.300000
The outer corners of the corner pixels are:
VICAR-line    -samp GeoTIFF-samp    -line            East           North
       0.5      0.5          0.0      0.0  349980.4000000  5307060.300000
       0.5   2000.5       2000.0      0.0  369980.4000000  5307060.300000
    1000.5      0.5          0.0   1000.0  349980.4000000  5287060.300000
    1000.5   2000.5       2000.0   1000.0  369980.4000000  5287060.300000
The rotation of the image relative to an E-N geographic frame is:
rotation 1
123
456
789
The scale units of the image are (ignoring sign):
1 sample = 10.000000000000 map units east
1 line   = 20.000000000000 map units north
The scale fraction is 1 /  78000.0
(assuming mapunit = 1.000000 meters and the map is 10.000000 inches)
gen xxxim1 nl=500 ns=500
Beginning VICAR task gen
GEN Version 6
GEN task completed
gtgen inp=xxxim1 out=xxxim2 'tiecnvrt  +
   geotiff=("ModelTiePointTag=(0,0,0,350807.4,5317081.3,0.0)",  +
          "ModelTiePointTag=(100,100,0,351807.4,5316081.3,0.0)",  +
          "ModelTiePointTag=(100,0,0,351807.4,5317081.3,0.0)",  +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
label-list xxxim2
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File xxxim2 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                500 lines per band
                500 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Property: GEOTIFF ----
MODELTIEPOINTTAG='(0,0,0,350807.4,5317081.3,0.0)'
MODELPIXELSCALETAG='(10.0,10.0,0.0)'
GTRASTERTYPEGEOKEY='1(RasterPixelIsArea)'
---- Task: GEN -- User: wlb -- Wed Jan 13 14:35:34 2016 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: GTGEN -- User: wlb -- Wed Jan 13 14:35:34 2016 ----
 
************************************************************
gtlist xxxim2
Beginning VICAR task gtlist
gtlist version 2016-01-13
VICAR GeoTIFF LABEL LIST
The VICAR GeoTIFF label is:
MODELTIEPOINTTAG=(0,0,0,350807.4,5317081.3,0.0)
MODELPIXELSCALETAG=(10.0,10.0,0.0)
GTRASTERTYPEGEOKEY=1(RasterPixelIsArea)

The image raster is an 'area' type
The centers of the corner pixels are:
VICAR-line    -samp GeoTIFF-samp    -line            East           North
       1.0      1.0          0.5      0.5  350812.4000000  5317076.300000
       1.0    500.0        499.5      0.5  355802.4000000  5317076.300000
     500.0      1.0          0.5    499.5  350812.4000000  5312086.300000
     500.0    500.0        499.5    499.5  355802.4000000  5312086.300000
The outer corners of the corner pixels are:
VICAR-line    -samp GeoTIFF-samp    -line            East           North
       0.5      0.5          0.0      0.0  350807.4000000  5317081.300000
       0.5    500.5        500.0      0.0  355807.4000000  5317081.300000
     500.5      0.5          0.0    500.0  350807.4000000  5312081.300000
     500.5    500.5        500.0    500.0  355807.4000000  5312081.300000
The rotation of the image relative to an E-N geographic frame is:
rotation 1
123
456
789
The scale units of the image are (ignoring sign):
1 sample = 10.000000000000 map units east
1 line   = 10.000000000000 map units north
The scale fraction is 1 /  19500.0
(assuming mapunit = 1.000000 meters and the map is 10.000000 inches)
gtgen inp=xxxim1 out=xxxim2 'add geotiff=(  +
  "PCSCitationGeoKey=""UTM Zone 60 N with WGS84""")
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
label-list xxxim2
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File xxxim2 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                500 lines per band
                500 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Property: GEOTIFF ----
PCSCITATIONGEOKEY='"UTM Zone 60 N with WGS84"'
---- Task: GEN -- User: wlb -- Wed Jan 13 14:35:34 2016 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: GTGEN -- User: wlb -- Wed Jan 13 14:35:34 2016 ----
 
************************************************************
gtlist xxxim2
Beginning VICAR task gtlist
gtlist version 2016-01-13
No mapping in GeoTIFF label
The GeoTIFF label is:
PCSCITATIONGEOKEY="UTM Zone 60 N with WGS84"

gtgen inp=xxxim1 out=xxxim2  +
   geotiff=("ModelTiePointTag=(0,0,0,350807.4,5317081.3,0.0)",  +
          "ModelTiePointTag=(100,100,0,351807.4,5316081.3,0.0)",  +
          "ModelTiePointTag=(100,0,0,351807.4,5317081.3,0.0)",  +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
label-list xxxim2
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File xxxim2 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                500 lines per band
                500 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Property: GEOTIFF ----
MODELTIEPOINTTAG=('(0,0,0,350807.4,5317081.3,0.0)', 
'(100,100,0,351807.4,5316081.3,0.0)', '(100,0,0,351807.4,5317081.3,0.0)')
GTRASTERTYPEGEOKEY='1(RasterPixelIsArea)'
---- Task: GEN -- User: wlb -- Wed Jan 13 14:35:34 2016 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: GTGEN -- User: wlb -- Wed Jan 13 14:35:34 2016 ----
 
************************************************************
gtlist xxxim2
Beginning VICAR task gtlist
gtlist version 2016-01-13
No mapping in GeoTIFF label
The GeoTIFF label is:
MODELTIEPOINTTAG=(0,0,0,350807.4,5317081.3,0.0)
MODELTIEPOINTTAG=(100,100,0,351807.4,5316081.3,0.0)
MODELTIEPOINTTAG=(100,0,0,351807.4,5317081.3,0.0)
GTRASTERTYPEGEOKEY=1(RasterPixelIsArea)

gtgen out=xxxlab6   +
  geotiff=("GTRasterTypeGeoKey=1(RasterPixelIsArea)")
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
gtlist xxxlab6
Beginning VICAR task gtlist
gtlist version 2016-01-13
No mapping in GeoTIFF label
The GeoTIFF label is:
GTRASTERTYPEGEOKEY=1(RasterPixelIsArea)

disable-log
$ Return
$!#############################################################################
