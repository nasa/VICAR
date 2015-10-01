$!****************************************************************************
$!
$! Build proc for MIPL module pixmap
$! VPACK Version 1.9, Thursday, January 26, 2012, 13:58:26
$!
$! Execute by entering:		$ @pixmap
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
$ write sys$output "*** module pixmap ***"
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
$ write sys$output "Invalid argument given to pixmap.com file -- ", primary
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
$   if F$SEARCH("pixmap.imake") .nes. ""
$   then
$      vimake pixmap
$      purge pixmap.bld
$   else
$      if F$SEARCH("pixmap.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake pixmap
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @pixmap.bld "STD"
$   else
$      @pixmap.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create pixmap.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack pixmap.com -mixed -
	-s pixmap.c -
	-i pixmap.imake -
	-p pixmap.pdf -
	-t tstpixmap.pdf tstpixmap.log_solos
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create pixmap.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <math.h>
#include <string.h>
#include <ctype.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"

/*#include "cartoVicarProtos.h"*/
#include "cartoLsqUtils.h"
#include "cartoStrUtils.h"
#include "cartoMemUtils.h"
#include "cartoGtUtils.h"

/************************************************************************/
/* program pixmap                                                      */
/************************************************************************/
/*  99-08 ...alz... initial version                     */
/************************************************************************/

void main44(void)
{
   int i,cols[4],maptopix,pixtomap,ix,inpcnt,unit,colcount,coldef;
   int ibis,status,clen,labnl,labns,len,igcount,igdef;
   char *labelstr;
   double *xpar,*ypar,t[6],tinv[6],x,y,corner[4],ignore[2];
   double xmag,ymag;
           
   zifmessage("pixmap version Thu Jan  3 2008");
   
   /* get the basic parameters and calculate the mapping */
   
   maptopix = zvptst("maptopix");
   pixtomap = zvptst("pixtomap");
   if (maptopix&&pixtomap)
      zmabend("Only one keyword for mapping direction can be given");
   if (!maptopix&&!pixtomap)
      zmabend("One keyword for mapping direction must be given");
   
   if (maptopix) ix = 0; else ix = 2;
   zvparm("mapcols",&cols[ix],&colcount,&coldef,2,0);
   zvparm("pixcols",&cols[2-ix],&colcount,&coldef,2,0);
   status = zvpcnt("inp",&inpcnt);
   if (inpcnt!=2) zmabend("Requires two input files");
   zvparmd("ignore",ignore,&igcount,&igdef,2,0);
   if (igcount!=2) zmabend("Ignore param requires two values");
   
   printf("converting columns (%d,%d) to columns (%d,%d)\n",cols[0],
       cols[1],cols[2],cols[3]);
   
   /* calculate the mapping */
   
   status = gtgetlab("inp",2,&labelstr,&labnl,&labns);
   len = strlen(labelstr);
   for (i=0;i<len;i++) labelstr[i] = toupper(labelstr[i]);
   status = geofix(labelstr,t,tinv,labnl,labns,corner);
   if (status!=1) zmabend("Failed to get mapping from GeoTIFF label");
   if (ix==0) for (i=0;i<6;i++) t[i] = tinv[i];
   
   /* read in points from the ibis interface file */

   status = zvunit(&unit,"inp",1,NULL);
   status = IBISFileOpen(unit,&ibis,"update",0,0,0,0);
   if (status!=1) IBISSignalU(unit,status,1);
   IBISFileGet(ibis,"nr",&clen,1,1,0);
   mz_alloc1((unsigned char **)&xpar,clen,8);
   mz_alloc1((unsigned char **)&ypar,clen,8);
   
   for (i=0;i<4;i++)
      {
      status = IBISColumnSet(ibis,"U_FORMAT","DOUB",cols[i]);
      if (status!=1) IBISSignal(ibis,status,1);
      }
   
   status = IBISColumnRead(ibis,(char*)xpar,cols[0],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnRead(ibis,(char*)ypar,cols[1],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   
   /* calculate the output data */
   
   xmag = 0.001; ymag = 0.001;
   for (i=0;i<clen;i++)
      {
      xmag = MAX(fabs(xpar[i]),xmag);
      ymag = MAX(fabs(ypar[i]),ymag);
      }
   xmag *= 1.0e-14; ymag *= 1.0e-14;
   for (i=0;i<clen;i++)
      {
      x = xpar[i];
      y = ypar[i];
      if (fabs(x-ignore[0])<xmag&&fabs(y-ignore[1])<ymag)
         {
         xpar[i] = x;
         ypar[i] = y;
         }
      else
         {
         xpar[i] = x*t[0]+y*t[1]+t[2];
         ypar[i] = x*t[3]+y*t[4]+t[5];
         }
      }
   
   /* Output points to the ibis interface file */
   
   if (clen>0) status = IBISColumnWrite(ibis,(char*)xpar,cols[2],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   if (clen>0) status = IBISColumnWrite(ibis,(char*)ypar,cols[3],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   
   status = IBISFileClose(ibis,0);
   if (status!=1) IBISSignal(ibis,status,1);
  
   return;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create pixmap.imake
#define  PROGRAM   pixmap

#define MODULE_LIST pixmap.c

#define MAIN_LANG_C
#define R2LIB 

/* Comment this out before delivery.
#define DEBUG
*/

#define USES_ANSI_C

#define LIB_CARTO
#define LIB_P2SUB
#define LIB_TAE
#define LIB_RTL
$ Return
$!#############################################################################
$PDF_File:
$ create pixmap.pdf
PROCESS       HELP=*
PARM INP      TYPE=(STRING,99) COUNT=2
PARM MAPCOLS  TYPE=INTEGER COUNT=2
PARM PIXCOLS  TYPE=INTEGER COUNT=2
PARM MAPTOPIX TYPE=KEYWORD COUNT=(0:1) VALID=MAPTOPIX DEFAULT=--
PARM PIXTOMAP TYPE=KEYWORD COUNT=(0:1) VALID=PIXTOMAP DEFAULT=--
PARM IGNORE   TYPE=REAL COUNT=2 DEFAULT=(-999.0,-999.0)
END-PROC
.TITLE
VICAR Program pixmap
.HELP
PURPOSE

     PIXMAP converts map coordinates to/from vicar pixel coord-
     inates in an IBIS file using a GeoTIFF label to define the
     conversion.  NOTE THAT THE PIXEL COORDINATES WILL BE IN 
     THE VICAR COORDINATE SYSTEM, NOT IN THE GeoTIFF COORDINATE
     SYSTEM.  This allows the pixel coordinates to be used for
     any VICAR operation such as GEOM.
     
     The GeoTIFF label relates the pixel coordinate to the map
     coordinate, but it regards the upper left corner of the 
     image as (0.0,0.0) or (-0.5,-0.5) depending on the type
     of image ("area" or "post").  The PIXMAP program will
     automatically change that in the IBIS file to VICAR pixel
     referencing which regards the upper left corner as (0.5,0.5).
     
     The bottom line is that users will always work with VICAR
     coordinates except when initially setting up the GeoTIFF
     label.
     
     The user only has to name the IBIS file, the file or VICAR
     image containing the GeoTIFF label, the columns involved,
     and the direction of the mapping ('MAPTOPIX or 'PIXTOMAP
     keywords).  None of the keywords are defaulted except that
     only one of the directional keywords is used.
     
.PAGE
TAE COMMAND LINE FORMAT

     pixmap INP=(A,B) MAPCOLS=(E,N) PIXCOLS=(L,S) DIRKEYWORD
     
     where

     A           is an IBIS file 
     B		 is a VICAR image with a GeoTIFF label or a
                      standalone ascii GeoTIFF label
     MAPCOLS     are the IBIS file columns (integers) contain-
                      ing the map coordinates in the same order
                      as the GeoTIFF label (should be East
                      coordinate, then north coordinate)
     PIXCOLS     are the IBIS file columns (integers) contain-
                      ing the pixel coordinates in the order
                      line, sample (backwards from GeoTIFF)
     DIRKEYWORD  is one of the two keywords that tell the 
                      program which way to map
.PAGE
OPERATION
   The GeoTIFF label is accessed and a six coordinate mapping
   is calculated from the point-scale or matrix representation
   of the GeoTIFF mapping.  The inverse mapping is calculated
   for the case of coordinate to pixel conversion.
   
   The transformation is adjusted to convert from/to VICAR pixel
   coordinates as opposed to GeoTIFF pixel coordinates.
   
   The transformation is then applied to the input columns of
   the IBIS file, putting the result in the output columns.
   
   All transformations are calculated in double precision, even
   if the IBIS file is single precision.
   
.PAGE

TIMING

     Should be able to map millions if IBIS records in less than
     a minute.
     
RESTRICTIONS

   The maximum number of coordinate points is limited by IBIS table
   size (currently about 10 million?).  Internal to the program,
   dynamic memory allocation is used.


.PAGE
WRITTEN BY:            A. L. Zobrist, 27 August 1999

COGNIZANT PROGRAMMER:  B. A. McGuffie

REVISIONS: 
Thu Jan  3 2008 wlb switched to USES_ANSI_C AND LIB_CARTO; misc cleanup  

.LEVEL1
.VARIABLE INP
Input/output IBIS table and
GeoTIFF reference
.VARIABLE MAPCOLS
Map columns in IBIS file in
same order as GeoTIFF
.VARIABLE PIXCOLS
Pixel columns in IBIS file
in the order (line,samp)
.VARIABLE MAPTOPIX
Convert map to pixel
.VARIABLE PIXTOMAP
Convert pixel to map
.VARIABLE IGNORE
Coordinate values to ignore

.LEVEL2
.VARIABLE INP
       INP=A		 Input IBIS tabular file containing the
			 input points.  Also contains columns to
			 receive the result of mapping.  
.VARIABLE MAPCOLS
    COLS=(C1,C2)         Columns in the IBIS tabular file that
			 contain or will contain the mapping
			 coordinates (depending on the direction
			 of the mapping).  The order of C1 and C2
			 is the same as the order of coordinates
			 in the GeoTIFF label (for example, in
                         ModelTiePointTag=(0,0,0,121.0,55.0,0.0)
                         the order is lon then lat.  Generally,
                         the order should be East then North.
.VARIABLE PIXCOLS
    COLS=(C1,C2)         Columns in the IBIS tabular file that
			 contain or will contain the pixel
			 coordinates always in the order (line,
			 sample).
                          
.VARIABLE MAPTOPIX
    'MAPTOPIX		 Keyword to convert map to pixel.  One, but
                         not both of the mapping direction keywords
                         must be given.  The pixel column will be
                         erased and filled with the results of
                         conversion.
.VARIABLE PIXTOMAP
    'PIXTOMAP		 Keyword to convert pixel to map.  One, but
                         not both of the mapping direction keywords
                         must be given.  The map column will be
                         erased and filled with the results of
                         conversion.
.VARIABLE IGNORE
Coordinate values to ignore.  An input (whether map or pixel) will
be ignored if they test equal to this value within 1.0e-14.  The 
output value will be set to the input value.

This is a pair of values, so both values have to match to be ignored.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstpixmap.pdf
procedure
refgbl $echo
refgbl $autousage
parm version string def="ibis-2"
parm org string def="column"
body
!let _onfail="continue"
let $autousage="none"
let $echo="yes"

! TEST SCRIPT FOR PIXMAP

gen xim0 nl=100 ns=200
gtgen xim0 xim 'tiecnvrt +
   geotiff=("ModelTiePointTag=(2,3,0,350807.4,5317081.3,0.0)", +
          "ModelTiePointTag=(102,83,0,351807.4,5316081.3,0.0)", +
          "ModelTiePointTag=(2,83,0,350807.4,5316081.3,0.0)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
gtlist xim

ibis-gen a version=&version org=&org  datacol=(1,2,3,4) +
    nc=4 nr=2 deff="DOUB"
mf a func=("c1=index*99-98","c2=index*199-198")

ibis-list a 'format

pixmap (a,xim) mapcol=(3,4) pixcol=(1,2) 'pixtomap
ibis-list a 'format

mf a func=("c1=0","c2=0")
ibis-list a 'format

pixmap (a,xim) mapcol=(3,4) pixcol=(1,2) 'maptopix
ibis-list a 'format

! repeat for random points

ibis-gen a version=&version org=&org  datacol=(1,2,3,4) +
    nc=4 nr=10 deff="DOUB"
mf a func=("c1=sqrt(index*17+2763)","c2=sqrt(index*7+3431)")
mf a func=("c1=mod(c1,0.0001)*1000000","c2=mod(c2,0.0001)*1000000")

ibis-list a 'format

pixmap (a,xim) mapcol=(3,4) pixcol=(1,2) 'pixtomap
ibis-list a 'format

mf a func=("c1=0","c2=0")
ibis-list a 'format

pixmap (a,xim) mapcol=(3,4) pixcol=(1,2) 'maptopix
ibis-list a 'format


! test the ignore keyword, same as case above

gen xim0 nl=100 ns=200
gtgen xim0 xim 'tiecnvrt +
   geotiff=("ModelTiePointTag=(2,3,0,350807.4,5317081.3,0.0)", +
          "ModelTiePointTag=(102,83,0,351807.4,5316081.3,0.0)", +
          "ModelTiePointTag=(2,83,0,350807.4,5316081.3,0.0)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")

ibis-gen a version=&version org=&org  datacol=(1,2,3,4) +
    nc=4 nr=2 deff="DOUB"
mf a func=("c1=index*99-98","c2=index*199-198")

pixmap (a,xim) mapcol=(3,4) pixcol=(1,2) 'pixtomap +
      ignore=(100.0,200.0)
ibis-list a 'format

end-proc
$!-----------------------------------------------------------------------------
$ create tstpixmap.log_solos
tstpixmap
let $autousage="none"
let $echo="yes"
gen xim0 nl=100 ns=200
Beginning VICAR task gen
GEN Version 6
GEN task completed
gtgen xim0 xim 'tiecnvrt  +
   geotiff=("ModelTiePointTag=(2,3,0,350807.4,5317081.3,0.0)",  +
          "ModelTiePointTag=(102,83,0,351807.4,5316081.3,0.0)",  +
          "ModelTiePointTag=(2,83,0,350807.4,5316081.3,0.0)",  +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
gtlist xim
Beginning VICAR task gtlist
gtlist version Wed Jan  2 2008
ibis-gen a version=ibis-2 org=column  datacol=(1,2,3,4)  +
    nc=4 nr=2 deff="DOUB"
Beginning VICAR task ibis
mf a func=("c1=index*99-98","c2=index*199-198")
Beginning VICAR task mf
ibis-list a 'format
Beginning VICAR task ibis
 
Number of Rows:2  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:2
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
        DOUB        DOUB        DOUB        DOUB
+-----------+-----------+-----------+-----------
        1.00        1.00        0.00        0.00
      100.00      200.00        0.00        0.00
pixmap (a,xim) mapcol=(3,4) pixcol=(1,2) 'pixtomap
Beginning VICAR task pixmap
pixmap version Thu Jan  3 2008
ibis-list a 'format
Beginning VICAR task ibis
 
Number of Rows:2  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:2
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
        DOUB        DOUB        DOUB        DOUB
+-----------+-----------+-----------+-----------
        1.00        1.00   350792.40  5317112.55
      100.00      200.00   352782.40  5315875.05
mf a func=("c1=0","c2=0")
Beginning VICAR task mf
ibis-list a 'format
Beginning VICAR task ibis
 
Number of Rows:2  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:2
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
        DOUB        DOUB        DOUB        DOUB
+-----------+-----------+-----------+-----------
        0.00        0.00   350792.40  5317112.55
        0.00        0.00   352782.40  5315875.05
pixmap (a,xim) mapcol=(3,4) pixcol=(1,2) 'maptopix
Beginning VICAR task pixmap
pixmap version Thu Jan  3 2008
ibis-list a 'format
Beginning VICAR task ibis
 
Number of Rows:2  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:2
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
        DOUB        DOUB        DOUB        DOUB
+-----------+-----------+-----------+-----------
        1.00        1.00   350792.40  5317112.55
      100.00      200.00   352782.40  5315875.05
ibis-gen a version=ibis-2 org=column  datacol=(1,2,3,4)  +
    nc=4 nr=10 deff="DOUB"
Beginning VICAR task ibis
mf a func=("c1=sqrt(index*17+2763)","c2=sqrt(index*7+3431)")
Beginning VICAR task mf
mf a func=("c1=mod(c1,0.0001)*1000000","c2=mod(c2,0.0001)*1000000")
Beginning VICAR task mf
ibis-list a 'format
Beginning VICAR task ibis
 
Number of Rows:10  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:10
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
        DOUB        DOUB        DOUB        DOUB
+-----------+-----------+-----------+-----------
        5.53       61.93        0.00        0.00
       74.31       23.80        0.00        0.00
       51.00       24.63        0.00        0.00
       43.22       64.43        0.00        0.00
       58.61       47.00        0.00        0.00
       97.15       64.73        0.00        0.00
       62.68       25.23        0.00        0.00
       62.81       28.52        0.00        0.00
        1.36       66.95        0.00        0.00
       78.34       51.98        0.00        0.00
pixmap (a,xim) mapcol=(3,4) pixcol=(1,2) 'pixtomap
Beginning VICAR task pixmap
pixmap version Thu Jan  3 2008
ibis-list a 'format
Beginning VICAR task ibis
 
Number of Rows:10  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:10
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
        DOUB        DOUB        DOUB        DOUB
+-----------+-----------+-----------+-----------
        5.53       61.93   351401.70  5317055.99
       74.31       23.80   351020.37  5316196.18
       51.00       24.63   351028.69  5316487.57
       43.22       64.43   351426.65  5316584.80
       58.61       47.00   351252.41  5316392.48
       97.15       64.73   351429.67  5315910.64
       62.68       25.23   351034.73  5316341.57
       62.81       28.52   351067.58  5316339.92
        1.36       66.95   351451.93  5317108.00
       78.34       51.98   351302.23  5316145.81
mf a func=("c1=0","c2=0")
Beginning VICAR task mf
ibis-list a 'format
Beginning VICAR task ibis
 
Number of Rows:10  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:10
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
        DOUB        DOUB        DOUB        DOUB
+-----------+-----------+-----------+-----------
        0.00        0.00   351401.70  5317055.99
        0.00        0.00   351020.37  5316196.18
        0.00        0.00   351028.69  5316487.57
        0.00        0.00   351426.65  5316584.80
        0.00        0.00   351252.41  5316392.48
        0.00        0.00   351429.67  5315910.64
        0.00        0.00   351034.73  5316341.57
        0.00        0.00   351067.58  5316339.92
        0.00        0.00   351451.93  5317108.00
        0.00        0.00   351302.23  5316145.81
pixmap (a,xim) mapcol=(3,4) pixcol=(1,2) 'maptopix
Beginning VICAR task pixmap
pixmap version Thu Jan  3 2008
ibis-list a 'format
Beginning VICAR task ibis
 
Number of Rows:10  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:10
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
        DOUB        DOUB        DOUB        DOUB
+-----------+-----------+-----------+-----------
        5.53       61.93   351401.70  5317055.99
       74.31       23.80   351020.37  5316196.18
       51.00       24.63   351028.69  5316487.57
       43.22       64.43   351426.65  5316584.80
       58.61       47.00   351252.41  5316392.48
       97.15       64.73   351429.67  5315910.64
       62.68       25.23   351034.73  5316341.57
       62.81       28.52   351067.58  5316339.92
        1.36       66.95   351451.93  5317108.00
       78.34       51.98   351302.23  5316145.81
gen xim0 nl=100 ns=200
Beginning VICAR task gen
GEN Version 6
GEN task completed
gtgen xim0 xim 'tiecnvrt  +
   geotiff=("ModelTiePointTag=(2,3,0,350807.4,5317081.3,0.0)",  +
          "ModelTiePointTag=(102,83,0,351807.4,5316081.3,0.0)",  +
          "ModelTiePointTag=(2,83,0,350807.4,5316081.3,0.0)",  +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
ibis-gen a version=ibis-2 org=column  datacol=(1,2,3,4)  +
    nc=4 nr=2 deff="DOUB"
Beginning VICAR task ibis
mf a func=("c1=index*99-98","c2=index*199-198")
Beginning VICAR task mf
pixmap (a,xim) mapcol=(3,4) pixcol=(1,2) 'pixtomap  +
      ignore=(100.0,200.0)
Beginning VICAR task pixmap
pixmap version Thu Jan  3 2008
ibis-list a 'format
Beginning VICAR task ibis
 
Number of Rows:2  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:2
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
        DOUB        DOUB        DOUB        DOUB
+-----------+-----------+-----------+-----------
        1.00        1.00   350792.40  5317112.55
      100.00      200.00      100.00      200.00
end-proc
exit
slogoff
if ($RUNTYPE = "INTERACTIVE")
  if ($syschar(1) = "VAX_VMS")
  end-if
else
  if ($syschar(1) = "VAX_VMS")
  end-if
end-if
ulogoff
END-PROC
END-PROC
$ Return
$!#############################################################################
