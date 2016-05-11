$!****************************************************************************
$!
$! Build proc for MIPL module gtappend
$! VPACK Version 2.1, Wednesday, January 13, 2016, 15:06:33
$!
$! Execute by entering:		$ @gtappend
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
$ write sys$output "*** module gtappend ***"
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
$ write sys$output "Invalid argument given to gtappend.com file -- ", primary
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
$   if F$SEARCH("gtappend.imake") .nes. ""
$   then
$      vimake gtappend
$      purge gtappend.bld
$   else
$      if F$SEARCH("gtappend.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake gtappend
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @gtappend.bld "STD"
$   else
$      @gtappend.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create gtappend.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack gtappend.com -mixed -
	-s gtappend.c -
	-i gtappend.imake -
	-p gtappend.pdf -
	-t tstgtappend.pdf tstgtappend.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create gtappend.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <math.h>
#include <string.h>
#include <ctype.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"

#include "cartoMemUtils.h"
#include "cartoStrUtils.h"
#include "cartoGtUtils.h"
#include "cartoLsqUtils.h"

#ifndef MAX
#define MAX(a,b)	(((a)>(b))?(a):(b))
#endif

#ifndef MIN
#define MIN(a,b)	(((a)<(b))?(a):(b))
#endif


/*  image top to bottom concatenate   A. Zobrist    10/28/99   */

static char msgBuf[10000];

void main44(void)
{
   int i,img,iout,jout,inpcnt,vunit[48],inl[48],ins[48],pixsiz;
   int bignline,bignsamp=0,vsize[4],sizepcnt,sizedef,outnline;
   int tpixsiz=0,status,o_unit,dummy,outnsamp;
   int labnl,labns,len,rot1,rot2,gtfirst,tolerct,tolerdf;
   int sctype1,sctype2,mapck,lcount,overlap1;
   char *labelstr1,*labelstr2;
   unsigned char *outbuf;
   double t[6],tinv[6],r[6],rinv[6],corner[4],rcorner[4];
   double toler,xcorner,ycorner,lcorner,offline;
   double scale11,scale12,scale21,scale22;
   
   zifmessage("gtappend version 2016-01-13");
   
   /* get some parms */
   
   zvparm("SIZE",vsize,&sizepcnt,&sizedef,4,0);
   if (vsize[0]==1) zvp("SL",&vsize[0],&dummy);
   if (vsize[1]==1) zvp("SS",&vsize[1],&dummy);
   if (vsize[2]==0) zvp("NL",&vsize[2],&dummy);
   if (vsize[3]==0) zvp("NS",&vsize[3],&dummy);
   mapck = zvptst("MAPCK");
   overlap1 = zvptst("OVERLAP1");
   zvparmd("toler",&toler,&tolerct,&tolerdf,1,0);
   
   /* first the geotiff comparisons */
   
   status = gtgetlab("inp",1,&labelstr1,&labnl,&labns);
   gtfirst = status==1;
   if (gtfirst)
      {
      if (vsize[0]!=1||vsize[1]!=1||vsize[2]!=0||vsize[3]!=0)
         zmabend("Can't use size parms with GeoTIFF");
      len = strlen(labelstr1);
      for (i=0;i<len;i++) labelstr1[i] = toupper(labelstr1[i]);
      status = geofix(labelstr1,t,tinv,labnl,labns,corner);
      if (status!=1)
         zmabend("Failed to get mapping from GeoTIFF label, first input");
      }
   status = zvpcnt("inp",&inpcnt);
   offline = (double)labnl;
   if (overlap1) offline = offline-1.0;
   if (gtfirst&&mapck)
      for (img=1;img<inpcnt;img++)
      {
      status = gtgetlab("inp",img+1,&labelstr2,&labnl,&labns);
      if (status!=1)
         zmabend("Failed to get mapping from GeoTIFF label, i-th input");
      len = strlen(labelstr2);
      for (i=0;i<len;i++) labelstr2[i] = toupper(labelstr2[i]);
      status = gtrect(labelstr1,(double)1.0e-12);
      if (status!=1)
         zmabend("GTMSS is restricted to rectangular mappings when GeoTIFF");
      status = gtrect(labelstr2,(double)1.0e-12);
      if (status!=1)
         zmabend("GTMSS is restricted to rectangular mappings when GeoTIFF");
      status = geofix(labelstr2,r,rinv,labnl,labns,rcorner);
      if (status!=1)
         zmabend("Failed to get mapping from GeoTIFF label, i-th input");
      status = gtmapcom(labelstr1,labelstr2);
      if (status!=1) zmabend("Mappings not compatible");
      rot1 = gtgetrot(labelstr1);
      rot2 = gtgetrot(labelstr2);
      if (rot1!=rot2)
         zmabend("Different rotations for two inputs, use GTROTATE");
      gtgetscl(labelstr1,&sctype1,&scale11,&scale12);
      gtgetscl(labelstr2,&sctype2,&scale21,&scale22);
      if (sctype1!=sctype2) /* this is redundant, see rotation */
         zmabend("Different rotations for two inputs, use GTROTATE");
      if (fabs(scale11-scale21)>toler*fabs(scale11))
         zmabend("Different scales for two inputs, use GTSIZE");
      if (fabs(scale12-scale22)>toler*fabs(scale12))
         zmabend("Different scales for two inputs, use GTSIZE");
      xcorner = r[0]*(1.0-offline)+r[1]+r[2];
      ycorner = r[3]*(1.0-offline)+r[4]+r[5];
      lcorner = tinv[0]*xcorner+tinv[1]*ycorner+tinv[2];
      sprintf(msgBuf, "Mapping discrepancy at seam %d = %15.12f", img, lcorner-1.0);
      zifmessage(msgBuf);
      if (fabs(lcorner-1.0)>toler)
         zmabend("Mapping across seam exceeds tolerance parameter");
      offline += (double)labnl;
      if (overlap1) offline = offline-1.0;
      }
   if (gtfirst&&mapck) {
     sprintf(msgBuf, "All mapping discrepancies within tolerance");
     zifmessage(msgBuf);
   }
   
   /* then OK to simply cat the files, the GeoTIFF label of the
   first image simply becomes the label for the whole image */
   
   bignline = 0;
   for (i=0;i<inpcnt;i++)
      {
      status = zvunit(&vunit[i],"INP",i+1, NULL);
      status = zvopen(vunit[i],"OPEN_ACT","SA","IO_ACT","SA", NULL);
      zvget(vunit[i],"NL",&inl[i],"NS",&ins[i],"PIX_SIZE",&pixsiz, NULL);
      if (overlap1&&i<inpcnt-1) inl[i]--;
      
      /* resolve input samples */
      if (vsize[3]!=0) outnsamp = MIN(vsize[3],ins[i]-vsize[1]+1);
      else outnsamp = ins[i];
      if (vsize[3]>ins[i]-vsize[1]+1) {
	sprintf(msgBuf, "Output samples truncated to match input size, input %d", i+1);
	zifmessage(msgBuf);
      }
      ins[i] = outnsamp;
      
      /* resolve input lines */
      if (vsize[2]!=0) outnline = MIN(vsize[2],inl[i]-vsize[0]+1);
      else outnline = inl[i];
      if (vsize[2]>inl[i]-vsize[0]+1) {
	sprintf(msgBuf, "Output lines truncated to match input size, input %d", i+1);
	zifmessage(msgBuf);
      }
      inl[i] = outnline;
      
      if (i==0)
         {
         bignsamp = ins[0];
         tpixsiz = pixsiz;
         }
      else
         {
         if (ins[i]!=bignsamp) zmabend("Images must have same NS");
         if (pixsiz!=tpixsiz) zmabend("Images must have same pixel size");
         }
      bignline += inl[i];
      }
   
   status=zvunit( &o_unit,"OUT",1, NULL);
   status=zvopen( o_unit,"U_NL",bignline,"U_NS",bignsamp,
	"OP","WRITE","OPEN_ACT","SA","IO_ACT","SA", NULL);
   
   /* dynamically allocate the buffer */
   
   mz_alloc1((unsigned char **)&outbuf,bignsamp*pixsiz,1);
   
   /* read the inputs serially and concat for output */
   
   lcount = 1;
   for (jout=0;jout<inpcnt;jout++)
      {
      for (iout=0;iout<inl[jout];iout++)
         {
         status = zvread(vunit[jout],outbuf,"LINE",vsize[0]+iout,
            "SAMP",vsize[1],"NSAMPS",ins[jout], NULL);
         zvwrit(o_unit,outbuf,"LINE",lcount++,"SAMP",1,"NSAMPS",bignsamp, NULL);
         }
      }
   
   for (i=0;i<inpcnt;i++) zvclose(vunit[i], NULL);
   zvclose(o_unit, NULL);
   return;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create gtappend.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM gtappend

   To Create the build file give the command:

		$ vimake gtappend			(VMS)
   or
		% vimake gtappend			(Unix)


************************************************************************/


#define PROGRAM	gtappend

#define MODULE_LIST gtappend.c

#define MAIN_LANG_C
#define R2LIB
#define USES_ANSI_C

#define LIB_CARTO
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create gtappend.pdf
process help=*
PARM INP TYPE=STRING COUNT=2:48
PARM OUT TYPE=STRING
PARM SIZE TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SL TYPE=INTEGER DEFAULT=1
PARM SS TYPE=INTEGER DEFAULT=1
PARM NL TYPE=INTEGER DEFAULT=0
PARM NS TYPE=INTEGER DEFAULT=0
PARM TOLER TYPE=REAL DEFAULT=1.0E-3
PARM MAPCK TYPE=KEYWORD COUNT=(0:1) VALID=(MAPCK,NOMAPCK) +
     default=MAPCK
PARM OVERLAP1 TYPE=KEYWORD COUNT=(0:1) VALID=OVERLAP1 default=--
!# parm inp(3-48) hints=default
END-PROC
.TITLE
gtappend - combine images top to bottom, check GeoTIFF labels for position
.HELP
PURPOSE:
gtappend combines up to 48 datasets into a single dataset with append format.
This is equivalent to concatenating the input images in a top to bottom
fashion.  The program works like VICAR routine APPEND if there are no
GeoTIFF labels.  If there are, the GeoTIFF labels of the inputs are read
for any of several mistakes:

1.  any differences in mapping
2.  a difference in scale
3.  a difference in image rotation with respect to North
4.  an error of offset, the images must butt together geographically
    exactly as they are concatenated in the image sense.
    
If any of these errors occur, the program aborts with an appropriate
error message.

A correct GeoTIFF label is attached to the output (it will be a copy
of the first (topmost) input since its offset is (1,1) in the VICAR
pixel referencing system.

If a SIZE parameter is used, it applies to all of the input images.
The SIZE parameter is not allowed in GeoTIFF label cases.

EXECUTION:

Example

gtappend INP=(A,B,C) OUT=D  will put images A, B, and C top-to-bottom
to form D.

OPERATION:
gtappend combines datasets in the following manner:

First, the data sets are checked for GeoTIFF labels, and if they are
present, a number of conditions must be satisfied or the program
aborts:

1.  any differences in mapping
2.  a difference in scale
3.  a difference in image rotation with respect to North
4.  an error of offset, the images must butt together geographically
    exactly as they are concatenated in the image sense.
5.  if the first image has a GeoTIFF label then all must have one

The first input is read and copied to output (modified by SIZE if given).
Then the second image is read and copying to the output continues.  If
any of the images have a different NS, or a different pixel size, the
program abortw with an error message.

The program uses dynamic allocation of buffers in the C programming
language, so truly colossal images can be combined (for example, an
output image of 30 x 10,000,000) would not be a problem.

 TIMING: 

As fast as VICAR can read and write the lines.  

 ORIGINAL PROGRAMMER:    A. Zobrist          29 Oct 1999
 COGNIZANT PROGRAMMER:   Barbara McGuffie    29 Oct 1999
 
 REVISION HISTORY
  1999-10-29 AZ  Initial version
  2007-12-29 WLB Switched to USES_ANSI_C AND LIB_CARTO; misc cleanup
  2015-09-10 WLB Fixed headers for MIPL build
  2016-01-13 WLB Replaced printfs with zifmessages
  
.LEVEL1
.VARIABLE INP
STRING - Input image files
.VARIABLE OUT
STRING - Output image file
.VARIABLE SIZE
INTEGER - Region of input files
to be concatenated
.VARIABLE SL
INTEGER - Starting line
.VARIABLE SS
INTEGER - Starting sample
.VARIABLE NL
INTEGER - Number of lines
.VARIABLE NS
INTEGER - Number of samples
.VARIABLE TOLER
REAL - Error allowed in GeoTIFF
labels (unit is pixels)
.VARIABLE MAPCK
MAPCK - Will do GeoTIFF map
check (default)
NOMAPCK - Turns off GeoTIFF
map check
.VARIABLE OVERLAP1
Use if images overlap one
pixel, as in DMA DTED
.LEVEL2
.VARIABLE INP
INP specifies the input data sets.  Up to 48 are allowed.
.VARIABLE SIZE
The SIZE parameter may be used when only a sub-region of each image is to
be concatenated; it has the format SIZE=(SL,SS,NL,NS), where the parameters
are starting line, starting sample, number of lines, and number of samples,
respectively.  SIZE=(1,1,10,10), for example, will cause gtappend to only look
at the first ten samples of each of the first ten lines in each image, when
performing the concatenation. The default (1,1,0,0) means use the whole image.

The SIZE parameter is not allowed in GeoTIFF cases.
.VARIABLE SL
INTEGER - Starting line (see SIZE)
.VARIABLE SS
INTEGER - Starting sample (see SIZE)
.VARIABLE NL
INTEGER - Number of lines (see SIZE)
.VARIABLE NS
INTEGER - Number of samples (see SIZE)
.VARIABLE TOLER
As each image is butted, the mappings are checked for whether they
continue smoothly across the seam.  The default tolerance of one
millionth of a pixel allows for a fair amount of numeric error in
the calculations, which should always be carried out in double
precision.

See the main help for other checks on the mapping (coordinate system,
projection, datum, rotation, scale, etc must all be the same).
.VARIABLE MAPCK
The default, MAPCK, will do GeoTIFF map checking (or do nothing if there
are no GeoTIFF labels).  Using the other option, NOMAPCK, is useful to
add extra pixels to the bottom of a GeoTIFF image, or to adjoin
an image that is already known to be mapped correctly but doesn't have
a GeoTIFF label.  The first, or top, image must have a correct
GeoTIFF label.
.VARIABLE OVERLAP1
Use if images overlap one pixel, as in DMA DTED.  

The program will strip one pixel between each image, from the topmost
image of each pair.  The extra pixel in the last image is NOT stripped.
So two 601 pixel images will concatenate into a 1201 pixel image.

If geotiff labels are present, the location checking will account for
the overlap pixel.  If the images do not, in fact, have an overlap and
this keyword is used, an error would be reported if the geotiff labels
were present, because there would be a missing pixel.

Bottom line for geotiff labels, if there is not a clean butt or an overlap
of one pixel, you have to use imcopy to cut one of the images or 
featherv to mosaic the images.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstgtappend.pdf
procedure
refgbl $echo
refgbl $autousage
refgbl $syschar
body
enable-log

let $autousage = "none"
let _onfail="continue"
let $echo="yes"

! TEST SCRIPT FOR append

gen ba.img 3 4 
gen bb.img 2 4 ival=20
gen bc.img 4 4 ival=40
gtappend (ba.img, bb.img, bc.img) bd.img
list bd.img

gtappend (ba.img, bb.img, bc.img) bd.img SIZE=(1,1,3,3)
list bd.img


! try SL and SS not equal to 1.

gen ba.img 3 4 
gen bb.img 2 7 ival=20
gen bc.img 4 9 ival=40
gtappend (ba.img, bb.img, bc.img) be.img SIZE=(2,2,2,2)
list be.img

! TRY HALFWORD DATA

gen ha.img 3 4 'HALF
gen hb.img 2 4  'HALF ival=20
gen hc.img 4 4 'HALF ival=40

gtappend (ha.img, hb.img, hc.img) hd.img 
list hd.img

! try SL and SS not equal to 1.

gtappend (ha.img, hb.img, hc.img) he.img SIZE=(2,3,8,5)
list he.img


! TRY REAL*4 DATA

gen ra.img 3 4 'REAL4
gen rb.img 2 4  'REAL4 ival=20
gen rc.img 4 4 'REAL4 ival=40

gtappend (ra.img, rb.img, rc.img) rd.img 
list rd.img

! try SL and SS not equal to 1.

gtappend (ra.img, rb.img, rc.img) re.img SIZE=(2,2,2,2)
list re.img 'REAL4


! GeoTIFF label case, see devgtappend.pdf for many error detecting
! cases

gen xxxim1 nl=2 ns=5
gtgen inp=xxxim1 out=xxxim2 'tiecnvrt +
   geotiff=("ModelTiePointTag=(2,3,0,30000.,17000.,0.0)", +
          "ModelTiePointTag=(202,103,0,31000.,16000.,0.0)", +
          "ModelTiePointTag=(202,3,0,31000.,17000.,0.0)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
gtlist xxxim2
gen xxxim3 nl=4 ns=5
gtgen inp=xxxim3 out=xxxim4 'tiecnvrt +
   geotiff=("ModelTiePointTag=(2,1,0,30000.,17000.,0.0)", +
          "ModelTiePointTag=(202,101,0,31000.,16000.,0.0)", +
          "ModelTiePointTag=(202,1,0,31000.,17000.,0.0)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
gtlist xxxim4
gtappend (xxxim2,xxxim4) xxxim5
gtlist xxxim5

! GeoTIFF label case, testing 'overlap1 keyword


gen xxxim1 nl=3 ns=5
gtgen inp=xxxim1 out=xxxim2 'tiecnvrt +
   geotiff=("ModelTiePointTag=(2,3,0,30000.,17000.,0.0)", +
          "ModelTiePointTag=(202,103,0,31000.,16000.,0.0)", +
          "ModelTiePointTag=(202,3,0,31000.,17000.,0.0)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
gtlist xxxim2
gen xxxim3 nl=4 ns=5
gtgen inp=xxxim3 out=xxxim4 'tiecnvrt +
   geotiff=("ModelTiePointTag=(2,1,0,30000.,17000.,0.0)", +
          "ModelTiePointTag=(202,101,0,31000.,16000.,0.0)", +
          "ModelTiePointTag=(202,1,0,31000.,17000.,0.0)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
gtlist xxxim4
gtappend (xxxim2,xxxim4) xxxim5 'overlap1
gtlist xxxim5

list xxxim2
list xxxim4
list xxxim5


! GeoTIFF label case, using nomapck option just to adjoin data

gen xxxim1 nl=2 ns=5
gtgen inp=xxxim1 out=xxxim2 'tiecnvrt +
   geotiff=("ModelTiePointTag=(2,3,0,30000.,17000.,0.0)", +
          "ModelTiePointTag=(202,103,0,31000.,16000.,0.0)", +
          "ModelTiePointTag=(202,3,0,31000.,17000.,0.0)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
gtlist xxxim2
gen xxxim3 nl=4 ns=5 ival=20
gtappend (xxxim2,xxxim3) xxxim4 'nomapck
gtlist xxxim4
list xxxim4

theend>

disable-log
end-proc
$!-----------------------------------------------------------------------------
$ create tstgtappend.log
gen ba.img 3 4
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen bb.img 2 4 ival=20
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen bc.img 4 4 ival=40
Beginning VICAR task gen
GEN Version 6
GEN task completed
gtappend (ba.img, bb.img, bc.img) bd.img
Beginning VICAR task gtappend
gtappend version 2016-01-13
list bd.img
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Wed Jan 13 15:01:44 2016
 Task:GTAPPEND  User:wlb       Date_Time:Wed Jan 13 15:01:45 2016
     Samp     1       3
   Line
      1       0   1   2   3
      2       1   2   3   4
      3       2   3   4   5
      4      20  21  22  23
      5      21  22  23  24
      6      40  41  42  43
      7      41  42  43  44
      8      42  43  44  45
      9      43  44  45  46
gtappend (ba.img, bb.img, bc.img) bd.img SIZE=(1,1,3,3)
Beginning VICAR task gtappend
gtappend version 2016-01-13
Output lines truncated to match input size, input 2
list bd.img
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Wed Jan 13 15:01:44 2016
 Task:GTAPPEND  User:wlb       Date_Time:Wed Jan 13 15:01:45 2016
     Samp     1       3
   Line
      1       0   1   2
      2       1   2   3
      3       2   3   4
      4      20  21  22
      5      21  22  23
      6      40  41  42
      7      41  42  43
      8      42  43  44
gen ba.img 3 4
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen bb.img 2 7 ival=20
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen bc.img 4 9 ival=40
Beginning VICAR task gen
GEN Version 6
GEN task completed
gtappend (ba.img, bb.img, bc.img) be.img SIZE=(2,2,2,2)
Beginning VICAR task gtappend
gtappend version 2016-01-13
Output lines truncated to match input size, input 2
list be.img
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Wed Jan 13 15:01:45 2016
 Task:GTAPPEND  User:wlb       Date_Time:Wed Jan 13 15:01:45 2016
     Samp     1
   Line
      1       2   3
      2       3   4
      3      22  23
      4      42  43
      5      43  44
gen ha.img 3 4 'HALF
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen hb.img 2 4  'HALF ival=20
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen hc.img 4 4 'HALF ival=40
Beginning VICAR task gen
GEN Version 6
GEN task completed
gtappend (ha.img, hb.img, hc.img) hd.img
Beginning VICAR task gtappend
gtappend version 2016-01-13
list hd.img
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:wlb       Date_Time:Wed Jan 13 15:01:45 2016
 Task:GTAPPEND  User:wlb       Date_Time:Wed Jan 13 15:01:45 2016
     Samp       1     2     3     4
   Line
      1         0     1     2     3
      2         1     2     3     4
      3         2     3     4     5
      4        20    21    22    23
      5        21    22    23    24
      6        40    41    42    43
      7        41    42    43    44
      8        42    43    44    45
      9        43    44    45    46
gtappend (ha.img, hb.img, hc.img) he.img SIZE=(2,3,8,5)
Beginning VICAR task gtappend
gtappend version 2016-01-13
Output samples truncated to match input size, input 1
Output lines truncated to match input size, input 1
Output samples truncated to match input size, input 2
Output lines truncated to match input size, input 2
Output samples truncated to match input size, input 3
Output lines truncated to match input size, input 3
list he.img
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:wlb       Date_Time:Wed Jan 13 15:01:45 2016
 Task:GTAPPEND  User:wlb       Date_Time:Wed Jan 13 15:01:45 2016
     Samp       1     2
   Line
      1         3     4
      2         4     5
      3        23    24
      4        43    44
      5        44    45
      6        45    46
gen ra.img 3 4 'REAL4
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen rb.img 2 4  'REAL4 ival=20
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen rc.img 4 4 'REAL4 ival=40
Beginning VICAR task gen
GEN Version 6
GEN task completed
gtappend (ra.img, rb.img, rc.img) rd.img
Beginning VICAR task gtappend
gtappend version 2016-01-13
list rd.img
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:wlb       Date_Time:Wed Jan 13 15:01:45 2016
 Task:GTAPPEND  User:wlb       Date_Time:Wed Jan 13 15:01:45 2016
     Samp             1           2           3           4
   Line
      1       0.000E+00   1.000E+00   2.000E+00   3.000E+00
      2       1.000E+00   2.000E+00   3.000E+00   4.000E+00
      3       2.000E+00   3.000E+00   4.000E+00   5.000E+00
      4       2.000E+01   2.100E+01   2.200E+01   2.300E+01
      5       2.100E+01   2.200E+01   2.300E+01   2.400E+01
      6       4.000E+01   4.100E+01   4.200E+01   4.300E+01
      7       4.100E+01   4.200E+01   4.300E+01   4.400E+01
      8       4.200E+01   4.300E+01   4.400E+01   4.500E+01
      9       4.300E+01   4.400E+01   4.500E+01   4.600E+01
gtappend (ra.img, rb.img, rc.img) re.img SIZE=(2,2,2,2)
Beginning VICAR task gtappend
gtappend version 2016-01-13
Output lines truncated to match input size, input 2
list re.img 'REAL4
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:wlb       Date_Time:Wed Jan 13 15:01:45 2016
 Task:GTAPPEND  User:wlb       Date_Time:Wed Jan 13 15:01:45 2016
     Samp             1           2
   Line
      1       2.000E+00   3.000E+00
      2       3.000E+00   4.000E+00
      3       2.200E+01   2.300E+01
      4       4.200E+01   4.300E+01
      5       4.300E+01   4.400E+01
gen xxxim1 nl=2 ns=5
Beginning VICAR task gen
GEN Version 6
GEN task completed
gtgen inp=xxxim1 out=xxxim2 'tiecnvrt  +
   geotiff=("ModelTiePointTag=(2,3,0,30000.,17000.,0.0)",  +
          "ModelTiePointTag=(202,103,0,31000.,16000.,0.0)",  +
          "ModelTiePointTag=(202,3,0,31000.,17000.,0.0)",  +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
gtlist xxxim2
Beginning VICAR task gtlist
gtlist version 2016-01-13
VICAR GeoTIFF LABEL LIST
The VICAR GeoTIFF label is:
MODELTIEPOINTTAG=(2,3,0,30000.,17000.,0.0)
MODELPIXELSCALETAG=(5.0,10.0,0.0)
GTRASTERTYPEGEOKEY=1(RasterPixelIsArea)

The image raster is an 'area' type
The centers of the corner pixels are:
VICAR-line    -samp GeoTIFF-samp    -line            East           North
       1.0      1.0          0.5      0.5  29992.50000000  17025.00000000
       1.0      5.0          4.5      0.5  30012.50000000  17025.00000000
       2.0      1.0          0.5      1.5  29992.50000000  17015.00000000
       2.0      5.0          4.5      1.5  30012.50000000  17015.00000000
The outer corners of the corner pixels are:
VICAR-line    -samp GeoTIFF-samp    -line            East           North
       0.5      0.5          0.0      0.0  29990.00000000  17030.00000000
       0.5      5.5          5.0      0.0  30015.00000000  17030.00000000
       2.5      0.5          0.0      2.0  29990.00000000  17010.00000000
       2.5      5.5          5.0      2.0  30015.00000000  17010.00000000
The rotation of the image relative to an E-N geographic frame is:
rotation 1
123
456
789
The scale units of the image are (ignoring sign):
1 sample = 5.0000000000000 map units east
1 line   = 10.0000000000000 map units north
The scale fraction is 1 /     97.5
(assuming mapunit = 1.000000 meters and the map is 10.000000 inches)
gen xxxim3 nl=4 ns=5
Beginning VICAR task gen
GEN Version 6
GEN task completed
gtgen inp=xxxim3 out=xxxim4 'tiecnvrt  +
   geotiff=("ModelTiePointTag=(2,1,0,30000.,17000.,0.0)",  +
          "ModelTiePointTag=(202,101,0,31000.,16000.,0.0)",  +
          "ModelTiePointTag=(202,1,0,31000.,17000.,0.0)",  +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
gtlist xxxim4
Beginning VICAR task gtlist
gtlist version 2016-01-13
VICAR GeoTIFF LABEL LIST
The VICAR GeoTIFF label is:
MODELTIEPOINTTAG=(2,1,0,30000.,17000.,0.0)
MODELPIXELSCALETAG=(5.0,10.0,0.0)
GTRASTERTYPEGEOKEY=1(RasterPixelIsArea)

The image raster is an 'area' type
The centers of the corner pixels are:
VICAR-line    -samp GeoTIFF-samp    -line            East           North
       1.0      1.0          0.5      0.5  29992.50000000  17005.00000000
       1.0      5.0          4.5      0.5  30012.50000000  17005.00000000
       4.0      1.0          0.5      3.5  29992.50000000  16975.00000000
       4.0      5.0          4.5      3.5  30012.50000000  16975.00000000
The outer corners of the corner pixels are:
VICAR-line    -samp GeoTIFF-samp    -line            East           North
       0.5      0.5          0.0      0.0  29990.00000000  17010.00000000
       0.5      5.5          5.0      0.0  30015.00000000  17010.00000000
       4.5      0.5          0.0      4.0  29990.00000000  16970.00000000
       4.5      5.5          5.0      4.0  30015.00000000  16970.00000000
The rotation of the image relative to an E-N geographic frame is:
rotation 1
123
456
789
The scale units of the image are (ignoring sign):
1 sample = 5.0000000000000 map units east
1 line   = 10.0000000000000 map units north
The scale fraction is 1 /     97.5
(assuming mapunit = 1.000000 meters and the map is 10.000000 inches)
gtappend (xxxim2,xxxim4) xxxim5
Beginning VICAR task gtappend
gtappend version 2016-01-13
Mapping discrepancy at seam 1 = -0.000000000000
All mapping discrepancies within tolerance
gtlist xxxim5
Beginning VICAR task gtlist
gtlist version 2016-01-13
VICAR GeoTIFF LABEL LIST
The VICAR GeoTIFF label is:
MODELTIEPOINTTAG=(2,3,0,30000.,17000.,0.0)
MODELPIXELSCALETAG=(5.0,10.0,0.0)
GTRASTERTYPEGEOKEY=1(RasterPixelIsArea)

The image raster is an 'area' type
The centers of the corner pixels are:
VICAR-line    -samp GeoTIFF-samp    -line            East           North
       1.0      1.0          0.5      0.5  29992.50000000  17025.00000000
       1.0      5.0          4.5      0.5  30012.50000000  17025.00000000
       6.0      1.0          0.5      5.5  29992.50000000  16975.00000000
       6.0      5.0          4.5      5.5  30012.50000000  16975.00000000
The outer corners of the corner pixels are:
VICAR-line    -samp GeoTIFF-samp    -line            East           North
       0.5      0.5          0.0      0.0  29990.00000000  17030.00000000
       0.5      5.5          5.0      0.0  30015.00000000  17030.00000000
       6.5      0.5          0.0      6.0  29990.00000000  16970.00000000
       6.5      5.5          5.0      6.0  30015.00000000  16970.00000000
The rotation of the image relative to an E-N geographic frame is:
rotation 1
123
456
789
The scale units of the image are (ignoring sign):
1 sample = 5.0000000000000 map units east
1 line   = 10.0000000000000 map units north
The scale fraction is 1 /     97.5
(assuming mapunit = 1.000000 meters and the map is 10.000000 inches)
gen xxxim1 nl=3 ns=5
Beginning VICAR task gen
GEN Version 6
GEN task completed
gtgen inp=xxxim1 out=xxxim2 'tiecnvrt  +
   geotiff=("ModelTiePointTag=(2,3,0,30000.,17000.,0.0)",  +
          "ModelTiePointTag=(202,103,0,31000.,16000.,0.0)",  +
          "ModelTiePointTag=(202,3,0,31000.,17000.,0.0)",  +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
gtlist xxxim2
Beginning VICAR task gtlist
gtlist version 2016-01-13
VICAR GeoTIFF LABEL LIST
The VICAR GeoTIFF label is:
MODELTIEPOINTTAG=(2,3,0,30000.,17000.,0.0)
MODELPIXELSCALETAG=(5.0,10.0,0.0)
GTRASTERTYPEGEOKEY=1(RasterPixelIsArea)

The image raster is an 'area' type
The centers of the corner pixels are:
VICAR-line    -samp GeoTIFF-samp    -line            East           North
       1.0      1.0          0.5      0.5  29992.50000000  17025.00000000
       1.0      5.0          4.5      0.5  30012.50000000  17025.00000000
       3.0      1.0          0.5      2.5  29992.50000000  17005.00000000
       3.0      5.0          4.5      2.5  30012.50000000  17005.00000000
The outer corners of the corner pixels are:
VICAR-line    -samp GeoTIFF-samp    -line            East           North
       0.5      0.5          0.0      0.0  29990.00000000  17030.00000000
       0.5      5.5          5.0      0.0  30015.00000000  17030.00000000
       3.5      0.5          0.0      3.0  29990.00000000  17000.00000000
       3.5      5.5          5.0      3.0  30015.00000000  17000.00000000
The rotation of the image relative to an E-N geographic frame is:
rotation 1
123
456
789
The scale units of the image are (ignoring sign):
1 sample = 5.0000000000000 map units east
1 line   = 10.0000000000000 map units north
The scale fraction is 1 /     97.5
(assuming mapunit = 1.000000 meters and the map is 10.000000 inches)
gen xxxim3 nl=4 ns=5
Beginning VICAR task gen
GEN Version 6
GEN task completed
gtgen inp=xxxim3 out=xxxim4 'tiecnvrt  +
   geotiff=("ModelTiePointTag=(2,1,0,30000.,17000.,0.0)",  +
          "ModelTiePointTag=(202,101,0,31000.,16000.,0.0)",  +
          "ModelTiePointTag=(202,1,0,31000.,17000.,0.0)",  +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
gtlist xxxim4
Beginning VICAR task gtlist
gtlist version 2016-01-13
VICAR GeoTIFF LABEL LIST
The VICAR GeoTIFF label is:
MODELTIEPOINTTAG=(2,1,0,30000.,17000.,0.0)
MODELPIXELSCALETAG=(5.0,10.0,0.0)
GTRASTERTYPEGEOKEY=1(RasterPixelIsArea)

The image raster is an 'area' type
The centers of the corner pixels are:
VICAR-line    -samp GeoTIFF-samp    -line            East           North
       1.0      1.0          0.5      0.5  29992.50000000  17005.00000000
       1.0      5.0          4.5      0.5  30012.50000000  17005.00000000
       4.0      1.0          0.5      3.5  29992.50000000  16975.00000000
       4.0      5.0          4.5      3.5  30012.50000000  16975.00000000
The outer corners of the corner pixels are:
VICAR-line    -samp GeoTIFF-samp    -line            East           North
       0.5      0.5          0.0      0.0  29990.00000000  17010.00000000
       0.5      5.5          5.0      0.0  30015.00000000  17010.00000000
       4.5      0.5          0.0      4.0  29990.00000000  16970.00000000
       4.5      5.5          5.0      4.0  30015.00000000  16970.00000000
The rotation of the image relative to an E-N geographic frame is:
rotation 1
123
456
789
The scale units of the image are (ignoring sign):
1 sample = 5.0000000000000 map units east
1 line   = 10.0000000000000 map units north
The scale fraction is 1 /     97.5
(assuming mapunit = 1.000000 meters and the map is 10.000000 inches)
gtappend (xxxim2,xxxim4) xxxim5 'overlap1
Beginning VICAR task gtappend
gtappend version 2016-01-13
Mapping discrepancy at seam 1 = -0.000000000000
All mapping discrepancies within tolerance
gtlist xxxim5
Beginning VICAR task gtlist
gtlist version 2016-01-13
VICAR GeoTIFF LABEL LIST
The VICAR GeoTIFF label is:
MODELTIEPOINTTAG=(2,3,0,30000.,17000.,0.0)
MODELPIXELSCALETAG=(5.0,10.0,0.0)
GTRASTERTYPEGEOKEY=1(RasterPixelIsArea)

The image raster is an 'area' type
The centers of the corner pixels are:
VICAR-line    -samp GeoTIFF-samp    -line            East           North
       1.0      1.0          0.5      0.5  29992.50000000  17025.00000000
       1.0      5.0          4.5      0.5  30012.50000000  17025.00000000
       6.0      1.0          0.5      5.5  29992.50000000  16975.00000000
       6.0      5.0          4.5      5.5  30012.50000000  16975.00000000
The outer corners of the corner pixels are:
VICAR-line    -samp GeoTIFF-samp    -line            East           North
       0.5      0.5          0.0      0.0  29990.00000000  17030.00000000
       0.5      5.5          5.0      0.0  30015.00000000  17030.00000000
       6.5      0.5          0.0      6.0  29990.00000000  16970.00000000
       6.5      5.5          5.0      6.0  30015.00000000  16970.00000000
The rotation of the image relative to an E-N geographic frame is:
rotation 1
123
456
789
The scale units of the image are (ignoring sign):
1 sample = 5.0000000000000 map units east
1 line   = 10.0000000000000 map units north
The scale fraction is 1 /     97.5
(assuming mapunit = 1.000000 meters and the map is 10.000000 inches)
list xxxim2
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Wed Jan 13 15:01:45 2016
 Task:GTGEN     User:wlb       Date_Time:Wed Jan 13 15:01:45 2016
     Samp     1       3       5
   Line
      1       0   1   2   3   4
      2       1   2   3   4   5
      3       2   3   4   5   6
list xxxim4
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Wed Jan 13 15:01:45 2016
 Task:GTGEN     User:wlb       Date_Time:Wed Jan 13 15:01:45 2016
     Samp     1       3       5
   Line
      1       0   1   2   3   4
      2       1   2   3   4   5
      3       2   3   4   5   6
      4       3   4   5   6   7
list xxxim5
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Wed Jan 13 15:01:45 2016
 Task:GTAPPEND  User:wlb       Date_Time:Wed Jan 13 15:01:45 2016
     Samp     1       3       5
   Line
      1       0   1   2   3   4
      2       1   2   3   4   5
      3       0   1   2   3   4
      4       1   2   3   4   5
      5       2   3   4   5   6
      6       3   4   5   6   7
gen xxxim1 nl=2 ns=5
Beginning VICAR task gen
GEN Version 6
GEN task completed
gtgen inp=xxxim1 out=xxxim2 'tiecnvrt  +
   geotiff=("ModelTiePointTag=(2,3,0,30000.,17000.,0.0)",  +
          "ModelTiePointTag=(202,103,0,31000.,16000.,0.0)",  +
          "ModelTiePointTag=(202,3,0,31000.,17000.,0.0)",  +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
gtlist xxxim2
Beginning VICAR task gtlist
gtlist version 2016-01-13
VICAR GeoTIFF LABEL LIST
The VICAR GeoTIFF label is:
MODELTIEPOINTTAG=(2,3,0,30000.,17000.,0.0)
MODELPIXELSCALETAG=(5.0,10.0,0.0)
GTRASTERTYPEGEOKEY=1(RasterPixelIsArea)

The image raster is an 'area' type
The centers of the corner pixels are:
VICAR-line    -samp GeoTIFF-samp    -line            East           North
       1.0      1.0          0.5      0.5  29992.50000000  17025.00000000
       1.0      5.0          4.5      0.5  30012.50000000  17025.00000000
       2.0      1.0          0.5      1.5  29992.50000000  17015.00000000
       2.0      5.0          4.5      1.5  30012.50000000  17015.00000000
The outer corners of the corner pixels are:
VICAR-line    -samp GeoTIFF-samp    -line            East           North
       0.5      0.5          0.0      0.0  29990.00000000  17030.00000000
       0.5      5.5          5.0      0.0  30015.00000000  17030.00000000
       2.5      0.5          0.0      2.0  29990.00000000  17010.00000000
       2.5      5.5          5.0      2.0  30015.00000000  17010.00000000
The rotation of the image relative to an E-N geographic frame is:
rotation 1
123
456
789
The scale units of the image are (ignoring sign):
1 sample = 5.0000000000000 map units east
1 line   = 10.0000000000000 map units north
The scale fraction is 1 /     97.5
(assuming mapunit = 1.000000 meters and the map is 10.000000 inches)
gen xxxim3 nl=4 ns=5 ival=20
Beginning VICAR task gen
GEN Version 6
GEN task completed
gtappend (xxxim2,xxxim3) xxxim4 'nomapck
Beginning VICAR task gtappend
gtappend version 2016-01-13
gtlist xxxim4
Beginning VICAR task gtlist
gtlist version 2016-01-13
VICAR GeoTIFF LABEL LIST
The VICAR GeoTIFF label is:
MODELTIEPOINTTAG=(2,3,0,30000.,17000.,0.0)
MODELPIXELSCALETAG=(5.0,10.0,0.0)
GTRASTERTYPEGEOKEY=1(RasterPixelIsArea)

The image raster is an 'area' type
The centers of the corner pixels are:
VICAR-line    -samp GeoTIFF-samp    -line            East           North
       1.0      1.0          0.5      0.5  29992.50000000  17025.00000000
       1.0      5.0          4.5      0.5  30012.50000000  17025.00000000
       6.0      1.0          0.5      5.5  29992.50000000  16975.00000000
       6.0      5.0          4.5      5.5  30012.50000000  16975.00000000
The outer corners of the corner pixels are:
VICAR-line    -samp GeoTIFF-samp    -line            East           North
       0.5      0.5          0.0      0.0  29990.00000000  17030.00000000
       0.5      5.5          5.0      0.0  30015.00000000  17030.00000000
       6.5      0.5          0.0      6.0  29990.00000000  16970.00000000
       6.5      5.5          5.0      6.0  30015.00000000  16970.00000000
The rotation of the image relative to an E-N geographic frame is:
rotation 1
123
456
789
The scale units of the image are (ignoring sign):
1 sample = 5.0000000000000 map units east
1 line   = 10.0000000000000 map units north
The scale fraction is 1 /     97.5
(assuming mapunit = 1.000000 meters and the map is 10.000000 inches)
list xxxim4
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Wed Jan 13 15:01:45 2016
 Task:GTAPPEND  User:wlb       Date_Time:Wed Jan 13 15:01:45 2016
     Samp     1       3       5
   Line
      1       0   1   2   3   4
      2       1   2   3   4   5
      3      20  21  22  23  24
      4      21  22  23  24  25
      5      22  23  24  25  26
      6      23  24  25  26  27
disable-log
$ Return
$!#############################################################################
