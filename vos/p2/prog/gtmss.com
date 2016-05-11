$!****************************************************************************
$!
$! Build proc for MIPL module gtmss
$! VPACK Version 2.1, Friday, January 08, 2016, 13:06:37
$!
$! Execute by entering:		$ @gtmss
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
$ write sys$output "*** module gtmss ***"
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
$ write sys$output "Invalid argument given to gtmss.com file -- ", primary
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
$   if F$SEARCH("gtmss.imake") .nes. ""
$   then
$      vimake gtmss
$      purge gtmss.bld
$   else
$      if F$SEARCH("gtmss.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake gtmss
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @gtmss.bld "STD"
$   else
$      @gtmss.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create gtmss.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack gtmss.com -mixed -
	-s gtmss.c -
	-i gtmss.imake -
	-p gtmss.pdf -
	-t tstgtmss.pdf tstgtmss.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create gtmss.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <math.h>
#include <string.h>
#include <ctype.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"

#include "cartoMemUtils.h"
#include "cartoLsqUtils.h"
#include "cartoStrUtils.h"
#include "cartoGtUtils.h"

#ifndef MAX
#define MAX(a,b)	(((a)>(b))?(a):(b))
#endif

#ifndef MIN
#define MIN(a,b)	(((a)<(b))?(a):(b))
#endif

/*  image left to right concatenate   A. Zobrist    10/28/99   */

void main44(void)
{
   int i,img,iout,jout,inpcnt,vunit[48],inl[48],ins[48],pixsiz;
   int bignline=0,bignsamp,vsize[4],sizepcnt,sizedef,outnline=0;
   int inptr[48],tpixsiz=0,status,o_unit,dummy,outnsamp;
   int labnl,labns,len,rot1,rot2,gtfirst,tolerct,tolerdf;
   int sctype1,sctype2,mapck,overlap1;
   char *labelstr1,*labelstr2;
   unsigned char *outbuf;
   double t[6],tinv[6],r[6],rinv[6],corner[4],rcorner[4];
   double toler,xcorner,ycorner,scorner,offsamp;
   double scale11,scale12,scale21,scale22;
   
   zifmessage("gtmss version 2015-09-10");
   
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
   offsamp = (double)labns;
   if (overlap1) offsamp = offsamp-1.0;
   if (gtfirst&&mapck) for (img=1;img<inpcnt;img++)
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
      xcorner = r[0]+r[1]*(1.0-offsamp)+r[2];
      ycorner = r[3]+r[4]*(1.0-offsamp)+r[5];
      scorner = tinv[3]*xcorner+tinv[4]*ycorner+tinv[5];
      printf("Mapping discrepancy at seam %d = %15.12f\n",img,scorner-1.0);
      if (fabs(scorner-1.0)>toler)
         zmabend("Mapping across seam exceeds tolerance parameter");
      offsamp += (double)labns;
      if (overlap1) offsamp = offsamp-1.0;
      }
   if (gtfirst&&mapck)
      printf("All mapping discrepancies within tolerance\n");
      
   
   /* then OK to simply cat the files, the GeoTIFF label of the
   first image simply becomes the label for the whole image */
   
   bignsamp = 0;
   for (i=0;i<inpcnt;i++)
      {
      status = zvunit(&vunit[i],"INP",i+1, NULL);
      status = zvopen(vunit[i],"OPEN_ACT","SA","IO_ACT","SA", NULL);
      zvget(vunit[i],"NL",&inl[i],"NS",&ins[i],"PIX_SIZE",&pixsiz, NULL);
      if (overlap1&&i<inpcnt-1) ins[i]--;
      
      /* resolve input samples */
      if (vsize[3]!=0) outnsamp = MIN(vsize[3],ins[i]-vsize[1]+1);
      else outnsamp = ins[i];
      if (vsize[3]>ins[i]-vsize[1]+1)
         printf("\nOutput samples truncated to match input size, input %d\n\n",
                  i+1);
      ins[i] = outnsamp;
      
      /* resolve input lines */
      if (vsize[2]!=0) outnline = MIN(vsize[2],inl[i]-vsize[0]+1);
      else outnline = inl[i];
      if (vsize[2]>inl[i]-vsize[0]+1)printf(
         "\nOutput lines truncated to match input size, input %d\n\n",i+1);
      inl[i] = outnline;
      
      if (i==0)
         {
         bignline = inl[0];
         inptr[0] = 0;
         tpixsiz = pixsiz;
         }
      else
         {
         if (inl[i]!=bignline) zmabend("Images must have same NL");
         if (pixsiz!=tpixsiz) zmabend("Images must have same pixel size");
         inptr[i] = inptr[i-1]+ins[i-1]*pixsiz;
         }
      bignsamp += ins[i];
      }
   
   status=zvunit( &o_unit,"OUT",1, NULL);
   status=zvopen( o_unit,"U_NL",bignline,"U_NS",bignsamp,
	"OP","WRITE","OPEN_ACT","SA","IO_ACT","SA", NULL);
   
   /* dynamically allocate the buffer */
   
   mz_alloc1((unsigned char **)&outbuf,bignsamp*pixsiz,1);
   
   /* read the input lines and concat for output */
   
   for (iout=0;iout<outnline;iout++)
      {
      for (jout=0;jout<inpcnt;jout++)
         {
         status = zvread(vunit[jout],&outbuf[inptr[jout]],
            "LINE",iout+vsize[0],"SAMP",vsize[1],
            "NSAMPS",ins[jout], NULL);
         }
      zvwrit(o_unit,outbuf,"LINE",iout+1,"SAMP",1,"NSAMPS",bignsamp, NULL);
      }
   
   
   for (i=0;i<inpcnt;i++) zvclose(vunit[i], NULL);
   zvclose(o_unit, NULL);
   return;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create gtmss.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM gtmss

   To Create the build file give the command:

		$ vimake gtmss			(VMS)
   or
		% vimake gtmss			(Unix)


************************************************************************/


#define PROGRAM	gtmss

#define MODULE_LIST gtmss.c

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
$ create gtmss.pdf
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
gtmss - combine images left to right, check GeoTIFF labels for position
.HELP
PURPOSE:
gtmss combines up to 48 datasets into a single dataset with mss format.
This is equivalent to concatenating the input images in a left to right
fashion.  The program works like VICAR routine MSS if there are no
GeoTIFF labels.  If there are, the GeoTIFF labels of the inputs are read
for any of several mistakes::

1.  any differences in mapping
2.  a difference in scale
3.  a difference in image rotation with respect to North
4.  an error of offset, the images must butt together geographically
    exactly as they are concatenated in the image sense.
    
If any of these errors occur, the program aborts with an appropriate
error message.

A correct GeoTIFF label is attached to the output (it will be a copy
of the first (leftmost) input since its offset is (1,1) in the VICAR
pixel referencing system.

If a SIZE parameter is used, it applies to all of the input images.

EXECUTION:

Example

gtmss INP=(A,B,C) OUT=D  will put images A, B, and C side-by-side to form D.

OPERATION:
gtmss combines datasets in the following manner:  
Each line is made up of the corresponding input lines laid end to end in
a concatenated manner.  That is, the first pixel of each input is placed
to the right of the last pixel of the previous input.  The line thus
formed will have the same number of samples per line as the sum of the
inputs. (If the SIZE field is used, it will be NS * #-inputs.)

The program uses dynamic allocation of buffers in the C programming
language, so truly colossal images can be combined (for example, an
output image of 30 x 10,000,000) would not be a problem.

 TIMING: 

As fast as VICAR can read and write the lines.  

 ORIGINAL PROGRAMMER:    A. Zobrist          29 Oct 1999
 COGNIZANT PROGRAMMER:   Barbara McGuffie    29 Oct 1999
 
 REVISION HISTORY
  1999-10-29 AZ  Initial version
  2008-01-03 WLB Switched to USES_ANSI_C AND LIB_CARTO; misc cleanup  
  2015-09-10 WLB Fixed headers for MIPL build
  
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
respectively.  SIZE=(1,1,10,10), for example, will cause gtmss to only look
at the first ten samples of each of the first ten lines in each image, when
performing the concatenation. The default (1,1,0,0) means use the whole image.
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

The program will strip one pixel between each image, from the leftmost
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
$ create tstgtmss.pdf
procedure
refgbl $echo
refgbl $autousage
refgbl $syschar
body
enable-log
let $autousage = "none"
let _onfail="continue"
let $echo="yes"

! TEST SCRIPT FOR mss

gen ba.img 4 3 
gen bb.img 4 2 ival=20
gen bc.img 4 4 ival=40
gtmss (ba.img, bb.img, bc.img) bd.img
list bd.img

gtmss (ba.img, bb.img, bc.img) bd.img SIZE=(1,1,2,3)
list bd.img

gtmss (ba.img, bb.img, bc.img) bd.img SIZE=(1,1,6,6)
list bd.img

! try SL and SS not equal to 1.

gtmss (ba.img, bb.img, bc.img) be.img SIZE=(2,2,2,1)
list be.img
 
! TRY HALFWORD DATA

gen ha.img 10 10 'HALF
gen hb.img 10 8  'HALF ival=20
gen hc.img 10 10 'HALF ival=40

gtmss (ha.img, hb.img, hc.img) hd.img 
list hd.img
 
! try SL and SS not equal to 1.

gtmss (ha.img, hb.img, hc.img) he.img SIZE=(2,3,8,5)
list he.img

! TRY REAL*4 DATA

gen ra.img 4 3 'REAL4
gen rb.img 4 2  'REAL4 ival=20
gen rc.img 4 3 'REAL4 ival=40

gtmss (ra.img, rb.img, rc.img) rd.img 
list rd.img


! try SL and SS not equal to 1.

gtmss (ra.img, rb.img, rc.img) re.img SIZE=(2,2,2,1)
list re.img 'REAL4


! correctly mapped case using GeoTIFF labels

gen xxxim1 nl=5 ns=3
gtgen inp=xxxim1 out=xxxim2 'tiecnvrt +
   geotiff=("ModelTiePointTag=(2,3,0,30000.,17000.,0.0)", +
          "ModelTiePointTag=(202,103,0,31000.,16000.,0.0)", +
          "ModelTiePointTag=(202,3,0,31000.,17000.,0.0)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
gtlist xxxim2
gen xxxim3 nl=5 ns=2
gtgen inp=xxxim3 out=xxxim4 'tiecnvrt +
   geotiff=("ModelTiePointTag=(-1,3,0,30000.,17000.,0.0)", +
          "ModelTiePointTag=(199,103,0,31000.,16000.,0.0)", +
          "ModelTiePointTag=(199,3,0,31000.,17000.,0.0)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
gtlist xxxim4
gtmss (xxxim2,xxxim4) xxxim5
gtlist xxxim5

! correctly mapped case using GeoTIFF labels, overlap1 keyword
! will strip 1 row of pixels off left image

gen xxxim1 nl=5 ns=4
gtgen inp=xxxim1 out=xxxim2 'tiecnvrt +
   geotiff=("ModelTiePointTag=(2,3,0,500000.,1700000.,0.0)", +
          "ModelTiePointTag=(202,103,0,501000.,1600000.,0.0)", +
          "ModelTiePointTag=(202,3,0,501000.,1700000.,0.0)", +
          "ProjectedCSTypeGeoKey=32637(PCS_WGS84_UTM_zone_37N)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
gtlist xxxim2
gen xxxim3 nl=5 ns=2
gtgen inp=xxxim3 out=xxxim4 'tiecnvrt +
   geotiff=("ModelTiePointTag=(-1,3,0,500000.,1700000.,0.0)", +
          "ModelTiePointTag=(199,103,0,501000.,1600000.,0.0)", +
          "ModelTiePointTag=(199,3,0,501000.,1700000.,0.0)", +
          "ProjectedCSTypeGeoKey=32637(PCS_WGS84_UTM_zone_37N)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
gtlist xxxim4
gtmss (xxxim2,xxxim4) xxxim5 'overlap1
gtlist xxxim5

list xxxim2
list xxxim4
list xxxim5

! incorrectly mapped case using GeoTIFF labels, nomapck allows

gen xxxim1 nl=5 ns=3
gtgen inp=xxxim1 out=xxxim2 'tiecnvrt +
   geotiff=("ModelTiePointTag=(2,3,0,30000.,17000.,0.0)", +
          "ModelTiePointTag=(202,103,0,31000.,16000.,0.0)", +
          "ModelTiePointTag=(202,3,0,31000.,17000.,0.0)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
gtlist xxxim2
gen xxxim3 nl=5 ns=2
gtmss (xxxim2,xxxim3) xxxim4 'nomapck
gtlist xxxim4


theend>
disable-log
end-proc
$!-----------------------------------------------------------------------------
$ create tstgtmss.log
gen ba.img 4 3
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen bb.img 4 2 ival=20
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen bc.img 4 4 ival=40
Beginning VICAR task gen
GEN Version 6
GEN task completed
gtmss (ba.img, bb.img, bc.img) bd.img
Beginning VICAR task gtmss
gtmss version 2015-09-10
list bd.img
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Fri Jan  8 13:05:23 2016
 Task:GTMSS     User:wlb       Date_Time:Fri Jan  8 13:05:23 2016
     Samp     1       3       5       7       9
   Line
      1       0   1   2  20  21  40  41  42  43
      2       1   2   3  21  22  41  42  43  44
      3       2   3   4  22  23  42  43  44  45
      4       3   4   5  23  24  43  44  45  46
gtmss (ba.img, bb.img, bc.img) bd.img SIZE=(1,1,2,3)
Beginning VICAR task gtmss
gtmss version 2015-09-10
list bd.img
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Fri Jan  8 13:05:23 2016
 Task:GTMSS     User:wlb       Date_Time:Fri Jan  8 13:05:23 2016
     Samp     1       3       5       7
   Line
      1       0   1   2  20  21  40  41  42
      2       1   2   3  21  22  41  42  43
gtmss (ba.img, bb.img, bc.img) bd.img SIZE=(1,1,6,6)
Beginning VICAR task gtmss
gtmss version 2015-09-10
list bd.img
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Fri Jan  8 13:05:23 2016
 Task:GTMSS     User:wlb       Date_Time:Fri Jan  8 13:05:23 2016
     Samp     1       3       5       7       9
   Line
      1       0   1   2  20  21  40  41  42  43
      2       1   2   3  21  22  41  42  43  44
      3       2   3   4  22  23  42  43  44  45
      4       3   4   5  23  24  43  44  45  46
gtmss (ba.img, bb.img, bc.img) be.img SIZE=(2,2,2,1)
Beginning VICAR task gtmss
gtmss version 2015-09-10
list be.img
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Fri Jan  8 13:05:23 2016
 Task:GTMSS     User:wlb       Date_Time:Fri Jan  8 13:05:23 2016
     Samp     1       3
   Line
      1       2  22  42
      2       3  23  43
gen ha.img 10 10 'HALF
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen hb.img 10 8  'HALF ival=20
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen hc.img 10 10 'HALF ival=40
Beginning VICAR task gen
GEN Version 6
GEN task completed
gtmss (ha.img, hb.img, hc.img) hd.img
Beginning VICAR task gtmss
gtmss version 2015-09-10
list hd.img
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:wlb       Date_Time:Fri Jan  8 13:05:23 2016
 Task:GTMSS     User:wlb       Date_Time:Fri Jan  8 13:05:23 2016
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14    15
   Line
      1         0     1     2     3     4     5     6     7     8     9    20    21    22    23    24
      2         1     2     3     4     5     6     7     8     9    10    21    22    23    24    25
      3         2     3     4     5     6     7     8     9    10    11    22    23    24    25    26
      4         3     4     5     6     7     8     9    10    11    12    23    24    25    26    27
      5         4     5     6     7     8     9    10    11    12    13    24    25    26    27    28
      6         5     6     7     8     9    10    11    12    13    14    25    26    27    28    29
      7         6     7     8     9    10    11    12    13    14    15    26    27    28    29    30
      8         7     8     9    10    11    12    13    14    15    16    27    28    29    30    31
      9         8     9    10    11    12    13    14    15    16    17    28    29    30    31    32
     10         9    10    11    12    13    14    15    16    17    18    29    30    31    32    33

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:wlb       Date_Time:Fri Jan  8 13:05:23 2016
 Task:GTMSS     User:wlb       Date_Time:Fri Jan  8 13:05:23 2016
     Samp      16    17    18    19    20    21    22    23    24    25    26    27    28
   Line
      1        25    26    27    40    41    42    43    44    45    46    47    48    49
      2        26    27    28    41    42    43    44    45    46    47    48    49    50
      3        27    28    29    42    43    44    45    46    47    48    49    50    51
      4        28    29    30    43    44    45    46    47    48    49    50    51    52
      5        29    30    31    44    45    46    47    48    49    50    51    52    53
      6        30    31    32    45    46    47    48    49    50    51    52    53    54
      7        31    32    33    46    47    48    49    50    51    52    53    54    55
      8        32    33    34    47    48    49    50    51    52    53    54    55    56
      9        33    34    35    48    49    50    51    52    53    54    55    56    57
     10        34    35    36    49    50    51    52    53    54    55    56    57    58
gtmss (ha.img, hb.img, hc.img) he.img SIZE=(2,3,8,5)
Beginning VICAR task gtmss
gtmss version 2015-09-10
list he.img
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:wlb       Date_Time:Fri Jan  8 13:05:23 2016
 Task:GTMSS     User:wlb       Date_Time:Fri Jan  8 13:05:23 2016
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14    15
   Line
      1         3     4     5     6     7    23    24    25    26    27    43    44    45    46    47
      2         4     5     6     7     8    24    25    26    27    28    44    45    46    47    48
      3         5     6     7     8     9    25    26    27    28    29    45    46    47    48    49
      4         6     7     8     9    10    26    27    28    29    30    46    47    48    49    50
      5         7     8     9    10    11    27    28    29    30    31    47    48    49    50    51
      6         8     9    10    11    12    28    29    30    31    32    48    49    50    51    52
      7         9    10    11    12    13    29    30    31    32    33    49    50    51    52    53
      8        10    11    12    13    14    30    31    32    33    34    50    51    52    53    54
gen ra.img 4 3 'REAL4
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen rb.img 4 2  'REAL4 ival=20
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen rc.img 4 3 'REAL4 ival=40
Beginning VICAR task gen
GEN Version 6
GEN task completed
gtmss (ra.img, rb.img, rc.img) rd.img
Beginning VICAR task gtmss
gtmss version 2015-09-10
list rd.img
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:wlb       Date_Time:Fri Jan  8 13:05:23 2016
 Task:GTMSS     User:wlb       Date_Time:Fri Jan  8 13:05:23 2016
     Samp             1           2           3           4           5           6           7           8
   Line
      1       0.000E+00   1.000E+00   2.000E+00   2.000E+01   2.100E+01   4.000E+01   4.100E+01   4.200E+01
      2       1.000E+00   2.000E+00   3.000E+00   2.100E+01   2.200E+01   4.100E+01   4.200E+01   4.300E+01
      3       2.000E+00   3.000E+00   4.000E+00   2.200E+01   2.300E+01   4.200E+01   4.300E+01   4.400E+01
      4       3.000E+00   4.000E+00   5.000E+00   2.300E+01   2.400E+01   4.300E+01   4.400E+01   4.500E+01
gtmss (ra.img, rb.img, rc.img) re.img SIZE=(2,2,2,1)
Beginning VICAR task gtmss
gtmss version 2015-09-10
list re.img 'REAL4
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:wlb       Date_Time:Fri Jan  8 13:05:23 2016
 Task:GTMSS     User:wlb       Date_Time:Fri Jan  8 13:05:23 2016
     Samp             1           2           3
   Line
      1       2.000E+00   2.200E+01   4.200E+01
      2       3.000E+00   2.300E+01   4.300E+01
gen xxxim1 nl=5 ns=3
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
gtlist version Wed Jan  2 2008
gen xxxim3 nl=5 ns=2
Beginning VICAR task gen
GEN Version 6
GEN task completed
gtgen inp=xxxim3 out=xxxim4 'tiecnvrt  +
   geotiff=("ModelTiePointTag=(-1,3,0,30000.,17000.,0.0)",  +
          "ModelTiePointTag=(199,103,0,31000.,16000.,0.0)",  +
          "ModelTiePointTag=(199,3,0,31000.,17000.,0.0)",  +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
gtlist xxxim4
Beginning VICAR task gtlist
gtlist version Wed Jan  2 2008
gtmss (xxxim2,xxxim4) xxxim5
Beginning VICAR task gtmss
gtmss version 2015-09-10
gtlist xxxim5
Beginning VICAR task gtlist
gtlist version Wed Jan  2 2008
gen xxxim1 nl=5 ns=4
Beginning VICAR task gen
GEN Version 6
GEN task completed
gtgen inp=xxxim1 out=xxxim2 'tiecnvrt  +
   geotiff=("ModelTiePointTag=(2,3,0,500000.,1700000.,0.0)",  +
          "ModelTiePointTag=(202,103,0,501000.,1600000.,0.0)",  +
          "ModelTiePointTag=(202,3,0,501000.,1700000.,0.0)",  +
          "ProjectedCSTypeGeoKey=32637(PCS_WGS84_UTM_zone_37N)",  +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
gtlist xxxim2
Beginning VICAR task gtlist
gtlist version Wed Jan  2 2008
gen xxxim3 nl=5 ns=2
Beginning VICAR task gen
GEN Version 6
GEN task completed
gtgen inp=xxxim3 out=xxxim4 'tiecnvrt  +
   geotiff=("ModelTiePointTag=(-1,3,0,500000.,1700000.,0.0)",  +
          "ModelTiePointTag=(199,103,0,501000.,1600000.,0.0)",  +
          "ModelTiePointTag=(199,3,0,501000.,1700000.,0.0)",  +
          "ProjectedCSTypeGeoKey=32637(PCS_WGS84_UTM_zone_37N)",  +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
gtlist xxxim4
Beginning VICAR task gtlist
gtlist version Wed Jan  2 2008
gtmss (xxxim2,xxxim4) xxxim5 'overlap1
Beginning VICAR task gtmss
gtmss version 2015-09-10
gtlist xxxim5
Beginning VICAR task gtlist
gtlist version Wed Jan  2 2008
list xxxim2
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Fri Jan  8 13:05:23 2016
 Task:GTGEN     User:wlb       Date_Time:Fri Jan  8 13:05:23 2016
     Samp     1       3
   Line
      1       0   1   2   3
      2       1   2   3   4
      3       2   3   4   5
      4       3   4   5   6
      5       4   5   6   7
list xxxim4
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Fri Jan  8 13:05:23 2016
 Task:GTGEN     User:wlb       Date_Time:Fri Jan  8 13:05:23 2016
     Samp     1
   Line
      1       0   1
      2       1   2
      3       2   3
      4       3   4
      5       4   5
list xxxim5
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Fri Jan  8 13:05:23 2016
 Task:GTMSS     User:wlb       Date_Time:Fri Jan  8 13:05:23 2016
     Samp     1       3       5
   Line
      1       0   1   2   0   1
      2       1   2   3   1   2
      3       2   3   4   2   3
      4       3   4   5   3   4
      5       4   5   6   4   5
gen xxxim1 nl=5 ns=3
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
gtlist version Wed Jan  2 2008
gen xxxim3 nl=5 ns=2
Beginning VICAR task gen
GEN Version 6
GEN task completed
gtmss (xxxim2,xxxim3) xxxim4 'nomapck
Beginning VICAR task gtmss
gtmss version 2015-09-10
gtlist xxxim4
Beginning VICAR task gtlist
gtlist version Wed Jan  2 2008
disable-log
$ Return
$!#############################################################################
