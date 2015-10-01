$!****************************************************************************
$!
$! Build proc for MIPL module gtgen
$! VPACK Version 1.9, Thursday, January 26, 2012, 13:57:30
$!
$! Execute by entering:		$ @gtgen
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
$ write sys$output "*** module gtgen ***"
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
$ write sys$output "Invalid argument given to gtgen.com file -- ", primary
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
$   if F$SEARCH("gtgen.imake") .nes. ""
$   then
$      vimake gtgen
$      purge gtgen.bld
$   else
$      if F$SEARCH("gtgen.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake gtgen
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @gtgen.bld "STD"
$   else
$      @gtgen.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create gtgen.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack gtgen.com -mixed -
	-s gtgen.c -
	-i gtgen.imake -
	-p gtgen.pdf -
	-t tstgtgen.pdf devgtgen.pdf tstgtgen.log_solos
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create gtgen.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "vicmain_c.h"
#include "applic.h"
#include <math.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#include "defines.h"
#include "cartoGtUtils.h"
#include "cartoMemUtils.h"
#include "cartoStrUtils.h"
#include "cartoLsqUtils.h"
/*#include "cartoVicarProtos.h"*/

/*  GeoTIFF file creation routine   A. Zobrist    8/4/99   */
/*  Fri Dec 28 2007 wlb switched to USES_ANSI_C AND LIB_CARTO; misc cleanup */

int strncmp_p_ins(p,q,n)
   char *p,*q; int n;
{
   char *ptemp; int i;
   
   if ((ptemp=(char *)malloc(n))==NULL) zmabend("malloc failed");
   for (i=0;i<n;i++) ptemp[i] = toupper(p[i]);
   return strncmp(ptemp,q,n);
}

void main44(void)
{
   int i,len,add,addtie,tiecnvrt,gtcount,gtdef,inpcnt;
   int tiecount,ier,firstpoint,scaletype=0,twofilecase;
   int labnl,labns,i_unit,o_unit,status,nline,nsamp;
   int infilecase,pixsiz,rectfit,outcnt;
   char *labelstr,gtparms[40][200],buf[300];
   char *p,*imbuf;
   double img1[9],img2[9],map[6],ddummy,xmain,xcross,xtot;
   
   /* initialize, fetch params, two input files a special case */

   zifmessage("gtgen version Fri Jan 11 2008");
   
   status = zvpcnt("inp",&inpcnt);
   status = zvpcnt("out",&outcnt);
   if (inpcnt>=1)
      {
      status = zvunit( &i_unit, "INP",1,NULL);
      status = zvopen( i_unit,"OPEN_ACT","SA","IO_ACT","SA",NULL);
      zvget(i_unit,"NL",&nline,"NS",&nsamp,"PIX_SIZE",&pixsiz,NULL);
      infilecase = 1;
      }
   else 
      {
      nline = 1;
      nsamp = 1;
      pixsiz = 1;
      infilecase = 0;
      }
   if (inpcnt==2)
      {
      status = gtgetlab("inp",2,&labelstr,&labnl,&labns);
      if (status!=1) zmabend("second input file problem");
      len = strlen(labelstr);
      for (i=0;i<=len;i++) labelstr[i] = toupper(labelstr[i]);
      if (len<2) zmabend("There was no GeoTIFF label in second data input");
      twofilecase = 1;
      }
   else twofilecase = 0;
   
   add = zvptst("ADD");
   tiecnvrt = zvptst("TIECNVRT");
   addtie = add&&tiecnvrt;
   rectfit = zvptst("RECTFIT");
   if (add&&twofilecase)
      zmabend("No parameters allowed for two input file case");
   if (add&&!infilecase)
      zmabend("Add not allowed for no input file case");
   if (tiecnvrt&&twofilecase)
      zmabend("No parameters allowed for two input file case");
   if (add) printf("add mode on\n"); else printf("add mode off\n");
   if (tiecnvrt) printf("tiecnvrt mode on\n"); else printf("tiecnvrt mode off\n");
   zvparm("geotiff",gtparms,&gtcount,&gtdef,40,200);
   if (gtcount>0&&twofilecase)
      zmabend("No parameters allowed for two input file case");
   if (twofilecase) goto skipgtparms;
   
   mz_alloc1((unsigned char **)&labelstr,4000,1);
   strcpy(labelstr,"");
   if (tiecnvrt)
      {
      tiecount = 0;
      for (i=0;i<gtcount;i++)
         if (strncmp_p_ins(gtparms[i],"MODELTIEPOINTTAG",16)==0)
            {
            p = &gtparms[i][18];
            img1[tiecount] = ms_dnum(&p); p++;
            img1[tiecount+3] = ms_dnum(&p); p++;
            img1[tiecount+6] = 1.0;
            img2[tiecount] = img1[tiecount];
            img2[tiecount+3] = img1[tiecount+3];
            img2[tiecount+6] = 1.0;
            ddummy = ms_dnum(&p); p++;
            map[tiecount] = ms_dnum(&p); p++;
            map[tiecount+3] = ms_dnum(&p); p++;
            tiecount++;
            if (tiecount==3) break;
            }
      dgauss(img1,map,3,1.e-14,&ier);
      if (ier!=0) zmabend("Tiepoints collinear");
      dgauss(img2,&map[3],3,1.e-14,&ier);
      if (ier!=0) zmabend("Tiepoints collinear");
      xmain = fabs(map[0])+fabs(map[4]);
      xcross = fabs(map[1])+fabs(map[3]);
      xtot = xmain+xcross;
      scaletype = xcross/xtot<1.e-10;
      if (rectfit)
         {
         scaletype = xcross<xtot;
         if (scaletype)
            {
            map[1] = 0.0;
            map[3] = 0.0;
            }
         else
            {
            map[0] = 0.0;
            map[4] = 0.0;
            }
         }
      }
   
   /* logic for linefeeds is:  before any item except first  */
   
   firstpoint = 1;
   for (i=0;i<gtcount;i++)
      {
      len = strlen(gtparms[i]);
      if (!tiecnvrt||!(strncmp_p_ins(gtparms[i],"MODELTIEPOINTTAG",16)==0))
         {
         if (i>0) strcat(labelstr,"\n");
         sprintf(buf,"%s",gtparms[i]);
         strcat(labelstr,buf);
         }
      else
         {
         if (firstpoint)
            {
            if (i>0) strcat(labelstr,"\n");
            if (scaletype)
               {
               sprintf(buf,"%s",gtparms[i]);
               strcat(labelstr,buf);
               strcat(labelstr,"\nModelPixelScaleTag=");
               scalefmt(buf,map[0],-map[4]);
               strcat(labelstr,buf);
               }
            else
               {
               strcat(labelstr,"ModelTransformationTag=");
               trnsfmt(buf,map);
               strcat(labelstr,buf);
               }
            }
         firstpoint = 0;
         }
      }
   
   /* process the output, two cases: creating a new 1 x 1 or copying
   first input image */
   
   skipgtparms:
   if (outcnt==0&&inpcnt==1) goto updatecase;
   status = zvunit(&o_unit,"OUT",1,NULL);
   status=zvopen(o_unit,"U_NL",nline,"U_NS",nsamp,
		"OP","WRITE","OPEN_ACT","SA","IO_ACT","SA",NULL);
   mz_alloc1((unsigned char **)&imbuf,nsamp*pixsiz+16,1);
   
   if (infilecase) for (i=0;i<nline;i++)
      {
      status = zvread(i_unit,imbuf,"LINE",i+1,"SAMP",1,"NSAMPS",nsamp,NULL);
      status = zvwrit(o_unit,imbuf,"LINE",i+1,"SAMP",1,"NSAMPS",nsamp,NULL);
      }
      
   /* now put labelstr in the state label under GeoTIFF, map param is a dummy
      here, is not used */
   
   zvclose(o_unit,NULL);
   gtreplab("OUT",1,labelstr,add+addtie,0,map,"","");
      
   return;
   
   /* update case operates on input */
   
   updatecase:
   zvclose(i_unit,NULL);
   gtreplab("INP",1,labelstr,add+addtie,0,map,"","");
      
   return;
   
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create gtgen.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM gtgen

   To Create the build file give the command:

		$ vimake gtgen			(VMS)
   or
		% vimake gtgen			(Unix)


************************************************************************/


#define PROGRAM	gtgen

#define MODULE_LIST gtgen.c

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
$ create gtgen.pdf
process help=*
 !
 PARM INP         TYPE=STRING COUNT=(0:2) DEFAULT=--
 PARM OUT         TYPE=STRING COUNT=(0:1) DEFAULT=--
 PARM GEOTIFF     TYPE=STRING COUNT=(0:100) DEFAULT=--
 PARM ADD         TYPE=KEYWORD COUNT=(0:1) VALID=ADD DEFAULT=--
 PARM TIECNVRT    TYPE=KEYWORD COUNT=(0:1) VALID=TIECNVRT DEFAULT=--
 PARM RECTFIT     TYPE=KEYWORD COUNT=(0:1) VALID=RECTFIT DEFAULT=--
!
 END-PROC
!
! HELP TEXT FOR GTGEN
.TITLE
GTGEN - Program for creating a VICAR GeoTIFF label file.
.HELP
PURPOSE
     GTGEN is a VICAR applications program which creates a GeoTIFF
     label from parameter input.  This label can be used as a reference
     for numerous VICAR/IBIS operations such as mapping a graphics
     data set or an image.  The label can be examined with the 
     routine GTLIST, including calculated information such as location
     of corner points and pixel size.  The label is coded as a
     part of the state VICAR label.  It can be inserted into an IBIS
     file for graphics or tabular data with routine GTIBIS (not
     available yet).
     
     A special feature of GTGEN is that a three tiepoint set of
     parameters can be converted into a tiepoint-scale or tiepoint-
     transformation-matrix format of GeoTIFF label.  Often, the
     three tiepoints are known or are easier to work with than the
     internal formats of GeoTIFF.  This feature is invoked by the
     keyword TIECNVRT.  If the three tiepoints are not exactly
     rectilinear (which can be proven by using GTLIST on the result)
     and a rectilinear mapping is desired, then the keyword RECTFIT
     can be used to give a best fit (in the least-squares sense)
     that is rectilinear.
     
     If a VICAR file is created with the GEOTIFF parameter but no
     input data sets, then it will be a 1 x 1 image (one pixel)
     that serves mainly as the holder of the GeoTIFF label.  It 
     would also be useful for the 'COVERINP parameter of VICAR
     program GTSIZE or GTWARP.
     
     GTGEN has six modes of operation, so read the help for 
     parameter INP to determine your case.
     
CALL
     gtgen INP OUT '(QUALIFIERS) GEOTIFF
  WHERE:
     INP            is the input data set (may be none, one, or two).
     OUT            is the output data set (may be none, or one).
     QUALIFIERS     consist of any of the following keywords:
          ADD           add the geotiff parameters to an existing label
          TIECNVRT      convert three tiepoint input to tiepoint-scale-
                        model format to indicate that image is mapped
     GEOTIFF        are tag-value pairs that become geotiff parms in
                    the label; each is formatted tag=value.
OR
     gtgen (INP1,INP2) OUT
  WHERE:
     INP1           is a VICAR input data set(s).
     INP2           is a VICAR input data set that has a GeoTIFF
                    label in its state label area.
     OUT            is a VICAR output data set that will be a copy
                    of the first data set with an inserted GeoTIFF
                    label from the second data set.
     
    
OPERATION

For the normal case of creating a label from parameters,
GTGEN processes the input string pairs, turning each into a
GeoTIFF tag-value pair.

The parameter uses only the string format.  Numbers are presented as
strings.  Vectors are presented as a string containing symbols (,,).
etc.  Double quotes must be doubled inside the TAE double quotes
(see example below).  Parenthetical documentation can be added as
part of the strings.

The format is flattened out for VICAR label purposes.  The higher
level descriptors such as GeoKeyDirectoryTag are not used in the
vicar table.  They are restored when an external GeoTIFF label is
created.   The following example is taken from 3.1.1 of the 
GeoTIFF Format Specification REv 1 (see references).

geotiff=("ModelTiePointTag=(0,0,0,350807.4,5316081.3,0.0)",+
"ModelPixelScaleTag=(100.0,100.0,0.0)",+
"GTModelTypeGeoKey=1(ModelTypeProjected)",+
"GTRasterTypeGeoKey=1(RasterPixelIsArea)",+
"ProjectedCSTypeGeoKey=32660(PCS_WGS84-UTM-zone_60N)",+
"PCSCitationGeoKey=""UTM Zone 60 N with WGS84""")

For the case of two input files, no parameters are allowed.  The 
program simply places the ascii file key=value pairs into the
VICAR state area.  Multiple occurrences of keys become vectors
in the VICAR state area (since repeats of keys are disallowed).

For the update case of one input, no output data sets... the
geotiff parameter is processed in update mode into the primary
input.  This is the fastest way to process large data sets.

PERFORMANCE

Less than 1 second, except for the case where an output file is
written.  Then the time is mostly copying the output data set.

.PAGE
RESTRICTIONS
------------

Only 100 parameter pairs can be input.  For more, use repeated
calls to GTGEN with the 'ADD parameter after the first call. 

REFERENCES

     Ritter, N., Ruth, M. "GeoTIFF Format Specification, Revision 1.0",
     JPL Cartographic Applications Group.
     
.PAGE
Original Programmer: A. L. Zobrist, 4 Aug. 1999
Current Cognizant Programmer: B. A. McGuffie
Revisions:
  Fri Fri Jan 11 2008 wlb switched to USES_ANSI_C AND LIB_CARTO; misc cleanup

.LEVEL1
.VARI INP
Input file name(s)
There are six cases:
1.  1 output, no add
2.  1 input, 1 output, no add
3.  1 input, 1 output, add
4.  2 input, 1 output
5.  1 input, 0 output (update)
6.  1 input, 0 output (update), add
(see help level 2)
.VARI OUT
output file name
.VARI GEOTIFF
Key-value pairs expressed
as one string per pair
.VARI ADD
The key-value pairs are
added to an existing file
.VARI TIECNVRT
Three tiepoints in the 
parameters are converted to
tiepoint-scale-model
.VARI RECTFIT
Used with TIECNVRT; gives the
rectilinear mapping that best
approximates the tiepoints
.LEVEL2
.VARI INP
There are five cases to consider:
1.  single output file, no add.
2.  single input, single output file, no add.
3.  single input, single output file, add.
4.  two input, single output file.
5.  one input, no output (update case)
6.  one input, no output (update case), add

The first case will create a VICAR file from the parameters with
NL=1 NS=1 (i.e., a single pixel).  This is handy as a holder of a
GeoTIFF label.  If a file exists already, it will be erased.

The second case will put the parameters into the state label of a VICAR
image file.  If any state parameters exist, THEY WILL BE ERASED.  This
erasure includes ALL state labels, whether GeoTIFF related or not.

The third case will add the parameters into the state label of a VICAR
image file.  If any state parameters exist, they are kept.  A special
case for TIECNVRT, an existing mapping will be deleted since it doesn't
make sense to have two mappings.

The fourth case will put the GeoTIFF state labels of the second
VICAR input to the state label area of a VICAR image.  If any state
parameters exist, THEY WILL BE ERASED.  This erasure includes ALL
state labels, whether GeoTIFF related or not.

The fifth case will put the parameters into the state label of a VICAR
image file.  If any state parameters exist, they are deleted.  This case
uses update on the vicar label and is the fastest method for large files.

The sixth case will add the parameters into the state label of a VICAR
image file.  If any state parameters exist, they are kept.  This case
uses update on the vicar label and is the fastest method for large files.

.VARI OUT
The output will be a copy of the first input image with a GeoTIFF
label inserted from the parameters or the second input image.

If there is no first input, then the output will be a 1 x 1 image
containing the GeoTIFF label from the parameters.  The GeoTIFF label
will be in the state area of the image.
.VARI GEOTIFF
The key-value pairs are expressed as "key=value".  Double quotes
must be doubled inside the TAE double quotes (see example below).
Parenthetical documentation can be added as part of the strings.

The format is flattened out for VICAR label purposes.  The higher
level descriptors such as GeoKeyDirectoryTag are not used in the
vicar table.  They are restored when an external GeoTIFF label is
created.   The following example is taken from 3.1.1 of the 
GeoTIFF Format Specification REv 1 (see references).

geotiff=("ModelTiePointTag=(0,0,0,350807.4,5316081.3,0.0)",+
"ModelPixelScaleTag=(100.0,100.0,0.0)",+
"GTModelTypeGeoKey=1(ModelTypeProjected)",+
"GTRasterTypeGeoKey=1(RasterPixelIsArea)",+
"ProjectedCSTypeGeoKey=32660(PCS_WGS84-UTM-zone_60N)",+
"PCSCitationGeoKey=""UTM Zone 60 N with WGS84""")

.VARI ADD
Add can only be used in the single input cases.
.VARI TIECNVRT
The GeoTIFF standard implies that a mapped image can be represented
by a single tiepoint and a scale.  Actually, the scale can include
negative values to indicate that the image storage is "flipped" from
the VICAR order (starting at the top left of the image and running
along the top in sample order, then down in line order), or a matrix
indicating that the image is rotated in addition.  These 
representations are very hard for the user to set up, so this
keyword will convert a simpler set of three tiepoints into a
tiepoint-scale-model format, using the model matrix only if needed
(i.e., the simplest format is decided automatically by GTGEN).

Only the first tiepoint is kept, and only the first three are used
to calculate.  TIECNVRT cannot be used with the ADD keyword.

As an example, a DMA DTED can be input (without disturbing its
lines and samples format) by the following tiepoints:

        "ModelTiePointTag=(0,0,0,121.0,25.0,0.0)"
        "ModelTiePointTag=(1200,0,0,121.0,26.0,0.0)"
        "ModelTiePointTag=(0,1200,0,122.0,25.0,0.0)"

DMA DTED also uses RasterPixelIsPoint, and has the rotation diagram

        369
        258
        147
        
where 123 is the first line, 456 is the second line, etc.
.VARI RECTFIT
The cross terms are simply set to zero.  That is, 

       north   =   a * line  +  b * sample  + c
       east    =   d * line  +  e * sample  + f

either (a,c) is set to (0,0) or (b,d) is.  For a rectilinear
image, one of these pairs should be zero, but approximations
or rounding in the tiepoints cause one of the pairs to be small 
numbers close to zero.  Which pair is a function of the rotation
of the image.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstgtgen.pdf
procedure
refgbl $echo
parm version string def="ibis-1"
parm org string def="column"
body
!let _onfail="continue"
let $echo="yes"


!   TEST SCRIPT FOR GTGEN

gtgen out=xxxlab1 'tiecnvrt +
   geotiff=("ModelTiePointTag=(2,3,0,350807.4,5317081.3,0.0)", +
          "ModelTiePointTag=(202,103,0,351807.4,5316081.3,0.0)", +
          "ModelTiePointTag=(2,103,0,350807.4,5316081.3,0.0)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
   
list xxxlab1 'zeroes
gtlist xxxlab1


! vicar image case

gen xxxim1 nl=500 ns=500

gtgen inp=xxxim1 out=xxxim2 'tiecnvrt +
   geotiff=("ModelTiePointTag=(0,0,0,350807.4,5317081.3,0.0)", +
          "ModelTiePointTag=(200,100,0,351807.4,5316081.3,0.0)", +
          "ModelTiePointTag=(200,0,0,351807.4,5317081.3,0.0)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")

label-list xxxim2
gtlist xxxim2

! add case

gtgen inp=xxxim2 out=xxxim3 'add +
   geotiff=("PCSCitationGeoKey=""UTM Zone 60 N with WGS84""")

label-list xxxim3
gtlist xxxim3

! label transfer case

gen xxxim4 nl=40 ns=40

gtgen inp=(xxxim4,xxxim3) out=xxxim5

label-list xxxim5
gtlist xxxim5

! testing the 'rectfit keyword, first a non-rect case

gtgen out=xxxlab1 'tiecnvrt +
   geotiff=("ModelTiePointTag=(2,3,0,350807.4,5317081.3,0.0)", +
          "ModelTiePointTag=(202,103,0,351807.4,5316081.3,0.0)", +
          "ModelTiePointTag=(2,103,0,350807.4,5316081.2,0.0)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
   
gtlist xxxlab1

! testing the 'rectfit keyword, now repeat with the keyword

gtgen out=xxxlab1 'tiecnvrt 'rectfit +
   geotiff=("ModelTiePointTag=(2,3,0,350807.4,5317081.3,0.0)", +
          "ModelTiePointTag=(202,103,0,351807.4,5316081.3,0.0)", +
          "ModelTiePointTag=(2,103,0,350807.4,5316081.2,0.0)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
   
gtlist xxxlab1

! exponents in tiepoints coordinates

gtgen out=xxxlab2 'tiecnvrt +
   geotiff=("ModelTiePointTag=(2,3,0,35080.74e+01,53170813D-01,0.0)", +
          "ModelTiePointTag=(202,103,0,351807.4,5316081.3,0.0)", +
          "ModelTiePointTag=(2,103,0,350807.4,5316081.3,0.0)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
   
gtlist xxxlab2


! test update case, this will save time for world landsat mosaic

gen xxxlab3 nl=1 ns=1

gtgen in=xxxlab3 'tiecnvrt +
   geotiff=("ModelTiePointTag=(2,3,0,35080.74e+01,53170813D-01,0.0)", +
          "ModelTiePointTag=(202,103,0,351807.4,5316081.3,0.0)", +
          "ModelTiePointTag=(2,103,0,350807.4,5316081.3,0.0)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
gtlist xxxlab3

! add case for update

gtgen inp=xxxlab3 'add +
   geotiff=("PCSCitationGeoKey=""UTM Zone 60 N with WGS84""")
gtlist xxxlab3

! replace case for update

gtgen in=xxxlab3 'tiecnvrt +
   geotiff=("ModelTiePointTag=(12,3,0,35080.74e+01,53170813D-01,0.0)", +
          "ModelTiePointTag=(212,103,0,351807.4,5316081.3,0.0)", +
          "ModelTiePointTag=(12,103,0,350807.4,5316081.3,0.0)", +
          "GTRasterTypeGeoKey=2(RasterPixelIsPoint)")
gtlist xxxlab3


! add case, special case of adding tiecnvrt-tiepoints, the old
! mapping should be deleted

gen xxxim1 nl=5 ns=5

gtgen inp=xxxim1 out=xxxim2 'tiecnvrt +
   geotiff=("ModelTiePointTag=(0,0,0,-110.,34.,0.0)", +
          "ModelTiePointTag=(0,100,0,-110.,33.,0.0)", +
          "ModelTiePointTag=(100,0,0,-109.,34.,0.0)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")

label-list xxxim2

! continuing the add case, special case of adding tiecnvrt-tiepoints,
! the old mapping should be deleted

gtgen inp=xxxim2 out=xxxim3 'add 'tiecnvrt +
   geotiff=("ModelTiePointTag=(0,0,0,-110.,34.,0.0)", +
          "ModelTiePointTag=(0,50,0,-110.,33.,0.0)", +
          "ModelTiePointTag=(50,0,0,-109.,34.,0.0)")

label-list xxxim3
end-proc
$!-----------------------------------------------------------------------------
$ create devgtgen.pdf
procedure
refgbl $echo
refgbl $autousage
parm mean real def=0.0
parm sdev real def=1.0
parm seed real def=9.0
body
let $autousage="none"
!let _onfail="continue"
let $echo="yes"

goto curr

! add case, special case of adding tiecnvrt-tiepoints, the old
! mapping should be deleted

gen xxxim1 nl=5 ns=5

gtgen inp=xxxim1 out=xxxim2 'tiecnvrt +
   geotiff=("ModelTiePointTag=(0,0,0,-110.,34.,0.0)", +
          "ModelTiePointTag=(0,100,0,-110.,33.,0.0)", +
          "ModelTiePointTag=(100,0,0,-109.,34.,0.0)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")

label-list xxxim2

! continuing the add case, special case of adding tiecnvrt-tiepoints,
! the old mapping should be deleted

gtgen inp=xxxim2 out=xxxim3 'add 'tiecnvrt +
   geotiff=("ModelTiePointTag=(0,0,0,-110.,34.,0.0)", +
          "ModelTiePointTag=(0,50,0,-110.,33.,0.0)", +
          "ModelTiePointTag=(50,0,0,-109.,34.,0.0)")

label-list xxxim3


! testing vtiff3 conversions, include Bellenger kwds, note that
! both units keywords are in conflict
curr>

gen xxxim1 nl=5 ns=5

gtgen inp=xxxim1 out=xxxim2 'tiecnvrt +
   geotiff=("ModelTiePointTag=(0,0,0,-110.,34.,0.0)", +
          "ModelTiePointTag=(0,100,0,-110.,33.,0.0)", +
          "ModelTiePointTag=(100,0,0,-109.,34.,0.0)", +
          "GeographicTypeGeoKey=4326(GCS_WGS_84)", +
          "GTModelTypeGeoKey=2(ModelTypeGeographic)", +
          "ProjLinearUnitsGeoKey=9001(Linear_Meter)", +
          "GeogAngularUnitsGeoKey=9102(Angular_Degree)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")

gtlist xxxim2
vtiff3-fromvic xxxim2 xxxim3
! how to see
vtiff3-tovic xxxim3 xxxim4
gtlist xxxim4


END-PROC
$!-----------------------------------------------------------------------------
$ create tstgtgen.log_solos
tstgtgen
gtgen out=xxxlab1 'tiecnvrt  +
   geotiff=("ModelTiePointTag=(2,3,0,350807.4,5317081.3,0.0)",  +
          "ModelTiePointTag=(202,103,0,351807.4,5316081.3,0.0)",  +
          "ModelTiePointTag=(2,103,0,350807.4,5316081.3,0.0)",  +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
list xxxlab1 'zeroes
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GTGEN     User:lwk       Date_Time:Tue Jan 24 14:30:41 2012
     Samp     1
   Line
      1       0
gtlist xxxlab1
Beginning VICAR task gtlist
gtlist version Wed Jan  2 2008
gen xxxim1 nl=500 ns=500
Beginning VICAR task gen
GEN Version 6
GEN task completed
gtgen inp=xxxim1 out=xxxim2 'tiecnvrt  +
   geotiff=("ModelTiePointTag=(0,0,0,350807.4,5317081.3,0.0)",  +
          "ModelTiePointTag=(200,100,0,351807.4,5316081.3,0.0)",  +
          "ModelTiePointTag=(200,0,0,351807.4,5317081.3,0.0)",  +
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
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                500 lines per band
                500 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Property: GEOTIFF ----
MODELTIEPOINTTAG='(0,0,0,350807.4,5317081.3,0.0)'
MODELPIXELSCALETAG='(5.0,10.0,0.0)'
GTRASTERTYPEGEOKEY='1(RasterPixelIsArea)'
---- Task: GEN -- User: lwk -- Tue Jan 24 14:30:42 2012 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: GTGEN -- User: lwk -- Tue Jan 24 14:30:42 2012 ----
 
************************************************************
gtlist xxxim2
Beginning VICAR task gtlist
gtlist version Wed Jan  2 2008
gtgen inp=xxxim2 out=xxxim3 'add  +
   geotiff=("PCSCitationGeoKey=""UTM Zone 60 N with WGS84""")
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
label-list xxxim3
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File xxxim3 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                500 lines per band
                500 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Property: GEOTIFF ----
MODELTIEPOINTTAG='(0,0,0,350807.4,5317081.3,0.0)'
MODELPIXELSCALETAG='(5.0,10.0,0.0)'
GTRASTERTYPEGEOKEY='1(RasterPixelIsArea)'
PCSCITATIONGEOKEY='"UTM Zone 60 N with WGS84"'
---- Task: GEN -- User: lwk -- Tue Jan 24 14:30:42 2012 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: GTGEN -- User: lwk -- Tue Jan 24 14:30:42 2012 ----
---- Task: GTGEN -- User: lwk -- Tue Jan 24 14:30:42 2012 ----
 
************************************************************
gtlist xxxim3
Beginning VICAR task gtlist
gtlist version Wed Jan  2 2008
gen xxxim4 nl=40 ns=40
Beginning VICAR task gen
GEN Version 6
GEN task completed
gtgen inp=(xxxim4,xxxim3) out=xxxim5
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
label-list xxxim5
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File xxxim5 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                40 lines per band
                40 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Property: GEOTIFF ----
MODELTIEPOINTTAG='(0,0,0,350807.4,5317081.3,0.0)'
MODELPIXELSCALETAG='(5.0,10.0,0.0)'
GTRASTERTYPEGEOKEY='1(RASTERPIXELISAREA)'
PCSCITATIONGEOKEY='"UTM ZONE 60 N WITH WGS84"'
---- Task: GEN -- User: lwk -- Tue Jan 24 14:30:42 2012 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: GTGEN -- User: lwk -- Tue Jan 24 14:30:42 2012 ----
 
************************************************************
gtlist xxxim5
Beginning VICAR task gtlist
gtlist version Wed Jan  2 2008
gtgen out=xxxlab1 'tiecnvrt  +
   geotiff=("ModelTiePointTag=(2,3,0,350807.4,5317081.3,0.0)",  +
          "ModelTiePointTag=(202,103,0,351807.4,5316081.3,0.0)",  +
          "ModelTiePointTag=(2,103,0,350807.4,5316081.2,0.0)",  +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
gtlist xxxlab1
Beginning VICAR task gtlist
gtlist version Wed Jan  2 2008
gtgen out=xxxlab1 'tiecnvrt 'rectfit  +
   geotiff=("ModelTiePointTag=(2,3,0,350807.4,5317081.3,0.0)",  +
          "ModelTiePointTag=(202,103,0,351807.4,5316081.3,0.0)",  +
          "ModelTiePointTag=(2,103,0,350807.4,5316081.2,0.0)",  +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
gtlist xxxlab1
Beginning VICAR task gtlist
gtlist version Wed Jan  2 2008
gtgen out=xxxlab2 'tiecnvrt  +
   geotiff=("ModelTiePointTag=(2,3,0,35080.74e+01,53170813D-01,0.0)",  +
          "ModelTiePointTag=(202,103,0,351807.4,5316081.3,0.0)",  +
          "ModelTiePointTag=(2,103,0,350807.4,5316081.3,0.0)",  +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
gtlist xxxlab2
Beginning VICAR task gtlist
gtlist version Wed Jan  2 2008
gen xxxlab3 nl=1 ns=1
Beginning VICAR task gen
GEN Version 6
GEN task completed
gtgen in=xxxlab3 'tiecnvrt  +
   geotiff=("ModelTiePointTag=(2,3,0,35080.74e+01,53170813D-01,0.0)",  +
          "ModelTiePointTag=(202,103,0,351807.4,5316081.3,0.0)",  +
          "ModelTiePointTag=(2,103,0,350807.4,5316081.3,0.0)",  +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
gtlist xxxlab3
Beginning VICAR task gtlist
gtlist version Wed Jan  2 2008
gtgen inp=xxxlab3 'add  +
   geotiff=("PCSCitationGeoKey=""UTM Zone 60 N with WGS84""")
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
gtlist xxxlab3
Beginning VICAR task gtlist
gtlist version Wed Jan  2 2008
gtgen in=xxxlab3 'tiecnvrt  +
   geotiff=("ModelTiePointTag=(12,3,0,35080.74e+01,53170813D-01,0.0)",  +
          "ModelTiePointTag=(212,103,0,351807.4,5316081.3,0.0)",  +
          "ModelTiePointTag=(12,103,0,350807.4,5316081.3,0.0)",  +
          "GTRasterTypeGeoKey=2(RasterPixelIsPoint)")
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
gtlist xxxlab3
Beginning VICAR task gtlist
gtlist version Wed Jan  2 2008
gen xxxim1 nl=5 ns=5
Beginning VICAR task gen
GEN Version 6
GEN task completed
gtgen inp=xxxim1 out=xxxim2 'tiecnvrt  +
   geotiff=("ModelTiePointTag=(0,0,0,-110.,34.,0.0)",  +
          "ModelTiePointTag=(0,100,0,-110.,33.,0.0)",  +
          "ModelTiePointTag=(100,0,0,-109.,34.,0.0)",  +
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
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                5 lines per band
                5 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Property: GEOTIFF ----
MODELTIEPOINTTAG='(0,0,0,-110.,34.,0.0)'
MODELPIXELSCALETAG='(0.01,0.01,0.0)'
GTRASTERTYPEGEOKEY='1(RasterPixelIsArea)'
---- Task: GEN -- User: lwk -- Tue Jan 24 14:30:43 2012 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: GTGEN -- User: lwk -- Tue Jan 24 14:30:43 2012 ----
 
************************************************************
gtgen inp=xxxim2 out=xxxim3 'add 'tiecnvrt  +
   geotiff=("ModelTiePointTag=(0,0,0,-110.,34.,0.0)",  +
          "ModelTiePointTag=(0,50,0,-110.,33.,0.0)",  +
          "ModelTiePointTag=(50,0,0,-109.,34.,0.0)")
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
label-list xxxim3
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File xxxim3 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                5 lines per band
                5 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Property: GEOTIFF ----
GTRASTERTYPEGEOKEY='1(RasterPixelIsArea)'
MODELTIEPOINTTAG='(0,0,0,-110.,34.,0.0)'
MODELPIXELSCALETAG='(0.02,0.02,0.0)'
---- Task: GEN -- User: lwk -- Tue Jan 24 14:30:43 2012 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: GTGEN -- User: lwk -- Tue Jan 24 14:30:43 2012 ----
---- Task: GTGEN -- User: lwk -- Tue Jan 24 14:30:44 2012 ----
 
************************************************************
end-proc
disable-log
$ Return
$!#############################################################################
