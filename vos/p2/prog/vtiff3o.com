$!****************************************************************************
$!
$! Build proc for MIPL module vtiff3o
$! VPACK Version 2.1, Wednesday, October 14, 2015, 18:17:09
$!
$! Execute by entering:		$ @vtiff3o
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
$ write sys$output "*** module vtiff3o ***"
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
$ write sys$output "Invalid argument given to vtiff3o.com file -- ", primary
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
$   if F$SEARCH("vtiff3o.imake") .nes. ""
$   then
$      vimake vtiff3o
$      purge vtiff3o.bld
$   else
$      if F$SEARCH("vtiff3o.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake vtiff3o
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @vtiff3o.bld "STD"
$   else
$      @vtiff3o.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create vtiff3o.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack vtiff3o.com -mixed -
	-s vtiff3o.c optimal_color2.c optimal_color2.h uFixedNums.c -
	   uFixedNums.h -
	-p vtiff3o.pdf -
	-t tstvtiff3o.pdf tstvtiff3o.log -
	-i vtiff3o.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create vtiff3o.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/**
 **  Source code for:      vtiff3.c
 **
 **    Uses:
 **      SGI's libtiff package  (ftp://ftp.sgi.com/graphics/tiff)
 **      ZLIB package           (ftp://ftp.uu.net/pub/archiving/zip/zlib/)
 **      LIBGEOTIFF package     (ftp://mtritter.jpl.nasa.gov/pub/tiff/geotiff/code/)
 **
 **    And:
 **      VICAR runtime library.
 **/

#include <ctype.h>
#include "vicmain_c"
#include "defines.h"
#include <stdio.h>

#include <stdlib.h>
#include <math.h>
#include "geotiffio.h"
#include "geo_tiffp.h"
#include "geo_keyp.h"

#include "xtiffio.h"
#include "ibisfile.h"

#include <time.h>
#include <string.h>
#include "uFixedNums.h"
#include "optimal_color2.h"

#include "cartoMemUtils.h"
#include "cartoStrUtils.h"

#ifndef MIN
#define MIN(x,y) ((x) < (y) ? (x) : (y))
#endif

#define TIFFOpen XTIFFOpen
#define TIFFClose XTIFFClose

/* I defined OLD_COMPRESSION_CODE to enable LZW compression.  pk */
#define OLD_COMPRESSION_CODE 1

static tsize_t TIFFReadTile2(
			     TIFF*,tdata_t, uint32, uint32, uint32, tsample_t  );
static tsize_t TIFFWriteTile2( 
			      TIFF*,tdata_t, uint32, uint32, uint32, tsample_t );
static int TIFFWriteScanline2(  TIFF*,tdata_t, uint32, tsample_t );
static int TIFFReadScanline2( TIFF*,tdata_t, uint32,  tsample_t );
static void SetNULL(TIFF *out);
static void StartStatus(int maxval);
static void UpdateStatus(int current);

void vic_to_tiff();
void tiff_to_vic();
void WriteTIFFClut(TIFF *tif);
void WriteFileFromTIFF(char *parm, int instance, TIFF *tif);
void SetUpVicarFile(char *parm, int instance, TIFF *in, int * outunit, int *nl, int *ns, int*nb, int*numout);
int WriteVicarData(int * outunit, TIFF *in, int nl,int ns, int nb);
void AddTIFFGeoreference(TIFF *out);
void AddFiletoTIFF(char *parm, TIFF *tif, char *mode);
void VicAddGeoTIFF(TIFF *out, GTIF *gtif);
void AddGeoTIFF(TIFF *out, GTIF *gtif);
void SetUpTiffDirectory(int * inunit, TIFF *out, char *mode, int *nl, int *ns, int nb);
int ConvertToLut(int * unit);
int WriteTIFFData(int * inunit, TIFF *out, int nl, int ns, int nb);
void SetTIFFClut(int * unit, unsigned short clut[3][256], char *type);
static void UpdateStatus(int current);

static unsigned int sleeptime=1;
static int printstat=0;
static int numinp=0;

#undef sleep
#define sleep(x) time(NULL)

/* routines for vicar geotiff label reading. al zobrist 1/3/01 */

int gtlabnl,gtlabns,gtlabelflag;
char *gtlabelstr;

/*================================================================

int gtgetlab

gtgetlab gets a geotiff label into a string parameter.  It
mallocs a large buffer, reads the geotiff label, then mallocs
the string parameter to the exact size, copies the label, then
frees the large buffer.  A null string is returned for any
failure to read a geotiff label.  The user will usually change
to all caps for speedier key identification.

function return:
     int, 1 if successful, 0 if cannot find info in label

arguments:
      1. inp: char buf[];
	 VICAR parameter for file that contains GeoTIFF label
	 usually "inp"
      2. instance: int instance;
         which instance of the previous parm
      3. labelstr: char **labelstr;
	 (output) pointer to string containing the label; is
	 mallocked to the exact size of the string, plus
	 terminating 0. user will usually change to all caps.
      4. nl: int *nl;
	 (output) nl for case of VICAR image, -1 if not
      5. ns: int *ns;
	 (output) ns for case of VICAR image, -1 if not
*/

int gtgetlab(inp,instance,labelstr,nl,ns)
     char inp[];
     int instance,*nl,*ns;
     char **labelstr;
{
  int i,status,geounit;
  int maxlen,nelement,len;
  char *buf,valformat[9],vformat[9];
  char svalue[133],key[33];
  int sawPropertyLabel = 0;
   
  /* malloc large temporary buffer for reading the string */
   
  mz_alloc1((unsigned char **)&buf,1000001,1);
   
  /* open file */
   
  status = zvunit(&geounit,inp,instance, NULL);
  status = zvopen(geounit,"OP","READ","OPEN_ACT","SA",
		  "LAB_ACT","SA", NULL);
      
  strcpy(buf,"");
  do
    {
      status=zlninfo(geounit,key,valformat,&maxlen,
		     &nelement,"ERR_ACT"," ", NULL);
      if (status!=1) break;
      if (strcmp(key,"NL")==0)
	{
	  status=zlget(geounit,"SYSTEM",key,(char*)nl,
		       "ERR_ACT","SA","FORMAT","INT", NULL);
	}
      if (strcmp(key,"NS")==0)
	{
	  status=zlget(geounit,"SYSTEM",key,(char*)ns,
		       "ERR_ACT","SA","FORMAT","INT", NULL);
	}
      status=zlinfo(geounit,"PROPERTY",key,vformat,
		    &maxlen,&nelement,"ERR_ACT"," ",
		    "PROPERTY","GEOTIFF", NULL);
      if (status!=1) continue;
      if (strcmp(key,"PROPERTY")==0 && sawPropertyLabel)
	break;
      else
	sawPropertyLabel = 1;
      if (strcmp(key,"PROPERTY")==0) continue;
      /* now concatenate the string values / can be vector */
      
      for (i=1;i<=nelement;i++)
	{
	  if (nelement==1)
            status=zlget(geounit,"PROPERTY",key,svalue,
			 "ERR_ACT","SA","FORMAT","STRING","NELEMENT",1,
			 "PROPERTY","GEOTIFF","ULEN",133, NULL);
	  else
            status=zlget(geounit,"PROPERTY",key,svalue,"ELEMENT",i,
			 "ERR_ACT","SA","FORMAT","STRING","NELEMENT",1,
			 "PROPERTY","GEOTIFF","ULEN",133, NULL);
	  strcat(buf,key);
	  strcat(buf,"=");
	  strcat(buf,svalue);
	  strcat(buf,"\n");
	}
    }
  while (1);
  status = zvclose(geounit, NULL);
   
  /* resave in smaller buffer */
   
  len = strlen(buf);
  if (((*labelstr)=(char *)malloc(len+1))==NULL) zmabend("malloc failed");
  strcpy(*labelstr,buf);
   
  free(buf);
  if (strlen(*labelstr)<1) return 0; else return 1;
}

void main44()
{   
  int count,def;
  char command[12];
    
  zifmessage("vtiff3o version 2015-09-17");
    
  printstat = zvptst("PRINT");

  zvparm("_SUBCMD",command,&count,&def,0,0);
  switch (toupper(command[0]))
    {
    case 'F' : vic_to_tiff();
      break;
    case 'T' : tiff_to_vic();
      break;
    }
}

void AbortMsg(message)
     char *message;
{
  zvmessage(message,"");
  zabend();
}

void dump_file(TIFF *in)
{
  int count;
  char metadata[200];
  GTIF *gtif=(GTIF*)0; /* GeoKey-level descriptor */

  gtif = GTIFNew(in);
  if (!gtif)
    {
      zvmessage("failed in GTIFNew"," ");
      zabend();
    }

  TIFFPrintDirectory(in,stdout,TIFFPRINT_NONE);

  zvp("METADATA",metadata,&count);
  if (metadata[0] && metadata[0]!=' ')
    {
      FILE *metafile = fopen(metadata,"w");
      if (!metafile)
	{
	  zvmessage("Failed to open GeoTIFF metadata"," ");
	  zabend();
	}
      GTIFPrint(gtif,0,metafile);
      fclose(metafile);
    }

  GTIFFree(gtif);
  TIFFClose(in);
}


/*********************************************************************/
/*********************** tiff_to_vic        **************************/
/*********************************************************************/

static void addGTLabel (int vunit, char * key, char * value) {
  char msgBuf [200];

  if (zladd (vunit, "PROPERTY",
	     key, value,
	     "PROPERTY", "GEOTIFF",
	     "FORMAT", "STRING", NULL) != 1) {
    sprintf (msgBuf, "addGTLabel failed to add a label for key %s, value %s", key, value);
    zmabend (msgBuf);
  }
}

#define FMT_DOUBLE  "%-17.9lg"
#define FMT_SHORT   "%-11hd"
static int PrintKey(GeoKey *key, char **buf)
{
  char *data;
  int keyid = key->gk_key;
  int count = key->gk_count;
  int vals_now,i;
  pinfo_t *sptr;
  double *dptr;

  if (key->gk_type==TYPE_SHORT && count==1)
    data = (char *)&key->gk_data;
  else
    data = key->gk_data;
		
  switch (key->gk_type)
    {
    case TYPE_ASCII: 
      *buf = malloc (count + 50);
      strncpy (*buf, data, count);
      (*buf) [count-1] = '\0';	/* the last character is a '|', representing a null */
      if (strlen (*buf) > 75)
	(*buf)[76] = '\0';	/* zlinfo, zlget don't like long strings */
      sprintf (*buf + strlen(*buf), ";gk_key=%d", keyid);
      break;
    case TYPE_DOUBLE: 
      *buf = malloc (40 * count + 40);
      **buf = '\0';


      for (dptr = (double *)data; count > 0; count-= vals_now)
	{
	  vals_now = count > 3? 3: count;
	  for (i=0; i<vals_now; i++,dptr++)
	    {
	      if (**buf == '\0')
		sprintf (*buf, "(%.15lg", *dptr);
	      else
		sprintf (*buf + strlen (*buf), ", %.15lg", *dptr);
	    }
	  sprintf (*buf, ");gk_type=%s;gk_key=%d", GTIFTypeName(key->gk_type), keyid);
	}
      break;
    case TYPE_SHORT: 
      sptr = (pinfo_t *)data;
      if (count==1)
	{
	  *buf = malloc(100);

	  sprintf (*buf,"%d(%s);gk_type=%s;gk_key=%d", *sptr, GTIFValueName(keyid,*sptr), GTIFTypeName(key->gk_type), keyid);
	}
      else {
	*buf = malloc(15 * count + 50);
	**buf = '\0';

	for (; count > 0; count-= vals_now)
	  {
	    vals_now = count > 3? 3: count;
	    for (i=0; i<vals_now; i++,sptr++)
	      {
		if (**buf == '\0')
		  sprintf (*buf, "(%d", *sptr);
		else
		  sprintf (*buf + strlen (*buf), ", %d", *sptr);
	      }
	    sprintf (*buf, ");gk_type=%s;gk_key=%d", GTIFTypeName(key->gk_type), keyid);
	  }
      }
      break;
    default: 
      *buf = malloc(100);
      sprintf (*buf, "Unknown Type (%d)\n", key->gk_type);
      break;
    }

  return keyid;
}

/* add TIFF geo tags from geotiff to vicar label */
static void PrintTags (TIFF  *in, GTIF *gt, int * outunit, int outCount) {
    double *data;
    int doubleCount;
    tiff_t *tif=gt->gt_tif;
    int outIndex;
    char * imageDescription = 0;

  /* import MODELTIEPOINTTAG */
  if ((gt->gt_methods.get)(tif, GTIFF_TIEPOINTS, &doubleCount, &data ))
    {
      int sixTuple, part;
      int size = 3;		/* () and null */
      char * buf = malloc (3);

      strcpy (buf, "(");

      for (sixTuple = 0; sixTuple < doubleCount / 6; sixTuple ++) {
	char numBuf [40];

	for (part = 0; part < 6; part ++) {
	  sprintf (numBuf, "%.15lg", data [sixTuple * 6 + part]);
	  size += strlen (numBuf) + (size==3?0:1); /* plus one for a comma */
	  buf = realloc (buf, size);
	  if (size > 3 + strlen(numBuf))
	    strcat (buf, ",");
	  strcat (buf, numBuf);
	}
      }

      strcat (buf, ")");

      for (outIndex = 0; outIndex < outCount; outIndex ++)
	addGTLabel (outunit [outIndex], "MODELTIEPOINTTAG", buf);

      free (buf);
    }

  /* import MODELPIXELSCALETAG */
  if ((gt->gt_methods.get)(tif, GTIFF_PIXELSCALE, &doubleCount, &data ))
    {
      int part;
      int size = 3;
      char * buf = malloc (3);
      char numBuf [40];
      strcpy (buf, "(");

      for (part = 0; part < doubleCount; part ++) {
	sprintf (numBuf, "%.15lg", data [part]);
	size += strlen (numBuf) + (size==3?0:1);
	buf = realloc (buf, size);
	if (size > 3 + strlen(numBuf))
	  strcat (buf, ",");
	strcat (buf, numBuf);
      }
      strcat (buf, ")");

      for (outIndex = 0; outIndex < outCount; outIndex ++)
	addGTLabel (outunit [outIndex], "MODELPIXELSCALETAG", buf);

      free (buf);
    }

  /* import MODELTRANSFORMATIONTAG */
  if ((gt->gt_methods.get)(tif, GTIFF_TRANSMATRIX, &doubleCount, &data ))
    {
      int part;
      int size = 3;
      char * buf = malloc (3);
      char numBuf [40];
      strcpy (buf, "(");

      for (part = 0; part < doubleCount; part ++) {
	sprintf (numBuf, "%.15lg", data [part]);
	size += strlen (numBuf) + (size==3?0:1);
	buf = realloc (buf, size);
	if (size > 3 + strlen(numBuf))
	  strcat (buf, ",");
	strcat (buf, numBuf);
      }
      strcat (buf, ")");

      for (outIndex = 0; outIndex < outCount; outIndex ++)
	addGTLabel (outunit [outIndex], "MODELTRANSFORMATIONTAG", buf);

      free (buf);
    }

  /* import TIFFTAG_IMAGEDESCRIPTION */

  if (TIFFGetField (in, TIFFTAG_IMAGEDESCRIPTION, & imageDescription) == 1) {
    int zero;
    long 	gLongitude;
    long 	gLatitude;
    unsigned long	gPixDegLatInt, gPixDegLatFract;
    unsigned long	gPixDegLonInt, gPixDegLonFract;
    unsigned long	gXPixel, gYPixel;
    DoubleFixed df;
    double pixDegLon, pixDegLat;
    char * str;

    str = strstr (imageDescription, "CART_RESOURCE");
    if (str && sscanf (imageDescription, "CART_RESOURCE(%d,%lu,%lu,%lu,%lu,%lu,%lu,%lu,%lu)",
		       & zero, & gLongitude, & gLatitude,  
		       & gPixDegLonInt, & gPixDegLonFract, 
		       & gPixDegLatInt, & gPixDegLatFract,  
		       & gXPixel, & gYPixel) == 9) {

      df.integerPart = gPixDegLonInt;
      df.fractionalPart = gPixDegLonFract;
      ConvertDoubleFixedToDouble (& df, & pixDegLon);

      df.integerPart = gPixDegLatInt;
      df.fractionalPart = gPixDegLatFract;
      ConvertDoubleFixedToDouble (& df, & pixDegLat);

      {
	int part;
	double tiePoint [6];
	int size = 3;
	char * buf = malloc (3);
	char numBuf [40];

	tiePoint [0] = gXPixel;
	tiePoint [1] = gYPixel;
	tiePoint [2] = 0.0;
	tiePoint [3] = gLongitude / 3600000.0 - 180.0;
	tiePoint [4] = gLatitude / 3600000.0 - 90.0;
	tiePoint [5] = 0.0;
	
	strcpy (buf, "(");

	for (part = 0; part < 6; part ++) {
	  sprintf (numBuf, "%.15lg", tiePoint [part]);
	  size += strlen (numBuf) + (size==3?0:1);
	  buf = realloc (buf, size);
	  if (size > 3 + strlen(numBuf))
	    strcat (buf, ",");
	  strcat (buf, numBuf);
	}
	strcat (buf, ")");

	for (outIndex = 0; outIndex < outCount; outIndex ++)
	  addGTLabel (outunit [outIndex], "MODELTIEPOINTTAG", buf);

	free (buf);
      }

      /* modelpixelscale is (1/pixDegLon, 1/pixDegLat, 0) */
      {
	int part;
	int size = 3;
	char * buf = malloc (3);
	char numBuf [40];
	double pixScale [3];

	pixScale [0] = 1.0 / pixDegLon;
	pixScale [1] = 1.0 / pixDegLat;
	pixScale [2] = 0.0;

	strcpy (buf, "(");

	for (part = 0; part < 3; part ++) {
	  sprintf (numBuf, "%.15lg", pixScale [part]);
	  size += strlen (numBuf) + (size==3?0:1);
	  buf = realloc (buf, size);
	  if (size > 3 + strlen(numBuf))
	    strcat (buf, ",");
	  strcat (buf, numBuf);
	}
	strcat (buf, ")");

	for (outIndex = 0; outIndex < outCount; outIndex ++)
	  addGTLabel (outunit [outIndex], "MODELPIXELSCALETAG", buf);

	free (buf);
      }
    } /* CART_RESOURCE */

    str = strstr (imageDescription, "AFIDS_RESOURCE(");
    /* AFIDS_RESOURCE(name1=value1,name2=value2,...,nameN=valueN) 
       Examples of supported resource names:
       ACCURACY=1.247456244212
       ACCURACY_CEP=0.2976306287921
       ACCURACY_UNIT='pixels'
       ACCREF='/home/alz/aliapp/alcase/alcase135.img'
       SAT_MODEL_POINTING_ERR=246.2045865996
       SAT_MODEL_POINTING_ERR_UNITS='utm-meter'
       SAT_MODEL_POINTING_ERR_REF='/home/alz/aliapp/basemos/az_landsat_b3.img'
     */
    if (str) {
      char * end = strchr (str, ')');
      char * nameBuf = 0;
      char * value = 0;
      char * p;

      /* special characters:
	 ) closes AFIDS_RESOURCE
	 = separates names from values
	 , separates name/value pairs
       */
      str += strlen ("AFIDS_RESOURCE(");
      
      if (! end) {
	fprintf (stderr, "error parsing AFIDS_RESOURCE, ')' expected");
	return;
      }
      
      {
#define AR_CNT 7
	char * coregMetaNames [AR_CNT] = {"ACCURACY",
					  "ACCURACY_CEP",
					  "ACCURACY_UNIT",
					  "ACCREF",
					  "SAT_MODEL_POINTING_ERR",
					  "SAT_MODEL_POINTING_ERR_UNITS",
					  "SAT_MODEL_POINTING_ERR_REF"
	};

	int i, foundMetaName;
	
	while (str && str < end) {
	  p = strchr (str, '=');
	  if (p && p < end) {
	    nameBuf = realloc (nameBuf, p - str + 1);
	    strncpy (nameBuf, str, p - str);
	    nameBuf [p - str] = 0;

	    str = p + 1;
	    p = strchr( str, ',' );
	    if (! p || p >= end)
	      p = strchr( str, ')');
	    if (p && p <= end) {
	      value = realloc (value, p - str + 1);
	      strncpy (value, str, p - str);
	      value [p - str] = 0;

	      str = p + 1;

	      foundMetaName = 0;
	      for (i = 0; i < AR_CNT; i ++) /* for each value name */
		if (! strcmp (nameBuf, coregMetaNames [i])) { /* if this is coreg meta data */
		  foundMetaName = 1;

		  /* insert name/value into each image */
		  for (outIndex = 0; outIndex < outCount; outIndex ++) {
		    if (isdigit (value [0])) { /* int or real */
		      if (strchr (value, '.')) { /* real */
			double doubleVal;
			sscanf (value, "%lf", & doubleVal);
			if (zladd (outunit [outIndex], "PROPERTY",
				   coregMetaNames [i], & doubleVal,
				   "PROPERTY", "COREG_META_DATA",
				   "FORMAT", "DOUB", NULL) != 1) {
			  fprintf (stderr, "zladd failed to add %s=\"%f\" to image %d\n", coregMetaNames [i], doubleVal, outIndex);
			}
		      } else { /* int */
			int intVal;
			sscanf (value, "%d", & intVal);
			if (zladd (outunit [outIndex], "PROPERTY",
				   coregMetaNames [i], & intVal,
				   "PROPERTY", "COREG_META_DATA",
				   "FORMAT", "INT", NULL) != 1) {
			  fprintf (stderr, "zladd failed to add %s=\"%d\" to image %d\n", coregMetaNames [i], intVal, outIndex);
			}
		      }
		    } else { /* string */
		      if (zladd (outunit [outIndex], "PROPERTY",
				 coregMetaNames [i], value,
				 "PROPERTY", "COREG_META_DATA",
				 "FORMAT", "STRING", NULL) != 1) {
			fprintf (stderr, "zladd failed to add %s=\"%s\" to image %d\n", coregMetaNames [i], value, outIndex);
		      }
		    }
		  }
		}
	    
	      if (! foundMetaName) { /* just a vanilla geotiff tag */
		for (outIndex = 0; outIndex < outCount; outIndex ++) {
		  if (zladd (outunit [outIndex], "PROPERTY",
			     nameBuf, value,
			     "PROPERTY", "GEOTIFF",
			     "FORMAT", "STRING", NULL) != 1) {
		    fprintf (stderr, "zladd failed to add %s=\"%s\" to image %d\n", nameBuf, value, outIndex);
		  }
		}
	      }

	    } else		/* no more name/value pairs  */
	      break;
	  } else		/* error -- didn't find '=' */
	    break;

	} /* while loop */
      }
    }
  }
}

void tiff_to_vic()
{
  int i, outCount, auxCount, outIndex;
  TIFF  *in;			/* TIFF-level descriptor */
  GTIF *gt=(GTIF*)0;		/* GeoKey-level descriptor */
  char inname[133];

  /* open input TIFF file */
  zvselpi(0);
  zvpone("INP", inname, 1, 132);
  in = TIFFOpen(inname, "r");
  if (in == NULL) AbortMsg("Error opening TIFF file");

#ifdef CARTOTAG
  GetTIFFGeoreference(in);
#endif

  if (zvptst("DUMP"))
    {
      dump_file(in);
      return;
    }

  /* If desired, convert to lookup table and write out to file */
  zvpcnt("OUT",&outCount);
  if (outCount==2) {
    WriteTIFFClut(in);
  }

  /* convert TIFF to output file */
  zveaction("SA", " ");
  WriteFileFromTIFF("OUT",1,in);


  /* create a geo key parser */
  gt = GTIFNew(in);
  if (!gt)
    {
      fprintf(stderr,"failed in GTIFNew\n");
      AbortMsg("Error opening creating GTIF\n");
    }

  /* import GeoTIFF tags and keys*/
  {
    int outunit [4];

    /* open the output images for inserting GeoTIFF data */
    for (outIndex = 0; outIndex < outCount; outIndex ++) {
      if (zvunit (& outunit [outIndex], "OUT", outIndex + 1, NULL) != 1)
	zmabend ("zvunit failed");
      zvopen (outunit [outIndex], "OP", "UPDATE", NULL);
    }

    /* import the TIFF geo tags */
    PrintTags (in, gt, outunit, outCount);

    /* import the GeoTIFF keys */
    {
      int numkeys = gt->gt_num_keys;
      GeoKey *key = gt->gt_keys;
      int index;
      char * buf;
      int keyid;

      for (index = 0; index < numkeys; index ++) {
	buf = 0;
	/* gets the key's id and prints its value into buf */
	keyid = PrintKey (++key, &buf);

	for (outIndex = 0; outIndex < outCount; outIndex ++)
	  addGTLabel (outunit [outIndex], GTIFKeyName (keyid), buf);

	free (buf);
      }

    }

    /* close output images */
    for (outIndex = 0; outIndex < outCount; outIndex ++)
      zvclose (outunit [outIndex], NULL);
  }

  /* add auxilliary files, if any */
  zvpcnt("AUXIL",&auxCount);
  for (i=0; i<auxCount; i++)
    {
      if (!TIFFReadDirectory(in))
	AbortMsg("Not enough images in TIFF file for auxilliaries");
      WriteFileFromTIFF ("AUXIL", i + 1, in);
    }
      
  /* close up shop */
  TIFFClose(in);
}

void WriteFileFromTIFF(char *parm, int instance, TIFF *tif)
{
  int  unit[4],nl,ns,i,nb,numout;

  SetUpVicarFile(parm,instance,tif,unit,&nl,&ns,&nb,&numout);
  if (!WriteVicarData(unit,tif,nl,ns,nb))
    AbortMsg("Error in Copying data to TIFF ");

  for (i=0; i<nb; i++)
    zvclose(unit[i], NULL);
}


void SetUpVicarFile(char *parm, int instance, TIFF *in, int * outunit, int *nl, int *ns, int*nb, int*numout)
{
  int i;
  uint16 nbs;
  char name[132];
  int nlout,nsout;
  int count;
  char *format=NULL;
  uint16 pixsize;

  TIFFGetField(in, TIFFTAG_BITSPERSAMPLE, &pixsize);
  switch(pixsize)
    {
    case 32: format = "FULL"; break;
    case 16: format = "HALF"; break;
    case 1: case 2: case 4: case 8: format = "BYTE"; break;
    }
  zvpcnt("OUT", numout);
     
  /* get info from TIFF file */
  TIFFGetField(in,TIFFTAG_IMAGEWIDTH,ns);
  TIFFGetField(in,TIFFTAG_IMAGELENGTH,nl);
  if (!TIFFGetField(in, TIFFTAG_SAMPLESPERPIXEL, &nbs))
    nbs = 1;
  *nb = nbs;

  if (*nb < 1) {
    printf ("forcing nb to 1, was %d\n", *nb);
    *nb=1;
  }
  if (*nb > 4) *nb=3;
  if (*nb==3 && *numout<3)
    {
      zvmessage("Cannot yet compress RGB to 8-bit"," ");
      zabend();
    }
  if (*numout==3) {
    printf ("forcing nb to 3, was %d, because numout == 3\n", *nb);
    *nb=3;
  }

  if (*numout==0) {
    printf ("forcing nb to 0, was %d, because numout == 0\n", *nb);
    *nb=0;
  }

  zvselpi(0);  /* INP is not a VICAR file */

  zvp("NL",&nlout,&count);
  if (!nlout || (tolower(*parm)!='o')) nlout=*nl;
  zvp("NS",&nsout,&count);
  if (!nsout || (tolower(*parm)!='o')) nsout=*ns;

  *nl=nlout;
  *ns=nsout;

  for (i=0;i<*nb;i++)
    {
      zvpone(parm, name, instance+i, 132);
      zvunit(outunit+i, "xxx", instance+i,"U_NAME",name, NULL);
	      
      /* Open up VICAR file with proper size */
	
      zvopen(outunit[i],"OP","WRITE","U_NL",nlout,"U_NS",nsout,
	     "O_FORMAT",format, NULL);
	      
      /* Reopen in UPDATE for random access */
      zvclose(outunit[i], NULL);
      zvopen(outunit[i],"OP","UPDATE", NULL);
    }
}


int WriteVicarData(int * outunit, TIFF *in, int nl,int ns, int nb)
{
  unsigned char *buf,*lbuf[3],*ptr;
  uint32 x,y,y1,z;
  uint16 planar,photo;
  int nlines,nsamps;
  int tilebufsize;
  int tile_width,tile_height;
  int scanbytes;
  int band;
  int lookup;
  int chunky;
  int pixsize;
  int i,j,val;
  unsigned short *cptr;
  uint16* clut[3];

  if (!nb) return 1;

  TIFFGetField( in,  TIFFTAG_PHOTOMETRIC, &photo);
  lookup = (photo == PHOTOMETRIC_PALETTE && nb==3);
  if (lookup )
    {
      /* need to create lookup table */
      TIFFGetField(in, TIFFTAG_COLORMAP,
		   clut,clut+1,clut+2);
      for (j=0;j<3;j++)
	{
	  cptr = clut[j];
	  for (i=0;i<256;i++,cptr++)
	    *cptr = (*cptr)>>8;
	}

    }

  zvget(outunit[0],"PIX_SIZE",&pixsize, NULL);
  TIFFGetField( in, TIFFTAG_PLANARCONFIG, &planar);
  chunky = (planar == PLANARCONFIG_CONTIG) && (nb > 1);

  if (TIFFIsTiled(in))            /* TIFF IMAGE ORGANIZED IN TILES */
    {
      TIFFGetField(in, TIFFTAG_TILEWIDTH, &tile_width);
      TIFFGetField(in, TIFFTAG_TILELENGTH, &tile_height);
      tilebufsize = TIFFTileSize(in);
      buf = (unsigned char *)malloc(tilebufsize);
      if (chunky && !lookup)
	lbuf[0] = (unsigned char *)malloc(tilebufsize);
      else
	lbuf[0] = (unsigned char *)malloc(tilebufsize*nb);
      for (i=1;i<nb;i++) 
	lbuf[i] = lbuf[0]+ i*tile_width*tile_height*pixsize;
      if (!buf || !lbuf[0]) 
	AbortMsg("failed to allocate buffer for tiles");
 
      if (printstat) StartStatus(nl*nb);
      for (band=0; band < nb; band++)
	{
	  for (y = 0; y < nl; y+=tile_height)
	    {
	      if (printstat) UpdateStatus(band*nl+y+1);
	      nlines = MIN(tile_height, nl - y);
	      for (x = 0; x < ns; x+=tile_width)
		{
				/* Read tile in to buffer */
				
		  nsamps = MIN(tile_width, ns - x);
		  if (lookup)
		    {
		      if (TIFFReadTile2(in, buf, x,y,0,0) < 0)
			goto bad;
		      /*  Apply lookup table */
		      ptr=buf;
		      for (z=0; z<tile_width*tile_height; z++)
			{
			  val = *ptr++;
			  for (band=0;band<nb;band++)
			    lbuf[band][z] = clut[band][val];
			}
		    }
		  else if (chunky)
		    {
		      if (TIFFReadTile2(in, buf, x,y,0,0) < 0)
			goto bad;
		      /* reshuffle pixels from chunky: rgbrgb...*/
		      ptr=buf;
		      for (z=0; z<tile_width*tile_height; z++)
			for (band=0;band<nb;band++)
			  lbuf[band][z] = *ptr++;
		    }
		  else
		    {
		      for (band=0; band<nb; band++)
			if (TIFFReadTile2(in, lbuf[band], x,y,0,band) < 0)
			  goto bad;
		    }
	      
				/* Write out a single tile to VICAR */
		  for (y1=0; y1< nlines; y1++)
		    {
		      for (band=0;band<nb;band++)
			{
			  zvwrit(outunit[band],
				 lbuf[band]+y1*tile_width*pixsize,"line",y+y1+1,
				 "samp",x+1,"nsamps", nsamps, NULL);
			}
		    }
		} /* samp loop */
	    } /* line loop */
	} /* band loop */
    }
  else                              /* TIFF IMAGE ORGANIZED IN STRIPS */
    {
      scanbytes = TIFFScanlineSize(in);
      buf = (unsigned char *)malloc(scanbytes);
      if (chunky && !lookup)
	lbuf[0] = (unsigned char *)malloc(scanbytes);
      else
	lbuf[0] = (unsigned char *)malloc(scanbytes * nb);

      for (i=1;i<nb;i++) 
	lbuf[i] = lbuf[0]+ i*ns;
      
      if (printstat) StartStatus(nl);
      for (y = 0; y < nl; y++)
	{
	  if (printstat) UpdateStatus(y+1);
	  if (lookup)
	    {
	      if (TIFFReadScanline2(in, buf,y,0) < 0)
		goto bad;
	      /* Use lookup table */
	      ptr=buf;
	      for (z=0; z<ns; z++)
		{
		  val = *ptr++;
		  for (band=0;band<nb;band++)
		    lbuf[band][z] = clut[band][val];
		}
	    }
	  else if (chunky)
	    {
	      if (TIFFReadScanline2(in, buf,y,0) < 0)
		goto bad;
	      /* reshuffle pixels from chunky: rgbrgb...*/
	      ptr=buf;
	      for (z=0; z<ns; z++)
		for (band=0;band<nb;band++)
		  lbuf[band][z] = *ptr++;
	    }
	  else  /* band-interleaved by line */
	    {
	      for (band=0; band<nb; band++)
		if (TIFFReadScanline2(in, lbuf[band],y,band) < 0)
		  goto bad;
	    }
		
	  for (band=0; band<nb; band++)
	    {
	      zvwrit(outunit[band],lbuf[band],"LINE",y+1, NULL);
	    }
		  
	} /* line loop */
		    
    }
      
  free(buf);
  free(lbuf[0]);
  return (1);
 bad:
  free(buf);
  free(lbuf[0]);
  return (0);

}


/*********************************************************************/
/*********************** vic_to_tiff        **************************/
/*********************************************************************/


static void exportGeoTIFFKeys (int unit, TIFF * tiff, GTIF *gtif)
{
#define return_on_error(A)   zvsignal(A,status,0); if (status <= 0) zabend()
#define continue_on_error(A) zvsignal(A,status,0); if (status <= 0) continue
#define break_on_error(A)    zvsignal(A,status,0); if (status <= 0) break
#define MAXLINESIZE 200000
/* Buffer for print_key_value_pairs() so it can pack items */
    int status;			/* Return status indicator */
/* Multivalued label item structure */
    struct multival
    {
	int nelements;
	int maxlength;
	char *data;		/* ptr to array[nelements][maxlength] */
	int allocsize;
    };

    int instances[MAX_PROPS],number_of_props,subset,dummy;
    char prop_names[MAX_PROPS][MAX_LABEL_KEY_SIZE+1];
    char key[MAX_LABEL_KEY_SIZE+1],format[32];
    struct multival value;
    int maxlength, nelement;
    char * afidsResource = 0;

    value.allocsize = 0;
    value.data = NULL;

    /* Get property names of property subsets */
    number_of_props = MAX_PROPS;	/* No more than MAX_PROPS allowed */
    status = zlpinfo(unit,prop_names[0],&number_of_props,
		     "inst_num", instances, "ulen",MAX_LABEL_KEY_SIZE+1, NULL);
    return_on_error(unit);

    printf ("exporting GeoTIFF\n");

    /* Cycle through each subset, listing out all labels */
    for (subset = 0; subset < number_of_props; subset++)
      {
	if (!strcmp ("GEOTIFF", prop_names[subset]) || !strcmp ("COREG_META_DATA", prop_names[subset])) {
	  /* Set current key to task name */
	  status = zlinfo(unit,"PROPERTY","PROPERTY",format,&dummy,
			  &value.nelements,"PROPERTY",prop_names[subset],
			  "INSTANCE",instances[subset],
			  "STRLEN", &value.maxlength, NULL);
	  continue_on_error(unit);

	  /* Cycle through each key in the subset */
	  while (TRUE)
	    {
	      /* Get next keyword */
	      status = zlninfo(unit,key,format,&dummy,&value.nelements,
			       "STRLEN", &value.maxlength, NULL);
	      if ((status == END_OF_LABEL) || (strcmp(key,"TASK") == 0) ||
		  (strcmp(key,"PROPERTY") == 0)) break;
	      break_on_error(unit);
	      value.maxlength++;	/* leave room for null string terminator */

	      if (!strcmp ("GEOTIFF", prop_names[subset])) {
		int gk_key = -1, foundLegacyValue = 0;
		char * value;

		/* the VICAR property label key is now in the string "key" */
		/* get the type, number and length of label item indicated by key*/
		zlinfo (unit, "PROPERTY", key, format, &maxlength, &nelement, "PROPERTY", "GEOTIFF", NULL);

		value = malloc (maxlength + 1);

		/* get the key's value */
		zlget (unit, "PROPERTY", key, value, "PROPERTY", "GEOTIFF", NULL);

		if (strcasecmp (key, "ModelTransformationTag") && /* older code already handles these */
		    strcasecmp (key, "ModelPixelScaleTag") &&
		    strcasecmp (key, "ModelTiepointTag")) {
		  char * p = strstr (value, ";gk_key=");

		  if (!p)
		    p = strstr (value, ";GK_KEY=");

		  if (p)	/* found gk_key */
		    sscanf (p + 8, "%d", & gk_key); /* get the key */
		  else {	/* found GEOTIFF value with no gk_key; try legacy values */
		    if (! strcasecmp (key, "GTModelTypeGeoKey"))
		      gk_key = 1024;
		    else if (! strcasecmp (key, "GTRasterTypeGeoKey"))
		      gk_key = 1025;
		    else if (! strcasecmp (key, "GeographicTypeGeoKey"))
		      gk_key = 2048;
		    else if (! strcasecmp (key, "GeogEllipsoidGeoKey"))
		      gk_key = 2056;
		    else if (! strcasecmp (key, "GeogPrimeMeridianGeoKey"))
		      gk_key = 2051;
		    else if (! strcasecmp (key, "GeogAngularUnitsGeoKey"))
		      gk_key = 2054;
		    else if (! strcasecmp (key, "GeogSemiMajorAxisGeoKey"))
		      gk_key = 2057;
		    else if (! strcasecmp (key, "GeogSemiMinorAxisGeoKey"))
		      gk_key = 2058;
		    else if (! strcasecmp (key, "PCSCitationGeoKey"))
		      gk_key = 3073;
		    else if (! strcasecmp (key, "ProjCenterLatGeoKey"))
		      gk_key = 3089;
		    else if (! strcasecmp (key, "ProjLinearUnitsGeoKey"))
		      gk_key = 3076;
		    else if (! strcasecmp (key, "ProjectedCSTypeGeoKey"))
		      gk_key = 3072;
		    else if (! strcasecmp (key, "ProjectionGeoKey"))
		      gk_key = 3074;
		    if (gk_key > 0)
		      foundLegacyValue = 1;
		  }

		  if (gk_key < 0) {
		    printf ("ignoring GEOTIFF value with no gk_key: %s=%s\n", key, value);
		  } else {
		    if (foundLegacyValue || strstr (value, ";gk_type=Short")) { /* it's a Short */
		      if (! foundLegacyValue)
			* (strstr (value, ";gk_type=Short")) = '\0';
		      if (value [0] == '(')
			printf ("not exporting GEOTIFF multi Short values with gk_key %d: %s=%s\n", gk_key, key, value);
		      else {
			int shortValue;
			{
			  char * p = strstr (value, "(");
			  if (p)
			    * p = '\0';
			}
			sscanf (value, "%d", & shortValue);
			GTIFKeySet(gtif, gk_key, TYPE_SHORT, 1, shortValue);
		      }
		    } else if (strstr (value, ";gk_type=Double")) { /* it's a Double */
		      * (strstr (value, ";gk_type=Double")) = '\0';
		      if (value [0] == '(')
			printf ("not exporting GEOTIFF multi Double values with gk_key %d: %s=%s\n", gk_key, key, value);
		      else {
			double doubleValue;
			{
			  char * p = strstr (value, "(");
			  if (p)
			    * p = '\0';
			}
			sscanf (value, "%lf", & doubleValue);
			GTIFKeySet(gtif, gk_key, TYPE_DOUBLE, 1, doubleValue);
		      }
		    } else {	/* it's a String */
		      *p = '\0';

		      GTIFKeySet(gtif, gk_key, TYPE_ASCII, 0, value);
		    }
		  }
		}
	      } else {  /* COREG_META_DATA */
		/* the VICAR property label key is now in the string "key" */
		/* get the type, number and length of label item indicated by key*/

		zlinfo (unit, "PROPERTY", key, format, &maxlength, &nelement, "PROPERTY", "COREG_META_DATA", NULL);

		if (! strcasecmp (key, "ACCURACY") ||
		    ! strcasecmp (key, "ACCURACY_CEP") ||
		    ! strcasecmp (key, "ACCURACY_UNIT") ||
		    ! strcasecmp (key, "ACCREF") ||
		    ! strcasecmp (key, "SAT_MODEL_POINTING_ERR") ||
		    ! strcasecmp (key, "SAT_MODEL_POINTING_ERR_UNITS") ||
		    ! strcasecmp (key, "SAT_MODEL_POINTING_ERR_REF")) {

		  char * value = malloc (maxlength + 1);
		
		  printf ("found key %s\n", key);
		  /* get the key's value */
		  zlget (unit, "PROPERTY", key, value, "PROPERTY", "COREG_META_DATA", NULL);

		  if (!strcmp (format, "INT")) {
		    int intVal = * ((int *) value);
		    free (value);
		    value = malloc (100);
		    sprintf (value, "%d", intVal);
		  } else if (!strcmp (format, "REAL")) {
		    float realVal = * ((float *) value);
		    free (value);
		    value = malloc (100);
		    sprintf (value, "%f", realVal);
		  } else if (!strcmp (format, "DOUB")) {
		    double doubleVal = * ((double *) value);
		    free (value);
		    value = malloc (100);
		    sprintf (value, "%lf", doubleVal);
		  }

		  if (afidsResource) {
		    afidsResource = realloc (afidsResource, strlen (afidsResource) + strlen (key) + strlen (value) + 3); /* , plus = plus null */
		    strcat (afidsResource, ",");
		    strcat (afidsResource, key);
		    strcat (afidsResource, "=");
		    strcat (afidsResource, value);
		  } else {
		    afidsResource = malloc (strlen (key) + strlen (value) + 2); /* = plus null */
		    strcpy (afidsResource, key);
		    strcat (afidsResource, "=");
		    strcat (afidsResource, value);
		  }
		
		  free (value);
		} else {
		  printf ("ignoring key %s\n", key);
		}
	      }
	      
	      continue_on_error(unit);
	      
	    }	/* End of while loop */
	}
      }		/* End of for loop */

    if (value.data != NULL)
      free(value.data);

    if (afidsResource) {
      char * buf = malloc (strlen (afidsResource) + strlen ("AFIDS_RESOURCE()") + 1);

      sprintf (buf, "AFIDS_RESOURCE(%s)", afidsResource);
      TIFFSetField (tiff, TIFFTAG_IMAGEDESCRIPTION, buf);

      free (buf);
      free (afidsResource);
    }
}

void vic_to_tiff()
{
  int count;
  TIFF  *out;
  char outname[133];
  char tiffmode[10];

  /* getting the geotiff vicar label, if any.  the file is closed after */
  gtlabelflag = gtgetlab("INP",1,&gtlabelstr,&gtlabnl,&gtlabns);

  /* open output TIFF file */
  zvpone("out", outname, 1, 132);

  out = TIFFOpen(outname, "w");

  if (out == NULL) AbortMsg("Error opening TIFF file");
  AddTIFFGeoreference(out);

  /* convert input file to TIFF */
  zveaction("SA", "");

  zvp("TIFFMODE",tiffmode,&count);

  AddFiletoTIFF("INP",out,tiffmode); 


  /* add auxilliary files if any, using strips */
  zvpcnt("AUXIL",&count);

  if (count > 0)
    {
      TIFFWriteDirectory(out);

      AddFiletoTIFF("AUXIL",out,"strips");

    }
      
  /* close up shop */
  TIFFClose(out);
}

/* convert deg-min-sec to signed decimal degrees */

double DMSToDecDegrees(angl,count)
     double angl[3];
     int count;
{
  register double degrees=0.0;
  enum {deg,min,sec};
	
  switch (count-1)
    {
    case sec:
      degrees+=(double)angl[sec]/3600.;  /* fall through */
    case min:
      degrees+=(double)angl[min]/60.0;  /* fall through */
    case deg:
      degrees+=(double)angl[deg];
      break;
    default: /* default value is 1.0 */
      degrees=1.0;
      break;
    }
	
  return degrees;
}

/* convert deg-min-sec to thousandths of second */

double DMSToSec1000(angl,count)
     double angl[3];
     int count;
{
  register double s1000=0.0;
  enum {deg,min,sec};
	
  switch (count-1)
    {
    case sec:
      s1000+=angl[sec]*1e3;  /* fall through */
    case min:
      s1000+=angl[min]*6e4;  /* fall through */
    case deg:
      s1000+=angl[deg]*36e5;
      break;
    default: /* default value is 1.0 */
      s1000=1.0;
      break;
    }
	
  return s1000;
}


#define EQUATOR 90L*60*60*1000
#define PRIME_MERIDIAN 180L*60*60*1000

/* Add GeoTIFF 1.0 compliant Tags */
void AddTIFFGeoreference(TIFF *out)
{
  char metadata[200];
  GTIF *gtif=(GTIF*)0; /* GeoKey-level descriptor */
  double theLat[3];
  int count,def;

  zvp("METADATA",metadata,&count);
  zvparmd("LAT",theLat,&count,&def,0,0);
  if (def && (!metadata[0] ||metadata[0]==' ') && zvptst("LATLONG")
      && !gtlabelflag) return; /* not defined */

  gtif = GTIFNew(out);
  if (!gtif)
    {
      zvmessage("failed in GTIFNew"," ");
      zabend();
    }

  /* at this point, either get georef from file or parms */

  if (metadata[0] && metadata[0]!=' ')
    {
      FILE *metafile = fopen(metadata,"r");

      if (!metafile || !GTIFImport(gtif,0,metafile))
	{
	  zvmessage("Failed to import GeoTIFF metadata"," ");
	  zabend();
	}
      fclose(metafile);
    }
  else if (gtlabelflag)
    VicAddGeoTIFF(out,gtif);
  else AddGeoTIFF(out,gtif);

  /* backward compatibility NOT */
  /* AddOldTIFFGeoreference(out); */

  GTIFWriteKeys(gtif);
  GTIFFree(gtif);	
}

void VicAddGeoTIFF(TIFF *out, GTIF *gtif)
{
  double * tiepoints = 0;
  double modelTrans [16];
  int tiepointCount = 0;
  double pixscale[3];
  char *p;
  int i;
	
  printf("USING VICAR GEOTIFF LABEL TO CREATE, PARAMETERS IGNORED\n");

  p = ms_find(gtlabelstr,"MODELTRANSFORMATIONTAG=(");
  if (p!=0) {
    for (i = 0; i < 16; i ++) {
      modelTrans [i] = ms_dnum(&p);
      p++;
    }

    TIFFSetField(out,TIFFTAG_GEOTRANSMATRIX, 16, modelTrans);
  }
        
  p = ms_find(gtlabelstr,"MODELPIXELSCALETAG=(");
  if (p!=0)
    {
      pixscale[0] = ms_dnum(&p); p++;
      pixscale[1] = ms_dnum(&p);
      pixscale[2] = 0.0;
      TIFFSetField(out,TIFFTAG_GEOPIXELSCALE, 3,pixscale);
    }
  p = ms_find(gtlabelstr,"MODELTIEPOINTTAG=(");

  while (p!=0)
    {
      tiepointCount ++;
      tiepoints = (double *) realloc (tiepoints, tiepointCount * 6 * sizeof (double));

      tiepoints[(tiepointCount - 1) * 6 + 0] = ms_dnum(&p); p++;
      tiepoints[(tiepointCount - 1) * 6 + 1] = ms_dnum(&p); p++;
      tiepoints[(tiepointCount - 1) * 6 + 2] = ms_dnum(&p); p++;
      tiepoints[(tiepointCount - 1) * 6 + 3] = ms_dnum(&p); p++;
      tiepoints[(tiepointCount - 1) * 6 + 4] = ms_dnum(&p); p++;
      tiepoints[(tiepointCount - 1) * 6 + 5] = ms_dnum(&p); p++;
      p = ms_find(p,"MODELTIEPOINTTAG=(");
    }

  if (tiepoints) {
    TIFFSetField(out,TIFFTAG_GEOTIEPOINTS, tiepointCount * 6,tiepoints);
    free (tiepoints);
  }
        
  /* add GeoTIFF keys */
  {
    int geounit;
    zvunit(&geounit,"INP",1, NULL);
    zvopen(geounit,"OP","READ","OPEN_ACT","SA", "LAB_ACT","SA", NULL);
    exportGeoTIFFKeys (geounit, out, gtif);
    zvclose(geounit, NULL);
  }
}

void AddGeoTIFF(TIFF *out, GTIF *gtif)
{
  double theLat[3],theLong[3];
  double hDegPix[3],vDegPix[3];
  double tiepoints[6];
  double pixscale[3];
  double latd,longd;
  double parmvalues[3];
  double xPos,yPos;
  int def,count,geog_set,proj_set,has_geotiff=0;
  double hres,vres;
  int gcs;
  char *gcs_name;

  zvparmd("LAT",theLat,&count,&def,0,0);
  geog_set = (!def);
  proj_set = !zvptst("LATLONG");
  if (geog_set)
    {
      latd=DMSToDecDegrees(theLat,count);
      if (zvptst("SOUTH"))
	latd=-latd;			
      zvparmd("LONG",theLong,&count,&def,0,0);
      longd=DMSToDecDegrees(theLong,count);
      if (zvptst("WEST"))
	longd= -longd;			
      zvparmd("HDEGPIX",hDegPix,&count,&def,0,0);
      hres = DMSToDecDegrees(hDegPix,count);
      zvparmd("VDEGPIX",vDegPix,&count,&def,0,0);
      vres = DMSToDecDegrees(vDegPix,count);

      pixscale[0] = hres;
      pixscale[1] = vres;
      pixscale[2] = 0.0;
      tiepoints[3] = longd;
      tiepoints[4] = latd;
      has_geotiff = 1;
    }
  else if (proj_set)
    {
      zvparmd("PIXSIZE",pixscale,&count,&def,0,0);
      zvparmd("PIX_ORIG",&tiepoints[3],&count,&def,0,0);
      pixscale[2] = 0.0;
      has_geotiff = 1;
    }
	    
		
  if (has_geotiff) 
    {
      zvparmd("XPIXPOS",&xPos,&count,&def,0,0);
      zvparmd("YPIXPOS",&yPos,&count,&def,0,0);
      tiepoints[0] = xPos;
      tiepoints[1] = yPos;
      tiepoints[2] = 0.0;
      tiepoints[5] = 0.0;

      if (zvptst("WGS84"))
	{	
	  gcs = GCS_WGS_84;
	  gcs_name = "WGS-84 Datum";
	}
      else if (zvptst("WGS72"))
	{	
	  gcs = GCS_WGS_72;
	  gcs_name = "WGS-72 Datum";
	}
      else if (zvptst("NAD27"))
	{	
	  gcs = GCS_NAD27;
	  gcs_name = "NAD 27 Datum";
	}
      else if (zvptst("NAD83"))
	{	
	  gcs = GCS_NAD83;
	  gcs_name = "NAD 83 Datum";
	}
      else
	{	
	  gcs = GCS_WGS_84;
	  gcs_name = "WGS-84 Datum";
	}

      /* Add GeoTIFF tags tying the image down to projection space */
	
      TIFFSetField(out,TIFFTAG_GEOTIEPOINTS, 6,tiepoints);
      TIFFSetField(out,TIFFTAG_GEOPIXELSCALE, 3,pixscale);
		
      /* Set the GeoTIFF datum (Geographic type) */
		
      GTIFKeySet(gtif, GeographicTypeGeoKey, TYPE_SHORT,  1, gcs);
      GTIFKeySet(gtif, GeogCitationGeoKey, TYPE_ASCII, 0, gcs_name);
    }
  /* Add GeoTIFF Keys Defining the projection */

  if (geog_set)
    {
      GTIFKeySet(gtif, GTModelTypeGeoKey, TYPE_SHORT, 1, ModelGeographic);
      GTIFKeySet(gtif, GTRasterTypeGeoKey, TYPE_SHORT, 1, RasterPixelIsArea);
      GTIFKeySet(gtif, GTCitationGeoKey, TYPE_ASCII, 0, "Geographic Model/GeoTIFF 1.0");
    }
  else if (proj_set)
    {
      GTIFKeySet(gtif, GTModelTypeGeoKey, TYPE_SHORT, 1, ModelProjected);
      GTIFKeySet(gtif, GTRasterTypeGeoKey, TYPE_SHORT, 1, RasterPixelIsPoint);
      GTIFKeySet(gtif, GTCitationGeoKey, TYPE_ASCII, 0, "GeoTIFF 1.0");
      GTIFKeySet(gtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1, KvUserDefined);
      GTIFKeySet(gtif, ProjectionGeoKey, TYPE_SHORT, 1, KvUserDefined);
      GTIFKeySet(gtif, ProjLinearUnitsGeoKey, TYPE_SHORT, 1, Linear_Meter);

      GTIFKeySet(gtif, ProjCoordTransGeoKey, TYPE_SHORT, 1, CT_AlbersEqualArea);
		
      /* parameters for Albers */
      zvparmd("STDPARAL",parmvalues,&count,&def,0,0);  /* check for 2 values */
      GTIFKeySet(gtif, ProjStdParallel1GeoKey, TYPE_DOUBLE, 1,parmvalues[0] );
      GTIFKeySet(gtif, ProjStdParallel2GeoKey, TYPE_DOUBLE, 1,parmvalues[1] );

      zvparmd("NO_LONG",parmvalues,&count,&def,0,0);  /* check for 2 values */
      GTIFKeySet(gtif, ProjNatOriginLongGeoKey, TYPE_DOUBLE, 1,parmvalues[0] );

      zvparmd("FO_LAT",parmvalues,&count,&def,0,0);  /* check for 2 values */
      GTIFKeySet(gtif, ProjFalseOriginLatGeoKey, TYPE_DOUBLE, 1,parmvalues[0] );

      zvparmd("FAL_ORIG",parmvalues,&count,&def,0,0);  /* check for 2 values */
      GTIFKeySet(gtif, ProjFalseOriginEastingGeoKey, TYPE_DOUBLE, 1,parmvalues[0] );
      GTIFKeySet(gtif, ProjFalseOriginNorthingGeoKey, TYPE_DOUBLE, 1,parmvalues[1] );
    }
}

void AddFiletoTIFF(char *parm, TIFF *tif, char *mode)      /* "tiled" or "Strips" */
{
  int  unit[4];
  int i,nl,ns,nb,status;
  int bit8;
  char name[201];

  zvpcnt(parm, &numinp);
  bit8 = zvptst("bit8");
     
   
  for (i=0; i<numinp; i++)
    {
      zvpone(parm, name, i+1, 200);
      zvunit(unit+i, parm, i+1,"U_NAME",name, NULL);
      status = zvopen(unit[i],"OP","READ", NULL);
    }

  nb = (numinp==3) ? 3 : 1;
      
  if (numinp==3 && bit8)
    {
      ConvertToLut( unit );
      nb = 1;
      numinp=2;
    }
      
  switch (*parm)
    {
    case 'i':case 'I':
      TIFFSetField(tif, TIFFTAG_SUBFILETYPE, 0);
      break;
    default:
      TIFFSetField(tif, TIFFTAG_SUBFILETYPE, FILETYPE_REDUCEDIMAGE);
      break;
    }


  SetUpTiffDirectory(unit,tif,mode,&nl,&ns,nb);
  if (!WriteTIFFData(unit,tif,nl,ns,nb))
    AbortMsg("Error in Copying data to TIFF ");
	    
  for (i=0; i<nb; i++)
    zvclose(unit[i], NULL);
}

#define MAX_DIM 512

int ConvertToLut(int * unit)
{
  int tempunit[2];
  int i;
  int line,samp=0;
  int status=1;
  int lineinc,sampinc;
  int nl,ns,numcolors,size;
  unsigned char *lbuffer[3];
  unsigned char *index;
  unsigned char *red,*grn,*blu,*iptr;
  struct table_def *table=NULL;
	
  zvget(unit[0],"NL",&nl,"NS",&ns, NULL);
  lineinc = (nl + MAX_DIM-1)/MAX_DIM;
  sampinc = (ns + MAX_DIM-1)/MAX_DIM;
  while (!(ns % sampinc)) sampinc++;
	
  /* Create a temporary index and LUT file */
	
  status = zvunit( tempunit+0, "xxx",5,"u_name","vtiff_temp_img", NULL);
  status = zvopen( tempunit[0], "op", "write","u_nl",nl,"u_ns",ns, NULL);

  status = zvunit( tempunit+1, "xxx",6,"u_name","vtiff_temp_lut", NULL);
  status = zvopen( tempunit[1], "op", "write","u_nl",1,"u_ns",1024, NULL);
	
  /* allocate memory */
	
  if (!table_new( &table, 254 ))
    {
      zvmessage("Memory error creating color table"," ");
      zabend();
    }
  size = (ns<256) ? 256 : ns;
  for (i=0;i<3;i++)
    {
      lbuffer[i] = (unsigned char *)malloc(size);
      if (!lbuffer[i])
	{
	  zvmessage("Memory error allocating LUT buffer"," ");
	  zabend();
	}
    }
  index = (unsigned char *)malloc(size < 1024 ? 1024 : size);
  if (!index) 
    {
      zvmessage("Memory error creating index table"," ");
      zabend();
    }

	
  /* run through image & collect colors */
	
  for (line=1; line<=nl; line+=lineinc)
    {
      for (i=0;i<3;i++)
	zvread(unit[i],lbuffer[i],"line",line, NULL);
		
      red = lbuffer[0]; grn=lbuffer[1]; blu=lbuffer[2];
      numcolors=0;
      for (samp=samp%ns; samp<ns; samp+=sampinc,numcolors++)
	{
	  red[numcolors] = red[samp];
	  grn[numcolors] = grn[samp];
	  blu[numcolors] = blu[samp];
	}
		
      table_add_colors(table, red, grn, blu, numcolors );

    }
	
  table_build( table );  /* computes optimal color table */
	
  /* write out INDEX image */
  red = lbuffer[0]; grn=lbuffer[1]; blu=lbuffer[2];
  for (line=1; line<=nl; line++)
    {
      for (i=0;i<3;i++)
	zvread(unit[i],lbuffer[i],"line",line, NULL);

      table_rgb_to_index( table, red, grn, blu, ns, index );
      zvwrit( tempunit[0], index, NULL);
    }
	
  /* write out LUT */
  table_extract_lut( table, red, grn, blu);
  iptr = index;
  memset(index, 0, 1024);
  for (i=0;i<256;i++)
    {
      *iptr++ = *red++;
      *iptr++ = *grn++;
      *iptr++ = *blu++;
      iptr++;
    }
  zvwrit( tempunit[1], index, NULL);

  /* close old files * free up memory */
  for (i=0;i<3;i++)
    {
      zvclose( unit[i], NULL);
      free(lbuffer[i]);
    }
  free (index);
  table_free( table );
	
  /* reset units */
  status = zvclose( tempunit[0], NULL);
  status = zvopen( tempunit[0], "op", "read","clos_act", "delete", NULL);
  status = zvclose( tempunit[1], NULL);
  status = zvopen( tempunit[1], "op", "read","clos_act", "delete", NULL);
  unit[0] = tempunit[0];
  unit[1] = tempunit[1];
	
  return status;
}

/* This will probably do for initializing most TIFF files */

void SetUpTiffDirectory(int * inunit, TIFF *out, char *mode,      /* "tiled" or "Strips" */
			int *nl, int *ns,	 /* output: sizes */
			int nb)		 /* input : number of bands */
{
  int compression; // added by pk
  int count,def,tiled;
  int tile_width,tile_height;
  int pixsize;
  int resunit;
  float xresolution,yresolution;
  char *tiff_time();
  uint16 clut[3][256];

  tiled = (toupper(*mode) == 'T');

  resunit = zvptst( "INCH") ? RESUNIT_INCH : RESUNIT_CENTIMETER;
  zvp( "XRES", &xresolution, &def); 
  zvp( "YRES", &yresolution, &def); 
  TIFFSetField(out, TIFFTAG_RESOLUTIONUNIT, resunit);
  TIFFSetField(out, TIFFTAG_XRESOLUTION, xresolution);
  TIFFSetField(out, TIFFTAG_YRESOLUTION, yresolution);
  zvget(inunit[0],"PIX_SIZE",&pixsize, NULL);
  TIFFSetField(out, TIFFTAG_BITSPERSAMPLE, (uint16) pixsize*8);

  if (pixsize==2)  /* This is a signed halfword */
    TIFFSetField(out, TIFFTAG_SAMPLEFORMAT, SAMPLEFORMAT_INT);
  else if (pixsize==4)  /* This is unsigned fullword */
    TIFFSetField(out, TIFFTAG_SAMPLEFORMAT, SAMPLEFORMAT_UINT);

 
#ifdef OLD_COMPRESSION_CODE
  /* Compression */
  if (zvptst("LZW"))
    compression = COMPRESSION_LZW;
  else if (zvptst("PACKBITS"))
    compression = COMPRESSION_PACKBITS;
  else               
    compression = COMPRESSION_NONE;

  TIFFSetField(out, TIFFTAG_COMPRESSION, compression);
#else

  if (zvptst("ZIP")) {
    TIFFSetField (out, TIFFTAG_COMPRESSION, COMPRESSION_DEFLATE);
    {
      int zipcomp, cnt;
      zvp("ZIPCOMP", & zipcomp, & cnt);

      TIFFSetField (out, TIFFTAG_ZIPQUALITY, zipcomp); /* all values are lossless; this value is fastest */
    }
  }

#ifdef WE_EVER_GET_THE_SCOOP_ON_LOSSLESS_JPEG_IN_TILED_TIFF
  else if (zvptst ("JPG")) {
    TIFFSetField (out, TIFFTAG_COMPRESSION, COMPRESSION_JPEG);
    TIFFSetField (out, TIFFTAG_JPEGQUALITY, 100); /* lossless? */
    TIFFSetField (out, TIFFTAG_JPEGCOLORMODE, JPEGCOLORMODE_RAW);
  }
#endif

#endif

  /* Tiling parameters */
  if (tiled)
    {
      zvp("TLINES",&tile_height,&count);
      zvp("TSAMPS",&tile_width,&count);
      if (tile_height%8 || tile_width%8)
	{
	  zvmessage("Tile size must be a multiple of 8"," ");
	  zabend();
	}
      TIFFSetField(out, TIFFTAG_TILEWIDTH, tile_width);
      TIFFSetField(out, TIFFTAG_TILELENGTH, tile_height);
    }
  else
    {
      TIFFSetField(out, TIFFTAG_ROWSPERSTRIP, 1);
    }      

      
  /* Set width,length, and bands (called "samples" in TIFF) */
  zvget(inunit[0],"NL",nl,"NS",ns, NULL);

  TIFFSetField(out, TIFFTAG_IMAGEWIDTH,(uint32) *ns);
  TIFFSetField(out, TIFFTAG_IMAGELENGTH,(uint32) *nl);


  /* Name of the program creating this tiff file: */
  TIFFSetField(out, TIFFTAG_SOFTWARE, "VICAR Program VTIFF");

  /* Set the TIFF standard date-time */
  TIFFSetField(out, TIFFTAG_DATETIME, tiff_time());

  /* set the color type and color map, if any */
  if (zvptst( "chunky" ))
    TIFFSetField(out, TIFFTAG_PLANARCONFIG,PLANARCONFIG_CONTIG);
  else
    TIFFSetField(out, TIFFTAG_PLANARCONFIG,PLANARCONFIG_SEPARATE);
	  
  TIFFSetField(out, TIFFTAG_SAMPLESPERPIXEL, nb);

  if (nb==1)
    {
      if (numinp == 2)
	{
	  TIFFSetField(out, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_PALETTE);
	  SetTIFFClut(inunit,clut,"DiskFile");
	  TIFFSetField(out, TIFFTAG_COLORMAP, clut[0],clut[1],clut[2]);
	}
      else TIFFSetField(out, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_MINISBLACK);

    }
  else
    {
      TIFFSetField(out, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_RGB);
    }
}

void SetTIFFClut(int * unit, unsigned short clut[3][256], char *type) /* "Gray" or "Disk" */
{
  uint16 i,j,val;
  enum {red,green,blue};
  unsigned char idx_lut[256][4];
  int lutunit;
      
  switch (toupper(*type))
    {
    case 'G':
      for (i=0;i<256;i++)             /* default gray clut */
	{
	  val = i + (i<<8);
	  clut[red][i] = val;
	  clut[green][i] = val;
	  clut[blue][i] = val;
	}
      break;
    case 'D':				/* Disk File (IDX) clut */
      lutunit = unit[1];
	
      zvread( lutunit, idx_lut, NULL);
      for (j=0; j<3; j++)
	for (i=0;i<256;i++)
	  clut[j][i] = (int)257*idx_lut[i][j];
	
      zvclose( lutunit, NULL);
      break;
    default:
      AbortMsg("Unknown lookup table type");
    }

}


void WriteTIFFClut(TIFF *tif)
{
  int i,j;
  enum {red,green,blue};
  uint16* clut[3];
  unsigned char idx_lut[256][4];
  int lutunit;
  uint16 photo;

  TIFFGetField( tif, TIFFTAG_PHOTOMETRIC, &photo );
  if (photo==PHOTOMETRIC_PALETTE)
    {
      zvunit(&lutunit,"OUT",2, NULL);
      zvopen( lutunit, "op", "write", "u_nl", 1,
	      "u_ns", 1024, "u_format", "byte", NULL);
		
      TIFFGetField(tif, TIFFTAG_COLORMAP,
		   clut,clut+1,clut+2);
	
      /** create IDX format lut rgb0rgb0...**/
	
      memset( idx_lut, 0, 1024L );
      for (i=0; i<3; i++)
	for (j=0;j<256;j++)
	  idx_lut[j][i] = clut[i][j]>>8;
		
      zvwrit( lutunit, idx_lut, NULL);
      zvclose( lutunit, NULL);
    }
  else zvmessage( "*** no lookup table in TIFF ***", " ");
}


char *tiff_time()
{
  static char time_string[20];
  time_t cur_time;
  struct tm timv;

  cur_time = time(0L);
  timv = *localtime(&cur_time);
  sprintf(time_string,"%4d:%02d:%02d %02d:%02d:%02d",
	  1900+(timv.tm_year), 1+(timv.tm_mon), (timv.tm_mday),
	  (timv.tm_hour),   (timv.tm_min), (timv.tm_sec));
      
  return (time_string);
}

/* For JPL/NGT encoding, need to know where the NULL tiles are */
 
int GetNULLS(unsigned char **nulltiles)
{
  int unit;
  int nulltable;
  char maskname[200];
  int count;
  int numnulls;
  int maskcol;
  int nc;
  unsigned char *nulls;
  int status;
	
  zvp("NULLMASK",maskname,&count);
  if (!maskname[0] || maskname[0]==' ') return 0;
  status = zvunit(&unit, "xxx", 1, "U_NAME",maskname, NULL);
  if (status != 1) zvsignal(unit,status,1);

  zvp("MASKCOL",&maskcol,&count);
	
  status = IBISFileOpen( unit, &nulltable,IMODE_READ, 0,0,0,0 );
  if (status != 1) IBISSignalU(unit,status,1);
	
  count = IBISFileGet(nulltable,IFILE_NR,&numnulls,1,1,0);	
  if (count < 0) IBISSignal(nulltable,count,1);
	
  count = IBISFileGet(nulltable,IFILE_NC,&nc,1,1,0);	
  if (count < 0) IBISSignal(nulltable,count,1);

  if (maskcol>nc)
    {
      zvmessage("MASKCOL is not a valid column"," ");
      zabend();
    }
	
  nulls = (unsigned char *)malloc(sizeof(char)*numnulls);
  if (!nulls || numnulls <=0 )
    {
      zvmessage("memory error in GetNULLs()"," ");
      zabend();
    }
  *nulltiles = nulls;
	
  /* These are only 1/0 flags, so we can save a lot of memory */
  status = IBISColumnSet( nulltable,  ICOLUMN_U_FORMAT, 
			  IFMT_BYTE, maskcol );
  if (status != 1) IBISSignal(nulltable,status,1);

  /* just read the whole table in */
  status = IBISColumnRead(nulltable,(char *) nulls,maskcol,1,numnulls);

  IBISFileClose(nulltable,0);
	
  return numnulls;
}

int WriteTIFFData(int * inunit, TIFF *out, int nl, int ns, int nb)
{
  unsigned char *buf,*ptr,*lbuf[3];
  uint32 x,y,y1,z;
  int nsamps,nrows,pixsize;
  int tilebufsize;
  int tile_width,tile_height;
  int scanbytes;
  int band;
  int chunky;
  int i;

  zvget(inunit[0],"PIX_SIZE",&pixsize, NULL);
  chunky = zvptst( "chunky" ) && (nb>1);

  if (TIFFIsTiled(out))
    {
      unsigned char *nulltiles;
      int tilenum=0,numnulls=0;
		
      if (zvptst("JPL")) numnulls = GetNULLS(&nulltiles);

      TIFFGetField(out, TIFFTAG_TILEWIDTH, &tile_width);
      TIFFGetField(out, TIFFTAG_TILELENGTH, &tile_height);
      tilebufsize = TIFFTileSize(out);
      buf = (unsigned char *)malloc(tilebufsize);
      if (chunky)
	lbuf[0] = (unsigned char *)malloc(tilebufsize);
      else
	lbuf[0] = (unsigned char *)malloc(tilebufsize*nb);
      for (i=1;i<nb;i++) 
	lbuf[i] = lbuf[0]+ i*tile_width*tile_height*pixsize;
      if (!buf || !lbuf[0]) 
	AbortMsg("failed to allocate buffer for tiles");

      if (printstat) StartStatus(nl);
      for (y = 0; y < nl; y+=tile_height)
	{
	  if (printstat) UpdateStatus(y+tile_height);
	  nrows = MIN(tile_height, nl - y);
	  for (x = 0; x < ns; x+=tile_width)
	    {
	      /* read in a single tile */
	      memset(buf,0,(size_t)tilebufsize);
	      nsamps = MIN(tile_width, ns - x);
	      for (y1=0; y1< nrows; y1++)
		{
		  for (band=0;band<nb;band++)
		    {
		      zvread(inunit[band],lbuf[band]+
			     y1*pixsize*tile_width,
			     "line",y+y1+1, "samp",x+1,"nsamps", nsamps, NULL);
		    }
			    
		}
			
	      if (chunky)
		{
		  /* reshuffle pixels into chunky: rgbrgb...*/
		  ptr=buf;
		  for (z=0; z<tile_width*tile_height; z++)
		    for (band=0;band<nb;band++)
		      *ptr++ = lbuf[band][z];
		  if (tilenum < numnulls && !nulltiles[tilenum])
		    SetNULL(out);
		  tilenum++;
		  if (TIFFWriteTile2(out, buf, x,y,0,0) < 0)
		    goto bad;
		}
	      else /* interleave by tile */
		{
		  for (band=0; band<nb; band++)
		    {
		      if (tilenum < numnulls &&!nulltiles[tilenum])
			SetNULL(out);
		      tilenum++;
		      if (TIFFWriteTile2(out, lbuf[band], x,y,0,band) < 0)
			goto bad;
		    }
		}
	    }
	}
    }
  else                              /* TIFF IMAGE ORGANIZED IN STRIPS */
    {
      scanbytes = TIFFScanlineSize(out);
      buf = (unsigned char *)malloc(scanbytes);
      if (chunky)
	lbuf[0] = (unsigned char *)malloc(scanbytes);
      else
	lbuf[0] = (unsigned char *)malloc(scanbytes * nb);

      for (i=1;i<nb;i++) 
	lbuf[i] = lbuf[0]+ i*ns*pixsize;

      if (!buf || !lbuf[0]) AbortMsg("failed to allocate buffer for tiles");
            
      if (printstat) StartStatus(nl);
      for (y = 0; y < nl; y++)
	{
	  if (printstat) UpdateStatus(y+1);
	  for (band=0; band<nb; band++)
	    {
	      zvread(inunit[band],lbuf[band],"LINE",y+1, NULL);
	    }
	  if (chunky)
	    {
	      /* reshuffle pixels into chunky: rgbrgb...*/
	      ptr=buf;
	      for (z=0; z<ns; z++)
		for (band=0;band<nb;band++)
		  *ptr++ = lbuf[band][z];
	      if (TIFFWriteScanline2(out, buf,y,0) < 0)
		goto bad;
	    }
	  else   /* band-interleaved by line */
	    {
	      for (band=0; band<nb; band++)
		if (TIFFWriteScanline2(out, lbuf[band],y,band) < 0)
		  goto bad;
	    }
	
	}
    }
      
  free(buf);
  free(lbuf[0]);
  return (1);
 bad:
  free(buf);
  free(lbuf[0]);
  return (0);
}

static tsize_t 
TIFFReadTile2(TIFF*tif,
	      tdata_t buf, uint32 x, uint32 y, uint32 z, tsample_t s)
{
  if (sleeptime) sleep(sleeptime);
  return TIFFReadTile(tif,buf,x,y,z,s);
}
static tsize_t 
TIFFWriteTile2(TIFF*tif,
	       tdata_t buf, uint32 x, uint32 y, uint32 z, tsample_t s)
{
  if (sleeptime) sleep(sleeptime);
  return TIFFWriteTile(tif,buf,x,y,z,s);
}
static int
TIFFReadScanline2(TIFF* out,tdata_t buf, uint32 row, tsample_t s)
{
  if (sleeptime) sleep(sleeptime);
  return TIFFReadScanline(out,buf,row,s);
}
static int
TIFFWriteScanline2(TIFF* out,tdata_t buf, uint32 row, tsample_t s)
{
  if (sleeptime) sleep(sleeptime);
  return TIFFWriteScanline(out,buf,row,s);
}

static void SetNULL(TIFF *out)
{
}

static int maxstat=0;
static int curstat=0;
static void StartStatus(int maxval)
{
  maxstat=maxval;
  printf("%s",
	 "0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100%\n"
	 "|----|----|----|----|----X----|----|----|----|----|\n"
	 "|");	
}
static void UpdateStatus(int current)
{
  int newstat = (current*50)/maxstat;
  if (newstat > curstat)
    {
      for (;curstat<newstat;curstat++) printf(">");
      if (curstat==50) printf("\n");
    }
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create optimal_color2.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*
 *    TABLE: -- Convert 24 bit RGB images into 8 bit pseudocolor
 *
 *         Description:
 *
 *            The TABLE module allows the creation of table structures
 *            which may be used to gather color histogram data for an
 *            RGB image, compute an optimal color table, and convert
 *            the RGB into an index-color table pair.
 *
 *         Color conversion Algorithm:
 *
 *            Currently the TABLE module converts RGB into a 12-bit
 *            packed integer, by taking the first 4 bits from each of
 *            the RGB byte values. As colors are added to the palette of
 *            the table, a histogram is kept of each color. When the
 *            process of adding colors is complete, the colors are sorted
 *            and the most common are picked for the color table. In addition,
 *            the color white and black are added to the table for completeness.
 * 
 *            A lookup table is used to map the 12-bit integers to the correct
 *            color table index value. The table is initialized by computing
 *            the index values for just the colors used to compute the histogram,
 *            and others are added as requests are made to compute indices for
 *            new RGB triplets.
 *
 *            Eventually, the optimization will be performed on a packed integer
 *            based on the YCbCr (luminance-chrominance) model, using seven bits
 *            for the Y component, and 4 each for the chrominance. This will
 *            be transparent to the client program, as the subroutine interface
 *            will be the same.
 *
 *         Compatibility:
 *  
 *            This code should work on either 2-byte or 4-byte int compilers
 *            and does not depend upon byte order, except that the temporary
 *            files derived from table_pack_rgb will have the 2-byte byte-order
 *            of the native machine, and so should not be moved to other hosts.
 *
 *         Revision History:
 *
 *            Original program by	Alan Mazer	fall 1988
 *	      Modularized               Niles Ritter	2 June 1993
 *	      Add new routines:		Niles Ritter	15 June 1993
 *		table_ncolors
 *		table_increment_color
 */

#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>

#include "optimal_color2.h"

#ifndef NULL
#define NULL ((void*)0)
#endif

#define BITS_USED 4

/* useful macros */

#define RGB_INDEX( red,grn,blu )        \
       ( *(shift_left_red   + (red))  | \
         *(shift_left_green + (grn))  | \
         *(shift_left_blue  + (blu)) )
#define BYTE_COPY( src,dst,count) (memcpy((dst),(src),(count)))
#define BYTE_ZERO( src,count) (memset((src),0,(count)))

struct entry_def {
    unsigned char red,green,blue;    /* unpacked color: each ranges from 0 to 16 */
    unsigned int color;              /* packed color:  ranges from 0 to 4096    */
    long count;                      /* # times color found in sampling          */
};

struct table_def {
	struct entry_def *palette;   /* Palette of all colors found in image    */
	int *colormap;               /* Table mapping packed color to LUT index */
	int bits_used;               /* Number of bits used in packing (==4)    */
	int ncolors;                 /* Number of colors desired in LUT         */
	long length;                 /* Dimension of Palette (==4096)          */
};

/* public routines */
/*static public_routines(){}*/

/*
int  table_new();
void table_free();
void table_add_colors();
long table_build();
void table_extract_lut();
void table_rgb_to_index();
void table_packed_to_index();
void table_pack_rgb();
void table_increment_color();
int  table_ncolors();
*/


/* private routines */
/*static private_routines(){}*/
static void initialize_tables();
static long merge();
static long merge_new_index();
static int compare();
static int ShellSort();
void swap_entries();


/* private data */
/*static private_data(){}*/
static  int first_time=1;
static  long square[513];
static  unsigned int shift_left_red[256];
static  unsigned int shift_left_green[256];
static  unsigned int shift_left_blue[256];
static  unsigned int shift_left[256];



/**
 **  *****  PUBLIC IMPLEMENTATION ROUTINES  ******
 **/


/**
 **   table_new:
 **      creates new table structure.
 **      currently the table posesses a palette,
 **      used to gather the RGB color cube histogram,
 **      and a colormap array, which maps the packed
 **      RGB value to a sub-palette of <ncolors> colors.
 **
 **      Returns: 1 if successful, 0 if failure.
 **/



int table_new( struct table_def **tab, int ncolors )
{
    int bits2;
    int i;
    int *colormap;
    long length;
    register int index;
    struct table_def *table=(struct table_def *)0;
    struct entry_def *palette;
    unsigned char pix_red,pix_green,pix_blue;

    if (first_time) initialize_tables();
    if (ncolors > 256 || ncolors < 0) goto failure;
    if (ncolors == 0) ncolors = 256;
   
             /* useful values */

    bits2 = 1<<BITS_USED;
    length = bits2 * bits2 * bits2;
 
	    /* allocate space for color table */

    table = (struct table_def *)calloc( 1L, sizeof( struct table_def) );
    if (!table) goto failure;
   
    table->length = length;
    table->bits_used = BITS_USED;  /* currently hardwired to 5 */
    table->ncolors = ncolors;
       

	/* initialize palette */

    table->palette = (struct entry_def *)calloc( 1L,
          sizeof(struct entry_def) * length);
    if (!table->palette) goto failure;
    palette = table->palette;

    for (pix_red=0;(int)pix_red < bits2; pix_red++)
      for (pix_green=0;(int)pix_green < bits2; pix_green++)
        for (pix_blue=0; (int)pix_blue < bits2; pix_blue++) {
            index = pix_red << 2*BITS_USED | pix_green << BITS_USED | pix_blue;
            (palette+index)->color = index;
            (palette+index)->red = pix_red;
            (palette+index)->green = pix_green;
            (palette+index)->blue = pix_blue;
        }

           /* init colormap to -1: flags that color is not mapped yet */
           
    colormap = (int *)malloc( sizeof(int) * length);
    table->colormap = colormap;
    if (!table->colormap) goto failure;
    for (i=0;i<length;i++)
       *colormap++ = -1;


           /* set pointer and return success flag */
    
    *tab = table;
    return (1);
    
failure:
    table_free( table );
    return (0);
}



/**
 **   table_free: destruction method for table structure.
 **/

void table_free( struct table_def *table )
{
    if (table)
    {
    	if (table->palette) free(table->palette);
    	if (table->colormap) free(table->colormap);
    	free (table);
    }
}


/**
 ** table_add_colors - adds the specified arrays of RGB
 **  pixels to the palette, for eventual sorting and
 **  optimization.
 **/

void table_add_colors( struct table_def *table,
		       unsigned char *red,unsigned char *green,unsigned char *blue,
		       long npix )
{
    struct entry_def *palette = table->palette;
    register long pixel;
    register unsigned int color;
    register unsigned char *red_ptr,*green_ptr,*blue_ptr;
 
	/*
         *   augment the color histogram with new colors
	 */

    red_ptr = red;
    green_ptr = green;
    blue_ptr = blue;
    for (pixel=0;pixel<npix;pixel++) {
	color = RGB_INDEX( *red_ptr++, *green_ptr++, *blue_ptr++ );
	(palette+color)->count++;
    }

}



/*
 * table_increment_color:
 *    increment the histogram count of single color specified.
 */

void table_increment_color(table, red, green, blue, number)
    struct table_def *table;
    unsigned char red;
    unsigned char green;
    unsigned char blue;
    long number;
{
    unsigned int color;
    struct entry_def *palette = table->palette;

        color = RGB_INDEX(red, green, blue);
        (palette+color)->count += number;
}



/*
 *  table_ncolors:
 *    return the number of lut colors set in table_new
 */


int table_ncolors( table )
struct table_def *table;
{
	return( table->ncolors );
}




/**
 **   table_build: gather color gamut information collected
 **     from the (red,grn,blue) arrays, and set up an
 **     optimal <ncolors>-color mapping for that set of colors.
 **     returns max error distance in colorspace.
 **/

long table_build( struct table_def *table )
{
    int compare(),max_error;
    register long num_colors_found;
    struct entry_def *palette=table->palette;
    long ncolors=table->ncolors;
    long length=table->length;
    register long source;

	/* compress palette by tossing out all colors with zero counts */

    source = 1;                  /* dont toss out black at 0 */
    palette[length-1].count = 1; /* dont toss out white, either */
    for (num_colors_found = 1;;num_colors_found++) {
	while ((palette+source)->count == 0 && source < length) source++;
	if (source == length) break;
	BYTE_COPY((char *)(palette+source),(char *)(palette+num_colors_found),
	    sizeof(*palette));
	source++;
    }
 
	    /*
	     *   Exclude BLACK and WHITE in the gamut from
	     *   the histogram sorting process. The palette will
	     *   eventually look like this:
	     *
	     *   WHITE, p[1], p[2], ... , BLACK, <everybody else>.
	     *
	     *    XXX-should also include points of extreme color.
	     */

	    /* sort the remainder to determine colors to use */

    ShellSort((char *)(palette+1),num_colors_found-2,
         (long)sizeof(*palette),compare);





	    /* 
	     * Force the Lightest color into the table 
	     * and swap BLACK and WHITE.
	     */


    /* Add WHITE to table */
    swap_entries(palette, ncolors-1, num_colors_found-1);
    
    /* swap WHITE <-> BLACK */
    swap_entries(palette, 0L, ncolors-1);
        
	    /* merge infrequent colors to nearest class */

    max_error = merge(table,num_colors_found);

    return (max_error);
}

/**
 ** table_extract_lut:
 **  return the lookup table used, in a public format
 **/
  
void table_extract_lut( struct table_def *table,
			unsigned char *red_lut,unsigned char*green_lut,unsigned char*blue_lut )
{
    int dn;
    register int ncolors = table->ncolors;
    register struct entry_def *palette = table->palette;  
	/* determine lookup table */

    for (dn=0;dn < ncolors;dn++) {
	red_lut[dn]   = shift_left[ (palette+dn)->red   ];
	green_lut[dn] = shift_left[ (palette+dn)->green ];
	blue_lut[dn]  = shift_left[ (palette+dn)->blue  ];
    }
}


/**
 **  table_rgb_to_index
 **   Compress RGB array to indexed value.
 **/


void table_rgb_to_index( struct table_def *table,
			 unsigned char *red,unsigned char*green,unsigned char*blue,
			 long npix,
			 unsigned char *index )
{
    register long pixel;
    register long color;
    register unsigned char *red_ptr,*green_ptr,*blue_ptr;
    register int *ColorMap = table->colormap;
    

	/* create new red, green, and blue planes */

    red_ptr = red;
    green_ptr = green;
    blue_ptr = blue;
    for (pixel=0;pixel<npix;pixel++) {

        color = RGB_INDEX( *red_ptr++, *green_ptr++, *blue_ptr++ );
	 
                /* If table hasn't seen this; add to ColorMap */
                
	if (*(ColorMap+color) < 0) merge_new_index( table, color );
	    
	*(index+pixel) = *(ColorMap+color);
    }
}


/**
 **  table_packed_to_index
 **   Compress 2-byte packed color to indexed value.
 **/


void table_packed_to_index( table, pack, npix, index )
struct table_def *table;
unsigned short *pack;
long npix;
unsigned char *index;
{
    register long pixel;
    register long color;
    register unsigned short *color_ptr=pack;
    register int *ColorMap = table->colormap;
    

	/* create new red, green, and blue planes */

    for (pixel=0;pixel<npix;pixel++) {

        color = *color_ptr++;
	 
                /* If table hasn't seen this; add to ColorMap */
                
	if (*(ColorMap+color) < 0) merge_new_index( table, color );
	    
	*index++ = *(ColorMap+color);
    }
}


/**
 **  table_pack_rgb
 **   Compress RGB array to 2-byte packed representaton.
 **/


void table_pack_rgb( table, red, green, blue, npix, pack )
struct table_def *table;
unsigned char *red,*green,*blue;
long npix;
unsigned short *pack;
{
    register long pixel;
    register unsigned char *red_ptr,*green_ptr,*blue_ptr;
    
    red_ptr = red;
    green_ptr = green;
    blue_ptr = blue;
    for (pixel=0;pixel<npix;pixel++) {

        *pack++ = RGB_INDEX( *red_ptr++, *green_ptr++, *blue_ptr++ );

    }
}


/**
 **  *****  PRIVATE IMPLEMENTATION ROUTINES  ******
 **/

/**
 **  swap_entries
 **    swaps  two color entries in a palette
 **/


void swap_entries(palette, index1, index2 )
struct entry_def *palette;
long index1;
long index2;
{
   struct entry_def temp;
  
   BYTE_COPY( palette+index1, &temp,          sizeof(temp));
   BYTE_COPY( palette+index2, palette+index1, sizeof(temp));
   BYTE_COPY( &temp,          palette+index2, sizeof(temp));
}



/**
 **  initialize_tables:
 **   sets up data arrays for module.
 **/

static void initialize_tables()
{
    register long int_temp;
 
    if (!first_time) return;
 
	/* make table for squares of differences */

    for (int_temp = -256;int_temp <= 256;int_temp++)
	square[int_temp+256] = int_temp*int_temp;

	/* make tables for right and left shifts */

    for (int_temp = 0;int_temp < 256;int_temp++) {
	shift_left_red[int_temp] =   (int_temp >> (8-BITS_USED)) << (2*BITS_USED);
	shift_left_green[int_temp] = (int_temp >> (8-BITS_USED)) << BITS_USED;
	shift_left_blue[int_temp] =  (int_temp >> (8-BITS_USED));
	
	shift_left[int_temp] =       (int_temp << (8-BITS_USED)) | int_temp;

    }
    
    first_time=0;
}



    /* Merge - takes color table generated by Build_Table and merges */
    /* infrequently used colors in with most popular.                */

static long merge(table,num_colors)
struct table_def *table;
long num_colors;
{
    int best_dist;
    long unclassed_index;
    int j,max_error,ncolors=table->ncolors;
    register int *ColorMap = table->colormap;
    register struct entry_def *palette=table->palette;

	/* Initialize ColorMap output colors */

    for (j=0;j<ncolors;j++)
	*(ColorMap+ (palette[j].color) ) = j;

	/* do merge */

    max_error = 0;
    for (unclassed_index=ncolors;unclassed_index < num_colors;
	    unclassed_index++) {
	
	/*
	 * XXX for optimal performance, should #define a
	 * common loop for merge_new_index and this routine
	 * to save the stack-frame overhead.
	 */

	best_dist = merge_new_index(table, unclassed_index );
	if (best_dist > max_error) max_error = best_dist;

    }
    return(max_error);
}

/*
 * merge_new_index:
 *  merges a single color into the preset color table
 *  by finding the best matching color and then setting the
 *  new indexes colormap value to the best match.
 */

static long merge_new_index(table, unclassed_index )
struct table_def *table;
long unclassed_index;
{
    int best_index;
    long best_dist,dist;
    int j,ncolors=table->ncolors;
    register int *ColorMap = table->colormap;
    register long *square_off;
    register struct entry_def *palette=table->palette,*pal_ptr;
    register int red_diff,green_diff,blue_diff;
    register unsigned char pix_red,pix_green,pix_blue;

    square_off = &square[256];  /* squares of differences */

    /* do merge */
	
    pix_red = palette[unclassed_index].red;
    pix_green = palette[unclassed_index].green;
    pix_blue = palette[unclassed_index].blue;
    red_diff = pix_red - palette->red;
    green_diff = pix_green - palette->green;
    blue_diff = pix_blue - palette->blue;
    best_dist = *(square_off+red_diff) + *(square_off+green_diff) +
        *(square_off+blue_diff);
    best_index = 0;
    pal_ptr = palette+1;
    for (j=1;j < ncolors;j++) {
        red_diff = pix_red - pal_ptr->red;
        green_diff = pix_green - pal_ptr->green;
        blue_diff = pix_blue - pal_ptr->blue;
        dist = *(square_off+red_diff) + *(square_off+green_diff) +
           *(square_off+blue_diff);
        if (dist < best_dist) {
           best_dist = dist;
           best_index = j;
        }
        pal_ptr++;
    }
    
    *(ColorMap+(palette+unclassed_index)->color) = best_index;
    (palette+best_index)->count += (palette+unclassed_index)->count;
  
    return(best_dist);
}


/*
 * compare:
 *    ordering routine for sorting color entries by 
 * histogram count.
 */

static int compare(struct entry_def *entry1,struct entry_def *entry2)
{
    int return_value;
    return_value = (entry1->count < entry2->count? 1:
	(entry1->count == entry2->count? 0:-1));
    return(return_value);
}

/*
 * ShellSort:
 *  A standard array-sorting algorithm.
 */

static int ShellSort(start, nelem, size, compar)
  char *start;		/* starting addr for sort	*/
  long	nelem;		/* number of elements to sort	*/
  long	size;		/* size of one element in bytes	*/
  int	(*compar)();	/* addr of compare routine	*/
{
  int	i,j,h;
  char  *v;

  v = malloc(size);
  if (v == 0) return 0;

  for (h = 1; h <= nelem; h = 3 * h + 1);
  while (h >= 3)
  {
    h /= 3;
    for (i = h; i < nelem; i++)
    {
      BYTE_COPY(start + size * i, v, size);
      j = i;
      while ((*compar)(start + size * (j - h), v) > 0)
      {
	BYTE_COPY(start + size * (j - h), start + size * j, size);
	j -= h;
	if (j < h) break;
      }
      BYTE_COPY(v, start + size * j, size);
    }
  }
  
  free (v);
  return 1;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create optimal_color2.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/**
 **   table_new:
 **      creates new table structure.
 **      currently the table posesses a palette,
 **      used to gather the RGB color cube histogram,
 **      and a colormap array, which maps the packed
 **      RGB value to a sub-palette of <ncolors> colors.
 **
 **      Returns: 1 if successful, 0 if failure.
 **/

struct table_def;

int table_new( struct table_def **tab, int ncolors );

/**
 ** table_add_colors - adds the specified arrays of RGB
 **  pixels to the palette, for eventual sorting and
 **  optimization.
 **/

void table_add_colors( struct table_def *table,
		       unsigned char *red,unsigned char *green,unsigned char *blue,
		       long npix );

/**
 **   table_free: destruction method for table structure.
 **/

void table_free( struct table_def *table );

/**
 **   table_build: gather color gamut information collected
 **     from the (red,grn,blue) arrays, and set up an
 **     optimal <ncolors>-color mapping for that set of colors.
 **     returns max error distance in colorspace.
 **/

long table_build( struct table_def *table );

/**
 **  table_rgb_to_index
 **   Compress RGB array to indexed value.
 **/


void table_rgb_to_index( struct table_def *table,
			 unsigned char *red,unsigned char*green,unsigned char*blue,
			 long npix,
			 unsigned char *index );

/**
 ** table_extract_lut:
 **  return the lookup table used, in a public format
 **/
  
void table_extract_lut( struct table_def *table,
			unsigned char *red_lut,unsigned char*green_lut,unsigned char*blue_lut );

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create uFixedNums.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* Module uFixedNums implementation ==========================================*/


/* Public definitions ========================================================*/
#include "uFixedNums.h"	/* module's public interface */


/* Private definitions =======================================================*/

/* Links to the outside */
#include <limits.h>

#define FALSE 0
#define TRUE 1

#ifdef Examples
1. Convert from a double to a double fixed and a fixed and back again.

	#include <stdio.h>
	#include "uFixedNums.h"

#include "uFixedNums.proto.h"

	main (void)
	{
		Boolean success;
		Fixed aFixedValue;
		DoubleFixed aDoubleFixedValue;
		double aDoubleValue, a2ndDouble;
		FILE *pOut;


		pOut = fopen ("test fixed report", "w");
	
		aDoubleValue = 12345.067895;
		success = ConvertDoubleToDoubleFixed (&aDoubleValue, &aDoubleFixedValue);
		fprintf (pOut, "\n%.12f to dfixed: (%d), 0x%lx,0x%lx\n", aDoubleValue,
			success, aDoubleFixedValue.integerPart,
			aDoubleFixedValue.fractionalPart);
		ConvertDoubleFixedToDouble (&aDoubleFixedValue, &a2ndDouble);
		fprintf (pOut, "    and back: %.12f\n", a2ndDouble);
		success = ConvertDoubleToFixed (&aDoubleValue, &aFixedValue);
		fprintf (pOut, "%.12f to fixed: (%d), 0x%lx\n", aDoubleValue, success,
			aFixedValue);
		ConvertFixedToDouble (&aFixedValue, &a2ndDouble);
		fprintf (pOut, "    and back: %.12f\n", a2ndDouble);

		fclose (pOut);

	}  /* main */

#endif


/* Routine implementations ===================================================*/


void
ConvertDoubleFixedToDouble (
	DoubleFixed *pFixedValue,
	double *pDoubleValue
) {
	Boolean negative;
	unsigned long fractionalConverter;


/* Set up number as positive */
	if ((negative = (pFixedValue->integerPart < 0)))
		pFixedValue->integerPart = -pFixedValue->integerPart;
	fractionalConverter = (unsigned long)0xFFFFFFFF;

/* Convert */
	*pDoubleValue = (double)pFixedValue->integerPart +
		((double)pFixedValue->fractionalPart / (double)fractionalConverter);

/* Restore sign */
	if (negative) {
		pFixedValue->integerPart = -pFixedValue->integerPart;
		*pDoubleValue = -(*pDoubleValue);
	}

}  /* ConvertDoubleFixedToDouble */


Boolean
ConvertDoubleToDoubleFixed (
	double *pDoubleValue,
	DoubleFixed *pFixedValue
) {
	Boolean negative;
	unsigned long fractionalConverter;


/* Check for overflow */
	if (*pDoubleValue < LONG_MIN  ||  LONG_MAX < *pDoubleValue) {
		pFixedValue->integerPart = 0;
		pFixedValue->fractionalPart = 0;
		return (FALSE);
	}

/* Set up number as positive */
	if ((negative = (*pDoubleValue < 0.0)))
		*pDoubleValue = -(*pDoubleValue);
	fractionalConverter = (unsigned long)0xFFFFFFFF;

/* Convert */
	pFixedValue->integerPart = (long)(*pDoubleValue);
	pFixedValue->fractionalPart = (unsigned long)
		((*pDoubleValue - pFixedValue->integerPart) * fractionalConverter);

/* Restore sign */
	if (negative) {
		*pDoubleValue = -(*pDoubleValue);
		pFixedValue->integerPart = -pFixedValue->integerPart;
	}
	return (TRUE);

}  /* ConvertDoubleToFixed */


#if 0
Boolean
ConvertDoubleToFixed (
	double *pDoubleValue,
	Fixed *pFixedValue
) {
	Boolean negative;
	short integerPart;
	unsigned short fractionalConverter;


/* Check for overflow */
	if (*pDoubleValue < SHRT_MIN  ||  SHRT_MAX < *pDoubleValue) {
		*pFixedValue = 0;
		return (FALSE);
	}

/* Set up number as positive */
	if (negative = *pDoubleValue < 0.0)
		*pDoubleValue = -(*pDoubleValue);
	fractionalConverter = (unsigned short)0xFFFF;

/* Convert */
	integerPart = (short)(*pDoubleValue);
	*pFixedValue = ((long)integerPart << 16)  |  (unsigned short)
		((*pDoubleValue - integerPart) * fractionalConverter);

/* Restore sign */
	if (negative) {
		*pDoubleValue = -(*pDoubleValue);
		*pFixedValue = -(*pFixedValue);
	}
	return (TRUE);

}  /* ConvertDoubleToFixed */


void
ConvertFixedToDouble (
	Fixed *pFixedValue,
	double *pDoubleValue
) {
	Boolean negative;
	unsigned short fractionalConverter;


/* Set up number as positive */
	if (negative = *pFixedValue < 0)
		*pFixedValue = -(*pFixedValue);
	fractionalConverter = (unsigned short)0xFFFF;

/* Convert */
	*pDoubleValue = (double)((*pFixedValue) >> 16) +
		((double)((*pFixedValue) & 0xFFFF) / (double)fractionalConverter);

/* Restore sign */
	if (negative) {
		*pFixedValue = -(*pFixedValue);
		*pDoubleValue = -(*pDoubleValue);
	}

}  /* ConvertFixedToDouble */
#endif

/* uFixedNums.c ==============================================================*/

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create uFixedNums.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/* Module uFixedNums public interface ========================================*/

#ifndef header_uFixedNums /*==================================================*/
#define header_uFixedNums


/* Data types */
typedef struct {
/* Reference: IM I-79 */
	long integerPart;
	unsigned long fractionalPart;
} DoubleFixed;


#define Boolean int

/* Routines */
extern void ConvertDoubleFixedToDouble (DoubleFixed *pFixedValue,
	double *pDoubleValue);
extern Boolean ConvertDoubleToDoubleFixed (double *pDoubleValue,
	DoubleFixed *pFixedValue);
/*  extern Boolean ConvertDoubleToFixed (double *pDoubleValue, Fixed *pFixedValue); */
/*  extern void ConvertFixedToDouble (Fixed *pFixedValue, double *pDoubleValue); */


#endif /* uFixedNums.h =======================================================*/
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create vtiff3o.pdf
process help=*
SUBCMD TOVIC	! convert from TIFF to vicar
    PARM INP	TYPE=STRING COUNT=1
    PARM OUT	TYPE=STRING COUNT=0:4 default=--
    PARM AUXIL  TYPE=STRING COUNT=0:2 DEFAULT=--
    PARM INFO   TYPE=KEYWORD VALID=(DUMP,NODUMP) DEFAULT=NODUMP
    PARM FORMAT TYPE=KEYWORD VALID=(BYTE,HALF,FULL,REAL,DOUB,COMP,SAME)  +
    		DEFAULT=SAME
!	PARM SIZE	TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
!	PARM SL		TYPE=INTEGER COUNT=1 DEFAULT=1
!	PARM SS		TYPE=INTEGER COUNT=1 DEFAULT=1
	PARM NL		TYPE=INTEGER COUNT=1 DEFAULT=0
	PARM NS		TYPE=INTEGER COUNT=1 DEFAULT=0
	PARM METADATA	TYPE=STRING DEFAULT=" "
    PARM STATUS		TYPE=KEYWORD VALID=(PRINT,NOPRINT) DEFAULT=PRINT
END-SUBCMD

SUBCMD-DEFAULT FROMVIC	! convert from vicar to TIFF
    PARM INP 		TYPE=STRING COUNT=1:3
    PARM OUT 		TYPE=STRING COUNT=1
    PARM AUXIL 		TYPE=STRING COUNT=0:2 DEFAULT=--
    PARM TLINES		TYPE=INTEGER DEFAULT=128
    PARM TSAMPS		TYPE=INTEGER DEFAULT=128
    PARM COMPRESS	TYPE=KEYWORD VALID=(LZW,PACKBITS,JPL,ZIP,JPG,NOCOMP)+
      DEFAULT=NOCOMP
    PARM ZIPCOMP        TYPE=INTEGER DEFAULT=1 VALID=(1,2,3,4,5,6,7,8,9)
    PARM COLORBITS  TYPE=KEYWORD VALID=(BIT8,BIT24) DEFAULT=BIT24
    PARM TIFFMODE	TYPE=KEYWORD VALID=(TILED,STRIPS) DEFAULT=STRIPS
    PARM INTRLEAV	TYPE=KEYWORD VALID=(CHUNKY,PLANAR) DEFAULT=CHUNKY
    PARM LAT		TYPE=REAL valid=(0:90) COUNT=0:3 DEFAULT=--
    PARM HEMILAT	TYPE=KEYW VALID=(NORTH,SOUTH) DEF=NORTH
    PARM LONG		TYPE=REAL valid=(0:180) COUNT=0:3 DEFAULT=--
    PARM HEMILONG	TYPE=KEYW VALID=(EAST,WEST) DEF=EAST
    PARM HDEGPIX	TYPE=REAL COUNT=0:3 DEFAULT=(0,0,1.0)
    PARM VDEGPIX	TYPE=REAL COUNT=0:3 DEFAULT=(0,0,1.0)
    PARM XPIXPOS	TYPE=REAL COUNT=1 DEFAULT=0.0
    PARM YPIXPOS	TYPE=REAL COUNT=1 DEFAULT=0.0
    PARM RESUNIT	TYPE=KEYWORD VALID=(INCH,CENTIMETER) DEF=INCH
    PARM XRES      	TYPE=REAL COUNT=1 DEF=72.0
    PARM YRES		TYPE=REAL COUNT=1 DEF=72.0
	PARM NULLMASK	TYPE=STRING DEFAULT=" "
	PARM METADATA	TYPE=STRING DEFAULT=" "
	PARM GCS		TYPE=KEYWORD VALID=(WGS84,WGS72,NAD83,NAD27) DEFAULT=WGS84
    PARM JPLCOMP	TYPE=KEYWORD VALID=(ZIPJPL,NGT) DEFAULT=ZIPJPL
    PARM MASKCOL	TYPE=INTEGER COUNT=1 valid=(1:1024) DEFAULT=1
    PARM STATUS		TYPE=KEYWORD VALID=(PRINT,NOPRINT) DEFAULT=PRINT
	
	! Added to enable Albers GeoTIFF
	PARM PROJ		TYPE=KEYWORD VALID=(LATLONG,ALBERS,LAMBCC) DEFAULT=LATLONG
	PARM PIXSIZE    TYPE=REAL COUNT=2 DEFAULT=(1.0,1.0)
	PARM STDPARAL	TYPE=REAL COUNT=2 DEFAULT=(0,0)
	PARM PIX_ORIG   TYPE=REAL COUNT=2 DEFAULT=(0.0,0.0)
	PARM FAL_ORIG   TYPE=REAL COUNT=2 DEFAULT=(0.0,0.0)
	PARM FO_LAT	TYPE=REAL valid=(-90:90) COUNT=1 DEFAULT=0
	PARM NO_LONG	TYPE=REAL valid=(-180:180) COUNT=1 DEFAULT=0
	PARM CARTTAG	TYPE=KEYWORD valid=(OLD,NONE) default=OLD

END-SUBCMD

end-proc
.TITLE
VICAR2 Program VTIFF
.HELP
   VTIFF is a Vicar2 program, which can be used to convert
   between VICAR labeled images and TIFF format files, using
   either scanline (strip) organization, or the newer Revision
   6.0 TIFF tiled-image format. Currently grayscale, image-
   lookup table pairs and RGB tripletes are supported. In addition,
   multiple auxiliary images may be placed in the same file,
   such as "thumbnail" preview images. GeoTIFF 1.0 georeferencing
   parameters may also be inserted for Geographic and Albers.

   Need to add:
     sinusoidal
     utm
     lambert
     polar stereographic

     plus other datums

   "TIFF" is a trademark of Aldus Corporation, and is a public
   standard for platform-independent raster-image storage and
   transmission. It is an acronym for Tagged Image File Format.

.PAGE
CALLING SEQUENCE:

	vtiff-tovic   image.tiff  image 	  AUXIL=(files...)
	vtiff-tovic   image.tiff  (image, lookup) AUXIL=(files...)
	vtiff-tovic   image.tiff  (red, grn, blu) AUXIL=(files...)

	vtiff-fromvic  image  		image.tif  [options..]
	vtiff-fromvic  (image, lookup)  image.tif  [options..]
	vtiff-fromvic  (red, grn, blu)  image.tif  [options..]

   where image.tiff is TIFF format, and lookup is a
   color lookup table in IDX format (1024 sample, 1 line
   byte image, with values r1,g1,b1,0,r2,g2,b2,0,...).


.PAGE
OPERATION

  In the "tovicar" mode, the program unpacks the image and
  lookup table, if desired. It will also unpack any additional
  files and save them as specified by the AUX parameter.

  In the "fromvicar" mode, you have the option of saving the
  files in strip-mode (horizontal scanlines contiguous), which
  makes them TIFF 5.0 compliant; or using the TIFF 6.0 tiling
  option, which breaks the image up into rectangular tiles, and
  saves those in a contiguous manner.

  You may save multiple additional images in the same file using
  the "AUX" parameter. If the file uses a lookup table, you may
  include this in the second INP parameter. It currently must be
  an IDX-format lookup table (1 line x 1024 sample byte image:
  r1,g1,b1,0,r2..). NOTE: Tiling is a new feature, which many
  TIFF programs cant yet handle. If so, use the default strip-mode. 
.PAGE
OPERATION

  You may also chose a TIFF data compression option. The
  currently supported keywords in VTIFF are 'LZW = lempel-ziv-welch
  compression, 'PACKBITS = Macintosh-style packbits (run-length
  encoding) compression, or 'NOCOMP = no compression. NOTE: The
  TIFF 6.0 standard now discourages the use of LZW, as Unisys Corp.
  claims to have a patent on the algorithm, and so may not
  support LZW in the future. If this is a concern, you can try
  the 'ZIP Deflation compression, which does not have any
  limitations.
.page
OPERATION

  The 'JPL compression by default implies the 'ZIPJPL codec, which
  uses a modified ZIP/Deflate compression scheme, allowing for NULL's.
  An optional NULLMASK parameter allows the specification of tiles
  which are all zero, permitting faster,better,cheaper compression
  on those tiles. The NULLMASK file should be an IBIS tabular file 
  containing 0's on those row number corresponding
  to NULL tiles. Use the MASKCOL parameter to specify which column
  contains the NULL information.

  In this version a ('JPL 'NGT) compression is also enabled, 
  supporting National Geographic Television/JPL compression support.
  this mechanism operates in a similar mode to the 'ZIPJPL scheme,
  but is patented and not for general use.
.PAGE
OPERATION

  For RGB triplets, whether tiled or in strips, you may choose
  to organize the data using 'CHUNKY or 'PLANAR interleaving.
  The 'CHUNKY format is analogous to the VICAR BIP (Band-interleaved
  by pixel) organization, and is the default. The 'PLANAR is
  similar to BSQ (Band-sequential) format, and while this permits
  faster conversion and extraction, it is an extension to TIFF
  that is not supported by as many TIFF-compatible programs.
.PAGE
OPERATION

  The latitude-longitude parmeters are extensions to TIFF to
  permit specification of scanned map georeference data. These
  extensions are GeoTIFF 1.0-compliant, and will not interfere with
  any standard TIFF-reading program, which will ignore the 
  extended data fields. For more information on the cartographic
  TIFF extensions, see the GeoTIFF web page, and "HELP PROJ".

  In general, for the most easily exportable TIFF file, use as few
  of the parameters as possible. 

.PAGE

REVISION HISTORY

   Written by:            N. D. Ritter  September 1991
   Cognizant Programmer:  W Bunch

   2003-09-05 WLB Added GeoTIFF support.
   2004-07-12 WLB Added legacy cart tag support.
   2004-10-19 WLB Added support for fourth input TIFF band.
   2004-12-01 WLB Added support for non GeoTIFF metata (to/from TIFF image description tag).
   2005-05-11 WLB Added support for new tags
   2005-06-01 WLB Bug fix
   2006-11-21 WLB strcpy/cat and ms_dnum bugs fixed
   2008-01-03 WLB Switched to USES_ANSI_C AND LIB_CARTO; misc cleanup
   2015-10-14 WLB Migrated to MIPL


REFERENCES

   "TIFF" Revision 6.0, Final - Jun 3, 1992,
      Aldus Developers Desk, available via anonymous ftp
      through sgi.com.
.PAGE
AKNOWLEDGMENT

   This program is a VICAR front-end to a public-domain
   subroutine library of TIFF file format routines, written
   by Sam Leffler, and extended for JPL use by Niles Ritter.
   The source code carries the following copyright notice:

   <quote>
   Copyright (c) 1988, 1989, 1990, 1991, 1992 Sam Leffler
   Copyright (c) 1991, 1992 Silicon Graphics, Inc.
 
   Permission to use, copy, modify, distribute, and sell this software and 
   its documentation for any purpose is hereby granted without fee, provided
   that (i) the above copyright notices and this permission notice appear in
   all copies of the software and related documentation, and (ii) the names of
   Sam Leffler and Silicon Graphics may not be used in any advertising or
   publicity relating to the software without the specific, prior written
   permission of Sam Leffler and Silicon Graphics.
.PAGE
AKNOWLEDGMENT
   
   THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, 
   EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY 
   WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  
   
   IN NO EVENT SHALL SAM LEFFLER OR SILICON GRAPHICS BE LIABLE FOR
   ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
   OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
   WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF 
   LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
   OF THIS SOFTWARE.

   <unquote>.

.LEVEL1


.SUBCMD TOVIC
Convert TIFF format file
to VICAR.
.VAR INP -TOVIC
Input TIFF file
.VAR OUT -TOVIC
Output VICAR file.
.VAR AUXIL -TOVIC
Auxiliary files to pull
out of TIFF file.
.VAR INFO -TOVIC
Dump information only.
.VAR SIZE -TOVIC
.VAR SL -TOVIC
.VAR SS -TOVIC
.VAR NL -TOVIC
.VAR NS -TOVIC
.VAR METADATA -TOVIC
Dump GeoTIFF to file
.VAR STATUS -TOVIC
Print Progress Bar?

.SUBCMD FROMVIC
Covert from VICAR image
to TIFF format file.
.VAR INP -FROMVIC
Input VICAR file
.VAR OUT -FROMVIC
Output TIFF file
.VAR AUXIL -FROMVIC
Auxiliary files to stuff
into output TIFF file.
.VAR TLINES -FROMVIC
Number of lines in tile
.VAR TSAMPS -FROMVIC
Number of samps in tile
.VAR COMPRESS -FROMVIC
Compression type
.VAR COLORBITS  -FROMVIC 
Convert RGB to 8-bit?
.VAR TIFFMODE -FROMVIC
Use Strips or Tiles ?
.VAR INTRLEAV -FROMVIC
Chunky Interleave:rgbrgb...?
.VAR LUT -FROMVIC
Type of Color Lookup 
Table to use.
.VAR LAT -FROMVIC
Ref. Latitude in (D,Min,Sec).
.VAR HEMILAT -FROMVIC
North or South Latitude ?
.VAR LONG -FROMVIC
Ref. Longitude in (D,Min,Sec).
.VAR HEMILONG -FROMVIC
East or West Longitude ?
.VAR HDEGPIX -FROMVIC
Horiz. (D,Min,Sec) per pixel.
.VAR VDEGPIX -FROMVIC
Vert. (D,Min,Sec) per pixel.
.VAR XPIXPOS -FROMVIC
X-Position of Coord. axis.
.VAR YPIXPOS -FROMVIC
Y-Position of Coord. axis.
.VAR RESUNIT -FROMVIC
Units used in X/YRES
.VAR XRES -FROMVIC
#pixels per (RESUNIT) horiz.
.VAR YRES -FROMVIC
#pixels per (RESUNIT) vert.
.VAR GCS -FROMVIC
Geog. Coord. System Used
.VAR NULLMASK -FROMVIC
IBIS table flagging NULL tiles
(0 = NULL, 1 = valid data)
.VAR MASKCOL -FROMVIC
Column containing NULL flag
.VAR STATUS -FROMVIC
Print Progress Bar?
.VAR PROJ -FROMVIC
Lat-Long, Albers...?
.VAR PIXSIZE -FROMVIC
horiz,vert Pixel Size (m)
.VAR STDPARAL -FROMVIC
Standard Parallels (degrees)
.VAR NAT_ORIG -FROMVIC
Natural Origin (E,N)
.VAR FAL_ORIG -FROMVIC
False Origin (E,N)
.VAR FO_LAT -FROMVIC
False Origin Latitude
.VAR NO_LONG -FROMVIC
Natural Origin Longitude
(aka Central Meridian)
.VAR CARTTAG -FROMVIC
Include Old Carto tags?

.level2

.SUBCMD TOVIC
Convert TIFF format file
to VICAR.
.VAR INP -TOVIC
Input TIFF file
.VAR OUT -TOVIC
Output VICAR file.
.VAR AUXIL -TOVIC
Auxilary files to pull
out of TIFF file.
.VAR INFO -TOVIC
Dump information only.
.VAR FORMAT -TOVIC
Output format.
.VAR NL -TOVIC
Number of Lines
.VAR NS -TOVIC
Number of Samples
.VAR METADATA -TOVIC
Dump GeoTIFF to file

.SUBCMD FROMVIC
Covert from VICAR image
to TIFF format file.
.VAR INP -FROMVIC
Input VICAR file
.VAR OUT -FROMVIC
Output TIFF file
.VAR AUXIL -FROMVIC
Auxilary files to stuff
into output TIFF file.
.VAR TLINES -FROMVIC
Number of lines in tile.
.VAR TSAMPS -FROMVIC
Number of samps in tile.
.VAR COMPRESS -FROMVIC
Use Compression ?
.VAR COLORBITS  -FROMVIC 
When an (r,g,b) triplet is input, tells VTIFF whether the
output TIFF file should be 8-bit ('BIT8) or 24-bit (BIT24) color .
.VAR TIFFMODE -FROMVIC
Use Strips or Tiles ?
.VAR INTRLEAV -FROMVIC
Chunky Interleave:rgbrgb...?
.VAR LUT -FROMVIC
Type of Color Lookup 
Table to use.

.VAR LAT -FROMVIC
Ref. Latitude in DMS.
.VAR LONG -FROMVIC
Ref. Longitude in DMS.
.VAR HEMILAT -FROMVIC
North or South latitude?
.VAR HEMILONG -FROMVIC
East or West longitude?
.VAR HDEGPIX -FROMVIC
Horiz. (D,Min,Sec) per pixel.
.VAR VDEGPIX -FROMVIC
Vert. (D,Min,Sec) per pixel.
.VAR XPIXPOS -FROMVIC
X-Position of Coord. axis.
.VAR YPIXPOS -FROMVIC
Y-Position of Coord. axis.
.VAR RESUNIT -FROMVIC
Units used in X/YRES to expression resolution
of displayed map/image.
.VAR XRES -FROMVIC
#pixels per (RESUNIT) horizontal.
.VAR YRES -FROMVIC
#pixels per (RESUNIT) vertical.
.VAR NULLMASK -FROMVIC
IBIS table flagging NULL tiles
(0 = NULL, 1 = valid data)
.VAR MASKCOL -FROMVIC
Column containing NULL flag
.VAR PROJ -FROMVIC
Allows choice of Geographic (Lat-long) coordinate system, or
a projected coordinate system such as Albers Equal Area.

For Albers Equal Area the following georeferencing approach
should be used:

First, define the cartographic projected coordinate system. Use
the NO_LONG parameter to define the central meridian (in decimal
degrees), and the STDPARAL parameter to define the two standard
parallels. There is no natural origin latitude, so instead we
choose a latitude for a false origin (given by FO_LAT), and assign it an
arbitrary Easting,Northing ( given by FAL_ORIG).

The most common choice for the False origin is a parallel which hits
the top or bottom of the image tangentially (at the center of image),
for which the latitude is known.

Next, the raster XY pixel-space should be tied to this easting,northing
coordinate system. The recommended way to do this is to set the
pixel origin (PIX_ORIG) easting,northing equal to the coordinate
system false origin (PIX_ORIG = FAL_ORIG), and then specify the
pixel (X,Y) location of of this origin in the image. Also, use the
PIXSIZE parameter to specify the horizontal and vertical size of
the pixels in meters. In the XY coordinate system the location of
the upper-left corner of the upper-left pixel is (0,0), and the
bottom-right corner of the bottom-right pixel is (NS+1, NL+1).


.VAR PIXSIZE -FROMVIC
horiz,vert Pixel Size (m)

  *** See "HELP PROJ" *** 
  
.VAR STDPARAL -FROMVIC
Standard Parallels (degrees)

  *** See "HELP PROJ" *** 
  
.VAR NAT_ORIG -FROMVIC
Natural Origin (E,N)

  *** See "HELP PROJ" *** 
  
.VAR FAL_ORIG -FROMVIC
False Origin (E,N)

  *** See "HELP PROJ" *** 
  
.VAR FO_LAT -FROMVIC
False Origin Latitude

  *** See "HELP PROJ" *** 
  
.VAR NO_LONG -FROMVIC
Natural Origin Longitude
(aka Central Meridian)

  *** See "HELP PROJ" *** 
  

.END
$ Return
$!#############################################################################
$Test_File:
$ create tstvtiff3o.pdf
procedure
refgbl $echo
refgbl $autousage
body

let $echo="yes"
let $autousage="none"

write "each of the difpic calls should yield zero differences"

gen r 50 50
vtiff3o r r.tif
vtiff3o-tovic r.tif r1
difpic (r,r1)

vtiff3o r r.tif 'packbits
vtiff3o-tovic r.tif r1
difpic (r,r1)

vtiff3o r r.tif 'lzw
vtiff3o-tovic r.tif r1
difpic (r,r1)

vtiff3o r r.tif 'tiled
vtiff3o-tovic r.tif r1
difpic (r,r1)

vtiff3o r r.tif 'tiled 'lzw
vtiff3o-tovic r.tif r1
difpic (r,r1)

f2 out=lut fun="(samp %256)*(samp%4)" nl=1 ns=1024
vtiff3o (r,lut) lut.tif
vtiff3o-tovic lut.tif (r1,lut1)
difpic (r,r1)
difpic (lut,lut1)

gen g 50 50 linc=0
gen b 50 50 sinc=0

vtiff3o (r,g,b) color.tif
vtiff3o-tovic  color.tif (r1,g1,b1)
difpic (r,r1)
difpic (g,g1)
difpic (b,b1)

vtiff3o (r,g,b) color.tif 'planar
vtiff3o-tovic  color.tif (r1,g1,b1)
difpic (r,r1)
difpic (g,g1)
difpic (b,b1)

vtiff3o (r,g,b) color.tif 'tiled
vtiff3o-tovic  color.tif (r1,g1,b1)
difpic (r,r1)
difpic (g,g1)
difpic (b,b1)

vtiff3o (r,g,b) color.tif 'planar 'tiled
vtiff3o-tovic  color.tif (r1,g1,b1)
difpic (r,r1)
difpic (g,g1)
difpic (b,b1)

vtiff3o (r,g,b) color.tif 'planar 'tiled 'lzw
vtiff3o-tovic  color.tif (r1,g1,b1)
difpic (r,r1)
difpic (g,g1)
difpic (b,b1)

gen a nl=500 ns=500
vtiff3o a a.tif 'tile tline=128 tsamp=128 'packbits
vtiff3o-tovic a.tif a1
difpic (a,a1)

!test dump facility:
vtiff3o-tovic a.tif a1 'dump


!  new case for reading the VICAR GeoTIFF label, not parameters

gen xxxim1 nl=10 ns=10
gtgen in=xxxim1 'tiecnvrt +
   geotiff=("GTModelTypeGeoKey=2(ModelTypeGeographic)", +
          "GeogEllipsoidGeoKey=7030(Ellipse_WGS84)", +
          "GeogPrimeMeridianGeoKey=40.0", +
          "ModelTiePointTag=(2,3,0,-110,39,0.0)", +
          "ModelTiePointTag=(2,10,0,-110,35,0.0)", +
          "ModelTiePointTag=(10,3,0,-108,39,0.0)", +
          "GTRasterTypeGeoKey=2(RasterPixelIsPoint)")

gtlist xxxim1

vtiff3o xxxim1 xxxim1.tif

ush strings xxxim1.tif
vtiff3o-tovic xxxim1.tif xxim2 'dump

theend>

end-proc
$!-----------------------------------------------------------------------------
$ create tstvtiff3o.log
                Version 5C/16C

      ***********************************************************
      *                                                         *
      * VICAR Supervisor version 5C, TAE V5.2                   *
      *   Debugger is now supported on all platforms            *
      *   USAGE command now implemented under Unix              *
      *                                                         *
      * VRDI and VIDS now support X-windows and Unix            *
      * New X-windows display program: xvd (for all but VAX/VMS)*
      *                                                         *
      * VICAR Run-Time Library version 16C                      *
      *   '+' form of temp filename now avail. on all platforms *
      *   ANSI C now fully supported                            *
      *                                                         *
      * See B.Deen(RGD059) with problems                        *
      *                                                         *
      ***********************************************************

  --- Type NUT for the New User Tutorial ---

  --- Type MENU for a menu of available applications ---

let $autousage="none"
write "each of the difpic calls should yield zero differences"
each of the difpic calls should yield zero differences
gen r 50 50
Beginning VICAR task gen
GEN Version 6
GEN task completed
vtiff3o r r.tif
Beginning VICAR task vtiff3o
vtiff3o version 2015-09-17
0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100%
|----|----|----|----|----X----|----|----|----|----|
|>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
vtiff3o-tovic r.tif r1
Beginning VICAR task vtiff3o
vtiff3o version 2015-09-17
0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100%
|----|----|----|----|----X----|----|----|----|----|
|>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
difpic (r,r1)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
vtiff3o r r.tif 'packbits
Beginning VICAR task vtiff3o
vtiff3o version 2015-09-17
0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100%
|----|----|----|----|----X----|----|----|----|----|
|>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
vtiff3o-tovic r.tif r1
Beginning VICAR task vtiff3o
vtiff3o version 2015-09-17
0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100%
|----|----|----|----|----X----|----|----|----|----|
|>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
difpic (r,r1)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
vtiff3o r r.tif 'lzw
Beginning VICAR task vtiff3o
vtiff3o version 2015-09-17
0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100%
|----|----|----|----|----X----|----|----|----|----|
|>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
vtiff3o-tovic r.tif r1
Beginning VICAR task vtiff3o
vtiff3o version 2015-09-17
0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100%
|----|----|----|----|----X----|----|----|----|----|
|>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
difpic (r,r1)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
vtiff3o r r.tif 'tiled
Beginning VICAR task vtiff3o
vtiff3o version 2015-09-17
0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100%
|----|----|----|----|----X----|----|----|----|----|
|>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>vtiff3o-tovic r.tif r1
Beginning VICAR task vtiff3o
vtiff3o version 2015-09-17
0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100%
|----|----|----|----|----X----|----|----|----|----|
|>difpic (r,r1)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
vtiff3o r r.tif 'tiled 'lzw
Beginning VICAR task vtiff3o
vtiff3o version 2015-09-17
0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100%
|----|----|----|----|----X----|----|----|----|----|
|>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>vtiff3o-tovic r.tif r1
Beginning VICAR task vtiff3o
vtiff3o version 2015-09-17
0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100%
|----|----|----|----|----X----|----|----|----|----|
|>difpic (r,r1)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
f2 out=lut fun="(samp %256)*(samp%4)" nl=1 ns=1024
Beginning VICAR task f2
F2 version 98-Aug-2015
F2 calculating every pixel
FUNCTION EVALUATED 1024 TIMES
vtiff3o (r,lut) lut.tif
Beginning VICAR task vtiff3o
vtiff3o version 2015-09-17
0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100%
|----|----|----|----|----X----|----|----|----|----|
|>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
vtiff3o-tovic lut.tif (r1,lut1)
Beginning VICAR task vtiff3o
vtiff3o version 2015-09-17
0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100%
|----|----|----|----|----X----|----|----|----|----|
|>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
difpic (r,r1)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
difpic (lut,lut1)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
gen g 50 50 linc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen b 50 50 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
vtiff3o (r,g,b) color.tif
Beginning VICAR task vtiff3o
vtiff3o version 2015-09-17
0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100%
|----|----|----|----|----X----|----|----|----|----|
|>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
vtiff3o-tovic  color.tif (r1,g1,b1)
Beginning VICAR task vtiff3o
vtiff3o version 2015-09-17
forcing nb to 3, was 3, because numout == 3
0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100%
|----|----|----|----|----X----|----|----|----|----|
|>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
difpic (r,r1)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
difpic (g,g1)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
difpic (b,b1)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
vtiff3o (r,g,b) color.tif 'planar
Beginning VICAR task vtiff3o
vtiff3o version 2015-09-17
0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100%
|----|----|----|----|----X----|----|----|----|----|
|>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
vtiff3o-tovic  color.tif (r1,g1,b1)
Beginning VICAR task vtiff3o
vtiff3o version 2015-09-17
forcing nb to 3, was 3, because numout == 3
0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100%
|----|----|----|----|----X----|----|----|----|----|
|>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
difpic (r,r1)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
difpic (g,g1)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
difpic (b,b1)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
vtiff3o (r,g,b) color.tif 'tiled
Beginning VICAR task vtiff3o
vtiff3o version 2015-09-17
0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100%
|----|----|----|----|----X----|----|----|----|----|
|>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>vtiff3o-tovic  color.tif (r1,g1,b1)
Beginning VICAR task vtiff3o
vtiff3o version 2015-09-17
forcing nb to 3, was 3, because numout == 3
0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100%
|----|----|----|----|----X----|----|----|----|----|
|difpic (r,r1)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
difpic (g,g1)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
difpic (b,b1)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
vtiff3o (r,g,b) color.tif 'planar 'tiled
Beginning VICAR task vtiff3o
vtiff3o version 2015-09-17
0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100%
|----|----|----|----|----X----|----|----|----|----|
|>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>vtiff3o-tovic  color.tif (r1,g1,b1)
Beginning VICAR task vtiff3o
vtiff3o version 2015-09-17
forcing nb to 3, was 3, because numout == 3
0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100%
|----|----|----|----|----X----|----|----|----|----|
|difpic (r,r1)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
difpic (g,g1)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
difpic (b,b1)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
vtiff3o (r,g,b) color.tif 'planar 'tiled 'lzw
Beginning VICAR task vtiff3o
vtiff3o version 2015-09-17
0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100%
|----|----|----|----|----X----|----|----|----|----|
|>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>vtiff3o-tovic  color.tif (r1,g1,b1)
Beginning VICAR task vtiff3o
vtiff3o version 2015-09-17
forcing nb to 3, was 3, because numout == 3
0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100%
|----|----|----|----|----X----|----|----|----|----|
|difpic (r,r1)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
difpic (g,g1)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
difpic (b,b1)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
gen a nl=500 ns=500
Beginning VICAR task gen
GEN Version 6
GEN task completed
vtiff3o a a.tif 'tile tline=128 tsamp=128 'packbits
Beginning VICAR task vtiff3o
vtiff3o version 2015-09-17
0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100%
|----|----|----|----|----X----|----|----|----|----|
|>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>vtiff3o-tovic a.tif a1
Beginning VICAR task vtiff3o
vtiff3o version 2015-09-17
0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100%
|----|----|----|----|----X----|----|----|----|----|
|>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>difpic (a,a1)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
vtiff3o-tovic a.tif a1 'dump
Beginning VICAR task vtiff3o
vtiff3o version 2015-09-17
TIFF Directory at offset 0x40808 (264200)
  Subfile Type: (0 = 0x0)
  Image Width: 500 Image Length: 500
  Tile Width: 128 Tile Length: 128
  Resolution: 72, 72 pixels/inch
  Bits/Sample: 8
  Compression Scheme: PackBits
  Photometric Interpretation: min-is-black
  Samples/Pixel: 1
  Planar Configuration: single image plane
  Software: VICAR Program VTIFF
  DateTime: 2015:10:14 18:16:38
gen xxxim1 nl=10 ns=10
Beginning VICAR task gen
GEN Version 6
GEN task completed
gtgen in=xxxim1 'tiecnvrt  +
   geotiff=("GTModelTypeGeoKey=2(ModelTypeGeographic)",  +
          "GeogEllipsoidGeoKey=7030(Ellipse_WGS84)",  +
          "GeogPrimeMeridianGeoKey=40.0",  +
          "ModelTiePointTag=(2,3,0,-110,39,0.0)",  +
          "ModelTiePointTag=(2,10,0,-110,35,0.0)",  +
          "ModelTiePointTag=(10,3,0,-108,39,0.0)",  +
          "GTRasterTypeGeoKey=2(RasterPixelIsPoint)")
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
add mode off
tiecnvrt mode on
gtlist xxxim1
Beginning VICAR task gtlist
gtlist version Wed Jan  2 2008


      VICAR GeoTIFF LABEL LIST

The VICAR GeoTIFF label is:
GTMODELTYPEGEOKEY=2(ModelTypeGeographic)
GEOGELLIPSOIDGEOKEY=7030(Ellipse_WGS84)
GEOGPRIMEMERIDIANGEOKEY=40.0
MODELTIEPOINTTAG=(2,3,0,-110,39,0.0)
MODELPIXELSCALETAG=(0.25,0.5714285714286,0.0)
GTRASTERTYPEGEOKEY=2(RasterPixelIsPoint)


The image raster is a 'point' or 'post' type

The centers of the corner pixels are:

VICAR-line    -samp GeoTIFF-samp    -line            East           North

       1.0      1.0          0.0      0.0 -110.5000000000  40.71428571429
       1.0     10.0          9.0      0.0 -108.2500000000  40.71428571429
      10.0      1.0          0.0      9.0 -110.5000000000  35.57142857143
      10.0     10.0          9.0      9.0 -108.2500000000  35.57142857143


The outer corners of the corner pixels are:

VICAR-line    -samp GeoTIFF-samp    -line            East           North

       0.5      0.5         -0.5     -0.5 -110.6250000000  41.00000000000
       0.5     10.5          9.5     -0.5 -108.1250000000  41.00000000000
      10.5      0.5         -0.5      9.5 -110.6250000000  35.28571428571
      10.5     10.5          9.5      9.5 -108.1250000000  35.28571428571

The rotation of the image relative to an E-N geographic frame is:

rotation 1
123
456
789


The scale units of the image are (ignoring sign):

1 sample = 0.2500000000000 map units east
1 line   = 0.5714285714286 map units north


The scale fraction is 1 /      9.8
(assuming mapunit = 1.000000 meters and the map is 10.000000 inches)

vtiff3o xxxim1 xxxim1.tif
Beginning VICAR task vtiff3o
vtiff3o version 2015-09-17
USING VICAR GEOTIFF LABEL TO CREATE, PARAMETERS IGNORED
exporting GeoTIFF
0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100%
|----|----|----|----|----X----|----|----|----|----|
|>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
ush strings xxxim1.tif
VICAR Program VTIFF
2015:10:14 18:16:38
vtiff3o-tovic xxxim1.tif xxim2 'dump
Beginning VICAR task vtiff3o
vtiff3o version 2015-09-17
TIFF Directory at offset 0x6c (108)
  Subfile Type: (0 = 0x0)
  Image Width: 10 Image Length: 10
  Resolution: 72, 72 pixels/inch
  Bits/Sample: 8
  Compression Scheme: None
  Photometric Interpretation: min-is-black
  Samples/Pixel: 1
  Rows/Strip: 1
  Planar Configuration: single image plane
  Software: VICAR Program VTIFF
  DateTime: 2015:10:14 18:16:38
  GeoPixelScale: 0.250000,0.571429,0.000000
  GeoTiePoints: 2.000000,3.000000,0.000000,-110.000000,39.000000,0.000000
  GeoKeyDirectory: 1,1,0,4,1024,0,1,2,1025,0,1,2,2051,0,1,40,2056,0,1,7030
end-proc
$ Return
$!#############################################################################
$Imake_File:
$ create vtiff3o.imake

#define PROGRAM vtiff3o

#define MODULE_LIST vtiff3o.c optimal_color2.c uFixedNums.c

#define MAIN_LANG_C
#define R2LIB

/* Comment this out before delivery.*/
#define DEBUG


#define USES_ANSI_C

#define LIB_CARTO
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_TIFF
$ Return
$!#############################################################################
