/**
 **  VTIFF is a Vicar2 program that converts between
 VICAR labeled images and TIFF format files. Partial
 conversion of GeoTIFF information is included.
 **
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
int WriteVicarData(int * outunit, TIFF *in, int nl,int ns, int nb, int numout);
void AddTIFFGeoreference(TIFF *out);
void AddFiletoTIFF(char *parm, TIFF *tif, char *mode);
void VicAddGeoTIFF(TIFF *out, GTIF *gtif);
void AddGeoTIFF(TIFF *out, GTIF *gtif);
void SetUpTiffDirectory(int * inunit, TIFF *out, char *mode, int *nl, int *ns, int nb);
int ConvertToLut(int * unit, int ninp);
int WriteTIFFData(int * inunit, TIFF *out, int nl, int ns, int nb, int ninp);
void SetTIFFClut(int * unit, unsigned short clut[3][256], char *type);
static void UpdateStatus(int current);

static unsigned int sleeptime=0; /* why was this ever non-zero? */
static int printstat=0;
static int useLut=0;

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
    
  zifmessage("*** vtiff3o version 2017-08-15 ***");
    
  printstat = zvptst("PRINT");

  zvparm("_SUBCMD",command,&count,&def,0,0);
  switch (toupper(command[0]))
    {
    case 'F' :
      vic_to_tiff();
      break;
    case 'T' :
      tiff_to_vic();
      break;
    }
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
    snprintf (msgBuf, 200, "addGTLabel failed to add a label for key %s, value %s", key, value);
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
  int allocSize = 0;

  if (key->gk_type==TYPE_SHORT && count==1)
    data = (char *)&key->gk_data;
  else
    data = key->gk_data;
		
  switch (key->gk_type)
    {
    case TYPE_ASCII: 
      allocSize = count + 50;
      *buf = malloc (allocSize);
      strncpy (*buf, data, count);
      (*buf) [count-1] = '\0';	/* the last character is a '|', representing a null */
      if (strlen (*buf) > 75)
	(*buf)[76] = '\0';	/* zlinfo, zlget don't like long strings */
      snprintf (*buf + strlen(*buf), allocSize, ";gk_key=%d", keyid);
      break;
    case TYPE_DOUBLE: 
      allocSize = 40 * count + 40;
      *buf = malloc (allocSize);
      **buf = '\0';


      for (dptr = (double *)data; count > 0; count-= vals_now)
	{
	  vals_now = count > 3? 3: count;
	  for (i=0; i<vals_now; i++,dptr++)
	    {
	      if (**buf == '\0')
		snprintf (*buf, allocSize, "(%.15lg", *dptr);
	      else
		snprintf (*buf + strlen (*buf), allocSize, ", %.15lg", *dptr);
	    }
	  snprintf (*buf, allocSize, ");gk_type=%s;gk_key=%d", GTIFTypeName(key->gk_type), keyid);
	}
      break;
    case TYPE_SHORT: 
      sptr = (pinfo_t *)data;
      if (count==1)
	{
	  allocSize = 100;
	  *buf = malloc(allocSize);

	  snprintf (*buf, allocSize,
		   "%d(%s);gk_type=%s;gk_key=%d", *sptr, GTIFValueName(keyid,*sptr), GTIFTypeName(key->gk_type), keyid);
	}
      else {
	allocSize = 15 * count + 50;
	*buf = malloc(allocSize);
	**buf = '\0';

	for (; count > 0; count-= vals_now)
	  {
	    vals_now = count > 3? 3: count;
	    for (i=0; i<vals_now; i++,sptr++)
	      {
		if (**buf == '\0')
		  snprintf (*buf, allocSize, "(%d", *sptr);
		else
		  snprintf (*buf + strlen (*buf), allocSize, ", %d", *sptr);
	      }
	    snprintf (*buf, allocSize, ");gk_type=%s;gk_key=%d", GTIFTypeName(key->gk_type), keyid);
	  }
      }
      break;
    default: 
      allocSize = 100;
      *buf = malloc(allocSize);
      snprintf (*buf, allocSize, "Unknown Type (%d)\n", key->gk_type);
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
	  snprintf (numBuf, 40, "%.15lg", data [sixTuple * 6 + part]);
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
	snprintf (numBuf, 40, "%.15lg", data [part]);
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
	snprintf (numBuf, 40, "%.15lg", data [part]);
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
	  snprintf (numBuf, 40, "%.15lg", tiePoint [part]);
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
	  snprintf (numBuf, 40, "%.15lg", pixScale [part]);
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
  if (in == NULL) zmabend("Error opening TIFF file");

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
      zmabend("Error opening creating GTIF\n");
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
	zmabend("Not enough images in TIFF file for auxilliaries");
      WriteFileFromTIFF ("AUXIL", i + 1, in);
    }
      
  /* close up shop */
  TIFFClose(in);
}

void WriteFileFromTIFF(char *parm, int instance, TIFF *tif)
{
  int  unit[4],nl,ns,i,nb,numout;

  SetUpVicarFile(parm,instance,tif,unit,&nl,&ns,&nb,&numout);
  if (!WriteVicarData(unit,tif,nl,ns,nb,numout))
    zmabend("Error in Copying data to TIFF ");

  for (i=0; i<numout; i++)
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

  /* how much (if any) of the following is necessary? */
  if (*nb < 1) {
    printf ("forcing nb to 1, was %d\n", *nb);
    *nb=1;
  }
  if (*nb > 4)
    *nb=3;
  if (*numout==3 && *nb != 3) {
    printf ("forcing nb to 3, was %d, because numout == 3\n", *nb);
    *nb=3;
  }
  if (*numout==0 && *nb != 0) {
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

  for (i=0;i<*numout;i++)
    {
      zvpone(parm, name, instance+i, 132);
      zvunit(outunit+i, "xxx", instance+i,"U_NAME",name, NULL);
	      
      /* Open up VICAR file with proper size */
	
      if (*numout == 1 && *nb == 3)
	zvopen(outunit[i],"OP","WRITE","U_NL",nlout,"U_NS",nsout, "U_NB", 3, "O_FORMAT",format, NULL);
      else
	zvopen(outunit[i],"OP","WRITE","U_NL",nlout,"U_NS",nsout, "O_FORMAT",format, NULL);
	      
      /* Reopen in UPDATE for random access */
      zvclose(outunit[i], NULL);
      zvopen(outunit[i],"OP","UPDATE", NULL);
    }
}


int WriteVicarData(int * outunit, TIFF *in, int nl,int ns, int nb, int numout)
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
	zmabend("failed to allocate buffer for tiles");
 
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
			  if (nb == 3 && numout == 1)
			    zvwrit(outunit[0],
				   lbuf[band]+y1*tile_width*pixsize,"line",y+y1+1,
				   "samp",x+1,"nsamps", nsamps, "band", band+1, NULL);
			  else
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
	      if (nb == 3 && numout == 1)
		zvwrit(outunit[0],lbuf[band],"LINE",y+1, "BAND", band+1, NULL);
	      else
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

  zifmessage("exporting GeoTIFF");

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
		  printf("ignoring GEOTIFF value with no gk_key: %s=%s\n", key, value);
		} else {
		  if (foundLegacyValue || strstr (value, ";gk_type=Short")) { /* it's a Short */
		    if (! foundLegacyValue)
		      * (strstr (value, ";gk_type=Short")) = '\0';
		    if (value [0] == '(')
		      printf("not exporting GEOTIFF multi Short values with gk_key %d: %s=%s\n", gk_key, key, value);
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
		      printf("not exporting GEOTIFF multi Double values with gk_key %d: %s=%s\n", gk_key, key, value);
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
		
		/* printf("found key %s\n", key); */
		/* get the key's value */
		zlget (unit, "PROPERTY", key, value, "PROPERTY", "COREG_META_DATA", NULL);

		if (!strcmp (format, "INT")) {
		  int intVal = * ((int *) value);
		  free (value);
		  value = malloc (100);
		  snprintf (value, 100, "%d", intVal);
		} else if (!strcmp (format, "REAL")) {
		  float realVal = * ((float *) value);
		  free (value);
		  value = malloc (100);
		  snprintf (value, 100, "%f", realVal);
		} else if (!strcmp (format, "DOUB")) {
		  double doubleVal = * ((double *) value);
		  free (value);
		  value = malloc (100);
		  snprintf (value, 100, "%lf", doubleVal);
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

  if (out == NULL) zmabend("Error opening TIFF file");
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
	
  zifmessage("USING VICAR GEOTIFF LABEL TO CREATE, PARAMETERS IGNORED");

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
  int i,nl,ns,nb,status,uinb;
  int bit8;
  char name[201];
  int ninp;

  zvpcnt(parm, &ninp);
  bit8 = zvptst("bit8");
     
  for (i=0; i<ninp; i++)
    {
      zvpone(parm, name, i+1, 200);
      zvunit(unit+i, parm, i+1,"U_NAME",name, NULL);
      status = zvopen(unit[i],"OP","READ", NULL);
    }

  switch (ninp) {
  case 1:			/* check for multi-band inp file */
    zvget(unit[0], "NB", &uinb, NULL);
    if (uinb == 3)
      nb = 3;
    else if (uinb == 1)
      nb = 1;
    else
      zmabend("Input VICAR file must have either 1 or 3 bands");
    break;
  case 2:
    nb = 1;			/* second inp should be a lut */
    /* make sure first input is single-band */
    zvget(unit[0], "NB", &uinb, NULL);
    if (uinb != 1)
      zmabend("Single input VICAR file with lut file must have only one band");
    break;
  case 3:
    nb = 3;
    /* make sure multi-file input is single-band per file */
    for (i = 0; i < 3; ++i) {
      zvget(unit[i], "NB", &uinb, NULL);
      if (uinb != 1)
	zmabend("Multi-file VICAR input must have one band per file");
    }
    break;
  default:
    zmabend("Input VICAR image must come from either 1 or 3 files");
    break;
  }
      
  if (nb==3 && bit8)
    {
      if (ninp == 1)
	unit[2] = unit[1] = unit[0];
      ConvertToLut(unit, ninp);
      nb = 1;
      useLut = 1;		/* global */
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

  if (!WriteTIFFData(unit, tif, nl, ns, nb, ninp))
    zmabend("Error in Copying data to TIFF ");
	    
  for (i=0; i<ninp; i++)
    zvclose(unit[i], NULL);
}

#define MAX_DIM 512

int ConvertToLut(int * unit, int ninp)
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
      if (ninp == 3)
	for (i=0;i<3;i++)
	  zvread(unit[i], lbuffer[i], "line", line, NULL);
      else
	for (i=0;i<3;i++)
	  zvread(unit[0], lbuffer[i], "line", line, "band", i+1, NULL);
		
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
      if (ninp == 3)
	for (i=0;i<3;i++)
	  zvread(unit[i], lbuffer[i], "line", line, NULL);
      else
	for (i=0;i<3;i++)
	  zvread(unit[i], lbuffer[i], "line", line, "band", i+1, NULL);

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

  /* close old files */
  for (i=0; i<ninp; i++)
    zvclose(unit[i], NULL);

  /* free up memory */
  for (i=0; i<3; i++)
    free(lbuffer[i]);

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
      if (useLut)		/* global */
	{
	  TIFFSetField(out, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_PALETTE);
	  SetTIFFClut(inunit,clut,"DiskFile");
	  TIFFSetField(out, TIFFTAG_COLORMAP, clut[0],clut[1],clut[2]);
	}
      else
	TIFFSetField(out, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_MINISBLACK);

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
      zmabend("Unknown lookup table type");
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
  snprintf(time_string,20,"%4d:%02d:%02d %02d:%02d:%02d",
	   1900+(timv.tm_year), 1+(timv.tm_mon), (timv.tm_mday),
	   (timv.tm_hour), (timv.tm_min), (timv.tm_sec));
      
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

int WriteTIFFData(int * inunit, TIFF *out, int nl, int ns, int nb, int ninp)
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
	zmabend("failed to allocate buffer for tiles");

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
		      if (ninp == 3)
			zvread(inunit[band],lbuf[band]+
			       y1*pixsize*tile_width,
			       "LINE", y+y1+1, "SAMP", x+1,"NSAMPS", nsamps, NULL);
		      else
			zvread(inunit[0],lbuf[band]+
			       y1*pixsize*tile_width,
			       "LINE", y+y1+1, "SAMP", x+1,"NSAMPS", nsamps, "BAND", band+1, NULL);
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

      if (!buf || !lbuf[0]) zmabend("failed to allocate buffer for tiles");
            
      if (printstat) StartStatus(nl);
      for (y = 0; y < nl; y++)
	{
	  if (printstat) UpdateStatus(y+1);
	  for (band=0; band<nb; band++)
	    {
	      if (ninp == 3)
		zvread(inunit[band], lbuf[band], "LINE", y+1, NULL);
	      else
		zvread(inunit[0], lbuf[band], "LINE", y+1, "BAND", band + 1, NULL);
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
