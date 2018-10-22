#include <math.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <stdlib.h>

/* need these for stat */
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "zmabend.h"
#include "zifmessage.h"
#include "applic.h"
#include "ibisfile.h"
#include "ibiserrs.h"

#include "cartoLoggerUtils.h"

int labelRaw16bitImage (char * inpath, char * outpath, int nl, int ns) {
  int vunit;
  char * lineBuf = malloc (2 * ns);
  int lineNum;
  FILE * infile = fopen (inpath, "r");
  char msg [100];
  int readFailed = 0;

  if (! lineBuf)
    zmabend ("malloc failed in labelRaw16bitImage");
  if (! infile)
    zmabend ("fopen failed in labelRaw16bitImage");
  
  if (zvunit (& vunit, "U_NAME", 1, "U_NAME", outpath, NULL) != 1)
    zmabend ("zvunit failed for out image");

  if (zvopen (vunit, "U_NL", nl, "U_NS", ns, "OP", "WRITE", "OPEN_ACT", "SA", "IO_ACT", "SA", "O_FORMAT", "HALF", NULL) != 1)
    zmabend ("zvopen failed for out image");

  for (lineNum = 0; lineNum < nl; lineNum ++) {
    if (readFailed || fread (lineBuf, 2, ns, infile) != ns) {
      if (! readFailed) {
	sprintf (msg, "fread failed at line %d in labelRaw16bitImage; padding image", lineNum + 1);
	zifmessage (msg);
	memset (lineBuf, 0, 2 * ns);
	readFailed = 1;
      }
    }

    zvwrit (vunit,
	    lineBuf,
	    "LINE", lineNum + 1,
	    "SAMP", 1,
	    "NSAMPS", ns, NULL);
  }

  free (lineBuf);

  return vunit;
}

short shortSwapBytes (short s) {
  unsigned char left = ((unsigned char *) & s) [0];
  unsigned char right = ((unsigned char *) & s) [1];
  ((unsigned char *) & s) [0] = right;
  ((unsigned char *) & s) [1] = left;
  return s;
}

void * checkedMalloc (size_t size, char * description) {
  void * p = malloc (size);

  if (! p) {
    char * prefix = "checkedMalloc failed on ";
    char * buf = (char *) malloc (strlen (prefix) + strlen (description) + 1);

    if (! buf)
      zmabend ("checkedMalloc failed allocating message buffer");

    sprintf (buf, "%s%s", prefix, description);

    zmabend (buf);
  }

  return p;
}

/* This routine mallocs space for the contents of a file and reads it
   into the malloced buffer. The caller should free the buffer when no
   longer needed. The read data is null terminated. */
char * mallocAndRead (char * path) {
  FILE * file;
  char msgBuf [100];
  struct stat statBuf;
  char * buf = 0;
  int status;

  /* stat the file to determine size, so we can read it all at once */
  if (stat (path, & statBuf)) {
    sprintf (msgBuf, "error stating \"%s\"", path);
    zmabend (msgBuf);
  }

  if (! (buf = (char *) malloc (statBuf.st_size + 1)))
    zmabend ("error mallocing file buffer");

  /* open file */
  if (! (file = fopen (path, "r"))) {
    sprintf (msgBuf, "error opening %s for input", path);
    zmabend (msgBuf);
  }

  /* read the data */
  if ((status = fread (buf, 1, statBuf.st_size, file)) != statBuf.st_size) {
    sprintf (msgBuf, "error reading %s", path);
    zmabend (msgBuf);
  }
  
  buf [statBuf.st_size] = 0;	/* null terminate the data */

  fclose (file);		/* done with data */

  return buf;
}

void getDataSetTime (char * dataSetName, char * times, int * utcTimeInMinutes, char * date, char * time) {
  char * p = strstr (times, dataSetName);
  int hours, minutes;
  char msgBuf [1000];

  if (! p || p - 40 < times) {
    sprintf (msgBuf, "Data set %s not found in SAA times data", dataSetName);
    zmabend (msgBuf);
  }

  strncpy (date, p - 40, 10);
  date [10] = 0;
  strncpy (time, p - 40 + 11, 12);
  time [12] = 0;

  sscanf (time, "%2d:%2d:", & hours, & minutes);
  * utcTimeInMinutes = hours * 60 + minutes;
}

void forceSubAreaSanity (int * sl, int * ss, int * nl, int * ns, int lines, int samples) {
  if (* sl < 1)
    * sl = 1;
  if (* sl > lines)
    * sl = lines;

  if (* ss < 1) 
    * ss = 1;
  if (* ss > samples)
    * ss = samples;

  if (* nl < 1 || * nl > lines - * sl + 1)
    * nl = lines - * sl + 1;
  if (* ns < 1 || * ns > samples - * ss + 1)
    * ns = samples - * ss + 1;
}

int usingLatLonSubArea (int imageLines, int imageSamples,
		       double * minLat, double * maxLat, double * minLon, double * maxLon,
		       int * sl, int * ss, int * nl, int * ns, int allow) {
  int parmct, pcount, pdef;

  /* fetch line/sample params */
  zvp ("sl", sl, & parmct);
  zvp ("ss", ss, & parmct);
  zvp ("nl", nl, & parmct);
  zvp ("ns", ns, & parmct);

  /* fetch lat/lon params */
  zvparmd ("minlat", minLat, & pcount, & pdef, 1, 0);
  zvparmd ("maxLat", maxLat, & pcount, & pdef, 1, 0);
  zvparmd ("minLon", minLon, & pcount, & pdef, 1, 0);
  zvparmd ("maxLon", maxLon, & pcount, & pdef, 1, 0);

  /* if a line/sample value was specified */
  if (* sl != -999 || * ss != -999 || * nl != -999 || * ns != -999) {
    /* ensure they all were specified */
    if (* sl == -999 || * ss == -999 || * nl == -999 || * ns == -999)
      zmabend ("some but not all of (sl, ss, nl, ns) sub-area parameters were specified");

    /* ensure no lat/lon values were specified */
    if (* minLat != -999.0 || * maxLat != -999.0 || * minLon != -999.0 || * maxLat != -999.0)
      zmabend ("both line/sample and lat/lon sub-area parameters were specified");

    /* ensure line/sample values are within image */
    if (! BETWEEN (1, * sl, imageLines) ||
	! BETWEEN (1, * ss, imageSamples) ||
	! BETWEEN (1, * sl + * nl - 1, imageLines) ||
	! BETWEEN (1, * ss + * ns - 1, imageSamples))
      zmabend ("line/sample sub-area parameter(s) outside of image");

  } else if (allow &&
	     (* minLat != -999.0 || * maxLat != -999.0 || * minLon != -999.0 || * maxLat != -999.0)) {
    /* a lat/lon value was specified */
    /* ensure they all are specified */
    if (* minLat == -999.0 || * maxLat == -999.0 || * minLon == -999.0 || * maxLat == -999.0) {
      if (* minLat == -999.0)
	* minLat = -90.0;
      if (* maxLat == -999.0)
	* maxLat = 90.0;
      if (* minLon == -999.0)
	* minLon = -180.0;
      if (* maxLon == -999.0)
	* maxLon = 360.0;
    }

    /* ensure no line/sample values were specified */
    if (* sl != -999 || * ss != -999 || * nl != -999 || * ns != -999)
      zmabend ("both line/sample and lat/lon sub-area parameters were specified");

    return 1;

  } else {
    /* no sub-area was specified; use whole image */

    * sl = 1;
    * ss = 1;
    * nl = imageLines;
    * ns = imageSamples;
  }

  return 0;
}

void initMetaData (char * path) {
  FILE * f;
  time_t now;
  char msgBuf [100];

  if (! (f = fopen (path, "w"))) {
    sprintf (msgBuf, "error opening parm file \"%s\" for writing", path);
    zmabend (msgBuf);
  }

  if (time (& now) < 0)
    zmabend ("error reading current time");

  strcpy (msgBuf, ctime (& now));
  msgBuf [strlen (msgBuf) - 1] = 0; /* trim the trailing newline */
  fprintf (f, "LOG_TIME=\"%s\"\n", msgBuf);

  fclose (f);    
}

#define metaDataLabelProperty "LOGGER_META_DATA"

void metaToLabel (char * metaName, int vunit) {
  int status;
  char * buf = mallocAndRead (metaName);
  int size = strlen (buf);

  if ((status = zladd (vunit, "PROPERTY",
		       "ALL_META_DATA_SIZE", & size,
		       "PROPERTY", metaDataLabelProperty,
		       "FORMAT", "INT", NULL)) != 1)
    zmabend ("zladd failed to add a label for ALL_META_DATA_SIZE");

  if ((status = zladd (vunit, "PROPERTY",
		       "ALL_META_DATA", buf,
		       "PROPERTY", metaDataLabelProperty,
		       "FORMAT", "STRING", NULL)) != 1)
    zmabend ("zladd failed to add a label for ALL_META_DATA");
  free (buf);
}

void logMetaString (int echoMeta, char * path, char * name, char * value, int unitCount, int * vunits) {
  logMetaStringToProperty (echoMeta, path, name, value, unitCount, vunits, 0, 0);
}

void logMetaStringToProperty (int echoMeta, char * path, char * name, char * value, int unitCount, int * vunits, char * property, int noQuotes) {
  FILE * f;
  char msgBuf [100];
  int status;
  int vunit;
  
  if (path) {
    if (! (f = fopen (path, "a"))) {
      sprintf (msgBuf, "error opening parm file \"%s\" for appending", path);
      zmabend (msgBuf);
    }

    if (noQuotes)
      fprintf (f, "%s=%s\n", name, value);
    else
      fprintf (f, "%s=\"%s\"\n", name, value);

    fclose (f);    
  }

  if (echoMeta) {
    char * buf = checkedMalloc (strlen (name) + strlen (value) + 2, /* + 2 for = and null characters */
				"buf for meta string echo");
    sprintf (buf, "%s=%s", name, value);
    zifmessage (buf);
    free (buf);
  }

  for (vunit = 0; vunit < unitCount; vunit ++)
    if ((status = zladd (vunits [vunit], "PROPERTY",
			 name, value,
			 "PROPERTY", property?property:metaDataLabelProperty,
			 "FORMAT", "STRING", NULL)) != 1)
      zmabend ("zladd failed to add a label in logMetaString");
}

void logMetaInt (int echoMeta, char * path, char * name, int value, int unitCount, int * vunits) {
  FILE * f;
  char msgBuf [100];
  int vunit;
  int status;

  if (path) {
    if (! (f = fopen (path, "a"))) {
      sprintf (msgBuf, "error opening parm file \"%s\" for appending", path);
      zmabend (msgBuf);
    }

    fprintf (f, "%s=%d\n", name, value);

    fclose (f);    
  }

  if (echoMeta) {
    char * buf = checkedMalloc (strlen (name) + 20, "buf for meta int echo");
    sprintf (buf, "%s=%d", name, value);
    zifmessage (buf);
    free (buf);
  }

  for (vunit = 0; vunit < unitCount; vunit ++)
    if ((status = zladd (vunits [vunit], "PROPERTY",
			 name, & value,
			 "PROPERTY", metaDataLabelProperty,
			 "FORMAT", "INT", NULL)) != 1)
      zmabend ("zladd failed to add a label in logMetaInt");
}

void logMetaDouble (int echoMeta, char * path, char * name, double value, int unitCount, int * vunits) {
  logMetaDoubleToProperty (echoMeta, path, name, value, unitCount, vunits, 0);
}

void logMetaDoubleToProperty (int echoMeta, char * path, char * name, double value, int unitCount, int * vunits, char * property) {
  FILE * f;
  char msgBuf [100];
  int vunit;
  int status;

  if (path) {
    if (! (f = fopen (path, "a"))) {
      sprintf (msgBuf, "error opening parm file \"%s\" for appending", path);
      zmabend (msgBuf);
    }

    fprintf (f, "%s=%.16lf\n", name, value);

    fclose (f);    
  }

  if (echoMeta) {
    char * buf = checkedMalloc (strlen (name) + 35, "buf for double string echo");
    sprintf (buf, "%s=%18.16lf", name, value);
    zifmessage (buf);
    free (buf);
  }

  for (vunit = 0; vunit < unitCount; vunit ++)
    if ((status = zladd (vunits [vunit], "PROPERTY",
			 name, & value,
			 "PROPERTY", property?property:metaDataLabelProperty,
			 "FORMAT", "DOUB", NULL)) != 1)
      zmabend ("zladd failed to add a label in logMetaDouble");
}

void checkLoggerUtilsVersion (int minimum) {
  char msgBuf [100];

  sprintf (msgBuf, "expected loggerutils version >= %d, but using %d\n",
	   minimum, _loggerutils_version_);

  if (_loggerutils_version_ < minimum)
    zmabend (msgBuf);
}

#define IBISNCol 4
int logNavDataToIBIS (char * zvunitParmName, int zvunitParmIndex,
		      int numRows,
		      double * lineColumn, double * sampleColumn,
		      double * latColumn, double * lonColumn) {
  char IBISColumnFormats [IBISNCol] [6];
  int vunit, iunit;
  int i, status;

   /* set up IBIS column format labels */
  for (i = 0; i < IBISNCol; i ++)
    strcpy (IBISColumnFormats [i], "DOUB");

   /* open/setup IBIS navigation output */
  if (! strcmp (zvunitParmName, "OUT")){
    if ((status = zvunit (& vunit, zvunitParmName, zvunitParmIndex, NULL)) != 1)
      zmabend ("zvunit failed in logNavDataToIBIS");
  } else {			/* custom name */
    if ((status = zvunit (& vunit, "U_NAME", zvunitParmIndex, "U_NAME", zvunitParmName, NULL)) != 1)
      zmabend ("zvunit failed in logNavDataToIBIS");
  }
  if ((status = IBISFileUnit (vunit, &iunit, "write", IBISNCol, numRows, (char *) IBISColumnFormats, "column")) != 1)
    IBISSignal (iunit, status, 1);
  if ((status = IBISFileUnitOpen (iunit)) != 1)
    IBISSignal (iunit, status, 1);
  for (i = 0; i < IBISNCol; i ++)
    if ((status = IBISColumnSet (iunit, "U_FORMAT", "DOUB", i + 1 /* one based value*/)) != 1)
      IBISSignal (iunit, status, 1);
			  
  /* write columns LINE, SAMPLE, LAT, LON */
  if ((status = IBISColumnWrite (iunit,
				 (char*)lineColumn,       /* column data */
				 1,	           /* one based column number (line column)*/
				 1,                /* one based line number */
				 numRows)) != 1 || /* one IBIS row for each image pixel */
      (status = IBISColumnWrite (iunit,
				 (char*)sampleColumn,
				 2,	           /* sample column */
				 1,           
				 numRows)) != 1 ||
      (status = IBISColumnWrite (iunit,
				 (char*)latColumn,   
				 3,	           /* lat column */
				 1,           
				 numRows)) != 1 ||
      (status = IBISColumnWrite (iunit,
				 (char*)lonColumn,   
				 4,	           /* lon column */
				 1,           
				 numRows)) != 1)
    IBISSignal (iunit, status, 1);

  /* done with IBIS navigation output */
  if ((status = IBISFileClose (iunit, 0)) != 1)
    IBISSignal (iunit, status, 1);

  return 1;
}

#undef IBISNCol
#define IBISNCol 6
int logNavAndAnglesDataToIBIS (char * zvunitParmName, int zvunitParmIndex,
			       int numRows,
			       double * lineColumn, double * sampleColumn,
			       double * latColumn, double * lonColumn,
			       double * zenithColumn, double * azimuthColumn) {
  char IBISColumnFormats [IBISNCol] [6];
  int vunit, iunit;
  int i, status;

   /* set up IBIS column format labels */
  for (i = 0; i < IBISNCol; i ++)
    strcpy (IBISColumnFormats [i], "DOUB");

   /* open/setup IBIS navigation output */
  if (! strcmp (zvunitParmName, "OUT")){
    if ((status = zvunit (& vunit, zvunitParmName, zvunitParmIndex, NULL)) != 1)
      zmabend ("zvunit failed in logNavDataToIBIS");
  } else {			/* custom name */
    if ((status = zvunit (& vunit, "U_NAME", zvunitParmIndex, "U_NAME", zvunitParmName, NULL)) != 1)
      zmabend ("zvunit failed in logNavDataToIBIS");
  }
  if ((status = IBISFileUnit (vunit, &iunit, "write", IBISNCol, numRows, (char *) IBISColumnFormats, 0)) != 1)
    IBISSignal (iunit, status, 1);
  if ((status = IBISFileUnitOpen (iunit)) != 1)
    IBISSignal (iunit, status, 1);
  for (i = 0; i < IBISNCol; i ++)
    if ((status = IBISColumnSet (iunit, "U_FORMAT", "DOUB", i + 1 /* one based value*/)) != 1)
      IBISSignal (iunit, status, 1);
			  
  /* write columns LINE, SAMPLE, LAT, LON */
  if ((status = IBISColumnWrite (iunit,
				 (char*) lineColumn,       /* column data */
				 1,	           /* one based column number (line column)*/
				 1,                /* one based line number */
				 numRows)) != 1 || /* one IBIS row for each image pixel */
      (status = IBISColumnWrite (iunit,
				 (char*)sampleColumn,
				 2,	           /* sample column */
				 1,           
				 numRows)) != 1 ||
      (status = IBISColumnWrite (iunit,
				 (char*)latColumn,   
				 3,	           /* lat column */
				 1,           
				 numRows)) != 1 ||
      (status = IBISColumnWrite (iunit,
				 (char*)lonColumn,   
				 4,	           /* lon column */
				 1,           
				 numRows)) != 1 ||
      (status = IBISColumnWrite (iunit,
				 (char*)zenithColumn,   
				 5,	           /* zenith column */
				 1,           
				 numRows)) != 1 ||
      (status = IBISColumnWrite (iunit,
				 (char*)azimuthColumn,   
				 6,	           /* azimuth column */
				 1,           
				 numRows)) != 1)
    IBISSignal (iunit, status, 1);

  /* done with IBIS navigation output */
  if ((status = IBISFileClose (iunit, 0)) != 1)
    IBISSignal (iunit, status, 1);

  return 1;
}

/* To convert between geocentric latitude (gc) and geodetic latitude (gd), use the formula 
   tan(gc) = tan(gd) * (1-f)^2
   where f is the flattening parameter =  1 / 298.257223563
*/
#define degrad (3.14159265359 / 180.0)
#define f (1.0 / 298.257223563)
#define divisor ((1.0 - f) * (1.0 - f))
double geocentricToGeodetic (double latitude) {
  return atan (tan (latitude * degrad) / divisor) / degrad;
}

void addGTKey (int vunit, char * key, char * value) {
  char msgBuf [200];

  if (zladd (vunit, "PROPERTY",
	     key, value,
	     "PROPERTY", "GEOTIFF",
	     "FORMAT", "STRING", NULL) != 1) {
    sprintf (msgBuf, "addGTKey failed to add a label for key %s, value %s", key, value);
    zmabend (msgBuf);
  }
}

static int leapYear (int year) {
  int leap = 0;

  if (year % 4 == 0)
    leap = 1;
  if (year % 100 == 0)
    leap = 0;
  if (year % 400 == 0)
    leap = 1;

  return leap;
}

int daysInMonth (int year, int month) {
  switch (month) {
  case 2:
    return 28 + leapYear (year);
  case 4: case 6: case 9: case 11:
    return 30;
  default:
    return 31;
  }
}

/* dayOfYear is 1-based, e.g. dayOfYear==1 => January 1 */
int dateToDayOfYear (int year, int month, int day) {
  int thisMonth;
  int dayOfYear = day;

  for (thisMonth = 1; thisMonth < month; thisMonth ++)
    dayOfYear += daysInMonth (year, thisMonth);
  
  return dayOfYear;
}

/* dayOfYear is 1-based, e.g. dayOfYear==1 => January 1 */
void dayOfYearToDate (int dayOfYear, int year, int * month, int * day) {
  int daysSoFar = 0;

  for (* month = 1; * month <= 12; (* month) ++) {
    if (dayOfYear <= daysSoFar + daysInMonth (year, * month)) {
      * day = dayOfYear - daysSoFar;
      return;
    }

    daysSoFar += daysInMonth (year, * month);
  }
}

