#ifndef _loggerutils_h_
#define _loggerutils_h_

#include <stdlib.h>
#include "ibisfile.h"
#include "zvproto.h"

#define _loggerutils_version_ 22

void checkLoggerUtilsVersion (int minimum);

/* Version Changes:
   22 Enhanced labelRaw16bitImage to pad short images
   21 Changed labelRaw16bitImage to complain and return when fread fails
   20 Added logMetaDoubleProperty
   19 Switched IBIS organization from row to column
   18 Added noQuotes parm to logMetaStringToProperty
   17 Allowed metadata to log to arbitrary label
   16 Added raw image labeler
   15 Enhanced usingLatLonSubArea to allow specification of only one lat/lon limit
   14 Added shortSwapBytes to convert shorts from 386 architecture
   13 Added echoMeta flag to logMeta* to echo to stdout via zifmessage
   12 Added more HDF declarations and azimuth and zenith columns to IBIS file
      Added checkedMalloc function
   11 Added HDF declarations
   10 Added daysInMonth (from sunup.c)
      Added dateToDayOfYear (from sunup.c)
      Added dayOfYearToDate (from sunup.c)
    9 Added null path option to logMeta* to inhibit logging to file
    8 Added VICAR image labeling with meta data interface; replaced getTimesData with mallocAndRead
    7 Added zvread to missing VICAR declarations
    6 Removed metaFileName from getTimesData args; replaced logDataSetTime with getDataSetTime
    5 Added geocentricToGeodetic
    4 Added logDayTimeFlag, getTimesData, logDataSetTime (factored out of avhrrlog and avhrrllog)
    3 Added forceSubAreaSanity
    2 Added meta data interface and lat/long sub-area parameters
      Added lat/lon specified sub-area
    1 Initial version Mon Dec  3 2001
  */

short shortSwapBytes (short s); /* to convert shorts from 386 architecture */

/* calls stdlib malloc, calls abend on null returned pointer; description used for error message */
void * checkedMalloc (size_t size, char * description);

#ifndef MAX
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#endif
#ifndef MIN
#define MIN(a, b) ((a) < (b) ? (a) : (b))
#endif

#define BOUNDED(val, min, max) (MIN ((max), MAX ((min), (val))))

char * mallocAndRead (char * path);
double geocentricToGeodetic (double latitude);
void getDataSetTime (char * dataSetName, char * times, int * utcTimeInMinutes, char * date, char * time);

/* Ensures sub area make sense for image shape.
   sl, ss are forced to be within the image
   if nl or ns are < 1 > possible, they are forced to include all image available
 */
void forceSubAreaSanity (int * sl, int * ss, int * nl, int * ns, int lines, int samples);

/* Allows lat/lon specified sub-area
   Assumes that sl, ss, nl, ns default to -999
   Assumes that minLat, maxLat, minLon, maxLon default to -999.0
   Assumes that exactly one of the following is true:
      1 All line/sample values and no lat/lon values are specified; returns 0
      2 All lat/lon values and no line/sample values are specified; returns 1
      3 No line/sample values or lat/lon values are specified; returns 0
 */
int usingLatLonSubArea (int imageLines, int imageSamples,
		       double * minLat, double * maxLat, double * minLon, double * maxLon,
		       int * sl, int * ss, int * nl, int * ns, int allow);

/* Meta data interface
   initMetaData must be called before meta data is logged. The path names the
   meta data file. unitCount indicates the number of vunits for the data to be 
   added to the vicar label as Property "loggerMetaData", Key name, Value value.
   MetaToLabel stuffs the whole contents of the named meta file into a label named
   ALL_META_DATA.
*/

void initMetaData (char * path);
void logMetaString (int echoMeta, char * path, char * name, char * value, int unitCount, int * vunits);
void logMetaStringToProperty (int echoMeta, char * path, char * name, char * value, int unitCount, int * vunits, char * property, int noQuotes);
void logMetaInt (int echoMeta, char * path, char * name, int value, int unitCount, int * vunits);
void logMetaDouble (int echoMeta, char * path, char * name, double value, int unitCount, int * vunits);
void logMetaDoubleToProperty (int echoMeta, char * path, char * name, double value, int unitCount, int * vunits, char * property);

void metaToLabel (char * metaName, int vunit);
void addGTKey (int vunit, char * key, char * value);

/* IBIS navigation data support */
int logNavDataToIBIS (char * zvunitParmName, int zvunitParmIndex,
		      int numRows,
		      double * lineColumn, double * sampleColumn,
		      double * latColumn, double * lonColumn);
int logNavAndAnglesDataToIBIS (char * zvunitParmName, int zvunitParmIndex,
			       int numRows,
			       double * lineColumn, double * sampleColumn,
			       double * latColumn, double * lonColumn,
			       double * zenithColumn, double * azimuthColumn);

/* returns vunit on open image; must be zvclosed */
int labelRaw16bitImage (char * inpath, char * outpath, int nl, int ns);

/* Time functions */
int daysInMonth (int year, int month);

/* dayOfYear is 1-based, e.g. dayOfYear==1 => January 1 */
int dateToDayOfYear (int year, int month, int day);
void dayOfYearToDate (int dayOfYear, int year, int * month, int * day);

/* Misc macros */
#ifndef MIN
#define MIN(x, y) ((x) < (y) ? (x) : (y))
#endif

#ifndef MAX
#define MAX(x, y) ((x) > (y) ? (x) : (y))
#endif

#define BETWEEN(left, middle, right) ((left) <= (middle) && (middle) <= (right))

#endif

