////////////////////////////////////////////////////////////////
// VicarImageFile.cc
//
//	This is used to open and read in 1 vicar image file
//	from disk using the Vicar RTL routines.
////////////////////////////////////////////////////////////////
#include "VicarImageFile.h"
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <iostream>
using namespace std;
#include "zvproto.h"
#include "defines.h"
#include "ImageLabel.h"

#define WIDTH 80		/* width of the printout on the screen */
				/* Should be changeable some day */


// Multivalued label item structure
struct multival {
   int nelements;
   int maxlength;
   char *data;			
   int allocsize;
};

int VicarImageFile::_instance = 1;
const int VicarImageFile::_rtlSUCCESS = 1;

static void printKeyValuePair(char *key, struct multival *value,
                              char *format, char *printbuf, char*& label, int *maxsize);
static void flushKeyValuePair(char *printbuf, char*& label, int *maxsize);
static void flushLabelString(const char *printbuf, char*& label, int *maxsize);


////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////
VicarImageFile::VicarImageFile(int fileIndex)
{

   _isOpened = False;
   strcpy(_className, "VicarImageFile");

   _fileIndex = fileIndex;
   _unit = 0;
   _pixelType.set(imBYTE);
   _numbPixelsInDim1 = 0;
   _numbPixelsInDim2 = 0;
   _numbPixelsInDim3 = 0;
   _dimensions = 0;
   _fileOrgString[0] = '\0';
   _fileType[0] = '\0';
   _binLabelType[0] = '\0';
   _bintFmt[0] = '\0';
   _brealFmt[0] = '\0';
   _recordSize = 0;
   _filename = new char[1];
   _filename[0] = '\0';
   _numbLinesInImage = 0;
   _numbSamplesInImage = 0;
   _numbBandsInImage = 0;
   _numBytesPref=0;
   _numBytesHeader=0;
   _host[0] = '\0';

}

////////////////////////////////////////////////////////////////
// nopath() provides the filename without the path
////////////////////////////////////////////////////////////////
void VicarImageFile::nopath(char *filename)
{
   char *value;

#if VMS_OS
   value = strrchr(filename,':');
   if (value != NULL)
    {
      strcpy(filename,value);
      value = &filename[1];
      strcpy(filename,value);
    }
   value = strrchr(filename,']');
   if (value != NULL)
    {
      strcpy(filename,value);
      value = &filename[1];
      strcpy(filename,value);
    }
#else
   value = strrchr(filename,'/');
   if (value != NULL)
    {
      strcpy(filename,value);
      value = &filename[1];
      strcpy(filename,value);
    }
#endif
}

////////////////////////////////////////////////////////////////
// open
//	calls RTL: zvunit & zvopen to open file and saves filename argument.
////////////////////////////////////////////////////////////////
StatusType VicarImageFile::open(char *filename)
{
int	 	status;
char		msg[132];

   status = zvunit(&_unit, _className, _instance++, "u_name", filename, NULL);
   if (status != _rtlSUCCESS) {
      close();
      sprintf( msg, "Unable to zvunit for file: %s\n", filename);
      cerr << msg;
      return  imFAILURE;
   }

   // Open file

   status = zvopen(_unit, "method", "random", "cond", "binary", NULL);
   if (status != _rtlSUCCESS) {
      close();
      sprintf( msg, "Unable to zvopen for file: %s\n", filename);
      cerr << msg;
      return  imFAILURE;
   }  
   else {
      _isOpened = TRUE;
   }

   // Save filename
   delete[] _filename;
   _filename = new char[strlen(filename)+1];
   strcpy( _filename, filename );

   return imSUCCESS; 
}

////////////////////////////////////////////////////////////////
// readImageLabel
//	retrieves info from image label
////////////////////////////////////////////////////////////////
StatusType VicarImageFile::readImageLabel()
{
   int status;
   StatusType return_status = imFAILURE;
   char msg[132];
   char format[12];

   status = zvget(_unit,
		"nl", & _numbLinesInImage,
		"ns", & _numbSamplesInImage,
		"format", &format,
		"nb", &_numbBandsInImage,
		"n1", &_numbPixelsInDim1,
		"n2", &_numbPixelsInDim2,
		"n3", &_numbPixelsInDim3,
		"dim",&_dimensions,
		"org", &_fileOrgString,
		"type",&_fileType,		// "TYPE=IMAGE"
		"bltype", &_binLabelType,	// e.g. "HRSC" or "WAOSS"
		"bintfmt", &_bintFmt,
		"brealfmt", &_brealFmt,
		"recsize",&_recordSize,
		"nbb", &_numBytesPref,
		"nlb", &_numBytesHeader,
		"host", &_host,
		NULL);

   if (strcmp(format,"WORD") == 0 || strcmp(format,"word") == 0)
      strcpy(format, "HALF");			// Handle obsolete type
   if (strcmp(format,"LONG") == 0 || strcmp(format,"long") == 0)
      strcpy(format, "FULL");			// Handle obsolete type

   _pixelType.set(format);

   if (status == _rtlSUCCESS) {
      if (strcmp(_fileType,"IMAGE") != 0)
         return_status = imFAILURE;
      else
         return_status = imSUCCESS;
   }

   // If read was unsuccessful, close file and issue error msg
   else {
      close();
      sprintf(msg, "Unable to read image label (zvget) from: %s\n", _filename);
      cerr << msg;
      return_status = imFAILURE;
   }

   return return_status;
}

////////////////////////////////////////////////////////////////
// close
////////////////////////////////////////////////////////////////
StatusType VicarImageFile::close()
{
   int status;
   char msg[132];

   if (_isOpened) { 
      _isOpened = False;
      status = zvclose(_unit, "clos_act", "free", NULL);
      if (status != _rtlSUCCESS ) {
         sprintf(msg, "Unable to close file: %s\n", _filename);
         cerr << msg;
         return imFAILURE;
      }
   }
   return imSUCCESS;
}

////////////////////////////////////////////////////////////////
// From widget docs
////////////////////////////////////////////////////////////////
static inline int IDIV(int x, int y)
	{ return ((x)>0 ? (x)/(y) : ((x)-(y)+1)/(y)); }

////////////////////////////////////////////////////////////////
// read1Tile1Band
//	reads 1 band of data and stores at mem location pointed
//	to by bufferPtr.
//
//	Note: using image coordinates not zoom coordinates
//
//	buffer_width is the width in *bytes* of the buffer, used so
//	subsequent lines will show up at the right spot.  If only
//	one line is being read, buffer_width may be 0.
//
//	The "band" is 1-based, ready for VICAR; while the
//	startLine/SampleOffset are 0-based like all widget coordinates
//
//	FUTURE:  
//		(1)  Will make use of file format (BSQ, BIL, BIP) for
//			efficiency (i.e. may not want to read in 1 band
//			at a time).
////////////////////////////////////////////////////////////////
StatusType VicarImageFile::read1Tile1Band(int band,
	int startLineOffset, int height, int startSampleOffset, int width,
	ZoomFactor &tileZoom, unsigned char *bufferPtr, int buffer_width)
{
   int status = 0;	// rtlSUCCESS = 1, rtlFAILURE can be < 1;
   StatusType return_status = imFAILURE;
   char msg[132];
   int prezoomLineStart;
   int prezoomLineEnd;
   int line;
   int startSample;
   int nsamps;		// amount to read, not size of buf
   int extra_pixels_on_line = 0;

   // Get start sample and amout to read from args

   startSample = startSampleOffset + 1;	// zvread starts count at 1, not 0
   int endSample = (startSampleOffset + width - 1);	// 0-based
   if (endSample >= _numbSamplesInImage) {
      extra_pixels_on_line = endSample - _numbSamplesInImage;
      endSample = _numbSamplesInImage - 1;
   }
   nsamps = (endSample - startSampleOffset + 1);

   // Get Y zoom factors from args
   // (only work to make Y smaller, not larger - will let widget enlarge)
   // (too much work to zoom out (or in) on samples)

   int ZoomYin = tileZoom.getYIn();
   int ZoomYout = tileZoom.getYOut();
   int subPixelPanY = tileZoom.getSubPixelPanY();
   if (ZoomYout < ZoomYin) {
      ZoomYout = 1;
      ZoomYin = 1;
   }

   // Calculate prezoom coordinates from formulas in widget docs

   prezoomLineStart=
		IDIV((startLineOffset * ZoomYin - subPixelPanY + ZoomYout-1),
		     ZoomYout);
   prezoomLineEnd=
		IDIV(((startLineOffset + height) * ZoomYin - subPixelPanY - 1),
		     ZoomYout);

   // Read one line at a time, applying the zoom factor as we go

   return_status = imSUCCESS;
   for (int i=prezoomLineStart; i <= prezoomLineEnd; i++ ) {

      // Convert from prezoom to unzoomed coordinates
      // +1 is because VICAR starts counting lines at 1
      line = IDIV((i * ZoomYout + subPixelPanY), ZoomYin) + 1;
      if (nsamps > 0 && line > 0 && line <= _numbLinesInImage)
         status = zvread( _unit, bufferPtr, "line", line + _numBytesHeader,
			"samp", startSample + _numBytesPref, "nsamps", nsamps,
			"band", band, NULL);
      else
         status = -1;		// force it out of range

      if ( status == _rtlSUCCESS) {
         if (extra_pixels_on_line) {
            memset((void *)(bufferPtr + nsamps*_pixelType.getPixelSize()),
			0, extra_pixels_on_line * _pixelType.getPixelSize());
         }
      }
      else {
         memset((void *)bufferPtr, 0, width * _pixelType.getPixelSize());
         return_status = imFAILURE;
      }
      bufferPtr += buffer_width;
   }

   // Error message if unable to read
   if (return_status != imSUCCESS) {
      sprintf(msg, "Problem reading file: %s\n", _filename);
      cerr << msg;
   }
   return return_status;  
}

////////////////////////////////////////////////////////////////
// Reads the history label and writes it into the string HistLabel
////////////////////////////////////////////////////////////////
StatusType VicarImageFile::readHistoryLabel(char*& histLabel, int *maxsize)
{
   char format[32];		// The format of a given label item
   int instances[MAX_TASKS];	// Array containing task instances
   char key[MAX_LABEL_KEY_SIZE+1];	// Name of a label item keyword
   int number_of_tasks;		// Number of history subsets in label
   int subset;			// Increment variable for subsets
   char task_names[MAX_TASKS][MAX_LABEL_KEY_SIZE+1]; // names of history subsets
   char time[28];		// Time returned by zlget
   char username[32];		// User field of a given task
   struct multival value;	// Struct that describes the value for the item
   int dummy;			// holds LENGTH from zlinfo... needed since we
				// need the len of the string (via STRLEN
				// optional) instead of the len of the int or
				// real, since everything is treated as a
				// string by the zlget calls.

   char buf[255];		// Data transfer buffer for all routines
   int status; 
   char printbuf[MAX_LABEL_ITEM_SIZE+1];	// Buffer for printing
   char file[120];                 // filename without path

  // get the filename without path for the top line
   strcpy(file, _filename);
   nopath(file);

   printbuf[0] = '\0';		// empty the string buffer
   value.allocsize = 0;
   value.data = NULL;

   // Print a top line for the history label
   sprintf(printbuf,
         "   ************************************************************\n");
   sprintf(buf, 
         "       +++++ History Label of file %s +++++\n", file);
   strcat(printbuf, buf);

   // Get task names of history subsets
   number_of_tasks = MAX_TASKS;		// No more than MAX_TASKS allowed
   status = zlhinfo(_unit, (char *)task_names,instances, &number_of_tasks,
			"ulen", MAX_LABEL_KEY_SIZE+1, NULL);
   if (status <= 0) return imFAILURE;
              
   // Cycle through each subset, listing out all labels
   for (subset = 0; subset < number_of_tasks; subset++) {
      flushKeyValuePair(printbuf, histLabel, maxsize);

      // Get the user and the time (standard task info) for task header
      status = zlget(_unit,(char *)"HISTORY",(char *)"USER",username,
            "HIST",task_names[subset],"INSTANCE",instances[subset], NULL);
      if (status <= 0 && value.data != NULL)
         delete[] value.data;
      if (status <= 0) return imFAILURE;
      status = zlget(_unit,(char *)"HISTORY",(char *)"DAT_TIM",time,
              "HIST",task_names[subset],"INSTANCE",instances[subset], NULL);
      if (status <= 0 && value.data != NULL)
         delete[] value.data;
      if (status <= 0) return imFAILURE;

      // Print out the header for the task
      sprintf(printbuf , "  ---- Task: %s -- User: %s -- %s ----\n",
                task_names[subset],username,time);
                            
      // Set current key to task name
      status = zlinfo(_unit,(char *)"HISTORY",(char *)"TASK",
	    format,&dummy,&value.nelements,
            "HIST",task_names[subset],"INSTANCE",instances[subset],
            "STRLEN", &value.maxlength, NULL);
      if (status <= 0) continue;

      // Cycle through each key in the subset
      while (TRUE) {

         // Get next keyword
         status = zlninfo(_unit,key,format,&dummy,&value.nelements,
                "STRLEN", &value.maxlength, NULL);
         if ((status == END_OF_LABEL) || (strcmp(key,"TASK") == 0)) break;
         if (status <= 0) break;
         value.maxlength++;	// leave room for null string terminator

         // Don't print out DAT_TIM or USER again
         if ((strcmp(key,"DAT_TIM") ==0) || (strcmp(key,"USER") == 0))
            continue;

         // Get next value
         if (value.maxlength * value.nelements > value.allocsize) {
            if (value.data != NULL)
               delete[] value.data;
            value.data = new char[value.maxlength * value.nelements];
            value.allocsize = value.maxlength * value.nelements;
            if (value.data == NULL) {
              cerr << "Out of memory!!!" << endl;
            }
         }
         status = zlget(_unit,(char *)"HISTORY",key, value.data,
                "HIST",task_names[subset], "INSTANCE",instances[subset],
                "FORMAT","STRING", "ULEN", value.maxlength,
                "NELEMENT", value.nelements, NULL);
         if (status <= 0) continue;

         // Print out key and value pair
         printKeyValuePair(key, &value, format, printbuf, histLabel, maxsize);
      }		// End of while loop
   }		// End of for loop

   if (value.data != NULL)
      delete[] value.data;

   flushLabelString(
         "   ************************************************************\n",
	 histLabel, maxsize);
   flushKeyValuePair(printbuf, histLabel, maxsize);

   return imSUCCESS;
}

////////////////////////////////////////////////////////////////
// Reads the entire property label and writes it into the string PropLabel
////////////////////////////////////////////////////////////////
StatusType VicarImageFile::readPropertyLabel(char*& propLabel, int *maxsize)
{
   int instances[MAX_PROPS];         // Array containing task instances
   int number_of_props;		// Number of property subsets in label
   int subset;			// Increment variable for subsets
   char prop_names[MAX_PROPS][MAX_LABEL_KEY_SIZE+1]; // names of prop subsets
   struct multival value;	// Struct that describes the value for the item
   char buf[255];		// Data transfer buffer for all routines
   int status;
   char printbuf[MAX_LABEL_ITEM_SIZE+1];	// Buffer for printing
   char file[120];                 // filename without path
   StatusType retstatus = imSUCCESS;

  // get the filename without path for the top line
   strcpy(file, _filename);
   nopath(file);

   printbuf[0] = '\0';		// empty the string buffer
   value.allocsize = 0;
   value.data = NULL;

   // print a top line for the property label
   sprintf(printbuf,
         "   ************************************************************\n");
   sprintf(buf,
         "       +++++ Property Label of file %s +++++\n", file);
   strcat(printbuf, buf);
   flushKeyValuePair(printbuf, propLabel, maxsize);

   // Get property names of property subsets
   number_of_props = MAX_PROPS;		// No more than MAX_PROPS allowed
   status = zlpinfo(_unit,(char *)prop_names,&number_of_props,
             "inst_num", instances,
             "ulen",MAX_LABEL_KEY_SIZE+1, NULL);
   if (status <= 0) return imFAILURE;

   // Cycle through each subset, listing out all property labels
   for (subset = 0; subset < number_of_props; subset++)
   {
      status = getPropLabel(propLabel, prop_names[subset], 
                  instances[subset], maxsize);
      if (retstatus != imSUCCESS)
         return retstatus;
   }

   if (value.data != NULL)
      delete[] value.data;
     
   strcat(printbuf,
         "   ************************************************************\n");

   flushKeyValuePair(printbuf, propLabel, maxsize);

   return imSUCCESS;
}
  
////////////////////////////////////////////////////////////////
// Reads the system label and writes a system label string into SysLabel
////////////////////////////////////////////////////////////////
StatusType VicarImageFile::readSystemLabel(char*& SysLabel, int *maxsize)
{
   StatusType status;
   char message[255];              // buffer for single system label item 
   char file[120];                 // filename without path 
   char printbuf[MAX_LABEL_ITEM_SIZE+1];        // Buffer for printing

  // get the filename without path for the top line
   strcpy(file, _filename);
   nopath(file);

   status = readImageLabel();      // reads the entire system label
   if(status == imFAILURE) {
      cerr << "Unable to read system label !!" << endl;
      return status;
   }
   sprintf(printbuf,
         "\n   ************************************************************\n");
   sprintf(message, 
         "       +++++ System Label of file %s +++++\n", file);
   strcat(printbuf, message); 

   sprintf(message, "              %d dimensional %s file\n",
                                                  _dimensions, _fileType);
   strcat(printbuf, message);

   sprintf(message, "              File organization is %s\n", _fileOrgString);
   strcat(printbuf, message);

   sprintf(message, "              Pixels are in %s format from a %s host\n",
                                                 _pixelType.getString(), _host);
   strcat(printbuf, message);

   if (_dimensions == 2)
      sprintf(message, "              %d lines\n", _numbLinesInImage);
   else {
      sprintf(message, "              %d bands\n", _numbBandsInImage);
      strcat(printbuf, message);
      sprintf(message, "              %d lines per band\n", _numbLinesInImage);
   }
   strcat(printbuf, message);

   sprintf(message, "              %d samples per line\n", _numbSamplesInImage);
   strcat(printbuf, message);

   if (strlen(_fileType) != 0)
      sprintf(message, "              %d lines of binary header of type %s\n", 
                                              _numBytesHeader, _binLabelType);
   else
      sprintf(message, "              %d lines of binary header\n",
                                              _numBytesHeader);
   strcat(printbuf, message);

   sprintf(message, "              %d bytes of binary prefix per line\n",
                                                   _numBytesPref);
   strcat(printbuf, message);

   sprintf(message,
         "   ************************************************************\n");
   strcat(printbuf, message);

   flushKeyValuePair(printbuf, SysLabel, maxsize);

   return status; 
}

////////////////////////////////////////////////////////////////
//  Reads a property label item from the image
////////////////////////////////////////////////////////////////
LabelType VicarImageFile::readLabelItem(char *Property, char *Label,
				char *Lb_string, int *Lb_int, float *Lb_real)
{
   char lbf[10];		// format of label (INT,REAL,STRING)
   int length;			// Length of label item (bytes) */
   int nel;			// number of elements of item */
   LabelType label_type;
   int status;

   // get the format of label (INT, REAL or STRING)

   status=zlinfo(_unit, (char *)"PROPERTY", Label, lbf, &length, &nel,
                          "PROPERTY", Property, "ERR_ACT", "", NULL);
   if (status != 1)
      return (UNDEFINED_LB);
   if (strcmp(lbf, "REAL") && strcmp(lbf, "INT") && strcmp(lbf, "STRING"))
      return (UNDEFINED_LB);

   if (!strcmp(lbf, "INT")) {
      label_type = INT_LB;
      status=zlget(_unit, (char *)"PROPERTY", Label, (char*) Lb_int,
                        "FORMAT", lbf, "PROPERTY", Property, "ERR_ACT", "", NULL);
      if (status!=1)
         return (UNDEFINED_LB);		// item or property doesn't exist
   }
   if (!strcmp(lbf, "REAL")) {
      label_type = REAL_LB;
      status=zlget(_unit, (char *)"PROPERTY", Label, (char*) Lb_real,
                        "FORMAT", lbf, "PROPERTY", Property, "ERR_ACT", "", NULL);
      if (status!=1)
         return (UNDEFINED_LB);		// item or property doesn't exist
   }
   if (!strcmp(lbf, "STRING")) {
      label_type = STRING_LB;
      status=zlget(_unit, (char *)"PROPERTY", Label, Lb_string,
                        "FORMAT", lbf, "PROPERTY", Property, "ERR_ACT", "", NULL);
      if (status!=1)
         return (UNDEFINED_LB);		// item or property not exist
   }

   return(label_type);		// reading was successful, returns LabelType
}

////////////////////////////////////////////////////////////////
// Reads one binary prefix from the image
////////////////////////////////////////////////////////////////
StatusType VicarImageFile::readPrefix( int band, int Line, unsigned char *buf)
{
   int status;

   status = zvread(_unit, buf, "line", Line + _numBytesHeader,
                              "nsamps", _numBytesPref, "band", band, NULL);
   if (status == 1)
      return imSUCCESS; 
   else
      return imFAILURE;
}

////////////////////////////////////////////////////////////////
// Reads the binary header of the current image file
////////////////////////////////////////////////////////////////
StatusType VicarImageFile::readHeader(int band, unsigned char *buf)
{ 
   int status;

   if (_numBytesHeader != 0) {
      status = zvread( _unit, buf, "line", 1,
				"nsamps", _numBytesHeader, "band", band, NULL);
      if (status == 1)
         return imSUCCESS;
      else
         return imFAILURE; 
   }
   return imFAILURE;
}

////////////////////////////////////////////////////////////////
//  subroutine to flush print buffer for a key-value pair
//  TBD: Technically, malloc (realloc) and new/delete should not be
//  mixed.  Hasn't caused a problem yet, but it might in the future.
////////////////////////////////////////////////////////////////
static void flushKeyValuePair(char *printbuf, char*& labels, int *maxsize)
{
   if (maxsize) {
      if (*maxsize != 0) {    // check to dynamically allocating memory
         while ((int)(strlen(labels) + strlen(printbuf) + 7) > (*maxsize)) {
            *maxsize = 2*(*maxsize);
            labels = (char *)realloc((char *)labels, *maxsize);
         }
      }
      else {
         *maxsize = MAX_IMAGE_LABEL_SIZE+1;
         if ((int)(strlen(printbuf)+1) > (*maxsize))
            *maxsize = strlen(printbuf) + 7;
         labels = new char[*maxsize];
         labels[0] = '\0';
      }
   }

   if (strlen(printbuf) != 0) {
      strcat(labels, "     ");
      strcat(labels, printbuf);
      strcat(labels, "\n");
      printbuf[0]='\0';
   }
}

////////////////////////////////////////////////////////////////
//  Just like flushKeyValuePair except the string is appended
//  unadorned - no extra spaces.  This is to fix problems where
//  some strings were being appended without checking the buffer
//  size, which can overflow the buffer!  Also, printbuf is NOT
//  set to an empty string (it is not modified).
//  TBD: Technically, malloc (realloc) and new/delete should not be
//  mixed.  Hasn't caused a problem yet, but it might in the future.
////////////////////////////////////////////////////////////////
static void flushLabelString(const char *printbuf, char*& labels, int *maxsize)
{
   if (maxsize) {
      if (*maxsize != 0) {    // check to dynamically allocating memory
         while ((int)(strlen(labels) + strlen(printbuf) + 2) > (*maxsize)) {
            *maxsize = 2*(*maxsize);
            labels = (char *)realloc((char *)labels, *maxsize);
         }
      }
      else {
         *maxsize = MAX_IMAGE_LABEL_SIZE+1;
         if ((int)(strlen(printbuf)+1) > (*maxsize))
            *maxsize = strlen(printbuf) + 2;
         labels = new char[*maxsize];
         labels[0] = '\0';
      }
   }

   if (strlen(printbuf) != 0) {
      strcat(labels, printbuf);
   }
}
////////////////////////////////////////////////////////////////
//  subroutine to print out a key-value pair 
////////////////////////////////////////////////////////////////
static void printKeyValuePair(char *key, struct multival *value,
                              char *format, char *printbuf, char*& label, int *maxsize)
{
   int i,length;

   // If packing, make sure key and at least one element will fit on the line
   length = strlen(key) + strlen(value->data) + 8;   // len of key + 1 elem
   if ((strlen(printbuf)!=0) && (strlen(printbuf)+length >= WIDTH)) {
      flushLabelString(printbuf, label, maxsize);	// flush old buffer
      flushLabelString("\n", label, maxsize);
      printbuf[0] = '\0';
   }

   if (strlen(printbuf) != 0)
      strcat(printbuf, "  ");		// two spaces between items
   strcat(printbuf, key);
   strcat(printbuf, "=");

   if (value->nelements > 1)		// multivalued
      strcat(printbuf, "(");

   for (i=0; i<value->nelements; i++) {
      length = strlen(value->data+(i*value->maxlength)) + 4;
      if ((strlen(printbuf)!=0) && (strlen(printbuf)+length >= WIDTH)) {
         //zvmessage(printbuf, "");	// flush old buffer
         flushKeyValuePair(printbuf, label, maxsize);
      }

      if (*format == 'S')
         strcat(printbuf, "'");
      strcat(printbuf, value->data+(i*value->maxlength));
      if (*format == 'S')
         strcat(printbuf, "'");
      if (i != value->nelements-1)
         strcat(printbuf, ", ");
   }

   if (value->nelements > 1)
      strcat(printbuf, ")");

   flushKeyValuePair(printbuf, label, maxsize);

}

MP VicarImageFile::readMPInfoFromFile() 
{
  // successful return values: 1 for VICAR (zv), 0 for MP (mp)
  
  int vicStatus, mpStatus, unit;
  MP mpObject = NULL;
  
  vicStatus = 1;

  if (_isOpened != True) {

    vicStatus = zvunit(&unit, (char *)"in_file", 1, "u_name", _filename, NULL);
    if (vicStatus != 1) {
      cerr << "Unable to zvunit for file: " << _filename << "\n";
      return NULL;
    }

    // open the file for reading
    vicStatus = zvopen(unit, "OP", "READ", "U_FORMAT", "BYTE", NULL);
    if (vicStatus != 1) {
      cerr << "Unable to zvopen for file " << _filename << "\n";
      return NULL;
    }
  }
  else  // it *is* already open, so use the existing unit #
    unit = _unit;

#ifndef NO_MP_ROUTINES
  mpStatus = mpInit(&mpObject);   // create the object
  if (mpStatus == 0)
    mpStatus = mpLabelRead(mpObject, unit);  // read data into the object
#endif  

  if (_isOpened != True)
    vicStatus = zvclose(unit, NULL);
  
  if ((vicStatus != 1) || (mpStatus != 0))
    return NULL;

  return mpObject;
}

//////////////////////////////////////////////////////////
// Read in Vicar image labels
//////////////////////////////////////////////////////////
StatusType VicarImageFile::readVicarLabel(char*& labels, 
   VicarLabelType type, char *set, int instance, int *maxsize) 
{
   StatusType status = imFAILURE;

   switch (type) {
      case V_LISTALL:
         status = readSystemLabel(labels, maxsize);
         if (status != imSUCCESS )
            return status;
         flushLabelString("\n", labels, maxsize);
         status = readPropertyLabel(labels, maxsize);         
         if (status != imSUCCESS )
            return status;
         flushLabelString("\n", labels, maxsize);
         status = readHistoryLabel(labels, maxsize);         
         if (status != imSUCCESS )
            return status;
         flushLabelString("\n", labels, maxsize);
         break;
      case V_DUMPALL:
         strcpy(set, "_all");
         status = readSysLabel(labels, maxsize);
         if (status != imSUCCESS )
            return status;
         flushLabelString("\n", labels, maxsize);
         status = readPropLabel(labels, set, instance, maxsize);
         if (status != imSUCCESS )
            return status;
         flushLabelString("\n", labels, maxsize);
         status = readHistLabel(labels, set, instance, maxsize);
         if (status != imSUCCESS )
            return status;
         flushLabelString("\n", labels, maxsize);
         break;
      case V_SYSTEM:
         status = readSysLabel(labels, maxsize);
         if (status != imSUCCESS )
            return status;
         break;
      case V_PROPERTY:
         status = readPropLabel(labels, set, instance, maxsize);
         if (status != imSUCCESS )
            return status;
         break;
      case V_HISTORY:
         status = readHistLabel(labels, set, instance, maxsize);
         if (status != imSUCCESS )
            return status;
         break;
      default:
         return imFAILURE; 
   }

   return status;
}


////////////////////////////////////////////////////////////////
// Reads the history label and writes it into the string HistLabel
////////////////////////////////////////////////////////////////
StatusType VicarImageFile::readHistLabel(char*& histLabel, 
           char *taskname, int instance, int *maxsize)
{
   int instances[MAX_TASKS];         // Array containing task instances
   char task_names[MAX_TASKS][MAX_LABEL_KEY_SIZE+1]; // names of hist subsets
   int number_of_tasks;              // Number of history subsets in label
   int subset;                       // Increment variable for subsets
   char msg[132];                    // filename without path
   StatusType retstatus=imSUCCESS;
   int status;
   char file[120];                 // filename without path

  // get the filename without path for the top line
   strcpy(file, _filename);
   nopath(file);

   if (strcmp(taskname, "_all") == 0) {
      number_of_tasks = MAX_TASKS;         // No more than MAX_TASKS allowed
      status = zlhinfo(_unit,(char *)task_names, instances, 
                &number_of_tasks,
                "ulen",MAX_LABEL_KEY_SIZE+1, NULL);
      if (status <= 0) {
         sprintf( msg, "Unable to zlhinfo for file: %s\n", file);
         cerr << msg;
         return imFAILURE; 
      }

      /* Cycle through each subset, listing out all history labels */
      for (subset = 0; subset < number_of_tasks; subset++)
      {
         retstatus = getHistLabel(histLabel, task_names[subset], instances[subset], maxsize);
      }  
   } 
   else
      retstatus = getHistLabel(histLabel, taskname, instance, maxsize);

   return retstatus;
}

////////////////////////////////////////////////////////////////
// Reads the history label and writes it into the string HistLabel
////////////////////////////////////////////////////////////////
StatusType VicarImageFile::getHistLabel(char*& hLabel, 
           char *taskname, int instance, int *maxsize)
{
   char format[32];             // The format of a given label item
   char key[MAX_LABEL_KEY_SIZE+1];      // Name of a label item keyword
   struct multival value;       // Struct that describes the value for the item
   int dummy;                   // holds LENGTH from zlinfo... needed since we
                                // need the len of the string (via STRLEN
                                // optional) instead of the len of the int or
                                // real, since everything is treated as a
                                // string by the zlget calls.

   int status;
   char printbuf[MAX_LABEL_ITEM_SIZE+1];        // Buffer for printing
   char file[120];                 // filename without path

  // get the filename without path for the top line
   strcpy(file, _filename);
   nopath(file);

   printbuf[0] = '\0';          // empty the string buffer
   value.allocsize = 0;
   value.data = NULL;

   flushKeyValuePair(printbuf, hLabel, maxsize);

   // Set current key to task name
   status = zlinfo(_unit,(char *)"HISTORY",(char *)"TASK",
	 format,&dummy,&value.nelements,
         "HIST",taskname, "INSTANCE",instance,
         "STRLEN", &value.maxlength, NULL);
   if (status <= 0) return imFAILURE;

   sprintf(printbuf, 
   "---------------------------------------------\n\n   TASK = '%s'\n", taskname);
   flushKeyValuePair(printbuf, hLabel, maxsize);

   // Cycle through each key in the subset
   while (TRUE) {

      // Get next keyword
      status = zlninfo(_unit,key,format,&dummy,&value.nelements,
             "STRLEN", &value.maxlength, NULL);
      if ((status == END_OF_LABEL) || (strcmp(key,"TASK") == 0)) break;
      if (status <= 0) break;
      value.maxlength++;     // leave room for null string terminator

      // Get next value
      if (value.maxlength * value.nelements > value.allocsize) {
         if (value.data != NULL)
            delete[] value.data;
         value.data = new char[value.maxlength * value.nelements];
         value.allocsize = value.maxlength * value.nelements;
         if (value.data == NULL) {
            cerr << "Out of memory!!!" << endl;
         }
      }
      status = zlget(_unit,(char *)"HISTORY",key, value.data,
             "HIST",taskname, "INSTANCE",instance,
             "FORMAT","STRING", "ULEN", value.maxlength,
             "NELEMENT", value.nelements, NULL);
      if (status <= 0) continue;

      // Print out key and value pair
      printKeyValuePair(key, &value, format, printbuf, hLabel, maxsize);
   }  

   if (value.data != NULL)
      delete[] value.data;

   flushKeyValuePair(printbuf, hLabel, maxsize);

   return imSUCCESS;
}


////////////////////////////////////////////////////////////////
// Reads the property labels
////////////////////////////////////////////////////////////////
StatusType VicarImageFile::readPropLabel(char*& propLabel,
           char *propname, int instance, int *maxsize)
{
   int instances[MAX_PROPS];        // Array containing prop instances
   char prop_names[MAX_PROPS][MAX_LABEL_KEY_SIZE+1]; // names of prop subsets
   int number_of_props;             // Number of propory subsets in label
   int subset;                      // Increment variable for subsets
   char msg[132];                   // filename without path
   StatusType retstatus = imSUCCESS;
   int status;
   char file[120];                 // filename without path

  // get the filename without path for the top line
   strcpy(file, _filename);
   nopath(file);

   if (strcmp(propname, "_all") == 0) {
      number_of_props = MAX_PROPS;      // No more than MAX_PROPS allowed
      status = zlpinfo(_unit,(char *)prop_names,&number_of_props,
                "inst_num", instances,
                "ulen",MAX_LABEL_KEY_SIZE+1, NULL);
      if (status <= 0) {
         sprintf( msg, "Unable to zlpinfo for file: %s\n", file);
         cerr << msg;
         return imFAILURE;
      }

      // Cycle through each subset, listing out all property labels 
      for (subset = 0; subset < number_of_props; subset++)
      {
         retstatus = getPropLabel(propLabel, prop_names[subset], instances[subset], maxsize);
      }
   }
   else
      retstatus = getPropLabel(propLabel, propname, instance, maxsize);

   return retstatus;
}

////////////////////////////////////////////////////////////////
// Reads the entire property label and writes it into the string PropLabel
////////////////////////////////////////////////////////////////
StatusType VicarImageFile::getPropLabel(char*& propLabel,
           char *propName, int instance, int *maxsize)
{
   char format[32];             // The format of a given label item
   char key[MAX_LABEL_KEY_SIZE+1];      // Name of a label item keyword
   struct multival value;       // Struct that describes the value for the item
   int dummy;                   // holds LENGTH from zlinfo... needed since we
                                // need the len of the string (via STRLEN
                                // optional) instead of the len of the int or
                                // real, since everything is treated as a
                                // string by the zlget calls.

   int status;
   char printbuf[MAX_LABEL_ITEM_SIZE+1];        // Buffer for printing
   printbuf[0] = '\0';          // empty the string buffer
   value.allocsize = 0;
   value.data = NULL;

   // List out all labels in the subset
   flushKeyValuePair(printbuf, propLabel, maxsize);

   // Set current key to task name
   status = zlinfo(_unit,(char *)"PROPERTY",(char *)"PROPERTY",format,&dummy,
             &value.nelements,"PROPERTY",propName,
             "INSTANCE", instance,
             "STRLEN", &value.maxlength, NULL);

   if (status <= 0) return imFAILURE;

   sprintf(printbuf,
   "---------------------------------------------\n\n   PROPERTY = '%s'\n", propName);
   flushKeyValuePair(printbuf, propLabel, maxsize);

   // Cycle through each key in the subset
   while (TRUE) {

      // Get next keyword
      status = zlninfo(_unit,key,format,&dummy,&value.nelements,
             "STRLEN", &value.maxlength, NULL);
      if ((status == END_OF_LABEL) || (strcmp(key,"TASK") == 0) ||
             (strcmp(key,"PROPERTY") == 0)) break;
      if (status <= 0) break;
      value.maxlength++;     // leave room for null string terminator

      // Get next value
      if (value.maxlength * value.nelements > value.allocsize) {
         if (value.data != NULL)
            delete[] value.data;
         value.data = new char[value.maxlength * value.nelements];
         value.allocsize = value.maxlength * value.nelements;
         if (value.data == NULL) {
            cerr << "Out of memory!!!" << endl;
         }
      }
      status = zlget(_unit,(char *)"PROPERTY",key,value.data,
             "PROPERTY",propName, "FORMAT","STRING",
             "ULEN", value.maxlength, "NELEMENT", value.nelements, NULL);
      if (status <= 0) continue;

      // Print out key and value pair
      printKeyValuePair(key,&value,format,printbuf, propLabel, maxsize);
   }         // End of while loop

   if (value.data != NULL)
      delete[] value.data;

   flushKeyValuePair(printbuf, propLabel, maxsize);

   return imSUCCESS;
}

////////////////////////////////////////////////////////////////
// Reads the system labels and writes them into SysLabel
////////////////////////////////////////////////////////////////
StatusType VicarImageFile::readSysLabel(char*& sysLabel, int *maxsize)
{
   char format[32];             // The format of a given label item
   char key[MAX_LABEL_KEY_SIZE+1];      // Name of a label item keyword
   struct multival value;       // Struct that describes the value for the item
   int dummy;                   // holds LENGTH from zlinfo... needed since we
                                // need the len of the string (via STRLEN
                                // optional) instead of the len of the int or
                                // real, since everything is treated as a
                                // string by the zlget calls.

   int status;
   char printbuf[MAX_LABEL_ITEM_SIZE+1];    // Buffer for printing

   // make the first key in system label the current key
   status = zlinfo(_unit,(char *)"SYSTEM",(char *)"LBLSIZE",
	 format,&dummy,&value.nelements,
         "STRLEN", &value.maxlength, NULL);
   if (status <= 0) return imFAILURE;

   printbuf[0] = '\0';          // empty the string buffer
   value.allocsize = 0;
   value.data = NULL;

   strcpy(key, "LBLSIZE");      // the first key in system labels

   // Cycle through system labels
   while (TRUE) {

      // Get next value
      if (value.maxlength * value.nelements > value.allocsize) {
         if (value.data != NULL)
            delete[] value.data;
         value.data = new char[value.maxlength * value.nelements];
         value.allocsize = value.maxlength * value.nelements;
         if (value.data == NULL) {
            cerr << "Out of memory!!!" << endl;
         }
      }
      status = zlget(_unit,(char *)"SYSTEM",key,value.data, "FORMAT","STRING",
             "ULEN", value.maxlength, "NELEMENT", value.nelements, NULL);
      if (status <= 0) continue;

      // Print out key and value pair
      printKeyValuePair(key,&value,format,printbuf, sysLabel, maxsize);

      // Get next keyword
      status = zlninfo(_unit,key,format,&dummy,&value.nelements,
             "STRLEN", &value.maxlength, NULL);
      if ((status == END_OF_LABEL) || (strcmp(key,"TASK") == 0) ||
             (strcmp(key,"PROPERTY") == 0)) break;
      if (status <= 0) break;
      value.maxlength++;     // leave room for null string terminator

   }

   if (value.data != NULL)
      delete[] value.data;

   flushKeyValuePair(printbuf, sysLabel, maxsize);

   return imSUCCESS;

}

////////////////////////////////////////////////////////////////
// Read in all labels and construct the internal label tree
////////////////////////////////////////////////////////////////
ImageLabel *VicarImageFile::buildLabelTree(ImageData *image, int fileIndex)
{
   char            msg[132];
   int pinstances[MAX_TASKS];   // Array containing property instances
   int hinstances[MAX_TASKS];   // Array containing task instances
   char key[MAX_STRING_SIZE+1]; // Identifier for a ImageLabel set
   int number_of_props;         // Number of property subsets in label
   int number_of_tasks;         // Number of history subsets in label
   int subset;                  // Increment variable for subsets
   char prop_names[MAX_TASKS][MAX_LABEL_KEY_SIZE+1]; // names of prop subsets
   char task_names[MAX_TASKS][MAX_LABEL_KEY_SIZE+1]; // names of hist subsets
   int status;
   char printbuf[MAX_LABEL_ITEM_SIZE+1];        // Buffer for printing
   char file[120];                 // filename without path

  // get the filename without path for the top line
   strcpy(file, _filename);
   nopath(file);

   printbuf[0] = '\0';          // empty the string buffer

   sprintf(key, "%d", fileIndex);
   ImageLabel *labelRoot = new ImageLabel(image, file, key);
   sprintf(key, "%d/%s", fileIndex, "list");
   _allList = new ImageLabel(image, "List All", key);
   sprintf(key, "%d/%s", fileIndex, "dump");
   _allDump = new ImageLabel(image, "Dump All", key);
   sprintf(key, "%d/%s", fileIndex, "system");
   _systemLabel = new ImageLabel(image, "System", key);
   sprintf(key, "%d/%s", fileIndex, "property");
   _propertyLabel = new ImageLabel(image, "Property", key); 
   sprintf(key, "%d/%s", fileIndex, "history");
   _historyLabel = new ImageLabel(image, "History", key); 
   labelRoot->addChild(_allList);
   labelRoot->addChild(_allDump);
   labelRoot->addChild(_systemLabel);

   // Get property names of property subsets
   number_of_props = MAX_PROPS;         // No more than MAX_PROPS allowed
   status = zlpinfo(_unit,(char *)prop_names, &number_of_props,
                "inst_num", pinstances,
                "ulen",MAX_LABEL_KEY_SIZE+1, NULL);

   if (status <= 0) {
      sprintf( msg, "Unable to zlpinfo for file: %s\n", file);
      cerr << msg;
      number_of_props = 0;
   }

   // Get task names of history subsets
   number_of_tasks = MAX_TASKS;         // No more than MAX_TASKS allowed
   status = zlhinfo(_unit, (char *)task_names,hinstances, &number_of_tasks,
                        "ulen", MAX_LABEL_KEY_SIZE+1, NULL);
   if (status <= 0) {
      sprintf( msg, "Unable to zlhinfo for file: %s\n", file);
      cerr << msg;
      number_of_tasks = 0;
   }

   if (number_of_props > 0)
      labelRoot->addChild(_propertyLabel);
   if (number_of_tasks > 0)
      labelRoot->addChild(_historyLabel);

   if (number_of_props > 1) {
      sprintf(key, "%d/property/_all", fileIndex);
      strcat(key, "\0");

      ImageLabel *propLabel = new ImageLabel(image, "All", key);
      _propertyLabel->addChild(propLabel);
   }

   // Cycle through each property subset, building ImageLabel nodes
   for (subset = 0; subset < number_of_props; subset++) {
      sprintf(key, "%d/property/%s/%d", fileIndex, 
              prop_names[subset], pinstances[subset]);          
      strcat(key, "\0");

      ImageLabel *propLabel = new ImageLabel(image, prop_names[subset], key);
      _propertyLabel->addChild(propLabel);
   }

   if (number_of_tasks > 1) {
      sprintf(key, "%d/history/_all", fileIndex);
      strcat(key, "\0");

      ImageLabel *histLabel = new ImageLabel(image, "All", key);
      _historyLabel->addChild(histLabel);
   }

   // Cycle through each history subset, building ImageLabel nodes
   for (subset = 0; subset < number_of_tasks; subset++) {
      sprintf(key, "%d/history/%s/%d", fileIndex, 
              task_names[subset], hinstances[subset]);          
      strcat(key, "\0");

      ImageLabel *histLabel = new ImageLabel(image, task_names[subset], key);
      _historyLabel->addChild(histLabel);
   }

   return labelRoot;
}

