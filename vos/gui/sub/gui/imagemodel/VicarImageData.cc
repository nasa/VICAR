//////////////////////////////////////////////////////////////////
// VicarImageData.cc
//
//	This is a subclass of ImageData.  It retrieves data
//	from VICAR formatted files.  
//
//	FUTURE ENHANCEMENTS: 
//		fill in initFileZoom() code
////////////////////////////////////////////////////////////////
#include "ImageData.h"
#include "ImageLabel.h"
#include "VicarImageData.h"
#include "VicarImageFile.h"
#include "ZoomFactor.h"
#include "zvproto.h"
#include "BasicComponent.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <iostream>
using namespace std;

const int MSG_SIZE  = 512;

////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////
VicarImageData::VicarImageData() : ImageData()
{
   int i;

   _numbFiles = 0;
   _numbChannels = 0;

   for (i=0; i<MAX_FILES; i++)
      _files[i] = NULL;

   for (i=0; i<MAX_CHANS; i++) {
      _fileForChannel[i] = NULL;
      _bandForChannel[i] = 0;
   }
   _mpObject = NULL;
}

////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////
VicarImageData::~VicarImageData() 
{
   // Delete file objects (files closed automatically)
   for (int i=0; i< _numbFiles; i++)
      delete _files[i];
   if (_mpObject != NULL)
     mpFree(_mpObject);
}

////////////////////////////////////////////////////////////////
// allows file access to the user
////////////////////////////////////////////////////////////////
VicarImageFile *VicarImageData::getVicarFile(int index)
{
   if (index > _numbFiles)
      return NULL;
   else
      return _files[index];
}
    
////////////////////////////////////////////////////////////////
// open
//	Creates 1 to 3 file objects.
//	stringOfFilenames argument can have 1 to 3 filenames
//	in a single string separated by commas or whitespace.  An optional
//	band number can follow a filename in parentheses (with no space).
//	Names that start with a "." but don't have any "/"s (or ":" or "]"
//	for VMS) in them are abbreviations meaning to take the previous
//	filename, strip off the extension (actually, everything after the
//	last period), and add the given extension.  If the band number is
//	not given, one is assigned automatically (the usual cases are three
//	files with one band each or one file with three bands).
//
// examples:
// "/usr/local/images/io.red,/usr/local/images/io.grn,/usr/local/images/io.blu"
// "/usr/local/images/io.red .grn  .blu"
// "nims.cube(3),nims.cube(10),nims.cube(33)"
// "/usr/local/images/big.img"	(three bands in one file)
// "/usr/local/images/big.img(2)"	(one band from above as bw)
// "/tmp/F_65N002.L"		(b&w image)
//
// VMS:
// "images:io.red images:io.grn images:io.blu"
// "disk:[mydata.project]file.red,.grn,.blu"
// "disk:[project.gll.nims]nims.cube(2), .cube(3), .cube(4)"
////////////////////////////////////////////////////////////////
StatusType VicarImageData::open(char *stringOfFilenames)
{
   StatusType status, return_status = imSUCCESS;
   char filename[2048];
   char *p, *input_string;
   int i;

   // Close previous files
   close();

   // Separate string into individual filenames and process each.

   _numbChannels = 0;		// 0==RED/BW, 1==GREEN, 2==BLUE
   _numbFiles = 0;
   for (i=0; i<MAX_FILES; i++)
      _files[i] = NULL;
   for (i=0; i<MAX_CHANS; i++) {
      _fileForChannel[i] = NULL;
      _bandForChannel[i] = 0;
   }
   input_string = stringOfFilenames;
   strcpy(filename, "");

   // Set up label tree root
   _labelTree = new ImageLabel(this, "LABELS", "vicarlabeltreeroot");

   while (input_string && (strlen(input_string) > 0)) {

      int band = 0;

      // Search for comma or whitespace
      int len = (int)strcspn(input_string, ", \t\n");

      // Check for ".grn" style abbreviation.  Must start with . and no /
      p = filename;
      if (*input_string == '.' && ((int)strcspn(input_string,"/:]")) >= len) {
         // Find the extension in the previous filename and replace it
         p = strrchr(filename, '.');	// is there a '.'?
         if (p == NULL)
            p = filename;		// no '.', so copy whole string
      }
      strncpy(p, input_string, len);
      *(p+len) = '\0';

      // Check for band number
      if ((p = strchr(filename, '('))) {	// don't bother checking for ')'
         band = atoi(p+1);
         *p = '\0';		// strip off the band
      }

      // Add it to the list.  If no band is given, several may be used

      status = addFile(filename, band);
      if (status != imSUCCESS)
         return_status = status;

      input_string += len;		// Skip this filename
      // Now skip past the comma (if present) and any whitespace on
      // either side.  But, we want to make sure that there is only one
      // comma skipped. (for degenerate cases like "file.red,,file.blu"
      // which may or may not work with everything else).
      input_string += strspn(input_string, " \t\n"); // Skip whitespace
      if (*input_string == ',')
         input_string++;
      input_string += strspn(input_string, " \t\n"); // Skip whitespace
   }

   // Init internal info based on label

   // Make sure we don't have empty channels at the end
   for (i=_numbChannels-1; i>=0; i--) {
      if (_fileForChannel[i] == NULL)
         _numbChannels--;
      else
         break;		// found a non-empty one
   }

   if (_numbChannels == 0)
      _mode = UNDEFmode;
   else if (_numbChannels == 1)
      _mode = BWmode;
   else {
      _mode = COLORmode;
      _numbChannels = 3; // In case we *do* have empty channels at the end
   }

   status = initNumbers();
   if (status != imSUCCESS)
      return_status = status;

   strcpy(_inputDataSourceName, stringOfFilenames);

   // Load the map projection information, if any:
   readMapProjInfo();

   initDataRange();

   updateViews();

   return (return_status);
}

////////////////////////////////////////////////////////////////
// close()
////////////////////////////////////////////////////////////////
StatusType VicarImageData::close() 
{
   int i;

   for (i=0; i< _numbFiles; i++ ) {
      delete _files[i]; 
      _files[i] = NULL;
   }
   for (i=0; i<MAX_CHANS; i++ )
      _fileForChannel[i] = NULL;

   _numbFiles = 0;
   _numbChannels = 0;
   strcpy(_inputDataSourceName,"\0");
   _dataSourceOpened = FALSE;

   _errorMsgIssued = False;	// Clear the error so it can be reissued
   if (_errorMsg)
      delete []_errorMsg;
   _errorMsg = NULL;

   return imSUCCESS;
} 

////////////////////////////////////////////////////////////////
// addFile (Private)
//	Called by open for each new filename.  If a band is given,
//	add that file/band combo to the list.  If no band is given
//	(i.e. it's 0), add as many bands as needed (and available)
//	from the file to fill up the color slots.  An empty filename
//	(not NULL but 0-length) will simply set a NULL for the
//	file pointer and a 0 for the band.
////////////////////////////////////////////////////////////////
StatusType VicarImageData::addFile(char *filename, int band)
{
   StatusType return_status = imSUCCESS;
   char msg[MSG_SIZE];
   int i;

   if (filename == NULL)
      return imFAILURE;

   if (_numbChannels >= MAX_CHANS) {
      if (strlen(filename) > 0) {
         sprintf( msg, "Too many filenames at:  %s  \n", filename );
         setErrorMsg(msg);
         cerr << msg;
         return imFAILURE;
      }
      return imSUCCESS;
   }

   if (strlen(filename) == 0) {
      _fileForChannel[_numbChannels] = NULL;
      _bandForChannel[_numbChannels] = 0;
      _numbChannels++;
      return imSUCCESS;
   }

   // See if the file matches one already used

   for (i = 0; i<_numbFiles; i++) {
      if (_files[i] && strcmp(_files[i]->getName(), filename) == 0) {
         _fileForChannel[_numbChannels] = _files[i];
         break;
      }
   }

   if (_fileForChannel[_numbChannels] == NULL) {

      // Instantiate VicarImageFile class for this file

      _files[_numbFiles] = new VicarImageFile(_numbFiles);
      return_status = _files[_numbFiles]->open( filename );

      // Read image label

      if (return_status == imSUCCESS ) 
         return_status = _files[_numbFiles]->readImageLabel();

      // If unable to open, print error

      if ( return_status == imFAILURE ) {
         sprintf( msg, "Unable to load file:  %s  \n", filename );
         setErrorMsg(msg);
         cerr << msg;
      }
      else 
         _dataSourceOpened = True;

      // Read in all labels and build the internal label tree

      ImageLabel *lTree = _files[_numbFiles]->buildLabelTree(this, _numbFiles);
      _labelTree->addChild(lTree);

      _fileForChannel[_numbChannels] = _files[_numbFiles];
      _numbFiles++;
   }

   if (band != 0) {		// We're given a band
      if (band <= _fileForChannel[_numbChannels]->getNumberOfBands())
         _bandForChannel[_numbChannels] = band;
      else {
         sprintf(msg, "Invalid band (%d), 1 assumed, for file:  %s\n",
					band, filename);
         setErrorMsg(msg);
         cerr << msg;
         _bandForChannel[_numbChannels] = 1;
         return_status = imFAILURE;
      }
   }
   else {				// Not given a band, figure it out
      _bandForChannel[_numbChannels] = 1;
      int bandsLeft = _fileForChannel[_numbChannels]->getNumberOfBands()-1;

      // If we're not full of channels and there are bands left in this,
      // file, copy the file to the next channel and increment the band

      while (_numbChannels < MAX_CHANS-1 && bandsLeft > 0) {
         _numbChannels++;
         bandsLeft--;
         _fileForChannel[_numbChannels] = _fileForChannel[_numbChannels-1];
         _bandForChannel[_numbChannels] = _bandForChannel[_numbChannels-1] + 1;
      }
   }

   _numbChannels++;

   return (return_status);
}

////////////////////////////////////////////////////////////////
// getSuggestedUnzoomedTileSize
//	returns the recommended tile size.
//	For VICAR, this will be the entire sample width by 100
//	lines (unzoomed), always.
////////////////////////////////////////////////////////////////

StatusType VicarImageData::getSuggestedUnzoomedTileSize(int &height, int &width)
{
   StatusType result = imFAILURE;

   if (_numbChannels >= 0 && _numbChannels <= MAX_CHANS) result=imSUCCESS;
   height = 100;
   if ( height > _numberOfLines ) height = _numberOfLines;
   width = _numberOfSamples;
   return (result);
}

////////////////////////////////////////////////////////////////
// initNumberOfSamples (Private)
//	Assigns the number of samples in image to the file with
//	the largest number of samples per line.
////////////////////////////////////////////////////////////////

StatusType VicarImageData::initNumberOfSamples()
{
   int numbSamples;
   char msg[MSG_SIZE];
   StatusType return_status = imSUCCESS;
	
   for (int i=0; i< _numbFiles; i++ ) {
      if (i == 0) _numberOfSamples = _files[i]->getNumberOfSamples();
      numbSamples = _files[i]->getNumberOfSamples();
      if (_numberOfSamples != numbSamples) {
         _inconsistentImage = TRUE;
         sprintf(msg,
	    "Number of Samples in file:  %s  doesn't match previous file(s)\n",
	    _files[i]->getName());
         setErrorMsg(msg);
         cerr << msg;
         return_status = imFAILURE;
      }
      if ( _numberOfSamples < numbSamples)  
         _numberOfSamples = numbSamples;
   }
   return return_status;
}

////////////////////////////////////////////////////////////////
// initNumberOfLines (Private)
//	Assigns the number of lines in image to the file with
//	the largest number of lines per image.
////////////////////////////////////////////////////////////////
StatusType VicarImageData::initNumberOfLines()
{
   int numbLines;
   char msg[MSG_SIZE];
   StatusType return_status = imSUCCESS;

   _numberOfLines = 0;
   for (int i=0; i< _numbFiles; i++ ) {
      if (i == 0) _numberOfLines = _files[i]->getNumberOfLines();
      numbLines = _files[i]->getNumberOfLines();
      if (_numberOfLines != numbLines) {
         _inconsistentImage = TRUE;
         sprintf(msg,
	    "Number of Lines in file:  %s  doesn't match previous file(s)\n",
	    _files[i]->getName());
         setErrorMsg(msg);
         cerr << msg;
         return_status = imFAILURE;
      }
      if ( _numberOfLines < numbLines)  
         _numberOfLines = numbLines;
   }
   return return_status;
}

////////////////////////////////////////////////////////////////
// initPixelType (Private)
//	Assigns the pixel size/type to the largest
//	pixel size in each of the 3 files.
////////////////////////////////////////////////////////////////
StatusType VicarImageData::initPixelType()
{
   char msg[MSG_SIZE];
   StatusType return_status = imSUCCESS;
   ImagePixelType pt;

   for (int i=0; i < _numbFiles; i++) {
      pt = _files[i]->getPixelType();
      if (i>0) {
         if (pt != _pixelType) {
            _inconsistentImage = TRUE;
            sprintf(msg,
		"Pixel format in:  %s  doesn't match previous file(s)\n",
		_files[i]->getName());
            setErrorMsg(msg);
            cerr << msg;
            return_status = imFAILURE;
            if (pt.getPixelSize() > _pixelType.getPixelSize())
               _pixelType = pt;
         }
      }
      else
         _pixelType = pt;
   }
   return return_status;
}

////////////////////////////////////////////////////////////////
// initNumbers (Private)
//	Calls other init functions.  Internal use only
////////////////////////////////////////////////////////////////
StatusType VicarImageData::initNumbers()
{
   StatusType status, return_status = imSUCCESS;

   _inconsistentImage = FALSE;
   status = initNumberOfSamples();
   if (status != imSUCCESS);
      return_status = status;
   status = initNumberOfLines();
   if (status != imSUCCESS);
      return_status = status;
   status = initPixelType();
   if (status != imSUCCESS);
      return_status = status;

   if (_tileWidth == 0) 
      _tileWidth = _numberOfSamples;   
   if (_tileHeight == 0)
      _tileHeight = _numberOfLines;

   return return_status;
}

////////////////////////////////////////////////////////////////
// readTile()
//	reads data from files and stores in tile (at bufferPtr).
//	May perform a partial zoom based on user zoom
//		
//	Note: parameters are in image (unzoomed) coordinates
//
////////////////////////////////////////////////////////////////
StatusType VicarImageData::readTile(
		int unzoomedStartSample, int unzoomedStartLine,
		int unzoomedWidth, int unzoomedHeight, ImageTile &tile)
{
   StatusType status, return_status;
   return_status = imSUCCESS;
   char msg[MSG_SIZE];

   // Get tile zoom
   ZoomFactor &tileZoom = tile.getTileZoom();

   if (_mode != COLORmode && _mode != BWmode)
      return imFAILURE;  

   // For each channel

   for (int i=0; i < _numbChannels; i++) {

      unsigned char *bufferPtr = tile.getBufferPtr(i);

      // Now read from 1 band of data
      if (_fileForChannel[i] != NULL) {
         status = _fileForChannel[i]->read1Tile1Band(_bandForChannel[i],
				unzoomedStartLine, unzoomedHeight,
				unzoomedStartSample, unzoomedWidth,
				tileZoom, bufferPtr, tile.getLineWidth());

         // Set failure if bad read (but don't stop, we might get *some* data)
         if (status == imFAILURE) {
            sprintf(msg, "Problem reading file: '%s'\n",
		    _fileForChannel[i]->getName());
            setErrorMsg(msg);
            cerr << msg;
            return_status = imFAILURE;
         }
      }
      else {		// file ptr is NULL, return 0 data
         memset(bufferPtr, 0, tile.getBufferSize());
      }
   }

   // Set the current start line & sample in tile object
   tile.setStartLineSample( unzoomedStartLine, unzoomedStartSample);

   return return_status;
}

////////////////////////////////////////////////////////////////
// readLine
//	reads 1 line of pixels from selected band.
//	No partial zoom performed here.
////////////////////////////////////////////////////////////////
StatusType VicarImageData::readLine (ColorType color, int line,
			unsigned char *bufferPtr)
{
   StatusType status = imFAILURE;
   ZoomFactor zoom;	// Zoom will default to no zoom
   int index;
   char msg[MSG_SIZE];

   switch (color) {
      case RED:
         index = 0;
         break;
      case GREEN:
         index = 1;
         break;
      case BLUE:
         index = 2;
         break;
      case BWcolor:
         index = 0;
         break;
      default:
         return imFAILURE;		// oops!
   }

   // READ LINE FROM FILE
   if (_fileForChannel[index] != NULL) {
      status=_fileForChannel[index]->read1Tile1Band(_bandForChannel[index],
					line, 1, 0, _numberOfSamples,
					zoom, bufferPtr, 0 );
      if (status == imFAILURE) {
         sprintf(msg, "Problem reading file: '%s'\n",
		 _fileForChannel[index]->getName());
         setErrorMsg(msg);
         cerr << msg;
      }
   }
   else {			// file ptr is NULL, return 0 data
      memset(bufferPtr, 0, _numberOfSamples * _pixelType.getPixelSize());
      status = imSUCCESS;
   }

   return status;
}

////////////////////////////////////////////////////////////////
// readPixel 
//	reads 1 pixel from selected band in file(s)
////////////////////////////////////////////////////////////////
StatusType VicarImageData::readPixel(ColorType color,
		int sampleOffset, int lineOffset, unsigned char *pixelBuffer)
{
   StatusType status = imFAILURE;
   ZoomFactor zoom;	// Zoom will default to no zoom
   int index;
   char msg[MSG_SIZE];

   switch (color) {
      case RED:
         index = 0;
         break;
      case GREEN:
         index = 1;
         break;
      case BLUE:
         index = 2;
         break;
      case BWcolor:
         index = 0;
         break;
      default:
         return imFAILURE;		// oops!
   }

   if (_fileForChannel[index] != NULL) {
      status=_fileForChannel[index]->read1Tile1Band(_bandForChannel[index],
			lineOffset, 1, sampleOffset, 1, zoom, pixelBuffer, 0); 
      if (status == imFAILURE) {
         sprintf(msg, "Problem reading file: '%s'\n",
		 _fileForChannel[index]->getName());
         setErrorMsg(msg);
         cerr << msg;
      }
   }
   else {			// file ptr is NULL, return 0 data
      memset(pixelBuffer, 0, _pixelType.getPixelSize());
      status = imSUCCESS;
   }
   return status;
}

////////////////////////////////////////////////////////////////
// calcTileZoom 
//	Calc the partial zoom (prezoom) that we can do on the file
//	data, given that it has it's own filezoom, to speed up the
//	read process.  The widget takes care of whatever zoom
//	it needs to do to equal the user requested zoom. 
//
//	Note that the filezoom is not really implemented yet.  At
//	this time we assume that it is always == 1.
////////////////////////////////////////////////////////////////
ZoomFactor &VicarImageData::calcTileZoom(ImageTile *tile, int, int,
		int Yin, int Yout, int, int Ysubpan)
{
   int fileYIn, fileYOut;

   if (_files[0]) {
      ZoomFactor &fileZoom = _files[0]->getFileZoom(); // check other files too
      fileYOut = fileZoom.getYOut();
      fileYIn = fileZoom.getYIn();
   }
   else {
      fileYOut = 1;
      fileYIn = 1;
   }

   // Init Y zoom factor 
   //   (only work to make Y smaller, not larger - will let widget enlarge)
   //   (too much work to zoom out (or in) on samples)
   int readZoomYin = 1;
   int readZoomYout = 1;
   int readZoomXin = 1;
   int readZoomXout = 1;
   int usrYOut = Yout;
   int usrYIn = Yin;
   int subPanY = 0;

   // If zoomout > zoomin, then set read zoom factors

   if ((usrYOut > usrYIn) &&
		((((double)usrYOut)/usrYIn) > (((double)fileYOut)/fileYIn))) {
      readZoomYin = usrYIn * fileYOut;
      readZoomYout = usrYOut * fileYIn;
      subPanY = Ysubpan;
   }

   // Set tile zoom in tile

   if (tile != NULL) {
      tile->setTileZoom(readZoomXin,readZoomXout,readZoomYin,readZoomYout);
      tile->setTileSubPixelPan(0, subPanY);
   }

   _newTileZoom.setX(readZoomXin, readZoomXout);
   _newTileZoom.setY(readZoomYin, readZoomYout);
   _newTileZoom.setSubPixelPanX(0);
   _newTileZoom.setSubPixelPanY(subPanY);

   return _newTileZoom;
}

// return 1 for success, 0 for failure
int VicarImageData::readMapProjInfo() 
{ 

  if (_files[0] != NULL)
      _mpObject = _files[0]->readMPInfoFromFile();

  if (_mpObject != NULL) {
    _mapProjInfoPresent = 1;
    updateViews();

    return 1;
  }
  else
    return 0;
}

int VicarImageData::lineSampToLatLon(double line, double samp, double *lat,
				     double *lon, int type)
{
  if (!_mapProjInfoPresent)
    return 0;
#ifndef NO_MP_ROUTINES
  if( mpxy2ll(_mpObject, line +1 , samp+1, lat, lon, type) )
    return 0;
  else 
    return 1;
#endif
  return 0;
}

int VicarImageData::latLonToLineSamp(double *line, double *samp, double lat,
				     double lon, int type)
{
  if (!_mapProjInfoPresent)
    return 0;
#ifndef NO_MP_ROUTINES
  if ( mpll2xy(_mpObject, line+1, samp+1, lat, lon, type) )
    return 0;
  else 
    return 1;
#endif
  return 0;
}

LongitudeDirection VicarImageData::getLonDirection() 
{
  if (!_mapProjInfoPresent)
    return UNDEFdirection;

  char pos_lon[5];

#ifndef NO_MP_ROUTINES
  if ( mpGetValues(_mpObject, mpPOSITIVE_LONGITUDE_DIRECTION, pos_lon, "") ) 
    return UNDEFdirection;

  if ( !strcmp(pos_lon, "EAST") )
    return EAST;
  if ( !strcmp(pos_lon, "WEST") )
    return WEST;
#endif
  return UNDEFdirection;
}

///////////////////////////////////////////////
// Read all labels in the set
///////////////////////////////////////////////
StatusType VicarImageData::getLabelSetValue(char*& labels, char *key, int *maxsize) 
{
   int lastKeyUsed = 0;
   int fileIndex=0, instance=0;
   VicarLabelType type = V_LISTALL;
   char set[MAX_LABEL_KEY_SIZE+1];      // Name for label subset
   StatusType return_status = imSUCCESS;
   char msg[200];

   if (key == NULL) {
      if (_lastKey) {
         key = strdup(_lastKey);
         lastKeyUsed = 1;
      }
      else
         key = strdup("0");
   }

   // parse label key
   return_status = parseLabelKey(key, &fileIndex, &type, &set[0], &instance);
   if (return_status == imFAILURE && !lastKeyUsed)
      return imFAILURE;

   // read in the labels
   if (return_status == imSUCCESS)
      return_status = _files[fileIndex]->readVicarLabel(labels, type, set, 
                      instance, maxsize);

   // Use the default key (0 -- all) if lastkey yields nil labels
   if (!labels || (!strcmp(labels, "") && lastKeyUsed)) {
      fileIndex = 0;
      type = V_LISTALL;
      return_status = _files[fileIndex]->readVicarLabel(labels, type,
                      NULL, 0, maxsize);
   }

   // If unable to read label, print error
   if ( return_status == imFAILURE ) {
      sprintf( msg, "Unable to read label.\n");
      setErrorMsg(msg);
      cerr << msg;
   }

   strcpy(_lastKey, key);

   return return_status;
}

///////////////////////////////////////////////////////////
// Parse label key
// key being in the form of fileindex/type/set/instance
///////////////////////////////////////////////////////////
StatusType VicarImageData::parseLabelKey(char *key, int *fileIndex, 
                    VicarLabelType *type, char *set, int *instance) 
{
   char *thiskey, *nextkey;
   char *element;
   int elementIndex;
   char msg[MSG_SIZE];
   
   elementIndex = 1;
   *fileIndex = 0;
   *type = (VicarLabelType)V_LISTALL; 
   if (!key) return imFAILURE;
   thiskey = new char[strlen(key)+1];
   strcpy(thiskey, key);
   thiskey[strlen(thiskey)] = '\0';
   nextkey = thiskey;
   while (nextkey != NULL && elementIndex <= 4) { 
      char *ptr = strchr(nextkey, '/');
      if (ptr) {
         int str_len = strlen(nextkey) - strlen(ptr);
         element = new char [str_len + 1];
         strncpy(element, nextkey, str_len);
         element[str_len] = '\0';    // strncpy doesn't terminate string
         nextkey = ptr + 1;            // move past the separator
      } else {
         element = strdup(nextkey);
         nextkey = NULL;
      }
      if (elementIndex == 1) {
         *fileIndex = atoi(element);
         if (*fileIndex < 0 || *fileIndex > _numbFiles) {
            sprintf( msg, "Unable to parse key:  %s  \n", key);
            setErrorMsg(msg);
            cerr << msg;
            return imFAILURE;
         }
      }
      else if (elementIndex == 2) {
         if (!strcmp(element, "system")) {
            *type = (VicarLabelType)V_SYSTEM;
            break;
         }
         else if (!strcmp(element, "property")) 
            *type = (VicarLabelType)V_PROPERTY;
         else if (!strcmp(element, "history")) 
            *type = (VicarLabelType)V_HISTORY;
         else if (!strcmp(element, "list")) 
            *type = (VicarLabelType)V_LISTALL;
         else if (!strcmp(element, "dump")) {
            *type = (VicarLabelType)V_DUMPALL;
            break;
         }
         else 
            return imFAILURE;
      }
      else if (elementIndex == 3) 
         strcpy(set, element);
      else if (elementIndex == 4) 
         *instance = atoi(element);
      delete[] element;
      elementIndex++;
   }
   delete [] thiskey;

   return imSUCCESS;
}


