////////////////////////////////////////////////////////////////
// ImageData.cc
//
//	This is an abstract class for image data.
//	This is used with ImageView for displaying an image
//	from a file or other source onto the screen.
//
//	"this describes what an image is, not what it
//	looks like or how to draw it"
//
//   FUTURE ENHANCEMENTS:
//	(1) Allow Mode to be multispectral
//
//   IMPORTANT NOTE!
//      There is UGLY platform-specific code in getImageTypeFromFile,
//      simply to deal with large files (>2GB).  We have to use fopen64()
//      instead of fopen() on some platforms, even though we only read the
//      first few bytes.  Ugh!!!!
////////////////////////////////////////////////////////////////
#include <stdio.h>
#include "ImageData.h"
#include "ViewMacros.h"
#include "BasicImageView.h"
#include "Application.h"		// see readDataRange
#include "ImageDefs.h"

#include <iostream>
using namespace std;

////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////
ImageData::ImageData()
{
   _labelTree = NULL;
   _mapProjInfoPresent = 0;
   _tileHeight = 0;	
   _tileWidth = 0;
   _numViews = 0;
   _views = NULL;
   _inputDataSourceName[0] = '\0';
   _dataSourceOpened = False;

   _numberOfLines = 0;
   _numberOfSamples = 0;
   _mode = UNDEFmode;
   _inconsistentImage = False;
   _pixelType.set(imBYTE);

   _errorMsgIssued = False;
   _errorMsg = NULL;

   _fileDataRangeValid = False;
   _useFileMin = True;
   _useFileMax = True;
   _userMinDataRange = _pixelType.getMinDataRange();
   _userMaxDataRange = _pixelType.getMaxDataRange();
}

////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////
ImageData::~ImageData()
{
    for (int i = 0; i < _numViews; i++)
	detachView(_views[i]);
    if (_errorMsg)
	delete []_errorMsg;

    if (_labelTree) {
       _labelTree->deleteLabelTree();
       delete _labelTree;
    }
}

////////////////////////////////////////////////////////////////
// attachView:
//	Standard Model function for attaching views to Model.
////////////////////////////////////////////////////////////////
void ImageData::attachView(BasicImageView *view)
{
   AttachViewMacro(BasicImageView, _views, _numViews, view);
   view->update();
}

////////////////////////////////////////////////////////////////
// detachView:
//	Standard Model function for detaching views from Model.
////////////////////////////////////////////////////////////////
void ImageData::detachView(BasicImageView *view)
{
   DetachViewMacro(BasicImageView, _views, _numViews, view);
}

////////////////////////////////////////////////////////////////
// updateViews:
//	Standard Model function for updating all views attached to model.
////////////////////////////////////////////////////////////////
void ImageData::updateViews()
{
   int i;

   for (i=0; i<_numViews; i++)
      _views[i]->update();
}

////////////////////////////////////////////////////////////////
// updatePartViews:
//	Inform views that only a small part of the model has changed
//	(not the entire image).  Flags indicate what changed.
////////////////////////////////////////////////////////////////
void ImageData::updatePartViews(int flags)
{
   int i;

   for (i=0; i<_numViews; i++)
      _views[i]->updatePart(flags);
}

////////////////////////////////////////////////////////////////
// initTileProperties
//	Passes properties onto tile.  Can be overridden by subclasses
////////////////////////////////////////////////////////////////
void ImageData::initTileProperties(ImageTile &tile, ZoomFactor &tileZoom)
{
   tile.setPixelType(_pixelType);
   tile.setMode(_mode);
   tile.setSize(_tileHeight, _tileWidth);
   tile.setTileZoom(tileZoom.getXIn(), tileZoom.getXOut(),
		    tileZoom.getYIn(), tileZoom.getYOut());
   tile.setTileSubPixelPan(tileZoom.getSubPixelPanX(),
			   tileZoom.getSubPixelPanY());
}

////////////////////////////////////////////////////////////////
// Save the given error message for use later
////////////////////////////////////////////////////////////////

void ImageData::setErrorMsg(const char *msg)
{
  if (_errorMsg != NULL) 
    return;			// only use the first error

   if (_errorMsg)
      delete []_errorMsg;
   _errorMsg = new char[strlen(msg)+1];
   strcpy(_errorMsg, msg);
}

////////////////////////////////////////////////////////////////
// initDataRange
//	Must be called by all open()'s that support more than byte data.
////////////////////////////////////////////////////////////////
void ImageData::initDataRange()
{
   if (_useFileMin || _useFileMax)
      readDataRange();
   else
      _fileDataRangeValid = False;
}

////////////////////////////////////////////////////////////////
// readDataRange
//	Determine min/max range of data values in the file.  Default
//	behavior is to read the entire file to determine this.
//	Should be overridden by any subclass that has data range info
//	easily available (e.g. from a label instead of reading all
//	the data).
//	NOTE:  This version refuses to read the file if it is byte
//	data, because byte data rarely needs a data range.  If you
//	really REALLY want this, subclass this function and read
//	the bytes yourself...
//	TBD:  Fix this up so it goes in the background, or provide
//	an abort or something, to deal with huge files.  In the meantime,
//	we do something yucky by setting a busy cursor around the
//	read.  This is yucky because the model is directly talking
//	to the interface, which should be a no-no.
////////////////////////////////////////////////////////////////
void ImageData::readDataRange()
{
   unsigned char *buffer;
   int line;
   StatusType status = imSUCCESS;

   if (_pixelType.get() == imBYTE) {
      _fileMinDataRange = _pixelType.getMinDataRange();
      _fileMaxDataRange = _pixelType.getMaxDataRange();
      _fileDataRangeValid = True;
      return;
   }

   theApplication->setBusyCursor();

   _fileDataRangeValid = False;
   buffer = new unsigned char[_pixelType.getPixelSize() * _numberOfSamples];
   if (buffer == NULL) {
      theApplication->removeBusyCursor();
      return;			// valid is False
   }

   _fileMinDataRange = _pixelType.getMaxDataRange();
   _fileMaxDataRange = _pixelType.getMinDataRange();

   for (line=0; line<_numberOfLines; line++) {
      if (_mode == COLORmode) {
         status = readLine(RED, line, buffer);
         if (status != imSUCCESS)
            break;
         findMaxMinInLine(buffer);
         status = readLine(GREEN, line, buffer);
         if (status != imSUCCESS)
            break;
         findMaxMinInLine(buffer);
         status = readLine(BLUE, line, buffer);
         if (status != imSUCCESS)
            break;
         findMaxMinInLine(buffer);
      }
      else if (_mode == BWmode) {
         status = readLine(BWcolor, line, buffer);
         if (status != imSUCCESS)
            break;
         findMaxMinInLine(buffer);
      }
      else {
         status = imFAILURE;
         break;			// unknown mode
      }
   }
   if (status == imSUCCESS)
      _fileDataRangeValid = True;

   theApplication->removeBusyCursor();
}

////////////////////////////////////////////////////////////////
// findMaxMinInLine
//	Find the max and min pixel values in the buffer, and update
//	_minDataRange and _maxDataRange appropriately.
////////////////////////////////////////////////////////////////
void ImageData::findMaxMinInLine(unsigned char *buffer)
{
   int i;

#define LOOP(type)					\
   {							\
      type min = (type)_fileMinDataRange;		\
      type max = (type)_fileMaxDataRange;		\
      type *buf = (type *)buffer;			\
      for (i=0; i<_numberOfSamples; i++) {		\
         if (buf[i] < min)				\
            min = buf[i];				\
         if (buf[i] > max)				\
            max = buf[i];				\
      }							\
      _fileMinDataRange = (double)min;			\
      _fileMaxDataRange = (double)max;			\
   }

   switch (_pixelType.get()) {
      case imBYTE:
         LOOP(imByte)
         break;
      case imHALF:
         LOOP(imHalf)
         break;
      case imUHALF:
         LOOP(imUHalf)
         break;
      case imFULL:
         LOOP(imFull)
         break;
      case imUFULL:
         LOOP(imUFull)
         break;
      case imREAL:
         LOOP(imReal)
         break;
      case imDOUBLE:
         LOOP(imDouble)
         break;
   }
}

////////////////////////////////////////////////////////////////
// getSuggestedUnzoomedTileSize
//	Returns the recommended tile size.  This is arbitrarily
//	set to 100x100 in the base class.  This should be overridden
//	by subclasses.
////////////////////////////////////////////////////////////////
StatusType ImageData::getSuggestedUnzoomedTileSize(int &height, int &width)
{
   height = 100;
   width = 100;
   return imSUCCESS;
}

////////////////////////////////////////////////////////////////
// transDisplayToImageCoords()
//	Transformation function which transforms display coordinates
//	into image coordinates. In case of this class the function
//	has only a little to do:
//	Display coordinate system (0,0) => Image coordinate system (1,1)
////////////////////////////////////////////////////////////////
void ImageData::transDisplayToImageCoords(int x, int y,
					  int *sample, int *line)
{
    // Transformation from display system (0,0) to image system (1,1)
    *line = y + 1;
    *sample = x + 1;
}

void ImageData::transDisplayToImageCoords(double x, double y,
                                          double *sample, double *line)
{
    // Transformation from display system (0,0) to image system (1,1)
    *line = y + 1.0;
    *sample = x + 1.0;
}

////////////////////////////////////////////////////////////////
// transImageToDisplayCoords()
//      Transformation function which transforms image coordinates
//      into display coordinates. In case of this class the function
//      has only a little to do:
//      Display coordinate system (1,1) => Image coordinate system (0,0)
////////////////////////////////////////////////////////////////
void ImageData::transImageToDisplayCoords(int samp, int line, int *x, int *y)
{
    // Transformation from image system (1,1) to display system (0,0)
    *x = samp - 1;
    *y = line - 1;
}

void ImageData::transImageToDisplayCoords(double samp, double line,
					  double *x, double *y)
{
    // Transformation from image system (1,1) to display system (0,0)
    *x = samp - 1.0;
    *y = line - 1.0;
}

////////////////////////////////////////////////////////////////
// createTile
//	Creates a tile with buffers.  Base class ignores willCreateBuffers
//	argument and always creates the buffers.
//	Error checking can't be done with this design.  We have to return
//	a Tile; if the tile couldn't be created, what do you return?  So
//	we just ignore new failure or createBuffers failure... nothing else
//	is any better.  It should return a pointer or a status instead of
//	a reference.
////////////////////////////////////////////////////////////////
ImageTile &ImageData::createTile(ZoomFactor &tileZoom, Boolean)
{
   // Create tile

   ImageTile *tile = new ImageTile;

   if (tile) {
      initTileProperties(*tile, tileZoom);
      tile->createBuffers();		// can't do anything with error
   }

   return *tile;
}

////////////////////////////////////////////////////////////////
// destroyBuffers
//	This routine should destroy tile buffers owned by this
//	class.  However, since the base class does not own any,
//	there's nothing to do.  Subclasses can override this if needed.
////////////////////////////////////////////////////////////////
void ImageData::destroyBuffers()
{
}

////////////////////////////////////////////////////////////////
// getDataRange
//	Gets the current data range.  If Auto mode is set and the
//	file data is not valid, it attempts (again?) to read the
//	file min/max.
////////////////////////////////////////////////////////////////
void ImageData::getDataRange(double &min, double &max)
{
   if ((_useFileMin || _useFileMax) && !_fileDataRangeValid)
      readDataRange();

   if (_useFileMin) {
      if (_fileDataRangeValid)
         min = _fileMinDataRange;
      else
         min = _pixelType.getMinDataRange();
   }
   else
      min = _userMinDataRange;

   if (_useFileMax) {
      if (_fileDataRangeValid)
         max = _fileMaxDataRange;
      else
         max = _pixelType.getMaxDataRange();
   }
   else
      max = _userMaxDataRange;

}

////////////////////////////////////////////////////////////////
// setDataRange
//	Sets the current data range, and sets both flags to use
//	user-supplied max/min instead of file max/min.
////////////////////////////////////////////////////////////////
void ImageData::setDataRange(double min, double max)
{
   _userMinDataRange = min;
   _userMaxDataRange = max;
   _useFileMin = False;
   _useFileMax = False;

   updateViews();
}

////////////////////////////////////////////////////////////////
// setDataMin
//	Sets the minimum only for the current data range, and sets
//	the user-supplied min flag.
////////////////////////////////////////////////////////////////
void ImageData::setDataMin(double min)
{
   _userMinDataRange = min;
   _useFileMin = False;

   updateViews();
}

////////////////////////////////////////////////////////////////
// setDataMax
//	Sets the maximum only for the current data range, and sets
//	the user-supplied max flag.
////////////////////////////////////////////////////////////////
void ImageData::setDataMax(double max)
{
   _userMaxDataRange = max;
   _useFileMax = False;

   updateViews();
}

////////////////////////////////////////////////////////////////
// setMinAuto
//	Sets the min flag to get data from file.  Doesn't call
//	readDataRange() directly; presumably it will get called
//	via getDataRange() if needed.
////////////////////////////////////////////////////////////////
void ImageData::setMinAuto(Boolean min_auto)
{
   if (_useFileMin != min_auto) {
      _useFileMin = min_auto;
      updateViews();
   }
}

////////////////////////////////////////////////////////////////
// setMaxAuto
//	Sets the max flag to get data from file.  Doesn't call
//	readDataRange() directly; presumably it will get called
//	via getDataRange() if needed.
////////////////////////////////////////////////////////////////
void ImageData::setMaxAuto(Boolean max_auto)
{
   if (_useFileMax != max_auto) {
      _useFileMax = max_auto;
      updateViews();
   }
}

////////////////////////////////////////////////////////////////
// calcTileZoom
//	Calculate the partial zoom (prezoom) that we can do on the file
//	data, given the desired zoom, to speed up the read process.
//	The widget takes care of any remaining zoom needed to match the
//	requested zoom.
//
//	Subclasses that wish to provide prezoomed data should override
//	this function to set the real zoom they're returning in the tile,
//	and in the returned ZoomFactor.  This base class assumes no prezoom
//	so sets them all to 1.
////////////////////////////////////////////////////////////////
ZoomFactor &ImageData::calcTileZoom(ImageTile *tile, 
				    int, int, int, int, int, int)
{
   if (tile != NULL) {
      tile->setTileZoom(1, 1, 1, 1);
      tile->setTileSubPixelPan(0, 0);
   }

   _newTileZoom.setX(1, 1);
   _newTileZoom.setY(1, 1);
   _newTileZoom.setSubPixelPanX(0);
   _newTileZoom.setSubPixelPanY(0);

   return _newTileZoom;
}

////////////////////////////////////////////////////////////////
// getImageTypeFromFile: reads in an image file and tries to
// determine which ImageData type to instantiate.
//
// For dual-labeled PDS/VICAR files, the file is assumed to be in PDS
// mode by default (meaning the user can access the PDS labels but not
// the VICAR ones).  If the filename is preceded with the three characters
// "{V}" or "{P}", (upper or lower case) then the file is opened in that
// mode.  Those two sets of three characters are simply ignored if the
// file is not a dualie.
//
// "LBLSIZE" indicates a VICAR image
// "CCSD", "NJPL" or "PDS_VERSION_ID" indicate a PDS image
//
////////////////////////////////////////////////////////////////

ImageType ImageData::getImageTypeFromFile(char *stringOfFilenames)
{
    FILE *imageFile;
    ImageType fileType = UNDEFimage;
    char buf[1000];

    // Look for the {P} or {V} special tags

    Boolean forceVicar = False;
    if (strncmp(stringOfFilenames, "{V}", 3) == 0 ||
	strncmp(stringOfFilenames, "{v}", 3) == 0) {
	forceVicar = True;
	stringOfFilenames += 3;
    }
    else if (strncmp(stringOfFilenames, "{P}", 3) == 0 ||
	strncmp(stringOfFilenames, "{p}", 3) == 0) {
	stringOfFilenames += 3;
    }

    // Search for comma or whitespace

    int len = strcspn(stringOfFilenames, "(, \t\n");
    char *filename = new char [len + 1];
    strncpy(filename, stringOfFilenames, len);
    filename[len] = '\0';

// Okay, here is some REALLY UGLY platform-specific code.  In order to support
// large files (>2GB), on some platforms we have to use fopen64() instead of
// fopen(), even though we're only reading 50 bytes!!!!  When the file is
// opened by the I/O package (e.g. VICAR), large files are handled okay.
// This should be in an include somewhere but hopefully this is the only
// occurrence.  It *IS* in xvmaininc.h, in VICAR, but we don't want to be
// dependent on VICAR includes in this part of the code.  Sigh.  Unlike
// xvmaininc.h, we depend on _LARGEFILE64_SOURCE being predefined by the
// compiler, via <stdio.h>
// YUCK!!!!

#if defined(_LARGEFILE64_SOURCE)
    imageFile = fopen64(filename, "r");
#else
    imageFile = fopen(filename, "r");
#endif

    if (imageFile == NULL) {
	delete [] filename;
	return UNDEFimage;
    }
    
    fgets ( buf, 50, imageFile );

    if ( !strncmp( "LBLSIZE", buf, 7 ) )
	fileType = VICARimage;
    
#ifndef NO_PDS
    else if ( !strncmp( "PDS_VERSION_ID", buf, 14 ) || 
	      !strncmp( "ODL_VERSION_ID", buf, 14 ) ||
	      !strncmp( "CCSD", buf, 4 ) || 
	      !strncmp ( "NJPL", buf, 4) ) {
	if (forceVicar)
	    fileType = PDS_VICARimage;
	else
	    fileType = PDSimage;
    }
#endif
    
    else
	fileType = UNDEFimage;
    
    (void) fclose(imageFile);

    delete [] filename;
    return fileType;
}

