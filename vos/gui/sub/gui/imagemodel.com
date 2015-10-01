$!****************************************************************************
$!
$! Build proc for MIPL module imagemodel
$! VPACK Version 1.9, Thursday, March 03, 2011, 17:56:46
$!
$! Execute by entering:		$ @imagemodel
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
$ write sys$output "*** module imagemodel ***"
$!
$ Create_Source = ""
$ Create_Repack =""
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
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to imagemodel.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
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
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("imagemodel.imake") .nes. ""
$   then
$      vimake imagemodel
$      purge imagemodel.bld
$   else
$      if F$SEARCH("imagemodel.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake imagemodel
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @imagemodel.bld "STD"
$   else
$      @imagemodel.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create imagemodel.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack imagemodel.com -mixed -
	-s ImageData.cc ImageDataWrapper.cc ImageTile.cc VicarImageData.cc -
	   VicarImageFile.cc ZoomFactor.cc ImagePixelType.cc ImageLabel.cc -
	-i imagemodel.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ImageData.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
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

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ImageDataWrapper.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// ImageDataWrapper.cc: This is a class derived from ImageData class.
// It is intended to maintain image data if the specific type 
// (e.g.; VicarImageData or PdsImageData) is not known initially.
// For dual-labeled PDS/VICAR files, the file is opened in PDS mode by
// default (meaning the user can access the PDS labels but not the VICAR
// ones).  If the filename is preceded with the three characters "{V}"
// or "{P}" (upper or lower case), then the file is opened in that mode.
// Those two sets of three characters are stripped regardless, and simply
// ignored if the file is not a dualie.
//////////////////////////////////////////////////////////////////////////////
#include "ImageDataWrapper.h"
#include "VicarImageData.h"
#include "ViewMacros.h"
#include <assert.h>
#include <stdio.h>

#ifndef NO_PDS
#include "PdsImageData.h"
#endif

ImageDataWrapper::ImageDataWrapper() : ImageData()
{
    _actualImage = NULL;
    _weOwnImage = False;
    strcpy(_lastKey, "0");
}

ImageDataWrapper::~ImageDataWrapper()
{
    if (_actualImage != NULL && _weOwnImage)
	delete _actualImage;
}

void ImageDataWrapper::registerActualImage(ImageData *actualImage,
					   Boolean weOwnImage)
{
    assert(actualImage != NULL);
    _weOwnImage = weOwnImage;
    if (_actualImage) delete _actualImage;
    
    _actualImage = actualImage;
    _weOwnImage = weOwnImage;

    _labelTree = actualImage->getLabelRoot();
    for (int i = 0; i < _numViews; i++)
	_actualImage->attachView(_views[i]);
}

void ImageDataWrapper::unregisterActualImage()
{
    assert(_actualImage != NULL);
    _weOwnImage = False;
    for (int i = 0; i < _numViews; i++)
	_actualImage->detachView(_views[i]);
    delete _actualImage;
    _actualImage = NULL;
}

void ImageDataWrapper::attachView(BasicImageView *view)
{
    // Attach view to the wrapper, but don't call the update

    AttachViewMacro(BasicImageView, _views, _numViews, view);

    if (_actualImage)
	_actualImage->attachView(view);
}

void ImageDataWrapper::detachView(BasicImageView *view)
{
    ImageData::detachView(view);
    if (_actualImage)
	_actualImage->detachView(view);
}

int ImageDataWrapper::getPixelSize()
{
    if (_actualImage)
	return _actualImage->getPixelSize();
    else
	return _pixelType.getPixelSize();
}

void ImageDataWrapper::transDisplayToImageCoords(int x, int y, 
						 int *samp, int *line)
{
    assert(_actualImage != NULL);
    _actualImage->transDisplayToImageCoords(x, y, samp, line);
}

void ImageDataWrapper::transDisplayToImageCoords(double x, double y,
						 double *samp, double *line)
{
    assert(_actualImage != NULL);
    _actualImage->transDisplayToImageCoords(x, y, samp, line);
}

void ImageDataWrapper::transImageToDisplayCoords(int samp, int line, 
						 int *x, int *y)
{
    assert(_actualImage != NULL);
    _actualImage->transImageToDisplayCoords(samp, line, x, y);
}

void ImageDataWrapper::transImageToDisplayCoords(double samp, double line,
						 double *x, double *y)
{
    assert(_actualImage != NULL);
    _actualImage->transImageToDisplayCoords(samp, line, x, y);
}

void ImageDataWrapper::setMode(ModeType mode)
{
    ImageData::setMode(mode);
    assert(_actualImage != NULL);
    _actualImage->setMode( mode );
}

void ImageDataWrapper::getDataRange(double &min, double &max)
{
    if (_actualImage)
	_actualImage->getDataRange(min, max);
    else
	ImageData::getDataRange(min, max);
}

void ImageDataWrapper::setDataRange(double min, double max)
{
    ImageData::setDataRange(min, max);

    if (_actualImage != NULL)
	_actualImage->setDataRange(min, max);
}

void ImageDataWrapper::setDataMin(double min)
{
    ImageData::setDataMin(min);
    if (_actualImage != NULL)
	_actualImage->setDataMin(min);
}

void ImageDataWrapper::setDataMax(double max)
{
    ImageData::setDataMax(max);
    if (_actualImage != NULL)
	_actualImage->setDataMax(max);
}

void ImageDataWrapper::setMinAuto(Boolean minAuto)
{
    ImageData::setMinAuto(minAuto);
    if (_actualImage)
	_actualImage->setMinAuto( minAuto );
}

void ImageDataWrapper::setMaxAuto(Boolean maxAuto)
{
    ImageData::setMaxAuto(maxAuto);
    if (_actualImage)
	_actualImage->setMaxAuto(maxAuto);
}

StatusType ImageDataWrapper::getSuggestedUnzoomedTileSize(int &height, 
							  int &width)
{
    if (_actualImage)
	return _actualImage->getSuggestedUnzoomedTileSize(height, width);
    else
	return ImageData::getSuggestedUnzoomedTileSize(height, width);
}

ImageTile &ImageDataWrapper::createTile(ZoomFactor &tileZoom,
					Boolean willCreateBuffers )
{
    assert(_actualImage != NULL);
    return _actualImage->createTile(tileZoom, willCreateBuffers);
}

void ImageDataWrapper::initTileProperties(ImageTile &tile, ZoomFactor &tileZoom)
{
    // We should be able to call _actualImage->initTileProperties() but that
    // causes a protection problem for some stupid reason... (we are a subclass,
    // after all!!)
    assert(_actualImage != NULL);
    tile.setPixelType(_actualImage->getPixelType());
    tile.setMode(_actualImage->getMode());
    int h, w;
    _actualImage->getUnzoomedTileSize(h, w);
    tile.setSize(h, w);
    tile.setTileZoom(tileZoom.getXIn(), tileZoom.getXOut(),
                     tileZoom.getYIn(), tileZoom.getYOut());
    tile.setTileSubPixelPan(tileZoom.getSubPixelPanX(),
                            tileZoom.getSubPixelPanY());
}

void ImageDataWrapper::destroyBuffers()
{
    assert(_actualImage != NULL);
    _actualImage->destroyBuffers();
}

Boolean ImageDataWrapper::isNewBufferNeeded(ImageTile &tile)
{
    assert(_actualImage != NULL);
    return _actualImage->isNewBufferNeeded(tile);
}

char *ImageDataWrapper::getErrorMsg()
{
  if (_actualImage != NULL)

    return _actualImage->getErrorMsg();

  else                             // we can still report an error, 
                                   // even if we don't yet have an _actualImage

    return ImageData::getErrorMsg();
}

Boolean ImageDataWrapper::errorMsgIssued()
{
    return ImageData::errorMsgIssued();
}

StatusType ImageDataWrapper::open(char *filename)
{
    ImageType fileType;
    fileType = getImageTypeFromFile(filename);
    ImageData *actualImage;
    char msg[256];

    // Save the unmodified filename for Reload.
    strcpy(_reloadDataSourceName, filename);

    // Remove the {P} or {V} special tags
    if (strncmp(filename, "{V}", 3) == 0 ||
	strncmp(filename, "{v}", 3) == 0 ||
	strncmp(filename, "{P}", 3) == 0 ||
	strncmp(filename, "{p}", 3) == 0)
	filename += 3;

    if ( fileType == VICARimage || fileType == PDS_VICARimage )
	actualImage = new VicarImageData();

#ifndef NO_PDS
    else if ( fileType == PDSimage )
	actualImage = new PdsImageData();
#endif
    else {    
      sprintf( msg, "Unable to determine the format of file: %s\n", filename );
      setErrorMsg(msg);
      return imFAILURE;
    }

    actualImage->setLastKey(_lastKey);

    actualImage->setDataRange(_userMinDataRange, _userMaxDataRange);
    actualImage->setMinAuto(_useFileMin); //!!! Not needed?
    actualImage->setMaxAuto(_useFileMax); //!!! Not needed?

    StatusType status = actualImage->open(filename);

#ifndef NO_PDS
    // If the file is ambiguous (PDS format but user asked for VICAR)
    // and VICAR is unable to open the file, then maybe it's not a dualie
    // after all.  Reset with PDS and try again.

    if (status != imSUCCESS && fileType == PDS_VICARimage) {
	delete actualImage;
	actualImage = new PdsImageData();

	actualImage->setLastKey(_lastKey);
	actualImage->setDataRange(_userMinDataRange, _userMaxDataRange);
	actualImage->setMinAuto(_useFileMin); //!!! Not needed?
	actualImage->setMaxAuto(_useFileMax); //!!! Not needed?

	status = actualImage->open(filename);
    }
#endif

    if ( status != imSUCCESS ) {

      sprintf( msg, "Could not open file: %s\n", filename );
      setErrorMsg( msg );

      return status;
    }

    registerActualImage(actualImage, True);
    return status;
}

StatusType ImageDataWrapper::close()
{
    assert(_actualImage != NULL);
    strcpy(_lastKey, _actualImage->getLastKey());
    StatusType status = _actualImage->close();
    unregisterActualImage();
    return status;
}

StatusType ImageDataWrapper::readPixel(ColorType color, int x, int y,
				       unsigned char *bufferPtr)
{
    assert(_actualImage != NULL);
    return _actualImage->readPixel(color, x, y, bufferPtr);
}

StatusType ImageDataWrapper::readLine(ColorType color, int line,
				      unsigned char *bufferPtr)
{
    assert(_actualImage != NULL);
    return _actualImage->readLine(color, line, bufferPtr);
}

StatusType ImageDataWrapper::readTile(int unzoomedStartSample, 
				      int unzoomedStartLine,
				      int unzoomedWidth, int unzoomedHeight,
				      ImageTile &tile)
{
    assert ( _actualImage != NULL );
    return _actualImage->readTile(unzoomedStartSample,
				  unzoomedStartLine,
				  unzoomedWidth, unzoomedHeight,
				  tile);
}

// Zoom

ZoomFactor &ImageDataWrapper::calcTileZoom(ImageTile *tile, 
					   int Xin, int Xout,
					   int Yin, int Yout, 
					   int Xpan, int Ypan)
{
    if (_actualImage)
	return _actualImage->calcTileZoom(tile, Xin, Xout,
					  Yin, Yout, Xpan, Ypan);
    else
	return ImageData::calcTileZoom(tile, Xin, Xout,
				       Yin, Yout, Xpan, Ypan);
}

void ImageDataWrapper::getInputDataSourceName(char *name)
{
    if (_actualImage)
	_actualImage->getInputDataSourceName(name);
    else
	strcpy(name, "");
}

char *ImageDataWrapper::getInputDataSourceName()
{
    if (_actualImage)
	return _actualImage->getInputDataSourceName();
    else
	return ((char *)"");
}

Boolean ImageDataWrapper::isDataSourceOpened() const
{
    if (_actualImage)
	return _actualImage->isDataSourceOpened();
    else
	return False;
}

void ImageDataWrapper::getPixelType(char *pixelType) const
{
    if (_actualImage)
	_actualImage->getPixelType(pixelType);
    else
	strcpy(pixelType, "imBYTE");
}

ImagePixelType ImageDataWrapper::getPixelType() const
{
    if (_actualImage)
	return _actualImage->getPixelType();
    else
	return imBYTE;
}

ModeType ImageDataWrapper::getMode() const
{
    if (_actualImage)
	return _actualImage->getMode();
    else
	return UNDEFmode;
}

int ImageDataWrapper::getNumbSamples() const
{
    if (_actualImage)
	return _actualImage->getNumbSamples();
    else
	return 0;
}

int ImageDataWrapper::getNumbLines() const
{
    if (_actualImage)
	return _actualImage->getNumbLines();
    else
	return 0;
}

void ImageDataWrapper::getUnzoomedTileSize(int &height, int &width)
{
    if (_actualImage)
	_actualImage->getUnzoomedTileSize(height, width);
    else {
	height = 0; width = 0;
    }
}

void ImageDataWrapper::setUnzoomedTileSize(int h, int w)
{
    assert(_actualImage);
    _actualImage->setUnzoomedTileSize(h, w);
}

Boolean ImageDataWrapper::isConsistent() const
{
    if (_actualImage)
	return _actualImage->isConsistent();
    else
	return False;
}
 
ZoomFactor &ImageDataWrapper::getTileZoom(ImageTile &tile)
{
    assert(_actualImage);
    return _actualImage->getTileZoom(tile);
}

double ImageDataWrapper::getMinDataRange()
{
    if (_actualImage)
	return _actualImage->getMinDataRange();
    else
	return ImageData::getMinDataRange();
}

double ImageDataWrapper::getMaxDataRange()
{
    if (_actualImage)
	return _actualImage->getMaxDataRange();
    else
	return ImageData::getMaxDataRange();
}

Boolean ImageDataWrapper::isMinAuto()
{
    if (_actualImage)
	return _actualImage->isMinAuto();
    else
	return _useFileMin;
}

Boolean ImageDataWrapper::isMaxAuto()
{
    if (_actualImage)
	return _actualImage->isMaxAuto();
    else
	return _useFileMax;
}

int ImageDataWrapper::readMapProjInfo()
{
  if (_actualImage)
    return _actualImage->readMapProjInfo();
  else
    return 0;
}

int ImageDataWrapper::lineSampToLatLon(double line, double samp, double *lat,
				       double *lon, int type)
{
  if (_actualImage)
    return _actualImage->lineSampToLatLon(line, samp, lat, lon, type);
  else
    return 0;
}

int ImageDataWrapper::latLonToLineSamp(double *line, double *samp, double lat,
				       double lon, int type)
{ 
  if (_actualImage)
    return _actualImage->latLonToLineSamp(line, samp, lat, lon, type);
  else
    return 0;
}

int ImageDataWrapper::isMapProjInfoPresent()
{
    if (_actualImage)
	return _actualImage->isMapProjInfoPresent();
    else
	return 0;
}

LongitudeDirection ImageDataWrapper::getLonDirection() 
{
  if (_actualImage)
    return _actualImage->getLonDirection();
  else
    return UNDEFdirection;
}

StatusType ImageDataWrapper::getLabelSetValue(char*& labels, char *key, int *maxsize) 
{
   StatusType status;

   if (_actualImage) {
      status = _actualImage->getLabelSetValue(labels, key, maxsize);
      return status;
   }
   else
      return imFAILURE;
}

ImageLabel *ImageDataWrapper::getLabelRoot()
{
  if (_actualImage)
     return _actualImage->getLabelRoot();
  else
     return NULL;
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ImageTile.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// ImageTile.cc
//
//	Created by image view (or created by model when requested by view).  
//	Keeps tile information and location of buffer(s) that are needed for
//	the imaging widget.
//
//	FUTURE ENHANCEMENTS:
//		(1) readPixel 
//		(2) createBuffers to support multiband BW data
////////////////////////////////////////////////////////////////
#include  "ImageTile.h"
#include <ctype.h>

////////////////////////////////////////////////////////////////
// createBuffers (if for TILE ownership)
//	create either 1 or 3 buffers, up to 1 for each color band.
//	Buffers will be owned by tile.
////////////////////////////////////////////////////////////////
StatusType ImageTile::createBuffers()
{
   StatusType status = imSUCCESS;
	
   // Delete *tile* buffers if they exist
   destroyBuffers();

   // If no other buffers exist
   if ( _numberOfBuffers == 0 ) {

      // Get number of buffers & buffersize
      createBufferAttributes( );
      if (_mode == COLORmode) _numberOfBuffers = 3;
      if (_mode == BWmode) _numberOfBuffers = 1;

      // Create buffers
      for (int i=0; i < _numberOfBuffers; i++) {
         _bufferPtr[i] = NULL;
         unsigned char *newBuffer = new unsigned char[_bufferSize];
         _bufferPtr[i] = newBuffer;
         if ( ! _bufferPtr[i])
            status = imFAILURE; 
      }

      // If unable to create any of the buffers, destroy the others
      if ((status == imFAILURE) && (_numberOfBuffers > 0))
         destroyBuffers();

      else {				// success
         _tileOwnsBuffer = TRUE;
         _byteOffset = 0;
      }
      _newBufferNeeded = False;
   }
   else {
      status = imFAILURE;
   }

   return status;
}

////////////////////////////////////////////////////////////////
// destroyBuffers (owned by TILE) 
//	destroys 1 or 3 buffers that are owned by tile.
//	May be called by anyone
////////////////////////////////////////////////////////////////
StatusType ImageTile::destroyBuffers()
{
   StatusType status = imFAILURE;

   if (_tileOwnsBuffer) {
      status = imSUCCESS;
      for (int i=0; i < _numberOfBuffers; i++) {
         delete [] _bufferPtr[i];
         _bufferPtr[i] = NULL;
      }
      _numberOfBuffers = 0;
      _tileOwnsBuffer = False;
   }
   return status;
}

////////////////////////////////////////////////////////////////
// setBuffers
//	may be called by anyone that has already created the buffers
//	and wants to pass pointers and establish ownership of buffers.
////////////////////////////////////////////////////////////////
void ImageTile::setBuffers(unsigned char *ptr0, unsigned char *ptr1,
			   unsigned char *ptr2, int lineWidth, int byteOffset)
{

   // If tile owns some buffers already, delete existing ones and assume
   // caller knows what they are doing.
   if (_tileOwnsBuffer)
      destroyBuffers();

   // Save buffer pointers and other arguments
   _bufferPtr[0] = ptr0; 
   _bufferPtr[1] = ptr1; 
   _bufferPtr[2] = ptr2;  

   _numberOfBuffers = 0;
   if (_bufferPtr[0]) _numberOfBuffers++;
   if (_bufferPtr[1]) _numberOfBuffers++;
   if (_bufferPtr[2]) _numberOfBuffers++;

   if ( lineWidth == 0 )
      _lineWidth = _tileWidth * _pixelType.getPixelSize();
   else
      _lineWidth = lineWidth;
   _byteOffset = byteOffset;
   _newBufferNeeded = False;
}

////////////////////////////////////////////////////////////////
// getPixel() - THIS IS A STUB
//	This routine is left as a FUTURE ENHANCEMENT.
//	x,y is pixel coordinate (zoom factor = 1)
////////////////////////////////////////////////////////////////
unsigned char ImageTile::getPixel(int, int)
{
   return (unsigned char) 0;
}

////////////////////////////////////////////////////////////////
// createBufferAttributes()
//	This code is taken from ZOOMX, ZOOMY, IDIV, 
//	and _XvicIdiv located in XvicBasicImageP.h. 
//	For an explanation, see XvicImage.doc.
////////////////////////////////////////////////////////////////
void ImageTile::createBufferAttributes()
{
   // Calc tile buffer width in samples

   int Xin = _tileZoom.getXIn();
   int Xout = _tileZoom.getXOut();
   int zXin = ((((_tileWidth)+1) * Xin) - 1);
   _bufferSampleWidth = (zXin >= 0 ? zXin/Xout : ( zXin - Xout + 1) / Xout);

   // Calc tile buffer width in bytes

   _lineWidth = _bufferSampleWidth * _pixelType.getPixelSize();

   // Calc tile buffer height in samples

   int Yin = _tileZoom.getYIn();
   int Yout = _tileZoom.getYOut();
   int zYin = ((((_tileHeight)+1) * Yin) - 1);
   _bufferSampleHeight = (zYin >= 0 ? zYin/Yout : ( zYin - Yout + 1) / Yout);

   // Calc buffer size in bytes
   // Why is this commented out?????!!!!
   //// _bufferSize = _pixelType.getPixelSize() * _bufferSampleHeight * _bufferSampleWidth;
   _bufferSize = _pixelType.getPixelSize() * _tileWidth * _tileHeight;

}

////////////////////////////////////////////////////////////////
// setTileZoom
//	called by ImageData to set prezoom
////////////////////////////////////////////////////////////////
void ImageTile::setTileZoom(int Xin=1, int Xout=1, int Yin=1, int Yout=1)
{
   if ((_tileZoom.getXIn() != Xin) ||
       (_tileZoom.getXOut() != Xout) ||
       (_tileZoom.getYIn() != Yin) ||
       (_tileZoom.getYOut() != Yout)) {
      _newBufferNeeded = True;
   }
   _tileZoom.setX(Xin, Xout); 
   _tileZoom.setY(Yin, Yout); 
   createBufferAttributes();
}

////////////////////////////////////////////////////////////////
// setTileSubPixelPan
////////////////////////////////////////////////////////////////
void ImageTile::setTileSubPixelPan(int x=0, int y=0) 
{ 
   if ((_tileZoom.getSubPixelPanX() != x) ||
       (_tileZoom.getSubPixelPanY() != y)) {
      _newBufferNeeded = True;
   }
   _tileZoom.setSubPixelPanX(x);	// same as above
   _tileZoom.setSubPixelPanY(y);
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create VicarImageData.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
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


$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create VicarImageFile.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
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

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ZoomFactor.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// ZoomFactor.h
//
//	This is a simple object for storing the zoom info
//	and reducing it to its lowest common denominator.
//	Read XvicImage.doc for information about zoom factors.
//
//	FUTURE:  extra logic for subpixelpanX < out
////////////////////////////////////////////////////////////////
#include "ZoomFactor.h"

////////////////////////////////////////////////////////////////
// reduceRational .. lifted from BOB's code
//	Takes pointers to the numerator and denominator of a rational
//	number,	and reduces them so they share no common integer factors.
////////////////////////////////////////////////////////////////
void ZoomFactor::reduceRational(int *numer, int *denom)
{
   if ((*numer == 0) || (*denom == 0)) {
      *denom = 1;
      *numer = 1;
      return;
   }

   int factor = 2;

   while (factor <= *numer && factor <= *denom) {
      while ((*numer % factor == 0) && (*denom % factor == 0)) {
         *numer /= factor;
         *denom /= factor;
      }
      if (factor == 2)
         factor++;
      else
         factor+=2;		/* skip evens */
   }	
}

////////////////////////////////////////////////////////////////
// setX
//	reduces in & out factors of X and stores values in Private
////////////////////////////////////////////////////////////////
void ZoomFactor::setX(int in, int out)
{  
   _zoomXIn = in;
   _zoomXOut = out;
   reduceRational(&_zoomXIn, &_zoomXOut);
} 

////////////////////////////////////////////////////////////////
// setY
//	reduces in & out factors of Y and stores values in Private
////////////////////////////////////////////////////////////////
void ZoomFactor::setY(int in, int out)    
{ 
   _zoomYIn = in;
   _zoomYOut = out;
   reduceRational(&_zoomYIn, &_zoomYOut);
} 		

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ImagePixelType.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// ImagePixelType.cc
////////////////////////////////////////////////////////////////

#include "ImagePixelType.h"
#include <string.h>
#include <ctype.h>
#include <limits.h>
#include <stdio.h>

// The following must match the Image Widget.  Keep them consistent!!

int ImagePixelType::_DTtoXiw[imNUM_DATA_TYPES] =
   { 140,      11,       142,      10,       144,      145,      146 };

// Coincidentally :-)  these also match the VICAR data types...
// An input string may be longer than this, e.g. "DOUBLE", and still match.

const char *ImagePixelType::_DTtoStr[imNUM_DATA_TYPES] =
   { "BYTE",   "HALF",   "UHALF",  "FULL",   "UFULL",  "REAL",   "DOUB" };

int ImagePixelType::_DTtoSize[imNUM_DATA_TYPES] =
   { sizeof(imByte), sizeof(imHalf), sizeof(imUHalf),
     sizeof(imFull), sizeof(imUFull), sizeof(imReal), sizeof(imDouble) };

double ImagePixelType::_DTtoMin[imNUM_DATA_TYPES] =
   { 0,        SHRT_MIN, 0,        INT_MIN,  0,        0.0,      0.0 };

double ImagePixelType::_DTtoMax[imNUM_DATA_TYPES] =
   { 255,      SHRT_MAX, USHRT_MAX,INT_MAX,  UINT_MAX, 1.0,      1.0 };

// # of characters required to display a pixel of this type
int ImagePixelType::_DTtoWidth[imNUM_DATA_TYPES] =
   { 3,        6,        5,        11,       10,       10,       16  };

////////////////////////////////////////////////////////////////

void ImagePixelType::set(char *str)
{
   int i;
   char str_up[12];

   int len = strlen(str);
   if (len > 12) len = 12;

   for (i=0; i<len; i++)
      str_up[i] = toupper(str[i]);

   for (i=0; i<imNUM_DATA_TYPES; i++) {
      if (strncmp(str_up, _DTtoStr[i], strlen(_DTtoStr[i])) == 0) {
         _pixelType = (DataType) i;
         return;
      }
   }

   _pixelType = imBYTE;			// fail-safe
   return;

}

////////////////////////////////////////////////////////////////

void ImagePixelType::printPixel(void *value_ptr, char *buffer)
{
   switch (_pixelType) {
      case imBYTE:
         sprintf(buffer, "%3d", *(imByte *)value_ptr);
         break;
      case imHALF:
         sprintf(buffer, "%6d", *(imHalf *)value_ptr);
         break;
      case imUHALF:
         sprintf(buffer, "%5u", *(imUHalf *)value_ptr);
         break;
      case imFULL:
         sprintf(buffer, "%11d", *(imFull *)value_ptr);
         break;
      case imUFULL:
         sprintf(buffer, "%10u", *(imUFull *)value_ptr);
         break;
      case imREAL:
         sprintf(buffer, "%8.6g", *(imReal *)value_ptr);
         break;
      case imDOUBLE:
         sprintf(buffer, "%14.11g", *(imDouble *)value_ptr);
         break;
   }
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ImageLabel.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
//
//    ImageLabel.cc: 
//
//    This is a class for image label objects.
//
////////////////////////////////////////////////////////////////////////
#include "ImageLabel.h"
#include "ImageData.h"
#include "BasicComponent.h"

ImageLabel::ImageLabel(ImageData *image, const char *name, const char *key) 
{
   _image = image;
   _numbSubset = 0;
   _name = strdup(name);;
   strcpy(_key, key);
   for (int i = 0; i < MAX_IMAGE_LABEL_SUBSET; i++) 
      _subsetList[i] = NULL;
}

ImageLabel::~ImageLabel() 
{
   delete[] _name;
}
   
ImageLabel * ImageLabel::getChildLabel(int index) 
{
   if (index >= 0 && index < _numbSubset)
      return _subsetList[index];
   else 
      return NULL;
}

void ImageLabel::addChild(ImageLabel *childLabel) 
{
   _subsetList[_numbSubset] = childLabel;
   _numbSubset++;
}

void ImageLabel::deleteLabelTree()
{
   int childNumb;

   if (!this) return;
   childNumb = this->getChildNumb();
   if (childNumb > 0) {
      for (int i=0; i<childNumb; i++) {
         // recursivly go down  
         ImageLabel *childLabel = this->getChildLabel(i);
         if (childLabel->getChildNumb() > 0) 
            this->getChildLabel(i)->deleteLabelTree();
         else 
            delete childLabel;
      }
   }

   return;
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create imagemodel.imake
#define SUBROUTINE imagemodel
#define MODULE_LIST ImageData.cc ImageDataWrapper.cc ImageTile.cc \
   VicarImageData.cc VicarImageFile.cc ZoomFactor.cc ImagePixelType.cc \
   ImageLabel.cc

#define GUI_SUBLIB

#define USES_C_PLUS_PLUS

#ifndef NO_PDS
#define LIB_PDS
#endif

#ifndef NO_MP_ROUTINES
#define LIB_P1SUB
#endif
/* #define LIB_RTL */
#define LIB_TAE
#define LIB_MOTIFAPP
#define LIB_MOTIF

#if 0
#define DEBUG
#endif
$ Return
$!#############################################################################
