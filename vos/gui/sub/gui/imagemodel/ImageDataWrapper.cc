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

