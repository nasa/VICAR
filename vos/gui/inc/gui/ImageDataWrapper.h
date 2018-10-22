//////////////////////////////////////////////////////////////////////////////
// ImageDataWrapper.h: This is a class derived from ImageData class.
// It is intended to maintain image data if the specific type
// (e.g.; VicarImageData or PdsImageData) is not known initially.
//////////////////////////////////////////////////////////////////////////////
#ifndef IMAGEDATAWRAPPER_H
#define IMAGEDATAWRAPPER_H
#include "ImageData.h"
#include "ImageLabel.h"
#include <Xm/Xm.h>              // only for Boolean!

class ImageDataWrapper : public ImageData { 

  protected:
		
    ImageData *_actualImage;    // the thing around which we're wrapped
    Boolean _weOwnImage;        // True if we "own" the ImageData object

    // The user class gives us the pointer to the actual image
    virtual void registerActualImage(ImageData *actualImage,
                                     Boolean weOwnImage);

    virtual void unregisterActualImage();

    virtual void initTileProperties(ImageTile &tile, ZoomFactor &tileZoom);

    char _lastKey[MAX_IMAGE_LABEL_KEY_SIZE];

    char _reloadDataSourceName[4000]; // like _inputDataSourceName but un-munged

  public:

    ImageDataWrapper();
    virtual ~ImageDataWrapper();
    
    // Get the pointer to the actual Image 
    virtual ImageData *getActualImage() { return _actualImage; }

    virtual void attachView(BasicImageView *view);
    virtual void detachView(BasicImageView *view);
    virtual int getPixelSize();
    virtual void transDisplayToImageCoords(int x, int y, int *samp, int *line);
    virtual void transDisplayToImageCoords(double x, double y, 
					   double *samp, double *line);
    virtual void transImageToDisplayCoords(int samp, int line, int *x, int *y);
    virtual void transImageToDisplayCoords(double samp, double line, 
					   double *x, double *y);
    virtual void setMode(ModeType mode) ;
    virtual void getDataRange(double &min, double &max);
    virtual void setDataRange(double min, double max);
    virtual void setDataMin(double min);
    virtual void setDataMax(double max);
    virtual void setMinAuto(Boolean minAuto);
    virtual void setMaxAuto(Boolean maxAuto);
    virtual StatusType getSuggestedUnzoomedTileSize(int &height, int &width);
    virtual ImageTile &createTile(ZoomFactor &tileZoom,
				  Boolean willCreateBuffers = TRUE);
    virtual void destroyBuffers();
    virtual Boolean isNewBufferNeeded(ImageTile &tile);
    virtual char *getErrorMsg();
    virtual Boolean errorMsgIssued();
    StatusType open(char *filename);
    StatusType close();
    StatusType readPixel(ColorType color, int x, int y, 
			 unsigned char *bufferPtr);
    StatusType readLine(ColorType color, int line, 
			unsigned char *bufferPtr); 
    StatusType readTile(int unzoomedStartSample, int unzoomedStartLine,
			int unzoomedWidth, int unzoomedHeight,
			ImageTile &tile);
    virtual ZoomFactor &calcTileZoom(ImageTile *tile, int Xin, int Xout, 
				     int Yin, int Yout, int Xpan, int Ypan);
    void getInputDataSourceName(char *name);
    char *getInputDataSourceName(); 
    virtual char *getReloadDataSourceName() { return _reloadDataSourceName; }
    virtual Boolean isDataSourceOpened() const; 
    virtual void getPixelType(char *pixelType) const; 
    virtual ImagePixelType getPixelType() const; 
    virtual ModeType getMode() const;
    virtual int getNumbSamples() const;
    virtual int getNumbLines() const;
    virtual void getUnzoomedTileSize(int &height, int &width);
    virtual void setUnzoomedTileSize(int h, int w);
    virtual Boolean isConsistent() const;
    virtual ZoomFactor &getTileZoom(ImageTile &tile); 
    virtual double getMinDataRange();
    virtual double getMaxDataRange();
    virtual Boolean isMinAuto(); 
    virtual Boolean isMaxAuto(); 

    virtual int readMapProjInfo();
    virtual int lineSampToLatLon(double line, double samp, double *lat,
                                 double *lon, int type);

    virtual int latLonToLineSamp(double *line, double *samp, double lat,
				 double lon, int type);
    virtual int isMapProjInfoPresent();
    virtual LongitudeDirection getLonDirection();

    virtual StatusType getLabelSetValue(char*& label, char *key, int *maxsize);
    ImageLabel *getLabelRoot();

    virtual const char *const className() { return("ImageDataWrapper"); }

};
#endif

