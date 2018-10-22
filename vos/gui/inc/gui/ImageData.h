////////////////////////////////////////////////////////////////
// ImageData.h
//
//	This is an abstract class for image data. Currently  
//	(Oct 94) the only subclass available is VicarImageData
//	data which is the standard MIPL Vicar formatted
//	 (1 or 3) files.  This is used with ImageView for
//	displaying an image from file onto screen.
//
//	"this describes what an image is, not what it
//	looks like or how to draw it"
//
//
//   FUTURE ENHANCEMENTS:
//	(1) Allow Mode to be multispectrum
////////////////////////////////////////////////////////////////
#ifndef IMAGEDATA_H
#define IMAGEDATA_H
#include "ImageDefs.h"
#include "ZoomFactor.h"
#include "ImageTile.h"
#include "ImagePixelType.h"
#include "ImageLabel.h"
#include <Xm/Xm.h>              // only for Boolean!

#include <iostream>

const int MAX_CHANS = 3;	// Max # of channels (e.g. R,G,B)
const int MAX_FILES = 3;	// Max # of physical files

typedef enum LongitudeDirection { UNDEFdirection,EAST, WEST}LongitudeDirection;

class BasicImageView;
class ImageLabel;

class ImageData {

 protected:

   ImageLabel *_labelTree;      // The root of the label tree.
   char _lastKey[MAX_IMAGE_LABEL_KEY_SIZE];

   int			_numViews;
   BasicImageView	**_views;

   int _numberOfLines;
   int _numberOfSamples;

   int _tileHeight;
   int _tileWidth;

   ModeType	_mode;			// bw, color, etc
   ImagePixelType _pixelType;

   char	_inputDataSourceName[4000];  // usually a filename
   Boolean _dataSourceOpened;

   Boolean _inconsistentImage;
   Boolean _errorMsgIssued;	// So we don't do multiples
   char	*_errorMsg;

   double _fileMinDataRange;
   double _fileMaxDataRange;
   double _userMinDataRange;
   double _userMaxDataRange;

   Boolean _fileDataRangeValid;
   Boolean _useFileMin;
   Boolean _useFileMax;

   ZoomFactor _newTileZoom; // really just a temp variable to hold space

   virtual void updateViews();	// Called whenever the model's data changes
   virtual void updatePartViews(int flags); // only part of the model changed

   virtual void initTileProperties(ImageTile &tile, ZoomFactor &tileZoom);

   virtual void setErrorMsg(const char *);  // For subclasses to report errors

   // Must be called by all open()'s that support more than byte data.
   virtual void initDataRange();

   // Determine data range from the file.  Should be overridden by any
   // subclass that has data range info easily available (e.g. from a label
   // instead of reading all the data).
   virtual void readDataRange();
   virtual void findMaxMinInLine(unsigned char *buffer);

   int _mapProjInfoPresent;  // 1 if we have map projection data

   ImageData();

 public:

   virtual ~ImageData();

   // Label stuff
   virtual StatusType getLabelSetValue(char*& label, char *key, int *maxsize) 
   { return imSUCCESS; }
   ImageLabel *getLabelRoot() { return _labelTree; }

   // View stuff
   virtual void attachView(BasicImageView * view);
   virtual void detachView(BasicImageView * view);

   virtual void getInputDataSourceName(char *name) 
	{ strcpy(name, _inputDataSourceName); } 
   virtual char *getInputDataSourceName() { return _inputDataSourceName; }
   // Used for reload only... in case a subclass munges the name somehow
   virtual char *getReloadDataSourceName() { return _inputDataSourceName; }
   virtual Boolean isDataSourceOpened() const { return _dataSourceOpened; }

   // Image data management
   virtual StatusType open(char *) = 0;		// open file(s)
   virtual StatusType close() = 0;		// close files(s)

   // To get information only
   virtual int getPixelSize() { return _pixelType.getPixelSize(); }
   virtual void getPixelType(char *pixelType) const
	{ strcpy(pixelType, _pixelType.getString()); }
   virtual ImagePixelType getPixelType() const { return _pixelType; }

   virtual ModeType getMode() const { return _mode; }	// color or black&white
   virtual int getNumbSamples() const { return  _numberOfSamples; }
   virtual int getNumbLines() const { return  _numberOfLines; }

   // Get optimum buff size for reading in data
   virtual StatusType getSuggestedUnzoomedTileSize(int &height, int &width);

   // Get numb lines and samples per image
   virtual void getUnzoomedTileSize(int &height, int &width)
	{ height = _tileHeight; width = _tileWidth; }

   // Transformation of display coords into image coords
   virtual void transDisplayToImageCoords(int x, int y, int *samp, int *line);
   virtual void transDisplayToImageCoords(double x, double y, 
                                          double *samp, double *line);

   // Transformation of image coords into display coords
   virtual void transImageToDisplayCoords(int samp, int line, int *x, int *y);
   virtual void transImageToDisplayCoords(double samp, double line, 
                                          double *x, double *y);

   // Tile/buffer management.  Subclasses may override any of these.
   virtual ImageTile &createTile(ZoomFactor &tileZoom,
				 Boolean willCreateBuffers = TRUE);
   virtual void destroyBuffers();	// destroy buffers owned by the model
   virtual Boolean isNewBufferNeeded(ImageTile &tile)
	{ return tile.isNewBufferNeeded(); }

   // Outside controls, defaults if not called

   // Set size of tile (for reading in data)
   virtual void setUnzoomedTileSize(int h, int w)
	{ if (h != 0) _tileHeight = h; if (w != 0) _tileWidth = w; }

   // Set mode: color or black&white. will default if not set
   virtual void setMode(ModeType mode)
	{ _mode = mode; updateViews(); }

   // Data good or bad 
   virtual Boolean isConsistent() const { return (Boolean) !_inconsistentImage; }

   // Get tile (partial) zoom on image
   virtual ZoomFactor &getTileZoom(ImageTile &tile) { return tile.getTileZoom();}

   // Get and set data range
   virtual void getDataRange(double &min, double &max);
   virtual double getMinDataRange()
	{  double min,max; getDataRange(min,max); return min;  }
   virtual double getMaxDataRange()
	{  double min,max; getDataRange(min,max); return max;  }
   virtual void setDataRange(double min, double max);
   virtual void setDataMin(double min);
   virtual void setDataMax(double max);

   // True if min/max comes from file instead of user
   virtual Boolean isMinAuto() { return _useFileMin; }
   virtual Boolean isMaxAuto() { return _useFileMax; }
   virtual void setMinAuto(Boolean min_auto);
   virtual void setMaxAuto(Boolean max_auto);

   // Read image data
   virtual StatusType readTile(int unzoomedStartSample, int unzoomedStartLine,
				int unzoomedWidth, int unzoomedHeight,
				ImageTile & tile ) = 0;
   virtual StatusType readLine(ColorType color, int line,
				unsigned char *bufferPtr) = 0;
   virtual StatusType readPixel(ColorType color, int sampleOffset,
				int lineOffset, unsigned char *bufferPtr) = 0;

   // Zoom
   virtual ZoomFactor &calcTileZoom(ImageTile *tile,
		int Xin=1, int Xout=1, int Yin=1, int Yout=1,
		int Xpan=0, int Ypan=0);

   // Error management
   virtual char *getErrorMsg() { _errorMsgIssued=True; return _errorMsg; }
   virtual Boolean errorMsgIssued() { return _errorMsgIssued; }

   static ImageType getImageTypeFromFile( char * );

   // derived classes should implement the MP methods, if applicable:

   virtual int readMapProjInfo() { return 0; }

   // for the LatLon functions, the "type" argument refers to the map
   // projection type (0 = planetocentric, 1 = planetodetic)

   virtual int lineSampToLatLon(double   /* line */ ,
                                double   /* samp */ ,
                                double * /* lat */ ,
				double * /* lon */ ,
                                int      /* type */ ) { return 0; }

   virtual int latLonToLineSamp(double * /* line */ ,
                                double * /* samp */ ,
                                double   /* lat */ ,
                                double   /* lon */ ,
                                int      /* type */ ) { return 0; }
				
   virtual int isMapProjInfoPresent() { return _mapProjInfoPresent; }

   // for MP-capable images, determine whether EAST or WEST longitude is used:
   // 0 for undefined, 1 for east, 2 for west
   virtual LongitudeDirection getLonDirection() { return UNDEFdirection; }
   virtual char* getLastKey() { return _lastKey; }
   virtual void setLastKey(char* lastkey) { strcpy(_lastKey, lastkey); }
};

#endif

