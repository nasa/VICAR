////////////////////////////////////////////////////////////////
//
//      RotatedImageData.h
//
//	This is a class derived from ImageDataWrapper class. 
//	It will rotate the wrapped image in a number of ways
//	(see RotationDefs.h).
//
////////////////////////////////////////////////////////////////
#ifndef ROTATEDIMAGEDATA_H
#define ROTATEDIMAGEDATA_H
#include "ImageDataWrapper.h"
#include "RotationDefs.h" 

class RotatedImageData : public ImageDataWrapper { 

  protected:

    RotationType _rotationMode;    // left, right, flip

    void copyTileBuffer(ImageTile *rotTile, ImageTile *unrotTile,
			int height, int width);

    int getUnrotTile_x (int x);
    int getUnrotTile_y (int y);
    int getUnrotTile_x (double x);
    int getUnrotTile_y (double y);

  public:

    RotatedImageData();
    RotatedImageData(RotationType mode);
    virtual ~RotatedImageData();

    // Functions specific to this subclass

    RotationType getRotationMode() { return(_rotationMode); };
    void setRotationMode(RotationType rotMode);

    // Overrides of superclass functions

    // Surprisingly, open/close require no overrides.  All rotations are
    // handled in the various other routines.

    void transDisplayToImageCoords(int x, int y, int *Sample, int *Line);
    void transDisplayToImageCoords(double x, double y, 
				   double *sample, double *line);
    void transImageToDisplayCoords(int sample, int line, int *x, int *y);
    void transImageToDisplayCoords(double sample, double line,
				   double *x, double *y);

    virtual StatusType getSuggestedUnzoomedTileSize(int &height, int &width);
    virtual void getUnzoomedTileSize(int &height, int &width);
    virtual void setUnzoomedTileSize(int height, int width);

    virtual ZoomFactor &calcTileZoom(ImageTile *tile,
				     int Xin=1, int Xout=1, 
				     int Yin=1, int Yout=1, 
				     int Xpan = 0, int Ypan = 0);    
    virtual ImageTile &createTile(ZoomFactor &tileZoom,
				  Boolean willCreateBuffers = TRUE);
    // getTileZoom() does not need to be overridden.  The ImageTile object is
    // always the rotated one; unrotated tiles are not seen by the caller.
    // All getTileZoom() does is to call TileZoom::getTileZoom() and really
    // should not exist.

    virtual int getNumbSamples() const;
    virtual int getNumbLines() const;

    virtual int lineSampToLatLon(double line, double samp, double *lat,
				 double *lon, int type);
    virtual int latLonToLineSamp(double *line, double *samp, double lat,
				 double lon, int type);

    // Read image data

    StatusType readPixel(ColorType color, int x, int y, 
			 unsigned char *bufferPtr);
    StatusType readLine(ColorType color, int line, 
			unsigned char *bufferPtr);
    StatusType readTile(int unzoomedStartSample, int unzoomedStartLine,
			int unzoomedWidth, int unzoomedHeight,
			ImageTile &tile);

    // Given the coordinates of a tile, return the corresponding coordinates
    // of the same tile in the unrotated (original) image

    void getUnrotTileCoords(int *x, int *y, int *w, int *h);
};
#endif

