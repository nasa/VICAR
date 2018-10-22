////////////////////////////////////////////////////////////////
// ImageTile.h
//
//	Created by image view (or created by model when requested by view).
//	Keeps tile information and location of buffer(s) that are needed for
//	the imaging widget.
//
//	FUTURE ENHANCEMENTS:
//		(1)  Fill in function getPixel().  This
//		       routine is a stub for now.
//
////////////////////////////////////////////////////////////////
#ifndef IMAGETILE_H
#define IMAGETILE_H
#include "ImageDefs.h"
#include "ZoomFactor.h"
#include "ImagePixelType.h"
#include <Xm/Xm.h>	// only for Boolean!

class ImageTile {

 protected:

   ImagePixelType _pixelType;
   ZoomFactor	_tileZoom;
   int		_tileWidth;
   int		_tileHeight;
   int		_bufferSampleWidth;
   int		_bufferSampleHeight;
   int		_startLine;
   int		_startSample;
   int		_lineWidth;  // if Tile owns buffer, lineWidth = _tileWidth *
			     // _pixelSize; else it is set by the buffer owner.
   int		_bufferSize;	// units = bytes; all buffers the same size
   unsigned char * _bufferPtr[3];
   int		_numberOfBuffers;
   Boolean	_tileOwnsBuffer;
   ModeType	_mode;
   int		_byteOffset;
   Boolean	_newBufferNeeded;

   void		createBufferAttributes();

 public:

   ImageTile()
	: _numberOfBuffers(0), _tileOwnsBuffer(FALSE), _newBufferNeeded(True)
	{ } 
   virtual ~ImageTile() { destroyBuffers(); }

   // Management of buffers owned by tile.  1 - 3 buffers are needed to store
   // the image data that is read in by the model.  Buffers can be owned by:
   // (1) tile, (2) view, (3) model.  ImageDisplayView puts the ownership on
   // the tile.

   StatusType createBuffers();	// Optional.  Called if tile 'owns' buffers.
   StatusType destroyBuffers();

 	// sets buffer pointers, called by buffer owner
   void	setBuffers(unsigned char *ptr0=NULL, unsigned char *ptr1=NULL,
		unsigned char *ptr2=NULL, int lineWidth=0, int byteOffset=0);
   inline Boolean doesTileOwnBuffer() const { return _tileOwnsBuffer; }
	// change ownership of buffer to (or from) tile
   inline void changeBufferOwnerToTile(Boolean own)
	{  _tileOwnsBuffer = own; }
   inline Boolean isNewBufferNeeded() { return _newBufferNeeded; }

   // Init tile properties .. set by image subclasses

   virtual void	setTileZoom(int, int, int, int); // sets prezoom
   virtual void setTileSubPixelPan(int x, int y);
   inline void setSize(int height, int width)	// set size of tile to be read
	{  if (height != 0) _tileHeight = height;
	   if (width != 0) _tileWidth = width;
	   createBufferAttributes();
	}
   void setPixelType(char *pixelType) { _pixelType.set(pixelType); }
   void setPixelType(ImagePixelType dt) { _pixelType = dt; }

   inline void setMode(ModeType mode) { _mode = mode; }  // set color/bw mode

   // Info set by ImageData subclass when data is moved into buffer

   inline void setStartLineSample(int line, int sample)
	{  _startSample = sample; _startLine = line; }

   // Misc info may be needed by caller

   inline int getPixelSize() const { return _pixelType.getPixelSize(); }
   inline ImagePixelType getPixelType() const { return _pixelType; }
   inline ZoomFactor &getTileZoom() { return _tileZoom; } // get tile's prezoom
   inline int getBufferWidth() const { return _bufferSampleWidth; }
   inline int getBufferHeight() const { return _bufferSampleHeight; }
   inline int getBufferSize() const { return _bufferSize; } // size in bytes

   inline ModeType getMode() const { return _mode; }	// color or black&white
   inline int getHeight() const { return _tileHeight; }	// unzoomed coords
   inline int getWidth() const { return _tileWidth; }	// unzoomed coords
   inline int getLineWidth() const { return _lineWidth; } // in bytes
   inline int getByteOffset() const { return _byteOffset; } // in bytes to
							     // starting pixel
   inline int getXcoordinate() const { return _startSample; }
   inline int getYcoordinate() const { return _startLine; }
   inline unsigned char *getBufferPtr(int index)
	{  if (index < 3 && index >= 0) return _bufferPtr[index];
	    else return NULL;
	}

   // getPixel() is intended for returning a pixel in the same manner as
   // readPixel() does in the model.  However, VicarImageData model calls a
   // read on the file when readPixel() is called.  getPixel() should read
   // the pixel from the tile buffer for better performance.

   unsigned char getPixel(int x, int y);	// Stub only

};

#endif

