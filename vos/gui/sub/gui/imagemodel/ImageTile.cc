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

