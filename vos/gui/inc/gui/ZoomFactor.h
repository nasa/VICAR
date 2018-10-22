////////////////////////////////////////////////////////////////
// ZoomFactor.h
//
//	This is a simple object for storing the zoom info
//	and reducing it to its lowest common denominator.
//	Read XvicImage.doc for information about zoom factors.
//
//	FUTURE: extra logic for subpixelpanX < out
////////////////////////////////////////////////////////////////
#ifndef ZOOMFACTOR_H
#define ZOOMFACTOR_H

class ZoomFactor {

 protected:

   int	_zoomXIn;
   int	_zoomXOut;
   int	_zoomYIn;
   int	_zoomYOut;
   int	_subPixelPanX;
   int	_subPixelPanY;

   void	reduceRational(int *numer, int *denom);

 public:

   ZoomFactor()
	{  _zoomXIn = 1; _zoomYIn = 1; _zoomXOut = 1; _zoomYOut = 1;
	   _subPixelPanX = 0; _subPixelPanY = 0;
	}
   ZoomFactor(int zoomXIn, int zoomYIn, int zoomXOut, int zoomYOut,
		int subPPX=0, int subPPY=0)
	{  _zoomXIn = zoomXIn; _zoomYIn = zoomYIn;
	   _zoomXOut = zoomXOut; _zoomYOut = zoomYOut;
	   _subPixelPanX = subPPX; _subPixelPanY = subPPY;
	}
   virtual ~ZoomFactor() { }

   // Zoom only:

   inline virtual int getXIn()  const { return _zoomXIn; }
   inline virtual int getYIn()  const { return _zoomYIn; }
   inline virtual int getXOut() const { return _zoomXOut; }
   inline virtual int getYOut() const { return _zoomYOut; }
   virtual void setX(int in, int out);
   virtual void setY(int in, int out);

   // SubPixelPan only:

   inline virtual int getSubPixelPanX() const { return _subPixelPanX; }
   inline virtual int getSubPixelPanY() const { return _subPixelPanY; }
   inline void setSubPixelPanX(int x) { _subPixelPanX = x; }
   inline void setSubPixelPanY(int y) { _subPixelPanY = y; }

};

#endif

