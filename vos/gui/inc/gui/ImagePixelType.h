////////////////////////////////////////////////////////////////
// ImagePixelType.h
////////////////////////////////////////////////////////////////
#ifndef IMAGEPIXELTYPE_H
#define IMAGEPIXELTYPE_H

enum DataType { imBYTE=0, imHALF=1, imUHALF=2, imFULL=3, imUFULL=4,
		imREAL=5, imDOUBLE=6 };
#define imNUM_DATA_TYPES 7

// The data type defines may need to change on some platforms.  They
// must match the Image Widget.  If a platform needs different types, use
// #if's here.
 
typedef unsigned char imByte;
typedef short int imHalf;
typedef unsigned short int imUHalf;
typedef int imFull;
typedef unsigned int imUFull;
typedef float imReal;
typedef double imDouble;

class ImagePixelType {

 protected:

   static int _DTtoXiw[imNUM_DATA_TYPES];
   static const char *_DTtoStr[imNUM_DATA_TYPES];
   static int _DTtoSize[imNUM_DATA_TYPES];
   static double _DTtoMin[imNUM_DATA_TYPES];
   static double _DTtoMax[imNUM_DATA_TYPES];
   static int _DTtoWidth[imNUM_DATA_TYPES];

   DataType _pixelType;

 public:

   ImagePixelType() { _pixelType = imBYTE; }
   ImagePixelType(DataType d) { _pixelType = d; }
   ImagePixelType(const ImagePixelType &p) { _pixelType = p._pixelType; }

   void set(DataType d) { _pixelType = d; }
   void set(char *str);

   DataType get() const { return _pixelType; }
   const char *getString() const { return _DTtoStr[_pixelType]; }
   int getXiw() const { return _DTtoXiw[_pixelType]; }

   int isIntegral() const { return (_pixelType!=imREAL&&_pixelType!=imDOUBLE); }

   int getPixelSize() const { return _DTtoSize[_pixelType]; }
   double getMinDataRange() const { return _DTtoMin[_pixelType]; }
   double getMaxDataRange() const { return _DTtoMax[_pixelType]; }

   int neededWidth() const { return _DTtoWidth[_pixelType]; }
   void printPixel(void *value_ptr, char *buffer);

   friend int operator==(ImagePixelType p1, ImagePixelType p2)
      {  return (p1._pixelType == p2._pixelType); }
   friend int operator!=(ImagePixelType p1, ImagePixelType p2)
      {  return (p1._pixelType != p2._pixelType); }
   ImagePixelType& operator=(const ImagePixelType& p)
      {  _pixelType = p._pixelType;  return *this;  }

};

#endif

