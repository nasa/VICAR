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

