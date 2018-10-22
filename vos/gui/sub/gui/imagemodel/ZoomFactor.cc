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

