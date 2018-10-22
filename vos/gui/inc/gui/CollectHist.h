////////////////////////////////////////////////////////////////
// CollectHist.h: These subroutines can be used to fill in histogram 
// model.  The caller should pass a pointer to existing histogram 
// model object.
////////////////////////////////////////////////////////////////
#ifndef COLLECTHIST_H
#define COLLECTHIST_H

#include "ImageDefs.h"

class ImageData;
class Histogram;

#define HISTSIZE 256

// Calculate histogram from the image

void CollectHist(ImageData *imageModel, 
		Histogram *histR, Histogram *histG, Histogram *histB);
void getHistPtr (Histogram *, ImageData *, int, int, int, ColorType);


// Calculate histogram from the array of size HISTSIZE

void CollectHist(int arrayR[], int arrayG[], int arrayB[],
                Histogram *histR, Histogram *histG, Histogram *histB);


// Calculate histogram from the array of arbitrary size

void CollectHist(int *arrayR, int *arrayG, int *arrayB, int size,
                Histogram *histR, Histogram *histG, Histogram *histB);

void getHistPtr (Histogram *, int *array, int size);

// Actually read the pixels for one line's worth and update the histogram

void CollectHistLine(Histogram *hist, unsigned char *buffer, int size,
		ImagePixelType type);

#endif

