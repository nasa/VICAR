////////////////////////////////////////////////////////////////
// SiCollectHist.h: These subroutines can be used to fill in histogram 
// model.  The caller should pass a pointer to existing histogram 
// model object.
////////////////////////////////////////////////////////////////
#ifndef SICOLLECTHIST_H
#define SICOLLECTHIST_H

#include "ImageDefs.h"

class ImageData;
class SiHistogram;

// Calculate histogram from the image

void SiCollectHist(ImageData *imageModel, 
		SiHistogram *histR, SiHistogram *histG, SiHistogram *histB);
void siGetHistPtr (SiHistogram *, ImageData *, int, int, int, ColorType);


void SiCollectHist(int arrayR[], int arrayG[], int arrayB[],
                SiHistogram *histR, SiHistogram *histG, SiHistogram *histB);


// Calculate histogram from the array of arbitrary size

void SiCollectHist(int *arrayR, int *arrayG, int *arrayB, int size,
                SiHistogram *histR, SiHistogram *histG, SiHistogram *histB);

void siGetHistPtr (SiHistogram *, int *array, int size);

// Actually read the pixels for one line's worth and update the histogram

void SiCollectHistLine(SiHistogram *hist, unsigned char *buffer, int size,
		ImagePixelType type);

#endif

