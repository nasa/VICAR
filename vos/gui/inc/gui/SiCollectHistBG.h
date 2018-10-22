////////////////////////////////////////////////////////////////
// SiCollectHistBG.h
////////////////////////////////////////////////////////////////
#ifndef SICOLLECTHISTBG_H
#define SICOLLECTHISTBG_H

class ImageData;
class SiHistogram;
class ImageTile;

void SiCollectHistBG(ImageData *imageModel, 
	SiHistogram *histR, SiHistogram *histG, SiHistogram *histB, 
	void **active);

void SiCollectHistFromTile(SiHistogram *hist, int bufferIndex, 
	ImageTile &tile, int width, int height);

#endif

